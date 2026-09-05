// Driving a purchase: draw a code, ask for an invoice, then keep one watch per order alive
// until it settles. What each answer means is `order.ts`'s to say.

import {
  ApiError, AbortedError, BACKOFF_MAX, BACKOFF_START, NOTHING_RECEIVED, createInvoice, inferMethod,
  paymentMark, readInvoice, waitForChange,
  type CreatedInvoice, type InvoiceView, type PaymentMark, type Sleep,
} from "./api.js";
import { display } from "./codes.js";
import { composeOrder } from "./domain.js";
import {
  applyView, closedInvoice, offlineInvoice, recordFromView, serverState, viewFor, withoutCode,
  type PaymentView,
} from "./order.js";
import type { Method, OrderRecord, OrderStatus } from "./domain.js";
import type { Store } from "./store.js";

export const GIVE_UP_MS = 15 * 60 * 1000;
export const CODE_ATTEMPTS = 5;
export const DEFAULT_RETRY_AFTER_SECONDS = 60;
const HISTORY_REFRESH_LIMIT = 10;

export interface Selection {
  priceId: string;
  offerId?: string;
  method: Method;
}

export type CheckoutOutcome =
  | { kind: "created"; order: OrderRecord; invoice: CreatedInvoice; method: Method; savedLocally: boolean }
  | { kind: "catalogChanged" }
  | { kind: "rateLimited"; retryAfter: number }
  | { kind: "providerUnavailable"; method: Method }
  | { kind: "failed" };

export interface FlowDeps {
  store: Store;
  fetch: typeof fetch;
  sleep: Sleep;
  now: () => number;
  newCode: () => string;
  hashCode: (code: string) => Promise<string>;
  render: (view: PaymentView) => void;
}

class GaveUp extends Error {
  constructor() {
    super("the confirming screen gave up");
    this.name = "GaveUp";
  }
}

export interface Watch {
  readonly orderId: string;
  suspend(): void;
  resume(): void;
  stop(): void;
  /** What a fresh loop on this order needs to draw the same screen. */
  restartOptions(): WatchOptions;
  readonly done: Promise<void>;
}

export interface WatchOptions {
  initial?: InvoiceView;
  method?: Method;
  resumed?: boolean;
  record?: OrderRecord;
}

class WatchLoop implements Watch {
  private suspended = false;
  private stopped = false;
  private started = false;
  finished = false;
  private ctl = new AbortController();
  private wake: (() => void) | null = null;
  private giveUpAt: number | undefined;
  private budgetLeft: number | undefined;
  private method: Method | undefined;
  private record: OrderRecord | undefined;
  private savedLocally = false;
  readonly done: Promise<void>;
  private settleDone!: () => void;

  constructor(
    readonly orderId: string,
    private readonly d: FlowDeps,
    private readonly opts: WatchOptions,
  ) {
    this.method = opts.method;
    this.record = opts.record ?? d.store.order(orderId);
    this.done = new Promise<void>((resolve) => { this.settleDone = resolve; });
  }

  start(): void {
    if (this.started) return;
    this.started = true;
    void this.run();
  }

  restartOptions(): WatchOptions {
    return {
      ...(this.method !== undefined ? { method: this.method } : {}),
      ...(this.opts.resumed !== undefined ? { resumed: this.opts.resumed } : {}),
      ...(this.record !== undefined ? { record: this.record } : {}),
    };
  }

  suspend(): void {
    if (this.suspended || this.finished) return;
    this.suspended = true;
    // What is left of the budget, so hidden time neither spends it nor refills it
    if (this.giveUpAt !== undefined) this.budgetLeft = Math.max(0, this.giveUpAt - this.d.now());
    this.ctl.abort();
  }

  resume(): void {
    if (!this.suspended || this.finished) return;
    this.suspended = false;
    if (this.budgetLeft !== undefined) {
      this.giveUpAt = this.d.now() + this.budgetLeft;
      this.budgetLeft = undefined;
    }
    this.ctl = new AbortController();
    const wake = this.wake;
    this.wake = null;
    wake?.();
  }

  stop(): void {
    if (this.finished) return;
    this.stopped = true;
    this.ctl.abort();
    const wake = this.wake;
    this.wake = null;
    wake?.();
  }

  private park(): Promise<void> {
    return new Promise<void>((resolve) => { this.wake = resolve; });
  }

  // A loop that has been stopped has no screen any more: the page has moved to another order,
  // and painting now would draw this order's address under that order's URL.
  private emit(view: PaymentView): void {
    if (this.stopped) return;
    if (view.screen === "awaitingConfirmation" && this.giveUpAt === undefined) this.giveUpAt = this.d.now() + GIVE_UP_MS;
    // both together: a budget outliving its deadline re-arms one on the wrong screen
    if (view.screen !== "awaitingConfirmation") {
      this.giveUpAt = undefined;
      this.budgetLeft = undefined;
    }
    this.d.render(view);
  }

  private show(record: OrderRecord, invoice: InvoiceView | undefined): void {
    this.emit(viewFor(record, invoice, this.method, {
      savedLocally: this.savedLocally,
      resumed: this.opts.resumed ?? false,
    }));
  }

  private receive(view: InvoiceView): void {
    this.method = inferMethod(view) ?? this.method;
    const applied = applyView(this.d.store, this.orderId, view, this.d.now(), this.record, this.method);
    this.record = applied.record;
    this.savedLocally = applied.savedLocally || this.savedLocally;
    this.show(this.record, view);
  }

  private async pass(seen: "open" | "expired", seenPayment: PaymentMark): Promise<InvoiceView> {
    const wait = waitForChange(this.orderId, seen, this.d.fetch, this.d.sleep, this.ctl.signal, this.d.now, seenPayment);
    if (this.giveUpAt === undefined) return wait;
    const remaining = this.giveUpAt - this.d.now();
    const settled = wait.then((v) => ({ ok: true as const, v }), (e: unknown) => ({ ok: false as const, e }));
    if (remaining <= 0) {
      this.ctl.abort();
      await settled;
      throw new GaveUp();
    }
    const deadlineCtl = new AbortController();
    const deadline = this.d.sleep(remaining, deadlineCtl.signal).then(() => "timeout" as const, () => "cancelled" as const);
    const first = await Promise.race([settled.then(() => "answered" as const), deadline]);
    if (first === "timeout") {
      this.ctl.abort();
      await settled;
      throw new GaveUp();
    }
    deadlineCtl.abort();
    const r = await settled;
    if (r.ok) return r.v;
    throw r.e;
  }

  /** The one answer that ends a watch outright. Pure: each site decides what to do with it,
   * so the control flow stays where a reader can see it. */
  private isNotFound(e: unknown): boolean {
    return e instanceof ApiError && e.code === "not_found";
  }

  private showUnknownOrder(): void {
    this.emit({ screen: "unknownOrder", orderId: this.orderId });
  }

  private async run(): Promise<void> {
    try {
      if (this.record?.status === "paid") {
        this.savedLocally = this.d.store.holdsCode(this.orderId, this.record.code);
        this.show(this.record, this.opts.initial);
        return;
      }
      let seen: "open" | "expired" = "open";
      let seenPayment: PaymentMark = NOTHING_RECEIVED;
      let painted = false;
      // what the next request compares against, and whether anything is on screen yet
      const take = (view: InvoiceView): boolean => {
        this.receive(view);
        if (view.status === "paid") return true;
        seen = view.status;
        seenPayment = paymentMark(view);
        painted = true;
        return false;
      };
      let first = this.opts.initial;
      if (first === undefined) {
        try {
          first = await readInvoice(this.orderId, this.d.fetch, this.ctl.signal);
        } catch (e) {
          if (this.isNotFound(e)) { this.showUnknownOrder(); return; }
          first = undefined; // transient: read again below rather than hold on nothing
        }
      }
      if (first !== undefined) {
        if (take(first)) return;
      } else if (this.record !== undefined) {
        seen = this.record.status;
        const offline = offlineInvoice(this.record);
        if (offline !== undefined) {
          this.method = this.record.cryptoCurrency;
          this.show(this.record, offline);
          painted = true;
        } else if (this.record.status !== "open") {
          // a closed order says all it needs to from the record: only settlement can move it,
          // and the screen for it draws what arrived rather than anywhere to send more
          this.show(this.record, closedInvoice(this.record));
          painted = true;
        }
      }

      // A hold answers only on a change, so entering one with nothing on screen leaves the
      // buyer on the spinner for as long as the invoice does not move, which is the whole
      // point of an open invoice. Read plainly until one answers.
      let backoff = BACKOFF_START;
      while (!painted && !this.stopped) {
        if (this.suspended) { await this.park(); continue; }
        try {
          await this.d.sleep(backoff, this.ctl.signal);
          if (take(await readInvoice(this.orderId, this.d.fetch, this.ctl.signal))) return;
        } catch (e) {
          if (this.isNotFound(e)) { this.showUnknownOrder(); return; }
          if (e instanceof AbortedError) continue; // suspended or stopped; the top decides which
          backoff = Math.min(backoff * 2, BACKOFF_MAX);
        }
      }
      if (this.stopped) return;

      for (;;) {
        if (this.stopped) return;
        if (this.suspended) { await this.park(); continue; }
        let view: InvoiceView;
        try {
          view = await this.pass(seen, seenPayment);
        } catch (e) {
          if (e instanceof AbortedError) continue; // suspended or stopped; the top decides which
          if (e instanceof GaveUp) {
            this.emit({
              screen: "awaitingConfirmation", gaveUp: true, invoice: undefined,
              order: withoutCode(this.record ?? recordFromView(this.orderId, { status: "open" }, this.d.now())),
            });
            return;
          }
          if (this.isNotFound(e)) this.showUnknownOrder();
          return;
        }
        if (take(view)) return;
      }
    } finally {
      this.finished = true;
      this.settleDone();
    }
  }
}

export class Flow {
  private readonly watches = new Map<string, WatchLoop>();
  private epochCount = 0;

  constructor(private readonly d: FlowDeps) {}

  async checkout(sel: Selection): Promise<CheckoutOutcome> {
    for (let attempt = 0; attempt < CODE_ATTEMPTS; attempt++) {
      const wipes = this.d.store.wipeCount;
      const code = this.d.newCode();
      let created: CreatedInvoice;
      try {
        created = await createInvoice({
          priceId: sel.priceId,
          ...(sel.offerId !== undefined ? { offerId: sel.offerId } : {}),
          method: sel.method,
          codeHash: await this.d.hashCode(code),
        }, this.d.fetch);
      } catch (e) {
        if (e instanceof ApiError) {
          switch (e.code) {
            case "code_conflict": continue; // draw again, invisibly to the buyer
            case "catalog_changed": return { kind: "catalogChanged" };
            case "rate_limited": return { kind: "rateLimited", retryAfter: e.retryAfter ?? DEFAULT_RETRY_AFTER_SECONDS };
            case "provider_unavailable": return { kind: "providerUnavailable", method: sel.method };
            default: return { kind: "failed" };
          }
        }
        return { kind: "failed" };
      }
      const order = composeOrder(
        {
          orderId: created.invoiceId,
          badgeType: created.badgeType,
          months: created.months,
          createdAt: new Date(this.d.now()).toISOString(),
        },
        { code: display(code), submitted: undefined, method: sel.method },
        serverState({ ...created, status: "open" }, undefined),
      );
      // The buyer can empty this browser while the invoice is being bought. Writing the order back
      // afterwards would put a code into a store they were told could not be undone, so the sale
      // stands at the service and this browser keeps nothing. Clearing the session is `pay`'s to
      // do, once it knows the answer is still the one the page is waiting for.
      if (this.d.store.wipeCount === wipes) this.d.store.saveOrder(order);
      const savedLocally = this.d.store.holdsCode(order.orderId, order.code);
      return { kind: "created", order, invoice: created, method: sel.method, savedLocally };
    }
    return { kind: "failed" };
  }

  watch(orderId: string, opts: WatchOptions = {}): Watch {
    const live = this.watches.get(orderId);
    if (live && !live.finished) {
      live.resume();
      return live;
    }
    const loop = new WatchLoop(orderId, this.d, opts);
    this.watches.set(orderId, loop);
    loop.start();
    return loop;
  }

  checkAgain(orderId: string): Watch | null {
    const prev = this.watches.get(orderId);
    if (prev && !prev.finished) return null;
    const loop = new WatchLoop(orderId, this.d, prev ? prev.restartOptions() : {});
    this.watches.set(orderId, loop);
    loop.start();
    return loop;
  }

  liveWatches(): Watch[] {
    return [...this.watches.values()].filter((w) => !w.finished);
  }

  stopAll(): void {
    for (const w of this.watches.values()) w.stop();
    this.watches.clear();
    this.epochCount += 1;
  }

  /** Bumped by `stopAll`: an answer awaited across it belongs to a page that has moved on, so it
   * is not owed a repaint, a rewritten address bar, or a watch. It says nothing about the store,
   * which the buyer can empty without navigating: `Store.wipeCount` is what covers that. */
  get epoch(): number {
    return this.epochCount;
  }

  async refreshHistory(limit = HISTORY_REFRESH_LIMIT): Promise<OrderRecord[]> {
    const stale: OrderStatus[] = ["open", "expired"];
    const mine = this.epochCount;
    const targets = this.d.store.orders().filter((o) => stale.includes(o.status)).slice(0, limit);
    for (const order of targets) {
      try {
        const view = await readInvoice(order.orderId, this.d.fetch);
        if (this.epochCount !== mine) break;
        applyView(this.d.store, order.orderId, view, this.d.now(), undefined, inferMethod(view));
      } catch (e) {
        if (e instanceof ApiError && e.code === "rate_limited") break;
      }
    }
    return this.d.store.orders();
  }
}
