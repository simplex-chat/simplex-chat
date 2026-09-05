import { flush, settle, timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { AbortedError, ApiError, BACKOFF_START, readInvoice, type InvoiceView, type Sleep } from "../src/api.js";
import { SINGLE_MONTH } from "../src/catalog.js";
import { CODE_ATTEMPTS, DEFAULT_RETRY_AFTER_SECONDS, Flow, GIVE_UP_MS, type FlowDeps } from "../src/flow.js";
import { applyView, historyRows, offlineInvoice, recordFromView, orderPhase, selectionFromOrder, viewFor, withoutCode, type PaymentView } from "../src/order.js";
import { countdown, money, moneyCompact, startedAgo } from "../src/format.js";
import { type OrderRecord } from "../src/domain.js";
import { Store, type StorageLike } from "../src/store.js";

const apiTest = timedTest(2000);

// ------------------------------------------------------------------- fixtures

class Mem implements StorageLike {
  readonly m = new Map<string, string>();
  getItem(k: string): string | null { return this.m.get(k) ?? null; }
  setItem(k: string, v: string): void { this.m.set(k, v); }
  removeItem(k: string): void { this.m.delete(k); }
}

/** Private browsing, or quota: every write refuses. */
class Refusing implements StorageLike {
  getItem(): string | null { return null; }
  setItem(): void { throw new Error("QuotaExceededError"); }
  removeItem(): void { /* nothing to remove */ }
}

/** `raw` is a body that is not JSON at all: an HTML error page from a proxy. */
interface Reply { status: number; body?: unknown; raw?: string; headers?: Record<string, string> }

function toResponse(r: Reply): Response {
  const headerMap = r.headers ?? {};
  return {
    ok: r.status < 400,
    status: r.status,
    headers: {
      get: (name: string) => {
        const key = Object.keys(headerMap).find((k) => k.toLowerCase() === name.toLowerCase());
        return key ? headerMap[key]! : null;
      },
    },
    json: async () => {
      if (r.raw !== undefined) throw new SyntaxError(`Unexpected token < in JSON at position 0`);
      return r.body;
    },
    text: async () => r.raw ?? JSON.stringify(r.body ?? {}),
  } as unknown as Response;
}

/** Answers from a script or holds the connection the way the `?wait=` does, so concurrency is observable:
 * `maxInFlight` above one means two loops are live on the same order. */
class Net {
  readonly calls: Array<{ url: string; init?: RequestInit }> = [];
  inFlight = 0;
  maxInFlight = 0;
  private readonly scripted: Reply[] = [];
  private held: Array<{ resolve: (r: Reply) => void; reject: (e: unknown) => void; onAbort?: () => void; signal?: AbortSignal }> = [];

  script(...replies: Reply[]): void { this.scripted.push(...replies); }

  get pendingCount(): number { return this.held.length; }

  get urls(): string[] { return this.calls.map((c) => c.url); }

  /** Answers every request currently holding, in order. */
  answerHeld(...replies: Reply[]): void {
    const held = this.held;
    this.held = [];
    held.forEach((h, i) => {
      h.signal?.removeEventListener("abort", h.onAbort!);
      h.resolve(replies[Math.min(i, replies.length - 1)]!);
    });
  }

  readonly fetch = (async (input: unknown, init?: RequestInit): Promise<Response> => {
    const url = String(input);
    this.calls.push(init ? { url, init } : { url });
    this.inFlight++;
    this.maxInFlight = Math.max(this.maxInFlight, this.inFlight);
    try {
      if (this.scripted.length > 0) return toResponse(this.scripted.shift()!);
      return toResponse(await this.hold(init?.signal ?? undefined));
    } finally {
      this.inFlight--;
    }
  }) as unknown as typeof fetch;

  private hold(signal?: AbortSignal): Promise<Reply> {
    return new Promise<Reply>((resolve, reject) => {
      if (signal?.aborted) { reject(new Error("aborted")); return; }
      const entry: { resolve: (r: Reply) => void; reject: (e: unknown) => void; onAbort?: () => void; signal?: AbortSignal } =
        { resolve, reject, ...(signal ? { signal } : {}) };
      entry.onAbort = () => {
        this.held = this.held.filter((h) => h !== entry);
        reject(new Error("aborted"));
      };
      signal?.addEventListener("abort", entry.onAbort, { once: true });
      this.held.push(entry);
    });
  }
}

/** A virtual clock. Nothing sleeps for real, and nothing fires unless advanced. */
class Clock {
  ms = Date.parse("2026-08-28T12:00:00Z");
  private timers: Array<{ at: number; resolve: () => void; onAbort?: () => void; signal?: AbortSignal }> = [];

  readonly now = (): number => this.ms;

  readonly sleep: Sleep = (ms, signal) => {
    if (signal?.aborted) return Promise.reject(new AbortedError());
    return new Promise<void>((resolve, reject) => {
      const t: { at: number; resolve: () => void; onAbort?: () => void; signal?: AbortSignal } =
        { at: this.ms + ms, resolve, ...(signal ? { signal } : {}) };
      t.onAbort = () => {
        this.timers = this.timers.filter((x) => x !== t);
        reject(new AbortedError());
      };
      signal?.addEventListener("abort", t.onAbort, { once: true });
      this.timers.push(t);
    });
  };

  async advance(ms: number): Promise<void> {
    this.ms += ms;
    const due = this.timers.filter((t) => t.at <= this.ms);
    this.timers = this.timers.filter((t) => t.at > this.ms);
    for (const t of due) {
      t.signal?.removeEventListener("abort", t.onAbort!);
      t.resolve();
    }
    await settle();
  }
}

interface Harness {
  flow: Flow;
  store: Store;
  net: Net;
  clock: Clock;
  views: PaymentView[];
  codes: string[];
}

let codeSeed = 0;
function harness(storage: StorageLike = new Mem(), fetchOverride?: typeof fetch): Harness {
  const store = new Store(storage);
  const net = new Net();
  const clock = new Clock();
  const views: PaymentView[] = [];
  const codes: string[] = [];
  const deps: FlowDeps = {
    store,
    fetch: fetchOverride ?? net.fetch,
    sleep: clock.sleep,
    now: clock.now,
    newCode: () => { const c = `CODE${String(codeSeed++).padStart(16, "0")}`; codes.push(c); return c; },
    hashCode: async (c) => `hash-of-${c}`,
    render: (v) => { views.push(v); },
  };
  return { flow: new Flow(deps), store, net, clock, views, codes };
}

/** Every read fails: for the screens that have to be drawable from the store alone. */
function offlineHarness(): Harness {
  const failing = (async () => { throw new TypeError("offline"); }) as unknown as typeof fetch;
  return harness(new Mem(), failing);
}

const openXmr = {
  status: "open", badgeType: "legend", months: 12, amount: 42000,
  currency: "usd", expiresAt: "2026-08-28T13:00:00Z",
  address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr",
} as const;
const openCard = {
  status: "open", badgeType: "supporter", months: 1, amount: 700,
  currency: "usd", expiresAt: "2026-08-28T13:00:00Z", clientSecret: "cs_test_abc",
} as const;
const paidXmr = { ...openXmr, status: "paid", amountPaid: 42000, cryptoAmountPaid: "1.482", settledAt: "2026-08-28T12:05:00Z" } as const;
const expiredXmr = { ...openXmr, status: "expired", amountPaid: 21000, cryptoAmountPaid: "0.734" } as const;
const processingXmr = { ...openXmr, amountPaid: 42000, cryptoAmountPaid: "1.482", cryptoAmountDue: "0.001", paidInFull: true } as const;
const partPaidXmr = { ...openXmr, amountPaid: 21000, cryptoAmountPaid: "0.741", paidInFull: false } as const;

const createdXmr = {
  invoiceId: "inv_9f3a", badgeType: "legend", months: 12,
  amount: 42000, currency: "usd", expiresAt: "2026-08-28T13:00:00Z",
  address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr" as const,
};

function record(over: Partial<OrderRecord> = {}): OrderRecord {
  return {
    orderId: "inv_9f3a", badgeType: "legend", months: 12,
    createdAt: "2026-08-28T11:00:00Z", status: "open", ...over,
  };
}

// ------------------------------------------------------------- the plain read

apiTest("flow: readInvoice issues the plain GET, with no wait parameter", async () => {
  const net = new Net();
  net.script({ status: 200, body: openXmr });
  const view = await readInvoice("inv 9f3a/../x", net.fetch);
  assert.equal(net.urls[0], "/api/invoice/inv%209f3a%2F..%2Fx");
  assert.ok(!net.urls[0]!.includes("wait="), "the first load has nothing to compare against");
  assert.equal(view.status, "open");
  assert.equal(view.address, openXmr.address);
});

apiTest("flow: readInvoice refuses a 200 whose status is not one of the three, rather than passing it on", async () => {
  for (const body of [{ status: "settled" }, { status: 1 }, {}, null, "paid"]) {
    const net = new Net();
    net.script({ status: 200, body });
    await assert.rejects(() => readInvoice("x", net.fetch),
      (e: unknown) => e instanceof ApiError && e.code === "invalid_response",
      `body=${JSON.stringify(body)}`);
  }
});

apiTest("flow: readInvoice refuses a 200 the holding read refuses — the same body, the same verdict", async () => {
  // The plain read is the first screen of every `?order=` link, and casting the body rather than validating it
  // let a `cryptoCurrency` the design does not define reach `paymentUri`, the QR and the store: a `monero:` URI
  // over a Bitcoin address under the heading `Send 0.004 undefined`, redrawn from the record by the resume.
  const bodies: unknown[] = [
    { ...openXmr, cryptoCurrency: "doge" },
    { ...openXmr, cryptoCurrency: 7 },
    { ...openXmr, address: 42 },
    { ...openXmr, clientSecret: { toString: "no" } },
  ];
  for (const body of bodies) {
    const net = new Net();
    net.script({ status: 200, body });
    await assert.rejects(() => readInvoice("x", net.fetch),
      (e: unknown) => e instanceof ApiError && e.code === "invalid_response",
      `body=${JSON.stringify(body)}`);
  }
});

apiTest("flow: readInvoice returns the fields and nothing else it was sent", async () => {
  const net = new Net();
  net.script({ status: 200, body: { ...openXmr, nonsense: "<script>", cryptoCurrency: "btc" } });
  const view = await readInvoice("x", net.fetch);
  assert.equal((view as unknown as Record<string, unknown>).nonsense, undefined, "an unknown field is dropped, not passed on");
  assert.equal(view.cryptoCurrency, "btc");
  assert.equal(view.address, openXmr.address);
});

apiTest("flow: EVERY 404 is the unknown order, whatever body sits on it", async () => {
  // the unknown-order screen defines that screen by the status, and the promise about the body
  // binds this service, not the proxy or the CDN in front of it (Anubis puts
  // Anubis there). Keying on the body left the page with no local record
  // "Checking with the payment network" forever.
  const shapes: Array<[string, Reply]> = [
    ["the service's own body", { status: 404, body: { error: "not_found" } }],
    ["a proxy's HTML page", { status: 404, raw: "<html><body>404 Not Found</body></html>" }],
    ["an empty body", { status: 404, raw: "" }],
    ["unrelated JSON", { status: 404, body: { message: "no route matched" } }],
    ["a body naming another code", { status: 404, body: { error: "internal" } }],
  ];
  for (const [what, reply] of shapes) {
    const net = new Net();
    net.script(reply);
    await assert.rejects(() => readInvoice("x", net.fetch),
      (e: unknown) => e instanceof ApiError && e.code === "not_found" && e.status === 404, what);
  }
});

apiTest("flow: a 404 with a proxy's body still renders the unknown order, not a dead screen", async () => {
  const h = harness();
  h.net.script({ status: 404, raw: "<html>404</html>" });
  await h.flow.watch("inv_missing").done;
  assert.deepEqual(h.views, [{ screen: "unknownOrder", orderId: "inv_missing" }]);
});

apiTest("flow: a 404 with a proxy's body during the WAIT stops the loop too", async () => {
  const h = harness();
  h.net.script({ status: 200, body: openXmr }, { status: 404, raw: "<html>404</html>" });
  await h.flow.watch("inv_9f3a").done;
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingPayment", "unknownOrder"],
    "the payment screen must not be left standing with a live address and a dead loop");
});

apiTest("flow: readInvoice carries a 429's Retry-After through, so the history list can stop", async () => {
  const net = new Net();
  net.script({ status: 429, body: { error: "rate_limited" }, headers: { "retry-after": "46" } });
  await assert.rejects(() => readInvoice("x", net.fetch),
    (e: unknown) => e instanceof ApiError && e.code === "rate_limited" && e.retryAfter === 46);
});

// ----------------------------------------------------------- the local record

apiTest("flow: applyView overwrites the status and keeps the stored code", () => {
  const store = new Store(new Mem());
  store.saveOrder(record({ code: "SXB-AAAAA-BBBBB-CCCCC-DDDDD" }));
  const { record: after, savedLocally } = applyView(store, "inv_9f3a", paidXmr, Date.now());
  assert.equal(after.status, "paid");
  assert.equal(after.code, "SXB-AAAAA-BBBBB-CCCCC-DDDDD");
  assert.equal(savedLocally, true);
  assert.equal(store.order("inv_9f3a")!.status, "paid");
  assert.equal(store.order("inv_9f3a")!.code, "SXB-AAAAA-BBBBB-CCCCC-DDDDD");
});

apiTest("flow: the `submitted` survives every 200, and a store that refused the write", () => {
  // It is sticky in the same direction as the code and for a harder reason: a
  // browser that has confirmed a card payment for this order never
  // un-confirms it, and the screen the flag selects is the one that withholds
  // every control that could charge the buyer twice.
  const store = new Store(new Mem());
  store.saveOrder(record({ status: "open", submitted: true }));
  const kept = applyView(store, "inv_9f3a", openCard, Date.parse("2026-08-28T12:00:00Z"));
  assert.equal(kept.record.submitted, true);
  assert.equal(store.order("inv_9f3a")!.submitted, true, "and it is still on the stored entry");

  // The store refusing the write is the private-browsing case: the flag has
  // to survive in memory, or the next render puts a live Pay button back.
  const refusing = new Store(new Refusing());
  const memory = record({ status: "open", submitted: true });
  const inMemory = applyView(refusing, "inv_9f3a", openCard, Date.parse("2026-08-28T12:00:00Z"), memory);
  assert.equal(inMemory.record.submitted, true);
  assert.equal(viewFor(inMemory.record, openCard, "card", { savedLocally: false, resumed: true }).screen, "awaitingConfirmation");
});

apiTest("flow: a stored entry with no code does not overwrite the code held in memory", () => {
  // The line above `submitted` in `localState`, and the one that matters most: the code exists
  // in no other place, and the service keeps nothing but its hash.
  // The store answering with an entry that has no code is the case: a second tab wrote this order
  // before the code existed, and `base` is then the stored one. Only the loop's own memory still
  // has it, and a 200 that read the code off `base` alone would drop the whole purchase.
  const shared = new Store(new Mem());
  shared.saveOrder(record({ status: "open" }));
  const held = record({ status: "open", code: "SXB-YDC8A-YGQTM-PUYZ9-2TUXP" });
  const applied = applyView(shared, "inv_9f3a", openXmr, Date.parse("2026-08-28T12:00:00Z"), held);
  assert.equal(applied.record.code, "SXB-YDC8A-YGQTM-PUYZ9-2TUXP", "the answer must not drop it");
  assert.equal(shared.order("inv_9f3a")!.code, "SXB-YDC8A-YGQTM-PUYZ9-2TUXP", "and it is written back");
});

apiTest("flow: a 200 with no entry present creates one from the response, holding no code", () => {
  const store = new Store(new Mem());
  const nowMs = Date.parse("2026-08-28T12:00:00Z");
  const { record: made } = applyView(store, "inv_other", paidXmr, nowMs, undefined, "xmr");
  assert.deepEqual(made, {
    orderId: "inv_other", badgeType: "legend", months: 12,
    createdAt: "2026-08-28T12:00:00.000Z", status: "paid",
    // what was paid and how, on a record this browser did not create: the price off the
    // response, the method off `inferMethod`, and the amounts the history list needs to name the state
    amountPaid: 42000, cryptoAmountPaid: "1.482",
    amount: 42000, currency: "usd", method: "xmr",
  });
  assert.equal("paidInFull" in made, false, "a field the response left out is absent, not undefined");
  assert.equal(made.code, undefined, "a second device can never hold a code");
});

apiTest("flow: a method that cannot be inferred leaves the row without one, rather than guessing", () => {
  const store = new Store(new Mem());
  const nowMs = Date.parse("2026-08-28T12:00:00Z");
  // Every settled response is this shape: no `clientSecret` and no
  // `cryptoCurrency`, so `inferMethod` answers undefined.
  const { record: made } = applyView(store, "inv_other", { status: "paid", amount: 42000, currency: "usd" }, nowMs);
  assert.equal(made.method, undefined);
  assert.equal("method" in made, false, "an absent method is absent, not a key holding undefined");
  assert.equal(made.amount, 42000, "and the price is still there to print");
});

apiTest("flow: a refusing store still yields the record, and says the code was not saved", () => {
  const store = new Store(new Refusing());
  const memory = record({ code: "SXB-AAAAA-BBBBB-CCCCC-DDDDD" });
  const { record: after, savedLocally } = applyView(store, "inv_9f3a", paidXmr, Date.now(), memory);
  assert.equal(after.status, "paid");
  assert.equal(after.code, "SXB-AAAAA-BBBBB-CCCCC-DDDDD", "the flow keeps working from memory");
  assert.equal(savedLocally, false, "the code screen must drop its saved-copy clause");
});

apiTest("flow: recordFromView fills the fields the read endpoint sends and defaults the rest", () => {
  const nowMs = Date.parse("2026-08-28T12:00:00Z");
  assert.deepEqual(recordFromView("i", { status: "open" }, nowMs), {
    orderId: "i", badgeType: "", months: 0,
    createdAt: "2026-08-28T12:00:00.000Z", status: "open",
  });
});

// ------------------------------------------------------------------ the views

apiTest("flow: orderPhase names every state the order screen and the history both read", () => {
  const open = { status: "open" as const };
  assert.equal(orderPhase(open), "awaiting");
  // the provider reports a payment on a New invoice too, so money alone is not "paid"
  assert.equal(orderPhase({ ...open, cryptoAmountPaid: "0.741", paidInFull: false }), "partPaid");
  assert.equal(orderPhase({ ...open, cryptoAmountPaid: "1.482", paidInFull: true }), "processing");
  // the verdict is the provider's: it tolerates a small underpayment, so the amounts can
  // say "short" for an invoice it has already accepted
  assert.equal(orderPhase({ ...open, cryptoAmountPaid: "1.475", paidInFull: true }), "processing");
  // and an older record, written before the field existed, must not claim to be paid
  assert.equal(orderPhase({ ...open, cryptoAmountPaid: "1.482" }), "partPaid");
  assert.equal(orderPhase({ status: "paid" }), "paid");
  assert.equal(orderPhase({ status: "expired", cryptoAmountPaid: "0.7", paidInFull: false }), "expired");
});

apiTest("flow: a view with no payment clears one the record was holding", () => {
  const store = new Store(new Mem());
  const nowMs = Date.parse("2026-08-28T12:00:00Z");
  const funded = { ...openXmr, amountPaid: 42000, cryptoAmountPaid: "1.482", paidInFull: true };
  applyView(store, "inv_9f3a", funded, nowMs, undefined, "xmr");
  assert.equal(historyRows(store.orders())[0]!.kind, "processing");
  const { record } = applyView(store, "inv_9f3a", openXmr, nowMs, undefined, "xmr");
  assert.equal(record.paidInFull, undefined);
  assert.equal(record.cryptoAmountPaid, undefined);
  assert.equal(historyRows(store.orders())[0]!.kind, "open");
});

apiTest("flow: the history row names the same state the order screen shows", () => {
  const base = record({ status: "open", amount: 42000 });
  const kinds = (over: Partial<OrderRecord>) => historyRows([{ ...base, ...over }])[0]!.kind;
  assert.equal(kinds({}), "open");
  assert.equal(kinds({ cryptoAmountPaid: "0.741", paidInFull: false }), "partPaid");
  // the bug this replaces: a funded order's row read "waiting for payment" while its own
  // screen read "Payment received"
  assert.equal(kinds({ cryptoAmountPaid: "1.482", paidInFull: true }), "processing");
  assert.equal(kinds({ status: "expired" }), "expired");
});

// The history reads the phase off the stored record and the order's own screen reads it off
// the live view, so the two agree only while the record is a faithful copy of the last view.
// Both stale-page bugs were this invariant breaking, in opposite directions. The store is
// seeded funded every time, so a field that falls back instead of clearing fails here.
apiTest("flow: the stored record names the phase of the view it was written from", () => {
  const nowMs = Date.parse("2026-08-28T12:00:00Z");
  const views: InvoiceView[] = [openXmr, partPaidXmr, processingXmr, paidXmr, expiredXmr, { status: "open" }];
  for (const view of views) {
    const store = new Store(new Mem());
    store.saveOrder(record({ ...HELD, method: "xmr", amountPaid: 42000, cryptoAmountPaid: "1.482", paidInFull: true }));
    const { record: after } = applyView(store, "inv_9f3a", view, nowMs, undefined, "xmr");
    const phase = orderPhase(view);
    assert.equal(orderPhase(after), phase, `${JSON.stringify(view)}: the record in hand`);
    assert.equal(orderPhase(store.order("inv_9f3a")!), phase, `${JSON.stringify(view)}: and the one in the store`);
  }
});

// The offline page reads its view from the record instead of the network, so it needs the same
// agreement. It did not have it: a fully paid invoice came back as "awaiting" and drew the
// address and the countdown again, inviting a second payment.
apiTest("flow: the offline view names the phase of the record it was rebuilt from", () => {
  const held = { ...HELD, method: "xmr" as const };
  for (const paid of [
    {},
    { amountPaid: 21000, cryptoAmountPaid: "0.741", cryptoAmountDue: "0.745", paidInFull: false },
    { amountPaid: 42000, cryptoAmountPaid: "1.482", cryptoAmountDue: "0.000", paidInFull: true },
  ]) {
    const stored = record({ ...held, ...paid });
    const offline = offlineInvoice(stored)!;
    assert.equal(orderPhase(offline), orderPhase(stored), `${JSON.stringify(paid)}`);
    assert.equal(offline.cryptoAmountDue, stored.cryptoAmountDue,
      "the remainder is carried too: without it the part-paid screen has no figure to ask for");
    const screen = viewFor(stored, offline, "xmr", { savedLocally: false, resumed: false }).screen;
    assert.equal(screen, orderPhase(stored) === "processing" ? "awaitingConfirmation" : "awaitingPayment",
      "a paid invoice must not show its address again");
  }
});

// The general form of the rule, so a field added later cannot quietly grow a fallback: what
// the service stops sending is gone. Only the price and the destination are exempt, and only
// for the reasons `serverState` gives.
apiTest("flow: a field the service stops sending is cleared, not remembered", () => {
  const nowMs = Date.parse("2026-08-28T12:00:00Z");
  const store = new Store(new Mem());
  applyView(store, "inv_9f3a", processingXmr, nowMs, undefined, "xmr");
  const { record: after } = applyView(store, "inv_9f3a", { status: "open" }, nowMs, undefined, "xmr");
  for (const key of ["amountPaid", "cryptoAmountPaid", "cryptoAmountDue", "paidInFull"] as const) {
    assert.equal(after[key], undefined, `${key} must not outlive the answer that carried it`);
    assert.equal(key in after, false, `${key} must be absent, not a key holding undefined`);
  }
  assert.equal(after.amount, 42000, "the price is settled at creation and a wait answer may omit it");
  assert.equal(after.address, "48HqK2XmVexampleAddress9fRtWc", "and the destination is still payable");
});

apiTest("flow: viewFor leaves the payment screen for the processing screen once a payment is seen", () => {
  const open = viewFor(record(), openXmr, "xmr", { savedLocally: false, resumed: false });
  assert.equal(open.screen, "awaitingPayment");
  // the invoice is still open: the provider reports the payment before it confirms, and
  // showing the address after that invites a second one
  const seen = viewFor(record(), processingXmr, "xmr", { savedLocally: false, resumed: false });
  assert.equal(seen.screen, "awaitingConfirmation");
  assert.equal(seen.screen === "awaitingConfirmation" ? seen.method : undefined, "xmr");
  assert.equal(seen.screen === "awaitingConfirmation" ? seen.gaveUp : true, false);
  // BTCPay reports a payment on a New invoice too, so an underpayment must stay on the payment screen:
  // hiding the address would leave the buyer unable to send the rest
  const part = viewFor(record(), partPaidXmr, "xmr", { savedLocally: false, resumed: false });
  assert.equal(part.screen, "awaitingPayment");
});

apiTest("flow: viewFor renders the code screen with the code only on a paid order", () => {
  const paid = record({ status: "paid", code: "SXB-Y-Y-Y-Y" });
  const v = viewFor(paid, paidXmr, "xmr", { savedLocally: true, resumed: false });
  assert.equal(v.screen, "codeIssued");
  assert.equal(v.screen === "codeIssued" ? v.code : undefined, "SXB-Y-Y-Y-Y");
});

apiTest("flow: viewFor renders the paid-no-code screen for a paid order this browser has no code for", () => {
  const v = viewFor(record({ status: "paid" }), paidXmr, "xmr", { savedLocally: false, resumed: false });
  assert.equal(v.screen, "paidNoCode");
});

apiTest("flow: viewFor never renders a code on an unpaid order, whatever the store holds", () => {
  const held = "SXB-AAAAA-BBBBB-CCCCC-DDDDD";
  const cases: Array<[OrderRecord, InvoiceView, "btc" | "xmr" | "card"]> = [
    [record({ status: "open", code: held }), openXmr, "xmr"],
    [record({ status: "open", code: held }), openCard, "card"],
    [record({ status: "open", code: held, submitted: true }), openCard, "card"],
    [record({ status: "expired", code: held }), expiredXmr, "xmr"],
  ];
  for (const [rec, invoice, method] of cases) {
    const v = viewFor(rec, invoice, method, { savedLocally: true, resumed: false });
    assert.notEqual(v.screen, "codeIssued", `${rec.status}/${method}/${rec.submitted === true} must not be the code screen`);
    assert.equal((v as { code?: string }).code, undefined,
      `${v.screen} carried a code for a ${rec.status} order`);
  }
});

apiTest("flow: viewFor follows the open/card table — the form, then the confirming screen once confirm succeeded", () => {
  const open = record({ status: "open" });
  const form = viewFor(open, openCard, "card", { savedLocally: false, resumed: true });
  assert.equal(form.screen, "cardForm");
  assert.equal(form.screen === "cardForm" ? form.clientSecret : "", "cs_test_abc");
  assert.equal(form.screen === "cardForm" ? form.resumed : false, true);
  const waiting = viewFor(record({ status: "open", submitted: true }), openCard, "card", { savedLocally: false, resumed: false });
  assert.equal(waiting.screen, "awaitingConfirmation");
});

apiTest("flow: viewFor renders the payment screen for crypto, carrying the address the QR needs", () => {
  const v = viewFor(record(), openXmr, "xmr", { savedLocally: false, resumed: false });
  assert.equal(v.screen, "awaitingPayment");
  assert.equal(v.screen === "awaitingPayment" ? v.invoice.address : "", openXmr.address);
  assert.equal(v.screen === "awaitingPayment" ? v.method : "card", "xmr");
});

apiTest("flow: an open order whose method cannot be inferred renders the reference, not a blank page", () => {
  const bare: InvoiceView = { status: "open" };
  const v = viewFor(record(), bare, undefined, { savedLocally: false, resumed: false });
  assert.equal(v.screen, "detailsUnavailable");
  assert.equal(v.screen === "detailsUnavailable" ? v.order.orderId : "", "inv_9f3a");
});

apiTest("flow: a paid order still renders the code screen when the method cannot be inferred — only `open` needs it", () => {
  const paid = record({ status: "paid", code: "SXB-Y" });
  assert.equal(viewFor(paid, { status: "paid" }, undefined, { savedLocally: true, resumed: false }).screen, "codeIssued");
  const expired = record({ status: "expired" });
  assert.equal(viewFor(expired, { status: "expired" }, undefined, { savedLocally: false, resumed: false }).screen, "windowClosed");
});

// ------------------------------------------------------------------- checkout

apiTest("flow: a 200 saves the order with the display code and leaves the session alone", async () => {
  const h = harness();
  h.store.saveSession({ step: "checkout", priceId: "price_legend" });
  h.net.script({ status: 200, body: createdXmr });
  const out = await h.flow.checkout({ priceId: "price_legend", offerId: "offer_12m", method: "xmr" });
  assert.equal(out.kind, "created");
  assert.equal(h.net.urls[0], "/api/invoice");
  const stored = h.store.order("inv_9f3a")!;
  assert.equal(stored.status, "open");
  assert.equal(stored.code, `SXB-${h.codes[0]!.match(/.{1,5}/g)!.join("-")}`);
  // spending the draft is `pay`'s call, not this one's: only the page knows whether the buyer has
  // chosen again since, and clearing a choice they made while this was on the wire loses it
  assert.equal(h.store.session().priceId, "price_legend", "the draft is not this call's to spend");
});

// "Remove every code stored in this browser? This cannot be undone." has to mean it, including
// for an invoice that was already on the wire when it was answered. The sale stands at the
// service; this browser keeps nothing, and says so rather than claiming the code is here.
apiTest("flow: a wipe while the invoice is bought leaves this browser holding nothing", async () => {
  const net = new Net();
  net.script({ status: 200, body: createdXmr });
  let store: Store | undefined;
  const wipingFetch = (async (u: unknown, i: unknown) => {
    store!.forgetEverything();
    return net.fetch(u as string, i as RequestInit);
  }) as unknown as typeof fetch;
  const h = harness(new Mem(), wipingFetch);
  store = h.store;

  const out = await h.flow.checkout({ priceId: "price_legend", offerId: "offer_12m", method: "xmr" });
  assert.equal(out.kind, "created", "the invoice really was bought");
  assert.equal(out.kind === "created" && out.savedLocally, false, "and this browser does not claim to hold it");
  assert.equal(h.store.order("inv_9f3a"), undefined, "the emptied store stays empty");
});

apiTest("flow: catalog_changed lands on the catalog-changed screen and nothing is stored", async () => {
  const h = harness();
  h.net.script({ status: 400, body: { error: "catalog_changed" } });
  const out = await h.flow.checkout({ priceId: "p", method: "xmr" });
  assert.deepEqual(out, { kind: "catalogChanged" });
  assert.equal(h.store.orders().length, 0);
});

apiTest("flow: rate_limited lands on the rate-limited screen carrying exactly the Retry-After seconds", async () => {
  const h = harness();
  h.net.script({ status: 429, body: { error: "rate_limited" }, headers: { "retry-after": "46" } });
  assert.deepEqual(await h.flow.checkout({ priceId: "p", method: "xmr" }), { kind: "rateLimited", retryAfter: 46 });
});

apiTest("flow: a 429 naming no usable Retry-After falls back to the window", async () => {
  const h = harness();
  h.net.script({ status: 429, body: { error: "rate_limited" } });
  assert.deepEqual(await h.flow.checkout({ priceId: "p", method: "xmr" }),
    { kind: "rateLimited", retryAfter: DEFAULT_RETRY_AFTER_SECONDS });
});

apiTest("flow: provider_unavailable lands on the provider-unavailable screen naming the method that failed", async () => {
  const h = harness();
  h.net.script({ status: 503, body: { error: "provider_unavailable" } });
  assert.deepEqual(await h.flow.checkout({ priceId: "p", method: "xmr" }), { kind: "providerUnavailable", method: "xmr" });
});

apiTest("flow: bad_request, 500 and a network failure all land on the failure screen", async () => {
  for (const reply of [{ status: 400, body: { error: "bad_request" } }, { status: 500, body: { error: "internal" } }]) {
    const h = harness();
    h.net.script(reply);
    assert.deepEqual(await h.flow.checkout({ priceId: "p", method: "card" }), { kind: "failed" });
  }
  const h = harness();
  const failing = (async () => { throw new TypeError("network"); }) as unknown as typeof fetch;
  const flow = new Flow({
    store: h.store, fetch: failing, sleep: h.clock.sleep, now: h.clock.now,
    newCode: () => "CODE0000000000000000", hashCode: async (c) => c, render: () => {},
  });
  assert.deepEqual(await flow.checkout({ priceId: "p", method: "card" }), { kind: "failed" });
});

apiTest("flow: code_conflict draws a NEW code and resubmits, invisibly to the buyer", async () => {
  const h = harness();
  h.net.script({ status: 409, body: { error: "code_conflict" } }, { status: 200, body: createdXmr });
  const out = await h.flow.checkout({ priceId: "price_legend", method: "xmr" });
  assert.equal(out.kind, "created", "the conflict never reaches a screen");
  assert.equal(h.net.calls.length, 2);
  const first = JSON.parse(String(h.net.calls[0]!.init!.body)).codeHash;
  const second = JSON.parse(String(h.net.calls[1]!.init!.body)).codeHash;
  assert.notEqual(first, second, "the same code must never be resubmitted");
  assert.equal(h.store.orders().length, 1, "the conflicting code leaves no entry behind");
});

apiTest("flow: a server stuck on code_conflict gives up bounded, rather than spinning", async () => {
  const h = harness();
  for (let i = 0; i < CODE_ATTEMPTS + 5; i++) h.net.script({ status: 409, body: { error: "code_conflict" } });
  assert.deepEqual(await h.flow.checkout({ priceId: "p", method: "xmr" }), { kind: "failed" });
  assert.equal(h.net.calls.length, CODE_ATTEMPTS);
});

apiTest("flow: a store that refuses the write still creates the order, flagged unsaved", async () => {
  const h = harness(new Refusing());
  h.net.script({ status: 200, body: createdXmr });
  const out = await h.flow.checkout({ priceId: "p", method: "xmr" });
  assert.equal(out.kind, "created");
  assert.equal(out.kind === "created" ? out.savedLocally : true, false);
  assert.ok(out.kind === "created" && out.order.code !== undefined, "the code is still in hand");
});

// --------------------------------------------------------------- the watching

apiTest("flow: a paid first read renders the code screen and issues no hold at all", async () => {
  const h = harness();
  h.store.saveOrder(record({ code: "SXB-Y" }));
  h.net.script({ status: 200, body: paidXmr });
  const w = h.flow.watch("inv_9f3a");
  await w.done;
  assert.deepEqual(h.views.map((v) => v.screen), ["codeIssued"]);
  assert.equal(h.net.calls.length, 1, "paid stops the loop");
  assert.equal(h.store.order("inv_9f3a")!.status, "paid");
});

apiTest("flow: expired KEEPS WAITING, so a last-second payment turns the closed-window screen into the code screen", async () => {
  const h = harness();
  h.store.saveOrder(record({ code: "SXB-Y" }));
  h.net.script(
    { status: 200, body: openXmr },      // the plain first read
    { status: 200, body: expiredXmr },   // ?wait=open answers "expired"
    { status: 200, body: paidXmr },      // ?wait=expired answers "paid"
  );
  const w = h.flow.watch("inv_9f3a");
  await w.done;
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingPayment", "windowClosed", "codeIssued"]);
  assert.deepEqual(h.net.urls, [
    "/api/invoice/inv_9f3a",
    "/api/invoice/inv_9f3a?wait=open&seenPaid=&seenFull=0",
    // the expired answer carried a part payment, and the next wait says so: the service
    // answers at once rather than parking when it holds a payment this page has not seen
    "/api/invoice/inv_9f3a?wait=expired&seenPaid=0.734&seenFull=0",
  ]);
});

apiTest("flow: no pass ever asks the read endpoint to wait on 'paid', which the client refuses by contract", async () => {
  const h = harness();
  h.store.saveOrder(record());
  h.net.script(
    { status: 200, body: openXmr },
    { status: 200, body: expiredXmr },
    { status: 200, body: paidXmr },
  );
  await h.flow.watch("inv_9f3a").done;
  for (const url of h.net.urls) assert.ok(!url.includes("wait=paid"), url);
});

apiTest("flow: every 200 updates the local record BEFORE rendering", async () => {
  const store = new Store(new Mem());
  store.saveOrder(record({ code: "SXB-Y" }));
  const net = new Net();
  const clock = new Clock();
  const seen: Array<{ screen: string; storedStatus: string | undefined }> = [];
  const flow = new Flow({
    store, fetch: net.fetch, sleep: clock.sleep, now: clock.now,
    newCode: () => "CODE0000000000000000", hashCode: async (c) => c,
    render: (v) => { seen.push({ screen: v.screen, storedStatus: store.order("inv_9f3a")?.status }); },
  });
  net.script({ status: 200, body: openXmr }, { status: 200, body: paidXmr });
  await flow.watch("inv_9f3a").done;
  assert.deepEqual(seen, [
    { screen: "awaitingPayment", storedStatus: "open" },
    { screen: "codeIssued", storedStatus: "paid" },
  ]);
});

apiTest("flow: not even a paid order renders its code before the store agrees it is paid", async () => {
  const h = harness();
  h.store.saveOrder(record({ code: "SXB-AAAAA-BBBBB-CCCCC-DDDDD" }));
  h.net.script({ status: 200, body: openXmr }, { status: 200, body: expiredXmr }, { status: 200, body: paidXmr });
  await h.flow.watch("inv_9f3a").done;
  for (const v of h.views) {
    const code = (v as { code?: string }).code;
    if (code !== undefined) {
      assert.equal(v.screen, "codeIssued", "only the code screen may carry a code");
      assert.equal((v as { order: OrderRecord }).order.status, "paid");
    }
  }
  assert.deepEqual(h.views.filter((v) => (v as { code?: string }).code !== undefined).map((v) => v.screen), ["codeIssued"]);
});

apiTest("flow: a 404 on the first read renders the unknown order without waiting", async () => {
  const h = harness();
  h.net.script({ status: 404, body: { error: "not_found" } });
  await h.flow.watch("inv_missing").done;
  assert.deepEqual(h.views, [{ screen: "unknownOrder", orderId: "inv_missing" }]);
  assert.equal(h.net.calls.length, 1);
});

apiTest("flow: exactly one loop per order — a second watch() rejoins the first", async () => {
  const h = harness();
  const w1 = h.flow.watch("inv_9f3a", { initial: openXmr, method: "xmr" });
  await settle();
  assert.equal(h.net.pendingCount, 1);
  const w2 = h.flow.watch("inv_9f3a", { initial: openXmr, method: "xmr" });
  const w3 = h.flow.watch("inv_9f3a", { initial: openXmr, method: "xmr" });
  await settle();
  assert.equal(w2, w1);
  assert.equal(w3, w1);
  assert.equal(h.net.pendingCount, 1, "a second loop would hold a second connection");
  assert.equal(h.net.maxInFlight, 1, "two live loops double the rate against the 60/min");
  assert.equal(h.net.calls.length, 1);
  w1.stop();
  await settle();
  assert.equal(h.net.pendingCount, 0, "stop must release the held connection");
  await w1.done;
});

apiTest("flow: suspend aborts the held request, and resume restarts exactly one", async () => {
  const h = harness();
  const w = h.flow.watch("inv_9f3a", { initial: openXmr, method: "xmr" });
  await settle();
  assert.equal(h.net.pendingCount, 1);
  for (let cycle = 0; cycle < 3; cycle++) {
    w.suspend();
    await settle();
    assert.equal(h.net.pendingCount, 0, `cycle ${cycle}: the hidden tab must hold nothing`);
    w.resume();
    w.resume(); // a duplicate visibilitychange must change nothing
    w.resume();
    await settle();
    assert.equal(h.net.pendingCount, 1, `cycle ${cycle}: exactly one connection after resume`);
    assert.equal(h.net.maxInFlight, 1, `cycle ${cycle}: never two at once`);
  }
  assert.equal(h.net.calls.length, 4, "one initial hold plus one per resume");
  w.stop();
  await settle();
  assert.equal(h.net.pendingCount, 0, "stop must release the held connection");
  await w.done;
});

apiTest("flow: the loop survives suspend/resume and still reports the settlement", async () => {
  const h = harness();
  h.store.saveOrder(record({ code: "SXB-AAAAA-BBBBB-CCCCC-DDDDD" }));
  const w = h.flow.watch("inv_9f3a", { initial: openXmr, method: "xmr" });
  await settle();
  w.suspend();
  await settle();
  w.resume();
  await settle();
  h.net.answerHeld({ status: 200, body: paidXmr });
  await w.done;
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingPayment", "codeIssued"]);
  assert.equal(h.net.calls.length, 2, "the resumed loop is the same loop, not a second one");
});

apiTest("flow: an expired order paints from the record while the read is retried", async () => {
  // `offlineInvoice` rebuilds only an open order, so a closed one used to wait on the network
  // to say what the record already says, and the screen for it draws no address, so there is
  // nothing to be wrong about.
  const failing = (async () => { throw new TypeError("offline"); }) as unknown as typeof fetch;
  const clock = new Clock();
  const views: PaymentView[] = [];
  const store = new Store(new Mem());
  store.saveOrder(record({ status: "expired", code: "SXB-Y" }));
  const flow = new Flow({
    store, fetch: failing, sleep: clock.sleep, now: clock.now,
    newCode: () => "C", hashCode: async (c) => c, render: (v) => { views.push(v); },
  });
  const w = flow.watch("inv_9f3a", { resumed: true });
  await settle();
  assert.deepEqual(views.map((v) => v.screen), ["windowClosed"],
    "the record is enough to say the window closed");
  w.stop();
  await w.done;
});

apiTest("flow: a first read that fails is read again, not held on", async () => {
  // The hold answers only on a change, so a watch that entered one having painted nothing left
  // the buyer on the spinner for as long as the invoice stayed open, as an open invoice
  // does. This is the second device, the bookmark and the history list's [ Open ].
  let call = 0;
  const flaky = (async (): Promise<Response> => {
    call += 1;
    if (call === 1) throw new TypeError("offline");
    return {
      ok: true, status: 200, headers: { get: () => null },
      json: async () => openXmr, text: async () => JSON.stringify(openXmr),
    } as unknown as Response;
  }) as unknown as typeof fetch;
  const clock = new Clock();
  const views: PaymentView[] = [];
  const flow = new Flow({
    store: new Store(new Mem()), fetch: flaky, sleep: clock.sleep, now: clock.now,
    newCode: () => "C", hashCode: async (c) => c, render: (v) => { views.push(v); },
  });
  const w = flow.watch("inv_9f3a");
  await settle();
  assert.equal(views.length, 0, "nothing is rendered from a read that never arrived");
  await clock.advance(BACKOFF_START);
  await settle();
  assert.deepEqual(views.map((v) => v.screen), ["awaitingPayment"],
    "the payment screen must arrive without the invoice having to change");
  assert.ok(call >= 2, "and it took a second read to get there, the first having failed");
  w.stop();
  await w.done;
});

apiTest("flow: a verdict with no figure moves the buyer off the payment screen", async () => {
  // Monero reports an invoice as confirming while `paymentMethodPaid` is still zero, so the
  // service records the verdict with no figure. Keyed on the figure alone, the loop discarded
  // that body every pass and the buyer sat on a live QR with the money already sent.
  const h = harness();
  const confirming = { ...openXmr, paidInFull: true } as const;
  h.net.script({ status: 200, body: openXmr }, { status: 200, body: confirming });
  const w = h.flow.watch("inv_9f3a");
  await settle();
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingPayment", "awaitingConfirmation"],
    "the verdict is the whole difference between those two screens");
  w.stop();
  await w.done;
});

apiTest("flow: the confirming screen gives up after exactly fifteen minutes, counted from its first render", async () => {
  const h = harness();
  h.store.saveOrder(record({ status: "open", submitted: true }));
  const w = h.flow.watch("inv_9f3a", { initial: openCard, method: "card" });
  await settle();
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingConfirmation"]);
  assert.equal(h.views[0]!.screen === "awaitingConfirmation" ? h.views[0]!.gaveUp : true, false);

  await h.clock.advance(GIVE_UP_MS - 1000);
  assert.equal(h.views.length, 1, "one second short of fifteen minutes it is still waiting");

  await h.clock.advance(1000);
  await w.done;
  assert.equal(h.views.length, 2);
  const last = h.views[1]!;
  assert.equal(last.screen, "awaitingConfirmation");
  assert.equal(last.screen === "awaitingConfirmation" ? last.gaveUp : false, true);
  assert.equal(h.net.pendingCount, 0, "giving up must not leave a connection held");
});

apiTest("flow: nothing else gives up — the payment screen is still waiting an hour later", async () => {
  const h = harness();
  const w = h.flow.watch("inv_9f3a", { initial: openXmr, method: "xmr" });
  await settle();
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingPayment"]);
  await h.clock.advance(4 * GIVE_UP_MS);
  const outcome = await Promise.race([w.done.then(() => "ended" as const), flush().then(() => "waiting" as const)]);
  assert.equal(outcome, "waiting", "only the confirming screen gives up");
  assert.equal(h.views.length, 1);
  assert.equal(h.net.pendingCount, 1);
  w.stop();
  await settle();
  assert.equal(h.net.pendingCount, 0, "stop must release the held connection");
  await w.done;
});

apiTest("flow: the closed-window screen keeps waiting for an hour too, so a late settlement still shows the code", async () => {
  const h = harness();
  const w = h.flow.watch("inv_9f3a", { initial: expiredXmr, method: "xmr" });
  await settle();
  assert.deepEqual(h.views.map((v) => v.screen), ["windowClosed"]);
  await h.clock.advance(4 * GIVE_UP_MS);
  assert.equal(h.net.pendingCount, 1);
  assert.equal(h.net.urls[0], "/api/invoice/inv_9f3a?wait=expired&seenPaid=0.734&seenFull=0");
  w.stop();
  await settle();
  assert.equal(h.net.pendingCount, 0, "stop must release the held connection");
  await w.done;
});

apiTest("flow: a card order that settles before the deadline renders the code screen and disarms the give-up", async () => {
  const h = harness();
  h.store.saveOrder(record({ status: "open", code: "SXB-Y", submitted: true }));
  const w = h.flow.watch("inv_9f3a", { initial: openCard, method: "card" });
  await settle();
  await h.clock.advance(GIVE_UP_MS - 60_000);
  h.net.answerHeld({ status: 200, body: { ...paidXmr, clientSecret: "cs_test_abc", address: undefined, cryptoAmount: undefined, cryptoCurrency: undefined } });
  await w.done;
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingConfirmation", "codeIssued"]);
});

apiTest("flow: the method is inferred from the response, and a later response that omits it keeps the one resolved", async () => {
  const h = harness();
  h.net.script(
    { status: 200, body: openXmr },
    { status: 200, body: { status: "expired" } },  // the read endpoint sends the details only when needed
    { status: 200, body: { status: "paid" } },
  );
  await h.flow.watch("inv_9f3a").done;
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingPayment", "windowClosed", "paidNoCode"]);
});

apiTest("flow: an open order whose response names no method renders detailsUnavailable and keeps waiting", async () => {
  const h = harness();
  h.net.script({ status: 200, body: { status: "open" } });
  const w = h.flow.watch("inv_9f3a");
  await settle();
  assert.deepEqual(h.views.map((v) => v.screen), ["detailsUnavailable"]);
  assert.equal(h.net.pendingCount, 1, "the order exists; the loop still waits for it to settle");
  w.stop();
  await settle();
  assert.equal(h.net.pendingCount, 0, "stop must release the held connection");
  await w.done;
});

apiTest("flow: a failed first read renders nothing, and its retry is cancellable", async () => {
  const h = offlineHarness();
  h.store.saveOrder(record({ code: "SXB-Y" }));
  const w = h.flow.watch("inv_9f3a");
  await settle();
  assert.deepEqual(h.views, [], "nothing is rendered from a read that never arrived");
  w.stop();
  // The loop is parked in the backoff delay, not on a connection; stopping
  // must cancel that delay rather than wait it out.
  await Promise.race([w.done, flush().then(() => { throw new Error("stop left the loop in its backoff"); })]);
});

// ------------------------------------------------------------------------- the history list

apiTest("flow: refreshHistory re-reads only open and expired, newest first, at most ten", async () => {
  const h = harness();
  for (let i = 0; i < 14; i++) {
    h.store.saveOrder(record({
      orderId: `inv_${String(i).padStart(2, "0")}`,
      createdAt: new Date(Date.parse("2026-08-01T00:00:00Z") + i * 60_000).toISOString(),
      status: i % 2 === 0 ? "open" : "paid",
      ...(i % 2 === 1 ? { code: "SXB-Y" } : {}),
    }));
  }
  for (let i = 0; i < 20; i++) h.net.script({ status: 200, body: paidXmr });
  await h.flow.refreshHistory();
  assert.equal(h.net.calls.length, 7, "seven open entries, none of the paid ones");
  assert.deepEqual(h.net.urls[0], "/api/invoice/inv_12", "newest first");
  assert.ok(h.net.urls.every((u) => !u.includes("wait=")), "the history list does not wait");
});

apiTest("flow: refreshHistory caps at ten requests however many entries are stale", async () => {
  const h = harness();
  for (let i = 0; i < 20; i++) {
    h.store.saveOrder(record({
      orderId: `inv_${String(i).padStart(2, "0")}`,
      createdAt: new Date(Date.parse("2026-08-01T00:00:00Z") + i * 60_000).toISOString(),
      status: "expired",
    }));
  }
  for (let i = 0; i < 30; i++) h.net.script({ status: 200, body: expiredXmr });
  await h.flow.refreshHistory();
  assert.equal(h.net.calls.length, 10);
});

apiTest("flow: refreshHistory stops on a 429 rather than walking the rest of the list", async () => {
  const h = harness();
  for (let i = 0; i < 5; i++) {
    h.store.saveOrder(record({
      orderId: `inv_${i}`,
      createdAt: new Date(Date.parse("2026-08-01T00:00:00Z") + i * 60_000).toISOString(),
      status: "open",
    }));
  }
  h.net.script({ status: 200, body: paidXmr }, { status: 429, body: { error: "rate_limited" } }, { status: 200, body: paidXmr });
  await h.flow.refreshHistory();
  assert.equal(h.net.calls.length, 2);
});

apiTest("flow: refreshHistory leaves a 404'd entry exactly as it was", async () => {
  const h = harness();
  h.store.saveOrder(record({ status: "open", code: "SXB-Y" }));
  h.net.script({ status: 404, body: { error: "not_found" } });
  const after = await h.flow.refreshHistory();
  assert.equal(after.length, 1);
  assert.equal(after[0]!.status, "open");
  assert.equal(after[0]!.code, "SXB-Y");
});

// ------------------------------------------------------------------ formatting

apiTest("flow: money prints the exact figure the order summary and the payment screen carry", () => {
  assert.equal(money(42000, "usd"), "$420.00");
  assert.equal(money(700, "usd"), "$7.00");
  assert.equal(money(4299, "usd"), "$42.99");
  assert.equal(money(42000, "chf"), "420.00 CHF");
});

apiTest("flow: moneyCompact drops a zero remainder, as the tier list and the duration list do", () => {
  assert.equal(moneyCompact(700, "usd"), "$7");
  assert.equal(moneyCompact(14000, "usd"), "$140");
  assert.equal(moneyCompact(4299, "usd"), "$42.99");
});

apiTest("flow: the countdown reads expiresAt and goes null at zero, never expiring on its own clock", () => {
  const now = Date.parse("2026-08-28T12:00:00Z");
  assert.equal(countdown("2026-08-28T12:58:12Z", now), "58:12");
  assert.equal(countdown("2026-08-28T13:30:05Z", now), "1:30:05");
  assert.equal(countdown("2026-08-28T12:00:00Z", now), null);
  assert.equal(countdown("2026-08-28T11:59:00Z", now), null);
  assert.equal(countdown(undefined, now), null);
  assert.equal(countdown("not a date", now), null);
});

apiTest("flow: startedAgo phrases the payment screen's resumed line", () => {
  const now = Date.parse("2026-08-28T12:00:00Z");
  assert.equal(startedAgo("2026-08-28T11:46:00Z", now), "Started 14 minutes ago.");
  assert.equal(startedAgo("2026-08-28T11:59:30Z", now), "Started less than a minute ago.");
  assert.equal(startedAgo("2026-08-28T11:59:00Z", now), "Started 1 minute ago.");
  assert.equal(startedAgo("2026-08-28T10:00:00Z", now), "Started 2 hours ago.");
  assert.equal(startedAgo("nonsense", now), null);
});

apiTest("flow: realSleep resolves after its delay and rejects at once on abort", async () => {
  const { realSleep } = await import("../src/api.js");
  const began = Date.now();
  await realSleep(5);
  assert.ok(Date.now() - began >= 4, "the delay is real");

  const ctl = new AbortController();
  const pending = realSleep(60_000, ctl.signal);
  ctl.abort();
  await assert.rejects(() => pending, (e: unknown) => e instanceof AbortedError);

  const already = new AbortController();
  already.abort();
  await assert.rejects(() => realSleep(60_000, already.signal), (e: unknown) => e instanceof AbortedError);
});


// ------------------------------------------- the code, stripped by value

apiTest("flow: every view but the code screen carries an order with NO code key at all", () => {
  const held = "SXB-AAAAA-BBBBB-CCCCC-DDDDD";
  const cases: Array<[string, PaymentView]> = [
    ["awaitingPayment", viewFor(record({ code: held }), openXmr, "xmr", { savedLocally: true, resumed: false })],
    ["cardForm", viewFor(record({ code: held }), openCard, "card", { savedLocally: true, resumed: false })],
    ["awaitingConfirmation", viewFor(record({ code: held, submitted: true }), openCard, "card", { savedLocally: true, resumed: false })],
    ["windowClosed", viewFor(record({ status: "expired", code: held }), expiredXmr, "xmr", { savedLocally: true, resumed: false })],
    ["paidNoCode", viewFor(record({ status: "paid" }), paidXmr, "xmr", { savedLocally: false, resumed: false })],
    ["detailsUnavailable", viewFor(record({ code: held }), { status: "open" }, undefined, { savedLocally: true, resumed: false })],
  ];
  for (const [name, v] of cases) {
    const order = (v as { order?: Record<string, unknown> }).order;
    assert.ok(order, `${name} has no order`);
    // Not `=== undefined`: the KEY must be gone, so it cannot be read back
    // through a cast, spread into an attribute, or serialised into a dataset.
    assert.equal(Object.hasOwn(order, "code"), false, `${name}'s order still has a code key`);
    assert.ok(!JSON.stringify(v).includes(held), `${name} serialises the code`);
  }
  // And the code screen, which is the one place it must survive.
  const codeIssued = viewFor(record({ status: "paid", code: held }), paidXmr, "xmr", { savedLocally: true, resumed: false });
  assert.equal(codeIssued.screen, "codeIssued");
  assert.equal(codeIssued.screen === "codeIssued" ? codeIssued.code : "", held);
});

apiTest("flow: withoutCode removes the key rather than blanking the value", () => {
  const stripped = withoutCode(record({ code: "SXB-Y" }));
  assert.equal(Object.hasOwn(stripped, "code"), false);
  assert.deepEqual(Object.keys(stripped).sort(), ["badgeType", "createdAt", "months", "orderId", "status"]);
});

apiTest("flow: historyRows hands a code to the paid row and to no other", () => {
  const rows = historyRows([
    record({ orderId: "a", status: "paid", code: "SXB-PAID0" }),
    record({ orderId: "b", status: "paid" }),
    record({ orderId: "c", status: "open", code: "SXB-OPEN0" }),
    record({ orderId: "d", status: "expired", code: "SXB-EXPD0" }),
  ]);
  assert.deepEqual(rows.map((r) => r.kind), ["paid", "paidNoCode", "open", "expired"]);
  assert.equal(rows[0]!.kind === "paid" ? rows[0]!.code : "", "SXB-PAID0");
  for (const row of rows) {
    assert.equal(Object.hasOwn(row.order, "code"), false, `${row.kind}'s order still has a code key`);
    if (row.kind !== "paid") assert.ok(!JSON.stringify(row).includes("SXB-"), `${row.kind} serialises a code`);
  }
});

// ------------------------------------------------- the give-up, armed and not

apiTest("flow: the confirming screen's give-up is DISARMED when the screen changes, so expired still waits", async () => {
  const h = harness();
  h.store.saveOrder(record({ status: "open", code: "SXB-Y", submitted: true }));
  const w = h.flow.watch("inv_9f3a", { initial: openCard, method: "card" });
  await settle();
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingConfirmation"], "the deadline is now armed");

  // Ten minutes in, the invoice expires. The watch loop keeps waiting on `expired`, and
  // the give-up rule gives up on the confirming screen only, so the deadline must be dropped, not carried.
  await h.clock.advance(10 * 60_000);
  h.net.answerHeld({ status: 200, body: { ...expiredXmr, clientSecret: "cs_test_abc" } });
  await settle();
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingConfirmation", "windowClosed"]);

  // Well past the original fifteen minutes, and past a fresh fifteen too.
  await h.clock.advance(20 * 60_000);
  const outcome = await Promise.race([w.done.then(() => "ended" as const), flush().then(() => "waiting" as const)]);
  assert.equal(outcome, "waiting", "a card order that expired must keep waiting, not give up");
  assert.equal(h.views.length, 2, "and must not overwrite the closed-window screen with the give-up screen");
  assert.equal(h.net.urls[h.net.urls.length - 1], "/api/invoice/inv_9f3a?wait=expired&seenPaid=0.734&seenFull=0");
  w.stop();
  await settle();
  assert.equal(h.net.pendingCount, 0, "stop must release the held connection");
  await w.done;
});

apiTest("flow: a settlement after the expiry still reaches the code screen, deadline or not", async () => {
  const h = harness();
  h.store.saveOrder(record({ status: "open", code: "SXB-AAAAA-BBBBB-CCCCC-DDDDD", submitted: true }));
  const w = h.flow.watch("inv_9f3a", { initial: openCard, method: "card" });
  await settle();
  h.net.answerHeld({ status: 200, body: { ...expiredXmr, clientSecret: "cs_test_abc" } });
  await settle();
  await h.clock.advance(30 * 60_000);
  h.net.answerHeld({ status: 200, body: { ...paidXmr, clientSecret: "cs_test_abc" } });
  await w.done;
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingConfirmation", "windowClosed", "codeIssued"]);
});

// -------------------------------------------------------- the code screen with no network

apiTest("flow: a paid record renders the code screen from the store alone, with no request", async () => {
  const h = harness();
  h.store.saveOrder(record({ status: "paid", code: "SXB-AAAAA-BBBBB-CCCCC-DDDDD" }));
  const w = h.flow.watch("inv_9f3a");
  // Asserted before awaiting `done`, so a regression that goes to the network
  // fails on the assertion rather than by hanging on a request nothing answers.
  await settle();
  assert.equal(h.net.calls.length, 0, "nothing is fetched: a reload needs no network");
  assert.deepEqual(h.views.map((v) => v.screen), ["codeIssued"]);
  assert.equal(h.views[0]!.screen === "codeIssued" ? h.views[0]!.code : "", "SXB-AAAAA-BBBBB-CCCCC-DDDDD");
  await w.done;
});

apiTest("flow: a paid record with no code renders the paid-no-code screen offline too, and asks nothing", async () => {
  const h = harness();
  h.store.saveOrder(record({ status: "paid" }));
  const w = h.flow.watch("inv_9f3a");
  await settle();
  assert.equal(h.net.calls.length, 0);
  assert.deepEqual(h.views.map((v) => v.screen), ["paidNoCode"]);
  await w.done;
});

apiTest("flow: an OPEN record holding no destination renders nothing without the network", async () => {
  const h = offlineHarness();
  h.store.saveOrder(record({ status: "open", code: "SXB-Y" }));
  const w = h.flow.watch("inv_9f3a");
  await settle();
  assert.deepEqual(h.views, [], "the payment screen cannot be drawn from this record: it holds no address");
  w.stop();
  await Promise.race([w.done, flush().then(() => { throw new Error("stop left the loop in its backoff"); })]);
});

// ------------------------------------------------------------- [ Check again ]

apiTest("flow: checkAgain returns null while a loop is live, so the screen is not blanked", async () => {
  const h = harness();
  const w = h.flow.watch("inv_9f3a", { initial: { status: "open" } });
  await settle();
  assert.deepEqual(h.views.map((v) => v.screen), ["detailsUnavailable"]);
  assert.equal(h.flow.checkAgain("inv_9f3a"), null, "there is nothing to check again — it never stopped");
  await settle();
  assert.equal(h.net.calls.length, 1, "and no second request is issued");
  w.stop();
  await settle();
  assert.equal(h.net.pendingCount, 0, "stop must release the held connection");
  await w.done;
});

apiTest("flow: checkAgain after a give-up restarts the confirming screen and RE-ARMS the fifteen minutes", async () => {
  const h = harness();
  h.store.saveOrder(record({ status: "open", code: "SXB-AAAAA-BBBBB-CCCCC-DDDDD", submitted: true }));
  const first = h.flow.watch("inv_9f3a", { initial: openCard, method: "card", resumed: true });
  await settle();
  await h.clock.advance(GIVE_UP_MS);
  await first.done;
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingConfirmation", "awaitingConfirmation"]);
  assert.equal(h.views[1]!.screen === "awaitingConfirmation" ? h.views[1]!.gaveUp : false, true);
  // This view is assembled in the watcher rather than by `viewFor`, so the
  // enumeration test above never walks it: assert its order is stripped too.
  const gaveUpOrder = (h.views[1]! as { order: Record<string, unknown> }).order;
  assert.equal(Object.hasOwn(gaveUpOrder, "code"), false, "the give-up view still carries a code key");
  assert.ok(!JSON.stringify(h.views[1]).includes("SXB-"), "the give-up view serialises a code");

  // The restart reads the flag off the order record, which is where a card
  // confirm writes it. A restart that re-read a page-global flag would
  // drop the buyer onto the card form the moment any other order cleared it.
  const again = h.flow.checkAgain("inv_9f3a");
  assert.ok(again, "a finished loop must restart");
  await settle();
  // Checking again means asking the server: a plain read first, then
  // the screen it answers with.
  assert.equal(h.net.pendingCount, 1);
  assert.equal(h.net.urls[h.net.urls.length - 1], "/api/invoice/inv_9f3a", "plain, not a hold");
  h.net.answerHeld({ status: 200, body: openCard });
  await settle();
  const restarted = h.views[2]!;
  assert.equal(restarted.screen, "awaitingConfirmation", "it comes back as the confirming screen, not the card form");
  assert.equal(restarted.screen === "awaitingConfirmation" ? restarted.gaveUp : true, false,
    "waiting again from the top, not still given up");

  // And the clock re-arms: fifteen more minutes, then it gives up again.
  await h.clock.advance(GIVE_UP_MS - 1000);
  assert.equal(h.views.length, 3, "one second short, still waiting");
  await h.clock.advance(1000);
  await again.done;
  assert.equal(h.views.length, 4);
  assert.equal(h.views[3]!.screen === "awaitingConfirmation" ? h.views[3]!.gaveUp : false, true);
});


// ------------------------------------------- the wizard's answers, recovered

const PRICES = [
  { priceId: "price_supporter", badgeType: "supporter" },
  { priceId: "price_legend", badgeType: "legend" },
];
const OFFERS = [
  { offerId: "offer_3m", priceId: "price_legend", months: 3 },
  { offerId: "offer_12m", priceId: "price_legend", months: 12 },
  { offerId: "offer_12m_s", priceId: "price_supporter", months: 12 },
];

apiTest("flow: selectionFromOrder recovers the answers the store rules cleared from the session", () => {
  assert.deepEqual(selectionFromOrder({ badgeType: "legend", months: 12 }, PRICES, OFFERS),
    { priceId: "price_legend", offerId: "offer_12m" });
  assert.deepEqual(selectionFromOrder({ badgeType: "supporter", months: 12 }, PRICES, OFFERS),
    { priceId: "price_supporter", offerId: "offer_12m_s" }, "the offer must belong to the price");
  // One month is the unoffered term, which the session spells with its own key,
  // never the empty string, which the session already uses for "nothing
  // chosen". A recovered one-month order must come back AS one month.
  assert.deepEqual(selectionFromOrder({ badgeType: "legend", months: 1 }, PRICES, OFFERS),
    { priceId: "price_legend", offerId: SINGLE_MONTH });
  assert.notEqual(SINGLE_MONTH, "", "the sentinel must be distinguishable from an unanswered session");
  assert.equal(OFFERS.some((o) => o.offerId === SINGLE_MONTH), false,
    "and must name no offer: the create endpoint sends no offerId at all for one month");
});

apiTest("flow: selectionFromOrder gives up rather than guessing", () => {
  assert.equal(selectionFromOrder(undefined, PRICES, OFFERS), undefined);
  assert.equal(selectionFromOrder({ badgeType: "founder", months: 12 }, PRICES, OFFERS), undefined,
    "a badge type this build does not sell");
  assert.equal(selectionFromOrder({ badgeType: "legend", months: 6 }, PRICES, OFFERS), undefined,
    "a term this build has no offer for — a buyer one deploy behind");
  assert.equal(selectionFromOrder({ badgeType: "supporter", months: 3 }, PRICES, OFFERS), undefined,
    "an offer belonging to another price is not a match");
});

// ------------------------------------------------------- the offline resume
//
// Resuming an open invoice's stored address, amount and QR is meant to work offline, and

apiTest("flow: checkout stores the destination the payment screen redraws from with no network", async () => {
  const h = harness();
  h.net.script({ status: 200, body: createdXmr });
  await h.flow.checkout({ priceId: "price_legend", offerId: "offer_12m", method: "xmr" });
  const stored = h.store.order("inv_9f3a")!;
  assert.equal(stored.address, "48HqK2XmVexampleAddress9fRtWc");
  assert.equal(stored.cryptoAmount, "1.482");
  assert.equal(stored.cryptoCurrency, "xmr");
  assert.equal(stored.expiresAt, "2026-08-28T13:00:00Z", "or the payment screen offline shows an address with no window");
  assert.equal(stored.amount, 42000);
  assert.equal(stored.currency, "usd", "the fiat line needs both, or it cannot be rendered at all");
});

apiTest("flow: a card checkout stores no destination and no client secret", async () => {
  const storage = new Mem();
  const h = harness(storage);
  h.net.script({ status: 200, body: { ...createdXmr, address: undefined, cryptoAmount: undefined, cryptoCurrency: undefined, clientSecret: "cs_test_abc" } });
  await h.flow.checkout({ priceId: "price_supporter", method: "card" });
  const stored = h.store.order("inv_9f3a")!;
  assert.equal(stored.address, undefined);
  assert.equal(stored.cryptoAmount, undefined);
  assert.equal(stored.cryptoCurrency, undefined);
  assert.equal(stored.expiresAt, undefined, "with nothing to send to there is no window worth holding");
  // the store rules as amended: the price and the method are not part of the destination
  // and are kept whatever the method was: the history list's row prints both.
  assert.equal(stored.amount, 42000);
  assert.equal(stored.currency, "usd");
  assert.equal(stored.method, "card");
  // The raw value, not the typed read: a secret smuggled in as an excess
  // property would be invisible to `store.order` and still be written to disk.
  const raw = storage.m.get("sxb.orders.v1") ?? "";
  assert.ok(!raw.includes("cs_test_abc") && !raw.includes("clientSecret"),
    "the offline promise puts the card form under \"needs the network\": no payment secret is written to rest");
});

const HELD = {
  address: "48HqK2Xm", cryptoAmount: "1.482", cryptoCurrency: "xmr" as const,
  expiresAt: "2026-08-28T13:00:00Z", amount: 42000, currency: "usd",
};

apiTest("flow: an open response without the details keeps what is already stored", () => {
  const store = new Store(new Mem());
  store.saveOrder(record(HELD));
  // the read endpoint sends the payment details only when the browser might not have them.
  const { record: after } = applyView(store, "inv_9f3a", { status: "open" }, Date.now());
  assert.equal(after.address, "48HqK2Xm");
  assert.equal(after.expiresAt, "2026-08-28T13:00:00Z");
  assert.equal(store.order("inv_9f3a")!.cryptoAmount, "1.482");
  assert.equal(store.order("inv_9f3a")!.amount, 42000);
});

apiTest("flow: a response carrying the destination but no window keeps the window held", () => {
  const store = new Store(new Mem());
  store.saveOrder(record(HELD));
  const { record: after } = applyView(store, "inv_9f3a",
    { status: "open", address: "48HqK2Xm", cryptoAmount: "1.482", cryptoCurrency: "xmr" }, Date.now());
  assert.equal(after.expiresAt, "2026-08-28T13:00:00Z", "each group is replaced on its own, or not at all");
  assert.equal(after.amount, 42000);
});

apiTest("flow: an amount with no currency is not stored — the fiat line needs both", () => {
  const store = new Store(new Mem());
  const { record: after } = applyView(store, "inv_new",
    { status: "open", address: "48HqK2Xm", cryptoAmount: "1.482", cryptoCurrency: "xmr", amount: 42000 },
    Date.now());
  assert.equal(after.amount, undefined);
  assert.equal(after.currency, undefined);
  assert.equal(after.address, "48HqK2Xm", "and the destination is still kept");
});

apiTest("flow: paid and expired CLEAR the stored destination, in the store and not only in hand", () => {
  for (const status of ["paid", "expired"] as const) {
    const storage = new Mem();
    const store = new Store(storage);
    store.saveOrder(record({ ...HELD, method: "xmr" }));
    const { record: after } = applyView(store, "inv_9f3a", { ...paidXmr, status }, Date.now());
    assert.equal(after.address, undefined, `${status} has no destination left to send to`);
    const stored = store.order("inv_9f3a")!;
    assert.equal(stored.address, undefined, `${status}: cleared in the store, not only in the record returned`);
    for (const key of ["cryptoAmount", "cryptoCurrency", "expiresAt"] as const) {
      assert.equal(stored[key], undefined, `${status} must clear ${key}`);
    }
    assert.ok(!(storage.m.get("sxb.orders.v1") ?? "").includes("48HqK2Xm"));
    assert.ok(!(storage.m.get("sxb.orders.v1") ?? "").includes("2026-08-28T13:00:00Z"));
  }
});

apiTest("flow: paid and expired KEEP what was paid and how, which is the history list's row", () => {
  // The rule the destination follows is "nothing may still be sent there", and a price and a method are not
  // somewhere to send. The history is their reader: a settled row naming neither would be a purchase with
  // the purchase left out, so "every field has a reader" cuts the other way here.
  for (const status of ["paid", "expired"] as const) {
    const store = new Store(new Mem());
    store.saveOrder(record({ ...HELD, method: "xmr" }));
    applyView(store, "inv_9f3a", { status }, Date.now());
    const stored = store.order("inv_9f3a")!;
    assert.equal(stored.amount, 42000, `${status} must keep the price`);
    assert.equal(stored.currency, "usd", `${status} must keep the currency`);
    assert.equal(stored.method, "xmr", `${status} must keep the method`);
  }
});

apiTest("flow: offlineInvoice draws the payment screen from an open record only", () => {
  assert.deepEqual(offlineInvoice(record(HELD)), { status: "open", ...HELD });
  assert.equal(offlineInvoice(record({ ...HELD, status: "paid" })), undefined, "a paid order is the code screen, from the code");
  assert.equal(offlineInvoice(record({ ...HELD, status: "expired" })), undefined);
  assert.equal(offlineInvoice(record({ address: "48HqK2Xm" })), undefined,
    "half a destination is not one: the amount is what the payment screen's heading and QR need");
});

apiTest("flow: an open crypto order resumes from the record alone when the read fails", async () => {
  const h = offlineHarness();
  h.store.saveOrder(record({ code: "SXB-Y", ...HELD }));
  const w = h.flow.watch("inv_9f3a", { resumed: true });
  await settle();
  assert.deepEqual(h.views.map((v) => v.screen), ["awaitingPayment"], "the offline promise: the address, the amount and the QR come from the store");
  const first = h.views[0]!;
  assert.equal(first.screen === "awaitingPayment" ? first.invoice.address : "", "48HqK2Xm");
  assert.equal(first.screen === "awaitingPayment" ? first.invoice.cryptoAmount : "", "1.482");
  assert.equal(first.screen === "awaitingPayment" ? first.method : "", "xmr");
  assert.equal(first.screen === "awaitingPayment" ? first.invoice.expiresAt : "gone", "2026-08-28T13:00:00Z",
    "the rate window is drawn too: an address with no window is what support reconciliation has to reconcile");
  assert.equal(first.screen === "awaitingPayment" ? first.invoice.amount : 0, 42000);
  // the store rules: the code is in the store from before the invoice existed, and the payment screen is
  // an unpaid screen: the view it is handed must not carry it at all.
  assert.ok(!JSON.stringify(first).includes("SXB-Y"), "an unpaid screen never receives a code");
  w.stop();
  await Promise.race([w.done, flush().then(() => { throw new Error("stop left the loop in its backoff"); })]);
});

apiTest("flow: the offline resume keeps asking — the status is never drawn from the store", async () => {
  const failing = (async (): Promise<Response> => { throw new TypeError("offline"); }) as unknown as typeof fetch;
  let calls = 0;
  const counting = (async (...args: unknown[]): Promise<Response> => {
    calls++;
    return (failing as unknown as (...a: unknown[]) => Promise<Response>)(...args);
  }) as unknown as typeof fetch;
  const store = new Store(new Mem());
  store.saveOrder(record(HELD));
  const clock = new Clock();
  const views: PaymentView[] = [];
  const flow = new Flow({
    store, fetch: counting, sleep: clock.sleep, now: clock.now,
    newCode: () => "C", hashCode: async (c) => c, render: (v) => { views.push(v); },
  });
  const w = flow.watch("inv_9f3a", { resumed: true });
  await settle();
  const afterFirst = calls;
  assert.ok(afterFirst >= 1, "the read is attempted: only the provider knows if money arrived");
  await clock.advance(1000); // the first backoff step
  assert.ok(calls > afterFirst, "the loop is still asking; the payment screen on screen is not an answer about money");
  assert.deepEqual(views.map((v) => v.screen), ["awaitingPayment"], "and the screen does not flicker while it asks");
  w.stop();
  await w.done;
});

apiTest("flow: a stopped watch never paints, however late its answer arrives", async () => {
  // The page moves from one order to another by stopping every watch and starting a new one.
  // The old loop's request is already in flight, and painting its answer draws the old
  // order's address and QR under the new order's URL.
  const h = harness();
  const watch = h.flow.watch("inv_old");
  await settle();
  h.flow.stopAll();
  h.net.answerHeld({ status: 200, body: openXmr });
  await settle();
  await watch.done;
  assert.deepEqual(h.views, [], "a stopped loop has no screen to draw on");
});

apiTest("flow: the first read of a watch is abortable, like every read after it", async () => {
  const h = harness();
  h.flow.watch("inv_9f3a");
  await settle();
  const first = h.net.calls[0]!;
  assert.equal(first.url, "/api/invoice/inv_9f3a", "the plain read, before any wait");
  assert.ok(first.init?.signal !== undefined, "an unabortable read outlives the screen that wanted it");
});

apiTest("flow: a suspended tab does not spend its give-up budget while it is hidden", async () => {
  const h = harness();
  h.store.saveOrder(record({ status: "open", submitted: true }));
  const w = h.flow.watch("inv_9f3a", { initial: openCard, method: "card" });
  await settle();
  assert.equal(h.views.at(-1)?.screen, "awaitingConfirmation", "the confirm has been sent; this is the waiting screen");

  w.suspend();
  await settle();
  await h.clock.advance(GIVE_UP_MS * 2);
  w.resume();
  await settle();
  const back = h.views.at(-1)!;
  assert.equal(back.screen === "awaitingConfirmation" && back.gaveUp, false,
    "coming back to a hidden tab must not give up before anything has been asked");

  // and the deadline resumes where it left off: clearing it left the give-up unreachable, and
  // refilling it let a tab toggled every few minutes wait for ever
  await h.clock.advance(GIVE_UP_MS + 1000);
  await settle();
  const later = h.views.at(-1)!;
  assert.equal(later.screen === "awaitingConfirmation" && later.gaveUp, true,
    "the give-up must still fire once the budget has been spent asking");
});

apiTest("flow: a funded verdict with no figures is still confirming, not waiting for payment", () => {
  // Monero reports an invoice as confirming while `paymentMethodPaid` is still zero. Reading
  // the amount before the verdict left a paid invoice saying "waiting for payment".
  assert.equal(orderPhase({ status: "open", paidInFull: true }), "processing");
  assert.equal(orderPhase({ status: "open", paidInFull: true, cryptoAmountPaid: "0.0134" }), "processing");
  assert.equal(orderPhase({ status: "open", cryptoAmountPaid: "0.007", paidInFull: false }), "partPaid");
  assert.equal(orderPhase({ status: "open" }), "awaiting");

  const record = { orderId: "inv_9f3a", badgeType: "supporter", months: 1,
    createdAt: "2026-08-28T11:00:00Z", status: "open" as const, paidInFull: true };
  assert.equal(historyRows([record])[0]!.kind, "processing", "and the history row agrees");
});

apiTest("flow: hiding a tab neither spends the give-up budget nor refills it", async () => {
  const h = harness();
  h.store.saveOrder(record({ status: "open", submitted: true }));
  const w = h.flow.watch("inv_9f3a", { initial: openCard, method: "card" });
  await settle();

  // most of the budget spent asking, then a hide and a return
  await h.clock.advance(GIVE_UP_MS - 60_000);
  w.suspend();
  await settle();
  await h.clock.advance(GIVE_UP_MS * 4);
  w.resume();
  await settle();
  const back = h.views.at(-1)!;
  assert.equal(back.screen === "awaitingConfirmation" && back.gaveUp, false, "hidden time is not spent");

  // only the minute that was left remains, not a fresh quarter of an hour
  await h.clock.advance(61_000);
  await settle();
  const after = h.views.at(-1)!;
  assert.equal(after.screen === "awaitingConfirmation" && after.gaveUp, true,
    "a tab hidden often enough must still reach the give-up screen");
});
