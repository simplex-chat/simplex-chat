import {
  composeOrder, readChain, readMethod, readStatus, readStep, readTheme,
  type OrderRecord, type SessionRecord, type Theme,
} from "./domain.js";
import { asObject, filledText, flag, positiveInteger, text, nonNegativeInteger } from "./parse.js";

export interface StorageLike {
  getItem(key: string): string | null;
  setItem(key: string, value: string): void;
  removeItem(key: string): void;
}

// Date.parse returns NaN for a value it cannot read, and NaN in a comparator makes every
// comparison false, so the list ends up in whatever order the sort happened to produce.
// An unreadable timestamp sorts oldest instead.
function createdMs(o: OrderRecord): number {
  const t = Date.parse(o.createdAt);
  return Number.isNaN(t) ? Number.MIN_SAFE_INTEGER : t;
}

const SESSION_KEY = "sxb.session.v1";
const ORDERS_KEY = "sxb.orders.v1";
// A key of its own, because the theme belongs to the device: it survives a checkout,
// which clears the session, and [ Forget everything on this device ], which is about codes.
const THEME_KEY = "sxb.theme.v1";
const CAP = 50;

/** A field that does not read is left out and the record kept, unlike a response, because the service
 * can be asked again and storage holds the one copy of a code. Only the identity and the status are
 * required, since a row with no order to point at has nothing to show. */
function readOrder(value: unknown): OrderRecord | undefined {
  const o = asObject(value);
  if (o === undefined) return undefined;
  const orderId = filledText(o.orderId);
  const createdAt = filledText(o.createdAt);
  const status = readStatus(o.status);
  if (orderId === undefined || createdAt === undefined || status === undefined) return undefined;
  return composeOrder(
    {
      orderId,
      createdAt,
      badgeType: text(o.badgeType) ?? "",
      months: positiveInteger(o.months) ?? 0,
    },
    { code: text(o.code), submitted: flag(o.submitted), method: readMethod(o.method) },
    {
      status,
      amount: positiveInteger(o.amount),
      currency: text(o.currency),
      amountPaid: nonNegativeInteger(o.amountPaid),
      cryptoAmountPaid: text(o.cryptoAmountPaid),
      cryptoAmountDue: text(o.cryptoAmountDue),
      paidInFull: flag(o.paidInFull),
      address: text(o.address),
      cryptoAmount: text(o.cryptoAmount),
      cryptoCurrency: readChain(o.cryptoCurrency),
      expiresAt: text(o.expiresAt),
    },
  );
}

function readSession(value: unknown): SessionRecord | undefined {
  const s = asObject(value);
  const step = s === undefined ? undefined : readStep(s.step);
  if (s === undefined || step === undefined) return undefined;
  const session: SessionRecord = { step };
  const priceId = text(s.priceId);
  const offerId = text(s.offerId);
  const method = readMethod(s.method);
  if (priceId !== undefined) session.priceId = priceId;
  if (offerId !== undefined) session.offerId = offerId;
  if (method !== undefined) session.method = method;
  return session;
}

export class Store {
  private wipes = 0;

  /** `durable` is false for a store that accepts writes and loses them on the next load, which is
   * what this page falls back to where the browser refuses `localStorage`. Every screen that
   * promises the buyer their codes are kept has to read it. */
  constructor(private readonly storage: StorageLike, readonly durable = true) {}

  /** Whether a code bought now could be kept at all: the store has to survive a reload, and the
   * orders list has to have room that is not already holding someone's code. Read before the
   * money, since afterwards the answer is only bad news. */
  canHoldACode(): boolean {
    if (!this.durable) return false;
    const list = this.orders();
    return list.length < CAP || list.some((o) => o.code === undefined);
  }

  /** Whether this browser is really holding that code. A round trip through the store proves
   * nothing on its own: the in-memory fallback answers with whatever it was just handed. */
  holdsCode(orderId: string, code: string | undefined): boolean {
    return this.durable && code !== undefined && this.order(orderId)?.code === code;
  }

  // Missing, unreadable and not JSON all answer undefined: nothing is stored under this key
  // that this build can use, and every caller has a value it falls back on.
  private read(key: string): unknown {
    try {
      const raw = this.storage.getItem(key);
      return raw === null ? undefined : JSON.parse(raw);
    } catch {
      return undefined;
    }
  }

  private write(key: string, value: unknown): boolean {
    try {
      this.storage.setItem(key, JSON.stringify(value));
      return true;
    } catch {
      return false;
    }
  }

  session(): SessionRecord {
    return readSession(this.read(SESSION_KEY)) ?? { step: "tier" };
  }

  saveSession(patch: Partial<SessionRecord>): boolean {
    return this.write(SESSION_KEY, { ...this.session(), ...patch });
  }

  clearSession(): void {
    this.forget(SESSION_KEY);
  }

  orders(): OrderRecord[] {
    const list = this.read(ORDERS_KEY);
    if (!Array.isArray(list)) return [];
    return list.flatMap((o) => readOrder(o) ?? []);
  }

  // Replaces the stored entry rather than merging, which made an omitted key mean "keep what was there", so
  // a caller that stopped writing a field kept reporting a payment the service no longer sends. Exceptions
  // are what only this browser holds: a code, lost for good if dropped, and the card confirmation, never unset.
  saveOrder(record: OrderRecord): boolean {
    const list = this.orders();
    const at = list.findIndex((o) => o.orderId === record.orderId);
    if (at >= 0) {
      const kept = list[at]!;
      list[at] = {
        ...record,
        ...(record.code === undefined && kept.code !== undefined ? { code: kept.code } : {}),
        ...(kept.submitted === true ? { submitted: true } : {}),
      };
    } else {
      if (list.length >= CAP) {
        let victim = -1;
        for (let i = list.length - 1; i >= 0; i--) if (!list[i]!.code) { victim = i; break; }
        if (victim < 0) return false;
        list.splice(victim, 1);
      }
      list.unshift(record);
    }
    list.sort((a, b) => createdMs(b) - createdMs(a));
    return this.write(ORDERS_KEY, list);
  }

  markSubmitted(orderId: string): boolean {
    const record = this.order(orderId);
    if (record === undefined) return false;
    return this.saveOrder({ ...record, submitted: true });
  }

  order(orderId: string): OrderRecord | undefined {
    return this.orders().find((o) => o.orderId === orderId);
  }

  newestOpen(): OrderRecord | undefined {
    return this.orders().find((o) => o.status === "open");
  }

  theme(): Theme {
    return readTheme(this.read(THEME_KEY)) ?? "system";
  }

  saveTheme(theme: Theme): boolean {
    return this.write(THEME_KEY, theme);
  }

  /** One `try` for both would let a throw on the first key leave the second one written. */
  forgetEverything(): void {
    this.wipes += 1;
    this.forget(ORDERS_KEY);
    this.forget(SESSION_KEY);
  }

  /** Bumped by `forgetEverything`. A write awaited across it belongs to a store the buyer emptied,
   * and "this cannot be undone" has to mean it. */
  get wipeCount(): number {
    return this.wipes;
  }

  private forget(key: string): void {
    try { this.storage.removeItem(key); } catch { /* a store that refuses this keeps the key */ }
  }
}
