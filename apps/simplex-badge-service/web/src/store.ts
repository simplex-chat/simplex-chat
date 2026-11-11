import {
  composeOrder, readApp, readChain, readMethod, readStatus, readStep, readTheme,
  type OrderRecord, type SessionRecord, type Theme,
} from "./domain.js";
import { asObject, filledText, flag, positiveInteger, text, nonNegativeInteger } from "./parse.js";

export interface StorageLike {
  getItem(key: string): string | null;
  setItem(key: string, value: string): void;
  removeItem(key: string): void;
}

// Date.parse returns NaN for an unreadable value, and NaN breaks the sort comparator, so an unreadable timestamp sorts oldest.
function createdMs(o: OrderRecord): number {
  const t = Date.parse(o.createdAt);
  return Number.isNaN(t) ? Number.MIN_SAFE_INTEGER : t;
}

const SESSION_KEY = "sb.session.v1";
const ORDERS_KEY = "sb.orders.v1";
const THEME_KEY = "sb.theme.v1";
const CARD_RETURN_KEY = "sb.cardReturn.v1";
const CAP = 50;

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
    { code: text(o.code), submitted: flag(o.submitted), canceled: flag(o.canceled), method: readMethod(o.method), app: readApp(o.app) },
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
  const app = readApp(s.app);
  if (priceId !== undefined) session.priceId = priceId;
  if (offerId !== undefined) session.offerId = offerId;
  if (method !== undefined) session.method = method;
  if (app !== undefined) session.app = app;
  return session;
}

export class Store {
  private wipes = 0;

  /** `durable` is false for the in-memory fallback used when the browser refuses `localStorage`. */
  constructor(private readonly storage: StorageLike, readonly durable = true) {}

  canHoldACode(): boolean {
    if (!this.durable) return false;
    const list = this.orders();
    return list.length < CAP || list.some((o) => o.code === undefined);
  }

  holdsCode(orderId: string, code: string | undefined): boolean {
    return this.durable && code !== undefined && this.order(orderId)?.code === code;
  }

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

  // The record replaces the stored entry, but the browser-only fields (code, submitted, canceled, app) are preserved because the service never sends them back.
  saveOrder(record: OrderRecord): boolean {
    const list = this.orders();
    const at = list.findIndex((o) => o.orderId === record.orderId);
    if (at >= 0) {
      const kept = list[at]!;
      list[at] = {
        ...record,
        ...(record.code === undefined && kept.code !== undefined ? { code: kept.code } : {}),
        ...(kept.submitted === true ? { submitted: true } : {}),
        ...(kept.canceled === true ? { canceled: true } : {}),
        ...(record.app === undefined && kept.app !== undefined ? { app: kept.app } : {}),
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

  markCanceled(orderId: string): boolean {
    const record = this.order(orderId);
    if (record === undefined) return false;
    return this.saveOrder({ ...record, canceled: true });
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

  rememberCardReturn(orderId: string, atMs: number): boolean {
    return this.write(CARD_RETURN_KEY, { orderId, at: atMs });
  }

  takeCardReturn(withinMs: number, nowMs: number): string | undefined {
    const held = this.read(CARD_RETURN_KEY);
    this.forget(CARD_RETURN_KEY);
    if (typeof held !== "object" || held === null) return undefined;
    const { orderId, at } = held as { orderId?: unknown; at?: unknown };
    if (typeof orderId !== "string" || orderId === "" || typeof at !== "number") return undefined;
    return nowMs - at <= withinMs ? orderId : undefined;
  }

  clearCardReturn(): void {
    this.forget(CARD_RETURN_KEY);
  }

  forgetEverything(): void {
    this.wipes += 1;
    this.forget(ORDERS_KEY);
    this.forget(SESSION_KEY);
  }

  get wipeCount(): number {
    return this.wipes;
  }

  private forget(key: string): void {
    try { this.storage.removeItem(key); } catch { /* a store that refuses this keeps the key */ }
  }
}
