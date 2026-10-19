import { SINGLE_MONTH } from "./catalog.js";
import { composeOrder, definedOnly } from "./domain.js";
import { screenForOpenOrder } from "./routing.js";
import type { InvoiceView } from "./api.js";
import type { Chain, Method, OrderLocalState, OrderRecord, OrderServerState, OrderStatus } from "./domain.js";
import type { Store } from "./store.js";

export type UnpaidOrder = Omit<OrderRecord, "code">;

// This strips the code by value, since an extra property survives a widening cast.
export function withoutCode(record: OrderRecord): UnpaidOrder {
  const { code: _code, ...rest } = record;
  return rest;
}

export function withoutDestination(record: OrderRecord): OrderRecord {
  return definedOnly({
    ...record,
    address: undefined, cryptoAmount: undefined, cryptoCurrency: undefined, expiresAt: undefined,
  });
}

export type PaymentView =
  | { screen: "unknownOrder"; orderId: string }
  | { screen: "codeIssued"; order: OrderRecord; invoice: InvoiceView | undefined; code: string; savedLocally: boolean }
  | { screen: "paidNoCode"; order: UnpaidOrder; invoice: InvoiceView | undefined }
  | { screen: "awaitingPayment"; order: UnpaidOrder; invoice: InvoiceView; method: Chain; resumed: boolean }
  | { screen: "awaitingConfirmation"; order: UnpaidOrder; invoice: InvoiceView | undefined; gaveUp: boolean; method?: Method }
  | { screen: "windowClosed"; order: UnpaidOrder; invoice: InvoiceView | undefined }
  | { screen: "cardForm"; order: UnpaidOrder; invoice: InvoiceView; clientSecret: string; resumed: boolean }
  | { screen: "detailsUnavailable"; order: UnpaidOrder };

export interface PaymentProgress {
  status: OrderStatus;
  cryptoAmountPaid?: string | undefined;
  paidInFull?: boolean | undefined;
}

export type HistoryRow =
  | { kind: "paid"; order: UnpaidOrder; code: string }
  | { kind: "paidNoCode"; order: UnpaidOrder }
  | { kind: "open"; order: UnpaidOrder }
  | { kind: "partPaid"; order: UnpaidOrder }
  | { kind: "processing"; order: UnpaidOrder }
  | { kind: "canceled"; order: UnpaidOrder }
  | { kind: "expired"; order: UnpaidOrder };

export function historyRows(entries: readonly OrderRecord[]): HistoryRow[] {
  return entries.map((e) => {
    const order = withoutCode(e);
    // A canceled order wins over the expired or open status the server left behind, but a paid order
    // keeps its code even where a cancel lost to settlement.
    switch (orderPhase(e)) {
      case "paid":
        return e.code !== undefined ? { kind: "paid" as const, order, code: e.code } : { kind: "paidNoCode" as const, order };
      case "expired":
        return e.canceled === true ? { kind: "canceled" as const, order } : { kind: "expired" as const, order };
      case "processing":
        return { kind: "processing" as const, order };
      case "partPaid":
        return { kind: "partPaid" as const, order };
      case "awaiting":
        return e.canceled === true ? { kind: "canceled" as const, order } : { kind: "open" as const, order };
    }
  });
}

export function recordFromView(orderId: string, view: InvoiceView, nowMs: number): OrderRecord {
  return {
    orderId,
    badgeType: view.badgeType ?? "",
    months: view.months ?? 0,
    createdAt: new Date(nowMs).toISOString(),
    status: view.status,
  };
}

interface Destination {
  address: string;
  cryptoAmount: string;
  cryptoCurrency: Chain;
  expiresAt: string | undefined;
}

interface Price {
  amount: number;
  currency: string;
}

function destinationOf(from: Partial<OrderServerState> | undefined): Destination | undefined {
  const { address, cryptoAmount, cryptoCurrency, expiresAt } = from ?? {};
  if (address === undefined || cryptoAmount === undefined || cryptoCurrency === undefined) return undefined;
  return { address, cryptoAmount, cryptoCurrency, expiresAt };
}

type Paid = Pick<OrderServerState, "amountPaid" | "cryptoAmountPaid" | "cryptoAmountDue" | "paidInFull">;

function paidOf(from: Partial<OrderServerState> | undefined): Paid {
  return {
    amountPaid: from?.amountPaid,
    cryptoAmountPaid: from?.cryptoAmountPaid,
    cryptoAmountDue: from?.cryptoAmountDue,
    paidInFull: from?.paidInFull,
  };
}

function priceOf(from: Partial<OrderServerState> | undefined): Price | undefined {
  const { amount, currency } = from ?? {};
  return amount !== undefined && currency !== undefined ? { amount, currency } : undefined;
}

function stateOf(status: OrderStatus, price: Price | undefined, paid: Paid, to: Destination | undefined): OrderServerState {
  return {
    status,
    amount: price?.amount,
    currency: price?.currency,
    ...paid,
    address: to?.address,
    cryptoAmount: to?.cryptoAmount,
    cryptoCurrency: to?.cryptoCurrency,
    expiresAt: to?.expiresAt,
  };
}

// Paid never falls back to the stored value, since the service withdrawing it means it is no longer true.
export function serverState(view: InvoiceView, held: OrderRecord | undefined): OrderServerState {
  const fresh = destinationOf(view);
  const stored = destinationOf(held);
  const to = view.status !== "open" ? undefined
    : fresh === undefined ? stored
    : { ...fresh, expiresAt: fresh.expiresAt ?? stored?.expiresAt };
  return stateOf(view.status, priceOf(view) ?? priceOf(held), paidOf(view), to);
}

function localState(base: OrderRecord, memory: OrderRecord | undefined, method: Method | undefined): OrderLocalState {
  return {
    code: memory?.code ?? base.code,
    submitted: memory?.submitted === true || base.submitted === true ? true : undefined,
    canceled: memory?.canceled === true || base.canceled === true ? true : undefined,
    method: method ?? base.method,
    app: memory?.app ?? base.app,
  };
}

export function offlineInvoice(record: OrderRecord): InvoiceView | undefined {
  if (record.status !== "open") return undefined;
  const to = destinationOf(record);
  if (to === undefined) return undefined;
  return definedOnly(stateOf("open", priceOf(record), paidOf(record), to));
}

export function closedInvoice(record: OrderRecord): InvoiceView | undefined {
  if (record.status === "open") return undefined;
  return definedOnly(stateOf(record.status, priceOf(record), paidOf(record), undefined));
}

export function applyView(
  store: Store,
  orderId: string,
  view: InvoiceView,
  nowMs: number,
  memory?: OrderRecord,
  method?: Method,
): { record: OrderRecord; savedLocally: boolean } {
  const base = store.order(orderId) ?? memory ?? recordFromView(orderId, view, nowMs);
  const record = composeOrder(base, localState(base, memory, method), serverState(view, base));
  store.saveOrder(record);
  const savedLocally = store.holdsCode(orderId, record.code);
  return { record, savedLocally };
}

export function viewFor(
  record: OrderRecord,
  invoice: InvoiceView | undefined,
  method: Method | undefined,
  opts: { savedLocally: boolean; resumed: boolean },
): PaymentView {
  const order = withoutCode(record);
  const resumed = opts.resumed;
  const unavailable = { screen: "detailsUnavailable", order } as const;
  // The payment is read from the invoice when present, since a held response knows about a payment
  // before the record is written from it.
  const seen = invoice ?? record;
  switch (orderPhase({ status: record.status, cryptoAmountPaid: seen.cryptoAmountPaid, paidInFull: seen.paidInFull })) {
    case "paid":
      return record.code !== undefined
        ? { screen: "codeIssued", order: record, invoice, code: record.code, savedLocally: opts.savedLocally }
        : { screen: "paidNoCode", order, invoice };
    case "expired":
      return { screen: "windowClosed", order, invoice };
    case "processing":
      return method === undefined
        ? unavailable
        : { screen: "awaitingConfirmation", order, invoice, gaveUp: false, method };
    case "awaiting":
    case "partPaid": {
      if (method === undefined) return unavailable;
      const screen = screenForOpenOrder(record, method);
      if (screen === "awaitingConfirmation") return { screen, order, invoice, gaveUp: false, method };
      if (invoice === undefined) return unavailable;
      if (screen === "cardForm") {
        const clientSecret = invoice.clientSecret;
        return clientSecret === undefined ? unavailable : { screen, order, invoice, clientSecret, resumed };
      }
      if (method === "card" || invoice.address === undefined || invoice.cryptoAmount === undefined) return unavailable;
      return { screen: "awaitingPayment", order, invoice, method, resumed };
    }
  }
}

export type OrderPhase = "awaiting" | "partPaid" | "processing" | "paid" | "expired";

export function orderPhase(o: PaymentProgress): OrderPhase {
  if (o.status === "paid") return "paid";
  if (o.status === "expired") return "expired";
  // Monero reports a payment as confirming while the paid amount is still zero, so the paidInFull
  // verdict must be checked before any figure.
  if (o.paidInFull === true) return "processing";
  return o.cryptoAmountPaid === undefined ? "awaiting" : "partPaid";
}

export function selectionFromOrder(
  order: Pick<OrderRecord, "badgeType" | "months"> | undefined,
  prices: readonly { priceId: string; badgeType: string }[],
  offers: readonly { offerId: string; priceId: string; months: number }[],
): { priceId: string; offerId: string } | undefined {
  if (order === undefined) return undefined;
  const price = prices.find((p) => p.badgeType === order.badgeType);
  if (price === undefined) return undefined;
  if (order.months === 1) return { priceId: price.priceId, offerId: SINGLE_MONTH };
  const offer = offers.find((o) => o.priceId === price.priceId && o.months === order.months);
  return offer === undefined ? undefined : { priceId: price.priceId, offerId: offer.offerId };
}
