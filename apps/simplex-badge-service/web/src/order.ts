// What an order's state is right now: what to keep from a response, and what that means for
// the screen the buyer sees and the row in their history. Nothing here waits, retries, or
// touches the network on a schedule. That is the flow's business.

import { SINGLE_MONTH } from "./catalog.js";
import { composeOrder, definedOnly } from "./domain.js";
import { screenForOpenOrder } from "./routing.js";
import type { InvoiceView } from "./api.js";
import type { Chain, Method, OrderLocalState, OrderRecord, OrderServerState, OrderStatus } from "./domain.js";
import type { Store } from "./store.js";

export type UnpaidOrder = Omit<OrderRecord, "code">;

// Strips the code by value as well as by type, since an extra property survives a
// widening cast.
export function withoutCode(record: OrderRecord): UnpaidOrder {
  const { code: _code, ...rest } = record;
  return rest;
}

/** The order with nowhere left to send money, for a refusal that proves the invoice is closed. */
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

/** The fields an order's state is read from, which a stored record and a fresh response both
 * carry. Written with `| undefined` rather than `?` so a record holding the key explicitly
 * is the same thing to this function as one that never had it. */
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
  | { kind: "expired"; order: UnpaidOrder };

export function historyRows(entries: readonly OrderRecord[]): HistoryRow[] {
  return entries.map((e) => {
    const order = withoutCode(e);
    switch (orderPhase(e)) {
      case "paid":
        return e.code !== undefined ? { kind: "paid" as const, order, code: e.code } : { kind: "paidNoCode" as const, order };
      case "expired":
        return { kind: "expired" as const, order };
      case "processing":
        return { kind: "processing" as const, order };
      case "partPaid":
        return { kind: "partPaid" as const, order };
      case "awaiting":
        return { kind: "open" as const, order };
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

/** Where to pay and until when. A partial destination is not payable, so the address, the
 * amount and the chain travel as one piece or not at all. */
interface Destination {
  address: string;
  cryptoAmount: string;
  cryptoCurrency: Chain;
  expiresAt: string | undefined;
}

/** The price of the badge, which the two figures state together: a currency without an
 * amount, or an amount without one, names no price. */
interface Price {
  amount: number;
  currency: string;
}

function destinationOf(from: Partial<OrderServerState> | undefined): Destination | undefined {
  const { address, cryptoAmount, cryptoCurrency, expiresAt } = from ?? {};
  if (address === undefined || cryptoAmount === undefined || cryptoCurrency === undefined) return undefined;
  return { address, cryptoAmount, cryptoCurrency, expiresAt };
}

/** What the service has said about money on this invoice. The record and the wire carry all four
 * as a group, so composing them one field at a time is how one goes missing. */
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

/** Every server field assembled as one group, so saving cannot leave a stale one behind and no field is
 * composed one at a time. The destination lands as undefined keys, which `definedOnly` removes for the
 * stored forms. */
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

/** Paid never falls back on the stored value, since the service withdrawing it means it is untrue; the price
 * does, settled at creation and left out of some `?wait=` answers. The destination goes once the order is not
 * open, so nothing is sent there. */
export function serverState(view: InvoiceView, held: OrderRecord | undefined): OrderServerState {
  const fresh = destinationOf(view);
  const stored = destinationOf(held);
  const to = view.status !== "open" ? undefined
    : fresh === undefined ? stored
    : { ...fresh, expiresAt: fresh.expiresAt ?? stored?.expiresAt };
  return stateOf(view.status, priceOf(view) ?? priceOf(held), paidOf(view), to);
}

/** The other half of a record, kept whole for the same reason: dropping a field here would
 * lose a code, and a code exists in no other place. */
function localState(base: OrderRecord, memory: OrderRecord | undefined, method: Method | undefined): OrderLocalState {
  return {
    code: memory?.code ?? base.code,
    submitted: memory?.submitted === true || base.submitted === true ? true : undefined,
    method: method ?? base.method,
  };
}

/** The last answer this browser saw, rebuilt from what it stored, so a reload with no network
 * shows the state the order was really in. Carrying every field is the point: leaving the
 * payment out drew the address and the countdown again for an invoice already paid in full. */
export function offlineInvoice(record: OrderRecord): InvoiceView | undefined {
  if (record.status !== "open") return undefined;
  const to = destinationOf(record);
  if (to === undefined) return undefined;
  return definedOnly(stateOf("open", priceOf(record), paidOf(record), to));
}

/** What the record knows about a closed order's money, for the screen that reports it when no
 * answer can be fetched. No destination: nothing may be sent to a closed invoice, and without this
 * the closed-window screen tells a buyer whose payment arrived that nothing was received. */
export function closedInvoice(record: OrderRecord): InvoiceView | undefined {
  if (record.status === "open") return undefined;
  return definedOnly(stateOf(record.status, priceOf(record), paidOf(record), undefined));
}

// Three complete groups over disjoint keys: what the order is, what this browser knows, and
// what the service last said. Nothing is merged into a previous record, so no field can
// outlive the answer it came from.
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
  // One dispatch on the phase, the same one the history row reads. The status comes from the
  // record, which the store owns; what has been paid comes from the fresher of the two, since
  // a held response knows about a payment before the record is written from it.
  const seen = invoice ?? record;
  switch (orderPhase({ status: record.status, cryptoAmountPaid: seen.cryptoAmountPaid, paidInFull: seen.paidInFull })) {
    case "paid":
      return record.code !== undefined
        ? { screen: "codeIssued", order: record, invoice, code: record.code, savedLocally: opts.savedLocally }
        : { screen: "paidNoCode", order, invoice };
    case "expired":
      return { screen: "windowClosed", order, invoice };
    case "processing":
      // from a full payment the address and the rate hold are the wrong things to show, and
      // a second payment would be a loss
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

/** What the buyer still owes, which the invoice status alone cannot say: the provider
 * reports a payment before it confirms, and reports one for an underpayment too.
 * `paidInFull` is the provider's own verdict: it applies a payment tolerance, so
 * comparing the amounts here would call a tolerated underpayment "part paid". */
export function orderPhase(o: PaymentProgress): OrderPhase {
  if (o.status === "paid") return "paid";
  if (o.status === "expired") return "expired";
  // The provider's verdict decides, before any figure does. Monero reports a payment as
  // confirming while `paymentMethodPaid` is still zero, so reading the amount first left a
  // paid invoice showing "waiting for payment".
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
