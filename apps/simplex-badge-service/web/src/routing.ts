import type { Method, OrderRecord } from "./domain.js";

/** What an open order shows. The settled screens are `viewFor`'s to choose, from the status. */
export type Screen = "awaitingPayment" | "awaitingConfirmation" | "cardForm";

/** An order to open, or the wizard. Which panel of the wizard is the hash's business, not
 * this function's: a bare `/` is the landing screen whatever the stored session says. */
export type Load = { kind: "order"; orderId: string } | { kind: "step" };

export interface UrlParts { search: string }

export function resolveLoad(url: UrlParts, newestOpen: OrderRecord | undefined): Load {
  const orderId = new URLSearchParams(url.search).get("order");
  if (orderId) return { kind: "order", orderId };
  if (newestOpen) return { kind: "order", orderId: newestOpen.orderId };
  return { kind: "step" };
}

export function screenForOpenOrder(order: OrderRecord, method: Method): Screen {
  if (method !== "card") return "awaitingPayment";
  return order.submitted === true ? "awaitingConfirmation" : "cardForm";
}
