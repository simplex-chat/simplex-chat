import type { Method, OrderRecord } from "./domain.js";

export type Screen = "awaitingPayment" | "awaitingConfirmation" | "cardForm";

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
