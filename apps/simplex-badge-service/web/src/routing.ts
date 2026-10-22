import type { App, Method, OrderRecord } from "./domain.js";

export type Screen = "awaitingPayment" | "awaitingConfirmation" | "cardForm";

export type Load = { kind: "order"; orderId: string } | { kind: "step" };

export interface UrlParts { search: string }

export interface HashParts { route: string; app: App | undefined }

// The flag rides in the fragment, which is never sent to the service nor carried in a Referer.
export function readHash(hash: string): HashParts {
  const at = hash.indexOf("?");
  if (at < 0) return { route: hash, app: undefined };
  const flag = new URLSearchParams(hash.slice(at + 1)).get("app");
  return {
    route: hash.slice(0, at),
    app: flag === "true" ? "mobile" : flag === "desktop" ? "desktop" : undefined,
  };
}

// A bearer token: whoever reads this URL can redeem the badge.
export function appCodeLink(code: string): string {
  return `simplexchat:/badge/code/${encodeURIComponent(code)}`;
}

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
