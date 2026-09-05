// One open Monero order, seeded into storage and answered by the first read. `boot.ts` allows one
// boot per process, so every scenario needing this state is its own file; they share the fixture
// rather than each carrying a copy of it.
import { MemStorage } from "./stub-dom.js";

export const NOW = Date.parse("2026-08-28T12:00:00Z");
export const HELD_CODE = "SXB-YDC8A-YGQTM-PUYZ9-2TUXP";
export const ADDRESS = "48HqK2XmVexampleAddress9fRtWc";
export const ORDER_ID = "inv_open";

/** The record as the store holds it: an open order this browser still has the code for. Written
 * straight into the map, because the stores that most need seeding are the ones whose `setItem`
 * throws or silently drops, and seeding them through it would seed nothing. */
export function seededStorage<S extends MemStorage>(into: S = new MemStorage() as S): S {
  into.m.set("sxb.orders.v1", JSON.stringify([{
    orderId: ORDER_ID, badgeType: "legend", months: 12,
    createdAt: new Date(NOW - 60_000).toISOString(), status: "open", code: HELD_CODE,
    address: ADDRESS, cryptoAmount: "1.482", cryptoCurrency: "xmr",
    expiresAt: "2026-08-28T12:58:12Z", amount: 42000, currency: "usd", method: "xmr",
  }]));
  return into;
}

/** The order as the store holds it now. These fixtures seed exactly one, so it is the only one. */
export function storedOrder(from: MemStorage): Record<string, unknown> | undefined {
  return (JSON.parse(from.getItem("sxb.orders.v1") ?? "[]") as Record<string, unknown>[])[0];
}

/** The same order as the service reports it, for the first read the watch makes. */
export const openReply = {
  status: 200,
  body: {
    status: "open", badgeType: "legend", months: 12,
    amount: 42000, currency: "usd", expiresAt: "2026-08-28T12:58:12Z",
    address: ADDRESS, cryptoAmount: "1.482", cryptoCurrency: "xmr",
  },
} as const;
