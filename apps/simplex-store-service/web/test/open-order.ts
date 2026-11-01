// One open Monero order shared across files, since boot.ts allows one boot per process.
import { MemStorage } from "./stub-dom.js";

export const NOW = Date.parse("2026-08-28T12:00:00Z");
export const HELD_CODE = "SB-YDC8A-YGQTM-PUYZ9-2TUXP";
export const ADDRESS = "48HqK2XmVexampleAddress9fRtWc";
export const ORDER_ID = "inv_open";

// Written straight into the map, since the stores worth seeding are the ones whose setItem throws or drops.
export function seededStorage<S extends MemStorage>(into: S = new MemStorage() as S): S {
  into.m.set("sb.orders.v1", JSON.stringify([{
    orderId: ORDER_ID, badgeType: "legend", months: 12,
    createdAt: new Date(NOW - 60_000).toISOString(), status: "open", code: HELD_CODE,
    address: ADDRESS, cryptoAmount: "1.482", cryptoCurrency: "xmr",
    expiresAt: "2026-08-28T12:58:12Z", amount: 42000, currency: "usd", method: "xmr",
  }]));
  return into;
}

export function storedOrder(from: MemStorage): Record<string, unknown> | undefined {
  return (JSON.parse(from.getItem("sb.orders.v1") ?? "[]") as Record<string, unknown>[])[0];
}

export const openReply = {
  status: 200,
  body: {
    status: "open", badgeType: "legend", months: 12,
    amount: 42000, currency: "usd", expiresAt: "2026-08-28T12:58:12Z",
    address: ADDRESS, cryptoAmount: "1.482", cryptoCurrency: "xmr",
  },
} as const;
