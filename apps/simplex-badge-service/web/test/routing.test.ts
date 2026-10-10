import { timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { resolveLoad, screenForOpenOrder } from "../src/routing.js";
import { type OrderRecord } from "../src/domain.js";
import { Store } from "../src/store.js";

const routingTest = timedTest(2000);

class MemoryStorage {
  map = new Map<string, string>();
  getItem(k: string) { return this.map.get(k) ?? null; }
  setItem(k: string, v: string) { this.map.set(k, v); }
  removeItem(k: string) { this.map.delete(k); }
}

const o = (over: Partial<OrderRecord>): OrderRecord => ({
  orderId: "abc", badgeType: "legend", months: 12,
  createdAt: "2026-08-24T11:02:19Z", status: "open", ...over,
});

routingTest("routing: ?order= wins over the hash and the store", () => {
  const r = resolveLoad({ search: "?order=abc" }, o({ orderId: "different" }));
  assert.deepEqual(r, { kind: "order", orderId: "abc" });
});

routingTest("routing: a bare load resumes the newest open order, not the newest order of any status or the oldest open one", () => {
  const store = new Store(new MemoryStorage());
  store.saveOrder(o({ orderId: "open-oldest", status: "open", createdAt: "2026-08-20T00:00:00Z" }));
  store.saveOrder(o({ orderId: "open-newest", status: "open", createdAt: "2026-08-25T00:00:00Z" }));
  store.saveOrder(o({ orderId: "expired-newest-overall", status: "expired", createdAt: "2026-08-26T00:00:00Z" }));
  const r = resolveLoad({ search: "" }, store.newestOpen());
  assert.deepEqual(r, { kind: "order", orderId: "open-newest" });
});

routingTest("routing: with no open order it is the wizard, and the hash says which panel", () => {
  assert.deepEqual(resolveLoad({ search: "" }, undefined), { kind: "step" });
});

routingTest("routing: an open order's screen follows the method and the card confirmation", () => {
  assert.equal(screenForOpenOrder(o({ status: "open" }), "xmr"), "awaitingPayment");
  assert.equal(screenForOpenOrder(o({ status: "open" }), "btc"), "awaitingPayment");
  assert.equal(screenForOpenOrder(o({ status: "open" }), "card"), "cardForm");
  assert.equal(screenForOpenOrder(o({ status: "open", submitted: true }), "card"), "awaitingConfirmation");
});

routingTest("routing: the flag is read off THE ORDER, so one order's confirm never speaks for another", () => {
  const confirmed = o({ orderId: "inv_confirmed", status: "open", submitted: true });
  const untouched = o({ orderId: "inv_other", status: "open" });
  assert.equal(screenForOpenOrder(confirmed, "card"), "awaitingConfirmation");
  assert.equal(screenForOpenOrder(untouched, "card"), "cardForm",
    "an order with no confirm of its own is still payable");
});

routingTest("routing: the flag survives clearSession, a second order and a reload", () => {
  const storage = new MemoryStorage();
  const store = new Store(storage);
  store.saveOrder(o({ orderId: "inv_confirmed", status: "open", createdAt: "2026-08-24T11:02:19Z" }));
  assert.equal(store.markSubmitted("inv_confirmed"), true);

  store.clearSession();
  store.saveOrder(o({ orderId: "inv_second", status: "open", createdAt: "2026-08-24T12:00:00Z" }));
  store.clearSession();

  const reloaded = new Store(storage);
  const kept = reloaded.order("inv_confirmed")!;
  assert.equal(kept.submitted, true, "the flag is in the orders key, which nothing but Forget clears");
  assert.equal(screenForOpenOrder(kept, "card"), "awaitingConfirmation");
  assert.equal(reloaded.order("inv_second")!.submitted, undefined,
    "and it did not spread to the order that followed it");
});
