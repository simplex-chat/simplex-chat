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
  // The store's open order and the session step are both deliberately
  // different from the query id, so this can only pass if the query
  // parameter genuinely wins rather than a fixture where every source
  // happens to agree.
  const r = resolveLoad({ search: "?order=abc" }, o({ orderId: "different" }));
  assert.deepEqual(r, { kind: "order", orderId: "abc" });
});

routingTest("routing: a bare load resumes the newest open order, not the newest order of any status or the oldest open one", () => {
  // The globally newest record ("expired-newest-overall") is deliberately not open, so "newest open" is
  // distinguishable from "newest regardless of status", and a second open order, older than "open-newest",
  // keeps it distinguishable from "any open" and "oldest open" too.
  const store = new Store(new MemoryStorage());
  store.saveOrder(o({ orderId: "open-oldest", status: "open", createdAt: "2026-08-20T00:00:00Z" }));
  store.saveOrder(o({ orderId: "open-newest", status: "open", createdAt: "2026-08-25T00:00:00Z" }));
  store.saveOrder(o({ orderId: "expired-newest-overall", status: "expired", createdAt: "2026-08-26T00:00:00Z" }));
  const r = resolveLoad({ search: "" }, store.newestOpen());
  assert.deepEqual(r, { kind: "order", orderId: "open-newest" });
});

routingTest("routing: with no open order it is the wizard, and the hash says which panel", () => {
  // Not the stored step: `main.ts` reads the hash for that, so a bare `/` is the landing
  // screen and browser Back from it leaves the site.
  assert.deepEqual(resolveLoad({ search: "" }, undefined), { kind: "step" });
});

routingTest("routing: an open order's screen follows the method and the card confirmation", () => {
  // The settled screens belong to `viewFor`, which reads the status before it asks here.
  assert.equal(screenForOpenOrder(o({ status: "open" }), "xmr"), "awaitingPayment");
  assert.equal(screenForOpenOrder(o({ status: "open" }), "btc"), "awaitingPayment");
  assert.equal(screenForOpenOrder(o({ status: "open" }), "card"), "cardForm");
  assert.equal(screenForOpenOrder(o({ status: "open", submitted: true }), "card"), "awaitingConfirmation");
});

routingTest("routing: the flag is read off THE ORDER, so one order's confirm never speaks for another", () => {
  // The bug this replaces: the flag lived on the page-global session, so an
  // order nobody had confirmed rendered the confirming screen, "Waiting for the card network to
  // confirm", because a different order had been.
  const confirmed = o({ orderId: "inv_confirmed", status: "open", submitted: true });
  const untouched = o({ orderId: "inv_other", status: "open" });
  assert.equal(screenForOpenOrder(confirmed, "card"), "awaitingConfirmation");
  assert.equal(screenForOpenOrder(untouched, "card"), "cardForm",
    "an order with no confirm of its own is still payable");
});

routingTest("routing: the flag survives clearSession, a second order and a reload", () => {
  // The whole point of moving it off the session: `clearSession` runs on every
  // checkout 200 and on every [ New invoice ], and it used to take the money
  // rule with it, putting a live Pay button back on an order whose card
  // payment may still have been in flight.
  const storage = new MemoryStorage();
  const store = new Store(storage);
  store.saveOrder(o({ orderId: "inv_confirmed", status: "open", createdAt: "2026-08-24T11:02:19Z" }));
  assert.equal(store.markSubmitted("inv_confirmed"), true);

  store.clearSession();
  store.saveOrder(o({ orderId: "inv_second", status: "open", createdAt: "2026-08-24T12:00:00Z" }));
  store.clearSession();

  // A reload is a fresh Store over the same storage, and nothing else.
  const reloaded = new Store(storage);
  const kept = reloaded.order("inv_confirmed")!;
  assert.equal(kept.submitted, true, "the flag is in the orders key, which nothing but Forget clears");
  assert.equal(screenForOpenOrder(kept, "card"), "awaitingConfirmation");
  assert.equal(reloaded.order("inv_second")!.submitted, undefined,
    "and it did not spread to the order that followed it");
});
