import { timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { type OrderRecord } from "../src/domain.js";
import { Store } from "../src/store.js";

const storeTest = timedTest(2000);


class MemoryStorage {
  map = new Map<string, string>();
  failWrites = false;
  getItem(k: string) { return this.map.get(k) ?? null; }
  setItem(k: string, v: string) { if (this.failWrites) throw new Error("QuotaExceeded"); this.map.set(k, v); }
  removeItem(k: string) { this.map.delete(k); }
}

const order = (id: string, over: Partial<OrderRecord> = {}): OrderRecord => ({
  orderId: id, badgeType: "legend", months: 12,
  createdAt: new Date(Date.parse("2026-08-24T11:02:19Z") + Number(id)).toISOString(),
  status: "open", ...over,
});

storeTest("store: session round-trips the step and the draft", () => {
  const s = new Store(new MemoryStorage());
  assert.equal(s.session().step, "tier");
  s.saveSession({ step: "months", priceId: "price_legend" });
  assert.equal(s.session().step, "months");
  assert.equal(s.session().priceId, "price_legend");
  s.clearSession();
  assert.equal(s.session().priceId, undefined);
});

storeTest("store: orders upsert by id and keep a stored code", () => {
  const s = new Store(new MemoryStorage());
  s.saveOrder(order("1", { code: "SXB-CODE" }));
  s.saveOrder(order("1", { status: "paid" }));
  const got = s.orders()[0]!;
  assert.equal(got.status, "paid");
  assert.equal(got.code, "SXB-CODE", "a later record must not clear the code");
});

storeTest("store: the cap drops the oldest entry holding no code", () => {
  const s = new Store(new MemoryStorage());
  for (let i = 0; i < 50; i++) s.saveOrder(order(String(i), i < 10 ? {} : { code: "C" + i }));
  s.saveOrder(order("999", { code: "NEW" }));
  const ids = s.orders().map((o) => o.orderId);
  assert.equal(ids.length, 50);
  assert.ok(!ids.includes("0"), "the oldest codeless entry is dropped");
  assert.ok(ids.includes("999"));
});

storeTest("store: with every entry holding a code the new one is not stored", () => {
  const s = new Store(new MemoryStorage());
  for (let i = 0; i < 50; i++) s.saveOrder(order(String(i), { code: "C" + i }));
  assert.equal(s.saveOrder(order("999", { code: "NEW" })), false);
  assert.equal(s.orders().length, 50);
});

storeTest("store: a failing storage degrades without throwing", () => {
  const mem = new MemoryStorage();
  const s = new Store(mem);
  mem.failWrites = true;
  assert.equal(s.saveOrder(order("1", { code: "C" })), false);
  assert.equal(s.saveSession({ step: "months" }), false);
  assert.deepEqual(s.orders(), []);
  assert.equal(s.session().step, "tier");
});

storeTest("store: corruption is replaced, not parsed", () => {
  const mem = new MemoryStorage();
  mem.map.set("sxb.orders.v1", "{not json");
  const s = new Store(mem);
  assert.deepEqual(s.orders(), []);
  assert.equal(s.saveOrder(order("1")), true);
  assert.equal(s.orders().length, 1);
});

storeTest("store: newestOpen finds the resumable order", () => {
  const s = new Store(new MemoryStorage());
  s.saveOrder(order("1", { status: "expired" }));
  s.saveOrder(order("2", { status: "open" }));
  s.saveOrder(order("3", { status: "paid", code: "C" }));
  assert.equal(s.newestOpen()?.orderId, "2");
});

storeTest("store: shape-corrupted but parseable entries are dropped, not thrown", () => {
  const mem = new MemoryStorage();
  mem.map.set("sxb.orders.v1", JSON.stringify([{ foo: 1 }, { bar: 2 }]));
  const s = new Store(mem);
  assert.deepEqual(s.orders(), [], "malformed elements are dropped rather than surfaced");
  assert.equal(s.saveOrder(order("1")), true, "the write path must not throw on corrupted shape");
  assert.equal(s.orders().length, 1);
});

storeTest("store: orders sort chronologically across second- and millisecond-precision timestamps", () => {
  const s = new Store(new MemoryStorage());
  // Same UTC second: a server-formatted record (to-the-second) and a
  // browser-formatted one (to-the-millisecond) that is actually later.
  const secOrder: OrderRecord = {
    orderId: "sec", badgeType: "legend", months: 12,
    createdAt: "2026-08-24T11:02:19Z", status: "open",
  };
  const msOrder: OrderRecord = {
    orderId: "ms", badgeType: "legend", months: 12,
    createdAt: "2026-08-24T11:02:19.900Z", status: "open",
  };
  s.saveOrder(secOrder);
  s.saveOrder(msOrder);
  assert.deepEqual(
    s.orders().map((o) => o.orderId),
    ["ms", "sec"],
    "the .900Z record is chronologically newer despite sorting lexicographically before the second-precision string",
  );
});

storeTest("store: an unparseable createdAt sorts oldest instead of poisoning the comparator", () => {
  // `orders()` admits any string as `createdAt`, from another tab's partial write or a hand edit, and
  // `Date.parse` answers NaN for one it cannot read. NaN makes every comparison false, so rather than sorting
  // the bad entry late it leaves the whole list to the sort algorithm, and with it which order `newestOpen` resumes.
  const mem = new MemoryStorage();
  mem.map.set("sxb.orders.v1", JSON.stringify([
    { orderId: "undated", badgeType: "legend", months: 12, createdAt: "whenever", status: "open" },
    { orderId: "older", badgeType: "legend", months: 12, createdAt: "2026-08-20T00:00:00Z", status: "open" },
  ]));
  const s = new Store(mem);
  s.saveOrder(order("1", { status: "open", createdAt: "2026-08-26T00:00:00Z" }));
  assert.deepEqual(s.orders().map((o) => o.orderId), ["1", "older", "undated"]);
  assert.equal(s.newestOpen()?.orderId, "1", "the newest DATED open order is the one resumed");
});

storeTest("store: a stored record keeps the fields that read and drops the ones that do not", () => {
  // The opposite policy from a response, and for the reason that separates them: the service
  // can be asked again, and this is the one copy of the code.
  const mem = new MemoryStorage();
  mem.map.set("sxb.orders.v1", JSON.stringify([{
    orderId: "corrupt", createdAt: "2026-08-24T11:00:00Z", status: "open",
    code: "SXB-KEEP-ME", badgeType: null, months: "12",
    currency: 42, paidInFull: "yes", cryptoCurrency: "eth", amount: 42000,
  }]));
  const stored = new Store(mem).orders();
  assert.equal(stored.length, 1, "a record with a code is never dropped over a bad field");
  const o = stored[0]!;
  assert.equal(o.code, "SXB-KEEP-ME");
  assert.equal(o.amount, 42000, "and what does read is kept");
  for (const key of ["currency", "paidInFull", "cryptoCurrency"] as const) {
    assert.equal(o[key], undefined, `${key} does not read, so it is not there to be believed`);
  }
  assert.equal(o.badgeType, "");
  assert.equal(o.months, 0);
});

storeTest("store: a record with nothing to point at is dropped whole", () => {
  const mem = new MemoryStorage();
  mem.map.set("sxb.orders.v1", JSON.stringify([
    { orderId: "no-status", createdAt: "2026-08-24T11:00:00Z" },
    { orderId: "bad-status", createdAt: "2026-08-24T11:00:00Z", status: "refunded" },
    { orderId: "", createdAt: "2026-08-24T11:00:00Z", status: "open" },
    { createdAt: "2026-08-24T11:00:00Z", status: "open" },
    "not-an-object",
    null,
  ]));
  assert.deepEqual(new Store(mem).orders(), [], "an order with no id or no state has no row to draw");
  mem.map.set("sxb.orders.v1", JSON.stringify({ orderId: "not-a-list" }));
  assert.deepEqual(new Store(mem).orders(), []);
});

storeTest("store: a session or theme outside the known set falls back to the default", () => {
  const mem = new MemoryStorage();
  mem.map.set("sxb.session.v1", JSON.stringify({ step: "elsewhere", priceId: 12, method: "paypal" }));
  mem.map.set("sxb.theme.v1", JSON.stringify("neon"));
  assert.deepEqual(new Store(mem).session(), { step: "tier" });
  assert.equal(new Store(mem).theme(), "system");

  mem.map.set("sxb.session.v1", JSON.stringify({ step: "months", priceId: "price_legend", method: "paypal" }));
  assert.deepEqual(new Store(mem).session(), { step: "months", priceId: "price_legend" },
    "a good step keeps what reads beside it, and only that");
});

storeTest("store: saveOrder replaces the stored entry rather than merging into it", () => {
  // Merging made an omitted key mean "keep what was there", so a record could go on
  // reporting a payment its writer no longer had any evidence for.
  const s = new Store(new MemoryStorage());
  s.saveOrder(order("1", { address: "48HqK2Xm", cryptoAmountPaid: "1.482", paidInFull: true, amount: 42000 }));
  s.saveOrder(order("1", { status: "expired" }));
  const stored = s.order("1")!;
  for (const key of ["address", "cryptoAmountPaid", "paidInFull", "amount"] as const) {
    assert.equal(stored[key], undefined, `${key} must not survive a write that leaves it out`);
  }
  assert.equal(stored.status, "expired");
});

storeTest("store: the two facts only this browser holds survive a write that omits them", () => {
  // A code exists nowhere else, and a card confirmation never comes back off. Everything
  // else on a record can be asked for again.
  const s = new Store(new MemoryStorage());
  s.saveOrder(order("1", { code: "SXB-CODE" }));
  assert.equal(s.markSubmitted("1"), true);
  s.saveOrder(order("1", { status: "paid" }));
  assert.equal(s.order("1")!.code, "SXB-CODE");
  assert.equal(s.order("1")!.submitted, true);
});

storeTest("store: markSubmitted is per order, sticky, and survives everything but Forget", () => {
  const mem = new MemoryStorage();
  const s = new Store(mem);
  s.saveOrder(order("1", { code: "SXB-CODE" }));
  s.saveOrder(order("2"));
  assert.equal(s.markSubmitted("1"), true);
  assert.equal(s.markSubmitted("absent"), false, "there is no order to mark");

  s.clearSession();
  s.saveOrder(order("1", { status: "open" }));   // a later 200 for the same order
  assert.equal(s.order("1")!.submitted, true, "a plain upsert must not take it back off");
  assert.equal(s.order("1")!.code, "SXB-CODE");
  assert.equal(s.order("2")!.submitted, undefined, "and it is not a page-wide flag");

  // A reload is a fresh Store over the same storage.
  assert.equal(new Store(mem).order("1")!.submitted, true);
});

storeTest("store: the cap evicts the oldest codeless entry even when an older entry holds a code", () => {
  const s = new Store(new MemoryStorage());
  // Interleaved fixture: the oldest entry overall ("0") holds a code and must
  // survive; the oldest entry with no code ("1") is the correct victim. A
  // fixture where the codeless entries are also the oldest block (as in the
  // test above) cannot distinguish "evict oldest" from "evict oldest codeless".
  for (let i = 0; i < 50; i++) s.saveOrder(order(String(i), i % 2 === 0 ? { code: "C" + i } : {}));
  s.saveOrder(order("999", { code: "NEW" }));
  const ids = s.orders().map((o) => o.orderId);
  assert.equal(ids.length, 50);
  assert.ok(ids.includes("0"), "the oldest entry overall holds a code and must survive");
  assert.ok(!ids.includes("1"), "the oldest codeless entry is evicted, not the oldest overall");
  assert.ok(ids.includes("999"));
});

storeTest("store: a store that loses its writes never claims to hold the code", () => {
  // The page falls back to an in-memory store where the browser refuses `localStorage`. That
  // store accepts every write and forgets them all on the next load, so a round trip through it
  // proves nothing: the code screen's "Saved in this browser" rests on this answer.
  const durable = new Store(new MemoryStorage());
  durable.saveOrder(order("1", { code: "SXB-CODE" }));
  assert.equal(durable.holdsCode("1", "SXB-CODE"), true);

  const losing = new Store(new MemoryStorage(), false);
  losing.saveOrder(order("1", { code: "SXB-CODE" }));
  assert.equal(losing.order("1")?.code, "SXB-CODE", "it answers with what it was handed");
  assert.equal(losing.holdsCode("1", "SXB-CODE"), false, "and still does not claim to hold it");

  assert.equal(durable.holdsCode("1", undefined), false, "no code is not a held code");
  assert.equal(durable.holdsCode("2", "SXB-CODE"), false);
});

storeTest("store: a full list cannot hold another code, and says so before the money", () => {
  // the durable half is not the only way a code goes unkept: with the cap reached and every
  // entry holding someone's code there is nothing to evict, and `saveOrder` refuses
  const s = new Store(new MemoryStorage());
  assert.equal(s.canHoldACode(), true, "an empty store has room");
  for (let i = 0; i < 50; i++) s.saveOrder(order(String(i), { code: "C" + i }));
  assert.equal(s.saveOrder(order("999", { code: "NEW" })), false, "the cap really is reached");
  assert.equal(s.canHoldACode(), false, "and the buyer can be told before paying");

  const roomy = new Store(new MemoryStorage());
  for (let i = 0; i < 50; i++) roomy.saveOrder(order(String(i), i === 7 ? {} : { code: "C" + i }));
  assert.equal(roomy.canHoldACode(), true, "one codeless entry is room enough");

  assert.equal(new Store(new MemoryStorage(), false).canHoldACode(), false, "and a losing store never has room");
});

storeTest("store: forgetting one key does not depend on the other succeeding", () => {
  // both removals are what [ Forget everything ] promises; a store that refuses the first must
  // not take the second with it
  class RefusesOrders extends MemoryStorage {
    override removeItem(k: string): void {
      if (k === "sxb.orders.v1") throw new Error("SecurityError");
      super.removeItem(k);
    }
  }
  const storage = new RefusesOrders();
  const s = new Store(storage);
  s.saveOrder(order("1", { code: "SXB-CODE" }));
  s.saveSession({ step: "checkout" });
  s.forgetEverything();
  assert.equal(storage.getItem("sxb.session.v1"), null, "the session goes even though the orders key threw");
});

storeTest("store: forgetting everything is countable, so a write awaited across it can be dropped", () => {
  const s = new Store(new MemoryStorage());
  const before = s.wipeCount;
  s.saveOrder(order("1"));
  assert.equal(s.wipeCount, before, "an ordinary write is not a wipe");
  s.forgetEverything();
  assert.equal(s.wipeCount, before + 1, "and the wipe is");
  s.forgetEverything();
  assert.equal(s.wipeCount, before + 2, "each one counts: two answers may be in flight");
});
