import { timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { CATALOG, offerTotal, savingPercent } from "../src/catalog.js";

const catalogTest = timedTest(2000);


catalogTest("catalog: one month at the month price", () => {
  assert.deepEqual(offerTotal(7000, undefined), { months: 1, price: 7000, amount: 7000 });
});

catalogTest("catalog: free months charge for the difference and deliver the term", () => {
  const offer = { offerId: "o", priceId: "p", months: 3, discount: { type: "freeMonths", freeMonths: 1 } } as const;
  assert.deepEqual(offerTotal(7000, offer), { months: 3, price: 21000, amount: 14000 });
});

catalogTest("catalog: percentage discount truncates in the buyer's favour", () => {
  const offer = { offerId: "o", priceId: "p", months: 3, discount: { type: "discount", discount: 33 } } as const;
  // 21000 * 67 / 100 = 14070
  assert.deepEqual(offerTotal(7000, offer), { months: 3, price: 21000, amount: 14070 });
});

catalogTest("catalog: every guard returns a reason, never a number", () => {
  const bad = [
    { offerId: "a", priceId: "p", months: 0, discount: { type: "discount", discount: 10 } },
    { offerId: "b", priceId: "p", months: 3, discount: { type: "freeMonths", freeMonths: 3 } },
    { offerId: "c", priceId: "p", months: 3, discount: { type: "discount", discount: 100 } },
  ] as const;
  for (const o of bad) assert.equal(typeof offerTotal(7000, o), "string", `${o.offerId} must be refused`);
  assert.equal(typeof offerTotal(0, undefined), "string", "a zero month price is unsellable");
});

catalogTest("catalog: the compiled-in catalog matches the screens", () => {
  const s = CATALOG.prices.find((p) => p.badgeType === "supporter");
  const l = CATALOG.prices.find((p) => p.badgeType === "legend");
  assert.equal(s?.monthPrice, 700);
  assert.equal(l?.monthPrice, 7000);
});

catalogTest("catalog: the saving percentage is display-only arithmetic", () => {
  assert.equal(savingPercent(21000, 14000), 33);
  assert.equal(savingPercent(7000, 7000), 0);
});

catalogTest("catalog: guards reject fractional months", () => {
  const offer = { offerId: "o", priceId: "p", months: 2.5, discount: { type: "freeMonths", freeMonths: 1 } } as const;
  assert.equal(typeof offerTotal(700, offer), "string", "fractional months must be rejected");
});

catalogTest("catalog: guards reject negative free months", () => {
  const offer = { offerId: "o", priceId: "p", months: 1, discount: { type: "freeMonths", freeMonths: -5 } } as const;
  assert.equal(typeof offerTotal(700, offer), "string", "negative free months must be rejected");
});

catalogTest("catalog: guards reject negative discount", () => {
  const offer = { offerId: "o", priceId: "p", months: 1, discount: { type: "discount", discount: -50 } } as const;
  assert.equal(typeof offerTotal(700, offer), "string", "negative discount must be rejected");
});

catalogTest("catalog: a full price over the cap is unsellable, as the service also refuses it", () => {
  // Catalog.hs guards the gross as well as the charge, because a price that large does not
  // fit the column. Guarding only the charge here made the page offer a total checkout refused.
  const offer = { offerId: "o", priceId: "p", months: 3, discount: { type: "freeMonths", freeMonths: 2 } } as const;
  assert.equal(typeof offerTotal(50_000_000, offer), "string", "gross 150000000 is over the cap");
  const ok = offerTotal(3500, { offerId: "o", priceId: "p", months: 12, discount: { type: "freeMonths", freeMonths: 2 } });
  assert.deepEqual(ok, { months: 12, price: 42000, amount: 35000 }, "an ordinary offer still prices");
});
