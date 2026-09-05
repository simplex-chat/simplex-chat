// A card purchase in a browser whose orders list is full: every stored row holds a code, so
// `saveOrder` can evict nothing and the new order is never written. The order summary says so
// before the money ("copy the code as soon as it appears"), and the code screen is where it
// appears. This walks the whole purchase and asks whether it does.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, inViewOf, primaryOf, screenOf, settle, timedTest, until } from "./boot.js";
import { MemStorage, type StubElement } from "./stub-dom.js";

const capTest = timedTest(5000);
const NOW = Date.parse("2026-08-28T12:00:00Z");

const full = new MemStorage();
full.m.set("sxb.orders.v1", JSON.stringify(
  Array.from({ length: 50 }, (_, i) => ({
    orderId: `inv_old_${i}`, badgeType: "supporter", months: 1,
    createdAt: new Date(NOW - (i + 2) * 86_400_000).toISOString(), status: "paid", paidInFull: true,
    code: `SXB-OLD${String(i).padStart(2, "0")}-YGQTM-PUYZ9-2TUXP`, amount: 700, currency: "usd",
  })),
));

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const page = installPage({ storage: full });
const { app } = page;
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));
const inView = (): StubElement => inViewOf(app);

capTest("main: a card code survives the confirm when the orders list is full", async () => {
  primaryOf(inView())!.click();                                                  // landing → tiers
  inView().all("button.choice").find((c) => c.textContent.startsWith("Supporter"))!.click();
  primaryOf(inView())!.click();                                                  // tiers → durations
  inView().all("button.choice").find((c) => c.textContent.startsWith("1 month"))!.click();
  primaryOf(inView())!.click();                                                  // durations → summary
  assert.equal(headingOf(inView()), "Check your order");
  assert.ok(inView().textContent.includes("This browser cannot save anything new right now."),
    "the store is full, and the summary says so before the money");

  inView().all("button.choice.method").find((b) => b.textContent.startsWith("Card"))!.click();
  page.respondWith({ status: 200, body: {
    invoiceId: "inv_card", badgeType: "supporter", months: 1,
    amount: 700, currency: "usd", expiresAt: "2026-08-28T12:58:12Z",
    clientSecret: "cs_test_abc",
  } });
  inView().all("button.primary").find((b) => b.textContent.startsWith("Pay"))!.click();
  await until(() => heading() === "Pay by card", `the card form, not ${heading()}`);

  const codeShape = /SXB-[0-9A-Z]{5}-[0-9A-Z]{5}-[0-9A-Z]{5}-[0-9A-Z]{5}/;
  assert.ok(!codeShape.test(screenOf(app).serialize()), "no code is on an unpaid screen");

  screenOf(app).all("button").find((b) => b.textContent === "Simulate a confirmed card payment")!.click();
  await until(() => heading() === "Payment received", `the confirming screen, not ${heading()}`);

  // the card network confirms, which is the only thing between the buyer and their code
  page.answerHeld({ status: 200, body: {
    status: "paid", badgeType: "supporter", months: 1,
    amount: 700, currency: "usd", paidInFull: true, settledAt: "2026-08-28T12:05:00Z",
  } }, "/api/invoice/inv_card");
  await settle();

  const painted = screenOf(app).serialize();
  assert.equal(heading(), "Paid. Here is your code.",
    `the code screen, not "${heading()}": ${painted.slice(0, 400)}`);
  assert.ok(codeShape.test(painted), `and the code is on it: ${painted.slice(0, 400)}`);
});
