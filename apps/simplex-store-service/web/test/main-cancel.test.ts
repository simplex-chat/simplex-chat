import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { type StubElement } from "./stub-dom.js";
import { ADDRESS, HELD_CODE, NOW, openReply, ORDER_ID, seededStorage, storedOrder } from "./open-order.js";
import { CANCEL_INVOICE, CANCEL_FAILED, CANCEL_HAS_FUNDS, CANCEL_NOT_OPEN, CANCEL_STILL_OPEN } from "../src/screens.js";

const cancelTest = timedTest(3000);

const storage = seededStorage();

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage, url: `/?order=${ORDER_ID}` });
const { app, fetches } = page;
// This answers the watch's first read; the ?wait= after it holds, as a real one does.
page.respondWith(openReply);
await import("../src/main.js");

function cancelControl(): StubElement | undefined {
  return screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE);
}

cancelTest("main: a cancel the service refuses says so and keeps watching the order", async () => {
  await until(() => headingOf(screenOf(app)).startsWith("Send"), "the payment screen");
  page.confirmAnswer(true);
  page.respondWith({ status: 500, body: { error: "internal" } });
  const before = fetches.length;

  cancelControl()?.click();
  await settle();
  await until(() => screenOf(app).textContent.includes(CANCEL_FAILED), "the failure is on screen");
  // A count alone is satisfied by the cancel POST, so this checks for the read the restarted watch makes.
  const after = fetches.slice(before).map((f) => f.url);
  assert.ok(after.some((u) => u.startsWith(`/api/invoice/${ORDER_ID}`) && !u.endsWith("/cancel")),
    `the watch must be reading again, not left dead: ${JSON.stringify(after)}`);
});

cancelTest("main: each cancel refusal says which of the two it is", async () => {
  page.confirmAnswer(true);
  page.respondWith({ status: 409, body: { error: "funded" } });
  cancelControl()?.click();
  await settle();
  await until(() => screenOf(app).textContent.includes(CANCEL_HAS_FUNDS), "money is riding on it");
  assert.ok(!screenOf(app).textContent.includes(CANCEL_FAILED), "never the generic wording");

  // The reason must outlive the repaint the restarted watch makes, or the buyer never reads it.
  page.setOffline(true);
  await settle();
  page.setOffline(false);
  await settle();
  assert.ok(screenOf(app).textContent.includes(CANCEL_HAS_FUNDS),
    `a repaint must not take the reason away: ${screenOf(app).textContent.slice(0, 120)}`);

  page.confirmAnswer(true);
  page.respondWith({ status: 409, body: { error: "not_open" } });
  cancelControl()?.click();
  await settle();
  await until(() => screenOf(app).textContent.includes(CANCEL_NOT_OPEN), "the invoice is simply gone");
  assert.ok(!screenOf(app).textContent.includes(CANCEL_HAS_FUNDS),
    "an empty expired invoice must not be described as holding a payment");
});

cancelTest("main: a cancel answered 200 but still open is not a done deal", async () => {
  page.confirmAnswer(true);
  // An open answer with nothing on it should not happen, but if it does the address is dead and 200 alone must not read as cancelled.
  page.respondWith(openReply);

  cancelControl()?.click();
  await settle();
  await until(() => screenOf(app).textContent.includes(CANCEL_STILL_OPEN),
    "the buyer is told, not sent off to start a new purchase");
});

cancelTest("main: a cancelled order keeps its code and loses everything payable", async () => {
  page.confirmAnswer(true);
  page.respondWith({ status: 200, body: { status: "expired", amount: 42000, currency: "usd" } });

  cancelControl()!.click();
  await until(() => storedOrder(storage)!.status === "expired", "the answer is applied to the record");

  const after = storedOrder(storage)!;
  assert.equal(after.canceled, true, "the buyer's own cancel is recorded, so Your codes reads it as canceled");
  assert.equal(after.code, HELD_CODE, "the code is the one thing this browser cannot get back");
  for (const key of ["address", "cryptoAmount", "cryptoCurrency", "expiresAt"] as const) {
    assert.equal(after[key], undefined, `${key} must go: the address stops accepting payment`);
  }
  assert.ok(!JSON.stringify(after).includes(ADDRESS), "and nothing anywhere still names it");

  await until(() => headingOf(screenOf(app)) === "Support SimpleX",
    "the landing screen, ready for a new purchase");
});
