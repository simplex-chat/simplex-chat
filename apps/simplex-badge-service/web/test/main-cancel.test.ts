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
// the first read the watch makes; the `?wait=` after it holds, as a real one does
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
  // a count alone is satisfied by the cancel POST itself, so this asks for the read the
  // restarted watch makes: without it the page sits on a screen nothing updates any more
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

  // the other refusal is a different fact, and saying this one for it would be false
  // the reason has to outlive the screen it was asked on: the watch this restarts repaints that
  // screen within a round trip, and a reason the buyer reads for 200ms is one they never read
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
  // The service expires the row once the provider has accepted the cancel, and a payment landing
  // in between is the branch above, so an open answer with nothing on it should not happen at all.
  // If it ever does, the address is dead and 200 alone must not read as cancelled.
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
  assert.equal(after.code, HELD_CODE, "the code is the one thing this browser cannot get back");
  for (const key of ["address", "cryptoAmount", "cryptoCurrency", "expiresAt"] as const) {
    assert.equal(after[key], undefined, `${key} must go: the address stops accepting payment`);
  }
  assert.ok(!JSON.stringify(after).includes(ADDRESS), "and nothing anywhere still names it");

  // the arm this drives is the whole point of a clean cancel: nothing arrived, so the buyer is
  // put back at the start rather than left watching an invoice that can never move again
  await until(() => headingOf(screenOf(app)) === "Support SimpleX",
    "the landing screen, ready for a new purchase");
});
