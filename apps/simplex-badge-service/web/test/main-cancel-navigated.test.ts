// A cancel the buyer walks away from still has to be written down. The record is what an offline
// reload draws from, so an order left `open` with its destination draws a dead address and a QR
// for it, which is the thing `cancelInvoice` says it exists to prevent.
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { ADDRESS, HELD_CODE, NOW, openReply, ORDER_ID, seededStorage, storedOrder } from "./open-order.js";
import { CANCEL_INVOICE } from "../src/screens.js";
import { mock } from "node:test";

const cancelTest = timedTest(3000);

const storage = seededStorage();
mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage, url: `/?order=${ORDER_ID}` });
const { app, chrome } = page;
page.respondWith(openReply);
await import("../src/main.js");

cancelTest("main: a cancel answered after the buyer left is still written to the record", async () => {
  await until(() => headingOf(screenOf(app)).startsWith("Send"), "the payment screen");
  page.confirmAnswer(true);

  // nothing armed, so the cancel holds on the wire
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)!.click();
  await settle();

  chrome.all("button.menu-item").find((b) => b.textContent === "Buy a code")!.click();
  assert.equal(headingOf(screenOf(app)), "Support SimpleX", "the buyer is starting a new purchase");

  assert.ok(page.answerHeld({ status: 200, body: { status: "expired", amount: 42000, currency: "usd" } }, "/cancel"),
    "the cancel is the request still holding");
  await settle();

  const after = storedOrder(storage)!;
  assert.equal(after.status, "expired", `the invoice is dead and the record says so: ${JSON.stringify(after)}`);
  for (const key of ["address", "cryptoAmount", "expiresAt"] as const) {
    assert.equal(after[key], undefined, `${key} must go: an offline reload draws the record`);
  }
  assert.ok(!JSON.stringify(after).includes(ADDRESS), "and nothing anywhere still names the dead address");
  assert.equal(after.code, HELD_CODE, "the code is the one thing this browser cannot get back");
});
