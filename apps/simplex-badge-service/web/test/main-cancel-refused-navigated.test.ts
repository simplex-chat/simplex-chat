// `not_open` is the service proving the invoice is closed. Its destination has to leave the record
// whether or not the buyer is still on the page: a reload draws the record whenever the first read
// fails, which is any offline reload and any read the limiter refuses, so a destination left behind
// is a payable screen and a scannable QR for an invoice nothing can reach.
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { ADDRESS, NOW, openReply, ORDER_ID, seededStorage, storedOrder } from "./open-order.js";
import { CANCEL_INVOICE } from "../src/screens.js";
import { mock } from "node:test";

const cancelTest = timedTest(3000);

const storage = seededStorage();
mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage, url: `/?order=${ORDER_ID}` });
const { app, chrome } = page;
page.respondWith(openReply);
await import("../src/main.js");

cancelTest("main: a not_open refused after the buyer left still takes the destination", async () => {
  await until(() => headingOf(screenOf(app)).startsWith("Send"), "the payment screen");
  page.confirmAnswer(true);

  // nothing armed, so the cancel holds on the wire
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)!.click();
  await settle();

  chrome.all("button.menu-item").find((b) => b.textContent === "Buy a code")!.click();
  assert.equal(headingOf(screenOf(app)), "Support SimpleX", "the buyer is starting a new purchase");

  // the invoice settled or expired on the service's clock while the buyer was deciding
  assert.ok(page.answerHeld({ status: 409, body: { error: "not_open" } }, "/cancel"),
    "the cancel is the request still holding");
  await settle();

  const after = storedOrder(storage)!;
  for (const key of ["address", "cryptoAmount", "cryptoCurrency", "expiresAt"] as const) {
    assert.equal(after[key], undefined, `${key} must go: nothing can reach that address now`);
  }
  assert.ok(!JSON.stringify(after).includes(ADDRESS), `and nothing still names it: ${JSON.stringify(after)}`);
});
