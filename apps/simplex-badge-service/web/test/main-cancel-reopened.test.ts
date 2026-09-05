// A buyer who cancels, goes to the list, and reopens the same order gets a watch built from the
// record as it stood before the cancel. When that watch's first read fails it draws the address,
// its QR and a countdown, and the cancel's answer corrects the record but repaints nothing. The
// address is dead, so the answer has to reach the screen as well as the store.
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { ADDRESS, NOW, openReply, ORDER_ID, seededStorage, storedOrder } from "./open-order.js";
import { CANCEL_INVOICE } from "../src/screens.js";
import { mock } from "node:test";

const cancelTest = timedTest(4000);

const storage = seededStorage();
mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage, url: `/?order=${ORDER_ID}` });
const { app, chrome } = page;
page.respondWith(openReply);
await import("../src/main.js");

cancelTest("main: an accepted cancel takes the payable screen down, not just the record", async () => {
  await until(() => headingOf(screenOf(app)).startsWith("Send"), "the payment screen");
  page.confirmAnswer(true);

  // the cancel holds on the wire while the buyer goes looking
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)!.click();
  await settle();

  // the menu, the list, and then the same order opened from it
  chrome.all("button.menu-item").find((b) => b.textContent === "Codes on this device")!.click();
  await settle();
  page.respondWith({ status: 500, body: { error: "internal" } });
  const open = screenOf(app).all("a").find((a) => a.textContent === "Open");
  assert.ok(open !== undefined, "the list offers the order");
  open.click({ button: 0 });
  await until(() => screenOf(app).serialize().includes(ADDRESS), "the reopened payment screen");

  assert.ok(page.answerHeld({ status: 200, body: { status: "expired", amount: 42000, currency: "usd" } }, "/cancel"),
    "the cancel is the request still holding");
  await settle(30);

  const after = storedOrder(storage)!;
  assert.equal(after.status, "expired", "the record knows the invoice is dead");
  assert.equal(after.address, undefined, "and holds nothing payable");

  const painted = screenOf(app).serialize();
  assert.ok(!painted.includes(ADDRESS),
    `the address of an invoice this buyer cancelled is still on screen: ${headingOf(screenOf(app))}`);
});
