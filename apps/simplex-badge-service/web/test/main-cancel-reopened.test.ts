// A reopened order builds its watch from the record before the cancel, so the cancel's answer must reach the screen and not only the store.
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

  // The cancel holds on the wire while the buyer goes looking.
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)!.click();
  await settle();

  chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();
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
