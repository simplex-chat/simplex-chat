// One round trip, two decisions: the buyer cancels an invoice and, before the service answers,
// wipes the browser. The answer is a write, and writing it would put back a record the buyer
// asked to forget, without its code, which the wipe took.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { NOW, openReply, ORDER_ID, seededStorage } from "./open-order.js";
import { CANCEL_INVOICE } from "../src/screens.js";

const raceTest = timedTest(3000);

const storage = seededStorage();

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage, url: `/?order=${ORDER_ID}` });
const { app } = page;
page.respondWith(openReply);
await import("../src/main.js");


raceTest("main: a cancel answered after the wipe is not written back", async () => {
  await until(() => headingOf(screenOf(app)).startsWith("Send"), "the payment screen");

  page.confirmAnswer(true);
  page.respondWith({ status: 200, body: { status: "expired", amount: 42000, currency: "usd" } });
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)!.click();

  // no settle: the cancel is on the wire, and this is the wipe landing while it is
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Forget everything on this device")!.click();
  assert.equal(storage.getItem("sxb.orders.v1"), null, "the wipe itself is immediate");

  await settle(10);
  assert.equal(storage.getItem("sxb.orders.v1"), null,
    "and the cancelled invoice does not come back as a row the buyer cannot remove");
});
