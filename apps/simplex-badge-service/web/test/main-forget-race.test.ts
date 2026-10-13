// The cancel's answer is a write, so it would put back a record the buyer asked to forget, without its code.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { forgetControl, headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
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

  // There is no settle here, because the cancel is on the wire while the wipe lands.
  forgetControl(page)!.click();
  assert.equal(storage.getItem("sb.orders.v1"), null, "the wipe itself is immediate");

  await settle(10);
  assert.equal(storage.getItem("sb.orders.v1"), null,
    "and the cancelled invoice does not come back as a row the buyer cannot remove");
});
