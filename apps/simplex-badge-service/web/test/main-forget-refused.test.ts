// A refused cancel restarts the watch, and a watch started over a just-wiped store would write the forgotten order back on its first read.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { forgetControl, headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { NOW, openReply, ORDER_ID, seededStorage } from "./open-order.js";
import { CANCEL_INVOICE } from "../src/screens.js";

const refusedTest = timedTest(3000);

const storage = seededStorage();

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage, url: `/?order=${ORDER_ID}` });
const { app } = page;
page.respondWith(openReply);
await import("../src/main.js");


refusedTest("main: a refused cancel answered after the wipe is not written back", async () => {
  await until(() => headingOf(screenOf(app)).startsWith("Send"), "the payment screen");

  page.confirmAnswer(true);
  page.respondWith({ status: 409, body: { error: "funded" } });
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)!.click();

  // There is no settle here, because the cancel is on the wire while the wipe lands.
  forgetControl(page)!.click();
  assert.equal(storage.getItem("sb.orders.v1"), null, "the wipe itself is immediate");

  page.respondWith(openReply);
  await settle(10);
  assert.equal(storage.getItem("sb.orders.v1"), null,
    "the order the buyer erased does not come back, with its address, as a row they cannot remove");
});
