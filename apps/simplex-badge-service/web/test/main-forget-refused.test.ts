// The other half of the wipe race: the cancel the service refuses. The failure path has to start
// the watch again or the page sits on a screen nothing updates, and a watch started over a store
// the buyer just wiped writes the forgotten order back on its first read.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
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

  // no settle: the cancel is on the wire, and this is the wipe landing while it is
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Forget everything on this device")!.click();
  assert.equal(storage.getItem("sxb.orders.v1"), null, "the wipe itself is immediate");

  // the refusal lands, and the watch it would restart is the thing that puts the record back
  page.respondWith(openReply);
  await settle(10);
  assert.equal(storage.getItem("sxb.orders.v1"), null,
    "the order the buyer erased does not come back, with its address, as a row they cannot remove");
});
