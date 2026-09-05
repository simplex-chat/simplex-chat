// The checkout is bought over a round trip the buyer can navigate away from. `flow.checkout` saves
// the order and its code before it answers, so a store that took the write still has it. But the
// answer must not take the root or the address bar of whatever page the buyer went to, and must
// not start a watch for an order the buyer has walked away from.
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, settle, timedTest, until } from "./boot.js";

const payTest = timedTest(3000);

const page = installPage();
const { app, fetches, location, storage } = page;
await import("../src/main.js");

const inView = (): ReturnType<typeof inViewOf> => inViewOf(app);

payTest("main: a checkout that lands after the buyer left keeps its order but not the page", async () => {
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Check your order");

  // nothing is armed, so the POST holds on the wire while the buyer navigates
  const before = fetches.length;
  inView().all("button.primary")[0]!.click();
  await until(() => fetches.slice(before).some((f) => f.url === "/api/invoice"), "the checkout POST");

  page.chrome.all("button.menu-item").find((b) => b.textContent === "Codes on this device")!.click();
  assert.equal(headingOf(inView()), "Codes on this device", "the page the buyer asked for");

  // and only now does the invoice come back
  assert.ok(page.answerHeld({
    status: 200,
    body: {
      invoiceId: "inv_left", badgeType: "legend", months: 12,
      amount: 42000, currency: "usd", expiresAt: "2026-08-28T13:00:00Z",
      address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr",
    },
  }, "/api/invoice"), "the checkout POST is the one still holding");
  await settle();

  assert.equal(headingOf(inView()), "Codes on this device",
    "the checkout's answer does not paint over the page the buyer went to");

  assert.equal(location.search, "",
    `the address bar belongs to the page the buyer is on: ${location.search}`);
  const watches = fetches.slice(before).filter((f) => f.url.startsWith("/api/invoice/"));
  assert.equal(watches.length, 0,
    `no watch may start for an order the buyer navigated away from: ${JSON.stringify(watches.map((f) => f.url))}`);

  // the invoice was really bought, so it and its code have to be recoverable from this browser
  const stored = JSON.parse(storage.getItem("sxb.orders.v1") ?? "[]") as Array<Record<string, unknown>>;
  const saved = stored.find((o) => o.orderId === "inv_left");
  assert.ok(saved, `the order is kept whatever the page did: ${JSON.stringify(stored)}`);
  assert.equal(typeof saved.code, "string", "and with the code only this browser holds");
});
