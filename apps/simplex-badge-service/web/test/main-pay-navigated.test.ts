// flow.checkout saves the order before it answers, but the answer must not take the root, the address bar, or start a watch for a page the buyer left.
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

  // Nothing is armed, so the POST holds on the wire while the buyer navigates.
  const before = fetches.length;
  inView().all("button.primary")[0]!.click();
  await until(() => fetches.slice(before).some((f) => f.url === "/api/invoice"), "the checkout POST");

  page.chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();
  assert.equal(headingOf(inView()), "Your codes", "the page the buyer asked for");

  assert.ok(page.answerHeld({
    status: 200,
    body: {
      invoiceId: "inv_left", badgeType: "legend", months: 12,
      amount: 42000, currency: "usd", expiresAt: "2026-08-28T13:00:00Z",
      address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr",
    },
  }, "/api/invoice"), "the checkout POST is the one still holding");
  await settle();

  assert.equal(headingOf(inView()), "Your codes",
    "the checkout's answer does not paint over the page the buyer went to");

  assert.equal(location.search, "",
    `the address bar belongs to the page the buyer is on: ${location.search}`);
  const watches = fetches.slice(before).filter((f) => f.url.startsWith("/api/invoice/"));
  assert.equal(watches.length, 0,
    `no watch may start for an order the buyer navigated away from: ${JSON.stringify(watches.map((f) => f.url))}`);

  const stored = JSON.parse(storage.getItem("sb.orders.v1") ?? "[]") as Array<Record<string, unknown>>;
  const saved = stored.find((o) => o.orderId === "inv_left");
  assert.ok(saved, `the order is kept whatever the page did: ${JSON.stringify(stored)}`);
  assert.equal(typeof saved.code, "string", "and with the code only this browser holds");
});
