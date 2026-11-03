import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { type StubElement } from "./stub-dom.js";
import { ADDRESS, NOW, openReply, ORDER_ID, seededStorage, storedOrder } from "./open-order.js";
import { CANCEL_INVOICE, CANCEL_HAS_FUNDS } from "../src/screens.js";

const refusalTest = timedTest(3000);

const storage = seededStorage();

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage, url: `/?order=${ORDER_ID}` });
const { app, fetches } = page;
page.respondWith(openReply);
await import("../src/main.js");

function cancelControl(): StubElement | undefined {
  return screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE);
}

refusalTest("main: a funded refusal keeps the order's own screen, not a stripped-down one", async () => {
  await until(() => headingOf(screenOf(app)).startsWith("Send"), "the payment screen");

  page.confirmAnswer(true);
  page.respondWith({ status: 409, body: { error: "funded" } });
  cancelControl()!.click();
  // The restarted watch reads here, and without the previous loop's options it would draw this order as a fresh one.
  page.respondWith(openReply);
  await until(() => screenOf(app).textContent.includes(CANCEL_HAS_FUNDS), "money is riding on it");

  assert.ok(screenOf(app).textContent.includes("Started"),
    "the repaint draws the order's own screen — the line saying how long it has been open — not a stripped-down one");
});

refusalTest("main: the reason does not follow the buyer to an order they did not cancel", async () => {
  assert.ok(screenOf(app).textContent.includes(CANCEL_HAS_FUNDS), "the reason is still on the order it belongs to");

  history.pushState(null, "", "/");
  page.fire("popstate");
  await settle();
  // The reply is queued before the navigation so it is ready when the order screen mounts and reads.
  page.respondWith(openReply);
  history.pushState(null, "", `?order=${ORDER_ID}`);
  page.fire("popstate");
  await until(() => headingOf(screenOf(app)).startsWith("Send"), "the payment screen again");

  assert.ok(!screenOf(app).textContent.includes(CANCEL_HAS_FUNDS),
    `a visit with no cancel behind it draws no refusal: ${screenOf(app).textContent.slice(0, 160)}`);
});

refusalTest("main: a repaint mid-cancel does not arm a second one", async () => {
  const before = fetches.filter((f) => f.url.endsWith("/cancel")).length;
  page.confirmAnswer(true);
  page.respondWith({ status: 409, body: { error: "funded" } });
  // No await after this click, so the POST stays on the wire, which is the window a second click must be refused in.
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)!.click();
  // The online event redraws the panel, giving a Cancel button whose disabled attribute is gone.
  page.fire("online");
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)?.click();

  page.respondWith(openReply);
  await settle(10);
  assert.equal(fetches.filter((f) => f.url.endsWith("/cancel")).length - before, 1,
    "the second click is refused while the first is still on the wire");
});

refusalTest("main: a not_open refusal takes the destination with it", async () => {
  page.confirmAnswer(true);
  page.respondWith({ status: 409, body: { error: "not_open" } });
  cancelControl()!.click();
  await settle();

  const after = storedOrder(storage);
  assert.notEqual(after, undefined, "the order is still known to this browser");
  for (const key of ["address", "cryptoAmount", "cryptoCurrency", "expiresAt"] as const) {
    assert.equal(after?.[key], undefined,
      `${key} must go: the service has said the invoice is closed, so nothing may be sent to it`);
  }
  assert.ok(!JSON.stringify(after).includes(ADDRESS),
    "and an offline reload cannot draw the dead address back out of the record");
});
