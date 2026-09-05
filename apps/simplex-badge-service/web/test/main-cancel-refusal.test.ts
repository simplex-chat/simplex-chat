// What a refused cancel leaves behind. The refusal itself is one round trip of screen text; what
// matters afterwards is the record it wrote, the controls the restarted watch draws, and whether
// the reason follows the buyer to a screen where no cancel was ever attempted.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { type StubElement } from "./stub-dom.js";
import { ADDRESS, NOW, openReply, ORDER_ID, seededStorage, storedOrder } from "./open-order.js";
import { CANCEL_INVOICE, CANCEL_HAS_FUNDS, NEW_INVOICE } from "../src/screens.js";

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
  // the restarted watch reads, and that repaint is where a restart without the previous loop's
  // options would draw this order as a fresh one
  page.respondWith(openReply);
  await until(() => screenOf(app).textContent.includes(CANCEL_HAS_FUNDS), "money is riding on it");

  assert.ok(screenOf(app).all("button").some((b) => b.textContent === NEW_INVOICE),
    "the repaint keeps [ New invoice ], which only the resumed options draw");
  assert.ok(screenOf(app).textContent.includes("Started"), "and the line saying how long this order has been open");
});

refusalTest("main: the reason does not follow the buyer to an order they did not cancel", async () => {
  assert.ok(screenOf(app).textContent.includes(CANCEL_HAS_FUNDS), "the reason is still on the order it belongs to");

  history.pushState(null, "", "/");
  page.fire("popstate");
  await settle();
  // queued before the navigation: the read goes out as the order screen mounts, and a reply
  // arriving after the assertion would leave it looking at the landing screen instead
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
  // no await after this click: the POST is on the wire for the whole of what follows, which is the
  // window a second click has to be refused in
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)!.click();
  // the event that redraws the panel, and with it a Cancel button whose disabled attribute is gone
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
