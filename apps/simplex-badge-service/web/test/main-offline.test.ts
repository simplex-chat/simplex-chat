import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, inViewOf, primaryOf, screenOf, settle, timedTest, until } from "./boot.js";
import { MemStorage, type StubElement } from "./stub-dom.js";
import { OFFLINE_NOTE } from "../src/screens.js";

const offlineTest = timedTest(3000);

const NOW = Date.parse("2026-08-28T12:00:00Z");
const CREATED = new Date(NOW - 9 * 60_000).toISOString();
const HELD_CODE = "SXB-YDC8A-YGQTM-PUYZ9-2TUXP";
const ADDRESS = "48HqK2XmVexampleAddress9fRtWc";

// An open order keeps everything the payment screen draws: where to
// send, how much, until when, and the fiat figure beside it. Nothing else
// about the payment. No `clientSecret`, and nothing on a settled order.
const storage = new MemStorage();
storage.setItem("sxb.orders.v1", JSON.stringify([{
  orderId: "inv_open", badgeType: "legend", months: 12,
  createdAt: CREATED, status: "open", code: HELD_CODE,
  address: ADDRESS, cryptoAmount: "1.482", cryptoCurrency: "xmr",
  expiresAt: "2026-08-28T12:58:12Z", amount: 42000, currency: "usd",
}]));

// The loop backs off on a network error, which is a real timer. Mocked,
// so the backoff is spent in a millisecond and the process still exits.
mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage });
const { app, fetches } = page;
// The network is gone before the module runs: this is the buyer who opened the
// tab on a train, not one who lost the network while watching.
page.setOffline(true);
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));
function noteCount(): number {
  return screenOf(app).all("p.offline").filter((p) => p.textContent === OFFLINE_NOTE).length;
}
const primary = primaryOf;

// ------------------------------------------------------- resuming with no net

offlineTest("main: a load with no network resumes the payment screen from the store alone", async () => {
  await until(() => heading().startsWith("Send"), "the payment screen drawn from the record");
  assert.equal(heading(), "Send 1.482 XMR", "the amount is the one stored with the order");
  assert.ok(screenOf(app).textContent.includes(ADDRESS), "and so is the address");
  assert.ok(screenOf(app).textContent.includes("inv_open"), "with the reference, which is all support needs");
  assert.equal(fetches.length >= 1, true, "the status was still asked for: only the provider knows it");
  assert.equal(fetches[0]!.url, "/api/invoice/inv_open");
});

offlineTest("main: the payment screen says it is offline, beside the status and not instead of it", () => {
  assert.equal(noteCount(), 1, `"${OFFLINE_NOTE}" is shown while the network is gone`);
  assert.ok(screenOf(app).textContent.includes("Waiting for the payment to confirm"),
    "the last known status stays rendered: a frozen screen must not read as a stalled payment");
  const note = screenOf(app).all("p.offline")[0]!;
  assert.equal(note.getAttribute("role"), "status", "it is announced, not just drawn");
});

offlineTest("main: the rate window is on screen offline, not silently dropped", () => {
  // Without a stored `expiresAt` this screen would show a payable address and
  // no window at all, while its own last line promises the countdown lives on
  // this URL: a buyer sending hours later lands money on a dead invoice.
  assert.ok(screenOf(app).textContent.includes("$420.00 — this rate is held for 58:12"),
    `the stored window and fiat figure are rendered: ${screenOf(app).textContent}`);
  assert.ok(screenOf(app).textContent.includes("Bookmark this page — the address and the countdown both live on this URL."),
    "which is what that line promises");
  // Past the window, with no answer from the service, the rule stands: the
  // browser never renders expiry on its own clock, so the countdown is replaced
  // rather than turned into a verdict (screens.test.ts pins the same rule).
});

offlineTest("main: the code it holds is still not on the unpaid screen", () => {
  assert.ok(page.storage.getItem("sxb.orders.v1")!.includes(HELD_CODE), "the code is in the store");
  assert.ok(!screenOf(app).serialize().includes("SXB-"), "and nowhere on a screen whose order is unpaid");
});

offlineTest("main: coming back online clears the note and asks for nothing extra", async () => {
  const before = fetches.length;
  page.setOffline(false);
  await settle();
  assert.equal(noteCount(), 0, "the note goes when the network comes back");
  assert.equal(heading(), "Send 1.482 XMR", "and the screen underneath it does not change");
  assert.equal(fetches.length, before,
    "the repaint asks nothing: the backoff is what retries, and it never stopped");
});

offlineTest("main: losing the network again puts the note back", async () => {
  page.setOffline(true);
  await settle();
  assert.equal(noteCount(), 1);
});

// ------------------------------------- what a repaint may not bring back

offlineTest("main: [ New invoice ] leaves the payment screen behind, and no repaint returns it", async () => {
  screenOf(app).all("button.secondary").find((b) => b.textContent === "New invoice")!.click();
  await settle();
  assert.equal(heading(), "Support SimpleX", "the landing screen — and the watch loop stopped that invoice's loop on the way");
  // A connectivity change repaints the screen that is waiting. There is no
  // longer one: painting the abandoned the payment screen over the wizard would put a screen
  // on top with no loop behind it, which nothing would ever update again.
  for (const off of [false, true]) {
    page.setOffline(off);
    await settle();
    assert.equal(heading(), "Support SimpleX", `an ${off ? "offline" : "online"} event must not restore the payment screen`);
    assert.ok(!app.serialize().includes(ADDRESS), "the abandoned invoice is nowhere on screen");
  }
});

// --------------------------------------------------- the unknown-order screen, which is truthful

offlineTest("main: pressing Pay offline lands on the unknown-order screen, whose copy is exactly true", async () => {
  // The whole wizard with no network at any point: the first four panels are meant to work
  // offline, because the catalog is compiled in.
  const inView = (): StubElement => inViewOf(app);
  primary(inView())!.click();                                                   // the landing screen → the tier list
  inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
  primary(inView())!.click();                                                   // the tier list → the duration list
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primary(inView())!.click();                                                   // the duration list → the order summary
  assert.equal(headingOf(inView()), "Check your order");
  assert.ok(inView().textContent.includes("$420.00"), "the prices are compiled in");

  const before = fetches.length;
  inView().all("button.primary")[0]!.click();
  await until(() => heading() === "That did not go through", "the invoice failure");
  assert.equal(fetches.length, before + 1, "it did try: the provider must create the invoice");
  assert.ok(screenOf(app).textContent.includes("The order was not created, and nothing was charged."),
    "which is exactly true with no network — the request never reached the service");
  assert.equal(noteCount(), 0, "the unknown-order screen is not a waiting screen: nothing here is going to keep checking");
});

offlineTest("main: the worker was registered on this load too, after it had rendered", () => {
  assert.deepEqual(page.workers.registrations.map((r) => r.url), ["/sw.js"]);
  assert.ok(page.workers.registrations[0]!.appChildren > 0,
    "no network is not a reason to register before the shell is confirmed");
});

offlineTest("main: the history list is not replaced by the payment screen behind it either", async () => {
  // the [ Try again ] with the network back: the create endpoint has no idempotency key, so
  // this is a second invoice, and it lands on its own the payment screen.
  page.setOffline(false);
  page.respondWith({ status: 200, body: {
    invoiceId: "inv_second", badgeType: "legend", months: 12,
    amount: 42000, currency: "usd", expiresAt: "2026-08-28T13:00:00Z",
    address: "8BdXsecondAddressExample", cryptoAmount: "1.500", cryptoCurrency: "xmr",
  } });
  screenOf(app).all("button.primary")[0]!.click();
  await until(() => heading().startsWith("Send"), "the payment screen for the second invoice");

  // The hash carrier, reached by a navigation. The history list takes the root while a
  // payment screen is what was last painted.
  page.history.pushState(null, "", "/#/codes");
  page.fire("popstate");
  await settle();
  assert.equal(heading(), "Codes on this device");
  for (const off of [true, false]) {
    page.setOffline(off);
    await settle();
    assert.equal(heading(), "Codes on this device",
      `an ${off ? "offline" : "online"} event must not paint a payment screen over the history`);
    assert.ok(!app.serialize().includes("8BdXsecondAddressExample"));
  }
});
