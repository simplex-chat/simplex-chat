import { after, mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { MemStorage } from "./stub-dom.js";

const resumeTest = timedTest(3000);

const NOW = Date.parse("2026-08-28T12:00:00Z");
const CREATED = new Date(NOW - 14 * 60_000).toISOString();
const HELD_CODE = "SXB-YDC8A-YGQTM-PUYZ9-2TUXP";

// the watch loop: "someone who sent a Monero payment and closed the tab reopens
// badges.simplex.chat and sees their pending invoice, not a landing page."
const storage = new MemStorage();
storage.setItem("sxb.orders.v1", JSON.stringify([{
  orderId: "inv_open", badgeType: "legend", months: 12,
  createdAt: CREATED, status: "open", code: HELD_CODE,
}, {
  // A card order whose `actions.confirm()` returned success, in a browser that
  // has since started another order. the flag lives on the ORDER, so
  // it is still here. This is the record the confirming screen test below opens. It is older
  // than `inv_open`, which keeps "resume the newest open order" meaningful.
  orderId: "inv_card", badgeType: "supporter", months: 1,
  createdAt: new Date(NOW - 40 * 60_000).toISOString(), status: "open", submitted: true,
}]));

// The clock is fixed so "Started 14 minutes ago" is a fact and not a race, and
// `setTimeout` with it so the fifteen minutes can be spent in a millisecond.
// Nothing schedules a timer until the confirming screen arms its deadline. `setImmediate` is
// deliberately not mocked: it is what `settle` drains.
mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage });
const { app, history, location, fetches } = page;

const crypto = {
  status: "open", badgeType: "legend", months: 12, amount: 42000,
  currency: "usd", expiresAt: "2026-08-28T12:58:12Z",
  address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr",
};

// The plain first read of the read endpoint is answered before the module runs, because
// `main.ts` issues it during import.
page.respondWith({ status: 200, body: crypto });
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));

// ------------------------------------------------------------------- the resume

resumeTest("main: a FRESH LOAD at / resumes the newest open order", async () => {
  await until(() => heading().startsWith("Send"), "awaitingPayment");
  // Not the landing page: the pending invoice, with everything needed to pay.
  assert.equal(heading(), "Send 1.482 XMR");
  assert.ok(screenOf(app).textContent.includes("48HqK2XmVexampleAddress9fRtWc"));
  assert.ok(screenOf(app).textContent.includes("inv_open"));
  assert.equal(fetches[0]!.url, "/api/invoice/inv_open", "the status is fetched first");
  assert.ok(!fetches[0]!.url.includes("wait="), "plainly, so the address appears at once");
});

resumeTest("main: a resumed payment screen says how long ago it started, and offers a way out", () => {
  // Both of these turn on the `resumed` flag reaching the screen. A reload
  // used to produce `resumed: false` and lose both.
  assert.ok(screenOf(app).textContent.includes("Started 14 minutes ago."), screenOf(app).textContent);
  const out = screenOf(app).all("button.secondary").find((b) => b.textContent === "New invoice");
  assert.ok(out, "the give-up rule gives a resumed payment screen [ New invoice ]");
});

resumeTest("main: the resumed screen still never shows the code it holds", () => {
  assert.ok(page.storage.getItem("sxb.orders.v1")!.includes(HELD_CODE), "the code is in the store");
  assert.ok(!screenOf(app).serialize().includes(HELD_CODE), "and nowhere on the unpaid screen");
  assert.ok(!screenOf(app).serialize().includes("SXB-"));
});

// ------------------------------------------- detailsUnavailable, end to end

resumeTest("main: [ Check again ] on detailsUnavailable RE-RENDERS, and never blanks", async () => {
  // An open order whose response names no method: the table has no row, so
  // the buyer is given the reference. Its only control used to replace the
  // screen with a placeholder that nothing would ever clear.
  page.respondWith({ status: 200, body: { status: "open" } });
  history.pushState(null, "", "?order=inv_bare");
  page.fire("popstate");
  await until(() => heading() === "The payment details are not available", "detailsUnavailable");
  assert.ok(screenOf(app).textContent.includes("inv_bare"), "the reference is the point of this screen");

  const check = screenOf(app).all("button.primary").find((b) => b.textContent === "Check again")!;
  check.click();
  await settle();
  assert.equal(heading(), "The payment details are not available",
    "the loop is still live, so the screen stands rather than becoming a placeholder");
  assert.ok(screenOf(app).textContent.includes("inv_bare"), "and the reference is still on it");

  const out = screenOf(app).all("button.secondary").find((b) => b.textContent === "New invoice");
  assert.ok(out, "this screen's order is open with nothing paid, so it may offer one");
});

// ----------------------------- the confirming screen and its give-up, end to end

resumeTest("main: the confirming screen gives up after fifteen minutes and [ Check again ] restarts it", async () => {
  // `submitted` is the browser's own note that this order's confirm() succeeded, and it is on the order
  // record seeded above, not the session, which every checkout 200 and every [ New invoice ] wipes. The
  // session is cleared here first because the rule that withholds a second charge must not go with it.
  page.storage.removeItem("sxb.session.v1");
  {
    page.respondWith({ status: 200, body: { status: "open", badgeType: "supporter", months: 1, clientSecret: "cs_test_abc" } });
    history.pushState(null, "", "?order=inv_card");
    page.fire("popstate");
    await until(() => heading() === "Payment received", "awaitingConfirmation");
    assert.ok(screenOf(app).textContent.includes("Still processing"));

    mock.timers.tick(15 * 60_000);
    await settle();
    assert.equal(heading(), "This is taking longer than expected", "the give-up");
    assert.ok(screenOf(app).textContent.includes("inv_card"), "quoting the reference");

    // The reversal: no control here may start a second charge. confirm()
    // returned success, the create endpoint has no idempotency key, and [ New invoice ]
    // cancels nothing: it abandons an invoice that may yet settle.
    assert.equal(screenOf(app).all("button").filter((b) => b.textContent === "New invoice").length, 0,
      "the give-up screen must not offer [ New invoice ]");

    // And [ Check again ] restarts the loop it had stopped, as the confirming screen.
    const before = fetches.length;
    page.respondWith({ status: 200, body: { status: "open", badgeType: "supporter", months: 1, clientSecret: "cs_test_abc" } });
    screenOf(app).all("button.primary").find((b) => b.textContent === "Check again")!.click();
    await until(() => heading() === "Payment received", "the confirming screen again");
    // Checking again means asking the server: a plain read, and then the
    // hold that the answer starts. Not a second loop: one of each.
    assert.equal(fetches[before]!.url, "/api/invoice/inv_card", "a plain read, not a hold");
    assert.deepEqual(fetches.slice(before).map((f) => f.url),
      ["/api/invoice/inv_card", "/api/invoice/inv_card?wait=open&seenPaid=&seenFull=0"]);

    // Re-armed: fifteen more minutes, then it gives up again.
    mock.timers.tick(15 * 60_000);
    await settle();
    assert.equal(heading(), "This is taking longer than expected", "the clock re-armed");
  }
});

resumeTest("main: the history list is not painted over by the loop of the order left behind", async () => {
  // The buyer sits on a screen that waits, its whole purpose, and opens the
  // menu. If the loop behind it is still running, the next thing the invoice does replaces the
  // list with an order screen, while the URL still says `#/codes` and Back goes somewhere else.
  page.respondWith({ status: 200, body: { status: "open", badgeType: "supporter", months: 1, clientSecret: "cs_test_abc" } });
  history.pushState(null, "", "?order=inv_card");
  page.fire("popstate");
  await until(() => heading() === "Payment received", "the confirming screen, with its deadline armed");

  page.chrome.all("button.menu-button")[0]!.click();
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Codes on this device")!.click();
  await until(() => heading() === "Codes on this device", "the history list");

  mock.timers.tick(15 * 60_000);
  await settle();
  assert.equal(heading(), "Codes on this device",
    "the give-up of the order behind it must not take the screen the buyer asked for");
  assert.equal(location.hash, "#/codes", "and what is on screen is what the URL says");
  // the whole URL, not just its hash: a bare `#/codes` keeps the `?order=` it was opened from,
  // and `syncFromLocation` reads the query first, so Forward or a reload would leave the list
  assert.equal(location.search, "", "the order this was opened from is off the URL");
  history.back();
  await settle();
});

resumeTest("main: opening another order never repaints the one it replaced", async () => {
  // `lastView` is what a connectivity event repaints. Left on the order behind it, a network
  // blip while the new order is still loading draws the old address and the old amount under
  // the new order's URL, and on-chain there is no taking that payment back.
  page.respondWith({ status: 200, body: crypto });
  history.pushState(null, "", "?order=inv_open");
  page.fire("popstate");
  await until(() => heading() === "Send 1.482 XMR", "the first order's payment screen");

  // the second order's read is left in flight, and the loading screen exists for that
  history.pushState(null, "", "?order=inv_card");
  page.fire("popstate");
  await settle();
  assert.notEqual(heading(), "Send 1.482 XMR", "the first order's screen is gone");

  page.setOffline(true);
  await settle();
  assert.ok(!screenOf(app).textContent.includes("48HqK2XmVexampleAddress9fRtWc"),
    `no address from the order that was left behind: ${screenOf(app).textContent.slice(0, 120)}`);
  assert.notEqual(heading(), "Send 1.482 XMR", "and not its screen either");
  page.setOffline(false);
  await settle();
});

resumeTest("main: another card order does not inherit the confirmed one's confirming screen", async () => {
  // The mirror of the test above, and the defect it used to hide: with the flag
  // on the page-global session, any card order opened after a confirm rendered
  // "Waiting for the card network to confirm", for an order nobody had
  // confirmed, and the buyer had no way to pay it.
  page.respondWith({ status: 200, body: { status: "open", badgeType: "supporter", months: 1, clientSecret: "cs_test_other" } });
  history.pushState(null, "", "?order=inv_card_other");
  page.fire("popstate");
  await until(() => heading() === "Pay by card", `the card form, not ${heading()}`);
  assert.ok(!screenOf(app).textContent.includes("Waiting for the card network to confirm."));
  const stored = JSON.parse(page.storage.getItem("sxb.orders.v1")!) as Array<Record<string, unknown>>;
  assert.equal(stored.find((o) => o.orderId === "inv_card_other")!.submitted, undefined,
    "and reading it wrote no flag of its own");
  assert.equal(stored.find((o) => o.orderId === "inv_card")!.submitted, true,
    "while the order that WAS confirmed still carries it");
});

resumeTest("main: nothing is left running once the page has moved on", async () => {
  history.pushState(null, "", "/");
  page.fire("popstate");
  await settle();
  assert.equal(location.search, "");
  const before = fetches.length;
  await settle(10);
  assert.equal(fetches.length, before, "no loop is still issuing requests");
  for (const f of fetches.filter((x) => x.url.includes("?wait="))) {
    assert.equal(f.init!.signal!.aborted, true, "every waiting request has been aborted");
  }
});

resumeTest("main: [ Forget everything ] leaves nothing that restores the order", async () => {
  // The wipe is only as good as the loops it stops: a live watch saves the record it is watching on every
  // 200, so one left running would put the order straight back into the store just emptied. The test above
  // has already abandoned this page's holds, so what is pinned here is the outcome, not the abort it owns.
  assert.ok(storage.getItem("sxb.orders.v1") !== null, "there is an order to forget");
  page.confirmAnswer(true);
  const before = fetches.length;

  const forget = page.chrome.all("button.menu-item").find((b) => b.textContent === "Forget everything on this device");
  assert.ok(forget, "the menu carries the action");
  forget.click();

  assert.equal(storage.getItem("sxb.orders.v1"), null);
  assert.equal(storage.getItem("sxb.session.v1"), null, "the draft goes with the codes");
  assert.equal(heading(), "Support SimpleX", "the landing page is the only screen still true once nothing is stored");
  await settle(10);
  assert.equal(storage.getItem("sxb.orders.v1"), null, "and nothing wrote it back");
  assert.equal(fetches.length, before, "no loop survived to ask again");
});

resumeTest("main: an answer already on the wire is dropped once the store is forgotten", async () => {
  // The history list refreshes every stale order it holds, and those answers are writes. One
  // still in flight when the buyer wipes the store would put a forgotten order straight back,
  // which is exactly what the confirm promises will not happen.
  storage.setItem("sxb.orders.v1", JSON.stringify([{
    orderId: "inv_late", badgeType: "supporter", months: 1,
    createdAt: new Date(NOW - 60_000).toISOString(), status: "open",
  }]));
  page.respondWith({ status: 200, body: { status: "expired", badgeType: "supporter", months: 1 } });
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Codes on this device")!.click();

  // no settle: the read is on the wire, and this is the wipe landing while it is
  page.confirmAnswer(true);
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Forget everything on this device")!.click();
  assert.equal(storage.getItem("sxb.orders.v1"), null, "the wipe itself is immediate");

  await settle(10);
  assert.equal(storage.getItem("sxb.orders.v1"), null, "and the answer that arrived after it is dropped");
});

// The faked clock is released last, so nothing outlives the file.
after(() => { mock.timers.reset(); });
