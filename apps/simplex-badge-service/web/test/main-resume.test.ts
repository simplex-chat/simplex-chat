import { after, mock } from "node:test";
import assert from "node:assert/strict";
import { forgetControl, headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { MemStorage, StubElement } from "./stub-dom.js";

const resumeTest = timedTest(3000);

const NOW = Date.parse("2026-08-28T12:00:00Z");
const CREATED = new Date(NOW - 14 * 60_000).toISOString();
const HELD_CODE = "SB-YDC8A-YGQTM-PUYZ9-2TUXP";

const storage = new MemStorage();
storage.setItem("sb.orders.v1", JSON.stringify([{
  orderId: "inv_open", badgeType: "legend", months: 12,
  createdAt: CREATED, status: "open", code: HELD_CODE,
}, {
  // This order carries submitted from a confirmed card checkout and is older than inv_open, which keeps "resume the newest open order" meaningful.
  orderId: "inv_card", badgeType: "supporter", months: 1,
  createdAt: new Date(NOW - 40 * 60_000).toISOString(), status: "open", submitted: true,
}]));

// setImmediate is deliberately not mocked because settle drains it.
mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage });
const { app, history, location, fetches } = page;

const crypto = {
  status: "open", badgeType: "legend", months: 12, amount: 42000,
  currency: "usd", expiresAt: "2026-08-28T12:58:12Z",
  address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr",
};

// The publishable key makes a non-submitted card order render the real Payment Element form, not the "card unavailable" screen.
const keyMeta = new StubElement("meta");
keyMeta.setAttribute("id", "stripe-publishable-key");
keyMeta.setAttribute("content", "pk_test_resume");
page.document.byId.set("stripe-publishable-key", keyMeta);
(globalThis as unknown as { window: Record<string, unknown> }).window.Stripe = () => ({
  elements: () => ({ create: () => ({ mount: () => {}, destroy: () => {} }) }),
  confirmPayment: async () => ({ paymentIntent: { status: "succeeded" } }),
});

// main.ts issues the first read during import, so its answer is queued before the module runs.
page.respondWith({ status: 200, body: crypto });
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));

// ------------------------------------------------------------------- the resume

resumeTest("main: a FRESH LOAD at / resumes the newest open order", async () => {
  await until(() => heading().startsWith("Send"), "awaitingPayment");
  assert.equal(heading(), "Send 1.482 XMR");
  assert.ok(screenOf(app).textContent.includes("48HqK2XmVexampleAddress9fRtWc"));
  assert.ok(screenOf(app).textContent.includes("inv_open"));
  assert.equal(fetches[0]!.url, "/api/invoice/inv_open", "the status is fetched first");
  assert.ok(!fetches[0]!.url.includes("wait="), "plainly, so the address appears at once");
});

resumeTest("main: a resumed payment screen says how long ago it started, with no Buy a new code", () => {
  assert.ok(screenOf(app).textContent.includes("Started 14 minutes ago."), screenOf(app).textContent);
  assert.equal(screenOf(app).all("button").filter((b) => b.textContent === "Buy a new code").length, 0,
    "the payment screen offers no Buy a new code");
});

resumeTest("main: the resumed screen still never shows the code it holds", () => {
  assert.ok(page.storage.getItem("sb.orders.v1")!.includes(HELD_CODE), "the code is in the store");
  assert.ok(!screenOf(app).serialize().includes(HELD_CODE), "and nowhere on the unpaid screen");
  assert.ok(!screenOf(app).serialize().includes("SB-"));
});

// ------------------------------------------- detailsUnavailable, end to end

resumeTest("main: [ Check again ] on detailsUnavailable RE-RENDERS, and never blanks", async () => {
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

  const out = screenOf(app).all("button.secondary").find((b) => b.textContent === "Buy a new code");
  assert.ok(out, "this screen's order is open with nothing paid, so it may offer one");
});

// ----------------------------- the confirming screen and its give-up, end to end

resumeTest("main: the confirming screen gives up after fifteen minutes and [ Check again ] restarts it", async () => {
  // The session is removed first because submitted lives on the order record, so the no-second-charge rule must hold without the session.
  page.storage.removeItem("sb.session.v1");
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

    // The create endpoint has no idempotency key and confirm() already succeeded, so no control here may start a second charge.
    assert.equal(screenOf(app).all("button").filter((b) => b.textContent === "Buy a new code").length, 0,
      "the give-up screen must not offer [ Buy a new code ]");

    const before = fetches.length;
    page.respondWith({ status: 200, body: { status: "open", badgeType: "supporter", months: 1, clientSecret: "cs_test_abc" } });
    screenOf(app).all("button.primary").find((b) => b.textContent === "Check again")!.click();
    await until(() => heading() === "Payment received", "the confirming screen again");
    assert.equal(fetches[before]!.url, "/api/invoice/inv_card", "a plain read, not a hold");
    assert.deepEqual(fetches.slice(before).map((f) => f.url),
      ["/api/invoice/inv_card", "/api/invoice/inv_card?wait=open&seenPaid=&seenFull=0"]);

    mock.timers.tick(15 * 60_000);
    await settle();
    assert.equal(heading(), "This is taking longer than expected", "the clock re-armed");
  }
});

resumeTest("main: the history list is not painted over by the loop of the order left behind", async () => {
  // If the loop behind the list is still running, its next answer repaints an order screen over the list while the URL still says #/codes.
  page.respondWith({ status: 200, body: { status: "open", badgeType: "supporter", months: 1, clientSecret: "cs_test_abc" } });
  history.pushState(null, "", "?order=inv_card");
  page.fire("popstate");
  await until(() => heading() === "Payment received", "the confirming screen, with its deadline armed");

  page.chrome.all("button.menu-button")[0]!.click();
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();
  await until(() => heading() === "Your codes", "the history list");

  mock.timers.tick(15 * 60_000);
  await settle();
  assert.equal(heading(), "Your codes",
    "the give-up of the order behind it must not take the screen the buyer asked for");
  assert.equal(location.hash, "#/codes", "and what is on screen is what the URL says");
  // syncFromLocation reads the query before the hash, so the ?order= must be off the URL or Forward or a reload would leave the list.
  assert.equal(location.search, "", "the order this was opened from is off the URL");
  history.back();
  await settle();
});

resumeTest("main: opening another order never repaints the one it replaced", async () => {
  // A connectivity event repaints lastView, so if it still points at the order left behind a network blip draws the old address under the new order's URL.
  page.respondWith({ status: 200, body: crypto });
  history.pushState(null, "", "?order=inv_open");
  page.fire("popstate");
  await until(() => heading() === "Send 1.482 XMR", "the first order's payment screen");

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
  page.respondWith({ status: 200, body: { status: "open", badgeType: "supporter", months: 1, clientSecret: "cs_test_other" } });
  history.pushState(null, "", "?order=inv_card_other");
  page.fire("popstate");
  await until(() => heading() === "Pay by card", `the card form, not ${heading()}`);
  assert.ok(!screenOf(app).textContent.includes("Waiting for the card network to confirm."));
  const stored = JSON.parse(page.storage.getItem("sb.orders.v1")!) as Array<Record<string, unknown>>;
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
  // A live watch saves its record on every 200, so a loop left running would write the forgotten order straight back into the emptied store.
  assert.ok(storage.getItem("sb.orders.v1") !== null, "there is an order to forget");
  page.confirmAnswer(true);

  // Reaching the wipe control navigates to the codes list, which issues its own refresh, so the fetch count is pinned after the list is up.
  const forget = forgetControl(page);
  assert.ok(forget, "the codes list carries the wipe control");
  await settle(10);
  const before = fetches.length;
  forget.click();

  assert.equal(storage.getItem("sb.orders.v1"), null);
  assert.equal(storage.getItem("sb.session.v1"), null, "the draft goes with the codes");
  assert.equal(heading(), "Support SimpleX", "the landing page is the only screen still true once nothing is stored");
  await settle(10);
  assert.equal(storage.getItem("sb.orders.v1"), null, "and nothing wrote it back");
  assert.equal(fetches.length, before, "no loop survived to ask again");
});

resumeTest("main: an answer already on the wire is dropped once the store is forgotten", async () => {
  // The history list refreshes stale orders, and one answer still in flight when the store is wiped would write a forgotten order back.
  storage.setItem("sb.orders.v1", JSON.stringify([{
    orderId: "inv_late", badgeType: "supporter", months: 1,
    createdAt: new Date(NOW - 60_000).toISOString(), status: "open",
  }]));
  page.respondWith({ status: 200, body: { status: "expired", badgeType: "supporter", months: 1 } });
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();

  // There is no settle here, because the read is on the wire while the wipe lands.
  page.confirmAnswer(true);
  forgetControl(page)!.click();
  assert.equal(storage.getItem("sb.orders.v1"), null, "the wipe itself is immediate");

  await settle(10);
  assert.equal(storage.getItem("sb.orders.v1"), null, "and the answer that arrived after it is dropped");
});

// The faked clock is released last, so nothing outlives the file.
after(() => { mock.timers.reset(); });
