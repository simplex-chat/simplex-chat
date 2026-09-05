// The cheapest purchase, end to end: the duration list's "1 month" card, and what pressing Pay
// on it sends.

import assert from "node:assert/strict";
import { headingOf, installPage, inViewOf, timedTest, until } from "./boot.js";
import type { StubElement } from "./stub-dom.js";

const monthTest = timedTest(2000);

const page = installPage();
const { app, storage, fetches } = page;
await import("../src/main.js");

const SESSION_KEY = "sxb.session.v1";

const inView = (): StubElement => inViewOf(app);
const heading = headingOf;
function continueButton(p: StubElement): StubElement {
  const go = p.all("button.primary")[0];
  assert.ok(go, "the panel must carry a primary button");
  return go;
}
function choice(p: StubElement, startsWith: string): StubElement {
  const found = p.all("button.choice").find((c) => c.textContent.startsWith(startsWith));
  assert.ok(found, `no choice card starting with ${startsWith}: ${p.all("button.choice").map((c) => c.textContent).join(" | ")}`);
  return found;
}
function session(): Record<string, unknown> {
  return JSON.parse(storage.getItem(SESSION_KEY) ?? "{}") as Record<string, unknown>;
}

/** the landing screen → the tier list → the level named, → the duration list. */
function walkToDurations(level: string): StubElement {
  while (heading(inView()) !== "Support SimpleX") inView().all("button.back")[0]!.click();
  continueButton(inView()).click();
  assert.equal(heading(inView()), "Choose your level");
  choice(inView(), level).click();
  continueButton(inView()).click();
  assert.equal(heading(inView()), "How long?");
  return inView();
}

// ------------------------------------------------------------------ selection

monthTest("main: choosing 1 month marks that card and ENABLES Continue", () => {
  const durations = walkToDurations("Supporter");
  const one = choice(durations, "1 month");
  assert.deepEqual(
    durations.all("button.choice").map((c) => c.getAttribute("aria-pressed")),
    ["false", "false", "false"],
    "nothing is chosen before the click",
  );
  assert.ok(continueButton(durations).hasAttribute("disabled"), "and Continue waits for an answer");

  one.click();
  const after = inView();
  assert.equal(choice(after, "1 month").getAttribute("aria-pressed"), "true",
    "the cheapest card must mark itself chosen — the empty string could never do that");
  assert.deepEqual(
    after.all("button.choice").filter((c) => c.getAttribute("aria-pressed") === "true").length, 1,
    "and it is the only one chosen",
  );
  assert.equal(continueButton(after).hasAttribute("disabled"), false,
    "Continue must enable: a buyer who wants one month was otherwise stuck on the duration list with a dead button");
  assert.notEqual(session().offerId, "", "the stored answer is a key of its own, not the empty string");
});

// ----------------------------------------------------------------- clearing

monthTest("main: changing the level clears the duration and disables Continue again", () => {
  // The duration was priced under the old level, so it cannot survive it. The
  // clear is an explicit `offerId: undefined` through `saveSession`, which
  // merges and drops the field on stringify, not an empty string, which is
  // what made "chosen" and "unchosen" the same value in the first place.
  const durations = walkToDurations("Legend");
  choice(durations, "12 months").click();
  assert.equal(continueButton(inView()).hasAttribute("disabled"), false);
  assert.equal(session().offerId, "offer_12m");

  while (heading(inView()) !== "Choose your level") inView().all("button.back")[0]!.click();
  choice(inView(), "Supporter").click();
  assert.equal("offerId" in session(), false,
    `the stored session must have no offerId at all: ${JSON.stringify(session())}`);

  continueButton(inView()).click();
  const fresh = inView();
  assert.equal(heading(fresh), "How long?");
  assert.deepEqual(fresh.all("button.choice").map((c) => c.getAttribute("aria-pressed")),
    ["false", "false", "false"], "nothing carries over from the level that was abandoned");
  assert.ok(continueButton(fresh).hasAttribute("disabled"), "and the question is asked again");
});

// ------------------------------------------------------------------ upgrades

monthTest("main: a session stored by an older build as offerId \"\" loads as nothing chosen", () => {
  // That build wrote the empty string for both readings, so it cannot be told
  // which one it meant. Reading it as "chosen" is the reading that strands the
  // buyer on a Continue that never enables; reading it as "not answered" only
  // asks the question again.
  storage.setItem(SESSION_KEY, JSON.stringify({ step: "months", priceId: "price_legend", offerId: "" }));
  while (heading(inView()) !== "Support SimpleX") inView().all("button.back")[0]!.click();
  continueButton(inView()).click();                 // the tier list, rebuilt from the seeded session
  continueButton(inView()).click();                 // the duration list, likewise
  const durations = inView();
  assert.equal(heading(durations), "How long?");
  assert.deepEqual(durations.all("button.choice").map((c) => c.getAttribute("aria-pressed")),
    ["false", "false", "false"], "the legacy value marks no card");
  assert.ok(continueButton(durations).hasAttribute("disabled"));

  // And the question can be answered: nobody is stuck after the upgrade.
  choice(durations, "1 month").click();
  assert.equal(continueButton(inView()).hasAttribute("disabled"), false);
});

// ---------------------------------------------------------------- the charge

// Last, because paying takes the wizard off the root and there is no walking
// back to it from a payment screen.
monthTest("main: paying for one month sends NO offerId, and one month's price", async () => {
  // The sentinel is a key in this browser and names no row in `CATALOG.offers`:
  // the create endpoint makes `offerId` optional exactly so the unoffered month goes without
  // one, and sending the sentinel would come back `catalog_changed`.
  const durations = walkToDurations("Supporter");
  choice(durations, "1 month").click();
  continueButton(inView()).click();
  const orderSummary = inView();
  assert.equal(heading(orderSummary), "Check your order");
  assert.ok(orderSummary.textContent.includes("1 month"), `the order summary must summarise one month: ${orderSummary.textContent}`);
  assert.ok(orderSummary.textContent.includes("$7.00"),
    "one supporter month is 700 minor units, which is what the buyer is shown and charged");

  const before = fetches.length;
  page.respondWith({
    status: 200,
    body: {
      invoiceId: "inv_1m", badgeType: "supporter", months: 1,
      amount: 700, currency: "usd", expiresAt: "2026-08-28T13:00:00Z",
      address: "48HqK2Xm", cryptoAmount: "0.02", cryptoCurrency: "xmr",
    },
  });
  orderSummary.all("button.primary")[0]!.click();
  await until(() => fetches.length > before, "the invoice request");
  const sent = fetches.find((f) => f.url.includes("/api/invoice"))!;
  const body = JSON.parse(String(sent.init!.body)) as Record<string, unknown>;
  assert.equal(body.priceId, "price_supporter");
  assert.equal("offerId" in body, false,
    `a one-month order carries no offerId at all, and got: ${JSON.stringify(body)}`);
  assert.equal(body.method, "xmr");
});
