import { mock } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { mediaFor, ruleFor, sheet } from "./css.js";
import { headingOf, installPage, inViewOf, primaryOf, screenOf, settle, timedTest, until } from "./boot.js";
import type { StubElement } from "./stub-dom.js";

const mainTest = timedTest(2000);

// `main.ts` runs on import: it resolves the load and renders. The page has to
// be in place first. This boot is the first-visit case: `/`, empty storage.
const page = installPage();
const { app, history, location, storage, fetches, confirms } = page;
await import("../src/main.js");

function panels(): StubElement[] { return app.all("section.panel"); }
function track(): StubElement { return app.firstChild as StubElement; }
const inView = (): StubElement => inViewOf(app);
function inertFlags(): boolean[] { return panels().map((p) => p.hasAttribute("inert")); }
/** The rail is what travels; the track is what clips it. */
function rail(): StubElement { return track().all("div.rail")[0]!; }
function railStyle(): string { return rail().getAttribute("style") ?? ""; }
/** One of the three device-wide controls in the header's menu. */
function menuItem(label: string): StubElement {
  const found = page.chrome.all("button.menu-item").find((b) => b.textContent === label);
  assert.ok(found, `the menu has no "${label}"`);
  return found;
}
const heading = headingOf;
const primary = primaryOf;

// ------------------------------------------------------------------ the track

mainTest("main: a first load renders the landing screen, with the other three panels inert", () => {
  assert.equal(panels().length, 4, "the landing screen to the order summary are four panels of one track");
  assert.equal(track().getAttribute("class"), "track");
  assert.deepEqual(inertFlags(), [false, true, true, true]);
  assert.equal(heading(inView()), "Support SimpleX");
  assert.equal(railStyle().includes("translateX(-0%)"), true, railStyle());
  assert.equal(history.url, "/", "the landing screen carries no hash, so browser Back leaves the site");
});

mainTest("main: the four panels are the landing screen, the tier list, the duration list and the order summary in order, built once", () => {
  assert.deepEqual(panels().map(heading), ["Support SimpleX", "Choose your level", "How long?", "Check your order"]);
});

mainTest("main: prefers-reduced-motion: reduce JUMPS instead of scrolling", () => {
  // An explicit ScrollOptions.behavior overrides the element's computed
  // scroll-behavior (CSSOM-View), so the CSS rule alone would never apply on
  // the only path that scrolls: the query has to be read in JS as well.
  page.reducedMotion(true);
  try {
    primary(inView())!.click();
    assert.equal(heading(inView()), "Choose your level");
    assert.ok(railStyle().includes("translateX(-100%)"), `it still moves: ${railStyle()}`);
    assert.ok(railStyle().includes("--slide:0ms"), `but it does not animate: ${railStyle()}`);
    inView().all("button.back")[0]!.click();
    assert.equal(heading(inView()), "Support SimpleX");
    assert.ok(railStyle().includes("--slide:0ms"), railStyle());
  } finally {
    page.reducedMotion(false);
  }
});

mainTest("main: Continue scrolls right by one panel and pushes a history entry", () => {
  primary(inView())!.click();
  assert.equal(heading(inView()), "Choose your level");
  assert.deepEqual(inertFlags(), [true, false, true, true]);
  // One column to the left, which puts panel 1 in the window and panel 0 on its
  // way out: both on stage, travelling together, for the length of the slide.
  assert.ok(railStyle().includes("translateX(-100%)"), railStyle());
  assert.ok(railStyle().includes(`--slide:320ms`), railStyle());
  assert.equal(history.url, "/#/tier", "each wizard step is its own history entry");
  assert.equal(history.stack.length, 2);
});

mainTest("main: [ ← Back ] is history.back(), and scrolls the track left again", () => {
  const back = inView().all("button.back")[0]!;
  assert.equal(back.textContent, "← Back");
  back.click();
  assert.equal(heading(inView()), "Support SimpleX");
  assert.deepEqual(inertFlags(), [false, true, true, true]);
  assert.ok(railStyle().includes("translateX(-0%)"), `Back travels left: ${railStyle()}`);
  assert.ok(railStyle().includes("--slide:320ms"), "and it animates, as Continue does");
  assert.equal(history.url, "/");
  assert.equal(history.at, 0);
});

mainTest("main: browser Back on the landing screen leaves the site rather than moving the track", () => {
  assert.equal(history.at, 0);
  history.back();
  assert.equal(history.left, true);
  assert.equal(heading(inView()), "Support SimpleX");
  history.left = false;
});

mainTest("main: the wizard walks the landing screen to the order summary, with the catalog's own figures", () => {
  primary(inView())!.click();                         // the landing screen → the tier list
  assert.equal(heading(inView()), "Choose your level");
  const legend = inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!;
  assert.ok(legend.textContent.includes("$70 / month"), "the tier list's price comes from the compiled-in catalog");
  legend.click();
  primary(inView())!.click();                         // the tier list → the duration list
  assert.equal(heading(inView()), "How long?");
  const twelve = inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!;
  assert.ok(twelve.textContent.includes("$420"), "12 legend months are $420");
  assert.ok(twelve.textContent.includes("save 50%"), "the saving is the only figure the browser computes");
  twelve.click();
  primary(inView())!.click();                         // the duration list → the order summary
  assert.equal(heading(inView()), "Check your order");
  assert.ok(inView().textContent.includes("$420.00"));
  assert.ok(railStyle().includes("translateX(-300%)"), railStyle());
  assert.deepEqual(inertFlags(), [true, true, true, false]);
  assert.equal(history.url, "/#/checkout");
  assert.equal(history.stack.length, 4);
});

mainTest("main: the answers are in the store, and never in the URL", () => {
  const session = JSON.parse(storage.getItem("sxb.session.v1")!) as Record<string, unknown>;
  assert.equal(session.priceId, "price_legend");
  assert.equal(session.offerId, "offer_12m");
  assert.equal(history.url, "/#/checkout", "a URL passed to someone else transfers no selection");
});

mainTest("main: a 429 lands on the rate-limited screen, counts down, and re-enables Pay at zero", async () => {
  mock.timers.enable({ apis: ["setInterval"] });
  try {
    page.respondWith({ status: 429, body: { error: "rate_limited" }, headers: { "retry-after": "46" } });
    const before = fetches.length;
    inView().all("button.primary")[0]!.click();
    await until(() => heading(inView()) === "Too many attempts", "the rate-limited screen");
    assert.equal(fetches.length, before + 1);

    const rateLimited = inView();
    assert.equal(heading(rateLimited), "Too many attempts");
    assert.ok(rateLimited.textContent.includes("Try again in 46 seconds"));
    const disabled = rateLimited.all("button.primary")[0]!;
    assert.ok(disabled.hasAttribute("disabled"), "the Pay button is disabled for exactly Retry-After");

    mock.timers.tick(1000);
    assert.ok(inView().textContent.includes("Try again in 45 seconds"), inView().textContent);
    mock.timers.tick(44_000);
    assert.ok(inView().textContent.includes("Try again in 1 second"), inView().textContent);
    assert.equal(heading(inView()), "Too many attempts", "still the rate-limited screen one second short");

    mock.timers.tick(1000);
    assert.equal(heading(inView()), "Check your order", "at zero the buyer is back on the order summary");
    assert.ok(primary(inView()), "with an enabled Pay button");

    // And the interval is gone: further ticks must not rebuild anything.
    const rebuilt = inView();
    mock.timers.tick(60_000);
    assert.equal(inView(), rebuilt, "the interval was cleared, not left running");
  } finally {
    mock.timers.reset();
  }
});

mainTest("main: a second press of Pay creates no second invoice", async () => {
  page.respondWith({ status: 500, body: { error: "internal" } });
  const before = fetches.length;
  const pay = inView().all("button.primary")[0]!;
  pay.click();
  pay.click();
  pay.click();
  await until(() => heading(screenOf(app)) === "That did not go through", "the failure screen");
  // Two presses would create two invoices and draw two codes, one of them dead
  // weight in a fifty-entry store that never evicts an entry holding a code.
  assert.equal(fetches.length, before + 1, "exactly one POST");
  assert.ok(pay.hasAttribute("disabled"), "the guard is the disabled attribute, as screens.ts uses");
  assert.equal(heading(screenOf(app)), "That did not go through", "a 500 is the failure screen");

  // [ Try again ] resubmits the same selection.
  page.respondWith({ status: 500, body: { error: "internal" } });
  screenOf(app).all("button.primary")[0]!.click();
  await until(() => fetches.length === before + 2, "the retry's POST");
  await settle();
  assert.equal(heading(screenOf(app)), "That did not go through");

  // Back out of the failure screen so the next test starts on a fresh order summary.
  history.back();
  await settle();
  assert.equal(heading(inView()), "How long?");
  primary(inView())!.click();
  assert.equal(heading(inView()), "Check your order");
});

mainTest("main: the 200 REPLACES the history entry, so Back cannot resubmit", async () => {
  const before = history.stack.length;
  page.respondWith({
    status: 200,
    body: {
      invoiceId: "inv_9f3a", badgeType: "legend", months: 12,
      amount: 42000, currency: "usd", expiresAt: "2126-08-28T13:00:00Z",
      address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr",
    },
  });
  const pay = inView().all("button.primary")[0]!;
  assert.ok(pay.textContent.startsWith("Pay $420.00 with "));
  pay.click();
  await until(() => location.search === "?order=inv_9f3a", "the invoice response");
  await settle();

  assert.equal(history.stack.length, before, "the entry is replaced, not pushed");
  assert.equal(history.url, "/?order=inv_9f3a");
  assert.equal(fetches[0]!.url, "/api/invoice");
  assert.equal(fetches[0]!.init!.method, "POST");

  // The payment screen is a single screen, not a panel of the track.
  assert.equal(app.all("div.track").length, 0);
  const screen = screenOf(app);
  assert.ok(screen.textContent.includes("Send 1.482 XMR"), screen.textContent);
  assert.ok(screen.textContent.includes("48HqK2XmVexampleAddress9fRtWc"));
  assert.ok(screen.textContent.includes("inv_9f3a"));

  // the store rules: the code was drawn and stored before this screen, and is not on it.
  const stored = JSON.parse(storage.getItem("sxb.orders.v1")!) as Array<Record<string, string>>;
  assert.equal(stored.length, 1);
  assert.equal(stored[0]!.orderId, "inv_9f3a");
  assert.ok(stored[0]!.code!.startsWith("SXB-"));
  assert.ok(!screen.textContent.includes(stored[0]!.code!), "the payment screen must never show the code");
  assert.ok(!JSON.stringify(fetches[0]!.init!.body).includes(stored[0]!.code!.replace(/-/g, "").replace(/^SXB/, "")),
    "the plaintext code never leaves the browser — only its hash");

  // the store rules: the 200 clears the session; the draft became an invoice.
  assert.equal(storage.getItem("sxb.session.v1"), null);
});

mainTest("main: the waiting loop holds, and the hidden tab releases it", async () => {
  const waits = fetches.filter((f) => f.url.includes("?wait="));
  assert.equal(waits.length, 1, "exactly one loop, holding once");
  assert.equal(waits[0]!.url, "/api/invoice/inv_9f3a?wait=open&seenPaid=&seenFull=0");
  const signal = waits[0]!.init!.signal!;
  assert.equal(signal.aborted, false);

  page.document.hidden = true;
  page.document.dispatch("visibilitychange");
  await settle();
  assert.equal(signal.aborted, true, "a hidden tab holds no connection");

  page.document.hidden = false;
  page.document.dispatch("visibilitychange");
  await settle();
  const after = fetches.filter((f) => f.url.includes("?wait="));
  assert.equal(after.length, 2, "resume reissues exactly one request, not two");
  assert.equal(after[1]!.init!.signal!.aborted, false);
});

mainTest("main: Back from a payment screen RETURNS TO THE WIZARD", async () => {
  // the resume row would send the buyer straight back to the payment screen:
  // the search is empty again, and an open order exists, so resume applies
  // to a fresh load only and a history navigation goes by the URL.
  assert.ok(storage.getItem("sxb.orders.v1")!.includes('"status":"open"'), "an open order exists to be resumed");
  const held = fetches.filter((f) => f.url.includes("?wait=")).at(-1)!.init!.signal!;
  assert.equal(held.aborted, false);

  history.back();
  await settle();
  assert.equal(location.search, "", "the ?order= entry is behind us");
  assert.equal(heading(inView()), "How long?", "Back lands on the wizard, not back on the payment screen");
  assert.equal(app.all("div.track").length, 1, "the track is on screen again");

  // And the loop that was watching the order is stopped, so it cannot repaint
  // over the screen the buyer just navigated to.
  assert.equal(held.aborted, true, "navigating away must stop the loop");
  const before = fetches.length;
  await settle();
  assert.equal(fetches.length, before, "and it must not reissue");
});

mainTest("main: Back after a purchase reaches a usable duration list, not an empty one", () => {
  // the session is cleared on the 200, so the duration list has no session to rebuild from.
  // Back must return to the duration list with the duration still chosen, and
  // the order record carries badgeType and months, the same pair.
  assert.equal(storage.getItem("sxb.session.v1"), null, "the session really is gone");
  const durations = inView();
  assert.equal(heading(durations), "How long?");
  const choices = durations.all("button.choice");
  assert.equal(choices.length, 3, "the duration list must be priced, not empty");
  const twelve = choices.find((c) => c.textContent.startsWith("12 months"))!;
  assert.equal(twelve.getAttribute("aria-pressed"), "true", "with the duration still chosen");
  assert.ok(primary(durations), "and Continue must not be permanently disabled");

  // All the way back to the landing screen, through a tier list that is also still answered.
  history.back();
  assert.equal(heading(inView()), "Choose your level");
  const legend = inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!;
  assert.equal(legend.getAttribute("aria-pressed"), "true");
  history.back();
  assert.equal(heading(inView()), "Support SimpleX");
  assert.equal(history.at, 0, "and the landing screen is the bottom of the stack");
});

mainTest("main: a bare `/` is the landing screen, so browser Back leaves the site", () => {
  // Falling through to the session step here would put Back on `/` onto the order summary.
  assert.equal(location.hash, "");
  assert.equal(heading(inView()), "Support SimpleX");
  history.back();
  assert.equal(history.left, true, "there is nothing behind the landing screen");
  history.left = false;
  assert.equal(heading(inView()), "Support SimpleX");
});

mainTest("main: the menu opens the history list from the store, with [ Open ] as a link", async () => {
  // The history is reachable from every screen now, and from the header rather
  // than from a line at the foot of whichever panel happened to carry one.
  assert.equal(heading(inView()), "Support SimpleX");
  assert.equal(inView().all("button.link").length, 0, "the landing page carries no navigation of its own");
  menuItem("Codes on this device").click();
  await until(() => app.all("li.entry").length > 0, "the history list's list");

  assert.equal(location.hash, "#/codes", "pushState must update location synchronously");
  const codes = screenOf(app);
  assert.equal(heading(codes), "Codes on this device");
  const rows = codes.all("li.entry");
  assert.equal(rows.length, 1);
  assert.ok(rows[0]!.textContent.includes("waiting for payment"), rows[0]!.textContent);
  const open = rows[0]!.all("a.secondary")[0]!;
  assert.equal(open.textContent, "Open");
  assert.equal(open.getAttribute("href"), "?order=inv_9f3a");
  // the fallback store is an in-memory Map, so the plain click must not be
  // a full navigation, since it would take every stored code with it.
  const plain = open.click();
  assert.equal(plain.defaultPrevented, true, "the plain click is handled in-document");
  await settle();
  assert.equal(location.search, "?order=inv_9f3a");
  assert.equal(storage.getItem("sxb.orders.v1") !== null, true, "and the store survives");
  history.back();
  await settle();
  assert.equal(location.hash, "#/codes");
  // the store rules: an open entry never shows its code, in text or in any attribute.
  const stored = (JSON.parse(storage.getItem("sxb.orders.v1")!) as Array<{ code: string }>)[0]!.code;
  assert.ok(stored.startsWith("SXB-"));
  assert.ok(!codes.serialize().includes(stored), "the history list leaked an unpaid code");
});

mainTest("main: [ Forget everything ] does nothing when the confirmation is refused", () => {
  const before = storage.getItem("sxb.orders.v1");
  assert.ok(before !== null && before.includes("inv_9f3a"));
  confirms.length = 0;
  page.confirmAnswer(false);
  menuItem("Forget everything on this device").click();
  assert.equal(confirms.length, 1, "the one irreversible action must ask first");
  assert.equal(storage.getItem("sxb.orders.v1"), before, "a refused confirmation keeps every code");
  assert.equal(app.all("li.entry").length, 1, "and the list stands");
});

mainTest("main: [ Forget everything ] removes the key and returns to the landing page", () => {
  page.confirmAnswer(true);
  menuItem("Forget everything on this device").click();
  assert.equal(storage.getItem("sxb.orders.v1"), null);
  assert.equal(storage.getItem("sxb.session.v1"), null, "the draft goes with the codes");
  // Not the empty history: every screen but the landing screen is drawn from something that has
  // just been deleted. Staying on the code screen would leave a code on screen that the
  // store no longer holds, beside a line calling this browser its only copy.
  assert.equal(history.url, "/", "the URL names the landing page, not the wiped order");
  const landing = screenOf(app);
  assert.ok(landing.textContent.includes("Support SimpleX"), landing.textContent);
  assert.ok(landing.textContent.includes("Choose your level"));
  assert.ok(!landing.textContent.includes("Nothing bought on this device"));
});

mainTest("main: the provider-unavailable screen notice does not outlive the checkout it belongs to", async () => {
  // Cleared only by a successful checkout, "Monero is temporarily unavailable" survived [ New invoice ], a
  // change of level and a return to the landing screen. [ Forget everything ] leaves the page on the landing
  // screen, where this walk starts; it used to land on the empty history, and this test opened by clicking out.
  assert.equal(heading(inView()), "Support SimpleX");
  const walk = (): void => {
    primary(inView())!.click();
    inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))?.click();
    primary(inView())!.click();
    inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))?.click();
    primary(inView())!.click();
  };
  walk();
  assert.equal(heading(inView()), "Check your order");

  page.respondWith({ status: 503, body: { error: "provider_unavailable" } });
  primary(inView())!.click();   // Pay, with Monero selected by default
  await until(() => inView().textContent.includes("temporarily unavailable"), "the provider-unavailable screen");
  assert.ok(inView().textContent.includes("Monero is temporarily unavailable"));
  assert.ok(inView().textContent.includes("Pay $420.00 with Bitcoin"), "the unknown-order screen re-labels Pay to what is available");

  // A change of level is a different order: the notice was about the last one.
  history.back();
  history.back();
  assert.equal(heading(inView()), "Choose your level");
  inView().all("button.choice").find((c) => c.textContent.startsWith("Supporter"))!.click();
  primary(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primary(inView())!.click();
  assert.equal(heading(inView()), "Check your order");
  assert.ok(!inView().textContent.includes("temporarily unavailable"), inView().textContent);
  assert.equal(inView().all("button.choice").filter((c) => c.hasAttribute("disabled")).length, 0,
    "and no method is left disabled by an attempt that is no longer being made");

  // And so is landing back on the landing screen, which is where [ New invoice ] and
  // [ Start again ] both go.
  page.respondWith({ status: 503, body: { error: "provider_unavailable" } });
  primary(inView())!.click();
  await until(() => inView().textContent.includes("temporarily unavailable"), "the provider-unavailable screen again");
  history.back();
  history.back();
  history.back();
  assert.equal(heading(inView()), "Support SimpleX");
  walk();
  assert.equal(heading(inView()), "Check your order");
  assert.ok(!inView().textContent.includes("temporarily unavailable"), inView().textContent);
});

mainTest("main: nothing is left running when the page has been navigated away from", async () => {
  // A leaked interval or a live loop would keep this process alive past the
  // last test and turn a failure into a two-minute hang.
  const before = fetches.length;
  await settle(10);
  assert.equal(fetches.length, before, "no loop is still issuing requests");
  for (const f of fetches.filter((x) => x.url.includes("?wait="))) {
    assert.equal(f.init!.signal!.aborted, true, "every waiting request has been aborted");
  }
});

// ------------------------------------------------------------- the stylesheet

mainTest("styles: colour tokens are on bare :root, by value, and redefined under dark", () => {
  const bare = ruleFor(sheet.rules, ":root");
  assert.ok(bare, "the palette must be defined on bare :root, not only inside a media query");
  for (const token of ["--bg", "--ink", "--muted", "--line", "--accent"]) {
    const value = bare.decls.get(token);
    assert.ok(value !== undefined && value.length > 0, `${token} must be defined on bare :root`);
  }
  // The value, not its presence anywhere in the file: a comment is not a palette.
  assert.equal(bare.decls.get("--accent")!.toUpperCase(), "#3889FF",
    "the accent is the reference's #3889FF, and the same one in both themes");

  const dark = mediaFor("(prefers-color-scheme: dark)");
  assert.ok(dark, "the dark query must exist");
  // Guarded, so the menu's explicit Light beats an operating system set to dark.
  const darkRoot = ruleFor(dark.rules, ':root:not([data-theme="light"])');
  assert.ok(darkRoot, "and must redefine :root for everything but an explicit light");
  for (const token of ["--bg", "--ink", "--muted", "--line"]) {
    const value = darkRoot.decls.get(token);
    assert.ok(value !== undefined, `${token} must be redefined under dark`);
    assert.notEqual(value, bare.decls.get(token), `${token} must actually differ in dark`);
  }
  // And an explicit dark beats an operating system set to light.
  assert.ok(ruleFor(sheet.rules, ':root[data-theme="dark"]'),
    "the menu's Dark needs a rule of its own, or it does nothing on a light machine");
});

mainTest("styles: reduced motion turns both halves of the slide off", () => {
  // The move is a transform on the rail and a height on the track, and either
  // one left animating is still an animation.
  for (const selector of [".track", ".rail"]) {
    const rule = ruleFor(sheet.rules, selector);
    assert.ok(rule, `${selector} must exist`);
    assert.match(rule.decls.get("transition") ?? "", /var\(--slide/,
      `${selector} must take its duration from the token main.ts writes`);
  }
  const reduce = mediaFor("(prefers-reduced-motion: reduce)");
  assert.ok(reduce, "the reduced-motion query must exist");
  // It has to name the elements that actually move; a rule for some other
  // selector satisfies a grep and nothing else.
  const off = reduce.rules.filter((r) => r.decls.get("transition") === "none").flatMap((r) => r.selector);
  for (const selector of [".track", ".rail"]) {
    assert.ok(off.includes(selector),
      `the query must turn ${selector}'s transition off, not ${JSON.stringify(reduce.rules.map((r) => r.selector))}`);
  }
});

mainTest("styles: the track clips, the rail travels, and each panel is one whole column", () => {
  // One element cannot clip and translate at once, which is why there are two.
  // `overflow: hidden` on the clipper is also what makes this a stepper rather
  // than a carousel: there is no scroll to drag, and a panel the buyer has not
  // reached cannot be flung to.
  const track = ruleFor(sheet.rules, ".track")!;
  assert.equal(track.decls.get("overflow"), "hidden");
  const rail = ruleFor(sheet.rules, ".rail")!;
  assert.equal(rail.decls.get("display"), "flex");
  assert.equal(rail.decls.get("width"), "100%");
  const panel = ruleFor(sheet.rules, ".panel");
  assert.ok(panel, ".panel must exist");
  assert.equal(panel.decls.get("flex")?.replace(/\s+/g, " "), "0 0 100%", "each panel is exactly the column's width");
  assert.equal(ruleFor(sheet.rules, "#app")!.decls.get("max-width"), "560px");
  // And no panel is collapsed while it is off screen: an outgoing panel with no
  // height cannot travel, and that is how the old move came to read as a cut.
  assert.equal(ruleFor(sheet.rules, ".panel[inert]"), undefined,
    "an inert panel keeps its height, or the two screens are never on stage together");
});

mainTest("styles: below 560 px it is the same track, with no layout redeclared", () => {
  // EVERY narrow block, not the first one: `sheet.media` holds three, and a
  // test that read only the first proved nothing about the phone band where the
  // scale actually changes.
  const narrow = sheet.media.filter((m) => m.query.replace(/\s+/g, "") === "(max-width:560px)");
  assert.ok(narrow.length > 0, "the narrow query must exist");
  // The track, its panels and the cards inside them must be one layout at every
  // width: only cosmetic properties may vary. The chrome is exempt, and
  // deliberately so: a 320px panel floating inside a 350px column is a sheet.
  const oneLayout = /^\.(track|rail|panel|choices|choice|split|rows|row|entries|entry)\b/;
  const layout = /^(display|position|float|width|height|flex|flex-.*|grid|grid-.*|columns|transform|inset|top|left|right|bottom|order)$/;
  for (const block of narrow) {
    for (const rule of block.rules) {
      if (!oneLayout.test(rule.selector)) continue;
      for (const prop of rule.decls.keys()) {
        assert.ok(!layout.test(prop),
          `${rule.selector} redeclares "${prop}" below 560px — that is a second layout`);
      }
    }
  }
  // And the panel's own width rule is unconditional, so the viewport IS the panel.
  assert.equal(ruleFor(sheet.rules, ".panel")!.decls.get("flex")?.replace(/\s+/g, " "), "0 0 100%");
});

mainTest("styles: on a phone the menu is a sheet rather than a floating panel", () => {
  const narrow = sheet.media.filter((m) => m.query.replace(/\s+/g, "") === "(max-width:560px)");
  const rule = narrow.flatMap((m) => m.rules).find((r) => r.selector === ".menu");
  assert.ok(rule, "the menu must be redrawn below 560px, where 320px does not fit in the column");
  assert.equal(rule.decls.get("position"), "fixed");
  assert.equal(rule.decls.get("width"), "auto", "it spans the page instead of floating in it");
});

mainTest("shell: index.html carries the track and nothing else", () => {
  const html = readFileSync(new URL("../../public/index.html", import.meta.url), "utf8");
  assert.ok(html.includes('<main id="app" aria-live="polite"></main>'));
  assert.ok(/<script type="module" src="\/assets\/[0-9a-f]{16}\/main\.js"><\/script>/.test(html),
    "the offline promise: the entry module is under /assets/<buildHash>/, so the shell and its modules cannot skew");
  assert.ok(html.includes("simplex.chat/contact"), "every screen carries the footer");
  assert.ok(!/<section|<button|<h1/.test(html), "every screen is built in screens.ts");
});

// ------------------------------------------------------------- the chrome

mainTest("main: the chosen theme is written to <html>, and system removes it", () => {
  const html = page.documentElement;
  const segment = (label: string): StubElement => {
    const found = page.chrome.all("button.segment").find((b) => b.textContent === label);
    assert.ok(found, `the theme control has no "${label}"`);
    return found;
  };
  // Nothing chosen yet, so the page is whatever the operating system says.
  assert.equal(html.hasAttribute("data-theme"), false);

  segment("Dark").click();
  assert.equal(html.getAttribute("data-theme"), "dark");
  assert.equal(storage.getItem("sxb.theme.v1"), '"dark"', "and it survives a reload");
  assert.equal(segment("Dark").getAttribute("aria-pressed"), "true");
  assert.equal(segment("System").getAttribute("aria-pressed"), "false");

  segment("Light").click();
  assert.equal(html.getAttribute("data-theme"), "light",
    "an explicit light must beat an operating system set to dark, so the attribute is written");

  segment("System").click();
  assert.equal(html.hasAttribute("data-theme"), false,
    "system is the ABSENCE of the attribute, which hands the page back to the media query");
  assert.equal(storage.getItem("sxb.theme.v1"), '"system"');
});

mainTest("main: [ Forget everything ] does not take the theme with it", () => {
  page.chrome.all("button.segment").find((b) => b.textContent === "Dark")!.click();
  page.confirmAnswer(true);
  menuItem("Forget everything on this device").click();
  assert.equal(storage.getItem("sxb.orders.v1"), null, "the codes go");
  assert.equal(storage.getItem("sxb.theme.v1"), '"dark"',
    "and a colour scheme is not something that control exists to destroy");
  page.chrome.all("button.segment").find((b) => b.textContent === "System")!.click();
});

mainTest("main: an open menu takes the screen behind it out of the tree, and gives it back", () => {
  // The keydown loop keeps the keyboard in; `inert` is what keeps a screen
  // reader's virtual cursor from walking the page behind an open popup.
  const trigger = page.chrome.all("button.menu-button")[0]!;
  assert.equal(app.hasAttribute("inert"), false);
  trigger.click();
  assert.equal(app.hasAttribute("inert"), true, "the screen is inert while the menu is over it");
  trigger.click();
  assert.equal(app.hasAttribute("inert"), false, "and every close gives it back");
  // Including the close an item performs on its way to another screen.
  trigger.click();
  menuItem("Codes on this device").click();
  assert.equal(app.hasAttribute("inert"), false, "or the screen it navigated to would be dead");
  history.back();
});

mainTest("main: Escape closes the menu and hands focus back to the button", () => {
  const trigger = page.chrome.all("button.menu-button")[0]!;
  const menu = page.chrome.all("div.menu")[0]!;
  trigger.click();
  assert.equal(menu.hasAttribute("hidden"), false);
  const before = trigger.focused;
  page.press("Escape");
  assert.equal(menu.hasAttribute("hidden"), true);
  assert.equal(trigger.focused, before + 1, "the keyboard must not be left on a panel that is gone");
});

mainTest("main: Tab keeps the keyboard inside an open menu, and wraps at both ends", () => {
  const trigger = page.chrome.all("button.menu-button")[0]!;
  const items = page.chrome.all("button.segment").concat(page.chrome.all("button.menu-item"));
  trigger.click();
  // Nothing inside the menu holds focus yet, so Tab enters at the first item.
  page.press("Tab");
  assert.equal(document.activeElement, items[0]);
  // ...and Shift+Tab off the front wraps to the last rather than leaving.
  page.press("Tab", { shiftKey: true });
  assert.equal(document.activeElement, items[items.length - 1]);
  // Off the end it wraps forward again.
  page.press("Tab");
  assert.equal(document.activeElement, items[0]);
  page.press("Escape");
});

mainTest("main: a key press with the menu closed reaches nothing", () => {
  const menu = page.chrome.all("div.menu")[0]!;
  assert.equal(menu.hasAttribute("hidden"), true);
  const before = page.chrome.all("button.menu-button")[0]!.focused;
  page.press("Escape");
  page.press("Tab");
  assert.equal(page.chrome.all("button.menu-button")[0]!.focused, before, "nothing is stolen from the screen");
});

mainTest("main: the menu carries no order, on a page that holds one", () => {
  // The guard that covers every screen covers the chrome too: it is fixed
  // labels over callbacks, and there is no path by which an order reaches it.
  assert.equal(/SXB-|inv_|order=/.test(page.chrome.serialize()), false, page.chrome.serialize());
});

// ------------------------------------------------- the worker's registration

// The offline promise and Anubis: Anubis serves its challenge as HTML at the same path as the page, so a
// worker registered before the real shell is on screen could precache one as the shell, permanently, for
// this visitor. The order is what is asserted: `#app` already held a rendered panel when `register` was
// called. Registering earlier, or on any load that rendered nothing, turns these red.

mainTest("main: the worker is registered once, and only after the shell rendered", () => {
  assert.deepEqual(page.workers.registrations.map((r) => r.url), ["/sw.js"],
    "one registration, of the worker at the origin's root, which is its scope");
  assert.ok(page.workers.registrations[0]!.appChildren > 0,
    "#app was empty when register was called: that load is not evidence of a real shell");
});

mainTest("main: what was on screen when it registered was this build's own track", () => {
  // The stronger form of the same rule, and the one a challenge page fails: the
  // node in #app at that moment is the track this module built, so the document
  // that ran it was ours and not something served in its place.
  assert.equal(page.workers.registrations[0]!.shell, "div.track");
});
