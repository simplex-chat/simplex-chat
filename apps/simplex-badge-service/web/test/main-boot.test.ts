// The boot URL `main.test.ts` cannot reach: `main.ts` runs on import and a process gets one boot, so a second
// boot URL is a second file. Until this one existed, no test drove a boot at anything but `/`. The caveats in
// `stub-dom.ts` apply here too: no layout, no CSS, no focus.

import assert from "node:assert/strict";
import { headingOf, installPage, inViewOf, primaryOf, settle, timedTest } from "./boot.js";
import type { StubElement } from "./stub-dom.js";

const bootTest = timedTest(3000);

// A link, a bookmark or a restored tab, at the last step of a wizard whose
// questions this browser has never answered: no session, no orders.
const page = installPage({ url: "/#/checkout" });
const { app, location, history, storage, fetches } = page;
await import("../src/main.js");

const inView = (): StubElement => inViewOf(app);
const heading = (): string => headingOf(inView());

bootTest("main: booting at #/checkout with nothing answered lands on the landing screen, not a blank order summary", () => {
  // What it used to draw: a fully rendered order summary with a blank Level, a blank
  // Total, and a Pay button that silently did nothing: `pay()` returns at its
  // first guard when the session names no price, giving the buyer no feedback
  // at all. The duration list and the order summary are both drawn from the level, and there is none.
  assert.equal(heading(), "Support SimpleX");
  assert.ok(!inView().textContent.includes("Check your order"));
  assert.ok(!inView().textContent.includes("Pay "), "and no Pay button anywhere on it");
  assert.equal(storage.getItem("sxb.session.v1"), null, "nothing was invented to make it renderable");
  assert.equal(fetches.length, 0, "and nothing was asked of the network");
});

bootTest("main: the URL is rewritten too, so Back still leaves the site", () => {
  assert.equal(location.hash, "", "the landing screen carries no hash");
  assert.equal(history.url, "/");
  assert.equal(history.at, 0, "and it REPLACED the entry rather than adding one");
  history.back();
  assert.equal(history.left, true, "there is nothing behind the landing screen");
  history.left = false;
});

bootTest("main: the wizard still works from there — the tier list answers, and the order summary is reachable", async () => {
  inView().all("button.primary")[0]!.click();
  assert.equal(heading(), "Choose your level");
  inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
  primaryOf(inView())!.click();
  assert.equal(heading(), "How long?");
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primaryOf(inView())!.click();
  assert.equal(heading(), "Check your order");
  assert.ok(inView().textContent.includes("Legend"), inView().textContent);
  assert.ok(inView().textContent.includes("Pay $420.00 with "), "with a real total on a live button");
  await settle();
});

bootTest("main: and #/checkout is reachable once the level HAS been answered", () => {
  // The redirect is about the answer, not about the URL: the same hash that
  // landed on the landing screen above now renders the order summary, because the session names a price.
  history.pushState(null, "", "#/checkout");
  page.fire("popstate");
  assert.equal(heading(), "Check your order");
  assert.equal(location.hash, "#/checkout");
});

bootTest("main: nothing is left running once this file is done", async () => {
  const before = fetches.length;
  await settle(10);
  assert.equal(fetches.length, before, "no loop is still issuing requests");
});

// The stub answers an aborted request the way a real `fetch` does, including one whose signal was
// already aborted when it was called: `abort` never fires for those, so without the check the
// request would hold and a test would fail on its own timeout rather than say what happened.
bootTest("boot: a request whose signal is already aborted rejects rather than holding", async () => {
  const ctl = new AbortController();
  ctl.abort();
  const pageFetch = (globalThis as unknown as { window: { fetch: typeof fetch } }).window.fetch;
  await assert.rejects(() => pageFetch("/api/invoice/never", { signal: ctl.signal }));
});
