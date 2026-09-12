// The checkout's in-flight guard, against the thing that defeats a disabled attribute: a rebuild
// of the panel it is set on. Back and Continue do that, and so does choosing another method; the
// rebuilt Pay button carries none of it. `/api/invoice` has no idempotency key, so a second POST
// is a second charge and a second code.
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, settle, timedTest } from "./boot.js";

const payTest = timedTest(3000);

const page = installPage();
const { app, fetches } = page;
await import("../src/main.js");

const inView = (): ReturnType<typeof inViewOf> => inViewOf(app);

payTest("main: a repaint while the checkout is on the wire does not arm a second Pay", async () => {
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Check your order");

  const before = fetches.length;
  page.respondWith({ status: 500, body: { error: "internal" } });
  inView().all("button.primary")[0]!.click();

  // no await: the POST is on the wire for all of this. Back and Continue rebuild the panel, and
  // the rebuilt Pay button carries none of the disabled attribute the click handler set.
  inView().all("button.back")[0]!.click();
  primaryOf(inView())!.click();
  const armed = primaryOf(inView());
  assert.ok(armed, "the rebuilt Pay button really is armed again, or this proves nothing");
  armed.click();

  // `pay()` hashes the code before it posts, so nothing is on the wire yet at this instant; the
  // count that matters is the one after both clicks have had every chance to reach the network.
  // Back moved the page on, so the 500 draws no failure screen over the panel it landed on.
  await settle();
  assert.equal(headingOf(inView()), "Check your order", "the answer belongs to a page that left");
  const posts = fetches.slice(before).filter((f) => f.url === "/api/invoice");
  assert.equal(posts.length, 1,
    `exactly one POST: a repaint must not let a second through (${JSON.stringify(fetches.slice(before).map((f) => f.url))})`);
});
