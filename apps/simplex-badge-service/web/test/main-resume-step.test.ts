import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, timedTest } from "./boot.js";
import { MemStorage, type StubElement } from "./stub-dom.js";

const test = timedTest(2000);

// A tier is already chosen, so the duration step is reachable — without a `priceId` the router clamps
// a deep step back to the landing, and there would be nothing to rebuild.
const storage = new MemStorage();
storage.setItem("sb.session.v1", JSON.stringify({ step: "months", priceId: "price_legend", offerId: "offer_12m" }));

// The buyer arrives straight at the duration step: a reload, a deep link, or the site framing us at it
// via the iframe's src. This is the case where the steps before it are not in the document's history.
const page = installPage({ storage, url: "/#/months" });
const { app, history } = page;
await import("../src/main.js");

const inView = (): StubElement => inViewOf(app);
const heading = (): string => headingOf(inView());
const back = (): void => { inView().all("button.back")[0]!.click(); };

test("main: loading straight at a deep step rebuilds the stack, so [ ← Back ] walks it, never off the page", () => {
  assert.equal(heading(), "How long?", "the duration step, resolved from the URL and the stored tier");
  assert.equal(history.url, "/#/months");
  // Landing, tier, months: the two steps before this one, rebuilt beneath it. Without this the stack is
  // one entry and history.back() leaves the wizard — to the landing, or, embedded, off the badges page.
  assert.equal(history.stack.length, 3, `rebuilt beneath: ${JSON.stringify(history.stack)}`);

  back();
  assert.equal(heading(), "Choose your level", "Back reaches the tier list, one step, not the landing");
  assert.equal(history.url, "/#/tier");
  assert.equal(history.left, false, "and it stayed in the wizard, it did not leave the site");

  back();
  assert.equal(heading(), "Support SimpleX", "Back again reaches the landing");
  assert.equal(history.url, "/");
  assert.equal(history.left, false);
});
