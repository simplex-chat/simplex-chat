import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, timedTest } from "./boot.js";
import { MemStorage, type StubElement } from "./stub-dom.js";

const test = timedTest(2000);

// Without a priceId the router clamps a deep step back to the landing, so a tier is chosen to make the duration step reachable.
const storage = new MemStorage();
storage.setItem("sb.session.v1", JSON.stringify({ step: "months", priceId: "price_legend", offerId: "offer_12m" }));

// The buyer arrives straight at the duration step, so the steps before it are not in the document's history.
const page = installPage({ storage, url: "/#/months" });
const { app, history } = page;
await import("../src/main.js");

const inView = (): StubElement => inViewOf(app);
const heading = (): string => headingOf(inView());
const back = (): void => { inView().all("button.back")[0]!.click(); };

test("main: loading straight at a deep step rebuilds the stack, so [ ← Back ] walks it, never off the page", () => {
  assert.equal(heading(), "How long?", "the duration step, resolved from the URL and the stored tier");
  assert.equal(history.url, "/#/months");
  // The two steps before this one are rebuilt beneath it, or history.back() would leave the wizard, and when embedded, the badges page.
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
