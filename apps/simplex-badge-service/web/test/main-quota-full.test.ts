// The writes throw but codes already stored stay readable, so swapping in a memory store would hide them.
import assert from "node:assert/strict";
import { forgetControl, headingOf, inViewOf, installPage, timedTest } from "./boot.js";
import { MemStorage } from "./stub-dom.js";
import { ORDER_ID, seededStorage } from "./open-order.js";

const quotaTest = timedTest(3000);

class FullStorage extends MemStorage {
  override setItem(): void { throw new Error("QuotaExceededError"); }
}

const full = seededStorage(new FullStorage());

const page = installPage({ storage: full, url: `/?order=${ORDER_ID}` });
const { app } = page;
await import("../src/main.js");

quotaTest("main: codes already stored stay readable when the writes stop", () => {
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();
  const screen = inViewOf(app);
  assert.equal(headingOf(screen), "Your codes");
  assert.ok(screen.textContent.includes("Legend, 12 months"),
    `an order this browser really holds must still be listed: ${screen.textContent.slice(0, 200)}`);
  assert.ok(screen.textContent.includes("cannot save anything new"),
    "and the list says what it can and cannot promise");
});

quotaTest("main: Forget clears the store that is really holding the codes", () => {
  // The removal must reach the real store, since that is where the codes being erased actually live.
  assert.ok(full.m.has("sb.orders.v1"), "the real store is the one holding them");
  page.confirmAnswer(true);
  forgetControl(page)!.click();
  assert.equal(full.m.get("sb.orders.v1"), undefined, "and it is what Forget has to empty");
});
