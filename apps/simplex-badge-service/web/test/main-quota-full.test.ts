// A quota that filled after an earlier purchase: the writes throw, and the codes already stored
// are still readable. Swapping in a memory store on the failed write would hide them, and this
// page's one promise is holding the code.
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, timedTest } from "./boot.js";
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
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Codes on this device")!.click();
  const screen = inViewOf(app);
  assert.equal(headingOf(screen), "Codes on this device");
  assert.ok(screen.textContent.includes("Legend, 12 months"),
    `an order this browser really holds must still be listed: ${screen.textContent.slice(0, 200)}`);
  assert.ok(screen.textContent.includes("cannot save anything new"),
    "and the list says what it can and cannot promise");
});

quotaTest("main: Forget clears the store that is really holding the codes", () => {
  // writes go to memory, but the removal has to reach the real store: everything this page is
  // promising to erase is over there, and the confirm says it cannot be undone
  assert.ok(full.m.has("sxb.orders.v1"), "the real store is the one holding them");
  page.confirmAnswer(true);
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Forget everything on this device")!.click();
  assert.equal(full.m.get("sxb.orders.v1"), undefined, "and it is what Forget has to empty");
});
