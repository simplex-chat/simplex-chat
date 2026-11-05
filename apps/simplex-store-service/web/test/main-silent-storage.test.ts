// Some browsers accept every write, report success, and keep nothing, which a try/catch cannot see, so the probe writes and reads back.
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, timedTest } from "./boot.js";
import { MemStorage } from "./stub-dom.js";
import { ORDER_ID, seededStorage } from "./open-order.js";

const silentTest = timedTest(3000);

class SilentStorage extends MemStorage {
  override setItem(): void { /* accepted, and dropped */ }
}

const silent = seededStorage(new SilentStorage());

const page = installPage({ storage: silent, url: `/?order=${ORDER_ID}` });
const { app } = page;
await import("../src/main.js");

silentTest("main: a store that swallows writes is not called durable", () => {
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();
  const screen = inViewOf(app);
  assert.equal(headingOf(screen), "Your codes");
  assert.ok(screen.textContent.includes("cannot save anything new"),
    `a silent write is still a lost code, and the buyer has to be told: ${screen.textContent.slice(0, 200)}`);
  assert.ok(!screen.textContent.includes("in this browser, and nowhere else"),
    "nothing may promise the codes are kept here");
});
