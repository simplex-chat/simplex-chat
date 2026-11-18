import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, timedTest } from "./boot.js";
import { MemStorage } from "./stub-dom.js";
import { IDLE_DELETION_CODES } from "../src/screens.js";

const refusedTest = timedTest(3000);
const SAFARI_MAC = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.6 Safari/605.1.15";

class RefusingStorage extends MemStorage {
  override setItem(): void { throw new Error("SecurityError: the operation is insecure"); }
}

const page = installPage({ storage: new RefusingStorage(), userAgent: SAFARI_MAC });
const { app } = page;
await import("../src/main.js");

refusedTest("main: in Safari with localStorage refused, Your codes is shown without the deletion warning", () => {
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();
  const screen = inViewOf(app);
  assert.equal(headingOf(screen), "Your codes");
  assert.ok(!screen.textContent.includes(IDLE_DELETION_CODES), `the deletion warning is shown although writes to localStorage throw:${screen.textContent.slice(0, 300)}`);
});
