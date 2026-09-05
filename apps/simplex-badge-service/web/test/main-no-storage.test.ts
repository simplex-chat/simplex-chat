// A browser that refuses to keep anything: site data blocked for the origin, or a private mode
// whose `localStorage` throws on write. The page falls back to memory so the buyer can still get
// through the wizard; what it must not then claim is covered by `store.test.ts`'s `holdsCode` and
// `screens.test.ts`'s history list, which are where that promise is made.
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, timedTest } from "./boot.js";
import { MemStorage } from "./stub-dom.js";
import { NOT_KEPT_TITLE } from "../src/screens.js";

const noStorageTest = timedTest(3000);

// what a browser with site data blocked does: the write throws, and nothing is kept
class RefusingStorage extends MemStorage {
  override setItem(): void { throw new Error("SecurityError: the operation is insecure"); }
}

const page = installPage({ storage: new RefusingStorage() });
const { app } = page;
await import("../src/main.js");

const inView = (): ReturnType<typeof inViewOf> => inViewOf(app);

noStorageTest("main: the wizard still walks when every write is refused", () => {
  // the session cannot be stored, so a page that only wrote through `localStorage` would leave
  // the tier unmarked, Continue disabled, and no way forward and no reason given
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Choose your level");
  inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
  // re-queried: the click rebuilds the panel, so the node clicked above is detached by now
  const chosen = inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!;
  assert.equal(chosen.getAttribute("aria-pressed"), "true", "the choice is remembered in this session");
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "How long?");
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Check your order");

  // and the screen carrying the Pay button is where the buyer is told, since afterwards the code
  // is already drawn and copying it by hand is the only thing left
  assert.ok(inView().textContent.includes(NOT_KEPT_TITLE),
    `the warning belongs above the Pay button: ${inView().textContent.slice(0, 200)}`);
  assert.ok(inView().all("button").some((b) => b.textContent.startsWith("Pay ")),
    "and it is a warning, not a refusal");
});
