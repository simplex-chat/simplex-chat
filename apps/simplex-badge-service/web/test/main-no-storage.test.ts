import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, timedTest } from "./boot.js";
import { MemStorage } from "./stub-dom.js";
import { NOT_KEPT_TITLE } from "../src/screens.js";

const noStorageTest = timedTest(3000);

class RefusingStorage extends MemStorage {
  override setItem(): void { throw new Error("SecurityError: the operation is insecure"); }
}

const page = installPage({ storage: new RefusingStorage() });
const { app } = page;
await import("../src/main.js");

const inView = (): ReturnType<typeof inViewOf> => inViewOf(app);

noStorageTest("main: the wizard still walks when every write is refused", () => {
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Choose your level");
  inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
  // The click rebuilds the panel, so the node clicked above is detached and must be re-queried.
  const chosen = inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!;
  assert.equal(chosen.getAttribute("aria-pressed"), "true", "the choice is remembered in this session");
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "How long?");
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Check your order");

  assert.ok(inView().textContent.includes(NOT_KEPT_TITLE),
    `the warning belongs above the Pay button: ${inView().textContent.slice(0, 200)}`);
  assert.ok(inView().all("button").some((b) => b.textContent.startsWith("Pay ")),
    "and it is a warning, not a refusal");
});
