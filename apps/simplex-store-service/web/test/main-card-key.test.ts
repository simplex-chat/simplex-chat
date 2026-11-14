import assert from "node:assert/strict";
import { headingOf, installPage, inViewOf, primaryOf, screenOf, settle, timedTest, until } from "./boot.js";
import { StubElement } from "./stub-dom.js";

const cardKeyTest = timedTest(5000);
const KEY_AT_LOAD = "pk_test_key_at_load";
const KEY_AFTER_HIDDEN = "pk_test_key_after_hidden";
const KEY_AFTER_RESTORE = "pk_test_key_after_restore";
const CLIENT_SECRET = "cs_test_card_key";

const page = installPage();
const { app } = page;
page.siteConfig.stripePublishableKey = KEY_AT_LOAD;

const keysMounted: string[] = [];
(globalThis as unknown as { window: Record<string, unknown> }).window.Stripe = (key: string) => ({
  elements: () => ({ create: () => ({ mount: () => { keysMounted.push(key); }, destroy: () => {} }) }),
  confirmPayment: () => new Promise(() => {}),
});

await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));
const inView = (): StubElement => inViewOf(app);
const stripeTag = (): StubElement =>
  page.document.head.children.find(
    (c): c is StubElement => c instanceof StubElement && (c.getAttribute("src") ?? "").includes("js.stripe.com"),
  )!;

async function remountedAfterFlap(): Promise<string> {
  const mounts = keysMounted.length;
  page.setOffline(true);
  await settle();
  page.setOffline(false);
  await until(() => keysMounted.length === mounts + 1, "the card form mounted again after the network came back");
  assert.equal(heading(), "Pay by card");
  return keysMounted.at(-1)!;
}

cardKeyTest("main: the card form mounts with the key the page read at load", async () => {
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("Supporter"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("1 month"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice.method").find((b) => b.textContent.startsWith("Card"))!.click();
  page.respondWith({ status: 200, body: {
    invoiceId: "inv_card_key", badgeType: "supporter", months: 1,
    amount: 700, currency: "usd", expiresAt: "2126-08-28T12:58:12Z", clientSecret: CLIENT_SECRET,
  } });
  inView().all("button.primary").find((b) => b.textContent.startsWith("Pay"))!.click();
  await until(() => heading() === "Pay by card", `the card form, not ${heading()}`);
  stripeTag().dispatch("load");
  await until(() => keysMounted.length === 1, "the first card form mounted");
  assert.deepEqual(keysMounted, [KEY_AT_LOAD]);
});

cardKeyTest("main: while the page stays in view, a remount keeps the key it read", async () => {
  page.siteConfig.stripePublishableKey = KEY_AFTER_HIDDEN;
  assert.equal(await remountedAfterFlap(), KEY_AT_LOAD, `mounted keys: ${keysMounted.join(", ")}`);
});

cardKeyTest("main: a page shown again after being hidden reads the key the service serves now", async () => {
  page.document.hidden = true;
  page.document.dispatch("visibilitychange");
  assert.equal(await remountedAfterFlap(), KEY_AT_LOAD, "hiding alone does not drop the key a form on screen uses");
  page.document.hidden = false;
  page.document.dispatch("visibilitychange");
  assert.equal(await remountedAfterFlap(), KEY_AFTER_HIDDEN, `mounted keys: ${keysMounted.join(", ")}`);
});

cardKeyTest("main: a page restored from the back-forward cache reads the key again, and a fresh pageshow does not", async () => {
  page.siteConfig.stripePublishableKey = KEY_AFTER_RESTORE;
  page.fire("pageshow", { persisted: false });
  assert.equal(await remountedAfterFlap(), KEY_AFTER_HIDDEN, `mounted keys: ${keysMounted.join(", ")}`);
  page.fire("pageshow", { persisted: true });
  assert.equal(await remountedAfterFlap(), KEY_AFTER_RESTORE, `mounted keys: ${keysMounted.join(", ")}`);
});
