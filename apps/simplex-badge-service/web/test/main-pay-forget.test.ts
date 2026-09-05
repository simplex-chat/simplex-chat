// A checkout on the wire while the buyer wipes the browser.
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, settle, timedTest, until } from "./boot.js";

const payTest = timedTest(3000);

const page = installPage();
const { app, fetches, storage } = page;
await import("../src/main.js");

const inView = (): ReturnType<typeof inViewOf> => inViewOf(app);

payTest("main: a checkout answered after the wipe is not written back", async () => {
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Check your order");

  const before = fetches.length;
  inView().all("button.primary")[0]!.click();
  await until(() => fetches.slice(before).some((f) => f.url === "/api/invoice"), "the checkout POST");

  page.confirmAnswer(true);
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Forget everything on this device")!.click();
  assert.equal(storage.getItem("sxb.orders.v1"), null, "the wipe itself is immediate");

  assert.ok(page.answerHeld({
    status: 200,
    body: {
      invoiceId: "inv_left", badgeType: "legend", months: 12,
      amount: 42000, currency: "usd", expiresAt: "2026-08-28T13:00:00Z",
      address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr",
    },
  }, "/api/invoice"), "the checkout POST is the one still holding");
  await settle(10);

  assert.equal(storage.getItem("sxb.orders.v1"), null,
    "the order the buyer erased does not come back as a row with a code");
});
