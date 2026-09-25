import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, settle, timedTest, until } from "./boot.js";

const payTest = timedTest(4000);

const page = installPage();
const { app, fetches, storage } = page;
await import("../src/main.js");

const inView = (): ReturnType<typeof inViewOf> => inViewOf(app);

payTest("main: a checkout answered after a re-choice keeps the buyer's tier", async () => {
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Check your order");

  const before = fetches.length;
  inView().all("button.primary")[0]!.click();
  await until(() => fetches.slice(before).some((f) => f.url === "/api/invoice"), "the checkout POST");

  history.back();
  history.back();
  assert.equal(headingOf(inView()), "Choose your badge", `at the tier screen: ${headingOf(inView())}`);
  inView().all("button.choice").find((c) => c.textContent.startsWith("Supporter"))!.click();

  assert.ok(page.answerHeld({
    status: 200,
    body: {
      invoiceId: "inv_left", badgeType: "legend", months: 12,
      amount: 42000, currency: "usd", expiresAt: "2026-08-28T13:00:00Z",
      address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr",
    },
  }, "/api/invoice"), "the checkout POST is the one still holding");
  await settle(10);

  // If the new answer were cleared, effectiveSession would reseed from the bought order and the next checkout would charge Legend's price for the Supporter on screen.
  const session = JSON.parse(storage.getItem("sb.session.v1") ?? "null") as { priceId?: string } | null;
  assert.equal(session?.priceId, "price_supporter",
    `the buyer chose Supporter and it has to survive: ${JSON.stringify(session)}`);

  assert.equal(headingOf(inView()), "Choose your badge", `not taken to the bought order: ${headingOf(inView())}`);
});
