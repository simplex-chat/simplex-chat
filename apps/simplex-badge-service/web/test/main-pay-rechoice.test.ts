// The buyer walks back and picks a different tier while the checkout POST is on the wire.
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

  // back to the tier screen and a different tier
  history.back();      // #/months
  history.back();      // #/tier
  assert.equal(headingOf(inView()), "Choose your level", `at the tier screen: ${headingOf(inView())}`);
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

  // The Legend invoice was really bought, so the page goes to it. What must not happen is the
  // buyer's new answer being spent for it: cleared, `effectiveSession` reseeds from the order just
  // stored, and the next checkout charges Legend's $420 for the Supporter the buyer is looking at.
  const session = JSON.parse(storage.getItem("sxb.session.v1") ?? "null") as { priceId?: string } | null;
  assert.equal(session?.priceId, "price_supporter",
    `the buyer chose Supporter and it has to survive: ${JSON.stringify(session)}`);

  // and the page stays where the buyer went, on the choice they are making
  assert.equal(headingOf(inView()), "Choose your level", `not taken to the bought order: ${headingOf(inView())}`);
});
