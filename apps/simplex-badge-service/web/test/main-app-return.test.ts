// A mobile-app order comes back from Stripe with neither the query nor the fragment, so the ending is read off the order alone.
// This is also the reload after Show code: a fresh load puts the ending back, with the code one tap away.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, timedTest, until } from "./boot.js";
import { MemStorage } from "./stub-dom.js";

const returnTest = timedTest(3000);
const NOW = Date.parse("2026-08-28T12:00:00Z");
const CODE = "SB-YDC8A-YGQTM-PUYZ9-2TUXP";

const storage = new MemStorage();
storage.setItem("sb.orders.v1", JSON.stringify([{
  orderId: "inv_card", badgeType: "supporter", months: 1,
  createdAt: new Date(NOW - 60_000).toISOString(), status: "paid", paidInFull: true,
  submitted: true, code: CODE, amount: 700, currency: "usd", method: "card", app: "mobile",
}]));
storage.setItem("sb.cardReturn.v1", JSON.stringify({ orderId: "inv_card", at: NOW - 5_000 }));

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const page = installPage({ storage, url: "/?payment_intent=pi_1&redirect_status=succeeded" });
const { app } = page;
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));

returnTest("main: back from Stripe, the order's own flag picks the return-to-app ending", async () => {
  await until(() => heading() === "Paid", `the return-to-app ending, not ${heading()}`);
  assert.equal(page.location.search, "?order=inv_card");
  assert.equal(screenOf(app).all("iframe")[0]!.getAttribute("src"), `simplexchat:/badge/code/${CODE}`);
  assert.ok(!screenOf(app).textContent.includes(CODE));
});

returnTest("main: Show code reveals the stored code", () => {
  screenOf(app).all("button.link").find((b) => b.textContent === "Show code")!.click();
  assert.equal(heading(), "Paid. Here is your code.");
  assert.equal(screenOf(app).all("div.code")[0]!.textContent, CODE);
});
