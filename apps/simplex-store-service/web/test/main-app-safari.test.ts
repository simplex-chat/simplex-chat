import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, screenOf, timedTest, until } from "./boot.js";
import type { StubElement } from "./stub-dom.js";
import { COPY_UNLESS_ADDED, IDLE_DELETION, IDLE_DELETION_ALL } from "../src/screens.js";

const safariTest = timedTest(5000);
const NOW = Date.parse("2026-08-28T12:00:00Z");
const SAFARI_IPHONE = "Mozilla/5.0 (iPhone; CPU iPhone OS 18_6 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.6 Mobile/15E148 Safari/604.1";

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const page = installPage({ url: "/#/tier?app=true", userAgent: SAFARI_IPHONE });
const { app } = page;
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));
const inView = (): StubElement => inViewOf(app);

safariTest("main: in Safari opened from the app, the paid ending warns that the browser deletes the code", async () => {
  inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Check your order");

  page.respondWith({ status: 200, body: {
    invoiceId: "inv_safari", badgeType: "legend", months: 12, amount: 42000, currency: "usd",
    expiresAt: "2026-08-28T12:58:12Z", address: "48HqK2Xm", cryptoAmount: "1.482", cryptoCurrency: "xmr",
  } });
  inView().all("button.primary").find((b) => b.textContent.startsWith("Pay"))!.click();
  await until(() => heading() === "Send 1.482 XMR", `the payment screen, not ${heading()}`);

  page.answerHeld({ status: 200, body: {
    status: "paid", badgeType: "legend", months: 12, amount: 42000, currency: "usd",
    paidInFull: true, settledAt: "2026-08-28T12:05:00Z",
  } }, "/api/invoice/inv_safari");
  await until(() => heading() === "Paid", `the return-to-app ending, not ${heading()}`);

  const ending = inView();
  assert.ok(ending.textContent.includes(COPY_UNLESS_ADDED), `the ending lacks the hint: ${ending.textContent.slice(0, 300)}`);
  assert.ok(ending.textContent.includes(IDLE_DELETION), "and the reason");
});

safariTest("main: in Safari, Your codes says the browser deletes the codes stored here", () => {
  page.respondWith({ status: 200, body: {
    status: "paid", badgeType: "legend", months: 12, amount: 42000, currency: "usd",
    paidInFull: true, settledAt: "2026-08-28T12:05:00Z",
  } });
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();
  assert.equal(headingOf(inView()), "Your codes");
  assert.ok(inView().textContent.includes(IDLE_DELETION_ALL));
});
