// Opened by the mobile app, but every stored row already holds a code, so the new order's code cannot be saved.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, screenOf, timedTest, until } from "./boot.js";
import { MemStorage, type StubElement } from "./stub-dom.js";

const unsavedTest = timedTest(5000);
const NOW = Date.parse("2026-08-28T12:00:00Z");

const full = new MemStorage();
full.m.set("sb.orders.v1", JSON.stringify(
  Array.from({ length: 50 }, (_, i) => ({
    orderId: `inv_old_${i}`, badgeType: "supporter", months: 1,
    createdAt: new Date(NOW - (i + 2) * 86_400_000).toISOString(), status: "paid", paidInFull: true,
    code: `SB-OLD${String(i).padStart(2, "0")}-YGQTM-PUYZ9-2TUXP`, amount: 700, currency: "usd",
  })),
));

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const page = installPage({ storage: full, url: "/#/tier?app=true" });
const { app } = page;
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));
const inView = (): StubElement => inViewOf(app);

unsavedTest("main: a code this browser could not save is shown at once, not held back behind the link", async () => {
  inView().all("button.choice").find((c) => c.textContent.startsWith("Supporter"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("1 month"))!.click();
  primaryOf(inView())!.click();
  page.respondWith({ status: 200, body: {
    invoiceId: "inv_unsaved", badgeType: "supporter", months: 1, amount: 700, currency: "usd",
    expiresAt: "2026-08-28T12:58:12Z", address: "48HqK2Xm", cryptoAmount: "0.02", cryptoCurrency: "xmr",
  } });
  inView().all("button.primary").find((b) => b.textContent.startsWith("Pay"))!.click();
  await until(() => heading() === "Send 0.02 XMR", `the payment screen, not ${heading()}`);

  page.answerHeld({ status: 200, body: {
    status: "paid", badgeType: "supporter", months: 1, amount: 700, currency: "usd",
    paidInFull: true, settledAt: "2026-08-28T12:05:00Z",
  } }, "/api/invoice/inv_unsaved");
  await until(() => heading() === "Paid. Here is your code.", `the plain code screen, not ${heading()}`);

  assert.match(screenOf(app).all("div.code")[0]!.textContent, /^SB-[0-9A-Z]{5}-[0-9A-Z]{5}-[0-9A-Z]{5}-[0-9A-Z]{5}$/);
  assert.ok(screenOf(app).textContent.includes("This code could not be saved in this browser."));
  assert.equal(screenOf(app).all("iframe").length, 0, "no link is tried");
  assert.equal(screenOf(app).all("button").filter((b) => b.textContent === "Show code").length, 0);
});
