// A reload mid-purchase: the flag is off the URL by now, so only the session can still carry it to the order.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, screenOf, timedTest, until } from "./boot.js";
import { MemStorage } from "./stub-dom.js";

const reloadTest = timedTest(5000);
const NOW = Date.parse("2026-08-28T12:00:00Z");

const storage = new MemStorage();
storage.setItem("sb.session.v1", JSON.stringify({ step: "months", priceId: "price_legend", offerId: "offer_12m", app: "mobile" }));

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const page = installPage({ storage, url: "/#/months" });
const { app } = page;
await import("../src/main.js");

reloadTest("main: after a reload the session still carries the flag, and the order it makes is stamped", async () => {
  assert.equal(headingOf(inViewOf(app)), "How long?");
  assert.equal((JSON.parse(storage.getItem("sb.session.v1")!) as { app?: unknown }).app, "mobile");

  primaryOf(inViewOf(app))!.click();
  page.respondWith({ status: 200, body: {
    invoiceId: "inv_reload", badgeType: "legend", months: 12, amount: 42000, currency: "usd",
    expiresAt: "2026-08-28T12:58:12Z", address: "48HqK2Xm", cryptoAmount: "1.482", cryptoCurrency: "xmr",
  } });
  inViewOf(app).all("button.primary").find((b) => b.textContent.startsWith("Pay"))!.click();
  await until(() => headingOf(screenOf(app)) === "Send 1.482 XMR", "the payment screen");
  const order = (JSON.parse(storage.getItem("sb.orders.v1")!) as Array<{ app?: string }>)[0]!;
  assert.equal(order.app, "mobile");
});
