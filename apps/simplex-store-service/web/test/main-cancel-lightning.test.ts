import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, settle, timedTest, until } from "./boot.js";
import { MemStorage } from "./stub-dom.js";
import { NOW } from "./open-order.js";
import { CANCEL_CONFIRM_LIGHTNING, CANCEL_INVOICE } from "../src/screens.js";

const cancelTest = timedTest(3000);

const ORDER_ID = "inv_ln_open";
const BOLT11 = "lnbc499999440p1pfakeinvoicedestinationforthetestsonly";
const openLn = {
  status: "open", badgeType: "supporter", months: 1, amount: 700, currency: "usd",
  expiresAt: "2026-08-28T12:58:12Z", address: BOLT11, cryptoAmount: "0.00049999944", cryptoCurrency: "ln",
} as const;

const storage = new MemStorage();
storage.m.set("sb.orders.v1", JSON.stringify([{
  orderId: ORDER_ID, createdAt: new Date(NOW - 60_000).toISOString(), method: "ln", ...openLn,
}]));

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage, url: `/?order=${ORDER_ID}` });
const { app, confirms, fetches } = page;
page.respondWith({ status: 200, body: openLn });
await import("../src/main.js");

cancelTest("main: cancelling a Lightning invoice asks with the Lightning wording, and a refusal sends nothing", async () => {
  await until(() => headingOf(screenOf(app)) === "Send 0.00049999944 BTC", "the Lightning payment screen");
  page.confirmAnswer(false);
  const before = fetches.length;
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)!.click();
  await settle();
  assert.deepEqual(confirms, [CANCEL_CONFIRM_LIGHTNING]);
  assert.ok(!fetches.slice(before).some((f) => f.url.endsWith("/cancel")), "a declined confirm cancels nothing");
});
