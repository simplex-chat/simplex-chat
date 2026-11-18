import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, timedTest, until } from "./boot.js";
import { IDLE_DELETION_CODES } from "../src/screens.js";

const lostTest = timedTest(3000);
const NOW = Date.parse("2026-08-28T12:00:00Z");
const SAFARI_MAC = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.6 Safari/605.1.15";

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const page = installPage({ url: "/?order=inv_gone", userAgent: SAFARI_MAC });
const { app } = page;
page.respondWith({ status: 200, body: {
  status: "paid", badgeType: "supporter", months: 1, amount: 700, currency: "usd",
  paidInFull: true, settledAt: "2026-08-20T12:05:00Z",
} });
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));

lostTest("main: in Safari, the deletion line is shown for a paid order opened without its code", async () => {
  await until(() => heading() === "This code is not on this device", `the paid-no-code screen, not ${heading()}`);
  assert.ok(screenOf(app).textContent.includes(IDLE_DELETION_CODES), screenOf(app).textContent.slice(0, 400));
});
