// A payment can land during the cancel's round trip, so the answer is expired with a figure on it, and reading the status alone would send the buyer to a new purchase with money stuck.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, timedTest, until } from "./boot.js";
import { ADDRESS, NOW, openReply, ORDER_ID, seededStorage } from "./open-order.js";
import { CANCEL_INVOICE } from "../src/screens.js";

const raceTest = timedTest(3000);

const storage = seededStorage();

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });

const page = installPage({ storage, url: `/?order=${ORDER_ID}` });
const { app } = page;
page.respondWith(openReply);
await import("../src/main.js");


const cancelledWithPayment = {
  status: 200,
  body: {
    status: "expired", badgeType: "legend", months: 12,
    amount: 42000, currency: "usd",
    cryptoAmountPaid: "0.741", cryptoAmountDue: "0.741", paidInFull: false,
  },
} as const;

raceTest("main: a cancel that raced a payment leaves the buyer on the order", async () => {
  await until(() => headingOf(screenOf(app)).startsWith("Send"), "the payment screen");

  page.confirmAnswer(true);
  page.respondWith(cancelledWithPayment);
  screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE)!.click();
  // The restart's first read decides what the buyer sees, and failing it makes the loop fall back on the record, which still names the address.
  page.respondWith({ status: 500, body: { error: "internal" } });
  await until(() => headingOf(screenOf(app)) === "This invoice expired",
    "the closed-window screen, not the payment screen it replaced");

  assert.ok(!screenOf(app).textContent.includes(ADDRESS),
    `no dead address is drawn: ${screenOf(app).textContent.slice(0, 160)}`);
  assert.ok(!screenOf(app).textContent.includes("Choose your badge"),
    "and not the landing screen either");

  assert.ok(screenOf(app).textContent.includes(ORDER_ID), "the reference support reconciles against");
});
