// A card confirm redirects the whole page to the return URL, so the buyer comes back on a fresh load,
// not on the screen the confirm was on. The order being paid is remembered in local state before the
// redirect (sb.cardReturn), and the frame resumes it on the next load — no marker in the URL, so it
// works embedded (return URL is the host page) and standalone alike. Here the buyer has TWO unpaid
// orders and paid the OLDER one (a card order reopened from history), so "newest" would resume the
// wrong one; the card also settled during the redirect, so the order is paid, which newestOpen misses.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, screenOf, timedTest, until } from "./boot.js";
import { MemStorage } from "./stub-dom.js";

const returnTest = timedTest(3000);
const NOW = Date.parse("2026-08-28T12:00:00Z");
const CODE = "SB-YDC8A-YGQTM-PUYZ9-2TUXP";
const codeShape = /SB-[0-9A-Z]{5}-[0-9A-Z]{5}-[0-9A-Z]{5}-[0-9A-Z]{5}/;

const storage = new MemStorage();
storage.setItem("sb.orders.v1", JSON.stringify([
  {
    orderId: "inv_newer", badgeType: "legend", months: 12,
    createdAt: new Date(NOW - 10_000).toISOString(), status: "open",
    address: "48HqK2Xmv", cryptoAmount: "1.4", cryptoCurrency: "xmr", amount: 42000, currency: "usd",
  },
  {
    orderId: "inv_card", badgeType: "supporter", months: 1,
    createdAt: new Date(NOW - 60_000).toISOString(), status: "paid", paidInFull: true,
    submitted: true, code: CODE, amount: 700, currency: "usd", settledAt: "2026-08-28T11:59:50Z",
  },
]));
storage.setItem("sb.cardReturn.v1", JSON.stringify({ orderId: "inv_card", at: NOW - 5_000 }));

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
// The return URL Stripe redirected to, with the params it appends and no marker of our own.
const page = installPage({ storage, url: "/?payment_intent=pi_1&redirect_status=succeeded" });
const { app } = page;
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));

returnTest("main: a card return resumes the exact order it was for, not the newest, and shows the code", async () => {
  await until(() => heading() === "Paid. Here is your code.", `the code screen, not ${heading()}`);
  assert.ok(codeShape.test(screenOf(app).serialize()), "the code the buyer paid for is on it");
  assert.ok(screenOf(app).serialize().includes(CODE), "and it is the code of inv_card, the order that was paid");
  assert.ok(!screenOf(app).serialize().includes("48HqK2Xmv"), "not the newer open order's address");
});

returnTest("main: the return is spent — the URL is the order and the remembered id is cleared", () => {
  assert.equal(page.location.search, "?order=inv_card", "the URL is normalised to the order resumed");
  assert.equal(page.storage.getItem("sb.cardReturn.v1"), null,
    "the remembered order is consumed, so a later plain load never re-resumes it");
});
