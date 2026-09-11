// Stripe's card confirm redirects the whole page to the return URL when it completes, so the buyer
// lands back on a fresh load — not on the code screen the confirm was on. The return URL carries no
// order id (a bearer capability never handed to Stripe), only the `sb_return` marker, so the app
// resumes the order from local state. The id of the order being paid is remembered before the redirect
// (`sb.cardReturn`), which matters here: the buyer has TWO unpaid orders and paid the OLDER one (a card
// order reopened from history), so "the newest order" would resume the wrong one. The card also settled
// during the redirect, so the order is `paid`, not `open` — resolving it as "the newest open order"
// would miss it entirely and drop the buyer on the welcome page for the split second after their code.
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
  // The newest order: a later crypto invoice the buyer left open. `orders()[0]` — the wrong one.
  {
    orderId: "inv_newer", badgeType: "legend", months: 12,
    createdAt: new Date(NOW - 10_000).toISOString(), status: "open",
    address: "48HqK2Xmv", cryptoAmount: "1.4", cryptoCurrency: "xmr", amount: 42000, currency: "usd",
  },
  // The older card order the buyer actually paid: settled by the watch in the instant before the
  // redirect committed, and holding its code.
  {
    orderId: "inv_card", badgeType: "supporter", months: 1,
    createdAt: new Date(NOW - 60_000).toISOString(), status: "paid", paidInFull: true,
    submitted: true, code: CODE, amount: 700, currency: "usd", settledAt: "2026-08-28T11:59:50Z",
  },
]));
// The confirm remembered which order it redirected for, before Stripe navigated the page away.
storage.setItem("sb.cardReturn.v1", JSON.stringify("inv_card"));

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
// The URL Stripe returns to: the bare page with our marker, plus the params Stripe appends itself.
const page = installPage({ storage, url: "/?sb_return&payment_intent=pi_1&redirect_status=succeeded" });
const { app } = page;
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));

returnTest("main: a card return resumes the paid order it was for, not the newest, and shows the code", async () => {
  await until(() => heading() === "Paid. Here is your code.", `the code screen, not ${heading()}`);
  assert.ok(codeShape.test(screenOf(app).serialize()), "the code the buyer paid for is on it");
  assert.ok(screenOf(app).serialize().includes(CODE), "and it is the code of inv_card, the order that was paid");
  assert.ok(!screenOf(app).serialize().includes("48HqK2Xmv"), "not the newer open order's address");
});

returnTest("main: the return is spent — the marker and the remembered order are both cleared", () => {
  assert.equal(page.location.search, "?order=inv_card", "the URL is normalised to the order resumed");
  assert.ok(!page.location.search.includes("sb_return"), "the marker is gone");
  assert.equal(page.storage.getItem("sb.cardReturn.v1"), null,
    "and the remembered order is consumed, so a later plain load never re-resumes it");
});
// The guard against a false resume — a bare load with paid orders in the store landing on the welcome
// page, not an old code — is main-card-full-store.test.ts, which boots at `/` with 50 paid orders and
// walks in from the welcome screen.
