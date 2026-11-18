import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, screenOf, timedTest, until } from "./boot.js";
import type { StubElement } from "./stub-dom.js";
import { CAN_DELETE_CODE, MAY_DO_SO_AFTER, KEEP_OUTSIDE } from "../src/screens.js";

const codeTest = timedTest(5000);
const NOW = Date.parse("2026-08-28T12:00:00Z");
const SAFARI_MAC = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.6 Safari/605.1.15";

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const page = installPage({ url: "/#/tier", userAgent: SAFARI_MAC });
const { app } = page;
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));
const inView = (): StubElement => inViewOf(app);

codeTest("main: in Safari, the deletion warning box follows the only-copy box", async () => {
  inView().all("button.choice").find((c) => c.textContent.startsWith("Supporter"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("1 month"))!.click();
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Check your order");

  page.respondWith({ status: 200, body: {
    invoiceId: "inv_mac", badgeType: "supporter", months: 1, amount: 700, currency: "usd",
    expiresAt: "2026-08-28T12:58:12Z", address: "48HqK2Xm", cryptoAmount: "0.02", cryptoCurrency: "xmr",
  } });
  inView().all("button.primary").find((b) => b.textContent.startsWith("Pay"))!.click();
  await until(() => heading() === "Send 0.02 XMR", `the payment screen, not ${heading()}`);

  page.answerHeld({ status: 200, body: {
    status: "paid", badgeType: "supporter", months: 1, amount: 700, currency: "usd",
    paidInFull: true, settledAt: "2026-08-28T12:05:00Z",
  } }, "/api/invoice/inv_mac");
  await until(() => heading() === "Paid. Here is your code.", `the code screen, not ${heading()}`);

  const boxes = screenOf(app).all("div.warn");
  assert.deepEqual(boxes.map((b) => b.all("span.title")[0]!.textContent), ["This is the only copy.", CAN_DELETE_CODE]);
  assert.deepEqual(boxes[1]!.all("p").map((l) => l.textContent), [MAY_DO_SO_AFTER, KEEP_OUTSIDE]);
});
