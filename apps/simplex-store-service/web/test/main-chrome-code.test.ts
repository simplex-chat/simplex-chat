import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, screenOf, timedTest, until } from "./boot.js";
import type { StubElement } from "./stub-dom.js";
import { CAN_DELETE_CODES, KEEP_OUTSIDE } from "../src/screens.js";

const codeTest = timedTest(5000);
const NOW = Date.parse("2026-08-28T12:00:00Z");
const CHROME_MAC = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/139.0.0.0 Safari/537.36";

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const page = installPage({ url: "/#/tier", userAgent: CHROME_MAC });
const { app } = page;
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));
const inView = (): StubElement => inViewOf(app);

codeTest("main: in Chrome, the only-copy box is shown without the deletion warning", async () => {
  inView().all("button.choice").find((c) => c.textContent.startsWith("Supporter"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("1 month"))!.click();
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Check your order");

  page.respondWith({ status: 200, body: {
    invoiceId: "inv_chrome", badgeType: "supporter", months: 1, amount: 700, currency: "usd",
    expiresAt: "2026-08-28T12:58:12Z", address: "48HqK2Xm", cryptoAmount: "0.02", cryptoCurrency: "xmr",
  } });
  inView().all("button.primary").find((b) => b.textContent.startsWith("Pay"))!.click();
  await until(() => heading() === "Send 0.02 XMR", `the payment screen, not ${heading()}`);

  page.answerHeld({ status: 200, body: {
    status: "paid", badgeType: "supporter", months: 1, amount: 700, currency: "usd",
    paidInFull: true, settledAt: "2026-08-28T12:05:00Z",
  } }, "/api/invoice/inv_chrome");
  await until(() => heading() === "Paid. Here is your code.", `the code screen, not ${heading()}`);

  const boxes = screenOf(app).all("div.warn");
  assert.deepEqual(boxes.map((b) => b.all("span.title")[0]!.textContent), ["This is the only copy."],
    `the deletion warning is shown in Chrome: ${screenOf(app).textContent.slice(0, 400)}`);
  assert.ok(!screenOf(app).textContent.includes(KEEP_OUTSIDE));
});

codeTest("main: in Chrome, Your codes is shown without the deletion warning", () => {
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();
  assert.equal(headingOf(inView()), "Your codes");
  assert.equal(inView().all("li.entry").length, 1, "the paid code is listed");
  assert.ok(!inView().textContent.includes(CAN_DELETE_CODES), `the deletion warning is shown in Chrome: ${inView().textContent.slice(0, 300)}`);
});
