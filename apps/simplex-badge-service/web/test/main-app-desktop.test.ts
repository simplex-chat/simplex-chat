// Opened by the desktop app, which has no scheme handler: the code is shown as ever, and the app has Redeem code open beside it.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, screenOf, timedTest, until } from "./boot.js";
import type { StubElement } from "./stub-dom.js";

const appTest = timedTest(5000);
const NOW = Date.parse("2026-08-28T12:00:00Z");

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const page = installPage({ url: "/#/tier?app=desktop" });
const { app, history, storage, fetches } = page;
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));
const inView = (): StubElement => inViewOf(app);

appTest("main: #/tier?app=desktop lands on Choose your badge", () => {
  assert.equal(headingOf(inView()), "Choose your badge");
  assert.equal(history.url, "/#/tier");
});

appTest("main: paid from the desktop app, the code screen says Redeem code is already open", async () => {
  inView().all("button.choice").find((c) => c.textContent.startsWith("Supporter"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("1 month"))!.click();
  primaryOf(inView())!.click();
  page.respondWith({ status: 200, body: {
    invoiceId: "inv_desk", badgeType: "supporter", months: 1, amount: 700, currency: "usd",
    expiresAt: "2026-08-28T12:58:12Z", address: "48HqK2Xm", cryptoAmount: "0.02", cryptoCurrency: "xmr",
  } });
  inView().all("button.primary").find((b) => b.textContent.startsWith("Pay"))!.click();
  await until(() => heading() === "Send 0.02 XMR", `the payment screen, not ${heading()}`);
  for (const f of fetches) {
    const sent = `${f.url} ${String(f.init?.body ?? "")}`;
    assert.ok(!/app=|"app"|desktop/.test(sent), `no request names the app: ${sent}`);
  }

  page.answerHeld({ status: 200, body: {
    status: "paid", badgeType: "supporter", months: 1, amount: 700, currency: "usd",
    paidInFull: true, settledAt: "2026-08-28T12:05:00Z",
  } }, "/api/invoice/inv_desk");
  await until(() => heading() === "Paid. Here is your code.", `the code screen, not ${heading()}`);

  const code = (JSON.parse(storage.getItem("sb.orders.v1")!) as Array<{ code: string }>)[0]!.code;
  assert.equal(screenOf(app).all("div.code")[0]!.textContent, code, "the code, shown as it always was");
  assert.ok(screenOf(app).textContent.includes("The Redeem code screen is already open."));
  assert.ok(!screenOf(app).textContent.includes("Settings → Supporter perks"), "the one changed line");
  assert.equal(screenOf(app).all("iframe").length, 0, "desktop has no scheme to try");
});
