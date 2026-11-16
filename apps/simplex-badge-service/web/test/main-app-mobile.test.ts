// Opened by the mobile app: the code leaves by the simplexchat: link, and the page shows it only when asked.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, primaryOf, screenOf, settle, timedTest, until } from "./boot.js";
import type { StubElement } from "./stub-dom.js";

const appTest = timedTest(5000);
const NOW = Date.parse("2026-08-28T12:00:00Z");

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const page = installPage({ url: "/#/tier?app=true" });
const { app, history, location, storage, fetches } = page;
await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));
const inView = (): StubElement => inViewOf(app);
const stored = (): Record<string, unknown> =>
  (JSON.parse(storage.getItem("sb.orders.v1") ?? "[]") as Array<Record<string, unknown>>)[0] ?? {};

appTest("main: #/tier?app=true lands on Choose your badge, with the flag in the session and off the URL", () => {
  assert.equal(headingOf(inView()), "Choose your badge");
  assert.equal(history.url, "/#/tier", "the tail is dropped once read");
  assert.deepEqual(history.stack, ["/", "/#/tier"], "and Back still reaches the landing");
  assert.equal((JSON.parse(storage.getItem("sb.session.v1")!) as { app?: unknown }).app, "mobile");
});

appTest("main: the flag survives the steps and is stamped on the order, and no request carries it", async () => {
  inView().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
  primaryOf(inView())!.click();
  assert.equal(headingOf(inView()), "Check your order");

  page.respondWith({ status: 200, body: {
    invoiceId: "inv_mob", badgeType: "legend", months: 12, amount: 42000, currency: "usd",
    expiresAt: "2026-08-28T12:58:12Z", address: "48HqK2Xm", cryptoAmount: "1.482", cryptoCurrency: "xmr",
  } });
  inView().all("button.primary").find((b) => b.textContent.startsWith("Pay"))!.click();
  await until(() => heading() === "Send 1.482 XMR", `the payment screen, not ${heading()}`);

  assert.equal(location.search, "?order=inv_mob");
  assert.equal(stored().app, "mobile", "the order carries the flag across the ?order= round trip");
  assert.equal(storage.getItem("sb.session.v1"), null, "the session that carried it here is done");
  const created = fetches.find((f) => f.url === "/api/invoice")!;
  assert.deepEqual(Object.keys(JSON.parse(String(created.init!.body)) as object).sort(),
    ["codeHash", "method", "offerId", "priceId"], "the service is told nothing about the app");
  for (const f of fetches) {
    const sent = `${f.url} ${String(f.init?.body ?? "")}`;
    assert.ok(!/app=|"app"|mobile/.test(sent), `no request names the app: ${sent}`);
  }
});

appTest("main: once paid, the ending tries the link once and holds the code back", async () => {
  page.answerHeld({ status: 200, body: {
    status: "paid", badgeType: "legend", months: 12, amount: 42000, currency: "usd",
    paidInFull: true, settledAt: "2026-08-28T12:05:00Z",
  } }, "/api/invoice/inv_mob");
  await until(() => heading() === "Paid", `the return-to-app ending, not ${heading()}`);

  const code = stored().code as string;
  const link = `simplexchat:/badge/code/${code}`;
  const frames = screenOf(app).all("iframe");
  assert.equal(frames.length, 1, "one automatic attempt");
  assert.equal(frames[0]!.getAttribute("src"), link);
  assert.ok(frames[0]!.hasAttribute("hidden"), "a refusal stays inside a frame nobody sees");
  assert.equal(screenOf(app).serialize().split(code).length - 1, 1, "the frame's src is the only place the code appears");
  assert.ok(!screenOf(app).textContent.includes(code), "and none of it is text on the screen");
});

appTest("main: a repaint keeps the ending but does not fire the link again", async () => {
  page.setOffline(true);
  await settle();
  page.setOffline(false);
  await settle();
  assert.equal(heading(), "Paid");
  assert.equal(screenOf(app).all("iframe").length, 0);
});

appTest("main: with the automatic attempt refused, Return to SimpleX still navigates, off the page's history", () => {
  const before = [...history.stack];
  screenOf(app).all("button.primary").find((b) => b.textContent === "Return to SimpleX")!.click();
  assert.equal((location as { href?: string }).href, `simplexchat:/badge/code/${stored().code as string}`);
  assert.deepEqual(history.stack, before, "the page adds no history entry of its own");
  assert.equal(heading(), "Paid", "and the screen is still there to fall back on");
});

appTest("main: Show code reveals the stored code in place, keeps Return to SimpleX, and it stays revealed", async () => {
  screenOf(app).all("button.link").find((b) => b.textContent === "Show code")!.click();
  const code = stored().code as string;
  const revealed = (): void => {
    assert.equal(heading(), "Paid", "still the ending, not the plain code screen");
    assert.equal(screenOf(app).all("div.code")[0]!.textContent, code);
    assert.equal(screenOf(app).all("svg").filter((s) => s.getAttribute("class") === "qr").length, 0, "no QR");
    assert.ok(screenOf(app).all("button.primary").some((b) => b.textContent === "Return to SimpleX"));
  };
  revealed();
  assert.equal(screenOf(app).all("iframe").length, 0, "revealing does not fire the link again");

  page.clipboard.writes.length = 0;
  screenOf(app).all("button.primary").find((b) => b.textContent === "Copy code")!.click();
  await settle();
  assert.deepEqual(page.clipboard.writes, [code]);

  page.setOffline(true);
  await settle();
  page.setOffline(false);
  await settle();
  revealed();

  screenOf(app).all("button.primary").find((b) => b.textContent === "Return to SimpleX")!.click();
  assert.equal((location as { href?: string }).href, `simplexchat:/badge/code/${code}`, "the button still works once the code is out");
});

appTest("main: nothing the page wrote to history or storage outside the order holds the link", () => {
  for (const url of history.stack) assert.ok(!url.includes("simplexchat") && !url.includes("SB-"), url);
  for (const [key, value] of storage.m) {
    if (key !== "sb.orders.v1") assert.ok(!value.includes("SB-") && !value.includes("simplexchat"), key);
  }
});
