// A PaymentIntent is only cancelable before it confirms, so the card Cancel is inert while a confirm is in flight.
import { mock } from "node:test";
import assert from "node:assert/strict";
import { headingOf, installPage, inViewOf, primaryOf, screenOf, settle, timedTest, until } from "./boot.js";
import { MemStorage, StubElement } from "./stub-dom.js";
import { CANCEL_INVOICE } from "../src/screens.js";

const cardCancelTest = timedTest(5000);
const NOW = Date.parse("2026-08-28T12:00:00Z");
const PUBLISHABLE_KEY = "pk_test_card_cancel";
const CLIENT_SECRET = "cs_test_cancel";

mock.timers.enable({ apis: ["setTimeout", "Date"], now: NOW });
const storage = new MemStorage();
const page = installPage({ storage });
const { app } = page;

const keyMeta = new StubElement("meta");
keyMeta.setAttribute("id", "stripe-publishable-key");
keyMeta.setAttribute("name", "stripe-publishable-key");
keyMeta.setAttribute("content", PUBLISHABLE_KEY);
page.document.byId.set("stripe-publishable-key", keyMeta);

// The confirm is released by hand so the in-flight window stays open long enough to press Cancel inside it.
let releaseConfirm: (() => void) | null = null;
(globalThis as unknown as { window: Record<string, unknown> }).window.Stripe = () => ({
  elements: () => ({ create: () => ({ mount: () => {}, destroy: () => {} }) }),
  confirmPayment: () => new Promise((res) => { releaseConfirm = () => res({ paymentIntent: { status: "succeeded" } }); }),
});

await import("../src/main.js");

const heading = (): string => headingOf(screenOf(app));
const inView = (): StubElement => inViewOf(app);
const cancelBtn = (): StubElement | undefined => screenOf(app).all("button").find((b) => b.textContent === CANCEL_INVOICE);
const payBtn = (): StubElement => screenOf(app).all("button.primary").find((b) => b.textContent.startsWith("Pay"))!;
const stripeTag = (): StubElement =>
  page.document.head.children.find(
    (c): c is StubElement => c instanceof StubElement && (c.getAttribute("src") ?? "").includes("js.stripe.com"),
  )!;
const storedOrder = (id: string): Record<string, unknown> | undefined =>
  (JSON.parse(storage.m.get("sb.orders.v1") ?? "[]") as Array<Record<string, unknown>>).find((o) => o.orderId === id);

async function walkToMountedCardForm(invoiceId: string): Promise<void> {
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("Supporter"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice").find((c) => c.textContent.startsWith("1 month"))!.click();
  primaryOf(inView())!.click();
  inView().all("button.choice.method").find((b) => b.textContent.startsWith("Card"))!.click();
  page.respondWith({ status: 200, body: {
    invoiceId, badgeType: "supporter", months: 1,
    amount: 700, currency: "usd", expiresAt: "2126-08-28T12:58:12Z", clientSecret: CLIENT_SECRET,
  } });
  inView().all("button.primary").find((b) => b.textContent.startsWith("Pay"))!.click();
  await until(() => heading() === "Pay by card", `the card form, not ${heading()}`);
  stripeTag().dispatch("load");
  await settle();
}

cardCancelTest("main: the card form offers Cancel, and a clean cancel marks the order canceled", async () => {
  await walkToMountedCardForm("inv_card_c1");
  assert.ok(cancelBtn() !== undefined, "the card form offers Cancel, the same control the crypto screen shows");

  page.confirmAnswer(true);
  page.respondWith({ status: 200, body: { status: "expired", amount: 700, currency: "usd" } });
  cancelBtn()!.click();
  await until(() => heading() === "Support SimpleX", "a clean cancel lands on the landing screen");

  assert.equal(storedOrder("inv_card_c1")?.canceled, true,
    "the order is marked canceled, so Your codes reads it as canceled, not expired");
});

cardCancelTest("main: the card Cancel is inert once a confirm is in flight", async () => {
  await walkToMountedCardForm("inv_card_c2");
  assert.ok(!payBtn().hasAttribute("disabled"), "the Element mounted, so Pay is live");
  payBtn().click();
  await settle();
  assert.equal(heading(), "Pay by card", "the form stays put while the confirm is in flight");

  const before = page.fetches.length;
  page.confirmAnswer(true);
  cancelBtn()!.click();
  await settle();
  assert.equal(heading(), "Pay by card", "canceling did nothing: a confirming intent is not cancelable");
  assert.equal(page.fetches.slice(before).filter((f) => f.url.endsWith("/cancel")).length, 0,
    "and no cancel request was sent");

  releaseConfirm!();
  await until(() => heading() === "Payment received", "the confirm resolves to the confirming screen");
});
