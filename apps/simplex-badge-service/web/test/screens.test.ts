import { mock } from "node:test";
import { timedTest } from "./boot.js";
import assert from "node:assert/strict";

const domTest = timedTest(2000);

import { StubElement, installDocument } from "./stub-dom.js";
import { historyRows, withoutCode } from "../src/order.js";
type UnpaidOrder = import("../src/order.js").UnpaidOrder;

const { clipboard } = installDocument();
const copied = clipboard.writes;

// The stub has to be in place before `screens.ts` is evaluated, because `el`
// reaches for `document` the moment it is called.
const screens = await import("../src/screens.js");
type Rec = import("../src/domain.js").OrderRecord;
type View = import("../src/api.js").InvoiceView;

function render(node: unknown): StubElement { return node as unknown as StubElement; }

/** Over the whole serialized subtree, not its text: an unpaid code smuggled into `title`, `data-*`,
 * `aria-label` or `href` is as visible to a buyer (a native tooltip) or a screen reader as one in a text
 * node, and `textContent` sees none. The display form and the raw body are both checked: either redeems. */
function assertNoCode(node: StubElement, where: string): void {
  const dump = node.serialize();
  for (const form of [HELD_CODE, HELD_CODE.replace(/-/g, ""), HELD_CODE.replace(/^SXB-/, ""), "SXB-"]) {
    assert.ok(!dump.includes(form), `${where} leaked a code (${form}) into: ${dump.slice(0, 400)}`);
  }
}

/**
 * An order that STILL CARRIES its code, handed to a screen whose parameter type
 * says it cannot. `OrderRecord` is assignable to `UnpaidOrder` (the field is
 */
function smuggled(over: Partial<Rec> = {}): UnpaidOrder {
  return record({ code: HELD_CODE, ...over });
}

/** And the stripped form, for asserting `flow.ts` does the stripping. */
function unpaidRecord(over: Partial<Rec> = {}): UnpaidOrder {
  return withoutCode(record(over));
}

function record(over: Partial<Rec> = {}): Rec {
  return {
    orderId: "inv_9f3a", badgeType: "legend", months: 12,
    createdAt: "2026-08-28T11:46:00Z", status: "open", ...over,
  };
}

const openXmr: View = {
  status: "open", badgeType: "legend", months: 12, amount: 42000,
  currency: "usd", expiresAt: "2026-08-28T12:58:12Z",
  address: "48HqK2XmVexampleAddress9fRtWc", cryptoAmount: "1.482", cryptoCurrency: "xmr",
};
const NOW = Date.parse("2026-08-28T12:00:00Z");
const HELD_CODE = "SXB-YDC8A-YGQTM-PUYZ9-2TUXP";

const noop = (): void => {};
const noopAsync = (): Promise<void> => Promise.resolve();

// ------------------------------------------------------------------ the landing screen to the order summary

domTest("screens: the landing screen carries the copy the mockups fix", () => {
  const p = render(screens.landing({ onStart: noop }));
  const text = p.textContent;
  for (const line of [
    "Support SimpleX",
    "SimpleX has no ads, no user accounts and nothing to sell.",
    "A supporter badge helps pay for the people who build it.",
    "Choose your level",
    "Already bought a code?",
    "Redeem it in the app: Settings, Supporter perks.",
    "The badge shows on your profile. Nothing renews by itself, and no account is created.",
  ]) assert.ok(text.includes(line), `the landing screen is missing: ${line}`);
  // The line to `#/codes` used to hang off the foot of this panel and off the code screen, for want of
  // anywhere else. The header's menu holds it now, on every screen rather than on two, and a landing page
  // whose last element is a second navigation link is the clutter the chrome exists to absorb.
  assert.equal(p.all("button.link").length, 0, "the history is the menu's, not the landing page's");
});

domTest("screens: the tier list carries the level copy, and Continue is disabled until one is chosen", () => {
  const tiers = [
    { priceId: "price_supporter", badgeType: "supporter", name: "Supporter", price: "$7 / month", features: ["2 GB files", "7 days storage"], disabled: false },
    { priceId: "price_legend", badgeType: "legend", name: "Legend", price: "$70 / month", features: ["5 GB files", "21 days storage"], disabled: false },
  ];
  const blank = render(screens.tiers({ tiers, selected: undefined, onSelect: noop, onContinue: noop, onBack: noop }));
  for (const line of ["Choose your level", "Bigger files, and longer for people to collect them.",
    "Supporter", "$7 / month", "2 GB files", "7 days storage", "Legend", "$70 / month", "5 GB files", "21 days storage", "← Back"]) {
    assert.ok(blank.textContent.includes(line), `the tier list is missing: ${line}`);
  }
  assert.ok(blank.all("button.primary")[0]!.hasAttribute("disabled"), "Continue must wait for an answer");

  let chosen = "";
  const picked = render(screens.tiers({ tiers, selected: "price_legend", onSelect: (id) => { chosen = id; }, onContinue: noop, onBack: noop }));
  assert.equal(picked.all("button.primary")[0]!.hasAttribute("disabled"), false);
  assert.deepEqual(picked.all("button.choice").map((c) => c.getAttribute("aria-pressed")), ["false", "true"]);
  picked.all("button.choice")[0]!.click();
  assert.equal(chosen, "price_supporter");
});

domTest("screens: a tier with no total is disabled and cannot be chosen", () => {
  let chosen = "";
  const p = render(screens.tiers({
    tiers: [{ priceId: "price_broken", badgeType: "legend", name: "Legend", price: "", features: [], disabled: true }],
    selected: undefined, onSelect: (id) => { chosen = id; }, onContinue: noop, onBack: noop,
  }));
  const card = p.all("button.choice")[0]!;
  assert.ok(card.hasAttribute("disabled"));
  card.click();
  assert.equal(chosen, "", "a disabled tier has no listener at all");
});

domTest("screens: the duration list prints the durations and their savings, unpriced where there is no total", () => {
  const p = render(screens.durations({
    durations: [
      { key: "", name: "1 month", price: "$70", disabled: false },
      { key: "offer_3m", name: "3 months", price: "$140", savingPercent: 33, disabled: false },
      { key: "offer_12m", name: "12 months", price: "$420", savingPercent: 50, disabled: false },
      { key: "offer_bad", name: "24 months", disabled: true },
    ],
    selected: "offer_12m", onSelect: noop, onContinue: noop, onBack: noop,
  }));
  for (const line of ["How long?", "Prepaid months. Nothing renews by itself.",
    "1 month", "$70", "3 months", "$140", "save 33%", "12 months", "$420", "save 50%"]) {
    assert.ok(p.textContent.includes(line), `the duration list is missing: ${line}`);
  }
  const bad = p.all("button.choice")[3]!;
  assert.ok(bad.hasAttribute("disabled"));
  assert.equal(bad.textContent, "24 months", "a duration with no total renders unpriced");
});

domTest("screens: the order summary is the summary and the method row, with the total on the Pay button", () => {
  const p = render(screens.orderSummary({ canKeepTheCode: true,
    badgeType: "legend", months: 12, total: "$420.00", selected: "xmr",
    onSelect: noop, onPay: noop, onBack: noop,
  }));
  for (const line of ["Check your order", "Level", "Legend", "Duration", "12 months", "Total", "$420.00",
    "Pay with", "Bitcoin", "Monero", "Card", "Pay $420.00 with Monero",
    "Card is handled by Stripe. Bitcoin and Monero are on-chain, through BTCPay.", "← Back"]) {
    assert.ok(p.textContent.includes(line), `the order summary is missing: ${line}`);
  }
});

domTest("screens: the provider-unavailable screen shows the unavailable method disabled rather than omitting it", () => {
  const p = render(screens.orderSummary({ canKeepTheCode: true,
    badgeType: "legend", months: 12, total: "$420.00", selected: "btc", unavailable: "xmr",
    onSelect: noop, onPay: noop, onBack: noop,
  }));
  assert.ok(p.textContent.includes("Monero is temporarily unavailable"));
  assert.ok(p.textContent.includes("Try another method, or come back later."));
  assert.ok(p.textContent.includes("Pay $420.00 with Bitcoin"), "the Pay button re-labels to the method now selected");
  const monero = p.all("button.choice").find((c) => c.textContent.startsWith("Monero"))!;
  assert.ok(monero.hasAttribute("disabled"));
  assert.ok(monero.textContent.includes("unavailable"));
  assert.equal(p.all("button.choice").length, 3, "the method is shown, not dropped");
});

domTest("screens: the order summary links to an order already waiting for payment", () => {
  const opened: string[] = [];
  const p = render(screens.orderSummary({ canKeepTheCode: true,
    badgeType: "legend", months: 12, total: "$420.00", selected: "xmr",
    openOrder: { orderId: "inv_9f3a", onOpen: (id) => opened.push(id) },
    onSelect: noop, onPay: noop, onBack: noop,
  }));
  const line = p.all("a.link")[0]!;
  assert.equal(line.textContent, "You have an order waiting for payment");
  assert.equal(line.getAttribute("href"), "?order=inv_9f3a", "the id is carried by the link, not the text");
  assert.ok(!p.textContent.includes("inv_9f3a"), "and never rendered as text");
  const plain = line.click();
  assert.equal(plain.defaultPrevented, true, "a plain click is handled in-document");
  assert.deepEqual(opened, ["inv_9f3a"]);
});

domTest("screens: the order summary withholds Pay while that order's card payment awaits confirmation", () => {
  // the give-up rule takes [ New invoice ] off the confirming screen so a confirmed card payment cannot be
  // duplicated; browser Back is the way round it, landing on an order summary that would
  // cheerfully create a second invoice. The create endpoint has no idempotency key.
  const opened: string[] = [];
  const paid: number[] = [];
  const p = render(screens.orderSummary({ canKeepTheCode: true,
    badgeType: "legend", months: 12, total: "$420.00", selected: "card",
    openOrder: { orderId: "inv_9f3a", awaitingCard: true, onOpen: (id) => opened.push(id) },
    onSelect: noop, onPay: () => paid.push(1), onBack: noop,
  }));
  assert.ok(p.textContent.includes(screens.AWAITING_CARD_TITLE), p.textContent);
  assert.equal(p.all("button.primary").length, 0, "no Pay button, not even a disabled one");
  assert.ok(!p.textContent.includes("Pay $420.00"));
  assert.equal(p.all("button.choice").length, 0, "and no method to choose for an order that cannot be started");
  assert.equal(paid.length, 0);

  // Nobody is stranded: the order is linked, and the way out is named.
  const line = p.all("a.link")[0]!;
  assert.equal(line.getAttribute("href"), "?order=inv_9f3a");
  assert.ok(!p.textContent.includes("inv_9f3a"), "the id is carried by the link, not the text");
  assert.ok(p.textContent.includes("When its invoice expires, a new one can be started there."));
  line.click();
  assert.deepEqual(opened, ["inv_9f3a"]);

  // The same order without the flag is the ordinary line, and Pay is live.
  const ordinary = render(screens.orderSummary({ canKeepTheCode: true,
    badgeType: "legend", months: 12, total: "$420.00", selected: "card",
    openOrder: { orderId: "inv_9f3a", onOpen: noop },
    onSelect: noop, onPay: () => paid.push(1), onBack: noop,
  }));
  assert.ok(!ordinary.textContent.includes(screens.AWAITING_CARD_TITLE));
  ordinary.all("button.primary")[0]!.click();
  assert.deepEqual(paid, [1], "an order merely waiting for payment does not withhold anything");
});

domTest("screens: an order link stays a real link for a modified click", () => {
  // the fallback store is an in-memory Map, so a full navigation would
  // destroy every record, but a buyer asking for a new tab must still get one.
  const opened: string[] = [];
  const p = render(screens.purchaseHistory({ keepsNewCodes: true,
    rows: historyRows([record({ orderId: "inv_x", status: "open" })]),
    onOpen: (id) => opened.push(id), onStart: noop,
  }));
  const link = p.all("a.secondary")[0]!;
  for (const modifier of [{ metaKey: true }, { ctrlKey: true }, { shiftKey: true }, { altKey: true }, { button: 1 }]) {
    const e = link.click(modifier);
    assert.equal(e.defaultPrevented, false, `${JSON.stringify(modifier)} must stay a real navigation`);
  }
  assert.deepEqual(opened, [], "and must not also route in-document");
  assert.equal(link.click().defaultPrevented, true);
  assert.deepEqual(opened, ["inv_x"]);
});

domTest("screens: the catalog-changed screen has no Back, because [ Start again ] replaces the entry", () => {
  let restarted = false;
  const p = render(screens.catalogChanged(() => { restarted = true; }));
  for (const line of ["These prices have changed", "Start again with the current prices",
    "The badge you chose was repriced while you were deciding.", "Nothing was charged."]) {
    assert.ok(p.textContent.includes(line), `the catalog-changed screen is missing: ${line}`);
  }
  assert.equal(p.all("button.back").length, 0, "there is nothing behind the catalog-changed screen");
  p.all("button.primary")[0]!.click();
  assert.equal(restarted, true);
});

domTest("screens: the rate-limited screen disables Pay and counts the Retry-After seconds down", () => {
  // Mocked, so a `stop()` that stops nothing cannot leave a 46-second interval
  // holding the process open and turn a red test into a 48-second suite.
  mock.timers.enable({ apis: ["setInterval"] });
  let expired = false;
  const { node, stop } = screens.rateLimited(
    { total: "$420.00", method: "xmr", seconds: 46, onBack: noop },
    () => { expired = true; },
  );
  const p = render(node);
  try {
    assert.ok(p.textContent.includes("Too many attempts"));
    assert.ok(p.textContent.includes("Try again in 46 seconds"));
    assert.ok(p.textContent.includes("The Pay button is disabled until then."));
    const pay = p.all("button.primary")[0]!;
    assert.equal(pay.textContent, "Pay $420.00 with Monero");
    assert.ok(pay.hasAttribute("disabled"), "the button is disabled for exactly Retry-After seconds");
    assert.equal(expired, false);
  } finally {
    stop();
    mock.timers.reset();
  }
});

// ------------------------------------------------------------- the waiting

/** The payment screen's countdown runs on a timer, stopped the moment the node is rendered: nothing in these
 * assertions reads a second tick, and a live interval would hold the test process open after it returned. */
function renderAwaitingPayment(o: Parameters<typeof screens.awaitingPayment>[0]) {
  const built = screens.awaitingPayment(o);
  built.stop();
  return render(built.node);
}

domTest("screens: a discounted duration strikes the price it is a discount from", () => {
  // "save 33%" is a percentage off a figure the buyer cannot otherwise see.
  const p = render(screens.durations({
    durations: [
      { key: "1m", name: "1 month", price: "$70", disabled: false },
      { key: "offer_3m", name: "3 months", price: "$140", wasPrice: "$210", savingPercent: 33, disabled: false },
    ],
    selected: undefined, onSelect: noop, onContinue: noop, onBack: noop,
  }));
  const cards = p.all("button.choice");
  assert.equal(cards[1]!.all("s.was")[0]?.textContent, "$210", "the gross is struck through");
  assert.ok(cards[1]!.textContent.includes("$140"), "beside the amount actually charged");
  assert.ok(cards[1]!.textContent.includes("save 33%"));
  // An undiscounted term has nothing to strike, and must not invent one.
  assert.equal(cards[0]!.all("s.was").length, 0, "one month is not a discount off itself");
});

domTest("screens: the payment screen carries the address, the held rate and the reference — and never a code", () => {
  const p = renderAwaitingPayment({
    order: smuggled(), invoice: openXmr, method: "xmr",
    nowMs: NOW, resumed: false, onNewInvoice: noop, onCancel: noopAsync,
  });
  for (const line of ["Send 1.482 XMR", "$420.00 — this rate is held for 58:12", "Monero address",
    "48HqK2XmVexampleAddress9fRtWc", "Waiting for the payment to confirm", "Reference", "inv_9f3a",
    "Bookmark this page — the address and the countdown both live on this URL."]) {
    assert.ok(p.textContent.includes(line), `the payment screen is missing: ${line}`);
  }
  assertNoCode(p, "awaitingPayment");
  assert.ok(!p.textContent.includes("New invoice"), "[ New invoice ] belongs to a resumed screen only");
});

domTest("screens: the payment screen's held-rate countdown TICKS, rather than freezing until a reload", () => {
  // The figure claims a rate is held for a stated time. Rendered once and left
  // alone it keeps claiming the same time however long the buyer sits there,
  // and only a reload corrects it, so the one number on this screen that is
  // about to run out is the one that looks like it never does.
  mock.timers.enable({ apis: ["setInterval"] });
  try {
    let clock = NOW;
    const built = screens.awaitingPayment({
      order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: clock,
      now: () => clock, resumed: false, onNewInvoice: noop, onCancel: noopAsync,
    });
    const p = render(built.node);
    const rate = (): string => p.all("p.rate")[0]!.textContent;
    assert.equal(rate(), "$420.00 — this rate is held for 58:12");

    clock += 1000;
    mock.timers.tick(1000);
    assert.equal(rate(), "$420.00 — this rate is held for 58:11", "one second later, one second less");

    clock += 60_000;
    mock.timers.tick(1000);
    assert.equal(rate(), "$420.00 — this rate is held for 57:11",
      "the figure is re-read from expiresAt, not decremented, so a throttled tab cannot drift");

    // At zero it settles on the replacement and stops, rather than counting
    // into negative time or announcing an expiry the browser decided itself.
    clock += 60 * 60_000;
    mock.timers.tick(1000);
    assert.equal(rate(), "Checking with the payment network");
    mock.timers.tick(60_000);
    assert.equal(rate(), "Checking with the payment network", "the interval was cleared, not left running");
    built.stop();
  } finally {
    mock.timers.reset();
  }
});

domTest("screens: the payment screen states what it is doing outside the fields, not as one of them", () => {
  // Boxed between the address and the reference, the status took the same card
  // shape as the two facts either side of it and read as a third fact about the
  // payment. It is what the page is doing, so it sits under both columns.
  const p = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: NOW, resumed: false, onNewInvoice: noop, onCancel: noopAsync,
  });
  const details = p.all("div.details")[0]!;
  assert.equal(details.all("p.awaiting").length, 0, "the status is not stacked with the fields");
  assert.equal(p.all("p.awaiting").length, 1, "it is on the panel, under both columns");
  assert.ok(p.all("p.awaiting")[0]!.textContent.includes("Waiting for the payment to confirm"));
  // The address and the reference, and nothing else pretending to be a field.
  assert.ok(details.textContent.includes("48HqK2XmVexampleAddress9fRtWc"));
  assert.ok(details.textContent.includes("inv_9f3a"));
  assert.ok(!details.textContent.includes("Waiting for the payment"));
});

domTest("screens: the offline line sits beside the status, not among the fields", () => {
  const p = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: NOW,
    resumed: false, offline: true, onNewInvoice: noop, onCancel: noopAsync,
  });
  assert.equal(p.all("div.details")[0]!.all("p.offline").length, 0);
  assert.ok(p.textContent.includes(screens.OFFLINE_NOTE), "and it is still said");
});

domTest("screens: the payment screen replaces the countdown at zero rather than expiring on its own clock", () => {
  const after = Date.parse("2026-08-28T13:30:00Z");
  const p = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: after, resumed: false, onNewInvoice: noop, onCancel: noopAsync,
  });
  assert.ok(p.textContent.includes("Checking with the payment network"));
  assert.ok(!p.textContent.includes("this rate is held for"));
  assert.ok(!p.textContent.includes("expired"), "expiry comes from the server, never from here");
});

domTest("screens: a resumed payment screen says how long ago it started and offers [ New invoice ]", () => {
  let fresh = false;
  const p = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: NOW, resumed: true,
    onNewInvoice: () => { fresh = true; }, onCancel: noopAsync,
  });
  assert.ok(p.textContent.includes("Started 14 minutes ago."));
  const button = p.all("button.secondary").find((b) => b.textContent === "New invoice")!;
  button.click();
  assert.equal(fresh, true);
});

domTest("screens: the payment screen's Copy button puts the address on the clipboard, never the code", () => {
  copied.length = 0;
  const p = renderAwaitingPayment({
    order: smuggled(), invoice: openXmr, method: "xmr",
    nowMs: NOW, resumed: false, onNewInvoice: noop, onCancel: noopAsync,
  });
  for (const b of p.all("button.secondary")) b.click();
  assert.deepEqual(copied, ["1.482", "48HqK2XmVexampleAddress9fRtWc"]);
});

function cancelButton(p: StubElement) {
  return p.all("button").find((b) => b.textContent === screens.CANCEL_INVOICE
    || b.textContent === screens.CANCEL_PENDING)!;
}

domTest("screens: the payment screen's cancel is a quiet danger link, not a second block button", () => {
  const p = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: NOW, resumed: true,
    onNewInvoice: noop, onCancel: noopAsync,
  });
  const cancel = cancelButton(p);
  const classes = cancel.getAttribute("class")!.split(" ");
  assert.ok(classes.includes("link"), cancel.getAttribute("class") ?? "");
  assert.ok(classes.includes("danger"), cancel.getAttribute("class") ?? "");
  // the resumed screen already ends in a full-width [ New invoice ]; a second block
  // control beside it is what this screen must not become
  const blocks = p.all("button.secondary")
    .filter((b) => !b.getAttribute("class")!.split(" ").includes("inline"));
  assert.deepEqual(blocks.map((b) => b.textContent), [screens.NEW_INVOICE]);
  // the confirmation names what cancelling costs, since a payment sent afterwards is gone
  assert.ok(screens.CANCEL_CONFIRM.includes("stops accepting payment"), screens.CANCEL_CONFIRM);
});

domTest("screens: a cancel in flight cannot be sent twice, and says so when it fails", async () => {
  let calls = 0;
  let reject: (e: Error) => void = () => {};
  const p = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: NOW, resumed: false,
    onNewInvoice: noop,
    onCancel: () => {
      calls += 1;
      return new Promise<void>((_, rj) => { reject = rj; });
    },
  });
  const cancel = cancelButton(p);
  cancel.click();
  assert.equal(calls, 1);
  assert.equal(cancel.textContent, screens.CANCEL_PENDING);
  assert.ok(cancel.hasAttribute("disabled"));
  // a second click while the first is still open would send a second POST
  cancel.click();
  assert.equal(calls, 1);

  reject(new Error("provider down"));
  await new Promise((r) => setImmediate(r));
  assert.equal(cancel.textContent, screens.CANCEL_INVOICE);
  assert.equal(cancel.hasAttribute("disabled"), false, "a failed cancel can be retried");
  assert.ok(p.textContent.includes(screens.CANCEL_FAILED), p.textContent);
});

domTest("screens: the payment screen renders no development note, whatever the origin", () => {
  const p = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: NOW, resumed: false,
    onNewInvoice: noop, onCancel: noopAsync,
  });
  assert.equal(p.all("div.warn").length, 0);
  assert.equal(p.all("div.command").length, 0);
  for (const leak of ["Development stand-in", "control/settle", "curl", "BTCPay", "mock"]) {
    assert.ok(!p.textContent.includes(leak), `the payment screen leaked "${leak}"`);
  }
});

domTest("screens: the payment screen offers the amount and a wallet link, both carrying the amount", () => {
  copied.length = 0;
  const p = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: NOW, resumed: false,
    onNewInvoice: noop, onCancel: noopAsync,
  });
  const uri = "monero:48HqK2XmVexampleAddress9fRtWc?tx_amount=1.482";
  const link = p.all("a.wallet-link")[0];
  assert.ok(link !== undefined, "the payment screen has no wallet link");
  assert.equal(link.getAttribute("href"), uri);
  // .copy-line is absolutely positioned inside a .field.copyable; used here it anchored to
  // the page and rendered under the header's menu button
  const line = p.all("p.wallet-line")[0];
  assert.ok(line !== undefined, "the wallet link needs its own in-flow line");
  assert.equal(p.all("p.copy-line").filter((n) => n.all("a.wallet-link").length > 0).length, 0);
  assert.ok(p.all("div.qr-wrap")[0]!.all("p.wallet-line").length === 1,
    "the link belongs under the symbol it opens");
  // the amount is copyable on its own: a wallet that takes neither the QR nor the
  // link would otherwise have it retyped from the heading
  assert.ok(p.textContent.includes("Amount in XMR"), p.textContent);
  for (const b of p.all("button.secondary")) b.click();
  assert.deepEqual(copied, ["1.482", "48HqK2XmVexampleAddress9fRtWc"]);
});

domTest("screens: the confirming screen waits, and promises not to expire while it does", () => {
  const p = render(screens.awaitingConfirmation({ invoice: undefined, method: undefined, order: smuggled(), gaveUp: false, onCheckAgain: noop }));
  for (const line of ["Payment received", "Waiting for the card network to confirm.", "Still processing",
    "This usually takes a few seconds. The page updates itself.",
    screens.KEEPS_WAITING]) {
    assert.ok(p.textContent.includes(line), `the confirming screen is missing: ${line}`);
  }
  // the old line said we stop waiting after 15 minutes, which stopped being true when a
  // funded invoice became exempt from the expiry sweep
  assert.ok(!p.textContent.includes("we stop waiting"), p.textContent);
  assertNoCode(p, "awaitingConfirmation");
});

domTest("screens: an underpaid payment screen keeps the address and says what is still owed", () => {
  const part = { ...openXmr, amountPaid: 21000, cryptoAmountPaid: "0.741" };
  const p = renderAwaitingPayment({
    order: unpaidRecord(), invoice: part, method: "xmr", nowMs: NOW, resumed: false,
    onNewInvoice: noop, onCancel: noopAsync,
  });
  assert.ok(p.textContent.includes(screens.PART_PAID_TITLE), p.textContent);
  assert.ok(p.textContent.includes("We have seen 0.741 XMR of 1.482 XMR."), p.textContent);
  // the address is the whole point: without it the rest cannot be sent
  assert.ok(p.textContent.includes("48HqK2XmVexampleAddress9fRtWc"), p.textContent);
  assert.equal(p.all("div.warn").length, 1);
});

domTest("screens: the confirming screen echoes what arrived and what settlement needs", () => {
  const paid = { ...openXmr, amountPaid: 42000, cryptoAmountPaid: "1.482", requiredConfirmations: 2 };
  const p = render(screens.awaitingConfirmation({
    order: smuggled(), invoice: paid, method: "xmr", gaveUp: false, onCheckAgain: noop,
  }));
  assert.ok(p.textContent.includes("Received"), p.textContent);
  assert.ok(p.textContent.includes("1.482 XMR"), p.textContent);
  assert.ok(p.textContent.includes("This is settled once the payment has 2 confirmations on the Monero blockchain."), p.textContent);

  // one confirmation is singular, and zero-conf says nothing rather than "0 confirmations"
  const one = render(screens.awaitingConfirmation({
    order: smuggled(), invoice: { ...paid, requiredConfirmations: 1 }, method: "btc", gaveUp: false, onCheckAgain: noop,
  }));
  assert.ok(one.textContent.includes("has 1 confirmation on the Bitcoin blockchain"), one.textContent);
  const zero = render(screens.awaitingConfirmation({
    order: smuggled(), invoice: { ...paid, requiredConfirmations: 0 }, method: "btc", gaveUp: false, onCheckAgain: noop,
  }));
  assert.ok(!zero.textContent.includes("settled once"), zero.textContent);
});

domTest("screens: the confirming screen names the network it is waiting on, and shows the pulse", () => {
  for (const [method, confirming, wait] of [
    ["btc", "Waiting for the Bitcoin network to confirm.", "usually about ten minutes"],
    ["xmr", "Waiting for the Monero network to confirm.", "usually a couple of minutes"],
  ] as const) {
    const p = render(screens.awaitingConfirmation({ order: smuggled(), invoice: undefined, method, gaveUp: false, onCheckAgain: noop }));
    for (const line of ["Payment received", confirming, "Still processing", wait]) {
      assert.ok(p.textContent.includes(line), `the confirming screen/${method} is missing: ${line}`);
    }
    // the card wait would be a lie about a chain confirmation
    assert.ok(!p.textContent.includes("card network"), p.textContent);
    assert.equal(p.all("span.pulse").length, 1, "the processing state has no pulse");
    // nothing to pay into: a second send here is a second payment
    for (const leak of ["Copy", "Open in wallet", "rate is held"]) {
      assert.ok(!p.textContent.includes(leak), `the confirming screen/${method} leaked "${leak}"`);
    }
    assert.ok(p.textContent.includes("inv_9f3a"), "the reference is the only thing to quote");
    assertNoCode(p, `the confirming screen/${method}`);
  }
});

domTest("screens: the confirming screen's give-up screen offers [ Check again ] and nothing that charges again", () => {
  let checked = false;
  const p = render(screens.awaitingConfirmation({ invoice: undefined, method: undefined,
    order: smuggled(), gaveUp: true, onCheckAgain: () => { checked = true; },
  }));
  // the give-up rule spells out this branch's controls. `actions.confirm()` returned
  // success here, so a new invoice risks a second charge that the create endpoint cannot
  // deduplicate and a later change defers any remedy for.
  assert.equal(p.all("button").filter((b) => b.textContent === "New invoice").length, 0,
    "the give-up screen must not offer a control that starts a second charge");
  for (const line of ["This is taking longer than expected",
    "The payment has not been confirmed. This page keeps working: come back to it later, or quote the reference below.",
    "Reference", "inv_9f3a", "Check again"]) {
    assert.ok(p.textContent.includes(line), `the confirming screen (gave up) is missing: ${line}`);
  }
  assertNoCode(p, "the confirming screen (gave up)");
  p.all("button.primary")[0]!.click();
  assert.equal(checked, true);
});

domTest("screens: the closed-window screen's part-paid variant reads cryptoAmountPaid, and shows no code or order URL", () => {
  const p = render(screens.windowClosed({
    order: smuggled({ status: "expired" }),
    invoice: { ...openXmr, status: "expired", amountPaid: 21000, cryptoAmountPaid: "0.734" },
    onNewInvoice: noop,
  }));
  for (const line of ["This invoice expired", "0.734 XMR arrived, which is not the full amount",
    "The rate window has closed, so the shortfall is no longer meaningful.",
    "Quote the reference below and we will sort it out.", "inv_9f3a", screens.NEW_INVOICE]) {
    assert.ok(p.textContent.includes(line), `the closed-window screen is missing: ${line}`);
  }
  assertNoCode(p, "windowClosed");
  // the bare id is the reference the buyer quotes; the URL it belongs to is still not printed
  assert.ok(!p.textContent.includes("?order="), "and never the ?order= URL");
});

domTest("screens: a payment that arrived late and in full is not called short", () => {
  // BTCPay reports a late payment as expired, never as paid in full, so the figures are the
  // only thing that can say whether it was short. Telling a buyer who paid in full that they
  // underpaid is the one wrong thing this screen can say.
  const p = render(screens.windowClosed({
    order: smuggled({ status: "expired" }),
    invoice: { ...openXmr, status: "expired", amountPaid: 42000, cryptoAmountPaid: "1.482" },
    onNewInvoice: noop,
  }));
  assert.ok(!p.textContent.includes("not the full amount"), p.textContent);
  assert.ok(p.textContent.includes("1.482 XMR arrived after the window closed"), p.textContent);
  assert.ok(p.textContent.includes("inv_9f3a"), "and the reference support works from");
});

domTest("screens: the provider's verdict decides the expired screen, not the figures", () => {
  // The tolerance case: 41900 of 42000 arrived and BTCPay called it paid in full. The service
  // then refuses to cancel the invoice as funded, so telling the buyer they underpaid is both
  // wrong and contradicted by what happens if they try anything.
  const tolerated = render(screens.windowClosed({
    order: smuggled({ status: "expired" }),
    invoice: { ...openXmr, status: "expired", amountPaid: 41900, cryptoAmountPaid: "1.4785", paidInFull: true },
    onNewInvoice: noop,
  }));
  assert.ok(!tolerated.textContent.includes("not the full amount"), tolerated.textContent);
  assert.ok(tolerated.textContent.includes("1.4785 XMR arrived after the window closed"), tolerated.textContent);

  // And the Monero shape: the verdict arrives with the figures still zero, which the service
  // records as a payment. "Nothing was received" would be the one wrong thing to say.
  const verdictOnly = render(screens.windowClosed({
    order: smuggled({ status: "expired" }),
    invoice: { ...openXmr, status: "expired", paidInFull: true },
    onNewInvoice: noop,
  }));
  assert.ok(!verdictOnly.textContent.includes("Nothing was received"), verdictOnly.textContent);
  assert.ok(verdictOnly.textContent.includes("inv_9f3a"), "and the reference support works from");
});

domTest("screens: a payment worth less than a minor unit is still a payment", () => {
  // BTCPay reports the crypto figure whatever it is worth, and `paymentHolds` counts it: the
  // service will not cancel or sweep this invoice. "Nothing was received" would contradict it,
  // and the else branch does not even give the buyer the reference to quote.
  const dust = render(screens.windowClosed({
    order: smuggled({ status: "expired" }),
    invoice: { ...openXmr, status: "expired", amountPaid: 0, cryptoAmountPaid: "0.00000001", paidInFull: false },
    onNewInvoice: noop,
  }));
  assert.ok(!dust.textContent.includes("Nothing was received"), dust.textContent);
  assert.ok(dust.textContent.includes("0.00000001 XMR arrived, which is not the full amount"), dust.textContent);
  assert.ok(dust.textContent.includes("inv_9f3a"), "with the reference, which is the actionable part");
});

domTest("screens: the closed-window screen prints no fiat figure it cannot stand behind", () => {
  // The fiat fallback this replaces printed `$300.00 of $0.00 arrived` and `$` over a EUR invoice: the read
  // endpoint sends `amount` and `currency` only when the browser may lack them, and `applyView` clears the
  // stored pair on expiry. A part-payment with no crypto figure says so in words, above the reference support uses.
  const p = render(screens.windowClosed({
    order: unpaidRecord({ status: "expired" }),
    invoice: { status: "expired", amountPaid: 30000 },
    onNewInvoice: noop,
  }));
  assert.ok(p.textContent.includes("A payment arrived, which is not the full amount"), p.textContent);
  assert.ok(p.textContent.includes("inv_9f3a"), "with the reference, which is the actionable part");
  assert.ok(!p.textContent.includes("$0.00"), "and never a total the response did not carry");
  assert.ok(!p.textContent.includes("$"), "nor a currency symbol nothing named");

  // The same body with a EUR pair present: still no invented dollar figure.
  const eur = render(screens.windowClosed({
    order: unpaidRecord({ status: "expired" }),
    invoice: { status: "expired", amount: 42000, amountPaid: 30000, currency: "eur" },
    onNewInvoice: noop,
  }));
  assert.ok(!eur.textContent.includes("$"), eur.textContent);
});

domTest("screens: the closed-window screen's nothing-received variant is the plain one", () => {
  const p = render(screens.windowClosed({
    order: unpaidRecord({ status: "expired" }),
    invoice: { status: "expired", amount: 42000, currency: "usd" },
    onNewInvoice: noop,
  }));
  assert.ok(p.textContent.includes("Nothing was received, and nothing was charged."));
  assert.ok(!p.textContent.includes("arrived, which is not the full amount"));
});

// ------------------------------------------------------------ the code, the history list

domTest("screens: the code screen is the one screen that prints a code, with the warning", () => {
  copied.length = 0;
  const p = render(screens.codeIssued({ code: HELD_CODE, savedLocally: true }));
  for (const line of ["Paid. Here is your code.", HELD_CODE, "Copy code", "Redeem it in the app",
    "Settings → Supporter perks → Redeem code", "This is the only copy.",
    // The mockup's own line. The clause that used to be appended to it, "not
    // in any account, and not on our side", restated the sentence it hung off.
    "Saved in this browser and nowhere else.",
    "Anyone using this browser can read it, and clearing the browser loses it."]) {
    assert.ok(p.textContent.includes(line), `the code screen is missing: ${line}`);
  }
  p.all("button.primary")[0]!.click();
  assert.deepEqual(copied, [HELD_CODE]);
});

domTest("screens: the code screen drops its saved-copy clause where the local write failed", () => {
  const p = render(screens.codeIssued({ code: HELD_CODE, savedLocally: false }));
  assert.ok(p.textContent.includes("This code could not be saved in this browser."));
  assert.ok(p.textContent.includes("Copy it now. It is shown here and nowhere else."));
  assert.ok(!p.textContent.includes("Saved in this browser"), "the page must not claim a saved copy");
  assert.ok(p.textContent.includes(HELD_CODE), "the code is still shown — it exists nowhere else");
});

domTest("screens: the paid-no-code screen names the badge and the reference, and has nothing else to offer", () => {
  const p = render(screens.paidNoCode({ order: unpaidRecord({ status: "paid" }), settledAt: "2026-08-24T09:00:00Z" }));
  for (const line of ["This code is not on this device",
    "The code was generated in the browser it was bought in, and is not stored anywhere else.",
    "Quote the reference below and we will sort it out.", "Legend, 12 months", "inv_9f3a"]) {
    assert.ok(p.textContent.includes(line), `the paid-no-code screen is missing: ${line}`);
  }
  assert.ok(p.textContent.includes("paid 24 August"));
});

domTest("screens: a history row states its status beside the title, not on a line of its own", () => {
  // The status is one short phrase. Given its own row it turned two lines of
  // fact into a three-line card, which is most of what made the list feel
  // empty and tall.
  const p = render(screens.purchaseHistory({ keepsNewCodes: true,
    rows: historyRows([
      record({ orderId: "a", status: "paid", code: HELD_CODE }),
      record({ orderId: "b", status: "open" }),
      record({ orderId: "c", status: "expired" }),
    ]),
    onOpen: noop, onStart: noop,
  }));
  const rows = p.all("li.entry");
  assert.ok(rows.length >= 2, "at least a paid row and an unpaid one");
  assert.equal(p.all("div.entry-foot").length, 0, "no row keeps a footer for the status alone");

  for (const row of rows) {
    const lines = row.all("div.entry-row");
    const status = row.all("span.status")[0];
    assert.ok(status, "every row states where it stands");
    // The status shares the first line with the title, and [ Open ] the second
    // with the meta. A row is two lines whatever it holds.
    assert.ok(lines[0]!.all("span.status").length === 1, "the status is on the title line");
    const open = row.all("a.secondary").find((a) => a.textContent === "Open");
    if (open !== undefined) {
      assert.equal(lines[1]?.all("a.secondary").length, 1, "[ Open ] rides the meta line");
    }
  }
});

domTest("screens: the history list prints a code only on a paid entry that holds one", () => {
  const entries: Rec[] = [
    record({ orderId: "a", status: "paid", code: HELD_CODE }),
    record({ orderId: "b", status: "open", code: "SXB-OPEN0-OPEN0-OPEN0-OPEN0" }),
    record({ orderId: "c", status: "expired", code: "SXB-EXPD0-EXPD0-EXPD0-EXPD0" }),
    record({ orderId: "d", status: "paid" }),
  ];
  const p = render(screens.purchaseHistory({ keepsNewCodes: true, rows: historyRows(entries), onOpen: noop, onStart: noop }));
  const rows = p.all("li.entry");
  assert.equal(rows.length, 4);
  assert.ok(rows[0]!.textContent.includes(HELD_CODE));
  assert.ok(rows[1]!.textContent.includes("waiting for payment"));
  assert.ok(rows[2]!.textContent.includes("this invoice expired"));
  assert.ok(rows[3]!.textContent.includes("paid, and the code was not saved here"));
  // The whole serialized row, so a code in an attribute is caught too.
  for (const [i, row] of rows.entries()) {
    if (i === 0) continue;
    assert.ok(!row.serialize().includes("SXB-"), `row ${i} leaked a code: ${row.serialize()}`);
  }
  for (const [i, row] of rows.entries()) {
    if (i === 0) continue;
    const link = row.all("a.secondary")[0]!;
    assert.equal(link.textContent, "Open", "every non-paid line keeps its link");
    assert.equal(link.getAttribute("href"), `?order=${entries[i]!.orderId}`,
      "[ Open ] is the only invoice id rendered, and it is a link");
  }
  // [ Forget everything on this device ] is an action on the device and not on
  // this list, and it is in the menu, which every screen has.
  assert.ok(!p.textContent.includes("Forget everything on this device"),
    "the list must not carry the control that empties it");
});

domTest("screens: a history list row is a receipt — badge, level, price, method and day", () => {
  const p = render(screens.purchaseHistory({ keepsNewCodes: true,
    rows: historyRows([record({
      orderId: "a", status: "paid", code: HELD_CODE,
      amount: 42000, currency: "usd", method: "xmr",
    })]),
    onOpen: noop, onStart: noop,
  }));
  const row = p.all("li.entry")[0]!;
  // The badge that was bought, in the tier's own gradient: the same artwork
  // the tier list's card carries, and decorative here as it is there.
  const art = row.all("svg.badge-art");
  assert.equal(art.length, 1, "the row draws the badge it is a receipt for");
  assert.equal(art[0]!.getAttribute("viewBox"), "8.25 8.25 300 399");
  assert.equal(art[0]!.getAttribute("aria-hidden"), "true");
  // The method, with the order summary's own mark beside it.
  const mark = row.all("svg.mark");
  assert.equal(mark.length, 1);
  assert.equal(mark[0]!.all("path")[0]!.getAttribute("fill"), "#FF6600", "Monero's orange");
  for (const line of ["Legend, 12 months", "Monero", "$420.00", "28 August 2026", "paid"]) {
    assert.ok(row.textContent.includes(line), `the row is missing: ${line}`);
  }
  // The year, which the paid-no-code screen's "paid 24 August" does not print: this list is the one
  // screen that can hold purchases from more than one of them.
  assert.equal(row.all("div.meta")[0]!.texts.join("|"), "Monero|$420.00|28 August 2026");
});

domTest("screens: the four the history list states are told apart by their own ground", () => {
  const rows = historyRows([
    record({ orderId: "a", status: "paid", code: HELD_CODE }),
    record({ orderId: "b", status: "paid" }),
    record({ orderId: "c", status: "open" }),
    record({ orderId: "d", status: "expired" }),
  ]);
  const drawn = render(screens.purchaseHistory({ keepsNewCodes: true, rows, onOpen: noop, onStart: noop })).all("li.entry");
  const status = (i: number): StubElement => drawn[i]!.all("span.status")[0]!;
  assert.deepEqual(drawn.map((_, i) => status(i).textContent),
    ["paid", "paid, and the code was not saved here", "waiting for payment", "this invoice expired"]);
  // the payment-URI rule splits the palette by what has happened: settled, waiting, and lost.
  // A paid order whose code is gone is a loss, and only one of the four is not.
  assert.deepEqual(drawn.map((_, i) => status(i).getAttribute("class")),
    ["status settled", "status lost", "status pending", "status lost"]);
});

domTest("screens: a row missing the method or the price shows what it has", () => {
  // Every record written before the store rules kept the method is this row, and
  // `orders()` validates `orderId` and `createdAt` and nothing else, so a
  // partial entry has to stay renderable rather than print `undefined` or
  // vanish from a list that is the only copy of what it names.
  const p = render(screens.purchaseHistory({ keepsNewCodes: true,
    rows: historyRows([
      record({ orderId: "old", status: "paid", code: HELD_CODE }),
      { orderId: "bare", badgeType: "", months: 0, createdAt: "not a date", status: "open" },
    ]),
    onOpen: noop, onStart: noop,
  }));
  const rows = p.all("li.entry");
  assert.equal(rows.length, 2, "neither entry may be dropped");

  const old = rows[0]!;
  assert.equal(old.all("svg.mark").length, 0, "no method, so no mark and no name for one");
  assert.equal(old.textContent.includes("undefined"), false, old.textContent);
  for (const line of ["Legend, 12 months", "28 August 2026", HELD_CODE]) {
    assert.ok(old.textContent.includes(line), `the row lost: ${line}`);
  }

  // Nothing but an id and an unreadable timestamp: no badge, no title, no meta
  // line at all, and still its state and its way back to the order.
  const bare = rows[1]!;
  assert.equal(bare.all("svg").length, 0);
  assert.equal(bare.all("div.meta").length, 0, "an empty meta line would draw an empty row of nothing");
  assert.equal(bare.all("div.name").length, 0);
  assert.equal(bare.textContent.includes("NaN") || bare.textContent.includes("Invalid"), false, bare.textContent);
  assert.ok(bare.textContent.includes("waiting for payment"));
  assert.equal(bare.all("a.secondary")[0]!.getAttribute("href"), "?order=bare");
});

domTest("screens: the history list's Copy is offered only on a paid entry, and copies that entry's code", () => {
  copied.length = 0;
  const p = render(screens.purchaseHistory({ keepsNewCodes: true,
    rows: historyRows([record({ orderId: "a", status: "paid", code: HELD_CODE }), record({ orderId: "b", status: "open", code: "SXB-OPEN0" })]),
    onOpen: noop, onStart: noop,
  }));
  const copies = p.all("li.entry").flatMap((r) => r.all("button.secondary").filter((b) => b.textContent === "Copy"));
  assert.equal(copies.length, 1);
  copies[0]!.click();
  assert.deepEqual(copied, [HELD_CODE]);
});

domTest("screens: an empty store reads 'Nothing bought on this device'", () => {
  const p = render(screens.purchaseHistory({ keepsNewCodes: true, rows: [], onOpen: noop, onStart: noop }));
  assert.ok(p.textContent.includes("Nothing bought on this device"));
  assert.ok(p.textContent.includes("Choose your level"));
});

// ------------------------------------------------------------- the unknown-order screen refusals

domTest("screens: the invoice failure says nothing was charged, and offers [ Try again ]", () => {
  let retried = false;
  const p = render(screens.invoiceFailure(() => { retried = true; }));
  for (const line of ["That did not go through", "The order was not created, and nothing was charged.",
    "If this happens again, get in touch.", "Try again"]) {
    assert.ok(p.textContent.includes(line), `the failure screen is missing: ${line}`);
  }
  p.all("button.primary")[0]!.click();
  assert.equal(retried, true);
});

domTest("screens: the unknown order distinguishes nothing, as the unknown-order screen requires", () => {
  const p = render(screens.unknownOrder(noop));
  assert.ok(p.textContent.includes("This link does not work"));
  assert.ok(p.textContent.includes("Check the address you were given, or start again."));
  assert.ok(!p.textContent.includes("inv_"), "nothing distinguishes an unknown id from a guess");
});

domTest("screens: an order with no payment details gives the reference AND a way out", () => {
  let checked = false;
  let fresh = false;
  const p = render(screens.detailsUnavailable({
    order: smuggled(),
    onCheckAgain: () => { checked = true; }, onNewInvoice: () => { fresh = true; },
  }));
  assert.ok(p.textContent.includes("The payment details are not available"));
  assert.ok(p.textContent.includes("inv_9f3a"));
  assertNoCode(p, "detailsUnavailable");
  p.all("button.primary")[0]!.click();
  assert.equal(checked, true);
  // the watch loop: this screen's only control used to take the reference away and leave
  // the buyer with no exit; it must offer one.
  const out = p.all("button.secondary").find((b) => b.textContent === "New invoice");
  assert.ok(out, "detailsUnavailable must offer [ New invoice ]");
  out.click();
  assert.equal(fresh, true);
});

domTest("screens: the mounted card form shows the summary and the reference, and no code", () => {
  const p = render(screens.cardForm({
    order: smuggled(),
    invoice: { status: "open", amount: 42000, currency: "usd", clientSecret: "cs_test_abc" },
    resumed: false, onNewInvoice: noop,
  }));
  assert.ok(p.textContent.includes("Legend"));
  assert.ok(p.textContent.includes("$420.00"));
  assert.ok(p.textContent.includes("inv_9f3a"));
  assert.equal(p.all("div.card-mount").length, 1, "the Payment Element mounts here");
  assertNoCode(p, "cardForm");
  assert.ok(!p.textContent.includes("cs_test_abc"), "the client secret is never rendered as text");
});

// ------------------------------------------------------- the invariant, again

domTest("screens: ACROSS EVERY UNPAID SCREEN, the code is absent from the WHOLE subtree", () => {
  const held = smuggled();
  const expired = smuggled({ status: "expired" });
  const unpaid: Array<[string, StubElement]> = [
    ["awaitingPayment", renderAwaitingPayment({ order: held, invoice: openXmr, method: "xmr", nowMs: NOW, resumed: true, onNewInvoice: noop, onCancel: noopAsync })],
    ["awaitingConfirmation", render(screens.awaitingConfirmation({ invoice: undefined, method: undefined, order: held, gaveUp: false, onCheckAgain: noop }))],
    ["the confirming screen/gaveUp", render(screens.awaitingConfirmation({ invoice: undefined, method: undefined, order: held, gaveUp: true, onCheckAgain: noop }))],
    ["the closed-window screen/part", render(screens.windowClosed({ order: expired, invoice: { ...openXmr, status: "expired", amountPaid: 21000, cryptoAmountPaid: "0.734" }, onNewInvoice: noop }))],
    ["the closed-window screen/none", render(screens.windowClosed({ order: expired, invoice: { status: "expired" }, onNewInvoice: noop }))],
    ["cardForm", render(screens.cardForm({ order: held, invoice: { status: "open", clientSecret: "cs" }, resumed: true, onNewInvoice: noop }))],
    ["detailsUnavailable", render(screens.detailsUnavailable({ order: held, onCheckAgain: noop, onNewInvoice: noop }))],
    ["the history list", render(screens.purchaseHistory({ keepsNewCodes: true, rows: historyRows([record({ code: HELD_CODE }), record({ status: "expired", code: HELD_CODE })]), onOpen: noop, onStart: noop }))],
  ];
  // Attributes included: a `title`, `data-*`, `aria-label` or `href` carrying
  // the code is a tooltip and a screen-reader announcement, not a hidden field.
  for (const [name, node] of unpaid) assertNoCode(node, name);
  // And the clipboard: nothing may put an unpaid code there either.
  copied.length = 0;
  for (const [, node] of unpaid) for (const b of node.all("button.secondary")) b.click();
  for (const [, node] of unpaid) for (const b of node.all("button.primary")) b.click();
  assert.ok(!copied.some((v) => v.includes("SXB-")), `an unpaid code reached the clipboard: ${copied.join(", ")}`);
});

domTest("screens: the code screen is the only screen whose subtree may contain the code", () => {
  const p = render(screens.codeIssued({ code: HELD_CODE, savedLocally: true }));
  assert.ok(p.serialize().includes(HELD_CODE), "the code screen must show it — it exists nowhere else");
});


// ------------------------------------------------------- the clipboard

domTest("screens: the code screen says so when the clipboard refuses — the code is the only copy", async () => {
  copied.length = 0;
  clipboard.fail = true;
  try {
    const p = render(screens.codeIssued({ code: HELD_CODE, savedLocally: true }));
    const copy = p.all("button.primary")[0]!;
    assert.equal(copy.textContent, "Copy code");
    copy.click();
    await new Promise((r) => setImmediate(r));
    assert.deepEqual(copied, [], "nothing was copied");
    const status = p.all("p.muted").find((n) => n.getAttribute("role") === "status")!;
    assert.equal(status.textContent, "Could not copy. Select it above and copy it by hand.",
      "a silent failure would leave the buyer believing they had a copy");
  } finally {
    clipboard.fail = false;
  }
});

domTest("screens: the code screen says so when there is no clipboard at all (an insecure origin)", () => {
  clipboard.absent = true;
  try {
    const p = render(screens.codeIssued({ code: HELD_CODE, savedLocally: true }));
    p.all("button.primary")[0]!.click();
    const status = p.all("p.muted").find((n) => n.getAttribute("role") === "status")!;
    assert.equal(status.textContent, "Could not copy. Select it above and copy it by hand.");
  } finally {
    clipboard.absent = false;
  }
});

domTest("screens: a successful copy confirms IN PLACE and adds no line, on the code screen and on the payment screen", async () => {
  // A confirmation appended below the control pushed everything under it down
  // and stayed there for the rest of the visit. The control says it instead,
  // where the buyer is already looking, and says it for two seconds.
  copied.length = 0;
  const six = render(screens.codeIssued({ code: HELD_CODE, savedLocally: true }));
  const codeButton = six.all("button.primary")[0]!;
  const before = six.serialize().length;
  codeButton.click();
  await new Promise((r) => setImmediate(r));
  assert.deepEqual(copied, [HELD_CODE]);
  assert.equal(codeButton.textContent, "Copied");
  assert.ok(codeButton.getAttribute("class")!.split(" ").includes("copied"), codeButton.getAttribute("class") ?? "");
  assert.equal(six.all("p.muted").find((n) => n.getAttribute("role") === "status")!.textContent, "",
    "the status line stays empty: it is for a FAILURE, which is not transient");
  assert.ok(six.serialize().length < before + "Copied.".length * 2,
    "nothing was added to the tree — the label was swapped");

  const five = renderAwaitingPayment({
    order: smuggled(), invoice: openXmr, method: "xmr", nowMs: NOW, resumed: false, onNewInvoice: noop, onCancel: noopAsync,
  });
  const addressButton = five.all("button.secondary")[1]!;
  addressButton.click();
  await new Promise((r) => setImmediate(r));
  assert.deepEqual(copied, [HELD_CODE, openXmr.address]);
  assert.equal(addressButton.textContent, "Copied");
});


domTest("screens: no confirming screen offers [ New invoice ] — waiting or given up", () => {
  // The design grouped the confirming screen with the payment screen and the closed-window screen, which is an over-generalisation:
  // those two say in their own copy that nothing was charged, and the confirming screen is the
  // one screen where `actions.confirm()` returned success. The create endpoint has no
  // idempotency key and the button abandons rather than cancels, so the
  // buyer would hold two live invoices and could be charged twice, at minute
  // 2 exactly as at minute 16. The way out is [ Check again ], the reference,
  // and the footer; when the invoice expires the closed-window screen offers a new one.
  for (const gaveUp of [false, true]) {
    const p = render(screens.awaitingConfirmation({ invoice: undefined, method: undefined, order: smuggled(), gaveUp, onCheckAgain: noop }));
    assert.equal(p.all("button").filter((b) => b.textContent === "New invoice").length, 0,
      `the confirming screen (gaveUp: ${gaveUp}) must offer nothing that starts a second charge`);
  }
  // Waiting, the screen updates itself and needs no control; when it stops
  // waiting it hands over the reference and [ Check again ], which is where a
  // buyer who needs a human goes.
  const waiting = render(screens.awaitingConfirmation({ invoice: undefined, method: undefined, order: smuggled(), gaveUp: false, onCheckAgain: noop }));
  assert.ok(waiting.textContent.includes("The page updates itself."));
  const stopped = render(screens.awaitingConfirmation({ invoice: undefined, method: undefined, order: smuggled(), gaveUp: true, onCheckAgain: noop }));
  assert.ok(stopped.textContent.includes("inv_9f3a"), "the reference is the way out instead");
  assert.ok(stopped.all("button.primary").some((b) => b.textContent === "Check again"));
});

domTest("screens: one second reads as one second on the first paint, not only on the tick", () => {
  // the service answers `max 1 (…)`, so a buyer who trips the limit in the last second of the
  // window lands on this screen with a 1 already in it
  mock.timers.enable({ apis: ["setInterval"] });
  const { node, stop } = screens.rateLimited({ total: "$420.00", method: "xmr", seconds: 1, onBack: noop }, noop);
  try {
    assert.equal(render(node).all("span.title")[0]!.textContent, "Try again in 1 second");
  } finally {
    stop();
    mock.timers.reset();
  }
});

domTest("screens: the rate-limited screen's countdown opts out of #app's live region", () => {
  mock.timers.enable({ apis: ["setInterval"] });
  const { node, stop } = screens.rateLimited({ total: "$420.00", method: "xmr", seconds: 9, onBack: noop }, noop);
  try {
    const line = render(node).all("span.title")[0]!;
    assert.equal(line.textContent, "Try again in 9 seconds");
    // The shell is aria-live="polite"; a figure that changes every second
    // would otherwise be announced every second.
    assert.equal(line.getAttribute("aria-live"), "off");
  } finally {
    stop();
    mock.timers.reset();
  }
});

domTest("screens: the rate-limited screen's stop() halts the countdown, so the timer cannot outlive the screen", () => {
  mock.timers.enable({ apis: ["setInterval"] });
  try {
    let expired = 0;
    const { node, stop } = screens.rateLimited(
      { total: "$420.00", method: "xmr", seconds: 3, onBack: noop },
      () => { expired += 1; },
    );
    const p = render(node);
    mock.timers.tick(1000);
    assert.ok(p.textContent.includes("Try again in 2 seconds"));
    stop();
    // Nothing after this may run: an interval left behind keeps the whole
    // process alive and mutates a panel that is no longer on screen.
    mock.timers.tick(60_000);
    assert.ok(p.textContent.includes("Try again in 2 seconds"), "the countdown must be frozen");
    assert.equal(expired, 0, "and the expiry callback must never fire");
  } finally {
    mock.timers.reset();
  }
});

domTest("screens: a disabled control does nothing when clicked", () => {
  // the tier list's Continue keeps its listener and relies on the disabled attribute, so
  // that attribute is the guard, not the absence of a handler.
  let advanced = false;
  const p = render(screens.tiers({
    tiers: [{ priceId: "p", badgeType: "legend", name: "Legend", price: "$70 / month", features: [], disabled: false }],
    selected: undefined, onSelect: noop, onContinue: () => { advanced = true; }, onBack: noop,
  }));
  const go = p.all("button.primary")[0]!;
  assert.ok(go.hasAttribute("disabled"));
  assert.equal(go.listenerCount("click"), 1, "the handler is still attached");
  go.click();
  assert.equal(advanced, false, "a disabled control must not activate");
});

// ------------------------------------------------------- the offline note

domTest("screens: the offline note goes beside the status on every screen still waiting", () => {
  const awaitingPayment = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: NOW, resumed: false,
    offline: true, onNewInvoice: noop, onCancel: noopAsync,
  });
  assert.ok(awaitingPayment.textContent.includes("Waiting for the payment to confirm"), "the status stays");
  assert.ok(awaitingPayment.textContent.includes(screens.OFFLINE_NOTE), "and the note is added to it");

  const awaitingConfirmation = render(screens.awaitingConfirmation({ invoice: undefined, method: undefined,
    order: unpaidRecord(), gaveUp: false, offline: true, onCheckAgain: noop,
  }));
  assert.ok(awaitingConfirmation.textContent.includes(screens.OFFLINE_NOTE));

  // the watch loop: `expired` keeps waiting, so a buyer who paid at the last second sees
  // the closed-window screen become the code screen, so the closed-window screen is a screen that is still checking.
  const windowClosed = render(screens.windowClosed({
    order: unpaidRecord({ status: "expired" }), invoice: { status: "expired" }, offline: true, onNewInvoice: noop,
  }));
  assert.ok(windowClosed.textContent.includes(screens.OFFLINE_NOTE));
});

domTest("screens: nothing says it will keep checking when it is not", () => {
  const online = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: NOW, resumed: false, onNewInvoice: noop, onCancel: noopAsync,
  });
  assert.ok(!online.textContent.includes(screens.OFFLINE_NOTE), "absent is online");

  // the give-up branch has stopped waiting: `offline` reaches it and it
  // still must not promise to keep checking.
  const gaveUp = render(screens.awaitingConfirmation({ invoice: undefined, method: undefined,
    order: unpaidRecord(), gaveUp: true, offline: true, onCheckAgain: noop,
  }));
  assert.ok(!gaveUp.textContent.includes(screens.OFFLINE_NOTE),
    "the loop has ended: this screen keeps checking nothing");

  // the code screen renders from the store and needs no network at all, so there is
  // nothing for it to be offline about.
  const codeIssued = render(screens.codeIssued({ code: HELD_CODE, savedLocally: true }));
  assert.ok(!codeIssued.textContent.includes(screens.OFFLINE_NOTE));
});

domTest("screens: a part-paid invoice asks for the remainder, everywhere it names a figure", () => {
  const full = renderAwaitingPayment({
    order: unpaidRecord(), invoice: openXmr, method: "xmr", nowMs: NOW,
    resumed: false, onNewInvoice: noop, onCancel: noopAsync,
  });
  assert.equal(full.all("svg.qr").length, 1, "an untouched invoice still scans");

  const part = renderAwaitingPayment({
    order: unpaidRecord(),
    invoice: { ...openXmr, cryptoAmountPaid: "0.741", cryptoAmountDue: "0.745", paidInFull: false },
    method: "xmr", nowMs: NOW, resumed: false, onNewInvoice: noop, onCancel: noopAsync,
  });
  // 1.482 invoiced, 0.741 seen, and the provider says 0.745 is still due - more than the
  // difference, because a network fee lands with the first partial payment. Every figure the
  // buyer can act on must be that 0.745, never the invoice's own 1.482.
  const text = part.textContent;
  assert.ok(text.includes("Send 0.745 XMR"), `the heading asks the provider figure: ${text.slice(0, 80)}`);
  assert.ok(text.includes("We have seen 0.741 XMR of 1.482 XMR"), "and still says what arrived");
  const link = part.all("a.wallet-link")[0]?.getAttribute("href") ?? "";
  assert.equal(link, `monero:${openXmr.address!}?tx_amount=0.745`,
    "the wallet link carries it too - 0.745, not the 0.741 a subtraction would give");
  assert.equal(part.all("svg.qr").length, 1, "and it is scannable again, now that the figure is right");
  assert.equal(part.all("p.awaiting")[0]!.textContent, "Waiting for the rest of the payment",
    "the status says what is actually being waited for, not that the invoice is confirming");
});



domTest("screens: the provider figure decides, and zero is never a figure to send", () => {
  const draw = (paid: string, due: string | undefined) => renderAwaitingPayment({
    order: unpaidRecord(),
    invoice: { ...openXmr, cryptoAmountPaid: paid, paidInFull: false, ...(due === undefined ? {} : { cryptoAmountDue: due }) },
    method: "xmr", nowMs: NOW, resumed: false, onNewInvoice: noop, onCancel: noopAsync,
  });

  // BTCPay reports a covered invoice as "0.00000000"; asking for zero is worse than asking
  // for nothing, and a QR encoding it is worse still
  const zero = draw("1.482", "0.00000000");
  assert.ok(!zero.textContent.includes("Send 0"), `never a zero to send: ${zero.textContent.slice(0, 80)}`);
  assert.ok(zero.textContent.includes(screens.PAID_IN_FULL_TITLE), "it says the amount has arrived");
  assert.ok(zero.textContent.includes("Nothing more is owed"), "and says so in the notice too");
  assert.equal(zero.all("p.awaiting")[0]!.textContent, "Waiting for the payment to confirm",
    "with nothing left to send, confirmation is what is being waited for");
  assert.ok(!zero.textContent.includes("Send the rest"), "asking for more of a covered invoice is the bug");
  assert.equal(zero.all("svg.qr").length, 0, "and offers nothing to scan");

  // an older record, or an answer that carried no figure: no claim either way, and no figure
  for (const [name, due] of [["absent", undefined], ["unreadable", "1,4"]] as const) {
    const unknown = draw("0.741", due);
    assert.ok(unknown.textContent.includes(screens.PART_PAID_TITLE), `${name}: stays part-paid`);
    assert.ok(!unknown.textContent.includes(screens.PAID_IN_FULL_TITLE), `${name}: claims nothing`);
    assert.equal(unknown.all("svg.qr").length, 0, `${name}: no figure, no scan`);
    assert.equal(unknown.all("p.awaiting")[0]!.textContent, "Waiting for the rest of the payment",
      `${name}: the notice asks for the rest, so the status cannot say the payment is confirming`);
  }

  // a field labelled "Amount in XMR" with nothing in it, beside a Copy that copies nothing
  for (const [name, screen] of [["covered", zero], ["unknown", draw("0.741", undefined)]] as const) {
    assert.ok(!screen.textContent.includes("Amount in"), `${name}: no amount field with no amount to put in it`);
  }
});

domTest("screens: the history list does not promise codes are kept where nothing is kept", () => {
  // the same browser the code screen was taught not to lie to: this list is the session's own
  // memory, and it goes when the page does
  const rows = historyRows([record({ orderId: "a", status: "paid", code: HELD_CODE })]);
  const kept = render(screens.purchaseHistory({ keepsNewCodes: true, rows, onOpen: noop, onStart: noop }));
  assert.ok(kept.textContent.includes("in this browser, and nowhere else"));

  const losing = render(screens.purchaseHistory({ keepsNewCodes: false, rows, onOpen: noop, onStart: noop }));
  assert.ok(!losing.textContent.includes("in this browser, and nowhere else"),
    `nothing is kept, so nothing may say it is: ${losing.textContent.slice(0, 160)}`);
  assert.ok(losing.textContent.includes("cannot save anything new"));
});

domTest("screens: the order summary warns before the money, not after", () => {
  // the code is the whole purchase and this browser holds the only copy, so learning that it
  // cannot be kept belongs before the Pay button, not on the screen that follows it
  const base = {
    badgeType: "legend", months: 12, total: "$420.00", selected: "xmr" as const,
    onSelect: noop, onPay: noop, onBack: noop,
  };
  const keeping = render(screens.orderSummary({ canKeepTheCode: true, ...base }));
  assert.ok(!keeping.textContent.includes(screens.NOT_KEPT_TITLE));

  const losing = render(screens.orderSummary({ canKeepTheCode: false, ...base }));
  assert.ok(losing.textContent.includes(screens.NOT_KEPT_TITLE),
    `the buyer must be told before paying: ${losing.textContent.slice(0, 200)}`);
  assert.ok(losing.textContent.includes("copy the code as soon as it appears"));
  assert.ok(losing.all("button").some((b) => b.textContent.startsWith("Pay ")),
    "and it is a warning, not a refusal: the buyer may still pay");
});
