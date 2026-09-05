import assert from "node:assert/strict";
import { headingOf, installPage, inViewOf, primaryOf, settle, timedTest, until } from "./boot.js";
import { StubElement } from "./stub-dom.js";

const cardTest = timedTest(3000);

const page = installPage();
const { app, history, location, storage, fetches } = page;

// The shell's meta element, empty as it is committed. Every render reads
// it, so a test can change the configuration between two orders.
const keyMeta = new StubElement("meta");
keyMeta.setAttribute("id", "stripe-publishable-key");
keyMeta.setAttribute("name", "stripe-publishable-key");
keyMeta.setAttribute("content", "");
page.document.byId.set("stripe-publishable-key", keyMeta);
function configureKey(key: string): void { keyMeta.setAttribute("content", key); }

const stripe = await import("../src/stripe.js");
const screens = await import("../src/screens.js");
await import("../src/main.js");

type UnpaidOrder = import("../src/order.js").UnpaidOrder;
type InvoiceView = import("../src/api.js").InvoiceView;

const HELD_CODE = "SXB-YDC8A-YGQTM-PUYZ9-2TUXP";
const CLIENT_SECRET = "cs_test_a1b2c3d4";
const PUBLISHABLE_KEY = "pk_test_51NotARealKey";

function render(node: unknown): StubElement { return node as unknown as StubElement; }
function noop(): void { /* a control this test does not press */ }

/** An order that still carries its code, handed to a screen whose type says it cannot. */
const order: UnpaidOrder = {
  orderId: "inv_card", badgeType: "legend", months: 12,
  createdAt: "2026-08-28T11:02:19Z", status: "open",
  code: HELD_CODE,
} as unknown as UnpaidOrder;

const cardInvoice: InvoiceView = {
  status: "open", amount: 42000, currency: "usd", clientSecret: CLIENT_SECRET,
};

/** the guard over the whole serialized subtree, attributes included. */
function assertNoCode(node: StubElement, where: string): void {
  const dump = node.serialize();
  for (const form of [HELD_CODE, HELD_CODE.replace(/-/g, ""), "SXB-"]) {
    assert.ok(!dump.includes(form), `${where} leaked a code (${form})`);
  }
}

// ------------------------------------------------------------------ the gate

cardTest("stripe: with no publishable key there is a stand-in and nothing to load", () => {
  for (const [what, key] of [["absent", undefined], ["empty", ""], ["whitespace", "   "]] as const) {
    const plan = stripe.cardPlan(key, false);
    assert.equal(plan.kind, "standIn", `a ${what} key configures no card form`);
  }
});

cardTest("stripe: with a key configured the plan is to LOAD, and carries the key", () => {
  const plan = stripe.cardPlan(`  ${PUBLISHABLE_KEY}  `, false);
  assert.equal(plan.kind, "load");
  assert.equal(plan.kind === "load" ? plan.publishableKey : "", PUBLISHABLE_KEY, "trimmed, as a config value is");
  // The mutation this catches: a stand-in reachable on a configured page. The
  // `load` arm carries no token, so `screens.cardStandIn` cannot be called from
  // it at all. This asserts the runtime half of that.
  assert.ok(!("proof" in plan), "a configured page holds no token to build a stand-in with");
});

cardTest("stripe: offline with a key is a failure screen, not a form that never fills", () => {
  const plan = stripe.cardPlan(PUBLISHABLE_KEY, true);
  assert.equal(plan.kind, "unavailable");
  assert.equal(plan.kind === "unavailable" ? plan.reason : "", "offline");
});

cardTest("stripe: offline with NO key is still the stand-in — the local flow keeps working", () => {
  assert.equal(stripe.cardPlan("", true).kind, "standIn");
});

cardTest("stripe: the script URL is Stripe's own origin, which may not be self-hosted", () => {
  assert.ok(stripe.STRIPE_JS_URL.startsWith("https://js.stripe.com/"), stripe.STRIPE_JS_URL);
});

// ----------------------------------------------------------------- the mount

interface Trace {
  calls: string[]; target: unknown; secret: string; key: string; elementArgs: number;
  /** How many times the Element was actually torn down. */
  destroys: number;
}

function fakeStripe(over: {
  initFails?: boolean; mountThrows?: boolean; destroyThrows?: boolean;
  confirmResult?: import("../src/stripe.js").ConfirmResult; confirmRejects?: boolean;
} = {}): { load: import("../src/stripe.js").LoadStripeJs; trace: Trace; loaded: string[] } {
  const trace: Trace = { calls: [], target: null, secret: "", key: "", elementArgs: -1, destroys: 0 };
  const loaded: string[] = [];
  const sdk = {
    createPaymentElement: (...args: unknown[]) => {
      trace.calls.push("createPaymentElement");
      trace.elementArgs = args.length;
      return {
        mount: (target: unknown) => {
          trace.calls.push("mount");
          trace.target = target;
          if (over.mountThrows === true) throw new Error("no such node");
        },
        destroy: () => {
          trace.calls.push("destroy");
          trace.destroys += 1;
          if (over.destroyThrows === true) throw new Error("already gone");
        },
      };
    },
    loadActions: async () => {
      trace.calls.push("loadActions");
      return {
        actions: {
          confirm: async () => {
            trace.calls.push("confirm");
            if (over.confirmRejects === true) throw new Error("network");
            return over.confirmResult ?? {};
          },
        },
      };
    },
  };
  const load: import("../src/stripe.js").LoadStripeJs = async (src) => {
    loaded.push(src);
    return (key: string) => {
      trace.calls.push("Stripe");
      trace.key = key;
      return {
        initCheckoutElementsSdk: async (options: { clientSecret: string }) => {
          trace.calls.push("initCheckoutElementsSdk");
          trace.secret = options.clientSecret;
          if (over.initFails === true) throw new Error("no such session");
          return sdk;
        },
      };
    };
  };
  return { load, trace, loaded };
}

function loadPlan(): import("../src/stripe.js").LoadPlan {
  const plan = stripe.cardPlan(PUBLISHABLE_KEY, false);
  assert.equal(plan.kind, "load");
  return plan as import("../src/stripe.js").LoadPlan;
}

cardTest("stripe: mounting follows Stripe's script rule — init the SDK, create the element, mount it", async () => {
  const { load, trace, loaded } = fakeStripe();
  const target = { the: "mount point" };
  const result = await stripe.mountCard({ plan: loadPlan(), clientSecret: CLIENT_SECRET, target, loadStripe: load });
  assert.equal(result.kind, "mounted");
  assert.deepEqual(trace.calls, ["Stripe", "initCheckoutElementsSdk", "createPaymentElement", "mount"]);
  assert.deepEqual(loaded, [stripe.STRIPE_JS_URL], "loaded once, from js.stripe.com");
  assert.equal(trace.key, PUBLISHABLE_KEY);
  assert.equal(trace.secret, CLIENT_SECRET, "the client_secret, and nothing else about the order");
  assert.equal(trace.target, target, "the Element goes into the node it was handed");
  // Stripe's script rule: `ui_mode: elements` with no email field, since the fields are the ones we
  // render, and we render none. Passing options here is how one would creep in.
  assert.equal(trace.elementArgs, 0, "createPaymentElement takes no field configuration");
});

cardTest("stripe: a mounted form can be torn down, once, and a throwing teardown is survivable", async () => {
  const { load, trace } = fakeStripe();
  const mounted = await stripe.mountCard({ plan: loadPlan(), clientSecret: CLIENT_SECRET, target: {}, loadStripe: load });
  if (mounted.kind !== "mounted") throw new Error("expected a mounted form");
  assert.equal(trace.destroys, 0, "mounting tears nothing down");
  mounted.destroy();
  assert.equal(trace.destroys, 1, "Stripe's own teardown is what takes the iframes with it");
  mounted.destroy();
  assert.equal(trace.destroys, 1, "idempotent: a second release destroys nothing twice");

  const thrower = fakeStripe({ destroyThrows: true });
  const second = await stripe.mountCard({ plan: loadPlan(), clientSecret: CLIENT_SECRET, target: {}, loadStripe: thrower.load });
  if (second.kind !== "mounted") throw new Error("expected a mounted form");
  // The node is being replaced either way; a teardown that throws must not
  // take the screen with it.
  assert.doesNotThrow(() => { second.destroy(); });
  assert.equal(thrower.trace.destroys, 1);
});

cardTest("stripe: a script that does not load is a failure, and nothing is mounted", async () => {
  const { trace } = fakeStripe();
  const result = await stripe.mountCard({
    plan: loadPlan(), clientSecret: CLIENT_SECRET, target: {},
    loadStripe: async () => { throw new Error("blocked"); },
  });
  assert.equal(result.kind, "failed");
  assert.equal(result.kind === "failed" ? result.reason : "", "script");
  assert.deepEqual(trace.calls, [], "nothing of Stripe's was touched");
});

cardTest("stripe: an SDK that refuses the client secret is its own failure", async () => {
  const { load } = fakeStripe({ initFails: true });
  const result = await stripe.mountCard({ plan: loadPlan(), clientSecret: "cs_gone", target: {}, loadStripe: load });
  assert.equal(result.kind, "failed");
  assert.equal(result.kind === "failed" ? result.reason : "", "sdk");
});

cardTest("stripe: a mount that throws leaves no half-usable form", async () => {
  const { load } = fakeStripe({ mountThrows: true });
  const result = await stripe.mountCard({ plan: loadPlan(), clientSecret: CLIENT_SECRET, target: {}, loadStripe: load });
  assert.equal(result.kind, "failed");
  assert.equal(result.kind === "failed" ? result.reason : "", "sdk");
});

// --------------------------------------------------------------- confirming

async function confirmWith(over: Parameters<typeof fakeStripe>[0]): Promise<{
  outcome: import("../src/stripe.js").ConfirmOutcome; trace: Trace;
}> {
  const { load, trace } = fakeStripe(over);
  const mounted = await stripe.mountCard({ plan: loadPlan(), clientSecret: CLIENT_SECRET, target: {}, loadStripe: load });
  if (mounted.kind !== "mounted") throw new Error(`expected a mounted form, got ${mounted.kind}`);
  return { outcome: await mounted.confirm(), trace };
}

cardTest("stripe: confirming is loadActions() then confirm(), and success is `submitted` only", async () => {
  const { outcome, trace } = await confirmWith({});
  assert.deepEqual(outcome, { kind: "submitted" });
  assert.deepEqual(trace.calls.slice(-2), ["loadActions", "confirm"]);
  // the watch loop and the give-up rule: success is not proof of payment. Nothing here says paid,
  // carries a settlement time, or could be read as one.
  assert.ok(!("paid" in outcome) && !("settledAt" in outcome));
});

cardTest("stripe: a refusal is the reason Stripe gave, and the form stays", async () => {
  const { outcome } = await confirmWith({ confirmResult: { type: "error", error: { message: "Your card was declined." } } });
  assert.deepEqual(outcome, { kind: "error", message: "Your card was declined." });
});

cardTest("stripe: an error with no message of its own still says something usable", async () => {
  const { outcome } = await confirmWith({ confirmResult: { type: "error" } });
  assert.deepEqual(outcome, { kind: "error", message: stripe.CONFIRM_FAILED });
});

cardTest("stripe: a confirm that THREW is an error, and never a submission", async () => {
  const { outcome } = await confirmWith({ confirmRejects: true });
  assert.equal(outcome.kind, "error", "a rejected confirm must not move the page to the confirming screen");
});

// ------------------------------------------------------------- the screens

function standInProof(): import("../src/stripe.js").NoKeyConfigured {
  const plan = stripe.cardPlan("", false);
  if (plan.kind !== "standIn") throw new Error("a keyless page must plan a stand-in");
  return plan.proof;
}

cardTest("screens: the stand-in cannot be mistaken for a card form, and has no fields", () => {
  let confirmed = false;
  const p = render(screens.cardStandIn(standInProof(), {
    origin: "http://127.0.0.1:8099",
    orderId: "inv_card", onConfirm: () => { confirmed = true; },
  }));
  assert.ok(p.textContent.includes(screens.DEV_STAND_IN_TITLE), p.textContent);
  assert.ok(p.textContent.includes("no card form"));
  assert.ok(p.textContent.includes("charges nothing"));
  assert.equal(p.all("input").length, 0, "nothing to type a card number into");
  assert.equal(p.all("div.card-mount").length, 0, "and nowhere for a Payment Element to appear");
  // The command is runnable as printed: the real verb, this page's own origin,
  // and this order's id. A bare "POST /control/settle/:id" is not something a
  // reader can paste, which is the whole point of printing it.
  assert.ok(p.textContent.includes("curl -X POST http://127.0.0.1:8099/control/settle/inv_card"), p.textContent);
  // And it is selectable text, not a button label.
  assert.equal(p.all("code")[0]?.textContent, "curl -X POST http://127.0.0.1:8099/control/settle/inv_card");
  p.all("button.secondary")[0]!.click();
  assert.equal(confirmed, true, "the one control does what a confirm does");
});

cardTest("screens: the card fields are disabled until the Element is actually mounted", () => {
  let paid = 0;
  const mount = screens.cardMount();
  const fields = screens.cardFields({ mount, total: "$420.00", onPay: () => { paid += 1; } });
  const node = render(fields.node);
  assert.equal(node.all("div.card-mount")[0], render(mount), "the Element goes into the node handed in");
  const pay = node.all("button.primary")[0]!;
  assert.ok(pay.textContent.includes("$420.00"));
  assert.ok(pay.hasAttribute("disabled"), "Stripe.js has not run yet");
  assert.ok(node.textContent.includes(screens.CARD_LOADING));
  pay.click();
  assert.equal(paid, 0, "a disabled control does nothing");
  fields.enable();
  assert.ok(!pay.hasAttribute("disabled"));
  pay.click();
  assert.equal(paid, 1);
  fields.busy(true);
  pay.click();
  assert.equal(paid, 1, "one press, one attempt — there is no idempotency key");
  fields.busy(false);
  fields.error("Your card was declined.");
  assert.ok(node.textContent.includes("Your card was declined."));
});

cardTest("screens: the card form renders the body it is given, and no second mount point", () => {
  const withStandIn = render(screens.cardForm({
    order, invoice: cardInvoice, resumed: false, onNewInvoice: noop,
    body: screens.cardStandIn(standInProof(), { orderId: "inv_card", origin: "http://127.0.0.1:8099", onConfirm: noop }),
  }));
  assert.ok(withStandIn.textContent.includes("Legend"));
  assert.ok(withStandIn.textContent.includes("$420.00"));
  assert.ok(withStandIn.textContent.includes("inv_card"), "the reference is on every screen that may need support");
  assert.equal(withStandIn.all("div.card-mount").length, 0, "a keyless page has nowhere to mount");
  assert.ok(!withStandIn.textContent.includes(CLIENT_SECRET), "the client secret is never rendered");
  assertNoCode(withStandIn, "cardForm/standIn");
  assert.equal(withStandIn.all("svg.qr").length, 0, "the store rules: no QR on an unpaid screen");

  const bare = render(screens.cardForm({ order, invoice: cardInvoice, resumed: false, onNewInvoice: noop }));
  assert.equal(bare.all("div.card-mount").length, 1, "the default body is the mount point");
});

cardTest("screens: a card form that could not load is a screen, not a blank panel", () => {
  let retried = 0;
  let fresh = 0;
  for (const [reason, expected] of [["offline", "You are offline"], ["script", "served by Stripe"], ["sdk", "served by Stripe"]] as const) {
    const p = render(screens.cardUnavailable({
      order, reason, onRetry: () => { retried += 1; }, onNewInvoice: () => { fresh += 1; },
    }));
    assert.equal(p.all("h1")[0]!.textContent, "The card form did not load");
    assert.ok(p.textContent.includes(expected), `${reason}: ${p.textContent}`);
    // The copy has to be true: this order WAS created, and nothing was charged.
    assert.ok(p.textContent.includes("Nothing was charged"));
    assert.ok(p.textContent.includes("still waiting to be paid"));
    assert.ok(p.textContent.includes("inv_card"));
    assert.equal(p.all("div.card-mount").length, 0, "no mount point on a screen with nothing to mount");
    assertNoCode(p, `cardUnavailable/${reason}`);
  }
  const p = render(screens.cardUnavailable({ order, reason: "script", onRetry: () => { retried += 1; }, onNewInvoice: () => { fresh += 1; } }));
  p.all("button.primary")[0]!.click();
  p.all("button.secondary")[0]!.click();
  assert.equal(retried, 1, "[ Try again ] retries");
  assert.equal(fresh, 1, "and the buyer is never stranded");
});

// ------------------------------------------------------- main.ts, end to end

/**
 * The panel a buyer is looking at: the one that is not `inert` on the wizard's
 * track, and the only one there is on a payment screen.
 */
const screen = (): StubElement => inViewOf(app);
const heading = (): string => headingOf(screen());

/** An invoice running out on its own: the give-up rule leaves a confirmed order waiting for exactly that. */
function expireStoredOrder(orderId: string): void {
  const orders = JSON.parse(storage.getItem("sxb.orders.v1")!) as Array<Record<string, unknown>>;
  orders.find((o) => o.orderId === orderId)!.status = "expired";
  storage.setItem("sxb.orders.v1", JSON.stringify(orders));
}

function scripts(): StubElement[] {
  return page.document.head.children.filter((c): c is StubElement => c instanceof StubElement);
}
function stripeTags(): StubElement[] {
  return scripts().filter((s) => (s.getAttribute("src") ?? "").includes("js.stripe.com"));
}

cardTest("main: nothing of Stripe's is fetched at page load", () => {
  assert.equal(heading(), "Support SimpleX", "the boot is the landing screen");
  assert.equal(scripts().length, 0, "the shell adds no script of its own");
  assert.equal(stripeTags().length, 0, "Stripe.js is loaded on the card path and nowhere else");
  assert.ok(!fetches.some((f) => f.url.includes("stripe")), "and nothing requested it another way");
});

/** the landing screen → the order summary with Card selected, which is the only route to the card form. */
function walkToCard(): void {
  const primary = (): StubElement => primaryOf(screen())!;
  if (heading() === "Support SimpleX") primary().click();
  if (heading() === "Choose your level") {
    screen().all("button.choice").find((c) => c.textContent.startsWith("Legend"))!.click();
    primary().click();
  }
  if (heading() === "How long?") {
    screen().all("button.choice").find((c) => c.textContent.startsWith("12 months"))!.click();
    primary().click();
  }
  assert.equal(heading(), "Check your order");
  screen().all("button.choice").find((c) => c.textContent.startsWith("Card"))!.click();
}

function cardCreated(invoiceId: string): void {
  page.respondWith({
    status: 200,
    body: {
      invoiceId, badgeType: "legend", months: 12,
      amount: 42000, currency: "usd", expiresAt: "2126-08-28T13:00:00Z",
      clientSecret: CLIENT_SECRET,
    },
  });
}

cardTest("main: with NO key the card path renders the stand-in, and still loads nothing", async () => {
  walkToCard();
  cardCreated("inv_card_1");
  screen().all("button.primary")[0]!.click();
  await until(() => location.search === "?order=inv_card_1", "the card invoice");
  await settle();

  assert.equal(heading(), "Pay by card");
  assert.ok(screen().textContent.includes(screens.DEV_STAND_IN_TITLE), screen().textContent);
  assert.equal(screen().all("div.card-mount").length, 0);
  assert.equal(stripeTags().length, 0, "no key, no script — the gate is before the load");
  assert.ok(!screen().textContent.includes(CLIENT_SECRET), "the client secret is never on screen");
  // the store rules: the code is in localStorage from before the invoice existed.
  const stored = JSON.parse(storage.getItem("sxb.orders.v1")!) as Array<Record<string, string>>;
  assert.ok(stored[0]!.code!.startsWith("SXB-"));
  assert.ok(!screen().serialize().includes(stored[0]!.code!), "and never on an unpaid screen");
  assert.equal(storage.getItem("sxb.orders.v1")!.includes(CLIENT_SECRET), false,
    "the store rules: clientSecret is never written to rest");
});

cardTest("main: the stand-in's confirm lands on the confirming screen, WHICH WAITS — never on a code", async () => {
  const before = fetches.length;
  screen().all("button.secondary").find((b) => b.textContent === "Simulate a confirmed card payment")!.click();
  await settle();

  assert.equal(heading(), "Payment received", "the confirming screen, and not the code screen");
  assert.ok(screen().textContent.includes("Waiting for the card network to confirm."));
  assert.ok(!screen().textContent.includes("Here is your code"), "a confirm is not a payment");
  const stored = JSON.parse(storage.getItem("sxb.orders.v1")!) as Array<Record<string, string>>;
  assert.ok(!screen().serialize().includes(stored[0]!.code!), "the store rules: no code while the order is unpaid");
  assert.equal(stored[0]!.status, "open", "and the order is still open");
  // the watch loop as amended: a successful actions.confirm() writes `submitted` onto
  // this order, where the next checkout's `clearSession` cannot reach it.
  assert.equal(stored.find((o) => o.orderId === "inv_card_1")!.submitted, true);
  assert.equal(storage.getItem("sxb.session.v1"), null,
    "and the session, which the checkout 200 cleared, is not where it lives");
  // the give-up rule: the loop keeps asking. The provider is what settles this.
  assert.ok(fetches.length > before, "the waiting loop was started");
  assert.ok(fetches.slice(before).some((f) => f.url.startsWith("/api/invoice/inv_card_1")));
  // the give-up rule: [ New invoice ] is withheld here, and only here.
  assert.equal(screen().all("button").filter((b) => b.textContent === "New invoice").length, 0);
});

cardTest("main: the order summary withholds Pay while a card payment awaits confirmation", async () => {
  // the give-up rule takes [ New invoice ] off the confirming screen, and browser Back is the way round it:
  // the confirming screen → the wizard → the order summary → Pay creates a SECOND invoice for a purchase whose
  // confirm already returned success. The create endpoint has no idempotency key, so that is a
  // second real charge with no remedy.
  history.back();
  await settle();
  assert.equal(heading(), "How long?", "Back lands on the wizard");
  primaryOf(screen())!.click();
  assert.equal(heading(), "Check your order");

  assert.ok(screen().textContent.includes(screens.AWAITING_CARD_TITLE), screen().textContent);
  assert.equal(screen().all("button.primary").length, 0, "and no Pay button at all");
  assert.ok(!screen().textContent.includes("Pay $"), "not even a disabled one to press");
  const link = screen().all("a.link")[0]!;
  assert.equal(link.getAttribute("href"), "?order=inv_card_1", "the order is linked, so nobody is stranded");
  assert.ok(screen().textContent.includes("When its invoice expires, a new one can be started there."),
    "and the way out is named: the closed-window screen offers a new invoice once this one expires");

  // The guard is not the button alone: `pay()` refuses the same case, which is
  // what the invoice-failure screen's [ Try again ] would otherwise walk into.
  const before = fetches.length;
  await settle();
  assert.equal(fetches.slice(before).filter((f) => f.url === "/api/invoice").length, 0);
});

cardTest("main: Back from a payment screen to the order summary, and Pay still works", async () => {
  // The invoice awaiting confirmation above has expired with nothing charged,
  // its own way out, so the order summary may offer to create one again.
  expireStoredOrder("inv_card_1");
  // The 200 cleared `sxb.session.v1` and the panels are seeded from the
  // newest order instead, so the order summary draws a complete summary with an empty session
  // behind it. `pay()` used to read the raw session and return at its first
  // guard: a fully-rendered checkout whose Pay button did nothing at all.
  history.back();
  await settle();
  assert.equal(heading(), "How long?", "Back lands on the wizard, not on the replaced #/checkout");

  // And the answer given now must be the answer charged: the seed goes UNDER
  // the session, so a fresh duration is not overwritten by the old order's.
  screen().all("button.choice").find((c) => c.textContent.startsWith("3 months"))!.click();
  primaryOf(screen())!.click();
  assert.equal(heading(), "Check your order");
  assert.ok(screen().textContent.includes("$140.00"), `3 legend months are $140: ${screen().textContent}`);
  screen().all("button.choice").find((c) => c.textContent.startsWith("Card"))!.click();

  configureKey(PUBLISHABLE_KEY);
  const before = fetches.length;
  page.respondWith({
    status: 200,
    body: {
      invoiceId: "inv_card_2", badgeType: "legend", months: 3,
      amount: 14000, currency: "usd", expiresAt: "2126-08-28T13:00:00Z",
      clientSecret: CLIENT_SECRET,
    },
  });
  screen().all("button.primary")[0]!.click();
  await until(() => location.search === "?order=inv_card_2", "the second card invoice");
  await settle();

  const post = fetches.slice(before).find((f) => f.url === "/api/invoice");
  assert.ok(post, "Pay must actually submit — a dead primary button is worse than an error");
  const body = JSON.parse(String(post.init!.body)) as Record<string, string>;
  assert.equal(body.priceId, "price_legend", "the level the panel was drawn with");
  assert.equal(body.offerId, "offer_3m", "and the duration the buyer just chose, not the old order's");
  assert.equal(body.method, "card");
});

cardTest("main: with a key configured the stand-in is GONE and Stripe.js is fetched", () => {
  // The card form the test above landed on: the same order, now on a page that
  // has a key.
  assert.equal(heading(), "Pay by card");
  assert.ok(!screen().textContent.includes(screens.DEV_STAND_IN_TITLE),
    "a configured page has no development stand-in on it");
  assert.equal(screen().all("div.card-mount").length, 1, "the Payment Element mounts here");
  assert.equal(stripeTags().length, 1, "exactly one script tag, and only now");
  assert.equal(stripeTags()[0]!.getAttribute("src"), stripe.STRIPE_JS_URL);
  const pay = screen().all("button.primary")[0]!;
  assert.ok(pay.hasAttribute("disabled"), "nothing is payable until the Element is mounted");
});

cardTest("main: a Stripe.js that fails to load lands on the failure screen, not a blank panel", async () => {
  stripeTags()[0]!.dispatch("error");
  await settle();

  assert.equal(heading(), "The card form did not load");
  assert.ok(screen().textContent.includes("Nothing was charged"));
  assert.ok(screen().textContent.includes("inv_card"), "with the reference, so support is reachable");
  assert.equal(screen().all("div.card-mount").length, 0, "and no empty box where the fields were");
});

/** Every Element the page has mounted, and whether each has been torn down. */
const elements: Array<{ node: unknown; destroyed: boolean }> = [];
const confirms: number[] = [];
/**
 * When set, `actions.confirm()` holds until it is released: the window in
 * which a repaint must not rebuild the form.
 */
let heldConfirm: { promise: Promise<void>; release: () => void } | null = null;
function holdNextConfirm(): void {
  let release = (): void => {};
  const promise = new Promise<void>((r) => { release = r; });
  heldConfirm = { promise, release };
}

/** What a loaded Stripe.js defines. Installed once; every mount goes through it. */
(globalThis as unknown as { window: Record<string, unknown> }).window.Stripe = (key: string) => {
  assert.equal(key, PUBLISHABLE_KEY, "the page's own configured key, and no other");
  return {
    initCheckoutElementsSdk: async (options: { clientSecret: string }) => {
      assert.equal(options.clientSecret, CLIENT_SECRET);
      return {
        createPaymentElement: () => {
          const element = { node: null as unknown, destroyed: false };
          elements.push(element);
          return {
            mount: (node: unknown) => { element.node = node; },
            destroy: () => { element.destroyed = true; },
          };
        },
        loadActions: async () => ({
          actions: {
            confirm: async () => {
              confirms.push(1);
              if (heldConfirm !== null) await heldConfirm.promise;
              return {};
            },
          },
        }),
      };
    },
  };
};

cardTest("main: [ Try again ] asks for the script again, and a load mounts the Element", async () => {
  screen().all("button.primary")[0]!.click();   // [ Try again ]
  await settle();
  assert.equal(heading(), "Pay by card");
  // the tag that failed takes itself out of the head, so a page that retries all afternoon
  // carries one script element, not one per attempt
  assert.equal(stripeTags().length, 1, "the failed load is retried with a fresh tag, and only that");

  stripeTags()[0]!.dispatch("load");
  await settle();
  assert.equal(elements.length, 1, "the Element was mounted");
  assert.equal(elements[0]!.node, screen().all("div.card-mount")[0], "into the node on screen");
  const pay = screen().all("button.primary")[0]!;
  assert.ok(!pay.hasAttribute("disabled"), "and only now is the order payable");
  assert.equal(confirms.length, 0, "mounting confirms nothing");
});

cardTest("main: a remount DESTROYS the Element it replaces, and loads no second script", async () => {
  // the offline promise: losing the network takes the card form off the screen, and the node
  // the Element was mounted into goes with it. Without the teardown, Stripe's
  // iframes and listeners survive it, one set per mount.
  page.setOffline(true);
  await settle();
  try {
    assert.equal(heading(), "The card form did not load");
    assert.ok(screen().textContent.includes("You are offline"), screen().textContent);
    assert.equal(elements.length, 1, "and nothing new was mounted");
    assert.equal(elements[0]!.destroyed, true, "the Element that was on screen is gone");
  } finally {
    // Restored even when an assertion above fails: the waiting loop backs off
    // on real timers while offline, and leaving the page offline would
    // hang this process rather than just fail this test.
    page.setOffline(false);
    await settle();
  }
  assert.equal(heading(), "Pay by card");
  assert.equal(elements.length, 2, "coming back mounts a fresh Element");
  assert.equal(elements[1]!.destroyed, false, "which is the live one");
  assert.equal(elements[1]!.node, screen().all("div.card-mount")[0]);
  assert.equal(stripeTags().length, 1, "one load per page: the global is reused, not re-fetched");
  assert.ok(!screen().all("button.primary")[0]!.hasAttribute("disabled"), "and it is payable again");
});

cardTest("main: a connectivity flap must not remount the form under an in-flight confirm", async () => {
  // `online`/`offline` repaint the screen and `renderCardForm` built a fresh `cardFields` closure each time,
  // starting disabled and enabling itself when the new mount resolved, while the first `confirm()` was still
  // pending, writing into the closure just thrown away. A second Pay on one Checkout Session is a real charge.
  holdNextConfirm();
  const mounted = elements.length;
  const node = screen().all("div.card-mount")[0];
  screen().all("button.primary")[0]!.click();   // Pay
  await settle();
  assert.equal(confirms.length, 1, "the press issued one confirm");
  assert.ok(screen().all("button.primary")[0]!.hasAttribute("disabled"), "and the button went busy");

  page.setOffline(true);
  await settle();
  page.setOffline(false);
  await settle();

  assert.equal(heading(), "Pay by card", "the form the confirm belongs to is still the screen");
  assert.equal(elements.length, mounted, "nothing was remounted under the in-flight confirm");
  assert.equal(elements[mounted - 1]!.destroyed, false, "and the live Element was not torn down");
  assert.equal(screen().all("div.card-mount")[0], node, "it is the same form, with whatever was typed into it");
  assert.ok(screen().all("button.primary")[0]!.hasAttribute("disabled"),
    "the busy state survives the repaint — a re-enabled button is a second charge");
  assert.equal(confirms.length, 1, "and still exactly one confirm was issued");

  heldConfirm!.release();
  heldConfirm = null;
  await until(() => heading() === "Payment received", "the confirming screen once the confirm resolves");
  assert.equal(confirms.length, 1);
});

cardTest("main: a real confirm ALSO lands on the confirming screen, and the confirming screen alone", async () => {
  assert.equal(heading(), "Payment received");
  assert.equal(confirms.length, 1, "one press, one confirm");
  assert.ok(elements.every((e) => e.destroyed), "and the form it left behind took its Element with it");
  // the give-up rule as amended: no confirming screen offers a control that could start a second charge.
  assert.equal(screen().all("button").filter((b) => b.textContent === "New invoice").length, 0);
  assert.ok(!screen().textContent.includes("Here is your code"));
  const stored = (JSON.parse(storage.getItem("sxb.orders.v1")!) as Array<Record<string, string>>)
    .find((o) => o.orderId === "inv_card_2")!;
  assert.equal(stored.status, "open", "nothing about the order changed: the provider decides");
  assert.ok(!screen().serialize().includes(stored.code!));
  // the watch loop as amended: the flag is on the order, so it survives the next
  // checkout's `clearSession`, and [ New invoice ] cannot erase it.
  assert.equal(stored.submitted, true);
  assert.equal(storage.getItem("sxb.session.v1"), null, "and not in the session");
});

cardTest("main: the code appears only when the SERVER says paid", async () => {
  const stored = (JSON.parse(storage.getItem("sxb.orders.v1")!) as Array<Record<string, string>>)
    .find((o) => o.orderId === "inv_card_2")!;
  page.respondWith({
    status: 200,
    body: { status: "paid", badgeType: "legend", months: 3, settledAt: "2026-08-28T11:20:00Z" },
  });
  // Reopening the same `?order=` URL, as a buyer coming back does. The mock's
  // POST /control/settle/<id> is what makes it answer `paid`.
  page.fire("popstate");
  await until(() => heading().startsWith("Paid"), "codeIssued");
  assert.ok(screen().serialize().includes(stored.code!), "the code screen is the one screen that shows it");
});
