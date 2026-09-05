// Wiring: DOM events in, `flow.ts` calls out, `screens.ts` renders. The module that owns the
// page's state and the panel track.
//
// The track is one panel per index: 0 is the landing screen and panel i renders STEPS[i - 1],
// so CHECKOUT_INDEX is the last of them and the one every checkout screen replaces.
//
// An answer that arrives after the page has moved on must not act on what was true when it was
// asked for. Which guard applies depends on what the answer wants to do:
//
//   `flow.epoch`       a repaint, the address bar, or starting a watch
//   `store.wipeCount`  inserting a row the buyer may have erased since
//   the row existing   updating one, which is nothing to do once it is gone
//   node identity      a repaint that took the root
//   the hash           the history list
//
// Store writes are deliberately not behind the epoch: navigating bumps it, and an answer the
// buyer navigated away from still has to be recorded.

import { CATALOG, SINGLE_MONTH, offerTotal, savingPercent, type Offer, type Price, type Total } from "./catalog.js";
import { generate, hash } from "./codes.js";
import { Flow, type CheckoutOutcome, type Selection } from "./flow.js";
import { applyView, historyRows, selectionFromOrder, type PaymentView, withoutDestination } from "./order.js";
import { money, moneyCompact } from "./format.js";
import * as api from "./api.js";
import { resolveLoad } from "./routing.js";
import * as screens from "./screens.js";
import { cardPlan, loadStripeJs, mountCard, publishableKey, type CardFailure, type ConfirmOutcome } from "./stripe.js";
import { Store, type StorageLike } from "./store.js";
import { STEPS } from "./domain.js";
import type { Method, OrderRecord, SessionRecord, Step, Theme } from "./domain.js";

const app = document.getElementById("app");
if (app === null) throw new Error("main: #app is missing from the shell");
const root = app;

const pageFetch: typeof fetch = (input, init) => window.fetch(input, init);

const chromeSlot = document.getElementById("chrome");
if (chromeSlot === null) throw new Error("main: #chrome is missing from the shell");

// A read cannot tell a store that keeps things from one that will lose them: the fallback below
// accepts every write and forgets them all on the next load. The probe is a write round trip, and
// its answer travels with the store, because the code screen's promise rests on it.
function pageStore(): Store {
  const probe = "sxb.probe";
  let real: StorageLike | undefined;
  try {
    const s = window.localStorage;
    real = s;
    s.setItem(probe, "1");
    const durable = s.getItem(probe) === "1";
    if (durable) {
      s.removeItem(probe);
      return new Store(s, true);
    }
  } catch {
    // falls through to the memory store below
  }
  // the probe is this page's own key, so a read that throws does not get to leave it behind:
  // Forget does not know about it. A store that also refuses the removal keeps it, and there is
  // nothing further to be done about that.
  try { real?.removeItem(probe); } catch { /* nothing more can be done about it */ }
  return new Store(memoryOver(real), false);
}

// Writes go to memory so the page still works, but reads fall through to the real store where
// there is one: a browser that refuses writes because its quota is full is still holding the codes
// bought before it filled, and dropping the reader would hide them. Removals go to both, since
// [ Forget everything ] has to clear what is really there.
function memoryOver(real: StorageLike | undefined): StorageLike {
  const m = new Map<string, string>();
  return {
    getItem: (k) => {
      const held = m.get(k);
      if (held !== undefined) return held;
      try { return real?.getItem(k) ?? null; } catch { return null; }
    },
    setItem: (k, v) => { m.set(k, v); },
    removeItem: (k) => {
      m.delete(k);
      try { real?.removeItem(k); } catch { /* the store that refused the write may refuse this too */ }
    },
  };
}

const store = pageStore();

const flow = new Flow({
  store,
  fetch: pageFetch,
  sleep: api.realSleep,
  now: Date.now,
  newCode: generate,
  hashCode: hash,
  render: (view) => { paint(view); },
});

const chromeUi = screens.chrome({
  onNewPurchase: newInvoice,
  onHistory: showCodes,
  onForget: () => {
    if (!window.confirm("Remove every code stored in this browser? This cannot be undone.")) return;
    store.forgetEverything();
    resetToLanding("replace");
  },
  theme: store.theme(),
  onTheme: (theme) => { store.saveTheme(theme); applyTheme(theme); },
  onToggle: (open) => {
    for (const node of [root, document.getElementById("contact")]) {
      if (open) node?.setAttribute("inert", "");
      else node?.removeAttribute("inert");
    }
  },
});
chromeSlot.replaceChildren(chromeUi.node);

const THEME_ATTRIBUTE = "data-theme";

function applyTheme(theme: Theme): void {
  const html = document.documentElement;
  if (theme === "system") html?.removeAttribute(THEME_ATTRIBUTE);
  else html?.setAttribute(THEME_ATTRIBUTE, theme);
  chromeUi.showTheme(theme);
}

applyTheme(store.theme());

function syncChrome(): void {
  chromeUi.offerNewPurchase(store.newestOpen()?.submitted !== true);
}

window.addEventListener("keydown", (event) => {
  if (!chromeUi.isOpen()) return;
  const key = event.key;
  if (key === "Escape") { chromeUi.close(); return; }
  if (key !== "Tab") return;
  const items = chromeUi.focusables();
  if (items.length === 0) return;
  const at = items.findIndex((item) => item === document.activeElement);
  const back = event.shiftKey;
  const next = at < 0 ? (back ? items.length - 1 : 0) : at + (back ? -1 : 1);
  if (at >= 0 && next >= 0 && next < items.length) return;   // the browser can do it
  event.preventDefault?.();
  items[next < 0 ? items.length - 1 : next % items.length]?.focus?.();
});

document.addEventListener("click", (event) => {
  const target = event.target;
  if (target === null) return;
  if (chromeUi.isOpen() && !chromeUi.holds(target)) chromeUi.close();
});

const TIER_FEATURES: Readonly<Record<string, readonly string[]>> = {
  supporter: ["2 GB files", "7 days storage"],
  legend: ["5 GB files", "21 days storage"],
};

function priceOf(priceId: string | undefined): Price | undefined {
  return CATALOG.prices.find((p) => p.priceId === priceId);
}

function chosenDuration(session: SessionRecord): string | undefined {
  return session.offerId === undefined || session.offerId === "" ? undefined : session.offerId;
}

function offerOf(key: string | undefined): Offer | undefined {
  return key === undefined || key === SINGLE_MONTH ? undefined : CATALOG.offers.find((o) => o.offerId === key);
}

function durationsFor(price: Price): Array<{ key: string; offer: Offer | undefined }> {
  const offers = CATALOG.offers.filter((o) => o.priceId === price.priceId);
  return [{ key: SINGLE_MONTH, offer: undefined }, ...offers.map((o) => ({ key: o.offerId, offer: o }))]
    .sort((a, b) => (a.offer?.months ?? 1) - (b.offer?.months ?? 1));
}

function totalFor(price: Price | undefined, offer: Offer | undefined): Total | undefined {
  if (price === undefined) return undefined;
  const t = offerTotal(price.monthPrice, offer);
  return typeof t === "string" ? undefined : t;
}

const HASHES: Readonly<Record<Step, string>> = { tier: "#/tier", months: "#/months", checkout: "#/checkout" };
const CODES_HASH = "#/codes";

const rail = screens.el("div", { class: "rail" });
const track = screens.el("div", { class: "track" }, rail);
const panels: HTMLElement[] = [];
let index = 0;

function landingIndex(): number {
  const fromHash = STEPS.findIndex((s) => HASHES[s] === location.hash);
  return fromHash >= 0 ? fromHash + 1 : 0;
}

const FIRST_INDEX_NEEDING_A_LEVEL = 2;
// the wizard's steps, then the panel the order summary and every checkout screen share
const CHECKOUT_INDEX = STEPS.length;
const PANEL_COUNT = STEPS.length + 1;

function reachableIndex(at: number): number {
  return at >= FIRST_INDEX_NEEDING_A_LEVEL && effectiveSession().priceId === undefined ? 0 : at;
}

function hashForIndex(i: number): string {
  const step = STEPS[i - 1];
  return step === undefined ? "/" : HASHES[step];
}

let rateLimitStop: (() => void) | null = null;
let rateHoldStop: (() => void) | null = null;

function stopCountdowns(): void {
  rateLimitStop?.();
  rateLimitStop = null;
  rateHoldStop?.();
  rateHoldStop = null;
}

function replacePanel(at: number, node: HTMLElement): void {
  if (at === CHECKOUT_INDEX) stopCountdowns();
  const old = panels[at];
  if (old === undefined) { panels[at] = node; return; }
  rail.replaceChild(node, old);
  panels[at] = node;
}

function rebuild(at: number): void {
  replacePanel(at, buildPanel(at));
  applyInert();
  if (at === index) moveTrack(false);
}

// The order seeds the session from underneath, never over it: a buyer who walks back and
// picks a different duration would otherwise have that answer overwritten by the old
// order's, first on screen and then in what is charged.
function effectiveSession(): SessionRecord {
  const session = store.session();
  if (session.priceId !== undefined) return session;
  const seed = selectionFromOrder(store.orders()[0], CATALOG.prices, CATALOG.offers);
  return seed === undefined ? session : { ...seed, ...session };
}

function buildPanel(at: number): HTMLElement {
  const session = effectiveSession();
  switch (at) {
    case 0:
      return screens.landing({ onStart: () => goToIndex(1) });
    case 1:
      return screens.tiers({
        tiers: CATALOG.prices.map((p) => ({
          priceId: p.priceId,
          badgeType: p.badgeType,
          name: p.badgeType.charAt(0).toUpperCase() + p.badgeType.slice(1),
          price: `${moneyCompact(p.monthPrice, p.currency)} / month`,
          features: TIER_FEATURES[p.badgeType] ?? [],
          disabled: totalFor(p, undefined) === undefined,
        })),
        ...(session.priceId !== undefined ? { selected: session.priceId } : { selected: undefined }),
        onSelect: (priceId) => {
          unavailableMethod = undefined;
          store.saveSession({ step: "tier", priceId, offerId: undefined });
          rebuild(1);
          rebuild(2);
          rebuild(CHECKOUT_INDEX);
        },
        onContinue: () => goToIndex(2),
        onBack: () => history.back(),
      });
    case 2: {
      const price = priceOf(session.priceId);
      const durations = price === undefined ? [] : durationsFor(price).map(({ key, offer }) => {
        const total = totalFor(price, offer);
        const months = offer?.months ?? 1;
        const saving = total === undefined ? undefined : savingPercent(total.price, total.amount);
        return {
          key,
          name: months === 1 ? "1 month" : `${months} months`,
          ...(total !== undefined ? { price: moneyCompact(total.amount, price.currency) } : {}),
          ...(total !== undefined && saving !== undefined && saving > 0
            ? { wasPrice: moneyCompact(total.price, price.currency) } : {}),
          ...(saving !== undefined && saving > 0 ? { savingPercent: saving } : {}),
          disabled: total === undefined,
        };
      });
      return screens.durations({
        durations,
        selected: chosenDuration(session),
        onSelect: (key) => { store.saveSession({ step: "months", offerId: key }); rebuild(2); rebuild(CHECKOUT_INDEX); },
        onContinue: () => goToIndex(CHECKOUT_INDEX),
        onBack: () => history.back(),
      });
    }
    default: {
      const price = priceOf(session.priceId);
      const offer = offerOf(chosenDuration(session));
      const total = totalFor(price, offer);
      const method = session.method ?? "xmr";
      const open = store.newestOpen();
      return screens.orderSummary({
        canKeepTheCode: store.canHoldACode(),
        badgeType: price?.badgeType ?? "",
        months: total?.months ?? 1,
        total: total === undefined ? "" : money(total.amount, price?.currency ?? "usd"),
        selected: method,
        ...(unavailableMethod !== undefined ? { unavailable: unavailableMethod } : {}),
        ...(open !== undefined
          ? { openOrder: {
              orderId: open.orderId,
              ...(open.submitted === true ? { awaitingCard: true } : {}),
              onOpen: goToOrder,
            } }
          : {}),
        onSelect: (m) => { store.saveSession({ step: "checkout", method: m }); rebuild(CHECKOUT_INDEX); },
        onPay: () => { void pay(); },
        onBack: () => history.back(),
      });
    }
  }
}

function applyInert(): void {
  panels.forEach((p, i) => {
    if (i === index) p.removeAttribute("inert");
    else p.setAttribute("inert", "");
  });
}

function reducedMotion(): boolean {
  return window.matchMedia?.("(prefers-reduced-motion: reduce)").matches === true;
}

const SLIDE_MS = 320;

function panelHeight(at: number): number | null {
  const box = panels[at]?.getBoundingClientRect?.();
  const height = box === undefined ? 0 : Math.ceil(box.height);
  return height > 0 ? height : null;
}

function moveTrack(smooth: boolean): void {
  const slide = `--slide:${smooth && !reducedMotion() ? SLIDE_MS : 0}ms`;
  rail.setAttribute("style", `${slide};transform:translateX(-${index * 100}%)`);
  const height = panelHeight(index);
  track.setAttribute("style", height === null ? slide : `${slide};height:${height}px`);
}

function goToIndex(at: number): void {
  const step = STEPS[at - 1];
  if (step !== undefined) store.saveSession({ step });
  history.pushState(null, "", hashForIndex(at));
  showIndex(at, true);
}

function showIndex(at: number, smooth: boolean): void {
  if (at === 0) unavailableMethod = undefined;
  lastView = null;
  releaseCardElement();
  const freshMount = panels.length === 0 || root.firstChild !== track;
  if (panels.length === 0) {
    for (let i = 0; i < PANEL_COUNT; i++) panels.push(buildPanel(i));
    rail.replaceChildren(...panels);
  } else {
    replacePanel(at, buildPanel(at));
  }
  if (root.firstChild !== track) root.replaceChildren(track);
  index = at;
  applyInert();
  syncChrome();
  moveTrack(smooth && !freshMount);
}

let unavailableMethod: Method | undefined;

let checkoutInFlight = false;

async function pay(): Promise<void> {
  if (checkoutInFlight) return;
  const session = effectiveSession();
  const price = priceOf(session.priceId);
  if (price === undefined) return;
  if (store.newestOpen()?.submitted === true) return;
  const duration = chosenDuration(session);
  const sel: Selection = {
    priceId: price.priceId,
    ...(duration !== undefined && duration !== SINGLE_MONTH ? { offerId: duration } : {}),
    method: session.method ?? "xmr",
  };
  const payButton = panels[CHECKOUT_INDEX]?.querySelector("button.primary");
  if (payButton === null || payButton === undefined || payButton.hasAttribute("disabled")) return;
  payButton.setAttribute("disabled", "");

  // The disabled attribute does not survive a repaint of the panel, and a second checkout
  // buys a second invoice the buyer never asked for.
  checkoutInFlight = true;
  // The buyer can navigate while the invoice is being bought, and no outcome below is safe on a
  // page that has moved on: `created` rewrites the address bar and starts a watch for an order the
  // buyer has left, `failed` and `catalogChanged` take the root, and the other two arm a countdown
  // or overwrite the stored method for a screen nobody is on. Nothing is charged either way: the
  // buyer never sees an address or a card form, and a store that took the write keeps the order
  // in the codes list.
  const since = flow.epoch;
  let outcome: CheckoutOutcome;
  try {
    outcome = await flow.checkout(sel);
  } finally {
    checkoutInFlight = false;
  }
  if (flow.epoch !== since) return;
  switch (outcome.kind) {
    case "created": {
      // The draft is spent here rather than in `checkout`, which answers across a round trip the
      // buyer can navigate away from: cleared there, a choice they made in the meantime goes with
      // it, and `effectiveSession` reseeds from this order to charge a tier they had deselected.
      store.clearSession();
      history.replaceState(null, "", `?order=${encodeURIComponent(outcome.order.orderId)}`);
      unavailableMethod = undefined;
      root.replaceChildren(screens.loading());
      flow.watch(outcome.order.orderId, {
        initial: { status: "open", ...outcome.invoice },
        method: outcome.method,
        record: outcome.order,
      });
      return;
    }
    case "catalogChanged":
      root.replaceChildren(screens.catalogChanged(() => {
        newInvoice();
      }));
      return;
    case "rateLimited": {
      const screen = screens.rateLimited(
        { total: payTotal(), method: sel.method, seconds: outcome.retryAfter, onBack: () => history.back() },
        () => { if (index === CHECKOUT_INDEX && root.firstChild === track) rebuild(CHECKOUT_INDEX); },
      );
      replacePanel(CHECKOUT_INDEX, screen.node);
      rateLimitStop = screen.stop;
      applyInert();
      return;
    }
    case "providerUnavailable":
      unavailableMethod = outcome.method;
      store.saveSession({ method: firstAvailable(outcome.method) });
      rebuild(CHECKOUT_INDEX);
      return;
    case "failed":
      root.replaceChildren(screens.invoiceFailure(() => {
        rebuild(CHECKOUT_INDEX);
        root.replaceChildren(track);
        applyInert();
        moveTrack(false);
        void pay();
      }));
      return;
  }
}

function payTotal(): string {
  const session = effectiveSession();
  const price = priceOf(session.priceId);
  const total = totalFor(price, offerOf(chosenDuration(session)));
  return total === undefined ? "" : money(total.amount, price?.currency ?? "usd");
}

function firstAvailable(down: Method): Method {
  return screens.METHOD_ORDER.find((m) => m !== down) ?? "btc";
}

// A refusal written into the asking screen dies on the next repaint from the restarted watch, so it
// is held against its order and its epoch: every repaint of that order draws it, and leaving bumps
// the epoch, which drops it. Only a refusal that leaves the invoice open is held, since the closed
// screens have nowhere to draw one.
let cancelNotice: { orderId: string; epoch: number; message: string } | undefined;

/** Every refusal ends the same way: the watch was stopped before the request, so it has to be
 * started again or the page sits on a screen nothing updates, and the reason is thrown for
 * `cancelControl` to draw. */
function refuseCancel(orderId: string, resume: Parameters<typeof flow.watch>[1], message: string): never {
  flow.watch(orderId, resume);
  throw new screens.CancelRefused(message);
}

let cancelInFlight = false;

async function cancelInvoice(orderId: string): Promise<void> {
  if (!window.confirm(screens.CANCEL_CONFIRM)) return;
  cancelNotice = undefined;
  // `stopAll` empties the map, so `watch` has no previous loop to take these from: without them a
  // restart redraws the order as a fresh one, losing the resumed line and the New invoice button.
  // The record is deliberately not among them: this call is about to rewrite it, and a loop
  // holding the copy from before would draw the dead address back the first time a read failed.
  const { record: _record, ...resume } = flow.liveWatches().find((w) => w.orderId === orderId)?.restartOptions() ?? {};
  flow.stopAll();
  // the same rule the history refresh follows: an answer that lands after the store was wiped, or
  // after this page moved on, is not ours to write, and not ours to start a watch for either
  const since = flow.epoch;
  let cancelled: api.InvoiceView | undefined;
  try {
    // The answer is the now-expired invoice. Dropping it left the record `open` with its
    // destination, so an offline reload drew the dead address and a QR for it.
    cancelled = await api.cancelInvoice(orderId, pageFetch);
    // The record is what a reload draws from whenever the first read fails, so it is written
    // whenever it still exists. A record that is gone stays gone: with none stored, `applyView`
    // would compose a new one from the view and put a forgotten order back.
    if (store.order(orderId) !== undefined) applyView(store, orderId, cancelled, Date.now());
    if (flow.epoch !== since) {
      // The page moved on. If it moved to this same order, it is drawing the record as it stood
      // before this answer: the dead address, its QR, and a countdown to it. That loop will not
      // read again until the hold it is in answers, so this answer goes straight to a fresh loop
      // rather than leave an address on screen that nothing can reach.
      const live = flow.liveWatches().find((w) => w.orderId === orderId);
      if (live !== undefined) {
        // Awaited, not just stopped: `flow.watch` hands back the existing loop while it is
        // unfinished, so without this the restart would return the one just stopped. The unwind is
        // microtasks throughout, so nothing of the buyer's can land inside it.
        live.stop();
        await live.done;
        flow.watch(orderId, { initial: cancelled });
      }
      return;
    }
  } catch (e) {
    const code = e instanceof api.ApiError ? e.code : undefined;
    // `not_open` is the one refusal that proves the invoice is closed, having settled or expired
    // while the buyer decided, so its destination leaves the record on the same rule as the answer
    // above: a reload draws the record whenever the first read fails, and this page may be gone.
    if (code === "not_open") {
      const held = store.order(orderId);
      if (held !== undefined) store.saveOrder(withoutDestination(held));
    }
    if (flow.epoch !== since) return;
    // the closed screens have no slot for a notice, so this refusal is said once, where it was asked
    if (code === "not_open") refuseCancel(orderId, resume, screens.CANCEL_NOT_OPEN);
    // The invoice is still open on every other path, so the notice has a screen to draw on: money
    // is riding on it, or it failed for a reason this page cannot name.
    cancelNotice = {
      orderId,
      epoch: flow.epoch,
      message: code === "funded" ? screens.CANCEL_HAS_FUNDS : screens.CANCEL_FAILED,
    };
    if (code === "funded") refuseCancel(orderId, resume, screens.CANCEL_HAS_FUNDS);
    flow.watch(orderId, resume);
    throw e;
  }
  // The row is expired whether or not a payment landed, so an answer that is still open means the
  // service disagrees with its own contract. The address is dead either way, and this notice draws
  // because the order is still open: the payment screen is the only one with a slot for it.
  if (cancelled.status === "open") {
    cancelNotice = { orderId, epoch: flow.epoch, message: screens.CANCEL_STILL_OPEN };
    refuseCancel(orderId, resume, screens.CANCEL_STILL_OPEN);
  }
  // The provider is told before the row is written, so a payment can land during that round trip,
  // and a cancel can lose to settlement outright. Either way the order still has something to show,
  // so stay on it and let its own screen say what: the closed-window screen names what arrived and
  // the reference, and the code screen hands over the code.
  if (cancelled.paidInFull === true || cancelled.cryptoAmountPaid !== undefined) {
    flow.watch(orderId, resume);
    return;
  }
  newInvoice();
}

/** One cancel at a time. The button's own disabled attribute does not survive a repaint, and
 * `repaintConnectivity` redraws this screen on any online or offline event, handing back an armed
 * Cancel while the first request is still on the wire. */
async function cancelInvoiceOnce(orderId: string): Promise<void> {
  if (cancelInFlight) return;
  cancelInFlight = true;
  try {
    await cancelInvoice(orderId);
  } finally {
    cancelInFlight = false;
  }
}

/** Back to the landing screen with nothing of the last order still running or on screen. */
function resetToLanding(nav: "push" | "replace"): void {
  stopCountdowns();
  flow.stopAll();
  if (nav === "push") history.pushState(null, "", "/");
  else history.replaceState(null, "", "/");
  panels.length = 0;
  showIndex(0, false);
}

function newInvoice(): void {
  store.clearSession();
  resetToLanding("replace");
}

let lastView: PaymentView | null = null;

function isOffline(): boolean {
  return navigator.onLine === false;
}

function paint(view: PaymentView): void {
  if (view.screen === "cardForm" && cardConfirmPending) {
    lastView = view;
    return;
  }
  stopCountdowns();
  releaseCardElement();
  lastView = view;
  syncChrome();
  switch (view.screen) {
    case "unknownOrder":
      root.replaceChildren(screens.unknownOrder(newInvoice));
      return;
    case "codeIssued":
      root.replaceChildren(screens.codeIssued({ code: view.code, savedLocally: view.savedLocally }));
      return;
    case "paidNoCode":
      root.replaceChildren(screens.paidNoCode({ order: view.order, settledAt: view.invoice?.settledAt }));
      return;
    case "awaitingPayment": {
      const awaitingPayment = screens.awaitingPayment({
        order: view.order, invoice: view.invoice, method: view.method,
        nowMs: Date.now(), resumed: view.resumed, offline: isOffline(),
        onNewInvoice: newInvoice, onCancel: () => cancelInvoiceOnce(view.order.orderId),
        ...(cancelNotice?.orderId === view.order.orderId && cancelNotice.epoch === flow.epoch
          ? { notice: cancelNotice.message } : {}),
      });
      rateHoldStop = awaitingPayment.stop;
      root.replaceChildren(awaitingPayment.node);
      return;
    }
    case "awaitingConfirmation":
      root.replaceChildren(screens.awaitingConfirmation({
        order: view.order, invoice: view.invoice, method: view.method, gaveUp: view.gaveUp,
        offline: isOffline() && !view.gaveUp,
        onCheckAgain: () => checkAgain(view.order.orderId),
      }));
      return;
    case "windowClosed":
      root.replaceChildren(screens.windowClosed({
        order: view.order, invoice: view.invoice, offline: isOffline(), onNewInvoice: newInvoice,
      }));
      return;
    case "cardForm":
      renderCardForm(view);
      return;
    case "detailsUnavailable":
      root.replaceChildren(screens.detailsUnavailable({
        order: view.order,
        onCheckAgain: () => checkAgain(view.order.orderId), onNewInvoice: newInvoice,
      }));
      return;
  }
}

let cardElementDestroy: (() => void) | null = null;

let cardConfirmPending = false;

function releaseCardElement(): void {
  const destroy = cardElementDestroy;
  cardElementDestroy = null;
  destroy?.();
}

type CardView = Extract<PaymentView, { screen: "cardForm" }>;

// While a confirm is pending the form is left exactly as it is. A repaint would build a
// fresh Element and re-enable Pay while the previous confirm was still in flight, and
// checkout has no idempotency key.
function renderCardForm(view: CardView): void {
  releaseCardElement();
  const plan = cardPlan(publishableKey(), isOffline());
  if (plan.kind === "unavailable") {
    root.replaceChildren(cardFailureScreen(view, plan.reason));
    return;
  }
  const shell = (body: HTMLElement): HTMLElement => screens.cardForm({
    order: view.order, invoice: view.invoice, resumed: view.resumed, body, onNewInvoice: newInvoice,
  });
  if (plan.kind === "standIn") {
    const standIn = shell(screens.cardStandIn(plan.proof, {
      orderId: view.order.orderId,
      origin: location.origin,
      onConfirm: () => { cardConfirmed(view, standIn); },
    }));
    root.replaceChildren(standIn);
    return;
  }
  const mount = screens.cardMount();
  let confirm: (() => Promise<ConfirmOutcome>) | null = null;
  const fields = screens.cardFields({
    mount,
    total: screens.invoiceTotal(view.invoice),
    onPay: () => {
      if (confirm === null) return;
      fields.busy(true);
      cardConfirmPending = true;
      void confirm().then((outcome) => {
        cardConfirmPending = false;
        if (outcome.kind === "submitted") { cardConfirmed(view, node); return; }
        // a repaint was suppressed while this was in flight; if the buyer has navigated since,
        // the form this error belongs to is not on screen and the current one is owed the paint
        if (root.firstChild !== node) {
          if (lastView !== null) paint(lastView);
          return;
        }
        fields.busy(false);
        fields.error(outcome.message);
      });
    },
  });
  const node = shell(fields.node);
  root.replaceChildren(node);
  void mountCard({ plan, clientSecret: view.clientSecret, target: mount, loadStripe: loadStripeJs })
    .then((result) => {
      if (root.firstChild !== node) {
        if (result.kind === "mounted") result.destroy();
        return;
      }
      if (result.kind === "failed") {
        root.replaceChildren(cardFailureScreen(view, result.reason));
        return;
      }
      confirm = result.confirm;
      cardElementDestroy = result.destroy;
      fields.enable();
    });
}

function cardFailureScreen(view: CardView, reason: CardFailure): HTMLElement {
  return screens.cardUnavailable({
    order: view.order, reason,
    onRetry: () => { renderCardForm(view); },
    onNewInvoice: newInvoice,
  });
}

// A successful confirm is a hint, not proof, so this draws the confirming screen and never a code. The flag
// goes on the order rather than the session, which the next checkout clears.
function cardConfirmed(view: CardView, owner: Node): void {
  store.markSubmitted(view.order.orderId);
  // the confirm is recorded whatever the page shows now, but a buyer who navigated while it was
  // in flight is on another screen: taking the root would put this order under that URL. Where the
  // write lands, the flag is on the record and opening this order again comes back here.
  if (root.firstChild !== owner) return;
  // `view.order` has had the code stripped out of it, and `stopAll` below drops the loop that is
  // holding the one record that still carries it. The store refuses this order's write when the
  // list is full of orders that all hold codes, and when `setItem` starts failing after the boot
  // probe passed. The loop is the only copy left in both, so it is read before it goes.
  const live = flow.liveWatches().find((w) => w.orderId === view.order.orderId)?.restartOptions().record;
  const confirmed: OrderRecord = { ...(store.order(view.order.orderId) ?? live ?? view.order), submitted: true };
  flow.stopAll();
  paint({ screen: "awaitingConfirmation", order: view.order, invoice: undefined, gaveUp: false, method: "card" });
  flow.watch(view.order.orderId, { record: confirmed, method: "card" });
}

function goToOrder(orderId: string): void {
  history.pushState(null, "", `?order=${encodeURIComponent(orderId)}`);
  openOrder(orderId);
}

function checkAgain(orderId: string): void {
  if (flow.checkAgain(orderId) === null) {
    if (lastView !== null) paint(lastView);
    return;
  }
  root.replaceChildren(screens.loading());
}

function openOrder(orderId: string): void {
  flow.stopAll();
  // the previous order's view must not outlive its screen: a connectivity event repaints
  // `lastView`, and while this read is still in flight that would draw the order we just left
  // (its address, its amount) under this order's URL
  lastView = null;
  releaseCardElement();
  root.replaceChildren(screens.loading());
  flow.watch(orderId, { resumed: true });
}

function showCodes(): void {
  // as every other navigation does: a loop left running behind the list paints its order over
  // the list the moment the invoice moves, leaving the URL saying `#/codes`
  flow.stopAll();
  stopCountdowns();
  // the path, not a bare hash: a hash alone resolves against the current URL and would keep
  // the `?order=` of the screen this was opened from, which `syncFromLocation` reads first
  if (location.hash !== CODES_HASH) history.pushState(null, "", location.pathname + CODES_HASH);
  renderCodes(store.orders());
  void flow.refreshHistory().then(renderCodes);
}

function renderCodes(entries: readonly OrderRecord[]): void {
  if (location.hash !== CODES_HASH) return; // the buyer navigated away mid-refresh
  lastView = null; // the history list is taking the root; see `lastView`.
  releaseCardElement(); // it is mounted in a node this replaceChildren is about to drop
  syncChrome();
  root.replaceChildren(screens.purchaseHistory({
    rows: historyRows(entries),
    keepsNewCodes: store.durable,
    onOpen: goToOrder,
    onStart: () => { resetToLanding("push"); },
  }));
}

function syncFromLocation(fresh: boolean): void {
  flow.stopAll();
  stopCountdowns();
  chromeUi.close();
  const orderId = new URLSearchParams(location.search).get("order");
  if (orderId === null && location.hash === CODES_HASH) {
    renderCodes(store.orders());
    void flow.refreshHistory().then(renderCodes);
    return;
  }
  const load = resolveLoad({ search: location.search }, fresh ? store.newestOpen() : undefined);
  if (load.kind === "order") {
    openOrder(load.orderId);
    return;
  }
  const at = reachableIndex(landingIndex());
  const want = hashForIndex(at);
  if (want === "/" ? location.hash !== "" : location.hash !== want) {
    history.replaceState(null, "", want === "/" ? location.pathname : want);
  }
  showIndex(at, root.firstChild === track && panels.length > 0);
}

window.addEventListener("popstate", () => { syncFromLocation(false); });

window.addEventListener("resize", () => {
  if (root.firstChild === track) moveTrack(false);
});

document.addEventListener("visibilitychange", () => {
  for (const w of flow.liveWatches()) {
    if (document.hidden) w.suspend();
    else w.resume();
  }
});

function repaintConnectivity(): void {
  if (lastView !== null) paint(lastView);
}

window.addEventListener("online", repaintConnectivity);
window.addEventListener("offline", repaintConnectivity);

// Anubis serves its challenge as HTML at the same path as the page, so a worker
// registered before the real shell is on screen could cache the challenge as the shell.
// This call is the last statement in the module, and the tests check that ordering.
function registerServiceWorker(): void {
  if (!("serviceWorker" in navigator)) return;
  void navigator.serviceWorker.register("/sw.js").catch(() => { /* no offline support this visit */ });
}

syncFromLocation(true);
registerServiceWorker();
