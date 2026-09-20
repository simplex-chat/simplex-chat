import { CATALOG, SINGLE_MONTH, offerTotal, savingPercent, type Offer, type Price, type Total } from "./catalog.js";
import { minimumFor, perkAmount } from "./crowdfunding.js";
import { generate, hash } from "./codes.js";
import { Flow, type CheckoutOutcome, type Selection } from "./flow.js";
import { applyView, historyRows, selectionFromOrder, type PaymentView, withoutDestination } from "./order.js";
import { money, moneyCompact } from "./format.js";
import * as api from "./api.js";
import { resolveLoad } from "./routing.js";
import * as screens from "./screens.js";
import { appearanceFor, cardPlan, loadStripeJs, mountCard, publishableKey, type CardFailure, type ConfirmOutcome } from "./stripe.js";
import { Store, type StorageLike } from "./store.js";
import { STEPS } from "./domain.js";
import type { Method, OrderRecord, SessionRecord, Step, Theme } from "./domain.js";
import { EMBED_READY, HEIGHT_MESSAGE, NAV_MESSAGE, THEME_MESSAGE, bgFromMessage, isNewPurchaseMessage, returnUrlFromMessage, routeFromMessage, themeFromMessage, trustedHost } from "./embed.js";

const app = document.getElementById("app");
if (app === null) throw new Error("main: #app is missing from the shell");
const root = app;

const pageFetch: typeof fetch = (input, init) => window.fetch(input, init);

const chromeSlot = document.getElementById("chrome");
if (chromeSlot === null) throw new Error("main: #chrome is missing from the shell");

function pageStore(): Store {
  const probe = "sb.probe";
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
  } catch {}
  try { real?.removeItem(probe); } catch {}
  return new Store(memoryOver(real), false);
}

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
      try { real?.removeItem(k); } catch {}
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

const embedded = window.self !== window.top;

const chromeUi = screens.chrome({
  onNewPurchase: startPurchase,
  onHistory: showCodes,
  theme: store.theme(),
  onTheme: (theme) => { setTheme(theme, true); },
  onToggle: (open) => {
    for (const node of [root, document.getElementById("contact")]) {
      if (open) node?.setAttribute("inert", "");
      else node?.removeAttribute("inert");
    }
  },
  onHome: () => goToIndex(0),
});
if (embedded) document.documentElement.classList.add("embedded");
else chromeSlot.replaceChildren(chromeUi.node);

const THEME_ATTRIBUTE = "data-theme";

function applyTheme(theme: Theme): void {
  const html = document.documentElement;
  if (theme === "system") html?.removeAttribute(THEME_ATTRIBUTE);
  else html?.setAttribute(THEME_ATTRIBUTE, theme);
  // The site's navbar keys its dark palette on a `.dark` class, so mirror the resolved theme onto it while styles.css reads data-theme.
  html?.classList.toggle("dark", theme === "dark" || (theme === "system" && prefersDark()));
  chromeUi.showTheme(theme);
}

let hostOrigin: string | undefined;

let hostReturnUrl: string | undefined;

// echo is false for a theme that arrived from the host, so applying it does not post it straight back to the host.
function setTheme(theme: Theme, echo: boolean): void {
  store.saveTheme(theme);
  applyTheme(theme);
  if (lastView?.screen === "cardForm") paint(lastView);
  if (echo && embedded && hostOrigin !== undefined) {
    window.parent.postMessage({ type: THEME_MESSAGE, theme }, hostOrigin);
  }
}

let welcomeHeight = 0;
function measureWelcome(): void {
  const landing = panels[0];
  if (landing === undefined || root.firstChild !== track) return;
  const pad = Number.parseFloat(getComputedStyle(document.body).paddingTop) || 0;
  const measured = Math.ceil(landing.getBoundingClientRect().height + pad);
  if (measured > 0) welcomeHeight = measured;
}

let lastHeight = 0;
function postHeight(): void {
  measureWelcome();
  // Rounding the fractional box height down leaves the iframe a sub-pixel short and the footer rides over the last row.
  const height = Math.ceil(document.documentElement.getBoundingClientRect().height);
  if (height === lastHeight) return;
  lastHeight = height;
  window.parent.postMessage({ type: HEIGHT_MESSAGE, height, min: welcomeHeight }, hostOrigin ?? "*");
}

function announceLocation(): void {
  if (!embedded || hostOrigin === undefined) return;
  window.parent.postMessage({ type: NAV_MESSAGE, hash: location.hash }, hostOrigin);
}

if (embedded) {
  window.addEventListener("message", (event) => {
    if (!trustedHost(event.origin)) return;
    if (hostOrigin === undefined) { hostOrigin = event.origin; postHeight(); }
    const theme = themeFromMessage(event.data);
    if (theme !== undefined) { hostOrigin = event.origin; setTheme(theme, false); return; }
    if (isNewPurchaseMessage(event.data)) { hostOrigin = event.origin; startPurchase(); return; }
    const hash = routeFromMessage(event.data);
    if (hash !== undefined) { hostOrigin = event.origin; applyRoute(hash); return; }
    const returnUrl = returnUrlFromMessage(event.data);
    if (returnUrl !== undefined) { hostOrigin = event.origin; hostReturnUrl = returnUrl; return; }
    const bg = bgFromMessage(event.data);
    if (bg !== undefined) { hostOrigin = event.origin; document.documentElement.style.setProperty("--bg", bg); }
  });
  new ResizeObserver(() => { postHeight(); }).observe(document.body);
  try { window.parent.postMessage({ type: EMBED_READY, theme: store.theme() }, "*"); } catch {}
}

applyTheme(store.theme());

function prefersDark(): boolean {
  return window.matchMedia?.("(prefers-color-scheme: dark)").matches === true;
}

function cardAppearance(): ReturnType<typeof appearanceFor> {
  return appearanceFor(store.theme(), prefersDark());
}

// A remembered return older than this is a stale attempt, not the buyer coming back from the redirect.
const CARD_RETURN_WINDOW_MS = 15 * 60 * 1000;
// Embedded, the redirect target must be the host page, or the confirm pulls the whole tab out of the site onto the standalone app.
function cardReturnUrl(): string {
  return embedded && hostReturnUrl !== undefined ? hostReturnUrl : location.origin + location.pathname;
}

window.matchMedia?.("(prefers-color-scheme: dark)").addEventListener?.("change", () => {
  if (store.theme() !== "system") return;
  applyTheme("system");
  if (lastView?.screen === "cardForm") paint(lastView);
});

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
  if (at >= 0 && next >= 0 && next < items.length) return;
  event.preventDefault?.();
  items[next < 0 ? items.length - 1 : next % items.length]?.focus?.();
});

document.addEventListener("click", (event) => {
  const target = event.target;
  if (target === null) return;
  if (chromeUi.isOpen() && !chromeUi.holds(target)) chromeUi.close();
});

const TIER_LIMITS: Readonly<Record<string, { gb: number; days: number }>> = {
  supporter: { gb: 2, days: 7 },
  legend: { gb: 5, days: 21 },
};

function tierName(badgeType: string): string {
  return badgeType.charAt(0).toUpperCase() + badgeType.slice(1);
}

function tierFeatures(badgeType: string): readonly string[] {
  const limits = TIER_LIMITS[badgeType];
  return limits === undefined ? [] : [`Files up to ${limits.gb} GB`, `Available for ${limits.days} days`];
}

// The line under the duration screen's heading: what the chosen badge gives, since the tier cards are a screen back.
function tierSummary(badgeType: string): screens.ChosenTier | undefined {
  const limits = TIER_LIMITS[badgeType];
  return limits === undefined ? undefined
    : { badge: `${tierName(badgeType)}:`, gives: `${limits.gb} GB files available for ${limits.days} days.` };
}

// The investment that earns the chosen badge as a perk: the tier's minimum until a term is chosen,
// then the exact amount for that term.
function investPerk(price: Price | undefined, months: number | undefined): screens.InvestPerk | undefined {
  if (price === undefined) return undefined;
  const exact = months === undefined ? undefined : perkAmount(price.badgeType, months);
  return exact === undefined || months === undefined
    ? { badgeType: price.badgeType, amount: minimumFor(price.badgeType) }
    : { badgeType: price.badgeType, amount: exact, months };
}

function withPerk<T extends object>(o: T, perk: screens.InvestPerk | undefined): T & { perk?: screens.InvestPerk } {
  return perk === undefined ? o : { ...o, perk };
}

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

// The order seeds the session from underneath, never over it, so a duration the buyer just picked is not overwritten by the old order.
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
          name: tierName(p.badgeType),
          price: `${moneyCompact(p.monthPrice, p.currency)} / month`,
          features: tierFeatures(p.badgeType),
          disabled: totalFor(p, undefined) === undefined,
        })),
        ...(session.priceId !== undefined ? { selected: session.priceId } : { selected: undefined }),
        ...withPerk({}, investPerk(priceOf(session.priceId), undefined)),
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
      const chosenKey = chosenDuration(session);
      const chosen = chosenKey === undefined ? undefined : totalFor(price, offerOf(chosenKey));
      const tier = price === undefined ? undefined : tierSummary(price.badgeType);
      return screens.durations({
        durations,
        selected: chosenKey,
        ...(tier !== undefined ? { tier } : {}),
        ...withPerk({}, investPerk(price, chosen?.months)),
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
        ...(total !== undefined && total.price > total.amount
          ? { discount: {
              price: money(total.price, price?.currency ?? "usd"),
              off: money(total.price - total.amount, price?.currency ?? "usd"),
              ...(savingPercent(total.price, total.amount) > 0 ? { percent: savingPercent(total.price, total.amount) } : {}),
            } }
          : {}),
        selected: method,
        ...withPerk({}, investPerk(price, total?.months)),
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
  announceLocation();
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

  // The disabled attribute does not survive a repaint, so this flag stops a second checkout buying a second invoice.
  checkoutInFlight = true;
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
      store.clearSession();
      history.replaceState(null, "", `?order=${encodeURIComponent(outcome.order.orderId)}`);
      announceLocation();
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
      replacePanel(CHECKOUT_INDEX, screens.invoiceFailure({
        ...withPerk({}, investPerk(price, totalFor(price, offerOf(duration))?.months)),
        onBack: () => { rebuild(CHECKOUT_INDEX); },
        onRetry: () => { rebuild(CHECKOUT_INDEX); void pay(); },
      }));
      applyInert();
      moveTrack(false);
      return;
  }
}

function payTotal(): string {
  const session = effectiveSession();
  const price = priceOf(session.priceId);
  const total = totalFor(price, offerOf(chosenDuration(session)));
  return total === undefined ? "" : money(total.amount, price?.currency ?? "usd");
}

// A coin that is down falls back to the other coin, and the card to the first coin: a buyer who
// chose a coin is not moved to a card.
function firstAvailable(down: Method): Method {
  return screens.METHOD_ORDER.find((m) => m !== "card" && m !== down) ?? "card";
}

let cancelNotice: { orderId: string; epoch: number; message: string } | undefined;

// The watch was stopped before the request, so restart it or the page sits on a screen nothing updates.
function refuseCancel(orderId: string, resume: Parameters<typeof flow.watch>[1], message: string): never {
  flow.watch(orderId, resume);
  throw new screens.CancelRefused(message);
}

let cancelInFlight = false;

async function cancelInvoice(orderId: string): Promise<void> {
  if (!window.confirm(screens.CANCEL_CONFIRM)) return;
  cancelNotice = undefined;
  const { record: _record, ...resume } = flow.liveWatches().find((w) => w.orderId === orderId)?.restartOptions() ?? {};
  flow.stopAll();
  const since = flow.epoch;
  let cancelled: api.InvoiceView | undefined;
  try {
    cancelled = await api.cancelInvoice(orderId, pageFetch);
    if (store.order(orderId) !== undefined) applyView(store, orderId, cancelled, Date.now());
    if (flow.epoch !== since) {
      const live = flow.liveWatches().find((w) => w.orderId === orderId);
      if (live !== undefined) {
        // flow.watch hands back the existing unfinished loop, so the stopped loop must be awaited before restarting.
        live.stop();
        await live.done;
        flow.watch(orderId, { initial: cancelled });
      }
      return;
    }
  } catch (e) {
    const code = e instanceof api.ApiError ? e.code : undefined;
    if (code === "not_open") {
      const held = store.order(orderId);
      if (held !== undefined) store.saveOrder(withoutDestination(held));
    }
    if (flow.epoch !== since) return;
    if (code === "not_open") refuseCancel(orderId, resume, screens.CANCEL_NOT_OPEN);
    cancelNotice = {
      orderId,
      epoch: flow.epoch,
      message: code === "funded" ? screens.CANCEL_HAS_FUNDS : screens.CANCEL_FAILED,
    };
    if (code === "funded") refuseCancel(orderId, resume, screens.CANCEL_HAS_FUNDS);
    flow.watch(orderId, resume);
    throw e;
  }
  if (cancelled.status === "open") {
    cancelNotice = { orderId, epoch: flow.epoch, message: screens.CANCEL_STILL_OPEN };
    refuseCancel(orderId, resume, screens.CANCEL_STILL_OPEN);
  }
  if (cancelled.paidInFull === true || cancelled.cryptoAmountPaid !== undefined) {
    flow.watch(orderId, resume);
    return;
  }
  store.markCanceled(orderId);
  newInvoice();
}

// The Cancel button's disabled attribute does not survive a repaint, so this flag allows one cancel at a time.
async function cancelInvoiceOnce(orderId: string): Promise<void> {
  if (cancelInFlight) return;
  cancelInFlight = true;
  try {
    await cancelInvoice(orderId);
  } finally {
    cancelInFlight = false;
  }
}

function resetToLanding(nav: "push" | "replace"): void {
  stopCountdowns();
  flow.stopAll();
  store.clearCardReturn();
  if (nav === "push") history.pushState(null, "", "/");
  else history.replaceState(null, "", "/");
  panels.length = 0;
  showIndex(0, false);
  announceLocation();
}

function newInvoice(): void {
  store.clearSession();
  resetToLanding("replace");
}

function startPurchase(): void {
  stopCountdowns();
  flow.stopAll();
  store.clearCardReturn();
  history.replaceState(null, "", "/");
  history.pushState(null, "", hashForIndex(1));
  panels.length = 0;
  showIndex(1, false);
  announceLocation();
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
        onCancel: () => cancelInvoiceOnce(view.order.orderId),
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
        order: view.order, invoice: view.invoice, offline: isOffline(),
        canceled: store.order(view.order.orderId)?.canceled === true,
        onNewInvoice: newInvoice,
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

// A repaint would build a fresh Element and re-enable Pay while the previous confirm is in flight, and checkout has no idempotency key.
function renderCardForm(view: CardView): void {
  releaseCardElement();
  const plan = cardPlan(publishableKey(), isOffline());
  if (plan.kind === "unavailable") {
    root.replaceChildren(cardFailureScreen(view, plan.reason));
    return;
  }
  const shell = (body: HTMLElement): HTMLElement => screens.cardForm({
    order: view.order, invoice: view.invoice, resumed: view.resumed, body,
    // A PaymentIntent is only cancelable before it confirms, so the button is inert once a confirm is in flight.
    onCancel: () => (cardConfirmPending ? Promise.resolve() : cancelInvoiceOnce(view.order.orderId)),
    ...(cancelNotice?.orderId === view.order.orderId && cancelNotice.epoch === flow.epoch
      ? { notice: cancelNotice.message } : {}),
  });
  const mount = screens.cardMount();
  let confirm: (() => Promise<ConfirmOutcome>) | null = null;
  const fields = screens.cardFields({
    mount,
    total: screens.invoiceTotal(view.invoice),
    onPay: () => {
      if (confirm === null) return;
      fields.busy(true);
      cardConfirmPending = true;
      // The confirm redirects the page, so remember the return before calling it.
      store.rememberCardReturn(view.order.orderId, Date.now());
      void confirm().then((outcome) => {
        cardConfirmPending = false;
        if (outcome.kind === "submitted") { cardConfirmed(view, node); return; }
        store.clearCardReturn();
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
  void mountCard({ plan, clientSecret: view.clientSecret, target: mount, appearance: cardAppearance(), returnUrl: cardReturnUrl(), loadStripe: loadStripeJs })
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

function cardConfirmed(view: CardView, owner: Node): void {
  store.markSubmitted(view.order.orderId);
  if (root.firstChild !== owner) return;
  const live = flow.liveWatches().find((w) => w.orderId === view.order.orderId)?.restartOptions().record;
  const confirmed: OrderRecord = { ...(store.order(view.order.orderId) ?? live ?? view.order), submitted: true };
  flow.stopAll();
  paint({ screen: "awaitingConfirmation", order: view.order, invoice: undefined, gaveUp: false, method: "card" });
  flow.watch(view.order.orderId, { record: confirmed, method: "card" });
}

function goToOrder(orderId: string): void {
  history.pushState(null, "", `?order=${encodeURIComponent(orderId)}`);
  openOrder(orderId);
  announceLocation();
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
  // A connectivity event repaints lastView, which would draw the order just left under this order's URL.
  lastView = null;
  releaseCardElement();
  root.replaceChildren(screens.loading());
  flow.watch(orderId, { resumed: true });
}

function showCodes(): void {
  flow.stopAll();
  stopCountdowns();
  // A bare hash resolves against the current URL and keeps the ?order= this was opened from, so push the full path.
  if (location.hash !== CODES_HASH) history.pushState(null, "", location.pathname + CODES_HASH);
  renderCodes(store.orders());
  void flow.refreshHistory().then(renderCodes);
  announceLocation();
}

function renderCodes(entries: readonly OrderRecord[]): void {
  if (location.hash !== CODES_HASH) return;
  lastView = null;
  releaseCardElement();
  syncChrome();
  root.replaceChildren(screens.purchaseHistory({
    rows: historyRows(entries),
    keepsNewCodes: store.durable,
    onOpen: goToOrder,
    onStart: startPurchase,
    onForget: () => {
      if (!window.confirm("Remove every code stored in this browser? This cannot be undone.")) return;
      store.forgetEverything();
      resetToLanding("replace");
    },
  }));
}

function syncFromLocation(fresh: boolean): void {
  flow.stopAll();
  stopCountdowns();
  chromeUi.close();
  const params = new URLSearchParams(location.search);
  const orderId = params.get("order");
  if (orderId === null && location.hash === CODES_HASH) {
    renderCodes(store.orders());
    void flow.refreshHistory().then(renderCodes);
    announceLocation();
    return;
  }
  if (orderId === null && fresh) {
    const resumeId = store.takeCardReturn(CARD_RETURN_WINDOW_MS, Date.now());
    if (resumeId !== undefined) {
      history.replaceState(null, "", `?order=${encodeURIComponent(resumeId)}`);
      openOrder(resumeId);
      announceLocation();
      return;
    }
  }
  const load = resolveLoad({ search: location.search }, fresh ? store.newestOpen() : undefined);
  if (load.kind === "order") {
    openOrder(load.orderId);
    announceLocation();
    return;
  }
  const at = reachableIndex(landingIndex());
  if (fresh && at > 0) {
    // The steps beneath a deep-linked one are not in history, so rebuild the stack or Back leaves the wizard.
    history.replaceState(null, "", "/");
    for (let i = 1; i <= at; i++) history.pushState(null, "", hashForIndex(i));
  } else {
    const want = hashForIndex(at);
    if (want === "/" ? location.hash !== "" : location.hash !== want) {
      history.replaceState(null, "", want === "/" ? location.pathname : want);
    }
  }
  showIndex(at, root.firstChild === track && panels.length > 0);
  announceLocation();
}

function applyRoute(hash: string): void {
  const path = location.pathname;
  history.pushState(null, "", hash === "" || hash === "/" ? path : path + hash);
  syncFromLocation(false);
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

// Anubis serves its challenge as HTML at the page's path, so a worker registered before the shell is on screen could cache the challenge as the shell.
function registerServiceWorker(): void {
  if (!("serviceWorker" in navigator)) return;
  void navigator.serviceWorker.register("/sw.js").catch(() => {});
}

// The shell prerenders the welcome page, so the document is that page until the first render, and a load resolving to a codes or order screen never mounts the wizard for measureWelcome to read.
if (embedded && welcomeHeight <= 0) welcomeHeight = Math.ceil(document.documentElement.getBoundingClientRect().height);

syncFromLocation(true);
document.documentElement.classList.remove("sb-booting");
registerServiceWorker();
