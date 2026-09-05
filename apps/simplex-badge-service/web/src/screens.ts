import { money, countdown, outstanding, startedAgo, type Outstanding } from "./format.js";
import { badgeIcon, hamburger, hasBadgeArt, methodMark } from "./icons.js";
import { paymentUri, qrSvg } from "./qr.js";
import type { HistoryRow, UnpaidOrder } from "./order.js";
import type { InvoiceView } from "./api.js";
import { THEMES, type Method, type Theme } from "./domain.js";
import type { CardFailure, NoKeyConfigured } from "./stripe.js";

/** A browser's interval id is a number and this does nothing. Under the test runner it is an
 * object, and unreferencing it is what stops a screen that was built and never stopped from
 * holding the process open until the suite is killed. */
function releaseFromEventLoop(timer: unknown): void {
  if (typeof timer === "object" && timer !== null && "unref" in timer && typeof timer.unref === "function") {
    timer.unref();
  }
}

export function el<K extends keyof HTMLElementTagNameMap>(
  tag: K,
  attrs: Record<string, string> = {},
  ...kids: Array<Node | string>
): HTMLElementTagNameMap[K] {
  const node = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) node.setAttribute(k, v);
  for (const kid of kids) node.append(typeof kid === "string" ? document.createTextNode(kid) : kid);
  return node;
}

export function button(label: string, onClick: () => void, cls = "primary"): HTMLButtonElement {
  const b = el("button", { class: cls, type: "button" }, label);
  b.addEventListener("click", onClick);
  return b;
}

function backButton(onBack: () => void): HTMLButtonElement {
  return button("← Back", onBack, "back");
}

function panel(...kids: Array<Node | string>): HTMLElement {
  return el("section", { class: "panel" }, ...kids);
}

export const MENU_ID = "menu";
const MENU_LABEL = "Menu";
export const THEME_LABEL = "Theme";
export const NEW_PURCHASE = "Buy a code";
export const PURCHASE_HISTORY = "Codes on this device";
export const FORGET_EVERYTHING = "Forget everything on this device";
export const NEW_INVOICE = "New invoice";
export const PART_PAID_TITLE = "Part of the amount has arrived";
export const KEEPS_WAITING = "This page keeps checking. The invoice will not expire while it waits, however long the network takes.";
/** A refusal with wording of its own. Any other failure keeps the generic message: an internal
 * error's text is not something to put in front of a buyer. */
export class CancelRefused extends Error {}

export const PAID_IN_FULL_TITLE = "The full amount has arrived";
export const CANCEL_NOT_OPEN = "This invoice is no longer open, so there is nothing to cancel.";
export const CANCEL_HAS_FUNDS = "This invoice has a payment on it, so it cannot be cancelled.";
export const CANCEL_STILL_OPEN = "This invoice was cancelled, but the service still shows it open. Do not send anything more to the address.";
export const CANCEL_INVOICE = "Cancel this invoice";
export const CANCEL_CONFIRM = "Cancel this invoice? The address stops accepting payment, and anything sent to it afterwards is lost.";
export const CANCEL_PENDING = "Cancelling\u2026";
export const CANCEL_FAILED = "Could not cancel it. Check the invoice before sending anything more.";

const THEME_NAMES: Readonly<Record<Theme, string>> = {
  light: "Light", dark: "Dark", system: "System",
};

export interface ChromeOptions {
  onNewPurchase: () => void;
  onHistory: () => void;
  onForget: () => void;
  theme: Theme;
  onTheme: (theme: Theme) => void;
  onToggle: (open: boolean) => void;
}

export interface Chrome {
  node: HTMLElement;
  close(): void;
  isOpen(): boolean;
  holds(target: unknown): boolean;
  offerNewPurchase(on: boolean): void;
  showTheme(theme: Theme): void;
  focusables(): HTMLElement[];
}

// Every screen is built node by node and markup is never assigned from a string, so
// nothing a buyer or a server sends can become markup. `design.test.ts` checks the sinks.
//
// Nothing in the menu may carry an order: every item is a fixed label with a callback.
export function chrome(o: ChromeOptions): Chrome {
  const brand = el("a", { class: "brand", href: "/", "aria-label": "SimpleX" });
  const panelNode = el("div", {
    class: "menu", id: MENU_ID, hidden: "", role: "dialog", "aria-label": MENU_LABEL,
  });
  const trigger = el("button", {
    class: "menu-button", type: "button",
    "aria-label": MENU_LABEL, "aria-expanded": "false", "aria-controls": MENU_ID,
  });
  trigger.append(hamburger());

  let open = false;
  const setOpen = (next: boolean): void => {
    open = next;
    trigger.setAttribute("aria-expanded", String(next));
    if (next) panelNode.removeAttribute("hidden");
    else panelNode.setAttribute("hidden", "");
    o.onToggle(next);
  };
  const close = (): void => {
    if (!open) return;
    setOpen(false);
    trigger.focus?.();
  };
  trigger.addEventListener("click", () => { if (open) close(); else setOpen(true); });

  const actions: HTMLButtonElement[] = [];
  const action = (label: string, run: () => void, cls = "menu-item"): HTMLButtonElement => {
    const b = button(label, () => { close(); run(); }, cls);
    actions.push(b);
    return b;
  };

  const segments = new Map<Theme, HTMLButtonElement>();
  const segmented = el("div", { class: "segmented", role: "group", "aria-label": THEME_LABEL });
  for (const theme of THEMES) {
    const seg = button(THEME_NAMES[theme], () => { o.onTheme(theme); }, "segment");
    seg.setAttribute("aria-pressed", String(o.theme === theme));
    segments.set(theme, seg);
    segmented.append(seg);
  }
  const showTheme = (theme: Theme): void => {
    for (const [name, seg] of segments) seg.setAttribute("aria-pressed", String(name === theme));
  };

  const fresh = action(NEW_PURCHASE, o.onNewPurchase);
  panelNode.append(
    el("div", { class: "menu-section" },
      el("div", { class: "menu-row" }, el("span", { class: "menu-label" }, THEME_LABEL), segmented)),
    el("div", { class: "menu-section" }, fresh, action(PURCHASE_HISTORY, o.onHistory)),
    el("div", { class: "menu-section" }, action(FORGET_EVERYTHING, o.onForget, "menu-item danger")),
  );
  const node = el("header", { class: "chrome-bar" },
    brand, el("nav", { class: "menu-wrap" }, trigger, panelNode));
  return {
    node,
    close,
    isOpen: () => open,
    holds: (target) => target instanceof Node && node.contains(target),
    offerNewPurchase: (on) => {
      if (on) fresh.removeAttribute("hidden");
      else fresh.setAttribute("hidden", "");
    },
    showTheme,
    focusables: () => [...segments.values(), ...actions].filter((b) => !b.hasAttribute("hidden")),
  };
}

function notice(title: string, ...lines: string[]): HTMLElement {
  return el("div", { class: "notice" },
    el("span", { class: "title" }, title),
    ...lines.map((l) => el("p", {}, l)),
  );
}

function warning(title: string, ...lines: string[]): HTMLElement {
  const node = notice(title, ...lines);
  node.setAttribute("class", "warn");
  return node;
}

function field(label: string, value: Node | string, ...extra: Array<Node | string>): HTMLElement {
  return el("div", { class: "rows field" },
    el("span", { class: "label" }, label),
    el("div", { class: "mono" }, value),
    ...extra,
  );
}

// On the code screen the code exists nowhere else, so every write reports what happened. Success is
// confirmed on the button's own label, where the buyer is already looking; a failure needs
// an instruction and stays in the status line.
function copyableField(label: string, value: string, control: HTMLElement, status: HTMLElement): HTMLElement {
  const node = field(label, value, status);
  node.setAttribute("class", "rows field copyable");
  node.append(el("p", { class: "copy-line" }, control));
  return node;
}

/** The order id doubles as the buyer's reference: it is what support asks for, and it is already
 * in the address bar of the page they are looking at. */
function reference(orderId: string): HTMLElement {
  return field("Reference", orderId);
}

const COPY_CONFIRMED = "Copied";
const COPY_RESTORE_MS = 2000;

function copyControl(label: string, value: string, cls: string): { control: HTMLButtonElement; status: HTMLElement } {
  const status = el("p", { class: "muted copy-status", role: "status" });
  let restore: ReturnType<typeof setTimeout> | undefined;
  const confirmed = (): void => {
    status.textContent = "";
    control.textContent = COPY_CONFIRMED;
    control.setAttribute("class", `${cls} copied`);
    if (restore !== undefined) clearTimeout(restore);
    restore = setTimeout(() => {
      restore = undefined;
      control.textContent = label;
      control.setAttribute("class", cls);
    }, COPY_RESTORE_MS);
  };
  const control = button(label, () => {
    const write = navigator.clipboard?.writeText(value);
    if (write === undefined) {
      status.textContent = COPY_FAILED;
      return;
    }
    write.then(confirmed, () => { status.textContent = COPY_FAILED; });
  }, cls);
  return { control, status };
}

const COPY_FAILED = "Could not copy. Select it above and copy it by hand.";

function qrFigure(payload: string, label: string, caption?: string): HTMLElement | null {
  const symbol = qrSvg(payload, label);
  if (symbol === null) return null;
  const figure = el("div", { class: "qr-wrap" }, symbol);
  if (caption !== undefined) figure.append(el("p", { class: "muted" }, caption));
  return figure;
}

export interface LandingOptions {
  onStart: () => void;
}

export function landing(o: LandingOptions): HTMLElement {
  const p = panel(
    el("h1", {}, "Support SimpleX"),
    el("p", { class: "lede" }, "SimpleX has no ads, no user accounts and nothing to sell."),
    el("p", { class: "lede" }, "A supporter badge helps pay for the people who build it."),
    el("div", { class: "hero", role: "presentation" }),
    button("Choose your level", o.onStart),
    el("div", { class: "info" },
      el("span", { class: "title" }, "Already bought a code?"),
      el("p", {}, "Redeem it in the app: Settings, Supporter perks."),
    ),
    el("p", { class: "muted" }, "The badge shows on your profile. Nothing renews by itself, and no account is created."),
  );
  return p;
}

export interface TierOption {
  priceId: string;
  badgeType: string;
  name: string;
  price: string;
  features: readonly string[];
  disabled: boolean;
}

export interface TiersOptions {
  tiers: readonly TierOption[];
  selected: string | undefined;
  onSelect: (priceId: string) => void;
  onContinue: () => void;
  onBack: () => void;
}

export function tiers(o: TiersOptions): HTMLElement {
  const choices = el("div", { class: "choices" });
  for (const t of o.tiers) {
    const card = el("button", {
      class: "choice", type: "button", "aria-pressed": String(o.selected === t.priceId),
      ...(t.disabled ? { disabled: "" } : {}),
    });
    if (hasBadgeArt(t.badgeType)) card.append(badgeIcon(t.badgeType));
    card.append(
      el("div", { class: "name" }, t.name),
      el("div", { class: "price" }, t.price),
      ...t.features.map((f) => el("div", { class: "feature" }, f)),
    );
    if (!t.disabled) card.addEventListener("click", () => o.onSelect(t.priceId));
    choices.append(card);
  }
  const go = button("Continue", o.onContinue);
  if (o.selected === undefined) go.setAttribute("disabled", "");
  return panel(
    backButton(o.onBack),
    el("h1", {}, "Choose your level"),
    el("p", { class: "lede" }, "Bigger files, and longer for people to collect them."),
    choices,
    go,
  );
}

export interface DurationOption {
  key: string;
  name: string;
  price?: string;
  wasPrice?: string;
  savingPercent?: number;
  disabled: boolean;
}

export interface DurationsOptions {
  durations: readonly DurationOption[];
  selected: string | undefined;
  onSelect: (key: string) => void;
  onContinue: () => void;
  onBack: () => void;
}

export function durations(o: DurationsOptions): HTMLElement {
  const choices = el("div", { class: "choices" });
  for (const d of o.durations) {
    const card = el("button", {
      class: "choice term", type: "button", "aria-pressed": String(o.selected === d.key),
      ...(d.disabled ? { disabled: "" } : {}),
    }, el("div", { class: "name" }, d.name));
    if (d.price !== undefined) {
      card.append(d.wasPrice !== undefined
        ? el("div", { class: "price" }, el("s", { class: "was" }, d.wasPrice), d.price)
        : el("div", { class: "price" }, d.price));
    }
    if (d.savingPercent !== undefined && d.savingPercent > 0) {
      card.append(el("div", {}, el("span", { class: "pill" }, `save ${d.savingPercent}%`)));
    }
    if (!d.disabled) card.addEventListener("click", () => o.onSelect(d.key));
    choices.append(card);
  }
  const go = button("Continue", o.onContinue);
  if (o.selected === undefined) go.setAttribute("disabled", "");
  return panel(
    backButton(o.onBack),
    el("h1", {}, "How long?"),
    el("p", { class: "lede" }, "Prepaid months. Nothing renews by itself."),
    choices,
    go,
  );
}

const METHOD_NAMES: Readonly<Record<Method, string>> = { btc: "Bitcoin", xmr: "Monero", card: "Card" };
export const METHOD_ORDER: readonly Method[] = ["btc", "xmr", "card"];

export const NOT_KEPT_TITLE = "This browser will not keep your code";

export interface OrderSummaryOptions {
  badgeType: string;
  months: number;
  total: string;
  /** False where a code bought now could not be kept: this browser refuses to store anything, or
   * the orders list is full and every entry holds someone else's code. Either way, copy it. */
  canKeepTheCode: boolean;
  selected: Method;
  unavailable?: Method;
  openOrder?: OpenOrderLine;
  onSelect: (m: Method) => void;
  onPay: () => void;
  onBack: () => void;
}

export interface OpenOrderLine {
  orderId: string;
  awaitingCard?: boolean;
  onOpen: (orderId: string) => void;
}

function isPlainClick(e: MouseEvent): boolean {
  return e.button === 0 && !e.metaKey && !e.ctrlKey && !e.shiftKey && !e.altKey;
}

// Handled without leaving the document: when localStorage refuses, the store falls back to
// an in-memory Map and a full navigation would destroy every record in it.
function orderLink(orderId: string, label: string, cls: string, onOpen: (orderId: string) => void): HTMLElement {
  const a = el("a", { class: cls, href: `?order=${encodeURIComponent(orderId)}` }, label);
  a.addEventListener("click", (e) => {
    if (!isPlainClick(e)) return;
    e.preventDefault();
    onOpen(orderId);
  });
  return a;
}

function summaryRows(badgeType: string, months: number, total: string): HTMLElement {
  const level = badgeType.charAt(0).toUpperCase() + badgeType.slice(1);
  return el("div", { class: "rows" },
    el("div", { class: "row" }, el("span", {}, "Level"), el("span", {}, level)),
    el("div", { class: "row" }, el("span", {}, "Duration"), el("span", {}, months === 1 ? "1 month" : `${months} months`)),
    el("div", { class: "row total" }, el("span", {}, "Total"), el("span", {}, total)),
  );
}

export const AWAITING_CARD_TITLE = "A card payment is waiting to be confirmed";

export function orderSummary(o: OrderSummaryOptions): HTMLElement {
  const p = panel(backButton(o.onBack), el("h1", {}, "Check your order"));
  const awaiting = o.openOrder?.awaitingCard === true;
  if (o.openOrder) {
    p.append(el("p", { class: "row-line" }, orderLink(
      o.openOrder.orderId,
      awaiting ? "You have an order waiting to be confirmed" : "You have an order waiting for payment",
      "link", o.openOrder.onOpen,
    )));
  }
  p.append(summaryRows(o.badgeType, o.months, o.total));
  if (awaiting) {
    p.append(notice(AWAITING_CARD_TITLE,
      "A second order would be a second charge, so this one cannot be started yet.",
      "Open the order above. When its invoice expires, a new one can be started there."));
    return p;
  }
  if (o.unavailable !== undefined) {
    p.append(warning(`${METHOD_NAMES[o.unavailable]} is temporarily unavailable`,
      "Try another method, or come back later."));
  }
  // said here rather than only on the code screen: the code is the whole purchase, this browser
  // holds the only copy, and the service keeps nothing but its hash. After paying is too late to
  // learn that copying it by hand is the one thing standing between the buyer and losing it.
  if (!o.canKeepTheCode) {
    // no cause named: this is reached both where site data is off and where the store is full,
    // and the buyer's next move is the same either way
    p.append(warning(NOT_KEPT_TITLE,
      "This browser cannot save anything new right now.",
      "You can still pay, but copy the code as soon as it appears."));
  }
  p.append(el("span", { class: "label standalone" }, "Pay with"));
  const choices = el("div", { class: "choices methods" });
  for (const m of METHOD_ORDER) {
    const off = m === o.unavailable;
    const card = el("button", {
      class: "choice method center", type: "button", "aria-pressed": String(o.selected === m && !off),
      ...(off ? { disabled: "" } : {}),
    });
    card.append(methodMark(m), el("div", { class: "name" }, METHOD_NAMES[m]));
    if (off) card.append(el("div", { class: "feature" }, "unavailable"));
    else card.addEventListener("click", () => o.onSelect(m));
    choices.append(card);
  }
  p.append(choices);
  p.append(button(`Pay ${o.total} with ${METHOD_NAMES[o.selected]}`, o.onPay));
  p.append(el("p", { class: "muted" }, "Card is handled by Stripe. Bitcoin and Monero are on-chain, through BTCPay."));
  return p;
}

export function catalogChanged(onStartAgain: () => void): HTMLElement {
  return panel(
    el("h1", { class: "tight" }, "These prices have changed"),
    notice("Start again with the current prices",
      "The badge you chose was repriced while you were deciding.",
      "Nothing was charged."),
    button("Start again", onStartAgain),
  );
}

export interface RateLimitedOptions {
  total: string;
  method: Method;
  seconds: number;
  onBack: () => void;
}

export function rateLimited(o: RateLimitedOptions, onExpired: () => void): { node: HTMLElement; stop: () => void } {
  const tryAgainIn = (left: number): string => `Try again in ${left} second${left === 1 ? "" : "s"}`;
  const line = el("span", { class: "title", "aria-live": "off" }, tryAgainIn(o.seconds));
  const pay = button(`Pay ${o.total} with ${METHOD_NAMES[o.method]}`, () => {});
  pay.setAttribute("disabled", "");
  let left = o.seconds;
  const timer = setInterval(() => {
    left -= 1;
    if (left > 0) {
      line.textContent = tryAgainIn(left);
      return;
    }
    clearInterval(timer);
    onExpired();
  }, 1000);
  releaseFromEventLoop(timer);
  const stop = (): void => { clearInterval(timer); };
  const node = panel(
    backButton(o.onBack),
    el("h1", { class: "tight" }, "Too many attempts"),
    el("div", { class: "notice" }, line, el("p", {}, "The Pay button is disabled until then.")),
    pay,
  );
  return { node, stop };
}

export const OFFLINE_NOTE = "Offline. Still checking.";

function offlineNote(): HTMLElement {
  return el("p", { class: "muted offline", role: "status" }, OFFLINE_NOTE);
}

const CRYPTO_NAMES: Readonly<Record<"btc" | "xmr", string>> = { btc: "Bitcoin", xmr: "Monero" };
const CRYPTO_TICKERS: Readonly<Record<"btc" | "xmr", string>> = { btc: "BTC", xmr: "XMR" };

function settleCommand(origin: string, orderId: string): Node[] {
  const command = `curl -X POST ${origin}/control/settle/${orderId}`;
  const copy = copyControl("Copy command", command, "secondary inline");
  return [
    el("p", { class: "muted" }, "Against the mock, settle the order with its control endpoint:"),
    el("div", { class: "command" }, el("code", { class: "mono" }, command), copy.control),
    copy.status,
  ];
}

// Quiet on purpose: this screen exists to be paid, and a loud control beside the address
// competes with that. The red is spent on the confirmation and on a failure instead.
function cancelControl(onCancel: () => Promise<void>, notice: string | undefined): Node[] {
  const status = el("p", { class: "muted cancel-status", role: "status" }, notice ?? "");
  const control = button(CANCEL_INVOICE, () => {
    if (control.hasAttribute("disabled")) return;
    status.textContent = "";
    control.setAttribute("disabled", "");
    control.textContent = CANCEL_PENDING;
    const done = (): void => {
      control.removeAttribute("disabled");
      control.textContent = CANCEL_INVOICE;
    };
    // the refusal a caller names is more use than the generic one: "it has money in it" is a
    // different thing for a buyer to read than "it is still safe to pay"
    onCancel().then(done, (e: unknown) => {
      done();
      status.textContent = e instanceof CancelRefused ? e.message : CANCEL_FAILED;
    });
  }, "link danger");
  return [el("p", { class: "row-line" }, control), status];
}

export interface AwaitingPaymentOptions {
  order: UnpaidOrder;
  invoice: InvoiceView;
  method: "btc" | "xmr";
  nowMs: number;
  now?: () => number;
  resumed: boolean;
  offline?: boolean;
  onNewInvoice: () => void;
  onCancel: () => Promise<void>;
  /** Why the last cancel was refused, which outlives the screen it was asked on. */
  notice?: string;
}

export function awaitingPayment(o: AwaitingPaymentOptions): { node: HTMLElement; stop: () => void } {
  const amount = o.invoice.cryptoAmount ?? "";
  const clock = o.now ?? Date.now;
  const fiat = invoiceTotal(o.invoice);
  const phrase = (nowMs: number): string => {
    const held = countdown(o.invoice.expiresAt, nowMs);
    return held !== null ? `${fiat} — this rate is held for ${held}` : "Checking with the payment network";
  };
  const rateLine = phrase(o.nowMs);
  const address = copyControl("Copy", o.invoice.address ?? "", "secondary inline");
  const p = panel();
  if (o.resumed) {
    const started = startedAgo(o.order.createdAt, o.nowMs);
    if (started !== null) p.append(el("p", { class: "muted" }, started));
  }
  const paidSoFar = o.invoice.cryptoAmountPaid;
  // The provider's own figure for what is still owed: it applies the payment tolerance and adds
  // a network fee once a partial payment lands, so the invoice's amount minus what arrived is
  // the wrong number to ask for. Absent until something has been paid, when the amount stands.
  const left: Outstanding = paidSoFar === undefined
    ? { kind: "owed", amount }
    : outstanding(o.invoice.cryptoAmountDue);
  const owed = left.kind === "owed" ? left.amount : undefined;
  const ticker = CRYPTO_TICKERS[o.method];
  const rate = el("p", { class: "lede rate", "aria-live": "off" }, rateLine);
  p.append(
    el("h1", { class: "tight" }, owed !== undefined ? `Send ${owed} ${ticker}`
      : left.kind === "covered" ? PAID_IN_FULL_TITLE : PART_PAID_TITLE),
    rate,
  );
  if (paidSoFar !== undefined) {
    const seen = `We have seen ${paidSoFar} ${ticker} of ${amount} ${ticker}.`;
    p.append(owed !== undefined
      ? warning(PART_PAID_TITLE, seen, `Send the remaining ${owed} ${ticker} to the same address below.`)
      : left.kind === "covered"
        ? warning(seen, "Nothing more is owed. The invoice stays open until the payment confirms.")
        : warning(seen, "Send the rest to the same address below. The invoice stays open until it is paid in full."));
  }
  const uri = owed === undefined ? null : paymentUri(o.method, o.invoice.address ?? "", owed);
  const qr = uri === null ? null : qrFigure(uri, `${CRYPTO_NAMES[o.method]} payment code`);
  if (qr !== null && uri !== null) {
    const open = el("a", { class: "secondary inline wallet-link", href: uri }, "Open in wallet");
    // not .copy-line: that one is absolutely positioned inside a .field.copyable, and in
    // here it anchored to the page and landed under the header's menu button
    qr.append(el("p", { class: "wallet-line" }, open));
  }
  const amountCopy = copyControl("Copy", owed ?? "", "secondary inline");
  const details = el("div", { class: "details" },
    ...(owed === undefined ? [] : [copyableField(`Amount in ${ticker}`, owed, amountCopy.control, amountCopy.status)]),
    copyableField(`${CRYPTO_NAMES[o.method]} address`, o.invoice.address ?? "",
      address.control, address.status),
    reference(o.order.orderId),
  );
  const split = el("div", { class: "split" });
  if (qr !== null) split.append(qr);
  split.append(details);

  // what arrived is confirming, and the rest has not been sent: saying only the first, under a
  // notice asking for the remainder, reads as though the invoice were settled
  const partly = paidSoFar !== undefined && left.kind !== "covered";
  const waiting = el("p", { class: "awaiting", role: "status" },
    el("span", { class: "pulse", "aria-hidden": "true" }),
    partly ? "Waiting for the rest of the payment" : "Waiting for the payment to confirm");
  p.append(split, waiting);
  if (o.offline === true) p.append(offlineNote());
  p.append(
    el("p", { class: "muted" }, "Bookmark this page — the address and the countdown both live on this URL."),
  );
  if (o.resumed) p.append(button(NEW_INVOICE, o.onNewInvoice, "secondary"));
  p.append(...cancelControl(o.onCancel, o.notice));

  const timer = setInterval(() => {
    // the clear comes first: once the hold has lapsed the phrase stops changing, so a clear
    // after the equality check is a clear that never runs
    if (countdown(o.invoice.expiresAt, clock()) === null) clearInterval(timer);
    const next = phrase(clock());
    if (next === rate.textContent) return;
    rate.textContent = next;
  }, 1000);
  releaseFromEventLoop(timer);
  const stop = (): void => { clearInterval(timer); };
  return { node: p, stop };
}

export interface AwaitingConfirmationOptions {
  order: UnpaidOrder;
  invoice: InvoiceView | undefined;
  method: Method | undefined;
  gaveUp: boolean;
  offline?: boolean;
  onCheckAgain: () => void;
}

// No confirming screen offers [ New invoice ]: a card confirm has succeeded, checkout has no idempotency
// key, and the buyer would end up holding two live invoices. Both methods share it, the money being
// committed with something else to confirm it, so only the wait's words differ.
const CONFIRMING: Readonly<Record<Method, { status: string; wait: string }>> = {
  card: {
    status: "Waiting for the card network to confirm.",
    wait: "This usually takes a few seconds. The page updates itself.",
  },
  btc: {
    status: "Waiting for the Bitcoin network to confirm.",
    wait: "This takes a block, usually about ten minutes. The page updates itself.",
  },
  xmr: {
    status: "Waiting for the Monero network to confirm.",
    wait: "This takes a block, usually a couple of minutes. The page updates itself.",
  },
};

export function awaitingConfirmation(o: AwaitingConfirmationOptions): HTMLElement {
  if (o.gaveUp) {
    return panel(
      el("h1", { class: "tight" }, "This is taking longer than expected"),
      el("p", { class: "lede" }, "The payment has not been confirmed. This page keeps working: come back to it later, or quote the reference below."),
      reference(o.order.orderId),
      button("Check again", o.onCheckAgain),
    );
  }
  const method = o.method ?? "card";
  const p = panel(
    el("h1", { class: "tight" }, "Payment received"),
    el("p", { class: "awaiting", role: "status" },
      el("span", { class: "pulse", "aria-hidden": "true" }),
      CONFIRMING[method].status),
    notice("Still processing", CONFIRMING[method].wait),
  );
  // Greenfield reports no running count, so this states what settlement needs, not
  // progress. It goes with the prose above rather than between the two cards below.
  const needed = o.invoice?.requiredConfirmations;
  if (needed !== undefined && needed > 0 && method !== "card") {
    p.append(el("p", { class: "muted" },
      `This is settled once the payment has ${needed === 1 ? "1 confirmation" : `${needed} confirmations`} on the ${CRYPTO_NAMES[method]} blockchain.`));
  }
  // what we saw, so the buyer is not left guessing whether the amount was right
  const received = o.invoice?.cryptoAmountPaid;
  if (received !== undefined && method !== "card") {
    p.append(field("Received", `${received} ${CRYPTO_TICKERS[method]}`));
  }
  p.append(
    // the address and the rate countdown are deliberately gone: sending again would be a
    // second payment, and the rate hold stopped mattering when this one landed
    reference(o.order.orderId),
    el("p", { class: "muted" }, KEEPS_WAITING),
  );
  if (o.offline === true) p.append(offlineNote());
  return p;
}

export interface WindowClosedOptions {
  order: UnpaidOrder;
  invoice: InvoiceView | undefined;
  offline?: boolean;
  onNewInvoice: () => void;
}

export interface DetailsUnavailableOptions {
  order: UnpaidOrder;
  onCheckAgain: () => void;
  onNewInvoice: () => void;
}

export function windowClosed(o: WindowClosedOptions): HTMLElement {
  const paid = o.invoice?.amountPaid;
  // What the service counts as money on the invoice: the provider's verdict, reached through its own
  // payment tolerance, or any crypto figure at all. The sweep spares such an invoice down to less than
  // a minor unit, so this screen must not tell the buyer nothing arrived.
  const funded = o.invoice?.paidInFull === true || o.invoice?.cryptoAmountPaid !== undefined;
  const p = panel(el("h1", { class: "tight" }, "This invoice expired"));
  if (funded || (paid !== undefined && paid > 0)) {
    const crypto = o.invoice?.cryptoAmountPaid;
    const ticker = o.invoice?.cryptoCurrency;
    const arrived = crypto !== undefined && ticker !== undefined
      ? `${crypto} ${CRYPTO_TICKERS[ticker]}`
      : "A payment";
    // a payment that lands after the window is reported as closed, not as paid in full, so
    // where there is no verdict the figures are what is left to compare
    const total = o.invoice?.amount;
    const short = o.invoice?.paidInFull !== true && (paid === undefined || total === undefined || paid < total);
    p.append(warning(
      short ? `${arrived} arrived, which is not the full amount` : `${arrived} arrived after the window closed`,
      short
        ? "The rate window has closed, so the shortfall is no longer meaningful."
        : "The rate window had already closed when it arrived.",
      "Quote the reference below and we will sort it out."));
    p.append(reference(o.order.orderId));
  } else {
    p.append(el("p", { class: "lede" }, "Nothing was received, and nothing was charged."));
  }
  if (o.offline === true) p.append(offlineNote());
  p.append(button(NEW_INVOICE, o.onNewInvoice, "primary outline"));
  return p;
}

export interface CodeIssuedOptions {
  code: string;
  savedLocally: boolean;
}

export function codeIssued(o: CodeIssuedOptions): HTMLElement {
  const onlyCopy = o.savedLocally
    ? el("div", { class: "warn" },
        el("span", { class: "title" }, "This is the only copy."),
        el("p", {}, "Saved in this browser and nowhere else."),
        el("p", {}, "Anyone using this browser can read it, and clearing the browser loses it."))
    : el("div", { class: "warn" },
        el("span", { class: "title" }, "This code could not be saved in this browser."),
        el("p", {}, "Copy it now. It is shown here and nowhere else."));
  const copy = copyControl("Copy code", o.code, "primary outline");
  const p = panel(
    el("div", { class: "tick" }, "✓"),
    el("h1", { class: "tight center" }, "Paid. Here is your code."),
    el("div", { class: "code" }, o.code),
    copy.control,
    copy.status,
  );
  const qr = qrFigure(o.code, "Badge code as a scannable code", "scan to carry it to your phone");
  const details = el("div", { class: "details" },
    el("div", { class: "rows plain" },
      el("span", { class: "label" }, "Redeem it in the app"),
      el("div", {}, "Settings → Supporter perks → Redeem code"),
    ),
    onlyCopy,
  );
  const split = el("div", { class: "split" });
  if (qr !== null) split.append(qr);
  split.append(details);
  p.append(split);
  return p;
}

const MONTHS: readonly string[] = [
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December",
];

function calendarDate(when: string | undefined, withYear = false): string | null {
  if (when === undefined) return null;
  const at = new Date(when);
  if (Number.isNaN(at.getTime())) return null;
  const day = `${at.getUTCDate()} ${MONTHS[at.getUTCMonth()]}`;
  return withYear ? `${day} ${at.getUTCFullYear()}` : day;
}

function orderTitle(badgeType: string, months: number): string {
  const level = badgeType === "" ? "" : badgeType.charAt(0).toUpperCase() + badgeType.slice(1);
  const term = !Number.isFinite(months) || months <= 0 ? ""
    : months === 1 ? "1 month" : `${months} months`;
  return [level, term].filter((part) => part !== "").join(", ");
}

export interface PaidNoCodeOptions {
  order: UnpaidOrder;
  settledAt: string | undefined;
}

export function paidNoCode(o: PaidNoCodeOptions): HTMLElement {
  const summary = el("div", { class: "rows field" },
    el("div", { class: "name" }, orderTitle(o.order.badgeType, o.order.months)),
  );
  const p = panel(
    el("h1", { class: "tight" }, "This code is not on this device"),
    notice("The code was generated in the browser it was bought in, and is not stored anywhere else.",
      "Quote the reference below and we will sort it out."),
    summary,
  );
  const settled = calendarDate(o.settledAt);
  if (settled !== null) summary.append(el("p", { class: "muted" }, `paid ${settled}`));
  p.append(reference(o.order.orderId));
  return p;
}

export interface PurchaseHistoryOptions {
  rows: readonly HistoryRow[];
  /** False where this browser accepts writes and loses them. Rows already stored are still read
   * back, so this is about what the list may promise from here on, not about what it shows. */
  keepsNewCodes: boolean;
  onOpen: (orderId: string) => void;
  onStart: () => void;
}

const ENTRY_STATES: Readonly<Record<HistoryRow["kind"], { text: string; tone: string }>> = {
  paid: { text: "paid", tone: "settled" },
  paidNoCode: { text: "paid, and the code was not saved here", tone: "lost" },
  open: { text: "waiting for payment", tone: "pending" },
  partPaid: { text: "part of the amount received", tone: "pending" },
  processing: { text: "payment received, waiting to confirm", tone: "pending" },
  expired: { text: "this invoice expired", tone: "lost" },
};

function entryMeta(o: UnpaidOrder): HTMLElement {
  const meta = el("div", { class: "meta" });
  if (o.method !== undefined) {
    meta.append(el("span", { class: "method" }, methodMark(o.method), METHOD_NAMES[o.method]));
  }
  if (o.amount !== undefined && o.currency !== undefined) {
    meta.append(el("span", {}, money(o.amount, o.currency)));
  }
  const bought = calendarDate(o.createdAt, true);
  if (bought !== null) meta.append(el("span", {}, bought));
  return meta;
}

function entryLine(row: HistoryRow, onOpen: (orderId: string) => void): HTMLElement {
  const o = row.order;
  const head = el("div", { class: "entry-head" });
  if (hasBadgeArt(o.badgeType)) head.append(badgeIcon(o.badgeType));
  const main = el("div", { class: "entry-main" });
  const state = ENTRY_STATES[row.kind];

  const titleRow = el("div", { class: "entry-row" });
  const title = orderTitle(o.badgeType, o.months);
  if (title !== "") titleRow.append(el("div", { class: "name" }, title));
  titleRow.append(el("span", { class: `status ${state.tone}` }, state.text));
  main.append(titleRow);

  const metaRow = el("div", { class: "entry-row" });
  const meta = entryMeta(o);
  if (meta.children.length > 0) metaRow.append(meta);
  if (row.kind !== "paid") metaRow.append(orderLink(o.orderId, "Open", "secondary", onOpen));
  if (metaRow.children.length > 0) main.append(metaRow);
  head.append(main);

  const item = el("li", { class: "entry" }, head);
  if (row.kind === "paid") {
    const copy = copyControl("Copy", row.code, "secondary inline");
    item.append(el("div", { class: "code-row" }, el("code", { class: "mono" }, row.code), copy.control), copy.status);
  }
  return item;
}

export function purchaseHistory(o: PurchaseHistoryOptions): HTMLElement {
  if (o.rows.length === 0) {
    return panel(
      el("h1", {}, "Codes on this device"),
      el("p", { class: "lede" }, "Nothing bought on this device"),
      button("Choose your level", o.onStart),
    );
  }
  const list = el("ul", { class: "entries" });
  for (const row of o.rows) list.append(entryLine(row, o.onOpen));
  return panel(
    el("h1", {}, "Codes on this device"),
    el("p", { class: "lede" }, o.keepsNewCodes
      ? "Every code you bought is in this browser, and nowhere else."
      : "This browser cannot save anything new right now. Copy any code you have not kept elsewhere."),
    list,
  );
}

export function invoiceFailure(onRetry: () => void): HTMLElement {
  return panel(
    el("h1", { class: "tight" }, "That did not go through"),
    el("p", { class: "lede" }, "The order was not created, and nothing was charged."),
    el("p", { class: "lede" }, "If this happens again, get in touch."),
    button("Try again", onRetry),
  );
}

export function unknownOrder(onStartAgain: () => void): HTMLElement {
  return panel(
    el("h1", { class: "tight" }, "This link does not work"),
    el("p", { class: "lede" }, "Check the address you were given, or start again."),
    button("Start again", onStartAgain),
  );
}

export function detailsUnavailable(o: DetailsUnavailableOptions): HTMLElement {
  return panel(
    el("h1", { class: "tight" }, "The payment details are not available"),
    el("p", { class: "lede" }, "This order is open and unpaid, and the payment details did not arrive."),
    el("p", { class: "lede" }, "Quote the reference below and we will sort it out."),
    reference(o.order.orderId),
    button("Check again", o.onCheckAgain),
    button(NEW_INVOICE, o.onNewInvoice, "secondary"),
  );
}

export interface CardFormOptions {
  order: UnpaidOrder;
  invoice: InvoiceView;
  resumed: boolean;
  body?: HTMLElement;
  onNewInvoice: () => void;
}

export function invoiceTotal(invoice: InvoiceView): string {
  return invoice.amount !== undefined && invoice.currency !== undefined
    ? money(invoice.amount, invoice.currency)
    : "";
}

export function cardMount(): HTMLElement {
  return el("div", { class: "card-mount", id: "payment-element" });
}

export function cardForm(o: CardFormOptions): HTMLElement {
  const p = panel(
    el("h1", { class: "tight" }, "Pay by card"),
    summaryRows(o.order.badgeType, o.order.months, invoiceTotal(o.invoice)),
    o.body ?? cardMount(),
    reference(o.order.orderId),
  );
  if (o.resumed) p.append(button(NEW_INVOICE, o.onNewInvoice, "secondary"));
  return p;
}

export interface CardFieldsOptions {
  mount: HTMLElement;
  total: string;
  onPay: () => void;
}

export interface CardFields {
  node: HTMLElement;
  enable(): void;
  busy(on: boolean): void;
  error(message: string): void;
}

export const CARD_LOADING = "Loading the card form";

export function cardFields(o: CardFieldsOptions): CardFields {
  const status = el("p", { class: "muted", role: "status" }, CARD_LOADING);
  const pay = button(`Pay ${o.total}`, o.onPay);
  pay.setAttribute("disabled", "");
  return {
    node: el("div", { class: "card-fields" }, o.mount, pay, status),
    enable: () => { pay.removeAttribute("disabled"); status.textContent = ""; },
    busy: (on) => { if (on) pay.setAttribute("disabled", ""); else pay.removeAttribute("disabled"); },
    error: (message) => { status.textContent = message; },
  };
}

export const DEV_STAND_IN_TITLE = "Development stand-in. This is not a payment form.";

export interface CardStandInOptions {
  orderId: string;
  origin: string;
  onConfirm: () => void;
}

// `proof` is a token only stripe.ts's no-key branch produces, so a configured page cannot
// reach this at all.
export function cardStandIn(_proof: NoKeyConfigured, o: CardStandInOptions): HTMLElement {
  return el("div", { class: "warn" },
    el("span", { class: "title" }, DEV_STAND_IN_TITLE),
    el("p", {}, "No Stripe publishable key is configured, so this page has no card form and can take no card details."),
    el("p", {}, "The button below records the local hint and waits for the provider, as a successful confirmation does. It contacts nobody and charges nothing."),
    button("Simulate a confirmed card payment", o.onConfirm, "secondary"),
    ...settleCommand(o.origin, o.orderId),
  );
}

export interface CardUnavailableOptions {
  order: UnpaidOrder;
  reason: CardFailure;
  onRetry: () => void;
  onNewInvoice: () => void;
}

export function cardUnavailable(o: CardUnavailableOptions): HTMLElement {
  const offline = o.reason === "offline";
  return panel(
    el("h1", { class: "tight" }, "The card form did not load"),
    el("p", { class: "lede" }, offline
      ? "You are offline. The card form is the one part of this page that needs a connection."
      : "The card form is served by Stripe, and it did not arrive."),
    el("p", { class: "lede" }, "Nothing was charged. This order is still waiting to be paid."),
    reference(o.order.orderId),
    button("Try again", o.onRetry),
    button(NEW_INVOICE, o.onNewInvoice, "secondary"),
  );
}

export function loading(): HTMLElement {
  return panel(el("p", { class: "muted" }, "Checking with the payment network"));
}
