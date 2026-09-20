import { money, countdown, outstanding, startedAgo, type Outstanding } from "./format.js";
import { badgeIcon, hamburger, hasBadgeArt, methodMark, wefunderMark } from "./icons.js";
import { paymentUri, qrSvg } from "./qr.js";
import type { HistoryRow, UnpaidOrder } from "./order.js";
import type { InvoiceView } from "./api.js";
import { THEMES, type Method, type Theme } from "./domain.js";
import type { CardFailure } from "./stripe.js";
import { CROWDFUNDING_ACTIVE, WEFUNDER_URL, dollars } from "./crowdfunding.js";

/** Under the test runner the timer is an object whose unref must be called, or a screen that was
 * built and never stopped holds the process open until the suite is killed. */
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
export const PURCHASE_HISTORY = "Your codes";
export const FORGET_EVERYTHING = "Forget everything on this device";
export const BUY_NEW_CODE = "Buy a new code";
export const PART_PAID_TITLE = "Part of the amount has arrived";
export const KEEPS_WAITING = "This page keeps checking. The invoice will not expire while it waits, however long the network takes.";
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
  theme: Theme;
  onTheme: (theme: Theme) => void;
  onToggle: (open: boolean) => void;
  onHome: () => void;
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

export function chrome(o: ChromeOptions): Chrome {
  const brand = el("a", { class: "brand", href: "/", "aria-label": "SimpleX" });
  brand.addEventListener("click", (e) => { e.preventDefault(); o.onHome(); });
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

function copyableField(label: string, value: string, control: HTMLElement, status: HTMLElement): HTMLElement {
  const node = field(label, value, status);
  node.setAttribute("class", "rows field copyable");
  node.append(el("p", { class: "copy-line" }, control));
  return node;
}

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

export const INVEST_LABEL = "Invest on Wefunder";

/** The investment that earns a badge as a perk: the minimum for a tier, or the amount for a chosen term. */
export interface InvestPerk {
  badgeType: string;
  amount: number;
  months?: number;
}

/** "Invest on" and the Wefunder wordmark, as one link in the accent. */
function investLink(): HTMLAnchorElement {
  return el("a", { class: "invest-link", href: WEFUNDER_URL, target: "_blank", rel: "noopener", "aria-label": INVEST_LABEL },
    "Invest on ", wefunderMark());
}

/** The sentence in its two halves, so that where it wraps it wraps before "and". */
export function investSentence(perk: InvestPerk | undefined): [string, string] {
  if (perk === undefined) return ["Invest $100+ in SimpleX Chat", "and get a free supporter badge."];
  const term = perk.months === undefined ? "" : ` for ${perk.months === 1 ? "1 month" : `${perk.months} months`}`;
  return [`Invest ${dollars(perk.amount)}+ in SimpleX Chat`, `and get a free ${perk.badgeType} badge${term}.`];
}

/** The block under the main button of a screen: one sentence and the link. Absent once the round has closed. */
export function investPanel(perk: InvestPerk | undefined): HTMLElement | undefined {
  if (!CROWDFUNDING_ACTIVE) return undefined;
  const [lead, rest] = investSentence(perk);
  return el("div", { class: "invest" },
    el("p", {}, el("span", { class: "half" }, lead), " ", el("span", { class: "half" }, rest)),
    investLink());
}

/** The one line under Pay. It does not wrap, so it says the amount and no more. */
export function investLine(perk: InvestPerk | undefined): HTMLElement | undefined {
  if (!CROWDFUNDING_ACTIVE || perk === undefined) return undefined;
  return el("p", { class: "muted invest-line" },
    `Or invest ${dollars(perk.amount)}+ `,
    el("a", { class: "link", href: WEFUNDER_URL, target: "_blank", rel: "noopener" }, "on Wefunder"),
    ".");
}

function withInvest(p: HTMLElement, node: HTMLElement | undefined): HTMLElement {
  if (node !== undefined) p.append(node);
  return p;
}

export interface LandingOptions {
  onStart: () => void;
  perk?: InvestPerk;
}

export function landing(o: LandingOptions): HTMLElement {
  const p = panel(
    el("h1", {}, "Support SimpleX"),
    el("p", { class: "lede" }, "Get a badge to send larger files (2‑5GB) that stay available longer (7‑21 days), and to show it on your profile."),
    el("div", { class: "hero", role: "presentation" }),
    el("div", { class: "notes" },
      el("p", { class: "muted" }, "You pay once for the months you choose. No subscription, no account."),
      el("p", { class: "muted" }, "Already have a code? Redeem it in the app: Settings, Supporter perks."),
    ),
    button("Choose your level", o.onStart),
  );
  return withInvest(p, investPanel(o.perk));
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
  perk?: InvestPerk;
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
  return withInvest(panel(
    backButton(o.onBack),
    el("h1", {}, "Choose your level"),
    el("p", { class: "lede" }, "Larger files that stay available longer."),
    choices,
    go,
  ), investPanel(o.perk));
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
  perk?: InvestPerk;
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
  return withInvest(panel(
    backButton(o.onBack),
    el("h1", {}, "How long?"),
    el("p", { class: "lede" }, "Paid once, for the months you choose. No subscription."),
    choices,
    go,
  ), investPanel(o.perk));
}

const METHOD_NAMES: Readonly<Record<Method, string>> = { btc: "Bitcoin", xmr: "Monero", card: "Card" };
export const METHOD_ORDER: readonly Method[] = ["btc", "xmr", "card"];

export const NOT_KEPT_TITLE = "This browser will not keep your code";

export interface OrderSummaryOptions {
  badgeType: string;
  months: number;
  total: string;
  discount?: Discount;
  canKeepTheCode: boolean;
  selected: Method;
  unavailable?: Method;
  openOrder?: OpenOrderLine;
  perk?: InvestPerk;
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

// When localStorage refuses, the store falls back to an in-memory Map, and a full navigation would
// destroy every record in it.
function orderLink(orderId: string, label: string, cls: string, onOpen: (orderId: string) => void): HTMLElement {
  const a = el("a", { class: cls, href: `?order=${encodeURIComponent(orderId)}` }, label);
  a.addEventListener("click", (e) => {
    if (!isPlainClick(e)) return;
    e.preventDefault();
    onOpen(orderId);
  });
  return a;
}

export interface Discount {
  price: string;
  off: string;
  percent?: number;
}

function summaryRows(badgeType: string, months: number, total: string, discount?: Discount): HTMLElement {
  const level = badgeType.charAt(0).toUpperCase() + badgeType.slice(1);
  const rows: HTMLElement[] = [
    el("div", { class: "row" }, el("span", {}, "Level"), el("span", {}, level)),
    el("div", { class: "row" }, el("span", {}, "Duration"), el("span", {}, months === 1 ? "1 month" : `${months} months`)),
  ];
  if (discount !== undefined) {
    rows.push(el("div", { class: "row" }, el("span", {}, "Price"), el("span", {}, discount.price)));
    rows.push(el("div", { class: "row discount" },
      el("span", {}, discount.percent !== undefined ? `Discount (${discount.percent}% off)` : "Discount"),
      // This is a U+2212 minus rather than a hyphen, so it lines up with the tabular figures.
      el("span", {}, `−${discount.off}`)));
  }
  rows.push(el("div", { class: "row total" }, el("span", {}, "Total"), el("span", {}, total)));
  return el("div", { class: "rows" }, ...rows);
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
  p.append(summaryRows(o.badgeType, o.months, o.total, o.discount));
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
  if (!o.canKeepTheCode) {
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
  p.append(el("div", { class: "notes" },
    el("p", { class: "muted" }, "Card is handled by Stripe. Bitcoin and Monero are on‑chain, through BTCPay.")));
  p.append(button(`Pay ${o.total} with ${METHOD_NAMES[o.selected]}`, o.onPay));
  return withInvest(p, investLine(o.perk));
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
  onCancel: () => Promise<void>;
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
  // The provider applies a payment tolerance and adds a network fee once a partial payment lands,
  // so the invoice amount minus what arrived is the wrong figure to ask for.
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
    // This uses .wallet-line rather than .copy-line, which is absolutely positioned inside
    // .field.copyable and here would anchor to the page under the header's menu button.
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

  const partly = paidSoFar !== undefined && left.kind !== "covered";
  const waiting = el("p", { class: "awaiting", role: "status" },
    el("span", { class: "pulse", "aria-hidden": "true" }),
    partly ? "Waiting for the rest of the payment" : "Waiting for the payment to confirm");
  p.append(split, waiting);
  if (o.offline === true) p.append(offlineNote());
  p.append(
    el("p", { class: "muted" }, "Bookmark this page — the address and the countdown both live on this URL."),
  );
  p.append(...cancelControl(o.onCancel, o.notice));

  const timer = setInterval(() => {
    // The clear must come before the equality check below, since once the hold lapses the phrase
    // stops changing and a clear placed after it would never run.
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
  const needed = o.invoice?.requiredConfirmations;
  if (needed !== undefined && needed > 0 && method !== "card") {
    p.append(el("p", { class: "muted" },
      `This is settled once the payment has ${needed === 1 ? "1 confirmation" : `${needed} confirmations`} on the ${CRYPTO_NAMES[method]} blockchain.`));
  }
  const received = o.invoice?.cryptoAmountPaid;
  if (received !== undefined && method !== "card") {
    p.append(field("Received", `${received} ${CRYPTO_TICKERS[method]}`));
  }
  p.append(
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
  canceled?: boolean;
  onNewInvoice: () => void;
}

export interface DetailsUnavailableOptions {
  order: UnpaidOrder;
  onCheckAgain: () => void;
  onNewInvoice: () => void;
}

export function windowClosed(o: WindowClosedOptions): HTMLElement {
  const buyNewCode = button(BUY_NEW_CODE, o.onNewInvoice, "primary outline");
  if (o.canceled === true) {
    return panel(
      el("h1", { class: "tight" }, "Invoice canceled"),
      el("p", { class: "lede" }, "You canceled this invoice. Nothing was charged."),
      reference(o.order.orderId),
      buyNewCode,
    );
  }
  const paid = o.invoice?.amountPaid;
  // A payment can land after the window closes, so this screen must count any crypto figure as money
  // and not tell the buyer nothing arrived.
  const funded = o.invoice?.paidInFull === true || o.invoice?.cryptoAmountPaid !== undefined;
  const p = panel(el("h1", { class: "tight" }, "This invoice expired"));
  if (funded || (paid !== undefined && paid > 0)) {
    const crypto = o.invoice?.cryptoAmountPaid;
    const ticker = o.invoice?.cryptoCurrency;
    const arrived = crypto !== undefined && ticker !== undefined
      ? `${crypto} ${CRYPTO_TICKERS[ticker]}`
      : "A payment";
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
  p.append(buyNewCode);
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

function calendarDate(when: string | undefined, withYear = false, withTime = false): string | null {
  if (when === undefined) return null;
  const at = new Date(when);
  if (Number.isNaN(at.getTime())) return null;
  const day = `${at.getDate()} ${MONTHS[at.getMonth()]}`;
  const dated = withYear ? `${day} ${at.getFullYear()}` : day;
  if (!withTime) return dated;
  const time = `${String(at.getHours()).padStart(2, "0")}:${String(at.getMinutes()).padStart(2, "0")}`;
  return `${dated}, ${time}`;
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
  keepsNewCodes: boolean;
  onOpen: (orderId: string) => void;
  onStart: () => void;
  onForget: () => void;
}

const ENTRY_STATES: Readonly<Record<HistoryRow["kind"], { text: string; tone: string }>> = {
  paid: { text: "paid", tone: "settled" },
  paidNoCode: { text: "paid, and the code was not saved here", tone: "lost" },
  open: { text: "waiting for payment", tone: "pending" },
  partPaid: { text: "part of the amount received", tone: "pending" },
  processing: { text: "payment received, waiting to confirm", tone: "pending" },
  canceled: { text: "canceled", tone: "lost" },
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
  const bought = calendarDate(o.createdAt, true, true);
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
  if (row.kind !== "paid" && row.kind !== "canceled") metaRow.append(orderLink(o.orderId, "Open", "secondary", onOpen));
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
      el("h1", {}, "Your codes"),
      el("p", { class: "lede" }, "Nothing bought on this device"),
      button("Choose your level", o.onStart),
    );
  }
  const list = el("ul", { class: "entries" });
  for (const row of o.rows) list.append(entryLine(row, o.onOpen));
  const forget = el("p", { class: "forget-line" },
    button(FORGET_EVERYTHING, o.onForget, "link danger"));
  return panel(
    el("h1", {}, "Your codes"),
    el("p", { class: "lede" }, o.keepsNewCodes
      ? "Every code you bought is in this browser, and nowhere else."
      : "This browser cannot save anything new right now. Copy any code you have not kept elsewhere."),
    list,
    forget,
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
    button(BUY_NEW_CODE, o.onNewInvoice, "secondary"),
  );
}

export interface CardFormOptions {
  order: UnpaidOrder;
  invoice: InvoiceView;
  resumed: boolean;
  body?: HTMLElement;
  onCancel?: () => Promise<void>;
  notice?: string;
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
  if (o.onCancel !== undefined) p.append(...cancelControl(o.onCancel, o.notice));
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

export interface CardUnavailableOptions {
  order: UnpaidOrder;
  reason: CardFailure;
  onRetry: () => void;
  onNewInvoice: () => void;
}

export function cardUnavailable(o: CardUnavailableOptions): HTMLElement {
  const lede =
    o.reason === "offline"
      ? "You are offline. The card form is the one part of this page that needs a connection."
      : o.reason === "unconfigured"
        ? "Card payment is not available on this page."
        : "The card form is served by Stripe, and it did not arrive.";
  return panel(
    el("h1", { class: "tight" }, "The card form did not load"),
    el("p", { class: "lede" }, lede),
    el("p", { class: "lede" }, "Nothing was charged. This order is still waiting to be paid."),
    reference(o.order.orderId),
    button("Try again", o.onRetry),
    button(BUY_NEW_CODE, o.onNewInvoice, "secondary"),
  );
}

export function loading(): HTMLElement {
  return panel(el("p", { class: "muted" }, "Checking with the payment network"));
}
