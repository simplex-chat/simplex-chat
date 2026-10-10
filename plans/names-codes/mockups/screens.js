// Every screen of the names board, built in the browser on the compiled webapp modules.
// Shared screens call the real `screens.js`; name screens are prototypes for stage 5's `nameScreens.ts`.
import * as s from "./assets/screens.js";
import { chevronLeft, methodMark, wefunderMark } from "./assets/icons.js";
import { qrSvg } from "./assets/qr.js";

const { el, button } = s;
const noop = () => {};
const NOW = Date.parse("2026-10-10T12:00:00Z");
const HOLD_MS = (59 * 60 + 59) * 1000;
const ORDER_ID = "q7ZpL2dN9xWc4KfR8tYb1A";
// a year on chain is 365 days, so 730 days from 10 October 2026 cross 29 February 2028 and end on the 9th
const UNTIL = "9 October 2028";
const CODE_UNTIL = "8 October 2033";
const OWNER_SHORT = "0x5aE1…7cD3";
const APP_OWNER_SHORT = "0x2F0b…94aE";
// a stand-in: the page makes a fresh phrase, with its BIP-39 checksum, for every order
const PHRASE = ["orbit", "velvet", "canyon", "ripple", "harbor", "tiny", "lemon", "frost", "danger", "museum", "shield", "olive"];
// placeholder amount, as crowdfunding.ts keeps the badge ones
const NAME_INVEST = "$500";

const ALPHABET = "0123456789ABCDEFGHJKMNPQRSTVWXYZ";
const BASE = ALPHABET.length;
const PAYLOAD = 19;

function checkValue(values) {
  let sum = 0;
  let factor = 2;
  for (let i = values.length - 1; i >= 0; i--) {
    const addend = factor * values[i];
    sum += Math.floor(addend / BASE) + (addend % BASE);
    factor = factor === 2 ? 1 : 2;
  }
  return (BASE - (sum % BASE)) % BASE;
}

// A name code: "SN", the shortest name it covers when it has one (a convention: the service's table
// decides), then 19 payload characters and a check character over the length digits and the payload.
function nameCode(minLength, seed) {
  const len = minLength === undefined ? "" : String(minLength);
  const params = [...len].map((c) => ALPHABET.indexOf(c));
  let x = seed;
  const payload = Array.from({ length: PAYLOAD }, () => {
    x = (x * 48271) % 2147483647;
    return x % BASE;
  });
  const body = [...payload, checkValue([...params, ...payload])].map((v) => ALPHABET[v]).join("");
  return `SN${len}-${body.match(/.{5}/g).join("-")}`;
}

// The service's answer for a name; the page shows only this price, never a list.
const PRICES = { 6: 40000, 7: 20000, 8: 10000 };

function priceOf(label) {
  return PRICES[Math.min(label.length, 8)];
}

function dollars(minor, cents = false) {
  const n = minor / 100;
  return cents ? `$${n.toLocaleString("en-US", { minimumFractionDigits: 2 })}` : `$${n.toLocaleString("en-US")}`;
}

function panel(...kids) {
  return el("section", { class: "panel" }, ...kids);
}

function back() {
  return el("button", { class: "back", type: "button" }, chevronLeft(), "Back");
}

function notice(cls, title, ...lines) {
  return el("div", { class: cls }, el("span", { class: "title" }, title), ...lines.map((l) => el("p", {}, l)));
}

function field(label, value) {
  return el("div", { class: "rows field" }, el("span", { class: "label" }, label), el("div", { class: "mono" }, value));
}

function reference() {
  return field("Reference", ORDER_ID);
}

function rows(pairs, total) {
  return el("div", { class: "rows" },
    ...pairs.map(([k, v]) => el("div", { class: "row" }, el("span", {}, k), el("span", {}, v))),
    el("div", { class: "row total" }, el("span", {}, "Total"), el("span", {}, total)));
}

function qr(payload, label, caption) {
  return el("div", { class: "qr-wrap" }, qrSvg(payload, label), el("p", { class: "muted" }, caption));
}

function link(text) {
  return el("a", { class: "link", href: "#" }, text);
}

// The invest block's place under the main button, so the button stands where it does on the badge pages.
function slot(...kids) {
  return el("div", { class: "invest" }, ...kids);
}

// The names store's invest block: the badge store's sentence and Wefunder link with the names amount,
// and "Check your code" under it.
function namesInvest() {
  return slot(
    el("p", {},
      el("span", { class: "half" }, `Invest ${NAME_INVEST}+ in SimpleX Chat`), " ",
      el("span", { class: "half" }, "and get a free SimpleX name.")),
    el("a", { class: "invest-link", href: "#", "aria-label": s.INVEST_LABEL }, "Learn more on ", wefunderMark()),
    el("p", { class: "check-code" }, link("Check your code")));
}

// ---------------------------------------------------------------- search

const SVG_NS = "http://www.w3.org/2000/svg";

function magnifier() {
  const svg = document.createElementNS(SVG_NS, "svg");
  svg.setAttribute("viewBox", "0 0 24 24");
  svg.setAttribute("aria-hidden", "true");
  const circle = document.createElementNS(SVG_NS, "circle");
  for (const [k, v] of Object.entries({ cx: "10.5", cy: "10.5", r: "6.5", fill: "none", stroke: "currentColor", "stroke-width": "2.4" })) circle.setAttribute(k, v);
  const line = document.createElementNS(SVG_NS, "line");
  for (const [k, v] of Object.entries({ x1: "15.5", y1: "15.5", x2: "21", y2: "21", stroke: "currentColor", "stroke-width": "2.4", "stroke-linecap": "round" })) line.setAttribute(k, v);
  svg.append(circle, line);
  return svg;
}

function searchBox(label, tone) {
  const input = el("input", {
    class: "mono", type: "search", value: label, placeholder: "yourname",
    "aria-label": "Name", autocomplete: "off", spellcheck: "false", enterkeyhint: "search",
  });
  return el("div", { class: "rows field" },
    el("div", { class: `name-box${tone ? ` ${tone}` : ""}` },
      input, el("span", { class: "tld mono" }, ".simplex"),
      el("button", { class: "glass", type: "button", "aria-label": "Search" }, magnifier())));
}

function avatar(initials, hue) {
  return el("span", { class: "avatar", style: `--hue:${hue}`, "aria-hidden": "true" }, initials);
}

function entity({ initials, hue, name, kind, link: url }) {
  return el("div", { class: "entity" },
    avatar(initials, hue),
    el("div", { class: "entity-main" },
      el("div", { class: "entity-name" }, name),
      el("div", { class: "entity-kind" }, kind),
      el("div", { class: "entity-link mono" }, url)),
    el("a", { class: "secondary inline", href: "#" }, "Open in SimpleX"));
}

const BAKERY = [
  { initials: "AB", hue: 28, name: "Alice's Bakery", kind: "contact address", link: "https://smp8.simplex.im/a#Y8m3qT0vLw2…" },
  { initials: "BN", hue: 200, name: "Bakery news", kind: "channel", link: "https://smp9.simplex.im/c#Rk27xN4pDe9…" },
];

// an investor code: any name, for 7 years
const APPLIED = nameCode(undefined, 101);
const APPLIED_YEARS = 7;

// state: empty | typed | available | coded | short | chars | taken | unlinked | reserved | pending | unchecked | limited | credit | fromApp
function search(state, label = "") {
  const tone = { available: "ok", coded: "ok", credit: "ok", fromApp: "ok", short: "bad", chars: "bad", taken: "bad", unlinked: "bad", reserved: "bad", pending: "bad" }[state];
  const p = panel(el("h1", {}, "Your SimpleX domain"),
    el("p", { class: "lede" },
      el("span", { class: "half" }, "One name for your public channel"), " ",
      el("span", { class: "half" }, "and contact address.")));
  if (state === "credit") {
    p.append(notice("notice", "Your payment is kept: $200.00",
      "Choose another name of up to $200. It is registered with what you paid."));
  }
  if (state === "fromApp") {
    p.append(notice("notice", "Opened from SimpleX", `The name is registered to your app's wallet, ${APP_OWNER_SHORT}.`));
  }
  p.append(searchBox(label, tone));
  const rule = (text) => el("p", { class: "name-status bad", role: "status" }, text);
  const result = (cls, ...kids) => el("div", { class: `result${cls ? ` ${cls}` : ""}` }, ...kids);
  let action = button("Search", noop);
  switch (state) {
    case "empty":
      action.setAttribute("disabled", "");
      break;
    case "typed":
      break;
    case "available":
    case "credit":
    case "fromApp":
      p.append(result("ok",
        el("div", { class: "result-title" }, `✓ ${label}.simplex is available`),
        el("div", { class: "result-price" }, dollars(priceOf(label)), el("span", {}, " for 2 years")),
        ...(state === "credit" ? [el("div", { class: "result-note" }, "Covered by your payment.")] : [])));
      action = button(state === "credit" ? "Register with your payment" : `Register ${label}.simplex`, noop);
      break;
    case "coded":
      p.append(result("ok",
        el("div", { class: "result-title" }, `✓ ${label}.simplex is available`),
        el("div", { class: "result-price" }, el("s", { class: "was" }, dollars(priceOf(label))), " $0", el("span", {}, ` for ${APPLIED_YEARS} years`)),
        el("div", { class: "result-note" }, `Covered by your code ${APPLIED}. `, link("Remove"))));
      action = button("Register with code", noop);
      break;
    case "short":
      p.append(rule(`${label.length} letter${label.length === 1 ? "" : "s"}. A name has 6 letters or more: a–z, 0–9, and hyphens inside it.`));
      break;
    case "chars":
      p.append(rule("Use only a–z, 0–9, and hyphens inside the name, not two at places 3 and 4."));
      break;
    case "taken":
      p.append(result("", el("div", { class: "result-title bad" }, `${label}.simplex is taken`),
        el("span", { class: "label" }, "Used by"), ...BAKERY.map(entity)));
      action.setAttribute("disabled", "");
      break;
    case "unlinked":
      p.append(result("", el("div", { class: "result-title bad" }, `${label}.simplex is taken`),
        el("div", { class: "result-note" }, "It is registered, and does not point at any address or channel yet.")));
      action.setAttribute("disabled", "");
      break;
    case "reserved":
      p.append(result("", el("div", { class: "result-title bad" }, `${label}.simplex is reserved`),
        el("div", { class: "result-note" }, "Reserved names are not sold here. If it is yours to use, contact the SimpleX team."),
        el("a", { class: "secondary inline", href: "#" }, "Contact the SimpleX team")));
      action.setAttribute("disabled", "");
      break;
    case "pending":
      p.append(result("", el("div", { class: "result-title bad" }, `${label}.simplex is being registered`),
        el("div", { class: "result-note" }, "Another order holds it. If that order lapses, it can be searched again.")));
      action.setAttribute("disabled", "");
      break;
    case "unchecked":
      p.append(notice("notice", "This name could not be checked", "The name service did not answer. Try again in a moment."));
      break;
    case "limited":
      p.append(notice("notice", "Try again in 42 seconds", "Too many names were searched from here. Searching is paused until then."));
      action.setAttribute("disabled", "");
      break;
  }
  p.append(action, state === "fromApp" || state === "credit" ? slot() : namesInvest());
  return p;
}

// ---------------------------------------------------------------- the code popup

// state: valid | used | invalid | adding
function codePopup(state) {
  const code = { valid: APPLIED, used: nameCode(6, 202), invalid: "SN7-4K2P7-TQ9M1-ZX3RB-8HJ5X", adding: nameCode(6, 505) }[state];
  const ok = state === "valid" || state === "adding";
  const title = state === "adding" ? "Check a code" : "Check your code";
  const box = el("div", { class: "popup", role: "dialog", "aria-label": title },
    el("div", { class: "popup-head" }, el("span", { class: "popup-title" }, title),
      el("button", { class: "popup-close", type: "button", "aria-label": "Close" }, "✕")),
    el("p", { class: "muted popup-lede" }, "A code from SimpleX, for example for investors. Nothing is spent until a name is registered with it."),
    el("div", { class: `name-box${ok ? " ok" : " bad"}` },
      el("input", { class: "mono", type: "text", value: code, "aria-label": "Code", spellcheck: "false" })));
  const result = (good, title, note) => el("div", { class: `result${good ? " ok" : ""}` },
    el("div", { class: `result-title${good ? "" : " bad"}` }, title), el("div", { class: "result-note" }, note));
  const answer = {
    valid: [result(true, "✓ Unused", `Covers any name, for ${APPLIED_YEARS} years.`), "Use this code"],
    adding: [result(true, "✓ Unused", "Covers a name of 6 or more letters, for 2 years."), "Add to your codes"],
    used: [result(false, "This code was already used", "Each code registers one name."), "Use this code"],
    invalid: [result(false, "This code is not valid", "Check it for a mistyped character."), "Use this code"],
  }[state];
  const action = button(answer[1], noop);
  if (!ok) action.setAttribute("disabled", "");
  box.append(answer[0], action);
  return box;
}

// ---------------------------------------------------------------- checkout

const METHOD_NAMES = { card: "Card", btc: "Bitcoin", xmr: "Monero" };

function methods(selected, unavailable) {
  const choices = el("div", { class: "choices methods" });
  for (const m of ["card", "btc", "xmr"]) {
    const off = m === unavailable;
    const card = el("button", {
      class: "choice method center", type: "button", "aria-pressed": String(m === selected && !off),
      ...(off ? { disabled: "" } : {}),
    }, methodMark(m), el("div", { class: "name" }, METHOD_NAMES[m]));
    if (off) card.append(el("div", { class: "feature" }, "unavailable"));
    choices.append(card);
  }
  return choices;
}

const WALLET_NOTE = "The name is registered to a wallet made in this browser; you get its recovery phrase.";

// opts.code: none | entry | covering | short
function checkout(label, selected, opts = {}) {
  const codeState = opts.code ?? "none";
  // a code entered here that covers the name prices it at $0, as one applied from the popup does
  const covered = codeState === "covering" || codeState === "entry";
  const total = covered ? 0 : priceOf(label);
  const p = panel(back(), el("h1", {}, "Check your order"));
  if (opts.openOrder) {
    p.append(el("p", { class: "row-line" }, link("You have an order waiting to be confirmed")));
  }
  const term = covered ? `${APPLIED_YEARS} years from registration` : "2 years from registration";
  p.append(rows([["Name", `${label}.simplex`], ["Term", term], ...(covered ? [["Code", APPLIED]] : [])], dollars(total, true)));
  if (opts.openOrder) {
    p.append(notice("notice", s.AWAITING_CARD_TITLE,
      "A second order would be a second charge, so this one cannot be started yet.",
      "Open the order above. When its invoice expires, a new one can be started there."));
    return p;
  }
  if (opts.noStorage) {
    p.append(notice("warn", "This browser will not keep your recovery phrase", "It is shown once, when the name is registered. Have paper ready to write it down."));
  }
  if (opts.unavailable) {
    p.append(notice("warn", `${METHOD_NAMES[opts.unavailable]} is temporarily unavailable`, "Try another method, or come back later."));
  }
  if (codeState === "entry" || codeState === "short") {
    const ok = codeState === "entry";
    p.append(el("div", { class: "code-entry" },
      el("span", { class: "label" }, "Your code"),
      el("div", { class: `name-box${ok ? " ok" : " bad"}` }, el("input", { class: "mono", type: "text", value: ok ? APPLIED : nameCode(8, 404), "aria-label": "Code" })),
      el("p", { class: `name-status${ok ? " ok" : " bad"}` }, ok ? `✓ Covers any name, for ${APPLIED_YEARS} years.` : `This code covers names of 8 or more letters; ${label} has ${label.length}.`)));
  }
  if (covered) {
    p.append(button("Register with code", noop), slot(el("p", {}, WALLET_NOTE)));
    return p;
  }
  p.append(el("span", { class: "label standalone" }, "Pay with"), methods(selected, opts.unavailable),
    el("div", { class: "notes slot" }),
    button(`Pay ${dollars(total, true)} with ${METHOD_NAMES[selected]}`, noop),
    slot(...(opts.fromApp || codeState === "short" ? [] : [el("p", { class: "check-code" }, link("I have a code"))]),
      el("p", {}, opts.fromApp ? `The name is registered to your app's wallet, ${APP_OWNER_SHORT}.` : WALLET_NOTE)));
  return p;
}

function priceChanged() {
  return panel(el("h1", { class: "tight" }, "The price changed"),
    notice("notice", "dynamis.simplex costs $250.00 now", "The price changed while you were deciding.", "Nothing was charged."),
    button("Search again", noop));
}

function invoiceFailure() {
  return panel(back(), el("h1", {}, "That did not go through"),
    el("p", { class: "lede" }, el("span", { class: "half" }, "The order was not created,"), " ", el("span", { class: "half" }, "and nothing was charged.")),
    el("p", { class: "lede" }, "If this happens again, get in touch."),
    button("Try again", noop),
    slot());
}

// ---------------------------------------------------------------- payment, shared with badges

const ORDER = { orderId: ORDER_ID, badgeType: "", months: 0, createdAt: new Date(NOW).toISOString(), status: "open" };
const CRYPTO = {
  status: "open", amount: 20000, currency: "usd", address: "bc1q8n4xqs0v2kr7w5mxj9d3tlh6c0yzpe2a7kqfuv",
  cryptoAmount: "0.00312", cryptoCurrency: "btc", expiresAt: new Date(NOW + HOLD_MS).toISOString(),
};

function awaiting(invoice, now = NOW) {
  return s.awaitingPayment({ order: ORDER, invoice, method: "btc", nowMs: now, now: () => now, resumed: false, onCancel: async () => {} }).node;
}

function cardForm(label) {
  const mount = el("div", { class: "card-mount stripe-standin" },
    el("span", { class: "sl" }, "Card number"), el("div", { class: "sf" }, "1234 1234 1234 1234"),
    el("div", { class: "sr" },
      el("div", {}, el("span", { class: "sl" }, "Expiry"), el("div", { class: "sf" }, "MM / YY")),
      el("div", {}, el("span", { class: "sl" }, "CVC"), el("div", { class: "sf" }, "CVC"))),
    el("span", { class: "sl" }, "Country"), el("div", { class: "sf" }, "United Kingdom"));
  return panel(el("h1", { class: "tight" }, "Pay by card"),
    rows([["Name", `${label}.simplex`], ["Term", "2 years from registration"]], dollars(priceOf(label), true)),
    el("div", { class: "card-fields" }, mount, button(`Pay ${dollars(priceOf(label), true)}`, noop)),
    reference(),
    el("p", { class: "row-line" }, button(s.CANCEL_INVOICE, noop, "link danger")));
}

// ---------------------------------------------------------------- registration

// Each step: pending | active | done | failed
function steps(label, states, failedNote, owner = OWNER_SHORT) {
  const NAMES = [["Committing", "Committed"], ["Waiting 60s", "Waited 60s"], ["Registering", "Registered"]];
  const TX = ["0x8f3a…c21d", null, "0x2b71…9e04"];
  const list = el("div", { class: "steps" }, el("span", { class: "label" }, `${label}.simplex`));
  states.forEach((st, i) => {
    const name = st === "done" ? NAMES[i][1] : NAMES[i][0];
    const mark = { pending: "", done: "✓", failed: "✕" }[st];
    const row = el("div", { class: `step-row ${st}` },
      el("span", { class: "step-mark", "aria-hidden": "true" }, ...(st === "active" ? [el("span", { class: "pulse" })] : [mark])),
      el("span", { class: "step-name" }, name));
    // a failed step links no transaction: N8's dry run sent none, and N9's commit never landed
    if (TX[i] && (st === "done" || (st === "active" && i === 0))) row.append(el("a", { class: "step-tx mono", href: "#" }, `${TX[i]} ↗`));
    list.append(row);
  });
  list.append(el("div", { class: "step-owner" }, el("span", {}, "Owner"), el("span", { class: "mono" }, owner)));
  if (failedNote) list.append(el("p", { class: "step-note" }, failedNote));
  return list;
}

function registering(label, states) {
  const active = states.indexOf("active");
  const lede = ["Committing the name on the blockchain.", "Committed. The contract makes every registration wait a minute.", "Registering the name to your wallet."][active];
  return panel(el("h1", { class: "tight" }, `Registering ${label}.simplex`),
    el("p", { class: "lede" }, lede),
    steps(label, states),
    el("p", { class: "muted" }, "About two minutes. You can close this page: the registration carries on, and this link brings you back."),
    reference());
}

function phraseGrid() {
  return el("ol", { class: "phrase" }, ...PHRASE.map((w) => el("li", {}, el("span", { class: "mono" }, w))));
}

function registered(label, saved, years = 2) {
  const p = panel(el("div", { class: "tick" }, "✓"),
    el("h1", { class: "tight center" }, `${label}.simplex is yours`),
    el("p", { class: "lede center" }, `Registered for ${years} years, until ${years === 2 ? UNTIL : CODE_UNTIL}, to ${OWNER_SHORT}.`),
    el("span", { class: "label standalone" }, "Recovery phrase"),
    phraseGrid(),
    button("Copy phrase", noop, "primary outline"));
  const details = el("div", { class: "details" },
    el("div", { class: "rows plain" }, el("span", { class: "label" }, "Import it in the app"),
      el("div", {}, "In the SimpleX app, import a name and scan this code.")),
    notice("notice", "This phrase owns the name.", "Whoever has it controls the name. Nobody else has a copy, not even us: write it down."),
    saved
      ? notice("warn", "This is the only copy.", "Saved in this browser and nowhere else.", "Clearing the browser loses it.")
      : notice("warn", "This phrase could not be saved in this browser.", "Write it down now. It is shown here and nowhere else."));
  p.append(el("div", { class: "split" }, qr(PHRASE.join(" "), "recovery phrase", "scan it with the app"), details));
  return p;
}

function registeredToApp(label) {
  return panel(el("div", { class: "tick" }, "✓"),
    el("h1", { class: "tight center" }, `${label}.simplex is yours`),
    el("p", { class: "lede center" }, `Registered for 2 years, until ${UNTIL}, to your app's wallet.`),
    steps(label, ["done", "done", "done"], undefined, APP_OWNER_SHORT),
    button("Back to SimpleX", noop),
    reference());
}

function takenMeanwhile(label) {
  return panel(el("h1", { class: "tight" }, "Someone registered it first"),
    el("p", { class: "lede" }, `${label}.simplex was registered just before ours. Your payment is kept.`),
    steps(label, ["done", "done", "failed"], "The register transaction was never sent: its dry run found the name no longer free."),
    button("Choose another name", noop),
    button(`Take a code for a name of ${Math.min(label.length, 8)}+ letters`, noop, "secondary"),
    reference());
}

function takenCode(label) {
  const code = nameCode(Math.min(label.length, 8), 909);
  const p = panel(el("div", { class: "tick" }, "✓"),
    el("h1", { class: "tight center" }, "Here is a code instead"),
    el("p", { class: "lede center" }, `For any name of ${Math.min(label.length, 8)} or more letters, for 2 years: what ${label}.simplex was paid for.`),
    el("div", { class: "code" }, code),
    button("Copy code", noop, "primary outline"));
  p.append(el("div", { class: "split" }, qr(code, "name code", "scan it with the app"),
    el("div", { class: "details" },
      el("div", { class: "rows plain" }, el("span", { class: "label" }, "Use it"),
        el("div", {}, "Search another name here and choose I have a code, or redeem it in the app.")),
      notice("warn", "Keep a copy of this code.", "The service keeps only its hash; this browser keeps the code, under Your names."))));
  return p;
}

function failedAfterPaying(label) {
  return panel(el("h1", { class: "tight" }, "Paid, not registered"),
    el("p", { class: "lede" }, `Your payment is kept and ${label}.simplex is still free. Trying again charges nothing.`),
    steps(label, ["failed", "pending", "pending"], "The blockchain did not take the commit, and the service's own retries gave up."),
    button("Try again", noop),
    reference());
}

function notOnThisDevice(label) {
  return panel(el("h1", { class: "tight" }, "The recovery phrase is not on this device"),
    notice("notice", `${label}.simplex was registered for this order to ${OWNER_SHORT}.`,
      "Its recovery phrase was made in the browser it was bought in, and exists nowhere else. Nobody, not even us, can make another."),
    reference());
}

// ---------------------------------------------------------------- history, and the badges store

function entry({ title, sub, status, tone, method, price, when, phrase, code, open, use }) {
  const meta = el("div", { class: "meta" });
  if (method) meta.append(el("span", { class: "method" }, methodMark(method), METHOD_NAMES[method]));
  if (price) meta.append(el("span", {}, price));
  meta.append(el("span", {}, when));
  const main = el("div", { class: "entry-main" },
    el("div", { class: "entry-row" }, el("div", { class: "name" }, title), el("span", { class: `status ${tone}` }, status)));
  if (sub) main.append(el("div", { class: "entry-sub" }, sub));
  const metaRow = el("div", { class: "entry-row" }, meta);
  if (open) metaRow.append(el("a", { class: "secondary", href: "#" }, "Open"));
  if (use) metaRow.append(el("a", { class: "secondary", href: "#" }, "Use"));
  main.append(metaRow);
  const item = el("li", { class: "entry" }, el("div", { class: "entry-head" }, el("span", { class: "name-mark", "aria-hidden": "true" }, code ? "≡" : "#"), main));
  if (phrase) item.append(el("div", { class: "code-row" }, el("code", { class: "mono" }, "•••• •••• •••• (12 words)"), button("Show phrase", noop, "secondary inline")));
  if (code) item.append(el("div", { class: "code-row" }, el("code", { class: "mono" }, code), button("Copy", noop, "secondary inline")));
  return item;
}

function history() {
  return panel(el("h1", {}, "Your names"),
    el("p", { class: "lede" }, "Every name and code here is in this browser, and nowhere else."),
    el("ul", { class: "entries" },
      entry({ title: "dynamis.simplex", sub: `Until ${UNTIL} · owner ${OWNER_SHORT}`, status: "registered", tone: "settled",
        method: "btc", price: "$200.00", when: "10 October 2026, 12:00", phrase: true }),
      entry({ title: "nebula.simplex", sub: "Committed, waiting for the minute to pass", status: "registering", tone: "pending",
        method: "xmr", price: "$400.00", when: "10 October 2026, 12:17", open: true }),
      entry({ title: "A code", sub: "Covers any name, for 7 years", status: "unused", tone: "settled",
        when: "added 9 October 2026", code: nameCode(undefined, 303), use: true }),
      entry({ title: "orbital.simplex", sub: "Nothing was charged", status: "this invoice expired", tone: "lost",
        method: "btc", price: "$200.00", when: "28 September 2026, 18:03", open: true })),
    button("Check a code", noop, "secondary"),
    el("p", { class: "forget-line" }, button(s.FORGET_EVERYTHING, noop, "link danger")));
}

function badgesLanding() {
  const p = s.landing({ onStart: noop });
  p.append(el("p", { class: "check-code" }, link("SimpleX names are at simplex.domains")));
  return p;
}

// ---------------------------------------------------------------- the app and the operator

function app(...kids) {
  return el("div", { class: "app-screen" }, ...kids);
}

function appImport() {
  return app(el("div", { class: "app-nav" }, "‹ Back"), el("div", { class: "app-title" }, "Import a name"),
    el("div", { class: "app-label" }, "RECOVERY PHRASE"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("span", {}, "12 words, scanned"), el("span", { class: "app-ok" }, "✓"))),
    el("div", { class: "app-label" }, "FOUND"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("b", {}, "dynamis.simplex"), el("span", {}, "until 9 Oct 2028"))),
    el("p", { class: "app-note" }, "The phrase becomes an account of this device's wallet."),
    el("div", { class: "app-button" }, "Import"));
}

function appImported() {
  return app(el("div", { class: "app-nav" }, "‹ Back"), el("div", { class: "app-title" }, "Import a name"),
    el("div", { class: "app-alert" },
      el("b", {}, "The name is in your app"),
      el("p", {}, "dynamis.simplex is yours until 9 Oct 2028, in this device's wallet."),
      el("div", { class: "app-alert-button" }, "OK")));
}

function terminal() {
  const code = nameCode(7, 11);
  return el("pre", { class: "term-screen" },
    "> //issue name 7\n",
    el("span", { class: "out" }, `Code: ${code}\n`),
    "> //issue name years 7\n",
    el("span", { class: "out" }, `Code: ${nameCode(undefined, 17)}\n`),
    "> //issue name 5\n",
    el("span", { class: "out" }, "Usage: //issue supporter|legend|investor [months 1-255] [paid|unpaid|free]\n       //issue name [<length 6-63>] [years <Y>] [paid|unpaid|free]\n       //revoke <code>\n"),
    "> //revoke ", code, "\n",
    el("span", { class: "out" }, "Revoked.\n"));
}

function groupChat() {
  const msg = (who, cls, ...lines) => el("div", { class: `bubble ${cls}` }, el("b", {}, who), ...lines.map((l) => el("div", {}, l)));
  return el("div", { class: "chat-screen" },
    el("div", { class: "chat-title" }, "#SimpleX Store"),
    msg("alice · moderator", "in", "/issue name 6"),
    msg("SimpleX Store", "out", `Code: ${nameCode(6, 23)}`),
    msg("alice · moderator", "in", "/bulk name years 7 count 3"),
    msg("SimpleX Store", "out", nameCode(undefined, 31), nameCode(undefined, 37), nameCode(undefined, 41)),
    msg("bob · member", "in", "/issue name 6"),
    el("div", { class: "chat-note" }, "a member below moderator gets no reply, as for badge codes"));
}

// ---------------------------------------------------------------- the table

// A screen is a panel, or [panel, popup] for a popup drawn over it.
const SCREENS = {
  N1: () => search("empty"),
  N1a: () => search("typed", "dynamis"),
  N1b: () => search("short", "alice"),
  N1c: () => search("chars", "my_name"),
  N2: () => search("available", "dynamis"),
  N2a: () => search("taken", "bakery"),
  N2b: () => search("unlinked", "harbour"),
  N2c: () => search("reserved", "support"),
  N2d: () => search("unchecked", "dynamis"),
  N2e: () => search("limited", "dynamis"),
  N2f: () => search("pending", "orbital"),
  K1: () => [search("available", "dynamis"), codePopup("valid")],
  K2: () => search("coded", "dynamis"),
  K3: () => checkout("dynamis", "btc", { code: "covering" }),
  K4: () => checkout("dynamis", "btc", { code: "entry" }),
  K5: () => [search("available", "dynamis"), codePopup("used")],
  K6: () => [search("available", "dynamis"), codePopup("invalid")],
  K7: () => checkout("dynamis", "btc", { code: "short" }),
  K8: () => registered("dynamis", true, 7),
  N3: () => checkout("dynamis", "btc"),
  N3a: () => checkout("dynamis", "xmr", { unavailable: "btc" }),
  N3b: () => priceChanged(),
  N3c: () => s.rateLimited({ total: "$200.00", method: "btc", seconds: 41, onBack: noop }, noop).node,
  N3d: () => invoiceFailure(),
  N3e: () => checkout("dynamis", "btc", { openOrder: true }),
  N3f: () => checkout("dynamis", "btc", { noStorage: true }),
  N4: () => awaiting(CRYPTO),
  N4a: () => awaiting({ ...CRYPTO, cryptoAmountPaid: "0.00150", cryptoAmountDue: "0.00162" }, NOW + 14 * 60_000),
  N4b: () => s.windowClosed({ order: ORDER, invoice: { status: "expired" }, canceled: true, onNewInvoice: noop }),
  N4c: () => s.windowClosed({ order: ORDER, invoice: { status: "expired" }, onNewInvoice: noop }),
  N4d: () => s.windowClosed({ order: ORDER, invoice: { status: "expired", amount: 20000, currency: "usd", amountPaid: 9615, cryptoAmountPaid: "0.00150", cryptoCurrency: "btc" }, onNewInvoice: noop }),
  N5: () => s.awaitingConfirmation({ order: ORDER, invoice: { status: "open", cryptoAmountPaid: "0.00312", requiredConfirmations: 1 }, method: "btc", gaveUp: false, onCheckAgain: noop }),
  N6: () => registering("dynamis", ["active", "pending", "pending"]),
  N6a: () => registering("dynamis", ["done", "active", "pending"]),
  N6b: () => registering("dynamis", ["done", "done", "active"]),
  N7: () => registered("dynamis", true),
  N7a: () => registered("dynamis", false),
  N7b: () => notOnThisDevice("dynamis"),
  N8: () => takenMeanwhile("dynamis"),
  N8a: () => search("credit", "dynamos"),
  N8b: () => takenCode("dynamis"),
  N9: () => failedAfterPaying("dynamis"),
  N10: () => cardForm("dynamis"),
  N10a: () => s.cardUnavailable({ order: ORDER, reason: "script", onRetry: noop, onNewInvoice: noop }),
  N10b: () => s.awaitingConfirmation({ order: ORDER, invoice: undefined, method: "card", gaveUp: false, onCheckAgain: noop }),
  N10c: () => s.awaitingConfirmation({ order: ORDER, invoice: undefined, method: "card", gaveUp: true, onCheckAgain: noop }),
  N11: () => s.unknownOrder(noop),
  N12: () => history(),
  N13: () => search("empty"),
  N12a: () => [history(), codePopup("adding")],
  L1: () => search("fromApp", "dynamis"),
  L2: () => checkout("dynamis", "btc", { fromApp: true }),
  L3: () => registeredToApp("dynamis"),
  B0: () => badgesLanding(),
  A1: () => appImport(),
  A2: () => appImported(),
  OP1: () => terminal(),
  OP2: () => groupChat(),
};

const NAMES_MENU = ["Register a name", "Your names"];
const NAMES_NEW_INVOICE = "Get a new invoice";

// The shared payment screens say "Buy a new code"; stage 5 gives screens.ts a per-store label, drawn here.
function relabelForNames(screen) {
  screen.querySelectorAll("button").forEach((b) => { if (b.textContent === s.BUY_NEW_CODE) b.textContent = NAMES_NEW_INVOICE; });
  return screen;
}

// The names store's header: the shared chrome, its menu relabelled for names, and the badges store linked from it.
function namesChrome(theme) {
  const chrome = s.chrome({ theme: theme ?? "system", onNewPurchase: noop, onHistory: noop, onTheme: noop, onToggle: noop, onHome: noop });
  chrome.node.querySelectorAll(".menu-item").forEach((item, i) => { item.textContent = NAMES_MENU[i]; });
  chrome.node.querySelector(".menu-section:last-child").append(el("a", { class: "menu-item", href: "#" }, "SimpleX badges ↗"));
  return chrome;
}

export function render(id, theme) {
  const build = SCREENS[id];
  if (build === undefined) throw new Error(`mockups: no screen ${id}`);
  const html = document.documentElement;
  if (theme === "dark") html.setAttribute("data-theme", "dark");
  if (/^(N|K|L)/.test(id)) {
    const built = build();
    const [screen, popup] = Array.isArray(built) ? built : [built, undefined];
    const chrome = namesChrome(theme);
    document.getElementById("chrome").replaceChildren(chrome.node);
    if (id === "N13") chrome.node.querySelector(".menu-button").click();
    document.getElementById("app").replaceChildren(relabelForNames(screen));
    if (popup) document.body.append(el("div", { class: "popup-overlay" }, popup));
  } else if (id === "B0") {
    const chrome = s.chrome({ theme: theme ?? "system", onNewPurchase: noop, onHistory: noop, onTheme: noop, onToggle: noop, onHome: noop });
    document.getElementById("chrome").replaceChildren(chrome.node);
    document.getElementById("app").replaceChildren(build());
  } else {
    document.body.className = "plain";
    document.body.replaceChildren(el("div", { id: "shot" }, build()));
  }
}
