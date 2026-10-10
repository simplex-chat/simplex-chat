// Every screen of the names board, built in the browser on the compiled webapp modules.
// Shared screens call the real `screens.js`; name screens are prototypes for stage 5's `nameScreens.ts`.
import * as s from "./assets/screens.js";
import { chevronLeft, methodMark } from "./assets/icons.js";
import { qrSvg } from "./assets/qr.js";

const { el, button } = s;
const noop = () => {};
const NOW = Date.parse("2026-10-10T12:00:00Z");
const HOLD_MS = (59 * 60 + 59) * 1000;
const ORDER_ID = "q7ZpL2dN9xWc4KfR8tYb1A";
const UNTIL = "10 October 2028";

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

// The proposed name code: "SN", the shortest name, the years and "Y", then 19 payload characters
// and a check character taken over the parameters and the payload.
function nameCode(minLength, seed) {
  const params = [...`${minLength}2Y`].map((c) => ALPHABET.indexOf(c));
  let x = seed;
  const payload = Array.from({ length: PAYLOAD }, () => {
    x = (x * 48271) % 2147483647;
    return x % BASE;
  });
  const body = [...payload, checkValue([...params, ...payload])].map((v) => ALPHABET[v]).join("");
  return `SN${minLength}-2Y-${body.match(/.{5}/g).join("-")}`;
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

function qr(payload, caption) {
  const wrap = el("div", { class: "qr-wrap" }, qrSvg(payload, "code"));
  if (caption) wrap.append(el("p", { class: "muted" }, caption));
  return wrap;
}

// The invest block's place under the main button, so the button stands where it does on the badge screens.
function slot(...kids) {
  return el("div", { class: "invest" }, ...kids);
}

function links(...items) {
  const p = el("p", { class: "store-links" });
  items.forEach((item, i) => {
    if (i > 0) p.append(el("span", { class: "dot", "aria-hidden": "true" }, "·"));
    p.append(el("a", { href: "#" }, item));
  });
  return p;
}

const SECONDARY = () => links("Buy a code for any name of 6+ letters", "SimpleX badges");

// ---------------------------------------------------------------- search

function nameBox(label, tone) {
  const input = el("input", {
    class: "mono", type: "text", value: label, placeholder: "yourname",
    "aria-label": "Name", autocomplete: "off", spellcheck: "false",
  });
  return el("div", { class: "rows field name-field" }, el("span", { class: "label" }, "Name"),
    el("div", { class: `name-box${tone ? ` ${tone}` : ""}` }, input, el("span", { class: "tld mono" }, ".simplex")));
}

function avatar(initials, hue) {
  return el("span", { class: "avatar", style: `--hue:${hue}`, "aria-hidden": "true" }, initials);
}

function entity({ initials, hue, name, kind, link }) {
  return el("div", { class: "entity" },
    avatar(initials, hue),
    el("div", { class: "entity-main" },
      el("div", { class: "entity-name" }, name),
      el("div", { class: "entity-kind" }, kind),
      el("div", { class: "entity-link mono" }, link)),
    el("a", { class: "secondary inline", href: "#" }, "Open in SimpleX"));
}

const BAKERY = [
  { initials: "AB", hue: 28, name: "Alice's Bakery", kind: "contact address", link: "https://smp8.simplex.im/a#Y8m3qT0vLw2…" },
  { initials: "BN", hue: 200, name: "Bakery news", kind: "channel", link: "https://smp9.simplex.im/c#Rk27xN4pDe9…" },
];

// state: empty | typed | available | short | chars | taken | unlinked | reserved | unchecked | limited | credit
function search(state, label = "") {
  const tone = { available: "ok", credit: "ok", short: "bad", chars: "bad", taken: "bad", unlinked: "bad", reserved: "bad" }[state];
  const p = panel(el("h1", {}, "Your SimpleX domain"),
    el("p", { class: "lede" },
      el("span", { class: "half" }, "One name for your public channel"), " ",
      el("span", { class: "half" }, "and contact address.")));
  if (state === "credit") {
    p.append(notice("notice", "Your payment is kept: $200.00",
      "Choose another name of up to $200. It is registered with what you paid."));
  }
  p.append(nameBox(label, tone));
  const status = (cls, text) => el("p", { class: `name-status${cls ? ` ${cls}` : ""}`, role: "status" }, text);
  let action = button("Search", noop);
  switch (state) {
    case "empty":
      p.append(status(null, "6 letters or more: a–z, 0–9, and hyphens inside the name."));
      action.setAttribute("disabled", "");
      break;
    case "typed":
      p.append(status(null, `${label.length} letters. Nothing is looked up until you search.`));
      break;
    case "available":
    case "credit":
      p.append(el("div", { class: "result ok" },
        el("div", { class: "result-title" }, `✓ ${label}.simplex is available`),
        el("div", { class: "result-price" }, dollars(priceOf(label)), el("span", {}, " for 2 years")),
        ...(state === "credit" ? [el("div", { class: "result-note" }, "Covered by your payment.")] : [])));
      action = button(state === "credit" ? "Register with your payment" : `Register ${label}.simplex`, noop);
      break;
    case "short":
      p.append(status("bad", `${label.length} letters. A name has 6 letters or more.`));
      action.setAttribute("disabled", "");
      break;
    case "chars":
      p.append(status("bad", "Use only a–z, 0–9, and single hyphens inside the name."));
      action.setAttribute("disabled", "");
      break;
    case "taken":
      p.append(el("div", { class: "result" },
        el("div", { class: "result-title bad" }, `${label}.simplex is taken`),
        el("span", { class: "label" }, "Used by"),
        ...BAKERY.map(entity)));
      action.setAttribute("disabled", "");
      break;
    case "unlinked":
      p.append(el("div", { class: "result" },
        el("div", { class: "result-title bad" }, `${label}.simplex is taken`),
        el("div", { class: "result-note" }, "It is registered, and does not point at any address or channel yet.")));
      action.setAttribute("disabled", "");
      break;
    case "reserved":
      p.append(el("div", { class: "result" },
        el("div", { class: "result-title bad" }, `${label}.simplex is reserved`),
        el("div", { class: "result-note" }, "Reserved names are not sold here. If it is yours to use, contact the SimpleX team."),
        el("a", { class: "secondary inline", href: "#" }, "Contact the SimpleX team")));
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
  p.append(action, slot(SECONDARY()));
  return p;
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

function checkout({ what, total, selected, unavailable, openOrder, note }) {
  const p = panel(back(), el("h1", {}, "Check your order"));
  if (openOrder) {
    p.append(el("p", { class: "row-line" }, el("a", { class: "link", href: "#" }, "You have an order waiting to be confirmed")));
  }
  p.append(rows([...what, ["Term", "2 years from registration"]], dollars(total, true)));
  if (openOrder) {
    p.append(notice("notice", s.AWAITING_CARD_TITLE,
      "A second order would be a second charge, so this one cannot be started yet.",
      "Open the order above. When its invoice expires, a new one can be started there."));
    return p;
  }
  if (unavailable) {
    p.append(notice("warn", `${METHOD_NAMES[unavailable]} is temporarily unavailable`, "Try another method, or come back later."));
  }
  p.append(el("span", { class: "label standalone" }, "Pay with"), methods(selected, unavailable),
    el("div", { class: "notes slot" }, ...(selected === "card" ? [el("p", { class: "muted" }, "Card payments are processed by Stripe.")] : [])),
    button(`Pay ${dollars(total, true)} with ${METHOD_NAMES[selected]}`, noop),
    slot(el("p", { class: "muted" }, note)));
  return p;
}

function nameCheckout(label, selected, opts = {}) {
  return checkout({
    what: [["Name", `${label}.simplex`]], total: priceOf(label), selected, ...opts,
    note: "The name is registered for you as soon as the payment confirms.",
  });
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

function awaiting(invoice) {
  return s.awaitingPayment({ order: ORDER, invoice, method: "btc", nowMs: NOW, now: () => NOW, resumed: false, onCancel: async () => {} }).node;
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
function steps(label, states, failedNote) {
  const NAMES = [["Committing", "Committed"], ["Waiting 60s", "Waited 60s"], ["Registering", "Registered"]];
  const TX = ["0x8f3a…c21d", null, "0x2b71…9e04"];
  const list = el("div", { class: "steps" }, el("span", { class: "label" }, `${label}.simplex`));
  states.forEach((st, i) => {
    const name = st === "done" ? NAMES[i][1] : NAMES[i][0];
    const mark = { pending: "", active: "", done: "✓", failed: "✕" }[st];
    const row = el("div", { class: `step-row ${st}` },
      el("span", { class: "step-mark", "aria-hidden": "true" }, ...(st === "active" ? [el("span", { class: "pulse" })] : [mark])),
      el("span", { class: "step-name" }, name));
    // a failed step sent nothing: its dry run stopped it
    if (TX[i] && (st === "done" || (st === "active" && i === 0))) row.append(el("a", { class: "step-tx mono", href: "#" }, `${TX[i]} ↗`));
    list.append(row);
  });
  if (failedNote) list.append(el("p", { class: "step-note" }, failedNote));
  return list;
}

function registering(label, states) {
  const active = states.indexOf("active");
  const lede = ["Paid. Committing the name on the blockchain.", "Committed. The contract makes every registration wait a minute.", "Registering the name to hold it for you."][active];
  return panel(el("h1", { class: "tight" }, `Registering ${label}.simplex`),
    el("p", { class: "lede" }, lede),
    steps(label, states),
    el("p", { class: "muted" }, "About two minutes. You can close this page: the registration carries on, and this link brings you back."),
    reference());
}

function registered(label, saved) {
  const code = nameCode(Math.min(label.length, 8), 7);
  const p = panel(el("div", { class: "tick" }, "✓"),
    el("h1", { class: "tight center" }, `${label}.simplex is yours`),
    el("p", { class: "lede center" }, `Registered for 2 years, until ${UNTIL}.`),
    el("div", { class: "code" }, code),
    button("Open in SimpleX", noop),
    button("Copy code", noop, "secondary"));
  const details = el("div", { class: "details" },
    el("div", { class: "rows plain" }, el("span", { class: "label" }, "Claim it in the app"),
      el("div", {}, `Open the link above, or search ${label}.simplex in the app and choose Claim with a code.`)),
    notice("notice", "It points nowhere yet.", "Claiming moves it to your app and points it at your address or channel. The 2 years have started."),
    saved
      ? notice("warn", "This is the only copy.", "Saved in this browser and nowhere else.", "Anyone using this browser can read it, and clearing the browser loses it.")
      : notice("warn", "This code could not be saved in this browser.", "Copy it now. It is shown here and nowhere else."));
  p.append(el("div", { class: "split" }, qr(code, "scan to carry it to your phone"), details));
  return p;
}

function takenMeanwhile(label) {
  return panel(el("h1", { class: "tight" }, "Someone registered it first"),
    el("p", { class: "lede" }, `${label}.simplex was registered just before ours. Your payment is kept.`),
    steps(label, ["done", "done", "failed"], "Registering stopped before anything was spent on it: the name was no longer free."),
    button("Choose another name", noop),
    button(`Take a code for any name of ${Math.min(label.length, 8)}+ letters`, noop, "secondary"),
    reference());
}

function failedAfterPaying(label) {
  return panel(el("h1", { class: "tight" }, "Paid, not registered"),
    el("p", { class: "lede" }, `Your payment is kept and ${label}.simplex is still free. Trying again charges nothing.`),
    steps(label, ["failed", "pending", "pending"], "The blockchain did not take the commit."),
    button("Try again", noop),
    reference());
}

function paidElsewhere(label) {
  return panel(el("h1", { class: "tight" }, "The claim code is not on this device"),
    notice("notice", `${label}.simplex was registered for this order until ${UNTIL}.`,
      "Its claim code was made in the browser it was bought in, and is not stored anywhere else.",
      "Quote the reference below and we will sort it out."),
    reference());
}

// ---------------------------------------------------------------- the secondary code

function codeLength() {
  const choices = el("div", { class: "choices" });
  [[8, "8+ letters"], [7, "7+ letters"], [6, "6+ letters"]].forEach(([n, name]) => {
    choices.append(el("button", { class: "choice term", type: "button", "aria-pressed": String(n === 7) },
      el("div", { class: "name" }, name), el("div", { class: "price" }, dollars(PRICES[n]))));
  });
  return panel(back(), el("h1", {}, "A code for any name"),
    el("p", { class: "lede" },
      el("span", { class: "half" }, "For a name you choose later, in the app."), " ",
      el("span", { class: "half" }, "It covers names of this length or longer, for 2 years.")),
    choices,
    button("Continue", noop),
    slot(links("Search a name instead")));
}

function codeCheckout() {
  return checkout({
    what: [["A code for", "names of 7+ letters"]], total: PRICES[7], selected: "xmr",
    note: "Register any name of 7 or more letters with it in the app.",
  });
}

function codeIssued() {
  const code = nameCode(7, 13);
  const p = panel(el("div", { class: "tick" }, "✓"),
    el("h1", { class: "tight center" }, "Paid. Here is your code."),
    el("div", { class: "code" }, code),
    button("Open in SimpleX", noop),
    button("Copy code", noop, "secondary"));
  const details = el("div", { class: "details" },
    el("div", { class: "rows plain" }, el("span", { class: "label" }, "Register a name with it"),
      el("div", {}, "In the app, search a name of 7 or more letters and choose Use a code.")),
    notice("warn", "This is the only copy.", "Saved in this browser and nowhere else.", "Anyone using this browser can read it, and clearing the browser loses it."));
  p.append(el("div", { class: "split" }, qr(code, "scan to carry it to your phone"), details));
  return p;
}

// ---------------------------------------------------------------- history, and the badges store

function entry({ title, sub, status, tone, method, price, when, code, open }) {
  const meta = el("div", { class: "meta" },
    el("span", { class: "method" }, methodMark(method), METHOD_NAMES[method]),
    el("span", {}, price), el("span", {}, when));
  const main = el("div", { class: "entry-main" },
    el("div", { class: "entry-row" }, el("div", { class: "name" }, title), el("span", { class: `status ${tone}` }, status)));
  if (sub) main.append(el("div", { class: "entry-sub" }, sub));
  const metaRow = el("div", { class: "entry-row" }, meta);
  if (open) metaRow.append(el("a", { class: "secondary", href: "#" }, "Open"));
  main.append(metaRow);
  const item = el("li", { class: "entry" }, el("div", { class: "entry-head" }, el("span", { class: "name-mark", "aria-hidden": "true" }, "#"), main));
  if (code) item.append(el("div", { class: "code-row" }, el("code", { class: "mono" }, code), button("Copy", noop, "secondary inline")));
  return item;
}

function history() {
  return panel(el("h1", {}, "Your names"),
    el("p", { class: "lede" }, "Every name and code you bought is in this browser, and nowhere else."),
    el("ul", { class: "entries" },
      entry({ title: "dynamis.simplex", sub: `Registered until ${UNTIL} · claim code`, status: "registered", tone: "settled",
        method: "btc", price: "$200.00", when: "10 October 2026, 12:18", code: nameCode(7, 7) }),
      entry({ title: "nebula.simplex", sub: "Committed, waiting for the minute to pass", status: "registering", tone: "pending",
        method: "xmr", price: "$400.00", when: "10 October 2026, 11:52", open: true }),
      entry({ title: "A code for any name", sub: "7+ letters · 2 years", status: "paid", tone: "settled",
        method: "card", price: "$200.00", when: "3 October 2026, 09:40", code: nameCode(7, 13) }),
      entry({ title: "orbital.simplex", sub: "Nothing was charged", status: "this invoice expired", tone: "lost",
        method: "btc", price: "$200.00", when: "28 September 2026, 18:03", open: true })),
    el("p", { class: "forget-line" }, button(s.FORGET_EVERYTHING, noop, "link danger")));
}

function badgesLanding() {
  const p = s.landing({ onStart: noop });
  p.append(links("SimpleX names are at simplex.domains"));
  return p;
}

// ---------------------------------------------------------------- the app and the operator

function app(...kids) {
  return el("div", { class: "app-screen" }, ...kids);
}

function appClaim() {
  return app(el("div", { class: "app-nav" }, "‹ Back"), el("div", { class: "app-title" }, "Claim a name"),
    el("div", { class: "app-label" }, "THE NAME"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("b", {}, "dynamis.simplex"), el("span", { class: "app-ok" }, "held for you"))),
    el("div", { class: "app-label" }, "POINTS AT"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("span", {}, "Alice — your address"), el("span", { class: "app-x" }, "✕"))),
    el("div", { class: "app-label" }, "CODE"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("span", {}, "Your claim code"), el("span", {}, "until 10 Oct 2028"))),
    el("p", { class: "app-note" }, "Bought on simplex.domains. Claiming moves the name to this device's wallet and points it at Alice."),
    el("div", { class: "app-button" }, "Claim"));
}

function appClaimed() {
  return app(el("div", { class: "app-nav" }, "‹ Back"), el("div", { class: "app-title" }, "Claim a name"),
    el("div", { class: "app-alert" },
      el("b", {}, "The name is yours"),
      el("p", {}, "dynamis.simplex points at Alice until 10 Oct 2028."),
      el("div", { class: "app-alert-button" }, "OK")));
}

function terminal() {
  const code = nameCode(7, 11);
  return el("pre", { class: "term-screen" },
    "> //issue name 7\n",
    el("span", { class: "out" }, `Code: ${code}\n`),
    "> //issue name 5\n",
    el("span", { class: "out" }, "Usage: //issue supporter|legend|investor [months 1-255] [paid|unpaid|free]\n       //issue name 6|7|8 [paid|unpaid|free]\n       //revoke <code>\n"),
    "> //revoke ", code, "\n",
    el("span", { class: "out" }, "Revoked.\n"));
}

function groupChat() {
  const msg = (who, cls, ...lines) => el("div", { class: `bubble ${cls}` }, el("b", {}, who), ...lines.map((l) => el("div", {}, l)));
  return el("div", { class: "chat-screen" },
    el("div", { class: "chat-title" }, "#SimpleX Store"),
    msg("alice · moderator", "in", "/issue name 6"),
    msg("SimpleX Store", "out", `Code: ${nameCode(6, 23)}`),
    msg("alice · moderator", "in", "/bulk name 8 count 3"),
    msg("SimpleX Store", "out", nameCode(8, 31), nameCode(8, 37), nameCode(8, 41)),
    msg("bob · member", "in", "/issue name 6"),
    el("div", { class: "chat-note" }, "a member below moderator gets no reply, as for badge codes"));
}

// ---------------------------------------------------------------- the table

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
  N3: () => nameCheckout("dynamis", "btc"),
  N3a: () => nameCheckout("dynamis", "xmr", { unavailable: "btc" }),
  N3b: () => priceChanged(),
  N3c: () => s.rateLimited({ total: "$200.00", method: "btc", seconds: 41, onBack: noop }, noop).node,
  N3d: () => invoiceFailure(),
  N3e: () => nameCheckout("dynamis", "btc", { openOrder: true }),
  N4: () => awaiting(CRYPTO),
  N4a: () => awaiting({ ...CRYPTO, cryptoAmountPaid: "0.00150", cryptoAmountDue: "0.00164" }),
  N4b: () => s.windowClosed({ order: ORDER, invoice: { status: "expired" }, canceled: true, onNewInvoice: noop }),
  N4c: () => s.windowClosed({ order: ORDER, invoice: { status: "expired" }, onNewInvoice: noop }),
  N4d: () => s.windowClosed({ order: ORDER, invoice: { status: "expired", amount: 20000, currency: "usd", amountPaid: 9600, cryptoAmountPaid: "0.00150", cryptoCurrency: "btc" }, onNewInvoice: noop }),
  N5: () => s.awaitingConfirmation({ order: ORDER, invoice: { status: "open", cryptoAmountPaid: "0.00312", requiredConfirmations: 1 }, method: "btc", gaveUp: false, onCheckAgain: noop }),
  N6: () => registering("dynamis", ["active", "pending", "pending"]),
  N6a: () => registering("dynamis", ["done", "active", "pending"]),
  N6b: () => registering("dynamis", ["done", "done", "active"]),
  N7: () => registered("dynamis", true),
  N7a: () => registered("dynamis", false),
  N7b: () => paidElsewhere("dynamis"),
  N8: () => takenMeanwhile("dynamis"),
  N8a: () => search("credit", "dynamos"),
  N9: () => failedAfterPaying("dynamis"),
  N10: () => cardForm("dynamis"),
  N10a: () => s.cardUnavailable({ order: ORDER, reason: "script", onRetry: noop, onNewInvoice: noop }),
  N10b: () => s.awaitingConfirmation({ order: ORDER, invoice: undefined, method: "card", gaveUp: false, onCheckAgain: noop }),
  N10c: () => s.awaitingConfirmation({ order: ORDER, invoice: undefined, method: "card", gaveUp: true, onCheckAgain: noop }),
  N11: () => s.unknownOrder(noop),
  N12: () => history(),
  C1: () => codeLength(),
  C2: () => codeCheckout(),
  C3: () => codeIssued(),
  B0: () => badgesLanding(),
  A1: () => appClaim(),
  A2: () => appClaimed(),
  OP1: () => terminal(),
  OP2: () => groupChat(),
};

const NAMES_MENU = ["Register a name", "Your names"];

// The names store's header: the shared chrome, its menu relabelled for names, and the badges store linked from it.
function namesChrome(theme) {
  const chrome = s.chrome({ theme: theme ?? "system", onNewPurchase: noop, onHistory: noop, onTheme: noop, onToggle: noop, onHome: noop });
  chrome.node.querySelectorAll(".menu-item").forEach((item, i) => { item.textContent = NAMES_MENU[i] ?? item.textContent; });
  chrome.node.querySelector(".menu-section:last-child")?.append(el("a", { class: "menu-item", href: "#" }, "SimpleX badges ↗"));
  return chrome;
}

export function render(id, theme) {
  const build = SCREENS[id];
  if (build === undefined) throw new Error(`mockups: no screen ${id}`);
  const html = document.documentElement;
  if (theme === "dark") { html.setAttribute("data-theme", "dark"); html.classList.add("dark"); html.style.colorScheme = "dark"; }
  if (/^(N|C)/.test(id)) {
    document.getElementById("chrome").replaceChildren(namesChrome(theme).node);
    document.getElementById("app").replaceChildren(build());
  } else if (id === "B0") {
    const chrome = s.chrome({ theme: theme ?? "system", onNewPurchase: noop, onHistory: noop, onTheme: noop, onToggle: noop, onHome: noop });
    document.getElementById("chrome").replaceChildren(chrome.node);
    document.getElementById("app").replaceChildren(build());
  } else {
    document.body.className = "plain";
    document.body.replaceChildren(el("div", { id: "shot" }, build()));
  }
}
