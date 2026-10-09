// Every screen of the names board, built in the browser on the compiled webapp modules.
// Shared screens call the real `screens.js`; name screens are prototypes for stage 4's `nameScreens.ts`.
import * as s from "./assets/screens.js";
import { badgeIcon, chevronLeft, methodMark } from "./assets/icons.js";
import { qrSvg } from "./assets/qr.js";

const { el, button } = s;
const noop = () => {};
const NOW = Date.parse("2026-10-09T12:00:00Z");
const HOLD_MS = (59 * 60 + 59) * 1000;
const ORDER_ID = "q7ZpL2dN9xWc4KfR8tYb1A";

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
function nameCode(minLength, years, seed) {
  const params = [...`${minLength}${years}Y`].map((c) => ALPHABET.indexOf(c));
  let x = seed;
  const payload = Array.from({ length: PAYLOAD }, () => {
    x = (x * 48271) % 2147483647;
    return x % BASE;
  });
  const body = [...payload, checkValue([...params, ...payload])].map((v) => ALPHABET[v]).join("");
  return `SN${minLength}-${years}Y-${body.match(/.{5}/g).join("-")}`;
}

const TIERS = [
  { minLength: 8, label: "8+ letters", yearPrice: 5000 },
  { minLength: 7, label: "7 letters", yearPrice: 10000 },
  { minLength: 6, label: "6 letters", yearPrice: 20000 },
];

function tierOf(label) {
  return Math.min(label.length, 8);
}

function yearPrice(label) {
  return TIERS.find((t) => t.minLength === tierOf(label)).yearPrice;
}

function dollars(minor, cents = false) {
  const n = minor / 100;
  return cents ? `$${n.toLocaleString("en-US", { minimumFractionDigits: 2 })}` : `$${n.toLocaleString("en-US")}`;
}

function covers(label) {
  return tierOf(label) === 8 ? "names of 8+ letters" : `names of ${tierOf(label)}+ letters`;
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

function nameRows(label, years) {
  return rows([
    ["Name", `${label}.simplex`],
    ["Your code covers", covers(label)],
    ["Term", `${years} years`],
  ], dollars(yearPrice(label) * years, true));
}

function qr(payload, caption) {
  const wrap = el("div", { class: "qr-wrap" }, qrSvg(payload, "name code"));
  if (caption) wrap.append(el("p", { class: "muted" }, caption));
  return wrap;
}

function nameMark() {
  return el("span", { class: "name-mark", "aria-hidden": "true" }, "#");
}

// ---------------------------------------------------------------- the name screen

function tierStrip(label) {
  const active = label === undefined ? undefined : tierOf(label);
  return el("div", { class: "tiers" }, ...TIERS.map((t) =>
    el("div", { class: "tier", ...(t.minLength === active ? { "aria-current": "true" } : {}) },
      el("span", { class: "tier-len" }, t.label),
      el("span", { class: "tier-price" }, `${dollars(t.yearPrice)} / year`))));
}

// state: empty | typing | available | short | chars | taken | reserved | unchecked | limited
function nameEntry(state, label = "") {
  const bad = ["short", "chars", "taken", "reserved"].includes(state);
  const input = el("input", {
    class: "mono", type: "text", value: label, placeholder: "yourname",
    "aria-label": "Name", autocomplete: "off", spellcheck: "false",
  });
  const box = el("div", { class: `name-box${bad ? " bad" : state === "available" ? " ok" : ""}` },
    input, el("span", { class: "tld mono" }, ".simplex"));
  const p = panel(back(), el("h1", {}, "Choose your name"),
    el("p", { class: "lede" },
      el("span", { class: "half" }, "The name people find you by,"), " ",
      el("span", { class: "half" }, "and open your address with.")),
    el("div", { class: "rows field name-field" }, el("span", { class: "label" }, "Name"), box));
  const status = {
    empty: [null, "6 letters or more: a–z, 0–9, and hyphens inside the name."],
    typing: [null, `${label.length} letters. Check that nobody has it yet.`],
    available: ["ok", `✓ ${label}.simplex is available`],
    short: ["bad", `${label.length} letters. A name has 6 letters or more.`],
    chars: ["bad", "Use only a–z, 0–9, and single hyphens inside the name."],
    taken: ["bad", `${label}.simplex is registered to someone else.`],
    reserved: ["bad", `${label}.simplex is reserved and cannot be bought.`],
    unchecked: [null, `${label.length} letters.`],
    limited: [null, `${label.length} letters.`],
  }[state];
  p.append(el("p", { class: `name-status${status[0] ? ` ${status[0]}` : ""}`, role: "status" }, status[1]));
  p.append(tierStrip(["empty", "short", "chars"].includes(state) ? undefined : label));
  if (state === "unchecked") {
    p.append(notice("notice", "This name could not be checked", "The name service did not answer. Try again in a moment."));
  }
  if (state === "limited") {
    p.append(notice("notice", "Try again in 42 seconds", "Too many names were checked from here. Checking is paused until then."));
  }
  const action = state === "available" ? button("Continue", noop) : button("Check availability", noop);
  if (["empty", "short", "chars", "taken", "reserved", "limited"].includes(state)) action.setAttribute("disabled", "");
  p.append(action);
  return p;
}

// ---------------------------------------------------------------- the years screen

function nameYears(label, years) {
  const minus = el("button", { class: "step", type: "button", "aria-label": "One year less" }, "−");
  const plus = el("button", { class: "step", type: "button", "aria-label": "One year more" }, "+");
  if (years <= 2) minus.setAttribute("disabled", "");
  if (years >= 10) plus.setAttribute("disabled", "");
  const per = yearPrice(label);
  return panel(back(), el("h1", {}, "How long?"),
    el("p", { class: "lede" },
      el("span", { class: "half" }, `${label}.simplex: ${tierOf(label) === 8 ? "8+ letters" : `${label.length} letters`},`), " ",
      el("span", { class: "half" }, `${dollars(per)} a year.`)),
    el("p", { class: "lede" },
      el("span", { class: "half" }, "Paid once, from 2 to 10 years."), " ",
      el("span", { class: "half" }, "Nothing renews.")),
    el("div", { class: "stepper", role: "group", "aria-label": "Years" },
      minus,
      el("div", { class: "term" }, el("span", { class: "years" }, String(years)), el("span", { class: "unit" }, "years")),
      plus),
    el("div", { class: "term-total" }, dollars(per * years)),
    el("div", { class: "notes" }, el("p", { class: "muted" }, "The years start when you register the name in the app.")),
    button("Continue", noop));
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

function nameSummary(label, years, selected, opts = {}) {
  const total = dollars(yearPrice(label) * years, true);
  const p = panel(back(), el("h1", {}, "Check your order"));
  if (opts.openOrder) {
    p.append(el("p", { class: "row-line" }, el("a", { class: "link", href: "#" },
      opts.awaitingCard ? "You have an order waiting to be confirmed" : "You have an order waiting for payment")));
  }
  p.append(nameRows(label, years));
  if (opts.awaitingCard) {
    p.append(notice("notice", s.AWAITING_CARD_TITLE,
      "A second order would be a second charge, so this one cannot be started yet.",
      "Open the order above. When its invoice expires, a new one can be started there."));
    return p;
  }
  if (opts.unavailable) {
    p.append(notice("warn", `${METHOD_NAMES[opts.unavailable]} is temporarily unavailable`, "Try another method, or come back later."));
  }
  p.append(el("span", { class: "label standalone" }, "Pay with"), methods(selected, opts.unavailable),
    el("div", { class: "notes slot" }, ...(selected === "card" ? [el("p", { class: "muted" }, "Card payments are processed by Stripe.")] : [])),
    button(`Pay ${total} with ${METHOD_NAMES[selected]}`, noop),
    el("p", { class: "muted" }, `You get a code for any name of ${tierOf(label)}+ letters. Register ${label}.simplex with it in the app.`));
  return p;
}

function catalogChanged() {
  return panel(el("h1", { class: "tight" }, "These prices have changed"),
    notice("notice", "Start again with the current prices", "The price changed while you were deciding.", "Nothing was charged."),
    button("Start again", noop));
}

function invoiceFailure() {
  return panel(back(), el("h1", {}, "That did not go through"),
    el("p", { class: "lede" }, el("span", { class: "half" }, "The order was not created,"), " ", el("span", { class: "half" }, "and nothing was charged.")),
    el("p", { class: "lede" }, "If this happens again, get in touch."),
    button("Try again", noop));
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

function cardForm(label, years) {
  const mount = el("div", { class: "card-mount stripe-standin" },
    el("span", { class: "sl" }, "Card number"), el("div", { class: "sf" }, "1234 1234 1234 1234"),
    el("div", { class: "sr" },
      el("div", {}, el("span", { class: "sl" }, "Expiry"), el("div", { class: "sf" }, "MM / YY")),
      el("div", {}, el("span", { class: "sl" }, "CVC"), el("div", { class: "sf" }, "CVC"))),
    el("span", { class: "sl" }, "Country"), el("div", { class: "sf" }, "United Kingdom"));
  const p = panel(el("h1", { class: "tight" }, "Pay by card"), nameRows(label, years),
    el("div", { class: "card-fields" }, mount, button(`Pay ${dollars(yearPrice(label) * years, true)}`, noop)),
    reference(),
    el("p", { class: "row-line" }, button(s.CANCEL_INVOICE, noop, "link danger")));
  return p;
}

// ---------------------------------------------------------------- the code

function nameCodeIssued(label, years, saved) {
  const code = nameCode(tierOf(label), years, 7);
  const p = panel(el("div", { class: "tick" }, "✓"),
    el("h1", { class: "tight center" }, "Paid. Here is your name code."),
    el("div", { class: "code" }, code),
    button("Open in SimpleX", noop),
    button("Copy code", noop, "secondary"));
  const details = el("div", { class: "details" },
    el("div", { class: "rows plain" }, el("span", { class: "label" }, "Register it in the app"),
      el("div", {}, `Open the link above, or search ${label}.simplex in the app and choose Use a code.`)),
    notice("notice", "The name is not reserved yet.",
      `The code covers any name of ${tierOf(label)}+ letters for ${years} years. Until ${label}.simplex is registered, someone else can take it.`),
    saved
      ? notice("warn", "This is the only copy.", "Saved in this browser and nowhere else.", "Anyone using this browser can read it, and clearing the browser loses it.")
      : notice("warn", "This code could not be saved in this browser.", "Copy it now. It is shown here and nowhere else."));
  p.append(el("div", { class: "split" }, qr(code, "scan to carry it to your phone"), details));
  return p;
}

function paidNoCode(label, years) {
  return panel(el("h1", { class: "tight" }, "This code is not on this device"),
    notice("notice", "The code was made in the browser it was bought in, and is not stored anywhere else.", "Quote the reference below and we will sort it out."),
    el("div", { class: "rows field" },
      el("div", { class: "name" }, `${label}.simplex · ${tierOf(label)}+ letters · ${years} years`),
      el("p", { class: "muted" }, "paid 9 October")),
    reference());
}

// ---------------------------------------------------------------- history

function entry({ art, title, sub, status, tone, method, price, when, code, open }) {
  const meta = el("div", { class: "meta" },
    el("span", { class: "method" }, methodMark(method), METHOD_NAMES[method]),
    el("span", {}, price), el("span", {}, when));
  const main = el("div", { class: "entry-main" },
    el("div", { class: "entry-row" }, el("div", { class: "name" }, title), el("span", { class: `status ${tone}` }, status)));
  if (sub) main.append(el("div", { class: "entry-sub" }, sub));
  const metaRow = el("div", { class: "entry-row" }, meta);
  if (open) metaRow.append(el("a", { class: "secondary", href: "#" }, "Open"));
  main.append(metaRow);
  const item = el("li", { class: "entry" }, el("div", { class: "entry-head" }, art, main));
  if (code) item.append(el("div", { class: "code-row" }, el("code", { class: "mono" }, code), button("Copy", noop, "secondary inline")));
  return item;
}

function history() {
  const list = el("ul", { class: "entries" },
    entry({ art: nameMark(), title: "dynamis.simplex", sub: "Name code · 7+ letters · 2 years", status: "paid", tone: "settled",
      method: "btc", price: "$200.00", when: "9 October 2026, 12:18", code: nameCode(7, 2, 7) }),
    entry({ art: nameMark(), title: "nebula.simplex", sub: "Name code · 6+ letters · 3 years", status: "waiting for payment", tone: "pending",
      method: "xmr", price: "$600.00", when: "9 October 2026, 11:52", open: true }),
    entry({ art: badgeIcon("legend"), title: "Legend, 12 months", status: "paid", tone: "settled",
      method: "card", price: "$420.00", when: "2 October 2026, 09:40", code: "SB-JKN2E-E888G-5KK16-KZAK5" }),
    entry({ art: nameMark(), title: "orbital.simplex", sub: "Name code · 7+ letters · 2 years", status: "this invoice expired", tone: "lost",
      method: "btc", price: "$200.00", when: "28 September 2026, 18:03", open: true }));
  return panel(el("h1", {}, "Your codes"),
    el("p", { class: "lede" }, "Every code you bought is in this browser, and nowhere else."),
    list,
    el("p", { class: "forget-line" }, button(s.FORGET_EVERYTHING, noop, "link danger")));
}

// ---------------------------------------------------------------- landing

function landing() {
  const p = panel(el("h1", {}, "Support SimpleX"),
    el("p", { class: "lede" },
      el("span", { class: "line" }, "Get a badge for larger files (2‑5GB),"), " ",
      el("span", { class: "line" }, "or a SimpleX name that people find you by."),
    ),
    el("div", { class: "hero", role: "presentation" }),
    el("div", { class: "notes" }, el("p", { class: "muted" },
      el("span", { class: "half" }, "You pay once for the time you choose."), " ",
      el("span", { class: "half" }, "No subscription, no account."))),
    button("Choose your badge", noop),
    button("Get a SimpleX name", noop, "primary outline next"));
  const invest = s.investPanel(undefined);
  if (invest) p.append(invest);
  return p;
}

// ---------------------------------------------------------------- the app and the operator

function app(...kids) {
  return el("div", { class: "app-screen" }, ...kids);
}

function appRegister() {
  return app(el("div", { class: "app-nav" }, "‹ Back"), el("div", { class: "app-title" }, "Register a name"),
    el("div", { class: "app-label" }, "THE NAME"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("b", {}, "dynamis.simplex"), el("span", { class: "app-ok" }, "available"))),
    el("div", { class: "app-label" }, "POINTS AT"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("span", {}, "Alice — your address"), el("span", { class: "app-x" }, "✕"))),
    el("div", { class: "app-label" }, "YOU PAY"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("span", {}, "Your code"), el("span", {}, "2 years"))),
    el("p", { class: "app-note" }, "Bought on the web a moment ago. Nothing to type: the code came back in the link."),
    el("div", { class: "app-button" }, "Register with code"),
    el("div", { class: "app-link" }, "Pay in the app instead"));
}

function appTaken() {
  return app(el("div", { class: "app-nav" }, "‹ Back"), el("div", { class: "app-title" }, "Register a name"),
    el("div", { class: "app-alert" },
      el("b", {}, "dynamis.simplex was just registered"),
      el("p", {}, "Someone registered it after you checked. Your code is kept: choose another name of 7 or more letters."),
      el("div", { class: "app-alert-button" }, "Choose another name")));
}

function terminal() {
  const code = nameCode(7, 3, 11);
  return el("pre", { class: "term-screen" },
    "> //issue name 7 3\n",
    el("span", { class: "out" }, `Code: ${code}\n`),
    "> //issue name 5\n",
    el("span", { class: "out" }, "Usage: //issue supporter|legend|investor [months 1-255] [paid|unpaid|free]\n       //issue name 6|7|8 [years 2-10] [paid|unpaid|free]\n       //revoke <code>\n"),
    "> //revoke ", code, "\n",
    el("span", { class: "out" }, "Revoked.\n"));
}

function groupChat() {
  const msg = (who, cls, ...lines) => el("div", { class: `bubble ${cls}` }, el("b", {}, who), ...lines.map((l) => el("div", {}, l)));
  return el("div", { class: "chat-screen" },
    el("div", { class: "chat-title" }, "#SimpleX Badges"),
    msg("alice · moderator", "in", "/issue name 6 years 2"),
    msg("SimpleX Badges", "out", `Code: ${nameCode(6, 2, 23)}`),
    msg("alice · moderator", "in", "/bulk name 8 count 3"),
    msg("SimpleX Badges", "out", nameCode(8, 2, 31), nameCode(8, 2, 37), nameCode(8, 2, 41)),
    msg("bob · member", "in", "/issue name 6"),
    el("div", { class: "chat-note" }, "a member below moderator gets no reply, as for badge codes"));
}

// ---------------------------------------------------------------- the table

const SCREENS = {
  N0: () => landing(),
  N1: () => nameEntry("empty"),
  N1a: () => nameEntry("typing", "dynamis"),
  N1b: () => nameEntry("available", "dynamis"),
  N1c: () => nameEntry("short", "alice"),
  N1d: () => nameEntry("chars", "my_name"),
  N1e: () => nameEntry("taken", "privacy"),
  N1f: () => nameEntry("reserved", "support"),
  N1g: () => nameEntry("unchecked", "dynamis"),
  N1h: () => nameEntry("limited", "dynamis"),
  N2: () => nameYears("dynamis", 2),
  N2a: () => nameYears("dynamis", 10),
  N2b: () => nameYears("nebula", 3),
  N3: () => nameSummary("dynamis", 2, "btc"),
  N3a: () => nameSummary("dynamis", 2, "xmr", { unavailable: "btc" }),
  N3b: () => catalogChanged(),
  N3c: () => s.rateLimited({ total: "$200.00", method: "btc", seconds: 41, onBack: noop }, noop).node,
  N3d: () => invoiceFailure(),
  N3e: () => nameSummary("dynamis", 2, "btc", { openOrder: true, awaitingCard: true }),
  N5: () => awaiting(CRYPTO),
  N5a: () => awaiting({ ...CRYPTO, cryptoAmountPaid: "0.00150", cryptoAmountDue: "0.00164" }),
  N5b: () => s.windowClosed({ order: ORDER, invoice: { status: "expired" }, canceled: true, onNewInvoice: noop }),
  N6: () => s.awaitingConfirmation({ order: ORDER, invoice: { status: "open", cryptoAmountPaid: "0.00312", requiredConfirmations: 1 }, method: "btc", gaveUp: false, onCheckAgain: noop }),
  N7a: () => s.windowClosed({ order: ORDER, invoice: { status: "expired" }, onNewInvoice: noop }),
  N7b: () => s.windowClosed({ order: ORDER, invoice: { status: "expired", amount: 20000, currency: "usd", amountPaid: 9600, cryptoAmountPaid: "0.00150", cryptoCurrency: "btc" }, onNewInvoice: noop }),
  N8: () => cardForm("dynamis", 2),
  N8a: () => s.cardUnavailable({ order: ORDER, reason: "script", onRetry: noop, onNewInvoice: noop }),
  N8b: () => s.awaitingConfirmation({ order: ORDER, invoice: undefined, method: "card", gaveUp: false, onCheckAgain: noop }),
  N8c: () => s.awaitingConfirmation({ order: ORDER, invoice: undefined, method: "card", gaveUp: true, onCheckAgain: noop }),
  N4: () => nameCodeIssued("dynamis", 2, true),
  N4a: () => nameCodeIssued("dynamis", 2, false),
  N4b: () => paidNoCode("dynamis", 2),
  N9: () => s.unknownOrder(noop),
  N10: () => history(),
  A1: () => appRegister(),
  A1a: () => appTaken(),
  OP1: () => terminal(),
  OP2: () => groupChat(),
};

const WEB = new Set(Object.keys(SCREENS).filter((id) => id.startsWith("N")));

export function render(id, theme) {
  const build = SCREENS[id];
  if (build === undefined) throw new Error(`mockups: no screen ${id}`);
  const html = document.documentElement;
  if (theme === "dark") { html.setAttribute("data-theme", "dark"); html.classList.add("dark"); html.style.colorScheme = "dark"; }
  if (WEB.has(id)) {
    const chrome = s.chrome({ theme: theme ?? "system", onNewPurchase: noop, onHistory: noop, onTheme: noop, onToggle: noop, onHome: noop });
    document.getElementById("chrome").replaceChildren(chrome.node);
    document.getElementById("app").replaceChildren(build());
  } else {
    document.body.className = "plain";
    document.body.replaceChildren(el("div", { id: "shot" }, build()));
  }
}
