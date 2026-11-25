// The names store's screens in simplex.domains' look: prototypes for the names app in that repo.
// The names app takes its API client, payments, storage, code parsing and QR encoder from @simplex-chat/store; its crypto (wallet.ts, names.ts), QR drawing and views are its own.
import { methodMark } from "./assets/icons.js";
import { qrSvg } from "./assets/qr.js";
import {
  APP_OWNER_SHORT, APPLIED, APPLIED_SHORT, APPLIED_YEARS, BAKERY, CODE_UNTIL, CODE_UNTIL_SHORT, COMMIT_TX, NAME, NAME_INVEST, ORDER_ID, OWNER_SHORT,
  OWNER_CODE, REGISTER_TX, UNTIL, UNTIL_SHORT, dollars, prepaidCode, priceOf,
} from "./fixtures.js";

function el(tag, attrs = {}, ...kids) {
  const node = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) node.setAttribute(k, v);
  for (const kid of kids.flat()) if (kid !== null && kid !== undefined && kid !== false) node.append(kid);
  return node;
}

// Line icons, drawn as stroked paths on a 24-unit grid.
const ICONS = {
  search: "M11 19a8 8 0 1 0 0-16 8 8 0 0 0 0 16Z M21 21l-4.3-4.3",
  arrow: "M5 12h14 M12 5l7 7-7 7",
  link: "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71 M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71",
  check: "M20 6 9 17l-5-5",
  x: "M18 6 6 18 M6 6l12 12",
  shield: "M12 22s8-3.5 8-10V5l-8-3-8 3v7c0 6.5 8 10 8 10Z M9 12l2 2 4-4",
  copy: "M10 8h10a2 2 0 0 1 2 2v10a2 2 0 0 1-2 2H10a2 2 0 0 1-2-2V10a2 2 0 0 1 2-2Z M4 16a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2",
  phone: "M7 2h10a2 2 0 0 1 2 2v16a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2Z M12 18h.01",
  key: "M15.5 7.5a4.5 4.5 0 1 1-3.9 6.7L4 21.8 2.2 20l1.8-1.8L2.2 16.4 4 14.6l1.8 1.8 4.4-4.4a4.5 4.5 0 0 1 5.3-4.5Z",
  moon: "M12 3a6 6 0 0 0 9 9 9 9 0 1 1-9-9Z",
  sun: "M12 16a4 4 0 1 0 0-8 4 4 0 0 0 0 8Z M12 2v2 M12 20v2 M4.9 4.9l1.4 1.4 M17.7 17.7l1.4 1.4 M2 12h2 M20 12h2 M6.3 17.7l-1.4 1.4 M19.1 4.9l-1.4 1.4",
  alert: "M12 9v4 M12 17h.01 M10.3 3.9 1.8 18a2 2 0 0 0 1.7 3h17a2 2 0 0 0 1.7-3L13.7 3.9a2 2 0 0 0-3.4 0Z",
  clock: "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20Z M12 6v6l4 2",
  info: "M12 22a10 10 0 1 0 0-20 10 10 0 0 0 0 20Z M12 16v-4 M12 8h.01",
  refresh: "M21 12a9 9 0 1 1-3-6.7L21 8 M21 3v5h-5",
  menu: "M4 6h16 M4 12h16 M4 18h16",
  wallet: "M20 12V8H6a2 2 0 0 1 0-4h12v4 M4 6v12a2 2 0 0 0 2 2h14v-4 M18 12a2 2 0 0 0 0 4h4v-4h-4Z",
};
function icon(name) {
  const s = document.createElementNS("http://www.w3.org/2000/svg", "svg");
  s.setAttribute("viewBox", "0 0 24 24");
  s.setAttribute("class", "i");
  s.setAttribute("aria-hidden", "true");
  const p = document.createElementNS("http://www.w3.org/2000/svg", "path");
  p.setAttribute("d", ICONS[name]);
  s.append(p);
  return s;
}

const METHODS = { card: ["Card", "Processed by Stripe"], btc: ["Bitcoin", "On-chain"], xmr: ["Monero", "Private by default"] };
const CRYPTO = { address: "bc1q8n4xqs0v2kr7w5mxj9d3tlh6c0yzpe2a7kqfuv", amount: "0.00312", partial: "0.00150" };
const CRYPTO_REMAINING = (Number(CRYPTO.amount) - Number(CRYPTO.partial)).toFixed(5);
// what the order screens' name costs
const PAID = dollars(priceOf(NAME), true);
const SHORT_REF = ORDER_ID.slice(0, 8) + "…";
const INVEST_LINE = `Invest ${NAME_INVEST}+ in SimpleX Chat and get a free SimpleX name.`;
const paidTotal = () => el("div", { class: "total" }, el("span", { class: "label" }, "Total"), el("b", {}, PAID));

const btn = (text, cls = "", ...extra) => el("button", { class: `btn ${cls}`.trim(), type: "button" }, text, ...extra);
const disabled = (node) => { node.setAttribute("disabled", ""); return node; };
const link = (text) => el("a", { href: "#" }, text);
// the term as the page words it: a code's or a search's years, and the 30 days every registration adds
const termText = (years) => `${years} years + 30 days`;
const chip = (text, cls = "", dot = false) => el("span", { class: `chip ${cls}`.trim() }, dot ? el("span", { class: "dot" }) : null, text);
const alertBox = (tone, ic, title, text) => el("div", { class: `alert ${tone}` }, icon(ic), el("div", {}, title ? el("b", {}, title) : null, text));
const nameTitle = (label, cls = "name") => el("div", { class: cls }, label, el("span", {}, ".simplex"));

function brand() {
  return el("a", { class: "brand", href: "#" },
    el("img", { class: "logo-light", src: "domains/img/logo-light.png", alt: "SimpleX" }),
    el("img", { class: "logo-dark", src: "domains/img/logo-dark.png", alt: "SimpleX" }));
}

function header(active, dark) {
  const nav = (text, key) => el("a", { href: "#", class: key === active ? "on" : "" }, text);
  return el("header", { class: "top" }, el("div", { class: "wrap" },
    brand(),
    el("nav", {}, nav("Search", "search"), nav("Your names", "names"), nav("SimpleX badges", "badges")),
    el("span", { class: "grow" }),
    el("a", { class: "ghost-btn", href: "#" }, icon("key"), "Check a code"),
    el("a", { class: "icon-btn", href: "#", "aria-label": "Switch light or dark theme" }, icon(dark ? "sun" : "moon")),
    el("a", { class: "icon-btn menu-btn", href: "#", "aria-label": "Menu" }, icon("menu"))));
}

function footer() {
  return el("footer", { class: "foot" }, el("div", { class: "wrap" },
    brand(),
    el("div", { class: "links" }, link("Wefunder"), link("Trademark owners"), link("Contact")),
    el("span", { class: "grow" }),
    el("div", { class: "pays" }, "We accept", ...["card", "btc", "xmr"].map((m) => methodMark(m)))));
}

function searchBox(label, { bad = false, off = false } = {}) {
  const go = btn("Search", "lg", icon("arrow"));
  return el("div", { class: "search" + (bad ? " bad" : "") },
    icon("search"),
    el("input", { type: "text", value: label, placeholder: "yourname", "aria-label": "Name" }),
    el("span", { class: "tld" }, ".simplex"),
    off ? disabled(go) : go);
}

function hero(label, { banner, box = {}, after = [] } = {}) {
  return el("section", { class: "hero" }, el("div", { class: "wrap" },
    el("span", { class: "eyebrow" }, el("span", { class: "tag" }, "NEW"), "Names for SimpleX channels and addresses"),
    el("h1", { class: "display" }, "Own your name", el("br"), "on ", el("span", { class: "grad" }, "SimpleX")),
    el("p", { class: "lead" }, "One name for your public channel and contact address, registered to a wallet only you hold."),
    banner ?? null,
    searchBox(label, box),
    ...after));
}

function bento() {
  const step = (n, title, text, extra) => el("article", { class: "tile" }, el("span", { class: "n" }, n), el("h3", {}, title), el("p", {}, text), extra);
  return el("section", { class: "section" }, el("div", { class: "wrap" },
    el("div", { class: "section-head" }, el("h2", { class: "display" }, "How it works")),
    el("div", { class: "bento" },
      step("1", "Search", "Any name of 6 letters or more: a–z, 0–9 and single hyphens between them. The search sends only its hash and length."),
      step("2", "Pay", "One price for 2 years, shown with the name, and 30 more days free.",
        el("div", { class: "pays" }, ...["card", "btc", "xmr"].map((m) => el("span", {}, methodMark(m), METHODS[m][0])))),
      step("3", "It's yours", "Registered to a wallet made in your browser. Its name code is the only key, and moves the name to your app."),
      el("article", { class: "tile wide" },
        el("h3", {}, "One name, wherever people find you"),
        el("p", {}, "Point it at your contact address and your channel. People type the name in SimpleX and connect."),
        el("div", { class: "preview" },
          el("span", { class: "nm grad" }, "bakery.simplex"), el("span", { class: "arrow" }, icon("arrow")),
          el("span", { class: "avatar" }, "AB"),
          el("div", { class: "who" }, el("b", {}, "Alice's Bakery"), el("span", {}, "Contact address · Channel")))),
      el("article", { class: "tile invest" },
        el("h3", {}, "Investors get a name free"),
        el("p", {}, INVEST_LINE),
        el("div", { class: "links" }, link("Wefunder ↗"), link("Check your code"))))));
}

function entity(e, openable = true) {
  return el("div", { class: "entity" },
    el("span", { class: "avatar", style: `background: hsl(${e.hue} 70% 50%)` }, e.initials),
    el("div", { class: "who" }, el("b", {}, e.name), el("span", {}, e.kind)),
    el("span", { class: "url" }, e.link),
    openable ? link("Open in SimpleX") : null);
}

function availableResult(label, { coded = false, credit = false } = {}) {
  const price = priceOf(label);
  const what = el("div", { class: "what" },
    nameTitle(label),
    el("div", { class: "meta" }, chip("Available", "ok", true), chip(`${label.length} letters`),
      chip(coded ? termText(APPLIED_YEARS) : termText(2)), coded ? chip("Code applied", "brand") : null, credit ? chip("Covered by your payment", "brand") : null),
    coded ? el("div", { class: "covered" }, "Covered by your code", el("span", { class: "mono nowrap" }, APPLIED_SHORT), "·", link("Remove")) : null);
  const priceBox = coded
    ? el("div", { class: "price" }, el("b", {}, el("s", {}, dollars(price)), "$0"), el("span", {}, `for ${APPLIED_YEARS} years`))
    : el("div", { class: "price" }, el("b", {}, dollars(price)), el("span", {}, "for 2 years"));
  const action = coded ? "Register with code" : credit ? "Register with your payment" : "Register";
  return el("div", { class: "result" }, what, priceBox, btn(action, "", icon("arrow")));
}

function takenResult(label, chipNode, below) {
  return el("div", { class: "result stack" },
    el("div", { class: "result-top" }, el("div", { class: "what" }, nameTitle(label), el("div", { class: "meta" }, chipNode))),
    el("div", { class: "below" }, ...below));
}

function searchScreen(state) {
  const hint = el("p", { class: "hint" }, "Press ", el("kbd", {}, "Enter"), " or Search");
  const under = (...kids) => el("div", { class: "under-search" }, ...kids);
  switch (state) {
    case "empty": return [hero(""), bento()];
    case "typed": return [hero(NAME, { after: [hint] }), bento()];
    case "short": return [hero("alice", { box: { bad: true }, after: [el("p", { class: "rule" }, icon("alert"), "5 letters. A name has 6 letters or more: a–z, 0–9 and single hyphens between them.")] }), bento()];
    case "chars": return [hero("my_name", { box: { bad: true }, after: [el("p", { class: "rule" }, icon("alert"), "Use only a–z, 0–9 and single hyphens between them.")] }), bento()];
    case "available": return [hero(NAME, { after: [availableResult(NAME)] }), bento()];
    case "coded": return [hero(NAME, { after: [availableResult(NAME, { coded: true })] }), bento()];
    case "taken": return [hero("bakery", { after: [takenResult("bakery", chip("Taken", "bad", true),
      [el("span", { class: "label" }, "Used by"), el("div", { class: "entities" }, ...BAKERY.map((e) => entity(e)))])] }), bento()];
    case "unlinked": return [hero("harbour", { after: [takenResult("harbour", chip("Taken", "bad", true),
      [el("p", { class: "note" }, "Registered, and not pointing at any address or channel that we know of yet.")])] }), bento()];
    case "reserved": return [hero("support", { after: [takenResult("support", chip("Reserved", "warn", true),
      [el("p", { class: "note" }, "Reserved names are not sold here. If it is yours to use, contact the SimpleX team."),
        el("div", { style: "margin-top: 12px" }, btn("Contact the SimpleX team", "quiet"))])] }), bento()];
    case "unchecked": return [hero(NAME, { after: [under(el("div", {}, alertBox("warn", "alert", "This name could not be checked", "The name service did not answer."), el("div", { style: "margin-top: 14px" }, btn("Try again", "quiet", icon("refresh")))))] }), bento()];
    case "limited": return [hero(NAME, { box: { off: true }, after: [under(alertBox("warn", "clock", "Try again in 42 seconds", "Too many names were searched from here. Searching is paused until then."))] }), bento()];
    case "credit": return [hero("dynamos", {
      banner: el("div", { class: "banner" }, icon("wallet"), el("span", {}, el("b", {}, `Your payment is kept: ${PAID}. `), `Choose another name of up to ${PAID}; nothing more is charged.`)),
      after: [availableResult("dynamos", { credit: true })] })];
    case "fromApp": return [hero(NAME, {
      banner: el("div", { class: "banner" }, icon("phone"), el("span", {}, el("b", {}, "Opened from SimpleX. "), `The name is registered to your app's wallet, ${APP_OWNER_SHORT}.`)),
      after: [availableResult(NAME)] })];
    default: throw new Error(`domains: no search state ${state}`);
  }
}

function codeModal(state) {
  const codes = { valid: APPLIED, used: prepaidCode(6, 202), invalid: "SN7-4K2P7-TQ9M1-ZX3RB-8HJ5X", adding: prepaidCode(6, 505) };
  const verdicts = {
    valid: el("div", { class: "verdict ok" }, icon("check"), el("div", {}, el("b", {}, "Unused"), el("span", {}, `Covers names of 6 or more letters, for ${APPLIED_YEARS} years.`))),
    adding: el("div", { class: "verdict ok" }, icon("check"), el("div", {}, el("b", {}, "Unused"), el("span", {}, "Covers names of 6 or more letters, for 2 years."))),
    used: el("div", { class: "verdict bad" }, icon("x"), el("div", {}, el("b", {}, "This code was already used"), el("span", {}, "Each code registers one name."))),
    invalid: el("div", { class: "verdict bad" }, icon("x"), el("div", {}, el("b", {}, "This code is not valid"), el("span", {}, "Check it for a mistyped character."))),
  };
  const ok = state === "valid" || state === "adding";
  const action = btn(state === "adding" ? "Add to your codes" : "Use this code", "block");
  return el("div", { class: "modal-overlay" }, el("div", { class: "modal", role: "dialog" },
    el("div", { class: "modal-head" }, el("h3", {}, state === "adding" ? "Add a code" : "Check your code"), el("a", { class: "icon-btn", href: "#", "aria-label": "Close" }, icon("x"))),
    el("p", { class: "sec-note" }, "A code from SimpleX, for example for investors. Nothing is spent until a name is registered with it."),
    el("div", { class: `input code-input ${ok ? "ok" : "bad"}` }, icon("key"), el("input", { type: "text", value: codes[state], "aria-label": "Code", spellcheck: "false" })),
    verdicts[state],
    ok ? action : disabled(action)));
}

// opened from the app there is no owner code to save; a prepaid code leaves nothing to pay
function crumbs(at, { fromApp = false, prepaid = false } = {}) {
  const names = ["Search", "Details", fromApp ? null : "Name code", prepaid ? null : "Payment", "Registration"].filter(Boolean);
  const out = [];
  names.forEach((n, i) => {
    if (i) out.push(el("span", { class: "crumb-sep" }));
    out.push(el("span", { class: "crumb" + (i < at ? " done" : i === at ? " on" : "") }, el("i", {}, i < at ? "✓" : String(i + 1)), n));
  });
  return el("div", { class: "crumbs" }, ...out);
}

function pageHead(at, title, lead, steps = {}) {
  return el("section", { class: "page-head" }, el("div", { class: "wrap" },
    at === null ? null : crumbs(at, steps), el("h1", { class: "display" }, title), lead ? el("p", { class: "lead" }, lead) : null));
}

function orderCard(label, rows, ...rest) {
  return el("aside", { class: "panel summary" },
    el("span", { class: "label" }, "Your order"),
    nameTitle(label, "nm"),
    el("div", { class: "rows" }, ...rows.map(([k, v]) => el("div", { class: "r" }, el("span", {}, k), el("span", {}, v)))),
    ...rest);
}

function twoCols(main, side) {
  return el("div", { class: "wrap" }, el("div", { class: "cols" }, main, side));
}

function method(m, { on = false, off = false } = {}) {
  return el("div", { class: "method" + (on ? " on" : "") + (off ? " off" : "") },
    el("span", { class: "radio" }), methodMark(m),
    el("div", {}, el("b", {}, METHODS[m][0]), el("br"), el("span", {}, off ? "Temporarily unavailable" : METHODS[m][1])));
}

function miniInvest() {
  return el("div", { class: "mini-invest" }, INVEST_LINE, el("br"), link("Learn more on Wefunder ↗"));
}

// opts: selected, unavailable, openOrder, noStorage, links ("open"), code ("covering" | "entry" | "short"), fromApp
function checkoutScreen(opts = {}) {
  const selected = opts.selected ?? "btc";
  const covered = opts.code === "covering" || opts.code === "entry";
  const linksSec = opts.fromApp ? null : el("div", { class: "panel-sec" },
    el("div", { class: "sec-title" }, el("h3", {}, "Point it at your SimpleX"), el("span", { class: "opt" }, "Optional")),
    el("p", { class: "sec-note" }, "Set in the same registration. You can change them later."),
    opts.links === "open"
      ? [el("div", { class: "input ok" }, icon("link"), el("input", { type: "text", value: BAKERY[0].link }), el("span", { class: "kind" }, "Address")),
        el("div", { style: "margin: 10px 0" }, entity(BAKERY[0], false))]
      : el("div", { class: "input" }, icon("link"), el("input", { type: "text", placeholder: "Paste your address" }), el("span", { class: "kind" }, "Address")),
    el("div", { class: "input" }, icon("link"), el("input", { type: "text", placeholder: "Paste your channel link" }), el("span", { class: "kind" }, "Channel")));
  let paySec;
  if (covered) {
    paySec = el("div", { class: "panel-sec" }, el("div", { class: "sec-title" }, el("h3", {}, "Payment")),
      el("div", { class: "covered-box" }, icon("check"), el("div", {}, "Paid with your code", el("p", {}, `Registered for ${termText(APPLIED_YEARS)}, with nothing to pay.`))));
  } else if (opts.openOrder) {
    paySec = el("div", { class: "panel-sec" }, el("div", { class: "sec-title" }, el("h3", {}, "Payment method")),
      alertBox("info", "info", "You have an order waiting to be confirmed", "A second order would be a second charge, so this one cannot start yet. When that payment is confirmed or expires, you can start a new one here."),
      el("div", { style: "margin-top: 14px" }, btn("Open the waiting order", "quiet")));
  } else {
    paySec = el("div", { class: "panel-sec" }, el("div", { class: "sec-title" }, el("h3", {}, "Payment method")),
      opts.unavailable ? el("div", { style: "margin-bottom: 14px" }, alertBox("warn", "alert", `${METHODS[opts.unavailable][0]} is temporarily unavailable`, "Try another method, or come back later.")) : null,
      el("p", { class: "sec-note" }, opts.fromApp ? "You pay on the next page." : "You pay on the next pages, after saving your name code."),
      el("div", { class: "methods" }, ...["card", "btc", "xmr"].map((m) => method(m, { on: m === selected, off: m === opts.unavailable }))));
  }
  const storage = opts.noStorage ? el("div", { class: "panel-sec" }, alertBox("warn", "alert", "This browser will not keep your name code", "The next step shows it: your saved copy will be the only one.")) : null;
  const main = el("div", { class: "panel" }, storage, linksSec, paySec);

  const term = covered ? termText(APPLIED_YEARS) : termText(2);
  const until = covered ? CODE_UNTIL_SHORT : UNTIL_SHORT;
  const rows = [["Term", term], ["Registered until", `about ${until}`]];
  if (opts.code !== "entry" && !opts.openOrder) rows.push(covered ? ["Code", APPLIED_SHORT] : ["Paying with", METHODS[selected][0]]);
  const total = el("div", { class: "total" }, el("span", { class: "label" }, "Total"),
    el("b", {}, covered ? el("s", {}, PAID) : null, covered ? "$0.00" : PAID));
  const go = btn("Continue", "lg block", icon("arrow"));
  let codeEntry = null;
  if (opts.code === "entry" || opts.code === "short") {
    const ok = opts.code === "entry";
    codeEntry = el("div", { class: "code-entry" },
      el("span", { class: "label" }, "Your code"),
      el("div", { class: `input code-input ${ok ? "ok" : "bad"}`, style: "margin-top: 8px" }, icon("key"), el("input", { type: "text", value: ok ? APPLIED : prepaidCode(8, 404) })),
      el("div", { class: `line ${ok ? "ok" : "bad"}` }, icon(ok ? "check" : "x"), ok ? `Covers names of 6 or more letters, for ${APPLIED_YEARS} years.` : `This code covers names of 8 or more letters; ${NAME} has ${NAME.length}.`));
  }
  const showCodeLink = !opts.fromApp && !codeEntry && !covered && !opts.openOrder;
  const side = orderCard(NAME, rows,
    codeEntry,
    total,
    opts.openOrder ? disabled(go) : go,
    showCodeLink ? el("div", { class: "sub-links" }, link("I have a code")) : null,
    el("p", { class: "fine" }, icon("shield"), opts.fromApp ? el("span", {}, "Registered to your app's wallet, ", el("span", { class: "nowrap" }, APP_OWNER_SHORT), ".") : "Registered to a wallet made in this browser; nobody else holds its key."),
    opts.fromApp ? null : miniInvest());
  return [pageHead(1, "Check your order", null, { fromApp: opts.fromApp, prepaid: covered }), twoCols(main, side)];
}

function codeBlock() {
  return [el("div", { class: "code-display" }, OWNER_CODE),
    el("div", { class: "code-actions" }, btn("Copy code", "quiet", icon("copy")))];
}

function codeScreen() {
  return [pageHead(2, "Save your name code", `It will own ${NAME}.simplex. If you lose it, nothing else can recover the name.`),
    twoCols(
      el("div", { class: "panel" }, el("div", { class: "panel-sec" },
        el("div", { class: "sec-title" }, el("h3", {}, "Your name code")),
        el("p", { class: "sec-note" }, "Copy it somewhere safe or write it down. You redeem it in the app later."),
        ...codeBlock(),
        el("label", { class: "check" }, el("span", { class: "box" }), "I have saved my code"))),
      orderCard(NAME, [["Term", termText(2)], ["Registered until", `about ${UNTIL_SHORT}`], ["Paying with", "Bitcoin"]],
        paidTotal(),
        disabled(btn("Continue to payment", "lg block", icon("arrow"))),
        el("div", { class: "why" },
          el("div", {}, icon("key"), "The code owns the name once registered."),
          el("div", {}, icon("shield"), "The service never sees it; nobody else has a copy."),
          el("div", {}, icon("refresh"), "It also finishes the order on another browser, if this one loses it."))))];
}

function priceChanged() {
  return status("warn", "alert", "The price changed", `${NAME}.simplex costs ${dollars(25000, true)} now; it changed while you were deciding. Nothing was charged.`, [btn("Search again")]);
}

function rateLimited() {
  return status("warn", "clock", "Too many attempts", "Too many orders were tried from here, so ordering is paused. Nothing was charged.", [disabled(btn("Try again in 41 seconds")), btn("Back", "quiet")]);
}

function invoiceFailure() {
  return status("bad", "alert", "That did not go through", "Nothing was charged. If this happens again, get in touch.", [btn("Try again"), btn("Back", "quiet")]);
}

function copyField(label, value, copy = true) {
  return el("div", { class: "copy-field" }, el("span", { class: "label" }, label),
    el("div", { class: "val" }, el("span", {}, value), copy ? link("Copy") : null));
}

function cryptoScreen({ partial = false } = {}) {
  const due = partial ? CRYPTO_REMAINING : CRYPTO.amount;
  const main = el("div", { class: "panel" }, el("div", { class: "panel-sec" },
    partial ? el("div", { style: "margin-bottom: 18px" }, alertBox("warn", "alert", "Part of the payment arrived", `${CRYPTO.partial} of ${CRYPTO.amount} BTC is seen. Send the remaining ${CRYPTO_REMAINING} BTC to the same address.`)) : null,
    el("div", { class: "pay-head" },
      el("div", {}, el("span", { class: "label" }, partial ? "Send the rest" : "Send"), el("div", { class: "amount" }, `${due} BTC`), el("div", { class: "usd" }, `${PAID} in total`)),
      chip(partial ? "Rate held 45:59" : "Rate held 59:59", "brand")),
    el("div", { class: "pay-body" },
      el("div", { class: "qr" }, qrSvg(`bitcoin:${CRYPTO.address}?amount=${due}`, "Bitcoin payment")),
      el("div", {}, copyField("Amount", `${due} BTC`), copyField("Bitcoin address", CRYPTO.address), copyField("Reference", ORDER_ID, false))),
    el("div", { class: "waiting" }, el("span", { class: "live" }, partial ? "Waiting for the rest of the payment" : "Waiting for payment"), partial ? null : link("Cancel this invoice"))));
  return [pageHead(3, "Pay with Bitcoin", "Bookmark this page: the address and the countdown live on its link."),
    twoCols(main, orderCard(NAME, [["Term", termText(2)], ["Paying with", "Bitcoin"]], paidTotal()))];
}

function confirmingScreen(card) {
  const main = el("div", { class: "panel" }, el("div", { class: "panel-sec" },
    el("div", { class: "pay-head" }, el("div", {},
      el("span", { class: "label" }, card ? "Card accepted" : "Payment received"),
      el("div", { class: "amount", style: "font-size: 34px" }, card ? "Waiting for Stripe to confirm" : "Waiting for 1 confirmation"))),
    el("p", { class: "sec-note", style: "margin-top: 12px" }, card
      ? "Stripe usually confirms within a minute. Registration starts by itself; this page moves on."
      : `${CRYPTO.amount} BTC arrived. Registration starts once it confirms; this page moves on by itself.`),
    el("div", { class: "meter" }, el("i")),
    el("div", { class: "waiting" }, el("span", { class: "live" }, "Confirming payment"), el("span", { class: "mono", style: "font-size: 13px; color: var(--ink-3)" }, ORDER_ID))));
  return [pageHead(3, card ? "Paying by card" : "Confirming your payment"), twoCols(main, orderCard(NAME, [["Term", termText(2)], ["Paying with", card ? "Card" : "Bitcoin"]], paidTotal()))];
}

function cardScreen() {
  const f = (label, value) => el("label", { class: "f" }, el("span", {}, label), el("div", { class: "box" }, value));
  const main = el("div", { class: "panel" }, el("div", { class: "panel-sec" },
    el("div", { class: "sec-title" }, el("h3", {}, "Card details")),
    el("div", { class: "stripe" },
      el("div", { class: "label" }, el("span", {}, "Stripe's payment form"), el("span", {}, "Secured by Stripe")),
      f("Card number", "1234 1234 1234 1234"),
      el("div", { class: "two" }, f("Expiry", "MM / YY"), f("CVC", "CVC")),
      f("Country", "Portugal")),
    el("div", { style: "margin-top: 18px" }, btn(`Pay ${PAID}`, "lg block"))));
  return [pageHead(3, "Pay by card"), twoCols(main, orderCard(NAME, [["Term", termText(2)], ["Paying with", "Card"]], paidTotal()))];
}

// a single outcome, centred, as the badge store's endings are
function status(tone, ic, title, text, actions = [], { reference = false, extra = null } = {}) {
  return [el("div", { class: "status-wrap" }, el("div", { class: "panel status" },
    el("div", { class: `orb ${tone}` }, icon(ic)),
    el("h1", { class: "display" }, title),
    el("p", {}, text),
    extra,
    actions.length ? el("div", { class: "actions" }, ...actions) : null,
    reference ? el("div", { class: "ref" }, el("span", {}, "Reference"), el("span", { class: "mono" }, ORDER_ID)) : null))];
}

function track(states) {
  const stage = (st, n, title, text) => el("div", { class: `stage ${st}` }, el("span", { class: "node" }, st === "done" ? "✓" : st === "failed" ? "✕" : n), el("b", {}, title), el("span", {}, text));
  return el("div", { class: "track" },
    stage(states[0], "1", states[0] === "done" ? "Committed" : "Committing", "The name, hidden, goes on chain"),
    stage(states[1], "2", "Waiting", "Every registration waits after its commit"),
    stage(states[2], "3", states[2] === "done" ? "Registered" : "Registering", "Then it is yours"));
}

function txRows(states, owner = OWNER_SHORT) {
  const tx = (k, v, c) => el("div", { class: "tx" }, el("span", { class: "k" }, k), v, c);
  const commit = [el("a", { class: "v", href: "#" }, `${COMMIT_TX} ↗`), states[0] === "done" ? chip("Confirmed", "ok") : chip("Sent", "brand")];
  const register = states[2] === "done" ? [el("a", { class: "v", href: "#" }, `${REGISTER_TX} ↗`), chip("Confirmed", "ok")]
    : states[2] === "active" ? [el("a", { class: "v", href: "#" }, `${REGISTER_TX} ↗`), chip("Sent", "brand")]
    : [el("span", { class: "v" }, "—"), chip("After the wait")];
  return el("div", { class: "panel-sec txs" }, tx("Commit", ...commit), tx("Register", ...register), tx("Owner", el("span", { class: "v" }, owner), null));
}

function registeringScreen(states) {
  const active = states.indexOf("active");
  const box = [
    el("div", { class: "countdown" }, el("b", {}, icon("clock")), el("p", {}, "Waiting for the commit to be mined. The wait starts once it is.")),
    el("div", { class: "countdown" }, el("b", {}, "0:42"), el("p", {}, "The contract makes every registration wait after its commit, so nobody can take a name they saw being registered.")),
    el("div", { class: "countdown" }, el("b", {}, icon("clock")), el("p", {}, "The register transaction is being mined; the name is yours once it is, and final 3 blocks later.")),
  ][active];
  return [pageHead(4, ["Registering ", el("span", { class: "grad" }, `${NAME}.simplex`)], "About two minutes. Keep this page open: after the wait it signs the name to finish it."),
    twoCols(el("div", { class: "panel" }, el("div", { class: "panel-sec" }, track(states), box), txRows(states)),
      orderCard(NAME, [["Paid", `${PAID} · Bitcoin`], ["Term", termText(2)], ["Reference", SHORT_REF]]))];
}

function celebrate(label, lead) {
  return el("section", { class: "celebrate" }, el("div", { class: "wrap" },
    el("div", { class: "tick" }, icon("check")),
    el("h1", { class: "display" }, el("span", { class: "grad" }, `${label}.simplex`), el("br"), "is yours"),
    el("p", { class: "lead" }, lead)));
}

function registeredScreen({ saved = true, years = 2, until = UNTIL } = {}) {
  const next = (ic, title, text, action) => el("div", { class: "item" }, el("span", { class: "ic" }, icon(ic)), el("div", {}, el("b", {}, title), el("p", {}, text), action));
  return [celebrate(NAME, `Registered for ${termText(years)}, until ${until}, to ${OWNER_SHORT}, a wallet made in this browser.`),
    twoCols(
      el("div", { class: "panel" }, el("div", { class: "panel-sec" },
        el("div", { class: "sec-title" }, el("h3", {}, "Your name code")),
        el("p", { class: "sec-note" }, "The name code it was ordered with. It owns the name."),
        ...codeBlock(),
        saved
          ? alertBox("warn", "shield", null, "Whoever has this code controls the name. Nobody else has a copy, not even us.")
          : alertBox("bad", "alert", "This code could not be saved in this browser", "The copy you saved before paying is the only one."))),
      el("aside", { class: "panel summary" },
        el("span", { class: "label" }, "Next steps"),
        el("div", { class: "next", style: "margin-top: 12px" },
          next("phone", "Redeem it in the app", "Scan or enter the code in SimpleX: the name moves to the app's wallet.",
            el("div", { class: "qr", style: "margin-top: 10px" }, qrSvg(OWNER_CODE, "name code"))),
          next("link", "Point it at your address", "Link your contact address or channel.", link("Edit links")),
          next("key", "Keep the code safe", "Until you redeem it, it is the only key to the name.", null))))];
}

function registeredToApp() {
  return [celebrate(NAME, `Registered until ${UNTIL} to your app's wallet, ${APP_OWNER_SHORT}.`),
    el("div", { class: "wrap", style: "max-width: 760px" }, el("div", { class: "panel" },
      el("div", { class: "panel-sec" }, track(["done", "done", "done"])), txRows(["done", "done", "done"], APP_OWNER_SHORT),
      el("div", { class: "panel-sec" }, btn("Back to SimpleX", "lg block", icon("arrow")))))];
}

function notOnThisDevice() {
  return status("info", "key", "The name code is not in this browser",
    ["This order registers a name to ", el("span", { class: "nowrap" }, OWNER_SHORT), ". Its code was made in the browser it was bought in. Enter the code you saved before paying to carry on here."],
    [btn("Enter your name code", "", icon("arrow"))], { reference: true });
}

function restoreScreen() {
  return [pageHead(null, "Restore with a name code", "The code stays in this browser. The service sees only a signature from the name's key."),
    twoCols(
      el("div", { class: "panel" }, el("div", { class: "panel-sec" },
        el("span", { class: "label" }, "Name code"),
        el("div", { class: "input code-input", style: "margin-top: 8px" }, icon("key"), el("input", { type: "text", value: OWNER_CODE })),
        el("div", { style: "margin-top: 20px" }, btn("Restore", "lg block", icon("arrow"))))),
      el("aside", { class: "panel summary" },
        el("span", { class: "label" }, "How it works"),
        el("div", { class: "why" },
          el("div", {}, icon("wallet"), "The page rebuilds the name's key and the order's secret from the code."),
          el("div", {}, icon("shield"), "It signs a request with that key and finds the code's orders; knowing the address alone finds nothing."),
          el("div", {}, icon("refresh"), "If the registration waits for this browser, it carries on from here."))))];
}

function takenMeanwhile() {
  return [pageHead(4, "Someone registered it first", `${NAME}.simplex was registered just before ours. Your payment is kept.`),
    twoCols(el("div", { class: "panel" },
      el("div", { class: "panel-sec" }, track(["done", "done", "failed"]),
        el("p", { class: "sec-note", style: "margin: 18px 0 0" }, "Another registration of this name went through first, so ours could not."),
        el("div", { class: "options" },
          el("div", { class: "option" }, el("b", {}, "Choose another name"), el("p", {}, `Any name of up to ${PAID}, registered to the same wallet, so your code owns it.`), btn("Choose another name", "block")),
          el("div", { class: "option" }, el("b", {}, "Take a code instead"), el("p", {}, `A code for names of ${NAME.length} or more letters, for 2 years, to use here or in the app.`), btn("Take a code", "quiet block"))))),
      orderCard(NAME, [["Paid", `${PAID} · Bitcoin`], ["Kept", PAID], ["Reference", SHORT_REF]]))];
}

function takenCode() {
  const code = prepaidCode(7, 909);
  return status("ok", "key", "Here is a code instead", `For names of ${NAME.length} or more letters, for 2 years: what ${NAME}.simplex was paid for.`,
    [btn("Copy code", "", icon("copy")), btn("Search another name", "quiet")],
    { extra: [el("div", { class: "code-display" }, code),
      el("div", { style: "display: flex; justify-content: center; margin-top: 16px" }, el("div", { class: "qr" }, qrSvg(code, "code"))),
      el("div", { style: "text-align: left" }, alertBox("info", "info", "Keep a copy of this code", "The service keeps only its hash; this browser keeps the code, under Your names."))] });
}

function failedAfterPaying() {
  return [pageHead(4, "Paid, not registered", "Your payment is kept. Trying again charges nothing."),
    twoCols(el("div", { class: "panel" },
      el("div", { class: "panel-sec" }, track(["failed", "", ""]),
        el("div", { style: "margin-top: 22px" }, alertBox("bad", "alert", "The blockchain did not take the commit", "The service's own retries gave up.")),
        el("div", { style: "margin-top: 18px" }, btn("Try again", "lg", icon("refresh"))))),
      orderCard(NAME, [["Paid", `${PAID} · Bitcoin`], ["Charged again", "Never"], ["Reference", SHORT_REF]]))];
}

function historyScreen() {
  const row = (mark, title, sub, statusChip, ...acts) => el("div", { class: "row-item" },
    el("span", { class: "mark" }, mark), el("div", { class: "main" }, el("b", {}, title), el("div", { class: "sub" }, sub)), statusChip, el("div", { class: "acts" }, ...acts));
  const small = (text, ic) => el("a", { class: "small-btn", href: "#" }, ic ? icon(ic) : null, text);
  return [pageHead(null, "Your names", "This list is kept only in this browser."),
    el("div", { class: "wrap" },
      el("div", { class: "toolbar" }, el("a", { class: "btn quiet", href: "#" }, icon("key"), "Add a code"), el("a", { class: "btn quiet", href: "#" }, icon("refresh"), "Restore with a name code")),
      el("div", { class: "panel" },
        el("div", { class: "list-title" }, el("h3", {}, "Names")),
        row("D", `${NAME}.simplex`, `Until ${UNTIL_SHORT} · points at Alice's Bakery · ${PAID} Bitcoin`, chip("Registered", "ok", true), small("Show code", "key"), small("Edit links", "link")),
        row("N", "nebula.simplex", `Committed, waiting for the minute to pass · ${dollars(priceOf("nebula"), true)} Monero`, chip("Registering", "brand", true), small("Open")),
        row("O", "orbital.simplex", "Nothing was charged · 28 Sep 2026", chip("Invoice expired", "bad"), small("Open"))),
      el("div", { class: "panel", style: "margin-top: 16px" },
        el("div", { class: "list-title" }, el("h3", {}, "Codes")),
        row("≡", prepaidCode(6, 303), "Names of 6 or more letters, for 5 years · added 9 Oct 2026", chip("Unused", "ok", true), small("Use"), small("Copy", "copy"))),
      el("a", { class: "danger-link", href: "#" }, "Forget everything in this browser"))];
}

function editLinksScreen() {
  return [pageHead(null, ["Edit links for ", el("span", { class: "grad" }, `${NAME}.simplex`)]),
    twoCols(
      el("div", { class: "panel" }, el("div", { class: "panel-sec" },
        el("span", { class: "label" }, "Contact address"),
        el("div", { class: "input ok", style: "margin-top: 8px" }, icon("link"), el("input", { type: "text", value: BAKERY[0].link })),
        el("div", { style: "margin-top: 10px" }, entity(BAKERY[0], false)),
        el("span", { class: "label", style: "display: block; margin-top: 20px" }, "Channel"),
        el("div", { class: "input ok", style: "margin-top: 8px" }, icon("link"), el("input", { type: "text", value: BAKERY[1].link })),
        el("div", { style: "margin-top: 10px" }, entity(BAKERY[1], false)))),
      el("aside", { class: "panel summary" },
        el("span", { class: "label" }, "Saving"),
        el("div", { class: "why" },
          el("div", {}, icon("key"), "This browser signs the change with the name's key, from your name code."),
          el("div", {}, icon("shield"), "SimpleX sends it and pays the fee.")),
        el("div", { style: "margin-top: 20px" }, btn("Save links", "lg block"))))];
}

// A screen is a list of page sections; a popup is appended over the page.
const SCREENS = {
  N1: () => searchScreen("empty"),
  N1a: () => searchScreen("typed"),
  N1b: () => searchScreen("short"),
  N1c: () => searchScreen("chars"),
  N2: () => searchScreen("available"),
  N2a: () => searchScreen("taken"),
  N2b: () => searchScreen("unlinked"),
  N2c: () => searchScreen("reserved"),
  N2d: () => searchScreen("unchecked"),
  N2e: () => searchScreen("limited"),
  K1: () => [...searchScreen("available"), codeModal("valid")],
  K2: () => searchScreen("coded"),
  K3: () => checkoutScreen({ code: "covering" }),
  K4: () => checkoutScreen({ code: "entry" }),
  K5: () => [...searchScreen("available"), codeModal("used")],
  K6: () => [...searchScreen("available"), codeModal("invalid")],
  K7: () => checkoutScreen({ code: "short" }),
  K8: () => registeredScreen({ years: APPLIED_YEARS, until: CODE_UNTIL }),
  N3: () => checkoutScreen(),
  N3a: () => checkoutScreen({ selected: "xmr", unavailable: "btc" }),
  N3b: () => priceChanged(),
  N3c: () => rateLimited(),
  N3d: () => invoiceFailure(),
  N3e: () => checkoutScreen({ openOrder: true }),
  N3f: () => checkoutScreen({ noStorage: true }),
  N3g: () => checkoutScreen({ links: "open" }),
  N3h: () => codeScreen(),
  N4: () => cryptoScreen(),
  N4a: () => cryptoScreen({ partial: true }),
  N4b: () => status("info", "x", "Invoice cancelled", "This invoice was cancelled and nothing was received.", [btn("Order the name again")], { reference: true }),
  N4c: () => status("info", "clock", "Invoice expired", "Nothing was received before its rate window closed.", [btn("Get a new invoice")], { reference: true }),
  N4d: () => status("warn", "alert", "Invoice expired", `${CRYPTO.partial} BTC arrived, which is not the full amount. Write to chat@simplex.chat with the reference below and we will sort it out.`, [btn("Get a new invoice", "quiet")], { reference: true }),
  N5: () => confirmingScreen(false),
  N6: () => registeringScreen(["active", "", ""]),
  N6a: () => registeringScreen(["done", "active", ""]),
  N6b: () => registeringScreen(["done", "done", "active"]),
  N7: () => registeredScreen(),
  N7a: () => registeredScreen({ saved: false }),
  N7b: () => notOnThisDevice(),
  N7c: () => restoreScreen(),
  N8: () => takenMeanwhile(),
  N8a: () => searchScreen("credit"),
  N8b: () => takenCode(),
  N9: () => failedAfterPaying(),
  N10: () => cardScreen(),
  N10a: () => status("warn", "alert", "The card form did not load", "Stripe's form was blocked or failed to load. Nothing was charged.", [btn("Load the card form again"), btn("Get a new invoice", "quiet")], { reference: true }),
  N10b: () => confirmingScreen(true),
  N10c: () => status("warn", "clock", "This is taking longer than expected", "The payment has not been confirmed. This page keeps working: come back to it later, or quote the reference below.", [btn("Check again")], { reference: true }),
  N11: () => status("info", "link", "This link does not work", "It names no order this service knows. Check the link, or start again.", [btn("Search a name")]),
  N12: () => historyScreen(),
  N12a: () => [...historyScreen(), codeModal("adding")],
  N12b: () => editLinksScreen(),
  L1: () => searchScreen("fromApp"),
  L2: () => checkoutScreen({ fromApp: true }),
  L3: () => registeredToApp(),
};

export function render(id, theme, tab) {
  const build = SCREENS[id];
  if (build === undefined) throw new Error(`domains: no screen ${id}`);
  const dark = theme === "dark";
  if (dark) document.documentElement.classList.add("dark");
  const parts = build();
  // a popup covers the whole document, so it sits in the body, not in the page
  const overlays = parts.filter((p) => p.classList?.contains("modal-overlay"));
  const page = parts.filter((p) => !overlays.includes(p));
  document.body.replaceChildren(header(tab, dark), el("main", { class: "page" }, ...page), footer(), ...overlays);
}
