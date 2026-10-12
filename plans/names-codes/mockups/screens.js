// The board's frames that are not the names store: the badges landing, built on the compiled badges
// webapp, and the app and the operator, drawn in their own idioms. The names screens are in domains.js.
import * as s from "./assets/screens.js";
import { nameCode, UNTIL } from "./fixtures.js";

const { el } = s;
const noop = () => {};
const UNTIL_SHORT = UNTIL.replace("October", "Oct");

function badgesLanding() {
  const p = s.landing({ onStart: noop });
  p.append(el("p", { class: "check-code" }, el("a", { class: "link", href: "#" }, "SimpleX names are at simplex.domains")));
  return p;
}

function app(...kids) {
  return el("div", { class: "app-screen" }, ...kids);
}

function appImport() {
  return app(el("div", { class: "app-nav" }, "‹ Back"), el("div", { class: "app-title" }, "Import a name"),
    el("div", { class: "app-label" }, "RECOVERY PHRASE"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("span", {}, "12 words, scanned"), el("span", { class: "app-ok" }, "✓"))),
    el("div", { class: "app-label" }, "FOUND"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("b", {}, "dynamis.simplex"), el("span", {}, `until ${UNTIL_SHORT}`))),
    el("p", { class: "app-note" }, "The phrase becomes an account of this device's wallet."),
    el("div", { class: "app-button" }, "Import"));
}

function appImported() {
  return app(el("div", { class: "app-nav" }, "‹ Back"), el("div", { class: "app-title" }, "Import a name"),
    el("div", { class: "app-alert" },
      el("b", {}, "The name is in your app"),
      el("p", {}, `dynamis.simplex is yours until ${UNTIL_SHORT}, in this device's wallet.`),
      el("div", { class: "app-alert-button" }, "OK")));
}

function terminal() {
  const code = nameCode(7, 11);
  return el("pre", { class: "term-screen" },
    "> //issue name 7\n",
    el("span", { class: "out" }, `Code: ${code}\n`),
    "> //issue name 6 years 7\n",
    el("span", { class: "out" }, `Code: ${nameCode(6, 17)}\n`),
    "> //issue name\n",
    el("span", { class: "out" }, "Usage: //issue supporter|legend|investor [months 1-255] [paid|unpaid|free]\n       //issue name <length 6-63> [years <Y>] [paid|unpaid|free]\n       //revoke <code>\n"),
    "> //revoke ", code, "\n",
    el("span", { class: "out" }, "Revoked.\n"));
}

function groupChat() {
  const msg = (who, cls, ...lines) => el("div", { class: `bubble ${cls}` }, el("b", {}, who), ...lines.map((l) => el("div", {}, l)));
  return el("div", { class: "chat-screen" },
    el("div", { class: "chat-title" }, "#SimpleX Store"),
    msg("alice · moderator", "in", "/issue name 6"),
    msg("SimpleX Store", "out", `Code: ${nameCode(6, 23)}`),
    msg("alice · moderator", "in", "/bulk name 6 years 7 count 3"),
    msg("SimpleX Store", "out", nameCode(6, 31), nameCode(6, 37), nameCode(6, 41)),
    msg("bob · member", "in", "/issue name 6"),
    el("div", { class: "chat-note" }, "a member below moderator gets no reply, as for badge codes"));
}

const SCREENS = {
  B0: () => badgesLanding(),
  A1: () => appImport(),
  A2: () => appImported(),
  OP1: () => terminal(),
  OP2: () => groupChat(),
};

export function render(id, theme) {
  const build = SCREENS[id];
  if (build === undefined) throw new Error(`mockups: no screen ${id}`);
  if (theme === "dark") document.documentElement.setAttribute("data-theme", "dark");
  if (id === "B0") {
    const chrome = s.chrome({ theme: theme ?? "system", onNewPurchase: noop, onHistory: noop, onTheme: noop, onToggle: noop, onHome: noop });
    document.getElementById("chrome").replaceChildren(chrome.node);
    document.getElementById("app").replaceChildren(build());
  } else {
    document.body.className = "plain";
    document.body.replaceChildren(el("div", { id: "shot" }, build()));
  }
}
