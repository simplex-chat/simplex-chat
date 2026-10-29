// The board's frames that are not the names store: the badges landing, built on the compiled badges
// webapp, and the app and the operator, drawn in their own idioms. The names screens are in domains.js.
import * as s from "./assets/screens.js";
import { NAME, OWNER_CODE, prepaidCode, UNTIL_SHORT } from "./fixtures.js";

const { el } = s;
const noop = () => {};

function badgesLanding() {
  const p = s.landing({ onStart: noop });
  p.append(el("p", { class: "names-link" }, el("a", { class: "link", href: "#" }, "SimpleX names are at simplex.domains")));
  return p;
}

function app(...kids) {
  return el("div", { class: "app-screen" }, ...kids);
}

function appRedeem() {
  return app(el("div", { class: "app-nav" }, "‹ Back"), el("div", { class: "app-title" }, "Redeem a name"),
    el("div", { class: "app-label" }, "NAME CODE"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("span", {}, `${OWNER_CODE.slice(0, 8)}…${OWNER_CODE.slice(-5)}, scanned`), el("span", { class: "app-ok" }, "✓"))),
    el("div", { class: "app-label" }, "FOUND"),
    el("div", { class: "app-card" }, el("div", { class: "app-row" }, el("b", {}, `${NAME}.simplex`), el("span", {}, `until ${UNTIL_SHORT}`))),
    el("p", { class: "app-note" }, "The name moves to this app's wallet: the code's wallet offers it and the app's wallet takes it."),
    el("div", { class: "app-button" }, "Redeem"));
}

function appRedeemed() {
  return app(el("div", { class: "app-nav" }, "‹ Back"), el("div", { class: "app-title" }, "Redeem a name"),
    el("div", { class: "app-alert" },
      el("b", {}, "The name is in your app"),
      el("p", {}, `${NAME}.simplex is yours until ${UNTIL_SHORT}, in this app's wallet. The code no longer controls it.`),
      el("div", { class: "app-alert-button" }, "OK")));
}

function terminal() {
  const code = prepaidCode(7, 11);
  return el("pre", { class: "term-screen" },
    "> //issue name 7\n",
    el("span", { class: "out" }, `Code: ${code}\n`),
    "> //issue name 6 years 5\n",
    el("span", { class: "out" }, `Code: ${prepaidCode(6, 17)}\n`),
    "> //issue name\n",
    el("span", { class: "out" }, "bad chat command: Usage: //issue supporter|legend|investor [months 1-255] [paid|unpaid|free], //issue name <length 6-63> [years <Y>] [paid|unpaid|free], or //revoke <code>\n"),
    "> //revoke ", code, "\n",
    el("span", { class: "out" }, "Revoked.\n"));
}

function groupChat() {
  const msg = (who, cls, ...lines) => el("div", { class: `bubble ${cls}` }, el("b", {}, who), ...lines.map((l) => el("div", {}, l)));
  return el("div", { class: "chat-screen" },
    el("div", { class: "chat-title" }, "#SimpleX Store"),
    msg("alice · moderator", "in", "/issue name 6"),
    msg("SimpleX Store", "out", `Code: ${prepaidCode(6, 23)}`),
    msg("alice · moderator", "in", "/bulk name 6 years 5 count 3"),
    msg("SimpleX Store", "out", prepaidCode(6, 31), prepaidCode(6, 37), prepaidCode(6, 41)),
    msg("bob · member", "in", "/issue name 6"),
    el("div", { class: "chat-note" }, "a member below moderator gets no reply, as for badge codes"));
}

const SCREENS = {
  B0: () => badgesLanding(),
  A1: () => appRedeem(),
  A2: () => appRedeemed(),
  OP1: () => terminal(),
  OP2: () => groupChat(),
};

export function render(id) {
  const build = SCREENS[id];
  if (build === undefined) throw new Error(`mockups: no screen ${id}`);
  if (id === "B0") {
    const chrome = s.chrome({ theme: "system", onNewPurchase: noop, onHistory: noop, onTheme: noop, onToggle: noop, onHome: noop });
    document.getElementById("chrome").replaceChildren(chrome.node);
    document.getElementById("app").replaceChildren(build());
  } else {
    document.body.className = "plain";
    document.body.replaceChildren(el("div", { id: "shot" }, build()));
  }
}
