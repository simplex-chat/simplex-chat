// The visual contract: the numbers and the artwork the mockups in
// `plans/badges-codes/screens/*.svg` fix, pinned so they cannot rot quietly.

import { timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { readdirSync, readFileSync } from "node:fs";
import { decl, mediaFor, type Rule, ruleFor, sheet } from "./css.js";

const designTest = timedTest(2000);

import { StubElement, installDocument } from "./stub-dom.js";

installDocument();
const icons = await import("../src/icons.js");
const screens = await import("../src/screens.js");

function render(node: unknown): StubElement { return node as unknown as StubElement; }

// --------------------------------------------------------------- the palette

/**
 * The palette, and where every value comes from.
 *
 */
const REFERENCE_LIGHT: ReadonlyArray<readonly [string, string]> = [
  ["--accent", "#3889FF"],
  ["--on-accent", "#ffffff"],
  ["--ink", "#1E2122"],
  ["--bg", "#F7F7F7"],
  ["--surface", "#FFFFFF"],
  ["--menu", "#FFFFFF"],
  ["--line", "#E8E8E8"],
];

/** Dark redefines these and not the accent pair: one blue with white on it in both themes, the single
 * decision that makes the two themes one product and removes the contrast problem a second accent creates. */
const REFERENCE_DARK: ReadonlyArray<readonly [string, string]> = [
  ["--ink", "#FFFFFF"],
  ["--bg", "#141416"],
  ["--surface", "#1E2122"],
  ["--menu", "#242427"],
  ["--line", "#424347"],
];

const SEMANTIC: ReadonlyArray<readonly [string, string]> = [
  ["--ok-fg", "#1c7c3f"],
  ["--ok-bg", "#e8f5ec"],
  ["--ok-line", "#bfe3cc"],
  ["--warn-fg", "#8a6100"],
  ["--warn-bg", "#fff8e6"],
  ["--warn-line", "#f0dcae"],
  ["--danger-fg", "#b3261e"],
  ["--danger-bg", "#fdecec"],
  ["--danger-line", "#f3c9c6"],
];

/** Everything the light theme must define on bare `:root`. */
const PALETTE: ReadonlyArray<readonly [string, string]> = [...REFERENCE_LIGHT, ...SEMANTIC];

const darkQuery = (): Rule => {
  const media = mediaFor("(prefers-color-scheme: dark)");
  assert.ok(media, "the dark query must exist");
  // `:not([data-theme="light"])` and not a bare `:root`: an explicit light
  // choice has to beat the operating system, or the menu's Light does nothing
  // on a machine set to dark.
  const rule = ruleFor(media.rules, ':root:not([data-theme="light"])');
  assert.ok(rule, `the dark query must claim everything but an explicit light, and got ${JSON.stringify(media.rules.map((r) => r.selector))}`);
  return rule;
};

/** The same palette again, for the menu's explicit Dark. */
const darkAttribute = (): Rule => {
  const rule = ruleFor(sheet.rules, ':root[data-theme="dark"]');
  assert.ok(rule, "an explicit dark choice must beat an operating system set to light");
  return rule;
};

designTest("design: every light token holds the value it is measured from", () => {
  const root = ruleFor(sheet.rules, ":root");
  assert.ok(root, "the palette must be defined on bare :root, not only inside a media query");
  for (const [token, value] of PALETTE) {
    const found = root.decls.get(token);
    assert.ok(found !== undefined, `${token} is not defined on :root`);
    assert.equal(found.toLowerCase(), value.toLowerCase(), `${token} must be ${value}`);
  }
});

designTest("design: the dark theme is the reference's own, and the accent does not move", () => {
  const light = ruleFor(sheet.rules, ":root")!;
  const dark = darkQuery();
  for (const [token, value] of REFERENCE_DARK) {
    assert.equal(dark.decls.get(token)?.toLowerCase(), value.toLowerCase(), `${token} must be ${value} under dark`);
  }
  // One blue with white on it in both themes: no second accent, and no
  // contrast problem to solve on either ground.
  for (const token of ["--accent", "--on-accent"]) {
    assert.equal(dark.decls.get(token), undefined,
      `${token} must not be redefined under dark — it is one value for the whole page`);
  }
  // The coherence the reference keeps: its dark surface IS its light ink.
  assert.equal(dark.decls.get("--surface")?.toLowerCase(), light.decls.get("--ink")?.toLowerCase());
  // The light ground carries one faint wash; the dark ground is flat.
  assert.match(light.decls.get("--page") ?? "", /^radial-gradient\(50% 50% at 50% 50%,/);
  assert.equal(dark.decls.get("--page"), "none", "a wash on #141416 is a blue smear");
  assert.equal(decl("body", "background-image"), "var(--page)");
});

designTest("design: the two dark blocks are the same palette, token for token", () => {
  // CSS cannot express three theme states in one rule, so the dark palette is
  // written twice: once for the operating system and once for the menu's
  // explicit Dark. Divergence between them is a theme that changes when you
  // choose the one you were already looking at.
  const fromQuery = darkQuery();
  const fromAttribute = darkAttribute();
  assert.deepEqual([...fromAttribute.decls.entries()].sort(), [...fromQuery.decls.entries()].sort());
  assert.ok(fromQuery.decls.size >= REFERENCE_DARK.length + SEMANTIC.length);
});

designTest("design: dark restates every token, and none of them by the light value", () => {
  // A theme is not a filter: each role is redefined, and a token left at its
  // light value is a white card or black text on a near-black page. The one
  // deliberate exception is the accent pair, which is one colour on purpose.
  const light = ruleFor(sheet.rules, ":root")!;
  const dark = darkQuery();
  for (const [token] of PALETTE) {
    if (token === "--accent" || token === "--on-accent") continue;
    const value = dark.decls.get(token);
    assert.ok(value !== undefined, `${token} is not redefined under dark`);
    assert.notEqual(value.toLowerCase(), light.decls.get(token)!.toLowerCase(),
      `${token} is the same in both themes, which is one of them being wrong`);
  }
  for (const token of ["--hero", "--wordmark"]) {
    assert.notEqual(dark.decls.get(token), light.decls.get(token),
      `${token} is a second image, not the first one dimmed`);
  }
});

// -------------------------------------------------------------- the geometry

designTest("design: the content column is the mockups' 560px, and the gutter is outside it", () => {
  // The mockups are an 880-wide page with the column at x=160..720. `#app` IS
  // that column, so the side gutter lives on `body`. Put it on the panel and
  // every card inside is 40px narrower than what was drawn.
  assert.equal(decl("#app", "max-width"), "560px");
  assert.equal(decl("#app", "margin"), "0 auto", "and it is centred");
  assert.equal(decl("footer", "max-width"), "560px", "the footer rule spans the same column");
  assert.match(decl("body", "padding") ?? "", /^0 \d+px$/, "the gutter is body's, so the column keeps its width");
  const panelPadding = decl(".panel", "padding") ?? "";
  assert.ok(/^\d+px 0( \d+px)?$/.test(panelPadding),
    `a panel adds no horizontal padding, and got "${panelPadding}"`);
});

designTest("design: the primary button is the reference's pill, in both themes", () => {
  assert.equal(decl(".primary", "width"), "100%");
  assert.equal(decl(".primary", "min-height"), "48px", "ref draws it 48 tall");
  assert.equal(decl(".primary", "font-weight"), "700", "and labelled at 700");
  // simplex.chat's own header button and the reference chrome are both fully
  // rounded, and one radius scale across the page is what stops it reading as
  // parts from different designs.
  assert.equal(decl(".primary", "border-radius"), "var(--r-pill)");
  assert.equal(decl(".secondary", "border-radius"), "var(--r-pill)");
  assert.equal(ruleFor(sheet.rules, ":root")!.decls.get("--r-pill"), "9999px");
  // ref: the menu button is a circle, not a rounded rectangle.
  assert.equal(decl(".menu-button", "border-radius"), "50%");
  assert.equal(decl(".primary", "background"), "var(--accent)");
  assert.equal(decl(".primary", "color"), "var(--on-accent)",
    "the label is a token, because the dark theme's accent is a light blue");
  assert.equal(decl(".primary", "font-size"), "1rem");
});

designTest("design: a card is a 1px hairline at one shared radius, and the chosen one is accent", () => {
  // One radius token per role, and every card on the page takes the same one:
  // a page whose boxes are drawn at 8, 10 and 12 reads as assembled rather than
  // designed, and a reviewer calls that ad-hoc.
  assert.equal(ruleFor(sheet.rules, ":root")!.decls.get("--r-card"), "20px", "ref's radius for a card or a field");
  assert.equal(decl(".choice", "border"), "1px solid var(--line)");
  assert.equal(decl(".choice", "border-radius"), "var(--r-card)");
  assert.equal(decl(".rows", "border"), "1px solid var(--line)");
  assert.equal(decl(".rows", "border-radius"), "var(--r-card)");
  assert.equal(decl(".entry", "border-radius"), "var(--r-card)");
  // ref draws NO shadow on a card: the border is the whole of the edge, and the
  // only shadow on the page is the one the menu needs to float over a screen.
  for (const selector of [".choice", ".rows", ".entry"]) {
    assert.equal(decl(selector, "box-shadow"), undefined, `${selector} must not carry a shadow`);
  }
  assert.equal(decl(".menu", "box-shadow"), "var(--shadow-pop)");
  assert.equal(decl('.choice[aria-pressed="true"]', "border-color"), "var(--accent)");
  assert.equal(decl(".info", "background"), "var(--surface)", "the landing screen's info panel is a card like any other");
  assert.equal(decl(".notice", "background"), "var(--warn-bg)", "waiting and repriced are the warn ground");
  assert.equal(decl(".warn", "background"), "var(--danger-bg)", "a failure or a loss is the danger ground");
});

designTest("design: a landed copy is not overwritten by the hover it is under", () => {
  // The pointer is still on the button that was just pressed, so every hover
  // and active rule on a control that can be copied from has to step aside;
  // otherwise the confirmation is drawn in the hover colour and never seen.
  const copyable = [".primary", ".primary.outline", ".secondary", ".secondary.inline"];
  for (const rule of sheet.rules) {
    if (!/:hover|:active/.test(rule.selector)) continue;
    const base = rule.selector.replace(/:(hover|active|not\([^)]*\))/g, "").trim();
    if (!copyable.includes(base)) continue;
    assert.match(rule.selector, /:not\(\.copied\)/,
      `${rule.selector} would repaint a confirmation the buyer is looking at`);
  }
  assert.equal(decl(".primary.copied", "color"), "var(--ok-fg)");
});

designTest("design: the shell paints the right ground before the stylesheet lands", () => {
  const shell = readFileSync(new URL("../../public/index.html", import.meta.url), "utf8");
  const root = ruleFor(sheet.rules, ":root")!;
  const dark = darkQuery();
  for (const [scheme, token] of [["light", root], ["dark", dark]] as const) {
    const ground = token.decls.get("--bg")!;
    assert.ok(shell.includes(`<meta name="theme-color" content="${ground}" media="(prefers-color-scheme: ${scheme})">`),
      `theme-color for ${scheme} must be ${ground}, or the browser frames the page in the wrong colour`);
  }
});

designTest("design: the footer is a hairline rule then a centred accent link", () => {
  assert.equal(decl("footer a", "border-top"), "1px solid var(--line)");
  assert.equal(decl("footer a", "color"), "var(--accent)");
  assert.equal(decl("footer", "text-align"), "center");
  assert.equal(decl("footer a", "font-size"), ".875rem");
});

designTest("design: the hero is a CSS background, relative to the stylesheet's own directory", () => {
  // Relative, so it resolves inside /assets/<buildHash>/ beside styles.css. That
  // puts it under the same hash and lets the worker precache it.
  const root = ruleFor(sheet.rules, ":root")!;
  const hero = root.decls.get("--hero");
  assert.ok(hero !== undefined, "--hero must be a token, so the theme can swap it");
  assert.match(hero, /^url\([^/][^)]*\.png\)$/, `the URL must be relative, and got "${hero}"`);
  assert.equal(decl(".hero", "background-image"), "var(--hero)");
  assert.equal(decl(".hero", "width"), "268px", "the mockup draws it 268 wide");
  assert.equal(decl(".hero", "aspect-ratio"), "460 / 578", "at the source image's own proportions");
});

// ------------------------------------------------------------ the type scale

// Every size the sheet may use. A tenth step is a step nobody chose, so it is refused here.
const SCALE: readonly string[] = [
  "2.25rem", "1.875rem", "1.625rem", "1.375rem", "1.25rem", "1.125rem", "1rem", ".875rem", ".75rem",
];

/** The weight ladder: plain, medium, every title, every label and button, and the one heading. */
const WEIGHTS: readonly string[] = ["400", "500", "600", "700", "800"];

/** Every rule of the sheet, base and phone band alike, with which band it came from. */
function everyRule(): Array<{ selector: string; decls: Map<string, string>; phone: boolean }> {
  const out = sheet.rules.map((r) => ({ ...r, phone: false }));
  for (const block of sheet.media) {
    if (block.query.replace(/\s+/g, "") !== "(max-width:560px)") continue;
    for (const r of block.rules) out.push({ ...r, phone: true });
  }
  return out;
}

/** The phone band's own rules, which shadow the base sheet below 560px. */
function phoneRules(): Array<{ selector: string; decls: Map<string, string> }> {
  return sheet.media
    .filter((m) => m.query.replace(/\s+/g, "") === "(max-width:560px)")
    .flatMap((m) => m.rules);
}

/** As the browser resolves it: the LAST declaration for that selector in the band wins. */
function phoneDecl(selector: string, property: string): string | undefined {
  let value: string | undefined;
  for (const rule of phoneRules()) {
    if (rule.selector !== selector) continue;
    const found = rule.decls.get(property);
    if (found !== undefined) value = found;
  }
  return value;
}

/** Specificity as (ids, classes, elements), which is all this sheet uses. */
function specificity(selector: string): [number, number, number] {
  const bare = selector.replace(/\s*[>+~]\s*/g, " ");
  const ids = (bare.match(/#[\w-]+/g) ?? []).length;
  const classes = (bare.match(/\.[\w-]+|\[[^\]]+\]|:[\w-]+\([^)]*\)|:(?!:)[\w-]+/g) ?? []).length;
  const elements = (bare.match(/(^|\s)[a-z][\w-]*/g) ?? []).length;
  return [ids, classes, elements];
}

const beats = (a: [number, number, number], b: [number, number, number]): boolean =>
  a[0] !== b[0] ? a[0] > b[0] : a[1] !== b[1] ? a[1] > b[1] : a[2] > b[2];

/** The last compound of a selector: what the rule is actually about. */
const subject = (selector: string): string => selector.trim().split(/\s*[>+~]\s*|\s+/).pop()!;

designTest("type: every size the sheet declares is a step of the reference scale", () => {
  // The two that were not: `.choice .price .was` at `.8em`, meaning 17.6px and a different 17.6px in each
  // price it had been dropped into, and `.choice .price` at 1.0625rem, the mockup's 17 transcribed literally
  // onto a scale that has no 17. A size off the scale matches nothing else on the page by construction.
  const offScale: string[] = [];
  for (const rule of everyRule()) {
    const size = rule.decls.get("font-size");
    if (size === undefined) continue;
    // `body` carries the root size the rem steps above are measured from.
    if (rule.selector === "body") {
      assert.equal(size, "16px", "the root size the whole scale is relative to");
      continue;
    }
    if (!SCALE.includes(size)) offScale.push(`${rule.phone ? "@phone " : ""}${rule.selector} { font-size: ${size} }`);
  }
  assert.deepEqual(offScale, [], `these sizes stand on no step of the scale: ${offScale.join(", ")}`);
});

designTest("type: every weight is a rung of one ladder, and 800 belongs to the heading alone", () => {
  const wrong: string[] = [];
  for (const rule of everyRule()) {
    const weight = rule.decls.get("font-weight");
    if (weight === undefined) continue;
    if (!WEIGHTS.includes(weight)) wrong.push(`${rule.selector} { font-weight: ${weight} }`);
    // The reference draws its one primary heading at 800 and nothing else near
    // it; a second element at that weight would compete with the page's title.
    else if (weight === "800") assert.equal(rule.selector, "h1", "only the primary heading is 800");
  }
  assert.deepEqual(wrong, [], `these weights are off the ladder ${WEIGHTS.join("/")}: ${wrong.join(", ")}`);
});

designTest("type: every heading size is declared with the line-height the reference pairs it with", () => {
  // A size without its own line-height inherits body's 1.5, which is a paragraph
  // leading on a 36px word. Each pair below is the reference's, measured.
  const PAIRS: ReadonlyArray<readonly [string, string, string]> = [
    ["h1", "2.25rem", "3rem"],
    ["h1.tight", "1.875rem", "2.5rem"],
  ];
  for (const [selector, size, height] of PAIRS) {
    assert.equal(decl(selector, "font-size"), size, `${selector} takes the ${size} step`);
    assert.equal(decl(selector, "line-height"), height, `${selector} takes the ${size} step's own line-height`);
  }
  // And the same pairs, one step down, below 560px.
  const PHONE: ReadonlyArray<readonly [string, string, string]> = [
    ["h1", "1.625rem", "2.125rem"],
    ["h1.tight", "1.375rem", "1.875rem"],
  ];
  for (const [selector, size, height] of PHONE) {
    assert.equal(phoneDecl(selector, "font-size"), size, `@phone ${selector} takes the ${size} step`);
    assert.equal(phoneDecl(selector, "line-height"), height, `@phone ${selector} pairs ${size} with its own line-height`);
  }
});

designTest("type: a block title and its copy are one rule across all four grounds", () => {
  // `.notice`, `.warn` and `.info` are one box on three grounds, and the rate-limited screen reuses the
  // notice for its countdown. While `.info` declared its own size and weight, the same title was 16/700 in
  // a notice and 14/700 in the info card below 560px: one role, two treatments, on consecutive screens.
  const title = sheet.rules.filter((r) => /^\.(notice|warn|info) \.title$/.test(r.selector) && r.decls.has("font-size"));
  assert.equal(title.length, 3, "the three grounds must share one title rule, and got " + title.map((r) => r.selector).join(", "));
  for (const rule of title) {
    assert.equal(rule.decls.get("font-size"), "1rem", `${rule.selector} is the body step`);
    // 600 and not 700: 700 is what the micro-labels are set at (PAY WITH, the
    // pills, the menu's headings), and a block title is a heading, not a label.
    assert.equal(rule.decls.get("font-weight"), "600", `${rule.selector} is a title weight, not a label weight`);
  }
  const body = sheet.rules.filter((r) => /^\.(notice|warn|info) p$/.test(r.selector) && r.decls.has("font-size"));
  assert.equal(body.length, 3, "and one body rule, and got " + body.map((r) => r.selector).join(", "));
  for (const rule of body) assert.equal(rule.decls.get("font-size"), ".875rem", `${rule.selector} is the small step`);
  // Nothing may restate the type per ground, at either width. That is exactly
  // how the info card drifted a step away from the notice beside it.
  for (const rule of everyRule()) {
    if (!/^\.(notice|warn|info)(\s|$)/.test(rule.selector)) continue;
    if (/\.(notice|warn|info) (\.title|p)$/.test(rule.selector)) continue;
    for (const property of ["font-size", "font-weight"]) {
      assert.equal(rule.decls.get(property), undefined,
        `${rule.selector} restates ${property}; the four grounds must differ only in colour`);
    }
  }
});

designTest("type: block copy carries no leading of its own — the page has one", () => {
  // `.notice p, .warn p` used to declare 1.35, which was neither the body's 1.5
  // nor `.choice .feature`'s 1.4: three leadings for the same 14px, and the
  // 1.35 set the payment screen's stand-in and the code screen's "only copy" visibly tighter than the same
  // size reads anywhere else on the page.
  assert.equal(decl("body", "line-height"), "1.5", "the page's one leading");
  for (const selector of [".notice p", ".warn p", ".info p", ".choice .feature", ".muted", ".lede"]) {
    const declared = decl(selector, "line-height");
    assert.ok(declared === undefined || declared === "1.5",
      `${selector} declares line-height: ${declared}, which is a second leading for the same copy`);
  }
});

designTest("type: `Legend, 12 months` is one title, on the history list's rows and on the paid-no-code screen's summary", () => {
  // Both are `orderTitle` in `screens.ts`: the same string, naming the same
  // purchase. The paid-no-code screen's copy matched no rule at all and inherited the body, so it
  // read as a caption under a heading while the history list's read as a title.
  const rule = sheet.rules.find((r) => r.selector === ".rows.field > .name" && r.decls.has("font-size"));
  assert.ok(rule, "the paid-no-code screen's summary title must be styled, not left to inherit the body");
  const row = sheet.rules.find((r) => r.selector === ".entry .name" && r.decls.has("font-size"));
  assert.ok(row, "the history list's row title must be styled");
  for (const [name, r] of [["paidNoCode", rule], ["the history list", row]] as const) {
    assert.equal(r.decls.get("font-size"), "1.125rem", `${name}'s purchase title takes the same step`);
    assert.equal(r.decls.get("font-weight"), "600", `${name}'s purchase title takes the same weight`);
  }
  // And they step together, rather than one of them going with the phone.
  assert.equal(phoneDecl(".entry .name", "font-size"), "1rem");
  assert.equal(phoneDecl(".rows.field > .name", "font-size"), "1rem");
  // The rendered proof: one function draws both, so a divergence here is a
  // divergence in the stylesheet and nowhere else.
  const order = {
    orderId: "inv_9f3a", badgeType: "legend", months: 12,
    createdAt: "2026-08-28T11:46:00Z", status: "open" as const,
  };
  const paidNoCode = render(screens.paidNoCode({ order, settledAt: undefined }));
  const history = render(screens.purchaseHistory({ keepsNewCodes: true,
    rows: [{ kind: "open" as const, order }], onOpen: () => {}, onStart: () => {},
  }));
  assert.equal(paidNoCode.all("div.name")[0]!.textContent, "Legend, 12 months");
  assert.equal(history.all("div.name")[0]!.textContent, "Legend, 12 months",
    "the same string, so it must not be two type roles");
});

designTest("type: a tier leads with its name and a duration with its price — never both", () => {
  // The mockups put the emphasis in different places: a tier states its price plainly under a heavier name,
  // a duration states its term plainly above a larger price. One `.choice` rule serving both had to be wrong
  // on one of the two screens, and was, weighting a duration's term and its price identically.
  assert.equal(decl(".choice .name", "font-weight"), "600", "a tier's name leads");
  assert.equal(decl(".choice .price", "font-weight"), "400", "and its price is stated plainly");
  assert.equal(decl(".choice.term .name", "font-weight"), "500", "a duration's term steps back");
  assert.equal(decl(".choice.term .price", "font-weight"), "600", "and its price leads");
  assert.equal(decl(".choice.term .price", "font-size"), "1.375rem", "at a larger step than the term above it");
  // Neither card may weight both of its lines the same: that is emphasis that
  // says nothing, which is where this audit started.
  for (const kind of ["", ".term"]) {
    const name = decl(`.choice${kind} .name`, "font-weight") ?? decl(".choice .name", "font-weight");
    const price = decl(`.choice${kind} .price`, "font-weight") ?? decl(".choice .price", "font-weight");
    assert.notEqual(name, price, `.choice${kind} weights its name and its price alike`);
  }
  // The badge-art variant is one step up on both lines and moves NEITHER weight:
  // a tier this build has no artwork for must read as the same card, smaller.
  assert.equal(decl(".choice .badge-art + .name", "font-size"), "1.25rem");
  assert.equal(decl(".choice .badge-art ~ .price", "font-size"), "1.125rem");
  assert.equal(decl(".choice .badge-art ~ .price", "font-weight"), undefined,
    "artwork changes the step, never the weight");
});

designTest("type: the struck total is a step of the scale, and stacks at every width", () => {
  // `.8em` computed to 17.6px beside a 22px price and would have computed to
  // something else beside any other, so the one figure on the page that is not
  // the one being charged was also the one size that belonged to no step.
  assert.equal(decl(".choice .price .was", "font-size"), ".875rem");
  assert.equal(decl(".choice .price .was", "color"), "var(--muted)");
  // Above the price rather than beside it, and unconditionally, because the price layout rule
  // is one layout at every width. Three cards across a 350px column leave 98px
  // inside each, which two figures a step and a half apart cannot share.
  assert.equal(decl(".choice .price .was", "display"), "block");
  assert.equal(phoneDecl(".choice .price .was", "display"), undefined,
    "the stack is the layout, not a phone variant of it");
});

designTest("type: the phone band steps the selectors that can actually win", () => {
  // A media query adds no specificity. `.choice .price { font-size: 1.25rem }`
  // sat in the band and reached nothing: every price on the tier list is
  // `.choice .badge-art ~ .price` and every price on the duration list is
  const sized = sheet.rules.filter((r) => r.decls.has("font-size"));
  for (const banded of phoneRules()) {
    const step = banded.decls.get("font-size");
    if (step === undefined) continue;
    const mine = specificity(banded.selector);
    for (const base of sized) {
      if (subject(base.selector) !== subject(banded.selector)) continue;
      if (!beats(specificity(base.selector), mine)) continue;
      if (base.decls.get("font-size") === step) continue;
      assert.ok(phoneRules().some((r) => r.selector === base.selector && r.decls.has("font-size")),
        `@phone ${banded.selector} steps to ${step} but is beaten by `
        + `${base.selector} { font-size: ${base.decls.get("font-size")} }, which the band never steps — `
        + "the declaration reaches nothing");
    }
  }
});

designTest("type: figures that tick or line up are set in tabular numerals", () => {
  // the payment screen's held-rate clock and the rate-limited screen's countdown are redrawn every second, and
  // proportional digits are not the same width, so the line shifted under the
  // buyer as it counted. The duration list's three prices and the order summary's summary column are figures
  // read against one another, which only line up when the digits do.
  const rule = sheet.rules.filter((r) => r.decls.get("font-variant-numeric") === "tabular-nums");
  const claimed = new Set(rule.map((r) => r.selector));
  for (const selector of [".rate", ".notice .title", ".choice .price", ".row", ".entry .meta"]) {
    assert.ok(claimed.has(selector), `${selector} holds a figure and must set tabular numerals`);
  }
});

designTest("type: the uppercase micro-labels are one rule, tracked in em", () => {
  // PAY WITH, REFERENCE, BITCOIN ADDRESS and REDEEM IT IN THE APP are all
  // `.label`, so there is exactly one place the tracking of an uppercase run is
  // decided, and `.label.standalone`, which is the same label out of a card,
  // restates none of the type it would otherwise be able to drift from.
  assert.equal(decl(".label", "text-transform"), "uppercase");
  assert.equal(decl(".label", "font-size"), ".75rem");
  assert.equal(decl(".label", "font-weight"), "700");
  assert.match(decl(".label", "letter-spacing") ?? "", /em$/, "tracking follows the size, so it survives a step down");
  for (const property of ["font-size", "font-weight", "letter-spacing", "text-transform"]) {
    assert.equal(decl(".label.standalone", property), undefined,
      `.label.standalone restates ${property}; only its place on the screen differs`);
  }
  // The code frame is the page's other tracked run, and it is tracked the same
  // way: at .5px the phone band's 16px code was tracked a third harder than the
  // 22px it steps down from.
  assert.match(decl(".code", "letter-spacing") ?? "", /em$/);
});

designTest("type: the monospace runs are sized against the copy beside them", () => {
  // Three of them, and each takes the step of the text it sits with: the code screen's code
  // is the thing the page was for and leads at 1.375rem, a field's value is the
  // body step under its label, and a command or a saved code inside a box is
  // one step below that, the step of the prose around it.
  assert.equal(decl(".code", "font-size"), "1.375rem");
  assert.equal(decl(".code", "font-family"), "var(--mono)");
  assert.equal(decl(".mono", "font-size"), "1rem");
  assert.equal(decl(".mono", "font-family"), "var(--mono)");
  for (const selector of [".command code", ".entry .code-row code"]) {
    assert.equal(decl(selector, "font-size"), ".875rem", `${selector} is the small step`);
  }
  assert.equal(decl(".warn p", "font-size"), ".875rem",
    "which is the size of the copy the command sits inside");
  // And one declaration per box: `.entry .mono` restated the same .875rem that
  // `.entry .code-row code` already sets on the same element.
  assert.equal(decl(".entry .mono", "font-size"), undefined);
});

// -------------------------------------------------------------- the artwork

/** The brand mark is a file, not path data. Two `d` attributes transcribed into `icons.ts` out of the mockups
 * had gone out of date unnoticed: flat navy over flat blue, where the official mark's X is a cyan gradient.
 * These four assertions are the whole of the guarantee that what ships is what the brand ships. */
const officialAssets: ReadonlyArray<readonly [string, string, (source: string) => string]> = [
  ["wordmark-light.svg", "../../../../../website/src/img/simplex.svg", (x) => x],
  // The one edit, and it is the same one the site's own dark header makes:
  // simplex.chat serves `logo-dark.png`, which is this artwork with its navy
  // lettering set to white. Nothing else about the file moves.
  ["wordmark-dark.svg", "../../../../../website/src/img/simplex.svg",
    (x) => x.replace(/fill="#030749"/g, 'fill="#FFFFFF"')],
  ["symbol-light.svg", "../../../../../media-logos/simplex-symbol-light.svg", (x) => x],
  ["symbol-dark.svg", "../../../../../media-logos/simplex-symbol-dark.svg", (x) => x],
];

designTest("design: the brand artwork is the official file, byte for byte", () => {
  for (const [shipped, source, transform] of officialAssets) {
    const served = readFileSync(new URL(`../../public/img/${shipped}`, import.meta.url), "utf8");
    const official = readFileSync(new URL(source, import.meta.url), "utf8");
    assert.equal(served, transform(official),
      `public/img/${shipped} is not ${source} — the mark must be the file, never a redrawing of it`);
  }
});

designTest("design: no module reaches for a sink that turns a string into markup", () => {
  // `screens.ts` says this rule is checked. This is that check: the sinks below are the ones
  // that parse a string as markup or as code. Attributes are not covered, since `screens.ts` sets
  // `href` from a URI its own builders make, so this pins the rule, not every way past it.
  const sinks = /\.innerHTML|\.outerHTML|insertAdjacentHTML|document\.write|\beval\(|new Function\(|srcdoc/;
  const dir = new URL("../../src/", import.meta.url);
  for (const name of readdirSync(dir).filter((f) => f.endsWith(".ts"))) {
    const source = readFileSync(new URL(name, dir), "utf8");
    assert.equal(sinks.test(source), false, `src/${name} reaches for a markup sink`);
  }
});

designTest("design: no module draws the mark, so no transcription can drift", () => {
  assert.equal("simplexLogo" in icons, false,
    "the mark is served as a file and painted by the stylesheet; a builder here is a second copy of it");
  const source = readFileSync(new URL("../../src/icons.ts", import.meta.url), "utf8");
  // The first path of the mark, where the old transcription began.
  assert.equal(source.includes("M3.02972 8.59396"), false, "the mark's path data is back in the module");
});

designTest("design: the wordmark is the header's, themed and precachable", () => {
  const root = ruleFor(sheet.rules, ":root")!;
  const wordmark = root.decls.get("--wordmark");
  assert.ok(wordmark !== undefined, "--wordmark must be a token, so the theme can swap it");
  // Relative, so it resolves inside /assets/<buildHash>/ beside styles.css.
  assert.match(wordmark, /^url\([^/][^)]*\.svg\)$/, `the URL must be relative, and got "${wordmark}"`);
  assert.equal(decl(".brand", "background-image"), "var(--wordmark)");
  assert.equal(decl(".brand", "aspect-ratio"), "161 / 40", "at the source file's own proportions");
});

function testChrome(over: Partial<Parameters<typeof screens.chrome>[0]> = {}): ReturnType<typeof screens.chrome> {
  return screens.chrome({
    onNewPurchase: () => {}, onHistory: () => {}, onForget: () => {},
    theme: "system", onTheme: () => {}, onToggle: () => {}, ...over,
  });
}

designTest("design: the chrome is a wordmark home link and a menu, and holds no order", () => {
  let started = 0;
  let history = 0;
  let forgot = 0;
  const ui = testChrome({
    onNewPurchase: () => { started += 1; },
    onHistory: () => { history += 1; },
    onForget: () => { forgot += 1; },
  });
  const bar = render(ui.node);
  const brand = bar.all("a.brand")[0]!;
  assert.equal(brand.getAttribute("href"), "/", "the wordmark goes home");
  assert.equal(brand.getAttribute("aria-label"), "SimpleX", "and carries the name, since it has no text");
  const trigger = bar.all("button.menu-button")[0]!;
  assert.equal(trigger.getAttribute("aria-expanded"), "false");
  assert.equal(trigger.getAttribute("aria-controls"), screens.MENU_ID);
  assert.equal(bar.all("svg.bars").length, 1, "the hamburger is drawn, not typed");
  // The three device-wide actions, in the order the menu lists them.
  assert.deepEqual(bar.all("button.menu-item").map((b) => b.textContent),
    [screens.NEW_PURCHASE, screens.PURCHASE_HISTORY, screens.FORGET_EVERYTHING]);
  for (const b of bar.all("button.menu-item")) b.click();
  assert.deepEqual([started, history, forgot], [1, 1, 1]);
  // Three sections, divided as the reference divides them: the setting, the
  // two actions, and the one that destroys something on its own.
  assert.equal(bar.all("div.menu-section").length, 3);
  assert.equal(bar.all("div.menu-section")[2]!.all("button.danger").length, 1,
    "[ Forget everything ] is separated, and marked");
  // the store rules: the menu is fixed labels over callbacks, so nothing about an order can
  // reach it: not a code, not an address, not a reference.
  assert.equal(/SXB-|order=|inv_/.test(bar.serialize()), false, bar.serialize());
});

designTest("design: the theme control is the reference's segmented setting, over three values", () => {
  const chosen: string[] = [];
  const ui = testChrome({ theme: "dark", onTheme: (t) => chosen.push(t) });
  const bar = render(ui.node);
  assert.ok(bar.textContent.includes(screens.THEME_LABEL));
  const segments = bar.all("button.segment");
  assert.deepEqual(segments.map((b) => b.textContent), ["Light", "Dark", "System"]);
  assert.deepEqual(segments.map((b) => b.getAttribute("aria-pressed")), ["false", "true", "false"],
    "the stored theme is the one drawn as chosen");
  segments[0]!.click();
  assert.deepEqual(chosen, ["light"], "the choice is reported, and applying it is main.ts's");
  // The control is redrawn only once the choice has been applied, so it can
  // never show a theme the page is not in.
  assert.deepEqual(segments.map((b) => b.getAttribute("aria-pressed")), ["false", "true", "false"]);
  ui.showTheme("light");
  assert.deepEqual(segments.map((b) => b.getAttribute("aria-pressed")), ["true", "false", "false"]);
});

designTest("design: the menu opens, closes on choosing, and lists what the keyboard can reach", () => {
  const toggles: boolean[] = [];
  const ui = testChrome({ onToggle: (open) => toggles.push(open) });
  const bar = render(ui.node);
  const trigger = bar.all("button.menu-button")[0]!;
  const menu = bar.all("div.menu")[0]!;
  assert.equal(menu.getAttribute("role"), "dialog", "so a screen reader knows the page has a layer over it");
  assert.equal(menu.hasAttribute("hidden"), true, "it starts closed");
  trigger.click();
  assert.equal(ui.isOpen(), true);
  assert.equal(menu.hasAttribute("hidden"), false);
  assert.equal(trigger.getAttribute("aria-expanded"), "true");
  // Three segments and three actions, the six things Tab cycles between.
  assert.equal(ui.focusables().length, 6);
  bar.all("button.menu-item")[1]!.click();
  assert.equal(ui.isOpen(), false, "choosing an item closes it before the screen changes");
  assert.equal(menu.hasAttribute("hidden"), true);
  trigger.click();
  ui.close();
  assert.equal(ui.isOpen(), false);
  // Every open and every close is reported, in order, so whatever the caller
  // does to the screen behind the menu cannot be left half-done.
  assert.deepEqual(toggles, [true, false, true, false]);
});

designTest("design: the withholding reaches the menu — no second invoice while a card is in flight", () => {
  const ui = testChrome();
  const bar = render(ui.node);
  const fresh = bar.all("button.menu-item")[0]!;
  assert.equal(fresh.textContent, screens.NEW_PURCHASE);
  ui.offerNewPurchase(false);
  assert.equal(fresh.hasAttribute("hidden"), true,
    "the create endpoint has no idempotency key, so a menu that offered one would be a second charge");
  assert.equal(ui.focusables().length, 5, "and the keyboard cannot reach what the eye cannot see");
  ui.offerNewPurchase(true);
  assert.equal(fresh.hasAttribute("hidden"), false);
});

designTest("design: each badge is its source file's viewBox, gradient stops and glyph", () => {
  // Copied from `MR/images/badge_<tier>.svg`, which is the file the app draws:
  // a badge that differs here is a badge that looks bought from someone else.
  const expected: Record<string, string[]> = {
    supporter: ["#29f5ff", "#29f5ff", "#527eed", "#3669e9"],
    legend: ["#29f5ff", "#26dee8", "#3064ea", "#001064"],
  };
  for (const [tier, colours] of Object.entries(expected)) {
    assert.equal(icons.hasBadgeArt(tier), true);
    const art = render(icons.badgeIcon(tier as "supporter" | "legend"));
    assert.equal(art.getAttribute("viewBox"), "8.25 8.25 300 399", `${tier}'s box is the source file's`);
    assert.equal(art.getAttribute("aria-hidden"), "true");
    assert.deepEqual(art.all("stop").map((s) => s.getAttribute("stop-color")), colours,
      `${tier}'s gradient must be the file's four stops, in order`);
    assert.deepEqual(art.all("stop").map((s) => s.getAttribute("offset")), ["0%", "5%", "50%", "100%"].map(
      (o, i) => (tier === "supporter" ? ["0%", "5%", "95%", "100%"][i]! : o)));
    const paths = art.all("path");
    assert.equal(paths.length, 2, "a body and a glyph");
    assert.match(paths[0]!.getAttribute("fill") ?? "", /^url\(#/, "the body takes the gradient");
    assert.equal(paths[1]!.getAttribute("fill"), "#ffffff", "and the glyph is white");
  }
  assert.equal(icons.hasBadgeArt("investor"), false, "a tier the catalog does not sell has no art here");
});

designTest("design: two badges on one screen get two gradients, or both would be one colour", () => {
  // An id is a document-wide name: two <linearGradient id="g"> and the first
  // wins, so Legend would be painted in Supporter's cyan.
  const ids = [icons.badgeIcon("supporter"), icons.badgeIcon("legend"), icons.badgeIcon("supporter")]
    .map((n) => render(n).all("linearGradient")[0]!.getAttribute("id"));
  assert.equal(new Set(ids).size, 3, `every gradient needs its own id, and got ${JSON.stringify(ids)}`);
});

designTest("design: the tier list's tier cards carry the badge art, one each, above the name", () => {
  const p = render(screens.tiers({
    tiers: [
      { priceId: "price_supporter", badgeType: "supporter", name: "Supporter", price: "$7 / month", features: ["2 GB files"], disabled: false },
      { priceId: "price_legend", badgeType: "legend", name: "Legend", price: "$70 / month", features: ["5 GB files"], disabled: false },
    ],
    selected: "price_legend", onSelect: () => {}, onContinue: () => {}, onBack: () => {},
  }));
  const cards = p.all("button.choice");
  assert.equal(cards.length, 2);
  for (const [i, card] of cards.entries()) {
    const art = card.all("svg.badge-art");
    assert.equal(art.length, 1, `tier ${i} must draw exactly one badge`);
    assert.equal(art[0]!.getAttribute("viewBox"), "8.25 8.25 300 399");
    assert.equal(card.children.indexOf(art[0]!), 0, "and it is the first thing in the card, above the name");
  }
  // Supporter's cyan-to-blue and Legend's cyan-to-navy, told apart by the stop
  // the two do not share.
  assert.equal(cards[0]!.all("stop")[3]!.getAttribute("stop-color"), "#3669e9");
  assert.equal(cards[1]!.all("stop")[3]!.getAttribute("stop-color"), "#001064");
});

designTest("design: a tier with no artwork still renders, without a badge", () => {
  const p = render(screens.tiers({
    tiers: [{ priceId: "p", badgeType: "founder", name: "Founder", price: "$1 / month", features: [], disabled: false }],
    selected: undefined, onSelect: () => {}, onContinue: () => {}, onBack: () => {},
  }));
  assert.equal(p.all("svg.badge-art").length, 0, "no art is better than the wrong badge");
  assert.ok(p.textContent.includes("Founder"), "and the tier is still on the screen");
});

designTest("design: the order summary's method row carries all three payment marks, in order", () => {
  const p = render(screens.orderSummary({ canKeepTheCode: true,
    badgeType: "legend", months: 12, total: "$420.00", selected: "xmr",
    onSelect: () => {}, onPay: () => {}, onBack: () => {},
  }));
  const cards = p.all("button.choice");
  assert.equal(cards.length, 3);
  const marks = cards.map((c) => c.all("svg.mark"));
  for (const [i, m] of marks.entries()) {
    assert.equal(m.length, 1, `method ${i} must carry exactly one mark`);
    assert.equal(m[0]!.getAttribute("viewBox"), "0 0 24 24", "every mark is drawn in the same 24×24 box");
    assert.equal(m[0]!.getAttribute("aria-hidden"), "true");
  }
  // Bitcoin's orange, Monero's orange, and the card outlined rather than filled:
  // the three are told apart by what the mockup fills them with.
  assert.equal(marks[0]![0]!.all("path")[0]!.getAttribute("fill"), "#F7931A");
  assert.equal(marks[1]![0]!.all("path")[0]!.getAttribute("fill"), "#FF6600");
  assert.equal(marks[2]![0]!.all("path").length, 0, "the card is a rect and a line, not a filled glyph");
  // The card follows the text in whichever theme is on, which is why it is not
  // the mockup's #41506a: that slate is all but invisible on a dark ground, and
  // a theme-conditional override is one more thing to keep in step.
  assert.equal(marks[2]![0]!.all("rect")[0]!.getAttribute("stroke"), "currentColor");
  assert.equal(marks[2]![0]!.all("line")[0]!.getAttribute("stroke"), "currentColor");
  // The mark contributes no text, so the card still reads as its method name.
  assert.ok(cards[1]!.textContent.startsWith("Monero"), cards[1]!.textContent);
});

designTest("design: no screen repeats the mark — the header carries it, and the code screen has its tick", () => {
  // The mockups drew the mark at the top of every screen because the page had
  // no chrome. It has one now, and a wordmark in the header over a symbol on
  // every panel is the same brand said twice on one screen.
  const order = {
    orderId: "inv_9f3a", badgeType: "legend", months: 12,
    createdAt: "2026-08-28T11:46:00Z", status: "open" as const,
  };
  const everyScreen: Array<[string, unknown]> = [
    ["the landing screen", screens.landing({ onStart: () => {} })],
    ["the catalog-changed screen", screens.catalogChanged(() => {})],
    ["awaitingConfirmation", screens.awaitingConfirmation({ order, invoice: undefined, method: undefined, gaveUp: false, onCheckAgain: () => {} })],
    ["windowClosed", screens.windowClosed({ order, invoice: { status: "expired" }, onNewInvoice: () => {} })],
    ["paidNoCode", screens.paidNoCode({ order, settledAt: undefined })],
    ["unknownOrder", screens.unknownOrder(() => {})],
    ["invoiceFailure", screens.invoiceFailure(() => {})],
  ];
  for (const [name, node] of everyScreen) {
    assert.equal(render(node).all("svg.logo").length, 0, `${name} must not draw a second mark`);
    assert.equal(render(node).all("a.brand").length, 0, `${name} must not draw a second wordmark`);
  }
  const codeIssued = render(screens.codeIssued({ code: "SXB-YDC8A-YGQTM-PUYZ9-2TUXP", savedLocally: true }));
  assert.equal(codeIssued.all("div.tick").length, 1, "the code screen opens on the settled tick");
});

// ------------------------------------------------------------- offline

designTest("design: every image the sheet asks for is precached, so the landing screen works offline", () => {
  const worker = readFileSync(new URL("../../public/sw.js", import.meta.url), "utf8");
  const build = /const BUILD = "([0-9a-f]+)";/.exec(worker);
  assert.ok(build, "the worker must name a build");
  const root = ruleFor(sheet.rules, ":root")!;
  const dark = darkQuery();
  // Exactly the files the stylesheet asks for, and under this build's hash: an
  // image the worker never stored is a blank rectangle on the first screen.
  const tokens = ["--hero", "--wordmark"].flatMap((t) => [root.decls.get(t)!, dark.decls.get(t)!]);
  for (const token of tokens) {
    const file = /^url\(([^)]+)\)$/.exec(token)![1]!;
    assert.ok(worker.includes(`\${ASSETS}${file}\``),
      `the stylesheet asks for ${file}, and the worker does not precache it`);
  }
  // The shell's own favicon, which is the official symbol and travels the same road.
  const shell = readFileSync(new URL("../../public/index.html", import.meta.url), "utf8");
  for (const icon of ["symbol-light.svg", "symbol-dark.svg"]) {
    assert.ok(shell.includes(`/assets/${build[1]!}/${icon}`), `the shell must name ${icon} under this build`);
    assert.ok(worker.includes(`\${ASSETS}${icon}\``), `and the worker must precache ${icon}`);
  }
  assert.match(shell, /<link rel="icon"[^>]*media="\(prefers-color-scheme: light\)"/,
    "and pick the variant by theme, as the wordmark does");
});
