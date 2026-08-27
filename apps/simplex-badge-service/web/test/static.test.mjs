// The static half of D2's verification.
//
// There is no browser and no display in this environment, so nothing here
// claims that anything *renders*. What it does assert is every property of the
// built output that a browser would otherwise be the only witness to, and each
// assertion was checked to fail when the thing it names is broken (see the
// D2 report). Run with `npm test`, which compiles before it asserts.
//
// Assertions are against dist/ and against the two files served from web/,
// never against src/: the served bytes are what a browser gets.

import test from "node:test"
import assert from "node:assert/strict"
import {readFileSync, readdirSync} from "node:fs"
import {fileURLToPath} from "node:url"

import {parseCatalog} from "../dist/catalog.js"
import {SCREEN_IDS} from "../dist/router.js"
import {firstUnansweredScreen, nothingChosenMessage, optionsOfQuestion, questionOfScreen, screenView} from "../dist/view.js"
import {allRules, customProperties, declaration, effectiveValue, mediaRules, referencedProperties, rules, stripComments} from "./css.mjs"
import {findAll, text} from "./el.mjs"
import {catalogPayload} from "./fixture.mjs"

const read = (rel) => readFileSync(fileURLToPath(new URL(rel, import.meta.url)), "utf8")

const cssSource = read("../styles.css")
const css = stripComments(cssSource)
const DARK = "prefers-color-scheme: dark"
const indexHtml = read("../index.html")
const devHtml = read("../dist/dev.html")
const uiJs = read("../dist/ui.js")

// Derived, not listed: a screen that view.ts starts calling a question is
// covered by the fieldset and label checks below without editing this file.
const QUESTIONS = SCREEN_IDS.filter((id) => questionOfScreen(id) !== null)
const LOGOS = ["logo-symbol-light.svg", "logo-symbol-dark.svg"]

// -- colour tokens ----------------------------------------------------------

test("every custom property the sheet uses is defined on bare :root", () => {
  const rootBlocks = rules(css, ":root")
  assert.equal(rootBlocks.length, 1, "expected exactly one top-level :root rule")
  const defined = customProperties(rootBlocks[0])
  assert.ok(defined.size > 0, "the :root rule declares no custom properties")
  for (const name of referencedProperties(css)) {
    assert.ok(defined.has(name), `${name} is used but has no definition on bare :root, so it is undefined in light mode`)
  }
})

test("the dark block only redefines tokens, and introduces none", () => {
  const light = customProperties(rules(css, ":root")[0])
  const darkRoot = mediaRules(css, DARK, ":root")
  assert.equal(darkRoot.length, 1, "expected exactly one :root rule inside a dark media query")
  const dark = customProperties(darkRoot[0])
  assert.ok(dark.size > 0, "the dark :root rule declares no custom properties")
  for (const name of dark.keys()) {
    assert.ok(light.has(name), `${name} is defined only in dark mode, so it is undefined in light mode`)
  }
})

test("no custom property is declared outside a :root rule", () => {
  // A token declared on a selector would be scoped to it, and its light and
  // dark definitions could then disagree about which elements they reach.
  for (const rule of allRules(css)) {
    if (rule.selector === ":root") continue
    const stray = [...customProperties(rule.body).keys()]
    assert.deepEqual(stray, [], `${rule.selector} declares custom properties: ${stray.join(", ")}`)
  }
})

test("the accent is the website's primary colour in both schemes", () => {
  const light = customProperties(rules(css, ":root")[0])
  const dark = customProperties(mediaRules(css, DARK, ":root")[0])
  assert.equal(light.get("--accent"), "#0053D0", "light accent must be primary-light from website/tailwind.config.js")
  assert.equal(dark.get("--accent"), "#70F0F9", "dark accent must be primary-dark from website/tailwind.config.js")
})

// -- what the stylesheet may not contain ------------------------------------

test("styles.css carries no @@token@@: it is served verbatim and never substituted", () => {
  assert.equal(cssSource.match(/@@[\w.-]+@@/g), null)
})

test("styles.css references no external origin: default-src 'self' would block it", () => {
  assert.equal(css.match(/[a-z][a-z0-9+.-]*:\/\//gi), null, "an absolute URL in the stylesheet")
  assert.equal(css.match(/\burl\(/g), null, "url() would have to resolve to a same-origin asset; none is used")
  assert.equal(css.match(/@import/g), null)
})

test("the font stack is the system one", () => {
  const body = rules(css, "body")
  assert.equal(body.length, 1)
  assert.match(body[0], /font-family:\s*system-ui/)
})

// -- layout, motion, focus --------------------------------------------------

test("the column is capped at 560px", () => {
  const column = rules(css, ".column")
  assert.equal(column.length, 1, "expected exactly one .column rule")
  assert.match(column[0], /max-width:\s*560px/)
})

test("the screen transition is 150ms and prefers-reduced-motion turns it off", () => {
  const screen = rules(css, ".screen")
  assert.equal(screen.length, 1, "expected exactly one .screen rule")
  assert.match(screen[0], /animation:[^;]*\b150ms\b/, ".screen must carry the 150 ms fade")
  const reducedScreen = mediaRules(css, "prefers-reduced-motion: reduce", ".screen")
  assert.equal(reducedScreen.length, 1, "the reduced-motion block must address .screen")
  assert.match(reducedScreen[0], /animation:\s*none/)
})

test("a :focus-visible ring exists and nothing suppresses it", () => {
  const ring = rules(css, ":focus-visible")
  assert.equal(ring.length, 1, "expected a global :focus-visible rule")
  assert.match(ring[0], /outline:\s*\d+px\s+solid/)
  // Not "no blanket outline:none" but "no outline:none at all" — the weaker
  // rule needs a judgement about which selectors count as blanket.
  assert.equal(css.match(/outline\s*:\s*none/g), null, "outline: none anywhere would put this ring in doubt")
})

test("the selected card gets a 2px accent border, as PeriodCard does", () => {
  const body = rules(css, ".option__body")
  assert.equal(body.length, 1)
  assert.match(body[0], /border:\s*2px\s+solid\s+var\(--border\)/)
  const checked = rules(css, ".option__input:checked + .option__body")
  assert.equal(checked.length, 1, "expected a rule for the checked card")
  assert.match(checked[0], /border-color:\s*var\(--accent\)/)
})

test("the focus ring reaches the card, since the radio itself is visually hidden", () => {
  const hidden = rules(css, ".option__input")
  assert.equal(hidden.length, 1)
  assert.doesNotMatch(hidden[0], /display:\s*none/, "display: none would drop the radio out of the tab order")
  const focused = rules(css, ".option__input:focus-visible + .option__body")
  assert.equal(focused.length, 1, "a hidden radio must show its focus on the card")
  assert.match(focused[0], /outline:\s*\d+px\s+solid/)
})

// -- the logo toggle, which is an ordering question, not a rendering one -----

test("the dark-scheme logo rules are the last word on those selectors", () => {
  // This is the check that was missing when the toggle shipped dead. All four
  // selectors here are specificity (0,1,0) and @media adds none, so whichever
  // declaration appears LAST in the file wins. Written first, the dark rules
  // lose to the light-mode defaults and dark mode renders the dark-blue logo
  // on the navy background — no error anywhere.
  for (const [selector, expected] of [
    [".logo--light", "none"],
    [".logo--dark", "block"],
  ]) {
    const displays = allRules(css).filter((r) => r.selector === selector && declaration(r.body, "display") !== undefined)
    assert.ok(displays.length > 0, `no rule sets display on ${selector}`)
    const last = displays[displays.length - 1]
    assert.ok(
      last.at.some((a) => a.includes(DARK)),
      `the last rule setting display on ${selector} is not the dark-scheme one, so the toggle is dead`
    )
    assert.equal(declaration(last.body, "display"), expected)
  }
})

test("exactly one logo is visible in each scheme, resolved through the cascade", () => {
  // Spot-checking one side of this is not enough, and that is not a
  // hypothetical: the first version of this test asserted `.logo` is block and
  // that `.logo--dark` is hidden by default, and an added
  // `.logo--light { display: none }` — which shows *zero* logos in light mode —
  // passed it. So resolve what `display` each <img> actually gets in each
  // scheme, and assert on the outcome rather than on one rule.
  const logos = {
    "logo--light": {tag: "img", classes: ["logo", "logo--light"]},
    "logo--dark": {tag: "img", classes: ["logo", "logo--dark"]},
  }
  for (const scheme of ["light", "dark"]) {
    const visible = []
    for (const [name, element] of Object.entries(logos)) {
      const display = effectiveValue(css, element, "display", {scheme})
      assert.notEqual(display, undefined, `no rule sets display on .${name} in ${scheme} mode`)
      if (display !== "none") visible.push(name)
    }
    assert.deepEqual(visible, [scheme === "dark" ? "logo--dark" : "logo--light"], `wrong logos visible in ${scheme} mode`)
  }
})

test("forced colours carry the selected card by border style, not by colour alone", () => {
  // Forced colours override the accent border, and the radio is clipped, so
  // without this selected and unselected become indistinguishable.
  const base = mediaRules(css, "forced-colors: active", ".option__body")
  const checked = mediaRules(css, "forced-colors: active", ".option__input:checked + .option__body")
  assert.equal(base.length, 1, "expected a forced-colors rule for the card")
  assert.equal(checked.length, 1, "expected a forced-colors rule for the selected card")
  const unselectedStyle = declaration(base[0], "border-style")
  const selectedStyle = declaration(checked[0], "border-style")
  assert.ok(unselectedStyle && selectedStyle, "both cards must declare a border-style under forced colours")
  assert.notEqual(selectedStyle, unselectedStyle, "the two states must differ by something other than colour")
})

// -- the screens ------------------------------------------------------------

// Both states every screen has to survive: before the catalog arrives, and
// after. D3's prices are asserted in prices.test.mjs; the structure below has
// to hold in either state, and a screen that rendered nothing until the fetch
// landed would be a blank page for as long as the fetch takes.
const CATALOGS = [null, parseCatalog(catalogPayload())]

test("every screen has exactly one <h1>, and it says something", () => {
  for (const catalog of CATALOGS) {
    for (const id of SCREEN_IDS) {
      const headings = findAll(screenView(id, {}, catalog), "h1")
      assert.equal(headings.length, 1, `screen ${id} must have exactly one <h1>, found ${headings.length}`)
      assert.notEqual(text(headings[0].node).trim(), "", `the <h1> of screen ${id} is empty`)
    }
  }
})

test("every screen renders something under its heading", () => {
  for (const catalog of CATALOGS) {
    for (const id of SCREEN_IDS) {
      const view = screenView(id, {}, catalog)
      assert.ok(view.children.length > 1, `screen ${id} is a bare heading`)
    }
  }
})

test("the three question screens are the ones the plan names", () => {
  // Without this the loops below would pass vacuously if QUESTIONS were empty.
  assert.deepEqual(QUESTIONS, ["tier", "months", "pay"])
})

test("each question is a real fieldset with a real legend", () => {
  for (const catalog of CATALOGS) {
    for (const id of QUESTIONS) {
      const view = screenView(id, {}, catalog)
      const fieldsets = findAll(view, "fieldset")
      assert.equal(fieldsets.length, 1, `screen ${id} must group its radios in one <fieldset>`)
      const legends = findAll(fieldsets[0].node, "legend")
      assert.equal(legends.length, 1, `the <fieldset> of screen ${id} must have exactly one <legend>`)
      assert.notEqual(text(legends[0].node).trim(), "", `the <legend> of screen ${id} is empty`)
    }
  }
})

test("every option is a radio inside a label, and every option is offered", () => {
  for (const catalog of CATALOGS) {
    for (const id of QUESTIONS) {
      const view = screenView(id, {}, catalog)
      const radios = findAll(view, "input").filter(({node}) => node.attrs.type === "radio")
      const options = optionsOfQuestion(questionOfScreen(id), {}, catalog)
      assert.equal(radios.length, options.length, `screen ${id} offers ${options.length} options but renders ${radios.length} radios`)
      assert.ok(options.length > 1, `screen ${id} offers ${options.length} option(s), so the comparison above is vacuous`)
      for (const {node, parents} of radios) {
        assert.ok(
          parents.some((p) => p.tag === "label"),
          `a radio on screen ${id} is not inside a <label>, so its card is not clickable and it has no accessible name`
        )
        assert.equal(node.attrs.name, id, `a radio on screen ${id} is in the wrong group`)
      }
      assert.deepEqual(
        radios.map(({node}) => node.attrs.value),
        options.map((o) => o.value)
      )
    }
  }
})

test("the chosen option is the checked one, and only that one", () => {
  const catalog = CATALOGS[1]
  for (const id of QUESTIONS) {
    const options = optionsOfQuestion(questionOfScreen(id), {}, catalog)
    const chosen = options[options.length - 1].value
    // tier answers months's own options, which is why the whole answer set is
    // passed rather than only this screen's.
    const answers = {tier: "supporter", [id]: chosen}
    const radios = findAll(screenView(id, answers, catalog), "input")
    const checked = radios.filter(({node}) => "checked" in node.attrs).map(({node}) => node.attrs.value)
    assert.deepEqual(checked, [chosen], `screen ${id} must check exactly the chosen option`)
  }
})

test("the checkout summary shows every answer, and says so when one is missing", () => {
  const catalog = CATALOGS[1]
  const answered = screenView("checkout", {tier: "legend", months: "12", pay: "xmr"}, catalog)
  const summary = text(answered)
  assert.match(summary, /Legend/)
  assert.match(summary, /12 months/)
  assert.match(summary, /Monero/)
  assert.doesNotMatch(summary, /Not chosen/)
  assert.match(text(screenView("checkout", {}, catalog)), /Not chosen/, "an unanswered question must not render as blank")
})

// -- where a visit starts (D5's prefill rests on this) -----------------------

test("with no answers the visit starts at the first screen", () => {
  assert.equal(firstUnansweredScreen({}), "tier")
})

test("a seeded answer skips its screen, and only up to the first gap", () => {
  assert.equal(firstUnansweredScreen({tier: "legend"}), "months")
  assert.equal(firstUnansweredScreen({tier: "legend", months: "12"}), "pay")
  // months is seeded but tier is not: the gap is asked, not skipped past.
  assert.equal(firstUnansweredScreen({months: "12"}), "tier")
  assert.equal(firstUnansweredScreen({pay: "xmr"}), "tier")
})

test("every question answered lands on the summary, and never past it", () => {
  assert.equal(firstUnansweredScreen({tier: "legend", months: "12", pay: "xmr"}), "checkout")
})

// -- routing and the shell --------------------------------------------------

test("the shell registers a popstate listener, so back and forward work", () => {
  assert.match(uiJs, /addEventListener\(\s*["']popstate["']/, "no popstate listener in the built shell")
})

test("nothing can raise a native validation bubble", () => {
  // The browser list holds "an unanswered question shows the banner rather
  // than a native bubble". Whether the banner is *visible* needs a browser;
  // whether a bubble is possible does not. It takes a constraint to fail and
  // a submit that reaches the browser's default: neither exists.
  for (const id of SCREEN_IDS) {
    for (const {node} of findAll(screenView(id, {}, CATALOGS[1]), "input")) {
      for (const attr of ["required", "pattern", "min", "max", "minlength", "maxlength"]) {
        assert.ok(!(attr in node.attrs), `an input on screen ${id} carries ${attr}, which can raise a native bubble`)
      }
    }
  }
  assert.match(uiJs, /addEventListener\(\s*["']submit["']/, "no submit listener in the built shell")
  assert.match(uiJs, /\.preventDefault\(\)/, "the submit handler must stop the browser's own handling")
})

test("the shell can redraw in place, without a history entry", () => {
  // The catalog arrives after the first render. Without refresh() its only
  // way back onto the screen is go(), which pushes a duplicate entry and
  // breaks the back button.
  assert.match(uiJs, /refresh\(\)\s*\{/, "Shell must expose refresh()")
})

test("the catalog reaches the screens through the shell, and redraws them", () => {
  // The shell is the one module a test cannot execute here (it needs a DOM),
  // so these two are read out of the built file. What they check is on the
  // browser list as well: that the prices actually appear when the fetch lands.
  assert.match(uiJs, /screenView\(\w+,\s*\w+,\s*catalog\)/, "the shell must render the screens from the catalog it holds")
  assert.match(uiJs, /setCatalog\([^)]*\)\s*\{[^}]*refresh\(\)/, "setCatalog must redraw, or the prices land in a variable and nowhere else")
})

test("the message for an unanswered question depends on whether prices have arrived", () => {
  // Every option is disabled until the catalog lands, so "choose an option"
  // would blame the visitor for the fetch.
  assert.match(nothingChosenMessage(null), /Prices are still loading/)
  assert.match(nothingChosenMessage(CATALOGS[1]), /Choose an option/)
  assert.match(uiJs, /nothingChosenMessage\(catalog\)/, "the shell must use it, not a message of its own")
})

test("the shell navigates with pushState and never reloads the page", () => {
  assert.match(uiJs, /history\.pushState\(/)
  assert.equal(uiJs.match(/location\.(assign|reload|replace)\(/g), null, "a page reload would lose the wizard's answers")
})

test("no module builds markup from a string", () => {
  // Everything an option's label may later carry — catalog copy (D3), a query
  // parameter (D5), a provider's address (E5) — reaches the page as a text
  // node, so none of it can become markup.
  const dist = fileURLToPath(new URL("../dist", import.meta.url))
  for (const name of readdirSync(dist).filter((n) => n.endsWith(".js"))) {
    assert.equal(read(`../dist/${name}`).match(/\.innerHTML|\.outerHTML|insertAdjacentHTML\(|document\.write\(/g), null, `in dist/${name}`)
  }
})

// -- the served page --------------------------------------------------------

test("index.html carries a token for each logo, and dev.html resolves both", () => {
  for (const logo of LOGOS) {
    assert.ok(indexHtml.includes(`@@${logo}@@`), `index.html must reference ${logo} by token`)
    assert.ok(devHtml.includes(`./${logo}`), `dev.html must resolve ${logo} into dist/`)
  }
  assert.equal(devHtml.match(/@@[\w.-]+@@/g), null, "dev.html has an unresolved token")
})

test("the built assets are the two logos and nothing else", () => {
  const assets = readdirSync(fileURLToPath(new URL("../assets", import.meta.url))).filter((n) => !n.startsWith("."))
  assert.deepEqual(assets.sort(), [...LOGOS].sort())
})

test("the support contact renders on every screen, from index.html's footer", () => {
  assert.match(indexHtml, /<footer[^>]*>[\s\S]*@@support_contact@@[\s\S]*<\/footer>/)
  // Outside <main id="app">, which the shell replaces on every navigation.
  assert.ok(indexHtml.indexOf("</main>") < indexHtml.indexOf("<footer"))
})

test("no token is written anywhere but index.html", () => {
  const tokenRe = /@@[\w.-]+@@/g
  const src = fileURLToPath(new URL("../src", import.meta.url))
  for (const name of readdirSync(src)) {
    assert.equal(read(`../src/${name}`).match(tokenRe), null, `src/${name} carries a token, which nothing substitutes`)
  }
  assert.equal(cssSource.match(tokenRe), null)
})

test("exactly one <h1> exists in the page shell too, counting the screens' own", () => {
  // The screens supply the <h1>; index.html must not add a second one.
  assert.equal(indexHtml.match(/<h1[\s>]/g), null)
})
