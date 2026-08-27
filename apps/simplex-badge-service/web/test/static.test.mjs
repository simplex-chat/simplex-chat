// The static half of D2's verification.
//
// There is no browser and no display in this environment, so nothing here
// claims that anything *renders*. What it does assert is every property of the
// built output that a browser would otherwise be the only witness to, and each
// assertion was checked to fail when the thing it names is broken (see the
// D2 report). Run with `npm test` after `npm run build`.
//
// Assertions are against dist/ and against the two files served from web/,
// never against src/: the served bytes are what a browser gets.

import test from "node:test"
import assert from "node:assert/strict"
import {readFileSync, readdirSync} from "node:fs"
import {fileURLToPath} from "node:url"

import {SCREEN_IDS} from "../dist/router.js"
import {optionsOfQuestion, questionOfScreen, screenView} from "../dist/view.js"
import {atRule, customProperties, referencedProperties, rules, stripComments, withoutAtRules} from "./css.mjs"

const read = (rel) => readFileSync(fileURLToPath(new URL(rel, import.meta.url)), "utf8")

const cssSource = read("../styles.css")
const css = stripComments(cssSource)
const topLevel = withoutAtRules(css)
const darkBlock = atRule(css, "prefers-color-scheme: dark")
const indexHtml = read("../index.html")
const devHtml = read("../dist/dev.html")
const uiJs = read("../dist/ui.js")

// Derived, not listed: a screen that view.ts starts calling a question is
// covered by the fieldset and label checks below without editing this file.
const QUESTIONS = SCREEN_IDS.filter((id) => questionOfScreen(id) !== null)
const LOGOS = ["logo-symbol-light.svg", "logo-symbol-dark.svg"]

// -- colour tokens ----------------------------------------------------------

test("every custom property the sheet uses is defined on bare :root", () => {
  const rootBlocks = rules(topLevel, ":root")
  assert.equal(rootBlocks.length, 1, "expected exactly one top-level :root rule")
  const defined = customProperties(rootBlocks[0])
  assert.ok(defined.size > 0, "the :root rule declares no custom properties")
  for (const name of referencedProperties(css)) {
    assert.ok(defined.has(name), `${name} is used but has no definition on bare :root, so it is undefined in light mode`)
  }
})

test("the dark block only redefines tokens, and introduces none", () => {
  const light = customProperties(rules(topLevel, ":root")[0])
  const darkRoot = rules(darkBlock, ":root")
  assert.equal(darkRoot.length, 1, "expected exactly one :root rule inside the dark media query")
  const dark = customProperties(darkRoot[0])
  assert.ok(dark.size > 0, "the dark :root rule declares no custom properties")
  for (const name of dark.keys()) {
    assert.ok(light.has(name), `${name} is defined only in dark mode, so it is undefined in light mode`)
  }
})

test("no custom property is declared outside a :root rule", () => {
  // A token declared on a selector would be scoped to it, and its light and
  // dark definitions could then disagree about which elements they reach.
  for (const block of [topLevel, darkBlock]) {
    let i = 0
    while (i < block.length) {
      const open = block.indexOf("{", i)
      if (open < 0) break
      const selector = block.slice(i, open).trim()
      const close = matchingClose(block, open)
      if (selector !== ":root") {
        const stray = [...customProperties(block.slice(open + 1, close)).keys()]
        assert.deepEqual(stray, [], `${selector} declares custom properties: ${stray.join(", ")}`)
      }
      i = close + 1
    }
  }
})

function matchingClose(s, open) {
  let depth = 0
  for (let i = open; i < s.length; i++) {
    if (s[i] === "{") depth++
    else if (s[i] === "}" && --depth === 0) return i
  }
  throw new Error("unclosed block")
}

test("the accent is the website's primary colour in both schemes", () => {
  const light = customProperties(rules(topLevel, ":root")[0])
  const dark = customProperties(rules(darkBlock, ":root")[0])
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
  const body = rules(topLevel, "body")
  assert.equal(body.length, 1)
  assert.match(body[0], /font-family:\s*system-ui/)
})

// -- layout, motion, focus --------------------------------------------------

test("the column is capped at 560px", () => {
  const column = rules(topLevel, ".column")
  assert.equal(column.length, 1, "expected exactly one .column rule")
  assert.match(column[0], /max-width:\s*560px/)
})

test("the screen transition is 150ms and prefers-reduced-motion turns it off", () => {
  const screen = rules(topLevel, ".screen")
  assert.equal(screen.length, 1, "expected exactly one .screen rule")
  assert.match(screen[0], /animation:[^;]*\b150ms\b/, ".screen must carry the 150 ms fade")
  const reduced = atRule(css, "prefers-reduced-motion: reduce")
  const reducedScreen = rules(reduced, ".screen")
  assert.equal(reducedScreen.length, 1, "the reduced-motion block must address .screen")
  assert.match(reducedScreen[0], /animation:\s*none/)
})

test("a :focus-visible ring exists and nothing suppresses it", () => {
  const ring = rules(topLevel, ":focus-visible")
  assert.equal(ring.length, 1, "expected a global :focus-visible rule")
  assert.match(ring[0], /outline:\s*\d+px\s+solid/)
  // Not "no blanket outline:none" but "no outline:none at all" — the weaker
  // rule needs a judgement about which selectors count as blanket.
  assert.equal(css.match(/outline\s*:\s*none/g), null, "outline: none anywhere would put this ring in doubt")
})

test("the selected card gets a 2px accent border, as PeriodCard does", () => {
  const body = rules(topLevel, ".option__body")
  assert.equal(body.length, 1)
  assert.match(body[0], /border:\s*2px\s+solid\s+var\(--border\)/)
  const checked = rules(topLevel, ".option__input:checked + .option__body")
  assert.equal(checked.length, 1, "expected a rule for the checked card")
  assert.match(checked[0], /border-color:\s*var\(--accent\)/)
})

test("the focus ring reaches the card, since the radio itself is visually hidden", () => {
  const hidden = rules(topLevel, ".option__input")
  assert.equal(hidden.length, 1)
  assert.doesNotMatch(hidden[0], /display:\s*none/, "display: none would drop the radio out of the tab order")
  const focused = rules(topLevel, ".option__input:focus-visible + .option__body")
  assert.equal(focused.length, 1, "a hidden radio must show its focus on the card")
  assert.match(focused[0], /outline:\s*\d+px\s+solid/)
})

// -- the screens ------------------------------------------------------------

function walk(node, visit, parents = []) {
  visit(node, parents)
  const chain = [...parents, node]
  for (const child of node.children) if (typeof child !== "string") walk(child, visit, chain)
}

function findAll(root, tag) {
  const found = []
  walk(root, (node, parents) => {
    if (node.tag === tag) found.push({node, parents})
  })
  return found
}

function text(node) {
  let out = ""
  walk(node, (n) => {
    for (const child of n.children) if (typeof child === "string") out += child
  })
  return out
}

test("every screen has exactly one <h1>, and it says something", () => {
  for (const id of SCREEN_IDS) {
    const headings = findAll(screenView(id, {}), "h1")
    assert.equal(headings.length, 1, `screen ${id} must have exactly one <h1>, found ${headings.length}`)
    assert.notEqual(text(headings[0].node).trim(), "", `the <h1> of screen ${id} is empty`)
  }
})

test("every screen renders something under its heading", () => {
  for (const id of SCREEN_IDS) {
    const view = screenView(id, {})
    assert.ok(view.children.length > 1, `screen ${id} is a bare heading`)
  }
})

test("the three question screens are the ones the plan names", () => {
  // Without this the loops below would pass vacuously if QUESTIONS were empty.
  assert.deepEqual(QUESTIONS, ["tier", "months", "pay"])
})

test("each question is a real fieldset with a real legend", () => {
  for (const id of QUESTIONS) {
    const view = screenView(id, {})
    const fieldsets = findAll(view, "fieldset")
    assert.equal(fieldsets.length, 1, `screen ${id} must group its radios in one <fieldset>`)
    const legends = findAll(fieldsets[0].node, "legend")
    assert.equal(legends.length, 1, `the <fieldset> of screen ${id} must have exactly one <legend>`)
    assert.notEqual(text(legends[0].node).trim(), "", `the <legend> of screen ${id} is empty`)
  }
})

test("every option is a radio inside a label, and every option is offered", () => {
  for (const id of QUESTIONS) {
    const view = screenView(id, {})
    const radios = findAll(view, "input").filter(({node}) => node.attrs.type === "radio")
    const options = optionsOfQuestion(questionOfScreen(id))
    assert.equal(radios.length, options.length, `screen ${id} offers ${options.length} options but renders ${radios.length} radios`)
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
})

test("the chosen option is the checked one, and only that one", () => {
  for (const id of QUESTIONS) {
    const options = optionsOfQuestion(questionOfScreen(id))
    const chosen = options[options.length - 1].value
    const radios = findAll(screenView(id, {[id]: chosen}), "input")
    const checked = radios.filter(({node}) => "checked" in node.attrs).map(({node}) => node.attrs.value)
    assert.deepEqual(checked, [chosen], `screen ${id} must check exactly the chosen option`)
  }
})

test("the checkout summary shows every answer, and says so when one is missing", () => {
  const answered = screenView("checkout", {tier: "legend", months: "12", pay: "xmr"})
  const summary = text(answered)
  assert.match(summary, /Legend/)
  assert.match(summary, /12 months/)
  assert.match(summary, /Monero/)
  assert.doesNotMatch(summary, /Not chosen/)
  assert.match(text(screenView("checkout", {})), /Not chosen/, "an unanswered question must not render as blank")
})

// -- routing and the shell --------------------------------------------------

test("the shell registers a popstate listener, so back and forward work", () => {
  assert.match(uiJs, /addEventListener\(\s*["']popstate["']/, "no popstate listener in the built shell")
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
