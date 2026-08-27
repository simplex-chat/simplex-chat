// A very small CSS reader for the assertions in static.test.mjs.
//
// It understands exactly what this stylesheet uses — nested at-rules, rule
// blocks and custom properties — because the alternative is a parser
// dependency and this project is capped at one (decision 7). Anything it
// cannot answer, it answers by throwing rather than by returning nothing: a
// check that silently finds no rules is a check that cannot fail, and a
// cascade resolver that silently ignores what it cannot read is worse still,
// because it returns a confident wrong answer. That guarantee is enforced,
// not documented: see `effectiveValue`.
//
// Rules come back in document order with the at-rules enclosing them, because
// two of the questions asked here are ordering questions. A conditional group
// rule adds no specificity and does not reorder its contents, so a declaration
// inside @media cascades at its position in the file: whether a scheme
// override actually wins depends on where it sits, not on being in a media
// query at all.

/** The body of the block whose opening brace is at or after `from`. */
function blockAt(css, from) {
  const open = css.indexOf("{", from)
  if (open < 0) throw new Error(`no block after offset ${from}`)
  let depth = 0
  for (let i = open; i < css.length; i++) {
    if (css[i] === "{") depth++
    else if (css[i] === "}" && --depth === 0) return {open, body: css.slice(open + 1, i), end: i + 1}
  }
  throw new Error(`unclosed block opened at offset ${open}`)
}

/** Comments removed, so that prose in them is never mistaken for a declaration. */
export function stripComments(css) {
  return css.replace(/\/\*[\s\S]*?\*\//g, "")
}

/**
 * Every style rule in the sheet, in document order, as `{selector, body, at}`
 * — `at` being the preludes of the at-rules enclosing it, outermost first.
 * Position in the returned array *is* document order, which is what the
 * cascade needs; no offset is carried, because nothing would read it.
 * Keyframe blocks are skipped: `from`/`to` are not selectors.
 */
export function allRules(css, at = []) {
  const found = []
  let i = 0
  while (i < css.length) {
    const open = css.indexOf("{", i)
    if (open < 0) break
    const head = css.slice(i, open).trim()
    const block = blockAt(css, open)
    if (head.startsWith("@")) {
      const semi = css.indexOf(";", i)
      // An at-rule with no block (@import, @charset) ends at its semicolon.
      if (semi >= 0 && semi < open) {
        i = semi + 1
        continue
      }
      if (!head.startsWith("@keyframes")) found.push(...allRules(block.body, [...at, head]))
    } else {
      found.push({selector: head, body: block.body, at})
    }
    i = block.end
  }
  return found
}

/** The declarations of every top-level rule whose selector list is exactly `selector`. */
export function rules(css, selector) {
  return allRules(css)
    .filter((r) => r.at.length === 0 && r.selector === selector)
    .map((r) => r.body)
}

/** The same, for rules inside the at-rule whose prelude contains `prelude`. */
export function mediaRules(css, prelude, selector) {
  return allRules(css)
    .filter((r) => r.at.some((a) => a.includes(prelude)) && r.selector === selector)
    .map((r) => r.body)
}

/** The custom properties declared by a block, as a Map from name to value. */
export function customProperties(body) {
  const props = new Map()
  for (const m of body.matchAll(/(--[\w-]+)\s*:\s*([^;}]+)/g)) props.set(m[1], m[2].trim())
  return props
}

/** Every custom property referenced through var(), anywhere in the sheet. */
export function referencedProperties(css) {
  return new Set([...css.matchAll(/var\(\s*(--[\w-]+)/g)].map((m) => m[1]))
}

/** The value a block gives `property`, or undefined. */
export function declaration(body, property) {
  const m = body.match(new RegExp(`(?:^|;)\\s*${property}\\s*:\\s*([^;}]+)`))
  return m ? m[1].trim() : undefined
}

// -- resolving the cascade --------------------------------------------------
//
// Enough of it for the questions this stylesheet raises, and no more. Asking
// "which rule is last" only answers the cascade when you already know which
// rules match and that they are equally specific; asking for the *effective*
// value of a property on an element does not require knowing that in advance,
// which is why the logo check is written the second way.
//
// Every construct below that this resolver does not model makes it THROW. That
// is the whole point of the section: a partial cascade that quietly drops what
// it cannot read returns a confident wrong answer, which is worse than the
// narrow check it replaced. `!important` is the case that proved it — an early
// `.logo--dark { display: block !important }` outranks every later plain
// declaration in a browser, and an order-and-specificity resolver reports the
// opposite while every test stays green.

/** The conditions this resolver knows how to evaluate. Anything else throws. */
const MODELLED_MEDIA = [
  {condition: "prefers-color-scheme: dark", holds: (env) => env.scheme === "dark"},
  {condition: "prefers-color-scheme: light", holds: (env) => env.scheme === "light"},
  // Off in the plain environment these questions are asked in. Modelling one as
  // always true would silently answer for a browser nobody was asking about.
  {condition: "prefers-reduced-motion: reduce", holds: () => false},
  {condition: "forced-colors: active", holds: () => false},
]

/** True when an at-rule prelude holds in `env`. Throws on anything unmodelled. */
function atRuleHolds(prelude, env) {
  if (!prelude.startsWith("@media")) {
    throw new Error(`${prelude.split("{")[0].trim()} is not modelled by effectiveValue; it would change which rule wins. Teach it or remove the rule.`)
  }
  const known = MODELLED_MEDIA.find((m) => prelude.includes(m.condition))
  if (!known) throw new Error(`media condition not modelled by effectiveValue: ${prelude.trim()}`)
  return known.holds(env)
}

/** Specificity of a simple selector, as [ids, classes, types]. Null if not simple. */
function specificity(selector) {
  const s = selector.trim()
  if (s === "*") return [0, 0, 0]
  if (!/^(?:[a-z][\w-]*)?(?:\.[\w-]+)*$/i.test(s) || s === "") return null
  const type = /^[a-z][\w-]*/i.exec(s)
  const classes = [...s.matchAll(/\.([\w-]+)/g)]
  return [0, classes.length, type ? 1 : 0]
}

/** True when a simple selector list matches `element` — `{tag, classes}`. */
function selectorMatches(selectorList, element) {
  return selectorList.split(",").some((part) => {
    const s = part.trim()
    if (s === "*") return true
    if (specificity(s) === null) return false
    const type = /^[a-z][\w-]*/i.exec(s)
    if (type && type[0] !== element.tag) return false
    return [...s.matchAll(/\.([\w-]+)/g)].every((m) => element.classes.includes(m[1]))
  })
}

/**
 * True when a selector list contains a part this resolver cannot read that
 * could nonetheless match `element`.
 *
 * Combinators and pseudo-classes are not understood, and treating them as
 * "does not match" is only safe when they demonstrably cannot match: a part
 * naming none of the element's classes, not its tag and not `*` cannot. A
 * `::pseudo-element` part styles something that is not the element at all.
 */
function unreadableAndCouldMatch(selectorList, element) {
  return selectorList.split(",").some((part) => {
    const s = part.trim()
    if (s === "*" || specificity(s) !== null || s.includes("::")) return false
    if (s.includes("*")) return true
    if (new RegExp(`(^|[^\\w.-])${element.tag}([^\\w-]|$)`).test(s)) return true
    return element.classes.some((c) => s.includes(`.${c}`))
  })
}

/**
 * The value `property` actually takes on `element` in `env`, or undefined when
 * no matching rule declares it.
 *
 * Applicable rules are ordered by specificity and then by document position,
 * which is the whole cascade for a sheet with no `!important` and no `@layer`.
 * Both are refused rather than assumed: see the checks below. (Inline styles
 * and the style attribute cannot occur in a stylesheet, so they need no
 * clause — the earlier docstring naming them was noise.)
 */
export function effectiveValue(css, element, property, env) {
  // Layer order is sheet-wide and reorders the cascade regardless of where the
  // rules sit, so this one is global rather than per-candidate.
  if (/@layer\b/.test(css)) {
    throw new Error("effectiveValue does not model @layer, which reorders the cascade sheet-wide. Teach it or drop the layer.")
  }
  const candidates = allRules(css)
    .map((rule, order) => ({...rule, order}))
    .filter((r) => {
      if (unreadableAndCouldMatch(r.selector, element)) {
        throw new Error(`effectiveValue cannot read the selector "${r.selector}", which may match ${element.tag}.${element.classes.join(".")}`)
      }
      return selectorMatches(r.selector, element)
    })
    .filter((r) => declaration(r.body, property) !== undefined)
  for (const r of candidates) {
    if (/!\s*important/.test(declaration(r.body, property))) {
      throw new Error(`"${r.selector} { ${property}: ${declaration(r.body, property)} }" is !important, which outranks every plain declaration whatever the order. effectiveValue does not model it.`)
    }
  }
  const winners = candidates
    .filter((r) => r.at.every((a) => atRuleHolds(a, env)))
    .sort((a, b) => {
      const [sa, sb] = [specificity(a.selector.split(",")[0]), specificity(b.selector.split(",")[0])]
      for (let i = 0; i < 3; i++) if (sa[i] !== sb[i]) return sa[i] - sb[i]
      return a.order - b.order
    })
  return winners.length ? declaration(winners[winners.length - 1].body, property) : undefined
}
