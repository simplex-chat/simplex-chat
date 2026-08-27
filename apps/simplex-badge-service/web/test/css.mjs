// A very small CSS reader for the assertions in static.test.mjs.
//
// It understands exactly what this stylesheet uses — nested at-rules, rule
// blocks and custom properties — because the alternative is a parser
// dependency and this project is capped at one (decision 7). Anything it
// cannot answer, it answers by throwing rather than by returning nothing: a
// check that silently finds no rules is a check that cannot fail.
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

/** True when a media prelude holds in `env`. Any condition not modelled is false. */
function mediaMatches(prelude, env) {
  if (prelude.includes("prefers-color-scheme: dark")) return env.scheme === "dark"
  if (prelude.includes("prefers-color-scheme: light")) return env.scheme === "light"
  // Everything else — reduced motion, forced colours — is off in the plain
  // environment these questions are asked in. Modelling one as "always true"
  // would silently answer for a browser nobody was asking about.
  return false
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

/**
 * True when a selector list matches `element` — `{tag, classes}`.
 *
 * Only class, type and universal selectors are understood. Anything else
 * (combinators, pseudo-classes) is treated as not matching, which is safe
 * here: no such rule in this sheet targets a logo.
 */
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
 * The value `property` actually takes on `element` in `env`, or undefined when
 * no matching rule declares it. Applicable rules are ordered by specificity
 * and then by document position, which is the cascade for a sheet with no
 * `!important`, no inline styles and no layers — this one.
 */
export function effectiveValue(css, element, property, env) {
  const winners = allRules(css)
    .map((rule, order) => ({...rule, order}))
    .filter((r) => r.at.every((a) => mediaMatches(a, env)))
    .filter((r) => selectorMatches(r.selector, element))
    .filter((r) => declaration(r.body, property) !== undefined)
    .sort((a, b) => {
      const [sa, sb] = [specificity(a.selector.split(",")[0]), specificity(b.selector.split(",")[0])]
      for (let i = 0; i < 3; i++) if (sa[i] !== sb[i]) return sa[i] - sb[i]
      return a.order - b.order
    })
  return winners.length ? declaration(winners[winners.length - 1].body, property) : undefined
}
