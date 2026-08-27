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
 * Every style rule in the sheet, in document order, as
 * `{selector, body, start, at}` — `at` being the preludes of the at-rules
 * enclosing it, outermost first, and `start` the offset of its selector in the
 * sheet as a whole. Keyframe blocks are skipped: `from`/`to` are not selectors.
 */
export function allRules(css, at = [], base = 0) {
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
      if (!head.startsWith("@keyframes")) found.push(...allRules(block.body, [...at, head], base + open + 1))
    } else {
      const raw = css.slice(i, open)
      const indent = raw.length - raw.trimStart().length
      found.push({selector: head, body: block.body, start: base + i + indent, at})
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
