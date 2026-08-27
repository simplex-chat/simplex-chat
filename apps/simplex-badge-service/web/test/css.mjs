// A very small CSS reader for the assertions in static.test.mjs.
//
// It understands exactly what this stylesheet uses — nested at-rules, rule
// blocks and custom properties — because the alternative is a parser
// dependency and this project is capped at one (decision 7). Anything it
// cannot answer, it answers by throwing rather than by returning nothing: a
// check that silently finds no rules is a check that cannot fail.

/** The body of the block whose opening brace is at or after `from`. */
function blockAt(css, from) {
  const open = css.indexOf("{", from)
  if (open < 0) throw new Error(`no block after offset ${from}`)
  let depth = 0
  for (let i = open; i < css.length; i++) {
    if (css[i] === "{") depth++
    else if (css[i] === "}" && --depth === 0) return {body: css.slice(open + 1, i), end: i + 1}
  }
  throw new Error(`unclosed block opened at offset ${open}`)
}

/** Comments removed, so that prose in them is never mistaken for a declaration. */
export function stripComments(css) {
  return css.replace(/\/\*[\s\S]*?\*\//g, "")
}

/**
 * The body of the at-rule whose prelude contains `prelude`, e.g.
 * "prefers-color-scheme: dark". Throws when there is not exactly one.
 */
export function atRule(css, prelude) {
  const found = []
  const re = /@media[^{]*/g
  for (const m of css.matchAll(re)) {
    if (m[0].includes(prelude)) found.push(blockAt(css, m.index).body)
  }
  if (found.length !== 1) throw new Error(`expected exactly one @media containing "${prelude}", found ${found.length}`)
  return found[0]
}

/** Every at-rule block removed, leaving only top-level rules. */
export function withoutAtRules(css) {
  let out = ""
  let i = 0
  while (i < css.length) {
    const at = css.indexOf("@", i)
    if (at < 0) return out + css.slice(i)
    out += css.slice(i, at)
    const semi = css.indexOf(";", at)
    const brace = css.indexOf("{", at)
    // An at-rule with no block (@import, @charset) ends at its semicolon.
    if (semi >= 0 && (brace < 0 || semi < brace)) i = semi + 1
    else i = blockAt(css, at).end
  }
  return out
}

/** The declarations of every rule whose selector list matches `selector` exactly. */
export function rules(css, selector) {
  const found = []
  let i = 0
  while (i < css.length) {
    const open = css.indexOf("{", i)
    if (open < 0) break
    const sel = css.slice(i, open).trim()
    const {body, end} = blockAt(css, open)
    if (sel === selector) found.push(body)
    i = end
  }
  return found
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
