// Reading a rendered screen back.
//
// view.ts describes a screen as an element tree and ui.ts turns that tree into
// elements with createElement/textContent. There is no browser here, so the
// tests assert over the tree — the same value a browser would be handed — and
// read it back the way a reader sees it: an option card is its label and its
// detail LINE, not the fields some function returned.

export function walk(node, visit, parents = []) {
  visit(node, parents)
  const chain = [...parents, node]
  for (const child of node.children) if (typeof child !== "string") walk(child, visit, chain)
}

export function findAll(root, tag) {
  const found = []
  walk(root, (node, parents) => {
    if (node.tag === tag) found.push({node, parents})
  })
  return found
}

/** Every string in the subtree, in document order. */
export function text(node) {
  let out = ""
  walk(node, (n) => {
    for (const child of n.children) if (typeof child === "string") out += child
  })
  return out
}

function byClass(root, className) {
  const found = []
  walk(root, (node) => {
    if (node.attrs.class === className) found.push(node)
  })
  return found
}

function oneByClass(root, className) {
  const found = byClass(root, className)
  if (found.length !== 1) throw new Error(`expected exactly one .${className}, found ${found.length}`)
  return found[0]
}

/** The radio cards of a question screen, as rendered. */
export function optionCards(view) {
  return byClass(view, "option").map((card) => {
    const input = oneByClass(card, "option__input")
    return {
      value: input.attrs.value,
      disabled: "disabled" in input.attrs,
      checked: "checked" in input.attrs,
      label: text(oneByClass(card, "option__label")),
      detail: text(oneByClass(card, "option__detail")),
    }
  })
}

/** The checkout summary as {term: value}, in the order it is rendered. */
export function summaryRows(view) {
  const terms = byClass(view, "summary__term").map(text)
  const values = byClass(view, "summary__value").map(text)
  if (terms.length !== values.length) throw new Error(`summary has ${terms.length} terms and ${values.length} values`)
  return Object.fromEntries(terms.map((term, i) => [term, values[i]]))
}
