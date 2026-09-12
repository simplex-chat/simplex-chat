// A stylesheet is not a string to grep. Substring assertions passed a palette whose accent had
// been changed to #FF00FF with "#0053D0" left in a comment, a reduced-motion block that matched no
// element, and a genuine second layout under 560px, so the declarations are parsed out here and the
// tests assert their values.
import { readFileSync } from "node:fs";

export interface Rule { selector: string; decls: Map<string, string> }
interface Sheet { rules: Rule[]; media: Array<{ query: string; rules: Rule[] }> }

function declarations(body: string): Map<string, string> {
  const out = new Map<string, string>();
  for (const part of body.split(";")) {
    const at = part.indexOf(":");
    if (at < 0) continue;
    out.set(part.slice(0, at).trim().toLowerCase(), part.slice(at + 1).trim());
  }
  return out;
}

/** Brace-matching, comments stripped first, one level of @media. */
function parseCss(source: string): Sheet {
  const text = source.replace(/\/\*[\s\S]*?\*\//g, "");
  const sheet: Sheet = { rules: [], media: [] };
  let i = 0;
  while (i < text.length) {
    const open = text.indexOf("{", i);
    if (open < 0) break;
    const prelude = text.slice(i, open).trim();
    let depth = 1;
    let j = open + 1;
    while (j < text.length && depth > 0) {
      if (text[j] === "{") depth++;
      else if (text[j] === "}") depth--;
      j++;
    }
    const body = text.slice(open + 1, j - 1);
    if (prelude.startsWith("@media")) {
      sheet.media.push({ query: prelude.slice("@media".length).trim(), rules: parseCss(body).rules });
    } else if (!prelude.startsWith("@")) {
      for (const selector of prelude.split(",")) sheet.rules.push({ selector: selector.trim(), decls: declarations(body) });
    }
    i = j;
  }
  return sheet;
}

export const sheet = parseCss(readFileSync(new URL("../../public/styles.css", import.meta.url), "utf8"));

export const ruleFor = (rules: Rule[], selector: string): Rule | undefined => rules.find((r) => r.selector === selector);

export const mediaFor = (query: string): { query: string; rules: Rule[] } | undefined =>
  sheet.media.find((m) => m.query.replace(/\s+/g, "") === query.replace(/\s+/g, ""));

/** What that selector ends up declaring, the last value in the sheet and not the first: `.notice, .warn` sets
 * a shared ground that `.notice` overrides, so the first rule asserts a value the browser never uses. */
export function decl(selector: string, property: string): string | undefined {
  let value: string | undefined;
  for (const rule of sheet.rules) {
    if (rule.selector !== selector) continue;
    const found = rule.decls.get(property);
    if (found !== undefined) value = found;
  }
  return value;
}
