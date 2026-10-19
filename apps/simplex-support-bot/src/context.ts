import {readFileSync} from "fs"
import {parse as parseYaml} from "yaml"
import {GrokMessage} from "./grok.js"

const ALLOWED_ROLES: ReadonlySet<GrokMessage["role"]> = new Set(["system", "user", "assistant"])
// Roles surfaced from a YAML transcript. `user` entries from the file are
// validated but dropped — the customer's runtime message is the only
// `user` content sent to Grok.
const PREPEND_ROLES: ReadonlySet<GrokMessage["role"]> = new Set(["system", "assistant"])

// Loads --context-file. The flag is documented as "text file with Grok
// system context"; a `.yaml` / `.yml` extension is an undocumented
// alternative that switches to a multi-turn transcript in the harness
// format (a flat list of `{role, message}` entries).
export function loadGrokContext(path: string): GrokMessage[] {
  const text = readFileSync(path, "utf-8")
  return isYamlPath(path) ? parseYamlTranscript(path, text) : [{role: "system", content: text}]
}

function isYamlPath(path: string): boolean {
  const lower = path.toLowerCase()
  return lower.endsWith(".yaml") || lower.endsWith(".yml")
}

// Parses the harness transcript format. Returns only `system` and
// `assistant` turns; `user` entries are intentionally excluded so they
// don't merge with the customer's runtime message. Malformed YAML,
// unknown roles, or non-string messages throw — operator-supplied
// configuration should fail-fast at startup, not silently degrade.
function parseYamlTranscript(path: string, text: string): GrokMessage[] {
  let raw: unknown
  try {
    raw = parseYaml(text)
  } catch (e) {
    throw new Error(`${path}: failed to parse YAML: ${(e as Error).message}`)
  }
  if (raw === null || raw === undefined) return []
  if (!Array.isArray(raw)) {
    throw new Error(`${path}: top-level must be a list, got ${typeof raw}`)
  }
  const context: GrokMessage[] = []
  for (let i = 0; i < raw.length; i++) {
    const entry = raw[i]
    if (entry === null || typeof entry !== "object" || Array.isArray(entry)) {
      throw new Error(`${path}: entry ${i} is not a mapping`)
    }
    const {role, message} = entry as {role?: unknown; message?: unknown}
    if (typeof role !== "string" || !ALLOWED_ROLES.has(role as GrokMessage["role"])) {
      throw new Error(`${path}: entry ${i} has invalid role: ${JSON.stringify(role)}`)
    }
    if (typeof message !== "string") {
      throw new Error(`${path}: entry ${i} has non-string message`)
    }
    if (PREPEND_ROLES.has(role as GrokMessage["role"])) {
      context.push({role: role as GrokMessage["role"], content: message})
    }
  }
  return context
}
