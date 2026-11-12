import {readFileSync, readdirSync} from "fs"
import {join} from "path"

type Replacement = [form: string, mark: string]

type Messages = {messages: {info: unknown; parts: unknown[]}[]}

const secretName = /^[A-Z][A-Z0-9_]{0,63}$/
const encodings: BufferEncoding[] = ["base64", "base64url"]
const seen = new Map<string, string>()

export default {
  id: "simplex-secrets",
  server: async () => ({
    "experimental.chat.messages.transform": async (_input: object, {messages}: Messages): Promise<void> => {
      const folder = process.env.SIMPLEX_MCP_SECRETS
      if (folder === undefined) throw new Error("SIMPLEX_MCP_SECRETS: the folder of the secrets is not set.")
      for (const [form, mark] of replacements(folder)) seen.set(form, mark)
      const forms = [...seen].sort(([a], [b]) => b.length - a.length)
      for (const message of messages) message.parts = message.parts.map(part => redacted(part, forms))
    },
  }),
}

export function replacements(folder: string): Replacement[] {
  const forms = readdirSync(folder)
    .filter(name => secretName.test(name))
    .flatMap(name => {
      const value = readFileSync(join(folder, name), "utf8").replace(/\n+$/, "")
      if ([...value].length < 8) throw new Error(`${name}: a value of 8 characters or more expected.`)
      return [value, encodeURIComponent(value), ...base64Forms(Buffer.from(value))].map((form): Replacement => [form, `[secret ${name}]`])
    })
  return forms.sort(([a], [b]) => b.length - a.length)
}

export function redacted(value: unknown, forms: Replacement[]): unknown {
  if (typeof value === "string") return forms.reduce((text, [form, mark]) => text.replaceAll(form, mark), value)
  if (Array.isArray(value)) return value.map(item => redacted(item, forms))
  if (typeof value === "object" && value !== null) return Object.fromEntries(Object.entries(value).map(([key, item]) => [key, redacted(item, forms)]))
  return value
}

function base64Forms(value: Buffer): string[] {
  return [0, 1, 2].flatMap(leading => {
    const bytes = Buffer.concat([Buffer.alloc(leading), value])
    return encodings.map(encoding => bytes.toString(encoding).slice(Math.ceil((leading * 4) / 3), Math.floor((bytes.length * 4) / 3)))
  })
}
