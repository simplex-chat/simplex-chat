import {mkdtempSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import plugin, {redacted, replacements} from "../../opencode/simplex-secrets"
import {Secrets} from "../../src/files"

const value = "tok en/+?=é-1234"
const mark = "[secret API_TOKEN]"
let folder: string

beforeEach(() => {
  folder = mkdtempSync(join(tmpdir(), "simplex-mcp-opencode-"))
  writeFileSync(join(folder, "API_TOKEN"), `${value}\n`)
  delete process.env.SIMPLEX_MCP_SECRETS
})

afterEach(() => rmSync(folder, {recursive: true, force: true}))

describe("replacements", () => {
  it("replace a value and its URL encoding", () => {
    const forms = replacements(folder)
    expect(redacted(`curl -H "X-Token: ${value}"`, forms)).toBe(`curl -H "X-Token: ${mark}"`)
    expect(redacted(`https://example.com/?k=${encodeURIComponent(value)}&x=1`, forms)).toBe(`https://example.com/?k=${mark}&x=1`)
  })

  it("replace the base64 and base64url characters that hold only bits of the value, after 0, 1 and 2 leading bytes", () => {
    const forms = replacements(folder)
    const length = Buffer.byteLength(value)
    for (const prefix of ["", "a", "ab", "abc", "user:"]) {
      for (const encoding of ["base64", "base64url"] as const) {
        const encoded = Buffer.from(`${prefix}${value}!`).toString(encoding)
        const leading = Buffer.byteLength(prefix)
        const first = Math.ceil((leading * 8) / 6)
        const end = Math.floor(((leading + length) * 8) / 6)
        expect(redacted(encoded, forms)).toBe(`${encoded.slice(0, first)}${mark}${encoded.slice(end)}`)
      }
    }
    expect(redacted(`Authorization: Basic ${Buffer.from(`user:${value}`).toString("base64")}`, forms)).toMatch(/^Authorization: Basic dXNlcj.\[secret API_TOKEN\].{0,3}$/)
  })

  it("replace a longer value before a value it contains", () => {
    writeFileSync(join(folder, "LONG_TOKEN"), `prefix-${value}-suffix`)
    expect(redacted(`a prefix-${value}-suffix b ${value}`, replacements(folder))).toBe(`a [secret LONG_TOKEN] b ${mark}`)
  })

  it("read the files of the names that the server lists, without trailing newlines", () => {
    for (const name of ["GIT_TOKEN", "API_TOKEN.part", "api_token", ".API_TOKEN", "1_TOKEN", `A${"B".repeat(63)}`, `A${"B".repeat(64)}`]) writeFileSync(join(folder, name), `${name} value`)
    const forms = replacements(folder)
    expect(forms).toContainEqual([value, mark])
    expect([...new Set(forms.map(([, formMark]) => formMark))].sort()).toEqual(new Secrets(folder, join(folder, "index.json")).names().map(name => `[secret ${name}]`))
  })

  it("refuse a value of fewer than 8 characters, as the server does", () => {
    for (const [name, content] of [["EMPTY_TOKEN", "\n"], ["ONE_TOKEN", "a"], ["SHORT_TOKEN", "1234567\n"]]) {
      writeFileSync(join(folder, name), content)
      expect(() => replacements(folder)).toThrow(`${name}: a value of 8 characters or more expected.`)
      rmSync(join(folder, name))
    }
    writeFileSync(join(folder, "SHORT_TOKEN"), "12345678")
    expect(replacements(folder)).toContainEqual(["12345678", "[secret SHORT_TOKEN]"])
  })
})

describe("hook", () => {
  it("redacts the parts of every message and keeps their info", async () => {
    process.env.SIMPLEX_MCP_SECRETS = folder
    const info = {id: "msg_1", role: "assistant", summary: value}
    const messages = [
      {
        info,
        parts: [
          {type: "text", text: `token ${value}`},
          {type: "tool", tool: "bash", state: {status: "completed", input: {command: `curl -H "X-Token: ${value}"`}, output: `< x-token: ${value}`, metadata: {output: [value], exit: 0}}},
          {type: "tool", tool: "bash", state: {status: "error", input: {}, error: `bad token ${value}`}},
        ],
      },
    ]
    await (await plugin.server())["experimental.chat.messages.transform"]({}, {messages})
    expect(messages).toEqual([
      {
        info: {id: "msg_1", role: "assistant", summary: value},
        parts: [
          {type: "text", text: `token ${mark}`},
          {type: "tool", tool: "bash", state: {status: "completed", input: {command: `curl -H "X-Token: ${mark}"`}, output: `< x-token: ${mark}`, metadata: {output: [mark], exit: 0}}},
          {type: "tool", tool: "bash", state: {status: "error", input: {}, error: `bad token ${mark}`}},
        ],
      },
    ])
    expect(messages[0].info).toBe(info)
  })

  it("keeps replacing the values of removed and replaced secrets, also in a new server instance", async () => {
    process.env.SIMPLEX_MCP_SECRETS = folder
    const transform = async (messages: {info: unknown; parts: unknown[]}[]): Promise<void> => (await plugin.server())["experimental.chat.messages.transform"]({}, {messages})
    writeFileSync(join(folder, "GIT_TOKEN"), "git token value")
    await transform([])
    rmSync(join(folder, "GIT_TOKEN"))
    writeFileSync(join(folder, "API_TOKEN"), "new api token value")
    const messages = [{info: {}, parts: [{type: "text", text: `${value}, git token value, new api token value`}]}]
    await transform(messages)
    expect(messages[0].parts).toEqual([{type: "text", text: `${mark}, [secret GIT_TOKEN], ${mark}`}])
  })

  it("rejects without the folder of the secrets", async () => {
    const transform = (await plugin.server())["experimental.chat.messages.transform"]
    await expect(transform({}, {messages: []})).rejects.toThrow("SIMPLEX_MCP_SECRETS: the folder of the secrets is not set.")
    process.env.SIMPLEX_MCP_SECRETS = join(folder, "missing")
    await expect(transform({}, {messages: []})).rejects.toThrow("ENOENT")
  })

  it("is a server plugin with an id", () => {
    expect(plugin.id).toBe("simplex-secrets")
  })
})
