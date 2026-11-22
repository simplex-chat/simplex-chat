import {appendFileSync, mkdtempSync, rmSync, utimesSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {AccountReader, TranscriptReader, lastUsage} from "../../src/claude-code"

let folder: string

beforeEach(() => {
  folder = mkdtempSync(join(tmpdir(), "simplex-mcp-claude-code-"))
})

afterEach(() => rmSync(folder, {recursive: true, force: true}))

const goal = (attachment: object): string => `${JSON.stringify({type: "attachment", attachment: {type: "goal_status", condition: "ship it", ...attachment}})}\n`
const recap = (content: string): string => `${JSON.stringify({type: "system", subtype: "away_summary", content})}\n`

describe("Claude Code account", () => {
  it("is the email of the global config's OAuth account, read again after a write, and kept while the file is partly written", async () => {
    const config = join(folder, ".claude.json")
    const reader = new AccountReader()
    writeFileSync(config, JSON.stringify({oauthAccount: {emailAddress: "alice@example.com"}}))
    const first = await reader.read(config)
    writeFileSync(config, '{"oauthAccount": {"emailAd')
    utimesSync(config, new Date(), new Date(Date.now() + 1_000))
    const partial = await reader.read(config)
    writeFileSync(config, JSON.stringify({oauthAccount: {emailAddress: "bob@example.com"}}))
    utimesSync(config, new Date(), new Date(Date.now() + 2_000))
    expect([first, partial, await reader.read(config)]).toEqual(["alice@example.com", "alice@example.com", "bob@example.com"])
  })
})

describe("Claude Code agent transcript", () => {
  it("gives the tokens of the last request, its input, cache and output tokens, without a synthetic entry or a partly written line", async () => {
    const transcript = join(folder, "agent-a1.jsonl")
    const assistant = (model: string, usage: object): string => `${JSON.stringify({type: "assistant", message: {model, usage}})}\n`
    writeFileSync(transcript, `${assistant("claude-opus-5", {input_tokens: 2, cache_creation_input_tokens: 700, cache_read_input_tokens: 110_000, output_tokens: 1_783})}${assistant("<synthetic>", {input_tokens: 0, output_tokens: 0})}{"type":"assistant","message":{"usage":`)
    expect(await lastUsage(transcript)).toBe(112_485)
  })
})

describe("Claude Code transcript", () => {
  it("gives the goal state of the last goal entry: set, met, failed, and none after a clear", async () => {
    const transcript = join(folder, "t.jsonl")
    const reader = new TranscriptReader()
    const states: unknown[] = []
    for (const entry of [{met: false, sentinel: true}, {met: true, reason: "done"}, {met: false, failed: true}, {met: true, sentinel: true}]) {
      appendFileSync(transcript, goal(entry))
      states.push((await reader.read(transcript)).goal)
    }
    expect(states).toEqual(["set", "met", "failed", undefined])
  })

  it("reads the appended lines only, takes the recap without the hint and the title, and clears them at a new path", async () => {
    const transcript = join(folder, "t.jsonl")
    const reader = new TranscriptReader()
    writeFileSync(transcript, recap("Built the board. Next, you review it. (disable recaps in /config)"))
    const first = await reader.read(transcript)
    writeFileSync(transcript, goal({met: false}), {flag: "r+"})
    appendFileSync(transcript, `${JSON.stringify({type: "ai-title", aiTitle: "Board"})}\n${recap("Tests pass.")}`)
    const appended = await reader.read(transcript)
    writeFileSync(join(folder, "u.jsonl"), "")
    expect([first, appended, await reader.read(join(folder, "u.jsonl"))]).toEqual([{recap: "Built the board. Next, you review it."}, {recap: "Tests pass.", title: "Board"}, {}])
  })
})
