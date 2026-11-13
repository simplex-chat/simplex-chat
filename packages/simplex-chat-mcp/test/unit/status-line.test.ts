import {mkdtempSync, readFileSync, rmSync} from "fs"
import {Server} from "http"
import {builtinModules} from "module"
import {tmpdir} from "os"
import {join} from "path"
import {serviceToken, socketPath} from "../../src/files"
import {listen, sendJson} from "../../src/socket"
import {statusLine} from "../../src/status-line"
import {closeServer} from "./fixtures"

let root: string
let server: Server | undefined
const fiveHour = {used_percentage: 34.7, resets_at: 1_794_600_000}
const sevenDay = {used_percentage: 61, resets_at: 1_795_000_000}
const input = {
  session_id: "s1",
  transcript_path: "/t.jsonl",
  cwd: "/p",
  model: {id: "claude-opus-5-5[1m]", display_name: "Opus 5.5"},
  effort: {level: "xhigh"},
  cost: {total_cost_usd: 12.5},
  context_window: {total_input_tokens: 670_000, context_window_size: 1_000_000, used_percentage: 67, remaining_percentage: 33},
  rate_limits: {five_hour: fiveHour, seven_day: sevenDay},
}
const env = (): NodeJS.ProcessEnv => ({SIMPLEX_MCP_HOME: root, CLAUDE_PID: "77"})

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-status-line-"))
})

afterEach(async () => {
  if (server !== undefined) await closeServer(server)
  server = undefined
  rmSync(root, {recursive: true, force: true})
})

describe("status line command", () => {
  it("prints the model, effort, context and the plan's usage of a status line input, and sends the effort and the limits with the session ID and Claude Code's process", async () => {
    const bodies: unknown[] = []
    server = await listen(socketPath(root), serviceToken(root), {"POST /turn": async (body, res) => (bodies.push(body), sendJson(res, 200, {}))})
    const lines: string[] = []
    await statusLine(JSON.stringify(input), line => lines.push(line), env(), 12.5)
    await statusLine(JSON.stringify({...input, rate_limits: {seven_day: sevenDay}, effort: undefined}), line => lines.push(line), env(), 13.5)
    expect(lines).toEqual(["Opus 5.5 · effort xhigh · 67% context · 5 h: 34% · week: 61%", "Opus 5.5 · 67% context · week: 61%"])
    expect(bodies).toEqual([
      {id: "s1", pid: 77, at: 12.5, effort: "xhigh", limits: {fiveHour: {percent: 34.7, resetsAt: 1_794_600_000}, week: {percent: 61, resetsAt: 1_795_000_000}}},
      {id: "s1", pid: 77, at: 13.5, limits: {week: {percent: 61, resetsAt: 1_795_000_000}}},
    ])
  })

  it("prints its line without a service and without rate limits, and sends nothing without an effort level and limits", async () => {
    const lines: string[] = []
    const started = Date.now()
    await statusLine(JSON.stringify({...input, rate_limits: undefined}), line => lines.push(line), env())
    expect(Date.now() - started).toBeLessThan(500)
    const bodies: unknown[] = []
    server = await listen(socketPath(root), serviceToken(root), {"POST /turn": async (body, res) => (bodies.push(body), sendJson(res, 200, {}))})
    await statusLine(JSON.stringify({...input, rate_limits: undefined}), line => lines.push(line), env(), 14.5)
    await statusLine(JSON.stringify({session_id: "s1", model: {display_name: "Haiku 5.5"}, context_window: {used_percentage: null}}), line => lines.push(line), env())
    await statusLine("not json", line => lines.push(line), env())
    expect(lines).toEqual(["Opus 5.5 · effort xhigh · 67% context", "Opus 5.5 · effort xhigh · 67% context", "Haiku 5.5", ""])
    expect(bodies).toEqual([{id: "s1", pid: 77, at: 14.5, effort: "xhigh"}])
  })

  it("loads only Node modules, paths.ts and service-request.ts", () => {
    const source = readFileSync(join(__dirname, "../../src/status-line.ts"), "utf8")
    const imports = [...source.matchAll(/^import .* from "([^"]+)"$/gm)].map(([, module]) => module)
    expect(imports.filter(module => !builtinModules.includes(module))).toEqual(["./paths", "./service-request"])
  })
})
