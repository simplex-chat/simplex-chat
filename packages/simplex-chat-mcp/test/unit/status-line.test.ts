import {mkdtempSync, readFileSync, rmSync, statfsSync} from "fs"
import {Server} from "http"
import {builtinModules} from "module"
import {tmpdir, totalmem} from "os"
import {join} from "path"
import {serviceToken, socketPath} from "../../src/files"
import {listen, sendJson} from "../../src/socket"
import {Machine, machineParts, machineState, statusLine} from "../../src/status-line"
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
const roots: string[] = []
const machine = (folder: string): Machine => (roots.push(folder), {memoryFree: 12_400_000_000, memoryTotal: 32_000_000_000, diskFree: 7_380_000_000})
const free = " · RAM 12 GB free · disk 7.3 GB free"

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-status-line-"))
})

afterEach(async () => {
  if (server !== undefined) await closeServer(server)
  server = undefined
  rmSync(root, {recursive: true, force: true})
})

describe("status line command", () => {
  it("prints the model, effort, context, the plan's usage and the machine's free memory and disk of the data folder, and sends the effort and the limits with the session ID and Claude Code's process, without the machine's state", async () => {
    const bodies: unknown[] = []
    server = await listen(socketPath(root), serviceToken(root), {"POST /turn": async (body, res) => (bodies.push(body), sendJson(res, 200, {}))})
    const lines: string[] = []
    roots.length = 0
    await statusLine(JSON.stringify(input), line => lines.push(line), env(), 12.5, machine)
    await statusLine(JSON.stringify({...input, rate_limits: {seven_day: sevenDay}, effort: undefined}), line => lines.push(line), env(), 13.5, machine)
    expect(lines).toEqual([`Opus 5.5 · effort xhigh · 67% context · 5 h: 34% · week: 61%${free}`, `Opus 5.5 · 67% context · week: 61%${free}`])
    expect(roots).toEqual([root, root])
    expect(bodies).toEqual([
      {id: "s1", pid: 77, at: 12.5, effort: "xhigh", limits: {fiveHour: {percent: 34.7, resetsAt: 1_794_600_000}, week: {percent: 61, resetsAt: 1_795_000_000}}, statusLine: true},
      {id: "s1", pid: 77, at: 13.5, limits: {week: {percent: 61, resetsAt: 1_795_000_000}}, statusLine: true},
    ])
  })

  it("prints its line without a service and without rate limits, and sends nothing without an effort level and limits", async () => {
    const lines: string[] = []
    const started = Date.now()
    await statusLine(JSON.stringify({...input, rate_limits: undefined}), line => lines.push(line), env(), undefined, machine)
    expect(Date.now() - started).toBeLessThan(500)
    const bodies: unknown[] = []
    server = await listen(socketPath(root), serviceToken(root), {"POST /turn": async (body, res) => (bodies.push(body), sendJson(res, 200, {}))})
    await statusLine(JSON.stringify({...input, rate_limits: undefined}), line => lines.push(line), env(), 14.5, machine)
    await statusLine(JSON.stringify({session_id: "s1", model: {display_name: "Haiku 5.5"}, context_window: {used_percentage: null}}), line => lines.push(line), env(), undefined, machine)
    await statusLine("not json", line => lines.push(line), env(), undefined, machine)
    expect(lines).toEqual([`Opus 5.5 · effort xhigh · 67% context${free}`, `Opus 5.5 · effort xhigh · 67% context${free}`, `Haiku 5.5${free}`, free.slice(3)])
    expect(bodies).toEqual([{id: "s1", pid: 77, at: 14.5, effort: "xhigh", statusLine: true}])
  })

  it("shows free memory and disk in TB, GB with one decimal under 10 or MB, rounded down, with ❗️ after a part under 5% of the memory or under 5 GB of disk, and no disk part when it is unknown", () => {
    expect(machineParts({memoryFree: 12_400_000_000, memoryTotal: 32_000_000_000, diskFree: 1_259_000_000_000})).toEqual(["RAM 12 GB free", "disk 1.2 TB free"])
    expect(machineParts({memoryFree: 1_600_000_000, memoryTotal: 32_000_000_000, diskFree: 5_000_000_000})).toEqual(["RAM 1.6 GB free", "disk 5.0 GB free"])
    expect(machineParts({memoryFree: 1_599_000_000, memoryTotal: 32_000_000_000, diskFree: 4_999_000_000})).toEqual(["RAM 1.5 GB free❗️", "disk 4.9 GB free❗️"])
    expect(machineParts({memoryFree: 790_700_000, memoryTotal: 16_000_000_000, diskFree: 45_600_000_000_000})).toEqual(["RAM 790 MB free❗️", "disk 45 TB free"])
    expect(machineParts({memoryFree: 9_900_000_000, memoryTotal: 16_000_000_000})).toEqual(["RAM 9.9 GB free"])
    expect(machineParts(undefined)).toEqual([])
  })

  it("reads the available memory of the process's limits and the free disk of the data folder, or of its nearest existing parent", () => {
    const {bavail, bsize} = statfsSync(root)
    for (const folder of [root, join(root, "missing", "folder")]) {
      const {memoryFree, memoryTotal, diskFree} = machineState(folder)
      expect(memoryFree).toBeGreaterThan(0)
      expect(memoryFree).toBeLessThanOrEqual(memoryTotal)
      expect(memoryTotal).toBeLessThanOrEqual(totalmem())
      expect(Math.abs(diskFree! - bavail * bsize)).toBeLessThan(1_000_000_000)
    }
  })

  it("loads only Node modules, paths.ts and service-request.ts", () => {
    const source = readFileSync(join(__dirname, "../../src/status-line.ts"), "utf8")
    const imports = [...source.matchAll(/^import .* from "([^"]+)"$/gm)].map(([, module]) => module)
    expect(imports.filter(module => !builtinModules.includes(module))).toEqual(["./paths", "./service-request"])
  })
})
