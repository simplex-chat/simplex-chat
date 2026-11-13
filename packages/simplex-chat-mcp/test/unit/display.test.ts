import {mkdtempSync, readFileSync, rmSync} from "fs"
import {Server} from "http"
import {builtinModules} from "module"
import {tmpdir} from "os"
import {join} from "path"
import {display} from "../../src/display"
import {serviceToken, socketPath} from "../../src/files"
import {listen, sendJson} from "../../src/socket"
import {closeServer} from "./fixtures"

let root: string
let server: Server | undefined
const input = {session_id: "s1", transcript_path: "/t.jsonl", cwd: "/p", prompt_id: "p1", hook_event_name: "MessageDisplay", turn_id: "t1", message_id: "m1", index: 2, final: false, delta: "Second line.\n"}
const env = (): NodeJS.ProcessEnv => ({SIMPLEX_MCP_HOME: root, CLAUDE_PID: "77"})

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-display-"))
})

afterEach(async () => {
  if (server !== undefined) await closeServer(server)
  server = undefined
  rmSync(root, {recursive: true, force: true})
})

describe("display hook", () => {
  it("posts each batch of Claude's text to the service with the session, the process and the batch", async () => {
    const bodies: unknown[] = []
    server = await listen(socketPath(root), serviceToken(root), {"POST /turn": async (body, res) => (bodies.push(body), sendJson(res, 200, {}))})
    await display(JSON.stringify(input), env(), 123.5)
    await display(JSON.stringify({...input, index: 3, final: true, delta: ""}), env(), 124.5)
    await display(JSON.stringify({...input, message_id: undefined}), env(), 125.5)
    await display("not json", env(), 126.5)
    expect(bodies).toEqual([
      {id: "s1", pid: 77, at: 123.5, text: {message: "m1", index: 2, final: false, delta: "Second line.\n"}},
      {id: "s1", pid: 77, at: 124.5, text: {message: "m1", index: 3, final: true, delta: ""}},
    ])
  })

  it("returns at once and starts no service when none runs, and after 1 s when the service does not answer", async () => {
    let started = Date.now()
    await display(JSON.stringify(input), env())
    serviceToken(root)
    await display(JSON.stringify(input), env())
    expect(Date.now() - started).toBeLessThan(500)
    const held: unknown[] = []
    server = await listen(socketPath(root), serviceToken(root), {"POST /turn": async body => void held.push(body)})
    started = Date.now()
    await display(JSON.stringify(input), env())
    expect(held).toHaveLength(1)
    expect(Date.now() - started).toBeGreaterThanOrEqual(1_000)
    expect(Date.now() - started).toBeLessThan(2_000)
  })

  it("loads only Node modules, paths.ts and service-request.ts", () => {
    const lean = ["display", "paths", "service-request"]
    for (const name of lean) {
      const source = readFileSync(join(__dirname, "../../src", `${name}.ts`), "utf8")
      const imports = [...source.matchAll(/^import .* from "([^"]+)"$/gm)].map(([, module]) => module)
      for (const module of imports) expect(builtinModules.includes(module) || lean.some(file => module === `./${file}`)).toBe(true)
    }
  })
})
