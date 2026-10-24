import {EventEmitter, once} from "events"
import {mkdtempSync, rmSync} from "fs"
import {Server, ServerResponse} from "http"
import {tmpdir} from "os"
import {join} from "path"
import {PassThrough} from "stream"
import {DaemonClient, SpawnDaemon} from "../../src/client"
import {serverToken, socketPath} from "../../src/files"
import {ExitError} from "../../src/log"
import {protocolVersion} from "../../src/protocol"
import {startSession} from "../../src/session"
import {SocketError, closeServer, listen, sendJson, startLines} from "../../src/socket"
import {LineClient} from "./fixtures"

const id = "11111111-1111-4111-8111-111111111111"
const unknownSession = "Unknown session; restart the Claude Code session."
let root: string
let server: Server | undefined
const seen: string[] = []
const daemon = new EventEmitter()

type OnCall = (res: ServerResponse, n: number) => Promise<void>

async function fakeDaemon(onCall: OnCall): Promise<void> {
  let calls = 0
  server = await listen(socketPath(root), serverToken(root), {
    "GET /status": async (_b, res) => sendJson(res, 200, {version: "0.2.0", protocol: protocolVersion, mainGroup: "SimpleX MCP", owner: {owners: ["Alice"]}, sessions: []}),
    "POST /session": async (body, res) => {
      seen.push(`session ${JSON.stringify(body)}`)
      sendJson(res, 200, {number: 1, displayName: "SimpleX MCP - app", version: "0.2.0", protocol: protocolVersion, owner: {owners: ["Alice"]}})
    },
    "POST /attach": async (_b, res) => {
      seen.push("attach")
      startLines(res)({version: "0.2.0", protocol: protocolVersion})
      daemon.emit("attach", res)
    },
    "POST /call": async (body, res) => {
      seen.push(`call ${(body as {tool: string}).tool}`)
      daemon.emit("call", res)
      await onCall(res, ++calls)
    },
  })
}

function endWithResult(res: ServerResponse, result: unknown): void {
  res.end(`${JSON.stringify({result})}\n`)
}

const noSpawn: SpawnDaemon = async () => undefined

async function session(env: NodeJS.ProcessEnv = {}, spawnDaemon: SpawnDaemon = noSpawn): Promise<{client: LineClient; stdin: PassThrough}> {
  const stdin = new PassThrough()
  const stdout = new PassThrough()
  await startSession({CLAUDE_CODE_SESSION_ID: id, CLAUDE_PROJECT_DIR: "/p/app", SIMPLEX_MCP_HOME: root, ...env}, {stdin, stdout, spawnDaemon, version: "0.2.0"})
  const client = new LineClient(stdin, stdout)
  await client.initialize()
  return {client, stdin}
}

function listCall(client: LineClient): Promise<any> {
  return client.request("tools/call", {name: "list", arguments: {}})
}

const listed = {chats: [], actions: []}
const listResult = {content: [{type: "text", text: JSON.stringify(listed)}], structuredContent: listed}
const registered = `session ${JSON.stringify({id, project: "/p/app", config: {mainGroup: "SimpleX MCP", approve: "requests", expireHours: 23}})}`

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-session-"))
  seen.length = 0
  DaemonClient.forRoot(root)
})

afterEach(async () => {
  if (server) await closeServer(server)
  server = undefined
  daemon.removeAllListeners()
  rmSync(root, {recursive: true, force: true})
})

describe("session server", () => {
  it("refuses to run outside the plugin", async () => {
    const io = {stdin: new PassThrough(), stdout: new PassThrough(), spawnDaemon: noSpawn, version: "0.2.0"}
    await expect(startSession({SIMPLEX_MCP_HOME: root}, io)).rejects.toEqual(new ExitError("simplex-mcp session runs from the Claude Code plugin.", 2))
    await expect(startSession({SIMPLEX_MCP_HOME: root, CLAUDE_CODE_SESSION_ID: "x"}, io)).rejects.toEqual(new ExitError("Not a Claude Code session ID: x", 2))
  })

  it("answers every call with an invalid plugin option, without the daemon", async () => {
    await fakeDaemon(async res => endWithResult(res, listResult))
    const {client} = await session({CLAUDE_PLUGIN_OPTION_APPROVE: "x"})
    const error = {content: [{type: "text", text: "Plugin option approve: requests or host expected, got x. Change it in /plugin > Installed > simplex > Configure options."}], isError: true}
    expect((await listCall(client)).result).toEqual(error)
    expect((await client.request("tools/call", {name: "receive", arguments: {chat: "@1"}})).result).toEqual(error)
    expect(seen).toEqual([])
  })

  it("lists tools while the daemon is still starting", async () => {
    let spawned!: () => void
    const spawning = new Promise<void>(resolve => (spawned = resolve))
    const pending = new Promise<void>(() => undefined)
    const {client} = await session({}, () => (spawned(), pending))
    await spawning
    const {result} = await client.request("tools/list")
    expect(result.tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "list", "leave"])
  })

  it("registers, attaches and forwards a call with progress", async () => {
    await fakeDaemon(async res => {
      startLines(res)({progress: "Action 1 waits for an approver until 10:00 UTC."})
      endWithResult(res, listResult)
    })
    const {client} = await session()
    const reply = await client.request("tools/call", {name: "list", arguments: {}, _meta: {progressToken: 7}})
    expect(reply.result).toEqual(listResult)
    expect(client.notifications).toContainEqual(
      expect.objectContaining({method: "notifications/progress", params: expect.objectContaining({progressToken: 7, message: "Action 1 waits for an approver until 10:00 UTC."})}),
    )
    expect(seen).toEqual([registered, "attach", "call list"])
  })

  it("uses the working folder as the project without CLAUDE_PROJECT_DIR", async () => {
    await fakeDaemon(async res => endWithResult(res, listResult))
    const {client} = await session({CLAUDE_PROJECT_DIR: ""})
    await listCall(client)
    expect(JSON.parse(seen[0].slice("session ".length)).project).toBe(process.cwd())
  })

  it("returns the cancelled result when the daemon reports a cancelled call", async () => {
    await fakeDaemon(async res => endWithResult(res, null))
    const {client} = await session()
    expect((await listCall(client)).result).toEqual({content: [{type: "text", text: "Cancelled."}], isError: true})
  })

  it("reports a daemon that stopped during a call, and reconnects for the next one", async () => {
    await fakeDaemon(async (res, n) => {
      if (n === 1) res.destroy()
      else endWithResult(res, listResult)
    })
    const {client} = await session()
    expect((await listCall(client)).result).toEqual({content: [{type: "text", text: "The SimpleX daemon stopped; repeat the call."}], isError: true})
    expect((await listCall(client)).result).toEqual(listResult)
    expect(seen).toEqual([registered, "attach", "call list", registered, "attach", "call list"])
  })

  it("reports a call stream that ends without a result as a stopped daemon", async () => {
    await fakeDaemon(async res => {
      startLines(res)({progress: "waiting"})
      res.end()
    })
    const {client} = await session()
    expect((await listCall(client)).result).toEqual({content: [{type: "text", text: "The SimpleX daemon stopped; repeat the call."}], isError: true})
  })

  it("returns a daemon error as the tool error, and registers again for the next call", async () => {
    await fakeDaemon(async (res, n) => {
      if (n === 1) throw new SocketError(unknownSession, 409)
      endWithResult(res, listResult)
    })
    const {client} = await session()
    expect((await listCall(client)).result).toEqual({content: [{type: "text", text: unknownSession}], isError: true})
    expect((await listCall(client)).result).toEqual(listResult)
    expect(seen).toEqual([registered, "attach", "call list", registered, "attach", "call list"])
  })

  it("logs a failed background start and returns the error to the next call", async () => {
    const errors: string[] = []
    const stderr = jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text).trimEnd()), true))
    try {
      const {client} = await session({}, async () => {
        throw new ExitError("The SimpleX daemon did not start in 5 minutes.", 1)
      })
      expect((await listCall(client)).result).toEqual({content: [{type: "text", text: "The SimpleX daemon did not start in 5 minutes."}], isError: true})
      expect(errors).toEqual(["SimpleX daemon: The SimpleX daemon did not start in 5 minutes."])
    } finally {
      stderr.mockRestore()
    }
  })

  it("aborts the forwarded call when the client cancels", async () => {
    await fakeDaemon(async res => {
      startLines(res)({progress: "waiting"})
    })
    const {client} = await session()
    const started = once(daemon, "call")
    client.request("tools/call", {name: "receive", arguments: {chat: "@1", wait: 60}})
    const [res] = (await started) as [ServerResponse]
    const closed = once(res, "close")
    client.notify("notifications/cancelled", {requestId: 2})
    await closed
  })

  it("releases /attach when stdin closes", async () => {
    await fakeDaemon(async res => endWithResult(res, listResult))
    const attached = once(daemon, "attach")
    const {client, stdin} = await session()
    const [res] = (await attached) as [ServerResponse]
    await listCall(client)
    const closed = once(res, "close")
    stdin.end()
    await closed
  })
})
