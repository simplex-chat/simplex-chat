import {EventEmitter, once} from "events"
import {mkdtempSync, rmSync} from "fs"
import {Server, ServerResponse} from "http"
import {tmpdir} from "os"
import {join} from "path"
import {PassThrough} from "stream"
import {ServiceClient, SpawnService} from "../../src/client"
import {serviceToken, socketPath} from "../../src/files"
import {ExitError} from "../../src/log"
import {protocolVersion} from "../../src/protocol"
import {startSession} from "../../src/session"
import {SocketError, listen, sendJson, startLines} from "../../src/socket"
import {LineClient, closeServer, recordedRun, until} from "./fixtures"

const id = "11111111-1111-4111-8111-111111111111"
const unknownSession = "Unknown session; restart the Claude Code session."
const stoppedResult = {content: [{type: "text", text: "The SimpleX service stopped; repeat the call."}], isError: true}
let root: string
let server: Server | undefined
const seen: string[] = []
let statuses = 0
const service = new EventEmitter()

type OnCall = (res: ServerResponse, n: number) => Promise<void>

const attachLine = (res: ServerResponse): void => startLines(res)({version: "0.2.0", protocol: protocolVersion})

async function fakeService(onCall: OnCall, onAttach: (res: ServerResponse) => void = attachLine, protocol = protocolVersion, sessionRefusal?: SocketError): Promise<void> {
  let calls = 0
  server = await listen(socketPath(root), serviceToken(root), {
    "GET /status": async (_b, res) => {
      statuses++
      sendJson(res, 200, {version: "0.2.0", protocol, mainGroup: "SimpleX MCP", owner: {owners: ["Alice"]}, sessions: []})
    },
    "POST /session": async (body, res) => {
      seen.push(`session ${JSON.stringify(body)}`)
      if (sessionRefusal !== undefined) throw sessionRefusal
      sendJson(res, 200, {number: 1, displayName: "SimpleX MCP - app", version: "0.2.0", protocol: protocolVersion, owner: {owners: ["Alice"]}})
    },
    "POST /attach": async (_b, res) => {
      seen.push("attach")
      onAttach(res)
      service.emit("attach", res)
    },
    "POST /call": async (body, res) => {
      seen.push(`call ${(body as {tool: string}).tool}`)
      service.emit("call", res)
      await onCall(res, ++calls)
    },
    "POST /permission": async (body, res) => {
      seen.push(`permission ${JSON.stringify(body)}`)
      sendJson(res, 200, {})
    },
  })
}

function endWithResult(res: ServerResponse, result: unknown): void {
  res.end(`${JSON.stringify({result})}\n`)
}

function next(event: "attach" | "call", count = 1): Promise<ServerResponse[]> {
  const received: ServerResponse[] = []
  return new Promise(resolve => {
    const onEvent = (res: ServerResponse): void => {
      received.push(res)
      if (received.length < count) return
      service.off(event, onEvent)
      resolve(received)
    }
    service.on(event, onEvent)
  })
}

function captureStderr(): string[] {
  const errors: string[] = []
  jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text).trimEnd()), true))
  return errors
}

const noSpawn: SpawnService = async () => undefined

async function session(env: NodeJS.ProcessEnv = {}, spawnService: SpawnService = noSpawn): Promise<{client: LineClient; stdin: PassThrough}> {
  const stdin = new PassThrough()
  const stdout = new PassThrough()
  stdins.push(stdin)
  await startSession({CLAUDE_CODE_SESSION_ID: id, CLAUDE_PROJECT_DIR: "/p/app", SIMPLEX_MCP_HOME: root, ...env}, {stdin, stdout, spawnService, version: "0.2.0"})
  const client = new LineClient(stdin, stdout)
  await client.initialize()
  return {client, stdin}
}

function listCall(client: LineClient): Promise<any> {
  return client.request("tools/call", {name: "list", arguments: {}})
}

const listed = {chats: [], actions: []}
const listResult = {content: [{type: "text", text: JSON.stringify(listed)}], structuredContent: listed}
const registered = `session ${JSON.stringify({id, pid: process.ppid, project: "/p/app", config: {mainGroup: "SimpleX MCP", approve: "requests", expireHours: 23}})}`
const stdins: PassThrough[] = []

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-session-"))
  seen.length = 0
  statuses = 0
  ServiceClient.forRoot(root)
})

afterEach(async () => {
  for (const stdin of stdins.splice(0)) stdin.end()
  await new Promise(resolve => setImmediate(resolve))
  if (server) await closeServer(server)
  server = undefined
  service.removeAllListeners()
  jest.restoreAllMocks()
  rmSync(root, {recursive: true, force: true})
})

describe("session server", () => {
  it("refuses to run outside the plugin", async () => {
    const io = {stdin: new PassThrough(), stdout: new PassThrough(), spawnService: noSpawn, version: "0.2.0"}
    await expect(startSession({SIMPLEX_MCP_HOME: root}, io)).rejects.toEqual(new ExitError("simplex-mcp session runs from the Claude Code plugin.", 2))
    await expect(startSession({SIMPLEX_MCP_HOME: root, CLAUDE_CODE_SESSION_ID: "x"}, io)).rejects.toEqual(new ExitError("Not a Claude Code session ID: x", 2))
  })

  it("answers every call with an invalid plugin option, without the service", async () => {
    await fakeService(async res => endWithResult(res, listResult))
    const {client} = await session({CLAUDE_PLUGIN_OPTION_APPROVE: "x"})
    const error = {content: [{type: "text", text: "Plugin option approve: requests or host expected, got x. Change it in /plugin > Installed > simplex > Configure options."}], isError: true}
    expect((await listCall(client)).result).toEqual(error)
    expect((await client.request("tools/call", {name: "receive", arguments: {chat: "@1"}})).result).toEqual(error)
    expect(seen).toEqual([])
  })

  it("answers every call of a non-interactive run, as with claude -p, without the service", async () => {
    await fakeService(async res => endWithResult(res, listResult))
    const spawn = jest.fn(noSpawn)
    const {client} = await session({CLAUDE_CODE_ENTRYPOINT: "sdk-cli"}, spawn)
    const refused = {content: [{type: "text", text: "This Claude Code run is not interactive, so it has no SimpleX group; the SimpleX tools work in interactive sessions."}], isError: true}
    expect((await listCall(client)).result).toEqual(refused)
    await new Promise(resolve => setTimeout(resolve, 100))
    expect(seen).toEqual([])
    expect(statuses).toBe(0)
    expect(spawn).not.toHaveBeenCalled()
  })

  it("relays no permission request without the service in a non-interactive run or with an invalid plugin option", async () => {
    await fakeService(async res => endWithResult(res, listResult))
    const spawn = jest.fn(noSpawn)
    for (const env of [{CLAUDE_CODE_ENTRYPOINT: "sdk-cli"}, {CLAUDE_PLUGIN_OPTION_APPROVE: "x"}]) {
      const {client} = await session(env, spawn)
      client.notify("notifications/claude/channel/permission_request", {request_id: "abcde", tool_name: "Bash", description: "Run shell command", input_preview: "ls"})
      expect((await listCall(client)).result.isError).toBe(true)
    }
    await new Promise(resolve => setTimeout(resolve, 100))
    expect(seen).toEqual([])
    expect(statuses).toBe(0)
    expect(spawn).not.toHaveBeenCalled()
  })

  it("answers every call without the service in a run whose environment marks it unattended, and in claude -p under the mark of the attended session that started it", async () => {
    await fakeService(async res => endWithResult(res, listResult))
    const spawn = jest.fn(noSpawn)
    const refused = {content: [{type: "text", text: "This Claude Code run is not interactive, so it has no SimpleX group; the SimpleX tools work in interactive sessions."}], isError: true}
    for (const env of [{CLAUDE_CODE_ENTRYPOINT: "claude-vscode", CLAUDE_CODE_SESSION_ATTENDED: "0"}, {CLAUDE_CODE_ENTRYPOINT: "sdk-cli", CLAUDE_CODE_SESSION_ATTENDED: "1"}]) {
      const {client} = await session(env, spawn)
      expect((await listCall(client)).result).toEqual(refused)
    }
    await new Promise(resolve => setTimeout(resolve, 100))
    expect(seen).toEqual([])
    expect(spawn).not.toHaveBeenCalled()
  })

  it("answers every call with the text of a run that is not interactive, and registers no more, after the service refuses its process as marked unattended, as the server of a claude -p run started by a command of a VS Code session, with the session's environment", async () => {
    await fakeService(async res => endWithResult(res, listResult), attachLine, protocolVersion, new SocketError("marked unattended", 403))
    const errors = captureStderr()
    const {env} = recordedRun("claude-vscode", "mcp")
    const {client} = await session(env)
    const refused = {content: [{type: "text", text: "This Claude Code run is not interactive, so it has no SimpleX group; the SimpleX tools work in interactive sessions."}], isError: true}
    await until(() => seen.length === 1)
    expect((await listCall(client)).result).toEqual(refused)
    expect((await client.request("tools/call", {name: "receive", arguments: {chat: "@1"}})).result).toEqual(refused)
    client.notify("notifications/claude/channel/permission_request", {request_id: "abcde", tool_name: "Bash", description: "Run shell command", input_preview: "ls"})
    await new Promise(resolve => setTimeout(resolve, 1_500))
    expect(seen).toEqual([registered])
    expect(errors).toEqual([])
  })

  it("answers server/discover with Method not found and negotiates the revision 2025-11-25, as Claude Code 2.1.319 registers a channel only outside the modern MCP era", async () => {
    await fakeService(async res => endWithResult(res, listResult))
    const stdin = new PassThrough()
    const stdout = new PassThrough()
    stdins.push(stdin)
    await startSession({CLAUDE_CODE_SESSION_ID: id, CLAUDE_PROJECT_DIR: "/p/app", SIMPLEX_MCP_HOME: root}, {stdin, stdout, spawnService: noSpawn, version: "0.2.0"})
    const client = new LineClient(stdin, stdout)
    const clientInfo = {name: "claude-code", title: "Claude Code", version: "2.1.319"}
    const discovered = await client.request("server/discover", {_meta: {"io.modelcontextprotocol/protocolVersion": "2026-07-28", "io.modelcontextprotocol/clientInfo": clientInfo, "io.modelcontextprotocol/clientCapabilities": {roots: {listChanged: true}, elicitation: {form: {}, url: {}}, extensions: {"io.modelcontextprotocol/tasks": {}}}}})
    expect(discovered.error).toMatchObject({code: -32601, message: "Method not found"})
    const initialized = await client.request("initialize", {protocolVersion: "2025-11-25", capabilities: {roots: {listChanged: true}, elicitation: {form: {}, url: {}}}, clientInfo})
    expect(initialized.result).toMatchObject({protocolVersion: "2025-11-25", capabilities: {experimental: {"claude/channel": {}, "claude/channel/permission": {}}}})
  })

  it("lists tools while the service is still starting", async () => {
    let spawned!: () => void
    const spawning = new Promise<void>(resolve => (spawned = resolve))
    const pending = new Promise<void>(() => undefined)
    const {client} = await session({}, () => (spawned(), pending))
    await spawning
    const {result} = await client.request("tools/list")
    expect(result.tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "download", "list", "leave", "reply"])
  })

  it("is a channel: it passes prompts and verdicts of the service to Claude Code, and permission requests to the service", async () => {
    const attached = next("attach")
    await fakeService(async res => endWithResult(res, listResult), res => {
      attachLine(res)
      res.write(`${JSON.stringify({prompt: {text: "Fix the build", from: "Alice", item: 41, file: "/r/files/group/7_log.txt"}})}\n`)
    })
    const stdin = new PassThrough()
    const stdout = new PassThrough()
    stdins.push(stdin)
    await startSession({CLAUDE_CODE_SESSION_ID: id, CLAUDE_PROJECT_DIR: "/p/app", SIMPLEX_MCP_HOME: root}, {stdin, stdout, spawnService: noSpawn, version: "0.2.0"})
    const client = new LineClient(stdin, stdout)
    const initialized = await client.initialize()
    expect(initialized.capabilities.experimental).toEqual({"claude/channel": {}, "claude/channel/permission": {}})
    expect(initialized.instructions).toContain("Messages in the simplex channel are prompts of the members of the SimpleX group of this session: from is the member's name")
    const {result} = await client.request("tools/list")
    expect(result.tools.find(({name}: {name: string}) => name === "reply").description).toMatch(/^Sends a file to the members of the SimpleX group of this session, with text as its caption\.[^]*The file is sent at once/)
    const [attachment] = await attached
    const channel = (method: string) => client.notifications.filter(notification => notification.method === method).map(({params}) => params)
    await until(() => channel("notifications/claude/channel").length === 1)
    expect(channel("notifications/claude/channel")).toEqual([{content: "Fix the build", meta: {from: "Alice", item: "41", file: "/r/files/group/7_log.txt"}}])
    client.notify("notifications/claude/channel/permission_request", {request_id: "abcde", tool_name: "Bash", description: "Run shell command", input_preview: "rm -rf build"})
    await until(() => seen.some(line => line.startsWith("permission ")))
    expect(seen).toEqual([registered, "attach", `permission ${JSON.stringify({id, requestId: "abcde", toolName: "Bash", description: "Run shell command", inputPreview: "rm -rf build"})}`])
    attachment.write(`${JSON.stringify({permission: {requestId: "abcde", behavior: "deny"}})}\n`)
    await until(() => channel("notifications/claude/channel/permission").length === 1)
    expect(channel("notifications/claude/channel/permission")).toEqual([{request_id: "abcde", behavior: "deny"}])
  })

  it("registers, attaches and forwards a call with progress", async () => {
    await fakeService(async res => {
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
    await fakeService(async res => endWithResult(res, listResult))
    const {client} = await session({CLAUDE_PROJECT_DIR: ""})
    await listCall(client)
    expect(JSON.parse(seen[0].slice("session ".length)).project).toBe(process.cwd())
  })

  it("returns the cancelled result when the service reports a cancelled call", async () => {
    await fakeService(async res => endWithResult(res, null))
    const {client} = await session()
    expect((await listCall(client)).result).toEqual({content: [{type: "text", text: "Cancelled."}], isError: true})
  })

  it("reports a service that stopped during a call, and reconnects for the next one", async () => {
    await fakeService(async (res, n) => {
      if (n === 1) res.destroy()
      else endWithResult(res, listResult)
    })
    const {client} = await session()
    expect((await listCall(client)).result).toEqual(stoppedResult)
    expect((await listCall(client)).result).toEqual(listResult)
    expect(seen).toEqual([registered, "attach", "call list", registered, "attach", "call list"])
  })

  it("reports a call stream that ends without a result as a stopped service", async () => {
    await fakeService(async res => {
      startLines(res)({progress: "waiting"})
      res.end()
    })
    const {client} = await session()
    expect((await listCall(client)).result).toEqual(stoppedResult)
  })

  it("registers again and repeats a call the service refused for an unknown session", async () => {
    await fakeService(async (res, n) => {
      if (n === 1) throw new SocketError(unknownSession, 409)
      endWithResult(res, listResult)
    })
    const {client} = await session()
    expect((await listCall(client)).result).toEqual(listResult)
    expect(seen).toEqual([registered, "attach", "call list", registered, "attach", "call list"])
  })

  it("returns the unknown session error when the repeated call is refused too", async () => {
    await fakeService(async () => {
      throw new SocketError(unknownSession, 409)
    })
    const {client} = await session()
    expect((await listCall(client)).result).toEqual({content: [{type: "text", text: unknownSession}], isError: true})
    expect(seen).toEqual([registered, "attach", "call list", registered, "attach", "call list"])
  })

  it("reports a stopping service as stopped, and reconnects for the next call", async () => {
    await fakeService(async (res, n) => {
      if (n === 1) throw new SocketError("The SimpleX service is stopping.", 503)
      endWithResult(res, listResult)
    })
    const {client} = await session()
    expect((await listCall(client)).result).toEqual(stoppedResult)
    expect((await listCall(client)).result).toEqual(listResult)
    expect(seen).toEqual([registered, "attach", "call list", registered, "attach", "call list"])
  })

  it("returns any other service error as the tool error", async () => {
    await fakeService(async () => {
      throw new SocketError("Session 1 is closed.", 500)
    })
    const {client} = await session()
    expect((await listCall(client)).result).toEqual({content: [{type: "text", text: "Session 1 is closed."}], isError: true})
  })

  it("reconnects after /attach ends with no call in flight", async () => {
    await fakeService(async res => endWithResult(res, listResult))
    const attached = next("attach")
    const {client} = await session()
    const [res] = await attached
    await listCall(client)
    const closed = once(res, "close")
    res.destroy()
    await closed
    await ServiceClient.forRoot(root).request("GET", "/status")
    expect((await listCall(client)).result).toEqual(listResult)
    expect(seen).toEqual([registered, "attach", "call list", registered, "attach", "call list"])
  })

  it("attaches again after /attach ends, without a call", async () => {
    await fakeService(async res => endWithResult(res, listResult), res => {
      attachLine(res)
      res.write(`${JSON.stringify({prompt: {text: "Fix the build", from: "Alice", item: 41}})}\n`)
    })
    const attached = next("attach")
    const {client} = await session()
    const [res] = await attached
    await until(() => client.notifications.some(({method}) => method === "notifications/claude/channel"))
    const again = next("attach")
    res.destroy()
    await again
    expect(seen).toEqual([registered, "attach", registered, "attach"])
  })

  it("attaches once the service is back after a failed start, without a call", async () => {
    const errors = captureStderr()
    await session()
    await until(() => errors.length === 1)
    const attached = next("attach")
    await fakeService(async res => endWithResult(res, listResult))
    await attached
    expect(seen).toEqual([registered, "attach"])
  })

  it("attaches once the service answers after a start that reported a failure, without a call", async () => {
    const errors = captureStderr()
    await session({}, async () => {
      throw new ExitError("The SimpleX service did not start in 5 minutes.", 1)
    })
    await until(() => errors.length === 1)
    const attached = next("attach")
    await fakeService(async res => endWithResult(res, listResult))
    await attached
    expect(seen).toEqual([registered, "attach"])
  })

  it("waits twice as long before each new attempt when /attach fails before its first line", async () => {
    captureStderr()
    await fakeService(async res => endWithResult(res, listResult), res => res.destroy())
    await session()
    await new Promise(resolve => setTimeout(resolve, 3_600))
    expect(seen).toEqual([registered, "attach", registered, "attach", registered, "attach"])
  })

  it("stops attaching when the service speaks another protocol", async () => {
    const errors = captureStderr()
    await fakeService(async res => endWithResult(res, listResult), attachLine, protocolVersion + 1)
    await session()
    await until(() => errors.length === 1)
    await new Promise(resolve => setTimeout(resolve, 1_500))
    expect(statuses).toBe(1)
    expect(errors).toEqual(["SimpleX service: Restart Claude Code to update the SimpleX plugin."])
  })

  it("keeps a newer connection when an older call fails after it", async () => {
    await fakeService(async (res, n) => {
      if (n > 2) endWithResult(res, listResult)
    })
    const attaches: ServerResponse[] = []
    service.on("attach", res => attaches.push(res))
    const {client} = await session()
    const held = next("call", 2)
    const first = listCall(client)
    const second = listCall(client)
    const [a, b] = await held
    a.destroy()
    expect((await first).result).toEqual(stoppedResult)
    expect((await listCall(client)).result).toEqual(listResult)
    b.destroy()
    expect((await second).result).toEqual(stoppedResult)
    expect((await listCall(client)).result).toEqual(listResult)
    expect(seen).toEqual([registered, "attach", "call list", "call list", registered, "attach", "call list", "call list"])
    expect(attaches.map(res => res.destroyed)).toEqual([true, false])
  })

  it("logs a failed background start and returns the error to the next call", async () => {
    const errors = captureStderr()
    const {client} = await session({}, async () => {
      throw new ExitError("The SimpleX service did not start in 5 minutes.", 1)
    })
    expect((await listCall(client)).result).toEqual({content: [{type: "text", text: "The SimpleX service did not start in 5 minutes."}], isError: true})
    expect(errors).toEqual(["SimpleX service: The SimpleX service did not start in 5 minutes."])
  })

  it("reports a service that is gone after the start as stopped", async () => {
    const errors = captureStderr()
    const {client} = await session()
    expect((await listCall(client)).result).toEqual(stoppedResult)
    expect(errors).toEqual([expect.stringMatching(/^SimpleX service: .*connect ENOENT /)])
  })

  it("aborts the forwarded call when the client cancels", async () => {
    await fakeService(async res => {
      startLines(res)({progress: "waiting"})
    })
    const {client} = await session()
    const started = next("call")
    client.request("tools/call", {name: "receive", arguments: {chat: "@1", wait: 60}})
    const [res] = await started
    const closed = once(res, "close")
    client.notify("notifications/cancelled", {requestId: 2})
    await closed
  })

  it("releases /attach when stdin closes", async () => {
    await fakeService(async res => endWithResult(res, listResult))
    const attached = next("attach")
    const {client, stdin} = await session()
    const [res] = await attached
    await listCall(client)
    const closed = once(res, "close")
    stdin.end()
    await closed
  })

  it("logs nothing when stdin closes while /attach is opening", async () => {
    const errors = captureStderr()
    await fakeService(async res => endWithResult(res, listResult), () => undefined)
    const attached = next("attach")
    const {stdin} = await session()
    const [res] = await attached
    const closed = once(res, "close")
    stdin.end()
    await closed
    expect(errors).toEqual([])
  })
})
