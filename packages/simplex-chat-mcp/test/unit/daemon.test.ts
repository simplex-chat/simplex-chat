import {once} from "events"
import {closeSync, existsSync, mkdirSync, mkdtempSync, openSync, readFileSync, rmSync, writeFileSync} from "fs"
import {request} from "http"
import {Socket} from "net"
import {tmpdir} from "os"
import {join} from "path"
import {Actions} from "../../src/actions"
import {defaultConfig} from "../../src/config"
import {Daemon, DaemonDeps, HubLike, runDaemon} from "../../src/daemon"
import {LockedError, createDataFolder, readSessionFile, sessionFolder, socketPath, writeSessionFile} from "../../src/files"
import {protocolVersion} from "../../src/protocol"
import {requestJson, streamLines} from "../../src/socket"

const id1 = "11111111-1111-4111-8111-111111111111"
const id2 = "22222222-2222-4222-8222-222222222222"
const token = "t"
const minutes = 60_000
const readDefaultConfig = () => defaultConfig
const fakeTimers: Parameters<typeof jest.useFakeTimers>[0] = {doNotFake: ["nextTick", "setImmediate", "queueMicrotask"]}

interface FakeAgent {
  number: number
  displayName: string
  closed: boolean
  calls: string[]
}

function deferred<T = void>() {
  let resolve!: (value: T) => void
  const promise = new Promise<T>(r => (resolve = r))
  return {promise, resolve}
}

function deps(root: string) {
  const agents: FakeAgent[] = []
  const configs: unknown[] = []
  const actions: {session: number}[] = []
  let expiryChecks = 0
  const hubStops: number[] = []
  const events: string[] = []
  const gates: {open?: Promise<void>; close?: Promise<void>; call?: Promise<void>; hubStop?: Promise<void>} = {}
  const opening = deferred()
  const calling = deferred()
  const aborted = deferred()
  const hubStopping = deferred()
  const stopped = deferred<number>()
  const onStopped = jest.fn(stopped.resolve)
  const hub: HubLike = {
    mainGroup: "SimpleX MCP",
    actions: {ofSession: (n: number) => actions.filter(({session}) => session === n), expireDue: () => expiryChecks++} as unknown as Actions,
    owner: () => ({owners: [], link: "https://simplex.chat/g#link"}),
    configure: async config => {
      configs.push(config)
    },
    resetOwners: async () => "https://simplex.chat/g#new",
    standingApprover: () => undefined,
    stop: async () => {
      hubStops.push(agents.filter(({closed}) => closed).length)
      hubStopping.resolve()
      await gates.hubStop
      return 0
    },
  }
  const d: DaemonDeps = {
    root,
    token,
    version: "0.2.0",
    openHub: async () => hub,
    openSession: async ({number, name}) => {
      opening.resolve()
      await gates.open
      const agent: FakeAgent = {number, displayName: `SimpleX MCP - ${name}`, closed: false, calls: []}
      agents.push(agent)
      events.push(`open ${number}`)
      return {
        number,
        chat: undefined as never,
        tools: {agentChats: async () => [], knownChat: async () => Promise.reject(new Error("no chats"))} as never,
        perform: async () => undefined,
        get displayName() {
          return agent.displayName
        },
        rename: async (newName: string) => {
          agent.displayName = `SimpleX MCP - ${newName}`
        },
        close: async () => {
          await gates.close
          agent.closed = true
          events.push(`closed ${number}`)
        },
        call: async (tool: string, _input: unknown, call: {signal: AbortSignal; progress: (m: string) => void}) => {
          agent.calls.push(tool)
          calling.resolve()
          call.progress("waiting")
          if (tool === "receive") {
            await new Promise(resolve => call.signal.addEventListener("abort", resolve))
            aborted.resolve()
          } else await gates.call
          return call.signal.aborted ? undefined : {content: [{type: "text", text: "{}"}], structuredContent: {}}
        },
      }
    },
    onStopped,
  }
  const gate = (name: keyof typeof gates): (() => void) => {
    const {promise, resolve} = deferred()
    gates[name] = promise
    return resolve
  }
  return {d, agents, configs, actions, expiryChecks: () => expiryChecks, hubStops, events, onStopped, stoppedCode: stopped.promise, opening: opening.promise, calling: calling.promise, aborted: aborted.promise, hubStopping: hubStopping.promise, gate}
}

let root: string
let daemon: Daemon | undefined

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-daemon-"))
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(async () => {
  await daemon?.stop()
  daemon = undefined
  jest.useRealTimers()
  jest.restoreAllMocks()
  rmSync(root, {recursive: true, force: true})
})

const post = <T>(route: string, body?: unknown) => requestJson<T>(socketPath(root), token, "POST", route, body)
const register = (id: string, name?: string) => post("/session", {id, name, project: "/p/app", config: defaultConfig})
const title = (id: string, name?: string, config = defaultConfig) => post("/title", {id, name, config})
const titleReply = {mainGroup: "SimpleX MCP", owner: {owners: [], link: "https://simplex.chat/g#link"}, version: "0.2.0", protocol: protocolVersion}
const sessionsFolder = () => join(root, "_sessions")
const lockFile = () => join(root, "lock")

function attach(id: string, signal: AbortSignal) {
  const first = deferred()
  const lines: unknown[] = []
  const attached = streamLines(socketPath(root), token, "/attach", {id}, line => (lines.push(line), first.resolve()), signal).catch(() => undefined)
  return {attached, firstLine: first.promise, lines}
}

function serverConnection(daemon: Daemon): Promise<Socket> {
  return once(daemon["server"], "connection").then(([socket]) => socket as Socket)
}

function storeSession(id: string, number: number, name: string) {
  const folder = sessionFolder(root, id)
  createDataFolder(folder)
  writeSessionFile(folder, {number, name, project: "/p/app", created: "2026-10-24T00:00:00.000Z"})
}

describe("daemon", () => {
  it("registers sessions with stable numbers and renames on a new title", async () => {
    const {d, agents, configs} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    const first = await register(id1)
    expect(first).toEqual({number: 1, displayName: "SimpleX MCP - app", version: "0.2.0", protocol: protocolVersion, owner: {owners: [], link: "https://simplex.chat/g#link"}})
    expect(await post("/session", {id: id2, name: "docs", project: "/p/app", config: {...defaultConfig, mainGroup: "Agents"}})).toMatchObject({number: 2, displayName: "SimpleX MCP - docs"})
    expect(configs.at(-1)).toEqual({...defaultConfig, mainGroup: "Agents"})
    expect(await register(id1, "auth")).toMatchObject({number: 1, displayName: "SimpleX MCP - auth"})
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, name: "auth", project: "/p/app"})
    expect(agents).toHaveLength(2)
  })

  it("expires due actions at each request that touches sessions", async () => {
    const {d, expiryChecks} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    expect(expiryChecks()).toBe(1)
    await title(id1)
    expect(expiryChecks()).toBe(2)
    await requestJson(socketPath(root), token, "GET", "/status")
    expect(expiryChecks()).toBe(3)
    await streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined)
    expect(expiryChecks()).toBe(4)
    const controller = new AbortController()
    const {attached, firstLine} = attach(id1, controller.signal)
    await firstLine
    expect(expiryChecks()).toBe(5)
    controller.abort()
    await attached
    await post("/reset-owners")
    expect(expiryChecks()).toBe(5)
  })

  it("opens one agent for concurrent registrations", async () => {
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await Promise.all([1, 2, 3].map(() => register(id1)))
    expect(agents).toHaveLength(1)
  })

  it("holds the lock while running and refuses a second daemon in the same folder", async () => {
    const {d} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    expect(readFileSync(lockFile(), "utf8")).toBe(`${process.pid}\n`)
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const openHub = jest.fn(d.openHub)
    await expect(Daemon.start({...d, openHub}, readDefaultConfig)).rejects.toThrow(LockedError)
    expect(openHub).not.toHaveBeenCalled()
    writeFileSync(lockFile(), `${process.pid}\n`)
  })

  it("stops the hub and releases the lock when it cannot listen", async () => {
    const {d, hubStops} = deps(root)
    mkdirSync(socketPath(root))
    await expect(Daemon.start(d, readDefaultConfig)).rejects.toThrow()
    expect(hubStops).toHaveLength(1)
    expect(existsSync(lockFile())).toBe(false)
  })

  it("streams progress and the result, and cancels the call when the client closes", async () => {
    const {d, agents, aborted} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    const lines: unknown[] = []
    await streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, line => lines.push(line))
    expect(lines).toEqual([{progress: "waiting"}, {result: {content: [{type: "text", text: "{}"}], structuredContent: {}}}])
    const controller = new AbortController()
    const receiving = streamLines(socketPath(root), token, "/call", {id: id1, tool: "receive", input: {chat: "@1"}}, () => controller.abort(), controller.signal)
    await expect(receiving).rejects.toThrow()
    await aborted
    expect(agents[0].calls).toEqual(["list", "receive"])
  })

  it("refuses a call or an attachment of an unknown session", async () => {
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    const unknown = "Unknown session; restart the Claude Code session."
    await expect(streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined)).rejects.toThrow(unknown)
    await expect(streamLines(socketPath(root), token, "/attach", {id: id1}, () => undefined)).rejects.toMatchObject({message: unknown, status: 409})
    expect(agents).toHaveLength(0)
  })

  it("writes the versions first on attach and counts attachments", async () => {
    const {d} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    const controller = new AbortController()
    const {attached, firstLine, lines} = attach(id1, controller.signal)
    await firstLine
    expect(lines).toEqual([{version: "0.2.0", protocol: protocolVersion}])
    expect(await requestJson(socketPath(root), token, "GET", "/status")).toMatchObject({sessions: [{number: 1, attached: 1}]})
    controller.abort()
    await attached
  })

  it("closes an idle agent without actions, and then stops", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents, actions, onStopped, stoppedCode} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    const controller = new AbortController()
    const {attached, firstLine} = attach(id1, controller.signal)
    await firstLine
    await jest.advanceTimersByTimeAsync(20 * minutes)
    expect(agents[0].closed).toBe(false)
    actions.push({session: 1})
    controller.abort()
    await attached
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(agents[0].closed).toBe(false)
    actions.length = 0
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(agents[0].closed).toBe(true)
    expect(onStopped).not.toHaveBeenCalled()
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(await stoppedCode).toBe(0)
    expect(existsSync(lockFile())).toBe(false)
    daemon = undefined
  })

  it("closes a registered agent that never attaches", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    await jest.advanceTimersByTimeAsync(10 * minutes - 1)
    expect(agents[0].closed).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    expect(agents[0].closed).toBe(true)
  })

  it("stops 10 minutes after start when no session registers", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, stoppedCode} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(await stoppedCode).toBe(0)
    daemon = undefined
  })

  it("keeps running when a session registers during the idle stop wait", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents, onStopped} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await jest.advanceTimersByTimeAsync(5 * minutes)
    await register(id1)
    await jest.advanceTimersByTimeAsync(9 * minutes)
    expect(onStopped).not.toHaveBeenCalled()
    expect(agents[0].closed).toBe(false)
  })

  it("keeps running after a refused call of an unknown session, and stops when idle", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, stoppedCode} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await expect(streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined)).rejects.toThrow()
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(await stoppedCode).toBe(0)
    daemon = undefined
  })

  it("opens a stored session on a call, numbers new sessions after it, and closes it when idle", async () => {
    storeSession(id1, 1, "app")
    jest.useFakeTimers(fakeTimers)
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined)
    expect(agents).toMatchObject([{number: 1, displayName: "SimpleX MCP - app", calls: ["list"]}])
    expect(await register(id2)).toMatchObject({number: 2})
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(agents.map(({closed}) => closed)).toEqual([true, true])
  })

  it("opens a session again only after its idle close completes", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, events, gate} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    const openClose = gate("close")
    await jest.advanceTimersByTimeAsync(10 * minutes)
    const registered = register(id1)
    await jest.advanceTimersByTimeAsync(0)
    openClose()
    expect(await registered).toMatchObject({number: 1})
    expect(events).toEqual(["open 1", "closed 1", "open 1"])
  })

  it("refuses an attachment that opens while the daemon stops", async () => {
    storeSession(id1, 1, "app")
    const {d, agents, opening, hubStopping, gate, stoppedCode} = deps(root)
    const finishOpen = gate("open")
    daemon = await Daemon.start(d, readDefaultConfig)
    const attached = streamLines(socketPath(root), token, "/attach", {id: id1}, () => undefined)
    await opening
    const stopping = daemon.stop()
    await hubStopping
    finishOpen()
    await expect(attached).rejects.toThrow()
    await stopping
    expect(agents[0].closed).toBe(true)
    expect(await stoppedCode).toBe(0)
  })

  it("does not count an attachment whose client left while the session opened", async () => {
    jest.useFakeTimers(fakeTimers)
    storeSession(id1, 1, "app")
    const {d, agents, opening, gate} = deps(root)
    const finishOpen = gate("open")
    daemon = await Daemon.start(d, readDefaultConfig)
    const connection = serverConnection(daemon)
    const controller = new AbortController()
    const attached = streamLines(socketPath(root), token, "/attach", {id: id1}, () => undefined, controller.signal).catch(() => undefined)
    await opening
    controller.abort()
    await once(await connection, "close")
    finishOpen()
    await attached
    expect(await requestJson(socketPath(root), token, "GET", "/status")).toMatchObject({sessions: [{number: 1, attached: 0}]})
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(agents[0].closed).toBe(true)
  })

  it("does not run a call whose client left while the session opened", async () => {
    storeSession(id1, 1, "app")
    const {d, agents, opening, gate, stoppedCode} = deps(root)
    const finishOpen = gate("open")
    daemon = await Daemon.start(d, readDefaultConfig)
    const connection = serverConnection(daemon)
    const controller = new AbortController()
    const called = streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined, controller.signal).catch(() => undefined)
    await opening
    controller.abort()
    await once(await connection, "close")
    finishOpen()
    await called
    await daemon.stop()
    expect(agents[0].calls).toEqual([])
    expect(await stoppedCode).toBe(0)
  })

  it("keeps an unattached session open during a call and closes it 10 minutes after", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents, calling, gate} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    const finishCall = gate("call")
    const called = streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined)
    await calling
    await jest.advanceTimersByTimeAsync(25 * minutes)
    expect(agents[0].closed).toBe(false)
    expect(await requestJson(socketPath(root), token, "GET", "/status")).toMatchObject({sessions: [{number: 1, attached: 0}]})
    finishCall()
    await called
    await jest.advanceTimersByTimeAsync(10 * minutes - 1)
    expect(agents[0].closed).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    expect(agents[0].closed).toBe(true)
  })

  it.each(["/session", "/title"])("refuses %s when it arrives while the daemon stops", async route => {
    const {d, agents, gate, hubStopping, stoppedCode} = deps(root)
    const finishHubStop = gate("hubStop")
    daemon = await Daemon.start(d, readDefaultConfig)
    const requested = once(daemon["server"], "request")
    const req = request({socketPath: socketPath(root), method: "POST", path: route, headers: {authorization: `Bearer ${token}`}})
    const response = new Promise<{status: number | undefined; text: string}>((resolve, reject) => {
      req.on("response", res => {
        let text = ""
        res.setEncoding("utf8")
        res.on("data", chunk => (text += chunk))
        res.on("end", () => resolve({status: res.statusCode, text}))
      })
      req.on("error", reject)
    })
    req.flushHeaders()
    await requested
    const stopping = daemon.stop()
    await hubStopping
    req.end(JSON.stringify({id: id1, project: "/p/app", config: defaultConfig}))
    expect(await response).toEqual({status: 503, text: JSON.stringify({error: "The SimpleX daemon is stopping."})})
    finishHubStop()
    await stopping
    expect(agents).toHaveLength(0)
    expect(existsSync(sessionsFolder())).toBe(false)
    expect(await stoppedCode).toBe(0)
  })

  it("renames an open session on a new title and stores the name", async () => {
    const {d, agents, configs} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    expect(await title(id1, " auth ", {...defaultConfig, mainGroup: "Agents"})).toEqual(titleReply)
    expect(configs.at(-1)).toEqual({...defaultConfig, mainGroup: "Agents"})
    expect(agents).toMatchObject([{number: 1, displayName: "SimpleX MCP - auth"}])
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, name: "auth"})
    await title(id1)
    expect(agents).toMatchObject([{displayName: "SimpleX MCP - auth"}])
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({name: "auth"})
  })

  it("renames a session that is still opening once it opens", async () => {
    storeSession(id1, 1, "app")
    const {d, agents, opening, gate} = deps(root)
    const finishOpen = gate("open")
    daemon = await Daemon.start(d, readDefaultConfig)
    const called = streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined)
    await opening
    const titled = title(id1, "auth")
    finishOpen()
    expect(await titled).toEqual(titleReply)
    await called
    expect(agents).toMatchObject([{number: 1, displayName: "SimpleX MCP - auth"}])
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, name: "auth"})
  })

  it("renames a stored session in its file without opening it", async () => {
    storeSession(id1, 1, "app")
    const {d, agents, configs} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    expect(await title(id1, "auth", {...defaultConfig, approve: "host"})).toEqual(titleReply)
    expect(configs.at(-1)).toEqual({...defaultConfig, approve: "host"})
    expect(agents).toHaveLength(0)
    expect(readSessionFile(sessionFolder(root, id1))).toEqual({number: 1, name: "auth", project: "/p/app", created: "2026-10-24T00:00:00.000Z"})
    expect(await requestJson(socketPath(root), token, "GET", "/status")).toMatchObject({sessions: []})
    expect(await register(id1)).toMatchObject({number: 1, displayName: "SimpleX MCP - auth"})
  })

  it("remembers the title of an unknown session without a folder or a number, and uses it once", async () => {
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    expect(await title(id1, "auth")).toEqual(titleReply)
    expect(agents).toHaveLength(0)
    expect(existsSync(sessionsFolder())).toBe(false)
    expect(await requestJson(socketPath(root), token, "GET", "/status")).toMatchObject({sessions: []})
    expect(await register(id2)).toMatchObject({number: 1, displayName: "SimpleX MCP - app"})
    expect(await register(id1)).toMatchObject({number: 2, displayName: "SimpleX MCP - auth"})
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 2, name: "auth", project: "/p/app"})
    expect(daemon["titles"].size).toBe(0)
  })

  it("prefers the registration's own name to a remembered title", async () => {
    const {d} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await title(id1, "auth")
    expect(await register(id1, "docs")).toMatchObject({number: 1, displayName: "SimpleX MCP - docs"})
    expect(daemon["titles"].size).toBe(0)
  })

  it("remembers at most 100 titles, dropping the oldest", async () => {
    const {d} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    const ids = Array.from({length: 101}, (_, n) => `${n.toString(16).padStart(8, "0")}-0000-4000-8000-000000000000`)
    for (const [n, id] of ids.entries()) await title(id, `title ${n}`)
    expect(daemon["titles"].size).toBe(100)
    expect(await register(ids[0])).toMatchObject({displayName: "SimpleX MCP - app"})
    expect(await register(ids[1])).toMatchObject({displayName: "SimpleX MCP - title 1"})
    expect(await register(ids[100])).toMatchObject({displayName: "SimpleX MCP - title 100"})
  })

  it("keeps a re-titled ID as the newest and drops the next oldest", async () => {
    const {d} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    const ids = Array.from({length: 101}, (_, n) => `${n.toString(16).padStart(8, "0")}-0000-4000-8000-000000000000`)
    for (const [n, id] of ids.slice(0, 100).entries()) await title(id, `title ${n}`)
    await title(ids[0], "renamed 0")
    await title(ids[100], "title 100")
    expect(daemon["titles"].size).toBe(100)
    expect(await register(ids[1])).toMatchObject({displayName: "SimpleX MCP - app"})
    expect(await register(ids[0])).toMatchObject({displayName: "SimpleX MCP - renamed 0"})
    expect(await register(ids[2])).toMatchObject({displayName: "SimpleX MCP - title 2"})
  })

  it("does not delay the idle close of an open session with a title", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    await jest.advanceTimersByTimeAsync(9 * minutes)
    await title(id1, "auth")
    expect(agents).toMatchObject([{displayName: "SimpleX MCP - auth", closed: false}])
    await jest.advanceTimersByTimeAsync(1 * minutes)
    expect(agents[0].closed).toBe(true)
  })

  it("does not delay the idle stop with a title", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, stoppedCode} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await jest.advanceTimersByTimeAsync(5 * minutes)
    await title(id1, "auth")
    await jest.advanceTimersByTimeAsync(5 * minutes)
    expect(await stoppedCode).toBe(0)
    daemon = undefined
  })

  it.each(["/session", "/title"])("answers 400 to %s with an ID that is not a session ID", async route => {
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await expect(post(route, {id: "../x", project: "/p/app", config: defaultConfig})).rejects.toMatchObject({message: "Not a Claude Code session ID: ../x", status: 400})
    expect(agents).toHaveLength(0)
    expect(existsSync(sessionsFolder())).toBe(false)
  })

  it("stops when the /stop client leaves before the reply", async () => {
    const {d, hubStopping, gate, stoppedCode} = deps(root)
    const finishHubStop = gate("hubStop")
    daemon = await Daemon.start(d, readDefaultConfig)
    const connection = serverConnection(daemon)
    const controller = new AbortController()
    const stopRequest = streamLines(socketPath(root), token, "/stop", undefined, () => undefined, controller.signal).catch(() => undefined)
    await hubStopping
    controller.abort()
    await once(await connection, "close")
    finishHubStop()
    await stopRequest
    expect(await stoppedCode).toBe(0)
    daemon = undefined
  })

  it("stops when its lock file is already gone", async () => {
    const {d, stoppedCode} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    rmSync(lockFile())
    await daemon.stop()
    expect(await stoppedCode).toBe(0)
    daemon = undefined
  })

  it("reports status, resets owners and stops on request", async () => {
    const {d, agents, hubStops, stoppedCode} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    expect(await requestJson(socketPath(root), token, "GET", "/status")).toEqual({
      version: "0.2.0",
      protocol: protocolVersion,
      mainGroup: "SimpleX MCP",
      owner: {owners: [], link: "https://simplex.chat/g#link"},
      sessions: [{number: 1, name: "SimpleX MCP - app", attached: 0}],
    })
    expect(await post("/reset-owners")).toEqual({link: "https://simplex.chat/g#new"})
    expect(await post("/stop")).toEqual({})
    expect(hubStops).toEqual([0])
    expect(agents[0].closed).toBe(true)
    expect(existsSync(lockFile())).toBe(false)
    expect(await stoppedCode).toBe(0)
    daemon = undefined
  })
})

describe("runDaemon", () => {
  it("reports running when another daemon holds the lock", async () => {
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    await runDaemon({SIMPLEX_MCP_HOME: root})
    expect(write.mock.calls).toEqual([["running\n"]])
    expect(readFileSync(lockFile(), "utf8")).toBe(`${process.ppid}\n`)
  })

  it("reports running before it reads the config", async () => {
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    await runDaemon({SIMPLEX_MCP_HOME: root, CLAUDE_PLUGIN_OPTION_APPROVE: "always"})
    expect(write.mock.calls).toEqual([["running\n"]])
  })

  it("reports to the ready descriptor when given", async () => {
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const readyFile = join(root, "ready")
    const fd = openSync(readyFile, "w")
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    try {
      await runDaemon({SIMPLEX_MCP_HOME: root, SIMPLEX_MCP_READY_FD: String(fd)})
    } finally {
      closeSync(fd)
    }
    expect(readFileSync(readyFile, "utf8")).toBe("running\n")
    expect(write).not.toHaveBeenCalled()
  })

  it("reports a start error and exits with 1", async () => {
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    const exited = new Error("exited")
    const exit = jest.spyOn(process, "exit").mockImplementation(() => {
      throw exited
    })
    await expect(runDaemon({SIMPLEX_MCP_HOME: root, CLAUDE_PLUGIN_OPTION_APPROVE: "always"})).rejects.toBe(exited)
    expect(exit).toHaveBeenCalledWith(1)
    expect(write.mock.calls).toEqual([["error: Plugin option approve: requests or host expected, got always. Change it in /plugin > Installed > simplex > Configure options.\n"]])
    expect(existsSync(lockFile())).toBe(false)
  })
})
