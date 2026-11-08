import {once} from "events"
import {closeSync, existsSync, mkdirSync, mkdtempSync, openSync, readFileSync, rmSync, writeFileSync} from "fs"
import {Server, request} from "http"
import {Socket} from "net"
import {tmpdir} from "os"
import {join} from "path"
import {Mode} from "../../src/actions"
import {defaultConfig} from "../../src/config"
import {Daemon, DaemonDeps, HubLike, SessionLike, runDaemon} from "../../src/daemon"
import {LockedError, SessionFile, createDataFolder, readSessionFile, serverToken, sessionFolder, socketPath, writeSessionFile} from "../../src/files"
import {protocolVersion} from "../../src/protocol"
import * as socket from "../../src/socket"
import {requestJson, streamLines} from "../../src/socket"
import {closeServer, until} from "./fixtures"

const id1 = "11111111-1111-4111-8111-111111111111"
const id2 = "22222222-2222-4222-8222-222222222222"
const id3 = "33333333-3333-4333-8333-333333333333"
const token = "t"
const minutes = 60_000
const readDefaultConfig = () => defaultConfig
const fakeTimers: Parameters<typeof jest.useFakeTimers>[0] = {doNotFake: ["nextTick", "setImmediate", "queueMicrotask"]}

interface FakeSession extends SessionLike {
  displayName: string
  modes: Mode[]
  calls: string[]
  turns: unknown[]
}

function deferred<T = void>() {
  let resolve!: (value: T) => void
  const promise = new Promise<T>(r => (resolve = r))
  return {promise, resolve}
}

function deps(root: string) {
  const agents: FakeSession[] = []
  const configs: unknown[] = []
  const busy = new Set<number>()
  const stored: unknown[] = []
  const deletedGroups: number[] = []
  let expiryChecks = 0
  const hubStops: number[] = []
  const gates: {open?: Promise<void>; call?: Promise<void>; hubStop?: Promise<void>} = {}
  const opening = deferred()
  const calling = deferred()
  const aborted = deferred()
  const hubStopping = deferred()
  const stopped = deferred<number>()
  const onStopped = jest.fn(stopped.resolve)
  const hub: HubLike = {
    mainGroup: "SimpleX MCP",
    owner: () => ({owners: [], link: "https://simplex.chat/g#link"}),
    configure: async config => {
      configs.push(config)
    },
    resetOwners: async () => ({link: "https://simplex.chat/g#new"}),
    session: async (_id, {number, name}) => {
      opening.resolve()
      await gates.open
      const known = agents.find(agent => agent.number === number)
      if (known !== undefined) return known
      const agent: FakeSession = {
        number,
        displayName: `${number} · ${name}`,
        open: false,
        modes: [],
        calls: [],
        turns: [],
        busy: () => busy.has(number),
        rename: async (newName: string) => {
          agent.displayName = `${number} · ${newName}`
        },
        requestMode: mode => {
          agent.modes.push(mode)
        },
        permission: request => agent.turns.push(request),
        turn: async request => {
          agent.turns.push(request)
        },
        endPermissions: () => {
          agent.turns.push("ended")
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
      agents.push(agent)
      return agent
    },
    deleteGroup: async number => void deletedGroups.push(number),
    expireDue: () => expiryChecks++,
    stop: async () => {
      hubStops.push(agents.filter(({open}) => open).length)
      hubStopping.resolve()
      await gates.hubStop
      return []
    },
  }
  const d: DaemonDeps = {
    root,
    token,
    version: "0.2.0",
    openHub: async (_config, sessions) => {
      stored.push(...sessions)
      return hub
    },
    onStopped,
  }
  const gate = (name: keyof typeof gates): (() => void) => {
    const {promise, resolve} = deferred()
    gates[name] = promise
    return resolve
  }
  return {d, hub, agents, configs, busy, stored, deletedGroups, expiryChecks: () => expiryChecks, hubStops, onStopped, stoppedCode: stopped.promise, opening: opening.promise, calling: calling.promise, aborted: aborted.promise, hubStopping: hubStopping.promise, gate}
}

let root: string
let daemon: Daemon | undefined
let listened: Promise<Server>

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-daemon-"))
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
  const listen = socket.listen
  listened = new Promise(resolve => jest.spyOn(socket, "listen").mockImplementation(async (...args) => {
    const server = await listen(...args)
    resolve(server)
    return server
  }))
})

afterEach(async () => {
  await daemon?.stop()
  daemon = undefined
  jest.useRealTimers()
  jest.restoreAllMocks()
  rmSync(root, {recursive: true, force: true})
})

const post = <T>(route: string, body?: unknown) => requestJson<T>(socketPath(root), token, "POST", route, body)
const register = (id: string) => post("/session", {id, project: "/p/app", config: defaultConfig})
const title = (id: string, name?: string, config = defaultConfig, model?: string) => post("/title", {id, name, project: "/p/app", model, config})
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

function serverRequests(server: Server, count: number): Promise<void> {
  return new Promise(resolve => {
    let received = 0
    const onRequest = (): void => {
      if (++received < count) return
      server.off("request", onRequest)
      resolve()
    }
    server.on("request", onRequest)
  })
}

async function turnsUntil(condition: () => boolean): Promise<void> {
  for (let i = 0; i < 1000 && !condition(); i++) await new Promise(resolve => setImmediate(resolve))
  expect(condition()).toBe(true)
}

function storeSession(id: string, number: number, name: string, extra: Partial<SessionFile> = {}) {
  const folder = sessionFolder(root, id)
  createDataFolder(folder)
  writeSessionFile(folder, {number, name, project: "/p/app", created: "2026-10-24T00:00:00.000Z", ...extra})
}

describe("daemon", () => {
  it("registers sessions with stable numbers and stored names", async () => {
    const {d, agents, configs} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    const first = await register(id1)
    expect(first).toEqual({number: 1, displayName: "1 · app", version: "0.2.0", protocol: protocolVersion, owner: {owners: []}})
    await title(id2, "docs")
    expect(await post("/session", {id: id2, project: "/p/app", config: {...defaultConfig, mainGroup: "Agents"}})).toMatchObject({number: 2, displayName: "2 · docs"})
    expect(configs.at(-1)).toEqual({...defaultConfig, mainGroup: "Agents"})
    expect(await post("/session", {id: id1, project: "/p/other", config: defaultConfig})).toMatchObject({number: 1, displayName: "1 · app"})
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, name: "app", project: "/p/app"})
    expect(agents).toHaveLength(2)
  })

  it("passes the stored sessions with a group to the hub, and the approval mode of each registration to its session", async () => {
    storeSession(id1, 1, "app", {group: 20})
    storeSession(id3, 3, "old")
    const {d, agents, stored} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    expect(stored).toEqual([{id: id1, session: {number: 1, name: "app", project: "/p/app", created: "2026-10-24T00:00:00.000Z", group: 20}}])
    const host = {...defaultConfig, approve: "host" as const}
    await streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined)
    expect(agents[0].modes).toEqual([])
    await post("/session", {id: id1, project: "/p/app", config: host})
    await register(id2)
    await title(id2, "docs", host)
    await register(id1)
    expect(agents.map(({modes}) => modes)).toEqual([["host", "requests"], ["requests"]])
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

  it("releases the lock and opens no hub when it cannot listen", async () => {
    const {d} = deps(root)
    const openHub = jest.fn(d.openHub)
    mkdirSync(socketPath(root))
    await expect(Daemon.start({...d, openHub}, readDefaultConfig)).rejects.toThrow()
    expect(openHub).not.toHaveBeenCalled()
    expect(existsSync(lockFile())).toBe(false)
  })

  it("listens while the hub opens, and answers the requests that arrived meanwhile once it is open", async () => {
    const {d} = deps(root)
    const hubOpening = deferred()
    const openHubGate = deferred()
    const events: string[] = []
    const openHub: DaemonDeps["openHub"] = async (...args) => {
      hubOpening.resolve()
      await openHubGate.promise
      events.push("hub open")
      return d.openHub(...args)
    }
    const starting = Daemon.start({...d, openHub}, readDefaultConfig)
    await hubOpening.promise
    const received = serverRequests(await listened, 2)
    const status = requestJson(socketPath(root), token, "GET", "/status").then(reply => (events.push("status"), reply))
    const registered = register(id1).then(reply => (events.push("session"), reply))
    await received
    openHubGate.resolve()
    daemon = await starting
    expect(await status).toMatchObject({version: "0.2.0", mainGroup: "SimpleX MCP", sessions: []})
    expect(await registered).toMatchObject({number: 1})
    expect(events[0]).toBe("hub open")
    expect(events.slice(1).sort()).toEqual(["session", "status"])
  })

  it("fails the waiting requests, stops listening and releases the lock when the hub fails to open", async () => {
    const {d, agents} = deps(root)
    const errors: string[] = []
    jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text)), true))
    const hubOpening = deferred()
    const openHubGate = deferred()
    const openHub: DaemonDeps["openHub"] = async () => {
      hubOpening.resolve()
      await openHubGate.promise
      throw new Error("no network")
    }
    const starting = Daemon.start({...d, openHub}, readDefaultConfig)
    starting.catch(() => undefined)
    await hubOpening.promise
    const server = await listened
    const received = serverRequests(server, 2)
    const status = requestJson(socketPath(root), token, "GET", "/status")
    const registered = register(id1)
    await received
    openHubGate.resolve()
    await expect(starting).rejects.toThrow("no network")
    await expect(status).rejects.toMatchObject({message: "no network", status: 500})
    await expect(registered).rejects.toMatchObject({message: "no network", status: 500})
    expect(server.listening).toBe(false)
    expect(await register(id1).catch(error => socket.isNotRunning(error))).toBe(true)
    expect(existsSync(lockFile())).toBe(false)
    expect(agents).toHaveLength(0)
    expect(existsSync(sessionsFolder())).toBe(false)
    expect(errors.sort()).toEqual(["GET /status: no network\n", "POST /session: no network\n"])
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

  it("closes an idle session without actions, and then stops", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents, busy, onStopped, stoppedCode} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    const controller = new AbortController()
    const {attached, firstLine} = attach(id1, controller.signal)
    await firstLine
    await jest.advanceTimersByTimeAsync(20 * minutes)
    expect(agents[0].open).toBe(true)
    busy.add(1)
    controller.abort()
    await attached
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(agents[0].open).toBe(true)
    busy.clear()
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(agents[0].open).toBe(false)
    expect(onStopped).not.toHaveBeenCalled()
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(await stoppedCode).toBe(0)
    expect(existsSync(lockFile())).toBe(false)
    daemon = undefined
  })

  it("closes a registered session that never attaches", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    await jest.advanceTimersByTimeAsync(10 * minutes - 1)
    expect(agents[0].open).toBe(true)
    await jest.advanceTimersByTimeAsync(1)
    expect(agents[0].open).toBe(false)
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
    expect(agents[0].open).toBe(true)
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
    expect(agents).toMatchObject([{number: 1, displayName: "1 · app", calls: ["list"]}])
    expect(await register(id2)).toMatchObject({number: 2})
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(agents.map(({open}) => open)).toEqual([false, false])
  })

  it("refuses an attachment that opens while the daemon stops", async () => {
    storeSession(id1, 1, "app")
    const {d, opening, hubStopping, gate, stoppedCode} = deps(root)
    const finishOpen = gate("open")
    daemon = await Daemon.start(d, readDefaultConfig)
    const attached = streamLines(socketPath(root), token, "/attach", {id: id1}, () => undefined)
    await opening
    const stopping = daemon.stop()
    await hubStopping
    finishOpen()
    await expect(attached).rejects.toThrow()
    await stopping
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
    expect(agents[0].open).toBe(false)
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
    expect(agents[0].open).toBe(true)
    expect(await requestJson(socketPath(root), token, "GET", "/status")).toMatchObject({sessions: [{number: 1, attached: 0}]})
    finishCall()
    await called
    await jest.advanceTimersByTimeAsync(10 * minutes - 1)
    expect(agents[0].open).toBe(true)
    await jest.advanceTimersByTimeAsync(1)
    expect(agents[0].open).toBe(false)
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
    expect(agents).toMatchObject([{number: 1, displayName: "1 · auth"}])
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, name: "auth"})
    await title(id1)
    expect(agents).toMatchObject([{displayName: "1 · auth"}])
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
    expect(agents).toMatchObject([{number: 1, displayName: "1 · auth"}])
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, name: "auth"})
  })

  it("opens a stored session on its title, renames it and passes the model", async () => {
    storeSession(id1, 1, "app")
    const {d, agents, configs} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    expect(await title(id1, "auth", {...defaultConfig, approve: "host"}, "claude-opus-5")).toEqual(titleReply)
    expect(configs.at(-1)).toEqual({...defaultConfig, approve: "host"})
    expect(agents).toMatchObject([{number: 1, displayName: "1 · auth", modes: [], turns: [{id: id1, model: "claude-opus-5"}]}])
    expect(readSessionFile(sessionFolder(root, id1))).toEqual({number: 1, name: "auth", project: "/p/app", created: "2026-10-24T00:00:00.000Z"})
    expect(await requestJson(socketPath(root), token, "GET", "/status")).toMatchObject({sessions: [{number: 1, name: "1 · auth"}]})
  })

  it("creates an unknown session at its title, named by the title", async () => {
    const {d} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    expect(await title(id1, "auth")).toEqual(titleReply)
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, name: "auth", project: "/p/app"})
    expect(await register(id2)).toMatchObject({number: 2, displayName: "2 · app"})
    expect(await register(id1)).toMatchObject({number: 1, displayName: "1 · auth"})
  })

  it("passes permission requests and turns to the session, and its lines to the attachments while they exist", async () => {
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    const permission = {id: id1, requestId: "abcde", toolName: "Bash", description: "Run shell command", inputPreview: "ls"}
    expect(await post("/permission", permission)).toEqual({})
    expect(await post("/turn", {id: id1, answer: "Done."})).toEqual({})
    expect(agents[0].onLine).toBeUndefined()
    const controller = new AbortController()
    const {attached, firstLine, lines} = attach(id1, controller.signal)
    await firstLine
    agents[0].onLine!({prompt: {text: "hi", from: "Alice", item: 7}})
    await until(() => lines.length === 2)
    expect(lines[1]).toEqual({prompt: {text: "hi", from: "Alice", item: 7}})
    controller.abort()
    await attached
    await until(() => agents[0].turns.length === 3)
    expect(agents[0].turns).toEqual([permission, {id: id1, answer: "Done."}, "ended"])
    expect(agents[0].onLine).toBeUndefined()
    await expect(post("/turn", {id: id2, answer: "x"})).rejects.toMatchObject({status: 409})
  })

  it("does not delay the idle close of an open session with a title", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    await jest.advanceTimersByTimeAsync(9 * minutes)
    await title(id1, "auth")
    expect(agents).toMatchObject([{displayName: "1 · auth", open: true}])
    await jest.advanceTimersByTimeAsync(1 * minutes)
    expect(agents[0].open).toBe(false)
  })

  it("stops 10 minutes after a titled session closes", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, onStopped, stoppedCode} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await jest.advanceTimersByTimeAsync(5 * minutes)
    await title(id1, "auth")
    await jest.advanceTimersByTimeAsync(19 * minutes)
    expect(onStopped).not.toHaveBeenCalled()
    await jest.advanceTimersByTimeAsync(1 * minutes)
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

  it("keeps one session for a Claude Code process whose hooks change the session ID, as after /clear, and after a restart", async () => {
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await post("/title", {id: id1, pid: 100, project: "/p/app", config: defaultConfig})
    await post("/session", {id: id1, pid: 100, project: "/p/app", config: defaultConfig})
    const controller = new AbortController()
    const {attached, firstLine} = attach(id1, controller.signal)
    await firstLine
    expect(await post("/title", {id: id2, pid: 100, name: "auth", project: "/p/app", config: defaultConfig})).toEqual(titleReply)
    await post("/turn", {id: id2, pid: 100, prompt: "Fix the build"})
    expect(agents).toMatchObject([{number: 1, displayName: "1 · auth", turns: [{id: id2, pid: 100, prompt: "Fix the build"}]}])
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({name: "auth", aliases: [id2]})
    expect(existsSync(sessionFolder(root, id2))).toBe(false)
    controller.abort()
    await attached
    await daemon.stop()
    const restarted = deps(root)
    daemon = await Daemon.start(restarted.d, readDefaultConfig)
    await post("/turn", {id: id2, answer: "Fixed."})
    expect(restarted.agents).toMatchObject([{number: 1, turns: [{id: id2, answer: "Fixed."}]}])
  })

  it("joins the startup ID of a session server to the session of the title from its process, as with --continue", async () => {
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    const registered = post("/session", {id: id2, pid: 200, project: "/p/app", config: defaultConfig})
    await new Promise(resolve => setTimeout(resolve, 50))
    expect(await post("/title", {id: id1, pid: 200, name: "auth", project: "/p/app", config: defaultConfig})).toEqual(titleReply)
    expect(await registered).toMatchObject({number: 1, displayName: "1 · auth"})
    await streamLines(socketPath(root), token, "/call", {id: id2, tool: "list", input: {}}, () => undefined)
    expect(agents).toMatchObject([{number: 1, calls: ["list"]}])
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({aliases: [id2]})
    expect(await post("/session", {id: id2, pid: 201, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1})
  })

  it("passes the hooks of a stored session ID to the session attached from their process without joining the IDs, as after an in-session /resume", async () => {
    storeSession(id2, 7, "older")
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await post("/title", {id: id1, pid: 100, project: "/p/app", config: defaultConfig})
    await post("/session", {id: id1, pid: 100, project: "/p/app", config: defaultConfig})
    const controller = new AbortController()
    const {attached, firstLine} = attach(id1, controller.signal)
    await firstLine
    await post("/title", {id: id2, pid: 100, project: "/p/app", config: defaultConfig})
    await post("/turn", {id: id2, pid: 100, prompt: "Fix the build"})
    expect(agents).toMatchObject([{number: 8, turns: [{id: id2, pid: 100, prompt: "Fix the build"}]}])
    expect(readSessionFile(sessionFolder(root, id1))!.aliases).toBeUndefined()
    controller.abort()
    await attached
    await daemon.stop()
    daemon = await Daemon.start(deps(root).d, readDefaultConfig)
    expect(await post("/session", {id: id2, pid: 200, project: "/p/app", config: defaultConfig})).toMatchObject({number: 7})
  })

  it("joins a new session ID whose title came before its process registered to the stored session of that process, as after /clear during a re-attach", async () => {
    storeSession(id1, 1, "auth")
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    expect(await post("/title", {id: id2, pid: 100, name: "cleared", project: "/p/app", config: defaultConfig})).toEqual(titleReply)
    expect(await post("/session", {id: id1, pid: 100, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1})
    await turnsUntil(() => readSessionFile(sessionFolder(root, id1))!.aliases !== undefined)
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({name: "cleared", aliases: [id2]})
    expect(existsSync(sessionFolder(root, id2))).toBe(false)
    const controller = new AbortController()
    const {attached, firstLine} = attach(id1, controller.signal)
    await firstLine
    await post("/turn", {id: id2, pid: 100, prompt: "Fix the build"})
    expect(agents).toMatchObject([{number: 1, turns: [{id: id2, pid: 100, prompt: "Fix the build"}]}])
    controller.abort()
    await attached
    await daemon.stop()
    daemon = await Daemon.start(deps(root).d, readDefaultConfig)
    expect(await post("/session", {id: id2, pid: 200, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1})
  })

  it("joins a forked session ID to the resumed session of its process in either order of its title and the server's registration, as with --fork-session, also after a restart", async () => {
    const id4 = "44444444-4444-4444-8444-444444444444"
    storeSession(id1, 1, "auth")
    storeSession(id3, 2, "docs")
    const {d} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    expect(await post("/session", {id: id1, pid: 100, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1})
    await post("/title", {id: id2, pid: 100, project: "/p/app", config: defaultConfig})
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({aliases: [id2]})
    expect(existsSync(sessionFolder(root, id2))).toBe(false)
    await post("/title", {id: id4, pid: 200, project: "/p/app", config: defaultConfig})
    expect(await post("/session", {id: id3, pid: 200, project: "/p/app", config: defaultConfig})).toMatchObject({number: 2})
    await turnsUntil(() => readSessionFile(sessionFolder(root, id3))!.aliases !== undefined)
    expect(readSessionFile(sessionFolder(root, id3))).toMatchObject({aliases: [id4]})
    expect(existsSync(sessionFolder(root, id4))).toBe(false)
    await daemon.stop()
    const restarted = deps(root)
    daemon = await Daemon.start(restarted.d, readDefaultConfig)
    await post("/turn", {id: id4, pid: 200, at: 1, prompt: "Fix the build"})
    expect(restarted.agents).toMatchObject([{number: 2, turns: [{id: id4, pid: 200, at: 1, prompt: "Fix the build"}]}])
  })

  it("holds a second title of a process while the first waits, and joins both new IDs to the session its server registers, as after two /clear during a re-attach", async () => {
    storeSession(id1, 1, "auth")
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await post("/title", {id: id2, pid: 100, name: "first", project: "/p/app", config: defaultConfig})
    await post("/title", {id: id3, pid: 100, name: "second", project: "/p/app", config: defaultConfig})
    expect([sessionFolder(root, id2), sessionFolder(root, id3)].map(existsSync)).toEqual([false, false])
    expect(await post("/session", {id: id1, pid: 100, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1})
    await turnsUntil(() => readSessionFile(sessionFolder(root, id1))!.aliases?.length === 2)
    expect(readSessionFile(sessionFolder(root, id1))!.aliases).toEqual([id2, id3])
    expect([sessionFolder(root, id2), sessionFolder(root, id3)].map(existsSync)).toEqual([false, false])
    expect(agents).toHaveLength(1)
  })

  it("keeps a title that waits for its session server across a daemon stop", async () => {
    daemon = await Daemon.start(deps(root).d, readDefaultConfig)
    expect(await post("/title", {id: id1, pid: 300, name: "docs", model: "claude-opus-5", project: "/p/app", config: defaultConfig})).toEqual(titleReply)
    await daemon.stop()
    const restarted = deps(root)
    daemon = await Daemon.start(restarted.d, readDefaultConfig)
    expect(existsSync(join(root, "titles.json"))).toBe(false)
    expect(await post("/session", {id: id1, pid: 300, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1, displayName: "1 · docs"})
    await turnsUntil(() => restarted.agents[0].turns.length === 1)
    expect(restarted.agents[0].turns).toEqual([{id: id1, model: "claude-opus-5"}])
  })

  it("keeps the fields written to session.json while a title renames the session", async () => {
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    const renaming = deferred()
    const rename = jest.fn(() => renaming.promise)
    agents[0].rename = rename
    const titled = title(id1, "auth")
    await turnsUntil(() => rename.mock.calls.length === 1)
    const folder = sessionFolder(root, id1)
    writeSessionFile(folder, {...readSessionFile(folder)!, hostDeclined: true})
    renaming.resolve()
    await titled
    expect(readSessionFile(folder)).toMatchObject({name: "auth", hostDeclined: true})
  })

  it("starts its own session for a server whose process sends no title in 10 seconds, and 10 seconds after a title from a process whose session left over 10 seconds ago", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    let registered: unknown
    post("/session", {id: id1, pid: 300, project: "/p/app", config: defaultConfig}).then(reply => (registered = reply))
    await turnsUntil(() => daemon!["titled"].listenerCount("300") === 1)
    await jest.advanceTimersByTimeAsync(9_999)
    expect(registered).toBeUndefined()
    await jest.advanceTimersByTimeAsync(1)
    await turnsUntil(() => registered !== undefined)
    expect(registered).toMatchObject({number: 1, displayName: "1 · app"})
    const controller = new AbortController()
    const {attached, firstLine} = attach(id1, controller.signal)
    await firstLine
    controller.abort()
    await attached
    await turnsUntil(() => agents[0].turns.includes("ended"))
    await jest.advanceTimersByTimeAsync(10_001)
    expect(await post("/title", {id: id2, pid: 300, name: "docs", project: "/p/app", config: defaultConfig})).toEqual(titleReply)
    await jest.advanceTimersByTimeAsync(9_999)
    expect(agents.map(({displayName}) => displayName)).toEqual(["1 · app"])
    await jest.advanceTimersByTimeAsync(1)
    await turnsUntil(() => agents.length === 2)
    expect(agents.map(({displayName}) => displayName)).toEqual(["1 · app", "2 · docs"])
    expect(readSessionFile(sessionFolder(root, id1))!.aliases).toBeUndefined()
  })

  it.each(["/session", "/title"])("answers 400 to %s with an invalid config", async route => {
    const {d, agents} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    for (const config of [undefined, {...defaultConfig, mainGroup: undefined}, {...defaultConfig, approve: "always"}, {...defaultConfig, expireHours: "23"}, {...defaultConfig, expireHours: 24}]) {
      await expect(post(route, {id: id1, project: "/p/app", config})).rejects.toMatchObject({message: "Invalid config.", status: 400})
    }
    expect(agents).toHaveLength(0)
  })

  it("deletes the group of a session closed for 7 days, at its start and every hour, and keeps the session's file without the group", async () => {
    jest.useFakeTimers(fakeTimers)
    const day = 24 * 60 * minutes
    storeSession(id1, 1, "old", {group: 20, closed: new Date(Date.now() - 8 * day).toISOString()})
    storeSession(id2, 2, "recent", {group: 21, closed: new Date(Date.now() - 7 * day + 30 * minutes).toISOString()})
    const {d, deletedGroups, busy} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await turnsUntil(() => deletedGroups.length === 1)
    expect(deletedGroups).toEqual([1])
    expect(readSessionFile(sessionFolder(root, id1))).toEqual({number: 1, name: "old", project: "/p/app", created: "2026-10-24T00:00:00.000Z"})
    expect(readSessionFile(sessionFolder(root, id2))).toMatchObject({group: 21})
    await register(id1)
    busy.add(1)
    await jest.advanceTimersByTimeAsync(60 * minutes)
    await turnsUntil(() => deletedGroups.length === 2)
    expect(deletedGroups).toEqual([1, 2])
  })

  it("opens a session resumed while the daemon deletes its group once the deletion ends, without the deleted group", async () => {
    const day = 24 * 60 * minutes
    storeSession(id1, 1, "old", {group: 20, closed: new Date(Date.now() - 8 * day).toISOString()})
    const {d, hub, deletedGroups} = deps(root)
    const deleting = deferred()
    const deleteGroup = hub.deleteGroup
    hub.deleteGroup = async number => {
      await deleting.promise
      return deleteGroup(number)
    }
    const opened: SessionFile[] = []
    const session = hub.session
    hub.session = async (id, stored) => {
      opened.push(stored)
      return session(id, stored)
    }
    daemon = await Daemon.start(d, readDefaultConfig)
    const resumed = register(id1)
    await new Promise(resolve => setTimeout(resolve, 50))
    expect(opened).toEqual([])
    deleting.resolve()
    expect(await resumed).toMatchObject({number: 1})
    expect(deletedGroups).toEqual([1])
    expect(opened).toEqual([{number: 1, name: "old", project: "/p/app", created: "2026-10-24T00:00:00.000Z"}])
  })

  it("deletes the groups of the other sessions closed for 7 days when the deletion of one fails, and logs the failure", async () => {
    const day = 24 * 60 * minutes
    const closed = new Date(Date.now() - 8 * day).toISOString()
    storeSession(id1, 1, "first", {group: 20, closed})
    storeSession(id2, 2, "second", {group: 21, closed})
    const errors: string[] = []
    jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text)), true))
    const {d, hub, deletedGroups} = deps(root)
    const deleteGroup = hub.deleteGroup
    let calls = 0
    hub.deleteGroup = async number => {
      if (calls++ === 0) throw new Error("no network")
      return deleteGroup(number)
    }
    daemon = await Daemon.start(d, readDefaultConfig)
    await turnsUntil(() => deletedGroups.length === 1)
    const failed = deletedGroups[0] === 1 ? 2 : 1
    expect(errors).toEqual([`group of session ${failed}: no network\n`])
    expect(readSessionFile(sessionFolder(root, failed === 1 ? id1 : id2))).toMatchObject({group: 19 + failed, closed})
  })

  it("records when a session closes, also for a stored session at a daemon start, and removes the record when it opens", async () => {
    jest.useFakeTimers(fakeTimers)
    storeSession(id2, 2, "stored", {group: 21})
    daemon = await Daemon.start(deps(root).d, readDefaultConfig)
    expect(readSessionFile(sessionFolder(root, id2))!.closed).toBe(new Date().toISOString())
    await register(id1)
    expect(readSessionFile(sessionFolder(root, id1))!.closed).toBeUndefined()
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(readSessionFile(sessionFolder(root, id1))!.closed).toBe(new Date().toISOString())
    await register(id1)
    await register(id2)
    expect([id1, id2].map(id => readSessionFile(sessionFolder(root, id))!.closed)).toEqual([undefined, undefined])
  })

  it("trims its log every hour while it runs", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, busy} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    busy.add(1)
    writeFileSync(join(root, "daemon.log"), "x".repeat(10 * 1_048_576 + 1))
    await jest.advanceTimersByTimeAsync(60 * minutes)
    expect(readFileSync(join(root, "daemon.log"), "utf8")).toBe("")
  })

  it("reports status without the Main group link, the link on its own route, resets owners and stops on request", async () => {
    const {d, hubStops, stoppedCode} = deps(root)
    daemon = await Daemon.start(d, readDefaultConfig)
    await register(id1)
    expect(await requestJson(socketPath(root), token, "GET", "/status")).toEqual({
      version: "0.2.0",
      protocol: protocolVersion,
      mainGroup: "SimpleX MCP",
      owner: {owners: []},
      sessions: [{number: 1, name: "1 · app", attached: 0}],
    })
    expect(await requestJson(socketPath(root), token, "GET", "/link")).toEqual({owners: [], link: "https://simplex.chat/g#link"})
    expect(await post("/reset-owners")).toEqual({link: "https://simplex.chat/g#new"})
    expect(await post("/stop")).toEqual({})
    expect(hubStops).toEqual([1])
    expect(existsSync(lockFile())).toBe(false)
    expect(await stoppedCode).toBe(0)
    daemon = undefined
  })
})

describe("runDaemon", () => {
  let other: Server | undefined
  const answering = async (): Promise<void> => {
    other = await socket.listen(socketPath(root), serverToken(root), {})
  }

  afterEach(async () => {
    if (other) await closeServer(other)
    other = undefined
  })

  it("reports running when another daemon holds the lock", async () => {
    await answering()
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    await runDaemon({SIMPLEX_MCP_HOME: root})
    expect(write.mock.calls).toEqual([["running\n"]])
    expect(readFileSync(lockFile(), "utf8")).toBe(`${process.ppid}\n`)
  })

  it("reports another process that holds the lock and does not answer, and exits with 1", async () => {
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    const exited = new Error("exited")
    const exit = jest.spyOn(process, "exit").mockImplementation(() => {
      throw exited
    })
    await expect(runDaemon({SIMPLEX_MCP_HOME: root}, 10)).rejects.toBe(exited)
    expect(exit).toHaveBeenCalledWith(1)
    expect(write.mock.calls).toEqual([[`error: Another daemon uses ${root} (PID ${process.ppid}). If no daemon runs, remove ${lockFile()}.\n`]])
    expect(readFileSync(lockFile(), "utf8")).toBe(`${process.ppid}\n`)
  })

  it("waits for a daemon that is stopping to release the lock, and starts then", async () => {
    const {d, gate, hubStopping} = deps(root)
    const releaseStop = gate("hubStop")
    const stopping = await Daemon.start(d, readDefaultConfig)
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const stopped = stopping.stop()
    await hubStopping
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    const exited = new Error("exited")
    jest.spyOn(process, "exit").mockImplementation(() => {
      throw exited
    })
    const run = runDaemon({SIMPLEX_MCP_HOME: root}).catch(error => error)
    await new Promise(resolve => setTimeout(resolve, 2_000))
    expect(write).not.toHaveBeenCalled()
    jest.spyOn(Daemon, "start").mockRejectedValue(new Error("started again"))
    releaseStop()
    await stopped
    expect(await run).toBe(exited)
    expect(write.mock.calls).toEqual([["Stopped.\n"], ["error: started again\n"]])
  })

  it("reports running before it reads the config", async () => {
    await answering()
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    await runDaemon({SIMPLEX_MCP_HOME: root, CLAUDE_PLUGIN_OPTION_APPROVE: "always"})
    expect(write.mock.calls).toEqual([["running\n"]])
  })

  it("reports to the ready descriptor when given", async () => {
    await answering()
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
