import {once} from "events"
import * as fs from "fs"
import {closeSync, existsSync, mkdirSync, mkdtempSync, openSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync} from "fs"
import {Server, request} from "http"
import {Socket} from "net"
import {tmpdir} from "os"
import {join} from "path"
import {Mode} from "../../src/actions"
import {ServiceClient} from "../../src/client"
import {defaultConfig} from "../../src/config"
import {LockedError, createDataFolder, readSessionFile, serviceToken, sessionFolder, socketPath, writeSessionFile} from "../../src/files"
import {protocolVersion} from "../../src/protocol"
import {HubLike, Service, ServiceDeps, SessionLike, processStart, productionDeps, runService} from "../../src/service"
import * as socket from "../../src/socket"
import {requestJson, streamLines} from "../../src/socket"
import {closeServer, recordedRun, until} from "./fixtures"

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
  const starts = new Map<number, string>()
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
        endSteps: async () => {
          agent.turns.push("steps ended")
        },
        endToolApprovals: () => {
          agent.turns.push("tool approvals ended")
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
    expireDue: () => expiryChecks++,
    stop: async () => {
      hubStops.push(agents.filter(({open}) => open).length)
      hubStopping.resolve()
      await gates.hubStop
      return []
    },
  }
  const d: ServiceDeps = {
    root,
    token,
    version: "0.2.0",
    openHub: async (_config, sessions) => {
      stored.push(...sessions)
      return hub
    },
    onStopped,
    processStart: async pid => starts.get(pid),
  }
  const gate = (name: keyof typeof gates): (() => void) => {
    const {promise, resolve} = deferred()
    gates[name] = promise
    return resolve
  }
  return {d, agents, configs, busy, stored, starts, expiryChecks: () => expiryChecks, hubStops, onStopped, stoppedCode: stopped.promise, opening: opening.promise, calling: calling.promise, aborted: aborted.promise, hubStopping: hubStopping.promise, gate}
}

let root: string
let service: Service | undefined
let listened: Promise<Server>

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-service-"))
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
  const listen = socket.listen
  listened = new Promise(resolve => jest.spyOn(socket, "listen").mockImplementation(async (...args) => {
    const server = await listen(...args)
    resolve(server)
    return server
  }))
})

afterEach(async () => {
  await service?.stop()
  service = undefined
  jest.useRealTimers()
  jest.restoreAllMocks()
  rmSync(root, {recursive: true, force: true})
})

const post = <T>(route: string, body?: unknown) => requestJson<T>(socketPath(root), token, "POST", route, body)
const register = (id: string) => post("/session", {id, project: "/p/app", config: defaultConfig})
const title = (id: string, name?: string, config = defaultConfig, model?: string) => post("/title", {id, name, project: "/p/app", model, config})
const titleReply = {mainGroup: "SimpleX MCP", owner: {owners: [], link: "https://simplex.chat/g#link"}, version: "0.2.0", protocol: protocolVersion}
const sessionsFolder = () => join(root, "_sessions")
const lockFile = () => join(root, "service.lock")

function attach(id: string, signal: AbortSignal, pid?: number) {
  const first = deferred()
  const lines: unknown[] = []
  const attached = streamLines(socketPath(root), token, "/attach", {id, pid}, line => (lines.push(line), first.resolve()), signal).catch(() => undefined)
  return {attached, firstLine: first.promise, lines}
}

function serverConnection(service: Service): Promise<Socket> {
  return once(service["server"], "connection").then(([socket]) => socket as Socket)
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

function storeSession(id: string, number: number, name: string) {
  const folder = sessionFolder(root, id)
  createDataFolder(folder)
  writeSessionFile(folder, {number, name, project: "/p/app", created: "2026-10-24T00:00:00.000Z"})
}

describe("service", () => {
  it("registers sessions with stable numbers and stored names", async () => {
    const {d, agents, configs} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    const first = await register(id1)
    expect(first).toEqual({number: 1, displayName: "1 · app", version: "0.2.0", protocol: protocolVersion, owner: {owners: []}})
    await title(id2, "docs")
    expect(await post("/session", {id: id2, project: "/p/app", config: {...defaultConfig, mainGroup: "Agents"}})).toMatchObject({number: 2, displayName: "2 · docs"})
    expect(configs.at(-1)).toEqual({...defaultConfig, mainGroup: "Agents"})
    expect(await post("/session", {id: id1, project: "/p/other", config: defaultConfig})).toMatchObject({number: 1, displayName: "1 · app"})
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, name: "app", project: "/p/app"})
    expect(agents).toHaveLength(2)
  })

  it("passes the stored sessions to the hub, and the approval mode of each registration to its session", async () => {
    storeSession(id1, 1, "app")
    const {d, agents, stored} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    expect(stored).toEqual([{id: id1, session: {number: 1, name: "app", project: "/p/app", created: "2026-10-24T00:00:00.000Z"}}])
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
    service = await Service.start(d, readDefaultConfig)
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
    service = await Service.start(d, readDefaultConfig)
    await Promise.all([1, 2, 3].map(() => register(id1)))
    expect(agents).toHaveLength(1)
  })

  it("holds the lock while running and refuses a second service in the same folder", async () => {
    const {d} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    expect(readFileSync(lockFile(), "utf8")).toBe(`${process.pid}\n`)
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const openHub = jest.fn(d.openHub)
    await expect(Service.start({...d, openHub}, readDefaultConfig)).rejects.toThrow(LockedError)
    expect(openHub).not.toHaveBeenCalled()
    writeFileSync(lockFile(), `${process.pid}\n`)
  })

  it("starts its client and itself in a data root that holds folders of serve agents named token and lock, and leaves those folders as they are", async () => {
    const folders = ["token", "lock"]
    for (const name of folders) {
      createDataFolder(join(root, name))
      writeFileSync(join(root, name, "simplex_v1_chat.db"), name)
    }
    const client = ServiceClient.forRoot(root)
    service = await Service.start({...deps(root).d, token: productionDeps(root).token}, readDefaultConfig)
    expect(await client.request("GET", "/status")).toMatchObject({mainGroup: "SimpleX MCP"})
    for (const name of folders) {
      expect(statSync(join(root, name)).mode & 0o777).toBe(0o700)
      expect(readdirSync(join(root, name))).toEqual(["simplex_v1_chat.db"])
      expect(readFileSync(join(root, name, "simplex_v1_chat.db"), "utf8")).toBe(name)
    }
  })

  it("releases the lock and opens no hub when it cannot listen", async () => {
    const {d} = deps(root)
    const openHub = jest.fn(d.openHub)
    mkdirSync(socketPath(root))
    await expect(Service.start({...d, openHub}, readDefaultConfig)).rejects.toThrow()
    expect(openHub).not.toHaveBeenCalled()
    expect(existsSync(lockFile())).toBe(false)
  })

  it("listens while the hub opens, and answers the requests that arrived meanwhile once it is open", async () => {
    const {d} = deps(root)
    const hubOpening = deferred()
    const openHubGate = deferred()
    const events: string[] = []
    const openHub: ServiceDeps["openHub"] = async (...args) => {
      hubOpening.resolve()
      await openHubGate.promise
      events.push("hub open")
      return d.openHub(...args)
    }
    const starting = Service.start({...d, openHub}, readDefaultConfig)
    await hubOpening.promise
    const received = serverRequests(await listened, 2)
    const status = requestJson(socketPath(root), token, "GET", "/status").then(reply => (events.push("status"), reply))
    const registered = register(id1).then(reply => (events.push("session"), reply))
    await received
    openHubGate.resolve()
    service = await starting
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
    const openHub: ServiceDeps["openHub"] = async () => {
      hubOpening.resolve()
      await openHubGate.promise
      throw new Error("no network")
    }
    const starting = Service.start({...d, openHub}, readDefaultConfig)
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
    service = await Service.start(d, readDefaultConfig)
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
    service = await Service.start(d, readDefaultConfig)
    const unknown = "Unknown session; restart the Claude Code session."
    await expect(streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined)).rejects.toThrow(unknown)
    await expect(streamLines(socketPath(root), token, "/attach", {id: id1}, () => undefined)).rejects.toMatchObject({message: unknown, status: 409})
    expect(agents).toHaveLength(0)
  })

  it("writes the versions first on attach and counts attachments", async () => {
    const {d} = deps(root)
    service = await Service.start(d, readDefaultConfig)
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
    service = await Service.start(d, readDefaultConfig)
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
    service = undefined
  })

  it("closes a registered session that never attaches", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    await register(id1)
    await jest.advanceTimersByTimeAsync(10 * minutes - 1)
    expect(agents[0].open).toBe(true)
    await jest.advanceTimersByTimeAsync(1)
    expect(agents[0].open).toBe(false)
  })

  it("stops 10 minutes after start when no session registers", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, stoppedCode} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(await stoppedCode).toBe(0)
    service = undefined
  })

  it("keeps running when a session registers during the idle stop wait", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents, onStopped} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    await jest.advanceTimersByTimeAsync(5 * minutes)
    await register(id1)
    await jest.advanceTimersByTimeAsync(9 * minutes)
    expect(onStopped).not.toHaveBeenCalled()
    expect(agents[0].open).toBe(true)
  })

  it("keeps running after a refused call of an unknown session, and stops when idle", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, stoppedCode} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    await expect(streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined)).rejects.toThrow()
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(await stoppedCode).toBe(0)
    service = undefined
  })

  it("opens a stored session on a call, numbers new sessions after it, and closes it when idle", async () => {
    storeSession(id1, 1, "app")
    jest.useFakeTimers(fakeTimers)
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    await streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined)
    expect(agents).toMatchObject([{number: 1, displayName: "1 · app", calls: ["list"]}])
    expect(await register(id2)).toMatchObject({number: 2})
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(agents.map(({open}) => open)).toEqual([false, false])
  })

  it("refuses an attachment that opens while the service stops", async () => {
    storeSession(id1, 1, "app")
    const {d, opening, hubStopping, gate, stoppedCode} = deps(root)
    const finishOpen = gate("open")
    service = await Service.start(d, readDefaultConfig)
    const attached = streamLines(socketPath(root), token, "/attach", {id: id1}, () => undefined)
    await opening
    const stopping = service.stop()
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
    service = await Service.start(d, readDefaultConfig)
    const connection = serverConnection(service)
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
    service = await Service.start(d, readDefaultConfig)
    const connection = serverConnection(service)
    const controller = new AbortController()
    const called = streamLines(socketPath(root), token, "/call", {id: id1, tool: "list", input: {}}, () => undefined, controller.signal).catch(() => undefined)
    await opening
    controller.abort()
    await once(await connection, "close")
    finishOpen()
    await called
    await service.stop()
    expect(agents[0].calls).toEqual([])
    expect(await stoppedCode).toBe(0)
  })

  it("keeps an unattached session open during a call and closes it 10 minutes after", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents, calling, gate} = deps(root)
    service = await Service.start(d, readDefaultConfig)
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

  it.each(["/session", "/title"])("refuses %s when it arrives while the service stops", async route => {
    const {d, agents, gate, hubStopping, stoppedCode} = deps(root)
    const finishHubStop = gate("hubStop")
    service = await Service.start(d, readDefaultConfig)
    const requested = once(service["server"], "request")
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
    const stopping = service.stop()
    await hubStopping
    req.end(JSON.stringify({id: id1, project: "/p/app", config: defaultConfig}))
    expect(await response).toEqual({status: 503, text: JSON.stringify({error: "The SimpleX service is stopping."})})
    finishHubStop()
    await stopping
    expect(agents).toHaveLength(0)
    expect(existsSync(sessionsFolder())).toBe(false)
    expect(await stoppedCode).toBe(0)
  })

  it("renames an open session on a new title and stores the name", async () => {
    const {d, agents, configs} = deps(root)
    service = await Service.start(d, readDefaultConfig)
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
    service = await Service.start(d, readDefaultConfig)
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
    service = await Service.start(d, readDefaultConfig)
    expect(await title(id1, "auth", {...defaultConfig, approve: "host"}, "claude-opus-5")).toEqual(titleReply)
    expect(configs.at(-1)).toEqual({...defaultConfig, approve: "host"})
    expect(agents).toMatchObject([{number: 1, displayName: "1 · auth", modes: [], turns: [{id: id1, model: "claude-opus-5"}]}])
    expect(readSessionFile(sessionFolder(root, id1))).toEqual({number: 1, name: "auth", project: "/p/app", created: "2026-10-24T00:00:00.000Z"})
    expect(await requestJson(socketPath(root), token, "GET", "/status")).toMatchObject({sessions: [{number: 1, name: "1 · auth"}]})
  })

  it("creates an unknown session at its title, named by the title", async () => {
    const {d} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    expect(await title(id1, "auth")).toEqual(titleReply)
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, name: "auth", project: "/p/app"})
    expect(await register(id2)).toMatchObject({number: 2, displayName: "2 · app"})
    expect(await register(id1)).toMatchObject({number: 1, displayName: "1 · auth"})
  })

  it("passes permission requests and turns to the session, and its lines to the attachments while they exist, and ends its permissions and steps at the last detach", async () => {
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
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
    await until(() => agents[0].turns.length === 4)
    expect(agents[0].turns).toEqual([permission, {id: id1, answer: "Done."}, "ended", "steps ended"])
    expect(agents[0].onLine).toBeUndefined()
    await expect(post("/turn", {id: id2, answer: "x"})).rejects.toMatchObject({status: 409})
  })

  it("keeps the tool approvals of a session when its process attaches again, and ends them when another process registers the session and when the session closes", async () => {
    jest.useFakeTimers(fakeTimers)
    storeSession(id1, 1, "app")
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    const registerFrom = (pid: number) => post("/session", {id: id1, pid, project: "/p/app", config: defaultConfig})
    const approvalsEnded = () => agents[0].turns.filter(turn => turn === "tool approvals ended").length
    await registerFrom(300)
    const first = new AbortController()
    const {attached, firstLine} = attach(id1, first.signal)
    await firstLine
    first.abort()
    await attached
    await turnsUntil(() => agents[0].turns.includes("steps ended"))
    await registerFrom(300)
    const second = new AbortController()
    const again = attach(id1, second.signal)
    await again.firstLine
    expect(approvalsEnded()).toBe(0)
    await registerFrom(301)
    expect(approvalsEnded()).toBe(1)
    second.abort()
    await again.attached
    await turnsUntil(() => agents[0].turns.filter(turn => turn === "steps ended").length === 2)
    await jest.advanceTimersByTimeAsync(10 * minutes)
    expect(agents[0].open).toBe(false)
    expect(approvalsEnded()).toBe(2)
  })

  it("ends the tool approvals when a process with the PID of an ended process of the session registers it, by the start time of the process", async () => {
    storeSession(id1, 1, "app")
    const {d, agents, starts} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    const registerFrom = (pid: number) => post("/session", {id: id1, pid, project: "/p/app", config: defaultConfig})
    const approvalsEnded = () => agents[0].turns.filter(turn => turn === "tool approvals ended").length
    starts.set(300, "100")
    await registerFrom(300)
    await registerFrom(300)
    expect(approvalsEnded()).toBe(0)
    starts.set(300, "200")
    await registerFrom(300)
    expect(approvalsEnded()).toBe(1)
  })

  it("sends the approved tools only to the attachments of the process that registered the session last, and an empty list to the others", async () => {
    storeSession(id1, 1, "app")
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    const registerFrom = (pid: number) => post("/session", {id: id1, pid, project: "/p/app", config: defaultConfig})
    const controller = new AbortController()
    await registerFrom(300)
    const first = attach(id1, controller.signal, 300)
    await first.firstLine
    await registerFrom(301)
    const second = attach(id1, controller.signal, 301)
    await second.firstLine
    const tools = {tools: [{tool: "Bash", approvedBy: "Alice"}]}
    const prompt = {prompt: {text: "hi", from: "Alice", item: 7}}
    agents[0].onLine!(tools)
    agents[0].onLine!(prompt)
    await until(() => first.lines.length === 3 && second.lines.length === 3)
    expect(first.lines.slice(1)).toEqual([{tools: []}, prompt])
    expect(second.lines.slice(1)).toEqual([tools, prompt])
    controller.abort()
    await first.attached
    await second.attached
  })

  it("reads the start time of a running process, the same at each read, and nothing for a process that does not run", async () => {
    const start = await processStart(process.pid)
    expect(start).toEqual(process.platform === "linux" || process.platform === "darwin" ? expect.stringMatching(/\S/) : undefined)
    expect(await processStart(process.pid)).toBe(start)
    expect(await processStart(2 ** 22 + 1)).toBeUndefined()
  })

  it("does not delay the idle close of an open session with a title", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
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
    service = await Service.start(d, readDefaultConfig)
    await jest.advanceTimersByTimeAsync(5 * minutes)
    await title(id1, "auth")
    await jest.advanceTimersByTimeAsync(19 * minutes)
    expect(onStopped).not.toHaveBeenCalled()
    await jest.advanceTimersByTimeAsync(1 * minutes)
    expect(await stoppedCode).toBe(0)
    service = undefined
  })

  it.each(["/session", "/title"])("answers 400 to %s with an ID that is not a session ID", async route => {
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    await expect(post(route, {id: "../x", project: "/p/app", config: defaultConfig})).rejects.toMatchObject({message: "Not a Claude Code session ID: ../x", status: 400})
    expect(agents).toHaveLength(0)
    expect(existsSync(sessionsFolder())).toBe(false)
  })

  it("stops when the /stop client leaves before the reply", async () => {
    const {d, hubStopping, gate, stoppedCode} = deps(root)
    const finishHubStop = gate("hubStop")
    service = await Service.start(d, readDefaultConfig)
    const connection = serverConnection(service)
    const controller = new AbortController()
    const stopRequest = streamLines(socketPath(root), token, "/stop", undefined, () => undefined, controller.signal).catch(() => undefined)
    await hubStopping
    controller.abort()
    await once(await connection, "close")
    finishHubStop()
    await stopRequest
    expect(await stoppedCode).toBe(0)
    service = undefined
  })

  it("stops when its lock file is already gone", async () => {
    const {d, stoppedCode} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    rmSync(lockFile())
    await service.stop()
    expect(await stoppedCode).toBe(0)
    service = undefined
  })

  it("keeps one session for a Claude Code process whose hooks change the session ID, as after /clear, and after a restart", async () => {
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
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
    await service.stop()
    const restarted = deps(root)
    service = await Service.start(restarted.d, readDefaultConfig)
    await post("/turn", {id: id2, answer: "Fixed."})
    expect(restarted.agents).toMatchObject([{number: 1, turns: [{id: id2, answer: "Fixed."}]}])
  })

  it("joins the startup ID of a session server to the session of the title from its process, as with --continue", async () => {
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
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
    service = await Service.start(d, readDefaultConfig)
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
    await service.stop()
    service = await Service.start(deps(root).d, readDefaultConfig)
    expect(await post("/session", {id: id2, pid: 200, project: "/p/app", config: defaultConfig})).toMatchObject({number: 7})
  })

  it("joins a new session ID whose title came before its process registered to the stored session of that process, as after /clear during a re-attach", async () => {
    storeSession(id1, 1, "auth")
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
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
    await service.stop()
    service = await Service.start(deps(root).d, readDefaultConfig)
    expect(await post("/session", {id: id2, pid: 200, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1})
  })

  it("joins a forked session ID to the resumed session of its process in either order of its title and the server's registration, as with --fork-session, also after a restart", async () => {
    const id4 = "44444444-4444-4444-8444-444444444444"
    storeSession(id1, 1, "auth")
    storeSession(id3, 2, "docs")
    const {d} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    expect(await post("/session", {id: id1, pid: 100, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1})
    await post("/title", {id: id2, pid: 100, project: "/p/app", config: defaultConfig})
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({aliases: [id2]})
    expect(existsSync(sessionFolder(root, id2))).toBe(false)
    await post("/title", {id: id4, pid: 200, project: "/p/app", config: defaultConfig})
    expect(await post("/session", {id: id3, pid: 200, project: "/p/app", config: defaultConfig})).toMatchObject({number: 2})
    await turnsUntil(() => readSessionFile(sessionFolder(root, id3))!.aliases !== undefined)
    expect(readSessionFile(sessionFolder(root, id3))).toMatchObject({aliases: [id4]})
    expect(existsSync(sessionFolder(root, id4))).toBe(false)
    await service.stop()
    const restarted = deps(root)
    service = await Service.start(restarted.d, readDefaultConfig)
    await post("/turn", {id: id4, pid: 200, at: 1, prompt: "Fix the build"})
    expect(restarted.agents).toMatchObject([{number: 2, turns: [{id: id4, pid: 200, at: 1, prompt: "Fix the build"}]}])
  })

  it("holds a second title of a process while the first waits, and joins both new IDs to the session its server registers, as after two /clear during a re-attach", async () => {
    storeSession(id1, 1, "auth")
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    await post("/title", {id: id2, pid: 100, name: "first", project: "/p/app", config: defaultConfig})
    await post("/title", {id: id3, pid: 100, name: "second", project: "/p/app", config: defaultConfig})
    expect([sessionFolder(root, id2), sessionFolder(root, id3)].map(existsSync)).toEqual([false, false])
    expect(await post("/session", {id: id1, pid: 100, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1})
    await turnsUntil(() => readSessionFile(sessionFolder(root, id1))!.aliases?.length === 2)
    expect(readSessionFile(sessionFolder(root, id1))!.aliases).toEqual([id2, id3])
    expect([sessionFolder(root, id2), sessionFolder(root, id3)].map(existsSync)).toEqual([false, false])
    expect(agents).toHaveLength(1)
  })

  it("keeps a title that waits for its session server across a service stop, with the time its hook started", async () => {
    service = await Service.start(deps(root).d, readDefaultConfig)
    expect(await post("/title", {id: id1, pid: 300, at: 5, name: "docs", model: "claude-opus-5", project: "/p/app", config: defaultConfig})).toEqual(titleReply)
    await service.stop()
    expect(JSON.parse(readFileSync(join(root, "titles.json"), "utf8"))).toMatchObject([{id: id1, pid: 300, at: 5, model: "claude-opus-5"}])
    const restarted = deps(root)
    service = await Service.start(restarted.d, readDefaultConfig)
    expect(existsSync(join(root, "titles.json"))).toBe(false)
    expect(await post("/session", {id: id1, pid: 300, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1, displayName: "1 · docs"})
    await turnsUntil(() => restarted.agents[0].turns.length === 1)
    expect(restarted.agents[0].turns).toEqual([{id: id1, at: 5, model: "claude-opus-5"}])
  })

  it("passes the model of a held title with the time its hook started, and a turn of a hook that started during the wait with its own later time", async () => {
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    expect(await post("/title", {id: id1, pid: 300, at: 5, name: "docs", model: "claude-opus-5", project: "/p/app", config: defaultConfig})).toEqual(titleReply)
    await expect(post("/turn", {id: id1, pid: 300, at: 7, model: "claude-sonnet-5"})).rejects.toMatchObject({status: 409})
    expect(await post("/session", {id: id1, pid: 300, project: "/p/app", config: defaultConfig})).toMatchObject({number: 1, displayName: "1 · docs"})
    await turnsUntil(() => agents[0]?.turns.length === 1)
    await post("/turn", {id: id1, pid: 300, at: 7, model: "claude-sonnet-5"})
    expect(agents[0].turns).toEqual([{id: id1, at: 5, model: "claude-opus-5"}, {id: id1, pid: 300, at: 7, model: "claude-sonnet-5"}])
  })

  it("keeps the fields written to session.json while a title renames the session", async () => {
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
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
    service = await Service.start(d, readDefaultConfig)
    let registered: unknown
    post("/session", {id: id1, pid: 300, project: "/p/app", config: defaultConfig}).then(reply => (registered = reply))
    await turnsUntil(() => service!["titled"].listenerCount("300") === 1)
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

  it("refuses the session server of a process whose SessionStart hook marked its run unattended, before or after the server registers, and opens no session for it", async () => {
    const {ppid} = recordedRun("claude-vscode", "mcp")
    const pid = Number(recordedRun("claude-vscode", "SessionStart").env.CLAUDE_PID)
    expect(pid).toBe(ppid)
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    const unattended = {id: id1, pid, project: "/p/app", config: defaultConfig, unattended: true}
    expect(await post("/title", unattended)).toEqual(titleReply)
    const started = Date.now()
    await expect(post("/session", {id: id2, pid, project: "/p/app", config: defaultConfig})).rejects.toMatchObject({status: 403})
    expect(Date.now() - started).toBeLessThan(5_000)
    const early = post("/session", {id: id3, pid: pid + 1, project: "/p/app", config: defaultConfig}).catch(error => error)
    await turnsUntil(() => service!["titled"].listenerCount(String(pid + 1)) === 1)
    await post("/title", {...unattended, pid: pid + 1})
    expect(await early).toMatchObject({status: 403})
    expect(agents).toHaveLength(0)
    expect([id1, id2, id3].map(id => existsSync(sessionFolder(root, id)))).toEqual([false, false, false])
  })

  it("forgets the marks of unattended runs once they are 10 seconds old", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    const mark = (pid: number) => post("/title", {id: id1, pid, project: "/p/app", config: defaultConfig, unattended: true})
    await mark(400)
    await jest.advanceTimersByTimeAsync(10_001)
    await mark(401)
    expect([...service!["unattended"].keys()]).toEqual([401])
  })

  it.each(["/session", "/title"])("answers 400 to %s with an invalid config", async route => {
    const {d, agents} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    for (const config of [undefined, {...defaultConfig, mainGroup: undefined}, {...defaultConfig, approve: "always"}, {...defaultConfig, expireHours: "23"}, {...defaultConfig, expireHours: 24}]) {
      await expect(post(route, {id: id1, project: "/p/app", config})).rejects.toMatchObject({message: "Invalid config.", status: 400})
    }
    expect(agents).toHaveLength(0)
  })

  it("trims its log every hour while it runs", async () => {
    jest.useFakeTimers(fakeTimers)
    const {d, busy} = deps(root)
    service = await Service.start(d, readDefaultConfig)
    await register(id1)
    busy.add(1)
    writeFileSync(join(root, "service.log"), "x".repeat(10 * 1_048_576 + 1))
    await jest.advanceTimersByTimeAsync(60 * minutes)
    expect(readFileSync(join(root, "service.log"), "utf8")).toBe("")
  })

  it("reports status without the Main group link, the link on its own route, resets owners and stops on request", async () => {
    const {d, hubStops, stoppedCode} = deps(root)
    service = await Service.start(d, readDefaultConfig)
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
    service = undefined
  })
})

describe("runService", () => {
  let other: Server | undefined
  const answering = async (): Promise<void> => {
    other = await socket.listen(socketPath(root), serviceToken(root), {})
  }
  const brokenReadyPipe = (): jest.SpyInstance =>
    jest.spyOn(fs, "writeSync").mockImplementation(() => {
      throw Object.assign(new Error("EPIPE: broken pipe, write"), {code: "EPIPE"})
    })

  afterEach(async () => {
    if (other) await closeServer(other)
    other = undefined
  })

  it("reports running when another service holds the lock", async () => {
    await answering()
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    await runService({SIMPLEX_MCP_HOME: root})
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
    await expect(runService({SIMPLEX_MCP_HOME: root}, 10)).rejects.toBe(exited)
    expect(exit).toHaveBeenCalledWith(1)
    expect(write.mock.calls).toEqual([[`error: Another service uses ${root} (PID ${process.ppid}). If no service runs, remove ${lockFile()}.\n`]])
    expect(readFileSync(lockFile(), "utf8")).toBe(`${process.ppid}\n`)
  })

  it("waits for a service that is stopping to release the lock, and starts then", async () => {
    const {d, gate, hubStopping} = deps(root)
    const releaseStop = gate("hubStop")
    const stopping = await Service.start(d, readDefaultConfig)
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const stopped = stopping.stop()
    await hubStopping
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    const exited = new Error("exited")
    jest.spyOn(process, "exit").mockImplementation(() => {
      throw exited
    })
    const run = runService({SIMPLEX_MCP_HOME: root}).catch(error => error)
    await new Promise(resolve => setTimeout(resolve, 2_000))
    expect(write).not.toHaveBeenCalled()
    jest.spyOn(Service, "start").mockRejectedValue(new Error("started again"))
    releaseStop()
    await stopped
    expect(await run).toBe(exited)
    expect(write.mock.calls).toEqual([["Stopped.\n"], ["error: started again\n"]])
  })

  it("reports running before it reads the config", async () => {
    await answering()
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    await runService({SIMPLEX_MCP_HOME: root, CLAUDE_PLUGIN_OPTION_APPROVE: "always"})
    expect(write.mock.calls).toEqual([["running\n"]])
  })

  it("reports to the ready descriptor when given", async () => {
    await answering()
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const readyFile = join(root, "ready")
    const fd = openSync(readyFile, "w")
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    try {
      await runService({SIMPLEX_MCP_HOME: root, SIMPLEX_MCP_READY_FD: String(fd)})
    } finally {
      closeSync(fd)
    }
    expect(readFileSync(readyFile, "utf8")).toBe("running\n")
    expect(write).not.toHaveBeenCalled()
  })

  it("ends without an error when the process that waited for its running line is gone", async () => {
    await answering()
    writeFileSync(lockFile(), `${process.ppid}\n`)
    const write = brokenReadyPipe()
    await runService({SIMPLEX_MCP_HOME: root, SIMPLEX_MCP_READY_FD: "3"})
    expect(write).toHaveBeenCalledWith(3, "running\n")
  })

  it("keeps running after its own start when the process that waited for its ready line is gone", async () => {
    const stop = jest.fn(async () => undefined)
    jest.spyOn(Service, "start").mockResolvedValue({stop} as unknown as Service)
    jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    serviceToken(root)
    const write = brokenReadyPipe()
    const signals = ["SIGINT", "SIGTERM"] as const
    const before = signals.map(signal => process.listeners(signal))
    try {
      await runService({SIMPLEX_MCP_HOME: root, SIMPLEX_MCP_READY_FD: "3"})
      expect(write).toHaveBeenCalledWith(3, "ready\n")
      expect(signals.map(signal => process.listeners(signal).length)).toEqual(before.map(listeners => listeners.length + 1))
      expect(stop).not.toHaveBeenCalled()
    } finally {
      signals.forEach((signal, i) => process.listeners(signal).filter(listener => !before[i].includes(listener)).forEach(listener => process.off(signal, listener)))
    }
  })

  it("reports a start error and exits with 1", async () => {
    const write = jest.spyOn(process.stdout, "write").mockImplementation(() => true)
    const exited = new Error("exited")
    const exit = jest.spyOn(process, "exit").mockImplementation(() => {
      throw exited
    })
    await expect(runService({SIMPLEX_MCP_HOME: root, CLAUDE_PLUGIN_OPTION_APPROVE: "always"})).rejects.toBe(exited)
    expect(exit).toHaveBeenCalledWith(1)
    expect(write.mock.calls).toEqual([["error: Plugin option approve: requests or host expected, got always. Change it in /plugin > Installed > simplex > Configure options.\n"]])
    expect(existsSync(lockFile())).toBe(false)
  })
})
