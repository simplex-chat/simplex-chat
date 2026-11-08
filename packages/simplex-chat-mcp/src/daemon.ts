import {EventEmitter} from "events"
import {writeSync} from "fs"
import {Server, ServerResponse} from "http"
import {basename} from "path"
import {CallToolResult} from "@modelcontextprotocol/server"
import {Action, Mode} from "./actions"
import {Config, readConfig} from "./config"
import {LockedError, SessionFile, acquireLock, createDataFolder, dataRoot, logFile, readSessionFile, releaseLock, serverToken, sessionFolder, socketPath, storedSessions, trimLog, writeSessionFile} from "./files"
import {Hub, StoredSession, hubTexts} from "./hub"
import {ExitError, errorText, log, logError} from "./log"
import {ToolCall, validName} from "./messages"
import {AttachLine, AttachRequest, CallRequest, OwnerState, PermissionRequest, ResetReply, SessionReply, SessionRequest, StatusReply, TitleReply, TitleRequest, TurnRequest, protocolVersion} from "./protocol"
import {Handler, SocketError, isNotRunning, listen, requestJson, sendJson, startLines} from "./socket"
import {ToolName} from "./tools"
import {version} from "./version"

const idleClose = 10 * 60_000
const idleStop = 10 * 60_000
const processWait = 10_000
const logCheck = 3_600_000
const lockAnswerAttempts = 10
const lockAnswerWait = 100
const firstFreeFd = 3
const unnamedSession = "Claude Code"

export interface HubLike {
  readonly mainGroup: string
  owner(): OwnerState
  configure(config: Config): Promise<void>
  resetOwners(): Promise<ResetReply>
  session(id: string, stored: SessionFile): Promise<SessionLike>
  expireDue(): void
  stop(): Promise<Action[]>
}

export interface SessionLike {
  readonly number: number
  readonly displayName: string
  open: boolean
  onLine?: (line: AttachLine) => void
  busy(): boolean
  rename(name: string): Promise<void>
  requestMode(mode: Mode): void
  call(tool: ToolName, input: unknown, call: ToolCall): Promise<CallToolResult | undefined>
  permission(request: PermissionRequest): void
  turn(request: TurnRequest): Promise<void>
  endPermissions(): void
}

export interface DaemonDeps {
  root: string
  token: string
  version: string
  openHub(config: Config, stored: StoredSession[]): Promise<HubLike>
  onStopped(code: number): void
}

interface Entry {
  opening: Promise<SessionLike>
  agent?: SessionLike
  attached: number
  calls: number
  writers: Set<(line: AttachLine) => void>
  idle?: NodeJS.Timeout
  pid?: number
  seen?: number
}

export class Daemon {
  private readonly entries = new Map<string, Entry>()
  private readonly streams = new Set<ServerResponse>()
  private readonly stored: StoredSession[]
  private readonly aliases = new Map<string, string>()
  private readonly titles = new Map<number, {id: string; at: number}>()
  private readonly titled = new EventEmitter()
  private nextNumber: number
  private stopTimer?: NodeJS.Timeout
  private logTimer?: NodeJS.Timeout
  private stopping?: Promise<void>
  private hub!: HubLike
  private hubReady!: Promise<HubLike>
  private server!: Server

  private constructor(private readonly deps: DaemonDeps) {
    this.stored = storedSessions(deps.root)
    this.nextNumber = Math.max(0, ...this.stored.map(({session}) => session.number)) + 1
    for (const {id, session} of this.stored) for (const alias of session.aliases ?? []) this.aliases.set(alias, id)
  }

  static async start(deps: DaemonDeps, config: () => Config): Promise<Daemon> {
    acquireLock(deps.root, "daemon")
    try {
      const daemon = new Daemon(deps)
      // listen before the hub opens: a starter that lost the lock race sends /status right after "running"
      daemon.server = await listen(socketPath(deps.root), deps.token, daemon.routes())
      daemon.hubReady = daemon.openHub(config)
      try {
        await daemon.hubReady
      } catch (error) {
        await daemon.closeListener()
        throw error
      }
      daemon.scheduleStop()
      daemon.logTimer = setInterval(() => {
        try {
          trimLog(deps.root)
        } catch (error) {
          logError(logFile(deps.root), error)
        }
      }, logCheck).unref()
      return daemon
    } catch (error) {
      releaseLock(deps.root)
      throw error
    }
  }

  stop(reply?: ServerResponse): Promise<void> {
    this.stopping ??= this.shutdown(reply)
    return this.stopping
  }

  private async openHub(config: () => Config): Promise<HubLike> {
    this.hub = await this.deps.openHub(config(), this.stored)
    return this.hub
  }

  private closeListener(): Promise<void> {
    const closed = new Promise<void>(resolve => this.server.close(() => resolve()))
    this.server.closeIdleConnections()
    return closed
  }

  private routes(): {[route: string]: Handler} {
    return {
      "POST /session": this.expiring(async (body, res) => sendJson(res, 200, await this.onSession(body as SessionRequest))),
      "POST /title": this.expiring(async (body, res) => sendJson(res, 200, await this.onTitle(body as TitleRequest))),
      "POST /attach": this.expiring((body, res) => this.onAttach(body as AttachRequest, res)),
      "POST /call": this.expiring((body, res) => this.onCall(body as CallRequest, res)),
      "POST /permission": this.expiring(async (body, res) => {
        const request = body as PermissionRequest
        ;(await this.entry(this.canonical(request.id)).opening).permission(request)
        sendJson(res, 200, {})
      }),
      "POST /turn": this.expiring(async (body, res) => {
        const request = body as TurnRequest
        await (await this.entry(await this.resolve(request.id, request.pid)).opening).turn(request)
        sendJson(res, 200, {})
      }),
      "GET /status": this.expiring(async (_body, res) => sendJson(res, 200, this.status())),
      "POST /reset-owners": this.withHub(async (_body, res) => sendJson(res, 200, await this.hub.resetOwners())),
      "POST /stop": this.withHub(async (_body, res) => {
        await this.stop(res)
        sendJson(res, 200, {})
      }),
    }
  }

  private withHub(handler: Handler): Handler {
    return async (body, res, req) => {
      await this.hubReady
      return handler(body, res, req)
    }
  }

  private expiring(handler: Handler): Handler {
    return this.withHub((body, res, req) => {
      this.hub.expireDue()
      return handler(body, res, req)
    })
  }

  private async shutdown(reply: ServerResponse | undefined): Promise<void> {
    clearTimeout(this.stopTimer)
    clearInterval(this.logTimer)
    for (const entry of this.entries.values()) clearTimeout(entry.idle)
    const closed = new Promise<void>(resolve => this.server.close(() => resolve()))
    for (const res of this.streams) res.destroy()
    this.server.closeIdleConnections()
    const {length: cancelled} = await this.hub.stop().catch(error => {
      logError("stop", error)
      return []
    })
    if (cancelled > 0) log(`Cancelled ${cancelled} action${cancelled === 1 ? "" : "s"}.`)
    this.entries.clear()
    releaseLock(this.deps.root)
    log("Stopped.")
    const stopped = (): void => {
      this.server.closeAllConnections()
      closed.then(() => this.deps.onStopped(0))
    }
    // the /stop reply is sent after this promise resolves; "close" also fires when the client leaves before it is written
    if (reply === undefined || reply.destroyed) stopped()
    else reply.once("close", stopped)
  }

  private async onSession(request: SessionRequest): Promise<SessionReply> {
    validConfig(request.config)
    const id = await this.resolve(request.id, request.pid, true)
    const entry = this.entry(id, {...request, id})
    entry.pid = request.pid
    entry.seen = Date.now()
    const agent = await entry.opening
    if (entry.attached === 0) this.scheduleIdle(id, entry)
    await this.hub.configure(request.config)
    agent.requestMode(request.config.approve)
    return {number: agent.number, displayName: agent.displayName, version: this.deps.version, protocol: protocolVersion, owner: this.hub.owner()}
  }

  private async onTitle(request: TitleRequest): Promise<TitleReply> {
    const {name, project, model, config} = request
    validConfig(config)
    const id = await this.resolve(request.id, request.pid)
    const title = name?.trim() || undefined
    const entry = this.entry(id, {id, project, name: title, config})
    if (request.pid !== undefined) {
      this.titles.set(request.pid, {id, at: Date.now()})
      this.titled.emit(String(request.pid))
    }
    const agent = await entry.opening
    await this.hub.configure(config)
    if (title !== undefined) await this.rename(this.folder(id), agent, title)
    if (model !== undefined) await agent.turn({id, model})
    return {mainGroup: this.hub.mainGroup, owner: this.hub.owner(), version: this.deps.version, protocol: protocolVersion}
  }

  private canonical(id: string): string {
    return this.aliases.get(id) ?? id
  }

  private async resolve(id: string, pid: number | undefined, fromServer = false): Promise<string> {
    const alias = this.aliases.get(id)
    if (alias !== undefined) return alias
    if (readSessionFile(this.folder(id)) !== undefined) return (fromServer || pid === undefined ? undefined : this.attachedSession(pid)) ?? id
    if (pid === undefined) return id
    if (fromServer && this.processSession(pid) === undefined) {
      await new Promise<void>(resolve => {
        const done = (): void => {
          clearTimeout(timer)
          this.titled.off(String(pid), done)
          resolve()
        }
        const timer = setTimeout(done, processWait)
        this.titled.on(String(pid), done)
      })
    }
    const target = this.processSession(pid) ?? id
    if (target !== id) this.alias(id, target)
    return target
  }

  private attachedSession(pid: number): string | undefined {
    return [...this.entries].find(([, entry]) => entry.pid === pid && entry.attached > 0)?.[0]
  }

  private processSession(pid: number): string | undefined {
    const recent = Date.now() - processWait
    const detached = [...this.entries].find(([, entry]) => entry.pid === pid && (entry.seen ?? 0) > recent)?.[0]
    const title = this.titles.get(pid)
    return this.attachedSession(pid) ?? detached ?? (title !== undefined && title.at > recent ? title.id : undefined)
  }

  private alias(id: string, target: string): void {
    this.aliases.set(id, target)
    const folder = this.folder(target)
    const stored = readSessionFile(folder)
    if (stored !== undefined && !stored.aliases?.includes(id)) writeSessionFile(folder, {...stored, aliases: [...(stored.aliases ?? []), id]})
  }

  private async rename(folder: string, agent: SessionLike, name: string): Promise<void> {
    const stored = readSessionFile(folder)!
    if (name === stored.name) return
    await agent.rename(name)
    writeSessionFile(folder, {...stored, name})
  }

  private async onAttach(request: AttachRequest, res: ServerResponse): Promise<void> {
    const id = this.canonical(request.id)
    const entry = this.entry(id)
    const agent = await entry.opening
    if (!this.hold(res)) return
    entry.attached++
    clearTimeout(entry.idle)
    const write = startLines(res)
    write({version: this.deps.version, protocol: protocolVersion})
    entry.writers.add(write)
    agent.onLine = line => entry.writers.forEach(writer => writer(line))
    res.once("close", () => {
      entry.attached--
      entry.writers.delete(write)
      if (entry.attached > 0) return
      entry.seen = Date.now()
      agent.onLine = undefined
      agent.endPermissions()
      this.scheduleIdle(id, entry)
    })
  }

  private async onCall(request: CallRequest, res: ServerResponse): Promise<void> {
    const {tool, input} = request
    const id = this.canonical(request.id)
    const entry = this.entry(id)
    const agent = await entry.opening
    if (!this.hold(res)) return
    const controller = new AbortController()
    res.once("close", () => {
      if (!res.writableFinished) controller.abort()
    })
    const write = startLines(res)
    entry.calls++
    try {
      const result = await agent.call(tool, input, {signal: controller.signal, progress: progress => write({progress})})
      if (!controller.signal.aborted) res.end(`${JSON.stringify({result: result ?? null})}\n`)
    } finally {
      entry.calls--
      if (entry.attached === 0 && entry.calls === 0) this.scheduleIdle(id, entry)
    }
  }

  private hold(res: ServerResponse): boolean {
    if (this.stopping !== undefined || res.destroyed) {
      res.destroy()
      return false
    }
    this.streams.add(res)
    res.once("close", () => this.streams.delete(res))
    return true
  }

  // the stop timer runs only while no entries exist, so a new entry cancels it
  private entry(id: string, request?: SessionRequest): Entry {
    this.refuseWhenStopping()
    const folder = this.folder(id)
    const existing = this.entries.get(id)
    if (existing !== undefined) return existing
    clearTimeout(this.stopTimer)
    const entry: Entry = {attached: 0, calls: 0, writers: new Set(), opening: this.openAgent(id, folder, request)}
    this.entries.set(id, entry)
    this.scheduleIdle(id, entry)
    entry.opening.then(
      agent => {
        entry.agent = agent
        agent.open = true
      },
      () => {
        clearTimeout(entry.idle)
        this.entries.delete(id)
        if (this.entries.size === 0) this.scheduleStop()
      },
    )
    return entry
  }

  private refuseWhenStopping(): void {
    if (this.stopping !== undefined) throw new SocketError(hubTexts.stopping, 503)
  }

  private folder(id: string): string {
    try {
      return sessionFolder(this.deps.root, id)
    } catch (error) {
      throw error instanceof ExitError ? new SocketError(error.message, 400) : error
    }
  }

  private async openAgent(id: string, folder: string, request: SessionRequest | undefined): Promise<SessionLike> {
    let stored = readSessionFile(folder)
    if (stored === undefined) {
      if (request === undefined) throw new SocketError("Unknown session; restart the Claude Code session.", 409)
      createDataFolder(folder)
      stored = {number: this.nextNumber++, name: sessionName(request.name, request.project), project: request.project, created: new Date().toISOString()}
      writeSessionFile(folder, stored)
    }
    return this.hub.session(id, stored)
  }

  private scheduleIdle(id: string, entry: Entry): void {
    clearTimeout(entry.idle)
    if (this.stopping !== undefined) return
    entry.idle = setTimeout(() => {
      const {agent} = entry
      if (agent === undefined || entry.calls > 0 || agent.busy()) return this.scheduleIdle(id, entry)
      this.entries.delete(id)
      agent.open = false
      if (this.entries.size === 0) this.scheduleStop()
    }, idleClose)
  }

  private scheduleStop(): void {
    clearTimeout(this.stopTimer)
    if (this.stopping !== undefined) return
    this.stopTimer = setTimeout(() => {
      this.stop().catch(error => logError("stop", error))
    }, idleStop)
  }

  private status(): StatusReply {
    const sessions = [...this.entries.values()].flatMap(({agent, attached}) => (agent === undefined ? [] : [{number: agent.number, name: agent.displayName, attached}]))
    return {version: this.deps.version, protocol: protocolVersion, mainGroup: this.hub.mainGroup, owner: this.hub.owner(), sessions}
  }
}

export async function runDaemon(env: NodeJS.ProcessEnv = process.env): Promise<void> {
  const ready = readyChannel(env)
  const root = dataRoot(env)
  let daemon: Daemon
  try {
    createDataFolder(root)
    daemon = await Daemon.start(productionDeps(root), () => readConfig(env))
  } catch (error) {
    if (error instanceof LockedError && (await answers(root))) return ready("running")
    ready(`error: ${errorText(error)}`)
    process.exit(1)
  }
  log(`SimpleX MCP daemon ${version}, data folder ${root}`)
  ready("ready")
  const stop = (): void => {
    daemon.stop().catch(error => {
      logError("stop", error)
      process.exit(1)
    })
  }
  process.on("SIGINT", stop)
  process.on("SIGTERM", stop)
}

export function productionDeps(root: string): DaemonDeps {
  return {
    root,
    token: serverToken(root),
    version,
    openHub: (config, stored) => Hub.open(root, config, stored),
    onStopped: code => process.exit(code),
  }
}

async function answers(root: string): Promise<boolean> {
  for (let attempt = 0; attempt < lockAnswerAttempts; attempt++) {
    if (await requestJson(socketPath(root), serverToken(root), "GET", "/").then(() => true, error => !isNotRunning(error))) return true
    await new Promise(resolve => setTimeout(resolve, lockAnswerWait))
  }
  return false
}

function validConfig(config: Config | undefined): void {
  const {mainGroup, approve, expireHours} = config ?? {}
  const valid = typeof mainGroup === "string" && validName(mainGroup) === mainGroup && (approve === "requests" || approve === "host") && Number.isInteger(expireHours) && expireHours! >= 1 && expireHours! <= 23
  if (!valid) throw new SocketError("Invalid config.", 400)
}

export function sessionName(title: string | undefined, project: string): string {
  const named = title?.trim()
  if (named) return named
  return basename(project) || unnamedSession
}

function readyChannel(env: NodeJS.ProcessEnv): (line: string) => void {
  const fd = Number(env.SIMPLEX_MCP_READY_FD)
  return line => {
    if (Number.isInteger(fd) && fd >= firstFreeFd) writeSync(fd, `${line}\n`)
    else process.stdout.write(`${line}\n`)
  }
}
