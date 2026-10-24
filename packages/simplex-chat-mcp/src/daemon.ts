import {writeSync} from "fs"
import {Server, ServerResponse} from "http"
import {CallToolResult} from "@modelcontextprotocol/server"
import {T} from "@simplex-chat/types"
import {Action, Actions} from "./actions"
import {openSessionAgent, sessionName} from "./agent"
import {Config, readConfig} from "./config"
import {SessionView, Sessions} from "./control"
import {LockedError, acquireLock, createDataFolder, dataRoot, readSessionFile, releaseLock, serverToken, sessionFolder, sessionNumbers, socketPath, writeSessionFile} from "./files"
import {Hub, Perform} from "./hub"
import {errorText, log, logError} from "./log"
import {ToolCall} from "./messages"
import {AttachRequest, CallRequest, OwnerState, SessionReply, SessionRequest, StatusReply, protocolVersion} from "./protocol"
import {Handler, SocketError, listen, sendJson, startLines} from "./socket"
import {ToolName} from "./tools"
import {version} from "./version"

const idleClose = 10 * 60_000
const idleStop = 10 * 60_000
const firstFreeFd = 3

export interface HubLike {
  readonly mainGroup: string
  readonly actions: Actions
  owner(): OwnerState
  configure(config: Config): Promise<void>
  resetOwners(): Promise<string>
  standingApprover(chatInfo: T.ChatInfo): string | undefined
  stop(): Promise<number>
}

export interface AgentLike extends SessionView {
  rename(name: string): Promise<void>
  close(): Promise<void>
  call(tool: ToolName, input: unknown, call: ToolCall): Promise<CallToolResult | undefined>
  perform(action: Action, signal: AbortSignal): Promise<void>
}

export interface OpenSessionOptions {
  folder: string
  number: number
  name: string
  hub: HubLike
}

export interface DaemonDeps {
  root: string
  token: string
  version: string
  openHub(config: Config, sessions: Sessions, perform: Perform): Promise<HubLike>
  openSession(options: OpenSessionOptions): Promise<AgentLike>
  onStopped(code: number): void
}

interface Entry {
  opening: Promise<AgentLike>
  agent?: AgentLike
  attached: number
  idle?: NodeJS.Timeout
}

export class Daemon {
  private readonly entries = new Map<string, Entry>()
  private readonly closing = new Map<string, Promise<void>>()
  private readonly streams = new Set<ServerResponse>()
  private nextNumber: number
  private stopTimer?: NodeJS.Timeout
  private stopping?: Promise<void>
  private hub!: HubLike
  private server!: Server

  private constructor(private readonly deps: DaemonDeps) {
    this.nextNumber = Math.max(0, ...sessionNumbers(deps.root)) + 1
  }

  static async start(deps: DaemonDeps, config: Config): Promise<Daemon> {
    acquireLock(deps.root)
    try {
      const daemon = new Daemon(deps)
      daemon.hub = await deps.openHub(config, daemon.sessions(), (action, signal) => daemon.perform(action, signal))
      try {
        daemon.server = await listen(socketPath(deps.root), deps.token, daemon.routes())
      } catch (error) {
        await daemon.hub.stop().catch(stopError => logError("stop", stopError))
        throw error
      }
      daemon.scheduleStop()
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

  private routes(): {[route: string]: Handler} {
    return {
      "POST /session": async (body, res) => sendJson(res, 200, await this.onSession(body as SessionRequest)),
      "POST /attach": (body, res) => this.onAttach(body as AttachRequest, res),
      "POST /call": (body, res) => this.onCall(body as CallRequest, res),
      "GET /status": async (_body, res) => sendJson(res, 200, this.status()),
      "POST /reset-owners": async (_body, res) => sendJson(res, 200, {link: await this.hub.resetOwners()}),
      "POST /stop": async (_body, res) => {
        await this.stop(res)
        sendJson(res, 200, {})
      },
    }
  }

  private async shutdown(reply: ServerResponse | undefined): Promise<void> {
    clearTimeout(this.stopTimer)
    for (const entry of this.entries.values()) clearTimeout(entry.idle)
    const closed = new Promise<void>(resolve => this.server.close(() => resolve()))
    for (const res of this.streams) res.destroy()
    this.server.closeIdleConnections()
    const cancelled = await this.hub.stop().catch(error => {
      logError("stop", error)
      return 0
    })
    if (cancelled > 0) log(`Cancelled ${cancelled} action${cancelled === 1 ? "" : "s"}.`)
    const agents = await Promise.all([...this.entries.values()].map(({opening}) => opening.catch(() => undefined)))
    this.entries.clear()
    for (const agent of agents) await agent?.close().catch(error => logError("close session", error))
    await Promise.all(this.closing.values())
    releaseLock(this.deps.root)
    log("Stopped.")
    const stopped = (): void => {
      closed.then(() => this.deps.onStopped(0))
    }
    // the /stop reply is sent after this promise resolves; its connection closes only once the reply is written
    if (reply === undefined) stopped()
    else
      reply.once("finish", () => {
        this.server.closeIdleConnections()
        stopped()
      })
  }

  private agents(): AgentLike[] {
    return [...this.entries.values()].flatMap(({agent}) => (agent === undefined ? [] : [agent]))
  }

  private agent(number: number): AgentLike | undefined {
    return this.agents().find(agent => agent.number === number)
  }

  private sessions(): Sessions {
    return {get: number => this.agent(number), open: () => this.agents()}
  }

  private perform(action: Action, signal: AbortSignal): Promise<void> {
    const agent = this.agent(action.session)
    if (agent === undefined) return Promise.reject(new Error(`Session ${action.session} is closed.`))
    return agent.perform(action, signal)
  }

  private async onSession(request: SessionRequest): Promise<SessionReply> {
    const entry = this.entry(request.id, request)
    const agent = await entry.opening
    if (entry.attached === 0) this.scheduleIdle(request.id, entry)
    await this.hub.configure(request.config)
    const folder = sessionFolder(this.deps.root, request.id)
    const stored = readSessionFile(folder)!
    const name = request.name?.trim()
    if (name && name !== stored.name) {
      await agent.rename(name)
      writeSessionFile(folder, {...stored, name})
    }
    return {number: agent.number, displayName: agent.displayName, version: this.deps.version, protocol: protocolVersion, owner: this.hub.owner()}
  }

  private async onAttach({id}: AttachRequest, res: ServerResponse): Promise<void> {
    const entry = this.entry(id)
    await entry.opening
    if (!this.hold(res)) return
    entry.attached++
    clearTimeout(entry.idle)
    startLines(res)({version: this.deps.version, protocol: protocolVersion})
    res.once("close", () => {
      entry.attached--
      if (entry.attached === 0) this.scheduleIdle(id, entry)
    })
  }

  private async onCall({id, tool, input}: CallRequest, res: ServerResponse): Promise<void> {
    const agent = await this.entry(id).opening
    if (!this.hold(res)) return
    const controller = new AbortController()
    res.once("close", () => {
      if (!res.writableFinished) controller.abort()
    })
    const write = startLines(res)
    const result = await agent.call(tool, input, {signal: controller.signal, progress: progress => write({progress})})
    if (!controller.signal.aborted) res.end(`${JSON.stringify({result: result ?? null})}\n`)
  }

  private hold(res: ServerResponse): boolean {
    if (this.stopping !== undefined) {
      res.destroy()
      return false
    }
    this.streams.add(res)
    res.once("close", () => this.streams.delete(res))
    return true
  }

  // the stop timer runs only while no entries exist, so a new entry cancels it
  private entry(id: string, request?: SessionRequest): Entry {
    const existing = this.entries.get(id)
    if (existing !== undefined) return existing
    clearTimeout(this.stopTimer)
    const entry: Entry = {attached: 0, opening: this.openAgent(id, request)}
    this.entries.set(id, entry)
    this.scheduleIdle(id, entry)
    entry.opening.then(
      agent => {
        entry.agent = agent
      },
      () => {
        clearTimeout(entry.idle)
        this.entries.delete(id)
        if (this.entries.size === 0) this.scheduleStop()
      },
    )
    return entry
  }

  private async openAgent(id: string, request: SessionRequest | undefined): Promise<AgentLike> {
    await this.closing.get(id)
    const folder = sessionFolder(this.deps.root, id)
    let stored = readSessionFile(folder)
    if (stored === undefined) {
      if (request === undefined) throw new SocketError("Unknown session; restart the Claude Code session.", 409)
      createDataFolder(folder)
      stored = {number: this.nextNumber++, name: sessionName(request.name, request.project), project: request.project, created: new Date().toISOString()}
      writeSessionFile(folder, stored)
    }
    return this.deps.openSession({folder, number: stored.number, name: stored.name, hub: this.hub})
  }

  private scheduleIdle(id: string, entry: Entry): void {
    clearTimeout(entry.idle)
    if (this.stopping !== undefined) return
    entry.idle = setTimeout(() => {
      const {agent} = entry
      if (agent === undefined || this.hub.actions.ofSession(agent.number).length > 0) return this.scheduleIdle(id, entry)
      this.entries.delete(id)
      this.closeAgent(id, agent)
      if (this.entries.size === 0) this.scheduleStop()
    }, idleClose)
  }

  private closeAgent(id: string, agent: AgentLike): void {
    const closed = agent
      .close()
      .catch(error => logError("close session", error))
      .finally(() => this.closing.delete(id))
    this.closing.set(id, closed)
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
    daemon = await Daemon.start(productionDeps(root), readConfig(env))
  } catch (error) {
    if (error instanceof LockedError) return ready("running")
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
    openHub: (config, sessions, perform) => Hub.open(root, config, sessions, perform),
    openSession: async ({folder, number, name, hub}) => {
      const agent = await openSessionAgent({folder, number, name, actions: hub.actions, standingApprover: chatInfo => hub.standingApprover(chatInfo)})
      return Object.assign(agent, {
        call: (tool: ToolName, input: unknown, call: ToolCall) => agent.tools.call(tool, input, call),
        perform: (action: Action, signal: AbortSignal) => agent.tools.perform(action, signal),
      })
    },
    onStopped: code => process.exit(code),
  }
}

function readyChannel(env: NodeJS.ProcessEnv): (line: string) => void {
  const fd = Number(env.SIMPLEX_MCP_READY_FD)
  return line => {
    if (Number.isInteger(fd) && fd >= firstFreeFd) writeSync(fd, `${line}\n`)
    else process.stdout.write(`${line}\n`)
  }
}
