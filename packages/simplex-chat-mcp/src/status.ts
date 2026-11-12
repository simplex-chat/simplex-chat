import {T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {ToolError, stoppingMessage} from "./actions"
import type {ActivityState} from "./activity"
import type {InferenceState} from "./inference"
import {logError} from "./log"
import {chatNumber, chatType, getChat, isLive, pageSize, utcMinutes} from "./messages"
import type {Agent, ToolName} from "./tools"

interface Status {
  live: api.LiveMessage
  started: number
  renders: number
  label: string
  note?: string
  timer?: NodeJS.Timeout
}

const frames = ["◐", "◓", "◑", "◒"]
const liveRenders = 50
const maxRenders = 59
const firstGap = 3_000
const lastGap = 20_000
const checkGap = 300_000
const idleEnd = 30 * 60_000
const statusLifetime = 23 * 3_600_000
const deleteWindow = 30 * 3_600_000 - 10 * 60_000
const waitingLabel = "received, waiting for the agent"
const toolLabels = new Map<string | undefined, string>(Object.entries({Bash: "running a command", PowerShell: "running a command", Monitor: "running a command", Grep: "searching files", Glob: "searching files", WebSearch: "searching the web", WebFetch: "searching the web"}))
const fileVerbs = new Map<string | undefined, string>(Object.entries({Read: "reading", Edit: "editing", Write: "editing", MultiEdit: "editing", NotebookEdit: "editing"}))

export class Statuses {
  private readonly statuses = new Map<string, Status>()
  private readonly calls: {tool: ToolName}[] = []
  private lastCall = Date.now()
  private readonly endings = new Set<Promise<void>>()
  private inferenceFailed = false
  private stopping = false

  constructor(private readonly agent: Pick<Agent, "chat" | "actions" | "inference" | "activity">, private readonly allowed: (chat: string) => Promise<boolean>) {}

  startCall(tool: ToolName): () => void {
    const call = {tool}
    this.calls.push(call)
    this.lastCall = Date.now()
    return () => {
      this.calls.splice(this.calls.indexOf(call), 1)
      this.lastCall = Date.now()
    }
  }

  queue(chat: string): void {
    if (this.stopping || this.statuses.has(chat)) return
    const status = this.created(chat, waitingLabel)
    this.tick(chat, status).catch(error => logError(`status of ${chat}`, error))
  }

  start(chat: string): void {
    this.queue(chat)
    const status = this.statuses.get(chat)
    if (status?.label !== waitingLabel) return
    status.label = "thinking"
    if (status.renders > 0 && status.renders < maxRenders) this.render(status)
  }

  show(chat: string, note: string): void {
    if (this.stopping) throw new ToolError(stoppingMessage)
    const current = this.statuses.get(chat)
    if (current !== undefined && Date.now() - current.started >= statusLifetime) this.end(chat)
    const kept = this.statuses.get(chat)
    if (kept !== undefined && kept.renders >= maxRenders) throw new ToolError(`live: the update limit of the status message of ${chat} is reached; send your answer without live.`)
    const status = kept ?? this.created(chat)
    status.note = note
    this.render(status)
    if (kept === undefined) this.schedule(chat, status)
  }

  end(chat: string): Promise<void> {
    const status = this.statuses.get(chat)
    if (status === undefined) return Promise.resolve()
    this.statuses.delete(chat)
    clearTimeout(status.timer)
    const ending = status.live.end("")
    this.endings.add(ending)
    return ending.finally(() => this.endings.delete(ending))
  }

  async stop(): Promise<number> {
    this.stopping = true
    const deletions = [...this.statuses.values()].filter(({renders}) => renders > 0).length
    for (const chat of [...this.statuses.keys()]) this.end(chat)
    await Promise.all(this.endings)
    return deletions
  }

  async deleteLeftovers(chat: string): Promise<void> {
    const now = Date.now()
    const recent = ({meta}: T.ChatItem): boolean => now - Date.parse(meta.itemTs) < deleteWindow
    try {
      const ids: number[] = []
      for (let pagination: T.ChatPagination = {type: "last", count: pageSize}; ; ) {
        const items: T.ChatItem[] = (await getChat(this.agent.chat, chat, pagination)).chatItems
        ids.push(...items.filter(item => item.content.type === "sndMsgContent" && isLive(item) && item.meta.itemDeleted === undefined && recent(item)).map(({meta}) => meta.itemId))
        if (items.length < pageSize || !recent(items[0])) break
        pagination = {type: "before", chatItemId: items[0].meta.itemId, count: pageSize}
      }
      if (ids.length > 0) await this.agent.chat.apiDeleteChatItems(chatType(chat), chatNumber(chat), ids, T.CIDeleteMode.Broadcast)
    } catch (error) {
      logError(`status of ${chat}`, error)
    }
  }

  private created(chat: string, label = "thinking"): Status {
    const status: Status = {live: new api.LiveMessage(this.agent.chat, chatType(chat), chatNumber(chat), 0, error => this.failed(chat, status, error)), started: Date.now(), renders: 0, label}
    this.statuses.set(chat, status)
    return status
  }

  private isCurrent(chat: string, status: Status): boolean {
    return this.statuses.get(chat) === status
  }

  private failed(chat: string, status: Status, error: unknown): void {
    logError(`status of ${chat}`, error)
    if (this.isCurrent(chat, status)) this.end(chat)
  }

  private async tick(chat: string, status: Status): Promise<void> {
    const shown = await this.allowed(chat).catch(error => {
      if (this.isCurrent(chat, status)) logError(`status of ${chat}`, error)
      return false
    })
    if (!this.isCurrent(chat, status)) return
    const idle = status.renders >= liveRenders && !this.calls.some(call => call.tool !== "receive") && Date.now() - this.lastCall >= idleEnd
    if (!shown || Date.now() - status.started >= statusLifetime || idle) return this.end(chat)
    if (status.label !== waitingLabel && status.renders > 0 && status.renders + 1 < liveRenders) {
      const label = await this.label()
      if (!this.isCurrent(chat, status)) return
      status.label = label
    }
    if (status.renders < liveRenders) this.render(status)
    this.schedule(chat, status)
  }

  private render(status: Status): void {
    status.renders++
    status.live.update(statusText(status, Date.now()))
  }

  private schedule(chat: string, status: Status): void {
    const age = Date.now() - status.started
    const delay = status.renders < liveRenders ? Math.min(Math.max(age / 10, firstGap), lastGap) : checkGap
    status.timer = setTimeout(() => this.tick(chat, status).catch(error => logError(`status of ${chat}`, error)), delay).unref()
  }

  private async label(): Promise<string> {
    const state = await this.inferred()
    const waiting = this.agent.actions.pending().some(({open}) => open > 0)
    const tool = this.calls.filter(call => call.tool !== "receive").at(-1)?.tool
    return statusLabel(waiting, state, tool, this.agent.activity?.state())
  }

  private async inferred(): Promise<InferenceState | undefined> {
    if (this.agent.inference === undefined) return undefined
    try {
      const state = await this.agent.inference()
      this.inferenceFailed = false
      return state
    } catch (error) {
      if (!this.inferenceFailed) logError("inference endpoint", error)
      this.inferenceFailed = true
      return undefined
    }
  }
}

function statusText({started, renders, label, note}: Status, now: number): string {
  const seconds = Math.floor((now - started) / 1000)
  const line = renders < liveRenders ? `${frames[(renders - 1) % frames.length]} ${label} · ${Math.floor(seconds / 60)}:${String(seconds % 60).padStart(2, "0")}` : `◌ ${label === waitingLabel ? label : "working"} since ${utcMinutes(started)}`
  return note === undefined ? line : `${line}\n${note}`
}

function statusLabel(waiting: boolean, state: InferenceState | undefined, tool: ToolName | undefined, activity: ActivityState | undefined): string {
  switch (state?.type) {
    case "loading":
      return named("loading", state.model, "loading the model")
    case "prompt":
      return `reading the prompt${modelText(state.model)}${state.tokens > 0 ? `, ${state.tokens} tokens read` : ""}`
    case "output":
      return `generating${modelText(state.model)}${state.rate === undefined ? "" : `, ${Math.round(state.rate)} tokens/s`}`
    case "idle":
      return otherLabel(waiting, tool, activity, "working")
    case undefined:
      return otherLabel(waiting, tool, activity, "thinking")
  }
}

function modelText(model: string | undefined): string {
  return model === undefined ? "" : ` with \`${model}\``
}

function otherLabel(waiting: boolean, tool: ToolName | undefined, activity: ActivityState | undefined, rest: string): string {
  return waiting ? "waiting for approval"
    : activity?.asking ? "waiting for permission in Claude Code"
    : tool !== undefined ? `using ${tool}`
    : activity?.compacting ? "compacting the conversation"
    : activity?.tool !== undefined ? toolLabel(activity.tool)
    : activity?.subagent !== undefined ? named("running subagent", activity.subagent.name, "running a subagent")
    : rest
}

function toolLabel({name, file}: {name?: string; file?: string}): string {
  const verb = fileVerbs.get(name)
  if (verb !== undefined) return named(verb, file, `${verb} a file`)
  return toolLabels.get(name) ?? named("using", name?.replace(/^mcp__.+?__(?=.)/, ""), "using a tool")
}

function named(text: string, name: string | undefined, fallback: string): string {
  return name === undefined ? fallback : `${text} \`${name}\``
}
