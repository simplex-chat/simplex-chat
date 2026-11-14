import {basename} from "path"
import {fieldOf, textOf} from "./inference"

// spec: spec/architecture.md#activity-running
interface Running {
  session: string
  agent?: string
  name?: string
  file?: string
}

// spec: spec/architecture.md#activity-activitystate
export interface ActivityState {
  asking: boolean
  compacting: boolean
  tool?: Running
  subagent?: {session: string; name?: string}
}

// spec: spec/architecture.md#activity-constants
const maxSessions = 16
// spec: spec/architecture.md#activity-constants
const maxEntries = 64
// spec: spec/architecture.md#activity-constants
const maxEnded = 256
// spec: spec/architecture.md#activity-constants
const namePattern = /^[\w-]{1,64}$/
// spec: spec/architecture.md#activity-constants
const filePattern = /^[\w.+@-]{1,64}$/

// spec: spec/architecture.md#activity-activity
export class Activity {
  private readonly sessions = new Set<string>()
  private readonly tools = new Map<string, Running>()
  private readonly subagents = new Map<string, {session: string; name?: string}>()
  private readonly ended = new Set<string>()
  private readonly asking = new Set<string>()
  private readonly compacting = new Set<string>()
  private readonly idle = new Set<string>()

  constructor(private readonly name: string, private readonly turnsEnded: () => void) {}

  // spec: spec/architecture.md#activity-activity-record
  record(event: unknown): void {
    const session = textOf(event, "session_id")
    const tool = textOf(event, "tool_name", namePattern)
    const id = textOf(event, "tool_use_id")
    const agent = textOf(event, "agent_id")
    const own = tool?.startsWith(`mcp__simplex-${this.name}__`) ?? false
    if (session === undefined) return
    if (own) this.join(session)
    if (!this.sessions.has(session)) return
    switch (textOf(event, "hook_event_name")) {
      case "PreToolUse":
        if (agent === undefined) this.compacting.delete(session)
        if (id === undefined || own || tool === "Agent" || this.ended.has(id)) return
        this.tools.set(id, {session, agent, name: tool, file: fileName(fieldOf(event, "tool_input"))})
        return keepNewest(this.tools, maxEntries)
      case "PostToolUse":
      case "PostToolUseFailure":
      case "PermissionDenied":
        this.asking.delete(session)
        if (id === undefined || this.tools.delete(id)) return
        this.ended.add(id)
        return keepNewest(this.ended, maxEnded)
      case "PostToolBatch":
        this.asking.delete(session)
        return deleteWhere(this.tools, running => running.session === session && running.agent === agent)
      case "PermissionRequest":
        this.asking.add(session)
        return
      case "SubagentStart":
        if (agent === undefined) return
        this.subagents.set(agent, {session, name: textOf(event, "agent_type", namePattern)})
        return keepNewest(this.subagents, maxEntries)
      case "SubagentStop":
        if (agent !== undefined) this.subagents.delete(agent)
        return
      case "PreCompact":
        this.compacting.add(session)
        return
      case "PostCompact":
        this.compacting.delete(session)
        return
      case "UserPromptSubmit":
        return this.clear(session)
      case "Notification":
        if (textOf(event, "notification_type") === "idle_prompt") this.endTurn(session)
        return
      case "Stop":
      case "StopFailure":
      case "SessionEnd":
        return this.endTurn(session)
    }
  }

  // spec: spec/architecture.md#activity-activity-state
  state(): ActivityState {
    return {asking: this.asking.size > 0, compacting: this.compacting.size > 0, tool: [...this.tools.values()].at(-1), subagent: [...this.subagents.values()].at(-1)}
  }

  // spec: spec/architecture.md#activity-activity-endturns
  endTurns(): void {
    for (const session of this.sessions) this.endTurn(session)
  }

  // spec: spec/architecture.md#activity-activity-join
  private join(session: string): void {
    this.sessions.add(session)
    if (this.sessions.size <= maxSessions) return
    const [oldest] = this.sessions
    this.sessions.delete(oldest)
    this.clear(oldest)
  }

  // spec: spec/architecture.md#activity-activity-join
  private clear(session: string): void {
    deleteWhere(this.tools, tool => tool.session === session)
    deleteWhere(this.subagents, subagent => subagent.session === session)
    this.asking.delete(session)
    this.compacting.delete(session)
    this.idle.delete(session)
  }

  // spec: spec/architecture.md#activity-activity-join
  private endTurn(session: string): void {
    this.clear(session)
    this.idle.add(session)
    if ([...this.sessions].every(followed => this.idle.has(followed))) this.turnsEnded()
  }
}

// spec: spec/architecture.md#activity-filename
function fileName(input: unknown): string | undefined {
  const name = basename(textOf(input, "file_path") ?? textOf(input, "notebook_path") ?? "")
  return filePattern.test(name) ? name : undefined
}

// spec: spec/architecture.md#activity-filename
function keepNewest(keys: Set<string> | Map<string, unknown>, max: number): void {
  const [oldest] = keys.keys()
  if (keys.size > max) keys.delete(oldest)
}

// spec: spec/architecture.md#activity-filename
function deleteWhere<E>(entries: Map<string, E>, matches: (entry: E) => boolean): void {
  for (const [id, entry] of entries) if (matches(entry)) entries.delete(id)
}
