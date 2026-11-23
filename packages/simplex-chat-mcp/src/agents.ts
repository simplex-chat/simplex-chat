import {join} from "path"
import {AppendedLines} from "./claude-code"
import type {AgentStart, Step, TaskEnd, TaskStatus, WorkflowRun} from "./protocol"

// spec: spec/architecture.md#agents-agents
const stallMs = 120_000
// spec: spec/architecture.md#agents-agents
const stallCallMs = 600_000
// spec: spec/architecture.md#agents-agents
const maxEnded = 100
// spec: spec/architecture.md#agents-agenttext
const statusEmoji: {[status in TaskStatus]: string} = {completed: "✅", failed: "❌", killed: "⏹️", stopped: "⏹️"}

// spec: spec/architecture.md#agents-agents
export interface Live {
  item?: Promise<number | undefined>
  sent: string
  sentAt: number
  postedAt: number
  end?: End
}

// spec: spec/architecture.md#agents-agents
export interface End {
  status: TaskStatus
  at: number
  tokens?: number
  steps?: number
  ms?: number
}

// spec: spec/architecture.md#agents-agents
export interface Agent extends Live {
  kind: "agent"
  id: string
  type: string
  description: string
  toolUse?: string
  transcript: string
  startedAt: number
  lastEventAt: number
  steps: number
  open: Map<string, string>
  last?: string
  tokens?: number
  result?: string
}

// spec: spec/architecture.md#agents-agents
export interface JournalEntry {
  state: "started" | "result" | "failed"
  agentId: string
  phase: string
}

// spec: spec/architecture.md#agents-agents
export interface Workflow extends Live {
  kind: "workflow"
  toolUse: string
  name: string
  folder: string
  startedAt: number
  journal: Map<string, JournalEntry>
  phases: string[]
  tokens: Map<string, number>
  counted: Set<string>
  lines: AppendedLines
}

// spec: spec/architecture.md#agents-agents
export type Card = Agent | Workflow

// spec: spec/architecture.md#agents-agents
export class Agents {
  private cards: Card[] = []

  all(): Card[] {
    return this.cards
  }

  live(): Card[] {
    return this.cards.filter(({end}) => end === undefined)
  }

  start(id: string, {type, description, toolUse, transcript}: AgentStart, now: number): Agent {
    const agent: Agent = {kind: "agent", id, type, description, toolUse, transcript, startedAt: now, lastEventAt: now, steps: 0, open: new Map(), sent: "", sentAt: -Infinity, postedAt: now}
    this.add(agent)
    return agent
  }

  workflow({toolUse, name, folder}: WorkflowRun, now: number): Workflow {
    const workflow: Workflow = {kind: "workflow", toolUse, name, folder, startedAt: now, journal: new Map(), phases: [], tokens: new Map(), counted: new Set(), lines: new AppendedLines(), sent: "", sentAt: -Infinity, postedAt: now}
    this.add(workflow)
    return workflow
  }

  step(id: string, {id: toolUse, text, end}: Step, now: number): void {
    const agent = this.agent(id)
    if (agent === undefined) return
    agent.lastEventAt = now
    if (end !== true) {
      agent.steps++
      agent.open.set(toolUse, text)
    } else if (agent.open.delete(toolUse)) agent.last = text
  }

  report(id: string, text: string): void {
    const agent = this.agent(id)
    if (agent !== undefined && text !== "") agent.result = text
  }

  stop(id: string, text: string, now: number): Agent | undefined {
    const agent = this.agent(id)
    if (agent === undefined) return undefined
    if (text !== "") agent.result ??= text
    agent.end = {status: "completed", at: now}
    return agent
  }

  task({toolUse, status, tokens, steps, ms}: TaskEnd, now: number): Card | undefined {
    const card = [...this.cards].reverse().find(known => known.toolUse === toolUse)
    if (card !== undefined) card.end = {status, at: card.end?.at ?? now, tokens, steps, ms}
    return card
  }

  endAll(now: number): Card[] {
    const ended = this.live()
    for (const card of ended) card.end = {status: "stopped", at: now}
    return ended
  }

  private agent(id: string): Agent | undefined {
    return [...this.cards].reverse().find((card): card is Agent => card.kind === "agent" && card.id === id && card.end === undefined)
  }

  private add(card: Card): void {
    this.cards.push(card)
    const ended = this.cards.filter(({end}) => end !== undefined)
    if (ended.length > maxEnded) this.cards = this.cards.filter(known => known !== ended[0])
  }
}

// spec: spec/architecture.md#agents-applyjournal
export function applyJournal(workflow: Workflow, lines: string[]): void {
  for (const line of lines) {
    if (!line.startsWith('{"type":"started"') && !line.startsWith('{"type":"result"') && !line.startsWith('{"type":"failed"')) continue
    const {type, key, agentId, phase} = (parsed(line) ?? {}) as {type?: unknown; key?: unknown; agentId?: unknown; phase?: unknown}
    if (typeof key !== "string" || typeof agentId !== "string" || (type !== "started" && type !== "result" && type !== "failed")) continue
    const known = workflow.journal.get(key)
    const named = known?.phase ?? (typeof phase === "string" ? phase : "")
    if (!workflow.phases.includes(named)) workflow.phases.push(named)
    workflow.journal.set(key, {state: type, agentId, phase: named})
  }
}

// spec: spec/architecture.md#agents-applyjournal
export function journalPath(workflow: Workflow): string {
  return join(workflow.folder, "journal.jsonl")
}

// spec: spec/architecture.md#agents-applyjournal
export function workflowAgentTranscript(workflow: Workflow, agentId: string): string {
  return join(workflow.folder, `agent-${agentId}.jsonl`)
}

// spec: spec/architecture.md#agents-agenttext
export function cardText(card: Card, now: number): string {
  return card.kind === "agent" ? agentText(card, now) : workflowText(card, now)
}

// spec: spec/architecture.md#agents-agenttext
function agentText(agent: Agent, now: number): string {
  const title = `🤖 ${[agent.type, agent.description].filter(part => part !== "").join(" · ")}`
  const {end} = agent
  if (end !== undefined) {
    const figures = [`${statusEmoji[end.status]} ${clock(end.ms ?? end.at - agent.startedAt)}`, stepsText(end.steps ?? agent.steps), tokensText(end.tokens ?? agent.tokens)]
    return [[title, ...figures].filter(part => part !== undefined).join(" · "), agent.result].filter(line => line !== undefined).map(small).join("\n")
  }
  const stalled = stalledFor(agent, now)
  const open = [...agent.open.values()].at(-1)
  const activity = stalled !== undefined ? [`⚠️ no step for ${clock(fiveSeconds(stalled))}`, last(agent)] : open !== undefined ? [`now: ${open}`] : [last(agent)]
  const line = [stepsText(agent.steps), tokensText(agent.tokens), clock(fiveSeconds(now - agent.startedAt)), ...activity].filter(part => part !== undefined).join(" · ")
  return `${small(title)}\n${small(line)}`
}

// spec: spec/architecture.md#agents-agenttext
function workflowText(workflow: Workflow, now: number): string {
  const entries = [...workflow.journal.values()]
  const ended = workflow.end !== undefined
  const phases = workflow.phases.map((phase, i) => {
    const started = entries.filter(entry => entry.phase === phase)
    const done = started.filter(({state}) => state !== "started").length
    const complete = !ended && done === started.length && i < workflow.phases.length - 1
    return `${phase === "" ? "" : `${phase} `}${done}/${started.length}${complete ? " ✅" : ""}`
  })
  const tokens = tokensText(workflow.tokens.size === 0 ? undefined : [...workflow.tokens.values()].reduce((sum, count) => sum + count, 0))
  const {end} = workflow
  if (end !== undefined) return small([`⚙️ ${workflow.name}`, `${statusEmoji[end.status]} ${clock(end.at - workflow.startedAt)}`, ...phases, tokens].filter(part => part !== undefined).join(" · "))
  const working = entries.filter(({state}) => state === "started").length
  return small([`⚙️ ${workflow.name}`, ...phases, working === 0 ? undefined : `${working} working`, tokens, clock(fiveSeconds(now - workflow.startedAt))].filter(part => part !== undefined).join(" · "))
}

// spec: spec/architecture.md#agents-agentsline
export function agentsLine(cards: Card[], now: number): string | undefined {
  const live = cards.filter(({end}) => end === undefined)
  if (live.length === 0) return undefined
  const agents = live.filter((card): card is Agent => card.kind === "agent")
  const workflows = live.filter((card): card is Workflow => card.kind === "workflow")
  const working = agents.length + workflows.reduce((sum, {journal}) => sum + [...journal.values()].filter(({state}) => state === "started").length, 0)
  const stalls = agents.flatMap(agent => stalledFor(agent, now) ?? [])
  const workingText = working === 0 ? undefined : `${working} agent${working === 1 ? "" : "s"} working`
  const stalledText = stalls.length === 0 ? undefined : `${stalls.length} stalled`
  const phases = workflows.map(workflow => {
    const phase = workflow.phases.at(-1)
    const started = [...workflow.journal.values()].filter(entry => entry.phase === phase)
    const done = started.filter(({state}) => state !== "started").length
    return `⚙️ ${workflow.name}${phase === undefined ? "" : ` ${phase === "" ? "" : `${phase} `}${done}/${started.length}`}`
  })
  const line = [workingText === undefined ? undefined : `🤖 ${workingText}`, stalledText === undefined ? undefined : `${stalledText} ${clock(fiveSeconds(Math.max(...stalls)))}`, ...phases].filter(part => part !== undefined).join(" · ")
  return line === "" ? undefined : line
}

// spec: spec/architecture.md#agents-agents
function stalledFor(agent: Agent, now: number): number | undefined {
  const quiet = now - agent.lastEventAt
  return quiet > (agent.open.size === 0 ? stallMs : stallCallMs) ? quiet : undefined
}

function last(agent: Agent): string | undefined {
  return agent.last === undefined ? undefined : `last: ${agent.last}`
}

function stepsText(steps: number): string {
  return `${steps} step${steps === 1 ? "" : "s"}`
}

// spec: spec/architecture.md#agents-agenttext
function small(text: string): string {
  return `!- ${text.replaceAll("!", "ǃ")}!`
}

// spec: spec/architecture.md#agents-agenttext
export function tokensText(tokens: number | undefined): string | undefined {
  if (tokens === undefined) return undefined
  const [scale, unit] = tokens >= 1e6 ? [1e6, "M"] : tokens >= 1e3 ? [1e3, "k"] : [1, ""]
  const value = tokens / scale
  return `${scale === 1 ? tokens : value >= 100 ? Math.floor(value) : (Math.floor(value * 10) / 10).toFixed(1)}${unit} tokens`
}

// spec: spec/architecture.md#agents-agenttext
export function clock(ms: number): string {
  const seconds = Math.max(0, Math.floor(ms / 1_000))
  const [hours, minutes, rest] = [Math.floor(seconds / 3_600), Math.floor((seconds % 3_600) / 60), seconds % 60]
  const pad = (value: number): string => String(value).padStart(2, "0")
  return hours > 0 ? `${hours}:${pad(minutes)}:${pad(rest)}` : `${minutes}:${pad(rest)}`
}

function fiveSeconds(ms: number): number {
  return Math.floor(ms / 5_000) * 5_000
}

function parsed(line: string): unknown {
  try {
    return JSON.parse(line)
  } catch {
    return undefined
  }
}
