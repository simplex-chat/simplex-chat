import {errorText, log, logError, oneLine} from "./log"
import {Changes, Collected, ToolCall, nextChange, reportProgress, utcMinutes} from "./messages"
import type {ConnectInput, LeaveInput, PlanSummary, SendInput} from "./tools"

export type Mode = "requests" | "host"
export type State = "pending" | "performing" | "done" | "failed" | "rejected" | "expired"

const progressTick = 60_000
const expiryTick = 60_000
const maxPending = 20
const maxPerforming = 20

export const lostMessage = "The agent lost its control chat. The person who runs it has to restart it with --reset-owners."
export const stoppingMessage = "The server is stopping."
const pendingMessage = `${maxPending} actions are pending; wait for the owner's decisions.`
const performingMessage = `${maxPerforming} actions are being performed; wait for their results.`

export class ToolError extends Error {}

export interface Subject {
  kind: string
  name?: string
  simplexName?: string
  incognito: boolean
}

export interface ConnRef {
  pcc?: number
  contact?: number
  group?: number
}

interface ActionRecord {
  readonly n: number
  readonly key: string
  readonly created: number
  readonly expires: number
  readonly changes: Changes
  readonly standing: boolean
  state: State
  card?: number
  decidedBy?: string
  open: number
  interrupted: boolean
  error?: string
  stopCollecting?: () => void
}

export interface ConnectAction extends ActionRecord {
  readonly tool: "connect"
  readonly input: ConnectInput
  readonly subject: Subject
  readonly plan: PlanSummary
  readonly messages: Collected
  conn?: ConnRef
  chat?: string
  knownAs?: string
  connected?: boolean
  connectedAt?: number
  startedAt?: number
  connectUntil?: number
  linkTextBytes?: number
}

export interface SendAction extends ActionRecord {
  readonly tool: "send"
  readonly input: SendInput
  readonly subject: Subject
  readonly chat: string
  readonly replyTo?: {from: string; text: string}
  readonly mentions: {name: string; member: number}[]
  readonly responses: Collected
  item?: number
  sentAt?: number
}

export interface LeaveAction extends ActionRecord {
  readonly tool: "leave"
  readonly input: LeaveInput
  readonly subject: Subject
  readonly chat: string
}

export type Action = ConnectAction | SendAction | LeaveAction

export type NewAction = Omit<ConnectAction, keyof ActionRecord> | Omit<SendAction, keyof ActionRecord> | Omit<LeaveAction, keyof ActionRecord>

export interface ActionHooks {
  refusal(action: Action): string | undefined
  saveCounter(next: number): Promise<void>
  postCard(action: Action): Promise<number>
  perform(action: Action, signal: AbortSignal): Promise<void>
  outcome(action: Action): Promise<void>
}

export class Actions {
  lost = false
  private stopping = false
  private readonly abortPerforms = new AbortController()
  private readonly actions = new Map<number, Action>()
  private readonly expiry = setInterval(() => this.expireDue(), expiryTick).unref()
  private readonly posting = new Map<number, Promise<boolean>>()
  private readonly outcomes = new Set<Promise<void>>()

  constructor(readonly mode: Mode, readonly expireHours: number, public next: number, private readonly hooks: ActionHooks) {}

  get(n: number): Action | undefined {
    return this.actions.get(n)
  }

  all(): Action[] {
    return [...this.actions.values()]
  }

  pending(): Action[] {
    return this.all().filter(({state}) => state === "pending")
  }

  private performing(): Action[] {
    return this.all().filter(({state}) => state === "performing")
  }

  find(key: string): Action | undefined {
    return this.all().find(action => action.key === key)
  }

  create(key: string, fields: NewAction, approvedBy?: string): Action {
    const n = this.next
    const created = Date.now()
    const standing = approvedBy !== undefined
    const record: ActionRecord = {
      n,
      key,
      created,
      expires: created + this.expireHours * 3_600_000,
      changes: new Changes(),
      standing,
      state: this.mode === "host" || standing ? "performing" : "pending",
      decidedBy: approvedBy,
      open: 0,
      interrupted: false,
    }
    const action = {...fields, ...record} as Action
    const refusal = this.stopping ? stoppingMessage
      : this.lost ? lostMessage
      : this.pending().length >= maxPending ? pendingMessage
      : this.performing().length >= maxPerforming ? performingMessage
      : this.hooks.refusal(action)
    if (refusal !== undefined) throw new ToolError(refusal)
    this.next++
    this.actions.set(n, action)
    const {what, detail} = summary(action, oneLine)
    log(`Action ${n}: ${what}${detail}`)
    const posting = this.post(action)
    this.posting.set(n, posting)
    posting.then(posted => this.afterPosting(action, posted)).catch(error => logError(`action ${n}`, error))
    return action
  }

  remove(action: Action): void {
    this.actions.delete(action.n)
    action.stopCollecting?.()
  }

  expireDue(): void {
    const now = Date.now()
    for (const action of this.all().filter(({expires}) => expires <= now)) {
      if (action.state === "pending") {
        this.decide(action, "expired")
        log(`Action ${action.n} expired.`)
        this.updateCard(action)
      } else if (isDecided(action.state) && action.open === 0) {
        this.remove(action)
      }
    }
  }

  async decided(action: Action, call: ToolCall): Promise<boolean> {
    if (isDecided(action.state)) return true
    const stopProgress = reportProgress(call, () => progressText(action))
    try {
      while (!isDecided(action.state)) {
        if (!(await nextChange(action.changes, call.signal, progressTick))) return false
      }
      return true
    } finally {
      stopProgress()
    }
  }

  approve(action: Action, approver: string): void {
    action.decidedBy = approver
    this.decide(action, "performing")
    this.runPerform(action).catch(error => logError(`action ${action.n}`, error))
  }

  reject(action: Action, approver: string): void {
    action.decidedBy = approver
    this.decide(action, "rejected")
    log(`Action ${action.n} rejected by ${approver}.`)
    this.updateCard(action)
  }

  loseControl(): void {
    this.lost = true
    for (const action of this.pending()) {
      action.error = lostMessage
      this.decide(action, "failed")
    }
  }

  async stop(): Promise<number> {
    this.stopping = true
    clearInterval(this.expiry)
    await Promise.all(this.posting.values())
    const stopped = this.all().filter(({state}) => state === "pending" || state === "performing")
    for (const action of stopped) action.interrupted = true
    this.abortPerforms.abort()
    if (!this.lost) await Promise.all(stopped.map(action => this.hooks.outcome(action).catch(error => logError(`action ${action.n} card`, error))))
    await Promise.allSettled(this.outcomes)
    return stopped.length
  }

  private async post(action: Action): Promise<boolean> {
    try {
      await this.hooks.saveCounter(this.next)
      if (action.state === "pending") action.card = await this.hooks.postCard(action)
      return true
    } catch (error) {
      action.error = errorText(error)
      this.decide(action, "failed")
      log(`Action ${action.n}: failed: ${oneLine(action.error)}.`)
      return false
    }
  }

  private async afterPosting(action: Action, posted: boolean): Promise<void> {
    this.posting.delete(action.n)
    if (!posted || action.state !== "performing") return
    if (this.stopping) action.state = "pending"
    else await this.runPerform(action)
  }

  private async runPerform(action: Action): Promise<void> {
    try {
      await this.hooks.perform(action, this.abortPerforms.signal)
      this.decide(action, "done")
    } catch (error) {
      action.error = errorText(error)
      this.decide(action, "failed")
    }
    if (action.interrupted) return
    log(`Action ${action.n}${this.mode === "host" ? ":" : ` approved by ${action.decidedBy}:`} ${performedText(action, this.mode)}`)
    if (!this.lost) await this.postOutcome(action)
  }

  private decide(action: Action, state: State): void {
    action.state = state
    action.changes.notify()
  }

  private updateCard(action: Action): void {
    if (this.lost) return
    this.postOutcome(action).catch(error => logError(`action ${action.n} card`, error))
  }

  private postOutcome(action: Action): Promise<void> {
    const outcome = this.hooks.outcome(action)
    this.outcomes.add(outcome)
    return outcome.finally(() => this.outcomes.delete(outcome))
  }
}

export function isDecided(state: State): boolean {
  return state === "done" || state === "failed" || state === "rejected" || state === "expired"
}

export function subjectText({kind, name, simplexName}: Subject, showName: (name: string) => string): string {
  const named = name === undefined ? `an unnamed ${kind}` : `${kind} ${showName(name)}`
  return simplexName === undefined ? named : `${named} (${simplexName})`
}

export function summary(action: Action, showName: (name: string) => string): {what: string; detail: string} {
  const target = subjectText({...action.subject, simplexName: undefined}, showName)
  switch (action.tool) {
    case "connect":
      return {what: `connect to ${target}`, detail: `, ${action.subject.incognito ? "incognito" : "as itself"}`}
    case "send":
      return {what: `send to ${target}`, detail: ` (${action.chat})`}
    case "leave":
      return {what: `${leaveVerbs(action).present} ${target}`, detail: ` (${action.chat})`}
  }
}

export function leaveVerbs({chat}: LeaveAction): {present: string; past: string} {
  return chat.startsWith("@") ? {present: "delete", past: "deleted"} : {present: "leave", past: "left"}
}

function performedText(action: Action, mode: Mode): string {
  const failure = action.state === "failed" ? `; failed: ${oneLine(action.error ?? "")}` : ""
  if (mode === "host") return `performed without approval (mode host)${failure}.`
  if (failure !== "") return `failed: ${oneLine(action.error ?? "")}.`
  if (action.tool === "send") return "sent."
  if (action.tool === "leave") return `${leaveVerbs(action).past}.`
  return action.connected ? "connected." : "connection requested."
}

function progressText(action: Action): string {
  switch (action.state) {
    case "pending":
      return `Action ${action.n} waits for an approver until ${utcMinutes(action.expires)}.`
    case "performing":
      return action.tool === "connect" && action.connectUntil !== undefined
        ? `Action ${action.n} waits for the connection until ${utcMinutes(action.connectUntil)}.`
        : `Action ${action.n} is being performed.`
    default:
      return `Action ${action.n} is ${action.state}.`
  }
}
