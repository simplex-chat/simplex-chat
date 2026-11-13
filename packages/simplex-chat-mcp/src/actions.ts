import {errorText, log, logError, oneLine} from "./log"
import {Changes, Collected, FileOffer, FileType, ToolCall, nextChange, reportProgress, utcMinutes} from "./messages"
import type {ConnectInput, DownloadInput, LeaveInput, PlanSummary, SendInput} from "./tools"

// spec: spec/architecture.md#actions-mode
export type Mode = "requests" | "host"
// spec: spec/architecture.md#actions-state
export type State = "pending" | "performing" | "done" | "failed" | "rejected" | "expired"
// spec: spec/architecture.md#actions-standing
export type Standing = "messages" | "files"

export const maxPending = 20
const maxPerforming = 20
const expiryCheck = 60_000

export const lostMessage = "The agent lost its control chat. The person who runs it has to restart it with --reset-owners."
export const stoppingMessage = "The server is stopping."
const pendingMessage = `${maxPending} actions are pending; wait for the owner's decisions.`
const performingMessage = `${maxPerforming} actions are being performed; wait for their results.`

// spec: spec/architecture.md#actions-toolerror
export class ToolError extends Error {}

// spec: spec/architecture.md#actions-hosttexts
export interface HostTexts {
  lost: string
  stopping: string
  pending: string
}

// spec: spec/architecture.md#actions-subject
export interface Subject {
  kind: string
  name?: string
  simplexName?: string
  incognito: boolean
}

// spec: spec/architecture.md#actions-connref
export interface ConnRef {
  pcc?: number
  contact?: number
  group?: number
}

// spec: spec/architecture.md#actions-actionrecord
interface ActionRecord {
  readonly n: number
  readonly session?: number
  readonly mode: Mode
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
  ended?: boolean
  error?: string
  stopCollecting?: () => void
  cardImage?: string
}

// spec: spec/architecture.md#actions-connectaction
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

export interface Attachment {
  readonly path: string
  readonly name: string
  readonly size: number
  readonly inode: number
  readonly changedAt: number
  readonly type: FileType
  readonly resize: boolean
  readonly preview?: string
  readonly duration?: number
}

// spec: spec/architecture.md#actions-connectaction
export interface SendAction extends ActionRecord {
  readonly tool: "send"
  readonly input: SendInput
  readonly subject: Subject
  readonly chat: string
  readonly file?: Attachment
  readonly replyTo?: {from: string; text: string}
  readonly mentions: {name: string; member: number}[]
  readonly responses: Collected
  item?: number
  sentAt?: number
}

// spec: spec/architecture.md#actions-connectaction
export interface LeaveAction extends ActionRecord {
  readonly tool: "leave"
  readonly input: LeaveInput
  readonly subject: Subject
  readonly chat: string
}

// spec: spec/architecture.md#actions-connectaction
export interface DownloadAction extends ActionRecord {
  readonly tool: "download"
  readonly input: DownloadInput
  readonly subject: Subject
  readonly chat: string
  readonly item: number
  readonly from?: string
  readonly file: FileOffer
  readonly unknownServers?: string[]
  startedAt?: number
  path?: string
}

// spec: spec/architecture.md#actions-modeaction
export interface ModeAction extends ActionRecord {
  readonly tool: "mode"
  readonly input: {mode: Mode}
  readonly subject?: undefined
  readonly chat?: undefined
}

// spec: spec/architecture.md#actions-modeaction
export interface PermissionAction extends ActionRecord {
  readonly tool: "permission"
  readonly input: {requestId: string; toolName: string; description: string; inputPreview: string}
  readonly subject?: undefined
  readonly chat?: undefined
  previewBytes?: number
}

export type Action = ConnectAction | SendAction | LeaveAction | DownloadAction | ModeAction | PermissionAction

export type NewAction = Omit<ConnectAction, keyof ActionRecord> | Omit<SendAction, keyof ActionRecord> | Omit<LeaveAction, keyof ActionRecord> | Omit<DownloadAction, keyof ActionRecord> | Omit<ModeAction, keyof ActionRecord> | Omit<PermissionAction, keyof ActionRecord>

// spec: spec/architecture.md#actions-actionhooks
export interface ActionHooks {
  refusal(action: Action): string | undefined
  saveCounter(next: number): Promise<void>
  postCard(action: Action): Promise<number>
  perform(action: Action, signal: AbortSignal): Promise<void>
  outcome(action: Action): Promise<void>
}

// spec: spec/architecture.md#actions-actions
export class Actions {
  lost = false
  private stopping = false
  private readonly abortPerforms = new AbortController()
  private readonly actions = new Map<number, Action>()
  private readonly expiries = new Map<number, NodeJS.Timeout>()
  private readonly posting = new Map<number, Promise<boolean>>()
  private readonly outcomes = new Set<Promise<void>>()

  constructor(
    public mode: Mode,
    public expireHours: number,
    public next: number,
    private readonly hooks: ActionHooks,
    private readonly texts: HostTexts = {lost: lostMessage, stopping: stoppingMessage, pending: pendingMessage},
  ) {}

  get(n: number): Action | undefined {
    return this.actions.get(n)
  }

  all(): Action[] {
    return [...this.actions.values()]
  }

  pending(): Action[] {
    return this.all().filter(({state}) => state === "pending")
  }

  // spec: spec/api.md#common-rules
  find(key: string): Action | undefined {
    return this.all().find(action => action.key === key)
  }

  // spec: spec/architecture.md#actions-actions-create
  create(key: string, fields: NewAction, approvedBy?: string, session?: number, mode: Mode = this.mode): Action {
    const n = this.next
    const created = Date.now()
    const standing = approvedBy !== undefined
    const record: ActionRecord = {
      n,
      session,
      mode,
      key,
      created,
      expires: created + this.expireHours * 3_600_000,
      changes: new Changes(),
      standing,
      state: mode === "host" || standing ? "performing" : "pending",
      decidedBy: approvedBy,
      open: 0,
      interrupted: false,
    }
    const action = {...fields, ...record} as Action
    const ofSession = this.ofSession(session)
    const refusal = this.stopping ? this.texts.stopping
      : this.lost ? this.texts.lost
      : ofSession.filter(({state}) => state === "pending").length >= maxPending ? this.texts.pending
      : ofSession.filter(({state}) => state === "performing").length >= maxPerforming ? performingMessage
      : this.hooks.refusal(action)
    if (refusal !== undefined) throw new ToolError(refusal)
    this.next++
    this.actions.set(n, action)
    this.scheduleExpiry(action)
    const {what, detail} = summary(action, oneLine)
    log(`Action ${n}${session === undefined ? "" : ` of session ${session}`}: ${what}${detail}`)
    const posting = this.post(action)
    this.posting.set(n, posting)
    posting.then(posted => this.afterPosting(action, posted)).catch(error => logError(`action ${n}`, error))
    return action
  }

  // spec: spec/architecture.md#actions-actions-ofsession
  ofSession(session?: number): Action[] {
    return this.all().filter(action => action.session === session)
  }

  // spec: spec/architecture.md#actions-actions-ofsession
  remove(action: Action): void {
    if (!this.actions.delete(action.n)) return
    clearTimeout(this.expiries.get(action.n))
    this.expiries.delete(action.n)
    action.stopCollecting?.()
  }

  // spec: spec/architecture.md#actions-actions-ofsession
  openCall(action: Action): void {
    action.open++
  }

  // spec: spec/architecture.md#actions-actions-ofsession
  closeCall(action: Action): void {
    action.open--
    this.forgetExpired(action)
  }

  // spec: spec/architecture.md#actions-actions-ofsession
  end(action: Action): void {
    action.ended = true
    this.decide(action, "expired")
    this.updateCard(action)
  }

  // spec: spec/architecture.md#actions-actions-expiredue
  expireDue(): void {
    if (this.stopping) return
    const now = Date.now()
    for (const action of this.all().filter(({expires}) => expires <= now)) this.expire(action)
  }

  // spec: spec/architecture.md#actions-actions-decided
  async decided(action: Action, call: ToolCall, until: () => number = () => Infinity): Promise<boolean> {
    if (isDecided(action.state)) return true
    const stopProgress = reportProgress(call, () => progressText(action))
    try {
      while (!isDecided(action.state) && Date.now() < until()) {
        const left = until() - Date.now()
        if (!(await nextChange(action.changes, call.signal, left === Infinity ? undefined : left))) return false
      }
      return true
    } finally {
      stopProgress()
    }
  }

  // spec: spec/api.md#cards
  approve(action: Action, approver: string): void {
    action.decidedBy = approver
    this.decide(action, "performing")
    this.runPerform(action).catch(error => logError(`action ${action.n}`, error))
  }

  // spec: spec/api.md#terminal-lines
  reject(action: Action, approver: string): void {
    action.decidedBy = approver
    this.decide(action, "rejected")
    log(`Action ${action.n} rejected by ${approver}.`)
    this.updateCard(action)
  }

  loseControl(): void {
    this.lost = true
    for (const action of this.pending()) {
      action.error = this.texts.lost
      this.decide(action, "failed")
    }
  }

  // spec: spec/architecture.md#actions-actions-stop
  async stop(): Promise<Action[]> {
    this.stopping = true
    for (const timer of this.expiries.values()) clearTimeout(timer)
    this.expiries.clear()
    await Promise.all(this.posting.values())
    const stopped = this.all().filter(({state}) => state === "pending" || state === "performing")
    for (const action of stopped) action.interrupted = true
    this.abortPerforms.abort()
    if (!this.lost) await Promise.all(stopped.map(action => this.hooks.outcome(action).catch(error => logError(`action ${action.n} card`, error))))
    await Promise.allSettled(this.outcomes)
    return stopped
  }

  // spec: spec/architecture.md#actions-actions-scheduleexpiry
  private scheduleExpiry(action: Action, delay = action.expires - Date.now()): void {
    const timer = setTimeout(() => this.expire(action), Math.min(delay, expiryCheck))
    this.expiries.set(action.n, timer.unref())
  }

  // spec: spec/architecture.md#actions-actions-scheduleexpiry
  private expire(action: Action): void {
    clearTimeout(this.expiries.get(action.n))
    this.expiries.delete(action.n)
    if (action.expires > Date.now()) return this.scheduleExpiry(action)
    if (action.state !== "pending") return this.forgetExpired(action)
    const kept = action.session === undefined
    if (kept) this.scheduleExpiry(action, expiryCheck)
    this.decide(action, "expired", !kept)
    log(`Action ${action.n} expired.`)
    this.updateCard(action)
  }

  // spec: spec/architecture.md#actions-actions-scheduleexpiry
  private forgetExpired(action: Action): void {
    if (isDecided(action.state) && action.open === 0 && action.expires <= Date.now()) this.remove(action)
  }

  // spec: spec/api.md#terminal-lines
  private async post(action: Action): Promise<boolean> {
    try {
      await this.hooks.saveCounter(this.next)
      if (action.state === "pending") action.card = await this.hooks.postCard(action)
      return true
    } catch (error) {
      action.error = errorText(error)
      this.decide(action, "failed")
      log(sentence(action, `Action ${action.n}: failed: ${oneLine(action.error)}`))
      return false
    }
  }

  private async afterPosting(action: Action, posted: boolean): Promise<void> {
    this.posting.delete(action.n)
    if (!posted || action.state !== "performing") return
    if (this.stopping) this.decide(action, "pending")
    else await this.runPerform(action)
  }

  // spec: spec/api.md#cards
  private async runPerform(action: Action): Promise<void> {
    try {
      await this.hooks.perform(action, this.abortPerforms.signal)
      this.decide(action, "done")
    } catch (error) {
      action.error = errorText(error)
      this.decide(action, "failed")
    }
    if (action.interrupted) return
    log(`Action ${action.n}${action.mode === "host" ? ":" : ` approved by ${action.decidedBy}:`} ${performedText(action)}`)
    if (!this.lost) await this.postOutcome(action)
  }

  private decide(action: Action, state: State, forget = true): void {
    action.state = state
    action.changes.notify()
    if (forget) this.forgetExpired(action)
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

export function hasUnknownServers({unknownServers}: Pick<DownloadAction, "unknownServers">): boolean {
  return unknownServers !== undefined && unknownServers.length > 0
}

export function isDecided(state: State): boolean {
  return state === "done" || state === "failed" || state === "rejected" || state === "expired"
}

// spec: spec/api.md#cards
export function subjectText({kind, name, simplexName}: Subject, showName: (name: string) => string): string {
  const named = name === undefined ? `an unnamed ${kind}` : `${kind} ${showName(name)}`
  return simplexName === undefined ? named : `${named} (${simplexName})`
}

// spec: spec/architecture.md#actions-summary
export function summary(action: Action, showName: (name: string) => string): {what: string; detail: string} {
  if (action.tool === "mode") return {what: `run in mode ${action.input.mode}`, detail: ""}
  if (action.tool === "permission") return {what: `use ${showName(action.input.toolName)}`, detail: ""}
  const target = subjectText({...action.subject, simplexName: undefined}, showName)
  switch (action.tool) {
    case "connect":
      return {what: `connect to ${target}`, detail: `, ${action.subject.incognito ? "incognito" : "as itself"}`}
    case "send":
      return {what: `send to ${target}`, detail: ` (${action.chat})`}
    case "leave":
      return {what: `${leaveVerbs(action).present} ${target}`, detail: ` (${action.chat})`}
    case "download":
      return {what: `download ${showName(action.file.name)} from ${target}`, detail: ` (${action.chat})`}
  }
}

export function leaveVerbs({chat}: Pick<LeaveAction, "chat">): {present: string; past: string} {
  return chat.startsWith("@") ? {present: "delete", past: "deleted"} : {present: "leave", past: "left"}
}

// spec: spec/architecture.md#actions-summary
function performedText(action: Action): string {
  const failure = action.state === "failed" ? `; failed: ${oneLine(action.error ?? "")}` : ""
  if (action.mode === "host") return sentence(action, `performed without approval (mode host)${failure}`)
  if (failure !== "") return sentence(action, `failed: ${oneLine(action.error ?? "")}`)
  if (action.tool === "send") return "sent."
  if (action.tool === "leave") return `${leaveVerbs(action).past}.`
  if (action.tool === "download") return "downloaded."
  if (action.tool === "mode") return `mode ${action.input.mode}.`
  if (action.tool === "permission") return "allowed."
  return action.connected ? "connected." : "connection requested."
}

// spec: spec/architecture.md#actions-summary
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

// spec: spec/architecture.md#actions-summary
function sentence({session}: Action, text: string): string {
  return session !== undefined && text.endsWith(".") ? text : `${text}.`
}
