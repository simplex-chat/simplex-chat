import {constants, mkdirSync, rmSync} from "fs"
import {open} from "fs/promises"
import {dirname, join} from "path"
import {CC, CEvt, ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Action, Actions, HostTexts, Mode, isDecided, maxPending} from "./actions"
import {Config} from "./config"
import {Control, canSend, controlCommands, controlDataOf, createGroup, isControlEvent, isGroupMessage, parseCommand, rightsText, safeName, sessionApproverRoles} from "./control"
import {openCore} from "./core"
import {SessionFile, hubFolder, readSessionFile, sessionFolder, writeSessionFile} from "./files"
import {chatErrorOf, errorText, log, logError, oneLine} from "./log"
import {MainGroup, SessionGroups, createMainGroup, findMainGroup} from "./main-group"
import {Changes, ChatItemsWatch, ReplyTo, ToolCall, duration, getChat, nextChange, quoteLine, storedName, toReplyTo, validName} from "./messages"
import {AttachLine, LimitWindow, Limits, OwnerState, PermissionRequest, ResetReply, Step, TextBatch, TurnRequest} from "./protocol"
import {waitForCancelledCards} from "./serve"
import {limitParts} from "./status-line"
import {AgentTools, ToolName, featureOff, fitsMessage, maxMessageLength, ownerOf, refusedFeature} from "./tools"
import {User, userContact} from "./user"

// spec: spec/architecture.md#hub-constants
const profileName = "Claude"
// spec: spec/architecture.md#hub-constants
const sessionKey = "simplexMcpSession"
// spec: spec/architecture.md#hub-constants
const firstAction = 1
// spec: spec/architecture.md#hub-constants
const cardsWait = 10
// spec: spec/architecture.md#hub-constants
const promptEcho = 60_000
// spec: spec/architecture.md#hub-constants
const maxPushed = 20
// spec: spec/architecture.md#hub-constants
const fileWait = 10 * 60_000
// spec: spec/architecture.md#hub-constants
const pendingDays = 1
// spec: spec/architecture.md#hub-constants
const attachWait = 30_000
// spec: spec/architecture.md#hub-constants
const channelPrompt = /<channel source="plugin:simplex:simplex"[^\n]* item="(\d+)"[^\n]*>\n([\s\S]*?)\n<\/channel>/g
// spec: spec/api.md#session-groups
const commandsApproversOnly = "Only admins and owners of this group can send / commands."
// spec: spec/architecture.md#hub-constants
const reactionEmoji = ["👍", "👎", "😀", "😂", "😢", "❤", "🚀", "✅"]
// spec: spec/architecture.md#hub-constants
const unsupportedReaction = "This reaction is not supported."
const noteRefused = "Claude's note was not posted"
const maxFinished = 100
// spec: spec/api.md#session-groups
const liveLineMs = 23 * 3_600_000
// spec: spec/api.md#session-groups
const stepCheckMs = 60_000
// spec: spec/architecture.md#hub-constants
const textUpdateMs = 3_000
// spec: spec/architecture.md#hub-constants
const textEndMs = 10_000
// spec: spec/architecture.md#hub-constants
const maxRecentTexts = 20
// spec: spec/architecture.md#hub-constants
const stoppedLine = "⏹️ Stopped"
// spec: spec/api.md#list-of-sessions
const listCheckMs = 2_000
// spec: spec/api.md#list-of-sessions
const recordReadMs = 1_000
// spec: spec/api.md#list-of-sessions
const claudeStates = new Map<string, [string, string]>([
  ["waiting", ["🔵", "waiting for you in Claude Code"]],
  ["busy", ["🟠", "working"]],
  ["idle", ["🟢", "idle"]],
  ["shell", ["🟢", "idle"]],
])
// spec: spec/api.md#list-of-sessions
const unknownState: [string, string] = ["▶️", "running"]

// spec: spec/architecture.md#hub-hubtexts
export const hubTexts: HostTexts = {lost: "The group of this session is lost. Start a new Claude Code session.", stopping: "The SimpleX service is stopping.", pending: `${maxPending} actions are pending; wait for the approvers' decisions.`}

export interface StoredSession {
  id: string
  session: SessionFile
}

// spec: spec/architecture.md#hub-sessiongroupdata
interface SessionGroupData {
  session: number
  pending?: {connId: number; at: number}[]
  mode?: Mode
}

interface Part {
  text: string
  priority?: number
}

interface LiveStep {
  at: number
  text: string
  item: Promise<number | undefined>
  sentAt: number
}

interface LiveText {
  id: string
  at: number
  batches: string[]
  next: number
  last?: number
  done: string
  part: string
  item?: Promise<number | undefined>
  sent: string
  sentAt: number
  liveAt?: number
  timer?: NodeJS.Timeout
  refused?: true
  complete?: true
}

interface Opening {
  at: number
  until?: number
  item: Promise<number | undefined>
  sentAt: number
}

interface LineKind {
  refused: string
  log: string
}

const stepLines: LineKind = {refused: noteRefused, log: "steps"}
const textLines: LineKind = {refused: "Claude's text was not posted", log: "texts"}
const answerLines: LineKind = {refused: "Claude's answer was not posted", log: "answers"}

// spec: spec/architecture.md#hub-session
export class Session {
  open = false
  record?: string
  readonly actions: Actions
  readonly tools: AgentTools
  readonly control: Control
  readonly number: number
  private name: string
  private readonly pushed: {text: string; item: T.ChatItem; at: number; quoted?: boolean}[] = []
  private readonly receiving = new Map<number, {resolve: () => void; reject: (error: Error) => void}>()
  private description: {model?: string; effort?: string}
  private readonly describedAt = {model: -Infinity, effort: -Infinity}
  private limits: Limits & {at: number} = {at: -Infinity}
  private statusLineReports = false
  private turns: {at: number; item: T.ChatItem | null; added?: true}[] = []
  private answeredAt = -Infinity
  private requested?: Mode
  private hostDeclined: boolean
  private writer?: (line: AttachLine) => void
  private sentTools = "[]"
  private readonly attaching = new Changes()
  private delivery: Promise<unknown> = Promise.resolve()
  private readonly refusedReactions = new Set<string>()
  private readonly steps = new Map<string, LiveStep>()
  private readonly finished = new Set<string>()
  private finalizing: Promise<void> = Promise.resolve()
  private expiry?: NodeJS.Timeout
  private openings: Opening[] = []
  private readonly texts = new Map<string, LiveText>()
  private readonly endedTexts = new Set<string>()
  private readonly recentTexts: LiveText[] = []
  private liveOrder: Promise<void> = Promise.resolve()
  private claudeStatus?: string
  private recordRead?: Promise<void>

  constructor(
    private readonly hub: Hub,
    readonly id: string,
    stored: SessionFile,
    group: T.GroupInfo,
    private data: SessionGroupData,
  ) {
    const {chat, profile: user, root} = hub
    const {number, name, model, effort, hostDeclined = false} = stored
    this.number = number
    this.name = name
    this.description = {model, effort}
    this.hostDeclined = hostDeclined
    this.actions = new Actions(data.mode ?? "requests", hub.expireHours, controlDataOf(group.customData)?.nextAction ?? firstAction, {
      refusal: action => this.control.refusal(action),
      saveCounter: next => this.control.saveCounter(next),
      postCard: action => this.control.postCard(action),
      outcome: action => this.control.outcome(action),
      perform: (action, signal) => this.tools.perform(action, signal),
    }, hubTexts)
    const agent = {
      chat,
      user,
      actions: this.actions,
      watch: new ChatItemsWatch(),
      controlChat: () => undefined,
      standingApprover: (chatInfo: T.ChatInfo, standing: "messages" | "files") => this.control.standingApprover(chatInfo, standing),
      refusedFile: (path: string, chatInfo: T.ChatInfo, ref: string) => this.control.refusedFile(path, chatInfo, ref),
      data: sessionFolder(root, id),
      dataRoot: root,
      owner: id,
      replyChat: `#${group.groupId}`,
      setMode: (next: Mode) => this.setMode(next),
      pending: (connId: number) => this.savePending(connId),
    }
    this.tools = new AgentTools(agent, () => this.actions.mode, number)
    const session = {
      approves: (member: T.GroupMember) => this.approves(member),
      ownerOf: (n: number) => hub.sessionId(n),
      isUser: (member: T.GroupMember) => this.isUser(member),
      toolsChanged: () => this.sendTools(),
    }
    this.control = new Control(chat, group, {displayName: group.groupProfile.displayName, session, registerHint: "", qrCode: "small"}, this.actions, this.tools)
  }

  get displayName(): string {
    return groupName(this.number, this.name)
  }

  // spec: spec/architecture.md#hub-session-online
  get onLine(): ((line: AttachLine) => void) | undefined {
    return this.writer
  }

  set onLine(writer: ((line: AttachLine) => void) | undefined) {
    this.writer = writer
    this.sentTools = "[]"
    this.sendTools()
    this.attaching.notify()
  }

  // spec: spec/architecture.md#hub-session-busy
  busy(): boolean {
    return this.actions.all().some(({tool}) => tool !== "mode")
  }

  // spec: spec/architecture.md#hub-session-readrecord
  async readRecord(): Promise<void> {
    if (!this.attached || this.record === undefined) {
      this.claudeStatus = undefined
      return
    }
    this.recordRead ??= recordStatus(this.record)
      .then(
        status => {
          this.claudeStatus = status
        },
        error => {
          if ((error as NodeJS.ErrnoException).code === "ENOENT") this.claudeStatus = undefined
        },
      )
      .finally(() => (this.recordRead = undefined))
    await withinTime(this.recordRead, recordReadMs)
  }

  // spec: spec/architecture.md#hub-session-readrecord
  stateLine(): string | undefined {
    const state = claudeStates.get(this.claudeStatus ?? "")
    const dialog = state === undefined || this.claudeStatus === "waiting"
    const cards = this.actions.pending().filter(({tool, interrupted}) => !interrupted && (dialog || tool !== "permission")).length
    if (cards > 0) return `🔵 ${this.displayName}: waiting for you, ${cards} approval${cards === 1 ? "" : "s"}`
    if (!this.attached) return undefined
    const [emoji, text] = state ?? unknownState
    return `${emoji} ${this.displayName}: ${text}`
  }

  // spec: spec/architecture.md#hub-session-readrecord
  private get attached(): boolean {
    return this.onLine !== undefined && !this.hub.stopped
  }

  call(tool: ToolName, input: unknown, call: ToolCall): ReturnType<AgentTools["call"]> {
    return this.tools.call(tool, input, call)
  }

  async rename(name: string): Promise<void> {
    this.name = name
    await this.control.rename(this.displayName)
  }

  // spec: spec/architecture.md#hub-session-requestmode
  requestMode(mode: Mode): void {
    this.requested = mode
    if (mode === "host") return this.askMode()
    for (const action of this.actions.pending()) if (action.tool === "mode") this.actions.end(action)
    if (this.hostDeclined) this.declineHost(false)
    if (this.actions.mode !== mode) this.setMode(mode)
  }

  // spec: spec/architecture.md#hub-session-requestmode
  askMode(): void {
    if (this.requested !== "host" || this.hostDeclined || this.actions.mode === "host" || !this.control.hasApprover() || this.actions.pending().some(({tool}) => tool === "mode")) return
    try {
      const action = this.actions.create("mode", {tool: "mode", input: {mode: "host"}}, undefined, this.number)
      action.changes.subscribe(() => {
        if (action.state === "rejected") this.declineHost(true)
        if (action.state === "rejected" || action.state === "expired") this.requested = undefined
      })
    } catch (error) {
      logError(`mode host of session ${this.number}`, error)
    }
  }

  // spec: spec/architecture.md#hub-session-onmessage
  onMessage(ci: T.AChatItem): void {
    const {chatDir, content, quotedItem} = ci.chatItem
    if (chatDir.type !== "groupRcv" || content.type !== "rcvMsgContent") return
    const {text} = content.msgContent
    if (this.refusesCommand(text, chatDir.groupMember)) return this.queue(() => this.post([{text: commandsApproversOnly}], ci.chatItem.meta.itemId))
    if (parseCommand(text) !== undefined) return this.control.onMessage(ci)
    const replyTo = quotedItem === undefined ? undefined : toReplyTo(ci.chatInfo, {...quotedItem, content: withoutEndLine(quotedItem.content, quotedItem.chatDir)}, {name: this.hub.profile.profile.displayName})
    this.queue(() => this.prompt(ci, text, chatDir.groupMember, ci.chatItem.file, replyTo))
  }

  // spec: spec/architecture.md#hub-session-onreaction
  onReaction({added, reaction: {chatInfo, chatReaction}}: CEvt.ChatItemReaction): void {
    const {chatDir, chatItem, reaction} = chatReaction
    const {content} = chatItem
    if (!added || chatDir.type !== "groupRcv" || !canSend(chatDir.groupMember.memberRole) || this.isUser(chatDir.groupMember) || (content.type !== "rcvMsgContent" && content.type !== "sndMsgContent")) return
    if (reaction.type !== "emoji" || !reactionEmoji.includes(reaction.emoji)) return this.refuseReaction(chatDir.groupMember, chatItem)
    const replyTo = toReplyTo(chatInfo, {itemId: chatItem.meta.itemId, chatDir: chatItem.chatDir, content: withoutEndLine(content.msgContent, chatItem.chatDir), fileName: chatItem.file?.fileName}, {name: this.hub.profile.profile.displayName})
    this.queue(() => this.prompt({chatInfo, chatItem}, reaction.emoji, chatDir.groupMember, undefined, replyTo))
  }

  // spec: spec/architecture.md#hub-session-claimpending
  claimPending(connId: number, chat: string): void {
    const recent = Date.now() - pendingDays * 86_400_000
    if (!this.data.pending?.some(entry => entry.connId === connId && entry.at > recent)) return
    this.saveData({...this.data, pending: this.data.pending.filter(entry => entry.connId !== connId)}).catch(error => logError(`session ${this.number}`, error))
    this.tools.claim(chat).catch(error => logError(`owner of ${chat}`, error))
  }

  // spec: spec/architecture.md#hub-session-permission
  permission({requestId, toolName, description, inputPreview}: PermissionRequest): void {
    const toolApproved = this.control.approvedTools().some(({tool}) => tool === toolName)
    const action = this.actions.create(`permission ${requestId}`, {tool: "permission", input: {requestId, toolName, description, inputPreview}, toolApproved}, undefined, this.number, "requests")
    const unsubscribe = action.changes.subscribe(() => {
      const behavior = action.state === "performing" ? "allow" : action.state === "rejected" ? "deny" : undefined
      if (behavior !== undefined) this.onLine?.({permission: {requestId, behavior}})
      if (!isDecided(action.state)) return
      unsubscribe()
      this.actions.remove(action)
    })
  }

  // spec: spec/architecture.md#hub-session-turn
  async turn({at, prompt, typed, note, midTurn, answer, model, switched, effort, step, text, usage, limits, statusLine}: TurnRequest): Promise<void> {
    const startsTurn = prompt !== undefined && midTurn !== true
    if (startsTurn || answer !== undefined) {
      this.endPermissions(at)
      this.endSteps(at)
    }
    if (startsTurn) this.boundOpenings(at ?? Date.now())
    const started = answer === undefined ? undefined : this.turnStart(at ?? Infinity)
    const end = answer === undefined ? undefined : endLine(usage ?? (started === undefined ? undefined : duration((at ?? Date.now()) - started)), this.limits)
    const streamed = startsTurn || answer !== undefined ? this.endTexts(at ?? Infinity, answer, started, answer?.trim() === "" ? undefined : end) : undefined
    const quoted = answer === undefined ? undefined : this.turnQuote(at ?? Infinity)
    if (answer !== undefined) this.answeredAt = Math.max(this.answeredAt, at ?? Date.now())
    const matched = prompt === undefined ? [] : this.pushedIn(prompt)
    if (prompt !== undefined && (midTurn !== true || matched.length > 0) && (at ?? Date.now()) >= this.answeredAt) this.turns.push({at: at ?? Date.now(), item: matched[0]?.item ?? null, ...(startsTurn ? {} : {added: true as const})})
    this.pushed.splice(0, this.pushed.length, ...this.pushed.filter(entry => !matched.includes(entry)))
    const previous = this.description
    const newModel = this.newer("model", answer !== undefined && previous.model?.replace(/\[1m\]$/i, "") === model ? previous.model : model, at ?? Date.now())
    if (statusLine === true) this.statusLineReports = true
    const newEffort = this.newer("effort", statusLine === true || !this.statusLineReports ? effort : undefined, at ?? Date.now())
    const switches = [
      switched === true && newModel !== undefined && newModel !== previous.model ? `Model changed to ${newModel}` : undefined,
      newEffort !== undefined && previous.effort !== undefined && newEffort !== previous.effort ? `Effort changed to ${newEffort}` : undefined,
    ].flatMap(line => (line === undefined ? [] : [{text: small(line), priority: -1}]))
    const described = (newModel !== undefined && newModel !== previous.model) || (newEffort !== undefined && newEffort !== previous.effort)
    if (described) {
      this.description = {model: newModel ?? previous.model, effort: newEffort ?? previous.effort}
      this.store(this.description)
    }
    if (limits !== undefined && (at ?? Date.now()) >= this.limits.at) this.limits = {...limits, at: at ?? Date.now()}
    try {
      if (switches.length > 0) await this.post(switches).catch(error => this.postRefusal(error, noteRefused, featureOff))
      if (text !== undefined) this.text(text, at ?? Date.now())
      if (step !== undefined) await this.step(step, at ?? Date.now())
      if (note !== undefined) await this.post([{text: note, priority: -1}]).catch(error => this.postRefusal(error, noteRefused, featureOff))
      const typedPrompt = prompt !== undefined && typed !== false && prompt.replace(channelPrompt, "").trim() !== "" && matched.length === 0 && parseCommand(prompt) === undefined
      const userPost = typedPrompt ? this.hub.user?.post(this.number, textParts(prompt)).catch(error => this.postRefusal(error, "User's prompt was not posted", notAllowedForUser)) : undefined
      if (startsTurn && (typedPrompt || matched.length > 0) && (at ?? Date.now()) >= this.answeredAt) this.openTurn(at ?? Date.now(), matched[0]?.item, userPost)
      await userPost
      if (answer !== undefined && end !== undefined) await this.answer(answer, quoted, at ?? Date.now(), (await streamed) === true, end)
    } finally {
      const {model: shown, effort: level} = this.description
      if (described) await this.control.describe([shown, level === undefined ? undefined : `effort ${level}`].filter(part => part !== undefined).join(" · "))
    }
  }

  // spec: spec/architecture.md#hub-session-endpermissions
  endPermissions(before = Infinity): void {
    for (const action of this.actions.pending()) {
      if (action.tool === "permission" && action.created < before) this.actions.end(action)
    }
  }

  // spec: spec/architecture.md#hub-session-endpermissions
  endSteps(before = Infinity): Promise<void> {
    for (const [id, step] of this.steps) {
      if (step.at < before) {
        this.remember(id)
        this.endStep(id, step, step.text)
      }
    }
    return this.finalizing
  }

  // spec: spec/architecture.md#hub-session-endtoolapprovals
  endToolApprovals(): void {
    this.control.endToolApprovals()
    this.sendTools()
  }

  // spec: spec/architecture.md#hub-session-endtoolapprovals
  sendTools(): void {
    const tools = this.control.approvedTools()
    const sent = JSON.stringify(tools)
    if (sent === this.sentTools) return
    this.sentTools = sent
    this.onLine?.({tools})
  }

  // spec: spec/architecture.md#hub-session-endpermissions
  endLive(): Promise<void> {
    this.endSteps()
    this.closeOpenings(() => true)
    return this.endTexts(Infinity).then(() => this.finalizing)
  }

  // spec: spec/architecture.md#hub-session-endpermissions
  fileDone(fileId: number, error?: string): void {
    const waiting = this.receiving.get(fileId)
    this.receiving.delete(fileId)
    if (error === undefined) waiting?.resolve()
    else waiting?.reject(new Error(error))
  }

  // spec: spec/architecture.md#hub-session-invite
  async invite(member: T.GroupMember, role: T.GroupMemberRole): Promise<void> {
    await this.hub.chat.apiAddMemberViaGroup(this.control.groupId, this.hub.main.groupId, member.groupMemberId, role)
  }

  // spec: spec/architecture.md#hub-session-invite
  async announceRights(event: ChatEvent): Promise<void> {
    if (this.actions.lost || (event.type !== "joinedGroupMember" && event.type !== "connectedToGroupMember" && event.type !== "memberRole")) return
    const {member} = event
    if (this.isUser(member)) return
    const name = safeName(member.memberProfile.displayName)
    const observer = canSend(member.memberRole) && member.memberRole !== T.GroupMemberRole.Owner ? ` You can change role to observer to stop it: /'observer #${this.control.groupId} ${member.groupMemberId}'` : ""
    await this.post([{text: `${rightsText(name, member.memberRole)}${observer}`}])
  }

  // spec: spec/architecture.md#hub-session-prompt
  private async prompt(ci: T.AChatItem, text: string, member: T.GroupMember, ciFile: T.CIFile | undefined, replyTo?: ReplyTo): Promise<void> {
    if (this.isUser(member)) return
    await this.deliver(ci, text, member, ciFile, replyTo)
  }

  // spec: spec/architecture.md#hub-session-refusereaction
  private refuseReaction({groupMemberId}: T.GroupMember, {meta: {itemId}, content}: T.ChatItem): void {
    const refused = `${groupMemberId} ${itemId}`
    if (this.refusedReactions.has(refused) || (content.type === "sndMsgContent" && content.msgContent.text === unsupportedReaction)) return
    this.refusedReactions.add(refused)
    this.queue(() => this.post([{text: unsupportedReaction}], itemId))
  }

  // spec: spec/architecture.md#hub-session-deliver
  private async deliver(ci: T.AChatItem, text: string, member: T.GroupMember, ciFile: T.CIFile | undefined, replyTo?: ReplyTo): Promise<void> {
    const left = this.hub.promptsHeldUntil - Date.now()
    if (this.onLine === undefined && left > 0) await nextChange(this.attaching, new AbortController().signal, left)
    const {chatItem} = ci
    const file = this.onLine === undefined || ciFile === undefined ? undefined : await this.receive(ciFile).catch(async error => {
      await this.post([{text: `The file of this message was not received: ${oneLine(errorText(error))}`}], chatItem.meta.itemId)
      return undefined
    })
    if (this.onLine === undefined) {
      await this.post([{text: `Session ${this.number} is not running.`}], chatItem.meta.itemId)
      return
    }
    const promptText = replyTo === undefined ? text : `${quoteLine(replyTo)}\n${text}`
    if (this.pushed.push({text: promptText, item: chatItem, at: Date.now()}) > maxPushed) this.pushed.shift()
    this.onLine({prompt: {text: promptText, from: member.memberProfile.displayName, item: chatItem.meta.itemId, ...(file === undefined ? {} : {file}), ...(replyTo === undefined ? {} : {replyTo: {item: replyTo.item, from: replyTo.from}})}})
  }

  // spec: spec/architecture.md#hub-session-deliver
  private async receive({fileId, fileName}: T.CIFile): Promise<string> {
    const path = join(sessionFolder(this.hub.root, this.id), "files", "group", storedName(fileId, fileName))
    mkdirSync(dirname(path), {recursive: true, mode: 0o700})
    const received = new Promise<void>((resolve, reject) => this.receiving.set(fileId, {resolve, reject}))
    const timer = setTimeout(() => this.fileDone(fileId, "not received in 10 minutes"), fileWait).unref()
    await this.hub.chat.apiReceiveFile(fileId, path, true).catch(error => this.fileDone(fileId, errorText(error)))
    await received.finally(() => clearTimeout(timer))
    return path
  }

  // spec: spec/api.md#session-group-profile
  private newer(field: "model" | "effort", value: string | undefined, at: number): string | undefined {
    if (value === undefined || at < this.describedAt[field]) return undefined
    this.describedAt[field] = at
    return value
  }

  // spec: spec/architecture.md#hub-session-deliver
  private turnQuote(at: number): T.ChatItem | undefined {
    const earlier = this.turns.filter(turn => turn.at < at)
    if (earlier.length === 0) {
      const oldest = this.pushed.find(entry => !entry.quoted)
      if (oldest !== undefined) oldest.quoted = true
      return oldest?.item
    }
    this.turns = this.turns.filter(turn => turn.at >= at)
    return earlier.reduce((newest, turn) => (turn.at > newest.at ? turn : newest)).item ?? undefined
  }

  // spec: spec/architecture.md#hub-session-deliver
  private turnStart(at: number): number | undefined {
    const starts = this.turns.filter(turn => turn.added !== true && turn.at <= at).map(turn => turn.at)
    return starts.length === 0 ? undefined : Math.max(...starts)
  }

  // spec: spec/architecture.md#hub-session-deliver
  private async answer(text: string, quoted: T.ChatItem | undefined, at: number, streamed: boolean, end: string): Promise<void> {
    if (text.trim() === "") {
      const opening = this.takeOpening(at)
      if (opening !== undefined) await this.finalize(opening.item, end, textLines)
    } else if (!streamed) await this.postAnswer(text, quoted, at, end)
    this.closeOpenings(({at: start, until}) => (until ?? start) <= at)
  }

  // spec: spec/architecture.md#hub-session-deliver
  private async postAnswer(text: string, quoted: T.ChatItem | undefined, at: number, end: string): Promise<void> {
    const quote = quoteOf(quoted, text)
    const parts = withLine(textParts(text, quote), end, quote).map((part, i, all) => ({text: part, priority: i === all.length - 1 ? 1 : -1}))
    const opened = await this.takeOpening(at)?.item
    const filled = opened === undefined ? undefined : await this.fill(opened, parts[0].text, false, answerLines, undefined, end, parts.length === 1 ? 1 : undefined)
    if (filled === "refused") return
    const posted = filled === "filled" ? (parts.length > 1 ? this.post(parts.slice(1)) : undefined) : this.post(parts, quote === undefined ? undefined : quoted?.meta.itemId)
    await posted?.catch(error => this.postRefusal(error, answerLines.refused, featureOff, quoted?.meta.itemId, undefined, end))
  }

  // spec: spec/architecture.md#hub-session-step
  private async step({id, text, end}: Step, at: number): Promise<void> {
    if (this.finished.delete(id)) return
    this.expireLive()
    const started = this.steps.get(id)
    if (end !== true) {
      const sentAt = this.openingFor(at)?.sentAt ?? Date.now()
      const item = this.queueLive(async () => {
        await this.endComplete()
        return this.startLine(text, at, stepLines)
      })
      this.steps.set(id, {at, text, item, sentAt})
      this.watchLive()
      await item
    } else if (started === undefined) {
      this.remember(id)
      await this.post([{text, priority: -1}]).catch(error => this.postRefusal(error, noteRefused, featureOff))
    } else {
      await this.endStep(id, started, text)
    }
  }

  // spec: spec/architecture.md#hub-session-step
  private async startLine(text: string, at: number, kind: LineKind, live = true, onRefused?: () => void): Promise<number | undefined> {
    const opened = await this.takeOpening(at)?.item
    const filled = opened === undefined ? undefined : await this.fill(opened, text, live, kind, onRefused)
    if (filled === "filled") return opened
    if (filled === "refused") return undefined
    return this.sendLine(text, kind, live, undefined, onRefused)
  }

  // spec: spec/architecture.md#hub-session-step
  private async fill(itemId: number, text: string, live: boolean, kind: LineKind, onRefused?: () => void, line?: string, priority?: number): Promise<"filled" | "refused" | "failed"> {
    try {
      await this.updateItem(itemId, text, live, priority)
      return "filled"
    } catch (error) {
      if (refusedFeature(error) === undefined) {
        logError(`${kind.log} of session ${this.number}`, error)
        void this.finalize(Promise.resolve(itemId), stoppedLine, kind)
        return "failed"
      }
      if (await this.refused(error, kind, itemId, line)) onRefused?.()
      return "refused"
    }
  }

  // spec: spec/architecture.md#hub-session-step
  private async sendLine(text: string, kind: LineKind, live: boolean, quotedItemId?: number, onRefused?: () => void): Promise<number | undefined> {
    try {
      const [sent] = await this.post([{text, priority: -1}], quotedItemId, live)
      return sent?.chatItem.meta.itemId
    } catch (error) {
      if (await this.refused(error, kind)) onRefused?.()
      return undefined
    }
  }

  // spec: spec/architecture.md#hub-session-step
  private refused(error: unknown, kind: LineKind, itemId?: number, line?: string): Promise<boolean> {
    return this.postRefusal(error, kind.refused, featureOff, undefined, itemId, line).then(
      () => true,
      failure => {
        logError(`${kind.log} of session ${this.number}`, failure)
        if (itemId !== undefined) void this.finalize(Promise.resolve(itemId), stoppedLine, kind)
        return false
      },
    )
  }

  // spec: spec/architecture.md#hub-session-openturn
  private openTurn(at: number, quoted: T.ChatItem | undefined, after?: Promise<unknown>): void {
    const quotedItemId = quoteOf(quoted, "") === undefined ? undefined : quoted?.meta.itemId
    const send = () => this.sendLine("", textLines, true, quotedItemId)
    const item = after === undefined ? send() : after.then(send, send)
    this.openings.push({at, item, sentAt: Date.now()})
    this.watchLive()
  }

  // spec: spec/architecture.md#hub-session-openturn
  private boundOpenings(at: number): void {
    for (const opening of this.openings) if (opening.until === undefined && opening.at <= at) opening.until = at
  }

  // spec: spec/architecture.md#hub-session-openturn
  private openingFor(at: number): Opening | undefined {
    return this.openings.filter(({at: start, until}) => start <= at && (until === undefined || at < until)).at(-1)
  }

  // spec: spec/architecture.md#hub-session-openturn
  private takeOpening(at: number): Opening | undefined {
    const opening = this.openingFor(at)
    if (opening !== undefined) this.openings.splice(this.openings.indexOf(opening), 1)
    return opening
  }

  // spec: spec/architecture.md#hub-session-openturn
  private closeOpenings(closes: (opening: Opening) => boolean): void {
    const closed = this.openings.filter(closes)
    this.openings = this.openings.filter(opening => !closed.includes(opening))
    for (const {item} of closed) void this.finalize(item, stoppedLine, textLines)
  }

  // spec: spec/architecture.md#hub-session-text
  private text(batch: TextBatch, at: number): void {
    const answeredAt = this.answeredAt
    void this.queueLive(() => this.addBatch(batch, at, answeredAt))
  }

  // spec: spec/architecture.md#hub-session-text
  private queueLive<R>(work: () => Promise<R>): Promise<R | undefined> {
    const done = this.liveOrder.then(work)
    this.liveOrder = done.then(
      () => undefined,
      error => logError(`live posts of session ${this.number}`, error),
    )
    return done.catch(() => undefined)
  }

  // spec: spec/architecture.md#hub-session-text
  private async addBatch({message, index, final, delta}: TextBatch, at: number, answeredAt: number): Promise<void> {
    if (this.endedTexts.has(message)) return
    const known = this.texts.get(message)
    if (known === undefined && at < answeredAt) return keepLast(this.endedTexts, message)
    if (known === undefined) await this.endComplete()
    const text = known ?? {id: message, at, batches: [], next: 0, done: "", part: "", sent: "", sentAt: 0}
    this.texts.set(message, text)
    text.batches[index] = delta
    if (final) text.last = index
    while (text.batches[text.next] !== undefined) text.part += text.batches[text.next++]
    if (text.last !== undefined && text.next > text.last) await this.completeText(text)
    else await this.showText(text)
  }

  // spec: spec/architecture.md#hub-session-text
  private async completeText(text: LiveText): Promise<void> {
    await this.showText(text)
    if (this.texts.get(text.id) !== text) return
    if (text.item === undefined) return this.endText(text, text.part)
    clearTimeout(text.timer)
    await this.updateText(text, text.part, true)
    if (this.texts.get(text.id) !== text) return
    text.complete = true
    text.timer = setTimeout(() => void this.queueLive(async () => (this.texts.get(text.id) === text ? this.endText(text, text.part) : undefined)), textEndMs).unref()
  }

  // spec: spec/architecture.md#hub-session-text
  private async endComplete(): Promise<void> {
    for (const text of [...this.texts.values()]) if (text.complete === true) await this.endText(text, text.part)
  }

  // spec: spec/architecture.md#hub-session-text
  private async showText(text: LiveText): Promise<void> {
    const parts = textParts(text.part)
    if (parts.length > 1) {
      text.part = parts.pop()!
      await this.endPart(text, parts)
      text.done += parts.join("")
      text.item = undefined
      if (!this.texts.has(text.id)) return
    }
    if (text.item === undefined && text.part.trim() !== "") {
      text.liveAt = this.openingFor(text.at)?.sentAt ?? Date.now()
      text.item = this.startLine(text.part, text.at, textLines, true, () => this.refuseText(text))
      this.watchLive()
      text.sent = text.part
      text.sentAt = Date.now()
      if ((await text.item) === undefined) this.dropText(text)
    } else if (text.item !== undefined && text.timer === undefined && text.part !== text.sent) {
      text.timer = setTimeout(() => void this.queueLive(() => this.updateText(text, text.part, true)), Math.max(0, text.sentAt + textUpdateMs - Date.now())).unref()
    }
  }

  // spec: spec/architecture.md#hub-session-text
  private async updateText(text: LiveText, value: string, live: boolean, priority?: number): Promise<boolean> {
    text.timer = undefined
    if (live && this.texts.get(text.id) !== text) return false
    const itemId = await text.item
    if (itemId === undefined) return false
    if (live && value === text.sent) return true
    try {
      await this.updateItem(itemId, value, live, priority)
      text.sent = value
      text.sentAt = Date.now()
      return true
    } catch (error) {
      if (refusedFeature(error) === undefined) {
        logError(`texts of session ${this.number}`, error)
        return false
      }
      this.refuseText(text)
      await this.refused(error, textLines)
      await this.updateItem(itemId, text.sent, false).catch(failure => logError(`texts of session ${this.number}`, failure))
      return false
    }
  }

  // spec: spec/architecture.md#hub-session-text
  private async endPart(text: LiveText, [first, ...rest]: string[], alert = false): Promise<void> {
    clearTimeout(text.timer)
    const shown = text.item === undefined ? await this.postPart(text, first) : await this.updateText(text, first, false, alert && rest.length === 0 ? 1 : undefined)
    if (shown && rest.length > 0) {
      await this.post(rest.map((part, i) => ({text: part, priority: alert && i === rest.length - 1 ? 1 : -1}))).catch(async error => {
        if (await this.refused(error, textLines)) this.refuseText(text)
      })
    }
  }

  // spec: spec/architecture.md#hub-session-text
  private async postPart(text: LiveText, value: string): Promise<boolean> {
    if (value.trim() === "") return false
    text.item = this.startLine(value, text.at, textLines, false, () => this.refuseText(text))
    return (await text.item) !== undefined
  }

  // spec: spec/architecture.md#hub-session-text
  private async endText(text: LiveText, value: string, line?: string): Promise<void> {
    this.dropText(text)
    text.part = value
    await this.endPart(text, withLine(textParts(value), line), line !== undefined)
    if (text.refused !== true && (await text.item) !== undefined) this.keepRecent(text)
  }

  // spec: spec/architecture.md#hub-session-text
  private refuseText(text: LiveText): void {
    this.dropText(text)
    if (text.refused !== true) this.keepRecent(text)
    text.refused = true
  }

  // spec: spec/architecture.md#hub-session-text
  private keepRecent(text: LiveText): void {
    if (this.recentTexts.push(text) > maxRecentTexts) this.recentTexts.shift()
  }

  // spec: spec/architecture.md#hub-session-text
  private dropText(text: LiveText): void {
    clearTimeout(text.timer)
    text.timer = undefined
    this.texts.delete(text.id)
    keepLast(this.endedTexts, text.id)
  }

  // spec: spec/architecture.md#hub-session-text
  private endTexts(before: number, answer?: string, started = -Infinity, line?: string): Promise<boolean> {
    return this.queueLive(async () => {
      const streamed = answer !== undefined && (await this.claim(answer, before, started, line))
      for (const text of [...this.texts.values()]) if (text.at < before) await this.endText(text, text.part)
      return streamed
    }).then(streamed => streamed === true)
  }

  // spec: spec/architecture.md#hub-session-text
  private async claim(answer: string, before: number, started: number, line?: string): Promise<boolean> {
    const texts = [...this.texts.values(), ...this.recentTexts].filter(({at}) => at < before && at > started)
    const newest = texts.reduce<LiveText | undefined>((found, text) => (found === undefined || text.at > found.at ? text : found), undefined)
    const full = newest === undefined ? "" : (newest.done + newest.part).trim()
    if (newest === undefined || full === "" || !answer.trim().startsWith(full)) return false
    if (newest.refused) return true
    const rest = answer.trim().slice(newest.done.trimStart().length)
    if (this.texts.has(newest.id)) await this.endText(newest, rest, line)
    else if (line !== undefined || rest !== newest.part.trim()) await this.endPart(newest, withLine(textParts(rest), line), line !== undefined)
    return true
  }

  // spec: spec/architecture.md#hub-session-text
  private async updateItem(itemId: number, text: string, live: boolean, priority?: number): Promise<T.ChatItem> {
    if (priority === undefined) return this.hub.chat.apiUpdateChatItem(T.ChatType.Group, this.control.groupId, itemId, {type: "text", text}, live)
    const updatedMessage: T.UpdatedMessage & {priority: number} = {msgContent: {type: "text", text}, mentions: {}, priority}
    const r = await this.hub.chat.sendChatCmd(CC.APIUpdateChatItem.cmdString({chatRef: {chatType: T.ChatType.Group, chatId: this.control.groupId}, chatItemId: itemId, liveMessage: live, updatedMessage}))
    if (r.type === "chatItemUpdated" || r.type === "chatItemNotChanged") return r.chatItem.chatItem
    throw new api.ChatCommandError("error updating chat item", r)
  }

  // spec: spec/architecture.md#hub-session-step
  private endStep(id: string, {item}: LiveStep, text: string): Promise<void> {
    this.steps.delete(id)
    this.watchLive()
    return this.finalize(item, text, stepLines)
  }

  // spec: spec/architecture.md#hub-session-step
  private finalize(item: Promise<number | undefined>, text: string, {log}: LineKind): Promise<void> {
    const updated = item
      .then(itemId => (itemId === undefined ? undefined : this.updateItem(itemId, text, false)))
      .then(() => undefined, error => logError(`${log} of session ${this.number}`, error))
    this.finalizing = Promise.all([this.finalizing, updated]).then(() => undefined)
    return updated
  }

  // spec: spec/architecture.md#hub-session-step
  private expireLive(): void {
    const before = Date.now() - liveLineMs
    for (const [id, step] of this.steps) {
      if (step.sentAt <= before) this.endStep(id, step, step.text)
    }
    this.closeOpenings(({sentAt}) => sentAt <= before)
    void this.queueLive(async () => {
      for (const text of [...this.texts.values()]) if (text.liveAt !== undefined && text.liveAt <= before) await this.endText(text, text.part)
      this.watchLive()
    })
  }

  // spec: spec/architecture.md#hub-session-step
  private watchLive(): void {
    if (this.steps.size > 0 || this.texts.size > 0 || this.openings.length > 0) {
      this.expiry ??= setInterval(() => this.expireLive(), stepCheckMs).unref()
    } else {
      clearInterval(this.expiry)
      this.expiry = undefined
    }
  }

  // spec: spec/architecture.md#hub-session-step
  private remember(id: string): void {
    keepLast(this.finished, id)
  }

  // spec: spec/architecture.md#hub-session-deliver
  private async postRefusal(error: unknown, subject: string, refusal: (feature: string) => string, quotedItemId?: number, itemId?: number, line?: string): Promise<void> {
    const feature = refusedFeature(error)
    if (feature === undefined) throw error
    const text = `${subject}: ${refusal(feature)}${line === undefined ? "" : `\n${line}`}`
    await (itemId === undefined ? this.post([{text, ...(line === undefined ? {} : {priority: 1})}], quotedItemId) : this.updateItem(itemId, text, false, line === undefined ? undefined : 1))
  }

  // spec: spec/architecture.md#hub-session-deliver
  private post(parts: Part[], quotedItemId?: number, live = false): Promise<T.AChatItem[]> {
    const send = (quote?: number) => this.hub.chat.apiSendMessages([T.ChatType.Group, this.control.groupId], parts.map((part, i) => message(part, i === 0 ? quote : undefined)), live)
    return send(quotedItemId).catch(error => {
      if (quotedItemId === undefined || !isRefusedQuote(error)) throw error
      return send()
    })
  }

  // spec: spec/architecture.md#hub-session-deliver
  private pushedIn(prompt: string): {text: string; item: T.ChatItem; at: number}[] {
    const recent = Date.now() - promptEcho
    const tags = [...prompt.matchAll(channelPrompt)].map(([, item, content]) => ({item: Number(item), content: content.trim()}))
    const tagged: number[] = []
    for (const {item, content} of tags) {
      const i = this.pushed.findIndex((entry, j) => !tagged.includes(j) && item === entry.item.meta.itemId && content === entry.text.trim())
      if (i >= 0) tagged.push(i)
    }
    return this.pushed.filter((entry, i) => (entry.text === prompt && entry.at > recent) || tagged.includes(i))
  }

  // spec: spec/architecture.md#hub-session-queue
  private queue(work: () => Promise<unknown>): void {
    this.delivery = this.delivery.then(work).catch(error => logError(`prompt of session ${this.number}`, error))
  }

  // spec: spec/architecture.md#hub-session-queue
  private refusesCommand(text: string, member: T.GroupMember): boolean {
    return parseCommand(text) !== undefined && !this.approves(member) && !this.isUser(member)
  }

  // spec: spec/architecture.md#hub-session-approves
  private approves(member: T.GroupMember): boolean {
    return sessionApproverRoles.includes(member.memberRole) && !this.isUser(member)
  }

  // spec: spec/architecture.md#hub-session-approves
  private isUser(member: T.GroupMember): boolean {
    return member.memberContactId !== undefined && member.memberContactId === this.hub.userContactId
  }

  // spec: spec/architecture.md#hub-session-approves
  private setMode(mode: Mode): void {
    this.actions.mode = mode
    this.saveData({...this.data, mode}).catch(error => logError(`session ${this.number}`, error))
  }

  // spec: spec/architecture.md#hub-session-approves
  private declineHost(declined: boolean): void {
    this.hostDeclined = declined
    this.store({hostDeclined: declined || undefined})
  }

  // spec: spec/architecture.md#hub-session-approves
  private store(fields: Partial<SessionFile>): void {
    const folder = sessionFolder(this.hub.root, this.id)
    const stored = readSessionFile(folder)
    if (stored !== undefined) writeSessionFile(folder, {...stored, ...fields})
  }

  // spec: spec/architecture.md#hub-session-approves
  private savePending(connId: number): void {
    const recent = Date.now() - pendingDays * 86_400_000
    const pending = [...(this.data.pending ?? []).filter(({at}) => at > recent), {connId, at: Date.now()}]
    this.saveData({...this.data, pending}).catch(error => logError(`session ${this.number}`, error))
  }

  private saveData(data: SessionGroupData): Promise<void> {
    this.data = data
    return this.control.saveData({[sessionKey]: data})
  }
}

export class Hub implements SessionGroups {
  readonly main: MainGroup
  user?: User
  userContactId?: number
  promptsHeldUntil = 0
  private readonly sessions = new Map<number, Session>()
  stopped = false
  private controlEvents: Promise<void> = Promise.resolve()
  private listTimer?: NodeJS.Timeout
  private listing?: Promise<void>

  private constructor(
    readonly chat: api.ChatApi,
    readonly profile: T.User,
    readonly root: string,
    public expireHours: number,
    mainGroup: T.GroupInfo,
    mainName: string,
  ) {
    this.main = new MainGroup(chat, mainGroup, profile.userId, mainName, this)
  }

  // spec: spec/architecture.md#hub-hub-open
  static async open(root: string, config: Config, stored: StoredSession[]): Promise<Hub> {
    const core = await openCore(hubFolder(root), profileName, T.ChatPeerType.Agent)
    try {
      const {chat, user} = core
      const group = (await findMainGroup(chat, user.userId)) ?? (await createMainGroup(chat, user.userId, config.mainGroup))
      const hub = new Hub(chat, user, root, config.expireHours, group, config.mainGroup)
      await hub.main.load()
      await hub.main.start()
      hub.user = await User.open(root, chat, user).catch(error => (logError("User", error), undefined))
      hub.userContactId = (await userContact(chat, user.userId))?.contactId
      for (const {id, session} of stored) await hub.session(id, session)
      hub.promptsHeldUntil = Date.now() + attachWait
      await core.events(event => hub.onEvent(event))
      hub.listTimer = setInterval(() => hub.showSessions(), listCheckMs).unref()
      return hub
    } catch (error) {
      await core.chat.close().catch(() => undefined)
      throw error
    }
  }

  get mainGroup(): string {
    return this.main.displayName
  }

  // spec: spec/api.md#socket-protocol
  owner(): OwnerState {
    const {link} = this.main
    return {owners: this.main.owners().map(({memberProfile}) => memberProfile.displayName), ...(link === undefined ? {} : {link})}
  }

  // spec: spec/architecture.md#hub-hub-configure
  async configure(config: Config): Promise<void> {
    this.expireHours = config.expireHours
    for (const {actions} of this.sessions.values()) actions.expireHours = config.expireHours
    if (config.mainGroup !== this.main.displayName) await this.main.rename(config.mainGroup)
  }

  // spec: spec/api.md#socket-protocol
  async resetOwners(): Promise<ResetReply> {
    return {link: await this.main.replace()}
  }

  // spec: spec/architecture.md#hub-hub-session
  async session(id: string, stored: SessionFile): Promise<Session> {
    const opened = this.sessions.get(stored.number)
    if (opened !== undefined) {
      if (opened.displayName !== groupName(stored.number, stored.name)) await opened.rename(stored.name)
      return opened
    }
    const folder = sessionFolder(this.root, id)
    rmSync(join(folder, "files", "sent"), {recursive: true, force: true})
    const known = await this.storedGroup(stored)
    const group = known ?? (await this.createSessionGroup(stored))
    if (known === undefined) writeSessionFile(folder, {...(readSessionFile(folder) ?? stored), group: group.groupId})
    const session = new Session(this, id, stored, group, sessionDataOf(group.customData) ?? {session: stored.number})
    this.sessions.set(stored.number, session)
    await session.control.load()
    if (known === undefined) {
      for (const owner of this.main.owners()) await session.invite(owner, T.GroupMemberRole.Owner).catch(error => logError(`invitation to session ${stored.number}`, error))
    }
    return session
  }

  sessionId(number: number): string | undefined {
    return this.sessions.get(number)?.id
  }

  // spec: spec/api.md#list-of-sessions
  sessionsText(quote?: T.MsgContent): string {
    const sessions = [...this.sessions.values()].sort((a, b) => a.number - b.number)
    if (sessions.length === 0) return "No sessions."
    const lines = sessions.map(session => session.stateLine())
    const running = lines.filter(line => line !== undefined)
    const names = sessions.filter((_, i) => lines[i] === undefined).map(({displayName}) => displayName).reverse()
    const text = (shown: number): string => [...running, ...(names.length === 0 ? [] : [notRunningLine(names, shown)])].join("\n")
    const fits = (shown: number): boolean => fitsMessage({type: "text", text: text(shown)}, undefined, quote === undefined ? undefined : {content: quote})
    if (fits(names.length)) return text(names.length)
    let low = 0
    let high = names.length - 1
    while (low < high) {
      const middle = Math.ceil((low + high) / 2)
      if (fits(middle)) low = middle
      else high = middle - 1
    }
    return text(low)
  }

  // spec: spec/architecture.md#hub-hub-showsessions
  showSessions(): Promise<void> {
    this.listing ??= Promise.all([...this.sessions.values()].map(session => session.readRecord()))
      .then(() => this.main.showList(this.sessionsText()))
      .catch(error => logError("list of sessions", error))
      .finally(() => (this.listing = undefined))
    return this.listing
  }

  // spec: spec/api.md#main-group
  async join(member: T.GroupMember, number: number): Promise<string> {
    const session = this.sessions.get(number)
    if (session === undefined) return `No session ${number}. /sessions lists the sessions.`
    try {
      await session.invite(member, member.memberRole === T.GroupMemberRole.Owner ? T.GroupMemberRole.Owner : T.GroupMemberRole.Admin)
    } catch (error) {
      const chatError = chatErrorOf(error)
      if (chatError?.type === "error" && chatError.errorType.type === "groupDuplicateMember") return `You are a member of "${session.displayName}".`
      throw error
    }
    return `Sent you an invitation to "${session.displayName}".`
  }

  // spec: spec/architecture.md#hub-hub-sessionstext
  async inviteOwner(member: T.GroupMember): Promise<void> {
    for (const session of this.sessions.values()) {
      if (session.open) await session.invite(member, T.GroupMemberRole.Owner).catch(error => logError(`invitation to session ${session.number}`, error))
    }
  }

  expireDue(): void {
    for (const {actions} of this.sessions.values()) actions.expireDue()
  }

  // spec: spec/architecture.md#hub-hub-stop
  async stop(): Promise<Action[]> {
    this.stopped = true
    clearInterval(this.listTimer)
    const sessions = [...this.sessions.values()]
    const stopped = await Promise.all(sessions.map(async ({actions}) => ({lost: actions.lost, cancelled: await actions.stop()})))
    await this.controlEvents
    await this.listing
    await this.showSessions()
    await waitForCancelledCards(stopped.flatMap(({lost, cancelled}) => (lost ? [] : cancelled)), cardsWait, new AbortController().signal)
    await Promise.all(sessions.map(session => session.endLive()))
    for (const {tools} of sessions) tools.close()
    await this.user?.close()
    await this.chat.close()
    return stopped.flatMap(({cancelled}) => cancelled)
  }

  // spec: spec/architecture.md#hub-hub-storedgroup
  private async storedGroup({group, number}: SessionFile): Promise<T.GroupInfo | undefined> {
    if (group === undefined) return undefined
    const chat = await getChat(this.chat, `#${group}`, 0).catch(() => undefined)
    const groupInfo = chat?.chatInfo.type === "group" ? chat.chatInfo.groupInfo : undefined
    return sessionDataOf(groupInfo?.customData)?.session === number ? groupInfo : undefined
  }

  // spec: spec/api.md#session-group-profile
  private async createSessionGroup({number, name}: SessionFile): Promise<T.GroupInfo> {
    const displayName = groupName(number, name)
    const group = await createGroup(this.chat, this.profile.userId, {
      displayName,
      fullName: "",
      description: `The Claude Code session ${number}. Members send it prompts; admins and owners also send the group's commands and approve tool use.`,
      groupPreferences: {history: {enable: T.GroupFeatureEnabled.On}, directMessages: {enable: T.GroupFeatureEnabled.Off}, commands: controlCommands},
    }, {[sessionKey]: {session: number}})
    const user = await userContact(this.chat, this.profile.userId)
    if (user !== undefined) await this.chat.apiAddMember(group.groupId, user.contactId, T.GroupMemberRole.Member).catch(error => logError(`User in session ${number}`, error))
    log(`Group "${displayName}" of session ${number} created.`)
    return group
  }

  // spec: spec/architecture.md#hub-hub-onevent
  private async onEvent(event: ChatEvent): Promise<void> {
    if (this.stopped) return
    this.expireDue()
    const sessions = [...this.sessions.values()]
    if (event.type === "newChatItems") {
      for (const ci of event.chatItems) {
        if (isGroupMessage(ci, this.main.groupId)) this.main.onMessage(ci)
        else sessions.find(({control}) => isGroupMessage(ci, control.groupId))?.onMessage(ci)
      }
    }
    if (event.type === "chatItemReaction") sessions.find(({control}) => isGroupMessage(event.reaction, control.groupId))?.onReaction(event)
    const {file} = event.type === "rcvFileComplete" ? event.chatItem.chatItem : {}
    if (file !== undefined) sessions.forEach(session => session.fileDone(file.fileId))
    if (event.type === "rcvFileError") sessions.forEach(session => session.fileDone(event.rcvFileTransfer.fileId, errorText(event.agentError)))
    const connected = event.type === "contactConnected" ? {connId: event.contact.activeConn?.connId, chat: `@${event.contact.contactId}`} : event.type === "userJoinedGroup" ? {connId: event.hostMember.activeConn?.connId, chat: `#${event.groupInfo.groupId}`} : undefined
    if (connected?.connId !== undefined) sessions.forEach(session => session.claimPending(connected.connId!, connected.chat))
    for (const {tools} of sessions) tools.onEvent(event)
    if (event.type === "sndFileError") {
      const {chatItem_} = event
      const sending = chatItem_ === undefined ? undefined : sessions.find(({id, control}) => ownerOf(chatItem_.chatInfo) === id || isGroupMessage(chatItem_, control.groupId))
      sending?.control.fileNotSent(event)
    }
    if (isControlEvent(event, this.main.groupId)) {
      this.main.onEvent(event).catch(error => logError(`${event.type} in the Main group`, error))
      return
    }
    const session = sessions.find(({control}) => isControlEvent(event, control.groupId))
    if (session !== undefined) {
      this.controlEvents = this.controlEvents.then(() => session.control.onEvent(event)).then(() => session.sendTools()).then(() => session.announceRights(event)).then(() => session.askMode()).catch(error => logError(`${event.type} in the group of session ${session.number}`, error))
    }
  }
}

// spec: spec/architecture.md#hub-notallowedforuser
function notAllowedForUser(feature: string): string {
  return `${feature} are not allowed for User in the group of this session; its owners can allow them in the group's preferences.`
}

function groupName(number: number, name: string): string {
  return validName(`${number} · ${name}`)
}

// spec: spec/architecture.md#hub-session-readrecord
async function recordStatus(path: string): Promise<string | undefined> {
  const file = await open(path, constants.O_RDONLY | constants.O_NONBLOCK)
  try {
    if (!(await file.stat()).isFile()) return undefined
    const {status} = JSON.parse(await file.readFile("utf8")) as {status?: unknown}
    return typeof status === "string" ? status : undefined
  } finally {
    await file.close()
  }
}

// spec: spec/architecture.md#hub-session-readrecord
async function withinTime(work: Promise<void>, ms: number): Promise<void> {
  let timer: NodeJS.Timeout | undefined
  await Promise.race([work, new Promise<void>(resolve => (timer = setTimeout(resolve, ms)))])
  clearTimeout(timer)
}

// spec: spec/api.md#list-of-sessions
function notRunningLine(names: string[], shown: number): string {
  const rest = names.length - shown
  const older = rest === 0 ? [] : [`${rest} older session${rest === 1 ? "" : "s"}`]
  return `⚪ Not running: ${[...names.slice(0, shown), ...older].join(", ")}`
}

// spec: spec/architecture.md#hub-textparts
function textParts(text: string, quote?: Parameters<typeof fitsMessage>[2]): string[] {
  const chars = [...text]
  const parts: string[] = []
  let start = 0
  do {
    const fits = (end: number): boolean => fitsMessage({type: "text", text: chars.slice(start, end).join("")}, undefined, parts.length === 0 ? quote : undefined)
    let low = start + 1
    let high = Math.min(chars.length, start + maxMessageLength)
    while (low < high) {
      const middle = Math.ceil((low + high) / 2)
      if (fits(middle)) low = middle
      else high = middle - 1
    }
    parts.push(chars.slice(start, low).join(""))
    start = low
  } while (start < chars.length)
  return parts
}

function message({text, priority}: Part, quotedItemId?: number): T.ComposedMessage & {priority?: number} {
  return {msgContent: {type: "text", text}, quotedItemId, mentions: {}, priority}
}

// spec: spec/architecture.md#hub-quoteof
function quoteOf(quoted: T.ChatItem | undefined, text: string): Parameters<typeof fitsMessage>[2] {
  const quote = quoted?.content.type === "rcvMsgContent" || quoted?.content.type === "sndMsgContent" ? {content: quoted.content.msgContent, fileName: quoted.file?.fileName} : undefined
  return quote !== undefined && fitsMessage({type: "text", text: [...text].slice(0, 1).join("")}, undefined, quote) ? quote : undefined
}

// spec: spec/architecture.md#hub-quoteof
function endLine(figures: string | undefined, {fiveHour, week}: Limits): string {
  const now = Date.now() / 1_000
  const current = (window: LimitWindow | undefined): LimitWindow | undefined => (window !== undefined && window.resetsAt > now ? window : undefined)
  return small([`Done${figures === undefined ? "" : ` in ${figures}`}`, ...limitParts({fiveHour: current(fiveHour), week: current(week)})].join(" · "))
}

// spec: spec/architecture.md#hub-quoteof
function withoutEndLine(content: T.MsgContent, chatDir: T.CIDirection | undefined): T.MsgContent {
  return chatDir?.type === "groupSnd" ? {...content, text: content.text.replace(/\n!- Done(?: [^\n!]*)?!$/, "")} : content
}

// spec: spec/architecture.md#hub-quoteof
function withLine(parts: string[], line: string | undefined, quote?: Parameters<typeof fitsMessage>[2]): string[] {
  if (line === undefined) return parts
  const last = parts.at(-1)?.trimEnd() ?? ""
  const joined = last === "" ? line : `${last}\n${line}`
  if (textParts(joined, parts.length <= 1 ? quote : undefined).length === 1) return [...parts.slice(0, -1), joined]
  const chars = [...last]
  const keep = chars.length - [...line].length - 1
  return [...parts.slice(0, -1), chars.slice(0, keep).join(""), `${chars.slice(keep).join("")}\n${line}`]
}

// spec: spec/architecture.md#hub-quoteof
function small(text: string): string {
  return text.includes("!") ? text : `!- ${text}!`
}

// spec: spec/architecture.md#hub-quoteof
function keepLast(ids: Set<string>, id: string): void {
  ids.add(id)
  const [oldest] = ids
  if (ids.size > maxFinished) ids.delete(oldest)
}

// spec: spec/architecture.md#hub-isrefusedquote
function isRefusedQuote(error: unknown): boolean {
  const chatError = chatErrorOf(error)
  return chatError?.type === "errorStore" && (chatError.storeError.type === "invalidQuote" || chatError.storeError.type === "chatItemNotFound" || chatError.storeError.type === "largeMsg")
}

function sessionDataOf(customData: object | undefined): SessionGroupData | undefined {
  return (customData as {[sessionKey]?: SessionGroupData} | undefined)?.[sessionKey]
}
