import {ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Action, ActionHooks, Actions, ConnectAction, Mode, Subject, ToolError, leaveVerbs, subjectText, summary} from "./actions"
import {chatErrorOf, log, logError, oneLine} from "./log"
import {chatNumber, chatRef, isChatRef, utcMinutes, validName, viewName} from "./messages"
import {AgentTools, chatSubject, isActive, isConnected, memberCurrent} from "./tools"

const controlCommands: T.ChatBotCommand[] = [
  {type: "command", keyword: "pending", label: "Pending actions"},
  {type: "command", keyword: "list", label: "Chats and approvals"},
]

const commandsReply = "Commands: /approve, /reject, /revoke, /pending, /list. Each card shows its commands."
const cardTooLong = {send: "text: too long for its approval card; shorten it.", connect: "message: too long for its approval card; shorten it."}
const maxCardLength = 15_000
const listedChats = 100
const maxLineLength = 100
const truncationMark = "…"
const longestError = "€".repeat(600)
const greeting = "I'm your agents' Main group. Your Claude Code sessions ask here before they connect or send."
const noOwners = "No owners in the Main group."

export interface ControlData {
  control: true
  nextAction: number
}

export interface SessionView {
  readonly number: number
  readonly displayName: string
  readonly mode: Mode
  chat: api.ChatApi
  tools: Pick<AgentTools, "knownChat" | "agentChats">
}

export interface Sessions {
  get(number: number): SessionView | undefined
  open(): SessionView[]
}

export interface ControlOptions {
  displayName: string
}

export interface Command {
  keyword: string
  params: string
}

interface ChatApproval {
  approvedBy: string
  approvedById: number
  approvedAt: string
}

interface ChatTarget {
  session: number
  chat: string
}

type OwnerRequest =
  | {type: "approve" | "reject"; action: number; target?: ChatTarget}
  | {type: "approveChat" | "revoke"; target: ChatTarget}
  | {type: "pending" | "list"}

export class Control implements Pick<ActionHooks, "refusal" | "saveCounter" | "postCard" | "outcome"> {
  private members: T.GroupMember[] = []
  private writes: Promise<unknown> = Promise.resolve()
  private ownerCommands: Promise<unknown> = Promise.resolve()
  private events: Promise<unknown> = Promise.resolve()
  private linkText?: string

  constructor(
    private readonly chat: api.ChatApi,
    private group: T.GroupInfo,
    private readonly options: ControlOptions,
    private readonly actions: Actions,
    private readonly sessions: Sessions,
  ) {}

  get groupId(): number {
    return this.group.groupId
  }

  get link(): string | undefined {
    return this.linkText
  }

  owners(): string[] {
    return this.approvers().map(({memberProfile}) => oneLine(memberProfile.displayName))
  }

  async load(): Promise<void> {
    if (isLost(this.group)) return this.lose()
    const {groupProfile} = this.group
    if (groupProfile.displayName !== this.options.displayName || JSON.stringify(groupProfile.groupPreferences?.commands) !== JSON.stringify(controlCommands)) {
      this.group = await this.chat.apiUpdateGroupProfile(this.groupId, {...groupProfile, displayName: this.options.displayName, groupPreferences: {...groupProfile.groupPreferences, commands: controlCommands}})
    }
    await this.refreshMembers()
  }

  async start(): Promise<void> {
    if (this.actions.lost || this.approvers().length > 0) return
    if (this.humans().length > 0) log(noOwners)
    await this.openLink()
  }

  refusal(action: Action): string | undefined {
    const approver = this.longestApproverName()
    const header = this.header(action)
    if (action.tool === "connect") action.linkTextBytes = maxCardLength - largestCard({...action, linkTextBytes: 0}, approver, header)
    if (action.tool !== "leave" && largestCard(action, approver, header) > maxCardLength) return cardTooLong[action.tool]
    if (this.approvers().length > 0) return undefined
    return this.linkText === undefined ? "The agent has no owner. Run /simplex:link in Claude Code." : `The agent has no owner. Open this link in the SimpleX app and join: ${this.linkText}`
  }

  standingApprover(chatInfo: T.ChatInfo, mode: Mode): string | undefined {
    const approval = approvalOf(customDataOf(chatInfo))
    return mode === "requests" && approval !== undefined && this.approvers().some(({groupMemberId}) => groupMemberId === approval.approvedById) ? safeName(approval.approvedBy) : undefined
  }

  saveCounter(nextAction: number): Promise<void> {
    const write = this.writes.then(() => this.chat.apiSetGroupCustomData(this.groupId, controlData(nextAction)))
    this.writes = write.catch(() => undefined)
    return write
  }

  async postCard(action: Action): Promise<number> {
    return this.post(this.card(action))
  }

  async outcome(action: Action): Promise<void> {
    const text = this.card(action)
    if (action.card === undefined) await this.post(text)
    else await this.chat.apiUpdateChatItem(T.ChatType.Group, this.groupId, action.card, {type: "text", text}, false)
  }

  onMessage(ci: T.AChatItem): void {
    const {chatItem} = ci
    if (this.actions.lost || chatItem.chatDir.type !== "groupRcv" || chatItem.content.type !== "rcvMsgContent") return
    const command = parseCommand(chatItem.content.msgContent.text)
    if (command === undefined) return
    const sender = chatItem.chatDir.groupMember
    this.ownerCommands = this.ownerCommands
      .then(() => this.ownerCommand(sender, command))
      .then(reply => (reply === undefined ? undefined : this.chat.apiSendTextReply(ci, reply)))
      .catch(error => logError("Main group command", error))
  }

  onEvent(event: ChatEvent): Promise<void> {
    const handled = this.events.then(() => this.handleEvent(event))
    this.events = handled.catch(() => undefined)
    return handled
  }

  private async handleEvent(event: ChatEvent): Promise<void> {
    if (this.actions.lost) return
    if (losesControl(event)) return this.lose()
    const before = this.approvers()
    await this.refreshMembers()
    if (event.type === "joinedGroupMember" && this.isHuman(event.member) && this.approvers().length === 0) return this.ownerJoined(event.member)
    if (before.length > 0 && this.approvers().length === 0) {
      log(noOwners)
      await this.openLink()
    }
  }

  async ownerCommand(sender: T.GroupMember, command: Command): Promise<string | undefined> {
    this.actions.expireDue()
    const request = ownerRequest(command)
    if (request === undefined) return commandsReply
    if (!this.isApprover(sender)) return "Only owners approve actions."
    switch (request.type) {
      case "pending": {
        const lines = this.actions.pending().filter(({card}) => card !== undefined).map(pendingLine)
        return lines.length > 0 ? lines.join("\n") : "No pending actions."
      }
      case "list":
        return this.chatsText()
      case "approveChat":
        return this.approveChat(sender, request.target)
      case "revoke":
        return this.revoke(request.target)
      case "approve":
      case "reject": {
        const action = this.actions.get(request.action)
        if (action === undefined || action.state !== "pending" || action.card === undefined) return `Action ${request.action} is not pending.`
        if (request.target !== undefined && (action.tool === "connect" || action.session !== request.target.session || action.chat !== request.target.chat)) {
          return `Action ${request.action} is not for ${targetText(request.target)}.`
        }
        const approver = safeName(sender.memberProfile.displayName)
        if (request.type === "approve") this.actions.approve(action, approver)
        else this.actions.reject(action, approver)
        return undefined
      }
    }
  }

  async rename(displayName: string): Promise<void> {
    this.options.displayName = displayName
    await this.load()
  }

  async reset(userId: number): Promise<string> {
    if (!this.actions.lost) this.actions.loseControl()
    this.group = await resetControlChat(this.chat, userId, this.group, this.options.displayName, this.actions.next)
    this.actions.regainControl()
    await this.load()
    await this.openLink()
    return this.linkText!
  }

  private card(action: Action): string {
    return `${this.header(action)}\n${cardText(action)}`
  }

  private header({session}: Action): string {
    const view = this.sessions.get(session)
    return view === undefined ? `Session ${session}` : `Session ${session} · ${safeName(view.displayName)}`
  }

  private async approveChat(sender: T.GroupMember, target: ChatTarget): Promise<string> {
    const view = this.sessions.get(target.session)
    if (view === undefined) return noSession(target.session)
    if (view.mode === "host") return `Approvals apply in mode requests; session ${target.session} runs in mode host.`
    const chatInfo = await agentChat(view, target.chat)
    if (chatInfo === undefined) return `No chat ${targetText(target)}. /list shows the agents' chats.`
    const approvedBy = sender.memberProfile.displayName
    const approval: ChatApproval = {approvedBy, approvedById: sender.groupMemberId, approvedAt: new Date().toISOString()}
    await setCustomData(view.chat, target.chat, {...customDataOf(chatInfo), simplexMcp: approval})
    const approver = safeName(approvedBy)
    for (const action of this.actions.pending()) {
      if (action.tool === "send" && action.session === target.session && action.chat === target.chat && action.card !== undefined) this.actions.approve(action, approver)
    }
    return `Approved for ${chatName(chatSubject(chatInfo))} (${targetText(target)}) by ${approver}. /'revoke ${targetText(target)}'`
  }

  private async revoke(target: ChatTarget): Promise<string> {
    const view = this.sessions.get(target.session)
    if (view === undefined) return noSession(target.session)
    const chatInfo = await agentChat(view, target.chat)
    if (chatInfo === undefined || approvalOf(customDataOf(chatInfo)) === undefined) return `Messages to ${targetText(target)} have no standing approval.`
    const {simplexMcp, ...customData} = customDataOf(chatInfo) as {simplexMcp: unknown}
    await setCustomData(view.chat, target.chat, customData)
    return `Messages to ${chatName(chatSubject(chatInfo))} (${targetText(target)}) need approval again.`
  }

  private async chatsText(): Promise<string> {
    const views = this.sessions.open()
    if (views.length === 0) return "No open sessions."
    const lines: string[] = []
    for (const view of views) {
      lines.push(`Session ${view.number} · ${safeName(view.displayName)}`)
      const chats = await view.tools.agentChats()
      for (const {chatInfo} of chats.slice(0, listedChats)) {
        const approvedBy = this.standingApprover(chatInfo, view.mode)
        const approval = approvedBy === undefined ? "" : `, approved for this chat by ${approvedBy}`
        const connection = isConnected(chatInfo) ? "" : ", not connected"
        lines.push(truncatedLine(`${view.number}:${chatRef(chatInfo)}  ${chatName(chatSubject(chatInfo))}${approval}${connection}`, maxLineLength))
      }
      const rest = chats.length - Math.min(chats.length, listedChats)
      if (rest > 0) lines.push(`${rest} older chat${rest === 1 ? " is" : "s are"} not shown.`)
    }
    return lines.join("\n")
  }

  private async ownerJoined(member: T.GroupMember): Promise<void> {
    await this.deleteLink()
    await this.chat.apiSetMembersRole(this.groupId, [member.groupMemberId], T.GroupMemberRole.Owner)
    await this.refreshMembers()
    await this.post(greeting)
    log(`${joinedText(member, oneLine)} and is now an owner. The link is deleted.`)
  }

  private lose(): void {
    this.actions.loseControl()
    log("The agent lost its Main group. Run /simplex:reset-owners in Claude Code.")
  }

  private async openLink(): Promise<void> {
    await this.deleteLink()
    this.linkText = await this.chat.apiCreateGroupLink(this.groupId, T.GroupMemberRole.Member)
    log(`Main group link (works once): ${this.linkText}`)
    log(`Whoever joins by this link becomes an owner of "${this.options.displayName}" and approves the agents' actions.`)
  }

  private async deleteLink(): Promise<void> {
    this.linkText = undefined
    try {
      await this.chat.apiDeleteGroupLink(this.groupId)
    } catch (error) {
      const chatError = chatErrorOf(error)
      if (chatError?.type !== "errorStore" || chatError.storeError.type !== "groupLinkNotFound") throw error
    }
  }

  private async post(text: string): Promise<number> {
    const [item] = await this.chat.apiSendMessages([T.ChatType.Group, this.groupId], [{msgContent: {type: "text", text}, mentions: {}}])
    return item.chatItem.meta.itemId
  }

  private async refreshMembers(): Promise<void> {
    this.members = await this.chat.apiListMembers(this.groupId)
  }

  private humans(): T.GroupMember[] {
    return this.members.filter(member => this.isHuman(member))
  }

  private approvers(): T.GroupMember[] {
    return this.humans().filter(member => this.isApprover(member))
  }

  private longestApproverName(): string {
    const names = this.approvers().map(({memberProfile}) => safeName(memberProfile.displayName))
    return names.reduce((longest, name) => (encodedLength(name) > encodedLength(longest) ? name : longest), "")
  }

  private isHuman(member: T.GroupMember): boolean {
    return member.groupMemberId !== this.group.membership.groupMemberId && isActive(member)
  }

  private isApprover(member: T.GroupMember): boolean {
    return member.memberRole === T.GroupMemberRole.Owner
  }
}

export async function findControlGroup(chat: api.ChatApi, userId: number): Promise<T.GroupInfo | undefined> {
  const groups = (await chat.apiListGroups(userId)).filter(({customData}) => controlDataOf(customData) !== undefined)
  return groups.reduce<T.GroupInfo | undefined>((latest, group) => (latest === undefined || group.groupId > latest.groupId ? group : latest), undefined)
}

export function controlDataOf(customData: object | undefined): ControlData | undefined {
  const data = (customData as {simplexMcp?: Partial<ControlData>} | undefined)?.simplexMcp
  return data?.control === true && typeof data.nextAction === "number" ? {control: true, nextAction: data.nextAction} : undefined
}

export async function createControlChat(chat: api.ChatApi, userId: number, displayName: string, nextAction: number): Promise<T.GroupInfo> {
  const group = await chat.apiNewGroup(userId, {
    displayName,
    fullName: "",
    description: "Supervise your AI agents here: approve or reject their connections and messages with one tap.",
    groupPreferences: {history: {enable: T.GroupFeatureEnabled.On}, commands: controlCommands},
  })
  const customData = controlData(nextAction)
  await chat.apiSetGroupCustomData(group.groupId, customData)
  log(`Main group "${displayName}" created.`)
  return {...group, customData}
}

export async function resetControlChat(chat: api.ChatApi, userId: number, group: T.GroupInfo, displayName: string, nextAction: number): Promise<T.GroupInfo> {
  const {membership} = group
  if (memberCurrent(membership) && membership.memberRole !== T.GroupMemberRole.Owner) await chat.apiLeaveGroup(group.groupId)
  await chat.apiDeleteChat(T.ChatType.Group, group.groupId, {type: "full", notify: true})
  return createControlChat(chat, userId, displayName, nextAction)
}

export function isControlEvent(event: ChatEvent, groupId: number): boolean {
  switch (event.type) {
    case "joinedGroupMember":
    case "joinedGroupMemberConnecting":
    case "connectedToGroupMember":
    case "memberRole":
    case "leftMember":
    case "deletedMember":
    case "deletedMemberUser":
    case "groupDeleted":
      return event.groupInfo.groupId === groupId
    default:
      return false
  }
}

export function parseCommand(text: string): Command | undefined {
  const lines = text.split("\n")
  if (lines.at(-1) === "") lines.pop()
  const match = lines.length === 1 ? /^ *\/(\S+)([\s\S]*)$/.exec(lines[0]) : null
  return match ? {keyword: match[1], params: match[2].trim()} : undefined
}

export function cardText(action: Action): string {
  const request = requestLines(action)
  const approval = action.mode === "host" ? "Performed without approval (mode host)" : action.standing ? `Approved for this chat by ${action.decidedBy}` : `Approved by ${action.decidedBy}`
  switch (action.state) {
    case "pending":
      return action.interrupted
        ? [...request, "Cancelled: the SimpleX daemon stopped."].join("\n")
        : [...request, commandsLine(action)].join("\n")
    case "performing":
      return [...request, `${approval}; interrupted: the SimpleX daemon stopped.`].join("\n")
    case "done":
      return [...request, action.standing ? `${approval}. /'revoke ${action.session}:${action.chat}'` : `${approval}.`].join("\n")
    case "failed":
      return [...request, `${approval}; failed: ${codeSpan(oneLine(action.error ?? ""))}.`].join("\n")
    case "rejected":
      return [...request, `Rejected by ${action.decidedBy}.`].join("\n")
    case "expired":
      return [...request, "Expired."].join("\n")
  }
}

function requestLines(action: Action): string[] {
  const done = action.state === "done"
  const subject = subjectText(action.subject, safeName)
  switch (action.tool) {
    case "connect": {
      const {input} = action
      const verb = !done ? "I want to connect" : action.connected ? "I connected" : "I asked to connect"
      const message = input.message === undefined ? [] : ["With the message:", ...codeLines(input.message)]
      return [`${verb} to ${subject}, ${action.subject.incognito ? "incognito" : "as myself"}.`, ...linkLines(action), ...message]
    }
    case "send": {
      const {replyTo, mentions} = action
      const reply = replyTo === undefined ? [] : [`replying to ${safeName(replyTo.from)}: ${codeSpan([...replyTo.text.replace(/\s+/g, " ")].slice(0, 100).join(""))}`]
      const mentioning = mentions.length === 0 ? [] : [`mentioning ${mentions.map(({name}) => mentionText(name)).join(", ")}`]
      return [`${done ? "I sent" : "I want to send"} a message to ${subject}${action.subject.incognito ? ", where I am incognito" : ""}:`, ...codeLines(action.input.text), ...reply, ...mentioning]
    }
    case "leave": {
      const {present, past} = leaveVerbs(action)
      return [`${done ? `I ${past}` : `I want to ${present}`} ${chatName(action.subject)} (${action.chat}).`]
    }
  }
}

function commandsLine(action: Action): string {
  const chat = action.tool === "connect" ? "" : `${action.session}:${action.chat}`
  switch (action.tool) {
    case "connect":
      return `/'approve ${action.n}' or /'reject ${action.n}'`
    case "send":
      return `/'approve ${chat} ${action.n}', /'approve ${chat} all' or /'reject ${chat} ${action.n}'`
    case "leave":
      return `/'approve ${chat} ${action.n}' or /'reject ${chat} ${action.n}'`
  }
}

function pendingLine(action: Action): string {
  return `${action.n}  session ${action.session}: ${summary(action, safeName).what}  expires ${utcMinutes(action.expires)}`
}

function largestCard(action: Action, approver: string, header: string): number {
  const decidedBy = action.decidedBy ?? approver
  const cards: Action[] = [action, {...action, state: "done", decidedBy}, {...action, state: "failed", decidedBy, error: longestError}]
  return Math.max(...cards.map(card => encodedLength(`${header}\n${cardText(card)}`)))
}

function encodedLength(text: string): number {
  return [...text].reduce((length, c) => length + (/["\\\n\r\t]/.test(c) ? 2 : c < " " ? 6 : Buffer.byteLength(c)), 0)
}

function truncatedLine(line: string, maxLength: number): string {
  if (encodedLength(line) <= maxLength) return line
  let length = encodedLength(truncationMark)
  let kept = ""
  for (const c of line) {
    length += encodedLength(c)
    if (length > maxLength) break
    kept += c
  }
  return `${kept}${truncationMark}`
}

function linkLines({plan, linkTextBytes = Infinity}: ConnectAction): string[] {
  const lines: string[] = []
  let left = linkTextBytes
  for (const line of [...codeLines(plan.description), ...codeLines(plan.welcome)]) {
    const length = encodedLength(`\n${line}`)
    if (length > left) {
      const room = left - encodedLength("\n``")
      if (room >= encodedLength(truncationMark)) lines.push(`\`${truncatedLine(line.slice(1, -1), room)}\``)
      break
    }
    lines.push(line)
    left -= length
  }
  return lines
}

function chatName(subject: Subject): string {
  return subjectText({...subject, simplexName: undefined}, safeName)
}

function codeLines(text: string | undefined): string[] {
  return text === undefined ? [] : text.split("\n").map(line => (line.trim() === "" ? "" : codeSpan(line)))
}

function codeSpan(text: string): string {
  return `\`${text.replace(/`/g, "'").replace(/^ | $/g, "\u00a0")}\``
}

function safeName(name: string): string {
  const line = oneLine(name)
  return validName(line) === line ? line : codeSpan(line)
}

function mentionText(name: string): string {
  const line = oneLine(name)
  return validName(line) === line ? `@${viewName(line)}` : codeSpan(`@${line}`)
}

function controlData(nextAction: number): {simplexMcp: ControlData} {
  return {simplexMcp: {control: true, nextAction}}
}

function customDataOf(chatInfo: T.ChatInfo): object | undefined {
  return chatInfo.type === "direct" ? chatInfo.contact.customData : (chatInfo as T.ChatInfo.Group).groupInfo.customData
}

function approvalOf(customData: object | undefined): {approvedBy: string; approvedById: number} | undefined {
  const data = (customData as {simplexMcp?: Partial<ChatApproval>} | undefined)?.simplexMcp
  return typeof data?.approvedBy === "string" && typeof data.approvedById === "number" ? {approvedBy: data.approvedBy, approvedById: data.approvedById} : undefined
}

async function agentChat(view: SessionView, chat: string): Promise<T.ChatInfo | undefined> {
  try {
    return (await view.tools.knownChat(chat)).chatInfo
  } catch (error) {
    if (error instanceof ToolError) return undefined
    throw error
  }
}

function setCustomData(chat: api.ChatApi, ref: string, customData: object): Promise<void> {
  return ref.startsWith("@") ? chat.apiSetContactCustomData(chatNumber(ref), customData) : chat.apiSetGroupCustomData(chatNumber(ref), customData)
}

function chatTarget(word: string): ChatTarget | undefined {
  const match = /^([1-9]\d*):(.+)$/.exec(word)
  return match !== null && isChatRef(match[2]) ? {session: Number(match[1]), chat: match[2]} : undefined
}

function targetText({session, chat}: ChatTarget): string {
  return `${session}:${chat}`
}

function noSession(session: number): string {
  return `No open session ${session}. /list shows the open sessions.`
}

function ownerRequest({keyword, params}: Command): OwnerRequest | undefined {
  const words = params === "" ? [] : params.split(/\s+/)
  const target = words.length === 2 ? chatTarget(words[0]) : undefined
  const value = words.length === 1 || target !== undefined ? words.at(-1) : undefined
  if ((keyword === "approve" || keyword === "reject") && value !== undefined && /^\d+$/.test(value)) return {type: keyword, action: Number(value), target}
  if (keyword === "approve" && target !== undefined && value === "all") return {type: "approveChat", target}
  const revoked = keyword === "revoke" && words.length === 1 ? chatTarget(words[0]) : undefined
  if (revoked !== undefined) return {type: "revoke", target: revoked}
  if ((keyword === "pending" || keyword === "list") && words.length === 0) return {type: keyword}
  return undefined
}

function losesControl(event: ChatEvent): boolean {
  switch (event.type) {
    case "deletedMemberUser":
    case "groupDeleted":
      return true
    case "memberRole":
      return event.member.groupMemberId === event.groupInfo.membership.groupMemberId && event.toRole !== T.GroupMemberRole.Owner
    default:
      return false
  }
}

function isLost({membership}: T.GroupInfo): boolean {
  const statuses: T.GroupMemberStatus[] = [T.GroupMemberStatus.Creator, T.GroupMemberStatus.Connected, T.GroupMemberStatus.Complete]
  return membership.memberRole !== T.GroupMemberRole.Owner || !statuses.includes(membership.memberStatus)
}

function joinedText(member: T.GroupMember, showName: (name: string) => string): string {
  return `${showName(member.memberProfile.displayName)} joined (member ${member.groupMemberId})`
}
