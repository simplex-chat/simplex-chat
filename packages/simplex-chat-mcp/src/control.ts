import {ChatEvent, T} from "@simplex-chat/types"
import * as qrcode from "qrcode"
import {api} from "simplex-chat"
import {Action, ActionHooks, Actions, ConnectAction, Subject, ToolError, leaveVerbs, subjectText, summary} from "./actions"
import {ExitError, chatErrorOf, log, logError, oneLine, shellWord} from "./log"
import {chatNumber, chatRef, isChatRef, utcMinutes, validName, viewName} from "./messages"
import type {OwnerEntry, QrCodeSize} from "./serve"
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
const greeting = "I'm your agent. I ask here before I connect or send."
const noOwners = "No owners in the agent control chat."
const qrCodeHint = "If the QR code does not scan, restart with --qr-code large, or paste the link into the app."
const qrMargin = 2
const darkCell = "\x1b[40m  "
const lightCell = "\x1b[47m  "
const darkHalfCell = "\x1b[40m "
const lightHalfCell = "\x1b[47m "
const darkTopCell = "\x1b[37;40m▄"
const darkBottomCell = "\x1b[30;47m▄"
const resetColors = "\x1b[0m"

export interface ControlData {
  control: true
  nextAction: number
}

export interface ControlOptions {
  displayName: string
  owners?: OwnerEntry[]
  registerHint: string
  qrCode: QrCodeSize
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

type OwnerRequest =
  | {type: "approve" | "reject"; action: number; chat?: string}
  | {type: "approveChat" | "revoke"; chat: string}
  | {type: "pending" | "list"}

interface Shown {
  name: (name: string) => string
  command: (command: string) => string
}

const inLog: Shown = {name: oneLine, command: command => command}
const inChat: Shown = {name: safeName, command: codeSpan}

export class Control implements Pick<ActionHooks, "refusal" | "saveCounter" | "postCard" | "outcome"> {
  private members: T.GroupMember[] = []
  private writes: Promise<unknown> = Promise.resolve()
  private ownerCommands: Promise<unknown> = Promise.resolve()

  constructor(
    private readonly chat: api.ChatApi,
    private group: T.GroupInfo,
    private readonly options: ControlOptions,
    private readonly actions: Actions,
    private readonly tools: Pick<AgentTools, "knownChat" | "agentChats">,
  ) {}

  get groupId(): number {
    return this.group.groupId
  }

  get chatRef(): string {
    return `#${this.group.groupId}`
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
    if (this.actions.lost) return
    const {owners} = this.options
    if (owners === undefined) return this.startOwners()
    if (owners.length === 0) return this.startObserver()
    return this.startListed(owners)
  }

  refusal(action: Action): string | undefined {
    const {mode} = this.actions
    const approver = this.longestApproverName()
    if (action.tool === "connect") action.linkTextBytes = maxCardLength - largestCard({...action, linkTextBytes: 0}, mode, approver)
    if (action.tool !== "leave" && largestCard(action, mode, approver) > maxCardLength) return cardTooLong[action.tool]
    if (this.approvers().length > 0) return undefined
    return this.options.owners === undefined
      ? "The agent has no owner. The person who runs it has to join its control chat with the link the server printed."
      : "The agent has no approving owner. The person who runs it has to restart it with the --owners options it printed."
  }

  standingApprover(chatInfo: T.ChatInfo): string | undefined {
    const approval = approvalOf(customDataOf(chatInfo))
    return this.actions.mode === "requests" && approval !== undefined && this.approvers().some(({groupMemberId}) => groupMemberId === approval.approvedById) ? safeName(approval.approvedBy) : undefined
  }

  saveCounter(nextAction: number): Promise<void> {
    const write = this.writes.then(() => this.chat.apiSetGroupCustomData(this.groupId, controlData(nextAction)))
    this.writes = write.catch(() => undefined)
    return write
  }

  async postCard(action: Action): Promise<number> {
    return this.post(cardText(action, this.actions.mode))
  }

  async outcome(action: Action): Promise<void> {
    const text = cardText(action, this.actions.mode)
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
      .catch(error => logError("control chat command", error))
  }

  async onEvent(event: ChatEvent): Promise<void> {
    if (this.actions.lost) return
    if (losesControl(event)) return this.lose()
    const before = this.humanOwners()
    await this.refreshMembers()
    const {owners} = this.options
    if (event.type === "joinedGroupMember" && this.isHuman(event.member)) {
      if (owners === undefined) return this.ownerJoined(event.member)
      if (owners.length === 0) return this.observerJoined(event.member)
    }
    if (owners === undefined && before.length > 0 && this.approvers().length === 0) {
      log(noOwners)
      return this.openLink(T.GroupMemberRole.Member)
    }
    if (owners !== undefined && owners.length > 0) await this.reportOwnerChanges(owners, before)
  }

  async ownerCommand(sender: T.GroupMember, command: Command): Promise<string | undefined> {
    this.actions.expireDue()
    const request = ownerRequest(command)
    if (request === undefined) return commandsReply
    if (!this.isApprover(sender)) return this.options.owners === undefined ? "Only owners approve actions." : "Only the owners listed in --owners approve actions."
    switch (request.type) {
      case "pending": {
        const lines = this.actions.pending().filter(({card}) => card !== undefined).map(pendingLine)
        return lines.length > 0 ? lines.join("\n") : "No pending actions."
      }
      case "list":
        return this.chatsText()
      case "approveChat":
        if (this.actions.mode === "host") return "Approvals apply in mode requests; this server runs in mode host."
        return this.approveChat(sender, request.chat)
      case "revoke":
        return this.revoke(request.chat)
      case "approve":
      case "reject": {
        const action = this.actions.get(request.action)
        if (action === undefined || action.state !== "pending" || action.card === undefined) return `Action ${request.action} is not pending.`
        if (request.chat !== undefined && (action.tool === "connect" || action.chat !== request.chat)) return `Action ${request.action} is not for ${request.chat}.`
        const approver = safeName(sender.memberProfile.displayName)
        if (request.type === "approve") this.actions.approve(action, approver)
        else this.actions.reject(action, approver)
        return undefined
      }
    }
  }

  private async approveChat(sender: T.GroupMember, chat: string): Promise<string> {
    const chatInfo = await this.agentChat(chat)
    if (chatInfo === undefined) return `No chat ${chat}. /list shows the agent's chats.`
    const approvedBy = sender.memberProfile.displayName
    const approval: ChatApproval = {approvedBy, approvedById: sender.groupMemberId, approvedAt: new Date().toISOString()}
    await this.setCustomData(chat, {...customDataOf(chatInfo), simplexMcp: approval})
    const approver = safeName(approvedBy)
    for (const action of this.actions.pending()) {
      if (action.tool === "send" && action.chat === chat && action.card !== undefined) this.actions.approve(action, approver)
    }
    return `Approved for ${chatName(chatSubject(chatInfo))} (${chat}) by ${approver}. /'revoke ${chat}'`
  }

  private async revoke(chat: string): Promise<string> {
    const chatInfo = await this.agentChat(chat)
    if (chatInfo === undefined || approvalOf(customDataOf(chatInfo)) === undefined) return `Messages to ${chat} have no standing approval.`
    const {simplexMcp, ...customData} = customDataOf(chatInfo) as {simplexMcp: unknown}
    await this.setCustomData(chat, customData)
    return `Messages to ${chatName(chatSubject(chatInfo))} (${chat}) need approval again.`
  }

  private async chatsText(): Promise<string> {
    const chats = await this.tools.agentChats()
    const lines = chats.slice(0, listedChats).map(({chatInfo}) => {
      const approvedBy = this.standingApprover(chatInfo)
      const approval = approvedBy === undefined ? "" : `, approved for this chat by ${approvedBy}`
      const connection = isConnected(chatInfo) ? "" : ", not connected"
      return truncatedLine(`${chatRef(chatInfo)}  ${chatName(chatSubject(chatInfo))}${approval}${connection}`, maxLineLength)
    })
    const rest = chats.length - lines.length
    if (rest > 0) lines.push(`${rest} older chat${rest === 1 ? " is" : "s are"} not shown.`)
    return lines.length > 0 ? lines.join("\n") : "No chats."
  }

  private async agentChat(chat: string): Promise<T.ChatInfo | undefined> {
    try {
      return (await this.tools.knownChat(chat)).chatInfo
    } catch (error) {
      if (error instanceof ToolError) return undefined
      throw error
    }
  }

  private setCustomData(chat: string, customData: object): Promise<void> {
    return chat.startsWith("@") ? this.chat.apiSetContactCustomData(chatNumber(chat), customData) : this.chat.apiSetGroupCustomData(chatNumber(chat), customData)
  }

  private async startOwners(): Promise<void> {
    if (this.humanOwners().length > 0) return log(this.options.registerHint)
    if (this.humans().length > 0) log(noOwners)
    await this.openLink(T.GroupMemberRole.Member)
  }

  private async startObserver(): Promise<void> {
    if (this.humans().length === 0) return this.openLink(T.GroupMemberRole.Observer)
    await this.announce(shown => `${this.humans().map(member => joinedText(member, shown.name)).join(". ")}. ${restartText(this.humans(), shown)}`)
  }

  private async startListed(owners: OwnerEntry[]): Promise<void> {
    const humans = this.humans()
    const listed = owners.map(owner => {
      const member = humans.find(human => listsMember(owner, human))
      if (!member) throw new ExitError(`--owners: member ${entryText(owner)} is not in the agent control chat. Start with --reset-owners --owners, or use the IDs of current members.`, 2)
      return member
    })
    for (const member of listed) {
      if (member.memberRole !== T.GroupMemberRole.Owner) await this.chat.apiSetMembersRole(this.groupId, [member.groupMemberId], T.GroupMemberRole.Owner)
    }
    for (const member of humans) {
      if (!listed.includes(member)) await this.chat.apiRemoveMembers(this.groupId, [member.groupMemberId])
    }
    await this.deleteLink()
    await this.refreshMembers()
    log(`Owners: ${owners.map(entryText).join(",")}.`)
    log(this.options.registerHint)
  }

  private async ownerJoined(member: T.GroupMember): Promise<void> {
    await this.deleteLink()
    await this.chat.apiSetMembersRole(this.groupId, [member.groupMemberId], T.GroupMemberRole.Owner)
    await this.refreshMembers()
    await this.post(greeting)
    log(`${joinedText(member, oneLine)} and is now an owner. The link is deleted.`)
    log(this.options.registerHint)
  }

  private async observerJoined(member: T.GroupMember): Promise<void> {
    await this.deleteLink()
    await this.announce(shown => `${joinedText(member, shown.name)}. ${restartText(this.humans(), shown)}`)
  }

  private async reportOwnerChanges(owners: OwnerEntry[], before: T.GroupMember[]): Promise<void> {
    const after = this.humanOwners()
    const isListed = (member: T.GroupMember): boolean => owners.some(owner => listsMember(owner, member))
    for (const member of after) {
      if (!isListed(member) && !before.some(sameMember(member))) {
        await this.announce(shown => `${memberText(member, shown.name)} is now an owner and can't approve actions yet. ${restartText(after, shown)}`)
      }
    }
    for (const member of before) {
      if (isListed(member) && !after.some(sameMember(member))) {
        const remaining = (shown: Shown): string => (after.length > 0 ? restartText(after, shown) : "No owners remain: restart the server with --reset-owners --owners")
        await this.announce(shown => `${memberText(member, shown.name)} is no longer an owner. ${remaining(shown)}`)
      }
    }
  }

  private lose(): void {
    this.actions.loseControl()
    log(`The agent lost its control chat. Restart the server with --reset-owners.`)
  }

  private async openLink(role: T.GroupMemberRole): Promise<void> {
    await this.deleteLink()
    const link = await this.chat.apiCreateGroupLink(this.groupId, role)
    log("Join the agent control chat in the SimpleX app. The link works once:")
    log(qrCodeText(link, this.options.qrCode))
    log(link)
    if (this.options.qrCode === "small") log(qrCodeHint)
    log(role === T.GroupMemberRole.Observer
      ? "The link adds an observer. Its ID and name are printed when it joins."
      : `Whoever joins by this link becomes an owner of "${this.options.displayName}" and approves its actions. To approve owners yourself, start with --owners.`)
  }

  private async deleteLink(): Promise<void> {
    try {
      await this.chat.apiDeleteGroupLink(this.groupId)
    } catch (error) {
      const chatError = chatErrorOf(error)
      if (chatError?.type !== "errorStore" || chatError.storeError.type !== "groupLinkNotFound") throw error
    }
  }

  private async announce(text: (shown: Shown) => string): Promise<void> {
    log(text(inLog))
    await this.post(text(inChat))
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

  private humanOwners(): T.GroupMember[] {
    return this.humans().filter(({memberRole}) => memberRole === T.GroupMemberRole.Owner)
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
    const {owners} = this.options
    const owner = member.memberRole === T.GroupMemberRole.Owner
    return owners === undefined ? owner : owner && owners.some(entry => listsMember(entry, member))
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
    description: "Supervise your AI agent here: approve or reject its connections and messages with one tap.",
    groupPreferences: {history: {enable: T.GroupFeatureEnabled.On}, commands: controlCommands},
  })
  const customData = controlData(nextAction)
  await chat.apiSetGroupCustomData(group.groupId, customData)
  log(`Agent control chat "${displayName}" created.`)
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

export function cardText(action: Action, mode: Actions["mode"]): string {
  const request = requestLines(action)
  const approval = mode === "host" ? "Performed without approval (mode host)" : action.standing ? `Approved for this chat by ${action.decidedBy}` : `Approved by ${action.decidedBy}`
  switch (action.state) {
    case "pending":
      return action.interrupted
        ? [...request, "Cancelled: the server stopped."].join("\n")
        : [...request, commandsLine(action)].join("\n")
    case "performing":
      return [...request, `${approval}; interrupted: the server stopped.`].join("\n")
    case "done":
      return [...request, action.standing ? `${approval}. /'revoke ${action.chat}'` : `${approval}.`].join("\n")
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
  switch (action.tool) {
    case "connect":
      return `/'approve ${action.n}' or /'reject ${action.n}'`
    case "send":
      return `/'approve ${action.chat} ${action.n}', /'approve ${action.chat} all' or /'reject ${action.chat} ${action.n}'`
    case "leave":
      return `/'approve ${action.chat} ${action.n}' or /'reject ${action.chat} ${action.n}'`
  }
}

function pendingLine(action: Action): string {
  return `${action.n}  ${summary(action, safeName).what}  expires ${utcMinutes(action.expires)}`
}

function largestCard(action: Action, mode: Actions["mode"], approver: string): number {
  const decidedBy = action.decidedBy ?? approver
  const cards: Action[] = [action, {...action, state: "done", decidedBy}, {...action, state: "failed", decidedBy, error: longestError}]
  return Math.max(...cards.map(card => encodedLength(cardText(card, mode))))
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

function ownerRequest({keyword, params}: Command): OwnerRequest | undefined {
  const words = params === "" ? [] : params.split(/\s+/)
  const chat = words.length === 2 && isChatRef(words[0]) ? words[0] : undefined
  const value = words.length === 1 || chat !== undefined ? words.at(-1) : undefined
  if ((keyword === "approve" || keyword === "reject") && value !== undefined && /^\d+$/.test(value)) return {type: keyword, action: Number(value), chat}
  if (keyword === "approve" && chat !== undefined && value === "all") return {type: "approveChat", chat}
  if (keyword === "revoke" && chat === undefined && value !== undefined && isChatRef(value)) return {type: "revoke", chat: value}
  if ((keyword === "pending" || keyword === "list") && words.length === 0) return {type: keyword}
  return undefined
}

export function qrCodeText(link: string, size: QrCodeSize): string {
  const {modules} = qrcode.create(link, {errorCorrectionLevel: "L"})
  const positions = Array.from({length: modules.size + 2 * qrMargin}, (_, i) => i - qrMargin)
  const inside = (i: number): boolean => i >= 0 && i < modules.size
  const dark = (row: number, col: number): boolean => inside(row) && inside(col) && modules.get(row, col) !== 0
  const cell = (row: number, col: number): string => (dark(row, col) ? darkCell : lightCell)
  const halfCell = (top: boolean, bottom: boolean): string => {
    if (top === bottom) return top ? darkHalfCell : lightHalfCell
    return top ? darkTopCell : darkBottomCell
  }
  const lines = size === "large"
    ? positions.map(row => positions.map(col => cell(row, col)).join(""))
    : positions.filter((_, i) => i % 2 === 0).map(row => positions.map(col => halfCell(dark(row, col), dark(row + 1, col))).join(""))
  return lines.map(line => line + resetColors).join("\n")
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

function listsMember({memberId, name}: OwnerEntry, member: T.GroupMember): boolean {
  return member.groupMemberId === memberId && oneLine(member.memberProfile.displayName) === name
}

function sameMember(member: T.GroupMember): (other: T.GroupMember) => boolean {
  return other => other.groupMemberId === member.groupMemberId
}

function memberText({groupMemberId, memberProfile}: T.GroupMember, showName: (name: string) => string): string {
  return `${showName(memberProfile.displayName)} (member ${groupMemberId})`
}

function joinedText(member: T.GroupMember, showName: (name: string) => string): string {
  return `${showName(member.memberProfile.displayName)} joined (member ${member.groupMemberId})`
}

export function entryText({memberId, name}: OwnerEntry): string {
  return shellWord(`${memberId}:${name}`, /^\d+:[\p{L}\p{N}_-]+$/u)
}

function ownersText(members: T.GroupMember[]): string {
  return members.map(({groupMemberId, memberProfile}) => entryText({memberId: groupMemberId, name: oneLine(memberProfile.displayName)})).join(",")
}

function restartText(members: T.GroupMember[], shown: Shown): string {
  return `Restart the server with ${shown.command(`--owners ${ownersText(members)}`)}`
}
