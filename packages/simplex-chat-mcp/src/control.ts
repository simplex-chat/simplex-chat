import {CEvt, ChatEvent, T} from "@simplex-chat/types"
import {api, core} from "simplex-chat"
import {Action, ActionHooks, Actions, DownloadAction, Standing, Subject, ToolError, hasUnknownServers, leaveVerbs, subjectText, summary} from "./actions"
import {ExitError, chatErrorOf, errorText, fileError, log, logError, oneLine, serverHostname, shellWord} from "./log"
import {chatRef, fileContent, isChatRef, utcMinutes, validName, viewName} from "./messages"
import {linkCommand} from "./owner"
import {qrCodeText} from "./qr"
import type {OwnerEntry, QrCodeSize} from "./serve"
import {AgentTools, chatSubject, customDataOf, fitsMessage, isActive, isConnected, isGone, memberCurrent, refusedFileReason, setChatData} from "./tools"

export const controlCommands: T.ChatBotCommand[] = [
  {type: "command", keyword: "pending", label: "Pending actions"},
  {type: "command", keyword: "list", label: "Chats and approvals"},
  {type: "command", keyword: "leave", label: "Leave a chat", params: "<chat ID>"},
]

const commandsReply = "Commands: /approve, /reject, /receive, /forward, /revoke, /leave, /delete, /pending, /list. Each card shows its commands."
const sessionCommandsReply = "Commands: /approve, /reject, /receive, /forward, /revoke, /leave, /delete, /give, /pending, /list. Each card shows its commands."
const ownerKeywords = ["approve", "reject", "receive", "forward", "revoke", "leave", "delete", "give", "pending", "list"]
const cardTooLong = {
  send: "text: too long for its approval card; shorten it.",
  connect: "message: too long for its approval card; shorten it.",
  download: "item: the names of its file and chat are too long for its approval card.",
}
const standings: {[standing in Standing]: {key: string; approved: string; scope: string; revoke: (chat: string) => string; listed: string}} = {
  messages: {key: "simplexMcp", approved: "Approved for", scope: "Messages to", revoke: chat => `revoke ${chat}`, listed: "approved for this chat by"},
  files: {key: "simplexMcpFiles", approved: "Files approved for", scope: "Files from", revoke: chat => `receive ${chat} off`, listed: "files approved by"},
}
const maxCardLength = 15_000
const listedChats = 100
const maxLineLength = 100
const truncationMark = "…"
const longestError = "€".repeat(600)
const greeting = "I'm your agent. I ask here before I connect, send, download a file or leave a chat."
const noOwners = "No owners in the agent control chat."
const qrCodeHint = "If the QR code does not scan, restart with --qr-code large, or paste the link into the app."
const noSessionOwner = `The group of this session has no owner. An owner of the Main group joins it by its invitation there; without owners, run ${linkCommand} in a terminal.`

export interface ControlData {
  control: true
  nextAction: number
}

export interface ControlOptions {
  displayName: string
  owners?: OwnerEntry[]
  session?: SessionOptions
  registerHint: string
  qrCode: QrCodeSize
}

export interface SessionOptions {
  approves: (member: T.GroupMember) => boolean
  ownerOf: (session: number) => string | undefined
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
  | {type: "approve" | "reject" | "receive" | "forward"; action: number; chat?: string}
  | {type: "receiveAll"; action: number; chat: string}
  | {type: "approveChat" | "revoke"; chat: string; standing: Standing}
  | {type: "leave" | "deleteFiles"; chat: string}
  | {type: "give"; chat: string; session: number}
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
    private readonly tools: Pick<AgentTools, "knownChat" | "agentChats" | "leaveChat" | "forwardFile" | "deleteSentCopy" | "chatFiles" | "deleteChatFiles" | "giveChat">,
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
      this.group = {...(await this.chat.apiUpdateGroupProfile(this.groupId, {...groupProfile, displayName: this.options.displayName, groupPreferences: {...groupProfile.groupPreferences, commands: controlCommands}})), customData: this.group.customData}
    }
    await this.refreshMembers()
  }

  async rename(displayName: string): Promise<void> {
    this.options.displayName = displayName
    await this.load()
  }

  async describe(shortDescr: string): Promise<void> {
    if (this.group.groupProfile.shortDescr === shortDescr) return
    this.group = {...(await this.chat.apiUpdateGroupProfile(this.groupId, {...this.group.groupProfile, shortDescr})), customData: this.group.customData}
  }

  async start(): Promise<void> {
    if (this.actions.lost) return
    const {owners} = this.options
    if (owners === undefined) return this.startOwners()
    if (owners.length === 0) return this.startObserver()
    return this.startListed(owners)
  }

  refusal(action: Action): string | undefined {
    const {mode} = action
    const approver = this.longestApproverName()
    if (action.tool === "connect") action.linkTextBytes = maxCardLength - largestCard({...action, linkTextBytes: 0}, mode, approver)
    if (action.tool === "permission") action.previewBytes = maxCardLength - largestCard({...action, previewBytes: 0}, mode, approver)
    if (action.tool !== "leave" && action.tool !== "mode" && action.tool !== "permission" && action.tool !== "reply" && largestCard(action, mode, approver) > maxCardLength) return cardTooLong[action.tool]
    if (this.approvers().length > 0) return undefined
    if (this.options.session !== undefined) return noSessionOwner
    return this.options.owners === undefined
      ? "The agent has no owner. The person who runs it has to join its control chat with the link the server printed."
      : "The agent has no approving owner. The person who runs it has to restart it with the --owners options it printed."
  }

  hasApprover(): boolean {
    return this.approvers().length > 0
  }

  async onlyApprovers(userContactId?: number): Promise<boolean> {
    await this.refreshMembers()
    const own = (member: T.GroupMember): boolean => member.groupMemberId === this.group.membership.groupMemberId || (userContactId !== undefined && member.memberContactId === userContactId)
    return this.members.every(member => own(member) || isGone(member) || this.isApprover(member))
  }

  standingApprover(chatInfo: T.ChatInfo, standing: Standing): string | undefined {
    const approval = approvalOf(customDataOf(chatInfo), standings[standing].key)
    return this.actions.mode === "requests" && approval !== undefined && this.approvers().some(({groupMemberId}) => groupMemberId === approval.approvedById) ? safeName(approval.approvedBy) : undefined
  }

  saveCounter(nextAction: number): Promise<void> {
    return this.saveData(controlData(nextAction))
  }

  saveData(data: object): Promise<void> {
    const write = this.writes.then(async () => {
      const customData = {...this.group.customData, ...data}
      await this.chat.apiSetGroupCustomData(this.groupId, customData)
      this.group = {...this.group, customData}
    })
    this.writes = write.catch(() => undefined)
    return write
  }

  async postCard(action: Action): Promise<number> {
    return this.postMessage({msgContent: await this.cardContent(action), mentions: {}})
  }

  async outcome(action: Action): Promise<void> {
    if (!hasOutcomeCard(action)) return
    const content = await this.cardContent(action)
    if (action.card === undefined) await this.postMessage({msgContent: content, mentions: {}})
    else await this.chat.apiUpdateChatItem(T.ChatType.Group, this.groupId, action.card, content, false)
    const filesNote = action.tool === "leave" && action.state === "done" ? this.leftChatFilesNote(action.chat, action.subject) : undefined
    if (filesNote !== undefined) await this.post(filesNote)
  }

  refusedFile(path: string, chatInfo: T.ChatInfo, chat: string): void {
    const refused = (shown: Shown, shownPath: string): string => `Refused to send ${shownPath} to ${chatName(chatSubject(chatInfo), shown.name)} (${chat}): ${refusedFileReason}.`
    log(refused(inLog, oneLine(path)))
    if (!this.actions.lost) this.post(refused(inChat, codeSpan(oneLine(path)))).catch(error => logError("refused file", error))
  }

  fileNotSent({chatItem_, errorMessage}: CEvt.SndFileError): void {
    const file = chatItem_?.chatItem.file
    const chat = chatItem_ === undefined ? undefined : chatRef(chatItem_.chatInfo)
    if (chatItem_ === undefined || file === undefined || chat === undefined) return logError("file not sent", errorMessage)
    const {chatInfo} = chatItem_
    const reason = fileError(file) ?? errorMessage
    const notSent = (shown: Shown, name: string, error: string): string => `The file ${name} was not sent to ${chatName(chatSubject(chatInfo), shown.name)} (${chat}): ${error}.`
    log(notSent(inLog, oneLine(file.fileName), oneLine(reason)))
    if (!this.actions.lost) this.post(notSent(inChat, codeSpan(oneLine(file.fileName)), codeSpan(oneLine(reason)))).catch(error => logError("file not sent", error))
  }

  onMessage(ci: T.AChatItem): void {
    const {chatItem} = ci
    if (this.actions.lost || chatItem.chatDir.type !== "groupRcv" || chatItem.content.type !== "rcvMsgContent") return
    const command = parseCommand(chatItem.content.msgContent.text)
    if (command === undefined) return
    const sender = chatItem.chatDir.groupMember
    if (this.options.session !== undefined && !ownerKeywords.includes(command.keyword) && this.isApprover(sender)) return
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
    if (this.options.session !== undefined) return
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
    if (request === undefined) return this.options.session === undefined ? commandsReply : sessionCommandsReply
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
        return this.approveChat(sender, request.chat, request.standing)
      case "revoke":
        return this.revoke(request.chat, request.standing)
      case "leave":
        return this.leave(sender, request.chat)
      case "deleteFiles":
        return this.deleteFiles(request.chat)
      case "give":
        return this.give(sender, request.chat, request.session)
      case "approve":
      case "reject":
      case "receive":
      case "forward":
      case "receiveAll": {
        const action = this.actions.get(request.action)
        if (action === undefined || action.state !== "pending" || action.card === undefined) return `Action ${request.action} is not pending.`
        if (request.chat !== undefined && (action.tool === "connect" || action.chat !== request.chat)) return `Action ${request.action} is not for ${request.chat}.`
        const approver = safeName(sender.memberProfile.displayName)
        if (request.type === "reject") this.actions.reject(action, approver)
        else if (action.tool !== "download") {
          if (request.type !== "approve") return `Action ${request.action} is not a download.`
          this.actions.approve(action, approver)
        } else if (request.type === "approve") return `Action ${action.n} is a download: ${commandsLine(action)}.`
        else if (request.type === "receive") this.actions.approve(action, approver)
        else if (request.type === "forward") this.forward(action)
        else return this.approveChat(sender, action.chat, "files", action)
        return undefined
      }
    }
  }

  private async approveChat(sender: T.GroupMember, chat: string, standing: Standing, named?: Action): Promise<string> {
    const {key, approved, revoke} = standings[standing]
    const chatInfo = await this.agentChat(chat)
    if (chatInfo === undefined) return `No chat ${chat}. /list shows the agent's chats.`
    const approvedBy = sender.memberProfile.displayName
    const approval: ChatApproval = {approvedBy, approvedById: sender.groupMemberId, approvedAt: new Date().toISOString()}
    await this.setCustomData(chat, {...customDataOf(chatInfo), [key]: approval})
    const approver = safeName(approvedBy)
    const pending = this.actions.pending().filter(action => (named === undefined ? action.tool === "send" && action.chat === chat && action.card !== undefined : action === named))
    for (const action of pending) this.actions.approve(action, approver)
    return `${approved} ${chatName(chatSubject(chatInfo))} (${chat}) by ${approver}. /'${revoke(chat)}'`
  }

  private async revoke(chat: string, standing: Standing): Promise<string> {
    const {key, scope} = standings[standing]
    const chatInfo = await this.agentChat(chat)
    if (chatInfo === undefined || approvalOf(customDataOf(chatInfo), key) === undefined) return `${scope} ${chat} have no standing approval.`
    const {[key]: _, ...customData} = customDataOf(chatInfo) as {[key: string]: unknown}
    await this.setCustomData(chat, customData)
    return `${scope} ${chatName(chatSubject(chatInfo))} (${chat}) need approval again.`
  }

  private forward(action: DownloadAction): void {
    this.tools
      .forwardFile(action)
      .then(copy =>
        this.cardContent(action)
          .then(card => {
            const msgContent = fileContent(action.file, "")
            return this.postMessage({fileSource: {filePath: copy}, msgContent, quotedItemId: fitsMessage(msgContent, action.file.name, {content: card}) ? action.card : undefined, mentions: {}})
          })
          .catch(error => {
            this.tools.deleteSentCopy(copy)
            throw error
          }),
      )
      .catch(error => this.post(`Action ${action.n}: the file was not received: ${codeSpan(oneLine(errorText(error)))}.`))
      .catch(error => logError(`action ${action.n} file`, error))
  }

  private async leave(sender: T.GroupMember, chat: string): Promise<string> {
    const chatInfo = await this.agentChat(chat)
    if (chatInfo === undefined) return `No chat ${chat}. /list shows the agent's chats.`
    await this.tools.leaveChat(chat)
    const {past} = leaveVerbs({chat})
    const left = (shown: Shown): string => `${past[0].toUpperCase()}${past.slice(1)} ${chatName(chatSubject(chatInfo), shown.name)} (${chat}), requested by ${shown.name(sender.memberProfile.displayName)}.`
    log(left(inLog))
    const approver = safeName(sender.memberProfile.displayName)
    for (const action of this.actions.pending()) {
      if (action.chat === chat && action.card !== undefined) this.actions.reject(action, approver)
    }
    const filesNote = this.leftChatFilesNote(chat, chatSubject(chatInfo))
    if (filesNote !== undefined) this.queueAfterReplies(() => this.post(filesNote))
    return left(inChat)
  }

  private leftChatFilesNote(chat: string, subject: Subject): string | undefined {
    const {folder, count, bytes} = this.tools.chatFiles(chat)
    return count === 0 ? undefined : `Files from ${chatName(subject)} (${chat}) are kept in ${codeSpan(oneLine(folder))}: ${filesText(count)}, ${sizeText(bytes)}. /'delete ${chat} files'`
  }

  private async deleteFiles(chat: string): Promise<string> {
    const {count, bytes} = await this.tools.deleteChatFiles(chat)
    return count === 0 ? `No files from ${chat}.` : `Deleted ${filesText(count)} from ${chat}, ${sizeText(bytes)}.`
  }

  private async give(sender: T.GroupMember, chat: string, session: number): Promise<string> {
    if (this.options.session === undefined) return commandsReply
    const chatInfo = await this.agentChat(chat)
    if (chatInfo === undefined) return `No chat ${chat}. /list shows the agent's chats.`
    const owner = this.options.session.ownerOf(session)
    if (owner === undefined) return `No session ${session}. /sessions in the Main group lists the sessions.`
    await this.tools.giveChat(chat, owner)
    const approver = safeName(sender.memberProfile.displayName)
    for (const action of this.actions.pending()) {
      if (action.chat === chat && action.card !== undefined) this.actions.reject(action, approver)
    }
    return `Gave ${chatName(chatSubject(chatInfo))} (${chat}) to session ${session}.`
  }

  private queueAfterReplies(post: () => Promise<unknown>): void {
    this.ownerCommands = this.ownerCommands.then(post).catch(error => logError("control chat command", error))
  }

  private async chatsText(): Promise<string> {
    const chats = await this.tools.agentChats()
    const lines = chats.slice(0, listedChats).map(({chatInfo}) => {
      const approvals = (["messages", "files"] as const).map(standing => {
        const approvedBy = this.standingApprover(chatInfo, standing)
        return approvedBy === undefined ? "" : `, ${standings[standing].listed} ${approvedBy}`
      })
      const connection = isConnected(chatInfo) ? "" : ", not connected"
      return truncatedLine(`${chatRef(chatInfo)}  ${chatName(chatSubject(chatInfo))}${approvals.join("")}${connection}`, maxLineLength)
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
    return setChatData(this.chat, chat, customData)
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
    log(this.options.session === undefined ? `The agent lost its control chat. Restart the server with --reset-owners.` : `The group "${this.options.displayName}" is lost.`)
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
    return this.postMessage({msgContent: {type: "text", text}, mentions: {}})
  }

  private async postMessage(message: T.ComposedMessage): Promise<number> {
    const [item] = await this.chat.apiSendMessages([T.ChatType.Group, this.groupId], [message])
    return item.chatItem.meta.itemId
  }

  private async cardContent(action: Action): Promise<T.MsgContent> {
    const text = cardText(action, action.mode)
    const file = action.tool === "download" || action.tool === "send" ? action.file : undefined
    const content = file === undefined ? undefined : fileContent(file, text)
    if (content?.type !== "image" && content?.type !== "video") return {type: "text", text}
    action.cardImage ??= await this.fittedImage(action, content)
    return action.cardImage === "" ? {type: "text", text} : {...content, image: action.cardImage}
  }

  private async fittedImage(action: Action, content: T.MsgContent.Image | T.MsgContent.Video): Promise<string> {
    const {image} = content
    const fields = JSON.stringify({...content, text: "", image: ""}).length - JSON.stringify({type: "text", text: ""}).length
    const budget = maxCardLength - largestCard(action, action.mode, this.longestApproverName()) - fields
    if (encodedLength(image) <= budget) return image
    try {
      return await core.chatResizeImageUri(Buffer.from(image.slice(image.indexOf(",") + 1), "base64"), budget)
    } catch (error) {
      logError(`action ${action.n} card image`, error)
      return ""
    }
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
    const {owners, session} = this.options
    if (session !== undefined) return session.approves(member)
    const owner = member.memberRole === T.GroupMemberRole.Owner
    return owners === undefined ? owner : owner && owners.some(entry => listsMember(entry, member))
  }
}

export function hasOutcomeCard(action: Action): boolean {
  return action.tool !== "download" || action.card !== undefined
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
  const group = await createGroup(chat, userId, {
    displayName,
    fullName: "",
    description: "Supervise your AI agent here: approve or reject its connections and messages with one tap.",
    groupPreferences: {history: {enable: T.GroupFeatureEnabled.On}, commands: controlCommands},
  }, controlData(nextAction))
  log(`Agent control chat "${displayName}" created.`)
  return group
}

export async function createGroup(chat: api.ChatApi, userId: number, profile: T.GroupProfile, customData: object): Promise<T.GroupInfo> {
  const group = await chat.apiNewGroup(userId, profile)
  await chat.apiSetGroupCustomData(group.groupId, customData)
  return {...group, customData}
}

export async function resetControlChat(chat: api.ChatApi, userId: number, group: T.GroupInfo, displayName: string, nextAction: number): Promise<T.GroupInfo> {
  await deleteGroup(chat, group)
  return createControlChat(chat, userId, displayName, nextAction)
}

export async function deleteGroup(chat: api.ChatApi, {groupId, membership}: T.GroupInfo): Promise<void> {
  if (memberCurrent(membership) && membership.memberRole !== T.GroupMemberRole.Owner) await chat.apiLeaveGroup(groupId)
  await chat.apiDeleteChat(T.ChatType.Group, groupId, {type: "full", notify: true})
}

export function isGroupMessage({chatInfo}: T.AChatItem, groupId: number): boolean {
  return chatInfo.type === "group" && chatInfo.groupInfo.groupId === groupId && chatInfo.groupChatScope === undefined
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

export function isOwnerCommand(text: string): boolean {
  const command = parseCommand(text)
  return command !== undefined && ownerKeywords.includes(command.keyword)
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
      return [...request, action.ended ? "Ended." : "Expired."].join("\n")
  }
}

function requestLines(action: Action): string[] {
  const done = action.state === "done"
  if (action.tool === "mode") return [`${done ? "I run" : "I want to run"} in mode ${action.input.mode}: Claude Code asks you before each of my actions, and they are performed without approval here.`]
  if (action.tool === "permission") {
    const {toolName, description, inputPreview} = action.input
    return [`I want to use ${codeSpan(oneLine(toolName))}: ${codeSpan(oneLine(description))}`, ...fittedLines(codeLines(inputPreview), action.previewBytes)]
  }
  const subject = subjectText(action.subject, safeName)
  switch (action.tool) {
    case "connect": {
      const {input} = action
      const verb = !done ? "I want to connect" : action.connected ? "I connected" : "I asked to connect"
      const message = input.message === undefined ? [] : ["With the message:", ...codeLines(input.message)]
      return [`${verb} to ${subject}, ${action.subject.incognito ? "incognito" : "as myself"}.`, ...fittedLines([...codeLines(action.plan.description), ...codeLines(action.plan.welcome)], action.linkTextBytes), ...message]
    }
    case "send": {
      const {file, replyTo, mentions} = action
      const attaching = file === undefined ? [] : [`attaching ${file.type} ${codeSpan(oneLine(file.path))}, ${sizeText(file.size)}`]
      const reply = replyTo === undefined ? [] : [`replying to ${safeName(replyTo.from)}: ${codeSpan([...replyTo.text.replace(/\s+/g, " ")].slice(0, 100).join(""))}`]
      const mentioning = mentions.length === 0 ? [] : [`mentioning ${mentions.map(({name}) => mentionText(name)).join(", ")}`]
      return [`${done ? "I sent" : "I want to send"} a message to ${subject}${action.subject.incognito ? ", where I am incognito" : ""}:`, ...codeLines(action.input.text), ...attaching, ...reply, ...mentioning]
    }
    case "reply": {
      const {file} = action
      return [`${done ? "I sent" : "I want to send"} a file to ${subject}, where members who are not owners see it:`, `${codeSpan(oneLine(file.name))}, ${sizeText(file.size)}`]
    }
    case "leave": {
      const {present, past} = leaveVerbs(action)
      return [`${done ? `I ${past}` : `I want to ${present}`} ${chatName(action.subject)} (${action.chat}).`]
    }
    case "download": {
      const {from, file, unknownServers} = action
      const source = `${from === undefined ? "" : `${safeName(from)} in `}${chatName(action.subject)} (${action.chat})`
      const servers = unknownServers === undefined ? [] : [serversLine(unknownServers)]
      return [`${done ? "I downloaded" : "I want to download"} a file from ${source}:`, `${codeSpan(oneLine(file.name))}, ${sizeText(file.size)}`, ...servers]
    }
  }
}

function commandsLine(action: Action): string {
  switch (action.tool) {
    case "connect":
    case "mode":
    case "permission":
    case "reply":
      return `/'approve ${action.n}' or /'reject ${action.n}'`
    case "send":
      return `/'approve ${action.chat} ${action.n}', /'approve ${action.chat} all' or /'reject ${action.chat} ${action.n}'`
    case "leave":
      return `/'approve ${action.chat} ${action.n}' or /'reject ${action.chat} ${action.n}'`
    case "download": {
      const all = hasUnknownServers(action) ? "" : `/'receive ${action.chat} ${action.n} all', `
      return `/'receive ${action.chat} ${action.n}', ${all}/'forward ${action.chat} ${action.n}' or /'reject ${action.chat} ${action.n}'`
    }
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

function fittedLines(lines: string[], budget = Infinity): string[] {
  const fitted: string[] = []
  let left = budget
  for (const line of lines) {
    const length = encodedLength(`\n${line}`)
    if (length > left) {
      const room = left - encodedLength("\n``")
      if (room >= encodedLength(truncationMark)) fitted.push(`\`${truncatedLine(line.slice(1, -1), room)}\``)
      break
    }
    fitted.push(line)
    left -= length
  }
  return fitted
}

function chatName(subject: Subject, showName: (name: string) => string = safeName): string {
  return subjectText({...subject, simplexName: undefined}, showName)
}

function serversLine(unknownServers: string[]): string {
  if (unknownServers.length === 0) return "Servers: known."
  const hosts = unknownServers.map(server => codeSpan(oneLine(serverHostname(server)))).join(", ")
  return `Servers: ${hosts}, unknown: downloading shows ${unknownServers.length === 1 ? "it" : "them"} this computer's IP address.`
}

function sizeText(bytes: number): string {
  const exponent = Math.min(Math.floor(Math.log10(Math.max(bytes, 1)) / 3), 3)
  return exponent === 0 ? `${bytes} bytes` : `${(bytes / 1000 ** exponent).toFixed(1)} ${["KB", "MB", "GB"][exponent - 1]}`
}

function filesText(count: number): string {
  return `${count} file${count === 1 ? "" : "s"}`
}

function codeLines(text: string | undefined): string[] {
  return text === undefined ? [] : text.split("\n").map(line => (line.trim() === "" ? "" : codeSpan(line)))
}

export function codeSpan(text: string): string {
  return `\`${text.replace(/`/g, "'").replace(/^ | $/g, "\u00a0")}\``
}

export function safeName(name: string): string {
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

function approvalOf(customData: object | undefined, key: string): {approvedBy: string; approvedById: number} | undefined {
  const data = (customData as {[key: string]: Partial<ChatApproval> | undefined} | undefined)?.[key]
  return typeof data?.approvedBy === "string" && typeof data.approvedById === "number" ? {approvedBy: data.approvedBy, approvedById: data.approvedById} : undefined
}

function ownerRequest({keyword, params}: Command): OwnerRequest | undefined {
  const words = params === "" ? [] : params.split(/\s+/)
  const chat = words.length === 2 && isChatRef(words[0]) ? words[0] : undefined
  const value = words.length === 1 || chat !== undefined ? words.at(-1) : undefined
  if ((keyword === "approve" || keyword === "reject" || keyword === "receive" || keyword === "forward") && value !== undefined && /^\d+$/.test(value)) return {type: keyword, action: Number(value), chat}
  if (keyword === "receive" && words.length === 3 && isChatRef(words[0]) && /^\d+$/.test(words[1]) && words[2] === "all") return {type: "receiveAll", action: Number(words[1]), chat: words[0]}
  if (keyword === "receive" && chat !== undefined && value === "off") return {type: "revoke", chat, standing: "files"}
  if (keyword === "approve" && chat !== undefined && value === "all") return {type: "approveChat", chat, standing: "messages"}
  if (keyword === "revoke" && chat === undefined && value !== undefined && isChatRef(value)) return {type: keyword, chat: value, standing: "messages"}
  if (keyword === "leave" && chat === undefined && value !== undefined && isChatRef(value)) return {type: keyword, chat: value}
  if (keyword === "delete" && chat !== undefined && value === "files") return {type: "deleteFiles", chat}
  if (keyword === "give" && chat !== undefined && value !== undefined && /^\d+$/.test(value)) return {type: keyword, chat, session: Number(value)}
  if ((keyword === "pending" || keyword === "list") && words.length === 0) return {type: keyword}
  return undefined
}

export function losesControl(event: ChatEvent): boolean {
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

export function isLost({membership}: T.GroupInfo): boolean {
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

export function joinedText(member: T.GroupMember, showName: (name: string) => string): string {
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
