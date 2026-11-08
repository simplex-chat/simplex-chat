import {mkdirSync, rmSync} from "fs"
import {dirname, join} from "path"
import {ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Action, Actions, HostTexts, Mode, isDecided} from "./actions"
import {Config} from "./config"
import {Control, canSend, controlCommands, controlDataOf, createGroup, isControlEvent, isGroupMessage, parseCommand, rightsText, safeName, sessionApproverRoles} from "./control"
import {openCore} from "./core"
import {SessionFile, hubFolder, readSessionFile, sessionFolder, writeSessionFile} from "./files"
import {chatErrorOf, errorText, log, logError, oneLine} from "./log"
import {MainGroup, SessionGroups, createMainGroup, findMainGroup} from "./main-group"
import {Changes, ChatItemsWatch, ToolCall, getChat, nextChange, storedName, validName} from "./messages"
import {AttachLine, OwnerState, PermissionRequest, ResetReply, TurnRequest} from "./protocol"
import {waitForCancelledCards} from "./serve"
import {AgentTools, ToolName, fitsMessage, maxMessageLength, ownerOf} from "./tools"
import {User, userContact} from "./user"

const profileName = "Claude"
const sessionKey = "simplexMcpSession"
const firstAction = 1
const cardsWait = 10
const promptEcho = 60_000
const maxPushed = 20
const fileWait = 10 * 60_000
const pendingDays = 1
const attachWait = 30_000
const channelPrompt = /<channel source="plugin:simplex:simplex"[^\n]* item="(\d+)"[^\n]*>\n([\s\S]*?)\n<\/channel>/g
const bangText = "Messages starting with ! reach Claude only from admins and owners, as /!command."
const commandsAdminsOnly = "Only admins and owners of this group can send // and /! commands to Claude."
const commandsApproversOnly = "Only admins and owners of this group can send / commands."

export const hubTexts: HostTexts = {lost: "The group of this session is lost. Start a new Claude Code session.", stopping: "The SimpleX daemon is stopping."}

export interface StoredSession {
  id: string
  session: SessionFile
}

interface SessionGroupData {
  session: number
  pending?: {connId: number; at: number}[]
  mode?: Mode
}

export class Session {
  open = false
  readonly actions: Actions
  readonly tools: AgentTools
  readonly control: Control
  readonly number: number
  private name: string
  private readonly pushed: {text: string; item: T.ChatItem; at: number; quoted?: boolean}[] = []
  private readonly receiving = new Map<number, {resolve: () => void; reject: (error: Error) => void}>()
  private description: {model?: string; effort?: string}
  private turns: {at: number; item: T.ChatItem | null}[] = []
  private answeredAt = -Infinity
  private requested?: Mode
  private hostDeclined: boolean
  private writer?: (line: AttachLine) => void
  private readonly attaching = new Changes()
  private delivery: Promise<void> = Promise.resolve()

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
    }
    this.control = new Control(chat, group, {displayName: group.groupProfile.displayName, session, registerHint: "", qrCode: "small"}, this.actions, this.tools)
  }

  get displayName(): string {
    return groupName(this.number, this.name)
  }

  get onLine(): ((line: AttachLine) => void) | undefined {
    return this.writer
  }

  set onLine(writer: ((line: AttachLine) => void) | undefined) {
    this.writer = writer
    this.attaching.notify()
  }

  busy(): boolean {
    return this.actions.all().some(({tool}) => tool !== "mode")
  }

  call(tool: ToolName, input: unknown, call: ToolCall): ReturnType<AgentTools["call"]> {
    return this.tools.call(tool, input, call)
  }

  async rename(name: string): Promise<void> {
    this.name = name
    await this.control.rename(this.displayName)
  }

  requestMode(mode: Mode): void {
    this.requested = mode
    if (mode === "host") return this.askMode()
    for (const action of this.actions.pending()) if (action.tool === "mode") this.actions.end(action)
    if (this.hostDeclined) this.declineHost(false)
    if (this.actions.mode !== mode) this.setMode(mode)
  }

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

  onMessage(ci: T.AChatItem): void {
    const {chatDir, content} = ci.chatItem
    if (chatDir.type !== "groupRcv" || content.type !== "rcvMsgContent") return
    const {text} = content.msgContent
    if (this.refusesCommand(text, chatDir.groupMember)) return this.queue(() => this.post([commandsApproversOnly], ci.chatItem.meta.itemId))
    if (!text.startsWith("//") && !text.startsWith("/!") && parseCommand(text) !== undefined) return this.control.onMessage(ci)
    this.queue(() => this.prompt(ci, text, chatDir.groupMember))
  }

  claimPending(connId: number, chat: string): void {
    const recent = Date.now() - pendingDays * 86_400_000
    if (!this.data.pending?.some(entry => entry.connId === connId && entry.at > recent)) return
    this.saveData({...this.data, pending: this.data.pending.filter(entry => entry.connId !== connId)}).catch(error => logError(`session ${this.number}`, error))
    this.tools.claim(chat).catch(error => logError(`owner of ${chat}`, error))
  }

  permission({requestId, toolName, description, inputPreview}: PermissionRequest): void {
    const action = this.actions.create(`permission ${requestId}`, {tool: "permission", input: {requestId, toolName, description, inputPreview}}, undefined, this.number, "requests")
    const unsubscribe = action.changes.subscribe(() => {
      const behavior = action.state === "performing" ? "allow" : action.state === "rejected" ? "deny" : undefined
      if (behavior !== undefined) this.onLine?.({permission: {requestId, behavior}})
      if (!isDecided(action.state)) return
      unsubscribe()
      this.actions.remove(action)
    })
  }

  async turn({at, prompt, answer, model, effort}: TurnRequest): Promise<void> {
    if (prompt !== undefined || answer !== undefined) this.endPermissions(at)
    const quoted = answer === undefined ? undefined : this.turnQuote(at ?? Infinity)
    if (answer !== undefined) this.answeredAt = Math.max(this.answeredAt, at ?? Date.now())
    const matched = prompt === undefined ? [] : this.pushedIn(prompt)
    if (prompt !== undefined && (at ?? Date.now()) >= this.answeredAt) this.turns.push({at: at ?? Date.now(), item: matched[0]?.item ?? null})
    this.pushed.splice(0, this.pushed.length, ...this.pushed.filter(entry => !matched.includes(entry)))
    const described = (model !== undefined && model !== this.description.model) || (effort !== undefined && effort !== this.description.effort)
    if (described) {
      this.description = {model: model ?? this.description.model, effort: effort ?? this.description.effort}
      this.store(this.description)
    }
    try {
      if (prompt !== undefined && prompt.replace(channelPrompt, "").trim() !== "" && matched.length === 0 && parseCommand(prompt) === undefined) await this.hub.user?.post(this.number, textParts(prompt))
      if (answer !== undefined && answer.trim() !== "") await this.answer(answer, quoted)
    } finally {
      const {model: shown, effort: level} = this.description
      if (described) await this.control.describe([shown, level === undefined ? undefined : `effort ${level}`].filter(part => part !== undefined).join(" · "))
    }
  }

  endPermissions(before = Infinity): void {
    for (const action of this.actions.pending()) {
      if (action.tool === "permission" && action.created < before) this.actions.end(action)
    }
  }

  fileDone(fileId: number, error?: string): void {
    const waiting = this.receiving.get(fileId)
    this.receiving.delete(fileId)
    if (error === undefined) waiting?.resolve()
    else waiting?.reject(new Error(error))
  }

  async invite(member: T.GroupMember, role: T.GroupMemberRole): Promise<void> {
    await this.hub.chat.apiAddMemberViaGroup(this.control.groupId, this.hub.main.groupId, member.groupMemberId, role)
  }

  async announceRights(event: ChatEvent): Promise<void> {
    if (this.actions.lost || (event.type !== "joinedGroupMember" && event.type !== "connectedToGroupMember" && event.type !== "memberRole")) return
    const {member} = event
    if (this.isUser(member)) return
    const name = safeName(member.memberProfile.displayName)
    const observer = canSend(member.memberRole) && member.memberRole !== T.GroupMemberRole.Owner ? ` You can change role to observer to stop it: /'observer #${this.control.groupId} ${member.groupMemberId}'` : ""
    await this.post([`${rightsText(name, member.memberRole)}${observer}`])
  }

  private async prompt(ci: T.AChatItem, text: string, member: T.GroupMember): Promise<void> {
    if (this.isUser(member)) return
    const item = ci.chatItem.meta.itemId
    const command = text.startsWith("//") || text.startsWith("/!")
    if (command && !this.approves(member)) return this.post([commandsAdminsOnly], item)
    if (text.startsWith("!")) return this.post([bangText], item)
    await this.deliver(ci, command ? text.slice(1) : text, member)
  }

  private async deliver(ci: T.AChatItem, text: string, member: T.GroupMember): Promise<void> {
    const left = this.hub.promptsHeldUntil - Date.now()
    if (this.onLine === undefined && left > 0) await nextChange(this.attaching, new AbortController().signal, left)
    const {chatItem} = ci
    const file = this.onLine === undefined || chatItem.file === undefined ? undefined : await this.receive(chatItem.file).catch(async error => {
      await this.post([`The file of this message was not received: ${oneLine(errorText(error))}`], chatItem.meta.itemId)
      return undefined
    })
    if (this.onLine === undefined) {
      await this.post([`Session ${this.number} is not running.`], chatItem.meta.itemId)
      return
    }
    if (this.pushed.push({text, item: chatItem, at: Date.now()}) > maxPushed) this.pushed.shift()
    this.onLine({prompt: {text, from: member.memberProfile.displayName, item: chatItem.meta.itemId, ...(file === undefined ? {} : {file})}})
  }

  private async receive({fileId, fileName}: T.CIFile): Promise<string> {
    const path = join(sessionFolder(this.hub.root, this.id), "files", "group", storedName(fileId, fileName))
    mkdirSync(dirname(path), {recursive: true, mode: 0o700})
    const received = new Promise<void>((resolve, reject) => this.receiving.set(fileId, {resolve, reject}))
    const timer = setTimeout(() => this.fileDone(fileId, "not received in 10 minutes"), fileWait).unref()
    await this.hub.chat.apiReceiveFile(fileId, path, true).catch(error => this.fileDone(fileId, errorText(error)))
    await received.finally(() => clearTimeout(timer))
    return path
  }

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

  private async answer(text: string, quoted: T.ChatItem | undefined): Promise<void> {
    const quote = quoted?.content.type === "rcvMsgContent" ? {content: quoted.content.msgContent, fileName: quoted.file?.fileName} : undefined
    const quotes = quote !== undefined && fitsMessage({type: "text", text: [...text].slice(0, 1).join("")}, undefined, quote)
    await this.post(textParts(text, quotes ? quote : undefined), quotes ? quoted?.meta.itemId : undefined)
  }

  private async post(parts: string[], quotedItemId?: number): Promise<void> {
    const send = (quote?: number) => this.hub.chat.apiSendMessages([T.ChatType.Group, this.control.groupId], parts.map((part, i) => ({msgContent: {type: "text", text: part}, quotedItemId: i === 0 ? quote : undefined, mentions: {}})))
    await send(quotedItemId).catch(error => {
      if (quotedItemId === undefined || !isRefusedQuote(error)) throw error
      return send()
    })
  }

  private pushedIn(prompt: string): {text: string; item: T.ChatItem; at: number}[] {
    const recent = Date.now() - promptEcho
    const tags = [...prompt.matchAll(channelPrompt)].map(([, item, content]) => ({item: Number(item), content: content.trim()}))
    return this.pushed.filter(entry => (entry.text === prompt && entry.at > recent) || tags.some(({item, content}) => item === entry.item.meta.itemId && content === entry.text.trim()))
  }

  private queue(work: () => Promise<void>): void {
    this.delivery = this.delivery.then(work).catch(error => logError(`prompt of session ${this.number}`, error))
  }

  private refusesCommand(text: string, member: T.GroupMember): boolean {
    return (text.startsWith("//") || text.startsWith("/!") || parseCommand(text) !== undefined) && !this.approves(member) && !this.isUser(member)
  }

  private approves(member: T.GroupMember): boolean {
    return sessionApproverRoles.includes(member.memberRole) && !this.isUser(member)
  }

  private isUser(member: T.GroupMember): boolean {
    return member.memberContactId !== undefined && member.memberContactId === this.hub.userContactId
  }

  private setMode(mode: Mode): void {
    this.actions.mode = mode
    this.saveData({...this.data, mode}).catch(error => logError(`session ${this.number}`, error))
  }

  private declineHost(declined: boolean): void {
    this.hostDeclined = declined
    this.store({hostDeclined: declined || undefined})
  }

  private store(fields: Partial<SessionFile>): void {
    const folder = sessionFolder(this.hub.root, this.id)
    const stored = readSessionFile(folder)
    if (stored !== undefined) writeSessionFile(folder, {...stored, ...fields})
  }

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
  private stopped = false
  private controlEvents: Promise<void> = Promise.resolve()

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

  static async open(root: string, config: Config, stored: StoredSession[]): Promise<Hub> {
    const core = await openCore(hubFolder(root), profileName)
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
      return hub
    } catch (error) {
      await core.chat.close().catch(() => undefined)
      throw error
    }
  }

  get mainGroup(): string {
    return this.main.displayName
  }

  owner(): OwnerState {
    const {link} = this.main
    return {owners: this.main.owners().map(({memberProfile}) => memberProfile.displayName), ...(link === undefined ? {} : {link})}
  }

  async configure(config: Config): Promise<void> {
    this.expireHours = config.expireHours
    for (const {actions} of this.sessions.values()) actions.expireHours = config.expireHours
    if (config.mainGroup !== this.main.displayName) await this.main.rename(config.mainGroup)
  }

  async resetOwners(): Promise<ResetReply> {
    if (this.main.owners().length === 0) return {link: await this.main.replace()}
    await this.main.askReset()
    return {}
  }

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

  sessionsText(): string {
    const lines = [...this.sessions.values()].map(session => `${session.displayName}${session.open ? "" : ", closed"}`)
    return lines.length > 0 ? lines.join("\n") : "No sessions."
  }

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

  async inviteOwner(member: T.GroupMember): Promise<void> {
    for (const session of this.sessions.values()) {
      if (session.open) await session.invite(member, T.GroupMemberRole.Owner).catch(error => logError(`invitation to session ${session.number}`, error))
    }
  }

  expireDue(): void {
    for (const {actions} of this.sessions.values()) actions.expireDue()
  }

  async stop(): Promise<Action[]> {
    this.stopped = true
    const sessions = [...this.sessions.values()]
    const stopped = await Promise.all(sessions.map(async ({actions}) => ({lost: actions.lost, cancelled: await actions.stop()})))
    await this.controlEvents
    await waitForCancelledCards(stopped.flatMap(({lost, cancelled}) => (lost ? [] : cancelled)), cardsWait, new AbortController().signal)
    for (const {tools} of sessions) tools.close()
    await this.user?.close()
    await this.chat.close()
    return stopped.flatMap(({cancelled}) => cancelled)
  }

  private async storedGroup({group, number}: SessionFile): Promise<T.GroupInfo | undefined> {
    if (group === undefined) return undefined
    const chat = await getChat(this.chat, `#${group}`, 0).catch(() => undefined)
    const groupInfo = chat?.chatInfo.type === "group" ? chat.chatInfo.groupInfo : undefined
    return sessionDataOf(groupInfo?.customData)?.session === number ? groupInfo : undefined
  }

  private async createSessionGroup({number, name}: SessionFile): Promise<T.GroupInfo> {
    const displayName = groupName(number, name)
    const group = await createGroup(this.chat, this.profile.userId, {
      displayName,
      fullName: "",
      description: `The Claude Code session ${number}. Members send it prompts; admins and owners also send / commands and approve tool use.`,
      groupPreferences: {history: {enable: T.GroupFeatureEnabled.On}, directMessages: {enable: T.GroupFeatureEnabled.Off}, commands: controlCommands},
    }, {[sessionKey]: {session: number}})
    const user = await userContact(this.chat, this.profile.userId)
    if (user !== undefined) await this.chat.apiAddMember(group.groupId, user.contactId, T.GroupMemberRole.Member).catch(error => logError(`User in session ${number}`, error))
    log(`Group "${displayName}" of session ${number} created.`)
    return group
  }

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
      this.controlEvents = this.controlEvents.then(() => session.control.onEvent(event)).then(() => session.announceRights(event)).then(() => session.askMode()).catch(error => logError(`${event.type} in the group of session ${session.number}`, error))
    }
  }
}

function groupName(number: number, name: string): string {
  return validName(`${number} · ${name}`)
}

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

function isRefusedQuote(error: unknown): boolean {
  const chatError = chatErrorOf(error)
  return chatError?.type === "errorStore" && (chatError.storeError.type === "invalidQuote" || chatError.storeError.type === "chatItemNotFound" || chatError.storeError.type === "largeMsg")
}

function sessionDataOf(customData: object | undefined): SessionGroupData | undefined {
  return (customData as {[sessionKey]?: SessionGroupData} | undefined)?.[sessionKey]
}
