import {mkdirSync, rmSync} from "fs"
import {dirname, join} from "path"
import {ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Action, Actions, HostTexts, Mode, isDecided} from "./actions"
import {Config} from "./config"
import {Control, controlCommands, controlDataOf, createGroup, isControlEvent, isGroupMessage, parseCommand} from "./control"
import {openCore} from "./core"
import {SessionFile, hubFolder, readSessionFile, sessionFolder, writeSessionFile} from "./files"
import {chatErrorOf, errorText, log, logError} from "./log"
import {MainGroup, SessionGroups, createMainGroup, findMainGroup} from "./main-group"
import {ChatItemsWatch, ToolCall, getChat, storedName, validName} from "./messages"
import {AttachLine, OwnerState, PermissionRequest, ResetReply, TurnRequest} from "./protocol"
import {waitForCancelledCards} from "./serve"
import {AgentTools, ToolName, fitsMessage, ownerOf} from "./tools"
import {User, userContact} from "./user"

const profileName = "Claude"
const sessionKey = "simplexMcpSession"
const firstAction = 1
const cardsWait = 10
const promptEcho = 60_000
const maxPart = 12_000

export const hubTexts: HostTexts = {lost: "The group of this session is lost. Start a new Claude Code session.", stopping: "The SimpleX daemon is stopping."}

export interface StoredSession {
  id: string
  session: SessionFile
}

interface SessionGroupData {
  session: number
  invited: InvitedMember[]
}

interface InvitedMember {
  groupMemberId: number
  mainMemberId: number
}

export class Session {
  open = false
  onLine?: (line: AttachLine) => void
  readonly actions: Actions
  readonly tools: AgentTools
  readonly control: Control
  private readonly prompts: {text: string; at: number}[] = []
  private readonly receiving = new Map<number, {resolve: () => void; reject: (error: Error) => void}>()
  private description: {model?: string; effort?: string} = {}
  private turnItem?: T.ChatItem

  constructor(
    private readonly hub: Hub,
    readonly id: string,
    readonly number: number,
    private name: string,
    group: T.GroupInfo,
    private data: SessionGroupData,
    mode: Mode,
  ) {
    const {chat, profile: user, root} = hub
    this.actions = new Actions(mode, hub.expireHours, controlDataOf(group.customData)?.nextAction ?? firstAction, {
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
    }
    this.tools = new AgentTools(agent, () => this.actions.mode, number)
    const session = {approves: (member: T.GroupMember) => this.approves(member), ownerOf: (n: number) => hub.sessionId(n)}
    this.control = new Control(chat, group, {displayName: group.groupProfile.displayName, session, registerHint: "", qrCode: "small"}, this.actions, this.tools)
  }

  get displayName(): string {
    return groupName(this.number, this.name)
  }

  busy(): boolean {
    return this.actions.all().length > 0
  }

  call(tool: ToolName, input: unknown, call: ToolCall): ReturnType<AgentTools["call"]> {
    return this.tools.call(tool, input, call)
  }

  async rename(name: string): Promise<void> {
    this.name = name
    await this.control.rename(this.displayName)
  }

  requestMode(mode: Mode): void {
    if (mode === this.actions.mode) return
    if (mode === "requests") return this.setMode(mode)
    if (this.actions.pending().some(({tool}) => tool === "mode")) return
    try {
      this.actions.create("mode", {tool: "mode", input: {mode}}, undefined, this.number)
    } catch (error) {
      logError(`mode ${mode} of session ${this.number}`, error)
    }
  }

  onMessage(ci: T.AChatItem): void {
    this.control.onMessage(ci)
    const {chatItem} = ci
    const {chatDir, content} = chatItem
    if (chatDir.type !== "groupRcv" || content.type !== "rcvMsgContent" || parseCommand(content.msgContent.text) !== undefined || !this.approves(chatDir.groupMember)) return
    this.prompt(chatItem, content.msgContent.text, chatDir.groupMember).catch(error => logError(`prompt of session ${this.number}`, error))
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

  async turn({prompt, answer, model, effort}: TurnRequest): Promise<void> {
    if (model !== undefined || effort !== undefined) {
      this.description = {model: model ?? this.description.model, effort: effort ?? this.description.effort}
      const {model: shown, effort: level} = this.description
      await this.control.describe([shown, level === undefined ? undefined : `effort ${level}`].filter(part => part !== undefined).join(" · "))
    }
    const recent = Date.now() - promptEcho
    if (prompt !== undefined && !this.prompts.some(({text, at}) => text === prompt && at > recent)) {
      this.turnItem = undefined
      await this.hub.user?.post(this.number, textParts(prompt))
    }
    if (answer === undefined) return
    this.endPermissions()
    if (answer.trim() !== "") await this.answer(answer)
  }

  endPermissions(): void {
    for (const action of this.actions.pending()) {
      if (action.tool === "permission") this.actions.end(action)
    }
  }

  fileDone(fileId: number, error?: string): void {
    const waiting = this.receiving.get(fileId)
    this.receiving.delete(fileId)
    if (error === undefined) waiting?.resolve()
    else waiting?.reject(new Error(error))
  }

  async invite(member: T.GroupMember, role: T.GroupMemberRole): Promise<void> {
    const {groupMemberId} = await this.hub.chat.apiAddMemberViaGroup(this.control.groupId, this.hub.main.groupId, member.groupMemberId, role)
    const invited = [...this.data.invited.filter(entry => entry.groupMemberId !== groupMemberId), {groupMemberId, mainMemberId: member.groupMemberId}]
    await this.saveData({...this.data, invited})
  }

  async removeOwners(): Promise<void> {
    const members = this.data.invited.map(({groupMemberId}) => groupMemberId)
    if (members.length > 0) await this.hub.chat.apiRemoveMembers(this.control.groupId, members).catch(error => logError(`members of session ${this.number}`, error))
    await this.saveData({...this.data, invited: []})
  }

  private async prompt(chatItem: T.ChatItem, text: string, member: T.GroupMember): Promise<void> {
    const file = chatItem.file === undefined ? undefined : await this.receive(chatItem.file)
    const now = Date.now()
    this.prompts.splice(0, this.prompts.length, ...this.prompts.filter(({at}) => at > now - promptEcho), {text, at: now})
    this.turnItem = chatItem
    this.onLine?.({prompt: {text, from: member.memberProfile.displayName, item: chatItem.meta.itemId, ...(file === undefined ? {} : {file})}})
  }

  private async receive({fileId, fileName}: T.CIFile): Promise<string> {
    const path = join(sessionFolder(this.hub.root, this.id), "files", "group", storedName(fileId, fileName))
    mkdirSync(dirname(path), {recursive: true, mode: 0o700})
    const received = new Promise<void>((resolve, reject) => this.receiving.set(fileId, {resolve, reject}))
    await this.hub.chat.apiReceiveFile(fileId, path, true).catch(error => this.fileDone(fileId, errorText(error)))
    await received
    return path
  }

  private async answer(text: string): Promise<void> {
    const quoted = this.turnItem
    this.turnItem = undefined
    const parts = textParts(text)
    const quotedItemId = quoted?.content.type === "rcvMsgContent" && fitsMessage({type: "text", text: parts[0]}, undefined, {content: quoted.content.msgContent, fileName: quoted.file?.fileName}) ? quoted.meta.itemId : undefined
    await this.hub.chat.apiSendMessages([T.ChatType.Group, this.control.groupId], parts.map((part, i) => ({msgContent: {type: "text", text: part}, quotedItemId: i === 0 ? quotedItemId : undefined, mentions: {}})))
  }

  private approves(member: T.GroupMember): boolean {
    const invited = this.data.invited.find(({groupMemberId}) => groupMemberId === member.groupMemberId)
    return invited !== undefined && this.hub.main.isOwner(invited.mainMemberId)
  }

  private setMode(mode: Mode): void {
    this.actions.mode = mode
    const folder = sessionFolder(this.hub.root, this.id)
    writeSessionFile(folder, {...readSessionFile(folder)!, mode})
  }

  private saveData(data: SessionGroupData): Promise<void> {
    this.data = data
    return this.control.saveData({[sessionKey]: data})
  }
}

export class Hub implements SessionGroups {
  readonly main: MainGroup
  user?: User
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
      hub.user = await User.open(root, chat, user)
      for (const {id, session} of stored) await hub.session(id, session)
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
    if (known === undefined) writeSessionFile(folder, {...stored, group: group.groupId})
    const session = new Session(this, id, stored.number, stored.name, group, sessionDataOf(group.customData) ?? {session: stored.number, invited: []}, stored.mode ?? "requests")
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
      await session.invite(member, member.memberRole)
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

  async removeOwners(): Promise<void> {
    for (const session of this.sessions.values()) await session.removeOwners()
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
      description: `The Claude Code session ${number}: owners of the Main group approve its actions here.`,
      groupPreferences: {history: {enable: T.GroupFeatureEnabled.On}, directMessages: {enable: T.GroupFeatureEnabled.Off}, commands: controlCommands},
    }, {[sessionKey]: {session: number, invited: []}})
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
    for (const {tools} of sessions) tools.onEvent(event)
    if (event.type === "sndFileError") {
      const {chatItem_} = event
      const sending = chatItem_ === undefined ? undefined : sessions.find(({id, control}) => ownerOf(chatItem_.chatInfo) === id || isGroupMessage(chatItem_, control.groupId))
      sending?.control.fileNotSent(event)
    }
    if (isControlEvent(event, this.main.groupId)) return this.main.onEvent(event).catch(error => logError(`${event.type} in the Main group`, error))
    const session = sessions.find(({control}) => isControlEvent(event, control.groupId))
    if (session !== undefined) {
      this.controlEvents = this.controlEvents.then(() => session.control.onEvent(event)).catch(error => logError(`${event.type} in the group of session ${session.number}`, error))
    }
  }
}

function groupName(number: number, name: string): string {
  return validName(`${number} · ${name}`)
}

function textParts(text: string): string[] {
  const parts: string[] = []
  let part = ""
  let bytes = 0
  for (const c of text) {
    const size = Buffer.byteLength(JSON.stringify(c)) - 2
    if (bytes + size > maxPart) {
      parts.push(part)
      part = ""
      bytes = 0
    }
    part += c
    bytes += size
  }
  return [...parts, part]
}

function sessionDataOf(customData: object | undefined): SessionGroupData | undefined {
  return (customData as {[sessionKey]?: SessionGroupData} | undefined)?.[sessionKey]
}
