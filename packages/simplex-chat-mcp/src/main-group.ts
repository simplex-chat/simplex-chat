import {ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Command, createGroup, deleteGroup, isLost, joinedText, losesControl, parseCommand, safeName, updateGroupProfile} from "./control"
import {chatErrorOf, log, logError, oneLine} from "./log"
import {resetCommand} from "./owner"
import {isActive, mainKey} from "./tools"

// spec: spec/architecture.md#main-group-maincommands
const mainCommands: T.ChatBotCommand[] = [
  {type: "command", keyword: "sessions", label: "Claude Code sessions"},
  {type: "command", keyword: "join", label: "Join the group of a session", params: "<session number>"},
]
// spec: spec/api.md#main-group-profile
const listKey = "simplexMcpList"
// spec: spec/api.md#main-group-profile
const boardKey = "simplexMcpBoard"
// spec: spec/api.md#session-board
const boardAge = 23 * 3_600_000
// spec: spec/architecture.md#main-group-maincommands
const greeting = "I'm Claude. Each Claude Code session has its own group, and you get its invitation here. /sessions lists the sessions; /join N joins the group of session N."
// spec: spec/api.md#main-group
const unknownCommand = "Unknown command."
// spec: spec/architecture.md#main-group-maincommands
const noOwners = "No owners in the Main group."
// spec: spec/architecture.md#main-group-maincommands
const welcome = "Everyone in this group can join the groups of your Claude Code sessions, prompt Claude and approve what it does. Invite only people you trust with that."
// spec: spec/architecture.md#main-group-maincommands
const joinRights = "They can join the group of any Claude Code session here, prompt Claude and approve its actions."
// spec: spec/architecture.md#main-group-maincommands
const shortDescr = "Everyone in this group can join the groups of your Claude Code sessions, prompt Claude and approve what it does."

// spec: spec/architecture.md#main-group-sessiongroups
export interface SessionGroups {
  sessionsText(quote?: T.MsgContent): string
  join(member: T.GroupMember, session: number): Promise<string>
  inviteOwner(member: T.GroupMember): Promise<void>
}

// spec: spec/api.md#session-board
export interface BoardSession {
  session: number
  running: boolean
  emoji: string
  text: string
}

// spec: spec/api.md#main-group-profile
interface BoardItem {
  item: number
  at: number
}

// spec: spec/api.md#main-group-profile
interface BoardEntry extends BoardItem {
  session: number
}

// spec: spec/api.md#main-group-profile
interface Board {
  summary?: BoardItem
  sessions: BoardEntry[]
}

export class MainGroup {
  lost = false
  private members: T.GroupMember[] = []
  private events: Promise<unknown> = Promise.resolve()
  private commands: Promise<unknown> = Promise.resolve()
  private linkText?: string
  private board: Board
  private oldList?: number
  private readonly shown = new Map<number, string>()
  private shownSummary?: string

  constructor(
    private readonly chat: api.ChatApi,
    private group: T.GroupInfo,
    private readonly userId: number,
    private name: string,
    private readonly sessions: SessionGroups,
  ) {
    this.board = boardOf(group.customData)
    this.oldList = listOf(group.customData)
  }

  get groupId(): number {
    return this.group.groupId
  }

  get displayName(): string {
    return this.name
  }

  get link(): string | undefined {
    return this.linkText
  }

  // spec: spec/architecture.md#main-group-maingroup-owners
  owners(): T.GroupMember[] {
    return this.lost ? [] : this.humans().filter(({memberRole}) => memberRole === T.GroupMemberRole.Owner)
  }

  // spec: spec/api.md#main-group-profile
  async load(): Promise<void> {
    if (isLost(this.group)) return this.lose()
    const displayName = this.name
    this.group = await updateGroupProfile(this.chat, this.group, profile =>
      profile.displayName !== displayName || JSON.stringify(profile.groupPreferences?.commands) !== JSON.stringify(mainCommands)
        ? {...profile, displayName, groupPreferences: {...profile.groupPreferences, commands: mainCommands}}
        : undefined,
    )
    await this.refreshMembers()
  }

  // spec: spec/architecture.md#main-group-maingroup-load
  async start(): Promise<void> {
    if (this.lost || this.owners().length > 0) return
    if (this.humans().length > 0) log(noOwners)
    this.linkText = await this.chat.apiGetGroupLinkStr(this.groupId).catch(error => {
      if (!isMissingLink(error)) throw error
      return undefined
    })
    if (this.linkText === undefined) await this.openLink()
  }

  rename(name: string): Promise<void> {
    this.name = name
    return this.serialize(() => this.load())
  }

  // spec: spec/architecture.md#main-group-maingroup-onmessage
  onMessage(ci: T.AChatItem): void {
    const {chatItem} = ci
    if (this.lost || chatItem.chatDir.type !== "groupRcv" || chatItem.content.type !== "rcvMsgContent") return
    const {msgContent} = chatItem.content
    const command = parseCommand(msgContent.text)
    if (command === undefined) return
    const sender = chatItem.chatDir.groupMember
    this.commands = this.commands
      .then(() => this.command(sender, command, msgContent))
      .then(reply => this.chat.apiSendTextReply(ci, reply))
      .catch(error => logError("Main group command", error))
  }

  // spec: spec/architecture.md#main-group-maingroup-onevent
  onEvent(event: ChatEvent): Promise<void> {
    return this.serialize(() => this.handleEvent(event))
  }

  // spec: spec/architecture.md#main-group-maingroup-replace
  replace(): Promise<string> {
    return this.serialize(async () => {
      this.lost = true
      await deleteGroup(this.chat, this.group)
      this.group = await createMainGroup(this.chat, this.userId, this.name)
      this.board = {sessions: []}
      this.oldList = undefined
      this.shown.clear()
      this.shownSummary = undefined
      this.lost = false
      await this.load()
      await this.openLink()
      return this.linkText!
    })
  }

  // spec: spec/architecture.md#main-group-maingroup-showboard
  showBoard(sessions: BoardSession[]): Promise<void> {
    return this.serialize(async () => {
      if (this.lost) return
      if (this.oldList !== undefined) await this.deleteItem(this.oldList, "old list of sessions")
      const owned = this.owners().length > 0
      const posted: number[] = []
      const entries: BoardEntry[] = []
      for (const session of sessions) {
        const entry = this.board.sessions.find(known => known.session === session.session)
        const kept = entry === undefined ? (session.running && owned ? await this.postEntry(session, posted) : undefined) : await this.renderEntry(entry, session, posted)
        if (kept !== undefined) entries.push(kept)
      }
      for (const entry of this.board.sessions) if (!sessions.some(({session}) => session === entry.session)) await this.deleteEntry(entry)
      const shown = sessions.filter(({session}) => entries.some(entry => entry.session === session))
      const summary = await this.renderSummary(summaryText(shown), owned && entries.length > 0, posted)
      await this.saveBoard({...(summary === undefined ? {} : {summary}), sessions: entries}, posted)
    })
  }

  // spec: spec/architecture.md#main-group-maingroup-showboard
  private async renderEntry(entry: BoardEntry, session: BoardSession, posted: number[]): Promise<BoardEntry | undefined> {
    if (!session.running && entry.at <= Date.now() - boardAge) return this.deleteEntry(entry)
    if (session.text === this.shown.get(entry.session)) return entry
    if (await this.edited(entry.item, session.text)) {
      this.shown.set(entry.session, session.text)
      return entry
    }
    await this.deleteEntry(entry)
    return session.running ? this.postEntry(session, posted) : undefined
  }

  // spec: spec/architecture.md#main-group-maingroup-showboard
  private async postEntry(session: BoardSession, posted: number[]): Promise<BoardEntry> {
    const item = await this.postItem(session.text, posted)
    this.shown.set(session.session, session.text)
    return {session: session.session, ...item}
  }

  // spec: spec/architecture.md#main-group-maingroup-showboard
  private async deleteEntry(entry: BoardEntry): Promise<undefined> {
    this.shown.delete(entry.session)
    await this.deleteItem(entry.item, "old message of the board")
    return undefined
  }

  // spec: spec/architecture.md#main-group-maingroup-showboard
  private async renderSummary(text: string, postable: boolean, posted: number[]): Promise<BoardItem | undefined> {
    const {summary} = this.board
    if (summary !== undefined && (text === this.shownSummary || (await this.edited(summary.item, text)))) {
      this.shownSummary = text
      return summary
    }
    if (summary !== undefined) await this.deleteItem(summary.item, "old summary of the board")
    else if (!postable) return undefined
    this.shownSummary = text
    return this.postItem(text, posted)
  }

  // spec: spec/architecture.md#main-group-maingroup-showboard
  private async edited(item: number, text: string): Promise<boolean> {
    try {
      await this.chat.apiUpdateChatItem(T.ChatType.Group, this.groupId, item, {type: "text", text})
      return true
    } catch (error) {
      if (!isRefusedEdit(error)) throw error
      return false
    }
  }

  // spec: spec/architecture.md#main-group-maingroup-showboard
  private async postItem(text: string, posted: number[]): Promise<BoardItem> {
    const item = await this.post(text, -1)
    posted.push(item)
    return {item, at: Date.now()}
  }

  // spec: spec/architecture.md#main-group-maingroup-showboard
  private async saveBoard(board: Board, posted: number[]): Promise<void> {
    if (this.oldList === undefined && JSON.stringify(board) === JSON.stringify(this.board)) return
    const {[listKey]: _, ...data} = (this.group.customData ?? {}) as {[key: string]: unknown}
    const customData = {...data, [mainKey]: true, [boardKey]: board}
    try {
      await this.chat.apiSetGroupCustomData(this.groupId, customData)
    } catch (error) {
      for (const item of posted) await this.deleteItem(item, "new message of the board")
      throw error
    }
    this.board = board
    this.oldList = undefined
    this.group = {...this.group, customData}
  }

  private async deleteItem(item: number, subject: string): Promise<void> {
    await this.chat.apiDeleteChatItems(T.ChatType.Group, this.groupId, [item], T.CIDeleteMode.Broadcast).catch(error => logError(subject, error))
  }

  // spec: spec/api.md#main-group
  private async command(sender: T.GroupMember, {keyword, params}: Command, quote: T.MsgContent): Promise<string> {
    if (keyword === "sessions" && params === "") return this.sessions.sessionsText(quote)
    if (keyword === "join" && /^\d+$/.test(params)) return this.sessions.join(sender, Number(params))
    return unknownCommand
  }

  // spec: spec/architecture.md#main-group-maingroup-onevent
  private async handleEvent(event: ChatEvent): Promise<void> {
    if (this.lost) return
    if (losesControl(event)) return this.lose()
    const before = this.owners()
    await this.refreshMembers()
    if (event.type === "joinedGroupMember" && this.isHuman(event.member) && this.owners().length === 0) await this.ownerJoined(event.member)
    else if (before.length > 0 && this.owners().length === 0) {
      log(noOwners)
      return this.openLink()
    }
    const joined = event.type === "joinedGroupMember" || event.type === "connectedToGroupMember" ? this.humans().find(({groupMemberId}) => groupMemberId === event.member.groupMemberId) : undefined
    if (joined !== undefined) await this.post(`${safeName(joined.memberProfile.displayName)} joined. ${joinRights}`)
    for (const owner of this.owners()) {
      if (!before.some(({groupMemberId}) => groupMemberId === owner.groupMemberId)) await this.sessions.inviteOwner(owner)
    }
  }

  // spec: spec/architecture.md#main-group-maingroup-onevent
  private async ownerJoined(member: T.GroupMember): Promise<void> {
    await this.deleteLink()
    await this.chat.apiSetMembersRole(this.groupId, [member.groupMemberId], T.GroupMemberRole.Owner)
    await this.refreshMembers()
    await this.post(greeting)
    log(`${joinedText(member, oneLine)} and is now an owner. The link is deleted.`)
  }

  // spec: spec/api.md#plugin-commands
  private lose(): void {
    this.lost = true
    log(`The Main group is lost. Run ${resetCommand} in a terminal.`)
  }

  // spec: spec/architecture.md#main-group-maingroup-openlink
  private async openLink(): Promise<void> {
    await this.deleteLink()
    this.linkText = await this.chat.apiCreateGroupLink(this.groupId, T.GroupMemberRole.Member)
    log(`Main group link created. Whoever joins by it first becomes an owner of "${this.name}".`)
  }

  // spec: spec/architecture.md#main-group-maingroup-openlink
  private async deleteLink(): Promise<void> {
    this.linkText = undefined
    try {
      await this.chat.apiDeleteGroupLink(this.groupId)
    } catch (error) {
      if (!isMissingLink(error)) throw error
    }
  }

  private async post(text: string, priority?: number): Promise<number> {
    const message: T.ComposedMessage & {priority?: number} = {msgContent: {type: "text", text}, mentions: {}, priority}
    const [sent] = await this.chat.apiSendMessages([T.ChatType.Group, this.groupId], [message])
    return sent.chatItem.meta.itemId
  }

  private serialize<R>(work: () => Promise<R>): Promise<R> {
    const result = this.events.then(work)
    this.events = result.catch(() => undefined)
    return result
  }

  private async refreshMembers(): Promise<void> {
    this.members = await this.chat.apiListMembers(this.groupId)
  }

  private humans(): T.GroupMember[] {
    return this.members.filter(member => this.isHuman(member))
  }

  private isHuman(member: T.GroupMember): boolean {
    return member.groupMemberId !== this.group.membership.groupMemberId && isActive(member)
  }
}

// spec: spec/architecture.md#main-group-findmaingroup
export async function findMainGroup(chat: api.ChatApi, userId: number): Promise<T.GroupInfo | undefined> {
  const groups = (await chat.apiListGroups(userId)).filter(({customData}) => (customData as {[mainKey]?: boolean} | undefined)?.[mainKey] === true)
  return groups.reduce<T.GroupInfo | undefined>((latest, group) => (latest === undefined || group.groupId > latest.groupId ? group : latest), undefined)
}

function isMissingLink(error: unknown): boolean {
  const chatError = chatErrorOf(error)
  return chatError?.type === "errorStore" && chatError.storeError.type === "groupLinkNotFound"
}

// spec: spec/architecture.md#main-group-maingroup-showboard
function isRefusedEdit(error: unknown): boolean {
  const chatError = chatErrorOf(error)
  return (chatError?.type === "error" && chatError.errorType.type === "invalidChatItemUpdate") || (chatError?.type === "errorStore" && chatError.storeError.type === "chatItemNotFound")
}

// spec: spec/api.md#session-board
function summaryText(sessions: BoardSession[]): string {
  if (sessions.length === 0) return "No sessions."
  return `${sessions.length} session${sessions.length === 1 ? "" : "s"} ${sessions.map(({emoji}) => emoji).join(" ")}`
}

// spec: spec/api.md#main-group-profile
function listOf(customData: object | undefined): number | undefined {
  const item = (customData as {[listKey]?: {item?: unknown}} | undefined)?.[listKey]?.item
  return typeof item === "number" ? item : undefined
}

// spec: spec/api.md#main-group-profile
function boardOf(customData: object | undefined): Board {
  const board = (customData as {[boardKey]?: {summary?: unknown; sessions?: unknown}} | undefined)?.[boardKey]
  const summary = itemOf(board?.summary)
  const entries = board?.sessions
  const sessions = (Array.isArray(entries) ? entries : []).flatMap((entry: {session?: unknown} | null) => {
    const item = itemOf(entry)
    return item !== undefined && typeof entry?.session === "number" ? [{session: entry.session, ...item}] : []
  })
  return {...(summary === undefined ? {} : {summary}), sessions}
}

function itemOf(value: unknown): BoardItem | undefined {
  const {item, at} = (value ?? {}) as {item?: unknown; at?: unknown}
  return typeof item === "number" && typeof at === "number" ? {item, at} : undefined
}

// spec: spec/api.md#main-group-profile
export async function createMainGroup(chat: api.ChatApi, userId: number, displayName: string): Promise<T.GroupInfo> {
  const group = await createGroup(chat, userId, {
    displayName,
    fullName: "",
    shortDescr,
    description: welcome,
    groupPreferences: {history: {enable: T.GroupFeatureEnabled.On}, directMessages: {enable: T.GroupFeatureEnabled.On, role: T.GroupMemberRole.Owner}, commands: mainCommands},
  }, {[mainKey]: true})
  log(`Main group "${displayName}" created.`)
  return group
}
