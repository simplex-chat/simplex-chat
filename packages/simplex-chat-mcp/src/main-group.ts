import {ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Command, createGroup, deleteGroup, isLost, joinedText, losesControl, parseCommand, safeName, updateGroupProfile} from "./control"
import {chatErrorOf, log, logError, oneLine} from "./log"
import {resetCommand} from "./owner"
import {isActive} from "./tools"

const mainCommands: T.ChatBotCommand[] = [
  {type: "command", keyword: "sessions", label: "Claude Code sessions"},
  {type: "command", keyword: "join", label: "Join the group of a session", params: "<session number>"},
]
const mainKey = "simplexMcpMain"
const listKey = "simplexMcpList"
const listAge = 23 * 3_600_000
const greeting = "I'm Claude. Each Claude Code session has its own group, and you get its invitation here. /sessions lists the sessions; /join N joins the group of session N."
const unknownCommand = "Unknown command."
const noOwners = "No owners in the Main group."
const welcome = "Everyone in this group can join the groups of your Claude Code sessions, prompt Claude and approve what it does. Invite only people you trust with that."
const joinRights = "They can join the group of any Claude Code session here, prompt Claude and approve its actions."
const shortDescr = "Everyone in this group can join the groups of your Claude Code sessions, prompt Claude and approve what it does."

export interface SessionGroups {
  sessionsText(quote?: T.MsgContent): string
  join(member: T.GroupMember, session: number): Promise<string>
  inviteOwner(member: T.GroupMember): Promise<void>
}

interface SessionList {
  item: number
  at: number
}

export class MainGroup {
  lost = false
  private members: T.GroupMember[] = []
  private events: Promise<unknown> = Promise.resolve()
  private commands: Promise<unknown> = Promise.resolve()
  private linkText?: string
  private list?: SessionList
  private shown?: string

  constructor(
    private readonly chat: api.ChatApi,
    private group: T.GroupInfo,
    private readonly userId: number,
    private name: string,
    private readonly sessions: SessionGroups,
  ) {
    this.list = listOf(group.customData)
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

  owners(): T.GroupMember[] {
    return this.lost ? [] : this.humans().filter(({memberRole}) => memberRole === T.GroupMemberRole.Owner)
  }

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

  onEvent(event: ChatEvent): Promise<void> {
    return this.serialize(() => this.handleEvent(event))
  }

  replace(): Promise<string> {
    return this.serialize(async () => {
      this.lost = true
      await deleteGroup(this.chat, this.group)
      this.group = await createMainGroup(this.chat, this.userId, this.name)
      this.list = undefined
      this.lost = false
      await this.load()
      await this.openLink()
      return this.linkText!
    })
  }

  showList(text: string): Promise<void> {
    return this.serialize(async () => {
      if (this.lost) return
      const {list} = this
      if (list === undefined) {
        if (this.owners().length > 0) await this.postList(text)
        return
      }
      if (list.at <= Date.now() - listAge) return this.replaceList(list, text)
      if (text === this.shown) return
      try {
        await this.chat.apiUpdateChatItem(T.ChatType.Group, this.groupId, list.item, {type: "text", text})
        this.shown = text
      } catch (error) {
        if (!isRefusedEdit(error)) throw error
        await this.replaceList(list, text)
      }
    })
  }

  private async replaceList(old: SessionList, text: string): Promise<void> {
    await this.postList(text)
    await this.chat.apiDeleteChatItems(T.ChatType.Group, this.groupId, [old.item], T.CIDeleteMode.Broadcast).catch(error => logError("old list of sessions", error))
  }

  private async postList(text: string): Promise<void> {
    this.list = {item: await this.post(text, -1), at: Date.now()}
    this.shown = text
    const customData = {...this.group.customData, [mainKey]: true, [listKey]: this.list}
    await this.chat.apiSetGroupCustomData(this.groupId, customData)
    this.group = {...this.group, customData}
  }

  private async command(sender: T.GroupMember, {keyword, params}: Command, quote: T.MsgContent): Promise<string> {
    if (keyword === "sessions" && params === "") return this.sessions.sessionsText(quote)
    if (keyword === "join" && /^\d+$/.test(params)) return this.sessions.join(sender, Number(params))
    return unknownCommand
  }

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

  private async ownerJoined(member: T.GroupMember): Promise<void> {
    await this.deleteLink()
    await this.chat.apiSetMembersRole(this.groupId, [member.groupMemberId], T.GroupMemberRole.Owner)
    await this.refreshMembers()
    await this.post(greeting)
    log(`${joinedText(member, oneLine)} and is now an owner. The link is deleted.`)
  }

  private lose(): void {
    this.lost = true
    log(`The Main group is lost. Run ${resetCommand} in a terminal.`)
  }

  private async openLink(): Promise<void> {
    await this.deleteLink()
    this.linkText = await this.chat.apiCreateGroupLink(this.groupId, T.GroupMemberRole.Member)
    log(`Main group link created. Whoever joins by it first becomes an owner of "${this.name}".`)
  }

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

export async function findMainGroup(chat: api.ChatApi, userId: number): Promise<T.GroupInfo | undefined> {
  const groups = (await chat.apiListGroups(userId)).filter(({customData}) => (customData as {[mainKey]?: boolean} | undefined)?.[mainKey] === true)
  return groups.reduce<T.GroupInfo | undefined>((latest, group) => (latest === undefined || group.groupId > latest.groupId ? group : latest), undefined)
}

function isMissingLink(error: unknown): boolean {
  const chatError = chatErrorOf(error)
  return chatError?.type === "errorStore" && chatError.storeError.type === "groupLinkNotFound"
}

function isRefusedEdit(error: unknown): boolean {
  const chatError = chatErrorOf(error)
  return (chatError?.type === "error" && chatError.errorType.type === "invalidChatItemUpdate") || (chatError?.type === "errorStore" && chatError.storeError.type === "chatItemNotFound")
}

function listOf(customData: object | undefined): SessionList | undefined {
  const list = (customData as {[listKey]?: Partial<SessionList>} | undefined)?.[listKey]
  return typeof list?.item === "number" && typeof list.at === "number" ? {item: list.item, at: list.at} : undefined
}

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
