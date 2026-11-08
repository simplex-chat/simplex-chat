import {ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Command, createGroup, deleteGroup, isLost, joinedText, losesControl, parseCommand, safeName} from "./control"
import {chatErrorOf, log, logError, oneLine} from "./log"
import {isActive} from "./tools"

const mainCommands: T.ChatBotCommand[] = [
  {type: "command", keyword: "sessions", label: "Claude Code sessions"},
  {type: "command", keyword: "join", label: "Join the group of a session", params: "<session number>"},
]
const mainKey = "simplexMcpMain"
const greeting = "I'm Claude. Each Claude Code session has its own group, and you get its invitation here. /sessions lists the sessions; /join N joins the group of session N."
const commandsReply = "Commands: /sessions, /join N. Owners decide a requested reset of the owners with /'approve reset' or /'reject reset'."
const resetRequest = "A terminal asks to replace the owners: this group is deleted, and a new Main group with a new link replaces it."
const noOwners = "No owners in the Main group."

export interface SessionGroups {
  sessionsText(): string
  join(member: T.GroupMember, session: number): Promise<string>
  inviteOwner(member: T.GroupMember): Promise<void>
  removeOwners(): Promise<void>
}

export class MainGroup {
  lost = false
  private members: T.GroupMember[] = []
  private events: Promise<unknown> = Promise.resolve()
  private commands: Promise<unknown> = Promise.resolve()
  private linkText?: string
  private resetCard?: number

  constructor(
    private readonly chat: api.ChatApi,
    private group: T.GroupInfo,
    private readonly userId: number,
    private name: string,
    private readonly sessions: SessionGroups,
  ) {}

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

  isOwner(groupMemberId: number): boolean {
    return this.owners().some(member => member.groupMemberId === groupMemberId)
  }

  async load(): Promise<void> {
    if (isLost(this.group)) return this.lose()
    const {groupProfile} = this.group
    if (groupProfile.displayName !== this.name || JSON.stringify(groupProfile.groupPreferences?.commands) !== JSON.stringify(mainCommands)) {
      this.group = await this.chat.apiUpdateGroupProfile(this.groupId, {...groupProfile, displayName: this.name, groupPreferences: {...groupProfile.groupPreferences, commands: mainCommands}})
    }
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
    const command = parseCommand(chatItem.content.msgContent.text)
    if (command === undefined) return
    const sender = chatItem.chatDir.groupMember
    this.commands = this.commands
      .then(() => this.command(sender, command))
      .then(reply => (reply === undefined ? undefined : this.chat.apiSendTextReply(ci, reply)))
      .catch(error => logError("Main group command", error))
  }

  onEvent(event: ChatEvent): Promise<void> {
    return this.serialize(() => this.handleEvent(event))
  }

  async askReset(): Promise<void> {
    if (this.resetCard !== undefined) return
    this.resetCard = await this.post(`${resetRequest}\n/'approve reset' or /'reject reset'`)
    log("The reset of the owners waits for an owner's approval in the Main group.")
  }

  replace(): Promise<string> {
    return this.serialize(async () => {
      this.resetCard = undefined
      this.lost = true
      await deleteGroup(this.chat, this.group)
      this.group = await createMainGroup(this.chat, this.userId, this.name)
      this.lost = false
      await this.load()
      await this.sessions.removeOwners()
      await this.openLink()
      return this.linkText!
    })
  }

  private async command(sender: T.GroupMember, {keyword, params}: Command): Promise<string | undefined> {
    if (keyword === "sessions" && params === "") return this.sessions.sessionsText()
    if (keyword === "join" && /^\d+$/.test(params)) return this.sessions.join(sender, Number(params))
    if ((keyword === "approve" || keyword === "reject") && params === "reset") return this.decideReset(sender, keyword === "approve")
    return commandsReply
  }

  private async decideReset(sender: T.GroupMember, approved: boolean): Promise<string | undefined> {
    if (!this.isOwner(sender.groupMemberId)) return "Only owners decide a reset of the owners."
    const card = this.resetCard
    if (card === undefined) return "No reset of the owners is requested."
    this.resetCard = undefined
    const decision = `${approved ? "Approved" : "Rejected"} by ${safeName(sender.memberProfile.displayName)}.`
    await this.chat.apiUpdateChatItem(T.ChatType.Group, this.groupId, card, {type: "text", text: `${resetRequest}\n${decision}`}, false)
    log(`Reset of the owners: ${decision}`)
    if (approved) await this.replace()
    return undefined
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
    log("The Main group is lost. Run npx -y @simplex-chat/mcp reset-owners in a terminal.")
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

  private async post(text: string): Promise<number> {
    const [item] = await this.chat.apiSendMessages([T.ChatType.Group, this.groupId], [{msgContent: {type: "text", text}, mentions: {}}])
    return item.chatItem.meta.itemId
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

export async function createMainGroup(chat: api.ChatApi, userId: number, displayName: string): Promise<T.GroupInfo> {
  const group = await createGroup(chat, userId, {
    displayName,
    fullName: "",
    description: "Supervise your Claude Code sessions: each one has its own group, and you get its invitation here.",
    groupPreferences: {history: {enable: T.GroupFeatureEnabled.On}, directMessages: {enable: T.GroupFeatureEnabled.On, role: T.GroupMemberRole.Owner}, commands: mainCommands},
  }, {[mainKey]: true})
  log(`Main group "${displayName}" created.`)
  return group
}
