import {ChatEvent, T} from "@simplex-chat/types"
import * as qrcode from "qrcode"
import {api} from "simplex-chat"
import {Action, ActionHooks, Actions, pendingLine, subjectText} from "./actions"
import {ExitError, chatErrorOf, log, logError, shellWord} from "./log"
import {utcMinutes, validName, viewName} from "./messages"
import type {OwnerEntry} from "./serve"

const controlCommands: T.ChatBotCommand[] = [
  {type: "command", keyword: "approve", label: "Approve", params: "N"},
  {type: "command", keyword: "reject", label: "Reject", params: "N"},
  {type: "command", keyword: "pending", label: "Pending actions"},
]

const commandsReply = "Commands: /approve N, /reject N, /pending"
const greeting = "I'm your agent. I ask here before I connect or send."
const noOwners = "No owners in the agent control chat."

export interface ControlData {
  control: true
  nextAction: number
}

export interface ControlOptions {
  displayName: string
  owners?: OwnerEntry[]
  registerHint: string
}

export interface Command {
  keyword: string
  params: string
}

export class Control implements Pick<ActionHooks, "refusal" | "saveCounter" | "postCard" | "outcome"> {
  private members: T.GroupMember[] = []
  private writes: Promise<unknown> = Promise.resolve()

  constructor(
    private readonly chat: api.ChatApi,
    private group: T.GroupInfo,
    private readonly options: ControlOptions,
    private readonly actions: Actions,
  ) {}

  get groupId(): number {
    return this.group.groupId
  }

  get chatRef(): string {
    return `#${this.group.groupId}`
  }

  async load(): Promise<void> {
    if (isLost(this.group)) return this.lose()
    if (this.group.groupProfile.displayName !== this.options.displayName) {
      this.group = await this.chat.apiUpdateGroupProfile(this.groupId, {...this.group.groupProfile, displayName: this.options.displayName})
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

  refusal(): string | undefined {
    if (this.approvers().length > 0) return undefined
    return this.options.owners === undefined
      ? "The agent has no owner. The person who runs it has to join its control chat with the link the server printed."
      : "The agent has no approving owner. The person who runs it has to restart it with the --owners options it printed."
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
    const reply = this.ownerCommand(chatItem.chatDir.groupMember, command)
    if (reply !== undefined) this.chat.apiSendTextReply(ci, reply).catch(error => logError("control chat reply", error))
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

  ownerCommand(sender: T.GroupMember, {keyword, params}: Command): string | undefined {
    this.actions.expireDue()
    const decision = (keyword === "approve" || keyword === "reject") && /^\d+$/.test(params) ? Number(params) : undefined
    const pending = keyword === "pending" && params === ""
    if (decision === undefined && !pending) return commandsReply
    if (!this.isApprover(sender)) return this.options.owners === undefined ? "Only owners approve actions." : "Only the owners listed in --owners approve actions."
    if (decision === undefined) {
      const lines = this.actions.pending().filter(({card}) => card !== undefined).map(pendingLine)
      return lines.length > 0 ? lines.join("\n") : "No pending actions."
    }
    const action = this.actions.get(decision)
    if (action === undefined || action.state !== "pending" || action.card === undefined) return `Action ${decision} is not pending.`
    const approver = sender.memberProfile.displayName
    if (keyword === "approve") this.actions.approve(action, approver)
    else this.actions.reject(action, approver)
    return undefined
  }

  private async startOwners(): Promise<void> {
    if (this.humanOwners().length > 0) return log(this.options.registerHint)
    if (this.humans().length > 0) log(noOwners)
    await this.openLink(T.GroupMemberRole.Member)
  }

  private async startObserver(): Promise<void> {
    if (this.humans().length === 0) return this.openLink(T.GroupMemberRole.Observer)
    await this.announce(`${this.humans().map(joinedText).join(". ")}. Restart the server with --owners ${ownersText(this.humans())}`)
  }

  private async startListed(owners: OwnerEntry[]): Promise<void> {
    const humans = this.humans()
    const listed = owners.map(owner => {
      const member = humans.find(({groupMemberId, memberProfile}) => groupMemberId === owner.memberId && memberProfile.displayName === owner.name)
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
    log(`${joinedText(member)} and is now an owner. The link is deleted.`)
    log(this.options.registerHint)
  }

  private async observerJoined(member: T.GroupMember): Promise<void> {
    await this.deleteLink()
    await this.announce(`${joinedText(member)}. Restart the server with --owners ${ownersText(this.humans())}`)
  }

  private async reportOwnerChanges(owners: OwnerEntry[], before: T.GroupMember[]): Promise<void> {
    const after = this.humanOwners()
    const isListed = (member: T.GroupMember): boolean => owners.some(owner => listsMember(owner, member))
    for (const member of after) {
      if (!isListed(member) && !before.some(sameMember(member))) {
        await this.announce(`${memberText(member)} is now an owner and can't approve actions yet. Restart the server with --owners ${ownersText(after)}`)
      }
    }
    for (const member of before) {
      if (isListed(member) && !after.some(sameMember(member))) {
        const remaining = after.length > 0 ? `Restart the server with --owners ${ownersText(after)}` : "No owners remain: restart the server with --reset-owners --owners"
        await this.announce(`${memberText(member)} is no longer an owner. ${remaining}`)
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
    log((await qrcode.toString(link, {type: "terminal", small: true})).trimEnd())
    log(link)
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

  private async announce(text: string): Promise<void> {
    log(text)
    await this.post(text)
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
    description: "Your agent asks here before it connects or sends. /approve N approves, /reject N rejects, /pending lists pending actions.",
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
  const approval = mode === "host" ? "Performed without approval (mode host)" : `Approved by ${action.decidedBy}`
  switch (action.state) {
    case "pending":
      return action.interrupted
        ? [...request, "Cancelled: the server stopped."].join("\n")
        : [...request, `/'approve ${action.n}' or /'reject ${action.n}'. Expires ${utcMinutes(action.expires)}.`].join("\n")
    case "performing":
      return [...request, `${approval}; interrupted: the server stopped.`].join("\n")
    case "done":
      return [doneLine(action), ...request.slice(1), `${approval}.`].join("\n")
    case "failed":
      return [...request, `${approval}; failed: ${action.error}.`].join("\n")
    case "rejected":
      return [...request, `Rejected by ${action.decidedBy}.`].join("\n")
    case "expired":
      return [...request, "Expired."].join("\n")
  }
}

function requestLines(action: Action): string[] {
  const subject = subjectText({...action.subject, name: action.subject.name === undefined ? undefined : safeName(action.subject.name)})
  switch (action.tool) {
    case "connect": {
      const {plan, input} = action
      const message = input.message === undefined ? [] : ["With the message:", ...codeLines(input.message)]
      return [`I want to connect to ${subject}, ${action.subject.incognito ? "incognito" : "as myself"}.`, ...codeLines(plan.description), ...codeLines(plan.welcome), ...message]
    }
    case "send": {
      const {replyTo, mentions} = action
      const reply = replyTo === undefined ? [] : [`replying to ${safeName(replyTo.from)}: ${codeSpan(replyTo.text.replace(/\s+/g, " ").slice(0, 100))}`]
      const mentioning = mentions.length === 0 ? [] : [`mentioning ${mentions.map(({name}) => mentionText(name)).join(", ")}`]
      return [`I want to send a message to ${subject}${action.subject.incognito ? ", where I am incognito" : ""}:`, ...codeLines(action.input.text), ...reply, ...mentioning]
    }
  }
}

function doneLine(action: Action): string {
  const [first] = requestLines(action)
  if (action.tool === "send") return first.replace("I want to send", "I sent")
  return first.replace("I want to connect", action.connected ? "I connected" : "I asked to connect")
}

function codeLines(text: string | undefined): string[] {
  return text === undefined ? [] : text.split("\n").map(line => (line.trim() === "" ? "" : codeSpan(line)))
}

function codeSpan(text: string): string {
  return `\`${text.replace(/`/g, "'")}\``
}

function safeName(name: string): string {
  return validName(name) === name ? name : codeSpan(name)
}

function mentionText(name: string): string {
  return validName(name) === name ? `@${viewName(name)}` : codeSpan(`@${name}`)
}

function controlData(nextAction: number): {simplexMcp: ControlData} {
  return {simplexMcp: {control: true, nextAction}}
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

function isActive({memberStatus}: T.GroupMember): boolean {
  return memberStatus === T.GroupMemberStatus.Connected || memberStatus === T.GroupMemberStatus.Complete
}

function memberCurrent({memberStatus}: T.GroupMember): boolean {
  const current: T.GroupMemberStatus[] = [
    T.GroupMemberStatus.Introduced,
    T.GroupMemberStatus.Intro_inv,
    T.GroupMemberStatus.Accepted,
    T.GroupMemberStatus.Announced,
    T.GroupMemberStatus.Connected,
    T.GroupMemberStatus.Complete,
    T.GroupMemberStatus.Creator,
  ]
  return current.includes(memberStatus)
}

function listsMember({memberId, name}: OwnerEntry, member: T.GroupMember): boolean {
  return member.groupMemberId === memberId && member.memberProfile.displayName === name
}

function sameMember(member: T.GroupMember): (other: T.GroupMember) => boolean {
  return other => other.groupMemberId === member.groupMemberId
}

function memberText({groupMemberId, memberProfile}: T.GroupMember): string {
  return `${memberProfile.displayName} (member ${groupMemberId})`
}

function joinedText(member: T.GroupMember): string {
  return `${member.memberProfile.displayName} joined (member ${member.groupMemberId})`
}

export function entryText({memberId, name}: OwnerEntry): string {
  return shellWord(`${memberId}:${name}`, /^\d+:[\p{L}\p{N}_-]+$/u)
}

function ownersText(members: T.GroupMember[]): string {
  return members.map(({groupMemberId, memberProfile}) => entryText({memberId: groupMemberId, name: memberProfile.displayName})).join(",")
}
