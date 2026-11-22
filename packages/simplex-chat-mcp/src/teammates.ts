import {CC, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {logError} from "./log"
import {keepLast, validName} from "./messages"
import {Step} from "./protocol"
import {isActive} from "./tools"

// spec: spec/architecture.md#teammates-constants
const maxTeammates = 20
// spec: spec/architecture.md#teammates-constants
const maxDescription = 160
// spec: spec/architecture.md#teammates-constants
const liveLineMs = 23 * 3_600_000

// spec: spec/architecture.md#teammates-teammates
interface Line {
  text: string
  item: Promise<number | undefined>
  at: number
}

// spec: spec/architecture.md#teammates-teammates
interface Teammate {
  added: Promise<number | undefined>
  lines: Map<string, Line>
  finished: Set<string>
}

// spec: spec/architecture.md#teammates-teammates
export class Teammates {
  private readonly current = new Map<string, Teammate>()
  private readonly names = new Map<number, string>()
  private readonly members = new Set<string>()

  constructor(
    private readonly chat: api.ChatApi,
    private readonly group: T.GroupInfo,
    private readonly session: number,
  ) {}

  // spec: spec/architecture.md#teammates-teammates-adopt
  async adopt(): Promise<void> {
    const {groupId, membership} = this.group
    for (const member of await this.chat.apiListMembers(groupId)) {
      if (member.memberVia !== membership.memberId || !isActive(member)) continue
      const {groupMemberId, memberProfile: {displayName: name}} = member
      this.current.set(name, {added: Promise.resolve(groupMemberId), lines: new Map(), finished: new Set()})
      this.names.set(groupMemberId, name)
      this.members.add(name)
    }
  }

  // spec: spec/architecture.md#teammates-teammates-start
  async start(name: string, description: string): Promise<void> {
    const key = validName(name)
    if (key === "" || this.current.has(key) || this.current.size >= maxTeammates) return
    const shortDescr = [...description].slice(0, maxDescription).join("")
    const added = this.add({displayName: key, fullName: "", peerType: T.ChatPeerType.Agent, ...(shortDescr === "" ? {} : {shortDescr})})
    this.current.set(key, {added, lines: new Map(), finished: new Set()})
    await added
  }

  // spec: spec/architecture.md#teammates-teammates-post
  async post(name: string, parts: string[]): Promise<void> {
    const member = await this.current.get(validName(name))?.added
    if (member !== undefined) await this.send(member, parts)
  }

  // spec: spec/architecture.md#teammates-teammates-step
  async step(name: string, {id, text, end}: Step): Promise<void> {
    const teammate = this.current.get(validName(name))
    if (teammate === undefined) return
    void this.endLines(teammate, Date.now() - liveLineMs)
    if (teammate.finished.delete(id)) return
    if (end !== true) {
      const item = teammate.added.then(member => (member === undefined ? undefined : this.send(member, [text], true).then(([itemId]) => itemId, error => (logError(this.subject(name), error), undefined))))
      teammate.lines.set(id, {text, item, at: Date.now()})
      await item
      return
    }
    const line = teammate.lines.get(id)
    teammate.lines.delete(id)
    if (line !== undefined) return this.finish(line, text)
    keepLast(teammate.finished, id)
    const member = await teammate.added
    if (member !== undefined) await this.send(member, [text]).catch(error => logError(this.subject(name), error))
  }

  // spec: spec/architecture.md#teammates-teammates-end
  async end(name: string): Promise<void> {
    const key = validName(name)
    const teammate = this.current.get(key)
    if (teammate === undefined) return
    this.current.delete(key)
    const member = await teammate.added
    if (member === undefined) return
    this.names.delete(member)
    await this.endLines(teammate)
    await this.chat.apiRemoveMembers(this.group.groupId, [member]).catch(error => logError(this.subject(key), error))
  }

  // spec: spec/architecture.md#teammates-teammates-end
  async endAll(): Promise<void> {
    await Promise.all([...this.current.keys()].map(name => this.end(name)))
  }

  // spec: spec/architecture.md#teammates-teammates-end
  async endLive(): Promise<void> {
    await Promise.all([...this.current.values()].map(teammate => this.endLines(teammate)))
  }

  // spec: spec/architecture.md#teammates-teammates-names
  hadMember(name: string): boolean {
    return this.members.has(validName(name))
  }

  // spec: spec/architecture.md#teammates-teammates-names
  nameOf(member: number | undefined): string | undefined {
    return member === undefined ? undefined : this.names.get(member)
  }

  // spec: spec/architecture.md#teammates-teammates-names
  memberOf(name: string): {name: string; member: number} | undefined {
    const key = validName(name)
    const member = [...this.names].find(([, named]) => named === key)?.[0]
    return member === undefined ? undefined : {name: key, member}
  }

  // spec: spec/architecture.md#teammates-teammates-start
  private async add(profile: T.Profile): Promise<number | undefined> {
    try {
      const r = await this.chat.sendChatCmd(CC.APIAddViaMember.cmdString({groupId: this.group.groupId, memberRole: T.GroupMemberRole.Member, profile}))
      if (r.type !== "viaMemberAdded") throw new api.ChatCommandError("error adding member", r)
      this.names.set(r.member.groupMemberId, profile.displayName)
      this.members.add(profile.displayName)
      return r.member.groupMemberId
    } catch (error) {
      logError(this.subject(profile.displayName), error)
      return undefined
    }
  }

  // spec: spec/architecture.md#teammates-teammates-post
  private async send(member: number, parts: string[], live = false): Promise<number[]> {
    const messages: T.ComposedMessage[] = parts.map(text => ({msgContent: {type: "text", text}, mentions: {}, priority: -1}))
    const r = await this.chat.sendChatCmd(`/_send #${this.group.groupId}(as_member=${member})${live ? " live=on" : ""} json ${JSON.stringify(messages)}`)
    if (r.type !== "newChatItems") throw new api.ChatCommandError("error sending as member", r)
    return r.chatItems.map(({chatItem}) => chatItem.meta.itemId)
  }

  // spec: spec/architecture.md#teammates-teammates-step
  private async endLines(teammate: Teammate, before = Infinity): Promise<void> {
    const ended = [...teammate.lines].filter(([, {at}]) => at <= before)
    for (const [id] of ended) {
      teammate.lines.delete(id)
      keepLast(teammate.finished, id)
    }
    await Promise.all(ended.map(([, line]) => this.finish(line, line.text)))
  }

  // spec: spec/architecture.md#teammates-teammates-step
  private async finish({item}: Line, text: string): Promise<void> {
    const itemId = await item
    if (itemId !== undefined) await this.chat.apiUpdateChatItem(T.ChatType.Group, this.group.groupId, itemId, {type: "text", text}, false, -1).catch(error => logError(`teammate lines of session ${this.session}`, error))
  }

  private subject(name: string): string {
    return `teammate ${name} of session ${this.session}`
  }
}
