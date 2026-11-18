import {rmSync} from "fs"
import {CC, ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {Core, openCore} from "./core"
import {logError} from "./log"
import {validName} from "./messages"

// spec: spec/architecture.md#teammates-constants
export const maxTeammates = 20
// spec: spec/architecture.md#teammates-constants
export const teammateKey = "simplexMcpTeammate"
// spec: spec/architecture.md#teammates-constants
const hostName = "Teammates"
// spec: spec/architecture.md#teammates-constants
const joinTimeout = 5 * 60_000
// spec: spec/architecture.md#teammates-constants
const leaveWait = 30_000
// spec: spec/architecture.md#teammates-constants
const maxDescription = 160
// spec: spec/architecture.md#teammates-constants
const goneStatuses = [T.GroupMemberStatus.Left, T.GroupMemberStatus.Removed, T.GroupMemberStatus.Deleted, T.GroupMemberStatus.Rejected]

export type Opener = (folder: string) => Promise<Core>

export type StartResult = "started" | "known" | "full"

// spec: spec/architecture.md#teammates-teammatehost
export interface TeammateHost {
  chat: api.ChatApi
  userId: number
  groupId: number
  session: number
}

interface TeammateData {
  session: number
  name: string
}

interface Pending {
  parts: string[]
  posted: (posted: boolean) => void
}

interface Teammate {
  name: string
  userId?: number
  connId?: number
  contactId?: number
  memberId?: number
  groupId?: number
  joined: boolean
  pending: Pending[]
  ending?: NodeJS.Timeout
  ended?: true
  joining: Promise<boolean>
}

// spec: spec/architecture.md#teammates-teammates
export class Teammates {
  private core?: Promise<Core>
  private readonly teammates = new Map<string, Teammate>()
  private readonly memberIds = new Set<number>()
  private readonly invitations = new Map<number, (groupId: number) => void>()
  private readonly joins = new Map<number, () => void>()
  private queue: Promise<unknown> = Promise.resolve()
  private activeUserId?: number
  private closing = new AbortController()
  private closed?: Promise<void>

  constructor(
    private readonly folder: string,
    private readonly host: TeammateHost,
    private readonly opener: Opener = folder => openCore(folder, hostName, T.ChatPeerType.Agent),
  ) {}

  // spec: spec/architecture.md#teammates-teammates-ismember
  isMember({groupMemberId}: T.GroupMember): boolean {
    return this.memberIds.has(groupMemberId)
  }

  // spec: spec/architecture.md#teammates-teammates-start
  start(name: string, description?: string): StartResult {
    if (this.active(name) !== undefined) return "known"
    if ([...this.teammates.values()].filter(({ended}) => ended !== true).length >= maxTeammates) return "full"
    const teammate: Teammate = {name, joined: false, pending: [], joining: Promise.resolve(false)}
    this.teammates.set(name, teammate)
    teammate.joining = this.join(teammate, description).then(
      () => true,
      error => this.failed(teammate, error).then(() => false),
    )
    return "started"
  }

  // spec: spec/architecture.md#teammates-teammates-post
  post(name: string, parts: string[]): Promise<boolean> {
    const teammate = this.active(name)
    if (teammate === undefined) return Promise.resolve(false)
    if (teammate.joined) return this.send(teammate, parts)
    return new Promise(posted => teammate.pending.push({parts, posted}))
  }

  // spec: spec/architecture.md#teammates-teammates-end
  end(name: string, delay = 0): Promise<void> {
    const teammate = this.active(name)
    if (teammate === undefined) return Promise.resolve()
    if (delay === 0) return this.finish(teammate)
    teammate.ending ??= setTimeout(() => void this.finish(teammate), delay)
    return Promise.resolve()
  }

  // spec: spec/architecture.md#teammates-teammates-end
  close(): Promise<void> {
    this.closed ??= this.shutDown().finally(() => (this.closed = undefined))
    return this.closed
  }

  // spec: spec/architecture.md#teammates-teammates-end
  private async shutDown(): Promise<void> {
    this.closing.abort()
    await Promise.all([...this.teammates.values()].filter(({ended}) => ended !== true).map(teammate => this.finish(teammate)))
    const core = await this.core?.catch(() => undefined)
    this.core = undefined
    this.activeUserId = undefined
    this.closing = new AbortController()
    await core?.chat.close().catch(error => logError(`teammates of session ${this.host.session}`, error))
    try {
      rmSync(this.folder, {recursive: true, force: true})
    } catch (error) {
      logError(`teammates of session ${this.host.session}`, error)
    }
  }

  // spec: spec/architecture.md#teammates-teammates-join
  private async join(teammate: Teammate, description: string | undefined): Promise<void> {
    const {chat: hostChat, userId: hostUserId, groupId: hostGroupId, session} = this.host
    await this.closed
    const {chat} = await this.opened()
    const shortDescr = description === undefined ? undefined : [...description.trim()].slice(0, maxDescription).join("")
    const profile: T.Profile = {displayName: validName(teammate.name) || hostName, fullName: "", peerType: T.ChatPeerType.Agent, ...(shortDescr ? {shortDescr} : {})}
    const userId = await this.serialize(async () => {
      const {userId} = await chat.apiCreateActiveUser(profile)
      this.activeUserId = userId
      return userId
    })
    teammate.userId = userId
    const r = await hostChat.sendChatCmd(CC.APIAddContact.cmdString({userId: hostUserId, incognito: false}))
    if (r.type !== "invitation") throw new api.ChatCommandError("error creating link", r)
    const {connLinkInvitation, connection} = r
    teammate.connId = connection.pccConnId
    const connected = hostChat.wait("contactConnected", ({contact}) => contact.activeConn?.connId === connection.pccConnId, joinTimeout)
    await this.asUser(userId, async teammateChat => {
      const [, link] = await teammateChat.apiConnectPlan(userId, connLinkInvitation.connShortLink || connLinkInvitation.connFullLink)
      await teammateChat.apiConnect(userId, false, link)
    })
    const event = await Promise.race([connected, this.aborted()])
    if (event === undefined) throw new Error(`teammate ${teammate.name} did not connect to Claude`)
    const {contactId} = event.contact
    teammate.contactId = contactId
    const data: TeammateData = {session, name: teammate.name}
    await hostChat.apiSetContactCustomData(contactId, {[teammateKey]: data})
    const invited = this.awaited(this.invitations, userId)
    const member = await hostChat.apiAddMember(hostGroupId, contactId, T.GroupMemberRole.Member)
    teammate.memberId = member.groupMemberId
    this.memberIds.add(member.groupMemberId)
    const groupId = await invited
    teammate.groupId = groupId
    const joined = this.awaited(this.joins, userId)
    await this.asUser(userId, teammateChat => teammateChat.apiJoinGroup(groupId))
    await joined
    for (let next = teammate.pending.shift(); next !== undefined; next = teammate.pending.shift()) next.posted(await this.send(teammate, next.parts))
    teammate.joined = true
  }

  // spec: spec/architecture.md#teammates-teammates-join
  private async failed(teammate: Teammate, error: unknown): Promise<void> {
    logError(`teammate ${teammate.name} of session ${this.host.session}`, error)
    teammate.ended = true
    clearTimeout(teammate.ending)
    for (const {posted} of teammate.pending.splice(0)) posted(false)
    await this.leave(teammate)
  }

  // spec: spec/architecture.md#teammates-teammates-end
  private async finish(teammate: Teammate): Promise<void> {
    if (teammate.ended === true) return
    teammate.ended = true
    clearTimeout(teammate.ending)
    if (await teammate.joining) await this.leave(teammate)
  }

  // spec: spec/architecture.md#teammates-teammates-end
  private async leave({name, userId, connId, contactId, memberId, groupId}: Teammate): Promise<void> {
    const {chat: hostChat, groupId: hostGroupId, session} = this.host
    const context = `teammate ${name} of session ${session}`
    try {
      if (memberId !== undefined) {
        const left = hostChat.wait("leftMember", ({member}) => member.groupMemberId === memberId, leaveWait)
        const sent = groupId === undefined || userId === undefined ? false : await this.asUser(userId, chat => chat.apiLeaveGroup(groupId)).then(() => true, error => (logError(context, error), false))
        if (!sent || (await left) === undefined) await hostChat.apiRemoveMembers(hostGroupId, [memberId]).catch(error => logError(context, error))
      }
      if (userId !== undefined) {
        const {user} = await this.opened()
        await this.asUser(user.userId, chat => chat.apiDeleteUser(userId, true))
      }
    } catch (error) {
      logError(context, error)
    }
    if (contactId !== undefined) await hostChat.apiDeleteChat(T.ChatType.Direct, contactId, {type: "full", notify: false}).catch(error => logError(context, error))
    else if (connId !== undefined) await deleteConnection(hostChat, connId).catch(error => logError(context, error))
  }

  // spec: spec/architecture.md#teammates-teammates-opened
  private opened(): Promise<Core> {
    this.core ??= (async () => {
      rmSync(this.folder, {recursive: true, force: true})
      const core = await this.opener(this.folder)
      this.activeUserId = core.user.userId
      await core.events(event => this.onEvent(event))
      return core
    })().catch(error => {
      this.core = undefined
      throw error
    })
    return this.core
  }

  // spec: spec/architecture.md#teammates-teammates-opened
  private async onEvent(event: ChatEvent): Promise<void> {
    if (event.type === "receivedGroupInvitation") this.invitations.get(event.user.userId)?.(event.groupInfo.groupId)
    if (event.type === "userJoinedGroup") this.joins.get(event.user.userId)?.()
  }

  // spec: spec/architecture.md#teammates-teammates-join
  private awaited<V>(waits: Map<number, (value: V) => void>, userId: number): Promise<V> {
    const result = Promise.race([
      new Promise<V>((resolve, reject) => {
        const timer = setTimeout(() => {
          waits.delete(userId)
          reject(new Error("timed out"))
        }, joinTimeout).unref()
        waits.set(userId, value => {
          clearTimeout(timer)
          waits.delete(userId)
          resolve(value)
        })
      }),
      this.aborted(),
    ])
    result.catch(() => undefined)
    return result
  }

  // spec: spec/architecture.md#teammates-teammates-join
  private aborted(): Promise<never> {
    const {signal} = this.closing
    const result = new Promise<never>((_, reject) => {
      if (signal.aborted) reject(new Error("closed"))
      else signal.addEventListener("abort", () => reject(new Error("closed")), {once: true})
    })
    result.catch(() => undefined)
    return result
  }

  // spec: spec/architecture.md#teammates-teammates-post
  private send({name, userId, groupId}: Teammate, parts: string[]): Promise<boolean> {
    if (userId === undefined || groupId === undefined) return Promise.resolve(false)
    const messages = parts.map(text => ({msgContent: {type: "text", text}, mentions: {}, priority: -1}) as T.ComposedMessage)
    return this.asUser(userId, chat => chat.apiSendMessages([T.ChatType.Group, groupId], messages)).then(
      () => true,
      error => (logError(`teammate ${name} of session ${this.host.session}`, error), false),
    )
  }

  private active(name: string): Teammate | undefined {
    const teammate = this.teammates.get(name)
    return teammate?.ended === true ? undefined : teammate
  }

  // spec: spec/architecture.md#teammates-teammates-asuser
  private async asUser<R>(userId: number, action: (chat: api.ChatApi) => Promise<R>): Promise<R> {
    const {chat} = await this.opened()
    return this.serialize(async () => {
      if (this.activeUserId !== userId) {
        await chat.apiSetActiveUser(userId)
        this.activeUserId = userId
      }
      return action(chat)
    })
  }

  // spec: spec/architecture.md#teammates-teammates-asuser
  private serialize<R>(action: () => Promise<R>): Promise<R> {
    const result = this.queue.then(action)
    this.queue = result.catch(() => undefined)
    return result
  }
}

// spec: spec/architecture.md#teammates-teammates-end
async function deleteConnection(chat: api.ChatApi, connId: number): Promise<void> {
  const r = await chat.sendChatCmd(`/_delete :${connId}`)
  if (r.type !== "contactConnectionDeleted") throw new api.ChatCommandError("error deleting connection", r)
}

// spec: spec/architecture.md#teammates-removestale
export async function removeStale(chat: api.ChatApi, userId: number, groupOf: (session: number) => number | undefined): Promise<void> {
  const stale = (await chat.apiListContacts(userId)).flatMap(({contactId, customData}) => {
    const data = (customData as {[teammateKey]?: TeammateData} | undefined)?.[teammateKey]
    return data === undefined ? [] : [{contactId, groupId: groupOf(data.session)}]
  })
  for (const groupId of new Set(stale.flatMap(({groupId}) => (groupId === undefined ? [] : [groupId])))) {
    const contactIds = stale.filter(entry => entry.groupId === groupId).map(({contactId}) => contactId)
    const members = (await chat.apiListMembers(groupId)).filter(({memberContactId, memberStatus}) => memberContactId !== undefined && contactIds.includes(memberContactId) && !goneStatuses.includes(memberStatus))
    if (members.length > 0) await chat.apiRemoveMembers(groupId, members.map(({groupMemberId}) => groupMemberId)).catch(error => logError(`teammates of group ${groupId}`, error))
  }
  for (const {contactId} of stale) await chat.apiDeleteChat(T.ChatType.Direct, contactId, {type: "full", notify: false}).catch(error => logError(`teammate contact ${contactId}`, error))
}
