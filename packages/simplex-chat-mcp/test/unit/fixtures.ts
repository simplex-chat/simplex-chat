import {ChatResponse, T} from "@simplex-chat/types"
import {api, core} from "simplex-chat"
import {Actions, ActionHooks} from "../../src/actions"
import {ToolCall} from "../../src/messages"

export const user = {userId: 1, profile: {displayName: "agent", peerType: "agent", profileId: 1}} as unknown as T.User

export function contact(contactId: number, displayName: string, extra: object = {}): T.Contact {
  return {
    contactId,
    profile: {displayName, fullName: "", profileId: 100 + contactId, localAlias: "", ...extra},
    activeConn: {connId: 200 + contactId, connStatus: {type: "ready"}},
    updatedAt: "2026-10-17T10:00:00Z",
  } as unknown as T.Contact
}

export function member(groupMemberId: number, displayName: string, role: T.GroupMemberRole = T.GroupMemberRole.Member, status: T.GroupMemberStatus = T.GroupMemberStatus.Connected): T.GroupMember {
  return {
    groupMemberId,
    memberRole: role,
    memberStatus: status,
    memberProfile: {displayName, fullName: "", profileId: 300 + groupMemberId, localAlias: ""},
    memberContactProfileId: 300 + groupMemberId,
  } as unknown as T.GroupMember
}

export function group(groupId: number, displayName: string, membership: T.GroupMember = member(1, "agent", T.GroupMemberRole.Owner, T.GroupMemberStatus.Creator), extra: object = {}): T.GroupInfo {
  return {groupId, groupProfile: {displayName, fullName: ""}, membership, useRelays: false, updatedAt: "2026-10-17T10:00:00Z", ...extra} as unknown as T.GroupInfo
}

export function direct(c: T.Contact): T.ChatInfo {
  return {type: "direct", contact: c}
}

export function groupChat(g: T.GroupInfo): T.ChatInfo {
  return {type: "group", groupInfo: g}
}

export function item(itemId: number, text: string, chatDir: T.CIDirection = {type: "directRcv"}, extra: {status?: string; quoted?: number; mention?: boolean; ts?: string; live?: boolean} = {}): T.ChatItem {
  const received = chatDir.type.endsWith("Rcv")
  return {
    chatDir,
    meta: {
      itemId,
      itemTs: extra.ts ?? "2026-10-17T10:00:00Z",
      itemText: text,
      itemStatus: {type: extra.status ?? (received ? "rcvNew" : "sndSent")},
      userMention: extra.mention ?? false,
      ...(extra.live === undefined ? {} : {itemLive: extra.live}),
    },
    content: {type: received ? "rcvMsgContent" : "sndMsgContent", msgContent: {type: "text", text}},
    mentions: {},
    reactions: [],
    ...(extra.quoted === undefined ? {} : {quotedItem: {itemId: extra.quoted, sentAt: "", content: {type: "text", text: ""}}}),
  } as unknown as T.ChatItem
}

export function event(chatInfo: T.ChatInfo, chatItem: T.ChatItem): T.AChatItem {
  return {chatInfo, chatItem}
}

export function signal(): {controller: AbortController; call: ToolCall; progress: string[]} {
  const controller = new AbortController()
  const progress: string[] = []
  return {controller, progress, call: {signal: controller.signal, progress: message => progress.push(message)}}
}

export interface FakeHooks {
  calls: string[]
  hooks: ActionHooks
  performs: {resolve: () => void; reject: (error: Error) => void}[]
  approvers: number
}

export function fakeHooks(): FakeHooks {
  const fake: FakeHooks = {calls: [], performs: [], approvers: 1, hooks: undefined as unknown as ActionHooks}
  fake.hooks = {
    refusal: () => (fake.approvers > 0 ? undefined : "no approver"),
    saveCounter: async next => {
      fake.calls.push(`counter ${next}`)
    },
    postCard: async action => {
      fake.calls.push(`card ${action.n}`)
      return 1000 + action.n
    },
    perform: action =>
      new Promise((resolve, reject) => {
        fake.calls.push(`perform ${action.n}`)
        fake.performs.push({resolve, reject})
      }),
    outcome: async action => {
      fake.calls.push(`outcome ${action.n} ${action.state}${action.interrupted ? " interrupted" : ""}`)
    },
  }
  return fake
}

export function actionsWith(mode: "requests" | "host", fake: FakeHooks, next = 1, expireHours = 23): Actions {
  return new Actions(mode, expireHours, next, fake.hooks)
}

export async function settled(): Promise<void> {
  for (let i = 0; i < 10; i++) await Promise.resolve()
}

interface FakeChatData {
  chatInfo: T.ChatInfo
  items: T.ChatItem[]
}

export class FakeChat {
  commands: string[] = []
  sent: {chat: [T.ChatType, number]; messages: T.ComposedMessage[]}[] = []
  updates: {chatId: number; itemId: number; text: string}[] = []
  replies: string[] = []
  deleted: {chatType: T.ChatType; chatId: number; mode?: T.ChatDeleteMode}[] = []
  left: number[] = []
  customData: {chat: string; customData?: object}[] = []
  profiles: {groupId: number; groupProfile: T.GroupProfile}[] = []
  members = new Map<number, T.GroupMember[]>()
  chats = new Map<string, FakeChatData>()
  responses = new Map<string, object>()
  nextItemId = 500

  async sendChatCmd(cmd: string): Promise<ChatResponse> {
    this.commands.push(cmd)
    const fixed = this.responses.get(cmd)
    if (fixed !== undefined) return fixed as ChatResponse
    const getChat = /^\/_get chat ([@#]\d+) (.+)$/.exec(cmd)
    if (getChat) return this.getChat(getChat[1], getChat[2])
    if (cmd.startsWith("/_get chats ")) {
      const [, before, count] = /(?: before=(\S+))? count=(\d+)$/.exec(cmd)!
      const chats = [...this.chats.values()]
        .filter(({chatInfo}) => before === undefined || chatTs(chatInfo) < before)
        .slice(0, Number(count))
        .map(({chatInfo, items}) => ({chatInfo, chatItems: [], chatStats: chatStats(items)}))
      return structuredClone({type: "apiChats", user, chats}) as unknown as ChatResponse
    }
    if (cmd.startsWith("/_read chat items ")) {
      const [, ref, ids] = /^\/_read chat items ([@#]\d+) (.+)$/.exec(cmd)!
      const data = this.chats.get(ref)!
      for (const id of ids.split(",").map(Number)) {
        const found = data.items.find(({meta}) => meta.itemId === id)
        if (found) found.meta.itemStatus = {type: "rcvRead"}
      }
      return {type: "itemsReadForChat"} as unknown as ChatResponse
    }
    return {type: "cmdOk"}
  }

  async apiSendMessages(chat: [T.ChatType, number], messages: T.ComposedMessage[]): Promise<T.AChatItem[]> {
    this.sent.push({chat, messages})
    const ref = `${chat[0] === T.ChatType.Direct ? "@" : "#"}${chat[1]}`
    const chatInfo = this.chats.get(ref)?.chatInfo ?? groupChat(group(chat[1], "control"))
    return messages.map(({msgContent}) => event(chatInfo, item(this.nextItemId++, msgContent.text, chatInfo.type === "direct" ? {type: "directSnd"} : {type: "groupSnd"})))
  }

  async apiUpdateChatItem(_type: T.ChatType, chatId: number, itemId: number, msgContent: T.MsgContent): Promise<T.ChatItem> {
    this.updates.push({chatId, itemId, text: msgContent.text})
    return item(itemId, msgContent.text, {type: "groupSnd"})
  }

  async apiSendTextReply(_ci: T.AChatItem, text: string): Promise<T.AChatItem[]> {
    this.replies.push(text)
    return []
  }

  async apiListMembers(groupId: number): Promise<T.GroupMember[]> {
    return this.members.get(groupId) ?? []
  }

  async apiDeleteChat(chatType: T.ChatType, chatId: number, mode?: T.ChatDeleteMode): Promise<void> {
    this.deleted.push({chatType, chatId, mode})
    this.chats.delete(`${chatType === T.ChatType.Direct ? "@" : "#"}${chatId}`)
  }

  async apiLeaveGroup(groupId: number): Promise<T.GroupInfo> {
    this.left.push(groupId)
    const {groupInfo} = this.chats.get(`#${groupId}`)!.chatInfo as T.ChatInfo.Group
    groupInfo.membership = {...groupInfo.membership, memberStatus: T.GroupMemberStatus.Left}
    return groupInfo
  }

  async apiSetContactCustomData(contactId: number, customData?: object): Promise<void> {
    this.setCustomData(`@${contactId}`, customData)
  }

  async apiSetGroupCustomData(groupId: number, customData?: object): Promise<void> {
    this.setCustomData(`#${groupId}`, customData)
  }

  async apiDeleteGroupLink(): Promise<void> {}

  async apiUpdateGroupProfile(groupId: number, groupProfile: T.GroupProfile): Promise<T.GroupInfo> {
    this.profiles.push({groupId, groupProfile})
    return {...group(groupId, groupProfile.displayName), groupProfile}
  }

  api(): api.ChatApi {
    return this as unknown as api.ChatApi
  }

  addItem(ref: string, chatItem: T.ChatItem): T.AChatItem {
    const data = this.chats.get(ref)!
    data.items.push(chatItem)
    return event(data.chatInfo, structuredClone(chatItem))
  }

  private setCustomData(ref: string, customData: object | undefined): void {
    this.customData.push({chat: ref, customData})
    const {chatInfo} = this.chats.get(ref)!
    if (chatInfo.type === "direct") chatInfo.contact.customData = customData
    else (chatInfo as T.ChatInfo.Group).groupInfo.customData = customData
  }

  private getChat(ref: string, pagination: string): ChatResponse {
    const data = this.chats.get(ref)
    if (data === undefined) throw storeError(ref)
    const around = /^around=(\d+) count=0$/.exec(pagination)
    const after = /^after=(\d+) count=(\d+)$/.exec(pagination)
    const last = /^count=(\d+)$/.exec(pagination)
    const chatItems = around
      ? data.items.filter(({meta}) => meta.itemId === Number(around[1]))
      : after
        ? data.items.filter(({meta}) => meta.itemId > Number(after[1])).slice(0, Number(after[2]))
        : last && Number(last[1]) > 0
          ? data.items.slice(-Number(last[1]))
          : []
    if (around && chatItems.length === 0) throw storeError(ref)
    return structuredClone({type: "apiChat", chat: {chatInfo: data.chatInfo, chatItems, chatStats: chatStats(data.items)}}) as unknown as ChatResponse
  }
}

function chatTs(chatInfo: T.ChatInfo): string {
  return chatInfo.type === "direct" ? chatInfo.contact.updatedAt : (chatInfo as T.ChatInfo.Group).groupInfo.updatedAt
}

function chatStats(items: T.ChatItem[]): T.ChatStats {
  const unread = items.filter(({meta}) => meta.itemStatus.type === "rcvNew")
  return {unreadCount: unread.length, minUnreadItemId: unread[0]?.meta.itemId ?? 0, unreadMentions: 0, reportsCount: 0, unreadChat: false}
}

function storeError(ref: string): Error {
  return new core.ChatAPIError("Chat command error", {type: "errorStore", storeError: {type: "contactNotFound", contactId: Number(ref.slice(1))}})
}
