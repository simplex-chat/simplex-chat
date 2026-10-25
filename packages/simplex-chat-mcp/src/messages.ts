import {ChatResponse, T} from "@simplex-chat/types"
import {api} from "simplex-chat"

const progressInterval = 60_000
const maxTimer = 2_147_483_647
const pageSize = 100
const maxCollected = 100
const prohibitedNameChars = ".,;/\\#@'\"`~"
const maxNameLength = 50

export interface Message {
  chat: string
  item: number
  from: string
  from_member?: number
  from_type?: string
  time: string
  text: string
  new: boolean
  reply_to?: {chat: string; item: number}
  mentions_me: boolean
}

export interface Sender {
  name: string
  type?: string
}

export interface ToolCall {
  signal: AbortSignal
  progress: (message: string) => void
}

export class Collected {
  readonly items: T.AChatItem[] = []
  readonly changes = new Changes()
  readonly overflow = new Set<number>()
  lastAt?: number
}

export class Changes {
  private readonly wakes = new Set<() => void>()

  subscribe(wake: () => void): () => void {
    this.wakes.add(wake)
    return () => this.wakes.delete(wake)
  }

  notify(): void {
    for (const wake of [...this.wakes]) wake()
  }
}

type ItemChange = "new" | "updated" | "deleted"

type ItemListener = (item: T.AChatItem, change: ItemChange) => void

export class ChatItemsWatch {
  private readonly listeners = new Map<string, Set<ItemListener>>()

  listen(chat: string, listener: ItemListener): () => void {
    const listeners = this.listeners.get(chat) ?? new Set()
    this.listeners.set(chat, listeners)
    listeners.add(listener)
    return () => {
      listeners.delete(listener)
      if (listeners.size === 0 && this.listeners.get(chat) === listeners) this.listeners.delete(chat)
    }
  }

  collect(chat: string, collected: Collected, accepts: (item: T.ChatItem) => boolean = () => true): () => void {
    return this.listen(chat, (item, change) => {
      if (item.chatItem.content.type !== "rcvMsgContent" || !accepts(item.chatItem)) return
      const {itemId} = item.chatItem.meta
      const i = collected.items.findIndex(({chatItem}) => chatItem.meta.itemId === itemId)
      const overflowDeleted = change === "deleted" && collected.overflow.has(itemId)
      if (change !== "new" && i < 0 && !overflowDeleted) return
      if (overflowDeleted) collected.overflow.delete(itemId)
      else if (change === "deleted") collected.items.splice(i, 1)
      else if (i >= 0) collected.items[i] = item
      else if (collected.items.length < maxCollected) collected.items.push(item)
      else collected.overflow.add(itemId)
      collected.lastAt = Date.now()
      collected.changes.notify()
    })
  }

  deliver(items: T.AChatItem[]): void {
    for (const item of items) this.notify(item, "new")
  }

  update(item: T.AChatItem): void {
    this.notify(item, "updated")
  }

  remove(items: T.AChatItem[]): void {
    for (const item of items) this.notify(item, "deleted")
  }

  private notify(item: T.AChatItem, change: ItemChange): void {
    const chat = chatRef(item.chatInfo)
    if (chat === undefined) return
    for (const listener of [...this.listeners.get(chat) ?? []]) listener(item, change)
  }
}

export function chatRef(chatInfo: T.ChatInfo): string | undefined {
  switch (chatInfo.type) {
    case "direct":
      return `@${chatInfo.contact.contactId}`
    case "group":
      return chatInfo.groupChatScope === undefined ? `#${chatInfo.groupInfo.groupId}` : undefined
    default:
      return undefined
  }
}

export function isChatRef(value: string): boolean {
  return /^[@#][1-9]\d{0,14}$/.test(value)
}

export function chatNumber(chat: string): number {
  return Number(chat.slice(1))
}

export function isMessageItem({content}: T.ChatItem): boolean {
  return content.type === "rcvMsgContent" || content.type === "sndMsgContent"
}

export function isNew({meta}: T.ChatItem): boolean {
  return meta.itemStatus.type === "rcvNew"
}

export function isLive({meta}: T.ChatItem): boolean {
  return meta.itemLive === true
}

export function toMessage(chatInfo: T.ChatInfo, item: T.ChatItem, self: Sender): Message {
  const chat = chatRef(chatInfo) ?? ""
  const {meta, content, quotedItem} = item
  const text = content.type === "rcvMsgContent" || content.type === "sndMsgContent" ? content.msgContent.text : ""
  const sender = itemSender(chatInfo, item.chatDir, self)
  return {
    chat,
    item: meta.itemId,
    from: sender.name,
    ...(sender.member === undefined ? {} : {from_member: sender.member}),
    ...(sender.type === undefined ? {} : {from_type: sender.type}),
    time: meta.itemTs,
    text,
    new: isNew(item),
    ...(quotedItem?.itemId === undefined ? {} : {reply_to: {chat, item: quotedItem.itemId}}),
    mentions_me: meta.userMention,
  }
}

function itemSender(chatInfo: T.ChatInfo, chatDir: T.CIDirection, self: Sender): Sender & {member?: number} {
  switch (chatDir.type) {
    case "groupRcv": {
      const {groupMemberId, memberProfile} = chatDir.groupMember
      return {name: memberProfile.displayName, type: memberProfile.peerType, member: groupMemberId}
    }
    case "directRcv":
      return chatInfo.type === "direct" ? {name: chatInfo.contact.profile.displayName, type: chatInfo.contact.profile.peerType} : self
    case "channelRcv":
      return chatInfo.type === "group" ? {name: chatInfo.groupInfo.groupProfile.displayName} : self
    default:
      return self
  }
}

export async function chatCommand<R>(chat: api.ChatApi, cmd: string, type: string): Promise<R> {
  const r: ChatResponse = await chat.sendChatCmd(cmd)
  if ((r.type as string) !== type) throw new api.ChatCommandError(`unexpected response to ${cmd.split(" ")[0]}`, r)
  return r as unknown as R
}

export async function getChat(chat: api.ChatApi, ref: string, pagination: string): Promise<T.AChat> {
  return (await chatCommand<{chat: T.AChat}>(chat, `/_get chat ${ref} ${pagination}`, "apiChat")).chat
}

export async function markRead(chat: api.ChatApi, ref: string, itemIds: number[]): Promise<void> {
  if (itemIds.length > 0) await chatCommand(chat, `/_read chat items ${ref} ${itemIds.join(",")}`, "itemsReadForChat")
}

export async function unreadItems(chat: api.ChatApi, ref: string, {chatInfo, chatStats}: T.AChat, messages: number): Promise<{chatInfo: T.ChatInfo; items: T.ChatItem[]; unreadRest: number}> {
  if (chatStats.unreadCount === 0) return {chatInfo, items: [], unreadRest: 0}
  const items = (await getChat(chat, ref, `around=${chatStats.minUnreadItemId} count=0`)).chatItems
  for (let last = items.at(-1); last !== undefined && items.filter(isNewMessage).length < messages;) {
    const page = (await getChat(chat, ref, `after=${last.meta.itemId} count=${pageSize}`)).chatItems
    items.push(...page)
    last = page.length === pageSize ? page.at(-1) : undefined
  }
  const unread = items.filter(isNew)
  return {chatInfo, items: unread, unreadRest: Math.max(0, chatStats.unreadCount - unread.length)}
}

function isNewMessage(item: T.ChatItem): boolean {
  return item.content.type === "rcvMsgContent" && isNew(item) && !isLive(item)
}

export function reportProgress(call: ToolCall, message: () => string): () => void {
  call.progress(message())
  const timer = setInterval(() => call.progress(message()), progressInterval)
  return () => clearInterval(timer)
}

export function nextChange(changes: Changes, signal: AbortSignal, ms: number): Promise<boolean> {
  return new Promise(resolve => {
    if (signal.aborted) return resolve(false)
    const finish = (changed: boolean): void => {
      clearTimeout(timer)
      unsubscribe()
      signal.removeEventListener("abort", aborted)
      resolve(changed)
    }
    const aborted = (): void => finish(false)
    const timer = setTimeout(() => finish(true), Math.max(0, Math.min(ms, maxTimer)))
    const unsubscribe = changes.subscribe(() => finish(true))
    signal.addEventListener("abort", aborted, {once: true})
  })
}

export async function awaitResponses(collected: Collected, deadline: number, idle: number, call: ToolCall): Promise<boolean> {
  for (;;) {
    const live = collected.items.some(({chatItem}) => isLive(chatItem))
    const idleEnd = collected.lastAt === undefined || live ? Infinity : collected.lastAt + idle * 1000
    const remaining = Math.min(deadline, idleEnd) - Date.now()
    if (remaining <= 0) return true
    if (!(await nextChange(collected.changes, call.signal, remaining))) return false
  }
}

export function utcMinutes(time: number): string {
  return `${new Date(time).toISOString().slice(0, 16).replace("T", " ")} UTC`
}

export function validName(name: string): string {
  const chars: string[] = []
  let previous = ""
  let punctuation = 0
  for (const original of name) {
    const c = /^[\t\n\v\f\r\p{Zs}]$/u.test(original) ? " " : original
    const isPunctuation = /^[\p{Pc}\p{Pd}\p{Po}]$/u.test(c)
    const firstNameChar = /^[\p{L}\p{Nd}\p{So}]$/u.test(c)
    const firstChar = firstNameChar || /^[\p{Sc}\p{Sm}]$/u.test(c)
    const valid = prohibitedNameChars.includes(c) ? false
      : previous === "" ? c > " " && firstNameChar
      : previous === " " ? firstChar || (punctuation === 0 && isPunctuation)
      : punctuation > 0 ? firstChar || c === " "
      : firstChar || c === " " || /^\p{M}$/u.test(c) || isPunctuation
    if (valid) {
      chars.push(c)
      previous = c
      punctuation = isPunctuation ? punctuation + 1 : c === " " ? punctuation : 0
    }
  }
  const trimmed = chars.slice(0, maxNameLength).join("").replace(/ +$/, "")
  return trimmed.replace(/^([\s\S]+)_(\d+)$/, "$1-$2")
}

export function viewName(name: string): string {
  return /\s/u.test(name) || /\p{P}$/u.test(name) ? `'${name}'` : name
}

export function fullDomainName({nameTLD, domain, subDomain}: T.SimplexDomain): string {
  const tld = nameTLD === T.SimplexTLD.Simplex ? ".simplex" : nameTLD === T.SimplexTLD.Testing ? ".testing" : ""
  return `${[...subDomain].reverse().concat(domain).join(".")}${tld}`
}
