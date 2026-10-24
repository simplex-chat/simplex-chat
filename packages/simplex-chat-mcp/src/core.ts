import {join} from "path"
import {ChatEvent, T} from "@simplex-chat/types"
import {api, util} from "simplex-chat"
import {createDataFolder} from "./files"
import {logError} from "./log"
import {chatCommand} from "./messages"

const deliveryTimeout = 10_000
const deliveryPoll = 100
const newProfileChats = 100
// Display name of the contact the core adds to every new user (createPresetContactCards in Commands.hs).
const presetContactName = "Ask SimpleX Team"

export const networkCommand = "/network socks=off smp-proxy=always smp-proxy-fallback=protected"

export type EventHandler = (event: ChatEvent) => Promise<void>

export interface Core {
  chat: api.ChatApi
  user: T.User
  events: (handler: EventHandler) => Promise<void>
}

export async function openCore(folder: string, displayName: string): Promise<Core> {
  createDataFolder(folder)
  const chat = await api.ChatApi.init({type: "sqlite", filePrefix: join(folder, "simplex")})
  try {
    const existing = await chat.apiGetActiveUser()
    const created = existing ?? (await chat.apiCreateActiveUser({displayName, fullName: "", peerType: T.ChatPeerType.Agent}))
    await chatCommand(chat, networkCommand, "networkConfig")
    const events = bufferEvents(chat)
    await chat.startChat()
    if (existing === undefined) await deletePresetContacts(chat, created.userId)
    const user = await updatedUser(chat, created, displayName)
    return {chat, user, events}
  } catch (error) {
    await chat.close().catch(() => undefined)
    throw error
  }
}

export async function deletePresetContacts(chat: api.ChatApi, userId: number): Promise<void> {
  const chats = await chat.apiGetChats(userId, {type: "last", count: newProfileChats}).catch(error => (logError("preset contacts", error), []))
  for (const {chatInfo} of chats) {
    if (chatInfo.type !== "direct") continue
    const {contactId, profile, activeConn} = chatInfo.contact
    if (profile.displayName !== presetContactName || activeConn !== undefined) continue
    await chat.apiDeleteChat(T.ChatType.Direct, contactId, {type: "full", notify: false}).catch(error => logError(`preset contact ${contactId}`, error))
  }
}

export async function updatedUser(chat: api.ChatApi, user: T.User, displayName: string): Promise<T.User> {
  const {profile} = user
  if (displayName === profile.displayName && profile.peerType === T.ChatPeerType.Agent) return user
  await chat.apiUpdateProfile(user.userId, {...util.fromLocalProfile(profile), displayName, peerType: T.ChatPeerType.Agent})
  return (await chat.apiGetActiveUser())!
}

export async function deliveries(chat: api.ChatApi, groupId: number): Promise<void> {
  const connIds = (await sqlRows(chat, `/sql chat SELECT hex(agent_conn_id) FROM connections WHERE group_member_id IN (SELECT group_member_id FROM group_members WHERE group_id = ${groupId})`)).slice(1)
  if (connIds.length === 0) return
  const pending = `/sql agent SELECT count(*) FROM snd_message_deliveries WHERE failed = 0 AND hex(conn_id) IN (${connIds.map(id => `'${id}'`).join(",")})`
  const deadline = Date.now() + deliveryTimeout
  while (Date.now() < deadline && Number((await sqlRows(chat, pending)).at(-1) ?? 0) > 0) {
    await new Promise(resolve => setTimeout(resolve, deliveryPoll))
  }
}

async function sqlRows(chat: api.ChatApi, query: string): Promise<string[]> {
  try {
    const {rows} = (await chat.sendChatCmd(query)) as unknown as {rows?: string[]}
    return rows ?? []
  } catch (error) {
    logError("delivery wait", error)
    return []
  }
}

export function bufferEvents(chat: api.ChatApi): (handler: EventHandler) => Promise<void> {
  const buffered: ChatEvent[] = []
  let eventHandler: EventHandler | undefined
  chat.onAny(async event => {
    if (eventHandler) await eventHandler(event)
    else buffered.push(event)
  })
  return async handler => {
    for (let event = buffered.shift(); event !== undefined; event = buffered.shift()) {
      const {type} = event
      await handler(event).catch(error => logError(`${type} event`, error))
    }
    eventHandler = handler
  }
}
