import {mkdirSync, rmSync} from "fs"
import {join} from "path"
import {ChatEvent, T} from "@simplex-chat/types"
import {api, util} from "simplex-chat"
import {createDataFolder} from "./files"
import {logError} from "./log"

const newProfileChats = 100
// Display name of the contact the core adds to every new user (createPresetContactCards in Commands.hs).
const presetContactName = "Ask SimpleX Team"

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
    await prepareCore(chat, folder)
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

export async function prepareCore(chat: api.ChatApi, data: string): Promise<void> {
  const networkConfig = await chat.apiGetNetworkConfig()
  await chat.apiSetNetworkConfig({...networkConfig, smpProxyMode: T.SMPProxyMode.Always, smpProxyFallback: T.SMPProxyFallback.AllowProtected})
  await setTempFolder(chat, join(data, "tmp"))
  rmSync(join(data, "files", "sent"), {recursive: true, force: true})
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

async function setTempFolder(chat: api.ChatApi, folder: string): Promise<void> {
  mkdirSync(folder, {recursive: true, mode: 0o700})
  const r = await chat.sendChatCmd(`/_temp_folder ${folder}`)
  if (r.type !== "cmdOk") throw new api.ChatCommandError("error setting the temp folder", r)
}
