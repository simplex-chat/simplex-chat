import {api} from "simplex-chat"
import {T} from "@simplex-chat/types"
import {Config, IdName} from "./config.js"
import {loadGrokContext} from "./context.js"

// Ids the bot persists; a miss means it would create the entity again.
export interface DryRunState {
  teamGroupId?: number
  grokContactId?: number
  grokUserId?: number
}

// Names are searched rather than listing every chat: a bot with thousands of
// customer chats would otherwise be paged through to find a handful of ids.
const SEARCH_LIMIT = 100

// Only for the failure path, where the id has to be reported with the name it
// actually has: a renamed contact does not match the search above.
const LISTING_LIMIT = 1000

const GROK_PROFILE_NAME = "Grok"

export async function dryRun(chat: api.ChatApi, config: Config, state: DryRunState): Promise<boolean> {
  const results: boolean[] = []
  const check = (ok: boolean, msg: string): void => {
    results.push(ok)
    console.log(`${ok ? "ok  " : "FAIL"}  ${msg}`)
  }
  // Absence cannot be proven with the chat stopped: getChatPreviews filters on
  // contacts.contact_used, so a contact whose direct chat was never used is
  // invisible here even though the bot finds it at startup.
  const unverified = (msg: string): void => console.log(`?     ${msg}`)

  if (config.contextFile) {
    try {
      const context = loadGrokContext(config.contextFile)
      check(true, `Grok context: ${context.length} message(s) from ${config.contextFile}`)
    } catch (err) {
      const e = err as NodeJS.ErrnoException
      const missing = e.code === "ENOENT"
      check(missing, `Grok context ${config.contextFile}: ${missing ? "not found, would start without it" : e.message}`)
    }
  }

  const user = await chat.apiGetActiveUser()
  if (!user) {
    check(false, "no active user in database, the bot would create one")
    return false
  }
  check(true, `active user: ${user.userId}:${user.profile.displayName}`)

  if (state.grokUserId !== undefined) {
    const users = await chat.apiListUsers()
    const found = users.find(u => u.user.userId === state.grokUserId)
    check(!!found, `Grok user ${state.grokUserId}: ${found ? found.user.profile.displayName : "missing, the bot would exit"}`)
  }

  if (state.teamGroupId !== undefined) {
    const group = await findGroup(chat, user.userId, config.teamGroup.name, state.teamGroupId)
    if (!group) {
      unverified(`team group ${state.teamGroupId} not in chat previews, the bot verifies it at startup`)
    } else {
      check(true, `team group ${state.teamGroupId}: ${group.groupProfile.displayName}`)
    }
    if (group && group.groupProfile.displayName !== config.teamGroup.name) {
      check(true, `team group name differs from --team-group, profile would be updated to "${config.teamGroup.name}"`)
    }
  } else {
    check(true, `no team group in state, "${config.teamGroup.name}" would be created`)
  }

  if (state.grokContactId !== undefined) {
    const contact = await findContact(chat, user.userId, GROK_PROFILE_NAME, state.grokContactId)
    if (contact) check(true, `Grok contact ${state.grokContactId}: ${contact.profile.displayName}`)
    else unverified(`Grok contact ${state.grokContactId} not in chat previews, the bot verifies it at startup`)
  }

  await checkContacts(chat, user.userId, "team member", config.teamMembers, check, unverified)
  await checkContacts(chat, user.userId, "broadcaster", config.broadcasters, check, unverified)

  return results.every(Boolean)
}

async function checkContacts(
  chat: api.ChatApi,
  userId: number,
  role: string,
  contacts: IdName[],
  check: (ok: boolean, msg: string) => void,
  unverified: (msg: string) => void
): Promise<void> {
  for (const {id, name} of contacts) {
    const contact = await findContact(chat, userId, name, id)
    if (!contact) {
      unverified(`${role} ${id}:${name} not in chat previews, the bot verifies it at startup`)
    } else {
      const match = contact.profile.displayName === name
      check(match, `${role} ${id}:${name}${match ? "" : ` has display name "${contact.profile.displayName}", the bot would exit`}`)
    }
  }
}

async function findGroup(chat: api.ChatApi, userId: number, search: string, groupId: number): Promise<T.GroupInfo | undefined> {
  const group = (chats: T.AChat[]): T.GroupInfo | undefined => {
    for (const {chatInfo} of chats) {
      if (chatInfo.type === "group" && chatInfo.groupInfo.groupId === groupId) return chatInfo.groupInfo
    }
    return undefined
  }
  return group(await searchChats(chat, userId, search)) ?? group(await recentChats(chat, userId))
}

async function findContact(chat: api.ChatApi, userId: number, search: string, contactId: number): Promise<T.Contact | undefined> {
  const contact = (chats: T.AChat[]): T.Contact | undefined => {
    for (const {chatInfo} of chats) {
      if (chatInfo.type === "direct" && chatInfo.contact.contactId === contactId) return chatInfo.contact
    }
    return undefined
  }
  return contact(await searchChats(chat, userId, search)) ?? contact(await recentChats(chat, userId))
}

function searchChats(chat: api.ChatApi, userId: number, search: string): Promise<T.AChat[]> {
  return chat.apiGetChats(userId, {type: "last", count: SEARCH_LIMIT}, {type: "search", search})
}

function recentChats(chat: api.ChatApi, userId: number): Promise<T.AChat[]> {
  return chat.apiGetChats(userId, {type: "last", count: LISTING_LIMIT})
}
