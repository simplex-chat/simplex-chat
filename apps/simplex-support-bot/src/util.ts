import {Mutex} from "async-mutex"
import {api, core} from "simplex-chat"
import {T} from "@simplex-chat/types"

export const profileMutex = new Mutex()

export function isChatNotFound(err: unknown, kind: "group" | "contact"): boolean {
  if (!(err instanceof core.ChatAPIError)) return false
  if (err.chatError?.type !== "errorStore") return false
  const seType = err.chatError.storeError.type
  return kind === "group" ? seType === "groupNotFound" : seType === "contactNotFound"
}

export async function getGroupInfo(chat: api.ChatApi, groupId: number): Promise<T.GroupInfo | null> {
  try {
    const c = await chat.apiGetChat(T.ChatType.Group, groupId, 0)
    return c.chatInfo.type === "group" ? c.chatInfo.groupInfo : null
  } catch (err) {
    if (isChatNotFound(err, "group")) return null
    throw err
  }
}

export async function getContact(chat: api.ChatApi, contactId: number): Promise<T.Contact | null> {
  try {
    const c = await chat.apiGetChat(T.ChatType.Direct, contactId, 0)
    return c.chatInfo.type === "direct" ? c.chatInfo.contact : null
  } catch (err) {
    if (isChatNotFound(err, "contact")) return null
    throw err
  }
}

export function isWeekend(timezone: string): boolean {
  const day = new Intl.DateTimeFormat("en-US", {timeZone: timezone, weekday: "short"}).format(new Date())
  return day === "Sat" || day === "Sun"
}

export function log(msg: string, ...args: unknown[]): void {
  const ts = new Date().toISOString()
  if (args.length > 0) {
    console.log(`[${ts}] ${msg}`, ...args)
  } else {
    console.log(`[${ts}] ${msg}`)
  }
}

export function logError(msg: string, err: unknown): void {
  const ts = new Date().toISOString()
  console.error(`[${ts}] ERROR: ${msg}`, err)
}
