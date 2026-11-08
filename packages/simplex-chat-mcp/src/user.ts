import {join} from "path"
import {CC, ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {openCore} from "./core"
import {logError} from "./log"
import {memberCurrent, newestGroup} from "./tools"

const userName = "User"
const claudeKey = "simplexMcpClaude"
const userKey = "simplexMcpUser"
const sessionKey = "simplexMcpSession"
const connectTimeout = 60_000

export class User {
  private constructor(private readonly chat: api.ChatApi, private readonly user: T.User) {}

  static async open(root: string, hub: api.ChatApi, hubUser: T.User): Promise<User> {
    const core = await openCore(join(root, "_user"), userName)
    try {
      const user = new User(core.chat, core.user)
      await core.events(event => user.onEvent(event))
      if ((await userContact(hub, hubUser.userId)) === undefined) await user.connect(hub, hubUser)
      return user
    } catch (error) {
      await core.chat.close().catch(() => undefined)
      throw error
    }
  }

  async post(session: number, parts: string[]): Promise<void> {
    const group = newestGroup((await this.chat.apiListGroups(this.user.userId)).filter(({customData, membership}) => (customData as {[sessionKey]?: number} | undefined)?.[sessionKey] === session && memberCurrent(membership)))
    if (group !== undefined) await this.chat.apiSendMessages([T.ChatType.Group, group.groupId], parts.map(text => ({msgContent: {type: "text", text}, mentions: {}})))
  }

  close(): Promise<void> {
    return this.chat.close()
  }

  private async connect(hub: api.ChatApi, hubUser: T.User): Promise<void> {
    const r = await hub.sendChatCmd(CC.APIAddContact.cmdString({userId: hubUser.userId, incognito: false}))
    if (r.type !== "invitation") throw new api.ChatCommandError("error creating link", r)
    const {connLinkInvitation, connection} = r
    const connected = hub.wait("contactConnected", ({contact}) => contact.activeConn?.connId === connection.pccConnId, connectTimeout)
    const [, connLink] = await this.chat.apiConnectPlan(this.user.userId, connLinkInvitation.connShortLink || connLinkInvitation.connFullLink)
    await this.chat.apiConnect(this.user.userId, false, connLink)
    const event = await connected
    if (event === undefined) throw new Error("The User profile did not connect to Claude.")
    await hub.apiSetContactCustomData(event.contact.contactId, {[userKey]: true})
  }

  private async onEvent(event: ChatEvent): Promise<void> {
    if (event.type === "contactConnected") return this.chat.apiSetContactCustomData(event.contact.contactId, {[claudeKey]: true})
    if (event.type !== "receivedGroupInvitation" || (event.contact.customData as {[claudeKey]?: boolean} | undefined)?.[claudeKey] !== true) return
    const session = Number(/^(\d+) · /.exec(event.groupInfo.groupProfile.displayName)?.[1])
    if (!Number.isInteger(session)) return
    await this.chat.apiJoinGroup(event.groupInfo.groupId).catch(error => logError(`group of session ${session}`, error))
    await this.chat.apiSetGroupCustomData(event.groupInfo.groupId, {[sessionKey]: session})
  }
}

export async function userContact(chat: api.ChatApi, userId: number): Promise<T.Contact | undefined> {
  return (await chat.apiListContacts(userId)).find(({customData}) => (customData as {[userKey]?: boolean} | undefined)?.[userKey] === true)
}
