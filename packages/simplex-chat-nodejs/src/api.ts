import {CC, CEvt, ChatEvent, ChatResponse, T} from "@simplex-chat/types"
import * as core from "./core"

export class ChatCommandError extends Error {
  constructor(public message: string, public response: ChatResponse) {
    super(message)
  }
}

export enum ConnReqType {
  Invitation = "invitation",
  Contact = "contact",
}

export type EventSubscriberFunc<K extends CEvt.Tag> = (event: ChatEvent & {type: K}) => Promise<void>

interface EventSubscriber<K extends CEvt.Tag> {
  subscriber: EventSubscriberFunc<K>
  once: boolean
}

export class ChatApi {
  private receiveEvents = false
  private eventsLoop: Promise<void> | undefined = undefined
  private subscribers: {[K in CEvt.Tag]?: EventSubscriber<K>[]} = {}
  private receivers: ((event: ChatEvent) => Promise<void>)[] = []
  
  private constructor(protected ctrl_: bigint | undefined) {}

  static async init(
    dbPath: string,
    dbKey: string = "",
    confirm = core.MigrationConfirmation.YesUp
  ): Promise<ChatApi> {
    const ctrl = await core.chatMigrateInit(dbPath, dbKey, confirm)
    return new ChatApi(ctrl)
  }
    
  async startChat(): Promise<void> {
    this.receiveEvents = true
    this.eventsLoop = this.runEventsLoop()
    const r = await this.sendChatCmd(CC.StartChat.cmdString({mainApp: true, enableSndFiles: true}))
    if (r.type !== "chatStarted" && r.type !== "chatRunning") {
      throw new ChatCommandError("error starting chat", r)
    }
  }
  
  async stopChat(): Promise<void> {
    const r = await this.sendChatCmd("/_stop")
    if (r.type !== "chatStopped") throw new ChatCommandError("error starting chat", r)
    this.receiveEvents = false
    if (this.eventsLoop) await this.eventsLoop
    this.eventsLoop = undefined    
  }

  async close(): Promise<void> {
    this.receiveEvents = false
    if (this.eventsLoop) await this.eventsLoop
    this.eventsLoop = undefined    
    await core.chatCloseStore(this.ctrl)
    this.ctrl_ = undefined
  }
  
  private async runEventsLoop(): Promise<void> {
    while (this.receiveEvents) {
      try {
        const event = await this.recvChatEvent()
        if (!event) continue
        const subs = this.subscribers[event.type]
        if (subs) {
          for (const {subscriber, once} of [...subs]) {
            try { await (subscriber as EventSubscriberFunc<typeof event.type>)(event) }
            catch(e) { console.log(`${event.type} subsriber error`, e) }
            if (once) this.off(event.type, subscriber as EventSubscriberFunc<typeof event.type>)
          }
        }
        for (const r of [...this.receivers]) {          
          try { await r(event) }
          catch(e) { console.log(`${event.type} receiver error`, e) }        
        }
      } catch(err) {
        const e = err as core.ChatAPIError
        if ("chatError" in e) {
          console.log("Chat error", e.chatError)
        } else {
          console.log("Invalid event", e)
        }
      }
    }
  }

  on<K extends CEvt.Tag>(event: K, subscriber: EventSubscriberFunc<K>, once: boolean = false) {
    const subs: EventSubscriber<K>[] = this.subscribers[event] || (this.subscribers[event] = [])
    if (!subs.some(s => s.subscriber === subscriber)) {
      subs.push({subscriber, once})
    }
  }
  
  onAny(receiver: (event: ChatEvent) => Promise<void>) {
    if (!this.receivers.some(s => s === receiver)) {
      this.receivers.push(receiver)
    }
  }
  
  once<K extends CEvt.Tag>(event: K, subscriber: EventSubscriberFunc<K>) {
    this.on(event, subscriber, true)
  }

  // Waits for specific event, with optional predicate.
  // Returns `undefined` on timeout if specified.
  wait<K extends CEvt.Tag>(event: K): Promise<ChatEvent & {type: K}>
  wait<K extends CEvt.Tag>(event: K, predicate: ((event: ChatEvent & {type: K}) => boolean) | undefined): Promise<ChatEvent & {type: K}>
  wait<K extends CEvt.Tag>(event: K, timeout: number): Promise<ChatEvent & {type: K} | undefined>
  wait<K extends CEvt.Tag>(event: K, predicate: ((event: ChatEvent & {type: K}) => boolean) | undefined, timeout: number): Promise<ChatEvent & {type: K} | undefined>
  wait<K extends CEvt.Tag>(
    event: K,
    predicate: ((event: ChatEvent & {type: K}) => boolean) | undefined | number = undefined, // number for timeout
    timeout: number = 0 // milliseconds, default - indefinite
  ): Promise<ChatEvent & {type: K} | undefined> {
    if (typeof predicate === "number") {
      timeout = predicate
      predicate = undefined
    }
    return new Promise((resolve, reject) => {
      let done = false
      const cleanup = () => {
        done = true
        this.off(event, subscriber)    
      }
      const subscriber: EventSubscriberFunc<K> = async (evt: ChatEvent & {type: K}) => {
        if (done) return
        if (predicate) {
          try { if (!predicate(evt)) return }
          catch(e) { cleanup(); reject(e); return }
        }
        cleanup()
        resolve(evt)
      }
      this.on(event, subscriber)
      if (timeout > 0) {
        setTimeout(() => { if (!done) { cleanup(); resolve(undefined) } }, timeout)        
      }
    })
  }
    
  off<K extends CEvt.Tag>(event: K, subscriber: EventSubscriberFunc<K> | undefined = undefined) {
    if (subscriber) {
      const subs = this.subscribers[event]
      if (subs) {
        const i = subs.findIndex(s => s.subscriber === subscriber)
        if (i >= 0) subs.splice(i, 1)
      }
    } else {
      delete this.subscribers[event]
    }
  }

  offAny(receiver: ((event: ChatEvent) => Promise<void>) | undefined = undefined) {
    if (receiver) {
      const i = this.receivers.findIndex(r => r === receiver)
      if (i >= 0) this.receivers.splice(i, 1)
    } else {
      this.receivers = []
    }
  }
  
  get initialized(): boolean {
    return typeof this.ctrl_ === "bigint"
  }

  get receiving(): boolean {
    return this.receiveEvents && this.eventsLoop !== undefined 
  }

  get ctrl(): bigint {
    if (typeof this.ctrl_ === "bigint") return this.ctrl_
    else throw Error("chat api controller not initialized")
  }
  
  async sendChatCmd(cmd: string): Promise<ChatResponse> {
    return await core.chatSendCmd(this.ctrl, cmd)
  }
  
  async recvChatEvent(wait: number = 5_000_000): Promise<ChatEvent | undefined> {
    return await core.chatRecvMsgWait(this.ctrl, wait)
  }
  
  // Address commands
  // Bots can use these commands to automatically check and create address when initialized

  // Create bot address.
  // Network usage: interactive.
  async apiCreateUserAddress(userId: number): Promise<string> {
    const r = await this.sendChatCmd(CC.APICreateMyAddress.cmdString({userId}))
    if (r.type === "userContactLinkCreated") {
      const link = r.connLinkContact
      return link.connShortLink || link.connFullLink
    }
    throw new ChatCommandError("error creating user address", r)
  }

  // Delete bot address.
  // Network usage: background.
  async apiDeleteUserAddress(userId: number): Promise<void> {
    const r = await this.sendChatCmd(CC.APIDeleteMyAddress.cmdString({userId}))
    if (r.type === "userContactLinkDeleted") return
    throw new ChatCommandError("error deleting user address", r)
  }

  // Get bot address and settings.
  // Network usage: no.
  async apiGetUserAddress(userId: number): Promise<string | undefined> {
    try {
      const r = await this.sendChatCmd(CC.APIShowMyAddress.cmdString({userId}))
      switch (r.type) {
        case "userContactLink": {
          const link = r.contactLink.connLinkContact
          return link.connShortLink || link.connFullLink
        }
        default:
          throw new ChatCommandError("error loading user address", r)
      }
    } catch (err) {
      const e = err as any
      if (e.chatError?.type === "errorStore" && e.chatError.storeError?.type === "userContactLinkNotFound") return undefined
      throw e
    }
  }

  // Add address to bot profile.
  // Network usage: interactive.
  async apiSetProfileAddress(userId: number, enable: boolean): Promise<T.UserProfileUpdateSummary> {
    const r = await this.sendChatCmd(CC.APISetProfileAddress.cmdString({userId, enable}))
    switch (r.type) {
      case "userProfileUpdated":
        return r.updateSummary
      default:
        throw new ChatCommandError("error loading user address", r)
    }
  }

  // Set bot address settings.
  // Network usage: interactive.
  async apiSetAddressSettings(userId: number, settings: T.AddressSettings): Promise<void> {
    const r = await this.sendChatCmd(
      CC.APISetAddressSettings.cmdString({userId, settings})
    )
    if (r.type !== "userContactLinkUpdated") {
      throw new ChatCommandError("error changing user contact address settings", r)
    }  
  }

  async enableAddressAutoAccept(userId: number, autoReply?: T.MsgContent, businessAddress = false): Promise<void> {
    await this.apiSetAddressSettings(userId, {businessAddress, autoAccept: {acceptIncognito: false}, autoReply})
  }

  async disableAddressAutoAccept(userId: number, businessAddress = false): Promise<void> {
    await this.apiSetAddressSettings(userId, {businessAddress})
  }

  // Message commands
  // Commands to send, update, delete, moderate messages and set message reactions

  // Send messages.
  // Network usage: background.
  async apiSendMessages(chatType: T.ChatType, chatId: number, messages: T.ComposedMessage[]): Promise<T.AChatItem[]> {
    const r = await this.sendChatCmd(
      CC.APISendMessages.cmdString({sendRef: {chatType, chatId}, composedMessages: messages, liveMessage: false})
    )
    if (r.type === "newChatItems") return r.chatItems
    throw new ChatCommandError("unexpected response", r)
  }

  async apiSendTextMessage(chatType: T.ChatType, chatId: number, text: string): Promise<T.AChatItem[]> {
    return this.apiSendMessages(chatType, chatId, [{msgContent: {type: "text", text}, mentions: {}}])
  }

  // Update message.
  // Network usage: background.
  async apiUpdateChatItem(chatType: T.ChatType, chatId: number, chatItemId: number, msgContent: T.MsgContent): Promise<T.ChatItem> {
    const r = await this.sendChatCmd(
      CC.APIUpdateChatItem.cmdString({
        chatRef: {chatType, chatId},
        chatItemId,
        liveMessage: false,
        updatedMessage: {msgContent, mentions: {}},
      })
    )
    if (r.type === "chatItemUpdated") return r.chatItem.chatItem
    throw new ChatCommandError("error updating chat item", r)
  }

  // Delete message.
  // Network usage: background.
  async apiDeleteChatItems(
    chatType: T.ChatType,
    chatId: number,
    chatItemIds: number[],
    deleteMode: T.CIDeleteMode
  ): Promise<T.ChatItemDeletion[]> {
    const r = await this.sendChatCmd(CC.APIDeleteChatItem.cmdString({chatRef: {chatType, chatId}, chatItemIds, deleteMode}))
    if (r.type === "chatItemsDeleted") return r.chatItemDeletions
    throw new ChatCommandError("error deleting chat item", r)
  }

  // Moderate message. Requires Moderator role (and higher than message author's).
  // Network usage: background.
  async apiDeleteMemberChatItem(groupId: number, chatItemIds: number[]): Promise<T.ChatItemDeletion[]> {
    const r = await this.sendChatCmd(CC.APIDeleteMemberChatItem.cmdString({groupId, chatItemIds}))
    if (r.type === "chatItemsDeleted") return r.chatItemDeletions
    throw new ChatCommandError("error deleting member chat item", r)  
  }

  // Add/remove message reaction.
  // Network usage: background.
  async apiChatItemReaction(
    chatType: T.ChatType,
    chatId: number,
    chatItemId: number,
    add: boolean,
    reaction: T.MsgReaction
  ) {
    const r = await this.sendChatCmd(CC.APIChatItemReaction.cmdString({chatRef: {chatType, chatId}, chatItemId, add, reaction}))
    if (r.type === "chatItemsDeleted") return r.chatItemDeletions
    throw new ChatCommandError("error setting item reaction", r)  
  }

  // File commands
  // Commands to receive and to cancel files. Files are sent as part of the message, there are no separate commands to send files.

  // Receive file.
  // Network usage: no.
  async apiReceiveFile(fileId: number): Promise<T.AChatItem> {
    const r = await this.sendChatCmd(CC.ReceiveFile.cmdString({fileId, userApprovedRelays: true}))
    if (r.type === "rcvFileAccepted") return r.chatItem
    throw new ChatCommandError("error receiving file", r)
  }

  // Cancel file.
  // Network usage: background.
  async apiCancelFile(fileId: number): Promise<void> {
    const r = await this.sendChatCmd(CC.CancelFile.cmdString({fileId}))
    if (r.type === "sndFileCancelled" || r.type === "rcvFileCancelled") return
    throw new ChatCommandError("error canceling file", r)
  }

  // Group commands
  // Commands to manage and moderate groups. These commands can be used with business chats as well - they are groups. E.g., a common scenario would be to add human agents to business chat with the customer who connected via business address.

  // Add contact to group. Requires bot to have Admin role.
  // Network usage: interactive.
  async apiAddMember(groupId: number, contactId: number, memberRole: T.GroupMemberRole): Promise<T.GroupMember> {
    const r = await this.sendChatCmd(CC.APIAddMember.cmdString({groupId, contactId, memberRole}))
    if (r.type === "sentGroupInvitation") return r.member
    throw new ChatCommandError("error adding member", r)
  }

  // Join group.
  // Network usage: interactive.
  async apiJoinGroup(groupId: number): Promise<T.GroupInfo> {
    const r = await this.sendChatCmd(CC.APIJoinGroup.cmdString({groupId}))
    if (r.type === "userAcceptedGroupSent") return r.groupInfo
    throw new ChatCommandError("error joining group", r)
  }

  // Accept group member. Requires Admin role.
  // Network usage: background.
  async apiAcceptMember(groupId: number, groupMemberId: number, memberRole: T.GroupMemberRole): Promise<T.GroupMember> {
    const r = await this.sendChatCmd(CC.APIAcceptMember.cmdString({groupId, groupMemberId, memberRole}))
    if (r.type === "memberAccepted") return r.member
    throw new ChatCommandError("error accepting member", r)
  }
  
  // Set members role. Requires Admin role.
  // Network usage: background.
  async apiSetMembersRole(groupId: number, groupMemberIds: number[], memberRole: T.GroupMemberRole): Promise<void> {
    const r = await this.sendChatCmd(CC.APIMembersRole.cmdString({groupId, groupMemberIds, memberRole}))
    if (r.type === "membersRoleUser") return
    throw new ChatCommandError("error setting members role", r)
  }

  // Block members. Requires Moderator role.
  // Network usage: background.
  async apiBlockMembersForAll(groupId: number, groupMemberIds: number[], blocked: boolean): Promise<void> {
    const r = await this.sendChatCmd(CC.APIBlockMembersForAll.cmdString({groupId, groupMemberIds, blocked}))
    if (r.type === "membersBlockedForAllUser") return
    throw new ChatCommandError("error blocking members", r)
  }
  
  // Remove members. Requires Admin role.
  // Network usage: background.
  async apiRemoveMembers(groupId: number, memberIds: number[], withMessages = false): Promise<T.GroupMember[]> {
    const r = await this.sendChatCmd(CC.APIRemoveMembers.cmdString({groupId, groupMemberIds: memberIds, withMessages}))
    if (r.type === "userDeletedMembers") return r.members
    throw new ChatCommandError("error removing member", r)
  }

  // Leave group.
  // Network usage: background.
  async apiLeaveGroup(groupId: number): Promise<T.GroupInfo> {
    const r = await this.sendChatCmd(CC.APILeaveGroup.cmdString({groupId}))
    if (r.type === "leftMemberUser") return r.groupInfo
    throw new ChatCommandError("error leaving group", r)
  }

  // Get group members.
  // Network usage: no.
  async apiListMembers(groupId: number): Promise<T.GroupMember[]> {
    const r = await this.sendChatCmd(CC.APIListMembers.cmdString({groupId}))
    if (r.type === "groupMembers") return r.group.members
    throw new ChatCommandError("error getting group members", r)
  }

  // Create group.
  // Network usage: no.
  async apiNewGroup(userId: number, groupProfile: T.GroupProfile): Promise<T.GroupInfo> {
    const r = await this.sendChatCmd(CC.APINewGroup.cmdString({userId, groupProfile, incognito: false}))
    if (r.type === "groupCreated") return r.groupInfo
    throw new ChatCommandError("error creating group", r)
  }

  // Update group profile.
  // Network usage: background.
  async apiUpdateGroupProfile(groupId: number, groupProfile: T.GroupProfile): Promise<T.GroupInfo> {
    const r = await this.sendChatCmd(CC.APIUpdateGroupProfile.cmdString({groupId, groupProfile}))
    if (r.type === "groupUpdated") return r.toGroup
    throw new ChatCommandError("error updating group", r)
  }

  // Group link commands
  // These commands can be used by bots that manage multiple public groups

  // Create group link.
  // Network usage: interactive.
  async apiCreateGroupLink(groupId: number, memberRole: T.GroupMemberRole): Promise<string> {
    const r = await this.sendChatCmd(CC.APICreateGroupLink.cmdString({groupId, memberRole}))
    if (r.type === "groupLinkCreated") {
      const link = r.groupLink.connLinkContact
      return link.connShortLink || link.connFullLink
    }
    throw new ChatCommandError("error creating group link", r)
  }

  // Set member role for group link.
  // Network usage: no.
  async apiSetGroupLinkMemberRole(groupId: number, memberRole: T.GroupMemberRole): Promise<void> {
    const r = await this.sendChatCmd(CC.APIGroupLinkMemberRole.cmdString({groupId, memberRole}))
    if (r.type !== "groupLink") throw new ChatCommandError("error setting group link member role", r)
  }
  
  // Delete group link.
  // Network usage: background.
  async apiDeleteGroupLink(groupId: number): Promise<void> {
    const r = await this.sendChatCmd(CC.APIDeleteGroupLink.cmdString({groupId}))
    if (r.type !== "groupLinkDeleted") throw new ChatCommandError("error deleting group link", r)
  }

  // Get group link.
  // Network usage: no.
  async apiGetGroupLink(groupId: number): Promise<T.GroupLink> {
    const r = await this.sendChatCmd(CC.APIGetGroupLink.cmdString({groupId}))
    if (r.type === "groupLink") return r.groupLink
    throw new ChatCommandError("error getting group link", r)
  }
  
  async apiGetGroupLinkStr(groupId: number): Promise<string> {
    const link = (await this.apiGetGroupLink(groupId)).connLinkContact
    return link.connShortLink || link.connFullLink
  }

  // Connection commands
  // These commands may be used to create connections. Most bots do not need to use them - bot users will connect via bot address with auto-accept enabled.

  // Create 1-time invitation link.
  // Network usage: interactive.
  async apiCreateLink(userId: number): Promise<string> {
    const r = await this.sendChatCmd(CC.APIAddContact.cmdString({userId, incognito: false}))
    if (r.type === "invitation") {
      const link = r.connLinkInvitation
      return link.connShortLink || link.connFullLink
    }
    throw new ChatCommandError("error creating link", r)
  }

  // Determine SimpleX link type and if the bot is already connected via this link.
  // Network usage: interactive.
  async apiConnectPlan(userId: number, connectionLink: string): Promise<T.ConnectionPlan> {
    const r = await this.sendChatCmd(CC.APIConnectPlan.cmdString({userId, connectionLink}))
    if (r.type === "connectionPlan") return r.connectionPlan
    throw new ChatCommandError("error getting connect plan", r)
  }

  // Connect via prepared SimpleX link. The link can be 1-time invitation link, contact address or group link
  // Network usage: interactive.
  async apiConnect(userId: number, incognito: boolean, preparedLink?: T.CreatedConnLink): Promise<ConnReqType> {
    const r = await this.sendChatCmd(CC.APIConnect.cmdString({userId, incognito, preparedLink_: preparedLink}))
    return this.handleConnectResult(r)
  }

  // Connect via SimpleX link as string in the active user profile.
  // Network usage: interactive.
  async apiConnectActiveUser(connLink: string): Promise<ConnReqType> {
    const r = await this.sendChatCmd(CC.Connect.cmdString({incognito: false, connLink_: connLink}))
    return this.handleConnectResult(r)
  }

  private handleConnectResult(r: ChatResponse): ConnReqType {
    switch (r.type) {
      case "sentConfirmation":
        return ConnReqType.Invitation
      case "sentInvitation":
        return ConnReqType.Contact
      case "contactAlreadyExists":
        throw new ChatCommandError("contact already exists", r)
      default:
        throw new ChatCommandError("connection error", r)
    }    
  }  
  
  // Accept contact request.
  // Network usage: interactive.
  async apiAcceptContactRequest(contactReqId: number): Promise<T.Contact> {
    const r = await this.sendChatCmd(CC.APIAcceptContact.cmdString({contactReqId}))
    if (r.type === "acceptingContactRequest") return r.contact
    throw new ChatCommandError("error accepting contact request", r)
  }

  // Reject contact request. The user who sent the request is **not notified**.
  // Network usage: no.
  async apiRejectContactRequest(contactReqId: number): Promise<void> {
    const r = await this.sendChatCmd(CC.APIRejectContact.cmdString({contactReqId}))
    if (r.type === "contactRequestRejected") return
    throw new ChatCommandError("error rejecting contact request", r)
  }

  // Chat commands
  // Commands to list and delete conversations.

  // Get contacts.
  // Network usage: no.
  async apiListContacts(userId: number): Promise<T.Contact[]> {
    const r = await this.sendChatCmd(CC.APIListContacts.cmdString({userId}))
    if (r.type === "contactsList") return r.contacts
    throw new ChatCommandError("error listing contacts", r)
  }

  // Get groups.
  // Network usage: no.
  async apiListGroups(userId: number, contactId?: number, search?: string): Promise<T.GroupInfo[]> {
    const r = await this.sendChatCmd(CC.APIListGroups.cmdString({userId, contactId_: contactId, search}))
    if (r.type === "groupsList") return r.groups
    throw new ChatCommandError("error listing groups", r)
  }

  // Delete chat.
  // Network usage: background.
  async apiDeleteChat(chatType: T.ChatType, chatId: number, deleteMode: T.ChatDeleteMode = {type: "full", notify: true}): Promise<void> {
    const r = await this.sendChatCmd(CC.APIDeleteChat.cmdString({chatRef: {chatType, chatId}, chatDeleteMode: deleteMode}))
    switch (chatType) {
      case T.ChatType.Direct:
        if (r.type === "contactDeleted") return
        break
      case T.ChatType.Group:
        if (r.type === "groupDeletedUser") return
        break
    }
    throw new ChatCommandError("error deleting chat", r)
  }

  // User profile commands
  // Most bots don't need to use these commands, as bot profile can be configured manually via CLI or desktop client. These commands can be used by bots that need to manage multiple user profiles (e.g., the profiles of support agents).

  // Get active user profile
  // Network usage: no.
  async apiGetActiveUser(): Promise<T.User | undefined> {
    try {
      const r = await this.sendChatCmd(CC.ShowActiveUser.cmdString({}))
      switch (r.type) {
        case "activeUser":
          return r.user
        default:
          throw new ChatCommandError("unexpected response", r)
      }
    } catch (err) {
      const e = err as core.ChatAPIError
      if (e.chatError?.type === "error" && e.chatError.errorType.type === "noActiveUser") return undefined
      throw err
    }
  }

  // Create new user profile
  // Network usage: no.
  async apiCreateActiveUser(profile?: T.Profile): Promise<T.User> {
    const r = await this.sendChatCmd(CC.CreateActiveUser.cmdString({newUser: {profile, pastTimestamp: false}}))
    if (r.type === "activeUser") return r.user
    throw new ChatCommandError("unexpected response", r)
  }

  // Get all user profiles
  // Network usage: no.
  async apiListUsers(): Promise<T.UserInfo[]> {
    const r = await this.sendChatCmd(CC.ListUsers.cmdString({}))
    if (r.type === "usersList") return r.users
    throw new ChatCommandError("error listing users", r)
  }

  // Set active user profile
  // Network usage: no.
  async apiSetActiveUser(userId: number, viewPwd?: string): Promise<T.User> {
    const r = await this.sendChatCmd(CC.APISetActiveUser.cmdString({userId, viewPwd}))
    if (r.type === "activeUser") return r.user
    throw new ChatCommandError("error setting active user", r)
  }

  // Delete user profile.
  // Network usage: background.
  async apiDeleteUser(userId: number, delSMPQueues: boolean, viewPwd?: string): Promise<void> {
    const r = await this.sendChatCmd(CC.APIDeleteUser.cmdString({userId, delSMPQueues, viewPwd}))
    if (r.type === "cmdOk") return
    throw new ChatCommandError("error deleting user", r)
  }

  // Update user profile.
  // Network usage: background.
  async apiUpdateProfile(userId: number, profile: T.Profile): Promise<T.Profile | undefined> {
    const r = await this.sendChatCmd(CC.APIUpdateProfile.cmdString({userId, profile}))
    switch (r.type) {
      case "userProfileNoChange":
        return undefined
      case "userProfileUpdated":
        return r.toProfile
      default:
        throw new ChatCommandError("error updating profile", r)
    }
  }

  // Configure chat preference overrides for the contact.
  // Network usage: background.
  async apiSetContactPrefs(contactId: number, preferences: T.Preferences): Promise<void> {
    const r = await this.sendChatCmd(CC.APISetContactPrefs.cmdString({contactId, preferences}))
    if (r.type !== "contactPrefsUpdated") throw new ChatCommandError("error setting contact prefs", r)
  }
}
