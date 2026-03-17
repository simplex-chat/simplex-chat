import {api, util} from "simplex-chat"
import {T, CEvt} from "@simplex-chat/types"
import {Config} from "./config.js"
import {GrokMessage} from "./state.js"
import {GrokApiClient} from "./grok.js"
import {teamQueueMessage, grokActivatedMessage, teamAddedMessage, teamLockedMessage, teamAlreadyAddedMessage} from "./messages.js"
import {log, logError} from "./util.js"

const MAX_MSG_TEXT_BYTES = 15000 // conservative limit under SimpleX's maxEncodedMsgLength (15,602) minus JSON envelope

// --- Exported types for persistence ---

export type SenderType = "customer" | "team" | "grok"

export interface GroupMetadata {
  firstContact: number
  msgCount: number
  customerName: string
}

export interface GroupPendingInfo {
  lastEventType: "message" | "reaction"
  lastEventFrom: SenderType
  lastEventTimestamp: number
  lastMessageFrom: SenderType
}

// --- Internal types ---

interface GroupComposition {
  grokMember: T.GroupMember | undefined
  teamMember: T.GroupMember | undefined
}

function isActiveMember(m: T.GroupMember): boolean {
  return m.memberStatus === T.GroupMemberStatus.Connected
    || m.memberStatus === T.GroupMemberStatus.Complete
    || m.memberStatus === T.GroupMemberStatus.Announced
}

export class SupportBot {
  // Grok group mapping (persisted via onGrokMapChanged callback)
  private pendingGrokJoins = new Map<string, number>()      // memberId → mainGroupId
  private grokGroupMap = new Map<number, number>()           // mainGroupId → grokLocalGroupId
  private reverseGrokMap = new Map<number, number>()         // grokLocalGroupId → mainGroupId
  private grokJoinResolvers = new Map<number, () => void>()  // mainGroupId → resolve fn
  private grokFullyConnected = new Set<number>()             // mainGroupIds where connectedToGroupMember fired

  // Forwarded message tracking: "groupId:itemId" → {teamItemId, header, sender}
  private forwardedItems = new Map<string, {teamItemId: number; header: string; sender: SenderType}>()

  // [NEW] marker tracking: groupId → {teamItemId, timestamp, originalText}
  private newItems = new Map<number, {teamItemId: number; timestamp: number; originalText: string}>()

  // Pending DMs for team group members (contactId → message) — sent on contactConnected
  private pendingTeamDMs = new Map<number, string>()

  // Pending owner role assignments: "groupId:groupMemberId" — set on member connect
  private pendingOwnerRole = new Set<string>()

  // Groups where welcome flow (teamQueueMessage) was already completed
  private welcomeCompleted = new Set<number>()

  // Group activity tracking: groupId → last customer message timestamp (ms)
  private groupLastActive = new Map<number, number>()

  // A1: Reply-to-last threading: groupId → last teamItemId for that customer group
  private lastTeamItemByGroup = new Map<number, number>()

  // A4: Group metadata (firstContact, msgCount, customerName) — persisted
  private groupMetadata = new Map<number, GroupMetadata>()

  // D1: Pending tracking — persisted
  private groupPendingInfo = new Map<number, GroupPendingInfo>()

  // Bot's business address link (set after startup)
  businessAddress: string | null = null

  // Callback to persist grokGroupMap changes
  onGrokMapChanged: ((map: ReadonlyMap<number, number>) => void) | null = null

  // Callback to persist newItems changes
  onNewItemsChanged: ((map: ReadonlyMap<number, {teamItemId: number; timestamp: number; originalText: string}>) => void) | null = null

  // Callback to persist groupLastActive changes
  onGroupLastActiveChanged: ((map: ReadonlyMap<number, number>) => void) | null = null

  // Callback to persist groupMetadata changes
  onGroupMetadataChanged: ((map: ReadonlyMap<number, GroupMetadata>) => void) | null = null

  // Callback to persist groupPendingInfo changes
  onGroupPendingInfoChanged: ((map: ReadonlyMap<number, GroupPendingInfo>) => void) | null = null

  constructor(
    private mainChat: api.ChatApi,
    private grokChat: api.ChatApi,
    private grokApi: GrokApiClient,
    private config: Config,
  ) {}

  // --- Restore Methods ---

  restoreGrokGroupMap(entries: [number, number][]): void {
    for (const [mainGroupId, grokLocalGroupId] of entries) {
      this.grokGroupMap.set(mainGroupId, grokLocalGroupId)
      this.reverseGrokMap.set(grokLocalGroupId, mainGroupId)
    }
    log(`Restored Grok group map: ${entries.length} entries`)
  }

  restoreNewItems(entries: [number, {teamItemId: number; timestamp: number; originalText: string}][]): void {
    const now = Date.now()
    const DAY_MS = 24 * 60 * 60 * 1000
    for (const [groupId, info] of entries) {
      if (now - info.timestamp < DAY_MS) {
        this.newItems.set(groupId, info)
      }
    }
    log(`Restored NEW items: ${this.newItems.size} entries (pruned ${entries.length - this.newItems.size} expired)`)
  }

  restoreGroupLastActive(entries: [number, number][]): void {
    const now = Date.now()
    const PRUNE_MS = 48 * 60 * 60 * 1000
    for (const [groupId, timestamp] of entries) {
      if (now - timestamp < PRUNE_MS) {
        this.groupLastActive.set(groupId, timestamp)
      }
    }
    log(`Restored group activity: ${this.groupLastActive.size} entries (pruned ${entries.length - this.groupLastActive.size} expired)`)
  }

  restoreGroupMetadata(entries: [number, GroupMetadata][]): void {
    for (const [groupId, meta] of entries) {
      this.groupMetadata.set(groupId, meta)
    }
    log(`Restored group metadata: ${entries.length} entries`)
  }

  restoreGroupPendingInfo(entries: [number, GroupPendingInfo][]): void {
    for (const [groupId, info] of entries) {
      this.groupPendingInfo.set(groupId, info)
    }
    log(`Restored pending info: ${entries.length} entries`)
  }

  // --- Format Helpers (A2, A3, A4, A5) ---

  private formatDuration(ms: number): string {
    if (ms < 60_000) return "<1m"
    if (ms < 3_600_000) return `${Math.floor(ms / 60_000)}m`
    if (ms < 86_400_000) return `${Math.floor(ms / 3_600_000)}h`
    return `${Math.floor(ms / 86_400_000)}d`
  }

  private buildHeader(
    groupId: number,
    customerName: string,
    state: string,
    msgNum: number,
    firstContactTime: number | undefined,
    sender: SenderType,
    senderLabel?: string,
  ): string {
    const parts: string[] = []
    // A5: sender identification
    if (sender === "team" && senderLabel) {
      parts.push(`${senderLabel} > ${groupId}:${customerName}`)
    } else if (sender === "grok") {
      parts.push(`Grok > ${groupId}:${customerName}`)
    } else {
      parts.push(`${groupId}:${customerName}`)
    }
    // A3: state indicator
    parts.push(state)
    // A4: message number
    parts.push(`#${msgNum}`)
    // A4: duration since first contact
    if (firstContactTime !== undefined) {
      const elapsed = Date.now() - firstContactTime
      if (elapsed >= 60_000) {
        parts.push(this.formatDuration(elapsed))
      }
    }
    return parts.join(" · ")
  }

  // A2+A5: Build the full formatted message with color coding
  private formatForwardMessage(header: string, body: string, sender: SenderType, isNew: boolean): string {
    let line = ""
    // A5: Color-coded prefix
    if (isNew) {
      line += "!1 NEW! "
    } else if (sender === "team") {
      line += "!2 >>! "
    } else if (sender === "grok") {
      line += "!5 AI! "
    }
    // A2: Bold header
    line += `*${header}*`
    // A5: Italic body for Grok responses
    const formattedBody = sender === "grok" ? `_${body}_` : body
    // A2: Multi-line format
    return `${line}\n${formattedBody}`
  }

  // A6: Extract message content type for non-text indicators
  private getMsgContentType(chatItem: T.ChatItem): string | null {
    const content = chatItem.content as any
    if (content?.type === "rcvMsgContent" || content?.type === "sndMsgContent") {
      return content.msgContent?.type ?? null
    }
    return null
  }

  // --- State Tracking Helpers ---

  private initGroupMetadata(groupId: number, customerName: string): GroupMetadata {
    let meta = this.groupMetadata.get(groupId)
    if (!meta) {
      meta = {firstContact: Date.now(), msgCount: 0, customerName}
      this.groupMetadata.set(groupId, meta)
    } else {
      meta.customerName = customerName
    }
    this.onGroupMetadataChanged?.(this.groupMetadata)
    return meta
  }

  private incrementMsgCount(groupId: number): number {
    const meta = this.groupMetadata.get(groupId)
    if (meta) {
      meta.msgCount++
      this.onGroupMetadataChanged?.(this.groupMetadata)
      return meta.msgCount
    }
    return 1
  }

  private updatePendingInfo(groupId: number, eventType: "message" | "reaction", from: SenderType): void {
    const existing = this.groupPendingInfo.get(groupId)
    const info: GroupPendingInfo = {
      lastEventType: eventType,
      lastEventFrom: from,
      lastEventTimestamp: Date.now(),
      lastMessageFrom: eventType === "message" ? from : (existing?.lastMessageFrom ?? from),
    }
    this.groupPendingInfo.set(groupId, info)
    this.onGroupPendingInfoChanged?.(this.groupPendingInfo)
  }

  // --- State Derivation Helpers ---

  private async getGroupComposition(groupId: number): Promise<GroupComposition> {
    const members = await this.mainChat.apiListMembers(groupId)
    return {
      grokMember: members.find(m =>
        this.config.grokContactId !== null
        && m.memberContactId === this.config.grokContactId && isActiveMember(m)),
      teamMember: members.find(m =>
        this.config.teamMembers.some(tm => tm.id === m.memberContactId) && isActiveMember(m)),
    }
  }

  private async isFirstCustomerMessage(groupId: number): Promise<boolean> {
    if (this.welcomeCompleted.has(groupId)) return false
    const chat = await this.apiGetChat(groupId, 20)
    const found = chat.chatItems.some((ci: T.ChatItem) => {
      if (ci.chatDir.type !== "groupSnd") return false
      const text = util.ciContentText(ci)
      return text?.includes("forwarded to the team")
        || text?.includes("now chatting with Grok")
        || text?.includes("team member has been added")
        || text?.includes("team member has already been invited")
    })
    if (found) this.welcomeCompleted.add(groupId)
    return !found
  }

  private async getGrokHistory(groupId: number, grokMember: T.GroupMember, customerId: string): Promise<GrokMessage[]> {
    const chat = await this.apiGetChat(groupId, 100)
    const history: GrokMessage[] = []
    for (const ci of chat.chatItems) {
      if (ci.chatDir.type !== "groupRcv") continue
      const text = util.ciContentText(ci)?.trim()
      if (!text) continue
      if (ci.chatDir.groupMember.groupMemberId === grokMember.groupMemberId) {
        history.push({role: "assistant", content: text})
      } else if (ci.chatDir.groupMember.memberId === customerId && !util.ciBotCommand(ci)) {
        history.push({role: "user", content: text})
      }
    }
    return history
  }

  private async getCustomerMessages(groupId: number, customerId: string): Promise<string[]> {
    const chat = await this.apiGetChat(groupId, 100)
    return chat.chatItems
      .filter((ci: T.ChatItem) =>
        ci.chatDir.type === "groupRcv"
        && ci.chatDir.groupMember.memberId === customerId
        && !util.ciBotCommand(ci))
      .map((ci: T.ChatItem) => util.ciContentText(ci)?.trim())
      .filter((t): t is string => !!t)
  }

  private async hasTeamBeenActivatedBefore(groupId: number): Promise<boolean> {
    const chat = await this.apiGetChat(groupId, 50)
    return chat.chatItems.some((ci: T.ChatItem) =>
      ci.chatDir.type === "groupSnd"
      && util.ciContentText(ci)?.includes("A team member has been added"))
  }

  // Interim apiGetChat wrapper using sendChatCmd directly
  private async apiGetChat(groupId: number, count: number): Promise<T.AChat> {
    const r = await this.mainChat.sendChatCmd(`/_get chat #${groupId} count=${count}`) as any
    if (r.type === "apiChat") return r.chat
    throw new Error(`error getting chat for group ${groupId}: ${r.type}`)
  }

  // --- Event Handlers (main bot) ---

  async onBusinessRequest(evt: CEvt.AcceptingBusinessRequest): Promise<void> {
    const groupId = evt.groupInfo.groupId
    try {
      const profile = evt.groupInfo.groupProfile
      await this.mainChat.apiUpdateGroupProfile(groupId, {
        displayName: profile.displayName,
        fullName: profile.fullName,
        groupPreferences: {
          ...profile.groupPreferences,
          files: {enable: T.GroupFeatureEnabled.On},
        },
      })
      log(`Enabled media uploads for business group ${groupId}`)
    } catch (err) {
      logError(`Failed to enable media uploads for group ${groupId}`, err)
    }
  }

  async onNewChatItems(evt: CEvt.NewChatItems): Promise<void> {
    for (const ci of evt.chatItems) {
      try {
        await this.processChatItem(ci)
      } catch (err) {
        logError(`Error processing chat item in group`, err)
      }
    }
  }

  async onLeftMember(evt: CEvt.LeftMember): Promise<void> {
    const groupId = evt.groupInfo.groupId
    const member = evt.member
    const bc = evt.groupInfo.businessChat
    if (!bc) return

    // Customer left
    if (member.memberId === bc.customerId) {
      log(`Customer left group ${groupId}, cleaning up`)
      this.cleanupGrokMaps(groupId)
      this.welcomeCompleted.delete(groupId)
      if (this.newItems.delete(groupId)) {
        this.onNewItemsChanged?.(this.newItems)
      }
      if (this.groupLastActive.delete(groupId)) {
        this.onGroupLastActiveChanged?.(this.groupLastActive)
      }
      // Clean up new state
      this.lastTeamItemByGroup.delete(groupId)
      this.cleanupForwardedItems(groupId)
      if (this.groupMetadata.delete(groupId)) {
        this.onGroupMetadataChanged?.(this.groupMetadata)
      }
      if (this.groupPendingInfo.delete(groupId)) {
        this.onGroupPendingInfoChanged?.(this.groupPendingInfo)
      }
      return
    }

    // Grok left
    if (this.config.grokContactId !== null && member.memberContactId === this.config.grokContactId) {
      log(`Grok left group ${groupId}`)
      this.cleanupGrokMaps(groupId)
      return
    }

    // Team member left
    if (this.config.teamMembers.some(tm => tm.id === member.memberContactId)) {
      log(`Team member left group ${groupId}`)
    }
  }

  async onChatItemUpdated(evt: CEvt.ChatItemUpdated): Promise<void> {
    const {chatInfo, chatItem} = evt.chatItem
    if (chatInfo.type !== "group") return
    const groupInfo = chatInfo.groupInfo
    if (!groupInfo.businessChat) return
    const groupId = groupInfo.groupId

    if (chatItem.chatDir.type !== "groupRcv") return

    const itemId = chatItem.meta.itemId
    const key = `${groupId}:${itemId}`
    const entry = this.forwardedItems.get(key)
    if (!entry) return

    const text = util.ciContentText(chatItem)?.trim()
    if (!text) return

    // Rebuild the message using new format
    let fwd = this.truncateText(this.formatForwardMessage(entry.header, text, entry.sender, false))
    const newEntry = this.newItems.get(groupId)
    if (newEntry && newEntry.teamItemId === entry.teamItemId) {
      fwd = this.truncateText(this.formatForwardMessage(entry.header, text, entry.sender, true))
      newEntry.originalText = this.truncateText(this.formatForwardMessage(entry.header, text, entry.sender, false))
      this.onNewItemsChanged?.(this.newItems)
    }
    try {
      await this.mainChat.apiUpdateChatItem(
        T.ChatType.Group,
        this.config.teamGroup.id,
        entry.teamItemId,
        {type: "text", text: fwd},
        false,
      )
    } catch (err) {
      logError(`Failed to forward edit to team for group ${groupId}, item ${itemId}`, err)
    }
  }

  // D1: Reaction event handler
  async onChatItemReaction(evt: CEvt.ChatItemReaction): Promise<void> {
    if (!evt.added) return
    const chatInfo = evt.reaction.chatInfo
    if (chatInfo.type !== "group") return
    const groupInfo = (chatInfo as any).groupInfo
    if (!groupInfo?.businessChat) return
    const groupId = groupInfo.groupId

    const reactionDir = evt.reaction.chatReaction.chatDir
    if (reactionDir.type === "groupSnd") return
    if (reactionDir.type !== "groupRcv") return

    const sender = reactionDir.groupMember
    const isCustomer = sender.memberId === groupInfo.businessChat.customerId
    const isGrok = this.config.grokContactId !== null && sender.memberContactId === this.config.grokContactId
    const isTeam = this.config.teamMembers.some(tm => tm.id === sender.memberContactId)

    const from: SenderType | null = isCustomer ? "customer" : isGrok ? "grok" : isTeam ? "team" : null
    if (!from) return

    this.updatePendingInfo(groupId, "reaction", from)
  }

  async onJoinedGroupMember(evt: CEvt.JoinedGroupMember): Promise<void> {
    log(`Member joined group ${evt.groupInfo.groupId}: ${evt.member.memberProfile.displayName}`)
    if (evt.groupInfo.groupId === this.config.teamGroup.id) {
      await this.sendTeamMemberDM(evt.member)
    }
  }

  async onMemberConnected(evt: CEvt.ConnectedToGroupMember): Promise<void> {
    const groupId = evt.groupInfo.groupId
    log(`Member connected in group ${groupId}: ${evt.member.memberProfile.displayName}`)
    if (groupId === this.config.teamGroup.id) {
      await this.sendTeamMemberDM(evt.member, evt.memberContact)
    }
    // Set owner role for team members invited via /add
    const key = `${groupId}:${evt.member.groupMemberId}`
    if (this.pendingOwnerRole.delete(key)) {
      try {
        await this.mainChat.apiSetMembersRole(groupId, [evt.member.groupMemberId], T.GroupMemberRole.Owner)
        log(`Set owner role for member ${evt.member.groupMemberId} in group ${groupId}`)
      } catch (err) {
        logError(`Failed to set owner role for member ${evt.member.groupMemberId} in group ${groupId}`, err)
      }
    }
  }

  async onMemberContactReceivedInv(evt: CEvt.NewMemberContactReceivedInv): Promise<void> {
    const {contact, groupInfo, member} = evt
    if (groupInfo.groupId === this.config.teamGroup.id) {
      log(`Accepted DM contact from team group member: ${contact.contactId}:${member.memberProfile.displayName}`)
      if (!this.pendingTeamDMs.has(contact.contactId)) {
        const name = member.memberProfile.displayName
        const formatted = name.includes(" ") ? `'${name}'` : name
        const msg = `Added you to be able to invite you to customer chats later, keep this contact. Your contact ID is ${contact.contactId}:${formatted}`
        this.pendingTeamDMs.set(contact.contactId, msg)
      }
    } else {
      log(`DM contact received from non-team group ${groupInfo.groupId}, member ${member.memberProfile.displayName}`)
    }
  }

  async onContactConnected(evt: CEvt.ContactConnected): Promise<void> {
    const contactId = evt.contact.contactId
    const pendingMsg = this.pendingTeamDMs.get(contactId)
    if (pendingMsg === undefined) return
    this.pendingTeamDMs.delete(contactId)
    log(`Contact connected, sending pending DM to team member ${contactId}`)
    try {
      await this.mainChat.apiSendTextMessage([T.ChatType.Direct, contactId], pendingMsg)
    } catch (err) {
      logError(`Failed to send DM to new team member ${contactId}`, err)
    }
  }

  private async sendTeamMemberDM(member: T.GroupMember, memberContact?: T.Contact): Promise<void> {
    const name = member.memberProfile.displayName
    const formatted = name.includes(" ") ? `'${name}'` : name

    const contactId = memberContact?.contactId ?? member.memberContactId
    if (contactId) {
      const msg = `Added you to be able to invite you to customer chats later, keep this contact. Your contact ID is ${contactId}:${formatted}`
      try {
        await this.mainChat.apiSendTextMessage([T.ChatType.Direct, contactId], msg)
        log(`Sent DM to team member ${contactId}:${name}`)
      } catch (err) {
        logError(`Failed to send DM to team member ${contactId}`, err)
      }
      return
    }

    const groupId = this.config.teamGroup.id
    try {
      const r = await this.mainChat.sendChatCmd(
        `/_create member contact #${groupId} ${member.groupMemberId}`
      ) as any
      if (r.type !== "newMemberContact") {
        log(`Unexpected response creating member contact: ${r.type}`)
        return
      }
      const newContactId: number = r.contact.contactId
      const msg = `Added you to be able to invite you to customer chats later, keep this contact. Your contact ID is ${newContactId}:${formatted}`
      this.pendingTeamDMs.set(newContactId, msg)
      await this.mainChat.sendChatCmd(`/_invite member contact @${newContactId}`)
      log(`Sent DM invitation to team member ${newContactId}:${name}`)
    } catch (err) {
      logError(`Failed to create member contact for group member ${member.groupMemberId}`, err)
    }
  }

  // --- Event Handler (Grok agent) ---

  async onGrokGroupInvitation(evt: CEvt.ReceivedGroupInvitation): Promise<void> {
    const memberId = evt.groupInfo.membership.memberId
    const mainGroupId = this.pendingGrokJoins.get(memberId)
    if (mainGroupId === undefined) {
      log(`Grok received unexpected group invitation (memberId=${memberId}), ignoring`)
      return
    }
    log(`Grok joining group: mainGroupId=${mainGroupId}, grokGroupId=${evt.groupInfo.groupId}`)
    this.pendingGrokJoins.delete(memberId)
    try {
      await this.grokChat.apiJoinGroup(evt.groupInfo.groupId)
    } catch (err) {
      logError(`Grok failed to join group ${evt.groupInfo.groupId}`, err)
      return
    }

    this.grokGroupMap.set(mainGroupId, evt.groupInfo.groupId)
    this.reverseGrokMap.set(evt.groupInfo.groupId, mainGroupId)
    this.onGrokMapChanged?.(this.grokGroupMap)
  }

  onGrokMemberConnected(evt: CEvt.ConnectedToGroupMember): void {
    const grokGroupId = evt.groupInfo.groupId
    const mainGroupId = this.reverseGrokMap.get(grokGroupId)
    if (mainGroupId === undefined) return
    this.grokFullyConnected.add(mainGroupId)
    const resolver = this.grokJoinResolvers.get(mainGroupId)
    if (resolver) {
      this.grokJoinResolvers.delete(mainGroupId)
      log(`Grok fully connected in group: mainGroupId=${mainGroupId}, grokGroupId=${grokGroupId}`)
      resolver()
    }
  }

  // --- Internal Processing ---

  private async processChatItem(ci: T.AChatItem): Promise<void> {
    const {chatInfo, chatItem} = ci

    // Direct message (not from business group) → reply with business address
    if (chatInfo.type === "direct" && chatItem.chatDir.type === "directRcv") {
      const contactId = (chatInfo as any).contact?.contactId
      if (contactId && this.businessAddress) {
        try {
          await this.mainChat.apiSendTextMessage(
            [T.ChatType.Direct, contactId],
            `I can't answer your questions on non-business address, please add me through my business address: ${this.businessAddress}`,
          )
        } catch (err) {
          logError(`Failed to reply to direct message from contact ${contactId}`, err)
        }
      }
      return
    }

    if (chatInfo.type !== "group") return
    const groupInfo = chatInfo.groupInfo
    const groupId = groupInfo.groupId

    // Handle commands in team group (/add, /inviteall, /invitenew, /pending)
    if (groupId === this.config.teamGroup.id) {
      await this.processTeamGroupMessage(chatItem)
      return
    }

    if (!groupInfo.businessChat) return

    if (chatItem.chatDir.type === "groupSnd") return
    if (chatItem.chatDir.type !== "groupRcv") return
    const sender = chatItem.chatDir.groupMember

    const isCustomer = sender.memberId === groupInfo.businessChat.customerId

    if (!isCustomer) {
      const isGrok = this.config.grokContactId !== null && sender.memberContactId === this.config.grokContactId
      // Team member message → forward to team group
      if (this.config.teamMembers.some(tm => tm.id === sender.memberContactId)) {
        const text = util.ciContentText(chatItem)?.trim()
        if (text) {
          const customerName = groupInfo.groupProfile.displayName || `group-${groupId}`
          const teamMemberName = sender.memberProfile.displayName
          const contactId = sender.memberContactId
          const itemId = chatItem.meta?.itemId

          // Initialize metadata if needed, increment count
          this.initGroupMetadata(groupId, customerName)
          const msgNum = this.incrementMsgCount(groupId)
          const meta = this.groupMetadata.get(groupId)!

          // Get state for header
          const {grokMember, teamMember} = await this.getGroupComposition(groupId)
          const state = grokMember ? "GROK" : teamMember ? "TEAM" : "QUEUE"

          const senderLabel = `${contactId}:${teamMemberName}`
          const header = this.buildHeader(groupId, customerName, state, msgNum, meta.firstContact, "team", senderLabel)
          const teamReplyTo = this.resolveTeamReplyTo(groupId, chatItem)
          await this.forwardToTeam(groupId, header, text, "team", itemId, teamReplyTo)

          // D1: Track team message
          this.updatePendingInfo(groupId, "message", "team")
        }
      }
      // Any non-customer, non-Grok member TEXT message → remove Grok if present
      if (!isGrok && util.ciContentText(chatItem)?.trim()) {
        const {grokMember} = await this.getGroupComposition(groupId)
        if (grokMember) {
          log(`Team member sent message in group ${groupId}, removing Grok`)
          try {
            await this.mainChat.apiRemoveMembers(groupId, [grokMember.groupMemberId])
          } catch {
            // ignore — may have already left
          }
          this.cleanupGrokMaps(groupId)
        }
      }
      return
    }

    // Customer message — get composition for state, then forward + dispatch
    const {grokMember, teamMember} = await this.getGroupComposition(groupId)
    const state = grokMember ? "GROK" : teamMember ? "TEAM" : "QUEUE"

    const cmd = util.ciBotCommand(chatItem)
    const text = util.ciContentText(chatItem)?.trim() || null

    // A6: Detect non-text content type
    const contentType = this.getMsgContentType(chatItem)
    const isNonText = contentType !== null && contentType !== "text"
    const body = isNonText
      ? (text ? `_[${contentType}]_ ${text}` : `_[${contentType}]_`)
      : text

    if (body && !cmd) {
      // Track customer text/content activity
      this.groupLastActive.set(groupId, Date.now())
      this.onGroupLastActiveChanged?.(this.groupLastActive)

      // A4: Initialize and increment metadata
      const customerName = groupInfo.groupProfile.displayName || `group-${groupId}`
      this.initGroupMetadata(groupId, customerName)
      const msgNum = this.incrementMsgCount(groupId)
      const meta = this.groupMetadata.get(groupId)!

      const firstMessage = await this.isFirstCustomerMessage(groupId)
      const header = this.buildHeader(groupId, customerName, state, msgNum, meta.firstContact, "customer")
      const teamReplyTo = this.resolveTeamReplyTo(groupId, chatItem)
      await this.forwardToTeam(groupId, header, body, "customer", chatItem.meta?.itemId, teamReplyTo, firstMessage)
      if (firstMessage) {
        await this.sendToGroup(groupId, teamQueueMessage(this.config.timezone))
        await this.sendAddCommand(groupId, groupInfo)
        this.welcomeCompleted.add(groupId)
      }

      // D1: Track customer message
      this.updatePendingInfo(groupId, "message", "customer")
    }

    // State-specific handling (commands, Grok API, etc.)
    if (grokMember) {
      await this.handleGrokMode(groupId, groupInfo, chatItem, text, grokMember)
    } else if (teamMember) {
      await this.handleTeamMode(groupId, cmd ?? null)
    } else {
      await this.handleNoSpecialMembers(groupId, groupInfo, cmd ?? null)
    }

  }

  // Customer message when a team member is present (teamPending or teamLocked)
  private async handleTeamMode(groupId: number, cmd: {keyword: string} | null): Promise<void> {
    if (cmd?.keyword === "grok") {
      await this.sendToGroup(groupId, teamLockedMessage)
    }
    // /team → ignore (already team). Text → already forwarded above.
  }

  // Customer message when Grok is present
  private async handleGrokMode(
    groupId: number,
    groupInfo: T.GroupInfo,
    chatItem: T.ChatItem,
    text: string | null,
    grokMember: T.GroupMember,
  ): Promise<void> {
    const cmd = util.ciBotCommand(chatItem)

    if (cmd?.keyword === "grok") return // already in grok mode
    if (cmd?.keyword === "team") {
      await this.activateTeam(groupId, grokMember)
      return
    }
    if (!text) return
    // Text already forwarded to team in processChatItem — just send to Grok
    await this.forwardToGrok(groupId, groupInfo, text, grokMember, chatItem.meta?.itemId)
  }

  // Customer message when neither Grok nor team is present (welcome or teamQueue)
  private async handleNoSpecialMembers(
    groupId: number,
    groupInfo: T.GroupInfo,
    cmd: {keyword: string} | null,
  ): Promise<void> {
    const firstMessage = await this.isFirstCustomerMessage(groupId)

    if (firstMessage) {
      if (cmd?.keyword === "grok") {
        await this.activateGrok(groupId, groupInfo)
        return
      }
      if (cmd?.keyword === "team") {
        await this.activateTeam(groupId, undefined)
        return
      }
      return
    }

    // teamQueue state
    if (cmd?.keyword === "grok") {
      await this.activateGrok(groupId, groupInfo)
      return
    }
    if (cmd?.keyword === "team") {
      await this.activateTeam(groupId, undefined)
      return
    }
  }

  // --- Grok Activation ---

  private async activateGrok(groupId: number, groupInfo: T.GroupInfo): Promise<void> {
    await this.removeNewPrefix(groupId)
    if (this.config.grokContactId === null) {
      await this.sendToGroup(groupId, "Grok is temporarily unavailable. Please try again or click /team for a team member.")
      return
    }
    const grokContactId = this.config.grokContactId
    let member: T.GroupMember | undefined
    try {
      member = await this.mainChat.apiAddMember(groupId, grokContactId, T.GroupMemberRole.Member)
    } catch (err) {
      logError(`Failed to invite Grok to group ${groupId}`, err)
      await this.sendToGroup(groupId, "Grok is temporarily unavailable. Please try again or click /team for a team member.")
      return
    }

    this.pendingGrokJoins.set(member.memberId, groupId)
    await this.sendToGroup(groupId, grokActivatedMessage)
    this.welcomeCompleted.add(groupId)

    const joined = await this.waitForGrokJoin(groupId, 30000)
    if (!joined) {
      this.pendingGrokJoins.delete(member.memberId)
      try {
        await this.mainChat.apiRemoveMembers(groupId, [member.groupMemberId])
      } catch {
        // ignore — may have already left
      }
      this.cleanupGrokMaps(groupId)
      await this.sendToGroup(groupId, "Grok is temporarily unavailable. Please try again or click /team for a team member.")
      return
    }

    // Grok joined — call API with accumulated customer messages from chat history
    try {
      const customerId = groupInfo.businessChat!.customerId
      const customerMessages = await this.getCustomerMessages(groupId, customerId)
      const initialUserMsg = customerMessages.join("\n")
      const response = await this.grokApi.chat([], initialUserMsg)

      const grokLocalGId = this.grokGroupMap.get(groupId)
      if (grokLocalGId === undefined) {
        log(`Grok map entry missing after join for group ${groupId}, Grok may have left`)
        await this.sendToGroup(groupId, "Grok is temporarily unavailable. Please try again or click /team for a team member.")
        return
      }
      const replyTo = await this.findLastGrokReceivedItem(grokLocalGId)
      await this.grokSendMessage(grokLocalGId, response, replyTo)

      // Forward Grok response to team group with new format
      const customerName = groupInfo.groupProfile.displayName || `group-${groupId}`
      this.initGroupMetadata(groupId, customerName)
      const msgNum = this.incrementMsgCount(groupId)
      const meta = this.groupMetadata.get(groupId)!
      const header = this.buildHeader(groupId, customerName, "GROK", msgNum, meta.firstContact, "grok")
      const teamReplyTo = this.findLastForwardedTeamItem(groupId)
      await this.forwardToTeam(groupId, header, response, "grok", undefined, teamReplyTo)

      // D1: Track Grok response
      this.updatePendingInfo(groupId, "message", "grok")
    } catch (err) {
      logError(`Grok API/send failed for group ${groupId}`, err)
      try {
        await this.mainChat.apiRemoveMembers(groupId, [member.groupMemberId])
      } catch {
        // ignore
      }
      this.cleanupGrokMaps(groupId)
      await this.sendToGroup(groupId, "Grok is temporarily unavailable. Please try again or click /team for a team member.")
    }
  }

  // --- Grok Message Forwarding ---

  private async forwardToGrok(
    groupId: number,
    groupInfo: T.GroupInfo,
    text: string,
    grokMember: T.GroupMember,
    customerItemId?: number,
  ): Promise<void> {
    try {
      const grokLocalGId = this.grokGroupMap.get(groupId)
      const customerId = groupInfo.businessChat!.customerId
      const history = await this.getGrokHistory(groupId, grokMember, customerId)
      const response = await this.grokApi.chat(history, text)

      if (grokLocalGId !== undefined) {
        const replyTo = await this.findLastGrokReceivedItem(grokLocalGId)
        await this.grokSendMessage(grokLocalGId, response, replyTo)
      }

      // Forward Grok response to team group with new format
      const customerName = groupInfo.groupProfile.displayName || `group-${groupId}`
      this.initGroupMetadata(groupId, customerName)
      const msgNum = this.incrementMsgCount(groupId)
      const meta = this.groupMetadata.get(groupId)!
      const header = this.buildHeader(groupId, customerName, "GROK", msgNum, meta.firstContact, "grok")
      const teamReplyTo = customerItemId !== undefined
        ? this.forwardedItems.get(`${groupId}:${customerItemId}`)?.teamItemId
        : undefined
      await this.forwardToTeam(groupId, header, response, "grok", undefined, teamReplyTo)

      // D1: Track Grok response
      this.updatePendingInfo(groupId, "message", "grok")
    } catch (err) {
      logError(`Grok API error for group ${groupId}`, err)
      try {
        await this.mainChat.apiRemoveMembers(groupId, [grokMember.groupMemberId])
      } catch {
        // ignore — may have already left
      }
      this.cleanupGrokMaps(groupId)
      await this.sendToGroup(groupId, "Grok is temporarily unavailable. Please try again or click /team for a team member.")
    }
  }

  // --- Team Actions ---

  // A1+A2+A3+A4+A5: Forwarding with full formatting and threading
  private async forwardToTeam(
    groupId: number, header: string, body: string, sender: SenderType,
    sourceItemId?: number, inReplyTo?: number,
    isNew: boolean = false,
  ): Promise<void> {
    const cleanMsg = this.truncateText(this.formatForwardMessage(header, body, sender, false))
    const fwd = isNew ? this.truncateText(this.formatForwardMessage(header, body, sender, true)) : cleanMsg

    // A1: Reply-to-last threading — use explicit reply-to if provided, else last team item for this group
    const effectiveReplyTo = inReplyTo ?? this.lastTeamItemByGroup.get(groupId)

    try {
      const result = await this.mainChat.apiSendTextMessage(
        [T.ChatType.Group, this.config.teamGroup.id],
        fwd,
        effectiveReplyTo,
      )
      if (result && result[0]) {
        const teamItemId = result[0].chatItem.meta.itemId

        // A1: Update threading tracker
        this.lastTeamItemByGroup.set(groupId, teamItemId)

        // Edit tracking (only when sourceItemId provided)
        if (sourceItemId !== undefined) {
          this.forwardedItems.set(`${groupId}:${sourceItemId}`, {teamItemId, header, sender})
        }

        // [NEW] marker tracking
        if (isNew) {
          this.newItems.set(groupId, {teamItemId, timestamp: Date.now(), originalText: cleanMsg})
          this.onNewItemsChanged?.(this.newItems)
        }
      }
    } catch (err) {
      logError(`Failed to forward to team for group ${groupId}`, err)
    }
  }

  private async activateTeam(groupId: number, _grokMember: T.GroupMember | undefined): Promise<void> {
    await this.removeNewPrefix(groupId)
    if (await this.hasTeamBeenActivatedBefore(groupId)) {
      await this.sendToGroup(groupId, teamAlreadyAddedMessage)
      this.welcomeCompleted.add(groupId)
      return
    }
    if (this.config.teamMembers.length === 0) {
      logError(`No team members configured, cannot add team member to group ${groupId}`, new Error("no team members"))
      await this.sendToGroup(groupId, "No team members are available yet. Please try again later or click /grok.")
      return
    }
    try {
      const teamContactId = this.config.teamMembers[0].id
      const member = await this.addOrFindTeamMember(groupId, teamContactId)
      if (!member) {
        await this.sendToGroup(groupId, "Sorry, there was an error adding a team member. Please try again.")
        return
      }
      await this.sendToGroup(groupId, teamAddedMessage(this.config.timezone))
      this.welcomeCompleted.add(groupId)
    } catch (err) {
      logError(`Failed to add team member to group ${groupId}`, err)
      await this.sendToGroup(groupId, "Sorry, there was an error adding a team member. Please try again.")
    }
  }

  private async removeNewPrefix(groupId: number): Promise<void> {
    const entry = this.newItems.get(groupId)
    if (!entry) return
    this.newItems.delete(groupId)
    this.onNewItemsChanged?.(this.newItems)

    if (Date.now() - entry.timestamp >= 24 * 60 * 60 * 1000) return

    try {
      await this.mainChat.apiUpdateChatItem(
        T.ChatType.Group, this.config.teamGroup.id, entry.teamItemId,
        {type: "text", text: entry.originalText}, false)
    } catch (err) {
      logError(`Failed to remove [NEW] for group ${groupId}`, err)
    }
  }

  // --- Team Group Commands ---

  private async processTeamGroupMessage(chatItem: T.ChatItem): Promise<void> {
    if (chatItem.chatDir.type !== "groupRcv") return
    const text = util.ciContentText(chatItem)?.trim()
    if (!text) return
    const senderContactId = chatItem.chatDir.groupMember.memberContactId
    if (!senderContactId) return

    const addMatch = text.match(/^\/add\s+(\d+):/)
    if (addMatch) {
      await this.handleAddCommand(parseInt(addMatch[1]), senderContactId)
      return
    }
    if (text === "/inviteall") {
      await this.handleInviteAll(senderContactId)
      return
    }
    if (text === "/invitenew") {
      await this.handleInviteNew(senderContactId)
      return
    }
    // D1: /pending command
    if (text === "/pending") {
      await this.handlePending()
      return
    }
  }

  private async handleAddCommand(targetGroupId: number, senderContactId: number): Promise<void> {
    await this.removeNewPrefix(targetGroupId)

    try {
      const member = await this.addOrFindTeamMember(targetGroupId, senderContactId)
      if (member) {
        log(`Team member ${senderContactId} added to group ${targetGroupId} via /add command`)
        const key = `${targetGroupId}:${member.groupMemberId}`
        this.pendingOwnerRole.add(key)
        try {
          await this.mainChat.apiSetMembersRole(targetGroupId, [member.groupMemberId], T.GroupMemberRole.Owner)
          this.pendingOwnerRole.delete(key)
        } catch {
          // Member not yet connected — will be set in onMemberConnected
        }
      }
    } catch (err) {
      logError(`Failed to add team member to group ${targetGroupId} via /add`, err)
    }
  }

  private async inviteToGroups(
    groupIds: number[], senderContactId: number
  ): Promise<{added: number; alreadyMember: number; failed: number}> {
    let added = 0, alreadyMember = 0, failed = 0
    for (const groupId of groupIds) {
      try {
        const members = await this.mainChat.apiListMembers(groupId)
        if (members.some((m: T.GroupMember) => m.memberContactId === senderContactId)) {
          alreadyMember++
          continue
        }
        await this.removeNewPrefix(groupId)
        const member = await this.addOrFindTeamMember(groupId, senderContactId)
        if (member) {
          const key = `${groupId}:${member.groupMemberId}`
          this.pendingOwnerRole.add(key)
          try {
            await this.mainChat.apiSetMembersRole(groupId, [member.groupMemberId], T.GroupMemberRole.Owner)
            this.pendingOwnerRole.delete(key)
          } catch {
            // Member not yet connected — will be set in onMemberConnected
          }
          added++
        } else {
          failed++
        }
      } catch (err) {
        logError(`Failed to invite to group ${groupId}`, err)
        failed++
      }
    }
    return {added, alreadyMember, failed}
  }

  private async handleInviteAll(senderContactId: number): Promise<void> {
    const now = Date.now()
    const DAY_MS = 24 * 60 * 60 * 1000
    const groupIds: number[] = []
    for (const [groupId, timestamp] of this.groupLastActive) {
      if (now - timestamp < DAY_MS) {
        groupIds.push(groupId)
      }
    }
    const result = await this.inviteToGroups(groupIds, senderContactId)
    const summary = `Invited to ${result.added} group(s), already member in ${result.alreadyMember}, failed ${result.failed} (of ${groupIds.length} active in 24h)`
    log(`/inviteall: ${summary}`)
    await this.sendToGroup(this.config.teamGroup.id, summary)
  }

  private async handleInviteNew(senderContactId: number): Promise<void> {
    const now = Date.now()
    const TWO_DAYS_MS = 48 * 60 * 60 * 1000
    const candidateIds: number[] = []
    for (const [groupId, timestamp] of this.groupLastActive) {
      if (now - timestamp < TWO_DAYS_MS) {
        candidateIds.push(groupId)
      }
    }
    const groupIds: number[] = []
    for (const groupId of candidateIds) {
      const {grokMember, teamMember} = await this.getGroupComposition(groupId)
      if (!grokMember && !teamMember) {
        groupIds.push(groupId)
      }
    }
    const result = await this.inviteToGroups(groupIds, senderContactId)
    const summary = `Invited to ${result.added} group(s), already member in ${result.alreadyMember}, failed ${result.failed} (of ${candidateIds.length} active in 48h, ${groupIds.length} without team/Grok)`
    log(`/invitenew: ${summary}`)
    await this.sendToGroup(this.config.teamGroup.id, summary)
  }

  // D1: /pending command handler
  private async handlePending(): Promise<void> {
    const pending: {groupId: number; customerName: string; state: string; msgCount: number; firstContact: number}[] = []

    for (const [groupId, _lastActive] of this.groupLastActive) {
      const info = this.groupPendingInfo.get(groupId)
      const meta = this.groupMetadata.get(groupId)

      // If no pending info tracked (e.g., after restart), assume pending
      if (!info) {
        const {grokMember, teamMember} = await this.getGroupComposition(groupId)
        const state = grokMember ? "GROK" : teamMember ? "TEAM" : "QUEUE"
        pending.push({
          groupId,
          customerName: meta?.customerName ?? `group-${groupId}`,
          state,
          msgCount: meta?.msgCount ?? 0,
          firstContact: meta?.firstContact ?? _lastActive,
        })
        continue
      }

      // Not pending if last event is from team or grok
      if (info.lastEventFrom === "team" || info.lastEventFrom === "grok") continue

      // Not pending if last event is customer reaction but last message is not from customer
      if (info.lastEventType === "reaction" && info.lastEventFrom === "customer" && info.lastMessageFrom !== "customer") continue

      // It's pending
      const {grokMember, teamMember} = await this.getGroupComposition(groupId)
      const state = grokMember ? "GROK" : teamMember ? "TEAM" : "QUEUE"
      pending.push({
        groupId,
        customerName: meta?.customerName ?? `group-${groupId}`,
        state,
        msgCount: meta?.msgCount ?? 0,
        firstContact: meta?.firstContact ?? _lastActive,
      })
    }

    if (pending.length === 0) {
      await this.sendToGroup(this.config.teamGroup.id, "No pending conversations.")
      return
    }

    // Sort by firstContact ascending (longest waiting first)
    const now = Date.now()
    pending.sort((a, b) => a.firstContact - b.firstContact)

    let msg = `*Pending (${pending.length}):*`
    for (const p of pending) {
      const duration = this.formatDuration(now - p.firstContact)
      msg += `\n${p.groupId}:${p.customerName} · ${p.state} · #${p.msgCount} · ${duration}`
    }

    await this.sendToGroup(this.config.teamGroup.id, msg)
  }

  private async sendAddCommand(groupId: number, groupInfo: T.GroupInfo): Promise<void> {
    const name = groupInfo.groupProfile.displayName || `group-${groupId}`
    const formatted = name.includes(" ") ? `'${name}'` : name
    const cmd = `/add ${groupId}:${formatted}`
    await this.sendToGroup(this.config.teamGroup.id, cmd)
  }

  // --- Helpers ---

  private async addOrFindTeamMember(groupId: number, teamContactId: number): Promise<T.GroupMember | null> {
    try {
      return await this.mainChat.apiAddMember(groupId, teamContactId, T.GroupMemberRole.Owner)
    } catch (err: any) {
      if (err?.chatError?.errorType?.type === "groupDuplicateMember") {
        log(`Team member already in group ${groupId}, looking up existing member`)
        const members = await this.mainChat.apiListMembers(groupId)
        const existing = members.find(m => m.memberContactId === teamContactId)
        if (existing) {
          log(`Found existing team member: groupMemberId=${existing.groupMemberId}`)
          return existing
        }
        logError(`Team member contact ${teamContactId} reported as duplicate but not found in group ${groupId}`, err)
        return null
      }
      throw err
    }
  }

  private async sendToGroup(groupId: number, text: string): Promise<void> {
    try {
      await this.mainChat.apiSendTextMessage([T.ChatType.Group, groupId], text)
    } catch (err) {
      logError(`Failed to send message to group ${groupId}`, err)
    }
  }

  private waitForGrokJoin(groupId: number, timeout: number): Promise<boolean> {
    if (this.grokFullyConnected.has(groupId)) return Promise.resolve(true)
    return new Promise<boolean>((resolve) => {
      const timer = setTimeout(() => {
        this.grokJoinResolvers.delete(groupId)
        resolve(false)
      }, timeout)
      this.grokJoinResolvers.set(groupId, () => {
        clearTimeout(timer)
        resolve(true)
      })
    })
  }

  private async grokSendMessage(grokLocalGId: number, text: string, replyTo?: number): Promise<void> {
    const safeText = this.truncateText(text)
    try {
      await this.grokChat.apiSendTextMessage([T.ChatType.Group, grokLocalGId], safeText, replyTo)
    } catch (err: any) {
      if (replyTo !== undefined && err?.chatError?.type === "errorStore" && err?.chatError?.storeError?.type === "invalidQuote") {
        log(`Invalid quote in Grok group ${grokLocalGId}, retrying without reply`)
        await this.grokChat.apiSendTextMessage([T.ChatType.Group, grokLocalGId], safeText)
      } else {
        throw err
      }
    }
  }

  private async findLastGrokReceivedItem(grokLocalGId: number): Promise<number | undefined> {
    try {
      const r = await this.grokChat.sendChatCmd(`/_get chat #${grokLocalGId} count=20`) as any
      if (r.type !== "apiChat") return undefined
      const items = r.chat.chatItems
      for (let i = items.length - 1; i >= 0; i--) {
        if (items[i].chatDir.type !== "groupSnd") {
          return items[i].meta?.itemId
        }
      }
      return undefined
    } catch {
      return undefined
    }
  }

  private resolveTeamReplyTo(groupId: number, chatItem: T.ChatItem): number | undefined {
    const quotedItemId = (chatItem as any).quotedItem?.itemId
    if (quotedItemId === undefined) return undefined
    return this.forwardedItems.get(`${groupId}:${quotedItemId}`)?.teamItemId
  }

  private findLastForwardedTeamItem(groupId: number): number | undefined {
    return this.lastTeamItemByGroup.get(groupId)
  }

  private cleanupForwardedItems(groupId: number): void {
    const prefix = `${groupId}:`
    for (const key of this.forwardedItems.keys()) {
      if (key.startsWith(prefix)) this.forwardedItems.delete(key)
    }
  }

  private truncateText(text: string, maxBytes: number = MAX_MSG_TEXT_BYTES): string {
    const encoder = new TextEncoder()
    if (encoder.encode(text).length <= maxBytes) return text
    const suffix = "… [truncated]"
    const target = maxBytes - encoder.encode(suffix).length
    const decoder = new TextDecoder("utf-8", {fatal: false})
    const truncated = decoder.decode(encoder.encode(text).slice(0, target)).replace(/\uFFFD$/, "")
    return truncated + suffix
  }

  private cleanupGrokMaps(groupId: number): void {
    const grokLocalGId = this.grokGroupMap.get(groupId)
    this.grokFullyConnected.delete(groupId)
    if (grokLocalGId === undefined) return
    this.grokGroupMap.delete(groupId)
    this.reverseGrokMap.delete(grokLocalGId)
    this.onGrokMapChanged?.(this.grokGroupMap)
  }
}
