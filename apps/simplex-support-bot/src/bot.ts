import {api, util} from "simplex-chat"
import {T, CEvt} from "@simplex-chat/types"
import {Config} from "./config.js"
import {GrokMessage} from "./state.js"
import {GrokApiClient} from "./grok.js"
import {teamQueueMessage, grokActivatedMessage, teamAddedMessage, teamLockedMessage} from "./messages.js"
import {log, logError} from "./util.js"

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

  // Forwarded message tracking: "groupId:itemId" → {teamItemId, prefix}
  private forwardedItems = new Map<string, {teamItemId: number; prefix: string}>()

  // Callback to persist grokGroupMap changes
  onGrokMapChanged: ((map: ReadonlyMap<number, number>) => void) | null = null

  constructor(
    private mainChat: api.ChatApi,
    private grokChat: api.ChatApi,
    private grokApi: GrokApiClient,
    private config: Config,
  ) {}

  // Restore grokGroupMap from persisted state (call after construction, before events)
  restoreGrokGroupMap(entries: [number, number][]): void {
    for (const [mainGroupId, grokLocalGroupId] of entries) {
      this.grokGroupMap.set(mainGroupId, grokLocalGroupId)
      this.reverseGrokMap.set(grokLocalGroupId, mainGroupId)
    }
    log(`Restored Grok group map: ${entries.length} entries`)
  }

  // --- State Derivation Helpers ---

  private async getGroupComposition(groupId: number): Promise<GroupComposition> {
    const members = await this.mainChat.apiListMembers(groupId)
    return {
      grokMember: members.find(m =>
        m.memberContactId === this.config.grokContactId && isActiveMember(m)),
      teamMember: members.find(m =>
        this.config.teamMembers.some(tm => tm.id === m.memberContactId) && isActiveMember(m)),
    }
  }

  private async isFirstCustomerMessage(groupId: number): Promise<boolean> {
    const chat = await this.apiGetChat(groupId, 20)
    // The platform sends auto-messages on connect (welcome, commands, etc.) as groupSnd.
    // The bot's teamQueueMessage (sent after first customer message) uniquely contains
    // "forwarded to the team" — none of the platform auto-messages do.
    return !chat.chatItems.some((ci: T.ChatItem) =>
      ci.chatDir.type === "groupSnd"
      && util.ciContentText(ci)?.includes("forwarded to the team"))
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
      } else if (ci.chatDir.groupMember.memberId === customerId) {
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

  private async hasTeamMemberSentMessage(groupId: number, teamMember: T.GroupMember): Promise<boolean> {
    const chat = await this.apiGetChat(groupId, 50)
    return chat.chatItems.some((ci: T.ChatItem) =>
      ci.chatDir.type === "groupRcv"
      && ci.chatDir.groupMember.groupMemberId === teamMember.groupMemberId)
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
      return
    }

    // Grok left
    if (member.memberContactId === this.config.grokContactId) {
      log(`Grok left group ${groupId}`)
      this.cleanupGrokMaps(groupId)
      return
    }

    // Team member left — check if they had engaged (teamLocked vs teamPending)
    if (this.config.teamMembers.some(tm => tm.id === member.memberContactId)) {
      const engaged = await this.hasTeamMemberSentMessage(groupId, member)
      if (engaged) {
        log(`Engaged team member left group ${groupId}, adding replacement`)
        await this.addReplacementTeamMember(groupId)
      } else {
        log(`Pending team member left group ${groupId}, reverting to queue`)
        // No state to revert — member is already gone from DB
      }
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

    const fwd = `${entry.prefix}${text}`
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

  onMemberConnected(evt: CEvt.ConnectedToGroupMember): void {
    log(`Member connected in group ${evt.groupInfo.groupId}: ${evt.member.memberProfile.displayName}`)
  }

  onMemberContactReceivedInv(evt: CEvt.NewMemberContactReceivedInv): void {
    const {contact, groupInfo, member} = evt
    if (groupInfo.groupId === this.config.teamGroup.id) {
      log(`Accepted DM contact from team group member: ${contact.contactId}:${member.memberProfile.displayName}`)
    } else {
      log(`DM contact received from non-team group ${groupInfo.groupId}, member ${member.memberProfile.displayName}`)
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

    // Join request sent — set maps, but don't resolve waiter yet.
    this.grokGroupMap.set(mainGroupId, evt.groupInfo.groupId)
    this.reverseGrokMap.set(evt.groupInfo.groupId, mainGroupId)
    this.onGrokMapChanged?.(this.grokGroupMap)
  }

  onGrokMemberConnected(evt: CEvt.ConnectedToGroupMember): void {
    const grokGroupId = evt.groupInfo.groupId
    const mainGroupId = this.reverseGrokMap.get(grokGroupId)
    if (mainGroupId === undefined) return
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
    if (chatInfo.type !== "group") return
    const groupInfo = chatInfo.groupInfo
    const groupId = groupInfo.groupId

    // Handle /add command in team group
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
      // Team member message → forward to team group
      if (this.config.teamMembers.some(tm => tm.id === sender.memberContactId)) {
        const text = util.ciContentText(chatItem)?.trim()
        if (text) {
          const customerName = groupInfo.groupProfile.displayName || `group-${groupId}`
          const teamMemberName = sender.memberProfile.displayName
          const contactId = sender.memberContactId
          const itemId = chatItem.meta?.itemId
          const prefix = `${teamMemberName}:${contactId} > ${customerName}:${groupId}: `
          await this.forwardToTeam(groupId, prefix, text, itemId)
        }
      }
      return
    }

    // Customer message — derive state from group composition
    const {grokMember, teamMember} = await this.getGroupComposition(groupId)

    if (teamMember) {
      await this.handleTeamMode(groupId, chatItem)
    } else if (grokMember) {
      await this.handleGrokMode(groupId, groupInfo, chatItem, grokMember)
    } else {
      await this.handleNoSpecialMembers(groupId, groupInfo, chatItem)
    }
  }

  // Customer message when a team member is present (teamPending or teamLocked)
  private async handleTeamMode(groupId: number, chatItem: T.ChatItem): Promise<void> {
    const cmd = util.ciBotCommand(chatItem)
    if (cmd?.keyword === "grok") {
      await this.sendToGroup(groupId, teamLockedMessage)
    }
    // /team → ignore (already team). Other text → no forwarding (team sees directly).
  }

  // Customer message when Grok is present
  private async handleGrokMode(
    groupId: number,
    groupInfo: T.GroupInfo,
    chatItem: T.ChatItem,
    grokMember: T.GroupMember,
  ): Promise<void> {
    const cmd = util.ciBotCommand(chatItem)
    const text = util.ciContentText(chatItem)?.trim() || null

    if (cmd?.keyword === "grok") return // already in grok mode
    if (cmd?.keyword === "team") {
      await this.activateTeam(groupId, grokMember)
      return
    }
    if (!text) return
    const prefix = this.customerForwardPrefix(groupId, groupInfo)
    await this.forwardToTeam(groupId, prefix, text, chatItem.meta?.itemId)
    await this.forwardToGrok(groupId, groupInfo, text, grokMember)
  }

  // Customer message when neither Grok nor team is present (welcome or teamQueue)
  private async handleNoSpecialMembers(
    groupId: number,
    groupInfo: T.GroupInfo,
    chatItem: T.ChatItem,
  ): Promise<void> {
    const cmd = util.ciBotCommand(chatItem)
    const text = util.ciContentText(chatItem)?.trim() || null

    // Check if this is the first customer message (welcome state)
    const firstMessage = await this.isFirstCustomerMessage(groupId)

    if (firstMessage) {
      // Welcome state — first message transitions to teamQueue
      if (!text) return
      const prefix = this.customerForwardPrefix(groupId, groupInfo)
      await this.forwardToTeam(groupId, prefix, text, chatItem.meta?.itemId)
      await this.sendToGroup(groupId, teamQueueMessage(this.config.timezone))
      await this.sendAddCommand(groupId, groupInfo)
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
    if (!text) return
    const prefix = this.customerForwardPrefix(groupId, groupInfo)
    await this.forwardToTeam(groupId, prefix, text, chatItem.meta?.itemId)
  }

  // --- Grok Activation ---

  private async activateGrok(groupId: number, groupInfo: T.GroupInfo): Promise<void> {
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

    // Wait for Grok agent to join the group
    const joined = await this.waitForGrokJoin(groupId, 30000)
    if (!joined) {
      this.pendingGrokJoins.delete(member.memberId)
      await this.sendToGroup(groupId, "Grok is temporarily unavailable. Please try again or click /team for a team member.")
      return
    }

    // Verify group composition hasn't changed while awaiting (e.g., user sent /team concurrently)
    const {teamMember} = await this.getGroupComposition(groupId)
    if (teamMember) {
      log(`Team member appeared during Grok activation for group ${groupId}, aborting`)
      try {
        await this.mainChat.apiRemoveMembers(groupId, [member.groupMemberId])
      } catch {
        // ignore
      }
      this.cleanupGrokMaps(groupId)
      return
    }

    // Grok joined — call API with accumulated customer messages from chat history
    try {
      const customerId = groupInfo.businessChat!.customerId
      const customerMessages = await this.getCustomerMessages(groupId, customerId)
      const initialUserMsg = customerMessages.join("\n")
      const response = await this.grokApi.chat([], initialUserMsg)

      // Re-check composition after async API call
      const postApi = await this.getGroupComposition(groupId)
      if (postApi.teamMember) {
        log(`Team member appeared during Grok API call for group ${groupId}, aborting`)
        try {
          await this.mainChat.apiRemoveMembers(groupId, [member.groupMemberId])
        } catch {
          // ignore
        }
        this.cleanupGrokMaps(groupId)
        return
      }

      const grokLocalGId = this.grokGroupMap.get(groupId)
      if (grokLocalGId === undefined) {
        log(`Grok map entry missing after join for group ${groupId}`)
        return
      }
      await this.grokChat.apiSendTextMessage([T.ChatType.Group, grokLocalGId], response)
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
  ): Promise<void> {
    try {
      const customerId = groupInfo.businessChat!.customerId
      const history = await this.getGrokHistory(groupId, grokMember, customerId)
      const response = await this.grokApi.chat(history, text)

      const grokLocalGId = this.grokGroupMap.get(groupId)
      if (grokLocalGId !== undefined) {
        await this.grokChat.apiSendTextMessage([T.ChatType.Group, grokLocalGId], response)
      }
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

  private async forwardToTeam(groupId: number, prefix: string, text: string, sourceItemId?: number): Promise<void> {
    const fwd = `${prefix}${text}`
    try {
      const result = await this.mainChat.apiSendTextMessage(
        [T.ChatType.Group, this.config.teamGroup.id],
        fwd,
      )
      if (sourceItemId !== undefined && result && result[0]) {
        const teamItemId = result[0].chatItem.meta.itemId
        this.forwardedItems.set(`${groupId}:${sourceItemId}`, {teamItemId, prefix})
      }
    } catch (err) {
      logError(`Failed to forward to team for group ${groupId}`, err)
    }
  }

  private async activateTeam(groupId: number, grokMember: T.GroupMember | undefined): Promise<void> {
    // Remove Grok immediately if present
    if (grokMember) {
      try {
        await this.mainChat.apiRemoveMembers(groupId, [grokMember.groupMemberId])
      } catch {
        // ignore — may have already left
      }
      this.cleanupGrokMaps(groupId)
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
    } catch (err) {
      logError(`Failed to add team member to group ${groupId}`, err)
      await this.sendToGroup(groupId, "Sorry, there was an error adding a team member. Please try again.")
    }
  }

  private customerForwardPrefix(groupId: number, groupInfo: T.GroupInfo): string {
    const name = groupInfo.groupProfile.displayName || `group-${groupId}`
    return `${name}:${groupId}: `
  }

  // --- Team Group Commands ---

  private async processTeamGroupMessage(chatItem: T.ChatItem): Promise<void> {
    if (chatItem.chatDir.type !== "groupRcv") return
    const text = util.ciContentText(chatItem)?.trim()
    if (!text) return
    const match = text.match(/^\/add\s+(\d+):/)
    if (!match) return

    const targetGroupId = parseInt(match[1])
    const senderContactId = chatItem.chatDir.groupMember.memberContactId
    if (!senderContactId) return

    try {
      await this.addOrFindTeamMember(targetGroupId, senderContactId)
      log(`Team member ${senderContactId} added to group ${targetGroupId} via /add command`)
    } catch (err) {
      logError(`Failed to add team member to group ${targetGroupId} via /add`, err)
    }
  }

  private async sendAddCommand(groupId: number, groupInfo: T.GroupInfo): Promise<void> {
    const name = groupInfo.groupProfile.displayName || `group-${groupId}`
    const formatted = name.includes(" ") ? `'${name}'` : name
    const cmd = `/add ${groupId}:${formatted}`
    await this.sendToGroup(this.config.teamGroup.id, cmd)
  }

  // --- Helpers ---

  private async addReplacementTeamMember(groupId: number): Promise<void> {
    if (this.config.teamMembers.length === 0) return
    try {
      const teamContactId = this.config.teamMembers[0].id
      await this.addOrFindTeamMember(groupId, teamContactId)
    } catch (err) {
      logError(`Failed to add replacement team member to group ${groupId}`, err)
    }
  }

  private async addOrFindTeamMember(groupId: number, teamContactId: number): Promise<T.GroupMember | null> {
    try {
      return await this.mainChat.apiAddMember(groupId, teamContactId, T.GroupMemberRole.Member)
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
    if (this.grokGroupMap.has(groupId)) return Promise.resolve(true)
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

  private cleanupGrokMaps(groupId: number): void {
    const grokLocalGId = this.grokGroupMap.get(groupId)
    if (grokLocalGId === undefined) return
    this.grokGroupMap.delete(groupId)
    this.reverseGrokMap.delete(grokLocalGId)
    this.onGrokMapChanged?.(this.grokGroupMap)
  }
}
