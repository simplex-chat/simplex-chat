import {api, util} from "simplex-chat"
import {T, CEvt} from "@simplex-chat/types"
import {Config} from "./config.js"
import {ConversationState, GrokMessage} from "./state.js"
import {GrokApiClient} from "./grok.js"
import {teamQueueMessage, grokActivatedMessage, teamAddedMessage, teamLockedMessage} from "./messages.js"
import {log, logError} from "./util.js"

export class SupportBot {
  private conversations = new Map<number, ConversationState>()
  private pendingGrokJoins = new Map<string, number>()      // memberId → mainGroupId
  private grokGroupMap = new Map<number, number>()           // mainGroupId → grokLocalGroupId
  private reverseGrokMap = new Map<number, number>()         // grokLocalGroupId → mainGroupId
  private grokJoinResolvers = new Map<number, () => void>()  // mainGroupId → resolve fn

  constructor(
    private mainChat: api.ChatApi,
    private grokChat: api.ChatApi,
    private grokApi: GrokApiClient,
    private config: Config,
  ) {}

  // --- Event Handlers (main bot) ---

  onBusinessRequest(evt: CEvt.AcceptingBusinessRequest): void {
    const groupId = evt.groupInfo.groupId
    log(`New business request: groupId=${groupId}`)
    this.conversations.set(groupId, {type: "welcome"})
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
    const state = this.conversations.get(groupId)
    if (!state) return

    const member = evt.member
    const bc = evt.groupInfo.businessChat
    if (!bc) return

    // Customer left
    if (member.memberId === bc.customerId) {
      log(`Customer left group ${groupId}, cleaning up`)
      this.conversations.delete(groupId)
      this.cleanupGrokMaps(groupId)
      return
    }

    // Team member left — teamPending: gate not yet triggered, revert to teamQueue
    if (state.type === "teamPending" && member.groupMemberId === state.teamMemberGId) {
      log(`Team member left group ${groupId} (teamPending), reverting to teamQueue`)
      this.conversations.set(groupId, {type: "teamQueue", userMessages: []})
      return
    }

    // Team member left — teamLocked: one-way gate triggered, stay in team mode (add another member)
    if (state.type === "teamLocked" && member.groupMemberId === state.teamMemberGId) {
      log(`Team member left group ${groupId} (teamLocked), adding replacement team member`)
      await this.addReplacementTeamMember(groupId)
      return
    }

    // Grok left during grokMode
    if (state.type === "grokMode" && member.groupMemberId === state.grokMemberGId) {
      log(`Grok left group ${groupId} during grokMode, reverting to teamQueue`)
      this.conversations.set(groupId, {type: "teamQueue", userMessages: []})
      this.cleanupGrokMaps(groupId)
      return
    }
  }

  onDeletedMemberUser(evt: CEvt.DeletedMemberUser): void {
    const groupId = evt.groupInfo.groupId
    log(`Bot removed from group ${groupId}`)
    this.conversations.delete(groupId)
    this.cleanupGrokMaps(groupId)
  }

  onGroupDeleted(evt: CEvt.GroupDeleted): void {
    const groupId = evt.groupInfo.groupId
    log(`Group ${groupId} deleted`)
    this.conversations.delete(groupId)
    this.cleanupGrokMaps(groupId)
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
    // The waiter resolves when grokChat fires connectedToGroupMember (see onGrokMemberConnected).
    this.grokGroupMap.set(mainGroupId, evt.groupInfo.groupId)
    this.reverseGrokMap.set(evt.groupInfo.groupId, mainGroupId)
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
    if (!groupInfo.businessChat) return
    const groupId = groupInfo.groupId
    let state = this.conversations.get(groupId)
    if (!state) {
      // After restart, re-initialize state for existing business chats
      state = {type: "teamQueue", userMessages: []}
      this.conversations.set(groupId, state)
      log(`Re-initialized conversation state for group ${groupId} after restart`)
    }

    if (chatItem.chatDir.type === "groupSnd") return
    if (chatItem.chatDir.type !== "groupRcv") return
    const sender = chatItem.chatDir.groupMember

    const isCustomer = sender.memberId === groupInfo.businessChat.customerId
    const isTeamMember = (state.type === "teamPending" || state.type === "teamLocked")
      && sender.groupMemberId === state.teamMemberGId
    const isGrok = state.type === "grokMode"
      && state.grokMemberGId === sender.groupMemberId

    if (isGrok) return
    if (isCustomer) await this.onCustomerMessage(groupId, groupInfo, chatItem, state)
    else if (isTeamMember) await this.onTeamMemberMessage(groupId, state)
  }

  private async onCustomerMessage(
    groupId: number,
    groupInfo: T.GroupInfo,
    chatItem: T.ChatItem,
    state: ConversationState,
  ): Promise<void> {
    const cmd = util.ciBotCommand(chatItem)
    const text = util.ciContentText(chatItem)?.trim() || null

    switch (state.type) {
      case "welcome": {
        if (!text) return
        await this.forwardToTeam(groupId, groupInfo, text)
        await this.sendToGroup(groupId, teamQueueMessage(this.config.timezone))
        this.conversations.set(groupId, {type: "teamQueue", userMessages: [text]})
        break
      }

      case "teamQueue": {
        if (cmd?.keyword === "grok") {
          await this.activateGrok(groupId, state)
          return
        }
        if (cmd?.keyword === "team") {
          await this.activateTeam(groupId, state)
          return
        }
        if (!text) return
        await this.forwardToTeam(groupId, groupInfo, text)
        state.userMessages.push(text)
        break
      }

      case "grokMode": {
        if (cmd?.keyword === "grok") return
        if (cmd?.keyword === "team") {
          await this.activateTeam(groupId, state)
          return
        }
        if (!text) return
        await this.forwardToTeam(groupId, groupInfo, text)
        await this.forwardToGrok(groupId, text, state)
        break
      }

      case "teamPending": {
        if (cmd?.keyword === "grok") {
          await this.sendToGroup(groupId, teamLockedMessage)
          return
        }
        // /team → ignore (already team). Other text → no forwarding (team sees directly).
        break
      }

      case "teamLocked": {
        if (cmd?.keyword === "grok") {
          await this.sendToGroup(groupId, teamLockedMessage)
          return
        }
        // No action — team sees directly
        break
      }
    }
  }

  private async onTeamMemberMessage(groupId: number, state: ConversationState): Promise<void> {
    if (state.type !== "teamPending") return
    log(`Team member engaged in group ${groupId}, locking to teamLocked`)
    this.conversations.set(groupId, {type: "teamLocked", teamMemberGId: state.teamMemberGId})
  }

  // --- Grok Activation ---

  private async activateGrok(
    groupId: number,
    state: {type: "teamQueue"; userMessages: string[]},
  ): Promise<void> {
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

    // Verify state hasn't changed while awaiting (e.g., user sent /team concurrently)
    const currentState = this.conversations.get(groupId)
    if (!currentState || currentState.type !== "teamQueue") {
      log(`State changed during Grok activation for group ${groupId} (now ${currentState?.type}), aborting`)
      try {
        await this.mainChat.apiRemoveMembers(groupId, [member.groupMemberId])
      } catch {
        // ignore
      }
      this.cleanupGrokMaps(groupId)
      return
    }

    // Grok joined — call API with accumulated messages
    try {
      const initialUserMsg = state.userMessages.join("\n")
      const response = await this.grokApi.chat([], initialUserMsg)

      // Re-check state after async API call — another event may have changed it
      const postApiState = this.conversations.get(groupId)
      if (!postApiState || postApiState.type !== "teamQueue") {
        log(`State changed during Grok API call for group ${groupId} (now ${postApiState?.type}), aborting`)
        try {
          await this.mainChat.apiRemoveMembers(groupId, [member.groupMemberId])
        } catch {
          // ignore
        }
        this.cleanupGrokMaps(groupId)
        return
      }

      const history: GrokMessage[] = [
        {role: "user", content: initialUserMsg},
        {role: "assistant", content: response},
      ]

      const grokLocalGId = this.grokGroupMap.get(groupId)
      if (grokLocalGId === undefined) {
        log(`Grok map entry missing after join for group ${groupId}`)
        return
      }
      await this.grokChat.apiSendTextMessage([T.ChatType.Group, grokLocalGId], response)

      this.conversations.set(groupId, {
        type: "grokMode",
        grokMemberGId: member.groupMemberId,
        history,
      })
    } catch (err) {
      logError(`Grok API/send failed for group ${groupId}`, err)
      // Remove Grok since activation failed after join
      try {
        await this.mainChat.apiRemoveMembers(groupId, [member.groupMemberId])
      } catch {
        // ignore
      }
      this.cleanupGrokMaps(groupId)
      await this.sendToGroup(groupId, "Grok is temporarily unavailable. Please try again or click /team for a team member.")
      // Stay in teamQueue
    }
  }

  // --- Grok Message Forwarding ---

  private async forwardToGrok(
    groupId: number,
    text: string,
    state: {type: "grokMode"; grokMemberGId: number; history: GrokMessage[]},
  ): Promise<void> {
    try {
      const response = await this.grokApi.chat(state.history, text)
      state.history.push({role: "user", content: text})
      state.history.push({role: "assistant", content: response})

      const grokLocalGId = this.grokGroupMap.get(groupId)
      if (grokLocalGId !== undefined) {
        await this.grokChat.apiSendTextMessage([T.ChatType.Group, grokLocalGId], response)
      }
    } catch (err) {
      logError(`Grok API error for group ${groupId}`, err)
      // Per plan: revert to teamQueue on Grok API failure — remove Grok, clean up
      try {
        await this.mainChat.apiRemoveMembers(groupId, [state.grokMemberGId])
      } catch {
        // ignore — may have already left
      }
      this.cleanupGrokMaps(groupId)
      this.conversations.set(groupId, {type: "teamQueue", userMessages: []})
      await this.sendToGroup(groupId, "Grok is temporarily unavailable. Please try again or click /team for a team member.")
    }
  }

  // --- Team Actions ---

  private async forwardToTeam(groupId: number, groupInfo: T.GroupInfo, text: string): Promise<void> {
    const name = groupInfo.groupProfile.displayName || `group-${groupId}`
    const fwd = `[${name} #${groupId}]\n${text}`
    try {
      await this.mainChat.apiSendTextMessage(
        [T.ChatType.Group, this.config.teamGroup.id],
        fwd,
      )
    } catch (err) {
      logError(`Failed to forward to team for group ${groupId}`, err)
    }
  }

  private async activateTeam(groupId: number, state: ConversationState): Promise<void> {
    // Remove Grok immediately if present (per spec: "When switching to team mode, Grok is removed")
    const wasGrokMode = state.type === "grokMode"
    if (wasGrokMode) {
      try {
        await this.mainChat.apiRemoveMembers(groupId, [state.grokMemberGId])
      } catch {
        // ignore — may have already left
      }
      this.cleanupGrokMaps(groupId)
    }
    if (this.config.teamMembers.length === 0) {
      logError(`No team members configured, cannot add team member to group ${groupId}`, new Error("no team members"))
      if (wasGrokMode) {
        this.conversations.set(groupId, {type: "teamQueue", userMessages: []})
      }
      await this.sendToGroup(groupId, "No team members are available yet. Please try again later or click /grok.")
      return
    }
    try {
      const teamContactId = this.config.teamMembers[0].id
      const member = await this.addOrFindTeamMember(groupId, teamContactId)
      if (!member) {
        if (wasGrokMode) {
          this.conversations.set(groupId, {type: "teamQueue", userMessages: []})
        }
        await this.sendToGroup(groupId, "Sorry, there was an error adding a team member. Please try again.")
        return
      }
      this.conversations.set(groupId, {
        type: "teamPending",
        teamMemberGId: member.groupMemberId,
      })
      await this.sendToGroup(groupId, teamAddedMessage(this.config.timezone))
    } catch (err) {
      logError(`Failed to add team member to group ${groupId}`, err)
      // If Grok was removed, state is stale (grokMode but Grok gone) — revert to teamQueue
      if (wasGrokMode) {
        this.conversations.set(groupId, {type: "teamQueue", userMessages: []})
      }
      await this.sendToGroup(groupId, "Sorry, there was an error adding a team member. Please try again.")
    }
  }

  // --- Helpers ---

  private async addReplacementTeamMember(groupId: number): Promise<void> {
    if (this.config.teamMembers.length === 0) return
    try {
      const teamContactId = this.config.teamMembers[0].id
      const member = await this.addOrFindTeamMember(groupId, teamContactId)
      if (member) {
        this.conversations.set(groupId, {type: "teamLocked", teamMemberGId: member.groupMemberId})
      }
    } catch (err) {
      logError(`Failed to add replacement team member to group ${groupId}`, err)
      // Stay in teamLocked with stale teamMemberGId — one-way gate must hold
      // Team will see the message in team group and can join manually
    }
  }

  private async addOrFindTeamMember(groupId: number, teamContactId: number): Promise<T.GroupMember | null> {
    try {
      return await this.mainChat.apiAddMember(groupId, teamContactId, T.GroupMemberRole.Member)
    } catch (err: any) {
      if (err?.chatError?.errorType?.type === "groupDuplicateMember") {
        // Team member already in group (e.g., from previous session) — find existing member
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
    this.grokGroupMap.delete(groupId)
    if (grokLocalGId !== undefined) {
      this.reverseGrokMap.delete(grokLocalGId)
    }
  }
}
