import {api, util} from "simplex-chat"
import {T, CEvt} from "@simplex-chat/types"
import {Config} from "./config.js"
import {GrokMessage, GrokApiClient} from "./grok.js"
import {CardManager} from "./cards.js"
import {
  queueMessage, grokInvitingMessage, grokActivatedMessage, teamAddedMessage,
  teamAlreadyInvitedMessage, teamLockedMessage, noTeamMembersMessage,
  grokUnavailableMessage, grokErrorMessage, grokNoHistoryMessage,
} from "./messages.js"
import {profileMutex, log, logError} from "./util.js"

export class SupportBot {
  // Card manager
  cards: CardManager

  // Grok group mapping: memberId → mainGroupId (for pending joins)
  private pendingGrokJoins = new Map<string, number>()
  // Buffered invitations that arrived before pendingGrokJoins was set (race condition)
  private bufferedGrokInvitations = new Map<string, CEvt.ReceivedGroupInvitation>()
  // mainGroupId → grokLocalGroupId
  private grokGroupMap = new Map<number, number>()
  // grokLocalGroupId → mainGroupId
  private reverseGrokMap = new Map<number, number>()
  // mainGroupId → resolve fn for grok join
  private grokJoinResolvers = new Map<number, () => void>()
  // mainGroupIds where Grok connectedToGroupMember fired
  private grokFullyConnected = new Set<number>()

  // Pending DMs for team group members (contactId → message)
  private pendingTeamDMs = new Map<number, string>()
  // Contacts that already received the team DM (dedup)
  private sentTeamDMs = new Set<number>()

  // Tracked fire-and-forget operations (for testing)
  private _pendingOps: Promise<void>[] = []

  // Bot's business address link
  businessAddress: string | null = null

  constructor(
    private chat: api.ChatApi,
    private grokApi: GrokApiClient,
    private config: Config,
    private mainUserId: number,
    private grokUserId: number,
  ) {
    this.cards = new CardManager(chat, config, mainUserId, config.cardFlushMinutes * 60 * 1000)
  }

  // Wait for all fire-and-forget operations to settle (for testing)
  async flush(): Promise<void> {
    while (this._pendingOps.length > 0) {
      const ops = this._pendingOps.splice(0)
      await Promise.allSettled(ops)
    }
  }

  private fireAndForget(op: Promise<void>): void {
    const tracked = op.catch(err => logError("async operation error", err))
    this._pendingOps.push(tracked)
    tracked.finally(() => {
      const idx = this._pendingOps.indexOf(tracked)
      if (idx >= 0) this._pendingOps.splice(idx, 1)
    })
  }

  // --- Profile-switching helpers ---

  private async withMainProfile<R>(fn: () => Promise<R>): Promise<R> {
    return profileMutex.runExclusive(async () => {
      await this.chat.apiSetActiveUser(this.mainUserId)
      return fn()
    })
  }

  private async withGrokProfile<R>(fn: () => Promise<R>): Promise<R> {
    return profileMutex.runExclusive(async () => {
      await this.chat.apiSetActiveUser(this.grokUserId)
      return fn()
    })
  }

  // --- Main profile event handlers ---

  async onBusinessRequest(evt: CEvt.AcceptingBusinessRequest): Promise<void> {
    const groupId = evt.groupInfo.groupId
    try {
      const profile = evt.groupInfo.groupProfile
      await this.withMainProfile(() =>
        this.chat.apiUpdateGroupProfile(groupId, {
          displayName: profile.displayName,
          fullName: profile.fullName,
          groupPreferences: {
            ...profile.groupPreferences,
            files: {enable: T.GroupFeatureEnabled.On},
            history: {enable: T.GroupFeatureEnabled.On},
          },
        })
      )
      // file uploads + history enabled
    } catch (err) {
      logError(`Failed to update business group ${groupId} preferences`, err)
    }
  }

  async onNewChatItems(evt: CEvt.NewChatItems): Promise<void> {
    // Only process events for main profile
    if (evt.user.userId !== this.mainUserId) return
    for (const ci of evt.chatItems) {
      try {
        await this.processMainChatItem(ci)
      } catch (err) {
        logError("Error processing chat item", err)
      }
    }
  }

  async onChatItemUpdated(evt: CEvt.ChatItemUpdated): Promise<void> {
    if (evt.user.userId !== this.mainUserId) return
    const {chatInfo} = evt.chatItem
    if (chatInfo.type !== "group") return
    const groupInfo = chatInfo.groupInfo
    if (!groupInfo.businessChat) return
    this.cards.scheduleUpdate(groupInfo.groupId)
  }

  async onChatItemReaction(evt: CEvt.ChatItemReaction): Promise<void> {
    if (evt.user.userId !== this.mainUserId) return
    if (!evt.added) return
    const chatInfo = evt.reaction.chatInfo
    if (chatInfo.type !== "group") return
    const groupInfo = chatInfo.groupInfo
    if (!groupInfo.businessChat) return
    this.cards.scheduleUpdate(groupInfo.groupId)
  }

  async onLeftMember(evt: CEvt.LeftMember): Promise<void> {
    if (evt.user.userId !== this.mainUserId) return
    const groupId = evt.groupInfo.groupId
    const member = evt.member
    const bc = evt.groupInfo.businessChat
    if (!bc) return

    if (member.memberId === bc.customerId) {
      log(`Customer left group ${groupId}`)
      this.cleanupGrokMaps(groupId)
      try { await this.cards.clearCustomData(groupId) } catch {}
      return
    }

    if (this.config.grokContactId !== null && member.memberContactId === this.config.grokContactId) {
      log(`Grok left group ${groupId}`)
      this.cleanupGrokMaps(groupId)
      return
    }

    if (this.config.teamMembers.some(tm => tm.id === member.memberContactId)) {
      log(`Team member left group ${groupId}`)
    }
  }

  async onJoinedGroupMember(evt: CEvt.JoinedGroupMember): Promise<void> {
    if (evt.user.userId !== this.mainUserId) return
    if (evt.groupInfo.groupId === this.config.teamGroup.id) {
      await this.sendTeamMemberDM(evt.member)
    }
  }

  async onMemberConnected(evt: CEvt.ConnectedToGroupMember): Promise<void> {
    if (evt.user.userId !== this.mainUserId) return
    const groupId = evt.groupInfo.groupId

    // Team group → send DM (if not already sent by onJoinedGroupMember)
    if (groupId === this.config.teamGroup.id) {
      await this.sendTeamMemberDM(evt.member, evt.memberContact)
      return
    }

    // Customer group → promote to Owner (unless customer or Grok). Idempotent per plan §11.
    const bc = evt.groupInfo.businessChat
    if (bc) {
      const isCustomer = evt.member.memberId === bc.customerId
      const isGrok = this.config.grokContactId !== null
        && evt.member.memberContactId === this.config.grokContactId
      if (!isCustomer && !isGrok) {
        try {
          await this.withMainProfile(() =>
            this.chat.apiSetMembersRole(groupId, [evt.member.groupMemberId], T.GroupMemberRole.Owner)
          )
          log(`Promoted member ${evt.member.groupMemberId} to Owner in group ${groupId}`)
        } catch (err) {
          logError(`Failed to promote member in group ${groupId}`, err)
        }
      }
    }
  }

  async onMemberContactReceivedInv(evt: CEvt.NewMemberContactReceivedInv): Promise<void> {
    if (evt.user.userId !== this.mainUserId) return
    const {contact, groupInfo, member} = evt
    if (groupInfo.groupId === this.config.teamGroup.id) {
      if (this.sentTeamDMs.has(contact.contactId)) return
      log(`DM contact from team group member: ${contact.contactId}:${member.memberProfile.displayName}`)
      const name = member.memberProfile.displayName
      const formatted = name.includes(" ") ? `'${name}'` : name
      const msg = `Added you to be able to invite you to customer chats later, keep this contact. Your contact ID is ${contact.contactId}:${formatted}`
      // Try sending immediately — contact may already be usable
      try {
        await this.withMainProfile(() =>
          this.chat.apiSendTextMessage([T.ChatType.Direct, contact.contactId], msg)
        )
        this.sentTeamDMs.add(contact.contactId)
        log(`Sent DM to team member ${contact.contactId}:${name}`)
      } catch {
        // Not ready yet — queue for contactConnected / contactSndReady
        this.pendingTeamDMs.set(contact.contactId, msg)
        log(`Queued DM for team member ${contact.contactId}:${name}`)
      }
    }
  }

  async onContactConnected(evt: CEvt.ContactConnected): Promise<void> {
    if (evt.user.userId !== this.mainUserId) return
    await this.deliverPendingDM(evt.contact.contactId)
  }

  async onContactSndReady(evt: CEvt.ContactSndReady): Promise<void> {
    if (evt.user.userId !== this.mainUserId) return
    await this.deliverPendingDM(evt.contact.contactId)
  }

  private async deliverPendingDM(contactId: number): Promise<void> {
    if (this.sentTeamDMs.has(contactId)) {
      this.pendingTeamDMs.delete(contactId)
      return
    }
    const pendingMsg = this.pendingTeamDMs.get(contactId)
    if (pendingMsg === undefined) return
    this.pendingTeamDMs.delete(contactId)
    try {
      await this.withMainProfile(() =>
        this.chat.apiSendTextMessage([T.ChatType.Direct, contactId], pendingMsg)
      )
      this.sentTeamDMs.add(contactId)
      log(`Sent DM to team member ${contactId}`)
    } catch (err) {
      logError(`Failed to send DM to team member ${contactId}`, err)
    }
  }

  // --- Grok profile event handlers ---

  async onGrokGroupInvitation(evt: CEvt.ReceivedGroupInvitation): Promise<void> {
    if (evt.user.userId !== this.grokUserId) return
    const memberId = evt.groupInfo.membership.memberId
    const mainGroupId = this.pendingGrokJoins.get(memberId)
    if (mainGroupId === undefined) {
      // Buffer: invitation may arrive before pendingGrokJoins is set (race with apiAddMember)
      this.bufferedGrokInvitations.set(memberId, evt)
      return
    }
    this.pendingGrokJoins.delete(memberId)
    this.bufferedGrokInvitations.delete(memberId)
    await this.processGrokInvitation(evt, mainGroupId)
  }

  private async processGrokInvitation(evt: CEvt.ReceivedGroupInvitation, mainGroupId: number): Promise<void> {
    log(`Grok joining group: mainGroupId=${mainGroupId}, grokGroupId=${evt.groupInfo.groupId}`)
    try {
      await this.withGrokProfile(() => this.chat.apiJoinGroup(evt.groupInfo.groupId))
    } catch (err) {
      logError(`Grok failed to join group ${evt.groupInfo.groupId}`, err)
      return
    }
    this.grokGroupMap.set(mainGroupId, evt.groupInfo.groupId)
    this.reverseGrokMap.set(evt.groupInfo.groupId, mainGroupId)
  }

  async onGrokMemberConnected(evt: CEvt.ConnectedToGroupMember): Promise<void> {
    if (evt.user.userId !== this.grokUserId) return
    const grokGroupId = evt.groupInfo.groupId
    const mainGroupId = this.reverseGrokMap.get(grokGroupId)
    if (mainGroupId === undefined) return
    this.grokFullyConnected.add(mainGroupId)
    const resolver = this.grokJoinResolvers.get(mainGroupId)
    if (resolver) {
      this.grokJoinResolvers.delete(mainGroupId)
      log(`Grok fully connected: mainGroupId=${mainGroupId}, grokGroupId=${grokGroupId}`)
      resolver()
    }
  }

  async onGrokNewChatItems(evt: CEvt.NewChatItems): Promise<void> {
    if (evt.user.userId !== this.grokUserId) return
    for (const ci of evt.chatItems) {
      try {
        await this.processGrokChatItem(ci)
      } catch (err) {
        logError("Error processing Grok chat item", err)
      }
    }
  }

  // --- Main profile message routing ---

  private async processMainChatItem(ci: T.AChatItem): Promise<void> {
    const {chatInfo, chatItem} = ci

    // 1. Direct text message → reply with business address
    if (chatInfo.type === "direct" && chatItem.chatDir.type === "directRcv"
      && (chatItem.content as any).type === "rcvMsgContent") {
      if (this.businessAddress) {
        const contactId = chatInfo.contact.contactId
        try {
          await this.withMainProfile(() =>
            this.chat.apiSendTextMessage(
              [T.ChatType.Direct, contactId],
              `Please use my business address to ask questions: ${this.businessAddress}`,
            )
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

    // 2. Team group → handle /join
    if (groupId === this.config.teamGroup.id) {
      await this.processTeamGroupMessage(chatItem)
      return
    }

    // 3. Skip non-business groups
    if (!groupInfo.businessChat) return

    // 4. Skip own messages
    if (chatItem.chatDir.type === "groupSnd") return
    if (chatItem.chatDir.type !== "groupRcv") return

    const sender = chatItem.chatDir.groupMember
    const bc = groupInfo.businessChat
    const isCustomer = sender.memberId === bc.customerId

    // 6. Non-customer message → one-way gate check + card update
    if (!isCustomer) {
      const isTeam = this.config.teamMembers.some(tm => tm.id === sender.memberContactId)

      if (isTeam && util.ciContentText(chatItem)?.trim()) {
        // Check one-way gate: first team text → remove Grok
        const {grokMember} = await this.cards.getGroupComposition(groupId)
        if (grokMember) {
          log(`One-way gate: team message in group ${groupId}, removing Grok`)
          try {
            await this.withMainProfile(() =>
              this.chat.apiRemoveMembers(groupId, [grokMember.groupMemberId])
            )
          } catch {
            // may have already left
          }
          this.cleanupGrokMaps(groupId)
        }
      }
      // Schedule card update for any non-customer message (team or Grok)
      this.cards.scheduleUpdate(groupId)
      return
    }

    // 8. Customer message → derive state and dispatch
    const state = await this.cards.deriveState(groupId)
    const cmd = util.ciBotCommand(chatItem)
    const text = util.ciContentText(chatItem)?.trim() || null

    switch (state) {
      case "WELCOME":
        if (cmd?.keyword === "grok") {
          // WELCOME → GROK (skip queue msg)
          // Fire-and-forget: activateGrok awaits future events (waitForGrokJoin)
          // which would deadlock the sequential event loop if awaited here.
          // sendQueueOnFail=true: if Grok activation fails, send queue message as fallback
          await this.cards.createCard(groupId, groupInfo)
          this.fireAndForget(this.activateGrok(groupId, true))
          return
        }
        if (cmd?.keyword === "team") {
          await this.activateTeam(groupId)
          await this.cards.createCard(groupId, groupInfo)
          return
        }
        // First regular message → QUEUE
        if (text) {
          await this.sendToGroup(groupId, queueMessage(this.config.timezone))
          await this.cards.createCard(groupId, groupInfo)
        }
        break

      case "QUEUE":
        if (cmd?.keyword === "grok") {
          this.fireAndForget(this.activateGrok(groupId))
        } else if (cmd?.keyword === "team") {
          await this.activateTeam(groupId)
        }
        this.cards.scheduleUpdate(groupId)
        break

      case "GROK":
        if (cmd?.keyword === "team") {
          await this.activateTeam(groupId)
        } else if (cmd?.keyword === "grok") {
          // Already in grok mode — ignore
        } else if (text) {
          // Customer text → Grok responds (handled by Grok profile's onGrokNewChatItems)
          // Just schedule card update for the customer message
        }
        this.cards.scheduleUpdate(groupId)
        break

      case "TEAM-PENDING":
        if (cmd?.keyword === "grok") {
          // Invite Grok if not present
          const {grokMember} = await this.cards.getGroupComposition(groupId)
          if (!grokMember) {
            this.fireAndForget(this.activateGrok(groupId))
          }
          // else: already present, ignore
        } else if (cmd?.keyword === "team") {
          await this.sendToGroup(groupId, teamAlreadyInvitedMessage)
        }
        this.cards.scheduleUpdate(groupId)
        break

      case "TEAM":
        if (cmd?.keyword === "grok") {
          await this.sendToGroup(groupId, teamLockedMessage)
        }
        this.cards.scheduleUpdate(groupId)
        break
    }
  }

  // --- Grok profile message processing ---

  private async processGrokChatItem(ci: T.AChatItem): Promise<void> {
    const {chatInfo, chatItem} = ci
    if (chatInfo.type !== "group") return
    const groupInfo = chatInfo.groupInfo
    const grokGroupId = groupInfo.groupId

    // Only process received text messages from customer
    if (chatItem.chatDir.type !== "groupRcv") return
    const text = util.ciContentText(chatItem)?.trim()
    if (!text) return // ignore non-text

    // Ignore bot commands
    if (util.ciBotCommand(chatItem)) return

    // Only respond in business groups (survives restart without in-memory maps)
    const bc = groupInfo.businessChat
    if (!bc) return

    // Only respond to customer messages, not bot or team messages
    if (chatItem.chatDir.groupMember.memberId !== bc.customerId) return

    // Read history from Grok's own view
    try {
      const chat = await this.withGrokProfile(() =>
        this.chat.apiGetChat(T.ChatType.Group, grokGroupId, 100)
      )
      const history: GrokMessage[] = []
      for (const histCi of chat.chatItems) {
        const histText = util.ciContentText(histCi)?.trim()
        if (!histText) continue
        if (histCi.chatDir.type === "groupSnd") {
          history.push({role: "assistant", content: histText})
        } else if (histCi.chatDir.type === "groupRcv"
          && histCi.chatDir.groupMember.memberId === bc.customerId
          && !util.ciBotCommand(histCi)) {
          history.push({role: "user", content: histText})
        }
      }

      // Don't include the current message in history — it's the userMessage
      if (history.length > 0 && history[history.length - 1].role === "user"
        && history[history.length - 1].content === text) {
        history.pop()
      }

      // Call Grok API (outside mutex)
      const response = await this.grokApi.chat(history, text)

      // Send response via Grok profile
      await this.withGrokProfile(() =>
        this.chat.apiSendTextMessage([T.ChatType.Group, grokGroupId], response)
      )
    } catch (err) {
      logError(`Grok per-message error for grokGroup ${grokGroupId}`, err)
      try {
        await this.withGrokProfile(() =>
          this.chat.apiSendTextMessage([T.ChatType.Group, grokGroupId], grokErrorMessage)
        )
      } catch {}
    }

    // Card update scheduled by main profile seeing the groupRcv events
  }

  // --- Grok activation ---

  private async activateGrok(groupId: number, sendQueueOnFail = false): Promise<void> {
    if (this.config.grokContactId === null) {
      await this.sendToGroup(groupId, grokUnavailableMessage)
      if (sendQueueOnFail) await this.sendToGroup(groupId, queueMessage(this.config.timezone))
      this.cards.scheduleUpdate(groupId)
      return
    }

    await this.sendToGroup(groupId, grokInvitingMessage)

    let member: T.GroupMember
    try {
      member = await this.withMainProfile(() =>
        this.chat.apiAddMember(groupId, this.config.grokContactId!, T.GroupMemberRole.Member)
      )
    } catch (err: unknown) {
      const chatErr = err as {chatError?: {errorType?: {type?: string}}}
      if (chatErr?.chatError?.errorType?.type === "groupDuplicateMember") {
        // Grok already in group (e.g. customer sent /grok again before join completed) —
        // the in-flight activation will handle the outcome, just return silently
        return
      }
      logError(`Failed to invite Grok to group ${groupId}`, err)
      await this.sendToGroup(groupId, grokUnavailableMessage)
      if (sendQueueOnFail) await this.sendToGroup(groupId, queueMessage(this.config.timezone))
      this.cards.scheduleUpdate(groupId)
      return
    }

    this.pendingGrokJoins.set(member.memberId, groupId)

    // Drain buffered invitation that arrived during the apiAddMember await
    const buffered = this.bufferedGrokInvitations.get(member.memberId)
    if (buffered) {
      this.bufferedGrokInvitations.delete(member.memberId)
      this.pendingGrokJoins.delete(member.memberId)
      await this.processGrokInvitation(buffered, groupId)
    }

    const joined = await this.waitForGrokJoin(groupId, 120_000)
    if (!joined) {
      this.pendingGrokJoins.delete(member.memberId)
      try {
        await this.withMainProfile(() =>
          this.chat.apiRemoveMembers(groupId, [member.groupMemberId])
        )
      } catch {}
      this.cleanupGrokMaps(groupId)
      await this.sendToGroup(groupId, grokUnavailableMessage)
      if (sendQueueOnFail) await this.sendToGroup(groupId, queueMessage(this.config.timezone))
      this.cards.scheduleUpdate(groupId)
      return
    }

    await this.sendToGroup(groupId, grokActivatedMessage)

    // Grok joined — send initial response based on customer's accumulated messages
    try {
      const grokLocalGId = this.grokGroupMap.get(groupId)
      if (grokLocalGId === undefined) {
        await this.sendToGroup(groupId, grokUnavailableMessage)
        return
      }

      // Read history from Grok's own view — only customer messages
      const chat = await this.withGrokProfile(() =>
        this.chat.apiGetChat(T.ChatType.Group, grokLocalGId, 100)
      )
      const grokBc = chat.chatInfo.type === "group" ? chat.chatInfo.groupInfo.businessChat : null
      const customerMessages: string[] = []
      for (const ci of chat.chatItems) {
        if (ci.chatDir.type !== "groupRcv") continue
        if (grokBc && ci.chatDir.groupMember.memberId !== grokBc.customerId) continue
        const t = util.ciContentText(ci)?.trim()
        if (t && !util.ciBotCommand(ci)) customerMessages.push(t)
      }

      if (customerMessages.length === 0) {
        await this.withGrokProfile(() =>
          this.chat.apiSendTextMessage([T.ChatType.Group, grokLocalGId], grokNoHistoryMessage)
        )
        return
      }

      const initialMsg = customerMessages.join("\n")
      const response = await this.grokApi.chat([], initialMsg)

      await this.withGrokProfile(() =>
        this.chat.apiSendTextMessage([T.ChatType.Group, grokLocalGId], response)
      )
    } catch (err) {
      logError(`Grok initial response failed for group ${groupId}`, err)
      await this.sendToGroup(groupId, grokUnavailableMessage)
    }
  }

  // --- Team activation ---

  private async activateTeam(groupId: number): Promise<void> {
    if (this.config.teamMembers.length === 0) {
      await this.sendToGroup(groupId, noTeamMembersMessage)
      return
    }

    // Check if team was already activated before (message sent or "added" text in history)
    const hasTeamBefore = await this.cards.hasTeamMemberSentMessage(groupId)
    if (hasTeamBefore) {
      const {teamMembers} = await this.cards.getGroupComposition(groupId)
      if (teamMembers.length > 0) {
        await this.sendToGroup(groupId, teamAlreadyInvitedMessage)
        return
      }
      // Team members sent messages but all have left — re-add below
    }

    if (!hasTeamBefore) {
      // Check by scanning history for "team member has been added" AND verify team still present
      const chat = await this.cards.getChat(groupId, 50)
      const alreadyAdded = chat.chatItems.some((ci: T.ChatItem) =>
        ci.chatDir.type === "groupSnd"
        && util.ciContentText(ci)?.includes("team member has been added")
      )
      if (alreadyAdded) {
        const {teamMembers} = await this.cards.getGroupComposition(groupId)
        if (teamMembers.length > 0) {
          await this.sendToGroup(groupId, teamAlreadyInvitedMessage)
          return
        }
        // Team was previously added but all members left — re-add below
      }
    }

    // Add ALL configured team members — promoted to Owner on connectedToGroupMember
    for (const tm of this.config.teamMembers) {
      try {
        await this.addOrFindTeamMember(groupId, tm.id)
      } catch (err) {
        logError(`Failed to add team member ${tm.id} to group ${groupId}`, err)
      }
    }

    await this.sendToGroup(groupId, teamAddedMessage(this.config.timezone))
  }

  // --- Team group commands ---

  private async processTeamGroupMessage(chatItem: T.ChatItem): Promise<void> {
    if (chatItem.chatDir.type !== "groupRcv") return
    const text = util.ciContentText(chatItem)?.trim()
    if (!text) return
    const senderContactId = chatItem.chatDir.groupMember.memberContactId
    if (!senderContactId) return

    const joinMatch = text.match(/^\/join\s+(\d+):/)
    if (joinMatch) {
      await this.handleJoinCommand(parseInt(joinMatch[1], 10), senderContactId)
      return
    }
  }

  private async handleJoinCommand(targetGroupId: number, senderContactId: number): Promise<void> {
    // Validate target is a business group
    const groups = await this.withMainProfile(() =>
      this.chat.apiListGroups(this.mainUserId)
    )
    const targetGroup = groups.find(g => g.groupId === targetGroupId)
    if (!targetGroup?.businessChat) {
      await this.sendToGroup(this.config.teamGroup.id, `Error: group ${targetGroupId} is not a business chat`)
      return
    }

    try {
      const member = await this.addOrFindTeamMember(targetGroupId, senderContactId)
      if (member) {
        try {
          await this.withMainProfile(() =>
            this.chat.apiSetMembersRole(targetGroupId, [member.groupMemberId], T.GroupMemberRole.Owner)
          )
        } catch {
          // Not yet connected — will be promoted in onMemberConnected
        }
        log(`Team member ${senderContactId} joined group ${targetGroupId} via /join`)
      }
    } catch (err) {
      logError(`/join failed for group ${targetGroupId}`, err)
      await this.sendToGroup(this.config.teamGroup.id, `Error joining group ${targetGroupId}`)
    }
  }

  // --- Helpers ---

  private async addOrFindTeamMember(groupId: number, teamContactId: number): Promise<T.GroupMember | null> {
    try {
      return await this.withMainProfile(() =>
        this.chat.apiAddMember(groupId, teamContactId, T.GroupMemberRole.Member)
      )
    } catch (err: unknown) {
      const chatErr = err as {chatError?: {errorType?: {type?: string}}}
      if (chatErr?.chatError?.errorType?.type === "groupDuplicateMember") {
        log(`Team member already in group ${groupId}, looking up existing`)
        const members = await this.withMainProfile(() => this.chat.apiListMembers(groupId))
        return members.find(m => m.memberContactId === teamContactId) ?? null
      }
      throw err
    }
  }

  async sendToGroup(groupId: number, text: string): Promise<void> {
    try {
      await this.withMainProfile(() =>
        this.chat.apiSendTextMessage([T.ChatType.Group, groupId], text)
      )
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

  private async sendTeamMemberDM(member: T.GroupMember, memberContact?: T.Contact): Promise<void> {
    const name = member.memberProfile.displayName
    const formatted = name.includes(" ") ? `'${name}'` : name

    let contactId = memberContact?.contactId ?? member.memberContactId
    if (!contactId) {
      // No DM contact yet — create one and send invitation with message
      try {
        const createResp: any = await this.withMainProfile(() =>
          this.chat.sendChatCmd(`/_create member contact #${this.config.teamGroup.id} ${member.groupMemberId}`)
        )
        if (createResp.type !== "newMemberContact" || !createResp.contact?.contactId) {
          logError(`Unexpected response creating member contact for ${name}`, createResp)
          return
        }
        contactId = createResp.contact.contactId as number
        log(`Created DM contact ${contactId} for team member ${name}`)
      } catch (err) {
        logError(`Failed to create member contact for ${name}`, err)
        return
      }
      if (this.sentTeamDMs.has(contactId)) return
      const msg = `Added you to be able to invite you to customer chats later, keep this contact. Your contact ID is ${contactId}:${formatted}`
      try {
        await this.withMainProfile(() =>
          this.chat.sendChatCmd(`/_invite member contact @${contactId} text ${msg}`)
        )
        this.sentTeamDMs.add(contactId)
        this.pendingTeamDMs.delete(contactId)
        log(`Sent DM invitation to team member ${contactId}:${name}`)
      } catch {
        this.pendingTeamDMs.set(contactId, msg)
      }
      return
    }
    // Contact already exists — send via normal DM
    if (this.sentTeamDMs.has(contactId)) return
    const msg = `Added you to be able to invite you to customer chats later, keep this contact. Your contact ID is ${contactId}:${formatted}`
    try {
      await this.withMainProfile(() =>
        this.chat.apiSendTextMessage([T.ChatType.Direct, contactId], msg)
      )
      this.sentTeamDMs.add(contactId)
      this.pendingTeamDMs.delete(contactId)
      log(`Sent DM to team member ${contactId}:${name}`)
    } catch {
      this.pendingTeamDMs.set(contactId, msg)
    }
  }

  private cleanupGrokMaps(groupId: number): void {
    const grokLocalGId = this.grokGroupMap.get(groupId)
    this.grokFullyConnected.delete(groupId)
    if (grokLocalGId === undefined) return
    this.grokGroupMap.delete(groupId)
    this.reverseGrokMap.delete(grokLocalGId)
  }
}
