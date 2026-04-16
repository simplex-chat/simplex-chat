import {T} from "@simplex-chat/types"
import {api, util} from "simplex-chat"
import {Config} from "./config.js"
import {profileMutex, log, logError} from "./util.js"

// State derivation types
export type ConversationState = "WELCOME" | "QUEUE" | "GROK" | "TEAM-PENDING" | "TEAM"

export interface GroupComposition {
  grokMember: T.GroupMember | undefined
  teamMembers: T.GroupMember[]
}

interface CardData {
  cardItemId: number
  joinItemId?: number
  complete?: boolean
}

function isActiveMember(m: T.GroupMember): boolean {
  return m.memberStatus === T.GroupMemberStatus.Connected
    || m.memberStatus === T.GroupMemberStatus.Complete
    || m.memberStatus === T.GroupMemberStatus.Announced
}

// Prevent ! from triggering SimpleX markdown styled text (color/small).
// The parser treats !N<space> as color markup (N: 1-6, r, g, b, y, c, m, -)
// and closes at the next !. No escape mechanism exists in the parser,
// so we insert a zero-width space to break the trigger pattern.
function escapeStyledMarkdown(text: string): string {
  return text.replace(/!([1-6rgbycm-])/g, "!\u200B$1")
}

// Truncate a single message to ~maxChars, appending [truncated] if needed
function truncateMsg(text: string, maxChars: number): string {
  if (text.length <= maxChars) return text
  return text.slice(0, maxChars) + "… [truncated]"
}

// Describe non-text content types
function contentTypeLabel(ci: T.ChatItem): string | null {
  const content = ci.content as T.CIContent
  if (content.type !== "rcvMsgContent" && content.type !== "sndMsgContent") return null
  const mc = content.msgContent
  switch (mc.type) {
    case "image": return "[image]"
    case "video": return "[video]"
    case "voice": return "[voice]"
    case "file": return "[file]"
    default: return null
  }
}

export class CardManager {
  private pendingUpdates = new Set<number>()
  private flushInterval: NodeJS.Timeout

  constructor(
    private chat: api.ChatApi,
    private config: Config,
    private mainUserId: number,
    flushIntervalMs = 15 * 60 * 1000,
  ) {
    this.flushInterval = setInterval(() => this.flush(), flushIntervalMs)
    this.flushInterval.unref()
  }

  private async withMainProfile<R>(fn: () => Promise<R>): Promise<R> {
    return profileMutex.runExclusive(async () => {
      await this.chat.apiSetActiveUser(this.mainUserId)
      return fn()
    })
  }

  scheduleUpdate(groupId: number): void {
    this.pendingUpdates.add(groupId)
  }

  async createCard(groupId: number, groupInfo: T.GroupInfo): Promise<void> {
    const {text, joinCmd} = await this.composeCard(groupId, groupInfo)
    // Send card text and /join command as separate messages.
    // The /join must be a standalone single-line message so the client renders
    // the full command (including arguments) as clickable.
    const chatRef: T.ChatRef = {chatType: T.ChatType.Group, chatId: this.config.teamGroup.id}
    const items = await this.withMainProfile(() =>
      this.chat.apiSendMessages(chatRef, [
        {msgContent: {type: "text", text}, mentions: {}},
        {msgContent: {type: "text", text: joinCmd}, mentions: {}},
      ])
    )
    const data: CardData = {cardItemId: items[0].chatItem.meta.itemId}
    if (items.length > 1) data.joinItemId = items[1].chatItem.meta.itemId
    await this.withMainProfile(() =>
      this.chat.apiSetGroupCustomData(groupId, data)
    )
  }

  async flush(): Promise<void> {
    const groups = [...this.pendingUpdates]
    this.pendingUpdates.clear()
    for (const groupId of groups) {
      try {
        await this.updateCard(groupId)
      } catch (err) {
        logError(`Card flush failed for group ${groupId}`, err)
      }
    }
  }

  async refreshAllCards(): Promise<void> {
    const groups = await this.withMainProfile(() => this.chat.apiListGroups(this.mainUserId))
    const activeCards: {groupId: number; cardItemId: number}[] = []
    for (const group of groups) {
      const customData = group.customData as Record<string, unknown> | undefined
      if (customData && typeof customData.cardItemId === "number" && !customData.complete) {
        activeCards.push({groupId: group.groupId, cardItemId: customData.cardItemId})
      }
    }
    if (activeCards.length === 0) return

    // Sort ascending by cardItemId — higher ID = more recently updated card.
    // Oldest-updated cards refresh first; newest-updated refresh last,
    // so the most recent cards end up at the bottom of the team group.
    activeCards.sort((a, b) => a.cardItemId - b.cardItemId)

    log(`Startup: refreshing ${activeCards.length} card(s)`)

    for (const {groupId} of activeCards) {
      try {
        await this.updateCard(groupId)
      } catch (err) {
        logError(`Startup card refresh failed for group ${groupId}`, err)
      }
    }
  }

  destroy(): void {
    clearInterval(this.flushInterval)
  }

  // --- State derivation ---

  async getGroupComposition(groupId: number): Promise<GroupComposition> {
    const members = await this.withMainProfile(() => this.chat.apiListMembers(groupId))
    return {
      grokMember: members.find(m =>
        this.config.grokContactId !== null
        && m.memberContactId === this.config.grokContactId
        && isActiveMember(m)),
      teamMembers: members.filter(m =>
        this.config.teamMembers.some(tm => tm.id === m.memberContactId)
        && isActiveMember(m)),
    }
  }

  async deriveState(groupId: number): Promise<ConversationState> {
    const {grokMember, teamMembers} = await this.getGroupComposition(groupId)
    if (teamMembers.length > 0) {
      const hasTeamMsg = await this.hasTeamMemberSentMessage(groupId)
      return hasTeamMsg ? "TEAM" : "TEAM-PENDING"
    }
    if (grokMember) return "GROK"
    const isFirst = await this.isFirstCustomerMessage(groupId)
    return isFirst ? "WELCOME" : "QUEUE"
  }

  async isFirstCustomerMessage(groupId: number): Promise<boolean> {
    const chat = await this.getChat(groupId, 20)
    return !chat.chatItems.some((ci: T.ChatItem) => {
      if (ci.chatDir.type !== "groupSnd") return false
      const text = util.ciContentText(ci)
      return text?.includes("The team can see your message")
        || text?.includes("now chatting with Grok")
        || text?.includes("team member has been added")
        || text?.includes("team member has already been invited")
    })
  }

  async hasTeamMemberSentMessage(groupId: number): Promise<boolean> {
    const chat = await this.getChat(groupId, 50)
    return chat.chatItems.some((ci: T.ChatItem) => {
      if (ci.chatDir.type !== "groupRcv") return false
      const memberContactId = ci.chatDir.groupMember.memberContactId
      return this.config.teamMembers.some(tm => tm.id === memberContactId)
        && util.ciContentText(ci)?.trim()
    })
  }

  async getLastCustomerMessageTime(groupId: number, customerId: string): Promise<number | undefined> {
    const chat = await this.getChat(groupId, 20)
    for (let i = chat.chatItems.length - 1; i >= 0; i--) {
      const ci = chat.chatItems[i]
      if (ci.chatDir.type === "groupRcv" && ci.chatDir.groupMember.memberId === customerId) {
        return new Date(ci.meta.createdAt).getTime()
      }
    }
    return undefined
  }

  async getLastTeamOrGrokMessageTime(groupId: number): Promise<number | undefined> {
    const chat = await this.getChat(groupId, 20)
    for (let i = chat.chatItems.length - 1; i >= 0; i--) {
      const ci = chat.chatItems[i]
      if (ci.chatDir.type === "groupRcv") {
        const contactId = ci.chatDir.groupMember.memberContactId
        const isTeam = this.config.teamMembers.some(tm => tm.id === contactId)
        const isGrok = this.config.grokContactId !== null && contactId === this.config.grokContactId
        if (isTeam || isGrok) return new Date(ci.meta.createdAt).getTime()
      }
      if (ci.chatDir.type === "groupSnd") {
        // Bot's own messages don't count
      }
    }
    return undefined
  }

  // --- Custom data ---

  async getCustomData(groupId: number): Promise<CardData | null> {
    const groups = await this.withMainProfile(() => this.chat.apiListGroups(this.mainUserId))
    const group = groups.find(g => g.groupId === groupId)
    if (!group?.customData) return null
    const data = group.customData as Record<string, unknown>
    if (typeof data.cardItemId === "number") {
      const result: CardData = {cardItemId: data.cardItemId}
      if (typeof data.joinItemId === "number") result.joinItemId = data.joinItemId
      return result
    }
    return null
  }

  async setCustomData(groupId: number, data: CardData): Promise<void> {
    await this.withMainProfile(() => this.chat.apiSetGroupCustomData(groupId, data))
  }

  async clearCustomData(groupId: number): Promise<void> {
    await this.withMainProfile(() => this.chat.apiSetGroupCustomData(groupId))
  }

  // --- Chat history access ---

  async getChat(groupId: number, count: number): Promise<T.AChat> {
    return this.withMainProfile(() => this.chat.apiGetChat(T.ChatType.Group, groupId, count))
  }

  // --- Internal ---

  private async updateCard(groupId: number): Promise<void> {
    // Read customData and groupInfo in one apiListGroups call
    const groups = await this.withMainProfile(() => this.chat.apiListGroups(this.mainUserId))
    const groupInfo = groups.find(g => g.groupId === groupId)
    if (!groupInfo) return

    const customData = groupInfo.customData as Record<string, unknown> | undefined
    const cardItemId = customData?.cardItemId
    if (typeof cardItemId !== "number") return

    // Delete old card + join command messages
    const deleteIds = [cardItemId]
    const joinItemId = customData?.joinItemId
    if (typeof joinItemId === "number") deleteIds.push(joinItemId)
    try {
      await this.withMainProfile(() =>
        this.chat.apiDeleteChatItems(
          T.ChatType.Group, this.config.teamGroup.id, deleteIds, T.CIDeleteMode.Broadcast
        )
      )
    } catch {
      // card may already be deleted
    }

    const {text, joinCmd, complete} = await this.composeCard(groupId, groupInfo)
    const chatRef: T.ChatRef = {chatType: T.ChatType.Group, chatId: this.config.teamGroup.id}
    const items = await this.withMainProfile(() =>
      this.chat.apiSendMessages(chatRef, [
        {msgContent: {type: "text", text}, mentions: {}},
        {msgContent: {type: "text", text: joinCmd}, mentions: {}},
      ])
    )
    const data: CardData = {cardItemId: items[0].chatItem.meta.itemId}
    if (items.length > 1) data.joinItemId = items[1].chatItem.meta.itemId
    if (complete) data.complete = true
    await this.withMainProfile(() =>
      this.chat.apiSetGroupCustomData(groupId, data)
    )
  }

  private async composeCard(groupId: number, groupInfo: T.GroupInfo): Promise<{text: string, joinCmd: string, complete: boolean}> {
    const rawName = groupInfo.groupProfile.displayName || `group-${groupId}`
    const customerName = rawName.replace(/\n+/g, " ")
    const bc = groupInfo.businessChat
    const customerId = bc?.customerId

    // State derivation
    const {grokMember, teamMembers} = await this.getGroupComposition(groupId)
    let state: ConversationState
    if (teamMembers.length > 0) {
      const hasTeamMsg = await this.hasTeamMemberSentMessage(groupId)
      state = hasTeamMsg ? "TEAM" : "TEAM-PENDING"
    } else if (grokMember) {
      state = "GROK"
    } else {
      state = "QUEUE"
    }

    // Icon
    const icon = await this.computeIcon(groupId, state, customerId ?? undefined)

    // Wait time
    const waitStr = await this.computeWaitTime(groupId, state, customerId ?? undefined)

    // Message count (all except bot's own groupSnd)
    const chat = await this.getChat(groupId, 100)
    const msgCount = chat.chatItems.filter((ci: T.ChatItem) => ci.chatDir.type !== "groupSnd").length

    // State label
    const stateLabel = this.stateLabel(state)

    // Agents
    const agentNames = teamMembers.map(m => m.memberProfile.displayName)
    const agentStr = agentNames.length > 0 ? ` · ${agentNames.join(", ")}` : ""

    // Message preview
    const preview = this.buildPreview(chat.chatItems, customerName, customerId)

    // /join command uses raw name so it matches the actual group profile
    const formatted = rawName.includes(" ") ? `'${rawName}'` : rawName
    const joinCmd = `/join ${groupId}:${formatted}`

    // Compose card text (without /join)
    const line1 = `${icon} *${customerName}* · ${waitStr} · ${msgCount} msgs`
    const line2 = `${stateLabel}${agentStr}`
    return {text: `${line1}\n${line2}\n${preview}`, joinCmd, complete: icon === "✅"}
  }

  private async computeIcon(
    groupId: number, state: ConversationState, customerId?: string,
  ): Promise<string> {
    const now = Date.now()
    const completeMs = this.config.completeHours * 3600_000

    // Check auto-complete: last team/Grok message time vs customer silence
    const lastTeamGrokTime = await this.getLastTeamOrGrokMessageTime(groupId)
    if (lastTeamGrokTime) {
      const lastCustTime = customerId
        ? await this.getLastCustomerMessageTime(groupId, customerId)
        : undefined
      // Auto-complete if team/grok replied and customer hasn't responded since, for completeHours
      if (!lastCustTime || lastCustTime < lastTeamGrokTime) {
        if (now - lastTeamGrokTime >= completeMs) return "✅"
      }
    }

    switch (state) {
      case "QUEUE": {
        const lastCustTime = customerId
          ? await this.getLastCustomerMessageTime(groupId, customerId)
          : undefined
        if (!lastCustTime) return "🟡"
        const waitMs = now - lastCustTime
        if (waitMs < 5 * 60_000) return "🆕"
        if (waitMs < 2 * 3600_000) return "🟡"
        return "🔴"
      }
      case "GROK":
        return "🤖"
      case "TEAM-PENDING":
        return "👋"
      case "TEAM": {
        // Check if customer follow-up unanswered > 2h
        const lastCustTime = customerId
          ? await this.getLastCustomerMessageTime(groupId, customerId)
          : undefined
        if (lastCustTime && lastTeamGrokTime && lastCustTime > lastTeamGrokTime) {
          return (now - lastCustTime > 2 * 3600_000) ? "⏰" : "💬"
        }
        return "💬"
      }
      default:
        return "🟡"
    }
  }

  private async computeWaitTime(
    groupId: number, _state: ConversationState, customerId?: string,
  ): Promise<string> {
    const now = Date.now()
    const completeMs = this.config.completeHours * 3600_000

    const lastTeamGrokTime = await this.getLastTeamOrGrokMessageTime(groupId)
    if (lastTeamGrokTime) {
      const lastCustTime = customerId
        ? await this.getLastCustomerMessageTime(groupId, customerId)
        : undefined
      if (!lastCustTime || lastCustTime < lastTeamGrokTime) {
        if (now - lastTeamGrokTime >= completeMs) return "done"
      }
    }

    const lastCustTime = customerId
      ? await this.getLastCustomerMessageTime(groupId, customerId)
      : undefined
    if (!lastCustTime) return "<1m"
    return this.formatDuration(now - lastCustTime)
  }

  private stateLabel(state: ConversationState): string {
    switch (state) {
      case "QUEUE": return "Queue"
      case "GROK": return "Grok"
      case "TEAM-PENDING": return "Team – pending"
      case "TEAM": return "Team"
      default: return "Queue"
    }
  }

  private buildPreview(chatItems: T.ChatItem[], customerName: string, customerId?: string): string {
    const maxTotal = 500
    const maxPer = 200

    // Collect entries in chronological order (oldest first)
    const entries: {senderId: string; name: string; text: string}[] = []
    for (const ci of chatItems) {
      if (ci.chatDir.type === "groupSnd") continue

      let text = (util.ciContentText(ci)?.trim() || "").replace(/\n+/g, " ")
      const mediaLabel = contentTypeLabel(ci)
      if (mediaLabel && !text) text = mediaLabel
      else if (mediaLabel) text = `${mediaLabel} ${text}`
      if (!text) continue

      let senderId = ""
      let name = ""
      if (ci.chatDir.type === "groupRcv") {
        const member = ci.chatDir.groupMember
        const contactId = member.memberContactId
        senderId = member.memberId
        if (this.config.grokContactId !== null && contactId === this.config.grokContactId) {
          name = "Grok"
        } else if (customerId && member.memberId === customerId) {
          name = customerName
        } else {
          name = member.memberProfile.displayName
        }
      }

      entries.push({senderId, name, text: truncateMsg(text, maxPer)})
    }

    // Compute prefixed lines in chronological order (sender prefix on first msg of each run)
    const lines: {line: string; senderId: string; name: string}[] = []
    let lastSenderId = ""
    for (const entry of entries) {
      let line = entry.text
      if (entry.senderId !== lastSenderId && entry.name) {
        line = `${entry.name}: ${line}`
        lastSenderId = entry.senderId
      }
      lines.push({line, senderId: entry.senderId, name: entry.name})
    }

    // Take from the end (newest) until maxTotal exceeded — oldest messages are truncated
    const selected: string[] = []
    let totalLen = 0
    let firstSelectedIdx = lines.length
    for (let i = lines.length - 1; i >= 0; i--) {
      if (totalLen + lines[i].line.length > maxTotal && selected.length > 0) {
        break
      }
      selected.push(lines[i].line)
      totalLen += lines[i].line.length
      firstSelectedIdx = i
    }
    selected.reverse()

    // If truncation happened, ensure the first visible message has a sender prefix
    if (firstSelectedIdx > 0 && selected.length > 0) {
      const first = lines[firstSelectedIdx]
      if (first.name && !selected[0].startsWith(`${first.name}: `)) {
        selected[0] = `${first.name}: ${selected[0]}`
      }
      selected.unshift("[truncated]")
    }

    const preview = selected.map(escapeStyledMarkdown).join(" !3 /! ")
    return preview ? `"${preview}"` : '""'
  }

  private formatDuration(ms: number): string {
    if (ms < 60_000) return "<1m"
    if (ms < 3_600_000) return `${Math.floor(ms / 60_000)}m`
    if (ms < 86_400_000) return `${Math.floor(ms / 3_600_000)}h`
    return `${Math.floor(ms / 86_400_000)}d`
  }
}
