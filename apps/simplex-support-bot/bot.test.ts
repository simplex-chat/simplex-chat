// ═══════════════════════════════════════════════════════════════════
// SimpleX Support Bot — Acceptance Tests (Stateless)
// ═══════════════════════════════════════════════════════════════════
//
// Tests for the stateless support bot. State is derived from group
// composition (apiListMembers) and chat history (apiGetChat via
// sendChatCmd). All assertions verify observable behavior (messages
// sent, members added/removed) rather than internal state.
// ═══════════════════════════════════════════════════════════════════

import {describe, test, expect, beforeEach, afterEach, vi} from "vitest"

// ─── Module Mocks (hoisted by vitest) ────────────────────────────

vi.mock("simplex-chat", () => ({
  api: {},
  util: {
    ciBotCommand: (chatItem: any) =>
      chatItem._botCommand ? {keyword: chatItem._botCommand} : null,
    ciContentText: (chatItem: any) => chatItem._text ?? null,
  },
}))

vi.mock("@simplex-chat/types", () => ({
  T: {
    ChatType: {Group: "group"},
    GroupMemberRole: {Member: "member"},
    GroupMemberStatus: {
      Connected: "connected",
      Complete: "complete",
      Announced: "announced",
    },
    GroupFeatureEnabled: {
      On: "on",
      Off: "off",
    },
  },
  CEvt: {},
}))

vi.mock("./src/util", () => ({
  isWeekend: vi.fn(() => false),
  log: vi.fn(),
  logError: vi.fn(),
}))

vi.mock("fs", () => ({
  existsSync: vi.fn(() => false),
}))

vi.mock("child_process", () => ({
  execSync: vi.fn(() => ""),
}))

// ─── Imports (after mocks) ───────────────────────────────────────

import {SupportBot} from "./src/bot"
import {GrokApiClient} from "./src/grok"
import {resolveDisplayNameConflict} from "./src/startup"
import type {GrokMessage} from "./src/state"
import {isWeekend} from "./src/util"
import {existsSync} from "fs"
import {execSync} from "child_process"


// ─── Mock Grok API ──────────────────────────────────────────────

class MockGrokApi {
  private responses: Array<string | Error> = []
  calls: {history: GrokMessage[]; message: string}[] = []

  willRespond(text: string) { this.responses.push(text) }
  willFail()                { this.responses.push(new Error("Grok API error")) }

  async chat(history: GrokMessage[], message: string): Promise<string> {
    this.calls.push({history: [...history], message})
    const resp = this.responses.shift()
    if (!resp) throw new Error("MockGrokApi: no response configured")
    if (resp instanceof Error) throw resp
    return resp
  }

  lastCall()   { return this.calls[this.calls.length - 1] }
  callCount()  { return this.calls.length }
  reset()      { this.responses = []; this.calls = [] }
}


// ─── Mock Chat API ──────────────────────────────────────────────

interface SentMessage { chat: [string, number]; text: string }
interface AddedMember { groupId: number; contactId: number; role: string }
interface RemovedMembers { groupId: number; memberIds: number[] }

class MockChatApi {
  sent: SentMessage[] = []
  added: AddedMember[] = []
  removed: RemovedMembers[] = []
  joined: number[] = []
  members: Map<number, any[]> = new Map()  // groupId → members list
  chatItems: Map<number, any[]> = new Map()  // groupId → chat items (simulates DB)
  updatedProfiles: {groupId: number; profile: any}[] = []
  updatedChatItems: {chatType: string; chatId: number; chatItemId: number; msgContent: any}[] = []

  private addMemberFail = false
  private addMemberDuplicate = false
  private nextMemberGId = 50
  private nextItemId = 1000

  apiAddMemberWillFail()              { this.addMemberFail = true }
  apiAddMemberWillDuplicate()         { this.addMemberDuplicate = true }
  setNextGroupMemberId(id: number)    { this.nextMemberGId = id }
  setGroupMembers(groupId: number, members: any[]) { this.members.set(groupId, members) }
  setChatItems(groupId: number, items: any[]) { this.chatItems.set(groupId, items) }

  async apiSendTextMessage(chat: [string, number], text: string) {
    this.sent.push({chat, text})
    // Track bot-sent messages as groupSnd chat items (for isFirstCustomerMessage detection)
    const groupId = chat[1]
    if (!this.chatItems.has(groupId)) this.chatItems.set(groupId, [])
    this.chatItems.get(groupId)!.push({
      chatDir: {type: "groupSnd"},
      _text: text,
    })
    const itemId = this.nextItemId++
    return [{chatItem: {meta: {itemId}}}]
  }

  async apiUpdateGroupProfile(groupId: number, profile: any) {
    this.updatedProfiles.push({groupId, profile})
    return {groupId, groupProfile: profile}
  }

  async apiUpdateChatItem(chatType: string, chatId: number, chatItemId: number, msgContent: any, _live: false) {
    this.updatedChatItems.push({chatType, chatId, chatItemId, msgContent})
    return {meta: {itemId: chatItemId}}
  }

  async apiAddMember(groupId: number, contactId: number, role: string) {
    if (this.addMemberFail) { this.addMemberFail = false; throw new Error("apiAddMember failed") }
    if (this.addMemberDuplicate) {
      this.addMemberDuplicate = false
      const err: any = new Error("groupDuplicateMember")
      err.chatError = {type: "error", errorType: {type: "groupDuplicateMember", contactName: "TeamGuy"}}
      throw err
    }
    const gid = this.nextMemberGId++
    this.added.push({groupId, contactId, role})
    return {groupMemberId: gid, memberId: `member-${gid}`, memberContactId: contactId}
  }

  async apiRemoveMembers(groupId: number, memberIds: number[]) {
    this.removed.push({groupId, memberIds})
    // Remove from members list to reflect DB state
    const currentMembers = this.members.get(groupId)
    if (currentMembers) {
      this.members.set(groupId, currentMembers.filter(m => !memberIds.includes(m.groupMemberId)))
    }
  }

  async apiJoinGroup(groupId: number) {
    this.joined.push(groupId)
  }

  async apiListMembers(groupId: number) {
    return this.members.get(groupId) || []
  }

  // sendChatCmd is used by apiGetChat (interim approach)
  async sendChatCmd(cmd: string) {
    // Parse "/_get chat #<groupId> count=<n>"
    const match = cmd.match(/\/_get chat #(\d+) count=(\d+)/)
    if (match) {
      const groupId = parseInt(match[1])
      return {
        type: "apiChat",
        chat: {
          chatInfo: {type: "group"},
          chatItems: this.chatItems.get(groupId) || [],
          chatStats: {},
        },
      }
    }
    return {type: "cmdOk"}
  }

  sentTo(groupId: number): string[] {
    return this.sent.filter(m => m.chat[1] === groupId).map(m => m.text)
  }

  lastSentTo(groupId: number): string | undefined {
    const msgs = this.sentTo(groupId)
    return msgs[msgs.length - 1]
  }

  reset() {
    this.sent = []; this.added = []; this.removed = []; this.joined = []
    this.members.clear(); this.chatItems.clear()
    this.updatedProfiles = []; this.updatedChatItems = []
    this.addMemberFail = false; this.addMemberDuplicate = false; this.nextMemberGId = 50; this.nextItemId = 1000
  }
}


// ─── Event Factories ────────────────────────────────────────────

const GROUP_ID    = 100
const TEAM_GRP_ID = 1
const GROK_LOCAL  = 200
const CUSTOMER_ID = "cust-1"

function businessGroupInfo(groupId = GROUP_ID, displayName = "Alice") {
  return {
    groupId,
    groupProfile: {displayName},
    businessChat: {customerId: CUSTOMER_ID},
    membership: {memberId: "bot-member"},
  } as any
}

let nextChatItemId = 500

function customerChatItem(text: string | null, command: string | null = null) {
  const itemId = nextChatItemId++
  return {
    chatInfo: {type: "group", groupInfo: businessGroupInfo()},
    chatItem: {
      chatDir: {
        type: "groupRcv",
        groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10},
      },
      meta: {itemId},
      content: {type: "text", text: text ?? ""},
      _botCommand: command,
      _text: text,
    },
  } as any
}

function teamMemberChatItem(teamMemberGId: number, text: string) {
  const itemId = nextChatItemId++
  return {
    chatInfo: {type: "group", groupInfo: businessGroupInfo()},
    chatItem: {
      chatDir: {
        type: "groupRcv",
        groupMember: {memberId: "team-member-1", groupMemberId: teamMemberGId, memberContactId: 2, memberProfile: {displayName: "Bob"}},
      },
      meta: {itemId},
      content: {type: "text", text},
      _text: text,
    },
  } as any
}

function grokMemberChatItem(grokMemberGId: number, text: string) {
  return {
    chatInfo: {type: "group", groupInfo: businessGroupInfo()},
    chatItem: {
      chatDir: {
        type: "groupRcv",
        groupMember: {memberId: "grok-1", groupMemberId: grokMemberGId, memberContactId: 4},
      },
      content: {type: "text", text},
      _text: text,
    },
  } as any
}

function botOwnChatItem(text: string) {
  return {
    chatInfo: {type: "group", groupInfo: businessGroupInfo()},
    chatItem: {chatDir: {type: "groupSnd"}, content: {type: "text", text}},
  } as any
}


// ─── Test DSL ───────────────────────────────────────────────────

let bot: SupportBot
let mainChat: MockChatApi
let grokChat: MockChatApi
let grokApi: MockGrokApi
let lastTeamMemberGId: number
let lastGrokMemberGId: number

const customer = {
  async sends(text: string, groupId = GROUP_ID) {
    const isGrokCmd = text === "/grok"
    const isTeamCmd = text === "/team"
    const command = isGrokCmd ? "grok" : isTeamCmd ? "team" : null
    const ci = customerChatItem(text, command)
    ci.chatInfo.groupInfo = businessGroupInfo(groupId)
    // Track customer message in mock chat items (simulates DB)
    if (!mainChat.chatItems.has(groupId)) mainChat.chatItems.set(groupId, [])
    const storedItem: any = {
      chatDir: {
        type: "groupRcv",
        groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10},
      },
      _text: text,
    }
    if (command) storedItem._botCommand = command
    mainChat.chatItems.get(groupId)!.push(storedItem)
    await bot.onNewChatItems({chatItems: [ci]} as any)
  },

  async sendsNonText(groupId = GROUP_ID) {
    const ci = customerChatItem(null, null)
    ci.chatInfo.groupInfo = businessGroupInfo(groupId)
    await bot.onNewChatItems({chatItems: [ci]} as any)
  },

  async leaves(groupId = GROUP_ID) {
    await bot.onLeftMember({
      groupInfo: businessGroupInfo(groupId),
      member: {memberId: CUSTOMER_ID, groupMemberId: 10},
    } as any)
  },

  received(expected: string, groupId = GROUP_ID) {
    const msgs = mainChat.sentTo(groupId)
    expect(msgs).toContain(expected)
  },

  receivedFromGrok(expected: string) {
    const msgs = grokChat.sentTo(GROK_LOCAL)
    expect(msgs).toContain(expected)
  },

  receivedNothing(groupId = GROUP_ID) {
    expect(mainChat.sentTo(groupId)).toEqual([])
  },
}

// Format helpers for expected forwarded messages
function fmtCustomer(text: string, name = "Alice", groupId = GROUP_ID) {
  return `${name}:${groupId}: ${text}`
}
function fmtTeamMember(tmContactId: number, text: string, tmName = "Bob", customerName = "Alice", groupId = GROUP_ID) {
  return `${tmName}:${tmContactId} > ${customerName}:${groupId}: ${text}`
}

const teamGroup = {
  received(expected: string) {
    const msgs = mainChat.sentTo(TEAM_GRP_ID)
    expect(msgs).toContain(expected)
  },

  receivedNothing() {
    expect(mainChat.sentTo(TEAM_GRP_ID)).toEqual([])
  },
}

const teamMember = {
  wasInvited(groupId = GROUP_ID) {
    const found = mainChat.added.some(a => a.groupId === groupId && a.contactId === 2)
    expect(found).toBe(true)
  },

  async sends(text: string, groupId = GROUP_ID) {
    const ci = teamMemberChatItem(lastTeamMemberGId, text)
    ci.chatInfo.groupInfo = businessGroupInfo(groupId)
    // Track team member message in mock chat items
    if (!mainChat.chatItems.has(groupId)) mainChat.chatItems.set(groupId, [])
    mainChat.chatItems.get(groupId)!.push({
      chatDir: {
        type: "groupRcv",
        groupMember: {memberId: "team-member-1", groupMemberId: lastTeamMemberGId, memberContactId: 2},
      },
      _text: text,
    })
    await bot.onNewChatItems({chatItems: [ci]} as any)
  },

  async leaves(groupId = GROUP_ID) {
    await bot.onLeftMember({
      groupInfo: businessGroupInfo(groupId),
      member: {memberId: "team-member-1", groupMemberId: lastTeamMemberGId, memberContactId: 2},
    } as any)
  },
}

const grokAgent = {
  wasInvited(groupId = GROUP_ID) {
    const found = mainChat.added.some(a => a.groupId === groupId && a.contactId === 4)
    expect(found).toBe(true)
  },

  async joins() {
    await new Promise<void>(r => setTimeout(r, 0))
    const memberId = `member-${lastGrokMemberGId}`
    await bot.onGrokGroupInvitation({
      groupInfo: {
        groupId: GROK_LOCAL,
        membership: {memberId},
      },
    } as any)
    bot.onGrokMemberConnected({
      groupInfo: {groupId: GROK_LOCAL},
      member: {memberProfile: {displayName: "Bot"}},
    } as any)
  },

  async timesOut() {
    await vi.advanceTimersByTimeAsync(30_001)
  },

  wasRemoved(groupId = GROUP_ID) {
    const found = mainChat.removed.some(
      r => r.groupId === groupId && r.memberIds.includes(lastGrokMemberGId)
    )
    expect(found).toBe(true)
  },

  async leaves(groupId = GROUP_ID) {
    // Remove Grok from members list (simulates DB state after leave)
    const currentMembers = mainChat.members.get(groupId) || []
    mainChat.members.set(groupId, currentMembers.filter(m => m.groupMemberId !== lastGrokMemberGId))
    await bot.onLeftMember({
      groupInfo: businessGroupInfo(groupId),
      member: {memberId: "grok-1", groupMemberId: lastGrokMemberGId, memberContactId: 4},
    } as any)
  },
}


// ─── Constants ──────────────────────────────────────────────────

const TEAM_QUEUE_24H =
  `Your message is forwarded to the team. A reply may take up to 24 hours.\n\n` +
  `If your question is about SimpleX Chat, click /grok for an instant AI answer ` +
  `(non-sensitive questions only). Click /team to switch back any time.`

const TEAM_QUEUE_48H = TEAM_QUEUE_24H.replace("24 hours", "48 hours")

const GROK_ACTIVATED =
  `*You are now chatting with Grok. You can send questions in any language.* ` +
  `Your message(s) have been forwarded.\n` +
  `Send /team at any time to switch to a human team member.`

const TEAM_ADDED_24H =
  `A team member has been added and will reply within 24 hours. ` +
  `You can keep describing your issue — they will see the full conversation.`

const TEAM_ADDED_48H = TEAM_ADDED_24H.replace("24 hours", "48 hours")

const TEAM_LOCKED_MSG =
  `You are now in team mode. A team member will reply to your message.`

const GROK_UNAVAILABLE =
  `Grok is temporarily unavailable. Please try again or click /team for a team member.`

const TEAM_ADD_ERROR =
  `Sorry, there was an error adding a team member. Please try again.`


// ─── Setup ──────────────────────────────────────────────────────

const config = {
  teamGroup:     {id: 1, name: "SupportTeam"},
  teamMembers:   [{id: 2, name: "Bob"}],
  grokContactId: 4,
  timezone:      "America/New_York",
  groupLinks:    "https://simplex.chat/contact#...",
  grokApiKey:    "test-key",
  dbPrefix:      "./test-data/bot",
  grokDbPrefix:  "./test-data/grok",
}

beforeEach(() => {
  mainChat = new MockChatApi()
  grokChat = new MockChatApi()
  grokApi  = new MockGrokApi()
  mainChat.setNextGroupMemberId(50)
  lastTeamMemberGId = 50
  lastGrokMemberGId = 50
  nextChatItemId = 500
  // Simulate the welcome message that the platform auto-sends on business connect
  mainChat.setChatItems(GROUP_ID, [{chatDir: {type: "groupSnd"}, _text: "Welcome!"}])
  bot = new SupportBot(mainChat as any, grokChat as any, grokApi as any, config as any)
  vi.mocked(isWeekend).mockReturnValue(false)
})


// ─── State Setup Helpers ────────────────────────────────────────

// Reach teamQueue: customer sends first message → bot sends queue reply (groupSnd in DB)
async function reachTeamQueue(...messages: string[]) {
  await customer.sends(messages[0] || "Hello")
  for (const msg of messages.slice(1)) {
    await customer.sends(msg)
  }
}

// Reach grokMode: teamQueue → /grok → Grok joins → API responds
async function reachGrokMode(grokResponse = "Grok answer") {
  mainChat.setNextGroupMemberId(60)
  lastGrokMemberGId = 60
  await reachTeamQueue("Hello")
  grokApi.willRespond(grokResponse)
  const p = customer.sends("/grok")
  // After apiAddMember, register Grok as active member in the DB mock
  mainChat.setGroupMembers(GROUP_ID, [
    {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
  ])
  await grokAgent.joins()
  await p
}

// Reach teamPending: teamQueue → /team → team member added
async function reachTeamPending() {
  mainChat.setNextGroupMemberId(50)
  lastTeamMemberGId = 50
  await reachTeamQueue("Hello")
  // Before /team, ensure no special members
  mainChat.setGroupMembers(GROUP_ID, [])
  await customer.sends("/team")
  // After /team, team member is now in the group
  mainChat.setGroupMembers(GROUP_ID, [
    {groupMemberId: 50, memberContactId: 2, memberStatus: "connected"},
  ])
}

// Reach teamLocked: teamPending → team member sends message
async function reachTeamLocked() {
  await reachTeamPending()
  await teamMember.sends("I'll help you")
}


// ═══════════════════════════════════════════════════════════════
//  TESTS
// ═══════════════════════════════════════════════════════════════


// ─── 1. Connection & Welcome ────────────────────────────────────

describe("Connection & Welcome", () => {

  test("first message → forwarded to team, queue reply sent", async () => {
    // No prior bot messages → isFirstCustomerMessage returns true → welcome flow
    await customer.sends("How do I create a group?")

    teamGroup.received(fmtCustomer("How do I create a group?"))
    customer.received(TEAM_QUEUE_24H)
  })

  test("non-text message when no bot messages → ignored", async () => {
    await customer.sendsNonText()

    expect(mainChat.sent.length).toBe(0)
  })
})


// ─── 2. Team Queue ──────────────────────────────────────────────

describe("Team Queue", () => {

  test("additional messages forwarded to team, no second queue reply", async () => {
    await reachTeamQueue("First question")
    mainChat.sent = []

    await customer.sends("More details about my issue")

    teamGroup.received(fmtCustomer("More details about my issue"))
    // No queue message sent again — bot already sent a message (groupSnd in DB)
    expect(mainChat.sentTo(GROUP_ID).length).toBe(0)
  })

  test("non-text message in teamQueue → ignored", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    await customer.sendsNonText()

    expect(mainChat.sent.length).toBe(0)
  })

  test("unrecognized /command treated as normal text message", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    await customer.sends("/unknown")

    teamGroup.received(fmtCustomer("/unknown"))
  })
})


// ─── 3. Grok Activation ────────────────────────────────────────

describe("Grok Activation", () => {

  test("/grok → Grok invited, activated, API called, response sent", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("How do I create a group?")

    grokApi.willRespond("To create a group, go to Settings > New Group.")
    const p = customer.sends("/grok")
    // After invite, set Grok as active member in mock
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    grokAgent.wasInvited()
    customer.received(GROK_ACTIVATED)

    // Grok API called with empty history + accumulated message
    expect(grokApi.lastCall().history).toEqual([])
    expect(grokApi.lastCall().message).toBe("How do I create a group?")

    // Grok response sent via Grok identity
    customer.receivedFromGrok("To create a group, go to Settings > New Group.")
  })

  test("/grok with multiple accumulated messages → joined with newline", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Question about groups", "Also, how do I add members?")

    grokApi.willRespond("Here's how to do both...")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    expect(grokApi.lastCall().message).toBe(
      "Question about groups\nAlso, how do I add members?"
    )
    customer.receivedFromGrok("Here's how to do both...")
  })
})


// ─── 4. Grok Mode Conversation ─────────────────────────────────

describe("Grok Mode Conversation", () => {

  test("user messages forwarded to both Grok API and team group", async () => {
    await reachGrokMode("Initial answer")
    // Add the Grok response to chat items so history builds correctly
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {
        type: "groupRcv",
        groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4},
      },
      _text: "Initial answer",
    })
    mainChat.sent = []

    grokApi.willRespond("Follow-up answer from Grok")
    await customer.sends("What about encryption?")

    teamGroup.received(fmtCustomer("What about encryption?"))

    // History should include the initial exchange (from chat items in DB)
    const lastCall = grokApi.lastCall()
    expect(lastCall.history.length).toBeGreaterThanOrEqual(1)
    expect(lastCall.message).toBe("What about encryption?")

    customer.receivedFromGrok("Follow-up answer from Grok")
  })

  test("/grok in grokMode → silently ignored", async () => {
    await reachGrokMode()
    mainChat.sent = []
    grokApi.reset()

    await customer.sends("/grok")

    expect(mainChat.sent.length).toBe(0)
    expect(grokApi.callCount()).toBe(0)
  })

  test("non-text message in grokMode → ignored", async () => {
    await reachGrokMode()
    mainChat.sent = []
    grokApi.reset()

    await customer.sendsNonText()

    expect(mainChat.sent.length).toBe(0)
    expect(grokApi.callCount()).toBe(0)
  })
})


// ─── 5. Team Activation ────────────────────────────────────────

describe("Team Activation", () => {

  test("/team from teamQueue → team member invited, team added message", async () => {
    mainChat.setNextGroupMemberId(50)
    lastTeamMemberGId = 50
    await reachTeamQueue("Hello")
    mainChat.sent = []

    await customer.sends("/team")

    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)
  })

  test("/team from grokMode → Grok removed, team member added", async () => {
    await reachGrokMode()
    mainChat.setNextGroupMemberId(70)
    lastTeamMemberGId = 70
    mainChat.sent = []

    await customer.sends("/team")

    grokAgent.wasRemoved()
    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)
  })
})


// ─── 6. One-Way Gate ────────────────────────────────────────────

describe("One-Way Gate", () => {

  test("/grok in teamPending → 'team mode' reply", async () => {
    await reachTeamPending()
    mainChat.sent = []

    await customer.sends("/grok")

    customer.received(TEAM_LOCKED_MSG)
  })

  test("/grok in teamLocked → 'team mode' reply", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await customer.sends("/grok")

    customer.received(TEAM_LOCKED_MSG)
  })

  test("/team in teamPending → silently ignored", async () => {
    await reachTeamPending()
    mainChat.sent = []

    await customer.sends("/team")

    expect(mainChat.sentTo(GROUP_ID).length).toBe(0)
  })

  test("/team in teamLocked → silently ignored", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await customer.sends("/team")

    expect(mainChat.sentTo(GROUP_ID).length).toBe(0)
  })

  test("customer text in teamPending → no forwarding, no reply", async () => {
    await reachTeamPending()
    mainChat.sent = []

    await customer.sends("Here's more info about my issue")

    expect(mainChat.sentTo(GROUP_ID).length).toBe(0)
  })

  test("customer text in teamLocked → no forwarding, no reply", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await customer.sends("Thank you!")

    expect(mainChat.sentTo(GROUP_ID).length).toBe(0)
  })
})


// ─── 7. Gate Reversal vs Irreversibility ────────────────────────

describe("Gate Reversal vs Irreversibility", () => {

  test("team member leaves in teamPending → reverting to queue (no replacement)", async () => {
    await reachTeamPending()
    // Remove team member from mock members (simulates leave)
    mainChat.setGroupMembers(GROUP_ID, [])
    mainChat.added = []

    await teamMember.leaves()

    // No replacement added — teamPending revert means no action
    expect(mainChat.added.length).toBe(0)
  })

  test("after teamPending revert, /grok works again", async () => {
    await reachTeamPending()
    // Remove team member from mock members
    mainChat.setGroupMembers(GROUP_ID, [])
    await teamMember.leaves()

    // Now back in teamQueue equivalent — /grok should work
    mainChat.setNextGroupMemberId(61)
    lastGrokMemberGId = 61

    grokApi.willRespond("Grok is back")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 61, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    customer.receivedFromGrok("Grok is back")
  })

  test("team member leaves in teamLocked → replacement added", async () => {
    await reachTeamLocked()
    mainChat.added = []

    await teamMember.leaves()

    // Replacement team member invited
    expect(mainChat.added.length).toBe(1)
    expect(mainChat.added[0].contactId).toBe(2)
  })

  test("/grok still rejected after replacement in teamLocked", async () => {
    await reachTeamLocked()
    await teamMember.leaves()
    // Replacement added, set in members
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 51, memberContactId: 2, memberStatus: "connected"},
    ])
    mainChat.sent = []

    await customer.sends("/grok")

    customer.received(TEAM_LOCKED_MSG)
  })
})


// ─── 8. Member Leave & Cleanup ──────────────────────────────────

describe("Member Leave & Cleanup", () => {

  test("customer leaves → grok maps cleaned up", async () => {
    await reachTeamQueue("Hello")

    await customer.leaves()

    // No crash, grok maps cleaned
    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(false)
  })

  test("customer leaves in grokMode → grok maps cleaned", async () => {
    await reachGrokMode()

    await customer.leaves()

    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(false)
  })

  test("Grok leaves during grokMode → next customer message goes to teamQueue", async () => {
    await reachGrokMode()

    await grokAgent.leaves()
    mainChat.sent = []
    grokApi.reset()

    // Next customer message: no grok, no team → handleNoSpecialMembers → teamQueue
    // Bot has already sent messages (groupSnd), so not welcome → forward to team
    await customer.sends("Another question")

    teamGroup.received(fmtCustomer("Another question"))
    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(false)
  })

  test("bot removed from group → no crash", async () => {
    // onDeletedMemberUser no longer exists — just verify no crash
    // The bot simply won't receive events for that group anymore
  })

  test("customer leaves in welcome → no crash", async () => {
    // No prior messages sent — just leave
    await customer.leaves()
    // No crash expected
  })
})


// ─── 9. Error Handling ──────────────────────────────────────────

describe("Error Handling", () => {

  test("Grok invitation (apiAddMember) fails → error msg, stays in queue", async () => {
    await reachTeamQueue("Hello")
    mainChat.apiAddMemberWillFail()
    mainChat.sent = []

    await customer.sends("/grok")

    customer.received(GROK_UNAVAILABLE)
    expect(grokApi.callCount()).toBe(0)
  })

  test("Grok join timeout → error msg", async () => {
    vi.useFakeTimers()
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")
    mainChat.sent = []

    const sendPromise = customer.sends("/grok")
    await grokAgent.timesOut()
    await sendPromise

    customer.received(GROK_UNAVAILABLE)
    expect(grokApi.callCount()).toBe(0)
    vi.useRealTimers()
  })

  test("Grok API error during activation → remove Grok, error msg", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")
    grokApi.willFail()
    mainChat.sent = []

    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    grokAgent.wasRemoved()
    customer.received(GROK_UNAVAILABLE)
  })

  test("Grok API error during conversation → remove Grok, error msg", async () => {
    await reachGrokMode()
    grokApi.willFail()
    mainChat.sent = []

    await customer.sends("Another question")

    grokAgent.wasRemoved()
    customer.received(GROK_UNAVAILABLE)
  })

  test("after Grok API failure revert, /team still works", async () => {
    await reachGrokMode()
    grokApi.willFail()
    await customer.sends("Failing question")
    // After Grok removal, members list should be empty
    mainChat.setGroupMembers(GROUP_ID, [])
    mainChat.setNextGroupMemberId(51)
    lastTeamMemberGId = 51
    mainChat.sent = []

    await customer.sends("/team")

    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)
  })

  test("team member add fails from teamQueue → error, stays in queue", async () => {
    await reachTeamQueue("Hello")
    mainChat.apiAddMemberWillFail()
    mainChat.sent = []

    await customer.sends("/team")

    customer.received(TEAM_ADD_ERROR)
  })

  test("team member add fails after Grok removal → error msg", async () => {
    await reachGrokMode()
    mainChat.apiAddMemberWillFail()
    mainChat.sent = []

    await customer.sends("/team")

    grokAgent.wasRemoved()
    customer.received(TEAM_ADD_ERROR)
  })

  test("Grok failure then retry succeeds", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")

    // First attempt — API fails
    grokApi.willFail()
    const p1 = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p1
    // After failure, Grok removed from members
    mainChat.setGroupMembers(GROUP_ID, [])

    // Second attempt — succeeds
    mainChat.setNextGroupMemberId(61)
    lastGrokMemberGId = 61
    grokApi.willRespond("Hello! How can I help?")
    const p2 = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 61, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p2

    customer.receivedFromGrok("Hello! How can I help?")
  })
})


// ─── 10. Race Conditions ────────────────────────────────────────

describe("Race Conditions", () => {

  test("/team sent while waiting for Grok to join → abort Grok", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")

    // Start /grok — hangs on waitForGrokJoin
    grokApi.willRespond("answer")
    const grokPromise = customer.sends("/grok")
    // Flush microtasks so activateGrok reaches waitForGrokJoin before we change nextMemberGId
    await new Promise<void>(r => setTimeout(r, 0))

    // While waiting, /team is processed concurrently (no special members yet)
    mainChat.setNextGroupMemberId(70)
    lastTeamMemberGId = 70
    mainChat.setGroupMembers(GROUP_ID, [])
    await customer.sends("/team")
    customer.received(TEAM_ADDED_24H)

    // After /team, team member is now in the group
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 70, memberContactId: 2, memberStatus: "connected"},
    ])

    // Grok join completes — but team member is now present
    await grokAgent.joins()
    await grokPromise

    // Bot detects team member, removes Grok
    grokAgent.wasRemoved()
    expect(grokApi.callCount()).toBe(0)
  })

  test("state change during Grok API call → abort", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")

    // Make grokApi.chat return a controllable promise
    let resolveGrokCall!: (v: string) => void
    grokApi.chat = async () => new Promise<string>(r => { resolveGrokCall = r })

    const grokPromise = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    // Flush microtasks so activateGrok proceeds past waitForGrokJoin into grokApi.chat
    await new Promise<void>(r => setTimeout(r, 0))

    // While API call is pending, /team changes composition
    mainChat.setNextGroupMemberId(70)
    lastTeamMemberGId = 70
    // Update members to include team member (Grok still there from DB perspective)
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
      {groupMemberId: 70, memberContactId: 2, memberStatus: "connected"},
    ])
    await customer.sends("/team")

    // API call completes — but team member appeared
    resolveGrokCall("Grok answer")
    await grokPromise

    grokAgent.wasRemoved()
  })
})


// ─── 11. Weekend Hours ──────────────────────────────────────────

describe("Weekend Hours", () => {

  test("weekend: 48 hours in queue message", async () => {
    vi.mocked(isWeekend).mockReturnValue(true)

    await customer.sends("Hello")

    customer.received(TEAM_QUEUE_48H)
  })

  test("weekend: 48 hours in team added message", async () => {
    vi.mocked(isWeekend).mockReturnValue(true)

    await reachTeamQueue("Hello")
    await customer.sends("/team")

    customer.received(TEAM_ADDED_48H)
  })
})


// ─── 12. Team Forwarding Format ─────────────────────────────────

describe("Team Forwarding", () => {

  test("format: CustomerName:groupId: text", async () => {
    await customer.sends("My app crashes on startup")

    teamGroup.received(fmtCustomer("My app crashes on startup"))
  })

  test("grokMode messages also forwarded to team", async () => {
    await reachGrokMode()
    mainChat.sent = []

    grokApi.willRespond("Try clearing app data")
    await customer.sends("App keeps crashing")

    teamGroup.received(fmtCustomer("App keeps crashing"))
    customer.receivedFromGrok("Try clearing app data")
  })

  test("fallback displayName when empty → group-{id}", async () => {
    const emptyNameGroup = {...businessGroupInfo(101), groupProfile: {displayName: ""}}
    mainChat.sent = []

    const ci = customerChatItem("Hello", null)
    ci.chatInfo.groupInfo = emptyNameGroup
    ci.chatItem.chatDir.groupMember.memberId = emptyNameGroup.businessChat.customerId
    // No prior bot messages for group 101 → welcome flow
    await bot.onNewChatItems({chatItems: [ci]} as any)

    teamGroup.received(fmtCustomer("Hello", "group-101", 101))
  })
})


// ─── 13. Edge Cases ─────────────────────────────────────────────

describe("Edge Cases", () => {

  test("bot's own messages (groupSnd) → ignored", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [botOwnChatItem("queue reply")]} as any)

    expect(mainChat.sent.length).toBe(0)
  })

  test("non-business-chat group → ignored", async () => {
    const nonBizGroup = {
      groupId: 999,
      groupProfile: {displayName: "Random"},
      businessChat: undefined,
    }
    const ci = {
      chatInfo: {type: "group", groupInfo: nonBizGroup},
      chatItem: {chatDir: {type: "groupRcv", groupMember: {memberId: "x"}}, _text: "hi"},
    } as any

    await bot.onNewChatItems({chatItems: [ci]} as any)

    expect(mainChat.sent.length).toBe(0)
  })

  test("message in business chat after restart → correctly handled", async () => {
    // Simulate restart: no prior state. Bot has already sent messages (we simulate groupSnd in DB)
    mainChat.setChatItems(888, [
      {chatDir: {type: "groupSnd"}, _text: "Welcome!"},
      {chatDir: {type: "groupSnd"}, _text: "Your message is forwarded to the team."},
    ])
    mainChat.sent = []

    const ci = customerChatItem("I had a question earlier", null)
    ci.chatInfo.groupInfo = businessGroupInfo(888)
    // Track customer message in mock
    mainChat.chatItems.get(888)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "I had a question earlier",
    })
    await bot.onNewChatItems({chatItems: [ci]} as any)

    // Handled as teamQueue (not welcome, since bot already has groupSnd), forwarded to team
    teamGroup.received(fmtCustomer("I had a question earlier", "Alice", 888))
  })

  test("Grok's own messages in grokMode → ignored by bot (non-customer)", async () => {
    await reachGrokMode()
    mainChat.sent = []
    grokApi.reset()

    const ci = grokMemberChatItem(lastGrokMemberGId, "Grok's response text")
    await bot.onNewChatItems({chatItems: [ci]} as any)

    expect(grokApi.callCount()).toBe(0)
    expect(mainChat.sent.length).toBe(0)
  })

  test("unexpected Grok group invitation → ignored", async () => {
    await bot.onGrokGroupInvitation({
      groupInfo: {
        groupId: 999,
        membership: {memberId: "unknown-member"},
      },
    } as any)

    expect(grokChat.joined.length).toBe(0)
  })

  test("multiple concurrent conversations are independent", async () => {
    const GROUP_A = 100
    const GROUP_B = 300

    // Customer A sends message → welcome → teamQueue
    const ciA = customerChatItem("Question A", null)
    ciA.chatInfo.groupInfo = businessGroupInfo(GROUP_A, "Alice")
    mainChat.chatItems.set(GROUP_A, [{
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "Question A",
    }])
    await bot.onNewChatItems({chatItems: [ciA]} as any)

    // Customer A got queue reply
    customer.received(TEAM_QUEUE_24H, GROUP_A)

    // Customer B's first message in group 300
    const ciB = customerChatItem("Question B", null)
    ciB.chatInfo.groupInfo = businessGroupInfo(GROUP_B, "Charlie")
    ciB.chatItem.chatDir.groupMember.memberId = CUSTOMER_ID
    mainChat.chatItems.set(GROUP_B, [{
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "Question B",
    }])
    await bot.onNewChatItems({chatItems: [ciB]} as any)

    // Customer B also got queue reply
    customer.received(TEAM_QUEUE_24H, GROUP_B)
  })

  test("Grok leaves during grokMode, customer retries → works", async () => {
    await reachGrokMode()

    await grokAgent.leaves()

    // Retry /grok
    mainChat.setNextGroupMemberId(62)
    lastGrokMemberGId = 62
    grokApi.willRespond("I'm back!")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 62, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    customer.receivedFromGrok("I'm back!")
  })

  test("/grok as first message → treated as text (welcome state)", async () => {
    await customer.sends("/grok")

    // In welcome state, /grok is treated as a regular text message
    teamGroup.received(fmtCustomer("/grok"))
    customer.received(TEAM_QUEUE_24H)
  })

  test("/team as first message → treated as text (welcome state)", async () => {
    await customer.sends("/team")

    teamGroup.received(fmtCustomer("/team"))
    customer.received(TEAM_QUEUE_24H)
  })

  test("non-text message in teamPending → ignored", async () => {
    await reachTeamPending()
    mainChat.sent = []

    await customer.sendsNonText()

    expect(mainChat.sent.length).toBe(0)
  })

  test("non-text message in teamLocked → ignored", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await customer.sendsNonText()

    expect(mainChat.sent.length).toBe(0)
  })

  test("unknown member message → silently ignored", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []
    grokApi.reset()

    const ci = {
      chatInfo: {type: "group", groupInfo: businessGroupInfo()},
      chatItem: {
        chatDir: {
          type: "groupRcv",
          groupMember: {memberId: "unknown-1", groupMemberId: 999},
        },
        content: {type: "text", text: "Who am I?"},
        _text: "Who am I?",
      },
    } as any
    await bot.onNewChatItems({chatItems: [ci]} as any)

    expect(mainChat.sent.length).toBe(0)
    expect(grokApi.callCount()).toBe(0)
  })

  test("Grok apiJoinGroup failure → maps not set", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")

    // Make apiJoinGroup fail
    grokChat.apiJoinGroup = async () => { throw new Error("join failed") }

    grokApi.willRespond("answer")
    const p = customer.sends("/grok")

    await new Promise<void>(r => setTimeout(r, 0))
    const memberId = `member-${lastGrokMemberGId}`
    await bot.onGrokGroupInvitation({
      groupInfo: {groupId: GROK_LOCAL, membership: {memberId}},
    } as any)

    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(false)
    expect((bot as any).reverseGrokMap.has(GROK_LOCAL)).toBe(false)
  })

  test("replacement team member add fails → still in team mode", async () => {
    await reachTeamLocked()
    mainChat.apiAddMemberWillFail()

    await teamMember.leaves()

    // addReplacementTeamMember failed, but team mode continues
    // (next time a message arrives and no team member is found, it will be teamQueue)
  })

  test("/grok with null grokContactId → unavailable message", async () => {
    const nullGrokConfig = {...config, grokContactId: null}
    const nullBot = new SupportBot(mainChat as any, grokChat as any, grokApi as any, nullGrokConfig as any)
    // Send first message to move past welcome (welcome groupSnd already in mock from beforeEach)
    const ci1 = customerChatItem("Hello", null)
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "Hello",
    })
    await nullBot.onNewChatItems({chatItems: [ci1]} as any)
    mainChat.sent = []

    const grokCi = customerChatItem("/grok", "grok")
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "/grok",
      _botCommand: "grok",
    })
    await nullBot.onNewChatItems({chatItems: [grokCi]} as any)

    const msgs = mainChat.sentTo(GROUP_ID)
    expect(msgs).toContain(GROK_UNAVAILABLE)
  })

  test("/team with empty teamMembers → unavailable message", async () => {
    const noTeamConfig = {...config, teamMembers: []}
    const noTeamBot = new SupportBot(mainChat as any, grokChat as any, grokApi as any, noTeamConfig as any)
    // Send first message to move past welcome (welcome groupSnd already in mock from beforeEach)
    const ci1 = customerChatItem("Hello", null)
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "Hello",
    })
    await noTeamBot.onNewChatItems({chatItems: [ci1]} as any)
    mainChat.sent = []

    const teamCi = customerChatItem("/team", "team")
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "/team",
      _botCommand: "team",
    })
    await noTeamBot.onNewChatItems({chatItems: [teamCi]} as any)

    const msgs = mainChat.sentTo(GROUP_ID)
    expect(msgs).toContain("No team members are available yet. Please try again later or click /grok.")
  })
})


// ─── 14. Full End-to-End Flows ──────────────────────────────────

describe("End-to-End Flows", () => {

  test("full flow: welcome → grokMode → /team → teamLocked", async () => {
    // Step 1: first message → teamQueue
    await customer.sends("How do I enable disappearing messages?")
    teamGroup.received(fmtCustomer("How do I enable disappearing messages?"))
    customer.received(TEAM_QUEUE_24H)

    // Step 2: /grok → grokMode
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    grokApi.willRespond("Go to conversation settings and tap 'Disappearing messages'.")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p
    customer.received(GROK_ACTIVATED)
    customer.receivedFromGrok("Go to conversation settings and tap 'Disappearing messages'.")

    // Step 3: follow-up in grokMode
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4}},
      _text: "Go to conversation settings and tap 'Disappearing messages'.",
    })
    grokApi.willRespond("Yes, you can set different timers per conversation.")
    await customer.sends("Can I set different timers?")
    teamGroup.received(fmtCustomer("Can I set different timers?"))
    customer.receivedFromGrok("Yes, you can set different timers per conversation.")

    // Step 4: /team → team added (Grok removed)
    mainChat.setNextGroupMemberId(70)
    lastTeamMemberGId = 70
    await customer.sends("/team")
    grokAgent.wasRemoved()
    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)

    // Update members: Grok gone, team member present
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 70, memberContactId: 2, memberStatus: "connected"},
    ])

    // Step 5: /grok rejected
    await customer.sends("/grok")
    customer.received(TEAM_LOCKED_MSG)

    // Step 6: team member responds (the message in DB is the state change)
    await teamMember.sends("Hi! Let me help you.")

    // Step 7: /grok still rejected
    await customer.sends("/grok")
    customer.received(TEAM_LOCKED_MSG)

    // Step 8: customer continues — team sees directly, no forwarding
    mainChat.sent = []
    await customer.sends("Thanks for helping!")
    expect(mainChat.sentTo(GROUP_ID).length).toBe(0)
  })

  test("full flow: welcome → teamQueue → /team directly (skip Grok)", async () => {
    await customer.sends("I have a billing question")
    customer.received(TEAM_QUEUE_24H)

    mainChat.setNextGroupMemberId(50)
    lastTeamMemberGId = 50
    await customer.sends("/team")
    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)

    // Team member is now present
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 50, memberContactId: 2, memberStatus: "connected"},
    ])

    await teamMember.sends("Hi, I can help with billing")
    // Team member sent a message, now in "teamLocked" equivalent
    // /grok should be rejected
    await customer.sends("/grok")
    customer.received(TEAM_LOCKED_MSG)
  })
})


// ─── 15. Restart Recovery ───────────────────────────────────────

describe("Restart Recovery", () => {

  test("after restart, customer message with prior bot messages → forward as teamQueue", async () => {
    // Simulate restart: bot has previously sent messages (welcome + queue reply in DB)
    mainChat.setChatItems(777, [
      {chatDir: {type: "groupSnd"}, _text: "Welcome!"},
      {chatDir: {type: "groupSnd"}, _text: "Your message is forwarded to the team."},
    ])
    mainChat.sent = []

    const ci = customerChatItem("I had a question earlier", null)
    ci.chatInfo.groupInfo = businessGroupInfo(777)
    mainChat.chatItems.get(777)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "I had a question earlier",
    })
    await bot.onNewChatItems({chatItems: [ci]} as any)

    // Treated as teamQueue (not welcome), message forwarded to team
    teamGroup.received(fmtCustomer("I had a question earlier", "Alice", 777))
  })

  test("after restart, /grok works in recovered group", async () => {
    // Simulate restart with existing bot messages (welcome + queue reply)
    mainChat.setChatItems(777, [
      {chatDir: {type: "groupSnd"}, _text: "Welcome!"},
      {chatDir: {type: "groupSnd"}, _text: "Your message is forwarded to the team."},
    ])

    // Send /grok
    mainChat.setNextGroupMemberId(80)
    lastGrokMemberGId = 80
    grokApi.willRespond("Grok answer")
    const grokCi = customerChatItem("/grok", "grok")
    grokCi.chatInfo.groupInfo = businessGroupInfo(777)
    mainChat.chatItems.get(777)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "/grok",
      _botCommand: "grok",
    })
    const p = bot.onNewChatItems({chatItems: [grokCi]} as any)
    // Grok joins
    mainChat.setGroupMembers(777, [
      {groupMemberId: 80, memberContactId: 4, memberStatus: "connected"},
    ])
    await new Promise<void>(r => setTimeout(r, 0))
    const memberId = `member-${lastGrokMemberGId}`
    await bot.onGrokGroupInvitation({
      groupInfo: {groupId: GROK_LOCAL, membership: {memberId}},
    } as any)
    bot.onGrokMemberConnected({
      groupInfo: {groupId: GROK_LOCAL},
      member: {memberProfile: {displayName: "Bot"}},
    } as any)
    await p

    customer.receivedFromGrok("Grok answer")
  })
})


// ─── 16. Grok connectedToGroupMember ───────────────────────────

describe("Grok connectedToGroupMember", () => {

  test("waiter not resolved by onGrokGroupInvitation alone", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")
    grokApi.willRespond("answer")

    const p = customer.sends("/grok")

    // Only fire invitation (no connectedToGroupMember) — waiter should NOT resolve
    await new Promise<void>(r => setTimeout(r, 0))
    const memberId = `member-${lastGrokMemberGId}`
    await bot.onGrokGroupInvitation({
      groupInfo: {groupId: GROK_LOCAL, membership: {memberId}},
    } as any)

    // Maps set but waiter not resolved
    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(true)

    // Now fire connectedToGroupMember → waiter resolves
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    bot.onGrokMemberConnected({
      groupInfo: {groupId: GROK_LOCAL},
      member: {memberProfile: {displayName: "Bot"}},
    } as any)
    await p

    // Grok activated successfully
    customer.receivedFromGrok("answer")
  })

  test("onGrokMemberConnected for unknown group → ignored", () => {
    bot.onGrokMemberConnected({
      groupInfo: {groupId: 9999},
      member: {memberProfile: {displayName: "Someone"}},
    } as any)
  })
})


// ─── 17. groupDuplicateMember Handling ─────────────────────────

describe("groupDuplicateMember Handling", () => {

  test("/team with duplicate member already present → team mode (no message needed)", async () => {
    await reachTeamQueue("Hello")
    // Team member is already in the group (from previous session)
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 42, memberContactId: 2, memberStatus: "connected"},
    ])
    mainChat.sent = []

    await customer.sends("/team")

    // Bot sees team member via getGroupComposition → handleTeamMode → /team ignored
    // No message sent — team member is already present
    expect(mainChat.sentTo(GROUP_ID).length).toBe(0)
  })

  test("/team with duplicate but member not found in list → error message", async () => {
    await reachTeamQueue("Hello")
    mainChat.apiAddMemberWillDuplicate()
    mainChat.setGroupMembers(GROUP_ID, [])  // empty — member not found
    mainChat.sent = []

    await customer.sends("/team")

    customer.received(TEAM_ADD_ERROR)
  })

  test("replacement team member with duplicate → finds existing", async () => {
    await reachTeamLocked()
    mainChat.apiAddMemberWillDuplicate()
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 99, memberContactId: 2, memberStatus: "connected"},
    ])

    await teamMember.leaves()

    // No error — replacement found via duplicate handling
    expect(mainChat.added.length).toBeGreaterThanOrEqual(1)
  })
})


// ─── 18. DM Contact Received ───────────────────────────────────

describe("DM Contact Received", () => {

  test("onMemberContactReceivedInv from team group → no crash", () => {
    bot.onMemberContactReceivedInv({
      contact: {contactId: 10},
      groupInfo: {groupId: TEAM_GRP_ID},
      member: {memberProfile: {displayName: "TeamGuy"}},
    } as any)
  })

  test("onMemberContactReceivedInv from non-team group → no crash", () => {
    bot.onMemberContactReceivedInv({
      contact: {contactId: 11},
      groupInfo: {groupId: 999},
      member: {memberProfile: {displayName: "Stranger"}},
    } as any)
  })
})


// ─── 19. Business Request — Media Upload ─────────────────────

describe("Business Request — Media Upload", () => {

  test("onBusinessRequest enables files preference on group", async () => {
    await bot.onBusinessRequest({
      user: {},
      groupInfo: {
        groupId: 400,
        groupProfile: {displayName: "NewCustomer", fullName: "", groupPreferences: {directMessages: {enable: "on"}}},
        businessChat: {customerId: "new-cust"},
      },
    } as any)

    expect(mainChat.updatedProfiles.length).toBe(1)
    expect(mainChat.updatedProfiles[0].groupId).toBe(400)
    expect(mainChat.updatedProfiles[0].profile.groupPreferences.files).toEqual({enable: "on"})
    // Preserves existing preferences
    expect(mainChat.updatedProfiles[0].profile.groupPreferences.directMessages).toEqual({enable: "on"})
  })

  test("onBusinessRequest with no existing preferences → still sets files", async () => {
    await bot.onBusinessRequest({
      user: {},
      groupInfo: {
        groupId: 401,
        groupProfile: {displayName: "Another", fullName: ""},
        businessChat: {customerId: "cust-2"},
      },
    } as any)

    expect(mainChat.updatedProfiles.length).toBe(1)
    expect(mainChat.updatedProfiles[0].profile.groupPreferences.files).toEqual({enable: "on"})
  })
})


// ─── 20. Edit Forwarding ────────────────────────────────────

describe("Edit Forwarding", () => {

  test("customer edits forwarded message → team group message updated", async () => {
    // Send first message → forwarded to team (stores mapping)
    await customer.sends("Original question")
    // The customer chat item had itemId=500, the forwarded team msg got itemId=1000
    mainChat.sent = []

    // Simulate edit event
    await bot.onChatItemUpdated({
      chatItem: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatItem: {
          chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
          meta: {itemId: 500},
          content: {type: "text", text: "Edited question"},
          _text: "Edited question",
        },
      },
    } as any)

    expect(mainChat.updatedChatItems.length).toBe(1)
    expect(mainChat.updatedChatItems[0].chatId).toBe(TEAM_GRP_ID)
    expect(mainChat.updatedChatItems[0].chatItemId).toBe(1000)
    expect(mainChat.updatedChatItems[0].msgContent).toEqual({type: "text", text: fmtCustomer("Edited question")})
  })

  test("team member edits forwarded message → team group message updated", async () => {
    await reachTeamPending()
    // After reachTeamPending: nextChatItemId=502, nextItemId=1004
    // Team member sends → itemId=502, forwarded teamItemId=1004
    await teamMember.sends("I'll help you")
    mainChat.updatedChatItems = []

    // Team member edits their message
    await bot.onChatItemUpdated({
      chatItem: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatItem: {
          chatDir: {
            type: "groupRcv",
            groupMember: {memberId: "team-member-1", groupMemberId: lastTeamMemberGId, memberContactId: 2},
          },
          meta: {itemId: 502},
          content: {type: "text", text: "Actually, let me rephrase"},
          _text: "Actually, let me rephrase",
        },
      },
    } as any)

    expect(mainChat.updatedChatItems.length).toBe(1)
    expect(mainChat.updatedChatItems[0].chatId).toBe(TEAM_GRP_ID)
    expect(mainChat.updatedChatItems[0].chatItemId).toBe(1004)
    expect(mainChat.updatedChatItems[0].msgContent).toEqual({type: "text", text: fmtTeamMember(2, "Actually, let me rephrase")})
  })

  test("edit for non-forwarded message → ignored", async () => {
    await bot.onChatItemUpdated({
      chatItem: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatItem: {
          chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
          meta: {itemId: 9999},  // no forwarded mapping
          content: {type: "text", text: "Some edit"},
          _text: "Some edit",
        },
      },
    } as any)

    expect(mainChat.updatedChatItems.length).toBe(0)
  })

  test("edit in non-business-chat group → ignored", async () => {
    await bot.onChatItemUpdated({
      chatItem: {
        chatInfo: {type: "group", groupInfo: {groupId: 999, groupProfile: {displayName: "X"}}},
        chatItem: {
          chatDir: {type: "groupRcv", groupMember: {memberId: "x"}},
          meta: {itemId: 1},
          content: {type: "text", text: "edit"},
          _text: "edit",
        },
      },
    } as any)

    expect(mainChat.updatedChatItems.length).toBe(0)
  })

  test("edit of groupSnd message → ignored", async () => {
    await bot.onChatItemUpdated({
      chatItem: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatItem: {
          chatDir: {type: "groupSnd"},
          meta: {itemId: 1},
          content: {type: "text", text: "edit"},
          _text: "edit",
        },
      },
    } as any)

    expect(mainChat.updatedChatItems.length).toBe(0)
  })

  test("customer edit in grokMode → team group message updated", async () => {
    await reachGrokMode("Initial answer")

    // Customer sends a text message in grokMode (forwarded to team)
    grokApi.willRespond("Follow-up answer")
    await customer.sends("My question about encryption")
    // customerChatItem itemId=502, forwarded to team as itemId=1004
    mainChat.updatedChatItems = []

    // Customer edits the message
    await bot.onChatItemUpdated({
      chatItem: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatItem: {
          chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
          meta: {itemId: 502},
          content: {type: "text", text: "Edited encryption question"},
          _text: "Edited encryption question",
        },
      },
    } as any)

    expect(mainChat.updatedChatItems.length).toBe(1)
    expect(mainChat.updatedChatItems[0].chatId).toBe(TEAM_GRP_ID)
    expect(mainChat.updatedChatItems[0].chatItemId).toBe(1004)
    expect(mainChat.updatedChatItems[0].msgContent).toEqual({type: "text", text: fmtCustomer("Edited encryption question")})
  })

  test("edit with null text → ignored", async () => {
    await customer.sends("Original message")
    // customerChatItem itemId=500, forwarded to team as itemId=1000
    mainChat.updatedChatItems = []

    await bot.onChatItemUpdated({
      chatItem: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatItem: {
          chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
          meta: {itemId: 500},
          content: {type: "text", text: ""},
          _text: null,
        },
      },
    } as any)

    expect(mainChat.updatedChatItems.length).toBe(0)
  })
})


// ─── 21. Team Member Reply Forwarding ────────────────────────

describe("Team Member Reply Forwarding", () => {

  test("team member message → forwarded to team group", async () => {
    await reachTeamPending()
    mainChat.sent = []

    await teamMember.sends("I'll help you with this")

    teamGroup.received(fmtTeamMember(2, "I'll help you with this"))
  })

  test("team member message in teamLocked → forwarded to team group", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await teamMember.sends("Here is the solution")

    teamGroup.received(fmtTeamMember(2, "Here is the solution"))
  })

  test("Grok message → not forwarded to team group", async () => {
    await reachGrokMode()
    mainChat.sent = []
    grokApi.reset()

    const ci = grokMemberChatItem(lastGrokMemberGId, "Grok's response")
    await bot.onNewChatItems({chatItems: [ci]} as any)

    // Grok is not a team member — should not forward
    teamGroup.receivedNothing()
  })

  test("unknown member message → not forwarded to team group", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    const ci = {
      chatInfo: {type: "group", groupInfo: businessGroupInfo()},
      chatItem: {
        chatDir: {
          type: "groupRcv",
          groupMember: {memberId: "unknown-1", groupMemberId: 999, memberContactId: 99},
        },
        meta: {itemId: 800},
        content: {type: "text", text: "Who am I?"},
        _text: "Who am I?",
      },
    } as any
    await bot.onNewChatItems({chatItems: [ci]} as any)

    teamGroup.receivedNothing()
  })
})


// ─── 22. Grok Group Map Persistence ────────────────────────────

describe("Grok Group Map Persistence", () => {

  test("restoreGrokGroupMap correctly restores maps", () => {
    bot.restoreGrokGroupMap([[GROUP_ID, GROK_LOCAL]])

    expect((bot as any).grokGroupMap.get(GROUP_ID)).toBe(GROK_LOCAL)
    expect((bot as any).reverseGrokMap.get(GROK_LOCAL)).toBe(GROUP_ID)
  })

  test("after restore, Grok responds to customer messages", async () => {
    bot.restoreGrokGroupMap([[GROUP_ID, GROK_LOCAL]])
    lastGrokMemberGId = 60
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    mainChat.sent = []
    grokApi.willRespond("Here is the answer about encryption")

    await customer.sends("How does encryption work?")

    // Grok API called with history from DB
    expect(grokApi.callCount()).toBe(1)
    expect(grokApi.lastCall().message).toBe("How does encryption work?")

    // Response sent via grokChat to GROK_LOCAL
    customer.receivedFromGrok("Here is the answer about encryption")

    // Also forwarded to team group
    teamGroup.received(fmtCustomer("How does encryption work?"))
  })

  test("onGrokMapChanged fires on Grok join", async () => {
    const callback = vi.fn()
    bot.onGrokMapChanged = callback

    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")

    grokApi.willRespond("Answer")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    expect(callback).toHaveBeenCalled()
    const lastCallArg = callback.mock.calls[callback.mock.calls.length - 1][0]
    expect(lastCallArg.get(GROUP_ID)).toBe(GROK_LOCAL)
  })

  test("onGrokMapChanged fires on cleanup (customer leaves)", async () => {
    const callback = vi.fn()
    await reachGrokMode()
    bot.onGrokMapChanged = callback

    await customer.leaves()

    expect(callback).toHaveBeenCalled()
    const lastCallArg = callback.mock.calls[callback.mock.calls.length - 1][0]
    expect(lastCallArg.has(GROUP_ID)).toBe(false)
  })
})


// ─── 23. /add Command ─────────────────────────────────────────

describe("/add Command", () => {

  test("first customer message → /add command sent to team group", async () => {
    await customer.sends("Hello, I need help")

    // Team group receives forwarded message + /add command
    teamGroup.received(fmtCustomer("Hello, I need help"))
    teamGroup.received(`/add ${GROUP_ID}:Alice`)
  })

  test("/add command uses quotes when name has spaces", async () => {
    const spacedGroup = {
      ...businessGroupInfo(101, "Alice Smith"),
      groupProfile: {displayName: "Alice Smith"},
      businessChat: {customerId: CUSTOMER_ID},
    }
    mainChat.setChatItems(101, [{chatDir: {type: "groupSnd"}, _text: "Welcome!"}])
    const ci = customerChatItem("Hello", null)
    ci.chatInfo.groupInfo = spacedGroup
    mainChat.chatItems.get(101)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "Hello",
    })
    await bot.onNewChatItems({chatItems: [ci]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    expect(teamMsgs).toContain(`/add 101:'Alice Smith'`)
  })

  test("/add not sent on subsequent messages (teamQueue)", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    await customer.sends("More details")

    // Only the forwarded message, no /add
    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    expect(teamMsgs).toEqual([fmtCustomer("More details")])
  })

  test("team member sends /add → invited to customer group", async () => {
    // Simulate team member sending /add command in admin group
    const ci = {
      chatInfo: {type: "group", groupInfo: {groupId: TEAM_GRP_ID, groupProfile: {displayName: "SupportTeam"}}},
      chatItem: {
        chatDir: {
          type: "groupRcv",
          groupMember: {memberId: "tm-1", groupMemberId: 30, memberContactId: 2, memberProfile: {displayName: "Bob"}},
        },
        meta: {itemId: 900},
        content: {type: "text", text: `/add ${GROUP_ID}:Alice`},
        _text: `/add ${GROUP_ID}:Alice`,
      },
    } as any
    await bot.onNewChatItems({chatItems: [ci]} as any)

    // Team member (contactId=2) invited to the customer group
    const added = mainChat.added.find(a => a.groupId === GROUP_ID && a.contactId === 2)
    expect(added).toBeDefined()
  })

  test("team member sends /add with quoted name → invited", async () => {
    const ci = {
      chatInfo: {type: "group", groupInfo: {groupId: TEAM_GRP_ID, groupProfile: {displayName: "SupportTeam"}}},
      chatItem: {
        chatDir: {
          type: "groupRcv",
          groupMember: {memberId: "tm-1", groupMemberId: 30, memberContactId: 2, memberProfile: {displayName: "Bob"}},
        },
        meta: {itemId: 901},
        content: {type: "text", text: `/add 101:'Alice Smith'`},
        _text: `/add 101:'Alice Smith'`,
      },
    } as any
    await bot.onNewChatItems({chatItems: [ci]} as any)

    const added = mainChat.added.find(a => a.groupId === 101 && a.contactId === 2)
    expect(added).toBeDefined()
  })

  test("non-/add message in team group → ignored", async () => {
    const ci = {
      chatInfo: {type: "group", groupInfo: {groupId: TEAM_GRP_ID, groupProfile: {displayName: "SupportTeam"}}},
      chatItem: {
        chatDir: {
          type: "groupRcv",
          groupMember: {memberId: "tm-1", groupMemberId: 30, memberContactId: 2, memberProfile: {displayName: "Bob"}},
        },
        meta: {itemId: 902},
        content: {type: "text", text: "Just chatting"},
        _text: "Just chatting",
      },
    } as any
    mainChat.added = []
    await bot.onNewChatItems({chatItems: [ci]} as any)

    expect(mainChat.added.length).toBe(0)
  })

  test("bot's own /add message in team group → ignored (groupSnd)", async () => {
    const ci = {
      chatInfo: {type: "group", groupInfo: {groupId: TEAM_GRP_ID, groupProfile: {displayName: "SupportTeam"}}},
      chatItem: {
        chatDir: {type: "groupSnd"},
        meta: {itemId: 903},
        content: {type: "text", text: `/add ${GROUP_ID}:Alice`},
        _text: `/add ${GROUP_ID}:Alice`,
      },
    } as any
    mainChat.added = []
    await bot.onNewChatItems({chatItems: [ci]} as any)

    expect(mainChat.added.length).toBe(0)
  })
})


// ─── 24. Grok System Prompt ──────────────────────────────────

describe("Grok System Prompt", () => {

  let capturedBody: any

  beforeEach(() => {
    capturedBody = null
    vi.stubGlobal("fetch", vi.fn(async (_url: string, opts: any) => {
      capturedBody = JSON.parse(opts.body)
      return {
        ok: true,
        json: async () => ({choices: [{message: {content: "test response"}}]}),
      }
    }))
  })

  afterEach(() => {
    vi.unstubAllGlobals()
  })

  test("system prompt identifies as mobile support assistant", async () => {
    const client = new GrokApiClient("test-key", "")
    await client.chat([], "test")
    const systemMsg = capturedBody.messages[0]
    expect(systemMsg.role).toBe("system")
    expect(systemMsg.content).toContain("on mobile")
    expect(systemMsg.content).toContain("support assistant")
  })

  test("system prompt instructs concise, phone-friendly answers", async () => {
    const client = new GrokApiClient("test-key", "")
    await client.chat([], "test")
    const prompt = capturedBody.messages[0].content
    expect(prompt).toContain("Be concise")
    expect(prompt).toContain("phone screen")
  })

  test("system prompt discourages filler and preambles", async () => {
    const client = new GrokApiClient("test-key", "")
    await client.chat([], "test")
    const prompt = capturedBody.messages[0].content
    expect(prompt).toContain("Avoid filler, preambles, and repeating the question back")
  })

  test("system prompt instructs brief numbered steps for how-to", async () => {
    const client = new GrokApiClient("test-key", "")
    await client.chat([], "test")
    const prompt = capturedBody.messages[0].content
    expect(prompt).toContain("brief numbered steps")
  })

  test("system prompt instructs 1-2 sentence answers for simple questions", async () => {
    const client = new GrokApiClient("test-key", "")
    await client.chat([], "test")
    const prompt = capturedBody.messages[0].content
    expect(prompt).toContain("Answer simple questions in 1-2 sentences")
  })

  test("system prompt forbids markdown formatting", async () => {
    const client = new GrokApiClient("test-key", "")
    await client.chat([], "test")
    const prompt = capturedBody.messages[0].content
    expect(prompt).toContain("Do not use markdown formatting")
  })

  test("system prompt includes docs context", async () => {
    const docsContext = "SimpleX Chat uses double ratchet encryption."
    const client = new GrokApiClient("test-key", docsContext)
    await client.chat([], "test")
    const prompt = capturedBody.messages[0].content
    expect(prompt).toContain(docsContext)
  })

  test("system prompt does NOT contain old 'complete answers' instruction", async () => {
    const client = new GrokApiClient("test-key", "")
    await client.chat([], "test")
    const prompt = capturedBody.messages[0].content
    expect(prompt).not.toContain("Give clear, complete answers")
  })

  test("system prompt does NOT contain 'evangelist'", async () => {
    const client = new GrokApiClient("test-key", "")
    await client.chat([], "test")
    const prompt = capturedBody.messages[0].content
    expect(prompt).not.toContain("evangelist")
  })

  test("chat sends history and user message after system prompt", async () => {
    const client = new GrokApiClient("test-key", "")
    const history: GrokMessage[] = [
      {role: "user", content: "previous question"},
      {role: "assistant", content: "previous answer"},
    ]
    await client.chat(history, "new question")
    expect(capturedBody.messages.length).toBe(4) // system + 2 history + user
    expect(capturedBody.messages[1]).toEqual({role: "user", content: "previous question"})
    expect(capturedBody.messages[2]).toEqual({role: "assistant", content: "previous answer"})
    expect(capturedBody.messages[3]).toEqual({role: "user", content: "new question"})
  })

  test("chat truncates history to last 20 messages", async () => {
    const client = new GrokApiClient("test-key", "")
    const history: GrokMessage[] = Array.from({length: 30}, (_, i) => ({
      role: (i % 2 === 0 ? "user" : "assistant") as "user" | "assistant",
      content: `msg-${i}`,
    }))
    await client.chat(history, "final")
    // system(1) + history(20) + user(1) = 22
    expect(capturedBody.messages.length).toBe(22)
    expect(capturedBody.messages[1].content).toBe("msg-10") // starts from index 10
  })

  test("API error throws with status and body", async () => {
    vi.stubGlobal("fetch", vi.fn(async () => ({
      ok: false,
      status: 429,
      text: async () => "rate limited",
    })))
    const client = new GrokApiClient("test-key", "")
    await expect(client.chat([], "test")).rejects.toThrow("Grok API 429: rate limited")
  })

  test("empty API response throws", async () => {
    vi.stubGlobal("fetch", vi.fn(async () => ({
      ok: true,
      json: async () => ({choices: [{}]}),
    })))
    const client = new GrokApiClient("test-key", "")
    await expect(client.chat([], "test")).rejects.toThrow("Grok API returned empty response")
  })
})


// ─── 25. resolveDisplayNameConflict ──────────────────────────

describe("resolveDisplayNameConflict", () => {

  const mockExistsSync = vi.mocked(existsSync)
  const mockExecSync = vi.mocked(execSync)

  beforeEach(() => {
    mockExistsSync.mockReset()
    mockExecSync.mockReset()
  })

  test("no-op when database file does not exist", () => {
    mockExistsSync.mockReturnValue(false)

    resolveDisplayNameConflict("./data/bot", "Ask SimpleX Team")

    expect(mockExecSync).not.toHaveBeenCalled()
  })

  test("no-op when user already has the desired display name", () => {
    mockExistsSync.mockReturnValue(true)
    mockExecSync.mockReturnValueOnce("1\n" as any) // user count = 1

    resolveDisplayNameConflict("./data/bot", "Ask SimpleX Team")

    // Only one execSync call (the user check), no rename
    expect(mockExecSync).toHaveBeenCalledTimes(1)
    expect((mockExecSync.mock.calls[0][0] as string)).toContain("SELECT COUNT(*) FROM users")
  })

  test("no-op when name is not in display_names table", () => {
    mockExistsSync.mockReturnValue(true)
    mockExecSync
      .mockReturnValueOnce("0\n" as any) // user count = 0 (different name)
      .mockReturnValueOnce("0\n" as any) // display_names count = 0

    resolveDisplayNameConflict("./data/bot", "Ask SimpleX Team")

    expect(mockExecSync).toHaveBeenCalledTimes(2)
  })

  test("renames conflicting entry when name exists in display_names", () => {
    mockExistsSync.mockReturnValue(true)
    mockExecSync
      .mockReturnValueOnce("0\n" as any) // user count = 0
      .mockReturnValueOnce("1\n" as any) // display_names count = 1
      .mockReturnValueOnce("" as any)    // UPDATE statements

    resolveDisplayNameConflict("./data/bot", "Ask SimpleX Team")

    expect(mockExecSync).toHaveBeenCalledTimes(3)
    const updateCall = mockExecSync.mock.calls[2][0] as string
    expect(updateCall).toContain("UPDATE contacts SET local_display_name = 'Ask SimpleX Team_1'")
    expect(updateCall).toContain("UPDATE groups SET local_display_name = 'Ask SimpleX Team_1'")
    expect(updateCall).toContain("UPDATE display_names SET local_display_name = 'Ask SimpleX Team_1', ldn_suffix = 1")
  })

  test("uses correct database file path", () => {
    mockExistsSync.mockReturnValue(true)
    mockExecSync.mockReturnValueOnce("1\n" as any)

    resolveDisplayNameConflict("./data/mybot", "Test")

    expect(mockExistsSync).toHaveBeenCalledWith("./data/mybot_chat.db")
    expect((mockExecSync.mock.calls[0][0] as string)).toContain("./data/mybot_chat.db")
  })

  test("escapes single quotes in display name", () => {
    mockExistsSync.mockReturnValue(true)
    mockExecSync
      .mockReturnValueOnce("0\n" as any)
      .mockReturnValueOnce("1\n" as any)
      .mockReturnValueOnce("" as any)

    resolveDisplayNameConflict("./data/bot", "O'Brien's Bot")

    const updateCall = mockExecSync.mock.calls[2][0] as string
    expect(updateCall).toContain("O''Brien''s Bot")
  })

  test("catches execSync errors gracefully and logs error", async () => {
    const {logError} = await import("./src/util")
    vi.mocked(logError).mockClear()
    mockExistsSync.mockReturnValue(true)
    mockExecSync.mockImplementation(() => { throw new Error("sqlite3 not found") })

    expect(() => resolveDisplayNameConflict("./data/bot", "Test")).not.toThrow()
    expect(logError).toHaveBeenCalledWith(
      "Failed to resolve display name conflict (sqlite3 may not be available)",
      expect.any(Error)
    )
  })
})
