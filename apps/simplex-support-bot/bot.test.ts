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
    ChatType: {Group: "group", Direct: "direct"},
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
import {parseConfig, parseIdName} from "./src/config"
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

interface SentMessage { chat: [string, number]; text: string; inReplyTo?: number }
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
  roleChanges: {groupId: number; memberIds: number[]; role: string}[] = []

  private addMemberFail = false
  private addMemberDuplicate = false
  private nextMemberGId = 50
  private nextItemId = 1000

  apiAddMemberWillFail()              { this.addMemberFail = true }
  apiAddMemberWillDuplicate()         { this.addMemberDuplicate = true }
  setNextGroupMemberId(id: number)    { this.nextMemberGId = id }
  setGroupMembers(groupId: number, members: any[]) { this.members.set(groupId, members) }
  setChatItems(groupId: number, items: any[]) { this.chatItems.set(groupId, items) }

  async apiSendTextMessage(chat: [string, number], text: string, inReplyTo?: number) {
    this.sent.push({chat, text, inReplyTo})
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

  async apiSetMembersRole(groupId: number, memberIds: number[], role: string) {
    this.roleChanges.push({groupId, memberIds, role})
  }

  async apiJoinGroup(groupId: number) {
    this.joined.push(groupId)
  }

  async apiListMembers(groupId: number) {
    return this.members.get(groupId) || []
  }

  sentCmds: string[] = []
  private nextContactId = 100

  // sendChatCmd is used by apiGetChat, /_create member contact, /_invite member contact
  async sendChatCmd(cmd: string) {
    this.sentCmds.push(cmd)
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
    // Parse "/_create member contact #<groupId> <memberId>"
    const createMatch = cmd.match(/\/_create member contact #(\d+) (\d+)/)
    if (createMatch) {
      const contactId = this.nextContactId++
      return {type: "newMemberContact", contact: {contactId}}
    }
    // Parse "/_invite member contact @<contactId>"
    if (cmd.startsWith("/_invite member contact @")) {
      return {type: "newMemberContactSentInv"}
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
    this.sent = []; this.added = []; this.removed = []; this.joined = []; this.sentCmds = []
    this.members.clear(); this.chatItems.clear()
    this.updatedProfiles = []; this.updatedChatItems = []
    this.addMemberFail = false; this.addMemberDuplicate = false; this.nextMemberGId = 50; this.nextItemId = 1000; this.nextContactId = 100
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

  async sendsReplyTo(text: string, quotedItemId: number, groupId = GROUP_ID) {
    const ci = customerChatItem(text, null)
    ci.chatInfo.groupInfo = businessGroupInfo(groupId)
    ci.chatItem.quotedItem = {itemId: quotedItemId}
    if (!mainChat.chatItems.has(groupId)) mainChat.chatItems.set(groupId, [])
    mainChat.chatItems.get(groupId)!.push({
      chatDir: {
        type: "groupRcv",
        groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10},
      },
      _text: text,
    })
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

// Format helpers for expected forwarded messages (new A1-A6 format)
// Note: in tests, duration is always <60s so it's omitted from the header
function fmtCustomer(text: string, state = "QUEUE", msgNum = 2, name = "Alice", groupId = GROUP_ID) {
  return `*${groupId}:${name} · ${state} · #${msgNum}*\n${text}`
}
function fmtTeamMember(tmContactId: number, text: string, state = "TEAM", msgNum: number, tmName = "Bob", customerName = "Alice", groupId = GROUP_ID) {
  return `!2 >>! *${tmContactId}:${tmName} > ${groupId}:${customerName} · ${state} · #${msgNum}*\n${text}`
}
function fmtGrok(text: string, state = "GROK", msgNum: number, name = "Alice", groupId = GROUP_ID) {
  return `!5 AI! *Grok > ${groupId}:${name} · ${state} · #${msgNum}*\n_${text}_`
}
function fmtNewCustomer(text: string, state = "QUEUE", msgNum = 1, name = "Alice", groupId = GROUP_ID) {
  return `!1 NEW! *${groupId}:${name} · ${state} · #${msgNum}*\n${text}`
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

  async sendsReplyTo(text: string, quotedItemId: number, groupId = GROUP_ID) {
    const ci = teamMemberChatItem(lastTeamMemberGId, text)
    ci.chatInfo.groupInfo = businessGroupInfo(groupId)
    ci.chatItem.quotedItem = {itemId: quotedItemId}
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

  wasNotRemoved(groupId = GROUP_ID) {
    const found = mainChat.removed.some(
      r => r.groupId === groupId && r.memberIds.includes(lastGrokMemberGId)
    )
    expect(found).toBe(false)
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

const TEAM_ALREADY_ADDED =
  `A team member has already been invited to this conversation and will reply when available.`


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

  test("first message → forwarded to team with NEW, queue reply sent", async () => {
    // No prior bot messages → isFirstCustomerMessage returns true → welcome flow
    await customer.sends("How do I create a group?")

    teamGroup.received(fmtNewCustomer("How do I create a group?", "QUEUE", 1))
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

    teamGroup.received(fmtCustomer("More details about my issue", "QUEUE", 2))
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

    teamGroup.received(fmtCustomer("/unknown", "QUEUE", 2))
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

    // msgNum=3: #1=Hello, #2=Grok initial answer, #3=customer follow-up
    teamGroup.received(fmtCustomer("What about encryption?", "GROK", 3))

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

  test("/team from grokMode → team member added, Grok stays until team member connects", async () => {
    await reachGrokMode()
    mainChat.setNextGroupMemberId(70)
    lastTeamMemberGId = 70
    mainChat.sent = []

    await customer.sends("/team")

    // Grok NOT removed yet — stays functional during transition
    grokAgent.wasNotRemoved()
    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)

    // Team member sends first message → Grok removed
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: lastGrokMemberGId, memberContactId: 4, memberStatus: "connected"},
      {groupMemberId: 70, memberContactId: 2, memberStatus: "connected"},
    ])
    await teamMember.sends("Hi, I'll help you")
    grokAgent.wasRemoved()
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

  test("customer text in teamPending → forwarded to team group", async () => {
    await reachTeamPending()
    mainChat.sent = []

    await customer.sends("Here's more info about my issue")

    // msgNum=2: #1=Hello, #2=this message; TEAM state (team member present)
    teamGroup.received(fmtCustomer("Here's more info about my issue", "TEAM", 2))
    // No reply sent to customer group
    expect(mainChat.sentTo(GROUP_ID).length).toBe(0)
  })

  test("customer text in teamLocked → forwarded to team group", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await customer.sends("Thank you!")

    // msgNum=3: #1=Hello, #2=team "I'll help you", #3=customer "Thank you!"
    teamGroup.received(fmtCustomer("Thank you!", "TEAM", 3))
    // No reply sent to customer group
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

  test("team member leaves in teamLocked → no replacement added", async () => {
    await reachTeamLocked()
    mainChat.added = []

    await teamMember.leaves()

    // No replacement — team member is not auto-invited back
    expect(mainChat.added.length).toBe(0)
  })
})


// ─── 7b. Team Re-addition Prevention ─────────────────────────────

describe("Team Re-addition Prevention", () => {

  test("/team after team member left teamPending → not re-added, already-added message", async () => {
    await reachTeamPending()
    // Team member leaves (teamPending revert)
    mainChat.setGroupMembers(GROUP_ID, [])
    await teamMember.leaves()
    mainChat.added = []
    mainChat.sent = []

    // Customer sends /team again
    await customer.sends("/team")

    // Team member NOT re-added
    expect(mainChat.added.length).toBe(0)
    // Customer gets the already-added message
    customer.received(TEAM_ALREADY_ADDED)
  })

  test("/team after team member left teamLocked → not re-added", async () => {
    await reachTeamLocked()
    // Team member leaves
    mainChat.setGroupMembers(GROUP_ID, [])
    await teamMember.leaves()
    mainChat.added = []
    mainChat.sent = []

    // Customer sends /team again
    await customer.sends("/team")

    // Team member NOT re-added — hasTeamBeenActivatedBefore returns true
    expect(mainChat.added.length).toBe(0)
    customer.received(TEAM_ALREADY_ADDED)
  })

  test("/team from grokMode after prior team activation → Grok NOT removed, not re-added", async () => {
    // First: activate team, then team member leaves, then customer activates Grok
    await reachTeamPending()
    mainChat.setGroupMembers(GROUP_ID, [])
    await teamMember.leaves()

    // Now in teamQueue equivalent — activate Grok
    mainChat.setNextGroupMemberId(61)
    lastGrokMemberGId = 61
    grokApi.willRespond("Grok answer")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 61, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    mainChat.added = []
    mainChat.removed = []
    mainChat.sent = []

    // Customer sends /team while in grokMode — but team was already activated before
    await customer.sends("/team")

    // Grok NOT removed (activateTeam returned early)
    expect(mainChat.removed.length).toBe(0)
    // Team member NOT re-added
    expect(mainChat.added.length).toBe(0)
    customer.received(TEAM_ALREADY_ADDED)
  })

  test("first /team still works normally", async () => {
    await reachTeamQueue("Hello")
    mainChat.setGroupMembers(GROUP_ID, [])
    mainChat.setNextGroupMemberId(50)
    lastTeamMemberGId = 50
    mainChat.added = []
    mainChat.sent = []

    await customer.sends("/team")

    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)
  })

  test("restart after team activation → /team still blocked", async () => {
    await reachTeamPending()
    mainChat.setGroupMembers(GROUP_ID, [])
    await teamMember.leaves()

    // Simulate restart: create new bot instance, but chat history persists
    const freshBot = new SupportBot(mainChat as any, grokChat as any, grokApi as any, config as any)
    mainChat.added = []
    mainChat.sent = []

    // Customer sends /team via the restarted bot
    const ci = customerChatItem("/team", "team")
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "/team",
      _botCommand: "team",
    })
    await freshBot.onNewChatItems({chatItems: [ci]} as any)

    // Team member NOT re-added
    expect(mainChat.added.length).toBe(0)
    customer.received(TEAM_ALREADY_ADDED)
  })

  test("/add command still works after team activation (team-initiated)", async () => {
    await reachTeamPending()
    mainChat.setGroupMembers(GROUP_ID, [])
    await teamMember.leaves()
    mainChat.added = []

    // Team member uses /add in team group — should bypass the check
    const addCi = {
      chatInfo: {type: "group", groupInfo: {groupId: TEAM_GRP_ID, businessChat: null}},
      chatItem: {
        chatDir: {
          type: "groupRcv",
          groupMember: {memberId: "tm-1", groupMemberId: 30, memberContactId: 2, memberProfile: {displayName: "Bob"}},
        },
        meta: {itemId: 900},
        _text: `/add ${GROUP_ID}:Alice`,
      },
    } as any
    await bot.onNewChatItems({chatItems: [addCi]} as any)

    // /add bypasses activateTeam — team member added directly
    expect(mainChat.added.length).toBe(1)
    expect(mainChat.added[0].groupId).toBe(GROUP_ID)
    expect(mainChat.added[0].contactId).toBe(2)
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

    // msgNum=3: #1=Hello, #2=Grok answer in reachGrokMode, #3=this
    teamGroup.received(fmtCustomer("Another question", "QUEUE", 3))
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

  test("Grok join timeout → error msg, Grok member removed", async () => {
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
    // Grok member should be removed on timeout to prevent ghost grokMode
    grokAgent.wasRemoved()
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

  test("team member add fails in grokMode → error msg, Grok stays", async () => {
    await reachGrokMode()
    mainChat.apiAddMemberWillFail()
    mainChat.sent = []

    await customer.sends("/team")

    grokAgent.wasNotRemoved()
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

  test("/team sent while waiting for Grok to join → Grok continues, team member added", async () => {
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

    // Grok join completes — Grok keeps working (team member not yet connected)
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await grokPromise

    // Grok NOT removed — still functional
    grokAgent.wasNotRemoved()
    // Grok API was called (activation succeeded)
    expect(grokApi.callCount()).toBe(1)
  })

  test("team member connects during Grok session → Grok removed", async () => {
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

    // While API call is pending, /team adds team member
    mainChat.setNextGroupMemberId(70)
    lastTeamMemberGId = 70
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await customer.sends("/team")

    // API call completes — Grok answer is sent (no abort)
    resolveGrokCall("Grok answer")
    await grokPromise
    grokAgent.wasNotRemoved()

    // Team member sends message → Grok removed
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
      {groupMemberId: 70, memberContactId: 2, memberStatus: "connected"},
    ])
    await teamMember.sends("I'll take over")
    grokAgent.wasRemoved()
  })

  test("team member non-text event (join notification) does NOT remove Grok", async () => {
    await reachGrokMode()
    mainChat.sent = []

    // Simulate a non-text system event from a team member (e.g., join notification)
    const ci = {
      chatInfo: {type: "group", groupInfo: businessGroupInfo()},
      chatItem: {
        chatDir: {
          type: "groupRcv",
          groupMember: {memberId: "team-member-1", groupMemberId: 70, memberContactId: 2, memberProfile: {displayName: "Bob"}},
        },
        meta: {itemId: nextChatItemId++},
        content: {type: "rcvGroupEvent", rcvGroupEvent: {type: "memberConnected"}},
        _text: null,
      },
    } as any
    ci.chatInfo.groupInfo = businessGroupInfo()
    await bot.onNewChatItems({chatItems: [ci]} as any)

    // Grok should NOT be removed — only a real text message should trigger removal
    grokAgent.wasNotRemoved()
    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(true)
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

  test("format: first message has !1 NEW! color-coded prefix", async () => {
    await customer.sends("My app crashes on startup")

    teamGroup.received(fmtNewCustomer("My app crashes on startup", "QUEUE", 1))
  })

  test("grokMode messages also forwarded to team", async () => {
    await reachGrokMode()
    mainChat.sent = []

    grokApi.willRespond("Try clearing app data")
    await customer.sends("App keeps crashing")

    // msgNum=3: #1=Hello, #2=Grok answer, #3=customer follow-up
    teamGroup.received(fmtCustomer("App keeps crashing", "GROK", 3))
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

    teamGroup.received(fmtNewCustomer("Hello", "QUEUE", 1, "group-101", 101))
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
    // First message for group 888 in this bot instance → msgNum=1
    teamGroup.received(fmtCustomer("I had a question earlier", "QUEUE", 1, "Alice", 888))
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

  test("/grok as first message → activates grok directly", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    grokApi.willRespond("Hello! How can I help?")

    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    // Grok activated, no teamQueue message
    customer.received(GROK_ACTIVATED)
    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    expect(teamMsgs.some(m => m.includes("/grok"))).toBe(false) // Commands not forwarded
    // /add not sent — only sent on first forwarded text message
    expect(teamMsgs.some(m => m.startsWith("/add"))).toBe(false)
  })

  test("/team as first message → activates team directly", async () => {
    mainChat.setGroupMembers(GROUP_ID, [])
    await customer.sends("/team")

    // Team member added, no teamQueue message
    customer.received(TEAM_ADDED_24H)
    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    expect(teamMsgs.some(m => m.includes("/team"))).toBe(false) // Commands not forwarded
    // /add not sent — only sent on first forwarded text message
    expect(teamMsgs.some(m => m.startsWith("/add"))).toBe(false)
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

  test("team member leaves teamLocked → no auto-replacement attempted", async () => {
    await reachTeamLocked()
    mainChat.added = []

    await teamMember.leaves()

    // No replacement attempted
    expect(mainChat.added.length).toBe(0)
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

  test("null grokContactId → members with null memberContactId not matched as Grok", async () => {
    const nullGrokConfig = {...config, grokContactId: null}
    const nullBot = new SupportBot(mainChat as any, grokChat as any, grokApi as any, nullGrokConfig as any)
    // A member with null memberContactId is in the group (should NOT be treated as Grok)
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 99, memberContactId: null, memberStatus: "connected"},
    ])
    // Send first message to move past welcome
    const ci1 = customerChatItem("Hello", null)
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "Hello",
    })
    await nullBot.onNewChatItems({chatItems: [ci1]} as any)

    // Should route to handleNoSpecialMembers (welcome→teamQueue), NOT handleGrokMode
    customer.received(TEAM_QUEUE_24H)
  })

  test("null grokContactId → leftMember with null memberContactId not treated as Grok leave", async () => {
    const nullGrokConfig = {...config, grokContactId: null}
    const nullBot = new SupportBot(mainChat as any, grokChat as any, grokApi as any, nullGrokConfig as any)
    // Simulate a member with null memberContactId leaving — should not crash or misidentify
    await nullBot.onLeftMember({
      groupInfo: businessGroupInfo(),
      member: {memberId: "unknown-member", groupMemberId: 99, memberContactId: null},
    } as any)
    // No crash, and grok maps unchanged (was never set)
    expect((nullBot as any).grokGroupMap.size).toBe(0)
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
    // Step 1: first message → teamQueue (#1)
    await customer.sends("How do I enable disappearing messages?")
    teamGroup.received(fmtNewCustomer("How do I enable disappearing messages?", "QUEUE", 1))
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
    // msgNum=3: #1=customer msg, #2=Grok initial, #3=customer follow-up
    teamGroup.received(fmtCustomer("Can I set different timers?", "GROK", 3))
    customer.receivedFromGrok("Yes, you can set different timers per conversation.")

    // Step 4: /team → team added, Grok stays during transition
    mainChat.setNextGroupMemberId(70)
    lastTeamMemberGId = 70
    await customer.sends("/team")
    grokAgent.wasNotRemoved()
    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)

    // Step 4b: team member sends first message → Grok removed
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: lastGrokMemberGId, memberContactId: 4, memberStatus: "connected"},
      {groupMemberId: 70, memberContactId: 2, memberStatus: "connected"},
    ])
    await teamMember.sends("Hi! Let me help you.")
    grokAgent.wasRemoved()

    // Update members: Grok gone, team member present
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 70, memberContactId: 2, memberStatus: "connected"},
    ])

    // Step 7: /grok still rejected
    await customer.sends("/grok")
    customer.received(TEAM_LOCKED_MSG)

    // Step 8: customer continues — forwarded to team group, no reply to customer
    mainChat.sent = []
    await customer.sends("Thanks for helping!")
    // msgNum=6: #1=customer, #2=grok, #3=customer, #4=grok, #5=team, #6=customer
    teamGroup.received(fmtCustomer("Thanks for helping!", "TEAM", 6))
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
    teamGroup.received(fmtCustomer("I had a question earlier", "QUEUE", 1, "Alice", 777))
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

  test("grokGroupMap set does NOT satisfy waitForGrokJoin (only grokFullyConnected does)", async () => {
    // Verify the fast-path checks grokFullyConnected, not grokGroupMap
    // grokGroupMap can be set (by onGrokGroupInvitation) before connectedToGroupMember fires
    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(false)
    expect((bot as any).grokFullyConnected.has(GROUP_ID)).toBe(false)

    // Manually set grokGroupMap but NOT grokFullyConnected (simulates invitation processed)
    ;(bot as any).grokGroupMap.set(GROUP_ID, GROK_LOCAL)
    ;(bot as any).reverseGrokMap.set(GROK_LOCAL, GROUP_ID)

    // waitForGrokJoin should NOT resolve immediately (grokGroupMap is set but grokFullyConnected isn't)
    vi.useFakeTimers()
    const result = (bot as any).waitForGrokJoin(GROUP_ID, 100)
    await vi.advanceTimersByTimeAsync(101)
    expect(await result).toBe(false)
    vi.useRealTimers()

    // Cleanup
    ;(bot as any).grokGroupMap.delete(GROUP_ID)
    ;(bot as any).reverseGrokMap.delete(GROK_LOCAL)
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

  test("team member leaves → no replacement, no duplicate handling needed", async () => {
    await reachTeamLocked()
    mainChat.added = []

    await teamMember.leaves()

    expect(mainChat.added.length).toBe(0)
  })
})


// ─── 18. DM Contact — Proactive Member Contact Creation ────────

describe("DM Contact — Proactive Member Contact Creation", () => {

  test("member with existing contact (auto-accept) → DM sent directly", async () => {
    mainChat.sent = []
    mainChat.sentCmds = []

    await bot.onMemberConnected({
      groupInfo: {groupId: TEAM_GRP_ID},
      member: {groupMemberId: 30, memberContactId: 5, memberProfile: {displayName: "TeamGuy"}},
    } as any)

    // No /_create command — contact already exists
    expect(mainChat.sentCmds.some(c => c.includes("/_create member contact"))).toBe(false)

    // DM sent directly via existing contact
    const dm = mainChat.sent.find(m => m.chat[0] === "direct" && m.chat[1] === 5)
    expect(dm).toBeDefined()
    expect(dm!.text).toContain("keep this contact")
    expect(dm!.text).toContain("5:TeamGuy")
  })

  test("member with memberContact on event → DM sent directly via memberContact", async () => {
    mainChat.sent = []
    mainChat.sentCmds = []

    await bot.onMemberConnected({
      groupInfo: {groupId: TEAM_GRP_ID},
      member: {groupMemberId: 30, memberContactId: null, memberProfile: {displayName: "TeamGuy"}},
      memberContact: {contactId: 42},
    } as any)

    // No /_create command — memberContact provided
    expect(mainChat.sentCmds.some(c => c.includes("/_create member contact"))).toBe(false)

    // DM sent directly via memberContact
    const dm = mainChat.sent.find(m => m.chat[0] === "direct" && m.chat[1] === 42)
    expect(dm).toBeDefined()
    expect(dm!.text).toContain("keep this contact")
    expect(dm!.text).toContain("42:TeamGuy")
  })

  test("member with no contact → create contact, invite, DM on contactConnected", async () => {
    mainChat.sent = []
    mainChat.sentCmds = []

    await bot.onMemberConnected({
      groupInfo: {groupId: TEAM_GRP_ID},
      member: {groupMemberId: 30, memberContactId: null, memberProfile: {displayName: "TeamGuy"}},
    } as any)

    // /_create member contact and /_invite member contact sent
    expect(mainChat.sentCmds.some(c => c.includes("/_create member contact #1 30"))).toBe(true)
    expect(mainChat.sentCmds.some(c => c.includes("/_invite member contact @"))).toBe(true)

    // DM not sent yet — contact not connected
    expect(mainChat.sent.find(m => m.chat[0] === "direct")).toBeUndefined()

    // contactConnected fires → DM sent
    await bot.onContactConnected({contact: {contactId: 100}} as any)

    const dm = mainChat.sent.find(m => m.chat[0] === "direct" && m.chat[1] === 100)
    expect(dm).toBeDefined()
    expect(dm!.text).toContain("keep this contact")
    expect(dm!.text).toContain("100:TeamGuy")
  })

  test("member with spaces in name → name quoted in DM", async () => {
    mainChat.sent = []

    await bot.onMemberConnected({
      groupInfo: {groupId: TEAM_GRP_ID},
      member: {groupMemberId: 31, memberContactId: 7, memberProfile: {displayName: "Team Guy"}},
    } as any)

    const dm = mainChat.sent.find(m => m.chat[0] === "direct" && m.chat[1] === 7)
    expect(dm).toBeDefined()
    expect(dm!.text).toContain("7:'Team Guy'")
  })

  test("non-team group member connects → no create, no DM", async () => {
    mainChat.sent = []
    mainChat.sentCmds = []

    await bot.onMemberConnected({
      groupInfo: {groupId: 999},
      member: {groupMemberId: 30, memberContactId: null, memberProfile: {displayName: "Someone"}},
    } as any)

    expect(mainChat.sentCmds.some(c => c.includes("/_create member contact"))).toBe(false)
    expect(mainChat.sent.find(m => m.chat[0] === "direct")).toBeUndefined()
  })

  test("contactConnected for unknown contact → ignored", async () => {
    mainChat.sent = []
    await bot.onContactConnected({contact: {contactId: 999}} as any)

    expect(mainChat.sent.find(m => m.chat[0] === "direct")).toBeUndefined()
  })

  test("receivedInv fallback → DM queued and sent on contactConnected", async () => {
    mainChat.sent = []
    await bot.onMemberContactReceivedInv({
      contact: {contactId: 10},
      groupInfo: {groupId: TEAM_GRP_ID},
      member: {memberProfile: {displayName: "TeamGuy"}},
    } as any)

    // DM not sent yet
    expect(mainChat.sent.find(m => m.chat[0] === "direct")).toBeUndefined()

    // contactConnected fires → DM sent
    await bot.onContactConnected({contact: {contactId: 10}} as any)

    const dm = mainChat.sent.find(m => m.chat[0] === "direct" && m.chat[1] === 10)
    expect(dm).toBeDefined()
    expect(dm!.text).toContain("keep this contact")
  })

  test("non-team group receivedInv → no DM", async () => {
    mainChat.sent = []
    await bot.onMemberContactReceivedInv({
      contact: {contactId: 11},
      groupInfo: {groupId: 999},
      member: {memberProfile: {displayName: "Stranger"}},
    } as any)
    await bot.onContactConnected({contact: {contactId: 11}} as any)

    expect(mainChat.sent.find(m => m.chat[0] === "direct")).toBeUndefined()
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

  test("customer edits forwarded message → team group message updated (with *NEW:* if still new)", async () => {
    // Send first message → forwarded to team (stores mapping)
    await customer.sends("Original question")
    // The customer chat item had itemId=500, the forwarded team msg got itemId=1000
    mainChat.sent = []

    // Simulate edit event — first message still has *NEW:* marker
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
    // Edit uses stored header from original forward. Original was first msg with QUEUE state, #1
    expect(mainChat.updatedChatItems[0].msgContent).toEqual({type: "text", text: fmtNewCustomer("Edited question", "QUEUE", 1)})
  })

  test("team member edits forwarded message → team group message updated", async () => {
    await reachTeamPending()
    // After reachTeamPending: nextChatItemId=502, nextItemId=1004 (no command fwd)
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
    // Team member msg was #2 in TEAM state
    expect(mainChat.updatedChatItems[0].msgContent).toEqual({type: "text", text: fmtTeamMember(2, "Actually, let me rephrase", "TEAM", 2)})
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
    // customerChatItem itemId=502, forwarded to team as itemId=1005 (no command fwd)
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
    expect(mainChat.updatedChatItems[0].chatItemId).toBe(1005)
    // Edit uses stored header from original forward: GROK state, #3
    expect(mainChat.updatedChatItems[0].msgContent).toEqual({type: "text", text: fmtCustomer("Edited encryption question", "GROK", 3)})
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

    // Team member msg #2 in TEAM state
    teamGroup.received(fmtTeamMember(2, "I'll help you with this", "TEAM", 2))
  })

  test("team member message in teamLocked → forwarded to team group", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await teamMember.sends("Here is the solution")

    // Team member msg #3 in TEAM state (after #1=Hello, #2=team "I'll help you")
    teamGroup.received(fmtTeamMember(2, "Here is the solution", "TEAM", 3))
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

    // Also forwarded to team group (mock has no chat history after reset, so isFirstCustomerMessage → true → NEW)
    // State is GROK (grok member present), #1 (first tracked msg)
    teamGroup.received(fmtNewCustomer("How does encryption work?", "GROK", 1))
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

    // Team group receives forwarded message (with !1 NEW!) + /add command
    teamGroup.received(fmtNewCustomer("Hello, I need help", "QUEUE", 1))
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
    expect(teamMsgs).toEqual([fmtCustomer("More details", "QUEUE", 2)])
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


// ─── 25b. Forwarded Message Reply-To ─────────────────────────────

describe("Forwarded Message Reply-To", () => {

  test("customer reply-to is forwarded with inReplyTo to team group", async () => {
    // "Hello" gets chatItemId 500, forwarded → teamItemId 1000
    await reachTeamQueue("Hello")
    // Send a reply to "Hello" (quotedItemId 500)
    await customer.sendsReplyTo("Following up on that", 500)

    const fwdMsg = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.includes("Following up on that"))
    expect(fwdMsg).toBeDefined()
    expect(fwdMsg!.inReplyTo).toBe(1000)
  })

  test("customer reply-to unknown item → A1 threading falls back to lastTeamItemByGroup", async () => {
    await reachTeamQueue("Hello")
    // "Hello" teamItemId=1000. Reply-to unknown (999) → resolveTeamReplyTo returns undefined
    // But A1 threading: effectiveReplyTo = lastTeamItemByGroup = 1000
    await customer.sendsReplyTo("Reply to unknown", 999)

    const fwdMsg = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.includes("Reply to unknown"))
    expect(fwdMsg).toBeDefined()
    expect(fwdMsg!.inReplyTo).toBe(1000) // A1: auto-thread to last team item
  })

  test("customer message without reply-to → A1 auto-threads to last team item", async () => {
    await reachTeamQueue("Hello")
    // "Hello" teamItemId=1000
    mainChat.sent = []
    await customer.sends("Another question")

    const fwdMsg = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.includes("Another question"))
    expect(fwdMsg).toBeDefined()
    expect(fwdMsg!.inReplyTo).toBe(1000) // A1: auto-thread to last team item
  })

  test("team member reply-to is forwarded with inReplyTo", async () => {
    // Customer "Hello" (chatItemId 500) → teamItemId 1000
    await reachTeamPending()
    await teamMember.sendsReplyTo("I'll help with that", 500)

    const fwdMsg = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.includes("I'll help with that"))
    expect(fwdMsg).toBeDefined()
    expect(fwdMsg!.inReplyTo).toBe(1000)
  })

  test("customer reply-to in grok mode forwarded with inReplyTo", async () => {
    await reachGrokMode("Initial answer")
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4}},
      _text: "Initial answer",
    })
    grokChat.setChatItems(GROK_LOCAL, [
      {chatDir: {type: "groupRcv"}, meta: {itemId: 5001}, _text: "Follow-up on my hello"},
    ])
    grokApi.willRespond("Follow-up answer")
    mainChat.sent = []

    // Customer replies to their own "Hello" (itemId 500) which was forwarded (teamItemId 1000)
    await customer.sendsReplyTo("Follow-up on my hello", 500)

    // After reachGrokMode: #1=Hello, #2=Grok initial. Customer follow-up is #3 in GROK state
    const custFwd = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text === fmtCustomer("Follow-up on my hello", "GROK", 3))
    expect(custFwd).toBeDefined()
    expect(custFwd!.inReplyTo).toBe(1000)
  })
})


// ─── 25c. Grok Response Forwarded to Team ───────────────────────

describe("Grok Response Forwarded to Team", () => {

  test("activateGrok forwards grok response to team with reply-to", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")
    // "Hello" (chatItemId 500) → teamItemId 1000

    grokChat.setChatItems(GROK_LOCAL, [
      {chatDir: {type: "groupRcv"}, meta: {itemId: 6001}, _text: "Hello"},
    ])
    grokApi.willRespond("Hi there!")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    // activateGrok: #1=Hello, Grok response=#2 in GROK state
    const grokFwd = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text === fmtGrok("Hi there!", "GROK", 2))
    expect(grokFwd).toBeDefined()
    expect(grokFwd!.inReplyTo).toBe(1000)
  })

  test("forwardToGrok forwards grok response to team with reply-to", async () => {
    await reachGrokMode("Initial answer")
    // "Hello" (chatItemId 500) → teamItemId 1000
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4}},
      _text: "Initial answer",
    })
    grokChat.setChatItems(GROK_LOCAL, [
      {chatDir: {type: "groupRcv"}, meta: {itemId: 5001}, _text: "What about encryption?"},
    ])
    grokApi.willRespond("Encryption answer")
    mainChat.sent = []

    await customer.sends("What about encryption?")

    // Customer msg forwarded: #3 in GROK state (#1=Hello, #2=Grok initial)
    const custFwd = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text === fmtCustomer("What about encryption?", "GROK", 3))
    expect(custFwd).toBeDefined()

    // Grok response forwarded: #4 in GROK state
    const grokFwd = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text === fmtGrok("Encryption answer", "GROK", 4))
    expect(grokFwd).toBeDefined()
    // After reachGrokMode, mainChat.nextItemId = 1005 (no cmd fwd). Customer fwd gets 1005.
    expect(grokFwd!.inReplyTo).toBe(1005)
  })

  test("grok response format includes customer prefix", async () => {
    await reachGrokMode("Test response")

    // activateGrok: #2 in GROK state
    const grokFwd = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text === fmtGrok("Test response", "GROK", 2))
    expect(grokFwd).toBeDefined()
  })

  test("grok API failure does not forward to team", async () => {
    await reachGrokMode("Initial answer")
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4}},
      _text: "Initial answer",
    })
    grokChat.setChatItems(GROK_LOCAL, [
      {chatDir: {type: "groupRcv"}, meta: {itemId: 5001}, _text: "Fail me"},
    ])
    grokApi.willFail()
    mainChat.sent = []

    await customer.sends("Fail me")

    // No Grok response forwarded to team (look for AI prefix)
    const grokFwd = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.startsWith("!5 AI!"))
    expect(grokFwd).toBeUndefined()
  })
})


// ─── 25d. Grok Reply-To ─────────────────────────────────────────

describe("Grok Reply-To", () => {

  test("forwardToGrok replies to the last received message in grok chat", async () => {
    await reachGrokMode("Initial answer")
    // Simulate Grok agent's view: it has the previous customer message in its local chat
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4}},
      _text: "Initial answer",
    })
    // Set up Grok agent's local chat with the new customer message (as Grok would see it)
    grokChat.setChatItems(GROK_LOCAL, [
      {chatDir: {type: "groupRcv"}, meta: {itemId: 5001}, _text: "What about encryption?"},
    ])
    grokApi.willRespond("Encryption answer")
    grokChat.sent = []

    await customer.sends("What about encryption?")

    // Grok response sent with inReplyTo matching the customer message item ID in Grok's view
    const grokSent = grokChat.sent.find(m => m.chat[1] === GROK_LOCAL && m.text === "Encryption answer")
    expect(grokSent).toBeDefined()
    expect(grokSent!.inReplyTo).toBe(5001)
  })

  test("activateGrok replies to the last customer message", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")

    // Set up Grok agent's local chat — simulates Grok seeing the customer's message after join
    grokChat.setChatItems(GROK_LOCAL, [
      {chatDir: {type: "groupRcv"}, meta: {itemId: 6001}, _text: "Hello"},
    ])

    grokApi.willRespond("Hi there!")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    const grokSent = grokChat.sent.find(m => m.chat[1] === GROK_LOCAL && m.text === "Hi there!")
    expect(grokSent).toBeDefined()
    expect(grokSent!.inReplyTo).toBe(6001)
  })

  test("activateGrok with multiple customer messages replies to the last one", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("First question", "Second question")

    // Grok agent sees both customer messages — reply should target the last one
    grokChat.setChatItems(GROK_LOCAL, [
      {chatDir: {type: "groupRcv"}, meta: {itemId: 7001}, _text: "First question"},
      {chatDir: {type: "groupRcv"}, meta: {itemId: 7002}, _text: "Second question"},
    ])

    grokApi.willRespond("Answer to both")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    const grokSent = grokChat.sent.find(m => m.chat[1] === GROK_LOCAL && m.text === "Answer to both")
    expect(grokSent).toBeDefined()
    expect(grokSent!.inReplyTo).toBe(7002)
  })

  test("graceful fallback when grok chat has no matching item", async () => {
    await reachGrokMode("Initial answer")
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4}},
      _text: "Initial answer",
    })
    // Grok agent's chat is empty — no item to reply to
    grokChat.setChatItems(GROK_LOCAL, [])
    grokApi.willRespond("Some answer")
    grokChat.sent = []

    await customer.sends("New question")

    // Response sent without inReplyTo (graceful fallback)
    const grokSent = grokChat.sent.find(m => m.chat[1] === GROK_LOCAL && m.text === "Some answer")
    expect(grokSent).toBeDefined()
    expect(grokSent!.inReplyTo).toBeUndefined()
  })

  test("skips grok's own messages (groupSnd) when searching for reply target", async () => {
    await reachGrokMode("Initial answer")
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4}},
      _text: "Initial answer",
    })
    // Grok's chat: has Grok's own previous response (groupSnd) then the customer message
    grokChat.setChatItems(GROK_LOCAL, [
      {chatDir: {type: "groupSnd"}, meta: {itemId: 8001}, _text: "Follow-up question"},
      {chatDir: {type: "groupRcv"}, meta: {itemId: 8002}, _text: "Follow-up question"},
    ])
    grokApi.willRespond("Follow-up answer")
    grokChat.sent = []

    await customer.sends("Follow-up question")

    // Should reply to 8002 (groupRcv), not 8001 (groupSnd)
    const grokSent = grokChat.sent.find(m => m.chat[1] === GROK_LOCAL && m.text === "Follow-up answer")
    expect(grokSent).toBeDefined()
    expect(grokSent!.inReplyTo).toBe(8002)
  })

  test("replies to last received even if text differs", async () => {
    await reachGrokMode("Initial answer")
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4}},
      _text: "Initial answer",
    })
    // Grok's chat has a message with different text (e.g., previous message arrived but current hasn't yet)
    grokChat.setChatItems(GROK_LOCAL, [
      {chatDir: {type: "groupRcv"}, meta: {itemId: 9001}, _text: "How does encryption work exactly?"},
    ])
    grokApi.willRespond("Partial answer")
    grokChat.sent = []

    await customer.sends("How does encryption work?")

    // Replies to last received item regardless of text match
    const grokSent = grokChat.sent.find(m => m.chat[1] === GROK_LOCAL && m.text === "Partial answer")
    expect(grokSent).toBeDefined()
    expect(grokSent!.inReplyTo).toBe(9001)
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


// ─── 26. parseConfig & parseIdName ───────────────────────────────

describe("parseIdName", () => {
  test("parses valid id:name", () => {
    expect(parseIdName("2:Bob")).toEqual({id: 2, name: "Bob"})
  })

  test("parses name with colons", () => {
    expect(parseIdName("5:Alice:Admin")).toEqual({id: 5, name: "Alice:Admin"})
  })

  test("throws on missing colon", () => {
    expect(() => parseIdName("Bob")).toThrow('Invalid ID:name format: "Bob"')
  })

  test("throws on non-numeric id", () => {
    expect(() => parseIdName("abc:Bob")).toThrow('Invalid ID:name format (non-numeric ID): "abc:Bob"')
  })

  test("throws on colon at start", () => {
    expect(() => parseIdName(":Bob")).toThrow('Invalid ID:name format: ":Bob"')
  })
})

describe("parseConfig --team-members / --team-member aliases", () => {
  const baseArgs = ["--team-group", "Support Team"]

  beforeEach(() => {
    vi.stubEnv("GROK_API_KEY", "test-key")
  })

  afterEach(() => {
    vi.unstubAllEnvs()
  })

  test("--team-members with single member", () => {
    const config = parseConfig([...baseArgs, "--team-members", "2:Bob"])
    expect(config.teamMembers).toEqual([{id: 2, name: "Bob"}])
  })

  test("--team-members with multiple comma-separated members", () => {
    const config = parseConfig([...baseArgs, "--team-members", "2:Bob,5:Alice"])
    expect(config.teamMembers).toEqual([
      {id: 2, name: "Bob"},
      {id: 5, name: "Alice"},
    ])
  })

  test("--team-member with single member", () => {
    const config = parseConfig([...baseArgs, "--team-member", "2:Bob"])
    expect(config.teamMembers).toEqual([{id: 2, name: "Bob"}])
  })

  test("--team-member with multiple comma-separated members", () => {
    const config = parseConfig([...baseArgs, "--team-member", "2:Bob,5:Alice"])
    expect(config.teamMembers).toEqual([
      {id: 2, name: "Bob"},
      {id: 5, name: "Alice"},
    ])
  })

  test("both flags provided → members merged", () => {
    const config = parseConfig([...baseArgs, "--team-members", "2:Bob", "--team-member", "5:Alice"])
    expect(config.teamMembers).toEqual([
      {id: 2, name: "Bob"},
      {id: 5, name: "Alice"},
    ])
  })

  test("both flags with comma-separated values → all merged", () => {
    const config = parseConfig([...baseArgs, "--team-members", "2:Bob,3:Carol", "--team-member", "5:Alice"])
    expect(config.teamMembers).toEqual([
      {id: 2, name: "Bob"},
      {id: 3, name: "Carol"},
      {id: 5, name: "Alice"},
    ])
  })

  test("neither flag → empty array", () => {
    const config = parseConfig(baseArgs)
    expect(config.teamMembers).toEqual([])
  })

  test("other config fields still parsed correctly", () => {
    const config = parseConfig([...baseArgs, "--team-member", "2:Bob", "--timezone", "US/Eastern"])
    expect(config.teamGroup).toEqual({id: 0, name: "Support Team"})
    expect(config.timezone).toBe("US/Eastern")
    expect(config.teamMembers).toEqual([{id: 2, name: "Bob"}])
  })
})


// ─── 27. Message Truncation ──────────────────────────────────

describe("Message Truncation", () => {

  test("short message forwarded unchanged (with !1 NEW! on first)", async () => {
    await customer.sends("Short question")

    teamGroup.received(fmtNewCustomer("Short question", "QUEUE", 1))
  })

  test("message exceeding limit is truncated with suffix", async () => {
    // Create a message that exceeds 15000 bytes when combined with prefix
    const longText = "A".repeat(15000)
    await customer.sends(longText)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const fwdMsg = teamMsgs.find(m => m.startsWith("!1 NEW!"))
    expect(fwdMsg).toBeDefined()
    expect(fwdMsg!.endsWith("… [truncated]")).toBe(true)
    expect(new TextEncoder().encode(fwdMsg!).length).toBeLessThanOrEqual(15000)
  })

  test("prefix is preserved in truncated message", async () => {
    const longText = "B".repeat(15000)
    await customer.sends(longText)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const fwdMsg = teamMsgs.find(m => m.startsWith("!1 NEW!"))
    expect(fwdMsg).toBeDefined()
    // Header is intact at the start (with !1 NEW!)
    expect(fwdMsg!.startsWith(`!1 NEW! *${GROUP_ID}:Alice`)).toBe(true)
    expect(fwdMsg!.endsWith("… [truncated]")).toBe(true)
  })

  test("edit of a long message is also truncated", async () => {
    // Send first message → forwarded to team (stores mapping)
    await customer.sends("Original question")
    // customerChatItem itemId=500, forwarded teamItemId=1000
    mainChat.updatedChatItems = []

    // Simulate edit with very long text — first message still has !1 NEW! marker
    const longEditText = "C".repeat(15000)
    await bot.onChatItemUpdated({
      chatItem: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatItem: {
          chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
          meta: {itemId: 500},
          content: {type: "text", text: longEditText},
          _text: longEditText,
        },
      },
    } as any)

    expect(mainChat.updatedChatItems.length).toBe(1)
    const updatedText = mainChat.updatedChatItems[0].msgContent.text
    expect(updatedText.endsWith("… [truncated]")).toBe(true)
    expect(updatedText.startsWith(`!1 NEW! *${GROUP_ID}:Alice`)).toBe(true)
    expect(new TextEncoder().encode(updatedText).length).toBeLessThanOrEqual(15000)
  })

  test("Grok response to customer group is truncated when too long", async () => {
    const longGrokResponse = "D".repeat(16000)
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")

    grokApi.willRespond(longGrokResponse)
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    // Grok response sent to customer group (via grokChat) should be truncated
    const grokMsgs = grokChat.sentTo(GROK_LOCAL)
    const grokMsg = grokMsgs.find(m => m.endsWith("… [truncated]"))
    expect(grokMsg).toBeDefined()
    expect(new TextEncoder().encode(grokMsg!).length).toBeLessThanOrEqual(15000)
  })

  test("multi-byte characters are not broken by truncation", async () => {
    // Create a message with multi-byte chars that would be split mid-character
    const emoji = "\u{1F600}" // 4-byte emoji
    const longText = emoji.repeat(4000) // 16000 bytes
    await customer.sends(longText)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const fwdMsg = teamMsgs.find(m => m.startsWith("!1 NEW!"))
    expect(fwdMsg).toBeDefined()
    expect(fwdMsg!.endsWith("… [truncated]")).toBe(true)
    // Verify no replacement character (U+FFFD) from broken multi-byte sequences
    expect(fwdMsg!).not.toContain("\uFFFD")
    expect(new TextEncoder().encode(fwdMsg!).length).toBeLessThanOrEqual(15000)
  })
})


// ─── 28. NEW: Prefix ────────────────────────────────────────────

describe("NEW: Prefix", () => {

  test("first customer text gets !1 NEW! prefix in team group", async () => {
    await customer.sends("How do I create a group?")

    teamGroup.received(fmtNewCustomer("How do I create a group?", "QUEUE", 1))
  })

  test("second customer message does NOT get !1 NEW!", async () => {
    await reachTeamQueue("First question")
    mainChat.sent = []

    await customer.sends("More details")

    // Should be forwarded without !1 NEW!
    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    expect(teamMsgs).toContain(fmtCustomer("More details", "QUEUE", 2))
    expect(teamMsgs.some(m => m.includes("!1 NEW!"))).toBe(false)
  })

  test("/grok removes !1 NEW! (team message edited)", async () => {
    await customer.sends("Hello")
    // First message: chatItemId=500, teamItemId=1000
    mainChat.updatedChatItems = []

    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    grokApi.willRespond("Grok answer")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    // Team message should have been edited to remove !1 NEW! → originalText (clean version)
    const update = mainChat.updatedChatItems.find(u => u.chatItemId === 1000)
    expect(update).toBeDefined()
    expect(update!.msgContent.text).toBe(fmtCustomer("Hello", "QUEUE", 1))
  })

  test("/team removes !1 NEW! (team message edited)", async () => {
    await customer.sends("Hello")
    // First message: chatItemId=500, teamItemId=1000
    mainChat.updatedChatItems = []

    mainChat.setGroupMembers(GROUP_ID, [])
    await customer.sends("/team")

    // Team message should have been edited to remove !1 NEW! → originalText
    const update = mainChat.updatedChatItems.find(u => u.chatItemId === 1000)
    expect(update).toBeDefined()
    expect(update!.msgContent.text).toBe(fmtCustomer("Hello", "QUEUE", 1))
  })

  test("/add command removes *NEW:* (team message edited)", async () => {
    await customer.sends("Hello")
    // First message: chatItemId=500, teamItemId=1000
    mainChat.updatedChatItems = []

    // Team member sends /add command in team group
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

    // Team message should have been edited to remove !1 NEW! → originalText
    const update = mainChat.updatedChatItems.find(u => u.chatItemId === 1000)
    expect(update).toBeDefined()
    expect(update!.msgContent.text).toBe(fmtCustomer("Hello", "QUEUE", 1))
  })

  test("customer edit of first message preserves !1 NEW! prefix and updates originalText", async () => {
    await customer.sends("Original question")
    // First message: chatItemId=500, teamItemId=1000
    mainChat.updatedChatItems = []

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
    expect(mainChat.updatedChatItems[0].chatItemId).toBe(1000)
    // Edit should preserve !1 NEW! prefix (stored header is for #1 QUEUE)
    expect(mainChat.updatedChatItems[0].msgContent.text).toBe(fmtNewCustomer("Edited question", "QUEUE", 1))

    // originalText should be updated to the clean version
    const newEntry = (bot as any).newItems.get(GROUP_ID)
    expect(newEntry).toBeDefined()
    expect(newEntry.originalText).toBe(fmtCustomer("Edited question", "QUEUE", 1))
  })

  test("/grok as first message — no *NEW:* created", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    grokApi.willRespond("Hello!")

    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    // No *NEW:* entry created
    expect((bot as any).newItems.has(GROUP_ID)).toBe(false)
  })

  test("/team as first message — no *NEW:* created", async () => {
    mainChat.setGroupMembers(GROUP_ID, [])
    await customer.sends("/team")

    // No *NEW:* entry created
    expect((bot as any).newItems.has(GROUP_ID)).toBe(false)
  })

  test("24h expiry — removeNewPrefix skips edit for old entries", async () => {
    await customer.sends("Hello")
    // First message: chatItemId=500, teamItemId=1000
    mainChat.updatedChatItems = []

    // Manually age the entry to > 24h
    const entry = (bot as any).newItems.get(GROUP_ID)
    entry.timestamp = Date.now() - 25 * 60 * 60 * 1000

    // Trigger removeNewPrefix via /team
    mainChat.setGroupMembers(GROUP_ID, [])
    await customer.sends("/team")

    // newItems should be cleared
    expect((bot as any).newItems.has(GROUP_ID)).toBe(false)
    // But no edit should have been made (expired)
    const update = mainChat.updatedChatItems.find(u => u.chatItemId === 1000)
    expect(update).toBeUndefined()
  })

  test("customer leaves — newItems cleaned up", async () => {
    await customer.sends("Hello")
    expect((bot as any).newItems.has(GROUP_ID)).toBe(true)

    await customer.leaves()

    expect((bot as any).newItems.has(GROUP_ID)).toBe(false)
  })

  test("persistence — restoreNewItems prunes expired entries", () => {
    const now = Date.now()
    const fresh = {teamItemId: 100, timestamp: now - 1000, originalText: "fresh"}
    const expired = {teamItemId: 200, timestamp: now - 25 * 60 * 60 * 1000, originalText: "old"}

    bot.restoreNewItems([
      [GROUP_ID, fresh],
      [300, expired],
    ])

    expect((bot as any).newItems.has(GROUP_ID)).toBe(true)
    expect((bot as any).newItems.has(300)).toBe(false)
    expect((bot as any).newItems.size).toBe(1)
  })

  test("multiple groups — independent tracking", async () => {
    const GROUP_A = 100
    const GROUP_B = 300

    // Group A: first customer message
    const ciA = customerChatItem("Question A", null)
    ciA.chatInfo.groupInfo = businessGroupInfo(GROUP_A, "Alice")
    mainChat.chatItems.set(GROUP_A, [{
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "Question A",
    }])
    await bot.onNewChatItems({chatItems: [ciA]} as any)

    // Group B: first customer message
    const ciB = customerChatItem("Question B", null)
    ciB.chatInfo.groupInfo = businessGroupInfo(GROUP_B, "Charlie")
    ciB.chatItem.chatDir.groupMember.memberId = CUSTOMER_ID
    mainChat.chatItems.set(GROUP_B, [{
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "Question B",
    }])
    await bot.onNewChatItems({chatItems: [ciB]} as any)

    // Both groups should have newItems entries
    expect((bot as any).newItems.has(GROUP_A)).toBe(true)
    expect((bot as any).newItems.has(GROUP_B)).toBe(true)

    // Claim Group A via /team — only removes A's *NEW:*
    mainChat.setGroupMembers(GROUP_A, [])
    mainChat.updatedChatItems = []
    const teamCi = customerChatItem("/team", "team")
    teamCi.chatInfo.groupInfo = businessGroupInfo(GROUP_A, "Alice")
    mainChat.chatItems.get(GROUP_A)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "/team",
      _botCommand: "team",
    })
    await bot.onNewChatItems({chatItems: [teamCi]} as any)

    expect((bot as any).newItems.has(GROUP_A)).toBe(false)
    expect((bot as any).newItems.has(GROUP_B)).toBe(true)
  })

  test("onNewItemsChanged fires on first message", async () => {
    const callback = vi.fn()
    bot.onNewItemsChanged = callback

    await customer.sends("Hello")

    expect(callback).toHaveBeenCalled()
    const lastCallArg = callback.mock.calls[callback.mock.calls.length - 1][0]
    expect(lastCallArg.has(GROUP_ID)).toBe(true)
  })

  test("onNewItemsChanged fires on removal", async () => {
    await customer.sends("Hello")
    const callback = vi.fn()
    bot.onNewItemsChanged = callback

    mainChat.setGroupMembers(GROUP_ID, [])
    await customer.sends("/team")

    expect(callback).toHaveBeenCalled()
    const lastCallArg = callback.mock.calls[callback.mock.calls.length - 1][0]
    expect(lastCallArg.has(GROUP_ID)).toBe(false)
  })
})


// ─── 29. Direct Message Reply ──────────────────────────────────

describe("Direct Message Reply", () => {

  test("direct message → replies with business address redirect", async () => {
    bot.businessAddress = "https://simplex.chat/contact#abc123"

    const ci = {
      chatInfo: {type: "direct", contact: {contactId: 99}},
      chatItem: {
        chatDir: {type: "directRcv"},
        meta: {itemId: 900},
        content: {type: "text", text: "Hello, I have a question"},
        _text: "Hello, I have a question",
      },
    } as any
    await bot.onNewChatItems({chatItems: [ci]} as any)

    const reply = mainChat.sent.find(m => m.chat[0] === "direct" && m.chat[1] === 99)
    expect(reply).toBeDefined()
    expect(reply!.text).toBe(
      "I can't answer your questions on non-business address, please add me through my business address: https://simplex.chat/contact#abc123"
    )
  })

  test("direct message without business address → no reply", async () => {
    bot.businessAddress = null

    const ci = {
      chatInfo: {type: "direct", contact: {contactId: 99}},
      chatItem: {
        chatDir: {type: "directRcv"},
        meta: {itemId: 901},
        content: {type: "text", text: "Hello"},
        _text: "Hello",
      },
    } as any
    await bot.onNewChatItems({chatItems: [ci]} as any)

    const reply = mainChat.sent.find(m => m.chat[0] === "direct" && m.chat[1] === 99)
    expect(reply).toBeUndefined()
  })

  test("direct message does not get forwarded to team group", async () => {
    bot.businessAddress = "https://simplex.chat/contact#abc123"

    const ci = {
      chatInfo: {type: "direct", contact: {contactId: 99}},
      chatItem: {
        chatDir: {type: "directRcv"},
        meta: {itemId: 902},
        content: {type: "text", text: "Some question"},
        _text: "Some question",
      },
    } as any
    await bot.onNewChatItems({chatItems: [ci]} as any)

    teamGroup.receivedNothing()
  })
})


// ─── 30. /inviteall & /invitenew Commands ────────────────────────

function teamGroupCommand(text: string, senderContactId = 2) {
  return {
    chatInfo: {type: "group", groupInfo: {groupId: TEAM_GRP_ID, groupProfile: {displayName: "SupportTeam"}}},
    chatItem: {
      chatDir: {
        type: "groupRcv",
        groupMember: {memberId: "tm-1", groupMemberId: 30, memberContactId: senderContactId, memberProfile: {displayName: "Bob"}},
      },
      meta: {itemId: nextChatItemId++},
      content: {type: "text", text},
      _text: text,
    },
  } as any
}

describe("/inviteall & /invitenew Commands", () => {
  const GROUP_A = 300
  const GROUP_B = 301
  const GROUP_C = 302

  function setGroupLastActive(groups: [number, number][]) {
    bot.restoreGroupLastActive(groups)
  }

  test("/inviteall invites sender to groups active within 24h", async () => {
    const now = Date.now()
    // Group A active 1h ago, Group B active 2h ago — both within 24h
    setGroupLastActive([[GROUP_A, now - 1 * 60 * 60 * 1000], [GROUP_B, now - 2 * 60 * 60 * 1000]])
    mainChat.setGroupMembers(GROUP_A, [])
    mainChat.setGroupMembers(GROUP_B, [])

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/inviteall")]} as any)

    const addedA = mainChat.added.find(a => a.groupId === GROUP_A && a.contactId === 2)
    const addedB = mainChat.added.find(a => a.groupId === GROUP_B && a.contactId === 2)
    expect(addedA).toBeDefined()
    expect(addedB).toBeDefined()
  })

  test("/inviteall skips groups with last activity older than 24h", async () => {
    const now = Date.now()
    // Group A active 25h ago — outside 24h window
    setGroupLastActive([[GROUP_A, now - 25 * 60 * 60 * 1000]])
    mainChat.setGroupMembers(GROUP_A, [])

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/inviteall")]} as any)

    const addedA = mainChat.added.find(a => a.groupId === GROUP_A)
    expect(addedA).toBeUndefined()
  })

  test("/inviteall skips groups where sender is already a member", async () => {
    const now = Date.now()
    setGroupLastActive([[GROUP_A, now - 1 * 60 * 60 * 1000]])
    // Sender (contactId=2) already in group A
    mainChat.setGroupMembers(GROUP_A, [
      {groupMemberId: 70, memberContactId: 2, memberStatus: "connected"},
    ])

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/inviteall")]} as any)

    const addedA = mainChat.added.find(a => a.groupId === GROUP_A && a.contactId === 2)
    expect(addedA).toBeUndefined()
  })

  test("/inviteall sends summary to team group", async () => {
    const now = Date.now()
    setGroupLastActive([[GROUP_A, now - 1 * 60 * 60 * 1000]])
    mainChat.setGroupMembers(GROUP_A, [])

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/inviteall")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const summary = teamMsgs.find(m => m.includes("Invited to") && m.includes("active in 24h"))
    expect(summary).toBeDefined()
  })

  test("/invitenew invites sender only to groups with no grok and no team", async () => {
    const now = Date.now()
    // Group A: no special members, Group B: has team, Group C: has grok
    setGroupLastActive([
      [GROUP_A, now - 1 * 60 * 60 * 1000],
      [GROUP_B, now - 1 * 60 * 60 * 1000],
      [GROUP_C, now - 1 * 60 * 60 * 1000],
    ])
    mainChat.setGroupMembers(GROUP_A, [])
    mainChat.setGroupMembers(GROUP_B, [
      {groupMemberId: 71, memberContactId: 2, memberStatus: "connected"},
    ])
    mainChat.setGroupMembers(GROUP_C, [
      {groupMemberId: 72, memberContactId: 4, memberStatus: "connected"},
    ])

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/invitenew")]} as any)

    const addedA = mainChat.added.find(a => a.groupId === GROUP_A && a.contactId === 2)
    expect(addedA).toBeDefined()
    // B and C should NOT be invited (filtered by composition, not by already-member check)
    const addedB = mainChat.added.find(a => a.groupId === GROUP_B && a.contactId === 2)
    const addedC = mainChat.added.find(a => a.groupId === GROUP_C && a.contactId === 2)
    expect(addedB).toBeUndefined()
    expect(addedC).toBeUndefined()
  })

  test("/invitenew skips groups with grok member", async () => {
    const now = Date.now()
    setGroupLastActive([[GROUP_A, now - 1 * 60 * 60 * 1000]])
    mainChat.setGroupMembers(GROUP_A, [
      {groupMemberId: 72, memberContactId: 4, memberStatus: "connected"},
    ])

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/invitenew")]} as any)

    const addedA = mainChat.added.find(a => a.groupId === GROUP_A)
    expect(addedA).toBeUndefined()
  })

  test("/invitenew skips groups with team member", async () => {
    const now = Date.now()
    setGroupLastActive([[GROUP_A, now - 1 * 60 * 60 * 1000]])
    // Team member contactId=2 already in group as a member (not the sender checking membership —
    // this is the composition check)
    mainChat.setGroupMembers(GROUP_A, [
      {groupMemberId: 71, memberContactId: 2, memberStatus: "connected"},
    ])

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/invitenew")]} as any)

    const addedA = mainChat.added.find(a => a.groupId === GROUP_A)
    expect(addedA).toBeUndefined()
  })

  test("/invitenew skips groups with last activity older than 48h", async () => {
    const now = Date.now()
    setGroupLastActive([[GROUP_A, now - 49 * 60 * 60 * 1000]])
    mainChat.setGroupMembers(GROUP_A, [])

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/invitenew")]} as any)

    const addedA = mainChat.added.find(a => a.groupId === GROUP_A)
    expect(addedA).toBeUndefined()
  })

  test("/inviteall removes !1 NEW! prefix on invited groups", async () => {
    const now = Date.now()
    setGroupLastActive([[GROUP_A, now - 1 * 60 * 60 * 1000]])
    mainChat.setGroupMembers(GROUP_A, [])

    // First, create a NEW item for GROUP_A by simulating first customer message
    mainChat.setChatItems(GROUP_A, [{chatDir: {type: "groupSnd"}, _text: "Welcome!"}])
    const ci = customerChatItem("Help me", null)
    ci.chatInfo.groupInfo = businessGroupInfo(GROUP_A, "TestUser")
    mainChat.chatItems.get(GROUP_A)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "Help me",
    })
    await bot.onNewChatItems({chatItems: [ci]} as any)

    // Verify !1 NEW! prefix was set
    const newMsgs = mainChat.sentTo(TEAM_GRP_ID).filter(m => m.startsWith("!1 NEW!"))
    expect(newMsgs.length).toBe(1)

    mainChat.updatedChatItems = []
    await bot.onNewChatItems({chatItems: [teamGroupCommand("/inviteall")]} as any)

    // NEW prefix should have been removed (apiUpdateChatItem called)
    expect(mainChat.updatedChatItems.length).toBeGreaterThan(0)
    const update = mainChat.updatedChatItems.find(u => u.chatId === TEAM_GRP_ID)
    expect(update).toBeDefined()
    expect(update!.msgContent.text).not.toContain("!1 NEW!")
  })

  test("groupLastActive updated on every customer text message", async () => {
    const callback = vi.fn()
    bot.onGroupLastActiveChanged = callback

    await customer.sends("Hello")
    expect(callback).toHaveBeenCalledTimes(1)

    await customer.sends("Follow up")
    expect(callback).toHaveBeenCalledTimes(2)
  })

  test("groupLastActive NOT updated on non-text events", async () => {
    const callback = vi.fn()
    bot.onGroupLastActiveChanged = callback

    await customer.sendsNonText()

    expect(callback).not.toHaveBeenCalled()
  })

  test("groupLastActive NOT updated on command-only messages (/team)", async () => {
    // Reach teamQueue first so /team doesn't trigger welcome flow
    await reachTeamQueue("Hello")
    const callback = vi.fn()
    bot.onGroupLastActiveChanged = callback

    // /team command should not count as customer text activity
    mainChat.setGroupMembers(GROUP_ID, [])
    await customer.sends("/team")

    expect(callback).not.toHaveBeenCalled()
  })

  test("groupLastActive cleaned up on customer leave", async () => {
    const callback = vi.fn()
    bot.onGroupLastActiveChanged = callback

    await customer.sends("Hello")
    expect(callback).toHaveBeenCalledTimes(1)

    await customer.leaves()
    // Called again on leave (deletion)
    expect(callback).toHaveBeenCalledTimes(2)
  })

  test("restoreGroupLastActive prunes entries older than 48h", async () => {
    const now = Date.now()
    const entries: [number, number][] = [
      [GROUP_A, now - 1 * 60 * 60 * 1000],        // 1h ago — kept
      [GROUP_B, now - 49 * 60 * 60 * 1000],        // 49h ago — pruned
      [GROUP_C, now - 47 * 60 * 60 * 1000],        // 47h ago — kept
    ]

    const freshBot = new SupportBot(mainChat as any, grokChat as any, grokApi as any, config as any)
    freshBot.restoreGroupLastActive(entries)

    // Verify via /inviteall (24h window): only GROUP_A qualifies
    mainChat.setGroupMembers(GROUP_A, [])
    mainChat.setGroupMembers(GROUP_B, [])
    mainChat.setGroupMembers(GROUP_C, [])
    mainChat.added = []

    await freshBot.onNewChatItems({chatItems: [teamGroupCommand("/inviteall")]} as any)

    // GROUP_A (1h ago) → within 24h → invited
    const addedA = mainChat.added.find(a => a.groupId === GROUP_A)
    expect(addedA).toBeDefined()
    // GROUP_B (49h ago) → pruned at restore → not invited
    const addedB = mainChat.added.find(a => a.groupId === GROUP_B)
    expect(addedB).toBeUndefined()
    // GROUP_C (47h ago) → restored but outside 24h → not invited by inviteall
    const addedC = mainChat.added.find(a => a.groupId === GROUP_C)
    expect(addedC).toBeUndefined()
  })
})


// ─── 31. Welcome Flow Deduplication ────────────────────────────

describe("Welcome Flow Deduplication", () => {

  test("teamQueueMessage not re-sent when chat history overflows past 20 items", async () => {
    // First message → welcome flow: teamQueueMessage sent
    await customer.sends("Hello")
    customer.received(TEAM_QUEUE_24H)

    // Simulate long Grok conversation: clear chat items so "forwarded to the team"
    // is no longer in history (as if it scrolled past the 20-item window)
    mainChat.chatItems.set(GROUP_ID, [])
    mainChat.sent = []

    // Next customer message should NOT trigger teamQueueMessage again
    await customer.sends("Follow-up question")

    // Message forwarded to team (normal), but NO teamQueueMessage re-sent
    teamGroup.received(fmtCustomer("Follow-up question", "QUEUE", 2))
    const teamQueueMsgs = mainChat.sentTo(GROUP_ID).filter(m => m.includes("forwarded to the team"))
    expect(teamQueueMsgs.length).toBe(0)
  })

  test("welcomeCompleted cache cleared on customer leave — new customer gets welcome", async () => {
    // First customer triggers welcome
    await customer.sends("Hello")
    customer.received(TEAM_QUEUE_24H)

    // Customer leaves → cache cleared
    await customer.leaves()

    // Clear sent history for clean assertions
    mainChat.sent = []
    mainChat.chatItems.set(GROUP_ID, [])

    // New customer in same group → welcome flow should trigger again
    await customer.sends("New question")
    customer.received(TEAM_QUEUE_24H)
  })

  test("second message in same session never re-sends teamQueueMessage", async () => {
    await customer.sends("First question")
    mainChat.sent = []

    await customer.sends("Second question")

    // Only the forwarded message, no teamQueueMessage
    const customerMsgs = mainChat.sentTo(GROUP_ID)
    expect(customerMsgs.filter(m => m.includes("forwarded to the team")).length).toBe(0)
  })
})


// ─── 32. A1: Reply-to-last Threading ──────────────────────────────

describe("A1: Reply-to-last Threading", () => {

  test("first customer message in new group has no inReplyTo (no prior team item)", async () => {
    await customer.sends("Hello")

    const fwdMsg = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.includes("Hello"))
    expect(fwdMsg).toBeDefined()
    expect(fwdMsg!.inReplyTo).toBeUndefined()
  })

  test("second customer message auto-threads to last team item", async () => {
    await reachTeamQueue("Hello")
    // Hello's teamItemId = 1000
    mainChat.sent = []

    await customer.sends("Follow-up")

    const fwdMsg = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.includes("Follow-up"))
    expect(fwdMsg).toBeDefined()
    // A1: threads to 1000 (last team item for this group)
    expect(fwdMsg!.inReplyTo).toBe(1000)
  })

  test("third message threads to the second message's team item, not the first", async () => {
    await reachTeamQueue("Hello")
    // Hello teamItemId=1000. After reachTeamQueue: nextItemId=1003 (1001=queue msg, 1002=/add)
    await customer.sends("Second msg")
    // Second msg teamItemId = 1003
    mainChat.sent = []

    await customer.sends("Third msg")

    const fwdMsg = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.includes("Third msg"))
    expect(fwdMsg).toBeDefined()
    // A1: threads to 1003 (last team item after second message)
    expect(fwdMsg!.inReplyTo).toBe(1003)
  })

  test("explicit reply-to takes precedence over auto-threading", async () => {
    await reachTeamQueue("Hello")
    // Hello chatItemId=500 → teamItemId=1000. nextItemId=1003.
    await customer.sends("Second msg")
    // Second chatItemId=501 → teamItemId=1003 (lastTeamItemByGroup=1003)
    mainChat.sent = []

    // Reply to the original "Hello" (chatItemId=500 → teamItemId=1000)
    await customer.sendsReplyTo("Reply to hello", 500)

    const fwdMsg = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.includes("Reply to hello"))
    expect(fwdMsg).toBeDefined()
    // Explicit reply-to (1000) takes precedence over auto-thread (1003)
    expect(fwdMsg!.inReplyTo).toBe(1000)
  })

  test("team member message also updates lastTeamItemByGroup", async () => {
    await reachTeamPending()
    // Hello teamItemId=1000. /team didn't forward.
    await teamMember.sends("I'll help")
    // Team member's teamItemId = 1004
    mainChat.sent = []

    await customer.sends("Thanks!")

    const fwdMsg = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.includes("Thanks!"))
    expect(fwdMsg).toBeDefined()
    // A1: threads to 1004 (team member's forwarded item)
    expect(fwdMsg!.inReplyTo).toBe(1004)
  })

  test("grok response also updates lastTeamItemByGroup", async () => {
    await reachGrokMode("Grok answer")
    // Hello teamItemId=1000. After reachTeamQueue: nextItemId=1003. Grok activated msg=1003.
    // activateGrok: Grok response forwarded → teamItemId=1004
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4}},
      _text: "Grok answer",
    })
    grokApi.willRespond("More answer")
    mainChat.sent = []

    await customer.sends("Follow-up")

    const custFwd = mainChat.sent.find(m =>
      m.chat[1] === TEAM_GRP_ID && m.text.includes("Follow-up"))
    expect(custFwd).toBeDefined()
    // Customer follow-up should thread to grok response's team item (1004)
    expect(custFwd!.inReplyTo).toBe(1004)
  })

  test("customer leave clears lastTeamItemByGroup for that group", async () => {
    await reachTeamQueue("Hello")
    expect((bot as any).lastTeamItemByGroup.has(GROUP_ID)).toBe(true)

    await customer.leaves()

    expect((bot as any).lastTeamItemByGroup.has(GROUP_ID)).toBe(false)
  })

  test("customer leave clears forwardedItems for that group", async () => {
    await reachTeamQueue("Hello")
    // After reachTeamQueue, forwardedItems has entry for "100:500" (Hello chatItemId=500)
    expect((bot as any).forwardedItems.size).toBeGreaterThan(0)
    const hasGroupEntry = [...(bot as any).forwardedItems.keys()].some((k: string) => k.startsWith(`${GROUP_ID}:`))
    expect(hasGroupEntry).toBe(true)

    await customer.leaves()

    const hasGroupEntryAfter = [...(bot as any).forwardedItems.keys()].some((k: string) => k.startsWith(`${GROUP_ID}:`))
    expect(hasGroupEntryAfter).toBe(false)
  })
})


// ─── 33. A6: Non-Text Content Indicators ──────────────────────────

describe("A6: Non-Text Content Indicators", () => {

  test("image message → _[image]_ indicator in team forward", async () => {
    // First message to get past welcome
    await reachTeamQueue("Hello")
    mainChat.sent = []

    // Send image with caption
    const ci = {
      chatInfo: {type: "group", groupInfo: businessGroupInfo()},
      chatItem: {
        chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
        meta: {itemId: nextChatItemId++},
        content: {type: "rcvMsgContent", msgContent: {type: "image", text: "check this"}},
        _text: "check this",
      },
    } as any
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "check this",
    })
    await bot.onNewChatItems({chatItems: [ci]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const fwd = teamMsgs.find(m => m.includes("_[image]_") && m.includes("check this"))
    expect(fwd).toBeDefined()
  })

  test("file message without caption → _[file]_ only", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    const ci = {
      chatInfo: {type: "group", groupInfo: businessGroupInfo()},
      chatItem: {
        chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
        meta: {itemId: nextChatItemId++},
        content: {type: "rcvMsgContent", msgContent: {type: "file", text: ""}},
        _text: null,
      },
    } as any
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: null,
    })
    await bot.onNewChatItems({chatItems: [ci]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const fwd = teamMsgs.find(m => m.includes("_[file]_"))
    expect(fwd).toBeDefined()
  })

  test("voice message → _[voice]_ indicator", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    const ci = {
      chatInfo: {type: "group", groupInfo: businessGroupInfo()},
      chatItem: {
        chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
        meta: {itemId: nextChatItemId++},
        content: {type: "rcvMsgContent", msgContent: {type: "voice", text: "", duration: 5}},
        _text: null,
      },
    } as any
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: null,
    })
    await bot.onNewChatItems({chatItems: [ci]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const fwd = teamMsgs.find(m => m.includes("_[voice]_"))
    expect(fwd).toBeDefined()
  })

  test("video message with caption → _[video]_ caption", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    const ci = {
      chatInfo: {type: "group", groupInfo: businessGroupInfo()},
      chatItem: {
        chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
        meta: {itemId: nextChatItemId++},
        content: {type: "rcvMsgContent", msgContent: {type: "video", text: "my screen recording"}},
        _text: "my screen recording",
      },
    } as any
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
      _text: "my screen recording",
    })
    await bot.onNewChatItems({chatItems: [ci]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const fwd = teamMsgs.find(m => m.includes("_[video]_") && m.includes("my screen recording"))
    expect(fwd).toBeDefined()
  })

  test("regular text message has no content type indicator", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    await customer.sends("Just text")

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const fwd = teamMsgs.find(m => m.includes("Just text"))
    expect(fwd).toBeDefined()
    expect(fwd).not.toContain("_[")
  })
})


// ─── 34. D1: /pending Command ─────────────────────────────────────

describe("D1: /pending Command", () => {

  test("/pending with no active groups → 'No pending conversations.'", async () => {
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/pending")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    expect(teamMsgs).toContain("No pending conversations.")
  })

  test("/pending with customer message (no grok/team reply) → listed as pending", async () => {
    await customer.sends("Help me")
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/pending")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const pendingMsg = teamMsgs.find(m => m.includes("*Pending"))
    expect(pendingMsg).toBeDefined()
    expect(pendingMsg).toContain(`${GROUP_ID}:Alice`)
    expect(pendingMsg).toContain("QUEUE")
  })

  test("/pending: grok response makes group not pending", async () => {
    await reachGrokMode("Grok answer")
    // After Grok answer, last event is from grok → not pending
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/pending")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    expect(teamMsgs).toContain("No pending conversations.")
  })

  test("/pending: team member response makes group not pending", async () => {
    await reachTeamLocked()
    // After team member msg, last event is from team → not pending
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/pending")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    expect(teamMsgs).toContain("No pending conversations.")
  })

  test("/pending: customer message after grok → pending again", async () => {
    await reachGrokMode("Grok answer")
    // Grok answered → not pending
    // Customer sends follow-up in grok mode
    mainChat.chatItems.get(GROUP_ID)!.push({
      chatDir: {type: "groupRcv", groupMember: {memberId: "grok-1", groupMemberId: 60, memberContactId: 4}},
      _text: "Grok answer",
    })
    grokApi.willRespond("Follow-up answer")
    await customer.sends("More questions")
    // Customer message updates pending to "customer" → but then Grok responds, updating to "grok"
    // So after this, last event is from grok (the follow-up answer)
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/pending")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    // Grok responded last, so not pending
    expect(teamMsgs).toContain("No pending conversations.")
  })

  test("/pending: customer reaction while last message is from team → not pending", async () => {
    await reachTeamLocked()
    // Team member sent last message → not pending
    // Now customer reacts
    await bot.onChatItemReaction({
      added: true,
      reaction: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatReaction: {
          chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
          chatItem: {meta: {itemId: 500}},
          sentAt: new Date().toISOString(),
          reaction: {type: "emoji", emoji: "👍"},
        },
      },
    } as any)
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/pending")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    // Customer reaction, but last message was from team → not pending
    expect(teamMsgs).toContain("No pending conversations.")
  })

  test("/pending: team reaction makes group not pending", async () => {
    await customer.sends("Need help")
    // Customer msg → pending
    // Team member reacts
    await bot.onChatItemReaction({
      added: true,
      reaction: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatReaction: {
          chatDir: {type: "groupRcv", groupMember: {memberId: "team-member-1", groupMemberId: 50, memberContactId: 2}},
          chatItem: {meta: {itemId: 500}},
          sentAt: new Date().toISOString(),
          reaction: {type: "emoji", emoji: "👍"},
        },
      },
    } as any)
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/pending")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    // Team reacted → not pending
    expect(teamMsgs).toContain("No pending conversations.")
  })

  test("/pending: customer reaction while last message is from customer → still pending", async () => {
    await customer.sends("Help me")
    // Customer msg → pending (last event: customer message)
    // Customer reacts (last event: customer reaction, last message: customer)
    await bot.onChatItemReaction({
      added: true,
      reaction: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatReaction: {
          chatDir: {type: "groupRcv", groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10}},
          chatItem: {meta: {itemId: 500}},
          sentAt: new Date().toISOString(),
          reaction: {type: "emoji", emoji: "👍"},
        },
      },
    } as any)
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/pending")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    // Customer reaction AND last message was from customer → still pending
    const pendingMsg = teamMsgs.find(m => m.includes("*Pending"))
    expect(pendingMsg).toBeDefined()
    expect(pendingMsg).toContain(`${GROUP_ID}:Alice`)
  })

  test("/pending: non-business-chat group reaction → ignored", async () => {
    // Reaction in non-business group should not crash
    await bot.onChatItemReaction({
      added: true,
      reaction: {
        chatInfo: {type: "group", groupInfo: {groupId: 999, groupProfile: {displayName: "X"}}},
        chatReaction: {
          chatDir: {type: "groupRcv", groupMember: {memberId: "someone"}},
          chatItem: {meta: {itemId: 1}},
          sentAt: new Date().toISOString(),
          reaction: {type: "emoji", emoji: "👍"},
        },
      },
    } as any)
    // No crash = success
  })

  test("/pending: removed reaction (added=false) → ignored", async () => {
    await customer.sends("Help me")
    // Customer msg → pending
    mainChat.sent = []

    // Team removes reaction
    await bot.onChatItemReaction({
      added: false,
      reaction: {
        chatInfo: {type: "group", groupInfo: businessGroupInfo()},
        chatReaction: {
          chatDir: {type: "groupRcv", groupMember: {memberId: "team-member-1", groupMemberId: 50, memberContactId: 2}},
          chatItem: {meta: {itemId: 500}},
          sentAt: new Date().toISOString(),
          reaction: {type: "emoji", emoji: "👍"},
        },
      },
    } as any)

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/pending")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    // Removed reaction should be ignored → still pending (customer msg was last real event)
    const pendingMsg = teamMsgs.find(m => m.includes("*Pending"))
    expect(pendingMsg).toBeDefined()
  })

  test("/pending: group with no pending info but with lastActive → listed as pending", async () => {
    // Simulate a group that has lastActive but no pendingInfo (e.g., after restart)
    bot.restoreGroupLastActive([[GROUP_ID, Date.now()]])
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [teamGroupCommand("/pending")]} as any)

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const pendingMsg = teamMsgs.find(m => m.includes("*Pending"))
    expect(pendingMsg).toBeDefined()
  })

  test("groupPendingInfo cleaned up on customer leave", async () => {
    await customer.sends("Hello")
    expect((bot as any).groupPendingInfo.has(GROUP_ID)).toBe(true)

    await customer.leaves()

    expect((bot as any).groupPendingInfo.has(GROUP_ID)).toBe(false)
  })

  test("groupMetadata cleaned up on customer leave", async () => {
    await customer.sends("Hello")
    expect((bot as any).groupMetadata.has(GROUP_ID)).toBe(true)

    await customer.leaves()

    expect((bot as any).groupMetadata.has(GROUP_ID)).toBe(false)
  })

  test("restoreGroupMetadata works", () => {
    const meta = {firstContact: 1000000, msgCount: 5, customerName: "Test"}
    bot.restoreGroupMetadata([[GROUP_ID, meta]])

    expect((bot as any).groupMetadata.get(GROUP_ID)).toEqual(meta)
  })

  test("restoreGroupPendingInfo works", () => {
    const info = {lastEventType: "message" as const, lastEventFrom: "customer" as const, lastEventTimestamp: Date.now(), lastMessageFrom: "customer" as const}
    bot.restoreGroupPendingInfo([[GROUP_ID, info]])

    expect((bot as any).groupPendingInfo.get(GROUP_ID)).toEqual(info)
  })

  test("onGroupMetadataChanged fires on customer message", async () => {
    const callback = vi.fn()
    bot.onGroupMetadataChanged = callback

    await customer.sends("Hello")

    expect(callback).toHaveBeenCalled()
  })

  test("onGroupPendingInfoChanged fires on customer message", async () => {
    const callback = vi.fn()
    bot.onGroupPendingInfoChanged = callback

    await customer.sends("Hello")

    expect(callback).toHaveBeenCalled()
  })
})


// ─── 35. Welcome Flow After Command-First Interaction ──────────

describe("Welcome Flow After Command-First Interaction", () => {
  afterEach(() => vi.useRealTimers())

  test("/grok as first command then text → no duplicate welcome", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    grokApi.willRespond("AI answer")
    const p = customer.sends("/grok")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])
    await grokAgent.joins()
    await p

    // Now customer sends text — should NOT trigger teamQueueMessage
    grokApi.willRespond("Follow-up answer")
    await customer.sends("Help me with something")

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const hasQueueMsg = teamMsgs.some(m => m.includes("forwarded to the team"))
    expect(hasQueueMsg).toBe(false)
    const hasNewMarker = teamMsgs.some(m => m.includes("!1 NEW!"))
    expect(hasNewMarker).toBe(false)
  })

  test("/grok timeout as first command then text → no duplicate welcome", async () => {
    vi.useFakeTimers()
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    const p = customer.sends("/grok")
    await grokAgent.timesOut()
    await p
    vi.useRealTimers()

    // Customer sends text — welcomeCompleted stays set, no duplicate welcome
    await customer.sends("Hello")

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const hasQueueMsg = teamMsgs.some(m => m.includes("forwarded to the team"))
    expect(hasQueueMsg).toBe(false)
    const hasNewMarker = teamMsgs.some(m => m.includes("!1 NEW!"))
    expect(hasNewMarker).toBe(false)
  })

  test("/team as first command then text → no duplicate welcome", async () => {
    mainChat.setNextGroupMemberId(50)
    lastTeamMemberGId = 50
    mainChat.setGroupMembers(GROUP_ID, [])
    await customer.sends("/team")
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 50, memberContactId: 2, memberStatus: "connected"},
    ])

    await customer.sends("Can you help me?")

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const hasQueueMsg = teamMsgs.some(m => m.includes("forwarded to the team"))
    expect(hasQueueMsg).toBe(false)
  })

  test("/team when already activated before → sets welcomeCompleted", async () => {
    mainChat.setChatItems(GROUP_ID, [
      {chatDir: {type: "groupSnd"}, _text: "A team member has been added and will reply within 24 hours."},
    ])
    mainChat.setGroupMembers(GROUP_ID, [])
    await customer.sends("/team")
    customer.received("A team member has already been invited to this conversation and will reply when available.")

    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 50, memberContactId: 2, memberStatus: "connected"},
    ])

    await customer.sends("Still need help")

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const hasQueueMsg = teamMsgs.some(m => m.includes("forwarded to the team"))
    expect(hasQueueMsg).toBe(false)
  })

  test("isFirstCustomerMessage detects grokActivatedMessage in history (restart resilience)", async () => {
    // Simulate post-restart: history has grokActivatedMessage but welcomeCompleted is empty
    mainChat.setChatItems(GROUP_ID, [
      {chatDir: {type: "groupSnd"}, _text: "You are now chatting with Grok. You can send questions in any language."},
    ])
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 60, memberContactId: 4, memberStatus: "connected"},
    ])

    grokApi.willRespond("answer")
    await customer.sends("Hello")

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const hasQueueMsg = teamMsgs.some(m => m.includes("forwarded to the team"))
    expect(hasQueueMsg).toBe(false)
    const hasNewMarker = teamMsgs.some(m => m.includes("!1 NEW!"))
    expect(hasNewMarker).toBe(false)
  })

  test("isFirstCustomerMessage detects teamAddedMessage in history (restart resilience)", async () => {
    // Simulate post-restart: history has teamAddedMessage but welcomeCompleted is empty
    mainChat.setChatItems(GROUP_ID, [
      {chatDir: {type: "groupSnd"}, _text: "A team member has been added and will reply within 24 hours."},
    ])
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 50, memberContactId: 2, memberStatus: "connected"},
    ])

    await customer.sends("Hello")

    const teamMsgs = mainChat.sentTo(TEAM_GRP_ID)
    const hasQueueMsg = teamMsgs.some(m => m.includes("forwarded to the team"))
    expect(hasQueueMsg).toBe(false)
    const hasNewMarker = teamMsgs.some(m => m.includes("!1 NEW!"))
    expect(hasNewMarker).toBe(false)
  })
})
