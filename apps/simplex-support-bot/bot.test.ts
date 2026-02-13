// ═══════════════════════════════════════════════════════════════════
// SimpleX Support Bot — Acceptance Tests
// ═══════════════════════════════════════════════════════════════════
//
// Human-readable TypeScript tests for the support bot.
// Uses a conversation DSL: users are variables, actions use await,
// assertions use .received() / .stateIs().
//
// Grok API is mocked. All scenarios from the product specification
// and implementation plan are covered.
// ═══════════════════════════════════════════════════════════════════

import {describe, test, expect, beforeEach, vi} from "vitest"

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
  T: {ChatType: {Group: "group"}, GroupMemberRole: {Member: "member"}},
  CEvt: {},
}))

vi.mock("./src/util", () => ({
  isWeekend: vi.fn(() => false),
  log: vi.fn(),
  logError: vi.fn(),
}))

// ─── Imports (after mocks) ───────────────────────────────────────

import {SupportBot} from "./src/bot"
import type {GrokMessage} from "./src/state"
import {isWeekend} from "./src/util"


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

  private addMemberFail = false
  private addMemberDuplicate = false
  private nextMemberGId = 50

  apiAddMemberWillFail()              { this.addMemberFail = true }
  apiAddMemberWillDuplicate()         { this.addMemberDuplicate = true }
  setNextGroupMemberId(id: number)    { this.nextMemberGId = id }
  setGroupMembers(groupId: number, members: any[]) { this.members.set(groupId, members) }

  async apiSendTextMessage(chat: [string, number], text: string) {
    this.sent.push({chat, text})
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
    return {groupMemberId: gid, memberId: `member-${gid}`}
  }

  async apiRemoveMembers(groupId: number, memberIds: number[]) {
    this.removed.push({groupId, memberIds})
  }

  async apiJoinGroup(groupId: number) {
    this.joined.push(groupId)
  }

  async apiListMembers(groupId: number) {
    return this.members.get(groupId) || []
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
    this.members.clear()
    this.addMemberFail = false; this.addMemberDuplicate = false; this.nextMemberGId = 50
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

function customerChatItem(text: string | null, command: string | null = null) {
  return {
    chatInfo: {type: "group", groupInfo: businessGroupInfo()},
    chatItem: {
      chatDir: {
        type: "groupRcv",
        groupMember: {memberId: CUSTOMER_ID, groupMemberId: 10},
      },
      content: {type: "text", text: text ?? ""},
      _botCommand: command,
      _text: text,
    },
  } as any
}

function teamMemberChatItem(teamMemberGId: number, text: string) {
  return {
    chatInfo: {type: "group", groupInfo: businessGroupInfo()},
    chatItem: {
      chatDir: {
        type: "groupRcv",
        groupMember: {memberId: "team-member-1", groupMemberId: teamMemberGId},
      },
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
        groupMember: {memberId: "grok-1", groupMemberId: grokMemberGId},
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
// Thin wrappers that make test bodies read like conversations.
//
// IMPORTANT: activateGrok internally blocks on waitForGrokJoin.
// When testing /grok activation, do NOT await customer.sends("/grok")
// before grokAgent.joins(). Instead use:
//
//   const p = customer.sends("/grok")   // starts, blocks at waitForGrokJoin
//   await grokAgent.joins()             // resolves the join
//   await p                             // activateGrok completes
//
// All assertions must come after `await p`.

let bot: SupportBot
let mainChat: MockChatApi
let grokChat: MockChatApi
let grokApi: MockGrokApi
let lastTeamMemberGId: number
let lastGrokMemberGId: number

const customer = {
  async connects(groupId = GROUP_ID) {
    bot.onBusinessRequest({groupInfo: businessGroupInfo(groupId)} as any)
  },

  async sends(text: string, groupId = GROUP_ID) {
    const isGrokCmd = text === "/grok"
    const isTeamCmd = text === "/team"
    const command = isGrokCmd ? "grok" : isTeamCmd ? "team" : null
    const ci = customerChatItem(text, command)
    ci.chatInfo.groupInfo = businessGroupInfo(groupId)
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
    await bot.onNewChatItems({chatItems: [ci]} as any)
  },

  async leaves(groupId = GROUP_ID) {
    await bot.onLeftMember({
      groupInfo: businessGroupInfo(groupId),
      member: {memberId: "team-member-1", groupMemberId: lastTeamMemberGId},
    } as any)
  },
}

const grokAgent = {
  wasInvited(groupId = GROUP_ID) {
    const found = mainChat.added.some(a => a.groupId === groupId && a.contactId === 4)
    expect(found).toBe(true)
  },

  async joins() {
    // Flush microtasks so activateGrok reaches waitForGrokJoin before we resolve it.
    // activateGrok does: await apiAddMember → pendingGrokJoins.set → await sendToGroup → await waitForGrokJoin
    // Each await creates a microtask. setTimeout(r, 0) fires after all microtasks drain.
    await new Promise<void>(r => setTimeout(r, 0))
    const memberId = `member-${lastGrokMemberGId}`
    await bot.onGrokGroupInvitation({
      groupInfo: {
        groupId: GROK_LOCAL,
        membership: {memberId},
      },
    } as any)
    // Waiter resolves on connectedToGroupMember, not on apiJoinGroup
    bot.onGrokMemberConnected({
      groupInfo: {groupId: GROK_LOCAL},
      member: {memberProfile: {displayName: "Bot"}},
    } as any)
  },

  async timesOut() {
    // Advance fake timers past the 30s join timeout.
    // advanceTimersByTimeAsync interleaves microtask processing, so activateGrok's
    // internal awaits (apiAddMember, sendToGroup) complete before the 30s timeout fires.
    await vi.advanceTimersByTimeAsync(30_001)
  },

  wasRemoved(groupId = GROUP_ID) {
    const found = mainChat.removed.some(
      r => r.groupId === groupId && r.memberIds.includes(lastGrokMemberGId)
    )
    expect(found).toBe(true)
  },

  async leaves(groupId = GROUP_ID) {
    await bot.onLeftMember({
      groupInfo: businessGroupInfo(groupId),
      member: {memberId: "grok-1", groupMemberId: lastGrokMemberGId},
    } as any)
  },
}

function stateIs(groupId: number, expectedType: string) {
  const state = (bot as any).conversations.get(groupId)
  expect(state).toBeDefined()
  expect(state.type).toBe(expectedType)
}

function hasNoState(groupId: number) {
  expect((bot as any).conversations.has(groupId)).toBe(false)
}


// ─── Constants ──────────────────────────────────────────────────

const TEAM_QUEUE_24H =
  `Thank you for your message, it is forwarded to the team.\n` +
  `It may take a team member up to 24 hours to reply.\n\n` +
  `Click /grok if your question is about SimpleX apps or network, is not sensitive, ` +
  `and you want Grok LLM to answer it right away. *Your previous message and all ` +
  `subsequent messages will be forwarded to Grok* until you click /team. You can ask ` +
  `Grok questions in any language and it will not see your profile name.\n\n` +
  `We appreciate if you try Grok: you can learn a lot about SimpleX Chat from it. ` +
  `It is objective, answers the way our team would, and it saves our team time.`

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
  // Track the groupMemberIds that apiAddMember returns
  mainChat.setNextGroupMemberId(50)
  lastTeamMemberGId = 50
  lastGrokMemberGId = 50
  bot = new SupportBot(mainChat as any, grokChat as any, grokApi as any, config as any)
  // Reset isWeekend mock to default (weekday)
  vi.mocked(isWeekend).mockReturnValue(false)
})


// ─── State Helpers ──────────────────────────────────────────────

async function reachTeamQueue(...messages: string[]) {
  await customer.connects()
  await customer.sends(messages[0] || "Hello")
  for (const msg of messages.slice(1)) {
    await customer.sends(msg)
  }
}

async function reachGrokMode(grokResponse = "Grok answer") {
  mainChat.setNextGroupMemberId(60)
  lastGrokMemberGId = 60
  await reachTeamQueue("Hello")
  grokApi.willRespond(grokResponse)
  // Non-awaiting pattern: activateGrok blocks on waitForGrokJoin
  const p = customer.sends("/grok")
  await grokAgent.joins()
  await p
}

async function reachTeamPending() {
  mainChat.setNextGroupMemberId(50)
  lastTeamMemberGId = 50
  await reachTeamQueue("Hello")
  await customer.sends("/team")
}

async function reachTeamLocked() {
  await reachTeamPending()
  await teamMember.sends("I'll help you")
}


// ═══════════════════════════════════════════════════════════════
//  TESTS
// ═══════════════════════════════════════════════════════════════


// ─── 1. Connection & Welcome ────────────────────────────────────

describe("Connection & Welcome", () => {

  test("new customer connects → welcome state", async () => {
    await customer.connects()

    stateIs(GROUP_ID, "welcome")
  })

  test("first message → forwarded to team, queue reply, teamQueue state", async () => {
    await customer.connects()

    await customer.sends("How do I create a group?")

    teamGroup.received("[Alice #100]\nHow do I create a group?")
    customer.received(TEAM_QUEUE_24H)
    stateIs(GROUP_ID, "teamQueue")
  })

  test("non-text message in welcome → ignored", async () => {
    await customer.connects()

    await customer.sendsNonText()

    stateIs(GROUP_ID, "welcome")
  })
})


// ─── 2. Team Queue ──────────────────────────────────────────────

describe("Team Queue", () => {

  test("additional messages forwarded to team, no second queue reply", async () => {
    await reachTeamQueue("First question")
    mainChat.sent = []   // clear previous messages

    await customer.sends("More details about my issue")

    teamGroup.received("[Alice #100]\nMore details about my issue")
    // No queue message sent again — only on first message
    expect(mainChat.sentTo(GROUP_ID).length).toBe(0)
    stateIs(GROUP_ID, "teamQueue")
  })

  test("multiple messages accumulate in userMessages", async () => {
    await customer.connects()

    await customer.sends("Question 1")
    await customer.sends("Question 2")
    await customer.sends("Question 3")

    teamGroup.received("[Alice #100]\nQuestion 1")
    teamGroup.received("[Alice #100]\nQuestion 2")
    teamGroup.received("[Alice #100]\nQuestion 3")

    const state = (bot as any).conversations.get(GROUP_ID)
    expect(state.userMessages).toEqual(["Question 1", "Question 2", "Question 3"])
  })

  test("non-text message in teamQueue → ignored", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    await customer.sendsNonText()

    expect(mainChat.sent.length).toBe(0)
    stateIs(GROUP_ID, "teamQueue")
  })

  test("unrecognized /command treated as normal text message", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    await customer.sends("/unknown")

    teamGroup.received("[Alice #100]\n/unknown")
    stateIs(GROUP_ID, "teamQueue")
  })
})


// ─── 3. Grok Activation ────────────────────────────────────────

describe("Grok Activation", () => {

  test("/grok → Grok invited, activated, API called, response sent", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("How do I create a group?")

    grokApi.willRespond("To create a group, go to Settings > New Group.")
    // Non-awaiting pattern: activateGrok blocks on waitForGrokJoin
    const p = customer.sends("/grok")
    await grokAgent.joins()
    await p

    grokAgent.wasInvited()
    customer.received(GROK_ACTIVATED)

    // Grok API called with empty history + accumulated message
    expect(grokApi.lastCall().history).toEqual([])
    expect(grokApi.lastCall().message).toBe("How do I create a group?")

    // Grok response sent via Grok identity
    customer.receivedFromGrok("To create a group, go to Settings > New Group.")

    stateIs(GROUP_ID, "grokMode")
  })

  test("/grok with multiple accumulated messages → joined with newline", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Question about groups", "Also, how do I add members?")

    grokApi.willRespond("Here's how to do both...")
    const p = customer.sends("/grok")
    await grokAgent.joins()
    await p

    expect(grokApi.lastCall().message).toBe(
      "Question about groups\nAlso, how do I add members?"
    )
    customer.receivedFromGrok("Here's how to do both...")
    stateIs(GROUP_ID, "grokMode")
  })
})


// ─── 4. Grok Mode Conversation ─────────────────────────────────

describe("Grok Mode Conversation", () => {

  test("user messages forwarded to both Grok API and team group", async () => {
    await reachGrokMode("Initial answer")
    mainChat.sent = []

    grokApi.willRespond("Follow-up answer from Grok")
    await customer.sends("What about encryption?")

    teamGroup.received("[Alice #100]\nWhat about encryption?")

    expect(grokApi.lastCall().history).toEqual([
      {role: "user",      content: "Hello"},
      {role: "assistant", content: "Initial answer"},
    ])
    expect(grokApi.lastCall().message).toBe("What about encryption?")

    customer.receivedFromGrok("Follow-up answer from Grok")
    stateIs(GROUP_ID, "grokMode")
  })

  test("conversation history grows with each exchange", async () => {
    await reachGrokMode("Answer 1")

    grokApi.willRespond("Answer 2")
    await customer.sends("Follow-up 1")

    expect(grokApi.lastCall().history).toEqual([
      {role: "user",      content: "Hello"},
      {role: "assistant", content: "Answer 1"},
    ])

    grokApi.willRespond("Answer 3")
    await customer.sends("Follow-up 2")

    expect(grokApi.lastCall().history).toEqual([
      {role: "user",      content: "Hello"},
      {role: "assistant", content: "Answer 1"},
      {role: "user",      content: "Follow-up 1"},
      {role: "assistant", content: "Answer 2"},
    ])
  })

  test("/grok in grokMode → silently ignored", async () => {
    await reachGrokMode()
    mainChat.sent = []
    grokApi.reset()

    await customer.sends("/grok")

    expect(mainChat.sent.length).toBe(0)
    expect(grokApi.callCount()).toBe(0)
    stateIs(GROUP_ID, "grokMode")
  })

  test("non-text message in grokMode → ignored", async () => {
    await reachGrokMode()
    mainChat.sent = []
    grokApi.reset()

    await customer.sendsNonText()

    expect(mainChat.sent.length).toBe(0)
    expect(grokApi.callCount()).toBe(0)
    stateIs(GROUP_ID, "grokMode")
  })
})


// ─── 5. Team Activation ────────────────────────────────────────

describe("Team Activation", () => {

  test("/team from teamQueue → team member invited, teamPending", async () => {
    mainChat.setNextGroupMemberId(50)
    lastTeamMemberGId = 50
    await reachTeamQueue("Hello")
    mainChat.sent = []

    await customer.sends("/team")

    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)
    stateIs(GROUP_ID, "teamPending")
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
    stateIs(GROUP_ID, "teamPending")
  })
})


// ─── 6. One-Way Gate ────────────────────────────────────────────

describe("One-Way Gate", () => {

  test("/grok in teamPending → 'team mode' reply", async () => {
    await reachTeamPending()
    mainChat.sent = []

    await customer.sends("/grok")

    customer.received(TEAM_LOCKED_MSG)
    stateIs(GROUP_ID, "teamPending")
  })

  test("team member sends message → teamLocked", async () => {
    await reachTeamPending()

    await teamMember.sends("I'll help you with that")

    stateIs(GROUP_ID, "teamLocked")
  })

  test("/grok in teamLocked → 'team mode' reply", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await customer.sends("/grok")

    customer.received(TEAM_LOCKED_MSG)
    stateIs(GROUP_ID, "teamLocked")
  })

  test("/team in teamPending → silently ignored", async () => {
    await reachTeamPending()
    mainChat.sent = []

    await customer.sends("/team")

    expect(mainChat.sent.length).toBe(0)
    stateIs(GROUP_ID, "teamPending")
  })

  test("/team in teamLocked → silently ignored", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await customer.sends("/team")

    expect(mainChat.sent.length).toBe(0)
    stateIs(GROUP_ID, "teamLocked")
  })

  test("customer text in teamPending → no forwarding, no reply", async () => {
    await reachTeamPending()
    mainChat.sent = []

    await customer.sends("Here's more info about my issue")

    expect(mainChat.sent.length).toBe(0)
    stateIs(GROUP_ID, "teamPending")
  })

  test("customer text in teamLocked → no forwarding, no reply", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await customer.sends("Thank you!")

    expect(mainChat.sent.length).toBe(0)
    stateIs(GROUP_ID, "teamLocked")
  })
})


// ─── 7. Gate Reversal vs Irreversibility ────────────────────────

describe("Gate Reversal vs Irreversibility", () => {

  test("team member leaves in teamPending → revert to teamQueue", async () => {
    await reachTeamPending()

    await teamMember.leaves()

    stateIs(GROUP_ID, "teamQueue")
  })

  test("after teamPending revert, /grok works again", async () => {
    await reachTeamPending()
    await teamMember.leaves()
    // Now back in teamQueue
    mainChat.setNextGroupMemberId(61)
    lastGrokMemberGId = 61

    grokApi.willRespond("Grok is back")
    const p = customer.sends("/grok")
    await grokAgent.joins()
    await p

    customer.receivedFromGrok("Grok is back")
    stateIs(GROUP_ID, "grokMode")
  })

  test("team member leaves in teamLocked → replacement added, stays locked", async () => {
    await reachTeamLocked()
    mainChat.added = []

    await teamMember.leaves()

    // Replacement team member invited, state stays teamLocked
    expect(mainChat.added.length).toBe(1)
    expect(mainChat.added[0].contactId).toBe(2)
    stateIs(GROUP_ID, "teamLocked")
  })

  test("/grok still rejected after replacement in teamLocked", async () => {
    await reachTeamLocked()
    await teamMember.leaves()
    mainChat.sent = []

    await customer.sends("/grok")

    customer.received(TEAM_LOCKED_MSG)
    stateIs(GROUP_ID, "teamLocked")
  })
})


// ─── 8. Member Leave & Cleanup ──────────────────────────────────

describe("Member Leave & Cleanup", () => {

  test("customer leaves → state deleted", async () => {
    await reachTeamQueue("Hello")

    await customer.leaves()

    hasNoState(GROUP_ID)
  })

  test("customer leaves in grokMode → state and grok maps cleaned", async () => {
    await reachGrokMode()

    await customer.leaves()

    hasNoState(GROUP_ID)
    // grokGroupMap also cleaned (internal)
    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(false)
  })

  test("Grok leaves during grokMode → revert to teamQueue", async () => {
    await reachGrokMode()

    await grokAgent.leaves()

    stateIs(GROUP_ID, "teamQueue")
    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(false)
  })

  test("bot removed from group → state deleted", async () => {
    await reachTeamQueue("Hello")

    bot.onDeletedMemberUser({groupInfo: businessGroupInfo()} as any)

    hasNoState(GROUP_ID)
  })

  test("group deleted → state deleted", async () => {
    await reachGrokMode()

    bot.onGroupDeleted({groupInfo: businessGroupInfo()} as any)

    hasNoState(GROUP_ID)
    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(false)
  })

  test("customer leaves in welcome → state deleted", async () => {
    await customer.connects()

    await customer.leaves()

    hasNoState(GROUP_ID)
  })
})


// ─── 9. Error Handling ──────────────────────────────────────────

describe("Error Handling", () => {

  test("Grok invitation (apiAddMember) fails → error msg, stay in teamQueue", async () => {
    await reachTeamQueue("Hello")
    mainChat.apiAddMemberWillFail()
    mainChat.sent = []

    await customer.sends("/grok")

    customer.received(GROK_UNAVAILABLE)
    expect(grokApi.callCount()).toBe(0)
    stateIs(GROUP_ID, "teamQueue")
  })

  test("Grok join timeout → error msg, stay in teamQueue", async () => {
    vi.useFakeTimers()
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")
    mainChat.sent = []

    const sendPromise = customer.sends("/grok")
    // advanceTimersByTimeAsync flushes microtasks (so activateGrok reaches waitForGrokJoin)
    // then fires the 30s timeout
    await grokAgent.timesOut()
    await sendPromise

    customer.received(GROK_UNAVAILABLE)
    expect(grokApi.callCount()).toBe(0)
    stateIs(GROUP_ID, "teamQueue")
    vi.useRealTimers()
  })

  test("Grok API error during activation → remove Grok, error msg", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")
    grokApi.willFail()
    mainChat.sent = []

    const p = customer.sends("/grok")
    await grokAgent.joins()
    await p

    grokAgent.wasRemoved()
    customer.received(GROK_UNAVAILABLE)
    stateIs(GROUP_ID, "teamQueue")
  })

  test("Grok API error during conversation → remove Grok, revert to teamQueue", async () => {
    await reachGrokMode()
    grokApi.willFail()
    mainChat.sent = []

    await customer.sends("Another question")

    grokAgent.wasRemoved()
    customer.received(GROK_UNAVAILABLE)
    stateIs(GROUP_ID, "teamQueue")
  })

  test("after Grok API failure revert, /team still works", async () => {
    await reachGrokMode()
    grokApi.willFail()
    await customer.sends("Failing question")
    // Now back in teamQueue
    mainChat.setNextGroupMemberId(51)
    lastTeamMemberGId = 51
    mainChat.sent = []

    await customer.sends("/team")

    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)
    stateIs(GROUP_ID, "teamPending")
  })

  test("team member add fails from teamQueue → error, stay in teamQueue", async () => {
    await reachTeamQueue("Hello")
    mainChat.apiAddMemberWillFail()
    mainChat.sent = []

    await customer.sends("/team")

    customer.received(TEAM_ADD_ERROR)
    stateIs(GROUP_ID, "teamQueue")
  })

  test("team member add fails after Grok removal → revert to teamQueue", async () => {
    await reachGrokMode()
    mainChat.apiAddMemberWillFail()
    mainChat.sent = []

    await customer.sends("/team")

    grokAgent.wasRemoved()
    customer.received(TEAM_ADD_ERROR)
    // grokMode state is stale (Grok removed) → explicitly reverted to teamQueue
    stateIs(GROUP_ID, "teamQueue")
  })

  test("Grok failure then retry succeeds", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")

    // First attempt — API fails
    grokApi.willFail()
    const p1 = customer.sends("/grok")
    await grokAgent.joins()
    await p1
    stateIs(GROUP_ID, "teamQueue")

    // Second attempt — succeeds
    mainChat.setNextGroupMemberId(61)
    lastGrokMemberGId = 61
    grokApi.willRespond("Hello! How can I help?")
    const p2 = customer.sends("/grok")
    await grokAgent.joins()
    await p2

    customer.receivedFromGrok("Hello! How can I help?")
    stateIs(GROUP_ID, "grokMode")
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

    // While waiting, /team is processed concurrently
    mainChat.setNextGroupMemberId(70)
    lastTeamMemberGId = 70
    await customer.sends("/team")
    stateIs(GROUP_ID, "teamPending")

    // Grok join completes — but state changed
    await grokAgent.joins()
    await grokPromise

    // Bot detects state mismatch, removes Grok
    grokAgent.wasRemoved()
    expect(grokApi.callCount()).toBe(0)
    stateIs(GROUP_ID, "teamPending")
  })

  test("state change during Grok API call → abort", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")

    // Make grokApi.chat return a controllable promise
    let resolveGrokCall!: (v: string) => void
    grokApi.chat = async () => new Promise<string>(r => { resolveGrokCall = r })

    const grokPromise = customer.sends("/grok")
    await grokAgent.joins()
    // Flush microtasks so activateGrok proceeds past waitForGrokJoin into grokApi.chat
    await new Promise<void>(r => setTimeout(r, 0))
    // activateGrok now blocked on grokApi.chat

    // While API call is pending, /team changes state
    mainChat.setNextGroupMemberId(70)
    lastTeamMemberGId = 70
    await customer.sends("/team")
    stateIs(GROUP_ID, "teamPending")

    // API call completes — but state changed
    resolveGrokCall("Grok answer")
    await grokPromise

    grokAgent.wasRemoved()
    stateIs(GROUP_ID, "teamPending")
  })
})


// ─── 11. Weekend Hours ──────────────────────────────────────────

describe("Weekend Hours", () => {

  test("weekend: 48 hours in queue message", async () => {
    vi.mocked(isWeekend).mockReturnValue(true)

    await customer.connects()
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

  test("format: [displayName #groupId]\\ntext", async () => {
    await customer.connects()

    await customer.sends("My app crashes on startup")

    teamGroup.received("[Alice #100]\nMy app crashes on startup")
  })

  test("grokMode messages also forwarded to team", async () => {
    await reachGrokMode()
    mainChat.sent = []

    grokApi.willRespond("Try clearing app data")
    await customer.sends("App keeps crashing")

    teamGroup.received("[Alice #100]\nApp keeps crashing")
    customer.receivedFromGrok("Try clearing app data")
  })

  test("fallback displayName when empty → group-{id}", async () => {
    const emptyNameGroup = {...businessGroupInfo(101), groupProfile: {displayName: ""}}
    bot.onBusinessRequest({groupInfo: emptyNameGroup} as any)
    mainChat.sent = []

    // Send message in group 101 with empty display name
    const ci = customerChatItem("Hello", null)
    ci.chatInfo.groupInfo = emptyNameGroup
    ci.chatItem.chatDir.groupMember.memberId = emptyNameGroup.businessChat.customerId
    await bot.onNewChatItems({chatItems: [ci]} as any)

    teamGroup.received("[group-101 #101]\nHello")
  })
})


// ─── 13. Edge Cases ─────────────────────────────────────────────

describe("Edge Cases", () => {

  test("bot's own messages (groupSnd) → ignored", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [botOwnChatItem("queue reply")]} as any)

    expect(mainChat.sent.length).toBe(0)
    stateIs(GROUP_ID, "teamQueue")
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

    hasNoState(999)
  })

  test("message in business chat with no state → re-initialized to teamQueue", async () => {
    // Group 888 never had onBusinessRequest called (e.g., bot restarted)
    const ci = customerChatItem("Hello", null)
    ci.chatInfo.groupInfo = businessGroupInfo(888)
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [ci]} as any)

    // Re-initialized as teamQueue, message forwarded to team (no queue reply — already past welcome)
    stateIs(888, "teamQueue")
    teamGroup.received("[Alice #888]\nHello")
  })

  test("Grok's own messages in grokMode → ignored by bot", async () => {
    await reachGrokMode()
    mainChat.sent = []
    grokApi.reset()

    const ci = grokMemberChatItem(lastGrokMemberGId, "Grok's response text")
    await bot.onNewChatItems({chatItems: [ci]} as any)

    expect(grokApi.callCount()).toBe(0)
    expect(mainChat.sent.length).toBe(0)
  })

  test("bot passes full history to GrokApiClient (client truncates internally)", async () => {
    await reachGrokMode("Answer 0")

    // Build up 12 more exchanges → 26 history entries total
    for (let i = 1; i <= 12; i++) {
      grokApi.willRespond(`Answer ${i}`)
      await customer.sends(`Question ${i}`)
    }

    // 13th exchange — history passed to MockGrokApi has 26 entries
    // The real GrokApiClient.chat() does history.slice(-20) before calling the API
    grokApi.willRespond("Answer 13")
    await customer.sends("Question 13")

    const lastCall = grokApi.lastCall()
    expect(lastCall.history.length).toBe(26)
    expect(lastCall.message).toBe("Question 13")
  })

  test("unexpected Grok group invitation → ignored", async () => {
    await bot.onGrokGroupInvitation({
      groupInfo: {
        groupId: 999,
        membership: {memberId: "unknown-member"},
      },
    } as any)

    // No crash, no state change, no maps updated
    expect(grokChat.joined.length).toBe(0)
  })

  test("multiple concurrent conversations are independent", async () => {
    const GROUP_A = 100
    const GROUP_B = 300

    // Customer A connects
    bot.onBusinessRequest({groupInfo: businessGroupInfo(GROUP_A, "Alice")} as any)
    stateIs(GROUP_A, "welcome")

    // Customer B connects
    bot.onBusinessRequest({groupInfo: businessGroupInfo(GROUP_B, "Charlie")} as any)
    stateIs(GROUP_B, "welcome")

    // Customer A sends message → teamQueue
    const ciA = customerChatItem("Question A", null)
    ciA.chatInfo.groupInfo = businessGroupInfo(GROUP_A, "Alice")
    await bot.onNewChatItems({chatItems: [ciA]} as any)
    stateIs(GROUP_A, "teamQueue")

    // Customer B still in welcome
    stateIs(GROUP_B, "welcome")
  })

  test("Grok leaves during grokMode, customer retries → works", async () => {
    await reachGrokMode()

    await grokAgent.leaves()
    stateIs(GROUP_ID, "teamQueue")

    // Retry /grok
    mainChat.setNextGroupMemberId(62)
    lastGrokMemberGId = 62
    grokApi.willRespond("I'm back!")
    const p = customer.sends("/grok")
    await grokAgent.joins()
    await p

    customer.receivedFromGrok("I'm back!")
    stateIs(GROUP_ID, "grokMode")
  })

  test("/grok in welcome state → treated as regular text", async () => {
    await customer.connects()

    await customer.sends("/grok")

    // welcome state has no command handling — /grok is treated as text
    teamGroup.received("[Alice #100]\n/grok")
    customer.received(TEAM_QUEUE_24H)
    stateIs(GROUP_ID, "teamQueue")
  })

  test("/team in welcome state → treated as regular text", async () => {
    await customer.connects()

    await customer.sends("/team")

    // welcome state has no command handling — /team is treated as text
    teamGroup.received("[Alice #100]\n/team")
    customer.received(TEAM_QUEUE_24H)
    stateIs(GROUP_ID, "teamQueue")
  })

  test("non-text message in teamPending → ignored", async () => {
    await reachTeamPending()
    mainChat.sent = []

    await customer.sendsNonText()

    expect(mainChat.sent.length).toBe(0)
    stateIs(GROUP_ID, "teamPending")
  })

  test("non-text message in teamLocked → ignored", async () => {
    await reachTeamLocked()
    mainChat.sent = []

    await customer.sendsNonText()

    expect(mainChat.sent.length).toBe(0)
    stateIs(GROUP_ID, "teamLocked")
  })

  test("team member message in teamLocked → no state change", async () => {
    await reachTeamLocked()

    // onTeamMemberMessage checks state.type !== "teamPending" → returns
    await teamMember.sends("Just checking in")

    stateIs(GROUP_ID, "teamLocked")
  })

  test("unknown member message → silently ignored", async () => {
    await reachTeamQueue("Hello")
    mainChat.sent = []
    grokApi.reset()

    // A member who is neither customer, nor identified team member, nor Grok
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
    stateIs(GROUP_ID, "teamQueue")
  })

  test("Grok apiJoinGroup failure → maps not set", async () => {
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    await reachTeamQueue("Hello")

    // Make apiJoinGroup fail
    grokChat.apiJoinGroup = async () => { throw new Error("join failed") }

    grokApi.willRespond("answer")
    const p = customer.sends("/grok")

    // Trigger invitation — apiJoinGroup fails, resolver NOT called
    await new Promise<void>(r => setTimeout(r, 0))
    const memberId = `member-${lastGrokMemberGId}`
    await bot.onGrokGroupInvitation({
      groupInfo: {groupId: GROK_LOCAL, membership: {memberId}},
    } as any)

    // Maps should NOT be set (join failed)
    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(false)
    expect((bot as any).reverseGrokMap.has(GROK_LOCAL)).toBe(false)
  })

  test("replacement team member add fails → stays teamLocked", async () => {
    await reachTeamLocked()
    mainChat.apiAddMemberWillFail()

    await teamMember.leaves()

    // addReplacementTeamMember failed, but one-way gate holds
    stateIs(GROUP_ID, "teamLocked")
  })

  test("/grok with null grokContactId → unavailable message", async () => {
    const nullGrokConfig = {...config, grokContactId: null}
    const nullBot = new SupportBot(mainChat as any, grokChat as any, grokApi as any, nullGrokConfig as any)
    nullBot.onBusinessRequest({groupInfo: businessGroupInfo()} as any)
    const ci = customerChatItem("Hello", null)
    await nullBot.onNewChatItems({chatItems: [ci]} as any)
    mainChat.sent = []

    const grokCi = customerChatItem("/grok", "grok")
    await nullBot.onNewChatItems({chatItems: [grokCi]} as any)

    const msgs = mainChat.sentTo(GROUP_ID)
    expect(msgs).toContain("Grok is temporarily unavailable. Please try again or click /team for a team member.")
    const state = (nullBot as any).conversations.get(GROUP_ID)
    expect(state.type).toBe("teamQueue")
  })

  test("/team with empty teamMembers → unavailable message", async () => {
    const noTeamConfig = {...config, teamMembers: []}
    const noTeamBot = new SupportBot(mainChat as any, grokChat as any, grokApi as any, noTeamConfig as any)
    noTeamBot.onBusinessRequest({groupInfo: businessGroupInfo()} as any)
    const ci = customerChatItem("Hello", null)
    await noTeamBot.onNewChatItems({chatItems: [ci]} as any)
    mainChat.sent = []

    const teamCi = customerChatItem("/team", "team")
    await noTeamBot.onNewChatItems({chatItems: [teamCi]} as any)

    const msgs = mainChat.sentTo(GROUP_ID)
    expect(msgs).toContain("No team members are available yet. Please try again later or click /grok.")
    const state = (noTeamBot as any).conversations.get(GROUP_ID)
    expect(state.type).toBe("teamQueue")
  })
})


// ─── 14. Full End-to-End Flows ──────────────────────────────────

describe("End-to-End Flows", () => {

  test("full flow: welcome → grokMode → /team → teamLocked", async () => {
    // Step 1: connect
    await customer.connects()
    stateIs(GROUP_ID, "welcome")

    // Step 2: first message → teamQueue
    await customer.sends("How do I enable disappearing messages?")
    teamGroup.received("[Alice #100]\nHow do I enable disappearing messages?")
    customer.received(TEAM_QUEUE_24H)
    stateIs(GROUP_ID, "teamQueue")

    // Step 3: /grok → grokMode
    mainChat.setNextGroupMemberId(60)
    lastGrokMemberGId = 60
    grokApi.willRespond("Go to conversation settings and tap 'Disappearing messages'.")
    const p = customer.sends("/grok")
    await grokAgent.joins()
    await p
    customer.received(GROK_ACTIVATED)
    customer.receivedFromGrok("Go to conversation settings and tap 'Disappearing messages'.")
    stateIs(GROUP_ID, "grokMode")

    // Step 4: follow-up in grokMode
    grokApi.willRespond("Yes, you can set different timers per conversation.")
    await customer.sends("Can I set different timers?")
    teamGroup.received("[Alice #100]\nCan I set different timers?")
    customer.receivedFromGrok("Yes, you can set different timers per conversation.")
    stateIs(GROUP_ID, "grokMode")

    // Step 5: /team → teamPending (Grok removed)
    mainChat.setNextGroupMemberId(70)
    lastTeamMemberGId = 70
    await customer.sends("/team")
    grokAgent.wasRemoved()
    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)
    stateIs(GROUP_ID, "teamPending")

    // Step 6: /grok rejected
    await customer.sends("/grok")
    customer.received(TEAM_LOCKED_MSG)
    stateIs(GROUP_ID, "teamPending")

    // Step 7: team member responds → teamLocked
    await teamMember.sends("Hi! Let me help you.")
    stateIs(GROUP_ID, "teamLocked")

    // Step 8: /grok still rejected
    await customer.sends("/grok")
    customer.received(TEAM_LOCKED_MSG)
    stateIs(GROUP_ID, "teamLocked")

    // Step 9: customer continues — team sees directly, no forwarding
    mainChat.sent = []
    await customer.sends("Thanks for helping!")
    expect(mainChat.sent.length).toBe(0)
  })

  test("full flow: welcome → teamQueue → /team directly (skip Grok)", async () => {
    await customer.connects()

    await customer.sends("I have a billing question")
    customer.received(TEAM_QUEUE_24H)
    stateIs(GROUP_ID, "teamQueue")

    mainChat.setNextGroupMemberId(50)
    lastTeamMemberGId = 50
    await customer.sends("/team")
    teamMember.wasInvited()
    customer.received(TEAM_ADDED_24H)
    stateIs(GROUP_ID, "teamPending")

    await teamMember.sends("Hi, I can help with billing")
    stateIs(GROUP_ID, "teamLocked")
  })
})


// ─── 15. Restart Recovery ───────────────────────────────────────

describe("Restart Recovery", () => {

  test("after restart, customer message in unknown group → re-init to teamQueue, forward", async () => {
    // Simulate restart: no onBusinessRequest was called for group 777
    const ci = customerChatItem("I had a question earlier", null)
    ci.chatInfo.groupInfo = businessGroupInfo(777)
    mainChat.sent = []

    await bot.onNewChatItems({chatItems: [ci]} as any)

    // Re-initialized as teamQueue, message forwarded to team (no queue reply — already past welcome)
    stateIs(777, "teamQueue")
    teamGroup.received("[Alice #777]\nI had a question earlier")
  })

  test("after restart re-init, /grok works in re-initialized group", async () => {
    // Re-init group via first message
    const ci = customerChatItem("Hello", null)
    ci.chatInfo.groupInfo = businessGroupInfo(777)
    await bot.onNewChatItems({chatItems: [ci]} as any)
    stateIs(777, "teamQueue")

    // Now /grok
    mainChat.setNextGroupMemberId(80)
    lastGrokMemberGId = 80
    grokApi.willRespond("Grok answer")
    const grokCi = customerChatItem("/grok", "grok")
    grokCi.chatInfo.groupInfo = businessGroupInfo(777)
    const p = bot.onNewChatItems({chatItems: [grokCi]} as any)
    // Grok joins
    await new Promise<void>(r => setTimeout(r, 0))
    const memberId = `member-${lastGrokMemberGId}`
    await bot.onGrokGroupInvitation({
      groupInfo: {groupId: 201, membership: {memberId}},
    } as any)
    bot.onGrokMemberConnected({
      groupInfo: {groupId: 201},
      member: {memberProfile: {displayName: "Bot"}},
    } as any)
    await p

    stateIs(777, "grokMode")
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

    // Maps set but waiter not resolved — state still teamQueue
    expect((bot as any).grokGroupMap.has(GROUP_ID)).toBe(true)
    stateIs(GROUP_ID, "teamQueue")

    // Now fire connectedToGroupMember → waiter resolves
    bot.onGrokMemberConnected({
      groupInfo: {groupId: GROK_LOCAL},
      member: {memberProfile: {displayName: "Bot"}},
    } as any)
    await p

    stateIs(GROUP_ID, "grokMode")
  })

  test("onGrokMemberConnected for unknown group → ignored", () => {
    // Should not throw
    bot.onGrokMemberConnected({
      groupInfo: {groupId: 9999},
      member: {memberProfile: {displayName: "Someone"}},
    } as any)
  })
})


// ─── 17. groupDuplicateMember Handling ─────────────────────────

describe("groupDuplicateMember Handling", () => {

  test("/team with duplicate member → finds existing, transitions to teamPending", async () => {
    await reachTeamQueue("Hello")
    mainChat.apiAddMemberWillDuplicate()
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 42, memberContactId: 2, memberStatus: "memConnected"},
    ])
    mainChat.sent = []

    await customer.sends("/team")

    customer.received(TEAM_ADDED_24H)
    const state = (bot as any).conversations.get(GROUP_ID)
    expect(state.type).toBe("teamPending")
    expect(state.teamMemberGId).toBe(42)
  })

  test("/team with duplicate but member not found in list → error message", async () => {
    await reachTeamQueue("Hello")
    mainChat.apiAddMemberWillDuplicate()
    mainChat.setGroupMembers(GROUP_ID, [])  // empty — member not found
    mainChat.sent = []

    await customer.sends("/team")

    customer.received(TEAM_ADD_ERROR)
    stateIs(GROUP_ID, "teamQueue")
  })

  test("replacement team member with duplicate → finds existing, stays locked", async () => {
    await reachTeamLocked()
    mainChat.apiAddMemberWillDuplicate()
    mainChat.setGroupMembers(GROUP_ID, [
      {groupMemberId: 99, memberContactId: 2, memberStatus: "memConnected"},
    ])

    await teamMember.leaves()

    const state = (bot as any).conversations.get(GROUP_ID)
    expect(state.type).toBe("teamLocked")
    expect(state.teamMemberGId).toBe(99)
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
    // No error, logged acceptance
  })

  test("onMemberContactReceivedInv from non-team group → no crash", () => {
    bot.onMemberContactReceivedInv({
      contact: {contactId: 11},
      groupInfo: {groupId: 999},
      member: {memberProfile: {displayName: "Stranger"}},
    } as any)
    // No error
  })
})


// ═══════════════════════════════════════════════════════════════
//  Coverage Matrix
// ═══════════════════════════════════════════════════════════════
//
//  State / Input      | Text msg  | /grok   | /team   | Non-text | Team msg | Leave    | Unknown member
//  -------------------|-----------|---------|---------|----------|----------|----------|---------------
//  welcome            | 1.2       | 13.9    | 13.10   | 1.3      | —        | 8.6      | —
//  teamQueue          | 2.1, 2.2  | 3.1,3.2 | 5.1    | 2.3      | —        | 8.1      | 13.14
//  grokMode           | 4.1, 4.2  | 4.3     | 5.2    | 4.4      | —        | 8.3 grok | —
//  teamPending        | 6.6       | 6.1     | 6.4    | 13.11    | 6.2      | 7.1      | —
//  teamLocked         | 6.7       | 6.3     | 6.5    | 13.12    | 13.13    | 7.3      | —
//
//  Error scenario                          | Test
//  ----------------------------------------|-------
//  Grok invitation fails                   | 9.1
//  Grok join timeout                       | 9.2
//  Grok API error (activation)             | 9.3
//  Grok API error (conversation)           | 9.4
//  Grok API failure then retry             | 9.8
//  Team add fails (teamQueue)              | 9.6
//  Team add fails (after Grok removal)     | 9.7
//  Grok apiJoinGroup failure               | 13.15
//  Replacement team add fails              | 13.16
//  Race: /team during Grok join            | 10.1
//  Race: state change during API call      | 10.2
//  Bot removed / group deleted             | 8.4, 8.5
//  Weekend hours                           | 11.1, 11.2
//  Forwarding format                       | 12.1, 12.2, 12.3
//  Concurrent conversations                | 13.7
//  History passed to GrokApiClient         | 13.5
//  Full E2E flows                          | 14.1, 14.2
//  Restart recovery (re-init teamQueue)    | 15.1, 15.2
//  Grok connectedToGroupMember waiter      | 16.1, 16.2
//  groupDuplicateMember handling           | 17.1, 17.2, 17.3
//  DM contact received                     | 18.1, 18.2
