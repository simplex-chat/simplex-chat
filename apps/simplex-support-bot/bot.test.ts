import {describe, test, expect, beforeEach, vi} from "vitest"
import {SupportBot} from "./src/bot.js"
import {CardManager} from "./src/cards.js"
import {welcomeMessage, queueMessage, grokActivatedMessage, teamLockedMessage} from "./src/messages.js"

// Silence console output during tests
vi.spyOn(console, "log").mockImplementation(() => {})
vi.spyOn(console, "error").mockImplementation(() => {})

// ─── Type stubs ───

const ChatType = {Direct: "direct" as const, Group: "group" as const, Local: "local" as const}
const GroupMemberRole = {Member: "member" as const, Owner: "owner" as const, Admin: "admin" as const}
const GroupMemberStatus = {Connected: "connected" as const, Complete: "complete" as const, Announced: "announced" as const, Left: "left" as const}
const GroupFeatureEnabled = {On: "on" as const, Off: "off" as const}
const CIDeleteMode = {Broadcast: "broadcast" as const}

// ─── Mock infrastructure ───

let nextItemId = 1000

class MockChatApi {
  sent: {chat: [string, number]; text: string}[] = []
  added: {groupId: number; contactId: number; role: string}[] = []
  removed: {groupId: number; memberIds: number[]}[] = []
  joined: number[] = []
  deleted: {chatType: string; chatId: number; itemIds: number[]; mode: string}[] = []
  customData = new Map<number, any>()
  roleChanges: {groupId: number; memberIds: number[]; role: string}[] = []
  profileUpdates: {groupId: number; profile: any}[] = []

  members = new Map<number, any[]>()
  chatItems = new Map<number, any[]>()
  groups = new Map<number, any>()
  activeUserId = 1

  private _addMemberFails = false
  private _addMemberError: any = null
  private _deleteChatItemsFails = false

  apiAddMemberWillFail(err?: any) { this._addMemberFails = true; this._addMemberError = err }
  apiDeleteChatItemsWillFail() { this._deleteChatItemsFails = true }

  async apiSetActiveUser(userId: number) { this.activeUserId = userId; return {userId, profile: {displayName: "test"}} }
  async apiSendMessages(chatRef: any, messages: any[]) {
    // Normalize chat ref: accept both [type, id] tuples and {chatType, chatId} objects
    const chat: [string, number] = Array.isArray(chatRef)
      ? chatRef
      : [chatRef.chatType, chatRef.chatId]
    return messages.map(msg => {
      const text = msg.msgContent?.text || ""
      this.sent.push({chat, text})
      const itemId = nextItemId++
      return {chatItem: {meta: {itemId}, chatDir: {type: "groupSnd"}, content: {type: "sndMsgContent", msgContent: {type: "text", text}}}}
    })
  }
  async apiSendTextMessage(chat: [string, number], text: string) {
    return this.apiSendMessages(chat, [{msgContent: {type: "text", text}, mentions: {}}])
  }
  async apiAddMember(groupId: number, contactId: number, role: string) {
    if (this._addMemberFails) {
      this._addMemberFails = false
      throw this._addMemberError || new Error("apiAddMember failed")
    }
    this.added.push({groupId, contactId, role})
    const memberId = `member-${contactId}`
    const groupMemberId = 5000 + contactId
    return {memberId, groupMemberId, memberContactId: contactId, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: `Contact${contactId}`}}
  }
  async apiRemoveMembers(groupId: number, memberIds: number[]) {
    this.removed.push({groupId, memberIds})
    return memberIds.map(id => ({groupMemberId: id}))
  }
  async apiJoinGroup(groupId: number) {
    this.joined.push(groupId)
    return {groupId}
  }
  async apiSetMembersRole(groupId: number, memberIds: number[], role: string) {
    this.roleChanges.push({groupId, memberIds, role})
  }
  async apiListMembers(groupId: number) {
    return this.members.get(groupId) || []
  }
  async apiGetChat(_chatType: string, chatId: number, _count: number) {
    const items = this.chatItems.get(chatId) || []
    const groupInfo = this.groups.get(chatId)
    return {
      chatInfo: {type: "group", groupInfo: groupInfo || makeGroupInfo(chatId)},
      chatItems: items,
      chatStats: {unreadCount: 0, unreadMentions: 0, reportsCount: 0, minUnreadItemId: 0, unreadChat: false},
    }
  }
  async apiListGroups(_userId: number) {
    return [...this.groups.values()].map(g => ({...g, customData: this.customData.get(g.groupId)}))
  }
  async apiSetGroupCustomData(groupId: number, data?: any) {
    if (data === undefined) this.customData.delete(groupId)
    else this.customData.set(groupId, data)
  }
  async apiDeleteChatItems(chatType: string, chatId: number, itemIds: number[], mode: string) {
    if (this._deleteChatItemsFails) {
      this._deleteChatItemsFails = false
      throw new Error("apiDeleteChatItems failed")
    }
    this.deleted.push({chatType, chatId, itemIds, mode})
    return []
  }
  async apiUpdateGroupProfile(groupId: number, profile: any) {
    this.profileUpdates.push({groupId, profile})
    return this.groups.get(groupId) || makeGroupInfo(groupId)
  }

  memberContacts: {groupId: number; groupMemberId: number; contactId: number}[] = []
  memberContactInvitations: {contactId: number; text: string}[] = []

  async apiCreateMemberContact(groupId: number, groupMemberId: number): Promise<any> {
    const contactId = nextItemId++
    this.memberContacts.push({groupId, groupMemberId, contactId})
    return {contactId, profile: {displayName: "member"}}
  }
  async apiSendMemberContactInvitation(contactId: number, message?: any): Promise<any> {
    const text = typeof message === "string" ? message : (message?.text ?? "")
    this.memberContactInvitations.push({contactId, text})
    this.sent.push({chat: [ChatType.Direct, contactId], text})
    return {contactId, profile: {displayName: "member"}}
  }

  rawCmds: string[] = []
  async sendChatCmd(cmd: string) {
    this.rawCmds.push(cmd)
    return {type: "cmdOk"}
  }

  sentTo(groupId: number): string[] {
    return this.sent.filter(s => s.chat[0] === ChatType.Group && s.chat[1] === groupId).map(s => s.text)
  }
  lastSentTo(groupId: number): string | undefined {
    const msgs = this.sentTo(groupId)
    return msgs[msgs.length - 1]
  }
  sentDirect(contactId: number): string[] {
    return this.sent.filter(s => s.chat[0] === ChatType.Direct && s.chat[1] === contactId).map(s => s.text)
  }
}

class MockGrokApi {
  calls: {history: any[]; message: string}[] = []
  private _response = "Grok answer"
  private _willFail = false

  willRespond(text: string) { this._response = text; this._willFail = false }
  willFail() { this._willFail = true }

  async chat(history: any[], userMessage: string): Promise<string> {
    this.calls.push({history, message: userMessage})
    if (this._willFail) { this._willFail = false; throw new Error("Grok API error") }
    return this._response
  }
}

// ─── Factory helpers ───

const MAIN_USER_ID = 1
const GROK_USER_ID = 2
const TEAM_GROUP_ID = 50
const CUSTOMER_GROUP_ID = 100
const GROK_CONTACT_ID = 10
const TEAM_MEMBER_1_ID = 20
const TEAM_MEMBER_2_ID = 21
const GROK_LOCAL_GROUP_ID = 200
const CUSTOMER_ID = "customer-1"

// ─── Member factories ───

function makeTeamMember(contactId: number, name = `Contact${contactId}`, groupMemberId?: number) {
  return {
    memberId: `team-${contactId}`,
    groupMemberId: groupMemberId ?? 5000 + contactId,
    memberContactId: contactId,
    memberStatus: GroupMemberStatus.Connected,
    memberProfile: {displayName: name},
  }
}

function makeGrokMember(groupMemberId = 7777) {
  return {
    memberId: "grok-member",
    groupMemberId,
    memberContactId: GROK_CONTACT_ID,
    memberStatus: GroupMemberStatus.Connected,
    memberProfile: {displayName: "Grok"},
  }
}

function makeCustomerMember(status = GroupMemberStatus.Connected) {
  return {
    memberId: CUSTOMER_ID,
    groupMemberId: 3000,
    memberStatus: status,
    memberProfile: {displayName: "Customer"},
  }
}

function makeConfig(overrides: Partial<any> = {}) {
  return {
    dbPrefix: "./test-data/simplex",
    teamGroup: {id: TEAM_GROUP_ID, name: "SupportTeam"},
    teamMembers: [
      {id: TEAM_MEMBER_1_ID, name: "Alice"},
      {id: TEAM_MEMBER_2_ID, name: "Bob"},
    ],
    groupLinks: "",
    timezone: "UTC",
    completeHours: 3,
    cardFlushMinutes: 15,
    grokApiKey: "test-key",
    grokContactId: GROK_CONTACT_ID as number | null,
    ...overrides,
  }
}

function makeGroupInfo(groupId: number, opts: Partial<any> = {}): any {
  return {
    groupId,
    groupProfile: {displayName: opts.displayName || `Group${groupId}`, fullName: ""},
    businessChat: opts.businessChat !== undefined ? opts.businessChat : {
      chatType: "business",
      businessId: "bot-1",
      customerId: opts.customerId || CUSTOMER_ID,
    },
    membership: {memberId: "bot-member"},
    customData: opts.customData,
    chatSettings: {enableNtfs: "all", favorite: false},
    fullGroupPreferences: {},
    localDisplayName: `group-${groupId}`,
    localAlias: "",
    useRelays: false,
    createdAt: new Date().toISOString(),
    updatedAt: new Date().toISOString(),
    chatTags: [],
    groupSummary: {},
    membersRequireAttention: 0,
  }
}

function makeUser(userId: number) {
  return {userId, profile: {displayName: userId === MAIN_USER_ID ? "Ask SimpleX Team" : "Grok"}}
}

function makeChatItem(opts: {
  dir: "groupSnd" | "groupRcv" | "directRcv"
  text?: string
  memberId?: string
  memberContactId?: number
  memberDisplayName?: string
  msgType?: string
  groupId?: number
}): any {
  const itemId = nextItemId++
  const now = new Date().toISOString()
  const msgContent = opts.msgType
    ? {type: opts.msgType, text: opts.text || ""}
    : {type: "text", text: opts.text || ""}

  let chatDir: any
  if (opts.dir === "groupSnd") {
    chatDir = {type: "groupSnd"}
  } else if (opts.dir === "groupRcv") {
    chatDir = {
      type: "groupRcv",
      groupMember: {
        memberId: opts.memberId || CUSTOMER_ID,
        groupMemberId: 3000,
        memberContactId: opts.memberContactId,
        memberStatus: GroupMemberStatus.Connected,
        memberProfile: {displayName: opts.memberDisplayName || "Customer"},
      },
    }
  } else {
    chatDir = {type: "directRcv"}
  }

  return {
    chatDir,
    meta: {itemId, itemTs: now, createdAt: now, itemText: opts.text || "", itemStatus: {type: "sndSent"}, itemEdited: false},
    content: {type: opts.dir === "groupSnd" ? "sndMsgContent" : "rcvMsgContent", msgContent},
    mentions: {},
    reactions: [],
  }
}

function makeAChatItem(chatItem: any, groupId = CUSTOMER_GROUP_ID): any {
  return {
    chatInfo: {type: "group", groupInfo: makeGroupInfo(groupId)},
    chatItem,
  }
}

function makeDirectAChatItem(chatItem: any, contactId: number): any {
  return {
    chatInfo: {type: "direct", contact: {contactId, profile: {displayName: "Someone"}}},
    chatItem,
  }
}

// ─── Shared test state ───

let chat: MockChatApi
let grokApi: MockGrokApi
let config: ReturnType<typeof makeConfig>
let bot: InstanceType<typeof SupportBot>
let cards: InstanceType<typeof CardManager>

// ─── Setup and helpers ───

function setup(configOverrides: Partial<any> = {}) {
  nextItemId = 1000
  chat = new MockChatApi()
  grokApi = new MockGrokApi()
  config = makeConfig(configOverrides)

  // Register team group and customer group in mock
  const teamGroupInfo = makeGroupInfo(TEAM_GROUP_ID, {businessChat: null, displayName: "SupportTeam"})
  chat.groups.set(TEAM_GROUP_ID, teamGroupInfo)
  chat.groups.set(CUSTOMER_GROUP_ID, makeGroupInfo(CUSTOMER_GROUP_ID))

  cards = new CardManager(chat as any, config as any, MAIN_USER_ID, 999999999)
  bot = new SupportBot(chat as any, grokApi as any, config as any, MAIN_USER_ID, GROK_USER_ID)
  // Replace cards with our constructed one that has a long flush interval
  bot.cards = cards
}

function customerMessage(text: string, groupId = CUSTOMER_GROUP_ID): any {
  const ci = makeChatItem({dir: "groupRcv", text, memberId: CUSTOMER_ID})
  return {
    type: "newChatItems" as const,
    user: makeUser(MAIN_USER_ID),
    chatItems: [makeAChatItem(ci, groupId)],
  }
}

function customerNonTextMessage(groupId = CUSTOMER_GROUP_ID): any {
  const ci = makeChatItem({dir: "groupRcv", text: "", memberId: CUSTOMER_ID, msgType: "image"})
  return {
    type: "newChatItems" as const,
    user: makeUser(MAIN_USER_ID),
    chatItems: [makeAChatItem(ci, groupId)],
  }
}

function teamMemberMessage(text: string, contactId = TEAM_MEMBER_1_ID, groupId = CUSTOMER_GROUP_ID): any {
  const ci = makeChatItem({dir: "groupRcv", text, memberId: `team-${contactId}`, memberContactId: contactId, memberDisplayName: "Alice"})
  return {
    type: "newChatItems" as const,
    user: makeUser(MAIN_USER_ID),
    chatItems: [makeAChatItem(ci, groupId)],
  }
}

function grokResponseMessage(text: string, groupId = CUSTOMER_GROUP_ID): any {
  const ci = makeChatItem({dir: "groupRcv", text, memberId: "grok-member", memberContactId: GROK_CONTACT_ID, memberDisplayName: "Grok"})
  return {
    type: "newChatItems" as const,
    user: makeUser(MAIN_USER_ID),
    chatItems: [makeAChatItem(ci, groupId)],
  }
}

function directMessage(text: string, contactId: number): any {
  const ci = makeChatItem({dir: "directRcv", text})
  return {
    type: "newChatItems" as const,
    user: makeUser(MAIN_USER_ID),
    chatItems: [makeDirectAChatItem(ci, contactId)],
  }
}

function teamGroupMessage(text: string, senderContactId = TEAM_MEMBER_1_ID): any {
  const ci = makeChatItem({dir: "groupRcv", text, memberId: `team-${senderContactId}`, memberContactId: senderContactId, memberDisplayName: "Alice"})
  return {
    type: "newChatItems" as const,
    user: makeUser(MAIN_USER_ID),
    chatItems: [{chatInfo: {type: "group", groupInfo: makeGroupInfo(TEAM_GROUP_ID, {businessChat: null})}, chatItem: ci}],
  }
}

// Simulate bot sending a message to the customer group (adds it to chatItems history)
function addBotMessage(text: string, groupId = CUSTOMER_GROUP_ID) {
  const ci = makeChatItem({dir: "groupSnd", text})
  const items = chat.chatItems.get(groupId) || []
  items.push(ci)
  chat.chatItems.set(groupId, items)
}

function addCustomerMessageToHistory(text: string, groupId = CUSTOMER_GROUP_ID) {
  const ci = makeChatItem({dir: "groupRcv", text, memberId: CUSTOMER_ID})
  const items = chat.chatItems.get(groupId) || []
  items.push(ci)
  chat.chatItems.set(groupId, items)
}

function addTeamMemberMessageToHistory(text: string, contactId = TEAM_MEMBER_1_ID, groupId = CUSTOMER_GROUP_ID) {
  const ci = makeChatItem({dir: "groupRcv", text, memberId: `team-${contactId}`, memberContactId: contactId})
  const items = chat.chatItems.get(groupId) || []
  items.push(ci)
  chat.chatItems.set(groupId, items)
}

function addGrokMessageToHistory(text: string, groupId = CUSTOMER_GROUP_ID) {
  const ci = makeChatItem({dir: "groupRcv", text, memberId: "grok-member", memberContactId: GROK_CONTACT_ID})
  const items = chat.chatItems.get(groupId) || []
  items.push(ci)
  chat.chatItems.set(groupId, items)
}

// State helpers — reach specific states
async function reachQueue(groupId = CUSTOMER_GROUP_ID) {
  await bot.onNewChatItems(customerMessage("Hello, I need help", groupId))
  // This should have sent queue message + created card
}

async function reachGrok(groupId = CUSTOMER_GROUP_ID) {
  await reachQueue(groupId)
  // Add the queue message to history so state derivation sees it
  addBotMessage("The team can see your message", groupId)

  // Send /grok command. This triggers activateGrok which needs the join flow.
  // We need to simulate Grok join success.
  const grokJoinPromise = simulateGrokJoinSuccess(groupId)
  await bot.onNewChatItems(customerMessage("/grok", groupId))
  await grokJoinPromise
}

async function simulateGrokJoinSuccess(mainGroupId = CUSTOMER_GROUP_ID) {
  // Wait for apiAddMember to be called, then simulate Grok invitation + join
  await new Promise(r => setTimeout(r, 10))
  // Find the pending grok join via the added members
  const addedGrok = chat.added.find(a => a.contactId === GROK_CONTACT_ID && a.groupId === mainGroupId)
  if (!addedGrok) return

  // Simulate Grok receivedGroupInvitation
  const memberId = `member-${GROK_CONTACT_ID}`
  await bot.onGrokGroupInvitation({
    type: "receivedGroupInvitation",
    user: makeUser(GROK_USER_ID),
    groupInfo: {...makeGroupInfo(GROK_LOCAL_GROUP_ID), membership: {memberId}},
    contact: {contactId: 99},
    fromMemberRole: GroupMemberRole.Admin,
    memberRole: GroupMemberRole.Member,
  })

  // Simulate Grok connectedToGroupMember
  await bot.onGrokMemberConnected({
    type: "connectedToGroupMember",
    user: makeUser(GROK_USER_ID),
    groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID),
    member: {memberId: "bot-in-grok-view", groupMemberId: 9999, memberContactId: undefined},
  })
}

async function reachTeamPending(groupId = CUSTOMER_GROUP_ID) {
  await reachQueue(groupId)
  addBotMessage("The team can see your message", groupId)
  await bot.onNewChatItems(customerMessage("/team", groupId))
}

async function reachTeam(groupId = CUSTOMER_GROUP_ID) {
  await reachTeamPending(groupId)
  addBotMessage("A team member has been added", groupId)
  chat.members.set(groupId, [makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
  // Team member sends a text message (triggers one-way gate)
  addTeamMemberMessageToHistory("Hi, how can I help?", TEAM_MEMBER_1_ID, groupId)
  await bot.onNewChatItems(teamMemberMessage("Hi, how can I help?", TEAM_MEMBER_1_ID, groupId))
}

// ─── Assertion helpers ───

function expectSentToGroup(groupId: number, substring: string) {
  const msgs = chat.sentTo(groupId)
  expect(msgs.some(m => m.includes(substring)),
    `Expected message containing "${substring}" sent to group ${groupId}, got:\n${msgs.join("\n")}`
  ).toBe(true)
}

function expectNotSentToGroup(groupId: number, substring: string) {
  expect(chat.sentTo(groupId).every(m => !m.includes(substring))).toBe(true)
}

function expectDmSent(contactId: number, substring: string) {
  expect(chat.sentDirect(contactId).some(m => m.includes(substring))).toBe(true)
}

function expectAnySent(substring: string) {
  expect(chat.sent.some(s => s.text.includes(substring))).toBe(true)
}

function expectMemberAdded(groupId: number, contactId: number) {
  expect(chat.added.some(a => a.groupId === groupId && a.contactId === contactId)).toBe(true)
}

function expectCardDeleted(cardItemId: number) {
  expect(chat.deleted.some(d => d.itemIds.includes(cardItemId))).toBe(true)
}

// ─── Event factories ───

function connectedEvent(groupId: number, member: any, memberContact?: any) {
  return {
    type: "connectedToGroupMember" as const,
    user: makeUser(MAIN_USER_ID),
    groupInfo: makeGroupInfo(groupId, groupId === TEAM_GROUP_ID ? {businessChat: null} : {}),
    member,
    ...(memberContact !== undefined ? {memberContact} : {}),
  }
}

function leftEvent(groupId: number, member: any) {
  return {
    type: "leftMember" as const,
    user: makeUser(MAIN_USER_ID),
    groupInfo: makeGroupInfo(groupId, groupId === TEAM_GROUP_ID ? {businessChat: null} : {}),
    member: {...member, memberStatus: GroupMemberStatus.Left},
  }
}

function updatedEvent(groupId: number, chatItem: any, userId = MAIN_USER_ID) {
  return {
    type: "chatItemUpdated" as const,
    user: makeUser(userId),
    chatItem: {
      chatInfo: {type: "group", groupInfo: makeGroupInfo(groupId, groupId === TEAM_GROUP_ID ? {businessChat: null} : {})},
      chatItem,
    },
  }
}

function reactionEvent(groupId: number, added: boolean) {
  return {
    type: "chatItemReaction" as const,
    user: makeUser(MAIN_USER_ID),
    added,
    reaction: {
      chatInfo: {type: "group", groupInfo: makeGroupInfo(groupId)},
      chatReaction: {reaction: {type: "emoji", emoji: "👍"}},
    },
  }
}

function joinedEvent(groupId: number, member: any, userId = MAIN_USER_ID) {
  return {
    type: "joinedGroupMember" as const,
    user: makeUser(userId),
    groupInfo: makeGroupInfo(groupId, groupId === TEAM_GROUP_ID ? {businessChat: null} : {}),
    member,
  }
}

function grokViewCustomerMessage(text: string, msgType?: string) {
  chat.groups.set(GROK_LOCAL_GROUP_ID, makeGroupInfo(GROK_LOCAL_GROUP_ID))
  const ci = makeChatItem({dir: "groupRcv", text, memberId: CUSTOMER_ID, ...(msgType ? {msgType} : {})})
  return {
    type: "newChatItems" as const,
    user: makeUser(GROK_USER_ID),
    chatItems: [{chatInfo: {type: "group", groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID)}, chatItem: ci}],
  }
}

// ═══════════════════════════════════════════════════════════
// Tests
// ═══════════════════════════════════════════════════════════

describe("Welcome & First Message", () => {
  beforeEach(() => setup())

  test("first message → queue reply sent, card created in team group", async () => {
    await bot.onNewChatItems(customerMessage("Hello"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "The team can see your message")
    const teamMsgs = chat.sentTo(TEAM_GROUP_ID)
    expect(teamMsgs.length).toBeGreaterThan(0)
    expect(teamMsgs[teamMsgs.length - 1]).toContain("/join")
  })

  test("non-text first message → no queue reply, no card", async () => {
    await bot.onNewChatItems(customerNonTextMessage())
    expectNotSentToGroup(CUSTOMER_GROUP_ID, "The team can see your message")
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBe(0)
  })

  test("second message → no duplicate queue reply", async () => {
    await bot.onNewChatItems(customerMessage("Hello"))
    addBotMessage("The team can see your message")
    const countBefore = chat.sentTo(CUSTOMER_GROUP_ID).filter(m => m.includes("The team can see your message")).length
    await bot.onNewChatItems(customerMessage("Second message"))
    const countAfter = chat.sentTo(CUSTOMER_GROUP_ID).filter(m => m.includes("The team can see your message")).length
    expect(countAfter).toBe(countBefore)
  })

  test("unrecognized /command → treated as normal message", async () => {
    await bot.onNewChatItems(customerMessage("/unknown"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "The team can see your message")
  })
})

describe("/grok Activation", () => {
  beforeEach(() => setup())

  test("/grok from QUEUE → Grok invited, grokActivatedMessage sent", async () => {
    await reachQueue()
    addBotMessage("The team can see your message")
    const joinPromise = simulateGrokJoinSuccess()
    await bot.onNewChatItems(customerMessage("/grok"))
    await joinPromise
    await bot.flush()
    expectMemberAdded(CUSTOMER_GROUP_ID, GROK_CONTACT_ID)
    expectSentToGroup(CUSTOMER_GROUP_ID, "now chatting with Grok")
  })

  test("/grok as first message → WELCOME→GROK directly, no queue message", async () => {
    const joinPromise = simulateGrokJoinSuccess()
    await bot.onNewChatItems(customerMessage("/grok"))
    await joinPromise
    await bot.flush()
    expectSentToGroup(CUSTOMER_GROUP_ID, "now chatting with Grok")
    expectNotSentToGroup(CUSTOMER_GROUP_ID, "The team can see your message")
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBeGreaterThan(0)
  })

  test("/grok in TEAM → rejected with teamLockedMessage", async () => {
    await reachTeam()
    await bot.onNewChatItems(customerMessage("/grok"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "team mode")
  })

  test("/grok when grokContactId is null → grokUnavailableMessage", async () => {
    setup({grokContactId: null})
    await reachQueue()
    addBotMessage("The team can see your message")
    await bot.onNewChatItems(customerMessage("/grok"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "temporarily unavailable")
  })

  test("/grok as first message + Grok join fails → queue message sent as fallback", async () => {
    chat.apiAddMemberWillFail()
    await bot.onNewChatItems(customerMessage("/grok"))
    await bot.flush()
    expectSentToGroup(CUSTOMER_GROUP_ID, "temporarily unavailable")
    expectSentToGroup(CUSTOMER_GROUP_ID, "The team can see your message")
  })
})

describe("Grok Conversation", () => {
  beforeEach(() => setup())

  test("Grok per-message: reads history, calls API, sends response", async () => {
    addCustomerMessageToHistory("How do I create a group?", GROK_LOCAL_GROUP_ID)
    grokApi.willRespond("To create a group, tap +, then New Group.")
    await bot.onGrokNewChatItems(grokViewCustomerMessage("How do I create a group?"))

    expect(grokApi.calls.length).toBe(1)
    expect(grokApi.calls[0].message).toBe("How do I create a group?")
    expectAnySent("To create a group, tap +, then New Group.")
  })

  test("customer non-text in GROK → no Grok API call", async () => {
    await bot.onGrokNewChatItems(grokViewCustomerMessage("", "image"))
    expect(grokApi.calls.length).toBe(0)
  })

  test("Grok API error → error message in group, stays GROK", async () => {
    grokApi.willFail()
    await bot.onGrokNewChatItems(grokViewCustomerMessage("A question"))
    expectAnySent("couldn't process that")
  })

  test("Grok ignores bot commands from customer", async () => {
    await bot.onGrokNewChatItems(grokViewCustomerMessage("/team"))
    expect(grokApi.calls.length).toBe(0)
  })

  test("Grok per-message: history includes prior Grok sent response as assistant", async () => {
    addCustomerMessageToHistory("How do I create a group?", GROK_LOCAL_GROUP_ID)
    addBotMessage("To create a group, tap + then New Group.", GROK_LOCAL_GROUP_ID)
    addCustomerMessageToHistory("How do I invite members?", GROK_LOCAL_GROUP_ID)
    grokApi.willRespond("Open the group and tap Invite.")
    await bot.onGrokNewChatItems(grokViewCustomerMessage("How do I invite members?"))

    expect(grokApi.calls.length).toBe(1)
    expect(grokApi.calls[0].message).toBe("How do I invite members?")
    expect(grokApi.calls[0].history).toEqual([
      {role: "user", content: "How do I create a group?"},
      {role: "assistant", content: "To create a group, tap + then New Group."},
    ])
  })

  test("Grok ignores non-customer messages", async () => {
    chat.groups.set(GROK_LOCAL_GROUP_ID, makeGroupInfo(GROK_LOCAL_GROUP_ID))
    const ci = makeChatItem({dir: "groupRcv", text: "Team message", memberId: "not-customer", memberContactId: TEAM_MEMBER_1_ID})
    const grokEvt = {
      type: "newChatItems" as const,
      user: makeUser(GROK_USER_ID),
      chatItems: [{chatInfo: {type: "group", groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID)}, chatItem: ci}],
    }
    await bot.onGrokNewChatItems(grokEvt)
    expect(grokApi.calls.length).toBe(0)
  })

  test("Grok ignores own messages (groupSnd)", async () => {
    chat.groups.set(GROK_LOCAL_GROUP_ID, makeGroupInfo(GROK_LOCAL_GROUP_ID))
    const ci = makeChatItem({dir: "groupSnd", text: "My own response"})
    const grokEvt = {
      type: "newChatItems" as const,
      user: makeUser(GROK_USER_ID),
      chatItems: [{chatInfo: {type: "group", groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID)}, chatItem: ci}],
    }
    await bot.onGrokNewChatItems(grokEvt)
    expect(grokApi.calls.length).toBe(0)
  })

  test("batch: multiple customer messages in one event → only last triggers Grok API call", async () => {
    chat.groups.set(GROK_LOCAL_GROUP_ID, makeGroupInfo(GROK_LOCAL_GROUP_ID))
    addCustomerMessageToHistory("First question", GROK_LOCAL_GROUP_ID)
    addCustomerMessageToHistory("Second question", GROK_LOCAL_GROUP_ID)

    const ci1 = makeChatItem({dir: "groupRcv", text: "First question", memberId: CUSTOMER_ID})
    const ci2 = makeChatItem({dir: "groupRcv", text: "Second question", memberId: CUSTOMER_ID})
    const evt = {
      type: "newChatItems" as const,
      user: makeUser(GROK_USER_ID),
      chatItems: [
        {chatInfo: {type: "group", groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID)}, chatItem: ci1},
        {chatInfo: {type: "group", groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID)}, chatItem: ci2},
      ],
    }

    await bot.onGrokNewChatItems(evt)

    expect(grokApi.calls.length).toBe(1)
    expect(grokApi.calls[0].message).toBe("Second question")
  })

  test("batch: messages from different groups → each group gets one response", async () => {
    const GROK_GROUP_A = 201
    const GROK_GROUP_B = 202
    chat.groups.set(GROK_GROUP_A, makeGroupInfo(GROK_GROUP_A))
    chat.groups.set(GROK_GROUP_B, makeGroupInfo(GROK_GROUP_B))
    addCustomerMessageToHistory("Question A", GROK_GROUP_A)
    addCustomerMessageToHistory("Question B", GROK_GROUP_B)

    const ciA = makeChatItem({dir: "groupRcv", text: "Question A", memberId: CUSTOMER_ID})
    const ciB = makeChatItem({dir: "groupRcv", text: "Question B", memberId: CUSTOMER_ID})
    const evt = {
      type: "newChatItems" as const,
      user: makeUser(GROK_USER_ID),
      chatItems: [
        {chatInfo: {type: "group", groupInfo: makeGroupInfo(GROK_GROUP_A)}, chatItem: ciA},
        {chatInfo: {type: "group", groupInfo: makeGroupInfo(GROK_GROUP_B)}, chatItem: ciB},
      ],
    }

    await bot.onGrokNewChatItems(evt)

    expect(grokApi.calls.length).toBe(2)
  })

  test("batch: non-customer messages filtered, only customer messages trigger response", async () => {
    chat.groups.set(GROK_LOCAL_GROUP_ID, makeGroupInfo(GROK_LOCAL_GROUP_ID))
    addCustomerMessageToHistory("Customer question", GROK_LOCAL_GROUP_ID)

    const custCi = makeChatItem({dir: "groupRcv", text: "Customer question", memberId: CUSTOMER_ID})
    const teamCi = makeChatItem({dir: "groupRcv", text: "Team reply", memberId: "not-customer", memberContactId: TEAM_MEMBER_1_ID})
    const evt = {
      type: "newChatItems" as const,
      user: makeUser(GROK_USER_ID),
      chatItems: [
        {chatInfo: {type: "group", groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID)}, chatItem: custCi},
        {chatInfo: {type: "group", groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID)}, chatItem: teamCi},
      ],
    }

    await bot.onGrokNewChatItems(evt)

    expect(grokApi.calls.length).toBe(1)
    expect(grokApi.calls[0].message).toBe("Customer question")
  })
})

describe("/team Activation", () => {
  beforeEach(() => setup())

  test("/team from QUEUE → ALL team members added, teamAddedMessage sent", async () => {
    await reachQueue()
    addBotMessage("The team can see your message")
    await bot.onNewChatItems(customerMessage("/team"))
    expectMemberAdded(CUSTOMER_GROUP_ID, TEAM_MEMBER_1_ID)
    expectMemberAdded(CUSTOMER_GROUP_ID, TEAM_MEMBER_2_ID)
    expectSentToGroup(CUSTOMER_GROUP_ID, "team member has been added")
  })

  test("/team as first message → WELCOME→TEAM, no queue message", async () => {
    await bot.onNewChatItems(customerMessage("/team"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "team member has been added")
    expectNotSentToGroup(CUSTOMER_GROUP_ID, "The team can see your message")
  })

  test("/team when already activated → teamAlreadyInvitedMessage", async () => {
    await reachTeamPending()
    addBotMessage("A team member has been added")
    chat.members.set(CUSTOMER_GROUP_ID, [makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
    await bot.onNewChatItems(customerMessage("/team"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "already been invited")
  })

  test("/team with no team members → noTeamMembersMessage", async () => {
    setup({teamMembers: []})
    await reachQueue()
    addBotMessage("The team can see your message")
    await bot.onNewChatItems(customerMessage("/team"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "No team members are available")
  })
})

describe("One-Way Gate", () => {
  beforeEach(() => setup())

  test("team member sends first TEXT → Grok removed if present", async () => {
    await reachTeamPending()
    addBotMessage("A team member has been added")
    chat.members.set(CUSTOMER_GROUP_ID, [makeGrokMember(), makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
    await bot.onNewChatItems(teamMemberMessage("Hi, how can I help?"))
    expect(chat.removed.some(r => r.groupId === CUSTOMER_GROUP_ID && r.memberIds.includes(7777))).toBe(true)
  })

  test("team member non-text (no ciContentText) → Grok NOT removed", async () => {
    await reachTeamPending()
    addBotMessage("A team member has been added")
    chat.members.set(CUSTOMER_GROUP_ID, [makeGrokMember()])
    await bot.onNewChatItems(teamMemberMessage("", TEAM_MEMBER_1_ID))
    expect(chat.removed.length).toBe(0)
  })

  test("/grok after gate → teamLockedMessage", async () => {
    await reachTeam()
    await bot.onNewChatItems(customerMessage("/grok"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "team mode")
  })

  test("customer text in TEAM → card update scheduled, no bot reply", async () => {
    await reachTeam()
    const sentBefore = chat.sentTo(CUSTOMER_GROUP_ID).length
    await bot.onNewChatItems(customerMessage("Follow-up question"))
    const sentAfter = chat.sentTo(CUSTOMER_GROUP_ID).length
    expect(sentAfter).toBe(sentBefore)
  })

  test("/grok in TEAM-PENDING → invite Grok if not present", async () => {
    await reachTeamPending()
    addBotMessage("A team member has been added")
    chat.members.set(CUSTOMER_GROUP_ID, [makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
    const joinPromise = simulateGrokJoinSuccess()
    await bot.onNewChatItems(customerMessage("/grok"))
    await joinPromise
    await bot.flush()
    expectMemberAdded(CUSTOMER_GROUP_ID, GROK_CONTACT_ID)
  })
})

describe("One-Way Gate with Grok Disabled", () => {
  test("team text removes Grok even when grokApi is null", async () => {
    setup()
    // Recreate bot without grokApi but with grokContactId still set (simulates disabled Grok with persisted contact)
    bot = new SupportBot(chat as any, null, config as any, MAIN_USER_ID, null)
    bot.cards = cards
    // Reach QUEUE state with Grok + team member already present
    addBotMessage("The team can see your message")
    addBotMessage("A team member has been added")
    chat.members.set(CUSTOMER_GROUP_ID, [makeGrokMember(), makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
    // Team member sends text → one-way gate should fire
    await bot.onNewChatItems(teamMemberMessage("Hi, how can I help?"))
    expect(chat.removed.some(r => r.groupId === CUSTOMER_GROUP_ID && r.memberIds.includes(7777))).toBe(true)
  })

  test("Grok does not respond when disabled even if grokContactId is set", async () => {
    setup()
    bot = new SupportBot(chat as any, null, config as any, MAIN_USER_ID, null)
    bot.cards = cards
    // Set up group with Grok member present
    chat.members.set(CUSTOMER_GROUP_ID, [makeGrokMember()])
    addBotMessage("The team can see your message")
    // Customer sends text in GROK state
    await bot.onNewChatItems(customerMessage("How do I use SimpleX?"))
    // Grok should not respond (grokApi is null)
    expect(grokApi.calls.length).toBe(0)
  })
})

describe("Team Member Lifecycle", () => {
  beforeEach(() => setup())

  test("team member connected → promoted to Owner", async () => {
    await bot.onMemberConnected(connectedEvent(CUSTOMER_GROUP_ID, makeTeamMember(TEAM_MEMBER_1_ID, "Alice")))
    expect(chat.roleChanges.some(r => r.groupId === CUSTOMER_GROUP_ID && r.memberIds.includes(5000 + TEAM_MEMBER_1_ID) && r.role === GroupMemberRole.Owner)).toBe(true)
  })

  test("customer connected → NOT promoted to Owner", async () => {
    await bot.onMemberConnected(connectedEvent(CUSTOMER_GROUP_ID, makeCustomerMember()))
    expect(chat.roleChanges.length).toBe(0)
  })

  test("Grok connected → NOT promoted to Owner", async () => {
    await bot.onMemberConnected(connectedEvent(CUSTOMER_GROUP_ID, makeGrokMember()))
    expect(chat.roleChanges.length).toBe(0)
  })

  test("all team members leave before sending → reverts to QUEUE", async () => {
    await reachTeamPending()
    addBotMessage("A team member has been added")
    // Remove team members from the group
    chat.members.set(CUSTOMER_GROUP_ID, [])
    // Customer sends another message — state should derive as QUEUE (no team members)
    const state = await cards.deriveState(CUSTOMER_GROUP_ID)
    expect(state).toBe("QUEUE")
  })

  test("/team after all team members left (TEAM-PENDING, no msg sent) → re-adds members", async () => {
    await reachTeamPending()
    addBotMessage("A team member has been added")
    chat.members.set(CUSTOMER_GROUP_ID, [])
    chat.added.length = 0

    await bot.onNewChatItems(customerMessage("/team"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "team member has been added")
    expectMemberAdded(CUSTOMER_GROUP_ID, TEAM_MEMBER_1_ID)
  })

  test("/team after all team members left (TEAM, msg was sent) → re-adds members", async () => {
    await reachTeamPending()
    addBotMessage("A team member has been added")
    chat.members.set(CUSTOMER_GROUP_ID, [makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
    addTeamMemberMessageToHistory("Hi, how can I help?", TEAM_MEMBER_1_ID)
    await bot.onNewChatItems(teamMemberMessage("Hi, how can I help?"))

    // All team members leave
    chat.members.set(CUSTOMER_GROUP_ID, [])
    chat.added.length = 0

    await bot.onNewChatItems(customerMessage("/team"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "team member has been added")
    expectMemberAdded(CUSTOMER_GROUP_ID, TEAM_MEMBER_1_ID)
  })
})

describe("Card Dashboard", () => {
  beforeEach(() => setup())

  test("first message creates card with customer name and /join command", async () => {
    await bot.onNewChatItems(customerMessage("Hello"))
    const teamMsgs = chat.sentTo(TEAM_GROUP_ID)
    expect(teamMsgs.length).toBeGreaterThan(0)
    const card = teamMsgs[teamMsgs.length - 1]
    expect(card).toContain(`/join ${CUSTOMER_GROUP_ID}:`)
  })

  test("card /join uses single-quotes for names with spaces", async () => {
    const groupInfo = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "John Doe"})
    chat.groups.set(CUSTOMER_GROUP_ID, groupInfo)
    // Build event with correct groupInfo embedded
    const ci = makeChatItem({dir: "groupRcv", text: "Hello", memberId: CUSTOMER_ID})
    const evt = {
      type: "newChatItems" as const,
      user: makeUser(MAIN_USER_ID),
      chatItems: [{chatInfo: {type: "group" as const, groupInfo}, chatItem: ci}],
    }
    await bot.onNewChatItems(evt)
    const teamMsgs = chat.sentTo(TEAM_GROUP_ID)
    expect(teamMsgs.some(m => m.includes(`/join ${CUSTOMER_GROUP_ID}:'John Doe'`))).toBe(true)
  })

  test("card update deletes old card then posts new one", async () => {
    chat.customData.set(CUSTOMER_GROUP_ID, {cardItemId: 555})
    await cards.flush()
    expect(chat.deleted.length).toBe(0)

    cards.scheduleUpdate(CUSTOMER_GROUP_ID)
    await cards.flush()
    expectCardDeleted(555)
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBeGreaterThan(0)
  })

  test("apiDeleteChatItems failure → ignored, new card posted", async () => {
    chat.customData.set(CUSTOMER_GROUP_ID, {cardItemId: 555})
    chat.apiDeleteChatItemsWillFail()
    cards.scheduleUpdate(CUSTOMER_GROUP_ID)
    await cards.flush()
    // New card should still be posted despite delete failure
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBeGreaterThan(0)
  })

  test("customData stores cardItemId → survives flush cycle", async () => {
    await bot.onNewChatItems(customerMessage("Hello"))
    // After card creation, customData should have cardItemId
    const data = chat.customData.get(CUSTOMER_GROUP_ID)
    expect(data).toBeDefined()
    expect(typeof data.cardItemId).toBe("number")
  })

  test("customer leaves → customData cleared", async () => {
    await bot.onNewChatItems(customerMessage("Hello"))
    chat.customData.set(CUSTOMER_GROUP_ID, {cardItemId: 999})
    await bot.onLeftMember(leftEvent(CUSTOMER_GROUP_ID, makeCustomerMember()))
    expect(chat.customData.has(CUSTOMER_GROUP_ID)).toBe(false)
  })
})

describe("Card Debouncing", () => {
  beforeEach(() => setup())

  test("rapid events within flush interval → single card update on flush", async () => {
    chat.customData.set(CUSTOMER_GROUP_ID, {cardItemId: 500})
    cards.scheduleUpdate(CUSTOMER_GROUP_ID)
    cards.scheduleUpdate(CUSTOMER_GROUP_ID)
    cards.scheduleUpdate(CUSTOMER_GROUP_ID)
    await cards.flush()
    // Only one delete and one post
    expect(chat.deleted.length).toBe(1)
    // Multiple schedules → single update (2 messages per card: text + /join)
    const teamMsgs = chat.sentTo(TEAM_GROUP_ID)
    expect(teamMsgs.length).toBe(2)
  })

  test("multiple groups pending → each reposted once per flush", async () => {
    const GROUP_A = 101
    const GROUP_B = 102
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A))
    chat.groups.set(GROUP_B, makeGroupInfo(GROUP_B))
    chat.customData.set(GROUP_A, {cardItemId: 501})
    chat.customData.set(GROUP_B, {cardItemId: 502})
    cards.scheduleUpdate(GROUP_A)
    cards.scheduleUpdate(GROUP_B)
    await cards.flush()
    expect(chat.deleted.length).toBe(2)
  })

  test("card create is immediate (not debounced)", async () => {
    await bot.onNewChatItems(customerMessage("Hello"))
    // Card should be posted immediately without flush
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBeGreaterThan(0)
  })

  test("flush with no pending updates → no-op", async () => {
    await cards.flush()
    expect(chat.deleted.length).toBe(0)
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBe(0)
  })
})

describe("Card Format & State Derivation", () => {
  beforeEach(() => setup())

  test("QUEUE state derived when no Grok or team members", async () => {
    addBotMessage("The team can see your message")
    const state = await cards.deriveState(CUSTOMER_GROUP_ID)
    expect(state).toBe("QUEUE")
  })

  test("WELCOME state derived for first customer message (no bot messages yet)", async () => {
    const state = await cards.deriveState(CUSTOMER_GROUP_ID)
    expect(state).toBe("WELCOME")
  })

  test("GROK state derived when Grok member present", async () => {
    chat.members.set(CUSTOMER_GROUP_ID, [makeGrokMember()])
    const state = await cards.deriveState(CUSTOMER_GROUP_ID)
    expect(state).toBe("GROK")
  })

  test("TEAM-PENDING derived when team member present but no team message", async () => {
    chat.members.set(CUSTOMER_GROUP_ID, [makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
    const state = await cards.deriveState(CUSTOMER_GROUP_ID)
    expect(state).toBe("TEAM-PENDING")
  })

  test("TEAM derived when team member present AND has sent a message", async () => {
    chat.members.set(CUSTOMER_GROUP_ID, [makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
    addTeamMemberMessageToHistory("Hi!", TEAM_MEMBER_1_ID)
    const state = await cards.deriveState(CUSTOMER_GROUP_ID)
    expect(state).toBe("TEAM")
  })

  test("message count excludes bot's own messages", async () => {
    addCustomerMessageToHistory("Hello")
    addBotMessage("Queue message")
    addCustomerMessageToHistory("Follow-up")
    const chatResult = await cards.getChat(CUSTOMER_GROUP_ID, 100)
    const nonBotCount = chatResult.chatItems.filter((ci: any) => ci.chatDir.type !== "groupSnd").length
    expect(nonBotCount).toBe(2)
  })
})

describe("/join Command (Team Group)", () => {
  beforeEach(() => setup())

  test("/join groupId:name → team member added to customer group", async () => {
    await bot.onNewChatItems(teamGroupMessage(`/join ${CUSTOMER_GROUP_ID}:Customer`))
    expectMemberAdded(CUSTOMER_GROUP_ID, TEAM_MEMBER_1_ID)
  })

  test("/join validates target is business group → error if not", async () => {
    const nonBizGroupId = 999
    chat.groups.set(nonBizGroupId, makeGroupInfo(nonBizGroupId, {businessChat: null}))
    await bot.onNewChatItems(teamGroupMessage(`/join ${nonBizGroupId}:Test`))
    expectSentToGroup(TEAM_GROUP_ID, "not a business chat")
  })

  test("/join with non-existent groupId → error in team group", async () => {
    await bot.onNewChatItems(teamGroupMessage("/join 99999:Nobody"))
    expect(chat.sentTo(TEAM_GROUP_ID).some(m => m.toLowerCase().includes("error"))).toBe(true)
  })

  test("customer sending /join in customer group → treated as normal message", async () => {
    await bot.onNewChatItems(customerMessage("/join 50:Test"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "The team can see your message")
  })
})

describe("DM Handshake", () => {
  beforeEach(() => setup())

  test("team member joins team group → DM sent with contact ID", async () => {
    const member = {memberId: "new-team", groupMemberId: 8000, memberContactId: 30, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: "Charlie"}}
    await bot.onMemberConnected(connectedEvent(TEAM_GROUP_ID, member, {contactId: 30, profile: {displayName: "Charlie"}}))
    expectDmSent(30, "Your contact ID is 30:Charlie")
  })

  test("DM with spaces in name → name single-quoted", async () => {
    const member = {memberId: "new-team", groupMemberId: 8001, memberContactId: 31, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: "Charlie Brown"}}
    await bot.onMemberConnected(connectedEvent(TEAM_GROUP_ID, member, {contactId: 31, profile: {displayName: "Charlie Brown"}}))
    expectDmSent(31, "31:'Charlie Brown'")
  })

  test("pending DM delivered on contactConnected", async () => {
    const invEvt = {
      type: "newMemberContactReceivedInv" as const,
      user: makeUser(MAIN_USER_ID),
      contact: {contactId: 32, profile: {displayName: "Dave"}},
      groupInfo: makeGroupInfo(TEAM_GROUP_ID, {businessChat: null}),
      member: {memberId: "dave-member", groupMemberId: 8002, memberContactId: 32, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: "Dave"}},
    }
    await bot.onMemberContactReceivedInv(invEvt)

    await bot.onContactConnected({
      type: "contactConnected" as const,
      user: makeUser(MAIN_USER_ID),
      contact: {contactId: 32, profile: {displayName: "Dave"}},
    })
    expectDmSent(32, "Your contact ID is 32:Dave")
  })

  test("team member with no DM contact → creates member contact and sends invitation", async () => {
    const member = {memberId: "new-team-no-dm", groupMemberId: 8010, memberContactId: null, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: "Frank"}}
    await bot.onMemberConnected(connectedEvent(TEAM_GROUP_ID, member, undefined))
    expect(chat.memberContacts.some(c => c.groupId === TEAM_GROUP_ID && c.groupMemberId === 8010)).toBe(true)
    expect(chat.memberContactInvitations.some(i => i.text.includes("Your contact ID is") && i.text.includes("Frank"))).toBe(true)
    const dms = chat.sent.filter(s => s.chat[0] === ChatType.Direct)
    expect(dms.some(m => m.text.includes("Your contact ID is") && m.text.includes("Frank"))).toBe(true)
  })

  test("joinedGroupMember in team group → creates member contact and sends invitation", async () => {
    const member = {memberId: "link-joiner", groupMemberId: 8020, memberContactId: null, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: "Grace"}}
    await bot.onJoinedGroupMember(joinedEvent(TEAM_GROUP_ID, member))
    expect(chat.memberContacts.some(c => c.groupId === TEAM_GROUP_ID && c.groupMemberId === 8020)).toBe(true)
    expect(chat.memberContactInvitations.some(i => i.text.includes("Grace"))).toBe(true)
  })

  test("no duplicate DM when both sendTeamMemberDM succeeds and onMemberContactReceivedInv fires", async () => {
    const invEvt = {
      type: "newMemberContactReceivedInv" as const,
      user: makeUser(MAIN_USER_ID),
      contact: {contactId: 33, profile: {displayName: "Eve"}},
      groupInfo: makeGroupInfo(TEAM_GROUP_ID, {businessChat: null}),
      member: {memberId: "eve-member", groupMemberId: 8003, memberContactId: 33, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: "Eve"}},
    }
    await bot.onMemberContactReceivedInv(invEvt)

    const eveMember = {memberId: "eve-member", groupMemberId: 8003, memberContactId: 33, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: "Eve"}}
    await bot.onMemberConnected(connectedEvent(TEAM_GROUP_ID, eveMember, {contactId: 33, profile: {displayName: "Eve"}}))

    await bot.onContactConnected({
      type: "contactConnected" as const,
      user: makeUser(MAIN_USER_ID),
      contact: {contactId: 33, profile: {displayName: "Eve"}},
    })

    const dms = chat.sentDirect(33)
    const contactIdMsgs = dms.filter(m => m.includes("Your contact ID is 33:Eve"))
    expect(contactIdMsgs.length).toBe(1)
  })
})

describe("Direct Message Handling", () => {
  beforeEach(() => setup())

  test("regular DM → bot replies with business address link", async () => {
    bot.businessAddress = "simplex:/contact#abc123"
    await bot.onNewChatItems(directMessage("Hi there", 99))
    expectDmSent(99, "simplex:/contact#abc123")
  })

  test("DM without business address set → no reply", async () => {
    bot.businessAddress = null
    await bot.onNewChatItems(directMessage("Hi there", 99))
    expect(chat.sentDirect(99).length).toBe(0)
  })

  test("non-message DM event (e.g. contactConnected) → no reply", async () => {
    bot.businessAddress = "simplex:/contact#abc123"
    const ci = {
      chatDir: {type: "directRcv"},
      content: {type: "rcvDirectEvent"},
      meta: {itemId: 9999, createdAt: new Date().toISOString()},
    }
    const evt = {
      type: "newChatItems" as const,
      user: makeUser(MAIN_USER_ID),
      chatItems: [makeDirectAChatItem(ci, 99)],
    }
    await bot.onNewChatItems(evt)
    expect(chat.sentDirect(99).length).toBe(0)
  })
})

describe("Business Request Handler", () => {
  beforeEach(() => setup())

  test("acceptingBusinessRequest → enables file uploads AND visible history", async () => {
    await bot.onBusinessRequest({
      type: "acceptingBusinessRequest" as const,
      user: makeUser(MAIN_USER_ID),
      groupInfo: makeGroupInfo(CUSTOMER_GROUP_ID),
    })
    expect(chat.profileUpdates.some(u =>
      u.groupId === CUSTOMER_GROUP_ID
      && u.profile.groupPreferences?.files?.enable === GroupFeatureEnabled.On
      && u.profile.groupPreferences?.history?.enable === GroupFeatureEnabled.On
    )).toBe(true)
  })
})

describe("chatItemUpdated Handler", () => {
  beforeEach(() => setup())

  test("chatItemUpdated in business group → card update scheduled", async () => {
    await bot.onChatItemUpdated(updatedEvent(CUSTOMER_GROUP_ID, makeChatItem({dir: "groupRcv", text: "edited message", memberId: CUSTOMER_ID})))
    chat.customData.set(CUSTOMER_GROUP_ID, {cardItemId: 600})
    await cards.flush()
    expectCardDeleted(600)
  })

  test("chatItemUpdated in non-business group → ignored", async () => {
    await bot.onChatItemUpdated(updatedEvent(TEAM_GROUP_ID, makeChatItem({dir: "groupRcv", text: "team msg"})))
    await cards.flush()
    expect(chat.deleted.length).toBe(0)
  })

  test("chatItemUpdated from wrong user → ignored", async () => {
    await bot.onChatItemUpdated(updatedEvent(CUSTOMER_GROUP_ID, makeChatItem({dir: "groupRcv", text: "edited"}), GROK_USER_ID))
    await cards.flush()
    expect(chat.deleted.length).toBe(0)
  })
})

describe("Reactions", () => {
  beforeEach(() => setup())

  test("reaction in business group → card update scheduled", async () => {
    await bot.onChatItemReaction(reactionEvent(CUSTOMER_GROUP_ID, true))
    chat.customData.set(CUSTOMER_GROUP_ID, {cardItemId: 700})
    await cards.flush()
    expectCardDeleted(700)
  })

  test("reaction removed (added=false) → no card update", async () => {
    await bot.onChatItemReaction(reactionEvent(CUSTOMER_GROUP_ID, false))
    await cards.flush()
    expect(chat.deleted.length).toBe(0)
  })
})

describe("Customer Leave", () => {
  beforeEach(() => setup())

  test("customer leaves → customData cleared", async () => {
    chat.customData.set(CUSTOMER_GROUP_ID, {cardItemId: 800})
    await bot.onLeftMember(leftEvent(CUSTOMER_GROUP_ID, makeCustomerMember()))
    expect(chat.customData.has(CUSTOMER_GROUP_ID)).toBe(false)
  })

  test("Grok leaves → in-memory maps cleaned", async () => {
    await bot.onLeftMember(leftEvent(CUSTOMER_GROUP_ID, makeGrokMember()))
  })

  test("team member leaves → logged, no crash", async () => {
    await bot.onLeftMember(leftEvent(CUSTOMER_GROUP_ID, makeTeamMember(TEAM_MEMBER_1_ID, "Alice")))
  })

  test("leftMember in non-business group → ignored", async () => {
    const member = {memberId: "someone", groupMemberId: 9000, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: "Someone"}}
    await bot.onLeftMember(leftEvent(TEAM_GROUP_ID, member))
  })
})

describe("Error Handling", () => {
  beforeEach(() => setup())

  test("apiAddMember fails (Grok invite) → grokUnavailableMessage", async () => {
    await reachQueue()
    addBotMessage("The team can see your message")
    chat.apiAddMemberWillFail()
    await bot.onNewChatItems(customerMessage("/grok"))
    await bot.flush()
    expectSentToGroup(CUSTOMER_GROUP_ID, "temporarily unavailable")
  })

  test("groupDuplicateMember on Grok invite → only inviting message, no result", async () => {
    await reachQueue()
    addBotMessage("The team can see your message")
    chat.apiAddMemberWillFail({chatError: {errorType: {type: "groupDuplicateMember"}}})
    const sentBefore = chat.sent.length
    await bot.onNewChatItems(customerMessage("/grok"))
    await bot.flush()
    // Only the "Inviting Grok" message is sent — no activated/unavailable result
    expect(chat.sent.length).toBe(sentBefore + 1)
    expectSentToGroup(CUSTOMER_GROUP_ID, "Inviting Grok")
    expectNotSentToGroup(CUSTOMER_GROUP_ID, "now chatting with Grok")
    expectNotSentToGroup(CUSTOMER_GROUP_ID, "temporarily unavailable")
  })

  test("groupDuplicateMember on /team → apiListMembers fallback", async () => {
    await reachQueue()
    addBotMessage("The team can see your message")

    // First team member add succeeds, second fails with groupDuplicateMember
    let callCount = 0
    const origAddMember = chat.apiAddMember.bind(chat)
    chat.apiAddMember = async (groupId: number, contactId: number, role: string) => {
      callCount++
      if (callCount === 2) {
        chat.members.set(groupId, [
          {memberId: `team-${contactId}`, groupMemberId: 5000 + contactId, memberContactId: contactId, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: `Contact${contactId}`}},
        ])
        throw {chatError: {errorType: {type: "groupDuplicateMember"}}}
      }
      return origAddMember(groupId, contactId, role)
    }

    await bot.onNewChatItems(customerMessage("/team"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "team member has been added")
  })
})

describe("Profile / Event Filtering", () => {
  beforeEach(() => setup())

  test("newChatItems from Grok profile → ignored by main handler", async () => {
    const evt = {
      type: "newChatItems" as const,
      user: makeUser(GROK_USER_ID),
      chatItems: [makeAChatItem(makeChatItem({dir: "groupRcv", text: "test"}))],
    }
    const sentBefore = chat.sent.length
    await bot.onNewChatItems(evt)
    expect(chat.sent.length).toBe(sentBefore)
  })

  test("Grok events from main profile → ignored by Grok handlers", async () => {
    const evt = {
      type: "receivedGroupInvitation" as const,
      user: makeUser(MAIN_USER_ID),
      groupInfo: makeGroupInfo(300),
      contact: {contactId: 1},
      fromMemberRole: GroupMemberRole.Admin,
      memberRole: GroupMemberRole.Member,
    }
    await bot.onGrokGroupInvitation(evt)
    expect(chat.joined.length).toBe(0)
  })

  test("own messages (groupSnd) → ignored", async () => {
    const ci = makeChatItem({dir: "groupSnd", text: "Bot message"})
    const evt = {
      type: "newChatItems" as const,
      user: makeUser(MAIN_USER_ID),
      chatItems: [makeAChatItem(ci)],
    }
    const sentBefore = chat.sent.length
    await bot.onNewChatItems(evt)
    expect(chat.sent.length).toBe(sentBefore)
  })

  test("non-business group messages → ignored", async () => {
    const ci = makeChatItem({dir: "groupRcv", text: "test"})
    const nonBizGroup = makeGroupInfo(999, {businessChat: null})
    const evt = {
      type: "newChatItems" as const,
      user: makeUser(MAIN_USER_ID),
      chatItems: [{chatInfo: {type: "group", groupInfo: nonBizGroup}, chatItem: ci}],
    }
    const sentBefore = chat.sent.length
    await bot.onNewChatItems(evt)
    expect(chat.sent.length).toBe(sentBefore)
  })
})

describe("Grok Join Flow", () => {
  beforeEach(() => setup())

  test("Grok receivedGroupInvitation → apiJoinGroup called", async () => {
    // First need to set up a pending grok join
    // Simulate the main profile side: add Grok to a group
    await reachQueue()
    addBotMessage("The team can see your message")

    // This kicks off activateGrok which adds member and waits
    const joinComplete = new Promise<void>(async (resolve) => {
      // Simulate Grok invitation after a small delay
      setTimeout(async () => {
        const addedGrok = chat.added.find(a => a.contactId === GROK_CONTACT_ID)
        if (addedGrok) {
          const memberId = `member-${GROK_CONTACT_ID}`
          await bot.onGrokGroupInvitation({
            type: "receivedGroupInvitation",
            user: makeUser(GROK_USER_ID),
            groupInfo: {...makeGroupInfo(GROK_LOCAL_GROUP_ID), membership: {memberId}},
            contact: {contactId: 99},
            fromMemberRole: GroupMemberRole.Admin,
            memberRole: GroupMemberRole.Member,
          })
        }
        resolve()
      }, 10)
    })

    // Don't await bot.onNewChatItems yet — let it start
    const botPromise = bot.onNewChatItems(customerMessage("/grok"))
    await joinComplete
    // Complete the join
    await bot.onGrokMemberConnected({
      type: "connectedToGroupMember",
      user: makeUser(GROK_USER_ID),
      groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID),
      member: {memberId: "bot-in-grok-view", groupMemberId: 9999},
    })
    await botPromise

    expect(chat.joined).toContain(GROK_LOCAL_GROUP_ID)
  })

  test("unmatched Grok invitation → buffered, not joined", async () => {
    const evt = {
      type: "receivedGroupInvitation" as const,
      user: makeUser(GROK_USER_ID),
      groupInfo: {...makeGroupInfo(999), membership: {memberId: "unknown-member"}},
      contact: {contactId: 99},
      fromMemberRole: GroupMemberRole.Admin,
      memberRole: GroupMemberRole.Member,
    }
    await bot.onGrokGroupInvitation(evt)
    expect(chat.joined.length).toBe(0)
  })

  test("buffered invitation drained after pendingGrokJoins set → apiJoinGroup called", async () => {
    // Simulate the race: invitation arrives before pendingGrokJoins is set
    const memberId = `member-${GROK_CONTACT_ID}`
    const invEvt = {
      type: "receivedGroupInvitation" as const,
      user: makeUser(GROK_USER_ID),
      groupInfo: {...makeGroupInfo(GROK_LOCAL_GROUP_ID), membership: {memberId}},
      contact: {contactId: 99},
      fromMemberRole: GroupMemberRole.Admin,
      memberRole: GroupMemberRole.Member,
    }
    // Buffer the invitation (no pending join registered yet)
    await bot.onGrokGroupInvitation(invEvt)
    expect(chat.joined.length).toBe(0)

    // Now trigger activateGrok — apiAddMember returns, pendingGrokJoins set, buffer drained
    const joinComplete = new Promise<void>((resolve) => {
      setTimeout(async () => {
        // Grok connected after buffer drain processed the invitation
        await bot.onGrokMemberConnected({
          type: "connectedToGroupMember",
          user: makeUser(GROK_USER_ID),
          groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID),
          member: {memberId: "bot-in-grok-view", groupMemberId: 9999},
        })
        resolve()
      }, 20)
    })

    await reachQueue()
    addBotMessage("The team can see your message")
    const botPromise = bot.onNewChatItems(customerMessage("/grok"))
    await joinComplete
    await botPromise
    await bot.flush()

    expect(chat.joined).toContain(GROK_LOCAL_GROUP_ID)
    expectSentToGroup(CUSTOMER_GROUP_ID, "now chatting with Grok")
  })

  test("per-message responses suppressed during activateGrok initial response", async () => {
    await reachQueue()
    addBotMessage("The team can see your message")

    // Customer's message visible in Grok's view (activateGrok reads it for initial response)
    addCustomerMessageToHistory("Hello, I need help", GROK_LOCAL_GROUP_ID)
    chat.groups.set(GROK_LOCAL_GROUP_ID, makeGroupInfo(GROK_LOCAL_GROUP_ID))

    // Start /grok activation (fireAndForget)
    const botPromise = bot.onNewChatItems(customerMessage("/grok"))

    // Wait for apiAddMember to complete
    await new Promise(r => setTimeout(r, 10))

    // Simulate Grok invitation → sets grokGroupMap/reverseGrokMap
    const memberId = `member-${GROK_CONTACT_ID}`
    await bot.onGrokGroupInvitation({
      type: "receivedGroupInvitation",
      user: makeUser(GROK_USER_ID),
      groupInfo: {...makeGroupInfo(GROK_LOCAL_GROUP_ID), membership: {memberId}},
      contact: {contactId: 99},
      fromMemberRole: GroupMemberRole.Admin,
      memberRole: GroupMemberRole.Member,
    })

    // grokInitialResponsePending is set, reverseGrokMap is set.
    // Simulate per-message event (as if message backlog arrived for Grok profile)
    await bot.onGrokNewChatItems(grokViewCustomerMessage("Hello, I need help"))

    // Gating: per-message handler must NOT have called Grok API
    expect(grokApi.calls.length).toBe(0)

    // Now complete the join → activateGrok sends initial combined response
    await bot.onGrokMemberConnected({
      type: "connectedToGroupMember",
      user: makeUser(GROK_USER_ID),
      groupInfo: makeGroupInfo(GROK_LOCAL_GROUP_ID),
      member: {memberId: "bot-in-grok-view", groupMemberId: 9999, memberContactId: undefined},
    })

    await botPromise
    await bot.flush()

    // Only 1 Grok API call: the initial combined response from activateGrok
    expect(grokApi.calls.length).toBe(1)
    expect(grokApi.calls[0].message).toContain("Hello, I need help")
  })

  test("per-message responses resume after activateGrok completes", async () => {
    await reachGrok()
    await bot.flush()
    const callsAfterActivation = grokApi.calls.length

    // Send a new customer message via Grok's view — should be processed normally
    addCustomerMessageToHistory("Follow-up question", GROK_LOCAL_GROUP_ID)
    await bot.onGrokNewChatItems(grokViewCustomerMessage("Follow-up question"))

    expect(grokApi.calls.length).toBe(callsAfterActivation + 1)
    expect(grokApi.calls[grokApi.calls.length - 1].message).toBe("Follow-up question")
  })
})

describe("Grok No-History Fallback", () => {
  beforeEach(() => setup())

  test("Grok joins but sees no customer messages → sends grokNoHistoryMessage", async () => {
    chat.chatItems.set(GROK_LOCAL_GROUP_ID, [])
    chat.groups.set(GROK_LOCAL_GROUP_ID, makeGroupInfo(GROK_LOCAL_GROUP_ID))

    const grokJoinPromise = simulateGrokJoinSuccess()
    await bot.onNewChatItems(customerMessage("/grok"))
    await grokJoinPromise
    await bot.flush()
    expectAnySent("couldn't see your earlier messages")
  })
})

describe("Non-customer messages trigger card update", () => {
  beforeEach(() => setup())

  test("Grok response in customer group → card update scheduled", async () => {
    await bot.onNewChatItems(grokResponseMessage("Grok says hi"))
    chat.customData.set(CUSTOMER_GROUP_ID, {cardItemId: 900})
    await cards.flush()
    expectCardDeleted(900)
  })

  test("team member message → card update scheduled", async () => {
    await bot.onNewChatItems(teamMemberMessage("Team says hi"))
    chat.customData.set(CUSTOMER_GROUP_ID, {cardItemId: 901})
    await cards.flush()
    expectCardDeleted(901)
  })
})

describe("End-to-End Flows", () => {
  beforeEach(() => setup())

  test("WELCOME → QUEUE → /team → TEAM-PENDING → team msg → TEAM", async () => {
    await bot.onNewChatItems(customerMessage("Help me"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "The team can see your message")
    addBotMessage("The team can see your message")

    await bot.onNewChatItems(customerMessage("/team"))
    expectSentToGroup(CUSTOMER_GROUP_ID, "team member has been added")
    expectMemberAdded(CUSTOMER_GROUP_ID, TEAM_MEMBER_1_ID)
    addBotMessage("A team member has been added")

    chat.members.set(CUSTOMER_GROUP_ID, [makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
    const pendingState = await cards.deriveState(CUSTOMER_GROUP_ID)
    expect(pendingState).toBe("TEAM-PENDING")

    addTeamMemberMessageToHistory("I'll help you", TEAM_MEMBER_1_ID)
    await bot.onNewChatItems(teamMemberMessage("I'll help you"))

    const teamState = await cards.deriveState(CUSTOMER_GROUP_ID)
    expect(teamState).toBe("TEAM")
  })

  test("WELCOME → /grok first msg → GROK", async () => {
    const joinPromise = simulateGrokJoinSuccess()
    await bot.onNewChatItems(customerMessage("/grok"))
    await joinPromise
    await bot.flush()

    expectSentToGroup(CUSTOMER_GROUP_ID, "now chatting with Grok")
    expectNotSentToGroup(CUSTOMER_GROUP_ID, "The team can see your message")
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBeGreaterThan(0)
  })

  test("multiple concurrent conversations are independent", async () => {
    const GROUP_A = 101
    const GROUP_B = 102
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A, {customerId: "cust-a"}))
    chat.groups.set(GROUP_B, makeGroupInfo(GROUP_B, {customerId: "cust-b"}))

    const ciA = makeChatItem({dir: "groupRcv", text: "Help A", memberId: "cust-a"})
    await bot.onNewChatItems({
      type: "newChatItems",
      user: makeUser(MAIN_USER_ID),
      chatItems: [{chatInfo: {type: "group", groupInfo: makeGroupInfo(GROUP_A, {customerId: "cust-a"})}, chatItem: ciA}],
    })

    const ciB = makeChatItem({dir: "groupRcv", text: "Help B", memberId: "cust-b"})
    await bot.onNewChatItems({
      type: "newChatItems",
      user: makeUser(MAIN_USER_ID),
      chatItems: [{chatInfo: {type: "group", groupInfo: makeGroupInfo(GROUP_B, {customerId: "cust-b"})}, chatItem: ciB}],
    })

    expectSentToGroup(GROUP_A, "The team can see your message")
    expectSentToGroup(GROUP_B, "The team can see your message")
  })
})

describe("Message Templates", () => {
  test("welcomeMessage is a non-empty string", () => {
    expect(typeof welcomeMessage).toBe("string")
    expect(welcomeMessage.length).toBeGreaterThan(0)
  })

  test("grokActivatedMessage mentions Grok can see earlier messages", () => {
    expect(grokActivatedMessage).toContain("Grok can see your earlier messages")
  })

  test("teamLockedMessage mentions team mode", () => {
    expect(teamLockedMessage).toContain("team mode")
  })

  test("queueMessage mentions hours", () => {
    const msg = queueMessage("UTC", true)
    expect(msg).toContain("hours")
  })
})

describe("isFirstCustomerMessage detection", () => {
  beforeEach(() => setup())

  test("detects 'The team can see your message' as queue message", async () => {
    addBotMessage("The team can see your message. A reply may take up to 24 hours.")
    const isFirst = await cards.isFirstCustomerMessage(CUSTOMER_GROUP_ID)
    expect(isFirst).toBe(false)
  })

  test("detects 'now chatting with Grok' as grok activation", async () => {
    addBotMessage("You are now chatting with Grok.")
    const isFirst = await cards.isFirstCustomerMessage(CUSTOMER_GROUP_ID)
    expect(isFirst).toBe(false)
  })

  test("detects 'team member has been added' as team activation", async () => {
    addBotMessage("A team member has been added and will reply within 24 hours.")
    const isFirst = await cards.isFirstCustomerMessage(CUSTOMER_GROUP_ID)
    expect(isFirst).toBe(false)
  })

  test("detects 'team member has already been invited'", async () => {
    addBotMessage("A team member has already been invited to this conversation.")
    const isFirst = await cards.isFirstCustomerMessage(CUSTOMER_GROUP_ID)
    expect(isFirst).toBe(false)
  })

  test("returns true when no bot messages present", async () => {
    const isFirst = await cards.isFirstCustomerMessage(CUSTOMER_GROUP_ID)
    expect(isFirst).toBe(true)
  })

  test("returns true when only unrelated bot messages present", async () => {
    addBotMessage("Some other message")
    const isFirst = await cards.isFirstCustomerMessage(CUSTOMER_GROUP_ID)
    expect(isFirst).toBe(true)
  })
})

describe("Card Preview Sender Prefixes", () => {
  beforeEach(() => setup())

  // Helper: extract preview line from card text posted to team group
  function getCardPreview(): string {
    const teamMsgs = chat.sentTo(TEAM_GROUP_ID)
    // Card text is the first sent message; /join command is the second
    const cardText = teamMsgs[0]
    if (!cardText) return ""
    const lines = cardText.split("\n")
    // Preview is the last line of the card
    return lines[lines.length - 1] || ""
  }

  test("customer-only messages: first prefixed, rest not", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    addCustomerMessageToHistory("Hello")
    addCustomerMessageToHistory("Need help")
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).toContain("Alice: Hello")
    expect(preview).toContain("!3 /! Need help")
    // Second message must NOT have prefix (same sender)
    expect(preview).not.toContain("Alice: Need help")
  })

  test("three consecutive customer messages: only first gets prefix", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    addCustomerMessageToHistory("First")
    addCustomerMessageToHistory("Second")
    addCustomerMessageToHistory("Third")
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    const prefixCount = (preview.match(/Alice:/g) || []).length
    expect(prefixCount).toBe(1)
    expect(preview).toContain("Alice: First")
  })

  test("alternating customer and Grok: each sender change triggers prefix", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    addCustomerMessageToHistory("How does encryption work?")
    addGrokMessageToHistory("SimpleX uses double ratchet")
    addCustomerMessageToHistory("And metadata?")
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).toContain("Alice: How does encryption work?")
    expect(preview).toContain("Grok: SimpleX uses double ratchet")
    expect(preview).toContain("Alice: And metadata?")
  })

  test("Grok identified by grokContactId, not by display name", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    // Grok message uses GROK_CONTACT_ID → labeled "Grok" regardless of memberProfile
    addGrokMessageToHistory("I am Grok")
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).toContain("Grok: I am Grok")
  })

  test("team member messages use their memberProfile displayName", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    addCustomerMessageToHistory("Help please")
    // Add team member message with explicit display name
    const teamCi = makeChatItem({
      dir: "groupRcv", text: "On it!",
      memberId: `team-${TEAM_MEMBER_1_ID}`, memberContactId: TEAM_MEMBER_1_ID,
      memberDisplayName: "Bob",
    })
    const items = chat.chatItems.get(CUSTOMER_GROUP_ID) || []
    items.push(teamCi)
    chat.chatItems.set(CUSTOMER_GROUP_ID, items)
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).toContain("Alice: Help please")
    expect(preview).toContain("Bob: On it!")
  })

  test("bot messages (groupSnd) excluded from preview", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    addCustomerMessageToHistory("Hello")
    addBotMessage("The team can see your message")
    addCustomerMessageToHistory("Thanks")
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).not.toContain("The team can see your message")
    // Both customer messages are from the same sender — only first prefixed
    expect(preview).toContain("Alice: Hello")
    expect(preview).toContain("!3 /! Thanks")
  })

  test("media-only message shows type label", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    const imgCi = makeChatItem({dir: "groupRcv", text: "", memberId: CUSTOMER_ID, msgType: "image"})
    const items = chat.chatItems.get(CUSTOMER_GROUP_ID) || []
    items.push(imgCi)
    chat.chatItems.set(CUSTOMER_GROUP_ID, items)
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).toContain("[image]")
  })

  test("media message with caption shows label + text", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    const imgCi = makeChatItem({dir: "groupRcv", text: "screenshot of the bug", memberId: CUSTOMER_ID, msgType: "image"})
    const items = chat.chatItems.get(CUSTOMER_GROUP_ID) || []
    items.push(imgCi)
    chat.chatItems.set(CUSTOMER_GROUP_ID, items)
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).toContain("[image] screenshot of the bug")
  })

  test("long message truncated with [truncated]", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    const longMsg = "x".repeat(300)
    addCustomerMessageToHistory(longMsg)
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).toContain("[truncated]")
    // Truncated at ~200 chars + prefix
    expect(preview.length).toBeLessThan(300)
  })

  test("total overflow truncates oldest messages, keeps newest", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    // Add many messages to exceed 1000 chars total
    for (let i = 0; i < 20; i++) {
      addCustomerMessageToHistory(`Message number ${i} with some extra padding text to fill space quickly`)
    }
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).toContain("[truncated]")
    // Newest messages should be present, oldest truncated
    expect(preview).toContain("Message number 19")
    expect(preview).not.toContain("Message number 0")
    // Should not include all 20 messages
    const slashCount = (preview.match(/ \/ /g) || []).length
    expect(slashCount).toBeLessThan(19)
  })

  test("empty preview when no messages", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).toBe('""')
  })

  test("only bot messages → empty preview", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    addBotMessage("Welcome!")
    addBotMessage("Queue message")
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).toBe('""')
  })

  test("newlines in message text → replaced with spaces", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "Alice"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    addCustomerMessageToHistory("line1\nline2\n\nline3")
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const preview = getCardPreview()
    expect(preview).not.toContain("\n")
    expect(preview).toContain("line1 line2 line3")
  })

  test("newlines in customer display name → sanitized in card header, raw in /join", async () => {
    const gi = makeGroupInfo(CUSTOMER_GROUP_ID, {displayName: "First\nLast"})
    chat.groups.set(CUSTOMER_GROUP_ID, gi)
    addCustomerMessageToHistory("Hello")
    await cards.createCard(CUSTOMER_GROUP_ID, gi)
    const teamMsgs = chat.sentTo(TEAM_GROUP_ID)
    const cardText = teamMsgs[0]
    // Card header should have sanitized name (no newlines)
    expect(cardText).toContain("First Last")
    expect(cardText.split("\n").length).toBe(3) // exactly 3 lines: header, state, preview
    // /join command (second message) should use raw name
    const joinCmd = teamMsgs[1]
    expect(joinCmd).toContain("First\nLast")
  })
})

describe("Restart Card Recovery", () => {
  beforeEach(() => setup())

  test("refreshAllCards refreshes groups with active cards", async () => {
    const GROUP_A = 101
    const GROUP_B = 102
    const GROUP_NO_CARD = 103
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A))
    chat.groups.set(GROUP_B, makeGroupInfo(GROUP_B))
    chat.groups.set(GROUP_NO_CARD, makeGroupInfo(GROUP_NO_CARD))
    chat.customData.set(GROUP_A, {cardItemId: 501, joinItemId: 502})
    chat.customData.set(GROUP_B, {cardItemId: 503, joinItemId: 504})

    await cards.refreshAllCards()

    expectCardDeleted(501)
    expectCardDeleted(503)
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBe(4) // 2 cards × 2 messages each
  })

  test("refreshAllCards with no active cards → no-op", async () => {
    await cards.refreshAllCards()
    expect(chat.deleted.length).toBe(0)
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBe(0)
  })

  test("refreshAllCards ignores groups without cardItemId in customData", async () => {
    const GROUP_A = 101
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A))
    chat.customData.set(GROUP_A, {someOtherData: true})

    await cards.refreshAllCards()
    expect(chat.deleted.length).toBe(0)
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBe(0)
  })

  test("refreshAllCards orders by cardItemId ascending (oldest first, newest last)", async () => {
    // GROUP_C has higher cardItemId (more recent) than GROUP_A and GROUP_B
    const GROUP_A = 101, GROUP_B = 102, GROUP_C = 103
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A))
    chat.groups.set(GROUP_B, makeGroupInfo(GROUP_B))
    chat.groups.set(GROUP_C, makeGroupInfo(GROUP_C))
    chat.customData.set(GROUP_C, {cardItemId: 900}) // newest — should refresh last
    chat.customData.set(GROUP_A, {cardItemId: 100}) // oldest — should refresh first
    chat.customData.set(GROUP_B, {cardItemId: 500}) // middle

    await cards.refreshAllCards()

    // Verify deletion order: oldest cardItemId first, newest last
    expect(chat.deleted.length).toBe(3)
    expect(chat.deleted[0].itemIds).toEqual([100])
    expect(chat.deleted[1].itemIds).toEqual([500])
    expect(chat.deleted[2].itemIds).toEqual([900])

    // Newest card's messages are posted last → appear at bottom of team group
    const teamMsgs = chat.sentTo(TEAM_GROUP_ID)
    expect(teamMsgs.length).toBe(6) // 3 cards × 2 messages each
  })

  test("refreshAllCards skips cards marked complete", async () => {
    const GROUP_A = 101, GROUP_B = 102
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A))
    chat.groups.set(GROUP_B, makeGroupInfo(GROUP_B))
    chat.customData.set(GROUP_A, {cardItemId: 100, complete: true})
    chat.customData.set(GROUP_B, {cardItemId: 200})

    await cards.refreshAllCards()

    expect(chat.deleted.length).toBe(1)
    expect(chat.deleted[0].itemIds).toEqual([200])
    expect(chat.deleted.some(d => d.itemIds.includes(100))).toBe(false)
  })

  test("refreshAllCards deletes old card before reposting", async () => {
    const GROUP_A = 101
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A))
    chat.customData.set(GROUP_A, {cardItemId: 501, joinItemId: 502})

    await cards.refreshAllCards()

    // Old card + join command should be deleted
    expect(chat.deleted.length).toBe(1)
    expect(chat.deleted[0].itemIds).toEqual([501, 502])
    // New card posted
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBe(2)
  })

  test("refreshAllCards ignores delete failure (>24h old card)", async () => {
    const GROUP_A = 101
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A))
    chat.customData.set(GROUP_A, {cardItemId: 501})
    chat.apiDeleteChatItemsWillFail()

    await cards.refreshAllCards()

    // Delete failed but new card still posted
    expect(chat.sentTo(TEAM_GROUP_ID).length).toBe(2)
    // customData updated with new cardItemId
    const newData = chat.customData.get(GROUP_A)
    expect(typeof newData.cardItemId).toBe("number")
    expect(newData.cardItemId).not.toBe(501) // new ID, not the old one
  })

  test("card flush writes complete: true for auto-completed conversations", async () => {
    const GROUP_A = 101
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A))
    chat.members.set(GROUP_A, [makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
    // Team member message from 4 hours ago (> completeHours=3h) → auto-complete
    const oldCi = makeChatItem({dir: "groupRcv", text: "Resolved!", memberId: `team-${TEAM_MEMBER_1_ID}`, memberContactId: TEAM_MEMBER_1_ID})
    oldCi.meta.createdAt = new Date(Date.now() - 4 * 3600_000).toISOString()
    chat.chatItems.set(GROUP_A, [oldCi])
    // Create initial card data
    chat.customData.set(GROUP_A, {cardItemId: 500})

    cards.scheduleUpdate(GROUP_A)
    await cards.flush()

    const data = chat.customData.get(GROUP_A)
    expect(data.complete).toBe(true)
  })

  test("card flush clears complete flag when conversation becomes active again", async () => {
    const GROUP_A = 101
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A))
    chat.members.set(GROUP_A, [makeTeamMember(TEAM_MEMBER_1_ID, "Alice")])
    // Team member message from 4h ago + recent customer message → NOT complete
    const teamCi = makeChatItem({dir: "groupRcv", text: "Resolved!", memberId: `team-${TEAM_MEMBER_1_ID}`, memberContactId: TEAM_MEMBER_1_ID})
    teamCi.meta.createdAt = new Date(Date.now() - 4 * 3600_000).toISOString()
    const custCi = makeChatItem({dir: "groupRcv", text: "Actually one more question", memberId: CUSTOMER_ID})
    chat.chatItems.set(GROUP_A, [teamCi, custCi])
    // Previously complete
    chat.customData.set(GROUP_A, {cardItemId: 500, complete: true})

    cards.scheduleUpdate(GROUP_A)
    await cards.flush()

    const data = chat.customData.get(GROUP_A)
    expect(data.complete).toBeUndefined()
  })

  test("refreshAllCards continues on individual card failure", async () => {
    const GROUP_A = 101, GROUP_B = 102
    chat.groups.set(GROUP_A, makeGroupInfo(GROUP_A))
    chat.groups.set(GROUP_B, makeGroupInfo(GROUP_B))
    chat.customData.set(GROUP_A, {cardItemId: 100})
    chat.customData.set(GROUP_B, {cardItemId: 200})

    chat.apiDeleteChatItemsWillFail()
    await cards.refreshAllCards()
    expectCardDeleted(200)
  })
})

describe("joinedGroupMember Event Filtering", () => {
  beforeEach(() => setup())

  test("joinedGroupMember in non-team group → ignored (no DM)", async () => {
    const member = {memberId: "someone", groupMemberId: 9000, memberContactId: null, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: "Someone"}}
    await bot.onJoinedGroupMember(joinedEvent(CUSTOMER_GROUP_ID, member))
    expect(chat.rawCmds.length).toBe(0)
    expect(chat.sent.filter(s => s.chat[0] === ChatType.Direct).length).toBe(0)
  })

  test("joinedGroupMember from wrong user → ignored", async () => {
    const member = {memberId: "someone", groupMemberId: 9001, memberContactId: null, memberStatus: GroupMemberStatus.Connected, memberProfile: {displayName: "Someone"}}
    await bot.onJoinedGroupMember(joinedEvent(TEAM_GROUP_ID, member, GROK_USER_ID))
    expect(chat.rawCmds.length).toBe(0)
  })
})
