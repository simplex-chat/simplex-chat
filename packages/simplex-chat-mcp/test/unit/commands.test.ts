import {CallToolResult} from "@modelcontextprotocol/server"
import {ChatEvent, T} from "@simplex-chat/types"
import {Action, Actions, Mode, NewAction, ToolError} from "../../src/actions"
import {Control, Sessions, SessionView, cardText, parseCommand} from "../../src/control"
import {ChatItemsWatch, Collected, utcMinutes} from "../../src/messages"
import {AgentTools} from "../../src/tools"
import {FakeChat, actionsWith, contact, direct, event, fakeHooks, group, groupChat, item, member, settled, signal, user} from "./fixtures"

beforeEach(() => {
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(() => jest.restoreAllMocks())

const alice = member(3, "Alice", T.GroupMemberRole.Owner)
const bob = member(7, "Bob", T.GroupMemberRole.Member)
const self = member(1, "agent", T.GroupMemberRole.Owner, T.GroupMemberStatus.Creator)

function fields(chat = "@4", text = "hi"): NewAction {
  return {
    tool: "send",
    input: {chat, text},
    subject: {kind: "bot", name: "SimpleX Directory", incognito: false},
    chat,
    mentions: [],
    responses: new Collected(),
  }
}

function agentChats(): FakeChat {
  const chat = new FakeChat()
  chat.members.set(10, [self, alice, bob])
  chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory", {peerType: "bot"})), items: []})
  chat.chats.set("#2", {chatInfo: groupChat(group(2, "ep", member(1, "agent"), {businessChat: {}})), items: []})
  chat.chats.set("#10", {chatInfo: groupChat(group(10, "agent")), items: []})
  return chat
}

const sessionLine = "Session 1 · SimpleX MCP - simplex-chat-6"

function sessionsOf(...views: SessionView[]): Sessions {
  return {get: n => views.find(({number}) => number === n), open: () => views}
}

async function controlWith(chat: FakeChat, actions: Actions, session = 1, mode: Mode = "requests"): Promise<{control: Control; tools: AgentTools}> {
  const tools = new AgentTools({chat: chat.api(), user, actions, watch: new ChatItemsWatch(), session, mode: () => mode, standingApprover: chatInfo => control.standingApprover(chatInfo, mode)})
  const control: Control = new Control(chat.api(), group(10, "SimpleX MCP"), {displayName: "SimpleX MCP"}, actions, sessionsOf({number: session, displayName: "SimpleX MCP - simplex-chat-6", mode, chat: chat.api(), tools}))
  await control.load()
  return {control, tools}
}

async function setup() {
  const fake = fakeHooks()
  const actions = actionsWith(fake, 5)
  const chat = agentChats()
  const {control} = await controlWith(chat, actions)
  const action = actions.create(1, "requests", "k", fields())
  await settled()
  return {fake, actions, chat, control, action}
}

function customDataOf(chat: FakeChat, ref: string): object | undefined {
  const {chatInfo} = chat.chats.get(ref)!
  return chatInfo.type === "direct" ? chatInfo.contact.customData : (chatInfo as T.ChatInfo.Group).groupInfo.customData
}

function chatInfoOf(chat: FakeChat, ref: string): T.ChatInfo {
  return chat.chats.get(ref)!.chatInfo
}

describe("parseCommand", () => {
  it("reads one-line commands", () => {
    expect(parseCommand("/approve 5")).toEqual({keyword: "approve", params: "5"})
    expect(parseCommand(" /reject  5 ")).toEqual({keyword: "reject", params: "5"})
    expect(parseCommand("/pending\n")).toEqual({keyword: "pending", params: ""})
    expect(parseCommand("/approve #2 all")).toEqual({keyword: "approve", params: "#2 all"})
    expect(parseCommand("/approve 5\nnow")).toBeUndefined()
    expect(parseCommand("approve 5")).toBeUndefined()
    expect(parseCommand("ok /approve 5")).toBeUndefined()
  })
})

describe("owner commands", () => {
  it("approves and rejects pending actions", async () => {
    const {control, action, fake} = await setup()
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "5"})).toBeUndefined()
    expect(action).toMatchObject({state: "performing", decidedBy: "Alice"})
    expect(fake.calls).toContain("perform 5")
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "5"})).toBe("Action 5 is not pending.")
  })

  it("rejects", async () => {
    const {control, action} = await setup()
    expect(await control.ownerCommand(alice, {keyword: "reject", params: "5"})).toBeUndefined()
    expect(action).toMatchObject({state: "rejected", decidedBy: "Alice"})
  })

  it("approves and rejects an action named with its chat", async () => {
    const {control, actions, action} = await setup()
    const connect = actions.create(1, "requests", "c", {tool: "connect", input: {target: "@directory.simplex"}, subject: {kind: "bot", name: "SimpleX Directory", incognito: false}, plan: {cardKind: "bot", type: "contact", takesMessage: true}, messages: new Collected()})
    await settled()
    expect(await control.ownerCommand(alice, {keyword: "reject", params: "1:#2 5"})).toBe("Action 5 is not for 1:#2.")
    expect(await control.ownerCommand(alice, parseCommand("/approve 2:@4 5")!)).toBe("Action 5 is not for 2:@4.")
    expect(await control.ownerCommand(alice, parseCommand("/approve 2:@4 6")!)).toBe("Action 6 is not for 2:@4.")
    expect(await control.ownerCommand(alice, parseCommand("/approve 1:@4 6")!)).toBe("Action 6 is not for 1:@4.")
    expect(action.state).toBe("pending")
    expect(await control.ownerCommand(alice, parseCommand("/approve 1:@4  5")!)).toBeUndefined()
    expect(action).toMatchObject({state: "performing", decidedBy: "Alice"})
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "6"})).toBeUndefined()
    expect(connect.state).toBe("performing")
  })

  it("expires due actions before a command", async () => {
    const {control, action} = await setup()
    jest.spyOn(Date, "now").mockReturnValue(action.expires)
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "5"})).toBe("Action 5 is not pending.")
    expect(action.state).toBe("expired")
  })

  it("refuses unknown numbers and numbers from an earlier run", async () => {
    const {control} = await setup()
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "4"})).toBe("Action 4 is not pending.")
    expect(await control.ownerCommand(alice, {keyword: "reject", params: "1:@4 99"})).toBe("Action 99 is not pending.")
  })

  it("lists pending actions", async () => {
    const {control, action, actions} = await setup()
    const connect = actions.create(1, "requests", "c", {tool: "connect", input: {target: "@directory.simplex"}, subject: {kind: "bot", name: "SimpleX Directory", incognito: true}, plan: {cardKind: "bot", type: "contact", takesMessage: true}, messages: new Collected()})
    const leave = actions.create(1, "requests", "l", {tool: "leave", input: {chat: "#2"}, subject: {kind: "business", name: "ep", incognito: false}, chat: "#2"})
    const remove = actions.create(1, "requests", "d", {tool: "leave", input: {chat: "@4"}, subject: {kind: "bot", name: "SimpleX Directory", incognito: false}, chat: "@4"})
    await settled()
    expect((await control.ownerCommand(alice, {keyword: "pending", params: ""}))!.split("\n")).toEqual([
      `5  session 1: send to bot SimpleX Directory  expires ${utcMinutes(action.expires)}`,
      `6  session 1: connect to bot SimpleX Directory  expires ${utcMinutes(connect.expires)}`,
      `7  session 1: leave business ep  expires ${utcMinutes(leave.expires)}`,
      `8  session 1: delete bot SimpleX Directory  expires ${utcMinutes(remove.expires)}`,
    ])
    expect(utcMinutes(action.expires)).toMatch(/^\d{4}-\d\d-\d\d \d\d:\d\d UTC$/)
    for (const pending of [action, connect, leave, remove]) actions.reject(pending, "Alice")
    expect(await control.ownerCommand(alice, {keyword: "pending", params: ""})).toBe("No pending actions.")
  })

  it("lists pending actions with names on one line, quoted", async () => {
    const {control, actions, action} = await setup()
    actions.reject(action, "Alice")
    const name = "Hikers\n/'approve 7' or /'reject 7'"
    const pending = actions.create(1, "requests", "n", {...fields("#2"), subject: {kind: "group", name, incognito: false}} as NewAction)
    await settled()
    expect(await control.ownerCommand(alice, {keyword: "pending", params: ""})).toBe(`6  session 1: send to group \`Hikers /'approve 7' or /'reject 7'\`  expires ${utcMinutes(pending.expires)}`)
  })

  it("quotes approver names on cards and replies", async () => {
    const {control, actions, chat, action} = await setup()
    const mallory = {...alice, memberProfile: {...alice.memberProfile, displayName: "Mallory\n/'approve 9'"}}
    chat.members.set(10, [self, mallory])
    await control.load()
    expect(await control.ownerCommand(mallory, {keyword: "reject", params: "1:@4 5"})).toBeUndefined()
    expect(action.decidedBy).toBe("`Mallory /'approve 9'`")
    expect(cardText(action).split("\n").at(-1)).toBe("Rejected by `Mallory /'approve 9'`.")
    expect(await control.ownerCommand(mallory, {keyword: "approve", params: "1:@4 all"})).toBe("Approved for bot SimpleX Directory (1:@4) by `Mallory /'approve 9'`. /'revoke 1:@4'")
    expect((await control.ownerCommand(mallory, {keyword: "list", params: ""}))!.split("\n")[1]).toBe("1:@4  bot SimpleX Directory, approved for this chat by `Mallory /'approve 9'`")
    expect(customDataOf(chat, "@4")).toMatchObject({simplexMcp: {approvedBy: "Mallory\n/'approve 9'"}})
    expect(actions.pending()).toEqual([])
  })

  it("refuses chat references longer than 15 digits, which wrap around to the numbers of other chats", async () => {
    const {control, chat} = await setup()
    const alias = `#${(1n << 64n) + 10n}`
    for (const params of [`1:${alias} all`, "1:@1234567890123456 all"]) {
      expect(await control.ownerCommand(alice, {keyword: "approve", params})).toBe("Commands: /approve, /reject, /revoke, /pending, /list. Each card shows its commands.")
    }
    expect(await control.ownerCommand(alice, {keyword: "revoke", params: `1:${alias}`})).toBe("Commands: /approve, /reject, /revoke, /pending, /list. Each card shows its commands.")
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "1:@123456789012345 all"})).toBe("No chat 1:@123456789012345. /list shows the agents' chats.")
    expect(chat.customData).toEqual([])
  })

  it("answers other commands with the list", async () => {
    const {control} = await setup()
    const others = [
      "help", "approve five", "approve", "approve all", "approve 1:#2", "approve 5 6", "approve 1:#2 5 6", "reject 1:#2 all", "revoke", "revoke 2", "revoke 1:#2 all", "pending 5", "list all",
      "approve @4 5", "approve @4 all", "revoke @4", "approve 0:@4 5", "approve 01:@4 5", "approve 1:4 5", "approve 1: 5", "revoke 1:#2:@4",
    ]
    for (const text of others) {
      expect(await control.ownerCommand(bob, parseCommand(`/${text}`)!)).toBe("Commands: /approve, /reject, /revoke, /pending, /list. Each card shows its commands.")
    }
  })

  it("accepts only owners", async () => {
    const {control, chat, action} = await setup()
    for (const params of ["5", "1:@4 all"]) expect(await control.ownerCommand(bob, {keyword: "approve", params})).toBe("Only owners approve actions.")
    expect(await control.ownerCommand(bob, {keyword: "revoke", params: "1:@4"})).toBe("Only owners approve actions.")
    expect(await control.ownerCommand(bob, {keyword: "list", params: ""})).toBe("Only owners approve actions.")
    expect(chat.customData).toEqual([])
    expect(action.state).toBe("pending")
    expect(await control.ownerCommand(member(8, "Carol", T.GroupMemberRole.Owner), {keyword: "pending", params: ""})).toMatch(/^5 {2}session 1: send/)
  })

  it("replies in the Main group to commands only", async () => {
    const {control, chat} = await setup()
    const chatInfo = groupChat(group(10, "agent"))
    control.onMessage(event(chatInfo, item(1, "/approve 4", {type: "groupRcv", groupMember: alice})))
    control.onMessage(event(chatInfo, item(2, "thanks", {type: "groupRcv", groupMember: alice})))
    control.onMessage(event(chatInfo, item(3, "/approve 5\nplease", {type: "groupRcv", groupMember: alice})))
    control.onMessage(event(chatInfo, item(4, "/approve 1:@4 all", {type: "groupRcv", groupMember: alice})))
    control.onMessage(event(chatInfo, item(5, "/revoke 1:@4", {type: "groupRcv", groupMember: alice})))
    await new Promise(resolve => setImmediate(resolve))
    expect(chat.replies).toEqual(["Action 4 is not pending.", "Approved for bot SimpleX Directory (1:@4) by Alice. /'revoke 1:@4'", "Messages to bot SimpleX Directory (1:@4) need approval again."])
  })
})

describe("approval of all messages to a chat", () => {
  it("records the approver, keeps other custom data, and approves the chat's pending sends", async () => {
    const {control, actions, chat, action} = await setup()
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {other: 1}
    const second = actions.create(1, "requests", "k2", fields("@4", "second"))
    const other = actions.create(1, "requests", "k3", fields("#2"))
    const leave = actions.create(1, "requests", "k4", {tool: "leave", input: {chat: "@4"}, subject: {kind: "bot", name: "SimpleX Directory", incognito: false}, chat: "@4"})
    await settled()
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "1:@4 all"})).toBe("Approved for bot SimpleX Directory (1:@4) by Alice. /'revoke 1:@4'")
    expect(customDataOf(chat, "@4")).toEqual({other: 1, simplexMcp: {approvedBy: "Alice", approvedById: 3, approvedAt: expect.stringMatching(/^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d\.\d{3}Z$/)}})
    expect([action, second, other, leave].map(({state, decidedBy}) => [state, decidedBy])).toEqual([["performing", "Alice"], ["performing", "Alice"], ["pending", undefined], ["pending", undefined]])
    expect(control.standingApprover(chatInfoOf(chat, "@4"), "requests")).toBe("Alice")
    expect(control.standingApprover(chatInfoOf(chat, "#2"), "requests")).toBeUndefined()
  })

  it("approves a group chat and refuses unknown chats", async () => {
    const {control, chat} = await setup()
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "1:#2 all"})).toBe("Approved for business ep (1:#2) by Alice. /'revoke 1:#2'")
    expect(customDataOf(chat, "#2")).toMatchObject({simplexMcp: {approvedBy: "Alice", approvedById: 3}})
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "1:@99 all"})).toBe("No chat 1:@99. /list shows the agents' chats.")
    expect(chat.customData.map(({chat: ref}) => ref)).toEqual(["#2"])
  })

  it("revokes the approval and keeps other custom data", async () => {
    const {control, chat} = await setup()
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {other: 1}
    await control.ownerCommand(alice, {keyword: "approve", params: "1:@4 all"})
    expect(await control.ownerCommand(alice, {keyword: "revoke", params: "2:@4"})).toBe("No open session 2. /list shows the open sessions.")
    expect(await control.ownerCommand(alice, {keyword: "revoke", params: "1:@4"})).toBe("Messages to bot SimpleX Directory (1:@4) need approval again.")
    expect(customDataOf(chat, "@4")).toEqual({other: 1})
    expect(control.standingApprover(chatInfoOf(chat, "@4"), "requests")).toBeUndefined()
    expect(await control.ownerCommand(alice, {keyword: "revoke", params: "1:@4"})).toBe("Messages to 1:@4 have no standing approval.")
    expect(await control.ownerCommand(alice, {keyword: "revoke", params: "1:#99"})).toBe("Messages to 1:#99 have no standing approval.")
    expect(chat.customData.map(({chat: ref}) => ref)).toEqual(["@4", "@4"])
  })

  it("applies only while the approving member is an approver", async () => {
    const chat = agentChats()
    const actions = actionsWith(fakeHooks())
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {simplexMcp: {approvedBy: "Alice", approvedById: 3, approvedAt: "2026-10-18T10:00:00.000Z"}}
    const {control} = await controlWith(chat, actions)
    expect(control.standingApprover(chatInfoOf(chat, "@4"), "requests")).toBe("Alice")
    chat.members.set(10, [self, {...alice, memberRole: T.GroupMemberRole.Admin}, bob])
    await control.load()
    expect(control.standingApprover(chatInfoOf(chat, "@4"), "requests")).toBeUndefined()
    chat.members.set(10, [self, bob])
    await control.load()
    expect(control.standingApprover(chatInfoOf(chat, "@4"), "requests")).toBeUndefined()
    chat.members.set(10, [self, alice, {...bob, memberRole: T.GroupMemberRole.Owner}])
    await control.load()
    expect(control.standingApprover(chatInfoOf(chat, "@4"), "requests")).toBe("Alice")
  })
})

describe("chat list in the Main group", () => {
  it("lists the agent's chats with their approvals and connection state", async () => {
    const {control, chat} = await setup()
    chat.chats.set("@6", {chatInfo: direct({...contact(6, "Ask SimpleX Team"), activeConn: undefined}), items: []})
    chat.chats.set("#7", {chatInfo: groupChat(group(7, "/approve 9", member(1, "agent"))), items: []})
    await control.ownerCommand(alice, {keyword: "approve", params: "1:@4 all"})
    expect((await control.ownerCommand(alice, {keyword: "list", params: ""}))!.split("\n")).toEqual([
      sessionLine,
      "1:@4  bot SimpleX Directory, approved for this chat by Alice",
      "1:#2  business ep",
      "1:#10  group agent, not connected",
      "1:@6  contact Ask SimpleX Team, not connected",
      "1:#7  group `/approve 9`",
    ])
  })

  it("shows only the session line of a session without chats", async () => {
    const {control} = await controlWith(new FakeChat(), actionsWith(fakeHooks()))
    expect(await control.ownerCommand(alice, {keyword: "list", params: ""})).toBe(sessionLine)
  })

  it("lists the 100 most recent chats, each line at most 100 bytes, and counts the older ones", async () => {
    const {control, chat} = await setup()
    chat.chats.set("#7", {chatInfo: groupChat(group(7, "😀".repeat(30), member(1, "agent"))), items: []})
    for (let i = 100; i < 200; i++) chat.chats.set(`@${i}`, {chatInfo: direct(contact(i, `contact ${i}`)), items: []})
    const lines = (await control.ownerCommand(alice, {keyword: "list", params: ""}))!.split("\n")
    expect(lines).toHaveLength(102)
    expect(lines.slice(0, 6)).toEqual([sessionLine, "1:@4  bot SimpleX Directory", "1:#2  business ep", "1:#10  group agent, not connected", `1:#7  group ${"😀".repeat(21)}…`, "1:@100  contact contact 100"])
    expect(lines.every(line => Buffer.byteLength(line) <= 100)).toBe(true)
    expect(lines.at(-1)).toBe("4 older chats are not shown.")
    for (const ref of ["@197", "@198", "@199"]) chat.chats.delete(ref)
    expect((await control.ownerCommand(alice, {keyword: "list", params: ""}))!.split("\n").at(-1)).toBe("1 older chat is not shown.")
  })
})

describe("command menu", () => {
  const menu = [
    {type: "command", keyword: "pending", label: "Pending actions"},
    {type: "command", keyword: "list", label: "Chats and approvals"},
  ]

  it("is updated at load when it differs", async () => {
    const chat = agentChats()
    const actions = actionsWith(fakeHooks())
    const old = group(10, "agent", self, {groupProfile: {displayName: "agent", fullName: "", groupPreferences: {history: {enable: "on"}, commands: [{type: "command", keyword: "approve", label: "Approve", params: "N"}]}}})
    const tools = new AgentTools({chat: chat.api(), session: 1, user, actions, watch: new ChatItemsWatch(), mode: () => "requests", standingApprover: () => undefined})
    const sessions = sessionsOf({number: 1, displayName: "agent", mode: "requests", chat: chat.api(), tools})
    await new Control(chat.api(), old, {displayName: "agent"}, actions, sessions).load()
    expect(chat.profiles).toEqual([{groupId: 10, groupProfile: {displayName: "agent", fullName: "", groupPreferences: {history: {enable: "on"}, commands: menu}}}])
    const current = group(10, "agent", self, {groupProfile: {displayName: "agent", fullName: "", groupPreferences: {history: {enable: "on"}, commands: menu}}})
    await new Control(chat.api(), current, {displayName: "agent"}, actions, sessions).load()
    expect(chat.profiles).toHaveLength(1)
  })
})

describe("sends to a chat approved for all messages", () => {
  async function standing(approvers: T.GroupMember[], mode: Mode = "requests") {
    const chat = agentChats()
    chat.members.set(10, [self, ...approvers])
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {simplexMcp: {approvedBy: "Alice", approvedById: 3, approvedAt: "2026-10-18T10:00:00.000Z"}}
    const actions: Actions = new Actions(23, 1, {
      refusal: action => control.refusal(action),
      saveCounter: async () => undefined,
      postCard: action => control.postCard(action),
      outcome: action => control.outcome(action),
      perform: (action, signal) => tools.perform(action, signal),
    })
    const {control, tools} = await controlWith(chat, actions, 1, mode)
    return {chat, actions, control, tools}
  }

  const cards = (chat: FakeChat): string[] => chat.sent.filter(({chat: [, id]}) => id === 10).map(({messages: [{msgContent}]}) => msgContent.text)

  it("performs the send at once, without a card, and posts the outcome card", async () => {
    const {chat, tools} = await standing([alice])
    const result = await tools.call("send", {chat: "@4", text: "trail shoes", wait: 0}, signal().call)
    expect(result!.structuredContent).toMatchObject({action: 1, status: "done", chat: "@4", responses: [], more: 0})
    expect(chat.sent.filter(({chat: [, id]}) => id === 4).map(({messages: [{msgContent}]}) => msgContent.text)).toEqual(["trail shoes"])
    await settled()
    expect(cards(chat)).toEqual([`${sessionLine}\nI sent a message to bot SimpleX Directory:\n\`trail shoes\`\nApproved for this chat by Alice. /'revoke 1:@4'`])
  })

  it("posts a card when the approving member is no longer an approver", async () => {
    const {chat, actions, tools} = await standing([{...alice, memberRole: T.GroupMemberRole.Member}, member(8, "Carol", T.GroupMemberRole.Owner)])
    const {controller, call} = signal()
    const sending = tools.call("send", {chat: "@4", text: "trail shoes", wait: 0}, call)
    await settled()
    expect(actions.get(1)).toMatchObject({state: "pending", standing: false})
    expect(cards(chat)).toEqual([`${sessionLine}\nI want to send a message to bot SimpleX Directory:\n\`trail shoes\`\n/'approve 1:@4 1', /'approve 1:@4 all' or /'reject 1:@4 1'`])
    expect(chat.sent.filter(({chat: [, id]}) => id === 4)).toEqual([])
    controller.abort()
    expect(await sending).toBeUndefined()
  })

  it("does not cover leave", async () => {
    const {chat, actions, tools} = await standing([alice])
    const {controller, call} = signal()
    const leaving = tools.call("leave", {chat: "@4"}, call)
    await settled()
    expect(actions.get(1)).toMatchObject({tool: "leave", state: "pending", standing: false})
    expect(cards(chat)).toEqual([`${sessionLine}\nI want to delete bot SimpleX Directory (@4).\n/'approve 1:@4 1' or /'reject 1:@4 1'`])
    controller.abort()
    expect(await leaving).toBeUndefined()
  })

  it("applies in no chat in mode host", async () => {
    const {chat, control, tools} = await standing([alice], "host")
    expect(control.standingApprover(chatInfoOf(chat, "@4"), "host")).toBeUndefined()
    const {chats} = (await tools.call("list", {}, signal().call))!.structuredContent as {chats: {chat: string}[]}
    expect(chats.find(({chat: ref}) => ref === "@4")).not.toHaveProperty("approved")
    expect((await control.ownerCommand(alice, {keyword: "list", params: ""}))!.split("\n")[1]).toBe("1:@4  bot SimpleX Directory")
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "1:#2 all"})).toBe("Approvals apply in mode requests; session 1 runs in mode host.")
    expect(chat.customData).toEqual([])
    expect((await tools.call("send", {chat: "@4", text: "trail shoes", wait: 0}, signal().call))!.structuredContent).toMatchObject({action: 1, status: "done"})
    await settled()
    expect(cards(chat)).toEqual([`${sessionLine}\nI sent a message to bot SimpleX Directory:\n\`trail shoes\`\nPerformed without approval (mode host).`])
  })
})

describe("approval card size", () => {
  const tooLong = "text: too long for its approval card; shorten it."

  async function sendTools(approvers: T.GroupMember[] = [alice]) {
    const chat = agentChats()
    chat.members.set(10, [self, ...approvers, bob])
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {simplexMcp: {approvedBy: "Alice", approvedById: 3, approvedAt: "2026-10-18T10:00:00.000Z"}}
    const actions: Actions = new Actions(23, 1, {
      refusal: action => control.refusal(action),
      saveCounter: async () => undefined,
      postCard: action => control.postCard(action),
      outcome: action => control.outcome(action),
      perform: (action, signal) => tools.perform(action, signal),
    })
    const {control, tools} = await controlWith(chat, actions)
    const textOf = (result: CallToolResult | undefined): string => (result!.content[0] as {text: string}).text
    return {chat, actions, tools, textOf}
  }

  function connectFields(name: string, extra: {message?: string; description?: string; welcome?: string}): NewAction {
    return {
      tool: "connect",
      input: {target: "https://simplex.chat/a#ridgeline", ...(extra.message === undefined ? {} : {message: extra.message})},
      subject: {kind: "contact", name, incognito: false},
      plan: {cardKind: "contact", type: "contact", takesMessage: true, description: extra.description, welcome: extra.welcome},
      messages: new Collected(),
    }
  }

  const encodedLength = (text: string): number => Buffer.byteLength(JSON.stringify(text)) - 2

  const flushed = (): Promise<void> => new Promise(resolve => setImmediate(resolve))

  it("accepts a text whose largest card has 15000 bytes and refuses one byte more, counting multi-byte characters", async () => {
    const {chat, actions, tools, textOf} = await sendTools()
    const atLimit = "€".repeat(4360) + "a"
    expect(textOf(await tools.call("send", {chat: "#2", text: `${atLimit}a`}, signal().call))).toBe(tooLong)
    expect(actions.next).toBe(1)
    const {controller, call} = signal()
    const sending = tools.call("send", {chat: "#2", text: atLimit}, call)
    await flushed()
    expect(actions.get(1)).toMatchObject({tool: "send", state: "pending", card: expect.any(Number)})
    expect(chat.sent.map(({chat: [, id]}) => id)).toEqual([10])
    controller.abort()
    expect(await sending).toBeUndefined()
  })

  it("counts escaped characters and the standing approval line of a chat approved for all messages", async () => {
    const {chat, actions, tools, textOf} = await sendTools()
    const atLimit = "\b".repeat(2176) + "a"
    expect(textOf(await tools.call("send", {chat: "@4", text: `${atLimit}a`, wait: 0}, signal().call))).toBe(tooLong)
    expect(actions.next).toBe(1)
    expect((await tools.call("send", {chat: "@4", text: atLimit, wait: 0}, signal().call))!.structuredContent).toMatchObject({action: 1, status: "done"})
    await flushed()
    expect(chat.sent.map(({chat: [, id]}) => id)).toEqual([4, 10])
  })

  it("measures the card with the longest name of the current approvers", async () => {
    const {actions, tools, textOf} = await sendTools([alice, member(8, "😀".repeat(50), T.GroupMemberRole.Owner)])
    expect(textOf(await tools.call("send", {chat: "#2", text: "€".repeat(4360) + "a"}, signal().call))).toBe(tooLong)
    const {controller, call} = signal()
    const sending = tools.call("send", {chat: "#2", text: "€".repeat(4295) + "a"}, call)
    await flushed()
    expect(actions.get(1)).toMatchObject({state: "pending"})
    controller.abort()
    expect(await sending).toBeUndefined()
  })

  it("counts the lines of the text, which the card quotes one by one", async () => {
    const {actions, tools, textOf} = await sendTools()
    expect(textOf(await tools.call("send", {chat: "#2", text: "a\n".repeat(3000)}, signal().call))).toBe(tooLong)
    const {controller, call} = signal()
    const sending = tools.call("send", {chat: "#2", text: "a\n".repeat(2000)}, call)
    await flushed()
    expect(actions.get(1)).toMatchObject({state: "pending"})
    controller.abort()
    expect(await sending).toBeUndefined()
  })

  it("refuses a connect whose message makes its largest card longer than 15000 bytes", async () => {
    const {chat, actions} = await sendTools()
    const name = "😀".repeat(1000)
    expect(() => actions.create(1, "requests", "c", connectFields(name, {message: "\b".repeat(2000), welcome: "Welcome!"}))).toThrow(new ToolError("message: too long for its approval card; shorten it."))
    expect(actions.next).toBe(1)
    actions.create(1, "requests", "c", connectFields(name, {message: "\b".repeat(1400), welcome: "Welcome!"}))
    await flushed()
    expect(chat.sent.map(({chat: [, id]}) => id)).toEqual([10])
  })

  it("cuts the link texts on a connect card to fit 15000 bytes in its largest form, and keeps them whole in the action", async () => {
    const {chat, actions} = await sendTools()
    const description = "Trail shoes. ".repeat(1500)
    const action = actions.create(1, "requests", "c", connectFields("Ridgeline", {description, welcome: "Welcome!"}))
    await flushed()
    const [card] = chat.sent.filter(({chat: [, id]}) => id === 10).map(({messages: [{msgContent}]}) => msgContent.text.split("\n"))
    expect(card).toHaveLength(4)
    expect(card[0]).toBe(sessionLine)
    expect(card[2]).toMatch(/^`Trail shoes\. .*…`$/)
    const failed = encodedLength(`${sessionLine}\n${cardText({...action, state: "failed", decidedBy: "Alice", error: "€".repeat(600)})}`)
    expect(failed).toBeLessThanOrEqual(15000)
    expect(failed).toBeGreaterThan(15000 - 8)
    expect(action.tool === "connect" && action.plan.description).toBe(description)
  })
})

describe("sessions in the Main group", () => {
  it("starts cards with the session line and names the session in commands", async () => {
    const {chat, control, action} = await setup()
    await control.postCard(action)
    const card = chat.sent.at(-1)!.messages[0].msgContent.text.split("\n")
    expect(card[0]).toBe("Session 1 · SimpleX MCP - simplex-chat-6")
    expect(card.at(-1)).toBe("/'approve 1:@4 5', /'approve 1:@4 all' or /'reject 1:@4 5'")
  })

  it("starts edited cards with the session line, and names a closed session by its number", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake, 5)
    const chat = agentChats()
    const {control} = await controlWith(chat, actions, 2)
    const open = actions.create(2, "requests", "k2", fields())
    const closed = actions.create(3, "requests", "k3", fields())
    await settled()
    actions.reject(open, "Alice")
    await control.outcome(open)
    await control.outcome({...closed, card: 900})
    expect(chat.updates.map(({text}) => text.split("\n")[0])).toEqual(["Session 2 · SimpleX MCP - simplex-chat-6", "Session 3"])
    expect(chat.updates[0].text.split("\n").at(-1)).toBe("Rejected by Alice.")
  })

  it("quotes a session name that is not a valid display name", async () => {
    const chat = agentChats()
    const actions = actionsWith(fakeHooks(), 5)
    const tools = new AgentTools({chat: chat.api(), user, actions, watch: new ChatItemsWatch(), session: 1, mode: () => "requests", standingApprover: () => undefined})
    const control = new Control(chat.api(), group(10, "SimpleX MCP"), {displayName: "SimpleX MCP"}, actions, sessionsOf({number: 1, displayName: "SimpleX MCP - /approve 9\nnow", mode: "requests", chat: chat.api(), tools}))
    await control.load()
    await control.postCard(actions.create(1, "requests", "k", fields()))
    expect(chat.sent.at(-1)!.messages[0].msgContent.text.split("\n")[0]).toBe("Session 1 · `SimpleX MCP - /approve 9 now`")
    expect((await control.ownerCommand(alice, parseCommand("/list")!))!.split("\n")[0]).toBe("Session 1 · `SimpleX MCP - /approve 9 now`")
  })

  it("approves all messages to a chat of one session through that session's database", async () => {
    const fake = fakeHooks()
    const actions = actionsWith(fake, 5)
    const hubChat = agentChats()
    const one = agentChats()
    const two = agentChats()
    const tool = (chat: FakeChat, session: number) => new AgentTools({chat: chat.api(), user, actions, watch: new ChatItemsWatch(), session, mode: () => "requests", standingApprover: () => undefined})
    const control = new Control(hubChat.api(), group(10, "SimpleX MCP"), {displayName: "SimpleX MCP"}, actions, sessionsOf(
      {number: 1, displayName: "SimpleX MCP - one", mode: "requests", chat: one.api(), tools: tool(one, 1)},
      {number: 2, displayName: "SimpleX MCP - two", mode: "requests", chat: two.api(), tools: tool(two, 2)},
    ))
    await control.load()
    const first = actions.create(1, "requests", "k1", fields())
    const second = actions.create(2, "requests", "k2", fields())
    await settled()
    expect(await control.ownerCommand(alice, parseCommand("/approve 2:@4 all")!)).toBe("Approved for bot SimpleX Directory (2:@4) by Alice. /'revoke 2:@4'")
    expect(two.customData.at(-1)).toMatchObject({chat: "@4", customData: {simplexMcp: {approvedBy: "Alice", approvedById: 3}}})
    expect(one.customData).toEqual([])
    expect(hubChat.customData.filter(({chat}) => chat === "@4")).toEqual([])
    expect(second.state).toBe("performing")
    expect(first.state).toBe("pending")
    expect(await control.ownerCommand(alice, parseCommand("/approve 3:@4 all")!)).toBe("No open session 3. /list shows the open sessions.")
    expect(await control.ownerCommand(alice, parseCommand("/approve 2:@9 all")!)).toBe("No chat 2:@9. /list shows the agents' chats.")
    expect(await control.ownerCommand(alice, parseCommand("/revoke 2:@4")!)).toBe("Messages to bot SimpleX Directory (2:@4) need approval again.")
    expect(two.customData.at(-1)).toEqual({chat: "@4", customData: {}})
    expect(one.customData).toEqual([])
  })

  it("applies the approval mode of each session to its own actions and cards", async () => {
    const hubChat = agentChats()
    const one = agentChats()
    const two = agentChats()
    const views: SessionView[] = [
      {number: 1, displayName: "SimpleX MCP - one", mode: "host", chat: one.api(), tools: undefined as never},
      {number: 2, displayName: "SimpleX MCP - two", mode: "requests", chat: two.api(), tools: undefined as never},
    ]
    const actions: Actions = new Actions(23, 1, {
      refusal: action => control.refusal(action),
      saveCounter: async () => undefined,
      postCard: action => control.postCard(action),
      outcome: action => control.outcome(action),
      perform: (action, signal) => tools[action.session - 1].perform(action, signal),
    })
    const tools = views.map(view => new AgentTools({chat: view.chat, user, actions, watch: new ChatItemsWatch(), session: view.number, mode: () => view.mode, standingApprover: chatInfo => control.standingApprover(chatInfo, view.mode)}))
    views.forEach((view, i) => (view.tools = tools[i]))
    const control: Control = new Control(hubChat.api(), group(10, "SimpleX MCP"), {displayName: "SimpleX MCP"}, actions, sessionsOf(...views))
    await control.load()
    const cards = (): string[] => hubChat.sent.map(({messages: [{msgContent}]}) => msgContent.text)
    const sent = (chat: FakeChat): string[] => chat.sent.map(({messages: [{msgContent}]}) => msgContent.text)

    expect((await tools[0].call("send", {chat: "@4", text: "from one", wait: 0}, signal().call))!.structuredContent).toMatchObject({action: 1, status: "done"})
    const sending = tools[1].call("send", {chat: "@4", text: "from two", wait: 0}, signal().call)
    await settled()
    expect(sent(one)).toEqual(["from one"])
    expect(sent(two)).toEqual([])
    expect(cards()).toEqual([
      "Session 1 · SimpleX MCP - one\nI sent a message to bot SimpleX Directory:\n`from one`\nPerformed without approval (mode host).",
      "Session 2 · SimpleX MCP - two\nI want to send a message to bot SimpleX Directory:\n`from two`\n/'approve 2:@4 2', /'approve 2:@4 all' or /'reject 2:@4 2'",
    ])
    expect(await control.ownerCommand(alice, parseCommand("/approve 1:@4 all")!)).toBe("Approvals apply in mode requests; session 1 runs in mode host.")
    expect(one.customData).toEqual([])
    await control.ownerCommand(alice, parseCommand("/approve 2:@4 2")!)
    expect((await sending)!.structuredContent).toMatchObject({action: 2, status: "done"})
    await settled()
    expect(sent(two)).toEqual(["from two"])
    expect(hubChat.updates.map(({text}) => text.split("\n").at(-1))).toEqual(["Approved by Alice."])
  })

  it("lists chats and pending actions grouped by session", async () => {
    const {control} = await setup()
    expect(await control.ownerCommand(alice, parseCommand("/list")!)).toBe(
      ["Session 1 · SimpleX MCP - simplex-chat-6", "1:@4  bot SimpleX Directory", "1:#2  business ep", "1:#10  group agent, not connected"].join("\n"),
    )
    expect(await control.ownerCommand(alice, parseCommand("/pending")!)).toMatch(/^5  session 1: send to bot SimpleX Directory  expires \d{4}-\d\d-\d\d \d\d:\d\d UTC$/)
  })

  it("lists the chats of every open session under its session line", async () => {
    const actions = actionsWith(fakeHooks())
    const one = agentChats()
    const two = new FakeChat()
    two.chats.set("@8", {chatInfo: direct(contact(8, "Ana Lu")), items: []})
    const tool = (chat: FakeChat, session: number) => new AgentTools({chat: chat.api(), user, actions, watch: new ChatItemsWatch(), session, mode: () => "requests", standingApprover: () => undefined})
    const control = new Control(one.api(), group(10, "SimpleX MCP"), {displayName: "SimpleX MCP"}, actions, sessionsOf(
      {number: 1, displayName: "SimpleX MCP - one", mode: "requests", chat: one.api(), tools: tool(one, 1)},
      {number: 4, displayName: "SimpleX MCP - two", mode: "requests", chat: two.api(), tools: tool(two, 4)},
    ))
    await control.load()
    expect((await control.ownerCommand(alice, parseCommand("/list")!))!.split("\n")).toEqual([
      "Session 1 · SimpleX MCP - one",
      "1:@4  bot SimpleX Directory",
      "1:#2  business ep",
      "1:#10  group agent, not connected",
      "Session 4 · SimpleX MCP - two",
      "4:@8  contact Ana Lu",
    ])
  })

  it("says no session is open", async () => {
    const actions = actionsWith(fakeHooks())
    const control = new Control(agentChats().api(), group(10, "SimpleX MCP"), {displayName: "SimpleX MCP"}, actions, sessionsOf())
    await control.load()
    expect(await control.ownerCommand(alice, parseCommand("/list")!)).toBe("No open sessions.")
  })

  it("puts the link into the refusal while the Main group has no owner", async () => {
    const chat = agentChats()
    chat.members.set(10, [self])
    const {control} = await controlWith(chat, actionsWith(fakeHooks()))
    const action = {...fields(), n: 1, session: 1, state: "pending", standing: false} as unknown as Action
    expect(control.link).toBeUndefined()
    expect(control.refusal(action)).toBe("The agent has no owner. Run /simplex:link in Claude Code.")
    await control.start()
    expect(control.link).toBe("https://simplex.chat/g#link")
    expect(control.refusal(action)).toBe("The agent has no owner. Open this link in the SimpleX app and join: https://simplex.chat/g#link")
  })

  it("opens no link at start while the Main group has an owner", async () => {
    const {control, chat} = await setup()
    await control.start()
    expect(control.link).toBeUndefined()
    expect(chat.links).toEqual([])
  })

  it("makes the first person who joins the owner and deletes the link", async () => {
    const chat = agentChats()
    chat.members.set(10, [self])
    const {control} = await controlWith(chat, actionsWith(fakeHooks()))
    await control.start()
    const carol = member(9, "Carol", T.GroupMemberRole.Member)
    chat.members.set(10, [self, carol])
    chat.roles = []
    await control.onEvent({type: "joinedGroupMember", user, groupInfo: group(10, "SimpleX MCP"), member: carol} as unknown as ChatEvent)
    expect(chat.roles).toEqual([{groupId: 10, members: [9], role: T.GroupMemberRole.Owner}])
    expect(control.link).toBeUndefined()
  })

  it("makes only the first of two people who join in a row the owner", async () => {
    const chat = agentChats()
    chat.members.set(10, [self])
    const {control} = await controlWith(chat, actionsWith(fakeHooks()))
    await control.start()
    const carol = member(9, "Carol", T.GroupMemberRole.Member)
    const dave = member(11, "Dave", T.GroupMemberRole.Member)
    chat.members.set(10, [self, carol, dave])
    const joined = (joiner: T.GroupMember) => control.onEvent({type: "joinedGroupMember", user, groupInfo: group(10, "SimpleX MCP"), member: joiner} as unknown as ChatEvent)
    await Promise.all([joined(carol), joined(dave)])
    expect(chat.roles).toEqual([{groupId: 10, members: [9], role: T.GroupMemberRole.Owner}])
    expect(control.owners()).toEqual(["Carol"])
  })

  it("opens a new link when the last owner leaves", async () => {
    const {control, chat} = await setup()
    chat.members.set(10, [self, bob])
    await control.onEvent({type: "leftMember", user, groupInfo: group(10, "SimpleX MCP"), member: alice} as unknown as ChatEvent)
    expect(control.owners()).toEqual([])
    expect(control.link).toBe("https://simplex.chat/g#link")
    expect(chat.links).toEqual([{groupId: 10, role: T.GroupMemberRole.Member}])
  })

  it("names the owners on one line", async () => {
    const {control, chat} = await setup()
    chat.members.set(10, [self, member(3, "Ana\nLu", T.GroupMemberRole.Owner), bob, member(8, "Carol", T.GroupMemberRole.Owner, T.GroupMemberStatus.Left)])
    await control.load()
    expect(control.owners()).toEqual(["Ana Lu"])
  })

  it("renames the Main group", async () => {
    const {control, chat} = await setup()
    chat.profiles = []
    await control.rename("Agents")
    expect(chat.profiles.map(({groupProfile}) => groupProfile.displayName)).toEqual(["Agents"])
  })

  it("resets the Main group: fails pending actions and opens a new link", async () => {
    const {actions, chat, control, action} = await setup()
    const link = await control.reset(user.userId)
    expect(action.state).toBe("failed")
    expect(actions.lost).toBe(false)
    expect(link).toBe("https://simplex.chat/g#link")
    expect(chat.newGroups).toHaveLength(1)
    expect(chat.deleted).toEqual([{chatType: T.ChatType.Group, chatId: 10, mode: {type: "full", notify: true}}])
    expect(control.groupId).toBe(20)
    expect(chat.links).toEqual([{groupId: 20, role: T.GroupMemberRole.Member}])
  })
})
