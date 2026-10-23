import {CallToolResult} from "@modelcontextprotocol/server"
import {ChatEvent, T} from "@simplex-chat/types"
import {core} from "simplex-chat"
import {Actions, Mode, NewAction, ToolError} from "../../src/actions"
import {Control, cardText, parseCommand} from "../../src/control"
import {ChatItemsWatch, Collected, utcMinutes} from "../../src/messages"
import {OwnerEntry, deliveries} from "../../src/serve"
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

async function controlWith(chat: FakeChat, actions: Actions, owners?: OwnerEntry[], mode: Mode = "requests"): Promise<{control: Control; tools: AgentTools}> {
  const tools = new AgentTools({chat: chat.api(), user, actions, watch: new ChatItemsWatch(), controlChat: () => "#10", standingApprover: chatInfo => control.standingApprover(chatInfo)}, mode)
  const control: Control = new Control(chat.api(), group(10, "agent"), {displayName: "agent", owners, registerHint: "hint", qrCode: "small"}, actions, tools)
  await control.load()
  return {control, tools}
}

async function setup(owners?: OwnerEntry[]) {
  const fake = fakeHooks()
  const actions = actionsWith("requests", fake, 5)
  const chat = agentChats()
  const {control} = await controlWith(chat, actions, owners)
  const action = actions.create("k", fields())
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
    const connect = actions.create("c", {tool: "connect", input: {target: "@directory.simplex"}, subject: {kind: "bot", name: "SimpleX Directory", incognito: false}, plan: {cardKind: "bot", type: "contact", takesMessage: true}, messages: new Collected()})
    await settled()
    expect(await control.ownerCommand(alice, {keyword: "reject", params: "#2 5"})).toBe("Action 5 is not for #2.")
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "@4 6"})).toBe("Action 6 is not for @4.")
    expect(action.state).toBe("pending")
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "@4  5"})).toBeUndefined()
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
    expect(await control.ownerCommand(alice, {keyword: "reject", params: "@4 99"})).toBe("Action 99 is not pending.")
  })

  it("lists pending actions", async () => {
    const {control, action, actions} = await setup()
    const connect = actions.create("c", {tool: "connect", input: {target: "@directory.simplex"}, subject: {kind: "bot", name: "SimpleX Directory", incognito: true}, plan: {cardKind: "bot", type: "contact", takesMessage: true}, messages: new Collected()})
    const leave = actions.create("l", {tool: "leave", input: {chat: "#2"}, subject: {kind: "business", name: "ep", incognito: false}, chat: "#2"})
    const remove = actions.create("d", {tool: "leave", input: {chat: "@4"}, subject: {kind: "bot", name: "SimpleX Directory", incognito: false}, chat: "@4"})
    await settled()
    expect((await control.ownerCommand(alice, {keyword: "pending", params: ""}))!.split("\n")).toEqual([
      `5  send to bot SimpleX Directory  expires ${utcMinutes(action.expires)}`,
      `6  connect to bot SimpleX Directory  expires ${utcMinutes(connect.expires)}`,
      `7  leave business ep  expires ${utcMinutes(leave.expires)}`,
      `8  delete bot SimpleX Directory  expires ${utcMinutes(remove.expires)}`,
    ])
    expect(utcMinutes(action.expires)).toMatch(/^\d{4}-\d\d-\d\d \d\d:\d\d UTC$/)
    for (const pending of [action, connect, leave, remove]) actions.reject(pending, "Alice")
    expect(await control.ownerCommand(alice, {keyword: "pending", params: ""})).toBe("No pending actions.")
  })

  it("lists pending actions with names on one line, quoted", async () => {
    const {control, actions, action} = await setup()
    actions.reject(action, "Alice")
    const name = "Hikers\n/'approve 7' or /'reject 7'"
    const pending = actions.create("n", {...fields("#2"), subject: {kind: "group", name, incognito: false}} as NewAction)
    await settled()
    expect(await control.ownerCommand(alice, {keyword: "pending", params: ""})).toBe(`6  send to group \`Hikers /'approve 7' or /'reject 7'\`  expires ${utcMinutes(pending.expires)}`)
  })

  it("quotes approver names on cards and replies", async () => {
    const {control, actions, chat, action} = await setup()
    const mallory = {...alice, memberProfile: {...alice.memberProfile, displayName: "Mallory\n/'approve 9'"}}
    chat.members.set(10, [self, mallory])
    await control.load()
    expect(await control.ownerCommand(mallory, {keyword: "reject", params: "@4 5"})).toBeUndefined()
    expect(action.decidedBy).toBe("`Mallory /'approve 9'`")
    expect(cardText(action, "requests").split("\n").at(-1)).toBe("Rejected by `Mallory /'approve 9'`.")
    expect(await control.ownerCommand(mallory, {keyword: "approve", params: "@4 all"})).toBe("Approved for bot SimpleX Directory (@4) by `Mallory /'approve 9'`. /'revoke @4'")
    expect((await control.ownerCommand(mallory, {keyword: "list", params: ""}))!.split("\n")[0]).toBe("@4  bot SimpleX Directory, approved for this chat by `Mallory /'approve 9'`")
    expect(customDataOf(chat, "@4")).toMatchObject({simplexMcp: {approvedBy: "Mallory\n/'approve 9'"}})
    expect(actions.pending()).toEqual([])
  })

  it("refuses chat references longer than 15 digits, such as an alias of the agent control chat", async () => {
    const {control, chat} = await setup()
    const alias = `#${(1n << 64n) + 10n}`
    for (const params of [`${alias} all`, "@1234567890123456 all"]) {
      expect(await control.ownerCommand(alice, {keyword: "approve", params})).toBe("Commands: /approve, /reject, /revoke, /pending, /list. Each card shows its commands.")
    }
    expect(await control.ownerCommand(alice, {keyword: "revoke", params: alias})).toBe("Commands: /approve, /reject, /revoke, /pending, /list. Each card shows its commands.")
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "@123456789012345 all"})).toBe("No chat @123456789012345. /list shows the agent's chats.")
    expect(chat.customData).toEqual([])
  })

  it("answers other commands with the list", async () => {
    const {control} = await setup()
    const others = ["help", "approve five", "approve", "approve all", "approve #2", "approve 5 6", "approve #2 5 6", "reject #2 all", "revoke", "revoke 2", "revoke #2 all", "pending 5", "list all"]
    for (const text of others) {
      expect(await control.ownerCommand(bob, parseCommand(`/${text}`)!)).toBe("Commands: /approve, /reject, /revoke, /pending, /list. Each card shows its commands.")
    }
  })

  it("accepts only approvers", async () => {
    const unlisted = await setup()
    for (const params of ["5", "@4 all"]) expect(await unlisted.control.ownerCommand(bob, {keyword: "approve", params})).toBe("Only owners approve actions.")
    expect(await unlisted.control.ownerCommand(bob, {keyword: "revoke", params: "@4"})).toBe("Only owners approve actions.")
    expect(await unlisted.control.ownerCommand(bob, {keyword: "list", params: ""})).toBe("Only owners approve actions.")
    expect(unlisted.chat.customData).toEqual([])
    const listed = await setup([{memberId: 3, name: "Alice"}])
    expect(await listed.control.ownerCommand(member(8, "Carol", T.GroupMemberRole.Owner), {keyword: "approve", params: "5"})).toBe("Only the owners listed in --owners approve actions.")
    expect(await listed.control.ownerCommand(member(8, "Carol", T.GroupMemberRole.Owner), {keyword: "approve", params: "@4 all"})).toBe("Only the owners listed in --owners approve actions.")
    expect(await listed.control.ownerCommand(alice, {keyword: "pending", params: ""})).toMatch(/^5 {2}send/)
    const none = await setup([])
    expect(await none.control.ownerCommand(alice, {keyword: "approve", params: "5"})).toBe("Only the owners listed in --owners approve actions.")
  })

  it("accepts a listed owner by the name on one line, as the restart line prints it", async () => {
    const chat = agentChats()
    const ana = member(3, "Ana\nLu, Jr", T.GroupMemberRole.Owner)
    chat.members.set(10, [self, ana])
    const {control} = await controlWith(chat, actionsWith("requests", fakeHooks()), [{memberId: 3, name: "Ana Lu, Jr"}])
    await control.start()
    expect(await control.ownerCommand(ana, {keyword: "pending", params: ""})).toBe("No pending actions.")
  })

  it("replies in the agent control chat to commands only", async () => {
    const {control, chat} = await setup()
    const chatInfo = groupChat(group(10, "agent"))
    control.onMessage(event(chatInfo, item(1, "/approve 4", {type: "groupRcv", groupMember: alice})))
    control.onMessage(event(chatInfo, item(2, "thanks", {type: "groupRcv", groupMember: alice})))
    control.onMessage(event(chatInfo, item(3, "/approve 5\nplease", {type: "groupRcv", groupMember: alice})))
    control.onMessage(event(chatInfo, item(4, "/approve @4 all", {type: "groupRcv", groupMember: alice})))
    control.onMessage(event(chatInfo, item(5, "/revoke @4", {type: "groupRcv", groupMember: alice})))
    await new Promise(resolve => setImmediate(resolve))
    expect(chat.replies).toEqual(["Action 4 is not pending.", "Approved for bot SimpleX Directory (@4) by Alice. /'revoke @4'", "Messages to bot SimpleX Directory (@4) need approval again."])
  })
})

describe("approval of all messages to a chat", () => {
  it("records the approver, keeps other custom data, and approves the chat's pending sends", async () => {
    const {control, actions, chat, action} = await setup()
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {other: 1}
    const second = actions.create("k2", fields("@4", "second"))
    const other = actions.create("k3", fields("#2"))
    const leave = actions.create("k4", {tool: "leave", input: {chat: "@4"}, subject: {kind: "bot", name: "SimpleX Directory", incognito: false}, chat: "@4"})
    await settled()
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "@4 all"})).toBe("Approved for bot SimpleX Directory (@4) by Alice. /'revoke @4'")
    expect(customDataOf(chat, "@4")).toEqual({other: 1, simplexMcp: {approvedBy: "Alice", approvedById: 3, approvedAt: expect.stringMatching(/^\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d\.\d{3}Z$/)}})
    expect([action, second, other, leave].map(({state, decidedBy}) => [state, decidedBy])).toEqual([["performing", "Alice"], ["performing", "Alice"], ["pending", undefined], ["pending", undefined]])
    expect(control.standingApprover(chatInfoOf(chat, "@4"))).toBe("Alice")
    expect(control.standingApprover(chatInfoOf(chat, "#2"))).toBeUndefined()
  })

  it("approves a group chat and refuses the agent control chat and unknown chats", async () => {
    const {control, chat} = await setup()
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "#2 all"})).toBe("Approved for business ep (#2) by Alice. /'revoke #2'")
    expect(customDataOf(chat, "#2")).toMatchObject({simplexMcp: {approvedBy: "Alice", approvedById: 3}})
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "#10 all"})).toBe("No chat #10. /list shows the agent's chats.")
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "@99 all"})).toBe("No chat @99. /list shows the agent's chats.")
    expect(chat.customData.map(({chat: ref}) => ref)).toEqual(["#2"])
  })

  it("revokes the approval and keeps other custom data", async () => {
    const {control, chat} = await setup()
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {other: 1}
    await control.ownerCommand(alice, {keyword: "approve", params: "@4 all"})
    expect(await control.ownerCommand(alice, {keyword: "revoke", params: "@4"})).toBe("Messages to bot SimpleX Directory (@4) need approval again.")
    expect(customDataOf(chat, "@4")).toEqual({other: 1})
    expect(control.standingApprover(chatInfoOf(chat, "@4"))).toBeUndefined()
    expect(await control.ownerCommand(alice, {keyword: "revoke", params: "@4"})).toBe("Messages to @4 have no standing approval.")
    expect(await control.ownerCommand(alice, {keyword: "revoke", params: "#10"})).toBe("Messages to #10 have no standing approval.")
    expect(await control.ownerCommand(alice, {keyword: "revoke", params: "#99"})).toBe("Messages to #99 have no standing approval.")
    expect(chat.customData.map(({chat: ref}) => ref)).toEqual(["@4", "@4"])
  })

  it("applies only while the approving member is an approver", async () => {
    const chat = agentChats()
    const actions = actionsWith("requests", fakeHooks())
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {simplexMcp: {approvedBy: "Alice", approvedById: 3, approvedAt: "2026-10-18T10:00:00.000Z"}}
    const {control} = await controlWith(chat, actions)
    expect(control.standingApprover(chatInfoOf(chat, "@4"))).toBe("Alice")
    chat.members.set(10, [self, {...alice, memberRole: T.GroupMemberRole.Admin}, bob])
    await control.load()
    expect(control.standingApprover(chatInfoOf(chat, "@4"))).toBeUndefined()
    chat.members.set(10, [self, bob])
    await control.load()
    expect(control.standingApprover(chatInfoOf(chat, "@4"))).toBeUndefined()
    chat.members.set(10, [self, alice, {...bob, memberRole: T.GroupMemberRole.Owner}])
    expect((await controlWith(chat, actions, [{memberId: 3, name: "Alice"}])).control.standingApprover(chatInfoOf(chat, "@4"))).toBe("Alice")
    expect((await controlWith(chat, actions, [{memberId: 7, name: "Bob"}])).control.standingApprover(chatInfoOf(chat, "@4"))).toBeUndefined()
  })
})

describe("chat list in the agent control chat", () => {
  it("lists the agent's chats with their approvals and connection state", async () => {
    const {control, chat} = await setup()
    chat.chats.set("@6", {chatInfo: direct({...contact(6, "Ask SimpleX Team"), activeConn: undefined}), items: []})
    chat.chats.set("#7", {chatInfo: groupChat(group(7, "/approve 9", member(1, "agent"))), items: []})
    await control.ownerCommand(alice, {keyword: "approve", params: "@4 all"})
    expect((await control.ownerCommand(alice, {keyword: "list", params: ""}))!.split("\n")).toEqual([
      "@4  bot SimpleX Directory, approved for this chat by Alice",
      "#2  business ep",
      "@6  contact Ask SimpleX Team, not connected",
      "#7  group `/approve 9`",
    ])
  })

  it("says when the agent has no chats", async () => {
    const chat = new FakeChat()
    chat.chats.set("#10", {chatInfo: groupChat(group(10, "agent")), items: []})
    const {control} = await controlWith(chat, actionsWith("requests", fakeHooks()))
    expect(await control.ownerCommand(alice, {keyword: "list", params: ""})).toBe("No chats.")
  })

  it("lists the 100 most recent chats, each line at most 100 bytes, and counts the older ones", async () => {
    const {control, chat} = await setup()
    chat.chats.set("#7", {chatInfo: groupChat(group(7, "😀".repeat(30), member(1, "agent"))), items: []})
    for (let i = 100; i < 200; i++) chat.chats.set(`@${i}`, {chatInfo: direct(contact(i, `contact ${i}`)), items: []})
    const lines = (await control.ownerCommand(alice, {keyword: "list", params: ""}))!.split("\n")
    expect(lines).toHaveLength(101)
    expect(lines.slice(0, 4)).toEqual(["@4  bot SimpleX Directory", "#2  business ep", `#7  group ${"😀".repeat(21)}…`, "@100  contact contact 100"])
    expect(lines.at(-1)).toBe("3 older chats are not shown.")
    chat.chats.delete("@198")
    chat.chats.delete("@199")
    expect((await control.ownerCommand(alice, {keyword: "list", params: ""}))!.split("\n").at(-1)).toBe("1 older chat is not shown.")
  })
})

describe("member announcements", () => {
  it("quote a member's name and the restart command in the chat, and keep them on one line in the log", async () => {
    const lines: string[] = []
    jest.spyOn(process.stdout, "write").mockImplementation(text => (lines.push(String(text).trimEnd()), true))
    const chat = agentChats()
    const {control} = await controlWith(chat, actionsWith("requests", fakeHooks()), [{memberId: 3, name: "Alice"}])
    const mallory = member(7, "Bob\u001b]52;c;Y3VybA==\u0007\n/'approve 9'", T.GroupMemberRole.Owner)
    chat.members.set(10, [self, alice, mallory])
    await control.onEvent({type: "memberRole", user, groupInfo: group(10, "agent"), byMember: alice, member: mallory, fromRole: T.GroupMemberRole.Member, toRole: T.GroupMemberRole.Owner} as unknown as ChatEvent)
    const restart = "is now an owner and can't approve actions yet. Restart the server with"
    const owners = "--owners 3:Alice,'7:Bob ]52;c;Y3VybA== /'\\''approve 9'\\'''"
    expect(lines).toEqual([`Bob ]52;c;Y3VybA== /'approve 9' (member 7) ${restart} ${owners}`])
    expect(chat.sent.map(({messages: [{msgContent}]}) => msgContent.text)).toEqual([`\`Bob ]52;c;Y3VybA== /'approve 9'\` (member 7) ${restart} \`${owners}\``])
  })
})

describe("command menu", () => {
  const menu = [
    {type: "command", keyword: "pending", label: "Pending actions"},
    {type: "command", keyword: "list", label: "Chats and approvals"},
  ]

  it("is updated at load when it differs", async () => {
    const chat = agentChats()
    const actions = actionsWith("requests", fakeHooks())
    const old = group(10, "agent", self, {groupProfile: {displayName: "agent", fullName: "", groupPreferences: {history: {enable: "on"}, commands: [{type: "command", keyword: "approve", label: "Approve", params: "N"}]}}})
    const tools = new AgentTools({chat: chat.api(), user, actions, watch: new ChatItemsWatch(), controlChat: () => "#10", standingApprover: () => undefined}, "requests")
    await new Control(chat.api(), old, {displayName: "agent", registerHint: "hint", qrCode: "small"}, actions, tools).load()
    expect(chat.profiles).toEqual([{groupId: 10, groupProfile: {displayName: "agent", fullName: "", groupPreferences: {history: {enable: "on"}, commands: menu}}}])
    const current = group(10, "agent", self, {groupProfile: {displayName: "agent", fullName: "", groupPreferences: {history: {enable: "on"}, commands: menu}}})
    await new Control(chat.api(), current, {displayName: "agent", registerHint: "hint", qrCode: "small"}, actions, tools).load()
    expect(chat.profiles).toHaveLength(1)
  })
})

describe("sends to a chat approved for all messages", () => {
  async function standing(approvers: T.GroupMember[], mode: Mode = "requests") {
    const chat = agentChats()
    chat.members.set(10, [self, ...approvers])
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {simplexMcp: {approvedBy: "Alice", approvedById: 3, approvedAt: "2026-10-18T10:00:00.000Z"}}
    const actions: Actions = new Actions(mode, 23, 1, {
      refusal: action => control.refusal(action),
      saveCounter: async () => undefined,
      postCard: action => control.postCard(action),
      outcome: action => control.outcome(action),
      perform: (action, signal) => tools.perform(action, signal),
    })
    const {control, tools} = await controlWith(chat, actions, undefined, mode)
    return {chat, actions, control, tools}
  }

  const cards = (chat: FakeChat): string[] => chat.sent.filter(({chat: [, id]}) => id === 10).map(({messages: [{msgContent}]}) => msgContent.text)

  it("performs the send at once, without a card, and posts the outcome card", async () => {
    const {chat, tools} = await standing([alice])
    const result = await tools.call("send", {chat: "@4", text: "trail shoes", wait: 0}, signal().call)
    expect(result!.structuredContent).toMatchObject({action: 1, status: "done", chat: "@4", responses: [], more: 0})
    expect(chat.sent.filter(({chat: [, id]}) => id === 4).map(({messages: [{msgContent}]}) => msgContent.text)).toEqual(["trail shoes"])
    await settled()
    expect(cards(chat)).toEqual(["I sent a message to bot SimpleX Directory:\n`trail shoes`\nApproved for this chat by Alice. /'revoke @4'"])
  })

  it("posts a card when the approving member is no longer an approver", async () => {
    const {chat, actions, tools} = await standing([{...alice, memberRole: T.GroupMemberRole.Member}, member(8, "Carol", T.GroupMemberRole.Owner)])
    const {controller, call} = signal()
    const sending = tools.call("send", {chat: "@4", text: "trail shoes", wait: 0}, call)
    await settled()
    expect(actions.get(1)).toMatchObject({state: "pending", standing: false})
    expect(cards(chat)).toEqual(["I want to send a message to bot SimpleX Directory:\n`trail shoes`\n/'approve @4 1', /'approve @4 all' or /'reject @4 1'"])
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
    expect(cards(chat)).toEqual(["I want to delete bot SimpleX Directory (@4).\n/'approve @4 1' or /'reject @4 1'"])
    controller.abort()
    expect(await leaving).toBeUndefined()
  })

  it("applies in no chat in mode host", async () => {
    const {chat, control, tools} = await standing([alice], "host")
    expect(control.standingApprover(chatInfoOf(chat, "@4"))).toBeUndefined()
    const {chats} = (await tools.call("list", {}, signal().call))!.structuredContent as {chats: {chat: string}[]}
    expect(chats.find(({chat: ref}) => ref === "@4")).not.toHaveProperty("approved")
    expect((await control.ownerCommand(alice, {keyword: "list", params: ""}))!.split("\n")[0]).toBe("@4  bot SimpleX Directory")
    expect(await control.ownerCommand(alice, {keyword: "approve", params: "#2 all"})).toBe("Approvals apply in mode requests; this server runs in mode host.")
    expect(chat.customData).toEqual([])
    expect((await tools.call("send", {chat: "@4", text: "trail shoes", wait: 0}, signal().call))!.structuredContent).toMatchObject({action: 1, status: "done"})
    await settled()
    expect(cards(chat)).toEqual(["I sent a message to bot SimpleX Directory:\n`trail shoes`\nPerformed without approval (mode host)."])
  })
})

describe("approval card size", () => {
  const tooLong = "text: too long for its approval card; shorten it."

  async function sendTools(approvers: T.GroupMember[] = [alice]) {
    const chat = agentChats()
    chat.members.set(10, [self, ...approvers, bob])
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {simplexMcp: {approvedBy: "Alice", approvedById: 3, approvedAt: "2026-10-18T10:00:00.000Z"}}
    const actions: Actions = new Actions("requests", 23, 1, {
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
    const atLimit = "€".repeat(4374) + "aa"
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
    const atLimit = "\b".repeat(2183) + "aa"
    expect(textOf(await tools.call("send", {chat: "@4", text: `${atLimit}a`, wait: 0}, signal().call))).toBe(tooLong)
    expect(actions.next).toBe(1)
    expect((await tools.call("send", {chat: "@4", text: atLimit, wait: 0}, signal().call))!.structuredContent).toMatchObject({action: 1, status: "done"})
    await flushed()
    expect(chat.sent.map(({chat: [, id]}) => id)).toEqual([4, 10])
  })

  it("measures the card with the longest name of the current approvers", async () => {
    const {actions, tools, textOf} = await sendTools([alice, member(8, "😀".repeat(50), T.GroupMemberRole.Owner)])
    expect(textOf(await tools.call("send", {chat: "#2", text: "€".repeat(4374) + "aa"}, signal().call))).toBe(tooLong)
    const {controller, call} = signal()
    const sending = tools.call("send", {chat: "#2", text: "€".repeat(4309) + "aa"}, call)
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
    expect(() => actions.create("c", connectFields(name, {message: "\b".repeat(2000), welcome: "Welcome!"}))).toThrow(new ToolError("message: too long for its approval card; shorten it."))
    expect(actions.next).toBe(1)
    actions.create("c", connectFields(name, {message: "\b".repeat(1400), welcome: "Welcome!"}))
    await flushed()
    expect(chat.sent.map(({chat: [, id]}) => id)).toEqual([10])
  })

  it("cuts the link texts on a connect card to fit 15000 bytes in its largest form, and keeps them whole in the action", async () => {
    const {chat, actions} = await sendTools()
    const description = "Trail shoes. ".repeat(1500)
    const action = actions.create("c", connectFields("Ridgeline", {description, welcome: "Welcome!"}))
    await flushed()
    const [card] = chat.sent.filter(({chat: [, id]}) => id === 10).map(({messages: [{msgContent}]}) => msgContent.text.split("\n"))
    expect(card).toHaveLength(3)
    expect(card[1]).toMatch(/^`Trail shoes\. .*…`$/)
    const failed = encodedLength(cardText({...action, state: "failed", decidedBy: "Alice", error: "€".repeat(600)}, "requests"))
    expect(failed).toBeLessThanOrEqual(15000)
    expect(failed).toBeGreaterThan(15000 - 8)
    expect(action.tool === "connect" && action.plan.description).toBe(description)
  })
})

describe("delivery wait at the stop", () => {
  it("logs an error of the SQL query and ends", async () => {
    const errors: string[] = []
    jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text).trimEnd()), true))
    const chat = new FakeChat()
    chat.sendChatCmd = async () => {
      throw new core.ChatAPIError("Chat command error", {type: "errorStore", storeError: {type: "dBException", message: "no such table: connections"}} as unknown as T.ChatError)
    }
    await deliveries(chat.api(), 10)
    expect(errors).toEqual(['delivery wait: SimpleX error: errorStore: dBException: {"message":"no such table: connections"}'])
  })
})
