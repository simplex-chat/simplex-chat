import {T} from "@simplex-chat/types"
import {Actions, NewAction} from "../../src/actions"
import {Control, parseCommand} from "../../src/control"
import {ChatItemsWatch, Collected} from "../../src/messages"
import {OwnerEntry} from "../../src/serve"
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

async function controlWith(chat: FakeChat, actions: Actions, owners?: OwnerEntry[]): Promise<{control: Control; tools: AgentTools}> {
  const tools = new AgentTools({chat: chat.api(), user, actions, watch: new ChatItemsWatch(), controlChat: () => "#10", standingApprover: chatInfo => control.standingApprover(chatInfo)}, "requests")
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
    expect(await control.ownerCommand(alice, {keyword: "pending", params: ""})).toBe(`5  send to bot SimpleX Directory  expires ${new Date(action.expires).toISOString().slice(11, 16)}`)
    actions.reject(action, "Alice")
    expect(await control.ownerCommand(alice, {keyword: "pending", params: ""})).toBe("No pending actions.")
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
  async function standing(approvers: T.GroupMember[]) {
    const chat = agentChats()
    chat.members.set(10, [self, ...approvers])
    const {contact: directory} = chatInfoOf(chat, "@4") as T.ChatInfo.Direct
    directory.customData = {simplexMcp: {approvedBy: "Alice", approvedById: 3, approvedAt: "2026-10-18T10:00:00.000Z"}}
    const actions = new Actions("requests", 23, 1)
    const {control, tools} = await controlWith(chat, actions)
    actions.setHooks({
      refusal: () => control.refusal(),
      saveCounter: async () => undefined,
      postCard: action => control.postCard(action),
      outcome: action => control.outcome(action),
      perform: action => tools.perform(action),
    })
    return {chat, actions, tools}
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
})
