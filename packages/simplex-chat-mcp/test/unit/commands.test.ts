import {T} from "@simplex-chat/types"
import {NewAction} from "../../src/actions"
import {Control, parseCommand} from "../../src/control"
import {Collected} from "../../src/messages"
import {FakeChat, actionsWith, event, fakeHooks, group, groupChat, item, member, settled} from "./fixtures"

beforeEach(() => {
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(() => jest.restoreAllMocks())

const alice = member(3, "Alice", T.GroupMemberRole.Owner)
const bob = member(7, "Bob", T.GroupMemberRole.Member)

function fields(): NewAction {
  return {
    tool: "send",
    input: {chat: "@4", text: "hi"},
    subject: {kind: "bot", name: "SimpleX Directory", incognito: false},
    chat: "@4",
    mentions: [],
    responses: new Collected(),
  }
}

async function setup(owners?: {memberId: number; name: string}[]) {
  const fake = fakeHooks()
  const actions = actionsWith("requests", fake, 5)
  const chat = new FakeChat()
  const control = new Control(chat.api(), group(10, "agent"), {displayName: "agent", owners, registerHint: "hint", qrCode: "small"}, actions)
  const action = actions.create("k", fields())
  await settled()
  return {fake, actions, chat, control, action}
}

describe("parseCommand", () => {
  it("reads one-line commands", () => {
    expect(parseCommand("/approve 5")).toEqual({keyword: "approve", params: "5"})
    expect(parseCommand(" /reject  5 ")).toEqual({keyword: "reject", params: "5"})
    expect(parseCommand("/pending\n")).toEqual({keyword: "pending", params: ""})
    expect(parseCommand("/approve 5\nnow")).toBeUndefined()
    expect(parseCommand("approve 5")).toBeUndefined()
    expect(parseCommand("ok /approve 5")).toBeUndefined()
  })
})

describe("owner commands", () => {
  it("approves and rejects pending actions", async () => {
    const {control, action, fake} = await setup()
    expect(control.ownerCommand(alice, {keyword: "approve", params: "5"})).toBeUndefined()
    expect(action).toMatchObject({state: "performing", decidedBy: "Alice"})
    expect(fake.calls).toContain("perform 5")
    expect(control.ownerCommand(alice, {keyword: "approve", params: "5"})).toBe("Action 5 is not pending.")
  })

  it("rejects", async () => {
    const {control, action} = await setup()
    expect(control.ownerCommand(alice, {keyword: "reject", params: "5"})).toBeUndefined()
    expect(action).toMatchObject({state: "rejected", decidedBy: "Alice"})
  })

  it("expires due actions before a command", async () => {
    const {control, action} = await setup()
    jest.spyOn(Date, "now").mockReturnValue(action.expires)
    expect(control.ownerCommand(alice, {keyword: "approve", params: "5"})).toBe("Action 5 is not pending.")
    expect(action.state).toBe("expired")
  })

  it("refuses unknown numbers and numbers from an earlier run", async () => {
    const {control} = await setup()
    expect(control.ownerCommand(alice, {keyword: "approve", params: "4"})).toBe("Action 4 is not pending.")
    expect(control.ownerCommand(alice, {keyword: "reject", params: "99"})).toBe("Action 99 is not pending.")
  })

  it("lists pending actions", async () => {
    const {control, action, actions} = await setup()
    expect(control.ownerCommand(alice, {keyword: "pending", params: ""})).toBe(`5  send to bot SimpleX Directory  expires ${new Date(action.expires).toISOString().slice(11, 16)}`)
    actions.reject(action, "Alice")
    expect(control.ownerCommand(alice, {keyword: "pending", params: ""})).toBe("No pending actions.")
  })

  it("answers other commands with the list", async () => {
    const {control} = await setup()
    for (const command of [{keyword: "help", params: ""}, {keyword: "approve", params: "five"}, {keyword: "approve", params: ""}, {keyword: "pending", params: "5"}]) {
      expect(control.ownerCommand(bob, command)).toBe("Commands: /approve N, /reject N, /pending")
    }
  })

  it("accepts only approvers", async () => {
    const unlisted = await setup()
    expect(unlisted.control.ownerCommand(bob, {keyword: "approve", params: "5"})).toBe("Only owners approve actions.")
    const listed = await setup([{memberId: 3, name: "Alice"}])
    expect(listed.control.ownerCommand(member(8, "Carol", T.GroupMemberRole.Owner), {keyword: "approve", params: "5"})).toBe("Only the owners listed in --owners approve actions.")
    expect(listed.control.ownerCommand(alice, {keyword: "pending", params: ""})).toMatch(/^5 {2}send/)
    const none = await setup([])
    expect(none.control.ownerCommand(alice, {keyword: "approve", params: "5"})).toBe("Only the owners listed in --owners approve actions.")
  })

  it("replies in the agent control chat to commands only", async () => {
    const {control, chat} = await setup()
    const chatInfo = groupChat(group(10, "agent"))
    control.onMessage(event(chatInfo, item(1, "/approve 4", {type: "groupRcv", groupMember: alice})))
    control.onMessage(event(chatInfo, item(2, "thanks", {type: "groupRcv", groupMember: alice})))
    control.onMessage(event(chatInfo, item(3, "/approve 5\nplease", {type: "groupRcv", groupMember: alice})))
    await settled()
    expect(chat.replies).toEqual(["Action 4 is not pending."])
  })
})
