import {existsSync, mkdirSync, mkdtempSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {CallToolResult} from "@modelcontextprotocol/server"
import {CEvt, ChatEvent, T} from "@simplex-chat/types"
import {api, core} from "simplex-chat"
import {Action} from "../../src/actions"
import {controlCommands} from "../../src/control"
import {SessionFile, createDataFolder, readSessionFile, sessionFolder, writeSessionFile} from "../../src/files"
import {Hub, Session} from "../../src/hub"
import {createMainGroup} from "../../src/main-group"
import {Collected} from "../../src/messages"
import {AttachLine} from "../../src/protocol"
import * as tools from "../../src/tools"
import {User} from "../../src/user"
import {version} from "../../src/version"
import {FakeChat, contact, direct, event, fileItem, group, groupChat, item, member, signal, until, user} from "./fixtures"

const unknownCommand = "Unknown command."

const id1 = "11111111-1111-4111-8111-111111111111"
const id2 = "22222222-2222-4222-8222-222222222222"
const self = member(1, "Claude", T.GroupMemberRole.Owner, T.GroupMemberStatus.Creator)
const alice = member(3, "Alice", T.GroupMemberRole.Owner)
const bob = member(7, "Bob", T.GroupMemberRole.Member)
const invitedAlice = member(1003, "Alice", T.GroupMemberRole.Owner)
const noApprover = `The group of this session has no admin or owner to approve actions. Members of the Main group join it by /join N there; without members, run npx -y @simplex-chat/mcp@${version} link in a terminal.`
const commandsApproversOnly = "Only admins and owners of this group can send / commands."
const modeCard = "I want to run in mode host: Claude Code asks you before each of my actions, and they are performed without approval here."
const approverRights = "can send prompts to Claude and the group's commands, and approve tool use."
const senderRights = "can send prompts to Claude; the group's commands and approvals need the role admin or owner."
const aliceJoined = `Alice is an owner and ${approverRights}`
const joinRights = "They can join the group of any Claude Code session here, prompt Claude and approve its actions."

type HubConstructor = new (chat: unknown, user: T.User, root: string, expireHours: number, mainGroup: T.GroupInfo, mainName: string) => Hub

let root: string

beforeEach(() => {
  root = mkdtempSync(join(tmpdir(), "simplex-mcp-hub-"))
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(() => {
  jest.restoreAllMocks()
  jest.useRealTimers()
  rmSync(root, {recursive: true, force: true})
})

async function hubWith(chat: FakeChat, mainMembers: T.GroupMember[] = [self, alice, bob]): Promise<Hub> {
  const main = group(10, "SimpleX MCP", self, {customData: {simplexMcpMain: true}})
  chat.chats.set("#10", {chatInfo: groupChat(main), items: []})
  chat.members.set(10, mainMembers)
  const hub = new (Hub as unknown as HubConstructor)(chat.api(), user, root, 23, main, "SimpleX MCP")
  await hub.main.load()
  await hub.main.start()
  return hub
}

function stored(number: number, name: string): SessionFile {
  return {number, name, project: "/p/app", created: "2026-11-08T00:00:00.000Z"}
}

function openSession(hub: Hub, id: string, file: SessionFile): Promise<Session> {
  const folder = sessionFolder(root, id)
  createDataFolder(folder)
  writeSessionFile(folder, file)
  return hub.session(id, file)
}

async function withOwner(chat: FakeChat, session: Session): Promise<void> {
  chat.members.set(session.control.groupId, [self, invitedAlice])
  await session.control.load()
}

function ownedContact(contactId: number, name: string, owner: string, extra: object = {}): T.ChatInfo {
  return direct({...contact(contactId, name, {peerType: "bot"}), customData: {simplexMcpOwner: {session: owner}, ...extra}} as T.Contact)
}

function textOf(result: CallToolResult | undefined): string {
  return (result!.content[0] as {text: string}).text
}

function sentTo(chat: FakeChat, groupId: number): string[] {
  return chat.sent.filter(({chat: [, id]}) => id === groupId).map(({messages: [{msgContent}]}) => msgContent.text)
}

function mainMessage(text: string, sender: T.GroupMember): T.AChatItem {
  return event(groupChat(group(10, "SimpleX MCP", self)), item(900, text, {type: "groupRcv", groupMember: sender}))
}

async function listed(session: Session): Promise<string[]> {
  const {chats} = (await session.call("list", {}, signal().call))!.structuredContent as {chats: {chat: string}[]}
  return chats.map(({chat}) => chat)
}

describe("session groups", () => {
  it("are created at the session start, named by number and title, and the Main group's owners are invited through the Main group, without a record of the invitation", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    expect(session.displayName).toBe("1 · auth")
    expect(chat.newGroups).toEqual([
      {
        userId: 1,
        groupProfile: {
          displayName: "1 · auth",
          fullName: "",
          description: "The Claude Code session 1. Members send it prompts; admins and owners also send the group's commands and approve tool use.",
          groupPreferences: {history: {enable: "on"}, directMessages: {enable: "off"}, commands: controlCommands},
        },
      },
    ])
    expect(chat.invitations).toEqual([{groupId: 20, viaGroupId: 10, groupMemberId: 3, memberRole: T.GroupMemberRole.Owner}])
    expect(chat.customData.filter(({chat: ref}) => ref === "#20").map(({customData}) => customData)).toEqual([{simplexMcpSession: {session: 1}}])
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, group: 20})
    expect(await hub.session(id1, {...stored(1, "auth"), group: 20})).toBe(session)
  })

  it("keep the fields the service wrote in the session file while the group was created", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const folder = sessionFolder(root, id1)
    createDataFolder(folder)
    writeSessionFile(folder, {...stored(1, "auth"), aliases: [id2]})
    await hub.session(id1, stored(1, "auth"))
    expect(readSessionFile(folder)).toEqual({...stored(1, "auth"), aliases: [id2], group: 20})
  })

  it("count approvals from admins and owners of the group and prompts from every member but User, whatever way they joined and whatever their role in the Main group", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    expect(await hub.join(bob, 1)).toBe(`Sent you an invitation to "1 · auth".`)
    const invitedBob = member(1007, "Bob", T.GroupMemberRole.Admin)
    const mallory = member(9, "Mallory", T.GroupMemberRole.Admin)
    const mia = member(12, "Mia", T.GroupMemberRole.Member)
    const max = member(13, "Max", T.GroupMemberRole.Moderator)
    const oscar = member(11, "Oscar", T.GroupMemberRole.Observer)
    hub.userContactId = 6
    const sessionUser = {...member(1006, "User"), memberContactId: 6}
    chat.members.set(20, [self, invitedAlice, invitedBob, mallory, mia, max, oscar, sessionUser])
    await session.control.load()
    const pending = {keyword: "pending", params: ""}
    for (const sender of [invitedAlice, invitedBob, mallory]) expect(await session.control.ownerCommand(sender, pending)).toBe("No pending actions.")
    for (const sender of [mia, max, oscar, sessionUser]) expect(await session.control.ownerCommand(sender, pending)).toBeUndefined()
    chat.members.set(10, [self, {...alice, memberRole: T.GroupMemberRole.Member}, {...bob, memberStatus: T.GroupMemberStatus.Left}])
    await hub.main.onEvent({type: "memberRole", groupInfo: group(10, "SimpleX MCP", self), byMember: bob, member: alice, fromRole: T.GroupMemberRole.Owner, toRole: T.GroupMemberRole.Member} as unknown as ChatEvent)
    expect(await session.control.ownerCommand(invitedAlice, pending)).toBe("No pending actions.")
    expect(await session.control.ownerCommand(invitedBob, pending)).toBe("No pending actions.")
    const lines: AttachLine[] = []
    session.onLine = line => lines.push(line)
    const sessionGroup = groupChat(group(20, "1 · auth"))
    session.onMessage(event(sessionGroup, item(41, "Run the tests", {type: "groupRcv", groupMember: sessionUser})))
    session.onMessage(event(sessionGroup, item(42, "Write the docs", {type: "groupRcv", groupMember: mallory})))
    session.onMessage(event(sessionGroup, item(43, "Fix the build", {type: "groupRcv", groupMember: invitedBob})))
    session.onMessage(event(sessionGroup, item(44, "Tag the release", {type: "groupRcv", groupMember: mia})))
    session.onMessage(event(sessionGroup, item(45, "Deploy", {type: "groupRcv", groupMember: max})))
    await until(() => lines.length === 4)
    expect(lines.map(line => line.prompt?.from)).toEqual(["Mallory", "Bob", "Mia", "Max"])
    chat.chats.set("@4", {chatInfo: ownedContact(4, "SimpleX Directory", id1), items: []})
    chat.members.set(20, [self, mia, max, oscar, sessionUser])
    await session.control.load()
    expect(textOf(await session.call("send", {chat: "@4", text: "hi"}, signal().call))).toBe(noApprover)
    chat.members.set(20, [self, mallory])
    await session.control.load()
    const {controller, call} = signal()
    const sending = session.call("send", {chat: "@4", text: "hi"}, call)
    await until(() => session.actions.pending().some(({tool}) => tool === "send"))
    controller.abort()
    expect(await sending).toBeUndefined()
  })

  it("switch a session to mode host only after an owner approves its card in the session's group", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    await withOwner(chat, session)
    session.requestMode("host")
    session.requestMode("host")
    await until(() => sentTo(chat, 20).length === 1)
    expect(sentTo(chat, 20)).toEqual([`${modeCard}\n/'approve 1' or /'reject 1'`])
    expect(session.actions.mode).toBe("requests")
    expect(session.busy()).toBe(false)
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "approve", params: "1"})).toBeUndefined()
    await until(() => chat.updates.length === 1)
    expect(session.actions.mode).toBe("host")
    const savedMode = () => (chat.customData.filter(({chat: ref}) => ref === "#20").at(-1)!.customData as {simplexMcpSession: {mode?: string}}).simplexMcpSession.mode
    await until(() => savedMode() === "host")
    expect(readSessionFile(sessionFolder(root, id1))).not.toHaveProperty("mode")
    expect(chat.updates[0].text).toBe("I run in mode host: Claude Code asks you before each of my actions, and they are performed without approval here.\nApproved by Alice.")
    session.requestMode("requests")
    expect(session.actions.mode).toBe("requests")
    await until(() => savedMode() === "requests")
  })

  it("take the approved mode from the session group's data and not from the session's file", async () => {
    const chat = new FakeChat()
    const session = await openSession(await hubWith(chat), id1, {...stored(1, "auth"), mode: "host"} as SessionFile)
    expect(session.actions.mode).toBe("requests")
    const {groupInfo} = chat.chats.get("#20")!.chatInfo as T.ChatInfo.Group
    groupInfo.customData = {...groupInfo.customData, simplexMcpSession: {session: 1, mode: "host"}}
    const reopened = await (await hubWith(chat)).session(id1, readSessionFile(sessionFolder(root, id1))!)
    expect(reopened.actions.mode).toBe("host")
  })

  it("ask for mode host once an owner joins the group of a session that requested it before", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    session.requestMode("host")
    expect(session.actions.all()).toEqual([])
    chat.members.set(20, [self, invitedAlice])
    await hub["onEvent"]({type: "joinedGroupMember", groupInfo: group(20, "1 · auth"), member: invitedAlice} as unknown as ChatEvent)
    await until(() => sentTo(chat, 20).length === 2)
    expect(sentTo(chat, 20)).toEqual([aliceJoined, `${modeCard}\n/'approve 1' or /'reject 1'`])
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "reject", params: "1"})).toBeUndefined()
    await hub["onEvent"]({type: "connectedToGroupMember", groupInfo: group(20, "1 · auth"), member: invitedAlice} as unknown as ChatEvent)
    await hub["controlEvents"]
    expect(sentTo(chat, 20)).toEqual([aliceJoined, `${modeCard}\n/'approve 1' or /'reject 1'`, aliceJoined])
    expect(session.actions.mode).toBe("requests")
  })

  it("ask for mode host once a member becomes the first admin of the group of a session that requested it before", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    const mia = member(12, "Mia", T.GroupMemberRole.Member)
    chat.members.set(20, [self, mia])
    await session.control.load()
    session.requestMode("host")
    expect(session.actions.all()).toEqual([])
    const admin = {...mia, memberRole: T.GroupMemberRole.Admin}
    chat.members.set(20, [self, admin])
    await hub["onEvent"]({type: "memberRole", groupInfo: group(20, "1 · auth"), byMember: invitedAlice, member: admin, fromRole: T.GroupMemberRole.Member, toRole: T.GroupMemberRole.Admin} as unknown as ChatEvent)
    await until(() => sentTo(chat, 20).length === 2)
    expect(sentTo(chat, 20)).toEqual([`Mia is an admin and ${approverRights} You can change role to observer to stop it: /'observer #20 12'`, `${modeCard}\n/'approve 1' or /'reject 1'`])
  })

  it("post the rights of each member who joins by the member's role, whatever way they joined, with the command that makes a sending member an observer, and nothing for User", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    await openSession(hub, id1, stored(1, "auth"))
    hub.userContactId = 6
    const sessionUser = {...member(1006, "User"), memberContactId: 6}
    const mallory = member(9, "Mallory", T.GroupMemberRole.Admin)
    const mia = member(12, "Mia", T.GroupMemberRole.Member)
    const max = member(13, "Max", T.GroupMemberRole.Moderator)
    const oscar = member(11, "Oscar", T.GroupMemberRole.Observer)
    chat.members.set(20, [self, sessionUser, invitedAlice, mallory, mia, max, oscar])
    const joined = (type: string, joiner: T.GroupMember): Promise<void> => hub["onEvent"]({type, groupInfo: group(20, "1 · auth"), member: joiner} as unknown as ChatEvent)
    await joined("joinedGroupMember", sessionUser)
    await joined("joinedGroupMember", invitedAlice)
    await joined("connectedToGroupMember", {...mallory, memberStatus: T.GroupMemberStatus.Intro_inv})
    await joined("joinedGroupMember", mia)
    await joined("connectedToGroupMember", max)
    await joined("joinedGroupMember", oscar)
    await hub["controlEvents"]
    expect(sentTo(chat, 20)).toEqual([
      aliceJoined,
      `Mallory is an admin and ${approverRights} You can change role to observer to stop it: /'observer #20 9'`,
      `Mia is a member and ${senderRights} You can change role to observer to stop it: /'observer #20 12'`,
      `Max is a moderator and ${senderRights} You can change role to observer to stop it: /'observer #20 13'`,
      "Oscar is an observer and can't send messages.",
    ])
  })

  it("post the rights of a member at each change of the member's role in the group, and nothing for User", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    hub.userContactId = 6
    const sessionUser = {...member(1006, "User"), memberContactId: 6}
    const mia = member(12, "Mia", T.GroupMemberRole.Admin)
    const dan = member(14, "Dan", T.GroupMemberRole.Observer)
    chat.members.set(20, [self, sessionUser, invitedAlice, mia, dan])
    await session.control.load()
    const changed = (changedMember: T.GroupMember, fromRole: T.GroupMemberRole): Promise<void> => hub["onEvent"]({type: "memberRole", groupInfo: group(20, "1 · auth"), byMember: invitedAlice, member: changedMember, fromRole, toRole: changedMember.memberRole} as unknown as ChatEvent)
    await changed(mia, T.GroupMemberRole.Member)
    await changed(dan, T.GroupMemberRole.Admin)
    await changed({...sessionUser, memberRole: T.GroupMemberRole.Observer}, T.GroupMemberRole.Member)
    await hub["controlEvents"]
    expect(sentTo(chat, 20)).toEqual([`Mia is an admin and ${approverRights} You can change role to observer to stop it: /'observer #20 12'`, "Dan is an observer and can't send messages."])
    expect(await session.control.ownerCommand(mia, {keyword: "pending", params: ""})).toBe("No pending actions.")
    expect(await session.control.ownerCommand(dan, {keyword: "pending", params: ""})).toBeUndefined()
  })

  it("keep both the new name and the new model in the group's profile when a rename and a model change of the session overlap", async () => {
    const chat = new FakeChat()
    const session = await openSession(await hubWith(chat), id1, stored(1, "auth"))
    await Promise.all([session.turn({id: id1, model: "claude-opus-5"}), session.rename("deploy")])
    const {groupInfo} = chat.chats.get("#20")!.chatInfo as T.ChatInfo.Group
    expect(groupInfo.groupProfile).toMatchObject({displayName: "1 · deploy", shortDescr: "claude-opus-5"})
  })

  it("keep the profile an owner changed, its preference for files and media included, at the next update of the model and at a rename, from the group's current profile", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    const {groupInfo} = chat.chats.get("#20")!.chatInfo as T.ChatInfo.Group
    const files = {enable: T.GroupFeatureEnabled.Off}
    groupInfo.groupProfile = {...groupInfo.groupProfile, description: "Release work", groupPreferences: {...groupInfo.groupProfile.groupPreferences, files}}
    await session.turn({id: id1, model: "claude-opus-5"})
    await session.rename("deploy")
    expect(chat.profiles.filter(({groupId}) => groupId === 20).map(({groupProfile}) => groupProfile)).toEqual([
      expect.objectContaining({shortDescr: "claude-opus-5", description: "Release work", groupPreferences: expect.objectContaining({files})}),
      expect.objectContaining({displayName: "1 · deploy", shortDescr: "claude-opus-5", description: "Release work", groupPreferences: expect.objectContaining({files})}),
    ])
  })

  it("make a member an observer by /observer #G ID, from a member whose role lets them change that member's role in SimpleX, and reply with the observer's rights; a member who left or has not joined yet is no member", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    const dan = member(12, "Dan", T.GroupMemberRole.Admin)
    const eve = member(13, "Eve", T.GroupMemberRole.Owner)
    const mallory = member(9, "Mallory", T.GroupMemberRole.Member)
    chat.members.set(20, [self, invitedAlice, dan, eve, mallory, member(14, "Fay", T.GroupMemberRole.Member, T.GroupMemberStatus.Left), member(15, "Gus", T.GroupMemberRole.Admin, T.GroupMemberStatus.Invited)])
    await session.control.load()
    const observer = (sender: T.GroupMember, params: string) => session.control.ownerCommand(sender, {keyword: "observer", params})
    expect(await observer(mallory, "#20 12")).toBeUndefined()
    expect(await observer(dan, "#20 13")).toBe("Your role does not let you change the role of Eve.")
    expect(await observer(dan, "#21 9")).toBe("#21 is not this group.")
    expect(await observer(dan, "#20 99")).toBe("No member 99 in this group.")
    expect(await observer(dan, "#20 14")).toBe("No member 14 in this group.")
    expect(await observer(dan, "#20 15")).toBe("No member 15 in this group.")
    expect(await observer(dan, "#20 12")).toBe("Dan's role can't be changed.")
    expect(await observer(dan, "#20 1")).toBe("Claude's role can't be changed.")
    expect(await observer(dan, "20 9")).toBe(unknownCommand)
    expect(chat.roles).toEqual([])
    expect(await observer(dan, "#20 9")).toBe("Mallory is an observer and can't send messages.")
    expect(await observer(invitedAlice, "#20 12")).toBe("Dan is an observer and can't send messages.")
    expect(chat.roles).toEqual([{groupId: 20, members: [9], role: T.GroupMemberRole.Observer}, {groupId: 20, members: [12], role: T.GroupMemberRole.Observer}])
    expect(await observer(member(9, "Mallory", T.GroupMemberRole.Observer), "#20 13")).toBeUndefined()
    expect(chat.roles).toHaveLength(2)
  })

  it("count the standing approval of a chat no more once /observer made its approver an observer", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    const dan = member(12, "Dan", T.GroupMemberRole.Admin)
    chat.chats.set("@4", {chatInfo: ownedContact(4, "SimpleX Directory", id1, {simplexMcp: {approvedBy: "Dan", approvedById: 12, approvedAt: "2026-11-08T10:00:00.000Z"}}), items: []})
    chat.members.set(20, [self, invitedAlice, dan])
    await session.control.load()
    const list = () => session.control.ownerCommand(invitedAlice, {keyword: "list", params: ""})
    expect(await list()).toBe("@4  bot SimpleX Directory, approved for this chat by Dan")
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "observer", params: "#20 12"})).toBe("Dan is an observer and can't send messages.")
    expect(await list()).toBe("@4  bot SimpleX Directory")
    const {controller, call} = signal()
    const sending = session.call("send", {chat: "@4", text: "hi"}, call)
    await until(() => session.actions.pending().some(({tool}) => tool === "send"))
    controller.abort()
    await sending
  })

  it("count the standing approval of a member made an observer by /observer again once that member is an admin again", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    const dan = member(12, "Dan", T.GroupMemberRole.Admin)
    chat.chats.set("@4", {chatInfo: ownedContact(4, "SimpleX Directory", id1, {simplexMcp: {approvedBy: "Dan", approvedById: 12, approvedAt: "2026-11-08T10:00:00.000Z"}}), items: []})
    chat.members.set(20, [self, invitedAlice, dan])
    await session.control.load()
    const list = () => session.control.ownerCommand(invitedAlice, {keyword: "list", params: ""})
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "observer", params: "#20 12"})).toBe("Dan is an observer and can't send messages.")
    expect(await list()).toBe("@4  bot SimpleX Directory")
    chat.members.set(20, [self, invitedAlice, dan])
    await hub["onEvent"]({type: "memberRole", groupInfo: group(20, "1 · auth"), byMember: invitedAlice, member: dan, fromRole: T.GroupMemberRole.Observer, toRole: T.GroupMemberRole.Admin} as unknown as ChatEvent)
    expect(await list()).toBe("@4  bot SimpleX Directory, approved for this chat by Dan")
  })

  it("end a pending mode card when the option returns to requests", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    await withOwner(chat, session)
    session.requestMode("host")
    await until(() => sentTo(chat, 20).length === 1)
    session.requestMode("requests")
    await until(() => chat.updates.length === 1)
    expect(chat.updates[0].text).toBe(`${modeCard}\nEnded.`)
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "approve", params: "1"})).toBe("Action 1 is not pending.")
    expect(session.actions.mode).toBe("requests")
  })

  it("keep the model and effort of a session in its file, for the description after a restart", async () => {
    const chat = new FakeChat()
    const session = await openSession(await hubWith(chat), id1, stored(1, "auth"))
    await session.turn({id: id1, model: "claude-opus-5"})
    await session.turn({id: id1, answer: "", effort: "high"})
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({model: "claude-opus-5", effort: "high"})
    const restarted = new FakeChat()
    const reopened = await openSession(await hubWith(restarted), id1, readSessionFile(sessionFolder(root, id1))!)
    await reopened.turn({id: id1, answer: "", effort: "low"})
    expect(restarted.profiles.filter(({groupId}) => groupId === reopened.control.groupId).map(({groupProfile}) => groupProfile.shortDescr)).toEqual(["claude-opus-5 · effort low"])
  })

  it("claim a chat connected after its connect call returned, and a known chat without owner at a connect, and refuse a known chat of another session", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    await withOwner(chat, session)
    const link = "https://simplex.chat/a#ridgeline"
    const plan = (connectionPlan: object) => ({type: "connectionPlan", user, connLink: {connFullLink: link}, connectionPlan})
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "ok"}}))
    chat.responses.set(`/_connect 1 ${link}`, {type: "sentInvitation", connection: {pccConnId: 300}})
    const connecting = session.call("connect", {target: link, wait: 0}, signal().call)
    await until(() => sentTo(chat, 20).length === 1)
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "approve", params: "1"})).toBeUndefined()
    expect((await connecting)!.structuredContent).toMatchObject({status: "done", connected: false})
    expect(chat.customData.filter(({chat: ref}) => ref === "#20").at(-1)!.customData).toMatchObject({simplexMcpSession: {pending: [{connId: 300}]}})
    const accepted = {...contact(12, "Ridgeline"), activeConn: {connId: 300, connStatus: {type: "ready"}}} as unknown as T.Contact
    chat.chats.set("@12", {chatInfo: direct(accepted), items: []})
    await hub["onEvent"]({type: "contactConnected", user, contact: accepted} as unknown as ChatEvent)
    await until(() => chat.customData.some(({chat: ref}) => ref === "@12"))
    expect(await listed(session)).toEqual(["@12"])
    expect(chat.customData.filter(({chat: ref}) => ref === "#20").at(-1)!.customData).toMatchObject({simplexMcpSession: {pending: []}})
    chat.chats.set("@13", {chatInfo: direct(contact(13, "Trailhead")), items: []})
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "known", contact: contact(13, "Trailhead")}}))
    expect((await session.call("connect", {target: link}, signal().call))!.structuredContent).toMatchObject({status: "known", chat: "@13"})
    expect((await listed(session)).sort()).toEqual(["@12", "@13"])
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "known", contact: (ownedContact(14, "Summit", id2) as T.ChatInfo.Direct).contact}}))
    expect(textOf(await session.call("connect", {target: link}, signal().call))).toBe(`The chat of ${link} belongs to another session; an admin or owner of that session's group can give it to this session with /give.`)
  })

  it("claim a group joined after its connect call returned, and no chat of a connection pending for over a day", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    session["savePending"](301)
    session["savePending"](302)
    chat.chats.set("#30", {chatInfo: groupChat(group(30, "Team")), items: []})
    await hub["onEvent"]({type: "userJoinedGroup", user, groupInfo: group(30, "Team"), hostMember: {...member(5, "Host", T.GroupMemberRole.Owner), activeConn: {connId: 301}}} as unknown as ChatEvent)
    await until(() => chat.customData.some(({chat: ref}) => ref === "#30"))
    expect(chat.customData.filter(({chat: ref}) => ref === "#30").at(-1)!.customData).toEqual({simplexMcpOwner: {session: id1}})
    const accepted = {...contact(12, "Ridgeline"), activeConn: {connId: 302, connStatus: {type: "ready"}}} as unknown as T.Contact
    chat.chats.set("@12", {chatInfo: direct(accepted), items: []})
    const dayLater = jest.spyOn(Date, "now").mockReturnValue(Date.now() + 86_400_001)
    await hub["onEvent"]({type: "contactConnected", user, contact: accepted} as unknown as ChatEvent)
    dayLater.mockRestore()
    await new Promise(resolve => setTimeout(resolve, 50))
    expect(chat.customData.some(({chat: ref}) => ref === "@12")).toBe(false)
  })

  it("keep the data written to the group of a session while its profile changes", async () => {
    const chat = new FakeChat()
    const session = await openSession(await hubWith(chat), id1, stored(1, "auth"))
    await Promise.all([session.control.describe("claude-opus-5"), session.control.saveData({marker: true})])
    await session.control.saveCounter(5)
    expect(chat.customData.filter(({chat: ref}) => ref === "#20").at(-1)!.customData).toMatchObject({marker: true, simplexMcp: {nextAction: 5}})
  })

  it("ask for mode host no more after an owner rejected it, also after a restart, until the option returns to requests", async () => {
    const chat = new FakeChat()
    const session = await openSession(await hubWith(chat), id1, stored(1, "auth"))
    await withOwner(chat, session)
    session.requestMode("host")
    await until(() => sentTo(chat, 20).length === 1)
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "reject", params: "1"})).toBeUndefined()
    session.requestMode("host")
    expect(session.actions.pending()).toEqual([])
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({hostDeclined: true})
    const reopened = await (await hubWith(chat)).session(id1, readSessionFile(sessionFolder(root, id1))!)
    reopened.requestMode("host")
    expect(reopened.actions.pending()).toEqual([])
    reopened.requestMode("requests")
    expect(readSessionFile(sessionFolder(root, id1))!.hostDeclined).toBeUndefined()
    reopened.requestMode("host")
    await until(() => sentTo(chat, 20).length === 2)
  })

  it("show each session only its own chats, and /give hands a chat to another session", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const one = await openSession(hub, id1, stored(1, "one"))
    const two = await openSession(hub, id2, stored(2, "two"))
    chat.chats.set("@4", {chatInfo: ownedContact(4, "SimpleX Directory", id1), items: [item(30, "hello")]})
    await withOwner(chat, one)
    expect(await listed(one)).toEqual(["@4"])
    expect(await listed(two)).toEqual([])
    expect(textOf(await two.call("receive", {chat: "@4"}, signal().call))).toBe("No chat @4. list shows the agent's chats.")
    expect(textOf(await two.call("send", {chat: "#20", text: "hi"}, signal().call))).toBe("No chat #20. list shows the agent's chats.")
    expect(await one.control.ownerCommand(invitedAlice, {keyword: "give", params: "@4 3"})).toBe("No session 3. /sessions in the Main group lists the sessions.")
    expect(await one.control.ownerCommand(invitedAlice, {keyword: "give", params: "@4 2"})).toBe("Gave bot SimpleX Directory (@4) to session 2.")
    expect(await listed(one)).toEqual([])
    expect(await listed(two)).toEqual(["@4"])
    expect(await one.control.ownerCommand(invitedAlice, {keyword: "give", params: "@4 1"})).toBe("No chat @4. /list shows the agent's chats.")
  })

  it("list and revoke the standing approvals of a closed session in its group", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    const approval = {simplexMcp: {approvedBy: "Alice", approvedById: 1003, approvedAt: "2026-11-08T10:00:00.000Z"}}
    chat.chats.set("@4", {chatInfo: ownedContact(4, "SimpleX Directory", id1, approval), items: []})
    await withOwner(chat, session)
    expect(session.open).toBe(false)
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "list", params: ""})).toBe("@4  bot SimpleX Directory, approved for this chat by Alice")
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "revoke", params: "@4"})).toBe("Messages to bot SimpleX Directory (@4) need approval again.")
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "list", params: ""})).toBe("@4  bot SimpleX Directory")
  })

  it("refuse to send files of the data root, except the session's own received files", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    chat.chats.set("@4", {chatInfo: ownedContact(4, "SimpleX Directory", id1), items: []})
    const otherFiles = join(sessionFolder(root, id2), "files", "contact-1")
    const ownFiles = join(sessionFolder(root, id1), "files", "contact-4")
    for (const folder of [otherFiles, ownFiles]) mkdirSync(folder, {recursive: true})
    writeFileSync(join(root, "token"), "secret\n")
    writeFileSync(join(otherFiles, "1.txt"), "other")
    writeFileSync(join(ownFiles, "2.txt"), "own")
    const send = async (file: string): Promise<string> => textOf(await session.call("send", {chat: "@4", file}, signal().call))
    expect(await send(join(root, "token"))).toMatch(/ is refused: private keys and the server's own files are not sent\.$/)
    expect(await send(join(otherFiles, "1.txt"))).toMatch(/ is refused: /)
    expect(await send(join(ownFiles, "2.txt"))).toBe(noApprover)
  })

  it("expire the overdue actions of every session at each event of the core", async () => {
    jest.useFakeTimers({doNotFake: ["nextTick", "setImmediate", "queueMicrotask"]})
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    await withOwner(chat, session)
    session.requestMode("host")
    await jest.advanceTimersByTimeAsync(0)
    jest.setSystemTime(Date.now() + 24 * 3_600_000)
    expect(session.actions.all().map(({state}) => state)).toEqual(["pending"])
    await hub["onEvent"]({type: "hostConnected"} as unknown as ChatEvent)
    await jest.advanceTimersByTimeAsync(0)
    expect(session.actions.all()).toEqual([])
    expect(chat.updates.map(({text}) => text)).toEqual([`${modeCard}\nExpired.`])
  })
})

describe("session channel", () => {
  const sessionGroup = groupChat(group(20, "1 · auth"))
  const mallory = member(9, "Mallory", T.GroupMemberRole.Owner)
  const from = (sender: T.GroupMember): T.CIDirection => ({type: "groupRcv", groupMember: sender})
  const tag = (n: number, text: string): string => `<channel source="plugin:simplex:simplex" from="Alice" item="${n}">\n${text}\n</channel>`
  const answers = (chat: FakeChat) => chat.sent.filter(({chat: [, groupId]}) => groupId === 20).map(({messages}) => messages.map(({msgContent, quotedItemId}) => ({text: msgContent.text, quotedItemId})))
  const quotingPosts = (chat: FakeChat) => answers(chat).flatMap(([first]) => (first.quotedItemId === undefined ? [] : [first.text]))
  const shown = (chat: FakeChat) => {
    let itemId = chat.nextItemId - chat.sent.reduce((count, {messages}) => count + messages.length, 0)
    return chat.sent.flatMap(({chat: [, groupId], messages}) =>
      messages.flatMap(({msgContent, quotedItemId}) => {
        const id = itemId++
        const text = chat.updates.filter(update => update.itemId === id).at(-1)?.text ?? msgContent.text
        return groupId === 20 ? [{text, quotedItemId}] : []
      }),
    )
  }
  const ended = {text: expect.stringMatching(/^!- ✅ Done in \d+ ms!$/), quotedItemId: undefined}
  const endedIn = (time: string) => ({text: `!- ✅ Done in ${time}!`, quotedItemId: undefined})
  const done = {text: "!- ✅ Done!", quotedItemId: undefined}
  const ownerPrompt = async (session: Session, n: number, text: string): Promise<void> => {
    session.onMessage(event(sessionGroup, item(n, text, from(invitedAlice))))
    await until(() => session["pushed"].some(({item: pushed}) => pushed.meta.itemId === n))
  }

  async function channel(chat: FakeChat): Promise<{hub: Hub; session: Session; lines: AttachLine[]}> {
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    await withOwner(chat, session)
    const lines: AttachLine[] = []
    session.onLine = line => lines.push(line)
    return {hub, session, lines}
  }

  it("posts a card for each tool use, passes the members' verdicts to the session, whatever way they joined, and ends the open cards with the turn", async () => {
    const chat = new FakeChat()
    const {session, lines} = await channel(chat)
    for (const requestId of ["abcde", "fghij", "kmnop"]) session.permission({id: id1, requestId, toolName: "Bash", description: "Run shell command", inputPreview: "rm -rf build"})
    await until(() => sentTo(chat, 20).length === 3)
    const card = "I want to use `Bash`: `Run shell command`\n`rm -rf build`"
    expect(sentTo(chat, 20)).toEqual([1, 2, 3].map(n => `${card}\n/'approve ${n}' or /'reject ${n}'`))
    expect(await session.control.ownerCommand(mallory, {keyword: "approve", params: "1"})).toBeUndefined()
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "reject", params: "2"})).toBeUndefined()
    await until(() => chat.updates.length === 2)
    expect(lines).toEqual([{permission: {requestId: "abcde", behavior: "allow"}}, {permission: {requestId: "fghij", behavior: "deny"}}])
    expect(session.busy()).toBe(true)
    await session.turn({id: id1, answer: ""})
    await until(() => chat.updates.length === 3)
    expect(chat.updates.map(({text}) => text)).toEqual([`${card}\nApproved by Mallory.`, `${card}\nRejected by Alice.`, `${card}\nEnded.`])
    expect(lines).toHaveLength(2)
    expect(session.busy()).toBe(false)
    expect(sentTo(chat, 20)).toHaveLength(3)
  })

  it("leaves the tool-use and mode cards out of the actions that list returns to the session", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    session.permission({id: id1, requestId: "abcde", toolName: "Bash", description: "Run shell command", inputPreview: "ls"})
    session.requestMode("host")
    await until(() => sentTo(chat, 20).length === 2)
    expect(session.actions.pending().map(({tool}) => tool)).toEqual(["permission", "mode"])
    expect((await session.call("list", {}, signal().call))!.structuredContent).toEqual({chats: [], actions: []})
  })

  it("cuts the input preview of a tool-use card to the size of a card", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const [first, second] = [`{"old_string": "${"漢".repeat(3500)}",`, `"new_string": "${"字".repeat(3500)}"}`]
    session.permission({id: id1, requestId: "abcde", toolName: "Edit", description: "Edit a file", inputPreview: `${first}\n${second}`})
    await until(() => sentTo(chat, 20).length === 1)
    const [card] = sentTo(chat, 20)
    expect(Buffer.byteLength(JSON.stringify(card))).toBeLessThanOrEqual(15_000)
    expect(card.split("\n")).toEqual(["I want to use `Edit`: `Edit a file`", `\`${first}\``, expect.stringMatching(/^`"new_string": "字+…`$/), "/'approve 1' or /'reject 1'"])
  })

  it("passes the messages of owners to the session as prompts, with their files, and not their commands", async () => {
    const chat = new FakeChat()
    const {hub, session, lines} = await channel(chat)
    session.onMessage(event(sessionGroup, item(40, "/pending", from(invitedAlice))))
    session.onMessage(event(sessionGroup, item(42, "Fix the build", from(invitedAlice))))
    await until(() => lines.length === 1 && chat.replies.length === 1)
    expect(lines).toEqual([{prompt: {text: "Fix the build", from: "Alice", item: 42}}])
    expect(chat.replies).toEqual(["No pending actions."])
    const path = join(sessionFolder(root, id1), "files", "group", "8.pdf")
    const offered = fileItem(43, {type: "file", text: "See the log"}, {type: "rcvInvitation"}, {chatDir: from(invitedAlice), fileId: 8})
    chat.responses.set(`/freceive 8 approved_relays=on ${path}`, {type: "rcvFileAccepted", user, chatItem: event(sessionGroup, offered)})
    session.onMessage(event(sessionGroup, offered))
    await until(() => chat.commands.includes(`/freceive 8 approved_relays=on ${path}`))
    expect(lines).toHaveLength(1)
    await hub["onEvent"]({type: "rcvFileComplete", user, chatItem: event(sessionGroup, offered)} as unknown as ChatEvent)
    await until(() => lines.length === 2)
    expect(lines[1]).toEqual({prompt: {text: "See the log", from: "Alice", item: 43, file: path}})
    session.onLine = undefined
    session.onMessage(event(sessionGroup, item(44, "Run the tests", from(invitedAlice))))
    await until(() => quotingPosts(chat).length === 1)
    expect(answers(chat).at(-1)).toEqual([{text: "Session 1 is not running.", quotedItemId: 44}])
    expect(lines).toHaveLength(2)
  })

  it("passes a member's reply to the session as the quote line and the text, with the ID and sender of the quoted message: Claude's answer, a card of several lines, a post of User, another member's prompt, an image whose quote has the file name, a video without text, and a message absent from the group", async () => {
    const chat = new FakeChat()
    const {session, lines} = await channel(chat)
    const reply = (n: number, text: string, quotedItem: object): T.AChatItem => event(sessionGroup, {...item(n, text, from(invitedAlice)), quotedItem: {sentAt: "", ...quotedItem}} as unknown as T.ChatItem)
    const quoted = (chatDir: object, itemId: number | undefined, content: object): object => ({chatDir, ...(itemId === undefined ? {} : {itemId}), content})
    const text = (value: string): object => ({type: "text", text: value})
    const card = "I want to use `Bash`: `Run shell command`\n`rm -rf build`\n/'approve 3' or /'reject 3'"
    const replies = [
      reply(43, "No, the other test", quoted({type: "groupSnd"}, 41, text("Fixed: the build passes."))),
      reply(44, "Why?", quoted({type: "groupSnd"}, 38, text(card))),
      reply(45, "Is it done?", quoted(from(member(8, "User")), 36, text("Fix the build"))),
      reply(46, "Agreed", quoted(from(bob), 35, text("Run the tests"))),
      reply(47, "What is it?", quoted(from(bob), 34, {type: "image", text: "IMG_2041.jpg", image: ""})),
      reply(48, "Play it", quoted(from(bob), 33, {type: "video", text: "", image: "", duration: 5})),
      reply(49, "Shorter", quoted({type: "groupSnd"}, 32, text("a".repeat(300)))),
      reply(50, "Who said it?", quoted(from(bob), undefined, text("Meet at 9"))),
    ]
    for (const ci of replies) session.onMessage(ci)
    await until(() => lines.length === replies.length)
    expect(lines).toEqual([
      {prompt: {text: "> Fixed: the build passes.\nNo, the other test", from: "Alice", item: 43, replyTo: {item: 41, from: "agent"}}},
      {prompt: {text: "> I want to use `Bash`: `Run shell command`…\nWhy?", from: "Alice", item: 44, replyTo: {item: 38, from: "agent"}}},
      {prompt: {text: "> Fix the build\nIs it done?", from: "Alice", item: 45, replyTo: {item: 36, from: "User"}}},
      {prompt: {text: "> Run the tests\nAgreed", from: "Alice", item: 46, replyTo: {item: 35, from: "Bob"}}},
      {prompt: {text: "> image: IMG_2041.jpg\nWhat is it?", from: "Alice", item: 47, replyTo: {item: 34, from: "Bob"}}},
      {prompt: {text: "> video\nPlay it", from: "Alice", item: 48, replyTo: {item: 33, from: "Bob"}}},
      {prompt: {text: `> ${"a".repeat(200)}…\nShorter`, from: "Alice", item: 49, replyTo: {item: 32, from: "agent"}}},
      {prompt: {text: "> Meet at 9\nWho said it?", from: "Alice", item: 50, replyTo: {from: "Bob"}}},
    ])
  })

  it("matches the prompt of a reply in the channel tag with the quote line and the reply attributes, escaped as Claude Code escapes them, so User posts nothing and the answer in the turn's opening message quotes the reply", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posts: string[] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posts.push(...parts)} as unknown as User
    const quoted = {chatDir: from(member(7, `Bob "B" O'Neil`)), itemId: 41, sentAt: "", content: {type: "text", text: 'Fixed: <b>the</b> build & "tests"'}}
    session.onMessage(event(sessionGroup, {...item(43, 'No, the other "test"', from(invitedAlice)), quotedItem: quoted} as unknown as T.ChatItem))
    await until(() => session["pushed"].length === 1)
    await session.turn({id: id1, prompt: '<channel source="plugin:simplex:simplex" from="Alice" item="43" reply_to="41" reply_to_from="Bob &quot;B&quot; O&apos;Neil">\n> Fixed: <b>the</b> build & "tests"\nNo, the other "test"\n</channel>'})
    await session.turn({id: id1, answer: "Done."})
    expect(posts).toEqual([])
    expect(shown(chat)).toEqual([{text: "Done.", quotedItemId: 43}, ended])
  })

  it("shows the model and effort in the group description, posts terminal prompts as User except pushed ones, raw or in this channel's tag, and prompts made only of this channel's tags, and answers as Claude in the opening message of the turn, quoting the owner's prompt", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posts: string[][] = []
    hub.user = {post: async (n: number, parts: string[]) => void posts.push([String(n), ...parts])} as unknown as User
    const described = () => chat.profiles.filter(({groupId}) => groupId === 20).map(({groupProfile}) => groupProfile.shortDescr)
    await session.turn({id: id1, model: "claude-opus-5"})
    await session.turn({id: id1, answer: "", effort: "high"})
    await session.turn({id: id1, answer: "", effort: "high"})
    await session.turn({id: id1, model: "claude-sonnet-5"})
    expect(described()).toEqual(["claude-opus-5", "claude-opus-5 · effort high", "claude-sonnet-5 · effort high"])
    session.onMessage(event(sessionGroup, item(42, "Fix the build", from(invitedAlice))))
    await until(() => session["pushed"].length === 1)
    await session.turn({id: id1, prompt: tag(42, "Fix the build")})
    await session.turn({id: id1, answer: "Fixed."})
    session.onMessage(event(sessionGroup, item(43, "Run the tests", from(invitedAlice))))
    await until(() => session["pushed"].length === 1)
    await session.turn({id: id1, prompt: "Run the tests"})
    await session.turn({id: id1, answer: "Tests pass."})
    await session.turn({id: id1, prompt: tag(41, "Fix the build")})
    await session.turn({id: id1, prompt: '<channel source="plugin:other:other" from="Alice" item="42">\nFix the build\n</channel>'})
    await session.turn({id: id1, prompt: "/compact"})
    await session.turn({id: id1, prompt: "Run the tests"})
    await session.turn({id: id1, answer: `${"é".repeat(7481)}ok`})
    expect(posts.map(([, text]) => text)).toEqual(['<channel source="plugin:other:other" from="Alice" item="42">\nFix the build\n</channel>', "Run the tests"])
    await session["finalizing"]
    expect(shown(chat)).toEqual([
      {text: "Fixed.", quotedItemId: 42},
      ended,
      {text: "Tests pass.", quotedItemId: 43},
      ended,
      {text: "⏹️ Stopped", quotedItemId: undefined},
      {text: "é".repeat(7481), quotedItemId: undefined},
      {text: "ok", quotedItemId: undefined},
      ended,
    ])
  })

  it("keeps the model and the effort of the request whose hook started last, also when an older request arrives later", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const described = () => chat.profiles.filter(({groupId}) => groupId === 20).map(({groupProfile}) => groupProfile.shortDescr)
    await session.turn({id: id1, at: 2, model: "claude-sonnet-5"})
    await session.turn({id: id1, at: 1, answer: "Done.", model: "claude-opus-5"})
    expect(described()).toEqual(["claude-sonnet-5"])
    await session.turn({id: id1, at: 4, answer: "Done.", model: "claude-opus-5"})
    expect(described()).toEqual(["claude-sonnet-5", "claude-opus-5"])
    await session.turn({id: id1, at: 3, model: "claude-haiku-5"})
    expect(described()).toEqual(["claude-sonnet-5", "claude-opus-5"])
    await session.turn({id: id1, at: 6, prompt: "Think harder", effort: "high"})
    await session.turn({id: id1, at: 5, answer: "Done.", effort: "low"})
    expect(described().at(-1)).toBe("claude-opus-5 · effort high")
    await session.turn({id: id1, at: 8, answer: "Done.", effort: "max"})
    await session.turn({id: id1, at: 7, model: "claude-opus-5", effort: "low"})
    expect(described()).toEqual(["claude-sonnet-5", "claude-opus-5", "claude-opus-5 · effort high", "claude-opus-5 · effort max"])
    await session.turn({id: id1, model: "claude-sonnet-5"})
    expect(described().at(-1)).toBe("claude-sonnet-5 · effort max")
  })

  it("keeps the model of a hook that started while a title was held, when the title's model comes later with the older time of its hook", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const described = () => chat.profiles.filter(({groupId}) => groupId === 20).map(({groupProfile}) => groupProfile.shortDescr)
    await session.turn({id: id1, at: 7, answer: "Done.", model: "claude-sonnet-5"})
    await session.turn({id: id1, at: 5, model: "claude-opus-5"})
    expect(described()).toEqual(["claude-sonnet-5"])
  })

  it("posts as User only the prompts a person typed, keeps a turn record for every prompt, and quotes an owner's prompt in its answer", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posts: string[] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posts.push(parts.join(""))} as unknown as User
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build"), typed: false})
    await session.turn({id: id1, at: 2, answer: "Fixed."})
    await session.turn({id: id1, at: 3, prompt: "<task-notification>\n<task-id>b1</task-id>\n</task-notification>", typed: false})
    await session.turn({id: id1, at: 4, answer: "Noted."})
    await session.turn({id: id1, at: 5, prompt: "Run the tests", typed: true})
    await session.turn({id: id1, at: 6, answer: "Tests pass."})
    await session.turn({id: id1, at: 7, prompt: "Run the linter"})
    expect(posts).toEqual(["Run the tests", "Run the linter"])
    expect(shown(chat)).toEqual([
      {text: "Fixed.", quotedItemId: 42},
      endedIn("1 ms"),
      {text: "Noted.", quotedItemId: undefined},
      endedIn("1 ms"),
      {text: "Tests pass.", quotedItemId: undefined},
      endedIn("1 ms"),
      {text: "", quotedItemId: undefined},
    ])
    expect(session["turns"]).toEqual([{at: 7, item: null}])
  })

  it("posts the note of Claude Code's own prompt as Claude without a quote, and the answer of the turn it starts without a quote", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posts: string[] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posts.push(parts.join(""))} as unknown as User
    const note = '✅ Background command "Build" completed (exit code 0)'
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build"), typed: false})
    await session.turn({id: id1, at: 2, answer: "Fixed."})
    await session.turn({id: id1, at: 3, prompt: "<task-notification>\n<task-id>b1</task-id>\n</task-notification>", typed: false, note})
    await session.turn({id: id1, at: 4, answer: "The build passed."})
    expect(posts).toEqual([])
    expect(shown(chat)).toEqual([{text: "Fixed.", quotedItemId: 42}, endedIn("1 ms"), {text: note, quotedItemId: undefined}, {text: "The build passed.", quotedItemId: undefined}, endedIn("1 ms")])
  })

  it("keeps the turn's opening message and its open tool-use cards for a prompt that reaches the running turn, and adds a turn record for an owner's prompt among them", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posts: string[] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posts.push(parts.join(""))} as unknown as User
    const note = '✅ Background command "Tests" completed (exit code 0)'
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build"), typed: false})
    session.permission({id: id1, requestId: "abcde", toolName: "Bash", description: "Run shell command", inputPreview: "ls"})
    await until(() => sentTo(chat, 20).length === 2)
    const later = Date.now() + 1_000
    await session.turn({id: id1, at: later, prompt: "<task-notification>\n<task-id>b2</task-id>\n</task-notification>", typed: false, note, midTurn: true})
    await session.turn({id: id1, at: later + 1, prompt: "Also run the linter", typed: true, midTurn: true})
    expect(session.actions.pending().map(({tool}) => tool)).toEqual(["permission"])
    expect(chat.updates).toEqual([])
    await session.turn({id: id1, at: later + 2, answer: "Fixed, and the linter passes."})
    expect(posts).toEqual(["Also run the linter"])
    await until(() => chat.updates.length === 2)
    expect(shown(chat)).toEqual([{text: "Fixed, and the linter passes.", quotedItemId: 42}, {text: "I want to use `Bash`: `Run shell command`\n`ls`\nEnded.", quotedItemId: undefined}, {text: note, quotedItemId: undefined}, {text: expect.stringMatching(/^!- ✅ Done in /), quotedItemId: undefined}])
    await ownerPrompt(session, 43, "Deploy")
    await session.turn({id: id1, at: later + 3, prompt: tag(43, "Deploy"), typed: false})
    await ownerPrompt(session, 44, "and tag it")
    await session.turn({id: id1, at: later + 4, prompt: tag(44, "and tag it"), typed: false, midTurn: true})
    expect(session["turns"].map(({item}) => item?.meta.itemId)).toEqual([43, 44])
    await session.turn({id: id1, at: later + 5, answer: "Deployed and tagged."})
    expect(shown(chat).slice(4)).toEqual([{text: "Deployed and tagged.", quotedItemId: 43}, endedIn("2 ms")])
  })

  const priorities = (chat: FakeChat) => chat.sent.filter(({chat: [, groupId]}) => groupId === 20).map(({messages}) => messages.map(message => (message as {priority?: number}).priority))

  it("posts the start of a step as a silent live line and updates it with the step's end, and posts the end that arrives before its start as a silent line, and nothing for that start", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const posted = () => chat.sent.filter(({chat: [, groupId]}) => groupId === 20)
    const line = chat.nextItemId
    await session.turn({id: id1, at: 1, step: {id: "toolu_1", text: "💻 Run the tests"}})
    expect(posted()).toEqual([{chat: [T.ChatType.Group, 20], messages: [{msgContent: {type: "text", text: "💻 Run the tests"}, mentions: {}, priority: -1}], live: true}])
    await session.turn({id: id1, at: 2, step: {id: "toolu_1", text: "💻 Run the tests ✅ 2.0 s", end: true}})
    expect(chat.updates).toEqual([{chatId: 20, itemId: line, text: "💻 Run the tests ✅ 2.0 s"}])
    await session.turn({id: id1, at: 4, step: {id: "toolu_2", text: "📖 Read src/hook.ts ✅ 41 ms", end: true}})
    await session.turn({id: id1, at: 3, step: {id: "toolu_2", text: "📖 Read src/hook.ts"}})
    expect(posted().slice(1)).toEqual([{chat: [T.ChatType.Group, 20], messages: [{msgContent: {type: "text", text: "📖 Read src/hook.ts ✅ 41 ms"}, mentions: {}, priority: -1}]}])
    expect(chat.updates).toHaveLength(1)
  })

  it("ends the live lines of steps without an end with their text at an answer, at a prompt that starts a turn, at the last detach and at the hub's stop, not at a prompt added to the running turn, and posts nothing for their later ends", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const lines = chat.nextItemId
    await session.turn({id: id1, at: 1, step: {id: "toolu_1", text: "💻 Run the tests"}})
    await session.turn({id: id1, at: 5, answer: ""})
    await until(() => chat.updates.length === 1)
    await session.turn({id: id1, at: 6, step: {id: "toolu_1", text: "💻 Run the tests ✅ 2.0 s", end: true}})
    await session.turn({id: id1, at: 7, step: {id: "toolu_2", text: "📖 Read src/hook.ts"}})
    await session.turn({id: id1, at: 8, prompt: "<task-notification>\n<task-id>b1</task-id>\n</task-notification>", typed: false, midTurn: true})
    await session.turn({id: id1, at: 9, step: {id: "toolu_3", text: "✏️ Edit src/hub.ts"}})
    await session.turn({id: id1, at: 9, prompt: "Then lint it.", typed: false})
    await until(() => chat.updates.length === 2)
    await session.turn({id: id1, at: 10, step: {id: "toolu_4", text: "🤖 Agent: Review"}})
    await session.endLive()
    await session.turn({id: id1, at: 11, step: {id: "toolu_5", text: "📝 Write rfcs/steps.md"}})
    await hub.stop()
    expect(chat.updates).toEqual([
      {chatId: 20, itemId: lines, text: "💻 Run the tests"},
      {chatId: 20, itemId: lines + 1, text: "📖 Read src/hook.ts"},
      {chatId: 20, itemId: lines + 2, text: "✏️ Edit src/hub.ts"},
      {chatId: 20, itemId: lines + 3, text: "🤖 Agent: Review"},
      {chatId: 20, itemId: lines + 4, text: "📝 Write rfcs/steps.md"},
    ])
    expect(sentTo(chat, 20)).toHaveLength(5)
  })

  it("posts the note of Claude Code's prompt and every part of an answer as silent, and the end line in small print as important", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    await session.turn({id: id1, at: 1, prompt: "<task-notification>\n<task-id>b1</task-id>\n</task-notification>", typed: false, note: '✅ Background command "Build" completed (exit code 0)'})
    await session.turn({id: id1, at: 2, answer: "a".repeat(20_000)})
    await session.turn({id: id1, at: 3, answer: "Done."})
    expect(priorities(chat)).toEqual([[-1], [-1, -1], [1], [-1], [1]])
    expect(sentTo(chat, 20).slice(2)).toEqual(["!- ✅ Done in 1 ms!", "Done.", "!- ✅ Done!"])
  })

  it("posts the end line with the turn's usage and the plan's usage of the newest status line report, leaving out a window whose reset time has passed, in small print as important after the silent answer", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const usage = "2 min 13 s · 1.5k output tokens · 535k tokens in context"
    const later = Date.now() / 1_000 + 3_600
    await session.turn({id: id1, at: 1, answer: "Fixed.", usage})
    await session.turn({id: id1, at: 3, effort: "high", limits: {fiveHour: {percent: 34.7, resetsAt: later}, week: {percent: 61, resetsAt: later}}})
    await session.turn({id: id1, at: 2, limits: {fiveHour: {percent: 99, resetsAt: later}}})
    await session.turn({id: id1, at: 4, answer: "Tests pass.", usage})
    await session.turn({id: id1, at: 5, limits: {fiveHour: {percent: 80, resetsAt: Date.now() / 1_000 - 1}, week: {percent: 62, resetsAt: later}}})
    await session.turn({id: id1, at: 6, answer: "Lint passes."})
    expect(sentTo(chat, 20)).toEqual(["Fixed.", `!- ✅ Done in ${usage}!`, "Tests pass.", `!- ✅ Done in ${usage} · 5 h: 34% · week: 61%!`, "Lint passes.", "!- ✅ Done · week: 62%!"])
    expect(priorities(chat)).toEqual([[-1], [1], [-1], [1], [-1], [1]])
  })

  it("posts a line when the model switches and when a known effort level changes, also from the status line, before the other posts of the request, and none for a first value, a model of an answer or a value older than the one taken last", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    await session.turn({id: id1, at: 1, model: "claude-opus-5[1m]"})
    await session.turn({id: id1, at: 2, answer: "Done.", effort: "high"})
    await session.turn({id: id1, at: 3, model: "claude-sonnet-5", switched: true})
    await session.turn({id: id1, at: 4, answer: "Done.", model: "claude-haiku-5"})
    await session.turn({id: id1, at: 6, effort: "max"})
    await session.turn({id: id1, at: 3.5, model: "claude-opus-5", switched: true, effort: "low"})
    await session.turn({id: id1, at: 7, answer: "Done.", effort: "low"})
    expect(sentTo(chat, 20)).toEqual(["Done.", "!- ✅ Done!", "!- Model changed to claude-sonnet-5!", "Done.", "!- ✅ Done!", "!- Effort changed to max!", "!- Effort changed to low!", "Done.", "!- ✅ Done!"])
    expect(priorities(chat)).toEqual([[-1], [1], [-1], [-1], [1], [-1], [-1], [-1], [1]])
    expect(chat.profiles.filter(({groupId}) => groupId === 20).at(-1)!.groupProfile.shortDescr).toBe("claude-haiku-5 · effort low")
  })

  it("keeps the shown model with [1m] and its description when an answer's model is that model without [1m]", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const described = () => chat.profiles.filter(({groupId}) => groupId === 20).map(({groupProfile}) => groupProfile.shortDescr)
    await session.turn({id: id1, at: 1, model: "claude-opus-5[1m]"})
    await session.turn({id: id1, at: 2, answer: "Done.", model: "claude-opus-5"})
    expect(described()).toEqual(["claude-opus-5[1m]"])
    expect(readSessionFile(sessionFolder(root, id1))?.model).toBe("claude-opus-5[1m]")
    await session.turn({id: id1, at: 3, answer: "Done.", model: "claude-sonnet-5"})
    expect(described()).toEqual(["claude-opus-5[1m]", "claude-sonnet-5"])
  })

  it("updates the line of a step at its end and at the end of the steps also while the send of its start is in flight, and sends the starts in their order", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const send = chat.apiSendMessages.bind(chat)
    const held: (() => void)[] = []
    chat.apiSendMessages = (target, messages, live) => (live === true ? new Promise(resolve => held.push(() => resolve(send(target, messages, live)))) : send(target, messages, live))
    const lines = chat.nextItemId
    const started = session.turn({id: id1, at: 1, step: {id: "toolu_1", text: "💻 Run the tests"}})
    const ended = session.turn({id: id1, at: 2, step: {id: "toolu_1", text: "💻 Run the tests ✅ 2.0 s", end: true}})
    const second = session.turn({id: id1, at: 3, step: {id: "toolu_2", text: "📖 Read src/hook.ts"}})
    const stepsEnded = session.endSteps()
    await until(() => held.length === 1)
    held[0]()
    await until(() => held.length === 2)
    held[1]()
    await Promise.all([started, ended, second, stepsEnded])
    expect(sentTo(chat, 20)).toEqual(["💻 Run the tests", "📖 Read src/hook.ts"])
    expect(chat.updates).toEqual([
      {chatId: 20, itemId: lines, text: "💻 Run the tests ✅ 2.0 s"},
      {chatId: 20, itemId: lines + 1, text: "📖 Read src/hook.ts"},
    ])
  })

  it("logs a failed update of a step's line and a failed send of its start, and posts nothing more for them", async () => {
    const errors: string[] = []
    jest.spyOn(process.stderr, "write").mockImplementation(text => (errors.push(String(text).trimEnd()), true))
    const chat = new FakeChat()
    const {session} = await channel(chat)
    chat.apiUpdateChatItem = async () => Promise.reject(new Error("invalid chat item update"))
    await session.turn({id: id1, at: 1, step: {id: "toolu_1", text: "💻 Run the tests"}})
    await session.turn({id: id1, at: 2, step: {id: "toolu_1", text: "💻 Run the tests ✅ 2.0 s", end: true}})
    const send = chat.apiSendMessages.bind(chat)
    chat.apiSendMessages = async (target, messages, live) => (live === true ? Promise.reject(new Error("network")) : send(target, messages, live))
    await session.turn({id: id1, at: 3, step: {id: "toolu_2", text: "📖 Read src/hook.ts"}})
    await session.turn({id: id1, at: 4, step: {id: "toolu_2", text: "📖 Read src/hook.ts ✅ 41 ms", end: true}})
    expect(sentTo(chat, 20)).toEqual(["💻 Run the tests"])
    expect(errors).toEqual(["steps of session 1: invalid chat item update", "steps of session 1: network"])
  })

  it("ends the live line of a step 23 hours after its start, before the core refuses its update, and posts its later end as a new silent line", async () => {
    jest.useFakeTimers({doNotFake: ["nextTick", "setImmediate", "queueMicrotask"]})
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const line = chat.nextItemId
    await session.turn({id: id1, at: 1, step: {id: "toolu_1", text: "💻 Run the tests"}})
    await jest.advanceTimersByTimeAsync(23 * 3_600_000 - 1)
    expect(chat.updates).toEqual([])
    await jest.advanceTimersByTimeAsync(1)
    expect(chat.updates).toEqual([{chatId: 20, itemId: line, text: "💻 Run the tests"}])
    await session.turn({id: id1, at: 2, step: {id: "toolu_1", text: "💻 Run the tests ✅ 25 h 0 min", end: true}})
    expect(sentTo(chat, 20)).toEqual(["💻 Run the tests", "💻 Run the tests ✅ 25 h 0 min"])
    expect(priorities(chat)).toEqual([[-1], [-1]])
    expect(chat.updates).toHaveLength(1)
  })

  it("ends a live line 23 hours after its start by the wall clock, which runs on while the machine sleeps, at the next check of the minute or at the end of its step, and posts that end as a new silent line", async () => {
    jest.useFakeTimers({doNotFake: ["nextTick", "setImmediate", "queueMicrotask"]})
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const line = chat.nextItemId
    await session.turn({id: id1, at: 1, step: {id: "toolu_1", text: "💻 Run the tests"}})
    jest.setSystemTime(Date.now() + 23 * 3_600_000)
    expect(chat.updates).toEqual([])
    await jest.advanceTimersByTimeAsync(60_000)
    expect(chat.updates).toEqual([{chatId: 20, itemId: line, text: "💻 Run the tests"}])
    await session.turn({id: id1, at: 2, step: {id: "toolu_2", text: "📖 Read src/hook.ts"}})
    jest.setSystemTime(Date.now() + 23 * 3_600_000)
    await session.turn({id: id1, at: 3, step: {id: "toolu_2", text: "📖 Read src/hook.ts ✅ 23 h 0 min", end: true}})
    expect(chat.updates).toEqual([
      {chatId: 20, itemId: line, text: "💻 Run the tests"},
      {chatId: 20, itemId: line + 1, text: "📖 Read src/hook.ts"},
    ])
    expect(sentTo(chat, 20)).toEqual(["💻 Run the tests", "📖 Read src/hook.ts", "📖 Read src/hook.ts ✅ 23 h 0 min"])
    expect(priorities(chat)).toEqual([[-1], [-1], [-1]])
  })

  it("closes the core at the hub's stop only after the updates of the lines that the end of the steps at the last detach started", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const events: string[] = []
    await session.turn({id: id1, at: 1, step: {id: "toolu_1", text: "💻 Run the tests"}})
    const update = chat.apiUpdateChatItem.bind(chat)
    const held: (() => void)[] = []
    chat.apiUpdateChatItem = (...args) => new Promise(resolve => held.push(() => resolve(update(...args).then(item => (events.push("updated"), item)))))
    chat.close = async () => void events.push("closed")
    void session.endLive()
    const stopped = hub.stop()
    await until(() => held.length === 1)
    expect(events).toEqual([])
    held[0]()
    await stopped
    expect(events).toEqual(["updated", "closed"])
  })

  it("opens an empty silent live message at a prompt that starts a turn, replying to the owner's prompt and after User's post of a typed prompt, and none for a prompt added to the running turn, a command or Claude Code's own prompt", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posted: string[] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posted.push(`User: ${parts.join("")}`)} as unknown as User
    const send = chat.apiSendMessages.bind(chat)
    chat.apiSendMessages = async (ref, messages, live) => (posted.push(`Claude${live === true ? " live" : ""}: ${messages[0].msgContent.text}`), send(ref, messages, live))
    const note = '✅ Background command "Build" completed (exit code 0)'
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build"), typed: false})
    await session.turn({id: id1, at: 2, prompt: "Also lint it", typed: true, midTurn: true})
    await session.turn({id: id1, at: 3, prompt: "/compact", typed: true})
    await session.turn({id: id1, at: 4, prompt: "<task-notification>\n<task-id>b1</task-id>\n</task-notification>", typed: false, note})
    await session.turn({id: id1, at: 5, prompt: "Run the tests", typed: true})
    expect(posted).toEqual(["Claude live: ", "User: Also lint it", `Claude: ${note}`, "User: Run the tests", "Claude live: "])
    expect(shown(chat)).toEqual([{text: "", quotedItemId: 42}, {text: note, quotedItemId: undefined}, {text: "", quotedItemId: undefined}])
    expect(priorities(chat)).toEqual([[-1], [-1], [-1]])
  })

  it("fills the opening message with the turn's first step or text, only from a hook that started after the prompt's hook", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const opening = chat.nextItemId
    await session.turn({id: id1, at: 1, prompt: "Fix the build", typed: true})
    await session.turn({id: id1, at: 2, step: {id: "toolu_1", text: "💻 Run the tests"}})
    await session.turn({id: id1, at: 3, step: {id: "toolu_1", text: "💻 Run the tests ✅ 2.0 s", end: true}})
    await session.turn({id: id1, at: 4, answer: "Fixed."})
    await session.turn({id: id1, at: 6, prompt: "Then lint it", typed: true})
    await session.turn({id: id1, at: 5, step: {id: "toolu_2", text: "🔍 Grep TODO"}})
    await session.turn({id: id1, at: 7, text: {message: "m1", index: 0, final: true, delta: "Linted."}})
    await session["liveOrder"]
    expect(shown(chat)).toEqual([{text: "💻 Run the tests ✅ 2.0 s", quotedItemId: undefined}, {text: "Fixed.", quotedItemId: undefined}, endedIn("3 ms"), {text: "Linted.", quotedItemId: undefined}, {text: "🔍 Grep TODO", quotedItemId: undefined}])
    expect(chat.updates).toEqual([
      {chatId: 20, itemId: opening, text: "💻 Run the tests", live: true},
      {chatId: 20, itemId: opening, text: "💻 Run the tests ✅ 2.0 s"},
      {chatId: 20, itemId: opening + 3, text: "Linted."},
    ])
  })

  it("streams each text of Claude into a silent live message from its first line that is not blank, in index order, updated at most every 3 s, and finalized at its final batch", async () => {
    jest.useFakeTimers({doNotFake: ["nextTick", "setImmediate", "queueMicrotask"]})
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const line = chat.nextItemId
    const batch = async (index: number, delta: string, final = false): Promise<void> => {
      await session.turn({id: id1, at: 1, text: {message: "m1", index, final, delta}})
      await session["liveOrder"]
    }
    await batch(0, "\n")
    expect(sentTo(chat, 20)).toEqual([])
    await batch(2, "Third line.\n")
    expect(sentTo(chat, 20)).toEqual([])
    await batch(1, "Second line.\n")
    expect(chat.sent.filter(({chat: [, groupId]}) => groupId === 20)).toEqual([{chat: [T.ChatType.Group, 20], messages: [{msgContent: {type: "text", text: "\nSecond line.\nThird line.\n"}, mentions: {}, priority: -1}], live: true}])
    await batch(3, "Fourth line.\n")
    await jest.advanceTimersByTimeAsync(2_999)
    expect(chat.updates).toEqual([])
    await jest.advanceTimersByTimeAsync(1)
    await batch(4, "Fifth line.\n")
    await jest.advanceTimersByTimeAsync(1_000)
    const fourLines = "\nSecond line.\nThird line.\nFourth line.\n"
    expect(chat.updates).toEqual([{chatId: 20, itemId: line, text: fourLines, live: true}])
    await batch(5, "Done.", true)
    expect(chat.updates).toEqual([
      {chatId: 20, itemId: line, text: fourLines, live: true},
      {chatId: 20, itemId: line, text: `${fourLines}Fifth line.\nDone.`},
    ])
    expect(sentTo(chat, 20)).toHaveLength(1)
  })

  it("returns from a batch of Claude's text before the hub posts it", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const send = chat.apiSendMessages.bind(chat)
    const held: (() => void)[] = []
    chat.apiSendMessages = (target, messages, live) => new Promise(resolve => held.push(() => resolve(send(target, messages, live))))
    await session.turn({id: id1, at: 1, text: {message: "m1", index: 0, final: false, delta: "Working.\n"}})
    await until(() => held.length === 1)
    expect(sentTo(chat, 20)).toEqual([])
    held[0]()
    await session["liveOrder"]
    expect(sentTo(chat, 20)).toEqual(["Working.\n"])
  })

  it("continues a text that outgrows a message in a new live message after the part that fits, for Latin, Cyrillic and CJK texts", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    for (const [message, text] of [["m1", "a".repeat(20000)], ["m2", "Ж".repeat(8000)], ["m3", "字".repeat(6000)]]) {
      await session.turn({id: id1, at: 1, text: {message, index: 0, final: false, delta: `${text}\n`}})
      await session.turn({id: id1, at: 1, text: {message, index: 1, final: true, delta: ""}})
    }
    await session["liveOrder"]
    expect(shown(chat).map(({text}) => [...text].length)).toEqual([14962, 5039, 7481, 520, 4987, 1014])
    expect(chat.sent.filter(({chat: [, groupId]}) => groupId === 20).map(({live}) => live)).toEqual([undefined, true, undefined, true, undefined, true])
    expect(chat.updates.map(({live}) => live)).toEqual([undefined, undefined, undefined])
  })

  it("finalizes the streamed answer with the Stop hook's text when it differs, posts the answer only when no text of Claude starts it, and then posts the end line in small print as important", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const text = (at: number, message: string, index: number, delta: string, final = false) => session.turn({id: id1, at, text: {message, index, final, delta}})
    await session.turn({id: id1, at: 1, prompt: "Fix the build", typed: true})
    await text(2, "m1", 0, "Fixed.", true)
    await session.turn({id: id1, at: 3, answer: "Fixed."})
    await session.turn({id: id1, at: 4, prompt: "Run the tests", typed: true})
    await text(5, "m2", 0, "Tests:\n")
    await text(5, "m2", 2, "all pass.", true)
    await session.turn({id: id1, at: 7, answer: "Tests:\n12 run\nall pass."})
    await session.turn({id: id1, at: 8, prompt: "Check the logs", typed: true})
    await text(9, "m3", 0, "Let me check.", true)
    await session.turn({id: id1, at: 10, step: {id: "toolu_1", text: "📖 Read build.log"}})
    await session.turn({id: id1, at: 11, answer: "All good."})
    await session["finalizing"]
    expect(shown(chat)).toEqual([
      {text: "Fixed.", quotedItemId: undefined},
      endedIn("2 ms"),
      {text: "Tests:\n12 run\nall pass.", quotedItemId: undefined},
      endedIn("3 ms"),
      {text: "Let me check.", quotedItemId: undefined},
      {text: "📖 Read build.log", quotedItemId: undefined},
      {text: "All good.", quotedItemId: undefined},
      endedIn("3 ms"),
    ])
    expect(priorities(chat)).toEqual([[-1], [1], [-1], [1], [-1], [-1], [-1], [1]])
  })

  it("drops the later batches of an ended text and the first batch of a text whose hook started before the last answer", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    await session.turn({id: id1, at: 1, text: {message: "m1", index: 0, final: true, delta: "Done."}})
    await session.turn({id: id1, at: 3, answer: "Done."})
    await session.turn({id: id1, at: 1, text: {message: "m1", index: 1, final: false, delta: "Late.\n"}})
    await session.turn({id: id1, at: 2, text: {message: "m2", index: 0, final: true, delta: "Late too."}})
    await session["liveOrder"]
    expect(sentTo(chat, 20)).toEqual(["Done.", "!- ✅ Done!"])
    expect(chat.updates).toEqual([])
  })

  it("ends the live texts with their text at a Stop, at a prompt that starts a turn and at the last detach, and an empty opening message with ⏹️ Stopped at the Stop of a later turn and at the hub's stop", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    await session.turn({id: id1, at: 1, prompt: "Fix the build", typed: true})
    await session.turn({id: id1, at: 2, prompt: "Fix it now", typed: true})
    await session.turn({id: id1, at: 3, text: {message: "m1", index: 0, final: false, delta: "Working on it.\n"}})
    await session.turn({id: id1, at: 4, answer: "Done."})
    await session.turn({id: id1, at: 5, prompt: "Run the tests", typed: true})
    await session.turn({id: id1, at: 6, text: {message: "m2", index: 0, final: false, delta: "Half a line\n"}})
    await session.turn({id: id1, at: 7, prompt: "Lint it", typed: true})
    await session.turn({id: id1, at: 8, text: {message: "m3", index: 0, final: false, delta: "Linting\n"}})
    await session["liveOrder"]
    await session.endLive()
    await session.turn({id: id1, at: 9, prompt: "Format it", typed: true})
    await hub.stop()
    expect(shown(chat)).toEqual([
      {text: "⏹️ Stopped", quotedItemId: undefined},
      {text: "Working on it.\n", quotedItemId: undefined},
      {text: "Done.", quotedItemId: undefined},
      endedIn("2 ms"),
      {text: "Half a line\n", quotedItemId: undefined},
      {text: "Linting\n", quotedItemId: undefined},
      {text: "⏹️ Stopped", quotedItemId: undefined},
    ])
    expect(chat.updates.filter(({live}) => live !== true).map(({text}) => text)).toEqual(["Working on it.\n", "⏹️ Stopped", "Half a line\n", "Linting\n", "⏹️ Stopped"])
  })

  it("splits an answer and a terminal prompt where a message reaches the size the core accepts, with the quote in the first part of an answer, for Latin, Cyrillic and CJK texts", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posts: string[][] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posts.push(parts)} as unknown as User
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build")})
    await session.turn({id: id1, at: 2, answer: "a".repeat(20000)})
    await session.turn({id: id1, at: 3, answer: "字".repeat(6000)})
    await session.turn({id: id1, at: 4, prompt: "Ж".repeat(8000)})
    const sizes = shown(chat).map(({text, quotedItemId}) => (text.startsWith("!- ✅") ? text : [[...text].length, quotedItemId]))
    expect(sizes).toEqual([[14928, 42], [5072, undefined], "!- ✅ Done in 1 ms!", [4987, undefined], [1013, undefined], "!- ✅ Done!", [0, undefined]])
    expect(posts.map(parts => parts.map(part => part.length))).toEqual([[7481, 519]])
  })

  it("measures each part of a long answer within one message size from the start of the part", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const measure = jest.spyOn(tools, "fitsMessage")
    await session.turn({id: id1, answer: "a".repeat(100_000)})
    expect(Math.max(...measure.mock.calls.map(([content]) => content.text.length))).toBeLessThanOrEqual(15_602)
    expect(answers(chat)[0].map(({text}) => text.length)).toEqual([14_962, 14_962, 14_962, 14_962, 14_962, 14_962, 10_228])
  })

  it("answers without the quote when the quote leaves no room for the first character of the answer", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    await ownerPrompt(session, 42, "a".repeat(14941))
    await session.turn({id: id1, at: 2, answer: "Done."})
    await ownerPrompt(session, 43, "a".repeat(14940))
    await session.turn({id: id1, at: 4, answer: "Done."})
    expect(answers(chat)).toEqual([[{text: "Done.", quotedItemId: undefined}], [done], [{text: "D", quotedItemId: 43}, {text: "one.", quotedItemId: undefined}], [done]])
  })

  it("quotes in each answer the prompt of its turn when an owner sends prompts during a turn, and the oldest prompt without a prompt hook", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build")})
    await ownerPrompt(session, 43, "Then run the tests")
    await session.turn({id: id1, at: 2, answer: "Fixed."})
    await session.turn({id: id1, at: 3, prompt: tag(43, "Then run the tests")})
    await session.turn({id: id1, at: 4, answer: "Tests pass."})
    await ownerPrompt(session, 44, "Deploy")
    await ownerPrompt(session, 45, "and tag it")
    await session.turn({id: id1, at: 5, prompt: `${tag(44, "Deploy")}\n${tag(45, "and tag it")}`})
    await session.turn({id: id1, at: 6, answer: "Deployed and tagged."})
    await ownerPrompt(session, 46, "Status?")
    await session.turn({id: id1, at: 7, answer: "Green."})
    expect(shown(chat)).toEqual([{text: "Fixed.", quotedItemId: 42}, endedIn("1 ms"), {text: "Tests pass.", quotedItemId: 43}, endedIn("1 ms"), {text: "Deployed and tagged.", quotedItemId: 44}, endedIn("1 ms"), {text: "Green.", quotedItemId: 46}, done])
  })

  it("quotes in each answer the prompt of its turn by the start of the hooks, when the Stop hook of a turn arrives after or during the prompt hook of the next turn", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build")})
    await ownerPrompt(session, 43, "Then run the tests")
    await session.turn({id: id1, at: 3, prompt: tag(43, "Then run the tests")})
    await session.turn({id: id1, at: 2, answer: "Fixed."})
    await session.turn({id: id1, at: 4, answer: "Tests pass."})
    await ownerPrompt(session, 44, "Deploy")
    await session.turn({id: id1, at: 5, prompt: tag(44, "Deploy")})
    await ownerPrompt(session, 45, "Tag it")
    await Promise.all([session.turn({id: id1, at: 6, answer: "Deployed.", effort: "high"}), session.turn({id: id1, at: 7, prompt: tag(45, "Tag it")})])
    await session.turn({id: id1, at: 8, answer: "Tagged."})
    expect(shown(chat)).toEqual([
      {text: "Fixed.", quotedItemId: 42},
      {text: "Tests pass.", quotedItemId: 43},
      endedIn("1 ms"),
      endedIn("1 ms"),
      {text: "Deployed.", quotedItemId: 44},
      {text: "Tagged.", quotedItemId: 45},
      endedIn("1 ms"),
      endedIn("1 ms"),
    ])
  })

  it("ends the turn of a prompt with an empty answer in its opening message, without an end line, so that a later answer without a prompt hook quotes its own prompt", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build")})
    await session.turn({id: id1, at: 2, answer: ""})
    await ownerPrompt(session, 43, "Status?")
    await session.turn({id: id1, at: 3, answer: "Green."})
    expect(shown(chat)).toEqual([{text: "!- ✅ Done in 1 ms!", quotedItemId: 42}, {text: "Green.", quotedItemId: 43}, done])
    expect(priorities(chat)).toEqual([[-1], [-1], [1]])
  })

  it("does not post an owner's prompt again as User when the Stop request of its turn is handled before its prompt request", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posts: string[] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posts.push(...parts)} as unknown as User
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 2, answer: "Fixed."})
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build")})
    await ownerPrompt(session, 43, "Then run the tests")
    await session.turn({id: id1, at: 3, prompt: tag(43, "Then run the tests")})
    await session.turn({id: id1, at: 4, answer: "Tests pass."})
    expect(shown(chat)).toEqual([{text: "Fixed.", quotedItemId: 42}, done, {text: "Tests pass.", quotedItemId: 43}, endedIn("1 ms")])
    expect(posts).toEqual([])
  })

  it("quotes the prompt of each turn when two turns in a row have their Stop request handled before their prompt request", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posts: string[] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posts.push(...parts)} as unknown as User
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 2, answer: "Fixed."})
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build")})
    await ownerPrompt(session, 43, "Then run the tests")
    await session.turn({id: id1, at: 4, answer: "Tests pass."})
    await session.turn({id: id1, at: 3, prompt: tag(43, "Then run the tests")})
    await session.turn({id: id1, at: 6, answer: "Renamed."})
    await session.turn({id: id1, at: 5, prompt: "Rename it"})
    expect(shown(chat)).toEqual([{text: "Fixed.", quotedItemId: 42}, done, {text: "Tests pass.", quotedItemId: 43}, done, {text: "Renamed.", quotedItemId: undefined}, done])
    expect(posts).toEqual(["Rename it"])
  })

  it("posts no prompt made only of this channel's tags as User, as after a service restart lost the pushed prompt, and posts one with other text", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posts: string[] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posts.push(...parts)} as unknown as User
    await session.turn({id: id1, at: 1, prompt: `${tag(42, "Fix the build")}\n${tag(43, "and test it")}\n`})
    await session.turn({id: id1, at: 2, prompt: `Also see:\n${tag(44, "the log")}`})
    expect(posts).toEqual([`Also see:\n${tag(44, "the log")}`])
  })

  it("updates the description of a new effort level also when its answer is not posted", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    chat.apiSendMessages = async () => {
      throw new Error("send failed")
    }
    await expect(session.turn({id: id1, at: 1, answer: "Fixed.", effort: "high"})).rejects.toThrow("send failed")
    expect(chat.profiles.at(-1)!.groupProfile.shortDescr).toBe("effort high")
  })

  it("replies that the file of a prompt was not received without the quote when the core refuses to quote the prompt", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const send = chat.apiSendMessages.bind(chat)
    chat.apiSendMessages = async (ref, messages) => {
      if (messages.some(({quotedItemId}) => quotedItemId !== undefined)) throw new core.ChatAPIError("Chat command error", {type: "errorStore", storeError: {type: "chatItemNotFound"}} as unknown as T.ChatError)
      return send(ref, messages)
    }
    chat.apiSendTextReply = async () => {
      throw new core.ChatAPIError("Chat command error", {type: "errorStore", storeError: {type: "chatItemNotFound"}} as unknown as T.ChatError)
    }
    session.onMessage(event(sessionGroup, fileItem(43, {type: "file", text: "See the log"}, {type: "rcvInvitation"}, {chatDir: from(invitedAlice), fileId: 8})))
    await until(() => answers(chat).length === 1)
    expect(answers(chat)).toEqual([[{text: "The file of this message was not received: SimpleX error: unexpected response cmdOk", quotedItemId: undefined}]])
  })

  it("posts the opening message of a turn, the answer and the replies without the quote when the core refuses to quote the prompt, as after the owner deleted it", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const refused = (type: string) => new core.ChatAPIError("Chat command error", {type: "errorStore", storeError: {type}} as unknown as T.ChatError)
    const send = chat.apiSendMessages.bind(chat)
    chat.apiSendMessages = async (ref, messages, live) => {
      if (messages.some(({quotedItemId}) => quotedItemId !== undefined)) throw refused(messages[0].msgContent.text === "Fixed." ? "invalidQuote" : "chatItemNotFound")
      return send(ref, messages, live)
    }
    chat.apiSendTextReply = async () => {
      throw refused("chatItemNotFound")
    }
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build")})
    await session.turn({id: id1, at: 2, answer: "Fixed."})
    await ownerPrompt(session, 43, "Run the tests")
    await session.turn({id: id1, at: 4, answer: "Tests pass."})
    session.onLine = undefined
    session.onMessage(event(sessionGroup, item(44, "Lint it", from(invitedAlice))))
    await until(() => shown(chat).length === 5)
    expect(shown(chat)).toEqual([{text: "Fixed.", quotedItemId: undefined}, endedIn("1 ms"), {text: "Tests pass.", quotedItemId: undefined}, done, {text: "Session 1 is not running.", quotedItemId: undefined}])
    expect(chat.sent.find(({chat: [, groupId]}) => groupId === 20)?.live).toBe(true)
  })

  it("posts the replies to a member's message without the quote when the core refuses them as too large with it", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const send = chat.apiSendMessages.bind(chat)
    chat.apiSendMessages = async (ref, messages) => {
      if (messages.some(({quotedItemId}) => quotedItemId !== undefined)) throw new core.ChatAPIError("Chat command error", {type: "errorStore", storeError: {type: "largeMsg"}} as unknown as T.ChatError)
      return send(ref, messages)
    }
    session.onLine = undefined
    session.onMessage(event(sessionGroup, item(43, "a".repeat(15_400), from(invitedAlice))))
    session.onMessage(event(sessionGroup, item(44, "/pending", from(member(13, "Mia", T.GroupMemberRole.Member)))))
    await until(() => answers(chat).length === 2)
    expect(answers(chat)).toEqual([[{text: "Session 1 is not running.", quotedItemId: undefined}], [{text: commandsApproversOnly, quotedItemId: undefined}]])
  })

  it("posts an answer with a new effort level in the opening message of its turn, before the prompt of the next turn, and then the description", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posted: string[] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posted.push(`User: ${parts.join("")}`)} as unknown as User
    const send = chat.apiSendMessages.bind(chat)
    chat.apiSendMessages = async (ref, messages, live) => {
      posted.push(`Claude: ${messages[0].msgContent.text}`)
      return send(ref, messages, live)
    }
    await session.turn({id: id1, at: 1, prompt: "Fix the build"})
    await Promise.all([session.turn({id: id1, at: 2, answer: "Fixed.", effort: "high"}), session.turn({id: id1, at: 3, prompt: "Run the tests"})])
    expect(posted.slice(0, 3)).toEqual(["User: Fix the build", "Claude: ", "User: Run the tests"])
    expect(shown(chat)).toEqual(expect.arrayContaining([{text: "Fixed.", quotedItemId: undefined}, endedIn("1 ms"), {text: "", quotedItemId: undefined}]))
    expect(shown(chat)[0]).toEqual({text: "Fixed.", quotedItemId: undefined})
    expect(chat.profiles.at(-1)!.groupProfile.shortDescr).toBe("effort high")
  })

  it("posts no empty terminal prompt as User, as Claude Code sends for a prompt with images only", async () => {
    const chat = new FakeChat()
    const {hub, session} = await channel(chat)
    const posts: string[] = []
    hub.user = {post: async (_n: number, parts: string[]) => void posts.push(...parts)} as unknown as User
    await session.turn({id: id1, at: 1, prompt: ""})
    await session.turn({id: id1, at: 2, prompt: " \n"})
    expect(posts).toEqual([])
  })

  it("delivers an owner's prompts in their order after a prompt with a file, and replies that the session is not running when it detaches during the transfer", async () => {
    const chat = new FakeChat()
    const {hub, session, lines} = await channel(chat)
    const offer = (n: number, fileId: number): T.ChatItem => {
      const offered = fileItem(n, {type: "file", text: "See the log"}, {type: "rcvInvitation"}, {chatDir: from(invitedAlice), fileId})
      chat.responses.set(`/freceive ${fileId} approved_relays=on ${join(sessionFolder(root, id1), "files", "group", `${fileId}.pdf`)}`, {type: "rcvFileAccepted", user, chatItem: event(sessionGroup, offered)})
      session.onMessage(event(sessionGroup, offered))
      return offered
    }
    const first = offer(43, 8)
    session.onMessage(event(sessionGroup, item(44, "and fix it", from(invitedAlice))))
    await until(() => chat.commands.some(command => command.startsWith("/freceive 8 ")))
    expect(lines).toEqual([])
    await hub["onEvent"]({type: "rcvFileComplete", user, chatItem: event(sessionGroup, first)} as unknown as ChatEvent)
    await until(() => lines.length === 2)
    expect(lines).toEqual([{prompt: {text: "See the log", from: "Alice", item: 43, file: join(sessionFolder(root, id1), "files", "group", "8.pdf")}}, {prompt: {text: "and fix it", from: "Alice", item: 44}}])
    const second = offer(45, 9)
    await until(() => chat.commands.some(command => command.startsWith("/freceive 9 ")))
    session.onLine = undefined
    await hub["onEvent"]({type: "rcvFileComplete", user, chatItem: event(sessionGroup, second)} as unknown as ChatEvent)
    await until(() => quotingPosts(chat).length === 1)
    expect(answers(chat)).toEqual([[{text: "Session 1 is not running.", quotedItemId: 45}]])
    expect(lines).toHaveLength(2)
  })

  it("ends at each prompt and answer only the tool-use cards created before its hook started", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const permission = (requestId: string) => session.permission({id: id1, requestId, toolName: "Bash", description: "Run shell command", inputPreview: "ls"})
    permission("abcde")
    await new Promise(resolve => setTimeout(resolve, 2))
    const at = Date.now()
    permission("fghij")
    await until(() => sentTo(chat, 20).length === 2)
    await session.turn({id: id1, at, answer: ""})
    await until(() => chat.updates.length === 1)
    expect(session.actions.pending().map(({n}) => n)).toEqual([2])
    await new Promise(resolve => setTimeout(resolve, 2))
    await session.turn({id: id1, at: Date.now(), prompt: "Next"})
    await until(() => chat.updates.length === 2)
    expect(session.actions.pending()).toEqual([])
    expect(chat.updates.map(({text}) => text.split("\n").at(-1))).toEqual(["Ended.", "Ended."])
  })

  it("passes the text of an owner's prompt whose file was not received, and says so in the group", async () => {
    const chat = new FakeChat()
    const {session, lines} = await channel(chat)
    session.onMessage(event(sessionGroup, fileItem(43, {type: "file", text: "See the log"}, {type: "rcvInvitation"}, {chatDir: from(invitedAlice), fileId: 8})))
    await until(() => lines.length === 1)
    expect(lines).toEqual([{prompt: {text: "See the log", from: "Alice", item: 43}}])
    expect(answers(chat)).toEqual([[{text: "The file of this message was not received: SimpleX error: unexpected response cmdOk", quotedItemId: 43}]])
  })

  it("passes an owner's prompt without its file when the file does not arrive in 10 minutes", async () => {
    jest.useFakeTimers({doNotFake: ["nextTick", "setImmediate", "queueMicrotask"]})
    const chat = new FakeChat()
    const {session, lines} = await channel(chat)
    const offered = fileItem(43, {type: "file", text: "See the log"}, {type: "rcvInvitation"}, {chatDir: from(invitedAlice), fileId: 8})
    const path = join(sessionFolder(root, id1), "files", "group", "8.pdf")
    chat.responses.set(`/freceive 8 approved_relays=on ${path}`, {type: "rcvFileAccepted", user, chatItem: event(sessionGroup, offered)})
    session.onMessage(event(sessionGroup, offered))
    await jest.advanceTimersByTimeAsync(10 * 60_000 - 1)
    expect(lines).toEqual([])
    await jest.advanceTimersByTimeAsync(1)
    expect(lines).toEqual([{prompt: {text: "See the log", from: "Alice", item: 43}}])
    expect(answers(chat)).toEqual([[{text: "The file of this message was not received: not received in 10 minutes", quotedItemId: 43}]])
  })

  it("holds an owner's prompt after the service start until the session attaches, and replies that it is not running after 30 s without an attachment", async () => {
    jest.useFakeTimers({doNotFake: ["nextTick", "setImmediate", "queueMicrotask"]})
    const chat = new FakeChat()
    const {hub, session, lines} = await channel(chat)
    session.onLine = undefined
    hub.promptsHeldUntil = Date.now() + 30_000
    session.onMessage(event(sessionGroup, item(42, "Fix the build", from(invitedAlice))))
    await jest.advanceTimersByTimeAsync(10_000)
    expect(quotingPosts(chat)).toEqual([])
    session.onLine = line => lines.push(line)
    await jest.advanceTimersByTimeAsync(0)
    expect(lines).toEqual([{prompt: {text: "Fix the build", from: "Alice", item: 42}}])
    session.onLine = undefined
    session.onMessage(event(sessionGroup, item(43, "Run the tests", from(invitedAlice))))
    await jest.advanceTimersByTimeAsync(19_999)
    expect(quotingPosts(chat)).toEqual([])
    await jest.advanceTimersByTimeAsync(1)
    expect(quotingPosts(chat)).toEqual(["Session 1 is not running."])
    session.onMessage(event(sessionGroup, item(44, "Status?", from(invitedAlice))))
    await jest.advanceTimersByTimeAsync(0)
    expect(quotingPosts(chat)).toEqual(["Session 1 is not running.", "Session 1 is not running."])
    expect(lines).toHaveLength(1)
  })

  it("answers every one-line / message of admins and owners as a command of the group, //help and /!ls included, refuses every / command of members and moderators with one reply, and passes all other messages as prompts, ! messages and multi-line messages included", async () => {
    const chat = new FakeChat()
    const {session, lines} = await channel(chat)
    const dan = member(12, "Dan", T.GroupMemberRole.Admin)
    const mia = member(13, "Mia", T.GroupMemberRole.Member)
    const max = member(14, "Max", T.GroupMemberRole.Moderator)
    chat.members.set(20, [self, invitedAlice, dan, mia, max])
    await session.control.load()
    session.onMessage(event(sessionGroup, item(40, "/model sonnet", from(invitedAlice))))
    session.onMessage(event(sessionGroup, item(41, "//help", from(dan))))
    session.onMessage(event(sessionGroup, item(42, "/!ls", from(dan))))
    session.onMessage(event(sessionGroup, item(43, "/!git status", from(mia))))
    session.onMessage(event(sessionGroup, item(44, "//compact", from(max))))
    session.onMessage(event(sessionGroup, item(45, "!git status", from(invitedAlice))))
    session.onMessage(event(sessionGroup, item(46, "/approve x", from(dan))))
    session.onMessage(event(sessionGroup, item(47, "/pending", from(mia))))
    session.onMessage(event(sessionGroup, item(48, "/approve 1", from(max))))
    session.onMessage(event(sessionGroup, item(49, "/model sonnet", from(mia))))
    session.onMessage(event(sessionGroup, item(50, "/tmp/build.log fails\nfix it", from(mia))))
    session.onMessage(event(sessionGroup, item(51, "!git status", from(mia))))
    session.onMessage(event(sessionGroup, item(52, "//compact\nthen go on", from(mia))))
    session.onMessage(event(sessionGroup, item(53, "//compact\nthen go on", from(dan))))
    session.onMessage(event(sessionGroup, item(54, "/!ls\nthen go on", from(dan))))
    await session["delivery"]
    await session.control["ownerCommands"]
    expect(lines).toEqual([
      {prompt: {text: "!git status", from: "Alice", item: 45}},
      {prompt: {text: "/tmp/build.log fails\nfix it", from: "Mia", item: 50}},
      {prompt: {text: "!git status", from: "Mia", item: 51}},
      {prompt: {text: "//compact\nthen go on", from: "Mia", item: 52}},
      {prompt: {text: "//compact\nthen go on", from: "Dan", item: 53}},
      {prompt: {text: "/!ls\nthen go on", from: "Dan", item: 54}},
    ])
    expect(chat.replies).toEqual([unknownCommand, unknownCommand, unknownCommand, unknownCommand])
    expect(answers(chat)).toEqual([43, 44, 47, 48, 49].map(quotedItemId => [{text: commandsApproversOnly, quotedItemId}]))
  })

  it("answers nothing and performs nothing for a / command of a member that reaches the commands past the one refusal", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const mia = member(13, "Mia", T.GroupMemberRole.Member)
    chat.members.set(20, [self, invitedAlice, mia])
    await session.control.load()
    session.permission({id: id1, requestId: "abcde", toolName: "Bash", description: "Run shell command", inputPreview: "ls"})
    await until(() => session.actions.get(1)?.card !== undefined)
    expect(await session.control.ownerCommand(mia, {keyword: "approve", params: "1"})).toBeUndefined()
    expect(session.actions.get(1)?.state).toBe("pending")
  })

  it("answers nothing to a part of a terminal prompt that User posts as one line starting with /, and answers the next command", async () => {
    const chat = new FakeChat()
    const {hub, session, lines} = await channel(chat)
    hub.userContactId = 6
    session.onMessage(event(sessionGroup, item(43, "/9j/4AAQSkZJRgABAQ", from({...member(1006, "User"), memberContactId: 6}))))
    session.onMessage(event(sessionGroup, item(44, "/pending", from(invitedAlice))))
    await until(() => chat.replies.includes("No pending actions."))
    expect(chat.replies).toEqual(["No pending actions."])
    expect(lines).toEqual([])
  })

  it("sends files to the session group with reply at once, without a card, under the refused-path rules", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    chat.members.set(20, [self, invitedAlice, mallory])
    await session.control.load()
    const received = join(sessionFolder(root, id1), "files", "group", "8.txt")
    mkdirSync(join(sessionFolder(root, id1), "files", "group"), {recursive: true})
    writeFileSync(received, "log")
    writeFileSync(join(root, "token"), "secret\n")
    expect((await session.call("reply", {file: received, text: "The log"}, signal().call))!.structuredContent).toEqual({item: 500})
    expect(chat.sent).toEqual([{chat: [T.ChatType.Group, 20], messages: [{fileSource: {filePath: received}, msgContent: {type: "file", text: "The log"}, mentions: {}}]}])
    expect(session.actions.all()).toEqual([])
    expect(textOf(await session.call("reply", {file: join(root, "token")}, signal().call))).toMatch(/ is refused: private keys and the server's own files are not sent\.$/)
  })

  it("answers with the group preference that refuses a file of reply or its caption, and returns the core's other errors", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const received = join(sessionFolder(root, id1), "files", "group", "8.txt")
    mkdirSync(join(sessionFolder(root, id1), "files", "group"), {recursive: true})
    writeFileSync(received, "log")
    const refusal = (message: string) => async () => {
      throw new core.ChatAPIError("Chat command error", {type: "error", errorType: {type: "commandError", message}} as unknown as T.ChatError)
    }
    chat.apiSendMessages = refusal("feature not allowed Files and media")
    const result = await session.call("reply", {file: received}, signal().call)
    expect(result!.isError).toBe(true)
    expect(textOf(result)).toBe("Files and media are off in the group of this session; its owners can turn them on in the group's preferences.")
    expect(session.actions.all()).toEqual([])
    chat.apiSendMessages = refusal("feature not allowed SimpleX links")
    expect(textOf(await session.call("reply", {file: received, text: "https://simplex.chat/contact#/?v=2-7&smp=smp%3A%2F%2Fx"}, signal().call))).toBe("SimpleX links are off in the group of this session; its owners can turn them on in the group's preferences.")
    chat.apiSendMessages = refusal("not a member")
    expect(textOf(await session.call("reply", {file: received}, signal().call))).toMatch(/not a member/)
  })

  it("sends an image by reply at once with its caption and preview, as send does", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const map = join(sessionFolder(root, id1), "files", "group", "map.gif")
    mkdirSync(join(sessionFolder(root, id1), "files", "group"), {recursive: true})
    writeFileSync(map, "GIF89a")
    const preview = expect.stringMatching(/^data:image\/png;base64,/)
    expect((await session.call("reply", {file: map, text: "The trail map"}, signal().call))!.structuredContent).toEqual({item: 500})
    expect(chat.sent).toEqual([{chat: [T.ChatType.Group, 20], messages: [{fileSource: {filePath: map}, msgContent: {type: "image", text: "The trail map", image: preview}, mentions: {}}]}])
  })

  it("refuses a reply whose caption is too long for a message with its file's preview, as send does, before its post", async () => {
    jest.spyOn(core, "resizeImage").mockResolvedValue(`data:image/jpg;base64,${"A".repeat(13_978)}`)
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const photo = join(sessionFolder(root, id1), "files", "group", "photo.jpg")
    mkdirSync(join(sessionFolder(root, id1), "files", "group"), {recursive: true})
    writeFileSync(photo, "photo")
    expect(textOf(await session.call("reply", {file: photo, text: "x".repeat(1100)}, signal().call))).toBe("text: too long for a message with this image; shorten it.")
    expect(chat.sent).toEqual([])
  })

  it("has no commands for files of reply: /approve N all and a bare /revoke are unknown in a session group", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    session.permission({id: id1, requestId: "abcde", toolName: "Bash", description: "Run shell command", inputPreview: "ls"})
    await until(() => session.actions.get(1)?.card !== undefined)
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "approve", params: "1 all"})).toBe(unknownCommand)
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "revoke", params: ""})).toBe(unknownCommand)
    expect(session.actions.get(1)?.state).toBe("pending")
  })

  describe("while files and media are off in the session group", () => {
    const groupPosts = (chat: FakeChat) => chat.sent.filter(({chat: [kind, id]}) => kind === T.ChatType.Group && id === 20).map(({messages: [{msgContent}]}) => msgContent)
    const peerPosts = (chat: FakeChat) => chat.sent.filter(({chat: [kind, id]}) => kind === T.ChatType.Direct && id === 4).map(({messages: [{msgContent}]}) => msgContent.type)

    function filesOff(chat: FakeChat): T.MsgContent[] {
      const send = chat.apiSendMessages.bind(chat)
      chat.apiSendMessages = async (target, messages) => {
        const media = messages.some(({fileSource, msgContent}) => fileSource !== undefined || ["image", "video", "file"].includes(msgContent.type))
        if (target[0] === T.ChatType.Group && target[1] === 20 && media) throw new core.ChatAPIError("Chat command error", {type: "error", errorType: {type: "commandError", message: "feature not allowed Files and media"}} as unknown as T.ChatError)
        return send(target, messages)
      }
      const edits: T.MsgContent[] = []
      const update = chat.apiUpdateChatItem.bind(chat)
      chat.apiUpdateChatItem = async (type, chatId, itemId, msgContent) => (edits.push(msgContent), update(type, chatId, itemId, msgContent))
      return edits
    }

    async function sendSetup(name: string): Promise<{chat: FakeChat; session: Session; edits: T.MsgContent[]; file: string}> {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      chat.chats.set("@4", {chatInfo: ownedContact(4, "SimpleX Directory", id1), items: []})
      const edits = filesOff(chat)
      mkdirSync(join(sessionFolder(root, id1), "files", "group"), {recursive: true})
      const file = join(sessionFolder(root, id1), "files", "group", name)
      writeFileSync(file, "GIF89a")
      return {chat, session, edits, file}
    }

    it.each([["map.gif", "image"], ["trail.mp4", "video"]])("posts the card of a send of %s as text, sends the file after the approval, and edits the card as text", async (name, type) => {
      const {chat, session, edits, file} = await sendSetup(name)
      const sending = session.call("send", {chat: "@4", text: "The trail map", file, wait: 0}, signal().call)
      await until(() => session.actions.get(1)?.card !== undefined)
      expect(groupPosts(chat)).toEqual([{type: "text", text: expect.stringMatching(/^I want to send a message to bot SimpleX Directory:\n/)}])
      expect(await session.control.ownerCommand(invitedAlice, {keyword: "approve", params: "@4 1"})).toBeUndefined()
      expect((await sending)!.structuredContent).toMatchObject({status: "done", chat: "@4"})
      expect(peerPosts(chat)).toEqual([type])
      await until(() => edits.length === 1)
      expect(edits).toEqual([{type: "text", text: expect.stringMatching(/\nApproved by Alice\.$/)}])
    })

    it.each([["map.gif", "image"], ["trail.mp4", "video"]])("posts the outcome card of a send of %s in mode host as text", async (name, type) => {
      const {chat, session, file} = await sendSetup(name)
      session.actions.mode = "host"
      expect((await session.call("send", {chat: "@4", text: "The trail map", file, wait: 0}, signal().call))!.structuredContent).toMatchObject({status: "done", chat: "@4"})
      expect(peerPosts(chat)).toEqual([type])
      await until(() => groupPosts(chat).length === 1)
      expect(groupPosts(chat)).toEqual([{type: "text", text: expect.stringMatching(/^I sent a message to bot SimpleX Directory:\n/)}])
    })

    const download = (session: Session, name: string, type: "image" | "video" | "file") =>
      session.actions.create("d", {tool: "download", input: {chat: "@4", item: 30}, subject: {kind: "bot", name: "SimpleX Directory", incognito: false}, chat: "@4", item: 30, from: "SimpleX Directory", file: {fileId: 7, name, size: 2400, type, preview: "data:image/jpg;base64,/9j/small"}}, undefined, 1, "requests")

    it.each([["map.jpg", "image"], ["trail.mp4", "video"]] as const)("posts the card of a download of %s as text and edits it as text", async (name, type) => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      const edits = filesOff(chat)
      const action = download(session, name, type)
      await until(() => action.card !== undefined)
      expect(groupPosts(chat)).toEqual([{type: "text", text: expect.stringMatching(/^I want to download a file /)}])
      session.actions.reject(action, "Alice")
      await until(() => edits.length === 1)
      expect(edits).toEqual([{type: "text", text: expect.stringMatching(/\nRejected by Alice\.$/)}])
    })

    it("answers a /forward whose file the core refuses that files and media are off, and deletes the copy", async () => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      filesOff(chat)
      const action = download(session, "report.pdf", "file")
      await until(() => action.card !== undefined)
      const copy = join(sessionFolder(root, id1), "files", "sent", "copy", "report.pdf")
      mkdirSync(join(sessionFolder(root, id1), "files", "sent", "copy"), {recursive: true})
      writeFileSync(copy, "report")
      session.tools.forwardFile = async () => copy
      expect(await session.control.ownerCommand(invitedAlice, {keyword: "forward", params: "@4 1"})).toBeUndefined()
      await until(() => groupPosts(chat).length === 2)
      expect(groupPosts(chat)[1]).toEqual({type: "text", text: "Action 1: Files and media are off in the group of this session; its owners can turn them on in the group's preferences."})
      expect(existsSync(copy)).toBe(false)
      expect(action.state).toBe("pending")
    })
  })

  describe("while a preference of the session group refuses a post", () => {
    const groupPosts = (chat: FakeChat) => chat.sent.filter(({chat: [kind, id]}) => kind === T.ChatType.Group && id === 20).map(({messages: [{msgContent}], messages}) => ({text: messages.map(({msgContent: {text}}) => text).join("\n"), type: msgContent.type}))
    const link = "https://simplex.chat/contact#/?v=2-7&smp=smp%3A%2F%2Fx"
    const refusal = (feature: string) => new core.ChatAPIError("Chat command error", {type: "error", errorType: {type: "commandError", message: `feature not allowed ${feature}`}} as unknown as T.ChatError)
    const off = (feature: string) => `${feature} are off in the group of this session; its owners can turn them on in the group's preferences.`

    function refuseInGroup(chat: FakeChat): void {
      const send = chat.apiSendMessages.bind(chat)
      const update = chat.apiUpdateChatItem.bind(chat)
      chat.apiSendMessages = async (target, messages, live) => {
        if (target[0] === T.ChatType.Group && target[1] === 20 && messages.some(({msgContent}) => msgContent.text.includes(link))) throw refusal("SimpleX links")
        if (target[0] === T.ChatType.Group && target[1] === 20 && messages.some(({msgContent}) => msgContent.type === "voice")) throw refusal("Voice messages")
        return send(target, messages, live)
      }
      chat.apiUpdateChatItem = async (type, chatId, itemId, msgContent, live) => {
        if (chatId === 20 && msgContent.text.includes(link)) throw refusal("SimpleX links")
        return update(type, chatId, itemId, msgContent, live)
      }
    }

    function failDirectSends(chat: FakeChat): void {
      const send = chat.apiSendMessages.bind(chat)
      chat.apiSendMessages = async (target, messages, live) => {
        if (target[0] === T.ChatType.Direct) throw new Error("contact not ready")
        return send(target, messages, live)
      }
    }

    it("posts that Claude's answer was not posted, naming the preference, and the end line of the turn", async () => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      refuseInGroup(chat)
      await session.turn({id: id1, answer: `The bot's address is ${link}`})
      expect(groupPosts(chat)).toEqual([{type: "text", text: `Claude's answer was not posted: ${off("SimpleX links")}`}, {type: "text", text: "!- ✅ Done!"}])
    })

    it("keeps the first part of a long answer in the opening message when the preference refuses a later part", async () => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      refuseInGroup(chat)
      const opening = chat.nextItemId
      const first = "a".repeat(14962)
      await session.turn({id: id1, at: 1, prompt: "Find the bot"})
      await session.turn({id: id1, at: 2, answer: `${first}See ${link}`})
      await session["finalizing"]
      expect(chat.updates).toEqual([{chatId: 20, itemId: opening, text: first}])
      expect(groupPosts(chat)).toEqual([
        {type: "text", text: ""},
        {type: "text", text: `Claude's answer was not posted: ${off("SimpleX links")}`},
        {type: "text", text: "!- ✅ Done in 1 ms!"},
      ])
    })

    it("posts that Claude's text was not posted, naming the preference, once for a text, and finalizes its message with the text accepted last", async () => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      refuseInGroup(chat)
      const line = chat.nextItemId
      await session.turn({id: id1, at: 1, text: {message: "m1", index: 0, final: false, delta: "Checking the bot.\n"}})
      await session.turn({id: id1, at: 2, text: {message: "m1", index: 1, final: false, delta: `Its address is ${link}\n`}})
      await session.turn({id: id1, at: 3, text: {message: "m1", index: 2, final: true, delta: "Done."}})
      await session.turn({id: id1, at: 4, text: {message: "m2", index: 0, final: true, delta: `See ${link}`}})
      await session["liveOrder"]
      expect(groupPosts(chat)).toEqual([
        {type: "text", text: "Checking the bot.\n"},
        {type: "text", text: `Claude's text was not posted: ${off("SimpleX links")}`},
        {type: "text", text: `Claude's text was not posted: ${off("SimpleX links")}`},
      ])
      expect(chat.updates).toEqual([{chatId: 20, itemId: line, text: "Checking the bot.\n"}])
    })

    it("posts that Claude's note was not posted, naming the preference, once for a note and once for a step, and nothing at the step's end", async () => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      refuseInGroup(chat)
      await session.turn({id: id1, prompt: "Another Claude session sent a message: ...", typed: false, note: `✉️ Message from reviewer: Join ${link}`})
      await session.turn({id: id1, at: 1, step: {id: "toolu_1", text: `🌐 Fetch ${link}`}})
      await session.turn({id: id1, at: 2, step: {id: "toolu_1", text: `🌐 Fetch ${link} ✅ 41 ms`, end: true}})
      expect(groupPosts(chat)).toEqual([1, 2].map(() => ({type: "text", text: `Claude's note was not posted: ${off("SimpleX links")}`})))
      expect(chat.updates).toEqual([])
    })

    it("posts that User's prompt was not posted, naming the preference, and posts the answer", async () => {
      const chat = new FakeChat()
      const {hub, session} = await channel(chat)
      hub.user = {post: async () => Promise.reject(refusal("SimpleX links"))} as unknown as User
      await session.turn({id: id1, at: 1, prompt: `Open ${link}`})
      await session.turn({id: id1, at: 2, answer: "Opened."})
      expect(shown(chat)).toEqual([{text: "User's prompt was not posted: SimpleX links are not allowed for User in the group of this session; its owners can allow them in the group's preferences.", quotedItemId: undefined}, {text: "Opened.", quotedItemId: undefined}, endedIn("1 ms")])
    })

    it("posts that a card was not posted, naming the preference, and fails its action with that text", async () => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      refuseInGroup(chat)
      const action = session.actions.create("s", {tool: "send", input: {chat: "@4", text: `Join ${link}`}, subject: {kind: "bot", name: "SimpleX Directory", incognito: false}, chat: "@4", mentions: [], responses: new Collected()}, undefined, 1, "requests")
      await until(() => action.state === "failed")
      expect(action.error).toBe(off("SimpleX links"))
      expect(groupPosts(chat)).toEqual([{type: "text", text: `Action 1: ${off("SimpleX links")}`}])
    })

    it("posts that an action performed in mode host was performed and its card was not posted, naming the preference, and the action stays done", async () => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      chat.chats.set("@4", {chatInfo: ownedContact(4, "SimpleX Directory", id1), items: []})
      refuseInGroup(chat)
      session.actions.mode = "host"
      const result = (await session.call("send", {chat: "@4", text: `Join ${link}`, wait: 0}, signal().call))!.structuredContent
      await until(() => groupPosts(chat).length === 1)
      expect(groupPosts(chat)).toEqual([{type: "text", text: `Action 1 was performed; its card was not posted: ${off("SimpleX links")}`}])
      expect(result).toMatchObject({status: "done", chat: "@4"})
    })

    it("posts that an action performed in mode host failed and its card was not posted, naming the preference, and the action stays failed", async () => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      chat.chats.set("@4", {chatInfo: ownedContact(4, "SimpleX Directory", id1), items: []})
      refuseInGroup(chat)
      failDirectSends(chat)
      session.actions.mode = "host"
      const result = (await session.call("send", {chat: "@4", text: `Join ${link}`, wait: 0}, signal().call))!.structuredContent
      await until(() => groupPosts(chat).length === 1)
      expect(groupPosts(chat)).toEqual([{type: "text", text: `Action 1 failed; its card was not posted: ${off("SimpleX links")}`}])
      expect(result).toMatchObject({status: "failed", error: "contact not ready"})
    })

    const approve = (session: Session, action: Action): void => session.actions.approve(action, "Alice")
    const reject = (session: Session, action: Action): void => session.actions.reject(action, "Alice")
    const end = (session: Session, action: Action): void => session.actions.end(action)
    const expire = (session: Session, action: Action): void => {
      jest.spyOn(Date, "now").mockReturnValue(action.expires)
      session.actions.expireDue()
    }

    it.each([
      ["was performed", approve, false],
      ["failed", approve, true],
      ["was rejected", reject, false],
      ["expired", expire, false],
      ["ended", end, false],
    ] as const)("posts that an action %s and its card was not updated, naming the preference", async (outcome, decide, sendFails) => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      if (sendFails) failDirectSends(chat)
      const action = session.actions.create("s", {tool: "send", input: {chat: "@4", text: "hi"}, subject: {kind: "bot", name: "SimpleX Directory", incognito: false}, chat: "@4", mentions: [], responses: new Collected()}, undefined, 1, "requests")
      await until(() => action.card !== undefined)
      chat.apiUpdateChatItem = async () => Promise.reject(refusal("SimpleX links"))
      decide(session, action)
      await until(() => groupPosts(chat).length === 2)
      expect(groupPosts(chat)[1]).toEqual({type: "text", text: `Action 1 ${outcome}; its card was not updated: ${off("SimpleX links")}`})
    })

    it("answers a /forward of a voice message whose post the core refuses with the preference, not that the file was not received", async () => {
      const chat = new FakeChat()
      const {session} = await channel(chat)
      refuseInGroup(chat)
      const action = session.actions.create("d", {tool: "download", input: {chat: "@4", item: 30}, subject: {kind: "bot", name: "SimpleX Directory", incognito: false}, chat: "@4", item: 30, from: "SimpleX Directory", file: {fileId: 7, name: "memo.m4a", size: 2400, type: "voice", duration: 5}}, undefined, 1, "requests")
      await until(() => action.card !== undefined)
      const copy = join(sessionFolder(root, id1), "files", "sent", "voice", "memo.m4a")
      mkdirSync(join(sessionFolder(root, id1), "files", "sent", "voice"), {recursive: true})
      writeFileSync(copy, "voice")
      session.tools.forwardFile = async () => copy
      expect(await session.control.ownerCommand(invitedAlice, {keyword: "forward", params: "@4 1"})).toBeUndefined()
      await until(() => groupPosts(chat).length === 2)
      expect(groupPosts(chat)[1]).toEqual({type: "text", text: `Action 1: ${off("Voice messages")}`})
    })
  })
})

describe("Main group", () => {
  it("makes the first person who joins its owner, and invites the owner to the open sessions", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat, [self])
    expect(hub.owner()).toEqual({owners: [], link: "https://simplex.chat/g#link"})
    const open = await openSession(hub, id1, stored(1, "open"))
    await openSession(hub, id2, stored(2, "closed"))
    open.open = true
    const carol = member(5, "Carol", T.GroupMemberRole.Member)
    chat.members.set(10, [self, carol])
    await hub.main.onEvent({type: "joinedGroupMember", groupInfo: group(10, "SimpleX MCP", self), member: carol} as unknown as ChatEvent)
    expect(chat.roles).toEqual([{groupId: 10, members: [5], role: T.GroupMemberRole.Owner}])
    expect(chat.invitations).toEqual([{groupId: 20, viaGroupId: 10, groupMemberId: 5, memberRole: T.GroupMemberRole.Owner}])
    expect(sentTo(chat, 10)).toEqual(["I'm Claude. Each Claude Code session has its own group, and you get its invitation here. /sessions lists the sessions; /join N joins the group of session N.", `Carol joined. ${joinRights}`])
    expect(hub.owner()).toEqual({owners: ["Carol"]})
  })

  it("sets its short description and welcome message at its creation only, keeps an owner's edits of them at a load and a rename, and posts at each join what members can do", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const texts = {shortDescr: "Everyone in this group can join the groups of your Claude Code sessions, prompt Claude and approve what it does.", description: "Everyone in this group can join the groups of your Claude Code sessions, prompt Claude and approve what it does. Invite only people you trust with that."}
    await createMainGroup(chat.api(), 1, "SimpleX MCP")
    expect(chat.newGroups.at(-1)!.groupProfile).toMatchObject(texts)
    const {groupInfo} = chat.chats.get("#10")!.chatInfo as T.ChatInfo.Group
    const edited = {shortDescr: "Sessions of the team", description: "Ask Alice before you approve."}
    groupInfo.groupProfile = {...groupInfo.groupProfile, ...edited}
    expect(chat.profiles).toHaveLength(1)
    await hub.main.load()
    expect(chat.profiles).toHaveLength(1)
    await hub.main.rename("Team")
    expect(chat.profiles.at(-1)).toEqual({groupId: 10, groupProfile: expect.objectContaining({displayName: "Team", ...edited})})
    const dave = member(8, "Dave", T.GroupMemberRole.Member)
    chat.members.set(10, [self, alice, bob, dave])
    await hub.main.onEvent({type: "connectedToGroupMember", groupInfo: group(10, "SimpleX MCP", self), member: {...dave, memberStatus: T.GroupMemberStatus.Intro_inv}} as unknown as ChatEvent)
    await hub.main.onEvent({type: "joinedGroupMember", groupInfo: group(10, "SimpleX MCP", self), member: bob} as unknown as ChatEvent)
    expect(sentTo(chat, 10)).toEqual([`Dave joined. ${joinRights}`, `Bob joined. ${joinRights}`])
  })

  it("keeps the profile its owners changed, its preference for files and media included, at a rename, from the group's current profile", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const {groupInfo} = chat.chats.get("#10")!.chatInfo as T.ChatInfo.Group
    const files = {enable: T.GroupFeatureEnabled.Off}
    groupInfo.groupProfile = {...groupInfo.groupProfile, image: "data:image/png;base64,AA", groupPreferences: {...groupInfo.groupProfile.groupPreferences, files}}
    await hub.configure({mainGroup: "Team", approve: "requests", expireHours: 23})
    expect(chat.profiles.at(-1)).toEqual({groupId: 10, groupProfile: expect.objectContaining({displayName: "Team", image: "data:image/png;base64,AA", groupPreferences: expect.objectContaining({files})})})
  })

  it("keeps its link across a restart while it has no owner", async () => {
    const chat = new FakeChat()
    await hubWith(chat, [self])
    const restarted = await hubWith(chat, [self])
    expect(chat.links).toEqual([{groupId: 10, role: T.GroupMemberRole.Member}])
    expect(restarted.owner()).toEqual({owners: [], link: "https://simplex.chat/g#link"})
  })

  it("lets the events of the sessions pass while it invites a new owner to them", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat, [self])
    const session = await openSession(hub, id1, stored(1, "auth"))
    session.open = true
    chat.apiAddMemberViaGroup = () => new Promise(() => undefined)
    const carol = member(5, "Carol", T.GroupMemberRole.Member)
    chat.members.set(10, [self, carol])
    const handled = hub["onEvent"]({type: "joinedGroupMember", groupInfo: group(10, "SimpleX MCP", self), member: carol} as unknown as ChatEvent).then(() => "handled")
    expect(await Promise.race([handled, new Promise(resolve => setTimeout(() => resolve("blocked"), 100))])).toBe("handled")
    await until(() => chat.roles.length === 1)
  })

  it("lists the sessions and invites a member to the group of a session as admin by /join", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const one = await openSession(hub, id1, stored(1, "one"))
    await openSession(hub, id2, stored(2, "two"))
    one.open = true
    hub.main.onMessage(mainMessage("/sessions", bob))
    hub.main.onMessage(mainMessage("/join 2", bob))
    hub.main.onMessage(mainMessage("/join 5", bob))
    hub.main.onMessage(mainMessage("/help", bob))
    await until(() => chat.replies.length === 4)
    expect(chat.replies).toEqual([
      "1 · one\n2 · two, closed",
      `Sent you an invitation to "2 · two".`,
      "No session 5. /sessions lists the sessions.",
      "Unknown command.",
    ])
    expect(chat.invitations.at(-1)).toEqual({groupId: 21, viaGroupId: 10, groupMemberId: 7, memberRole: T.GroupMemberRole.Admin})
    hub.main.onMessage(mainMessage("/join 2", alice))
    await until(() => chat.replies.length === 5)
    expect(chat.invitations.at(-1)).toEqual({groupId: 21, viaGroupId: 10, groupMemberId: 3, memberRole: T.GroupMemberRole.Owner})
  })

  it("replaces the Main group at once also while owners exist, without a post there, and answers /approve reset and /reject reset as unknown commands", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    expect(hub.owner().owners).toEqual(["Alice"])
    expect(await hub.resetOwners()).toEqual({link: "https://simplex.chat/g#link"})
    expect(sentTo(chat, 10)).toEqual([])
    expect(chat.deleted).toEqual([{chatType: T.ChatType.Group, chatId: 10, mode: {type: "full", notify: true}}])
    expect(hub.main.groupId).toBe(21)
    expect(hub.owner()).toEqual({owners: [], link: "https://simplex.chat/g#link"})
    expect(session.control.groupId).toBe(20)
    hub.main.onMessage(mainMessage("/approve reset", alice))
    hub.main.onMessage(mainMessage("/reject reset", alice))
    await until(() => chat.replies.length === 2)
    expect(chat.replies).toEqual(["Unknown command.", "Unknown command."])
  })

  it("leaves the session groups and the members who joined them from the Main group as they are at a reset of the owners: both still prompt and approve", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    await hub.join(bob, 1)
    const invitedBob = member(1007, "Bob", T.GroupMemberRole.Admin)
    chat.members.set(session.control.groupId, [self, invitedAlice, invitedBob])
    await session.control.load()
    await hub.resetOwners()
    expect(hub.main.groupId).toBe(21)
    expect(chat.removed).toEqual([])
    expect(chat.roles).toEqual([])
    const lines: AttachLine[] = []
    session.onLine = line => lines.push(line)
    const sessionGroup = groupChat(group(20, "1 · auth"))
    session.onMessage(event(sessionGroup, item(42, "Fix the build", {type: "groupRcv", groupMember: invitedAlice})))
    session.onMessage(event(sessionGroup, item(43, "Run the tests", {type: "groupRcv", groupMember: invitedBob})))
    await until(() => lines.length === 2)
    expect(lines).toEqual([{prompt: {text: "Fix the build", from: "Alice", item: 42}}, {prompt: {text: "Run the tests", from: "Bob", item: 43}}])
    for (const sender of [invitedAlice, invitedBob]) expect(await session.control.ownerCommand(sender, {keyword: "pending", params: ""})).toBe("No pending actions.")
  })

  it("leaves the groups of the sessions unchanged when a member leaves it, is removed from it or gets a lower role in it, also at the service's start", async () => {
    const chat = new FakeChat()
    const carol = member(5, "Carol", T.GroupMemberRole.Owner)
    const hub = await hubWith(chat, [self, alice, bob, carol])
    const one = await openSession(hub, id1, stored(1, "one"))
    one.open = true
    await hub.join(bob, 1)
    const invited = (main: T.GroupMember): T.GroupMember => member(1000 + main.groupMemberId, main.memberProfile.displayName, main.memberRole)
    chat.members.set(20, [self, invited(alice), invited(bob), invited(carol)])
    const mainEvent = (type: string, fields: object): Promise<void> => hub.main.onEvent({type, groupInfo: group(10, "SimpleX MCP", self), ...fields} as unknown as ChatEvent)
    chat.members.set(10, [self, alice, {...bob, memberStatus: T.GroupMemberStatus.Left}, carol])
    await mainEvent("leftMember", {member: bob})
    chat.members.set(10, [self, {...alice, memberStatus: T.GroupMemberStatus.Removed}, {...carol, memberRole: T.GroupMemberRole.Member}])
    await mainEvent("deletedMember", {byMember: carol, deletedMember: alice})
    await mainEvent("memberRole", {byMember: alice, member: carol, fromRole: T.GroupMemberRole.Owner, toRole: T.GroupMemberRole.Member})
    await (await hubWith(chat, [self])).session(id1, readSessionFile(sessionFolder(root, id1))!)
    expect(chat.removed).toEqual([])
    expect(chat.roles).toEqual([])
    expect(chat.invitations.filter(({groupId}) => groupId === 20)).toHaveLength(3)
  })
})

describe("User", () => {
  it("is the contact of the connection Claude created for it, not of another connection completed meanwhile", async () => {
    let deliver: ((event: CEvt.ContactConnected) => void) | undefined
    const hub = {
      sendChatCmd: async () => ({type: "invitation", user, connLinkInvitation: {connFullLink: "https://simplex.chat/i#user"}, connection: {pccConnId: 77}}),
      wait: (_event: string, matches: (event: CEvt.ContactConnected) => boolean) => new Promise(resolve => (deliver = event => matches(event) && resolve(event))),
      apiSetContactCustomData: jest.fn(async () => undefined),
    }
    const profile = {apiConnectPlan: async () => [{}, {connFullLink: "https://simplex.chat/i#user"}], apiConnect: async () => undefined}
    const connecting = new (User as unknown as new (chat: unknown, user: T.User) => User)(profile, {...user, userId: 2})["connect"](hub as unknown as api.ChatApi, user)
    await until(() => deliver !== undefined)
    deliver!({type: "contactConnected", user, contact: contact(5, "Bob")})
    deliver!({type: "contactConnected", user, contact: {...contact(6, "User"), activeConn: {connId: 77, connStatus: {type: "ready"}}} as unknown as T.Contact})
    await connecting
    expect(hub.apiSetContactCustomData.mock.calls).toEqual([[6, {simplexMcpUser: true}]])
  })
})
