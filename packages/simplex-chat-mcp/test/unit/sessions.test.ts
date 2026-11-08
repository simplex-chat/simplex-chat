import {mkdirSync, mkdtempSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {CallToolResult} from "@modelcontextprotocol/server"
import {ChatEvent, T} from "@simplex-chat/types"
import {controlCommands} from "../../src/control"
import {SessionFile, createDataFolder, readSessionFile, sessionFolder, writeSessionFile} from "../../src/files"
import {Hub, Session} from "../../src/hub"
import {FakeChat, contact, direct, event, group, groupChat, item, member, signal, user} from "./fixtures"

const id1 = "11111111-1111-4111-8111-111111111111"
const id2 = "22222222-2222-4222-8222-222222222222"
const self = member(1, "Claude", T.GroupMemberRole.Owner, T.GroupMemberStatus.Creator)
const alice = member(3, "Alice", T.GroupMemberRole.Owner)
const bob = member(7, "Bob", T.GroupMemberRole.Member)
const invitedAlice = member(1003, "Alice", T.GroupMemberRole.Owner)
const noOwner = "The group of this session has no owner. An owner of the Main group joins it by its invitation there; without owners, run npx -y @simplex-chat/mcp link in a terminal."
const modeCard = "I want to run in mode host: Claude Code asks you before each of my actions, and they are performed without approval here."

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

async function until(condition: () => boolean): Promise<void> {
  for (let i = 0; i < 200 && !condition(); i++) await new Promise(resolve => setTimeout(resolve, 5))
  expect(condition()).toBe(true)
}

async function listed(session: Session): Promise<string[]> {
  const {chats} = (await session.call("list", {}, signal().call))!.structuredContent as {chats: {chat: string}[]}
  return chats.map(({chat}) => chat)
}

describe("session groups", () => {
  it("are created at the session start, named by number and title, and the Main group's owners are invited through the Main group", async () => {
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
          description: "The Claude Code session 1: owners of the Main group approve its actions here.",
          groupPreferences: {history: {enable: "on"}, directMessages: {enable: "off"}, commands: controlCommands},
        },
      },
    ])
    expect(chat.invitations).toEqual([{groupId: 20, viaGroupId: 10, groupMemberId: 3, memberRole: T.GroupMemberRole.Owner}])
    expect(chat.customData.filter(({chat: ref}) => ref === "#20").at(-1)!.customData).toEqual({simplexMcpSession: {session: 1, invited: [{groupMemberId: 1003, mainMemberId: 3}]}})
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({number: 1, group: 20})
    expect(await hub.session(id1, {...stored(1, "auth"), group: 20})).toBe(session)
  })

  it("count approvals only from invited members whose Main group member is an owner", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    const mallory = member(9, "Mallory", T.GroupMemberRole.Owner)
    chat.members.set(20, [self, invitedAlice, mallory])
    await session.control.load()
    const pending = {keyword: "pending", params: ""}
    expect(await session.control.ownerCommand(invitedAlice, pending)).toBe("No pending actions.")
    expect(await session.control.ownerCommand(mallory, pending)).toBe("Only owners approve actions.")
    chat.members.set(10, [self, {...alice, memberRole: T.GroupMemberRole.Member}, bob])
    await hub.main.onEvent({type: "memberRole", groupInfo: group(10, "SimpleX MCP", self), byMember: bob, member: alice, fromRole: T.GroupMemberRole.Owner, toRole: T.GroupMemberRole.Member} as unknown as ChatEvent)
    expect(await session.control.ownerCommand(invitedAlice, pending)).toBe("Only owners approve actions.")
    chat.chats.set("@4", {chatInfo: ownedContact(4, "SimpleX Directory", id1), items: []})
    expect(textOf(await session.call("send", {chat: "@4", text: "hi"}, signal().call))).toBe(noOwner)
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
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "approve", params: "1"})).toBeUndefined()
    await until(() => chat.updates.length === 1)
    expect(session.actions.mode).toBe("host")
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({group: 20, mode: "host"})
    expect(chat.updates[0].text).toBe("I run in mode host: Claude Code asks you before each of my actions, and they are performed without approval here.\nApproved by Alice.")
    session.requestMode("requests")
    expect(session.actions.mode).toBe("requests")
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({mode: "requests"})
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
    expect(await send(join(ownFiles, "2.txt"))).toBe(noOwner)
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
    expect(sentTo(chat, 10)).toEqual(["I'm Claude. Each Claude Code session has its own group, and you get its invitation here. /sessions lists the sessions; /join N joins the group of session N."])
    expect(hub.owner()).toEqual({owners: ["Carol"]})
  })

  it("lists the sessions and invites a member to the group of a session by /join", async () => {
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
      "Commands: /sessions, /join N. Owners decide a requested reset of the owners with /'approve reset' or /'reject reset'.",
    ])
    expect(chat.invitations.at(-1)).toEqual({groupId: 21, viaGroupId: 10, groupMemberId: 7, memberRole: T.GroupMemberRole.Member})
  })

  it("resets the owners at once without owners, and only after an owner approves its card while owners exist", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    const card = "A terminal asks to replace the owners: this group is deleted, and a new Main group with a new link replaces it."
    expect(await hub.resetOwners()).toEqual({})
    expect(await hub.resetOwners()).toEqual({})
    expect(sentTo(chat, 10)).toEqual([`${card}\n/'approve reset' or /'reject reset'`])
    hub.main.onMessage(mainMessage("/approve reset", bob))
    hub.main.onMessage(mainMessage("/reject reset", alice))
    await until(() => chat.updates.length === 1)
    expect(chat.replies).toEqual(["Only owners decide a reset of the owners."])
    expect(chat.updates[0]).toEqual({chatId: 10, itemId: expect.any(Number), text: `${card}\nRejected by Alice.`})
    expect(chat.deleted).toEqual([])
    await hub.resetOwners()
    hub.main.onMessage(mainMessage("/approve reset", alice))
    await until(() => chat.deleted.length === 1)
    await until(() => chat.removed.length === 1)
    expect(chat.updates.at(-1)!.text).toBe(`${card}\nApproved by Alice.`)
    expect(chat.deleted).toEqual([{chatType: T.ChatType.Group, chatId: 10, mode: {type: "full", notify: true}}])
    expect(chat.removed).toEqual([{groupId: session.control.groupId, memberIds: [1003]}])
    expect(hub.main.groupId).toBe(21)
    await until(() => hub.owner().link !== undefined)
    expect(hub.owner()).toEqual({owners: [], link: "https://simplex.chat/g#link"})
    expect(await hub.resetOwners()).toEqual({link: "https://simplex.chat/g#link"})
  })
})
