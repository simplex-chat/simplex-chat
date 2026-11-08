import {mkdirSync, mkdtempSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {CallToolResult} from "@modelcontextprotocol/server"
import {CEvt, ChatEvent, T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {controlCommands} from "../../src/control"
import {SessionFile, createDataFolder, readSessionFile, sessionFolder, writeSessionFile} from "../../src/files"
import {Hub, Session} from "../../src/hub"
import {AttachLine} from "../../src/protocol"
import {User} from "../../src/user"
import {version} from "../../src/version"
import {FakeChat, contact, direct, event, fileItem, group, groupChat, item, member, signal, until, user} from "./fixtures"

const commandsReply = "Commands: /approve, /reject, /receive, /forward, /revoke, /leave, /delete, /pending, /list. Each card shows its commands."

const id1 = "11111111-1111-4111-8111-111111111111"
const id2 = "22222222-2222-4222-8222-222222222222"
const self = member(1, "Claude", T.GroupMemberRole.Owner, T.GroupMemberStatus.Creator)
const alice = member(3, "Alice", T.GroupMemberRole.Owner)
const bob = member(7, "Bob", T.GroupMemberRole.Member)
const invitedAlice = member(1003, "Alice", T.GroupMemberRole.Owner)
const noOwner = `The group of this session has no owner. An owner of the Main group joins it by its invitation there; without owners, run npx -y @simplex-chat/mcp@${version} link in a terminal.`
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

  it("keep the fields the daemon wrote in the session file while the group was created", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const folder = sessionFolder(root, id1)
    createDataFolder(folder)
    writeSessionFile(folder, {...stored(1, "auth"), aliases: [id2]})
    await hub.session(id1, stored(1, "auth"))
    expect(readSessionFile(folder)).toEqual({...stored(1, "auth"), aliases: [id2], group: 20})
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
    expect(session.busy()).toBe(false)
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "approve", params: "1"})).toBeUndefined()
    await until(() => chat.updates.length === 1)
    expect(session.actions.mode).toBe("host")
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({group: 20, mode: "host"})
    expect(chat.updates[0].text).toBe("I run in mode host: Claude Code asks you before each of my actions, and they are performed without approval here.\nApproved by Alice.")
    session.requestMode("requests")
    expect(session.actions.mode).toBe("requests")
    expect(readSessionFile(sessionFolder(root, id1))).toMatchObject({mode: "requests"})
  })

  it("ask for mode host once an owner joins the group of a session that requested it before", async () => {
    const chat = new FakeChat()
    const hub = await hubWith(chat)
    const session = await openSession(hub, id1, stored(1, "auth"))
    session.requestMode("host")
    expect(session.actions.all()).toEqual([])
    chat.members.set(20, [self, invitedAlice])
    await hub["onEvent"]({type: "joinedGroupMember", groupInfo: group(20, "1 · auth"), member: invitedAlice} as unknown as ChatEvent)
    await until(() => sentTo(chat, 20).length === 1)
    expect(sentTo(chat, 20)).toEqual([`${modeCard}\n/'approve 1' or /'reject 1'`])
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "reject", params: "1"})).toBeUndefined()
    await hub["onEvent"]({type: "connectedToGroupMember", groupInfo: group(20, "1 · auth"), member: invitedAlice} as unknown as ChatEvent)
    await hub["controlEvents"]
    expect(sentTo(chat, 20)).toHaveLength(1)
    expect(session.actions.mode).toBe("requests")
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

  it("claim a chat connected after its connect call returned, and a known chat without owner at a connect", async () => {
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

describe("session channel", () => {
  const sessionGroup = groupChat(group(20, "1 · auth"))
  const mallory = member(9, "Mallory", T.GroupMemberRole.Owner)
  const from = (sender: T.GroupMember): T.CIDirection => ({type: "groupRcv", groupMember: sender})
  const tag = (n: number, text: string): string => `<channel source="plugin:simplex:simplex" from="Alice" item="${n}">\n${text}\n</channel>`
  const answers = (chat: FakeChat) => chat.sent.filter(({chat: [, groupId]}) => groupId === 20).map(({messages}) => messages.map(({msgContent, quotedItemId}) => ({text: msgContent.text, quotedItemId})))
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

  it("posts a card for each tool use, passes the owners' verdicts to the session, and ends the open cards with the turn", async () => {
    const chat = new FakeChat()
    const {session, lines} = await channel(chat)
    for (const requestId of ["abcde", "fghij", "kmnop"]) session.permission({id: id1, requestId, toolName: "Bash", description: "Run shell command", inputPreview: "rm -rf build"})
    await until(() => sentTo(chat, 20).length === 3)
    const card = "I want to use `Bash`: `Run shell command`\n`rm -rf build`"
    expect(sentTo(chat, 20)).toEqual([1, 2, 3].map(n => `${card}\n/'approve ${n}' or /'reject ${n}'`))
    expect(await session.control.ownerCommand(mallory, {keyword: "approve", params: "1"})).toBe("Only owners approve actions.")
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "approve", params: "1"})).toBeUndefined()
    expect(await session.control.ownerCommand(invitedAlice, {keyword: "reject", params: "2"})).toBeUndefined()
    await until(() => chat.updates.length === 2)
    expect(lines).toEqual([{permission: {requestId: "abcde", behavior: "allow"}}, {permission: {requestId: "fghij", behavior: "deny"}}])
    expect(session.busy()).toBe(true)
    await session.turn({id: id1, answer: ""})
    await until(() => chat.updates.length === 3)
    expect(chat.updates.map(({text}) => text)).toEqual([`${card}\nApproved by Alice.`, `${card}\nRejected by Alice.`, `${card}\nEnded.`])
    expect(lines).toHaveLength(2)
    expect(session.busy()).toBe(false)
    expect(sentTo(chat, 20)).toHaveLength(3)
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

  it("passes the messages of owners to the session as prompts, with their files, and ignores commands and other members", async () => {
    const chat = new FakeChat()
    const {hub, session, lines} = await channel(chat)
    session.onMessage(event(sessionGroup, item(40, "/pending", from(invitedAlice))))
    session.onMessage(event(sessionGroup, item(41, "Delete the tests", from(mallory))))
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
    await until(() => chat.replies.length === 2)
    expect(chat.replies[1]).toBe("Session 1 is not running.")
    expect(lines).toHaveLength(2)
  })

  it("shows the model and effort in the group description, posts terminal prompts as User except pushed ones, raw or in this channel's tag, and answers as Claude, quoting the owner's prompt", async () => {
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
    await session.turn({id: id1, prompt: "/simplex:link"})
    await session.turn({id: id1, prompt: "Run the tests"})
    await session.turn({id: id1, answer: `${"é".repeat(6000)}ok`})
    expect(posts.map(([, text]) => text)).toEqual([tag(41, "Fix the build"), '<channel source="plugin:other:other" from="Alice" item="42">\nFix the build\n</channel>', "Run the tests"])
    expect(answers(chat)).toEqual([[{text: "Fixed.", quotedItemId: 42}], [{text: "Tests pass.", quotedItemId: 43}], [{text: "é".repeat(6000), quotedItemId: undefined}, {text: "ok", quotedItemId: undefined}]])
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
    expect(answers(chat)).toEqual([[{text: "Fixed.", quotedItemId: 42}], [{text: "Tests pass.", quotedItemId: 43}], [{text: "Deployed and tagged.", quotedItemId: 44}], [{text: "Green.", quotedItemId: 46}]])
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
    expect(answers(chat)).toEqual([[{text: "Fixed.", quotedItemId: 42}], [{text: "Tests pass.", quotedItemId: 43}], [{text: "Deployed.", quotedItemId: 44}], [{text: "Tagged.", quotedItemId: 45}]])
  })

  it("ends the turn of a prompt with an empty answer, so that a later answer without a prompt hook quotes its own prompt", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    await ownerPrompt(session, 42, "Fix the build")
    await session.turn({id: id1, at: 1, prompt: tag(42, "Fix the build")})
    await session.turn({id: id1, at: 2, answer: ""})
    await ownerPrompt(session, 43, "Status?")
    await session.turn({id: id1, at: 3, answer: "Green."})
    expect(answers(chat)).toEqual([[{text: "Green.", quotedItemId: 43}]])
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
    await until(() => chat.replies.length === 1)
    expect(chat.replies).toEqual(["Session 1 is not running."])
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
    expect(chat.replies).toEqual(["The file of this message was not received: SimpleX error: unexpected response cmdOk"])
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
    expect(chat.replies).toEqual(["The file of this message was not received: not received in 10 minutes"])
  })

  it("holds an owner's prompt after the daemon start until the session attaches, and replies that it is not running after 30 s without an attachment", async () => {
    jest.useFakeTimers({doNotFake: ["nextTick", "setImmediate", "queueMicrotask"]})
    const chat = new FakeChat()
    const {hub, session, lines} = await channel(chat)
    session.onLine = undefined
    hub.promptsHeldUntil = Date.now() + 30_000
    session.onMessage(event(sessionGroup, item(42, "Fix the build", from(invitedAlice))))
    await jest.advanceTimersByTimeAsync(10_000)
    expect(chat.replies).toEqual([])
    session.onLine = line => lines.push(line)
    await jest.advanceTimersByTimeAsync(0)
    expect(lines).toEqual([{prompt: {text: "Fix the build", from: "Alice", item: 42}}])
    session.onLine = undefined
    session.onMessage(event(sessionGroup, item(43, "Run the tests", from(invitedAlice))))
    await jest.advanceTimersByTimeAsync(19_999)
    expect(chat.replies).toEqual([])
    await jest.advanceTimersByTimeAsync(1)
    expect(chat.replies).toEqual(["Session 1 is not running."])
    session.onMessage(event(sessionGroup, item(44, "Status?", from(invitedAlice))))
    await jest.advanceTimersByTimeAsync(0)
    expect(chat.replies).toEqual(["Session 1 is not running.", "Session 1 is not running."])
    expect(lines).toHaveLength(1)
  })

  it("passes an owner's one-line message that is not a command of the group to the session as a prompt", async () => {
    const chat = new FakeChat()
    const {session, lines} = await channel(chat)
    session.onMessage(event(sessionGroup, item(40, "/model sonnet", from(invitedAlice))))
    session.onMessage(event(sessionGroup, item(41, "/model sonnet", from(mallory))))
    session.onMessage(event(sessionGroup, item(42, "/approve x", from(invitedAlice))))
    await until(() => lines.length === 1 && chat.replies.length === 2)
    expect(lines).toEqual([{prompt: {text: "/model sonnet", from: "Alice", item: 40}}])
    expect(chat.replies).toEqual([commandsReply, commandsReply])
  })

  it("sends files to the session group with reply, under the refused-path rules", async () => {
    const chat = new FakeChat()
    const {session} = await channel(chat)
    const received = join(sessionFolder(root, id1), "files", "group", "8.txt")
    mkdirSync(join(sessionFolder(root, id1), "files", "group"), {recursive: true})
    writeFileSync(received, "log")
    writeFileSync(join(root, "token"), "secret\n")
    expect((await session.call("reply", {file: received, text: "The log"}, signal().call))!.structuredContent).toEqual({item: 500})
    expect(chat.sent.at(-1)).toEqual({chat: [T.ChatType.Group, 20], messages: [{fileSource: {filePath: received}, msgContent: {type: "file", text: "The log"}, mentions: {}}]})
    expect(textOf(await session.call("reply", {file: join(root, "token")}, signal().call))).toMatch(/ is refused: private keys and the server's own files are not sent\.$/)
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
