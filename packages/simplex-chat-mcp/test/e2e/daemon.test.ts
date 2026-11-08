import {mkdtempSync, readFileSync, rmSync, writeFileSync} from "fs"
import {tmpdir} from "os"
import {dirname, join} from "path"
import {T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {DaemonClient} from "../../src/client"
import {defaultConfig} from "../../src/config"
import {Daemon, productionDeps} from "../../src/daemon"
import {hubFolder, readSessionFile, sessionFolder} from "../../src/files"
import {sessionStart, turn} from "../../src/hook"
import {AttachLine, SessionReply, StatusReply, TitleReply} from "../../src/protocol"
import {Bot, ItemCheck, callTool, captureLog, cardNumber, isActive, joinByLink, lines, person, prepareRoot, received, startBot, textStarts, until} from "./helpers"
import {SmpServer, startSmpServer, startXftpServer} from "./smpServer"
import {daemonPid, killDaemon, spawnSession} from "./stdio"

const one = "11111111-1111-4111-8111-111111111111"
const two = "22222222-2222-4222-8222-222222222222"
const three = "33333333-3333-4333-8333-333333333333"
const four = "44444444-4444-4444-8444-444444444444"
const five = "55555555-5555-4555-8555-555555555555"
const daemonStopped = "The SimpleX daemon stopped; repeat the call."

let smpServer: SmpServer
let xftpServer: SmpServer
let dir: string
let closing: (() => Promise<void>)[]

beforeAll(async () => {
  smpServer = await startSmpServer()
  xftpServer = await startXftpServer()
  dir = mkdtempSync(join(tmpdir(), "simplex-mcp-daemon-"))
  captureLog()
}, 120_000)

afterAll(async () => {
  jest.restoreAllMocks()
  await smpServer?.stop()
  await xftpServer?.stop()
  rmSync(dir, {recursive: true, force: true})
})

beforeEach(() => {
  closing = []
})

afterEach(async () => {
  for (const close of closing.reverse()) await close().catch(error => process.stderr.write(`cleanup: ${error}\n`))
})

type Person = {chat: api.ChatApi; user: T.User}

async function personIn(scenario: string, name: string): Promise<Person> {
  const p = await person(smpServer.address, join(dir, scenario, name), name, xftpServer.address)
  closing.push(() => p.chat.close())
  return p
}

async function botIn(scenario: string, name: string): Promise<Bot> {
  const b = await startBot(smpServer.address, join(dir, scenario, name), name, {welcomeMessage: "Welcome!"})
  closing.push(() => b.chat.close())
  return b
}

async function startDaemon(root: string): Promise<Daemon> {
  const daemon = await Daemon.start({...productionDeps(root), onStopped: () => undefined}, () => defaultConfig)
  closing.push(() => daemon.stop())
  return daemon
}

async function openSession(client: DaemonClient, id: string, name: string): Promise<SessionReply> {
  await client.request<TitleReply>("POST", "/title", {id, name, config: defaultConfig})
  return client.request<SessionReply>("POST", "/session", {id, project: "/p", config: defaultConfig})
}

const inGroup = (groupId: number, check: ItemCheck): ItemCheck => ci => ci.chatInfo.type === "group" && ci.chatInfo.groupInfo.groupId === groupId && check(ci)

async function joinSessionGroups(owner: Person, names: string[]): Promise<Map<string, number>> {
  const groups = new Map<string, number>()
  await new Promise<void>(resolve => {
    owner.chat.on("receivedGroupInvitationViaGroup", ({groupInfo}) => {
      groups.set(groupInfo.groupProfile.displayName, groupInfo.groupId)
      if (names.every(name => groups.has(name))) resolve()
    })
  })
  for (const groupId of groups.values()) {
    const joined = owner.chat.wait("userJoinedGroup", ({groupInfo}) => groupInfo.groupId === groupId, 60_000)
    await owner.chat.apiJoinGroup(groupId)
    await joined
  }
  return groups
}

test("an owner joins the Main group and each session's group, and approves its connect, send, download and leave there; chats belong to their sessions", async () => {
  const root = join(dir, "sessions", "root")
  await prepareRoot(smpServer.address, root, xftpServer.address)
  const alice = await personIn("sessions", "Alice")
  const directory = await botIn("sessions", "Directory")
  const shop = await botIn("sessions", "Shop")
  let daemon = await startDaemon(root)
  const client = DaemonClient.forRoot(root)

  expect(await openSession(client, one, "one")).toMatchObject({number: 1, displayName: "1 · one", owner: {owners: []}})
  expect(await openSession(client, two, "two")).toMatchObject({number: 2, displayName: "2 · two"})
  const {owner} = await client.request<StatusReply>("GET", "/status")
  const greeting = received(alice.chat, textStarts("I'm Claude. Each Claude Code session has its own group"))
  const invitations = joinSessionGroups(alice, ["1 · one", "2 · two"])
  await joinByLink(alice, owner.link!)
  await greeting
  const groups = await invitations
  const [groupOne, groupTwo] = [groups.get("1 · one")!, groups.get("2 · two")!]
  expect((await client.request<StatusReply>("GET", "/status")).owner).toEqual({owners: ["Alice"]})
  const ownerSends = (groupId: number, text: string) => alice.chat.apiSendTextMessage([T.ChatType.Group, groupId], text)
  const card = (groupId: number, text: string): Promise<T.AChatItem> => received(alice.chat, inGroup(groupId, textStarts(text)))
  const approved = async (groupId: number, cardText: string, start: () => Promise<{result: any}>, command = "approve"): Promise<{result: any}> => {
    const shown = card(groupId, cardText)
    const call = start()
    const ci = await shown
    const chat = /\/'\w+ ([@#]\d+) \d+'/.exec(ci.chatItem.meta.itemText)?.[1]
    await ownerSends(groupId, `/${command} ${chat === undefined ? "" : `${chat} `}${cardNumber(ci)}`)
    return call
  }

  const connected = await approved(groupOne, "I want to connect to bot Directory, as myself.", () => callTool(client, one, "connect", {target: directory.link, idle: 2}))
  expect(connected.result).toMatchObject({action: 1, status: "done", connected: true, type: "contact", name: "Directory"})
  const directoryChat = connected.result.chat as string
  expect(await callTool(client, two, "connect", {target: directory.link})).toMatchObject({isError: true, text: `The chat of ${directory.link} belongs to another session; an owner can give it to this session with /give.`})
  const shopped = await approved(groupTwo, "I want to connect to bot Shop, as myself.", () => callTool(client, two, "connect", {target: shop.link, idle: 2}))
  expect(shopped.result).toMatchObject({action: 1, status: "done", connected: true, name: "Shop"})
  const shopChat = shopped.result.chat as string
  const chatsOf = async (id: string): Promise<string[]> => (await callTool(client, id, "list", {})).result.chats.map(({chat}: {chat: string}) => chat)
  expect(await chatsOf(one)).toEqual([directoryChat])
  expect(await chatsOf(two)).toEqual([shopChat])

  const shownSend = card(groupOne, "I want to send a message to bot Directory:")
  const pending = callTool(client, one, "send", {chat: directoryChat, text: "trail shoes", idle: 2})
  const sendCard = await shownSend
  const notPending = received(alice.chat, inGroup(groupTwo, textStarts(`Action ${cardNumber(sendCard)} is not pending.`)))
  await ownerSends(groupTwo, `/approve ${directoryChat} ${cardNumber(sendCard)}`)
  await notPending
  await ownerSends(groupOne, `/approve ${directoryChat} ${cardNumber(sendCard)}`)
  expect((await pending).result.responses.map(({text}: {text: string}) => text)).toEqual(["result 1: trail shoes", "result 2: trail shoes"])

  const trail = await alice.chat.apiNewGroup(alice.user.userId, {displayName: "Trail", fullName: ""})
  const trailLink = await alice.chat.apiCreateGroupLink(trail.groupId, T.GroupMemberRole.Member)
  const joinedTrail = await approved(groupOne, "I want to connect to group Trail, as myself.", () => callTool(client, one, "connect", {target: trailLink, idle: 2}))
  const trailChat = joinedTrail.result.chat as string
  const notes = join(dir, "sessions", "notes.txt")
  writeFileSync(notes, "trail notes")
  await alice.chat.apiSendMessages([T.ChatType.Group, trail.groupId], [{fileSource: {filePath: notes}, msgContent: {type: "file", text: ""}, mentions: {}}])
  const [offered] = (await callTool(client, one, "receive", {chat: trailChat, wait: 30})).result.messages
  expect(offered.file).toMatchObject({name: "notes.txt", status: "offered"})
  const downloaded = await approved(groupOne, "I want to download a file from Alice in group Trail", () => callTool(client, one, "download", {chat: trailChat, item: offered.item, wait: 60}), "receive")
  expect(dirname(downloaded.result.path)).toBe(join(sessionFolder(root, one), "files", `group-${trailChat.slice(1)}`))
  expect(readFileSync(downloaded.result.path, "utf8")).toBe("trail notes")
  const left = await approved(groupOne, "I want to leave group Trail", () => callTool(client, one, "leave", {chat: trailChat}))
  expect(left.result).toMatchObject({status: "done", chat: trailChat})

  const gave = received(alice.chat, inGroup(groupOne, textStarts("Gave bot Directory")))
  await ownerSends(groupOne, `/give ${directoryChat} 2`)
  expect(lines(await gave)).toEqual([`Gave bot Directory (${directoryChat}) to session 2.`])
  expect(await chatsOf(one)).toEqual([])
  expect((await chatsOf(two)).sort()).toEqual([directoryChat, shopChat].sort())

  await daemon.stop()
  daemon = await startDaemon(root)
  const listedOne = received(alice.chat, inGroup(groupOne, textStarts("No chats.")))
  await ownerSends(groupOne, "/list")
  await listedOne
  expect(await openSession(client, two, "two")).toMatchObject({number: 2, displayName: "2 · two", owner: {owners: ["Alice"]}})
  expect((await chatsOf(two)).sort()).toEqual([directoryChat, shopChat].sort())
}, 600_000)

test("the hooks post a terminal prompt as User and the answer as Claude in the session's group; an owner's prompt reaches an attached session, in the channel tag or raw it is not posted again, and the answer quotes it", async () => {
  const root = join(dir, "mirror", "root")
  await prepareRoot(smpServer.address, root)
  const alice = await personIn("mirror", "Alice")
  await startDaemon(root)
  const client = DaemonClient.forRoot(root)
  await openSession(client, five, "five")
  const {owner} = await client.request<StatusReply>("GET", "/status")
  const invitations = joinSessionGroups(alice, ["1 · five"])
  await joinByLink(alice, owner.link!)
  const groupId = (await invitations).get("1 · five")!
  await until(async () => (await alice.chat.apiListMembers(groupId)).some(member => member.memberProfile.displayName === "User" && isActive(member)))
  const message = (text: string, sender: string, quoted?: number): ItemCheck =>
    inGroup(groupId, ({chatItem}) => chatItem.meta.itemText === text && chatItem.chatDir.type === "groupRcv" && chatItem.chatDir.groupMember.memberProfile.displayName === sender && chatItem.quotedItem?.itemId === quoted)
  const hook = (input: object) => turn(JSON.stringify({session_id: five, ...input}), {}, client)

  const typed = received(alice.chat, message("Fix the build", "User"))
  expect(await hook({hook_event_name: "UserPromptSubmit", prompt: "Fix the build"})).toEqual({})
  await typed
  const fixed = received(alice.chat, message("Fixed.", "Claude"))
  await hook({hook_event_name: "Stop", last_assistant_message: "Fixed."})
  await fixed
  const notRunning = received(alice.chat, inGroup(groupId, textStarts("Session 1 is not running.")))
  const [early] = await alice.chat.apiSendTextMessage([T.ChatType.Group, groupId], "Are you there?")
  expect((await notRunning).chatItem.quotedItem?.itemId).toBe(early.chatItem.meta.itemId)

  const attached: unknown[] = []
  const controller = new AbortController()
  closing.push(async () => controller.abort())
  client.stream("/attach", {id: five}, line => attached.push(line), controller.signal).catch(() => undefined)
  await until(() => attached.length === 1)
  const [sent] = await alice.chat.apiSendTextMessage([T.ChatType.Group, groupId], "Run the tests")
  await until(() => attached.length === 2)
  const {item} = (attached[1] as {prompt: {item: number}}).prompt
  expect(attached[1]).toEqual({prompt: {text: "Run the tests", from: "Alice", item}})
  const passed = received(alice.chat, message("Tests pass.", "Claude", sent.chatItem.meta.itemId))
  await hook({hook_event_name: "UserPromptSubmit", prompt: `<channel source="plugin:simplex:simplex" from="Alice" item="${item}">\nRun the tests\n</channel>`})
  await hook({hook_event_name: "Stop", last_assistant_message: "Tests pass."})
  await passed
  const [lint] = await alice.chat.apiSendTextMessage([T.ChatType.Group, groupId], "Run the linter")
  await until(() => attached.length === 3)
  const linted = received(alice.chat, message("Lint passes.", "Claude", lint.chatItem.meta.itemId))
  await hook({hook_event_name: "UserPromptSubmit", prompt: "Run the linter"})
  await hook({hook_event_name: "Stop", last_assistant_message: "Lint passes."})
  await linted
  const {chatItems} = await alice.chat.apiGetChat(T.ChatType.Group, groupId, 50)
  const fromUser = chatItems.filter(({chatDir, content}) => chatDir.type === "groupRcv" && chatDir.groupMember.memberProfile.displayName === "User" && content.type === "rcvMsgContent")
  expect(fromUser.map(({meta}) => meta.itemText)).toEqual(["Fix the build"])
}, 600_000)

test("a session keeps its group for hooks with a new ID from its process, relays tool use to its owners, owns a contact that accepts after connect returned, asks for mode host, and gets a prompt sent before it attached again after a daemon start", async () => {
  const root = join(dir, "channel", "root")
  await prepareRoot(smpServer.address, root)
  const alice = await personIn("channel", "Alice")
  const bob = await personIn("channel", "Bob")
  const daemon = await startDaemon(root)
  const client = DaemonClient.forRoot(root)
  const pid = 4242
  const env = {CLAUDE_PID: String(pid), SIMPLEX_MCP_HOME: root}
  await client.request<TitleReply>("POST", "/title", {id: one, pid, name: "one", config: defaultConfig})
  await client.request<SessionReply>("POST", "/session", {id: one, pid, project: "/p", config: defaultConfig})
  const attached: AttachLine[] = []
  const controller = new AbortController()
  closing.push(async () => controller.abort())
  client.stream("/attach", {id: one}, line => attached.push(line as AttachLine), controller.signal).catch(() => undefined)
  await until(() => attached.length === 1)
  const {owner} = await client.request<StatusReply>("GET", "/status")
  const invitations = joinSessionGroups(alice, ["1 · one"])
  await joinByLink(alice, owner.link!)
  const groupId = (await invitations).get("1 · one")!
  const card = (text: string): Promise<T.AChatItem> => received(alice.chat, inGroup(groupId, textStarts(text)))
  const ownerSends = (text: string) => alice.chat.apiSendTextMessage([T.ChatType.Group, groupId], text)

  expect(await sessionStart(JSON.stringify({session_id: two, cwd: "/p", source: "clear"}), env, {client, nodeVersion: process.versions.node})).toEqual({})
  const cleared = received(alice.chat, inGroup(groupId, ({chatItem}) => chatItem.meta.itemText === "Cleared." && chatItem.chatDir.type === "groupRcv" && chatItem.chatDir.groupMember.memberProfile.displayName === "Claude"))
  await turn(JSON.stringify({session_id: two, hook_event_name: "Stop", last_assistant_message: "Cleared."}), env, client)
  await cleared
  expect((await client.request<StatusReply>("GET", "/status")).sessions).toEqual([{number: 1, name: "1 · one", attached: 1}])

  const toolUse = card("I want to use `Bash`: `Run shell command`")
  await client.request("POST", "/permission", {id: one, requestId: "abcde", toolName: "Bash", description: "Run shell command", inputPreview: "ls"})
  await ownerSends(`/approve ${cardNumber(await toolUse)}`)
  await until(() => attached.some(({permission}) => permission !== undefined))
  expect(attached.filter(({permission}) => permission !== undefined)).toEqual([{permission: {requestId: "abcde", behavior: "allow"}}])

  const address = await bob.chat.apiCreateUserAddress(bob.user.userId)
  const requested = bob.chat.wait("receivedContactRequest", 60_000)
  const connectCard = card("I want to connect to")
  const connecting = callTool(client, one, "connect", {target: address.connShortLink ?? address.connFullLink, wait: 0})
  await ownerSends(`/approve ${cardNumber(await connectCard)}`)
  expect((await connecting).result).toMatchObject({status: "done", connected: false})
  await bob.chat.apiAcceptContactRequest((await requested)!.contactRequest.contactRequestId)
  await until(async () => (await callTool(client, one, "list", {})).result.chats.some(({name, connected}: {name: string; connected: boolean}) => name === "Bob" && connected))

  const modeCard = card("I want to run in mode host")
  await client.request("POST", "/session", {id: one, pid, project: "/p", config: {...defaultConfig, approve: "host"}})
  await ownerSends(`/approve ${cardNumber(await modeCard)}`)
  await until(() => readSessionFile(sessionFolder(root, one))?.mode === "host")

  controller.abort()
  await daemon.stop()
  await startDaemon(root)
  await ownerSends("Still there?")
  await new Promise(resolve => setTimeout(resolve, 5_000))
  const reattached: AttachLine[] = []
  const reattach = new AbortController()
  closing.push(async () => reattach.abort())
  client.stream("/attach", {id: one}, line => reattached.push(line as AttachLine), reattach.signal).catch(() => undefined)
  await until(() => reattached.some(({prompt}) => prompt?.text === "Still there?"))
  const {chatItems} = await alice.chat.apiGetChat(T.ChatType.Group, groupId, 20)
  expect(chatItems.map(({meta}) => meta.itemText)).not.toContain("Session 1 is not running.")
}, 600_000)

test("a new session has no chats", async () => {
  const root = join(dir, "fresh", "root")
  await prepareRoot(smpServer.address, root)
  await startDaemon(root)
  const client = DaemonClient.forRoot(root)

  expect(await openSession(client, four, "four")).toMatchObject({number: 1, displayName: "1 · four"})
  const listed = await callTool(client, four, "list", {})
  expect(listed.result.chats).toEqual([])
}, 600_000)

test("stdio session spawns the daemon and recovers after the daemon is killed", async () => {
  const root = join(dir, "stdio", "root")
  await prepareRoot(smpServer.address, root)
  const directory = await botIn("stdio", "Directory")
  const hub = await api.ChatApi.init({type: "sqlite", filePrefix: join(hubFolder(root), "simplex")})
  const user = (await hub.apiGetActiveUser())!
  await hub.startChat()
  const contactConnected = hub.wait("contactConnected", 60_000)
  const [, connLink] = await hub.apiConnectPlan(user.userId, directory.link)
  await hub.apiConnect(user.userId, false, connLink)
  const {contact} = (await contactConnected)!
  await hub.apiSetContactCustomData(contact.contactId, {simplexMcpOwner: {session: three}})
  await hub.close()
  closing.push(() => killDaemon(root))
  const session = spawnSession({...process.env, CLAUDE_CODE_SESSION_ID: three, SIMPLEX_MCP_HOME: root, CLAUDE_PROJECT_DIR: "/p/three"})
  closing.push(() => session.stop())

  expect(await session.client.initialize()).toMatchObject({serverInfo: {name: "simplex-mcp"}})
  const {result} = await session.client.request("tools/list")
  expect(result.tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "download", "list", "leave", "reply"])
  const listed = await session.call("list", {})
  expect(listed.isError).toBeUndefined()
  expect(listed.structuredContent.actions).toEqual([])
  const status = await DaemonClient.forRoot(root).request<StatusReply>("GET", "/status")
  expect(status.sessions).toEqual([{number: 1, name: "1 · three", attached: 1}])
  expect(listed.structuredContent.chats).toEqual([expect.objectContaining({chat: `@${contact.contactId}`, name: "Directory"})])

  await session.call("receive", {chat: `@${contact.contactId}`})
  const firstDaemon = daemonPid(root)
  const progressToken = "receive"
  const receiving = session.call("receive", {chat: `@${contact.contactId}`, wait: 60}, {progressToken})
  await until(() => session.client.notifications.some(({method, params}) => method === "notifications/progress" && params.progressToken === progressToken))
  await killDaemon(root)
  expect(await receiving).toMatchObject({isError: true, content: [{type: "text", text: daemonStopped}]})

  const relisted = await session.call("list", {})
  expect(relisted.isError).toBeUndefined()
  expect(relisted.structuredContent.chats).toEqual(listed.structuredContent.chats.map((chat: object) => ({...chat, unread: 0})))
  const secondDaemon = daemonPid(root)
  expect(secondDaemon).not.toBe(firstDaemon)

  await killDaemon(root)
  await until(async () => (await DaemonClient.forRoot(root).request<StatusReply>("GET", "/status").catch(() => undefined))?.sessions[0]?.attached === 1)
  expect(daemonPid(root)).not.toBe(secondDaemon)
}, 600_000)
