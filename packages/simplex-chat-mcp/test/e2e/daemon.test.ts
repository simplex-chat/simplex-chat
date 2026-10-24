import {mkdtempSync, rmSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {DaemonClient} from "../../src/client"
import {defaultConfig} from "../../src/config"
import {Daemon, productionDeps} from "../../src/daemon"
import {SessionReply, StatusReply} from "../../src/protocol"
import {Bot, ItemCheck, callTool, captureLog, cardNumber, joinByLink, lines, person, prepareRoot, received, startBot, textStarts, until, updated} from "./helpers"
import {SmpServer, startSmpServer} from "./smpServer"
import {daemonPid, killDaemon, spawnSession} from "./stdio"

const one = "11111111-1111-4111-8111-111111111111"
const two = "22222222-2222-4222-8222-222222222222"
const three = "33333333-3333-4333-8333-333333333333"
const four = "44444444-4444-4444-8444-444444444444"
const daemonStopped = "The SimpleX daemon stopped; repeat the call."

let smpServer: SmpServer
let dir: string
let logLines: string[]
let closing: (() => Promise<void>)[]

beforeAll(async () => {
  smpServer = await startSmpServer()
  dir = mkdtempSync(join(tmpdir(), "simplex-mcp-daemon-"))
  logLines = captureLog()
}, 120_000)

afterAll(async () => {
  jest.restoreAllMocks()
  await smpServer?.stop()
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
  const p = await person(smpServer.address, join(dir, scenario, name), name)
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

function openSession(client: DaemonClient, id: string, name: string): Promise<SessionReply> {
  return client.request<SessionReply>("POST", "/session", {id, name, project: "/p", config: defaultConfig})
}

const cardFrom = (session: number, name: string, request: string): ItemCheck => textStarts(`Session ${session} · SimpleX MCP - ${name}\n${request}`)

test("two sessions in one Main group, then resume after a restart", async () => {
  const root = join(dir, "sessions", "root")
  await prepareRoot(smpServer.address, root, [
    {id: one, name: "one"},
    {id: two, name: "two"},
  ])
  const alice = await personIn("sessions", "Alice")
  const directory = await botIn("sessions", "Directory")
  let daemon = await startDaemon(root)
  const client = DaemonClient.forRoot(root)

  expect(await openSession(client, one, "one")).toMatchObject({number: 1, displayName: "SimpleX MCP - one", owner: {owners: []}})
  expect(await openSession(client, two, "two")).toMatchObject({number: 2, displayName: "SimpleX MCP - two"})
  const {owner} = await client.request<StatusReply>("GET", "/status")
  expect(owner.link).toBeDefined()

  const greeting = received(alice.chat, textStarts("I'm your agents' Main group."))
  await joinByLink(alice, owner.link!)
  const mainId = ((await greeting).chatInfo as T.ChatInfo.Group).groupInfo.groupId
  await until(() => logLines.some(line => /^Alice joined \(member \d+\) and is now an owner\. The link is deleted\.$/.test(line)))
  expect((await client.request<StatusReply>("GET", "/status")).owner).toEqual({owners: ["Alice"]})
  const ownerSends = (text: string) => alice.chat.apiSendTextMessage([T.ChatType.Group, mainId], text)

  const connectDirectory = async (id: string, session: number, name: string): Promise<{n: number; chat: string}> => {
    const card = received(alice.chat, cardFrom(session, name, "I want to connect to bot Directory, as myself."))
    const connecting = callTool(client, id, "connect", {target: directory.link, idle: 2})
    const n = cardNumber(await card)
    await ownerSends(`/approve ${n}`)
    const {result} = await connecting
    expect(result).toMatchObject({action: n, status: "done", connected: true, type: "contact", name: "Directory"})
    return {n, chat: result.chat}
  }
  const first = await connectDirectory(one, 1, "one")
  const second = await connectDirectory(two, 2, "two")
  expect(second.n).not.toBe(first.n)
  const target1 = `1:${first.chat}`
  const target2 = `2:${second.chat}`

  const sendCard = (session: number, name: string, text: string): Promise<T.AChatItem> => {
    const isCard = cardFrom(session, name, "I want to send a message to bot Directory:")
    return received(alice.chat, ci => isCard(ci) && lines(ci)[2] === `\`${text}\``)
  }
  const responseTexts = ({result}: {result: any}): string[] => result.responses.map(({text}: {text: string}) => text)

  const card1 = sendCard(1, "one", "from one")
  const sending1 = callTool(client, one, "send", {chat: first.chat, text: "from one", idle: 2})
  const n1 = cardNumber(await card1)
  const card2 = sendCard(2, "two", "from two")
  const sending2 = callTool(client, two, "send", {chat: second.chat, text: "from two", idle: 2})
  const n2 = cardNumber(await card2)
  const outcome = (card: T.AChatItem): Promise<T.AChatItem> => updated(alice.chat, ci => ci.chatItem.meta.itemId === card.chatItem.meta.itemId)
  const notFor = received(alice.chat, textStarts(`Action ${n2} is not for ${target1}.`))
  await ownerSends(`/approve ${target1} ${n2}`)
  await notFor
  const approved2 = outcome(await card2)
  await ownerSends(`/approve ${target2} ${n2}`)
  const sent2 = await sending2
  expect(sent2.result).toMatchObject({action: n2, status: "done", chat: second.chat, more: 0})
  expect(responseTexts(sent2)).toEqual(["result 1: from two", "result 2: from two"])
  expect(lines(await approved2).at(-1)).toBe("Approved by Alice.")
  expect((await callTool(client, one, "list", {})).result.actions).toEqual([expect.objectContaining({action: n1, tool: "send", state: "pending"})])
  const rejected1 = outcome(await card1)
  await ownerSends(`/reject ${target1} ${n1}`)
  expect((await sending1).result).toEqual({action: n1, status: "rejected"})
  expect(lines(await rejected1).at(-1)).toBe("Rejected by Alice.")

  const approvedAll = received(alice.chat, textStarts("Approved for bot Directory"))
  await ownerSends(`/approve ${target2} all`)
  expect((await approvedAll).chatItem.meta.itemText).toBe(`Approved for bot Directory (${target2}) by Alice. /'revoke ${target2}'`)
  const standingOutcome = received(alice.chat, cardFrom(2, "two", "I sent a message to bot Directory:\n`standing`"))
  const standing = await callTool(client, two, "send", {chat: second.chat, text: "standing", idle: 2})
  expect(standing.result).toMatchObject({status: "done", chat: second.chat})
  expect(responseTexts(standing)).toEqual(["result 1: standing", "result 2: standing"])
  expect(lines(await standingOutcome).at(-1)).toBe(`Approved for this chat by Alice. /'revoke ${target2}'`)

  const revoked = received(alice.chat, textStarts(`Messages to bot Directory (${target2}) need approval again.`))
  await ownerSends(`/revoke ${target2}`)
  await revoked
  const card3 = sendCard(2, "two", "after revoke")
  const sending3 = callTool(client, two, "send", {chat: second.chat, text: "after revoke", idle: 2})
  const n3 = cardNumber(await card3)
  await ownerSends(`/approve ${target2} ${n3}`)
  expect((await sending3).result).toMatchObject({action: n3, status: "done"})

  await daemon.stop()
  daemon = await startDaemon(root)
  expect(await openSession(client, two, "two")).toMatchObject({number: 2, displayName: "SimpleX MCP - two", owner: {owners: ["Alice"]}})
  const listed = await callTool(client, two, "list", {})
  expect(listed.result.chats).toContainEqual(expect.objectContaining({chat: second.chat, type: "contact", name: "Directory", connected: true}))
  expect(listed.result.actions).toEqual([])
}, 600_000)

test("a profile the daemon creates has no chats", async () => {
  const root = join(dir, "fresh", "root")
  await prepareRoot(smpServer.address, root, [])
  await startDaemon(root)
  const client = DaemonClient.forRoot(root)

  expect(await openSession(client, four, "four")).toMatchObject({number: 1, displayName: "SimpleX MCP - four"})
  const listed = await callTool(client, four, "list", {})
  expect(listed.result.chats).toEqual([])
}, 600_000)

test("stdio session spawns the daemon and recovers after the daemon is killed", async () => {
  const root = join(dir, "stdio", "root")
  await prepareRoot(smpServer.address, root, [{id: three, name: "three"}])
  closing.push(() => killDaemon(root))
  const session = spawnSession({...process.env, CLAUDE_CODE_SESSION_ID: three, SIMPLEX_MCP_HOME: root, CLAUDE_PROJECT_DIR: "/p/three"})
  closing.push(() => session.stop())

  expect(await session.client.initialize()).toMatchObject({serverInfo: {name: "simplex-mcp"}})
  const {result} = await session.client.request("tools/list")
  expect(result.tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "list", "leave"])
  const listed = await session.call("list", {})
  expect(listed.isError).toBeUndefined()
  expect(listed.structuredContent.actions).toEqual([])
  const status = await DaemonClient.forRoot(root).request<StatusReply>("GET", "/status")
  expect(status.sessions).toEqual([{number: 1, name: "SimpleX MCP - three", attached: 1}])
  const [chat] = listed.structuredContent.chats as {chat: string}[]
  expect(chat).toBeDefined()

  const firstDaemon = daemonPid(root)
  const progressToken = "receive"
  const receiving = session.call("receive", {chat: chat.chat, wait: 60}, {progressToken})
  await until(() => session.client.notifications.some(({method, params}) => method === "notifications/progress" && params.progressToken === progressToken))
  await killDaemon(root)
  expect(await receiving).toMatchObject({isError: true, content: [{type: "text", text: daemonStopped}]})

  const relisted = await session.call("list", {})
  expect(relisted.isError).toBeUndefined()
  expect(relisted.structuredContent.chats).toEqual(listed.structuredContent.chats)
  expect(daemonPid(root)).not.toBe(firstDaemon)
}, 600_000)
