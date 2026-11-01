import {mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync} from "fs"
import {createServer} from "net"
import {tmpdir} from "os"
import {join} from "path"
import {T} from "@simplex-chat/types"
import {api} from "simplex-chat"
import {readServerFile} from "../../src/files"
import {RunningServer, ServeOptions, startServer} from "../../src/serve"
import {McpClient} from "./client"
import {
  captureLog,
  cardNumber,
  controlGroupOf,
  isActive,
  joinByLink,
  lastLink,
  lines,
  person,
  profileWithServer,
  received,
  serveOptions,
  startBot,
  textStarts,
  timeout,
  until,
  updated,
} from "./helpers"
import {SmpServer, freePort, startSmpServer, startXftpServer} from "./smpServer"

let smpServer: SmpServer
let xftpServer: SmpServer
let dir: string
let logLines: string[]
let closing: (() => Promise<void>)[]

beforeAll(async () => {
  smpServer = await startSmpServer()
  xftpServer = await startXftpServer()
  dir = mkdtempSync(join(tmpdir(), "simplex-mcp-scenarios-"))
  logLines = captureLog()
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
  for (const close of closing.reverse()) await close().catch(() => undefined)
})

type Person = {chat: api.ChatApi; user: T.User}

async function agentData(scenario: string): Promise<string> {
  const data = join(dir, scenario, "agent")
  mkdirSync(data, {recursive: true})
  await profileWithServer(smpServer.address, join(data, "simplex"), {displayName: "agent", fullName: "", peerType: T.ChatPeerType.Bot}, xftpServer.address)
  return data
}

async function personIn(scenario: string, name: string): Promise<Person> {
  const p = await person(smpServer.address, join(dir, scenario, name), name, xftpServer.address)
  closing.push(() => p.chat.close())
  return p
}

async function botIn(scenario: string, name: string, settings: api.BotAddressSettings): Promise<{chat: api.ChatApi; link: string}> {
  const b = await startBot(smpServer.address, join(dir, scenario, name), name, settings)
  closing.push(() => b.chat.close())
  return b
}

class Agent {
  server!: RunningServer
  client!: McpClient

  constructor(private readonly data: string, readonly port: number) {}

  async start(extra: Partial<ServeOptions> = {}): Promise<void> {
    this.server = await startServer(serveOptions(this.data, this.port, extra))
    this.client = new McpClient(`http://127.0.0.1:${this.port}/mcp`, this.server.token)
  }

  async stop(): Promise<void> {
    const {server} = this
    this.server = undefined as unknown as RunningServer
    await server?.stop()
  }
}

async function agentIn(scenario: string): Promise<Agent> {
  const agent = new Agent(await agentData(scenario), await freePort())
  closing.push(() => agent.stop())
  return agent
}

async function joinAsOwner(owner: Person): Promise<number> {
  const greeting = received(owner.chat, textStarts("I'm your agent."))
  const controlId = await joinByLink(owner, lastLink(logLines))
  await greeting
  return controlId
}

async function approve(owner: Person, controlId: number, card: Promise<T.AChatItem>): Promise<T.AChatItem> {
  const ci = await card
  await owner.chat.apiSendTextMessage([T.ChatType.Group, controlId], `/approve ${cardNumber(ci)}`)
  return ci
}

test("--owners: an observer joins, the restart lists it, a second owner can't approve", async () => {
  const alice = await personIn("owners", "Alice")
  const bob = await personIn("owners", "Bob")
  const directory = await botIn("owners", "Directory", {welcomeMessage: "Welcome!"})
  const agent = await agentIn("owners")

  await agent.start({owners: []})
  expect(logLines).toContain("The link adds an observer. Its ID and name are printed when it joins.")
  const restartLine = received(alice.chat, textStarts("Alice joined (member "))
  const controlId = await joinByLink(alice, lastLink(logLines))
  const line = (await restartLine).chatItem.meta.itemText
  const aliceId = Number(/^Alice joined \(member (\d+)\)/.exec(line)![1])
  expect(line).toBe(`Alice joined (member ${aliceId}). Restart the server with \`--owners ${aliceId}:Alice\``)
  expect(logLines).toContain(`Alice joined (member ${aliceId}). Restart the server with --owners ${aliceId}:Alice`)
  expect(await agent.client.tool("connect", {target: directory.link})).toMatchObject({
    isError: true,
    text: "The agent has no approving owner. The person who runs it has to restart it with the --owners options it printed.",
  })

  await agent.stop()
  await agent.start({owners: [{memberId: aliceId, name: "Alice"}]})
  expect(logLines).toContain(`Owners: ${aliceId}:Alice.`)
  const connectCard = received(alice.chat, textStarts("I want to connect to bot Directory"))
  const connecting = agent.client.tool("connect", {target: directory.link, idle: 2})
  await approve(alice, controlId, connectCard)
  const connected = await connecting
  expect(connected.result).toMatchObject({status: "done", connected: true})

  const aliceLink = await alice.chat.apiCreateGroupLink(controlId, T.GroupMemberRole.Member)
  const bobControlId = await joinByLink(bob, aliceLink)
  const {server} = agent
  const agentControlId = (await controlGroupOf(server.chat, server.user.userId)).groupId
  await until(async () => (await server.chat.apiListMembers(agentControlId)).some(m => m.memberProfile.displayName === "Bob" && isActive(m)))
  const bobOnAlice = (await alice.chat.apiListMembers(controlId)).find(m => m.memberProfile.displayName === "Bob")!
  const notice = received(alice.chat, textStarts("Bob (member "))
  await alice.chat.apiSetMembersRole(controlId, [bobOnAlice.groupMemberId], T.GroupMemberRole.Owner)
  const noticeText = (await notice).chatItem.meta.itemText
  expect(noticeText).toMatch(new RegExp(`^Bob \\(member (\\d+)\\) is now an owner and can't approve actions yet\\. Restart the server with \`--owners ${aliceId}:Alice,\\1:Bob\`$`))
  expect(logLines).toContain(noticeText.replace(/`/g, ""))

  const pendingCard = received(bob.chat, textStarts("I want to send a message to bot Directory:"))
  const sending = agent.client.tool("send", {chat: connected.result.chat, text: "from Bob's side"})
  const n = cardNumber(await pendingCard)
  const refusal = received(bob.chat, textStarts("Only the owners listed in --owners approve actions."))
  await bob.chat.apiSendTextMessage([T.ChatType.Group, bobControlId], `/approve ${n}`)
  await refusal
  await alice.chat.apiSendTextMessage([T.ChatType.Group, controlId], `/reject ${n}`)
  expect((await sending).result).toEqual({action: n, status: "rejected"})
}, 600_000)

test("host mode: actions run without approval and post outcome cards", async () => {
  const alice = await personIn("host", "Alice")
  const directory = await botIn("host", "Directory", {welcomeMessage: "Welcome!"})
  const agent = await agentIn("host")
  await agent.start({approve: "host"})
  await joinAsOwner(alice)

  const {tools} = await agent.client.request("tools/list")
  const meta = Object.fromEntries(tools.map(({name, _meta}: {name: string; _meta?: object}) => [name, _meta]))
  expect(meta).toEqual({
    connect: {"anthropic/requiresUserInteraction": true},
    send: {"anthropic/requiresUserInteraction": true},
    receive: undefined,
    download: {"anthropic/requiresUserInteraction": true},
    list: undefined,
    leave: {"anthropic/requiresUserInteraction": true},
  })

  const connectCard = received(alice.chat, textStarts("I connected to bot Directory, as myself."))
  const connected = await agent.client.tool("connect", {target: directory.link, idle: 2})
  expect(connected.result).toMatchObject({action: 1, status: "done", connected: true})
  expect(lines(await connectCard).at(-1)).toBe("Performed without approval (mode host).")
  expect(logLines).toContain("Action 1: performed without approval (mode host).")

  const sendCard = received(alice.chat, textStarts("I sent a message to bot Directory:"))
  const sent = await agent.client.tool("send", {chat: connected.result.chat, text: "trail shoes", idle: 2})
  expect(sent.result.responses.map(({text}: {text: string}) => text)).toEqual(["result 1: trail shoes", "result 2: trail shoes"])
  expect(lines(await sendCard)).toEqual(["I sent a message to bot Directory:", "`trail shoes`", "Performed without approval (mode host)."])
}, 600_000)

test("a group joined by its link: a mention, and the observer role", async () => {
  const alice = await personIn("group", "Alice")
  const switchback = await personIn("group", "Switchback")
  const agent = await agentIn("group")
  await agent.start()
  const controlId = await joinAsOwner(alice)

  const trail = await alice.chat.apiNewGroup(alice.user.userId, {displayName: "Trail", fullName: ""})
  const trailLink = await alice.chat.apiCreateGroupLink(trail.groupId, T.GroupMemberRole.Member)
  await joinByLink(switchback, trailLink)
  const joinCard = received(alice.chat, textStarts("I want to connect to group Trail, as myself."))
  const joining = agent.client.tool("connect", {target: trailLink, idle: 2})
  await approve(alice, controlId, joinCard)
  const joined = await joining
  expect(joined.result).toMatchObject({status: "done", type: "group", name: "Trail", connected: true, role: "member"})
  const group = joined.result.chat as string

  const {server} = agent
  let switchbackId = 0
  await until(async () => {
    const member = (await server.chat.apiListMembers(Number(group.slice(1)))).find(m => m.memberProfile.displayName === "Switchback")
    switchbackId = member?.groupMemberId ?? 0
    return member !== undefined && isActive(member)
  })
  const mentionCard = received(alice.chat, textStarts("I want to send a message to group Trail:"))
  const mentioned = received(switchback.chat, textStarts("@Switchback"))
  const sending = agent.client.tool("send", {chat: group, text: "any trail news?", mention: [switchbackId], idle: 1, wait: 3})
  expect(lines(await approve(alice, controlId, mentionCard))).toContain("mentioning @Switchback")
  expect((await sending).result).toMatchObject({status: "done", chat: group})
  const item = await mentioned
  expect(item.chatItem.meta.itemText).toBe("@Switchback any trail news?")
  expect(item.chatItem.meta.userMention).toBe(true)

  const news = await alice.chat.apiNewGroup(alice.user.userId, {displayName: "News", fullName: ""})
  const newsLink = await alice.chat.apiCreateGroupLink(news.groupId, T.GroupMemberRole.Observer)
  const newsCard = received(alice.chat, textStarts("I want to connect to group News, as myself."))
  const joiningNews = agent.client.tool("connect", {target: newsLink, idle: 2})
  await approve(alice, controlId, newsCard)
  const joinedNews = await joiningNews
  expect(joinedNews.result).toMatchObject({status: "done", type: "group", role: "observer"})
  const newsChat = joinedNews.result.chat as string
  expect(await agent.client.tool("send", {chat: newsChat, text: "hello"})).toMatchObject({isError: true, text: `The agent has role observer in ${newsChat} and can't post.`})
}, 600_000)

test("/leave: the owner makes the agent leave a group at once, and the pending send to it is rejected", async () => {
  const alice = await personIn("leave", "Alice")
  const agent = await agentIn("leave")
  await agent.start()
  const controlId = await joinAsOwner(alice)

  const trail = await alice.chat.apiNewGroup(alice.user.userId, {displayName: "Trail", fullName: ""})
  const trailLink = await alice.chat.apiCreateGroupLink(trail.groupId, T.GroupMemberRole.Member)
  const joinCard = received(alice.chat, textStarts("I want to connect to group Trail, as myself."))
  const joining = agent.client.tool("connect", {target: trailLink, idle: 2})
  await approve(alice, controlId, joinCard)
  const group = (await joining).result.chat as string

  const sendCard = received(alice.chat, textStarts("I want to send a message to group Trail:"))
  const sending = agent.client.tool("send", {chat: group, text: "any trail news?"})
  const card = await sendCard
  const rejectedCard = updated(alice.chat, ci => ci.chatItem.meta.itemId === card.chatItem.meta.itemId)
  const reply = received(alice.chat, textStarts("Left group Trail"))
  const agentLeft = alice.chat.wait("leftMember", ({groupInfo}) => groupInfo.groupId === trail.groupId, timeout)
  await alice.chat.apiSendTextMessage([T.ChatType.Group, controlId], `/leave ${group}`)
  const line = `Left group Trail (${group}), requested by Alice.`
  expect((await reply).chatItem.meta.itemText).toBe(line)
  expect(logLines).toContain(line)
  expect((await sending).result).toEqual({action: cardNumber(card), status: "rejected"})
  expect(lines(await rejectedCard).at(-1)).toBe("Rejected by Alice.")
  expect(await agentLeft).toBeDefined()
  expect((await agent.client.tool("list", {})).result.chats).not.toContainEqual(expect.objectContaining({chat: group}))
}, 600_000)

test("files: a received image downloaded after the approval, /receive, /reject and the files approval of a chat", async () => {
  const alice = await personIn("files", "Alice")
  const agent = await agentIn("files")
  await agent.start()
  const controlId = await joinAsOwner(alice)
  const trail = await alice.chat.apiNewGroup(alice.user.userId, {displayName: "Trail", fullName: ""})
  const trailLink = await alice.chat.apiCreateGroupLink(trail.groupId, T.GroupMemberRole.Member)
  const joinCard = received(alice.chat, textStarts("I want to connect to group Trail, as myself."))
  const joining = agent.client.tool("connect", {target: trailLink, idle: 2})
  await approve(alice, controlId, joinCard)
  const group = (await joining).result.chat as string

  const png = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAJCAAAAAAeQfPuAAAAD0lEQVR42mNoQAMMg0QAAIXUSAG1CN3UAAAAAElFTkSuQmCC"
  const preview = `data:image/png;base64,${png}`
  const image = join(dir, "files", "trail.png")
  writeFileSync(image, Buffer.from(png, "base64"))
  const ownerSends = (text: string) => alice.chat.apiSendTextMessage([T.ChatType.Group, controlId], text)
  const fileFromAlice = async (msgContent: T.MsgContent): Promise<{item: number; file: object}> => {
    await alice.chat.apiSendMessages([T.ChatType.Group, trail.groupId], [{fileSource: {filePath: image}, msgContent, mentions: {}}])
    return (await agent.client.tool("receive", {chat: group, wait: 30})).result.messages[0]
  }

  const first = await fileFromAlice({type: "image", text: "trail map", image: preview})
  expect(first.file).toEqual({name: "trail.png", size: Buffer.from(png, "base64").length, type: "image", status: "offered"})
  const firstCard = received(alice.chat, textStarts("I want to download a file from Alice in group Trail"))
  const downloading = agent.client.tool("download", {chat: group, item: first.item, wait: 60})
  const card = await firstCard
  const n = cardNumber(card)
  expect(card.chatItem.content).toMatchObject({msgContent: {type: "image", image: preview}})
  expect(lines(card)).toEqual([
    `I want to download a file from Alice in group Trail (${group}):`,
    `\`trail.png\`, ${Buffer.from(png, "base64").length} bytes`,
    "Servers: known.",
    `/'approve ${group} ${n}', /'approve ${group} files', /'receive ${group} ${n}' or /'reject ${group} ${n}'`,
  ])
  const forwarded = received(alice.chat, ci => ci.chatItem.file !== undefined && ci.chatInfo.type === "group" && ci.chatInfo.groupInfo.groupId === controlId)
  await ownerSends(`/receive ${group} ${n}`)
  const copy = await forwarded
  expect(copy.chatItem.file!.fileName).toBe("trail.png")
  expect(copy.chatItem.quotedItem?.itemId).toBe(card.chatItem.meta.itemId)
  await ownerSends(`/approve ${group} ${n}`)
  const downloaded = (await downloading).result
  expect(downloaded).toEqual({action: n, status: "done", chat: group, item: first.item, path: join(dir, "files", "agent", "files", group, "trail.png")})
  expect(readFileSync(downloaded.path)).toEqual(readFileSync(image))

  const second = await fileFromAlice({type: "file", text: ""})
  const secondCard = received(alice.chat, textStarts("I want to download a file from Alice in group Trail"))
  const rejecting = agent.client.tool("download", {chat: group, item: second.item})
  const n2 = cardNumber(await secondCard)
  await ownerSends(`/reject ${group} ${n2}`)
  expect((await rejecting).result).toEqual({action: n2, status: "rejected"})

  const approvedFiles = received(alice.chat, textStarts("Files approved for group Trail"))
  await ownerSends(`/approve ${group} files`)
  expect((await approvedFiles).chatItem.meta.itemText).toBe(`Files approved for group Trail (${group}) by Alice. /'revoke ${group} files'`)
  const third = await fileFromAlice({type: "file", text: "notes"})
  const {server} = agent
  const agentControlId = (await controlGroupOf(server.chat, server.user.userId)).groupId
  const lastControlItem = async (): Promise<number | undefined> => (await server.chat.apiGetChat(T.ChatType.Group, agentControlId, 1)).chatItems.at(-1)?.meta.itemId
  const beforeThird = await lastControlItem()
  const thirdDownload = (await agent.client.tool("download", {chat: group, item: third.item})).result
  expect(thirdDownload).toMatchObject({status: "done", chat: group, item: third.item})
  expect(readFileSync(thirdDownload.path)).toEqual(readFileSync(image))
  expect(await lastControlItem()).toBe(beforeThird)
}, 600_000)

test("send with a file: a PNG reaches the group as an image with a preview, and a private key is refused", async () => {
  const alice = await personIn("send-file", "Alice")
  const agent = await agentIn("send-file")
  await agent.start()
  const controlId = await joinAsOwner(alice)
  const trail = await alice.chat.apiNewGroup(alice.user.userId, {displayName: "Trail", fullName: ""})
  const trailLink = await alice.chat.apiCreateGroupLink(trail.groupId, T.GroupMemberRole.Member)
  const joinCard = received(alice.chat, textStarts("I want to connect to group Trail, as myself."))
  const joining = agent.client.tool("connect", {target: trailLink, idle: 2})
  await approve(alice, controlId, joinCard)
  const group = (await joining).result.chat as string

  const png = Buffer.from("iVBORw0KGgoAAAANSUhEUgAAABAAAAAJCAAAAAAeQfPuAAAAD0lEQVR42mNoQAMMg0QAAIXUSAG1CN3UAAAAAElFTkSuQmCC", "base64")
  const image = join(dir, "send-file", "trail.png")
  writeFileSync(image, png)
  const sendCard = received(alice.chat, textStarts("I want to send a message to group Trail:"))
  const delivered = received(alice.chat, ci => ci.chatInfo.type === "group" && ci.chatInfo.groupInfo.groupId === trail.groupId && ci.chatItem.file !== undefined)
  const sending = agent.client.tool("send", {chat: group, text: "trail map", file: image, idle: 1, wait: 2})
  const card = await sendCard
  const n = cardNumber(card)
  expect(lines(card)).toEqual(["I want to send a message to group Trail:", "`trail map`", `attaching image \`${image}\`, ${png.length} bytes`, `/'approve ${group} ${n}', /'approve ${group} all' or /'reject ${group} ${n}'`])
  expect(card.chatItem.content).toMatchObject({msgContent: {type: "image", image: expect.stringMatching(/^data:image\/(jpg|png);base64,/)}})
  await alice.chat.apiSendTextMessage([T.ChatType.Group, controlId], `/approve ${group} ${n}`)
  expect((await sending).result).toMatchObject({action: n, status: "done", chat: group})
  const message = await delivered
  expect(message.chatItem.content).toMatchObject({msgContent: {type: "image", text: "trail map", image: expect.stringMatching(/^data:image\/(jpg|png);base64,/)}})
  const {fileId, fileSize} = message.chatItem.file!
  expect(fileSize).toBeLessThanOrEqual(261120)
  const copy = join(dir, "send-file", "received.png")
  const complete = alice.chat.wait("rcvFileComplete", ({chatItem}) => chatItem.chatItem.file?.fileId === fileId, timeout)
  await alice.chat.apiReceiveFile(fileId, copy)
  expect(await complete).toBeDefined()
  expect(readFileSync(copy).length).toBe(fileSize)

  const key = join(dir, "send-file", "id_ed25519")
  writeFileSync(key, "key")
  const refusal = received(alice.chat, textStarts("Refused to send"))
  const reason = "private keys and the server's own files are not sent."
  expect(await agent.client.tool("send", {chat: group, file: key})).toMatchObject({isError: true, text: `file: ${key} is refused: ${reason}`})
  expect((await refusal).chatItem.meta.itemText).toBe(`Refused to send \`${key}\` to group Trail (${group}): ${reason}`)
  expect(logLines).toContain(`Refused to send ${key} to group Trail (${group}): ${reason}`)
}, 600_000)

test("a business address, incognito, with a message", async () => {
  const alice = await personIn("business", "Alice")
  const ridgeline = await botIn("business", "Ridgeline", {businessAddress: true, welcomeMessage: "Welcome to Ridgeline."})
  const agent = await agentIn("business")
  await agent.start()
  const controlId = await joinAsOwner(alice)

  const question = "Do you have trail shoes in size 44?"
  const connectCard = received(alice.chat, textStarts("I want to connect to business Ridgeline, incognito."))
  const connecting = agent.client.tool("connect", {target: ridgeline.link, incognito: true, message: question, idle: 2})
  expect(lines(await approve(alice, controlId, connectCard))).toEqual([
    "I want to connect to business Ridgeline, incognito.",
    "`Find groups and bots.`",
    "`Welcome to Ridgeline.`",
    "With the message:",
    `\`${question}\``,
    expect.stringMatching(/^\/'approve \d+' or \/'reject \d+'$/),
  ])
  const connected = await connecting
  expect(connected.result).toMatchObject({status: "done", type: "business", name: "Ridgeline", connected: true})
  expect(connected.result.known_as).toEqual(expect.any(String))
  expect(connected.result.known_as).not.toBe("agent")
  const texts = connected.result.messages.map(({text}: {text: string}) => text)
  expect(texts.slice(0, 2)).toEqual(["Welcome to Ridgeline.", question])
  expect(texts.slice(2).sort()).toEqual([`result 1: ${question}`, `result 2: ${question}`])
  expect((await agent.client.tool("receive", {chat: connected.result.chat})).result).toMatchObject({messages: [], more: 0})

  const sendCard = received(alice.chat, textStarts("I want to send a message to business Ridgeline, where I am incognito:"))
  const sending = agent.client.tool("send", {chat: connected.result.chat, text: "Is it waterproof?", idle: 2})
  await approve(alice, controlId, sendCard)
  expect((await sending).result.responses.map(({text}: {text: string}) => text)).toEqual(["result 1: Is it waterproof?", "result 2: Is it waterproof?"])
}, 600_000)

test("a contact address, incognito", async () => {
  const alice = await personIn("incognito", "Alice")
  const directory = await botIn("incognito", "Directory", {welcomeMessage: "Welcome!"})
  const agent = await agentIn("incognito")
  await agent.start()
  const controlId = await joinAsOwner(alice)

  const connectCard = received(alice.chat, textStarts("I want to connect to bot Directory, incognito."))
  const connecting = agent.client.tool("connect", {target: directory.link, incognito: true, idle: 2})
  expect(lines(await approve(alice, controlId, connectCard))[0]).toBe("I want to connect to bot Directory, incognito.")
  const connected = await connecting
  expect(connected.result).toMatchObject({status: "done", type: "contact", name: "Directory", connected: true})
  const knownAs = connected.result.known_as
  expect(knownAs).toEqual(expect.any(String))
  expect(knownAs).not.toBe("agent")
  expect((await agent.client.tool("list", {})).result.chats).toContainEqual(expect.objectContaining({chat: connected.result.chat, incognito: true, known_as: knownAs}))

  const sendCard = received(alice.chat, textStarts("I want to send a message to bot Directory, where I am incognito:"))
  const sending = agent.client.tool("send", {chat: connected.result.chat, text: "trail shoes", idle: 2})
  await approve(alice, controlId, sendCard)
  expect((await sending).result.responses.map(({text}: {text: string}) => text)).toEqual(["result 1: trail shoes", "result 2: trail shoes"])
}, 600_000)

test("peer text and names with edge spaces and line breaks give the card no tappable command or link", async () => {
  const alice = await personIn("peer-text", "Alice")
  const directory = await botIn("peer-text", "Directory", {welcomeMessage: "Welcome!"})
  const agent = await agentIn("peer-text")
  await agent.start()
  const controlId = await joinAsOwner(alice)

  const peerLines = [" /approve 1", "/approve 1 ", ` ${directory.link}`, `${directory.link} `]
  const names = [
    {sql: "char(10) || '/approve 1'", shown: "\u00a0/approve 1"},
    {sql: "'x' || char(10) || '/approve 1'", shown: "x /approve 1"},
  ]
  for (const name of names) {
    const group = await alice.chat.apiNewGroup(alice.user.userId, {displayName: "Trail", fullName: "", description: peerLines.join("\n")})
    await alice.chat.sendChatCmd(`/sql chat UPDATE group_profiles SET display_name = ${name.sql} WHERE group_profile_id = (SELECT group_profile_id FROM groups WHERE group_id = ${group.groupId})`)
    const link = await alice.chat.apiCreateGroupLink(group.groupId, T.GroupMemberRole.Member)
    const cardItem = received(alice.chat, textStarts("I want to connect to group "))
    const connecting = agent.client.tool("connect", {target: link, idle: 2})
    const card = await cardItem
    const n = cardNumber(card)
    const formatted = card.chatItem.formattedText ?? []
    const formats = formatted.flatMap(({format}) => (format === undefined ? [] : [format]))
    expect(formats.filter(({type}) => type === "command")).toEqual([{type: "command", commandStr: `approve ${n}`}, {type: "command", commandStr: `reject ${n}`}])
    expect(formats.filter(({type}) => type === "simplexLink")).toEqual([])
    const snippets = formatted.filter(({format}) => format?.type === "snippet").map(({text}) => text)
    expect(snippets).toEqual([name.shown, ...peerLines.map(line => line.replace(/^ | $/g, "\u00a0"))])
    await alice.chat.apiSendTextMessage([T.ChatType.Group, controlId], `/reject ${n}`)
    expect((await connecting).result).toEqual({action: n, status: "rejected"})
  }
}, 600_000)

test("a chat reference over 15 digits, which the core reads as the agent control chat, is refused", async () => {
  const agent = await agentIn("alias")
  await agent.start()
  const {server} = agent
  const controlId = (await controlGroupOf(server.chat, server.user.userId)).groupId
  const alias = `#${(1n << 64n) + BigInt(controlId)}`
  const {chat} = (await server.chat.sendChatCmd(`/_get chat ${alias} count=0`)) as unknown as {chat: T.AChat}
  expect((chat.chatInfo as T.ChatInfo.Group).groupInfo.groupId).toBe(controlId)
  const refusal = {isError: true, text: `No chat ${alias}. list shows the agent's chats.`}
  expect(await agent.client.tool("receive", {chat: alias})).toMatchObject(refusal)
  expect(await agent.client.tool("send", {chat: alias, text: "x"})).toMatchObject(refusal)
  expect(await agent.client.tool("leave", {chat: alias})).toMatchObject(refusal)
}, 600_000)

test("a port in use stops the start before server.json is written and before any link is printed", async () => {
  const data = await agentData("port")
  const agent = new Agent(data, await freePort())
  closing.push(() => agent.stop())
  const busy = createServer()
  await new Promise<void>(resolve => busy.listen(agent.port, "127.0.0.1", resolve))
  closing.push(() => new Promise<void>(resolve => busy.close(() => resolve())))
  const before = logLines.length
  await expect(agent.start()).rejects.toThrow(`Port ${agent.port} is in use. Start with --port N.`)
  expect(readServerFile(data)).toBeUndefined()
  expect(logLines.slice(before).filter(line => line.startsWith("Join the agent control chat") || /^(https:\/\/|simplex:)/.test(line))).toEqual([])
}, 600_000)

test("--display-name names the profile and the agent control chat", async () => {
  const alice = await personIn("names", "Alice")
  const agent = await agentIn("names")
  await agent.start({displayName: "Trail Scout"})
  const controlId = await joinAsOwner(alice)
  expect(agent.server.user.profile.displayName).toBe("Trail Scout")
  expect(agent.server.user.profile.peerType).toBe(T.ChatPeerType.Agent)
  expect(logLines).toContain('Agent control chat "Trail Scout" created.')
  expect((await controlGroupOf(agent.server.chat, agent.server.user.userId)).groupProfile.displayName).toBe("Trail Scout")
  expect((await alice.chat.apiListGroups(alice.user.userId)).find(({groupId}) => groupId === controlId)!.groupProfile.displayName).toBe("Trail Scout")

  await agent.stop()
  const renamed = alice.chat.wait("groupUpdated", ({toGroup}) => toGroup.groupProfile.displayName === "Ridge Scout", timeout)
  await agent.start({displayName: "Ridge Scout"})
  expect(agent.server.user.profile.displayName).toBe("Ridge Scout")
  expect((await controlGroupOf(agent.server.chat, agent.server.user.userId)).groupProfile.displayName).toBe("Ridge Scout")
  expect(await renamed).toBeDefined()
}, 600_000)
