import {mkdirSync, mkdtempSync, rmSync} from "fs"
import {tmpdir} from "os"
import {join} from "path"
import {T} from "@simplex-chat/types"
import {RunningServer, startServer} from "../../src/serve"
import {McpClient} from "./client"
import {captureLog, cardNumber, person, profileWithServer, received, serveOptions, startBot, textStarts, until, updated} from "./helpers"
import {SmpServer, freePort, startSmpServer} from "./smpServer"

let smpServer: SmpServer
let dir: string
let logLines: string[]

beforeAll(async () => {
  smpServer = await startSmpServer()
  dir = mkdtempSync(join(tmpdir(), "simplex-mcp-e2e-"))
  logLines = captureLog()
}, 120_000)

afterAll(async () => {
  jest.restoreAllMocks()
  await smpServer?.stop()
  rmSync(dir, {recursive: true, force: true})
})

test("agent with an owner, a bot and the four tools", async () => {
  const agentData = join(dir, "agent")
  mkdirSync(agentData, {recursive: true})
  await profileWithServer(smpServer.address, join(agentData, "simplex"), {displayName: "agent", fullName: "", peerType: T.ChatPeerType.Bot})
  const {chat: alice, user: aliceUser} = await person(smpServer.address, join(dir, "alice"), "Alice")
  const {chat: directory, user: directoryUser, link: directoryLink} = await startBot(smpServer.address, join(dir, "directory"), "Directory", {welcomeMessage: "Welcome! Send /help or a search."})

  const port = await freePort()
  let server: RunningServer | undefined
  try {
    server = await startServer(serveOptions(agentData, port))
    const client = new McpClient(`http://127.0.0.1:${port}/mcp`, server.token)

    expect(logLines).toContain('Agent control chat "agent" created.')
    const link = logLines.find(line => /^(https:\/\/|simplex:)/.test(line))!
    expect(link).toBeDefined()
    expect(logLines).toContain(`Whoever joins by this link becomes an owner of "agent" and approves its actions. To approve owners yourself, start with --owners.`)

    const noOwner = await client.tool("connect", {target: directoryLink})
    expect(noOwner).toMatchObject({isError: true, text: "The agent has no owner. The person who runs it has to join its control chat with the link the server printed."})

    const greeting = received(alice, textStarts("I'm your agent."))
    const [, controlLink] = await alice.apiConnectPlan(aliceUser.userId, link)
    await alice.apiConnect(aliceUser.userId, false, controlLink)
    const control = (await greeting).chatInfo as T.ChatInfo.Group
    const controlId = control.groupInfo.groupId
    await until(() => logLines.some(line => /^Alice joined \(member \d+\) and is now an owner\. The link is deleted\.$/.test(line)))
    expect(logLines).toContain("  npx -y @simplex-chat/mcp register agent --data " + agentData)
    const ownerSends = (text: string) => alice.apiSendTextMessage([T.ChatType.Group, controlId], text)

    expect(await client.initialize()).toMatchObject({protocolVersion: "2025-11-25", serverInfo: {name: "simplex-mcp"}})
    const {tools} = await client.request("tools/list")
    expect(tools.map(({name}: {name: string}) => name)).toEqual(["connect", "send", "receive", "list"])

    const connectCard = received(alice, textStarts("I want to connect to bot Directory, as myself."))
    const connecting = client.tool("connect", {target: directoryLink, idle: 2})
    const card1 = await connectCard
    expect(card1.chatItem.meta.itemText).toContain("`Find groups and bots.`")
    expect(card1.chatItem.meta.itemText).toContain("`Welcome! Send /help or a search.`")
    const n1 = cardNumber(card1)
    const connectedCard = updated(alice, ci => ci.chatItem.meta.itemId === card1.chatItem.meta.itemId && ci.chatItem.meta.itemText.startsWith("I connected to bot Directory"))
    await ownerSends(`/approve ${n1}`)
    const connected = await connecting
    expect(connected.isError).toBeUndefined()
    expect(connected.result).toMatchObject({action: n1, status: "done", connected: true, type: "contact", name: "Directory", peer_type: "bot"})
    expect(connected.result.commands).toEqual([{keyword: "help", label: "Help"}])
    expect((await connectedCard).chatItem.meta.itemText.split("\n").at(-1)).toBe("Approved by Alice.")
    const chat = connected.result.chat as string
    expect(chat).toMatch(/^@\d+$/)

    const known = await client.tool("connect", {target: directoryLink})
    expect(known.result).toMatchObject({status: "known", chat, connected: true})

    const sendCard = received(alice, textStarts("I want to send a message to bot Directory:"))
    const sending = client.tool("send", {chat, text: "trail shoes", idle: 2, wait: 30})
    const card2 = await sendCard
    expect(card2.chatItem.meta.itemText).toContain("`trail shoes`")
    await ownerSends(`/approve ${cardNumber(card2)}`)
    const sent = await sending
    expect(sent.result).toMatchObject({action: cardNumber(card2), status: "done", chat, more: 0})
    expect(sent.result.responses.map(({text}: {text: string}) => text)).toEqual(["result 1: trail shoes", "result 2: trail shoes"])
    expect(sent.result.responses.every(({item}: {item: number}) => item > sent.result.item)).toBe(true)

    const helpCard = received(alice, textStarts("I want to send a message to bot Directory:"))
    const helping = client.tool("send", {chat, text: "/help", idle: 2, wait: 30})
    await ownerSends(`/approve ${cardNumber(await helpCard)}`)
    expect((await helping).result.responses.map(({text}: {text: string}) => text)).toEqual(["Send a search, for example: trail shoes"])

    const agentContact = (await directory.apiListContacts(directoryUser.userId)).find(({profile}) => profile.displayName === "agent")!
    const receiving = client.tool("receive", {chat, wait: 30})
    await new Promise(resolve => setTimeout(resolve, 500))
    await directory.apiSendTextMessage([T.ChatType.Direct, agentContact.contactId], "news: new trail groups")
    const news = await receiving
    expect(news.result).toMatchObject({chat, more: 0})
    expect(news.result.messages.map(({text}: {text: string}) => text)).toEqual(["news: new trail groups"])
    expect((await client.tool("receive", {chat})).result).toEqual({chat, messages: [], more: 0})

    const rejectCard = received(alice, textStarts("I want to send a message to bot Directory:"))
    const rejecting = client.tool("send", {chat, text: "not this one"})
    const card3 = await rejectCard
    const rejectedCard = updated(alice, ci => ci.chatItem.meta.itemId === card3.chatItem.meta.itemId)
    await ownerSends(`/reject ${cardNumber(card3)}`)
    expect((await rejecting).result).toEqual({action: cardNumber(card3), status: "rejected"})
    expect((await rejectedCard).chatItem.meta.itemText.split("\n").at(-1)).toBe("Rejected by Alice.")

    const notPending = received(alice, textStarts(`Action ${cardNumber(card3)} is not pending.`))
    await ownerSends(`/approve ${cardNumber(card3)}`)
    await notPending

    const lostCard = received(alice, textStarts("I want to send a message to bot Directory:"))
    const abort = new AbortController()
    const lost = client.tool("send", {chat, text: "lost result", idle: 2}, abort.signal)
    const card4 = await lostCard
    const n4 = cardNumber(card4)
    abort.abort()
    await expect(lost).rejects.toThrow()
    await until(() => server!.actions.get(n4)!.open === 0)
    const listed = await client.tool("list", {})
    expect(listed.result.actions).toEqual([{action: n4, tool: "send", input: {chat, text: "lost result", idle: 2}, state: "pending", open: false}])
    expect(listed.result.chats).toEqual([
      expect.objectContaining({chat, type: "contact", name: "Directory", connected: true, incognito: false, unread: 0}),
      expect.objectContaining({type: "contact", name: "Ask SimpleX Team", connected: false}),
    ])
    await ownerSends(`/approve ${n4}`)
    await until(() => server!.actions.get(n4)!.state === "done")
    const attached = await client.tool("send", {chat, text: "lost result", idle: 2})
    expect(attached.result).toMatchObject({action: n4, status: "done", more: 0})
    expect(attached.result.responses.map(({text}: {text: string}) => text)).toEqual(["result 1: lost result", "result 2: lost result"])
    expect((await client.tool("list", {})).result.actions).toEqual([])

    const unknown = await client.tool("send", {chat: "#999", text: "x"})
    expect(unknown).toMatchObject({isError: true, text: "No chat #999. list shows the agent's chats."})

    const pendingCard = received(alice, textStarts("I want to send a message to bot Directory:"))
    void client.tool("send", {chat, text: "at the stop"}).catch(() => undefined)
    const card5 = await pendingCard
    const n5 = cardNumber(card5)
    const cancelledCard = updated(alice, ci => ci.chatItem.meta.itemId === card5.chatItem.meta.itemId)
    await server.stop()
    server = undefined
    expect(logLines).toContain("Cancelled 1 action.")
    expect(logLines).toContain("Stopped.")
    expect((await cancelledCard).chatItem.meta.itemText.split("\n").at(-1)).toBe("Cancelled: the server stopped.")

    server = await startServer(serveOptions(agentData, port))
    const refused = received(alice, textStarts(`Action ${n5} is not pending.`))
    await ownerSends(`/approve ${n5}`)
    await refused
    const restartedCard = received(alice, textStarts("I want to send a message to bot Directory:"))
    void client.tool("send", {chat, text: "after the restart"}).catch(() => undefined)
    expect(cardNumber(await restartedCard)).toBe(n5 + 1)
  } finally {
    await server?.stop()
    await alice.close()
    await directory.close()
  }
}, 600_000)
