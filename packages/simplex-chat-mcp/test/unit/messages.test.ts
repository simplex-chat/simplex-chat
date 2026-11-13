import * as childProcess from "child_process"
import * as fs from "fs"
import {chmodSync, existsSync, mkdirSync, mkdtempSync, readFileSync, readdirSync, realpathSync, rmSync, symlinkSync, writeFileSync} from "fs"
import * as os from "os"
import {basename, dirname, join} from "path"
import {CallToolResult} from "@modelcontextprotocol/server"
import {T} from "@simplex-chat/types"
import {core} from "simplex-chat"
import {Action, Actions, ConnectAction, DownloadAction, Mode, SendAction, Standing} from "../../src/actions"
import {cardText} from "../../src/control"
import {Changes, ChatItemsWatch, Collected, awaitResponses, chatRef, nextChange, toMessage} from "../../src/messages"
import {AgentTools, classifyPlan} from "../../src/tools"
import {FakeChat, FakeHooks, actionsWith, broadcastDeletion, contact, data, direct, event, fakeHooks, fileItem, group, groupChat, item, livePost, member, settled, signal, texts, user} from "./fixtures"

const createdTools: AgentTools[] = []

beforeEach(() => {
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(async () => {
  for (const tools of createdTools.splice(0)) await tools.statuses.stop()
  jest.restoreAllMocks()
  jest.useRealTimers()
})

afterAll(() => rmSync(data, {recursive: true, force: true}))

describe("message fields", () => {
  const self = {name: "agent", type: "agent"}
  const noRejections = new Set<number>()

  it("describes a group message", () => {
    const switchback = member(44, "Switchback")
    switchback.memberProfile.peerType = T.ChatPeerType.Bot
    const chatInfo = groupChat(group(15, "Trail"))
    expect(toMessage(chatInfo, item(31, "Ridgecrest, $142", {type: "groupRcv", groupMember: switchback}, {quoted: 30, mention: true, ts: "2026-10-17T11:00:00Z"}), self, noRejections, data)).toEqual({
      chat: "#15",
      item: 31,
      from: "Switchback",
      from_member: 44,
      from_type: "bot",
      time: "2026-10-17T11:00:00Z",
      text: "Ridgecrest, $142",
      new: true,
      reply_to: {chat: "#15", item: 30},
      mentions_me: true,
    })
  })

  it("describes direct, channel and sent messages", () => {
    const directory = direct(contact(4, "SimpleX Directory", {peerType: "bot"}))
    expect(toMessage(directory, item(21, "found 3", {type: "directRcv"}, {status: "rcvRead"}), self, noRejections, data)).toMatchObject({chat: "@4", from: "SimpleX Directory", from_type: "bot", new: false})
    expect(toMessage(directory, item(20, "shoes", {type: "directSnd"}), self, noRejections, data)).toEqual({
      chat: "@4",
      item: 20,
      from: "agent",
      from_type: "agent",
      time: "2026-10-17T10:00:00Z",
      text: "shoes",
      new: false,
      mentions_me: false,
    })
    const channel = toMessage(groupChat(group(12, "Ridgeline News")), item(40, "post", {type: "channelRcv"}), self, noRejections, data)
    expect(channel).toMatchObject({chat: "#12", from: "Ridgeline News"})
    expect(channel).not.toHaveProperty("from_member")
  })

  it("describes the file of a received message by its status", () => {
    const chatInfo = direct(contact(4, "Ridgeline"))
    const downloaded = join(data, "files", "contact-4", "7.pdf")
    const fileOf = (msgType: string, fileStatus: object, fileSource?: object, rejected: Set<number> = noRejections) =>
      toMessage(chatInfo, fileItem(30, {type: msgType, text: "report"}, fileStatus, {fileSource}), self, rejected, data).file
    expect(fileOf("file", {type: "rcvInvitation"})).toEqual({name: "report.pdf", size: 2400000, type: "file", status: "offered"})
    expect(fileOf("file", {type: "rcvComplete"}, {filePath: downloaded}, new Set([7]))).toEqual({name: "report.pdf", size: 2400000, type: "file", status: "rejected"})
    expect(fileOf("image", {type: "rcvTransfer", rcvProgress: 1, rcvTotal: 2})).toEqual({name: "report.pdf", size: 2400000, type: "image", status: "offered"})
    expect(fileOf("video", {type: "rcvComplete"}, {filePath: downloaded})).toEqual({name: "report.pdf", size: 2400000, type: "video", status: "failed", error: "The received file is not in the agent's files folder."})
    mkdirSync(dirname(downloaded), {recursive: true})
    writeFileSync(downloaded, "report")
    expect(fileOf("video", {type: "rcvComplete"}, {filePath: downloaded})).toEqual({name: "report.pdf", size: 2400000, type: "video", status: "received", path: downloaded})
    expect(fileOf("voice", {type: "rcvComplete"}, {filePath: join(data, "report.pdf")})).toEqual({name: "report.pdf", size: 2400000, type: "voice", status: "offered"})
    const forwarded = join(data, "forwarded", "contact-4", "7.pdf")
    expect(fileOf("file", {type: "rcvComplete"}, {filePath: forwarded})).toEqual({name: "report.pdf", size: 2400000, type: "file", status: "received", path: downloaded})
    rmSync(downloaded)
    mkdirSync(dirname(forwarded), {recursive: true})
    writeFileSync(forwarded, "report")
    expect(fileOf("file", {type: "rcvComplete"}, {filePath: forwarded})).toEqual({name: "report.pdf", size: 2400000, type: "file", status: "offered"})
    rmSync(forwarded)
    expect(fileOf("voice", {type: "rcvCancelled"})).toMatchObject({type: "voice", status: "failed", error: "The file was cancelled."})
    expect(fileOf("text", {type: "rcvAborted"})).toMatchObject({type: "file", status: "failed", error: "The download was aborted."})
    expect(fileOf("file", {type: "rcvError", rcvFileError: {type: "blocked", server: "xftp://k@xftp.example.com", blockInfo: {reason: "spam"}}})!.error).toBe("File is blocked by server operator: Spam.")
    expect(fileOf("file", {type: "rcvError", rcvFileError: {type: "relay", srvError: {type: "host"}}})!.error).toBe("File server error: Server address is incompatible with network settings.")
    expect(fileOf("file", {type: "rcvError", rcvFileError: {type: "other", fileError: "file digest"}})!.error).toBe("Error: file digest")
    expect(fileOf("file", {type: "invalid", text: "bad\nstatus"})!.error).toBe("Invalid file: bad status")
    const large = fileItem(33, {type: "file", text: "report"}, {type: "rcvInvitation"})
    const prohibited = {...large, file: {...large.file!, fileProhibited: {maxSize: 1048576, badgeStatus: T.BadgeStatus.Expired}}}
    expect(toMessage(chatInfo, prohibited, self, noRejections, data).file).toEqual({name: "report.pdf", size: 2400000, type: "file", status: "failed", error: "Large file! The file is above the limit of 1048576 bytes for its sender. Sender's badge expired."})
    expect(toMessage(chatInfo, fileItem(31, {type: "file", text: "report"}, {type: "sndComplete"}, {chatDir: {type: "directSnd"}, fileSource: {filePath: "/home/alice/report.pdf"}}), self, noRejections, data)).not.toHaveProperty("file")
    expect(toMessage(chatInfo, item(32, "no file"), self, noRejections, data)).not.toHaveProperty("file")
  })
})

describe("next change", () => {
  it("waits with no timer for a change or the signal when no time limit is given", async () => {
    jest.useFakeTimers()
    const changes = new Changes()
    const changed = nextChange(changes, signal().call.signal)
    expect(jest.getTimerCount()).toBe(0)
    changes.notify()
    expect(await changed).toBe(true)
    const {controller, call} = signal()
    const aborted = nextChange(changes, call.signal)
    expect(jest.getTimerCount()).toBe(0)
    controller.abort()
    expect(await aborted).toBe(false)
  })

  it("ends at the time limit when one is given", async () => {
    jest.useFakeTimers()
    let finished = false
    const waiting = nextChange(new Changes(), signal().call.signal, 1_000).then(result => (finished = result))
    await jest.advanceTimersByTimeAsync(999)
    expect(finished).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    expect(await waiting).toBe(true)
    expect(jest.getTimerCount()).toBe(0)
  })
})

describe("responses", () => {
  it("ends idle seconds after the last response", async () => {
    jest.useFakeTimers()
    const collected = new Collected()
    let finished = false
    const waiting = awaitResponses(collected, Date.now() + 60_000, 5, signal().call).then(result => (finished = result))
    await jest.advanceTimersByTimeAsync(10_000)
    expect(finished).toBe(false)
    collected.lastAt = Date.now()
    collected.changes.notify()
    await jest.advanceTimersByTimeAsync(4_999)
    expect(finished).toBe(false)
    collected.lastAt = Date.now()
    collected.changes.notify()
    await jest.advanceTimersByTimeAsync(4_999)
    expect(finished).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    expect(await waiting).toBe(true)
  })

  it("ends at wait without responses, and on the signal", async () => {
    jest.useFakeTimers()
    const collected = new Collected()
    let finished = false
    const waiting = awaitResponses(collected, Date.now() + 3_000, 5, signal().call).then(result => (finished = result))
    await jest.advanceTimersByTimeAsync(2_999)
    expect(finished).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    expect(await waiting).toBe(true)
    const {controller, call} = signal()
    const aborted = awaitResponses(collected, Date.now() + 60_000, 5, call)
    controller.abort()
    expect(await aborted).toBe(false)
  })

  it("waits while a live message is updated, and ends idle seconds after its last update", async () => {
    jest.useFakeTimers()
    const watch = new ChatItemsWatch()
    const chatInfo = direct(contact(4, "Switchback"))
    const collected = new Collected()
    const stop = watch.collect("@4", collected)
    let finished = false
    const waiting = awaitResponses(collected, Date.now() + 60_000, 5, signal().call).then(result => (finished = result))
    watch.deliver([event(chatInfo, item(30, "Ridge", {type: "directRcv"}, {live: true}))])
    await jest.advanceTimersByTimeAsync(10_000)
    expect(finished).toBe(false)
    watch.update(event(chatInfo, item(30, "Ridgecrest", {type: "directRcv"}, {live: true})))
    await jest.advanceTimersByTimeAsync(10_000)
    expect(finished).toBe(false)
    watch.update(event(chatInfo, item(30, "Ridgecrest, $142", {type: "directRcv"}, {live: false})))
    await jest.advanceTimersByTimeAsync(4_999)
    expect(finished).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    expect(await waiting).toBe(true)
    expect(collected.items.map(({chatItem}) => chatItem.meta.itemText)).toEqual(["Ridgecrest, $142"])
    watch.update(event(chatInfo, item(31, "not collected", {type: "directRcv"})))
    expect(collected.items).toHaveLength(1)
    stop()
  })

  it("keeps 100 messages and the IDs of the later ones as overflow, until they are deleted", () => {
    const watch = new ChatItemsWatch()
    const chatInfo = direct(contact(4, "Switchback"))
    const collected = new Collected()
    const stop = watch.collect("@4", collected)
    watch.deliver(Array.from({length: 102}, (_, i) => event(chatInfo, item(100 + i, `post ${i}`))))
    expect(collected.items.map(({chatItem}) => chatItem.meta.itemId)).toEqual(Array.from({length: 100}, (_, i) => 100 + i))
    expect([...collected.overflow]).toEqual([200, 201])
    watch.update(event(chatInfo, item(101, "edited")))
    watch.update(event(chatInfo, item(201, "edited")))
    expect(collected.items[1].chatItem.meta.itemText).toBe("edited")
    expect(collected.items).toHaveLength(100)
    expect([...collected.overflow]).toEqual([200, 201])
    watch.remove([event(chatInfo, item(201, "edited")), event(chatInfo, item(999, "other"))])
    expect(collected.items).toHaveLength(100)
    expect([...collected.overflow]).toEqual([200])
    watch.remove([event(chatInfo, item(100, "post 0"))])
    watch.update(event(chatInfo, item(200, "edited")))
    expect(collected.items).toHaveLength(99)
    expect([...collected.overflow]).toEqual([200])
    stop()
  })
})

function textOf(result: CallToolResult | undefined): string {
  return (result!.content[0] as {text: string}).text
}

function agentTools(chat: FakeChat, actions: Actions = actionsWith("requests", fakeHooks()), standingApprover: (chatInfo: T.ChatInfo, standing: Standing) => string | undefined = () => undefined, session?: number): {tools: AgentTools; watch: ChatItemsWatch} {
  const watch = new ChatItemsWatch()
  const tools = new AgentTools({chat: chat.api(), user, actions, watch, controlChat: () => "#10", standingApprover, refusedFile: () => undefined, data}, actions.mode, session)
  createdTools.push(tools)
  return {tools, watch}
}

function approvedTools(chat: FakeChat, mode: Mode = "requests"): {tools: AgentTools; watch: ChatItemsWatch; actions: Actions; fake: FakeHooks; approved: Set<string>} {
  const fake = fakeHooks()
  const actions = actionsWith(mode, fake)
  const approved = new Set(["@4"])
  const {tools, watch} = agentTools(chat, actions, (chatInfo, standing) => (standing === "messages" && approved.has(chatRef(chatInfo) ?? "") ? "Alice" : undefined))
  fake.hooks.perform = (action, signal) => tools.perform(action, signal)
  return {tools, watch, actions, fake, approved}
}

async function statusChat(): Promise<ReturnType<typeof approvedTools> & {chat: FakeChat; status: number}> {
  jest.useFakeTimers()
  const chat = new FakeChat()
  chat.chats.set("@4", {chatInfo: direct(contact(4, "Alice")), items: [item(20, "summarize README.md")]})
  const agent = approvedTools(chat)
  await agent.tools.call("receive", {chat: "@4", typing: true}, signal().call)
  await jest.advanceTimersByTimeAsync(0)
  return {...agent, chat, status: chat.nextItemId - 1}
}

describe("receive", () => {
  it("returns unread messages and messages arriving during the call, once each, and marks them read", async () => {
    const chat = new FakeChat()
    const chatInfo = direct(contact(4, "SimpleX Directory"))
    chat.chats.set("@4", {chatInfo, items: [item(20, "old", {type: "directRcv"}, {status: "rcvRead"}), item(21, "first", {type: "directRcv"})]})
    const {tools, watch} = agentTools(chat)
    const call = tools.call("receive", {chat: "@4", wait: 5}, signal().call)
    const other = tools.call("receive", {chat: "@4"}, signal().call)
    watch.deliver([event(chatInfo, item(21, "first", {type: "directRcv"})), chat.addItem("@4", item(22, "second", {type: "directRcv"})), event(chatInfo, item(23, "third", {type: "directRcv"}))])
    const result = (await call)!
    expect(result.isError).toBeUndefined()
    const {messages, more} = result.structuredContent as {messages: {item: number; text: string; new: boolean}[]; more: number}
    expect(messages.map(({item, text}) => [item, text])).toEqual([[21, "first"], [22, "second"], [23, "third"]])
    expect(messages.every(message => message.new)).toBe(true)
    expect(more).toBe(0)
    expect(chat.commands).toContain("/_read chat items @4 21,22,23")
    const concurrent = (await other)!.structuredContent as {messages: {item: number}[]}
    expect(concurrent.messages.map(({item}) => item)).toEqual([21, 22, 23])
  })

  it("waits for a new message", async () => {
    const chat = new FakeChat()
    const chatInfo = direct(contact(4, "SimpleX Directory"))
    chat.chats.set("@4", {chatInfo, items: []})
    const {tools, watch} = agentTools(chat)
    const {call, progress} = signal()
    const receiving = tools.call("receive", {chat: "@4", wait: 30, limit: 1}, call)
    await settled()
    expect(progress[0]).toMatch(/^No new messages in @4; waiting until /)
    watch.deliver([chat.addItem("@4", item(30, "a", {type: "directRcv"})), chat.addItem("@4", item(31, "b", {type: "directRcv"}))])
    const result = (await receiving)!.structuredContent as {messages: {item: number}[]; more: number}
    expect(result.messages.map(({item}) => item)).toEqual([30])
    expect(result.more).toBe(1)
  })

  it("counts the overflow in more", async () => {
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory")), items: []})
    const {tools, watch} = agentTools(chat)
    const {call, progress} = signal()
    const receiving = tools.call("receive", {chat: "@4", wait: 30}, call)
    await settled()
    expect(progress[0]).toMatch(/^No new messages in @4; waiting until /)
    watch.deliver(Array.from({length: 102}, (_, i) => chat.addItem("@4", item(30 + i, `post ${i}`))))
    watch.remove([event(chat.chats.get("@4")!.chatInfo, item(131, "post 101"))])
    const result = (await receiving)!.structuredContent as {messages: {item: number}[]; more: number}
    expect(result.messages).toHaveLength(50)
    expect(result.more).toBe(51)
  })

  it("reports a store error of a later read as that error, and an unknown chat only for the first read", async () => {
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory")), items: [item(21, "first", {type: "directRcv"})]})
    const sendChatCmd = chat.sendChatCmd.bind(chat)
    chat.sendChatCmd = async cmd => {
      if (cmd.includes(" around=")) throw new core.ChatAPIError("Chat command error", {type: "errorStore", storeError: {type: "chatItemNotFound", itemId: 21}} as unknown as T.ChatError)
      return sendChatCmd(cmd)
    }
    const {tools} = agentTools(chat)
    expect(textOf(await tools.call("receive", {chat: "@4"}, signal().call))).toBe('SimpleX error: errorStore: chatItemNotFound: {"itemId":21}')
    expect(textOf(await tools.call("receive", {chat: "@5"}, signal().call))).toBe("No chat @5. list shows the agent's chats.")
  })

  it("returns a live unread message after its final update", async () => {
    const chat = new FakeChat()
    const chatInfo = direct(contact(4, "Switchback"))
    chat.chats.set("@4", {chatInfo, items: [item(30, "Ridge", {type: "directRcv"}, {live: true})]})
    const {tools, watch} = agentTools(chat)
    const {call, progress} = signal()
    const receiving = tools.call("receive", {chat: "@4", wait: 30}, call)
    await new Promise(resolve => setImmediate(resolve))
    expect(progress[0]).toMatch(/^No new messages in @4; waiting until /)
    watch.update(event(chatInfo, item(30, "Ridgecrest, $142", {type: "directRcv"}, {live: false})))
    const result = (await receiving)!.structuredContent as {messages: {item: number; text: string}[]; more: number}
    expect(result.messages.map(({item, text}) => [item, text])).toEqual([[30, "Ridgecrest, $142"]])
    expect(result.more).toBe(0)
    expect(chat.commands.at(-1)).toBe("/_read chat items @4 30")
  })

  it("refuses the agent control chat and unknown chats", async () => {
    const {tools} = agentTools(new FakeChat())
    expect(textOf(await tools.call("receive", {chat: "#10"}, signal().call))).toBe("No chat #10. list shows the agent's chats.")
    expect(textOf(await tools.call("receive", {chat: "@99"}, signal().call))).toBe("No chat @99. list shows the agent's chats.")
    expect((await tools.call("receive", {chat: "4"}, signal().call))!.isError).toBe(true)
  })

  it("refuses chat references longer than 15 digits, such as an alias of the agent control chat", async () => {
    const chat = new FakeChat()
    chat.chats.set("#10", {chatInfo: groupChat(group(10, "agent")), items: [item(1, "/approve 5", {type: "groupRcv", groupMember: member(3, "Alice")})]})
    const {tools} = agentTools(chat)
    const alias = `#${(1n << 64n) + 10n}`
    for (const ref of [alias, "@1234567890123456"]) {
      expect(textOf(await tools.call("receive", {chat: ref}, signal().call))).toBe(`No chat ${ref}. list shows the agent's chats.`)
      expect(textOf(await tools.call("send", {chat: ref, text: "x"}, signal().call))).toBe(`No chat ${ref}. list shows the agent's chats.`)
      expect(textOf(await tools.call("leave", {chat: ref}, signal().call))).toBe(`No chat ${ref}. list shows the agent's chats.`)
    }
    expect(chat.commands).toEqual([])
  })

  it("stops reading after limit + 1 new messages and counts the unread rest in more", async () => {
    const chat = new FakeChat()
    const items = Array.from({length: 450}, (_, i) => item(100 + i, `m${i}`, {type: "directRcv"}, {status: i < 50 ? "rcvRead" : "rcvNew"}))
    chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory")), items})
    const {tools} = agentTools(chat)
    const result = (await tools.call("receive", {chat: "@4", limit: 150}, signal().call))!.structuredContent as {messages: {item: number}[]; more: number}
    expect(result.messages.map(({item}) => item)).toEqual(Array.from({length: 150}, (_, i) => 150 + i))
    expect(result.more).toBe(250)
    expect(chat.commands.filter(cmd => cmd.startsWith("/_get chat"))).toEqual(["/_get chat @4 initial=1", "/_get chat @4 around=150 count=0", "/_get chat @4 after=150 count=100", "/_get chat @4 after=250 count=100"])
    const rest = (await tools.call("receive", {chat: "@4", limit: 100}, signal().call))!.structuredContent as {messages: {item: number}[]; more: number}
    expect(rest.messages.map(({item}) => item)).toEqual(Array.from({length: 100}, (_, i) => 300 + i))
    expect(rest.more).toBe(150)
  })

  it("reads on past live messages, so a message that arrives during the call never passes an unread one", async () => {
    const chat = new FakeChat()
    const chatInfo = direct(contact(4, "SimpleX Directory"))
    chat.chats.set("@4", {chatInfo, items: Array.from({length: 500}, (_, i) => item(100 + i, `m${i}`, {type: "directRcv"}, {live: i < 120}))})
    const {tools, watch} = agentTools(chat)
    const receiving = tools.call("receive", {chat: "@4", limit: 150}, signal().call)
    watch.deliver([event(chatInfo, item(999, "late", {type: "directRcv"}))])
    const result = (await receiving)!.structuredContent as {messages: {item: number}[]; more: number}
    expect(result.messages.map(({item}) => item)).toEqual(Array.from({length: 150}, (_, i) => 220 + i))
    expect(result.more).toBe(351)
    expect(chat.commands.filter(cmd => cmd.startsWith("/_get chat")).slice(-3)).toEqual(["/_get chat @4 after=100 count=100", "/_get chat @4 after=200 count=100", "/_get chat @4 after=300 count=100"])
  })

  it("counts only messages in more when it reads every unread item, and marks the other unread items it read", async () => {
    const chat = new FakeChat()
    const event = (itemId: number): T.ChatItem => ({...item(itemId, "", {type: "directRcv"}), content: {type: "rcvDecryptionError"}}) as unknown as T.ChatItem
    chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory")), items: [event(20), item(21, "a", {type: "directRcv"}), event(22), item(23, "b", {type: "directRcv"}), event(24)]})
    const {tools} = agentTools(chat)
    const first = (await tools.call("receive", {chat: "@4", limit: 1}, signal().call))!.structuredContent as {messages: {item: number}[]; more: number}
    expect(first).toMatchObject({messages: [{item: 21}], more: 1})
    expect(chat.commands.at(-1)).toBe("/_read chat items @4 21,20,22,24")
    const second = (await tools.call("receive", {chat: "@4", limit: 1}, signal().call))!.structuredContent as {messages: {item: number}[]; more: number}
    expect(second).toMatchObject({messages: [{item: 23}], more: 0})
  })

  it("starts a status for returned messages with typing in an approved direct chat with a person", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Alice")), items: []})
    const {tools, watch} = approvedTools(chat)
    const {call, progress} = signal()
    const receiving = tools.call("receive", {chat: "@4", wait: 3600, typing: true}, call)
    await jest.advanceTimersByTimeAsync(0)
    expect(progress[0]).toMatch(/^No new messages in @4; waiting until /)
    expect(chat.sent).toEqual([])
    watch.deliver([chat.addItem("@4", item(20, "summarize README.md"))])
    expect((await receiving)!.structuredContent).toMatchObject({messages: [{item: 20, text: "summarize README.md"}], more: 0})
    await jest.advanceTimersByTimeAsync(0)
    expect(chat.sent).toEqual([livePost(T.ChatType.Direct, 4, "◐ thinking · 0:00")])
  })

  it("starts a status for returned messages with typing in an approved group and in an approved business chat", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    chat.chats.set("#15", {chatInfo: groupChat(group(15, "Trail", member(1, "agent"))), items: [item(20, "summarize README.md", {type: "groupRcv", groupMember: member(44, "Alice")})]})
    chat.chats.set("#16", {chatInfo: groupChat(group(16, "Ridgeline", member(1, "agent"), {businessChat: {}})), items: [item(21, "where is my order?", {type: "groupRcv", groupMember: member(45, "Bob")})]})
    const {tools, approved} = approvedTools(chat)
    for (const ref of ["#15", "#16"]) {
      approved.add(ref)
      expect((await tools.call("receive", {chat: ref, typing: true}, signal().call))!.structuredContent).toMatchObject({messages: [{chat: ref}], more: 0})
    }
    await jest.advanceTimersByTimeAsync(0)
    expect([...chat.sent].sort((a, b) => a.chat[1] - b.chat[1])).toEqual([15, 16].map(groupId => livePost(T.ChatType.Group, groupId, "◐ thinking · 0:00")))
  })

  it("starts no status for an aborted call, without typing, without returned messages, without the approval, with a bot or an agent, with a disconnected contact, in a channel, with the observer role in a group, after the control chat is lost, or in mode host", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Alice")), items: [item(20, "hi")]})
    chat.chats.set("@5", {chatInfo: direct(contact(5, "Bob")), items: [item(21, "hi")]})
    chat.chats.set("@7", {chatInfo: direct(contact(7, "Switchback", {peerType: "bot"})), items: [item(22, "hi")]})
    chat.chats.set("@8", {chatInfo: direct(contact(8, "Scout", {peerType: "agent"})), items: [item(23, "hi")]})
    chat.chats.set("@2", {chatInfo: direct({...contact(2, "Carol"), activeConn: undefined}), items: [item(24, "hi")]})
    chat.chats.set("#13", {chatInfo: groupChat(group(13, "Ridgeline News", member(1, "agent"), {useRelays: true})), items: [item(25, "hi", {type: "channelRcv"})]})
    chat.chats.set("#12", {chatInfo: groupChat(group(12, "News", member(1, "agent", T.GroupMemberRole.Observer))), items: [item(26, "hi", {type: "groupRcv", groupMember: member(44, "Switchback")})]})
    chat.chats.set("@6", {chatInfo: direct(contact(6, "Dana")), items: [item(27, "hi")]})
    const {tools, actions, approved} = approvedTools(chat)
    for (const ref of ["@7", "@8", "@2", "#13", "#12", "@6"]) approved.add(ref)
    const returned = async (target: AgentTools, args: object): Promise<number> => ((await target.call("receive", args, signal().call))!.structuredContent as {messages: object[]}).messages.length
    const {controller, call} = signal()
    controller.abort()
    expect(await tools.call("receive", {chat: "@4", typing: true}, call)).toBeUndefined()
    expect(await returned(tools, {chat: "@4"})).toBe(1)
    expect(await returned(tools, {chat: "@4", typing: true})).toBe(0)
    for (const ref of ["@5", "@7", "@8", "@2", "#13", "#12"]) expect(await returned(tools, {chat: ref, typing: true})).toBe(1)
    actions.loseControl()
    expect(await returned(tools, {chat: "@6", typing: true})).toBe(1)
    const host = new FakeChat()
    host.chats.set("@4", {chatInfo: direct(contact(4, "Alice")), items: [item(20, "hi")]})
    expect(await returned(approvedTools(host, "host").tools, {chat: "@4", typing: true})).toBe(1)
    await jest.advanceTimersByTimeAsync(3_000)
    expect([chat.sent, host.sent]).toEqual([[], []])
  })

  it("deletes the status when a receive of its chat waits", async () => {
    const {chat, tools, status} = await statusChat()
    const {controller, call, progress} = signal()
    const waiting = tools.call("receive", {chat: "@4", wait: 3600}, call)
    await jest.advanceTimersByTimeAsync(0)
    expect(progress).toEqual([expect.stringMatching(/^No new messages in @4; waiting until /)])
    expect(chat.deletedItems).toEqual([broadcastDeletion(T.ChatType.Direct, 4, status)])
    controller.abort()
    expect(await waiting).toBeUndefined()
  })

  it("leaves the status when a receive of another chat waits", async () => {
    const {chat, tools, status} = await statusChat()
    chat.chats.set("@5", {chatInfo: direct(contact(5, "Bob")), items: []})
    const {controller, call} = signal()
    const waiting = tools.call("receive", {chat: "@5", wait: 3600}, call)
    await jest.advanceTimersByTimeAsync(3_500)
    expect(chat.deletedItems).toEqual([])
    expect(chat.updates).toEqual([{chatId: 4, itemId: status, text: "◓ thinking · 0:03", live: true}])
    controller.abort()
    expect(await waiting).toBeUndefined()
  })
})

describe("send", () => {
  const flushed = (): Promise<void> => new Promise(resolve => setImmediate(resolve))

  it("returns the responses received after the sent item", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const chatInfo = direct(contact(4, "SimpleX Directory", {peerType: "bot"}))
    chat.chats.set("@4", {chatInfo, items: []})
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const {tools, watch} = agentTools(chat, actions)
    fake.hooks.perform = (action, signal) => tools.perform(action, signal)
    const {call} = signal()
    const sending = tools.call("send", {chat: "@4", text: "trail running shoes", wait: 60, idle: 5, limit: 2}, call)
    await jest.advanceTimersByTimeAsync(0)
    const action = actions.get(1)!
    expect(action).toMatchObject({state: "pending", open: 1})
    actions.approve(action, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    expect(action.state).toBe("done")
    const sent = chat.sent[0]
    expect(sent.messages[0].msgContent).toEqual({type: "text", text: "trail running shoes"})
    const sentItem = chat.nextItemId - 1
    watch.deliver([chat.addItem("@4", item(sentItem - 1, "earlier", {type: "directRcv"}))])
    watch.deliver([chat.addItem("@4", item(sentItem + 1, "result 1", {type: "directRcv"})), chat.addItem("@4", item(sentItem + 2, "result 2", {type: "directRcv"}))])
    watch.deliver([chat.addItem("@4", item(sentItem + 3, "result 3", {type: "directRcv"}))])
    await jest.advanceTimersByTimeAsync(6_000)
    const result = (await sending)!.structuredContent as {action: number; status: string; item: number; responses: {item: number}[]; more: number}
    expect(result).toMatchObject({action: 1, status: "done", item: sentItem, more: 1})
    expect(result.responses.map(({item}) => item)).toEqual([sentItem + 1, sentItem + 2])
    expect(action.open).toBe(0)
    expect(actions.get(action.n)).toBeUndefined()
    expect(chat.commands.at(-1)).toBe(`/_read chat items @4 ${sentItem + 1},${sentItem + 2}`)
  })

  it("waits and limits as the attaching call asks", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory", {peerType: "bot"})), items: []})
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const {tools, watch} = agentTools(chat, actions)
    fake.hooks.perform = (action, signal) => tools.perform(action, signal)
    const first = signal()
    const creating = tools.call("send", {chat: "@4", text: "trail running shoes", wait: 600}, first.call)
    await jest.advanceTimersByTimeAsync(0)
    first.controller.abort()
    expect(await creating).toBeUndefined()
    const attaching = tools.call("send", {chat: "@4", text: "trail running shoes", wait: 1, limit: 1}, signal().call)
    let finished = false
    attaching.then(() => (finished = true))
    await jest.advanceTimersByTimeAsync(0)
    const action = actions.get(1)!
    expect(action).toMatchObject({state: "pending", open: 1})
    actions.approve(action, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    const sentItem = chat.nextItemId - 1
    watch.deliver([chat.addItem("@4", item(sentItem + 1, "result 1", {type: "directRcv"})), chat.addItem("@4", item(sentItem + 2, "result 2", {type: "directRcv"}))])
    await jest.advanceTimersByTimeAsync(999)
    expect(finished).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    const result = (await attaching)!.structuredContent as {action: number; status: string; item: number; responses: {item: number}[]; more: number}
    expect(result).toMatchObject({action: 1, status: "done", item: sentItem, more: 1})
    expect(result.responses.map(({item}) => item)).toEqual([sentItem + 1])
    expect(actions.all()).toEqual([])
  })

  it("returns a live response only when final, and leaves it unread while live", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const chatInfo = direct(contact(4, "Switchback"))
    chat.chats.set("@4", {chatInfo, items: []})
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const {tools, watch} = agentTools(chat, actions)
    fake.hooks.perform = (action, signal) => tools.perform(action, signal)
    const send = async (text: string, final: boolean): Promise<{sentItem: number; result: object}> => {
      const sending = tools.call("send", {chat: "@4", text, wait: 10, idle: 2}, signal().call)
      await jest.advanceTimersByTimeAsync(0)
      actions.approve(actions.pending()[0], "Alice")
      await jest.advanceTimersByTimeAsync(0)
      const sentItem = chat.nextItemId - 1
      watch.deliver([chat.addItem("@4", item(sentItem + 1, "Ridge", {type: "directRcv"}, {live: true})), chat.addItem("@4", item(sentItem + 2, "In stock.", {type: "directRcv"}))])
      await jest.advanceTimersByTimeAsync(5_000)
      if (final) watch.update(event(chatInfo, item(sentItem + 1, "Ridgecrest, $142", {type: "directRcv"}, {live: false})))
      await jest.advanceTimersByTimeAsync(5_000)
      return {sentItem, result: (await sending)!.structuredContent!}
    }
    expect((await send("price?", true)).result).toMatchObject({responses: [{text: "Ridgecrest, $142"}, {text: "In stock."}], more: 0})
    const {sentItem, result} = await send("size 44?", false)
    expect(result).toMatchObject({responses: [{text: "In stock."}], more: 1})
    expect(chat.commands.at(-1)).toBe(`/_read chat items @4 ${sentItem + 2}`)
  })

  it("returns idle seconds after a live response is deleted", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const chatInfo = direct(contact(4, "Switchback"))
    chat.chats.set("@4", {chatInfo, items: []})
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const {tools, watch} = agentTools(chat, actions)
    fake.hooks.perform = (action, signal) => tools.perform(action, signal)
    const sending = tools.call("send", {chat: "@4", text: "price?", wait: 60, idle: 2}, signal().call)
    let finished = false
    sending.then(() => (finished = true))
    await jest.advanceTimersByTimeAsync(0)
    actions.approve(actions.get(1)!, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    const live = item(chat.nextItemId, "Ridge", {type: "directRcv"}, {live: true})
    watch.deliver([chat.addItem("@4", live)])
    await jest.advanceTimersByTimeAsync(10_000)
    expect(finished).toBe(false)
    watch.remove([event(chatInfo, live)])
    await jest.advanceTimersByTimeAsync(1_999)
    expect(finished).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    expect((await sending)!.structuredContent).toMatchObject({status: "done", responses: [], more: 0})
  })

  it("keeps actions of two sessions apart", async () => {
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Bob")), items: []})
    const actions = actionsWith("requests", fakeHooks())
    const first = agentTools(chat, actions, undefined, 1).tools
    const second = agentTools(chat, actions, undefined, 2).tools
    const calls = [signal(), signal()]
    const sending = [first, second].map((tools, i) => tools.call("send", {chat: "@4", text: "hello"}, calls[i].call))
    await settled()
    expect(actions.all().map(({session}) => session)).toEqual([1, 2])
    for (const tools of [first, second]) {
      const {actions: listed} = (await tools.call("list", {}, signal().call))!.structuredContent as {actions: {action: number}[]}
      expect(listed.length).toBe(1)
    }
    const {actions: listedFirst} = (await first.call("list", {}, signal().call))!.structuredContent as {actions: {action: number}[]}
    expect(listedFirst.map(({action}) => action)).toEqual([actions.all()[0].n])
    calls.forEach(({controller}) => controller.abort())
    await Promise.all(sending)
  })

  it("creates no action for a call aborted before it", async () => {
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Bob")), items: []})
    const actions = actionsWith("requests", fakeHooks())
    const {tools} = agentTools(chat, actions)
    const {controller, call} = signal()
    const sending = tools.call("send", {chat: "@4", text: "hello"}, call)
    controller.abort()
    expect(await sending).toBeUndefined()
    expect(actions.all()).toEqual([])
    expect(actions.next).toBe(1)
  })

  it("checks the chat, reply and mentions before creating an action", async () => {
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Bob")), items: [item(20, "hello", {type: "directRcv"})]})
    const observer = member(1, "agent", T.GroupMemberRole.Observer)
    chat.chats.set("#12", {chatInfo: groupChat(group(12, "News", observer)), items: []})
    chat.chats.set("#15", {chatInfo: groupChat(group(15, "Trail", member(1, "agent"))), items: []})
    chat.members.set(15, [member(44, "Switchback")])
    chat.chats.set("@2", {chatInfo: direct({...contact(2, "Ask SimpleX Team"), activeConn: undefined}), items: []})
    chat.chats.set("#16", {chatInfo: groupChat(group(16, "Old", member(1, "agent", T.GroupMemberRole.Member, T.GroupMemberStatus.Removed))), items: []})
    const actions = actionsWith("requests", fakeHooks())
    const {tools} = agentTools(chat, actions)
    const text = async (args: object): Promise<string> => textOf(await tools.call("send", args, signal().call))
    expect(await text({chat: "#10", text: "x"})).toBe("No chat #10. list shows the agent's chats.")
    expect(await text({chat: "@2", text: "x"})).toBe("The agent is not connected to @2 and can't post; list shows connected: false.")
    expect(await text({chat: "#16", text: "x"})).toBe("The agent is not connected to #16 and can't post; list shows connected: false.")
    expect(await text({chat: "#12", text: "x"})).toBe("The agent has role observer in #12 and can't post.")
    expect(await text({chat: "@4", text: "x", reply_to: 999})).toBe("reply_to: @4 has no item 999.")
    expect(await text({chat: "@4", text: "x", mention: [44]})).toBe("mention: @4 is a contact chat; mentions apply to groups and channels.")
    expect(await text({chat: "#15", text: "x", mention: [45]})).toBe("mention: #15 has no member 45.")
    expect(actions.all()).toHaveLength(0)
  })

  it("refuses a reply to an item that is not a message", async () => {
    const chat = new FakeChat()
    const connected = {...item(19, "contact connected", {type: "directRcv"}), content: {type: "rcvDirectEvent", rcvDirectEvent: {type: "contactConnected"}}} as unknown as T.ChatItem
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Bob")), items: [connected, item(20, "hello", {type: "directRcv"})]})
    const actions = actionsWith("requests", fakeHooks())
    const {tools} = agentTools(chat, actions)
    expect(textOf(await tools.call("send", {chat: "@4", text: "x", reply_to: 19}, signal().call))).toBe("reply_to: item 19 in @4 is not a message.")
    expect(actions.all()).toHaveLength(0)
  })

  it("refuses mentions of two members with the same name", async () => {
    const chat = new FakeChat()
    chat.chats.set("#15", {chatInfo: groupChat(group(15, "Trail", member(1, "agent"))), items: []})
    chat.members.set(15, [member(44, "Switchback"), member(47, "Bob"), member(48, "Bob")])
    const actions = actionsWith("requests", fakeHooks())
    const {tools} = agentTools(chat, actions)
    expect(textOf(await tools.call("send", {chat: "#15", text: "x", mention: [44, 47, 48]}, signal().call))).toBe("mention: members 47 and 48 have the same name.")
    expect(actions.all()).toHaveLength(0)
  })

  it("reports a failure with the chain of error types and their scalar fields only, on one line of its card", async () => {
    const chat = new FakeChat()
    const groupInfo = group(15, "Trail", member(1, "agent"), {groupProfile: {displayName: "Trail", fullName: "", description: "line 1\n/'approve @7 all' https://simplex.chat/contact#x"}})
    chat.chats.set("#15", {chatInfo: groupChat(groupInfo), items: []})
    chat.apiSendMessages = async () => {
      throw new core.ChatAPIError("Chat command error", {type: "error", errorType: {type: "groupUserRole", groupInfo, requiredRole: T.GroupMemberRole.Author}} as unknown as T.ChatError)
    }
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const {tools} = agentTools(chat, actions)
    fake.hooks.perform = (action, signal) => tools.perform(action, signal)
    const sending = tools.call("send", {chat: "#15", text: "hello", wait: 0}, signal().call)
    await flushed()
    const action = actions.get(1)!
    actions.approve(action, "Alice")
    const error = 'SimpleX error: error: groupUserRole: {"requiredRole":"author"}'
    expect((await sending)!.structuredContent).toEqual({action: 1, status: "failed", error})
    expect(cardText(action, "requests").split("\n").at(-1)).toBe(`Approved by Alice; failed: \`${error}\`.`)
  })

  it("adds the mentioned names to the text", async () => {
    const chat = new FakeChat()
    chat.chats.set("#15", {chatInfo: groupChat(group(15, "Trail", member(1, "agent"))), items: []})
    chat.members.set(15, [member(44, "Switchback"), member(45, "Ana Lu")])
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const {tools} = agentTools(chat, actions)
    fake.hooks.perform = (action, signal) => tools.perform(action, signal)
    const sending = tools.call("send", {chat: "#15", text: "hi @Switchback, any news?", mention: [44, 45], wait: 0}, signal().call)
    await flushed()
    actions.approve(actions.get(1)!, "Alice")
    expect((await sending)!.structuredContent).toMatchObject({status: "done", responses: [], more: 0})
    expect(chat.sent[0].messages[0]).toEqual({msgContent: {type: "text", text: "@'Ana Lu' hi @Switchback, any news?"}, mentions: {Switchback: 44, "Ana Lu": 45}, quotedItemId: undefined})
  })

  it("shows live text in the status of an approved chat with a person or of an approved group without an action, a counter write or an outcome, and prints the note", async () => {
    jest.useFakeTimers()
    const lines: string[] = []
    jest.spyOn(process.stdout, "write").mockImplementation(text => (lines.push(String(text).trimEnd()), true))
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Alice")), items: []})
    chat.chats.set("#15", {chatInfo: groupChat(group(15, "Trail", member(1, "agent"))), items: []})
    const {tools, actions, fake, approved} = approvedTools(chat)
    approved.add("#15")
    expect((await tools.call("send", {chat: "@4", text: "Reading\nthe file…", live: true}, signal().call))!.structuredContent).toEqual({status: "live", chat: "@4"})
    expect((await tools.call("send", {chat: "#15", text: "Checking the map…", live: true}, signal().call))!.structuredContent).toEqual({status: "live", chat: "#15"})
    await jest.advanceTimersByTimeAsync(0)
    expect(chat.sent).toEqual([
      livePost(T.ChatType.Direct, 4, "◐ thinking · 0:00\nReading\nthe file…"),
      livePost(T.ChatType.Group, 15, "◐ thinking · 0:00\nChecking the map…"),
    ])
    expect([actions.all(), actions.next, fake.calls]).toEqual([[], 1, []])
    expect(lines).toEqual(["Note to contact Alice (@4): Reading the file…", "Note to group Trail (#15): Checking the map…"])
  })

  it("returns nothing for an aborted live call, and shows and prints nothing", async () => {
    jest.useFakeTimers()
    const lines: string[] = []
    jest.spyOn(process.stdout, "write").mockImplementation(text => (lines.push(String(text).trimEnd()), true))
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Alice")), items: []})
    const {tools} = approvedTools(chat)
    const {controller, call} = signal()
    controller.abort()
    expect(await tools.call("send", {chat: "@4", text: "Reading the file…", live: true}, call)).toBeUndefined()
    await jest.advanceTimersByTimeAsync(3_000)
    expect([chat.sent, lines]).toEqual([[], []])
  })

  it("refuses live with file, reply_to or mention, over 2000 characters, outside approved chats with connected people, groups and business chats, after the control chat is lost, and in mode host", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Alice")), items: [item(20, "hello")]})
    chat.chats.set("@5", {chatInfo: direct(contact(5, "Bob")), items: []})
    chat.chats.set("@7", {chatInfo: direct(contact(7, "Switchback", {peerType: "bot"})), items: []})
    chat.chats.set("@8", {chatInfo: direct(contact(8, "Scout", {peerType: "agent"})), items: []})
    chat.chats.set("@2", {chatInfo: direct({...contact(2, "Carol"), activeConn: undefined}), items: []})
    chat.chats.set("#13", {chatInfo: groupChat(group(13, "Ridgeline News", member(1, "agent"), {useRelays: true})), items: []})
    chat.chats.set("#12", {chatInfo: groupChat(group(12, "News", member(1, "agent", T.GroupMemberRole.Observer))), items: []})
    chat.chats.set("#15", {chatInfo: groupChat(group(15, "Trail", member(1, "agent"))), items: []})
    const {tools, actions, approved} = approvedTools(chat)
    for (const ref of ["@7", "@8", "@2", "#13", "#12", "#15"]) approved.add(ref)
    const host = approvedTools(chat, "host")
    const refusal = async (target: AgentTools, args: object): Promise<string> => textOf(await target.call("send", {text: "x", live: true, ...args}, signal().call))
    const textOnly = "live: applies to text only; send file, reply_to and mention without live."
    expect(await refusal(tools, {chat: "@4", file: "/home/alice/notes.txt"})).toBe(textOnly)
    expect(await refusal(tools, {chat: "@4", reply_to: 20})).toBe(textOnly)
    expect(await refusal(tools, {chat: "#15", mention: [44]})).toBe(textOnly)
    expect(await refusal(tools, {chat: "@4", text: "😀".repeat(2001)})).toBe("text: at most 2000 characters with live.")
    expect(await refusal(tools, {chat: "@2"})).toBe("The agent is not connected to @2 and can't post; list shows connected: false.")
    expect(await refusal(tools, {chat: "#12"})).toBe("The agent has role observer in #12 and can't post.")
    for (const ref of ["@7", "@8", "#13"]) expect(await refusal(tools, {chat: ref})).toBe(`live: ${ref} is a channel or a chat with a bot or an agent; status messages are shown in chats with people, groups and business chats.`)
    expect(await refusal(tools, {chat: "@5"})).toBe("live: your owner's approval of all messages to @5 is required; list shows approved: true for such chats.")
    expect(await refusal(host.tools, {chat: "@4"})).toBe("live: status messages are shown in mode requests only; this server runs in mode host.")
    await jest.advanceTimersByTimeAsync(0)
    expect([chat.sent, actions.all(), host.actions.all()]).toEqual([[], [], []])
    expect((await tools.call("send", {chat: "@4", text: "😀".repeat(2000), live: true}, signal().call))!.structuredContent).toEqual({status: "live", chat: "@4"})
    actions.loseControl()
    expect(await refusal(tools, {chat: "#15"})).toBe("live: The agent lost its control chat. The person who runs it has to restart it with --reset-owners.")
  })

  it("attaches a live call to no action, also when a pending send has its text", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Alice")), items: []})
    const {tools, actions, approved} = approvedTools(chat)
    approved.delete("@4")
    const {controller, call} = signal()
    const pending = tools.call("send", {chat: "@4", text: "Reading the file…"}, call)
    await jest.advanceTimersByTimeAsync(0)
    controller.abort()
    expect(await pending).toBeUndefined()
    approved.add("@4")
    expect((await tools.call("send", {chat: "@4", text: "Reading the file…", live: true}, signal().call))!.structuredContent).toEqual({status: "live", chat: "@4"})
    expect(actions.all()).toHaveLength(1)
    expect(actions.get(1)).toMatchObject({state: "pending", open: 0})
  })

  it("sends the answer as a new message, then deletes the status, and returns the responses after the answer", async () => {
    const {chat, tools, watch, status} = await statusChat()
    const send = chat.apiSendMessages.bind(chat)
    const deletionsAtAnswer: number[] = []
    chat.apiSendMessages = async (target, messages, live) => {
      if (!live) deletionsAtAnswer.push(chat.deletedItems.length)
      return send(target, messages, live)
    }
    const sending = tools.call("send", {chat: "@4", text: "The README describes the server.", wait: 60, idle: 5}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    const answer = chat.nextItemId - 1
    watch.deliver([chat.addItem("@4", item(answer + 1, "and the tests?"))])
    await jest.advanceTimersByTimeAsync(5_000)
    expect((await sending)!.structuredContent).toMatchObject({action: 1, status: "done", item: answer, responses: [{item: answer + 1, text: "and the tests?"}], more: 0})
    expect(chat.sent.map(({messages: [{msgContent}], live}) => [msgContent.text, live])).toEqual([["◐ thinking · 0:00", true], ["The README describes the server.", undefined]])
    expect(deletionsAtAnswer).toEqual([0])
    expect(chat.deletedItems).toEqual([broadcastDeletion(T.ChatType.Direct, 4, status)])
    expect(chat.updates).toEqual([])
  })

  it("leaves the status at a send to another chat", async () => {
    const {chat, tools, approved, status} = await statusChat()
    chat.chats.set("@5", {chatInfo: direct(contact(5, "Bob")), items: []})
    approved.add("@5")
    expect((await tools.call("send", {chat: "@5", text: "hi", wait: 0}, signal().call))!.structuredContent).toMatchObject({status: "done"})
    await jest.advanceTimersByTimeAsync(3_500)
    expect(chat.deletedItems).toEqual([])
    expect(chat.updates.map(({itemId}) => itemId)).toEqual([status])
  })

  it("keeps the status when the answer fails", async () => {
    const {chat, tools, status} = await statusChat()
    const send = chat.apiSendMessages.bind(chat)
    chat.apiSendMessages = async (target, messages, live) => {
      if (!live) throw new Error("send failed")
      return send(target, messages, live)
    }
    expect((await tools.call("send", {chat: "@4", text: "12:00", wait: 0}, signal().call))!.structuredContent).toEqual({action: 1, status: "failed", error: "send failed"})
    await jest.advanceTimersByTimeAsync(3_500)
    expect(chat.deletedItems).toEqual([])
    expect(chat.updates).toEqual([{chatId: 4, itemId: status, text: "◓ thinking · 0:03", live: true}])
  })

  it("deletes the status at the next tick after the approval is revoked, and the answer then gets a card", async () => {
    const {chat, tools, actions, fake, approved, status} = await statusChat()
    approved.delete("@4")
    await jest.advanceTimersByTimeAsync(3_500)
    expect(chat.deletedItems).toEqual([broadcastDeletion(T.ChatType.Direct, 4, status)])
    expect(texts(chat)).toEqual(["◐ thinking · 0:00"])
    const {controller, call} = signal()
    const sending = tools.call("send", {chat: "@4", text: "12:00"}, call)
    await jest.advanceTimersByTimeAsync(0)
    expect(actions.get(1)).toMatchObject({state: "pending"})
    expect(fake.calls).toContain("card 1")
    controller.abort()
    expect(await sending).toBeUndefined()
  })

  it("deletes the status at the next tick after the control chat is lost", async () => {
    const {chat, actions, status} = await statusChat()
    actions.loseControl()
    await jest.advanceTimersByTimeAsync(3_500)
    expect(chat.deletedItems).toEqual([broadcastDeletion(T.ChatType.Direct, 4, status)])
    expect(texts(chat)).toEqual(["◐ thinking · 0:00"])
  })
})

describe("send with a file", () => {
  const fixedPreview = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAJCAAAAAAeQfPuAAAAD0lEQVR42mNoQAMMg0QAAIXUSAG1CN3UAAAAAElFTkSuQmCC"
  const preview = "data:image/jpg;base64,/9j/preview"
  const resized = Buffer.concat([Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a]), Buffer.from("resized")])
  let files: string

  beforeEach(() => {
    files = realpathSync(mkdtempSync(join(os.tmpdir(), "simplex-mcp-files-")))
  })

  afterEach(() => rmSync(files, {recursive: true, force: true}))

  function commandOutputs(output: (command: string) => Buffer | Error): jest.SpyInstance {
    const run = (command: string, _args: string[], _options: object, callback: (error: Error | null, stdout: Buffer) => void): void => {
      const result = output(command)
      if (result instanceof Error) callback(result, Buffer.alloc(0))
      else callback(null, result)
    }
    return jest.spyOn(childProcess, "execFile").mockImplementation(run as unknown as typeof childProcess.execFile)
  }

  function fileIn(name: string, content = "content"): string {
    const path = join(files, name)
    writeFileSync(path, content)
    return path
  }

  function sendTools(): {chat: FakeChat; tools: AgentTools; actions: Actions} {
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory", {peerType: "bot"})), items: []})
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const {tools} = agentTools(chat, actions)
    fake.hooks.perform = (action, signal) => tools.perform(action, signal)
    return {chat, tools, actions}
  }

  async function created(actions: Actions): Promise<Action> {
    while (actions.get(1) === undefined) await new Promise(resolve => setImmediate(resolve))
    return actions.get(1)!
  }

  async function sent(args: object): Promise<{chat: FakeChat; tools: AgentTools; action: SendAction; result: object}> {
    const {chat, tools, actions} = sendTools()
    const sending = tools.call("send", {chat: "@4", wait: 0, ...args}, signal().call)
    const action = (await created(actions)) as SendAction
    actions.approve(action, "Alice")
    return {chat, tools, action, result: (await sending)!.structuredContent!}
  }

  const resizeCalls = (resize: jest.SpyInstance): unknown[][] => resize.mock.calls.map(([image, size]) => [Buffer.from(image).toString(), size])

  it("sends a JPEG or PNG to a contact as an image with the core's preview and a resized copy, deleted when its upload completes", async () => {
    const resize = jest.spyOn(core, "resizeImage").mockImplementation(async (_image, size) => (size === core.ImageSize.MessagePreview ? preview : resized))
    const photo = fileIn("Trail.PNG", "photo")
    const {chat, tools, action, result} = await sent({text: "trail map", file: photo})
    expect(result).toMatchObject({status: "done"})
    expect(action.file).toEqual({path: photo, name: "Trail.PNG", size: 5, inode: expect.any(Number), changedAt: expect.any(Number), type: "image", resize: true, preview})
    const [message] = chat.sent[0].messages
    const copy = message.fileSource!.filePath
    expect(dirname(dirname(copy))).toBe(join(data, "files", "sent"))
    expect(basename(copy)).toBe("Trail.png")
    expect(readFileSync(copy)).toEqual(resized)
    expect(message).toEqual({fileSource: {filePath: copy}, msgContent: {type: "image", text: "trail map", image: preview}, mentions: {}, quotedItemId: undefined})
    expect(resizeCalls(resize)).toEqual([
      ["photo", "messagePreview"],
      ["photo", "image"],
    ])
    tools.onSentFile({filePath: copy} as T.FileTransferMeta)
    expect(existsSync(dirname(copy))).toBe(false)
  })

  it("sends a GIF or WebP with the fixed preview, a video without ffmpeg with the fixed preview and duration 0, and other files unchanged", async () => {
    const resize = jest.spyOn(core, "resizeImage")
    const commands = commandOutputs(command => new Error(`spawn ${command} ENOENT`))
    const cases: [object, object][] = [
      [{file: fileIn("wave.gif")}, {type: "image", text: "", image: fixedPreview}],
      [{text: "pass", file: fileIn("pass.WEBP")}, {type: "image", text: "pass", image: fixedPreview}],
      [{text: "ride", file: fileIn("ride.mp4")}, {type: "video", text: "ride", image: fixedPreview, duration: 0}],
      [{text: "report", file: fileIn("report.pdf")}, {type: "file", text: "report"}],
      [{text: "original", file: fileIn("photo.jpg"), as: "file"}, {type: "file", text: "original"}],
    ]
    for (const [args, msgContent] of cases) {
      const {chat} = await sent(args)
      expect(chat.sent[0].messages[0]).toEqual({fileSource: {filePath: (args as {file: string}).file}, msgContent, mentions: {}, quotedItemId: undefined})
    }
    expect(resize).not.toHaveBeenCalled()
    expect(commands.mock.calls.map(([command]) => command)).toEqual(["ffmpeg", "ffprobe"])
  })

  it("sends an image unchanged with the fixed preview when the core can't resize it, and logs the errors", async () => {
    jest.spyOn(core, "resizeImage").mockRejectedValue(new Error("image cannot be decoded or does not fit maxSize"))
    const errors = jest.spyOn(process.stderr, "write").mockImplementation(() => true)
    const photo = fileIn("photo.jpeg")
    const {chat} = await sent({text: "trail", file: photo})
    expect(chat.sent[0].messages[0]).toMatchObject({fileSource: {filePath: photo}, msgContent: {type: "image", text: "trail", image: fixedPreview}})
    expect(errors.mock.calls.map(([line]) => line)).toEqual([
      "preview: image cannot be decoded or does not fit maxSize\n",
      `resizing ${photo}: image cannot be decoded or does not fit maxSize\n`,
    ])
  })

  it("takes a video's preview from the first frame of ffmpeg and its duration from ffprobe", async () => {
    const commands = commandOutputs(command => Buffer.from(command === "ffmpeg" ? "frame" : "12.6\n"))
    const resize = jest.spyOn(core, "resizeImage").mockResolvedValue(preview)
    const video = fileIn("ride.MOV")
    const {chat} = await sent({text: "ride", file: video})
    expect(chat.sent[0].messages[0].msgContent).toEqual({type: "video", text: "ride", image: preview, duration: 13})
    expect(resizeCalls(resize)).toEqual([["frame", "messagePreview"]])
    expect(commands.mock.calls.map(([command, args, options]) => [command, args, options])).toEqual([
      ["ffmpeg", ["-nostdin", "-v", "error", "-protocol_whitelist", "file", "-f", "mov", "-i", video, "-frames:v", "1", "-f", "image2pipe", "-vcodec", "mjpeg", "-"], {encoding: "buffer", timeout: 30_000, maxBuffer: 64 * 1024 * 1024}],
      ["ffprobe", ["-v", "error", "-protocol_whitelist", "file", "-f", "mov", "-show_entries", "format=duration", "-of", "csv=p=0", video], {encoding: "buffer", timeout: 30_000, maxBuffer: 64 * 1024 * 1024}],
    ])
  })

  it("refuses paths that are not absolute readable files, private keys and the server's own files, after resolving symbolic links", async () => {
    const home = join(files, "home")
    mkdirSync(join(home, ".ssh"), {recursive: true})
    jest.spyOn(os, "homedir").mockReturnValue(home)
    mkdirSync(join(data, "files", "contact-4"), {recursive: true})
    const token = join(realpathSync(data), "token")
    writeFileSync(token, "secret")
    const received = join(realpathSync(data), "files", "contact-4", "report.pdf")
    writeFileSync(received, "report")
    const config = join(home, ".ssh", "config")
    writeFileSync(config, "")
    const link = join(files, "notes.txt")
    symlinkSync(token, link)
    const locked = fileIn("locked.txt")
    chmodSync(locked, 0)
    const {tools, actions} = sendTools()
    const text = async (args: object): Promise<string> => textOf(await tools.call("send", {chat: "@4", ...args}, signal().call))
    const refused = (path: string): string => `file: ${path} is refused: private keys and the server's own files are not sent.`
    expect(await text({text: "x", file: "notes.txt"})).toBe("file: an absolute path is expected.")
    expect(await text({file: join(files, "missing.txt")})).toBe(`file: no file at ${join(files, "missing.txt")}.`)
    expect(await text({file: files})).toBe(`file: ${files} is not a file.`)
    expect(await text({file: locked})).toBe(`file: ${locked} can't be read.`)
    expect(await text({file: link})).toBe(refused(token))
    expect(await text({file: config})).toBe(refused(config))
    for (const name of ["id_ed25519", "server.PEM", "tls.key", "cert.p12", "cert.pfx"]) expect(await text({file: fileIn(name)})).toBe(refused(join(files, name)))
    expect(await text({})).toBe("text: required without file.")
    expect(actions.all()).toEqual([])
    const {controller, call} = signal()
    const sending = tools.call("send", {chat: "@4", file: received}, call)
    expect(((await created(actions)) as SendAction).file).toMatchObject({path: received, type: "file"})
    controller.abort()
    expect(await sending).toBeUndefined()
  })

  it("refuses an empty file, as without file, and a text too long for a message with the file's preview", async () => {
    jest.spyOn(core, "resizeImage").mockResolvedValue(`data:image/jpg;base64,${"A".repeat(13_978)}`)
    const {tools, actions} = sendTools()
    const text = async (args: object): Promise<string> => textOf(await tools.call("send", {chat: "@4", ...args}, signal().call))
    expect(await text({file: fileIn("empty.txt", "")})).toBe(`file: ${join(files, "empty.txt")} is empty.`)
    expect(await text({text: "x", as: "file"})).toBe("as: applies only with file.")
    expect(await text({text: "x".repeat(1100), file: fileIn("photo.jpg")})).toBe("text: too long for a message with this image; shorten it.")
    expect(actions.all()).toEqual([])
  })

  it("counts the quoted message as the core sends it: an image's preview, unless an image replies to it", async () => {
    jest.spyOn(core, "resizeImage").mockResolvedValue(`data:image/jpg;base64,${"A".repeat(13_978)}`)
    const {chat, tools, actions} = sendTools()
    chat.chats.get("@4")!.items.push(fileItem(20, {type: "image", text: "", image: `data:image/jpg;base64,${"B".repeat(13_978)}`}, {type: "rcvComplete"}))
    const text = async (args: object): Promise<string> => textOf(await tools.call("send", {chat: "@4", reply_to: 20, ...args}, signal().call))
    expect(await text({text: "x".repeat(1100), file: fileIn("notes.txt")})).toBe("text: too long for a message with this file; shorten it.")
    expect(await text({text: "x".repeat(1700)})).toBe("text: too long for the message with its quote; shorten it.")
    expect(actions.all()).toEqual([])
    const {controller, call} = signal()
    const sending = tools.call("send", {chat: "@4", reply_to: 20, text: "x".repeat(800), file: fileIn("photo.jpg")}, call)
    expect(await created(actions)).toMatchObject({tool: "send", state: "pending"})
    controller.abort()
    expect(await sending).toBeUndefined()
  })

  it("refuses files in a key folder reached through a symbolic link, and in other agents' data folders", async () => {
    const home = join(files, "home")
    const keys = join(files, "keys")
    mkdirSync(keys, {recursive: true})
    mkdirSync(join(home, ".simplex-mcp", "other"), {recursive: true})
    symlinkSync(keys, join(home, ".ssh"))
    jest.spyOn(os, "homedir").mockReturnValue(home)
    const config = join(keys, "config")
    writeFileSync(config, "Host *")
    const token = join(home, ".simplex-mcp", "other", "token")
    writeFileSync(token, "secret")
    const {tools, actions} = sendTools()
    const text = async (file: string): Promise<string> => textOf(await tools.call("send", {chat: "@4", file}, signal().call))
    const refused = (path: string): string => `file: ${path} is refused: private keys and the server's own files are not sent.`
    expect(await text(join(home, ".ssh", "config"))).toBe(refused(config))
    expect(await text(token)).toBe(refused(token))
    expect(actions.all()).toEqual([])
  })

  it("fails when the file changed after the request, also at the same size", async () => {
    const notes = fileIn("notes.txt", "first")
    const {tools, actions} = sendTools()
    const sending = tools.call("send", {chat: "@4", file: notes, wait: 0}, signal().call)
    const action = await created(actions)
    await new Promise(resolve => setTimeout(resolve, 50))
    writeFileSync(notes, "other")
    actions.approve(action, "Alice")
    expect((await sending)!.structuredContent).toEqual({action: 1, status: "failed", error: `file: ${notes} changed after the request.`})
  })

  it("fails when the file changed after the request, and deletes the copy when the send fails", async () => {
    jest.spyOn(core, "resizeImage").mockImplementation(async (_image, size) => (size === core.ImageSize.MessagePreview ? preview : resized))
    const photo = fileIn("photo.png")
    const {tools, actions} = sendTools()
    const sending = tools.call("send", {chat: "@4", file: photo, wait: 0}, signal().call)
    const action = await created(actions)
    writeFileSync(photo, "a longer photo")
    actions.approve(action, "Alice")
    expect((await sending)!.structuredContent).toEqual({action: 1, status: "failed", error: `file: ${photo} changed after the request.`})
    const failing = sendTools()
    failing.chat.apiSendMessages = async () => {
      throw new Error("send failed")
    }
    const failed = failing.tools.call("send", {chat: "@4", file: photo, wait: 0}, signal().call)
    failing.actions.approve(await created(failing.actions), "Alice")
    expect((await failed)!.structuredContent).toEqual({action: 1, status: "failed", error: "send failed"})
    expect(readdirSync(join(data, "files", "sent"))).toEqual([])
  })
})

describe("connect", () => {
  const link = "https://simplex.chat/a#ridgeline"

  function connectTools(chat: FakeChat): {tools: AgentTools; actions: Actions; watch: ChatItemsWatch} {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const {tools, watch} = agentTools(chat, actions)
    fake.hooks.perform = (action, signal) => tools.perform(action, signal)
    return {tools, actions, watch}
  }

  function plan(connectionPlan: object): object {
    return {type: "connectionPlan", user, connLink: {connFullLink: link}, connectionPlan}
  }

  it("joins a known prepared contact whose connection is unset or prepared", () => {
    const known = (activeConn: object | undefined): object => {
      const prepared = {...contact(7, "Ridgeline"), activeConn, preparedContact: {}}
      return classifyPlan([{type: "contactAddress", contactAddressPlan: {type: "known", contact: prepared}} as T.ConnectionPlan, {connFullLink: link}], {target: link})
    }
    expect(known(undefined)).toMatchObject({join: {takesMessage: true}, method: {type: "prepared", conn: {contact: 7}}})
    expect(known({connId: 207, connStatus: {type: "prepared"}})).toMatchObject({join: {takesMessage: true}, method: {type: "prepared", conn: {contact: 7}}})
    expect(known({connId: 207, connStatus: {type: "joined"}})).toMatchObject({known: {type: "direct"}})
  })

  it("reports an unregistered name as the apps word it", async () => {
    const chat = new FakeChat()
    chat.sendChatCmd = async () => {
      throw new core.ChatAPIError("Chat command error", {
        type: "errorAgent",
        agentError: {type: "SMP", serverAddress: "smp://abc@smp4.simplex.im", smpErr: {type: "NAME", nameErr: {type: "NOT_FOUND"}}},
        agentConnId: "",
        connectionEntity_: {type: "rcvDirectMsgConnection", entityConnection: {connId: 1}},
      } as unknown as T.ChatError)
    }
    const {tools} = connectTools(chat)
    expect(textOf(await tools.call("connect", {target: "@nobody.simplex"}, signal().call))).toBe("Name not found: This SimpleX name is not registered. Please check the name.")
  })

  it("connects a known prepared contact, and deletes it when the connection fails", async () => {
    const chat = new FakeChat()
    const prepared = {...contact(7, "Ridgeline"), activeConn: undefined, preparedContact: {}}
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "known", contact: prepared}}))
    const {tools, actions} = connectTools(chat)
    const connecting = tools.call("connect", {target: link, message: "trail shoes?"}, signal().call)
    await settled()
    actions.approve(actions.get(1)!, "Alice")
    expect((await connecting)!.structuredContent).toEqual({action: 1, status: "failed", error: "SimpleX error: unexpected response cmdOk"})
    expect(chat.commands).toContain(`/_connect contact @7 json {"type":"text","text":"trail shoes?"}`)
    expect(chat.deleted).toEqual([{chatType: T.ChatType.Direct, chatId: 7, mode: {type: "full", notify: false}}])
  })

  it("records the connection of a started contact, completes on an event naming that connection, and waits for the reply to message", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const linkData = {profile: {displayName: "Ridgeline", fullName: ""}, business: false}
    const started = {...contact(8, "Ridgeline"), activeConn: {connId: 208, connStatus: {type: "joined"}}} as unknown as T.Contact
    const connected = {...contact(12, "Ridgeline"), activeConn: {connId: 208, connStatus: {type: "ready"}}} as unknown as T.Contact
    chat.chats.set("@8", {chatInfo: direct(started), items: []})
    chat.chats.set("@12", {chatInfo: direct(connected), items: [item(40, "Welcome!")]})
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "ok", contactSLinkData_: linkData}}))
    chat.responses.set(`/_prepare contact 1 ${link} ${JSON.stringify(linkData)}`, {type: "newPreparedChat", chat: {chatInfo: direct(started), chatItems: []}})
    chat.responses.set(`/_connect contact @8 json {"type":"text","text":"hello"}`, {type: "startedConnectionToContact", contact: started})
    const {tools, actions, watch} = connectTools(chat)
    const connecting = tools.call("connect", {target: link, message: "hello", wait: 120, idle: 10}, signal().call)
    let finished = false
    connecting.then(() => (finished = true))
    await jest.advanceTimersByTimeAsync(0)
    const action = actions.get(1)!
    actions.approve(action, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    expect(action).toMatchObject({state: "performing", conn: {contact: 8, pcc: 208}, chat: "@8"})
    tools.onContactConnected({type: "contactConnected", user, contact: connected})
    await jest.advanceTimersByTimeAsync(40_000)
    expect(finished).toBe(false)
    watch.deliver([chat.addItem("@12", item(41, "Size 44 is in stock."))])
    await jest.advanceTimersByTimeAsync(9_999)
    expect(finished).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    expect((await connecting)!.structuredContent).toMatchObject({
      action: 1,
      status: "done",
      chat: "@12",
      connected: true,
      messages: [{item: 40, text: "Welcome!"}, {item: 41, text: "Size 44 is in stock."}],
    })
    expect(chat.commands.at(-1)).toBe("/_read chat items @12 40,41")
  })

  it("ends the wait for the connection 10 minutes before the card can no longer be edited", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "ok"}}))
    chat.responses.set(`/_connect 1 ${link}`, {type: "sentInvitation", connection: {pccConnId: 300}})
    const {tools, actions} = connectTools(chat)
    const connecting = tools.call("connect", {target: link, wait: 86400}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    const action = actions.get(1)!
    await jest.advanceTimersByTimeAsync(22 * 3_600_000)
    actions.approve(action, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    expect(action).toMatchObject({state: "performing", connectUntil: action.created + 24 * 3_600_000 - 600_000})
    await jest.advanceTimersByTimeAsync(2 * 3_600_000 - 600_000 - 1)
    expect(action.state).toBe("performing")
    await jest.advanceTimersByTimeAsync(1)
    expect((await connecting)!.structuredContent).toMatchObject({action: 1, status: "done", connected: false})
  })

  it("ends the wait for the connection at the stop, with no timer left and no log line", async () => {
    jest.useFakeTimers()
    const lines: string[] = []
    jest.spyOn(process.stdout, "write").mockImplementation(text => (lines.push(String(text).trimEnd()), true))
    const chat = new FakeChat()
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "ok"}}))
    chat.responses.set(`/_connect 1 ${link}`, {type: "sentInvitation", connection: {pccConnId: 300}})
    const {tools, actions} = connectTools(chat)
    const {controller, call} = signal()
    const connecting = tools.call("connect", {target: link, wait: 3600}, call)
    await jest.advanceTimersByTimeAsync(0)
    const action = actions.get(1)!
    actions.approve(action, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    expect(action.state).toBe("performing")
    controller.abort()
    expect(await connecting).toBeUndefined()
    await actions.stop()
    await jest.advanceTimersByTimeAsync(0)
    expect(action).toMatchObject({state: "done", connected: false, interrupted: true})
    expect(jest.getTimerCount()).toBe(0)
    expect(lines).toEqual(["Action 1: connect to an unnamed contact, as itself"])
  })

  it("keeps the chat of a connect that completed when a later event names its connection", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const connected = {...contact(12, "Ridgeline"), activeConn: {connId: 300, connStatus: {type: "ready"}}} as unknown as T.Contact
    chat.chats.set("@12", {chatInfo: direct(connected), items: []})
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "ok"}}))
    chat.responses.set(`/_connect 1 ${link}`, {type: "sentInvitation", connection: {pccConnId: 300}})
    const {tools, actions} = connectTools(chat)
    const first = signal()
    const connecting = tools.call("connect", {target: link}, first.call)
    await jest.advanceTimersByTimeAsync(0)
    const action = actions.get(1)!
    actions.approve(action, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    tools.onContactConnected({type: "contactConnected", user, contact: connected})
    await jest.advanceTimersByTimeAsync(0)
    first.controller.abort()
    expect(await connecting).toBeUndefined()
    expect(action).toMatchObject({state: "done", chat: "@12", connected: true})
    tools.onContactConnected({type: "contactConnected", user, contact: {...contact(13, "Other"), activeConn: {connId: 300, connStatus: {type: "ready"}}} as unknown as T.Contact})
    expect(action.chat).toBe("@12")
  })

  it("completes on a connection event only the connect of the session whose core sent it, when contact IDs are equal", async () => {
    jest.useFakeTimers()
    const linkData = {profile: {displayName: "Ridgeline", fullName: ""}, business: false}
    const started = {...contact(8, "Ridgeline"), activeConn: {connId: 208, connStatus: {type: "joined"}}} as unknown as T.Contact
    const connected = {...contact(8, "Ridgeline"), activeConn: {connId: 208, connStatus: {type: "ready"}}} as unknown as T.Contact
    const sessionChat = (): FakeChat => {
      const chat = new FakeChat()
      chat.chats.set("@8", {chatInfo: direct(started), items: []})
      chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "ok", contactSLinkData_: linkData}}))
      chat.responses.set(`/_prepare contact 1 ${link} ${JSON.stringify(linkData)}`, {type: "newPreparedChat", chat: {chatInfo: direct(started), chatItems: []}})
      chat.responses.set(`/_connect contact @8 json {"type":"text","text":"hello"}`, {type: "startedConnectionToContact", contact: started})
      return chat
    }
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const first = agentTools(sessionChat(), actions, undefined, 1)
    const chat2 = sessionChat()
    const second = agentTools(chat2, actions, undefined, 2)
    fake.hooks.perform = (action, signal) => (action.session === 1 ? first : second).tools.perform(action, signal)
    const calls = [signal(), signal()]
    const input = {target: link, message: "hello"}
    const connecting = [first.tools.call("connect", input, calls[0].call), second.tools.call("connect", input, calls[1].call)]
    await jest.advanceTimersByTimeAsync(0)
    const [action1, action2] = [actions.get(1) as ConnectAction, actions.get(2) as ConnectAction]
    expect([action1.session, action2.session]).toEqual([1, 2])
    actions.approve(action1, "Alice")
    actions.approve(action2, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    expect([action1, action2]).toMatchObject([{state: "performing", conn: {contact: 8}}, {state: "performing", conn: {contact: 8}}])
    chat2.chats.set("@8", {chatInfo: direct(connected), items: []})
    second.tools.onContactConnected({type: "contactConnected", user, contact: connected})
    second.watch.deliver([chat2.addItem("@8", item(41, "Session 2 only."))])
    await jest.advanceTimersByTimeAsync(0)
    expect(action2).toMatchObject({connected: true, chat: "@8"})
    expect(action2.messages.items.map(({chatItem}) => chatItem.meta.itemId)).toEqual([41])
    expect(action1).toMatchObject({state: "performing", chat: "@8"})
    expect(action1.connected).toBeUndefined()
    expect(action1.stopCollecting).toBeUndefined()
    expect(action1.messages.items).toEqual([])
    for (const {controller} of calls) controller.abort()
    expect(await Promise.all(connecting)).toEqual([undefined, undefined])
    await actions.stop()
  })

  it("returns at the end of wait, counted from the approval, when no reply to message arrives", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const linkData = {profile: {displayName: "Ridgeline", fullName: ""}, business: false}
    const started = {...contact(8, "Ridgeline"), activeConn: {connId: 208, connStatus: {type: "ready"}}} as unknown as T.Contact
    chat.chats.set("@8", {chatInfo: direct(started), items: []})
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "ok", contactSLinkData_: linkData}}))
    chat.responses.set(`/_prepare contact 1 ${link} ${JSON.stringify(linkData)}`, {type: "newPreparedChat", chat: {chatInfo: direct(started), chatItems: []}})
    chat.responses.set(`/_connect contact @8 json {"type":"text","text":"hello"}`, {type: "startedConnectionToContact", contact: started})
    const {tools, actions} = connectTools(chat)
    const connecting = tools.call("connect", {target: link, message: "hello"}, signal().call)
    let finished = false
    connecting.then(() => (finished = true))
    await jest.advanceTimersByTimeAsync(0)
    actions.approve(actions.get(1)!, "Alice")
    await jest.advanceTimersByTimeAsync(59_999)
    expect(finished).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    expect((await connecting)!.structuredContent).toMatchObject({status: "done", chat: "@8", connected: true, messages: []})
  })

  it("joins a channel through its relays, incognito, as an observer", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const linkData = {groupProfile: {displayName: "Ridgeline News", fullName: "", publicGroup: {groupType: "channel"}}}
    const observer = {...member(1, "Quiet Fox", T.GroupMemberRole.Observer, T.GroupMemberStatus.Accepted), memberContactProfileId: 99}
    const joining = group(12, "Ridgeline News", observer, {useRelays: true})
    const joined = group(12, "Ridgeline News", {...observer, memberStatus: T.GroupMemberStatus.Connected}, {useRelays: true})
    const prepare = `/_prepare group 1 ${link} direct=off ${JSON.stringify(linkData)}`
    chat.chats.set("#12", {chatInfo: groupChat(joining), items: []})
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "groupLink", groupLinkPlan: {type: "ok", groupSLinkInfo_: {direct: false, groupRelays: []}, groupSLinkData_: linkData}}))
    chat.responses.set(prepare, {type: "newPreparedChat", chat: {chatInfo: groupChat(joining), chatItems: []}})
    chat.responses.set("/_connect group #12 incognito=on", {type: "startedConnectionToGroup", groupInfo: joining, relayResults: []})
    const {tools, actions} = connectTools(chat)
    const connecting = tools.call("connect", {target: link, incognito: true}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    const action = actions.get(1)!
    expect(action.subject).toEqual({kind: "channel", name: "Ridgeline News", incognito: true})
    actions.approve(action, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    expect(chat.commands.filter(cmd => /^\/_(prepare|connect group)/.test(cmd))).toEqual([prepare, "/_connect group #12 incognito=on"])
    expect(action).toMatchObject({state: "performing", conn: {group: 12}, chat: "#12"})
    chat.chats.set("#12", {chatInfo: groupChat(joined), items: [item(40, "Trail report", {type: "channelRcv"})]})
    tools.onUserJoinedGroup({type: "userJoinedGroup", user, groupInfo: joined, hostMember: member(2, "relay")})
    await jest.advanceTimersByTimeAsync(30_000)
    expect((await connecting)!.structuredContent).toMatchObject({
      status: "done",
      chat: "#12",
      type: "channel",
      role: "observer",
      known_as: "Quiet Fox",
      connected: true,
      messages: [{item: 40, from: "Ridgeline News", text: "Trail report"}],
    })
    expect(textOf(await tools.call("send", {chat: "#12", text: "hello"}, signal().call))).toBe("The agent has role observer in #12 and can't post.")
  })

  it("leaves out a message still live at the end of the wait, and leaves it unread", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const connected = {...contact(12, "Ridgeline"), activeConn: {connId: 300, connStatus: {type: "ready"}}} as unknown as T.Contact
    chat.chats.set("@12", {chatInfo: direct(connected), items: [item(40, "Welc", {type: "directRcv"}, {live: true}), item(41, "Find groups.")]})
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "ok"}}))
    chat.responses.set(`/_connect 1 ${link}`, {type: "sentInvitation", connection: {pccConnId: 300}})
    const {tools, actions} = connectTools(chat)
    const connecting = tools.call("connect", {target: link}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    actions.approve(actions.get(1)!, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    tools.onContactConnected({type: "contactConnected", user, contact: connected})
    await jest.advanceTimersByTimeAsync(30_000)
    expect((await connecting)!.structuredContent).toMatchObject({status: "done", chat: "@12", connected: true, messages: [{item: 41, text: "Find groups."}]})
    expect(chat.commands.at(-1)).toBe("/_read chat items @12 41")
  })

  it("prepares a contact found by its SimpleX name with the domain, and returns no unverified simplex_name", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const linkData = {profile: {displayName: "SimpleX Directory", fullName: "", peerType: "bot"}, business: false}
    const joining = {...contact(9, "SimpleX Directory"), activeConn: {connId: 209, connStatus: {type: "joined"}}} as unknown as T.Contact
    const ready = {...contact(9, "SimpleX Directory"), activeConn: {connId: 209, connStatus: {type: "ready"}}} as unknown as T.Contact
    const prepare = `/_prepare contact 1 ${link} domain=directory.simplex ${JSON.stringify(linkData)}`
    const name = {nameType: "contact", nameDomain: {nameTLD: "simplex", domain: "directory", subDomain: []}}
    chat.chats.set("@9", {chatInfo: direct(joining), items: []})
    chat.responses.set("/_connect plan 1 @directory.simplex", {...plan({type: "contactAddress", contactAddressPlan: {type: "ok", contactSLinkData_: linkData}}), planSimplexName: name})
    chat.responses.set(prepare, {type: "newPreparedChat", chat: {chatInfo: direct(joining), chatItems: []}})
    chat.responses.set("/_connect contact @9", {type: "startedConnectionToContact", contact: joining})
    const {tools, actions} = connectTools(chat)
    const connecting = tools.call("connect", {target: "@directory.simplex"}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    const action = actions.get(1)!
    expect(action.subject).toEqual({kind: "bot", name: "SimpleX Directory", simplexName: "@directory.simplex", incognito: false})
    actions.approve(action, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    expect(chat.commands.filter(cmd => /^\/_(prepare|connect contact)/.test(cmd))).toEqual([prepare, "/_connect contact @9"])
    chat.chats.set("@9", {chatInfo: direct(ready), items: []})
    tools.onContactConnected({type: "contactConnected", user, contact: ready})
    await jest.advanceTimersByTimeAsync(30_000)
    const result = (await connecting)!.structuredContent
    expect(result).toMatchObject({status: "done", chat: "@9", type: "contact", name: "SimpleX Directory", connected: true})
    expect(result).not.toHaveProperty("simplex_name")
  })
})

describe("download", () => {
  const preview = "data:image/jpg;base64,/9j/4AAQ"
  const image = {type: "image", text: "trail map", image: preview}
  const switchback = member(44, "Switchback")
  const trail = groupChat(group(15, "Trail", member(1, "agent")))
  const folder = join(data, "files", "group-15")
  const downloaded = join(folder, "7.pdf")
  const flushed = (): Promise<void> => new Promise(resolve => setImmediate(resolve))

  function downloadTools(chat: FakeChat, standingApprover: (chatInfo: T.ChatInfo, standing: Standing) => string | undefined = () => undefined, mode: Mode = "requests"): {tools: AgentTools; actions: Actions} {
    const fake = fakeHooks()
    const actions = actionsWith(mode, fake)
    const {tools} = agentTools(chat, actions, standingApprover)
    fake.hooks.perform = (action, signal) => tools.perform(action, signal)
    return {tools, actions}
  }

  const knownServer = "xftp://hash1@xftp1.simplex.im"

  function offeredIn(chat: FakeChat, fileStatus: object = {type: "rcvInvitation"}, fileSource?: object): T.ChatItem {
    const offered = fileItem(30, image, fileStatus, {chatDir: {type: "groupRcv", groupMember: switchback}, fileSource})
    chat.chats.set("#15", {chatInfo: trail, items: [offered]})
    chat.responses.set(`/freceive 7 ${downloaded}`, {type: "rcvFileAccepted", user, chatItem: event(trail, offered)})
    fileServersIn(chat, [knownServer])
    chat.responses.set("/_servers 1", {
      type: "userServers",
      user,
      userServers: [
        {smpServers: [], xftpServers: [userServer("xftp://hash1:secret@xftp1.simplex.im")], chatRelays: []},
        {operator: {operatorId: 2, enabled: false}, smpServers: [], xftpServers: [userServer("xftp://hash3@disabled.example.com")], chatRelays: []},
      ],
    })
    return offered
  }

  function fileServersIn(chat: FakeChat, fileXftpServers: string[]): void {
    chat.responses.set("/_get item info #15 30", {type: "chatItemInfo", user, chatItem: event(trail, chat.chats.get("#15")!.items[0]), chatItemInfo: {itemVersions: [], fileXftpServers}})
  }

  function userServer(server: string): object {
    return {serverId: 1, server, preset: false, enabled: true, roles: {}, deleted: false}
  }

  afterEach(() => {
    rmSync(folder, {recursive: true, force: true})
    rmSync(join(data, "forwarded"), {recursive: true, force: true})
  })

  function completed(path: string): T.AChatItem {
    mkdirSync(dirname(path), {recursive: true})
    writeFileSync(path, "report")
    return event(trail, fileItem(30, image, {type: "rcvComplete"}, {fileSource: {filePath: path}}))
  }

  it("downloads an offered file after the approval and returns its path", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    const action = actions.get(1)!
    expect(action).toMatchObject({tool: "download", state: "pending", chat: "#15", item: 30, from: "Switchback", file: {fileId: 7, name: "report.pdf", size: 2400000, type: "image", preview}})
    actions.approve(action, "Alice")
    await flushed()
    expect(existsSync(folder)).toBe(true)
    expect(chat.commands.filter(cmd => cmd.startsWith("/freceive"))).toEqual([`/freceive 7 ${downloaded}`])
    tools.onFile(completed(downloaded))
    expect((await downloading)!.structuredContent).toEqual({action: 1, status: "done", chat: "#15", item: 30, path: downloaded})
  })

  it("keeps a forwarded file outside the chat's folder and offered, and moves it there when the download is approved", async () => {
    const chat = new FakeChat()
    const offered = offeredIn(chat)
    offered.file!.fileSize = 6
    const forwarded = join(data, "forwarded", "group-15", "7.pdf")
    chat.responses.set(`/freceive 7 ${forwarded}`, {type: "rcvFileAccepted", user, chatItem: event(trail, offered)})
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    const action = actions.get(1) as DownloadAction
    const forwarding = tools.forwardFile(action)
    await flushed()
    writeFileSync(forwarded, "report")
    Object.assign(offered.file!, {fileStatus: {type: "rcvComplete"}, fileSource: {filePath: forwarded}})
    tools.onFile(event(trail, offered))
    expect(readFileSync(await forwarding, "utf8")).toBe("report")
    expect(existsSync(downloaded)).toBe(false)
    const {messages} = (await tools.call("receive", {chat: "#15"}, signal().call))!.structuredContent as {messages: {file: object}[]}
    expect(messages[0].file).toEqual({name: "report.pdf", size: 6, type: "image", status: "offered"})
    actions.approve(action, "Alice")
    expect((await downloading)!.structuredContent).toMatchObject({status: "done", path: downloaded})
    expect([existsSync(forwarded), readFileSync(downloaded, "utf8")]).toEqual([false, "report"])
    expect(chat.commands.filter(cmd => cmd.startsWith("/freceive"))).toEqual([`/freceive 7 ${forwarded}`])
  })

  it("returns performing when the download outlasts wait, and the path to the call that attaches again", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30, wait: 5}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    actions.approve(actions.get(1)!, "Alice")
    await jest.advanceTimersByTimeAsync(4_999)
    let finished = false
    downloading.then(() => (finished = true))
    await jest.advanceTimersByTimeAsync(0)
    expect(finished).toBe(false)
    await jest.advanceTimersByTimeAsync(1)
    expect((await downloading)!.structuredContent).toEqual({action: 1, status: "performing"})
    expect(actions.get(1)).toMatchObject({state: "performing", open: 0})
    const attaching = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    tools.onFile(completed(downloaded))
    expect((await attaching)!.structuredContent).toEqual({action: 1, status: "done", chat: "#15", item: 30, path: downloaded})
    expect(chat.commands.filter(cmd => cmd.startsWith("/freceive"))).toHaveLength(1)
  })

  it("returns the path of a file already downloaded, without downloading it again", async () => {
    const chat = new FakeChat()
    offeredIn(chat, {type: "rcvComplete"}, {filePath: downloaded})
    mkdirSync(folder, {recursive: true})
    writeFileSync(downloaded, "report")
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    actions.approve(actions.get(1)!, "Alice")
    expect((await downloading)!.structuredContent).toMatchObject({status: "done", path: downloaded})
    expect(chat.commands.filter(cmd => cmd.startsWith("/freceive"))).toEqual([])
  })

  it("fails when the file fails, the sender cancels it, its item is gone, or the received file is not in its folder", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat)
    const download = async (settle: () => void): Promise<object> => {
      const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
      await flushed()
      actions.approve(actions.pending()[0], "Alice")
      await flushed()
      settle()
      return (await downloading)!.structuredContent!
    }
    expect(await download(() => tools.onFile(event(trail, fileItem(30, image, {type: "rcvError", rcvFileError: {type: "noFile"}}))))).toMatchObject({status: "failed", error: "File not found - most likely file was deleted or cancelled."})
    expect(await download(() => tools.onFile(event(trail, fileItem(30, image, {type: "rcvCancelled"}))))).toMatchObject({status: "failed", error: "The file was cancelled."})
    const cancelled = {type: "rcvFileSndCancelled", user, chatItem: event(trail, fileItem(30, image, {type: "rcvTransfer", rcvProgress: 1, rcvTotal: 2})), rcvFileTransfer: {fileId: 7}} as unknown as Parameters<AgentTools["onFileCancelled"]>[0]
    expect(await download(() => tools.onFileCancelled(cancelled))).toMatchObject({status: "failed", error: "The sender cancelled the file."})
    const gone = {type: "rcvFileError", user, agentError: {type: "FILE", fileErr: {type: "NO_FILE"}}, rcvFileTransfer: {fileId: 7}} as unknown as Parameters<AgentTools["onFileError"]>[0]
    expect(await download(() => tools.onFileError(gone))).toMatchObject({status: "failed", error: "SimpleX error: errorAgent: FILE: NO_FILE"})
    expect(await download(() => tools.onFile(event(trail, fileItem(30, image, {type: "rcvComplete"}))))).toMatchObject({status: "failed", error: "The received file is not in the agent's files folder."})
  })

  it("fails when no part of the file arrives for 30 minutes, and cancels the file", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const offered = offeredIn(chat)
    chat.responses.set("/fcancel 7", {type: "rcvFileCancelled", user, rcvFileTransfer: {fileId: 7}})
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30, wait: 3600}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    actions.approve(actions.get(1)!, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    offered.file!.fileStatus = {type: "rcvTransfer", rcvProgress: 1000, rcvTotal: 4000}
    await jest.advanceTimersByTimeAsync(30 * 60_000)
    expect(actions.get(1)!.state).toBe("performing")
    expect(chat.commands).not.toContain("/fcancel 7")
    await jest.advanceTimersByTimeAsync(60_000)
    expect((await downloading)!.structuredContent).toEqual({action: 1, status: "failed", error: "The download made no progress for 30 minutes and was cancelled."})
    expect(chat.commands).toContain("/fcancel 7")
  })

  it("fails when its message or chat is deleted", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30, wait: 3600}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    actions.approve(actions.get(1)!, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    chat.chats.delete("#15")
    await jest.advanceTimersByTimeAsync(60_000)
    expect((await downloading)!.structuredContent).toEqual({action: 1, status: "failed", error: "The message or its chat was deleted."})
  })

  it("keeps the download running when a check can't read its message", async () => {
    jest.useFakeTimers()
    const errors = jest.spyOn(process.stderr, "write").mockImplementation(() => true)
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30, wait: 3600}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    actions.approve(actions.get(1)!, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    const getChat = chat.apiGetChat
    chat.apiGetChat = async () => {
      throw new Error("database busy")
    }
    await jest.advanceTimersByTimeAsync(60_000)
    expect(actions.get(1)!.state).toBe("performing")
    expect(errors).toHaveBeenCalledWith("download of file 7: database busy\n")
    chat.apiGetChat = getChat
    tools.onFile(completed(downloaded))
    expect((await downloading)!.structuredContent).toMatchObject({status: "done", path: downloaded})
  })

  describe("a file received without a download", () => {
    let elsewhere: string

    beforeEach(() => {
      elsewhere = realpathSync(mkdtempSync(join(os.tmpdir(), "simplex-mcp-received-")))
    })

    afterEach(() => rmSync(elsewhere, {recursive: true, force: true}))

    function receivedIn(chat: FakeChat, content: string): {source: string; received: T.ChatItem} {
      const source = join(elsewhere, "report.pdf")
      writeFileSync(source, content)
      const received = offeredIn(chat, {type: "rcvComplete"}, {filePath: source})
      received.file!.fileSize = content.length
      return {source, received}
    }

    it("is moved into the chat's folder when it is received, and a download returns it there", async () => {
      const chat = new FakeChat()
      const {source, received} = receivedIn(chat, "voice")
      const {tools, actions} = downloadTools(chat)
      tools.onFileComplete(event(trail, received))
      expect([existsSync(source), readFileSync(downloaded, "utf8")]).toEqual([false, "voice"])
      const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
      await flushed()
      actions.approve(actions.get(1)!, "Alice")
      expect((await downloading)!.structuredContent).toMatchObject({status: "done", path: downloaded})
    })

    it("is copied and then deleted when the rename crosses file systems", () => {
      const chat = new FakeChat()
      const {source, received} = receivedIn(chat, "voice")
      const {tools} = downloadTools(chat)
      const rename = jest.spyOn(fs, "renameSync").mockImplementationOnce(() => {
        throw Object.assign(new Error("EXDEV: cross-device link not permitted"), {code: "EXDEV"})
      })
      tools.onFileComplete(event(trail, received))
      expect([existsSync(source), readFileSync(downloaded, "utf8"), existsSync(`${downloaded}.part`)]).toEqual([false, "voice", false])
      expect(rename).toHaveBeenCalledTimes(2)
    })

    it("is moved into the group's folder also from a member chat of the group", () => {
      const chat = new FakeChat()
      const {source, received} = receivedIn(chat, "voice")
      const {tools} = downloadTools(chat)
      tools.onFileComplete(event({...trail, groupChatScope: {type: "memberSupport"}} as T.ChatInfo, received))
      expect([existsSync(source), readFileSync(downloaded, "utf8")]).toEqual([false, "voice"])
    })

    it("is moved into the chat's folder at the completion of a transfer that started in this run, whatever its size", async () => {
      const chat = new FakeChat()
      const {tools} = downloadTools(chat)
      const source = join(elsewhere, "report.pdf")
      writeFileSync(source, "vo")
      const incoming = offeredIn(chat, {type: "rcvAccepted"}, {filePath: source})
      tools.onNewItems([event(trail, incoming)])
      await flushed()
      writeFileSync(source, "voice")
      Object.assign(incoming.file!, {fileStatus: {type: "rcvComplete"}, fileSize: 6})
      tools.onFileComplete(event(trail, incoming))
      expect([existsSync(source), readFileSync(downloaded, "utf8")]).toEqual([false, "voice"])
    })

    it("is not moved when its size differs from the size in its message", () => {
      const chat = new FakeChat()
      const {source, received} = receivedIn(chat, "voice")
      received.file!.fileSize = 4
      const {tools} = downloadTools(chat)
      tools.onFileComplete(event(trail, received))
      expect([readFileSync(source, "utf8"), existsSync(downloaded)]).toEqual(["voice", false])
    })

    it("keeps its file when an update shows the file failed or cancelled", () => {
      const chat = new FakeChat()
      const {source} = receivedIn(chat, "voice")
      const {tools} = downloadTools(chat)
      for (const fileStatus of [{type: "rcvCancelled"}, {type: "rcvError", rcvFileError: {type: "noFile"}}]) {
        tools.onFile(event(trail, fileItem(30, image, fileStatus, {fileSource: {filePath: source}})))
      }
      expect(readFileSync(source, "utf8")).toBe("voice")
    })

    it("is followed also in a member chat of the group: moved whatever its size, and its partial file deleted at a failure, at a check and by /delete", async () => {
      jest.useFakeTimers()
      jest.spyOn(process.stderr, "write").mockImplementation(() => true)
      const memberChat = {...trail, groupChatScope: {type: "memberSupport"}} as T.ChatInfo
      const chat = new FakeChat()
      chat.responses.set("/fcancel 7", {type: "rcvFileCancelled", user, rcvFileTransfer: {fileId: 7}})
      const {tools} = downloadTools(chat)
      const source = join(elsewhere, "report.pdf")
      const track = async (): Promise<T.ChatItem> => {
        writeFileSync(source, "vo")
        const incoming = offeredIn(chat, {type: "rcvAccepted"}, {filePath: source})
        tools.onNewItems([event(memberChat, incoming)])
        await jest.advanceTimersByTimeAsync(0)
        return incoming
      }
      const received = await track()
      writeFileSync(source, "voice")
      Object.assign(received.file!, {fileStatus: {type: "rcvComplete"}, fileSize: 6})
      tools.onFileComplete(event(memberChat, received))
      expect([existsSync(source), readFileSync(downloaded, "utf8")]).toEqual([false, "voice"])
      rmSync(downloaded)
      const failed = await track()
      await jest.advanceTimersByTimeAsync(60_000)
      expect(existsSync(source)).toBe(true)
      failed.file!.fileStatus = {type: "rcvError", rcvFileError: {type: "noFile"}}
      await jest.advanceTimersByTimeAsync(60_000)
      expect(existsSync(source)).toBe(false)
      await track()
      chat.chats.delete("#15")
      await jest.advanceTimersByTimeAsync(60_000)
      expect(existsSync(source)).toBe(false)
      await track()
      await tools.deleteChatFiles("#15")
      expect(chat.commands).toContain("/fcancel 7")
      expect(existsSync(source)).toBe(false)
    })

    it("leaves no partial file when the sender cancels it or its message is deleted", async () => {
      jest.useFakeTimers()
      jest.spyOn(process.stderr, "write").mockImplementation(() => true)
      const chat = new FakeChat()
      const {tools} = downloadTools(chat)
      const partial = join(elsewhere, "report.pdf")
      const track = async (): Promise<T.ChatItem> => {
        writeFileSync(partial, "part")
        const incoming = offeredIn(chat, {type: "rcvAccepted"}, {filePath: partial})
        tools.onNewItems([event(trail, incoming)])
        await jest.advanceTimersByTimeAsync(0)
        return incoming
      }
      const cancelled = await track()
      tools.onFileCancelled({chatItem: event(trail, cancelled), rcvFileTransfer: {fileId: 7}} as unknown as Parameters<AgentTools["onFileCancelled"]>[0])
      expect(existsSync(partial)).toBe(false)
      await track()
      chat.chats.delete("#15")
      await jest.advanceTimersByTimeAsync(60_000)
      expect(existsSync(partial)).toBe(false)
    })
  })

  it("shows a rejected file in the messages that follow", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    actions.reject(actions.get(1)!, "Alice")
    expect((await downloading)!.structuredContent).toEqual({action: 1, status: "rejected"})
    const {messages} = (await tools.call("receive", {chat: "#15"}, signal().call))!.structuredContent as {messages: {file: object}[]}
    expect(messages[0].file).toEqual({name: "report.pdf", size: 2400000, type: "image", status: "rejected"})
  })

  it("performs at once under the files approval of the chat", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat, (_, standing) => (standing === "files" ? "Alice" : undefined))
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    expect(actions.get(1)).toMatchObject({state: "performing", standing: true, decidedBy: "Alice"})
    expect(actions.get(1)!.card).toBeUndefined()
    tools.onFile(completed(downloaded))
    expect((await downloading)!.structuredContent).toMatchObject({status: "done", path: downloaded})
  })

  it("asks the owner again for a rejected file under the files approval of the chat", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    let approved = false
    const {tools, actions} = downloadTools(chat, (_, standing) => (approved && standing === "files" ? "Alice" : undefined))
    const rejecting = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    actions.reject(actions.get(1)!, "Alice")
    expect((await rejecting)!.structuredContent).toEqual({action: 1, status: "rejected"})
    approved = true
    const {controller, call} = signal()
    const asking = tools.call("download", {chat: "#15", item: 30}, call)
    await flushed()
    expect(actions.get(2)).toMatchObject({state: "pending", standing: false})
    controller.abort()
    expect(await asking).toBeUndefined()
  })

  it("asks the owner for a file on unknown servers under the files approval of the chat", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    fileServersIn(chat, ["xftp://hash2@xftp.example.com"])
    const {tools, actions} = downloadTools(chat, (_, standing) => (standing === "files" ? "Alice" : undefined))
    const {controller, call} = signal()
    const downloading = tools.call("download", {chat: "#15", item: 30}, call)
    await flushed()
    expect(actions.get(1)).toMatchObject({state: "pending", standing: false, unknownServers: ["xftp://hash2@xftp.example.com"]})
    controller.abort()
    expect(await downloading).toBeUndefined()
  })

  it("reads the files approval of the chat just before it creates the action", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    chat.chats.get("#15")!.chatInfo = structuredClone(trail)
    fileServersIn(chat, [])
    const approved = (chatInfo: T.ChatInfo): boolean => (chatInfo as T.ChatInfo.Group).groupInfo.customData !== undefined
    const {tools, actions} = downloadTools(chat, (chatInfo, standing) => (standing === "files" && approved(chatInfo) ? "Alice" : undefined))
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    expect(actions.all()).toEqual([])
    ;(chat.chats.get("#15")!.chatInfo as T.ChatInfo.Group).groupInfo.customData = {simplexMcpFiles: {}}
    fileServersIn(chat, [knownServer])
    tools.onFileDescription()
    await flushed()
    expect(actions.get(1)).toMatchObject({state: "performing", standing: true, decidedBy: "Alice"})
    tools.onFile(completed(downloaded))
    expect((await downloading)!.structuredContent).toMatchObject({status: "done"})
  })

  it("downloads without approving unknown servers in mode host", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    fileServersIn(chat, ["xftp://hash2@xftp.example.com"])
    const {tools} = downloadTools(chat, () => undefined, "host")
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    await flushed()
    expect(chat.commands.filter(cmd => cmd.startsWith("/freceive"))).toEqual([`/freceive 7 ${downloaded}`])
    tools.onFile(completed(downloaded))
    expect((await downloading)!.structuredContent).toMatchObject({status: "done", path: downloaded})
  })

  it("approves the servers of a file only after a card that names them as unknown", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    fileServersIn(chat, ["xftp://hash2@xftp.example.com"])
    chat.responses.set(`/freceive 7 approved_relays=on ${downloaded}`, {type: "rcvFileAccepted", user, chatItem: event(trail, chat.chats.get("#15")!.items[0])})
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    actions.approve(actions.get(1)!, "Alice")
    await flushed()
    expect(chat.commands.filter(cmd => cmd.startsWith("/freceive"))).toEqual([`/freceive 7 approved_relays=on ${downloaded}`])
    tools.onFile(completed(downloaded))
    expect((await downloading)!.structuredContent).toMatchObject({status: "done", path: downloaded})
  })

  it("leaves the servers of a file to the core's check under the files approval", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat, (_, standing) => (standing === "files" ? "Alice" : undefined))
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    expect(actions.get(1)).toMatchObject({state: "performing", standing: true})
    expect(chat.commands.filter(cmd => cmd.startsWith("/freceive"))).toEqual([`/freceive 7 ${downloaded}`])
    tools.onFile(completed(downloaded))
    expect((await downloading)!.structuredContent).toMatchObject({status: "done"})
  })

  it("creates no card or action for an offered file without a file description, whatever its protocol", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const offered = offeredIn(chat)
    fileServersIn(chat, [])
    const {tools, actions} = downloadTools(chat, (_, standing) => (standing === "files" ? "Alice" : undefined))
    for (const fileProtocol of [T.FileProtocol.SMP, T.FileProtocol.XFTP]) {
      offered.file!.fileProtocol = fileProtocol
      const waiting = tools.call("download", {chat: "#15", item: 30, wait: 5}, signal().call)
      await jest.advanceTimersByTimeAsync(5_000)
      expect(textOf(await waiting)).toBe("item: the sender has not uploaded the file of item 30 in #15 yet; call download again later.")
      expect(actions.all()).toEqual([])
    }
  })

  it("asks for a file the core already received without reading its servers", async () => {
    const chat = new FakeChat()
    const received = offeredIn(chat, {type: "rcvComplete"}, {filePath: downloaded})
    received.file!.fileProtocol = T.FileProtocol.SMP
    mkdirSync(folder, {recursive: true})
    writeFileSync(downloaded, "report")
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    expect(chat.commands.filter(cmd => cmd.startsWith("/_get item info"))).toEqual([])
    actions.approve(actions.get(1)!, "Alice")
    expect((await downloading)!.structuredContent).toMatchObject({status: "done", path: downloaded})
  })

  it("fails with the core's words when the core refuses the servers of the file at the download", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat)
    const receive = chat.sendChatCmd.bind(chat)
    chat.sendChatCmd = async cmd => {
      if (cmd.startsWith("/freceive")) throw new core.ChatAPIError("Chat command error", {type: "error", errorType: {type: "fileNotApproved", fileId: 7, unknownServers: ["xftp://hash2@xftp.example.com"]}})
      return receive(cmd)
    }
    const downloading = tools.call("download", {chat: "#15", item: 30}, signal().call)
    await flushed()
    actions.approve(actions.get(1)!, "Alice")
    expect((await downloading)!.structuredContent).toEqual({action: 1, status: "failed", error: "Unknown servers! Without Tor or VPN, your IP address will be visible to these XFTP relays: xftp.example.com."})
  })

  it("fails at the next check when the core returns an accepted file to the offer because its servers are not approved", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat)
    const downloading = tools.call("download", {chat: "#15", item: 30, wait: 3600}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    actions.approve(actions.get(1)!, "Alice")
    await jest.advanceTimersByTimeAsync(60_000)
    expect((await downloading)!.structuredContent).toEqual({action: 1, status: "failed", error: "The file is on unknown servers that were not approved."})
  })

  it("keeps a download whose acceptance has not returned when a check reads the file as offered", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat)
    const receive = chat.sendChatCmd.bind(chat)
    let accept = (): void => undefined
    chat.sendChatCmd = async cmd => {
      if (cmd.startsWith("/freceive")) await new Promise<void>(resolve => (accept = resolve))
      return receive(cmd)
    }
    const downloading = tools.call("download", {chat: "#15", item: 30, wait: 3600}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    actions.approve(actions.get(1)!, "Alice")
    await jest.advanceTimersByTimeAsync(60_000)
    expect(actions.get(1)!.state).toBe("performing")
    accept()
    await jest.advanceTimersByTimeAsync(0)
    tools.onFile(completed(downloaded))
    expect((await downloading)!.structuredContent).toMatchObject({status: "done", path: downloaded})
  })

  it("waits for the file description before it asks, and names the unknown servers on the card", async () => {
    const chat = new FakeChat()
    offeredIn(chat)
    fileServersIn(chat, [])
    const {tools, actions} = downloadTools(chat)
    const {call, progress, controller} = signal()
    const downloading = tools.call("download", {chat: "#15", item: 30}, call)
    await flushed()
    expect(actions.all()).toEqual([])
    expect(progress).toEqual([expect.stringMatching(/^The sender is uploading the file of item 30 in #15; waiting until \d{4}-\d\d-\d\d \d\d:\d\d UTC\.$/)])
    const unknown = ["xftp://hash2@xftp.example.com", "xftp://hash3@disabled.example.com"]
    fileServersIn(chat, [knownServer, ...unknown])
    tools.onFileDescription()
    await flushed()
    const action = actions.get(1) as DownloadAction
    expect(action.unknownServers).toEqual(unknown)
    expect(cardText(action, "requests").split("\n")[2]).toBe("Servers: `xftp.example.com`, `disabled.example.com`, unknown: downloading shows them this computer's IP address.")
    controller.abort()
    expect(await downloading).toBeUndefined()
  })

  it("asks at once when the servers are known, and returns a tool error when the sender's upload outlasts wait", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    offeredIn(chat)
    const {tools, actions} = downloadTools(chat)
    const {call, controller} = signal()
    const asking = tools.call("download", {chat: "#15", item: 30}, call)
    await jest.advanceTimersByTimeAsync(0)
    expect(cardText(actions.get(1)!, "requests").split("\n")[2]).toBe("Servers: known.")
    controller.abort()
    expect(await asking).toBeUndefined()
    actions.reject(actions.get(1)!, "Alice")
    expect((await tools.call("download", {chat: "#15", item: 30}, signal().call))!.structuredContent).toEqual({action: 1, status: "rejected"})

    fileServersIn(chat, [])
    const waiting = tools.call("download", {chat: "#15", item: 30, wait: 5}, signal().call)
    await jest.advanceTimersByTimeAsync(4_999)
    expect(actions.all()).toEqual([])
    await jest.advanceTimersByTimeAsync(1)
    expect(textOf(await waiting)).toBe("item: the sender has not uploaded the file of item 30 in #15 yet; call download again later.")
    expect(actions.all()).toEqual([])
  })

  it("refuses an unknown item, an item without a received file, and a failed file", async () => {
    const chat = new FakeChat()
    chat.chats.set("#15", {
      chatInfo: trail,
      items: [item(20, "hello", {type: "groupRcv", groupMember: switchback}), fileItem(30, image, {type: "rcvError", rcvFileError: {type: "auth"}}, {chatDir: {type: "groupRcv", groupMember: switchback}})],
    })
    const {tools, actions} = downloadTools(chat)
    const text = async (args: object): Promise<string> => textOf(await tools.call("download", args, signal().call))
    expect(await text({chat: "#15", item: 99})).toBe("item: #15 has no item 99.")
    expect(await text({chat: "#15", item: 20})).toBe("item: item 20 in #15 has no received file.")
    expect(await text({chat: "#15", item: 30})).toBe("item: the file of item 30 in #15 failed: Wrong key or unknown file chunk address - most likely file is deleted.")
    expect(await text({chat: "#10", item: 1})).toBe("No chat #10. list shows the agent's chats.")
    expect(actions.all()).toEqual([])
  })
})

describe("leave", () => {
  function leaveTools(chat: FakeChat): {tools: AgentTools; actions: Actions} {
    const fake = fakeHooks()
    const actions = actionsWith("requests", fake)
    const {tools} = agentTools(chat, actions)
    fake.hooks.perform = (action, signal) => tools.perform(action, signal)
    return {tools, actions}
  }

  it("deletes a contact with notification", async () => {
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory", {peerType: "bot"})), items: []})
    const {tools, actions} = leaveTools(chat)
    const leaving = tools.call("leave", {chat: "@4"}, signal().call)
    await settled()
    const action = actions.get(1)!
    expect(action).toMatchObject({tool: "leave", state: "pending", chat: "@4", subject: {kind: "bot", name: "SimpleX Directory", incognito: false}})
    actions.approve(action, "Alice")
    expect((await leaving)!.structuredContent).toEqual({action: 1, status: "done", chat: "@4"})
    expect(chat.left).toEqual([])
    expect(chat.deleted).toEqual([{chatType: T.ChatType.Direct, chatId: 4, mode: {type: "full", notify: true}}])
  })

  it("leaves a group whose membership is current before deleting it", async () => {
    const chat = new FakeChat()
    chat.chats.set("#15", {chatInfo: groupChat(group(15, "Trail", member(1, "agent"))), items: []})
    chat.chats.set("#16", {chatInfo: groupChat(group(16, "Old", member(1, "agent", T.GroupMemberRole.Member, T.GroupMemberStatus.Removed))), items: []})
    const {tools, actions} = leaveTools(chat)
    for (const [n, ref] of [[1, "#15"], [2, "#16"]] as const) {
      const leaving = tools.call("leave", {chat: ref}, signal().call)
      await settled()
      actions.approve(actions.get(n)!, "Alice")
      expect((await leaving)!.structuredContent).toEqual({action: n, status: "done", chat: ref})
    }
    expect(chat.left).toEqual([15])
    expect(chat.deleted).toEqual([
      {chatType: T.ChatType.Group, chatId: 15, mode: {type: "full", notify: true}},
      {chatType: T.ChatType.Group, chatId: 16, mode: {type: "full", notify: true}},
    ])
  })

  it("leaves a group whose membership waits for approval or review", async () => {
    const chat = new FakeChat()
    chat.chats.set("#17", {chatInfo: groupChat(group(17, "Knock", member(1, "agent", T.GroupMemberRole.Member, T.GroupMemberStatus.Pending_approval))), items: []})
    chat.chats.set("#18", {chatInfo: groupChat(group(18, "Review", member(1, "agent", T.GroupMemberRole.Member, T.GroupMemberStatus.Pending_review))), items: []})
    const {tools, actions} = leaveTools(chat)
    for (const [n, ref] of [[1, "#17"], [2, "#18"]] as const) {
      const leaving = tools.call("leave", {chat: ref}, signal().call)
      await settled()
      actions.approve(actions.get(n)!, "Alice")
      expect((await leaving)!.structuredContent).toEqual({action: n, status: "done", chat: ref})
    }
    expect(chat.left).toEqual([17, 18])
  })

  it("attaches a repeated call to its action after the chat is deleted", async () => {
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Bob")), items: []})
    const {tools, actions} = leaveTools(chat)
    const first = signal()
    const leaving = tools.call("leave", {chat: "@4"}, first.call)
    await settled()
    first.controller.abort()
    expect(await leaving).toBeUndefined()
    actions.approve(actions.get(1)!, "Alice")
    await settled()
    expect(chat.chats.has("@4")).toBe(false)
    expect((await tools.call("leave", {chat: "@4"}, signal().call))!.structuredContent).toEqual({action: 1, status: "done", chat: "@4"})
    expect(textOf(await tools.call("leave", {chat: "@4"}, signal().call))).toBe("No chat @4. list shows the agent's chats.")
  })

  it("refuses the agent control chat and unknown chats, and returns a rejection", async () => {
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Bob")), items: []})
    const {tools, actions} = leaveTools(chat)
    expect(textOf(await tools.call("leave", {chat: "#10"}, signal().call))).toBe("No chat #10. list shows the agent's chats.")
    expect(textOf(await tools.call("leave", {chat: "@99"}, signal().call))).toBe("No chat @99. list shows the agent's chats.")
    expect(actions.all()).toEqual([])
    const leaving = tools.call("leave", {chat: "@4"}, signal().call)
    await settled()
    actions.reject(actions.get(1)!, "Alice")
    expect((await leaving)!.structuredContent).toEqual({action: 1, status: "rejected"})
    expect(chat.deleted).toEqual([])
  })

  it("deletes the status before it leaves the chat, and starts and shows none while it leaves", async () => {
    const {chat, tools, actions, status} = await statusChat()
    const remove = chat.apiDeleteChat.bind(chat)
    const deletionsAtLeave: number[] = []
    const notesAtLeave: string[] = []
    chat.apiDeleteChat = async (chatType, chatId, mode) => {
      deletionsAtLeave.push(chat.deletedItems.length)
      chat.addItem("@4", item(21, "and the tests?"))
      await tools.call("receive", {chat: "@4", typing: true}, signal().call)
      notesAtLeave.push(textOf(await tools.call("send", {chat: "@4", text: "Reading the file…", live: true}, signal().call)))
      await jest.advanceTimersByTimeAsync(0)
      return remove(chatType, chatId, mode)
    }
    const leaving = tools.call("leave", {chat: "@4"}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    actions.approve(actions.get(1)!, "Alice")
    expect((await leaving)!.structuredContent).toEqual({action: 1, status: "done", chat: "@4"})
    expect(chat.deletedItems).toEqual([broadcastDeletion(T.ChatType.Direct, 4, status)])
    expect(deletionsAtLeave).toEqual([1])
    expect(notesAtLeave).toEqual(["No chat @4. list shows the agent's chats."])
    expect(chat.sent).toHaveLength(1)
  })
})

describe("list", () => {
  it("marks the chats approved for all messages", async () => {
    const chat = new FakeChat()
    chat.chats.set("@4", {chatInfo: direct(contact(4, "SimpleX Directory", {peerType: "bot"})), items: [item(20, "hi")]})
    chat.chats.set("#2", {chatInfo: groupChat(group(2, "ep", member(1, "agent"))), items: []})
    chat.chats.set("#10", {chatInfo: groupChat(group(10, "agent")), items: []})
    const {tools} = agentTools(chat, actionsWith("requests", fakeHooks()), chatInfo => (chatInfo.type === "direct" ? "Alice" : undefined))
    const {chats} = (await tools.call("list", {}, signal().call))!.structuredContent as {chats: object[]}
    expect(chats).toEqual([
      {chat: "@4", type: "contact", name: "SimpleX Directory", peer_type: "bot", incognito: false, connected: true, unread: 1, approved: true},
      {chat: "#2", type: "group", name: "ep", incognito: false, connected: true, unread: 0},
    ])
    expect(chats[1]).not.toHaveProperty("approved")
  })

  it("reads all chats with one command, so chats with the same time are all listed", async () => {
    const chat = new FakeChat()
    for (let i = 100; i < 250; i++) chat.chats.set(`@${i}`, {chatInfo: direct(contact(i, `contact ${i}`)), items: []})
    const {tools} = agentTools(chat)
    const {chats} = (await tools.call("list", {}, signal().call))!.structuredContent as {chats: {chat: string}[]}
    expect(chats.map(({chat: ref}) => ref)).toEqual(Array.from({length: 150}, (_, i) => `@${100 + i}`))
    expect(chat.commands.filter(cmd => cmd.startsWith("/_get chats "))).toEqual(['/_get chats 1 count=1000000 {"type":"filters","favorite":false,"unread":false}'])
  })
})

describe("call", () => {
  it("marks a call as running until it ends, also when it throws", async () => {
    const {chat, tools} = await statusChat()
    const trail = groupChat(group(15, "Trail", member(1, "agent")))
    const offered = fileItem(30, {type: "file", text: "report"}, {type: "rcvInvitation"}, {chatDir: {type: "groupRcv", groupMember: member(44, "Switchback")}})
    chat.chats.set("#15", {chatInfo: trail, items: [offered]})
    chat.responses.set("/_get item info #15 30", {type: "chatItemInfo", user, chatItem: event(trail, offered), chatItemInfo: {itemVersions: [], fileXftpServers: []}})
    chat.responses.set("/_servers 1", {type: "userServers", user, userServers: []})
    const {controller, call, progress} = signal()
    const downloading = tools.call("download", {chat: "#15", item: 30}, call)
    await jest.advanceTimersByTimeAsync(3_500)
    expect(progress).toEqual([expect.stringMatching(/^The sender is uploading the file of item 30 in #15; /)])
    controller.abort()
    expect(await downloading).toBeUndefined()
    chat.apiGetChats = async () => {
      throw new Error("database busy")
    }
    expect(textOf(await tools.call("list", {}, signal().call))).toBe("database busy")
    await jest.advanceTimersByTimeAsync(3_000)
    expect(chat.updates.map(({text}) => text)).toEqual(["◓ using download · 0:03", "◑ thinking · 0:06"])
  })
})

describe("deleteLeftoverStatuses", () => {
  it("reads only approved chats with connected people and approved groups, and none in mode host, and deletes their own live items", async () => {
    jest.useFakeTimers({now: Date.parse("2026-10-17T12:00:00Z")})
    const chat = new FakeChat()
    const leftover = (itemId: number, chatDir: T.CIDirection = {type: "directSnd"}): T.ChatItem => item(itemId, "◐ thinking · 0:00", chatDir, {live: true})
    chat.chats.set("@4", {chatInfo: direct(contact(4, "Alice")), items: [leftover(20)]})
    chat.chats.set("@5", {chatInfo: direct(contact(5, "Bob")), items: [leftover(21)]})
    chat.chats.set("@7", {chatInfo: direct(contact(7, "Switchback", {peerType: "bot"})), items: [leftover(22)]})
    chat.chats.set("@2", {chatInfo: direct({...contact(2, "Carol"), activeConn: undefined}), items: [leftover(23)]})
    chat.chats.set("#15", {chatInfo: groupChat(group(15, "Trail", member(1, "agent"))), items: [leftover(24, {type: "groupSnd"})]})
    chat.chats.set("#13", {chatInfo: groupChat(group(13, "Ridgeline News", member(1, "agent"), {useRelays: true})), items: [leftover(25, {type: "groupSnd"})]})
    chat.chats.set("#10", {chatInfo: groupChat(group(10, "agent")), items: []})
    const reads = (): string[] => chat.commands.filter(cmd => cmd.startsWith("/_get chat "))
    await approvedTools(chat, "host").tools.deleteLeftoverStatuses()
    expect(reads()).toEqual([])
    const {tools, approved} = approvedTools(chat)
    for (const ref of ["@7", "@2", "#15", "#13", "#10"]) approved.add(ref)
    await tools.deleteLeftoverStatuses()
    expect(reads()).toEqual(["/_get chat @4 count=100", "/_get chat #15 count=100"])
    expect(chat.deletedItems).toEqual([broadcastDeletion(T.ChatType.Direct, 4, 20), broadcastDeletion(T.ChatType.Group, 15, 24)])
  })
})
