import {CallToolResult} from "@modelcontextprotocol/server"
import {CR, T} from "@simplex-chat/types"
import {core} from "simplex-chat"
import {Actions} from "../../src/actions"
import {cardText} from "../../src/control"
import {ChatItemsWatch, Collected, awaitResponses, toMessage} from "../../src/messages"
import {AgentTools, classifyPlan} from "../../src/tools"
import {FakeChat, actionsWith, contact, direct, event, fakeHooks, group, groupChat, item, member, settled, signal, user} from "./fixtures"

beforeEach(() => {
  jest.spyOn(process.stdout, "write").mockImplementation(() => true)
})

afterEach(() => {
  jest.restoreAllMocks()
  jest.useRealTimers()
})

describe("message fields", () => {
  const self = {name: "agent", type: "agent"}

  it("describes a group message", () => {
    const switchback = member(44, "Switchback")
    switchback.memberProfile.peerType = T.ChatPeerType.Bot
    const chatInfo = groupChat(group(15, "Trail"))
    expect(toMessage(chatInfo, item(31, "Ridgecrest, $142", {type: "groupRcv", groupMember: switchback}, {quoted: 30, mention: true, ts: "2026-10-17T11:00:00Z"}), self)).toEqual({
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
    expect(toMessage(directory, item(21, "found 3", {type: "directRcv"}, {status: "rcvRead"}), self)).toMatchObject({chat: "@4", from: "SimpleX Directory", from_type: "bot", new: false})
    expect(toMessage(directory, item(20, "shoes", {type: "directSnd"}), self)).toEqual({
      chat: "@4",
      item: 20,
      from: "agent",
      from_type: "agent",
      time: "2026-10-17T10:00:00Z",
      text: "shoes",
      new: false,
      mentions_me: false,
    })
    const channel = toMessage(groupChat(group(12, "Ridgeline News")), item(40, "post", {type: "channelRcv"}), self)
    expect(channel).toMatchObject({chat: "#12", from: "Ridgeline News"})
    expect(channel).not.toHaveProperty("from_member")
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

function agentTools(chat: FakeChat, actions: Actions = actionsWith("requests", fakeHooks()), standingApprover: (chatInfo: T.ChatInfo) => string | undefined = () => undefined): {tools: AgentTools; watch: ChatItemsWatch} {
  const watch = new ChatItemsWatch()
  return {tools: new AgentTools({chat: chat.api(), user, actions, watch, controlChat: () => "#10", standingApprover}, "requests"), watch}
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
})

describe("send", () => {
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
    await settled()
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
    await settled()
    actions.approve(actions.get(1)!, "Alice")
    expect((await sending)!.structuredContent).toMatchObject({status: "done", responses: [], more: 0})
    expect(chat.sent[0].messages[0]).toEqual({msgContent: {type: "text", text: "@'Ana Lu' hi @Switchback, any news?"}, mentions: {Switchback: 44, "Ana Lu": 45}, quotedItemId: undefined})
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
      return classifyPlan(plan({type: "contactAddress", contactAddressPlan: {type: "known", contact: prepared}}) as CR.ConnectionPlan, {target: link})
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
    expect(chat.commands).toContain(`/_connect contact @7 incognito=off json {"type":"text","text":"trail shoes?"}`)
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
    chat.responses.set(`/_connect contact @8 incognito=off json {"type":"text","text":"hello"}`, {type: "startedConnectionToContact", contact: started})
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
    chat.responses.set(`/_connect 1 incognito=off ${link}`, {type: "sentInvitation", connection: {pccConnId: 300}})
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
    chat.responses.set(`/_connect 1 incognito=off ${link}`, {type: "sentInvitation", connection: {pccConnId: 300}})
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
    chat.responses.set(`/_connect 1 incognito=off ${link}`, {type: "sentInvitation", connection: {pccConnId: 300}})
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

  it("returns at the end of wait, counted from the approval, when no reply to message arrives", async () => {
    jest.useFakeTimers()
    const chat = new FakeChat()
    const linkData = {profile: {displayName: "Ridgeline", fullName: ""}, business: false}
    const started = {...contact(8, "Ridgeline"), activeConn: {connId: 208, connStatus: {type: "ready"}}} as unknown as T.Contact
    chat.chats.set("@8", {chatInfo: direct(started), items: []})
    chat.responses.set(`/_connect plan 1 ${link}`, plan({type: "contactAddress", contactAddressPlan: {type: "ok", contactSLinkData_: linkData}}))
    chat.responses.set(`/_prepare contact 1 ${link} ${JSON.stringify(linkData)}`, {type: "newPreparedChat", chat: {chatInfo: direct(started), chatItems: []}})
    chat.responses.set(`/_connect contact @8 incognito=off json {"type":"text","text":"hello"}`, {type: "startedConnectionToContact", contact: started})
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
    chat.responses.set(`/_connect 1 incognito=off ${link}`, {type: "sentInvitation", connection: {pccConnId: 300}})
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
    chat.responses.set("/_connect contact @9 incognito=off", {type: "startedConnectionToContact", contact: joining})
    const {tools, actions} = connectTools(chat)
    const connecting = tools.call("connect", {target: "@directory.simplex"}, signal().call)
    await jest.advanceTimersByTimeAsync(0)
    const action = actions.get(1)!
    expect(action.subject).toEqual({kind: "bot", name: "SimpleX Directory", simplexName: "@directory.simplex", incognito: false})
    actions.approve(action, "Alice")
    await jest.advanceTimersByTimeAsync(0)
    expect(chat.commands.filter(cmd => /^\/_(prepare|connect contact)/.test(cmd))).toEqual([prepare, "/_connect contact @9 incognito=off"])
    chat.chats.set("@9", {chatInfo: direct(ready), items: []})
    tools.onContactConnected({type: "contactConnected", user, contact: ready})
    await jest.advanceTimersByTimeAsync(30_000)
    const result = (await connecting)!.structuredContent
    expect(result).toMatchObject({status: "done", chat: "@9", type: "contact", name: "SimpleX Directory", connected: true})
    expect(result).not.toHaveProperty("simplex_name")
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
    expect(chat.commands.filter(cmd => cmd.startsWith("/_get chats "))).toEqual(["/_get chats 1 pcc=off count=1000000"])
  })
})
