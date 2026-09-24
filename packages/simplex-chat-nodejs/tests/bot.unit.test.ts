import {ChatEvent, T} from "@simplex-chat/types"
import * as api from "../src/api"
import {subscribeChatItems} from "../src/bot"

type Handler = (evt: ChatEvent) => Promise<void>

function fakeBot(): {bot: api.ChatApi, deliver: (items: T.AChatItem[]) => Promise<void>} {
  let handler: Handler | undefined
  const bot = {on: (_event: string, h: Handler) => { handler = h }} as unknown as api.ChatApi
  const deliver = (chatItems: T.AChatItem[]) => handler!({type: "newChatItems", chatItems} as unknown as ChatEvent)
  return {bot, deliver}
}

function item(type: "rcvMsgContent" | "sndMsgContent", text: string): T.AChatItem {
  return {chatItem: {content: {type, msgContent: {type: "text", text}}}} as unknown as T.AChatItem
}

describe("subscribeChatItems", () => {
  it("sends a known command only to its handler", async () => {
    const {bot, deliver} = fakeBot()
    const calls: string[] = []
    subscribeChatItems(bot, async () => { calls.push("message") }, {help: async () => { calls.push("help") }})
    await deliver([item("rcvMsgContent", "/help")])
    expect(calls).toEqual(["help"])
  })

  it("sends an unknown command to the fallback handler", async () => {
    const {bot, deliver} = fakeBot()
    const calls: string[] = []
    subscribeChatItems(bot, async () => { calls.push("message") }, {"": async (_ci, cmd) => { calls.push(`fallback:${cmd.keyword}`) }})
    await deliver([item("rcvMsgContent", "/unknown")])
    expect(calls).toEqual(["fallback:unknown"])
  })

  it("sends unhandled commands and text to onMessage", async () => {
    const {bot, deliver} = fakeBot()
    const calls: string[] = []
    subscribeChatItems(bot, async (_ci, content) => { calls.push(`message:${(content as T.MsgContent & {text: string}).text}`) }, {help: async () => { calls.push("help") }})
    await deliver([item("rcvMsgContent", "/unknown"), item("rcvMsgContent", "hello")])
    expect(calls).toEqual(["message:/unknown", "message:hello"])
  })

  it("passes the chat api to handlers", async () => {
    const {bot, deliver} = fakeBot()
    const chats: api.ChatApi[] = []
    subscribeChatItems(bot, async (_ci, _content, chat) => { chats.push(chat) }, {help: async (_ci, _cmd, chat) => { chats.push(chat) }})
    await deliver([item("rcvMsgContent", "/help"), item("rcvMsgContent", "hello")])
    expect(chats).toEqual([bot, bot])
  })

  it("ignores sent items", async () => {
    const {bot, deliver} = fakeBot()
    const calls: string[] = []
    subscribeChatItems(bot, async () => { calls.push("message") }, {help: async () => { calls.push("help") }})
    await deliver([item("sndMsgContent", "/help"), item("sndMsgContent", "hello")])
    expect(calls).toEqual([])
  })
})
