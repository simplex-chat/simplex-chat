import {ChatEvent, ChatResponse, T} from "@simplex-chat/types"
import * as api from "../src/api"
import * as core from "../src/core"

const user = {userId: 1} as T.User

async function chatWithResponse(response: object): Promise<api.ChatApi> {
  jest.spyOn(core, "loadLibrary").mockResolvedValue()
  jest.spyOn(core, "chatMigrateInit").mockResolvedValue(BigInt(1))
  jest.spyOn(core, "chatSendCmd").mockResolvedValue(response as ChatResponse)
  return api.ChatApi.init({type: "sqlite", filePrefix: "unused"})
}

afterEach(() => jest.restoreAllMocks())

describe("documented success responses", () => {
  it("apiChatItemReaction returns the reaction", async () => {
    const reaction = {chatReaction: {reaction: {type: "emoji", emoji: "👍"}}}
    const chat = await chatWithResponse({type: "chatItemReaction", user, added: true, reaction})
    await expect(chat.apiChatItemReaction(T.ChatType.Direct, 1, 2, true, {type: "emoji", emoji: "👍"})).resolves.toEqual(reaction)
  })

  it("apiUpdateChatItem accepts chatItemNotChanged", async () => {
    const chatItem = {meta: {itemId: 2}}
    const chat = await chatWithResponse({type: "chatItemNotChanged", user, chatItem: {chatItem}})
    await expect(chat.apiUpdateChatItem(T.ChatType.Direct, 1, 2, {type: "text", text: "same"}, false)).resolves.toEqual(chatItem)
  })

  it("apiUpdateChatItem updates and finalizes a live message", async () => {
    const chatItem = {meta: {itemId: 2}}
    const chat = await chatWithResponse({type: "chatItemUpdated", user, chatItem: {chatItem}})
    await expect(chat.apiUpdateChatItem(T.ChatType.Group, 1, 2, {type: "text", text: "partial"}, true)).resolves.toEqual(chatItem)
    await expect(chat.apiUpdateChatItem(T.ChatType.Group, 1, 2, {type: "text", text: "final"})).resolves.toEqual(chatItem)
    expect(jest.mocked(core.chatSendCmd).mock.calls.map(([, cmd]) => cmd)).toEqual([
      '/_update item #1 2 live=on json {"msgContent":{"type":"text","text":"partial"},"mentions":{}}',
      '/_update item #1 2 json {"msgContent":{"type":"text","text":"final"},"mentions":{}}',
    ])
  })

  it("apiSetProfileAddress accepts userProfileNoChange", async () => {
    const chat = await chatWithResponse({type: "userProfileNoChange", user})
    await expect(chat.apiSetProfileAddress(1, true)).resolves.toEqual({updateSuccesses: 0, updateFailures: 0, changedContacts: []})
  })

  it("apiSetUserDomain sets and removes the name", async () => {
    const chat = await chatWithResponse({type: "userProfileNoChange", user})
    await expect(chat.apiSetUserDomain(1, "calc.simplex")).resolves.toEqual(user)
    await expect(chat.apiSetUserDomain(1)).resolves.toEqual(user)
    expect(jest.mocked(core.chatSendCmd).mock.calls.map(([, cmd]) => cmd)).toEqual(["/_set domain 1 calc.simplex", "/_set domain 1"])
  })

  it("apiReceiveFile reports a file cancelled by sender", async () => {
    const chat = await chatWithResponse({type: "rcvFileAcceptedSndCancelled", user, rcvFileTransfer: {}})
    await expect(chat.apiReceiveFile(3)).rejects.toThrow("file cancelled by sender")
  })
})

describe("startChat lifecycle", () => {
  function chatWithResponses(...responses: object[]): Promise<api.ChatApi> {
    jest.spyOn(core, "loadLibrary").mockResolvedValue()
    jest.spyOn(core, "chatMigrateInit").mockResolvedValue(BigInt(1))
    jest.spyOn(core, "chatRecvMsgWait").mockImplementation(() => new Promise(resolve => setTimeout(() => resolve(undefined), 10)))
    const send = jest.spyOn(core, "chatSendCmd")
    for (const r of responses) send.mockResolvedValueOnce(r as ChatResponse)
    return api.ChatApi.init({type: "sqlite", filePrefix: "unused"})
  }

  it("rejects a second start", async () => {
    const chat = await chatWithResponses({type: "chatStarted"}, {type: "chatStopped"})
    await chat.startChat()
    await expect(chat.startChat()).rejects.toThrow("chat already started")
    await chat.stopChat()
  })

  it("stops the events loop when start fails", async () => {
    const chat = await chatWithResponses({type: "chatCmdError"})
    await expect(chat.startChat()).rejects.toThrow("error starting chat")
    expect(chat.started).toBe(false)
  })

  it("rejects start after close", async () => {
    const chat = await chatWithResponses({type: "chatStopped"})
    jest.spyOn(core, "chatCloseStore").mockResolvedValue()
    await chat.close()
    await expect(chat.startChat()).rejects.toThrow("chat api controller not initialized")
  })

  it("stops the chat before closing the store", async () => {
    const chat = await chatWithResponses()
    const calls: string[] = []
    jest.mocked(core.chatSendCmd).mockImplementation(async (_ctrl, cmd) => {
      calls.push(`send ${cmd}`)
      return {type: "chatStopped"} as ChatResponse
    })
    jest.spyOn(core, "chatCloseStore").mockImplementation(async () => { calls.push("closeStore") })
    await chat.close()
    expect(calls).toEqual(["send /_stop", "closeStore"])
    expect(chat.initialized).toBe(false)
  })

  it("does not close the store when stopping fails", async () => {
    const chat = await chatWithResponses({type: "chatCmdError"})
    const closeStore = jest.spyOn(core, "chatCloseStore").mockResolvedValue()
    await expect(chat.close()).rejects.toThrow("error stopping chat")
    expect(closeStore).not.toHaveBeenCalled()
    expect(chat.initialized).toBe(true)
  })

  it("reports stop failures as stop errors", async () => {
    const chat = await chatWithResponses({type: "chatStarted"}, {type: "chatCmdError"}, {type: "chatStopped"})
    await chat.startChat()
    await expect(chat.stopChat()).rejects.toThrow("error stopping chat")
    expect(chat.started).toBe(true)
    await chat.stopChat()
  })

  it("passes the chat api to event subscribers", async () => {
    const chat = await chatWithResponses({type: "chatStarted"}, {type: "chatStopped"})
    jest.mocked(core.chatRecvMsgWait).mockResolvedValueOnce({type: "hostConnected"} as ChatEvent)
    const subscriberChat = new Promise(resolve => chat.on("hostConnected", (_event, c) => resolve(c)))
    await chat.startChat()
    await expect(subscriberChat).resolves.toBe(chat)
    await chat.stopChat()
  })

  it("passes event errors to chatError subscribers", async () => {
    const chat = await chatWithResponses({type: "chatStarted"}, {type: "chatStopped"})
    const chatError = {type: "errorStore", storeError: {type: "connectionNotFound", agentConnId: "Y29ubg=="}} as T.ChatError
    jest.mocked(core.chatRecvMsgWait).mockRejectedValueOnce(new core.ChatAPIError("Chat event error (see chatError property)", chatError))
    const received = new Promise(resolve => chat.on("chatError", event => resolve(event)))
    await chat.startChat()
    await expect(received).resolves.toEqual({type: "chatError", chatError})
    await chat.stopChat()
  })

  it("receives with a 500 ms wait", async () => {
    const chat = await chatWithResponses()
    await chat.recvChatEvent()
    expect(core.chatRecvMsgWait).toHaveBeenCalledWith(BigInt(1), 500_000)
  })
})

describe("LiveMessage", () => {
  const interval = 50
  const json = JSON.stringify

  function recordingChat() {
    const calls: string[] = []
    let itemId = 0
    const ref = (chatType: T.ChatType, chatId: number) => T.ChatType.cmdString(chatType) + chatId
    const chat = {
      apiSendMessages: async ([chatType, chatId]: [T.ChatType, number], [{msgContent}]: T.ComposedMessage[], live: boolean) => {
        calls.push(`send ${ref(chatType, chatId)} ${json(msgContent.text)} live=${live}`)
        return [{chatItem: {meta: {itemId: ++itemId}}}] as unknown as T.AChatItem[]
      },
      apiUpdateChatItem: async (chatType: T.ChatType, chatId: number, chatItemId: number, msgContent: T.MsgContent, live: boolean) => {
        calls.push(`update ${ref(chatType, chatId)} ${chatItemId} ${json(msgContent.text)} live=${live}`)
        return {} as T.ChatItem
      },
      apiDeleteChatItems: async (chatType: T.ChatType, chatId: number, chatItemIds: number[], deleteMode: T.CIDeleteMode) => {
        calls.push(`delete ${ref(chatType, chatId)} ${chatItemIds} ${deleteMode}`)
        return [] as T.ChatItemDeletion[]
      },
    }
    return {chat, calls}
  }

  function recordingLiveMessage(): {live: api.LiveMessage, calls: string[]} {
    const {chat, calls} = recordingChat()
    return {live: new api.LiveMessage(chat, T.ChatType.Group, 5, interval), calls}
  }

  function wait(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms))
  }

  it("sends the first text at once, updates at most once per interval and finalizes", async () => {
    const {live, calls} = recordingLiveMessage()
    live.update("")
    live.update("Hel")
    live.update("Hello")
    await wait(interval / 5)
    expect(calls).toEqual(['send #5 "" live=true'])
    await wait(interval * 2)
    expect(calls).toEqual(['send #5 "" live=true', 'update #5 1 "Hello" live=true'])
    await live.end("Hello world")
    expect(calls).toEqual(['send #5 "" live=true', 'update #5 1 "Hello" live=true', 'update #5 1 "Hello world" live=false'])
  })

  it("does not repeat unchanged text", async () => {
    const {live, calls} = recordingLiveMessage()
    live.update("Hello")
    live.update("Hello")
    await wait(interval * 2)
    await live.end()
    expect(calls).toEqual(['send #5 "Hello" live=true', 'update #5 1 "Hello" live=false'])
  })

  it("deletes a live message without text", async () => {
    const {live, calls} = recordingLiveMessage()
    live.update("")
    await live.end()
    expect(calls).toEqual(['send #5 "" live=true', "delete #5 1 broadcast"])
  })

  it("sends the final text without a live message", async () => {
    const {live, calls} = recordingLiveMessage()
    await live.end()
    await live.end("Done")
    expect(calls).toEqual(['send #5 "Done" live=false'])
  })

  it("finalizes the last text", async () => {
    const {live, calls} = recordingLiveMessage()
    live.update("")
    live.update("partial")
    await live.end()
    expect(calls).toEqual(['send #5 "" live=true', 'update #5 1 "partial" live=false'])
  })

  it("starts the next live message after the end", async () => {
    const {live, calls} = recordingLiveMessage()
    live.update("")
    void live.end("first")
    live.update("")
    await live.end("second")
    expect(calls).toEqual([
      'send #5 "" live=true',
      'update #5 1 "first" live=false',
      'send #5 "" live=true',
      'update #5 2 "second" live=false',
    ])
  })

  it("passes errors to the handler and repeats the change with the next text", async () => {
    const {chat, calls} = recordingChat()
    const error = new Error("update failed")
    chat.apiUpdateChatItem = jest.fn(chat.apiUpdateChatItem).mockRejectedValueOnce(error)
    const errors: unknown[] = []
    const live = new api.LiveMessage(chat, T.ChatType.Group, 5, interval, e => errors.push(e))
    live.update("")
    live.update("Hello")
    await wait(interval * 2)
    await live.end("Hello world")
    expect(errors).toEqual([error])
    expect(calls).toEqual(['send #5 "" live=true', 'update #5 1 "Hello world" live=false'])
  })

  it("logs errors by default", async () => {
    const {chat} = recordingChat()
    const error = new Error("send failed")
    chat.apiSendMessages = jest.fn(chat.apiSendMessages).mockRejectedValueOnce(error)
    const log = jest.spyOn(console, "log").mockImplementation(() => {})
    await new api.LiveMessage(chat, T.ChatType.Group, 5).end("Hello")
    expect(log).toHaveBeenCalledWith("live message error", error)
  })
})

describe("ChatApi.init", () => {
  it("loads the library for the configured backend before opening the database", async () => {
    let loaded!: () => void
    const load = jest.spyOn(core, "loadLibrary").mockReturnValue(new Promise(resolve => { loaded = resolve }))
    const migrate = jest.spyOn(core, "chatMigrateInit").mockResolvedValue(BigInt(1))
    const init = api.ChatApi.init({type: "postgres", connectionString: "postgres://unused"})
    await new Promise(setImmediate)
    expect(load).toHaveBeenCalledWith("postgres")
    expect(migrate).not.toHaveBeenCalled()
    loaded()
    await init
    expect(migrate).toHaveBeenCalled()
  })

  it("rejects an invalid config before loading the library", async () => {
    const load = jest.spyOn(core, "loadLibrary").mockResolvedValue()
    await expect(api.ChatApi.init({type: "mysql"} as unknown as api.DbConfig)).rejects.toThrow('Invalid DbConfig: {"type":"mysql"}')
    expect(load).not.toHaveBeenCalled()
  })
})
