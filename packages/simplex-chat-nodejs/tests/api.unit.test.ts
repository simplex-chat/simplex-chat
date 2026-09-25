import {ChatResponse, T} from "@simplex-chat/types"
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

  it("apiSetProfileAddress accepts userProfileNoChange", async () => {
    const chat = await chatWithResponse({type: "userProfileNoChange", user})
    await expect(chat.apiSetProfileAddress(1, true)).resolves.toEqual({updateSuccesses: 0, updateFailures: 0, changedContacts: []})
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

  it("receives with a 500 ms wait", async () => {
    const chat = await chatWithResponses()
    await chat.recvChatEvent()
    expect(core.chatRecvMsgWait).toHaveBeenCalledWith(BigInt(1), 500_000)
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
