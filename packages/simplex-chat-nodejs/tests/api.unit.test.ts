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

function sentCommands(): string[] {
  return jest.mocked(core.chatSendCmd).mock.calls.map(([, cmd]) => cmd)
}

function nextResponse(response: object): void {
  jest.mocked(core.chatSendCmd).mockResolvedValue(response as ChatResponse)
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
    expect(sentCommands()).toEqual([
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
    expect(sentCommands()).toEqual(["/_set domain 1 calc.simplex", "/_set domain 1"])
  })

  it("apiReceiveFile reports a file cancelled by sender", async () => {
    const chat = await chatWithResponse({type: "rcvFileAcceptedSndCancelled", user, rcvFileTransfer: {}})
    await expect(chat.apiReceiveFile(3)).rejects.toThrow("file cancelled by sender")
  })

  it("apiReceiveFile receives to the file path", async () => {
    const chatItem = {chatItem: {meta: {itemId: 2}}}
    const chat = await chatWithResponse({type: "rcvFileAccepted", user, chatItem})
    await expect(chat.apiReceiveFile(3, "./f.txt")).resolves.toEqual(chatItem)
    expect(sentCommands()).toEqual(["/freceive 3 approved_relays=on ./f.txt"])
  })

  it("apiGetChat accepts a count or a pagination", async () => {
    const aChat = {chatInfo: {type: "direct"}, chatItems: [], chatStats: {}}
    const chat = await chatWithResponse({type: "apiChat", user, chat: aChat})
    await expect(chat.apiGetChat(T.ChatType.Group, 1, 0)).resolves.toEqual(aChat)
    await expect(chat.apiGetChat(T.ChatType.Direct, 2, {type: "around", chatItemId: 5, count: 0})).resolves.toEqual(aChat)
    expect(sentCommands()).toEqual(["/_get chat #1 count=0", "/_get chat @2 around=5 count=0"])
  })

  it("apiChatItemsRead returns the chat info", async () => {
    const chatInfo = {type: "direct", contact: {contactId: 2}}
    const chat = await chatWithResponse({type: "itemsReadForChat", user, chatInfo})
    await expect(chat.apiChatItemsRead(T.ChatType.Direct, 2, [3, 4])).resolves.toEqual(chatInfo)
  })

  it("apiContactInfo returns the response", async () => {
    const info = {type: "contactInfo", user, contact: {contactId: 2}, customUserProfile: {displayName: "random", fullName: ""}}
    const chat = await chatWithResponse(info)
    await expect(chat.apiContactInfo(2)).resolves.toEqual(info)
  })

  it("apiPrepareContact and apiPrepareGroup return the prepared chat", async () => {
    const aChat = {chatInfo: {type: "direct"}, chatItems: [], chatStats: {}}
    const chat = await chatWithResponse({type: "newPreparedChat", user, chat: aChat})
    const connLink = {connFullLink: "F", connShortLink: "S"}
    await expect(chat.apiPrepareContact(1, connLink, {profile: {displayName: "bob", fullName: ""}, business: false}, "biz.simplex")).resolves.toEqual(aChat)
    await expect(chat.apiPrepareGroup(1, connLink, false, {groupProfile: {displayName: "team", fullName: ""}})).resolves.toEqual(aChat)
  })

  it("apiConnectPreparedContact returns the started connection or the existing contact", async () => {
    const started = {type: "startedConnectionToContact", user, contact: {contactId: 2}}
    const chat = await chatWithResponse(started)
    await expect(chat.apiConnectPreparedContact(2, false)).resolves.toEqual(started)
    const existing = {type: "contactAlreadyExists", user, contact: {contactId: 2}}
    nextResponse(existing)
    await expect(chat.apiConnectPreparedContact(2, true, {type: "text", text: "hi"})).resolves.toEqual(existing)
  })

  it("apiConnectPreparedGroup and apiConnectContactViaAddress return the response", async () => {
    const started = {type: "startedConnectionToGroup", user, groupInfo: {groupId: 3}, relayResults: []}
    const chat = await chatWithResponse(started)
    await expect(chat.apiConnectPreparedGroup(3, false)).resolves.toEqual(started)
    const sent = {type: "sentInvitationToContact", user, contact: {contactId: 2}}
    nextResponse(sent)
    await expect(chat.apiConnectContactViaAddress(1, false, 2)).resolves.toEqual(sent)
  })

  it("apiConnectPreparedLink returns the response, apiConnect the link type", async () => {
    const connLink = {connFullLink: "L"}
    const confirmation = {type: "sentConfirmation", user, connection: {pccConnId: 1}}
    const chat = await chatWithResponse(confirmation)
    await expect(chat.apiConnectPreparedLink(1, false, connLink)).resolves.toEqual(confirmation)
    await expect(chat.apiConnect(1, false, connLink)).resolves.toBe(api.ConnReqType.Invitation)
    const invitation = {type: "sentInvitation", user, connection: {pccConnId: 2}}
    nextResponse(invitation)
    await expect(chat.apiConnectPreparedLink(1, false, connLink)).resolves.toEqual(invitation)
    await expect(chat.apiConnect(1, false, connLink)).resolves.toBe(api.ConnReqType.Contact)
    const existing = {type: "contactAlreadyExists", user, contact: {contactId: 2}}
    nextResponse(existing)
    await expect(chat.apiConnectPreparedLink(1, false, connLink)).resolves.toEqual(existing)
    await expect(chat.apiConnect(1, false, connLink)).rejects.toThrow("contact already exists")
  })

  it("apiConnectPlan returns the SimpleX names", async () => {
    const connectionPlan = {type: "contactAddress", contactAddressPlan: {type: "ok"}}
    const connLink = {connFullLink: "L"}
    const planSimplexName = {nameType: "contact", nameDomain: {nameTLD: "simplex", domain: "biz", subDomain: []}}
    const otherSimplexName = {nameType: "publicGroup", nameDomain: {nameTLD: "simplex", domain: "news", subDomain: []}}
    const chat = await chatWithResponse({type: "connectionPlan", user, connLink, planSimplexName, otherSimplexName, connectionPlan})
    await expect(chat.apiConnectPlan(1, "@biz.simplex")).resolves.toEqual([connectionPlan, connLink, planSimplexName, otherSimplexName])
  })

  it("apiGetNetworkConfig returns the config, apiSetNetworkConfig accepts cmdOk", async () => {
    const networkConfig = {socksMode: T.SocksMode.Always, logTLSErrors: false} as T.NetworkConfig
    const chat = await chatWithResponse({type: "networkConfig", networkConfig})
    await expect(chat.apiGetNetworkConfig()).resolves.toEqual(networkConfig)
    nextResponse({type: "cmdOk"})
    await expect(chat.apiSetNetworkConfig(networkConfig)).resolves.toBeUndefined()
  })

  it("apiCreateMemberContact returns the contact, apiSendMemberContactInvitation sends a text message as JSON", async () => {
    const contact = {contactId: 2}
    const chat = await chatWithResponse({type: "newMemberContact", user, contact, groupInfo: {}, member: {}})
    await expect(chat.apiCreateMemberContact(1, 2)).resolves.toEqual(contact)
    nextResponse({type: "newMemberContactSentInv", user, contact, groupInfo: {}, member: {}})
    await expect(chat.apiSendMemberContactInvitation(2, "hi")).resolves.toEqual(contact)
    expect(sentCommands()).toEqual(["/_create member contact #1 2", '/_invite member contact @2 json {"type":"text","text":"hi"}'])
  })

  it("apiGetChatItemInfo returns the response", async () => {
    const info = {type: "chatItemInfo", user, chatItem: {chatItem: {meta: {itemId: 5}}}, chatItemInfo: {itemVersions: [], fileXftpServers: ["xftp://abc@localhost:7002"]}}
    const chat = await chatWithResponse(info)
    await expect(chat.apiGetChatItemInfo(T.ChatType.Direct, 2, 5)).resolves.toEqual(info)
    expect(sentCommands()).toEqual(["/_get item info @2 5"])
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
