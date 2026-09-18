import {ChatResponse, T} from "@simplex-chat/types"
import * as api from "../src/api"
import * as core from "../src/core"

const user = {userId: 1} as T.User

async function chatWithResponse(response: object): Promise<api.ChatApi> {
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
