import * as path from "path"
import * as fs from "fs"
import * as assert from "assert"
import {CEvt, T} from "@simplex-chat/types"
import {api, bot, util} from ".."

const CT = T.ChatType

describe("Bot tests (use preset servers)", () => {
  const tmpDir = "./tests/tmp"
  const botPath = path.join(tmpDir, "bot")
  const alicePath = path.join(tmpDir, "alice")

  beforeEach(() => fs.mkdirSync(tmpDir, {recursive: true}))
  afterEach(() => fs.rmSync(tmpDir, {recursive: true, force: true}))

  it("should reply to messages", async () => {
    // run bot
    const [chat, botUser, botAddress] = await bot.run({
      profile: {displayName: "Squaring bot", fullName: ""},
      dbOpts: {dbFilePrefix: botPath, dbKey: ""},
      options: {
        addressSettings: {welcomeMessage: "If you send me a number, I will calculate its square."},
      },
      onMessage: async (ci, content) => {
        const n = +content.text
        const reply = typeof n === "number" && !isNaN(n) ? `${n} * ${n} = ${n * n}` : `this is not a number`
        await chat.apiSendTextReply(ci, reply)
      }
    })
    assert(typeof botAddress === "object")
    // create user
    const alice = await api.ChatApi.init(alicePath)
    const aliceUser = await alice.apiCreateActiveUser({displayName: "alice", fullName: ""})
    await alice.startChat()
    // connect to bot
    const [plan, link] = await alice.apiConnectPlan(aliceUser.userId, util.contactAddressStr(botAddress.connLinkContact))
    assert(plan.type === "contactAddress")
    await expect(alice.apiConnect(aliceUser.userId, false, link)).resolves.toBe(api.ConnReqType.Contact)
    const [botContact, aliceContact] = await Promise.all([
      (await alice.wait("contactConnected")).contact,
      (await chat.wait("contactConnected")).contact
    ])
    expect(botContact.profile.displayName).toBe("Squaring bot")
    // send message to bot
    const isMessage = ({contactId}: T.Contact, msg: string) => (evt: CEvt.NewChatItems) =>
      evt.chatItems.some(ci => ci.chatInfo.type === CT.Direct && ci.chatInfo.contact.contactId === contactId && ci.chatItem.meta.itemText === msg)
    await alice.apiSendTextMessage([CT.Direct, botContact.contactId], "2")
    console.log("after sending message")
    await alice.wait("newChatItems", isMessage(botContact, "2 * 2 = 4"), 5000)
    // cleanup
    await alice.apiDeleteChat(CT.Direct, botContact.contactId)
    await chat.wait("contactDeletedByContact", ({contact}) => contact.contactId === aliceContact.contactId)
    await chat.apiDeleteUserAddress(botUser.userId)
    await chat.stopChat()
    await chat.close()
    await alice.stopChat()
    await alice.close()
  }, 30000)
})
