import * as path from "path"
import * as fs from "fs"
import * as assert from "assert"
import {CEvt, T} from "@simplex-chat/types"
import {api, bot, util} from ".."
import {SmpServer, describeIfSmpServerAvailable, startSmpServer} from "./smpServer"

const CT = T.ChatType

describeIfSmpServerAvailable("Bot tests (use local SMP server)", () => {
  const tmpDir = "./tests/tmp"
  const botPath = path.join(tmpDir, "bot")
  const alicePath = path.join(tmpDir, "alice")
  let smpServer: SmpServer

  beforeAll(async () => { smpServer = await startSmpServer() }, 120000)
  afterAll(() => smpServer?.stop())
  beforeEach(() => fs.mkdirSync(tmpDir, {recursive: true}))
  afterEach(() => fs.rmSync(tmpDir, {recursive: true, force: true}))

  async function useSmpServer(chat: api.ChatApi): Promise<void> {
    expect(await chat.sendChatCmd(`/smp ${smpServer.address}`)).toMatchObject({type: "cmdOk"})
  }

  async function prepareBotDatabase(): Promise<void> {
    const chat = await api.ChatApi.init({type: "sqlite", filePrefix: botPath})
    await chat.apiCreateActiveUser({displayName: "Squaring bot", fullName: ""})
    await chat.startChat()
    await useSmpServer(chat)
    await chat.close()
  }

  it("should reply to messages", async () => {
    await prepareBotDatabase()
    // run bot
    const [chat, botUser, botAddress] = await bot.run({
      profile: {displayName: "Squaring bot", fullName: ""},
      dbOpts: {type: "sqlite", filePrefix: botPath},
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
    const alice = await api.ChatApi.init({type: "sqlite", filePrefix: alicePath})
    const aliceUser = await alice.apiCreateActiveUser({displayName: "alice", fullName: ""})
    await alice.startChat()
    await useSmpServer(alice)
    // connect to bot
    const [plan, link] = await alice.apiConnectPlan(aliceUser.userId, util.contactAddressStr(botAddress.connLinkContact))
    assert(plan.type === "contactAddress")
    const connected = Promise.all([alice.wait("contactConnected"), chat.wait("contactConnected")])
    await expect(alice.apiConnect(aliceUser.userId, false, link)).resolves.toBe(api.ConnReqType.Contact)
    const [{contact: botContact}, {contact: aliceContact}] = await connected
    expect(botContact.profile.displayName).toBe("Squaring bot")
    // send message to bot
    const isMessage = ({contactId}: T.Contact, msg: string) => (evt: CEvt.NewChatItems) =>
      evt.chatItems.some(ci => ci.chatInfo.type === CT.Direct && ci.chatInfo.contact.contactId === contactId && ci.chatItem.meta.itemText === msg)
    const reply = alice.wait("newChatItems", isMessage(botContact, "2 * 2 = 4"), 5000)
    await alice.apiSendTextMessage([CT.Direct, botContact.contactId], "2")
    console.log("after sending message")
    await expect(reply).resolves.toBeDefined()
    // cleanup
    const deleted = chat.wait("contactDeletedByContact", ({contact}) => contact.contactId === aliceContact.contactId)
    await alice.apiDeleteChat(CT.Direct, botContact.contactId)
    await deleted
    await chat.apiDeleteUserAddress(botUser.userId)
    await chat.stopChat()
    await chat.close()
    await alice.stopChat()
    await alice.close()
  }, 30000)
})
