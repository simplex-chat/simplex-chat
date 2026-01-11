import * as path from "path"
import * as fs from "fs"
import {CEvt, T} from "@simplex-chat/types"
import {api} from "../src/index"

const CT = T.ChatType

describe("API tests (use preset servers)", () => {
  const tmpDir = "./tests/tmp"
  const alicePath = path.join(tmpDir, "alice")
  const bobPath = path.join(tmpDir, "bob")

  beforeEach(() => fs.mkdirSync(tmpDir, {recursive: true}))
  afterEach(() => fs.rmSync(tmpDir, {recursive: true, force: true}))

  it("should send/receive message", async () => {
    // create users and start chat controllers
    const alice = await api.ChatApi.init(alicePath)
    const bob = await api.ChatApi.init(bobPath)
    const servers: string[] = []
    let eventCount = 0
    alice.on("hostConnected" as CEvt.Tag, async ({transportHost}: any) => { servers.push(transportHost) })
    alice.onAny(async () => { eventCount++ })
    const aliceUser = await alice.apiCreateActiveUser({displayName: "alice", fullName: ""})
    await bob.apiCreateActiveUser({displayName: "bob", fullName: ""})
    await alice.startChat()
    await bob.startChat()
    // connect via link
    const link = await alice.apiCreateLink(aliceUser.userId)
    await bob.apiConnectActiveUser(link)
    const bobContact = (await alice.wait("contactConnected")).contact
    expect(bobContact).toMatchObject({profile: {displayName: "bob"}})
    const aliceContact = (await bob.wait("contactConnected")).contact
    expect(aliceContact).toMatchObject({profile: {displayName: "alice"}})
    // exchange messages
    const isMessage = ({contactId}: T.Contact, msg: string) => (evt: CEvt.NewChatItems) =>
      evt.chatItems.some(ci => ci.chatInfo.type === CT.Direct && ci.chatInfo.contact.contactId === contactId && ci.chatItem.meta.itemText === msg)
    await alice.apiSendTextMessage(CT.Direct, bobContact.contactId, "hello")
    await bob.wait("newChatItems", isMessage(aliceContact, "hello"))
    await bob.apiSendTextMessage(CT.Direct, bobContact.contactId, "hello too")
    await alice.wait("newChatItems", isMessage(bobContact, "hello too"), 10000)
    await alice.apiSendTextMessage(CT.Direct, bobContact.contactId, "how are you?")
    await bob.wait("newChatItems", isMessage(aliceContact, "how are you?"))
    await bob.apiSendTextMessage(CT.Direct, bobContact.contactId, "ok, and you?")
    await alice.wait("newChatItems", isMessage(bobContact, "ok, and you?"), 10000)
    // no more messages
    await expect(alice.wait("newChatItems", 500)).resolves.toBeUndefined()
    await expect(bob.wait("newChatItems", 500)).resolves.toBeUndefined()
    // delete contacts, stop chat controllers and close databases
    await alice.apiDeleteChat(CT.Direct, bobContact.contactId)
    await bob.wait("contactDeletedByContact")
    await bob.apiDeleteChat(CT.Direct, aliceContact.contactId)
    await alice.stopChat()
    await bob.stopChat()
    await alice.close()
    await bob.close()
    await expect(alice.startChat).rejects.toThrow()
    await expect(bob.startChat).rejects.toThrow()
    expect(servers.length).toBe(2)
    expect(servers[0] !== servers[1]).toBe(true)
    expect(eventCount > 0).toBe(true)
  }, 30000)
})
