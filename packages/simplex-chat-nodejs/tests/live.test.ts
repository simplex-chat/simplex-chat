import * as path from "path"
import * as fs from "fs"
import {T} from "@simplex-chat/types"
import {api} from ".."
import {SmpServer, describeIfSmpServerAvailable, startSmpServer} from "./smpServer"

const CT = T.ChatType

describeIfSmpServerAvailable("Live message tests (use local SMP server)", () => {
  const tmpDir = "./tests/tmp"
  const alicePath = path.join(tmpDir, "alice")
  const bobPath = path.join(tmpDir, "bob")
  let smpServer: SmpServer

  beforeAll(async () => { smpServer = await startSmpServer() }, 120000)
  afterAll(() => smpServer?.stop())
  beforeEach(() => fs.mkdirSync(tmpDir, {recursive: true}))
  afterEach(() => fs.rmSync(tmpDir, {recursive: true, force: true}))

  async function useSmpServer(chat: api.ChatApi): Promise<void> {
    expect(await chat.sendChatCmd(`/smp ${smpServer.address}`)).toMatchObject({type: "cmdOk"})
  }

  it("should show, update, finalize and delete live messages", async () => {
    const alice = await api.ChatApi.init({type: "sqlite", filePrefix: alicePath})
    const bob = await api.ChatApi.init({type: "sqlite", filePrefix: bobPath})
    const aliceUser = await alice.apiCreateActiveUser({displayName: "alice", fullName: ""})
    await bob.apiCreateActiveUser({displayName: "bob", fullName: ""})
    await alice.startChat()
    await bob.startChat()
    await useSmpServer(alice)
    await useSmpServer(bob)
    const aliceConnected = alice.wait("contactConnected", 30000)
    const bobConnected = bob.wait("contactConnected", 30000)
    await bob.apiConnectActiveUser(await alice.apiCreateLink(aliceUser.userId))
    const bobContact = (await aliceConnected)!.contact
    expect(await bobConnected).toBeDefined()

    const timeout = 10000
    const received = (check: (meta: T.CIMeta) => boolean) =>
      bob.wait("newChatItems", ({chatItems}) => chatItems.some(({chatItem: {meta}}) => check(meta)), timeout)
    const updated = (check: (meta: T.CIMeta) => boolean) =>
      bob.wait("chatItemUpdated", ({chatItem: {chatItem: {meta}}}) => check(meta), timeout)
    const live = new api.LiveMessage(alice, CT.Direct, bobContact.contactId, 500)

    const started = received(meta => meta.itemLive === true && meta.itemText === "")
    live.update("")
    const itemId = (await started)!.chatItems[0].chatItem.meta.itemId
    const updatedLive = updated(meta => meta.itemId === itemId && meta.itemLive === true && meta.itemText === "Hello")
    live.update("Hel")
    live.update("Hello")
    expect(await updatedLive).toBeDefined()
    const finalized = updated(meta => meta.itemId === itemId && meta.itemLive === false && meta.itemText === "Hello world")
    await live.end("Hello world")
    expect((await finalized)?.chatItem.chatItem.meta.itemEdited).toBe(false)

    const emptyStarted = received(meta => meta.itemLive === true && meta.itemText === "")
    live.update("")
    const emptyItemId = (await emptyStarted)!.chatItems[0].chatItem.meta.itemId
    const deleted = bob.wait("chatItemsDeleted", ({chatItemDeletions}) =>
      chatItemDeletions.some(({deletedChatItem}) => deletedChatItem.chatItem.meta.itemId === emptyItemId), timeout)
    await live.end()
    expect(await deleted).toBeDefined()

    await alice.close()
    await bob.close()
  }, 90000)
})
