import * as path from "path";
import * as fs from "fs";
import {T} from "@simplex-chat/types"
import {api} from "../src/index";

describe("Core tests", () => {
  const tmpDir = "./tests/tmp2";
  const alicePath = path.join(tmpDir, "alice");
  const bobPath = path.join(tmpDir, "bob");

  beforeEach(() => fs.mkdirSync(tmpDir, {recursive: true}));
  afterEach(() => fs.rmSync(tmpDir, {recursive: true, force: true}));

  it("should send/receive message", async () => {
    const a = await api.ChatApi.init(alicePath)
    const b = await api.ChatApi.init(bobPath)
    const aliceUser = await a.apiCreateActiveUser({displayName: "alice", fullName: ""})
    await b.apiCreateActiveUser({displayName: "bob", fullName: ""})
    await a.startChat()
    await b.startChat()
    const link = await a.apiCreateLink(aliceUser.userId)
    await b.apiConnectActiveUser(link)
    const bobContact = (await a.wait("contactConnected")).contact
    expect(bobContact).toMatchObject({profile: {displayName: "bob"}})
    const aliceContact = (await b.wait("contactConnected")).contact
    expect(aliceContact).toMatchObject({profile: {displayName: "alice"}})
    await a.apiSendTextMessage(T.ChatType.Direct, bobContact.contactId, "hello")
    await b.wait("newChatItems", ({chatItems}) =>
      chatItems.some(({chatItem}) => chatItem.meta.itemText === "hello"))
    await b.apiSendTextMessage(T.ChatType.Direct, bobContact.contactId, "hello too")
    await a.wait("newChatItems", ({chatItems}) =>
      chatItems.some(({chatItem}) => chatItem.meta.itemText === "hello too"))
    // await a.stopChat()
    // await b.stopChat()
    await new Promise(resolve => setTimeout(resolve, 2000))
    await a.close()
    await b.close()
  }, 10000)
})
