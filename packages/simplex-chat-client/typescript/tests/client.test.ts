import * as assert from "assert"
import {ChatClient} from "../src/index"
import {ConnReqType} from "../src/client"
import {ChatEvent, CEvt, T} from "@simplex-chat/types"

// This test is currently failing - it gets as far as starting connection.
// It has to be written differently, with event loop to only process "interesting" events, as some events may arrive in different order.
describe.skip("ChatClient (expects SimpleX Chat server with a user, without contacts, on localhost:5225)", () => {
  test("connect, send message to themselves, delete contact", async () => {
    const c = await ChatClient.create("ws://localhost:5225")
    assert.strictEqual((await c.msgQ.dequeue()).type, "contactSubSummary")
    assert.strictEqual((await c.msgQ.dequeue()).type, "userContactSubSummary")
    assert.strictEqual((await c.msgQ.dequeue()).type, "terminalEvent")
    assert.strictEqual((await c.msgQ.dequeue()).type, "terminalEvent")
    const user = await c.apiGetActiveUser()
    assert.strictEqual(typeof user?.localDisplayName, "string")
    const connReq = await c.apiCreateLink(user!.userId)
    console.log("created link")
    assert.strictEqual(typeof connReq, "string")
    const connReqType = await c.apiConnectActiveUser(connReq)
    assert.strictEqual((await c.msgQ.dequeue()).type, "contactConnecting")
    assert.strictEqual((await c.msgQ.dequeue()).type, "contactConnected")
    assert(connReqType === ConnReqType.Invitation || connReqType === ConnReqType.Contact)
    const r1 = await c.msgQ.dequeue()
    const r2 = await c.msgQ.dequeue()
    assert.strictEqual(r1.type, "contactConnecting")
    assert.strictEqual(r2.type, "contactConnected")
    const contact1 = (r1 as CEvt.ContactConnected).contact
    // const contact2 = (r2 as C.CRContactConnected).contact
    const r3 = await c.apiSendTextMessage(T.ChatType.Direct, contact1.contactId, "hello")
    assert(r3[0].chatItem.content.type === "sndMsgContent" && r3[0].chatItem.content.msgContent.text === "hello")
    const r4 = await c.msgQ.dequeue()
    assert(isItemSent(r4) || isNewRcvItem(r4))
    await c.disconnect()

    function isItemSent(r: ChatEvent): boolean {
      return r.type === "chatItemsStatusesUpdated" && r.chatItems[0].chatItem.meta.itemStatus.type === "sndSent"
    }

    function isNewRcvItem(r: ChatEvent): boolean {
      return (
        r.type === "newChatItems" &&
        r.chatItems[0].chatItem.content.type === "rcvMsgContent" &&
        r.chatItems[0].chatItem.content.msgContent.text === "hello"
      )
    }
  }, 20000)
})
