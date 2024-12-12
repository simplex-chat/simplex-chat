import * as assert from "assert"
import {ChatClient} from "../src/index"
import {ConnReqType} from "../src/client"
import * as CC from "../src/command"
import * as CR from "../src/response"

describe.skip("ChatClient (expects SimpleX Chat server with a user, without contacts, on localhost:5225)", () => {
  test("connect, send message to themselves, delete contact", async () => {
    const c = await ChatClient.create("ws://localhost:5225")
    assert.strictEqual((await c.msgQ.dequeue()).type, "contactSubSummary")
    assert.strictEqual((await c.msgQ.dequeue()).type, "memberSubErrors")
    assert.strictEqual((await c.msgQ.dequeue()).type, "pendingSubSummary")
    const user = await c.apiGetActiveUser()
    assert.strictEqual(typeof user?.localDisplayName, "string")
    const connReq = await c.apiCreateLink()
    assert.strictEqual(typeof connReq, "string")
    assert.strictEqual((await c.msgQ.dequeue()).type, "newContactConnection") // TODO add to response types
    const connReqType = await c.apiConnect(connReq)
    assert.strictEqual((await c.msgQ.dequeue()).type, "newContactConnection") // TODO add to response types
    assert((await c.msgQ.dequeue()).type === "contactConnecting")
    assert((await c.msgQ.dequeue()).type === "contactConnecting")
    assert(connReqType === ConnReqType.Invitation || connReqType === ConnReqType.Contact)
    const r1 = await c.msgQ.dequeue()
    const r2 = await c.msgQ.dequeue()
    assert(r1.type === "contactConnected")
    assert(r2.type === "contactConnected")
    const contact1 = (r1 as CR.CRContactConnected).contact
    // const contact2 = (r2 as C.CRContactConnected).contact
    const r3 = await c.apiSendTextMessage(CC.ChatType.Direct, contact1.contactId, "hello")
    assert(r3[0].chatItem.content.type === "sndMsgContent" && r3[0].chatItem.content.msgContent.text === "hello")
    const r4 = await c.msgQ.dequeue()
    assert(isItemSent(r4) || isNewRcvItem(r4))
    await c.disconnect()

    function isItemSent(r: CR.ChatResponse): boolean {
      return r.type === "chatItemStatusUpdated" && r.chatItem.chatItem.meta.itemStatus.type === "sndSent"
    }

    function isNewRcvItem(r: CR.ChatResponse): boolean {
      return (
        r.type === "newChatItems" &&
        r.chatItems[0].chatItem.content.type === "rcvMsgContent" &&
        r.chatItems[0].chatItem.content.msgContent.text === "hello"
      )
    }
  }, 20000)
})
