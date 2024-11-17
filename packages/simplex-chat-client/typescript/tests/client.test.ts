import * as assert from "assert"
import {ChatClient} from "../src/index"
import {ConnReqType} from "../src/client"
import * as CC from "../src/command"
import * as CR from "../src/response"
import { GroupMemberStatus } from "../src/response"

describe("ChatClient (expects SimpleX Chat server with a user, without contacts, on localhost:5225)", () => {
  test("connect, send message to themselves, delete contact", async () => {
    const c = await ChatClient.create("ws://127.0.0.1:5225")
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

  test("connect, create a new group successfully", async () => {
    const c = await ChatClient.create("ws://127.0.0.1:5225")
    const user = await c.apiGetActiveUser()
    assert.notEqual(user, null)

    const newGroupProfile = {
      displayName: "test1234",
      fullName: "test1234",
    }

    if (user == null) throw new Error("Cannot reach")
    const newGroup = await c.apiNewGroup(user.userId, newGroupProfile)
    assert.strictEqual(typeof newGroup.groupId, "number")
    assert.strictEqual(newGroup.groupProfile.displayName, newGroupProfile.displayName)

    // reset
    await c.apiLeaveGroup(newGroup.groupId)
    await c.disconnect()
  }, 20000)

  test("list groups successfully", async () => {
    const c = await ChatClient.create("ws://127.0.0.1:5225")
    const user = await c.apiGetActiveUser()
    assert.notEqual(user, null)

    if (user == null) throw new Error("Cannot reach")

      // debug 
      // const _debug = await c.apiListGroups(user.userId)
      // console.dir(_debug)
    const newGroupProfile = {
      displayName: "list_group",
      fullName: "list_group",
    }

    const searchGroupProfile = {
      displayName: "search_group",
      fullName: "search_group",
    }

    const newGroup = await c.apiNewGroup(user.userId, newGroupProfile)
    const searchGroup = await c.apiNewGroup(user.userId, searchGroupProfile)

    // fetch all groups
    const allGroups = (await c.apiListGroups(user.userId, user.userContactId)).filter(v => v.membership.memberStatus !== GroupMemberStatus.Left)
    assert.equal(allGroups.length, 2)

    const searchGroups = (await c.apiListGroups(user.userId, user.userContactId, "search_group")).filter(v => v.membership.memberStatus !== GroupMemberStatus.Left)
    assert.equal(searchGroups.length, 1)
    assert.equal(searchGroups[0].groupProfile.displayName, searchGroupProfile.displayName)
    assert.equal(searchGroups[0].groupId, searchGroup.groupId)

    // reset
    await c.apiLeaveGroup(newGroup.groupId)
    await c.apiLeaveGroup(searchGroup.groupId)
    await c.disconnect()
  }, 20000)
})
