import * as path from "path"
import * as fs from "fs"
import {CEvt, T} from "@simplex-chat/types"
import {api} from ".."

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
    await expect(alice.apiGetActiveUser()).resolves.toBeUndefined()
    const aliceUser = await alice.apiCreateActiveUser({displayName: "alice", fullName: ""})
    await expect(alice.apiGetActiveUser()).resolves.toMatchObject(aliceUser)
    await bob.apiCreateActiveUser({displayName: "bob", fullName: ""})
    await alice.startChat()
    await bob.startChat()
    // connect via link
    const link = await alice.apiCreateLink(aliceUser.userId)
    await expect(bob.apiConnectActiveUser(link)).resolves.toBe(api.ConnReqType.Invitation)
    const [bobContact, aliceContact] = await Promise.all([
      (await alice.wait("contactConnected")).contact,
      (await bob.wait("contactConnected")).contact
    ])
    expect(bobContact).toMatchObject({profile: {displayName: "bob"}})
    expect(aliceContact).toMatchObject({profile: {displayName: "alice"}})
    // exchange messages
    const isMessage = ({contactId}: T.Contact, msg: string) => (evt: CEvt.NewChatItems) =>
      evt.chatItems.some(ci => ci.chatInfo.type === CT.Direct && ci.chatInfo.contact.contactId === contactId && ci.chatItem.meta.itemText === msg)
    await alice.apiSendTextMessage([CT.Direct, bobContact.contactId], "hello")
    await bob.wait("newChatItems", isMessage(aliceContact, "hello"))
    await bob.apiSendTextMessage([CT.Direct, aliceContact.contactId], "hello too")
    await alice.wait("newChatItems", isMessage(bobContact, "hello too"), 10000)
    await alice.apiSendTextMessage([CT.Direct, bobContact.contactId], "how are you?")
    await bob.wait("newChatItems", isMessage(aliceContact, "how are you?"))
    await bob.apiSendTextMessage([CT.Direct, aliceContact.contactId], "ok, and you?")
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

  it("should create member contact and send invitation", async () => {
    // create 3 users and start chat controllers
    const alice = await api.ChatApi.init(alicePath)
    const bob = await api.ChatApi.init(bobPath)
    const carolPath = path.join(tmpDir, "carol")
    const carol = await api.ChatApi.init(carolPath)
    const aliceUser = await alice.apiCreateActiveUser({displayName: "alice", fullName: ""})
    await bob.apiCreateActiveUser({displayName: "bob", fullName: ""})
    await carol.apiCreateActiveUser({displayName: "carol", fullName: ""})
    await alice.startChat()
    await bob.startChat()
    await carol.startChat()
    // connect alice <-> bob
    const aliceLink1 = await alice.apiCreateLink(aliceUser.userId)
    await expect(bob.apiConnectActiveUser(aliceLink1)).resolves.toBe(api.ConnReqType.Invitation)
    const [bobContact] = await Promise.all([
      (await alice.wait("contactConnected")).contact,
      (await bob.wait("contactConnected")).contact
    ])
    // connect alice <-> carol
    const aliceLink2 = await alice.apiCreateLink(aliceUser.userId)
    await expect(carol.apiConnectActiveUser(aliceLink2)).resolves.toBe(api.ConnReqType.Invitation)
    const [carolContact] = await Promise.all([
      (await alice.wait("contactConnected")).contact,
      (await carol.wait("contactConnected")).contact
    ])
    // create group with direct messages enabled
    const group = await alice.apiNewGroup(aliceUser.userId, {
      displayName: "test-group",
      fullName: "",
      groupPreferences: {
        directMessages: {enable: T.GroupFeatureEnabled.On},
      },
    })
    const groupId = group.groupId
    // add bob to the group
    const bobInvP = bob.wait("receivedGroupInvitation", 15000)
    await alice.apiAddMember(groupId, bobContact.contactId, T.GroupMemberRole.Member)
    const bobInvEvt = await bobInvP
    expect(bobInvEvt).toBeDefined()
    const aliceBobConnP = alice.wait("connectedToGroupMember", 15000)
    const bobAliceConnP = bob.wait("connectedToGroupMember", 15000)
    await bob.apiJoinGroup(bobInvEvt!.groupInfo.groupId)
    await Promise.all([aliceBobConnP, bobAliceConnP])
    // add carol to the group
    const carolInvP = carol.wait("receivedGroupInvitation", 30000)
    await alice.apiAddMember(groupId, carolContact.contactId, T.GroupMemberRole.Member)
    const carolInvEvt = await carolInvP
    expect(carolInvEvt).toBeDefined()
    // wait for carol to connect to both alice and bob (and vice versa)
    const bobCarolConnP = bob.wait("connectedToGroupMember",
      (evt: CEvt.ConnectedToGroupMember) => evt.member.memberProfile.displayName === "carol", 30000)
    const carolAliceConnP = carol.wait("connectedToGroupMember",
      (evt: CEvt.ConnectedToGroupMember) => evt.member.memberProfile.displayName === "alice", 30000)
    const carolBobConnP = carol.wait("connectedToGroupMember",
      (evt: CEvt.ConnectedToGroupMember) => evt.member.memberProfile.displayName === "bob", 30000)
    const aliceCarolConnP = alice.wait("connectedToGroupMember",
      (evt: CEvt.ConnectedToGroupMember) => evt.member.memberProfile.displayName === "carol", 30000)
    await carol.apiJoinGroup(carolInvEvt!.groupInfo.groupId)
    await Promise.all([bobCarolConnP, carolAliceConnP, carolBobConnP, aliceCarolConnP])
    // find carol's memberId from bob's perspective
    const members = await bob.apiListMembers(groupId)
    const carolMember = members.find(m => m.memberProfile.displayName === "carol")
    expect(carolMember).toBeDefined()
    // test apiCreateMemberContact
    const dmContact = await bob.apiCreateMemberContact(groupId, carolMember!.groupMemberId)
    expect(dmContact).toBeDefined()
    expect(dmContact.contactId).toBeDefined()
    // test apiSendMemberContactInvitation
    const carolDmP = carol.wait("newMemberContactReceivedInv" as CEvt.Tag, 30000)
    const invContact = await bob.apiSendMemberContactInvitation(dmContact.contactId, "hello from bob")
    expect(invContact).toBeDefined()
    // carol should receive the member contact invitation
    const carolDmEvt = await carolDmP
    expect(carolDmEvt).toBeDefined()
    expect((carolDmEvt as any).contact).toBeDefined()
    // cleanup
    await alice.stopChat()
    await bob.stopChat()
    await carol.stopChat()
    await alice.close()
    await bob.close()
    await carol.close()
  }, 90000)
})
