import * as path from "path"
import * as fs from "fs"
import {CEvt, T} from "@simplex-chat/types"
import {api} from ".."
import {SmpServer, describeIfSmpServerAvailable, startSmpServer} from "./smpServer"

const CT = T.ChatType

describeIfSmpServerAvailable("API tests (use local SMP server)", () => {
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

  it("should send/receive message", async () => {
    // create users and start chat controllers
    const alice = await api.ChatApi.init({type: "sqlite", filePrefix: alicePath})
    const bob = await api.ChatApi.init({type: "sqlite", filePrefix: bobPath})
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
    await useSmpServer(alice)
    await useSmpServer(bob)
    // connect via link
    const link = await alice.apiCreateLink(aliceUser.userId)
    const connected = Promise.all([alice.wait("contactConnected"), bob.wait("contactConnected")])
    await expect(bob.apiConnectActiveUser(link)).resolves.toBe(api.ConnReqType.Invitation)
    const [{contact: bobContact}, {contact: aliceContact}] = await connected
    expect(bobContact).toMatchObject({profile: {displayName: "bob"}})
    expect(aliceContact).toMatchObject({profile: {displayName: "alice"}})
    // exchange messages
    const isMessage = ({contactId}: T.Contact, msg: string) => (evt: CEvt.NewChatItems) =>
      evt.chatItems.some(ci => ci.chatInfo.type === CT.Direct && ci.chatInfo.contact.contactId === contactId && ci.chatItem.meta.itemText === msg)
    const hello = bob.wait("newChatItems", isMessage(aliceContact, "hello"))
    await alice.apiSendTextMessage([CT.Direct, bobContact.contactId], "hello")
    await hello
    const helloToo = alice.wait("newChatItems", isMessage(bobContact, "hello too"), 10000)
    await bob.apiSendTextMessage([CT.Direct, aliceContact.contactId], "hello too")
    await expect(helloToo).resolves.toBeDefined()
    const howAreYou = bob.wait("newChatItems", isMessage(aliceContact, "how are you?"))
    await alice.apiSendTextMessage([CT.Direct, bobContact.contactId], "how are you?")
    await howAreYou
    const okAndYou = alice.wait("newChatItems", isMessage(bobContact, "ok, and you?"), 10000)
    await bob.apiSendTextMessage([CT.Direct, aliceContact.contactId], "ok, and you?")
    await expect(okAndYou).resolves.toBeDefined()
    // no more messages
    await expect(alice.wait("newChatItems", 500)).resolves.toBeUndefined()
    await expect(bob.wait("newChatItems", 500)).resolves.toBeUndefined()
    // delete contacts, stop chat controllers and close databases
    const deleted = bob.wait("contactDeletedByContact")
    await alice.apiDeleteChat(CT.Direct, bobContact.contactId)
    await deleted
    await bob.apiDeleteChat(CT.Direct, aliceContact.contactId)
    await alice.stopChat()
    await bob.stopChat()
    await alice.close()
    await bob.close()
    await expect(alice.startChat()).rejects.toThrow("chat api controller not initialized")
    await expect(bob.startChat()).rejects.toThrow("chat api controller not initialized")
    expect([...new Set(servers)]).toEqual(["localhost"])
    expect(eventCount > 0).toBe(true)
  }, 30000)

  it("should create member contact and send invitation", async () => {
    // create 3 users and start chat controllers
    const alice = await api.ChatApi.init({type: "sqlite", filePrefix: alicePath})
    const bob = await api.ChatApi.init({type: "sqlite", filePrefix: bobPath})
    const carolPath = path.join(tmpDir, "carol")
    const carol = await api.ChatApi.init({type: "sqlite", filePrefix: carolPath})
    const aliceUser = await alice.apiCreateActiveUser({displayName: "alice", fullName: ""})
    await bob.apiCreateActiveUser({displayName: "bob", fullName: ""})
    await carol.apiCreateActiveUser({displayName: "carol", fullName: ""})
    await alice.startChat()
    await bob.startChat()
    await carol.startChat()
    await useSmpServer(alice)
    await useSmpServer(bob)
    await useSmpServer(carol)
    // connect alice <-> bob
    const aliceLink1 = await alice.apiCreateLink(aliceUser.userId)
    const bobConnected = Promise.all([alice.wait("contactConnected"), bob.wait("contactConnected")])
    await expect(bob.apiConnectActiveUser(aliceLink1)).resolves.toBe(api.ConnReqType.Invitation)
    const [{contact: bobContact}] = await bobConnected
    // connect alice <-> carol
    const aliceLink2 = await alice.apiCreateLink(aliceUser.userId)
    const carolConnected = Promise.all([alice.wait("contactConnected"), carol.wait("contactConnected")])
    await expect(carol.apiConnectActiveUser(aliceLink2)).resolves.toBe(api.ConnReqType.Invitation)
    const [{contact: carolContact}] = await carolConnected
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
    const aliceBobConnP = alice.wait("joinedGroupMember", 15000)
    const bobAliceConnP = bob.wait("userJoinedGroup", 15000)
    await bob.apiJoinGroup(bobInvEvt!.groupInfo.groupId)
    expect(await Promise.all([aliceBobConnP, bobAliceConnP])).not.toContain(undefined)
    // add carol to the group
    const carolInvP = carol.wait("receivedGroupInvitation", 30000)
    await alice.apiAddMember(groupId, carolContact.contactId, T.GroupMemberRole.Member)
    const carolInvEvt = await carolInvP
    expect(carolInvEvt).toBeDefined()
    // wait for carol to connect to both alice and bob (and vice versa)
    const bobCarolConnP = bob.wait("connectedToGroupMember",
      (evt: CEvt.ConnectedToGroupMember) => evt.member.memberProfile.displayName === "carol", 30000)
    const carolAliceConnP = carol.wait("userJoinedGroup", 30000)
    const carolBobConnP = carol.wait("connectedToGroupMember",
      (evt: CEvt.ConnectedToGroupMember) => evt.member.memberProfile.displayName === "bob", 30000)
    const aliceCarolConnP = alice.wait("joinedGroupMember",
      (evt: CEvt.JoinedGroupMember) => evt.member.memberProfile.displayName === "carol", 30000)
    await carol.apiJoinGroup(carolInvEvt!.groupInfo.groupId)
    expect(await Promise.all([bobCarolConnP, carolAliceConnP, carolBobConnP, aliceCarolConnP])).not.toContain(undefined)
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
