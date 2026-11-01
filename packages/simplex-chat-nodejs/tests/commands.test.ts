import {CC, T} from "@simplex-chat/types"

describe("APIConnect.cmdString", () => {
  const preparedLink_ = {connFullLink: "L"}

  it("renders incognito=on", () => {
    expect(CC.APIConnect.cmdString({userId: 1, incognito: true, preparedLink_})).toBe("/_connect 1 incognito=on L")
  })

  it("omits incognito when off", () => {
    expect(CC.APIConnect.cmdString({userId: 1, incognito: false, preparedLink_})).toBe("/_connect 1 L")
  })
})

describe("APIConnectPlan.cmdString", () => {
  const plan = {userId: 1, connectTarget: "L"}

  it("omits the default resolve mode", () => {
    expect(CC.APIConnectPlan.cmdString({...plan, resolveMode: T.PlanResolveMode.Unknown})).toBe("/_connect plan 1 L")
  })

  it("renders other resolve modes", () => {
    expect(CC.APIConnectPlan.cmdString({...plan, resolveMode: T.PlanResolveMode.AllGroups})).toBe("/_connect plan 1 L resolve=allGroups")
    expect(CC.APIConnectPlan.cmdString({...plan, resolveMode: T.PlanResolveMode.Never})).toBe("/_connect plan 1 L resolve=never")
  })

  it("renders link owner signature", () => {
    const linkOwnerSig = {chatBinding: "B", ownerSig: "S"}
    expect(CC.APIConnectPlan.cmdString({...plan, resolveMode: T.PlanResolveMode.Unknown, linkOwnerSig})).toBe(`/_connect plan 1 L sig=${JSON.stringify(linkOwnerSig)}`)
  })
})

describe("APIGetChat.cmdString", () => {
  const chatRef = {chatType: T.ChatType.Direct, chatId: 2}

  it("renders each pagination", () => {
    expect(CC.APIGetChat.cmdString({chatRef: {chatType: T.ChatType.Group, chatId: 1}, chatPagination: {type: "last", count: 0}})).toBe("/_get chat #1 count=0")
    expect(CC.APIGetChat.cmdString({chatRef, chatPagination: {type: "initial", count: 1}})).toBe("/_get chat @2 initial=1")
    expect(CC.APIGetChat.cmdString({chatRef, chatPagination: {type: "around", chatItemId: 5, count: 0}})).toBe("/_get chat @2 around=5 count=0")
    expect(CC.APIGetChat.cmdString({chatRef, chatPagination: {type: "after", chatItemId: 5, count: 100}})).toBe("/_get chat @2 after=5 count=100")
    expect(CC.APIGetChat.cmdString({chatRef, chatPagination: {type: "before", chatItemId: 5, count: 10}})).toBe("/_get chat @2 before=5 count=10")
  })

  it("renders content tag and search", () => {
    expect(CC.APIGetChat.cmdString({chatRef, contentTag: T.MsgContentTag.Image, chatPagination: {type: "last", count: 10}, search: "k"})).toBe("/_get chat @2 content=image count=10 search=k")
  })
})

describe("APIChatItemsRead.cmdString", () => {
  it("renders item IDs", () => {
    expect(CC.APIChatItemsRead.cmdString({chatRef: {chatType: T.ChatType.Direct, chatId: 2}, chatItemIds: [3, 4]})).toBe("/_read chat items @2 3,4")
  })
})

describe("APIPrepareContact.cmdString", () => {
  const connLink = {connFullLink: "F", connShortLink: "S"}
  const contactSLinkData = {profile: {displayName: "bob", fullName: ""}, business: false}

  it("renders the verified domain", () => {
    expect(CC.APIPrepareContact.cmdString({userId: 1, connLink, verifiedDomain: "biz.simplex", contactSLinkData})).toBe(`/_prepare contact 1 F S domain=biz.simplex ${JSON.stringify(contactSLinkData)}`)
  })

  it("omits the domain", () => {
    expect(CC.APIPrepareContact.cmdString({userId: 1, connLink, contactSLinkData})).toBe(`/_prepare contact 1 F S ${JSON.stringify(contactSLinkData)}`)
  })
})

describe("APIPrepareGroup.cmdString", () => {
  const connLinkContact = {connFullLink: "F", connShortLink: "S"}
  const groupSLinkData = {groupProfile: {displayName: "team", fullName: ""}}

  it("renders direct=off for a channel", () => {
    expect(CC.APIPrepareGroup.cmdString({userId: 1, connLinkContact, direct: false, verifiedDomain: "news.simplex", groupSLinkData})).toBe(`/_prepare group 1 F S direct=off domain=news.simplex ${JSON.stringify(groupSLinkData)}`)
  })

  it("omits direct for a group", () => {
    expect(CC.APIPrepareGroup.cmdString({userId: 1, connLinkContact, direct: true, groupSLinkData})).toBe(`/_prepare group 1 F S ${JSON.stringify(groupSLinkData)}`)
  })
})

describe("prepared chat connection cmdString", () => {
  const msgContent_: T.MsgContent = {type: "text", text: "hi"}

  it("renders APIConnectPreparedContact with a message", () => {
    expect(CC.APIConnectPreparedContact.cmdString({contactId: 2, incognito: true, msgContent_})).toBe('/_connect contact @2 incognito=on json {"type":"text","text":"hi"}')
  })

  it("renders APIConnectPreparedGroup with owner contact", () => {
    expect(CC.APIConnectPreparedGroup.cmdString({groupId: 3, incognito: false, ownerContact: {contactId: 1, memberId: "abc"}})).toBe("/_connect group #3 contact=1 owner=abc")
    expect(CC.APIConnectPreparedGroup.cmdString({groupId: 3, incognito: false, msgContent_})).toBe('/_connect group #3 json {"type":"text","text":"hi"}')
  })

  it("renders APIConnectContactViaAddress", () => {
    expect(CC.APIConnectContactViaAddress.cmdString({userId: 1, incognito: true, contactId: 2})).toBe("/_connect contact 1 incognito=on 2")
  })
})

describe("member contact cmdString", () => {
  it("renders APICreateMemberContact", () => {
    expect(CC.APICreateMemberContact.cmdString({groupId: 1, groupMemberId: 2})).toBe("/_create member contact #1 2")
  })

  it("renders APISendMemberContactInvitation", () => {
    expect(CC.APISendMemberContactInvitation.cmdString({contactId: 2, msgContent_: {type: "text", text: "hi"}})).toBe('/_invite member contact @2 json {"type":"text","text":"hi"}')
    expect(CC.APISendMemberContactInvitation.cmdString({contactId: 2})).toBe("/_invite member contact @2")
  })
})

describe("network config cmdString", () => {
  it("renders APIGetNetworkConfig and APISetNetworkConfig", () => {
    const networkConfig = {socksMode: T.SocksMode.Always, logTLSErrors: true} as T.NetworkConfig
    expect(CC.APIGetNetworkConfig.cmdString({})).toBe("/network")
    expect(CC.APISetNetworkConfig.cmdString({networkConfig})).toBe(`/_network ${JSON.stringify(networkConfig)}`)
  })
})

describe("APIGetChatItemInfo.cmdString", () => {
  it("renders chat reference and item ID", () => {
    expect(CC.APIGetChatItemInfo.cmdString({chatRef: {chatType: T.ChatType.Direct, chatId: 2}, chatItemId: 5})).toBe("/_get item info @2 5")
  })
})

describe("APIGetUserServers.cmdString", () => {
  it("renders user ID", () => {
    expect(CC.APIGetUserServers.cmdString({userId: 1})).toBe("/_servers 1")
  })
})
