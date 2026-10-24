# Bot API: chat, connection, member contact and network commands

Twelve commands were undocumented, and their strings were built by hand: in bots, and in the Node and Python libraries for `APIGetChat`, `APICreateMemberContact` and `APISendMemberContactInvitation`.
The commands are documented; in the core, six command constructors and `ChatPagination` are changed to record syntax, and `verifiedDomain` is typed as `StrJSON`.
TypeScript and Python types are generated, with `cmdString` and `<Name>_cmd_string` functions; each command of the Node library is sent through its `cmdString`, and the methods of the Python library (`api.py`) are changed in a following pull request.

References are to branch `ep/bot-api` at `d53829b0b`. Haskell paths without a directory are in `src/Simplex/Chat/`. `Library/` is `src/Simplex/Chat/Library/`. `API/` is `bots/src/API/`. `TS/` is `packages/simplex-chat-client/types/typescript/src/`. `node/` is `packages/simplex-chat-nodejs/`. `SMQ/` is simplexmq `src/Simplex/Messaging/` at `8bc4d3601` (`cabal.project:24`).

## Decisions

Documented commands; strings in each syntax are parsed by `chatCommandP` at the `parser` line of `Library/Commands.hs`:
```
category / command               network usage   parser   syntax
Group commands
  APICreateMemberContact         interactive     :6552    /_create member contact #<groupId> <groupMemberId>
  APISendMemberContactInvitation background      :6553    /_invite member contact @<contactId>[ json <json(msgContent_)>]
Connection commands
  APIPrepareContact              no              :6560    /_prepare contact <userId> <str(connLink)>[ domain=<verifiedDomain>] <json(contactSLinkData)>
  APIPrepareGroup                no              :6561    /_prepare group <userId> <str(connLinkContact)>[ direct=off][ domain=<verifiedDomain>] <json(groupSLinkData)>
  APIConnectPreparedContact      interactive     :6564    /_connect contact @<contactId>[ incognito=on][ json <json(msgContent_)>]
  APIConnectPreparedGroup        interactive     :6565    /_connect group #<groupId>[ incognito=on][ <str(ownerContact)>][ json <json(msgContent_)>]
  APIConnectContactViaAddress    interactive     :6607    /_connect contact <userId>[ incognito=on] <contactId>
Chat commands
  APIGetChat                     no              :6349    /_get chat <str(chatRef)>[ content=text|link|image|video|voice|file|report|chat] <str(chatPagination)>[ search=<search>]
  APIChatItemsRead               no              :6376    /_read chat items <str(chatRef)> <chatItemIds[0]>[,<chatItemIds[1]>...]
  APIContactInfo                 no              :6469    /_info @<contactId>
Chat management
  APIGetNetworkConfig            no              :6461    /network
  APISetNetworkConfig            background      :6459    /_network <json(networkConfig)>
```
Changed documented syntax:
- `APIConnectPlan`: `/_connect plan <userId> <connectTarget><str(resolveMode)>[ sig=<json(linkOwnerSig)>]` (parser `Library/Commands.hs:6559`).
- `PlanResolveMode`: ` resolve=allGroups` and ` resolve=never`; `unknown`, the parser default (`Library/Commands.hs:6559`), as an empty string.

Documented errors:
```
APICreateMemberContact           CEGroupMemberNotActive
                                 CEPeerChatVRangeIncompatible
APISendMemberContactInvitation   CEGroupMemberNotActive
```

Documented responses:
```
response                    command
CRApiChat                   APIGetChat
CRContactInfo               APIContactInfo
CRItemsReadForChat          APIChatItemsRead
CRNetworkConfig             APIGetNetworkConfig
CRNewMemberContact          APICreateMemberContact
CRNewMemberContactSentInv   APISendMemberContactInvitation
CRNewPreparedChat           APIPrepareContact
                            APIPrepareGroup
```

Documented types:
```
with syntax
  ChatPagination         count=<count>|after=<chatItemId> count=<count>|before=<chatItemId> count=<count>|around=<chatItemId> count=<count>|initial=<count>
  GroupOwnerContact      contact=<contactId> owner=<memberId>
records
  ConnectionStats
  KeepAliveOpts
  NavigationInfo
  NetworkConfig
  NetworkTimeout
  RcvQueueInfo
  SndQueueInfo
enums
  HostMode
  MsgContentTag          MCUnknown_ excluded
  QueueStatus
  RcvSwitchStatus
  SMPProxyFallback
  SMPProxyMode
  SMPWebPortServers
  SocksMode
  SndSwitchStatus
  TransportSessionMode
```
Types generated as their JSON encoding, not as their Haskell type:
```
type                 generated as        JSON
AChatInfo            ChatInfo            JSON of ChatInfo (Messages.hs:254-256)
SocksProxyWithAuth   string              strEncode (SMQ/Transport/Client.hs:265-274)
RcvSwitchStatus      strEncode values    strEncode (SMQ/Agent/Protocol.hs:617-623, :637-642)
SndSwitchStatus      strEncode values    strEncode (SMQ/Agent/Protocol.hs:651-656, :669-674)
```

Core:
- Documented commands have named fields (`tests/APIDocs.hs:84-88`): six constructors are changed to record syntax.
- `ChatPagination` constructors have fields `count` and `chatItemId`, the fields of the generated union.
- `verifiedDomain` of `APIPrepareContact` and `APIPrepareGroup` has type `Maybe (StrJSON "SimplexDomain" SimplexDomain)`, as `simplexDomain` of `APISetUserDomain` (`Controller.hs:455`); it is generated as a string.
- Accepted command strings, handler results and responses are unchanged.

Node library:
- Every command is sent as `CC.<Command>.cmdString(...)`.
- Result of a new method, by the first matching row:
```
accepted responses                          result
CR.CmdOk only                               void
one type, with one field other than user    that field
other                                       the response
```
- For any other response, a `ChatCommandError` is thrown.
```
new method                    command                       result
apiConnectPreparedLink        APIConnect                    CR.SentConfirmation | CR.SentInvitation | CR.ContactAlreadyExists
apiPrepareContact             APIPrepareContact             chat
apiPrepareGroup               APIPrepareGroup               chat
apiConnectPreparedContact     APIConnectPreparedContact     CR.StartedConnectionToContact | CR.ContactAlreadyExists
apiConnectPreparedGroup       APIConnectPreparedGroup       CR.StartedConnectionToGroup
apiConnectContactViaAddress   APIConnectContactViaAddress   CR.SentInvitationToContact
apiChatItemsRead              APIChatItemsRead              chatInfo
apiContactInfo                APIContactInfo                CR.ContactInfo
apiGetNetworkConfig           APIGetNetworkConfig           networkConfig
apiSetNetworkConfig           APISetNetworkConfig           void, on CR.CmdOk
```
```
changed method                   change
stopChat                         APIStopChat; string unchanged, /_stop
apiReceiveFile                   optional filePath, passed to ReceiveFile
apiConnectPlan                   result: [plan, link, planSimplexName?, otherSimplexName?]
apiGetChat                       third parameter: a count or T.ChatPagination; a count is converted to {type: "last", count}, sent as count=<count>; result: T.AChat
apiCreateMemberContact           APICreateMemberContact; signature unchanged
apiSendMemberContactInvitation   APISendMemberContactInvitation; a string message is sent as {type: "text", text}; JSDoc network usage: background
```
- `apiConnect` is unchanged: `ConnReqType` is returned; for `contactAlreadyExists`, a `ChatCommandError` "contact already exists" is thrown.
- Package versions: as on master. Before `simplex-chat` is released, `@simplex-chat/types` is published above 7.1.0-beta.6.2 with these commands, and the dependency at `node/package.json:27` is raised to that version.

Delivery, decided by the user on 2026-10-24:
- The pull request in simplex-chat/simplex-chat is opened after the fork pull request is approved by the user.
- MCP server, in a following pull request, after the release of `@simplex-chat/types` and `simplex-chat`: the strings of the commands whose Node library methods are added or changed here are replaced by these methods.
```
APIGetChat                       packages/simplex-chat-mcp/src/messages.ts:179
APIChatItemsRead                 packages/simplex-chat-mcp/src/messages.ts:183
APIConnectPlan                   packages/simplex-chat-mcp/src/tools.ts:471
prepare and connect commands     packages/simplex-chat-mcp/src/tools.ts:520-535
APIConnect                       packages/simplex-chat-mcp/src/tools.ts:539
APIContactInfo                   packages/simplex-chat-mcp/src/tools.ts:771
```
- Python library, in a following pull request: the strings built by hand are replaced by the generated `<Name>_cmd_string` functions; a string message is then sent as JSON, and `import json` (`api.py:6`) is removed.
```
APIStopChat                      packages/simplex-chat-python/src/simplex_chat/api.py:116
APIGetChat                       packages/simplex-chat-python/src/simplex_chat/api.py:686
APICreateMemberContact           packages/simplex-chat-python/src/simplex_chat/api.py:764
APISendMemberContactInvitation   packages/simplex-chat-python/src/simplex_chat/api.py:774-779
```

## Mechanics

`Controller.hs`:
```
APICreateMemberContact :488
  groupId :: GroupId
  groupMemberId :: GroupMemberId
APISetNetworkConfig :516
  networkConfig :: NetworkConfig
APIContactInfo :524
  contactId :: ContactId
APIPrepareContact :569
  userId :: UserId
  connLink :: ACreatedConnLink
  verifiedDomain :: Maybe (StrJSON "SimplexDomain" SimplexDomain)
  contactSLinkData :: ContactShortLinkData
APIPrepareGroup :570
  userId :: UserId
  connLinkContact :: CreatedLinkContact
  direct :: DirectLink
  verifiedDomain :: Maybe (StrJSON "SimplexDomain" SimplexDomain)
  groupSLinkData :: GroupShortLinkData
APIConnectContactViaAddress :579
  userId :: UserId
  incognito :: IncognitoEnabled
  contactId :: ContactId
ChatPagination :1139-1144
  CPLast {count :: Int}
  CPAfter {chatItemId :: ChatItemId, count :: Int}
  CPBefore {chatItemId :: ChatItemId, count :: Int}
  CPAround {chatItemId :: ChatItemId, count :: Int}
  CPInitial {count :: Int}
```
`Library/Commands.hs`:
```
APIPrepareContact :2208     unStrJSON <$> verifiedDomain, passed to createPreparedGroup
APIPrepareGroup :2236       unStrJSON <$> verifiedDomain, passed to preparedGroupFromLink
connectWithPlan :4694       vName = StrJSON . nameDomain <$> planSimplexName, passed at :4704 and :4713
chatCommandP :6560          " domain=" *> (StrJSON <$> strP)
chatCommandP :6561          " domain=" *> (StrJSON <$> strP)
```
`API/Docs/Commands.hs` entries:
```
Group commands        :130-131
Connection commands   :156-160
Chat commands         :170-172
Chat management       :221-222
APIConnectPlan        :153, syntax
removed
  the twelve commands, from undocumentedCommands (:359)
  the placeholder comments of APIChatItemsRead and APIGetChat
```
`API/Docs/Responses.hs` entries:
```
CRApiChat                  :50
CRContactInfo              :65
CRItemsReadForChat         :85
CRNetworkConfig            :90
CRNewMemberContact         :92
CRNewMemberContactSentInv  :93
CRNewPreparedChat          :94
removed
  the seven responses, from undocumentedResponses (:130)
  the placeholder comments of CRApiChat and CRItemsReadForChat
```
`API/Docs/Types.hs`:
- Imports: `isLower` (`:19`); `KeepAliveOpts (..)` (`:53`).
- `consSnake` (`:164-165`), new: the constructor name without the prefix, in lowercase, with `_` after each lowercase letter followed by an uppercase one.
- `PlanResolveMode` syntax: `:355`.
- `deriving instance Generic` of the nineteen types: eleven added, eight enabled from comments.

Entries:
```
new, in alphabetical order
  GroupOwnerContact     :313
  HostMode              :324
  KeepAliveOpts         :328
  NetworkConfig         :348
  NetworkTimeout        :350
  QueueStatus           :367
  SMPProxyFallback      :398
  SMPProxyMode          :399
  SMPWebPortServers     :400
  SocksMode             :406
  TransportSessionMode  :416
enabled from comments
  ChatPagination        :438    STUnion, with syntax
  ConnectionStats       :439
  MsgContentTag         :443    suffix "_"
  NavigationInfo        :444
  RcvQueueInfo          :449
  RcvSwitchStatus       :450    STEnum' (consSnake "RS")
  SndQueueInfo          :452
  SndSwitchStatus       :453    STEnum' (consSnake "SS")
```
`API/TypeInfo.hs`:
- `AChatInfo` → `ChatInfo`: `:177`.
- `SocksProxyWithAuth` in `stringTypes`: `:228`.

`node/src/api.ts`:
```
import of CR                    :1
stopChat                        :151
apiReceiveFile                  :542
apiConnectPlan                  :732
apiConnectPreparedLink          :752
apiPrepareContact               :768
apiPrepareGroup                 :778
apiConnectPreparedContact       :788
apiConnectPreparedGroup         :803
apiConnectContactViaAddress     :813
apiGetChat                      :951
apiChatItemsRead                :962
apiContactInfo                  :972
apiGetNetworkConfig             :982
apiSetNetworkConfig             :992
apiCreateMemberContact          :1088
apiSendMemberContactInvitation  :1099
```
Generated by the "Bot API docs" tests (`tests/APIDocs.hs:32-50`):
```
bots/api/COMMANDS.md
bots/api/TYPES.md
TS/commands.ts
TS/responses.ts
TS/types.ts
packages/simplex-chat-python/src/simplex_chat/types/_commands.py
packages/simplex-chat-python/src/simplex_chat/types/_responses.py
packages/simplex-chat-python/src/simplex_chat/types/_types.py
```
Generated by `npm run docs` (typedoc, `node/package.json:24`):
```
node/docs/api.Class.ChatApi.md
node/docs/api.Class.LiveMessage.md    line links only
```

## Tests

`node/tests/commands.test.ts`, new tests of generated `cmdString`:
```
APIConnectPlan :15              unknown: /_connect plan 1 L; allGroups and never: " resolve=<mode>"; linkOwnerSig: " sig=<json>"
APIGetChat :33                  count=0; initial=1; around=5 count=0; after=5 count=100; before=5 count=10; content=image count=10 search=k
APIChatItemsRead :49            /_read chat items @2 3,4
APIPrepareContact :55           with and without " domain=biz.simplex"
APIPrepareGroup :68             direct false: " direct=off domain=news.simplex"; direct true: both omitted
prepared chat connection :81    /_connect contact @2 incognito=on json <msg>; /_connect group #3 contact=1 owner=abc; /_connect group #3 json <msg>; /_connect contact 1 incognito=on 2
member contact :98              /_create member contact #1 2; /_invite member contact @2 with and without json <msg>
network config :109             /network; /_network <json>
```
`node/tests/api.unit.test.ts`, `core.chatSendCmd` mocked.

New helpers:
```
sentCommands :14    strings passed to the mocked core.chatSendCmd, in call order
nextResponse :18    the response of later calls of the mocked core.chatSendCmd
```
Present tests, changed: `jest.mocked(core.chatSendCmd).mock.calls.map(([, cmd]) => cmd)` is replaced by `sentCommands()`:
```
apiUpdateChatItem updates and finalizes a live message   :37
apiSetUserDomain sets and removes the name               :53
```
New tests:
```
line   call                                                   sent string                                                  result
:65    apiReceiveFile(3, "./f.txt")                           /freceive 3 approved_relays=on ./f.txt                       chatItem
:72    apiGetChat, count 0                                    /_get chat #1 count=0                                        chat
       apiGetChat, around pagination                          /_get chat @2 around=5 count=0                               chat
:80    apiChatItemsRead                                                                                                    chatInfo
:86    apiContactInfo                                                                                                      response
:92    apiPrepareContact, apiPrepareGroup                                                                                  chat
:100   apiConnectPreparedContact                                                                                           startedConnectionToContact; contactAlreadyExists
:109   apiConnectPreparedGroup, apiConnectContactViaAddress                                                                response
:118   apiConnectPreparedLink                                                                                              sentConfirmation; sentInvitation; contactAlreadyExists
       apiConnect                                                                                                          Invitation; Contact; error "contact already exists"
:134   apiConnectPlan                                                                                                      [plan, link, planSimplexName, otherSimplexName]
:143   apiGetNetworkConfig                                                                                                 networkConfig
       apiSetNetworkConfig                                                                                                 undefined, on cmdOk
:151   apiCreateMemberContact                                 /_create member contact #1 2                                 contact
       apiSendMemberContactInvitation(2, "hi")                /_invite member contact @2 json {"type":"text","text":"hi"}  contact
```
- Present test, unchanged: "stops the chat before closing the store" (`:191`): `send /_stop` is expected before `closeStore`.

`node/tests/api.test.ts`, new tests with a local SMP server:
```
:164   should connect via prepared contact and read messages
         plan of alice's address: contactAddress, ok, with link data; planSimplexName undefined
         apiPrepareContact: direct chat; apiConnectPreparedContact: startedConnectionToContact
         after alice accepts the contact request and both clients receive contactConnected: apiGetChat initial: "hello" rcvNew; apiChatItemsRead: direct chat info
         apiGetChat around the item: rcvRead; apiContactInfo: contact, customUserProfile undefined
         apiConnectPreparedLink with alice's invitation: sentConfirmation, then both connected
:205   should get and set network config
         smpProxyFallback allowProtected and logTLSErrors true are set; the result of the next apiGetNetworkConfig is equal to the set configuration
```
- Present test, unchanged: "should create member contact and send invitation" (`:81`): both member contact methods are called against the core, with a string message (`:149`).

`tests/ChatTests/Names.hs`, new test:
```
testPrepareChannelWithName :328   /_prepare group ... direct=off domain=team.simplex (:343), parsed at Library/Commands.hs:6561
                                    plan of the prepared channel: "known prepared group #team", "SimpleX name: #team (verified)"
```

Haskell, present tests, unchanged:
- "Bot API docs" (`tests/APIDocs.hs`): coverage of commands, their field names, responses, events and types, and of the responses of each command (`:73-151`); each generated file is equal to its regenerated text (`:153-157`).
- Tests of the changed lines of `Library/Commands.hs`, in `tests/ChatTests/Names.hs`:
```
testConnectByName :34                     /c @alice.simplex: connectContactViaName, APIPrepareContact with the name
testConnectByChannelName :297             /c #team.simplex: joinChannelViaRelays, APIPrepareGroup with the name
testConnectByNameBusinessAndChannel :412  /_prepare contact ... domain=biz.simplex (:431): business chat with the domain
```
