# Bot API: chat, connection, member contact and network commands

Bot developers built the strings of twelve commands by hand: chat messages, prepared chats, member contacts and network configuration.
The commands are documented in the API docs generator, their TypeScript and Python types and `cmdString` functions are generated, and every command of the Node library is sent through a generated `cmdString`.

References are to branch `ep/bot-api` at `b6b8a862f`. Haskell paths without a directory are in `src/Simplex/Chat/`. `docs/` is `bots/src/API/Docs/`. `TS/` is `packages/simplex-chat-client/types/typescript/src/`. `node/` is `packages/simplex-chat-nodejs/`. `SMQ/` is simplexmq `src/Simplex/Messaging/` at `8bc4d3601` (`cabal.project:24`).

## Decisions

Documented commands. Each syntax is the input of the parser in `Library/Commands.hs`:
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
Documented errors:
```
APICreateMemberContact           CEGroupMemberNotActive
                                 CEPeerChatVRangeIncompatible
APISendMemberContactInvitation   CEGroupMemberNotActive
```
- `APIConnectPlan` syntax: `/_connect plan <userId> <connectTarget><str(resolveMode)>[ sig=<json(linkOwnerSig)>]` (parser `Library/Commands.hs:6559`).
- `PlanResolveMode` syntax: `allGroups` and `never` are rendered as ` resolve=<mode>`; `unknown`, the parser default (`Library/Commands.hs:6559`), is rendered as an empty string.

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
- `RcvSwitchStatus` and `SndSwitchStatus` values: their `strEncode` values (`SMQ/Agent/Protocol.hs:617-623`, `:651-656`), the JSON of both types (`:637-642`, `:669-674`).
- `AChatInfo` is typed as `ChatInfo`: its JSON is the JSON of `ChatInfo` (`Messages.hs:254-256`).
- `SocksProxyWithAuth` is typed as a string: its JSON is its `strEncode` (`SMQ/Transport/Client.hs:265-274`).

Core:
- Documented commands have named fields (`tests/APIDocs.hs:84-88`): six constructors are changed to record syntax.
- `ChatPagination` constructors have fields `count` and `chatItemId`, the fields of the generated union.
- `verifiedDomain` of `APIPrepareContact` and `APIPrepareGroup` has type `Maybe (StrJSON "SimplexDomain" SimplexDomain)`, as `simplexDomain` of `APISetUserDomain` (`Controller.hs:455`); it is generated as a string.
- Accepted command strings, handler results and responses are unchanged.

Node library:
- Every command is sent as `CC.<Command>.cmdString(...)`.
- A new method returns the response field other than `user` when the response has one such field and one response type is accepted; otherwise it returns the response.
- Any other response causes a `ChatCommandError`.
```
new method                    command                       returns
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
stopChat                         APIStopChat; the string stays /_stop
apiReceiveFile                   optional filePath, passed to ReceiveFile
apiConnectPlan                   returns [plan, link, planSimplexName?, otherSimplexName?]
apiGetChat                       third parameter: a count or T.ChatPagination; a count is sent as {type: "last", count}; returns T.AChat
apiCreateMemberContact           APICreateMemberContact; signature unchanged
apiSendMemberContactInvitation   APISendMemberContactInvitation; a string message is sent as {type: "text", text}; JSDoc network usage: background
```
- `apiConnect` returns `ConnReqType`, as before; for `contactAlreadyExists` a `ChatCommandError` "contact already exists" is thrown.
- Package versions are unchanged. `@simplex-chat/types` with these commands is released before the library (`node/package.json:27`).

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
`docs/Commands.hs`:
- Entries: `:130-131`, `:156-160`, `:170-172`, `:221-222`.
- `APIConnectPlan` syntax: `:153`.
- The twelve commands are removed from `undocumentedCommands` (`:359`); the placeholder comments of `APIChatItemsRead` and `APIGetChat` are removed.

`docs/Responses.hs`:
- Entries: `:50`, `:65`, `:85`, `:90`, `:92-94`.
- The seven responses are removed from `undocumentedResponses`; the placeholder comments of `CRApiChat` and `CRItemsReadForChat` are removed.

`docs/Types.hs`:
- Imports: `isLower` (`:19`); `KeepAliveOpts (..)` (`:53`).
- `consSnake` (`:164-165`): the constructor name without the prefix, in lowercase, with `_` after each lowercase letter followed by an uppercase one; used for the switch statuses (`:450`, `:453`).
- Entries in alphabetical order: `:313`, `:324`, `:328`, `:348`, `:350`, `:367`, `:398-400`, `:406`, `:416`.
- `PlanResolveMode` syntax: `:355`.
- Commented entries enabled in place: `:438-439`, `:443-444`, `:449-450`, `:452-453`.
- `Generic` instances of the nineteen types.

`bots/src/API/TypeInfo.hs`:
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

`node/tests/commands.test.ts`, generated `cmdString`:
```
APIConnectPlan :15              unknown: /_connect plan 1 L; allGroups and never: " resolve=<mode>"; linkOwnerSig: " sig=<json>"
APIGetChat :33                  count=0; initial=1; around=5 count=0; after=5 count=100; before=5 count=10; content=image count=10 search=k
APIChatItemsRead :49            /_read chat items @2 3,4
APIPrepareContact :55           with and without " domain=biz.simplex"
APIPrepareGroup :68             direct false: " direct=off domain=news.simplex"; direct true: both omitted
prepared chat connection :81    /_connect contact @2 incognito=on json <msg>; /_connect group #3 contact=1 owner=abc; /_connect contact 1 incognito=on 2
member contact :98              /_create member contact #1 2; /_invite member contact @2 with and without json <msg>
network config :109             /network; /_network <json>
```
`node/tests/api.unit.test.ts`, `core.chatSendCmd` mocked; helpers `sentCommands` (`:14`) and `nextResponse` (`:18`):
```
:65    apiReceiveFile(3, "./f.txt") sends /freceive 3 approved_relays=on ./f.txt
:72    apiGetChat with 0 sends count=0, with an around pagination sends around=5 count=0; chat is returned
:80    apiChatItemsRead returns chatInfo
:86    apiContactInfo returns the response
:92    apiPrepareContact and apiPrepareGroup return chat
:100   apiConnectPreparedContact returns startedConnectionToContact and contactAlreadyExists
:109   apiConnectPreparedGroup and apiConnectContactViaAddress return the response
:118   apiConnectPreparedLink returns each of its three responses; apiConnect returns Invitation, Contact, and throws "contact already exists"
:134   apiConnectPlan returns [plan, link, planSimplexName, otherSimplexName]
:143   apiGetNetworkConfig returns networkConfig; apiSetNetworkConfig resolves on cmdOk
:151   apiCreateMemberContact returns contact; apiSendMemberContactInvitation(2, "hi") sends json {"type":"text","text":"hi"}
```
- Present: "stops the chat before closing the store" (`:191`) asserts `send /_stop` from `stopChat`.

`node/tests/api.test.ts`, local SMP server:
```
:164   should connect via prepared contact and read messages
         plan of alice's address: contactAddress, ok, with link data; planSimplexName undefined
         apiPrepareContact: direct chat; apiConnectPreparedContact: startedConnectionToContact
         after accept and connection: apiGetChat initial: "hello" rcvNew; apiChatItemsRead: direct chat info
         apiGetChat around the item: rcvRead; apiContactInfo: contact, customUserProfile undefined
         apiConnectPreparedLink with alice's invitation: sentConfirmation, then both connected
:205   should get and set network config
         smpProxyFallback allowProtected and logTLSErrors true are set; the next get equals the set value
```
- Present: "should create member contact and send invitation" (`:81`) runs both member contact methods against the core with a string message (`:149`).

Haskell:
- "Bot API docs" (`tests/APIDocs.hs`): coverage of commands, responses and types (`:73-139`); each generated file is equal to its regenerated text (`:153-157`).
- Present tests of the changed lines of `Library/Commands.hs`, in `tests/ChatTests/Names.hs`:
```
testConnectByName :33                     /c @alice.simplex: connectContactViaName, APIPrepareContact with the name
testConnectByChannelName :296             /c #team.simplex: joinChannelViaRelays, APIPrepareGroup with the name
testConnectByNameBusinessAndChannel :388  /_prepare contact ... domain=biz.simplex (:407): business chat with the domain
```

## Open questions

1. `verifiedDomain` type (review comment 4364729789). Recommended: `StrJSON`, as now; the options are in the reply.
2. Descriptions of the twelve commands (`docs/Commands.hs`) and the seven responses (`docs/Responses.hs`). Recommended: as written.
3. Network commands in "Chat management", described as "These commands should not be used with CLI-based bots" (`docs/Commands.hs:218`). Recommended: as now; CLI bots set the network by options (`Options.hs:131-169`).
4. The MCP server builds the strings of these commands (`packages/simplex-chat-mcp/src/messages.ts:179, 183`; `tools.ts:471, 520-539, 771`; `serve.ts:90`). Recommended: the library methods in the MCP in a following pull request, after the release of `@simplex-chat/types` and `simplex-chat`.
