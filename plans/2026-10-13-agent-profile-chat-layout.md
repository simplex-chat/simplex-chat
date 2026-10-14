# Chat layout and agent peer type

Date: 2026-10-13.

The layout of a chat (`classic` or `terminal`) is stored with the chat when its record is created, from the group profile or from the address data, and is overridden by the user per chat; LLM agents are identified by the peer type `agent`.

## 1. Patches

- Patch 00, commit `be2e1c1cc`: type `ChatLayout`; `chatLayout` of `Contact` and `GroupInfo` (core, Kotlin, iOS).
- Patch 01, `patches/llm-ux/01-chat-layout.patch`: sections 2-11.
- Next patch: section 12, peer type `agent`.

## 2. Protocol and types

```
Types.hs
  ChatLayout (:782)
    CLClassic                                  "classic"
    CLTerminal                                 "terminal"
    CLUnknown Text                             any other text, encoded as received
  Contact (:196), GroupInfo (:518)
    chatLayout :: Maybe ChatLayout             layout of the chat
    chatLayoutOverride :: Maybe ChatLayout     choice of the user
  GroupProfile (:940)
    chatLayout :: Maybe ChatLayout             set by a group owner
Store/Profiles.hs
  AddressSettings (:512)
    chatLayout :: Maybe ChatLayout             set by the address owner
Protocol.hs
  ContactShortLinkData (:1606)
    chatLayout :: Maybe ChatLayout             address link data, next to business
```

- Chat protocol version: unchanged.
- JSON: a string; the key is omitted for `Nothing` (`defaultJSON`).
- `Profile` and `LocalProfile` are unchanged.
- `GroupProfile.chatLayout` is sent in every JSON encoding of the group profile:
  - `x.grp.info`
  - `x.grp.inv`
  - `x.grp.link.inv`
  - `x.grp.link.reject`
  - group link data (`GroupShortLinkData`)
  - chat link card `MCLGroup`

## 3. Layout of the chat

`chat_layout` of `contacts` and of `groups` is written once, in the function that creates the record:

```
record                                               function                                             chat_layout
group created by the user                            createNewGroup (Store/Groups.hs:384)                 chatLayout of the new group profile
group of a received invitation, x.grp.inv            createGroupInvitation_ (:484)                        chatLayout of the profile in the invitation
group or channel prepared, /_prepare group           createPreparedGroup (:677), createGroup_ (:933)      chatLayout of the profile in the link data
group joined via a link without prepare              createGroupViaLink' (:892), createGroup_             chatLayout of the profile in x.grp.link.inv
contact prepared from an address                     createPreparedContact (Store/Direct.hs:408),         ContactShortLinkData.chatLayout
                                                     createContact_ (Store/Shared.hs:419)
business chat prepared from a business address       createPreparedGroup, createGroup_                    ContactShortLinkData.chatLayout (4.2)
business chat joined without prepare                 createGroupViaLink', createGroup_                    AddressSettings.chatLayout of the business (4.2)
contact of an invitation link                        createPreparedContact, createDirectContact (:454)    NULL
contact of an address, joined without prepare        createDirectContact                                  NULL
contact of the address owner                         createOrUpdateContactRequest, createContactFromRequest   NULL
business chat of the business                        createBusinessRequestGroup (Store/Groups.hs:2210)    NULL
relay group                                          createRelayRequestGroup (:1952)                      NULL
member contact                                       createMemberContact, createMemberContactInvited      NULL
```

- In `createNewGroup`, `createGroupInvitation_` and `createGroup_`, `chatLayout` of the given `GroupProfile` is written to `group_profiles.chat_layout` and to `groups.chat_layout`.
- `chat_layout` of an existing record is unchanged by every later event: a received group profile is stored by `updateGroupProfile` (`Store/Groups.hs:2734`) in `group_profiles.chat_layout` only, also for a repeated invitation (`createGroupInvitation`, `:467-478`).

## 4. Sources

### 4.1. Group profile

```
owner sets            /_group, /_public group: profile JSON with chatLayout                  newGroup (Library/Commands.hs:4336)
                      /_group_profile #<groupId> <json>                                      runUpdateGroupProfile (:4179)
                      /set layout #<group> classic|terminal|default                          SetGroupChatLayout (:3673): updateGroupProfileByName gName $ \p -> p {chatLayout}
runUpdateGroupProfile updateGroupProfile; group link data is updated; x.grp.info is sent     existing path
member receives       x.grp.info                                                             xGrpInfo (Library/Subscriber.hs:3841)
                      group link data                                                        updateGroupFromLinkData (Library/Internal.hs:1635)
                      x.grp.link.inv for a prepared group                                    updatePreparedUserAndHostMembers' (Store/Groups.hs:826)
```

- A layout-only change of a group profile: the item "group profile updated" is created (`sameGroupProfileInfo`, `Library/Internal.hs:3321`); `CRGroupUpdated` and `CEvtGroupUpdated` are sent.
- Business chat: `/set layout #<chat>` of the business is stored in its group profile; `x.grp.prefs` is sent (`runUpdateGroupProfile`, `:4190-4203`); the chat of the customer is unchanged.

### 4.2. Address

The layout is passed on the path of `businessAddress`:

```
address owner
  APISetAddressSettings (Library/Commands.hs:2533)    settings are compared with the stored settings, with chatLayout
                                                      setMyAddressData (:4147): link data is written when shortLinkDataSet
                                                      updateUserAddressSettings (Store/Profiles.hs:640): user_contact_links.chat_layout
  contactShortLinkData (:4831)                        ContactShortLinkData p msg business layout Nothing; layout: chatLayout of the settings
connecting client
  APIConnectPlan                                      link data is returned to the client in the plan
  APIPrepareContact (:2200)                           chatLayout of the link data passed by the client:
    business address                                    createPreparedGroup with businessGroupProfile profile groupPreferences chatLayout
    contact address, SCMContact                         createPreparedContact ... welcomeSharedMsgId ctLayout verified_; ctLayout: chatLayout (:2223)
    invitation link, SCMInvitation                      the same call; ctLayout: Nothing
business chat
  businessGroupProfile (Library/Internal.hs:1217)     new parameter Maybe ChatLayout: chatLayout of the group profile
  profileContactRequest (Library/Subscriber.hs:1500)  AddressSettings {autoAccept, chatLayout}, passed to acceptBusinessJoinRequestAsync
  acceptBusinessJoinRequestAsync (Internal.hs:1128)   new parameter Maybe ChatLayout; x.grp.link.inv is sent with businessGroupProfile userProfile groupPreferences chatLayout
```

- The client of a customer without a prepared chat creates the business chat from `x.grp.link.inv` (`Library/Subscriber.hs:637-650`, `:3213-3218`).
- Invitation links: `contactShortLinkData profile Nothing`; `chatLayout` is `Nothing`.

## 5. User override

```
APISetChatLayout (ChatRef cType chatId scope) chatLayoutOverride    Library/Commands.hs:1658
  CTDirect                     -> getContact; setContactChatLayoutOverride db user ct chatLayoutOverride; ok user
  CTGroup, scope == Nothing    -> getGroupInfo; setGroupChatLayoutOverride db user g chatLayoutOverride; ok user
  otherwise                    -> throwCmdError "not supported"
```

- `chat_layout_override` is written by this command only; `chat_layout`, `ChatSettings` and `/_settings` are unchanged.

## 6. Storage

Migration `M20261013_chat_layout`, SQLite and Postgres, registered in `Store/SQLite/Migrations.hs`, `Store/Postgres/Migrations.hs` and `simplex-chat.cabal`:

```
contacts.chat_layout TEXT
contacts.chat_layout_override TEXT
groups.chat_layout TEXT
groups.chat_layout_override TEXT
group_profiles.chat_layout TEXT
user_contact_links.chat_layout TEXT
```

- The six columns are dropped by the down migration.
- `M20261013_chat_layout` is ordered after `M20261010_invite_member_via_group`. The migration is renamed to the date of its merge when a later migration name is merged first.

Reads and selected columns:

```
ContactRow' (Store/Shared.hs:493)             sixth tuple: uiThemes, chatDeleted, customData, chatItemTTL, chatLayout, chatLayoutOverride
                                              read by toContact (:499) and toContact' (Store/Connections.hs:134)
ct.chat_layout, ct.chat_layout_override       after chat_item_ttl: Store/Direct.hs:328, :981; Store/ContactRequest.hs:123; Store/Connections.hs:125
GroupInfoRow (Store/Shared.hs:699)            profile layout: after memberAdmission
                                              new tuple (chatLayout, chatLayoutOverride) before GroupKeysRow; row tuples have at most 12 fields
                                              (FromRow instances of simplexmq, Agent/Store/AgentStore.hs:2146-2152)
                                              read by toGroupInfo (:707); profile layout also by toGroupProfile (Store/Groups.hs:2852)
gp.chat_layout                                after gp.member_admission: Store/Shared.hs:846; Store/Connections.hs:158; Store/Groups.hs:2845
g.chat_layout, g.chat_layout_override         after g.group_domain_verified: Store/Shared.hs:852; Store/Connections.hs:164
chat_layout of user_contact_links             after auto_reply_msg_content: Store/Profiles.hs:548, :590; read by toUserContactLink (:531)
```

Writes:

```
contacts.chat_layout                createContact_ (Store/Shared.hs:430)
groups.chat_layout                  createNewGroup (Store/Groups.hs:419); createGroupInvitation_ (:500); createGroup_ (:957)
group_profiles.chat_layout          createNewGroup (:406); createGroupInvitation_ (:494); createGroup_ (:944); updateGroupProfile (:2766)
user_contact_links.chat_layout      updateUserAddressSettings (Store/Profiles.hs:640)
contacts.chat_layout_override       setContactChatLayoutOverride (Store/Direct.hs:1122), with updated_at
groups.chat_layout_override         setGroupChatLayoutOverride (Store/Groups.hs:3702), with updated_at
```

Files regenerated by the test suite:

- `src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql`: `tests/SchemaDump.hs`
- `src/Simplex/Chat/Store/SQLite/Migrations/chat_query_plans.txt`: `tests/SchemaDump.hs`
- `src/Simplex/Chat/Store/Postgres/Migrations/chat_schema.sql`: `tests/PostgresSchemaDump.hs`

## 7. API and CLI

```
/_layout <chatRef> classic|terminal|default                 APISetChatLayout ChatRef (Maybe ChatLayout); default is Nothing; CRCmdOk
/set layout #<group> classic|terminal|default               SetGroupChatLayout GroupName (Maybe ChatLayout); CRGroupUpdated
/_group_profile #<groupId> <json>                           APIUpdateGroupProfile, existing; chatLayout in the profile JSON
/_group <userId> <json>                                     APINewGroup, existing; chatLayout in the profile JSON
/_address_settings <userId> <json>                          APISetAddressSettings, existing; chatLayout in the settings JSON
/auto_accept on [business|incognito=on|off] [layout=classic|terminal] [<auto reply>]    SetAddressSettings; autoAcceptP (Library/Commands.hs:6764)
/auto_accept off [layout=classic|terminal]
/_prepare contact <userId> <links> <json>                   APIPrepareContact, existing; chatLayout in the link data JSON
```

- Constructors: `Controller.hs:458`, `:677`. Parsers: `chatCommandP` (`Library/Commands.hs:6242`, `:6492`), `chatLayoutP`, `chatLayoutP_` (`:6642-6643`).
- With `/auto_accept` without `layout=` the address layout is removed.
- Remote desktop: the new commands are forwarded to the host (`allowRemoteCommand`, `Controller.hs:737`).

## 8. Kotlin and iOS

The apps have the types, the computed value and the function of the override. Controls and drawing: `2026-10-13-flat-chat-layout-ui.md`.

```
                          Kotlin model/ChatModel.kt                          iOS SimpleXChat/ChatTypes.swift
Contact                   chatLayoutOverride: ChatLayout? = null (:1911)     var chatLayoutOverride: ChatLayout? (:2516)
GroupInfo                 chatLayoutOverride: ChatLayout? = null (:2541)     var chatLayoutOverride: ChatLayout? (:2921)
GroupProfile              chatLayout: ChatLayout? = null (:2711)             var chatLayout: ChatLayout? (:3091); init unchanged
ContactShortLinkData      chatLayout: ChatLayout? = null (:2750)             var chatLayout: ChatLayout? (:3139)
ChatInfo.chatLayout       (:1861)                                            (:2334)
AddressSettings           chatLayout: ChatLayout? = null (SimpleXAPI.kt:7704)    var chatLayout: ChatLayout? (AppAPITypes.swift:1587)

Kotlin model/SimpleXAPI.kt           CC.ApiSetChatLayout(type: ChatType, id: Long, layout: ChatLayout?) (:4039)
                                     cmdString "/_layout ${chatRef(type, id, scope = null)} ${layout?.text ?: "default"}"; cmdType "apiSetChatLayout"
                                     apiSetChatLayout (:1502): true on CR.CmdOk, as apiSetSettings
iOS Shared/Model/AppAPITypes.swift   case apiSetChatLayout(type: ChatType, id: Int64, layout: ChatLayout?) (:117)
                                     cmdString "/_layout \(ref(type, id, scope: nil)) \(layout?.text ?? "default")"; cmdType "apiSetChatLayout"
iOS Shared/Model/SimpleXAPI.swift    apiSetChatLayout (:918): sendCommandOkResp
Kotlin UserAddressView.kt            AddressSettingsState.chatLayout (:699): constructor parameter (:702), constructor(settings) (:715),
                                     addressSettings (:724), equals (:735), hashCode (:744); passed by the six constructions (:637-784)
iOS UserAddressView.swift            AddressSettingsState.chatLayout (:585): init(settings:) (:594), addressSettings (:602)
```

- `GroupProfile`, `ContactShortLinkData` and `AddressSettings` are sent back to the core by the apps (`/_prepare group`, `/_prepare contact`, `/_group_profile`, `/_address_settings`); with the new fields the layout is preserved in these commands.
- The address layout is read from the stored settings into `AddressSettingsState` and is sent unchanged by every save of the address settings; the apps have 0 controls for it.

`setChatLayout`: Kotlin `views/chatlist/ChatListNavLinkView.kt:1007`; iOS `Shared/Views/Chat/ChatView.swift:3262`:

```
apiSetChatLayout with the chat type and ID of the chat
on success, from the current chat info of the model (Kotlin: chatModel.getChat(chatInfo.id); iOS: chat.chatInfo):
  the contact or group is copied with chatLayoutOverride = layout
  Kotlin: chatsContext.updateChatInfo, with groupChatScope = null for a group
  iOS: ChatModel.updateContact, ChatModel.updateGroup
```

## 9. Terminal (`View.hs`)

```
viewChatLayout (:1927)          StyledString -> Maybe ChatLayout -> [StyledString]: "<name>: <layout>", when set
viewContactInfo (:1884)         lines "chat layout: <layout>" and "chat layout override: <layout>"
viewGroupInfo (:1905)           the same two lines
viewAddressSettings (:1188)     line "chat layout: <layout>" after the auto_accept line
viewGroupUpdated (:2108)        line "chat layout changed to: <layout>" or "chat layout removed", for the profile layout
viewGroupProfile (:2154)        line "chat layout: <layout>" after the welcome message lines, for the profile layout
```

## 10. Libraries and docs

```
src/Simplex/Chat/Bot.hs                                 initializeBotAddress' (:53): SetAddressSettings with chatLayout of the stored address settings; Nothing for a new address
packages/simplex-chat-python/src/simplex_chat/bot.py    Bot(chat_layout: T.ChatLayout | None = None) (:99); _sync_address (:158): desired["chatLayout"], when given
packages/simplex-chat-nodejs/src/api.ts                 apiSetAddressSettings (:415): third parameter storedSettings?: T.AddressSettings; written settings: its fields, then autoAccept, autoReply, businessAddress of the bot settings
packages/simplex-chat-nodejs/src/bot.ts                 createOrUpdateAddress (:179): address.addressSettings is passed as storedSettings
packages/simplex-chat-client/typescript/src/client.ts   enableAddressAutoAccept (:106), disableAddressAutoAccept (:115): unchanged (open question 3)
bots/src/API/Docs/Commands.hs                           cliCommands: "SetGroupChatLayout"; undocumentedCommands: "APISetChatLayout"
bots/README.md                                          section "Chat layout": address settings and group profile
docs/protocol/simplex-chat.schema.json                  groupProfile.optionalProperties: chatLayout {"enum": ["classic", "terminal"]}
docs/protocol/simplex-chat.md                           x.grp.info: chatLayout of the group profile
apps/multiplatform/spec/api.md, apps/ios/spec/api.md    apiSetChatLayout
apps/multiplatform/spec/state.md, apps/ios/spec/state.md    ChatInfo.chatLayout
regenerated by tests/APIDocs.hs                         bots/api/TYPES.md
                                                        packages/simplex-chat-client/types/typescript/src/types.ts
                                                        packages/simplex-chat-python/src/simplex_chat/types/_types.py
regenerated by typedoc (npm run docs)                   packages/simplex-chat-nodejs/docs/api.Class.ChatApi.md
```

- `packages/simplex-chat-nodejs` is built with the published `@simplex-chat/types` (`package.json:27`): `chatLayout` is kept by the object spread of `storedSettings`, and the library code is independent of the type `ChatLayout`. `BotAddressSettings` (`src/api.ts:23`) and `botAddressSettings` (`src/util.ts:48`) are unchanged (open question 3).
- Python `Bot`: the address settings are written as declared by the constructor; with `chat_layout=None` a stored address layout is removed, as a stored welcome message with `welcome=None`.

## 11. Compatibility and tests

```
group profile with chatLayout, older core           the key is ignored; chat_layout of the group is NULL
x.grp.info or link data of an older owner           group_profiles.chat_layout is replaced with NULL; groups.chat_layout is unchanged
unsigned x.grp.info forwarded by an older relay     encoded again (Protocol.hs:1593): chatLayout is removed from the forwarded profile
signed x.grp.info                                   forwarded as received bytes (Protocol.hs:1594)
address link data with chatLayout, older client     the key is ignored; chat_layout of the chat is NULL, also after the upgrade; the user sets the override
unknown layout value                                stored and sent as received (CLUnknown); skipped by ChatInfo.chatLayout
directory service                                   layout-only change of a listed group: sameProfile (Directory/Service.hs:533-536) is true; the listing status is unchanged
bots with older types                               the new fields are ignored
new desktop, old mobile                             records are received without the new fields: classic; /_layout: command error from the host, the app model is unchanged
old desktop, new mobile                             the new fields are ignored; chatLayout is removed from the JSON of /_prepare contact, /_prepare group, /_group_profile and /_address_settings sent by the old app
```

Tests: `tests/ChatTests/Profiles.hs`, group "chat layout". The pair `(chatLayout, chatLayoutOverride)` of the stored record is returned by `contactChatLayout` and `groupChatLayout`, `chatLayout` of the stored group profile by `groupProfileChatLayout` (`tests/ChatTests/Utils.hs:733-751`).

```
testSetChatLayout (:3891)                  NULL for a new contact and a new group; /_layout terminal, classic, default; /info lines; /_layout for notes and for a support scope: command error; /_layout @2 wide: parser error
testGroupChatLayout (:3930)                group created with chatLayout terminal; override; /info lines; member joined by invitation: terminal
                                           /set layout #team classic: owner and member keep terminal; /group_profile line: classic
                                           member prepared from the link: classic; /set layout #team default, then the member joins: classic
testGroupLinkChatLayout (:3996)            member joined by /c <full link> without prepare: terminal
testGroupInvitationChatLayout (:4013)      invitation repeated after /set layout #team classic: chat terminal, group profile classic
testUnknownChatLayout (:4027)              group created with chatLayout "wide": CLUnknown "wide" is read for the chat and for the group profile
testAddressChatLayout (:4037)              /_address_settings with chatLayout; prepared contact: terminal; override, default; contact of the address owner: NULL; /auto_accept with and without layout=
testBusinessAddressChatLayout (:4077)      /auto_accept on business layout=terminal; prepared business chat: terminal; chat of the business: NULL; customer joined by the full link: terminal
testChannelChatLayout (Groups.hs:10312)    /set layout #team terminal by the channel owner; owner and relay: NULL; subscriber joined by /c <short link> (joinChannelViaRelays): terminal
tests/ProtocolTests.hs (:354, :357)        x.grp.info with "chatLayout":"terminal"; "wide" is decoded as CLUnknown "wide" and encoded unchanged
nodejs tests/bot.unit.test.ts              run: stored address settings are passed (:131); apiSetAddressSettings: stored chatLayout is kept (:138)
python tests/test_client_and_waiters.py    test_bot_address_settings_include_the_chat_layout (:946): Bot(chat_layout="terminal")
```

Untested:

- contact created without prepare (open question 1)
- prepared contact of an invitation link: NULL
- member contact: NULL
- business chat: later change of the address layout
- business chat: `x.info` profile update (`updateGroupProfileFromMember`)
- `x.grp.info` of an older owner: profile value NULL, chat value unchanged
- both values after `APIChangePreparedContactUser` and `APIChangePreparedGroupUser`
- address layout in link data after a user profile update (`updateProfile_`)
- `/_layout` forwarded by a remote desktop
- `ChatInfo.chatLayout` and `setChatLayout` of both apps
- address settings saved in the apps with a stored layout
- `initializeBotAddress'` with a stored layout
- Postgres down migration

## 12. Peer type `agent` (next patch)

Peer type and layout are independent: the layout of a chat with an agent is `classic` unless the address of the agent has `chatLayout`.

```
Types.hs                                                ChatPeerType (:756-780): new constructor CPTAgent, text "agent", in textDecode and textEncode
Kotlin ChatModel.kt                                     ChatPeerType (:2175-2199): object Agent, "agent", in text and serializer
iOS ChatTypes.swift                                     ChatPeerType (:227): case agent, "agent", in text and init(from:)
/create agent [files=on] [service=on] <name> [<bio>]    CreateActiveUser; newBotUserP (Library/Commands.hs:6617) with a new parameter ChatPeerType: CPTBot for /create bot (:6115), CPTAgent here
/set bot commands ...                                   SetBotCommands (:3639): peerType CPTAgent is unchanged; any other value is replaced with CPTBot (:3642)
/_profile <userId> <json>                               APIUpdateProfile, existing; peerType in the profile JSON
```

Places where `peerType` is read (Kotlin strings: `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`):

```
site                                        Kotlin                     iOS                          agent
Contact.isBot                               ChatModel.kt:1962          ChatTypes.swift:2555         true for bot and agent
Contact.isAgent                             new                        new                          true for agent
useCommands                                 ChatModel.kt:1848          ChatTypes.swift:2318         by isBot
menuCommands                                ChatModel.kt:1855          ChatTypes.swift:2326         by isBot
contact icon, cube                          ChatModel.kt:1968          ChatTypes.swift:2564         by isBot
commands in messages are sent on tap        FramedItemView.kt:465      MsgContentView.swift:108     by useCommands
Connect button for an address               ComposeView.kt:1807        ComposeView.swift:512        by isBot
"contact should accept" in the preview      ChatPreviewView.kt:195     ChatPreviewView.swift:376    skipped, by isBot
"contact should accept" in the chat         ChatView.kt:880            ChatView.swift:1145          skipped, by isBot
attachment button hidden while files off    ComposeView.kt:1342        ComposeView.swift:1100       comparison with bot is replaced with isBot
cube icon of the link alert                 ConnectPlan.kt:720         NewChatView.swift:1134       comparison with bot is replaced with bot or agent
"Open to use bot"                           ChatPreviewView.kt:188     ChatPreviewView.swift:369    "Open to use agent", open_to_use_agent
"Tap Connect to use bot"                    ChatView.kt:2260           ChatView.swift:1106          "Tap Connect to use agent", chat_banner_connect_to_use_agent
"Bot"                                       ChatView.kt:2264           ChatView.swift:1110          "Agent", chat_banner_agent
agent as group member                                                                               peer type is unused
```

Terminal, libraries, docs, compatibility and tests:

```
View.hs viewUsersList (:666)               suffix " (agent)" for CPTAgent
View.hs viewUserProfile (:1594)            suffix " (agent)" for CPTAgent
View.hs viewUserProfile (:1584)            commands are listed when peerType is CPTBot or CPTAgent
nodejs src/bot.ts:102                      mkBotProfile: peerType Agent of the profile passed in BotConfig is unchanged; any other value is replaced with Bot
python bot.py                              Bot(agent: bool = False) (:85); _profile_to_wire (:203): peerType "agent" when agent, otherwise "bot"
bots/README.md:38-42                       peerType "agent" is set by /create agent or APIUpdateProfile
docs/protocol/simplex-chat.schema.json     :28 peerType enum: "agent" added
agent received by an older core            CPTUnknown "agent" (Types.hs:775); stored for contact and user profiles; read as CPTAgent after the upgrade
agent in an older app                      shown as a contact; commands menu, commands sent on tap, cube icon and bot texts are omitted
remote desktop                             "agent" stored by an old core is decoded as Agent by a new app
tests/ProtocolTests.hs                     x.info with "peerType":"agent"
tests/ChatTests/Profiles.hs                /create agent: "(agent)" is printed by /users and by /profile; after /set bot commands the peer type is agent
python tests                               test_bot_registration.py: Bot(agent=True): peerType "agent"
apps, manual                               the commands menu and commands sent on tap are available for an agent contact
```

## Interfaces

```
ChatLayout                                   Kotlin: Classic, Terminal, Unknown(type); iOS: classic, terminal, unknown
Contact.chatLayout, GroupInfo.chatLayout     layout of the chat, set by the core
chatLayoutOverride of Contact, GroupInfo     choice of the user
GroupProfile.chatLayout                      layout of the group profile; applied to chats created after it is set
ChatInfo.chatLayout                          Classic or Terminal:
                                               chatLayoutOverride, when it is Classic or Terminal
                                               otherwise chatLayout, when it is Classic or Terminal
                                               otherwise Classic
  Direct                                     of the contact
  Group                                      of the group, channel or business chat; member support scope and reports: of the group
  every other chat                           Classic
core                                         /_layout <chatRef> classic|terminal|default; CRCmdOk
                                             chatRef: @<contactId> or #<groupId>; with a scope or another chat type: command error
Kotlin                                       suspend fun apiSetChatLayout(rh: Long?, type: ChatType, id: Long, layout: ChatLayout?): Boolean
                                             fun setChatLayout(rhId: Long?, chatInfo: ChatInfo, layout: ChatLayout?, chatModel: ChatModel)
iOS                                          func apiSetChatLayout(type: ChatType, id: Int64, layout: ChatLayout?) async throws
                                             func setChatLayout(_ chat: Chat, layout: ChatLayout?)
```

- The value is updated without a reload: `chatInfo` of the chat is replaced in `setChatLayout`.
- Kotlin: the composable `ChatLayout` (`views/chat/ChatView.kt:918`) and the model type `ChatLayout` are both kept.
- Next patch: `Contact.isBot`, `Contact.isAgent` of section 12.

## Depends on

- `2026-10-13-flat-chat-layout-ui.md`: reader of `ChatInfo.chatLayout`; owner of drawing and of the controls from which `setChatLayout` is called.
- `2026-10-13-message-read-more.md`: reader of `ChatInfo.chatLayout`.
- `2026-10-13-block-markdown-rendering.md`: reader of `ChatInfo.chatLayout`.

## Open questions

1. A contact created by connecting to an address without `/_prepare contact` (`/connect <address>` of the CLI, full links): `chat_layout` is NULL; the link data is read before the contact record is created. Recommended: accept; the apps prepare the contact.
2. Chats of the address owner (contacts, business chats of the business) have NULL. Recommended: accept; the owner uses the override.
3. nodejs: `BotAddressSettings.chatLayout`; the settings written by `enableAddressAutoAccept` and `disableAddressAutoAccept` of `simplex-chat-client` are built from their parameters, and a stored address layout is removed. Recommended: the option and a `chatLayout` parameter of the two functions are added with the next published `@simplex-chat/types`.
4. Third term of `ChatInfo.chatLayout`: `Classic` in patch 01. Recommended: replaced with the Appearance value by the layout UI patch.
5. Agent identification: peer type `agent` and layout are independent; older apps show an agent as a contact without the commands menu; an agent that is a group member is shown as a member. Recommended: accept; member agents in a separate change.
