# Chat layout and agent peer type

Date: 2026-10-13.

The layout of a chat (`classic` or `terminal`) is computed by the core from the user's choice for the chat, the layout of the chat (stored from the group profile or the address data when the chat record is created), the user profile choice and the global choice; LLM agents are identified by the peer type `agent`.

## 1. Patches

- Patch 00, commit `be2e1c1cc`: type `ChatLayout`; `chatLayout` of `Contact` and `GroupInfo` (core, Kotlin, iOS).
- Patch 01, `patches/llm-ux/01-chat-layout.patch`: sections 2-11.
- Next patch: section 12, peer type `agent`.

## 2. Protocol and types

```
Types.hs
  ChatLayout (:787)
    CLClassic                                  "classic"; the value of an omitted non-Maybe field (omittedField, :792)
    CLTerminal                                 "terminal"
    CLUnknown Text                             any other text, encoded as received
  User (:134)
    chatLayout :: Maybe ChatLayout             choice of the user for the profile
  Contact (:197), GroupInfo (:521)
    chatLayout :: Maybe ChatLayout             layout of the chat
    userChatLayout :: Maybe ChatLayout         choice of the user for the chat
    defaultChatLayout :: ChatLayout            computed (section 5)
    activeChatLayout :: ChatLayout             computed (section 5)
  GroupProfile (:955)
    chatLayout :: Maybe ChatLayout             set by a group owner
  StoreCxt (:2319)
    globalLayout :: Maybe ChatLayout           global choice
Controller.hs
  ChatController (:343)
    globalChatLayout :: TVar (Maybe ChatLayout)    set by APISetGlobalChatLayout; kept in memory only
AppSettings.hs
  AppSettings
    uiChatLayout :: Maybe ChatLayout           global choice of the apps; default Just CLClassic
Store/Profiles.hs
  AddressSettings (:513)
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
group of a received invitation, x.grp.inv            createGroupInvitation_ (:487)                        chatLayout of the profile in the invitation
group or channel prepared, /_prepare group           createPreparedGroup (:683), createGroup_ (:939)      chatLayout of the profile in the link data
group joined via a link without prepare              createGroupViaLink' (:898), createGroup_             chatLayout of the profile in x.grp.link.inv
contact prepared from an address                     createPreparedContact (Store/Direct.hs:408),         ContactShortLinkData.chatLayout
                                                     createContact_ (Store/Shared.hs:419)
business chat prepared from a business address       createPreparedGroup, createGroup_                    ContactShortLinkData.chatLayout (4.2)
business chat joined without prepare                 createGroupViaLink', createGroup_                    AddressSettings.chatLayout of the business (4.2)
contact of an invitation link                        createPreparedContact, createDirectContact (:454)    NULL
contact of an address, joined without prepare        createDirectContact                                  NULL
contact of the address owner                         createOrUpdateContactRequest, createContactFromRequest   NULL
business chat of the business                        createBusinessRequestGroup (Store/Groups.hs:2216)    NULL
relay group                                          createRelayRequestGroup (:1958)                      NULL
member contact                                       createMemberContact, createMemberContactInvited      NULL
```

- In `createNewGroup`, `createGroupInvitation_` and `createGroup_`, `chatLayout` of the given `GroupProfile` is written to `group_profiles.chat_layout` and to `groups.chat_layout`.
- `chat_layout` of an existing record is unchanged by every later event: a received group profile is stored by `updateGroupProfile` (`Store/Groups.hs:2740`) in `group_profiles.chat_layout` only, also for a repeated invitation (`createGroupInvitation`, `:466`).
- A contact created by connecting to an address without preparing the chat from the link data (`/connect <address>`, `APIConnect` of a bot) gets `chat_layout` NULL; accepted by the product owner on 2026-10-14 and stated in `bots/README.md`.

## 4. Sources

### 4.1. Group profile

```
owner sets            /_group, /_public group: profile JSON with chatLayout                  newGroup (Library/Commands.hs:4348)
                      /_group_profile #<groupId> <json>                                      runUpdateGroupProfile (:4191)
                      /set layout #<group> classic|terminal|default                          SetGroupChatLayout (:3670): updateGroupProfileByName gName $ \p -> p {chatLayout}
runUpdateGroupProfile updateGroupProfile; group link data is updated; x.grp.info is sent     existing path
member receives       x.grp.info                                                             xGrpInfo (Library/Subscriber.hs:3841)
                      group link data                                                        updateGroupFromLinkData (Library/Internal.hs:1635)
                      x.grp.link.inv for a prepared group                                    updatePreparedUserAndHostMembers' (Store/Groups.hs:832)
```

- A layout-only change of a group profile: the item "group profile updated" is created (`sameGroupProfileInfo`, `Library/Internal.hs:3321`); `CRGroupUpdated` and `CEvtGroupUpdated` are sent.
- Business chat: `/set layout #<chat>` of the business is stored in its group profile; `x.grp.prefs` is sent (`runUpdateGroupProfile`, `:4202-4215`); the chat of the customer is unchanged.

### 4.2. Address

The layout is passed on the path of `businessAddress`:

```
address owner
  APISetAddressSettings (Library/Commands.hs:2541)    setAddressSettings user pqRatchet_ (const settings): the whole object replaces the stored settings
  SetAddressSettings (:2543), /auto_accept            setAddressSettings user pqRatchet_ (\stored -> settings {chatLayout = fromMaybe stored layout change})
  setAddressSettings (:4160)                          getUserAddress once; new settings = function of the stored settings
                                                      compared with the stored settings, with chatLayout
                                                      setMyAddressData (:4144): link data is written when shortLinkDataSet
                                                      updateUserAddressSettings (Store/Profiles.hs:641): user_contact_links.chat_layout
  contactShortLinkData (:4843)                        ContactShortLinkData p msg business layout Nothing; layout: chatLayout of the settings
connecting client
  APIConnectPlan                                      link data is returned to the client in the plan
  APIPrepareContact (:2208)                           chatLayout of the link data passed by the client:
    business address                                    createPreparedGroup with businessGroupProfile profile groupPreferences chatLayout
    contact address, SCMContact                         createPreparedContact ... welcomeSharedMsgId ctLayout verified_; ctLayout: chatLayout (:2231)
    invitation link, SCMInvitation                      the same call; ctLayout: Nothing
business chat
  businessGroupProfile (Library/Internal.hs:1217)     new parameter Maybe ChatLayout: chatLayout of the group profile
  profileContactRequest (Library/Subscriber.hs:1500)  AddressSettings {autoAccept, chatLayout}, passed to acceptBusinessJoinRequestAsync
  acceptBusinessJoinRequestAsync (Internal.hs:1128)   new parameter Maybe ChatLayout; x.grp.link.inv is sent with businessGroupProfile userProfile groupPreferences chatLayout
```

- The client of a customer without a prepared chat creates the business chat from `x.grp.link.inv` (`Library/Subscriber.hs:637-650`, `:3213-3218`).
- Invitation links: `contactShortLinkData profile Nothing`; `chatLayout` is `Nothing`.

## 5. Precedence and user choices

```
chatLayouts (Types.hs:812) :: StoreCxt -> User -> Maybe ChatLayout -> Maybe ChatLayout -> (ChatLayout, ChatLayout)
  defaultChatLayout    first known of: chatLayout of the chat, User.chatLayout, globalLayout of StoreCxt; else CLClassic
  activeChatLayout     userChatLayout when known, else defaultChatLayout
  known                CLClassic or CLTerminal; CLUnknown counts as absent
```

The two fields are computed where the records are built:

```
toContact (Store/Shared.hs:499), toContact' (Store/Connections.hs:134)
toGroupInfo (Store/Shared.hs:708)                  parameter User in place of the contact ID of the user; every caller changed
createNewGroup (Store/Groups.hs:384)               chatLayout of the new profile, userChatLayout Nothing
createGroupInvitation_ (:487)                      chatLayout of the invitation profile, userChatLayout Nothing
createContactFromRequest (Store/Direct.hs:896)     new parameter StoreCxt; chatLayout and userChatLayout Nothing
createMemberContact (Store/Groups.hs:3257)         new parameter StoreCxt; chatLayout and userChatLayout Nothing
```

Global choice:

```
APISetGlobalChatLayout (Maybe ChatLayout)    Library/Commands.hs:649: chatWriteVar globalChatLayout; CRCmdOk
chatStoreCxt' (Library/Internal.hs:3459)     storeCxt of the controller with globalLayout read from the TVar; chatStoreCxt = lift chatStoreCxt'
storeCxt (Controller.hs:233)                 pure, from config, globalLayout = Nothing: web previews, directory service, tests
```

User choices:

```
APISetChatLayout ChatRef (Maybe ChatLayout)       Library/Commands.hs:1666
  CTDirect                     -> getContact; setContactUserChatLayout (Store/Direct.hs:1125); ok user
  CTGroup, scope == Nothing    -> getGroupInfo; setGroupUserChatLayout (Store/Groups.hs:3710); ok user
  otherwise                    -> throwCmdError "not supported"
APISetUserChatLayout UserId (Maybe ChatLayout)    Library/Commands.hs:1659, as APISetUserUIThemes
  getUser; setUserChatLayout (Store/Profiles.hs:1159); currentUser is updated when the user is active; ok user'
```

- `chat_layout` of the chat, `ChatSettings` and `/_settings` are unchanged by these commands.
- A change of the user or global choice changes the computed fields of the records read after it; the apps reload the chat list (section 8).

## 6. Storage

Migration `M20261019_chat_layout`, SQLite and Postgres, registered in `Store/SQLite/Migrations.hs`, `Store/Postgres/Migrations.hs` and `simplex-chat.cabal`:

```
contacts.chat_layout TEXT
contacts.user_chat_layout TEXT
groups.chat_layout TEXT
groups.user_chat_layout TEXT
group_profiles.chat_layout TEXT
user_contact_links.chat_layout TEXT
users.chat_layout TEXT
```

- The seven columns are dropped by the down migration.
- `M20261019_chat_layout` is ordered after `M20261018_autoincrement_ids` of master. The migration is renamed to the date of its merge when a later migration name is merged first.

Reads:

```
contacts             chat_layout, user_chat_layout after chat_item_ttl; ContactRow' (Store/Shared.hs:493)
groups               chat_layout, user_chat_layout after group_domain_verified; a tuple before GroupKeysRow of GroupInfoRow (Store/Shared.hs:700)
group_profiles       chat_layout after member_admission
users                chat_layout after ui_themes; userQuery (Store/Shared.hs:564), third tuple of toUser (:571)
user_contact_links   chat_layout after auto_reply_msg_content; toUserContactLink (Store/Profiles.hs:532)
```

- Row tuples have at most 12 fields: SQLite `FromRow` instances of simplexmq (`Agent/Store/AgentStore.hs:2297-2311`).

Writes:

```
contacts.chat_layout                createContact_ (Store/Shared.hs:430)
groups.chat_layout                  createNewGroup (Store/Groups.hs:422); createGroupInvitation_ (:505); createGroup_ (:966)
group_profiles.chat_layout          createNewGroup (:410); createGroupInvitation_ (:497); createGroup_ (:954); updateGroupProfile (:2740)
user_contact_links.chat_layout      updateUserAddressSettings (Store/Profiles.hs:641)
contacts.user_chat_layout           setContactUserChatLayout (Store/Direct.hs:1125), with updated_at
groups.user_chat_layout             setGroupUserChatLayout (Store/Groups.hs:3710), with updated_at
users.chat_layout                   setUserChatLayout (Store/Profiles.hs:1159), with updated_at
```

Files regenerated by the test suite:

- `src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql`: `tests/SchemaDump.hs`
- `src/Simplex/Chat/Store/SQLite/Migrations/chat_query_plans.txt`: `tests/SchemaDump.hs`
- `src/Simplex/Chat/Store/Postgres/Migrations/chat_schema.sql`: `tests/PostgresSchemaDump.hs`

## 7. API and CLI

```
/_layout <chatRef> classic|terminal|default                 APISetChatLayout ChatRef (Maybe ChatLayout); default is Nothing; CRCmdOk
/_layout user <userId> classic|terminal|default             APISetUserChatLayout UserId (Maybe ChatLayout); CRCmdOk
/_layout global classic|terminal|default                    APISetGlobalChatLayout (Maybe ChatLayout); CRCmdOk
/set layout #<group> classic|terminal|default               SetGroupChatLayout GroupName (Maybe ChatLayout); CRGroupUpdated
/_group_profile #<groupId> <json>                           APIUpdateGroupProfile, existing; chatLayout in the profile JSON
/_group <userId> <json>                                     APINewGroup, existing; chatLayout in the profile JSON
/_show_address <userId>                                     APIShowMyAddress, existing; addressSettings of CRUserContactLink
/_address_settings <userId> <json>                          APISetAddressSettings, existing; the whole settings object
/auto_accept on [business|incognito=on|off] [layout=classic|terminal|default] [text <auto reply>]    deprecated; SetAddressSettings
/auto_accept off [layout=classic|terminal|default]
/_prepare contact <userId> <links> <json>                   APIPrepareContact, existing; chatLayout in the link data JSON
```

- Constructors: `Controller.hs:391`, `:460`, `:461`, `:596`, `:680`. Parsers: `chatCommandP` (`Library/Commands.hs:6254-6256`, `:6470`), `chatLayoutP`, `chatLayoutP_`, `autoAcceptP` (`:6778`).
- `SetAddressSettings {pqRatchet, settings, chatLayout :: Maybe (Maybe ChatLayout)}`: `Nothing` keeps the stored layout (`/auto_accept` without `layout=`), `Just Nothing` removes it (`layout=default`); the parser sets `settings.chatLayout = Nothing`, replaced by the handler.
- `/_layout user <userId> wide` and `/_layout global wide`: parser error.
- Remote desktop: the new commands are forwarded to the host (`allowRemoteCommand`, `Controller.hs:740`); the apps send `/_layout global` to the local core only (section 8).

## 8. Kotlin and iOS

The apps decode the computed fields and set the three choices. Drawing: `2026-10-13-flat-chat-layout-ui.md`.

```
                          Kotlin model/ChatModel.kt                              iOS SimpleXChat/ChatTypes.swift
User                      chatLayout: ChatLayout? = null (:1360)                 var chatLayout: ChatLayout? (:48)
Contact                   chatLayout, userChatLayout (:1918-1919);               chatLayout, userChatLayout (:2517-2518);
                          defaultChatLayout, activeChatLayout = Classic          defaultChatLayout, activeChatLayout = .classic (:2519-2520)
GroupInfo                 the same four fields (:2553-2556)                      the same four fields (:2924-2927)
GroupProfile              chatLayout: ChatLayout? = null (:2726)                 var chatLayout: ChatLayout? (:3097); init unchanged
ContactShortLinkData      chatLayout: ChatLayout? = null (:2765)                 var chatLayout: ChatLayout? (:3145)
ChatInfo.activeChatLayout of the contact or group, else Classic (:1872)          the same (:2343)
ChatLayout                ChatLayout.fromText (:2226), used by the serializer    RawRepresentable (:274), used by Codable and EnumDefault
AddressSettings           chatLayout: ChatLayout? = null (SimpleXAPI.kt:7724)    var chatLayout: ChatLayout? (AppAPITypes.swift:1593)
AddressSettingsState      chatLayout, read from and written to AddressSettings (UserAddressView.kt:699; UserAddressView.swift:585)
```

API:

```
Kotlin model/SimpleXAPI.kt           apiSetChatLayout (:1507), apiSetUserChatLayout (:1514): true on CR.CmdOk
                                     apiSetGlobalChatLayout (:1521): sendCommandOkResp to the local core (rh = null)
                                     CC.ApiSetChatLayout, CC.ApiSetUserChatLayout, CC.ApiSetGlobalChatLayout (:4053-4055)
iOS Shared/Model/AppAPITypes.swift   apiSetChatLayout, apiSetUserChatLayout, apiSetGlobalChatLayout (:117-119)
iOS Shared/Model/SimpleXAPI.swift    apiSetGlobalChatLayout (:396), sync; apiSetChatLayout (:922), apiSetUserChatLayout (:926)
```

Global choice:

```
storage        Kotlin appPrefs.chatLayout (SimpleXAPI.kt:278), key "ChatLayout"; iOS chatLayoutGroupDefault (AppGroup.swift:285), key "chatLayout"; default Classic
start          after the import of app settings, before the chat list is loaded: Kotlin Core.kt:153; iOS SimpleXAPI.swift:2301
migration      after the import of app settings in finishMigration: Kotlin MigrateToDevice.kt:696; iOS MigrateToDevice.swift:619
change         saved, sent with apiSetGlobalChatLayout, the chat list is reloaded
AppSettings    uiChatLayout: core AppSettings.hs; Kotlin SimpleXAPI.kt:8681 (prepareForExport, importIntoApp, defaults, current); iOS AppAPITypes.swift:2272, AppSettings.swift:70, :108
```

Controls:

```
chat theme screen, top section without a heading
  Kotlin ChatLayoutSection (views/chat/ChatInfoView.kt:1308), slot layoutSection of ChatWallpaperEditor (views/helpers/ThemeModeEditor.kt:239)
  iOS ChatLayoutSection (Shared/Views/Helpers/ThemeModeEditor.swift:459), first row of ChatWallpaperEditor
  row "Chat layout", two values:
    null          "<defaultChatLayout> (chat)" when chatLayout == defaultChatLayout, else "<defaultChatLayout> (default)"; sends /_layout <chat> default
    other layout  the layout other than defaultChatLayout; sends /_layout <chat> <value>
  selected: the other layout when userChatLayout is the other layout, else null
Settings > Appearance > Themes, after the destination picker
  Kotlin ChatLayoutRow (views/usersettings/Appearance.kt:897); iOS ChatLayoutPicker (AppearanceSettings.swift:878)
  all profiles     Classic, Terminal: the global choice
  a profile        "<global choice> (default)": /_layout user <userId> default; the other layout: /_layout user <userId> <value>
                   currentUser and the users list are updated: Kotlin ChatModel.updateCurrentUserChatLayout (:1155); iOS (ChatModel.swift:976)
reload           after a change of the global or profile choice: apiGetChats, updateChats; iOS also loadChat of the open chat
strings          Kotlin base strings.xml: chat_layout, chat_layout_classic, chat_layout_terminal, chat_layout_chat_suffix, chat_layout_default_suffix (:2294-2298)
                 iOS keys in code: "Chat layout", "Classic", "Terminal", "(chat)", "(default)"; en.xliff is regenerated by Xcode export
```

`setChatLayout`: Kotlin `views/chatlist/ChatListNavLinkView.kt:1007`; iOS `Shared/Views/Chat/ChatView.swift:3262`:

```
apiSetChatLayout with the chat type and ID of the chat
on success, from the current chat info of the model: the contact or group is copied with
  userChatLayout = layout, activeChatLayout = layout ?: defaultChatLayout
  Kotlin: chatsContext.updateChatInfo, with groupChatScope = null for a group; iOS: ChatModel.updateContact, ChatModel.updateGroup
```

- `GroupProfile`, `ContactShortLinkData` and `AddressSettings` are sent back to the core by the apps; with the new fields the layout is preserved in these commands.

## 9. Terminal (`View.hs`)

```
viewChatLayout (:1925)          StyledString -> Maybe ChatLayout -> [StyledString]: "<name>: <layout>", when set
viewChatLayouts (:1928)         lines, each when set:
                                  "chat layout: <layout>"
                                  "user chat layout: <layout>"
                                  "default chat layout: <layout>", when not classic
                                  "active chat layout: <layout>", when not classic
viewContactInfo (:1884)         viewChatLayouts after the UI themes line
viewGroupInfo (:1904)           the same
CRActiveUser (:119), /u         line "chat layout: <layout>" of User.chatLayout
viewAddressSettings (:1188)     line "chat layout: <layout>" after the auto_accept line
viewGroupUpdated (:2115)        line "chat layout changed to: <layout>" or "chat layout removed", for the profile layout
viewGroupProfile (:2161)        line "chat layout: <layout>" after the welcome message lines, for the profile layout
```

## 10. Libraries and docs

Every client reads the address settings with `/_show_address` and writes back the whole object with `/_address_settings`:

```
src/Simplex/Chat/Bot.hs                                 initializeBotAddress' (:53): SetAddressSettings Nothing settings Nothing (:71); the stored layout is kept
packages/simplex-chat-nodejs/src/api.ts                 apiSetAddressSettings(userId, botSettings) (:415): apiGetUserAddress; the stored settings, then autoAccept, autoReply, businessAddress of botSettings
packages/simplex-chat-nodejs/src/bot.ts                 createOrUpdateAddress (:179): apiSetAddressSettings(userId, addressSettings)
packages/simplex-chat-client/typescript/src/client.ts   apiGetAddressSettings(userId) (:118): addressSettings of CRUserContactLink; undefined without an address
                                                        apiSetAddressSettings(userId, settings) (:131)
                                                        enableAddressAutoAccept (:107), disableAddressAutoAccept (:113): @deprecated; the stored settings, then their parameters
packages/simplex-chat-python/src/simplex_chat/bot.py    Bot(chat_layout: T.ChatLayout | None = None) (:99); _sync_address (:158): None keeps the stored layout, a value sets it
bots/src/API/Docs/Commands.hs                           cliCommands: "SetGroupChatLayout"; undocumentedCommands: "APISetChatLayout", "APISetGlobalChatLayout", "APISetUserChatLayout"
bots/README.md                                          section "Chat layout" (:87): read and write of the whole settings object; contact without the address layout (section 3);
                                                        /auto_accept deprecated, stored layout kept unless layout= is given
docs/protocol/simplex-chat.schema.json                  groupProfile.optionalProperties: chatLayout {"enum": ["classic", "terminal"]}
docs/protocol/simplex-chat.md                           x.grp.info: chatLayout of the group profile
apps/multiplatform/spec/api.md, apps/ios/spec/api.md    apiSetChatLayout, apiSetUserChatLayout, apiSetGlobalChatLayout
apps/multiplatform/spec/state.md, apps/ios/spec/state.md    ChatInfo.activeChatLayout
regenerated by tests/APIDocs.hs                         bots/api/TYPES.md
                                                        packages/simplex-chat-client/types/typescript/src/types.ts
                                                        packages/simplex-chat-python/src/simplex_chat/types/_types.py
regenerated by typedoc (npm run docs)                   packages/simplex-chat-nodejs/docs/api.Class.ChatApi.md
```

- `packages/simplex-chat-nodejs` and `packages/simplex-chat-client/typescript` are built with the published `@simplex-chat/types`: the stored fields are kept by the object spread, and the library code is independent of the type `ChatLayout`. `BotAddressSettings` (`src/api.ts:23`) and `botAddressSettings` (`src/util.ts:48`) are unchanged.
- `/auto_accept` is documented in `bots/README.md` only; `CHANGELOG.md:431` is unchanged.

## 11. Compatibility and tests

```
group profile with chatLayout, older core           the key is ignored; chat_layout of the group is NULL
x.grp.info or link data of an older owner           group_profiles.chat_layout is replaced with NULL; groups.chat_layout is unchanged
unsigned x.grp.info forwarded by an older relay     encoded again (Protocol.hs:1593): chatLayout is removed from the forwarded profile
signed x.grp.info                                   forwarded as received bytes (Protocol.hs:1594)
address link data with chatLayout, older client     the key is ignored; chat_layout of the chat is NULL, also after the upgrade; the user sets the choice for the chat
unknown layout value                                stored and sent as received (CLUnknown); skipped by the precedence (section 5)
directory service                                   layout-only change of a listed group: sameProfile (Directory/Service.hs:533-536) is true; the listing status is unchanged
bots with older types                               the new fields are ignored
new desktop, old mobile                             responses of the host are decoded by the desktop core (Remote/Protocol.hs:185); defaultChatLayout and activeChatLayout
                                                    are absent: CLClassic (omittedField, Types.hs:792); the apps decode Classic (Kotlin defaults)
                                                    /_layout <chat> and /_layout user: command error from the host, the app model is unchanged
old desktop, new mobile                             the new fields are ignored; chatLayout is removed from the JSON of /_prepare contact, /_prepare group, /_group_profile and /_address_settings sent by the old app
remote desktop, global choice                       /_layout global is sent to the local core only (rh = null); records of the host are computed with the global choice
                                                    of the mobile app, sent by it at its start (open question 3)
remote desktop, profile choice                      /_layout user is sent to the host (rh of the current host)
```

Tests: `tests/ChatTests/Profiles.hs`, group "chat layout". The pair `(chatLayout, userChatLayout)` of the stored record is returned by `contactChatLayout` and `groupChatLayout`, `chatLayout` of the stored group profile by `groupProfileChatLayout` (`tests/ChatTests/Utils.hs:733-744`). The computed fields are read from the `/i` lines.

```
testSetChatLayout (:3892)                  NULL for a new contact and a new group; /_layout terminal, classic, default; /i lines; /_layout for notes and for a support scope: command error; /_layout @2 wide: parser error
testGroupChatLayout (:3933)                group created with chatLayout terminal; user choice; /i lines; member joined by invitation: terminal
                                           /set layout #team classic: owner and member keep terminal; /group_profile line: classic
                                           member prepared from the link: classic; /set layout #team default, then the member joins: classic
testGroupLinkChatLayout (:4000)            member joined by /c <full link> without prepare: terminal
testGroupInvitationChatLayout (:4017)      invitation repeated after /set layout #team classic: chat terminal, group profile classic
testUnknownChatLayout (:4031)              group created with chatLayout "wide": CLUnknown "wide" is read for the chat and for the group profile; /i: classic is computed
testChatLayoutPrecedence (:4041)           global terminal: computed terminal; user profile classic: /u line, computed classic; user choice of the chat classic over global terminal;
                                           group with chatLayout classic over user profile terminal; /_layout user 1 wide: parser error
testAddressChatLayout (:4079)              /_address_settings with chatLayout; prepared contact: terminal; user choice, default; contact of the address owner: NULL
                                           /auto_accept without layout= keeps the stored layout; layout=default removes it; /_address_settings writes the whole object
testBusinessAddressChatLayout (:4127)      /auto_accept on business layout=terminal; prepared business chat: terminal; chat of the business: NULL; customer joined by the full link: terminal
testChannelChatLayout (Groups.hs:10312)    /set layout #team terminal by the channel owner; owner and relay: NULL; subscriber joined by /c <short link> (joinChannelViaRelays): terminal
tests/ProtocolTests.hs (:354, :357)        x.grp.info with "chatLayout":"terminal"; "wide" is decoded as CLUnknown "wide" and encoded unchanged
nodejs tests/bot.unit.test.ts              run: apiSetAddressSettings(1, bot settings) (:131); apiSetAddressSettings: the stored settings are read, chatLayout is kept (:138)
python tests/test_client_and_waiters.py    Bot(chat_layout="terminal") sets the layout (:946); Bot() keeps the stored layout (:972)
```

Untested:

- contact created without prepare (section 3)
- prepared contact of an invitation link: NULL
- member contact: NULL
- business chat: later change of the address layout
- business chat: `x.info` profile update (`updateGroupProfileFromMember`)
- `x.grp.info` of an older owner: profile value NULL, chat value unchanged
- both values after `APIChangePreparedContactUser` and `APIChangePreparedGroupUser`
- address layout in link data after a user profile update (`updateProfile_`)
- the new commands forwarded by a remote desktop
- controls, `setChatLayout`, reload and app settings of both apps
- address settings saved in the apps with a stored layout
- `initializeBotAddress'` with a stored layout
- `simplex-chat-client` TypeScript functions
- Postgres down migration

## 12. Peer type `agent` (next patch)

Peer type and layout are independent: the layout of a chat with an agent is computed as in section 5; `chatLayout` of the address of the agent is the layout of the chat.

```
Types.hs                                                ChatPeerType (:761-785): new constructor CPTAgent, text "agent", in textDecode and textEncode
Kotlin ChatModel.kt                                     ChatPeerType (:2185-2210): object Agent, "agent", in text and serializer
iOS ChatTypes.swift                                     ChatPeerType (:228): case agent, "agent", in text and init(from:)
/create agent [files=on] [service=on] <name> [<bio>]    CreateActiveUser; newBotUserP (Library/Commands.hs:6631) with a new parameter ChatPeerType: CPTBot for /create bot (:6127), CPTAgent here
/set bot commands ...                                   SetBotCommands (:3636): peerType CPTAgent is unchanged; any other value is replaced with CPTBot (:3639)
/_profile <userId> <json>                               APIUpdateProfile, existing; peerType in the profile JSON
```

Places where `peerType` is read (Kotlin strings: `apps/multiplatform/common/src/commonMain/moko-resources/base/strings.xml`):

```
site                                        Kotlin                     iOS                          agent
Contact.isBot                               ChatModel.kt:1972          ChatTypes.swift:2559         true for bot and agent
Contact.isAgent                             new                        new                          true for agent
useCommands                                 ChatModel.kt:1858          ChatTypes.swift:2327         by isBot
menuCommands                                ChatModel.kt:1864          ChatTypes.swift:2335         by isBot
contact icon, cube                          ChatModel.kt:1978          ChatTypes.swift:2568         by isBot
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
View.hs viewUsersList (:667)               suffix " (agent)" for CPTAgent
View.hs viewUserProfile (:1594)            suffix " (agent)" for CPTAgent
View.hs viewUserProfile (:1584)            commands are listed when peerType is CPTBot or CPTAgent
nodejs src/bot.ts:102                      mkBotProfile: peerType Agent of the profile passed in BotConfig is unchanged; any other value is replaced with Bot
python bot.py                              Bot(agent: bool = False) (:85); _profile_to_wire (:204): peerType "agent" when agent, otherwise "bot"
bots/README.md:38-42                       peerType "agent" is set by /create agent or APIUpdateProfile
docs/protocol/simplex-chat.schema.json     :28 peerType enum: "agent" added
agent received by an older core            CPTUnknown "agent" (Types.hs:780); stored for contact and user profiles; read as CPTAgent after the upgrade
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
fields of User, Contact, GroupInfo           section 2, decoded by the apps (section 8)
GroupProfile.chatLayout                      layout of the group profile; applied to chats created after it is set
ChatInfo.activeChatLayout                    activeChatLayout of the contact or of the group, channel or business chat;
                                             member support scope and reports: of the group; every other chat: Classic
global choice                                Kotlin appPrefs.chatLayout; iOS chatLayoutGroupDefault
core                                         /_layout <chatRef> classic|terminal|default
                                               chatRef: @<contactId> or #<groupId>; with a scope or another chat type: command error
                                             /_layout user <userId> classic|terminal|default
                                             /_layout global classic|terminal|default
Kotlin                                       suspend fun apiSetChatLayout(rh: Long?, type: ChatType, id: Long, layout: ChatLayout?): Boolean
                                             suspend fun apiSetUserChatLayout(rh: Long?, userId: Long, layout: ChatLayout?): Boolean
                                             suspend fun apiSetGlobalChatLayout(layout: ChatLayout): Boolean
                                             fun setChatLayout(rhId: Long?, chatInfo: ChatInfo, layout: ChatLayout?, chatModel: ChatModel)
iOS                                          func apiSetChatLayout(type: ChatType, id: Int64, layout: ChatLayout?) async throws
                                             func apiSetUserChatLayout(userId: Int64, layout: ChatLayout?) async throws
                                             func apiSetGlobalChatLayout(_ layout: ChatLayout) throws
                                             func setChatLayout(_ chat: Chat, layout: ChatLayout?)
```

- A choice for the chat is applied without a reload: `chatInfo` of the chat is replaced in `setChatLayout`.
- A global or profile choice is applied by the reload of the chat list (section 8).
- Kotlin: the composable `ChatLayout` (`views/chat/ChatView.kt:918`) and the model type `ChatLayout` are both kept.
- Next patch: `Contact.isBot`, `Contact.isAgent` of section 12.

## Depends on

- `2026-10-13-flat-chat-layout-ui.md`: reader of `ChatInfo.activeChatLayout`; owner of drawing. The controls are in this patch (section 8).
- `2026-10-13-message-read-more.md`: reader of `ChatInfo.activeChatLayout`.
- `2026-10-13-block-markdown-rendering.md`: reader of `ChatInfo.activeChatLayout`.
- The three plans name `ChatInfo.chatLayout` and the values `Bubbles` and `Flat`; the flat layout plan also names `chatLayoutOverride` and its own controls. The names are updated with their patches.

## Open questions

1. Chats of the address owner (contacts, business chats of the business) have `chat_layout` NULL. Recommended: accept; the owner sets the choice for the chat or for the profile.
2. Text of `/i`: the computed fields are printed when they are not classic. Recommended: accept; the lines of the other `/i` tests are unchanged.
3. Remote desktop, global choice: the desktop app sends `/_layout global` to its local core; chats of a mobile host are computed with the global choice of the mobile app. Options: accept; send the global choice of the desktop to the host at the start of the session and on every change (the value of the mobile app is replaced until its restart); make `APISetGlobalChatLayout` local-only in `allowRemoteCommand` (`Controller.hs:740`). Recommended: accept.
4. Agent identification: peer type `agent` and layout are independent; older apps show an agent as a contact without the commands menu; an agent that is a group member is shown as a member. Recommended: accept; member agents in a separate change.
