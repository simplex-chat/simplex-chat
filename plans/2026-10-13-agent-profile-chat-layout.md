# Agent peer type and chat layout

Date: 2026-10-13.

LLM agents are identified by the peer type `agent`, and the layout of a chat (`bubbles` or `flat`) is selected by a profile property that the receiving user overrides per chat.

## 1. Protocol and types

Chat protocol version: unchanged (`currentChatVersion`, `Protocol.hs:97`).

`Types.hs`:
```
ChatPeerType (:752-776)    new constructor CPTAgent, text "agent", in textDecode and textEncode
ChatLayout                 new type after ChatPeerType, with the instances of ChatPeerType (:755-776)
  CLBubbles                  "bubbles"
  CLFlat                     "flat"
  CLUnknown Text             any other text, encoded as received
Profile (:734)             chatLayout :: Maybe ChatLayout            -- set by the owner of the profile
LocalProfile (:821)        chatLayout :: Maybe ChatLayout
GroupProfile (:912)        chatLayout :: Maybe ChatLayout            -- set by a group owner
Contact (:196)             chatLayoutOverride :: Maybe ChatLayout    -- local override, stored in the receiving database only
GroupInfo (:516)           chatLayoutOverride :: Maybe ChatLayout    -- local override
```

- JSON: a string; the key is omitted for `Nothing` (`defaultJSON`, `Types.hs:2320, 2322, 2347, 2381, 2449`).
- Peer type and layout are independent: an `agent` profile without `chatLayout` is shown in bubbles.

`chatLayout` is sent in every JSON encoding of the two profile types (`Protocol.hs:471-499, 756-758, 1606-1635`):
```
Profile            Profile                                     GroupProfile
x.info             x.grp.mem.new                               x.grp.info
x.contact          x.grp.mem.intro                             x.grp.inv
x.member           x.grp.mem.fwd                               x.grp.link.inv
x.grp.acpt         contact address and invitation link data    x.grp.link.reject
x.grp.link.mem     relay link data                             group link data
x.grp.mem.info     chat link card MCLContact                   chat link card MCLGroup
x.grp.relay.inv    chat link card MCLInvitation
```

## 2. Core

### 2.1. Profile fields

`chatLayout` is copied in:
```
toLocalProfile                      Types.hs:841-843
fromLocalProfile                    Types.hs:847-850
redactedMemberProfile               Library/Internal.hs:1361-1363    as peerType
businessGroupProfile                Library/Internal.hs:1216-1218    Profile to GroupProfile
updateGroupProfileFromMember        Store/Groups.hs:2822-2828        member Profile to GroupProfile
```

`chatLayout = Nothing` is added to the `Profile` and `GroupProfile` literals, `chatLayoutOverride = Nothing` to the `Contact` and `GroupInfo` literals:
```
Types.hs                                                :780
Core.hs                                                 :175
ProfileGenerator.hs                                     :13
Library/Internal.hs                                     :3480
Library/Commands.hs                                     :6597, :6606, :6618
Store/Profiles.hs                                       :442
Store/Direct.hs                                         :908
Store/Groups.hs                                         :432, :529, :1952, :3315
apps/simplex-badge-service/src/BadgeService/Group.hs    :71
tests/ProtocolTests.hs                                  :194, :197, :334
tests/ChatTests/Utils.hs                                :94
tests/ChatTests/Profiles.hs                             :1360
tests/Bots/BroadcastTests.hs                            :36
tests/Bots/DirectoryTests.hs                            :117
tests/Bots/BadgeService/BotTests.hs                     :127
tests/Bots/BadgeService/GroupTests.hs                   :118
```

### 2.2. Profile updates

`chatLayout` is part of the profile compared and stored in the existing paths:
```
updateProfile_ (Library/Commands.hs:4061)                 own profile               address link data is updated
                                                                                    x.info is sent to each contact, incognito connections excluded (:4116)
processContactProfileUpdate (Library/Subscriber.hs:2782)  x.info of a contact       CEvtContactUpdated
updateContactFromLinkData (Library/Internal.hs:1656)      contact link data
updateGroupFromLinkData (Library/Internal.hs:1634)        group link data
runUpdateGroupProfile (Library/Commands.hs:4164)          own group profile         link data is updated
                                                                                    x.grp.info is sent
xGrpInfo (Library/Subscriber.hs:3840)                     x.grp.info of an owner    CEvtGroupUpdated
```

- `updateUserProfile` (`Store/Profiles.hs:363`): `userMemberProfileChanged` is also true for a change of `chatLayout`. The profile is then sent to each group with the next message (`sendGroupProfileUpdate`, `Library/Internal.hs:2710`).
- Business chat: `x.info` of the main member is applied to the group profile in `updateGroupProfileFromMember` (`Library/Subscriber.hs:2957-2960`), with `chatLayout`; `CEvtGroupUpdated` is sent. From `x.grp.info` only preferences are applied (`:3862`), as now.
- For a layout-only change of a group profile the item "group profile updated" is created (`sameGroupProfileInfo`, `Library/Internal.hs:3320`). For a contact the item is omitted (`visibleProfileUpdated`, `Library/Subscriber.hs:2823`), as now.
- `processMemberProfileUpdate` (`Library/Subscriber.hs:2932, 2941`): the item "updated profile" is created when `itemChanged`, in place of `contentChanged`:
```
hidden pr = pr {chatLayout = Nothing, peerType = Nothing}
itemChanged = not (sameProfileContent (hidden (redactedMemberProfile gInfo m (fromLocalProfile p))) (hidden p''))
```

### 2.3. Local override

New command; unsupported chats are rejected as in `APISetChatUIThemes` (`Library/Commands.hs:1646-1657`):
```
APISetChatLayout (ChatRef cType chatId scope) layout_:
  CTDirect                     -> setContactChatLayoutOverride db user chatId layout_; ok user
  CTGroup, scope == Nothing    -> setGroupChatLayoutOverride db user chatId layout_; ok user
  otherwise                    -> throwCmdError "not supported"
```

Decision: the override is set by `/_layout` only; `ChatSettings` and `/_settings` are unchanged. The override is unchanged by profile updates.

## 3. Storage

Migration `M20261013_chat_layout`, SQLite and Postgres, registered in `Store/SQLite/Migrations.hs`, `Store/Postgres/Migrations.hs` and `simplex-chat.cabal`:
```
contact_profiles.chat_layout TEXT       -- profile layout
group_profiles.chat_layout TEXT         -- profile layout
contacts.chat_layout_override TEXT      -- local override
groups.chat_layout_override TEXT        -- local override
```

- `M20261013_chat_layout` is a working name. The migration is named with the date of its merge, and its name is ordered after every released migration name (`migrations = sortOn name`, `Store/SQLite/Migrations.hs:363`, `Store/Postgres/Migrations.hs:117`; simplexmq `migrationsToRun`, `Agent/Store/Migrations.hs:32-34`). The four columns are dropped in the down migration.
- Regenerated by `tests/SchemaDump.hs:41-48`: SQLite `chat_schema.sql`, `chat_query_plans.txt`. Regenerated by `tests/PostgresSchemaDump.hs:29-36`: Postgres `chat_schema.sql`.

Reads, by row type:
```
ProfileRow (Store/Shared.hs:705)                after receivedPrefs
MaybeGroupMemberRow (Store/Groups.hs:269)       after receivedPrefs
ContactRow' (Store/Shared.hs:493)               chatLayout: second tuple, after receivedPrefs; chatLayoutOverride: after chatItemTTL
toContact' (Store/Connections.hs:134)           the second decoder of ContactRow'
ContactRequestRow (Store/Shared.hs:551)         third tuple, after receivedPrefs
row of toUser (Store/Shared.hs:570)             second tuple, after userPreferences; also in the literal row of createUserRecordAt (Store/Profiles.hs:173)
GroupInfoRow (Store/Shared.hs:699)              chatLayout: after memberAdmission; chatLayoutOverride: after groupDomainVerified
row of toGroupProfile (Store/Groups.hs:2846)    after memberAdmission
```

Selected columns:
```
chat_layout of contact_profiles     after preferences_json, in the column lists at:
                                    Store/Shared.hs:544, :809, :856
                                    Store/Connections.hs:122, :169, :175
                                    Store/ContactRequest.hs:120, :157
                                    Store/Direct.hs:324, :854, :976
                                    Store/Messages.hs:728, :1150, :3113, :3122, :3129
                                    after ucp.preferences in userQuery (Store/Shared.hs:562)
gp.chat_layout                      after gp.member_admission: Store/Shared.hs:846, Store/Connections.hs:158, Store/Groups.hs:2839
chat_layout_override of contacts    after chat_item_ttl: Store/Direct.hs:327, 979; Store/ContactRequest.hs:123; Store/Connections.hs:125
g.chat_layout_override              after g.group_domain_verified: Store/Shared.hs:851, Store/Connections.hs:163
```

Writes:
```
contact_profiles                                                         columns added
createContact_                       Store/Shared.hs:425                 chat_layout
createUserRecordAt                   Store/Profiles.hs:164               chat_layout
updateUserProfileFields_'            Store/Profiles.hs:378               chat_layout
updateContactProfile_'               Store/Direct.hs:739                 chat_layout
createOrUpdateContactRequest         Store/ContactRequest.hs:177, 243    chat_peer_type, chat_layout
createJoiningMember                  Store/Groups.hs:2144                chat_peer_type, chat_layout
createNewMemberProfile_              Store/Groups.hs:2508                chat_peer_type, chat_layout
updateMemberContactProfileReset_'    Store/Direct.hs:759                 chat_peer_type, chat_layout
updateMemberContactProfile_'         Store/Direct.hs:778                 chat_peer_type, chat_layout
group_profiles
createNewGroup                       Store/Groups.hs:405                 chat_layout
createGroupInvitation                Store/Groups.hs:491                 chat_layout
createGroup_                         Store/Groups.hs:939                 chat_layout
createBusinessRequestGroup           Store/Groups.hs:2228                chat_layout of the customer profile
updateGroupProfile                   Store/Groups.hs:2760                chat_layout
contacts, groups
setContactChatLayoutOverride         Store/Direct.hs, new                chat_layout_override, updated_at; by contact ID, as updateContactSettings (:1081)
setGroupChatLayoutOverride           Store/Groups.hs, new                chat_layout_override, updated_at; by group ID, as updateGroupSettings (:3210)
```

## 4. API and CLI

```
/_layout <chatRef> bubbles|flat|default                 APISetChatLayout ChatRef (Maybe ChatLayout); default is Nothing; CRCmdOk
/set layout bubbles|flat|default                        SetUserChatLayout (Maybe ChatLayout); updateProfile, as UpdateProfileImage (Library/Commands.hs:3634)
/set layout #<group> bubbles|flat|default               SetGroupChatLayout GroupName (Maybe ChatLayout)
                                                        business chat: throwCmdError; otherwise runUpdateGroupProfile, as updateGroupProfileByName_ (:4258)
/create agent [files=on] [service=on] <name> [<bio>]    CreateActiveUser; newBotUserP (:6599) with a new parameter ChatPeerType: CPTBot for /create bot, CPTAgent here
/set bot commands ...                                   SetBotCommands, existing (:3629): peerType CPTAgent is unchanged; any other value is replaced with CPTBot
/_profile <userId> <json>                               APIUpdateProfile, existing; peerType and chatLayout in the profile JSON
/_group_profile #<groupId> <json>                       APIUpdateGroupProfile, existing; chatLayout in the group profile JSON
```

- Constructors: `Controller.hs`, after `APISetChatUIThemes` (`:457`). Parsers: `chatCommandP` (`Library/Commands.hs:6087`), `/set layout #` before `/set layout `.
- Remote desktop: the new commands are forwarded to the host (`allowRemoteCommand`, `Controller.hs:735-773`).

## 5. Places where `peerType` is read

Core: `View.hs` (section 7); in every other place of the core `peerType` is set, stored or passed. Apps (Kotlin strings in `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml`):
```
site                                        Kotlin                     iOS                          agent
Contact.isBot                               ChatModel.kt:1951          ChatTypes.swift:2509         true for bot and agent
Contact.isAgent                             new                        new                          true for agent
useCommands                                 ChatModel.kt:1848          ChatTypes.swift:2291         by isBot
menuCommands                                ChatModel.kt:1855          ChatTypes.swift:2299         by isBot
contact icon, cube                          ChatModel.kt:1957          ChatTypes.swift:2518         by isBot
commands in messages are sent on tap        FramedItemView.kt:465      MsgContentView.swift:108     by useCommands
Connect button for an address               ComposeView.kt:1807        ComposeView.swift:512        by isBot
"contact should accept" in the preview      ChatPreviewView.kt:195     ChatPreviewView.swift:376    skipped, by isBot
"contact should accept" in the chat         ChatView.kt:880            ChatView.swift:1145          skipped, by isBot
attachment button hidden while files off    ComposeView.kt:1342        ComposeView.swift:1100       comparison with bot is replaced with isBot
cube icon of the link alert                 ConnectPlan.kt:720         NewChatView.swift:1134       comparison with bot is replaced with bot or agent
"Open to use bot"                           ChatPreviewView.kt:188     ChatPreviewView.swift:369    "Open to use agent", open_to_use_agent
"Tap Connect to use bot"                    ChatView.kt:2260           ChatView.swift:1106          "Tap Connect to use agent", chat_banner_connect_to_use_agent
"Bot"                                       ChatView.kt:2264           ChatView.swift:1110          "Agent", chat_banner_agent
agent as group member                                                                               peer type and profile layout are unused
```

## 6. Kotlin and iOS

Types, in Kotlin `model/ChatModel.kt` and iOS `SimpleXChat/ChatTypes.swift`; the computed values and the function `effectiveChatLayout` of both files are defined in "Interfaces":
```
                       Kotlin                                                               iOS
ChatPeerType           (:2163) object Agent, "agent", in text and serializer                (:227) case agent, "agent", in text and init(from:)
ChatLayout             sealed class, text, serializer: as ChatPeerType (:2163-2189)         enum, text, Hashable, Codable: as ChatPeerType (:227-257)
Profile                (:2096) chatLayout: ChatLayout? = null; in toLocalProfile (:2118)    (:117) chatLayout: ChatLayout?, init default nil; in toLocalProfile (:576)
LocalProfile           (:2130) chatLayout: ChatLayout? = null; in toProfile (:2149)         (:165) chatLayout: ChatLayout?, init default nil; in fromLocalProfile (:592)
GroupProfile           (:2663) chatLayout: ChatLayout? = null                               (:3014) chatLayout: ChatLayout?, init default nil
Contact                (:1878) chatLayoutOverride: ChatLayout? = null; chatLayout           (:2449) var chatLayoutOverride: ChatLayout?; chatLayout
GroupInfo              (:2489) chatLayoutOverride: ChatLayout? = null; chatLayout           (:2859) var chatLayoutOverride: ChatLayout?; chatLayout
ChatInfo               (:1523) chatLayout                                                   (:1838) chatLayout
```

Command:
```
Kotlin model/SimpleXAPI.kt           CC.ApiSetChatLayout(type: ChatType, id: Long, layout: ChatLayout?), after ApiSetChatUIThemes (:4069)
                                     cmdString "/_layout ${chatRef(type, id, scope = null)} ${layout?.text ?: "default"}"; cmdType "apiSetChatLayout"
                                     apiSetChatLayout: true on CR.CmdOk, as apiSetSettings (:1495)
iOS Shared/Model/AppAPITypes.swift   case apiSetChatLayout(type: ChatType, id: Int64, layout: ChatLayout?), after apiSetChatSettings (:116)
                                     cmdString (:332) "/_layout \(ref(type, id, scope: nil)) \(layout?.text ?? "default")"; cmdType (:539)
iOS Shared/Model/SimpleXAPI.swift    apiSetChatLayout: sendCommandOkResp, as apiSetChatSettings (:914)
```

`setChatLayout`: Kotlin `views/chatlist/ChatListNavLinkView.kt`, after `updateChatSettings` (`:962`); iOS `Shared/Views/Chat/ChatView.swift`, after `updateChatSettings` (`:3237`):
```
apiSetChatLayout with the chat type and ID of the chat
on success, from the current chat info of the model (Kotlin: chatModel.getChat(chatInfo.id); iOS: chat.chatInfo):
  the contact or group is copied with chatLayoutOverride = layout
  Kotlin: chatsContext.updateChatInfo (ChatModel.kt:466), with groupChatScope = null for a group
  iOS: ChatModel.updateContact (ChatModel.swift:639), ChatModel.updateGroup (:651)
```

## 7. Terminal (`View.hs`)

```
viewUsersList (:666)              suffix " (agent)" for CPTAgent
viewUserProfile (:1593)           suffix " (agent)" for CPTAgent
viewUserProfile (:1583)           commands are listed when peerType is CPTBot or CPTAgent
viewUserProfile (:1577)           line "chat layout: <layout>", when set
viewUserProfileUpdated (:2032)    line "chat layout changed to <layout>" or "chat layout removed"
viewContactUpdated (:2330)        line "contact <name> changed chat layout to <layout>" or "contact <name> removed chat layout"
viewGroupUpdated (:2100)          line "chat layout changed to: <layout>" or "chat layout removed"
viewContactInfo (:1883)           lines "chat layout: <layout>" and "chat layout override: <layout>", each when set
viewGroupInfo (:1902)             the same two lines
```

## 8. Libraries and docs

```
bots/src/API/Docs/Types.hs:191              chatTypesDocsData: (sti @ChatLayout, STEnum, "CL", ["CLUnknown"], "", ""), after ChatPeerType (:238)
bots/src/API/Docs/Types.hs:471              deriving instance Generic ChatLayout
bots/src/API/Docs/Commands.hs:223           cliCommands: "SetUserChatLayout", "SetGroupChatLayout"
bots/src/API/Docs/Commands.hs:349           undocumentedCommands: "APISetChatLayout"
regenerated by tests/APIDocs.hs             bots/api/TYPES.md
                                            packages/simplex-chat-client/types/typescript/src/types.ts
                                            packages/simplex-chat-python/src/simplex_chat/types/_types.py
bots/README.md:38-42                        peerType "agent" is set by /create agent or APIUpdateProfile
                                            chatLayout is set by /set layout or APIUpdateProfile
docs/protocol/simplex-chat.schema.json      :28 peerType enum: "agent" added
                                            :16 profile.optionalProperties: chatLayout {"enum": ["bubbles", "flat"]}
                                            :53 groupProfile.optionalProperties: the same
docs/protocol/simplex-chat.md               :154 x.info, :265 x.grp.info: chatLayout; other values are treated as absent
apps/multiplatform/spec/api.md              apiSetChatLayout
apps/multiplatform/spec/state.md            ChatInfo.chatLayout (:288)
apps/ios/spec/api.md                        apiSetChatLayout
apps/ios/spec/state.md                      ChatInfo.chatLayout (:366)
```

`packages/simplex-chat-nodejs` and `packages/simplex-chat-python/src/simplex_chat`:
```
nodejs src/bot.ts:102     mkBotProfile: peerType Agent of the profile passed in BotConfig is unchanged; any other value is replaced with Bot
nodejs src/util.ts:56     fromLocalProfile: chatLayout copied
nodejs package.json:27    @simplex-chat/types: the version with the regenerated types
python client.py:31       Profile.chat_layout: T.ChatLayout | None = None
python client.py:886      _profile_to_wire: chatLayout is set when chat_layout is given
python client.py:872      _sync_profile: chatLayout compared
python bot.py:85          Bot(agent: bool = False)
python bot.py:199         _profile_to_wire: peerType "agent" when agent, otherwise "bot"
python util.py:80         from_local_profile: "chatLayout" in the key list
```

## 9. Compatibility

Decision: every case below is accepted without a new chat version.
```
agent received by an older core               CPTUnknown "agent" (Types.hs:771); stored for contact and user profiles; read as CPTAgent after the upgrade
agent in an older app                         shown as a contact; commands menu, commands sent on tap, cube icon and bot texts are omitted
chatLayout received by an older core          ignored; after the upgrade it is absent until the next profile change of the peer; the user sets the override
x.info with a changed chatLayout              applied without a chat item; the open chat is drawn again; the number of changes is unlimited; with an override set the drawn layout is unchanged
x.grp.link.inv of an older host or relay      the profile of a prepared group is replaced (Store/Groups.hs:839); chatLayout is removed until the next link data refresh or x.grp.info
x.grp.info of an older owner                  the whole profile is replaced for every member (Library/Subscriber.hs:3848); chatLayout is removed
link data written by an older owner           applied in updateGroupFromLinkData (Library/Internal.hs:1639); chatLayout is removed
unsigned x.info forwarded by an older host    encoded again (Protocol.hs:1593); chatLayout is removed in the member profile, and in the contact profile when contactUpdatableFromMember (Types.hs:319)
signed channel x.grp.info                     forwarded as received bytes (Protocol.hs:1594); chatLayout is unchanged
bots with older types                         unknown fields and "agent" are ignored
profile edit in the apps                      made on a copy of the stored profile; chatLayout is unchanged
directory service                             apps/simplex-directory-service/src/Directory/Service.hs: its peer type is CPTBot (:227), as now; for a layout-only change of a listed group sameProfile (:533-536) is true and the listing status is unchanged
```

Remote desktop:
```
new desktop, old mobile     profiles are received without the new fields: bubbles
                            "agent" stored by the old core is decoded as Agent
                            /_layout: command error from the host; the app model is unchanged
old desktop, new mobile     the new fields are ignored; an agent is shown as a contact
                            /_profile and /_group_profile sent by the old app: chatLayout of that profile is removed
                            /_settings sent by the old app: the override is unchanged
host responses              decoded in the controller core (Remote/Protocol.hs:185); the new fields are optional
```

## 10. Tests

```
tests/ProtocolTests.hs         x.info with "peerType":"agent" and "chatLayout":"flat"
                               x.grp.info with chatLayout
                               "chatLayout":"wide" is decoded as CLUnknown "wide" and encoded unchanged
tests/ChatTests/Profiles.hs    /set layout flat: the update is received by the contact without a chat item; "chat layout: flat" is printed by /info
                               /_layout @2 bubbles, then a profile update of the contact: the override is unchanged; it is removed by /_layout @2 default
                               /create agent: "(agent)" is printed by /users and by /profile; after /set bot commands the peer type is agent
                               agent address with flat: peer type and layout are set in the prepared contact before connecting
                               repeated contact request with a changed profile: peer type and layout are updated (Store/ContactRequest.hs:243)
                               agent business address with flat: the layout is set in the chat of the customer; after /set layout default and the next message it is removed
                               /set layout #biz flat in a business chat: command error
tests/ChatTests/Groups.hs      /set layout #team flat by the owner: it is received by the members with the item "group profile updated", and by a member that joins by link later
                               /_layout #1 bubbles by one member: unchanged after a later group profile update
                               /_layout #1(_support:2) flat: command error
                               /set layout flat by a member, then a message to the group: the profile is sent; the item "updated profile" is omitted
                               a member profile stored with chat_layout = 'wide' is sent unchanged in x.grp.mem.intro (Library/Internal.hs:1349-1359)
packages/simplex-chat-python/tests/test_bot_registration.py    Bot(agent=True): peerType "agent"
packages/simplex-chat-python/tests/test_util.py                from_local_profile with chatLayout
packages/simplex-chat-nodejs/tests/util.test.ts                fromLocalProfile with chatLayout
apps, manual                   the commands menu and commands sent on tap are available for an agent contact
                               after setChatLayout and after a profile update of the peer, chatInfo.chatLayout of the open chat is the new value
```

## Interfaces

Effective layout of a chat. The core sends the profile value and the override; the apps compute the value (Kotlin `model/ChatModel.kt`, iOS `SimpleXChat/ChatTypes.swift`):
```
effectiveChatLayout(override, profile): ChatLayout
  override, when it is Bubbles or Flat
  otherwise profile, when it is Bubbles or Flat
  otherwise Bubbles
Contact.chatLayout      effectiveChatLayout(chatLayoutOverride, profile.chatLayout)
GroupInfo.chatLayout    effectiveChatLayout(chatLayoutOverride, groupProfile.chatLayout)
ChatInfo.chatLayout     chat                             value
  Direct                contact                          contact.chatLayout
  Group                 group or channel                 groupInfo.chatLayout
                        business chat                    groupInfo.chatLayout; the profile value is copied from the profile of the main member of the other side
                        member support scope, reports    groupInfo.chatLayout of the group
  Local                 notes                            Bubbles
  ContactRequest        contact request                  Bubbles
  ContactConnection     pending connection               Bubbles
  InvalidJSON           invalid chat                     Bubbles
```

- `ChatInfo.chatLayout` is `Bubbles` or `Flat`. `Unknown` is a value of the stored fields only.
- The layout value of a chat view (`LocalChatLayout`, `EnvironmentValues.chatLayout`) is defined in `2026-10-13-flat-chat-layout-ui.md`.
- `ChatInfo.chatLayout` of the chat is passed to the viewer of `2026-10-13-message-read-more.md`, for `Format.Bold`.
- The value is updated without a reload: `chatInfo` of the chat is replaced in `setChatLayout` and on a profile update of the peer: Kotlin `CR.ContactUpdated` (`SimpleXAPI.kt:2982`), `CR.GroupUpdated` (`:3302`); iOS `.contactUpdated` (`SimpleXAPI.swift:2601`), `.groupUpdated` (`:2823`).

Local override:
```
core      /_layout <chatRef> bubbles|flat|default; CRCmdOk
          chatRef: @<contactId> or #<groupId>; with a scope or another chat type: command error
          with default the override is removed
Kotlin    suspend fun apiSetChatLayout(rh: Long?, type: ChatType, id: Long, layout: ChatLayout?): Boolean
          fun setChatLayout(rhId: Long?, chatInfo: ChatInfo, layout: ChatLayout?, chatModel: ChatModel)
iOS       func apiSetChatLayout(type: ChatType, id: Int64, layout: ChatLayout?) async throws
          func setChatLayout(_ chat: Chat, layout: ChatLayout?)
```

- Other names: the types and fields of sections 1 and 6; the commands of section 4; `Contact.isBot` and `Contact.isAgent` of section 5.
- Kotlin: the composable `ChatLayout` (`views/chat/ChatView.kt:918`) and the model type `ChatLayout` are both kept.

## Depends on

- `2026-10-13-flat-chat-layout-ui.md`: reader of `ChatInfo.chatLayout`; owner of the layout value of a chat view, of drawing and of the control from which `setChatLayout` is called.
- `2026-10-13-flat-chat-layout-ui.md`: the current contact is passed to `updateChatSettings` and `toggleNotifications` at `ChatInfoView.kt:84-86, 920`; `chatLayoutOverride` in the model is then unchanged by mute and send receipts.
- `2026-10-13-message-read-more.md`: reader of `ChatInfo.chatLayout`.
- `2026-10-13-block-markdown-rendering.md`: reader of `ChatInfo.chatLayout`.
- `getGroupChatItem` (`Store/Messages.hs:3090`), combined change: `chat_layout` in its three profile column lists; `item_silent` of `2026-10-13-silent-messages.md`; `item_parts` of `2026-10-13-message-parts.md`.

## Open questions

1. Agent identification: peer type `agent` and layout are independent; older apps show an agent as a contact without the commands menu (`bot` is not sent to them); an agent that is a group member is not identified. Recommended: accept the three; member agents in a separate change.
2. Layout in profiles is set by API, CLI and libraries only; the apps have the per-chat override. Recommended: yes.
3. The profile layout is applied to an existing chat at the next profile change of the peer; until then the user sets the override. Recommended: accept.
4. Name of the second layout. Recommended: `flat`. Candidates: `flat`, `plain`, `linear`, `transcript`.
5. Release grouping: three releases: (1) silent messages, agent profile, flat layout, read more, `Strong` with rendering part 1; (2) block parsing of the core and rendering part 2; (3) message parts. Recommended: yes.
