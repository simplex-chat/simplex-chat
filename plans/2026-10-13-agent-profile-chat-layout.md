# Chat layout and agent peer type

Date: 2026-10-13. Revised: 2026-10-21.

The layout of a chat (`classic` or `terminal`) is computed by the apps from the user's choice for the chat, the layout of the chat, the user's choice for the profile and the app setting. LLM agents are identified by the peer type `agent` (section 11, implemented in master).

## 1. Types

```
Types/UITheme.hs
  ChatLayout (:47)
    CLClassic                                "classic"
    CLTerminal                               "terminal"
    CLUnknown Text                           any other text, stored and sent as received
  UIThemeEntityOverrides (:33)
    chatLayout :: Maybe ChatLayout           user's choice for the chat or the profile, both colour modes
Types.hs
  Contact (:223)
    chatLayout :: Maybe ChatLayout           layout of the address the contact was prepared from
  GroupProfile (:924)
    chatLayout :: Maybe ChatLayout           set by a group owner
AppSettings.hs
  AppSettings (:62)
    uiChatLayout :: Maybe ChatLayout         app setting; default Just CLClassic
Store/Profiles.hs
  AddressSettings (:515)
    chatLayout :: Maybe ChatLayout           set by the address owner
Protocol.hs
  ContactShortLinkData (:1616)
    chatLayout :: Maybe ChatLayout           address link data, next to business
```

- Chat protocol version: unchanged.
- JSON: a string; the key is omitted for `Nothing` (`defaultJSON`).
- `ChatLayout` is defined in `Types/UITheme.hs`, imported by `Types.hs`.
- `GroupProfile.chatLayout` is sent in every JSON encoding of the group profile:
  - `x.grp.info`
  - `x.grp.inv`
  - `x.grp.link.inv`
  - `x.grp.link.reject`
  - group link data (`GroupShortLinkData`)
  - chat link card `MCLGroup`

## 2. Layout of the chat

Group: `groupProfile.chatLayout`, stored in `group_profiles.chat_layout` and updated with the profile, for every member.

```
writer                                                  function
group created by the user                               createNewGroup (Store/Groups.hs:383)
group of a received invitation                          createGroupInvitation_ (:481); repeated invitation: updateGroupProfile
group prepared from link data                           createPreparedGroup, createGroup_ (:928)
group joined via a link without prepare                 createGroupViaLink', createGroup_
x.grp.info, group link data                             updateGroupProfile (:2729)
x.grp.link.inv of a prepared group                      updatePreparedUserAndHostMembers' (:821), updateGroupProfile
business member profile update                          updateGroupProfileFromMember (:2823): chat_layout of the stored profile kept
```

Contact: `contacts.chat_layout`, written once.

```
record                                          chat_layout
contact prepared from an address                ContactShortLinkData.chatLayout: APIPrepareContact (Library/Commands.hs:2222), createPreparedContact (Store/Direct.hs:407), createContact_ (Store/Shared.hs:419)
contact of an invitation link                   NULL
contact of an address, joined without prepare   NULL
contact of the address owner                    NULL
member contact                                  NULL
```

Business chat:

```
customer, prepared from a business address      businessGroupProfile profile groupPreferences chatLayout (Library/Internal.hs:1218), chatLayout of the link data
customer, joined without prepare                x.grp.link.inv of acceptBusinessJoinRequestAsync (Internal.hs:1129), AddressSettings.chatLayout (Library/Subscriber.hs:1500)
business                                        group profile of the customer (createBusinessRequestGroup, Store/Groups.hs:2205): NULL
```

- A later change of the address layout applies to new business chats only.

## 3. Sources

### 3.1. Group profile

```
owner sets            /_group, /_public group: profile JSON with chatLayout
                      /_group_profile #<groupId> <json>
                      /set layout #<group> classic|terminal|default    SetGroupChatLayout (Library/Commands.hs:3670): updateGroupProfileByName gName $ \p -> p {chatLayout}
member receives       x.grp.info, group link data, x.grp.link.inv of a prepared group
```

- A layout-only change of a group profile: the item "group profile updated" is created; `CRGroupUpdated` and `CEvtGroupUpdated` are sent.
- Business chat: the profile change is stored by the business. Current-version members receive `x.grp.prefs`, older members receive `x.grp.info` (`runUpdateGroupProfile`, `Library/Commands.hs:4202-4215`). Business chat members apply only the preferences of `x.grp.info` (`xGrpInfo`, `Library/Subscriber.hs:3862`), so the layout of their chats is unchanged.

### 3.2. Address

```
APISetAddressSettings (Library/Commands.hs:2532)    setAddressSettings user pqRatchet_ (const settings): the whole object replaces the stored settings
SetAddressSettings (:2534), /auto_accept            setAddressSettings user pqRatchet_ (\stored -> settings {chatLayout = fromMaybe stored layout change})
setAddressSettings (:4160)                          stored settings read once; compared with the new settings; link data written when shortLinkDataSet;
                                                    updateUserAddressSettings (Store/Profiles.hs:640)
contactShortLinkData (:4843)                        ContactShortLinkData p msg business layout Nothing
```

- Invitation links: `chatLayout` of the link data is `Nothing`.

### 3.3. User choices

```
chat       uiThemes.chatLayout of the contact or group    /_set theme @<contactId>|#<groupId> <json>    APISetChatUIThemes (Library/Commands.hs:1657)
profile    uiThemes.chatLayout of the user                /_set theme user <userId> <json>               APISetUserUIThemes (:1650)
app        Kotlin appPrefs.chatLayout, iOS chatLayoutDefault, AppSettings.uiChatLayout
```

- Each theme command writes the whole record; the apps keep `chatLayout` in every theme write (section 6).

## 4. Precedence

Computed by the apps; `CLUnknown` counts as absent.

```
activeChatLayout     chat choice, else defaultChatLayout
defaultChatLayout    layout of the chat, else profile choice, else app setting, else classic
```

- Note folders have no chat values: the profile choice and the app setting apply.

## 5. Storage

Migration `M20261019_chat_layout`, SQLite and Postgres:

```
contacts.chat_layout TEXT
group_profiles.chat_layout TEXT
user_contact_links.chat_layout TEXT
```

- The three columns are dropped by the down migration.
- User choices are stored in `ui_themes` of `users`, `contacts` and `groups`.

```
column                          read                                                                      written
contacts.chat_layout            ContactRow', after chat_item_ttl (Store/Shared.hs)                       createContact_ (Store/Shared.hs:419)
group_profiles.chat_layout      after member_admission: groupInfoQueryFields, getConnectionEntityKeys,    createNewGroup, createGroupInvitation_, createGroup_, updateGroupProfile
                                getGroupProfileById (Store/Groups.hs:2831)
user_contact_links.chat_layout  toUserContactLink (Store/Profiles.hs:531)                                 updateUserAddressSettings (:640)
```

Files regenerated by the test suite:

- `src/Simplex/Chat/Store/SQLite/Migrations/chat_schema.sql`: `tests/SchemaDump.hs`
- `src/Simplex/Chat/Store/SQLite/Migrations/chat_query_plans.txt`: `tests/SchemaDump.hs`
- `src/Simplex/Chat/Store/Postgres/Migrations/chat_schema.sql`: `tests/PostgresSchemaDump.hs`

## 6. Apps

```
                          Kotlin                                                   iOS
ChatLayout                ChatLayoutMode (ChatModel.kt:2214), serializer          ChatTypes.swift:262, RawRepresentable, Codable
Contact.chatLayout        ChatModel.kt:1919                                         not decoded
GroupProfile.chatLayout   ChatModel.kt:2741                                         ChatTypes.swift:3107
ContactShortLinkData      ChatModel.kt:2780                                         ChatTypes.swift:3155
ThemeModeOverrides        chatLayout, layoutOnly(), withChatLayout (Theme.kt:476)   chatLayout, layoutOnly() (ThemeTypes.swift:570)
AddressSettings           SimpleXAPI.kt:7785; AddressSettingsState                  AppAPITypes.swift:1625; AddressSettingsState
                          (UserAddressView.kt:699)                                  (UserAddressView.swift:585)
AppSettings.uiChatLayout  SimpleXAPI.kt:8742                                        AppAPITypes.swift:2304, AppSettings.swift
app setting               appPrefs.chatLayout (SimpleXAPI.kt:280)                   chatLayoutDefault (SettingsView.swift:219)
```

Kotlin computation, `model/ChatModel.kt`:

```
ChatInfo.chatLayout (:1861)      contact.chatLayout; groupInfo.groupProfile.chatLayout
ChatInfo.uiThemes (:1867)        uiThemes of the contact or group
defaultChatLayout (:1873)        chatLayout, else profileChatLayout
activeChatLayout (:1876)         uiThemes.chatLayout, else defaultChatLayout
profileChatLayout (:2243)        user.uiThemes.chatLayout, else the app setting, else Classic
```

Controls, Kotlin. iOS has none until it draws terminal rows.

```
chat theme screen, first section    ChatLayoutSection (views/chat/ChatInfoView.kt:1313)
  values                            "<default> (chat)" when the chat layout is the default, else "<default> (default)"; the other layout
  write                             setChatLayout (:1382): uiThemes.withChatLayout(layout), apiSetChatUIThemes, chat info updated in the model
Settings › Appearance › Themes      ChatLayoutRow (views/usersettings/Appearance.kt:948), after the destination picker
  app theme destination             Classic, Terminal: appPrefs.chatLayout, sent with the app settings
  profile destination               "<app setting> (default)"; the other layout: themeUserDestination updated, saveThemeToDatabase (:818)
strings                             chat_layout, chat_layout_classic, chat_layout_terminal, chat_layout_chat_suffix, chat_layout_default_suffix
```

Theme writes that keep the layout:

```
"Reset to app theme", chat           save (ChatInfoView.kt): layoutOnly() when both modes are removed; iOS ChatWallpaperEditorSheet.save
"Reset to app theme", profile        save (Appearance.kt); iOS UserWallpaperEditorSheet.save
"Remove image", profile destination  removeUserThemeModeOverrides: layoutOnly(); iOS the same
app theme destination of Appearance  Kotlin: currentUser.uiThemes = uiThemes.layoutOnly(); the open chat keeps the profile layout
image copied for both modes          iOS: the changed mode is set on the record
```

- A change of any choice recomposes the open chat; the chat list is not reloaded.
- `GroupProfile`, `ContactShortLinkData`, `AddressSettings` and the theme records are sent back to the core whole by the apps; with the new fields the layout is kept.

## 7. API and CLI

```
/_set theme user <userId> <json>              APISetUserUIThemes, existing; chatLayout in the JSON
/_set theme <chatRef> <json>                  APISetChatUIThemes, existing; chatLayout in the JSON
/set layout #<group> classic|terminal|default SetGroupChatLayout GroupName (Maybe ChatLayout); CRGroupUpdated
/_group_profile #<groupId> <json>             APIUpdateGroupProfile, existing
/_group <userId> <json>                       APINewGroup, existing
/_show_address <userId>                       APIShowMyAddress, existing
/_address_settings <userId> <json>            APISetAddressSettings, existing; the whole settings object
/auto_accept on [business|incognito=on|off] [layout=classic|terminal|default] [text <auto reply>]    deprecated; SetAddressSettings
/auto_accept off [layout=classic|terminal|default]
/_prepare contact <userId> <links> <json>     APIPrepareContact, existing; chatLayout in the link data JSON
```

- Constructors: `Controller.hs:596`, `:682`. Parsers: `chatCommandP` (`Library/Commands.hs:6629`, `:6665`), `chatLayoutP`, `chatLayoutP_` (`:6815-6816`), `autoAcceptP` (`:6937`).
- `SetAddressSettings {pqRatchet, settings, chatLayout :: Maybe (Maybe ChatLayout)}`: `Nothing` keeps the stored layout (`/auto_accept` without `layout=`), `Just Nothing` removes it (`layout=default`).

## 8. Terminal (`View.hs`)

```
viewChatLayout (:1936)          "chat layout: <layout>", when set
viewContactInfo (:1896)         chatLayout of the contact
viewAddressSettings (:1192)     after the auto_accept line
viewGroupUpdated (:2117)        "chat layout changed to: <layout>" or "chat layout removed"
viewGroupProfile (:2163)        after the welcome message lines
/u, /i                          the user and chat choices: "UI themes" JSON line
```

## 9. Libraries and docs

Clients read the address settings with `/_show_address` and write back the whole object with `/_address_settings`:

```
src/Simplex/Chat/Bot.hs                                 initializeBotAddress': SetAddressSettings Nothing settings Nothing; the stored layout is kept
packages/simplex-chat-nodejs/src/api.ts                 BotAddressSettings.chatLayout (:43), default undefined; apiSetAddressSettings (:419) writes it
packages/simplex-chat-nodejs/src/util.ts                botAddressSettings (:48): chatLayout of the address, compared by createOrUpdateAddress
packages/simplex-chat-client/typescript/src/client.ts   apiGetAddressSettings (:116), apiSetAddressSettings (:129)
                                                        enableAddressAutoAccept (:106), disableAddressAutoAccept (:111): the stored settings, then their parameters
packages/simplex-chat-python/src/simplex_chat/bot.py    Bot(chat_layout: T.ChatLayout | None = None): None removes the stored layout
bots/src/API/Docs/Commands.hs                           cliCommands: "SetGroupChatLayout"
docs/protocol/simplex-chat.schema.json                  groupProfile.optionalProperties: chatLayout {"enum": ["classic", "terminal"]}
docs/protocol/simplex-chat.md                           x.grp.info: chatLayout of the group profile
apps/multiplatform/spec                                 state.md (precedence), services/theme.md (ThemeModeOverrides), client/chat-view.md (section 10)
apps/ios/spec/services/theme.md                         ThemeModeOverrides.chatLayout
regenerated by tests/APIDocs.hs                         bots/api/TYPES.md
                                                        packages/simplex-chat-client/types/typescript/src/types.ts
                                                        packages/simplex-chat-python/src/simplex_chat/types/_types.py
```

## 10. Compatibility and tests

```
group profile with chatLayout, older core           the key is ignored
x.grp.info or link data of an older owner           group_profiles.chat_layout is replaced with NULL; the chat follows
unsigned x.grp.info forwarded by an older relay     encoded again: chatLayout is removed from the forwarded profile
signed x.grp.info                                   forwarded as received bytes
address link data with chatLayout, older client     the key is ignored; chat_layout of the contact is NULL, also after the upgrade
theme record with chatLayout, older app             the key is ignored; a theme written by the older app has no chatLayout
unknown layout value                                stored and sent as received (CLUnknown); skipped by the precedence
directory service                                   layout-only change of a listed group: sameProfile (Directory/Service.hs:533) is true; the listing is unchanged
remote desktop                                      chat and profile choices: theme records of the host; app setting: of the desktop
databases that ran the earlier migration            keep groups.chat_layout, groups.user_chat_layout, contacts.user_chat_layout, users.chat_layout, unused
```

```
tests/ChatTests/Profiles.hs
  testSetUITheme (:3810)                   theme with chatLayout terminal written and read for the user, a contact and a group
  testGroupChatLayout (:3860)              group created with terminal; member by invitation: terminal
                                           /set layout #team classic: owner and member classic; /group_profile line
                                           member prepared from the link: classic; /set layout #team default: owner and member NULL; the prepared member joins: NULL
  testGroupLinkChatLayout (:3918)          member joined by /c <full link> without prepare: terminal
  testUnknownChatLayout (:3935)            group created with "wide": CLUnknown "wide" stored and read
  testAddressChatLayout (:3941)            /_address_settings with chatLayout; prepared contact: terminal, also after the connection; contact of the address owner: NULL
                                           /auto_accept without layout= keeps the stored layout; layout=default removes it; /_address_settings writes the whole object
  testBusinessAddressChatLayout (:3985)    prepared business chat: terminal; chat of the business: NULL; customer joined by the full link: terminal
tests/ChatTests/Groups.hs
  testChannelChatLayout (:10555)           /set layout #team terminal by the channel owner: owner, relay and subscriber joined via the short link: terminal
tests/ProtocolTests.hs (:357, :360)        x.grp.info with "terminal"; "wide" decoded as CLUnknown "wide"
tests/ChatTests/Utils.hs (:734, :741)      contactChatLayout: Contact.chatLayout; groupChatLayout: groupProfile.chatLayout
nodejs tests/bot.unit.test.ts              apiSetAddressSettings writes chatLayout; runBot updates the address when chatLayout differs, and not when the settings are the same
python tests/test_client_and_waiters.py    Bot(chat_layout="terminal") sets the layout (:946); Bot() removes the stored layout (:972)
```

Untested:

- contact created without prepare
- prepared contact of an invitation link: NULL
- member contact: NULL
- business chat: later change of the address layout
- business chat: `x.info` profile update (`updateGroupProfileFromMember`)
- `x.grp.info` of an older owner
- address layout in link data after a user profile update (`updateProfile_`)
- Kotlin controls and theme writes: checked on desktop on 2026-10-21; no automated tests
- iOS theme writes: not compiled here
- Postgres down migration

## 11. Peer type `agent`

Implemented in master: #7738 (type and icons), #7751 (incognito profiles keep the peer type), #7752 (the MCP server sets it). Peer type and layout are independent: the layout of a chat with an agent follows section 4; `chatLayout` of the agent's address is the layout of the chat.

```
Types.hs (:753, :772, :778)                 ChatPeerType: CPTAgent, text "agent"
Kotlin ChatModel.kt (:2183-2211)            ChatPeerType.Agent, "agent", in text and serializer
Kotlin ChatModel.kt (:2246)                 peerTypeIcon: Bot ic_cube, Agent ic_agent, otherwise ic_account_circle_filled
iOS ChatTypes.swift (:227, :297)            ChatPeerType.agent, "agent"; peerTypeIcon: bot cube.fill, agent Image("agent") of SharedAssets.xcassets, otherwise person.crop.circle.fill
contact icon                                Contact.chatIconName (ChatModel.kt:1975), Contact.chatIcon (ChatTypes.swift:2581): peerTypeIcon
link alert icon                             ConnectPlan.kt:720, NewChatView.swift:1134: peerTypeIcon, business icon for business addresses
isBot                                       peerType bot only (ChatModel.kt:1970, ChatTypes.swift:2573): an agent contact has no commands menu and commands are not sent on tap (useCommands, menuCommands)
incognito                                   generateIncognitoProfile (Library/Internal.hs:1341): the incognito profile keeps the peer type of the user
set by                                      APICreateActiveUser and APIUpdateProfile with peerType "agent": packages/simplex-chat-mcp/src/serve.ts:89, :189-190
generated                                   bots/api/TYPES.md, types.ts, _types.py; docs/protocol/simplex-chat.schema.json peerType enum
tests                                       tests/ProtocolTests.hs: x.info with agent peer type
```

## Interfaces

```
ChatLayout                            Kotlin ChatLayoutMode: Classic, Terminal, Unknown(type); iOS: classic, terminal, unknown
GroupProfile.chatLayout               layout of the group, for every member
Contact.chatLayout                    layout of the address, stored when the contact is prepared
UIThemeEntityOverrides.chatLayout     user's choice for the chat or the profile
ChatInfo.activeChatLayout             Kotlin: section 4
core                                  /set layout #<group> classic|terminal|default
                                      /_set theme user <userId> <json>
                                      /_set theme <chatRef> <json>
Kotlin                                fun ThemeModeOverrides.layoutOnly(): ThemeModeOverrides?
                                      fun ThemeModeOverrides?.withChatLayout(chatLayout: ChatLayoutMode?): ThemeModeOverrides?
iOS                                   func ThemeModeOverrides.layoutOnly() -> ThemeModeOverrides?
```

## Depends on

- `2026-10-13-flat-chat-layout-ui.md`: reader of `ChatInfo.activeChatLayout`; owner of drawing and of the theme screens of terminal chats.
