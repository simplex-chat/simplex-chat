# Silent messages

Date: 2026-10-13.

A sender marks a message as `silent`: notifications are created only for unmarked messages, and the unread badge of a chat is gray while all its unread messages are silent.

References are to `/code/simplex-chat-4`, master. Haskell paths without a directory are in `src/Simplex/Chat/`.

## 1. Rules

```
silent item      chat item created from x.msg.new with "silent": true; the value is kept until the item is deleted
userMention      false for a silent item, also when the user is mentioned or replied to in it
notifying item   received item with showNotification and silent = false, in a chat with MFAll,
                 or with MFMentions and userMention
```

Counters of a chat (`ChatStats`):

```
unreadCount       all unread items, silent included       unchanged
unreadMentions    unread items with userMention           unchanged
unreadSilent      unread silent items                     new
unreadNotSilent   max 0 (unreadCount - unreadSilent)      computed in the apps
```

A silent item is treated as an item of a muted chat (MFNone) in:

- notifications
- sounds
- in-app banners
- the "unread" filter of the apps and the unread counters of tags (`unreadTag`)
- profile totals (`UserInfo.unreadCount`) and the desktop tray
- the iOS app icon badge

In every other place it is handled as any content item:

- the number of the chat list badge
- unread counters and unread navigation of the chat view
- counters and badges of support chats (`support_chat_items_unread`)
- the core chat list filter `CLQFilters {unread}` (`Store/Messages.hs:843, 954`)
- delivery receipts (`Library/Subscriber.hs:609-615`)

Chat list badge:

```
number    unreadCount
gray      chat setting is MFMentions or MFNone (as now), or unreadCount > 0 and unreadNotSilent = 0
primary   otherwise
"@"       unchanged
```

| Unread items of an MFAll chat | Badge |
|---|---|
| 3 silent | gray "3" |
| 2 silent, 1 other | primary "3" |
| 1 silent, 1 mention | "@" and primary "2" |
| `unreadChat` only | primary, empty |

## 2. Protocol and types

`Protocol.hs`:

```
MsgContainer (:691)
  silent :: Maybe Bool      -- new, after live; JSON "silent": true; omitted for Nothing
```

- `mcSimple` (`:718`): `silent = Nothing`.
- `silent` is a field of `x.msg.new` only. `x.msg.update` and the other events are unchanged.
- In a signed message `silent` is covered by the signature.
- Chat protocol version: unchanged.
- New function:

```haskell
eventHasNotification :: ChatMsgEvent e -> Bool
eventHasNotification = \case
  XMsgNew MsgContainer {silent = Just True} -> False
  ev -> hasNotification (toCMEventTag ev)
```

`Types.hs`, next to `BoolDef` (`:2311`):

```
newtype IntDef = IntDef Int     -- FromJSON omittedField = Just (IntDef 0)
```

`Messages.hs`:

```
CIMeta (:509)
  silent :: BoolDef             -- new, after userMention
ChatStats (:458)
  unreadSilent :: IntDef        -- new, after unreadMentions
mkCIMeta (:535)
  silent                        -- new parameter
```

## 3. Core: sending

SMP flags (`MsgFlags {notification}`):

```
Library/Internal.hs:2248   sendDirectContactMessages    any eventHasNotification events
Library/Internal.hs:2748   sendGroupSignedMessages_     any eventHasNotification events; group members and channel relays
Library/Internal.hs:2910   sendPendingGroupMessages     True, unchanged
```

A batch of a silent `x.msg.new` and a notifying event is sent with `True`; the receiving app decides for each item.

Item sent in parts: the rules are in `2026-10-13-message-parts.md` (3.2 and "Depends on"). `item_silent` is set from the head and is unchanged by parts.

Changed functions:

- `Controller.hs:1335` `ComposedMessage`: `silent :: Maybe Bool`; `FromJSON` (`:1934`): `v .:? "silent"`.
- `Library/Commands.hs:4898` `prepareMsgs` (direct): `silent` of the composed message is copied to the container when `Just True`.
- `Library/Internal.hs:212` `prepareGroupMsg`: one new parameter, `silent :: Bool`, passed at:
  - `Library/Commands.hs:4994`: the value of the composed message
  - `Library/Internal.hs:1551` (`sendHistory`): the value of the stored item
- `parts` of the message-parts plan is set after `prepareGroupMsg` by `splitMsgParts`, and in `sendHistory` in the returned container (that plan, 3.2 and 6).
- `Library/Internal.hs:2983` `NewSndChatItemData`: `silent :: Bool`:
  - `prepareSndItemsData` (`Library/Commands.hs:5061`): the value of the composed message
  - the other 4 constructions: `False`
- `Library/Internal.hs:3011` `saveSndChatItems.createItem`: `silent` is passed to `createNewSndChatItem` and `mkChatItem_`.
- `Library/Internal.hs:3081` `mkChatItem_`: new parameter `silent`:
  - `:3079` (`mkChatItem`): `False`
  - `:3414` (`createLocalChatItems`): `False`
- The other values of `ComposedMessage` are built with `silent = Nothing`:

```
Library/Commands.hs                                    7 constructions
Bot.hs                                                 2 constructions
apps/simplex-badge-service/src/BadgeService/Group.hs   1 construction
```

## 4. Core: receiving

- `Library/Internal.hs:3030` `saveRcvChatItem'`, with unchanged callers:
  - `silent` is True when `chatMsgEvent` is `XMsgNew` with `silent = Just True`
  - `userMention' = not silent && (…)` (`:3062`)
  - `silent` is passed to `createNewRcvChatItem` and `mkChatItem_`
- Items forwarded in groups and channels (`xGrpMsgForward`, `Library/Subscriber.hs:4042`) and history items are created by the same function.
- An item created by `x.msg.update` for a missing item (`Library/Subscriber.hs:2081, 2327`) has `silent = False`.
- `item_silent` is unchanged by edits and by marking as deleted.
- `sendHistory` (`Library/Internal.hs:1545-1552`):
  - unsigned item: re-encoded with `silent` of the stored item
  - signed item: sent as its stored bytes
  - signed item after an update: the stored bytes are the signed `x.msg.update` (`Library/Subscriber.hs:2380`, `Store/Messages.hs:2780`); the item of the new member has `silent = False`

## 5. Storage

Migration, working name `M20261013_silent_items`:

- The migration is named with the date of its merge; its name is ordered after every released migration name (`migrations = sortOn name`, `Store/SQLite/Migrations.hs:363`, `Store/Postgres/Migrations.hs:117`; simplexmq `migrationsToRun`, `Agent/Store/Migrations.hs:32-34`).
- It is registered in `Store/SQLite/Migrations.hs`, `Store/Postgres/Migrations.hs` and `simplex-chat.cabal`.

```
chat_items
  item_silent INTEGER NOT NULL DEFAULT 0     -- SMALLINT in Postgres
idx_chat_items_contacts                      -- dropped and created again
  item_silent                                -- new last column, after created_at
idx_chat_items_group_scope_stats_all         -- dropped and created again
  item_silent                                -- new last column, after user_mention
```

- Down migration: both indexes are created with their previous columns; the column is dropped.
- The migration name is added to `skipComparisonForDownMigrations` (`tests/SchemaDump.hs:116`).

`Store/Messages.hs`:

```
createNewChatItem_ :591          new parameter silent, written to item_silent for sent and received items
createNewSndChatItem :549        new parameter silent; False in SendMessageBroadcast (Library/Commands.hs:2679)
createNewRcvChatItem :565        new parameter silent
createNewChatItemNoMsg :584      False
createLocalChatItems             False (Library/Internal.hs:3412)
ChatItemModeRow :2283            item_silent added
getDirectChatItem :2696          i.item_silent selected
getGroupChatItem :3090           i.item_silent selected
getLocalChatItem :3203           i.item_silent selected
toDirectChatItem :2308           silent passed to mkCIMeta
toGroupChatItem :2369            silent passed to mkCIMeta
toLocalChatItem :1096            silent passed to mkCIMeta
ChatStatsRow :785                unreadSilent added
GroupStatsRow :791               unreadSilent added
findDirectChatPreviews_ :821     SUM(item_silent) AS UnreadSilent
findGroupChatPreviews_ :925      SUM(item_silent) AS UnreadSilent
findLocalChatPreviews_ :1017     the constant 0 is selected for unreadSilent
getContactUnreadCount_ :1398     returns (count, COALESCE(SUM(item_silent), 0)); used at :1357, :1366
getGroupUnreadCount_ :1739       returns (count, mentions, COALESCE(SUM(item_silent), 0)); used at :1703, :1718
```

- `Store/Profiles.hs:176` `getUsersInfo`: `AND i.item_silent = 0` in both queries.
- `chat_schema.sql` (SQLite, Postgres) and `chat_query_plans.txt` are written by the schema and query plan tests at the merge, never by hand.

## 6. API and CLI

`/_send` syntax (`Library/Commands.hs:6174`) is unchanged. `silent` is a property of each composed message:

```
/_send @2 json [{"msgContent": {"type": "text", "text": "working"}, "silent": true}]
```

- `silent` is accepted in `APISendMessages` (`Library/Commands.hs:749`) for contacts and groups, in every scope.
- Items created by any other command have `silent = False`.
- Responses and events:
  - `CIMeta.silent` in every chat item
  - `ChatStats.unreadSilent` in `/_get chats`, and in `/_get chat` with `initial=` or `around=`

## 7. Kotlin (Android, desktop)

Paths are in `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`.

`model/ChatModel.kt`:

```
CIMeta :3871                             val silent: Boolean = false
Chat.ChatStats :1502                     val unreadSilent: Int = 0
                                         val unreadNotSilent: Int get() = (unreadCount - unreadSilent).coerceAtLeast(0)
ChatInfo.ntfsEnabled(ci) :1809           !ci.meta.silent && ntfsEnabled(ci.meta.userMention)
Chat.unreadTag :1475                     All -> unreadChat || unreadNotSilent > 0
addChatItem :606-616                     silent RcvNew item: unreadSilent + 1, increaseUnreadCounter is skipped
markItemsReadInCurrentChat :904          third result: the count of silent items marked read
markChatItemsRead :884                   unreadSilent reduced by that count; 0 when itemIds == null
                                         decreaseUnreadCounter by the change of unreadNotSilent
decreaseCounterInPrimaryContext :940     new parameter silent: unreadSilent - 1
                                         decreaseUnreadCounter by the change of unreadNotSilent
                                         called at :675 (pItem), :725 (cItem), :777 (item)
clearChat :822                           decreaseUnreadCounter by unreadNotSilent
```

`views/chatlist/ChatPreviewView.kt:497`: `backgroundColor` is `secondary` when `disabled || showNtfsIcon || (n > 0 && chat.chatStats.unreadNotSilent == 0)`.

`ChatInfo.ntfsEnabled(ci)` is the condition of every notification path:

- `platform/NtfManager.kt:39` `notifyMessageReceived`, called for new items (`model/SimpleXAPI.kt:3041`) and for updated items (`chatItemUpdateNotify`, `:3762`)
- `model/SimpleXAPI.kt:3026, 3072`: totals of the other profiles
- the Android service, the periodic worker and desktop: the same event loop (`model/SimpleXAPI.kt:784`)

## 8. iOS

`SimpleXChat/ChatTypes.swift`:

```
CIMeta :4229
  public var silent: Bool = false
ChatStats :2382
  public var unreadSilent: Int = 0                                   -- also a parameter of init
  var unreadNotSilent: Int { max(0, unreadCount - unreadSilent) }
ChatInfo.ntfsEnabled(chatItem:) :2261
  !chatItem.meta.silent && ntfsEnabled(chatItem.meta.userMention)
```

`Shared/Model/ChatModel.swift`:

```
Chat.unreadTag :1444                              .all: unreadChat || unreadNotSilent > 0
UnreadCollector :1062                             deltas (unread, mentions, silent); applied when any delta differs from 0
changeUnreadCounter(_:by:unreadMentions:) :1162   new parameter unreadSilent; the profile total and the app icon badge change by count - unreadSilent
addChatItem :733                                  silent delta + 1
removeChatItem :850                               silent delta - 1
removeMemberItems :885                            silent delta - 1
markChatItemsRead :1041                           new parameter silentRead
decreaseUnreadCounter(user:chat:) :1175           .mentions: unchanged; every other setting: unreadNotSilent
totalUnreadCountForAllUsers :1194                 .all: unreadNotSilent
```

- `Shared/Model/SimpleXAPI.swift:1925` `apiMarkChatItemsRead`: new parameter `silentRead`.
- `Shared/Views/Chat/ChatView.swift:1809-1856`: `unreadItemIds` also returns the count of silent items.
- `Shared/Views/ChatList/ChatPreviewView.swift:246`: the background is `secondary` also when `s.unreadCount > 0 && s.unreadNotSilent == 0`.
- App and background refresh: `ntfsEnabled(chatItem:)` is the condition at:
  - `Shared/Model/NtfManager.swift:253` `notifyMessageReceived`
  - `Shared/Model/SimpleXAPI.swift:2636, 2682`: totals of the other profiles

NSE (`SimpleX NSE/NotificationService.swift`), one change in `receivedMsgNtf` (`:1016`):

```
.newChatItems, cItem.meta.silent    nil is returned after autoReceiveFile (:1021); the best attempt of the entity is unchanged
.newChatItems, other items          as now (:1024)
```

The count of messages received per push is unchanged: 4 for a connection (`allowedGetNextAttempts`, `:256, :545-549`). Silent items, updates and parts are counted.

| Queue of the connection at the push for N (N notifies, S silent) | Notification |
|---|---|
| up to 3 messages (silent items, their updates and parts), then N | N |
| 4 or more such messages, then N | "message received" (default `.connectionEvent`, `:1202-1210`) |
| N and S in one batch | N |
| S sent with `notification = True` (pending member) | "message received" |

## 9. Terminal

- `View.hs:2362` `viewReceivedMessage_`, `:2400` `viewSentMessage`: the suffix ` (silent)` is added in `appendLast`. Order of the suffixes: `msgVerifiedStr`, ` (silent)`, ` (incomplete)` of the message-parts plan.
- `Terminal/Output.hs:178` `chatEventNotification`: `sendNtf` is skipped for a silent item. `setActiveChat` and `markRead` (`:161`) are unchanged.

## 10. Libraries and docs

- `bots/src/API/TypeInfo.hs:174`: `"IntDef" -> ST TInt []`.
- Written by `tests/APIDocs.hs` (`:153`):
  - `bots/api/TYPES.md`
  - `packages/simplex-chat-client/types/typescript/src/types.ts`
  - `packages/simplex-chat-python/src/simplex_chat/types/_types.py`
- `packages/simplex-chat-nodejs/package.json`: `@simplex-chat/types` at the version with the written types.
- Library functions are unchanged. A bot passes `silent: true` in a `ComposedMessage` to:
  - `apiSendMessages` (`packages/simplex-chat-nodejs/src/api.ts:443`)
  - `api_send_messages` (`packages/simplex-chat-python/src/simplex_chat/api.py:196`)
- `bots/README.md`, new section "Silent messages":
  - progress messages are sent with `silent`, the completion message as a separate unmarked message
  - iOS shows the text of the completion message when at most 3 messages precede it in the queue
  - apps released before this plan notify for silent messages as for other messages
- `docs/protocol/simplex-chat.md:168`: `silent` in `x.msg.new`.
- `docs/protocol/simplex-chat.schema.json:117`: `msgContainer.optionalProperties.silent`.
- `apps/ios/spec` and `apps/multiplatform/spec`: the descriptions of the changed functions.

## 11. Compatibility

```
receiver app before this release    "silent" is ignored by the parser
                                    the item is unread and not silent
                                    the app notifies when it receives the message
                                    the push is skipped by the sender's flag
iOS before this release             the NSE shows a silent item as a message
                                    with 4 or more silent messages before N, the notification of the push for N has the text of the 4th message
relay or forwarding host before     signed message: forwarded as received bytes, "silent" kept
                                    unsigned message: re-encoded from the parsed message (Protocol.hs:1591), "silent" dropped
remote desktop                      CIMeta.silent and ChatStats.unreadSilent have JSON defaults (docs/CONTRIBUTING.md:155-158)
                                    host JSON is decoded by the controller core (Remote/Protocol.hs:185)
                                    newer desktop, older mobile: false and 0
                                    older desktop, newer mobile: the fields are ignored; the desktop notifies for silent items
bots                                ComposedMessage.silent is optional
                                    CIMeta.silent and ChatStats.unreadSilent are added to events
```

`silent` is accepted from every contact and group member. A relay or forwarding host can add or remove `silent` in unsigned messages only.

## 12. Tests

`tests/ProtocolTests.hs`:

- `x.msg.new` with `"silent":true` is encoded and decoded
- `eventHasNotification`: `False` for a silent `XMsgNew`; equal to `hasNotification` of the tag for other events

`tests/ChatTests/Direct.hs`; `ChatStats` is read with `getChatPreviews` through `withCCTransaction` (`tests/ChatTests/Utils.hs:671`):

- silent message sent with `/_send … json`: both sides print ` (silent)`; recipient: `unreadCount = 1`, `unreadSilent = 1`
- silent and other message: `unreadCount = 2`, `unreadSilent = 1`; after `/_read chat items` for the silent item: `unreadSilent = 0`; after `/_read chat`: both 0
- silent item sent with `live=on`, updated and edited: silent
- muted second profile: silent items are excluded from the `/users` total

`tests/ChatTests/Groups.hs`, `tests/ChatTests/ChatRelays.hs`:

- silent message with a mention and silent reply to the user: `userMention` false, `unreadMentions = 0`
- silent message forwarded by the host to a member without a direct connection: silent
- history: an unsigned silent item, and a signed silent item without updates, are silent for a new member
- channel through a relay: the subscriber's item is silent

The test of a silent item sent in parts is in the message-parts plan (section 11).

Manual checks, Android, desktop, iOS:

1. Silent message with the app open, in background and closed: received with notification, sound and banner suppressed.
2. Chat list: the badge table of section 1; a chat with only silent unread items is excluded from the "unread" filter and the tag counters.
3. Profile total, desktop tray and iOS app icon badge: after silent messages are received, after they are read, after an unread silent item is deleted.
4. SMP flag, iOS, app suspended: 3 silent messages, then one other message: one notification with its text; 6 silent messages, then one other: one notification "message received".
5. MFMentions group: a silent reply to the user's message is received with the notification and the "@" suppressed.

## Interfaces

```
field           MsgContainer.silent
                ComposedMessage.silent
                CIMeta.silent
                ChatStats.unreadSilent
                NewSndChatItemData.silent
wire name       silent in x.msg.new
type            IntDef
core function   eventHasNotification
column          chat_items.item_silent
migration       M20261013_silent_items (working name)
app function    CIMeta.silent
                ChatStats.unreadSilent
                unreadNotSilent
terminal text   " (silent)"
```

## Depends on

This plan is implemented independently of the other plans and released before `2026-10-13-message-parts.md`. Changed together with other plans:

- `2026-10-13-message-parts.md` ("Depends on"): the combined types of the two plans; the rules for a silent item sent in parts (3.2, 6).
- `2026-10-13-agent-profile-chat-layout.md` ("Depends on"): the combined column list of `getGroupChatItem` (`Store/Messages.hs:3090`).

## Open questions

1. Silent items: counted in the badge number (gray badge); excluded from the "unread" filter, tag counters, profile totals and the app icon badge; without notification also for a mention or a reply. Recommended: yes to the three.
2. `silent` is accepted from any sender, without a limit by peer type or group preference. Recommended: any sender.
3. iOS push: 4 messages are received per push; with 4 or more silent messages, live updates or parts queued before the completion message its notification text is "message received". Recommended: accept in this release.
4. Release grouping: three releases: (1) silent messages, agent profile, flat layout, read more, `Strong` with rendering part 1; (2) block parsing of the core and rendering part 2; (3) message parts. Recommended: yes.
5. Is a silent item marked in the chat view? Recommended: later; the terminal prints ` (silent)`.
6. Is a chat version added so that a bot detects receivers that support `silent`? Recommended: later; apps before this release notify as now.
