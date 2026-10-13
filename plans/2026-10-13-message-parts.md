# Message parts

A text longer than one chat message is sent as several parts and shown as one chat item; for an update of the text only the changed parts are sent.

References are to `/code/simplex-chat-4`, master. Paths without a directory are in `src/Simplex/Chat/`.

## 1. Limit of one message

15602 bytes is the limit of every encoded chat message in every chat, and of the head and of every part of a split text. With PQ a body over 13380 bytes is delivered compressed.
```
creation    encodeChatMessage maxEncodedMsgLength (Library/Internal.hs:2353); over 15602: SELargeMsg (Store/Messages.hs:242)
batching    batchMessages maxEncodedMsgLength (Library/Internal.hs:2493)
delivery    connection without PQ: a body of up to 15602, as encoded (Library/Internal.hs:2578)
            connection with PQ: a body over 13380 is compressed; still over 13380: CEException "large compressed body" (:2495-2501)
            a contact connection has PQ after the first received message (:1842-1845)
relay       forwarded batch of up to 15763 (Library/Subscriber.hs:4234, Messages/Batch.hs:90)
receiver    up to 65536 bytes after decompression (Protocol.hs:944, 1028)
```

## 2. Protocol and types

`Protocol.hs`:
```
msgPartsVersion              next free chat version (23); currentChatVersion (:96) is raised
maxMsgParts = 8
maxQuotedTextLength = 2000   -- escaped bytes (3.1)
```

The head is the `x.msg.new` or `x.msg.update` message of an item. Part 1 is its `content.text`. The text of an item with parts is the field `text` of its `MsgContent`: the received parts, concatenated in order.

New field `parts :: Maybe Int`, the number of parts; absent = 1:
```
MsgContainer (:691, mcSimple :718)                with silent :: Maybe Bool of 2026-10-13-silent-messages.md
                                                  and new partsId :: Maybe SharedMsgId: random ID of the parts of a new item
XMsgUpdate (:463, parser :1421, encoder :1514)    last field; positional uses:
                                                  Library/Commands.hs:811, 845, 1474
                                                  Library/Subscriber.hs:589, 769, 1088, 1339, 4048
```

New event `x.msg.part`, tag `XMsgPart_`:
```
XMsgPart
  msgId :: SharedMsgId      -- part of a new item: partsId of its head; part of an update, live part: ID of the item
  partNo :: Int             -- 2 .. maxMsgParts
  text :: Text
  live :: Maybe Bool        -- applied on receipt to a live item
  scope :: Maybe MsgScope
  asGroup :: Maybe Bool

sites         ChatMsgEvent :460
              CMEventTag :1077
              StrEncoding (CMEventTag e) :1139
              StrEncoding ACMEventTag :1200
              toCMEventTag :1263
              appJsonToCM :1407
              chatToAppMessage :1500
properties    hasNotification XMsgPart_ = False (:1338)
              hasDeliveryReceipt XMsgPart_ = False (:1349)
              isForwardedGroupMsg XMsgPart {} = True (:528)
              signableContent XMsgPart_ = False (:1374)
              assertDirectAllowed: XMsgPart_ -> False (Library/Commands.hs:5826)
```

Messages of a change, in order:
```
new item              parts 2..n; x.msg.new
edit                  changed parts; x.msg.update
live update, head     changed parts; x.msg.update with live = true
live update, other    changed parts with live = true
end of live item      changed parts; x.msg.update
```

The head of a live update is sent in each of these cases:
- the content with part 1 as its text differs from that of the item
- the mentions differ
- the number of parts is lower
- all parts are unchanged

`Messages.hs`, `CIMeta` (`:509`) and `mkCIMeta` (`:535`): new `partsMissing :: BoolDef`, set when a part of 2..n is not received; next to `silent` of the silent messages plan.

## 3. Sender

### 3.1. Split and update

New function `splitMsgParts` in `Library/Internal.hs`:
```
input         chat version range; head event with the whole text; parts of the item, for an update
output        head event with part 1 and parts; texts of parts 2..n; numbers of the changed parts
encoded       encodeChatMessage (Protocol.hs:986) with an item ID of the generated length
escaped t     length of t as a JSON string of aeson, without the quotes
headBudget    maxEncodedMsgLength − length of the encoded head with text "", parts = maxMsgParts and, in x.msg.new, partsId
partBudget    maxEncodedMsgLength − length of the encoded x.msg.part with text "" and partNo = maxMsgParts
cut b t       longest prefix of t with escaped ≤ b, in one pass over the characters
part 1        cut headBudget text
part k ≥ 2    cut partBudget (rest of the text)

update item newText:
  old = text of the item cut by item_parts              -- one part for NULL
  k = largest i < length old such that old[1..i] is a prefix of newText and, for i ≥ 1, the new head with old[1] fits
  new = old[1..k] ++ parts from k + 1 of (newText without old[1..k])
  changed = [i | i <- [2 .. length new], i > length old || new[i] /= old[i]]
```
- The text is the field `text` of `MsgContent`. `MCUnknown` is sent as one message.
- A text is split when the head with the whole text is over the limit.

Errors:
```
SELargeMsg                                    headBudget is negative
                                              the text has more than maxMsgParts parts
                                              MCUnknown over the limit
                                              direct chat: the peer version is below msgPartsVersion
CECommandError "long text: signed message"    the message is signed (signMsgs, Library/Commands.hs:4937; reuseSign, :846)
CECommandError "long text: relay <name>"      channel: relay <name> of the recipients is below msgPartsVersion
```

### 3.2. Sending

Text is split in the three places of this section. Every other sender of content sends one message, as now.

New item, `sendContactContentMessages` (`Library/Commands.hs:4877-4878`) and `sendGroupContentMessages_` (`:4961-4962`); the items of a command are sent in order:
1. Containers are built as now (`prepareMsgs` `:4898`, `prepareGroupMsg` `Library/Internal.hs:212`) and passed to `splitMsgParts`. `partsId` of an item with parts is generated by `getSharedMsgId` (`Library/Commands.hs:5159`).
2. Consecutive items without parts are sent by one call, as now.
3. Item with parts: its parts are sent by one call of `sendDirectContactMessages` (`Library/Internal.hs:2244`) or `sendGroupMessages` (`:2699`), then its head by the existing call, before the messages of the next item.
4. The results of the calls are joined in item order; in a group the `GroupSndResult` values are merged.
5. An error of a part is the error of its item (`prepareSndItemsData`, `Library/Commands.hs:5061`): its head is skipped, and the part messages created for it are deleted (new `deleteSndMessages`).

Update, `APIUpdateChatItem` (`Library/Commands.hs:811-818`, `:845-854`): changed parts are sent by `sendDirectContactMessages` or `sendGroupMessages`; the head, when sent, by the existing calls (`:812`, `:847`).

All three places:
- Recipients of parts are the recipients of the head.
- SMP flag of a call (`Library/Internal.hs:2248`, `:2748`), with `eventHasNotification` of the silent messages plan:
```
parts only           False
head x.msg.new       True; False with silent
head x.msg.update    False
```
- `item_parts` and the links of the part messages are written by `setChatItemParts` in the transaction of the item:
```
NewSndChatItemData (Library/Internal.hs:2983)        new fields itemParts, partMsgIds; silent of the silent messages plan
saveSndChatItems.createItem (:3011)                  setChatItemParts
APIUpdateChatItem (Library/Commands.hs:813, :848)    setChatItemParts
```
- A version is recorded when a head is sent (`addInitialAndNewCIVersions`, `Library/Commands.hs:816, 851`).
- Item status is set from the delivery events of head messages: part messages are excluded in `getChatItemIdsByAgentMsgId` (`Store/Messages.hs:2522`), the query of every status update.
- Quote: the quoted text is cut to `maxQuotedTextLength` with `cut`, after `updatedMentionNames` (`Library/Internal.hs:225`, `Library/Commands.hs:4911`). `CIQuote.formattedText` is parsed from the cut text. The quote of a report is cut in the same way.

## 4. Receiver

`Library/Subscriber.hs`. Dispatch of `x.msg.part`; when a condition is false, the part is dropped with `messageError`:
```
direct processEvent (:586)     handler
group processEvent (:1080)     inside checkSendAsGroup and memberCanSend with p.scope, as XMsgUpdate (:1088-1091)
processForwardedMsg (:4041)    inside memberCanSend, as XMsgUpdate (:4048-4049)
```

### 4.1. Pseudocode

Key of a pending part: chat (connection of the contact, or group), author (member of the message; none for a forwarded channel message), p.msgId, partNo. Key of a head, `key h`: `partsId` for `x.msg.new`, the item ID for `x.msg.update`.
```
on x.msg.part p:
  drop unless 2 ≤ p.partNo ≤ maxMsgParts and byte length of p.text ≤ maxEncodedMsgLength
  if p.live == Just True:
    item_ <- find item (chat, author, p.msgId, p.asGroup)          -- as x.msg.update, :2091, :2342-2347
    case item_ of
      Just item | item.live && allowed item p -> setPart item p
      _ -> drop
  else:
    save pending p                 -- the pending part with the same key is deleted
    pending parts of the chat and author over 4 × maxMsgParts: the oldest are deleted

on head h with parts = n > 1:      -- x.msg.new, x.msg.update
  drop unless n ≤ maxMsgParts, h is unsigned, byte length of h.text ≤ maxEncodedMsgLength
  ps = pending parts (chat, author, key h) with partNo ≤ n
  parts = [ps[k] <|> part k of the item | k <- [2..n]]            -- Nothing: not received
  text = h.text ++ concat (catMaybes parts)
  msgParts = (lengths of Just h.text : parts, messages of ps)
  existing handler of h, with text as the content text and msgParts in RcvMessage
  delete pending parts (chat, author, key h) that are left

on head h without parts:
  existing handler of h; item_parts of an updated item is set to NULL

setPart item p:
  item_parts is extended to p.partNo with parts that are not received
  part p.partNo = p.text
  content text = received parts concatenated
  emit chatItemUpdated

allowed item p:
  the item content is CIRcvMsgContent
  msgVerified of the item is other than MVSigned
  group: p is sent by the author of the item; channel item: by an owner (as :2350-2356)
  group: prohibitedSimplexLinks is False for the resulting text (as :2299)
```

### 4.2. Functions and transactions

`RcvMessage` (`Messages.hs:1179`): new field `msgParts`; `Nothing` in `createNewRcvMessage` (`Store/Messages.hs:344`); set at dispatch of a head with parts.
```
save pending          saveRcvMsgPart, one transaction: replaced part message deleted; row inserted; limit applied
setPart               one transaction: content, text, item_parts (getChatItemParts, setChatItemParts), link of the part message
head, pending parts   getRcvMsgParts, before the handler
head, item created    saveRcvChatItem' (Library/Internal.hs:3034): setChatItemParts with msg.msgParts
head, item updated    transactions at :2082, :2098, :2328, :2373: setChatItemParts with msg.msgParts
```
- `setChatItemParts` is called when the stored content is the message content. In that transaction `item_parts` is written, the merged part messages are inserted in `chat_item_messages`, and their pending rows are deleted.
- A pending part is deleted by deletion of its message; the row is deleted by cascade.
- The existing handlers of the head events are called with the whole text. Content checks, mentions, moderation and blocking are applied to it, as now.
- A version is recorded when a head is applied (`:2100`, `:2375`), as now. Only the content text and `item_parts` are changed by `setPart`.
- `partsMissing` of an item: `item_parts` has a part that is not received.
- Messages of one chat are processed one at a time, under the lock of the send commands (`:146`; `Library/Commands.hs:752, 758, 799, 824`).
- `cleanupManager` (`Library/Commands.hs:5909`), new step: pending parts older than one day are deleted (`deleteOldRcvMsgParts`).

`Markdown.hs`, `hasObfuscatedSimplexLink` (`:195`): applied to windows of `2 × maxEncodedMsgLength` characters with a step of `maxEncodedMsgLength`.

## 5. Storage

Migration `M20261013_message_parts` (working name), SQLite and Postgres, registered in both `Migrations.hs` and `simplex-chat.cabal`. The migration is named with the date of its merge, and its name is ordered after every released migration name, the migration of the silent messages plan included (`Store/SQLite/Migrations.hs:363`, `Store/Postgres/Migrations.hs:117`; simplexmq `Agent/Store/Migrations.hs:32-34`). Down migration: the column, the index and the table are dropped.
```
chat_items
  item_parts TEXT                    -- JSON list of part lengths in characters; null: not received; NULL: one part

rcv_msg_parts                        -- pending parts
  message_id                         -- PRIMARY KEY; FK messages ON DELETE CASCADE
  shared_msg_id                      -- ID of the item
  part_no

idx_rcv_msg_parts_shared_msg_id
  shared_msg_id
```
- Chat, author and time of a pending part: `connection_id`, `group_id`, `author_group_member_id`, `created_at` of its message (`Store/Messages.hs:336-342`).
- `item_parts` is read with the item, next to `item_silent` of the silent messages plan; the combined change of `getGroupChatItem` is listed in "Depends on" of the agent profile plan:
```
ChatItemModeRow          Store/Messages.hs:2283
item queries             :2711, :3105, :3218
item row conversions     :1096, :2308, :2369
```
- `item_parts` and `item_live` are set to NULL in the UPDATE of `updateGroupChatItemModerated` (`:2859`) and of `updateGroupCIBlockedByAdmin` (`:2931`).
- `chat_schema.sql` and `chat_query_plans.txt` are regenerated by the schema and query plan tests.

## 6. Groups, channels, relays

Delivery tasks of a forwarding member (relay, host), `Library/Subscriber.hs`; the result of `processEvent` (`:1069`) is a list of tasks (`:1058`, `:1133-1144`):
```
live part applied       one task with the context of the item (infoToDeliveryContext, as :2384)
pending part            none
head accepted           tasks of the merged part messages in part order, then the task of the head, with the context of the head
part or head dropped    none
```
- `channelContentChanged` is signalled for an applied live part (`Library/Subscriber.hs:1136`).
- Channel: a text is split when every relay of the recipients (`getGroupRecipients`, `Library/Internal.hs:1939`) supports `msgPartsVersion`.
- `batchDeliveryTasks1` (`Messages/Batch.hs:84`): the batch is closed at the first task that does not fit it (`:95`); task order is kept. A task over the batch limit is reported as large (`:90`), as now.

History, `sendHistory` (`Library/Internal.hs:1414`):
```
getGroupHistoryItems (Store/Messages.hs:3737)
  result      item_parts of each item
  omitted     items with a part that is not received
  limit       the newest items with at most 100 parts in total; an item without parts counts as one

processContentItem (Library/Internal.hs:1528; item_parts is passed by itemForwardMsgs, :1450), item with parts
  text        the stored text; updatedMentionNames (:1536) is skipped
  head        prepareGroupMsg (:1551) with part 1 as the content text, live of the item
              and silent of the item (silent messages plan); parts = n and a new partsId are set in the returned container
  head size   over the limit: encoded without the quote; still over: the item is omitted
  parts       XMsgPart 2..n with that partsId, cut by item_parts; VMUnsigned, envelope msgId = Nothing, as the XMsgFileDescr events (:1559)
  order       parts, head, file descriptions
```

## 7. API and CLI

- Text of up to `maxMsgParts` parts is accepted by every command that sends or updates content through the places of 3.2.
- `CIMeta.partsMissing` is added to every chat item. Commands, the other types and events are unchanged. An item has the whole text in every response and event.
- Receiver events: `newChatItems` once for a new item; `chatItemUpdated` for each applied head and each applied live part.
- An item with `partsMissing` is skipped in forwarding, as a deleted item (`forwardMsgContent`, `Library/Commands.hs:5104`). It is quoted as any item.

## 8. Kotlin, iOS, terminal

Kotlin paths are in `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/`, iOS paths in `apps/ios/`.
```
Kotlin      model/ChatModel.kt:3871 CIMeta: val partsMissing: Boolean = false
            views/chat/item/CIMetaView.kt:83 (meta text), :155 (reserved space): for partsMissing, StatusIconText(ic_warning, Color.Red) after the edited mark
iOS         SimpleXChat/ChatTypes.swift:4229 CIMeta: partsMissing: Bool, false when absent
            Shared/Views/Chat/ChatItem/CIMetaView.swift:127: for partsMissing, statusIconText("exclamationmark.triangle", red) after the edited mark
            NSE: unchanged; parts and the head are counted in the 4 messages of a push (SimpleX NSE/NotificationService.swift:256, 545-549)
terminal    View.hs:2362 viewReceivedMessage_, suffixes in appendLast, in this order: msgVerifiedStr;
            " (silent)" of the silent messages plan; " (incomplete)" for an item with partsMissing
```

## 9. Libraries and docs

- `CIMeta.partsMissing`: the output of the docs generator is regenerated (`bots/api/TYPES.md` and the TypeScript and Python type files of `packages/`).
- `packages/simplex-chat-nodejs`: the whole text is passed to the core by `apiSendMessages` and `apiUpdateChatItem` (`src/api.ts:443, 482`). Added to `tests/api.test.ts`: one item for a text of several parts.
- `docs/protocol/simplex-chat.md`: `parts` and `partsId` in `x.msg.new` (`:168`); `parts` in `x.msg.update` (`:185`); new section `x.msg.part`.
- `docs/protocol/simplex-chat.schema.json`: `parts` and `partsId` in `msgContainer` (`:113`); `parts` in `x.msg.update` (`:470`); new `x.msg.part`.

## 10. Compatibility

```
contact below msgPartsVersion          sender: SELargeMsg, as now; the version of an upgraded contact is recorded from its next message (Library/Internal.hs:1832-1840)
group member or subscriber below       receives the parts and the head; part 1 is shown; one error event per part (Library/Subscriber.hs:1132, 4064)
member reached through a host below    the head is encoded again without parts (Protocol.hs:1591); part 1 is shown; parts are dropped by the host
relay below                            sender: CECommandError with the relay name; its version is recorded from each of its messages to the owner (Library/Internal.hs:1326, 2932)
recipient queue                        item of n parts: n of its 128 messages (simplexmq Server/Env/STM.hs:273); a full queue is handled as now (Library/Subscriber.hs:1755-1762, 4407)
remote desktop                         partsMissing = false from an older mobile; bodies up to 64 MB and 512 MB (Remote/Protocol.hs:277-281)
bots                                   one newChatItems event with the whole text; CIMeta.partsMissing is added
iOS push                               new item of more than 4 parts: the default notification text; the item is created when the app receives the head
```

## 11. Tests

Unit tests:
- `tests/ProtocolTests.hs`: `x.msg.part`, and `parts` in `x.msg.new` and `x.msg.update`, are encoded and decoded
- `tests/MessageBatching.hs`: `batchDeliveryTasks1` with a task over the remaining space of the batch; task order is kept
- `tests/MarkdownTests.hs`: `hasObfuscatedSimplexLink` with repeated `simplex:/contact#/?` of `maxMsgParts × maxEncodedMsgLength` characters

`tests/ChatTests/Direct.hs`:
- a 40 KB text: one item in one `newChatItems` event; the rows of `messages` are deleted with the item
- a silent item of 3 parts: `eventHasNotification` is False for every message; the item of the recipient is silent; `item_silent` is unchanged by a later live part
- uniformly random printable ASCII of 40 KB with PQ on is delivered; on failure the limit of a part on a PQ connection is `maxEncodedMsgLengthPQ`
- 4-byte characters and control characters at each part boundary
- a live text extended over two part boundaries; an update of one part is one message
- a live update with a changed part 1: one `chatItemUpdated` with the new text
- reply to a 40 KB item: the quote is 2000 escaped bytes
- a peer at the previous version: `SELargeMsg`
- a text of more than `maxMsgParts` parts: `SELargeMsg`
- a head or a part above the byte limit from a modified sender is dropped
- 6 items of 8 parts forwarded by one command are received complete

`tests/ChatTests/Groups.hs`:
- a long text forwarded by the host to a member without a connection to the sender
- a member whose recorded version is below the real one receives the whole text
- a live item of several parts
- group with the `signMessages` preference: `CECommandError`; a part for a verified item is dropped
- a part from an observer and a part from a member other than the author are dropped
- reply to a long item whose mentioned member was renamed
- a head received with one of its parts lost: `partsMissing`; the item is skipped by forwarding and history
- a message of another member with the `partsId` of pending parts as its envelope `msgId`: the item is created with all parts

`tests/ChatTests/ChatRelays.hs`:
- two relays: a long post is received in full; a new subscriber receives it in history
- a live post of several parts
- a part with `asGroup` from a subscriber is dropped by the relay
- 6 posts of 8 parts forwarded by one command are received complete
- a relay at the previous version: `CECommandError` with its name

Manual, on Android, desktop and iOS: an `agent` contact with `chatLayout: flat` sends silent messages with `live`, then a completion message of several parts with a heading, a list, a code block and a table. Expected: one notification with the completion text; the row is a teaser; the blocks are shown by the viewer.

Released before this plan:
- the linear parser of the block-markdown plan (section "Limits and cost")
- the read-more plan, 2.7 of the flat layout plan and part 2 of the rendering plan

Measured before release, on a mid-range phone: the time of one part applied to an item of `maxMsgParts` parts is under 50 ms in the core and under 100 ms of app redraw; above it `maxMsgParts` is lowered.

## Interfaces

```
protocol    msgPartsVersion
            x.msg.part, XMsgPart
            MsgContainer.parts
            MsgContainer.partsId
            XMsgUpdate.parts
            maxMsgParts
            maxQuotedTextLength
core        splitMsgParts
API, apps   CIMeta.partsMissing
            text of a sent or received item: at most maxMsgParts × maxEncodedMsgLength = 124,816 bytes
            note folder items are created without a length check (Library/Commands.hs:5109-5122)
storage     chat_items.item_parts
            rcv_msg_parts
            migration M20261013_message_parts (working name)
```

## Depends on

`2026-10-13-silent-messages.md`, combined result:
```
MsgContainer          silent, parts, partsId; Nothing in mcSimple
CIMeta, mkCIMeta      silent, partsMissing
ChatItemModeRow       item_silent, item_parts
NewSndChatItemData    silent, itemParts, partMsgIds
prepareGroupMsg       one new parameter, silent (silent messages plan); part 1 and parts are set after it by splitMsgParts
sendHistory           prepareGroupMsg (Library/Internal.hs:1551) with silent and live of the item and part 1 as the text
SMP flag              any eventHasNotification of the events of a call; the table of 3.2
receiver              item_silent is set from the head; unchanged by parts
```
- `2026-10-13-block-markdown.md`: the bound of the parsed text is its linear parser and `maxSegments`. The window change of `hasObfuscatedSimplexLink` (4.2) is named in its "Interfaces".
- `2026-10-13-message-read-more.md`: rows are bounded by the teaser; the chat list preview, the compose context item and the notification text by `msgPrefix`.
- `2026-10-13-flat-chat-layout-ui.md`: flat rows are bounded by its section 2.7; the `partsMissing` mark is part of `CIMetaView` in both layouts.
- `2026-10-13-block-markdown-rendering.md`: rows are bounded by `MSG_MAX_BLOCKS` block views (rendering plan, 2.2); the `partsMissing` mark is drawn in the added meta line.

## Open questions

1. "Read more" in the flat layout: a flat row above 500 lines (16,000 characters on a phone) is a teaser, against "CLI layout does not need read more". Recommended: accept; smaller texts are drawn in full.
2. Maximum text of one item: 8 parts (124,816 bytes, 16 items per recipient queue) or 32 parts (499,264 bytes, 4 items). Recommended: 8.
3. iOS push: 4 messages are received per push; with 4 or more silent messages, live updates or parts queued before the completion message its notification text is "message received". Recommended: accept in this release.
4. Long text is refused for a contact below `msgPartsVersion`, for signed messages (groups with the `signMessages` preference, signed channel posts) and for a channel with a relay below the version. Recommended: accept; parts of signed messages in a later chat version.
5. An item with a part that is not received is shown with the received text and a red warning icon in the meta; it is skipped by forwarding and history. Recommended: yes.
6. Name of the second layout. Recommended: `flat`.
7. Release grouping: three releases, message parts last. Recommended: yes.
