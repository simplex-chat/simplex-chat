# Group updates that show a message as new

Status: proposed; U1-U4 decided by the user on 2026-11-21, P1 open. One 7.2 core pull request with `2026-11-21-edit-window-preference.md`, stacked on message priority (#8195, `ep/message-priority` at `1f3fff669`). Code references: master `20ee7cb14`; priority code by name, from `ep/message-priority`.

The user's words:

> the challenge is that edit doesn't push the message to the bottom. This is a common problem that we cannot make updates that also update message time - I think we need to make it a hidden protocol option, finally, it seems rather simple, and you recent update improvement should help it. So update protocol message gets a flag to move the message to the bottom. This way, on every update, the message would jump to the bottom. Many bots would benefit from it, as currently we have to delete and recreate, and this pattern used in quite a few bots - the downside is jumping view and the loss of history. ... My bet is that update that updates time (also based on server time) is simple - this way sender cannot manipulate it. And we won't expose it in UI anyway.

> adding notification makes asNew precise, and it's simply atomic update that deletes and creates new item while preserving history and showing edited icon

## 1. What users see

Before:
- A bot keeps a status message current by edits. Later messages move it up the chat.
- To keep it last, a bot deletes it for everyone and posts it again: each repost alerts, a deleted item is left in place when full deletion is off, and the edit history of the message is lost. The Claude Code plugin replaces its list of sessions every 23 hours this way ([main-group.ts:174](../packages/simplex-chat-mcp/src/main-group.ts#L174)); the support bot reposts each card on every change ([cards.ts:257-287](../apps/simplex-support-bot/src/cards.ts#L257-L287)).

After, in groups and channels:
- A bot updates a message with `as_new=on`. For every member the item becomes a new message: it is shown last, with the time of the update, unread, and the group moves to the top of the chat list.
- It alerts as a new message would, by the group's notification setting (U1). With priority -1 it alerts nobody; with priority 1 it alerts as a mention, when the group's priority preference allows the sender (#8195).
- The item keeps its ID, reactions, replies to it and its edit history, and shows the edited mark when its text changed.
- Only the sender of a message updates it as new, as an edit, within the limits of `2026-11-21-edit-window-preference.md` (U2).
- In direct chats and note folders the flag is ignored: the update is a plain edit.
- The apps have no control for it (U4).
- An app or a core of an earlier release applies the update as an edit in place.

## 2. Protocol

```
x.msg.update params
  msgId          base64url          as now
  content        msgContent         as now
  mentions       object             optional, as now
  ttl            integer            optional, as now
  live           boolean            optional, as now
  scope          msgScope           optional, as now
  asGroup        boolean            optional, as now
  priority       integer            optional, #8195: 1 raises the item; with asNew: the priority of the new message
  asNew          boolean            optional, new; sent only as true, only to groups
```

```
Protocol.hs
  XMsgUpdate                asNew :: Maybe Bool          new last field, after priority of #8195
  appJsonToCM, XMsgUpdate_  asNew <- opt "asNew"
  chatToAppMessage          "asNew" .=? asNew
  eventPriority             XMsgUpdate {asNew = Just True, priority = Just p} -> limitPriority p     new case
  eventHasNotification      EQ -> raisesPriority ev || isAsNew ev || hasNotification (toCMEventTag ev)
  isAsNew                   new: ChatMsgEvent e -> Bool; XMsgUpdate {asNew = Just True}
  positional uses           Library/Commands.hs, Library/Subscriber.hs, tests/ProtocolTests.hs, tests/ChatTests/Groups.hs,
                            the sites of the priority field of #8195
```

- SMP notification flag of an update as new: priority -1, unset; 0 or absent, set; 1, set.
- The chat protocol version is unchanged. Parsers of earlier releases read optional parameters with `.:?` (`opt`, Protocol.hs:1422) and skip unknown keys.
- In a signed update `asNew` is covered by the signature.
- `docs/protocol/simplex-chat.md` (:185) and `docs/protocol/simplex-chat.schema.json` (:474): `asNew` with its receiver rule.

## 3. Time of an item updated as new

Each receiver sets the time from its own time source; the message contains only the flag.

```
time source       received directly          broker time of the message (metaBrokerTs, Subscriber.hs:1036)
                  forwarded by a host/relay  fwdBrokerTs of the forward (Subscriber.hs:4029), as for new items
                  sender's own copy          local time of the command
new item time     end of the second of the time source: floor(t) + 0.999 s
applied           only when later than the item's time
```

```haskell
asNewItemTs :: ChatItemTs -> UTCTime -> Maybe ChatItemTs
asNewItemTs itemTs t
  | ts > itemTs = Just ts
  | otherwise = Nothing
  where
    ts = addUTCTime 0.999 (roundDownToSecond t)
```

- `asNewItemTs` is in `Library/Internal.hs`, next to `roundDownToSecond` (:3090).
- Group items are ordered by `item_ts, chat_item_id` (`getChatItemIDs`, Store/Messages.hs:1541); direct chats and note folders by `created_at, chat_item_id` (:1570, :1576), where a new `item_ts` changes no position.
- Broker times have whole seconds: SMP encodes `systemSeconds` (simplexmq `Encoding.hs:146`). This code creates items with whole seconds: received items with the broker time (Internal.hs:3118), sent, internal and local items with `roundDownToSecond` (Internal.hs:3082, :3427, :3483; Commands.hs:2729). An item updated as new is therefore placed after every item of its second and earlier; an item created later in the same second is placed before it.
- SQLite stores `item_ts` as `YYYY-MM-DD HH:MM:SS[.fff]` (sqlcipher-simple `utcTimeToBuilder`), ordered as text; Postgres as `timestamptz`.
- Among items created by this code, a fractional second in `itemTs` marks an item updated as new.

## 4. Priority of an item updated as new

An update as new carries the priority of the new message, as `x.msg.new`; an update without `asNew` keeps the priority rules of #8195.

```
                    update without asNew (#8195)              update as new
sender sends        priority 1 when raising, else none        sndPriority highAllowed of UpdatedMessage.priority, as a new message
sender's item       priority 1 when raised                    the sent priority, 0 when none
receiver's item     priority 1 when raised and allowed        limitPriority p; 1 only when rcvPriorityAllowed cd, as saveRcvChatItem'
userMention         set when raised                           mention or reply of the updated content || priority > 0
SMP flag            set when raising                          unset for -1, set otherwise
apps notify         when raised (isRaised)                    when shown as new (isAsNew), unless silent
```

- `highAllowed` is `groupFeatureUserAllowed SGFMsgPriority gInfo` of #8195; a silent priority (-1) is allowed to every member.
- API: `/_update item #1 5 as_new=on json {"msgContent": {"type": "text", "text": "..."}, "mentions": {}, "priority": -1}`, with `UpdatedMessage.priority` of #8195.

```mermaid
flowchart LR
  U["update as new, priority p"] --> L{p}
  L -->|"-1"| S["unread, silent:<br>no SMP flag, no alert"]
  L -->|"0 or none"| N["unread, alert as a new message<br>by the chat's notification mode"]
  L -->|"1, sender allowed"| H["unread, high priority:<br>alert as a mention"]
  L -->|"1, sender not allowed"| N
```

## 5. Core: sending

`APIUpdateChatItem` (Controller.hs:428), new field `asNew :: Bool` after `liveMessage`:

```
/_update item <chatRef> <chatItemId>[ live=on][ as_new=on] json <updatedMessage>
parser (Commands.hs:6872)   <*> (" as_new=" *> onOffP <|> pure False), after liveMessageP
positional uses             Commands.hs:2747, :2751                        False
                            apps/simplex-badge-service/src/BadgeService/Group.hs:345   False
```

Group branch of the handler (Commands.hs:850); the direct (:825) and note folder (:886) branches ignore `asNew`:

```
asNew_     = if asNew then asNewItemTs (chatItemTs' ci) currentTs else Nothing
priority   if isJust asNew_ then sndPriority highAllowed priority_ else the raise of #8195
condition  changed || itemLive == Just True || raise || isJust asNew_
event      XMsgUpdate ... priority (justTrue (isJust asNew_))
edited     changed && itemLive /= Just True                           -- as in #8195
store      after updateGroupChatItem, with meta.priority set:
             forM_ asNew_ $ \ts -> setChatItemTs db ci' ts and updateChatTsStats db cxt user cd currentTs Nothing
response   CRChatItemUpdated with the item at its new time
```

- The edit limits of `2026-11-21-edit-window-preference.md` apply. `setChatItemTs` also sets `item_original_ts = COALESCE(item_original_ts, item_ts)`, and the limits are measured from it, so an update as new keeps the limits of the original send.

## 6. Core: receiving

```mermaid
sequenceDiagram
  participant B as Bot core
  participant S as SMP server
  participant R as Relay or host core
  participant C as Member core
  participant A as Member app
  B->>B: item time := asNewItemTs(itemTs, now)<br>chat_ts := now
  B->>S: x.msg.update asNew, priority p<br>SMP flag unless p = -1
  S->>R: message, broker time t1
  R->>R: item time := asNewItemTs(itemTs, t1)
  R->>S: forward, fwdBrokerTs t1, SMP flag unless p = -1
  S->>C: forward, push to iOS when flagged
  C->>C: item time := asNewItemTs(itemTs, t1)<br>status rcv_new, priority, chat_ts := now
  C->>A: chatItemUpdated: chatItem, prevChatItem
  A->>A: itemTs differs: item last, chat first<br>unread + 1, alert unless silent
```

`updateCI` of `groupMessageUpdate` (Subscriber.hs:2398), new parameter `asNew :: Maybe Bool`, passed from `XMsgUpdate` at :1091 and :4068. `messageUpdate` of direct chats (:590) ignores the field, as now.

```
asNew_     = if asNew == Just True
                && isNothing itemDeleted           -- deleted, moderated, blocked by admin: in place
                && not (memberBlocked sender)      -- as for mentions (:2345)
             then asNewItemTs (chatItemTs' ci) brokerTs
             else Nothing
priority   with asNew_: section 4; otherwise the raise of #8195
condition  changed || itemLive == Just True || requested || isJust asNew_
edited     changed && itemLive /= Just True                                  -- as in #8195
store      after updateGroupChatItem with meta.priority and userMention set: forM_ asNew_ $ \ts ->
             setGroupChatItemAsNew db ci' ts
             updateChatTsStats db cxt user cd now stats
               stats   main scope: Nothing
                       member support scope: memberChatStats of a new item (Internal.hs:3136),
                       with unread 1 only when the item was read
event      CEvtChatItemUpdated: the item at its new time with status rcv_new, the previous item; the chat info returned by updateChatTsStats
delivery   as now: Just context whenever the condition holds, so an update as new without a change of text is forwarded
           the SMP flag of the forward follows the rule of a forwarded new message: msgHasNotification (Subscriber.hs:4261),
           based on eventHasNotification in #8195
```

`Store/Messages.hs`, new:
```
setGroupChatItemAsNew :: DB.Connection -> ChatItem 'CTGroup 'MDRcv -> ChatItemTs -> IO (ChatItem 'CTGroup 'MDRcv)
  UPDATE chat_items
  SET item_original_ts = COALESCE(item_original_ts, item_ts), item_ts = ?, item_status = ?, item_viewed = 0
  WHERE chat_item_id = ?
  item_status = rcv_new; returns the item with meta.itemTs and meta.itemStatus set
setChatItemTs :: DB.Connection -> ChatItem 'CTGroup 'MDSnd -> ChatItemTs -> IO (ChatItem 'CTGroup 'MDSnd)
  UPDATE chat_items SET item_original_ts = COALESCE(item_original_ts, item_ts), item_ts = ? WHERE chat_item_id = ?
```
`item_priority` and `user_mention` are written by `updateGroupChatItem_` of #8195. `item_original_ts` is the column of `2026-11-21-edit-window-preference.md`. The new queries are added to `tests/chat_query_plans.txt` by the query plan test.

State of a received group item, read from the last row; `-` is unchanged:

```
event                                    item_ts             chat_ts  item_status  item_viewed  priority  versions
x.msg.new at t0                          t0                  now      rcv_new      0            0         -
read                                     t0                  -        rcv_read     1            -         -
x.msg.update, text changed               t0                  -        rcv_read     1            -         +1
x.msg.update asNew p=0 at t1, changed    floor(t1) + 0.999   now      rcv_new      0            0         +1
read                                     -                   -        rcv_read     1            -         -
x.msg.update asNew p=-1 at t2, same      floor(t2) + 0.999   now      rcv_new      0            -1        -
x.msg.update asNew, not later            -                   -        -            -            -         as an edit
```

- An update of an item missing on the receiver creates the item at the time source, unread, as now (Subscriber.hs:2331): it is last already.

## 7. Effects in groups

```
area                        effect of an update as new                               change
chat view order             item last: ORDER BY item_ts, chat_item_id (Store/Messages.hs:2476-2501)   none
pagination, navigation      before/after/around by item_ts (:2487, :2500, :1801-1828)                none
chat preview                last item by item_ts (:918)                              none
chat list order             chat_ts (:887)                                           updateChatTsStats
unread counts               +1 when the item was read: counted by item_status (:928); silent and high counters of #8195 by priority   setGroupChatItemAsNew
member support counters     as for a new item                                        updateChatTsStats stats
first unread, opening       item_viewed = 0: not the opening pivot (:1744); the first unread when no earlier one (:1735)   setGroupChatItemAsNew
notifications               as a new message, by priority (section 4)                Protocol.hs, apps, NSE, terminal
history for new members     ORDER BY item_ts (:3795); forwarded with fwdBrokerTs = itemTs in whole seconds (Internal.hs:1550)   none
channel web preview         ORDER BY item_ts (:3815); x.msg.update marks the content changed (Subscriber.hs:1139)   none
replies to the item         resolved by shared_msg_id (:702-748)                     none
forward of the item         a new item with the content                              none
expiration by chat TTL      item_ts <= cutoff AND created_at <= cutoff (:3662): kept a TTL after the update   none
disappearing messages       deleteAt of an item read before the update continues     none
edit and delete limits      from item_original_ts, kept across updates as new        2026-11-21-edit-window-preference.md
item info                   "sent at" shows the time of the update; versions keep the earlier texts and times   none
terminal view               viewItemUpdate (View.hs:870) as for an edit                none
```

- Bots: each update as new is a new unread message for every member, alerting by default. A bot that updates a status message often sends it with priority -1: the message stays last and unread, without alerts, and adds at most one unread item per member at a time.
- Abuse: an update as new alerts as a post does, and leaves at most one unread item per message; posting is unrestricted as well. Moderators block the member (updates are then applied in place), remove the member or delete the message.

## 8. Relays, hosts and channels

```
relay or host of this release     applies the update as new to its copy; forwards it with its SMP flag, an update without a change of text included
relay or host before              signed update: forwarded as received bytes, asNew kept (Protocol.hs:1597)
                                  unsigned update: encoded again from the parsed message, asNew dropped: an edit in place
                                  an update as new without a change of text: not forwarded (condition :2400 of this master)
subscriber                        time source fwdBrokerTs of the relay; every subscriber of one relay gets the same time
```

## 9. Apps and the notification extension

The apps detect an update as new when `chatItem.meta.itemTs != prevChatItem.meta.itemTs` in `chatItemUpdated`: `isAsNew(from:)` next to `isRaised(from:)` of #8195 (iOS `ChatTypes.swift`, Kotlin `ChatModel.kt`). Every other emitter of the event (Internal.hs:712, :857, :890; Subscriber.hs:421, :440, :729, :1232, :1581, :1610, :2055, :2133, :2411; Commands.hs:4969) keeps `itemTs`. The unread counters already follow the previous and the new item (#8219, with the silent and high counters of #8195).

```mermaid
flowchart TD
  E[chatItemUpdated] --> M{isAsNew}
  M -->|no| P[as now: preview if last,<br>replace loaded item]
  M -->|yes| L[chat list: preview by the rule of addChatItem,<br>pop the chat]
  L --> R[loaded items: remove the item,<br>insert it last as a new item]
  P --> U[unread counters from prev and new item]
  R --> U
  U --> N{chat added, isRaised or isAsNew}
  N -->|yes| A[notifyMessageReceived:<br>showNotification excludes silent,<br>ntfsEnabled applies the chat's mode]
  N -->|no| X[no alert]
```

```
iOS       Shared/Model/ChatModel.swift
            applyChatItemUpdate (:800)      moved branch
            addChatItem (:701)              its group preview rule (:716-729) used by both
            removal                         reversedChatItems.remove and chatState.itemsRemoved, as removeChatItem (:875)
            insertion                       _upsertChatItem (:837)
            pop                             popChatCollector.throttlePopChat (:736)
          Shared/Model/SimpleXAPI.swift
            chatItemUpdate (:3241)          notify = isRaised || isAsNew, in both profiles, as #8195 for isRaised
          SimpleX NSE/NotificationService.swift
            receivedMsgNtf, chatItemUpdated case of #8195
                                            isRaised || isAsNew, with ntfsEnabled and showNotification: messageReceived
                                            otherwise noNtf: an edit and a silent update as new show nothing
Kotlin    common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt
            applyChatItemUpdate (:702)      moved branch, in the primary and the secondary context
            addChatItem (:582)              its preview rule (:598-613) used by both
            removal                         chatItems.removeAllAndNotify, as removeChatItem (:769)
            insertion                       addToChatItems (:560, :564), before TEMP_LIVE_CHAT_ITEM_ID as addChatItem
            pop                             popChatCollector.throttlePopChat (:631)
          common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt
            chatItemUpdateNotify (:3847)    raised || asNew, in both profiles, as #8195 for raised
Terminal  src/Simplex/Chat/Terminal/Output.hs
            chatEventNotification           msgNtf for an update as new, as #8195 for a raise
specs     apps/ios/spec/state.md, apps/ios/spec/services/notifications.md,
          apps/multiplatform/spec/state.md, apps/multiplatform/spec/services/notifications.md
```

- iOS push: the SMP flag of an update as new wakes the notification extension as a new message does. An update without `asNew` and without a raise has no flag; when the extension receives it during another push, it shows nothing.
- An older app with a newer core, as with remote desktop: the update is shown as an edit in place, counted unread, without an alert, until the chat is opened again.
- An older iOS app receives the push of an update as new; its extension handles `chatItemUpdated` as no notification, as for a raise of #8195.

## 10. API and libraries

```
bots/src/API/Docs/Commands.hs:104   OnOffParam "as_new" "asNew" (Just False) after live
                                    description: "Update message. In groups, as_new=on makes the message new: last in the chat, unread and notified by its priority, with the time of the update set by each member; ignored in other chats."
generated by tests/APIDocs.hs       bots/api/COMMANDS.md
                                    packages/simplex-chat-client/types/typescript/src/commands.ts
                                    packages/simplex-chat-python/src/simplex_chat/types
packages/simplex-chat-nodejs/src/api.ts:488             apiUpdateChatItem: new asNew = false, with the priority parameter of #8195
packages/simplex-chat-python/src/simplex_chat/api.py:241  api_update_chat_item: new as_new=False, with the priority parameter of #8195
```

- A core of an earlier release refuses `as_new=on` as a command syntax error.
- The plugin and the support bot adopt the option in later pull requests, after a `simplex-chat` package with this core is published: the plugin's list with `as_new=on` and priority -1.

## 11. Composition

```mermaid
flowchart LR
  API["/_update item #g ... as_new=on json {priority}"] --> CMD["Commands.hs APIUpdateChatItem, group branch"]
  CMD --> MT["Internal.hs asNewItemTs"]
  CMD --> PR["Protocol.hs sndPriority, eventHasNotification"]
  CMD --> ST["Store/Messages.hs setChatItemTs<br>updateChatTsStats"]
  CMD --> P["Protocol.hs XMsgUpdate priority, asNew"]
  P --> SUB["Subscriber.hs groupMessageUpdate.updateCI"]
  SUB --> MT
  SUB --> RP["Internal.hs rcvPriorityAllowed"]
  SUB --> SN["Store/Messages.hs setGroupChatItemAsNew<br>updateChatTsStats"]
  SUB --> EV["chatItemUpdated"]
  EV --> APPS["iOS and Kotlin applyChatItemUpdate, notifications"]
  EV --> NSE["iOS NSE receivedMsgNtf"]
```

## 12. Merge order

```mermaid
flowchart LR
  M[master] --> PRI["#8195 ep/message-priority"]
  PRI --> THIS["ep/update-to-bottom: as new and edit limits"]
  THIS -->|"after #8195 merges:<br>git rebase --onto master"| M2[master]
```

The branch is stacked on `ep/message-priority` from its first code commit, and its pull request is merged after #8195 (P1).

## 13. Tests

Core, each failing on master and on `ep/message-priority`: the parser refuses `as_new=on`.

```
tests/ProtocolTests.hs     x.msg.update with "asNew":true and with "asNew":true, "priority":-1 encoded and decoded;
                             without them decoded to Nothing
                           eventHasNotification of an update as new: False for -1, True for 0 and 1
tests/ChatTests/Groups.hs  "update as new shows the item last and unread"
                             bob: "status"; alice: /_read chat items #1 <status>; cath: "hi"
                             bob: /_update item #1 <status> as_new=on text status 2, sent without a pause after "hi"
                             alice, bob, cath: /_get chat #1 count=2 -> "hi", "status 2"
                             alice: item_status of "status 2" is rcv_new (SQL, as in testDirectMessageUpdatePrevItem)
                             recorded: eventsFixture "updates/group-as-new", alice with markRead = False
                           "silent update as new": priority -1: last, rcv_new, item_priority -1
                             recorded: eventsFixture "updates/group-as-new-silent"
                           "high priority update as new": admin with priority 1: item_priority 1, user mention;
                             a member below the priority role: item_priority 0
                           "update as new without a change of text": not marked edited; last for all
                           "update as new of a blocked member": cath blocks bob; bob's update applied in place for cath
tests/ChatTests/Direct.hs  "update as new is ignored in a direct chat": bob "status", alice "hi", bob as_new=on;
                             both: /_get chat @2 -> "status 2", "hi"
tests/ChatTests/ChatRelays.hs
                           "owner updates a channel post as new": last and unread for a subscriber
tests/ChatTests/Utils.hs   stableJSON (:1043): itemTs is 1970-01-01T00:00:00Z plus the fraction of the recorded second
                             existing fixtures are unchanged: their itemTs have whole seconds
```

Apps, on `tests/fixtures/api/updates/group-as-new.json` and `group-as-new-silent.json` (new "status", new "hi", "status" read before the update and updated as new as "status 2"):

```
test                                              checked                                                      apps
item updated as new is last                       "status", "hi" -> "hi", "status 2"; preview "status 2"         both
item not loaded is inserted last                  chat opened after "hi": "hi" -> "hi", "status 2"              both
update as new alerts as a new message             "status" marked read in the model (markChatItemsRead);        both
                                                    unread 1 -> 2; notifications "status", "hi", "status 2"
silent update as new alerts nobody                the silent fixture: unread 1 -> 2; notifications "status", "hi"   both
another profile alerts and counts it              the other profile's unread 1 -> 2; notification "status 2"    both
NSE shows an update as new, not an edit           receivedMsgNtf: messageReceived for group-as-new; noNtf for   iOS
                                                    the edit of updates/direct-edit and for group-as-new-silent
```

Kotlin: `common/src/desktopTest/kotlin/chat/simplex/app/ChatItemUpdateTest.kt`. iOS: `SimpleX Unit Tests/ChatItemUpdateTests.swift`.

## 14. Options

```
option                                                     cost
A. asNew in x.msg.update, receiver time (recommended)      one optional field, one command parameter, two UPDATEs, one app branch per platform, NSE case
B. new event x.msg.move                                    a new tag with its codecs, parser, encoder, isForwardedGroupMsg, signableContent and handlers; an edit with a move is 2 messages; earlier releases report "unsupported message"
C. receiver creates a new row for the item                 new chat_item_id: reactions, versions, mentions, files and delivery statuses moved; app item identity lost
D. new ordering column                                     migration, 8 Postgres indexes with item_ts replaced, 26 uses of item_ts in Store/Messages.hs reviewed
E. time set by the sender                                  the sender places the item at any time
F. delete and post again, as now                           an alert per repost, deleted items left, edit history lost
```

```
merge order                                         cost
stacked on #8195, merged after it (recommended)     waits for #8195; priority composed in one change
before #8195, without priority                      every update as new alerts until #8195; bots that update often alert on each change;
                                                      #8195 then changes eventHasNotification, the NSE case and the app notification rule again
```

```
time of A                              consequence
whole second of the time source        an update in the second of a later item leaves the item above it
end of the second (recommended)        an item created later in the same second is placed above the item
```

## 15. Questions

U1. Decided: an update as new is a new message: last, unread, alerted by the group's notification setting and its priority, with the chat at the top of the list; name `asNew`.

U2. Decided: only the sender updates a message as new, as an edit; receivers apply an update as new of a blocked member or of a deleted item in place.

U3. Decided: the edit and delete limits with their role exception are a group preference, in the same core pull request (`2026-11-21-edit-window-preference.md`).

U4. Decided: the apps have no control for it; the option is documented in the protocol docs and the bot API with its clients.

P1. Merge order: this pull request stacked on #8195 and merged after it, so that an update as new carries its priority from the first release. Recommended: yes. Alternative: merged first without priority, every update as new alerting until #8195.
