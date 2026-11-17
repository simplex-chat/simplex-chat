# App tests of item updates on core recordings

Repository: simplex-chat/simplex-chat.
Branch: `ep/update-prev-item` (#8219).
Base: master.

The user's words:

> to merge 8219, you need to add UI tests using mocked API responses, in the same way we did in merged PRs 8234 and 8241

## Composition

```mermaid
flowchart LR
  CT["core tests<br>direct update, mention edit, missed call"] -->|"eventHook: recordItemEvent"| EV["item events<br>newChatItems, chatItemUpdated"]
  EV -->|"eventsFixture"| F["tests/fixtures/api/updates"]
  F --> KT["ChatItemUpdateTest<br>desktop"]
  F --> IT["ChatItemUpdateTests<br>iOS, hosted by the app"]
  KT -->|"processReceivedMsg"| KM["ChatModel: chats, loaded items,<br>counters, notifications"]
  IT -->|"processReceivedMsg"| IM["ChatModel: chats, loaded items,<br>counters"]
```

## Recording

`tests/ChatTests/Utils.hs`:

| Function | Mechanics |
|---|---|
| `recordItemEvent events evt` | appends `newChatItems` and `chatItemUpdated` to `events`, encoded as `chatRecvMsg` encodes them |
| `eventsFixture fixture events steps` | clears `events`, runs `steps`, writes `events` as one JSON array |
| `stableJSON` | `itemTs` replaced by 1970-01-01<br>`itemSharedMsgId` replaced by its key |

The recipient of each recording has `markRead = False`, so items stay unread, as in an app before the chat is opened.

| Fixture | Core test | Events of the recipient |
|---|---|---|
| updates/direct-edit | updated item event has the previous item | new "hello"<br>"hello" edited to "hello there"<br>new "how are you"<br>"hello there" edited to "hello, alice" |
| updates/direct-recreated | the same test, after the recipient deletes the item locally | new "hello again", recreated from the update |
| updates/group-mention | should send updated item with mentions added and removed by edit, the recipient's group notifications set to mentions | new "hello"<br>edited to mention the recipient<br>edited without the mention |
| updates/missed-call | mark call invitation missed when the caller ends the call | new pending call<br>the call updated to missed |

## Tests

Each test lists the chat of the first event without items, unless the test is about a chat not in the list; it receives the events with `processReceivedMsg` and compares the model.

| Test | Fixture | Checked | Apps |
|---|---|---|---|
| edit replaces loaded item | direct-edit | the open chat has "hello there" in place of "hello"; the preview is "hello there" | both |
| update of item not loaded inserts nothing | direct-edit | the chat opened after "hello" has no loaded items after the edit; the preview changes only while the edited item is the last one | both |
| counters follow previous and new item | group-mention | unread, mentions and the profile's unread: 1 0 1, 1 1 1, 1 0 1 | both |
| mention edits in another profile change its unread count | group-mention | the other profile's unread: 1 after the mention, 0 after its removal; Kotlin: no notification | both |
| update of a chat not in the list adds it | direct-edit, the edit only | the preview is "hello there"; Kotlin: one notification | both |
| recreated item is counted | direct-recreated | the preview is "hello again"; 1 0 1; Kotlin: one notification | both |
| edit in another profile does not notify | direct-edit | one notification, of "hello"; the other profile's unread is 1 | Kotlin |
| missed call does not notify | missed-call | the preview is the missed call; no notification | Kotlin |
| missed call in another profile does not notify | missed-call | no notification; the other profile's unread is 1 | Kotlin |
| response and event decode to their fields | direct-edit | the update event has `chatItemUpdate` with the previous item; the response of sample data has `chatItem` | Kotlin |

Notifications are checked in Kotlin only: iOS `NtfManager` has no hook.

The response of `/_update item` is not recorded: the sender's item in it has the delivery status of the moment, which `SENT` and receipts change asynchronously.

## Missed calls

`chatItemUpdateNotify` notifies only in the active profile, when the update added the chat. Its missed call branch is removed: `notifyMessageReceived` displays nothing for `RcvCall`, whose `showNotification` is false, since #5328. The spec line of `notifyMessageReceived` states the same.

## iOS

- `singleFieldJSON` converts tagged objects except `MsgContent`, which the core encodes with `type` in both JSON forms, recognized by its tags.
- other types tagged with `type` in both forms are converted: `MsgChatLink`, `LinkContent`, `MsgScope`, `MsgReaction` and `ChatBotCommand`; a fixture with them needs a rule by position, as the tag `group` of `MsgChatLink` is also a tag of `ChatInfo`.
- `fixtureJSON`, `singleFieldJSON` and `waitFor` are at file scope of `NameAlertsTests.swift`, shared by both test classes.
- the debounced unread collector is awaited by `assertCounts`.
