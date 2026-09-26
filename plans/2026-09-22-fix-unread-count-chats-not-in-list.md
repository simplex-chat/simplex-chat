# Fix unread count for a chat added to the list from a received message

## Problem

With many chats (more than 5000), the unread filter in the chat list misses a few chats
that do have unread messages. They are in the list, with the right preview and
timestamp, but they render as read: no unread badge, and the filter does not show them.
As reported, on desktop, their unread state appears only after restarting the app.

## Solution summary

In the branch of `addChatItem` that creates a chat from a received event, count that
item as unread instead of leaving the new chat's stats at their defaults, and raise the
profile unread counter and the chat tag counts with it — the same bookkeeping the branch
for chats already in the list does. The count is a floor until the list is reloaded,
because the event does not say how many items in that chat are unread; asking the
backend for the true count at this point was tried and rejected (see Fix).

## Cause

### 1. The filter reads the in-memory list, and nothing else

With the search field empty, `filteredChats` keeps the open chat unconditionally, and
otherwise keeps a chat that is not deleted, is not a contact card, and for which
`filtered(chat, activeFilter)` holds — for the unread filter, `chat.unreadTag`
(`ChatListView.kt`). `unreadTag` is `unreadChat || unreadCount > 0` for a chat with
notifications on, `unreadChat || unreadMentions > 0` when it is set to mentions only,
and `unreadChat` alone otherwise. All of it is read from the `Chat` object in
`ChatModel.chats`; nothing in the path consults the database. So a chat whose in-memory
`chatStats` says zero cannot be shown as unread, whatever the database holds.

### 2. The list is loaded with at most 5000 chats

Both clients send the command with no pagination argument — `is ApiGetChats -> "/_get
chats $userId pcc=on"` (`SimpleXAPI.kt`, and the same command from `AppAPITypes.swift`)
— so `pagination` parses as `Nothing` and the handler fills in the default:

```haskell
ChatConfig {maxChats} <- asks config
let pagination' = fromMaybe (PTLast maxChats) pagination
```

`maxChats` is 5000 for the mobile and desktop clients (`Simplex/Chat/Mobile.hs`; the
same default in `Simplex/Chat.hs`, where the CLI can override it with `--max-chats`).
`getChatPreviews` applies that count twice — `LIMIT ?` per chat type, and `take count`
over the merged list — so the UI is given the 5000 most recently active chats and
nothing else. Direct, group and note-folder previews order by `chat_ts DESC`; contact
requests and pending connections order by `updated_at DESC`.

Measured against a test profile of 5104 chats:

```
TOTAL CHATS RETURNED: 5000
Bot present: False
first 3: ['Dummy5100', 'Dummy5099', 'Dummy5098']   last 3: ['Dummy103', 'Dummy102', 'Dummy101']
```

Independently of the count, the contact preview queries select only contacts matching
`ct.user_id = ? AND ct.is_user = 0 AND ct.deleted = 0 AND ct.contact_used = 1`, which is
a second way for a chat to be missing from the list while it exists in the database.

### 3. A chat created from an event is created as read

When an item arrives for a chat that is in the list, `addChatItem` carries the unread
state and the counters that depend on it:

```kotlin
if (cInfo.groupChatScope() == null || cInfo.groupInfo_?.membership?.memberPending == true) {
  // selection of newPreviewItem elided
  val wasUnread = chat.unreadTag
  chatsContext.chats[i] = chat.copy(
    chatItems = arrayListOf(newPreviewItem),
    chatStats =
    if (cItem.meta.itemStatus is CIStatus.RcvNew) {
      increaseUnreadCounter(rhId, currentUser.value!!)
      chat.chatStats.copy(unreadCount = chat.chatStats.unreadCount + 1, unreadMentions = if (cItem.meta.userMention) chat.chatStats.unreadMentions + 1 else chat.chatStats.unreadMentions)
    } else
      chat.chatStats
  )
  updateChatTagReadInPrimaryContext(chatsContext.chats[i], wasUnread)
}
```

When the chat is *not* in the list, the same function built it from the event alone:

```kotlin
addChat(Chat(remoteHostId = rhId, chatInfo = cInfo, chatItems = arrayListOf(cItem)))
```

`Chat.chatStats` defaults to `ChatStats()` — `unreadCount = 0`, `unreadMentions = 0`,
`unreadChat = false` — and neither the profile unread counter nor the chat tag counters
are updated. So the chat is inserted at the top of the list with the right preview,
while the app believes it has no unread messages. The database is not involved and
remains correct.

Nothing downstream compensates: in the `CR.NewChatItems` handler, the branch taken when
the event is for the active user and host leaves all unread bookkeeping to
`addChatItem`, which on this path does none.

### 4. Why a few chats, and why until restart

Only the **first** message into such a chat is lost. Once the entry exists, the next
message finds it and increments 0 to 1, so the chat becomes visible again. What stays
invisible is the set of chats that received exactly one message since the list was
loaded.

Any reload repairs the value, because `/_get chats` returns the database's counts, and
by then the chat's `chat_ts` is recent enough to be inside the 5000. Android reloads on
`Lifecycle.Event.ON_START` when the chat is running (`SimplexApp.kt`) and iOS when the
app becomes active after being suspended (`SimpleXApp.swift`), so on mobile the wrong
value typically survives only until the app is next foregrounded. Desktop has no
lifecycle-driven reload — it reloads on a user or remote-host switch, on starting the
chat, on creating a further profile, on leaving a remote-controller session and after
changing the message TTL, none of which happens by simply using the app.

## Fix

Give the new chat the unread state of the item it is created from, and do the
bookkeeping that depends on it, in the branch of `addChatItem` that runs when the chat
is missing:

```kotlin
val unread = cItem.meta.itemStatus is CIStatus.RcvNew
// in a secondary context ChatView shows unread count of the chat it was opened for, it must not be set here
val chatStats = if (unread && secondaryContextFilter == null) {
  Chat.ChatStats(unreadCount = 1, unreadMentions = if (cItem.meta.userMention) 1 else 0)
} else {
  Chat.ChatStats()
}
val newChat = Chat(remoteHostId = rhId, chatInfo = cInfo, chatItems = arrayListOf(cItem), chatStats = chatStats)
if (unread) {
  increaseUnreadCounter(rhId, currentUser.value!!)
  updateChatTagReadInPrimaryContext(newChat, wasUnread = false)
}
addChat(newChat)
```

`wasUnread = false` is exact rather than assumed: the chat did not exist in this list a
moment ago, so it contributed nothing to the tag counters.

The two counter updates are not optional extras. `markChatItemsRead` subtracts
unconditionally — `decreaseUnreadCounter(..., chat.chatStats.unreadCount - unreadCount)`
and then `updateChatTagReadInPrimaryContext(..., wasUnread)` — so setting `unreadCount`
to 1 without raising the profile counter and the tag counts would make opening that chat
subtract from totals that were never added to. `users[i].unreadCount` is not clamped.

The counters are raised before `addChat`, which inserts the chat and only then suspends
in `throttlePopChat`, so the counters and the insertion complete before anything can
suspend; the in-list branch likewise writes its counters before it pops the chat.

`secondaryContextFilter == null` on the stats keeps the change out of the secondary
contexts (group reports, member support). Their `chats` list is never loaded from the
backend, and every path that inserts into it does so with default stats, so this change
would be the first to insert a chat there with a positive unread count. `ChatView` reads
that count out of the list its context owns, for the unread divider and the two floating
unread counters, so without the guard a main-scope message in the group whose reports or
member support view is open would raise all three inside that view. The guard is only
needed on the stats: `increaseUnreadCounter` and `updateChatTagReadInPrimaryContext` are
already no-ops in a secondary context — the first through the
`changeUnreadCounterInPrimaryContext` it delegates to — so they are called the same way
the in-list branch calls them.

### The count is one, not the chat's true count

The event says a message arrived; it does not say how many items in that chat are
unread. The count set here is therefore a floor, corrected by the next reload of the
list — and, for a chat the user opens, before that: `processLoadedChat` overwrites the
stats with the backend's, since `ChatPagination.Initial` is a pagination for which the
core computes them.

Asking the backend for the real count at this point was tried and rejected. `/_get chat`
with `ChatPagination.Initial(1)` does return real stats, but using it here mixes an
absolute count from the backend with the relative `+1` that the in-list branch applies
to the same chat: a second message arriving during the round trip is added on top of the
fetched value — deterministically on iOS, where `UnreadCollector` applies its delta a
second later — so two items can show as three. It also costs a priority store
transaction per message into a cold chat, whose `NavigationInfo` count over every item
after the first unread one is computed and then discarded, issued concurrently per chat
in exactly the scenario this bug is about: thousands of chats coming back online at
once. A cheap stats-only command in the core would make this approach viable; that is a
separate change.

## Scope / non-goals

- **The 5000 limit is not addressed.** With more chats than that, the UI still silently
  loads only the most recently active 5000: the rest are not shown, not searched, and
  missing from the in-memory tag counts, until one of them receives a message. Recorded
  as GAP-10.
- **`addPresetChatTags` is not called** for the newly added chat, as it is not today, so
  the preset tag counts stay low by one for such a chat until they are recomputed —
  which only `getUserChatData` does, not every chat-list reload.
- **A non-mention message in a mentions-only chat still does not light the filter**, and
  a muted chat never does, because `unreadTag` ignores `unreadCount` in those cases. The
  profile counter is a different matter: exactly as on the in-list branch, it is raised
  for any received item, including one in a muted chat, whereas the core excludes muted
  chats when it computes that counter for a profile, and excludes non-mentions in
  mentions-only groups. On iOS that `+1` cannot be taken back, because
  `decreaseUnreadCounter(user:chat:)` subtracts `unreadMentions` for a mentions-only
  chat, which is zero for a non-mention.
- **`unreadChat`** (a chat marked unread by hand) is not recovered: the received-item
  event carries no such flag, so a chat the user marked unread on another device is
  still added without it.
- **Counters still drift on paths this change does not touch.** Deleting or leaving a
  chat that has unread messages leaves the profile counter and `unreadTags` high, because
  `removeChat` decrements neither — it removes the chat's preset tags, but not those two;
  a cleared mention can leave `unreadMentions` behind. Both are recorded as GAP-08.
  Before this change such a chat contributed nothing to either counter, so the drift is
  newly reachable here even though the defect is not new.
- **A message that arrives while the profile or remote host is being switched can be
  counted twice.** `changeActiveUser_` refills `users` from `listUsers` and only then
  reloads the chat list, so during the reload every chat id misses in the stale list and
  takes this branch. If the core had already committed the item before `listUsers` ran,
  the `+1` is a second count. Master was inert here only because it dropped the count
  entirely.
- **Marking such a chat read from the list leaves the profile counter high.** The count
  shown is a floor, so `markChatItemsRead` decrements by it while `apiChatRead` clears
  every unread item in the chat: a chat with older unread messages, marked read from the
  list without ever being opened, leaves the profile counter too high by the difference
  for the session. Opening the chat instead is correct, because `processLoadedChat` replaces
  the stats with the backend's first. Closing this needs the chat's true count, which
  only the core has.
- **`upsertChatItem` has the same defect and is left alone.** Its missing-chat branch
  also creates the chat with default stats, and is reached when an item *update* arrives
  for a chat outside the window — an edit of an unread incoming message, a file status
  change. It is not fixed here because the counter semantics differ: the item it carries
  already exists, so the core counted it in `getUsersInfo` already, and raising the
  profile counter there would over-count. That branch needs the stats without a counter
  increment, which neither existing branch does.
- **The guard covers this add path only.** When the chat is already in the list,
  `addChatItem` writes stats into the primary list without a context check, using an
  index it took from the current context's list. That is existing behaviour, recorded as
  GAP-09.
- `addChatItem`'s branch for items in a group chat scope, which adds the chat with no
  items, is unchanged. It keeps an existing asymmetry: for a chat already in the list
  that function also counts a scoped item when the user is a pending member, while this
  branch tests only the scope.

## iOS

The same defect with the same shape — `addChat(Chat(chatInfo: cInfo, chatItems:
[cItem]))` in `addChatItem`, with `chatStats: ChatStats = ChatStats()` as the default.
iOS needs no new function: the fix adds the same call the in-list branch of
`addChatItem` already makes a few lines above.

```swift
addChat(Chat(chatInfo: cInfo, chatItems: [cItem]))
if case .rcvNew = cItem.meta.itemStatus {
    unreadCollector.changeUnreadCounter(cInfo.id, by: 1, unreadMentions: cItem.meta.userMention ? 1 : 0)
}
```

So on iOS the chat is inserted with default stats and the count is queued: the collector
accumulates per chat id behind one debounced subject and flushes into
`ChatModel.changeUnreadCounter(_:by:unreadMentions:)`, which updates the chat's stats,
calls `ChatTagsModel.shared.updateChatTagRead` and raises the profile counter — which on
iOS also moves the app icon badge. Until it fires, the chat shows no badge and is
outside the unread filter; Kotlin has no such window, because the stats are part of the
`Chat` before it is added. The delay is not new on iOS — every chat in the list is
counted through that same collector, and queuing rather than writing immediately means a
delta for a chat removed within that second is discarded instead of leaking into the
profile counter. iOS also keeps a single `chats` list, so the secondary-context guard
has no counterpart there.

## Implementation steps

1. `ChatModel.kt`, `addChatItem`: in the `cInfo.groupChatScope() == null` branch of the
   missing-chat case, build `Chat.ChatStats(unreadCount = 1, unreadMentions = …)` for an
   `RcvNew` item, guarded on `secondaryContextFilter == null`, and raise
   `increaseUnreadCounter` and `updateChatTagReadInPrimaryContext` before `addChat`.
2. `ChatModel.swift`, `addChatItem`: in the same branch, queue the count through
   `unreadCollector.changeUnreadCounter`, the call its in-list branch already makes.
3. `spec/state.md`, `spec/client/chat-list.md` and their iOS counterparts: record the
   changed behaviour of `addChatItem` and where the unread filter's input comes from.
4. `product/views/chat-list.md` and its iOS counterpart: record what a user sees for a
   chat outside the loaded window.
5. `product/gaps.md` (and the iOS catalogue): record the defects this work surfaced but
   does not fix — GAP-08, GAP-09, GAP-10 — and annotate them in the spec and product
   documents they belong to.

## Verification

Desktop, two binaries built from this worktree at this commit and differing only in the
Kotlin hunk: the fix, and a control produced by restoring `ChatModel.kt` from the parent
commit. Both link the same core, so nothing but the fix separates them. The fixture is a
profile of 5106 chats (5104 contacts, a note folder and a pending connection; a
different, smaller profile was used for the 5104-chat measurement in §2). Both binaries
were run against the same chat, which ranked 5102 places below the newest and was
confirmed absent from the list at startup; the fixture was reset between runs, so the
only difference between the two columns is the binary. Each message was sent while the
app was running and in the foreground, and the database was read immediately before and
after each run.

| | control | fix |
|---|---|---|
| database | `rcv_new` | `rcv_new` |
| chat list | chat appears, no unread badge | chat appears, badge `1` |
| unread filter | "No unread chats" | chat is listed |

A second message into that same chat took the badge from `1` to `2`, so once the insert
branch has created the chat the ordinary in-list branch finds it and increments, rather
than re-entering the insert branch and resetting the count.

In a separate run of the same binary a second chat, also outside the window, took its
own badge `1` rather than adding to the first, so counts do not leak between chats. That
message decrypted as an error rather than as text, because the ratchet on its connection
had been rolled back by an earlier snapshot restore. It does not weaken the observation:
the branch keys on `cInfo.groupChatScope()` and `cItem.meta.itemStatus`, and reads
`cItem.meta.userMention`, none of which depend on whether the content decrypted.

Two parts of the change are not covered by any of this. The iOS change was written and
reviewed against the surrounding code but was not compiled or run, as this was developed
on Linux; the desktop measurements do not carry over to it. The secondary-context guard
is present in the measured binary but was not exercised, because reaching it needs a
group with member support or reports and that fixture no longer exists; it rests on
reading `changeUnreadCounterInPrimaryContext` and `updateChatTagReadInPrimaryContext`,
which early-return on a secondary context, and on `ChatView` reading `unreadCount` from
whichever context it renders.

`:common:compileKotlinDesktop` passes on the current code.
