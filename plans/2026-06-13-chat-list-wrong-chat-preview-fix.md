# Chat list: message preview and unread counter applied to the wrong chat

**PR:** #7072 · **Files:** `ChatModel.kt` (`ChatsContext.addChatItem`, `ChatModel.addSentChatItem`, `ChatInfo.inMainChatList`), `ComposeView.kt`, `FramedItemView.kt`

## Symptom

While a secondary chat view is open — "Chat with admins", "chat with member", or a
group's reports view — an incoming message could land on an unrelated chat in the
main list: that chat took the message as its last-message preview and its unread
badge incremented. The wrong preview stays until that chat's next message or until the chat list is reloaded; the wrong
unread count until the chat is opened (opening reloads its stats).

## Root cause

`ChatsContext` is a nested class with two live instances: the primary chat list
(`ChatModel.chatsContext`, shown in the main list) and a secondary context created
per reports / member-support view (set up in `ChatListNavLinkView.kt`, `MemberSupportChatView.kt`,
`GroupReportsView.kt`). `SimpleXAPI` dispatches every incoming item to **both**.

Inside `addChatItem`:

```kotlin
val i = getChatIndex(rhId, cInfo.id)   // searches THIS context's own list
...
chat = chatsContext.chats[i]           // but reads the PRIMARY singleton's list
chatsContext.chats[i] = chat.copy(...) // ...and writes it
```

`getChatIndex` is a member of `ChatsContext`, so `chats` resolves to the receiver
context. `chatsContext` (unqualified) is a member of the enclosing `object ChatModel`
and always resolves to the **primary** instance. On the secondary context the two
lists hold different chats in a different order, so `i` — a secondary-list index —
points at an unrelated chat in the primary list, which then gets clobbered.

### Why the indices diverge

The secondary context's chat list starts empty (only the primary is seeded on load) and
grows by `addChat` at index 0 as chats receive messages. The two lists fall out of step
when a change reaches only one of them, for example:
- a message sent in "Chat with admins" creates the group's entry only in the secondary
  list;
- a chat added to or removed from the primary list only (a group invitation, a contact
  request);
- a burst of messages within 2 s: the primary list's pop is throttled, while the
  secondary `addChat` inserts at index 0 at once.

Until #7404 is merged, the secondary pop collector rebuilds the secondary list from the primary
one, which re-aligns them now and then, so the bug is intermittent.

### How it was introduced

Commit `b97e1e0f1` (#5909, "show new messages from support scope in main chat preview
when invitee is pending") changed `chats[i]` → `chatsContext.chats[i]` at four sites
while broadening the preview guard with `|| memberPending`. The guard change was the
feature for received items; the `chatsContext.` qualifier also made the invitee's sent
support messages reach the main list, but at an index from the secondary list, which
on the secondary context updates the wrong chat.

## The fix

Use the receiver context's own list at all four sites, keeping the broadened guard:

```diff
- chat = chatsContext.chats[i]
+ chat = chats[i]
- chatsContext.chats[i] = chat.copy(
+ chats[i] = chat.copy(
- updateChatTagReadInPrimaryContext(chatsContext.chats[i], wasUnread)
+ updateChatTagReadInPrimaryContext(chats[i], wasUnread)
- reorderChat(chatsContext.chats[i], 0)
+ reorderChat(chats[i], 0)
```

This is a byte-for-byte revert of the qualifier from `b97e1e0f1`, with the feature's
guard preserved. `chats[i]` get/set is the idiom used elsewhere in the class.

## The revert alone would break #5909

A pending invitee's own **sent** support message reaches only the **active** context —
`ComposeView` send/forward and `FramedItemView` command-send all call
`chatsCtx.addChatItem`, and in a member-support chat `chatsCtx` is the *secondary*
context. Before this change that call wrote `chatsContext.chats[i]`, i.e. the primary
list, so the group's main-list preview did update — but only as a side effect of the
bug, at whatever index happened to line up.

Reverting the four sites therefore removes it: the secondary updates its own list and
the primary is never touched, so the invitee's sent support messages stop appearing as
the group's preview. Confirmed by testing during review.

So the revert is paired with an explicit dispatch, which does the same thing on purpose
and at the right index:

```kotlin
suspend fun addSentChatItem(activeCtx: ChatsContext, rhId: Long?, cInfo: ChatInfo, cItem: ChatItem) {
  activeCtx.addChatItem(rhId, cInfo, cItem)
  if (activeCtx.secondaryContextFilter != null && cInfo.inMainChatList) {
    chatsContext.addChatItem(rhId, cInfo, cItem)
  }
}
```

`ChatInfo.inMainChatList` (`groupChatScope() == null || membership.memberPending`) is
the rule `addChatItem` applies to decide whether an item updates the preview of a chat
already in the list; both places use the property.

Cross-context dispatch stays with the **caller**, matching how `processReceivedMsg`
hands received items to both contexts and how `ChatView` handles deletes. No
`ChatsContext` method writes the other context — the invariant this fix restores
(the pop collector still reads the primary list until #7404).

Passing the scoped `cInfo` to the primary is safe: when the group is in the main list
`updateChatInfo` strips the scope before storing it (when it isn't, `addChat` stores it
as received items already do), and `chatItemBelongsToScope` returns false in the primary context
for a scoped `cInfo`, so the item never enters the primary item list.

## Why it is safe

- The four-site revert changes nothing for the **primary** context:
  `chats === chatsContext.chats` there, so both forms resolve to the same list. The
  secondary context stops touching the wrong one. The primary context additionally
  receives items sent from a secondary view that belong in the main list (the pending
  invitee's support messages; also a bot command tapped in the reports view, which the
  main chat now shows).
- `updateChatTagReadInPrimaryContext` and `increaseUnreadCounter` (through
  `changeUnreadCounterInPrimaryContext`) already return early in a secondary context,
  so they stay primary-only either way.
- `popChatCollector` is per-context (`PopChatCollector(this)`), so the pop/reorder
  calls write the context they run in (until #7404 the collector reads the primary
  list; see "A related defect" below).
- The unread stats increment is limited to the primary context
  (`secondaryContextFilter == null`). The secondary context's own entry for the open
  group is what `ChatView` reads for the unread button inside the open view; without
  the limit it would count main-scope group messages that the view doesn't show. The
  limit keeps increments out of that entry, as #7559 does for chats added to the list.
- iOS is unaffected — `ItemsModel` holds only chat items and the chat list is a single
  `chats` array on `ChatModel`, so the mis-indexing cannot occur there.

## Alternatives considered

- **Guard the whole block to `secondaryContextFilter == null`.** Looked smaller, but
  the secondary list would then stop getting the preview and order updates that
  received items give it, and the block would branch on the context type.
- **Compute `i` against the primary list and keep the primary writes.** Would make the
  secondary call double-update the primary preview and unread, and never update the
  secondary list at all.

## A related defect, fixed separately

`PopChatCollector` is nested but not `inner`, so the unqualified `getChat` / `chats`
inside `popCollectedChats()` also resolve to `ChatModel` — the primary list — while
the result is written to `chatsCtx.chats`. Same class of defect, different symptom
(it corrupts the secondary context's own list, not the main one). Fixed in #7404.

## Verification

Desktop, master vs this PR, with "Chat with admins" open in a group the user does not
own: see the PR description for the reproduction and the before/after screenshots.
