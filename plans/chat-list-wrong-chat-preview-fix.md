# Chat list: message preview and unread counter applied to the wrong chat

**PR:** #7072 · **Files:** `apps/multiplatform/.../model/ChatModel.kt` (`ChatsContext.addChatItem`)

## Symptom

While a secondary chat view is open — "Chat with admins", "chat with member", or a
group's reports view — an incoming message could land on an unrelated chat in the
main list: that chat took the message as its last-message preview, its unread badge
incremented, and it popped to the top. The corruption persisted until the chat list
was reloaded (app restart or user switch).

## Root cause

`ChatsContext` is a nested class with two live instances: the primary chat list
(`ChatModel.chatsContext`, shown in the main list) and a secondary context created
per reports / member-support view (`ChatListNavLinkView.kt`, `MemberSupportChatView.kt`,
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

The secondary context's chat list starts empty (nothing seeds it) and grows via
`addChat`, which inserts at index 0 as unrelated chats receive messages. The primary
also pops messaged chats to index 0, so for a while the two lists agree and the wrong
index coincidentally points at the right chat — which is why the bug is intermittent.

They fall out of step once **some chat is messaged twice**: the primary pops it back
to the top, while the secondary leaves it where it was, because `addChat` only runs
for a chat it has not seen. From then on the indices differ.

### How it was introduced

Commit `b97e1e0f1` (#5909, "show new messages from support scope in main chat preview
when invitee is pending") changed `chats[i]` → `chatsContext.chats[i]` at four sites
while broadening the preview guard with `|| memberPending`. The guard change was the
actual feature; the `chatsContext.` qualifier was an unintended side change that only
manifests on the secondary context.

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

This is a byte-for-byte revert of the unintended part of `b97e1e0f1`, with the
feature's guard preserved. `chats[i]` get/set is the established idiom used at nine
other sites in the same class.

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
the same rule `addChatItem` already applies to decide whether an item updates the main
list preview, now named once instead of spelled out.

Cross-context dispatch stays with the **caller**, matching how `processReceivedMsg`
hands received items to both contexts and how `ChatView` handles deletes. No
`ChatsContext` method reaches into the other context — which is the invariant this
whole fix restores.

Passing the scoped `cInfo` to the primary is safe: `updateChatInfo` strips the scope
before storing it, and `chatItemBelongsToScope` returns false in the primary context
for a scoped `cInfo`, so the item never enters the primary item list.

## Why it is safe

- Behaviour for the **primary** context is unchanged: `chats === chatsContext.chats`
  there, so both forms resolve to the same list. Only the secondary context changes,
  and only to stop touching the wrong one.
- `updateChatTagReadInPrimaryContext` and `increaseUnreadCounter` already self-guard
  with `if (secondaryContextFilter != null) return`, so they stay primary-only either way.
- `popChatCollector` is per-context (`PopChatCollector(this)`), so the pop/reorder
  calls act on the context they run in.
- One deliberate behaviour change follows: the secondary context's own chat entry now
  gets its unread count maintained, which is what `ChatView.kt` reads for the unread
  button inside the open view. Under the bug that entry was never updated.
- iOS is unaffected — `ItemsModel` holds only chat items and the chat list is a single
  `chats` array on `ChatModel`, so the mis-indexing cannot occur there.

## Alternatives considered

- **Guard the whole block to `secondaryContextFilter == null`.** Looked smaller but is
  riskier: the secondary context's `chats` list **is** read for display, so skipping the
  block would leave that list unmaintained. It also deletes live code paths.
- **Compute `i` against the primary list and keep the primary writes.** Would make the
  secondary call double-update the primary preview and unread, and never update the
  secondary list at all.

## A related defect, fixed separately

`PopChatCollector` is nested but not `inner`, so the unqualified `getChat` / `chats`
inside `popCollectedChats()` also resolve to `ChatModel` — the primary list — while
the result is written to `chatsCtx.chats`. Same class of defect, different symptom
(it corrupts the secondary context's own list, not the main one). Fixed in #7404.

## Verification

- Reproduced by hand on 7.1-beta.1 desktop: "Chat with admins" open in a channel the
  user does not own, messages from several direct contacts with one messaging twice,
  a couple of seconds apart. An unrelated chat took the message as its preview and
  gained an unread badge.
- The identical sequence — three scripted direct contacts, 15 rounds, ~3s apart, same
  support chat open — run against a build of this PR produced no wrong-chat previews
  and no stray unread badges.
- Compiles: `:common:compileKotlinDesktop`, zero errors.
