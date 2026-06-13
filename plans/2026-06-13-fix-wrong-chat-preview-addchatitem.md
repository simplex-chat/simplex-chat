# Fix: message preview & unread count applied to the wrong chat

**PR:** #7072 · **Branch:** `nd/fix-message-preview-and-unread-on-wrong-chat` · **Commit:** `8b93c226d`
**File:** `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt` (`ChatsContext.addChatItem`)

## User-facing bug

While a **group reports view** or a **member-support chat** is open, an incoming
message could:

- overwrite the **last-message preview** of an unrelated chat in the main chat list,
- increment the **unread badge** of that wrong chat, and
- **pop** the wrong chat to the top of the list.

The corruption persists until the chat list is reloaded (app restart / user switch).

### Reproduction
1. Open a group's reports view, or a member-support chat (this activates a *secondary* chats context).
2. Receive a direct message from contact **A**, then from **B**, then from **A** again.
3. A's second message appears as the preview of an unrelated chat with an inflated unread badge, instead of under A.

## Root cause

`ChatsContext` is a nested class with two live instances: the **primary** chat list
(`ChatModel.chatsContext`, shown in the main list) and the **secondary** context
(`ChatModel.secondaryChatsContext`, used by reports / member-support views). For every
incoming item, `addChatItem` is invoked on **both** (`SimpleXAPI.kt`).

Inside `addChatItem`:

```kotlin
val i = getChatIndex(rhId, cInfo.id)   // searches THIS context's own list
...
chat = chatsContext.chats[i]            // but reads the PRIMARY singleton's list
chatsContext.chats[i] = chat.copy(...)  // ...and writes it
```

`getChatIndex` resolves against the receiver context (`this.chats`), but
`chatsContext` (unqualified) always resolves to `ChatModel.chatsContext` — the
**primary** list. On the secondary context the two lists hold different chats in a
different order, so `i` (a secondary-list index) points at an unrelated chat in the
primary list, which then gets clobbered.

### How it was introduced
Commit `b97e1e0f1` (#5909, "show new messages from support scope in main chat preview
when invitee is pending") changed `chats[i]` → `chatsContext.chats[i]` at four sites
while broadening the preview guard with `|| memberPending`. The guard change was the
actual feature; the `chatsContext.` qualifier was an unintended side change that only
manifests on the secondary context.

## The fix

Use the receiver context's own list (`chats[i]`) consistently at all four sites,
keeping the broadened `memberPending` guard:

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

This is a clean revert of the unintended part of `b97e1e0f1`, with the feature's guard
preserved.

## Why this fix (alternatives considered)

- **Reverting `chatsContext.chats[i]` → `chats[i]` (chosen).** Index and list become
  consistent within each context. Behaviour for the **primary** context is byte-identical
  (`chats === chatsContext.chats` there); only the **secondary** context changes — and only
  to stop touching the wrong list. Matches every other method in `ChatsContext` and the iOS
  `addChatItem`, which operates on a single list with the same `memberPending` guard.
- **Guard the whole block to `secondaryContextFilter == null` (rejected).** Looked smaller
  but is riskier: the secondary context's `chats` list **is** read for display
  (`ChatView.kt:244` derives the top-right unread badge from `chatsCtx.chats`), so skipping
  the block would zero out that list and break the badge. It also deletes live code paths
  (larger behavioural delta).
- **Compute `i` against the primary list, keep primary writes (rejected).** Would make the
  secondary call also write the primary list, double-updating the primary preview/unread and
  never updating the secondary list the badge reads from.

## Why it's safe

- The #5909 feature (pending invitee sees support-scope messages in the main preview) is
  delivered by the **always-called primary** `addChatItem` invocation + the broadened guard;
  it does not depend on the secondary→primary write.
- `popChatCollector` is **per-context** (`PopChatCollector(this)`), so the pop/reorder calls
  act on the same context they run in.
- `updateChatTagReadInPrimaryContext` / `increaseUnreadCounter` already self-guard with
  `if (secondaryContextFilter != null) return`, so they remain primary-only.
- It was the **only** `chatsContext.`-qualified self-reference in the class; no sibling sites.

## Verification
- Reproduced and confirmed fixed by report author.
- Compiles: `chats[i]` get/set is the established idiom used throughout `ChatsContext`
  (lines 418, 436, 443, 493, 618, 621).
