# Fix: chat list preview — wrong-chat clobber + pending invitee's member-support messages

**PR:** #7072 · **Branch:** `nd/fix-message-preview-and-unread-on-wrong-chat`
**Commits:** `8b93c226d` wrong-chat revert · `862d93c64` sent preview · `bd7c6c3e8`/`55bdaa216` received preview (android-desktop/ios) · `5b37cf881` prefer-content (no event re-covers the preview) · `2b20c9efb`/`8440f17d5` edit/delete preview sync · `1d2a7a3b5`/`8bb47e505` media preview
**Files:** `ChatModel.kt`/`ChatModel.swift` (`addChatItem`)

Part 1 (below) is the original wrong-chat fix. Part 2 (Follow-up, at the end) makes the
#5909 pending-invitee preview feature correct in-session for sent / received — and
**corrects a wrong assumption in "Why it's safe" below**. Reload persistence was investigated
and **deliberately deferred** for performance (see 2c).

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
  delivered for **received** items by the **always-called primary** `addChatItem` invocation +
  the broadened guard. ⚠️ **Correction (see Follow-up):** this does **not** hold for **sent**
  items — they reach only the active (secondary) context, so the revert dropped the sent-message
  preview and a dedicated fix was required. #5909's `chatsContext.` write had masked this.
- `popChatCollector` is **per-context** (`PopChatCollector(this)`), so the pop/reorder calls
  act on the same context they run in.
- `updateChatTagReadInPrimaryContext` / `increaseUnreadCounter` already self-guard with
  `if (secondaryContextFilter != null) return`, so they remain primary-only.
- It was the **only** `chatsContext.`-qualified self-reference in the class; no sibling sites.

## Verification
- Reproduced and confirmed fixed by report author.
- Compiles: `chats[i]` get/set is the established idiom used throughout `ChatsContext`
  (lines 418, 436, 443, 493, 618, 621).

---

# Part 2 — Follow-up: pending invitee's member-support preview

The revert is correct for the wrong-chat bug, but testing surfaced that the #5909 feature
(a pending invitee's "chat with admins" / member-support messages shown as the group's
main-list preview) was not actually delivered end-to-end. Gaps found and fixed in the clients:
**2a (sent)**, **2b (received, incl. prefer-content)**, **2d (edit / delete sync)**, **2e (media)**;
**2c (reload persistence)** was investigated and deferred for performance.

## 2a. Sent messages — `862d93c64` (android, desktop)

**Symptom:** a pending invitee's own sent support message did not appear in the main-list
preview (it did before the revert).

**Cause:** received items are dispatched to **both** contexts (`SimpleXAPI` `NewChatItems`,
primary + secondary), but **sent** items are added only to the **active** context —
`ComposeView` send/forward and `FramedItemView` command-send all call
`chatsCtx.addChatItem`, and in the member-support view `chatsCtx` is the **secondary**
context. So a sent support item never reached the primary context that owns the main-list
preview; with the revert it updated only the (invisible) secondary list. #5909's
`chatsContext.chats[i]` write had masked this (at a wrong index = the wrong-chat bug).

**Fix:** in `addChatItem`, when on a member-support secondary context and the item is sent,
mirror it to the primary context (like the receive dispatcher):

```kotlin
if (secondaryContextFilter is SecondaryContextFilter.GroupChatScopeContext && cItem.chatDir.sent) {
  chatsContext.addChatItem(rhId, chatInfo, cItem)
}
```

Scoped to `GroupChatScopeContext` (reports view has no compose). Safe: sent items aren't
`RcvNew`, so no unread double-count; `chatItemBelongsToScope` keeps the body out of the main
scope; no recursion (primary delegate has `secondaryContextFilter == null`). Matches iOS,
where `ComposeView` already calls `chatModel.addChatItem` on its single list.

## 2b. Received messages — `bd7c6c3e8` (android, desktop) · `55bdaa216` (ios) · `5b37cf881` (prefer-content)

**Symptom:** a received support message showed the static "reviewed by admins" status text
in the preview instead of the message. (`ChatPreviewView` renders that text only when the
preview item has **no `msgContent`**, i.e. it's still a group event.)

**Cause:** the group preview keeps the higher-`itemTs` item:
`cItem.meta.itemTs >= currentPreviewItem.meta.itemTs`. A **received** item's `itemTs` is the
SMP **broker** clock; the placeholder group event (e.g. `RGEInvitedViaGroupLink`) has a
**local device** clock `itemTs`. This cross-clock comparison can keep the no-content event.
Sent items win only because their `itemTs` is the same device's local clock.

**Fix (two parts):** for a pending invitee, (1) **bypass the cross-clock comparison** so the
support message surfaces, and (2) **prefer content** so a no-content event can't re-cover an
already-shown message (Kotlin + iOS):
```
if (memberPending)
  if (cItem.content.hasMsgContent || !currentPreviewItem.content.hasMsgContent) cItem else currentPreviewItem
else if (cItem.meta.itemTs >= currentPreviewItem.meta.itemTs) cItem else currentPreviewItem
```
Part (2) (`5b37cf881`) fixes a follow-up symptom — **"reviewed by admins" reappearing after the
first message**: a pending invitee's member-support no-content events arrive as new chat items in
member-support scope and, with bare "take newest", re-covered the preview. The trigger is
`SGEUserPendingReview` on the PendingApproval→PendingReview transition (`Subscriber.hs:2661`),
plus member-connected / E2EE-info / group-feature items (`Subscriber.hs:878-881`); all have
`msgContent = null` so `hasMsgContent` is false (matching how `ChatPreviewView` decides to show
the status text). The outer guard already restricts to `groupChatScope() == null || memberPending`,
so non-pending groups keep plain `itemTs` ordering.

## 2d. Edit / delete of the shown support message — `2b20c9efb` · `8440f17d5` (android, desktop, ios)

Two parts.

**Guard** (`2b20c9efb`): `addChatItem` got the `memberPending` exception, but its siblings
`upsertChatItem` / `removeChatItem` still gated their main-list-preview update on
`groupChatScope() == null`. So once a support message was a pending invitee's preview, **editing it
left stale text and deleting/moderating it left a phantom**. The same `memberPending` exception is
added to both (Kotlin + iOS); they already match the preview item by id (`pItem?.id == cItem.id`).
On iOS the extra "update preview for an item missing from the open scope" clause is now restricted
to main scope, so a support item's status updates don't churn the preview. Side benefit: pairs the
support-item unread increment with a decrement on delete / read-of-preview (partial mitigation of
the unread limitation below).

**Edit dispatch** (`8440f17d5`, android/desktop only): the guard alone wasn't enough for an
invitee's **own** edit. Deletes reach the primary context (the receive dispatcher and the delete UI
both call both contexts), but an own edit goes through `ComposeView` → `chatsCtx.upsertChatItem` on
the **active (secondary) context only** — the same asymmetry as sends — so the primary preview was
never touched. Fix: on an edit in a `GroupChatScopeContext`, also call
`chatModel.chatsContext.upsertChatItem` (`ComposeView.kt`). iOS is unaffected (single `chats` list;
its `upsertChatItem` already updates the preview via the guard).

## 2e. Media support messages in the preview — `1d2a7a3b5` · `8bb47e505` (android, desktop, ios)

A caption-less photo / video / voice / file support message rendered its **thumbnail** (never
gated) but its **text line** showed the status ("reviewed by admins"), because `ChatPreviewView`
showed `chatPreviewInfoText()` whenever the last item had no non-empty *text* (`hasMsgContent`) —
which also catches caption-less media. Fix: **for a pending invitee only**, show the status text just
for true no-content events (`content.msgContent == null`), so a message — including caption-less media
— renders its content ("image" / "video message" / etc.). Scoped to `memberPending` (`8bb47e505`):
`ChatPreviewView` is shared rendering, so every other chat (direct connection states, other group
states) keeps the original `hasMsgContent` behaviour. Captioned media already worked (it has text).

## 2c. Reload persistence — investigated, **NOT implemented (performance)**

**The gap:** 2a/2b are in-memory only. `findGroupChatPreviews_` (`Store/Messages.hs`) selects the
main-list preview item with `group_scope_tag IS NULL AND group_scope_group_member_id IS NULL`,
excluding support-scope items — so after a full reload (app restart / user switch) a pending
invitee's preview reverts to the no-content group event ("reviewed by admins") until the next
support message re-populates it in memory.

**Why deferred:** persisting it means relaxing that scope filter in the **per-group preview
subquery**, which is a hot query run on every chat-list load. The naive relaxation measurably
regresses it for **large chat lists**, and the clean fixes add query complexity for a state that
is rare and **transient** (only until an admin approves/rejects the invitee). 2a/2b already
deliver the feature within the active session, and the reload revert is a minor cosmetic gap.

### Benchmark (SQLite 3.40, schema from `chat_schema.sql`, 500 groups × 100 items, 500 runs)

Per chat-list load for a 500-group user (`EXPLAIN QUERY PLAN` + wall-clock):

| Variant | per load | plan |
|---|---|---|
| Current (main-scope only) | **0.92 ms** | **COVERING** seek on `idx_chat_items_group_scope_item_ts` (4-col) |
| Naive `… OR EXISTS(pending)` | 2.36 ms (**2.6×**) | loses covering → `idx_chat_items_groups_item_ts` scan + **table-fetch per group** + correlated `group_members` lookup |
| `UNION ALL` of two covering seeks, pick newer | 1.98 ms (2.1×) | both branches covering; evaluates both |
| `COALESCE(support-if-pending, main)` | 1.43 ms (1.5×) | both covering; also fixes the cross-clock pick (prefers support) |
| `CASE WHEN EXISTS THEN support / main` | 1.56 ms (1.7×) | covering; skips support seek for non-pending groups |
| **Per-user gate** → old query if not pending | **1.06 ms** | non-pending users (≈ everyone) ~unchanged; one `EXISTS` check |

### Root cause & how far it could be optimized
- **Regression source:** the `OR` defeats the covering index
  `idx_chat_items_group_scope_item_ts (user_id, group_id, group_scope_tag, group_scope_group_member_id, item_ts)`,
  forcing a per-group table-row fetch to read the scope columns (visible as `sys` time ~0.01s→0.38s).
- **Best design found — per-user global gate + covering form:** check once whether the user is
  pending in *any* group; if not, run the **byte-identical old query** (zero plan change for the
  vast majority of users); if so, run the `COALESCE`/covering form. Non-pending users → ~old
  performance; a pending user stays on covering seeks (~1.7×, **sub-2 ms / 500 groups**) with only
  the actual pending group doing the extra support lookup. It would also fix the cross-clock pick
  (prefer support over a higher-`itemTs` local event) for free.
- **Irreducible cost:** while a user *is* pending, the relaxed branch (or the per-group check)
  applies — ~1.5–1.7× the old query, transient and sub-2 ms at 500 groups, but non-zero.
  Eliminating it entirely would need either a new covering index ordered
  `(…, item_ts)` for the support case, or a denormalized per-group "last preview item id" — both
  larger than this PR warrants.

**Decision:** ship 2a/2b only. The gated `COALESCE` form above is the ready design if persistence
is revisited.

## Residual limitations / known follow-ups
- **In-memory preview only** (see 2c): after a reload the preview reverts to "reviewed by admins"
  until the next support message; persistence deferred for performance.
- **Out-of-order content pins the last-arrived message:** for a pending invitee, content items skip
  the `itemTs` check (broker vs local clock are not comparable), so two content messages delivered
  out of order show arrival order rather than newest-by-timestamp. Low frequency for support chats.
- **Unread badge can lag (pre-existing):** a received support message increments the main-list
  unread (the #5909 `memberPending` branch, now reachable). 2d adds a decrement on edit/delete and
  read-of-preview, but reads of *non-preview* support messages reconcile via
  `membership.supportChat.unread` rather than the main counter, so the main unread badge can lag.
  Not fully addressed here.

## Verification (Part 2)
- Desktop AppImage built on the branch with 2a/2b; in-app send/receive pending-preview behavior to
  be confirmed by the report author.
- iOS change mirrors the Kotlin guard one-to-one; reuses the existing
  `cInfo.groupInfo?.membership.memberPending` accessor already used two lines above.
- 2c benchmark: `sqlite3` + `EXPLAIN QUERY PLAN` against `chat_schema.sql`, 500 groups × 100 items.
