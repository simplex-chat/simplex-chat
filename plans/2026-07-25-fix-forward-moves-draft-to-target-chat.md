# Fix: forwarding a message moves the message draft to another chat (desktop)

Branch: `nd/fix-forward-moves-draft-to-target-chat` (off `origin/master`)
Date: 2026-07-25
PR: #7307

All line references are against `origin/master` at `cdef9f51e`, with this
fix applied.

## Problem

On desktop, forwarding a message out of a chat that has an unsent message
draft can move that draft to a **different** chat. It is most visible when
the draft lands on the chat that was forwarded to: after the forward is
sent, the source chat's draft text appears in the destination chat's input,
and the source chat no longer has a draft (no draft preview in the chat
list, empty input when reopened).

Reported on Linux desktop. Android and iOS are not affected — see
"Cross-platform findings" below.

Repro (desktop):

1. Start the app (or press Esc so no chat is open), open chat **B**.
2. Click chat **A** in the chat list — without closing the chat in between.
3. Type text in A, do not send.
4. Right-click a message in A → Forward → pick **B**.
5. Send the forward → A's draft text appears in B's input, and is gone
   from A.

The condition for step 5 to show the draft is that the destination chat is
the chat that was open when the current `ComposeView` was first composed
(B, from step 1). With any other destination the draft is still misfiled —
it silently reappears in B later instead of in A.

## Cause

The draft is saved under a **stale chat id**.

Forwarding closes the chat before the destination is picked
(`ChatView.kt:3656-3662`, `forwardContent`: `chatId = null`, then
`sharedContent = SharedContent.Forward(...)`). On desktop `ChatView` is
composed only while `chatId != null` (`App.kt:387-417`,
`CenterPartOfScreen`), so this disposes `ComposeView`, and the
`KeyChangeEffect(chatModel.chatId.value)` that normally moves the draft
(`ComposeView.kt:1311-1332`) never gets to run — the composable leaves
composition in the same recomposition that sets `chatId = null`. That is
what the desktop-only `DisposableEffect` at `ComposeView.kt:1354-1366` is
for: it saves the draft on dispose while a forward is pending.

`DisposableEffect(Unit)` runs its effect lambda once and keeps the
`onDispose` it returned, so that closure captures the `chat` **parameter
value from the composition in which the effect was created** — later
recompositions with a different `chat` do not replace it. `composeState`
is a `MutableState` and reads fresh; `chat.id` does not.

That capture is stale whenever the chat changed without `ComposeView`
being disposed, which on desktop is the normal way to switch chats:

- `ChatItemsLoader.kt:71-73` and `ChatListNavLinkView.kt:238-244`
  (`openLoadedChat`) set `chatModel.chatId.value` directly, with no
  intermediate `null`, so `ChatView`/`ComposeView` are not disposed on a
  chat switch.
- `ChatView.kt:134-144` remembers `composeState` with `rememberSaveable`
  and **no keys**, i.e. the instance is deliberately shared across chat
  switches.

So `chat.id` in `onDispose` is the first chat opened after the last time
`chatId` was `null` — not the chat being forwarded from.

Why the draft then surfaces in the destination chat: after the forward is
sent, `ComposeView.kt:930-943` restores a draft that belongs to the
current chat (`draftChatId == chat.chatInfo.id` and the forward did not
originate there, line 939). That is a deliberate feature — a draft in the
destination chat survives forwarding into it — and it is correct. It only
misbehaves because it was handed a draft misattributed to that chat.

## Fix (2 lines, desktop-only)

`ComposeView.kt:1354-1366` — read the current chat id in `onDispose`
through `rememberUpdatedState` instead of the captured parameter:

```kotlin
if (appPlatform.isDesktop) {
  // the same ComposeView is reused when switching chats, so `chat` captured by onDispose would be the chat opened first, not the current one
  val currentChatId = rememberUpdatedState(chat.id)
  // Don't enable this on Android, it breaks it, This method only works on desktop. For Android there is a `KeyChangeEffect(chatModel.chatId.value)`
  DisposableEffect(Unit) {
    onDispose {
      if (chatModel.sharedContent.value is SharedContent.Forward && saveLastDraft && !composeState.value.empty) {
        chatModel.draft.value = composeState.value
        chatModel.draftChatId.value = currentChatId.value
      }
    }
  }
}
```

Blast radius:

- Inside the existing `if (appPlatform.isDesktop)` block; Android never
  executes it, iOS is a separate codebase.
- Only the value written to `draftChatId` changes, and only in the case
  where it was already wrong. When `ComposeView` was first composed for
  the chat being forwarded from — the case that already worked —
  `currentChatId.value == chat.id` and behavior is identical.
- `rememberUpdatedState` is already used in this file
  (`ComposeView.kt:1208-1210`, `1343`), so no new import.

Alternatives considered and rejected:

- `DisposableEffect(chat.id)` — restarts the effect on every chat switch,
  so `onDispose` would also fire on plain chat switches and write a draft
  there whenever a forward happens to be pending. Larger behavior change
  for no benefit.
- Moving the desktop save into `KeyChangeEffect` — it cannot run at all in
  this path, for the reason above. That is why the `DisposableEffect`
  exists.
- Restructuring so `ComposeView` is keyed per chat — would fix the stale
  capture but changes draft handling, focus and compose state for every
  chat switch on both platforms. Out of proportion to the bug.

## Cross-platform findings

**Android — not affected.**

- `ComposeView.kt:1354` — the `DisposableEffect` that writes
  `draftChatId` is inside `if (appPlatform.isDesktop)`.
- `ComposeView.kt:1311-1332` — Android saves the draft in
  `KeyChangeEffect(chatModel.chatId.value) { prevChatId -> ... }`
  (`draftChatId = prevChatId`, line 1329); `prevChatId` comes from the
  effect's own remembered previous key (`Utils.kt:683-699`), i.e. the chat
  id that was actually open, so it cannot go stale.
- `App.kt:344-352` — `currentChatId` (what `ChatView` is composed from) is
  set to `null` only after `onComposed(null)` finishes the slide-out
  animation, so `ComposeView` is still composed when `chatId` becomes
  `null` and that effect runs.
- `ComposeView.kt:1522-1524` — the `sharedContent` effect returns early
  while `chatId == null`, so the draft saved for the source chat is the
  plain text, without the forward context.

Result: the draft stays with the source chat, the restore condition at
`ComposeView.kt:939` does not match in the destination, and
`clearCurrentDraft()` there is a no-op.

**iOS — not affected by this symptom; has a different defect (kept
separate, not addressed here).**

iOS does not use `sharedContent` or `chatId = null` for forwarding — the
picker is a sheet over the open chat that writes into the source chat's
compose binding:

- `ChatItemForwardingView.swift:107-108` — forwarding to a different chat
  does `composeState.wrappedValue = ComposeState.init(forwardingItems:fromChatInfo:)`
  then `ItemsModel.shared.loadOpenChat(chat.id)`.
- `ComposeView.swift:95-101` — that initializer sets `message = ""`.
- `ComposeView.swift:682-707` — the only writer of `draftChatId` is
  `saveCurrentDraft()` (line 699, defined at `1852-1855`) in
  `.onDisappear`, and `ChatView.swift:355-368` keeps the same `ChatView`
  instance across a chat switch (it reassigns `chat`), so no disappear
  fires during a forward.

So on iOS `draftChatId` is never written with a stale id — the restore at
`ComposeView.swift:1548-1554` only fires for a draft that genuinely
belongs to the destination chat. Instead, text typed in the source chat
and not yet persisted as a draft is **discarded** when forwarding out of
that chat: loss, not migration, and a divergence from Android/desktop,
which preserve it as a draft. Two lower-confidence iOS leads, noted but
not verified:

- `ComposeView.swift:1551` lacks the
  `forwardingFromChatId != chat.chatInfo.id` guard that
  `ComposeView.kt:939` has, so forwarding into the same chat could
  re-populate the input from a stale saved draft (`ChatView.swift:783`
  restores a draft on chat open without clearing it).
- other in-chat `loadOpenChat` calls that do not overwrite compose state
  (e.g. `ChatItemInfoView.swift:386`) would carry typed text into the
  newly opened chat, since nothing saves or clears it on that path.

## Verification

- `./gradlew :common:compileKotlinDesktop` — passes.
- Manual, desktop AppImage built from this branch:
  1. R1 (was broken): app start → open B → click A → type in A → forward
     from A to B → send. Expected: B's input holds only the forwarded
     item; A still shows its draft in the chat list and when reopened.
  2. R2 (regression guard, was already correct): app start → open A
     directly → type in A → forward from A to B → send. A keeps its
     draft.
  3. R3 (the restore feature at `ComposeView.kt:939` must still work):
     draft in B → open A → forward a message from A into B → send. B's own
     draft is restored in the input after sending.
