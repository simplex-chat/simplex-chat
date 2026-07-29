# Fix: message being sent leaks into another chat's compose/draft, and erases what is typed there

Branch: `nd/fix-inflight-send-writes-other-chat-compose` (off `origin/master`)
Date: 2026-07-25
PR: #7308

Line references are against `origin/master` at `64bf35804`, with this fix
applied. Android and desktop only
(`multiplatform/.../views/chat/ComposeView.kt`); iOS has the same defect
but is not addressed here.

## Problem

Two reported symptoms, one cause. Both need a send that is still in
flight when the chat is switched (slow network, large file, or the send
just hanging with the progress circle showing):

1. **The message ends up in another chat's draft.** Reply to a message
   (or just type), press send, switch to another chat while it is
   sending: the text *and the reply context* appear in that chat's input,
   and leaving it saves them as that chat's draft. No forwarding
   involved.
2. **A late send erases what you typed.** Press send, the progress circle
   keeps spinning, switch to another chat and back, type a new message —
   when the original send finally succeeds, the newly typed message is
   erased.

## Cause

The compose state is shared, and the send outlives the chat:

- `ChatView.kt:134` — one `MutableState<ComposeState>` per `ChatView`
  instance, `rememberSaveable` with no keys, reused for every chat that
  the view displays.
- `Utils.kt:43-46` — `withLongRunningApi` launches on
  `CoroutineScope(Dispatchers.Default)`, a standalone scope with no tie
  to the composition or to the chat, and `sendMessage`
  (`ComposeView.kt:968-972`) uses it. Leaving the chat never cancels an
  in-flight send.

Two writes then act on the wrong chat:

- **On the chat switch** — `ComposeView.kt:1339-1343`: the `cs.inProgress`
  branch used to keep the message in the shared compose state
  (`composeState.value = cs.copy(inProgress = false, progressByTimeout = false)`)
  and only cleared the *previous* chat's saved draft. The text and the
  quote were therefore sitting in the input of the chat opened next, and
  `ComposeView.kt:1344-1354` (`!cs.empty`) then saved them as *that*
  chat's draft on the next switch. Symptom 1.
- **When the send completes** — `ComposeView.kt:939-963`, running in the
  detached coroutine after the switch: `clearState(live)` on success, or
  `composeState.value = lastFailed` on failure, where `lastFailed =
  cs.copy(inProgress = false, preview = preview)`
  (`ComposeView.kt:725`) **keeps `contextItem`, i.e. the reply**. On
  success this wipes whatever is in the input now — including a message
  typed after coming back (symptom 2); on failure it dumps the old
  message into whichever chat is open (symptom 1 again).

The same function was already inconsistent about which chat it acts on:
its draft bookkeeping (`clearCurrentDraft()`, and the forwarding
condition) uses the **captured** `chat` — the chat the message was
composed in — while its `composeState` writes hit whatever chat is
displayed at that moment.

## Fix

Two changes, both in `ComposeView.kt`.

**1. Do not keep the message being sent in the shared compose state**
(`ComposeView.kt:1339-1343`). On switching away with a send in flight the
compose state is cleared, so nothing leaks into the chat opened next:

```kotlin
} else if (cs.inProgress) {
  clearPrevDraft(prevChatId)
  // the message being sent must not be kept in the compose state, it is shared with the chat opened next;
  // if it fails to send it is restored in this chat or saved as its draft
  clearState()
}
```

`clearState()` is used rather than assigning an empty `ComposeState` so that
the link preview state is reset too (`pendingLinkUrl` still points at the
sent message's link, and its fetch would otherwise set a preview on the
input of the chat opened next), and so that the attachment size limit is
carried over the same way as everywhere else.

In-flight content is deliberately **not** saved as a draft here: the
message has been submitted and will most likely be sent, and a draft is
for messages that are not sent yet.

**2. Only touch the compose state if it still holds the message that was
sent** (`ComposeView.kt:932-963`):

```kotlin
withContext(Dispatchers.Main) {
  val chatIsOpen = chatModel.chatId.value == chat.id
  val sentMessageInCompose = live || cs.liveMessage != null || (chatIsOpen && composeState.value.inProgress)
  if (sentMessageInCompose) {
    if (lastFailed == null) {
      clearState(live)
    } else {
      composeState.value = lastFailed
    }
  }
  val draft = chatModel.draft.value
  if (wasForwarding && chatModel.draftChatId.value == draftChatId(chat.chatInfo.id, chatScope) && forwardingFromChatId != chat.chatInfo.id && draft != null) {
    if (sentMessageInCompose) composeState.value = draft
  } else {
    clearCurrentDraft()
    if (!sentMessageInCompose && lastFailed != null) {
      // the message was not sent, so it is restored in the chat it was composed in, or kept as its draft if another chat is open
      if (chatIsOpen && composeState.value.empty) {
        composeState.value = lastFailed
      } else if (saveLastDraft) {
        chatModel.draft.value = lastFailed
        chatModel.draftChatId.value = draftChatId(chat.id, chatScope)
      }
    }
  }
}
```

Both the checks and the changes run on `Dispatchers.Main` (the block has
no suspension points), so they cannot be interleaved with the user
switching chats or typing - `KeyChangeEffect`, which does change 1, runs
there too.

`inProgress` is the marker that the compose state is still the submitted
message: it is set by `sending()` (`ComposeView.kt:596-598`), preserved by
`copy` while sending (the only other write during a send is
`progressByTimeout` at `ComposeView.kt:1606-1613`), reset when switching
away (change 1), and never set by typing a new message. So a chat switch
*or* newly typed text both make the guard false.

A **failed** send is different from an in-flight one - the message was not
sent, so it is an unsent message. It is put back into the input if that
chat is open and nothing else is being composed there, and kept as that
chat's draft otherwise, so it never appears in another chat (see the
limitations below for when it is still dropped). Staying in the chat is
unaffected: the guard is true there
and the failed message is restored into the input as before, keeping
"preserving long message when failed to send" (`e61babdc8`) working.

Deliberately unchanged:

- The condition of the forwarding branch. Gating the whole branch would
  send a forward that completed after the user left to `clearCurrentDraft()`
  instead, **deleting** the destination chat's draft that the branch
  exists to preserve - only the compose write inside it is gated.
- Live message sends (`live`, or `cs.liveMessage != null` for the send
  that finalises a live message when leaving the chat, `ComposeView.kt:1334-1338`).
  They never call `sending()`, so a guard based on `inProgress` would
  change their behaviour: failed live sends would stop restoring and would
  write a draft on every failing keystroke send. `sendMessageAsync` reads
  `composeState` inside the coroutine (`ComposeView.kt:680`), so that
  branch cannot clear the state itself without racing the send.

## Behaviour after the fix

| situation | before | after |
| --- | --- | --- |
| send, stay in chat, succeeds | input cleared | input cleared (unchanged) |
| send, stay in chat, fails | message restored in input | message restored in input (unchanged) |
| send, switch chats, succeeds | message left in the other chat's input, saved as its draft | other chat untouched |
| send, switch chats, fails | message dumped into the other chat's input | message restored in the chat it was composed in, or kept as its draft |
| send hangs, switch away and back, type, then it succeeds | typed message erased | typed message kept |
| forward send, still in destination chat | destination chat's draft restored | unchanged |
| live message sent on leaving the chat | compose state cleared by the send | unchanged |

## Limitations

Kept deliberately, to not grow the change:

- A message that failed to send is dropped, rather than kept, when the
  "Message draft" privacy setting is off, when the destination chat of a
  failed forward already has a draft (its own draft is preserved
  instead), and when the single draft slot is later taken by another
  chat - drafts are one global slot, so the last write wins.
- Change 1 clears the compose state for any send in progress, including
  the three senders that connect a prepared chat (they call the same
  `sending()`). Those have no failed-message restore, so their typed
  message is dropped when the chat is switched instead of being carried
  into the next chat. They also still clear the compose on completion
  without checking which chat is open, so the symptom this fixes remains
  reachable through them, and their failure path can reset `inProgress`
  for a send started in the chat opened next, leaving that sent message
  in the input.
- Typing in the same chat while its own send is in flight is still
  cleared when the send completes: `inProgress` is preserved by `copy`,
  so the guard stays true. Unchanged from before, and different from the
  reported symptom, which needs the chat to be switched.
## Verification

- `./gradlew :common:compileKotlinDesktop` — passes.
- Manual (needs a slow or failing send — e.g. airplane mode, or a large
  file). On desktop any chat switch exercises it; on Android only an
  in-place switch does (member info → open chat), because leaving to the
  chat list destroys the view:
  1. Reply + type in A, send, switch to B while sending. B's input must
     stay empty; leaving B must not create a draft in B. If the send
     failed, A must hold the message (with the reply) as its draft.
  2. Send in A with the network off so the circle keeps spinning, switch
     to B and back to A, type a new message, restore the network. The
     typed message must survive the old send completing.
  3. Regression: ordinary send in A (input clears), failed send while
     staying in A (message comes back in the input), forward into a chat
     that has a draft (draft restored after sending).

Rebased onto the scope-aware draft ids introduced by #7309: the draft
written here for a message that failed to send uses
`draftChatId(chat.id, chatScope)`, like every other draft write.

Related: `plans/2026-07-25-fix-forward-moves-draft-to-target-chat.md`
(PR #7307) — different cause (stale `chat` captured by the desktop
`onDispose`), same shared-compose-state design.
