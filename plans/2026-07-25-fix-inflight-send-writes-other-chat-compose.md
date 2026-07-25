# Fix: message being sent leaks into another chat's compose/draft, and erases what is typed there

Branch: `nd/fix-inflight-send-writes-other-chat-compose` (off `origin/master`)
Date: 2026-07-25
PR: #7308

Line references are against `origin/master` at `64bf35804`, with this fix
applied. All platforms: Android and desktop
(`multiplatform/.../views/chat/ComposeView.kt`) and iOS
(`ios/Shared/Views/Chat/ComposeMessage/ComposeView.swift` and
`ios/Shared/Views/Chat/ChatView.swift`).

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
  (`ComposeView.kt:972-976`) uses it. Leaving the chat never cancels an
  in-flight send.

Two writes then act on the wrong chat:

- **On the chat switch** — `ComposeView.kt:1343-1347`: the `cs.inProgress`
  branch used to keep the message in the shared compose state
  (`composeState.value = cs.copy(inProgress = false, progressByTimeout = false)`)
  and only cleared the *previous* chat's saved draft. The text and the
  quote were therefore sitting in the input of the chat opened next, and
  `ComposeView.kt:1348-1358` (`!cs.empty`) then saved them as *that*
  chat's draft on the next switch. Symptom 1.
- **When the send completes** — `ComposeView.kt:943-968`, running in the
  detached coroutine after the switch: `clearState(live)` on success, or
  `composeState.value = lastFailed` on failure, where `lastFailed =
  cs.copy(inProgress = false, preview = preview)`
  (`ComposeView.kt:729`) **keeps `contextItem`, i.e. the reply**. On
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
(`ComposeView.kt:1343-1347`). On switching away with a send in flight the
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
sent** (`ComposeView.kt:936-968`). The predicate is shared with the other
senders (`ComposeView.kt:600-602`):

```kotlin
fun composeHasSentMessage(): Boolean = chatModel.chatId.value == chat.id && composeState.value.inProgress
```

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
there too. The three senders' failure paths are moved onto Main for the
same reason; their success paths already were. On iOS the equivalent
block is inside `await MainActor.run`, which gives the same guarantee.

`inProgress` is the marker that the compose state is still the submitted
message: it is set by `sending()` (`ComposeView.kt:596-598`), preserved by
`copy` while sending (the only other write during a send is
`progressByTimeout` at `ComposeView.kt:1610-1617`), reset when switching
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
  that finalises a live message when leaving the chat, `ComposeView.kt:1338-1342`).
  They never call `sending()`, so a guard based on `inProgress` would
  change their behaviour: failed live sends would stop restoring and would
  write a draft on every failing keystroke send. `sendMessageAsync` reads
  `composeState` inside the coroutine (`ComposeView.kt:684`), so that
  branch cannot clear the state itself without racing the send.

**3. The same guard for the three other senders that call `sending()`**
(`ComposeView.kt:604-616`, `618-636`, `659-681`: member contact
invitation, connect prepared contact, connect prepared group). Each of
them called `clearState()` on success and reset `inProgress` on failure
without checking which chat is open, so the reported symptoms were
reachable through them too - in particular a late `clearState()` erasing
a message typed in the chat opened next.

Blast radius: no new state, no new lifecycle. The clear/restore that
already ran now runs only when the compose state still belongs to the
sent message; the only added write is the failed-message draft, gated on
`saveLastDraft`.

## iOS

iOS has the same architecture and the same defect: `ChatView.swift:57`
holds `@State composeState` shared between chats, `ChatView.swift:355-401`
reassigns `chat` in place when `chatModel.chatId` changes (forward
destination, mention or member tap, notification, "open chat forwarded
from"), and `sendMessageAsync` has the identical
`if !live { ...; await sending() }` structure
(`ComposeView.swift:1483-1486`), so `composeState.inProgress` is the same
marker. The post-send block at `ComposeView.swift:1547-1560` cleared and
restored the shared state with no check of which chat is open, and the
three sibling senders (`1141-1161`, `1180-1195`, `1197-1215`) did the
same, exactly like the Kotlin ones.

The same guard is applied, via the same predicate:

```swift
private func composeHasSentMessage() -> Bool {
    chatModel.chatId == chat.id && composeState.inProgress
}
```

Not ported: iOS has no `lastMessageFailedToSend` equivalent - its send
tail only clears - so there is no "failed message is restored or kept as a
draft" half to mirror. A failed send on iOS drops the message as it does
today; this change only stops it from clearing or overwriting another
chat's compose.

iOS also needs change 1 in its own place. `ComposeView.onDisappear`
(`ComposeView.swift:682-707`) already clears the previous chat's draft
when a send is in progress, but it does not run when another chat is
opened in place - the same `ChatView` is reused and only `chat` is
reassigned. So the equivalent is done in that chat change
(`ChatView.swift:362-370`), mirroring what `onDisappear` does for the
same case:

```swift
if cId != chat.id, composeState.inProgress {
    if chatModel.draftChatId == draftChatId(chat.id, chat.chatInfo.groupChatScope()) {
        chatModel.draft = nil
        chatModel.draftChatId = nil
    }
    composeState = ComposeState()
}
```

It is limited to `inProgress` so that the compose state deliberately
carried into the chat opened next is untouched - forwarding sets it and
then opens the destination chat (`ChatItemForwardingView.swift:107-108`),
and `cId != chat.id` also skips secondary (member support) chat views,
which share the group's chat id.

Unlike the Kotlin side it cannot call `clearState()`, which also resets
the link preview state: those properties belong to `ComposeView`, not
`ChatView`. A link preview fetch still pending for the sent message can
therefore set a preview in the chat opened next - a pre-existing iOS
behaviour (the fetch is guarded only by `pendingLinkUrl`), not introduced
here, and unlikely for a message that has already been submitted.

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
| invitation / connect send, switch chats | cleared or leaked into the other chat | other chat untouched |

## Limitations

Kept deliberately, to not grow the change:

- A message that failed to send is dropped, rather than kept, when the
  "Message draft" privacy setting is off, when the destination chat of a
  failed forward already has a draft (its own draft is preserved
  instead), and when the single draft slot is later taken by another
  chat - drafts are one global slot, so the last write wins.
- The three connect/invitation senders have no failed-message restore at
  all. Their typed message is now cleared when leaving the chat instead
  of being carried into the next one, so if the call then fails it is
  lost. The failure is not silent - `apiSendMemberContactInvitation` and
  `apiConnectPreparedContact`/`apiConnectPreparedGroup` show an alert.
- Typing in the same chat while its own send is in flight is still
  cleared when the send completes: `inProgress` is preserved by `copy`,
  so the guard stays true. Unchanged from before, and different from the
  reported symptom, which needs the chat to be switched.
- iOS keeps no failed message either: its send tail only clears, so a
  failed send drops the message there as before.

## Verification

- `./gradlew :common:compileKotlinDesktop` — passes. The iOS side is not
  built: it needs an Xcode build before merging.
- Manual (needs a slow or failing send — e.g. airplane mode, or a large
  file). On Android and desktop any chat switch exercises it; on iOS the
  chat has to be opened in place (tap a mention or a member, or forward
  into another chat), because leaving to the chat list destroys the view:
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
`draftChatId(chat.id, chatScope)` on Android/desktop and
`draftChatId(chat.id, chat.chatInfo.groupChatScope())` on iOS, like every
other draft write.

Related: `plans/2026-07-25-fix-forward-moves-draft-to-target-chat.md`
(PR #7307) — different cause (stale `chat` captured by the desktop
`onDispose`), same shared-compose-state design.
