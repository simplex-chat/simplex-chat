# Fix: live message is sent to the chat opened after switching chats

Branch: `nd/fix-live-message-sent-to-wrong-chat` (off `origin/stable`)
Date: 2026-07-29

Line references are against `origin/stable` at `8dc387cb5`, with this fix
applied. Android and desktop (`commonMain/ComposeView.kt`).

## Problem

Typing a live message and switching to another chat sends that message to
the chat that was opened, without the user sending anything. Reported on
desktop, where every chat switch reuses the same view.

## Cause

A live message is committed when the chat is switched
(`ComposeView.kt:1316-1328`), which before this change was:

```
    if (cs.liveMessage != null && (cs.message.text.isNotEmpty() || cs.liveMessage.sent)) {
      sendMessage(null)
```

`KeyChangeEffect` is `LaunchedEffect(key1) { block(prev) }`
(`Utils.kt:683-698`), so when the key changes, `remember(key1)` rebuilds
it from the lambda of the composition that is running *now* - and by then
`chatModel.chatId` is already the new chat, `ChatView` has recomposed
`ComposeView` with the new `chat`, and the block that runs captured that
one.

`sendMessage(null)` → `sendMessageAsync` → `send(chat, ...)` uses the
captured `chat`, while the message content comes from `composeState`,
which is shared between the chats opened in this view. So the content of
the chat that was left is sent to the chat that was opened:

- `liveMessage.sent == false` - a new message is created in the wrong
  chat, which is what is seen;
- `liveMessage.sent == true` - `apiUpdateChatItem` is called with the new
  chat's type and id and the item id from the previous chat, which the
  backend cannot resolve.

The same mismatch made the post-send `clearCurrentDraft()` clear the
draft of the chat opened after the switch, deleting a draft that was
never sent.

## Fix

`sendMessageAsync` and `sendMessage` take the chat the message was
composed in, defaulting to the chat this view shows
(`ComposeView.kt:679-682`, `952-956`). Only what a live message can reach
uses it: the message send, the update of an already sent live message,
and the two places that clear the draft after sending. Live messages have
no context item (`SendMsgView.kt:156-165` only offers the button when the
compose is empty and has none), so the forwarding, editing and reporting
branches cannot run with a chat other than the view's and keep using
`chat` - the parameter is not threaded through them.

The chat switch resolves the chat by the id it had before the switch:

```kotlin
val liveMessageChat = if (prevChatId == null || prevChatId == chat.id) chat else chatsCtx.getChat(prevChatId)
// if that chat is gone there is nowhere to send it, and it must not be sent to the chat opened instead
if (liveMessageChat != null) sendMessage(null, toChat = liveMessageChat) else clearState()
```

`prevChatId == chat.id` keeps the view's own chat, which is what secondary
(member support) chat views need - they share the group's chat id, and
only their `chat` carries the scope.

If the previous chat can no longer be found the message is not sent at
all, and the compose state is cleared so it does not leak into the chat
that was opened. Sending it to the chat that is open now is the defect
being fixed, so it is not used as a fallback.

### Handing the compose state over to the opened chat

Sending to the right chat is not enough on its own: `composeState` is
shared between the chats opened in this view, and this is the only branch
of `KeyChangeEffect` that neither resets it nor loads the opened chat's
draft - the branch that loads a draft (`else if (chatModel.draftChatId
.value == draftChatId(chatModel.chatId.value, chatScope) ...)`) is later
in the same `if` chain and cannot be reached. So the live message stayed
in the compose state of a view that now shows another chat, and that
chat's draft was never read.

`sendMessageAsync` then made it visible. It runs on `Dispatchers.Default`,
so its writes land after the switch:

```kotlin
val liveMessage = cs.liveMessage
if (!live) {
  if (liveMessage != null) composeState.value = cs.copy(liveMessage = null)  // the whole composed state
  sending()                                                                  // and its spinner
}
```

The opened chat's input showed the text composed in the previous one until
the send completed and `clearState()` emptied it; the draft it should have
shown was still in the model, and the next switch away dropped it. This
predates this fix - on `master` the same writes happen, and there
`clearCurrentDraft()` resolves to the opened chat and deletes its draft
outright.

Four changes, all following from "this send no longer owns the compose
state":

- `sendMessageAsync` takes `composed: ComposeState = composeState.value`,
  and `sendMessage` takes `composed: ComposeState? = null`, so only the
  chat switch passes a state and every other sender still reads it inside
  the coroutine, exactly where the send read it before. The chat switch
  captures it on the main thread before replacing it - without that the
  send would read the compose state of the chat that was opened and send
  *its draft* to the previous chat.
- `checkLinkPreview` takes that state too. It re-read `composeState`
  rather than what was passed in, and it is reached by every text live
  message through `updateMsgContent`, so with the compose state handed
  over it would have rebuilt the message from the opened chat's draft, or
  from nothing - overwriting the live message instead of committing it.
  Only the calls inside `sendMessageAsync` pass the state; the three
  senders that connect a prepared chat keep reading the current one.
- Every `composeState` write in `sendMessageAsync` is guarded by
  `composeIsForSend()` (`toChat.id == chat.id`): directly for the two at
  the start, and through `chatIsOpen` for the clear/restore at the end,
  which #7308 already routes through `sentMessageInCompose`. It compares
  the two chats rather than checking which one is open, so the send made
  by a chat switch never takes the compose state back, not even if that
  chat is opened again before the send completes.
  `clearCurrentDraft(toChat)` is already keyed on the chat and needs no
  guard. Whether the *view's own* send may still write when its chat has
  been switched away is #7308's question, not this one's.
- The chat-switch branch then resets `composeState` to the opened chat's
  draft, or to an empty state, like the branches below it do.

## Blast radius

`toChat` defaults to the chat this view shows, so every other send is
unchanged: the send button (`SendMsgView.kt`), the live updates while
typing (`sendMessageAsync(live = true)`), forwarding, editing and
reporting all pass no chat and behave exactly as before. Only the send
started by the chat switch passes a different one, and only the branches
it can reach were changed.

The live message update loop is not affected: it is started once
(`SendMsgView.kt:523-559`) with the `::updateLiveMessage` reference of the
composition in which live mode started, so its updates already go to the
chat the message belongs to, and it exits when the send started by the
chat switch clears `liveMessage`. Only that send was created fresh on
every composition, which is why it was the one going to the wrong chat.

Not covered, and unchanged: a live message in a member support chat that
is closed without changing the chat id is never committed - the effect
that commits it is keyed on the chat id, which does not change when that
view is closed.

## Verification

- `./gradlew :common:compileKotlinDesktop` — passes.
- Manual:
  1. Start a live message in **A**, type, and switch to **B** while
     typing. The message must appear in **A**; nothing is sent in **B**,
     and B's input and draft are untouched.
  2. Repeat with a draft already saved in **B** - it must still be there
     after the switch. This is the case that was found failing: B showed
     the text composed in A, then emptied when the send completed, and B's
     draft was dropped on the next switch. Watch B's input from the moment
     of the switch, not only after the send finishes.
  3. Slow or failing send (network off) while doing 1 and 2, so the window
     between the switch and the send completing is long enough to type in
     **B** - what is typed there must survive the send completing.
  4. Regressions: an ordinary send goes to the chat it was typed in;
     forwarding still targets the chat it was forwarded to; reporting a
     message still reports it in the chat it belongs to; sending in a
     member support chat still goes to that scope.

## Merged with #7308

#7308 (a send that is still in flight when the chat is switched) landed in
`stable` first, so this branch was merged with it. Both changed the end of
`sendMessageAsync`, and the two guards are **not** the same rule - the
merge keeps both:

- here, `composeIsForSend()` = `toChat.id == chat.id` - is this send for
  the chat this view shows, or for another one;
- in #7308, `chatIsOpen` = `chatModel.chatId.value == chat.id` - is the
  chat this view shows still the one open.

`chatIsOpen` becomes the conjunction,
`composeIsForSend() && chatModel.chatId.value == chat.id`. Where `toChat`
is `chat` - every send but the one made by a chat switch - that reduces to
#7308's own check, so its behaviour is unchanged.

Nothing else in that block had to move. #7308 already routes both compose
writes through `sentMessageInCompose`, which derives from `chatIsOpen`, so
guarding `chatIsOpen` guards them; the rest of the change there is two
call sites taking `toChat` (`clearCurrentDraft`, and the draft id a failed
message is saved under).

An earlier revision of this note said that #7308's `cs.liveMessage != null`
clause "already covers the send made by the chat switch". **It did not.**
At the time that clause sat outside the `chatIsOpen` check:

```kotlin
val sentMessageInCompose = live || cs.liveMessage != null || (chatIsOpen && composeState.value.inProgress)
```

which is correct only while a live message is always sent to the chat that
is open - the assumption this fix removes. Read as written, the clause
*exempts* the chat-switch send from the very guard that protects the
opened chat, and a merge that followed it reintroduced the leak described
above. #7308 shipped with the live clauses moved inside `chatIsOpen`,
which was a no-op on its own branch and is what makes this merge work.
