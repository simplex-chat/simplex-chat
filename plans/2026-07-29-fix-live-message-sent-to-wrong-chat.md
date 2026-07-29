# Fix: live message is sent to the chat opened after switching chats

Branch: `nd/fix-live-message-sent-to-wrong-chat` (off `origin/master`)
Date: 2026-07-29

Line references are against `origin/master` at `64bf35804`, with this fix
applied. Android and desktop (`commonMain/ComposeView.kt`).

## Problem

Typing a live message and switching to another chat sends that message to
the chat that was opened, without the user sending anything. Reported on
desktop, where every chat switch reuses the same view.

## Cause

A live message is committed when the chat is switched
(`ComposeView.kt:1315-1322`):

```kotlin
KeyChangeEffect(chatModel.chatId.value) { prevChatId ->
  val cs = composeState.value
  if (cs.liveMessage != null && (cs.message.text.isNotEmpty() || cs.liveMessage.sent)) {
    sendMessage(null)
```

`KeyChangeEffect` is `LaunchedEffect(key1) { block(prev) }`
(`Utils.kt:640-654`), so when the key changes, `remember(key1)` rebuilds
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
(`ComposeView.kt:679-681`, `949-953`); inside, the API calls, the item
insertion and the draft bookkeeping use it instead of the captured
`chat`.

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

## Verification

- `./gradlew :common:compileKotlinDesktop` — passes.
- Manual:
  1. Start a live message in **A**, type, and switch to **B** while
     typing. The message must appear in **A**; nothing is sent in **B**,
     and B's input and draft are untouched.
  2. Repeat with a draft already saved in **B** - it must still be there
     after the switch.
  3. Regressions: an ordinary send goes to the chat it was typed in;
     forwarding still targets the chat it was forwarded to; reporting a
     message still reports it in the chat it belongs to; sending in a
     member support chat still goes to that scope.

Independent of #7308 (that one is about a send that is still in flight
when the chat is switched), but both change this file, so expect a small
conflict if they land together.
