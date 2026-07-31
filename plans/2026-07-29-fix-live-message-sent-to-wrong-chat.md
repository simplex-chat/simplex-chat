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
     after the switch.
  3. Regressions: an ordinary send goes to the chat it was typed in;
     forwarding still targets the chat it was forwarded to; reporting a
     message still reports it in the chat it belongs to; sending in a
     member support chat still goes to that scope.

Independent of #7308 (that one is about a send that is still in flight
when the chat is switched), but both change the end of `sendMessageAsync`,
so expect a conflict if they land together. Resolving it: the checks #7308
adds there act on the chat the message was sent to, so its `chat.id` /
`chat.chatInfo.id` become `toChat.id` / `toChat.chatInfo.id`; its
`cs.liveMessage != null` clause already covers the send made by the chat
switch.
