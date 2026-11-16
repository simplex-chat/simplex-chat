# Desktop chat info shows another chat's mute, receipts and member search

## Problem

On desktop the chat info pane stays open when another chat is clicked in the list, and then shows the new chat with some state of the previous chat:

- Mute: open muted contact A's info, click unmuted contact B. B's button says "Unmute"; a tap applies A's next mode, so B stays unmuted. Groups behave the same.
- Group "Send receipts": "no" in group A, then click group B: B shows "no" although its setting is the default.
- Member search: a search typed in group A's members filters group B's members. In a group of 8 members or fewer besides you, the search field is hidden, so the members of B that do not match stay hidden.

Changes made outside the pane while it is open are not shown: muting the chat from the chat list menu, and changing receipts in Settings → Your privacy.

## Cause

- Contact and group `MuteButton` store the mode in `remember { mutableStateOf(...) }` without a key, read through `derivedStateOf`. The pane is not recreated per chat, and only `updateChatSettings` called from the button updates that state.
- The group `sendReceipts` is `remember { ... }` without a key; the contact one is keyed by contact id only (#2898). Neither is keyed by the setting.
- The member search text is `stateGetOrPut("searchText")` in the pane's `ModalData`, one value for every chat the pane shows (#4817).

## Fix

- `MuteButton` (contact and group): `nextMode` of the chat's own `chatSettings.enableNtfs`; the local state and its `toggleNotifications` argument are removed.
- `sendReceipts` (contact and group): `remember` keyed by the chat id, the chat's `chatSettings.sendRcpts` and the user's default for that chat type. The setter still sets it before the API call returns. The option list of `SendReceiptsOption` is keyed by the user's default.
- Member search: `remember(chat.id) { stateGetOrPut("searchText:${chat.id}") { ... } }`; `filteredMembers` is keyed by `searchText` too. The text remains in the pane's `ModalData` after Back from a page opened over the pane.

## Scope

- The info screen closes on a chat switch on Android, and the info sheet is dismissed on iOS (`ChatView.swift`). Android contact info now also shows the saved mute and receipts values after the other setting is changed.
- Member info, message info, "Invite members" and "Chats with members" close on a chat switch (`KeyChangeEffect(chatModel.chatId.value)`); Group link opened from the chat menu stays open and acts on its own group. Not changed.
