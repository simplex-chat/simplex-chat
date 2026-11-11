# Contact info changes undo each other

## Problem

In contact info, a settings change reverts earlier changes to the contact:

- Mute, then change "Send receipts": the contact is unmuted again.
- Change "Send receipts", then mute: the receipts change is lost, so receipts can be sent to a contact they were turned off for.
- Desktop: favorite the contact from the chat list while its info is open, then mute from the info: the favorite is removed.
- An alias set in the info reverts in the chat list and the chat header after a mute or receipts change from the same info, until the chats are reloaded.

## Cause

`ChatInfoView` stores `chat = remember(contact.id) { ... }`, a copy taken when contact info opened (#2898). The receipts setter and `MuteButton` build the saved `ChatSettings` from `chat.chatInfo`. `updateChatSettings` sends all settings to the core and writes that copy, with the new settings, into the model.

## Fix

The receipts setter and `MuteButton` pass `(chatModel.getChat(chat.id) ?: chat).chatInfo`, as the wallpaper save does.

## Scope

Android and desktop. In group info, the chat is read from the model on every composition. The header and the delete and clear dialogs still use the copy.
