# Keep unsaved preference edits when a message arrives

## Problem

On Android and desktop, unsaved edits in Group (Channel, Chat) preferences, Member admission and Contact preferences are discarded when a message arrives or a group event changes the chat's group or contact data, for example a subscriber joining a channel. Leaving the screen then shows no "Save preferences?" alert ("Save admission settings?" in Member admission).

## Cause

Each screen keeps its edit state and the saved state in `rememberSaveable` keyed on the whole `GroupInfo` or `Contact`. Received messages and group events replace the chat's `chatInfo` with a new object (`ChatsContext.addChatItem` and `updateGroup` call `updateChatInfo`), whose `chatTs`, `updatedAt` or member counts differ, so the key changes and both states are re-initialised from the saved values. `ChatBotCommand.Command` and `Menu` are plain classes, so preferences that contain bot commands differ in every received `chatInfo`.

## Fix

The states are keyed on:

- GroupPreferences.kt: `gInfo.fullGroupPreferences` and `gInfo.isOwner`
- MemberAdmission.kt: `gInfo.groupProfile.memberAdmission` and `gInfo.isOwner`
- ContactPreferences.kt: `ct.mergedPreferences` and `user.fullPreferences`

A change of the saved preferences, by a save, by another owner or by the contact, and a change of the owner role still re-initialise the screen.

`ChatBotCommand.Command` and `ChatBotCommand.Menu` are data classes.

`savePrefs` and `saveAdmission` build the group profile from the current `groupInfo.value`.

## Scope

- apps/multiplatform common code: Android and desktop.
- iOS is not changed.
