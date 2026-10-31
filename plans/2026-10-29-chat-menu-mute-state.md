# Chat menu offers another chat's mute mode

## Problem

The mute item of the chat header ⋮ menu shows the next mode of the chat it was first shown for:

- Desktop: after clicking another chat in the list.
- Android and desktop: after opening a member's direct chat from a group, or opening a chat from a notification while another chat is open.
- Android and desktop: after muting from the chat info; desktop: after muting from the chat list menu. The item still offers "Mute".

A tap applies the remembered chat's next mode to the open chat. In a group opened after a contact, the item cycles through two modes, as for a contact.

## Cause

`ChatInfoToolbar` stores the mode in `remember { mutableStateOf(enableNtfs) }` without a key (#1026) and reads the next mode through `remember { derivedStateOf { ... } }` (#5574), which also captures the first chat's `hasMentions`. `ChatView` is not recreated per chat.

## Fix

`nextNtfMode = enableNtfs.nextMode(chatInfo.hasMentions)`, computed from the open chat on every composition.

`ntfMode` is removed, with the second parameter of `changeNtfsState` and the `currentState` parameter of `toggleNotifications` and `updateChatSettings` (`ChatListNavLinkView.kt`), which only set it.

## Scope

Android and desktop (common `ChatView`). The chat list menu already reads the list item's chat.
