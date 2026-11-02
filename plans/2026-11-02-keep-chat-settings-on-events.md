# Keep chat settings on chat events

## Problem

Right after joining a group or connecting a contact, a changed chat setting (notifications All → Mentions or off, favorite, send receipts) can return to the old value about 0.5 s later. The core keeps the new value; the chat list, the chat menu and the chat info show the old one until the chats are reloaded.

## Cause

- The core reads a chat's GroupInfo or Contact when it starts processing an agent event and includes that copy in the event (`Subscriber.hs` `processAgentMessage`).
- `APISetChatSettings` writes the settings without the chat's lock, so an event's copy can be read before the write.
- The app processes events one at a time, separately from command replies, so an event emitted before the write can also be processed after the reply.
- `updateChatInfo` (Kotlin `ChatsContext`, iOS `ChatModel`) replaces the whole chat info, settings included. Every event that updates a listed chat uses it: `updateGroup`, `updateContact`, `addChatItem`, `ChatInfoUpdated`.

## Fix

- `updateChatInfo` keeps the listed chat's current `chatSettings` for direct and group chats.
- New `setChatSettings(chatId, chatSettings)` sets the settings of a listed chat.
- The app's own settings writes use `setChatSettings`:
  - `updateChatSettings` (notifications, favorite, send receipts);
  - clearing the receipts overrides in Privacy settings.
- A chat added to the list takes its settings from the event, as before.

## Scope

- Kotlin (Android, desktop): `ChatModel.kt`, `ChatListNavLinkView.kt`, `PrivacySettings.kt`.
- iOS: `ChatModel.swift`, `ChatView.swift`, `PrivacySettings.swift`.
- Core unchanged.
