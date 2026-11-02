# Keep chat settings on chat events

## Problem

Right after joining a group, or while many messages of a group or contact arrive, a changed chat setting (notifications All → Mentions or off, favorite, send receipts) can return to the old value a few seconds later. The core keeps the new value. The chat list, the chat menu and the chat info show the old one until a later event of that chat, opening the chat or a reload of the chats shows the new value.

## Cause

- The core reads a chat's GroupInfo or Contact when it starts processing an agent event and includes that copy in the events it emits for that chat (`Subscriber.hs` `processAgentMessage`).
- `APISetChatSettings` writes the settings without the chat's lock, so an event's copy can be read before the write.
- The app processes events one at a time, separately from command replies, so an event emitted before the write can also be processed after the reply.
- `updateChatInfo` (Kotlin `ChatsContext`, iOS `ChatModel`) replaces the whole chat info, settings included. The events that update a listed chat use it: `updateGroup`, `updateContact`, `addChatItem`, `ChatInfoUpdated`.
- iOS `receivedContactRequest` replaces a listed chat that is not open with the event's chat (`replaceChat`), settings included.

## Fix

- `updateChatInfo` keeps the listed chat's current `chatSettings` for direct and group chats.
- iOS `receivedContactRequest` keeps them when it replaces a listed chat: `ChatInfo.keepingChatSettings(of:)`, which `updateChatInfo` uses too.
- New `setChatSettings` (Kotlin `(rhId, chatId, chatSettings)`, iOS `(chatId, chatSettings)`) sets the settings of a listed chat.
- The app's own settings writes use `setChatSettings`:
  - `updateChatSettings` (notifications, favorite, send receipts);
  - clearing the receipts overrides in Settings → Your privacy → More privacy.
- A chat added to the list, and every reload of the chats, take the settings from the core, as before.

## Scope

- Kotlin (Android, desktop): `ChatModel.kt`, `ChatListNavLinkView.kt`, `PrivacySettings.kt`; test `ChatSettingsEventTest.kt` (desktopTest).
- iOS: `ChatModel.swift`, `SimpleXAPI.swift`, `ChatView.swift`, `PrivacySettings.swift`.
- Specs: `apps/multiplatform/spec/state.md`, `apps/ios/spec/state.md`.
- Core unchanged.
