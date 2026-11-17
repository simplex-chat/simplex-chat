# Chat List Context Menu Preview & Key Actions

## Overview

Add native iOS Context Menu preview (Haptic Touch / long press) to conversation cells in the main chat list (`ChatListView`).
The menu displays a centered preview showing the last 5–10 messages of the conversation scrolled to the bottom, followed by key actions (Mark as read/unread, Mute/unmute, Favorite, Clear chat, Delete/Leave). Tapping the preview commits the transition into the full chat view.

## Critical Invariants

1. **Zero Read Receipts**: Opening or viewing the chat preview MUST NOT send any read receipt or delivery update over the network.
2. **Zero DB Status Mutation**: Displaying messages in the preview MUST NOT mark messages as read in the local SQLite database (`item_status` remains unchanged; `apiMarkChatItemsRead` and `apiChatRead` are never called).
3. **No UI Badge Reset**: The unread count / badge for the chat on the main list MUST NOT be cleared by viewing the preview.
4. **Smooth Commit**: Tapping the preview triggers `willPerformPreviewActionForMenuWith` and smoothly opens the full chat via `ItemsModel.shared.loadOpenChat(chatId)`.
5. **One-Hand UI & Theme Compatibility**: The preview and menu render correctly in both normal and inverted One-Hand UI mode, respecting the active `AppTheme`.

## Technical Architecture

### 1. Context Menu Interaction (`UIContextMenuInteraction`)
SwiftUI's `.contextMenu(menuItems:preview:)` has two major blockers:
- It is iOS 16+ only (SimpleX deployment target is iOS 15.0).
- It lacks any public API to handle tap-to-commit on the preview without a standard `NavigationLink` (SimpleX uses custom programmatic navigation via `NavLinkPlain` and `NavStackCompat`).

Therefore, we implement a resilient `ChatContextMenuModifier` backed by a `UIViewRepresentable` (`ChatContextMenuRepresentable`):
- Embeds `UIContextMenuInteraction` with `UIContextMenuInteractionDelegate`.
- `previewProvider` returns a `UIHostingController` wrapping `ChatHistoryPreviewView(chat: chat)`.
- Sets `preferredContentSize = CGSize(width: min(UIScreen.main.bounds.width - 32, 380), height: 440)` for an optimal centered preview card.
- `willPerformPreviewActionForMenuWith` animator executes `onCommit()` to navigate seamlessly to the full chat.
- Transparent `UITapGestureRecognizer` ensures single taps open the chat normally without conflicting with vertical table scrolling or horizontal swipe actions.

### 2. Message History Fetching & Privacy Isolation
To avoid loading the heavyweight `ItemsModel` (which loads 50+ items, initializes audio sessions, and triggers `ChatItemView.onAppear` that marks messages read), we fetch recent messages directly:
```swift
let (chatWithItems, _) = try await apiGetChat(
    chatId: chat.chatInfo.id,
    scope: chat.chatInfo.groupChatScope(),
    pagination: .last(count: 10)
)
```
- Fetches strictly from local SQLite via Haskell FFI (`getDirectChatLast_` / `getGroupChatLast_`).
- Does not hit the network.
- Does not change `item_status` or unread counters.
- Provides chronological messages (oldest to newest).

### 3. Lightweight Preview View (`ChatHistoryPreviewView`)
- Displays chat header (avatar, display name, status / subtitle).
- Displays message bubbles (text, media thumbnail, voice duration, file info, timestamps).
- Differentiates outgoing (trailing, `theme.appColors.sentMessage`) and incoming (leading, `theme.appColors.receivedMessage`).
- Uses `ScrollViewReader` to automatically position the view at the bottom (most recent messages).

### 4. Key Context Menu Actions
Limited to essential actions:
- **Read / Unread**: Toggle based on `chat.chatStats.unreadCount > 0 || chat.chatStats.unreadChat`.
- **Mute / Notifications**: Toggle notification mode (`chat.chatInfo.nextNtfMode`).
- **Favorite**: Toggle favorite status (`chat.chatInfo.chatSettings?.favorite`).
- **Clear Chat**: Confirm and clear chat history (if non-empty).
- **Delete / Leave**: Destructive action to delete contact or leave/delete group.

## Implementation Steps

1. Create `Shared/Views/ChatList/ChatHistoryPreviewView.swift`:
   - Header with `ChatInfoImage` and chat info.
   - Message list rendering last 10 messages with `ScrollViewReader` scrolled to bottom.
   - Initialized with `chat.chatItems` and asynchronously refreshed via `apiGetChat(..., pagination: .last(count: 10))`.
2. Create `Shared/Views/ChatList/ChatContextMenuModifier.swift`:
   - `ChatContextMenuUIView`: UIKit view with `UIContextMenuInteraction` and single-tap recognizer.
   - `ChatContextMenuRepresentable`: `UIViewRepresentable` bridging SwiftUI and UIKit.
   - `ChatContextMenuModifier`: View modifier exposing `.chatContextMenu(...)`.
3. Integrate into `Shared/Views/ChatList/ChatListNavLink.swift`:
   - Attach `.modifier(ChatContextMenuModifier(...))` to `contactNavLink`, `groupNavLink`, and `noteFolderNavLink`.
4. Register new files in `SimpleX.xcodeproj/project.pbxproj`.
5. Update documentation: `chat-list.md` and `impact.md`.
