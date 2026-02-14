# SimpleX Chat iOS -- Chat View Module

> Technical specification for the message rendering, chat item types, and context menu actions in the conversation view.
>
> Related specs: [Compose Module](compose.md) | [State Management](../state.md) | [API Reference](../api.md) | [README](../README.md)
> Related product: [Chat View](../../product/views/chat.md)

---

## Table of Contents

1. [Overview](#1-overview)
2. [ChatView](#2-chatview)
3. [ChatItemView -- Message Routing](#3-chatitemview)
4. [Message Renderers](#4-message-renderers)
5. [Media Views](#5-media-views)
6. [Metadata & Info](#6-metadata--info)
7. [Context Menu Actions](#7-context-menu-actions)
8. [Selection Mode](#8-selection-mode)

---

## 1. Overview

The chat view module renders individual conversations. It consists of:

- **ChatView** -- The main conversation screen with message list, compose bar, and navigation
- **ChatItemView** -- Router that dispatches each chat item to the appropriate renderer
- **Specialized renderers** -- FramedItemView (standard messages), EmojiItemView (emoji-only), CICallItemView (calls), event views, etc.
- **Media views** -- CIImageView, CIVideoView, CIVoiceView, CIFileView for attachments

```
ChatView
├── Message List (ScrollView / LazyVStack)
│   ├── ChatItemView (per message)
│   │   ├── FramedItemView (text/media bubbles)
│   │   │   ├── MsgContentView (text with markdown)
│   │   │   ├── CIImageView / CIVideoView / CIVoiceView
│   │   │   └── CIMetaView (timestamp, status)
│   │   ├── EmojiItemView (emoji-only messages)
│   │   ├── CICallItemView (call events)
│   │   ├── CIEventView (system events)
│   │   ├── CIGroupInvitationView (group invitations)
│   │   ├── DeletedItemView / MarkedDeletedItemView
│   │   └── CIInvalidJSONView (decode errors)
│   └── ... (more items)
├── ComposeView (message input)
└── Navigation bar (contact/group info)
```

---

## 2. ChatView

**File**: `Shared/Views/Chat/ChatView.swift`

The main conversation view. Key responsibilities:

### State
- Uses `ItemsModel.shared.reversedChatItems` for the primary message list
- `ChatModel.shared.chatId` identifies the active conversation
- Manages compose state, scroll position, keyboard visibility
- Tracks selection mode for multi-message actions

### Message List
- Renders messages in a `ScrollViewReader` with `LazyVStack`
- Items are in reverse chronological order (newest at bottom)
- Supports infinite scroll: preloads older messages when scrolling up via `ItemsModel.preloadState`
- Handles pagination splits (`chatState.splits`) for non-contiguous loaded ranges

### Navigation Bar
- Title: contact name / group name with connection status indicator
- Trailing button: navigates to `ChatInfoView` (direct) or `GroupChatInfoView` (group)
- Search button: toggles in-chat message search

### Scroll Behavior
- Auto-scrolls to bottom on new sent/received messages (if already near bottom)
- "Scroll to bottom" floating button when scrolled up
- `openAroundItemId` support: scrolls to a specific message (e.g., from search or notification)

---

## 3. ChatItemView

**File**: `Shared/Views/Chat/ChatItemView.swift`

Routes each `ChatItem` to the appropriate renderer based on its `CIContent` type:

### Content Types (CIContent enum)

| Content Type | Renderer | Description |
|-------------|----------|-------------|
| `sndMsgContent` / `rcvMsgContent` | `FramedItemView` | Standard sent/received text+media message |
| `sndDeleted` / `rcvDeleted` | `DeletedItemView` | Locally deleted message placeholder |
| `sndCall` / `rcvCall` | `CICallItemView` | Call event (missed, ended, duration) |
| `rcvIntegrityError` | `IntegrityErrorItemView` | Message integrity error |
| `rcvDecryptionError` | `CIRcvDecryptionError` | Decryption failure |
| `sndGroupInvitation` / `rcvGroupInvitation` | `CIGroupInvitationView` | Group invite |
| `sndGroupEvent` / `rcvGroupEvent` | `CIEventView` | Group system event |
| `rcvConnEvent` / `sndConnEvent` | `CIEventView` | Connection event |
| `rcvChatFeature` / `sndChatFeature` | `CIChatFeatureView` | Feature toggle event |
| `rcvChatPreference` / `sndChatPreference` | `CIFeaturePreferenceView` | Preference change |
| `invalidJSON` | `CIInvalidJSONView` | Failed to decode |

### Bubble Direction
- Sent messages: aligned right, sender-colored bubble
- Received messages: aligned left, receiver-colored bubble
- Events/system messages: centered, no bubble

### Appearance Dependencies
Each `ChatItemWithMenu` may depend on the previous and next items for visual decisions:
- Whether to show the sender name (group messages, different sender than previous)
- Whether to show the tail on the bubble (last consecutive message from same sender)
- Date separator between messages on different days

`ChatItemDummyModel.shared.sendUpdate()` forces a re-render of all items when global appearance changes.

---

## 4. Message Renderers

### FramedItemView

**File**: `Shared/Views/Chat/ChatItem/FramedItemView.swift`

The standard message bubble. Renders:
- Quote/reply preview (if replying to another message)
- Forwarded indicator
- Sender name (in groups)
- Message content (`MsgContentView` with markdown)
- Attached media (image, video, voice, file, link preview)
- Reaction summary bar
- Metadata line (`CIMetaView`)

### EmojiItemView

**File**: `Shared/Views/Chat/ChatItem/EmojiItemView.swift`

Renders emoji-only messages (messages containing only emoji characters) in a larger font without a bubble background.

### MsgContentView

**File**: `Shared/Views/Chat/ChatItem/MsgContentView.swift`

Renders message text with SimpleX markdown formatting (bold, italic, code, links, mentions).

### DeletedItemView / MarkedDeletedItemView

**Files**: `Shared/Views/Chat/ChatItem/DeletedItemView.swift`, `Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift`

- `DeletedItemView`: Placeholder for locally deleted messages
- `MarkedDeletedItemView`: Shows "message deleted" with optional moderation info (who deleted, when)

### CIEventView

**File**: `Shared/Views/Chat/ChatItem/CIEventView.swift`

Centered system event text for group events (member joined, left, role changed) and connection events.

### CIGroupInvitationView

**File**: `Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift`

Renders group invitation with accept/reject buttons.

---

## 5. Media Views

### CIImageView

**File**: `Shared/Views/Chat/ChatItem/CIImageView.swift`

Renders inline images. Tapping opens `FullScreenMediaView` for zooming/panning. Images are compressed to `MAX_IMAGE_SIZE` (255KB) before sending.

### CIVideoView

**File**: `Shared/Views/Chat/ChatItem/CIVideoView.swift`

Renders video thumbnails with play button. Tapping opens video player. Videos above auto-receive threshold require manual download.

### CIVoiceView / FramedCIVoiceView

**Files**: `Shared/Views/Chat/ChatItem/CIVoiceView.swift`, `Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift`

Renders voice messages with waveform visualization, play/pause control, and duration. `FramedCIVoiceView` is the version inside a message bubble with additional context.

### CIFileView

**File**: `Shared/Views/Chat/ChatItem/CIFileView.swift`

Renders file attachments with filename, size, and download/open actions. Shows transfer progress during upload/download.

### CILinkView

**File**: `Shared/Views/Chat/ChatItem/CILinkView.swift`

Renders link preview cards with OpenGraph metadata (title, description, image).

### AnimatedImageView

**File**: `Shared/Views/Chat/ChatItem/AnimatedImageView.swift`

Renders animated GIF images.

### FullScreenMediaView

**File**: `Shared/Views/Chat/ChatItem/FullScreenMediaView.swift`

Full-screen media viewer with zoom, pan, and share actions. Supports images and videos.

---

## 6. Metadata & Info

### CIMetaView

**File**: `Shared/Views/Chat/ChatItem/CIMetaView.swift`

Displays message metadata inline at the bottom of the bubble:
- Timestamp (sent time)
- Delivery status icon (sending, sent, delivered, read, error)
- Edit indicator (pencil icon if message was edited)
- Disappearing message timer (if timed message)

### ChatItemInfoView

**File**: `Shared/Views/Chat/ChatItemInfoView.swift`

Detailed message information sheet (accessed via long-press menu "Info"):
- Full delivery history (per-member delivery status in groups)
- Edit history (all previous versions of edited messages)
- Forward chain info
- Message timestamps (created, updated, deleted)

---

## 7. Context Menu Actions

Long-pressing a message shows a context menu with actions based on message type and ownership:

| Action | Available For | API Command |
|--------|--------------|-------------|
| Reply | All messages | Sets compose state to `.replying` |
| Forward | Sent/received content messages | `apiForwardChatItems` |
| Copy | Text messages | Copies to clipboard |
| Edit | Own sent messages (within edit window) | `apiUpdateChatItem` |
| Delete for me | All messages | `apiDeleteChatItem(mode: .cidmInternal)` |
| Delete for everyone | Own sent messages | `apiDeleteChatItem(mode: .cidmBroadcast)` |
| Moderate | Group admin/owner for others' messages | `apiDeleteMemberChatItem` |
| React | Content messages (if reactions enabled) | `apiChatItemReaction` |
| Select | All messages | Enters multi-select mode |
| Info | All messages | Opens `ChatItemInfoView` |
| Save | Media messages | Saves to photo library / files |
| Share | Content messages | iOS share sheet |

---

## 8. Selection Mode

Multi-selection mode allows batch operations on messages:

- Enter via long-press "Select" action
- Toggle individual messages with tap
- Toolbar appears with batch actions: Delete, Forward
- Exit via cancel button or completing batch action

---

## Source Files

| File | Path |
|------|------|
| Chat view | `Shared/Views/Chat/ChatView.swift` |
| Item router | `Shared/Views/Chat/ChatItemView.swift` |
| Framed bubble | `Shared/Views/Chat/ChatItem/FramedItemView.swift` |
| Emoji message | `Shared/Views/Chat/ChatItem/EmojiItemView.swift` |
| Image view | `Shared/Views/Chat/ChatItem/CIImageView.swift` |
| Video view | `Shared/Views/Chat/ChatItem/CIVideoView.swift` |
| Voice view | `Shared/Views/Chat/ChatItem/CIVoiceView.swift` |
| File view | `Shared/Views/Chat/ChatItem/CIFileView.swift` |
| Link preview | `Shared/Views/Chat/ChatItem/CILinkView.swift` |
| Call event | `Shared/Views/Chat/ChatItem/CICallItemView.swift` |
| Metadata | `Shared/Views/Chat/ChatItem/CIMetaView.swift` |
| Message info | `Shared/Views/Chat/ChatItemInfoView.swift` |
| System event | `Shared/Views/Chat/ChatItem/CIEventView.swift` |
| Deleted placeholder | `Shared/Views/Chat/ChatItem/DeletedItemView.swift` |
| Moderated placeholder | `Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift` |
| Text content | `Shared/Views/Chat/ChatItem/MsgContentView.swift` |
| Group invitation | `Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift` |
| Feature event | `Shared/Views/Chat/ChatItem/CIChatFeatureView.swift` |
| Decryption error | `Shared/Views/Chat/ChatItem/CIRcvDecryptionError.swift` |
| Integrity error | `Shared/Views/Chat/ChatItem/IntegrityErrorItemView.swift` |
| Full-screen media | `Shared/Views/Chat/ChatItem/FullScreenMediaView.swift` |
| Animated image | `Shared/Views/Chat/ChatItem/AnimatedImageView.swift` |
| Framed voice | `Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift` |
| Member contact | `Shared/Views/Chat/ChatItem/CIMemberCreatedContactView.swift` |
