# SimpleX Chat iOS -- Chat View Module

> Technical specification for the message rendering, chat item types, and context menu actions in the conversation view.
>
> Related specs: [Compose Module](compose.md) | [State Management](../state.md) | [API Reference](../api.md) | [README](../README.md)
> Related product: [Chat View](../../product/views/chat.md)

**Source:** [`ChatView.swift`](../../Shared/Views/Chat/ChatView.swift) | [`ChatInfoView.swift`](../../Shared/Views/Chat/ChatInfoView.swift) | [`GroupChatInfoView.swift`](../../Shared/Views/Chat/Group/GroupChatInfoView.swift)

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

## [2. ChatView](../../Shared/Views/Chat/ChatView.swift#L17-L3117)

**File**: [`Shared/Views/Chat/ChatView.swift`](../../Shared/Views/Chat/ChatView.swift)

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
- Trailing button: navigates to [`ChatInfoView`](../../Shared/Views/Chat/ChatInfoView.swift#L92) (direct) or [`GroupChatInfoView`](../../Shared/Views/Chat/Group/GroupChatInfoView.swift#L15) (group)
- Search button: toggles in-chat message search

### Scroll Behavior
- Auto-scrolls to bottom on new sent/received messages (if already near bottom)
- "Scroll to bottom" floating button when scrolled up
- `openAroundItemId` support: scrolls to a specific message (e.g., from search or notification)

### Key Functions

| Function | Line | Description |
|----------|------|-------------|
| [`body`](../../Shared/Views/Chat/ChatView.swift#L74) | L74 | Main view body |
| [`initChatView()`](../../Shared/Views/Chat/ChatView.swift#L672) | L672 | Initializes chat view state on appear |
| [`chatItemsList()`](../../Shared/Views/Chat/ChatView.swift#L814) | L814 | Builds the scrollable message list |
| [`scrollToItem(_:)`](../../Shared/Views/Chat/ChatView.swift#L731) | L731 | Scrolls to a specific message by ID |
| [`searchToolbar()`](../../Shared/Views/Chat/ChatView.swift#L764) | L764 | In-chat search toolbar UI |
| [`searchTextChanged(_:)`](../../Shared/Views/Chat/ChatView.swift#L1087) | L1087 | Handles search query changes |
| [`loadChatItems(_:_:)`](../../Shared/Views/Chat/ChatView.swift#L1519) | L1519 | Loads chat items with pagination |
| [`filtered(_:)`](../../Shared/Views/Chat/ChatView.swift#L801) | L801 | Filters items by content type |
| [`callButton(_:_:imageName:)`](../../Shared/Views/Chat/ChatView.swift#L1264) | L1264 | Audio/video call toolbar button |
| [`searchButton()`](../../Shared/Views/Chat/ChatView.swift#L1284) | L1284 | Search toggle toolbar button |
| [`addMembersButton()`](../../Shared/Views/Chat/ChatView.swift#L1352) | L1352 | Group add-members toolbar button |
| [`forwardSelectedMessages()`](../../Shared/Views/Chat/ChatView.swift#L1409) | L1409 | Forwards batch-selected messages |
| [`deletedSelectedMessages()`](../../Shared/Views/Chat/ChatView.swift#L1401) | L1401 | Deletes batch-selected messages |
| [`onChatItemsUpdated()`](../../Shared/Views/Chat/ChatView.swift#L1559) | L1559 | Reacts to chat items model changes |
| [`contentFilterMenu(withLabel:)`](../../Shared/Views/Chat/ChatView.swift#L1292) | L1292 | Content filter dropdown menu |

### Supporting Types

| Type | Line | Description |
|------|------|-------------|
| [`ChatItemWithMenu`](../../Shared/Views/Chat/ChatView.swift#L1586) | L1586 | Wraps each chat item with context menu |
| [`FloatingButtonModel`](../../Shared/Views/Chat/ChatView.swift#L2697) | L2697 | Manages scroll-to-bottom button state |
| [`ReactionContextMenu`](../../Shared/Views/Chat/ChatView.swift#L2882) | L2882 | Reaction picker context menu |
| [`ToggleNtfsButton`](../../Shared/Views/Chat/ChatView.swift#L2980) | L2980 | Mute/unmute notifications button |
| [`ContentFilter`](../../Shared/Views/Chat/ChatView.swift#L3031) | L3031 | Enum for message content filter types |
| [`deleteMessages()`](../../Shared/Views/Chat/ChatView.swift#L2779) | L2779 | Deletes messages with confirmation |
| [`archiveReports()`](../../Shared/Views/Chat/ChatView.swift#L2826) | L2826 | Archives report messages |

---

## [3. ChatItemView](../../Shared/Views/Chat/ChatItemView.swift#L41)

**File**: [`Shared/Views/Chat/ChatItemView.swift`](../../Shared/Views/Chat/ChatItemView.swift)

Routes each `ChatItem` to the appropriate renderer based on its `CIContent` type:

### Content Types (CIContent enum)

| Content Type | Renderer | Line | Description |
|-------------|----------|------|-------------|
| `sndMsgContent` / `rcvMsgContent` | [`FramedItemView`](../../Shared/Views/Chat/ChatItem/FramedItemView.swift#L13) | L13 | Standard sent/received text+media message |
| `sndDeleted` / `rcvDeleted` | [`DeletedItemView`](../../Shared/Views/Chat/ChatItem/DeletedItemView.swift#L13) | L13 | Locally deleted message placeholder |
| `sndCall` / `rcvCall` | [`CICallItemView`](../../Shared/Views/Chat/ChatItem/CICallItemView.swift#L13) | L13 | Call event (missed, ended, duration) |
| `rcvIntegrityError` | [`IntegrityErrorItemView`](../../Shared/Views/Chat/ChatItem/IntegrityErrorItemView.swift#L13) | L13 | Message integrity error |
| `rcvDecryptionError` | [`CIRcvDecryptionError`](../../Shared/Views/Chat/ChatItem/CIRcvDecryptionError.swift#L15) | L15 | Decryption failure |
| `sndGroupInvitation` / `rcvGroupInvitation` | [`CIGroupInvitationView`](../../Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift#L13) | L13 | Group invite |
| `sndGroupEvent` / `rcvGroupEvent` | [`CIEventView`](../../Shared/Views/Chat/ChatItem/CIEventView.swift#L13) | L13 | Group system event |
| `rcvConnEvent` / `sndConnEvent` | [`CIEventView`](../../Shared/Views/Chat/ChatItem/CIEventView.swift#L13) | L13 | Connection event |
| `rcvChatFeature` / `sndChatFeature` | [`CIChatFeatureView`](../../Shared/Views/Chat/ChatItem/CIChatFeatureView.swift#L13) | L13 | Feature toggle event |
| `rcvChatPreference` / `sndChatPreference` | [`CIFeaturePreferenceView`](../../Shared/Views/Chat/ChatItem/CIFeaturePreferenceView.swift#L13) | L13 | Preference change |
| `invalidJSON` | [`CIInvalidJSONView`](../../Shared/Views/Chat/ChatItem/CIInvalidJSONView.swift#L13) | L13 | Failed to decode |

### Bubble Direction
- Sent messages: aligned right, sender-colored bubble
- Received messages: aligned left, receiver-colored bubble
- Events/system messages: centered, no bubble

### Appearance Dependencies
Each [`ChatItemWithMenu`](../../Shared/Views/Chat/ChatView.swift#L1586) may depend on the previous and next items for visual decisions:
- Whether to show the sender name (group messages, different sender than previous)
- Whether to show the tail on the bubble (last consecutive message from same sender)
- Date separator between messages on different days

`ChatItemDummyModel.shared.sendUpdate()` forces a re-render of all items when global appearance changes.

---

## 4. Message Renderers

### [FramedItemView](../../Shared/Views/Chat/ChatItem/FramedItemView.swift#L13)

**File**: [`Shared/Views/Chat/ChatItem/FramedItemView.swift`](../../Shared/Views/Chat/ChatItem/FramedItemView.swift)

The standard message bubble. Renders:
- Quote/reply preview (if replying to another message)
- Forwarded indicator
- Sender name (in groups)
- Message content (`MsgContentView` with markdown)
- Attached media (image, video, voice, file, link preview)
- Reaction summary bar
- Metadata line (`CIMetaView`)

### [EmojiItemView](../../Shared/Views/Chat/ChatItem/EmojiItemView.swift#L13)

**File**: [`Shared/Views/Chat/ChatItem/EmojiItemView.swift`](../../Shared/Views/Chat/ChatItem/EmojiItemView.swift)

Renders emoji-only messages (messages containing only emoji characters) in a larger font without a bubble background.

### [MsgContentView](../../Shared/Views/Chat/ChatItem/MsgContentView.swift#L27)

**File**: [`Shared/Views/Chat/ChatItem/MsgContentView.swift`](../../Shared/Views/Chat/ChatItem/MsgContentView.swift)

Renders message text with SimpleX markdown formatting (bold, italic, code, links, mentions).

### DeletedItemView / MarkedDeletedItemView

**Files**: [`Shared/Views/Chat/ChatItem/DeletedItemView.swift`](../../Shared/Views/Chat/ChatItem/DeletedItemView.swift) | [`Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift`](../../Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift)

- [`DeletedItemView`](../../Shared/Views/Chat/ChatItem/DeletedItemView.swift#L13): Placeholder for locally deleted messages
- [`MarkedDeletedItemView`](../../Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift#L13): Shows "message deleted" with optional moderation info (who deleted, when)

### [CIEventView](../../Shared/Views/Chat/ChatItem/CIEventView.swift#L13)

**File**: [`Shared/Views/Chat/ChatItem/CIEventView.swift`](../../Shared/Views/Chat/ChatItem/CIEventView.swift)

Centered system event text for group events (member joined, left, role changed) and connection events.

### [CIGroupInvitationView](../../Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift#L13)

**File**: [`Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift`](../../Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift)

Renders group invitation with accept/reject buttons.

---

## 5. Media Views

### [CIImageView](../../Shared/Views/Chat/ChatItem/CIImageView.swift#L13)

**File**: [`Shared/Views/Chat/ChatItem/CIImageView.swift`](../../Shared/Views/Chat/ChatItem/CIImageView.swift)

Renders inline images. Tapping opens `FullScreenMediaView` for zooming/panning. Images are compressed to `MAX_IMAGE_SIZE` (255KB) before sending.

### [CIVideoView](../../Shared/Views/Chat/ChatItem/CIVideoView.swift#L15)

**File**: [`Shared/Views/Chat/ChatItem/CIVideoView.swift`](../../Shared/Views/Chat/ChatItem/CIVideoView.swift)

Renders video thumbnails with play button. Tapping opens video player. Videos above auto-receive threshold require manual download.

### CIVoiceView / FramedCIVoiceView

**Files**: [`Shared/Views/Chat/ChatItem/CIVoiceView.swift`](../../Shared/Views/Chat/ChatItem/CIVoiceView.swift) | [`Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift`](../../Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift)

Renders voice messages with waveform visualization, play/pause control, and duration. [`FramedCIVoiceView`](../../Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift#L15) is the version inside a message bubble with additional context.

### [CIFileView](../../Shared/Views/Chat/ChatItem/CIFileView.swift#L13)

**File**: [`Shared/Views/Chat/ChatItem/CIFileView.swift`](../../Shared/Views/Chat/ChatItem/CIFileView.swift)

Renders file attachments with filename, size, and download/open actions. Shows transfer progress during upload/download.

### [CILinkView](../../Shared/Views/Chat/ChatItem/CILinkView.swift#L13)

**File**: [`Shared/Views/Chat/ChatItem/CILinkView.swift`](../../Shared/Views/Chat/ChatItem/CILinkView.swift)

Renders link preview cards with OpenGraph metadata (title, description, image).

### [AnimatedImageView](../../Shared/Views/Chat/ChatItem/AnimatedImageView.swift#L10)

**File**: [`Shared/Views/Chat/ChatItem/AnimatedImageView.swift`](../../Shared/Views/Chat/ChatItem/AnimatedImageView.swift)

Renders animated GIF images.

### [FullScreenMediaView](../../Shared/Views/Chat/ChatItem/FullScreenMediaView.swift#L15)

**File**: [`Shared/Views/Chat/ChatItem/FullScreenMediaView.swift`](../../Shared/Views/Chat/ChatItem/FullScreenMediaView.swift)

Full-screen media viewer with zoom, pan, and share actions. Supports images and videos.

---

## 6. Metadata & Info

### [CIMetaView](../../Shared/Views/Chat/ChatItem/CIMetaView.swift#L13)

**File**: [`Shared/Views/Chat/ChatItem/CIMetaView.swift`](../../Shared/Views/Chat/ChatItem/CIMetaView.swift)

Displays message metadata inline at the bottom of the bubble:
- Timestamp (sent time)
- Delivery status icon (sending, sent, delivered, read, error)
- Edit indicator (pencil icon if message was edited)
- Disappearing message timer (if timed message)

### [ChatItemInfoView](../../Shared/Views/Chat/ChatItemInfoView.swift#L12)

**File**: [`Shared/Views/Chat/ChatItemInfoView.swift`](../../Shared/Views/Chat/ChatItemInfoView.swift)

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
| Info | All messages | Opens [`ChatItemInfoView`](../../Shared/Views/Chat/ChatItemInfoView.swift#L12) |
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

| File | Path | Line |
|------|------|------|
| Chat view | [`Shared/Views/Chat/ChatView.swift`](../../Shared/Views/Chat/ChatView.swift) | [L17](../../Shared/Views/Chat/ChatView.swift#L17) |
| Item router | [`Shared/Views/Chat/ChatItemView.swift`](../../Shared/Views/Chat/ChatItemView.swift) | [L41](../../Shared/Views/Chat/ChatItemView.swift#L41) |
| Framed bubble | [`Shared/Views/Chat/ChatItem/FramedItemView.swift`](../../Shared/Views/Chat/ChatItem/FramedItemView.swift) | [L13](../../Shared/Views/Chat/ChatItem/FramedItemView.swift#L13) |
| Emoji message | [`Shared/Views/Chat/ChatItem/EmojiItemView.swift`](../../Shared/Views/Chat/ChatItem/EmojiItemView.swift) | [L13](../../Shared/Views/Chat/ChatItem/EmojiItemView.swift#L13) |
| Image view | [`Shared/Views/Chat/ChatItem/CIImageView.swift`](../../Shared/Views/Chat/ChatItem/CIImageView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CIImageView.swift#L13) |
| Video view | [`Shared/Views/Chat/ChatItem/CIVideoView.swift`](../../Shared/Views/Chat/ChatItem/CIVideoView.swift) | [L15](../../Shared/Views/Chat/ChatItem/CIVideoView.swift#L15) |
| Voice view | [`Shared/Views/Chat/ChatItem/CIVoiceView.swift`](../../Shared/Views/Chat/ChatItem/CIVoiceView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CIVoiceView.swift#L13) |
| File view | [`Shared/Views/Chat/ChatItem/CIFileView.swift`](../../Shared/Views/Chat/ChatItem/CIFileView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CIFileView.swift#L13) |
| Link preview | [`Shared/Views/Chat/ChatItem/CILinkView.swift`](../../Shared/Views/Chat/ChatItem/CILinkView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CILinkView.swift#L13) |
| Call event | [`Shared/Views/Chat/ChatItem/CICallItemView.swift`](../../Shared/Views/Chat/ChatItem/CICallItemView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CICallItemView.swift#L13) |
| Metadata | [`Shared/Views/Chat/ChatItem/CIMetaView.swift`](../../Shared/Views/Chat/ChatItem/CIMetaView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CIMetaView.swift#L13) |
| Message info | [`Shared/Views/Chat/ChatItemInfoView.swift`](../../Shared/Views/Chat/ChatItemInfoView.swift) | [L12](../../Shared/Views/Chat/ChatItemInfoView.swift#L12) |
| System event | [`Shared/Views/Chat/ChatItem/CIEventView.swift`](../../Shared/Views/Chat/ChatItem/CIEventView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CIEventView.swift#L13) |
| Deleted placeholder | [`Shared/Views/Chat/ChatItem/DeletedItemView.swift`](../../Shared/Views/Chat/ChatItem/DeletedItemView.swift) | [L13](../../Shared/Views/Chat/ChatItem/DeletedItemView.swift#L13) |
| Moderated placeholder | [`Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift`](../../Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift) | [L13](../../Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift#L13) |
| Text content | [`Shared/Views/Chat/ChatItem/MsgContentView.swift`](../../Shared/Views/Chat/ChatItem/MsgContentView.swift) | [L27](../../Shared/Views/Chat/ChatItem/MsgContentView.swift#L27) |
| Group invitation | [`Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift`](../../Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift#L13) |
| Feature event | [`Shared/Views/Chat/ChatItem/CIChatFeatureView.swift`](../../Shared/Views/Chat/ChatItem/CIChatFeatureView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CIChatFeatureView.swift#L13) |
| Decryption error | [`Shared/Views/Chat/ChatItem/CIRcvDecryptionError.swift`](../../Shared/Views/Chat/ChatItem/CIRcvDecryptionError.swift) | [L15](../../Shared/Views/Chat/ChatItem/CIRcvDecryptionError.swift#L15) |
| Integrity error | [`Shared/Views/Chat/ChatItem/IntegrityErrorItemView.swift`](../../Shared/Views/Chat/ChatItem/IntegrityErrorItemView.swift) | [L13](../../Shared/Views/Chat/ChatItem/IntegrityErrorItemView.swift#L13) |
| Full-screen media | [`Shared/Views/Chat/ChatItem/FullScreenMediaView.swift`](../../Shared/Views/Chat/ChatItem/FullScreenMediaView.swift) | [L15](../../Shared/Views/Chat/ChatItem/FullScreenMediaView.swift#L15) |
| Animated image | [`Shared/Views/Chat/ChatItem/AnimatedImageView.swift`](../../Shared/Views/Chat/ChatItem/AnimatedImageView.swift) | [L10](../../Shared/Views/Chat/ChatItem/AnimatedImageView.swift#L10) |
| Framed voice | [`Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift`](../../Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift) | [L15](../../Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift#L15) |
| Member contact | [`Shared/Views/Chat/ChatItem/CIMemberCreatedContactView.swift`](../../Shared/Views/Chat/ChatItem/CIMemberCreatedContactView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CIMemberCreatedContactView.swift#L13) |
