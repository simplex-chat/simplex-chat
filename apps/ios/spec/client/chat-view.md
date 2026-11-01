# SimpleX Chat iOS -- Chat View Module

> Technical specification for the message rendering, chat item types, and context menu actions in the conversation view.
>
> Related specs: [Compose Module](compose.md) | [State Management](../state.md) | [API Reference](../api.md) | [README](../README.md)
> Related product: [Chat View](../../product/views/chat.md)

**Source:** [`ChatView.swift`](../../Shared/Views/Chat/ChatView.swift) | [`ChatInfoView.swift`](../../Shared/Views/Chat/ChatInfoView.swift) | [`GroupChatInfoView.swift`](../../Shared/Views/Chat/Group/GroupChatInfoView.swift) | [`ChannelMembersView.swift`](../../Shared/Views/Chat/Group/ChannelMembersView.swift) | [`ChannelRelaysView.swift`](../../Shared/Views/Chat/Group/ChannelRelaysView.swift)

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
9. [Terminal Layout](#9-terminal-layout)

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

## [2. ChatView](../../Shared/Views/Chat/ChatView.swift#L18-L3210)

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
- Trailing button: navigates to [`ChatInfoView`](../../Shared/Views/Chat/ChatInfoView.swift#L93) (direct) or [`GroupChatInfoView`](../../Shared/Views/Chat/Group/GroupChatInfoView.swift#L16) (group)
- Search button: toggles in-chat message search

### Scroll Behavior
- Auto-scrolls to bottom on new sent/received messages (if already near bottom)
- "Scroll to bottom" floating button when scrolled up
- `openAroundItemId` support: scrolls to a specific message (e.g., from search or notification)

### Key Functions

| Function | Line | Description |
|----------|------|-------------|
| [`body`](../../Shared/Views/Chat/ChatView.swift#L75) | L75 | Main view body |
| [`initChatView()`](../../Shared/Views/Chat/ChatView.swift#L660) | L660 | Initializes chat view state on appear |
| [`chatItemsList()`](../../Shared/Views/Chat/ChatView.swift#L817) | L817 | Builds the scrollable message list |
| [`scrollToItem(_:)`](../../Shared/Views/Chat/ChatView.swift#L731) | L731 | Scrolls to a specific message by ID |
| [`searchToolbar()`](../../Shared/Views/Chat/ChatView.swift#L765) | L765 | In-chat search toolbar UI |
| [`searchTextChanged(_:)`](../../Shared/Views/Chat/ChatView.swift#L1095) | L1095 | Handles search query changes |
| [`loadChatItems(_:_:)`](../../Shared/Views/Chat/ChatView.swift#L1531) | L1531 | Loads chat items with pagination |
| [`filtered(_:)`](../../Shared/Views/Chat/ChatView.swift#L803) | L803 | Filters items by content type |
| [`callButton(_:_:imageName:)`](../../Shared/Views/Chat/ChatView.swift#L1273) | L1273 | Audio/video call toolbar button |
| [`searchButton()`](../../Shared/Views/Chat/ChatView.swift#L1293) | L1293 | Search toggle toolbar button |
| [`addMembersButton()`](../../Shared/Views/Chat/ChatView.swift#L1361) | L1361 | Group add-members toolbar button |
| [`forwardSelectedMessages()`](../../Shared/Views/Chat/ChatView.swift#L1420) | L1420 | Forwards batch-selected messages |
| [`deletedSelectedMessages()`](../../Shared/Views/Chat/ChatView.swift#L1411) | L1411 | Deletes batch-selected messages |
| [`onChatItemsUpdated()`](../../Shared/Views/Chat/ChatView.swift#L1572) | L1572 | Reacts to chat items model changes |
| [`contentFilterMenu(withLabel:)`](../../Shared/Views/Chat/ChatView.swift#L1301) | L1301 | Content filter dropdown menu |

### Supporting Types

| Type | Line | Description |
|------|------|-------------|
| [`ChatItemWithMenu`](../../Shared/Views/Chat/ChatView.swift#L1600) | L1600 | Wraps each chat item with context menu |
| [`FloatingButtonModel`](../../Shared/Views/Chat/ChatView.swift#L2787) | L2787 | Manages scroll-to-bottom button state |
| [`ReactionContextMenu`](../../Shared/Views/Chat/ChatView.swift#L2974) | L2974 | Reaction picker context menu |
| [`ToggleNtfsButton`](../../Shared/Views/Chat/ChatView.swift#L3072) | L3072 | Mute/unmute notifications button |
| [`ContentFilter`](../../Shared/Views/Chat/ChatView.swift#L3124) | L3124 | Enum for message content filter types |
| [`deleteMessages()`](../../Shared/Views/Chat/ChatView.swift#L2870) | L2870 | Deletes messages with confirmation |
| [`archiveReports()`](../../Shared/Views/Chat/ChatView.swift#L2917) | L2917 | Archives report messages |

---

## [3. ChatItemView](../../Shared/Views/Chat/ChatItemView.swift#L42)

**File**: [`Shared/Views/Chat/ChatItemView.swift`](../../Shared/Views/Chat/ChatItemView.swift)

Routes each `ChatItem` to the appropriate renderer based on its `CIContent` type:

### Content Types (CIContent enum)

| Content Type | Renderer | Line | Description |
|-------------|----------|------|-------------|
| `sndMsgContent` / `rcvMsgContent` | [`FramedItemView`](../../Shared/Views/Chat/ChatItem/FramedItemView.swift#L14) | L14 | Standard sent/received text+media message |
| `sndDeleted` / `rcvDeleted` | [`DeletedItemView`](../../Shared/Views/Chat/ChatItem/DeletedItemView.swift#L14) | L14 | Locally deleted message placeholder |
| `sndCall` / `rcvCall` | [`CICallItemView`](../../Shared/Views/Chat/ChatItem/CICallItemView.swift#L13) | L13 | Call event (missed, ended, duration) |
| `rcvIntegrityError` | [`IntegrityErrorItemView`](../../Shared/Views/Chat/ChatItem/IntegrityErrorItemView.swift#L14) | L14 | Message integrity error |
| `rcvDecryptionError` | [`CIRcvDecryptionError`](../../Shared/Views/Chat/ChatItem/CIRcvDecryptionError.swift#L16) | L16 | Decryption failure |
| `sndGroupInvitation` / `rcvGroupInvitation` | [`CIGroupInvitationView`](../../Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift#L14) | L14 | Group invite |
| `sndGroupEvent` / `rcvGroupEvent` | [`CIEventView`](../../Shared/Views/Chat/ChatItem/CIEventView.swift#L14) | L14 | Group system event |
| `rcvConnEvent` / `sndConnEvent` | [`CIEventView`](../../Shared/Views/Chat/ChatItem/CIEventView.swift#L14) | L14 | Connection event |
| `rcvChatFeature` / `sndChatFeature` | [`CIChatFeatureView`](../../Shared/Views/Chat/ChatItem/CIChatFeatureView.swift#L14) | L14 | Feature toggle event |
| `rcvChatPreference` / `sndChatPreference` | [`CIFeaturePreferenceView`](../../Shared/Views/Chat/ChatItem/CIFeaturePreferenceView.swift#L14) | L14 | Preference change |
| `invalidJSON` | [`CIInvalidJSONView`](../../Shared/Views/Chat/ChatItem/CIInvalidJSONView.swift#L14) | L14 | Failed to decode |

### Bubble Direction
- Sent messages: aligned right, sender-colored bubble
- Received messages: aligned left, receiver-colored bubble
- Events/system messages: centered, no bubble

### Appearance Dependencies
Each [`ChatItemWithMenu`](../../Shared/Views/Chat/ChatView.swift#L1600) may depend on the previous and next items for visual decisions:
- Whether to show the sender name (group messages, different sender than previous)
- Whether to show the tail on the bubble (last consecutive message from same sender)
- Date separator between messages on different days

`ChatItemDummyModel.shared.sendUpdate()` forces a re-render of all items when global appearance changes.

### Channel Message Rendering (`.channelRcv`)

Channel messages (`CIDirection.channelRcv`) are rendered with the group avatar and group name as sender, with "channel" as the role label. This mirrors the `.groupRcv` path's `showGroupAsSender` visual but uses a dedicated code branch in [`chatItemListView()`](../../Shared/Views/Chat/ChatView.swift#L1846).

Key differences from `.groupRcv`:
- No `prevMember`/`memCount` logic — channels have no per-member identity
- Always shows group avatar (via `ProfileImage` with `groupInfo.image` / `groupInfo.chatIconName`)
- Tapping avatar opens `showChatInfoSheet` (not member info)
- [`shouldShowAvatar()`](../../Shared/Views/Chat/ChatView.swift#L1670) treats consecutive `.channelRcv` items as same sender
- [`getItemSeparation()`](../../Shared/Views/Chat/ChatView.swift#L1649) treats consecutive `.channelRcv` items as `sameMemberAndDirection`
- [`showMemberImage()`](../../Shared/Views/Chat/ChatView.swift#L2116) returns `true` when previous item is `.channelRcv` (different sender type)
- [`memberToModerate()`](../../SimpleXChat/ChatTypes.swift#L3297) returns `nil` for `.channelRcv` (no per-member moderation)

---

## 4. Message Renderers

### [FramedItemView](../../Shared/Views/Chat/ChatItem/FramedItemView.swift#L14)

**File**: [`Shared/Views/Chat/ChatItem/FramedItemView.swift`](../../Shared/Views/Chat/ChatItem/FramedItemView.swift)

The standard message bubble. Renders:
- Quote/reply preview (if replying to another message)
- Forwarded indicator
- Sender name (in groups)
- Message content (`MsgContentView` with markdown)
- Attached media (image, video, voice, file, link preview)
- Reaction summary bar
- Metadata line (`CIMetaView`)

### [EmojiItemView](../../Shared/Views/Chat/ChatItem/EmojiItemView.swift#L14)

**File**: [`Shared/Views/Chat/ChatItem/EmojiItemView.swift`](../../Shared/Views/Chat/ChatItem/EmojiItemView.swift)

Renders emoji-only messages (messages containing only emoji characters) in a larger font without a bubble background.

### [MsgContentView](../../Shared/Views/Chat/ChatItem/MsgContentView.swift#L28)

**File**: [`Shared/Views/Chat/ChatItem/MsgContentView.swift`](../../Shared/Views/Chat/ChatItem/MsgContentView.swift)

Renders message text with SimpleX markdown formatting (bold, italic, code, links, mentions). Blocks of `markdownBlocks` (headings, lists, quotes, code blocks, tables, rules) are drawn as a column of views by `MarkdownBlocksView`; code blocks and tables scroll sideways.

Text colour: `textColor`, else `messageTextColor(theme)`: `onSurface`, at 95% in dark themes. `FramedItemView` passes `onSurface` for quote text (terminal rows: 70%). `MarkdownView` (chat item info, group welcome message, profile description) draws its text in `messageTextColor(theme)`.

### DeletedItemView / MarkedDeletedItemView

**Files**: [`Shared/Views/Chat/ChatItem/DeletedItemView.swift`](../../Shared/Views/Chat/ChatItem/DeletedItemView.swift) | [`Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift`](../../Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift)

- [`DeletedItemView`](../../Shared/Views/Chat/ChatItem/DeletedItemView.swift#L14): Placeholder for locally deleted messages
- [`MarkedDeletedItemView`](../../Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift#L14): Shows "message deleted" with optional moderation info (who deleted, when)

### [CIEventView](../../Shared/Views/Chat/ChatItem/CIEventView.swift#L14)

**File**: [`Shared/Views/Chat/ChatItem/CIEventView.swift`](../../Shared/Views/Chat/ChatItem/CIEventView.swift)

Centered system event text for group events (member joined, left, role changed) and connection events.

### [CIGroupInvitationView](../../Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift#L14)

**File**: [`Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift`](../../Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift)

Renders group invitation with accept/reject buttons.

---

## 5. Media Views

### [CIImageView](../../Shared/Views/Chat/ChatItem/CIImageView.swift#L14)

**File**: [`Shared/Views/Chat/ChatItem/CIImageView.swift`](../../Shared/Views/Chat/ChatItem/CIImageView.swift)

Renders inline images. Tapping opens `FullScreenMediaView` for zooming/panning. Images are compressed to `MAX_IMAGE_SIZE` (255KB) before sending.

### [CIVideoView](../../Shared/Views/Chat/ChatItem/CIVideoView.swift#L16)

**File**: [`Shared/Views/Chat/ChatItem/CIVideoView.swift`](../../Shared/Views/Chat/ChatItem/CIVideoView.swift)

Renders video thumbnails with play button. Tapping opens video player. Videos above auto-receive threshold require manual download.

### CIVoiceView / FramedCIVoiceView

**Files**: [`Shared/Views/Chat/ChatItem/CIVoiceView.swift`](../../Shared/Views/Chat/ChatItem/CIVoiceView.swift) | [`Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift`](../../Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift)

Renders voice messages with waveform visualization, play/pause control, and duration. [`FramedCIVoiceView`](../../Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift#L16) is the version inside a message bubble with additional context.

### [CIFileView](../../Shared/Views/Chat/ChatItem/CIFileView.swift#L14)

**File**: [`Shared/Views/Chat/ChatItem/CIFileView.swift`](../../Shared/Views/Chat/ChatItem/CIFileView.swift)

Renders file attachments with filename, size, and download/open actions. Shows transfer progress during upload/download.

### MarkdownFileView

**File**: [`Shared/Views/Chat/ChatItem/MarkdownFileView.swift`](../../Shared/Views/Chat/ChatItem/MarkdownFileView.swift)

Sheet with a viewer of Markdown files up to `MAX_MARKDOWN_VIEW_SIZE` (1 MB), formatted or as text, with Save. Opened by a tap on the file item, or when reception completes after the tap that started it.

### [CILinkView](../../Shared/Views/Chat/ChatItem/CILinkView.swift#L14)

**File**: [`Shared/Views/Chat/ChatItem/CILinkView.swift`](../../Shared/Views/Chat/ChatItem/CILinkView.swift)

Renders link preview cards with OpenGraph metadata (title, description, image).

### [AnimatedImageView](../../Shared/Views/Chat/ChatItem/AnimatedImageView.swift#L11)

**File**: [`Shared/Views/Chat/ChatItem/AnimatedImageView.swift`](../../Shared/Views/Chat/ChatItem/AnimatedImageView.swift)

Renders animated GIF images.

### [FullScreenMediaView](../../Shared/Views/Chat/ChatItem/FullScreenMediaView.swift#L16)

**File**: [`Shared/Views/Chat/ChatItem/FullScreenMediaView.swift`](../../Shared/Views/Chat/ChatItem/FullScreenMediaView.swift)

Full-screen media viewer with zoom, pan, and share actions. Supports images and videos.

---

## 6. Metadata & Info

### [CIMetaView](../../Shared/Views/Chat/ChatItem/CIMetaView.swift#L14)

**File**: [`Shared/Views/Chat/ChatItem/CIMetaView.swift`](../../Shared/Views/Chat/ChatItem/CIMetaView.swift)

Displays message metadata inline at the bottom of the bubble:
- Timestamp (sent time)
- Delivery status icon (sending, sent, delivered, read, error)
- Edit indicator (pencil icon if message was edited)
- Disappearing message timer (if timed message)

### [ChatItemInfoView](../../Shared/Views/Chat/ChatItemInfoView.swift#L13)

**File**: [`Shared/Views/Chat/ChatItemInfoView.swift`](../../Shared/Views/Chat/ChatItemInfoView.swift)

Detailed message information sheet (accessed via long-press menu "Info"):
- Full delivery history (per-member delivery status in groups)
- Edit history (all previous versions of edited messages)
- Forward chain info
- Message timestamps (created, updated, deleted)
- When the file expires, and below it, for a user without a badge, a button opening the badges sheet

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
| Info | All messages | Opens [`ChatItemInfoView`](../../Shared/Views/Chat/ChatItemInfoView.swift#L13) |
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

## 9. Terminal Layout

**File**: [`Shared/Views/Chat/ChatItem/TerminalItemView.swift`](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift)

Rendering of a chat whose `ChatInfo.activeChatLayout(profileChatLayout(currentUser))` is `.terminal`: full-width start-aligned rows, messages without bubbles, the sender name once per group of messages, avatars in groups only. Reference: `apps/multiplatform/spec/client/chat-view.md`, sections 4 and 10.

### Activation

```
ChatView.terminal            chat.chatInfo.activeChatLayout(profileChatLayout(currentUser)) == .terminal
cells                        .environment(\.terminalLayout, terminal)
layout change                scrollView.updateItems(items, true): visible cells rebuilt
background                   wallpaper background colour; image omitted
```

`EnvironmentValues.terminalLayout` is false outside the chat cells and the theme screens: chat item info and the chat list are drawn classic.

Layout resolution, in [`ChatTypes.swift`](../../SimpleXChat/ChatTypes.swift) and [`ChatModel.swift`](../../Shared/Model/ChatModel.swift):

```
ChatInfo.activeChatLayout(profileLayout)    chat uiThemes.chatLayout, else defaultChatLayout(profileLayout)
ChatInfo.defaultChatLayout(profileLayout)   ChatInfo.chatLayout (contact or group profile), else profileLayout
profileChatLayout(user)                     user uiThemes.chatLayout, else chatLayoutDefault (app setting), else classic
unknown values                              skipped
```

### Rows

[`ChatItemWithMenu.terminalItemRow`](../../Shared/Views/Chat/ChatView.swift#L2149) wraps `chatItemWithMenu` in [`TerminalChatItemRow`](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift#L346):

```
row padding        top: topGap, plus terminalSenderTopGap with a sender row
gutter             groups and channels: memberImageSize + chatItemTextPadding, with the sender avatar chatItemTextPadding from the start; other chats: none
                   one 12pt padding (user, I26): screen edge to the text or the avatar, avatar to the text, time to the screen edge
avatar             memberImageSize: scaledMemberImageSize, 34pt × chat list profileImageSize ÷ 63 (dynamicSize at the text size, at most 75); classic rows too
                   classic group bubbles: maximum width less the avatar column, scaledMemberImageSize + 8pt
                   classic sender name: memberImageSize + 10pt after the avatar's start, on the bubble edge (user, I23)
sender row         name and badge at terminalNameSize, 13.5 × 17/16 pt (primary for own items, secondary otherwise); role label 15pt after the name
                   start padding chatItemTextPadding; terminalSenderBottomGap below
selection          SelectedChatItem 8pt from the start; row moved by 28pt; selected row tint primary 10%
media width        classic maxWidth, at most listWidth less the selection offset, the gutter and two text edges: media end at the time's end padding
bottom padding     newest row: 8pt; above a date separator: terminalLevelStep; otherwise 0
```

| Function | Rule |
|----------|------|
| [`terminalItemShowsSender(item, older)`](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift#L125) | Item with a sender (`hasTerminalSender`), and the older item is absent, has no sender or has another sender. Same sender: channel as sender on both items, or `CIDirection.sameDirection` when neither item has it |
| [`terminalRowGap(item, older, separated, largeGap, hasSenderRow)`](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift#L137) | `terminalLevelStep` when the older item exists, the rows are not separated, and one holds: large gap, sender row, either item without a sender, text at the item top starting with a heading. Otherwise 0 |
| [`terminalHeadingGap(item)`](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift#L144) | `terminalLevelStep` above a heading that starts the text below a quote, header or block |
| [`terminalRowsSeparated(olderRow)`](../../Shared/Views/Chat/ChatView.swift#L2179) | The newest item of the older row is the chat banner, or its `getItemSeparation` has a date (against its newer item only for single and revealed rows) |
| [`chatItemSender(ci, range)`](../../Shared/Views/Chat/ChatView.swift#L2186) | Channel as sender: group name, role "channel", group image. Received direct: contact name and badge. Group member: `memberNames`, badge and role of a single member, member image (tap: member info). Own group items: membership name, badge, role, image. Other own items: user name and badge, or "Incognito" |
| `CIContent.hasTerminalSender` | Sent messages, deleted and moderated items, calls and group invitations; received content with `showMemberName` |

`ChatView` passes `olderRow`, the next merged item. `MergedItem.revealed`: true for `.single`, the flag of `.grouped`.

### Items

`chatItemWithMenu`:

```
alignment          start for both directions (alignEnd = sent && !terminal); maximum width: the row
clip               ChatItemClipped skipped; its tail disabled in terminal rows
go-to button       search results and marked deleted items: after the item
                   forwarded items: endButton of FramedItemView, placed by ItemWithMeta
reactions          chatItemTextPadding − 6pt from the start
```

Item views start at the text edge `chatItemTextPadding` (12pt):

```
FramedItemView        background, quote background and header backgrounds omitted
                      headers: terminalHeaderTopGap above, terminalHeaderBottomGap below
                      quote: quoteBar(), text onSurface 70%, terminalBlockTopGap above, terminalBlockBottomGap below
                      quote sender: terminalNameSize (13.5 × 17/16 pt, Dynamic Type), no spacing above the text
                      quote thumbnail: 12pt after the quote text; a quote without text and sender: 12pt after the bar (quotedMsgOrGap)
                      quote thumbnail end padding: chatItemTextPadding, as the time's
                      quote thumbnails and media: message corner clip (msgRectMaxRadius × roundness)
                      link preview and chat link cards: quote colour, message corner clip (terminalBlock)
                      link preview card: at most maxWidth wide
                      media width limit for captionless media only (mediaLimit)
                      captionless media, chat link without text: terminalBlockBottomGap below (metaPlacement)
MsgContentView        line box terminalLineBox; meta line for live messages only
                      text ends with the transparent copy of the meta (reserveSpaceForMeta)
MarkdownBlocksView    paragraph break, heading after another block: terminalLevelStep
                      block other than a heading after a heading: terminalHeadingBottomGap
                      heading size: body × 1.25^max(0, 3 − level), bold (terminalHeadingFont); classic chat items too (chatHeadings, user)
                      item info, welcome message and profile description: body × 1.5, 1.3, 1.15 for levels 1–3 (headingFont)
                      code, tables and rules: terminalBlockTopGap above, terminalBlockBottomGap below (cardMargins)
                      last block of a message that is not live: no bottom margin, MessageLastBlock
                      rules of a rule-only message: as wide as the copy of the meta, or the item less its text edges
                      when a quote or header is wider (ruleMinWidth: msgWidth less 24pt; terminalQuote reports its full width)
emoji                 EmojiItemView and FramedItemView: terminalEmojiTopGap, terminalEmojiBottomGap, MessageLastLine
voice                 CIVoiceView: received layout for both directions, terminalGap(4) above, terminalGap(6) below
                      FramedCIVoiceView: terminalGap(4) above, terminalGap(6) below, player background
                      VoiceMessagePlayer circle in the quote colour (chatItemFrameContextColor); classic: the bubble colour
files                 CIFileView: terminalFileTopGap above, icon without top padding
events                terminalEventPadding, line box, line limit removed
                      feature and preference icons without height; merged feature icons one event line high
cards                 DeletedItemView, MarkedDeletedItemView, CIMsgError, CIRcvDecryptionError, CIGroupInvitationView:
                      ChatItemClipped(tailVisible: false), time beside the card
```

### Time and Status

[`ItemWithMeta(showMeta, metaReserved, overlayMeta, endButton, meta, content)`](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift#L178):

```
classic            content(meta()): the meta drawn inside the item, as before
terminal           content(nil); the meta in a box with 12pt end padding and the caption line box
iOS 16             TerminalMetaLayout
iOS 15             HStack(alignment: .lastTextBaseline): content, end button, meta
```

[`TerminalMetaLayout`](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift#L234):

```
content width      row width less the wider of: the end button; the meta, without metaReserved and overlayMeta
meta baseline      on the last text baseline of the content
meta below         content reports MessageLastLine.right and its right edge plus the meta width exceeds the row:
                   terminalMetaGap under the content
no baseline        meta ends terminalMetaGap above the content bottom
last block         MessageLastBlock.bottom at the content bottom, meta beside it: terminalBlockBottomGap below the content
overlayMeta        content and meta wider than the row: white meta over the media corner, terminalMetaGap above its bottom
end button         row end, centred between the content top and the meta top (overlayMeta: on the content)
row height         at least the meta height
```

`metaReserved`: content with its own reserve for the time: text with the copy, files, emoji, events.

[`ChatEventWithMeta(chatItem) { ts in }`](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift#L311) passes the end of the event text, placed after the classic separator: classic `chatItem.timestampText`; terminal `metaCopyJoiner`, two no-break spaces and the transparent unbreakable time in regular weight.

Meta copy, `ciMetaText(unbreakable: true)`:

```
separators         no-break spaces
icons              word joiner after the edit, timer and via-proxy icons
time               no-break space inside
prefix             metaCopyJoiner(rightToLeft): U+2060, then U+200E or U+200F; three no-break spaces
content            file encryption and signature included (MsgContentView encrypted, signedFileVerified)
```

### Spacing and Line Heights

Constants at the top of `TerminalItemView.swift`: the Kotlin values in sp at its 16sp body, converted by `terminalGap`, value × body point size ÷ 16 (17pt body by default; Dynamic Type included):

```
terminalBlockTopGap         5.75
terminalBlockBottomGap      5
terminalLevelStep           4.75
terminalHeadingBottomGap    2.375
terminalSenderTopGap        6.5
terminalSenderBottomGap     5.5
terminalHeaderTopGap        2.5
terminalHeaderBottomGap     1.5
terminalEventTopGap         8.25
terminalEventBottomGap      1.5
terminalFileTopGap          2
terminalEmojiTopGap         4.25
terminalEmojiBottomGap      1.5
terminalMetaGap             6
```

[`terminalLineBox(font, color, theme, capsOf:)`](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift#L65), a `TerminalLineBox` view modifier:

```
line height        font size × 22/16 × f(w)
f(w)               1 − 0.1515 × (1 − w); nil colour: 1
w                  L* contrast of the colour against the background ÷ that of messageTextColor, 0…1
extra height       split by ascent and descent: lineSpacing between lines, padding above and below
headings           capsOf body font: body line height scaled by the size ratio; capitals placed as body capitals
taps               handleTextTaps lays the text out with the same lineSpacing (withLineSpacing)
```

### Theme Screens

| Screen | `terminalLayout` |
|--------|------------------|
| Appearance: wallpaper presets | `profileChatLayout` of the theme destination: app or current profile |
| Customize theme | `profileChatLayout(nil)` |
| Profile theme | `profileChatLayout(currentUser)` |
| Chat theme | `activeChatLayout(profileChatLayout(currentUser))` of the chat |

```
ChatThemePreview          two TerminalChatItemRow rows: "alice", then "bob" as own; both layouts in one ZStack, one height
background                wallpaper background colour, image omitted; preset tiles drawn as colours
WallpaperPresetSelector   custom image tile hidden
WallpaperSetupView        image scale type, scale slider and wallpaper tint hidden
```

---

## GroupChatInfoView — Channel Adaptations

When `groupInfo.useRelays == true`, [`GroupChatInfoView`](../../Shared/Views/Chat/Group/GroupChatInfoView.swift#L16) adapts its sections:

### Section Structure (Channel)

| Section | Owner | Subscriber |
|---------|-------|-----------|
| 1. Links & Members | Channel link (manage via GroupLinkView), Owners & subscribers | Channel link (read-only QR from `groupProfile.publicGroup?.groupLink`), Owners |
| 2. Profile & Welcome | Edit channel profile, Welcome message | Welcome message (if exists) |
| 3. Theme & TTL | Chat theme, Delete messages after | Chat theme, Delete messages after |
| 4. Actions | Chat relays, Clear chat, Delete channel | Chat relays, Clear chat, Leave channel |

**Hidden for channels:** Member support, group reports, user support chat, send receipts, inline members list, group preferences.

### Label Replacements

All "group" labels are replaced with "channel" equivalents via `groupInfo.useRelays ? "Channel..." :` ternary prepended before existing `businessChat` ternary. Affected: delete/leave buttons, delete/leave alerts, remove member alert, edit profile button, group link nav title. Channel link button uses a separate `channelLinkButton()` with hardcoded "Channel link" label.

### [`channelMembersButton()`](../../Shared/Views/Chat/Group/GroupChatInfoView.swift#L627) → [`ChannelMembersView`](../../Shared/Views/Chat/Group/ChannelMembersView.swift)

Navigates to a dedicated members view with two sections:
- **Owners**: current user (if owner) + members with `memberRole >= .owner`
- **Subscribers** (admin+ only): members with `memberRole < .owner`

Member rows show profile image, display name (with verified shield), connection status, and role badge. Non-user rows link to `GroupMemberInfoView`.

### Channel Link

Owner sees [`channelLinkButton()`](../../Shared/Views/Chat/Group/GroupChatInfoView.swift#L605) (navigates to `GroupLinkView` for full link management), guarded by `groupInfo.isOwner && groupLink != nil` — channel links can only be created during channel creation, not from the info view. A TODO marks the need for protocol changes to allow other owners to manage the same channel link. Non-owner sees read-only QR code displaying `groupProfile.publicGroup?.groupLink` via `SimpleXLinkQRCode`. `apiGetGroupLink` is skipped in `onAppear` for non-owner channels.

Groups use separate [`groupLinkButton()`](../../Shared/Views/Chat/Group/GroupChatInfoView.swift#L593) which supports both "Create group link" and "Group link" labels.

### [`channelRelaysButton()`](../../Shared/Views/Chat/Group/GroupChatInfoView.swift#L639) → [`ChannelRelaysView`](../../Shared/Views/Chat/Group/ChannelRelaysView.swift)

Navigates to relay list view with role-based branches:
- **Owner**: loads `[GroupRelay]` via [`apiGetGroupRelays`](../../Shared/Model/SimpleXAPI.swift#L1839) (owner-only API, guarded by `assertUserGroupRole GROwner` on backend). Joins with `chatModel.groupMembers` by `groupMemberId` for display names. Shows status indicators (colored circle + `RelayStatus.text`). When `relayStatus == .rsRejected` the indicator dot is red and the text reads "rejected", matching the `connFailed`/`removed` rendering.
- **Member**: filters `chatModel.groupMembers` by `.memberRole == .relay`. Shows relay member display names only (no status data).
- **Add relay sheet**: `existingRelayIds` excludes every `chatRelayId` present in `groupRelays` regardless of `relayStatus`, so an already-listed relay (including `.rsInactive` and `.rsRejected`) cannot be re-added from the sheet. This mirrors the backend gate at `APIAddGroupRelays` (`existingRelayIds`), which rejects duplicate `chatRelayId`s; operator must remove the relay first via the swipe action.

### Relay Rejection Surface

When a relay operator runs `/leave #channel`, the relay sends `x.grp.relay.reject` over the owner-relay direct contact channel. Owner-side handling: the corresponding `GroupRelay.relayStatus` transitions `RSInvited → RSRejected`; the relay's `GroupMember.memberStatus` is set to `.memLeft` so the owner UI renders the rejected relay identically to one that explicitly ran `/leave` (`.memRejected` is reserved for the knocking-admission flow). The transition surfaces through `CEvtGroupRelayUpdated`. In `GroupMemberInfoView`, an additional `Status: rejected by relay operator` info row appears when `groupRelay?.relayStatus == .rsRejected`. The status is final on the owner side — clearable only by the relay operator running `/group allow #<channel>`, which has no owner-facing event.

### Leave Button Logic

Sole channel owner cannot leave (only delete). Guard: `members.filter({ $0.wrapped.memberRole == .owner && $0.wrapped.groupMemberId != groupInfo.membership.groupMemberId }).count > 0`.

---

## Source Files

| File | Path | Line |
|------|------|------|
| Chat view | [`Shared/Views/Chat/ChatView.swift`](../../Shared/Views/Chat/ChatView.swift) | [L18](../../Shared/Views/Chat/ChatView.swift#L18) |
| Item router | [`Shared/Views/Chat/ChatItemView.swift`](../../Shared/Views/Chat/ChatItemView.swift) | [L42](../../Shared/Views/Chat/ChatItemView.swift#L42) |
| Framed bubble | [`Shared/Views/Chat/ChatItem/FramedItemView.swift`](../../Shared/Views/Chat/ChatItem/FramedItemView.swift) | [L14](../../Shared/Views/Chat/ChatItem/FramedItemView.swift#L14) |
| Emoji message | [`Shared/Views/Chat/ChatItem/EmojiItemView.swift`](../../Shared/Views/Chat/ChatItem/EmojiItemView.swift) | [L14](../../Shared/Views/Chat/ChatItem/EmojiItemView.swift#L14) |
| Image view | [`Shared/Views/Chat/ChatItem/CIImageView.swift`](../../Shared/Views/Chat/ChatItem/CIImageView.swift) | [L14](../../Shared/Views/Chat/ChatItem/CIImageView.swift#L14) |
| Video view | [`Shared/Views/Chat/ChatItem/CIVideoView.swift`](../../Shared/Views/Chat/ChatItem/CIVideoView.swift) | [L16](../../Shared/Views/Chat/ChatItem/CIVideoView.swift#L16) |
| Voice view | [`Shared/Views/Chat/ChatItem/CIVoiceView.swift`](../../Shared/Views/Chat/ChatItem/CIVoiceView.swift) | [L14](../../Shared/Views/Chat/ChatItem/CIVoiceView.swift#L14) |
| File view | [`Shared/Views/Chat/ChatItem/CIFileView.swift`](../../Shared/Views/Chat/ChatItem/CIFileView.swift) | [L14](../../Shared/Views/Chat/ChatItem/CIFileView.swift#L14) |
| Link preview | [`Shared/Views/Chat/ChatItem/CILinkView.swift`](../../Shared/Views/Chat/ChatItem/CILinkView.swift) | [L14](../../Shared/Views/Chat/ChatItem/CILinkView.swift#L14) |
| Call event | [`Shared/Views/Chat/ChatItem/CICallItemView.swift`](../../Shared/Views/Chat/ChatItem/CICallItemView.swift) | [L13](../../Shared/Views/Chat/ChatItem/CICallItemView.swift#L13) |
| Metadata | [`Shared/Views/Chat/ChatItem/CIMetaView.swift`](../../Shared/Views/Chat/ChatItem/CIMetaView.swift) | [L14](../../Shared/Views/Chat/ChatItem/CIMetaView.swift#L14) |
| Message info | [`Shared/Views/Chat/ChatItemInfoView.swift`](../../Shared/Views/Chat/ChatItemInfoView.swift) | [L13](../../Shared/Views/Chat/ChatItemInfoView.swift#L13) |
| System event | [`Shared/Views/Chat/ChatItem/CIEventView.swift`](../../Shared/Views/Chat/ChatItem/CIEventView.swift) | [L14](../../Shared/Views/Chat/ChatItem/CIEventView.swift#L14) |
| Deleted placeholder | [`Shared/Views/Chat/ChatItem/DeletedItemView.swift`](../../Shared/Views/Chat/ChatItem/DeletedItemView.swift) | [L14](../../Shared/Views/Chat/ChatItem/DeletedItemView.swift#L14) |
| Moderated placeholder | [`Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift`](../../Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift) | [L14](../../Shared/Views/Chat/ChatItem/MarkedDeletedItemView.swift#L14) |
| Text content | [`Shared/Views/Chat/ChatItem/MsgContentView.swift`](../../Shared/Views/Chat/ChatItem/MsgContentView.swift) | [L28](../../Shared/Views/Chat/ChatItem/MsgContentView.swift#L28) |
| Group invitation | [`Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift`](../../Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift) | [L14](../../Shared/Views/Chat/ChatItem/CIGroupInvitationView.swift#L14) |
| Feature event | [`Shared/Views/Chat/ChatItem/CIChatFeatureView.swift`](../../Shared/Views/Chat/ChatItem/CIChatFeatureView.swift) | [L14](../../Shared/Views/Chat/ChatItem/CIChatFeatureView.swift#L14) |
| Decryption error | [`Shared/Views/Chat/ChatItem/CIRcvDecryptionError.swift`](../../Shared/Views/Chat/ChatItem/CIRcvDecryptionError.swift) | [L16](../../Shared/Views/Chat/ChatItem/CIRcvDecryptionError.swift#L16) |
| Integrity error | [`Shared/Views/Chat/ChatItem/IntegrityErrorItemView.swift`](../../Shared/Views/Chat/ChatItem/IntegrityErrorItemView.swift) | [L14](../../Shared/Views/Chat/ChatItem/IntegrityErrorItemView.swift#L14) |
| Full-screen media | [`Shared/Views/Chat/ChatItem/FullScreenMediaView.swift`](../../Shared/Views/Chat/ChatItem/FullScreenMediaView.swift) | [L16](../../Shared/Views/Chat/ChatItem/FullScreenMediaView.swift#L16) |
| Animated image | [`Shared/Views/Chat/ChatItem/AnimatedImageView.swift`](../../Shared/Views/Chat/ChatItem/AnimatedImageView.swift) | [L11](../../Shared/Views/Chat/ChatItem/AnimatedImageView.swift#L11) |
| Framed voice | [`Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift`](../../Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift) | [L16](../../Shared/Views/Chat/ChatItem/FramedCIVoiceView.swift#L16) |
| Member contact | [`Shared/Views/Chat/ChatItem/CIMemberCreatedContactView.swift`](../../Shared/Views/Chat/ChatItem/CIMemberCreatedContactView.swift) | [L14](../../Shared/Views/Chat/ChatItem/CIMemberCreatedContactView.swift#L14) |
| Terminal layout | [`Shared/Views/Chat/ChatItem/TerminalItemView.swift`](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift) | [L4](../../Shared/Views/Chat/ChatItem/TerminalItemView.swift#L4) |
| Channel members | [`Shared/Views/Chat/Group/ChannelMembersView.swift`](../../Shared/Views/Chat/Group/ChannelMembersView.swift) | [L12](../../Shared/Views/Chat/Group/ChannelMembersView.swift#L12) |
| Channel relays | [`Shared/Views/Chat/Group/ChannelRelaysView.swift`](../../Shared/Views/Chat/Group/ChannelRelaysView.swift) | [L12](../../Shared/Views/Chat/Group/ChannelRelaysView.swift#L12) |
