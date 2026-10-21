# Chat View Specification

Source: `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt`

---

## Table of Contents

1. [Overview](#1-overview)
2. [ChatView Composable](#2-chatview-composable)
3. [Message List](#3-message-list)
4. [ChatItemView](#4-chatitemview)
5. [Message Types](#5-message-types)
6. [Context Menu Actions](#6-context-menu-actions)
7. [ChatInfoView](#7-chatinfoview)
8. [GroupChatInfoView](#8-groupchatinfoview)
9. [Source Files](#9-source-files)
10. [Terminal Layout](#10-terminal-layout)

---

## Executive Summary

The Chat View is the primary message display and interaction surface in SimpleX Chat. It is built around the `ChatView` composable (line ~96 in `ChatView.kt`), which orchestrates a `ChatLayout` containing a reverse-scrolling `LazyColumn` of `ChatItemView` items and a `ComposeView` for message input. The view supports direct chats, group chats, local notes, and contact connections, with per-chat theming, search/filter, multi-select, and side-panel info modals. Message rendering is delegated to type-specific composables in the `views/chat/item/` package.

---

## 1. Overview

```
ChatView
|-- ChatLayout
|   |-- ChatInfoToolbar            (top/bottom app bar with back, title, call, search, menu)
|   |-- SupportChatsCountToolbar   (reports/support banner, group only)
|   |-- ChatItemsList              (LazyColumnWithScrollBar, reverse layout)
|   |   |-- ChatViewListItem
|   |   |   |-- DateSeparator
|   |   |   |-- MemberNameAndRole  (group received messages)
|   |   |   |-- MemberImage        (group received messages)
|   |   |   |-- ChatItemView       (message type routing)
|   |   |   +-- TerminalChatItemRow (terminal layout, replaces the three lines above)
|   |   |       |-- sender row, avatar in groups, GoToItemButton
|   |   |       +-- ChatItemView
|   |   |-- ChatBannerView         (first item: chat profile banner)
|   |   +-- FloatingButtons        (scroll-to-bottom, unread counter)
|   |-- ComposeView               (message composition area)
|   |   |-- ContextItemView        (reply/edit/forward/report indicator)
|   |   |-- previewView            (link/media/voice/file preview)
|   |   +-- SendMsgView            (text input + send/voice/timed buttons)
|   |-- GroupMentions              (mention autocomplete popup)
|   |-- CommandsMenuView           (bot commands popup)
|   +-- ChooseAttachmentView       (bottom sheet for attachment type)
|-- ChatInfoView                   (contact info, end modal)
+-- GroupChatInfoView              (group management, end modal)
```

---

<a id="ChatView"></a>

## 2. ChatView Composable

**Location:** [`ChatView.kt#L99`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L99)

```kotlin
fun ChatView(
  chatsCtx: ChatModel.ChatsContext,
  staleChatId: State<String?>,
  scrollToItemId: MutableState<Long?>,
  onComposed: suspend (chatId: String) -> Unit
)
```

### State Management

| State Variable | Type | Purpose |
|---|---|---|
| `showSearch` | `MutableState<Boolean>` | Controls search bar visibility |
| `searchText` | `MutableState<String>` | Current search query text |
| `composeState` | `MutableState<ComposeState>` | Full compose area state (message, preview, context, mentions) |
| `attachmentOption` | `MutableState<AttachmentOption?>` | Selected attachment type from bottom sheet |
| `selectedChatItems` | `MutableState<Set<Long>?>` | Multi-select mode item IDs; `null` = selection off |
| `showCommandsMenu` | `MutableState<Boolean>` | Bot commands menu visibility |
| `contentFilter` | `MutableState<ContentFilter?>` | Active content type filter (images, videos, etc.) |
| `availableContent` | `MutableState<List<ContentFilter>>` | Content types available in this chat |
| `activeChat` | `State<Chat?>` | Derived from `chatModel.chats` matching `staleChatId` |
| `unreadCount` | `State<Int>` | Unread message count derived from chat stats |

### Chat Loading

On chat ID change (via `snapshotFlow` on `chatModel.chatId.value`, line ~162):

1. Marks unread chat as read (`markUnreadChatAsRead`)
2. Clears group members state
3. Resets search, content filter, and selection
4. Fetches available content types (`updateAvailableContent`)
5. For direct chats, loads contact info and connection stats
6. For groups with pending membership, opens member support chat

### Chat Type Routing

The outer `when (chatInfo)` (line ~229) branches:

| ChatInfo Type | Behavior |
|---|---|
| `ChatInfo.Direct`, `ChatInfo.Group`, `ChatInfo.Local` | Full `ChatLayout` with compose, search, reactions, per-chat theme |
| `ChatInfo.ContactConnection` | `ModalView` wrapping `ContactConnectionInfoView` |
| `ChatInfo.InvalidJSON` | `ModalView` with raw JSON display and share button |

---

## 3. Message List

**Location:** [`ChatView.kt#L1592`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L1592) (`ChatItemsList` composable)

The message list is a `LazyColumnWithScrollBar` with `reverseLayout = true`, meaning index 0 is the newest message at the bottom of the screen.

### Key Behaviors

- **Merged Items:** Messages are grouped via `MergedItems.create()` (line ~1653), which collapses consecutive similar system events into expandable groups. Revealed state is tracked in `revealedItems`.
- **Pagination:** `PreloadItems` triggers `loadMessages` with `ChatPagination.Before` (older) or `ChatPagination.Last` (newer) when the user scrolls near list boundaries.
- **Scroll To Item:** `scrollToItem` lambda supports animated scrolling to a specific item ID, used by search result taps and quoted message navigation.
- **Unread Marking:** `MarkItemsReadAfterDelay` composable marks newly visible received items as read after a brief delay.
- **Date Separators:** `DateSeparator` composable renders between messages when the date changes (via `ItemSeparation.date`).
- **Swipe to Reply:** `SwipeToDismiss` modifier on each item (EndToStart direction, 30dp threshold) sets `ComposeContextItem.QuotedItem`.
- **Selection Mode:** When `selectedChatItems` is non-null, a checkbox overlay appears on each item; a full-width clickable overlay toggles selection.

### Item Layout (ChatViewListItem)

- **Group received messages** with `showAvatar = true`: Column layout with `MemberNameAndRole` header, `MemberImage` (clickable to `showMemberInfo`), and message bubble.
- **Group received without avatar:** Indented to align with avatar-bearing messages.
- **Sent messages (group or direct):** Right-aligned with larger start padding.
- **Direct messages:** Symmetric padding (76dp opposite side).
- **Terminal layout:** When `LocalTerminalLayout.current` is true, all chat types render through `TerminalChatItemRow`, which wraps `ChatItemView`, instead of the branches above (see section 10). The selection offset applies to sent items too.

---

<a id="ChatItemView"></a>

## 4. ChatItemView

**Location:** [`item/ChatItemView.kt#L71`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L71)

```kotlin
fun ChatItemView(
  chatsCtx, rhId, chat, cItem, composeState, imageProvider,
  useLinkPreviews, linkMode, revealed, highlighted, hoveredItemId,
  range, selectedChatItems, searchIsNotBlank, fillMaxWidth,
  selectChatItem, deleteMessage, deleteMessages, archiveReports,
  receiveFile, cancelFile, joinGroup, acceptCall, acceptFeature,
  openDirectChat, forwardItem, scrollToItem, scrollToItemId,
  scrollToQuotedItemFromItem, setReaction, showItemDetails,
  reveal, showMemberInfo, showChatInfo, developerTools, showViaProxy,
  showTimestamp, itemSeparation, ...
)
```

The composable routes based on `cItem.content` and `cItem.meta.itemDeleted`:

- **Deleted items** -> `DeletedItemView` or `MarkedDeletedItemView`
- **Message content** (`SndMsgContent`, `RcvMsgContent`) -> `FramedItemView` or specialized views depending on `msgContent` type
- **Call items** -> `CICallItemView`
- **Integrity/decryption errors** -> `IntegrityErrorItemView`, `CIRcvDecryptionError`
- **Group invitations** -> `CIGroupInvitationView`
- **Events** (group/direct/connection events) -> `CIEventView`
- **Feature changes** -> `CIChatFeatureView`, `CIFeaturePreferenceView`
- **E2EE info** -> `CIEventView`
- **Chat banner** -> handled at list level, not in `ChatItemView`
- **Invalid JSON** -> `CIInvalidJSONView`

### Reactions

`ChatItemReactions` row renders below each message bubble, showing emoji reaction counts. Tapping own reactions removes them; tapping others' opens a member list dropdown.

### Context Menu

Long-press or right-click opens a dropdown menu with context-sensitive actions (see section 6).

### Terminal Layout Adjustments

When `LocalTerminalLayout.current` is true, `ChatItemView` and the item views it calls keep their content and start at one text edge, `CHAT_ITEM_TEXT_PADDING` (12dp) inside the item; messages are drawn without the bubble fill and tail:

- Items are start-aligned for both directions ([`alignEnd`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L126)). The item click has no indication; the row draws it (section 10). `GoToItemButton` is placed by the row (section 10), not by `ChatItemView` ([L255](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L255), [L752](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L752)). Voice items use the arrangement of received items for both directions.
- [`chatItemTailEnabled()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L1152) returns false, so `shapeStyle` draws no tail.
- `FramedItemView` has a transparent background. Forwarded and report headers and the quote have no background and start at the text edge. The quote has the bar of Markdown quotes ([`Modifier.quoteBar`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/MarkdownBlocks.kt#L132)) and its text in `onSurface` at 70%.
- Images, videos, link previews and chat link cards start at the text edge and keep the message corner clip ([`mediaModifier`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L55)). Link previews and chat links are cards in the quote colour ([`cardModifier`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L57)).
- [`CIMarkdownText`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L462): top padding 0 when the text is the first part of the item (`first`), bottom padding `TERMINAL_TEXT_BOTTOM_PADDING` (1dp); `Modifier.messageTextBottom()` marks the bottom of the text for the meta.
- Event, feature and member-created items use [`chatEventHorizontalPadding()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIEventView.kt#L21); voice and call items and reactions start at the text edge.
- Time and status: placed at the end of the row by [`ItemWithMeta`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L111-L125), level with the last text line (section 10); message and file text keep the space reserved for them at the end of the last line, so the last line ends before the time. Images and videos without a caption (`captionlessMedia`) keep the meta in `secondary` beside the media when it fits in the row, otherwise the meta is drawn over the bottom end of the media in white, as in the classic layout (`overlayMeta`). Event items are wrapped in [`ChatEventWithMeta`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIEventView.kt#L23-L34), which passes the end of the event text as `ts`: in the classic layout the time in the event style, in the terminal layout a transparent no-break space and `reserveSpaceForMeta` of the meta, so the last line of the event text ends before the time at the row end.
- Group invitations, deleted items and error items (`IntegrityErrorItemView`, `CIRcvDecryptionError`) keep their rounded coloured card.

Both layouts: `MarkdownText` draws message text in [`messageTextColor()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TextItemView.kt#L54), `onSurface` at 87% in dark themes.

---

## 5. Message Types

| CIContent Variant | MsgContent Type | View Composable | Source File |
|---|---|---|---|
| `SndMsgContent` / `RcvMsgContent` | `MCText` | `FramedItemView` -> `TextItemView` or `EmojiItemView` | `TextItemView.kt`, `EmojiItemView.kt` |
| `SndMsgContent` / `RcvMsgContent` | `MCLink` | `FramedItemView` (with link preview) | `FramedItemView.kt` |
| `SndMsgContent` / `RcvMsgContent` | `MCImage` | `CIImageView` (inside `FramedItemView`) | `CIImageView.kt` |
| `SndMsgContent` / `RcvMsgContent` | `MCVideo` | `CIVideoView` (inside `FramedItemView`) | `CIVideoView.kt` |
| `SndMsgContent` / `RcvMsgContent` | `MCVoice` | `CIVoiceView` | `CIVoiceView.kt` |
| `SndMsgContent` / `RcvMsgContent` | `MCFile` | `CIFileView` | `CIFileView.kt` |
| `SndMsgContent` / `RcvMsgContent` | `MCReport` | `FramedItemView` (with report styling) | `FramedItemView.kt` |
| `SndCall` / `RcvCall` | -- | `CICallItemView` | `CICallItemView.kt` |
| `RcvIntegrityError` | -- | `IntegrityErrorItemView` | `IntegrityErrorItemView.kt` |
| `RcvDecryptionError` | -- | `CIRcvDecryptionError` | `CIRcvDecryptionError.kt` |
| `RcvGroupInvitation` / `SndGroupInvitation` | -- | `CIGroupInvitationView` | `CIGroupInvitationView.kt` |
| `RcvDirectEventContent` | -- | `CIEventView` | `CIEventView.kt` |
| `RcvGroupEventContent` / `SndGroupEventContent` | -- | `CIEventView` | `CIEventView.kt` |
| `RcvConnEventContent` / `SndConnEventContent` | -- | `CIEventView` | `CIEventView.kt` |
| `RcvChatFeature` / `SndChatFeature` | -- | `CIChatFeatureView` | `CIChatFeatureView.kt` |
| `RcvChatPreference` / `SndChatPreference` | -- | `CIFeaturePreferenceView` | `CIFeaturePreferenceView.kt` |
| `RcvGroupFeature` / `SndGroupFeature` | -- | `CIChatFeatureView` | `CIChatFeatureView.kt` |
| `SndModerated` / `RcvModerated` / `RcvBlocked` | -- | `MarkedDeletedItemView` | `MarkedDeletedItemView.kt` |
| `SndDirectE2EEInfo` / `RcvDirectE2EEInfo` | -- | `CIEventView` | `CIEventView.kt` |
| `SndGroupE2EEInfo` / `RcvGroupE2EEInfo` | -- | `CIEventView` | `CIEventView.kt` |
| `RcvChatFeatureRejected` / `RcvGroupFeatureRejected` | -- | `CIChatFeatureView` | `CIChatFeatureView.kt` |
| `ChatBanner` | -- | `ChatBannerView` (inline in `ChatItemsList`) | `ChatView.kt` |
| `InvalidJSON` | -- | `CIInvalidJSONView` | `CIInvalidJSONView.kt` |
| `CIMemberCreatedContact` | -- | `CIMemberCreatedContactView` | `CIMemberCreatedContactView.kt` |

### Animated Images

`SimpleAndAnimatedImageView` is `expect`/`actual`. Android delegates to coil, which drives the animation
itself. Desktop decodes frames with Skia's `Codec` in `platform/AnimatedImage.desktop.kt`, where
`rememberAnimatedImage(data, still, hidden)` returns the frame to draw and falls back to the still image when
the data is not an animation, exceeds the decode bounds, or fails before showing a frame. Decoding runs off the UI thread
on two threads of the shared pool, and pauses while the window is minimized or hidden, while the image is behind the
privacy blur, and while a full screen modal covers the chat. An animation whose frames cost too much to
decode stops on the frame it reached rather than falling back to the still. The chat list preview (`smallView`) stays a
still image. Only GIF reaches this path: desktop decodes stills with ImageIO, which has no WebP reader, so a
received `.webp` renders only as the sender's preview and never opens full screen.

---

## 6. Context Menu Actions

Context menu actions are built dynamically in `ChatItemView` based on message type, direction, chat type, and feature flags.

| Action | Condition | Effect |
|---|---|---|
| **Reply** | Message content (not event/deleted), not local notes | Sets `ComposeContextItem.QuotedItem` |
| **Edit** | Sent message, editable (`meta.editable`), text/link content | Sets `ComposeContextItem.EditingItem` |
| **Delete for me** | Any deletable item | `apiDeleteChatItems` with `cidmInternal` mode |
| **Delete for everyone** | Sent + within time window, or moderator privilege | `apiDeleteChatItems` with `cidmBroadcast` mode |
| **Moderate** | Group moderator + received message | `apiDeleteMemberChatItems` |
| **Forward** | Message content, not live message | Opens share sheet via `SharedContent.Forward` |
| **Select** | Any selectable item | Enters multi-select mode (`selectedChatItems`) |
| **React** | Message content, reactions enabled | Opens emoji picker; calls `apiChatItemReaction` |
| **Report** | Received group message, reports enabled | Sets `ComposeContextItem.ReportedItem` with reason |
| **Info** | Any message | Opens `ChatItemInfoView` in end modal |
| **Copy** | Text content present | Copies text to clipboard |
| **Save** | Image/video/file with completed download | Saves media to device |
| **Open** | File with completed download | Opens file with system handler |
| **Reveal / Hide** | Part of a merged group; expanded or collapsed | Toggles `revealedItems` state |

---

## 7. ChatInfoView

**Location:** [`ChatInfoView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt)

Opened via the `info` callback when the user taps the toolbar title in a direct chat. Displayed in `ModalManager.end`.

Preloads `apiContactInfo` (connection stats, server profile) and `apiGetContactCode` (verification code) before showing the modal.

Key sections: contact profile, local alias, connection stats, shared media, disappearing messages preference, voice/call/file feature toggles, encryption verification, and contact deletion.

---

## 8. GroupChatInfoView

**Location:** [`group/GroupChatInfoView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupChatInfoView.kt)

Opened via the `info` callback for group chats. Displayed in `ModalManager.end`.

Preloads group members (`setGroupMembers`) and group link (`apiGetGroupLink`).

Key sections: group profile, group link, member list with roles, group preferences (disappearing messages, direct messages, full deletion, voice, files, SimpleX links, history), member admission, welcome message, reports view, and group deletion/leave.

---

## 9. Source Files

### `views/chat/`

| File | Description |
|---|---|
| `ChatView.kt` | Main chat view, ChatLayout, ChatItemsList, ChatInfoToolbar |
| `ChatInfoView.kt` | Contact info modal |
| `ChatItemInfoView.kt` | Individual message delivery/read info; below a file's expiry, for a user without a badge, a button opening the badges view |
| `ChatItemsLoader.kt` | Pagination and message loading logic |
| `ChatItemsMerger.kt` | MergedItems grouping of consecutive events |
| `CommandsMenuView.kt` | Bot `/command` menu popup |
| `ComposeContextContactRequestActionsView.kt` | Contact request action buttons in compose area |
| `ComposeContextGroupDirectInvitationActionsView.kt` | Group direct invitation compose actions |
| `ComposeContextPendingMemberActionsView.kt` | Pending member compose actions |
| `ComposeContextProfilePickerView.kt` | Profile picker in compose context |
| `ComposeFileView.kt` | File attachment preview in compose |
| `ComposeImageView.kt` | Image/video attachment preview in compose |
| `ComposeView.kt` | Main compose area (ComposeState, send logic) |
| `ComposeVoiceView.kt` | Voice recording preview in compose |
| `ContactPreferences.kt` | Per-contact feature preferences |
| `ContextItemView.kt` | Reply/edit/forward context indicator |
| `ScanCodeView.kt` | QR code scanner |
| `SelectableChatItemToolbars.kt` | Multi-select toolbar (delete, forward, moderate) |
| `SendMsgView.kt` | Text input field, send button, voice record button |
| `VerifyCodeView.kt` | Contact/member encryption verification |

### `views/chat/item/`

| File | Description |
|---|---|
| `ChatItemView.kt` | Message type routing, context menu, reactions |
| `CIBrokenComposableView.kt` | Fallback for rendering errors |
| `CICallItemView.kt` | Call event display (incoming/outgoing/missed) |
| `CIChatFeatureView.kt` | Chat feature change event |
| `CIEventView.kt` | Generic event display (group/direct/connection) |
| `CIFeaturePreferenceView.kt` | Feature preference change event |
| `CIFileView.kt` | File message (download/upload progress) |
| `CIGroupInvitationView.kt` | Group invitation card |
| `CIImageView.kt` | Image message (thumbnail + fullscreen) |
| `CIInvalidJSONView.kt` | Invalid JSON fallback display |
| `CIMemberCreatedContactView.kt` | Member-created contact event |
| `CIMetaView.kt` | Message metadata (time, status indicators) |
| `CIRcvDecryptionError.kt` | Decryption error display |
| `CIVideoView.kt` | Video message (thumbnail + player) |
| `CIVoiceView.kt` | Voice message (waveform + player) |
| `DeletedItemView.kt` | Deleted message placeholder |
| `EmojiItemView.kt` | Large emoji-only message |
| `FramedItemView.kt` | Message bubble frame (quoted item, text, media) |
| `ImageFullScreenView.kt` | Fullscreen image gallery |
| `MarkdownFileView.kt` | Markdown file viewer, formatted or as text |
| `IntegrityErrorItemView.kt` | Message integrity error |
| `MarkedDeletedItemView.kt` | Marked-as-deleted / moderated message |
| `TerminalItemView.kt` | Terminal layout: `LocalTerminalLayout`, sender row rule, `TerminalChatItemRow`, `ItemWithMeta` |
| `TextItemView.kt` | Plain text message with markdown; blocks of `markdownBlocks` drawn as a column of views by `MarkdownBlocks` (`MarkdownBlocks.kt`) |

### `views/chat/group/`

| File | Description |
|---|---|
| `AddGroupMembersView.kt` | Add members to group |
| `GroupChatInfoView.kt` | Group info and management |
| `GroupLinkView.kt` | Group link display and management |
| `GroupMemberInfoView.kt` | Individual member info |
| `GroupMembersToolbar.kt` | Members toolbar in group info |
| `GroupMentions.kt` | @mention autocomplete |
| `GroupPreferences.kt` | Group feature preferences |
| `GroupProfileView.kt` | Group profile editor |
| `GroupReportsView.kt` | Group reports list view |
| `MemberAdmission.kt` | Member admission settings |
| `MemberSupportChatView.kt` | Member support chat (scoped context) |
| `MemberSupportView.kt` | Support chat list for moderators |
| `WelcomeMessageView.kt` | Group welcome message editor |
| `ChannelRelaysView.kt` | Channel relay list. Owner-only Add relay entry opens `AddGroupRelayView` with `existingRelayIds = groupRelays.mapNotNull { it.userChatRelay.chatRelayId }.toSet()` — every relay currently in `groupRelays` is excluded regardless of `relayStatus`, mirroring the backend `APIAddGroupRelays` gate. Long-press menu offers Remove relay for relays that can be removed. |
| `AddGroupRelayView.kt` | Sheet to pick relays to add to a channel |

### Relay Rejection Surface

When a relay operator runs `/leave #channel`, the relay sends `x.grp.relay.reject` over the owner-relay direct contact channel. Owner-side handling: the corresponding `GroupRelay.relayStatus` transitions `RSInvited → RSRejected`; the relay's `GroupMember.memberStatus` is set to `MemLeft` so the owner UI renders the rejected relay identically to one that explicitly ran `/leave` (`MemRejected` is reserved for the knocking-admission flow). In `GroupMemberInfoView`, an additional "Status: rejected by relay operator" `InfoRow` appears when `groupRelay?.relayStatus == RelayStatus.RsRejected`. The status is final on the owner side — clearable only by the relay operator running `/group allow #<channel>`, which has no owner-facing event.

The `RelayStatusIndicator` composable in `AddChannelView.kt` renders `RsRejected` with a red dot and "rejected" text, matching the `connFailed`/`removed` rendering.

---

<a id="TerminalLayout"></a>

## 10. Terminal Layout

**Location:** [`item/TerminalItemView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt)

Rendering of a chat whose [`ChatInfo.activeChatLayout`](../state.md) is `ChatLayoutMode.Terminal`: full-width start-aligned rows, the sender name once per group of messages, avatars in groups only, messages without bubbles (item-level changes: section 4). Android and desktop; iOS draws the classic layout.

### Activation

| Step | Location |
|---|---|
| `terminal = chatInfo.activeChatLayout(user) == ChatLayoutMode.Terminal` | [`ChatView`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L269) |
| `LocalTerminalLayout provides terminal`, next to `LocalAppBarHandler` and `LocalSelectionManager` | [`ChatView`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L270-L274) |
| `drawWallpaperImage = false` of `chatViewBackgroundModifier` | [`ChatLayout` composable](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L1007) |

[`LocalTerminalLayout`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L38) is `false` outside `ChatView` and the theme screens (below); chat item info keeps the bubble look. The value applies to every `ChatView` of the chat: main view, member support chats and reports.

Colours: the app, user and chat themes apply as in the classic layout. Wallpaper: `chatViewBackgroundModifier` fills the chat with the background colour of the wallpaper (preset or custom) and omits the image.

### Functions

| Function | Purpose |
|---|---|
| [`terminalItemShowsSender(item, older)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L42-L43) | Whether a row shows the sender row. `older`: the next older item in the list |
| [`sameTerminalSender(item, older)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L45-L53) | Private; same sender for grouping |
| [`CIContent.hasTerminalSender`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L58-L65) | Private. True for sent messages, sent deleted and moderated items, sent calls and group invitations, and received content with `showMemberName` (messages, deleted, moderated and blocked items, calls, errors, group invitations, invalid JSON); false for events, feature and preference changes, E2EE info and the chat banner |
| [`TerminalRowInteractionSource`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L67-L81) | Interaction source of the row and its item: press interactions are emitted at the position where the pointer went down in the row, so the ripple starts there |
| [`Modifier.terminalRowIndication(source)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L83-L94) | Records the pointer-down position; hover and press indication of the whole row |
| [`Modifier.terminalStartOffset(offset)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L96-L101) | Shifts the row by the selection offset and narrows it by the same amount |
| [`Modifier.messageTextBottom()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L105-L109) | Sets the private alignment line `MessageTextBottom` to the bottom of the message text |
| [`ItemWithMeta(meta, overlayMeta, content)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L111-L125) | Classic: `content` with the meta passed as `inlineMeta`. Terminal: `content` without meta, and the meta in a box with 6dp bottom and 12dp end padding at the end of the available width, the message column of the row (private [`TerminalMetaLayout`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L127-L146)); the box ends `CHAT_ITEM_TEXT_VERTICAL_PADDING` below `MessageTextBottom` when the content has a message text, otherwise at the content bottom. When `overlayMeta` is given and the content and the meta together are wider than the row, the `overlayMeta` box is placed at the bottom end of the content instead |
| [`terminalGutter(chatInfo)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L148-L150) | Gutter width: the member image size (scaled by `fontSizeSqrtMultiplier`) in groups, `GO_TO_ITEM_BUTTON_SIZE` (22dp) in other chats |
| [`TerminalChatItemRow(sender, gutter, topGap, modifier, senderTopGap, goToButton, content)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L152-L201) | Row layout below |
| [`TerminalSender`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L203) | Name, badge, role label, `own`, avatar |
| [`terminalSender(chatInfo, cItem, range, reversedChatItems, showMemberInfo, showChatInfo)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L205-L252) | Sender of a row (table below) |

### Row Layout

```
row padding        top: senderTopGap (8dp) with a sender row, otherwise the item gap (1dp, 4dp after a large gap); start 8dp; end 12dp
gutter             terminalGutter(chatInfo)
  avatar           top of the gutter, sender rows of groups only
  go-to button     centred in the gutter and on the message; below the avatar when the message is shorter
message column     starts at gutter + 4dp
  sender row       name and badge (13.5sp; primary for own messages, secondary otherwise), role label; start padding CHAT_ITEM_TEXT_PADDING
  item             ChatItemView; 4dp + topGap below the sender row
```

`ChatItemsList` passes [`showAvatar`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L2458) = `terminalItemShowsSender(item, merged.oldest().nextItem)`, and the row gets `terminalSender(...)` when it is true, `null` otherwise.

### Sender Rule

A row shows the sender row when its content has a sender and one of these holds:

- it is the oldest loaded item;
- the older item has no sender (an event);
- `sameTerminalSender` is false.

As in the classic layout, the time between the items and a date change do not repeat the sender row.

`sameTerminalSender`: when either item is posted as the channel (`ChannelRcv`, or `GroupSnd` with `meta.showGroupAsSender`), both must be; two `GroupRcv` items need the same `groupMemberId`; other items need the same direction class.

### Sender Resolution

| Direction | Name and badge | Role label | Avatar | Avatar click | Own colour |
|---|---|---|---|---|---|
| `ChannelRcv`, `GroupSnd` with `showGroupAsSender` | Channel name | "channel" (`channel_role_label`) | `ProfileImage` of the group, as in the classic layout | `showChatInfo` | For `GroupSnd` |
| `DirectRcv` | Contact display name and badge | None | None | None | No |
| `GroupRcv` | `memberNames` of the merged members; badge for one member | Member role when above `Member`, for one member | `MemberImage` | `showMemberInfo` | No |
| `GroupSnd` | `groupInfo.membership` display name and badge | Membership role when above `Member` | `MemberImage(membership)` | None | Yes |
| Other, incognito chat | "Incognito" | None | None | None | Yes |
| Other (`DirectSnd`, `LocalSnd`, `LocalRcv`) | Current user display name and badge | None | None | None | Yes |

The sender row has no time; the time of every item is placed by `ItemWithMeta`.

### Row Background

| State | Background |
|---|---|
| Selected (selection mode) | `primary` at 10% |
| Highlighted (`highlightedItems` contains the item, e.g. after jumping to a quoted message) | `primary` at 14% |
| Otherwise | Transparent |

Hover and press are drawn by `terminalRowIndication` over the whole row.

### Go-to Button

[`GoToItemButton`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L1322) (`GO_TO_ITEM_BUTTON_SIZE`, 22dp) is passed to the `goToButton` slot of the row, with the row hover as `parentActivated`: the search icon in search results, the arrow for forwarded items.

### Selection and Swipe

The selection checkbox (8dp start padding) and the full-row selection click overlay are shared with the bubble layout. The row is moved by `Modifier.terminalStartOffset` for sent and received items alike. Swipe to reply uses the `swipeableModifier` of the bubble layout on the whole row (Android; off in selection mode).

### Theme Screens

`LocalTerminalLayout` is provided to the theme screens of a terminal chat or profile:

| Screen | Value |
|---|---|
| Chat theme ([`ChatWallpaperEditorModal`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt#L1300)) | `activeChatLayout` of the chat |
| Appearance › Themes, preset selector ([`ThemesSection`](../../common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/Appearance.kt#L579)) | `profileChatLayout` of the current user, or of none for the app theme destination |
| Customize theme, profile theme, colour editors | the same, through `profileLayoutIsTerminal` and the `terminal` parameter of `editColor` |

With the value true:

- the preview draws two terminal rows ("alice", then "bob" in the own colour) without gutter; [`PreviewInEitherLayout`](../../common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/Appearance.kt#L335) measures both layouts and shows the active one centred in the larger height, so a layout switch keeps the preview height;
- the preview and the preset tiles show the wallpaper colour without the image;
- the custom image tile, the image scale, the scale slider and the wallpaper accent row are hidden.

Layout controls:

| Control | Values | Stored in |
|---|---|---|
| Chat theme screen, first section ([`ChatLayoutSection`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt#L1313)) | the chat default, "(chat)" when the chat layout is the default, otherwise "(default)"; the other layout | chat theme record, by [`setChatLayout`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt#L1382) and `apiSetChatUIThemes` |
| Appearance › Themes, app theme destination ([`ChatLayoutRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/Appearance.kt#L948)) | Classic, Terminal; the selection is `profileChatLayout(null)` | `appPrefs.chatLayout` |
| Appearance › Themes, profile destination | `profileChatLayout(null)` "(default)"; the other layout | profile theme record, by `saveThemeToDatabase` and `apiSetUserUIThemes` |
