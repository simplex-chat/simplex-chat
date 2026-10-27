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
10. [Telegram Reference Layout](#10-telegram-reference-layout)
11. [Outgoing Message Animation](#outgoing-message-animation)
12. [Image Viewer Transition](#image-viewer-transition)

---

## Executive Summary

The Chat View is the primary message display and interaction surface in SimpleX Chat. It is built around the `ChatView` composable, which orchestrates a `ChatLayout` containing a reverse-scrolling `LazyColumn` of `ChatItemView` items and a `ComposeView` for message input. The view supports direct chats, group chats, local notes, and contact connections, with per-chat theming, search/filter, multi-select, and side-panel info modals. Message rendering is delegated to type-specific composables in the `views/chat/item/` package.

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
|   |   |   +-- ChatItemView       (message type routing)
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

**Location:** [`ChatView()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L99)

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

**Location:** [`ChatItemsList()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L1741)

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

- **Group received messages** with `showAvatar = true`: bottom-aligned `MemberImage` (clickable to `showMemberInfo`) at the end of a sender run. Ordinary message sender names, badges and moderator roles appear inside the first bubble of the run; non-message events retain their external header.
- **Group received without avatar:** Indented to align with avatar-bearing messages.
- **Sent messages (group or direct):** Right-aligned with larger start padding.
- **Direct messages:** 6dp screen gutter and 49dp opposite gutter before tail alignment adjustments.

---

<a id="ChatItemView"></a>

## 4. ChatItemView

**Location:** [`ChatItemView()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L71)

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

### Message Gestures and Context Menu

[`ChatItemActions` and `LocalChatItemActions`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L65)
route bubble and nested-content gestures through the same callbacks. On Android,
ordinary taps open the existing dropdown (section 6). Long-press closes it and
calls `selectChatItem()` for items with `canBeDeletedForSelf`; this enters the
existing `selectedChatItems` bulk-message mode, where taps toggle selection. It
does not start character selection. Desktop retains its status-information tap,
long-press/right-click menu and existing drag text selection.

[`MarkdownText()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TextItemView.kt#L126)
gives links, SimpleX names, mentions, secrets, commands and modal
annotations their existing tap actions; unhandled taps call the message action.
Formatted-text long-press uses the shared action on Android. Stable pointer-input
keys and `rememberUpdatedState` in
[`ClickableText()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TextItemView.kt#L451)
preserve in-flight gestures across recomposition.
`FramedItemView` provides quote-specific actions: taps navigate to the quoted item,
while long-press still selects the enclosing message. Media, link cards, voice,
file, emoji, deleted/error/event views and privacy-blur overlays use the shared
long-press callback while preserving their own tap controls. Existing full-width
multi-select overlays and swipe-to-reply behavior remain in the list layer.

The [Details view](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatItemInfoView.kt#L281)
includes a status-information row when
`itemStatus.statusInto` or missing-signature information exists. Its alert retains
the explanation previously available by tapping the bubble.

<a id="message-menu-placement"></a>

### Message Menu Placement

[`ChatItemDropdownMenu()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemMenu.kt#L105)
uses a focusable `Popup` on Android and retains `DefaultDropdownMenu` on Desktop.
Every message-action menu uses this wrapper; reaction-member lists keep their
separate menu. [`ChatItemMenuAnchor`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemMenu.kt#L25)
stores the bubble's `LayoutCoordinates`. Its `update()` publishes observable bounds
only while `track(true)` is active during the popup's lifetime. `capture()` checks
attachment before converting to window pixels; `track(false)` clears the bounds.
This avoids per-row state changes during ordinary scrolling while allowing source
movement to invalidate the open menu's measurement. The popup measures its
scrollable surface against these bounds and the window's visible constraints.

[`chatItemMenuPlacement()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemMenu.kt#L78)
and [`ChatItemMenuPlacement.position()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemMenu.kt#L54)
apply the same policy during measurement and native popup positioning:

- Reserve an 8dp window gutter, the top status-bar inset, and a 4dp bubble gap.
  Use the popup's visible window height, which already excludes the keyboard.
- Cap the preferred 250dp width to the usable window width before measuring.
- Cap height to the larger space above/below the bubble. Prefer below if the
  measured menu fits; otherwise use above. The menu content scrolls independently.
- When neither vertical region has 96dp, try a full-width menu on either side.
  If neither side fits, use the viewport height and choose the clamped above/below
  position with the least vertical overlap. This fallback keeps actions accessible
  for a viewport-filling source.
- Align received menus to the bubble's left edge and sent menus to its right edge,
  then clamp to the window gutter. Alignment follows message direction.

The popup preserves inherited screen protection and Back/outside dismissal. Its
surface fades and scales from 0.96 over 120ms, including dismissal, using Compose's
system motion scaling. Bubble constraints and message content are unchanged.
`ChatItemMenuPlacementTest` covers edge alignment, above/below placement, long-menu
scroll limits, keyboard-sized and narrow windows, side placement, full-viewport
fallback, and non-overlap across portrait/landscape geometry.

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
| **Details** | Any message | Opens `ChatItemInfoView` in end modal, including available status/signature explanations |
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
| `ChatItemInfoView.kt` | Individual message delivery/read info and status/signature explanation alert |
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
| `ChatItemView.kt` | Message type routing, shared item actions, Android tap menu/long-press selection, reactions |
| `ChatSendAnimation.kt` | Scoped outgoing-send tokens, measured bubble motion and lifecycle cleanup |
| `ChatItemMenu.kt` | Android message popup, bounded scrolling and bubble-relative placement policy |
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
| `ImageGalleryAnimation.kt` | Thumbnail capture, shared image geometry, animated open/close state and source restoration |
| `IntegrityErrorItemView.kt` | Message integrity error |
| `MarkedDeletedItemView.kt` | Marked-as-deleted / moderated message |
| `TextItemView.kt` | Plain text message with markdown |

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

## 10. Telegram Reference Layout

The classic conversation adapts Telegram Android `3de37c8cf` geometry to Compose.

Source: [`TelegramChatTypography()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatBubbleStyle.kt#L31),
[`chatMessageTextStyle()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatBubbleStyle.kt#L46),
[`chatMessagesJoined()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatBubbleStyle.kt#L54).

| Rule | Compose implementation |
|---|---|
| Body typography | System default sans-serif, user's existing body size, natural line metrics and zero letter spacing. Scoped to conversation rows; the global application theme retains its fonts. |
| Text insets | 11dp at the sides; 5dp above and below the text layout. These are adapted layout-box insets, not measured glyph bounds. |
| Quotes | Font size `(2 * bodySize + 10) / 3`; bold sender; compact tinted card with 2dp accent, 4dp corners and 35dp media thumbnails. |
| Bubble contour | Circular corners, 17dp at the default roundness setting, 6dp near joined messages, limited by available width/height; 6dp tail outside the body. Existing tail and roundness preferences remain active. |
| Grouping | Ordinary messages with matching direction/sender within 300 seconds, inclusive. Different calendar dates, deleted items, system events and report/non-report changes break runs. |
| List spacing | 2dp inside runs, 6dp between runs; 8dp bottom space after the newest separate message. Group avatar-column constraints remain supported. |
| Sender | Name and badge inside the first received group bubble; avatar bottom-aligned at the end of the run. |
| Metadata | 12sp natural text; visible outgoing status after the time. Every message shows its timestamp. All SimpleX encryption, signature, proxy, error and disappearing-message indicators retain their visibility rules. |
| Metadata wrapping | Measure the real metadata row before composing message text. An inline placeholder reserves its width plus 4dp; a zero-width break allows metadata to move alone. Mixed RTL direction gets a separate metadata line. |

[`MeasuredChatMetadata()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIMetaView.kt#L29)
must wrap the framed bubble **outside** its intrinsic-width content, because a
`SubcomposeLayout` does not provide intrinsic measurements. The hidden measurement
copy is not placed. The visible row remains at the lower end of the bubble, 4.5dp
above the bottom. Specialized file/error renderers retain their existing space
reservation where they do not use `MarkdownText`.

[`CIMarkdownText()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L425)
uses the natural message style. Selectable and clickable text both receive the
inline content map; metadata/live-typing suffixes are excluded from selection.
[`SelectableText()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TextItemView.kt#L525)
retains desktop selection layout and highlighting.
`ChatBubbleGroupingTest` runs on desktop/JVM because model sample fixtures load
localized strings and require initialized Android resources on an Android JVM test.

<a id="outgoing-message-animation"></a>

## 11. Outgoing Message Animation

[`ChatLayout()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L902)
provides one `ChatSendAnimations` controller per mounted remote host, chat, group
scope and user. `SendMsgView` measures its padded input bounds in window coordinates.
The send gesture captures those bounds before API work; the successful response
registers each returned item ID on Main immediately before `addChatItem`.
See the [composition hooks](compose.md#outgoing-send-animation).

[`ChatSendAnimations`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatSendAnimation.kt#L28)
owns independent transitions keyed by item ID:

- `capture()` returns a `PendingChatSend` with immutable source bounds and a
  monotonic creation time; absent/empty bounds produce no token.
- `PendingChatSend.register()` delegates to the captured controller's `register()`.
  Disposed controllers and captures older than two seconds are ignored. Registered
  entries expire after one second if the list never composes the corresponding row.
- `finish()` removes only the matching transition, so an older completion cannot
  remove a replacement. `dispose()` invalidates the session and clears entries.
- `ChatSendTransition` contains geometry and a running flag; no message text is
  retained. The pending timeout never interrupts a running, system-scaled animation.

[`chatAnimationBounds()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatSendAnimation.kt#L73)
uses window position and layout size, without clipping the measured rectangle.
[`ChatSendAnimatedItem()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatSendAnimation.kt#L80)
keeps an untransformed outer `Box` separate from the animated content's
`graphicsLayer`. The content call site stays stable on completion, preserving
selection and remembered bubble state. Layout changes update the destination;
translation is recomputed against the captured source during scrolling/resizing.

The reference is Telegram Android `3de37c8cf`, `ChatListItemAnimator` and
`TextMessageEnterTransition`: duration 250 ms; vertical cubic
`(0.19919473, 0.010644531, 0.27920938, 0.9102539)`; horizontal easing is
`EASE_OUT(EASE_OUT_QUINT(t))`; opacity reaches one after 100 ms. The adaptation
adds a subtle scale from 0.96 to 1 and moves the existing whole bubble. It does not
implement Telegram's separate text reflow/background morph or neighboring-row
insertion motion. The composer is painted above the list and obscures the initial
part of the path inside the input area; this version does not use an overlay.

Zero `MotionDurationScale` skips the transition before waiting for a frame and
draws normally. Missing/offscreen destinations and bubbles taller than the viewport
also draw normally. Completion, cancellation and row disposal remove the entry;
chat/profile/scope navigation disposes the old controller. Existing content remains
the single semantic and interaction tree.

`ChatSendAnimationTest` covers stale responses after navigation, independent rapid
sends with captured bounds, and missing input/unmarked history.

<a id="image-viewer-transition"></a>

## 12. Image Viewer Transition

The reference is Telegram Android `3de37c8cf` `PhotoViewer` normal open/close:
200 ms with the platform's default accelerate-decelerate curve. The Compose
adaptation uses `(1 - cos(PI * t)) / 2` and the existing full-screen modal rather
than a separate window.

### Capture and Platform Entry Points

[`CIImageView()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIImageView.kt#L37)
remembers an `ImageGalleryThumbnail` for the image/file, small-view mode and radius.
The actual bounded thumbnail box records `LayoutCoordinates` and hides only its
drawing while the transition owns the source. Its disposal invalidates coordinates
and restores drawing. Loaded media supplies a transition using its decoded bitmap;
the current-source callback checks the current privacy blur state before capture.
Unrevealed media retains the existing reveal/download/error path and cannot supply
hidden pixels to an animated thumbnail.

The [`SimpleAndAnimatedImageView` expect](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIImageView.kt#L292)
and its [Android](../../common/src/androidMain/kotlin/chat/simplex/common/views/chat/item/CIImageView.android.kt#L21)
and [Desktop](../../common/src/desktopMain/kotlin/chat/simplex/common/views/chat/item/CIImageView.desktop.kt#L12)
actuals take `createTransition: () -> ImageGalleryTransition`. They capture synchronously
on the image click, then call `showCustomModal(animated = false)` and pass the
transition to the existing viewer. This avoids stacking the generic modal animation
over the image animation. `ImageFullScreenView(imageProvider: () -> ImageGalleryProvider,
close: () -> Unit, transition: ImageGalleryTransition? = null)` keeps existing
callers, including video, compatible; a null transition renders normally.

### Geometry and Lifecycle

[`ImageGalleryAnimation.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ImageGalleryAnimation.kt#L22)
contains the following types and functions:

| Symbol | Responsibility |
|---|---|
| `ImageGallerySourceBounds` | Full window bounds, visible clipped bounds, radius in pixels and crop mode. |
| `ImageGalleryThumbnail` | Owns coordinates, source visibility and active lifetime; `capture()` rejects detached/empty/nonfinite bounds and `dispose()` clears them. |
| `ImageGalleryTransition` | Decoded bitmap, optional captured source, current-source lookup and source-visibility callback. |
| `ImageGalleryGeometry` | Interpolated image, image clip, visible clip and corner radius. |
| `Rect.validImageRect()` | Rejects empty or nonfinite rectangles. |
| [`imageGalleryGeometry()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ImageGalleryAnimation.kt#L56) | Validates geometry, clamps progress, computes centered crop/FillWidth and fitted endpoints, then interpolates relative to the full-screen viewport's window origin. Its local `centered()` helper preserves aspect ratio. |
| `ImageGalleryPhase` | `Opening`, `Open`, `ClosingToSource`, `ClosingFade`. |
| `imageGalleryEasing` | Sinusoidal accelerate-decelerate timing. |
| [`ImageGalleryAnimatedContainer()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ImageGalleryAnimation.kt#L82) | Animates scrim and bitmap, gates gestures and coordinates close/disposal. |

Full-size thumbnails use their measured width and capped height; small previews
use crop mode. Image placement, rounded image clip and list-visible clip interpolate
independently, so tall/clipped thumbnails expand without stretching. The canvas
clamps corner radius to the clip dimensions. The real pager remains composed at a
stable call site behind the transition; it becomes visible when opening finishes.
Only draw/graphics-layer code reads animation progress, avoiding media reloads on
every frame.

[`ImageFullScreenView()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ImageFullScreenView.kt#L35)
routes Back and ordinary close taps through the container. It permits return to
source only on the original page, with no pager scroll or image zoom/translation.
The original index follows the initial pager rebase to zero. Closing during opening
reverses toward the captured source; later closes recapture the current visible
source, falling back to a fade when it is absent, private, invalid or outside the
viewport. Closing phases reject duplicate requests, and an interrupted opening
cannot overwrite the closing phase. Completion calls `provider.onDismiss` before
the modal close callback. Source visibility is restored on disposal and before a
fade fallback.

Paging is disabled and transform gestures ignored until phase `Open`; the transform
handler reads updated permission without restarting `pointerInput(Unit)`. A top
click layer handles early close requests. Zero `MotionDurationScale` snaps to the
endpoint; nonzero scales use Compose's animation scaling. Existing system-bar,
`fullscreenGalleryVisible`, privacy/screenshot window policy and video cleanup
remain attached to the existing viewer lifetime.

`ImageGalleryGeometryTest` covers window offsets, fitted aspect ratios, tall-image
crop/visible clipping, square-preview cropping, reverse endpoints and invalid
geometry.

**[REC] Source-corner fidelity:** the transition approximates the source with a
uniform 17dp radius (4dp for small previews), shrinking to zero. It does not capture
the exact joined-bubble corner/tail or user-roundness shape; carry the source's
actual contour if exact visual continuity is required.

**[GAP] Synchronous gallery loading:** `providerForGallery().getMedia()` in
[`ChatView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L3608)
already uses `runBlocking { getLoadedImage(...) }`, and the viewer calls it during
composition. This can block the UI while reading/decoding a gallery page. Draw-only
animation avoids multiplying that work per frame but does not remove the existing
blocking load. Use asynchronous loading with cached page results; see
[GAP-08](../../product/gaps.md#gap-08-gallery-images-load-synchronously-during-composition).
