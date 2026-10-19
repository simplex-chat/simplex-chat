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
10. [Flat Layout (Desktop)](#10-flat-layout-desktop)

---

## Executive Summary

The Chat View is the primary message display and interaction surface in SimpleX Chat. It is built around the `ChatView` composable (line ~99 in `ChatView.kt`), which orchestrates a `ChatLayout` containing a reverse-scrolling `LazyColumn` of `ChatItemView` items and a `ComposeView` for message input. The view supports direct chats, group chats, local notes, and contact connections, with per-chat theming, search/filter, multi-select, and side-panel info modals. Message rendering is delegated to type-specific composables in the `views/chat/item/` package.

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
|   |   |   +-- FlatChatItemRow    (desktop flat layout, replaces the three lines above)
|   |   |       |-- sender header, hover bar
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

On chat ID change (via `snapshotFlow` on `chatModel.chatId.value`, line ~180):

1. Marks unread chat as read (`markUnreadChatAsRead`)
2. Clears group members state
3. Resets search, content filter, and selection
4. Fetches available content types (`updateAvailableContent`)
5. For direct chats, loads contact info and connection stats
6. For groups with pending membership, opens member support chat

### Chat Type Routing

The outer `when (chatInfo)` (line ~272) branches:

| ChatInfo Type | Behavior |
|---|---|
| `ChatInfo.Direct`, `ChatInfo.Group`, `ChatInfo.Local` | Full `ChatLayout` with compose, search, reactions, per-chat theme |
| `ChatInfo.ContactConnection` | `ModalView` wrapping `ContactConnectionInfoView` |
| `ChatInfo.InvalidJSON` | `ModalView` with raw JSON display and share button |

---

## 3. Message List

**Location:** [`ChatView.kt#L1751`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L1751) (`ChatItemsList` composable)

The message list is a `LazyColumnWithScrollBar` with `reverseLayout = true`, meaning index 0 is the newest message at the bottom of the screen.

### Key Behaviors

- **Merged Items:** Messages are grouped via `MergedItems.create()` (line ~1815), which collapses consecutive similar system events into expandable groups. Revealed state is tracked in `revealedItems`.
- **Pagination:** `PreloadItems` triggers `loadMessages` with `ChatPagination.Before` (older) or `ChatPagination.Last` (newer) when the user scrolls near list boundaries.
- **Scroll To Item:** `scrollToItem` lambda supports animated scrolling to a specific item ID, used by search result taps and quoted message navigation.
- **Unread Marking:** `MarkItemsReadAfterDelay` composable marks newly visible received items as read after a brief delay.
- **Date Separators:** `DateSeparator` composable renders between messages when the date changes (via `ItemSeparation.date`).
- **Swipe to Reply:** `SwipeToDismiss` modifier on each item (EndToStart direction, 30dp threshold) sets `ComposeContextItem.QuotedItem` via `replyToItem()`.
- **Selection Mode:** When `selectedChatItems` is non-null, a checkbox overlay appears on each item; a full-width clickable overlay toggles selection.

### Item Layout (ChatViewListItem)

- **Group received messages** with `showAvatar = true`: Column layout with `MemberNameAndRole` header, `MemberImage` (clickable to `showMemberInfo`), and message bubble.
- **Group received without avatar:** Indented to align with avatar-bearing messages.
- **Sent messages (group or direct):** Right-aligned with larger start padding.
- **Direct messages:** Symmetric padding (76dp opposite side).
- **Flat layout (desktop):** When `LocalFlatLayout.current` is true, all chat types render through `FlatChatItemRow`, which wraps `ChatItemView`, instead of the branches above (see section 10). The selection offset applies to sent items too.

---

<a id="ChatItemView"></a>

## 4. ChatItemView

**Location:** [`item/ChatItemView.kt#L70`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L70)

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

Long-press or right-click opens a dropdown menu with context-sensitive actions (see section 6). In the flat layout, the hover bar's More button opens the same menu.

### Flat Layout Adjustments

`ChatItemView` takes `showMenu: MutableState<Boolean>` as a parameter, so the flat row's hover bar can open the same menu.

When `LocalFlatLayout.current` is true, `ChatItemView` and the item views it calls keep their content and drop the bubble look for text, media and deleted items:

- Items align to the start regardless of direction, `GoToItemButton` is always on the right, and the bubble click has no indication (the row draws the hover background).
- Voice items, with or without a caption, are not mirrored for sent messages.
- [`chatItemTailEnabled()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L1225) returns false, so `shapeStyle` never draws a tail.
- `FramedItemView` has a transparent background and 2dp vertical text padding (`FLAT_TEXT_VERTICAL_PADDING`). The forwarded and report header rows have no fill and 2dp vertical padding; the report header also pads at the bottom. Its outer container is a `Column`, so in the flat layout the reply line sits above the body and the meta follows only the body.
- Replies: the quote block is replaced by [`ciReplyLine`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L256-L293), one line above the body: a reply icon, [`ciReplyAvatar`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L241-L253) (16dp scaled; member, contact, channel or own avatar, with `FlatUserImage` handling incognito), the quoted sender in SemiBold capped at 160dp (scaled) with an ellipsis, and a one-line 13sp snippet in `secondary`. Media without text shows a label (image, video, file); voice shows "Voice message" with the duration; chat links use the link name as prefix and strip the link. A click calls [`openQuotedItem`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L227-L238) (shared with the bubble quote block) to scroll to the quoted item; hover tints the text `onBackground` without indication; long-press or right-click opens the item menu.
- `ChatItemsList` passes `showTimestamp = false`. When `flatMetaVisible()` is false (a received item whose meta would show only the time), `FramedItemView` and `CIMarkdownText` omit the meta and the space reserved for it; otherwise the meta shows its icons, and the delivery status only for sent items.
- `DeletedItemView` and `MarkedDeletedItemView` are transparent, with 2dp vertical padding and 14sp text (`FLAT_DELETED_FONT_SIZE`). `CIMetaView` returns early for deleted content when the timestamp is hidden.
- `CIEventView` uses the flat text padding (12dp start, 2dp vertical).
- `ChatItemReactions` renders pills (`flatReactionPill`, tinted and bordered in primary when the user reacted), always shows the count, uses 14sp emoji, and aligns with the message text.
- `DateSeparator` renders `FlatDateSeparator`: the date in a pill on a full-width divider.
- `ComposeView` hides its top `Divider`, and the main input row and the "send message to connect" row use `Modifier.flatComposeField()`: a rounded field (8dp corner, `onBackground` at 6%) with 12dp horizontal and 8dp vertical margins.

Group invitations (`CIGroupInvitationView`) and error items (`IntegrityErrorItemView`, `CIRcvDecryptionError`) keep their rounded coloured card.

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

The React, Reply and Forward conditions are computed by [`itemQuickActions()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L802-L811), which returns [`ItemQuickActions`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L800). The menu evaluates it inside its `DefaultDropdownMenu` content, so only while the menu is open. All three are false for items without message content, temporary items (`id < 0`) and reports. Otherwise:

| Field | Condition |
|---|---|
| `react` | Reactions feature enabled in the chat and `cItem.allowAddReaction` |
| `reply` | Not deleted, not live, not a local note, `cInfo.sendMsgEnabled` |
| `forward` | Not deleted, file (if any) allows forwarding, not the live dummy item, not live |

Shared helpers used by both the menu and the flat-layout hover bar:

| Function | Purpose |
|---|---|
| [`availableReactions(cItem)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L813-L814) | `MsgReaction.supported` minus the reactions the user already set |
| [`replyToItem(composeState, cItem, useLinkPreviews)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L816-L822) | Sets `ComposeContextItem.QuotedItem`; replaces the compose state when editing. Also used by swipe to reply |
| [`MsgReactionsMenu(cInfo, cItem, setReaction, showMenu)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L825-L842) | Scrollable row of available reactions; a tap sets the reaction and closes `showMenu` |

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
| `FlatChatItemView.kt` | Desktop flat layout: sender header rule, `FlatChatItemRow`, hover bar, flat styling helpers |
| `FramedItemView.kt` | Message bubble frame (quoted item, text, media) |
| `ImageFullScreenView.kt` | Fullscreen image gallery |
| `MarkdownFileView.kt` | Markdown file viewer, formatted or as text |
| `IntegrityErrorItemView.kt` | Message integrity error |
| `MarkedDeletedItemView.kt` | Marked-as-deleted / moderated message |
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

<a id="FlatLayout"></a>

## 10. Flat Layout (Desktop)

**Location:** [`item/FlatChatItemView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt)

Experimental Slack/Discord-like rendering of the message list: full-width rows, avatar and sender name once per group of messages, and no bubbles for text, media and deleted items (see section 4 for the item-level changes). Enabled by `LocalFlatLayout` (provided by `DesktopScreen`, see [navigation.md](navigation.md#DesktopScreen)), so it applies only on desktop with developer tools and the `desktopFlatLayout` preference on. It covers every chat shown in the center and end columns, including reports and member support chats. The Appearance message-shape preview provides `LocalFlatLayout = false`, so bubble settings stay previewable. The chat pane keeps the theme background and wallpaper; only the rail and the chat list are shaded (see [chat-list.md](chat-list.md#DesktopRail)).

### Functions

| Function | Purpose |
|---|---|
| [`flatItemHasHeader(item, older)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L69-L76) | Whether a row shows the sender header. `older` is the next older item in the list |
| [`sameFlatSender(item, older)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L78-L89) | Private; same sender for header grouping |
| [`flatHoverBarVisible(...)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L93-L94) | Internal; hover bar visibility rule |
| [`CIContent.hasFlatSender`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L96-L130) | Private. True for content that has a sender (messages, deleted/moderated/blocked, calls, errors, group invitations, invalid JSON); false for events, feature and preference changes, E2EE info and the chat banner |
| [`FlatChatItemRow(...)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L134-L185) | Full-width hoverable row, inset 4dp with 6dp rounded corners: a 56dp gutter (scaled by `fontSizeSqrtMultiplier`) with the avatar (inside `FlatAvatarContrast`) on header rows or, on hover, the time (10sp, end-aligned) on other rows; then a column with the header and the item content; then the `hoverBar` slot |
| [`FlatHoverBarSlot(...)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L188-L220) | Hover bar slot content: holds the reaction picker state, evaluates `itemQuickActions` and `flatHoverBarVisible`, renders the private `FlatHoverBar` |
| [`flatContrastSecondaryVariant(colors)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L222) | `secondaryVariant` mixed 15% toward `onBackground`, so placeholder avatars keep contrast on shaded surfaces and light wallpapers |
| [`FlatAvatarContrast { }`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L225-L228) | Applies `flatContrastSecondaryVariant` to its content (gutter and reply avatars) |
| [`FlatUserImage(chatInfo, size)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L308-L314) | Own avatar, or `IncognitoImage` in an incognito chat |
| [`FlatHoverBar(...)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L317-L360) | Private; bordered action bar |
| [`flatMetaVisible(...)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L370-L374) | False when the meta would show only the time (status counted only for sent items) |
| [`Modifier.flatStartOffset(offset)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L376-L380) | Shifts the row by the selection offset and narrows it by the same amount |
| [`Modifier.flatReactionPill(userReacted)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L383-L387) | Reaction pill background and border |
| [`FlatDateSeparator(text)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L390-L405) | Date pill on a divider |
| [`flatSelectionTopPadding(showHeader)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L412-L414) | Top padding that aligns the selection checkbox with the avatar on header rows and with the first text line otherwise |
| [`Modifier.flatComposeField()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FlatChatItemView.kt#L420-L424) | Rounded compose field in the flat layout; no-op otherwise |

### Header Rule

`ChatItemsList` computes `flatHeader = LocalFlatLayout.current && flatItemHasHeader(item, merged.oldest().nextItem)` and passes it through `ChatViewListItem`. A row has a header when its content has a sender and any of these holds:

- it is the oldest loaded item;
- the older item has no sender (an event);
- `sameFlatSender` is false;
- the items' `createdAt` are 60 seconds or more apart;
- the items' `itemTs` fall on different local dates.

`sameFlatSender` is true when both items have the same direction and, for `GroupRcv`, the same `groupMemberId`; for `GroupSnd`, the same `meta.showGroupAsSender` (an owner posting as the channel and as themselves are different senders). Any two `ChannelRcv` items match.

### Sender Resolution

| Direction | Name and badge | Role label | Avatar | Avatar click |
|---|---|---|---|---|
| `GroupRcv` | Member display name | Member role when above `Member` | `MemberImage` | `showMemberInfo` |
| `ChannelRcv` | Channel name | "channel" (`channel_role_label`) | `ChatInfoImage` | `showChatInfo` |
| `GroupSnd` with `showGroupAsSender` | Channel name | "channel" | `ChatInfoImage` | `showChatInfo` |
| `GroupSnd` | `groupInfo.membership` (incognito profile when incognito) | Membership role when above `Member` | `MemberImage(membership)` | None |
| `DirectRcv` | Contact display name | None | `ChatInfoImage` | `showChatInfo` |
| `DirectSnd`, `LocalSnd`, `LocalRcv` | Current user, or "Incognito" in an incognito chat | None | User profile image or `IncognitoImage` | None |

The header shows the name in SemiBold, the role, and `cItem.timestampText`.

### Row Background

The background is drawn inside a 4dp horizontal inset with 6dp rounded corners.

| State | Background |
|---|---|
| Highlighted (`highlightedItems` contains the item, e.g. after jumping to a quoted message) | `primary` at 12% |
| Hovered | `onBackground` at 4% (`FLAT_SUBTLE_FILL_ALPHA`) |
| Otherwise | Transparent |

### Hover Bar

`ChatItemsList` creates one `showMenu` state per item and passes it to both `FlatHoverBarSlot` (through `FlatChatItemRow`'s `hoverBar` slot) and `ChatItemViewShortHand`/`ChatItemView`, so the bar's More button opens the item's context menu. Flat rows do not write `hoveredItemId`.

The bar is drawn inside the row at its top end, and rows with message content have a minimum height (`FLAT_ROW_MIN_HEIGHT`) that fits it: the bar hides as soon as the pointer leaves the row. `flatHoverBarVisible` shows it when selection mode is off, the item has message content and is not the live dummy item, and the row is hovered or the reaction picker is open while reactions are available. A `LaunchedEffect` closes the picker when the bar is hidden or reactions become unavailable.

| Button | Shown when | Action |
|---|---|---|
| 3 quick reactions | `itemQuickActions().react` | First 3 of `availableReactions(cItem)`; sets the reaction |
| Add reaction | `itemQuickActions().react` | Opens `MsgReactionsMenu` in a `DefaultDropdownMenu` |
| Reply | `itemQuickActions().reply` | `replyToItem()` |
| Forward | `itemQuickActions().forward` | `forwardItem()` |
| More | Always | Opens the item's context menu (`showMenu`) |

### Selection

The selection checkbox and the full-row selection click overlay are shared with the bubble layout. In the flat layout the row is moved by `Modifier.flatStartOffset` for sent and received items alike, and the checkbox is top-aligned using `flatSelectionTopPadding` instead of being centered on the row.

### Roadmap

[REC] Thread side panel (replies to a message in the end column): needs a core query for items quoting a given message. `getChatItemIDs` (`src/Simplex/Chat/Store/Messages.hs`) has no such filter, and `chat_items.quoted_shared_msg_id` has no index.
