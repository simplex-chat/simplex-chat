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

The Chat View is the primary message display and interaction surface in SimpleX Chat. It is built around the [`ChatView`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L99) composable, which orchestrates a `ChatLayout` containing a reverse-scrolling `LazyColumn` of `ChatItemView` items and a `ComposeView` for message input. The view supports direct chats, group chats, local notes, and contact connections, with per-chat theming, search/filter, multi-select, and side-panel info modals. Message rendering is delegated to type-specific composables in the `views/chat/item/` package.

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
|   |   |       |-- sender row, avatar in groups
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

**Location:** [`ChatView.kt#L98`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L98)

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

On chat ID change (via [`snapshotFlow` on `chatModel.chatId.value`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L180)):

1. Marks unread chat as read (`markUnreadChatAsRead`)
2. Clears group members state
3. Resets search, content filter, and selection
4. Fetches available content types (`updateAvailableContent`)
5. For direct chats, loads contact info and connection stats
6. For groups with pending membership, opens member support chat

### Chat Type Routing

The outer [`when (chatInfo)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L274) branches:

| ChatInfo Type | Behavior |
|---|---|
| `ChatInfo.Direct`, `ChatInfo.Group`, `ChatInfo.Local` | Full `ChatLayout` with compose, search, reactions, per-chat theme |
| `ChatInfo.ContactConnection` | `ModalView` wrapping `ContactConnectionInfoView` |
| `ChatInfo.InvalidJSON` | `ModalView` with raw JSON display and share button |

---

## 3. Message List

**Location:** [`ChatView.kt#L1757`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L1757) (`ChatItemsList` composable)

The message list is a `LazyColumnWithScrollBar` with `reverseLayout = true`, meaning index 0 is the newest message at the bottom of the screen.

### Key Behaviors

- **Merged Items:** Messages are grouped via [`MergedItems.create()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L1821), which collapses consecutive similar system events into expandable groups. Revealed state is tracked in `revealedItems`; `MergedItem.revealed` is true for a single item and for a revealed group row.
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

Long-press or right-click opens a dropdown menu with context-sensitive actions (see section 6).

### Terminal Layout Adjustments

When `LocalTerminalLayout.current` is true, `ChatItemView` and the item views it calls keep their content and start at one text edge, `CHAT_ITEM_TEXT_PADDING` (12dp) inside the item; messages are drawn without the bubble fill and tail:

- Items are start-aligned for both directions ([`alignEnd`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L125)). The item click has no indication; the row draws it (section 10). `GoToItemButton` is placed as described in section 10 ([L345](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L345), [L817](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L817)). Voice items use the arrangement of received items for both directions.
- [`chatItemTailEnabled()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L1220) returns false, so `shapeStyle` draws no tail.
- `FramedItemView` has a transparent background. Headers (report, moderated, blocked, deleted, live, forwarded) and the quote have no background and start at the text edge. The quote has the bar of Markdown quotes ([`Modifier.quoteBar`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/MarkdownBlocks.kt#L151)), its text in `onSurface` at 70%, 15sp. The bar and the padding, `TERMINAL_BLOCK_TOP_GAP` (5.75sp) above and `TERMINAL_BLOCK_BOTTOM_GAP` (5sp) below, span the whole quote row, including an image or video thumbnail; the thumbnail has the message corner clip of media (`thumbnailClip`, `clipChatItem`, rounded by the `chatItemRoundness` preference, square at 0); the file, voice and chat link icons of the quote have no top padding. Headers: `TERMINAL_HEADER_TOP_GAP` (2.5sp) above, `TERMINAL_HEADER_BOTTOM_GAP` (1.5sp) below; the header text uses `terminalTextStyle` at its size in `secondary`, and the icon is centred on it and takes no height (`Modifier.inlineIcon()`).
- Images, videos, link previews and chat link cards start at the text edge, `TERMINAL_BLOCK_TOP_GAP` below the part above them, and keep the message corner clip ([`mediaModifier`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L56)). Link previews and chat links are cards in the quote colour ([`cardModifier`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L58)). The item itself is not clipped (`clipChatItem` of `FramedItemView` and `ChatItemView` is skipped), so headings and emoji are drawn whole. An image or video without caption, or a chat link card without text (`gapBelowBlock`, computed with `metaReserved`, which is passed to `ItemWithMeta`), has `TERMINAL_BLOCK_BOTTOM_GAP` below it, outside `ItemWithMeta`. Captions are selected by the classic conditions.
- Captions (user, D30): the text below an image, video, link preview, chat link card, voice message or file is laid out as the text of a close follow-up message, the time on it only. `PriorityLayout` measures it in the full width of the item (`limitToPriorityWidth = false`; classic: the width of the media) and places the elements at the start edge in both directions (`placeRelative`), the caption of a link preview has no width limit (classic: `DEFAULT_MAX_IMAGE_WIDTH`), and a chat link card keeps its own width (`width(IntrinsicSize.Max)`). Its gap below the media is the gap from the same media without text to a close follow-up message. An image or video without caption keeps the classic limit (`limitToPriorityWidth = true`): a quote or header above it takes the width of the media, so the white time over the media stays on its bottom end corner.
- [`CIMarkdownText`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt#L481): top padding `terminalTop` (required): 0 below the sender row, a header, the quote, a voice message or a file, `TERMINAL_BLOCK_BOTTOM_GAP` below an image, video, link preview or chat link card; plus [`terminalHeadingGap(item)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L121-L122), `TERMINAL_LEVEL_STEP` when the text starts with a heading and is not at the top of the item (below a quote, a header or a block); no bottom padding. Its text style: `body1` in `messageTextColor()`, merged with `terminalTextStyle` at the `body1` size. The default style of `MarkdownText` is classic and is used outside the rows (the compose context preview).
- Code block cards, table cards and rules of `MarkdownBlocks`: `TERMINAL_BLOCK_TOP_GAP` above, `TERMINAL_BLOCK_BOTTOM_GAP` below, except the last block of a message without a meta line (`marksLastBlock`; a live message has the meta line with the typing indicator, so its last block keeps the gap). That block has no bottom margin and is marked with `messageLastBlock()`: `TERMINAL_BLOCK_BOTTOM_GAP` is added below it when the time stays beside it, and the time moved under it is `TERMINAL_META_GAP` below its edge (section 10). In a message of rules only, the rules are at least as wide as the copy of the time (`ruleMinWidth` of `MarkdownBlocks`, measured by `MarkdownText`; user, D31), as in classic, where the meta line sets the width; the time stays beside them unless a wider quote or header widens the item so that the rule leaves no room for the time.
- Text blocks of [`MarkdownBlocks`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/MarkdownBlocks.kt#L59): a paragraph break, and a heading that does not start the text, add `TERMINAL_LEVEL_STEP` (4.75sp) (classic: half the line height, before a paragraph break only); above a heading that starts the text, the gap is added by `terminalRowGap` or `terminalHeadingGap`. A block after a heading, other than a heading, has `TERMINAL_HEADING_BOTTOM_GAP` (half of `TERMINAL_LEVEL_STEP`, 2.375sp) above it instead, so a heading is nearer to its own text than to the text above it.
- Text styles: [`terminalTextStyle(fontSize, color)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L78-L84) sets the line height to font size × `TERMINAL_LINE_HEIGHT_RATIO` (22/16) × f(w) and `LineHeightStyle(Proportional, Trim.None)`. Every line takes the full line height, one-line texts included. The space added to a line is split in the ratio of the font's ascent to its descent. On desktop each run of a line is given its own line box; with this alignment, runs of different sizes (the 12sp time in the copy of the meta) are placed on one baseline in a line of the body line height. f(w) = 1 − `TERMINAL_LINE_HEIGHT_CONTRAST_SLOPE` (0.1515) × (1 − w); w: the L* contrast of the text colour against `MaterialTheme.colors.background`, divided by that of `messageTextColor()`, limited to 0…1, and 1 when the message text has no contrast. Used for message text and captions (the style of `CIMarkdownText`), Markdown quotes (text colour at 70%), code, headings (body line height × scale), quote text and the quote's sender (in `body1`, Inter, in terminal rows), the sender name and role, event texts ([`chatEventTextStyle(base)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIEventView.kt#L22-L28): 12sp, `secondary`, merged into `base`, by default `LocalTextStyle.current`; classic: `base`; in both layouts the text direction is left unspecified, which Compose resolves to the layout direction, so that an event starting with a name in the opposite direction keeps the copy of the time at the end of the row), header texts and the time (classic: unchanged). For names and the white time over media (`lineHeightColor` of `CIMetaView`), f is computed with the message text colour. Line heights in the light and dark themes: message text 22sp, quote text 19.6sp and 20.1sp, event text and time 15.1sp and 15.5sp.
- Headings: body size × `TERMINAL_HEADING_RATIO` (1.3125) to the power 2, 1, 0 for levels 1, 2, 3, 27.6, 21 and 16sp for 16sp text; levels 4–6 also at the body size; all bold (classic: 1.5, 1.3, 1.15); line heights scaled the same. `Modifier.headingBox` lays a heading out with the space of a body line above its capitals and below its last baseline (Inter cap height 2048/2816 em). It uses the px ratio of the body and heading font sizes: Android scales font sizes non-linearly from font scale 1.03 and, above 1.05, sets line heights in proportion to the font size in px, so the capitals are placed as body capitals at every font scale (within 1px at 1, 1.15 and 1.3).
- Files: `TERMINAL_FILE_TOP_GAP` (2sp) above the file row, no padding below, no padding of its icon. Voice messages drawn by `FramedItemView` (with a caption, a quote or a header; `hasText = true`): the layout of a voice message drawn alone (4sp above, 6sp below, its indicator), without its own time; no padding below the item.
- Emoji-only items, also under a quote or a header: `TERMINAL_EMOJI_TOP_GAP` (4.25sp) above, `TERMINAL_EMOJI_BOTTOM_GAP` (1.5sp) below (classic: 8dp; under a quote or header 6dp above, 8dp below). Under a quote or a header the emoji is not stretched to the item width, so its reported last line is the emoji itself; `PriorityLayout` places it at the start edge in both directions.
- Item gaps: one gap above each row, [`terminalRowGap(item, older, separated, largeGap, hasSenderRow)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L118-L119), passed by `ChatItemsList` as `topGap` of the row. It is `TERMINAL_LEVEL_STEP` when an older item exists, the rows are not separated and one of these holds: the gap to the older item is large; the row shows a sender row; either item is an event (content without a terminal sender); the text of the item is at its top (text or unknown content without a file, without quote, header, deletion, live state or report) and its first block is a heading. Otherwise it is 0. `separated` (`terminalRowsSeparated`): a date separator is drawn between the two rows (the older row is revealed, a single item or a revealed group row, and its date differs from that of the oldest item of the row), or the older item is the chat banner. For a collapsed group, `older` is the item older than the group, and `largeGap` is computed against the oldest item of the group. The bottom of a row is `TERMINAL_LEVEL_STEP` when a date separator is drawn below it, 8dp for the newest row, as classic, otherwise 0 (`ChatItemBox(top, bottom)`; classic: 1dp or 4dp above, 1dp or 4dp below). Above a sender row, `TERMINAL_SENDER_TOP_GAP` (6.5sp) is added; below the name, `TERMINAL_SENDER_BOTTOM_GAP` (5.5sp). The gaps are constants in sp at the top of `TerminalItemView.kt`, converted to dp with the font scale. The bottom padding of call items (8dp), the paddings of voice messages drawn without `FramedItemView` (no caption, quote, header, deletion or live state; 4sp above, 6sp below) and the 6dp between the two rows of the forwarded-from header are classic.
- Vertical gaps, measured from the baseline (or a block's bottom edge) to the top of the tallest letter, a heading's capitals, or a block's top edge:

```
level 1   lines of a quote
level 2   lines of a message; messages of one sender within 60 seconds; a block against the text line or block next to it
level 3   messages of one sender 60 seconds or more apart; sender name → message or header; paragraph break; above a heading, also at the start of a message
level 4   last line of a message → next sender name; above and below an event
```
- Event, feature and member-created items use [`chatEventPadding(vertical)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIEventView.kt#L30-L33): in terminal rows the text edge at the sides, `TERMINAL_EVENT_TOP_GAP` (8.25sp) above and `TERMINAL_EVENT_BOTTOM_GAP` (1.5sp) below, so that the capitals of the event text are where the capitals of a sender name would be and its last baseline is as far from the row end as that of a message line (classic: 6dp at the sides, `vertical` above and below, 6dp by default, 8dp for the merged feature icons). The icons of feature and preference items are centred on the text and take no height (`Modifier.inlineIcon()`); the row of merged feature icons is one event text line high. `CIEventView` has no line limit in terminal rows, so the copy of the time stays at the end of the text (user, D23; classic: 4 lines). Voice and call items and reactions start at the text edge.
- Time and status: placed at the end of the row by [`ItemWithMeta`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L200-L214), the baseline of the time on the last baseline of the content (section 10). Message text ends with [`terminalReserveSpaceForMeta`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIMetaView.kt#L227-L270): a transparent copy of the meta joined to the last word by no-break spaces, in `FontWeight.Normal`. The copy starts with a word joiner (U+2060), which forbids a line break before the copy, also after the three spaces of an idle live message (`noTyping`), so the time is not moved alone to the next line. The direction mark of its paragraph follows (U+200E left-to-right, U+200F right-to-left), so that the bidirectional algorithm places the copy at the end of the line, also after words in the other direction on a wrapped last line. Its spaces and texts are appended by [`reserveSpaceForMeta`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIMetaView.kt#L145-L225), which has the conditions of `CIMetaText`, except that the timer is reserved for every timed message, and returns the classic string unchanged, through span parameters: icon spaces and the spaces between them are no-break spaces at the `body1` size in dp (16dp converted to sp with the density, so that they keep their width at every font scale, as the icons do; also after a heading and in event text), 18dp for each icon, wider than a 12dp icon with its spacer (15dp or 16dp); after the 17dp icons one more space is added (22.5dp against 21dp): the sent and received status icons (`tallStatusIcon`, also used by `CIMetaText`) and the via-proxy icon; the time and the timer text of a disappearing message are at 12sp. The copy includes the file encryption and signature icons (`encrypted` and `signedFileVerified` of `MarkdownText`, passed by `CIMarkdownText` from the item). When the last word and the copy do not fit in the line, the last word moves to the next line with the time. Text whose paragraph direction (`textLayoutDirection`: right-to-left when the first 50 characters of the message text hold a right-to-left character; all blocks and lines of a message take it) differs from the outer direction (`parentDirection`, passed by the blocks and by the meta line of a live message; otherwise `LocalLayoutDirection`) ends with a line break instead of the copy, so the time is placed on an added empty line. Emoji, and the last code block, table or rule of a message, report their width (`Modifier.messageLastLine()`, `Modifier.messageLastBlock()`); the time moves under them when the row leaves no room for it beside them. The meta line with the time is drawn under such a block only for live messages, for the typing indicator. File text keeps the space reserved for the time at the end of its size line. Images and videos without a caption (`captionlessMedia`) keep the meta in `secondary` beside the media when it fits in the row, otherwise the meta is drawn over the bottom end of the media in white, as in the classic layout (`overlayMeta`). Event items are wrapped in [`ChatEventWithMeta`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/CIEventView.kt#L35-L48), which passes the separator and the end of the event text apart (`separator`, `ts`): in the classic layout two spaces and the time without a style, in the terminal layout three transparent no-break spaces in `chatEventStyle` and the transparent `terminalReserveSpaceForMeta` of the meta without status and edit icons, so the last word of the event text is moved to the next line with the time when they do not fit. Each view puts the separator where the classic layout has two spaces (user, D28): after the event text, or in the "Open" and "Accept" links, so the classic output is unchanged.
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
| `terminal = chatInfo.activeChatLayout(user) == ChatLayoutMode.Terminal` | [`ChatView`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L268) |
| `LocalTerminalLayout provides terminal`, next to `LocalAppBarHandler` and `LocalSelectionManager` | [`ChatView`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L269-L273) |
| `drawWallpaperImage = false` of `chatViewBackgroundModifier` | [`ChatLayout` composable](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L1010) |

[`LocalTerminalLayout`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L56) is `false` outside `ChatView` and the theme screens (below); chat item info keeps the classic look. The value applies to every `ChatView` of the chat: main view, member support chats and reports.

Colours: the app, user and chat themes apply as in the classic layout. Wallpaper: `chatViewBackgroundModifier` fills the chat with the background colour of the wallpaper (preset or custom) and omits the image.

### Functions

| Function | Purpose |
|---|---|
| [`terminalTextStyle(fontSize, color)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L78-L84) | Terminal: line height and line height style of a text in the rows (section 4); classic: `TextStyle.Default`, so that merging it changes nothing |
| [`terminalItemShowsSender(item, older)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L102-L103) | Whether a row shows the sender row (sender rule below). `older`: the next older item in the list |
| [`sameSender(item, older)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L105-L113) | Private; same sender for grouping |
| [`terminalRowGap(item, older, separated, largeGap, hasSenderRow)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L118-L119) | The gap above a row (section 4, item gaps): `TERMINAL_LEVEL_STEP` or 0. Used by `ChatItemsList` and the theme preview |
| [`terminalHeadingGap(item)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L121-L122) | `TERMINAL_LEVEL_STEP` above a heading that starts the text of an item below a quote, a header or a block; added to the top padding of `CIMarkdownText` |
| [`ChatItem.hasQuoteOrHeader`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L836-L837) | A quote, a forwarded header, a deletion, the live state or a report: the item is drawn by `FramedItemView` with a quote or header above its content. Used by `ChatItemView` to choose `EmojiItemView` or `CIVoiceView` and by the heading gaps |
| [`CIContent.hasTerminalSender`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L130-L137) | Private. True for sent messages, sent deleted and moderated items, sent calls and group invitations, and received content with `showMemberName` (messages, deleted, moderated and blocked items, calls, errors, group invitations, invalid JSON); false for events, feature and preference changes, E2EE info and the chat banner |
| [`TerminalRowInteractionSource`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L139-L153) | Interaction source of the row and its item: press interactions are emitted at the position where the pointer went down in the row, so the ripple starts there |
| [`Modifier.terminalRowIndication(source)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L155-L166) | Records the pointer-down position; hover and press indication of the whole row |
| [`Modifier.terminalStartOffset(offset)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L168-L173) | Shifts the row by the selection offset and narrows it by the same amount |
| [`Modifier.messageLastLine()`, `Modifier.messageLastBlock()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L179-L187) | `messageLastLine()` sets the private alignment lines `LastLineLeft` and `LastLineRight` to the left and right edges of the element: emoji. `messageLastBlock()` also sets `LastBlockBottom` at the bottom edge of the element: the last code block, table or rule of a message without a meta line, which has no bottom margin |
| [`Modifier.inlineIcon()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L189-L198) | Terminal: lays the element out with no height, centred on its position, so that a row with `Alignment.CenterVertically` takes the height of its text and centres the element on it: icons of feature and preference items and of message headers. Classic: the modifier unchanged |
| [`ItemWithMeta(meta, overlayMeta, metaReserved, endButton, content)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L200-L214) | Classic: `content` with the meta passed as `inlineMeta`. Terminal: `content` without meta, and the meta in a box with 12dp end padding at the end of the available width, the message column of the row (private [`TerminalMetaLayout`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L216-L256)). `endButton`, in a box with the same end padding, is placed at the end of the row, centred vertically between the content top and the top of the time box (the whole content when the time is over media); the content is measured in the width less the wider of this box and, without `metaReserved`, the time box. The meta is measured first. Without `metaReserved` and `overlayMeta`, the content is measured in the width less the width of the box, so that it does not reach under the time. `metaReserved` is true for content with its own reserve of the space or a reported last line: text with the transparent copy (`metaReserved` of `FramedItemView`), files, emoji and event items. The first baseline of the box (the baseline of the time; without a time, the baseline of a 12sp time centred in the box) is placed on the last baseline of the content. When the content reports its last line and the line and the box together are wider than the row, the box is placed `TERMINAL_META_GAP` (6sp) under the content and the bottom of the row moves down by that gap and the height of the box. When the box stays beside a last block reported with `LastBlockBottom` at the content bottom (a table, or a code card or rule, which span the width of the item), `TERMINAL_BLOCK_BOTTOM_GAP` is added below the content. Content without a baseline (a message of a rule only, a failed call), and captionless media with `overlayMeta`: the box ends `TERMINAL_META_GAP` above the content bottom; when the content and the box together are wider than the row, the `overlayMeta` box is placed at the bottom end of the content instead, `TERMINAL_META_GAP` above its bottom. Either box is placed at the top of the content at most, and the row is at least as tall as the box. When the rule leaves room for the time beside it, the time stays on the baseline of the last text line above it (D24); otherwise it is placed under the rule. |
| [`ChatItemSender`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L258) | Name, badge, role label, `own`, avatar |
| [`chatItemSender(chatInfo, cItem, range, reversedChatItems, showMemberInfo, showChatInfo)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L260-L307) | Sender of a row (table below) |
| [`SenderNameAndRole(sender, modifier)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L312-L335) | Private; name and badge (13.5sp; primary for own items, secondary otherwise) and role label |
| [`terminalGutter(chatInfo)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L337-L339) | Gutter width: the member image size (scaled by `fontSizeSqrtMultiplier`) plus 4dp in groups, 0 in other chats |
| [`TerminalChatItemRow(sender, gutter, topGap, modifier, senderTopGap, content)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/TerminalItemView.kt#L341-L383) | Row layout below |

In `ChatView.kt`, shared with the classic date separator:

| Function | Purpose |
|---|---|
| [`terminalRowsSeparated(olderRow)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L3817-L3820) | A date separator or the chat banner between a row and the older row: the newest item of the older row is the banner, or [`rowItemSeparation(row)`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L3812-L3815) of the older row has a date. False without an older row |
| `rowItemSeparation(row)` | Private; the separation of a row, also used by `ChatItemsList` for the date separator it draws: `getItemSeparation` of the newest item of the row and the next newer item (the oldest item of the newer row) when the row is revealed (`MergedItem.revealed`: a single item or a revealed group row), without the newer item otherwise (user, D32) |

### Row Layout

```
row padding        top: topGap (terminalRowGap), plus senderTopGap (TERMINAL_SENDER_TOP_GAP, 6.5sp) with a sender row; start 8dp; end 12dp
gutter             terminalGutter(chatInfo)
  avatar           top of the gutter, sender rows of groups only
message column     starts at the gutter
  sender row       name and badge (13.5sp; primary for own messages, secondary otherwise), role label; start padding CHAT_ITEM_TEXT_PADDING; text style terminalTextStyle(13.5sp)
  item             ChatItemView; TERMINAL_SENDER_BOTTOM_GAP (5.5sp) below the sender row
```

`ChatItemsList` passes [`showAvatar`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L2460) = `terminalItemShowsSender(item, merged.oldest().nextItem)`, and the row gets `chatItemSender(...)` when it is true, `null` otherwise; the [gap above the row](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatView.kt#L2461-L2466), with `terminalRowsSeparated` of the older row (the next merged item of the reversed list), is passed to `ChatViewListItem` as `terminalGap`.

### Sender Rule

`terminalItemShowsSender`: a row shows the sender row when the item has a sender (`hasTerminalSender`) and one of these holds:

- it is the oldest loaded item;
- the older item has no sender (an event);
- `sameSender` is false.

The time between the items and a date change do not repeat the sender row.

`sameSender`: when either item is posted as the channel (`ChannelRcv`, or `GroupSnd` with `meta.showGroupAsSender`), both must be; two `GroupRcv` items need the same `memberId`; other items need the same direction class.

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

[`GoToItemButton`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt#L290) is activated by the row hover (`parentActivated`; the row interaction source is passed as `bubbleInteractionSource`).

- Search results: the search icon is drawn by `ChatItemView` after the item, as for classic received items: at the end of the row, centred on the item; every row of the results is narrowed by the button and its 13dp start padding, so their times stay aligned.
- Forwarded items drawn by `FramedItemView`: the arrow is passed as `endButton` and placed by `ItemWithMeta` at the end of the row, centred on the item above the line with the time (the forwarded header line for a one-line message); the text is narrowed by the arrow, and the time keeps its place.
- Forwarded items shown as marked deleted (`markedDeleted`, no header line): the arrow after the item, as in search results.

### Selection and Swipe

The selection checkbox (8dp start padding) and the full-row selection click overlay are shared with the classic layout. The row is moved by `Modifier.terminalStartOffset` for sent and received items alike. Swipe to reply uses the `swipeableModifier` of the classic layout on the whole row (Android; off in selection mode).

### Theme Screens

`LocalTerminalLayout` is provided to the theme screens of a terminal chat or profile:

| Screen | Value |
|---|---|
| Chat theme ([`ChatWallpaperEditorModal`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt#L1300)) | `activeChatLayout` of the chat |
| Appearance › Themes, preset selector ([`ThemesSection`](../../common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/Appearance.kt#L581)) | `profileChatLayout` of the current user, or of none for the app theme destination |
| Customize theme, profile theme, colour editors | the same, through `profileLayoutIsTerminal` and the `terminal` parameter of `editColor` |

With the value true:

- the preview draws two terminal rows ("alice", then "bob" in the own colour) without gutter, the gap between them by `terminalRowGap` and `TERMINAL_SENDER_TOP_GAP`, as in the chat; [`PreviewInEitherLayout`](../../common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/Appearance.kt#L337) measures both layouts and shows the active one centred in the larger height, so a layout switch keeps the preview height;
- the preview and the preset tiles show the wallpaper colour without the image;
- the custom image tile, the image scale, the scale slider and the wallpaper accent row are hidden.

Layout controls:

| Control | Values | Stored in |
|---|---|---|
| Chat theme screen, first section ([`ChatLayoutSection`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt#L1313)) | the chat default, "(chat)" when the chat layout is the default, otherwise "(default)"; the other layout | chat theme record, by [`setChatLayout`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt#L1382) and `apiSetChatUIThemes` |
| Appearance › Themes, app theme destination ([`ChatLayoutRow`](../../common/src/commonMain/kotlin/chat/simplex/common/views/usersettings/Appearance.kt#L950)) | Classic, Terminal; the selection is `profileChatLayout(null)` | `appPrefs.chatLayout` |
| Appearance › Themes, profile destination | `profileChatLayout(null)` "(default)"; the other layout | profile theme record, by `saveThemeToDatabase` and `apiSetUserUIThemes` |
