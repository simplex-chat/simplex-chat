# Chat View (Conversation)

> **Related spec:** [spec/client/chat-view.md](../../spec/client/chat-view.md)

## Purpose

Full conversation view for displaying and interacting with messages in a direct contact chat, group chat, or note-to-self. Supports text messaging with markdown, media attachments, voice messages, E2E encrypted calls, message reactions, replies, forwarding, reporting, and content search/filtering.

## Route / Navigation

- **Entry point**: Tap a chat row in `ChatListView` (routed by `ChatListNavLinkView`)
- **Presented by**: `ChatView` composable bound to `chatModel.chatId`; on Desktop, shown in the center column
- **Back navigation**: Sets `chatModel.chatId = null`, stops `AudioPlayer`, clears group members, returns to chat list
- **Sub-navigation**:
  - Info button opens `ChatInfoView` (contact) or `GroupChatInfoView` (group) via `ModalManager.end`
  - Member avatars in group chats navigate to `GroupMemberInfoView`
  - Reports button opens `GroupReportsView` for groups with moderation reports
  - Support chats button opens `MemberSupportView` (moderators) or member support chat (regular members)

## Page Sections

### Navigation Bar (`ChatLayout`)

Custom toolbar with themed background:

| Element | Description |
|---|---|
| Back button | Returns to chat list; stops audio/video playback |
| Contact/Group avatar | Small profile image in toolbar |
| Chat name | Display name; tappable to open info view |
| Verified shield | Shows verified contact checkmark (direct chats with verified contacts only) |
| More menu button | Opens overflow menu containing search and audio/video call buttons (call buttons shown in direct chats only) |
| Info button | Opens `ChatInfoView` (direct) or `GroupChatInfoView` (group) |
| Reports count | Badge for group reports count; taps open reports view |
| Support chats | Badge for member support; taps open support chat view |

### Message List

Rendered by `LazyColumnWithScrollBar` with pagination:

| Feature | Description |
|---|---|
| Scroll direction | Bottom-to-top (newest messages at bottom) |
| Pagination | `apiLoadMessages` called on scroll to load more; supports `.before`, `.after`, `.around`, `.initial` |
| Merged items | Adjacent messages grouped with `ItemSeparation` (timestamp, large gap, date separators) |
| Message appearance | Telegram reference system typography, compact text insets, circular bubble corners and small joined corners; five-minute sender runs stop at dates/events/deleted items. Each message shows a timestamp. |
| Sending motion | Newly sent text/link bubbles rise from the input over 250 ms, with a short fade and subtle scale. Replies and notes use the same transition. System animation settings are respected. |
| Floating buttons | Scroll-to-bottom button with unread count |
| Date separators | Date headers between messages from different days |
| Wallpaper | Per-chat themed background via `perChatTheme` from contact/group `uiThemes` |
| Content filter | Filter messages by type via `ContentFilter` (images, files, links, etc.) |

In received group messages, the first bubble of a run contains the sender name,
badge and moderator role; the avatar sits at the bottom of the last bubble. Reply
previews use a compact tinted card with an accent line and bold sender. Metadata
reserves its actual rendered width so text and status indicators do not overlap.
System, deletion and decryption error information remains visible in the compact
message typography. The input text also uses the system sans-serif font.

The send transition starts after successful local insertion. History, incoming
messages, edits, forwards, live updates, reports and attachments keep their normal
appearance. Leaving the chat cancels its transitions; missing bounds, very tall
bubbles or delayed API responses use ordinary rendering. This initial adaptation
moves the existing bubble; Telegram's separate text-reflow morph is not included.

### Message Types

Each type has a dedicated composable in `views/chat/item/`:

| Type | Composable | Description |
|---|---|---|
| Text | `FramedItemView` | Rendered with markdown (bold, italic, code, links, `@mentions`) via `CIMarkdownText` |
| Image | `CIImageView` | Thumbnail with tap-to-fullscreen via `ImageFullScreenView` |
| Video | `CIVideoView` | Video thumbnail with play button; inline playback via `VideoPlayerHolder` |
| Voice | `CIVoiceView` | Waveform visualization with playback controls and duration |
| File | `CIFileView` | File icon, name, size; download/open actions with progress indicator |
| Link preview | `ChatItemLinkView` | URL preview card with title, description, image (defined in `LinkPreviews.kt`) |
| Emoji-only | `EmojiItemView` | Large emoji rendering without message bubble |
| Call event | `CICallItemView` | Call status (missed, ended, duration) |
| Group event | `CIEventView` | Member joined/left, role changes, group updates |
| E2EE info | `CIChatFeatureView` | Encryption status and feature change notifications |
| Group invitation | `CIGroupInvitationView` | Inline group join invitation card |
| Deleted | `DeletedItemView` / `MarkedDeletedItemView` | Placeholder for deleted messages |
| Decryption error | `CIRcvDecryptionError` | Error with ratchet sync suggestion |
| Invalid JSON | `CIInvalidJSONView` | Developer fallback for malformed items |
| Integrity error | `IntegrityErrorItemView` | Message integrity/gap warnings |

### Message Interactions

On Android, tap an ordinary message row, including empty space beside its bubble,
to open its context menu. Long-pressing the message or adjacent empty space selects
it and opens the bulk-action toolbar; subsequent taps toggle
message selection. This selects whole messages, independently of desktop text
selection. Desktop retains its existing long-press/right-click menu and drag text
selection.

A short haptic tick marks the swipe-to-reply threshold on Android. It occurs once
per drag for messages that can be replied to and respects system haptic settings.

Links, mentions, commands, secret reveals, quoted-message navigation, media opening,
file controls and reactions keep their own tap actions. Long-presses on nested
message content follow the same message-selection action on Android.

Android message menus align with the message edge, with a small gap. They open
below the bubble when possible, otherwise above it. Long menus scroll within the
available space. A tall, narrow bubble can use a menu alongside it. If a message
fills the viewport and leaves no usable adjacent space, the menu stays within the
window and minimizes overlap. Placement adapts to the keyboard and window size.

| Action | Description |
|---|---|
| Reply | Sets compose bar to reply mode with quoted message (`ComposeContextItem.QuotedItem`) |
| Forward | Opens destination picker; uses `apiPlanForwardChatItems` with confirmation for partial forwards |
| Copy | Copies message text to clipboard |
| Edit | Enters edit mode (`ComposeContextItem.EditingItem`); own messages within edit window |
| Delete | Delete for self or delete for everyone (with confirmation via `deleteMessagesAlertDialog`) |
| Moderate | Group moderators can delete messages for all members (`moderateMessagesAlertDialog`) |
| React | Emoji reaction picker |
| Report | Report message to group moderators (`ComposeContextItem.ReportedItem` with `ReportReason`) |
| Select multiple | Enters multi-select mode (`selectedChatItems`) with bulk delete/forward/archive toolbar |
| Archive | Archive selected reports (moderators) |
| Details | Opens message information; delivery/signature explanation remains available there when present |

### Chat Image Previews

On Android, single-photo previews use Telegram-style bounds: portrait width starts
at 70% of the window's short side, while square and landscape photos use that side
minus 64dp. Height is limited to the initial width plus 100dp; tall portraits shrink
in both dimensions. Panoramas use a 120dp minimum height with centered cropping.
Tablet bounds account for a navigation pane, and actual chat gutters can narrow
the preview further. Measured message metadata sets a minimum width for thin images.

Preview dimensions stay stable when the full image loads or the keyboard opens.
Captions wrap within the existing media bubble layout. Small quote/list thumbnails
and desktop image sizing retain their existing behavior.

### Full-Screen Images

Tapping a loaded, revealed image expands its thumbnail into the existing full-screen
gallery over 200 ms while the black background fades in. The animation preserves
the image's proportions and uncrops tall or square previews. The source thumbnail
stays hidden while owned by the viewer and is restored on close or before a
fade-out fallback.

Closing returns to the visible source when the original page is settled and
unzoomed. Closing a different page, a zoomed image or a missing source uses a fade.
A close during opening reverses the opening path. Paging and zoom gestures resume
after opening; system animation settings are respected. Hidden media first follows
the existing reveal/download flow. Video and other viewer callers retain their
existing presentation.

The image transition is shared by Android and Desktop; Android's message gesture
changes do not change desktop message interactions. Source-corner fidelity remains
a [recommendation](../../spec/client/chat-view.md#image-viewer-transition).

### Compose Bar (`ComposeView` + `SendMsgView`)

Bottom input area for composing messages:

| Element | Description |
|---|---|
| Text field | `PlatformTextField` with markdown support, `@mention` autocomplete, file paste support |
| Attachment button | Opens `ModalBottomSheetLayout` with options: camera, gallery (image/video), file |
| Send button | Sends message; changes to checkmark for reports; animated size/alpha |
| Voice record button | Shown when text is empty and voice allowed; hold to record, release to preview |
| Live message button | Start/update live typing message (if `liveMessageAlertShown`) |
| Context preview | Shows quoted message, editing indicator, or forwarding source above text field |
| Media preview | Thumbnail row of selected images/videos before sending |
| Link preview | Auto-generated link preview card (`ComposePreview.CLinkPreview`) |
| Connecting status | "Connecting..." text shown when contact is not yet ready |
| Commands menu | Developer commands (`showCommandsMenu`) |

Compose states (`ComposeState`):
- `NoContextItem` -- normal new message
- `QuotedItem` -- replying to a message
- `EditingItem` -- editing own message
- `ForwardingItems` -- forwarding from another chat
- `ReportedItem` -- reporting a message with reason

### Multi-Select Toolbar (`SelectedItemsButtonsToolbar`)

Shown when `selectedChatItems != null`:

| Button | Description |
|---|---|
| Delete / Archive | Delete selected messages (for self, or for everyone if allowed by `fullDeleteAllowed`); shown as Archive for report items (group moderators only) |
| Forward | Forward selected messages to another chat |
| Moderate | Delete selected messages for all members (group moderators only) |

### Timed/Disappearing Messages

When `timedMessageAllowed` is true, compose bar includes a timer icon for setting message disappear time via `customDisappearingMessageTimePref`.

## Source Files

| File | Path |
|---|---|
| `ChatView.kt` | `views/chat/ChatView.kt` |
| `ComposeView.kt` | `views/chat/ComposeView.kt` |
| `SendMsgView.kt` | `views/chat/SendMsgView.kt` |
| Chat item views | `views/chat/item/*.kt` |
| Full-screen image gallery | `views/chat/item/ImageFullScreenView.kt` |
| Image transition geometry and lifecycle | `views/chat/item/ImageGalleryAnimation.kt` |
| Message details | `views/chat/ChatItemInfoView.kt` |
| Android message menu placement | `views/chat/item/ChatItemMenu.kt` |
