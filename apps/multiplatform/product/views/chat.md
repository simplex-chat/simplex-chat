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
| Floating buttons | Scroll-to-bottom button with unread count |
| Date separators | Date headers between messages from different days |
| Wallpaper | Per-chat themed background via `perChatTheme` from contact/group `uiThemes`; classic layout only |
| Content filter | Filter messages by type via `ContentFilter` (images, files, links, etc.) |

### Message Types

Each type has a dedicated composable in `views/chat/item/`:

| Type | Composable | Description |
|---|---|---|
| Text | `FramedItemView` | Rendered with markdown (bold, italic, code, links, `@mentions`) via `CIMarkdownText` |
| Image | `CIImageView` | Thumbnail with tap-to-fullscreen via `ImageFullScreenView`; animated GIFs play inline and full screen |
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

Long-press context menu on any message:

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

### Compose Bar (`ComposeView` + `SendMsgView`)

Bottom input area for composing messages:

| Element | Description |
|---|---|
| Text field | `PlatformTextField` with markdown support, `@mention` autocomplete, file paste support |
| Attachment button | Opens `ModalBottomSheetLayout` with options: camera, gallery (image/video), file |
| Emoji button | Desktop only (Android keyboards have their own emoji panel). Opens a popup with search and categories; a picked emoji is inserted at the cursor and the picker stays open. With text selected the emoji goes after it, as the text field drops its selection when the picker takes focus. Picked emoji are not remembered, for privacy. Esc, a click outside or the button closes it |
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

### Terminal Layout

Used for a direct or group chat whose layout is Terminal (chat layout set in Appearance settings, per user profile or on the chat theme screen), on Android and desktop: the main chat view, member support chats and reports. Note folders use the classic layout. App, user and chat theme colours apply; the wallpaper is omitted, and the chat is drawn on the theme background.

| Element | Description |
|---|---|
| Message rows | Full-width rows, start-aligned for sent and received messages. Text, media and deleted messages are drawn on the chat background; group invitations and error items keep their rounded coloured card |
| Sender header | Avatar, name, role label and time on the first message of a group. Role label: the member role above Member, or "channel" for channel posts (including an owner posting as the channel). A new header follows an event, a date change, a sender change (including an owner switching between posting as the channel and as themselves), or a gap of 60 seconds or more |
| Continuation rows | Avatar and name omitted. Time in the start gutter: desktop, on hover; Android, on the newest row of each run of rows with the same time |
| Avatar click | Received group message: member info; received direct or channel message, and a post shown as the channel: chat info. Other own messages: inactive |
| Row background | Desktop: light tint on hover. Highlighted message (for example after jumping to a quoted message): primary tint. Both are rounded and slightly inset from the pane edges |
| Avatars | Placeholder avatars have extra contrast against the background |
| Hover action bar | Desktop, top end of the hovered row, items with message content only (events, calls, errors, group invitations and moderated items are excluded; marked-deleted messages show only More): 3 quick reactions, add reaction (remaining reactions), reply, forward, and More (the context menu). Each button follows the rules of the context menu. Hidden in selection mode and when the pointer leaves the row, unless the reaction picker is open |
| Message time | Omitted from the message. Sent messages keep their delivery status; edited, timer and other icons stay. A received message with the time only shows no meta and reserves no space for it |
| Replies | One compact line above the message in place of the quote block: reply icon, small avatar of the quoted sender, the sender name in semibold (long names truncated), and a one-line snippet. Media without text: its type; voice message: "Voice message" and its duration. Click: scroll to the quoted message; hover: darker text; long-press or right-click: message menu |
| Code blocks, tables | Cards in one colour for sent and received messages |
| Forwarded and report headers | Drawn on the chat background |
| Deleted messages | Transparent, 14sp italic text, time omitted |
| Reactions | Rounded pills with the count; own reactions tinted |
| Date separators | Date in a pill on a full-width line |
| Voice messages | Same arrangement for sent and received messages |
| Search results | "Go to message" button at the end of each row |
| Compose field | Rounded, lightly filled field with margins; divider above the compose area omitted |
| Swipe to reply | Android: on the whole row |

The context menu and multi-select work as in the classic layout. Item info, the Appearance preview and the chat theme preview keep the classic look.

## Source Files

| File | Path |
|---|---|
| `ChatView.kt` | `views/chat/ChatView.kt` |
| `ComposeView.kt` | `views/chat/ComposeView.kt` |
| `SendMsgView.kt` | `views/chat/SendMsgView.kt` |
| `ComposeEmojiPicker.kt` | `views/chat/ComposeEmojiPicker.kt` (+ `.android.kt`, `.desktop.kt`) |
| Chat item views | `views/chat/item/*.kt` |
