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
| Wallpaper | Per-chat themed background via `perChatTheme` from contact/group `uiThemes`; terminal layout: background colour without the image |
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

Used for a direct or group chat whose layout is Terminal, on Android and desktop: the main chat view, member support chats and reports. The first set of these is used:

1. the user's choice for the chat, on the chat theme screen;
2. the layout of the chat: of the group profile, set by an owner, or of the address the contact was created from;
3. the user's choice for the profile, in Appearance with the profile as destination;
4. the app setting, in Appearance with the app theme as destination;
5. Classic.

Note folders use the profile choice and the app setting. The choices for the chat and the profile are stored in their themes; "Reset to app theme" and "Remove image" keep them. App, user and chat theme colours apply; the wallpaper colour is drawn without its image.

| Element | Description |
|---|---|
| Message rows | Full-width rows, start-aligned for sent and received messages; text, quotes, media, cards, events, voice, calls and reactions start at one edge. Messages are drawn without bubbles; group invitations, deleted messages and error items keep their rounded card |
| Sender row | Name and role label above the first message of a group of messages; the own name in the primary colour. Role label: the member role above Member, or "channel" for channel posts, including an owner posting as the channel. A new sender row follows an event or a sender change (including an owner switching between posting as the channel and as themselves); as in the classic layout, time gaps and date changes do not repeat it |
| Avatars | Groups and channels only, at the start of the sender row. Click: member info for a received group message, chat info for a channel post |
| Time | At the end of the row, on the baseline of the last line of text, for every item with a time: messages, media, voice, calls, deleted and error items, group invitations, events. Sent messages show the delivery status with it. When the last line leaves no room for the time, its last word moves to the next line with the time; after a code block, table, rule or emoji without room, the time moves under it. Images and videos without caption show it over the media in white, as in the classic layout, when the row has no space beside them |
| Quotes | A bar and lighter text, as Markdown quotes |
| Spacing | One line height ratio (22/16 of the font size, smaller for lighter text) for message text and captions, quotes, code, headings, the sender row, message headers, event texts and the time; every line takes the full line height, one-line texts included. Cards, files, voice messages and calls have the classic line heights until their redesign. Gaps are scaled with the font size. Four gaps, smallest first: lines of a quote; lines of a message, messages of one sender within a minute, and blocks of a message (quote, code, table, image, video, file, voice, card) against the text or block next to them, also in the next message; messages of one sender a minute or more apart, sender name → message or message header, a paragraph break, and the gap above a heading, also when it starts a message or follows a quote, header or block; last line of a message → next sender name, and the gaps above and below an event, whatever the time between them. Message headers (forwarded, moderated, live, report) sit one line gap above the text. Headings: 1.3125, 1.72 and 2.26 times the text size; the text under a heading is half a step further than the lines of a message, so a heading is nearer to its own text than to the text above it. Quote thumbnails are rounded as media, by the message roundness setting |
| Link previews, chat links | Cards in the quote colour |
| Captions | The text under an image, video, link preview, chat link, voice message or file is laid out as a close follow-up message with the same text: the full row width and the same gap under the media; only the time is on the caption, not on the media. The media keep their own size and layout |
| Rules | A message of only a rule draws the rule as wide as the time, with the time beside it |
| Go to message | Search results and forwarded messages: a button in the start gutter, centred on the message, below the avatar when the message is shorter |
| Row background | Hover and press over the whole row; selected and highlighted messages tinted in the primary colour |
| Swipe to reply | Android: on the whole row |

Theme screens of a terminal chat or profile show the preview as terminal rows and the wallpaper presets as colours; the custom image, image scale and wallpaper accent are hidden. The context menu and multi-select work as in the classic layout. Item info keeps the classic look.

## Source Files

| File | Path |
|---|---|
| `ChatView.kt` | `views/chat/ChatView.kt` |
| `ComposeView.kt` | `views/chat/ComposeView.kt` |
| `SendMsgView.kt` | `views/chat/SendMsgView.kt` |
| `ComposeEmojiPicker.kt` | `views/chat/ComposeEmojiPicker.kt` (+ `.android.kt`, `.desktop.kt`) |
| Chat item views | `views/chat/item/*.kt` |
