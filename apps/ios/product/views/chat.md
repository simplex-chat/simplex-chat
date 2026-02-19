# Chat View (Conversation)

> **Related spec:** [spec/client/chat-view.md](../../spec/client/chat-view.md) | [spec/client/compose.md](../../spec/client/compose.md)

## Purpose

Full conversation view for displaying and interacting with messages in a direct contact chat, group chat, or note-to-self. Supports text messaging with markdown, media attachments, voice messages, E2E encrypted calls, message reactions, replies, forwarding, and content search/filtering.

## Route / Navigation

- **Entry point**: Tap a chat row in `ChatListView`
- **Presented by**: `NavStackCompat` destination from `ChatListView`, bound to `chatModel.chatId`
- **Back navigation**: Dismiss sets `chatModel.chatId = nil`, returning to chat list
- **Sub-navigation**: Info button navigates to `ChatInfoView` (contact) or `GroupChatInfoView` (group); member avatars navigate to `GroupMemberInfoView`

## Page Sections

### Navigation Bar

Custom toolbar overlaying the chat with themed material background:

| Element | Description |
|---|---|
| Back button | Returns to chat list |
| Contact/Group avatar | Small profile image |
| Chat name | Display name; tappable to open info sheet |
| Encryption badge | Shows PQ (post-quantum) or standard E2E status |
| Call buttons | Audio and video call icons (direct chats only) |
| Search button | Toggles in-chat message search |
| Info button | Opens `ChatInfoView` or `GroupChatInfoView` |

### Message List

Rendered by `EndlessScrollView<MergedItem>` with lazy loading and pagination:

| Feature | Description |
|---|---|
| Scroll direction | Bottom-to-top (newest messages at bottom) |
| Pagination | Loads more items on scroll to top (`loadingTopItems`) and bottom (`loadingBottomItems`) |
| Merged items | Adjacent messages from the same sender are visually merged via `MergedItems` |
| Floating buttons | Scroll-to-bottom button with unread count; scroll-to-first-unread button |
| Date separators | Sticky date headers between messages from different days |
| Wallpaper | Themed background image with tint and opacity from `theme.wallpaper` |
| Content filter | Filter messages by type: `.images`, `.files`, `.links` |

### Message Types

Each type has a dedicated view in `Shared/Views/Chat/ChatItem/`:

| Type | View | Description |
|---|---|---|
| Text | `MsgContentView` | Rendered with markdown (bold, italic, code, links, mentions) |
| Image | `CIImageView` | Thumbnail with tap-to-fullscreen via `FullScreenMediaView` |
| Video | `CIVideoView` | Video thumbnail with play button; inline playback |
| Voice | `CIVoiceView` / `FramedCIVoiceView` | Waveform visualization with playback controls and duration |
| File | `CIFileView` | File icon, name, size; download/open actions |
| Link preview | `CILinkView` | URL preview card with title, description, image |
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
| Reply | Sets compose bar to reply mode with quoted message |
| Forward | Opens `forwardedChatItems` sheet to pick destination chat |
| Copy | Copies message text to clipboard |
| Edit | Enters edit mode in compose bar (own messages, within edit window) |
| Delete | Delete for self or delete for everyone (with confirmation) |
| React | Opens emoji reaction picker |
| Select multiple | Enters multi-select mode (`selectedChatItems`) with bulk delete/forward |
| Info | Shows delivery status and timestamps |

Emoji reactions bar displayed below messages with reaction counts.

### Compose Bar (`ComposeView`)

| Element | Description |
|---|---|
| Text input | `NativeTextEditor` with markdown support and auto-growing height |
| Attachment button | Opens picker for images, videos, files, camera |
| Send button | Sends composed message; changes to voice record button when empty |
| Voice record | Hold-to-record with waveform preview; swipe-to-cancel |
| Reply quote | Shows quoted message above input when replying |
| Edit indicator | Shows "editing" label when editing a previous message |
| Link preview | Auto-generated preview card for detected URLs (`ComposeLinkView`) |
| Image/Video preview | Thumbnail strip for selected media (`ComposeImageView`) |
| File preview | File name and size for attached file (`ComposeFileView`) |
| Voice preview | Waveform of recorded voice message (`ComposeVoiceView`) |
| Live message | Real-time typing broadcast (optional, with alert on first use) |
| Context actions | `ContextContactRequestActionsView` for accepting/rejecting contact requests; `ContextPendingMemberActionsView` for pending group member actions |
| Commands menu | `CommandsMenuView` for bot/menu commands in chats with `menuCommands` |
| Group mentions | `GroupMentionsView` autocomplete popup when typing `@` in groups |
| Profile picker | `ContextProfilePickerView` for choosing incognito/main profile |

### Member Support Chat (Groups)

For groups with member support enabled:
- `MemberSupportView` and `MemberSupportChatToolbar` shown as secondary chat within group
- `SecondaryChatView` for scoped group chat views (reports, member support)
- User knocking state: `userMemberKnockingTitleBar()` shown when user is pending admission

## Loading / Error States

| State | Behavior |
|---|---|
| Initial load | Messages load from `ItemsModel` with merged items; `allowLoadMoreItems` throttles pagination |
| Loading more (top) | `loadingTopItems` spinner at top of scroll view |
| Loading more (bottom) | `loadingBottomItems` spinner at bottom |
| Connection in progress | `ConnectProgressManager` shows connecting text below compose bar |
| Connecting text | "connecting..." label shown below message list when chat not yet ready |
| Send disabled | Compose bar shows `disabledText` reason when `userCantSendReason` is set |
| Empty chat | No messages placeholder (implicit -- empty scroll view) |

## Related Specs

- `spec/client/chat-view.md` -- Chat view feature specification
- `spec/client/compose.md` -- Compose bar specification
- [Chat List](chat-list.md) -- Parent navigation
- [Contact Info](contact-info.md) -- Info sheet for direct chats
- [Group Info](group-info.md) -- Info sheet for group chats
- [Call](call.md) -- Audio/video calls initiated from toolbar

## Source Files

- `Shared/Views/Chat/ChatView.swift` -- Main chat view, message list, navigation, state management
- `Shared/Views/Chat/ChatItemView.swift` -- Individual message item rendering dispatcher
- `Shared/Views/Chat/ComposeMessage/ComposeView.swift` -- Compose bar container
- `Shared/Views/Chat/ComposeMessage/SendMessageView.swift` -- Send button and voice record
- `Shared/Views/Chat/ComposeMessage/NativeTextEditor.swift` -- Text input with markdown
- `Shared/Views/Chat/ComposeMessage/ComposeImageView.swift` -- Image attachment preview
- `Shared/Views/Chat/ComposeMessage/ComposeFileView.swift` -- File attachment preview
- `Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift` -- Voice recording preview
- `Shared/Views/Chat/ComposeMessage/ComposeLinkView.swift` -- Link preview generation
- `Shared/Views/Chat/ComposeMessage/ContextItemView.swift` -- Reply/edit context display
- `Shared/Views/Chat/ComposeMessage/ContextContactRequestActionsView.swift` -- Contact request accept/reject
- `Shared/Views/Chat/ComposeMessage/ContextPendingMemberActionsView.swift` -- Pending member actions
- `Shared/Views/Chat/ComposeMessage/ContextProfilePickerView.swift` -- Profile picker for incognito
- `Shared/Views/Chat/ChatItem/FramedItemView.swift` -- Framed message bubble rendering
- `Shared/Views/Chat/ChatItem/MsgContentView.swift` -- Text message content with markdown
- `Shared/Views/Chat/ChatItem/CIImageView.swift` -- Image message view
- `Shared/Views/Chat/ChatItem/CIVideoView.swift` -- Video message view
- `Shared/Views/Chat/ChatItem/CIVoiceView.swift` -- Voice message view
- `Shared/Views/Chat/ChatItem/CIFileView.swift` -- File message view
- `Shared/Views/Chat/ChatItem/CILinkView.swift` -- Link preview view
- `Shared/Views/Chat/ChatItem/EmojiItemView.swift` -- Large emoji view
- `Shared/Views/Chat/ChatItem/CICallItemView.swift` -- Call event view
- `Shared/Views/Chat/ChatItem/CIEventView.swift` -- Group/system event view
- `Shared/Views/Chat/ChatItem/CIChatFeatureView.swift` -- Feature change notification
- `Shared/Views/Chat/ChatItem/CIMetaView.swift` -- Timestamp and delivery status
- `Shared/Views/Chat/ChatItem/FullScreenMediaView.swift` -- Fullscreen image/video viewer
- `Shared/Views/Chat/ChatItem/AnimatedImageView.swift` -- Animated GIF rendering
- `Shared/Views/Chat/Group/GroupMentions.swift` -- @mention autocomplete
- `Shared/Views/Chat/Group/MemberSupportView.swift` -- Member support scoped chat
- `Shared/Views/Chat/Group/MemberSupportChatToolbar.swift` -- Support chat toolbar
- `Shared/Views/Chat/Group/SecondaryChatView.swift` -- Secondary scoped chat view
