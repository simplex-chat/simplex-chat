# Messaging Flow

## Overview

Complete message lifecycle in SimpleX Chat iOS: composing, sending, receiving, editing, deleting, reacting to, replying to, and forwarding messages. All messages are end-to-end encrypted via the SMP protocol. The Haskell core handles encryption, routing, and persistence; the Swift UI layer drives composition and display.

## Prerequisites

- User profile created and chat engine running (`startChat()` completed)
- At least one established contact or group conversation
- `ChatModel.shared` populated with chat list data

## Step-by-Step Processes

### 1. Send Text Message

1. User navigates to a conversation (direct or group) via `ChatListView` -> `ChatView`.
2. User types text into `ComposeView`'s `SendMessageView` text editor.
3. Link previews are detected and fetched asynchronously (`ComposeLinkView`).
4. User taps the send button.
5. `ComposeView` builds a `ComposedMessage`:
   ```swift
   ComposedMessage(
       fileSource: nil,
       quotedItemId: nil,
       msgContent: .text("Hello"),
       mentions: [:]
   )
   ```
6. Calls `apiSendMessages(type:id:scope:live:ttl:composedMessages:)`.
7. Internally dispatches `ChatCommand.apiSendMessages(...)` to the Haskell core.
8. Core encrypts, queues via SMP, and returns `ChatResponse1.newChatItems(user, aChatItems)`.
9. `processSendMessageCmd` extracts `[ChatItem]` from response.
10. For direct chats, a background task tracks delivery via `chatModel.messageDelivery`.
11. `ChatModel` updates, UI refreshes to show the new message.

### 2. Send Media (Image/Video/File)

1. User taps the attachment button in `ComposeView`.
2. **Image**: Picked via `PhotosPicker` or camera. Compressed to <=255KB. Sent inline with `.image(text, base64Image)` content type.
3. **Video**: Picked from library. Thumbnail generated. Video file sent via XFTP for files >255KB. Content type: `.video(text, thumbnail, duration)`.
4. **File**: Picked via document picker. If <=255KB, sent inline. If >255KB, uploaded via XFTP (up to 1GB). Content type: `.file(text)`.
5. `ComposedMessage` includes `fileSource: CryptoFile(filePath:)`.
6. `apiSendMessages(...)` called with the composed message array.
7. Core handles XFTP upload for large files (chunked, encrypted upload to XFTP servers).
8. Recipient receives file reference and can download.

### 3. Receive Message

1. `ChatReceiver.shared` runs `receiveMsgLoop()` continuously calling `chatRecvMsg()`.
2. Core delivers events via `APIResult<ChatEvent>`.
3. On `ChatEvent.newChatItems(user, chatItems)`:
   - `processReceivedMsg` is called.
   - For the active user, `ChatModel` is updated with new items.
   - If the chat is currently open, `ItemsModel` appends to `reversedChatItems`.
   - `NtfManager` posts a local notification if the app is in the background.
4. Small files/images attached to incoming messages are auto-received if within size thresholds.

### 4. Edit Message

1. User long-presses a sent message -> selects "Edit" from context menu.
2. `ComposeView` enters edit mode with the original text pre-filled.
3. User modifies text and taps send.
4. Calls `apiUpdateChatItem(type:id:scope:itemId:updatedMessage:live:)`.
5. Dispatches `ChatCommand.apiUpdateChatItem(...)`.
6. Core returns `ChatResponse1.chatItemUpdated(user, aChatItem)` or `.chatItemNotChanged(user, aChatItem)`.
7. `ChatModel` updates the item in place. Edit timestamp is shown in the UI.

### 5. Delete Message

1. User long-presses a message -> selects "Delete".
2. Presented with options:
   - **Delete for me** (`CIDeleteMode.cidmInternal`) -- removes locally only.
   - **Delete for everyone** (`CIDeleteMode.cidmBroadcast`) -- sends deletion to recipient(s).
3. Calls `apiDeleteChatItems(type:id:scope:itemIds:mode:)`.
4. Dispatches `ChatCommand.apiDeleteChatItem(type:id:scope:itemIds:mode:)`.
5. Core returns `ChatResponse1.chatItemsDeleted(user, items, _)` containing `[ChatItemDeletion]`.
6. For group messages from other members, admin/owner can call `apiDeleteMemberChatItems(groupId:itemIds:)`.
7. `ChatModel` removes or replaces items with "deleted" placeholders.

### 6. React to Message

1. User long-presses a message -> selects "React" -> picks an emoji.
2. Calls `apiChatItemReaction(type:id:scope:itemId:add:reaction:)`.
3. `reaction` is `MsgReaction` (e.g., `.emoji(.heart)`).
4. `add: true` to add, `add: false` to remove.
5. Core returns `ChatResponse1.chatItemReaction(user, _, reaction)`.
6. The reaction is displayed below the message bubble.

### 7. Reply to Message

1. User long-presses a message -> selects "Reply".
2. `ComposeView` enters reply mode, showing quoted message in `ContextItemView`.
3. User types reply text and taps send.
4. `ComposedMessage` is created with `quotedItemId: originalItem.id`.
5. `apiSendMessages(...)` sends with the quote reference.
6. Recipient sees the reply with the quoted context rendered above.

### 8. Forward Message

1. User long-presses a message -> selects "Forward".
2. `ChatItemForwardingView` is presented for destination chat selection.
3. `apiPlanForwardChatItems(type:id:scope:itemIds:)` validates what can be forwarded, returns `([Int64], ForwardConfirmation?)`.
4. User confirms and selects destination chat.
5. Calls `apiForwardChatItems(toChatType:toChatId:toScope:fromChatType:fromChatId:fromScope:itemIds:ttl:)`.
6. Core returns `ChatResponse1.newChatItems(...)` with the forwarded items in the destination chat.

### 9. Voice Message

1. User taps and holds the microphone button in `ComposeView`.
2. `AudioRecPlay` starts recording to a temporary file.
3. On release, recording stops. Duration is calculated (max 5 minutes / 300 seconds).
4. `ComposedMessage` created with:
   - `fileSource: CryptoFile` pointing to the audio file
   - `msgContent: .voice(text: "", duration: seconds)`
5. `apiSendMessages(...)` sends the voice message.
6. Voice messages <=510KB sent inline; larger via XFTP.
7. Recipient sees `CIVoiceView` with waveform and playback controls.

### 10. Delivery Tracking

1. On send, message status starts as `CIStatus.sndNew`.
2. After SMP delivery: `CIStatus.sndSent(sndProgress)`.
3. When delivered to recipient's agent: status updates to delivered.
4. If delivery receipts are enabled by both parties, read status is reported.
5. Failed delivery results in `CIStatus.sndError*` or `CIStatus.sndWarning*`.
6. Status is displayed via `CIMetaView` (checkmarks/indicators).

## Data Structures

| Type | Location | Description |
|------|----------|-------------|
| `ComposedMessage` | `SimpleXChat/APITypes.swift` | Outgoing message: fileSource, quotedItemId, msgContent, mentions |
| `MsgContent` | `SimpleXChat/ChatTypes.swift` | Enum: `.text`, `.link`, `.image`, `.video`, `.voice`, `.file` |
| `CIContent` | `SimpleXChat/ChatTypes.swift` | Chat item content wrapper with sent/received variants |
| `CIStatus` | `SimpleXChat/ChatTypes.swift` | Delivery status: sndNew, sndSent, sndError, rcvNew, rcvRead |
| `CIDirection` | `SimpleXChat/ChatTypes.swift` | `.directSnd`, `.directRcv`, `.groupSnd`, `.groupRcv(groupMember)` |
| `ChatItem` | `SimpleXChat/ChatTypes.swift` | Full message model: content, meta, status, direction, quotedItem |
| `ChatItemDeletion` | `SimpleXChat/ChatTypes.swift` | Deleted item info with old/new item pairs |
| `CIDeleteMode` | `SimpleXChat/ChatTypes.swift` | `.cidmInternal` (local) or `.cidmBroadcast` (for everyone) |
| `MsgReaction` | `SimpleXChat/ChatTypes.swift` | Reaction type (emoji-based) |
| `UpdatedMessage` | `SimpleXChat/APITypes.swift` | Edited message content for update API |

## Error Cases

| Error | Cause | Handling |
|-------|-------|----------|
| `ChatError.errorAgent(.SMP(_, .AUTH))` | Recipient queue issue | Show "Connection error (AUTH)" alert |
| `ChatError.errorAgent(.BROKER(_, .TIMEOUT))` | Server timeout | Retryable: show retry dialog via `chatApiSendCmdWithRetry` |
| `ChatError.errorAgent(.BROKER(_, .NETWORK))` | Network failure | Retryable: show retry dialog |
| Send message error | Core processing failure | `sendMessageErrorAlert` shown to user |
| `chatItemNotChanged` | Edit with identical content | No error, item returned unchanged |
| File too large (>1GB) | XFTP limit exceeded | Prevented in UI file picker |
| `fileNotApproved` | Unknown XFTP relay servers | Show "Unknown servers!" alert with approve option |

## Key Files

| File | Purpose |
|------|---------|
| `Shared/Views/Chat/ComposeMessage/ComposeView.swift` | Message composition UI and send logic |
| `Shared/Views/Chat/ComposeMessage/SendMessageView.swift` | Text input and send button |
| `Shared/Views/Chat/ComposeMessage/ContextItemView.swift` | Reply/edit context display |
| `Shared/Views/Chat/ChatItemView.swift` | Per-message rendering dispatcher |
| `Shared/Views/Chat/ChatItem/MsgContentView.swift` | Text message content with markdown |
| `Shared/Views/Chat/ChatItem/CIMetaView.swift` | Delivery status indicators |
| `Shared/Views/Chat/ChatItemForwardingView.swift` | Forward destination picker |
| `Shared/Views/Chat/ChatItemInfoView.swift` | Message info (delivery details, timestamps) |
| `Shared/Model/SimpleXAPI.swift` | API functions: `apiSendMessages`, `apiUpdateChatItem`, `apiDeleteChatItems`, `apiChatItemReaction`, `apiForwardChatItems` |
| `SimpleXChat/APITypes.swift` | `ComposedMessage`, `ChatCommand` enum, response types |
| `SimpleXChat/ChatTypes.swift` | `MsgContent`, `CIContent`, `CIStatus`, `CIDirection`, `ChatItem` |
| `Shared/Model/AudioRecPlay.swift` | Voice message recording/playback engine |

## Related Specifications

- `apps/ios/product/views/chat.md` -- Chat view UI specification
- `apps/ios/product/README.md` -- Product overview and capability map
