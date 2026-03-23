# Messaging Flow

> **Related spec:** [spec/client/compose.md](../../spec/client/compose.md) | [spec/api.md](../../spec/api.md)

## Overview

Messaging is the core interaction in SimpleX Chat. Users compose and send text, images, video, voice notes, files, and link previews. Messages can be replied to, edited, deleted, forwarded, and reacted to with emoji. Special modes include timed (disappearing) messages, live messages (real-time typing), and message reports for moderation.

All message operations flow through the Haskell core via `ChatController.apiSendMessages`, with responses updating `ChatModel` and triggering Compose UI recomposition.

## Prerequisites

- Chat is initialized and running (`ChatModel.chatRunning == true`).
- An active user exists (`ChatModel.currentUser != null`).
- A chat is open (`ChatModel.chatId != null`) with an established connection.

---

## 1. Sending a Text Message

### 1.1 Compose and Send

1. User types in the compose field. `ComposeState.message` is updated as a `ComposeMessage(text, selection)`.
2. The compose area tracks context via `ComposeContextItem`: `NoContextItem` for a fresh message, `QuotedItem` for a reply, `EditingItem` for an edit, `ForwardingItems` for forwarding, or `ReportedItem` for a report.
3. User taps the send button. The `ComposeView` builds a `ComposedMessage`:
   ```kotlin
   class ComposedMessage(
     val fileSource: CryptoFile?,
     val quotedItemId: Long?,
     val msgContent: MsgContent,
     val mentions: Map<String, Long>
   )
   ```
4. For plain text, `msgContent` is `MsgContent.MCText(text)`.
5. `ChatController.apiSendMessages(rh, type, id, scope, live, ttl, composedMessages)` is called.
6. The core command `CC.ApiSendMessages` is dispatched via `sendCmd`.
7. On success, the response `CR.NewChatItems` returns a list of `AChatItem`.
8. `ChatModel` is updated and the chat item list recomposes to show the new message.
9. `ComposeState` is reset to its default.

### 1.2 Link Preview

1. As the user types, the text is parsed for URLs.
2. If `privacyLinkPreviews` preference is enabled and a URL is detected, a `LinkPreview` is fetched asynchronously.
3. The compose preview is set to `ComposePreview.CLinkPreview(linkPreview)`.
4. When sent, the `msgContent` is `MsgContent.MCLink(text, preview)`.

---

## 2. Sending Media (Image, Video, Voice)

### 2.1 Image

1. User picks or captures an image.
2. The image is resized (max inline data size `MAX_IMAGE_SIZE` = 255 KB for the preview thumbnail).
3. The full-size file is saved to the app files directory.
4. If local file encryption is enabled (`privacyEncryptLocalFiles`), the file is encrypted via `encryptCryptoFile`, producing a `CryptoFile` with `CryptoFileArgs(fileKey, fileNonce)`.
5. Compose preview becomes `ComposePreview.MediaPreview(images, content)`.
6. On send, `msgContent` is `MsgContent.MCImage(text, imageBase64)` and `fileSource` is the `CryptoFile`.
7. The core handles inline delivery (for small files) or XFTP upload (for larger files).

### 2.2 Video

1. User picks or records a video.
2. A thumbnail image is extracted and resized.
3. The video file is saved and optionally encrypted.
4. On send, `msgContent` is `MsgContent.MCVideo(text, image, duration)`.

### 2.3 Voice Message

1. User records a voice note. Recording state is tracked via `RecordingState` (NotStarted, Started, Finished).
2. The compose preview becomes `ComposePreview.VoicePreview(voice, durationMs, finished)`.
3. On send, `msgContent` is `MsgContent.MCVoice(text, durationSeconds)`.
4. A file attachment carries the actual audio data.

---

## 3. Sending Files

1. User picks a file via the file chooser.
2. File size is validated against `MAX_FILE_SIZE_XFTP` (1 GB).
3. Compose preview becomes `ComposePreview.FilePreview(fileName, uri)`.
4. On send, `msgContent` is `MsgContent.MCFile(text)` and the `fileSource` is populated.
5. Delivery via inline (small files under SMP threshold) or XFTP (large files) is determined by the core.

---

## 4. Receiving Messages

1. The `ChatController` receiver loop calls `chatRecvMsgWait` on the Haskell core.
2. Incoming messages arrive as `CR.NewChatItems` events.
3. `ChatModel` chat items list is updated, triggering recomposition.
4. For media messages, images below `MAX_IMAGE_SIZE_AUTO_RCV` (510 KB), videos below `MAX_VIDEO_SIZE_AUTO_RCV` (1023 KB), and voice notes below `MAX_VOICE_SIZE_AUTO_RCV` (510 KB) are auto-received if `privacyAcceptImages` is enabled.
5. Larger files require manual download initiation (see File Transfer Flow).

---

## 5. Editing a Message

1. User long-presses a sent message and selects "Edit".
2. `ComposeContextItem` becomes `EditingItem(chatItem)`.
3. The original text populates the compose field.
4. On send, `ChatController.apiUpdateChatItem(rh, type, id, scope, itemId, updatedMessage, live)` is called.
5. `updatedMessage` is an `UpdatedMessage(msgContent, mentions)`.
6. The core responds with `CR.ChatItemUpdated` or `CR.ChatItemNotChanged`.
7. The chat item in `ChatModel` is updated in place.

---

## 6. Deleting a Message

1. User long-presses a message and selects "Delete".
2. A delete mode is chosen: `CIDeleteMode.cidmBroadcast` (delete for everyone), `CIDeleteMode.cidmInternal` (delete for self), or `CIDeleteMode.cidmInternalMark` (mark as deleted internally).
3. `ChatController.apiDeleteChatItems(rh, type, id, scope, itemIds, mode)` is called.
4. The core responds with `CR.ChatItemsDeleted`, returning a list of `ChatItemDeletion`.
5. For group chats by moderators, `apiDeleteMemberChatItems(rh, groupId, itemIds)` is used.
6. Deleted items are either removed from the UI or replaced with a "deleted" marker.

---

## 7. Reacting to a Message

1. User long-presses a message and selects an emoji reaction.
2. `ChatController.apiChatItemReaction(rh, type, id, scope, itemId, add, reaction)` is called.
3. `reaction` is a `MsgReaction` (typically emoji).
4. `add = true` to add, `add = false` to remove a reaction.
5. The core responds with `CR.ChatItemReaction`, and the chat item's reaction list is updated.
6. In groups, `apiGetReactionMembers` can be called to see who reacted.

---

## 8. Replying to a Message

1. User swipes or long-presses a message and selects "Reply".
2. `ComposeContextItem` becomes `QuotedItem(chatItem)`.
3. The quoted item preview is shown above the compose field.
4. On send, the `ComposedMessage.quotedItemId` is set to the quoted item's ID.
5. The sent message renders with the quoted content inline.

---

## 9. Forwarding Messages

1. User selects one or more messages and taps "Forward".
2. `ChatController.apiPlanForwardChatItems(rh, fromChatType, fromChatId, fromScope, chatItemIds)` is called first to get a `CR.ForwardPlan` with forwardable/non-forwardable item categorization.
3. `ComposeContextItem` becomes `ForwardingItems(chatItems, fromChatInfo)`.
4. User picks a destination chat.
5. `ChatController.apiForwardChatItems(rh, toChatType, toChatId, toScope, fromChatType, fromChatId, fromScope, itemIds, ttl)` is called.
6. New chat items are created in the destination chat.

---

## 10. Timed (Disappearing) Messages

1. Timed messages are enabled per-chat via chat feature preferences.
2. When composing, a TTL (time-to-live) in seconds is passed as the `ttl` parameter to `apiSendMessages`.
3. The core attaches the TTL to the message metadata.
4. After the TTL expires, the message is automatically deleted on both sides.
5. The UI shows a countdown indicator on timed messages via `CIMetaView`.

---

## 11. Live Messages

1. User enables live message mode (long-press on send button if `liveMessageAlertShown` preference allows).
2. `ComposeState.liveMessage` is set to a `LiveMessage(chatItem, typedMsg, sentMsg, sent)`.
3. As the user types, `apiSendMessages` is called with `live = true` for the initial send, then `apiUpdateChatItem` with `live = true` for subsequent updates.
4. The recipient sees the message content updating in real-time.
5. When the user finalizes (taps send), a final `apiUpdateChatItem` with `live = false` is sent.

---

## 12. Message Reports

1. User long-presses a message and selects "Report".
2. `ComposeContextItem` becomes `ReportedItem(chatItem, reason)` where `reason` is a `ReportReason`.
3. On send, `msgContent` is `MsgContent.MCReport(text, reason)`.
4. The report is sent to group owners/admins for moderation review.
5. Group admins see reports in the `GroupReportsView`.

---

## Key Types Reference

| Type | Location | Purpose |
|------|----------|---------|
| `ComposeState` | `views/chat/ComposeView.kt` | Tracks compose field state |
| `ComposePreview` | `views/chat/ComposeView.kt` | Preview type: NoPreview, CLinkPreview, MediaPreview, VoicePreview, FilePreview |
| `ComposeContextItem` | `views/chat/ComposeView.kt` | Context: NoContextItem, QuotedItem, EditingItem, ForwardingItems, ReportedItem |
| `ComposedMessage` | `model/SimpleXAPI.kt` | Wire format for sending: fileSource, quotedItemId, msgContent, mentions |
| `UpdatedMessage` | `model/SimpleXAPI.kt` | Wire format for editing: msgContent, mentions |
| `MsgContent` | `model/ChatModel.kt` | Sealed class: MCText, MCLink, MCImage, MCVideo, MCVoice, MCFile, MCReport, MCChat, MCUnknown |
| `LiveMessage` | `views/chat/ComposeView.kt` | Tracks live message state |
| `MsgReaction` | `model/ChatModel.kt` | Emoji reaction type |
| `ChatItemDeletion` | `model/ChatModel.kt` | Deletion result with old/new item |
