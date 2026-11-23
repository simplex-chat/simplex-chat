# SimpleX Chat iOS -- Message Composition Module

> Technical specification for the compose bar, attachment types, reply/edit/forward modes, voice recording, and mentions.
>
> Related specs: [Chat View](chat-view.md) | [File Transfer](../services/files.md) | [API Reference](../api.md) | [README](../README.md)
> Related product: [Chat View](../../product/views/chat.md)

**Source:** [`ComposeView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift)

---

## Table of Contents

1. [Overview](#1-overview)
2. [ComposeView](#2-composeview)
3. [ComposeState Machine](#3-composestate-machine)
4. [Attachment Types](#4-attachment-types)
5. [Reply Mode](#5-reply-mode)
6. [Edit Mode](#6-edit-mode)
7. [Forward Mode](#7-forward-mode)
8. [Live Messages](#8-live-messages)
9. [Voice Recording](#9-voice-recording)
10. [Link Previews](#10-link-previews)
11. [Mentions](#11-mentions)

---

## 1. Overview

The compose module handles all message creation, editing, and forwarding. It sits at the bottom of `ChatView` and adapts its UI based on the current compose state.

```
ComposeView
├── Context banner (reply quote / edit indicator / forward indicator)
├── Attachment preview (image / video / file / voice waveform)
├── Text input (NativeTextEditor with markdown support)
├── Action buttons
│   ├── Attachment menu (camera, photo library, file picker)
│   ├── Voice record button (hold or toggle)
│   └── Send button (or live message indicator)
└── Link preview (auto-generated when URL detected)
```

---

## 2. [ComposeView](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L329) (`struct ComposeView: View`)

**File**: `Shared/Views/Chat/ComposeMessage/ComposeView.swift`

### Layout
- Fixed at the bottom of ChatView
- Expands vertically as text input grows (up to a maximum height)
- Context banner appears above the text field when in reply/edit/forward mode
- Attachment preview appears between context banner and text field

### Key Properties
- Reads `ChatModel.shared.draft` / `draftChatId` for persisted drafts
- Manages its own internal compose state
- Coordinates with `ChatView` for scroll-to-bottom behavior on send

### Send Flow
1. User taps send button
2. ComposeView constructs `[ComposedMessage]` from current state
3. Calls `apiSendMessages(type:, id:, scope:, live:, ttl:, composedMessages:)`
4. On success: clears compose state, scrolls to bottom
5. On failure: shows error alert, preserves compose state

### Key Functions

| Function | Line | Description |
|----------|------|-------------|
| [`body`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L371) | L371 | Main view body |
| [`sendMessageView()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L870) | L870 | Builds the send-message UI |
| [`sendMessage(ttl:)`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1286) | L1286 | Entry point: initiates send |
| [`sendMessageAsync()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1295) | L1295 | Async send implementation |
| [`clearState(live:)`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1649) | L1649 | Resets compose state after send |
| [`addMediaContent()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1073) | L1073 | Adds media attachment |
| [`connectCheckLinkPreview()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1046) | L1046 | Checks link preview before connect |
| [`commandsButton()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L931) | L931 | Builds commands menu button |

### Draft Persistence

| Function | Line | Description |
|----------|------|-------------|
| [`saveCurrentDraft()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1663) | L1663 | Saves compose state to `ChatModel.draft` |
| [`clearCurrentDraft()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1669) | L1669 | Clears persisted draft |

- When navigating away from a chat, compose state is saved to `ChatModel.draft` / `ChatModel.draftChatId`
- When returning to the same chat, draft is restored
- Drafts are not persisted across app restarts

---

## 3. [ComposeState](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L45) Machine (`struct ComposeState`)

The compose bar operates as a state machine with these primary states:

```
                    ┌──────────┐
                    │  .empty  │ ← initial / after send
                    └─────┬────┘
                          │ user types / attaches / quotes
                          v
        ┌─────────────────────────────────────┐
        │                                     │
   ┌────▼────┐  ┌──────────────┐  ┌──────────▼───┐
   │  .text  │  │ .mediaPending │  │ .voiceRecording │
   └─────────┘  └──────────────┘  └───────────────┘
        │                │
        │ long-press reply│ tap edit
        v                v
   ┌──────────┐   ┌──────────┐   ┌───────────┐
   │ .replying │   │ .editing │   │ .forwarding│
   └──────────┘   └──────────┘   └───────────┘
```

### Supporting Types

| Type | Line | Description |
|------|------|-------------|
| [`enum ComposePreview`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L11) | L11 | Preview variants (image, voice, file, etc.) |
| [`enum ComposeContextItem`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L20) | L20 | Context item for reply/quote |
| [`enum VoiceMessageRecordingState`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L29) | L29 | Recording state enum |
| [`struct ComposeState`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L45) | L45 | Full compose state struct |
| [`copy()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L98) | L98 | Copy compose state with overrides |
| [`mentionMemberName()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L118) | L118 | Format mention display name |
| [`chatItemPreview()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L266) | L266 | Build preview from chat item |
| [`enum UploadContent`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L287) | L287 | Upload content variants |

### States

| State | Description | UI |
|-------|-------------|-----|
| `.empty` | No input, no attachments | Placeholder text, attachment button |
| `.text` | Text entered, no attachments | Send button visible |
| `.mediaPending` | Media/file selected, optionally with text | Preview visible, send button |
| `.voiceRecording` | Voice recording in progress | Waveform animation, stop/send |
| `.replying` | Replying to a specific message | Quote banner above input |
| `.editing` | Editing a previously sent message | Edit banner, pre-filled text |
| `.forwarding` | Forwarding selected messages | Forward banner, item previews |

### Transitions

| From | Trigger | To |
|------|---------|-----|
| `.empty` | User types text | `.text` |
| `.empty` | User selects media | `.mediaPending` |
| `.empty` | User holds voice button | `.voiceRecording` |
| `.empty` | User long-presses message "Reply" | `.replying` |
| `.empty` | User long-presses message "Edit" | `.editing` |
| `.empty` | User selects "Forward" | `.forwarding` |
| Any | User taps send | `.empty` |
| Any | User taps cancel (X) | `.empty` |

---

## 4. Attachment Types

### [ComposeImageView](../../Shared/Views/Chat/ComposeMessage/ComposeImageView.swift#L12)

**File**: [`ComposeImageView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeImageView.swift) (struct at L12)

Preview of selected image(s) before sending. Shows thumbnail with remove button. Images are compressed to `MAX_IMAGE_SIZE` (255KB) before sending.

### [ComposeFileView](../../Shared/Views/Chat/ComposeMessage/ComposeFileView.swift#L11)

**File**: [`ComposeFileView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeFileView.swift) (struct at L11)

Preview of selected file or video. Shows filename, size, and remove button. Videos show a thumbnail frame.

### [ComposeVoiceView](../../Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift#L26)

**File**: [`ComposeVoiceView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift) (struct at L26)

Voice message recording/playback preview. Shows waveform visualization, duration, and play/delete buttons.

### Attachment Menu Options

| Option | Picker | Max Size | Transfer Method |
|--------|--------|----------|-----------------|
| Camera photo | UIImagePickerController | Compressed to 255KB | Inline in SMP message |
| Photo library | PHPickerViewController | Compressed to 255KB | Inline or XFTP |
| Video | PHPickerViewController | Up to 1GB | XFTP |
| File | UIDocumentPickerViewController | Up to 1GB | XFTP |

---

## 5. Reply Mode

Activated via long-press context menu "Reply" on any message.

### UI
- Quote banner above text input showing original message preview
- X button to cancel reply
- Original message reference stored in compose state

### API
- Reply is sent as part of `ComposedMessage` with `quotedItemId` parameter
- `apiSendMessages(composedMessages: [ComposedMessage(quotedItemId: originalItem.id, ...)])`

---

## 6. Edit Mode

Activated via long-press context menu "Edit" on own sent messages (within the edit window).

### UI
- Edit banner above text input with pencil icon
- Text field pre-filled with original message content
- Send button changes to "Save" / checkmark

### API
- `apiUpdateChatItem(type:, id:, scope:, itemId:, updatedMessage:, live:)`
- Response: `ChatResponse1.chatItemUpdated(user:, chatItem:)`

### Constraints
- Only own sent messages can be edited
- Edit is available within a server-defined time window
- Edited messages show a pencil indicator in `CIMetaView`
- Edit history is visible in `ChatItemInfoView`

---

## 7. Forward Mode

Activated via long-press context menu "Forward" or via multi-select toolbar.

### Flow
1. User selects "Forward" on message(s)
2. `apiPlanForwardChatItems(fromChatType:, fromChatId:, fromScope:, itemIds:)` is called to plan
3. Response: `ChatResponse1.forwardPlan(user:, chatItemIds:, forwardConfirmation:)`
4. User selects destination chat
5. `apiForwardChatItems(toChatType:, toChatId:, toScope:, fromChatType:, fromChatId:, fromScope:, itemIds:, ttl:)` executes the forward
6. Forwarded messages appear with a forwarded indicator

### ForwardConfirmation
The plan response may include a `forwardConfirmation` requiring user confirmation (e.g., forwarding to a less secure chat).

---

## 8. [Live Messages](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L36) (`struct LiveMessage`)

Optional feature where the recipient sees typing in real-time.

### How It Works
- User enables live message mode (lightning icon)
- As user types, `apiSendMessages(live: true)` is called repeatedly
- Each call sends the current text as an update to the same message
- Recipient sees the message being composed in real-time
- Final send marks the message as complete

### Key Functions

| Function | Line | Description |
|----------|------|-------------|
| [`sendLiveMessage()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1102) | L1102 | Initiates a live message |
| [`updateLiveMessage()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1120) | L1120 | Sends incremental live update |
| [`liveMessageToSend()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1139) | L1139 | Determines text diff to send |
| [`truncateToWords()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1144) | L1144 | Truncates text at word boundary |

### API
- Initial: `apiSendMessages(live: true, composedMessages: [...])` -- creates live message
- Updates: `apiUpdateChatItem(live: true)` -- updates content as user types
- Final: `apiUpdateChatItem(live: false)` -- marks as complete

---

## 9. Voice Recording

### Recording Flow
1. User taps (or holds) the microphone button
2. `AVAudioRecorder` starts recording in compressed format
3. Waveform visualization shows real-time audio levels
4. User taps stop (or releases hold) to finish recording
5. Preview with playback shown in compose area
6. User taps send to deliver

### Voice Functions

| Function | Line | Description |
|----------|------|-------------|
| [`startVoiceMessageRecording()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1564) | L1564 | Begins audio recording |
| [`finishVoiceMessageRecording()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1605) | L1605 | Stops recording, shows preview |
| [`allowVoiceMessagesToContact()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1616) | L1616 | Enables voice messages for contact |
| [`updateComposeVMRFinished()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1623) | L1623 | Updates state after recording finishes |
| [`cancelCurrentVoiceRecording()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1635) | L1635 | Cancels in-progress recording |
| [`cancelVoiceMessageRecording()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1642) | L1642 | Cancels and cleans up recording file |

### Constraints
- Maximum duration: `MAX_VOICE_MESSAGE_LENGTH = 300` seconds (5 minutes)
- Auto-receive threshold: `MAX_VOICE_SIZE_AUTO_RCV = 522,240` bytes (510KB)
- Compressed audio format for small file sizes

### Audio Management
- [`AudioRecorder`](../../Shared/Model/AudioRecPlay.swift#L14) (`Shared/Model/AudioRecPlay.swift` L14) manages recording and playback
- `ChatModel.stopPreviousRecPlay` coordinates exclusive audio playback (only one audio source plays at a time)

---

## 10. [Link Previews](../../Shared/Views/Chat/ComposeMessage/ComposeLinkView.swift#L12) (`ComposeLinkView`)

**File**: [`ComposeLinkView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeLinkView.swift) (struct at L12)

### Auto-Detection
- As user types, URLs in the text are detected
- The first URL outside `cancelledLinks` and outside SimpleX links is selected; its card shows a spinner at once, and after 1.5 s `loadLinkPreview` requests the preview from the core with `/_link preview <uri>`; the core loads the page through the network settings
- Preview card shows thumbnail image, title and link

### Link Preview State

The link preview is a part of `ComposeState`: `preview` is `.linkPreview(status:)`, with `LinkPreviewStatus` `.loading(uri:request:)` or `.shown(linkPreview:)`, and `cancelledLinks` holds the links that are not requested again in this message. `updateLinkPreview(_:previewsOn:)` is its only transition. `loadLinkPreview` runs in `.task(id: composeState.loadingRequest)`, so the wait of a request ends at the next request, and the pending result of a replaced state is discarded.

| Event | Sent by | Result |
|---|---|---|
| `.textChanged` | `.onChange(of: composeState.message)`<br>`forwardingItems(_:fromChatInfo:previewsOn:)`, a forward to the same chat | for an empty text, `cancelledLinks` is emptied; the card of the selected link is kept, requested or removed; an attachment is kept |
| `.fetched(request:linkPreview:)` | `loadLinkPreview` | applied only to the loading request; after a failure, the link is added to `cancelledLinks` and the next link is selected |
| `.cancelled` | X of the card, `cancelLinkPreview` | the link is added to `cancelledLinks` and the next link is selected |
| `.chatChanged` | `.onChange(of: chatModel.chatId)`, `saveCurrentDraft` | `cancelledLinks` is emptied and the link is selected again |

An edit opens with `ComposeState(editingItem:previewsOn:)`: the links of the text before the link of the edited card are added to `cancelledLinks`, and the preview is selected as `.textChanged` selects it. The card of the edited message is kept and saved with the message; a card whose link is absent from the text is replaced by the card of the selected link of the text.

### Link Preview Functions

| Function | Line | Description |
|----------|------|-------------|
| [`init(editingItem:previewsOn:)`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L91) | L91 | State of an edit, with the links before the link of its card cancelled |
| [`forwardingItems()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L123) | L123 | State of a forward to the same chat, with the text and its link preview |
| [`updateLinkPreview()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L323) | L323 | Transition of the link preview state |
| [`getMessageLinks()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L356) | L356 | Returns the first URL outside `cancelledLinks` and SimpleX links |
| [`checkLinkPreview()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L371) | L371 | Content of the sent message: `.link` when the shown card is of the selected URL of the text |
| [`cancelLinkPreview()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1975) | L1975 | Cancels the card |
| [`loadLinkPreview()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1980) | L1980 | Requests the preview of the loading link from the core after 1.5 s |
| [`apiGetLinkPreview()`](../../Shared/Model/SimpleXAPI.swift#L527) | L527 | Sends `/_link preview <uri>`; on failure returns no preview |

### Behavior
- Only the first URL in the message generates a preview; when its preview fails or is dismissed, the card of the next URL is shown
- Preview can be dismissed by the user
- Link preview data is included in the `ComposedMessage` sent to the core, from the state read when the send starts
- Toggle in privacy settings to disable auto-preview generation
- A failed preview shows no card and no alert; with **Show developer options** and **Show link preview errors** on, an alert shows the error of the core
- A failed or dismissed link is not requested again in the same message; it is requested again after the text is cleared, the message is sent, the draft is saved or another chat is opened
- A result is applied only while its request is loading; the result of a link replaced or removed from the text, or of a state replaced by an attachment, an edit or a reply, is discarded
- A forward to the same chat keeps the text, `cancelledLinks` and the link preview with its request; an attachment is removed, and the link of the text is selected
- An attachment, voice or chat link preview replaces the link preview and keeps `cancelledLinks`
- The share extension requests the preview of a shared link from the core when the toggle is on, from a global dispatch queue

---

## 11. Mentions

In group chats, typing `@` triggers member name autocomplete:

### Flow
1. User types `@` in the text field
2. Autocomplete dropdown appears with matching group members
3. User selects a member
4. `@displayName` is inserted into the text
5. Mention is rendered with special formatting in the sent message

### Data
- Group members loaded from `ChatModel.groupMembers`
- Mention metadata included in `ComposedMessage`

---

## Channel Compose Behavior

When `chat.chatInfo.groupInfo?.useRelays == true` (channel mode), compose behaves differently:

### Owner/Admin Compose
- [`send()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1498) passes `sendAsGroup: true` to `apiSendMessages` when `useRelays && memberRole >= .owner`
- [`forwardItems()`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L1526) passes `sendAsGroup: true` to `apiForwardChatItems` under same condition
- Placeholder text shows "Broadcast" instead of "Message" (via `sendMessageView()` `placeholder:` parameter)
- Share Extension ([`ShareAPI.swift`](../../SimpleX%20SE/ShareAPI.swift#L71)) uses the same `sendAsGroup` expression

### Subscriber Compose
- [`userCantSendReason`](../../SimpleXChat/ChatTypes.swift#L1566) returns `("you are subscriber", nil)` when `useRelays && memberRole == .observer`
- This check is evaluated after `memberPending` (which takes priority) but replaces the `observer` message
- Compose field is disabled; tapping shows "You can't send messages!" alert with no body text

---

## Source Files

| File | Path | Struct/Class | Line |
|------|------|--------------|------|
| Compose view | [`ComposeView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift) | `ComposeView` | [L329](../../Shared/Views/Chat/ComposeMessage/ComposeView.swift#L329) |
| Send message UI | [`SendMessageView.swift`](../../Shared/Views/Chat/ComposeMessage/SendMessageView.swift) | `SendMessageView` | [L15](../../Shared/Views/Chat/ComposeMessage/SendMessageView.swift#L15) |
| Image preview | [`ComposeImageView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeImageView.swift) | `ComposeImageView` | [L12](../../Shared/Views/Chat/ComposeMessage/ComposeImageView.swift#L12) |
| File preview | [`ComposeFileView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeFileView.swift) | `ComposeFileView` | [L11](../../Shared/Views/Chat/ComposeMessage/ComposeFileView.swift#L11) |
| Voice preview | [`ComposeVoiceView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift) | `ComposeVoiceView` | [L26](../../Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift#L26) |
| Link preview | [`ComposeLinkView.swift`](../../Shared/Views/Chat/ComposeMessage/ComposeLinkView.swift) | `ComposeLinkView` | [L12](../../Shared/Views/Chat/ComposeMessage/ComposeLinkView.swift#L12) |
| Audio recording | [`AudioRecPlay.swift`](../../Shared/Model/AudioRecPlay.swift) | `AudioRecorder` | [L14](../../Shared/Model/AudioRecPlay.swift#L14) |
