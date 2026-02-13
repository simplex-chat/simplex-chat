# SimpleX Chat iOS -- Message Composition Module

> Technical specification for the compose bar, attachment types, reply/edit/forward modes, voice recording, and mentions.
>
> Related specs: [Chat View](chat-view.md) | [File Transfer](../services/files.md) | [API Reference](../api.md) | [README](../README.md)
> Related product: [Chat View](../../product/views/chat.md)

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

## 2. ComposeView

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

### Draft Persistence
- When navigating away from a chat, compose state is saved to `ChatModel.draft` / `ChatModel.draftChatId`
- When returning to the same chat, draft is restored
- Drafts are not persisted across app restarts

---

## 3. ComposeState Machine

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

### ComposeImageView

**File**: `Shared/Views/Chat/ComposeMessage/ComposeImageView.swift`

Preview of selected image(s) before sending. Shows thumbnail with remove button. Images are compressed to `MAX_IMAGE_SIZE` (255KB) before sending.

### ComposeFileView

**File**: `Shared/Views/Chat/ComposeMessage/ComposeFileView.swift`

Preview of selected file or video. Shows filename, size, and remove button. Videos show a thumbnail frame.

### ComposeVoiceView

**File**: `Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift`

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

## 8. Live Messages

Optional feature where the recipient sees typing in real-time.

### How It Works
- User enables live message mode (lightning icon)
- As user types, `apiSendMessages(live: true)` is called repeatedly
- Each call sends the current text as an update to the same message
- Recipient sees the message being composed in real-time
- Final send marks the message as complete

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

### Constraints
- Maximum duration: `MAX_VOICE_MESSAGE_LENGTH = 300` seconds (5 minutes)
- Auto-receive threshold: `MAX_VOICE_SIZE_AUTO_RCV = 522,240` bytes (510KB)
- Compressed audio format for small file sizes

### Audio Management
- `AudioRecPlay` (`Shared/Model/AudioRecPlay.swift`) manages recording and playback
- `ChatModel.stopPreviousRecPlay` coordinates exclusive audio playback (only one audio source plays at a time)

---

## 10. Link Previews

**File**: `Shared/Views/Chat/ComposeMessage/ComposeLinkView.swift`

### Auto-Detection
- As user types, URLs in the text are detected
- When a URL is found, `ComposeLinkView` fetches OpenGraph metadata
- Preview card shows title, description, and thumbnail image

### Behavior
- Only the first URL in the message generates a preview
- Preview can be dismissed by the user
- Link preview data is included in the `ComposedMessage` sent to the core
- Toggle in privacy settings to disable auto-preview generation

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

## Source Files

| File | Path |
|------|------|
| Compose view | `Shared/Views/Chat/ComposeMessage/ComposeView.swift` |
| Image preview | `Shared/Views/Chat/ComposeMessage/ComposeImageView.swift` |
| File preview | `Shared/Views/Chat/ComposeMessage/ComposeFileView.swift` |
| Voice preview | `Shared/Views/Chat/ComposeMessage/ComposeVoiceView.swift` |
| Link preview | `Shared/Views/Chat/ComposeMessage/ComposeLinkView.swift` |
| Audio recording | `Shared/Model/AudioRecPlay.swift` |
