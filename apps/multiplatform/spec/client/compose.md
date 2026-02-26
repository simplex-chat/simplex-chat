# Message Composition Specification

Source: `common/src/commonMain/kotlin/chat/simplex/common/views/chat/ComposeView.kt`, `SendMsgView.kt`

---

## Table of Contents

1. [Overview](#1-overview)
2. [ComposeState Data Class](#2-composestate-data-class)
3. [ComposePreview Sealed Class](#3-composepreview-sealed-class)
4. [ComposeContextItem Sealed Class](#4-composecontextitem-sealed-class)
5. [SendMsgView](#5-sendmsgview)
6. [Attachment Handling](#6-attachment-handling)
7. [Draft Persistence](#7-draft-persistence)
8. [Source Files](#8-source-files)

---

## Executive Summary

Message composition in SimpleX Chat is managed by `ComposeView` (line ~345 in `ComposeView.kt`) backed by the serializable `ComposeState` data class. The compose area supports text input, link previews, media/file/voice attachments, reply/edit/forward contexts, live (streaming) messages, member @mentions, message reports, and timed (disappearing) messages. The `SendMsgView` composable (in `SendMsgView.kt`) provides the text field and action buttons. Draft state persists across chat switches when the privacy preference is enabled.

---

<a id="ComposeView"></a>

## 1. Overview

```
ComposeView
|-- contextItemView()
|   |-- ContextItemView (QuotedItem)       [reply indicator]
|   |-- ContextItemView (EditingItem)      [edit indicator]
|   |-- ContextItemView (ForwardingItems)  [forward indicator]
|   +-- ContextItemView (ReportedItem)     [report indicator]
|-- ReportReasonView                       [report reason header]
|-- MsgNotAllowedView                      [disabled send reason]
|-- previewView()
|   |-- ComposeLinkView                    [link preview card]
|   |-- ComposeImageView                   [media thumbnails]
|   |-- ComposeVoiceView                   [voice recording waveform]
|   +-- ComposeFileView                    [file name display]
|-- AttachmentAndCommandsButtons
|   |-- CommandsButton                     [bot commands "//"]
|   +-- AttachmentButton                   [paperclip icon]
+-- SendMsgView
    |-- PlatformTextField                  [multiline text input]
    |-- DeleteTextButton                   [clear text, shown on long text]
    |-- SendMsgButton                      [arrow/check icon]
    |-- RecordVoiceView                    [microphone + hold-to-record]
    |-- StartLiveMessageButton             [bolt icon]
    |-- CancelLiveMessageButton            [cancel live]
    +-- TimedMessageDropdown               [disappearing message timer]
```

---

<a id="ComposeState"></a>

## 2. ComposeState Data Class

**Location:** [`ComposeView.kt#L98`](ComposeView.kt#L98)

```kotlin
@Serializable
data class ComposeState(
  val message: ComposeMessage = ComposeMessage(),
  val parsedMessage: List<FormattedText> = emptyList(),
  val liveMessage: LiveMessage? = null,
  val preview: ComposePreview = ComposePreview.NoPreview,
  val contextItem: ComposeContextItem = ComposeContextItem.NoContextItem,
  val inProgress: Boolean = false,
  val progressByTimeout: Boolean = false,
  val useLinkPreviews: Boolean,
  val mentions: MentionedMembers = emptyMap()
)
```

### Fields

| Field | Type | Description |
|---|---|---|
| `message` | `ComposeMessage` | Current text and cursor selection (`TextRange`) |
| `parsedMessage` | `List<FormattedText>` | Markdown-parsed representation of message text |
| `liveMessage` | `LiveMessage?` | Active live (streaming) message state |
| `preview` | `ComposePreview` | Attachment preview (link, media, voice, file) |
| `contextItem` | `ComposeContextItem` | Reply/edit/forward/report context |
| `inProgress` | `Boolean` | Send operation in flight |
| `progressByTimeout` | `Boolean` | Show spinner after 1-second send delay |
| `useLinkPreviews` | `Boolean` | Link preview feature flag |
| `mentions` | `MentionedMembers` | Map of mention display name to `CIMention` |

### Computed Properties

| Property | Type | Description |
|---|---|---|
| `editing` | `Boolean` | True when `contextItem` is `EditingItem` |
| `forwarding` | `Boolean` | True when `contextItem` is `ForwardingItems` |
| `reporting` | `Boolean` | True when `contextItem` is `ReportedItem` |
| `sendEnabled` | `() -> Boolean` | True when there is content to send and not in progress |
| `linkPreviewAllowed` | `Boolean` | True when no media/voice/file preview is active |
| `linkPreview` | `LinkPreview?` | Extracts link preview from `CLinkPreview` |
| `attachmentDisabled` | `Boolean` | True when editing, forwarding, live, in-progress, or reporting |
| `attachmentPreview` | `Boolean` | True when a file or media preview is showing |
| `empty` | `Boolean` | True when no text, no preview, and no context item |
| `whitespaceOnly` | `Boolean` | True when message text contains only whitespace |
| `placeholder` | `String` | Input placeholder text (report reason text or default) |
| `memberMentions` | `Map<String, Long>` | Extracted member ID map for API calls |

### ComposeMessage

```kotlin
@Serializable
data class ComposeMessage(
  val text: String = "",
  val selection: TextRange = TextRange.Zero
)
```

### LiveMessage

```kotlin
@Serializable
data class LiveMessage(
  val chatItem: ChatItem,
  val typedMsg: String,
  val sentMsg: String,
  val sent: Boolean
)
```

Tracks a live (streaming) message: the associated `ChatItem`, the currently typed text, the last sent text, and whether the initial send has occurred.

### Serialization

`ComposeState` is fully `@Serializable` with a custom `Saver` (line ~214) that uses `json.encodeToString`/`decodeFromString` for `rememberSaveable` persistence across configuration changes.

---

<a id="ComposePreview"></a>

## 3. ComposePreview Sealed Class

**Location:** [`ComposeView.kt#L52`](ComposeView.kt#L52)

```kotlin
sealed class ComposePreview {
  object NoPreview : ComposePreview()
  class CLinkPreview(val linkPreview: LinkPreview?) : ComposePreview()
  class MediaPreview(val images: List<String>, val content: List<UploadContent>) : ComposePreview()
  data class VoicePreview(val voice: String, val durationMs: Int, val finished: Boolean) : ComposePreview()
  class FilePreview(val fileName: String, val uri: URI) : ComposePreview()
}
```

| Variant | Fields | View |
|---|---|---|
| `NoPreview` | -- | Nothing shown |
| `CLinkPreview` | `linkPreview: LinkPreview?` (null = loading) | `ComposeLinkView`: title, description, image thumbnail, cancel button |
| `MediaPreview` | `images: List<String>` (base64 thumbnails), `content: List<UploadContent>` | `ComposeImageView`: horizontal thumbnail strip, cancel button |
| `VoicePreview` | `voice: String` (file path), `durationMs: Int`, `finished: Boolean` | `ComposeVoiceView`: waveform visualization, duration, play/pause |
| `FilePreview` | `fileName: String`, `uri: URI` | `ComposeFileView`: file icon, file name, cancel button |

### UploadContent

Used within `MediaPreview` to track the source type:

- `SimpleImage(uri: URI)` -- still image
- `AnimatedImage(uri: URI)` -- GIF or animated WebP
- `Video(uri: URI, duration: Int)` -- video with duration in seconds

---

## 4. ComposeContextItem Sealed Class

**Location:** [`ComposeView.kt#L61`](ComposeView.kt#L61)

```kotlin
sealed class ComposeContextItem {
  object NoContextItem : ComposeContextItem()
  class QuotedItem(val chatItem: ChatItem) : ComposeContextItem()
  class EditingItem(val chatItem: ChatItem) : ComposeContextItem()
  class ForwardingItems(val chatItems: List<ChatItem>, val fromChatInfo: ChatInfo) : ComposeContextItem()
  class ReportedItem(val chatItem: ChatItem, val reason: ReportReason) : ComposeContextItem()
}
```

| Variant | Trigger | Compose Behavior |
|---|---|---|
| `NoContextItem` | Default state | Normal message composition |
| `QuotedItem` | Swipe-to-reply or reply menu action | Shows quoted message indicator; sends with `quoted` parameter |
| `EditingItem` | Edit menu action | Populates text field with existing message; send button becomes checkmark; calls `apiUpdateChatItem` |
| `ForwardingItems` | Forward action from another chat | Shows forwarded items indicator; calls `apiForwardChatItems`; can include optional text message |
| `ReportedItem` | Report menu action | Shows report indicator with reason; placeholder changes to reason text; calls `apiReportMessage` |

### Context Item View

`contextItemView()` (line ~1098 in `ComposeView.kt`) renders the active context as a dismissible bar above the text input:

- Icon: reply (ic_reply), edit (ic_edit_filled), forward (ic_forward), report (ic_flag)
- Content: quoted message preview text with sender name
- Close button: resets `contextItem` to `NoContextItem` (or `clearState()` for editing)

---

<a id="SendMsgView"></a>

## 5. SendMsgView

**Location:** [`SendMsgView.kt#L36`](SendMsgView.kt#L36)

```kotlin
fun SendMsgView(
  composeState: MutableState<ComposeState>,
  showVoiceRecordIcon: Boolean,
  recState: MutableState<RecordingState>,
  isDirectChat: Boolean,
  liveMessageAlertShown: SharedPreference<Boolean>,
  sendMsgEnabled: Boolean,
  userCantSendReason: Pair<String, String?>?,
  sendButtonEnabled: Boolean,
  sendToConnect: (() -> Unit)?,
  hideSendButton: Boolean,
  nextConnect: Boolean,
  needToAllowVoiceToContact: Boolean,
  allowedVoiceByPrefs: Boolean,
  sendButtonColor: Color,
  allowVoiceToContact: () -> Unit,
  timedMessageAllowed: Boolean,
  customDisappearingMessageTimePref: SharedPreference<Int>?,
  placeholder: String,
  sendMessage: (Int?) -> Unit,
  sendLiveMessage: (suspend () -> Unit)?,
  updateLiveMessage: (suspend () -> Unit)?,
  cancelLiveMessage: (() -> Unit)?,
  editPrevMessage: () -> Unit,
  onFilesPasted: (List<URI>) -> Unit,
  onMessageChange: (ComposeMessage) -> Unit,
  textStyle: MutableState<TextStyle>,
  focusRequester: FocusRequester?
)
```

### Layout

The view is a `Box` containing:

1. **PlatformTextField:** Multiline text input (platform-specific `expect`). Handles text changes via `onMessageChange`, up-arrow to `editPrevMessage`, file paste via `onFilesPasted`, and Enter to send.
2. **DeleteTextButton:** Shown when text is long; clears the field.
3. **Action area** (bottom-right, stacked):
   - **Progress indicator:** Shown when `progressByTimeout` is true.
   - **Report confirm button:** Checkmark icon when context is `ReportedItem`.
   - **Voice record button:** Shown when message is empty, not editing/forwarding, no preview active.
     - `RecordVoiceView`: Hold-to-record with waveform display.
     - `DisallowedVoiceButton`: Shown when voice is disabled by preferences.
     - `VoiceButtonWithoutPermissionByPlatform`: Shown when microphone permission is not granted.
   - **Live message button:** Bolt icon, starts streaming message (calls `sendLiveMessage`).
   - **Send button:** Arrow icon (new message) or checkmark (editing/live). Long-press opens dropdown:
     - "Send live message" option
     - Timed message options (1min, 5min, 1hr, 8hr, 1day, 1week, 1month, custom)

### RecordingState

```kotlin
sealed class RecordingState {
  object NotStarted : RecordingState()
  class Started(val filePath: String, val progressMs: Int) : RecordingState()
  class Finished(val filePath: String, val durationMs: Int) : RecordingState()
}
```

Voice recording of 300ms or less is auto-cancelled.

### Disabled State

When `sendMsgEnabled` is false (e.g., contact not ready, group permissions), an overlay covers the text field. If `userCantSendReason` is provided, tapping the overlay shows an alert explaining why sending is disabled.

---

## 6. Attachment Handling

<a id="AttachmentSelection"></a>

### Attachment Selection

The `AttachmentSelection` composable (line ~263 in `ComposeView.kt`) is an `expect` function with platform-specific implementations:

**Android:**
- Camera launcher (image capture)
- Gallery launcher (image/video picker, multi-select)
- File picker (any file type)

**Desktop:**
- File chooser dialog (filters for images or all files)

### ChooseAttachmentView

Bottom sheet (`ModalBottomSheetLayout`) presenting attachment type options:

| Option | Result |
|---|---|
| Camera (Android) | Launches camera intent; result processed as `SimpleImage` |
| Gallery | Launches media picker; results processed via `processPickedMedia` |
| File | Launches file picker; result processed via `processPickedFile` |

### File Processing

**`processPickedFile`** (line ~281):
1. Checks file size against `maxFileSize` (XFTP limit).
2. Extracts file name from URI.
3. Sets `ComposePreview.FilePreview` on compose state.

**`processPickedMedia`** (line ~300):
1. For each URI, determines type (image, animated image, video).
2. Images: Gets bitmap, creates `SimpleImage` or `AnimatedImage` upload content.
3. Videos: Extracts thumbnail and duration, creates `Video` upload content.
4. Generates base64 preview thumbnails (max 14KB).
5. Sets `ComposePreview.MediaPreview` with thumbnails and content list.

**`onFilesAttached`** (line ~270):
Groups dropped/pasted files into images and non-images; routes to `processPickedMedia` or `processPickedFile`.

### Send Flow

On send (line ~603, `sendMessageAsync`):

1. **Forwarding:** Calls `apiForwardChatItems`, then optionally sends a text message quoting the last forwarded item.
2. **Editing:** Calls `apiUpdateChatItem` with updated `MsgContent`.
3. **Reporting:** Calls `apiReportMessage` with reason and text.
4. **New message:** Iterates over `msgs` (one per media item or single for text/file/voice):
   - Saves file to app storage (or remote host).
   - For voice: encrypts if `privacyEncryptLocalFiles` is enabled.
   - Calls `apiSendMessages` or `apiCreateChatItems` (local notes).
5. On failure of the last message, restores compose state for retry.

### Link Preview

When `privacyLinkPreviews` is enabled and the message contains a URL:

1. `showLinkPreview` extracts first non-SimpleX, non-cancelled link from parsed markdown.
2. Sets `ComposePreview.CLinkPreview(null)` (loading state).
3. After 1.5s debounce, calls `getLinkPreview(url)`.
4. On success, updates to `CLinkPreview(linkPreview)`.
5. Cancel button adds the URL to `cancelledLinks` set.

---

## 7. Draft Persistence

**Location:** [`ComposeView.kt#L1230`](ComposeView.kt#L1230) (`KeyChangeEffect(chatModel.chatId.value)`)

Controlled by the `privacySaveLastDraft` preference.

### Save Behavior

When the user navigates away from a chat (`chatModel.chatId.value` changes):

| Compose State | Action |
|---|---|
| Live message active (text present or already sent) | Sends the live message immediately, clears draft |
| In progress | Clears in-progress flag, clears previous draft |
| Non-empty (text, preview, or context) | If `saveLastDraft` is true: saves `composeState.value` to `chatModel.draft.value` and `chatModel.draftChatId.value` |
| Empty but draft exists for current chat | Restores draft from `chatModel.draft` |
| Empty, no draft | Clears previous draft, deletes unused files |

### Restore Behavior

When entering a chat (line ~132 in `ChatView.kt`):

1. Checks if `chatModel.draftChatId.value` matches the chat ID.
2. If match and draft is not null (and not a cross-chat forward), initializes `composeState` from the draft.
3. Otherwise, creates a fresh `ComposeState`.

### Desktop-specific

On desktop, a `DisposableEffect` (line ~1256) saves the draft on dispose when forwarding content, since the `KeyChangeEffect` mechanism is Android-specific.

### Draft Display in Chat List

When a draft exists for a chat, `ChatPreviewView` shows a pencil icon with the draft text instead of the last message preview.

---

## 8. Source Files

| File | Description |
|---|---|
| `ComposeView.kt` | ComposeState, ComposePreview, ComposeContextItem, ComposeView composable, send logic, link preview, draft persistence |
| `SendMsgView.kt` | Text input field, send/voice/live/timed buttons, recording state |
| `ComposeFileView.kt` | File attachment preview (name, cancel) |
| `ComposeImageView.kt` | Media attachment preview (thumbnails, cancel) |
| `ComposeVoiceView.kt` | Voice recording preview (waveform, duration, play) |
| `ContextItemView.kt` | Reply/edit/forward/report context bar |
| `ComposeContextContactRequestActionsView.kt` | Contact request action buttons in compose area |
| `ComposeContextGroupDirectInvitationActionsView.kt` | Group direct invitation compose actions |
| `ComposeContextPendingMemberActionsView.kt` | Pending member compose actions |
| `ComposeContextProfilePickerView.kt` | Profile picker in compose context |
| `SelectableChatItemToolbars.kt` | Multi-select mode toolbar (delete, forward, moderate) |
