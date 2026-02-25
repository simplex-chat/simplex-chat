# WebRTC Calling Service

## Table of Contents

1. [Overview](#1-overview)
2. [Call State Machine](#2-call-state-machine)
3. [Android Implementation](#3-android-implementation)
4. [Desktop Implementation](#4-desktop-implementation)
5. [Common Call API](#5-common-call-api)
6. [IncomingCallAlertView](#6-incomingcallalertview)
7. [Source Files](#7-source-files)

## Executive Summary

WebRTC calling in SimpleX Chat operates over SMP (SimpleX Messaging Protocol) for signaling, with platform-specific WebRTC media implementations. Android uses a WebView-based approach with a dedicated `CallActivity` and foreground `CallService`, while Desktop opens the system browser and communicates via a NanoWSD WebSocket server on localhost. Both platforms share a common `CallManager` for call lifecycle and a `CallState` enum for state tracking. Call commands and responses are serialized as JSON and exchanged between the native layer and the WebRTC JavaScript layer.

---

## 1. Overview

Call signaling uses the same SMP protocol on all platforms -- call invitations, offers, answers, ICE candidates, and status updates flow through the chat backend via API commands. The WebRTC media plane, however, is implemented differently per platform:

- **Android**: WebView loads `call.html` from bundled assets; a `@JavascriptInterface` bridge (`WebRTCInterface`) forwards JSON messages between Kotlin and JavaScript.
- **Desktop**: The system browser opens `http://localhost:50395/simplex/call/`; a NanoWSD HTTP+WebSocket server serves `call.html` from classpath resources and relays JSON commands/responses over WebSocket.

Both platforms share the [`CallManager`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/CallManager.kt) class (119 lines), which orchestrates incoming call acceptance, call ending, and notification management.

---

## 2. Call State Machine

Defined in [`WebRTC.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/WebRTC.kt#L50):

```
enum class CallState {
  WaitCapabilities,    // Call initiated, waiting for local WebRTC capabilities
  InvitationSent,      // Invitation sent to peer via SMP
  InvitationAccepted,  // Peer's invitation accepted locally
  OfferSent,           // SDP offer sent to peer
  OfferReceived,       // SDP offer received from peer
  AnswerReceived,      // SDP answer received from peer
  Negotiated,          // ICE negotiation in progress
  Connected,           // Media flowing
  Ended;               // Call terminated
}
```

**Outgoing call flow**: `WaitCapabilities` -> `InvitationSent` -> `OfferSent` -> `AnswerReceived` -> `Negotiated` -> `Connected` -> `Ended`

**Incoming call flow**: `InvitationAccepted` -> `OfferReceived` -> `Negotiated` -> `Connected` -> `Ended`

State transitions are driven by `WCallResponse` messages from the WebRTC layer. Each transition typically triggers a corresponding API command (e.g., `apiSendCallInvitation`, `apiSendCallOffer`).

---

## 3. Android Implementation

### 3.1 CallActivity.kt (465 lines)

[`CallActivity.kt`](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt)

A dedicated `ComponentActivity` that hosts the call UI. Key responsibilities:

- **Intent handling** ([line 64](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L64)): On `AcceptCallAction` intent, looks up the matching `RcvCallInvitation` and calls `callManager.acceptIncomingCall()`.
- **Lock screen support** ([line 160](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L160)): `unlockForIncomingCall()` uses `setShowWhenLocked(true)` / `setTurnScreenOn(true)` on API 27+, falls back to window flags on older versions. `lockAfterIncomingCall()` reverses these settings.
- **Picture-in-Picture** ([line 99](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L99)): `setPipParams()` configures PiP aspect ratio and source rect hint. On Android 12+ (`Build.VERSION_CODES.S`), auto-enter PiP is enabled for video calls. `onPictureInPictureModeChanged` toggles `activeCallViewIsCollapsed` and sends a `WCallCommand.Layout` command.
- **Permission checks** ([line 122](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L122)): Checks `RECORD_AUDIO` and conditionally `CAMERA` permissions.
- **Service binding** ([line 181](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L181)): Binds to `CallService` as a workaround for Android 12 background activity launch restrictions.
- **CallActivityView composable** ([line 208](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L208)): Renders `ActiveCallView()` when permissions are granted and a call is active. Shows `CallPermissionsView` when permissions are needed. Shows `IncomingCallLockScreenAlert` for incoming calls on the lock screen.

### 3.2 CallService.kt (208 lines)

[`CallService.kt`](../../android/src/main/java/chat/simplex/app/CallService.kt)

An Android foreground `Service` that keeps the call alive when the app is backgrounded:

- **Foreground notification** ([line 131](../../android/src/main/java/chat/simplex/app/CallService.kt#L131)): Shows contact name (respecting `NotificationPreviewMode`), call type (audio/video), a chronometer when connected, and an "End call" action button.
- **WakeLock** ([line 66](../../android/src/main/java/chat/simplex/app/CallService.kt#L66)): Acquires `PARTIAL_WAKE_LOCK` to prevent CPU sleep during calls.
- **Notification channel** ([line 121](../../android/src/main/java/chat/simplex/app/CallService.kt#L121)): Creates `CALL_NOTIFICATION_CHANNEL_ID` with `IMPORTANCE_DEFAULT`.
- **Foreground service type** ([line 100](../../android/src/main/java/chat/simplex/app/CallService.kt#L100)): Uses `MEDIA_PLAYBACK | MICROPHONE` (+ `CAMERA` for video) on API 30+, `REMOTE_MESSAGING` on API 34+ when no active call.
- **Binder** ([line 158](../../android/src/main/java/chat/simplex/app/CallService.kt#L158)): `CallServiceBinder` allows `CallActivity` to call `updateNotification()` when call state changes.
- **CallActionReceiver** ([line 170](../../android/src/main/java/chat/simplex/app/CallService.kt#L170)): `BroadcastReceiver` that handles the `EndCallAction` from the notification.

### 3.3 CallView.android.kt (891 lines)

[`CallView.android.kt`](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt)

The `actual` platform implementation of `ActiveCallView()` and supporting composables:

- **ActiveCallState** ([line 74](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L74)): Manages proximity lock (screen-off wake lock), `CallAudioDeviceManager` for audio routing (earpiece/speaker/bluetooth), `CallSoundsPlayer` for ringtones and vibration. Implements `Closeable` to clean up resources on call end.
- **ActiveCallView** ([line 114](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L114)): Renders `WebRTCView` plus `ActiveCallOverlay`. Handles `WCallResponse` messages and dispatches corresponding API calls. Manages volume control stream (`STREAM_VOICE_CALL`), screen keep-on, and call command lifecycle.
- **WebRTCView** ([line 691](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L691)): Creates/reuses a static `WebView` via `AndroidView`. Configures `WebViewAssetLoader` for local asset loading. Sets up `WebRTCInterface` JavaScript bridge. Loads `file:android_asset/www/android/call.html`. Processes `WCallCommand` queue by evaluating `processCommand()` JavaScript.
- **ActiveCallOverlayLayout** ([line 329](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L329)): Full overlay with mic toggle, speaker/device selector, end call, video toggle, and camera flip buttons. Adapts layout for video vs audio calls.
- **CallPermissionsView** ([line 569](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L569)): Handles runtime permission requests for microphone and camera with a fallback to settings if the system dialog is not shown.

### 3.4 ActiveCallState

[`ActiveCallState`](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L74) (line 74 of `CallView.android.kt`):

| Component | Purpose |
|---|---|
| `proximityLock` | `PROXIMITY_SCREEN_OFF_WAKE_LOCK` -- turns screen off when phone is held to ear |
| `callAudioDeviceManager` | Manages audio routing between earpiece, speaker, Bluetooth, wired headset |
| `CallSoundsPlayer` | Plays connecting/ringing sounds and vibration patterns |
| `wasConnected` | Tracks if call ever connected (for end-of-call vibration) |
| `close()` | Stops sounds, vibrates on disconnect, releases proximity lock, clears audio manager overrides |

---

## 4. Desktop Implementation

### 4.1 CallView.desktop.kt (263 lines)

[`CallView.desktop.kt`](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt)

Desktop calls run WebRTC in the system browser, not an embedded WebView:

- **NanoWSD server** ([line 209](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L209)): `startServer()` creates a `NanoWSD` instance bound to `localhost:50395`. The server serves `call.html` from JAR resources at `/assets/www/desktop/call.html` for the path `/simplex/call/`. All other paths serve resources from `/assets/www/`.
- **WebSocket communication** ([line 238](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L238)): `MyWebSocket` handles WebSocket frames from the browser. `onMessage` deserializes JSON into `WVAPIMessage` and forwards to the response handler. `onClose` triggers `WCallResponse.End`.
- **WebRTCController** ([line 153](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L153)): Opens `http://localhost:50395/simplex/call/` via `LocalUriHandler`. Processes `WCallCommand` queue by sending JSON over WebSocket to all active connections. On dispose, sends `WCallCommand.End` and stops the server.
- **SendStateUpdates** ([line 137](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L137)): Sends `WCallCommand.Description` with call state and encryption info text to the browser for display.
- **ActiveCallView** ([line 28](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L28)): Handles `WCallResponse` messages identically to Android (same state machine), plus a `WCallCommand.Permission` message on `Capabilities` error for browser permission denial guidance.

---

## 5. Common Call API

Defined in [`SimpleXAPI.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt):

| Function | Line | Description |
|---|---|---|
| `apiGetCallInvitations` | [L1842](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1842) | Retrieve pending call invitations from the backend |
| `apiSendCallInvitation` | [L1849](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1849) | Send call invitation to a contact with `CallType` |
| `apiRejectCall` | [L1854](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1854) | Reject an incoming call |
| `apiSendCallOffer` | [L1859](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1859) | Send SDP offer with ICE candidates and capabilities |
| `apiSendCallAnswer` | [L1866](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1866) | Send SDP answer with ICE candidates |
| `apiSendCallExtraInfo` | [L1872](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1872) | Send additional ICE candidates discovered after initial exchange |
| `apiEndCall` | [L1878](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1878) | Terminate a call |
| `apiCallStatus` | [L1883](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1883) | Report WebRTC connection status to the backend |

All functions send commands via `sendCmd()` to the chat core and return `Boolean` success status (except `apiGetCallInvitations` which returns `List<RcvCallInvitation>`).

---

## 6. IncomingCallAlertView

[`IncomingCallAlertView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt) (128 lines)

An in-app notification banner shown when a call invitation arrives while the app is in the foreground:

- **IncomingCallAlertView** ([line 27](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt#L27)): Starts `SoundPlayer` for the ringtone (suppressed if already in a call view). Shows `IncomingCallAlertLayout`.
- **IncomingCallAlertLayout** ([line 49](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt#L49)): Colored banner with `ProfilePreview` of the caller, call type icon (audio/video), and three action buttons: Reject (red), Ignore (primary), Accept (green).
- **IncomingCallInfo** ([line 74](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt#L74)): Shows the user profile image (for multi-user), call media type icon, and call type text (encrypted/unencrypted audio/video).

---

## 7. Source Files

| File | Path | Lines | Description |
|---|---|---|---|
| `CallView.kt` | [`common/src/commonMain/.../views/call/CallView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/CallView.kt) | 28 | `expect fun ActiveCallView()`, delivery receipt waiting |
| `CallView.android.kt` | [`common/src/androidMain/.../views/call/CallView.android.kt`](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt) | 891 | Android WebView WebRTC, overlay, permissions |
| `CallView.desktop.kt` | [`common/src/desktopMain/.../views/call/CallView.desktop.kt`](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt) | 263 | Desktop browser WebRTC via NanoWSD |
| `CallActivity.kt` | [`android/src/main/java/.../views/call/CallActivity.kt`](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt) | 465 | Android call Activity, PiP, lock screen |
| `CallService.kt` | [`android/src/main/java/.../CallService.kt`](../../android/src/main/java/chat/simplex/app/CallService.kt) | 208 | Android foreground service for calls |
| `CallManager.kt` | [`common/src/commonMain/.../views/call/CallManager.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/CallManager.kt) | 119 | Call lifecycle management |
| `WebRTC.kt` | [`common/src/commonMain/.../views/call/WebRTC.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/WebRTC.kt) | -- | `CallState` enum, `WCallCommand`, `WCallResponse` types |
| `IncomingCallAlertView.kt` | [`common/src/commonMain/.../views/call/IncomingCallAlertView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt) | 128 | In-app incoming call notification banner |
| `SimpleXAPI.kt` | [`common/src/commonMain/.../model/SimpleXAPI.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt) | -- | Call API commands (L1837--L1881) |
