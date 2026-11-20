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

<a id="CallState"></a>

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
  Reconnecting,        // Connection lost, waiting for ICE to recover
  Ended;               // Call terminated
}
```

**Outgoing call flow**: `WaitCapabilities` -> `InvitationSent` -> `OfferSent` -> `AnswerReceived` -> `Negotiated` -> `Connected` -> `Ended`

**Incoming call flow**: `InvitationAccepted` -> `OfferReceived` -> `Negotiated` -> `Connected` -> `Ended`

State transitions are driven by `WCallResponse` messages from the WebRTC layer. Each transition typically triggers a corresponding API command (e.g., `apiSendCallInvitation`, `apiSendCallOffer`).

**Reconnection**: `Connected` -> `Reconnecting` -> `Connected` or `Ended`. When a connected call loses its WebRTC connection, `call.js` reports `"reconnecting"` connection state instead of ending the call. The app sets `CallState.Reconnecting` and plays the connecting sound; this state is not sent to the core with `apiCallStatus`, so the call item duration is not affected, and `connectedAt` is kept. The party that sent the initial offer (callee) restarts ICE every 10 seconds until reconnected: the restart offer and the answer are sent as `WCallResponse.Ice` / `WCallCommand.Ice` with `session`, via `apiSendCallExtraInfo` (`x.call.extra` with `rtcSession`). If the connection is not restored in 30 seconds, `call.js` reports `"disconnected"` and ends the call. Reconnection is enabled by `canReconnect` in `WCallCommand.Start` / `WCallCommand.Offer`, set when the contact's peer chat version is at least `CALL_RECONNECT_VERSION` (22); otherwise the call ends as soon as the connection is lost.

---

<a id="ActiveCallView"></a>

## 3. Android Implementation

### 3.1 CallActivity.kt (468 lines)

[`CallActivity.kt`](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt)

A dedicated `ComponentActivity` that hosts the call UI. Key responsibilities:

- **Intent handling** ([line 66](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L66)): On `AcceptCallAction` intent, looks up the matching `RcvCallInvitation` and calls `callManager.acceptIncomingCall()`.
- **Lock screen support** ([line 162](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L162)): `unlockForIncomingCall()` uses `setShowWhenLocked(true)` / `setTurnScreenOn(true)` on API 27+, falls back to window flags on older versions. `lockAfterIncomingCall()` reverses these settings.
- **Picture-in-Picture** ([line 101](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L101)): `setPipParams()` configures PiP aspect ratio and source rect hint. On Android 12+ (`Build.VERSION_CODES.S`), auto-enter PiP is enabled for video calls. `onPictureInPictureModeChanged` toggles `activeCallViewIsCollapsed` and sends a `WCallCommand.Layout` command.
- **Permission checks** ([line 124](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L124)): Checks `RECORD_AUDIO` and conditionally `CAMERA` permissions.
- **Service binding** ([line 183](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L183)): Binds to `CallService` as a workaround for Android 12 background activity launch restrictions.
- **CallActivityView composable** ([line 210](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt#L210)): Renders `ActiveCallView()` when permissions are granted and a call is active. Shows `CallPermissionsView` when permissions are needed. Shows `IncomingCallLockScreenAlert` for incoming calls on the lock screen.

### 3.2 CallService.kt (207 lines)

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

- **ActiveCallState** ([line 75](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L75)): Manages proximity lock (screen-off wake lock), `CallAudioDeviceManager` for audio routing (earpiece/speaker/bluetooth), `CallSoundsPlayer` for ringtones and vibration. Implements `Closeable` to clean up resources on call end.
- **ActiveCallView** ([line 115](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L115)): Renders `WebRTCView` plus `ActiveCallOverlay`. Handles `WCallResponse` messages and dispatches corresponding API calls. Manages volume control stream (`STREAM_VOICE_CALL`), screen keep-on, and call command lifecycle.
- **WebRTCView** ([line 692](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L692)): Creates/reuses a static `WebView` via `AndroidView`. Configures `WebViewAssetLoader` for local asset loading. Sets up `WebRTCInterface` JavaScript bridge. Loads `file:android_asset/www/android/call.html`. Processes `WCallCommand` queue by evaluating `processCommand()` JavaScript.
- **ActiveCallOverlayLayout** ([line 330](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L330)): Full overlay with mic toggle, speaker/device selector, end call, video toggle, and camera flip buttons. Adapts layout for video vs audio calls.
- **CallPermissionsView** ([line 570](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L570)): Handles runtime permission requests for microphone and camera with a fallback to settings if the system dialog is not shown.

### 3.4 ActiveCallState

[`ActiveCallState`](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt#L75) (line 75 of `CallView.android.kt`):

| Component | Purpose |
|---|---|
| `proximityLock` | `PROXIMITY_SCREEN_OFF_WAKE_LOCK` -- turns screen off when phone is held to ear |
| `callAudioDeviceManager` | Manages audio routing between earpiece, speaker, Bluetooth, wired headset |
| `CallSoundsPlayer` | Plays connecting/ringing sounds and vibration patterns |
| `wasConnected` | Tracks if call ever connected (for end-of-call vibration) |
| `close()` | Stops sounds, vibrates on disconnect, releases proximity lock, clears audio manager overrides |

---

## 4. Desktop Implementation

### 4.1 CallView.desktop.kt (308 lines)

[`CallView.desktop.kt`](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt)

Desktop calls run WebRTC in the system browser, not an embedded WebView:

- **NanoWSD server** ([line 215](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L215)): `startServer()` creates a `NanoWSD` instance bound to `localhost:50395`. If that port is already in use it falls back to an OS-assigned free port (`port 0`); `WebRTCController` reads `server.listeningPort` for the browser URL. The server serves `call.html` from JAR resources at `/assets/www/desktop/call.html` for the path `/simplex/call/` when the request includes a valid `token` query parameter. All other paths serve resources from `/assets/www/`.
- **Call token** ([line 271](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L271)): `newCallServerToken()` returns 32 random bytes, base64url-encoded. `hasValidCallServerToken()` ([line 277](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L277)) checks the `token` query parameter.
- **WebSocket upgrade** ([line 238](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L238)): Each server instance accepts one WebSocket upgrade. The upgrade request must include `Origin: http://localhost:<listeningPort>` and a valid token. Other upgrade requests receive `401 Unauthorized`.
- **WebSocket communication** ([line 283](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L283)): `MyWebSocket` handles WebSocket frames from the browser. `onMessage` deserializes JSON into `WVAPIMessage` and forwards to the response handler. `onClose` triggers `WCallResponse.End`.
- **WebRTCController** ([line 157](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L157)): Generates the call token, starts the server, then opens `http://localhost:<listeningPort>/simplex/call/?token=<token>` (normally `50395`) via `LocalUriHandler`. Processes `WCallCommand` queue by sending JSON over the WebSocket. On dispose, sends `WCallCommand.End` and stops the server.
- **SendStateUpdates** ([line 141](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L141)): Sends `WCallCommand.Description` with call state and encryption info text to the browser for display.
- **ActiveCallView** ([line 32](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt#L32)): Handles `WCallResponse` messages identically to Android (same state machine), plus a `WCallCommand.Permission` message on `Capabilities` error for browser permission denial guidance.

---

## 5. Common Call API

Defined in [`SimpleXAPI.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt):

| Function | Line | Description |
|---|---|---|
| `apiGetCallInvitations` | [L1843](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1843) | Retrieve pending call invitations from the backend |
| `apiSendCallInvitation` | [L1850](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1850) | Send call invitation to a contact with `CallType` |
| `apiRejectCall` | [L1855](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1855) | Reject an incoming call |
| `apiSendCallOffer` | [L1860](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1860) | Send SDP offer with ICE candidates and capabilities |
| `apiSendCallAnswer` | [L1867](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1867) | Send SDP answer with ICE candidates |
| `apiSendCallExtraInfo` | [L2157](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L2157) | Send additional ICE candidates discovered after initial exchange, or ICE restart offer/answer in `rtcSession` |
| `apiEndCall` | [L1879](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1879) | Terminate a call |
| `apiCallStatus` | [L1884](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L1884) | Report WebRTC connection status to the backend |

All functions send commands via `sendCmd()` to the chat core and return `Boolean` success status (except `apiGetCallInvitations` which returns `List<RcvCallInvitation>`).

---

<a id="IncomingCallAlertView"></a>

## 6. IncomingCallAlertView

[`IncomingCallAlertView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt) (128 lines)

An in-app notification banner shown when a call invitation arrives while the app is in the foreground:

- **IncomingCallAlertView** ([line 28](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt#L28)): Starts `SoundPlayer` for the ringtone (suppressed if already in a call view). Shows `IncomingCallAlertLayout`.
- **IncomingCallAlertLayout** ([line 50](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt#L50)): Colored banner with `ProfilePreview` of the caller, call type icon (audio/video), and three action buttons: Reject (red), Ignore (primary), Accept (green).
- **IncomingCallInfo** ([line 75](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt#L75)): Shows the user profile image (for multi-user), call media type icon, and call type text (encrypted/unencrypted audio/video).

---

## 7. Source Files

| File | Path | Lines | Description |
|---|---|---|---|
| `CallView.kt` | [`common/src/commonMain/.../views/call/CallView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/CallView.kt) | 28 | `expect fun ActiveCallView()`, delivery receipt waiting |
| `CallView.android.kt` | [`common/src/androidMain/.../views/call/CallView.android.kt`](../../common/src/androidMain/kotlin/chat/simplex/common/views/call/CallView.android.kt) | 891 | Android WebView WebRTC, overlay, permissions |
| `CallView.desktop.kt` | [`common/src/desktopMain/.../views/call/CallView.desktop.kt`](../../common/src/desktopMain/kotlin/chat/simplex/common/views/call/CallView.desktop.kt) | 308 | Desktop browser WebRTC via NanoWSD |
| `CallActivity.kt` | [`android/src/main/java/.../views/call/CallActivity.kt`](../../android/src/main/java/chat/simplex/app/views/call/CallActivity.kt) | 472 | Android call Activity, PiP, lock screen |
| `CallService.kt` | [`android/src/main/java/.../CallService.kt`](../../android/src/main/java/chat/simplex/app/CallService.kt) | 207 | Android foreground service for calls |
| `CallManager.kt` | [`common/src/commonMain/.../views/call/CallManager.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/CallManager.kt) | 119 | Call lifecycle management |
| `WebRTC.kt` | [`common/src/commonMain/.../views/call/WebRTC.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/WebRTC.kt) | -- | `CallState` enum, `WCallCommand`, `WCallResponse` types |
| `IncomingCallAlertView.kt` | [`common/src/commonMain/.../views/call/IncomingCallAlertView.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/views/call/IncomingCallAlertView.kt) | 128 | In-app incoming call notification banner |
| `SimpleXAPI.kt` | [`common/src/commonMain/.../model/SimpleXAPI.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt) | -- | Call API commands (L1837--L1881) |
