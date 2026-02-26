# Calling Flow

> **Related spec:** [spec/services/calls.md](../../spec/services/calls.md)

## Overview

SimpleX Chat supports audio and video calls using WebRTC, with signaling delivered over the existing SMP messaging channels. Calls are end-to-end encrypted with an additional shared key layer on top of WebRTC's SRTP encryption.

The architecture differs by platform:
- **Android**: Calls run in a dedicated `CallActivity` (separate from `MainActivity`) with a `WebView` hosting the WebRTC JavaScript. A foreground `CallService` keeps the process alive and shows a persistent notification.
- **Desktop**: Calls open the system browser pointed at a local NanoHTTPD/NanoWSD embedded server on `localhost:50395`, which serves the WebRTC HTML/JS and communicates with the app via WebSocket.

Both platforms share a common signaling flow through the Haskell core API.

## Prerequisites

- Both parties must have an established direct contact connection.
- Microphone permission is required; camera permission is required for video calls.
- On Android, the `CallOnLockScreen` preference controls lock-screen call behavior: `DISABLE`, `SHOW`, or `ACCEPT`.

---

## 1. Outgoing Call (Caller Side)

### 1.1 Initiate Call

1. User taps the audio or video call button in `ChatView`.
2. `startChatCall(remoteHostId, chatInfo, media)` is called (in `ChatView.kt`).
3. A `Call` object is created with `callState = CallState.WaitCapabilities`:
   ```kotlin
   Call(
     remoteHostId = remoteHostId,
     contact = contact,
     callUUID = null,
     callState = CallState.WaitCapabilities,
     initialCallType = media,  // Audio or Video
     userProfile = profile,
     androidCallState = platform.androidCreateActiveCallState()
   )
   ```
4. `ChatModel.activeCall` is set and `ChatModel.showCallView` is set to `true`.
5. A `WCallCommand.Capabilities(media)` command is added to `ChatModel.callCommand`.

### 1.2 WebRTC Capabilities Response

1. The WebRTC engine (WebView on Android, browser on Desktop) receives the `Capabilities` command.
2. It responds with `WCallResponse.Capabilities(capabilities)` containing encryption support info.
3. The app calls `ChatController.apiSendCallInvitation(rh, contact, callType)` to send the invitation via SMP.
4. Call state transitions to `CallState.InvitationSent`.
5. A connecting sound starts playing via `CallSoundsPlayer.startConnectingCallSound`.

### 1.3 Offer Exchange

1. When the callee accepts, the WebRTC engine generates an offer.
2. `WCallResponse.Offer(offer, iceCandidates, capabilities)` is received.
3. `ChatController.apiSendCallOffer(rh, contact, rtcSession, rtcIceCandidates, media, capabilities)` sends it.
4. Call state transitions to `CallState.OfferSent`.

### 1.4 Answer and Connection

1. The callee's answer arrives via SMP as a chat event.
2. The app dispatches `WCallCommand.Answer(answer, iceCandidates)` to the WebRTC engine.
3. Call state transitions to `CallState.Negotiated`, then to `CallState.Connected` once the ICE connection succeeds.
4. `Call.connectedAt` is set to the current timestamp.

---

## 2. Incoming Call (Callee Side)

### 2.1 Receive Invitation

1. An incoming call event arrives from the core as `CR.CallInvitation`.
2. `CallManager.reportNewIncomingCall(invitation)` is called.
3. A `RcvCallInvitation` is stored in `ChatModel.callInvitations` keyed by contact ID.
4. If the invitation is recent (within 3 minutes), a system notification is shown and `ChatModel.activeCallInvitation` is set.
5. On Android, `CallActivity` may be launched on the lock screen if `callOnLockScreen` is `SHOW` or `ACCEPT`.

### 2.2 Accept Call

1. User taps "Accept" on the `IncomingCallAlertView` or lock-screen alert.
2. `CallManager.acceptIncomingCall(invitation)` is called.
3. If another call is active, it is ended first (with `switchingCall` flag set).
4. A new `Call` is created with `callState = CallState.InvitationAccepted`.
5. ICE servers are loaded from preferences (`getIceServers()`).
6. `WCallCommand.Start(media, aesKey, iceServers, relay)` is dispatched to the WebRTC engine.
7. The call invitation is removed from `callInvitations` and the notification is cancelled.

### 2.3 Reject Call

1. User taps "Reject" or the invitation times out.
2. `CallManager.endCall(invitation)` is called.
3. `ChatController.apiRejectCall(rh, contact)` notifies the caller.
4. The invitation is removed from `callInvitations`.

---

## 3. Call State Machine

```
Outgoing: WaitCapabilities -> InvitationSent -> OfferSent -> AnswerReceived -> Negotiated -> Connected -> Ended
Incoming: InvitationAccepted -> OfferReceived -> Negotiated -> Connected -> Ended
```

| State | Description |
|-------|-------------|
| `WaitCapabilities` | Querying local WebRTC capabilities |
| `InvitationSent` | Caller sent invitation via SMP |
| `InvitationAccepted` | Callee accepted, starting WebRTC |
| `OfferSent` | Caller sent SDP offer |
| `OfferReceived` | Callee received SDP offer |
| `AnswerReceived` | Caller received SDP answer |
| `Negotiated` | ICE negotiation complete |
| `Connected` | Media flowing |
| `Ended` | Call terminated |

---

## 4. Ending a Call

1. User taps the end-call button, or the remote side ends the call.
2. `CallManager.endCall(call)` is called.
3. `ChatController.apiEndCall(rh, contact)` notifies the remote side via SMP.
4. `ChatModel.showCallView` is set to `false`.
5. `ChatModel.activeCall` is set to `null`.
6. On Android, `CallService` is stopped and the `WebView` is destroyed.
7. On Desktop, `WCallCommand.End` is sent to the browser via WebSocket, and the NanoWSD server is stopped.

---

## 5. Android-Specific: CallActivity and CallService

### 5.1 CallActivity

- `CallActivity` is a separate `ComponentActivity` (not `MainActivity`).
- It is launched via `platform.androidStartCallActivity(acceptCall, remoteHostId, chatId)`.
- It hosts `ActiveCallView` with a `WebView` for WebRTC.
- Supports lock-screen display: `setShowWhenLocked(true)` and `setTurnScreenOn(true)`.
- Supports Picture-in-Picture (PiP) mode for video calls.
  - On Android 12+, PiP auto-enters when the user navigates away.
  - On older versions, PiP is entered via `enterPictureInPictureMode()` on `onUserLeaveHint`.
  - PiP layout switches to `LayoutType.RemoteVideo` to show only the remote video feed.
- The activity finishes itself when both `invitation == null` and (`!showCallView || call == null`) and `!switchingCall`.

### 5.2 CallService

- `CallService` is a foreground `Service` that keeps the process alive during calls.
- Started via `CallService.startService()` which calls `ContextCompat.startForegroundService`.
- Acquires a partial `WakeLock` to prevent CPU sleep.
- Shows a persistent notification with:
  - Contact name and call type (audio/video).
  - An "End Call" action button.
  - A chronometer showing call duration (from `connectedAt`).
- The notification taps open `CallActivity`.
- Foreground service type includes `MICROPHONE`, `CAMERA` (if video), and `MEDIA_PLAYBACK`.

---

## 6. Desktop-Specific: Browser-Based WebRTC

### 6.1 NanoWSD Embedded Server

1. When a call starts, `startServer(onResponse)` creates a `NanoWSD` server on `localhost:50395`.
2. The server serves static WebRTC HTML/JS from bundled resources at `/assets/www/desktop/call.html`.
3. The system browser is opened to `http://localhost:50395/simplex/call/`.

### 6.2 WebSocket Communication

1. The browser page connects back via WebSocket to the same `localhost:50395` server.
2. Commands from the app to the browser are serialized as `WVAPICall(corrId, command)` JSON.
3. Responses from the browser arrive as `WVAPIMessage(corrId, resp, command)` JSON.
4. The `WebRTCController` composable manages the command queue:
   - Collects commands from `ChatModel.callCommand` (a `SnapshotStateList<WCallCommand>`).
   - Sends them to the browser via the WebSocket connection.
   - Processes responses through the same `WCallResponse` handling as Android.
5. On dispose, `WCallCommand.End` is sent, the server is stopped, and connections are cleared.

---

## 7. Common Signaling API

| API Function | Purpose |
|-------------|---------|
| `apiSendCallInvitation(rh, contact, callType)` | Send call invitation via SMP |
| `apiRejectCall(rh, contact)` | Reject incoming call |
| `apiSendCallOffer(rh, contact, rtcSession, rtcIceCandidates, media, capabilities)` | Send SDP offer |
| `apiSendCallAnswer(rh, contact, rtcSession, rtcIceCandidates)` | Send SDP answer |
| `apiSendCallExtraInfo(rh, contact, rtcIceCandidates)` | Send additional ICE candidates |
| `apiEndCall(rh, contact)` | End active call |
| `apiCallStatus(rh, contact, status)` | Report WebRTC connection status |

---

## 8. In-Call Media Controls

During an active call, the user can toggle media sources via `WCallCommand.Media(source, enable)`:

| Source | Control |
|--------|---------|
| `CallMediaSource.Mic` | Mute/unmute microphone |
| `CallMediaSource.Camera` | Enable/disable camera |
| `CallMediaSource.ScreenAudio` | Screen share audio |
| `CallMediaSource.ScreenVideo` | Screen share video |

Camera switching (front/back) is done via `WCallCommand.Camera(VideoCamera.User / VideoCamera.Environment)`.

---

## Key Types Reference

| Type | Location | Purpose |
|------|----------|---------|
| `Call` | `views/call/WebRTC.kt` | Active call state: contact, callState, media sources, encryption |
| `CallState` | `views/call/WebRTC.kt` | Enum: WaitCapabilities through Ended |
| `RcvCallInvitation` | `views/call/WebRTC.kt` | Incoming call invitation with contact, callType, sharedKey |
| `CallManager` | `views/call/CallManager.kt` | Manages call lifecycle: accept, end, report |
| `WCallCommand` | `views/call/WebRTC.kt` | Commands to WebRTC engine: Capabilities, Start, Offer, Answer, Ice, Media, Camera, End |
| `WCallResponse` | `views/call/WebRTC.kt` | Responses from WebRTC: Capabilities, Offer, Answer, Ice, Connection, Connected, End |
| `CallActivity` | `android/.../views/call/CallActivity.kt` | Android Activity hosting the call UI and WebView |
| `CallService` | `android/.../CallService.kt` | Android foreground Service for call persistence |
| `NanoWSD` | `desktopMain/.../views/call/CallView.desktop.kt` | Desktop embedded HTTP+WebSocket server |
