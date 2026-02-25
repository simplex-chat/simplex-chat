# Audio / Video Call

> **Related spec:** [spec/services/calls.md](../../spec/services/calls.md)

## Purpose

Make and receive end-to-end encrypted audio and video calls over WebRTC. The implementation differs significantly between Android (WebView-based with `CallActivity` and PiP support) and Desktop (browser-based WebRTC via NanoHTTPD server on localhost).

## Route / Navigation

- **Entry point (outgoing)**: Tap audio or video call button in `ChatInfoView` action buttons or `ChatView` toolbar
- **Entry point (incoming)**: `IncomingCallAlertView` banner appears at top of screen
- **Presented by**: `ActiveCallView()` (expect/actual composable) is shown when `chatModel.showCallView == true`
- **Dismiss**: Call ends when user taps end button or remote party disconnects; `callManager.endCall()` handles cleanup
- **Android PiP**: Call view supports picture-in-picture mode via `CallActivity`

## Platform Differences

| Aspect | Android | Desktop |
|---|---|---|
| WebRTC host | `WebView` with `WebViewAssetLoader` serving local assets | NanoHTTPD server on `localhost:50395` opened in system browser |
| Call activity | `CallActivity` (separate Android Activity) with lifecycle management | Inline composable with `WebRTCController` |
| PiP support | Native Android PiP via `CallActivity` | Not supported |
| Audio management | `CallAudioDeviceManager` with Android `AudioManager`, proximity wake lock | System browser audio routing |
| WebSocket | N/A | `NanoWSD` WebSocket server for bidirectional WebRTC signaling |

## Page Sections

### Incoming Call Banner (`IncomingCallAlertView`)

Displayed as an overlay banner when `chatModel.activeCallInvitation` is set:

| Element | Description |
|---|---|
| User profile image | Shown when multiple profiles exist (32dp `ProfileImage`) |
| Call type icon | `ic_videocam_filled` (green) for video, `ic_call_filled` (green) for audio |
| Call type text | `invitation.callTypeText` with caller info |
| Caller profile | `ProfilePreview` showing caller name and avatar (64dp) |
| Reject button | Red `ic_call_end_filled` icon -- ends the invitation via `callManager.endCall(invitation)` |
| Ignore button | Blue `ic_close` icon -- dismisses banner, cancels notification |
| Accept button | Green `ic_check_filled` icon -- accepts via `callManager.acceptIncomingCall(invitation)` |

Sound: `SoundPlayer.start()` plays ringtone while banner is visible (unless call view is already showing).

### Active Call View

#### Android (`CallView.android.kt`)

| Element | Description |
|---|---|
| WebView | `AndroidView` wrapping a `WebView` that loads `call.html` via `WebViewAssetLoader`; handles WebRTC JS bridge |
| `ActiveCallState` | Manages proximity lock (`PROXIMITY_SCREEN_OFF_WAKE_LOCK`), audio device manager, call sounds |
| Call controls overlay | Mic toggle, speaker toggle, camera switch, video toggle, end call button |
| Audio device selection | `CallAudioDeviceManager` with device enumeration (earpiece, speaker, Bluetooth, wired headset) |
| Permissions | Runtime permission checks for `CAMERA` and `RECORD_AUDIO` via Accompanist permissions library |

#### Desktop (`CallView.desktop.kt`)

| Element | Description |
|---|---|
| NanoHTTPD server | HTTP server on `localhost:50395` serving `call.html` and assets |
| NanoWSD WebSocket | WebSocket endpoint for bidirectional signaling between Kotlin and browser JS |
| `WebRTCController` | Processes `WCallCommand`/`WCallResponse` messages via `chatModel.callCommand` channel |
| Browser launch | `LocalUriHandler.openUri("http://localhost:50395/call.html")` opens system browser |
| Connection list | `connections: ArrayList<WebSocket>` tracks active WebSocket connections |

### WebRTC Signaling Flow

| Step | Command/Response | Description |
|---|---|---|
| 1. Capabilities | `WCallResponse.Capabilities` | Local capabilities reported; `apiSendCallInvitation()` called |
| 2. Offer | `WCallResponse.Offer` | SDP offer + ICE candidates sent via `apiSendCallOffer()` |
| 3. Answer | `WCallResponse.Answer` | SDP answer + ICE candidates sent via `apiSendCallAnswer()` |
| 4. ICE | `WCallResponse.Ice` | Additional ICE candidates exchanged via `apiSendCallExtraInfo()` |
| 5. Connection | `WCallResponse.Connection` | WebRTC connection state changes; `CallState.Connected` set on success |
| 6. Connected | `WCallResponse.Connected` | Connection info (relay/direct) stored in `call.connectionInfo` |
| 7. PeerMedia | `WCallResponse.PeerMedia` | Remote party media source changes (mic, camera, screen) |
| 8. Media control | `WCallCommand.Media` | Toggle local media sources (mic, camera, screen audio/video) |
| 9. Camera switch | `WCallCommand.Camera` | Switch between front/back camera |
| 10. End | `WCallResponse.End` / `WCallResponse.Ended` | Call termination; cleanup and UI dismissal |

### Call States (`CallState`)

| State | Description |
|---|---|
| `WaitCapabilities` | Waiting for WebRTC capabilities |
| `InvitationSent` | Call invitation sent to remote party |
| `OfferSent` | SDP offer sent |
| `Negotiated` | SDP answer received, negotiation complete |
| `Connected` | WebRTC media flowing; `connectedAt` timestamp set |
| `Ended` | Call terminated |

### Call Sounds

| Sound | Trigger |
|---|---|
| Connecting sound | `CallSoundsPlayer.startConnectingCallSound()` after invitation sent |
| In-call sound | `CallSoundsPlayer.startInCallSound()` when delivery receipt received |
| Ringtone | `SoundPlayer.start()` for incoming calls |
| End vibration | `CallSoundsPlayer.vibrate()` on call end (if was connected) |

## Source Files

| File | Path |
|---|---|
| `CallView.kt` | `views/call/CallView.kt` (common expect declarations) |
| `CallView.android.kt` | `androidMain/.../views/call/CallView.android.kt` |
| `CallView.desktop.kt` | `desktopMain/.../views/call/CallView.desktop.kt` |
| `IncomingCallAlertView.kt` | `views/call/IncomingCallAlertView.kt` |
