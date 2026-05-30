# Audio/Video Call Flow

> **Related spec:** [spec/services/calls.md](../../spec/services/calls.md)

## Overview

WebRTC-based audio and video calling in SimpleX Chat iOS. Calls are end-to-end encrypted with an additional shared key negotiated over the E2E encrypted SMP channel. The iOS app integrates with CallKit for native call UI (incoming call screen, lock screen integration) with a fallback mode for regions where CallKit is restricted (China). Call signaling (offer/answer/ICE candidates) is exchanged via SMP messages, not through a central signaling server.

## Prerequisites

- Established direct contact connection (calls are 1:1 only, not available in groups)
- Microphone permission granted (audio calls)
- Camera permission granted (video calls)
- Network connectivity for WebRTC peer-to-peer or relay

## Step-by-Step Processes

### 1. Initiate Call

1. User opens a direct chat in `ChatView`.
2. Taps the audio or video call button in the navigation bar.
3. `CallController` determines call type: `CallType(media: .audio/.video, capabilities: CallCapabilities(encryption: true))`.
4. If CallKit is enabled (`CallController.useCallKit()`):
   - `CXStartCallAction` is requested via `CXCallController`.
   - CallKit reports the outgoing call.
   - `provider(perform: CXStartCallAction)` fulfills and reports `reportOutgoingCall(startedConnectingAt:)`.
5. Calls `apiSendCallInvitation(contact:callType:)`:
   ```swift
   func apiSendCallInvitation(_ contact: Contact, _ callType: CallType) async throws
   ```
6. Sends `ChatCommand.apiSendCallInvitation(contact:callType:)`.
7. Core sends the call invitation to the contact via SMP.
8. `ChatModel.shared.activeCall` is set with the call state.

### 2. Receive Call

1. `ChatReceiver` receives `ChatEvent.callInvitation(callInvitation: RcvCallInvitation)`.
2. `RcvCallInvitation` contains: `user`, `contact`, `callType`, `sharedKey`, `callUUID`, `callTs`.
3. Processing in `processReceivedMsg`:
   - Call invitation is stored in `chatModel.callInvitations`.
4. If CallKit is enabled:
   - `CXProvider.reportNewIncomingCall` presents the native iOS incoming call UI.
   - Works even on lock screen and in background.
5. If CallKit is disabled (China / user preference):
   - `IncomingCallView` is shown as an in-app overlay.
   - `SoundPlayer` plays the ringtone.
6. User chooses to accept or reject.

### 3. Accept Call

1. **Via CallKit**: User swipes to accept on the native incoming call screen.
   - `provider(perform: CXAnswerCallAction)` is triggered.
   - Waits for chat to be started if needed (`waitUntilChatStarted(timeoutMs: 30_000)`).
   - `callManager.answerIncomingCall(callUUID:)` begins WebRTC setup.
   - `fulfillOnConnect` is set -- the action is fulfilled only when WebRTC reaches connected state (required for audio/mic to work on lock screen).
2. **Via in-app UI**: User taps "Accept" in `IncomingCallView`.
   - Directly starts WebRTC setup.

### 4. Reject Call

1. **Via CallKit**: User taps "Decline" on native UI.
   - `provider(perform: CXEndCallAction)` is triggered.
   - `callManager.endCall(callUUID:)` cleans up.
2. **Via API**: `apiRejectCall(contact:)` sends rejection to peer.
3. Call invitation is removed from `chatModel.callInvitations`.

### 5. WebRTC Setup (Signaling)

All signaling messages are exchanged via E2E encrypted SMP messages (no central signaling server).

**Caller side:**
1. `WebRTCClient` creates a `RTCPeerConnection`.
2. Creates SDP offer.
3. Calls `apiSendCallOffer(contact:rtcSession:rtcIceCandidates:media:capabilities:)`:
   ```swift
   func apiSendCallOffer(_ contact: Contact, _ rtcSession: String, _ rtcIceCandidates: String,
                         media: CallMediaType, capabilities: CallCapabilities) async throws
   ```
4. Constructs `WebRTCCallOffer(callType:rtcSession:)` and sends via `ChatCommand.apiSendCallOffer`.
5. Gathers ICE candidates and sends via `apiSendCallExtraInfo(contact:rtcIceCandidates:)`.

**Callee side:**
1. Receives the offer via SMP.
2. `WebRTCClient` sets remote description from the offer.
3. Creates SDP answer.
4. Calls `apiSendCallAnswer(contact:rtcSession:rtcIceCandidates:)`:
   ```swift
   func apiSendCallAnswer(_ contact: Contact, _ rtcSession: String, _ rtcIceCandidates: String) async throws
   ```
5. Constructs `WebRTCSession(rtcSession:rtcIceCandidates:)` and sends.
6. Gathers and sends additional ICE candidates via `apiSendCallExtraInfo`.

### 6. Media Streaming

1. WebRTC peer connection transitions to connected state.
2. If CallKit is used, `fulfillOnConnect` action is fulfilled (enables audio hardware).
3. Audio/video streams are active.
4. `ActiveCallView` displays:
   - Remote video (full screen)
   - Local video preview (picture-in-picture corner)
   - Call controls: mute, speaker, camera toggle, end call
   - Call duration timer
5. `CallViewRenderers` manages WebRTC video rendering surfaces.
6. Call status updates are sent via `apiCallStatus(contact:status:)`.

### 7. Audio Routing

1. `CallAudioDeviceManager` handles audio device selection.
2. Options: earpiece (receiver), speaker, Bluetooth devices.
3. `AudioDevicePicker` provides UI for device selection during call.
4. Uses `AVAudioSession` for routing configuration.

### 8. End Call

1. Either party taps "End" button.
2. Calls `apiEndCall(contact:)`:
   ```swift
   func apiEndCall(_ contact: Contact) async throws
   ```
3. Sends `ChatCommand.apiEndCall(contact:)` via SMP to notify peer.
4. `WebRTCClient` closes peer connection, releases media resources.
5. If CallKit: `CXEndCallAction` is requested, `provider(perform: CXEndCallAction)` fulfills.
6. `ChatModel.shared.activeCall` is cleared.
7. A `CICallItemView` event item is added to the chat history (call duration, type).

### 9. CallKit-Free Mode

1. `CallController.isInChina` checks `SKStorefront().countryCode == "CHN"`.
2. If in China or user disabled CallKit (`callKitEnabledGroupDefault`): `useCallKit()` returns `false`.
3. Incoming calls use `IncomingCallView` overlay instead of native CallKit UI.
4. `SoundPlayer` handles ringtone playback.
5. No lock-screen call answering; app must be in foreground or notified via push.

## Data Structures

| Type | Location | Description |
|------|----------|-------------|
| `CallType` | `SimpleXChat/CallTypes.swift` | `media: CallMediaType` (.audio/.video), `capabilities: CallCapabilities` |
| `CallMediaType` | `SimpleXChat/CallTypes.swift` | `.audio` or `.video` |
| `CallCapabilities` | `SimpleXChat/CallTypes.swift` | `encryption: Bool` for E2E call encryption support |
| `RcvCallInvitation` | `SimpleXChat/CallTypes.swift` | Incoming call: user, contact, callType, sharedKey, callUUID, callTs |
| `WebRTCCallOffer` | `SimpleXChat/CallTypes.swift` | SDP offer with call type and WebRTC session data |
| `WebRTCSession` | `SimpleXChat/CallTypes.swift` | `rtcSession` (SDP) and `rtcIceCandidates` (serialized) |
| `WebRTCExtraInfo` | `SimpleXChat/CallTypes.swift` | Additional ICE candidates sent after initial offer/answer |
| `WebRTCCallStatus` | `SimpleXChat/CallTypes.swift` | Call lifecycle states for status reporting |
| `CallMediaSource` | `SimpleXChat/CallTypes.swift` | `.mic`, `.camera`, `.screenAudio`, `.screenVideo`, `.unknown` |
| `VideoCamera` | `SimpleXChat/CallTypes.swift` | `.user` (front) or `.environment` (rear) camera |

## Error Cases

| Error | Cause | Handling |
|-------|-------|----------|
| Chat not ready on CallKit answer | App suspended, slow startup | `waitUntilChatStarted` with 30s timeout; `action.fail()` on timeout |
| Call invitation not found | Race condition between notification and event processing | `justRefreshCallInvitations()` retry |
| WebRTC peer connection failure | NAT traversal, network issues | Call ends with error status |
| CallKit action fail | Internal state mismatch | `action.fail()` called, call cleaned up |
| No camera/mic permission | User denied permissions | Permission request dialog shown |

## Key Files

| File | Purpose |
|------|---------|
| `Shared/Views/Call/CallController.swift` | CallKit integration, CXProvider delegate, PKPushRegistry, call lifecycle management |
| `Shared/Views/Call/CallManager.swift` | Call state management, starting/answering/ending calls |
| `Shared/Views/Call/WebRTCClient.swift` | WebRTC peer connection, SDP offer/answer, ICE candidate handling |
| `Shared/Views/Call/ActiveCallView.swift` | Active call UI: video renderers, controls, duration |
| `Shared/Views/Call/CallViewRenderers.swift` | WebRTC video rendering surfaces |
| `Shared/Views/Call/IncomingCallView.swift` | Non-CallKit incoming call overlay |
| `Shared/Views/Call/CallAudioDeviceManager.swift` | Audio routing: speaker, earpiece, Bluetooth |
| `Shared/Views/Call/AudioDevicePicker.swift` | Audio device selection UI |
| `Shared/Views/Call/SoundPlayer.swift` | Ringtone and call sound playback |
| `Shared/Views/Call/WebRTC.swift` | WebRTC configuration and utilities |
| `SimpleXChat/CallTypes.swift` | All call-related type definitions |
| `Shared/Model/SimpleXAPI.swift` | Call API functions: `apiSendCallInvitation`, `apiSendCallOffer`, `apiSendCallAnswer`, `apiSendCallExtraInfo`, `apiEndCall`, `apiRejectCall`, `apiCallStatus` |

## Related Specifications

- `apps/ios/product/README.md` -- Product overview: Calls capability
- `apps/ios/product/flows/connection.md` -- Calls require an established direct connection
