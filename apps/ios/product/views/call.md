# Audio / Video Call

> **Related spec:** [spec/services/calls.md](../../spec/services/calls.md)

## Purpose

Make and receive end-to-end encrypted audio and video calls over WebRTC. Supports CallKit integration for native iOS call UI, picture-in-picture for video calls, audio device selection, and collapsible call overlay.

## Route / Navigation

- **Entry point (outgoing)**: Tap audio or video call button in `ChatInfoView` action buttons or `ChatView` toolbar
- **Entry point (incoming)**: `IncomingCallView` banner appears at top of screen; or native CallKit UI if enabled
- **Presented by**: `ActiveCallView` is overlaid on the main app view when `chatModel.activeCall` is set
- **Collapsible**: Call view can be collapsed via `chatModel.activeCallViewIsCollapsed` to return to chat while call continues
- **Dismiss**: Call ends when user taps end button or remote party disconnects

## Page Sections

### Incoming Call Banner (`IncomingCallView`)

Displayed as an overlay banner when `CallController.activeCallInvitation` is set:

| Element | Description |
|---|---|
| Profile avatar | User profile image (shown when multiple profiles exist) |
| Call type icon | `video.fill` (green) for video calls, `phone.fill` (green) for audio |
| Call type text | "Audio call" or "Video call" with caller info |
| Caller profile | `ProfilePreview` showing caller name and image |
| Reject button | Red `phone.down.fill` icon -- ends the invitation |
| Ignore button | Neutral `multiply` icon -- dismisses the banner without rejecting |
| Accept button | Green `checkmark` icon -- accepts the call; if another call is active, ends it first |

Sound: Ringtone plays via `SoundPlayer.startRingtone()` while banner is visible (unless call view is already showing).

### Active Call View (`ActiveCallView`)

Full-screen overlay with black background:

| Element | Description |
|---|---|
| Remote video | Full-screen `CallViewRemote` showing remote party's camera feed; tap toggles between `scaleAspectFill` and `scaleAspectFit` |
| Local video preview | Small floating `CallViewLocal` in top-right corner (30% width); shows local camera with rounded corners |
| Call overlay | `ActiveCallOverlay` with call controls (hidden when PiP is active for video calls) |
| Screen keep-on | `AppDelegate.keepScreenOn(true)` prevents screen dimming during calls |

### Call Controls (`ActiveCallOverlay`)

Bottom bar of the active call:

| Control | Description |
|---|---|
| Mute toggle | Microphone on/off |
| Speaker toggle | Speaker/receiver switch |
| Camera switch | Front/back camera toggle (video calls) |
| Video toggle | Enable/disable video during call |
| End call | Red phone-down button to terminate |
| Audio device picker | `AudioDevicePicker` / `CallAudioDeviceManager` for selecting output (receiver, speaker, Bluetooth, AirPods) |

### Picture-in-Picture (PiP)

- When `pipShown == true` and call has video, the call overlay is hidden
- PiP window shows the remote video feed
- User can interact with the app normally while call continues

### CallKit Integration

Managed by `CallController`:

| Feature | Description |
|---|---|
| Native incoming call UI | iOS system call screen for incoming calls (when CallKit is enabled) |
| Call history | Optionally shown in Phone app recents (`DEFAULT_CALL_KIT_CALLS_IN_RECENTS`) |
| System audio routing | CallKit manages audio session configuration |
| Lock screen answering | Call can be answered from lock screen via system UI |

When CallKit is not used, the app falls back to `IncomingCallView` banner.

### WebRTC Client

| Component | Description |
|---|---|
| `WebRTCClient` | Manages peer connection, ICE candidates, media tracks |
| `WebRTC.swift` | Bridge between native code and WebRTC JavaScript via `WKWebView` |
| `CallViewRenderers` | `CallViewLocal` and `CallViewRemote` SwiftUI wrappers for video renderers |

## Loading / Error States

| State | Behavior |
|---|---|
| Permissions required | Prompts for microphone (and camera for video) permissions on first call |
| Connecting | Call overlay shows connecting state; `SoundPlayer` plays connecting tone |
| WebRTC client creation | `createWebRTCClient()` called on appear and when `canConnectCall` changes |
| Call ended | `CallSoundsPlayer.vibrate(long: true)` on disconnect if was connected; audio session reset to `.soloAmbient` |
| Call failed | Call dismissed; WebRTC client cleaned up |
| No call invitation | `IncomingCallView` body is empty when no active invitation |

## Audio Session Management

- During call: Audio session configured for voice chat
- Camera permissions: `AVFoundation.AVCaptureDevice` authorization checked
- Audio device management: `CallAudioDeviceManager` handles routing changes and device enumeration
- Post-call cleanup: Audio session reverted to `.soloAmbient`

## Related Specs

- `spec/services/calls.md` -- Call service specification
- [Chat](chat.md) -- Call buttons in chat navigation bar
- [Contact Info](contact-info.md) -- Call buttons in contact info action row
- [Settings](settings.md) -- Call settings (CallKit, ICE servers, relay policy)

## Source Files

- `Shared/Views/Call/ActiveCallView.swift` -- Main active call view with video renderers and overlay
- `Shared/Views/Call/IncomingCallView.swift` -- Incoming call notification banner
- `Shared/Views/Call/CallController.swift` -- CallKit integration and call lifecycle management
- `Shared/Views/Call/CallManager.swift` -- Call state management and CXProvider delegate
- `Shared/Views/Call/CallAudioDeviceManager.swift` -- Audio device enumeration and routing
- `Shared/Views/Call/AudioDevicePicker.swift` -- Audio output device picker UI
- `Shared/Views/Call/WebRTC.swift` -- WebRTC signaling bridge via WKWebView
- `Shared/Views/Call/WebRTCClient.swift` -- WebRTC peer connection management
- `Shared/Views/Call/CallViewRenderers.swift` -- SwiftUI wrappers for local and remote video views
- `Shared/Views/Call/SoundPlayer.swift` -- Ringtone and call sound playback
