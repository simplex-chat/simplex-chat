# SimpleX Chat iOS -- WebRTC Calling Service

> Technical specification for the calling system: CallController, WebRTCClient, CallKit integration, and signaling via SMP.
>
> Related specs: [Architecture](../architecture.md) | [API Reference](../api.md) | [Notifications](notifications.md) | [README](../README.md)
> Related product: [Chat View](../../product/views/chat.md)

**Source:** [`CallController.swift`](../../Shared/Views/Call/CallController.swift) | [`WebRTCClient.swift`](../../Shared/Views/Call/WebRTCClient.swift) | [`ActiveCallView.swift`](../../Shared/Views/Call/ActiveCallView.swift) | [`CallTypes.swift`](../../SimpleXChat/CallTypes.swift)

---

## Table of Contents

1. [Overview](#1-overview)
2. [CallController](#2-callcontroller)
3. [WebRTCClient](#3-webrtcclient)
4. [Call Flow via SMP](#4-call-flow-via-smp)
5. [CallKit Integration](#5-callkit-integration)
6. [CallKit-Free Mode](#6-callkit-free-mode)
7. [Audio Routing](#7-audio-routing)
8. [Key Types](#8-key-types)
9. [ActiveCallView](#9-activecallview)

---

## 1. Overview

SimpleX Chat provides end-to-end encrypted audio and video calls using WebRTC. The unique aspect is that all call signaling (SDP offers/answers, ICE candidates) is transmitted through the same encrypted SMP messaging channels used for chat, eliminating the need for a separate signaling server.

```
Caller                          SMP Relay              Callee
  │                               │                      │
  ├─ apiSendCallInvitation ──────→│──── push/event ──────→│
  │                               │                      │
  │                               │←── apiSendCallOffer ──┤
  │←── ChatEvent.callOffer ───────│                      │
  │                               │                      │
  ├─ apiSendCallAnswer ──────────→│──── callAnswer ──────→│
  │                               │                      │
  │←── callExtraInfo (ICE) ───────│←── apiSendCallExtraInfo│
  ├─ apiSendCallExtraInfo ───────→│──── callExtraInfo ───→│
  │                               │                      │
  │◄══════════ WebRTC P2P Media Stream ═══════════════════►│
  │                               │                      │
  ├─ apiEndCall ─────────────────→│──── callEnded ───────→│
```

---

## [2. CallController](../../Shared/Views/Call/CallController.swift#L18-L440)

**File**: `Shared/Views/Call/CallController.swift`

Central call coordinator that bridges SimpleX call protocol with iOS CallKit (or non-CallKit fallback).

### [Class Definition](../../Shared/Views/Call/CallController.swift#L18-L47)

```swift
class CallController: NSObject, CXProviderDelegate, PKPushRegistryDelegate, ObservableObject {
    static let shared = CallController()
    static let isInChina = SKStorefront().countryCode == "CHN"
    static func useCallKit() -> Bool { !isInChina && callKitEnabledGroupDefault.get() }

    private let provider: CXProvider           // CallKit provider
    private let controller: CXCallController   // CallKit controller
    private let callManager: CallManager       // Internal call state
    private let registry: PKPushRegistry       // VoIP push registration

    @Published var activeCallInvitation: RcvCallInvitation?
    var shouldSuspendChat: Bool = false
    var fulfillOnConnect: CXAnswerCallAction? = nil
}
```

### Key Responsibilities

| Method | Purpose | Line |
|--------|---------|------|
| [`reportNewIncomingCall()`](../../Shared/Views/Call/CallController.swift#L280) | Reports incoming call to CallKit for native UI | L280 |
| [`reportOutgoingCall()`](../../Shared/Views/Call/CallController.swift#L320) | Reports outgoing call to CallKit | L320 |
| [`provider(_:perform: CXAnswerCallAction)`](../../Shared/Views/Call/CallController.swift#L63) | Handles user answering via CallKit UI | L63 |
| [`provider(_:perform: CXEndCallAction)`](../../Shared/Views/Call/CallController.swift#L92) | Handles user ending via CallKit UI | L92 |
| [`provider(_:perform: CXStartCallAction)`](../../Shared/Views/Call/CallController.swift#L53) | Handles outgoing call start | L53 |
| [`pushRegistry(_:didReceiveIncomingPushWith:)`](../../Shared/Views/Call/CallController.swift#L196) | Handles VoIP push tokens | L196 |
| [`hasActiveCalls()`](../../Shared/Views/Call/CallController.swift#L426) | Checks if any calls are active | L426 |

### Call Manager (internal)

`CallManager` tracks call state internally:
- Maps call UUIDs to `Call` objects
- Handles call state transitions
- Coordinates between CallKit actions and SimpleX API calls

---

## [3. WebRTCClient](../../Shared/Views/Call/WebRTCClient.swift#L12-L665)

**File**: `Shared/Views/Call/WebRTCClient.swift` (~49KB)

Manages the WebRTC peer connection, media streams, and data channels.

### Responsibilities

- Creates and configures `RTCPeerConnection`
- Manages local audio/video capture (`RTCCameraVideoCapturer`, `RTCAudioTrack`)
- Handles SDP offer/answer creation and application
- Processes ICE candidates
- Manages media stream encryption

### Key Operations

| Operation | Description | Line |
|-----------|-------------|------|
| [`initializeCall`](../../Shared/Views/Call/WebRTCClient.swift#L91) | Sets up peer connection, tracks, encryption | L91 |
| [`createPeerConnection`](../../Shared/Views/Call/WebRTCClient.swift#L136) | Creates and configures RTCPeerConnection | L136 |
| [`sendCallCommand`](../../Shared/Views/Call/WebRTCClient.swift#L171) | Dispatches WCallCommand (offer/answer/ICE) | L171 |
| [`addIceCandidates`](../../Shared/Views/Call/WebRTCClient.swift#L161) | `peerConnection.add(RTCIceCandidate)` | L161 |
| [`getInitialIceCandidates`](../../Shared/Views/Call/WebRTCClient.swift#L280) | Collects initial ICE candidates | L280 |
| [`sendIceCandidates`](../../Shared/Views/Call/WebRTCClient.swift#L299) | Sends gathered ICE candidates | L299 |
| [`enableMedia`](../../Shared/Views/Call/WebRTCClient.swift#L358) | Enable/disable audio or video track | L358 |
| [`setupLocalTracks`](../../Shared/Views/Call/WebRTCClient.swift#L415) | Creates audio/video tracks and adds to connection | L415 |
| [`startCaptureLocalVideo`](../../Shared/Views/Call/WebRTCClient.swift#L571) | Front/back camera toggle and capture start | L571 |
| [`endCall`](../../Shared/Views/Call/WebRTCClient.swift#L634) | Tears down connection and tracks | L634 |

### [Additional Encryption](../../Shared/Views/Call/WebRTCClient.swift#L494-L537)

Beyond WebRTC's built-in SRTP encryption, SimpleX adds an extra encryption layer:
- A shared key from the E2E SMP channel is used
- Applied via `chat_encrypt_media` / `chat_decrypt_media` C FFI functions
- Each media frame is encrypted/decrypted with this additional key
- Provides defense-in-depth even if SRTP is compromised

---

## 4. Call Flow via SMP

All call signaling travels through the same encrypted SMP message channels used for chat. No separate signaling server is needed.

### Outgoing Call (Caller Side)

```
1. User initiates call
   └── apiSendCallInvitation(contact:, callType:)
       └── Sends CallInvitation via SMP to contact

2. Callee accepts, sends SDP offer
   └── ChatEvent.callOffer received
       └── WebRTCClient creates answer
           └── apiSendCallAnswer(contact:, answer:)

3. ICE candidates exchanged
   └── ChatEvent.callExtraInfo received → WebRTCClient.addIceCandidate()
   └── WebRTCClient generates candidates → apiSendCallExtraInfo(contact:, extraInfo:)

4. P2P connection established
   └── Media streams flowing

5. End call
   └── apiEndCall(contact:)
```

### Incoming Call (Callee Side)

```
1. ChatEvent.callInvitation received (or push notification)
   └── CallController reports to CallKit (or shows in-app notification)

2. User accepts
   └── WebRTCClient creates SDP offer (callee creates offer in SimpleX protocol)
       └── apiSendCallOffer(contact:, callOffer:)

3. Caller sends answer
   └── ChatEvent.callAnswer received
       └── WebRTCClient.setRemoteDescription(answer)

4. ICE candidates exchanged (same as above)

5. P2P connection established
```

### API Commands

| Command | Direction | Purpose |
|---------|-----------|---------|
| `apiSendCallInvitation(contact:, callType:)` | Caller -> Callee | Initiate call |
| `apiRejectCall(contact:)` | Callee -> Caller | Reject call |
| `apiSendCallOffer(contact:, callOffer:)` | Callee -> Caller | Send SDP offer |
| `apiSendCallAnswer(contact:, answer:)` | Caller -> Callee | Send SDP answer |
| `apiSendCallExtraInfo(contact:, extraInfo:)` | Both | Send ICE candidates |
| `apiEndCall(contact:)` | Either | End call |
| `apiGetCallInvitations` | -- | Get pending invitations |
| `apiCallStatus(contact:, callStatus:)` | -- | Report status change |

---

## [5. CallKit Integration](../../Shared/Views/Call/CallController.swift#L23-L150)

CallKit provides the native iOS incoming call experience (lock screen UI, call history, system call handling).

### [CXProvider Configuration](../../Shared/Views/Call/CallController.swift#L23-L36)

```swift
let configuration = CXProviderConfiguration()
configuration.supportsVideo = true
configuration.supportedHandleTypes = [.generic]
configuration.includesCallsInRecents = UserDefaults.standard.bool(
    forKey: DEFAULT_CALL_KIT_CALLS_IN_RECENTS
)
configuration.maximumCallGroups = 1
configuration.maximumCallsPerCallGroup = 1
configuration.iconTemplateImageData = UIImage(named: "icon-transparent")?.pngData()
```

### [VoIP Push (PKPushRegistry)](../../Shared/Views/Call/CallController.swift#L192-L278)

CallKit requires VoIP push for incoming calls on locked device:
- `PKPushRegistry` registers for `.voIP` push type
- VoIP push token is separate from regular APNs token
- When VoIP push received, **must** report an incoming call to CallKit within the callback (iOS requirement)

### CallKit Actions

| CXAction | Handler | Description | Line |
|----------|---------|-------------|------|
| `CXStartCallAction` | [`provider(_:perform:)`](../../Shared/Views/Call/CallController.swift#L53) | User starts outgoing call | L53 |
| `CXAnswerCallAction` | [`provider(_:perform:)`](../../Shared/Views/Call/CallController.swift#L63) | User answers incoming call from CallKit UI | L63 |
| `CXEndCallAction` | [`provider(_:perform:)`](../../Shared/Views/Call/CallController.swift#L92) | User ends call from CallKit UI | L92 |
| `CXSetMutedCallAction` | [`provider(_:perform:)`](../../Shared/Views/Call/CallController.swift#L107) | User mutes from CallKit UI | L107 |

### [Lock Screen Answer](../../Shared/Views/Call/CallController.swift#L63-L91)

When answering from the lock screen:
1. `CXAnswerCallAction` fires
2. CallController waits for chat to be ready ([`waitUntilChatStarted(timeoutMs: 30_000)`](../../Shared/Views/Call/CallController.swift#L178))
3. WebRTC connection established
4. `fulfillOnConnect` action is fulfilled only when WebRTC reaches connected state (required for audio to work on lock screen)

---

## [6. CallKit-Free Mode](../../Shared/Views/Call/CallController.swift#L20-L21)

In regions where CallKit is unavailable (e.g., China, determined by `SKStorefront.countryCode == "CHN"`), the app falls back to in-app notifications:

```swift
static let isInChina = SKStorefront().countryCode == "CHN"
static func useCallKit() -> Bool { !isInChina && callKitEnabledGroupDefault.get() }
```

### Non-CallKit Behavior
- Incoming calls shown as in-app banners (via `CallController.activeCallInvitation`)
- No lock screen call UI
- No system call integration
- User can also manually disable CallKit via settings (`callKitEnabledGroupDefault`)

---

## [7. Audio Routing](../../Shared/Views/Call/WebRTCClient.swift#L886-L994)

### [AVAudioSession Management](../../Shared/Views/Call/WebRTCClient.swift#L886-L939)

Audio routing is managed through `AVAudioSession`:
- **Receiver**: Default for audio-only calls (ear speaker)
- **Speaker**: For video calls or when user toggles speaker
- **Bluetooth**: Detected and used when available
- **Headphones**: Detected and used when connected

### Route Change Handling

The `WebRTCClient` observes `AVAudioSession.routeChangeNotification` to handle:
- Bluetooth device connection/disconnection
- Headphone plug/unplug
- Speaker/receiver toggle

---

## [8. Key Types](../../SimpleXChat/CallTypes.swift#L1-L110)

### [RcvCallInvitation](../../SimpleXChat/CallTypes.swift#L41-L67)

```swift
struct RcvCallInvitation {
    var user: User
    var contact: Contact
    var callType: CallType
    var sharedKey: String?       // Optional E2E encryption key
    var callUUID: String?
    var callTs: Date
}
```

### [CallType](../../SimpleXChat/CallTypes.swift#L69-L77)

```swift
struct CallType {
    var media: CallMediaType     // .audio or .video
    var capabilities: CallCapabilities
}

enum CallMediaType: String {
    case audio
    case video
}
```

### [WebRTCCallOffer](../../SimpleXChat/CallTypes.swift#L13-L21) / [WebRTCSession](../../SimpleXChat/CallTypes.swift#L23-L31)

```swift
struct WebRTCCallOffer {
    var callType: CallType
    var rtcSession: WebRTCSession
}

struct WebRTCSession {
    var rtcSession: String       // SDP string
    var rtcIceCandidates: String // ICE candidates JSON
}
```

### [WebRTCExtraInfo](../../SimpleXChat/CallTypes.swift#L33-L39)

```swift
struct WebRTCExtraInfo {
    var rtcIceCandidates: String  // Additional ICE candidates
}
```

### Call (Active Call State)

Stored in `ChatModel.activeCall`:
- Contact reference
- Call UUID
- Call state (enum: `.waitCapabilities`, `.invitationAccepted`, `.offerSent`, `.answerReceived`, `.connected`, etc.)
- Media type
- WebRTCClient reference

---

## [9. ActiveCallView](../../Shared/Views/Call/ActiveCallView.swift#L15-L284)

**File**: `Shared/Views/Call/ActiveCallView.swift`

Full-screen call UI when `ChatModel.showCallView == true`:

### UI Elements
- Remote video (full screen background)
- Local video (PiP corner, draggable)
- Contact name and call duration
- Control buttons: mute, camera toggle, speaker toggle, camera flip, end call
- Minimize button (collapses to banner)

### [ActiveCallOverlay](../../Shared/Views/Call/ActiveCallView.swift#L286-L514)

| Control | Method | Line |
|---------|--------|------|
| Audio call info | [`audioCallInfoView`](../../Shared/Views/Call/ActiveCallView.swift#L354) | L354 |
| Video call info | [`videoCallInfoView`](../../Shared/Views/Call/ActiveCallView.swift#L374) | L374 |
| End call | [`endCallButton`](../../Shared/Views/Call/ActiveCallView.swift#L403) | L403 |
| Mute toggle | [`toggleMicButton`](../../Shared/Views/Call/ActiveCallView.swift#L414) | L414 |
| Audio device | [`audioDeviceButton`](../../Shared/Views/Call/ActiveCallView.swift#L424) | L424 |
| Speaker toggle | [`toggleSpeakerButton`](../../Shared/Views/Call/ActiveCallView.swift#L448) | L448 |
| Camera toggle | [`toggleCameraButton`](../../Shared/Views/Call/ActiveCallView.swift#L460) | L460 |
| Flip camera | [`flipCameraButton`](../../Shared/Views/Call/ActiveCallView.swift#L471) | L471 |

### PiP (Picture-in-Picture)

When `ChatModel.activeCallViewIsCollapsed == true`:
- Call view collapses to a small floating overlay
- User can return to full-screen by tapping the banner
- Navigation continues normally underneath

---

## Source Files

| File | Path | Lines |
|------|------|-------|
| [Call controller](../../Shared/Views/Call/CallController.swift) | `Shared/Views/Call/CallController.swift` | 440 |
| [WebRTC client](../../Shared/Views/Call/WebRTCClient.swift) | `Shared/Views/Call/WebRTCClient.swift` | 1128 |
| [Active call UI](../../Shared/Views/Call/ActiveCallView.swift) | `Shared/Views/Call/ActiveCallView.swift` | 524 |
| WebRTC helpers | `Shared/Views/Call/WebRTC.swift` | |
| [Call types (Swift)](../../SimpleXChat/CallTypes.swift) | `SimpleXChat/CallTypes.swift` | 110 |
| Call types (Haskell) | `../../src/Simplex/Chat/Call.hs` | |
