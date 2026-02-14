# SimpleX Chat iOS -- WebRTC Calling Service

> Technical specification for the calling system: CallController, WebRTCClient, CallKit integration, and signaling via SMP.
>
> Related specs: [Architecture](../architecture.md) | [API Reference](../api.md) | [Notifications](notifications.md) | [README](../README.md)
> Related product: [Chat View](../../product/views/chat.md)

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

## 2. CallController

**File**: `Shared/Views/Call/CallController.swift`

Central call coordinator that bridges SimpleX call protocol with iOS CallKit (or non-CallKit fallback).

### Class Definition

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

| Method | Purpose |
|--------|---------|
| `reportNewIncomingCall()` | Reports incoming call to CallKit for native UI |
| `reportOutgoingCall()` | Reports outgoing call to CallKit |
| `provider(_:perform: CXAnswerCallAction)` | Handles user answering via CallKit UI |
| `provider(_:perform: CXEndCallAction)` | Handles user ending via CallKit UI |
| `provider(_:perform: CXStartCallAction)` | Handles outgoing call start |
| `pushRegistry(_:didReceiveIncomingPushWith:)` | Handles VoIP push tokens |
| `hasActiveCalls()` | Checks if any calls are active |

### Call Manager (internal)

`CallManager` tracks call state internally:
- Maps call UUIDs to `Call` objects
- Handles call state transitions
- Coordinates between CallKit actions and SimpleX API calls

---

## 3. WebRTCClient

**File**: `Shared/Views/Call/WebRTCClient.swift` (~49KB)

Manages the WebRTC peer connection, media streams, and data channels.

### Responsibilities

- Creates and configures `RTCPeerConnection`
- Manages local audio/video capture (`RTCCameraVideoCapturer`, `RTCAudioTrack`)
- Handles SDP offer/answer creation and application
- Processes ICE candidates
- Manages media stream encryption

### Key Operations

| Operation | Description |
|-----------|-------------|
| Create offer | `peerConnection.offer(for:)` -- generates SDP offer for caller |
| Create answer | `peerConnection.answer(for:)` -- generates SDP answer for callee |
| Set local description | Apply local SDP (offer or answer) |
| Set remote description | Apply remote SDP (received via SMP) |
| Add ICE candidate | `peerConnection.add(RTCIceCandidate)` |
| Toggle audio | Enable/disable local audio track |
| Toggle video | Enable/disable local video track |
| Switch camera | Front/back camera toggle |

### Additional Encryption

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

## 5. CallKit Integration

CallKit provides the native iOS incoming call experience (lock screen UI, call history, system call handling).

### CXProvider Configuration

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

### VoIP Push (PKPushRegistry)

CallKit requires VoIP push for incoming calls on locked device:
- `PKPushRegistry` registers for `.voIP` push type
- VoIP push token is separate from regular APNs token
- When VoIP push received, **must** report an incoming call to CallKit within the callback (iOS requirement)

### CallKit Actions

| CXAction | Handler | Description |
|----------|---------|-------------|
| `CXStartCallAction` | `provider(_:perform:)` | User starts outgoing call |
| `CXAnswerCallAction` | `provider(_:perform:)` | User answers incoming call from CallKit UI |
| `CXEndCallAction` | `provider(_:perform:)` | User ends call from CallKit UI |
| `CXSetMutedCallAction` | `provider(_:perform:)` | User mutes from CallKit UI |

### Lock Screen Answer

When answering from the lock screen:
1. `CXAnswerCallAction` fires
2. CallController waits for chat to be ready (`waitUntilChatStarted(timeoutMs: 30_000)`)
3. WebRTC connection established
4. `fulfillOnConnect` action is fulfilled only when WebRTC reaches connected state (required for audio to work on lock screen)

---

## 6. CallKit-Free Mode

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

## 7. Audio Routing

### AVAudioSession Management

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

## 8. Key Types

### RcvCallInvitation

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

### CallType

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

### WebRTCCallOffer / WebRTCSession

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

### WebRTCExtraInfo

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

## 9. ActiveCallView

**File**: `Shared/Views/Call/ActiveCallView.swift`

Full-screen call UI when `ChatModel.showCallView == true`:

### UI Elements
- Remote video (full screen background)
- Local video (PiP corner, draggable)
- Contact name and call duration
- Control buttons: mute, camera toggle, speaker toggle, camera flip, end call
- Minimize button (collapses to banner)

### PiP (Picture-in-Picture)

When `ChatModel.activeCallViewIsCollapsed == true`:
- Call view collapses to a small floating overlay
- User can return to full-screen by tapping the banner
- Navigation continues normally underneath

---

## Source Files

| File | Path |
|------|------|
| Call controller | `Shared/Views/Call/CallController.swift` |
| WebRTC client | `Shared/Views/Call/WebRTCClient.swift` |
| Active call UI | `Shared/Views/Call/ActiveCallView.swift` |
| WebRTC helpers | `Shared/Views/Call/WebRTC.swift` |
| Call types (Swift) | `SimpleXChat/CallTypes.swift` |
| Call types (Haskell) | `../../src/Simplex/Chat/Call.hs` |
