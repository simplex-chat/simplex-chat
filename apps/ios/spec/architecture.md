# SimpleX Chat iOS -- System Architecture

> Technical specification for the iOS app's layered architecture, FFI bridge, event system, and extension model.
>
> Related specs: [README](README.md) | [API Reference](api.md) | [State Management](state.md) | [Database](database.md)
> Related product: [Product Overview](../product/README.md)

**Source:** [`SimpleXApp.swift`](../Shared/SimpleXApp.swift#L1-L183) | [`AppDelegate.swift`](../Shared/AppDelegate.swift#L1-L213) | [`ContentView.swift`](../Shared/ContentView.swift#L1-L513) | [`ChatModel.swift`](../Shared/Model/ChatModel.swift#L1-L1373) | [`SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift#L1-L2915) | [`AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift#L1-L2357) | [`APITypes.swift`](../SimpleXChat/APITypes.swift#L1-L1071) | [`API.swift`](../SimpleXChat/API.swift#L1-L388)

---

## Table of Contents

1. [Layered Architecture](#1-layered-architecture)
2. [FFI Bridge](#2-ffi-bridge)
3. [Event Streaming](#3-event-streaming)
4. [Database Architecture](#4-database-architecture)
5. [App Lifecycle](#5-app-lifecycle)
6. [Extension Architecture](#6-extension-architecture)
7. [Remote Desktop Control](#7-remote-desktop-control)

---

## [1. Layered Architecture](../Shared/SimpleXApp.swift#L17-L183)

The app follows a strict layered model where each layer communicates only with its immediate neighbor:

```
┌─────────────────────────────────────────┐
│  SwiftUI Views                          │  Rendering, user interaction
│  (ChatListView, ChatView, ComposeView)  │
├─────────────────────────────────────────┤
│  ChatModel (ObservableObject)           │  App state, @Published properties
│  ItemsModel, Chat, ChatTagsModel        │  Per-chat state, tag filtering
├─────────────────────────────────────────┤
│  SimpleXAPI (FFI Bridge)                │  chatSendCmd/chatApiSendCmd
│  AppAPITypes (ChatCommand/Response)     │  JSON serialization/deserialization
├─────────────────────────────────────────┤
│  C FFI Layer                            │  chat_send_cmd_retry, chat_recv_msg_wait
│  (SimpleX.h, libsimplex.a)             │  Compiled Haskell via GHC cross-compiler
├─────────────────────────────────────────┤
│  Haskell Core (chat_ctrl)               │  Chat logic, chat protocol (x-events),
│  (Simplex.Chat.Controller)              │  database operations, file management
├─────────────────────────────────────────┤
│  simplexmq library (external)           │  SMP/XFTP protocols, SMP Agent,
│  (github.com/simplex-chat/simplexmq)    │  double-ratchet (PQDR), transport (TLS)
└─────────────────────────────────────────┘
```

**Key invariant**: No SwiftUI view directly calls FFI functions. All communication flows through `ChatModel` or dedicated API functions in `SimpleXAPI.swift`.

### Source Files

| Layer | File | Role | Line |
|-------|------|------|------|
| Views | [`Shared/Views/ChatList/ChatListView.swift`](../Shared/Views/ChatList/ChatListView.swift) | Chat list rendering | |
| Views | [`Shared/Views/Chat/ChatView.swift`](../Shared/Views/Chat/ChatView.swift) | Conversation rendering | |
| State | [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L337) | `ChatModel`, `ItemsModel`, `Chat` classes | L337, L74, L1271 |
| API | [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift#L93) | FFI bridge functions | L93 |
| API | [`Shared/Model/AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift#L15) | `ChatCommand`, `ChatResponse`, `ChatEvent` enums | L15, L649, L1055 |
| FFI | [`SimpleXChat/SimpleX.h`](../SimpleXChat/SimpleX.h#L1-L63) | C header declaring Haskell exports | |
| FFI | [`SimpleXChat/APITypes.swift`](../SimpleXChat/APITypes.swift#L27) | `APIResult<R>`, `ChatError`, `ChatCmdProtocol` | L27, L699, L17 |
| Core | `../../src/Simplex/Chat/Controller.hs` | Haskell command processor — see `processCommand` in `Controller.hs` | |

---

## [2. FFI Bridge](../SimpleXChat/SimpleX.h#L1-L63)

### [C Functions (SimpleX.h)](../SimpleXChat/SimpleX.h#L1-L63)

The Haskell core exposes these C functions, declared in `SimpleXChat/SimpleX.h`:

```c
typedef void* chat_ctrl;

// Initialize database, apply migrations, return controller
char *chat_migrate_init_key(char *path, char *key, int keepKey, char *confirm,
                            int backgroundMode, chat_ctrl *ctrl);

// Send command string, return JSON response string
char *chat_send_cmd_retry(chat_ctrl ctl, char *cmd, int retryNum);

// Block until next async event arrives (or timeout)
char *chat_recv_msg_wait(chat_ctrl ctl, int wait);

// Close/reopen database store
char *chat_close_store(chat_ctrl ctl);
char *chat_reopen_store(chat_ctrl ctl);

// Utility: markdown parsing, server validation, password hashing
char *chat_parse_markdown(char *str);
char *chat_parse_server(char *str);
char *chat_password_hash(char *pwd, char *salt);

// File encryption/decryption
char *chat_write_file(chat_ctrl ctl, char *path, char *data, int len);
char *chat_read_file(char *path, char *key, char *nonce);
char *chat_encrypt_file(chat_ctrl ctl, char *fromPath, char *toPath);
char *chat_decrypt_file(char *fromPath, char *key, char *nonce, char *toPath);

// Image resizing
char *chat_resize_image(char *data, int len, int maxSize, int png);
char *chat_resize_image_uri(char *data, int len, int maxSize);
char *chat_resize_image_file(char *path, int maxSize, int png);
char *chat_resize_image_file_uri(char *path, int maxSize);
```

### [Swift Bridge Functions (SimpleXAPI.swift)](../Shared/Model/SimpleXAPI.swift#L93-L221)

```swift
// Synchronous send -- blocks calling thread
func chatSendCmdSync<R: ChatAPIResult>(_ cmd: ChatCommand, bgTask: Bool = true,
    bgDelay: Double? = nil, ctrl: chat_ctrl? = nil) throws -> R                      // L91

// Async send -- dispatches to background
func chatApiSendCmd<R: ChatAPIResult>(_ cmd: ChatCommand, bgTask: Bool = true,
    bgDelay: Double? = nil, ctrl: chat_ctrl? = nil) async -> APIResult<R>             // L215

// Low-level FFI call -- serializes command to string, calls chat_send_cmd_retry, decodes JSON
func sendSimpleXCmd<R: ChatAPIResult>(_ cmd: ChatCmdProtocol, _ ctrl: chat_ctrl?,
    retryNum: Int32 = 0) -> APIResult<R>                                              // SimpleXChat/API.swift L114
```

### Data Flow

1. Swift constructs a `ChatCommand` enum value (e.g., `.apiSendMessages(type:id:scope:live:ttl:composedMessages:)`)
2. [`ChatCommand.cmdString`](../Shared/Model/AppAPITypes.swift#L15) serializes it to a command string (e.g., `"/_send @1 json {...}"`)
3. [`sendSimpleXCmd`](../SimpleXChat/API.swift#L115) passes the string to `chat_send_cmd_retry` via C FFI
4. Haskell core processes the command, returns JSON response string
5. Swift decodes JSON into [`APIResult<R>`](../SimpleXChat/APITypes.swift#L27) where `R: ChatAPIResult`
6. Result is either `.result(R)`, `.error(ChatError)`, or `.invalid(type, json)`

### [Background Task Protection](../Shared/Model/SimpleXAPI.swift#L54-L79)

All FFI calls are wrapped in [`beginBGTask()`](../Shared/Model/SimpleXAPI.swift#L54) / `endBackgroundTask()` to prevent iOS from killing the app mid-operation. The `maxTaskDuration` is 15 seconds.

---

## [3. Event Streaming](../Shared/Model/SimpleXAPI.swift#L2220-L2916)

The Haskell core emits async events (new messages, connection status changes, file progress, etc.) that are not direct responses to commands. These are received via polling:

```
Haskell Core --[chat_recv_msg_wait]--> Swift event loop --> ChatModel update --> SwiftUI re-render
```

The event loop is implemented in [`ChatReceiver`](../Shared/Model/SimpleXAPI.swift#L2220-L2263), and events are dispatched by [`processReceivedMsg`](../Shared/Model/SimpleXAPI.swift#L2266).

### [Event Types (ChatEvent enum)](../Shared/Model/AppAPITypes.swift#L1055-L1129)

Key async events delivered from core to UI:

| Event | Description | Line |
|-------|-------------|------|
| `newChatItems` | New messages received | [L1070](../Shared/Model/AppAPITypes.swift#L1070) |
| `chatItemUpdated` | Message edited by sender | [L1072](../Shared/Model/AppAPITypes.swift#L1072) |
| `chatItemsDeleted` | Messages deleted | [L1074](../Shared/Model/AppAPITypes.swift#L1074) |
| `chatItemReaction` | Reaction added/removed | [L1073](../Shared/Model/AppAPITypes.swift#L1073) |
| `contactConnected` | New contact connected | [L1062](../Shared/Model/AppAPITypes.swift#L1062) |
| `contactUpdated` | Contact profile changed | [L1066](../Shared/Model/AppAPITypes.swift#L1066) |
| `receivedGroupInvitation` | Group invitation received | [L1077](../Shared/Model/AppAPITypes.swift#L1077) |
| `groupMemberUpdated` | Group member info changed | [L1067](../Shared/Model/AppAPITypes.swift#L1067) |
| `callInvitation` | Incoming call | [L1115](../Shared/Model/AppAPITypes.swift#L1115) |
| `chatSuspended` | Core suspended (background) | [L1056](../Shared/Model/AppAPITypes.swift#L1056) |
| `rcvFileComplete` | File download finished | [L1099](../Shared/Model/AppAPITypes.swift#L1099) |
| `sndFileCompleteXFTP` | File upload finished | [L1110](../Shared/Model/AppAPITypes.swift#L1110) |

Events are decoded as [`ChatEvent`](../Shared/Model/AppAPITypes.swift#L1055) enum in `Shared/Model/AppAPITypes.swift` and dispatched to update `ChatModel` / `ItemsModel` properties, triggering SwiftUI view re-renders via `@Published` property observation.

---

## [4. Database Architecture](../SimpleXChat/FileUtils.swift#L70-L294)

Two SQLite databases in the app group container (shared with NSE):

| Database | File | Contents |
|----------|------|----------|
| Chat DB | `simplex_v1_chat.db` | Messages, contacts, groups, profiles, files, tags, preferences |
| Agent DB | `simplex_v1_agent.db` | SMP connections, keys, queues, server info |

Both databases use the `DB_FILE_PREFIX = "simplex_v1"` prefix. The database path is resolved via [`getAppDatabasePath()`](../SimpleXChat/FileUtils.swift#L70) in `SimpleXChat/FileUtils.swift`, which checks `dbContainerGroupDefault` to determine whether to use the app group container or legacy documents directory.

See [Database & Storage specification](database.md) for full details.

---

## [5. App Lifecycle](../Shared/SimpleXApp.swift#L17-L183)

### [Initialization Sequence (SimpleXApp.swift)](../Shared/SimpleXApp.swift#L17-L38)

```swift
// SimpleXApp.init()
1. haskell_init()                    // Initialize Haskell RTS (background queue, sync)
2. UserDefaults.register(defaults:)  // Register app preference defaults
3. setGroupDefaults()                // Sync preferences to app group container
4. setDbContainer()                  // Set database path                          L122
5. BGManager.shared.register()       // Register background task handlers
6. NtfManager.shared.registerCategories()  // Register notification action categories
```

### State Transitions

```
              ┌──────────┐
              │ Launched  │
              └─────┬─────┘
                    │ initChatAndMigrate()
                    v
              ┌──────────┐
              │ DB Setup  │ chat_migrate_init_key()
              └─────┬─────┘
                    │ startChat()                                    SimpleXAPI.swift L2098
                    v
              ┌──────────┐
              │  Active   │ apiActivateChat()                        SimpleXAPI.swift L358
              └─────┬─────┘
                    │ scenePhase == .background
                    v
              ┌──────────┐
              │Background │ apiSuspendChat(timeoutMicroseconds:)     SimpleXAPI.swift L368
              └─────┬─────┘
                    │ scenePhase == .active
                    v
              ┌──────────┐
              │  Active   │ startChatAndActivate()
              └──────────┘
```

### [Scene Phase Handling (SimpleXApp.swift)](../Shared/SimpleXApp.swift#L38-L121)

- **`.active`**: Calls `startChatAndActivate()`, processes pending notification responses, refreshes chat list and call invitations
- **`.background`**: Records authentication timestamp, calls [`suspendChatInBackground()`](../Shared/Model/SuspendChat.swift#L58-L65) unless the desktop session keepalive is running (see [Background Keepalive](#background-keepalive)), updates badge count
- **`.inactive`**: No explicit handling (transitional state)

`suspendChatInBackground()` calls `suspendChat()` and schedules `BGManager` background refresh, unless a CallKit call is active.

### CallKit Exception

When a CallKit call is active during backgrounding, chat suspension is deferred (`CallController.shared.shouldSuspendChat = true`) until the call ends, to maintain the WebRTC session.

### Desktop Session Exception

While [`RemoteCtrlKeepAlive.shared.running`](../Shared/Model/RemoteCtrlKeepAlive.swift#L31) is true, the chat is not suspended when the app enters background, so the connected desktop keeps using it. When the session ends in background, the keepalive restores the local session and calls `suspendChatInBackground()`.

Suspension stops the desktop session in the core without a `remoteCtrlStopped` event, so [`_suspendChat`](../Shared/Model/SuspendChat.swift#L24-L38) calls [`clearRemoteCtrlSession()`](../Shared/Model/SimpleXAPI.swift#L3229-L3236) on the main queue. This covers suspension requested by the share extension and at the end of a CallKit call while a desktop is connected.

---

## [6. Extension Architecture](../SimpleX%20NSE/NotificationService.swift#L1-L1228)

### [Notification Service Extension (NSE)](../SimpleX%20NSE/NotificationService.swift#L1-L1228)

The NSE ([`SimpleX NSE/NotificationService.swift`](../SimpleX%20NSE/NotificationService.swift#L1-L1228)) is a separate process that:

1. Receives encrypted push notification payload from APNs
2. Initializes its own Haskell core instance (`chat_ctrl`) with shared database access
3. Decrypts the push payload using stored keys
4. Generates a visible `UNMutableNotificationContent` with the decrypted message preview
5. Delivers the notification to the user

**Database sharing**: Both main app and NSE access the same database files in the app group container (`APP_GROUP_NAME`). Coordination uses file locks to prevent concurrent write conflicts.

**Lifecycle**: The NSE has a ~30-second execution window per notification. It must initialize Haskell RTS, open the database, decrypt, and deliver within this window.

### Share Extension (SE)

The Share Extension (`SimpleX SE/`) allows sharing content (text, images, files) from other apps into SimpleX conversations.

### Live Activity Extension

The WidgetKit extension (`SimpleX Live Activity/`, iOS 16.1+) only renders the Live Activity of a connected desktop session: [`RemoteCtrlLiveActivity`](../SimpleX%20Live%20Activity/RemoteCtrlLiveActivity.swift#L9-L77) shows "Connected to desktop", the desktop name (omitted when notification previews are hidden) with the connection time, and on iOS 17+ a Disconnect button. It does not access the database or the app group. [`RemoteCtrlActivity.swift`](../Shared/Model/RemoteCtrlActivity.swift) is compiled into both the app and the extension:

- [`RemoteCtrlActivityAttributes`](../Shared/Model/RemoteCtrlActivity.swift#L6-L11): `desktopName: String?`, `connectedAt: Date`; the content state is empty, the activity is never updated.
- [`DisconnectDesktopIntent`](../Shared/Model/RemoteCtrlActivity.swift#L14-L24) (iOS 17+): `LiveActivityIntent`, so the system performs it in the app process, where it calls `RemoteCtrlKeepAlive.shared.disconnect()`. The extension is built with the `LIVE_ACTIVITY_EXTENSION` compilation condition, which removes that call from its copy.

---

## [7. Remote Desktop Control](../Shared/Views/RemoteAccess/ConnectDesktopView.swift#L1-L546)

Optional desktop pairing allows controlling the mobile app from a desktop client:

- **Pairing**: Encrypted QR code scanned by desktop client establishes a session
- **Commands**: [`connectRemoteCtrl`](../Shared/Model/SimpleXAPI.swift#L1748), [`findKnownRemoteCtrl`](../Shared/Model/SimpleXAPI.swift#L1755), [`confirmRemoteCtrl`](../Shared/Model/SimpleXAPI.swift#L1759), [`verifyRemoteCtrlSession`](../Shared/Model/SimpleXAPI.swift#L1765), [`listRemoteCtrls`](../Shared/Model/SimpleXAPI.swift#L1771), [`stopRemoteCtrl`](../Shared/Model/SimpleXAPI.swift#L1777), [`deleteRemoteCtrl`](../Shared/Model/SimpleXAPI.swift#L1781)
- **State**: [`ChatModel.remoteCtrlSession`](../Shared/Model/ChatModel.swift#L455)`: RemoteCtrlSession?` tracks the active session
- **Ending a session**: [`stopRemoteCtrlSession()`](../Shared/Model/SimpleXAPI.swift#L3224-L3227) stops it in the core and calls [`clearRemoteCtrlSession()`](../Shared/Model/SimpleXAPI.swift#L3229-L3236), which restores the local session with [`switchToLocalSession()`](../Shared/Model/SimpleXAPI.swift#L3213-L3222) when the session was connected. The `remoteCtrlStopped` event handler ends sessions stopped by the desktop or by errors.
- **Transport**: Encrypted reverse HTTP transport between mobile and desktop
- **Source**: [`Shared/Views/RemoteAccess/ConnectDesktopView.swift`](../Shared/Views/RemoteAccess/ConnectDesktopView.swift#L1-L546), see `Remote.hs` in `../../src/Simplex/Chat/`

### Desktop Screen Presentation

While a session is connected, the desktop screen (`UserPickerSheet.useFromDesktop`) is the only UI of the app, because the mobile UI is not updated while the desktop uses the chat:

- The sheet cannot be dismissed by swiping (`interactiveDismissDisabled(m.activeRemoteCtrl)`).
- The session ends only by Disconnect (on the screen, in the Live Activity or in the notification), by the desktop, by an error, or by chat suspension.
- When `ConnectDesktopView` disappears while the scene is active (`AppSheetState.shared.scenePhaseActive`), it disconnects as before. When it disappears while the scene is not active, a connected session continues: this happens when SimpleX Lock hides the app content in background.
- [`ChatListView`](../Shared/Views/ChatList/ChatListView.swift#L249-L256) presents the desktop sheet again when it appears while the session is connected, e.g. after unlocking. `ConnectDesktopView.onAppear` keeps a connected session and disconnects a session that did not connect.

### Background Keepalive

[`RemoteCtrlKeepAlive`](../Shared/Model/RemoteCtrlKeepAlive.swift#L28-L97) keeps the app running in background while a desktop session is connected. [`observeSession()`](../Shared/Model/RemoteCtrlKeepAlive.swift#L35-L49), called from `AppDelegate.didFinishLaunching`, ends indicators left by a terminated process and subscribes to `ChatModel.remoteCtrlSession`; the keepalive starts when the session becomes `.connected` and stops when it changes from `.connected`, whichever code path changes it. The audio session, playback and indicator are behind the protocol [`RemoteCtrlKeepAliveServices`](../Shared/Model/RemoteCtrlKeepAlive.swift#L21-L26), implemented by [`RemoteCtrlSystemServices`](../Shared/Model/RemoteCtrlKeepAlive.swift#L99-L269) and replaced by a recorder in `RemoteCtrlKeepAliveTests`.

- **Start** ([`sessionConnected`](../Shared/Model/RemoteCtrlKeepAlive.swift#L66-L72)): skipped while a call is active, because the call owns the audio session. [`startAudio`](../Shared/Model/RemoteCtrlKeepAlive.swift#L107-L136) sets the audio session to `.playback` with `.mixWithOthers` and `prefersNoInterruptionsFromSystemAlerts`, and [`playSilence()`](../Shared/Model/RemoteCtrlKeepAlive.swift#L178-L194) loops a zero-filled buffer with `AVAudioEngine`. The `audio` background mode keeps the app running while it plays. The screen is kept on only when the keepalive could not start, as before.
- **Indicator** ([`showIndicator`](../Shared/Model/RemoteCtrlKeepAlive.swift#L155-L158)): a Live Activity on iOS 16.1+ when Live Activities are enabled, otherwise the notification of the `NTF_CAT_REMOTE_CTRL` category (see [notifications](services/notifications.md#desktop-session-category)). When the system ends the Live Activity (after 8 hours) or the user removes it, the notification is posted instead.
- **Interruptions** ([`audioInterrupted`](../Shared/Model/RemoteCtrlKeepAlive.swift#L207-L222)): when an interruption ends, or when the app runs again after the system suspended it, the audio session is activated and playback restarts; an engine configuration change (route or format change) also restarts playback. When restarting fails, or media services are reset, the `onFailure` closure of `startAudio` calls [`audioFailed`](../Shared/Model/RemoteCtrlKeepAlive.swift#L88-L96), which disconnects the desktop.
- **Stop** ([`sessionEnded`](../Shared/Model/RemoteCtrlKeepAlive.swift#L74-L86)): stops playback, restores the previous audio session configuration, ends the Live Activity and removes the notification. In background it then restores the local session and calls `suspendChatInBackground()`. `AppDelegate.applicationWillTerminate` calls [`stop()`](../Shared/Model/RemoteCtrlKeepAlive.swift#L59-L64).
- **Disconnect** ([`disconnect()`](../Shared/Model/RemoteCtrlKeepAlive.swift#L51-L57)): `stopRemoteCtrlSession()`, used by the Live Activity intent, the notification action and audio failures.

**Threat model**: a verified desktop, including a malicious or stalled one, can keep the app running in background and using network and battery until the user disconnects or the desktop disconnects; it cannot start a session, which requires the user to scan or confirm on the phone in the foreground. The indicator and the "Disconnect" button in the Live Activity are available on the Lock Screen without unlocking: anyone holding the locked phone can end the session, and can see that a desktop is connected; the desktop name is shown only when notification previews are not hidden.

---

## 8. Chat Relay Management

### Overview

Chat relays are SMP servers that forward messages to channel subscribers. They are configured in the Network & Servers settings and selected during channel creation.

### Data Model

| Type | Location | Description |
|------|----------|-------------|
| `UserChatRelay` | `ChatTypes.swift` | Relay server config: chatRelayId, address, name, domains, preset, tested, enabled, deleted |
| `UserOperatorServers.chatRelays` | `AppAPITypes.swift` | Array of `UserChatRelay` per operator |
| `UserServersWarning` | `AppAPITypes.swift` | Enum with `.noChatRelays(user:)` case |
| `ServerSettings.serverWarnings` | `ChatListView.swift` | `[UserServersWarning]` field on `ServerSettings` struct (exposed via `SaveableSettings.servers`) |

### Relay Management Views

| View | File | Description |
|------|------|-------------|
| `ChatRelayView` | `ChatRelayView.swift` | Edit/view relay: name, address, test, enable toggle, delete |
| `ChatRelayViewLink` | `ChatRelayView.swift` | NavigationLink row showing relay status icon + display name |
| `NewChatRelayView` | `ChatRelayView.swift` | Form to add new relay (name + address + test + enable toggle) |
| `ServersWarningView` | `NetworkAndServers.swift` | Orange exclamation triangle + warning text |

### Key Functions

| Function | File | Description |
|----------|------|-------------|
| `addChatRelay(...)` | `ChatRelayView.swift` | Validates name/address, appends to `userServers[nil operator].chatRelays`, calls `validateServers_` |
| `deleteChatRelay(...)` | `ProtocolServersView.swift` | Marks relay as deleted or removes if no `chatRelayId` |
| `validRelayName(_:)` | `ChatRelayView.swift` | Non-empty + valid display name check |
| `validRelayAddress(_:)` | `ChatRelayView.swift` | Parses via `parseSimpleXMarkdown`, validates `.simplexLink(_, .relay, _, _)` |
| `showRelayTestStatus(relay:)` | `ChatRelayView.swift` | ViewBuilder returning checkmark/multiply/clear icons |
| `validateServers_` | `NetworkAndServers.swift` | Extended signature: now accepts optional `Binding<[UserServersWarning]>?`; calls `validateServers` which returns `([UserServersError], [UserServersWarning])` tuple |
| `globalServersWarning(_:)` | `NetworkAndServers.swift` | Extracts `.noChatRelays` warning text for display |
| `bindingForChatRelays(_:_:)` | `NetworkAndServers.swift` | Creates binding for `chatRelays` at operator index |

### Relay Sections in Settings

"Chat relays" sections appear in:
- `OperatorView`: lists relays for the operator, with header and footer
- `YourServersView` (in `ProtocolServersView`): lists relays for non-operator servers, with delete support and "Add server" -> "Chat relay" option

### serverWarnings Plumbing

`Binding<[UserServersWarning]>` is threaded through: `NetworkAndServers` -> `OperatorView` -> `ProtocolServersView` -> `ProtocolServerView` / `NewServerView` / `ScanProtocolServer`. All `validateServers_` calls pass the warnings binding.

---

## Source Files

| File | Path | Line |
|------|------|------|
| App entry point | [`Shared/SimpleXApp.swift`](../Shared/SimpleXApp.swift#L17) | L17 |
| App delegate | [`Shared/AppDelegate.swift`](../Shared/AppDelegate.swift#L15) | L15 |
| Root view | [`Shared/ContentView.swift`](../Shared/ContentView.swift#L24) | L24 |
| FFI bridge | [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift#L93) | L93 |
| Low-level FFI | [`SimpleXChat/API.swift`](../SimpleXChat/API.swift#L115) | L115 |
| App state | [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L337) | L337 |
| API types | [`Shared/Model/AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift#L15) | L15 |
| Shared types | [`SimpleXChat/APITypes.swift`](../SimpleXChat/APITypes.swift#L27) | L27 |
| C header | [`SimpleXChat/SimpleX.h`](../SimpleXChat/SimpleX.h#L1-L63) | |
| NSE | [`SimpleX NSE/NotificationService.swift`](../SimpleX%20NSE/NotificationService.swift#L1-L1228) | |
| Haskell core | `../../src/Simplex/Chat/Controller.hs` — see `processCommand` in `Controller.hs` | |
| Chat protocol (x-events, message envelopes) | `../../src/Simplex/Chat/Protocol.hs` | |

### External: simplexmq Library

The lower-level protocol and encryption layers are in the separate [simplexmq](https://github.com/simplex-chat/simplexmq) library:

| Component | Spec | Implementation |
|-----------|------|----------------|
| SMP protocol | `simplexmq/protocol/simplex-messaging.md` | `simplexmq/src/Simplex/Messaging/Protocol.hs` |
| XFTP protocol | `simplexmq/protocol/xftp.md` | `simplexmq/src/Simplex/FileTransfer/Protocol.hs` |
| SMP Agent (duplex connections) | `simplexmq/protocol/agent-protocol.md` | `simplexmq/src/Simplex/Messaging/Agent.hs` |
| Double ratchet (PQDR) | `simplexmq/protocol/pqdr.md` | `simplexmq/src/Simplex/Messaging/Crypto/Ratchet.hs` |
| Post-quantum KEM (sntrup761) | `simplexmq/protocol/pqdr.md` | `simplexmq/src/Simplex/Messaging/Crypto/SNTRUP761.hs` |
| TLS transport | — | `simplexmq/src/Simplex/Messaging/Transport.hs` |
| File encryption | — | `simplexmq/src/Simplex/Messaging/Crypto/File.hs` |
