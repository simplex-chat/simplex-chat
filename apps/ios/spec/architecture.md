# SimpleX Chat iOS -- System Architecture

> Technical specification for the iOS app's layered architecture, FFI bridge, event system, and extension model.
>
> Related specs: [README](README.md) | [API Reference](api.md) | [State Management](state.md) | [Database](database.md)
> Related product: [Product Overview](../product/README.md)

**Source:** [`SimpleXApp.swift`](../Shared/SimpleXApp.swift#L1-L181) | [`AppDelegate.swift`](../Shared/AppDelegate.swift#L1-L209) | [`ContentView.swift`](../Shared/ContentView.swift#L1-L504) | [`ChatModel.swift`](../Shared/Model/ChatModel.swift#L1-L1354) | [`SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift#L1-L2897) | [`AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift#L1-L2352) | [`APITypes.swift`](../SimpleXChat/APITypes.swift#L1-L1066) | [`API.swift`](../SimpleXChat/API.swift#L1-L387)

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

## [1. Layered Architecture](../Shared/SimpleXApp.swift#L16-L182)

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
| State | [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L330) | `ChatModel`, `ItemsModel`, `Chat` classes | L330, L72, L1252 |
| API | [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift#L91) | FFI bridge functions | L91 |
| API | [`Shared/Model/AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift#L14) | `ChatCommand`, `ChatResponse`, `ChatEvent` enums | L14, L647, L1050 |
| FFI | [`SimpleXChat/SimpleX.h`](../SimpleXChat/SimpleX.h#L1-L49) | C header declaring Haskell exports | |
| FFI | [`SimpleXChat/APITypes.swift`](../SimpleXChat/APITypes.swift#L26) | `APIResult<R>`, `ChatError`, `ChatCmdProtocol` | L26, L695, L17 |
| Core | `../../src/Simplex/Chat/Controller.hs` | Haskell command processor — see `processCommand` in `Controller.hs` | |

---

## [2. FFI Bridge](../SimpleXChat/SimpleX.h#L1-L49)

### [C Functions (SimpleX.h)](../SimpleXChat/SimpleX.h#L1-L49)

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
```

### [Swift Bridge Functions (SimpleXAPI.swift)](../Shared/Model/SimpleXAPI.swift#L91-L215)

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
2. [`ChatCommand.cmdString`](../Shared/Model/AppAPITypes.swift#L14) serializes it to a command string (e.g., `"/_send @1 json {...}"`)
3. [`sendSimpleXCmd`](../SimpleXChat/API.swift#L114) passes the string to `chat_send_cmd_retry` via C FFI
4. Haskell core processes the command, returns JSON response string
5. Swift decodes JSON into [`APIResult<R>`](../SimpleXChat/APITypes.swift#L26) where `R: ChatAPIResult`
6. Result is either `.result(R)`, `.error(ChatError)`, or `.invalid(type, json)`

### [Background Task Protection](../Shared/Model/SimpleXAPI.swift#L53-L78)

All FFI calls are wrapped in [`beginBGTask()`](../Shared/Model/SimpleXAPI.swift#L53) / `endBackgroundTask()` to prevent iOS from killing the app mid-operation. The `maxTaskDuration` is 15 seconds.

---

## [3. Event Streaming](../Shared/Model/SimpleXAPI.swift#L2203-L2898)

The Haskell core emits async events (new messages, connection status changes, file progress, etc.) that are not direct responses to commands. These are received via polling:

```
Haskell Core --[chat_recv_msg_wait]--> Swift event loop --> ChatModel update --> SwiftUI re-render
```

The event loop is implemented in [`ChatReceiver`](../Shared/Model/SimpleXAPI.swift#L2203-L2246), and events are dispatched by [`processReceivedMsg`](../Shared/Model/SimpleXAPI.swift#L2248).

### [Event Types (ChatEvent enum)](../Shared/Model/AppAPITypes.swift#L1050-L1124)

Key async events delivered from core to UI:

| Event | Description | Line |
|-------|-------------|------|
| `newChatItems` | New messages received | [L1065](../Shared/Model/AppAPITypes.swift#L1065) |
| `chatItemUpdated` | Message edited by sender | [L1067](../Shared/Model/AppAPITypes.swift#L1067) |
| `chatItemsDeleted` | Messages deleted | [L1069](../Shared/Model/AppAPITypes.swift#L1069) |
| `chatItemReaction` | Reaction added/removed | [L1068](../Shared/Model/AppAPITypes.swift#L1068) |
| `contactConnected` | New contact connected | [L1057](../Shared/Model/AppAPITypes.swift#L1057) |
| `contactUpdated` | Contact profile changed | [L1061](../Shared/Model/AppAPITypes.swift#L1061) |
| `receivedGroupInvitation` | Group invitation received | [L1072](../Shared/Model/AppAPITypes.swift#L1072) |
| `groupMemberUpdated` | Group member info changed | [L1062](../Shared/Model/AppAPITypes.swift#L1062) |
| `callInvitation` | Incoming call | [L1110](../Shared/Model/AppAPITypes.swift#L1110) |
| `chatSuspended` | Core suspended (background) | [L1051](../Shared/Model/AppAPITypes.swift#L1051) |
| `rcvFileComplete` | File download finished | [L1094](../Shared/Model/AppAPITypes.swift#L1094) |
| `sndFileCompleteXFTP` | File upload finished | [L1105](../Shared/Model/AppAPITypes.swift#L1105) |

Events are decoded as [`ChatEvent`](../Shared/Model/AppAPITypes.swift#L1050) enum in `Shared/Model/AppAPITypes.swift` and dispatched to update `ChatModel` / `ItemsModel` properties, triggering SwiftUI view re-renders via `@Published` property observation.

---

## [4. Database Architecture](../SimpleXChat/FileUtils.swift#L61-L276)

Two SQLite databases in the app group container (shared with NSE):

| Database | File | Contents |
|----------|------|----------|
| Chat DB | `simplex_v1_chat.db` | Messages, contacts, groups, profiles, files, tags, preferences |
| Agent DB | `simplex_v1_agent.db` | SMP connections, keys, queues, server info |

Both databases use the `DB_FILE_PREFIX = "simplex_v1"` prefix. The database path is resolved via [`getAppDatabasePath()`](../SimpleXChat/FileUtils.swift#L61) in `SimpleXChat/FileUtils.swift`, which checks `dbContainerGroupDefault` to determine whether to use the app group container or legacy documents directory.

See [Database & Storage specification](database.md) for full details.

---

## [5. App Lifecycle](../Shared/SimpleXApp.swift#L16-L182)

### [Initialization Sequence (SimpleXApp.swift)](../Shared/SimpleXApp.swift#L16-L37)

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
                    │ startChat()                                    SimpleXAPI.swift L2082
                    v
              ┌──────────┐
              │  Active   │ apiActivateChat()                        SimpleXAPI.swift L350
              └─────┬─────┘
                    │ scenePhase == .background
                    v
              ┌──────────┐
              │Background │ apiSuspendChat(timeoutMicroseconds:)     SimpleXAPI.swift L359
              └─────┬─────┘
                    │ scenePhase == .active
                    v
              ┌──────────┐
              │  Active   │ startChatAndActivate()
              └──────────┘
```

### [Scene Phase Handling (SimpleXApp.swift)](../Shared/SimpleXApp.swift#L37-L121)

- **`.active`**: Calls `startChatAndActivate()`, processes pending notification responses, refreshes chat list and call invitations
- **`.background`**: Records authentication timestamp, calls `suspendChat()` (unless CallKit call active), schedules `BGManager` background refresh, updates badge count
- **`.inactive`**: No explicit handling (transitional state)

### CallKit Exception

When a CallKit call is active during backgrounding, chat suspension is deferred (`CallController.shared.shouldSuspendChat = true`) until the call ends, to maintain the WebRTC session.

---

## [6. Extension Architecture](../SimpleX%20NSE/NotificationService.swift#L1-L1206)

### [Notification Service Extension (NSE)](../SimpleX%20NSE/NotificationService.swift#L1-L1206)

The NSE ([`SimpleX NSE/NotificationService.swift`](../SimpleX%20NSE/NotificationService.swift#L1-L1206)) is a separate process that:

1. Receives encrypted push notification payload from APNs
2. Initializes its own Haskell core instance (`chat_ctrl`) with shared database access
3. Decrypts the push payload using stored keys
4. Generates a visible `UNMutableNotificationContent` with the decrypted message preview
5. Delivers the notification to the user

**Database sharing**: Both main app and NSE access the same database files in the app group container (`APP_GROUP_NAME`). Coordination uses file locks to prevent concurrent write conflicts.

**Lifecycle**: The NSE has a ~30-second execution window per notification. It must initialize Haskell RTS, open the database, decrypt, and deliver within this window.

### Share Extension (SE)

The Share Extension (`SimpleX SE/`) allows sharing content (text, images, files) from other apps into SimpleX conversations.

---

## [7. Remote Desktop Control](../Shared/Views/RemoteAccess/ConnectDesktopView.swift#L1-L545)

Optional desktop pairing allows controlling the mobile app from a desktop client:

- **Pairing**: Encrypted QR code scanned by desktop client establishes a session
- **Commands**: [`connectRemoteCtrl`](../Shared/Model/SimpleXAPI.swift#L1599), [`findKnownRemoteCtrl`](../Shared/Model/SimpleXAPI.swift#L1605), [`confirmRemoteCtrl`](../Shared/Model/SimpleXAPI.swift#L1609), [`verifyRemoteCtrlSession`](../Shared/Model/SimpleXAPI.swift#L1615), [`listRemoteCtrls`](../Shared/Model/SimpleXAPI.swift#L1621), [`stopRemoteCtrl`](../Shared/Model/SimpleXAPI.swift#L1627), [`deleteRemoteCtrl`](../Shared/Model/SimpleXAPI.swift#L1631)
- **State**: [`ChatModel.remoteCtrlSession`](../Shared/Model/ChatModel.swift#L387)`: RemoteCtrlSession?` tracks the active session
- **Transport**: Encrypted reverse HTTP transport between mobile and desktop
- **Source**: [`Shared/Views/RemoteAccess/ConnectDesktopView.swift`](../Shared/Views/RemoteAccess/ConnectDesktopView.swift#L1-L545), see `Remote.hs` in `../../src/Simplex/Chat/`

---

## Source Files

| File | Path | Line |
|------|------|------|
| App entry point | [`Shared/SimpleXApp.swift`](../Shared/SimpleXApp.swift#L16) | L16 |
| App delegate | [`Shared/AppDelegate.swift`](../Shared/AppDelegate.swift#L15) | L15 |
| Root view | [`Shared/ContentView.swift`](../Shared/ContentView.swift#L23) | L23 |
| FFI bridge | [`Shared/Model/SimpleXAPI.swift`](../Shared/Model/SimpleXAPI.swift#L91) | L91 |
| Low-level FFI | [`SimpleXChat/API.swift`](../SimpleXChat/API.swift#L114) | L114 |
| App state | [`Shared/Model/ChatModel.swift`](../Shared/Model/ChatModel.swift#L330) | L330 |
| API types | [`Shared/Model/AppAPITypes.swift`](../Shared/Model/AppAPITypes.swift#L14) | L14 |
| Shared types | [`SimpleXChat/APITypes.swift`](../SimpleXChat/APITypes.swift#L26) | L26 |
| C header | [`SimpleXChat/SimpleX.h`](../SimpleXChat/SimpleX.h#L1-L49) | |
| NSE | [`SimpleX NSE/NotificationService.swift`](../SimpleX%20NSE/NotificationService.swift#L1-L1206) | |
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
