# SimpleX Chat iOS -- System Architecture

> Technical specification for the iOS app's layered architecture, FFI bridge, event system, and extension model.
>
> Related specs: [README](README.md) | [API Reference](api.md) | [State Management](state.md) | [Database](database.md)
> Related product: [Product Overview](../product/README.md)

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

## 1. Layered Architecture

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

| Layer | File | Role |
|-------|------|------|
| Views | `Shared/Views/ChatList/ChatListView.swift` | Chat list rendering |
| Views | `Shared/Views/Chat/ChatView.swift` | Conversation rendering |
| State | `Shared/Model/ChatModel.swift` | `ChatModel`, `ItemsModel`, `Chat` classes |
| API | `Shared/Model/SimpleXAPI.swift` | FFI bridge functions |
| API | `Shared/Model/AppAPITypes.swift` | `ChatCommand`, `ChatResponse`, `ChatEvent` enums |
| FFI | `SimpleXChat/SimpleX.h` | C header declaring Haskell exports |
| FFI | `SimpleXChat/APITypes.swift` | `APIResult<R>`, `ChatError`, `ChatCmdProtocol` |
| Core | `../../src/Simplex/Chat/Controller.hs` | Haskell command processor |

---

## 2. FFI Bridge

### C Functions (SimpleX.h)

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

### Swift Bridge Functions (SimpleXAPI.swift)

```swift
// Synchronous send -- blocks calling thread
func chatSendCmdSync<R: ChatAPIResult>(_ cmd: ChatCommand, bgTask: Bool = true,
    bgDelay: Double? = nil, ctrl: chat_ctrl? = nil) throws -> R

// Async send -- dispatches to background
func chatApiSendCmd<R: ChatAPIResult>(_ cmd: ChatCommand, bgTask: Bool = true,
    bgDelay: Double? = nil, ctrl: chat_ctrl? = nil) async -> APIResult<R>

// Low-level FFI call -- serializes command to string, calls chat_send_cmd_retry, decodes JSON
func sendSimpleXCmd<R: ChatAPIResult>(_ cmd: ChatCmdProtocol, _ ctrl: chat_ctrl?,
    retryNum: Int32 = 0) -> APIResult<R>
```

### Data Flow

1. Swift constructs a `ChatCommand` enum value (e.g., `.apiSendMessages(type:id:scope:live:ttl:composedMessages:)`)
2. `ChatCommand.cmdString` serializes it to a command string (e.g., `"/_send @1 json {...}"`)
3. `sendSimpleXCmd` passes the string to `chat_send_cmd_retry` via C FFI
4. Haskell core processes the command, returns JSON response string
5. Swift decodes JSON into `APIResult<R>` where `R: ChatAPIResult`
6. Result is either `.result(R)`, `.error(ChatError)`, or `.invalid(type, json)`

### Background Task Protection

All FFI calls are wrapped in `beginBGTask()` / `endBackgroundTask()` to prevent iOS from killing the app mid-operation. The `maxTaskDuration` is 15 seconds.

---

## 3. Event Streaming

The Haskell core emits async events (new messages, connection status changes, file progress, etc.) that are not direct responses to commands. These are received via polling:

```
Haskell Core --[chat_recv_msg_wait]--> Swift event loop --> ChatModel update --> SwiftUI re-render
```

### Event Types (ChatEvent enum)

Key async events delivered from core to UI:

| Event | Description |
|-------|-------------|
| `newChatItems` | New messages received |
| `chatItemUpdated` | Message edited by sender |
| `chatItemsDeleted` | Messages deleted |
| `chatItemReaction` | Reaction added/removed |
| `contactConnected` | New contact connected |
| `contactUpdated` | Contact profile changed |
| `receivedGroupInvitation` | Group invitation received |
| `groupMemberUpdated` | Group member info changed |
| `callInvitation` | Incoming call |
| `chatSuspended` | Core suspended (background) |
| `rcvFileComplete` | File download finished |
| `sndFileCompleteXFTP` | File upload finished |

Events are decoded as `ChatEvent` enum in `Shared/Model/AppAPITypes.swift` and dispatched to update `ChatModel` / `ItemsModel` properties, triggering SwiftUI view re-renders via `@Published` property observation.

---

## 4. Database Architecture

Two SQLite databases in the app group container (shared with NSE):

| Database | File | Contents |
|----------|------|----------|
| Chat DB | `simplex_v1_chat.db` | Messages, contacts, groups, profiles, files, tags, preferences |
| Agent DB | `simplex_v1_agent.db` | SMP connections, keys, queues, server info |

Both databases use the `DB_FILE_PREFIX = "simplex_v1"` prefix. The database path is resolved via `getAppDatabasePath()` in `SimpleXChat/FileUtils.swift`, which checks `dbContainerGroupDefault` to determine whether to use the app group container or legacy documents directory.

See [Database & Storage specification](database.md) for full details.

---

## 5. App Lifecycle

### Initialization Sequence (SimpleXApp.swift)

```swift
// SimpleXApp.init()
1. haskell_init()                    // Initialize Haskell RTS (background queue, sync)
2. UserDefaults.register(defaults:)  // Register app preference defaults
3. setGroupDefaults()                // Sync preferences to app group container
4. setDbContainer()                  // Set database path
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
                    │ startChat()
                    v
              ┌──────────┐
              │  Active   │ apiActivateChat()
              └─────┬─────┘
                    │ scenePhase == .background
                    v
              ┌──────────┐
              │Background │ apiSuspendChat(timeoutMicroseconds:)
              └─────┬─────┘
                    │ scenePhase == .active
                    v
              ┌──────────┐
              │  Active   │ startChatAndActivate()
              └──────────┘
```

### Scene Phase Handling (SimpleXApp.swift)

- **`.active`**: Calls `startChatAndActivate()`, processes pending notification responses, refreshes chat list and call invitations
- **`.background`**: Records authentication timestamp, calls `suspendChat()` (unless CallKit call active), schedules `BGManager` background refresh, updates badge count
- **`.inactive`**: No explicit handling (transitional state)

### CallKit Exception

When a CallKit call is active during backgrounding, chat suspension is deferred (`CallController.shared.shouldSuspendChat = true`) until the call ends, to maintain the WebRTC session.

---

## 6. Extension Architecture

### Notification Service Extension (NSE)

The NSE (`SimpleX NSE/NotificationService.swift`) is a separate process that:

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

## 7. Remote Desktop Control

Optional desktop pairing allows controlling the mobile app from a desktop client:

- **Pairing**: Encrypted QR code scanned by desktop client establishes a session
- **Commands**: `connectRemoteCtrl`, `findKnownRemoteCtrl`, `confirmRemoteCtrl`, `verifyRemoteCtrlSession`, `listRemoteCtrls`, `stopRemoteCtrl`, `deleteRemoteCtrl`
- **State**: `ChatModel.remoteCtrlSession: RemoteCtrlSession?` tracks the active session
- **Transport**: Encrypted reverse HTTP transport between mobile and desktop
- **Source**: `Shared/Views/RemoteAccess/ConnectDesktopView.swift`, `../../src/Simplex/Chat/Remote.hs`

---

## Source Files

| File | Path |
|------|------|
| App entry point | `Shared/SimpleXApp.swift` |
| App delegate | `Shared/AppDelegate.swift` |
| Root view | `Shared/ContentView.swift` |
| FFI bridge | `Shared/Model/SimpleXAPI.swift` |
| App state | `Shared/Model/ChatModel.swift` |
| API types | `Shared/Model/AppAPITypes.swift` |
| Shared types | `SimpleXChat/APITypes.swift` |
| C header | `SimpleXChat/SimpleX.h` |
| NSE | `SimpleX NSE/NotificationService.swift` |
| Haskell core | `../../src/Simplex/Chat/Controller.hs` |
| Chat protocol (x-events, message envelopes) | `../../src/Simplex/Chat/Protocol.hs` |

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
