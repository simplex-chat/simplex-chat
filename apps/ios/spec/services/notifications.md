# SimpleX Chat iOS -- Push Notification Service

> Technical specification for the notification system: NtfManager, Notification Service Extension (NSE), notification modes, and token lifecycle.
>
> Related specs: [Architecture](../architecture.md) | [API Reference](../api.md) | [Navigation](../client/navigation.md) | [README](../README.md)
> Related product: [Product Overview](../../product/README.md)

**Source:** [`NtfManager.swift`](../../Shared/Model/NtfManager.swift) | [`BGManager.swift`](../../Shared/Model/BGManager.swift) | [`Notifications.swift`](../../SimpleXChat/Notifications.swift) | [`NotificationService.swift`](../../SimpleX NSE/NotificationService.swift)

---

## Table of Contents

1. [Overview](#1-overview)
2. [Notification Modes](#2-notification-modes)
3. [NtfManager](#3-ntfmanager)
4. [Notification Service Extension (NSE)](#4-notification-service-extension)
5. [Token Lifecycle](#5-token-lifecycle)
6. [Notification Categories & Actions](#6-notification-categories--actions)
7. [Badge Management](#7-badge-management)
8. [Background Tasks (BGManager)](#8-background-tasks)

---

## 1. Overview

SimpleX Chat uses a privacy-preserving notification architecture. Because messages are end-to-end encrypted and the notification server never sees message content, the app uses a Notification Service Extension (NSE) to decrypt push payloads on-device before displaying notifications.

```
APNs Push → NSE receives encrypted payload
         → NSE starts Haskell core (own chat_ctrl)
         → NSE decrypts message using stored keys
         → NSE creates UNNotificationContent with decrypted preview
         → iOS displays notification to user
```

The notification system has three modes of operation, allowing users to choose their privacy/convenience tradeoff.

---

## 2. Notification Modes

| Mode | Description | Mechanism |
|------|-------------|-----------|
| **Instant** | Real-time notifications via Apple Push | APNs push triggers NSE, which decrypts and displays |
| **Periodic** | Background fetch every ~20 minutes | `BGAppRefreshTask` wakes app, checks for new messages |
| **Off** | No notifications | User must open app to see messages |

### Configuration

Notification mode is set via:
```swift
ChatCommand.apiRegisterToken(token: DeviceToken, notificationMode: NotificationsMode)
```

`NotificationsMode` enum: `.instant`, `.periodic`, `.off`

The mode is stored in `ChatModel.notificationMode` and persisted in `GroupDefaults`.

---

## 3. NtfManager

**File**: [`Shared/Model/NtfManager.swift`](../../Shared/Model/NtfManager.swift)

Central notification coordinator. Singleton: `NtfManager.shared`.

### [Class Definition](../../Shared/Model/NtfManager.swift#L27)

```swift
class NtfManager: NSObject, UNUserNotificationCenterDelegate, ObservableObject {
    static let shared = NtfManager()
    public var navigatingToChat = false
    private var granted = false
    private var prevNtfTime: Dictionary<ChatId, Date> = [:]
}
```

### Key Responsibilities

| Method | Purpose | Line |
|--------|---------|------|
| [`registerCategories()`](../../Shared/Model/NtfManager.swift#L156) | Registers notification action categories with iOS | [152](../../Shared/Model/NtfManager.swift#L156) |
| [`requestAuthorization()`](../../Shared/Model/NtfManager.swift#L215) | Requests notification permission from user | [210](../../Shared/Model/NtfManager.swift#L215) |
| [`setNtfBadgeCount(_:)`](../../Shared/Model/NtfManager.swift#L264) | Updates app icon badge | [255](../../Shared/Model/NtfManager.swift#L264) |
| [`processNotificationResponse(_:)`](../../Shared/Model/NtfManager.swift#L54) | Handles user interaction with notification | [51](../../Shared/Model/NtfManager.swift#L54) |
| [`notifyContactRequest(_:)`](../../Shared/Model/NtfManager.swift#L239) | Shows contact request notification | [233](../../Shared/Model/NtfManager.swift#L239) |
| [`notifyCallInvitation(_:)`](../../Shared/Model/NtfManager.swift#L258) | Shows incoming call notification | [250](../../Shared/Model/NtfManager.swift#L258) |
| [`notifyMessageReceived(_:)`](../../Shared/Model/NtfManager.swift#L250) | Shows message received notification | [243](../../Shared/Model/NtfManager.swift#L250) |

### [Notification Response Processing](../../Shared/Model/NtfManager.swift#L40)

When user taps a notification:

1. `userNotificationCenter(didReceive:)` delegate method fires
2. If app is active: calls `processNotificationResponse()` immediately
3. If app is inactive: stores in `ChatModel.notificationResponse` for later processing
4. [`processNotificationResponse()`](../../Shared/Model/NtfManager.swift#L54):
   - Extracts `userId` from `userInfo` -- switches user if needed
   - Extracts `chatId` -- navigates to the conversation
   - Handles action identifiers (accept contact, accept/reject call)

### [Rate Limiting](../../Shared/Model/NtfManager.swift#L144)

`prevNtfTime` dictionary prevents notification flooding:
- Each chat has a timestamp of its last notification
- New notifications are suppressed if within `ntfTimeInterval` (1 second) of the previous one for the same chat

---

## 4. Notification Service Extension (NSE)

**File**: [`SimpleX NSE/NotificationService.swift`](../../SimpleX NSE/NotificationService.swift)

### Architecture

The NSE is a separate process that iOS launches when a push notification arrives. It has:
- Its own Haskell runtime instance (`chat_ctrl`)
- Shared database access (via app group container)
- ~30 second execution window per notification
- No access to main app's in-memory state

### [Processing Flow](../../SimpleX NSE/NotificationService.swift#L305)

```
1. didReceive(request:, withContentHandler:)                          L295
   ├── 2. Initialize Haskell core (if not already running)
   │       └── chat_migrate_init_key() with shared DB path            L852
   ├── 3. Decode encrypted notification payload
   │       └── apiGetNtfConns(nonce:, encNtfInfo:)                    L1112
   ├── 4. Fetch and decrypt messages
   │       └── apiGetConnNtfMessages(connMsgReqs:)                    L1129
   ├── 5. Create notification content
   │       ├── Contact name as title
   │       ├── Decrypted message preview as body
   │       └── Thread identifier for grouping
   └── 6. Deliver to content handler
```

### NSE Commands

The NSE uses a subset of the chat API:

| Command | Purpose | Line |
|---------|---------|------|
| [`apiGetNtfConns(nonce:, encNtfInfo:)`](../../SimpleX NSE/NotificationService.swift#L1134) | Decrypt notification connection info | [1111](../../SimpleX NSE/NotificationService.swift#L1134) |
| [`apiGetConnNtfMessages(connMsgReqs:)`](../../SimpleX NSE/NotificationService.swift#L1151) | Fetch messages for notification connections | [1128](../../SimpleX NSE/NotificationService.swift#L1151) |

### Database Coordination

- NSE checks `appStateGroupDefault` before processing
- If main app is `.active`, NSE may skip processing (main app handles notifications directly)
- NSE uses `chat_close_store` / `chat_reopen_store` for safe concurrent access

### [Preview Modes](../../SimpleXChat/Notifications.swift#L66)

`NotificationPreviewMode` controls what the NSE shows:

| Mode | Title | Body |
|------|-------|------|
| `.message` | Contact name | Message text |
| `.contact` | Contact name | "New message" |
| `.hidden` | "SimpleX" | "New message" |

### Key Internal Types

| Type | Purpose | Line |
|------|---------|------|
| [`NSENotificationData`](../../SimpleX NSE/NotificationService.swift#L28) | Enum of possible notification payloads | [25](../../SimpleX NSE/NotificationService.swift#L28) |
| [`NSEThreads`](../../SimpleX NSE/NotificationService.swift#L84) | Concurrency coordinator for multiple NSE instances | [79](../../SimpleX NSE/NotificationService.swift#L84) |
| [`NotificationEntity`](../../SimpleX NSE/NotificationService.swift#L248) | Per-connection processing state | [241](../../SimpleX NSE/NotificationService.swift#L248) |
| [`NotificationService`](../../SimpleX NSE/NotificationService.swift#L291) | Main NSE class (`UNNotificationServiceExtension`) | [282](../../SimpleX NSE/NotificationService.swift#L291) |
| [`NSEChatState`](../../SimpleX NSE/NotificationService.swift#L788) | Singleton managing NSE lifecycle state | [773](../../SimpleX NSE/NotificationService.swift#L788) |

### Key Internal Functions

| Function | Purpose | Line |
|----------|---------|------|
| [`startChat()`](../../SimpleX NSE/NotificationService.swift#L844) | Initializes Haskell core for NSE | [827](../../SimpleX NSE/NotificationService.swift#L844) |
| [`doStartChat()`](../../SimpleX NSE/NotificationService.swift#L870) | Performs actual chat initialization (migration, config) | [851](../../SimpleX NSE/NotificationService.swift#L870) |
| [`activateChat()`](../../SimpleX NSE/NotificationService.swift#L916) | Reactivates suspended chat controller | [897](../../SimpleX NSE/NotificationService.swift#L916) |
| [`suspendChat(_:)`](../../SimpleX NSE/NotificationService.swift#L930) | Suspends chat controller with timeout | [911](../../SimpleX NSE/NotificationService.swift#L930) |
| [`receiveMessages()`](../../SimpleX NSE/NotificationService.swift#L964) | Main message-receive loop | [943](../../SimpleX NSE/NotificationService.swift#L964) |
| [`receivedMsgNtf(_:)`](../../SimpleX NSE/NotificationService.swift#L1014) | Maps chat events to notification data | [991](../../SimpleX NSE/NotificationService.swift#L1014) |
| [`receiveNtfMessages(_:)`](../../SimpleX NSE/NotificationService.swift#L408) | Orchestrates notification message fetch and delivery | [397](../../SimpleX NSE/NotificationService.swift#L408) |
| [`deliverBestAttemptNtf()`](../../SimpleX NSE/NotificationService.swift#L610) | Delivers the best available notification content | [597](../../SimpleX NSE/NotificationService.swift#L610) |

---

## 5. Token Lifecycle

### Registration Flow

```
1. App starts → AppDelegate.didRegisterForRemoteNotificationsWithDeviceToken
   └── ChatModel.deviceToken = token

2. Token registration (when chat running and token available):
   └── apiRegisterToken(token, notificationMode)
       └── Response: ntfToken(token, status, ntfMode, ntfServer)
           └── ChatModel.tokenStatus = status

3. Token verification (if server requires):
   └── apiVerifyToken(token, nonce, code)
       └── ChatModel.tokenRegistered = true

4. Token check (periodic):
   └── apiCheckToken(token)
       └── Updates ChatModel.tokenStatus
```

### Token States (NtfTknStatus)

| Status | Description |
|--------|-------------|
| `.new` | Token just registered, not yet verified |
| `.registered` | Token registered with notification server |
| `.confirmed` | Token confirmed and ready |
| `.active` | Token actively receiving notifications |
| `.expired` | Token expired, needs re-registration |
| `.invalid` | Token invalid, needs new registration |
| `.invalidBad` | Token invalid due to bad data |
| `.invalidTopic` | Token invalid due to wrong topic |
| `.invalidExpired` | Token invalid because it expired |
| `.invalidUnregistered` | Token invalid, was unregistered |

### Token Deletion

```swift
ChatCommand.apiDeleteToken(token: DeviceToken)
```

Called when:
- User switches to `.off` notification mode
- User deletes their profile
- Token becomes invalid and needs replacement

---

## 6. Notification Categories & Actions

Registered in [`NtfManager.registerCategories()`](../../Shared/Model/NtfManager.swift#L156):

### Contact Request Category

```swift
// Category: "NTF_CAT_CONTACT_REQUEST"
// Actions:
//   - "NTF_ACT_ACCEPT_CONTACT": Accept contact request
```

When user taps "Accept" on a contact request notification:
1. `processNotificationResponse()` detects `ntfActionAcceptContact`
2. Calls `apiAcceptContact(incognito: false, contactReqId:)`
3. Navigates to the new contact's chat

### Call Invitation Category

```swift
// Category: "NTF_CAT_CALL_INVITATION"
// Actions:
//   - "NTF_ACT_ACCEPT_CALL": Accept incoming call
//   - "NTF_ACT_REJECT_CALL": Reject incoming call
```

When user taps "Accept" / "Reject" on a call notification:
1. `processNotificationResponse()` detects the action
2. Sets `ChatModel.ntfCallInvitationAction = (chatId, .accept/.reject)`
3. Call controller picks up the pending action

### Message Category

Standard tap-to-open behavior navigates to the chat.

### Many Events Category

Batch notification for multiple events -- navigates to the app without specific chat context.

---

## 7. Badge Management

The app icon badge shows the total unread message count:

```swift
// Updated when:
// 1. App enters background:
NtfManager.shared.setNtfBadgeCount(chatModel.totalUnreadCountForAllUsers())

// 2. Messages are read:
// Badge is recalculated and updated

// 3. NSE receives notification:
// NSE updates badge based on its count
```

`totalUnreadCountForAllUsers()` sums unread counts across all user profiles (not just the active user).

### NSE Badge Handling

| Method | Purpose | Line |
|--------|---------|------|
| [`setBadgeCount()`](../../SimpleX NSE/NotificationService.swift#L598) | Increments badge via `ntfBadgeCountGroupDefault` | [586](../../SimpleX NSE/NotificationService.swift#L598) |
| [`setNtfBadgeCount(_:)`](../../Shared/Model/NtfManager.swift#L264) | Sets badge on `UIApplication` | [255](../../Shared/Model/NtfManager.swift#L264) |
| [`changeNtfBadgeCount(by:)`](../../Shared/Model/NtfManager.swift#L270) | Adjusts badge by delta | [260](../../Shared/Model/NtfManager.swift#L270) |

---

## 8. Background Tasks

**File**: [`Shared/Model/BGManager.swift`](../../Shared/Model/BGManager.swift)

### [BGManager](../../Shared/Model/BGManager.swift#L30)

```swift
class BGManager {
    static let shared = BGManager()
    func register()    // Register BGAppRefreshTask handlers
    func schedule()    // Schedule next background refresh
}
```

| Method | Purpose | Line |
|--------|---------|------|
| [`register()`](../../Shared/Model/BGManager.swift#L38) | Registers `BGAppRefreshTask` handler with iOS | [35](../../Shared/Model/BGManager.swift#L38) |
| [`schedule()`](../../Shared/Model/BGManager.swift#L46) | Schedules next background refresh request | [42](../../Shared/Model/BGManager.swift#L46) |
| [`handleRefresh(_:)`](../../Shared/Model/BGManager.swift#L74) | Processes background refresh task | [69](../../Shared/Model/BGManager.swift#L74) |
| [`completionHandler(_:)`](../../Shared/Model/BGManager.swift#L95) | Creates completion callback with cleanup | [90](../../Shared/Model/BGManager.swift#L95) |
| [`receiveMessages(_:)`](../../Shared/Model/BGManager.swift#L112) | Activates chat and receives pending messages | [106](../../Shared/Model/BGManager.swift#L112) |

### Background Refresh (Periodic Mode)

When notification mode is `.periodic`:

1. `BGManager.schedule()` is called when app enters background
2. iOS wakes the app in the background approximately every 20 minutes
3. `BGAppRefreshTask` handler:
   - Activates the chat engine: `apiActivateChat(restoreChat: true)`
   - Checks for new messages
   - Creates local notifications for any new messages
   - Suspends chat: `apiSuspendChat(timeoutMicroseconds:)`
   - Schedules next refresh
4. Must complete within ~30 seconds or iOS terminates the task

### Background Task Protection

All API calls use `beginBGTask()` / `endBackgroundTask()` to request extra execution time:

```swift
func beginBGTask(_ handler: (() -> Void)? = nil) -> (() -> Void) {
    var id: UIBackgroundTaskIdentifier!
    // ...
    id = UIApplication.shared.beginBackgroundTask(expirationHandler: endTask)
    return endTask
}
```

Maximum task duration: `maxTaskDuration = 15` seconds.

---

## Notification Content Builders

**File**: [`SimpleXChat/Notifications.swift`](../../SimpleXChat/Notifications.swift)

| Function | Purpose | Line |
|----------|---------|------|
| [`createContactRequestNtf()`](../../SimpleXChat/Notifications.swift#L27) | Builds notification for incoming contact request | [L26](../../SimpleXChat/Notifications.swift#L27) |
| [`createContactConnectedNtf()`](../../SimpleXChat/Notifications.swift#L46) | Builds notification for contact connected event | [L44](../../SimpleXChat/Notifications.swift#L46) |
| [`createMessageReceivedNtf()`](../../SimpleXChat/Notifications.swift#L66) | Builds notification for received message | [L63](../../SimpleXChat/Notifications.swift#L66) |
| [`createCallInvitationNtf()`](../../SimpleXChat/Notifications.swift#L86) | Builds notification for incoming call | [L82](../../SimpleXChat/Notifications.swift#L86) |
| [`createConnectionEventNtf()`](../../SimpleXChat/Notifications.swift#L102) | Builds notification for connection events | [L97](../../SimpleXChat/Notifications.swift#L102) |
| [`createErrorNtf()`](../../SimpleXChat/Notifications.swift#L134) | Builds notification for database/encryption errors | [L128](../../SimpleXChat/Notifications.swift#L134) |
| [`createAppStoppedNtf()`](../../SimpleXChat/Notifications.swift#L160) | Builds notification when app is stopped | [L153](../../SimpleXChat/Notifications.swift#L160) |
| [`createNotification()`](../../SimpleXChat/Notifications.swift#L175) | Generic notification builder (used by all above) | [L167](../../SimpleXChat/Notifications.swift#L175) |
| [`hideSecrets()`](../../SimpleXChat/Notifications.swift#L200) | Redacts secret-formatted text in previews | [L191](../../SimpleXChat/Notifications.swift#L200) |

---

## Source Files

| File | Path |
|------|------|
| Notification manager | [`Shared/Model/NtfManager.swift`](../../Shared/Model/NtfManager.swift) |
| Background manager | [`Shared/Model/BGManager.swift`](../../Shared/Model/BGManager.swift) |
| Notification types | [`SimpleXChat/Notifications.swift`](../../SimpleXChat/Notifications.swift) |
| NSE service | [`SimpleX NSE/NotificationService.swift`](../../SimpleX NSE/NotificationService.swift) |
| App delegate (token) | `Shared/AppDelegate.swift` |
| Notification settings UI | `Shared/Views/UserSettings/NotificationsView.swift` |
