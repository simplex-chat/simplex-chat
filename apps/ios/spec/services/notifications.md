# SimpleX Chat iOS -- Push Notification Service

> Technical specification for the notification system: NtfManager, Notification Service Extension (NSE), notification modes, and token lifecycle.
>
> Related specs: [Architecture](../architecture.md) | [API Reference](../api.md) | [Navigation](../client/navigation.md) | [README](../README.md)
> Related product: [Product Overview](../../product/README.md)

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

**File**: `Shared/Model/NtfManager.swift`

Central notification coordinator. Singleton: `NtfManager.shared`.

### Class Definition

```swift
class NtfManager: NSObject, UNUserNotificationCenterDelegate, ObservableObject {
    static let shared = NtfManager()
    public var navigatingToChat = false
    private var granted = false
    private var prevNtfTime: Dictionary<ChatId, Date> = [:]
}
```

### Key Responsibilities

| Method | Purpose |
|--------|---------|
| `registerCategories()` | Registers notification action categories with iOS |
| `requestAuthorization()` | Requests notification permission from user |
| `setNtfBadgeCount(_:)` | Updates app icon badge |
| `processNotificationResponse(_:)` | Handles user interaction with notification |
| `notifyContactRequest(_:)` | Shows contact request notification |
| `notifyCallInvitation(_:)` | Shows incoming call notification |
| `notifyMessageReceived(_:)` | Shows message received notification |

### Notification Response Processing

When user taps a notification:

1. `userNotificationCenter(didReceive:)` delegate method fires
2. If app is active: calls `processNotificationResponse()` immediately
3. If app is inactive: stores in `ChatModel.notificationResponse` for later processing
4. `processNotificationResponse()`:
   - Extracts `userId` from `userInfo` -- switches user if needed
   - Extracts `chatId` -- navigates to the conversation
   - Handles action identifiers (accept contact, accept/reject call)

### Rate Limiting

`prevNtfTime` dictionary prevents notification flooding:
- Each chat has a timestamp of its last notification
- New notifications are suppressed if within `ntfTimeInterval` (1 second) of the previous one for the same chat

---

## 4. Notification Service Extension (NSE)

**File**: `SimpleX NSE/NotificationService.swift`

### Architecture

The NSE is a separate process that iOS launches when a push notification arrives. It has:
- Its own Haskell runtime instance (`chat_ctrl`)
- Shared database access (via app group container)
- ~30 second execution window per notification
- No access to main app's in-memory state

### Processing Flow

```
1. didReceive(request:, withContentHandler:)
   ├── 2. Initialize Haskell core (if not already running)
   │       └── chat_migrate_init_key() with shared DB path
   ├── 3. Decode encrypted notification payload
   │       └── apiGetNtfConns(nonce:, encNtfInfo:)
   ├── 4. Fetch and decrypt messages
   │       └── apiGetConnNtfMessages(connMsgReqs:)
   ├── 5. Create notification content
   │       ├── Contact name as title
   │       ├── Decrypted message preview as body
   │       └── Thread identifier for grouping
   └── 6. Deliver to content handler
```

### NSE Commands

The NSE uses a subset of the chat API:

| Command | Purpose |
|---------|---------|
| `apiGetNtfConns(nonce:, encNtfInfo:)` | Decrypt notification connection info |
| `apiGetConnNtfMessages(connMsgReqs:)` | Fetch messages for notification connections |

### Database Coordination

- NSE checks `appStateGroupDefault` before processing
- If main app is `.active`, NSE may skip processing (main app handles notifications directly)
- NSE uses `chat_close_store` / `chat_reopen_store` for safe concurrent access

### Preview Modes

`NotificationPreviewMode` controls what the NSE shows:

| Mode | Title | Body |
|------|-------|------|
| `.displayNameAndMessage` | Contact name | Message text |
| `.displayNameOnly` | Contact name | "New message" |
| `.hidden` | "SimpleX" | "New message" |

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
| `.verified` | Token verified and active |
| `.active` | Token actively receiving notifications |
| `.expired` | Token expired, needs re-registration |
| `.invalid` | Token invalid, needs new registration |

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

Registered in `NtfManager.registerCategories()`:

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

---

## 8. Background Tasks

**File**: `Shared/Model/BGManager.swift`

### BGManager

```swift
class BGManager {
    static let shared = BGManager()
    func register()    // Register BGAppRefreshTask handlers
    func schedule()    // Schedule next background refresh
}
```

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

## Source Files

| File | Path |
|------|------|
| Notification manager | `Shared/Model/NtfManager.swift` |
| Background manager | `Shared/Model/BGManager.swift` |
| Notification types | `SimpleXChat/Notifications.swift` |
| NSE service | `SimpleX NSE/NotificationService.swift` |
| App delegate (token) | `Shared/AppDelegate.swift` |
| Notification settings UI | `Shared/Views/UserSettings/NotificationsView.swift` |
