# Notification System

## Table of Contents

1. [Overview](#1-overview)
2. [NtfManager Abstract Class](#2-ntfmanager-abstract-class)
3. [Android Notification Manager](#3-android-notification-manager)
4. [Desktop Notification Manager](#4-desktop-notification-manager)
5. [Android Background Messaging](#5-android-background-messaging)
6. [Notification Privacy](#6-notification-privacy)
7. [Source Files](#7-source-files)

## Executive Summary

SimpleX Chat uses platform-specific notification strategies. The common `NtfManager` abstract class defines the notification contract with shared helper methods for message, contact, and call notifications. Android implements a full notification system with channels, grouped summaries, full-screen call intents, and a foreground service (`SimplexService`) or periodic `WorkManager` tasks for background message fetching. Desktop uses the TwoSlices library (with OS-native fallbacks) for system notifications. Notification privacy is controlled via `NotificationPreviewMode` (MESSAGE, CONTACT, HIDDEN).

---

## 1. Overview

Notifications serve three purposes in SimpleX Chat:

1. **Message notifications** -- alert users to new messages when the app is not focused on the relevant chat.
2. **Call notifications** -- high-priority alerts for incoming WebRTC calls, with full-screen intent support on Android for lock-screen scenarios.
3. **Contact events** -- notifications for contact connection and contact request events.

The architecture uses an abstract `NtfManager` in common code with platform-specific `actual` implementations. On Android, background message delivery requires a foreground service or periodic WorkManager tasks since SimpleX does not use push notifications (no Firebase/APNs dependency for privacy).

---

## 2. NtfManager Abstract Class

[`NtfManager.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt) (139 lines, commonMain)

The global `ntfManager` instance is declared at [line 17](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L17) and initialized by each platform at startup.

### Concrete methods

| Method | Line | Description |
|---|---|---|
| `notifyContactConnected` | [L20](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L20) | Displays "contact connected" notification for a `Contact` |
| `notifyContactRequestReceived` | [L27](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L27) | Shows contact request notification with an "Accept" action button |
| `notifyMessageReceived` | [L38](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L38) | Conditionally shows message notification based on `ntfsEnabled`, `showNotification`, and whether user is viewing that chat |
| `acceptContactRequestAction` | [L51](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L51) | Accepts a contact request from a notification action |
| `openChatAction` | [L59](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L59) | Opens a specific chat from a notification tap, switching user if needed |
| `showChatsAction` | [L74](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L74) | Opens the chat list, switching user if needed |
| `acceptCallAction` | [L88](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L88) | Accepts a call invitation from a notification action |

### Abstract methods

| Method | Line | Description |
|---|---|---|
| `notifyCallInvitation` | [L98](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L98) | Show call notification; returns `true` if notification was shown |
| `displayNotification` | [L102](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L102) | Display a message notification with optional image and action buttons |
| `cancelCallNotification` | [L103](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L103) | Cancel the active call notification |
| `hasNotificationsForChat` | [L99](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L99) | Check if notifications exist for a given chat |
| `cancelNotificationsForChat` | [L100](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L100) | Cancel all notifications for a specific chat |
| `cancelNotificationsForUser` | [L101](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L101) | Cancel all notifications for a user profile |
| `cancelAllNotifications` | [L104](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L104) | Cancel all notifications |
| `showMessage` | [L105](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L105) | Show a simple title+text notification |
| `androidCreateNtfChannelsMaybeShowAlert` | [L107](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L107) | Android-only: create notification channels (triggers permission prompt on Android 13+) |

### Private helpers

- `awaitChatStartedIfNeeded` ([line 109](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L109)): Waits up to 30 seconds for chat initialization (handles database decryption delay).
- `hideSecrets` ([line 122](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt#L122)): Replaces `Format.Secret` formatted text with `"..."` in notification previews.

---

## 3. Android Notification Manager

[`NtfManager.android.kt`](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt) (332 lines)

Implemented as a Kotlin `object` (singleton) in the Android module.

### Notification channels

| Channel | Constant | Importance | Purpose |
|---|---|---|---|
| Messages | `MessageChannel` (`chat.simplex.app.MESSAGE_NOTIFICATION`) | HIGH | All chat message notifications |
| Calls | `CallChannel` (`chat.simplex.app.CALL_NOTIFICATION_2`) | HIGH | Incoming call alerts with custom ringtone and vibration |

Channel creation happens in `createNtfChannelsMaybeShowAlert()` ([line 298](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt#L298)). Old channel IDs (`CALL_NOTIFICATION`, `CALL_NOTIFICATION_1`, `LOCK_SCREEN_CALL_NOTIFICATION`) are explicitly deleted.

### displayNotification (messages)

[Line 102](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt#L102):

- Uses `NotificationCompat.Builder` with `MessageChannel`.
- Groups notifications using `MessageGroup` with `GROUP_ALERT_CHILDREN` behavior.
- Applies rate limiting: silent mode if notification for the same `(userId, chatId)` was shown within 30 seconds (`msgNtfTimeoutMs`).
- Creates a group summary notification ([line 142](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt#L142)) with `setGroupSummary(true)`.
- Content intent uses `TaskStackBuilder` for proper back stack.
- Supports `NotificationAction.ACCEPT_CONTACT_REQUEST` action buttons via `NtfActionReceiver` broadcast receiver.

### notifyCallInvitation

[Line 160](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt#L160):

- Returns `false` (no notification) if app is in foreground -- in-app alert is used instead.
- **Lock screen / screen off**: Uses `setFullScreenIntent` with a `PendingIntent` to `CallActivity`, plus `VISIBILITY_PUBLIC`.
- **Foreground / unlocked**: Uses regular notification with Accept/Reject action buttons and a custom ringtone (`ring_once` raw resource).
- Notification flags include `FLAG_INSISTENT` for repeating sound and vibration.
- Call notification channel vibration pattern: `[250, 250, 0, 2600]` ms.

### Cancel operations

| Method | Line | Description |
|---|---|---|
| `cancelNotificationsForChat` | [L75](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt#L75) | Cancels by `chatId.hashCode()`, cleans up group summary if no children remain |
| `cancelNotificationsForUser` | [L88](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt#L88) | Iterates and cancels all notifications for a given `userId` |
| `cancelCallNotification` | [L261](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt#L261) | Cancels the singleton call notification (`CallNotificationId = -1`) |
| `cancelAllNotifications` | [L265](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt#L265) | Cancels all via `NotificationManager.cancelAll()` |

### NtfActionReceiver

[Line 311](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt#L311): A `BroadcastReceiver` that handles notification action intents:
- `ACCEPT_CONTACT_REQUEST` -- calls `ntfManager.acceptContactRequestAction()`
- `RejectCallAction` -- calls `callManager.endCall()` on the invitation

---

## 4. Desktop Notification Manager

[`NtfManager.desktop.kt`](../../common/src/desktopMain/kotlin/chat/simplex/common/model/NtfManager.desktop.kt) (194 lines)

Implemented as a Kotlin `object` using the [TwoSlices](https://github.com/sshtools/two-slices) library (`Toast` builder API) for cross-platform desktop notifications.

### displayNotification

[Line 97](../../common/src/desktopMain/kotlin/chat/simplex/common/model/NtfManager.desktop.kt#L97):

- Suppresses if `!user.showNotifications`.
- Respects `NotificationPreviewMode` for title and content.
- Calls `displayNotificationViaLib()` ([line 114](../../common/src/desktopMain/kotlin/chat/simplex/common/model/NtfManager.desktop.kt#L114)) which builds a `Toast` with title, content, icon, action buttons, and default action.
- Icon images are written to a temporary PNG file via `prepareIconPath()` ([line 150](../../common/src/desktopMain/kotlin/chat/simplex/common/model/NtfManager.desktop.kt#L150)).
- Default action on click opens the relevant chat via `openChatAction()`.

### notifyCallInvitation

[Line 22](../../common/src/desktopMain/kotlin/chat/simplex/common/model/NtfManager.desktop.kt#L22):

- Returns `false` if the SimpleX window is focused (in-app alert used instead).
- Creates a notification with Accept and Reject action buttons.
- Default click action opens the chat.

### OS-native fallbacks

[Line 162](../../common/src/desktopMain/kotlin/chat/simplex/common/model/NtfManager.desktop.kt#L162): The `displayNotification` private method dispatches based on `desktopPlatform`:

| Platform | Method |
|---|---|
| Linux | `notify-send` command with optional `-i` icon |
| Windows | `SystemTray` with `TrayIcon.displayMessage()` |
| macOS | `osascript -e 'display notification ...'` |

### Notification tracking

Previous notifications are tracked in `prevNtfs: ArrayList<Pair<Pair<Long, ChatId>, Slice>>` with a `Mutex` for thread safety. Cancel operations remove entries from this list.

---

## 5. Android Background Messaging

### 5.1 SimplexService.kt (735 lines)

[`SimplexService.kt`](../../android/src/main/java/chat/simplex/app/SimplexService.kt)

A foreground `Service` that keeps the app process alive for continuous message receiving. This is SimpleX's privacy-preserving alternative to push notifications.

**Service lifecycle:**

- `startService()` ([line 128](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L128)): Waits for database migration, validates DB status, saves service state as STARTED. WakeLock acquisition is commented out -- the app relies on battery optimization whitelisting instead.
- `onDestroy()` ([line 87](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L87)): Releases wakelocks, saves state as STOPPED, sends broadcast to `AutoRestartReceiver` if allowed.
- `onTaskRemoved()` ([line 211](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L211)): Schedules restart via `AlarmManager` when the app is swiped from recents.

**Notification:**

- Channel: `SIMPLEX_SERVICE_NOTIFICATION` with `IMPORTANCE_LOW` and badge disabled ([line 165](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L165)).
- Shows a persistent notification with a "Hide notification" action that opens channel settings.
- Service ID: `6789`.

**Restart mechanisms:**

| Receiver | Line | Trigger |
|---|---|---|
| `StartReceiver` | [L234](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L234) | Device boot (`BOOT_COMPLETED`) |
| `AutoRestartReceiver` | [L253](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L253) | Service destruction |
| `AppUpdateReceiver` | [L261](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L261) | App update (`MY_PACKAGE_REPLACED`) |
| `ServiceStartWorker` | [L283](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L283) | WorkManager one-time task |

**Battery optimization:**

- `isBackgroundAllowed()` ([line 681](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L681)): Checks both `isIgnoringBatteryOptimizations` and `!isBackgroundRestricted`.
- `showBackgroundServiceNoticeIfNeeded()` ([line 430](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L430)): Shows alerts guiding users to disable battery optimization or background restriction. Includes Xiaomi-specific guidance.
- `disableNotifications()` ([line 722](../../android/src/main/java/chat/simplex/app/SimplexService.kt#L722)): Switches mode to OFF, disables receivers, cancels workers.

### 5.2 MessagesFetcherWorker.kt (101 lines)

[`MessagesFetcherWorker.kt`](../../android/src/main/java/chat/simplex/app/MessagesFetcherWorker.kt)

A `CoroutineWorker` used in `PERIODIC` notification mode as an alternative to the persistent foreground service:

- `scheduleWork()` ([line 18](../../android/src/main/java/chat/simplex/app/MessagesFetcherWorker.kt#L18)): Schedules a `OneTimeWorkRequest` with a default 600-second (10 minute) initial delay and 60-second duration. Requires `NetworkType.CONNECTED` constraint.
- `doWork()` ([line 53](../../android/src/main/java/chat/simplex/app/MessagesFetcherWorker.kt#L53)): Skips if `SimplexService` is already running. Initializes chat controller if needed (self-destruct mode). Waits for DB migration. Runs for up to `durationSec` seconds, polling every 5 seconds until no messages have been received for 10 seconds (`WAIT_AFTER_LAST_MESSAGE`).
- Self-rescheduling: Always calls `reschedule()` at the end (creating a chain of one-time tasks that simulate periodic execution).

### 5.3 Notification modes

Defined in [`SimpleXAPI.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt#L7739):

```kotlin
enum class NotificationsMode {
  OFF,      // No background message fetching
  PERIODIC, // WorkManager periodic tasks (MessagesFetcherWorker)
  SERVICE;  // Persistent foreground service (SimplexService)
}
```

Default is `SERVICE`. The `requiresIgnoringBattery` property is `true` for both `SERVICE` and `PERIODIC` modes.

---

## 6. Notification Privacy

Defined in [`ChatModel.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt#L4823):

```kotlin
enum class NotificationPreviewMode {
  MESSAGE,  // Show sender name and message text
  CONTACT,  // Show sender name, generic "new message" text
  HIDDEN;   // Show "Somebody" as sender, generic "new message" text
}
```

Privacy mode affects:
- **Notification title**: `HIDDEN` uses `"Somebody"` instead of contact name.
- **Notification content**: Only `MESSAGE` mode shows actual message text.
- **Large icon**: `HIDDEN` uses the app icon instead of the contact's profile image.
- **Call notifications**: `HIDDEN` hides the caller's name and profile image.

Both Android and Desktop implementations check `appPreferences.notificationPreviewMode.get()` before constructing notification content.

---

## 7. Source Files

| File | Path | Lines | Description |
|---|---|---|---|
| `NtfManager.kt` | [`common/src/commonMain/.../platform/NtfManager.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/platform/NtfManager.kt) | 139 | Abstract notification manager with shared logic |
| `NtfManager.android.kt` | [`android/src/main/java/.../model/NtfManager.android.kt`](../../android/src/main/java/chat/simplex/app/model/NtfManager.android.kt) | 332 | Android notification channels, groups, call intents |
| `NtfManager.desktop.kt` | [`common/src/desktopMain/.../model/NtfManager.desktop.kt`](../../common/src/desktopMain/kotlin/chat/simplex/common/model/NtfManager.desktop.kt) | 194 | Desktop notifications via TwoSlices/OS-native |
| `SimplexService.kt` | [`android/src/main/java/.../SimplexService.kt`](../../android/src/main/java/chat/simplex/app/SimplexService.kt) | 735 | Android foreground service for background messaging |
| `MessagesFetcherWorker.kt` | [`android/src/main/java/.../MessagesFetcherWorker.kt`](../../android/src/main/java/chat/simplex/app/MessagesFetcherWorker.kt) | 101 | WorkManager periodic message fetcher |
| `ChatModel.kt` | [`common/src/commonMain/.../model/ChatModel.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt) | -- | `NotificationPreviewMode` enum (L4818) |
| `SimpleXAPI.kt` | [`common/src/commonMain/.../model/SimpleXAPI.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt) | -- | `NotificationsMode` enum (L7725) |
