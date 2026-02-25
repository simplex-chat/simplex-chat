# Navigation Specification

Source: `common/src/commonMain/kotlin/chat/simplex/common/App.kt` (467 lines)

---

## Table of Contents

1. [Overview](#1-overview)
2. [AppScreen Composable](#2-appscreen-composable)
3. [MainScreen](#3-mainscreen)
4. [Android Layout](#4-android-layout)
5. [Desktop Layout](#5-desktop-layout)
6. [ModalManager](#6-modalmanager)
7. [Authentication Gate](#7-authentication-gate)
8. [Onboarding Flow](#8-onboarding-flow)
9. [Source Files](#9-source-files)

---

## Executive Summary

SimpleX Chat navigation is a platform-adaptive system implemented in `App.kt`. The root `AppScreen` composable applies theming and safe-area insets, delegating to `MainScreen` which acts as a state machine routing between onboarding, authentication, database error, and the main chat interface. Android uses a 2-column sliding layout (`AndroidScreen`), while desktop uses a fixed 3-column layout (`DesktopScreen`). Modal presentation is managed by `ModalManager`, which provides named zones (start, center, end, fullscreen) for layered content. Authentication is gated by `AppLock`, and onboarding follows a linear `OnboardingStage` enum.

---

## 1. Overview

```
AppScreen (line 46)
+-- SimpleXTheme
    +-- Surface
        +-- MainScreen (line 82)
            |-- [Migration in progress]     -> DefaultProgressView
            |-- [Database opening]          -> DefaultProgressView
            |-- [Database error]            -> DatabaseErrorView
            |-- [Encryption check pending]  -> SplashView
            |-- [Onboarding incomplete]     -> AnimatedContent { OnboardingStage views }
            |-- [Onboarding complete]
            |   |-- [Android]
            |   |   +-- AndroidWrapInCallLayout
            |   |       +-- AndroidScreen (line 293)
            |   |           |-- StartPartOfScreen (ChatListView)
            |   |           +-- ChatView (slide-in panel)
            |   +-- [Desktop]
            |       +-- DesktopScreen (line 406)
            |           |-- StartPartOfScreen + UserPicker (left column)
            |           |-- ModalManager.start (overlay on left)
            |           |-- CenterPartOfScreen / ChatView (center column)
            |           +-- ModalManager.end (right column)
            |-- [Unauthorized] -> AuthView / SplashView / PasscodeView
            |-- [Active call] -> ActiveCallView (desktop) / startCallActivity (Android)
            +-- [Incoming call] -> IncomingCallAlertView
```

---

## 2. AppScreen Composable

**Location:** [`App.kt#L47`](App.kt#L47)

```kotlin
@Composable
fun AppScreen()
```

### Responsibilities

1. **Theme application:** Wraps content in `SimpleXTheme` with `Surface` using `MaterialTheme.colors.background`.
2. **Window insets:** Computes safe padding for landscape mode, accounting for display cutouts on both sides. Uses `WindowInsets.safeDrawing` and `WindowInsets.displayCutout` to calculate symmetric padding.
3. **Fullscreen gallery overlay:** When `chatModel.fullscreenGalleryVisible` is true, draws a black rectangle behind content extending into the cutout areas to provide an immersive gallery background.
4. **Delegates to `MainScreen()`.**

---

## 3. MainScreen

**Location:** [`App.kt#L84`](App.kt#L84)

```kotlin
@Composable
fun MainScreen()
```

### State Machine

`MainScreen` evaluates a series of conditions in priority order:

| Priority | Condition | View |
|---|---|---|
| 1 | `onboarding == Step1_SimpleXInfo && migrationState != null` | `SimpleXInfo` (migration in progress) |
| 2 | `dbMigrationInProgress` | `DefaultProgressView("Database migration...")` |
| 3 | `chatDbStatus == null && showInitializationView` | `DefaultProgressView("Opening database...")` |
| 4 | `showChatDatabaseError` | `DatabaseErrorView` |
| 5 | `chatDbEncrypted == null \|\| localUserCreated == null` | `SplashView` |
| 6 | `onboarding == OnboardingComplete` | Platform-specific main screen |
| 7 | Other onboarding stages | `AnimatedContent` with stage-specific views |

### Onboarding Complete Branch (line ~156)

When onboarding is complete:

1. Shows "advertise lock" alert if conditions met (not shown before, LA not enabled, >3 chats, no active call).
2. Sets up clipboard listener.
3. Routes to `AndroidScreen` or `DesktopScreen` based on platform.

### Overlay Layers (bottom of MainScreen)

| Layer | Condition | Content |
|---|---|---|
| `ModalManager.fullscreen` | Android + migration/onboarding | Fullscreen modals |
| `SwitchingUsersView` | User switch in progress | Loading overlay |
| Auth gate | `userAuthorized != true` | `AuthView` or `SplashView` + passcode |
| Active call | `showCallView == true` | `ActiveCallView` (desktop) or call activity (Android) |
| One-time passcode | Always | `ModalManager.fullscreen.showOneTimePasscodeInView` |
| Privacy alerts | Always | `AlertManager.privacySensitive` |
| Incoming call | `activeCallInvitation != null` | `IncomingCallAlertView` |
| Shared alerts | Always | `AlertManager.shared` |

---

## 4. Android Layout

**Location:** [`App.kt#L296`](App.kt#L296)

```kotlin
@Composable
fun AndroidScreen(userPickerState: MutableStateFlow<AnimatedViewState>)
```

### 2-Column Slide Animation

Uses `BoxWithConstraints` to get `maxWidth`, then two `Box` containers:

1. **Left panel (StartPartOfScreen):** Chat list, positioned at `translationX = -offset`.
2. **Right panel (ChatView):** Chat view, positioned at `translationX = maxWidth - offset`.

The `offset` is an `Animatable<Float>`:
- `0f` when no chat is selected (chat list visible).
- `maxWidth.value` when a chat is open (chat view visible).

### Animation Flow

1. `snapshotFlow { chatModel.chatId.value }` detects chat ID changes.
2. When `chatId` becomes null, `onComposed(null)` animates offset to 0.
3. When `ChatView` finishes composing (calls `onComposed(chatId)`), offset animates to `maxWidth`.
4. Animation uses `chatListAnimationSpec()` (standard spring or tween).

### Display Cutout Handling

If the device has a display cutout on horizontal sides (detected via `WindowInsets.displayCutout`), the panels are clipped with `RectangleShape` to prevent the chat list from showing through during transition.

### Call Layout Wrapper

`AndroidWrapInCallLayout` (line ~279) adds a 40dp top padding when an active call is in progress (not in `WaitCapabilities` or `InvitationAccepted` state), with an `ActiveCallInteractiveArea` banner above.

---

## 5. Desktop Layout

**Location:** [`App.kt#L410`](App.kt#L410)

```kotlin
@Composable
fun DesktopScreen(userPickerState: MutableStateFlow<AnimatedViewState>)
```

### 3-Column Layout

| Column | Width | Content |
|---|---|---|
| **Left** | `DEFAULT_START_MODAL_WIDTH * fontSizeSqrtMultiplier` (fixed) | `StartPartOfScreen` (ChatListView) + `UserPicker` overlay |
| **Left overlay** | Same as left column | `ModalManager.start` modals + `SwitchingUsersView` |
| **Center** | `min = DEFAULT_MIN_CENTER_MODAL_WIDTH`, `weight = 1f` (flexible) | `CenterPartOfScreen` (ChatView or "no selected chat" placeholder, or `ModalManager.center`) |
| **Right** | `max = DEFAULT_END_MODAL_WIDTH * fontSizeSqrtMultiplier` (flexible, 0 when empty) | `ModalManager.end` (ChatInfoView, GroupChatInfoView, ChatItemInfoView, etc.) |

### Column Separators

- `VerticalDivider` between left and center columns (always visible).
- `VerticalDivider` between center and right columns (visible when `ModalManager.end.hasModalsOpen()`).

### Click-to-Dismiss Overlay

When the UserPicker is visible or a start modal is open (but no center modal), a full-size clickable overlay covers the center+right area (line ~428). Clicking it closes start modals and hides the UserPicker.

### CenterPartOfScreen

**Location:** [`App.kt#L373`](App.kt#L373)

- When `chatId` is null and no center modals: shows "No selected chat" placeholder.
- When `chatId` is null and center modals open: shows `ModalManager.center`.
- When `chatId` is set: shows `ChatView`.
- Automatically closes center modals when a chat is selected.

### StartPartOfScreen

**Location:** [`App.kt#L352`](App.kt#L352)

Routes between:
- `SetDeliveryReceiptsView` (if `chatModel.setDeliveryReceipts` is true)
- `ChatListView` (normal operation)
- `ShareListView` (when `chatModel.sharedContent` is non-null, i.e., forwarding)

---

## 6. ModalManager

**Location:** `common/src/commonMain/kotlin/chat/simplex/common/views/helpers/ModalView.kt` (line 92)

```kotlin
class ModalManager(private val placement: ModalPlacement?)
```

### Zones

| Zone | Android Behavior | Desktop Behavior |
|---|---|---|
| `start` | Shared (same as all others) | Left column overlay, slides from start |
| `center` | Shared | Center column overlay, replaces ChatView |
| `end` | Shared | Right column, slides from end |
| `fullscreen` | Shared | Fullscreen overlay |

On Android, all four zones point to the same `shared` instance, meaning modals stack in a single overlay. On desktop, each zone is independent with its own `ModalPlacement`.

```kotlin
companion object {
  val start = if (appPlatform.isAndroid) shared else ModalManager(ModalPlacement.START)
  val center = if (appPlatform.isAndroid) shared else ModalManager(ModalPlacement.CENTER)
  val end = if (appPlatform.isAndroid) shared else ModalManager(ModalPlacement.END)
  val fullscreen = if (appPlatform.isAndroid) shared else ModalManager(ModalPlacement.FULLSCREEN)
}
```

### Modal Stack

Each `ModalManager` maintains a stack of `ModalViewHolder` objects with:
- `id: ModalViewId?` -- optional identifier for deduplication
- `animated: Boolean` -- whether to use enter/exit transitions
- `data: ModalData` -- scoped data for the modal
- `modal: @Composable ModalData.(close: () -> Unit) -> Unit` -- the modal content

### Key Methods

| Method | Description |
|---|---|
| `showModal` | Push a simple modal onto the stack |
| `showModalCloseable` | Push a modal with a close callback |
| `showCustomModal` | Push a modal with full control over `ModalView` wrapper |
| `closeModals` | Pop all modals from the stack |
| `closeModalsExceptFirst` | Pop all but the bottom modal |
| `hasModalsOpen()` | Check if any modals are on the stack |
| `showInView` | Render the current modal stack into the composable tree |

### Usage Pattern

| Action | Zone Used |
|---|---|
| Settings, New Chat, User Address | `ModalManager.start` |
| Onboarding conditions, What's New | `ModalManager.center` |
| ChatInfoView, GroupChatInfoView, ChatItemInfoView, GroupMemberInfoView | `ModalManager.end` |
| Passcode entry, Call view, Migration | `ModalManager.fullscreen` |

---

## 7. Authentication Gate

**Location:** [`AppLock.kt#L17`](../../../common/src/commonMain/kotlin/chat/simplex/common/AppLock.kt#L17)

```kotlin
object AppLock {
  val userAuthorized = mutableStateOf<Boolean?>(null)
  val enteredBackground = mutableStateOf<Long?>(null)
  val laFailed = mutableStateOf(false)
}
```

### State

| Field | Type | Description |
|---|---|---|
| `userAuthorized` | `MutableState<Boolean?>` | `null` = not yet determined, `true` = authenticated, `false` = locked |
| `enteredBackground` | `MutableState<Long?>` | Timestamp when app entered background (for lock delay) |
| `laFailed` | `MutableState<Boolean>` | True if last authentication attempt failed |

### Authentication Flow

1. **MainScreen** checks `unauthorized` (derived: `userAuthorized.value != true`) at line ~135.
2. If unauthorized and not in an active call:
   - Launches `AppLock.runAuthenticate()` which triggers platform-specific biometric/passcode prompt.
   - On Android with system auth finishing during activity destruction, authentication is skipped.
3. If `performLA` preference is set and `laFailed` is true: shows `AuthView` with "Unlock" button.
4. If `performLA` is set and `laFailed` is false: shows `SplashView` with passcode overlay.

### Lock Delay

The `laLockDelay` preference controls how long after backgrounding the app requires re-authentication. When `laLockDelay == 0`, screen rotation triggers a 3-second grace period (line ~270) to prevent unnecessary re-auth.

### Lock Modes

- `LAMode.SYSTEM`: Uses Android biometric/system lock screen.
- `LAMode.PASSCODE`: Uses in-app passcode (`SetAppPasscodeView`).

### First-Time Lock Notice

`showLANotice` (line ~33 in `AppLock.kt`) prompts users to enable SimpleX Lock when they have more than 3 chats, have not yet been shown the notice, and have not enabled lock. On Android, it offers a choice between system auth and passcode.

---

## 8. Onboarding Flow

**Location:** `common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/OnboardingView.kt` (line 3)

```kotlin
enum class OnboardingStage {
  Step1_SimpleXInfo,
  Step2_CreateProfile,
  LinkAMobile,
  Step2_5_SetupDatabasePassphrase,
  Step3_ChooseServerOperators,
  Step3_CreateSimpleXAddress,
  Step4_SetNotificationsMode,
  OnboardingComplete
}
```

### Stage Progression

| Stage | View | Next Stage |
|---|---|---|
| `Step1_SimpleXInfo` | `SimpleXInfo` -- app introduction, privacy features | `Step2_CreateProfile` or `LinkAMobile` (desktop) |
| `Step2_CreateProfile` | `CreateFirstProfile` -- display name, optional image | `Step2_5_SetupDatabasePassphrase` or `Step3_ChooseServerOperators` |
| `LinkAMobile` | `LinkAMobile` -- desktop linking to mobile device | `Step2_CreateProfile` |
| `Step2_5_SetupDatabasePassphrase` | `SetupDatabasePassphrase` -- optional DB encryption | `Step3_ChooseServerOperators` |
| `Step3_ChooseServerOperators` | `OnboardingConditionsView` -- server operator selection, T&C | `Step3_CreateSimpleXAddress` or `Step4_SetNotificationsMode` |
| `Step3_CreateSimpleXAddress` | `SetNotificationsMode` (legacy backcompat) | `Step4_SetNotificationsMode` |
| `Step4_SetNotificationsMode` | `SetNotificationsMode` -- notification permission setup | `OnboardingComplete` |
| `OnboardingComplete` | Main app screen | -- |

### Animated Transitions

Onboarding uses `AnimatedContent` with directional transitions:
- Forward: `fromEndToStartTransition` (slide left).
- Backward: `fromStartToEndTransition` (slide right).

The stage value is stored in `appPrefs.onboardingStage` and persisted across app restarts.

---

## 9. Source Files

| File | Description |
|---|---|
| `App.kt` | AppScreen, MainScreen, AndroidScreen, DesktopScreen, StartPartOfScreen, CenterPartOfScreen, EndPartOfScreen |
| `AppLock.kt` | AppLock object, authentication state, lock notice, LA mode selection |
| `views/helpers/ModalView.kt` | ModalManager class, ModalPlacement enum, modal stack management |
| `views/onboarding/OnboardingView.kt` | OnboardingStage enum |
| `views/onboarding/SimpleXInfo.kt` | Step 1: App introduction |
| `views/CreateFirstProfile.kt` | Step 2: Profile creation |
| `views/onboarding/LinkAMobile.kt` | Desktop: Link a mobile device |
| `views/onboarding/SetupDatabasePassphrase.kt` | Step 2.5: Database passphrase |
| `views/onboarding/OnboardingConditionsView.kt` | Step 3: Server operators and conditions |
| `views/onboarding/SetNotificationsMode.kt` | Step 4: Notification setup |
| `views/chatlist/ChatListView.kt` | Chat list (StartPartOfScreen content) |
| `views/chatlist/UserPicker.kt` | User switching panel |
| `views/chat/ChatView.kt` | Chat view (CenterPartOfScreen content) |
| `views/database/DatabaseErrorView.kt` | Database error recovery |
| `views/SplashView.kt` | Splash / loading screen |
| `views/call/ActiveCallView.kt` | In-call fullscreen view |
| `views/localauth/VerticalDivider.kt` | Column divider utility |
