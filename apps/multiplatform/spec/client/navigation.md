# Navigation Specification

Source: `common/src/commonMain/kotlin/chat/simplex/common/App.kt` (470 lines)

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

<a id="AppScreen"></a>

## 2. AppScreen Composable

**Location:** [`App.kt#L47`](../../common/src/commonMain/kotlin/chat/simplex/common/App.kt#L47)

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

<a id="MainScreen"></a>

## 3. MainScreen

**Location:** [`App.kt#L84`](../../common/src/commonMain/kotlin/chat/simplex/common/App.kt#L84)

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
2. Routes to `AndroidScreen` or `DesktopScreen` based on platform.

### URL Deep Link

On Android, [`processIntent()`](../../android/src/main/java/chat/simplex/app/MainActivity.kt#L144) puts the URI of a VIEW intent into `chatModel.appOpenUrl`. Once onboarding is complete, a `LaunchedEffect` in `MainScreen` ([`App.kt`](../../common/src/commonMain/kotlin/chat/simplex/common/App.kt#L241)) hands it to [`connectIfOpenedViaUri()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L775), which re-queues it while no profile is active.

On desktop the same state is set from three places, for `simplexchat:` app links and `simplex:` connection links alike. [`isAcceptedLink()`](../../common/src/desktopMain/kotlin/chat/simplex/common/AppLinks.kt#L11) accepts either scheme within 8192 bytes in UTF-8, which leaves room for a one-time link with its post-quantum key (about 2 KB) and for a second such key. A link that starts the app arrives as the only argument of [`main()`](../../desktop/src/jvmMain/kotlin/chat/simplex/desktop/Main.kt#L26) on Windows and Linux ([`linkFromArgs()`](../../common/src/desktopMain/kotlin/chat/simplex/common/AppLinks.kt#L14)). A link for an app that is already running starts a second process, which writes it into the single-instance signal file ([`signalRunningInstance()`](../../common/src/desktopMain/kotlin/chat/simplex/common/SingleInstance.kt#L127)); the running instance renames the file before reading and deleting it ([`takeSignal()`](../../common/src/desktopMain/kotlin/chat/simplex/common/SingleInstance.kt#L148)), handles a signal already present when its watcher ([`watchShowSignals()`](../../common/src/desktopMain/kotlin/chat/simplex/common/SingleInstance.kt#L215)) starts unless it predates the lock attempt by more than 2 s ([`deleteStaleSignalFiles()`](../../common/src/desktopMain/kotlin/chat/simplex/common/SingleInstance.kt#L105)), and passes the link to [`openDesktopLink()`](../../common/src/desktopMain/kotlin/chat/simplex/common/AppLinks.kt#L18), which queues it for the active host and shows the window. On macOS the link is an Apple Event handled by [`installOpenUriHandler()`](../../common/src/desktopMain/kotlin/chat/simplex/common/AppLinks.kt#L24). From there `connectIfOpenedViaUri()` handles the link as on Android: a connection link goes to `planAndConnect()`, which asks before connecting. Both schemes are declared by the macOS Info.plist, the deb, AppImage and Flatpak desktop entries, and at runtime by [`registerLinkSchemes()`](../../common/src/desktopMain/kotlin/chat/simplex/common/platform/AppLinkScheme.desktop.kt#L48) (HKCU on Windows, a hidden desktop entry for an AppImage). Only `simplexchat:` is checked: [`appLinkSchemeRegistered()`](../../common/src/desktopMain/kotlin/chat/simplex/common/platform/AppLinkScheme.desktop.kt#L46) decides the badge page's ending through [`badgePageUrl()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/badges/BadgeStore.kt#L53): `app=true` when a link is expected to reach this installation, otherwise `app=desktop` with the Redeem code screen opened beside the browser. [`desktopInstallation()`](../../common/src/desktopMain/kotlin/chat/simplex/common/platform/AppLinkScheme.desktop.kt#L55) tells the installation kind apart. A packaged macOS app and a Flatpak count as registered without a check, since a Flatpak cannot query the host's default handler ([GAP](../../product/gaps.md#gap-08-desktop-app-link-registration)); Windows when the HKCU `simplexchat` command points at this exe ([`registerWindowsSchemes()`](../../common/src/desktopMain/kotlin/chat/simplex/common/platform/AppLinkScheme.desktop.kt#L108)); an AppImage ([`registerAppImageScheme()`](../../common/src/desktopMain/kotlin/chat/simplex/common/platform/AppLinkScheme.desktop.kt#L176)) or deb when its own desktop entry is the default `simplexchat:` handler. An AppImage is not registered ([`onlyOwnerCanReplace()`](../../common/src/desktopMain/kotlin/chat/simplex/common/platform/AppLinkScheme.desktop.kt#L161)) when its file or any directory above it belongs to a user other than this user or root, or has permission bits that let others write it (POSIX ACLs are not inspected), or when its path contains a line break or `%`. An unpackaged run registers nothing.

App links use the app's own scheme, `simplexchat:`, for everything that is not a connection link. `connectIfOpenedViaUri()` branches on that scheme before the connection dispatch, so an app link never reaches `planAndConnect()`. [`isAppLink()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L797) tests the raw prefix, since text that does not parse as a URI is still an app link, and [`openAppLink()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListView.kt#L804) dispatches on its path. A badge link (`simplexchat:/badge/code/<code>`) goes to [`openBadgeLink()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/badges/BadgesRedeemCodeView.kt#L268), which shows `BadgesRedeemLinkView` as a `ModalManager.end` modal with `ModalViewId.BADGE_LINK`. Once the code parses, every modal is closed first, so the screen opens over the chat list, as iOS dismisses every sheet before a link; a code that does not parse gets an alert and closes nothing. The screen names the active profile and asks before redeeming, since any web page can send the link and a profile holds one badge at a time. On *Add badge* it redeems the code into that profile and then shows `BadgesView`; *Cancel* sends nothing. If that profile already shows a badge (read from `BadgeModel` when the screen composes), which core would refuse, the screen does not offer to add it: it says nothing was added, says where a code for another badge is (on the page it was bought on, under *Show code*), and offers *View your badge*; it cannot tell a repeated link for the badge it shows from another badge's code, so it asserts neither. While the code is being redeemed, *Dismiss* or back closes the screen without cancelling anything; if the redemption then succeeds with the screen closed or covered, an alert says the badge was added. Its step is held by the modal rather than remembered by the composition, so rotation and being covered do not reset it. While the screen shows a redemption in flight ([`isBadgeLinkIssuing()`](../../common/src/commonMain/kotlin/chat/simplex/common/views/badges/BadgesRedeemCodeView.kt#L265)), a further app link is ignored. Any other path is a link type from a later version, and gets an alert asking to check for app updates; text that does not parse is dropped. An app link can carry a secret, and nothing on this path logs the URI: `processIntent()` logs nothing, and `connectIfOpenedViaUri()` logs a fixed string.

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

<a id="AndroidScreen"></a>

## 4. Android Layout

**Location:** [`App.kt#L296`](../../common/src/commonMain/kotlin/chat/simplex/common/App.kt#L296)

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

<a id="DesktopScreen"></a>

## 5. Desktop Layout

**Location:** [`App.kt#L410`](../../common/src/commonMain/kotlin/chat/simplex/common/App.kt#L410)

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

**Location:** [`App.kt#L373`](../../common/src/commonMain/kotlin/chat/simplex/common/App.kt#L373)

- When `chatId` is null and no center modals: shows "No selected chat" placeholder.
- When `chatId` is null and center modals open: shows `ModalManager.center`.
- When `chatId` is set: shows `ChatView`.
- Automatically closes center modals when a chat is selected.

### StartPartOfScreen

**Location:** [`App.kt#L352`](../../common/src/commonMain/kotlin/chat/simplex/common/App.kt#L352)

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

<a id="AppLock"></a>

## 7. Authentication Gate

**Location:** [`AppLock.kt#L17`](../../common/src/commonMain/kotlin/chat/simplex/common/AppLock.kt#L17)

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
| `views/WelcomeView.kt` | Step 2: Profile creation (CreateFirstProfile) |
| `views/onboarding/LinkAMobileView.kt` | Desktop: Link a mobile device |
| `views/onboarding/SetupDatabasePassphrase.kt` | Step 2.5: Database passphrase |
| `views/onboarding/ChooseServerOperators.kt` | Step 3: Server operators and conditions |
| `views/onboarding/SetNotificationsMode.kt` | Step 4: Notification setup |
| `views/chatlist/ChatListView.kt` | Chat list (StartPartOfScreen content) |
| `views/chatlist/UserPicker.kt` | User switching panel |
| `views/chat/ChatView.kt` | Chat view (CenterPartOfScreen content) |
| `views/database/DatabaseErrorView.kt` | Database error recovery |
| `views/SplashView.kt` | Splash / loading screen |
| `views/call/CallView.kt` | In-call fullscreen view (ActiveCallView) |
| `views/localauth/PasswordEntry.kt` | Column divider utility (contains VerticalDivider) |
| `AppLinks.kt` (desktopMain) | Desktop app links from launch arguments and macOS Apple Events |
| `platform/AppLinkScheme.desktop.kt` (desktopMain) | Runtime `simplexchat:` registration and its result for the badge page |
