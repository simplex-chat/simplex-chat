# SimpleX Chat iOS -- Navigation Architecture

> Technical specification for the navigation stack, deep linking, sheet presentation, and call overlay.
>
> Related specs: [Chat List](chat-list.md) | [Chat View](chat-view.md) | [State Management](../state.md) | [README](../README.md)
> Related product: [Product Overview](../../product/README.md)

---

## Table of Contents

1. [Overview](#1-overview)
2. [Root View -- ContentView](#2-root-view)
3. [Navigation Stack](#3-navigation-stack)
4. [Sheet Presentation](#4-sheet-presentation)
5. [Deep Linking](#5-deep-linking)
6. [Call Overlay](#6-call-overlay)
7. [Authentication Gate](#7-authentication-gate)
8. [Onboarding Flow](#8-onboarding-flow)

---

## 1. Overview

The app's navigation follows a hierarchical model with a single navigation stack rooted in `ContentView`. Modal sheets and full-screen overlays augment the primary navigation path.

```
SimpleXApp
└── ContentView (root)
    ├── Authentication gate (LocalAuthView / SetAppPasscodeView)
    ├── Onboarding flow (if first launch / migration)
    ├── Main content
    │   └── NavigationStack / NavigationView
    │       ├── ChatListView (root of stack)
    │       │   ├── ChatView (pushed)
    │       │   │   ├── ChatInfoView / GroupChatInfoView (pushed)
    │       │   │   └── ChatItemInfoView (pushed)
    │       │   └── ContactConnectionInfo (pushed)
    │       └── Settings views (pushed)
    ├── Sheets (modal)
    │   ├── UserPicker
    │   ├── NewChatView
    │   ├── WhatsNew / Notices
    │   └── Settings sub-views
    └── Overlays (always on top)
        ├── Active call banner (when call active)
        └── ActiveCallView (full-screen call)
```

---

## 2. Root View -- ContentView

**File**: `Shared/ContentView.swift`

`ContentView` is the root view injected by `SimpleXApp`. It manages:

### Environment
- `@EnvironmentObject var chatModel: ChatModel`
- `@EnvironmentObject var theme: AppTheme`
- `@Environment(\.scenePhase) var scenePhase`

### Key State
| Property | Type | Purpose |
|----------|------|---------|
| `contentAccessAuthenticationExtended` | `Bool` | Passed at init to avoid re-render timing issues |
| `automaticAuthenticationAttempted` | `Bool` | Whether biometric auth was auto-attempted |
| `waitingForOrPassedAuth` | `Bool` | Whether auth gate should show |
| `chatListUserPickerSheet` | `UserPickerSheet?` | Active user picker sheet |

### View Selection Logic

```swift
// Simplified decision tree in ContentView.body:
if !prefPerformLA || accessAuthenticated {
    contentView()   // Main app content
} else {
    lockButton()    // Authentication required
}
```

The `contentView()` function further decides:
- If `chatModel.onboardingStage != .onboardingComplete`: show onboarding
- If `chatModel.migrationState != nil`: show migration UI
- Otherwise: show `ChatListView` in a navigation container

---

## 3. Navigation Stack

### iOS Version Compatibility

**File**: `Shared/Views/Helpers/NavStackCompat.swift`

The app supports iOS 15+ and uses a compatibility wrapper:

```swift
// NavStackCompat provides:
// - NavigationStack (iOS 16+): programmatic navigation via NavigationPath
// - NavigationView (iOS 15): classic NavigationLink-based navigation
```

### Primary Navigation Path

```
ChatListView
    │
    ├─[tap chat]─→ ChatView
    │                  │
    │                  ├─[tap info]─→ ChatInfoView (direct)
    │                  │              └─→ VerifyCodeView, etc.
    │                  │
    │                  ├─[tap info]─→ GroupChatInfoView (group)
    │                  │              ├─→ GroupMemberInfoView
    │                  │              ├─→ GroupProfileView
    │                  │              └─→ GroupLinkView
    │                  │
    │                  └─[tap message info]─→ ChatItemInfoView
    │
    ├─[tap connection]─→ ContactConnectionInfo
    │
    └─[settings]─→ SettingsView
                       ├─→ NotificationsView
                       ├─→ NetworkAndServers
                       ├─→ AppearanceSettings
                       ├─→ PrivacySettings
                       ├─→ DatabaseView
                       └─→ UserProfilesView
```

### Navigation Trigger

Chat navigation is triggered by setting `ChatModel.chatId`:

```swift
// In ChatListNavLink:
ItemsModel.shared.loadOpenChat(chatId) {
    // This sets ChatModel.chatId = chatId after a 250ms delay
    // allowing navigation animation to start smoothly
}
```

---

## 4. Sheet Presentation

Sheets are presented modally on top of the navigation stack:

| Sheet | Trigger | Content |
|-------|---------|---------|
| UserPicker | Tap user avatar in nav bar | User list, settings shortcuts |
| NewChatView | Tap FAB / "+" button | Create link, scan QR, paste link, new group |
| WhatsNew | App update detected | Release notes |
| AddGroupView | "New Group" action | Group creation wizard |
| ConnectDesktopView | Settings > Desktop | Remote desktop pairing |
| MigrateFromDevice | Settings > Migration | Device export |
| MigrateToDevice | Onboarding migration | Device import |
| LocalAuthView | App foreground after background | Biometric/passcode auth |

### Sheet Management

Sheets use SwiftUI `.sheet(item:)` or `.sheet(isPresented:)` modifiers on `ContentView` and `ChatListView`. Some sheets use the centralized `AppSheetState.shared` observable for coordination:

```swift
class AppSheetState: ObservableObject {
    static let shared = AppSheetState()
    var scenePhaseActive: Bool = false
    // ... sheet state coordination
}
```

---

## 5. Deep Linking

### Notification Deep Link

When the user taps a notification:

1. `NtfManager.processNotificationResponse()` extracts the `chatId` from notification payload
2. If a different user: calls `changeActiveUser(userId:)`
3. Sets `ChatModel.chatId = chatId` to navigate to the conversation
4. If the app was in background: the notification response is stored in `ChatModel.notificationResponse` and processed when the app becomes active

### URL Deep Link

SimpleX links (`simplex:/chat#...`) are handled via `onOpenURL`:

```swift
.onOpenURL { url in
    if AppChatState.shared.value == .active {
        chatModel.appOpenUrl = url      // Process immediately
    } else {
        chatModel.appOpenUrlLater = url // Process when active
    }
}
```

URL processing routes to the appropriate connection flow (join group, add contact, etc.).

### Call Deep Link

Call invitations from notifications:
1. `NtfManager` detects `ntfActionAcceptCall` action
2. Sets `ChatModel.ntfCallInvitationAction = (chatId, .accept)`
3. `ContentView` picks up the pending action and initiates the call

---

## 6. Call Overlay

The call UI overlays the entire app when a call is active:

### Call Banner

When `ChatModel.activeCall != nil` and call is in connecting/active state:
- A banner appears at the top of ContentView (height: `callTopPadding = 40`)
- Shows contact name, call duration, tap to return to full-screen call
- Main content is padded down to accommodate the banner

### Full-Screen Call View

When `ChatModel.showCallView == true`:
- `ActiveCallView` covers the entire screen as a ZStack overlay
- Contains local/remote video, controls (mute, camera, speaker, end)
- PiP mode: `ChatModel.activeCallViewIsCollapsed` collapses to mini view
- Call view is always rendered on top of navigation and sheets

```swift
// In ContentView.allViews():
ZStack {
    contentView()
        .padding(.top, showCallArea ? callTopPadding : 0)

    if showCallArea, let call = chatModel.activeCall {
        VStack {
            activeCallInteractiveArea(call)
            Spacer()
        }
    }

    if chatModel.showCallView, let call = chatModel.activeCall {
        callView(call)  // Full screen overlay
    }
}
```

---

## 7. Authentication Gate

### Local Authentication

When `DEFAULT_PERFORM_LA` is enabled:

1. App enters background: `chatModel.contentViewAccessAuthenticated = false`
2. App returns to foreground: `ContentView` shows `lockButton()` instead of content
3. User taps lock button: `LocalAuthView` presented
4. On successful auth: `chatModel.contentViewAccessAuthenticated = true`, content revealed

### Authentication Methods
- Face ID / Touch ID (via `LocalAuthentication` framework)
- Custom numeric passcode
- Custom alphanumeric passcode

### Extended Authentication
- After successful auth, a grace period prevents re-auth for brief background/foreground cycles
- `contentAccessAuthenticationExtended` is computed at `ContentView.init` to avoid render-time race conditions
- The `enteredBackgroundAuthenticated` timestamp tracks when the app was last authenticated in background

---

## 8. Onboarding Flow

First-launch experience controlled by `ChatModel.onboardingStage`:

```swift
enum OnboardingStage {
    case step1_SimpleXInfo      // Welcome screen
    case step2_CreateProfile    // Create first user profile
    case step3_ChooseNotifications // Set notification preferences
    case step4_SetupComplete    // Setup complete
    case onboardingComplete     // Normal operation
}
```

Each stage is a dedicated view presented in place of `ChatListView` within `ContentView`.

Migration state (`ChatModel.migrationState != nil`) takes precedence over onboarding.

---

## Source Files

| File | Path |
|------|------|
| Root view | `Shared/ContentView.swift` |
| App entry point | `Shared/SimpleXApp.swift` |
| Navigation compat | `Shared/Views/Helpers/NavStackCompat.swift` |
| Chat list (nav root) | `Shared/Views/ChatList/ChatListView.swift` |
| Nav link wrapper | `Shared/Views/ChatList/ChatListNavLink.swift` |
| User picker | `Shared/Views/ChatList/UserPicker.swift` |
| Active call view | `Shared/Views/Call/ActiveCallView.swift` |
| Local auth view | `Shared/Views/LocalAuth/LocalAuthView.swift` |
| Notification manager | `Shared/Model/NtfManager.swift` |
