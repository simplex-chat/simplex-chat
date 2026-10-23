# SimpleX Chat iOS -- Navigation Architecture

> Technical specification for the navigation stack, deep linking, sheet presentation, and call overlay.
>
> Related specs: [Chat List](chat-list.md) | [Chat View](chat-view.md) | [State Management](../state.md) | [README](../README.md)
> Related product: [Product Overview](../../product/README.md)

**Source:** [`ContentView.swift`](../../Shared/ContentView.swift) | [`NewChatView.swift`](../../Shared/Views/NewChat/NewChatView.swift) | [`SettingsView.swift`](../../Shared/Views/UserSettings/SettingsView.swift) | [`OnboardingView.swift`](../../Shared/Views/Onboarding/OnboardingView.swift) | [`UserProfilesView.swift`](../../Shared/Views/UserSettings/UserProfilesView.swift)

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

## 2. Root View -- [`ContentView`](../../Shared/ContentView.swift#L24)

**File**: [`Shared/ContentView.swift`](../../Shared/ContentView.swift)

`ContentView` is the root view injected by `SimpleXApp`. It manages:

### [Environment](../../Shared/ContentView.swift#L25-L37)
- `@EnvironmentObject var chatModel: ChatModel`
- `@EnvironmentObject var theme: AppTheme`
- `@Environment(\.scenePhase) var scenePhase`

### [Key State](../../Shared/ContentView.swift#L35-L52)
| Property | Type | Purpose |
|----------|------|---------|
| [`contentAccessAuthenticationExtended`](../../Shared/ContentView.swift#L35) | `Bool` | Passed at init to avoid re-render timing issues |
| [`automaticAuthenticationAttempted`](../../Shared/ContentView.swift#L38) | `Bool` | Whether biometric auth was auto-attempted |
| [`waitingForOrPassedAuth`](../../Shared/ContentView.swift#L51) | `Bool` | Whether auth gate should show |
| [`chatListUserPickerSheet`](../../Shared/ContentView.swift#L52) | `UserPickerSheet?` | Active user picker sheet |

### [View Selection Logic](../../Shared/ContentView.swift#L60-L80)

```swift
// Simplified decision tree in ContentView.body:
if !prefPerformLA || accessAuthenticated {
    contentView()   // Main app content
} else {
    lockButton()    // Authentication required
}
```

The [`contentView()`](../../Shared/ContentView.swift#L169) function further decides:
- If `chatModel.onboardingStage != .onboardingComplete`: show [onboarding](../../Shared/ContentView.swift#L174)
- If `chatModel.migrationState != nil`: show migration UI
- Otherwise: show `ChatListView` in a navigation container

---

## 3. Navigation Stack

### iOS Version Compatibility

**File**: [`Shared/Views/Helpers/NavStackCompat.swift`](../../Shared/Views/Helpers/NavStackCompat.swift)

The app supports iOS 15+ and uses a compatibility wrapper ([`NavStackCompat`](../../Shared/Views/Helpers/NavStackCompat.swift#L11)):

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
| [`NewChatView`](../../Shared/Views/NewChat/NewChatView.swift#L78) | Tap FAB / "+" button | Create link, scan QR, paste link, new group |
| WhatsNew | App update detected | Release notes |
| AddGroupView | "New Group" action | Group creation wizard |
| ConnectDesktopView | Settings > Desktop | Remote desktop pairing |
| MigrateFromDevice | Settings > Migration | Device export |
| MigrateToDevice | Onboarding migration | Device import |
| [LocalAuthView](../../Shared/ContentView.swift#L95) | App foreground after background | Biometric/passcode auth |

### Sheet Management

Sheets use SwiftUI `.sheet(item:)` or `.sheet(isPresented:)` modifiers on `ContentView` and `ChatListView`. Some sheets use the centralized [`AppSheetState.shared`](../../Shared/ContentView.swift#L29) observable for coordination:

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

### [URL Deep Link](../../Shared/ContentView.swift#L288)

SimpleX links (`simplex:/chat#...`) are handled via [`connectViaUrl()`](../../Shared/ContentView.swift#L456):

```swift
.onOpenURL { url in
    if AppChatState.shared.value == .active {
        chatModel.appOpenUrl = url      // Process immediately
    } else {
        chatModel.appOpenUrlLater = url // Process when active
    }
}
```

URL processing routes to the appropriate connection flow (join group, add contact, etc.) via [`planAndConnect()`](../../Shared/Views/NewChat/NewChatView.swift#L1342).

App links use the app's own scheme, `simplexchat:`, for everything that is not a connection link. `connectViaUrl_()` branches on that scheme before the path switch, so an app link never reaches `planAndConnect()`, and `openAppLink()` dispatches on its path. A badge link (`simplexchat:/badge/code/<code>`) goes to `openBadgeLink()` in [`BadgesRedeemCodeView.swift`](../../Shared/Views/Badges/BadgesRedeemCodeView.swift), which presents `BadgesRedeemLinkView` as an app sheet. The sheet names the active profile and asks before redeeming, since any web page can send the link and a profile holds one badge at a time. On *Add badge* it redeems the code into that profile and then shows `BadgesView`; *Cancel* sends nothing. If that profile already shows a badge (read from `BadgeModel` when the sheet renders), which core would refuse, the sheet does not offer to add it: it says nothing was added, says where a code for another badge is (on the page it was bought on, under *Show code*), and offers *View your badge*; it cannot tell a repeated link for the badge it shows from another badge's code, so it asserts neither. While the code is being redeemed, *Dismiss* or a swipe closes the sheet without cancelling anything; if the redemption then succeeds with the sheet closed or covered, an alert says the badge was added and names the profile, unless the profile is hidden and no longer active, when nothing is shown (`showBadgeAddedAlert()`, which the store lane shares). While the sheet shows a redemption in flight (`isBadgeLinkIssuing()`), `connectViaUrl_()` ignores a further app link; otherwise it dismisses the sheets as for any link, which replaces an earlier badge link sheet. Any other path is a link type from a later version, and gets an alert asking to check for app updates. An app link can carry a secret, so `onOpenURL` logs it as "app link" rather than the URL.

### Call Deep Link

Call invitations from notifications:
1. `NtfManager` detects `ntfActionAcceptCall` action
2. Sets `ChatModel.ntfCallInvitationAction = (chatId, .accept)`
3. `ContentView` picks up the pending action and initiates the call

---

## 6. Call Overlay

The call UI overlays the entire app when a call is active:

### [Call Banner](../../Shared/ContentView.swift#L203)

When `ChatModel.activeCall != nil` and call is in connecting/active state:
- A banner appears at the top of ContentView (height: [`callTopPadding = 40`](../../Shared/ContentView.swift#L54))
- Shows contact name, call duration, tap to return to full-screen call
- Main content is padded down to accommodate the banner

### [Full-Screen Call View](../../Shared/ContentView.swift#L185)

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

### [Local Authentication](../../Shared/ContentView.swift#L366)

When [`DEFAULT_PERFORM_LA`](../../Shared/ContentView.swift#L44) is enabled:

1. App enters background: `chatModel.contentViewAccessAuthenticated = false`
2. App returns to foreground: `ContentView` shows [`lockButton()`](../../Shared/ContentView.swift#L238) instead of content
3. User taps lock button: [`LocalAuthView`](../../Shared/ContentView.swift#L95) presented
4. On successful auth: `chatModel.contentViewAccessAuthenticated = true`, content revealed

### Authentication Methods
- Face ID / Touch ID (via `LocalAuthentication` framework)
- Custom numeric passcode
- Custom alphanumeric passcode

### [Extended Authentication](../../Shared/ContentView.swift#L358)
- After successful auth, a grace period prevents re-auth for brief background/foreground cycles ([`unlockedRecently()`](../../Shared/ContentView.swift#L358))
- [`contentAccessAuthenticationExtended`](../../Shared/ContentView.swift#L35) is computed at `ContentView.init` to avoid render-time race conditions
- The `enteredBackgroundAuthenticated` timestamp tracks when the app was last authenticated in background

---

## 8. [Onboarding Flow](../../Shared/Views/Onboarding/OnboardingView.swift#L13)

First-launch experience controlled by [`ChatModel.onboardingStage`](../../Shared/Views/Onboarding/OnboardingView.swift#L47):

```swift
enum OnboardingStage: String, Identifiable {
    case step1_SimpleXInfo              // Welcome screen
    case step2_CreateProfile            // deprecated
    case step3_CreateSimpleXAddress     // deprecated
    case step3_ChooseServerOperators    // Choose server operators
    case step4_SetNotificationsMode     // Set notification preferences
    case onboardingComplete             // Normal operation
}
```

Each stage is a dedicated view presented in place of `ChatListView` within [`ContentView`](../../Shared/ContentView.swift#L24).

Migration state (`ChatModel.migrationState != nil`) takes precedence over onboarding.

---

## 9. Channel Creation Flow (`AddChannelView`)

**Source:** [`Shared/Views/NewChat/AddChannelView.swift`](../../Shared/Views/NewChat/AddChannelView.swift)

### Entry Point

`NewChatMenuButton` includes a NavigationLink "Create public channel" with antenna icon, navigating to `AddChannelView`.

### Three-Step Wizard

| Step | Function | Description |
|------|----------|-------------|
| 1. Profile | `profileStepView()` | Channel name input (`channelNameTextField()`), profile image picker. "Configure relays" link to `NetworkAndServers`. Validates via `canCreateProfile()` (non-empty + valid display name) and `checkHasRelays()`. |
| 2. Progress | `progressStepView(_:)` | Relay connection progress with `RelayProgressIndicator` (circular active/total or spinner). Expandable relay list with `relayStatusIndicator(_:)` (green/red/orange dots). Cancel via `cancelChannelCreation(_:)` which calls `apiDeleteChat`. |
| 3. Link | `linkStepView(_:)` | Wraps `GroupLinkView(isChannel: true)` for channel link sharing. |

### Key Functions

| Function | Scope | Description |
|----------|-------|-------------|
| `createChannel()` | private | Calls `apiNewPublicGroup(incognito:relayIds:groupProfile:)`, sets `ChannelRelaysModel` |
| `getEnabledRelays()` | private | Filters enabled/non-deleted relays, selects random 3 |
| `checkHasRelays()` | private | Validates at least one relay exists |
| `relayDisplayName(_:)` | module | name > domain > link host > fallback |
| `relayStatusIndicator(_:)` | module | Green/red/orange dot + status text |
| `RelayProgressIndicator` | module | Circular progress (active/total) or spinner |

## 10. Relay URL Interception

**Source:** [`Shared/ContentView.swift`](../../Shared/ContentView.swift#L471)

In `connectViaUrl_()`, relay address links (URL path `/r`) are intercepted before processing:

```swift
if path == "/r" {
    showAlert(NSLocalizedString("Relay address", ...),
              message: NSLocalizedString("This is a chat relay address, it cannot be used to connect.", ...))
    return
}
```

Similarly, in `planAndConnect()` (`NewChatView.swift`), `.simplexLink(_, .relay, _, _)` patterns trigger the same alert and block connection.

## 11. Channel-Specific NewChatView Behavior

**Source:** [`Shared/Views/NewChat/NewChatView.swift`](../../Shared/Views/NewChat/NewChatView.swift)

### Prepared Group Alert (`showPrepareGroupAlert`)

When `groupShortLinkInfo?.direct == false` (channel relay link), the prepare alert uses:
- Channel icon: `antenna.radiowaves.left.and.right.circle.fill`
- Title: "Open channel"
- Error: "Error opening channel"
- `apiPrepareGroup` call passes `directLink: false`
- Stores `groupShortLinkInfo.groupRelays` in `ChatModel.shared.channelRelayHostnames`

The alert's confirm calls `prepareAndOpenGroup`, which prepares the group, stores the relays, adds the chat and opens it. `planAndConnect(…, prepareGroup: .open)`, which the online search's Open uses, calls `prepareAndOpenGroup` without the alert when the plan has no owner verification and no other name (see [Chat List, Opening a result](chat-list.md#opening-a-result)); with either, and with the default `.confirm`, the alert shows.

### Own Link Confirmation (`showOwnGroupLinkConfirmConnectSheet`)

For channels: shows "This is your link for channel" with only "Open channel" + "Cancel" buttons. No incognito or profile selection options.

### Known Group Alert (`showOpenKnownGroupAlert`)

For channels (`groupInfo.useRelays`): the title is "Open channel"; for groups, "Open group"; business chats keep "Open chat" / "Open new chat". Unless the chat is merely prepared (`nextConnectPrepared`) or a business chat, the alert shows an information line with the user's membership role (`memberRoleInformation`) in the secondary color: subscriber/contributor for channels, observer/member for groups, moderator/admin/owner for both.

## 12. New Chat Online Search

**Source:** [`Shared/Views/NewChat/NewChatMenuButton.swift`](../../Shared/Views/NewChat/NewChatMenuButton.swift)

[`NewChatSheet`](../../Shared/Views/NewChat/NewChatMenuButton.swift#L36) offers the chat list's online search (see [Chat List, Online Search](chat-list.md#11-online-search)) with its own [`OnlineSearchModel`](../../Shared/Views/NewChat/NewChatMenuButton.swift#L46), reset on every change of `searchText` ([L187](../../Shared/Views/NewChat/NewChatMenuButton.swift#L190)) and of `currentUser?.userId` ([L190](../../Shared/Views/NewChat/NewChatMenuButton.swift#L193)). The sheet's `onDisappear` calls `cancelConnectProgress()`, which cancels a search still in flight.

| Element | Where | Behavior |
|---------|-------|----------|
| Search key | [`ContactsListSearchBar`](../../Shared/Views/NewChat/NewChatMenuButton.swift#L418) with `onSearch` ([L424](../../Shared/Views/NewChat/NewChatMenuButton.swift#L428)) | `.submitLabel(.search)` and `.onSubmit` run [`runDirectorySearch()`](../../Shared/Views/NewChat/NewChatMenuButton.swift#L229): `searchDirectory(text)`, the directory only; the field neither autocorrects nor capitalizes |
| "Connect to …" row | below the field ([L99](../../Shared/Views/NewChat/NewChatMenuButton.swift#L99)) | shown whenever there is a name candidate, results or not |
| "Search in Directory" row | below the field ([L109](../../Shared/Views/NewChat/NewChatMenuButton.swift#L109)) | shown for non-empty, non-link text while no results show or load (`!onlineSearch.showResults`); runs [`runDirectorySearch()`](../../Shared/Views/NewChat/NewChatMenuButton.swift#L229) |
| Results card | in place of `ContactsList` while `showResults` and there is a shown contact or a row, [`onlineSearchSection(_:contacts:)`](../../Shared/Views/NewChat/NewChatMenuButton.swift#L198) | One `Section` without a header: the shown contacts (`ContactListNavLink(chat:showDeletedChatIcon: true)`, disabled while the chat isn't running, as in `ContactsList`), your chats (`rows.yourChats`, the loaded ones), then the results, then the retry or load row. Your chats are drawn as [`ContactListNavLink`](../../Shared/Views/Contacts/ContactListNavLink.swift#L12)`(chat:showDeletedChatIcon: true)`, whose [`groupNavLink`](../../Shared/Views/Contacts/ContactListNavLink.swift#L170) opens the group and closes the sheet (`dismissAllSheets` then `loadOpenChat`; avatar, name, favourite star, incognito mask; no swipe actions). Results are `DirectorySearchRow(imageSize: 30)`, New chat's 30-pt icons, with Open or "Already joining" and the same long-press menu as in the chat list; a tap or the menu's Open runs [`openResult(_:)`](../../Shared/Views/NewChat/NewChatMenuButton.swift#L220): `openDirectoryResult(result, dismiss: true)`, so the opened chat closes the sheet |

Both triggers do nothing for empty text or a pasted link and go through `confirmDirectorySearch`, which asks "Enable Directory search?" while the switch is off. The list is `insetGrouped` and not flipped, so the card is an ordinary `Section` with default row backgrounds, and its load row is the last row.

**De-duplication against the contacts list.** Before the contacts the list builder computes [`ContactsList.shownChats(…)`](../../Shared/Views/NewChat/NewChatMenuButton.swift#L375) with the same inputs `ContactsList` gets (`contactListChatPredicate`, the search state, `DEFAULT_SHOW_UNREAD_AND_FAVORITES`) and passes the ids of the shown contacts to `rows(shownChatIds:)` ([L173](../../Shared/Views/NewChat/NewChatMenuButton.swift#L173)). `shownChats` returns the contacts matching the predicate and the filtered, sorted list the section shows; `ContactsList.body` uses the same function, and the results card draws that list.

**Empty state.** [`ContactsList`](../../Shared/Views/NewChat/NewChatMenuButton.swift#L286) has `showsEmptyState` (default `true`, [L293](../../Shared/Views/NewChat/NewChatMenuButton.swift#L297)); the sheet passes `!onlineSearch.searching`, so "No filtered contacts" / "No contacts" are omitted while the online search runs. `ContactsList` is drawn only while the results card isn't, so the empty state also shows when a finished search found nothing and no contact matches.

**Archived contacts.** [`DeletedChats`](../../Shared/Views/NewChat/NewChatMenuButton.swift#L548) passes neither `onSearch` nor `showsEmptyState`: its search field keeps the `.return` label with a no-op submit, and its empty states are unchanged.

---

## Source Files

| File | Path |
|------|------|
| Root view | [`Shared/ContentView.swift`](../../Shared/ContentView.swift) |
| App entry point | `Shared/SimpleXApp.swift` |
| Navigation compat | [`Shared/Views/Helpers/NavStackCompat.swift`](../../Shared/Views/Helpers/NavStackCompat.swift) |
| Chat list (nav root) | `Shared/Views/ChatList/ChatListView.swift` |
| Nav link wrapper | `Shared/Views/ChatList/ChatListNavLink.swift` |
| User picker | `Shared/Views/ChatList/UserPicker.swift` |
| New chat view | [`Shared/Views/NewChat/NewChatView.swift`](../../Shared/Views/NewChat/NewChatView.swift) |
| Channel creation | [`Shared/Views/NewChat/AddChannelView.swift`](../../Shared/Views/NewChat/AddChannelView.swift) |
| New chat menu | [`Shared/Views/NewChat/NewChatMenuButton.swift`](../../Shared/Views/NewChat/NewChatMenuButton.swift) |
| Settings view | [`Shared/Views/UserSettings/SettingsView.swift`](../../Shared/Views/UserSettings/SettingsView.swift) |
| User profiles | [`Shared/Views/UserSettings/UserProfilesView.swift`](../../Shared/Views/UserSettings/UserProfilesView.swift) |
| Onboarding view | [`Shared/Views/Onboarding/OnboardingView.swift`](../../Shared/Views/Onboarding/OnboardingView.swift) |
| Active call view | `Shared/Views/Call/ActiveCallView.swift` |
| Local auth view | `Shared/Views/LocalAuth/LocalAuthView.swift` |
| Notification manager | `Shared/Model/NtfManager.swift` |
