# Onboarding

## Purpose

First-time setup flow for new users. Guides through app introduction, profile creation, server operator conditions acceptance, and notification configuration. Also provides an entry point for device migration.

## Route / Navigation

- **Entry point**: App launch when `onboardingStageDefault` is not `.onboardingComplete`
- **Presented by**: `OnboardingView` renders the appropriate step based on `OnboardingStage` enum
- **Flow direction**: Linear progression; back navigation hidden on later steps (`.navigationBarBackButtonHidden(true)`)
- **Completion**: Sets `onboardingStageDefault` to `.onboardingComplete` and updates `chatModel.onboardingStage`

## Onboarding Steps

### Step 1: Welcome / SimpleX Info (`SimpleXInfo`)

**Stage**: `step1_SimpleXInfo`

| Element | Description |
|---|---|
| Logo | SimpleX Chat logo (light/dark variant based on color scheme) |
| "The future of messaging" | Info button opening `HowItWorks` sheet |
| Privacy redefined | "No user identifiers." with privacy icon |
| Immune to spam | "You decide who can connect." with shield icon |
| Decentralized | "Anybody can host servers." with decentralized icon |
| **Create your profile** button | Primary action; navigates to `CreateFirstProfile` |
| **Migrate from another device** button | Secondary action; opens `MigrateToDevice` sheet |

The "How it works" sheet (`HowItWorks`) explains SimpleX's privacy model with an option to proceed to profile creation.

### Step 2: Create Profile (`CreateFirstProfile`)

**Stage**: `step2_CreateProfile` (deprecated -- now part of step 1 flow)

| Element | Description |
|---|---|
| Display name field | Required; auto-focused after 1 second delay |
| Validation | `mkValidName` check; alerts for invalid/duplicate names |
| Create button | Calls profile creation API; advances to next step |

Profile is stored locally and only shared with contacts. Footer explains this privacy property.

### Step 3: Server Operator Conditions (`OnboardingConditionsView`)

**Stage**: `step3_ChooseServerOperators` (changed to simplified conditions view)

| Element | Description |
|---|---|
| "Conditions of use" title | Large title header |
| Privacy explanation | "Private chats, groups and your contacts are not accessible to server operators." |
| Operator selection | Toggle operators (with `selectedOperatorIds`) |
| Show conditions | Sheet to view full conditions (`ConditionsWebView`) |
| Configure operators | Sheet to customize operator settings |
| **Accept** button | Accepts conditions and advances to notifications step |

Previous deprecated step `step3_CreateSimpleXAddress` (`CreateSimpleXAddress`) is no longer in the active flow.

### Step 4: Set Notification Mode (`SetNotificationsMode`)

**Stage**: `step4_SetNotificationsMode`

| Element | Description |
|---|---|
| "Push notifications" title | Large title header |
| Info text | Explanation of notification modes |
| Mode selector | `NtfModeSelector` for each `NotificationsMode.values` |
| **Enable notifications** / **Use chat** button | Sets notification mode and completes onboarding |
| Info sheet | `NotificationsInfoView` accessible for detailed explanation |

Notification modes:

| Mode | Description |
|---|---|
| Instant | Background connection maintained; real-time notifications |
| Periodic | Checks every 10 minutes; battery-friendly |
| Off | No push notifications; messages received only when app is open |

On completion, `onboardingStageDefault.set(.onboardingComplete)` is called.

### Completion

**Stage**: `onboardingComplete`

`OnboardingView` renders `EmptyView()` and the app proceeds to `ChatListView`.

## Optional Paths

### Migrate from Another Device

- Triggered from Step 1 via "Migrate from another device" button
- Sets `chatModel.migrationState = .pasteOrScanLink`
- Opens `MigrateToDevice` in a sheet within `NavigationView`
- User pastes or scans a migration link from the source device
- Imports database and settings from the linked device

### What's New (`WhatsNewView`)

- Not part of the linear onboarding flow
- Shown when `DEFAULT_WHATS_NEW_VERSION` differs from current version
- Accessible later from Settings > Help > What's new
- Displays changelog with feature descriptions

## Onboarding Stage Enum

```
enum OnboardingStage: String {
    case step1_SimpleXInfo
    case step2_CreateProfile          // deprecated
    case step3_CreateSimpleXAddress   // deprecated
    case step3_ChooseServerOperators  // conditions acceptance
    case step4_SetNotificationsMode
    case onboardingComplete
}
```

Persisted via `DEFAULT_ONBOARDING_STAGE` in `UserDefaults`.

## Loading / Error States

| State | Behavior |
|---|---|
| No device token | Alert "No device token!" if trying to set notification mode without token |
| Profile creation error | Alert with error description |
| Migration failure | Error handling within `MigrateToDevice` flow |
| Conditions loading | Async fetch of operator conditions |

## Related Specs

- `spec/architecture.md` -- App architecture and initialization flow
- [Chat List](chat-list.md) -- Destination after onboarding completes
- [User Profiles](user-profiles.md) -- Profile created during onboarding; additional profiles later
- [Settings](settings.md) -- Notification and server settings revisitable after onboarding

## Source Files

- `Shared/Views/Onboarding/OnboardingView.swift` -- Step router and `OnboardingStage` enum definition
- `Shared/Views/Onboarding/SimpleXInfo.swift` -- Step 1: Welcome screen with privacy highlights and migration entry
- `Shared/Views/Onboarding/CreateProfile.swift` -- Profile creation form (shared between onboarding and user profiles)
- `Shared/Views/Onboarding/CreateSimpleXAddress.swift` -- Deprecated step 3: SimpleX address creation
- `Shared/Views/Onboarding/ChooseServerOperators.swift` -- Step 3: Server operator conditions and selection
- `Shared/Views/Onboarding/SetNotificationsMode.swift` -- Step 4: Push notification mode selection
- `Shared/Views/Onboarding/HowItWorks.swift` -- "How it works" info sheet from step 1
- `Shared/Views/Onboarding/WhatsNewView.swift` -- Changelog / what's new display
- `Shared/Views/Onboarding/AddressCreationCard.swift` -- Address creation prompt card
