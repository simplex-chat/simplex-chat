# Onboarding

> **Related spec:** [spec/client/navigation.md](../../spec/client/navigation.md)

## Purpose

First-time setup flow for new users. Guides through app introduction, profile creation, database passphrase setup (Desktop), server operator conditions acceptance, SimpleX address creation, and notification configuration (Android). Also provides an entry point for device migration.

## Route / Navigation

- **Entry point**: App launch when `onboardingStage` is not `OnboardingComplete`
- **Presented by**: `OnboardingView` renders the appropriate step based on `OnboardingStage` enum
- **Flow direction**: Linear progression controlled by `appPrefs.onboardingStage`
- **Completion**: Sets `onboardingStage` to `OnboardingComplete`

## Onboarding Stages

The `OnboardingStage` enum defines the flow:

| Stage | Description |
|---|---|
| `Step1_SimpleXInfo` | Welcome screen with app introduction |
| `Step2_CreateProfile` | Create first user profile |
| `LinkAMobile` | Desktop-only: link a mobile device |
| `Step2_5_SetupDatabasePassphrase` | Desktop-only: set database encryption passphrase |
| `Step3_ChooseServerOperators` | Accept server operator conditions |
| `Step3_CreateSimpleXAddress` | Create a SimpleX contact address |
| `Step4_SetNotificationsMode` | Android-only: configure notification mode |
| `OnboardingComplete` | Onboarding finished |

## Page Sections

### Step 1: Welcome / SimpleX Info (`SimpleXInfo`)

**Stage**: `Step1_SimpleXInfo`

| Element | Description |
|---|---|
| Logo | `SimpleXLogo` -- SimpleX Chat logo (light/dark variant based on `isInDarkTheme()`) |
| Info button | `OnboardingInformationButton` -- "The next generation of private messaging"; taps open `HowItWorks` fullscreen modal |
| Privacy redefined | `InfoRow` with privacy icon: "No user identifiers" |
| Immune to spam | `InfoRow` with shield icon: "You decide who can connect" |
| Decentralized | `InfoRow` with decentralized icon: "Anybody can host servers" |
| **Create your profile** button | `OnboardingActionButton` -- primary action; advances to profile creation |
| **Migrate from another device** button | `TextButtonBelowOnboardingButton` -- opens `MigrateToDeviceView` fullscreen modal |

Layout: `ColumnWithScrollBar` with `DEFAULT_ONBOARDING_HORIZONTAL_PADDING`, max width constrained (250dp Android, 500dp Desktop).

### Step 2: Create Profile

**Stage**: `Step2_CreateProfile`

| Element | Description |
|---|---|
| Display name field | Required text input; auto-focused |
| Validation | Name validation with `mkValidName` check |
| Create button | Creates profile via API; advances to next step |

Profile is stored locally and only shared with contacts.

### Step 2.5: Setup Database Passphrase (Desktop only)

**Stage**: `Step2_5_SetupDatabasePassphrase`

| Element | Description |
|---|---|
| Passphrase field | Secure text input for database encryption key |
| Confirm field | Passphrase confirmation |
| Set button | Encrypts database with passphrase |

### Link a Mobile (Desktop only)

**Stage**: `LinkAMobile`

| Element | Description |
|---|---|
| Instructions | How to connect mobile device to desktop |
| QR code | Connection QR code for mobile scanning |
| Skip button | Skip this step |

### Step 3: Choose Server Operators

**Stage**: `Step3_ChooseServerOperators`

| Element | Description |
|---|---|
| Operator list | Available server operators with conditions |
| Conditions text | Terms of service for selected operators |
| Accept button | Accept conditions and continue |

Managed by `ChooseServerOperators.kt`.

### Step 3b: Create SimpleX Address

**Stage**: `Step3_CreateSimpleXAddress`

| Element | Description |
|---|---|
| Address creation | Auto-creates a SimpleX contact address |
| QR code | Displays the created address as QR code |
| Share button | Share address link |
| Skip button | Skip address creation |

### Step 4: Set Notifications Mode (Android only)

**Stage**: `Step4_SetNotificationsMode`

| Element | Description |
|---|---|
| Notification options | Instant (background service) / Periodic (every 10 min) / Off |
| Description | Explains battery impact and notification behavior for each mode |
| Continue button | Saves selection and completes onboarding |

Managed by `SetNotificationsMode.kt`.

### What's New (`WhatsNewView`)

Shown after onboarding or when triggered from Settings:

| Element | Description |
|---|---|
| Version highlights | New features and changes in the current version |
| Updated conditions | Notice about updated server operator conditions (if applicable) |
| Close button | Dismisses the view |

Triggered in `ChatListView` via `shouldShowWhatsNew()` with a 1-second delay.

## Source Files

| File | Path |
|---|---|
| `OnboardingView.kt` | `views/onboarding/OnboardingView.kt` |
| `SimpleXInfo.kt` | `views/onboarding/SimpleXInfo.kt` |
| `HowItWorks.kt` | `views/onboarding/HowItWorks.kt` |
| `SetupDatabasePassphrase.kt` | `views/onboarding/SetupDatabasePassphrase.kt` |
| `SetNotificationsMode.kt` | `views/onboarding/SetNotificationsMode.kt` |
| `ChooseServerOperators.kt` | `views/onboarding/ChooseServerOperators.kt` |
| `WhatsNewView.kt` | `views/onboarding/WhatsNewView.kt` |
| `LinkAMobileView.kt` | `views/onboarding/LinkAMobileView.kt` |
