# Implementation Plan: Onboarding UI Restructure, AGPL Logic, and Theme Support for SimpleX Chat

## Overview

Restructure the onboarding and initial interaction flow for SimpleX Chat mobile apps (Android and iOS). The plan covers three main areas: (1) AGPL-compliant handling of optional branded assets, (2) a new multi-screen intro and onboarding flow, and (3) consistent dark/light mode support across all new and updated screens.

The default build must remain fully AGPLv3-compliant and ship without non-AGPL assets. Branded images (illustrations, icons) are opt-in via a build-time flag and environment configuration.

## Design Approach

- **AGPL logic**: Introduce a build-time flag `USE_BRANDED_IMAGES` (default `false`). When false, use text-only or animated-card fallbacks. Images load only when the flag is set. Logos remain in the default build.
- **Asset placement**: Optional branded assets live outside the main AGPL repo—in a sibling folder or separate repo—and are included only when building with `USE_BRANDED_IMAGES=true`.
- **UI structure**: Replace the current single `SimpleXInfo` screen with a 3-page intro carousel, followed by Create profile, Conditions of use, notifications, and a "Now you can" hub that branches to Invite or Connect flows.
- **Theme support**: Use existing `Theme.AppCompat.DayNight` (Android) and theme system. All new screens use `MaterialTheme.colors`, `CurrentColors`, and `isInDarkTheme()` for consistent dark/light behavior.

## Key Files to Modify

### Build Configuration
- `apps/multiplatform/common/build.gradle.kts` - Add `USE_BRANDED_IMAGES` to `buildConfigField`
- `gradle.properties` or `local.properties` - Optional property for branded builds
- iOS: `.xcconfig` or build settings - Equivalent flag for iOS

### Onboarding and Intro
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/SimpleXInfo.kt` - Refactor or replace with intro carousel
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/` - New composables: `IntroCarouselView.kt`, `IntroPage.kt`, `NowYouCanView.kt`
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/App.kt` - Update onboarding stage routing
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/` - Add `Step0_IntroCarousel` or similar to `OnboardingStage` if defined in shared code

### Platform-Specific Onboarding
- `apps/ios/Shared/Views/Onboarding/SimpleXInfo.swift` - Align with new flow
- `apps/ios/Shared/Views/Onboarding/OnboardingView.swift` - Add intro carousel stage
- `apps/ios/Shared/ContentView.swift` - Routing for new stages

### New Chat / Invite / Connect
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/newchat/NewChatView.kt` - Reuse `HorizontalPager` pattern; adapt Invite/Connect layout to card-based design
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/newchat/NewChatSheet.kt` - Entry points for Invite and Connect from "Now you can"
- `apps/ios/Shared/Views/NewChat/NewChatView.swift` - Matching card layout and flow

### Conditions and Notifications
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/` - `OnboardingConditionsView`, `SetNotificationsMode` - Update layout to match new design
- `apps/ios/Shared/Views/Onboarding/` - Corresponding SwiftUI views

### Resources
- `apps/multiplatform/common/src/commonMain/resources/MR/` - New string resources for intro headlines, subtitles, button labels
- Optional asset folder (sibling or separate repo) - Illustrations for intro pages, Invite/Connect cards, Conditions screen

## New Onboarding Flow

### Screen Sequence

```
Intro 1 (swipeable) → Intro 2 → Intro 3 → Create profile → Conditions of use → Push notifications → Now you can
                                                                                        │
                                    ┌───────────────────────────────────────────────────┴───────────────────────────────────────────────────┐
                                    ▼                                                                                                       ▼
                            Invite someone                                                                                    Connect via link or QR code
                                    │                                                                                                       │
                    ┌───────────────┴───────────────┐                                                                           Paste link OR Scan QR code
                    ▼                               ▼
            Create 1-time link              Create SimpleX address
```

### Intro Carousel (Pages 1–3)

Each intro page shares the same layout:

- **Logo** (top): SIMPLEX logo (existing `SimpleXLogo()`)
- **Central graphic**: Illustration or icon (conditional on `USE_BRANDED_IMAGES`)
- **Headline**: Large bold text (e.g., "World's Most Secure Messaging")
- **Subtitle**: Supporting text (e.g., "SimpleX messaging has cutting-edge end-to-end encryption.")
- **Pagination dots**: Three dots indicating current page
- **Page 3 only**: "Create your profile" button and "Migrate from another device" link

**Content (placeholders TBC):**

| Page | Headline | Subtitle |
|------|----------|----------|
| 1 | World's Most Secure Messaging | SimpleX messaging has cutting-edge end-to-end encryption. |
| 2 | Freedom of Your Communications | You will support your favorite groups with future Community Vouchers. |
| 3 | You Own The Next Web | SimpleX is founded on the belief that you must own your identity, contacts and communities. |

### Create Profile

- Back button
- Isometric illustration (conditional on `USE_BRANDED_IMAGES`)
- Title: "Create profile"
- Subtitle: "Your profile is stored on your device and only shared with your contacts."
- Name input field: "Enter your name..."
- Primary button: "Create profile"

### Conditions of Use

- Isometric illustration (smartphone + server)
- Title: "Conditions of use"
- Privacy text: "Private chats, groups and your contacts are not accessible to server operators."
- Bullet list: "By using SimpleX Chat you agree to: send only legal content in public groups; respect other users - no spam."
- Link: "Privacy policy and conditions of use."
- Primary button: "Accept"
- Secondary links: "Configure server operators", "Configure notifications"

### Push Notifications

- Title: "Push notifications"
- Link: "How it affects privacy"
- Three options: Instant (E2E encrypted), Periodic (check every 20 min), No push server
- Primary button: "OK"

### Now You Can

- Title: "Now you can"
- Two cards:
  - "Invite someone to chat" (with chain-link icon)
  - "Connect via link or QR code" (with QR icon)
- Bottom navigation: profile, Chats, "+" button

### Invite Someone

- Two cards:
  - "Create private 1-time link" — "Your contact can use link or scan QR code"
  - "Create public SimpleX address" — "Public link for social media, email or website"

### Connect via Link or QR

- "PASTE THE LINK YOU RECEIVED" label
- Input field: "Tap to paste link"
- "OR SCAN QR CODE" label
- Camera / QR scanner area

## AGPL Logic Implementation

### Build-Time Flag

```kotlin
// BuildConfigCommon (or equivalent)
object BuildConfigCommon {
    // When true, branded images are included and shown. When false, use text/animated fallbacks.
    val USE_BRANDED_IMAGES: Boolean = false  // Set from Gradle/CI
}
```

### Gradle Configuration

```kotlin
// apps/multiplatform/common/build.gradle.kts
buildConfigField(
    "Boolean",
    "USE_BRANDED_IMAGES",
    project.findProperty("USE_BRANDED_IMAGES")?.toString()?.toBooleanStrictOrNull()?.toString() ?: "false"
)
```

### UI Conditional Display

```kotlin
@Composable
fun IntroPageCentralContent(pageIndex: Int) {
    if (BuildConfigCommon.USE_BRANDED_IMAGES) {
        BrandedIllustration(pageIndex)
    } else {
        AnimatedCardsOrTextFallback(pageIndex)
    }
}
```

### Asset Location (TBC)

- **Option A**: Sibling folder `simplex-chat-branded-assets/` in the same repo, excluded from default build
- **Option B**: Separate repo, pulled in via git submodule or CI when `USE_BRANDED_IMAGES=true`

**Key point:** Logos (SimpleX logo, app icon) remain in the default build and are not gated by this flag.

## Dark/Light Mode Support

### Current State

- Android: `Theme.AppCompat.DayNight.NoActionBar`, `values-night/themes.xml`, `configChanges="uiMode"`
- Programmatic control via `UiModeManager` (API 31+) for in-app theme (System/Light/Dark)
- Theme system: `CurrentColors`, `MaterialTheme.colors`, `isInDarkTheme()`

### Color Mappings for New Screens

| Element | Dark Mode | Light Mode |
|---------|-----------|------------|
| Background | Black (#000000) | White (#FFFFFF) |
| Primary text (headlines) | White | Black / dark gray |
| Secondary text (subtitles) | Light gray | Medium gray |
| Accent (links, buttons, active dot) | Blue (consistent) | Blue (consistent) |
| Card backgrounds | Dark gray / blue gradient | Light gray / white gradient |
| Status bar icons | White | Black |
| Inactive pagination dots | Dark gray | Light gray |

### Implementation

All new composables must use:

- `MaterialTheme.colors.background`, `MaterialTheme.colors.onBackground`
- `MaterialTheme.colors.primary` for accents
- `MaterialTheme.colors.secondary` for secondary text
- `isInDarkTheme()` when selecting asset variants (e.g., logo_light vs logo)

No new theme infrastructure is required; use existing theme system throughout.

## Reusable Components

### From Existing Codebase

| Component | Source | Use |
|-----------|--------|-----|
| `SimpleXLogo()` | `SimpleXInfo.kt` | Intro pages, branding |
| `HorizontalPager` pattern | `NewChatView.kt` | Intro carousel |
| `OnboardingActionButton` | `SimpleXInfo.kt` | "Create your profile" |
| `TextButtonBelowOnboardingButton` | `SimpleXInfo.kt` | "Migrate from another device" |
| `ModalView` | `helpers/ModalView.kt` | Full-screen modals |
| `InfoRow` | `SimpleXInfo.kt` | Bullet-like rows if needed |

### New Components to Extract

- **IntroPage**: Parametrised layout (logo, central content, headline, subtitle, pagination, optional buttons)
- **IntroCarousel**: `HorizontalPager` + page indicators + `IntroPage` for each page
- **OnboardingCard**: Card with illustration, title, description, action (for "Now you can", Invite options)

## Implementation Steps

### Phase 0: Build Configuration

1. Add `USE_BRANDED_IMAGES` to `BuildConfigCommon` in `common/build.gradle.kts`
2. Add optional Gradle property (e.g., in `gradle.properties` or CI)
3. Add equivalent flag for iOS build (`.xcconfig` or scheme setting)

### Phase 1: Intro Carousel

1. Create `IntroPage` composable with parametrised content
2. Create `IntroCarouselView` using `HorizontalPager` and dot indicators
3. Add `Step0_IntroCarousel` (or equivalent) to onboarding flow
4. Wire Intro 3 → "Create your profile" → existing `CreateFirstProfile`
5. Implement conditional central content (image vs animated/text fallback)

### Phase 2: Create Profile and Conditions

1. Update `CreateFirstProfile` layout to match design (illustration, spacing, input styling)
2. Update `OnboardingConditionsView` to match design (illustration, bullets, links, Accept, Configure links)
3. Ensure both screens use theme colors for dark/light mode

### Phase 3: Notifications and Now You Can

1. Update `SetNotificationsMode` layout (title, "How it affects privacy", three options, OK)
2. Create `NowYouCanView` with two cards (Invite, Connect)
3. Wire "Now you can" to main chat list and to Invite/Connect entry points

### Phase 4: Invite and Connect Screens

1. Update `NewChatView` / Invite flow to card-based layout (1-time link, SimpleX address)
2. Update Connect flow (paste link, QR scan area)
3. Add illustrations to cards (conditional on `USE_BRANDED_IMAGES`)

### Phase 5: Strings and Assets

1. Add all new string resources to MR
2. Create or integrate optional asset set (sibling folder or repo)
3. Verify reproducible build path when `USE_BRANDED_IMAGES=true`

## Testing Considerations

1. **AGPL build**: Default build (`USE_BRANDED_IMAGES=false`) runs without branded images; text/animated fallbacks display correctly
2. **Branded build**: With flag set, images load and display on intro, Create profile, Conditions, Invite cards
3. **Theme switching**: All screens render correctly in dark and light mode; no inverted or unreadable text
4. **Flow**: Full path Intro 1 → … → Now you can → Invite/Connect works on Android and iOS
5. **Backward compatibility**: Existing users past onboarding see correct main UI; no regression in Create profile or notifications setup

## Backward Compatibility

- **Onboarding state**: Add new stage(s) before or after existing ones; migration logic may be needed for users mid-onboarding
- **Default build**: No behavioral change for AGPL-only users; only UI structure and optional assets differ
- **Theme**: Existing theme preferences (System/Light/Dark) apply to new screens

## Design Decisions (Confirmed)

1. **AGPL flag**: Build-time only; no runtime toggle. Default `false` for AGPL compliance.

2. **Asset location**: TBC—sibling folder or separate repo. Must support reproducible builds when `USE_BRANDED_IMAGES=true`.

3. **Logos**: Remain in default build; not gated by `USE_BRANDED_IMAGES`.

4. **Intro page count**: Three swipeable pages before Create profile.

5. **"Now you can" placement**: After Conditions and Notifications; acts as hub before Invite/Connect.

6. **Theme**: Use existing theme system; no new theme infrastructure.
