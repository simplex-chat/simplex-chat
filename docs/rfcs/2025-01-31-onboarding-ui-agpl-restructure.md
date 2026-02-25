# RFC: Onboarding UI Restructure and AGPL Asset Separation

**Date:** 2025-01-31  
**Author:** hayk888997  
**Status:** Draft  
**Target Platforms:** Android, iOS

---

## Problem

The current SimpleX Chat onboarding experience has several limitations:

1. **Limited visual engagement** — The single `SimpleXInfo` screen doesn't effectively communicate SimpleX's unique privacy features to new users.

2. **AGPL licensing concerns** — Branded illustrations and assets are currently bundled directly in the AGPLv3 codebase, creating licensing complications for forks and community builds.

3. **Inconsistent onboarding flow** — The current flow lacks a clear progression that guides users through understanding the app's value proposition before account creation.

4. **Missing visual hierarchy** — New users don't have an intuitive path from onboarding to their first connection (invite or connect).

---

## Goals

- Create an engaging 3-page intro carousel that highlights SimpleX's privacy features
- Separate branded/proprietary assets from AGPL-licensed code
- Implement a clear onboarding progression: Intro → Profile → Conditions → Notifications → Hub
- Provide a "Now you can" hub screen that branches to Invite or Connect flows
- Maintain consistent dark/light theme support across all new screens
- Preserve reproducible builds for both AGPL and branded configurations
- Ensure backward compatibility for users who have already completed onboarding

## Non-Goals

- Changing the core onboarding data model or persistence logic
- Modifying server connection or SMP protocol logic
- Redesigning post-onboarding screens (chat list, conversation view, etc.)
- Adding new onboarding data collection or analytics
- Changing the Create Group flow (only adding illustrations)

---

## Design Reference

The visual designs are provided in the attached Photoshop file (`Brief_for_Developers_1.docx`).

**Key design notes:**
- Dark version groups are marked in **blue**
- Light version groups are marked in **red**
- Use standard system fonts and UI elements wherever possible
- The fonts/elements in the PSD may not match current app styling — always prefer existing app components
- Image content (headlines, subtitles) are placeholders and will be finalized later

---

## Overview

This RFC covers three main implementation areas:

1. **AGPL-compliant asset handling** — Introduce a build-time flag to optionally include branded images
2. **New multi-screen onboarding flow** — Replace single `SimpleXInfo` with carousel + hub pattern
3. **Theme support** — Ensure all new screens work correctly in dark/light modes

The default build must remain fully AGPLv3-compliant and ship without proprietary assets. Branded images (illustrations, icons) are opt-in via build configuration.

---

## New Onboarding Flow

### Screen Sequence

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           INTRO CAROUSEL                                     │
│  Intro 1 (swipe) → Intro 2 (swipe) → Intro 3 [Create profile button]        │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
                          ┌─────────────────┐
                          │  Create Profile  │
                          └─────────────────┘
                                    │
                                    ▼
                         ┌──────────────────┐
                         │ Conditions of Use │
                         └──────────────────┘
                                    │
                                    ▼
                         ┌──────────────────┐
                         │ Push Notifications │
                         └──────────────────┘
                                    │
                                    ▼
                          ┌─────────────────┐
                          │   Now You Can    │
                          └─────────────────┘
                                    │
              ┌─────────────────────┴─────────────────────┐
              ▼                                           ▼
    ┌─────────────────┐                       ┌───────────────────────┐
    │ Invite Someone  │                       │ Connect via Link/QR   │
    └─────────────────┘                       └───────────────────────┘
              │                                           │
    ┌─────────┴─────────┐                     Paste link OR Scan QR
    ▼                   ▼
┌────────────┐  ┌─────────────────┐
│ 1-time Link │  │ SimpleX Address │
└────────────┘  └─────────────────┘
```

### Screen Details

#### Intro Carousel (Pages 1–3)

Each intro page shares the same layout structure:

| Element | Description |
|---------|-------------|
| Logo (top) | SIMPLEX logo using existing `SimpleXLogo()` |
| Central graphic | Illustration (branded) OR animated cards/text (AGPL fallback) |
| Headline | Large bold text |
| Subtitle | Supporting descriptive text |
| Pagination dots | Three dots indicating current page |
| Buttons (Page 3 only) | "Create your profile" + "Migrate from another device" link |

**Content (placeholders — final copy TBC):**

| Page | Headline | Subtitle |
|------|----------|----------|
| 1 | World's Most Secure Messaging | SimpleX messaging has cutting-edge end-to-end encryption. |
| 2 | Freedom of Your Communications | You will support your favorite groups with future Community Vouchers. |
| 3 | You Own The Next Web | SimpleX is founded on the belief that you must own your identity, contacts and communities. |

#### Create Profile

- Back button (returns to Intro 3)
- Isometric illustration (conditional on `USE_BRANDED_IMAGES`)
- Title: "Create profile"
- Subtitle: "Your profile is stored on your device and only shared with your contacts."
- Name input field with placeholder: "Enter your name..."
- Primary button: "Create profile"

#### Conditions of Use

- Isometric illustration (smartphone + server graphic)
- Title: "Conditions of use"
- Privacy text: "Private chats, groups and your contacts are not accessible to server operators."
- Bullet list:
  - "By using SimpleX Chat you agree to:"
  - "send only legal content in public groups"
  - "respect other users - no spam"
- Link: "Privacy policy and conditions of use."
- Primary button: "Accept"
- Secondary links (with state icons):
  - "Configure server operators" — icon reflects current state
  - "Configure notifications" — icon reflects chosen mode

#### Push Notifications

- Title: "Push notifications"
- Link: "How it affects privacy"
- Three radio options:
  - Instant (E2E encrypted via notification server)
  - Periodic (check every 20 min)
  - No push server
- Primary button: "OK"

**Note:** This is a standard slide-up sheet on iOS, standard modal on Android. Layout remains unchanged from current implementation.

#### Now You Can

- Title: "Now you can"
- Two interactive cards:
  - **"Invite someone to chat"** — chain-link icon, tappable
  - **"Connect via link or QR code"** — QR icon, tappable
- Bottom navigation bar with: profile icon, Chats, circular "+" button
- **Change:** Square pencil icon replaced with circular plus button

#### Invite Someone

- Two expandable cards (tap to reveal additional content):
  - **"Create private 1-time link"**
    - Subtitle: "Your contact can use link or scan QR code"
    - Button: "Create private 1-time link"
  - **"Create public SimpleX address"**
    - Subtitle: "Public link for social media, email or website"
    - Button: "Create public SimpleX address"

#### Connect via Link or QR Code

- Label: "PASTE THE LINK YOU RECEIVED"
- Input field with placeholder: "Tap to paste link"
- Label: "OR SCAN QR CODE"
- Camera/QR scanner area

**Implementation note:** This is a variant of the existing scan page (without tabs). Implement by passing an option parameter to reuse the same view.

#### 1-time Link / SimpleX Address Sheets

- Same view with different parameters
- Add illustration at top (conditional on `USE_BRANDED_IMAGES`)
- Copy and Share buttons with standard icons (use existing app icons)
- Button spacing matches message compose box

#### Create Group (Enhancement)

- Add illustration to existing "Create group" modal
- Update default group icon to match chat list styling
- No other changes to existing functionality

---

## AGPL Logic Implementation

### Build-Time Flag

The flag controls whether branded images are included and displayed. When `false`, the app uses text-only or animated-card fallbacks.

```kotlin
// BuildConfigCommon.kt (or equivalent shared location)
object BuildConfigCommon {
    /**
     * When true, branded images are included and shown.
     * When false, use text/animated fallbacks.
     * Default: false (AGPL-compliant build)
     */
    val USE_BRANDED_IMAGES: Boolean = false // Set from Gradle/CI
}
```

### Android: Gradle Configuration

```kotlin
// apps/multiplatform/common/build.gradle.kts
android {
    defaultConfig {
        buildConfigField(
            "Boolean",
            "USE_BRANDED_IMAGES",
            project.findProperty("USE_BRANDED_IMAGES")?.toString()?.toBooleanStrictOrNull()?.toString() ?: "false"
        )
    }
}
```

```properties
# gradle.properties or local.properties (for branded builds)
USE_BRANDED_IMAGES=true
```

### iOS: Build Configuration

```swift
// In .xcconfig or Build Settings
USE_BRANDED_IMAGES = NO  // Default for AGPL build

// In Swift code (using build flag)
#if USE_BRANDED_IMAGES
    BrandedIllustrationView(page: pageIndex)
#else
    AnimatedCardsFallbackView(page: pageIndex)
#endif
```

Alternatively, use a Swift compile-time flag:

```swift
// Build Settings > Swift Compiler - Custom Flags
// Add: -DUSE_BRANDED_IMAGES for branded builds

#if USE_BRANDED_IMAGES
// Show branded content
#else
// Show fallback content
#endif
```

### UI Conditional Display (Compose)

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

### Asset Location Options

Assets must live outside the main AGPL repository to maintain license compliance:

| Option | Description | Pros | Cons |
|--------|-------------|------|------|
| **A: Sibling folder** | `simplex-chat-branded-assets/` in same parent directory | Simple setup, easy local dev | Requires manual clone |
| **B: Separate repo** | Dedicated repo, pulled via git submodule or CI | Clean separation, version control | More complex CI setup |
| **C: CI artifact** | Downloaded during branded CI builds only | No repo pollution | Requires artifact storage |

**Recommendation:** Option B (separate repo) for cleaner separation and independent versioning.

**Key constraint:** Logos (SimpleX logo, app icon) remain in the default AGPL build and are NOT gated by this flag.

---

## Dark/Light Mode Support

### Current Theme Infrastructure

| Platform | Implementation |
|----------|----------------|
| Android | `Theme.AppCompat.DayNight.NoActionBar`, `values-night/themes.xml`, `configChanges="uiMode"` |
| Android programmatic | `UiModeManager` (API 31+) for in-app theme selection (System/Light/Dark) |
| Compose | `CurrentColors`, `MaterialTheme.colors`, `isInDarkTheme()` |
| iOS | System appearance + override support |

### Color Mappings for New Screens

| Element | Dark Mode | Light Mode |
|---------|-----------|------------|
| Background | Black (#000000) | White (#FFFFFF) |
| Primary text (headlines) | White (#FFFFFF) | Black / dark gray (#1A1A1A) |
| Secondary text (subtitles) | Light gray (#A0A0A0) | Medium gray (#666666) |
| Accent (links, buttons, active dot) | Blue (consistent across themes) | Blue (consistent) |
| Card backgrounds | Dark gray / blue gradient | Light gray / white gradient |
| Status bar icons | White | Black |
| Inactive pagination dots | Dark gray (#404040) | Light gray (#D0D0D0) |

### Implementation Guidelines

All new composables must use theme-aware colors:

```kotlin
@Composable
fun IntroPage(/* params */) {
    Column(
        modifier = Modifier.background(MaterialTheme.colors.background)
    ) {
        Text(
            text = headline,
            color = MaterialTheme.colors.onBackground,
            style = MaterialTheme.typography.h1
        )
        Text(
            text = subtitle,
            color = MaterialTheme.colors.secondary
        )
        // For asset variants
        val logoRes = if (isInDarkTheme()) R.drawable.logo_dark else R.drawable.logo_light
    }
}
```

**No new theme infrastructure is required.** Use the existing theme system throughout.

---

## Key Files to Modify

### Build Configuration

| File | Change |
|------|--------|
| `apps/multiplatform/common/build.gradle.kts` | Add `USE_BRANDED_IMAGES` buildConfigField |
| `gradle.properties` | Optional property for branded builds |
| iOS `.xcconfig` or Build Settings | Equivalent flag for iOS |

### Onboarding Flow (Shared/Common)

| File | Change |
|------|--------|
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/SimpleXInfo.kt` | Refactor or replace with intro carousel |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/IntroCarouselView.kt` | **New:** Carousel container with HorizontalPager |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/IntroPage.kt` | **New:** Single intro page composable |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/NowYouCanView.kt` | **New:** Hub screen with Invite/Connect cards |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/App.kt` | Update onboarding stage routing |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/` | Add `Step0_IntroCarousel` to `OnboardingStage` enum |

### iOS Platform-Specific

| File | Change |
|------|--------|
| `apps/ios/Shared/Views/Onboarding/SimpleXInfo.swift` | Align with new carousel flow |
| `apps/ios/Shared/Views/Onboarding/OnboardingView.swift` | Add intro carousel stage handling |
| `apps/ios/Shared/Views/Onboarding/IntroCarouselView.swift` | **New:** SwiftUI carousel implementation |
| `apps/ios/Shared/Views/Onboarding/NowYouCanView.swift` | **New:** Hub screen |
| `apps/ios/Shared/ContentView.swift` | Routing for new onboarding stages |

### New Chat / Invite / Connect

| File | Change |
|------|--------|
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/newchat/NewChatView.kt` | Adapt to card-based Invite/Connect layout |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/newchat/NewChatSheet.kt` | Entry points from "Now you can" hub |
| `apps/ios/Shared/Views/NewChat/NewChatView.swift` | Matching card layout and flow |

### Conditions and Notifications

| File | Change |
|------|--------|
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/OnboardingConditionsView.kt` | Add illustration, update layout |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/SetNotificationsMode.kt` | Minor layout updates |
| Corresponding iOS SwiftUI views | Match Compose changes |

### Resources

| Location | Change |
|----------|--------|
| `apps/multiplatform/common/src/commonMain/resources/MR/` | New string resources |
| Sibling repo or folder (TBC) | Branded illustrations |

---

## New String Resources

| Key | EN Value (placeholder) |
|-----|------------------------|
| `intro_headline_1` | "World's Most Secure Messaging" |
| `intro_subtitle_1` | "SimpleX messaging has cutting-edge end-to-end encryption." |
| `intro_headline_2` | "Freedom of Your Communications" |
| `intro_subtitle_2` | "You will support your favorite groups with future Community Vouchers." |
| `intro_headline_3` | "You Own The Next Web" |
| `intro_subtitle_3` | "SimpleX is founded on the belief that you must own your identity, contacts and communities." |
| `now_you_can_title` | "Now you can" |
| `invite_someone_card` | "Invite someone to chat" |
| `connect_via_link_card` | "Connect via link or QR code" |
| `create_1time_link` | "Create private 1-time link" |
| `create_1time_link_desc` | "Your contact can use link or scan QR code" |
| `create_simplex_address` | "Create public SimpleX address" |
| `create_simplex_address_desc` | "Public link for social media, email or website" |
| `paste_link_label` | "PASTE THE LINK YOU RECEIVED" |
| `paste_link_placeholder` | "Tap to paste link" |
| `scan_qr_label` | "OR SCAN QR CODE" |

---

## Reusable Components

### From Existing Codebase

| Component | Source | Reuse For |
|-----------|--------|-----------|
| `SimpleXLogo()` | `SimpleXInfo.kt` | Intro pages header |
| `HorizontalPager` pattern | `NewChatView.kt` | Intro carousel |
| `OnboardingActionButton` | `SimpleXInfo.kt` | "Create your profile" button |
| `TextButtonBelowOnboardingButton` | `SimpleXInfo.kt` | "Migrate from another device" link |
| `ModalView` | `helpers/ModalView.kt` | Full-screen modals |
| `InfoRow` | `SimpleXInfo.kt` | Bullet-style rows |

### New Components to Create

| Component | Description |
|-----------|-------------|
| `IntroPage` | Parametrized layout: logo, central content slot, headline, subtitle, pagination, optional buttons |
| `IntroCarousel` | `HorizontalPager` wrapper + page indicators + state management |
| `OnboardingCard` | Tappable card with illustration, title, description, action callback |
| `PageIndicator` | Dot-style pagination indicator |

---

## Implementation Steps

### Phase 0: Build Configuration (Est: 1 day)

1. Add `USE_BRANDED_IMAGES` to `BuildConfigCommon` in `common/build.gradle.kts`
2. Add optional Gradle property documentation
3. Add equivalent flag for iOS build (`.xcconfig` or scheme setting)
4. Verify both configurations compile successfully

### Phase 1: Intro Carousel (Est: 3-4 days)

1. Create `IntroPage` composable with parametrized content slots
2. Create `IntroCarouselView` using `HorizontalPager` and `PageIndicator`
3. Add `Step0_IntroCarousel` to onboarding stage enum
4. Wire Intro 3 → "Create your profile" → existing `CreateFirstProfile`
5. Implement conditional central content (branded image vs animated/text fallback)
6. Add corresponding iOS SwiftUI implementation

### Phase 2: Create Profile and Conditions (Est: 2-3 days)

1. Update `CreateFirstProfile` layout (illustration slot, spacing, input styling)
2. Update `OnboardingConditionsView` (illustration, bullets, links, state icons)
3. Ensure both screens use theme colors correctly
4. Test dark/light mode switching

### Phase 3: Notifications and Now You Can (Est: 2-3 days)

1. Minor layout updates to `SetNotificationsMode`
2. Create `NowYouCanView` with two `OnboardingCard` components
3. Wire "Now you can" → main chat list and Invite/Connect entry points
4. Replace pencil icon with circular plus button

### Phase 4: Invite and Connect Screens (Est: 2-3 days)

1. Update Invite flow to card-based expandable layout
2. Update Connect flow (paste link input, QR scan area)
3. Add illustrations to link creation sheets (conditional)
4. Ensure reuse of existing scan/create views via parameters

### Phase 5: Strings, Assets, and Polish (Est: 2 days)

1. Add all new string resources to MR (both platforms)
2. Set up branded asset repository/folder structure
3. Verify reproducible builds for both configurations
4. Add Create Group illustration

---

## Migration

### Users Currently Mid-Onboarding

- Users who haven't completed onboarding will restart from Intro 1 on app update
- Rationale: Cleaner experience than trying to map partial old state to new flow
- Low impact: Onboarding is typically completed in one session

### Users Who Completed Onboarding

- No impact — proceed directly to chat list as before
- Onboarding state in local database remains valid

### Rollback Plan

If critical issues arise post-release:
1. Revert to previous `SimpleXInfo` single-screen implementation
2. Keep `USE_BRANDED_IMAGES` flag infrastructure for future use
3. Onboarding state remains compatible

---

## Testing Considerations

### Functional Tests

| Test Case | Expected Result |
|-----------|-----------------|
| Default AGPL build | App compiles and runs; text/animated fallbacks display; no branded images |
| Branded build (`USE_BRANDED_IMAGES=true`) | Illustrations load on intro, profile, conditions, invite cards |
| Full onboarding flow | Intro 1 → ... → Now you can → Invite OR Connect completes successfully |
| Migrate from another device | Link on Intro 3 navigates to migration flow |
| Back navigation | Each screen returns to previous correctly |

### Theme Tests

| Test Case | Expected Result |
|-----------|-----------------|
| System dark mode | All new screens render with dark theme colors |
| System light mode | All new screens render with light theme colors |
| In-app theme switch | Screens update immediately without restart |
| No inverted/unreadable text | All text remains legible in both themes |

### Compatibility Tests

| Test Case | Expected Result |
|-----------|-----------------|
| Android 8.0+ | Flow works correctly |
| iOS 14+ | Flow works correctly |
| Existing users past onboarding | See main UI directly, no regression |
| Fresh install | Full new onboarding flow presented |

### Reproducible Build Tests

| Test Case | Expected Result |
|-----------|-----------------|
| AGPL reproducible build | Identical APK/IPA hash across builds |
| Branded reproducible build | Identical APK/IPA hash when assets available |

---

## Risks and Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| Branded asset repo unavailable during CI | Low | Medium | Cache assets in CI; fail gracefully to text fallback |
| Theme colors don't match design exactly | Medium | Low | Use design tokens; iterative review with designer |
| Breaking existing UI automation tests | Medium | Medium | Update test selectors; add new tests for carousel |
| Users confused by new flow | Low | Low | Clear progression; "Migrate" option visible on Intro 3 |
| Performance regression on older devices | Low | Medium | Profile carousel animations; use simple fallbacks |

---

## Acceptance Criteria

### Must Have (P0)

- [ ] Default AGPL build compiles and runs without branded images
- [ ] Branded build shows all illustrations when `USE_BRANDED_IMAGES=true`
- [ ] 3-page intro carousel is swipeable with page indicators
- [ ] "Create your profile" on Intro 3 proceeds to profile creation
- [ ] "Now you can" hub presents Invite and Connect options
- [ ] All new screens support dark and light mode
- [ ] Full onboarding flow works on Android 8+ and iOS 14+

### Should Have (P1)

- [ ] Reproducible builds work for both AGPL and branded configurations
- [ ] Animated card fallback looks polished (not just placeholder text)
- [ ] Conditions screen shows operator state icons correctly
- [ ] Create Group modal includes illustration

### Nice to Have (P2)

- [ ] Smooth page transition animations in carousel
- [ ] Haptic feedback on card taps (where supported)

---

## Design Decisions (Confirmed)

| Decision | Resolution |
|----------|------------|
| AGPL flag type | Build-time only; no runtime toggle. Default `false`. |
| Asset location | TBC — sibling folder or separate repo. Must support reproducible builds. |
| Logos in AGPL build | Yes — logos remain in default build, not gated by flag. |
| Intro page count | Three swipeable pages before Create profile. |
| "Now you can" placement | After Conditions and Notifications; acts as hub. |
| Theme infrastructure | Use existing system; no new theme framework needed. |
| Scan/Create view reuse | Pass parameters to existing views rather than duplicate. |

---

## Open Questions

1. **Asset repository structure** — Final decision on sibling folder vs separate repo?
2. **Intro content** — Final copy for headlines and subtitles?
3. **Animated fallback design** — Specific animation/text layout for AGPL build?
4. **Migration flow entry** — Should "Migrate from another device" also appear elsewhere?
5. **Analytics** — Any onboarding completion tracking requirements?

---

## References

- Design brief: `Brief_for_Developers_1.docx`
- Related RFC: [simplexmq/rfcs/2026-01-30-send-file-page.md](https://github.com/simplex-chat/simplexmq/blob/ep/xftp-web/rfcs/2026-01-30-send-file-page.md)
- Existing onboarding: `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/onboarding/`
