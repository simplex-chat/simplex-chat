# Onboarding Cards — iOS Implementation Plan

## Scope

Screens 1 and 2 only — two card selection screens with slide navigation between them. No standalone onboarding variants of existing views. No banner. Those are separate future work.

## New file

`Shared/Views/NewChat/OnboardingCards.swift` — all new code in one file.

## What it contains

### `OnboardingCardView` — reusable card component

```swift
struct OnboardingCardView: View {
    @Environment(\.colorScheme) var colorScheme
    let imageName: String      // base asset name (without -light suffix)
    let icon: String           // SF Symbol name
    let title: LocalizedStringKey
    let subtitle: LocalizedStringKey?  // nil for screen 1 cards
    let action: () -> Void
}
```

Image selection follows the project convention:
- `colorScheme == .light` → `imageName` (base name, dark-colored image for light backgrounds)
- `colorScheme == .dark` → `"\(imageName)-light"` (light-colored image for dark backgrounds)

Note: this only works when the base name does NOT already contain `-light`. The card image base names are like `card-let-someone-connect-to-you-alpha` — the `-alpha` suffix distinguishes them, and appending `-light` gives `card-let-someone-connect-to-you-alpha-light`. Correct.

Structure (inside → out):
1. `Button(action:)` wrapping the entire card for tap handling, with `.buttonStyle(.plain)` to prevent default blue tint
2. Clipped to `RoundedRectangle(cornerRadius: 18)`
3. Inside, `ZStack(alignment: .bottom)`:
   - `LinearGradient` filling the card shape
   - `VStack(spacing: 0)`:
     - `#if SIMPLEX_ASSETS` block: `Image` with `.resizable().scaledToFit().frame(maxWidth: .infinity, maxHeight: .infinity)` — takes all space above label. Image uses `.clipped()` to prevent overflow into label area.
     - `#else` block: `Spacer()` — gradient-only card, label still functional.
     - Label area with fixed height: `HStack(spacing: 8)` with `Image(systemName: icon)` (20pt) + `VStack(alignment: .leading, spacing: 2)` containing title + optional subtitle. Padded `(.horizontal, 16)` and `(.vertical, 12)`.

Gradient stops (using `Color(red:green:blue:)` with values 0-1, no hex extension exists in the project):
- Light:
  - `Color(red: 0.824, green: 0.910, blue: 1.0)` at 0.0     (#d2e8ff)
  - `Color(red: 0.800, green: 0.914, blue: 1.0)` at 0.5     (#cce9ff)
  - `Color(red: 0.875, green: 1.0, blue: 1.0)` at 0.9       (#dfffff)
  - `Color(red: 1.0, green: 0.988, blue: 0.918)` at 1.0     (#fffcea)
- Dark:
  - `Color(red: 0.016, green: 0.039, blue: 0.141)` at 0.4   (#040a24)
  - `Color(red: 0.220, green: 0.329, blue: 0.671)` at 0.72  (#3854ab)
  - `Color(red: 0.659, green: 0.929, blue: 0.953)` at 0.9   (#a8edf3)
  - `Color(red: 1.0, green: 0.965, blue: 0.878)` at 1.0     (#fff6e0)
- Angle: 80° from vertical = 10° from horizontal. `LinearGradient(stops:..., startPoint: .init(x: 0.0, y: 0.6), endPoint: .init(x: 1.0, y: 0.4))`. Must verify visually — the exact start/end points for 80° depend on the view's aspect ratio. May need adjustment.

Define the gradient stops as static properties on `OnboardingCardView` to avoid recomputing them on every recomposition.

Label text styles:
- Title: `.body` weight `.semibold`, color `Color.white` in dark mode, `Color.primary` in light mode (from design: dark text on light gradient, light text on dark gradient).
- Subtitle: `.footnote`, color `.secondary` (adapts to theme).
- Icon: same color as title.

### `TalkToSomeoneView` — Screen 1

```swift
struct TalkToSomeoneView: View {
    @EnvironmentObject var theme: AppTheme
    @State private var showConnectWithSomeone = false
    @State private var showConnectViaLink = false
```

Body — NOT scrollable, fills available space:

```swift
var body: some View {
    VStack(spacing: 16) {
        Text("Talk to someone")
            .font(.largeTitle)
            .fontWeight(.bold)
            .frame(maxWidth: .infinity, alignment: .leading)
            .padding(.horizontal, 16)

        OnboardingCardView(
            imageName: "card-let-someone-connect-to-you-alpha",
            icon: "link",
            title: "Let someone connect to you",
            subtitle: nil,
            action: { showConnectWithSomeone = true }
        )
        .frame(maxHeight: .infinity)
        .padding(.horizontal, 16)

        OnboardingCardView(
            imageName: "card-connect-via-link-alpha",
            icon: "qrcode",
            title: "Connect via link or QR code",
            subtitle: nil,
            action: { showConnectViaLink = true }
        )
        .frame(maxHeight: .infinity)
        .padding(.horizontal, 16)
    }
    .padding(.vertical, 16)
    .background(
        NavigationLink(isActive: $showConnectWithSomeone) {
            ConnectWithSomeoneView()
        } label: { EmptyView() }
    )
    .background(
        NavigationLink(isActive: $showConnectViaLink) {
            NewChatView(selection: .connect, showQRCodeScanner: true)
                .navigationBarTitleDisplayMode(.inline)
        } label: { EmptyView() }
    )
}
```

Key layout decisions:
- `.frame(maxHeight: .infinity)` on each card makes them share remaining vertical space equally after the title takes its natural height.
- `.padding(.vertical, 16)` on the VStack adds 16pt above the title and 16pt below the second card (VStack `spacing` only applies between children, not before first or after last).
- Hidden `NavigationLink(isActive:)` in `.background()` — drives navigation without affecting layout. This is the deprecated iOS 15 API but it works on iOS 16+ inside `NavigationStack` and is used throughout the existing codebase (e.g., `NewChatMenuButton.swift` lines 100-110).

**`oneHandUI` inversion handling:** `TalkToSomeoneView` replaces `chatList` content. `chatListView` applies `.scaleEffect(x: 1, y: oneHandUI ? -1 : 1)` to the root page. The onboarding view gets inverted. It must counter-invert with `.scaleEffect(x: 1, y: oneHandUI ? -1 : 1)`. This is applied in `ChatListView.chatList`, NOT inside `TalkToSomeoneView` — the caller is responsible. When NavigationLink pushes Screen 2 or further views, those are new navigation pages outside the root page's scale effect, so they render normally.

### `ConnectWithSomeoneView` — Screen 2

```swift
struct ConnectWithSomeoneView: View {
    @EnvironmentObject var theme: AppTheme
    @State private var showInviteSomeone = false
    @State private var showCreateAddress = false
```

Same VStack layout as Screen 1, with these differences:
- Title: "Connect with someone"
- Card 1: imageName `"card-invite-someone-privately-alpha"`, icon `"link"`, title "Invite someone privately", subtitle "A link for one person to connect" → sets `showInviteSomeone = true`
- Card 2: imageName `"card-create-your-public-address-alpha"`, icon `"qrcode"`, title "Create your public address", subtitle "For anyone to reach you" → sets `showCreateAddress = true`

Navigation destinations (existing views, unmodified — onboarding variants are future work):
- `showInviteSomeone` → `NewChatView(selection: .invite)` — tabbed view, 1-time link tab pre-selected. Has tabs (not ideal) but functional.
- `showCreateAddress` → `UserAddressView(shareViaProfile: false, autoCreate: true)` — auto-creates address on appear.

Both wrapped with `.navigationBarTitleDisplayMode(.inline)`.

Navigation bar back button shows automatically (pushed via NavigationLink within the stack).

## Integration into ChatListView

### In `chatList` property (line 351 of ChatListView.swift)

Current code:
```swift
private var chatList: some View {
    let cs = filteredChats()
    return ZStack {
        ScrollViewReader { scrollProxy in
            List { ... }
        }
    }
}
```

Changed to:
```swift
@ViewBuilder
private var chatList: some View {
    if shouldShowOnboarding {
        TalkToSomeoneView()
            .scaleEffect(x: 1, y: oneHandUI ? -1 : 1, anchor: .center)
    } else {
        let cs = filteredChats()
        ZStack {
            ScrollViewReader { scrollProxy in
                List { ... }
            }
        }
    }
}
```

Requires `@ViewBuilder` because `if/else` returns different view types.

**`oneHandUI` inversion:** The `.scaleEffect(y: -1)` is applied by `chatListView` to the root page of the navigation stack. `TalkToSomeoneView` counter-inverts at the call site. When `NavigationLink` pushes Screen 2 or further, those are new navigation pages NOT affected by the root page's `.scaleEffect`. Only the root content needs the flip.

### `shouldShowOnboarding` and `noConversationChatsYet`

```swift
private var shouldShowOnboarding: Bool {
    !addressCreationCardShown && noConversationChatsYet
}

private var noConversationChatsYet: Bool {
    chatModel.chats.allSatisfy { chat in
        switch chat.chatInfo {
        case .local: return true
        case let .direct(contact): return contact.chatDeleted || contact.isContactCard
        case let .group(groupInfo, _): return groupInfo.chatDeleted
        case let .contactRequest(req): return req.chatDeleted
        case let .contactConnection(conn): return conn.chatDeleted
        case .invalidJSON: return true
        }
    }
}
```

Both are computed properties on `ChatListView`. `noConversationChatsYet` reads `chatModel.chats` which is `@Published` on `ChatModel` (`@EnvironmentObject`). SwiftUI re-evaluates the body when it changes, so `shouldShowOnboarding` is reactive.

Note: `chatModel.chats` may be empty during initial load (before `APIGetChats` completes). `allSatisfy` on an empty array returns `true`. Combined with `!addressCreationCardShown`, this means the onboarding flashes briefly on app launch for users who have conversations but `chats` hasn't loaded yet. Mitigation: also check `chatModel.chats.isEmpty` and show a loading indicator instead:

```swift
private var shouldShowOnboarding: Bool {
    !addressCreationCardShown && !chatModel.chats.isEmpty && noConversationChatsYet
}
```

When `chats` is empty (loading), neither onboarding nor chat list shows — the existing loading state (if any) handles it.

### Auto-dismiss

`addressCreationCardShown` must be set to `true` when the first real conversation appears, so the onboarding never returns.

```swift
.onChange(of: chatModel.chats.count) { _ in
    if !noConversationChatsYet && !addressCreationCardShown {
        addressCreationCardShown = true
    }
}
```

Placed on `chatList` view. Observes `.count` as a proxy for chat list changes. When count changes and `noConversationChatsYet` is false, the user default is set permanently. This covers: receiving a contact request, establishing a connection, creating a group, etc.

Edge case: chat count can change without affecting `noConversationChatsYet` (e.g., adding a second note folder). The check `!noConversationChatsYet` prevents unnecessary writes — only sets the default when there's actually a real conversation.

## User default

Existing `@AppStorage(DEFAULT_ADDRESS_CREATION_CARD_SHOWN) private var addressCreationCardShown = false` at ChatListView line 165. Constant defined in `SettingsView.swift` line 55 as `let DEFAULT_ADDRESS_CREATION_CARD_SHOWN = "addressCreationCardShown"`. Also referenced in `AddressCreationCard.swift` line 17 and in `SettingsView.swift` defaults reset (line 114, 144).

No new user default needed.

## String localization

8 new strings for `Localizable.strings` (en). Use `NSLocalizedString` or `LocalizedStringKey` inline — project uses both patterns.

- "Talk to someone"
- "Let someone connect to you"
- "Connect via link or QR code"
- "Connect with someone"
- "Invite someone privately"
- "A link for one person to connect"
- "Create your public address"
- "For anyone to reach you"

## Assets

8 card images in art repo (4 base + 4 light variants). Run `resize.sh` then `copy-assets.sh` to populate `SimpleXAssets.xcassets`. Gated with `#if SIMPLEX_ASSETS`. Without assets: gradient-only cards with labels, still tappable and functional.

Image base names for the `imageName` parameter:
- Screen 1: `"card-let-someone-connect-to-you-alpha"`, `"card-connect-via-link-alpha"`
- Screen 2: `"card-invite-someone-privately-alpha"`, `"card-create-your-public-address-alpha"`

The `-light` suffix is appended automatically by `OnboardingCardView` when `colorScheme == .dark`.

## Files changed

- `Shared/Views/ChatList/ChatListView.swift` — add `shouldShowOnboarding`, `noConversationChatsYet`, add `@ViewBuilder` to `chatList`, branch to `TalkToSomeoneView`, add `.onChange` for auto-dismiss
- **New:** `Shared/Views/NewChat/OnboardingCards.swift` — `OnboardingCardView`, `TalkToSomeoneView`, `ConnectWithSomeoneView`

No modifications to NewChatView, UserAddressView, or ConnectView in this phase.

## Revision 1 — corrections from design review

### Navigation scope (critical)
Both screens must keep the bottom/top toolbar visible. The onboarding NavigationView is SCOPED to just the card area — it does NOT replace the full chatListView. In `chatList`, wrap `TalkToSomeoneView()` in its own `NavigationView { }.navigationViewStyle(.stack)`. The toolbar from `chatListView.withToolbar()` stays outside and visible on both screens.

Screen 1 → Screen 2: real NavigationLink push within the scoped NavigationView.
Screen 2 → deeper views: also NavigationLink pushes within same scoped NavigationView.

### Screen 1 — reserve nav bar space
Screen 2 has a back button (navigation bar). Screen 1 must reserve the same height to prevent content shift on slide. Set `.navigationTitle("")` with `.navigationBarTitleDisplayMode(.inline)` on Screen 1's root — shows an empty inline nav bar matching Screen 2's bar height.

### Gradient direction fix
Current gradient is nearly horizontal — wrong. Correct angle is 80° CCW from horizontal (almost vertical, slight rightward lean).

Formula for full-coverage gradient at angle θ:
```
startPoint = (0.5 - 0.5·cos(θ), 0.5 + 0.5·sin(θ))
endPoint   = (0.5 + 0.5·cos(θ), 0.5 - 0.5·sin(θ))
```

For θ = 80°: `startPoint: .init(x: 0.413, y: 0.992), endPoint: .init(x: 0.587, y: 0.008)`

### Corner radius
Change from 18 to 24.

### Label stripe background
The label area has a distinct semi-transparent background strip at the bottom of the card. Add to `labelRow`:
- Light mode: `Color.white.opacity(0.5)`
- Dark mode: `Color.black.opacity(0.3)`
Exact opacity values need visual tuning.

### Card max height ratio
Cards have a max total height/width ratio of 0.75. On tall screens, cards are capped at this ratio with extra space distributed equally above and below. On short screens, cards shrink — ratio goes below 0.75, label stripe stays fixed height, only image area shrinks.

Implementation: use `GeometryReader` to get available width, compute `maxCardHeight = cardWidth * 0.75`, apply `.frame(maxHeight: maxCardHeight)` on each card. The VStack centers vertically in the GeometryReader — equal space above and below on tall screens.

### Title alignment
Change from `.leading` to `.center` — design shows centered titles on both screens.

### Subtitle color in dark mode
Change `.foregroundColor(.secondary)` to `.foregroundColor(colorScheme == .dark ? .white.opacity(0.7) : .secondary)` — standard `.secondary` is too gray on the dark gradient.

### Label stripe height proportions
The label stripe has fixed proportional heights relative to card width:
- Screen 1 (single-line labels): 0.132 × card width
- Screen 2 (two-line labels): 0.195 × card width

These are achieved via fixed padding on the label row. The image area is the remainder of the card height. When cards shrink on short screens, only the image area shrinks — the label stripe stays at its proportional height.

### Spacing between title and cards, between cards, and below cards
The gaps above first card and below second card should be EQUAL and LARGER than the gap between the two cards. The inter-card gap is the VStack spacing (~16pt). The outer gaps are larger — achieved by the GeometryReader centering the VStack vertically, which distributes extra space equally above and below.

### ThemedBackground on TalkToSomeoneView
`TalkToSomeoneView` needs `.modifier(ThemedBackground())` — it replaces `chatList` content and needs its own background. Currently missing.

### `oneHandUI` inversion on Screen 2
The scoped `NavigationView` sits inside `chatList` which is visually inverted by `chatListView`'s `.scaleEffect(y: -1)`. This inversion applies to the NavigationView's rendered frame — ALL pages inside it (both Screen 1 and Screen 2) are inverted. `TalkToSomeoneView` counter-inverts at the call site. `ConnectWithSomeoneView` (pushed within the NavigationView) also needs counter-inversion. Pass `oneHandUI` as a binding or read from `@AppStorage(GROUP_DEFAULT_ONE_HAND_UI)` directly inside `ConnectWithSomeoneView`, and apply `.scaleEffect(x: 1, y: oneHandUI ? -1 : 1)` on its root VStack. Same for any deeper pushed views — but those are existing views not modified in this phase, so their inversion behavior needs testing.

### Plan cleanup note
The original sections above contain outdated code snippets (wrong gradient, wrong corner radius, wrong switch cases, wrong alignment). The Revision 1 sections are authoritative. When implementing, follow Revision 1 values; treat original sections as structural context only.

