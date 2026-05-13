# RTL layout regressions (chat bubble tail, app bar, directional icons)

Design doc for the fix shipped in PR #6908 (Fixes #5448).

## Problem

Under an RTL locale (Arabic, Persian, Hebrew) on Android and desktop:

- Chat bubble tail points away from the message author's profile, with
  the tail overflowing onto the avatar.
- Chat header places the call + 3-dot menu on the visual right and the
  back chevron on the visual left, opposite of platform convention; the
  3-dot dropdown opens off-screen.
- Chat list header has the profile avatar on the visual left and the
  signal/wifi icon + "Chats" title on the right.
- Asymmetric directional drawables — back/forward arrows, chevrons,
  signal-strength waves, sent-via-proxy arrow — keep their LTR
  orientation everywhere they appear.

iOS is unaffected — UIKit's RTL support and SwiftUI's semantic edges
already handle these cases.

## Background — three independent mechanisms

The user-visible regressions all come from one of three roots, each
fixed at the root rather than per call site:

1. **Bubble shape mirroring** — `chatItemShape` in
   `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt`.
2. **App-bar slot placement** — `CenteredRowLayout` in
   `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt`,
   reached by every `DefaultAppBar` caller via `AppBarCenterAligned`
   (≈ 11 sites: `ChatView`, `ChatListView`, `NewChatSheet` ×2,
   `MemberSupportChatView` ×2, `GroupReportsView`, `ModalView`,
   `ShareListView`, `SelectableChatItemToolbars`).
3. **Asymmetric directional drawables** — applied at each Icon call
   site whose vector resource is intrinsically directional.

## Fix

### 1. Bubble tail direction

`chatItemShape` is a `GenericShape`. Compose already passes
`LayoutDirection` into the shape's lambda; the original signature
discarded it (`{ size, _ ->`). Capture it (`{ size, layoutDirection ->`)
and flip the mirror predicate from `sent` to
`sent != (layoutDirection == LayoutDirection.Rtl)`.

```kotlin
// Default path draws the tail at the bottom-left corner.
val tailOnRight = sent != (layoutDirection == LayoutDirection.Rtl)
if (tailOnRight) {
  val matrix = Matrix()
  matrix.scale(-1f, 1f)
  this.transform(matrix)
  this.translate(Offset(size.width, 0f))
}
```

The bubble-tail `padding(start, end)` in `FramedItemView.kt` and
`ChatView.kt` is already direction-aware, so the corrected shape falls
into place against the existing padding under both directions
automatically.

### 2. App-bar slot placement

`CenteredRowLayout` is a custom `Layout` with three slots
(leading / center / trailing). It placed slots with absolute `place(x, y)`,
which doesn't react to layout direction. Switch all three calls to
`placeRelative(x, y)` — the standard Compose primitive that mirrors the
x coordinate under RTL.

```kotlin
first.placeRelative(0, ...)
second.placeRelative((maxWidth - second.width) / 2, ...)
third.placeRelative(maxWidth - third.width, ...)
```

One source of truth, ≈ 11 call sites covered. `placeRelative` is
identical to `place` under LTR — bit-exact LTR preservation.

`DropdownMenu` `DpOffset(-width.value, ...)` calls in `ChatView.kt`
are intentionally untouched: `DropdownMenuPositionProvider` already
negates the content offset under RTL, so once `CenteredRowLayout` is
fixed the chat-header menus anchor correctly without a call-site change.

### 3. Asymmetric directional drawables

Add `Modifier.mirrorIfRtl()` in
`apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/helpers/Modifiers.kt`:

```kotlin
@Composable
fun Modifier.mirrorIfRtl(): Modifier =
  if (LocalLayoutDirection.current == LayoutDirection.Rtl)
    this.scale(scaleX = -1f, scaleY = 1f)
  else this
```

`Modifier.scale` is a draw-time transform: it does not change measured
size or hit-test bounds. For the icons in question (square or
near-square) it is visually identical to a mirrored asset and click
areas remain correct.

Apply at each call site whose drawable is intrinsically directional:

- back chevron (`NavigationButtonBack`, `OnboardingCards`'s `BackButton`,
  `ImageFullScreenView.desktop`, `WhatsNewView`'s "previous" pager,
  `CommandsMenuView`)
- forward chevron / arrow (`NewChatView`'s share-profile chevron,
  `WhatsNewView`'s "next" pager, `CommandsMenuView`'s submenu chevron,
  `GoToItemInnerButton`'s forward variant)
- subscription-status waves (5 variants in `SubscriptionStatusIcon`)
- sent-via-proxy arrow (`CIMetaView`, `ChatItemInfoView`)

Two structural details for site-level callers:

- **`SubscriptionStatusIcon`** renders the same `modifier` parameter at
  five Icon callsites within a single composable. Hoist the mirror once
  (`val iconModifier = modifier.mirrorIfRtl()`) and reuse — calling
  `mirrorIfRtl()` once or five times yields the same `Modifier` value
  inside one composition.
- **`GoToItemInnerButton`** is a private helper that renders both a
  directional icon (`ic_arrow_forward`, "go to forwarded source") and a
  non-directional icon (`ic_search`, search-result indicator) through
  the same `Icon(painterResource(icon), ...)` call. Add a
  `mirror: Boolean = false` parameter and pass `mirror = true` only at
  the directional callsite. The `ic_search` callsite keeps its default
  and is unchanged.

## Why this is correct under both layout directions

| direction | sent | mirror? | tail position | profile side | OK |
|---|---|---|---|---|---|
| LTR | true  | yes | bottom-right | sender right | ✓ |
| LTR | false | no  | bottom-left  | contact left | ✓ |
| RTL | true  | no  | bottom-left  | sender left  | ✓ |
| RTL | false | yes | bottom-right | contact right | ✓ |

The XOR predicate `sent != isRtl` collapses to `sent` under LTR — bit-
exact LTR preservation.

`placeRelative` is identical to `place` under LTR. `mirrorIfRtl()`
returns the receiver unchanged under LTR. No LTR location regresses.

## Why a wider structural change is not in scope here

These are deliberately deferred:

1. **`GroupSndStatus.Forwarded`'s `ic_chevron_right_2`** in member
   delivery status (long-press → Info → Delivery). Cleanly mirroring it
   requires extending `GroupSndStatus.statusIcon()`'s return type so a
   single `when (this)` produces `(icon, color, mirror)` — a
   consumer-side `is Forwarded` check would dispatch on `status` a
   second time and couple the consumer to status sub-types. The proper
   fix is a model API change, deserving its own commit.

2. **Two settings rows** route their icon through
   `SimpleButtonIconEnded` and `SettingsPreferenceItem`, which don't
   accept a per-icon `Modifier`. Fixing them requires extending those
   helper signatures.

3. **iOS-side**: not addressed because UIKit's RTL support and SwiftUI's
   semantic edges already handle these cases.

The PR aims at the user-visible regressions called out in the issue;
deferring the above keeps the diff focused (+35/−16 across 12 files)
and the regression risk low.

## Verification

Manual (Android + desktop) — switch in-app language to Arabic / Persian /
Hebrew (or system locale to RTL) and walk through the screens called
out in the issue:

- Open any chat — sent-message tails point to your profile (visual side
  closest to your avatar in 2-pane mode); received-message tails point
  to the contact's avatar.
- Chat header — call + 3-dot menu on the visual left, back chevron on
  the visual right and pointing right (`>`). Tapping the 3-dot menu
  opens a dropdown that anchors on the left and stays on-screen.
- Settings → Your settings — back chevron points `>` and is on the right
  edge of the navigation header.
- Chat list header — profile avatar on the right; "Chats" title and
  signal/wifi icon on the left, with the signal icon's "waves" mirrored.
- Settings → About → What's new — pagination chevrons mirror.
- Sent-via-proxy badge in chat-item meta and message info — arrow
  mirrors.
- Switch back to English — every above location matches pre-PR behavior;
  no LTR regression.

## Alternatives considered and rejected

- **`android:autoMirrored="true"` on the vector drawables.** Would set
  the mirror policy once per asset instead of once per call site. The
  project's vector drawables are generated by moko-resources from SVGs,
  and the SVG → vector XML pipeline doesn't currently propagate that
  flag. Per-call-site `mirrorIfRtl()` is the available alternative; if
  moko-resources adds the flag, the per-asset approach becomes cleaner
  and the modifier calls can be removed.

- **A `MirroredIcon` wrapper composable.** Each call site has different
  surrounding modifiers (`size`, `height`, `padding`), tints, and
  content descriptions. A wrapper would have to forward many parameters
  and produce a worse interface than direct `.mirrorIfRtl()` in the
  modifier chain.

- **Mirror `ic_search` inside `GoToItemInnerButton`.** Material Design
  recommends mirroring magnifying-glass icons under RTL (handle to
  bottom-left), but the original PR doesn't mirror search icons
  elsewhere and the issue doesn't call them out. Out of scope; revisit
  in a focused pass over search-icon directionality if reported.

- **Fix Forwarded delivery status icon by gating on
  `status is GroupSndStatus.Forwarded` at the consumer.** Considered
  during review and rejected: it dispatches on `status` a second time
  (after `statusIcon()` already did), couples the consumer to specific
  sub-types, and risks drift if a future status sub-type adopts a
  directional icon. The proper fix is to fuse the dispatch by extending
  `statusIcon()`'s return — deferred to its own commit.
