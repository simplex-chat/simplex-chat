# RTL layout regressions (chat bubble tail, app bar, directional icons)

Design doc for the fix shipped in PR #6908 (Fixes #5448).

## Problem

Under an RTL locale (Arabic, Persian, Hebrew) on Android and desktop:

- Chat bubble tail points away from the message author's profile, with
  the bubble's avatar-side edge overlapping the avatar.
- Chat header places the call + 3-dot menu on the visual right and the
  back chevron on the visual left, opposite of platform convention; the
  3-dot dropdown opens at the visual left, away from the 3-dot button.
- Chat list header has the profile avatar on the visual left, and the
  signal icon's waves keep their LTR orientation.
- Asymmetric directional drawables — back/forward arrows, chevrons,
  signal-strength waves, sent-via-proxy arrow — keep their LTR
  orientation everywhere they appear.

iOS is not changed. Its bubble shape has the same defect
(`ChatItemClipShape.swift`), but the iOS app ships no Arabic, Hebrew or
Persian localisation, so it never runs right-to-left.

## Background — four independent mechanisms

The user-visible regressions all come from one of four roots. The
layouts and the bubble shape are fixed once where they are defined; the
directional drawables are fixed at each call site:

1. **Bubble shape mirroring** — `chatItemShape` in
   `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/ChatItemView.kt`.
2. **App-bar slot placement** — `CenteredRowLayout` in
   `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/item/FramedItemView.kt`,
   reached by every centred `DefaultAppBar` (not in search mode, and
   with a title or at the bottom in one-hand mode) via `AppBarCenterAligned`
   (10 sites: `ChatView`, `ChatListView`, `NewChatSheet` ×2,
   `MemberSupportChatView` ×2, `GroupReportsView`, `ModalView`,
   `ShareListView`, `SelectableChatItemToolbars`).
3. **Asymmetric directional drawables** — applied at each Icon call
   site whose vector resource is intrinsically directional.
4. **Other custom layouts placing children at absolute x** — a short
   text under an image, file or voice message (`PriorityLayout`), the
   group member name row above a message (`DependentLayout`) and the
   supporter banner (`SupportSimpleXBanner`).

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
child's position under RTL (it is placed at `parentWidth - width - x`).

```kotlin
first.placeRelative(0, ...)
second.placeRelative((maxWidth - second.width) / 2, ...)
third.placeRelative(maxWidth - third.width, ...)
```

One source of truth, 10 call sites covered. `placeRelative` is
identical to `place` under LTR — bit-exact LTR preservation.

`DropdownMenu` `DpOffset(-width.value, ...)` calls in `ChatView.kt`
are intentionally untouched: the menu hangs from its own anchor box,
which already follows the layout direction, and
`DropdownMenuPositionProvider` negates the offset under RTL. On master
the menu already opened at the visual left; the fix moves the 3-dot
button there.

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
size or hit-test bounds. It flips about the centre, so it is visually
identical to a mirrored asset and click areas remain correct.

Apply at each call site whose drawable is intrinsically directional:

- back chevron (`NavigationButtonBack`, `OnboardingCards`'s `BackButton`,
  `ImageFullScreenView.desktop`, `WhatsNewView`'s "previous" pager,
  `CommandsMenuView`)
- forward chevron / arrow (`NewChatView`'s share-profile chevron,
  `WhatsNewView`'s "next" pager, `CommandsMenuView`'s submenu chevron,
  `GoToItemInnerButton`'s forward variant)
- subscription-status waves (4 drawables at 5 call sites in `SubscriptionStatusIcon`)
- sent-via-proxy arrow (`CIMetaView`, `ChatItemInfoView`)

Two structural details for site-level callers:

- **`SubscriptionStatusIcon`** renders the same `modifier` parameter at
  five Icon callsites within a single composable. Hoist the mirror once
  (`val iconModifier = modifier.mirrorIfRtl()`) and reuse — calling
  `mirrorIfRtl()` once or five times yields the same `Modifier` value
  inside one composition.
- **`GoToItemInnerButton`** is a local helper inside `ChatItemView` that renders both a
  directional icon (`ic_arrow_forward`, "go to forwarded source") and a
  non-directional icon (`ic_search`, search-result indicator) through
  the same `Icon(painterResource(icon), ...)` call. Add a
  `mirror: Boolean = false` parameter and pass `mirror = true` only at
  the directional callsite. The `ic_search` callsite keeps its default
  and is unchanged.

### 4. Icons drawn by shared helpers

Two icons are drawn inside a shared helper that takes a
`Painter` and no icon modifier: the "Don't enable" chevron on the
delivery receipts sheet (`SimpleButtonIconEnded`) and the "Show message
status" row in Advanced network settings (`SettingsPreferenceItem`; it
uses the same arrow that is mirrored in the message meta, so without
this the setting points the opposite way from the indicator it
controls). `Painter.mirrorIfRtl()` (next to `Modifier.mirrorIfRtl()`)
returns a painter that draws the original flipped under RTL; the
caller's alpha and tint are applied by `Painter`'s own layer around the
flipped drawing, so neither helper's signature changes.

The member delivery status "forwarded" chevron (message menu → Info →
Delivery) is the only directional glyph `GroupSndStatus.statusIcon()`
returns; the info view mirrors it when the returned icon is that
chevron, so the status is not dispatched on a second time.

### 5. Other absolute placements

- `PriorityLayout` (the rows of every framed message: header, quote,
  media, file, voice, link preview and text) placed its rows at x = 0,
  so a caption narrower than the image sat at the end edge under RTL. `placeRelative(0, y)`.
- `DependentLayout` (group member name and role above a received
  message) placed its rows at x = 0. Under RTL the row started at the
  left edge of the bubble column instead of the avatar side and could
  stick out past the pane edge on short messages. `placeRelative(0, y)`.
- `SupportSimpleXBanner` placed the image at the absolute right of the
  card, where the right-aligned text runs under it (the banner text is
  not translated, so it hides the end of the English text). The image uses
  `placeRelative`; the card fills the layout, so its position is the
  same in both directions.

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

## Not in this PR

- **Reply, forward and "open" arrows** (22 references, mostly drawn
  through shared helpers), and the backspace, login and logout icons.
  Material mirrors them under RTL, but they
  are not called out by the issue and do not point at a side of the
  layout; a maintainer decision, listed in the PR.
- **Selection mode** shifts received messages the wrong way under RTL
  (`ChatView.kt`), and the **voice message progress bar** fills from the
  left while its thumb runs from the right — different bugs, separate PR.
- **Desktop applies the layout direction at start**: choosing an RTL
  language in Appearance changes some strings at once and the layout
  after a restart. Separate change.
- `ic_search` in `GoToItemInnerButton` stays unmirrored, as everywhere
  else in the app.

## Verification

Manual, Android and desktop. Switch the in-app language to Arabic,
Persian or Hebrew (Settings → Appearance → Language). Android applies
the layout direction at once; on desktop restart the app after each
language change (to RTL and back). Then:

- Open any chat — sent-message tails are on the outer (end) edge,
  bottom-left under RTL; received-message tails point to the contact's
  avatar, bottom-right under RTL.
- Chat header — call + 3-dot menu on the visual left; on Android the
  back chevron is on the visual right and points right (`>`). Tapping
  the 3-dot menu opens a dropdown next to the button, on the left.
- Any settings page (e.g. Settings → Appearance) — the back button
  points `>` and is on the right edge of its bar (at the bottom with
  the default one-hand UI).
- Chat list header — profile avatar on the right; "Chats" title stays
  centred, with the signal icon to its left and its "waves" mirrored.
- Settings → Help → What's new — pagination chevrons mirror.
- With Advanced network settings → "Show message status" on and private
  routing used: the sent-via-proxy arrow in the message meta, and in a
  group message's Info → Delivery, points left; the "Show message
  status" row's own arrow is mirrored too.
- Delivery receipts sheet (shown at start only on installations
  upgraded from before delivery receipts) — the "Don't enable" chevron points
  left.
- Group chat: the member name/role row above a short received message
  starts at the avatar side and stays inside the pane (also the channel
  name row); an image, file or voice message with a short text has the
  text at the start side.
- Supporter banner on the chat list (more than 3 chats, no badge, not
  dismissed): its image is at the end side and does not cover the
  text.
- Switch back to English — every above location matches pre-PR
  behaviour; no LTR regression.

## Alternatives considered and rejected

- **`android:autoMirrored="true"` on the vector drawables.** Would set
  the mirror policy once per asset instead of once per call site. The
  project's vector drawables are generated by moko-resources from SVGs,
  and the SVG → vector XML pipeline doesn't currently propagate that
  flag, and desktop draws the SVG files directly, with no auto-mirror
  at all. Per-call-site `mirrorIfRtl()` works on both platforms.

- **A `MirroredIcon` wrapper composable.** Each call site has different
  surrounding modifiers (`size`, `height`, `padding`), tints, and
  content descriptions. A wrapper would have to forward many parameters
  and produce a worse interface than direct `.mirrorIfRtl()` in the
  modifier chain.

- **Mirror `ic_search` inside `GoToItemInnerButton`.** Material Design
  lists the search icon as a handheld object that is not mirrored under
  RTL, and the app does not mirror `ic_search` anywhere else.
