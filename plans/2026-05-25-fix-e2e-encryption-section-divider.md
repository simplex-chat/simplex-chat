# Fix E2E encryption section divider rendered inside the card

Branch: `fix/e2e-encryption-section-divider` · commit `4f2b96149` · PR [#7011](https://github.com/simplex-chat/simplex-chat/pull/7011).

## 1. Problem statement

On the contact info screen (Android and desktop), the "E2E encryption — Quantum resistant / Standard" section renders with a thin horizontal line (a `Divider`) cutting across the inside of the card, immediately below the single info row, followed by ~30 dp of empty padded space. Visually this reads as the card being sliced in two: a populated top half and a second, empty card below it. Reproduced for any contact with an active connection (`contact.activeConn != null`) — i.e. effectively all 1:1 contacts.

The bug is purely visual; the value and behavior of the E2E encryption row are correct.

## 2. Solution summary

One line in `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt`. The `SectionDividerSpaced()` call is moved out of the `SectionView { ... }` block so it spaces this section from the *next* section, instead of being rendered as content inside the same card.

```diff
     val conn = contact.activeConn
     if (conn != null) {
       SectionView {
         InfoRow("E2E encryption", if (conn.connPQEnabled) "Quantum resistant" else "Standard")
-        SectionDividerSpaced()
       }
+      SectionDividerSpaced()
     }
```

Total diff: 1 file, +1 / −1.

## 3. Root cause

Two unrelated, individually-harmless changes combined to produce the bug:

1. **9e3f528d4** (`android: remove experimental PQ toggle`, #4060) removed the conditional `AllowContactPQButton` and `SectionTextFooter` that used to live below the `InfoRow`, but left `SectionDividerSpaced()` in place — still inside the `SectionView` block. At the time, `SectionView` was a plain `Column` with no background, so a trailing divider inside the section was visually indistinguishable from a divider after it.

2. **df5ea3d46** (`android, desktop: new settings section design`, #6777) reshaped `SectionView` to wrap its content in `CardColumn { ... }` with `SectionCardShape` (rounded card background). After this change, *anything* drawn by `content` — including a `Divider` — is drawn inside the card.

`SectionDividerSpaced` (`Section.kt:336-348`) renders, when `LocalCardScreen.current` is `false` (the normal contact-info screen):

```kotlin
Divider(
  Modifier.padding(
    start = DEFAULT_PADDING_HALF,
    top = if (maxTopPadding) DEFAULT_PADDING + 18.dp else DEFAULT_PADDING + 2.dp,
    end = DEFAULT_PADDING_HALF,
    bottom = if (maxBottomPadding) DEFAULT_PADDING + 18.dp else DEFAULT_PADDING + 2.dp)
)
```

So the actual rendered structure was:

```
SectionView (card background, rounded shape)
  └ CardColumn
      ├ InfoRow("E2E encryption", "...")     <-- the visible row
      ├ <DEFAULT_PADDING + 2 dp top padding>
      ├ Divider                              <-- the "black line cutting through"
      └ <DEFAULT_PADDING + 18 dp bottom>     <-- the "second empty card"
```

The "second card" the user sees is just the bottom-padding region of `SectionDividerSpaced` inside the same rounded card shape, with the divider drawn near its top edge.

The correct pattern is used everywhere else in this file (e.g. `ChatInfoView.kt:614-618`):

```kotlin
SectionView {
  ChatTTLOption(...)
}
SectionTextFooter(...)
SectionDividerSpaced()    // <-- outside the SectionView, separates from the next section
```

## 4. The fix in detail

Moving the divider one line down — past the closing `}` of `SectionView` — changes its parent from the card's `CardColumn` to the enclosing layout. The divider and its 2 dp top / 18 dp bottom padding now render between the E2E encryption card and whatever section follows (`address_section_title` if `contact.contactLink != null`, otherwise `conn_stats_section_title_servers`).

- For contacts that have a contact link: the divider sits between the E2E card and the address-section card — same behavior as every other inter-section transition on this screen.
- For contacts without a contact link: the divider sits between the E2E card and the network-status card. Same as above.

No conditional, no new state, no styling change. The fix is structurally identical to the surrounding inter-section uses of `SectionDividerSpaced()`.

## 5. Why this specific shape

- **Why move it rather than delete it.** Every other section in `ChatInfoLayout` is followed by `SectionDividerSpaced()` (or `SectionTextFooter` + `SectionDividerSpaced()`). Deleting it would visually butt the E2E card against the next card with only the next section's leading padding — inconsistent spacing for one specific section. Moving preserves the existing inter-section rhythm.

- **Why no rename or constant extraction.** `SectionDividerSpaced` and its `DEFAULT_PADDING + 2.dp / + 18.dp` values are an existing convention used throughout `ChatInfoView.kt` and other settings screens. Renaming or extracting would bundle a stylistic change with a bug fix.

- **Why not also tighten `SectionView` to disallow dividers in its content.** Could be done (e.g. with a custom `ColumnScope` that omits `Divider`), but it would be a typed-API redesign affecting every call site — outside the scope of fixing one visual bug. The same misuse is not present elsewhere in the codebase (confirmed by `grep -rn "SectionDividerSpaced" apps/multiplatform`, manually scanning each match — every other occurrence sits outside the closing `}` of a `SectionView`).

- **Why no iOS change.** `apps/ios/Shared/Views/Chat/ChatInfoView.swift` uses SwiftUI's `Section`/`Form` API, not the Kotlin `SectionView` composable, and was not affected by #6777. The iOS rendering of the same row is correct.

## 6. Verification

Static checks done:

- Read `Section.kt:94-107` (`SectionView`) and `Section.kt:336-348` (`SectionDividerSpaced`) to confirm the rendered tree described in §3.
- `git log -L 620,626:apps/multiplatform/.../ChatInfoView.kt` to confirm the divider has been inside `SectionView` since #4060 and that the card wrapping was added later in #6777.
- `grep -n "SectionDividerSpaced" apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt` shows the post-fix line is consistent with the established pattern at lines 612, 618, 635 etc.

Manual verification (pending — would be done via the Android debug build):

- Open a 1:1 contact's chat info — E2E encryption card now shows a single row with the section's natural padding, no horizontal line through it and no apparent empty card below.
- The next section (address or servers) is separated from the E2E card by the same vertical gap as every other section transition on the screen.
- For a contact with PQ enabled the row shows "Quantum resistant"; for a non-PQ contact it shows "Standard". Both behave identically with respect to the card shape.

## 7. Risk and rollback

- **Blast radius**: one composable call site on `ChatInfoLayout`. The fix is structural — moves a node from one parent to a sibling parent — with no logic, no state, no styling change. No effect on iOS (separate codebase) and no effect on the card-screen variant of `SectionView` (`LocalCardScreen.current == true`), since the bug only manifested in the normal section variant.
- **Regression risk**: zero on non-E2E sections (untouched). For the E2E section, the only behavioral difference is that the inter-section divider is now drawn outside the card rather than inside it — which is the entire purpose of the fix and matches every other section on the screen.
- **Rollback**: `git revert 4f2b96149`.
