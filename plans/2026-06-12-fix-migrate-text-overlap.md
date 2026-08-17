# Fix overlapping warning texts after finalizing migration

Branch: `nd/fix-migrate-text` · regression from PR [#6777](https://github.com/simplex-chat/simplex-chat/pull/6777) (`df5ea3d46`, new settings section design).

## 1. Problem statement

On the "Migrate device" screen (Android and desktop), after tapping **Finalize migration** the finished state renders broken: the two warning texts — "You **must not** use the same database on two devices." and "**Please note**: using the same database on two devices will break the decryption of messages…" — are painted on top of each other and on top of the "Migration complete" section card, directly under the section header.

Reproduced on desktop with default settings. The screen immediately before (`LinkShownView`, with the QR code) renders correctly.

## 2. Solution summary

Move the two `SectionTextFooter` calls in `FinishedView` out of the `Box` and place them after it, so they render as sequential children of the screen's scroll `Column` — the same placement `LinkShownView` already uses for its footers.

```diff
     }
-    SectionTextFooter(annotatedStringResource(MR.strings.migrate_from_device_you_must_not_start_database_on_two_device))
-    SectionTextFooter(annotatedStringResource(MR.strings.migrate_from_device_using_on_two_device_breaks_encryption))
     if (chatDeletion) {
       ProgressView()
     }
   }
+  SectionTextFooter(annotatedStringResource(MR.strings.migrate_from_device_you_must_not_start_database_on_two_device))
+  SectionTextFooter(annotatedStringResource(MR.strings.migrate_from_device_using_on_two_device_breaks_encryption))
 }
```

Total diff: 1 file, 2 lines moved (+2 / −2 at different indentation).

## 3. Root cause

PR #6777 added card chrome to `SectionView` and, in a sub-commit ("Migrate views: move all SectionTextFooter / SectionSpacer out of SectionView lambdas"), moved footers out of the card lambdas so they read as captions below the cards. In `FinishedView` (`MigrateFromDevice.kt`) the footers were moved out of the `SectionView` — but left **inside the wrapping `Box`**:

```kotlin
Box {
  SectionView(stringResource(MR.strings.migrate_from_device_migration_complete)) {
    // "Start chat" / "Delete database" buttons
  }
  SectionTextFooter(…you_must_not_start_database_on_two_device…)   // Box child 2
  SectionTextFooter(…using_on_two_device_breaks_encryption…)       // Box child 3
  if (chatDeletion) {
    ProgressView()                                                  // Box child 4 (overlay)
  }
}
```

That `Box` exists for exactly one reason: to overlay `ProgressView` (a fullscreen-centered spinner) over the section while the chat database is being deleted. `Box` stacks its children at `TopStart`, so both footers render at the Box's top-left corner — over the card's top edge and over each other. This is the only migration sub-view where the refactor produced this shape: the other `Box`-wrapped states keep all flow content inside one child (a `SectionView` or an inner `Column`), and `LinkShownView` has no `Box` at all.

`FinishedView` is composed inside `ColumnWithScrollBar` (via `SectionByState`), so composables emitted at the function's top level land in the scroll `Column` and stack vertically — which is where the footers belong.

## 4. The fix in detail, and why this shape

Three candidate fixes were compared:

- **Move the 2 footer lines after the `Box`** (chosen). Smallest possible diff, zero re-indentation. Footers become siblings of the Box in the scroll `Column`, identical to the working `LinkShownView` pattern in the same file. `ProgressView` keeps its overlay semantics unchanged (same as `DatabaseInitView`, `ArchivingView`, `LinkCreationView`). Only behavioural delta beyond the bug fix: during the transient `chatDeletion` spinner, the overlay centers over the card rather than card + footers — matching every other migration sub-view.
- **Wrap card + footers in a `Column` inside the Box.** Behaviorally near-identical, but ~40 lines of indentation churn and a layout shape no sibling view uses. Rejected: larger diff, no benefit.
- **Also hoist `ProgressView` out of the Box.** Changes overlay semantics (spinner would flow below content instead of over it). Rejected: touches behavior the bug report doesn't concern.

Regression risk: the change is placement-only — no logic, no state, no measurement changes. The new arrangement is the proven pattern of the adjacent view.

## 5. Scope verification — no other instances of the bug class

The class ("flow content as direct children of an overlay `Box`") was searched for across all Kotlin source sets (`commonMain`, `androidMain`, `desktopMain`, `android`, `desktop`) with three complementary structural scans:

1. Every `Box` block with ≥2 stacking flow children (section views, footers, spacers, settings items): **only** `FinishedView`.
2. All 384 footer/spacer call sites classified by nearest enclosing block: the only ones directly inside a `Box` are the two fixed lines.
3. All ~25 composable functions that emit footers at function top level (placement decided by caller): no caller invokes them inside a `Box`.

iOS is structurally immune: SwiftUI footers are part of `Section { } footer: { }` inside a `List`; `MigrateFromDevice.swift`'s `finishedView` was verified correct.

Related but distinct (not fixed here): 10 `SectionTextFooter` calls app-wide still sit *inside* `SectionView` card lambdas (6 in migration views, plus `LinkAMobileView`, `ConnectMobileView`, 2 in `NetworkAndServers`), rendering inside the white card instead of as captions below it. Cosmetic placement inconsistency with #6777's stated pattern, no overlap — left for a separate change if desired.
