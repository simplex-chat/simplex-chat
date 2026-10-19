# Fix stray card dividers in server info, connect-to-desktop, appearance and migration screens

Branch: `nd/fix-ui-lines` · base: `master`.

## Problem

On several card screens (Android and desktop, current `master`), near-black horizontal lines appear inside section cards where they don't belong — most visible in dark/black themes:

- **Servers info → tap an SMP or XFTP server**: the server-detail card is sliced by stray lines around its nested stats / subscriptions / sessions blocks.
- **Use from desktop → "Connected to desktop"** (also the connecting / found / verify states): two black lines between the desktop name and its version (e.g. "Desktop" and "v7.0.0.1").
- **Settings → Appearance** with a custom image wallpaper: a double line with a gap between "Remove image" and "Color mode".
- **Migrate from this device**, "error stopping chat" state: a double line between the error text and the stop-chat action.

Behaviour is correct everywhere; the bugs are purely visual.

## Cause

PR #6777 (`df5ea3d46`, "android, desktop: new settings section design") wrapped `SectionView`'s content in `CardColumnLayout`, which draws a 2 dp divider (`canvasColorForCurrentTheme()`, near-black in dark/black themes) at the bottom of every direct child except the last. The same PR turned these screens into card screens (`ModalView(cardScreen = true)`) but left three content patterns that pre-date the card chrome, each of which now produces stray lines:

1. **Nested `SectionView`s** inside one outer `SectionView` (server detail) → card-in-card, plus a line around every nested block and spacer.
2. **Loose `Text` + `Spacer` + `Text`** as separate children of a card `SectionView` (connect-to-desktop name + version) → a line after the text and after the spacer.
3. **`SectionDividerSpaced()` as a middle child** (Appearance, migration) → on a card screen this is a 30 dp `Spacer`, so it gets a line above *and* below = a double line bracketing a gap.

This is the same bug class as `2026-05-25-fix-e2e-encryption-section-divider.md` (#7012); #6777 reintroduced further instances that earlier fix did not cover.

## Fix

`apps/multiplatform/common/...`, layout-only, matching the reference sibling pattern already used by `DetailedSMPStatsLayout`:

- **`ServersSummaryView.kt`** — `SMPServerSummaryLayout` / `XFTPServerSummaryLayout`: stop wrapping everything in one outer `SectionView`; make the address its own card and each sub-section a top-level sibling separated by `SectionDividerSpaced()` placed *between* them. `SMPSubscriptionsSection`'s rows are wrapped in their own `SectionView` so they keep card chrome.
- **`ConnectDesktopView.kt`** — wrap each device-name + version block in a single `Column` (4 states), so the card sees one child and draws no internal divider.
- **`Appearance.kt`** / **`MigrateFromDevice.kt`** — delete the in-card `SectionDividerSpaced()`; the auto-divider between the two now-adjacent rows already separates them (the #6777 precedent for in-card spacers).

## Risk

- Layout-only: no logic, state, side-effect, or click-handler change.
- iOS is a separate codebase (native grouped lists) and is unaffected.
- Verified: `:common:compileKotlinDesktop` and the full desktop AppImage build succeed, and the running app renders the screens correctly. Independent adversarial review confirmed every conditional branch and separator is preserved across all stats/subs/sessions combinations.
- Deliberately out of scope: `SectionTextFooter`-inside-card instances (NetworkAndServers, Migrate*) — these render as a single normal row-separator above an in-card caption, not the stray-line bug; a separate #6777-style cleanup.
- Rollback: `git revert` the fix commit.
