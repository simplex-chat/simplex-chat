# Fix E2E encryption section divider rendered inside the section

Branch: `nd/fix-e2e-encryption-section-divider` · base: `master`.

## Problem

On the contact info screen (Android and desktop, current `master`), the "E2E encryption — Quantum resistant / Standard" card has a horizontal divider line cutting across it under its single row, followed by extra padded space — visually reading as the card being sliced in two with a second, empty card underneath. Repros for any 1:1 contact with an active connection. Behaviour of the row itself is correct; bug is purely visual.

## Fix

One line in `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/ChatInfoView.kt` — move `SectionDividerSpaced()` out of the `SectionView { ... }` block:

```diff
     if (conn != null) {
       SectionView {
         InfoRow("E2E encryption", if (conn.connPQEnabled) "Quantum resistant" else "Standard")
-        SectionDividerSpaced()
       }
+      SectionDividerSpaced()
     }
```

Total diff: 1 file, +1 / −1.

## Cause

Two unrelated changes combined to produce the visible bug:

1. PR #4060 (`9e3f528d4`, "android: remove experimental PQ toggle") removed the conditional `AllowContactPQButton` / `SectionTextFooter` that used to sit between the `InfoRow` and the divider — but left `SectionDividerSpaced()` inside the `SectionView { ... }` block. At that point `SectionView` was a plain column, so the leftover divider only looked like extra inter-section spacing.

2. PR #6777 (`df5ea3d46`, "android, desktop: new settings section design") wrapped `SectionView`'s content in `CardColumn` with `SectionCardShape`, giving each section a rounded card background. After this, *anything* drawn by the section content — including the leftover `Divider` — is drawn inside the card.

Rendered structure on current master:

```
SectionView (card background, rounded shape)
  └ CardColumn
      ├ InfoRow("E2E encryption", ...)
      ├ Divider              ← line cutting across the card
      └ 18 dp bottom padding ← reads as a second, empty card
```

After the fix the divider re-parents to the enclosing `ChatInfoLayout` column and sits between the E2E card and the next section's card, matching the pattern used by every other section on the screen.

## Risk

- One composable call site, structural move of a single node; no logic, state, or styling change.
- iOS is a separate codebase and is unaffected.
- Grep confirms no other `SectionDividerSpaced` call sits inside a `SectionView { ... }` in `apps/multiplatform`.
- Rollback: `git revert` the fix commit.
