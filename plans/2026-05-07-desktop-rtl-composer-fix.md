# Fix #4137 — desktop: RTL text rendering under send button

Target file:
`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/PlatformTextField.desktop.kt`

---

## 1. Problem statement

### 1.1 Symptom

On desktop, when the user types right-to-left text (Arabic, Hebrew,
Persian) in the chat composer **while the global system locale is LTR**,
the first characters of the typed text are rendered **under the send
button** at the bottom-right corner and become invisible while typing.

The same defect places the voice-preview / disabled-state
`ComposeOverlay` text on the wrong horizontal side in this configuration.

### 1.2 Configurations affected

Tested 4 combinations of (global locale × typed-text direction):

| Global locale | Typed text | Behavior |
|---------------|------------|----------|
| LTR           | LTR        | OK       |
| LTR           | RTL        | **broken** — text under send button |
| RTL           | LTR        | OK       |
| RTL           | RTL        | OK       |

Only the LTR-locale + RTL-text combination is broken. This is the
configuration where the **inner text rendering direction** (forced RTL by
`decorationBox`) **disagrees** with the **outer layout direction** (LTR).

### 1.3 Why it matters

- Persian/Arabic/Hebrew users on a non-localized OS (very common: most
  desktop installs default to English) cannot see the start of their own
  message until it grows past the send button.
- The composer is the most-used input in the app; this is a daily
  papercut for the affected user population.

---

## 2. Root cause

A direction-resolution decoupling introduced by an unrelated refactor.
Two commits matter:

### 2.1 The original RTL fix — #4675 (`2ae5a8bff`, Aug 2024)

Added padding logic *inside* a forced-RTL scope:

```kotlin
CompositionLocalProvider(LocalLayoutDirection provides Rtl) {
  Column(Modifier.weight(1f).padding(start = startPadding, end = endPadding)) {
    TextFieldDecorationBox(...)
  }
}
```

Inside that scope `start` resolves to the **right** edge. So setting
`startPadding = 50.dp` for the RTL-text + LTR-locale case correctly
reserved 50dp on the visual right — same side as the send button.

**The padding side and button side were aligned by accident.** `start`
tracked the forced-RTL direction in the same way that `Alignment.BottomEnd`
in `SendMsgView.kt:120` tracked the global direction — and the two
happened to coincide *as long as those directions were the same.* The
pre-existing rule expressed in code was effectively "padding follows
typed-text direction," which was equivalent to "padding follows button
side" only when the inner forced direction and the outer global direction
agreed.

### 2.2 The breaking refactor — #5051 edge-to-edge (`4162bccc4`, Nov 2024)

The padding modifier was lifted **out** of the forced-RTL scope onto the
outer `BasicTextField` (the wrapping `Column` and `Row` were removed).
The outer modifier now resolves `start`/`end` against the **global**
layout direction, but `decorationBox` still forces
`LayoutDirection.Rtl` for RTL characters internally.

In LTR-global + RTL-text:

- `padding(start = 50.dp)` → 50dp reserved on visual **left**
- Text right-aligned by forced-RTL `decorationBox` → renders against
  visual **right**
- 0dp on the right → text under the send button (which is at
  `Alignment.BottomEnd` in LTR global = visual right)

The compensation logic written for the inner-scope semantics silently
became wrong when the modifier moved outward. Code compiled, tests passed,
behavior diverged.

### 2.3 The actual invariant the layout obeys

Reading the layout call graph (`SendMsgView` → `PlatformTextField`):

- `SendMsgView.kt:120` — `Box(Modifier.align(Alignment.BottomEnd)...)`
  places the send button using the **global** layout direction.
- `PlatformTextField.desktop.kt` — `BasicTextField` modifier chain is
  applied in the **global** layout direction.

The constraint is therefore exactly one rule:

> **The textfield must reserve space on the global layout direction's
> `end` — the same side `Alignment.BottomEnd` resolves to in the parent
> `Box`.**

Pre-PR code expressed a different (wrong) rule — "padding follows
typed-text direction" — which agreed with the actual invariant only when
no RTL-text/LTR-locale mismatch existed. The 4 of 4 case failure → 1 of 4
case failure shape is the signature of this kind of accidental alignment.

### 2.4 Why this is structural, not a typo

The defect is not a missing case — it is the **wrong rule**. Adding a
new branch (e.g. "if RTL-text + LTR-locale, swap padding sides
*again*") would silence the symptom while leaving the wrong rule in
place. The fix is to delete the wrong rule and write the actual
invariant.

---

## 3. Solution summary

Make the two conditional assignments that compute `startPadding` and
`endPadding` unconditional, taking the values they already produced in
the `else` branch:

```kotlin
val startPadding = 0.dp
val endPadding = startEndPadding
```

The surrounding code is unchanged — `startEndPadding`'s computation,
the `PaddingValues(startPadding, 12.dp, endPadding, 0.dp)` construction,
the `.padding(start = startPadding, end = endPadding)` modifier call,
and the original two-line comment all stay verbatim.

Master's `if (isRtlByCharacters && isLtrGlobally)` predicate split each
of `startPadding` and `endPadding` into two branches. In cases 1, 3, 4
the predicate is `false` and master takes the `else` branch — exactly
the values the surgical version produces unconditionally. Only case 2
(the bug) takes the `then` branch, and that branch reserves space on
the wrong horizontal side. Removing the predicate removes only case 2's
wrong values; cases 1/3/4 are byte-identical to master.

The 95dp/50dp distinction is preserved verbatim through `startEndPadding`,
which is unchanged.

`ComposeOverlay` (called twice at the bottom of `PlatformTextField`)
reuses the same `padding` value — its placement is corrected for the
same reason without an extra change.

**Net effect**: 2 lines changed.

---

## 4. Detailed technical design

### 4.1 Behavior matrix (post-fix)

| Case | Locale | Text | Master `(start, end)` | Surgical `(start, end)` | Button side |
|------|--------|------|-----------------------|-------------------------|-------------|
| 1    | LTR    | LTR  | `(0, 50)`             | `(0, 50)`               | right ✓ same |
| 2    | LTR    | RTL  | `(50, 0)`             | `(0, 50)`               | right ✓ **fix** |
| 2′   | LTR    | RTL + empty + voice | `(95, 0)` | `(0, 95)`     | right ✓ **fix** |
| 3    | RTL    | LTR  | `(0, 50)`             | `(0, 50)`               | left ✓ same  |
| 4    | RTL    | RTL  | `(0, 50)`             | `(0, 50)`               | left ✓ same  |

Three of the four pre-PR cases are byte-identical to the new code.
Only the broken case (LTR locale + RTL text) flips from `(50, 0)` to
`(0, 50)`, which matches the side where the send button resolves.

### 4.2 Why the 95dp condition stays exactly as-is

The 95dp special case fires only in RTL-text + LTR-locale + empty +
voice-button. In every other configuration, the placeholder text
either left-aligns (no collision with the right-side voice button row)
or sits on the visual side opposite to the buttons (RTL global puts
buttons on the left while forced-RTL placeholder displays on visual
right).

Only the RTL-text + LTR-global case puts a right-aligned placeholder
on the same side as the wider voice-button row. The condition is
intrinsic to the architecture (forced-RTL inside `decorationBox` while
the outer layout is global LTR), not a bug — it must be preserved.

### 4.3 What is *not* changed

Out of scope for #4137 — listed for clarity:

- The `CompositionLocalProvider` inside `decorationBox` that forces
  `LayoutDirection.Rtl` for RTL-by-characters input (the BiDi-detection
  workaround from #4675 itself).
- `lastTimeWasRtlByCharacters` state and `isRtl` detection on the first
  50 characters of the message.
- The `ComposeOverlay` composable — it inherits the corrected
  `padding`.
- `SendMsgView`, the `Alignment.BottomEnd` send button placement, and
  the voice-button row layout.
- The Android implementation (`PlatformTextField.android.kt`) — uses
  a native Android `EditText` with `setPaddingRelative`, which
  resolves against the view's own layout direction; behavior is
  unaffected and out of scope.

### 4.4 Properties of the resulting code

- The two adjacent conditional assignments dispatching on
  `isRtlByCharacters && isLtrGlobally` (one for `startPadding`, one for
  `endPadding`) become unconditional. The predicate is removed; the
  `else` branch's values are lifted to the bare assignments.
- All four locals (`startEndPadding`, `startPadding`, `endPadding`,
  `padding`) keep the same names and continue to exist.
- The `PaddingValues(startPadding, 12.dp, endPadding, 0.dp)` call and
  the `.padding(start = startPadding, end = endPadding)` modifier are
  unchanged.
- The original two-line comment is unchanged. "padding from right side
  should be bigger" remains accurate — `endPadding` is still `95.dp`
  vs `50.dp` under the same condition as before, just consistently on
  the global end side.
- No behavior is removed: RTL detection, the `decorationBox` direction
  override, overlay rendering, and the empty-text/voice-button 95dp
  expansion are all retained verbatim.
- Diff size: 2 lines changed, one file. No reformatting of unrelated
  code.

### 4.5 Risk surface

- **Compose 1.7.x BiDi engine** — unchanged; we still rely on
  `decorationBox`'s forced direction for right-alignment of typed RTL
  text. No new BiDi dependency.
- **Padding API** — `Modifier.padding(end = X.dp)` and
  `PaddingValues(start, top, end, bottom)` are stable Compose APIs.
- **Direction resolution** — `Modifier.padding`'s start/end have
  resolved against the enclosing `LocalLayoutDirection` since Compose
  Foundation 1.0; no version-sensitive behavior.
- **Cross-platform** — Android implementation uses a native
  `EditText`; no shared change required.

---

## 5. Detailed implementation plan

### 5.1 The exact edit

File:
`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/platform/PlatformTextField.desktop.kt`

**Lines 89–90 — replace 2 lines:**

```kotlin
// remove
  val startPadding = if (isRtlByCharacters && isLtrGlobally) startEndPadding else 0.dp
  val endPadding = if (isRtlByCharacters && isLtrGlobally) 0.dp else startEndPadding

// add
  val startPadding = 0.dp
  val endPadding = startEndPadding
```

No other lines change. No imports added or removed. The comment, the
`startEndPadding` computation, the `PaddingValues` construction, and
the `.padding(start = startPadding, end = endPadding)` modifier are
all preserved verbatim.

### 5.2 Steps

1. Edit `PlatformTextField.desktop.kt` at the site above (lines 89–90).
2. Build desktop module:
   `cd apps/multiplatform && ./gradlew :common:desktopMainClasses`
3. Run desktop app on an LTR system locale; type
   `متن راست به چپ` in the composer; verify all characters visible.
4. Type ASCII; verify no regression.
5. Switch system locale to Arabic/Persian/Hebrew; repeat both inputs;
   verify send button and reservation flip together to the visual
   left, with no overlap.
6. Trigger voice preview / disabled-state placeholder in each
   configuration; verify the overlay text is on the side opposite
   the send button.
7. Commit on a topic branch (`nd/fix-RTL`); PR title:
   `desktop: fix RTL text rendering under the send button`; reference
   `Fixes #4137`.

### 5.3 Test matrix to verify manually

| # | Locale | Typed text | Empty + voice? | Expectation |
|---|--------|-----------|----------------|-------------|
| 1 | LTR    | ASCII     | n/a            | unchanged from current |
| 2 | LTR    | RTL chars | no             | chars visible, no overlap with right-side button |
| 3 | LTR    | empty     | yes            | placeholder + voice-button row both visible |
| 4 | LTR    | (was RTL) → empty | yes    | placeholder clears 95dp on right (sticky `lastTimeWasRtlByCharacters`) |
| 5 | RTL    | ASCII     | no             | unchanged |
| 6 | RTL    | RTL chars | no             | unchanged |
| 7 | RTL    | empty     | yes            | unchanged |

### 5.4 Rollback

Revert is one commit and one file. Behavior reverts cleanly.

---

## 6. Alternative approaches considered

### 6.1 Chosen approach — drop the buggy `then` branch (§3)

The 2-line surgical change. Removes the predicate from the
`startPadding` and `endPadding` assignments, keeping the (correct)
`else` branch values as the unconditional definition. Smallest
possible diff; preserves all variable names, the comment, the
`PaddingValues` call, and the `.padding(start, end)` modifier.
Fixes the overlay placement as a free byproduct.

### 6.2 Re-couple padding to inner forced direction by wrapping `BasicTextField`

Move the `CompositionLocalProvider(LocalLayoutDirection = Rtl)` *outside*
`BasicTextField` rather than inside `decorationBox`. The outer
`.padding(start, end)` would then resolve in the same direction as the
inner text, restoring the pre-#5051 invariant and letting the
historical `start = 50.dp / end = 0.dp` swap work again.

**Pros**: padding-vs-text consistency at the source.

**Cons**: also flips `fillMaxWidth`, `focusRequester`, `onPreviewKeyEvent`,
and the parent `Box`'s `Alignment.BottomEnd` resolution direction is
**still global** — so the textfield and the button align against
different directions, moving the mismatch instead of removing it.
Bigger refactor, broader test surface, no net gain. **Rejected.**

### 6.3 Remove the forced-RTL override; rely on Compose BiDi

Delete the `CompositionLocalProvider` inside `decorationBox`. Let
Compose's BiDi engine right-align RTL paragraphs without forcing a
paragraph direction. Then `start`/`end` resolve consistently against
the global direction everywhere; `isRtlByCharacters`,
`lastTimeWasRtlByCharacters`, and the 95dp special case can all be
deleted.

**Pros**: largest simplification — eliminates the entire BiDi-detection
state machine and the 95dp branch.

**Cons**: depends on Compose Desktop 1.7.x BiDi engine matching what
#4675 originally needed to enforce. If automatic BiDi is insufficient
(e.g. mixed Latin-RTL paragraphs, neutral characters at paragraph start,
numbers in RTL paragraphs), regressions reappear. Requires manual
verification across all the cases #4675 originally fixed. Out of scope
for #4137. **Reasonable follow-up; not part of this fix.**

### 6.4 Derive padding from measured button-row width

Refactor `SendMsgView` so the textfield's reservation comes from the
**measured** width of the button row (via `SubcomposeLayout` or shared
state), instead of hard-coded 50/95dp. The textfield would reserve
exactly as much as the buttons need, regardless of direction or button
configuration.

**Pros**: removes the 50/95dp magic numbers and the
`showVoiceButton`-dependent branch. Self-correcting if the button row
ever changes.

**Cons**: significantly larger refactor; `SubcomposeLayout` adds cost
to a frequently-recomposing view; doesn't fix the bug at hand any
better than §6.1. **Reasonable longer-term cleanup; not part of this
fix.**

### 6.5 Add a third special case for the failing combination

`if isRtlByCharacters && isLtrGlobally then padding(end=50) else
padding(start=startPadding, end=endPadding)`.

**Pros**: one-line behavior fix.

**Cons**: leaves the wrong rule in place plus a workaround on top.
Three branches where one suffices, and the underlying defect — padding
following typed-text direction instead of button side — is preserved
and now harder to spot. **Rejected as a workaround.**

---

## 7. Recommendation

Implement §3 (the chosen approach). It is the minimal structural
root-cause fix, also corrects the overlay placement as a free byproduct,
and removes the wrong-side `then` branch from both `startPadding` and
`endPadding`.

Defer §6.3 and §6.4 to separate PRs if desired — both are reasonable
cleanups but are not necessary to fix #4137 and would expand the blast
radius beyond the bug.
