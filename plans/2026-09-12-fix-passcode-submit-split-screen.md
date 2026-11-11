# Fix Submit button not visible in passcode view in split screen

## Problem

On Android in **split screen**, the "Enter Passcode" view shows the title and the
keypad but **no Cancel/Submit buttons** — the app cannot be unlocked with a passcode
without leaving split screen.

The same `PasscodeView` also backs `SetAppPasscodeView`, so "New Passcode" /
"Confirm passcode" (Privacy & security → passcode, and the passcode onboarding in
`AppLock.setPasscode`) are affected identically.

Desktop is affected too whenever the window is short (below ~590dp of height at a
typical width), since desktop always uses the vertical layout.

## Cause

Two independent defects, both invisible at full screen.

### 1. The keypad consumes the space the buttons need

`PasscodeView.VerticalLayout` is a `Column` of three children — title, keypad,
buttons `Row` — and the keypad's size is derived from the constraints it is offered
(`PasswordEntry.kt`):

```kotlin
val s = if (appPlatform.isAndroid) minOf(maxWidth, maxHeight) / 4 - 1.dp else ...
```

`Column` measures unweighted children in order, each with `maxHeight` reduced by what
the previous ones took, so the keypad — the **second** child — is offered *all*
remaining height and a 4-row grid then occupies exactly that. The buttons `Row` is
measured last, from what is left, and `Modifier.heightIn(min = 70.dp)` cannot rescue
it: `heightIn` constrains its target into the incoming constraints, so a 70dp minimum
against a 1dp maximum yields 1dp.

At full screen the bug is dormant because `maxWidth` is the smaller term there — the
keypad is sized by width and the leftover height goes to the buttons. It appears as
soon as the remaining height drops below the width, which every split-screen half does.

Measured with a Compose harness that renders these composables at fixed window sizes
(411dp wide, density 1, `Surface(Modifier.fillMaxSize())` as in `LocalAuthentication.kt`):

| window | keypad `maxHeight` | key size | buttons Row height | Submit |
|---|---|---|---|---|
| 411×914 (full screen) | 805 | 101.75 | 70 @ y=763 | 108×40 ✓ |
| 411×520 | 411 | 101.75 | **0** | 108×0 |
| 411×480 | 371 | 91.75 | **0** | 108×0 |
| 411×445 (split half) | 336 | 83.0 | **1** @ y=444 | 108×**1** |
| 411×400 | 291 | 71.75 | **0** | 108×0 |

`HorizontalLayout` has the same defect in the width axis: its keypad is 4 keys wide
and sized `minOf(maxWidth, maxHeight) / 3.5f`, so the keypad's *width* grows with the
available *height*, and the Cancel/Submit `Column` — the **second** child of the `Row`
— gets the remainder. At 411×365 it is offered `maxWidth = 24dp` and Submit measures
**0×0**; at 411×445 it is offered **0dp** and Submit is placed at **x=441**, outside a
411dp-wide window.

This branch matters in split screen because `windowOrientation()` reads
`Configuration.orientation`, which is computed from the *activity window* bounds, not
the device: a split half of a tall phone is `PORTRAIT` (~411×445) while a half of a
16:9 phone is `LANDSCAPE` (~411×365). Both layouts are reachable, and both were broken.

### 2. No window insets

`MainActivity` calls `enableEdgeToEdge()`, and this screen never applied insets — at
full screen `SpaceEvenly` happened to leave the buttons ~80dp clear of the navigation
bar. Once the buttons are laid out correctly in a split half, they land at the window
edge: with fix 1 alone, Submit occupies y=390…430 of a 445dp window, and a 48dp
3-button navigation bar covers y=397…445 — **33 of its 40dp**, so the button is drawn
but not reliably tappable in the bottom split window.

## Fix

`Modifier.weight(1f, fill = false)` on the keypad in the vertical layout and on the
keypad column in the horizontal layout (`PasscodeView.kt`), which requires a `modifier`
parameter on `PasscodeEntry` (`PasswordEntry.kt`):

```kotlin
PasscodeEntry(passcode, true, Modifier.weight(1f, fill = false))
```

`Column`/`Row` measure **weighted children after all unweighted ones**, so the title
and the buttons are measured first at their natural size and the keypad receives what
is left — the inverse of the current order, with no size constant to keep in sync.
`fill = false` keeps the keypad at its own size rather than stretching it, and because
a weighted child makes the container expand to the incoming bounded maximum, the
`SpaceEvenly` / `SpaceBetween` distribution at full screen is unchanged.

Plus `Modifier.systemBarsPadding()` on both layout roots.

Measured after the fix:

| window | branch | key size | Submit |
|---|---|---|---|
| 411×445, 24dp status bar (top split half) | vertical | 59.5 | 108×40 @ y=390…430 ✓ |
| 411×445, 48dp nav bar (bottom split half) | vertical | 53.5 | 108×40 @ y=342…382, clear of the bar ✓ |
| 411×365, 48dp nav bar | horizontal | 29.3 | 108×40 @ x=263…371 ✓ |
| 411×914 (full screen) | vertical | **101.75 (unchanged)** | 108×40, column y=24…866 |

The insets go on the `Column`/`Row` themselves rather than a wrapping `Box`: Material
`Surface` lays its content out with `propagateMinConstraints = true`, so the layout root
receives the window size as a *minimum*. A plain `Box` in between drops that minimum
(its own default is `false`), the `Column` becomes wrap-content, `SpaceEvenly` has no
slack left to distribute, and the whole screen top-aligns — measured as the buttons
moving from y=763 to y=544 at full screen. Keeping the padding on the root preserves
the propagation.

## Scope / non-goals

- Both files are `commonMain`, so the fix covers Android and desktop; on desktop
  `systemBarsPadding()` resolves to zero insets and only the `weight` change has an
  effect (short desktop windows).
- The keypad necessarily gets smaller in a small window: 53.5–59.5dp keys in a portrait
  split half, 29.3dp in the 411×365 horizontal case, where the title, reason and
  passcode text consume ~170dp before the keypad is measured. Making the passcode
  screen itself more compact below some height (smaller title, dropping the reason
  line) is a separate design change and is not attempted here.
- `systemBarsPadding()` also shrinks the keypad in full-screen landscape (79.6 → 59.0dp).
  That is the same 72dp the bars always occupied — previously the keypad's bottom row
  extended ~8dp under the navigation bar — but it is a visible change on a screen that
  did not show the reported bug.
- Not changed: `windowOrientation()` branching on `Configuration.orientation`. Selecting
  the layout from the measured aspect ratio (as iOS does) would be a better fit for
  resizable windows, but it changes behaviour on every device rather than fixing this bug.

## iOS

Not affected, and for a structural reason worth recording: iOS picks its layout from the
*measured* geometry rather than an orientation flag —
`if g.size.width < g.size.height * 2 / 3` (`PasscodeView.swift`) — so a short, wide
window (the geometry that breaks Android's vertical layout) selects iOS's horizontal
layout, whose keypad is sized from height (`s = height / 5`) with the buttons column
explicitly bounded to `height / 5 * 3 * 0.97`. The vertical layout's width-driven keypad
(`s = width / 3`) is only ever used when the window is at least 1.5× taller than wide,
where it fits by construction. No iOS change required.

## Verification

- Compose measurement harness at the window sizes tabulated above, before and after,
  for both layout branches and for 24dp/48dp/72dp inset combinations.
- Android arm64 debug APK (`bash ~/build/android.sh`) — manual check of Submit in both
  the top and the bottom split-screen half, and no visual change at full screen.
- Linux x86_64 AppImage (`bash ~/build/linux.sh`) — manual check with the window resized
  short, confirming the desktop vertical layout keeps Cancel/Submit visible.
- Also exercise "New Passcode" / "Confirm passcode" (`SetAppPasscodeView`) in split
  screen, which share `PasscodeView`.
