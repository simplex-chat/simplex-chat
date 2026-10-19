# The profile picker's last row is hidden under the bottom app bar

`ActiveProfilePicker` in `newchat/NewChatView.kt` — the "Select chat profile" screen,
reached from New chat → 1-time link → "Share profile" (Android and desktop) and from the share sheet (Android).

## The defect

`ModalView` renders its app bar as an **overlay**, not as a layout sibling:

```kotlin
Box(modifier) { content() }                                    // list, full height
Box(Modifier.align(if (oneHandUI.value) BottomStart else TopStart)) { DefaultAppBar(...) }
```

With one-hand UI the bar sits at the **bottom**, so it obscures the last
`AppBarHeight + navigation bar insets` of the list while reserving no space for itself.
Against that, the list's only bottom clearance is its trailing spacer:

| | |
|---|---|
| obscured by the bar | `AppBarHeight` 56.dp × √(app font scale) + navigation bar |
| clearance in the list | `DEFAULT_BOTTOM_PADDING` 48.dp |
| `LazyColumnWithScrollBar` `contentPadding` | `0.dp` (default, not overridden) |

The shortfall is `8.dp` (more at larger app font scales) plus the navigation bar — commonly 32–56.dp, and the rows are
`DEFAULT_MIN_SECTION_ITEM_HEIGHT + 8 = 58.dp`, so on an Android phone drawing edge-to-edge with 3-button navigation nearly a whole row stays hidden; on desktop (no navigation bar) about 8.dp of it.
The navigation-bar part is inferred from the code: on Waydroid (3-button navigation) the app sees no navigation bar inset, and only about 8.dp was hidden there too.
It only shows once the list nearly fills the screen; with few profiles nothing is hidden,
which is why it survived.

`oneHandUI` defaults to **true**, so this is the default layout on Android rather than an
opt-in configuration.

## Why it went unnoticed

The casualty on master is the **last profile row** — the least-used profile at the end of a
list. A row you were not looking for is not obviously missing, and the picker is otherwise
fully functional, so nothing draws attention to it.

## Why it matters now

#7329 adds an **"Add profile"** row at the end of this list. That moves a deliberately
sought affordance into the broken position: on a default Android install with enough
profiles to scroll, the button the feature exists for is partly hidden, and with 3-button navigation on a phone mostly or fully hidden (inferred, as above). On such phones the
feature is hard or impossible to use without this fix, so #7329 should land after it.

Splitting it out anyway, because the defect is entirely pre-existing and worth fixing on
its own terms: it is not caused by, and does not require, the new row.

## The fix

Add the bar's height to the trailing spacer when one-hand UI is on, matching the clearance
idiom already used elsewhere (`AppBarHeight * fontSizeSqrtMultiplier`, e.g.
`UserProfileView`, `TerminalView`):

```kotlin
val bottomBarClearance = if (oneHandUI.value)
    WindowInsets.navigationBars.asPaddingValues().calculateBottomPadding() + AppBarHeight * fontSizeSqrtMultiplier
  else 0.dp
```

`oneHandUI` is read inside the spacer's own `item` rather than hoisted, so no existing code
moves. No change when one-hand UI is off.

## Testing

Open "Select chat profile" from New chat → 1-time link → "Share profile" on desktop and Android with one-hand UI on and
enough profiles that the list scrolls; the last row must be fully visible above the search
bar. Repeat with one-hand UI off — the layout must be unchanged. Repeat from the share
sheet, which uses the same picker with `contactConnection = null`.
