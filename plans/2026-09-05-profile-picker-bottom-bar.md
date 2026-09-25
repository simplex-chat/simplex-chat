# The profile picker's last row is hidden under the bottom app bar

`ActiveProfilePicker` in `newchat/NewChatView.kt` — the "Select chat profile" screen,
reached from an invitation link and from the share sheet.

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
| obscured by the bar | `AppBarHeight` 56.dp + navigation bar |
| clearance in the list | `DEFAULT_BOTTOM_PADDING` 48.dp |
| `LazyColumnWithScrollBar` `contentPadding` | `0.dp` (default, not overridden) |

The shortfall is `8.dp` plus the navigation bar — commonly 32–56.dp, and the rows are
`DEFAULT_MIN_SECTION_ITEM_HEIGHT + 8 = 58.dp`, so roughly one whole row is unreachable.
It only shows once the list is long enough to scroll; with few profiles nothing is hidden,
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
profiles to scroll, the button the feature exists for cannot be reached at all. The feature
is unusable on Android without this fix, so #7329 depends on it.

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

Open "Select chat profile" from an invitation link on Android with one-hand UI on and
enough profiles that the list scrolls; the last row must be fully visible above the search
bar. Repeat with one-hand UI off — the layout must be unchanged. Repeat from the share
sheet, which uses the same picker with `contactConnection = null`.
