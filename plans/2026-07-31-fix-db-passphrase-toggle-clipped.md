# Fix "Save passphrase in settings" toggle unreachable on desktop

## Symptom

On 7.0 desktop (reproduced on the Linux AppImage and on Windows), in
Chat data → Database passphrase & export → Database passphrase, the
"Save passphrase in settings" toggle cannot be switched off. The reporter sees
the switch sitting slightly past the right edge of the section card. Because the
setting stays on, the passphrase remains stored in `settings.properties` and the
app never prompts for it on start — on desktop that file holds the passphrase in
clear text, since `Cryptor.desktop.kt` is an identity implementation.

This is distinct from the case where the switch is *rendered disabled*
(`DatabaseEncryptionView.kt:127`, `enabled = (!initialRandomDBPassphrase && !progressIndicator) || migration`),
which is intended behaviour for a database still using the initial random
passphrase. The reports here are from users who set their own passphrase, so
`initialRandomDBPassphrase == false` and the switch is enabled — just not
reachable.

## Root cause

1. `SavePassphraseSetting` is hand-rolled in both platform actuals
   (`DatabaseEncryptionView.desktop.kt:43-53`, `.android.kt:43-53`) and is the
   only toggle row in the app whose label carries no `weight`:

   ```kotlin
   Text(stringResource(MR.strings.save_passphrase_in_settings), Modifier.padding(end = 24.dp))
   Spacer(Modifier.fillMaxWidth().weight(1f))
   DefaultSwitch(checked = useKeychain, onCheckedChange = onCheckedChange, enabled = enabled)
   ```

   `Row` measures unweighted children first against the full available width, then
   divides what is left among weighted ones. A label that does not comfortably fit
   consumes the remainder, the weighted `Spacer` collapses to zero, and
   `DefaultSwitch` is placed past the row's right edge.

2. Every other toggle row goes through `SettingsActionItemWithContent`
   (`SettingsView.kt:380`), which gives the label `Modifier.weight(1f)`. There the
   label truncates and the trailing control keeps its size and position, so the
   same string length is harmless.

3. Before #6777 this row had enough slack and no clipping. `SectionView` was a
   plain `Column` with no horizontal inset, and `SectionItemView` used
   `DEFAULT_PADDING` (20.dp) per side — 348.dp of content width on the desktop
   start pane (`DEFAULT_START_MODAL_WIDTH` = 388.dp). Any overflow still drew and
   still received pointer events.

4. #6777 introduced `LocalCardScreen` / `CardColumnLayout` (`Section.kt`), which
   wraps section content in `Modifier.padding(horizontal = CARD_PADDING /* 18.dp */)`
   … `.clip(SectionCardShape)`, and switches `itemHPadding` from `DEFAULT_PADDING`
   to `CARD_PADDING`. `DatabaseEncryptionView` is opened with `cardScreen = true`
   (`DatabaseView.kt:235`), so its row content width drops 348.dp → **316.dp**.

5. `Modifier.clip` clips pointer input as well as drawing. The displaced switch is
   therefore both cut off visually and unhittable — the toggle stops working rather
   than merely looking wrong.

Budget arithmetic on the desktop start pane: fixed cost in the row is ~96.dp
(24 icon + 8 spacer + 24 label end-padding + ~40 switch), leaving ~220.dp for a
27-character label at 16.sp, which needs ~215.dp in English. Borderline at 100%
font scale and over budget as soon as the label is longer — a longer localization,
or a larger font size, since the label scales with `fontSizeSqrtMultiplier` while
`CARD_PADDING` does not.

The widths above are derived from the layout constants, not measured against a
running client; the reporter's observation that the switch sits slightly past the
card edge is what confirms the row overflows in practice.

## Fix

Move the weight onto the label and drop the weighted spacer, in both actuals:

```kotlin
Text(stringResource(MR.strings.save_passphrase_in_settings), Modifier.weight(1f).padding(end = 24.dp))
DefaultSwitch(checked = useKeychain, onCheckedChange = onCheckedChange, enabled = enabled)
```

The label now truncates instead of displacing the switch, matching what
`SettingsActionItemWithContent` does for every other toggle row.

The spacer has to go: leaving both the label and the spacer weighted would split
the remaining space between them and starve the label instead, which trades one
layout bug for another.

## Why this fix and not alternatives

- **Widening the card or shrinking `CARD_PADDING`** would buy back the ~32.dp lost
  in #6777, but only until the next longer localization or font-size step. The row
  would stay the one place in the app where a long label can push a control out of
  reach.
- **Removing `clip` from `CardColumnLayout`** would restore clickability of
  overflowing content, but the clip is what gives section cards their rounded
  corners; dropping it would regress the design and leave the switch drawn outside
  its card.
- **Shortening the string** is a translation-wide problem, not a fix, and does not
  help at larger font sizes.

## Impact

- Desktop and Android only. Both actuals carry the identical defect; Android's row
  is in fact narrower still (~288.dp on a 360.dp-wide screen), so it is affected at
  least as much — it simply has not been reported.
- iOS is unaffected: `DatabaseEncryptionView.swift` uses a SwiftUI `Toggle` inside
  `settingsRow`, where the label truncates and the toggle cannot be displaced. The
  `initialRandomDBPassphrase` disabled-state logic is the same on iOS
  (`DatabaseEncryptionView.swift:80`) and is unchanged by this fix.
- Users already stuck in the bad state have `StoreDBPassphrase=true` in
  `settings.properties` with the passphrase stored alongside it. After this fix
  they can turn the setting off in the UI, which removes the stored passphrase via
  `removePassphraseFromKeyChain` and restores the prompt on start.
- No behaviour change beyond the row layout: no logic, preference, or string was
  touched.

## Verification

- `bash ~/build/linux.sh` on this branch: cold `dist-newstyle`, `libsimplex.so`
  rebuilt from master's sources, `:common:compileKotlinDesktop` executed,
  `BUILD SUCCESSFUL`, AppImage produced.
- `bash ~/build/android.sh` on this branch: `BUILD SUCCESSFUL`, arm64-v8a debug APK
  produced (native libs are the prebuilt ones, so this exercises the Kotlin change
  only).
- Not done: the rendered row has not been checked in a running client. Worth
  confirming at a raised font size and in a locale with a longer label, which is
  the case that made the overflow visible in the first place.
