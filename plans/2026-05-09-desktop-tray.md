# Desktop tray icon — minimize to tray on close

## What

Add a system tray icon (Windows notification area, Linux StatusNotifierItem, macOS menu bar) to the SimpleX desktop app, with a "minimize to tray" close behavior gated on first-time user choice.

Three pieces:

1. **First-close dialog** — the first time the user clicks the window's close (X) button, a modal asks whether to close the app or minimize it to the tray. The choice is remembered.
2. **Tray icon** — when the user has chosen "minimize to tray", the app installs a tray icon with a small right-click menu (Show / Quit) and an unread indicator. Clicking the icon restores the window.
3. **Appearance setting** — a "Minimize to tray when closing window" toggle in Appearance settings lets the user change their mind later.

Scope: Linux + Windows + macOS. No autostart. No number-on-icon unread badge. No profile switcher in the tray menu.

## Why

Today, closing the SimpleX desktop window quits the process and the user stops receiving messages until they reopen the app. There is no way to keep the app running quietly in the background, which is the standard expectation for a chat client.

We want this to be opt-in rather than a behavior change for existing users — hence the dialog on first close. Users who prefer the current quit-on-close behavior get exactly that with one click and never see the dialog again. Users who want background message delivery get it with one click and can manage it from settings.

We are using Compose Multiplatform's built-in `androidx.compose.ui.window.Tray` rather than a third-party library. It works cleanly on Windows, macOS, and Linux desktops with a system tray host (KDE Plasma, XFCE, Cinnamon, MATE, GNOME with the AppIndicator extension). The trade-off is that on stock GNOME the JDK deliberately returns `false` from `SystemTray.isSupported()` (per JDK-8322750), so we **probe at startup and disable the feature entirely** when the OS reports no tray support — the dialog hides the "Minimize to tray" option and the Appearance toggle is hidden too. Users with a working tray get the feature; users without never see broken/invisible UI.

All tray-specific code lives in `desktopMain` only. The Android target compiles none of it — there are no expect/actual surfaces calling into tray functionality from `commonMain`.

Users upgrading from a prior version will see the dialog on their first window-close after the update — that is intentional. The dialog is the chosen mechanism for getting consent before keeping a process running in the background, and an existing user has no way to give that consent in advance.

## How

### Close behavior — preference and flow

Add an enum preference:

```kotlin
enum class CloseBehavior { Ask, Quit, MinimizeToTray }

// in AppPreferences:
val closeBehavior: SharedPreference<CloseBehavior> =
  mkSafeEnumPreference(SHARED_PREFS_DESKTOP_CLOSE_BEHAVIOR, CloseBehavior.default)
```

`Ask` is the default for fresh installs and for users upgrading from a version that did not have this preference.

Replace the inline close handler in `DesktopApp.kt` (currently `onCloseRequest = { closedByError.value = false; exitApplication() }`) with a function that branches on the preference:

- **Crash recovery first.** If `closedByError.value == true`, exit immediately with no dialog, no minimize. The crash handler at `DesktopApp.kt:46-47` dispatches `WINDOW_CLOSING` and depends on the application loop ending so it can re-enter. Honouring `closedByError` is what keeps that path working.
- `Quit` → exit immediately, as today.
- `MinimizeToTray` → set `simplexWindowState.windowVisible.value = false` and return.
- `Ask` → show the first-close dialog. The dialog's button writes the preference and then performs the corresponding action.

The same handler is invoked for the X button, Alt+F4 on Windows, and the macOS red traffic-light close — Compose routes all three through `onCloseRequest`. **macOS Cmd+Q is not routed through `onCloseRequest`**: it goes through the application menu's Quit and calls `exitApplication()` directly. We accept that as "always quit" — Cmd+Q is an explicit user intent to quit the application and should not be intercepted by the dialog. Programmatic `WindowEvent.WINDOW_CLOSING` (e.g. from the crash handler) reaches `onCloseRequest` and is handled by the `closedByError` branch above.

The dialog is non-dismissible (no Esc, no outside-tap) so the user must choose. Wording verbatim:

> **Minimize to tray?**
>
> If you choose Close, messages won't be received.
> You can change it later in Appearance settings.
>
> [ Close the app ] [ Minimize to tray ]

The "Close the app" button uses `MaterialTheme.colors.error` (red); "Minimize to tray" uses `MaterialTheme.colors.primary` (blue). The dialog is implemented bespoke (not via the existing `AlertManager`), because `AlertManager` does not support the non-dismissible + custom-button-color combination needed here.

The Compose application loop already runs with `exitProcessOnExit = false`, so hiding the window does not exit the process. No restructuring of `showApp()` is needed.

### Tray icon

No new dependency. We use `androidx.compose.ui.window.Tray` (built into Compose Multiplatform, already on the classpath). It wraps `java.awt.SystemTray` under the hood — works wherever AWT's tray works, returns silently when it doesn't.

**Tray availability probe.** `java.awt.SystemTray.isSupported()` alone is not reliable — there is a JDK pattern where it returns `true` but `SystemTray.add()` then throws `AWTException` (and Compose-MP does not catch it). We expose a `desktopMain` value that runs a real add/remove of a transparent `TrayIcon` inside a `try/catch` and caches the result:

```kotlin
val trayIsAvailable: Boolean by lazy {
  if (!SystemTray.isSupported()) return@lazy false
  try {
    val tray = SystemTray.getSystemTray()
    val probe = TrayIcon(BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB))
    tray.add(probe)
    tray.remove(probe)
    true
  } catch (e: AWTException) { false }
    catch (e: SecurityException) { false }
}
```

The probe is force-evaluated at the top of `showApp()` (off the EDT) so the JDK-8322750 GNOME detection subprocess does not block composition. When `false`: the Appearance toggle is hidden, the first-close dialog is skipped (`Ask` migrates silently to `Quit`), and the close handler treats `MinimizeToTray` as `Quit` (in case the preference was carried over from a tray-capable machine).

The tray composable lives next to `AppWindow` inside `application(exitProcessOnExit = false) { … }` in `showApp()`. It is gated by the preference AND by tray availability:

```kotlin
if (trayIsAvailable && appPrefs.closeBehavior.state.value == CloseBehavior.MinimizeToTray) {
  // UserInfo.unreadCount is the pre-aggregated, ntfs-filtered counter — see SimpleXAPI.kt:2781-2783.
  val unread by remember { derivedStateOf {
    ChatModel.users.sumOf { it.unreadCount }
  } }
  val iconRes = if (unread > 0) MR.images.ic_simplex_tray_dot else MR.images.ic_simplex
  val tooltip = if (unread > 0)
    stringResource(MR.strings.tray_tooltip_unread, unread)
  else
    stringResource(MR.strings.tray_tooltip)
  Tray(
    icon = painterResource(iconRes),
    tooltip = tooltip,
    onAction = ::showWindow,
    menu = {
      Item(stringResource(MR.strings.tray_show), onClick = ::showWindow)
      Separator()
      Item(stringResource(MR.strings.tray_quit), onClick = { exitApplication() })
    }
  )
}
```

Note: Compose's `Tray` takes `icon: Painter` (not `iconContent`), `onAction` (not `primaryAction`), and the menu DSL uses `Separator()` (not `Divider()`). These are the right names for the built-in API.

`showWindow()` sets `windowVisible.value = true` and calls `window?.toFront()` + `window?.requestFocus()`. Quitting from the tray menu just calls `exitApplication()` — `closedByError` is already `false` in the non-crash path, so the outer loop in `showApp()` terminates cleanly.

**Unread indicator.** Icon swap based on `hasUnread`: reuse `ic_simplex` when zero, `ic_simplex_tray_dot` (same icon with a red dot overlay in the bottom-right) otherwise. Compose passes the `Painter` into AWT via `Painter.toAwtImage(density, layoutDirection, size)` — a single bitmap per state. One new image resource is enough:
- `MR.images.ic_simplex_tray_dot` — base icon with the red-dot overlay.

**Icon size.** Compose `Tray` rasterises the `Painter` once at a per-platform target size: Linux 22×22, Windows 16×16, macOS 22×22 (with retina 2×). It's a single bitmap, so we source the painter at a comfortable size (e.g. via a `painterResource(MR.images.ic_simplex)` from the 40×40 SVG already shipped) and let the conversion handle the scale. We accept the slight scaling cost on 16×16 Windows panels rather than ship multiple size variants.

**Tooltip.** Plain "SimpleX" when unread is zero; "SimpleX — N unread" otherwise.

**Window restore is best-effort.** Compose Multiplatform issue [#4231](https://github.com/JetBrains/compose-multiplatform/issues/4231) documents that `toFront()` does not always pull the restored window above other windows on Linux/Windows — the OS may flash the taskbar entry instead. Acceptable for v1; if it bites users we can add the `isAlwaysOnTop = true; toFront(); isAlwaysOnTop = false` workaround in a follow-up.

**No collision with the existing notification path.** `NtfManager.desktop.kt:178-188` contains an `java.awt.SystemTray` hack inside a private helper that turns out to be unreachable — the live notification path is `displayNotificationViaLib` (TwoSlices). The hack will not fire and cannot conflict with our tray icon. Cleaning up that dead code is out of scope here.

**Toggling at runtime.** The `Tray { … }` composable is gated on `closeBehavior.state.value == MinimizeToTray`; Compose's recomposition lifecycle handles install/uninstall when the user flips the setting. No `LaunchedEffect` is needed.

**Android isolation.** All tray code (the `Tray` composable, the close-behavior dialog, `showWindow`, the `trayIsAvailable` probe) lives in `desktopMain` only. The Android target compiles none of it — there are no expect/actual surfaces from `commonMain` calling into tray functionality. The only shared piece is the `CloseBehavior` enum + `closeBehavior` preference in `SimpleXAPI.kt`, which is plain data and never references tray APIs.

### Appearance settings row

In `Appearance.desktop.kt`, add one row to the existing settings section — **only when `trayIsAvailable`**:

> ☑ **Minimize to tray when closing window**
> *Keep SimpleX running in the background to receive messages.*

The toggle maps to the preference:

- `MinimizeToTray` → on.
- `Quit` or `Ask` → off.

Flipping on writes `MinimizeToTray`. Flipping off writes `Quit`. Touching the toggle resolves the `Ask` state to a definitive value — so a fresh-install user who opens Appearance settings, flips the row off, and then closes the window will *not* see the dialog (their preference is now `Quit`). This matches the user's apparent intent (they made a choice in settings) and avoids the surprise of a dialog appearing for a setting they thought they had already configured.

When `trayIsAvailable` is `false` (stock GNOME without AppIndicator extension), the entire row is omitted from Appearance settings, the first-close dialog is skipped (`Ask` migrates silently to `Quit`), and the close handler treats `MinimizeToTray` as `Quit` (in case the user previously enabled it on a different machine).

The wording "Minimize to tray" is used uniformly across all platforms, including macOS where the more native term would be "menu bar". A consistent in-app term is more important here than per-platform purity.

### Files changed

| File | Change |
|---|---|
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt` | Add `CloseBehavior` enum, `closeBehavior` preference, `SHARED_PREFS_DESKTOP_CLOSE_BEHAVIOR` constant. *(already in this branch as commit 1)* |
| `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt` | Replace inline `onCloseRequest`; add `windowVisible` to `SimplexWindowState`; wire `Window(visible = …)`; host the `Tray` composable conditionally on `trayIsAvailable && closeBehavior == MinimizeToTray`. |
| `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopTray.kt` *(new)* | `trayIsAvailable` probe, `requestCloseBehavior` + `CloseBehaviorDialog`, `SimplexTray` composable, `showWindow` helper. |
| `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/views/usersettings/Appearance.desktop.kt` | Add the toggle row (gated on `trayIsAvailable`). |
| `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml` | Add 8 new strings (dialog title/body/buttons, settings row, tray menu). |
| `apps/multiplatform/common/src/commonMain/resources/MR/images/` | Add `ic_simplex_tray` + `ic_simplex_tray_dot`. |

No `build.gradle.kts` change — Compose's `Tray` is already on the classpath via the existing `org.jetbrains.compose` plugin.

### New strings

```xml
<string name="close_behavior_dialog_title">Minimize to tray?</string>
<string name="close_behavior_dialog_text">If you choose Close, messages won\'t be received.\nYou can change it later in Appearance settings.</string>
<string name="close_behavior_dialog_close">Close the app</string>
<string name="close_behavior_dialog_minimize">Minimize to tray</string>
<string name="appearance_minimize_to_tray">Minimize to tray when closing window</string>
<string name="appearance_minimize_to_tray_desc">Keep SimpleX running in the background to receive messages.</string>
<string name="tray_show">Show SimpleX</string>
<string name="tray_quit">Quit SimpleX</string>
```

### Out of scope

The following are deliberately not in this PR:

- **Run on system startup / autostart entries.** Per-platform integration (Windows registry Run key, Linux `~/.config/autostart/*.desktop`, macOS LaunchAgents) is its own design.
- **Number-on-icon unread badges.** Cross-platform text rendering on tray icons is fragile across DPIs and macOS menu bar tinting.
- **Per-profile switcher / mute / mark-all-read** in the tray menu. Keep the menu to Show / Quit for now.
- **macOS template (auto-tinting) icon.** Compose `Tray` doesn't expose `NSImage.setTemplate:`; the tray icon will be a colored bitmap on macOS. Acceptable initial cost.
- **GNOME workaround documentation.** Users on stock GNOME won't see the option at all (probe returns false). We don't bundle or recommend the AppIndicator extension from the app itself; if we want to surface that guidance, it goes in the website/help docs, not in this PR.

### Test plan

Verified manually on at least one Linux (KDE Plasma), Windows 11, and macOS host:

1. Fresh install. Click X on the window. Dialog appears with the exact text and button colors. Dialog cannot be dismissed by Esc or outside-click.
2. Click "Close the app". App exits. Reopen, click X — app exits with no dialog (preference is now `Quit`).
3. Reset preference (or fresh install). Click X, click "Minimize to tray". Window hides. Tray icon appears.
4. Send a message to yourself / receive one. Tray icon switches to the red-dot variant; tooltip updates with unread count.
5. Click tray icon (left-click). Window restores and gains focus. Unread is cleared on viewing the chat.
6. Right-click tray icon. Menu shows "Show SimpleX" and "Quit SimpleX". Both work.
7. Open Appearance settings, flip "Minimize to tray when closing window" off. Tray icon disappears. Click X — app exits with no dialog.
8. Flip the toggle back on. Tray icon appears immediately (the composable is gated on the preference, so installation/removal follows the toggle).
