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

We are using `kdroidFilter/ComposeNativeTray` rather than `java.awt.SystemTray` / Compose's built-in `Tray` because the latter is a no-op on stock GNOME (the JDK deliberately disables it; see JDK-8322750) and is unmaintained. ComposeNativeTray uses platform-native APIs — `Shell_NotifyIcon` on Windows, `NSStatusItem` on macOS, and StatusNotifierItem over D-Bus on Linux — and is actively maintained.

Users upgrading from a prior version will see the dialog on their first window-close after the update — that is intentional. The dialog is the chosen mechanism for getting consent before keeping a process running in the background, and an existing user has no way to give that consent in advance.

## How

### Close behavior — preference and flow

Add an enum preference:

```kotlin
enum class CloseBehavior { Ask, Quit, MinimizeToTray }

// in AppPreferences:
val closeBehavior = mkEnumPreference(
  SHARED_PREFS_DESKTOP_CLOSE_BEHAVIOR,
  CloseBehavior.Ask
) { CloseBehavior.entries }
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

Add `io.github.kdroidfilter:composenativetray-jvm:1.3.0` to `common/build.gradle.kts` (desktopMain). The classpath needs exactly one of the two JNA artifacts (classic `net.java.dev.jna:jna` or modular `net.java.dev.jna:jna-jpms`) — both ship the same `com.sun.jna.*` classes, and having both produces duplicate-class errors.

The existing build already depends on classic `jna` (5.14.0) via `net.java.dev.jna:jna-platform:5.14.0` (used by `jSystemThemeDetector`). ComposeNativeTray brings `jna-jpms`. **Align by excluding `jna-jpms` from ComposeNativeTray's transitive deps** rather than excluding classic `jna` — the latter would break `jna-platform` and `jSystemThemeDetector`. Scope the exclusion to the desktop dependency block (not `configurations.all`):

```kotlin
implementation("io.github.kdroidfilter:composenativetray-jvm:1.3.0") {
  exclude(group = "net.java.dev.jna", module = "jna-jpms")
}
```

Verify after change with `./gradlew :common:dependencies` — there should be exactly one `com.sun.jna` provider on the desktop classpath.

The tray composable lives next to `AppWindow` inside `application(exitProcessOnExit = false) { … }` in `showApp()`. It is gated by the preference, so installing or removing the OS-level tray icon happens automatically when the user toggles the setting:

```kotlin
if (appPrefs.closeBehavior.state.value == CloseBehavior.MinimizeToTray) {
  val unread by remember { derivedStateOf {
    ChatModel.chats.value.sumOf { it.chatStats.unreadCount }
  } }
  Tray(
    iconContent = { TrayIcon(hasUnread = unread > 0) },
    tooltip = trayTooltip(unread),
    primaryAction = ::showWindow,
    menu = {
      Item(stringResource(MR.strings.tray_show), onClick = ::showWindow)
      Divider()
      Item(stringResource(MR.strings.tray_quit), onClick = ::quitApp)
    }
  )
}
```

`showWindow()` sets `windowVisible.value = true` and calls `window?.toFront()` + `window?.requestFocus()`. `quitApp()` sets `closedByError.value = false` and `exitApplication()`.

**Unread indicator.** `TrayIcon` is a small Compose `Painter` that draws the base SimpleX icon, plus, when `hasUnread` is true, a red dot overlay in the bottom-right corner. ComposeNativeTray re-rasterises on each recomposition, so swapping the icon when unread state changes is automatic.

Two new image resources:
- `MR.images.ic_simplex_tray` — base tray icon.
- `MR.images.ic_simplex_tray_dot` — same icon with the red-dot overlay.

**Icon size.** ComposeNativeTray rasterises a `Painter` into a single bitmap that the OS then scales to whatever size the panel/menu bar requests. Different OSes ask for different sizes (Windows 16, KDE/GNOME 22, macOS 22, retina 2×). To keep the source large enough that downscale stays sharp without fuzzing the dot at small sizes, render the source at **64×64**. We accept the slight scaling cost on 16×16 Windows panels rather than maintaining four pre-rendered size variants. If users report the icon looking poor on small panels, we can later switch to ComposeNativeTray's multi-size API and ship 16/22/32/48 PNGs.

**Tooltip.** Plain "SimpleX" when unread is zero; "SimpleX — N unread" otherwise.

**Window restore is best-effort.** Compose Multiplatform issue [#4231](https://github.com/JetBrains/compose-multiplatform/issues/4231) documents that `toFront()` does not always pull the restored window above other windows on Linux/Windows — the OS may flash the taskbar entry instead. Acceptable for v1; if it bites users we can add the `isAlwaysOnTop = true; toFront(); isAlwaysOnTop = false` workaround in a follow-up.

**No collision with the existing notification path.** `NtfManager.desktop.kt:178-188` contains an `java.awt.SystemTray` hack inside a private helper that turns out to be unreachable — the live notification path is `displayNotificationViaLib` (TwoSlices). The hack will not fire and cannot conflict with our ComposeNativeTray icon. Cleaning up that dead code is out of scope here.

**Toggling at runtime.** The `Tray { … }` composable is gated on `closeBehavior.state.value == MinimizeToTray`; Compose's recomposition lifecycle handles install/uninstall when the user flips the setting. No `LaunchedEffect` is needed.

### Appearance settings row

In `Appearance.desktop.kt`, add one row to the existing settings section:

> ☑ **Minimize to tray when closing window**
> *Keep SimpleX running in the background to receive messages.*

The toggle maps to the preference:

- `MinimizeToTray` → on.
- `Quit` or `Ask` → off.

Flipping on writes `MinimizeToTray`. Flipping off writes `Quit`. Touching the toggle resolves the `Ask` state to a definitive value — so a fresh-install user who opens Appearance settings, flips the row off, and then closes the window will *not* see the dialog (their preference is now `Quit`). This matches the user's apparent intent (they made a choice in settings) and avoids the surprise of a dialog appearing for a setting they thought they had already configured.

The wording "Minimize to tray" is used uniformly across all platforms, including macOS where the more native term would be "menu bar". A consistent in-app term is more important here than per-platform purity.

### Files changed

| File | Change |
|---|---|
| `apps/multiplatform/common/build.gradle.kts` | Add ComposeNativeTray dep; exclude its transitive `jna-jpms` so the existing classic `jna` from `jna-platform` stays the single JNA provider. |
| `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/SimpleXAPI.kt` | Add `CloseBehavior` enum, `closeBehavior` preference, `SHARED_PREFS_DESKTOP_CLOSE_BEHAVIOR` constant. |
| `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopApp.kt` | Replace inline `onCloseRequest`; add `windowVisible` to `SimplexWindowState`; wire `Window(visible = …)`; host the `Tray` composable conditionally. |
| `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/DesktopTray.kt` *(new)* | `TrayIcon` painter, `showCloseBehaviorDialog`, `showWindow`, `quitApp` helpers. |
| `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/views/usersettings/Appearance.desktop.kt` | Add the toggle row. |
| `apps/multiplatform/common/src/commonMain/resources/MR/base/strings.xml` | Add 8 new strings (dialog title/body/buttons, settings row, tray menu). |
| `apps/multiplatform/common/src/commonMain/resources/MR/images/` | Add `ic_simplex_tray` + `ic_simplex_tray_dot`. |

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
- **Linux tray-availability probing.** ComposeNativeTray is trusted to handle this; if a particular GNOME setup has no AppIndicator host, the icon will be invisible. Documented as a known limitation.
- **macOS template (auto-tinting) icon.** ComposeNativeTray does not expose `NSImage.setTemplate:` today; the tray icon will be a colored bitmap on macOS. Acceptable initial cost.

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
