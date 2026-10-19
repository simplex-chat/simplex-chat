# Desktop leaves `gsettings monitor` running after it exits (Linux)

## Problem

Every run of the desktop app on Linux left a `gsettings monitor org.gnome.desktop.interface gtk-theme` process running after the app exited, reparented to PID 1. Each one holds two D-Bus connections, one to the session bus and one to the system bus. On a test machine 228 of them accumulated and exhausted the system bus's per-user connection limit (256), after which other programs failed to connect to D-Bus ("No buffer space available").

Reproduced with the master AppImage: each launch and clean quit added one such process.

## Cause

`Theme.desktop.kt` registers a listener on jSystemThemeDetector 3.8's `OsThemeDetector`. On Linux the library always uses `GnomeThemeDetector`, whose `registerListener` starts a daemon thread that runs `Runtime.exec("gsettings monitor org.gnome.desktop.interface gtk-theme")` and reads its output with a blocking `readLine()`. The library destroys the process only when the thread is interrupted and then reads another line; nothing does that on JVM exit. `gsettings` never notices that its parent is gone: GLib ignores SIGPIPE and `g_print` ignores write errors, so it keeps running through theme changes with nobody reading its output, and it does not read stdin. It exits only when signalled or when the session bus closes, at logout. A leftover with its output closed survived three theme changes and exited when its session bus was stopped.

## Fix

A JVM shutdown hook, registered where the listener is registered, sends SIGTERM to the app's direct child processes whose executable path contains `gsettings`.

- It is installed once, when the detector is created, i.e. only when the monitor can exist.
- The filter is needed: other children must survive the exit (on Linux the app also starts `notify-send`, `xdg-mime`, and `xdg-open` for the downloaded `.deb` update). `contains` rather than `endsWith("/gsettings")`: after a glib upgrade the running binary's path is `/usr/bin/gsettings (deleted)`; NixOS wrappers are named `.gsettings-wrapped`.
- A `gsettings get` from the detector's `isDark()` that happens to run at that moment is stopped too; harmless at exit.
- It is installed on Linux only; on macOS and Windows the library starts no child process.

Alternatives considered:
- `detector.removeListener` on exit: only interrupts the thread, which is blocked in `readLine()`; the process is not destroyed.
- Destroying all children: kills the `.deb` installer opened by the updater and other handlers.
- Stopping it at `exitProcess(0)` in `DesktopApp.kt`: misses SIGTERM/SIGHUP (session logout).
- Upgrading the library: no release or its master stops the monitor on exit (upstream issue #42; the fix proposed there, PR #43, is the same shutdown hook, unmerged). 3.9.x also loops at full CPU once the monitor has exited (#52).
- Covering SIGKILL and crashes too needs the app to start the monitor itself: a `sh` wrapper that kills it when the app's pipe closes (tested), `setpriv --pdeathsig` (fires when the spawning thread exits, so it needs a dedicated thread), or polling `gsettings get` instead of a monitor. Each replaces part of the library for exits where a leftover lasts only until logout.

Not covered (no in-process code runs): SIGKILL, JVM crashes, native `exit()`. Ctrl-C in a terminal does not need it: the monitor is in the terminal's foreground process group, gets the SIGINT itself and exits; the app does not quit on the first Ctrl-C (the Haskell runtime takes it) and exits natively on a second one.

Side effect: when the monitor is stopped, the library's thread reads end-of-file and throws a `NullPointerException` (`readLine()` returned null); on some exits the JVM prints its stack trace to stderr before the process ends.

## Verification

Desktop Linux AppImage, the same base without (before) and with (after) the change, fresh profile:

| # | Check | before | after |
|---|---|---|---|
| G1 | Window closed (`xd quit`): the app's `gsettings monitor` child | still running, ppid 1 | gone |
| G2 | App sent SIGTERM: its child | still running, ppid 1 | gone |
| G3 | While the app runs: one `gsettings monitor` child, alive after ~20 s of use | yes | yes |
| G3 | Settings → Appearance, color mode System: the app follows the system (light) | yes | yes |
| G3 | Profile opens normally after G1/G2 | yes | yes |
| G4 | `App crashed` / `E:` / `Exception` log lines | none | none |

Android is not affected: the change is in `desktopMain`, which the APK does not contain.
