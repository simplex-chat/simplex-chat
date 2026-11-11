# Desktop: macOS theme detector logs an error in light mode

## Problem

On macOS the app prints `ERROR com.jthemedetecor.MacOSThemeDetector - Couldn't execute theme name query with the Os` with a stack trace.

## Cause

`MacOSThemeDetector.isDark()` reads `AppleInterfaceStyle`, which macOS leaves unset in light mode. `toStringViaUTF8` then returns `null`, `isDarkTheme(null)` throws `NullPointerException`, and the library logs it through slf4j-simple and returns `false`. The library catches the exception itself, so the app's `catch (e: Exception)` is never reached. Read from the jSystemThemeDetector 3.8 and jfa 1.2.0 bytecode; the unset key in light mode is not verified on a Mac.

## Fix

`simplelogger.properties` turns off the `com.jthemedetecor.MacOSThemeDetector` logger. This also hides that logger's other error ("Observer method cannot be added") and any other `RuntimeException` that `isDark()` catches; `isDark()` returns `false` as before. The oshi and Gnome loggers still print.

## Approaches considered

1. Turn off the logger (chosen): slf4j-simple can only set a level per logger, so this is the smallest change.
2. Read `AppleInterfaceStyle` in the app instead of the library: removes the cause, but is larger and cannot be tested here.
3. Bump jSystemThemeDetector to 3.9.1, which adds the null check: a different change. The same release rewrites the Gnome detector (it also watches `color-scheme`, and its loop does not block when `readLine` returns `null`, so it busy-loops once the `gsettings` process exits), which would need its own Gnome and shutdown-hook testing.

## Scope

Desktop only (`desktopMain`); in effect on macOS only.
