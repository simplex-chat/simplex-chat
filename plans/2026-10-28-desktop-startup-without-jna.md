# Desktop starts when JNA cannot load

## Problem

On desktop the app exits before any window opens when the JNA native library cannot be loaded, for example when the JNA temp directory (`~/.cache/JNA/temp`) is on a `noexec` mount. The log shows `NoClassDefFoundError: Could not initialize class com.sun.jna.Native` from `Resources_desktopKt.<clinit>`, caused by `UnsatisfiedLinkError: ... failed to map segment from shared object`. System theme detection is optional, yet it ends the startup.

## Cause

- `Resources.desktop.kt` (bf91666f0, #4109) and `Theme.desktop.kt` (26a233ab1, #2755) each create the detector in a top-level `val` (`OsThemeDetector.getDetector()`).
- `getDetector()` loads oshi, which loads JNA. The failure is a `LinkageError` thrown from a static initializer, outside the `try` around `isDark`, so nothing catches it.
- `Resources.desktop.kt` also declares `settings`, so the first preference read (`AppPreferences`) triggers the failing initializer.

## Fix

- `systemThemeDetector` in `Resources.desktop.kt` replaces the private `detector`: `getDetector()` inside `try`, `catch (_: LinkageError)` returns `null`. `WindowActivation.kt` catches `LinkageError` for the same condition (#7605).
- It stays an eager top-level `val`, so the detector is created at the same point as before. It does not log: `Log.w` reads `appPrefs`, and `AppPreferences` is under construction when `Resources_desktopKt` initialises (the first `settings` read), so logging there re-enters `appPrefs` and fails with a `NullPointerException`. On Linux oshi has already logged the failure; on Windows and macOS nothing is logged (not verified).
- `isInNightMode()` and `isSystemInDarkTheme()` return `false` (light theme) when there is no detector, the same value the existing `catch (e: Exception)` returns.
- In `Theme.desktop.kt`, `detector` is initialised from `systemThemeDetector`; `getDetector()` returns one cached instance, so both files already shared it. The change listener and the gsettings shutdown hook (#7747) are registered only when a detector exists.

## Approaches considered

1. Shared eager accessor with a `LinkageError` guard (chosen).
2. Lazy accessor that logs a warning: no re-entrancy, but moves the detector's start-up cost (about 190 ms measured on the baseline) from `runMigrations` to the first composition on the UI thread.
3. A guard in each file: the same fact (how to continue when JNA cannot be loaded) stated twice.
4. Guard only `Resources.desktop.kt`, the file in the report's trace: `Theme.desktop.kt` would throw the same error when the window starts.
5. Point JNA at an executable temp directory at launch: a different change (running native code extracted into a user-writable directory) and it does not help when mapping fails for another reason.

## Scope

Desktop only (`desktopMain`); `spec/services/theme.md` is updated to the new code. Not changed: JNA temp directory, VLC/video behaviour without JNA. A JNA native library of another version selected through an explicit `jna.boot.library.path` or `jna.nosys=false` throws `java.lang.Error`, not `LinkageError`, and is not caught; the app sets neither property.
