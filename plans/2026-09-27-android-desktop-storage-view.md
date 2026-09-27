# Android, desktop: storage breakdown in developer options

## Problem

When the Android or desktop app takes a lot of space, there is no way to see from inside the app what is using it. The Database screen shows only the count and size of received files (`directoryFileCountAndSize(appFilesDir)`, `Utils.kt`), and that helper is not recursive: it adds up `File.length()` of the direct children only.

The databases, their `-wal`/`.bak` copies, wallpapers, core temp files, migration leftovers, remote-host data and preferences are all invisible. On one desktop profile, for example, the `.bak` copies left by database migrations were 5.6 MB of the 13 MB of database files.

## Cause

iOS has had this screen since #5529 (`apps/ios/Shared/Views/UserSettings/StorageView.swift`, under Developer options), but it was never ported to Kotlin.

## Fix

A Storage item in Developer options (shown only when "Show developer options" is on) opens `StorageView`. The screen lists each top-level entry of the app's storage folders with its recursive size, largest first, and a total per folder.

Folders measured: `dataDir`, `preferencesDir` and `tmpDir`. Any folder that is the same as, or inside, another one is dropped, so nothing is counted twice:

| Platform | Folders shown |
|---|---|
| Android | `dataDir` only. `shared_prefs` and `app_temp` are inside it, and so are the databases, `files/`, `cache/` (exports) and `app_temp/remote_hosts`. |
| Linux, macOS | `$XDG_DATA_HOME/simplex`, `$XDG_CONFIG_HOME/simplex`, `java.io.tmpdir/simplex` |
| Windows | `%AppData%\SimpleX` (config and data are the same folder), `%TEMP%\simplex` |

Like iOS, it shows whatever entries actually exist rather than named categories. Named categories would need to be kept in sync with the path helpers and would hide anything unexpected, and unexpected entries (such as `.bak` files) are what this screen is for.

It improves on iOS in four ways:

- It measures on `Dispatchers.IO`; iOS measures on the main thread in `.onAppear`.
- It sorts rows by size; iOS iterates a dictionary, so the order is random.
- It shows a total per folder.
- An unreadable file is logged and skipped. On iOS, the first error ends the walk.

The walk uses `Files.walkFileTree` (API 26 = minSdk) without `FOLLOW_LINKS`, so a link inside the tree cannot loop or escape to `/`. A top-level entry that is a symlink is resolved with `toRealPath()` first, so a files folder moved to another disk and linked back is still measured. A broken link falls back to the link itself.

Sizes are file lengths (`BasicFileAttributes.size()`). iOS uses allocated size, which has no portable equivalent on the JVM, and length is what the existing Database screen reports.

## Verification

- A standalone harness copying the walk logic, run against:
  - a symlink loop and a link to `/`: not followed
  - an unreadable folder: logged, the walk continues
  - a nested folder and a duplicate folder: dropped
  - a missing folder: shown as empty
  - a symlinked top-level folder: target measured
  - a broken link: the link's own size
- On a real 187 MB desktop profile, the walk took 0.8 s and matched `du -sb` exactly (`du` also counts the 4 KB folder entry).
- A desktop AppImage run on a separate test profile showed all three folders, with sizes matching `ls`.

## Not included

- The desktop Postgres build keeps its database on the Postgres server, so it is not in the total.
- Leaving the screen does not cancel a walk in progress. The result is simply discarded.
