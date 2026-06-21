# Fix: in-app updater deletes the downloaded file before the user can open/install it

## Symptom

Desktop in-app updater (`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/views/helpers/AppUpdater.kt`): after a successful download the "Download completed" dialog appears, but clicking **"Open file location"** opens an **empty** `/tmp/simplex` — the downloaded artifact is gone. Reported against a Linux AppImage build; verified from a terminal that the file was genuinely absent on disk (not a file-manager display glitch).

## Root cause

`downloadAsset` writes the download to a temporary UUID-named file created by `createTmpFileAndDelete`, whose contract is to delete that temp file in a `finally` block. To keep the bytes, the code renames the temp file to the asset name so the survivor sits at a *different* path than the one the `finally` deletes:

```kotlin
createTmpFileAndDelete { file ->                 // file = /tmp/simplex/<UUID>
  file.outputStream().use { output -> stream.copyTo(output) }
  val newFile = File(file.parentFile, asset.name)
  file.renameTo(newFile)                         // return value IGNORED
  ... show "Download completed" dialog ...
}                                                // finally { tmpFile.delete() }
```

`File.renameTo` returns a boolean and **its result was ignored**. When the rename succeeds (the common case) the survivor is `newFile` and the `finally` deletes the now-absent UUID path (a no-op) — everything works. But if the rename returns `false`, the bytes stay at the UUID path, `newFile` is never created, and the `finally { tmpFile.delete() }` deletes the only copy. The dialog is still shown (the rename result was never checked), so the user sees "Download completed" over an empty directory.

The download path is **shared by every platform and asset type** (`.AppImage`, `.deb`, Windows `.msi`, macOS `.dmg`), so this affected all of them — most acutely `.deb`, where the in-app "Install" button is hidden and "Open file location" is the only way forward.

## Fix

Replace the unchecked `renameTo` with `Files.move(..., REPLACE_EXISTING)`:

```kotlin
val newFile = File(file.parentFile, asset.name)
Files.move(file.toPath(), newFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
```

Behaviour:

- **Same in-place rename in the normal case.** Verified that a same-directory `Files.move` preserves the inode — it executes the same `rename(2)` syscall as `renameTo`, with no copy. No behaviour or performance change on the happy path.
- **Recovers when an in-place rename is not possible** (e.g. cross-filesystem): falls back to copy-then-delete, so `newFile` still ends up present.
- **Surfaces genuine failures** by throwing, which the existing outer `catch (e: Exception)` in `downloadAsset` already handles (logs the error) — instead of silently deleting the download and showing a misleading "Download completed" dialog.

One line changed (plus a clarifying comment). `Files` / `StandardCopyOption` are already imported.

## Compatibility impact

Before this change the updater's download step worked only on *some* Linux systems — it failed on those where `File.renameTo` returns `false`. In particular it did **not** work on **Whonix**, where the "Download completed" dialog appeared over an empty folder and the update could not proceed. `Files.move` succeeds on those systems too (in-place rename when possible, copy+delete otherwise, throwing only on genuine failure), so this fix expands the set of platforms on which the in-app updater works — Whonix included — without changing behaviour where `renameTo` already succeeded.

## Scope / out of scope

- This change is limited to the shared download step; it fixes the reported symptom on every OS at once.
- The per-OS *install* paths are untouched. A separate, related fragility remains in the macOS install branch (`File("/Applications/SimpleX.app").renameTo(...)` return value ignored at the app-replace/restore steps); it is a different symptom (botched/missing install, not a lost download) and is left for a follow-up, consistent with the out-of-scope list in `plans/2026-05-16-desktop-updater-fixes.md`.
