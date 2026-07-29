# Fix: chat switch deletes the in-app updater's download mid-transfer

## Symptom

Desktop in-app updater (Linux AppImage, v7.0.0-beta.5): the download progress reaches 100%, and then
nothing happens — no "App update is downloaded" dialog, no error, and `/tmp/simplex` is empty.
The update cannot proceed and there is no indication of why.

Captured by starting the app from a terminal (the log goes nowhere otherwise — see *Why this was invisible*):

```
E: Failed to download the asset from release: java.nio.file.NoSuchFileException: /tmp/simplex/e859e2cb-b4c6-4ebf-812d-c327812db212
    at java.base/sun.nio.fs.UnixCopyFile.move(Unknown Source)
    at java.base/java.nio.file.Files.move(Unknown Source)
    at chat.simplex.common.views.helpers.AppUpdaterKt.downloadAsset$lambda$...(AppUpdater.kt:333)
    at chat.simplex.common.views.helpers.UtilsKt.createTmpFileAndDelete(Utils.kt:401)
    at chat.simplex.common.views.helpers.AppUpdaterKt.downloadAsset(AppUpdater.kt:323)
```

The exception names the **source** path, not the destination: the file being moved no longer exists.

## Root cause

The updater downloads into `createTmpFileAndDelete`, which registers every file it creates in a
chat-scoped cleanup set:

```kotlin
fun <T> createTmpFileAndDelete(dir: File = tmpDir, onCreated: (File) -> T): T {
  val tmpFile = File(dir, UUID.randomUUID().toString())
  tmpFile.parentFile.mkdirs()
  tmpFile.deleteOnExit()
  ChatModel.filesToDelete.add(tmpFile)          // <-- kills the download
  try { return onCreated(tmpFile) } finally { tmpFile.delete() }
}
```

`ComposeView.deleteUnusedFiles()` empties that set:

```kotlin
chatModel.filesToDelete.forEach { it.delete() }
chatModel.filesToDelete.clear()
```

and it is called from `KeyChangeEffect(chatModel.chatId.value)` — on chat open, switch and close,
whenever the composer is empty and there is no draft (`ComposeView.kt:1320`), plus after sending a
live message (`:1301`) and a voice message (`:860`).

`downloadAsset` writes the ~350 MB update into that temp file, which takes minutes, and the toast shown
during it ("Downloading app update, don't close the app") invites the user to keep using the app
meanwhile. Any chat switch in that window deletes the file out from under the running download.
`copyTo` keeps writing through the still-open file descriptor, so the transfer completes normally and
the progress indicator reaches 100%; only the subsequent `Files.move` fails.

The failure is then swallowed by `downloadAsset`'s outer `catch`, which only calls `Log.e` — on desktop
that is a bare `println` (`Log.desktop.kt`), invisible unless the app was launched from a terminal.
Net effect: silence.

This is not specific to the updater. The same window exists for the settings and themes writers
(`Resources.desktop.kt`, `Files.kt`), which use the same helper and then `Files.move` the temp file into
place; a chat switch landing between the write and the move surfaces there as an
"Error saving settings" alert and a rethrow.

### Why this was invisible until v7.0-beta.1

Before #7104 the same line was `file.renameTo(newFile)`, whose `false` return was ignored, so the
dialog appeared regardless — over an empty folder (that was the bug #7104 fixed). Replacing it with
`Files.move` was correct, but it converted a silently-ignored failure into a thrown exception routed
into a stdout-only log, which turned the visible-but-wrong symptom into no symptom at all. The
underlying deletion predates both.

## Fix

`downloadAsset` stops using `createTmpFileAndDelete` and owns the file itself:

```kotlin
val newFile = File(tmpDir, asset.name)
if (desktopPlatform.isWindows()) newFile.delete()
val partFile = File(tmpDir, "${asset.name}.part")
partFile.parentFile.mkdirs()
partFile.deleteOnExit()
try {
  partFile.outputStream().use { output -> stream.copyTo(output) }
  Files.move(partFile.toPath(), newFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
  AlertManager.shared.showAlertDialogButtonsColumn(...)   // unchanged
} finally {
  partFile.delete()
}
```

The download is never registered in `ChatModel.filesToDelete`, so `deleteUnusedFiles()` cannot reach
it and a chat switch during the transfer is harmless. Cleanup is unchanged in substance: the `finally`
removes a partial file when the download fails or is cancelled (and is a no-op after a successful
move, exactly as the helper's own `finally` was), `deleteOnExit()` covers a clean exit, and `Main.kt`
wipes `tmpDir` at startup after a crash — a leftover `.part` is also truncated by the next download.

This is the fix requested in review (#7295): the helper's contract is a scratch file that dies with
the lambda, and the updater's file is neither — it is moved to `asset.name` and kept for the user to
install, so the helper's `finally` never applied to it on the success path. `createTmpFileAndDelete`
is left as it is on master, with a comment warning that its file does not survive a chat switch.

## Other callers

Unchanged. The same trap still exists for anyone whose lambda outlives a chat switch, but no current
caller does: the settings/themes writers (`Resources.desktop.kt` ×2, `Files.kt`) write and `Files.move`
within milliseconds, and `CIFileView.kt`, `Utils.kt` and `Share.android.kt` finish with the file inside
the lambda. See "Follow-up" below.

## Alternatives considered

- **Removing `ChatModel.filesToDelete.add(tmpFile)` from the helper** (the first version of this PR) —
  fixes the whole class rather than one caller, and the registration is redundant with the helper's
  own `finally` at every call site. Rejected in review as too broad for this bug: it changes behaviour
  at six unrelated call sites to fix the updater. Kept as a follow-up, see below.
- **`filesToDelete.remove(file)` at the updater's call site** — leaves a window between the `add`
  inside the helper and the `remove` in the lambda, and adds a third concurrent mutation of
  `ChatModel.filesToDelete` (a plain `mutableSetOf`, already mutated from both `Dispatchers.Default`
  and the main thread).

## Follow-up (not in this PR)

`createTmpFileAndDelete` still registers its file in `ChatModel.filesToDelete` from whatever thread
calls it, including `Dispatchers.IO` (settings save, `saveFileFromUri`), while
`ComposeView.deleteUnusedFiles()` iterates that plain `mutableSetOf` on the UI thread — a concurrent
`add` during that `forEach` throws `ConcurrentModificationException`. The registration is also
redundant with the helper's `finally` at all six call sites. Removing it, or making the set
thread-safe, is a separate change.

Note also that `preferencesTmpDir` on desktop is `File(configPath, "tmp")` — outside `tmpDir`, so it is
not covered by the startup wipe in `Main.kt`; its temp files rely on the `finally` and `deleteOnExit`.

## Testing

Repro and verification were done against the first version of this fix: with `desktop.version_name`
temporarily set to `7.0-beta.4` the updater offers v7.0.0-beta.5, and switching chats repeatedly during
the download reproduces the disappearing file and the missing dialog. This version compiles
(`:common:compileKotlinDesktop`); the manual download-and-switch run should be repeated before merge,
including the install path on Windows (where the app exits before deleting the file).

## Out of scope

`downloadAsset` and `checkForUpdate` report failures only via `Log.e`, which on desktop reaches nothing
but stdout — that is why this bug went unnoticed. Surfacing updater failures to the user is a separate
concern: it would not have prevented the deletion, and this change does not make future failures
visible. Left for a follow-up.
