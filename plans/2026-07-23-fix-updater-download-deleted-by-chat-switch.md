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

`createTmpFileAndDelete` registers every temp file it creates in a global, chat-scoped cleanup set:

```kotlin
fun <T> createTmpFileAndDelete(dir: File = tmpDir, onCreated: (File) -> T): T {
  val tmpFile = File(dir, UUID.randomUUID().toString())
  tmpFile.parentFile.mkdirs()
  tmpFile.deleteOnExit()
  ChatModel.filesToDelete.add(tmpFile)          // <-- the defect
  try { return onCreated(tmpFile) } finally { tmpFile.delete() }
}
```

`ComposeView.deleteUnusedFiles()` empties that set unconditionally:

```kotlin
chatModel.filesToDelete.forEach { it.delete() }
chatModel.filesToDelete.clear()
```

and it is called from `KeyChangeEffect(chatModel.chatId.value)` — that is, on **every chat open, switch
and close**.

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

Remove the registration:

```diff
 fun <T> createTmpFileAndDelete(dir: File = tmpDir, onCreated: (File) -> T): T {
   val tmpFile = File(dir, UUID.randomUUID().toString())
   tmpFile.parentFile.mkdirs()
   tmpFile.deleteOnExit()
-  ChatModel.filesToDelete.add(tmpFile)
   try {
```

The registration provided nothing that was not already covered:

- the helper's own `finally { tmpFile.delete() }` always removes the file;
- `deleteOnExit()` covers a clean JVM exit;
- `Main.kt` wipes `tmpDir` at startup, covering a crash;
- `filesToDelete` is in-memory, so it survives no crash and aids no crash recovery.

It only ever duplicated the `finally`, while arming a trap for any caller whose lambda runs long.

## Why this is safe for the other callers

Every remaining call site was checked. None returns the temp file or lets it outlive the lambda, so
the registered path is already gone by the time `deleteUnusedFiles()` could reach it — deleting it
again was a no-op.

| Call site | temp file lifetime | effect of the registration |
| --- | --- | --- |
| `AppUpdater.kt` (update download) | minutes | **deleted the file mid-transfer** |
| `Resources.desktop.kt` (settings save) | ms, then `Files.move` | no-op |
| `Resources.desktop.kt` (themes save) | ms, then `Files.move` | no-op |
| `Files.kt` (themes.yaml) | ms, then `Files.move` | no-op |
| `CIFileView.kt` (decrypt then save) | seconds, explicit `delete()` | no-op |
| `Utils.kt` (encrypt into app files) | seconds, `finally` deletes | no-op |
| `Share.android.kt` (decrypt to MediaStore) | seconds, `finally` deletes | no-op |

`ChatModel.filesToDelete` keeps working as before: its legitimate entries come from seven direct
`add()` calls in the compose/share/paste flows (`ComposeView.kt`, `ChatModel.kt`,
`PlatformTextField.desktop.kt`, `Images.desktop.kt`, `Share.android.kt`, `Utils.android.kt`,
`GetImageView.android.kt`), none of which this change touches.

## Alternatives considered

- **Opt-out parameter on the helper** (`createTmpFileAndDelete(dir, track = false)`) — adds API surface
  for a behaviour no caller wants, and leaves the wrong registration in place for everyone else.
- **`filesToDelete.remove(file)` at the updater's call site** — smallest blast radius, but it leaves a
  window between the `add` inside the helper and the `remove` in the lambda, adds a third concurrent
  mutation to `ChatModel.filesToDelete` (a plain `mutableSetOf`, already mutated from both
  `Dispatchers.Default` and the main thread), and leaves the settings/themes writers exposed to the
  same race.

Removing the line is a smaller diff than either and removes the defect rather than routing around it.

## Testing

Built with `desktop.version_name` temporarily set to `7.0-beta.4` so the updater offers v7.0.0-beta.5,
then during the download switched chats repeatedly. Before the change the download file disappears and
the dialog never appears; after it the download completes, the file is present in `tmpDir` under the
asset name, and the "App update is downloaded" dialog is shown.

## Out of scope

`downloadAsset` and `checkForUpdate` report failures only via `Log.e`, which on desktop reaches nothing
but stdout — that is why this bug went unnoticed. Surfacing updater failures to the user is a separate
concern: it would not have prevented the deletion, and this change does not make future failures
visible. Left for a follow-up.
