# Fix: desktop crash + blinking window when temp dir is missing during a call

Date: 2026-06-29
Area: `apps/multiplatform` — desktop (Compose) audio/recording

## Symptom

On Windows, when an incoming call arrives the window starts blinking continuously and
does not stop until the call ends or the app is killed. The logs show:

```
java.io.FileNotFoundException: C:\Users\<user>\AppData\Local\Temp\simplex\<uuid> (The system cannot find the path specified)
    at java.base/java.io.FileOutputStream.open0(Native Method)
    ...
    at chat.simplex.common.platform.SoundPlayer.start(RecAndPlay.desktop.kt:276)
    at chat.simplex.common.views.call.IncomingCallAlertViewKt$IncomingCallAlertView$1$1.invokeSuspend(IncomingCallAlertView.kt:32)
    ...
    at androidx.compose.ui.scene.BaseComposeScene.render(BaseComposeScene.skiko.kt:171)
```

## Root cause

`tmpDir` is declared once as a top-level `val`:

```kotlin
// common/.../platform/Files.desktop.kt:13
actual val tmpDir: File =
  File(System.getProperty("java.io.tmpdir") + File.separator + "simplex").also { it.deleteOnExit() }
```

It registers `deleteOnExit()` but is **never created** at declaration. The directory is
created at startup by `Main.kt:30-31`, which does `tmpDir.deleteRecursively()` then
`tmpDir.mkdir()` on every launch — so on a normal session the directory exists by the
time the UI runs. A handful of features re-create it on demand (`NtfManager.desktop.kt`,
`DatabaseView.kt`), but none of those is on the incoming-call path.

Three writers assume the directory already exists and write into it without creating it:

- `SoundPlayer.start` — `RecAndPlay.desktop.kt:276` (the crash site; incoming-call ring)
- `CallSoundsPlayer.start` — `RecAndPlay.desktop.kt:297` (connecting / in-call sounds)
- `RecorderNative.start` — `RecAndPlay.desktop.kt:29` (voice messages)

Because `Main.kt` creates the directory at startup, the realistic trigger is **not** a
cold start but **mid-session deletion**: an OS temp cleaner (Windows Storage Sense / Disk
Cleanup) purging `...\Temp\simplex` while the app sits open, hours or days before a call
arrives. Nothing re-creates the directory after that until the next launch, so the next
writer to touch it fails. `SoundPlayer.start` and `CallSoundsPlayer.start` fail at
`tmpFile.outputStream()` with `FileNotFoundException` (as in the stack trace);
`RecorderNative.start` fails at `File.createTempFile(..., tmpDir)` with the equivalent
`IOException`. The reported crash is itself evidence this happens in reality — the user
hit it despite `Main.kt` creating the directory at startup.

That the directory really is deleted at runtime is already acknowledged elsewhere in the
codebase: `DatabaseView.kt` lines 568 and 570 do `tmpDir.deleteRecursively()` followed by
`tmpDir.mkdir()`.

### Why it blinks instead of just failing once

Of the three writers, only `SoundPlayer.start` is on the Compose render path: it is
invoked synchronously inside a `LaunchedEffect` in `IncomingCallAlertView.kt:30-32`, with
no surrounding try/catch. The stack trace confirms the exception unwinds through the
render/flush path (`FlushCoroutineDispatcher.flush` -> `BaseComposeScene.render`) and
escapes into the AWT event-dispatch thread. The incoming-call alert's composition never
commits cleanly, so the window keeps repainting — the continuous blinking — until the
alert is dismissed (call stops) or the process is killed.

The other two writers run off the render path — `CallSoundsPlayer.start` from background
coroutines in `CallView` (`withBGApi`), `RecorderNative.start` from a user-initiated
record action — so the same missing-dir bug there fails the sound/recording once rather
than producing a render loop. They are the same latent bug, guarded in the same pass.

## Fix

Guarantee the parent directory exists immediately before writing, on each of the three
writers in `RecAndPlay.desktop.kt`:

```kotlin
tmpDir.mkdirs()
```

`mkdirs()` is idempotent (no-op when the directory already exists), cheap, and does not
change control flow, so it is safe on every call. Crucially, placing it at the point of
use — rather than only at the `tmpDir` declaration — also recovers from **mid-session**
deletion, which a one-time initializer structurally cannot: a top-level `val` initializer
runs exactly once, so it cannot re-create a directory removed later by an OS temp cleaner.

### Related but NOT fixed here: `coreTmpDir` (core file transfers)

A separate error in a **different** directory looks like the same bug but is not, and is
deliberately left out of this change:

```
error chat exception ...\AppData\Roaming\SimpleX\tmp\<ts>_snd.xftp:
CreateDirectory "\\?\C:\Users\<user>\AppData\Roaming\SimpleX\tmp\<ts>_snd.xftp": does not exist
```

`coreTmpDir` (`Files.desktop.kt:17` = `...\AppData\Roaming\SimpleX\tmp`) is handed to the
core via `apiSetAppFilePaths` (`Core.kt:125`). The core **already** creates it recursively
at startup — `Commands.hs:546-554` (`APISetAppFilePaths` → `setFolder` →
`createDirectoryIfMissing True`). So a Kotlin-side `mkdirs()` on the declaration would be
**redundant** (duplicates the core) and **ineffective**: like the core's own startup
creation it runs once at init, so it cannot help when the directory is deleted
*mid-session* (its `Roaming` location points to a third-party cleaner / profile-sync /
manual deletion rather than OS temp cleanup).

The real cause is that the XFTP agent creates the send work dir with a **non-recursive**
`createDirectory prefixPath` (simplexmq `FileTransfer/Agent.hs:357`, `:370`, rcv `:132`),
which throws when `coreTmpDir` is missing at send time. The surgical fix is
`createDirectory` → `createDirectoryIfMissing True` in the agent — an upstream `simplexmq`
change, out of scope for this repo-side PR.

## Scope / what this does not change

- Only these three writers are guarded. `SoundPlayer.start` is the one the bug report
  exercises and the only one that fails catastrophically (Compose render loop); the other
  two share the identical missing-dir bug on non-render paths and are guarded defensively
  in the same change.
- The root `tmpDir` declaration is intentionally left unchanged, and a root-level
  `mkdirs()` is deliberately **not** added. It would help nothing: cold start is already
  covered by `Main.kt:30-31`, and a one-time `val` initializer cannot survive the
  mid-session deletion that is the only real trigger.
- The other on-demand consumers (`Share.desktop.kt`, `PlatformTextField.desktop.kt`,
  `Images.desktop.kt`, `ChatModel.kt`, `Utils.kt`) share the same latent exposure to
  mid-session deletion, but each already wraps its temp write in `try/catch` and fails
  gracefully (a logged error or a single error alert, feature no-ops) — none blinks or
  crashes. They are left unguarded here; hardening them would require per-use `mkdirs()`
  at each site and is low value given the graceful failure, so it is out of scope.

## Verification

Windows-only filesystem path; the root cause was established by reading the code and the
stack trace rather than reproduced on Linux. Each change is a single idempotent `mkdirs()`
call before an existing `RecAndPlay.desktop.kt` temp-file write, with no change to control
flow.
