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
cold start but **mid-session deletion**. The precise deleter is a **transient second
instance**, via the `deleteOnExit()` on the shared path:

1. `deleteOnExit()` on `tmpDir` (Files.desktop.kt:13) registers `...\Temp\simplex` for
   removal at JVM shutdown.
2. `acquireSingleInstance()` (SingleInstance.kt:33) touches `dataDir`. `dataDir` and
   `tmpDir` are top-level `val`s in the **same file**, so one facade-class `<clinit>`
   initializes both — meaning any process that reaches `acquireSingleInstance` registers
   `tmpDir`'s `deleteOnExit()`, even though it never uses `tmpDir`.
3. A second launch (`Main.kt:23`, `if (!acquireSingleInstance()) return`) sees the lock
   held, signals the primary, and returns — the second JVM exits **normally**, firing the
   shutdown hook: `File("...\Temp\simplex").delete()`.
4. `File.delete()` on a directory succeeds only if it is **empty**, which explains the
   intermittency — the folder is removed when a second instance exits while `tmpDir`
   happens to be empty (e.g. just after the primary's startup wipe). The **still-running
   primary** now has no temp directory.

The next writer then fails: `SoundPlayer.start` / `CallSoundsPlayer.start` at
`tmpFile.outputStream()` with `FileNotFoundException` (as in the stack trace);
`RecorderNative.start` at `File.createTempFile(..., tmpDir)` with the equivalent
`IOException`.

An OS temp cleaner (Windows Storage Sense / Disk Cleanup) is a possible secondary cause,
but the second-instance path above is concrete and in-app. That the directory is deleted
at runtime is also acknowledged elsewhere: `DatabaseView.kt` lines 568/570 do
`tmpDir.deleteRecursively()` then `tmpDir.mkdir()`.

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

**Root cause — no destructive filesystem side effects in `Files.desktop` `val`
initializers.** Top-level `val` initializers run in *any* process that touches the facade
class, including a transient second instance (via `acquireSingleInstance()` →
`dataDir`). Two of them **delete** shared state and must not run there:

- `tmpDir` had `.also { it.deleteOnExit() }` — a second instance's normal exit deleted
  `...\Temp\simplex`.
- `preferencesTmpDir` had `.also { it.deleteRecursively() }` — a second instance's
  `<clinit>` wiped `configPath\tmp` (the same anti-pattern, firing even earlier).

Make both declarations pure and move the destructive work into `Main`, past the
single-instance check, so only the owning instance performs it:

```kotlin
// Main.kt
if (!acquireSingleInstance()) return
preferencesTmpDir.deleteRecursively()   // was a val initializer; early, before settings writes
...
tmpDir.deleteRecursively()
tmpDir.mkdir()
tmpDir.deleteOnExit()                    // only the owning instance cleans up on exit
```

A transient second instance returns before these lines, so it no longer damages the
running primary's temp dirs. The remaining `val`-initializer side effects
(`wallpapersDir.mkdirs()`, `preferencesDir.parentFile.mkdirs()`) are **creations** — they
are idempotent and destroy nothing, so they are intentionally left in place.

### Considered and not included: point-of-use `tmpDir.mkdirs()` guards

An earlier version of this fix also added `tmpDir.mkdirs()` before each of the three
writers in `RecAndPlay.desktop.kt`, to recover from deletion by any *other* actor (a
Windows temp cleaner sweeping `%LOCALAPPDATA%\Temp`, or the `DatabaseView` delete/recreate
window). These were removed once the root cause was fixed: the reported failure was the
second-instance deletion, which no longer happens, and the directory is otherwise created
at startup. The guards were judged redundant against the remaining low-probability cases
and dropped in favour of the smaller, root-cause-only change. (`createTmpFileAndDelete`
still keeps its own `parentFile.mkdirs()`, so the preferences-temp path remains guarded.)

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
