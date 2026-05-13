# Desktop single instance — implementation plan

Companion to the design at `plans/2026-05-13-desktop-single-instance.md`. Read that first.

## What

Three small commits that build the feature incrementally. After each commit the build is green and the app still runs; the first commit already prevents the worst symptom (duplicate process hitting the SQLite lock), and the third makes the window-restore UX work end-to-end.

## Why

We split this way so each commit is reviewable and revertable on its own. The order is chosen so that the build stays green and so that the first commit alone is a worthwhile bugfix even if the rest is held back.

## How

### Pre-flight

- On branch `sh/tray-followup` (current). It is ahead of `stable` by the tray-followup commits and the two `plans/` commits for the single-instance spec.
- Confirm the desktop build is green before changing anything: `cd apps/multiplatform && ./gradlew :common:desktopMainClasses` — should succeed.
- Confirm tests run: `./gradlew desktopTest` — `SemVerTest` should pass.
- Read `plans/2026-05-13-desktop-single-instance.md` end to end.

---

### Task 1 — File lock + early-exit signaller (no IPC yet)

**Files**
- Create: `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/SingleInstance.desktop.kt`
- Modify: `apps/multiplatform/desktop/src/jvmMain/kotlin/chat/simplex/desktop/Main.kt` (entry-point)
- Create: `apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/SingleInstanceTest.kt`

**What to add.** A module-level lock acquisition that runs before any Haskell / DB init. The function returns `true` if this process owns the lock (proceed with normal startup), `false` if another process already owns it (caller exits `main`). No IPC yet — the second instance simply exits silently. This already fixes the SQLite-contention crash that motivated the work.

```kotlin
package chat.simplex.common

import chat.simplex.common.platform.Log
import chat.simplex.common.platform.TAG
import chat.simplex.common.platform.dataDir
import java.io.IOException
import java.nio.channels.FileChannel
import java.nio.channels.FileLock
import java.nio.channels.OverlappingFileLockException
import java.nio.file.StandardOpenOption.CREATE
import java.nio.file.StandardOpenOption.READ
import java.nio.file.StandardOpenOption.WRITE

// Held for the process lifetime. Module-level `var`s deliberately, so the
// FileChannel isn't garbage-collected — a GC'd FileChannel releases the lock.
private var lockChannel: FileChannel? = null
private var lockHandle: FileLock? = null

// Returns true if this process owns the single-instance lock (caller proceeds
// with normal startup). Returns false if another instance already owns it
// (caller must return from main without further init).
fun acquireSingleInstanceOrSignalAndExit(): Boolean {
  dataDir.mkdirs()
  val lockFile = dataDir.resolve("simplex.lock").toPath()
  val channel: FileChannel = try {
    FileChannel.open(lockFile, READ, WRITE, CREATE)
  } catch (e: IOException) {
    // Filesystem doesn't allow opening the lock file at all — proceed without
    // single-instance enforcement. No worse than today's behavior.
    Log.w(TAG, "single-instance: cannot open $lockFile: ${e.message}")
    return true
  }
  val held: FileLock? = try {
    // Lock exactly one byte — NOT the zero-arg form, which locks
    // [0, Long.MAX_VALUE) and is rejected by some SMB/NFS impls (JDK-6674134).
    channel.tryLock(0L, 1L, false)
  } catch (e: OverlappingFileLockException) {
    // Same JVM trying to lock twice. Treat as "we hold it" — cheaper than crashing.
    Log.w(TAG, "single-instance: overlapping lock on $lockFile")
    return true
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: tryLock failed on $lockFile: ${e.message}")
    channel.close()
    return true
  }
  if (held == null) {
    // Another instance owns the lock. IPC signalling comes in Task 3.
    channel.close()
    return false
  }
  lockChannel = channel
  lockHandle = held
  return true
}
```

**Wire into `Main.kt`.** Add the check as the very first thing in `main()`:

```kotlin
fun main() {
  if (!acquireSingleInstanceOrSignalAndExit()) return
  // Disable hardware acceleration
  //System.setProperty("skiko.renderApi", "SOFTWARE")
  initHaskell()
  runMigrations()
  setupUpdateChecker()
  initApp()
  tmpDir.deleteRecursively()
  tmpDir.mkdir()
  return showApp()
}
```

Import: `chat.simplex.common.acquireSingleInstanceOrSignalAndExit`.

**Test.** Two unit tests pin the JDK semantics the production code relies on. Cross-process contention (the path where `tryLock` returns `null`) can NOT be exercised from inside a single JVM — within one JVM, a second `tryLock` on an already-locked region throws `OverlappingFileLockException` rather than returning `null`. The production code's `catch (OverlappingFileLockException)` branch handles exactly that within-JVM case, so we test both: the exception, and the release/reacquire round-trip.

In `SingleInstanceTest.kt`:

```kotlin
package chat.simplex.app

import java.nio.channels.FileChannel
import java.nio.channels.OverlappingFileLockException
import java.nio.file.Files
import java.nio.file.StandardOpenOption.CREATE
import java.nio.file.StandardOpenOption.READ
import java.nio.file.StandardOpenOption.WRITE
import kotlin.test.Test
import kotlin.test.assertFailsWith
import kotlin.test.assertNotNull

class SingleInstanceTest {
  @Test
  fun overlappingLockOnSameRegionThrowsWithinOneJvm() {
    val tmp = Files.createTempDirectory("simplex-singleinstance-test").toFile()
    tmp.deleteOnExit()
    val lockPath = tmp.toPath().resolve("simplex.lock")

    val first = FileChannel.open(lockPath, READ, WRITE, CREATE)
    val firstLock = first.tryLock(0L, 1L, false)
    assertNotNull(firstLock, "first acquirer must get the lock")

    val second = FileChannel.open(lockPath, READ, WRITE, CREATE)
    assertFailsWith<OverlappingFileLockException> {
      second.tryLock(0L, 1L, false)
    }
    second.close()

    firstLock.release()
    first.close()
  }

  @Test
  fun releasedLockCanBeReacquired() {
    val tmp = Files.createTempDirectory("simplex-singleinstance-test").toFile()
    tmp.deleteOnExit()
    val lockPath = tmp.toPath().resolve("simplex.lock")

    val first = FileChannel.open(lockPath, READ, WRITE, CREATE)
    val firstLock = first.tryLock(0L, 1L, false)
    assertNotNull(firstLock)
    firstLock.release()
    first.close()

    val second = FileChannel.open(lockPath, READ, WRITE, CREATE)
    val secondLock = second.tryLock(0L, 1L, false)
    assertNotNull(secondLock, "after release, a fresh acquirer must succeed")
    secondLock.release()
    second.close()
  }
}
```

Note: the cross-process `null`-return path is reached only when an actually-separate JVM holds the lock — that's the production scenario, covered by the manual smoke test below, not by unit tests.

**Verify.**
- Build: `./gradlew :common:desktopMainClasses` — succeeds.
- Test: `./gradlew desktopTest` — `SingleInstanceTest.overlappingLockOnSameRegionThrowsWithinOneJvm` and `SingleInstanceTest.releasedLockCanBeReacquired` both pass. These pin the JDK semantics our code relies on; they do *not* exercise `acquireSingleInstanceOrSignalAndExit()` directly — that goes through the manual smoke test below, by design (a JVM-level integration test would require spawning a second JVM, which is heavyweight for what the manual test catches cheaply).
- Run the desktop app twice from a terminal (`./gradlew :desktop:run` in one terminal, then the same in another). The second process should exit cleanly (well under a second after JVM startup) without opening a window. The first remains running. No SQLite lock error in the second process's logs.
- Confirm `simplex.lock` exists in `dataDir` (Linux: `~/.local/share/simplex/`). It is empty.

**Commit.** `desktop: single-instance file lock`

---

### Task 2 — IPC listener on the first instance

**Files**
- Modify: `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/SingleInstance.desktop.kt`

**What to add.** When the lock acquisition succeeds, spin up a daemon thread that binds a `ServerSocket` on `127.0.0.1:0`, writes the chosen port to `simplex.port` atomically, and logs each received line. Acting on `SHOW` comes in Task 3 — for now the listener just proves the IPC plumbing works without invoking any UI code.

Add to `SingleInstance.desktop.kt`. Merge these `import` lines into the existing import block at the top of the file rather than duplicating; the same applies in Task 3.

```kotlin
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.InetAddress
import java.net.ServerSocket
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import kotlin.concurrent.thread

private const val LISTENER_THREAD_NAME = "simplex-single-instance"

private fun startSingleInstanceListener() {
  val server = try {
    ServerSocket(0, 0, InetAddress.getLoopbackAddress())
  } catch (e: IOException) {
    // Ephemeral port range starved (rare, observed behind some VPNs). Lock is
    // still held, so duplicate launches see the lock but no port to signal —
    // they retry and exit silently. Worst case: user clicks the tray icon.
    Log.w(TAG, "single-instance: ServerSocket bind failed: ${e.message}")
    return
  }
  writePortFile(server.localPort)
  thread(name = LISTENER_THREAD_NAME, isDaemon = true) {
    while (true) {
      val socket = try {
        server.accept()
      } catch (e: IOException) {
        Log.w(TAG, "single-instance: accept() failed: ${e.message}")
        return@thread
      }
      try {
        socket.soTimeout = 1000
        val line = BufferedReader(InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8))
          .readLine()
        Log.i(TAG, "single-instance: received $line")
        // SHOW handler comes in Task 3.
      } catch (e: IOException) {
        Log.w(TAG, "single-instance: read failed: ${e.message}")
      } finally {
        try { socket.close() } catch (_: IOException) {}
      }
    }
  }
}

private fun writePortFile(port: Int) {
  val portFile = dataDir.resolve("simplex.port").toPath()
  val tmp = dataDir.resolve("simplex.port.tmp").toPath()
  try {
    Files.writeString(tmp, port.toString(), StandardCharsets.UTF_8)
    try {
      Files.move(tmp, portFile, StandardCopyOption.ATOMIC_MOVE)
    } catch (e: java.nio.file.AtomicMoveNotSupportedException) {
      // Exotic filesystem. Fall back to plain move; the reader retries on
      // parse failure so a brief window of a half-written port file is fine.
      Files.move(tmp, portFile)
    }
  } catch (e: IOException) {
    Log.w(TAG, "single-instance: writing port file failed: ${e.message}")
  }
}
```

Add `Log.i` (it already exists in the codebase — check `DesktopTray.kt:42` for `Log.w`; `Log.i` is the same shape).

Then in `acquireSingleInstanceOrSignalAndExit()`, just before `return true`, call:

```kotlin
  lockChannel = channel
  lockHandle = held
  startSingleInstanceListener()
  return true
```

**Verify.**
- Build: `./gradlew :common:desktopMainClasses` — succeeds.
- Run desktop app once: `./gradlew :desktop:run`.
- Confirm `simplex.port` appears in `dataDir` and contains a port number (e.g. `54231`).
- From another terminal: `printf 'SHOW\n' | nc 127.0.0.1 $(cat ~/.local/share/simplex/simplex.port)` (Linux/macOS; Windows: use PowerShell `Test-NetConnection` or any TCP client).
- In the app's log output, see `single-instance: received SHOW`. App keeps running normally.
- Send garbage: `printf 'WHATEVER\n' | nc 127.0.0.1 $(cat ~/.local/share/simplex/simplex.port)` — log shows `received WHATEVER`. No crash.

**Commit.** `desktop: single-instance IPC listener`

---

### Task 3 — Second instance signals SHOW; listener restores window

**Files**
- Modify: `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/SingleInstance.desktop.kt`

**What to change.**

1. Have the listener act on `SHOW` by calling the existing `showWindow()` on the AWT EDT.

2. Have the second-instance path read `simplex.port` and send `SHOW\n`.

Replace the listener's "SHOW handler comes in Task 3" comment with:

```kotlin
// Only SHOW is recognised today. Future commands (e.g. open-URL) will extend
// this with new top-level if-branches; unknown commands are intentionally ignored.
if (line == "SHOW") {
  javax.swing.SwingUtilities.invokeLater { showWindow() }
}
```

In the early-exit branch of `acquireSingleInstanceOrSignalAndExit()` (where `held == null`), replace `channel.close(); return false` with `channel.close(); signalShowAndReturn(); return false`. Then add the signaller:

```kotlin
import java.net.InetSocketAddress
import java.net.Socket

private fun signalShowAndReturn() {
  val port = readPortWithRetry() ?: return
  try {
    Socket().use { sock ->
      sock.connect(InetSocketAddress(InetAddress.getLoopbackAddress(), port), 1000)
      sock.getOutputStream().write("SHOW\n".toByteArray(StandardCharsets.UTF_8))
      sock.getOutputStream().flush()
    }
  } catch (e: IOException) {
    // First instance is starting, shutting down, or stuck. Doing nothing is
    // strictly less harmful than spawning a duplicate that will fail on the
    // SQLite lock. The stale-port-after-crash case lands here too — handled.
    Log.w(TAG, "single-instance: SHOW signal failed: ${e.message}")
  }
}

private fun readPortWithRetry(): Int? {
  val portFile = dataDir.resolve("simplex.port").toPath()
  repeat(2) { attempt ->
    val raw = try {
      Files.readString(portFile, StandardCharsets.UTF_8).trim()
    } catch (e: IOException) {
      null
    }
    val parsed = raw?.toIntOrNull()
    if (parsed != null && parsed in 1..65535) return parsed
    // First-instance may still be writing the port during startup; one retry.
    if (attempt == 0) Thread.sleep(200)
  }
  return null
}
```

**Verify.**
- Build: `./gradlew :common:desktopMainClasses` — succeeds.
- Test: `./gradlew desktopTest` — Task 1's test still passes.
- Run the desktop app: `./gradlew :desktop:run`. Click X with "Minimize to tray" enabled → window hides (tray icon visible from the earlier tray work).
- From another terminal: launch the desktop app again (`./gradlew :desktop:run` in the same checkout). The second process exits within ~1 second; the first instance's window comes back to front and gains focus.
- Repeat with the window already visible (not minimized): second launch brings the window to front (already-visible windows still get `toFront()` + `requestFocus()`).
- Crash recovery test: kill the first instance with `kill -9 <pid>` (SIGKILL — no shutdown hooks). Launch again — the second process acquires the released lock and starts normally as the new primary (this proves the OS releases the lock on hard kill, as the spec assumes). With that new primary running, launch a third time → it signals `SHOW` and the new primary's window comes forward. The literal stale-port window (lock released, new primary started but has not yet rewritten `simplex.port`) is far narrower than one keystroke and not reliably reproducible by hand — the silent-exit-on-connect-failure path is exercised by inspection of the code path, not the smoke test.

**Manual smoke test pass on all three platforms** (per the spec's Tests section):

- **Linux** (KDE / GNOME with AppIndicator extension): run from terminal twice; second exits, first comes forward. Minimize to tray, run again, window restores from tray.
- **Windows 11**: same. Confirm Windows Defender doesn't prompt for the loopback `ServerSocket` bind. (It shouldn't, because we bind explicitly to `127.0.0.1`.)
- **macOS**: confirm `open -n /Applications/SimpleX.app` and direct `/Applications/SimpleX.app/Contents/MacOS/SimpleX` both deduplicate. Finder/Dock double-click was already deduplicated by LaunchServices before this change — confirm it still is.

**Commit.** `desktop: single-instance window restore via SHOW IPC`

---

### Task 4 — Audit fixes (post-implementation review)

After Tasks 1–3 landed, four parallel reviews (correctness/concurrency, security, edge-cases/integration, code-quality) ran against the branch. The genuine, cheap-to-fix findings were folded back in as a single follow-up commit. The non-issues and the more invasive suggestions (slow-loris handling, port-file ACL hardening, signal authentication, additional test coverage) were investigated and declined — see the commit message and the design spec for rationale.

**File**
- Rename `SingleInstance.desktop.kt` → `SingleInstance.kt` (the `.desktop.kt` suffix is the project's convention for `expect/actual` files under `platform/`, not for top-level desktop-only files; cf. `DesktopTray.kt`, `DesktopApp.kt`).

**Code changes**
- Drop redundant `lockChannel`. `FileLock` pins its own `FileChannel`, so a single reference to the `FileLock` keeps both alive against GC.
- `e.stackTraceToString()` in `Log.w` (matches `DesktopTray.kt:42`); top-of-file `SwingUtilities` import.
- Pin loopback bind and connect to explicit IPv4 `127.0.0.1` via `InetAddress.getByAddress(byteArrayOf(127, 0, 0, 1))`, instead of `InetAddress.getLoopbackAddress()`, which can return `::1` on dual-stack systems where IPv6 is preferred (and IPv6 loopback isn't always Defender-exempt on Windows).
- Replace `BufferedReader.readLine()` (unbounded) with a single bounded read of 256 bytes. Closes the OOM vector where a same-UID adversary could stream bytes without a newline.
- Delete any stale `simplex.port` at the top of the listener setup, before binding the new `ServerSocket`. Closes the race where a second instance arriving between our lock acquisition and our `writePortFile()` would read the old port and signal `SHOW` to whatever unrelated process now owns it.
- `OverlappingFileLockException` branch: close the channel and return `false` (fail-closed). Branch is unreachable by construction today, but the previous behaviour would silently break enforcement if the function were ever called twice.

**Verify**
- `./gradlew :common:desktopMainClasses :common:desktopTest` — green; both existing JDK-contract tests still pass.
- `./gradlew :desktop:assemble` — green.

**Commit.** `desktop: single-instance audit fixes`

---

### Task 5 — Round-2 audit fixes

A second parallel-review pass against the audited code found a few additional issues, fixed in one follow-up commit. The non-issues investigated this round (ATOMIC_MOVE missing REPLACE_EXISTING — empirically verified to replace, log-rate-limiting — pre-existing posture, function naming nits — subjective) were declined.

**Code changes**
- Replace fixed-name `simplex.port.tmp` with `Files.createTempFile(dataDir, "simplex.port.", ".tmp")` (`O_CREAT | O_EXCL` + randomized name). Closes a same-UID symlink-hijack vector where an attacker could pre-plant the fixed temp path as a symlink to e.g. `~/.bashrc`; our subsequent `Files.writeString` follows symlinks and would truncate the target.
- Wrap the port-file write in a `try/finally` that `deleteIfExists(tmp)` when the move never completes — closes an orphaned-`.tmp` leak on partial-failure paths.
- Skip the `SHOW` dispatch and the "received" log when `read() <= 0` (connect-then-close probes from same-UID processes).
- Fix stale `SingleInstance.desktop.kt` reference in the test file's header comment after the rename in Task 4.
- Refactor the test setup into a `withTempLockDir(block)` helper that explicitly walks-and-deletes the temp dir on each test exit. `File.deleteOnExit()` is a no-op for non-empty directories and was leaking a temp dir per test run.

**Verify**
- `./gradlew :common:desktopMainClasses :common:desktopTest :desktop:assemble` — green; tests still pass; `/tmp/simplex-singleinstance-test*` no longer leaks per run.

**Commit.** `desktop: single-instance round-2 audit fixes`

---

### Final pass

- Run the full test suite: `./gradlew desktopTest` — green.
- Confirm `git log --oneline` shows three commits since the spec commits, in the order Task 1 → Task 2 → Task 3.
- Confirm `simplex.lock` and `simplex.port` appear in `dataDir` after a normal run; nothing else.
- Sanity check that `closeBehavior` is still respected (the lock check runs before all the existing init, so it shouldn't interact with `closeBehavior` at all — but confirm by running through the tray plan's manual checks one more time).

If anything fails the smoke test on a specific platform, file a follow-up; the spec's "Failure modes" section enumerates the expected degraded behaviors and which are acceptable.
