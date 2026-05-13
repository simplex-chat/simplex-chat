# Desktop single instance — restore on duplicate launch

## What

Prevent a second SimpleX desktop process from running concurrently against the same data directory. When the user launches the executable a second time (e.g., clicks the launcher icon while the window is minimized to tray), the running instance restores its window instead of a duplicate spawning.

Three pieces:

1. **Lock acquisition** at process start, before any Haskell / DB init. If the lock is already held, the second invocation becomes a "signaller" that wakes the first and exits.
2. **IPC channel** — a loopback `ServerSocket` on `127.0.0.1` with a random ephemeral port written to a file alongside the lock. Used by the signaller to send a `SHOW` command.
3. **Window restore** — first instance reacts to `SHOW` by calling the existing `showWindow()` in `DesktopTray.kt`, which un-minimizes from the tray and brings the window to front.

Scope: Linux + Windows + macOS. Per-data-directory scope — users with different `XDG_DATA_HOME` settings or portable installs still run side-by-side because each install has its own `dataDir` and therefore its own lock.

## Why

After the tray work landed (#6970), the desktop app can be minimized to the tray. Users who minimize and later click the launcher icon expect the app to come back to focus — but currently a second process spawns. The second process then tries to open the chat databases and either crashes on the SQLite lock or runs in a degraded state, depending on timing.

A survey of comparable apps (Electron / VS Code / JetBrains / Telegram Desktop) confirms the chosen shape is canonical for desktop apps: file lock in the user data directory plus a loopback IPC socket. The differences across apps are details (named pipe vs TCP, port range scan vs ephemeral port). We chose loopback TCP with an ephemeral port and a side-band port file because:

- Cross-platform with no platform-specific branching.
- No firewall prompt on Windows Defender when bound explicitly to `127.0.0.1`.
- Survives ungraceful crash — the OS releases the file lock at process exit; a stale port file is harmless because the signaller's `connect()` fails and the signaller exits silently.
- One lock per data directory is automatic: the lock lives inside `dataDir`, which already differs per `XDG_DATA_HOME` / install.

We deliberately do **not** scan a fixed port range like JetBrains (6942–6991). That pattern is known to fail behind corporate VPNs and virtual NICs that starve the entire range, and produces the "IDE refuses to start" class of bug (YouTrack IJPL-94). Ephemeral port via `ServerSocket(0)` with a port file is strictly better.

We also deliberately do **not** pull in a library (`unique4j`, `JUnique`). The whole feature is ~80 lines of stdlib calls; the dep is not worth it.

### Out of scope

- Carrying file-open / `simplex:` URL arguments through the IPC. The protocol is line-based, so this extends cleanly later if we add deep-link or file-association support.
- macOS `Desktop.setOpenURIHandler` / `setOpenFileHandler` for deep links. Separate feature, not required for "no duplicate instance" — LaunchServices already prevents Finder/Dock duplication on macOS, and the lock catches `open -n` / direct binary launches.
- Cleaning up the port file on graceful shutdown — unnecessary, it is overwritten on the next start, and a stale value is handled by the signaller's failure path.

## How

### Lock and port files

Two files live directly in `dataDir`:

- `simplex.lock` — empty file. The running instance holds a `FileChannel` open and an exclusive `FileLock` on bytes `[0, 1)`. The OS releases the lock when the process exits (clean or crashed). The file itself is never deleted by the app.
- `simplex.port` — current loopback port, plain integer text. Rewritten atomically each time a new instance acquires the lock.

We use `tryLock(0L, 1L, false)`, **not** the zero-arg form. Per JDK-6674134, the zero-arg form locks `[0, Long.MAX_VALUE)` and is rejected by SMB/CIFS and some NFS implementations — locking a single byte is portable.

The `FileLock` is held in a module-level `var` (assigned once after acquisition) for the process lifetime. `FileLock` pins its `FileChannel` internally, so a single reference keeps both alive against GC.

### Entry point in `Main.kt`

```kotlin
fun main() {
  if (!acquireSingleInstanceOrSignalAndExit()) return
  initHaskell()
  runMigrations()
  setupUpdateChecker()
  initApp()
  tmpDir.deleteRecursively()
  tmpDir.mkdir()
  return showApp()
}
```

The single-instance check runs **before** `initHaskell()` and `runMigrations()`. Both touch the chat databases, which the first instance has already locked at the SQLite layer — without the early check we would hit an SQLite-level lock failure that's much harder to recover from.

### New file `SingleInstance.desktop.kt`

In `apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/`.

**`acquireSingleInstanceOrSignalAndExit(): Boolean`**

1. `dataDir.mkdirs()` — the directory is not guaranteed to exist before `initApp()`, and we need to create the lock file inside it.
2. Open `FileChannel` on `simplex.lock` with `READ`, `WRITE`, `CREATE`.
3. Call `channel.tryLock(0L, 1L, false)`.
4. If the lock is `null`: take the second-instance path (see below). Return `false` (caller exits `main`).
5. Lock held: stash `channel` and `lock` in module-level refs so they aren't GC'd. Start the listener thread. Return `true`.

**Listener thread (daemon, name `simplex-single-instance`)**

- Before binding, delete any stale `simplex.port` left by a previous primary. This closes the window where a second instance arriving between lock acquisition and `writePortFile()` could read an old port and signal `SHOW` to whatever process now owns it. A signaller arriving in that tiny window now sees no port file and exits silently.
- `ServerSocket(0, 0, LOOPBACK)` where `LOOPBACK` is an explicit IPv4 `127.0.0.1`. `InetAddress.getLoopbackAddress()` may return `::1` on dual-stack systems where IPv6 is preferred, and IPv6 loopback is not always Defender-exempt on Windows.
- Write the assigned port to `simplex.port` atomically: create a randomized-name temp file via `Files.createTempFile(dataDir, "simplex.port.", ".tmp")` (`O_CREAT | O_EXCL`, un-plantable as a symlink by a same-UID attacker), write the port number, then `Files.move(tmp, port, StandardCopyOption.ATOMIC_MOVE)`. Both files live in `dataDir` (same volume), so `ATOMIC_MOVE` is supported on all three platforms. If `AtomicMoveNotSupportedException` is ever observed (exotic filesystem), retry with `REPLACE_EXISTING` — the second-instance read path already retries on parse failure, so a brief window of a half-written port file is tolerable. The temp file is cleaned up in a `finally` if the move never completes.
- Loop on `accept()`. For each connection, set a 1 s read timeout (`socket.soTimeout = 1000`) so a misbehaving client can't hang the thread, then do a **single bounded read** of at most 256 bytes from the input stream and parse the first newline-terminated UTF-8 line. The bound is the OOM guard: `BufferedReader.readLine()` is unbounded and a same-UID adversary could otherwise stream gigabytes without a newline. Our own signaller writes the whole payload (`SHOW\n`) in one call, so a single bounded read covers the legitimate case. If the line equals `"SHOW"`, post `showWindow()` to the AWT EDT via `SwingUtilities.invokeLater`. Close the socket.
- Daemon thread exits with the JVM at process shutdown; the server socket closes with the JVM.

**Second-instance path**

1. Read `simplex.port`. If missing or unparseable, sleep 200 ms and retry once — the first instance may still be writing it during a startup race.
2. Construct `Socket()` and call `connect(InetSocketAddress("127.0.0.1", port), 1000)`. Using `Socket(host, port)` directly has no connect timeout; the explicit `connect(addr, timeout)` form is required.
3. Write `SHOW\n` (UTF-8), close.
4. Return from `main`. The JVM exits because no non-daemon threads are running.
5. **On any failure** (port file still missing after retry, connect timeout, `IOException`): log via `Log.w(TAG, …)` — same logger the rest of `desktopMain` uses (see `DesktopTray.kt:42`) — and exit silently. The first instance is starting up, shutting down, or in a stuck state; doing nothing is strictly less harmful than spawning a duplicate process that will then fail on the SQLite lock.

### Window restore handler

`showWindow()` at `DesktopTray.kt:50` already does exactly what we need:

```kotlin
fun showWindow() {
  simplexWindowState.windowVisible.value = true
  simplexWindowState.window?.toFront()
  simplexWindowState.window?.requestFocus()
}
```

Reuse it. The IPC thread is not the EDT, so the listener marshals via `SwingUtilities.invokeLater { showWindow() }`. No new public API.

### Failure modes

- **Stale `simplex.port` file after hard crash** — the OS releases the lock, but the port file persists pointing at a dead port. The next launch acquires the lock and overwrites the port file. The only victim is a signaller that hits the crash-restart window between "lock released" and "port file rewritten": it gets `ConnectException` and exits silently. Acceptable — a single missed signal during a crash-restart window is fine. A one-line code comment explains why we don't delete the port file on startup.
- **`tryLock()` throws `OverlappingFileLockException`** — same JVM trying to lock twice. Shouldn't happen by construction; if it does, log and treat as "we hold the lock" (return `true`). Cheaper than crashing.
- **Lock file on a filesystem that doesn't support file locks** — `tryLock` returns `null` or throws `IOException`. We treat this as "no other instance" and proceed without a lock. The user gets exactly the duplicate-instance behavior they have today, no worse. Log a warning.
- **`ServerSocket(0)` fails** (ephemeral port range starved — rare; observed behind certain VPNs). Log, skip the listener, proceed without IPC. The lock is still held, so a duplicate launch sees the lock held but no port to signal — the second instance retries the port file once, fails, exits silently. Worst-case behavior: the user must click the tray icon themselves. No data loss.
- **macOS Finder/Dock double-click** — LaunchServices dedupes by `CFBundleIdentifier` before our code ever runs. The lock fires only for terminal-driven launches (`open -n`, direct `Contents/MacOS/SimpleX`). Cost there is one `tryLock` call, negligible.

### Tests

Unit-level — `kotlin.test`, in `apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/SingleInstanceTest.kt`, matching the existing layout (`SemVerTest.kt` is the reference). Run via `./gradlew desktopTest`. Each test uses a fresh temp directory in place of `dataDir`.

- Acquire lock, second `tryLock` on the same path returns `null`. Release the first, second acquires.
- Write port to file atomically and re-read — round-trip integer parse.

Integration (Gradle-level, gated if we want it): spawn two short-lived JVM processes against the same temp dataDir. First binds, writes its port to `simplex.port`, listens, accepts one `SHOW`, exits. Second reads `simplex.port`, connects, sends `SHOW`, exits with status 0. Assert the first observed the payload. The port handoff goes through the file — same code path as production — not a test-only side channel.

Manual smoke test on all three platforms:

- Linux: run twice from a terminal → second exits, window comes forward.
- Linux with tray active and window minimized: same, window restores from tray.
- Windows: same.
- macOS: `open -n /Applications/SimpleX.app` and direct `Contents/MacOS/SimpleX` launches both deduplicate correctly.
