# Desktop single instance - restore on duplicate launch

## Problem

After tray support (#6970), the desktop app can minimize to tray. The process stays alive holding the database. When the user clicks the app launcher again (forgetting about the tray), a second process starts and either crashes on the SQLite lock or runs in a degraded state.

## Design

Two files in `dataDir`: `simplex.started` (lock file) and `simplex.show` (signal file).

### Startup

1. Try `FileChannel.tryLock(0, 1, false)` on `simplex.started`.
2. **Lock acquired**: delete stale `simplex.show` if present (leftover from crash), start a daemon `WatchService` on `dataDir` for `ENTRY_CREATE`, start the app normally.
3. **Lock taken** (another process holds it): create `simplex.show`, exit. The running instance detects it and shows its window.
4. **Lock fails** (IOException, filesystem doesn't support locks, etc.): start normally but disable minimize-to-tray. Close quits the app. No worse than before tray support existed.

### Signal handling

While the lock is held, the daemon watcher runs for the JVM lifetime. When `simplex.show` appears it deletes the file and posts `showWindow()` to the EDT. `showWindow()` sets `windowVisible = true`, clears `ICONIFIED`, and brings the window to front — restores from tray, from taskbar-minimize, or just raises if visible-but-behind.

Minimize-to-tray is only available when `singleInstanceLock` is held. If the lock couldn't be acquired (case 4), close always quits - preventing the scenario where two tray'd instances fight over the database.

### Crash recovery

The OS releases the file lock when the process dies. `simplex.show` may be left behind but is harmless - the next startup (step 2) deletes it.

## Scope

Linux, Windows, macOS. Per-data-directory - separate installs with different `dataDir` run independently.
