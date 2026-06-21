# Desktop In-App Updater Fixes

## Problem Statement

The desktop in-app updater (`apps/multiplatform/common/src/desktopMain/kotlin/chat/simplex/common/views/helpers/AppUpdater.kt`) silently or visibly fails on three of the four supported desktop platforms:

1. **Windows**: no update dialog ever appears for any Windows user, regardless of how out-of-date the running version is.
2. **AppImage (x86_64)**: the update dialog appears and the download succeeds, but clicking "Install update" opens the new AppImage in an archive viewer ("Archive format not recognized") instead of installing it. The running AppImage is never replaced.
3. **AppImage (aarch64)**: no update dialog ever appears for any aarch64 AppImage user.

The desktop installer flow on macOS and the `.deb` flow on Debian-derivative Linux are not affected and remain unchanged.

## Root Causes

### 1. Windows — `which dpkg` IOException swallowed

`chooseGitHubReleaseAssets` (AppUpdater.kt) invokes `Runtime.getRuntime().exec("which dpkg")` unconditionally to detect Debian-derivative systems. On Windows there is no `which.exe`; `CreateProcess` returns error 2 and the JVM throws `IOException: Cannot run program "which"` synchronously from `Runtime.exec`. The exception propagates up through `chooseGitHubReleaseAssets` into `checkForUpdate`'s outer `try { ... } catch (e: Exception) { Log.e(...) }`, which logs to stderr and returns. The user-facing alert is never built.

The `.deb` probe was correct in intent but executed too eagerly: it has no business running on a non-Linux platform.

### 2. AppImage — `xdg-open` is the wrong operation

The Linux branch of `installAppUpdate` calls `xdg-open <downloaded.AppImage>`. An AppImage is not "installable" in the package-manager sense — it is a self-contained executable that lives at the path stored in the `$APPIMAGE` environment variable (set by the AppImage runtime). On most desktop environments, `xdg-open` resolves the `.AppImage` MIME type to an archive handler (file-roller, ark, engrampa). The handler attempts to read the AppImage as a `.iso`/squashfs archive and fails with "Archive format not recognized". Even when it succeeds, it does not replace the running AppImage — the next launch still runs the old binary.

The existing code had no awareness of `$APPIMAGE` at install time. The `GitHubAsset.isAppImage` field hints at an earlier abandoned attempt at AppImage-specific handling.

### 3. aarch64 AppImage — leading space in asset name

`Platform.desktop.kt` declares:

```kotlin
LINUX_AARCH64("so", unixConfigPath, unixDataPath, " simplex-desktop-aarch64.AppImage"),
```

The `githubAssetName` literal has a leading space character. The actual release asset published by `.github/workflows/build.yml` is `simplex-desktop-aarch64.AppImage` (no space — verified against the live GitHub releases API). The exact-name filter in `chooseGitHubReleaseAssets` (`release.assets.filter { it.name == desktopPlatform.githubAssetName }`) never matches, the asset list is empty, and `checkForUpdate` returns at the "No assets to download for current system" branch without ever showing a dialog. Same silent-failure pattern as the Windows bug, single arch in blast radius.

## Solution Summary

Three small, independent commits — one per root cause. None of them changes shared logic; each touches one line (Windows, aarch64) or one branch of the install dispatch (AppImage).

### Fix 1 — Gate the `dpkg` probe on Linux

```kotlin
// AppUpdater.kt: chooseGitHubReleaseAssets
} else if (desktopPlatform.isLinux() && !isRunningFromAppImage()
           && Runtime.getRuntime().exec("which dpkg").onExit().join().exitValue() == 0) {
```

Single conjunct (`desktopPlatform.isLinux() &&`) added at the start of the `else if`. Boolean short-circuit ensures `Runtime.exec` is never reached on non-Linux. The added gate matches the actual semantic intent: `.deb` is a Linux-only package format. Both the Windows IOException and the (theoretical) macOS misbehavior of the `which` probe are eliminated.

### Fix 2 — AppImage-aware install path

In `installAppUpdate`'s Linux branch, read `System.getenv("APPIMAGE")`:

- If non-null, the running app is an AppImage at that path. Replacing it crash-safely takes two steps, because an atomic file replacement is only possible *within a single filesystem* (POSIX `rename(2)`), and the download lives in the temp dir — usually a different filesystem (tmpfs) from where `$APPIMAGE` lives:
  1. `Files.copy` the downloaded file to a staging file (`.<name>.update`) in the target's *own* directory. This is the unavoidable cross-filesystem transfer; it is not atomic, but it writes only a sidecar, never the live `$APPIMAGE`.
  2. Mark the staging file executable, then `Files.move` it onto `$APPIMAGE` with `ATOMIC_MOVE`. Because staging now shares the target's filesystem, this is a real atomic `rename(2)`: the live file flips from old to new in one indivisible step, never partially written.

  A direct `Files.move(downloaded, target, REPLACE_EXISTING)` is **not** sufficient — across filesystems it copies bytes straight onto the live `$APPIMAGE`, which is neither atomic nor crash-safe (an interrupted copy destroys the user's installed app). `ATOMIC_MOVE` on a cross-filesystem move throws `AtomicMoveNotSupportedException`. Staging on the target's filesystem first is what makes the atomic move possible. On Linux the kernel keeps the running process's open file descriptors valid across the rename: the running app continues to function until the user restarts, at which point the new binary is used.
- If null, fall back to the existing `xdg-open` path (used for `.deb` install on Debian, which is the only remaining caller of this path after Fix 2).

On any exception (permission denied if the AppImage lives in `/opt/`, target read-only, etc.) the catch deletes the staging file and falls back to `desktopOpenDir(file.parentFile)` — the same fallback the original `xdg-open` path used.

### Fix 3 — Remove leading space from `LINUX_AARCH64` asset name

```kotlin
LINUX_AARCH64("so", unixConfigPath, unixDataPath, "simplex-desktop-aarch64.AppImage"),
```

Single character removed. The asset name now matches what `make-appimage-linux.sh` produces and what GitHub releases publish.

## Why three commits, not one

Each fix has a different blast radius, a different fix size, and (potentially) a different review path. Three focused commits let a reviewer judge each one in isolation:

- Windows fix: 1 line, gates a side-effecting `Runtime.exec` on a platform check that the surrounding code already establishes.
- AppImage install: ~35 lines, introduces new file-system operations (`Files.copy` to a staging file, then `Files.move` with `ATOMIC_MOVE`).
- aarch64 fix: 1 character, fixes a typo in a string literal.

Bundling them as a single commit would force a reviewer to verify all three at once and would obscure `git blame` on the AppImage install logic, which is the only one of the three that introduces meaningful new behavior.

## Out of scope

The following were identified during the audit (`apps/multiplatform/app-updater-audit.md`) but deliberately deferred to keep this PR focused:

- `msiexec /i ${file.absolutePath}` uses the single-string `Runtime.exec` overload that tokenizes on whitespace; paths containing spaces (uncommon on Windows but possible) break the install.
- Download failures (network, TLS, disk-full, GitHub error) are caught but only logged; the user sees nothing.
- `process.children().count() > 0` in the Linux `xdg-open` path is racy and arguably wrong.
- No SHA256 / signature verification on the downloaded artifact — the updater installs whatever GitHub serves.
- 24h delay with no retry / backoff on transient network errors.
- macOS install hardcodes `/Applications/SimpleX.app`.

Each is documented with `file:line` references in the audit; none affects the three platforms this PR fixes.

## Test plan

- **Windows**: built x86_64 MSI via the fork CI workflow [`build-windows-msi.yml`](https://github.com/Narasimha-sc/simplex-chat/actions/runs/25958413517), installed in a Windows VM as version 6.5.1 (intentionally lowered to trigger the check against current stable 6.5.2). Settings → Check for updates → Stable: dialog appeared as expected.
- **AppImage x86_64**: built locally (host build, GHC 9.6.3, gradle createDistributable, appimagetool), installed and ran on Linux. Settings → Check for updates → Stable: dialog appeared, Download landed file at `/tmp/simplex/simplex-desktop-x86_64.AppImage`, Install replaced `$APPIMAGE` in place. Verified by hashing `$APPIMAGE` before and after.
- **aarch64 AppImage**: not separately tested. Fix is a 1-character literal change verified against the live GitHub releases API (`simplex-desktop-aarch64.AppImage`, no leading space).
- **macOS**: no changes to the macOS install branch.
