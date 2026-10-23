# Fix: Windows desktop fails to launch after in-app MSI update

Issue: https://github.com/simplex-chat/simplex-chat/issues/7105

## Symptom

After updating SimpleX Desktop on Windows via the in-app updater (MSI), the MSI reports
success but `SimpleX.exe` never launches afterwards — no window, no process in Task Manager,
and it persists across a reboot. No application crash is logged. Reported on Windows 11
(6.5.4 → 7.0.0), and the reporter saw it on a prior update too.

Event Viewer shows the cause: RestartManager 10010 (`SimpleX.exe … cannot be restarted -
Application SID does not match Conductor SID`) followed by MsiInstaller 1038
(`requires a system restart … Reason for Restart: 1` = files in use).

## Root cause

The Windows branch of `installAppUpdate` (`AppUpdater.kt`) ran the MSI **while the app was
still running** and waited for it (`Runtime.getRuntime().exec("msiexec /i …").onExit().join()`).
The desktop MSI is a per-machine major upgrade (`desktop/build.gradle.kts`:
`perUserInstall = false`, fixed `upgradeUuid`), so it must replace `SimpleX.exe`, the bundled
JRE and the app DLLs in `C:\Program Files\SimpleX`. The running JVM holds those files open, so
the installer cannot replace them: Restart Manager can't close the app across the
user/elevated security boundary (the "SID does not match" event), the locked files are
deferred to `PendingFileRenameOperations`, and a reboot is requested. At reboot the deferred
uninstall-then-install operations leave the install directory inconsistent, so the executable
never starts.

macOS and Linux are unaffected because those kernels allow a running executable's file to be
replaced by inode; Windows locks open files, so the app must release its locks before the MSI
replaces them.

## Fix

Launch the installer and **exit the app immediately** instead of waiting, so the files are
unlocked before the MSI reaches its file-replacement phase (which happens only after UAC
elevation and the installer's costing phase — well after the JVM is gone). With no running
instance there are no locked files, no reboot deferral and no Restart Manager conflict.

Two small changes, both confined to the Windows path:

1. `installAppUpdate` Windows branch: replace the `msiexec … .onExit().join()` + result
   handling with `Runtime.getRuntime().exec(arrayOf("msiexec", "/i", file.absolutePath))`
   followed by `exitProcess(0)`. The array form also fixes a latent bug where a space in the
   path broke the single-string `exec`.
2. `downloadAsset`: before downloading, `if (desktopPlatform.isWindows()) File(tmpDir,
   asset.name).delete()`. Because the app now exits before it can delete the installer, a
   leftover MSI is removed at the next download instead of accumulating in the temp dir.

The user reopens the updated app from the Start Menu (the MSI recreates the shortcut) — the
same hand-off the updater already uses for the `.deb` path and all failure branches.

## Alternatives considered

- **Helper script that waits for exit, installs, then relaunches** — adds a generated batch
  with PID-polling, relaunch and self-delete; its only benefit over exiting is auto-relaunch,
  which is not worth the complexity and platform-specific fragility.
- **Register the app with Restart Manager** so the elevated MSI can close/restart it —
  jpackage apps don't register RM, the SID mismatch shows RM can't manage the process across
  the elevation boundary anyway, and it is far more code.
- **Keep the app running with `REBOOT=ReallySuppress` / repair flags** — does not address the
  lock; in-use files still defer to a reboot.

## Scope / out of scope

- In scope: the Windows branch of `installAppUpdate` and a Windows-guarded cleanup in
  `downloadAsset`. macOS, Linux AppImage and `.deb` paths are unchanged.
- Trade-off accepted: the app no longer shows its own "installed successfully" dialog (it has
  exited); msiexec shows its own progress UI and the relaunched app is the success signal.
- Out of scope (separate follow-ups): the macOS install branch ignores `renameTo` results;
  download failures are only logged with no user-facing error; no signature/SHA verification
  of the downloaded artifact.

## Test plan (Windows VM)

1. Install a per-machine MSI built at a version below the latest release.
2. Settings → Check for updates → download + Install the newer MSI.
3. Before the fix: RestartManager 10010 + MsiInstaller 1038 (Reason 1) appear and the app
   fails to launch after reboot (reproduces #7105).
4. After the fix: the app exits, the MSI installs with no reboot request, and the new version
   launches normally from the Start Menu; `C:\Program Files\SimpleX` is a complete install
   with no queued `PendingFileRenameOperations`.
5. Re-run with a user profile path containing a space to confirm the array-form `exec` fix.
