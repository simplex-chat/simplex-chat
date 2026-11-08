# Fix Windows uninstall and update deleting everything in the chosen install folder

## Problem

The Windows MSI installer has a folder selection page. When the selected folder already contains
the user's files, for example Desktop, uninstalling SimpleX deletes that folder recursively, as
far as it can: for the Desktop, the user's files and subfolders, and the Desktop folder itself.
Updating has the same result, because an update first removes the installed version.

Reproduced with the released MSIs on a GitHub Windows runner, with the installer UI driven
through UI Automation (fork runs 39472341425, 39473484103 and 39492828153):

- Desktop contained `My notes.txt`, `Tax return 2025.txt` and `Holiday photos\beach.txt`.
- `C:\Users\runneradmin\Desktop\` was entered on the folder page. The installer showed "The
  folder C:\Users\runneradmin\Desktop\ already exist. Would you like to install to that folder
  anyway?", and choosing Yes continued the installation.
- Uninstalling v7.0.3 removed `C:\Users\runneradmin\Desktop`; Explorer reported
  "C:\Users\runneradmin\Desktop is unavailable". The uninstall log contains `Removing foreign
  folder: C:\Users\runneradmin\Desktop\Holiday photos\`.
- Updating v7.0.2 installed in Desktop to v7.0.3 through the installer UI, as the in-app updater
  does (`msiexec /i`), removed `C:\Users\runneradmin\Desktop` in the same way, during
  `RemoveExistingProducts`.
- Uninstalling v7.0.3 installed in the user profile folder: the `WixRemoveFoldersEx` recursion
  collected 189 of the profile's folders (in `.cargo`, `.docker`, `.dotnet`, `.rustup`,
  `.templateengine` and `AppData\Local\.IdentityService`), and in run 39492828153 also the
  user's `.my-settings`, before it failed with access denied at the junction
  `AppData\Local\Application Data`. Windows Installer ignores errors from that custom action, so
  `RemoveFiles` then deleted the files in those folders and the folders that became empty;
  `AppData\Roaming` was not reached.
- Uninstalling v7.0.3 installed in `%AppData%` (`C:\Users\runneradmin\AppData\Roaming\`)
  removed every folder in `%AppData%` except `Microsoft`, including `%AppData%\SimpleX`, where the
  app stores its database and settings (`Platform.desktop.kt`), and the user's file there. In
  `Microsoft` it removed 71 files, including the DPAPI master keys in `Microsoft\Protect`, a key
  file in `Microsoft\Crypto\RSA` and Start Menu shortcuts; the folder itself remained.

## Cause

The MSI is built by jpackage through the Compose `packageMsi` task, with `dirChooser = true` in
`apps/multiplatform/desktop/build.gradle.kts`. For `INSTALLDIR`, jpackage adds a WiX
`util:RemoveFolderEx On="uninstall"` element (`WixAppImageFragmentBuilder.addDirectoryCleaner` in
JDK 17, which CI uses, and in the JDK `master` branch as of October 2026). WiX compiles this
element into these MSI entries:

- a `WixRemoveFolderEx` row (`InstallMode` 2, uninstall) for property `RM_RF…`;
- a `Registry` row, from which Windows Installer writes `[INSTALLDIR]` to
  `HKLM\Software\Unknown\SimpleX\<version>\RM_RF…` during installation, and
  `AppSearch`/`RegLocator` rows, from which the `AppSearch` action reads it back at the start of
  every installer run, including removal;
- the WiX `WixRemoveFoldersEx` custom action, which schedules every file and folder under that
  path for removal.

The folder page sets `INSTALLDIR` to the selected folder without appending a product subfolder.

Windows Installer caches the MSI of every installed version and removes that version with its
cached tables, both on uninstall and when a later version removes it during an update.

## Fix

As the last action (`doLast`) of every Compose MSI packaging task (`packageMsi`,
`packageReleaseMsi`), `scripts/desktop/disable-msi-install-dir-wipe-windows.ps1` deletes the rows
of the `WixRemoveFolderEx` table through the Windows Installer automation interface
(`WindowsInstaller.Installer`). Removing a version installed from such an MSI, by uninstalling or
updating, deletes only the files that version installed, and the folders that become empty when
those files are removed: each file belongs to an MSI component, and Windows Installer removes
component files itself, independently of `RemoveFolderEx`.

- With WiX 3.11.2, the version Compose downloads, the `WixRemoveFoldersEx` custom action
  schedules nothing when the `WixRemoveFolderEx` table is empty (`RemoveFoldersEx.cpp`: the
  record loop ends with `E_NOMOREITEMS`, which the function maps to `S_OK`).
- The `Registry`, `AppSearch` and `RegLocator` rows and the custom action stay: the registry
  value is the key path of a jpackage component, and without `WixRemoveFolderEx` rows nothing
  uses the `RM_RF…` property.
- The hook is added only to `TargetFormat.Msi` tasks, which Compose enables only on Windows.
- If a future jpackage no longer emits `util:RemoveFolderEx`, the MSI has no `WixRemoveFolderEx`
  table, the query fails, and the build fails. With WiX 4 or later, WiX compiles the element into
  the `Wix4RemoveFolderEx` table, so the script then has to target that table.
- The script calls the `WindowsInstaller.Installer` methods through `InvokeMember`, which worked in
  Windows PowerShell 5.1 (the hook and the patch step of the tests) and in PowerShell 7 (the test
  scenarios). A direct `OpenDatabase` call was tested only in PowerShell 7, where it works.
- The hook runs the script with `Project.exec`, as `apps/multiplatform/android/build.gradle.kts`
  does. `Project.exec` is deprecated since Gradle 8.11; the project uses Gradle 8.12.

## Alternatives considered

| Alternative | Why not |
|---|---|
| `dirChooser = false` | Removes the folder choice for everyone and does not change silent installs with `INSTALLDIR=`. |
| Append a `SimpleX\` subfolder to the selected folder (MSI `ControlEvent` rows) | Keeps the recursive delete: an existing unrelated folder named `SimpleX`, or a folder passed as `INSTALLDIR=` on the command line, is still deleted. A larger change, and a change to the installer UI. |
| Override jpackage resources (`--resource-dir`) | Compose 1.8.2 clears its resource directory before running jpackage and passes its own `--resource-dir` after `freeArgs`; jpackage uses the last one. Not possible until JetBrains/compose-multiplatform#5612. |
| Run jpackage a second time with Compose's arguments file and another resource directory | Doubles the MSI build time and depends on Compose's internal arguments file. |
| Replace Compose's resource directory through reflection | Compose clears that directory too; depends on Compose internals. |
| A step in `.github/workflows/build.yml` after `packageMsi` | Shorter, but local builds and `packageReleaseMsi` would still produce MSIs with the recursive delete. |
| Remove `WixRemoveFoldersEx` from `InstallExecuteSequence` instead of deleting the table rows | A `DELETE` that matches no row succeeds, so if the action name changes (for example with WiX 4) the build would not detect that the recursive delete remains. |
| The script as an inline `-Command` in the Gradle file | Every PowerShell `$` would need escaping in the Kotlin string. |
| Patch or vendor the Compose Gradle plugin, or change installer tooling | A far larger change than deleting the table rows. |

### Protecting existing installations during an update

An installed earlier version is removed with its own cached tables, including its
`WixRemoveFolderEx` row. These options were evaluated and not taken:

| Option | Why not |
|---|---|
| Delete the earlier versions' `RM_RF…` registry values in an elevated deferred custom action before `RemoveExistingProducts` | Fails with error 2613 (`RemoveExistingProducts action sequenced incorrectly`, fork run 39474876911): only deferred actions can run without impersonation, deferred actions must follow `InstallInitialize`, and after `InstallInitialize`, `RemoveExistingProducts` must either precede every action that generates execution script or follow `InstallExecute` (the next row). |
| The same, with `RemoveExistingProducts` after `InstallExecute` | Changes every update: new files are installed before the old version is removed, so a bundled file with a lower version than the installed one is not replaced, and updating from a Desktop installation would leave the old files there (not tested; most component GUIDs are the same in every version, because jpackage derives them from the install path). |
| Stop the update when the earlier version's install folder contains other files | Leaves the user to move the files; adds a PowerShell custom action and an `Error` table entry. |
| A check in the app before it starts an update | The update to this version is started by the updater of the installed version, which does not contain the check. |

## Scope

- Linux `.deb`: the maintainer scripts contain no recursive delete, and the package installs to
  the fixed `/opt/simplex`.
- Linux AppImage in-app update: replaces only the running AppImage file, through a staging file
  next to it. macOS in-app update:
  replaces or removes only `/Applications/SimpleX.app` and `/Applications/SimpleX-old.app`.
- `TargetFormat.Exe`: the EXE contains an MSI that jpackage builds in the same task; the hook is
  not added to that task. The EXE is not built in CI or released, but
  `packageDistributionForCurrentOS`, the documented desktop build command, builds it on Windows,
  and the MSI inside that EXE still contains the `WixRemoveFolderEx` row.
- Not fixed: when Windows Installer removes a version installed from an earlier release MSI, on
  uninstall or during an update, it uses that version's cached `WixRemoveFolderEx` row and
  deletes the install folder. Users who installed into a folder with other files need to move
  those files before uninstalling or updating.

## Testing

The workflow `windows-msi-gui-test.yml` with `apps/multiplatform/scripts/windows-msi/` (in
draft PR #7723, not merged; the runs below used the same scripts on a fork) drives the installer
UI on a Windows runner and records screenshots, installer logs and check results. Expected
results with the fix:

| Scenario | Expected |
|---|---|
| Install into Desktop, uninstall | The user's files and Desktop remain; `SimpleX.exe`, `app` and `runtime` are removed. |
| Install the fixed version into Desktop, update to a copy of the same MSI with a higher version (the test script changes the product version, product code, package code, `Upgrade` rows and versioned registry key) | The user's files remain; the old files are removed; the updated app starts. |
| Install into the default folder, start the app, uninstall | The app starts; `C:\Program Files\SimpleX` is removed after uninstall. |
| Install into the user profile folder, start the app, uninstall | The user's files in the profile remain. |
| Install into `%AppData%`, start the app, uninstall | The user's files in `%AppData%` and every file in `%AppData%\SimpleX`, including the database, remain. |
| Install v7.0.2 into Desktop, update to the fixed version | Known issue: the user's files are deleted, because Windows Installer removes v7.0.2 with v7.0.2's cached `WixRemoveFolderEx` row. |

Results (fork runs):

- Before, released MSIs (39492828153): 17 checks failed. Deleted: the user's files in the Desktop
  uninstall and update scenarios, `.my-settings` in the profile folder, and the user's file and
  folder and the SimpleX database in `%AppData%` (`Adobe`, `Mozilla` and `NuGet` were also
  removed, and 71 files in `Microsoft`, including the DPAPI master keys); the default-folder
  scenario passed.
- After, MSI built by `packageMsi` from this branch (build 39495576670, test 39570009651):
  `WixRemoveFolderEx` has 0 rows, no uninstall or update of the fixed version logged `Removing
  foreign folder`, and only the three checks of the known-issue scenario (last table row) failed.
  The app started after each update and after installing into the default folder, the user
  profile folder and `%AppData%`.
