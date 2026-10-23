# Windows installer GUI tests

These tests install, update, start and uninstall the desktop app through the real MSI installer
UI on a GitHub-hosted Windows runner. `msi-scenarios.ps1` drives the installer dialogs and
Explorer through UI Automation (`wingui.psm1`), takes screenshots, keeps the installer logs, and
asserts what is on disk after each step. There is no local Windows harness: the tests run only
in the `windows-msi-gui-test` workflow, which is started manually.

## Running

Start the workflow from the Actions tab or with `gh`:

```sh
gh workflow run windows-msi-gui-test.yml -R <owner>/simplex-chat --ref <branch with the workflow> \
  -f scenario=all -f label="what this run checks" \
  -f new_msi_run_id=<run id of a workflow that uploaded the MSI as an artifact>
```

| Input | Meaning |
|---|---|
| `scenario` | One scenario, `all` (every scenario except known issues) or `everything`. |
| `label` | Free text shown in the run name. |
| `new_msi_run_id`, `new_msi_artifact` | Run id and artifact name of the MSI under test. |
| `new_msi_url` | URL of the MSI under test, used when `new_msi_run_id` is empty (default: the v7.0.3 release). |
| `old_msi_url` | MSI installed before the update in `update-desktop` (default: the v7.0.2 release). |
| `patch_with_ref` | Branch whose `scripts/desktop/disable-msi-install-dir-wipe-windows.ps1` is applied to the MSI under test before the scenarios. Checks an MSI change in about 10 minutes without a Windows build. |

The run uploads the artifact `windows-gui-evidence`:

- `checks.txt`: one line per check, `name = actual (expected X) PASS|FAIL`, plus informational lines;
- `captions.tsv` and numbered PNG screenshots (1024x768);
- `logs/`: verbose `msiexec` logs (UTF-16) for every install and uninstall.

The job fails if any check fails or a dialog does not appear in time. A missing dialog is
recorded as an `Unexpected state` screenshot.

`gh run view --log` can return an empty log; read it with
`gh api repos/<owner>/simplex-chat/actions/jobs/<job id>/logs`.

## Scenarios

| Scenario | Steps | Expected |
|---|---|---|
| `uninstall-desktop` | Put user files on the Desktop, install into `Desktop\` (the installer warns that the folder exists), uninstall | User files and Desktop remain; `SimpleX.exe`, `app`, `runtime` are removed |
| `update-same-build` | Install into `Desktop\`, update through the installer UI to a copy of the same MSI with a higher version | User files remain; old files removed; the updated app starts |
| `default-folder` | Install into `C:\Program Files\SimpleX\`, start the app, uninstall | The app starts; the folder is removed |
| `profile-folder` | Install into the user profile folder, start the app, add user files there, uninstall | User files and every file in `%AppData%\SimpleX` remain |
| `appdata-folder` | The same with `%AppData%\` as the install folder | As above |
| `update-desktop` (known issue) | Install the old MSI into `Desktop\`, update to the MSI under test | User files remain. Fails while the old MSI deletes its install folder on removal |

The update scenarios update the way the in-app updater does: `msiexec /i` with the full UI and
the default folder. The higher-version copy for `update-same-build` is made by the script: it
changes the product version, product code, package code, `Upgrade` rows and the versioned
registry key of the MSI under test.

## Notes

- Before a fix, `profile-folder` and `appdata-folder` delete files of the runner's user profile;
  they run last.
- The runner user is an administrator, so the installer does not show a UAC prompt.
- Windows Installer keeps the MSI open after a COM call until the COM objects are released;
  `Close-MsiHandles` forces that before the file is opened again.
