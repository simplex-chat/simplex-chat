# Migration: reject an archive too large to upload, warn about low storage before exporting

Part 1 of #7696.

## Problem

1. **Archive too large.** XFTP uploads are limited to 5 GB (`maxFileSizeHard`, checked by the core and the agent before uploading). Neither app checks before uploading: the user waits for the whole export, then gets "Error uploading the archive" (on Android and desktop with an opaque `API$Error` text) and a "Repeat upload" button that can never succeed (#7696, a 5.62 GB profile).
2. **Low storage.** Exporting or migrating needs about twice the data size free (for migration, until the upload finishes): the export copies the databases and files, then archives the copy. Nothing checks free space first, so on a nearly full device the export fails or fills the disk.

## Change

- **Size check** (Android/desktop `MigrateFromDevice.kt`, iOS `MigrateFromDevice.swift`), in two places, both showing "Database is too large" with the size and the limit and returning to the upload confirmation:
  - before archiving: if the estimated data size (the app files and wallpapers folders plus both databases, the same estimate as the storage check) is over `MAX_FILE_SIZE_XFTP_HARD` (5 GB), migration stops without exporting;
  - right after archiving (before the "exported with errors" alert, if any): an archive over the limit is deleted. This catches archives that end up over the limit although the estimate was not.
  - `MAX_FILE_SIZE_XFTP_HARD` is a separate constant from `MAX_FILE_SIZE_XFTP_LEGEND` (also 5 GB today): one is the hard limit for any XFTP upload (the migration archive is a standalone upload), the other a badge tier for sending files in chats, which can change.
- **Storage check** before "Export database" and before archiving for migration: the size of the data (the app files and wallpapers folders plus both databases) is compared with free space; if short, "You may not have enough storage" shows the estimate and what is available, with Continue / Cancel. It is advisory: Continue proceeds as before.
  - Android/desktop (`confirmExportStorage` in `DatabaseView.kt`): the export copies the data to the export folder while it writes the archive to the target folder (the export folder itself for "Export database", `migration_temp_files` in the data folder for migration), and a migration upload then writes an encrypted copy of the archive to that same folder. So the target folder needs twice the data size free and the export folder the data size; on desktop the two can be on different volumes (the temp folder vs the data folder).
  - iOS: one data volume, twice the data size in "available capacity for important usage". The estimate counts the files' content size (top-level files, as Android/desktop do), as the upload limit does.
  - The export progress is shown before the check, so the export button is disabled while the files folder is measured. On iOS the warning is one of each screen's own alerts, and Continue starts the export from it, so nothing waits on it if the screen is closed. If the migration screen is closed while the warning is shown (e.g. by SimpleX Lock), the chat stays stopped, as when it is closed during archiving on master.

## Alternatives considered

- **Check only after archiving** (the first revision): a user over the limit waits for the whole export (minutes for 5+ GB) and needs up to twice the data in temporary space before being told.
- **Warn before exporting, with Continue**: keeps migration possible when the archive would compress under 5 GB — with "Encrypt local files" off (or files saved before it existed), compressible files can make the archive smaller than the estimate (the database itself is always encrypted by then: migration requires a passphrase). Refusing was chosen instead, so that a user over the limit does not wait for an export that will be rejected.
- **Check in the core**: upload already rejects an oversized file with a typed `CEFileSize`, but only after the whole export (Android and desktop show it as `API$Error@…`); checking it in export would change a shared API, and the storage check needs a free-space call the core does not have.
- **Split the archive across several XFTP files**: changes the migration link format and the receiving side; a feature, not a fix.

Refusing before the export can block a migration whose archive would have been under 5 GB (e.g. unencrypted files that compress well); the limit then has to be met by deleting files first.

## Known edges

- Android/desktop: the storage warning is awaited (as `showStartChatAfterRestartAlert` is); if all alerts are cleared while it is shown (e.g. an agent error, switching the remote host), migration stays on "Archiving" until the app restarts, and "Export database" stays disabled until the screen is reopened.

These exist on master during the export itself; the warning makes the effect last longer (until the user answers):
- Android: a configuration change that recreates the activity while the migration warning is shown (font size, locale, split-screen resize; rotation only where the portrait lock is ignored) restores the "Archiving" screen, which runs the export again, and a second warning is shown on top of the first; each answer acts on its own (Cancel on the first one answered and Continue on the second runs the export behind "Archive and upload").
- Android/desktop: if the chat was running, a race between stopping it and starting the export can clear the export progress, leaving "Export database" enabled until the export ends.

## Not changed

- Cancel in the "Export database" warning leaves the chat stopped, as it is after a completed export (the export flow never restarts the chat).
- The receiving device ("Migrate here") and "Import database" have no free-space check; separate change.
- #7696 parts 2 and 3 (an exported archive that fails to import, Windows hanging on "Stopping chat") are different bugs.

## Testing

Desktop, master vs this PR, built from the same base (iOS reviewed by reading only, no macOS here):
- Oversized data (6 random 940 MiB files, 5.51 GiB): master fails at the start of the upload with "Error uploading the archive" (`API$Error@…`), keeps the 5.51 GiB archive, and "Repeat upload" fails again; the final PR build shows "Database is too large" with the size and the limit within a second of "Archive and upload", without archiving and without leaving an archive, also on a second try. An earlier revision with only the after-archiving check (kept as a safety net) showed the alert after archiving and deleted the archive.
Earlier revision (same storage-check code; the data size calculation was later moved into a helper):
- Low storage (temp folder on a 1 GB tmpfs; a sparse file of ≈0.6 GB added to the files folder): "Export database" — master exports without a warning, PR warns (need ≈1.2× free); Cancel and clicking outside close it with the buttons enabled again, Continue exports a valid archive. Migration — the temp folder has room for the data copy, so neither build warns and both reach the link screen; with a 1.1× sparse file the PR warns and Cancel returns to "Archive and upload".
- Regression (both builds): export and migration with enough space show no warning; "Cancel migration" behaves as before. PR build only: import of an exported archive and the delete alert.
