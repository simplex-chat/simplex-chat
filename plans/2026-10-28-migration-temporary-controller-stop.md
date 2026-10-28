# Migration: stop the temporary chat controller (Android, desktop)

## Problem

After a device migration is imported, the chat console of the new device shows `error agent INTERNAL ... SQLite3 returned ErrorReadOnly while attempting to perform step: attempt to write a readonly database` every 10 seconds until the app restarts. The app keeps the temporary database files of the migration open after they are deleted, and the agent of the temporary controller stays active with its network clients.

## Cause

A migration uses a temporary chat controller: on the new device to download the archive, on the old device to upload it. The controller is started with `/_start`, and `/_stop` is never sent to it, so its agent stays active and saves server statistics to its database every 10 seconds.
- New device, successful import: the controller is not closed; `finishMigration` deletes the directory of its database files, and each statistics save fails with `SQLITE_READONLY_DBMOVED`, shown by the receiver loop of the controller, which also keeps running.
- Old device, "Cancel migration": the screen is closed by `closeModals()`, which skips `cleanUpOnBack`, so the controller is not closed.
- Other exits call `stopAndCleanUp`, which closes the database of the controller without stopping it; each statistics save then fails with "database closed".

## Fix

- `stopAndCleanUp` of both migration receivers sends `/_stop` to the temporary controller (`apiStopChat(ctrl)`) before closing its database. A failed `/_stop` is logged, and the database is closed.
- New device: the download controller is stopped when the archive is downloaded, before the import (`ArchiveImportView`).
- Old device: "Cancel migration" stops the upload controller.

## Scope

Android and desktop: `MigrateToDevice.kt`, `MigrateFromDevice.kt`, and `apiStopChat` in `SimpleXAPI.kt` (an optional controller parameter, as in `apiStartChat`). The main chat controller is unchanged. iOS is changed in a separate commit.
