# Migration: stop the temporary chat controller (Android, desktop)

## Problem

After the migration archive is imported on the new device, `error agent INTERNAL ... SQLite3 returned ErrorReadOnly while attempting to perform step: attempt to write a readonly database` appears in the chat console every 10 seconds until the app restarts, and the app keeps the deleted temporary database files of the migration open.

## Cause

The new device downloads the archive with a temporary chat controller. Its agent is active from the creation of the controller and saves server statistics to its database every 10 seconds. The migration code sends `/_stop` only to the main controller.
- Successful import: the controller stays open, and `finishMigration` deletes the directory of its database files. Each statistics save then fails with `SQLITE_READONLY_DBMOVED`, and the error is added to the chat console by the receiver loop, which is still running.
- `stopAndCleanUp` runs on back and on a download failure. `initTemporaryDatabase` (used by both migration screens) calls `chatCloseStore` when the start returns no user, and leaves the controller open when the start throws. Both close the database without stopping the controller, and each statistics save then fails with "database closed".
- The receiver is kept in `remember`. When Android recreates the activity during the download, the new composition holds `null`, and the controller and the receiver loop stay active.
- On "Open chat", the app checks the passphrase by opening the imported database with `chatInitTemporaryDatabase` and discards the controller, so a second agent stays active on the main database.

## Fix

- `stopChatWithTemporaryDatabase(ctrl)` in `SimpleXAPI.kt` sends `/_stop` to the controller (`apiStopChat(ctrl)`) and closes its database; a failed `/_stop` is logged.
- New device: `stopAndCleanUp` of the receiver calls it and becomes `suspend`. `ArchiveImportView` calls `stopAndCleanUp` before `importArchive`. The receiver is kept in the `ModalData` of the screen (`stateGetOrPutNullable`).
- `initTemporaryDatabase` calls it when the start returns no user or throws, then deletes the temporary database files.
- The passphrase check calls it when the result is `DBMigrationResult.OK`.

## Scope

Android and desktop: `MigrateToDevice.kt`, `initTemporaryDatabase` in `MigrateFromDevice.kt`, and `SimpleXAPI.kt`. The receiver of the old device, "Cancel migration" and "Finalize migration" are unchanged.
