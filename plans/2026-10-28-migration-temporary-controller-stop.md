# Migration: stop the temporary chat controller (Android, desktop)

## Problem

After a device migration is imported, the chat console of the new device shows `error agent INTERNAL ... SQLite3 returned ErrorReadOnly while attempting to perform step: attempt to write a readonly database` every 10 seconds until the app restarts, and the deleted temporary database files of the migration stay open.

## Cause

The new device downloads the archive with a temporary chat controller. Its agent is active from the creation of the controller and saves server statistics to its database every 10 seconds; `/_stop` is never sent to the controller.
- Successful import: the controller is not closed; `finishMigration` deletes the directory of its database files, and each statistics save fails with `SQLITE_READONLY_DBMOVED`, shown by the receiver loop of the controller, which also keeps running.
- Back and download failure call `stopAndCleanUp`, and a failed start of the controller (`initTemporaryDatabase`, used by both migration screens) calls `chatCloseStore`: both close the database without stopping the controller, and each statistics save then fails with "database closed". When the start throws, the controller is not closed.
- The receiver is kept in `remember`; an activity recreation during the download drops it, so nothing is stopped afterwards.

## Fix

- `stopChatWithTemporaryDatabase(ctrl)` in `SimpleXAPI.kt` sends `/_stop` to the controller (`apiStopChat(ctrl)`) and closes its database; a failed `/_stop` is logged.
- New device: `stopAndCleanUp` of the receiver calls it; the download controller is stopped when the archive is downloaded, before the import (`ArchiveImportView`); the receiver is kept in the `ModalData` of the screen (`stateGetOrPutNullable`).
- `initTemporaryDatabase` calls it when the start fails, also when the start throws, and then deletes the temporary database files.

## Scope

Android and desktop: `MigrateToDevice.kt`, `initTemporaryDatabase` in `MigrateFromDevice.kt`, and `SimpleXAPI.kt`. The receiver of the old device, "Cancel migration" and "Finalize migration" are unchanged: `/_stop` cancels the remote deletion of the uploaded archive that these exits start.
