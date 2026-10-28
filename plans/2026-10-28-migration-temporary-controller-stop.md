# Migration: stop the temporary chat controller (Android, desktop)

## Problem

After a device migration is imported, the chat console of the new device shows `error agent INTERNAL ... SQLite3 returned ErrorReadOnly while attempting to perform step: attempt to write a readonly database` every 10 seconds until the app restarts. The app keeps the temporary database files of the migration open after they are deleted, and the agent of the temporary controller stays active with its network clients.

## Cause

A migration uses a temporary chat controller: on the new device to download the archive, on the old device to upload it. Its agent is active from the creation of the controller and saves server statistics to its database every 10 seconds; `/_stop` is never sent to the controller.
- New device, successful import: the controller is not closed; `finishMigration` deletes the directory of its database files, and each statistics save fails with `SQLITE_READONLY_DBMOVED`, shown by the receiver loop of the controller, which also keeps running.
- Old device, "Cancel migration": the screen is closed by `closeModals()`, which skips `cleanUpOnBack`, so the controller is not closed.
- Back, download or upload failure and "Finalize migration" call `stopAndCleanUp`, and a failed start of the controller (`initTemporaryDatabase`) calls `chatCloseStore`: both close the database of the controller without stopping it, and each statistics save then fails with "database closed". When the start throws, the controller is not closed.

## Fix

- `stopChatWithTemporaryDatabase(ctrl)` in `SimpleXAPI.kt` sends `/_stop` to the controller (`apiStopChat(ctrl)`) and closes its database; a failed `/_stop` is logged. `stopAndCleanUp` of both migration receivers and the failed start in `initTemporaryDatabase` (also when the start throws) call it.
- New device: the download controller is stopped when the archive is downloaded, before the import (`ArchiveImportView`).
- Old device: "Cancel migration" stops the upload controller.

## Scope

Android and desktop: `MigrateToDevice.kt`, `MigrateFromDevice.kt`, and `SimpleXAPI.kt` (`apiStopChat` with an optional controller parameter, as in `apiStartChat`, and `stopChatWithTemporaryDatabase` next to `startChatWithTemporaryDatabase`). The main chat controller is unchanged. iOS is changed in a separate commit.
