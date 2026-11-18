# Migration: stop the temporary chat controller (iOS)

## Problem

After the migration archive is imported on an iOS device, the agent of the temporary chat controller stays active until the app restarts, and a statistics save fails every 10 seconds.

## Cause

The new device downloads the archive with a temporary chat controller. Its agent is active from the creation of the controller and saves server statistics every 10 seconds. The migration code sends `/_stop` only to the main controller.
- `stopAndCleanUp` of `MigrationChatReceiver` runs on a download failure and when the screen disappears; after a successful import that is after `finishMigration` deletes the directory of the database files. It closes the database with `chat_close_store` without stopping the controller.
- When the start of a temporary controller (`initTemporaryDatabase`) returns no user or throws, the controller stays open and active.
- On "Open chat", the app checks the passphrase by opening the imported database with `chatInitTemporaryDatabase` and discards the controller, so a second agent stays active on the main database.

## Fix

- `stopChatWithTemporaryDatabase(ctrl:)` in `SimpleXAPI.swift` sends `/_stop` to the controller (`apiStopChat(ctrl:)`) and closes its database; a failed `/_stop` is logged.
- New device: `stopAndCleanUp` of the receiver calls it and becomes `async`. `archiveImportView` calls `stopAndCleanUp` before `importArchive`.
- `initTemporaryDatabase` of both screens calls it when the start returns no user or throws, and becomes `async`.
- The passphrase check calls it when a controller is returned.

## Scope

iOS: `MigrateToDevice.swift`, `initTemporaryDatabase` in `MigrateFromDevice.swift`, and `SimpleXAPI.swift`. The receiver of the old device, "Cancel migration" and "Finalize migration" are unchanged.
