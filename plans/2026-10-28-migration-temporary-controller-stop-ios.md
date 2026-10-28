# Migration: stop the temporary chat controller (iOS)

## Problem

After a device migration is imported on iOS, the agent of the temporary chat controller stays active until the app restarts, and each of its statistics saves fails every 10 seconds.

## Cause

The new device downloads the archive with a temporary chat controller. Its agent is active from the creation of the controller and saves server statistics every 10 seconds; `/_stop` is never sent to the controller. `stopAndCleanUp` of `MigrationChatReceiver` runs when the screen disappears, after `finishMigration` deletes the directory of the database files, and closes the database with `chat_close_store` without stopping the controller. When the start of a temporary controller fails (`initTemporaryDatabase`), the controller is neither stopped nor closed.

## Fix

- `stopChatWithTemporaryDatabase(ctrl:)` in `SimpleXAPI.swift` sends `/_stop` to the controller (`apiStopChat(ctrl:)`) and closes its database; a failed `/_stop` is logged.
- New device: `stopAndCleanUp` of the receiver calls it and becomes `async`; the archive import view stops the download controller before the import.
- `initTemporaryDatabase` of both screens calls it when the start fails and becomes `async`.

## Scope

iOS: `MigrateToDevice.swift`, `initTemporaryDatabase` in `MigrateFromDevice.swift`, and `SimpleXAPI.swift`. The receiver of the old device is unchanged: `/_stop` cancels the remote deletion of the uploaded archive that its exits start.
