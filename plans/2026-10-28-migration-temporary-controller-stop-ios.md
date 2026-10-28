# Migration: stop the temporary chat controller (iOS)

## Problem

After a device migration on iOS, the agent of the temporary chat controller stays active with its network clients until the app restarts, and each of its statistics saves fails every 10 seconds.

## Cause

A migration uses a temporary chat controller (archive download on the new device, upload on the old device). Its agent is active from the creation of the controller and saves server statistics every 10 seconds; `/_stop` is never sent to the controller. `stopAndCleanUp` of `MigrationChatReceiver` closes its database with `chat_close_store`, which closes the database only, so each statistics save of the still active agent fails with "database closed". When the start of the controller fails (`initTemporaryDatabase`), the controller is neither stopped nor closed.

## Fix

- `stopChatWithTemporaryDatabase(ctrl:)` in `SimpleXAPI.swift` sends `/_stop` to the controller (`apiStopChat(ctrl:)`) and closes its database with `chat_close_store`; a failed `/_stop` is logged.
- `stopAndCleanUp` of both migration receivers calls it and becomes `async`; the two `.onAppear` call sites start it in a `Task`.
- `initTemporaryDatabase` of both screens calls it when the start fails and becomes `async`.

## Scope

iOS: `MigrateToDevice.swift`, `MigrateFromDevice.swift`, and `SimpleXAPI.swift` (`apiStopChat` with an optional controller parameter, as in `apiStartChat`, and `stopChatWithTemporaryDatabase` next to `startChatWithTemporaryDatabase`). The main chat controller is unchanged.
