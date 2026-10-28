# Migration: stop the temporary chat controller (iOS)

## Problem

After a device migration on iOS, the agent of the temporary chat controller stays active with its network clients until the app restarts, and each of its statistics saves fails every 10 seconds.

## Cause

The temporary controller of a migration (archive download on the new device, upload on the old device) is started with `/_start`, and `/_stop` is never sent to it. `stopAndCleanUp` of `MigrationChatReceiver` closes its database with `chat_close_store`, which closes the database only, so each statistics save of the still active agent fails with "database closed".

## Fix

`stopAndCleanUp` of both migration receivers sends `/_stop` to the temporary controller (`apiStopChat(ctrl:)`) before `chat_close_store`. A failed `/_stop` is logged, and the database is closed. `stopAndCleanUp` becomes `async`; the two `.onAppear` call sites start it in a `Task`.

## Scope

iOS: `MigrateToDevice.swift`, `MigrateFromDevice.swift`, and `apiStopChat` in `SimpleXAPI.swift` (an optional controller parameter, as in `apiStartChat`). The main chat controller is unchanged.
