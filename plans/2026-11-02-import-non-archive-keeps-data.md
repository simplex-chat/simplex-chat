# Importing a non-archive file keeps the chat data

## Problem

Settings → Chat data → Database passphrase & export → Import database, with a file that is not a chat archive, deletes every profile, contact and message, and the database backups. On desktop and Android the "Error importing chat database" alert is not shown. When the chat was running, the app restarts it and "Error starting chat" is shown; when it was stopped, no alert is shown and the progress indicator stays on.

## Cause

- The apps call `apiDeleteStorage` (`/_db delete`) before `apiImportArchive` (`/_db import`):
  - Kotlin `importArchive` in `DatabaseView.kt`;
  - Kotlin `importArchive` in `MigrateToDevice.kt`;
  - iOS `DatabaseView.importArchive`;
  - iOS `MigrateToDevice.importArchive`.
- `deleteStorage` (`Archive.hs`) removes both databases, their `.bak` and `.exported` copies, the files folder, the assets folder and the temp folder.
- `importArchive` opens the archive only afterwards, so a file that is not a zip fails after the data is gone.
- Kotlin `DatabaseView.importArchive` catches `Error`, while `apiImportArchive` throws `Exception`: the alert is skipped, and when the chat was running, `stopChatRunBlockStartChat` tries to start it, which fails because the core marked the store as changed.

## Fix

- `ArchiveConfig` has a new optional field `replaceStorage`.
- Core `importArchive`:
  1. unpacks the archive into a new temporary directory (under `parentTempDirectory`, else the system temp directory);
  2. fails with `not a chat archive: no <file>` when `simplex_v1_chat.db` or `simplex_v1_agent.db` is missing;
  3. closes the stores;
  4. with `replaceStorage = true`, runs `removeStorage` (the removal steps of `deleteStorage`, now shared); otherwise backs up the databases as before;
  5. copies the archive contents.
- The four app call sites pass `replaceStorage = true` and no longer call `apiDeleteStorage`; the wallpapers folder is created after the import instead of after the deletion.
- Kotlin `DatabaseView.importArchive` catches `Exception` and shows "Error importing chat database"; its "Error deleting chat database" branch is removed with the deletion. iOS `DatabaseView.importArchive` loses the same branch.
- `/_db import` without `replaceStorage` (CLI) keeps its behaviour: backup, then overwrite.

## Scope

- Core: `Archive.hs`, `Controller.hs` (`ArchiveConfig`), `Commands.hs` (`ExportArchive`).
- Kotlin: `DatabaseView.kt`, `MigrateToDevice.kt`, `SimpleXAPI.kt` (`ArchiveConfig`).
- iOS: `DatabaseView.swift`, `MigrateToDevice.swift`, `AppAPITypes.swift` (`ArchiveConfig`).
- Specs: `apps/ios/spec/database.md`, `apps/multiplatform/spec/api.md`, `apps/multiplatform/spec/database.md`.
- `MigrateToAppGroupView.swift` imports without `replaceStorage`, as before.
- Test: `testImportNonArchiveKeepsData` in `tests/ChatTests/Direct.hs`.
