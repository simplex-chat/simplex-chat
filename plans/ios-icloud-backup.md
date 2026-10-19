# iOS: allow iCloud backup of chat data when the database passphrase is set

## Problem

Since #7623 the iOS app sets `isExcludedFromBackup` on the app group container and the Documents directory on every launch, so chat data is never in iCloud (or computer) backups. Users who set their own database passphrase cannot back up their chats at all.

## Cause

Backup exclusion was unconditional. It is only required when the database is not encrypted, or is encrypted with the initial random passphrase: that passphrase is in a `kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly` keychain item, so the backup could not be opened on another device. With a passphrase set by the user the backup is encrypted and can be opened after a restore by entering it.

An unencrypted database with a non-random passphrase flag is possible: archives exported by the terminal CLI (unencrypted by default) and imported on iOS, and databases from before encryption was added. So the rule checks encryption, not only the passphrase flag.

## Fix

"Chat backup" section in Settings → Chat data → Database passphrase & export, between "Chat database" and "Run chat", with the "Enable iCloud backup" toggle (`DEFAULT_ICLOUD_BACKUP`, off by default; the choice persists).

The app group container is included in backup when the toggle is on and `iCloudBackupBlock` returns `nil`. Otherwise the toggle is disabled and shown off, and the footer states the reason:

| Reason | Footer |
|---|---|
| the database is still in Documents | Database is not migrated yet. Migrate it when the app restarts. |
| the database is not encrypted | Database is not encrypted. Set passphrase to enable iCloud backup. |
| the database uses the initial random passphrase | Database is encrypted using a random passphrase. Set passphrase to enable iCloud backup. |

`excludeAppDataFromBackup(_:)` always excludes the Documents directory (legacy database, exported archives, migration files), `temp_files` (decrypted copies of files) and the `.bak` copies of the database (made by the core on passphrase changes and migrations, possibly unencrypted or with an old passphrase). It clears the attribute on the database files, because restoring a backup in `DatabaseErrorView` copies them from the excluded `.bak` files with `FileManager.copyItem`, which keeps the attribute.

The rule is applied when the database is opened (after the file paths are set, as the core re-creates `temp_files`), after a passphrase change, when the toggle changes, and at launch, before the database is opened. Launch never includes the app data, because encryption is only known after opening (a passphrase stored in the keychain is saved before it is verified, e.g. after restoring a backup): it excludes it when the toggle is off, the database is in Documents or uses the random passphrase, or the passphrase is stored in the keychain without a key (an unencrypted database — importing an archive removes the key); otherwise it keeps the decision applied at the last open. This is skipped while protected data is unavailable (the device is locked). Importing a database excludes the app data before the old database is removed, until the new one is opened. Deleting a database needs no exclusion: nothing of it remains, and the new database is excluded when it is opened (random passphrase) or at launch (no key is stored). The toggle is disabled while an import or deletion is in progress (`progressIndicator`), and after it until the chat is started again, as it would be based on the old database. "Delete temp data" (Developer → Storage) re-applies the exclusion to the `temp_files` directory it re-creates.

## Accepted gaps

- `.bak` copies made during a passphrase change or migration, and `temp_files` re-created by the NSE or SE, are excluded at the next update.
- Files in `app_files` are backed up as stored; they are unencrypted when "Encrypt local files" is off.
- `/db decrypt` in the developer console (it needs the chat stopped) removes the database encryption without updating `chatDbEncrypted` or the keychain, so it can leave the container included while the database is unencrypted, and the toggle can still include it in that session. The launch rule does not detect it (the keychain still holds the old passphrase, or the passphrase is not stored), and opening the database with the old passphrase fails before the rule is applied, so the container is excluded only when the app next opens the database successfully.

## Verification

Not built yet: needs an Xcode build and a device check of the toggle states and of `isExcludedFromBackup` on the container, Documents, `temp_files`, the database files and `.bak` copies.
