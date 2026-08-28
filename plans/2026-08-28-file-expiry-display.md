# File expiry display

Show the sender and recipients when an XFTP file stops being downloadable.

The value is the storage expiry the server grants. The agent already reports it in `SFDONE` as `Maybe GrantedStorageTime` (`GSTExpires { epochSeconds }`, absolute UTC). No simplexmq change. `Nothing` means unknown (a server or chunk below the storage-time version).

## Protocol

In `Simplex.Chat.Protocol`:

- add optional `fileExpires :: Maybe UTCTime` to `XMsgFileDescr`
- encode with `"fileExpires" .=? fileExpires`, decode with `opt "fileExpires"`
- no chat version bump; an older app skips the field, an older sender omits it

## Chat item file

In `Simplex.Chat.Messages`:

- add `fileExpires :: Maybe UTCTime` to `CIFile`
- add no `CIFileStatus`; the app derives the expired state from `fileExpires < now`

## Store

In the SQLite and PostgreSQL stores:

- add a nullable `file_expires_at` column to `files`, typed as the other `files` timestamps; migration in both stores
- add `setFileExpires :: DB.Connection -> User -> FileTransferId -> UTCTime -> IO ()`
- read `file_expires_at` into `CIFile` in the file-row queries

## Sender

In `Simplex.Chat.Library.Subscriber`, `SFDONE` handler:

- bind the granted time (currently dropped)
- convert `GSTExpires epochSeconds` to `UTCTime` and store it with `setFileExpires`
- thread the expiry through `sendFileDescriptions` into each `XMsgFileDescr` it builds

## Recipient

In `Simplex.Chat.Library.Subscriber`, `XMsgFileDescr` handling (direct and group):

- take the received `fileExpires` and store it with `setFileExpires`

## Apps

- show "File can be received until …" from `fileExpires`, for the sender and the recipients
- past `fileExpires`, keep the plain X; a tap still attempts the download
- a download that finds no file shows an "expired" alert, not "no such file", for XFTP files
- prompt a badge for longer storage where it fits: the short default window, and failed downloads
