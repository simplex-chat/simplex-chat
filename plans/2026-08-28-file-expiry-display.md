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

Received files show the expiry; sent files are unchanged, they keep the checkmark. A tap still attempts the download: the receive actions, the download overlays, and the "Download file" menu item stay as they are.

Model:

- `CIFile` gets `fileExpires` — `Date?` in `SimpleXChat/ChatTypes.swift`, `Instant?` in `model/ChatModel.kt`
- `CIFile` gets `expired`, beside `loaded`: `fileExpires` is set and has passed

Message information, in `ChatItemInfoView`, a row after "Disappears at":

- "File can be received until <time>", or "File was available until <time>" when expired

File indicator, the `rcvInvitation` icon in each view — `CIFileView`, `CIImageView`, `CIVideoView`, `CIVoiceView` on both platforms:

- when expired, show the X that view already uses for `rcvError`, in place of the download arrow (`play.fill` for voice)
- `showStatusIconInSmallView` returns `expired` for `rcvInvitation`, so a small view shows the X too

Alert, in `showFileErrorAlert`:

- the function takes the expiry
- when the file expired and the error is `auth` or `noFile`, the alert reads "File expired" / "File was available until <time>", in place of "Wrong key or unknown file chunk address…" and "File not found…"

Strings:

- `strings.xml`: `info_row_file_expires`, `info_row_file_expired`, `file_expired`, `file_error_expired`
- iOS takes the literals in the source, `en.lproj/Localizable.strings` holds overrides only

Left out: promotion of badges for longer storage, and for the file size limit in the two "Large file!" alerts. The apps have no badge screen; the only badge link is "Learn more" in `showBadgeInfoAlert`.

An expired file that also exceeds the size limit for the sender's badge keeps the size behaviour: the icon stays the size warning, and the tap reports the size limit. The expiry applies only to files within the limit.
