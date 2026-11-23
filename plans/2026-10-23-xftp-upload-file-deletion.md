# Agent file deletion after XFTP uploads

On iOS, with simplexmq PR #2010, the agent completes suspension only after the agent file of each upload started in foreground is deleted by the chat or TTL cleanup, or when the chat is stopped, or at the suspend timeout. Files with rows absent or flagged deleted or failed at the start of the suspension are not awaited. On master, the chat skipped this deletion on several paths of `SFDONE` and `SFERR`, handling errors among them.
The deletions in the branches of `processAgentMsgSndFile` are replaced by one deletion after the handling of each `SFDONE` or `SFERR` with a file ID, also when an error is thrown. On `SFDONE` of an undeleted sent item with a shared message ID, the file descriptions are queued before it in the direct and group branches.
The iOS app holds a background task for each XFTP file it sends, from the send until the upload ends, so that iOS suspends the app later while the upload runs.

Agent side: https://github.com/simplex-chat/simplexmq/pull/2010

## Decisions

- Terminal events:
  - `SFDONE`;
  - `SFERR`.
- Deletion: `xftpDeleteSndFileInternal`, once per terminal event with a file ID, after its handling, also when an error is thrown.

  ```mermaid
  sequenceDiagram
    participant Agent
    participant Chat
    Agent->>Chat: SFDONE or SFERR with a file ID
    Note over Chat: contact or group lock, file lock
    Chat->>Agent: file descriptions queued, on SFDONE of an undeleted sent item
    Note over Chat: locks released, also after an error
    Chat->>Agent: xftpDeleteSndFileInternal
  ```

- The deletion is executed after the contact or group lock and the file lock are released.
- On `SFDONE` of an undeleted sent item with a shared message ID, file descriptions are queued before the deletion in the direct and group branches.
- `SFPROG` and `SFWARN`: the agent file is deleted only when the user is missing, as on master.
- `SFERR` with an empty entity ID: the agent error is sent to the view; the file lookup and the deletion are skipped.
- Last file event of each branch, sent before the deletion:

  | Branch | Event |
  |---|---|
  | item missing | `CEvtSndFileRedirectStartXFTP`, `CEvtSndStandaloneFileComplete` or `CEvtSndFileError` |
  | direct | `CEvtSndFileProgressXFTP` |
  | group | `CEvtSndFileCompleteXFTP` |
  | `SFERR` | `CEvtSndFileError` |

- The deletion is added to these paths of master `99b91696f`:
  - the agent file is unknown to the chat (:225);
  - file cancelled: the deletion by `agentXFTPDeleteSndFilesRemote` at cancellation is repeated;
  - `SFDONE` with fewer descriptions than transfers (:270);
  - failed steps:
    - the user lookup (:227);
    - `getSndFileTransfer` (:240): the file row deleted after the user lookup, or a database error;
    - `SFDONE`: the private description update or the item lookup (:251-252);
    - direct:
      - `contactSendConn_` (:275): contact not ready, inactive or disabled;
      - the chat binding (:277);
      - the extra descriptions and badge proof write (:281-283);
      - queueing (:284);
      - the delivery ID update (:287);
    - group:
      - the recipients lookup (:294);
      - the extra descriptions and badge proof write (:303-305);
      - queueing (:306-307);
      - the item update (:308-310);
    - `SFERR`: the status update (:360-362);
  - other chat type or direction, or a direct sent item without transfers (:322; `sft : _` is required by the direct branch at :274);
  - item deleted or without a shared message ID (:323).
- iOS spec, the same text in each of two files: "While the chat is suspending, the agent continues the XFTP uploads started in foreground by this app process. The chat deletes the agent file of each upload after handling its completion or error, queueing the file descriptions of a completed upload first. The core sends `chatSuspended` after these deletions and the delivery of the queued messages, or at the suspend timeout. Uploads started while suspending or resumed from an earlier process are not awaited."
- App, upload background tasks:
  - one task per XFTP file of a sent item, in direct and group chats, keyed by file ID;
  - begun on the response to a send or a forward;
  - ended once and removed on the first of:

    | Ending | Source |
    |---|---|
    | `sndFileCompleteXFTP` | event; direct: after the file description is sent; group: after the file descriptions are queued |
    | `sndFileError` | event |
    | `sndFileCancelled` | response to `/fcancel` |
    | `chatItemsDeleted` with the item | response to an item deletion, or event |
    | expiration | iOS |

  - message delivery tasks: unchanged; groups keep `msgDelay`.

  ```mermaid
  sequenceDiagram
    participant App
    participant Core
    participant iOS
    App->>Core: send or forward with an XFTP file
    Core-->>App: newChatItems
    App->>iOS: beginBackgroundTask, stored in fileUploads by file ID
    Core-->>App: sndFileCompleteXFTP, sndFileError, sndFileCancelled or chatItemsDeleted
    App->>iOS: endBackgroundTask, removed from fileUploads
    iOS-->>App: or expiration: the same end
  ```

- `cabal.project`: simplexmq pinned to commit `bdda2fa4` of the simplexmq fork during review.
- After the simplexmq pull request in simplex-chat/simplexmq is merged and before the final pull request is opened in simplex-chat/simplex-chat:
  - the pin is replaced by the merged simplex-chat/simplexmq commit;
  - `scripts/nix/sha256map.nix` is updated.

## Mechanics

- `src/Simplex/Chat/Library/Subscriber.hs`, `processAgentMsgSndFile`:
  - clause for `SFERR` with an empty entity ID (:224-225): `eToView $ chatErrorAgent e`;
  - `flip allFinally (when sndFileEnded deleteAgentFile)` around the handling (:226);
  - missing user (:232): `unless sndFileEnded deleteAgentFile`;
  - `sndFileEnded :: Bool` (:235-239): `SFDONE` or `SFERR`;
  - `deleteAgentFile :: CM ()` (:240-241);
  - removed, the deletions of master `99b91696f`:
    - :255, item missing;
    - :291, direct;
    - :311, group;
    - :363, `sendFileError`.
- `apps/ios/spec/architecture.md` (:137), "Background Task Protection": the text.
- `apps/ios/spec/services/files.md` (:274), "Sending": the text.
- `apps/ios/Shared/Model/ChatModel.swift`, `fileUploads` (:470).
- `apps/ios/Shared/Model/SimpleXAPI.swift`:
  - `processSendMessageCmd`: `beginFileUploadTasks(cItems)` in the direct (:584) and group (:598) branches;
  - new: `beginFileUploadTasks` (:606-615), `endFileUploadTask` (:617-620), `endFileUploadTasks` (:622-627);
  - `apiDeleteChatItems` (:689), `apiDeleteMemberChatItems` (:698), `apiCancelFile` (:1768);
  - `processReceivedMsg`: `chatItemsDeleted` (:2809), `sndFileCompleteXFTP` (:3042), `sndFileError` (:3048).
- `cabal.project` (:23-24):
  - `location: https://github.com/evgeny-simplex/simplexmq.git`;
  - `tag: bdda2fa40960f7c433138adda00ecfedad0c93f5`.

## Tests

`tests/ChatTests/Files.hs`, "file transfer over XFTP":
- `checkNoAgentSndFiles` (:1171-1173): the count of agent `snd_files` rows with `deleted = 0` eventually equals 0.
- "send and receive file" (:703): `checkNoAgentSndFiles alice` after the direct transfer (:732).
- "send and receive file in group" (:789): `checkNoAgentSndFiles alice` after the group transfer (:826).
- "delete agent file after upload error" (:1078):
  - XFTP server with `newFileBasicAuth = Just "abcd"`; the client's server password `server_password` differs;
  - alice: "error uploading file 1 (test.pdf) for bob";
  - `checkNoAgentSndFiles alice`.
- "delete agent file when contact is deleted during upload" (:1093):
  - XFTP server down at the send;
  - bob deletes the contact with notification (`/_delete @2 full notify=on`);
  - server started: alice "bob: not ready";
  - `checkNoAgentSndFiles alice` (:1106).
  - On master, a test failure is expected: the deletion at master :291 is skipped after the error at :275, and the agent row is kept.
- "delete agent file when item is marked deleted during upload" (:1108):
  - XFTP server down;
  - alice marks the item deleted for everyone;
  - the agent row is marked `deleted = 1` by the cancellation (`src/Simplex/Chat/Library/Internal.hs:649, 511, 3213`);
  - the agent's prepare worker finds the row marked deleted and sends `SFERR` (simplexmq `src/Simplex/FileTransfer/Agent.hs:510-511, 516-521`); the agent file is deleted after its handling;
  - the count of all agent `snd_files` rows eventually equals 0 (:1121).
  - On master, a test failure is expected when the check starts within 10 s of the agent start: the deletions are skipped for cancelled files, the marked row is deleted only by the agent cleanup 30 s after the agent starts, and the check polls for 20 s.
- "delete agent file when file user is missing during upload" (:1123):
  - XFTP server down at the send of a 17 MB file;
  - the user's contact profile is unset, so the file user is not found;
  - server started: the agent row is eventually marked `deleted = 1`, by the deletion on the first `SFPROG`.
- "complete upload before chat is suspended" (:1137):
  - XFTP server down at the send;
  - alice: `/_app suspend 60000000`;
  - server started: alice "completed uploading file 1 (test.pdf) for bob", then "chat suspended";
  - `checkNoAgentSndFiles alice`;
  - bob receives the file.
- "show agent file error without file ID" (:1161):
  - agent cleanup 2 s after the agent starts;
  - the agent column `snd_files.prefix_path` renamed, so the three cleanup steps of sent files fail;
  - alice: three lines "internal error: …", from `SFERR` without a file ID.

`tests/ChatTests/Files.hs`, "file transfer over XFTP without chat items":
- "send and receive small standalone file" (:1404): `checkNoAgentSndFiles src` after the upload (:1418).
- "send and receive large standalone file" (:1460): `checkNoAgentSndFiles src` after the upload and its redirect upload (:1477).

`tests/ChatTests/Files.hs`, "record upload: …", one recording in `tests/fixtures/api/files` each:
- "send text", "send file", "send group file": the responses to a direct text send (`send-text`), a direct file send (`send-file`) and a group file send (`send-group-file`).
- With file 1 sent while the XFTP server is down, and its item received by bob (`withSndFileEvents`):
  - "complete": `sndFileCompleteXFTP` after the XFTP server starts (`upload-complete`);
  - "error": `sndFileError` from an XFTP server with another password (`upload-error`);
  - "cancel": the response to `/fcancel 1` (`cancel`);
  - "delete": the response to the internal deletion of the item (`delete`).

`apps/ios/SimpleX Unit Tests/FileUploadTests.swift`, `sendSimpleXCmdStr` replaced by the recorded response:

| Test | Path | Asserted `fileUploads` keys |
|---|---|---|
| `testSentFileBeginsTask` | `apiSendMessages`, direct, `send-file` | `[1]` |
| `testSentGroupFileBeginsTask` | `apiSendMessages`, group, `send-group-file` | `[1]` |
| `testSentTextBeginsNoTask` | `apiSendMessages`, direct, `send-text` | `[]` |
| `testCompletedUploadEndsTask` | task of file 1, `processReceivedMsg`, `upload-complete` | `[]` |
| `testUploadErrorEndsTask` | task of file 1, `processReceivedMsg`, `upload-error` | `[]` |
| `testCancelledUploadEndsTask` | task of file 1, `apiCancelFile`, `cancel` | `[]` |
| `testDeletedItemEndsTask` | task of file 1, `apiDeleteChatItems`, `delete` | `[]` |
| `testDeletedItemEventEndsTask` | task of file 1, `processReceivedMsg`, `delete` decoded as the event | `[]` |

Device tests, run by the user:
- A 20 MB file is sent, and the app leaves the foreground at once: the upload completes and the recipient receives the file, when the upload takes less than the background time iOS grants.
