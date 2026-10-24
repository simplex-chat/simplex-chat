# Agent file deletion after XFTP uploads

On iOS, with simplexmq PR #2, the agent completes suspension only after the agent file of each upload started in foreground is deleted by the chat or TTL cleanup, or at the suspend timeout; files with rows absent or flagged deleted or failed at the start of the suspension are not awaited; on master, the chat skipped this deletion on several paths of `SFDONE` and `SFERR`, handling errors among them.
The deletions in the branches of `processAgentMsgSndFile` are replaced by one deletion after the handling of each `SFDONE` or `SFERR` with a file ID, also when an error is thrown; on `SFDONE` of an undeleted sent item with a shared message ID, the file descriptions are queued before it in the direct and group branches.

Agent side: https://github.com/evgeny-simplex/simplexmq/pull/2

## Decisions

- Terminal events:
  - `SFDONE`;
  - `SFERR`.
- Deletion: `xftpDeleteSndFileInternal`, once per terminal event with a file ID, after its handling, also when an error is thrown.
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

- The deletion is added to these paths of master `d63906ad5`:
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
- iOS spec, one sentence in each of two files: "While the chat is suspending, the agent continues the XFTP uploads started in foreground by this app process, and the core sends `chatSuspended` after the agent ends these uploads, or at the suspend timeout; uploads resumed from an earlier process are not awaited."
- `cabal.project`: simplexmq pinned to commit `1d74618a` of simplexmq PR #2 during review.
- After the simplexmq pull request in simplex-chat/simplexmq is merged and before the final pull request is opened in simplex-chat/simplex-chat:
  - the pin is replaced by the merged simplex-chat/simplexmq commit;
  - `scripts/nix/sha256map.nix` is updated.

## Mechanics

- `src/Simplex/Chat/Library/Subscriber.hs`, `processAgentMsgSndFile`:
  - clause for `SFERR` with an empty entity ID (:224-225): `eToView $ chatErrorAgent e`;
  - `` (`allFinally` when sndFileEnded deleteAgentFile) `` around the handling (:226);
  - missing user (:232): `unless sndFileEnded deleteAgentFile`;
  - `sndFileEnded :: Bool` (:235-239): `SFDONE` or `SFERR`;
  - `deleteAgentFile :: CM ()` (:240-241);
  - removed, the deletions of master `d63906ad5`:
    - :255, item missing;
    - :291, direct;
    - :311, group;
    - :363, `sendFileError`.
- `apps/ios/spec/architecture.md` (:131), "Background Task Protection": the sentence.
- `apps/ios/spec/services/files.md` (:274), "Sending": the sentence.
- `cabal.project` (:23-24):
  - `location: https://github.com/evgeny-simplex/simplexmq.git`;
  - `tag: 1d74618ab8c60ea949e23942e4c577cb099d8c51`.

## Tests

`tests/ChatTests/Files.hs`, "file transfer over XFTP":
- `checkNoAgentSndFiles` (:1088-1090): the count of agent `snd_files` rows with `deleted = 0` eventually equals 0.
- "send and receive file" (:668): `checkNoAgentSndFiles alice` after the direct transfer (:697).
- "send and receive file in group" (:754): `checkNoAgentSndFiles alice` after the group transfer (:791).
- "delete agent file after upload error" (:1043):
  - XFTP server with `newFileBasicAuth = Just "abcd"`; the client's server password `server_password` differs;
  - alice: "error uploading file 1 (test.pdf) for bob";
  - `checkNoAgentSndFiles alice`.
- "delete agent file when contact is deleted during upload" (:1058):
  - XFTP server down at the send;
  - bob deletes the contact with notification (`/_delete @2 full notify=on`);
  - server started: alice "bob: not ready";
  - `checkNoAgentSndFiles alice` (:1071).
  - On master, a test failure is expected: the deletion at master :291 is skipped after the error at :275, and the agent row is kept.
- "delete agent file when item is marked deleted during upload" (:1073), a regression check of cancellation by marking; a pass is expected on master as well:
  - XFTP server down at the send;
  - alice marks the item deleted for everyone;
  - the agent row is marked `deleted = 1` by the cancellation (`src/Simplex/Chat/Library/Internal.hs:648, 510, 3211`) before the server is started;
  - server started: `checkNoAgentSndFiles alice`.

## Open questions

1. Should the app yield to the share extension? While the agent of the app is suspending and continues uploads, the share extension and the app upload the same pending replicas:
   - the share extension starts or activates its chat with send workers at setup (`apps/ios/SimpleX SE/ShareModel.swift:181-182, 194`, `apps/ios/SimpleX SE/ShareAPI.swift:25, 87-89`, `src/Simplex/Chat/Library/Commands.hs:273, 608-610`);
   - later, in `send()`, it waits for the other processes (`ShareModel.swift:147-148`);
   - two concurrent `FPUT` of one replica (simplexmq `src/Simplex/FileTransfer/Server.hs`):
     - a second `FPUT` admitted by the commit check (:579) before the first commit, with the file opened after the first one's digest read, is answered with `AUTH`, and the stored chunk is removed by the server (:601-612; `Server/Store.hs:117-126`; `Server/Store/Postgres.hs:98-101`);
     - a second `FPUT` with the file opened during that read is answered with `FILE_IO`, the file is removed by the server (`src/Simplex/FileTransfer/Transport.hs:195-200, 238-239`; `Server.hs:613-617`), and the path of the removed chunk is committed for the first `FPUT`;
     - a second `FPUT` with the file opened during the first one's body write is answered with `FILE_IO`, and the file is removed by the server (`Transport.hs:195-200, 238`; `Server.hs:613-617`); the same error is then returned to the first `FPUT` at its digest read of the removed path (`Transport.hs:200, 239`; `Server.hs:613-617`);
     - each agent that receives an error records the file error and sends `SFERR` (simplexmq `src/Simplex/FileTransfer/Agent.hs:515-520, 541, 550-552`);
   - on master, the overlap is limited to the chunk in flight.

   Recommended: the app yields to the share extension.
   - The share extension sets `.sendingMessage` and waits for the other processes before it starts or activates its chat.
   - The app creates its share extension subscriber, the global constant at `apps/ios/Shared/Model/SuspendChat.swift:39`, at launch; in app code the constant is referenced only at its definition, and a global constant is created by Swift on its first access.
   - In `.suspending`, on that signal, the app suspends at once, by the steps of the `.suspending` branch of `terminateChat` (`SuspendChat.swift:69-74`).
   - In the subscriber's `.inactive` branch, the chat is activated only while the app is in foreground; today it is activated in `.suspending` and `.suspended` (`SuspendChat.swift:43-44`).
