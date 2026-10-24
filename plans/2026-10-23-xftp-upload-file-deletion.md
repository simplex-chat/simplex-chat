# Agent file deletion after XFTP uploads

On iOS, with simplexmq PR #2, the agent completes suspension only after the agent file of each upload started in foreground is deleted by the chat or TTL cleanup, or when the chat is stopped, or at the suspend timeout. Files with rows absent or flagged deleted or failed at the start of the suspension are not awaited. On master, the chat skipped this deletion on several paths of `SFDONE` and `SFERR`, handling errors among them.
The deletions in the branches of `processAgentMsgSndFile` are replaced by one deletion after the handling of each `SFDONE` or `SFERR` with a file ID, also when an error is thrown. On `SFDONE` of an undeleted sent item with a shared message ID, the file descriptions are queued before it in the direct and group branches.
The app yields to the share extension: the share extension signals the other processes and waits for them before it starts or activates its chat; the app subscribes to this signal at launch, suspends at once on it, and activates its chat in foreground after the share extension ends.

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
- iOS spec, the same text in each of two files: "While the chat is suspending, the agent continues the XFTP uploads started in foreground by this app process. The chat deletes the agent file of each upload after handling its completion or error, queueing the file descriptions of a completed upload first. The core sends `chatSuspended` after these deletions and the delivery of the queued messages, or at the suspend timeout. Uploads started while suspending or resumed from an earlier process are not awaited."
- Share extension states:

  ```
  setup         SEState .sendingMessage; wait for the app and the NSE to suspend, at most 2 s; chat started or activated
  send()        SEState .sendingMessage; wait as at setup; message sent
  completion    chat suspended; SEState .inactive
  ```

- The app's share extension subscriber is created at launch, in `application(_:didFinishLaunchingWithOptions:)`.
- App, on a share extension state:

  | Share extension state | App state | Action |
  |---|---|---|
  | `.sendingMessage` | `.active` | instant suspension |
  | `.sendingMessage` | `.activating` | instant suspension |
  | `.sendingMessage` | `.bgRefresh` | instant suspension |
  | `.sendingMessage` | `.suspending` | instant suspension |
  | `.sendingMessage` | `.suspended` | none |
  | `.sendingMessage` | `.stopped` | none |
  | `.inactive` | `.suspending` | `startChatAndActivate {}`, unless the app is in background |
  | `.inactive` | `.suspended` | `startChatAndActivate {}`, unless the app is in background |
  | `.inactive` | other states | none |

- Instant suspension, under `suspendLockQueue`, also by `terminateChat` in `.suspending`:

  ```
  _chatSuspended()               app state .suspended; ChatReceiver stopped while the chat is running; store closed
  apiSuspendChat(timeout 0)      when ChatModel.ok; the core sends no suspension events
  chatCloseStore()
  ```

- The NSE's share extension subscriber: unchanged.
- `cabal.project`: simplexmq pinned to commit `5cbc42c2` of simplexmq PR #2 during review.
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
- `apps/ios/spec/architecture.md` (:131), "Background Task Protection": the text.
- `apps/ios/spec/services/files.md` (:274), "Sending": the text.
- `apps/ios/SimpleX SE/ShareModel.swift`, `setup(with:)` (:97-98): `SEChatState.shared.set(.sendingMessage)`, then `await waitForOtherProcessesToSuspend()`, before `initChat`.
- `apps/ios/Shared/AppDelegate.swift`, `application(_:didFinishLaunchingWithOptions:)` (:22): `_ = seSubscriber`.
- `apps/ios/Shared/Model/SuspendChat.swift`:
  - `seSubscriber` (:39-54):
    - `.inactive` (:43-46): `startChatAndActivate {}` when the app state is `inactive` and `UIApplication.shared.applicationState != .background`;
    - `.sendingMessage` (:47-51): under `suspendLockQueue`, `_suspendChatInstantly()` when the app state is `canSuspend` or `.suspending`.
  - `terminateChat`, `.suspending` (:74-76): `_suspendChatInstantly()`.
  - new: `_suspendChatInstantly` (:88-93), the three steps of `terminateChat` in `.suspending`, with the comment of `apiSuspendChat`.
- `cabal.project` (:23-24):
  - `location: https://github.com/evgeny-simplex/simplexmq.git`;
  - `tag: 5cbc42c286ffd4461d496d7a7efc956078da06d4`.

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
- "delete agent file when item is marked deleted during upload" (:1073):
  - XFTP server down;
  - alice marks the item deleted for everyone;
  - the agent row is marked `deleted = 1` by the cancellation (`src/Simplex/Chat/Library/Internal.hs:648, 510, 3211`);
  - the agent's prepare worker finds the row marked deleted and sends `SFERR` (simplexmq `src/Simplex/FileTransfer/Agent.hs:509-510, 515-520`); the agent file is deleted after its handling;
  - the count of all agent `snd_files` rows eventually equals 0 (:1086).
  - On master, a test failure is expected when the check starts within 10 s of the agent start: the deletions are skipped for cancelled files, the marked row is deleted only by the agent cleanup 30 s after the agent starts, and the check polls for 20 s.

Device tests, run by the user:
- During a 20 MB upload, the app leaves the foreground; within 15 s, a link is shared through the share sheet:
  - both sends complete;
  - the app log shows `_chatSuspended` right after the share sheet opens.
- The share sheet is opened over the app in foreground, and a file is sent: the app's chat is active after the share sheet closes.
