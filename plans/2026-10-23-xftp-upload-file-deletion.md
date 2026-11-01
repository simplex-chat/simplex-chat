# Agent file deletion after XFTP uploads

On iOS, with simplexmq PR #2, the agent completes suspension only after the agent file of each upload started in foreground is deleted by the chat or TTL cleanup, or when the chat is stopped, or at the suspend timeout. Files with rows absent or flagged deleted or failed at the start of the suspension are not awaited. On master, the chat skipped this deletion on several paths of `SFDONE` and `SFERR`, handling errors among them.
The deletions in the branches of `processAgentMsgSndFile` are replaced by one deletion after the handling of each `SFDONE` or `SFERR` with a file ID, also when an error is thrown. On `SFDONE` of an undeleted sent item with a shared message ID, the file descriptions are queued before it in the direct and group branches.
A suspending app yields to the share extension. The share extension signals the other processes at setup and, when the app is suspending at setup, waits for them before it starts or activates its chat. The app subscribes to this signal at launch and suspends at once on it while suspending.

Agent side: https://github.com/evgeny-simplex/simplexmq/pull/2

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
- Share extension states:

  ```
  setup         SEState .sendingMessage; app state .suspending -> wait for the app and the NSE to suspend, at most 2 s; chat started or activated
  send()        SEState .sendingMessage; wait for the app and the NSE to suspend, at most 2 s; message sent
  completion    chat suspended; SEState .inactive
  ```

- The app's share extension subscriber is created at launch, in `application(_:didFinishLaunchingWithOptions:)`.
- App, on a share extension state:

  | Share extension state | App state | Action |
  |---|---|---|
  | `.sendingMessage` | `.suspending` | instant suspension |
  | `.sendingMessage` | other states | none |
  | `.inactive` | any | none |

- Instant suspension, under `suspendLockQueue`, also by `terminateChat` in `.suspending`:

  ```
  apiSuspendChat(timeout 0)      when ChatModel.ok; the core sends no suspension events
  _chatSuspended()               app state .suspended; ChatReceiver stopped while the chat is running; stores closed
  ```

  ```mermaid
  sequenceDiagram
    participant SE as Share extension
    participant App
    SE->>App: .sendingMessage, at setup
    Note over App: app state .suspending: apiSuspendChat 0, then _chatSuspended
    Note over SE: app state .suspending at setup: wait for the app and the NSE, at most 2 s
    Note over SE: chat started or activated
    SE->>App: .sendingMessage, before sending
    Note over SE: wait for the app and the NSE, at most 2 s; message sent
    SE->>App: .inactive, at completion
  ```

- The app chat is activated by the scene phase handling when the app is opened, as on master.
- A chunk upload of the app in flight at the instant suspension is not stopped; the share extension can upload the same replica at the same time, as on master.
- An agent event queued or in handling at the instant suspension can be handled after the stores are closed:
  - `SFDONE`: the file descriptions are not sent, since the agent sends no further `SFDONE` for a complete file; the agent file is deleted at the next activation;
  - `SFERR`: the error status of the item is not recorded.
- The NSE's share extension subscriber: unchanged. The NSE does not act on `.sendingMessage` (its subscriber is never created). The wait therefore ends only when the NSE suspends on its own, or after 2 s. While the stored NSE state is `.created`, the wait lasts the full 2 s. The app's instant suspension can start the chat of an NSE instance waiting for the app, and that chat then runs concurrently with the share extension.
- `cabal.project`: simplexmq pinned to commit `a90dcbc4` of the simplexmq fork during review.
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
- `apps/ios/spec/architecture.md` (:263), "Share Extension (SE)": the signal at setup and before sending; the wait at setup when the app state read at setup is `.suspending`, and before sending in any app state; the NSE, which does not act on the signal; the full 2 s wait while the stored NSE state is `.created`; the instant suspension of the app in `.suspending`, with the conditions `ChatModel.ok` and `chatRunning == true`.
- `apps/ios/spec/database.md` (:79) and `apps/ios/spec/architecture.md` (:8): the `AppDelegate.swift` line ranges.
- `apps/ios/SimpleX SE/ShareModel.swift`, `setup(with:)` (:97-100): `SEChatState.shared.set(.sendingMessage)`, then `await waitForOtherProcessesToSuspend()` when `appStateGroupDefault.get() == .suspending`, before `initChat`.
- `apps/ios/Shared/AppDelegate.swift`, `application(_:didFinishLaunchingWithOptions:)` (:22): `_ = seSubscriber`.
- `apps/ios/Shared/Model/SuspendChat.swift`:
  - `seSubscriber` (:39-53):
    - `.inactive` (:43-44): no action;
    - `.sendingMessage` (:45-50): under `suspendLockQueue`, `_suspendChatInstantly()` when the app state is `.suspending`.
  - `terminateChat`, `.suspending` (:73-75): `_suspendChatInstantly()`.
  - new: `_suspendChatInstantly` (:87-91): `apiSuspendChat(timeoutMicroseconds: 0)` when `ChatModel.ok`, with its comment from `terminateChat`, then `_chatSuspended()`.
- `cabal.project` (:23-24):
  - `location: https://github.com/evgeny-simplex/simplexmq.git`;
  - `tag: a90dcbc4d2aa1f07cc20ddc6d56d3b0d3d9ea4ae`.

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

Device tests, run by the user:
- During a 20 MB upload, the app leaves the foreground; within 15 s, a link is shared through the share sheet; then the app is opened:
  - both sends complete, unless a chunk upload of the app was in flight when the share sheet opened; then the app's file can fail with an upload error, as on master;
  - the app log shows `_chatSuspended` right after the share sheet opens.
- The share sheet is opened over the app in foreground, and a file is sent: the app's chat stays active.
