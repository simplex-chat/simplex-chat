# Agent file deletion after XFTP uploads

On iOS, an upload started in foreground continues while the app suspends, and suspension waits until the chat deletes the agent file of the upload or the suspend timeout (simplexmq PR #2).
The chat deletes the agent file once after every terminal upload event, after the file descriptions are queued, also when the handling of the event fails.

Agent side: https://github.com/evgeny-simplex/simplexmq/pull/2

## Decisions

- Terminal events: `SFDONE`, `SFERR`.
- Deletion: `xftpDeleteSndFileInternal`, once per terminal event, after its handling, also after an error of the handling.
- File descriptions are queued before the deletion.
- `SFPROG` and `SFWARN`: the agent file is deleted only when the user is missing, as on master.
- `SFERR` with an empty entity ID: the agent error is sent to the view; the file lookup and the deletion are skipped.
- Events sent before the deletion:

  | Branch | Events |
  |---|---|
  | item missing | `CEvtSndFileRedirectStartXFTP`, `CEvtSndStandaloneFileComplete` or `CEvtSndFileError` |
  | direct | `CEvtSndFileProgressXFTP` |
  | group | `CEvtSndFileCompleteXFTP` |
  | `SFERR` | `CEvtSndFileError` |

- Paths of master without the deletion, now with it:
  - agent file unknown to the chat;
  - file record missing;
  - file cancelled, deleted by the canceller before: the deletion is repeated;
  - `SFDONE` with fewer descriptions than transfers;
  - direct: `contactSendConn_` fails (contact not ready, inactive or disabled); queueing fails;
  - group: queueing or the item update fails;
  - other chat type or direction;
  - item deleted or without a shared message ID;
  - `SFERR`: the status update fails.
- iOS spec, one sentence in each of two files: "While the chat is suspending, XFTP uploads started in foreground by this app process continue, and the core sends `chatSuspended` after they end or at the suspend timeout; uploads resumed from an earlier process are not awaited."
- `cabal.project`: simplexmq is pinned to commit `7fcc2176` of simplexmq PR #2 during review; the pin is replaced by the merged simplex-chat/simplexmq commit, and `scripts/nix/sha256map.nix` is updated, before this pull request is marked ready.

## Mechanics

- `src/Simplex/Chat/Library/Subscriber.hs`, `processAgentMsgSndFile`:
  - clause for `SFERR` with an empty entity ID (:224-225): `eToView $ chatErrorAgent e`;
  - `` (`allFinally` when sndFileEnded deleteAgentFile) `` around the handling (:226);
  - missing user (:232): `unless sndFileEnded deleteAgentFile`;
  - `sndFileEnded :: Bool` (:235-239): `SFDONE` or `SFERR`;
  - `deleteAgentFile :: CM ()` (:240-241);
  - removed: the deletions of master `d63906ad5` at :255 (item missing), :291 (direct), :311 (group) and :363 (`sendFileError`).
- `apps/ios/spec/architecture.md` (:131), "Background Task Protection": the sentence.
- `apps/ios/spec/services/files.md` (:274), "Sending": the sentence.
- `cabal.project` (:23-24): `location: https://github.com/evgeny-simplex/simplexmq.git`, `tag: 7fcc2176475f04b7563aaa912f69bb70acee524e`.

## Tests

`tests/ChatTests/Files.hs`, "file transfer over XFTP":
- `checkNoAgentSndFiles` (:1088-1090): agent `snd_files` rows with `deleted = 0`, eventually 0.
- "send and receive file" (:668), assertion added (:697): direct `SFDONE`.
- "send and receive file in group" (:754), assertion added (:791): group `SFDONE`.
- "delete agent file after upload error" (:1043):
  - XFTP server with `newFileBasicAuth`, client without a password;
  - alice: "error uploading file 1 (test.pdf) for bob";
  - `checkNoAgentSndFiles alice`.
- "delete agent file when contact is deleted during upload" (:1058):
  - XFTP server down at the send;
  - bob deletes the contact;
  - server started: alice "bob: not ready";
  - `checkNoAgentSndFiles alice`.
- "delete agent file when item is marked deleted during upload" (:1073):
  - XFTP server down at the send;
  - alice marks the item deleted for everyone;
  - server started: `checkNoAgentSndFiles alice`.

## Open questions

1. While the app's upload stage is open, the share extension and the app upload the same pending replicas:
   - the share extension starts its chat with send workers at setup (`apps/ios/SimpleX SE/ShareModel.swift:194`, `apps/ios/SimpleX SE/ShareAPI.swift:25`, `src/Simplex/Chat/Library/Commands.hs:273`), before its wait for the other processes in `send()` (`ShareModel.swift:147-148`);
   - two concurrent `FPUT` of one replica fail the upload: the server returns `AUTH` to the second and removes the stored chunk (simplexmq `src/Simplex/FileTransfer/Server.hs:579, 601-612`, `src/Simplex/FileTransfer/Server/Store.hs:117-126`);
   - on master the overlap was the chunk in flight.

   Recommended: the app yields to the share extension.
   - The share extension sets `.sendingMessage` and waits for the other processes before it starts its chat.
   - The app creates its share extension subscriber at launch; it is a global constant defined at `apps/ios/Shared/Model/SuspendChat.swift:39`, Swift creates a global constant on its first access, and app code does not access it.
   - In `.suspending`, on that signal, the app suspends at once, as the `.suspending` branch of `terminateChat` does (`SuspendChat.swift:69-74`).
   - The subscriber's `.inactive` branch activates the chat only when the app is active.
