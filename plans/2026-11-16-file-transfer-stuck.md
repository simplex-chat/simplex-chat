# Files stuck in transfer

Three fixes in one pull request: #8018, #8019 and fork #68 (Narasimha-sc/simplex-chat).

## Problems

1. A received file stays "receiving" forever when the temporary folder is on another file system than the files folder (for example `/tmp` on tmpfs). #8018.
2. With host file encryption on, storing a file on a remote host fails when the remote hosts folder is on another file system than the temporary folder. Fork #68.
3. In a group, a received file can stay on "Waiting for file" forever, while the sender shows it as uploaded. #8019.

## Approach

1 and 2. The core moves a finished file out of the temporary folder with `renameFile`, which fails across file systems (`EXDEV`). Both sites now call one helper, `moveFile` (`Simplex.Chat.Util`): rename, and on an IO error copy and remove the source. A failed removal of the source is ignored, as the file is already in place. Sites:
- `processAgentMsgRcvFile` (`Library/Subscriber.hs`), on `RFDONE`;
- `storeRemoteFile` (`Remote.hs`), with encryption on.

3. The sender sends a group file's description once, on upload completion, only to members with a transfer record and a ready connection. A forwarded member depends on the host, and the host stops forwarding when the two members connect; a member whose file message was queued as pending gets nothing. On upload completion, the sender now also sends the description to every member that got the file message and has no description yet: now when its connection is ready, else as a pending group message. Details: `plans/2026-11-03-group-file-description-late-members.md`.

## Tested

- 1: new test in `tests/ChatTests/Files.hs` fails on master and passes with the fix (fork CI, SQLite and Postgres); desktop Linux and Android runs.
- 2: new test in `tests/RemoteTests.hs` passes in fork CI; not run without the fix.
- 3: new test in `tests/ChatTests/Groups.hs` fails on master and passes with the fix (SQLite and Postgres); desktop Linux runs with CLI peers.

## Open questions

1. `moveFile` ignores a failed removal of the source; the original #68 reported it as an error after the file was stored. Recommended: ignore.
2. `appendFileChunk` (`Library/Internal.hs`) moves an encrypted SMP-received file from the temporary folder with `renameFile`, the same failure across file systems. Recommended: use `moveFile` there in this pull request.
3. Merge order: one pull request; the three fixes share no code.
