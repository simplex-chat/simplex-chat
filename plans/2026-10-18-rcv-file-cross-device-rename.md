# Received files stuck when the temporary folder is on another file system

## Problem

Receiving a file over XFTP, or inline with local encryption, never completes when the chat's temporary folder and the folder the file is saved to are on different file systems.

Typical setups:

- CLI and bots with default folders: the temporary folder is the system temporary directory (`/tmp`, a tmpfs on Fedora, Arch and others), and files are saved to `~/Downloads` when that folder exists. This includes a CLI used as a remote host for desktop.
- CLI with `/fr N <path>`, `/_download` or `--files-folder` / `--temp-folder` pointing to different file systems.
- Desktop whose `simplex_v1_files` folder is a symlink or mount point on another disk.

The desktop and mobile apps with default folders are not affected: desktop sets both folders inside its data folder (`coreTmpDir` and `filesDir` in `Files.desktop.kt`), Android in `filesDir`, iOS under one app directory.

## Cause

Both receive paths finish by moving a file from the temporary folder to the target path with `renameFile`. A rename across file systems fails: with `EXDEV` on Linux and macOS, and with `ERROR_NOT_SAME_DEVICE` on Windows, where `renameFile` calls `MoveFileEx` without `MOVEFILE_COPY_ALLOWED`.

1. **XFTP receive** (`RFDONE` in `Subscriber.hs`). The agent downloads and decrypts the file into `xftp.decrypted` in its work folder `<temp>/<ts>_rcv.xftp`, sends `RFDONE`, and then marks the download complete, whatever the result of processing `RFDONE`. The chat renames the file to the target path. When the rename fails, the error is shown and `RFDONE` is not sent again. A received chat item stays at "receiving" with 100% progress, and the destination contains the empty placeholder file created when the file was accepted; a `/_download` never reports completion. Two days after the download started, the agent's cleanup deletes its work folder, which contains the only complete copy.
2. **Legacy receive with local encryption** (`appendFileChunk` in `Internal.hs`). Files received inline over SMP (offered, or sent without acceptance) are written to the target path in plain text. With local encryption on, after the last chunk is written the file is encrypted into a temporary file, the plain file is removed, and the encrypted file is renamed to the target path. When the rename fails, the plain file is already removed, the last chunk is already recorded, and the item stays at "receiving". Only the encrypted temporary file remains, and no code refers to it. Current clients send user files only over XFTP, but receivers still accept inline files. Clients older than v5.5.5 sent them, but such a client (v5.5.3) fails the handshake with the current preset SMP servers, and a current client cannot send a full-size chunk inline (`maxEncodedMsgLength` is smaller than `fileChunkSize`), so in practice only a modified client reaches this path. Group roster transfers also use this function; they have no local encryption, so they are not affected.

## Scope

All renames in the chat core and the simplexmq client code were checked:

| Site | From → to | Affected |
|---|---|---|
| `Subscriber.hs`, `RFDONE` | agent work folder (temporary folder) → files folder, `/fr` path or `/_download` path | yes, fixed |
| `Internal.hs`, `appendFileChunk` | chat temporary folder → files folder or `/fr` path | yes, fixed |
| `Remote.hs`, `storeRemoteFile` | chat temporary folder → remote hosts folder (controller side) | yes, on the send side when host-file encryption is on (for example, with a symlinked `remote_hosts` folder); not changed here |
| `Web.hs`, `Archive.hs` | within one folder | no |
| simplexmq client (agent, XFTP send and receive) | no renames | no |

## Change

When the rename fails, copy the file instead.

1. `RFDONE`: `renameFile xftpPath fsTargetPath` falls back to `copyFile`. The source is not removed at this call site: it is in the agent's work folder. `agentXFTPDeleteRcvFile`, called after the file status is updated, deletes that folder or, if the agent has not yet marked the download complete, marks the download for the agent's periodic cleanup.
2. `appendFileChunk`: `renameFile tmpFile fsFilePath` falls back to `copyFile`, then removes `tmpFile`. No other code refers to this file. A failure to remove it is ignored, as for the other `tmpFile` removal in this function.

`copyFile` writes to a temporary file in the destination folder and renames it over the destination. When the copy fails with an exception (for example, a full disk), that temporary file is removed and the destination is unchanged.

## Options considered

1. **Copy when the rename fails (this change).** Two call sites; receives where the rename succeeds are unchanged.
2. **Copy only on the cross-device error.** Requires an IO-level catch and new imports, and does not complete receives after other rename failures that a copy can recover from.
3. **A shared "rename or copy" helper.** There are two sites, and they differ in whether the source is removed at the call site, so a helper would require a parameter or would add a removal that the XFTP site does not require.
4. **Write directly to the files folder.** The agent would require a target path from the chat (a simplexmq API change), and the legacy path would change for every locally encrypted inline receive, not only for those where the rename fails.

## Limitations

- On a cross-file-system receive, the copy runs in `agentSubscriber`, which processes agent events one at a time, so other events are delayed for the duration of the copy. For an XFTP file received in a contact or group chat, the copy also runs under that chat's lock, so commands for that chat wait as well.
- If the process stops during the copy, the item stays at "receiving", and in the legacy path the encrypted temporary file remains. When the process stops without running exception handlers (a crash, a forced kill), a `.copyFile*.tmp` file remains in the destination folder. With `renameFile` this interval was negligible.
- If the copy fails, the item stays at "receiving", as before for any rename failure, and the error shown is the copy error; the rename error is not shown or logged. In the legacy path the encrypted temporary file remains in the temporary folder, as before.
- If processing `RFDONE` fails after a successful copy (for example, a database error), `agentXFTPDeleteRcvFile` is not called, and the source file in the agent's work folder remains until the agent deletes the download.
