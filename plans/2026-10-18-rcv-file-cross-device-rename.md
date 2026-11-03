# Received files stuck when the temporary folder is on another file system

## Problem

Receiving a file over XFTP never completes when the chat's temporary folder and the folder the file is saved to are on different file systems.

Typical setups:

- CLI and bots with default folders: the temporary folder is the system temporary directory (`/tmp`, a tmpfs on Fedora, Arch and others), and files are saved to `~/Downloads` when that folder exists. This includes a CLI used as a remote host for desktop.
- CLI with `/fr N <path>`, `/_download` or `--files-folder` / `--temp-folder` pointing to different file systems.
- Desktop whose `simplex_v1_files` folder is a symlink or mount point on another disk.

The desktop and mobile apps with default folders are not affected: desktop sets both folders inside its data folder (`coreTmpDir` and `filesDir` in `Files.desktop.kt`), Android in `filesDir`, iOS under one app directory.

## Cause

An XFTP receive finishes by moving a file from the temporary folder to the target path with `renameFile`. A rename across file systems fails: with `EXDEV` on Linux and macOS, and with `ERROR_NOT_SAME_DEVICE` on Windows, where `renameFile` calls `MoveFileEx` without `MOVEFILE_COPY_ALLOWED`.

The agent downloads and decrypts the file into `xftp.decrypted` in its work folder `<temp>/<ts>_rcv.xftp`, sends `RFDONE`, and then marks the download complete, whatever the result of processing `RFDONE`. The chat renames the file to the target path. When the rename fails, the error is shown and `RFDONE` is not sent again. A received chat item stays at "receiving" with 100% progress, and the destination contains the empty placeholder file created when the file was accepted; a `/_download` never reports completion. Two days after the download started, the agent's cleanup deletes its work folder, which contains the only complete copy.

## Fix

In `RFDONE`, when `renameFile xftpPath fsTargetPath` fails, `copyFile` copies the file instead. The source is not removed at this call site: `agentXFTPDeleteRcvFile`, called after the file status is updated, deletes the agent's work folder or, if the agent has not yet marked the download complete, marks the download for the agent's periodic cleanup.

`copyFile` writes to a temporary file in the destination folder and renames it over the destination. When the copy fails with an exception (for example, a full disk), that temporary file is removed and the destination is unchanged.

A cross-file-system copy runs in `agentSubscriber`, which processes agent events one at a time, and, for a file in a contact or group chat, under that chat's lock. If the copy fails, the item stays at "receiving" and the copy error is shown.

## Scope

| Site | From → to | Changed |
|---|---|---|
| `Subscriber.hs`, `RFDONE` | agent work folder (temporary folder) → files folder, `/fr` path or `/_download` path | yes |
| `Internal.hs`, `appendFileChunk` (inline receive with local encryption) | chat temporary folder → files folder or `/fr` path | no: no sender in this code base produces inline user files (`xftpFileInvitation` and `createSndFileTransferXFTP` set `fileInline = Nothing`) |
| `Remote.hs`, `storeRemoteFile` | chat temporary folder → remote hosts folder (controller side, host-file encryption on) | no: send side, separate change |
| `Web.hs`, `Archive.hs` | within one folder | no |
| simplexmq client (agent, XFTP send and receive) | no renames | no |
