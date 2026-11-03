# XFTP file receipt never completed when the temporary folder is on another file system

## Problem

The chat never completes the receipt of an XFTP file when the chat's temporary folder and the folder the file is saved to are on different file systems.

Typical setups:

- CLI and bots with default folders: the temporary folder is the system temporary directory (`/tmp`, a tmpfs on Fedora, Arch and others), and files are saved to `~/Downloads` when that folder exists. This includes a CLI used as a remote host for desktop.
- CLI with `/fr N <path>`, `/_download` or `--files-folder` / `--temp-folder` set to folders on different file systems.
- Desktop whose `simplex_v1_files` folder is a symlink or mount point on another file system.

The desktop and mobile apps with default folders are not affected: desktop sets both folders inside its data folder (`coreTmpDir` and `filesDir` in `Files.desktop.kt`), Android in `filesDir`, iOS under one app directory.

## Cause

At the end of an XFTP download, the chat moves the file from the temporary folder to the target path with `renameFile`. A rename across file systems is rejected: with `EXDEV` on Linux and macOS, and with `ERROR_NOT_SAME_DEVICE` on Windows, where `MoveFileEx` is called by `renameFile` without `MOVEFILE_COPY_ALLOWED`.

The agent downloads and decrypts the file into `xftp.decrypted` in its work folder `<temp>/<ts>_rcv.xftp`, sends `RFDONE`, and then marks the download complete, whatever the result of processing `RFDONE`. When the rename fails, the error is shown and `RFDONE` is not sent again. A received chat item stays at "receiving" with 100% progress, and the destination contains the empty placeholder file created when the file was accepted; the chat never reports the completion of a `/_download`. Two days after the download started, the agent deletes its work folder during cleanup, and with it the only complete copy.

## Fix

In the `RFDONE` handler, on an error from `renameFile xftpPath fsTargetPath`, the file is copied with `copyFile`, and then the source is removed with `removeFile`, whose error is ignored. After the file status is updated, the agent's work folder is deleted by `agentXFTPDeleteRcvFile` or, if the agent has not yet marked the download complete, the download is marked for the agent's periodic cleanup.

With `copyFile`, the data is written to a temporary file in the destination folder, which is then renamed over the destination. On an exception during the copy (for example, a full disk), that temporary file is removed and the destination is left unchanged.

A cross-file-system copy is performed in `agentSubscriber`, where agent events are processed one at a time, under the file's lock and, for a file in a contact or group chat, under that chat's lock. On a copy error, the item stays at "receiving" and the copy error is shown.

## Scope

| Site | From → to | Changed |
|---|---|---|
| `Subscriber.hs`, `RFDONE` | agent work folder (temporary folder) → files folder, `/fr` path or `/_download` path | yes |
| `Internal.hs`, `appendFileChunk` (inline receive with local encryption) | chat temporary folder → files folder or `/fr` path | no: inline user files come only from a modified sender (`fileInline = Nothing` is set in `xftpFileInvitation` and `createSndFileTransferXFTP`) |
| `Remote.hs`, `storeRemoteFile` | chat temporary folder → remote hosts folder (controller side, host-file encryption on) | no: send side, separate change |
| `Web.hs`, `Archive.hs`, directory service `Listing.hs` | within one folder | no |
| simplexmq | server code only | no |
