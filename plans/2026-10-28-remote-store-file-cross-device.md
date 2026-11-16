# Store an encrypted file on the controller when the rename to the remote hosts folder fails

## Problem

`/store remote file <host> encrypt=on <path>` (and sending a file to a remote host with host file encryption on) fails when the controller's temp folder and `remote_hosts_folder` are on different filesystems. The file has already been uploaded to the remote host at that point, the command returns an error, the message is not sent, and the encrypted temp file stays in the temp folder.

## Cause

FACT (Remote.hs `storeRemoteFile`): with encryption the file is encrypted into `getChatTempDirectory`, uploaded with `remoteStoreFile`, then moved into `<remote_hosts_folder>/<storePath>/<remoteFilesFolder>` with `renameFile`. `renameFile` cannot move a file across filesystems (EXDEV), and nothing handles that. Without encryption the same step is `copyFile` and works.

FACT: reaching this needs two filesystems. INFERENCE (not verified): examples are a CLI controller with the default temp folder in `/tmp` (tmpfs on many systems) and `remote_hosts_folder` on disk, or symlinked desktop folders.

INFERENCE: the leaked temp file is the pre-existing result of any failure after `encryptLocalFile`; this PR fixes it only for the rename failure.

## Fix

In `storeRemoteFile`, when encrypting, fall back to copy and remove if the rename fails:

```haskell
if encrypt
  then renameFile filePath hPath `catchAllErrors` \_ -> copyFile filePath hPath >> removeFile filePath
  else copyFile filePath hPath
```

`removeFile` is added to the `UnliftIO.Directory` import. The same-filesystem path is unchanged (one rename). `catchAllErrors` is already used in this module and works in `CM` (`ExceptT ChatError IO`).

## Alternatives

- Always copy then remove: costs a full copy for the common same-filesystem case.
- Create the encrypted temp file inside the hosts folder: changes where plaintext-adjacent temp data lives and needs the folder before the upload.
- Also delete the temp file when the upload fails: separate pre-existing leak, not needed for this fix; left out to keep the diff minimal.

## Tests

`remoteStoreFileOtherFsTest` in tests/RemoteTests.hs points `/remote_hosts_folder` at a directory in `/dev/shm`, runs `/store remote file 1 encrypt=on`, and decrypts the stored file to compare with the fixture. It is marked pending when `/dev/shm` does not exist or a probe rename from the test's chat temp folder (`tmpPath`, set by `SetTempFolder` and the source of the renamed temp file) into it succeeds (same filesystem), so it cannot pass vacuously. INFERENCE: it should fail on master because `renameFile` cannot cross filesystems (EXDEV); confirm on CI. It reads the stored file only and does not assert that the encrypted temp file is removed. Not run locally; CI only.

## Open questions

- If the copy succeeds but `removeFile` of the temp file fails, the command reports that error although the file is stored. Accepted, same as the existing upload-failure leak; INFERENCE: the OS clears the temp directory eventually (no core cleanup found, apps not checked).
- A rename error other than EXDEV (permissions) is swallowed and the copy reports its own error.
