# Desktop: a Save dialog saves another file's content

## Problem

On desktop some save requests open the Save dialog only after a wait: a file tapped before it was received opens it when the download finishes (`OpenWhenReceivedEffect` → `saveFileLauncher.launch`), a file saved or shared while connected to a mobile after it is fetched from the mobile (`loadRemoteFile`), and the database export after the archive is written. `DialogState.awaitResult` replaces the pending result and parameters on every call, while the JFileChooser that is already open keeps running. So when such a request arrives while a Save dialog for another file is open, the open dialog still shows the first file's name, but pressing Save completes the second request: the second file's content is written under the first file's name, and the first request never completes.

Reproduced on the master prebuild: download a 115-byte text file and cancel its Save dialog; tap a 40 MB file; click the text file again and keep its Save dialog open until the large file finishes; press Save. The saved "file-A-small.txt" is 40,000,000 bytes, identical to the large file.

## Fix

`FileChooserLauncher.launch` completes a save request with no file, as if cancelled, when a Save dialog is already open (`saveDialog.isAwaiting`), instead of taking over that dialog. Every save callback already handles a cancel (the database export deletes its temporary archive). The chooser is modal, so a click can't make a second request; only one of the delayed requests above can, and it is the one dropped.

## Not fixed

- The open-file dialogs (`openDialog`, `openMultipleDialog`) have the same replace-on-call shape, but nothing opens them without a click, and the modal chooser blocks clicks.
- `saveTempImageUncompressed` calls `saveDialog.awaitResult` directly; it is opened only by a click.
- The check and the dialog opening are not atomic, so two delayed requests in the same instant could still both reach the dialog, as on master.

## Effect

Desktop only: while a Save dialog is open, a delayed save request is cancelled instead of saving its content under the open dialog's name. A received or fetched file stays in the chat to be saved again; a database export has to be run again. Android has no such dialog state; unchanged.
