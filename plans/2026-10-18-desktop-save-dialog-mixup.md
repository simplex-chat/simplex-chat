# Desktop: saving from an open Save dialog writes another file's content

## Problem

On desktop some save requests open the Save dialog only after a wait: a file tapped before it was received opens it when the download finishes (`OpenWhenReceivedEffect` → `saveFileLauncher.launch`), a file saved or shared from a linked mobile opens it once the file is fetched (`loadRemoteFile`), and the database export opens it after the archive is written. `DialogState.awaitResult` replaces the pending result and parameters on every call, while the file chooser that is already open (JFileChooser on Linux and Windows, AWT `FileDialog` on macOS) is created once and keeps its parameters. So when such a request arrives while a Save dialog for another file is open, the open dialog still shows the first file's name, but pressing Save completes the second request: the second request's callback writes the second file's content to the chosen path, and the first request never completes.

Reproduced on the master prebuild: download a 115-byte text file and cancel its Save dialog; tap a 40 MB file; click the text file again and keep its Save dialog open until the large file finishes; press Save. The saved "file-A-small.txt" is 40,000,000 bytes, identical to the large file.

## Fix

`DialogState` takes the value its dialog returns on Cancel: `null` for the open and save slots, an empty list for the open-multiple slot. When a request is already awaiting in the slot, `awaitResult` returns that value at once instead of replacing the pending request. The check and the claim are done under one lock, so of two requests at the same instant the dialog is opened for one, and the other is cancelled.

Inside the lock, `awaitResult` now writes `params` before `onResult` (master writes them the other way round). The dialog is shown when `onResult` is set (`isAwaiting`), and the chooser takes its file name once, when it opens; the composable may read `params` again on recomposition, which does not change what the open chooser uses. With master's order, a dialog composed between the two writes would get the previous parameters. For a later request, it keeps the previous request's file name for as long as it is open, while Save completes the new request: the same mix-up this PR fixes. For the slot's first request, it gets no file name: on Linux and Windows the Save dialog opens as a folder chooser, and the launcher saves to `<folder>/<the request's own name>` (`FileChooserLauncher.launch`); on macOS the user types the name. The request's own content is saved, so this case is not the mix-up.

The guard is in the slot, so every caller is covered: `FileChooserLauncher.launch` (open and save), `FileChooserMultipleLauncher.launch` and `saveTempImageUncompressed` (QR code save). Every callback already handles a cancel (the database export deletes its temporary archive). The request that arrives second is the one cancelled, whether it is delayed or comes from a click. A click can make a second request before the chooser is shown, because only a shown chooser blocks clicks; and a QR code click computes the QR image before it calls `awaitResult`.

## Alternatives

- A guard at the call site, in the save branch of `FileChooserLauncher.launch` (+1 line). It leaves `saveTempImageUncompressed` unguarded: if a delayed request opens the Save dialog while a clicked QR code image is being computed, the QR request replaces it: pressing Save completes the QR request, and the delayed request never completes (for a database export, the progress indicator stays shown and the archive is not deleted). Every new caller would have to repeat the check.
- The same guard repeated in `saveTempImageUncompressed` (+1 line). It closes the QR path, but the check is then needed at every caller of `awaitResult`, and the open slots stay unguarded.
- Leaving the QR path open. No content is written to the wrong file, but the delayed request never completes, which is half of this bug.
- Holding `params` and the request in one state value, so that both are set by one write and the order is structural (about 4-6 more changed lines). Not chosen: the diff is larger, and the write order is now stated here and reviewed.
- Serialising requests so a delayed request gets its own dialog after the open one closes. Better for the user, but larger, and it depends on a request made right after another getting a dialog (see "Not fixed").

## Not fixed

- If a slot is left awaiting with no dialog shown, every later request to that slot is cancelled at once, where master leaves it waiting forever. Two pre-existing causes, to be fixed separately: `awaitResult` does not clear the slot when the awaiting coroutine is cancelled, and a request made right after another, with no frame between them, may get no dialog.
- A peer can cause the first of these: if the peer connects through a one-time link while the user has the Save dialog of that link's QR code open, the QR code view closes and cancels the coroutine waiting in `awaitResult`. The fix is a `try/finally` in `awaitResult` that clears the slot; the open dialog's callback must then accept a cleared slot. Separate PR.

## Effect

Desktop only: while a request is awaiting in a file dialog slot, a further request to the same slot is cancelled instead of replacing the request that the open dialog completes. A received or fetched file stays in the chat to be saved again; a database export has to be run again. The open dialogs have no delayed caller, so there a second request needs a second click before the chooser is shown, and that request is cancelled. Requests made while no request is awaiting behave as before. Android has no such dialog state; unchanged.
