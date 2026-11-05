# Desktop: Save in an open Save dialog writes another file's content

## Problem

On desktop, the Save dialog of some save requests opens after a wait:
- a received file tapped before its download finished (`OpenWhenReceivedEffect`);
- a file fetched from a linked mobile (`loadRemoteFile`);
- the database export, after the archive is written.

When such a request arrives while a Save dialog for another file is open and the user presses Save, the second file's content is saved under the first file's name, and the second file gets no dialog.

After a QR code is clicked and its image saved, later saves open nothing until the app is restarted. The same happens when a QR code view closes while its folder chooser is open.

## Cause

`DialogState.awaitResult` (`DesktopApp.kt`) replaces the slot's pending request and `params` on every call. The chooser is composed while any request is pending and is created once, so the open chooser completes the newest request.

The slot is cleared only after `await()` returns. A cancelled awaiting coroutine (the QR code view's scope) leaves the slot claimed, and no later request gets a chooser.

After the QR image is saved, desktop `shareFile` makes a second save request for the same file.

## Fix

`DialogState<T>(cancelled: T)`:
- `cancelled` is the value of the user's Cancel: `null` for the open and save slots, an empty list for the open-multiple slot.
- `awaitResult`, under one lock: when a request is pending, logs `File dialog request cancelled: another request is pending` and returns `cancelled`; otherwise writes `params`, then `request`.
- `awaitResult` awaits in `try`; `finally` clears `request` under the same lock.
- `request` is readable outside the class; `isAwaiting` and `onResult` are removed.

`DesktopApp.kt`: each of the 3 choosers is composed inside `key(request)` and its callback completes that request.

`FileDialogChooserMultiple` (Linux and Windows): the `JFileChooser` is created before the coroutine starts, and `onDispose` calls `cancelSelection()` on it.

`QRCode.kt`: after the image is saved, `shareFile` is called only on Android.

`DialogStateTest` (desktopTest):
- a save request made while one is pending gets `null` at once;
- a save request made after a cancelled one waits for its dialog (the `finally`).

## Scope

Desktop file dialogs: the open, open-multiple and save slots and all their callers. Android: only the platform check before the existing `shareFile` call.
