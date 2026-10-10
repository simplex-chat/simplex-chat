# Desktop: crash in the SimpleX address screen when there is no profile

## Problem

A Windows user who uses the desktop app only through a linked mobile reported this crash:

```
java.lang.NullPointerException
  at chat.simplex.common.views.chatlist.ComposableSingletons$UserPickerKt$lambda$-1608374373$1.invoke(UserPicker.kt:223)
  at chat.simplex.common.views.chatlist.UserPickerKt$UserPicker$6$showCustomModal$1$1$1.invoke(UserPicker.kt:157)
  at chat.simplex.common.views.helpers.ModalManager$showInView$4$1$1.invoke(ModalView.kt:218)
```

## Cause

The "SimpleX address" row of the user picker opens `UserAddressView` with `shareViaProfile = it.currentUser.value!!.addressShared`. `currentUser` is null on a desktop that has no local profile and no connected mobile. Two actions show this screen in that state:

1. The screen was opened for the mobile's profile, and then the mobile disconnects. `switchUIRemoteHost(null)` sets `currentUser` to the local user, which is null. The left-panel modals stay open, and the screen crashes when it recomposes. If another screen is on top of it, it crashes when the user goes back to it.
2. With no profile, the user picker still shows the "Create SimpleX address" row. Clicking it crashes immediately.

## Fix

Return from the modal when there is no current user. The "Chat preferences" row below already does this (`m.currentUser.value ?: return@showCustomModal`).

Closing the left-panel screens on a device switch, so that case 1 does not leave a stale screen, is a separate PR.

## Verification

Desktop AppImages from master and from this branch, each with a fresh database. The v7.1.0-beta.6 CLI acts as the linked mobile (`/connect remote ctrl`).

- master, case 1: NPE at `UserPicker.kt:223`.
- master, case 2: NPE at `UserPicker.kt:223` with the reported stack (`showCustomModal` 157, `ModalView.kt:218`).
- This branch, case 1: no crash, with the address screen on top and with another screen on top followed by back.
- This branch, case 2: no crash.
