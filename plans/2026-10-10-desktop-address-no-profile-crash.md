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

The "SimpleX address" row of the user picker opens `UserAddressView` with `shareViaProfile = it.currentUser.value!!.addressShared`. The modal content reads `currentUser`, so it recomposes whenever `currentUser` changes. It throws whenever it is composed while `currentUser` is null.

On desktop, `currentUser` is null when there is no local profile and no connected mobile. The screen can be shown in that state in these ways:

1. The screen was opened for the mobile's profile, and then the mobile disconnects. `switchUIRemoteHost(null)` sets `currentUser` to the local user, which is null, and the left-panel modals stay open. The screen crashes when it recomposes. If another screen is on top of it, it crashes when the user goes back to it.
2. With no profile, the user picker still shows the "Create SimpleX address" row. Clicking it crashes immediately.
3. The address screen is open, and the last visible profile is deleted while hidden profiles remain. Desktop then sets `currentUser` from `apiGetActiveUser` (`UserProfilesView.kt`, `doRemoveUser`) and does not close the modals.

## Fix

When there is no current user, the modal closes itself instead of composing `UserAddressView`.

An early `return@showCustomModal`, as the "Chat preferences" row does, would avoid the crash. But it would leave an invisible modal on the left-panel stack, with no back button. That modal would come back as the address screen once a profile exists.

## Verification

Desktop AppImages from master and from this branch, each with a fresh database. The v7.1.0-beta.6 CLI acts as the linked mobile (`/connect remote ctrl`).

- master, case 1: NPE at `UserPicker.kt:223`.
- master, case 2: NPE at `UserPicker.kt:223` with the reported stack (`showCustomModal` 157, `ModalView.kt:218`).
- This branch, case 1 with the address screen on top: no crash, and the screen closes.
- This branch, case 1 with another screen on top: no crash; going back closes the address screen and shows the chat list.
- This branch, case 2: no crash, and nothing is left open. A profile created afterwards finishes onboarding without the address screen appearing, and its own address screen then opens normally.
- This branch, with a local profile: the address screen opens and closes normally. When the mobile disconnects with its address screen open, the screen closes and the app returns to the local profile.
