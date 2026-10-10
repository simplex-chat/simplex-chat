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

The "SimpleX address" row of the user picker opens `UserAddressView` with `shareViaProfile = it.currentUser.value!!.addressShared`. The modal content reads `currentUser`, so it recomposes whenever `currentUser` changes. It throws whenever it is composed while `currentUser` is null, that is, when there is no active user. A desktop that was only used with a linked mobile has no active user of its own.

The screen can be shown in that state in two ways:

1. **The mobile disconnects.** The screen was opened for the mobile's profile, and then the mobile disconnects. `switchUIRemoteHost(null)` sets `currentUser` to the local user, which is null, and the left-panel modals stay open.
   - If the address screen is on top, it crashes when it recomposes.
   - If another screen is on top of it, it crashes when the user goes back to it.

   `UserAddressView` closes itself when the user changes (`KeyChangeEffect` on the user), but with a null user it is never reached.
2. **The row is clicked with no active user.** With no profile and no mobile, the user picker opens by itself (`App.kt`, `desktopNoUserNoRemote`) and still shows the "Create SimpleX address" row. Clicking it crashes immediately.

   The row is also offered after deleting the last visible profile while hidden ones remain. Desktop then sets `currentUser` from `apiGetActiveUser` (`UserProfilesView.kt`, `doRemoveUser`), which is null.

## Fix

When there is no current user, the modal closes itself instead of composing `UserAddressView`.

`ModalManager.closeModal` closes the top modal. So the effect closes only when this modal is the top one that is not being removed, using a new `ModalManager.isLastModal(data)`.

The effect is keyed on the modal and on the left-panel modal count. That makes it run again in two cases:
- `AnimatedContent` reuses the same composition for another modal, as happens with a quick second click on the row while the first modal is still animating out.
- This modal becomes the top again, for example after a screen opened from it is closed.

It never closes a different screen, such as one opened just before or just after the address screen during an animation.

An early `return@showCustomModal`, as the "Chat preferences" row does, would avoid the crash. But it would leave an invisible modal on the left-panel stack, with no back button. That modal would come back as the address screen once a profile exists.

## Verification

Desktop AppImages from master and from this branch, each with a fresh database. The v7.1.0-beta.6 CLI acts as the linked mobile (`/connect remote ctrl`). "Leftover" is checked by creating a profile afterwards: a leftover modal reappears as the address screen when onboarding finishes.

- master, case 1: NPE at `UserPicker.kt:223`.
- master, case 2: NPE at `UserPicker.kt:223` with the reported stack (`showCustomModal` 157, `ModalView.kt:218`).
- This branch, case 1 with the address screen on top: no crash, and the screen closes.
- This branch, case 1 with "SimpleX address or 1-time link?" on top: no crash. That screen stays open after the disconnect, as on master. Going back closes the address screen and shows the chat list.
- This branch, case 2, double click 30 ms apart (two runs): no crash, and no leftover. With `LaunchedEffect(Unit)` the address screen reappeared after onboarding in 2 of 3 runs.
- This branch, case 2, "Settings" then "Create SimpleX address" 100 ms apart: no crash, and Settings stays open. With the close keyed on the modal count but without the `isLastModal` check, Settings was closed too.
- This branch, with a local profile: the address screen opens and closes normally. When the mobile disconnects with its address screen open, the screen closes through `UserAddressView`'s own user-change effect, and the app returns to the local profile.
