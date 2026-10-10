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

The "SimpleX address" row of the user picker opens `UserAddressView` with `shareViaProfile = it.currentUser.value!!.addressShared`. The modal content reads `currentUser`, so it recomposes whenever `currentUser` changes, and it throws whenever it is composed while `currentUser` is null.

On desktop, `currentUser` is null when the active side (this device or the connected mobile) has no visible profile, for example a desktop that was only used with a linked mobile. The screen can be shown in that state in two ways:

1. **The mobile disconnects.** The screen was opened for the mobile's profile, and then the mobile disconnects. `switchUIRemoteHost(null)` sets `currentUser` to the local user, which is null, and the left-panel modals stay open. If the address screen is on top, it crashes when it recomposes. If another screen is on top of it, it crashes when the user goes back to it.
2. **The row is clicked with no profile.** With no profile and no mobile, the user picker opens by itself (`App.kt`, `desktopNoUserNoRemote`) and still shows the "Create SimpleX address" row. Clicking it crashes immediately. The same applies after deleting the last visible profile while hidden ones remain: desktop then sets `currentUser` from `apiGetActiveUser` (`UserProfilesView.kt`, `doRemoveUser`), and the row is still offered.

The reported frames (`showCustomModal` 157, `ModalView.kt:218`) show the screen being composed fresh, not recomposed in place. That matches case 2, or going back to the screen in case 1.

## Fix

When there is no current user, the modal closes itself instead of composing `UserAddressView`.

`close` closes the top modal of the left panel. The effect is keyed on the modal and on the left-panel modal count, so it also runs again in two cases:
- `AnimatedContent` reuses the same composition for another modal, as with a quick second click on the row while the first modal is still animating out.
- The stack changes while this content is still composed. For example, a screen opened from the address screen is still animating in when the mobile disconnects. Both are then closed instead of leaving the address modal open and invisible.

An early `return@showCustomModal`, as the "Chat preferences" row does, would avoid the crash. But it would leave an invisible modal on the left-panel stack, with no back button. That modal would come back as the address screen once a profile exists.

## Verification

Desktop AppImages from master and from this branch, each with a fresh database. The v7.1.0-beta.6 CLI acts as the linked mobile (`/connect remote ctrl`). "Leftover" is checked by creating a profile afterwards: a leftover modal reappears as the address screen when onboarding finishes.

- master, case 1: NPE at `UserPicker.kt:223`.
- master, case 2: NPE at `UserPicker.kt:223` with the reported stack (`showCustomModal` 157, `ModalView.kt:218`).
- This branch, case 1 with the address screen on top: no crash, and the screen closes.
- This branch, case 1 with "SimpleX address or 1-time link?" on top: no crash. Going back closes the address screen and shows the chat list.
- This branch, case 2, single click: no crash, and no leftover.
- This branch, case 2, double click 30 ms apart: no crash, and no leftover. With `LaunchedEffect(Unit)` the address screen reappeared after onboarding.
- This branch, with a local profile: the address screen opens and closes normally. When the mobile disconnects with its address screen open, the screen closes and the app returns to the local profile.
