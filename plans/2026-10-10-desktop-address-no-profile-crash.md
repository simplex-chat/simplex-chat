# Crash in the SimpleX address screen when there is no profile

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

The screen can be shown in that state in three ways:

1. **The mobile disconnects.** The screen was opened for the mobile's profile, and then the mobile disconnects. `switchUIRemoteHost(null)` sets `currentUser` to the local user, which is null, and the left-panel modals stay open.
   - If the address screen is on top, it crashes when it recomposes.
   - If another screen is on top of it, it crashes when the user goes back to it.

   `UserAddressView` closes itself when the user changes (`KeyChangeEffect` on the user), but with a null user it is never reached.
2. **The row is clicked with no active user.** With no profile and no connected mobile (a mobile was linked before, otherwise the desktop shows onboarding), the user picker opens by itself (`App.kt`, `desktopNoUserNoRemote`) and still shows the "Create SimpleX address" row. Clicking it crashes immediately.

   The row is also offered after deleting the active profile when no other visible profile remains, including the only profile. `doRemoveUser` (`UserProfilesView.kt`) then calls `changeActiveUser_` with no user, which sets `currentUser` from `apiGetActiveUser`. After the deletion there is no active user, so it is null.
3. **The self-destruct passcode is entered while the screen is open.** `deleteStorageAndRestart` (`LocalAuthView.kt`) calls `reinitChatController`, which sets `currentUser` from the new empty database to null (`Core.kt`). Only later does it create the new profile and call `closeAllModalsEverywhere`. On Android, the shared modal stack stays composed under the lock screen, so in between the address screen recomposes with a null user. On desktop, `initChatController` replaces the main screen with the splash screen shortly afterwards (`localUserCreated = null`), so the crash needs a frame to fall in that short window.

## Fix

When there is no current user, the modal closes itself instead of composing `UserAddressView`.

`ModalManager.closeModal` closes the top modal. So the effect closes only when this modal is the top one that is not being removed, using a new `ModalManager.isLastModal(data)`.

The effect is keyed on the modal and on the modal count of `ModalManager.start` (the left panel on desktop, the shared stack on Android). So it checks again when the count changes, or when its composition is reused for another modal, while this content is still composed. Other than the top modal, a modal stays composed only while it animates out (250 ms). For example, a quick second click on the row reuses the composition of the first modal, which is still animating out.

A modal that is covered after an animation ends is disposed. It is composed afresh when it becomes the top again, for example when the user goes back to it, and the effect runs then.

With the `isLastModal` check, the effect does not close a different screen, such as one opened just before or just after the address screen during an animation. This holds for stack changes on the main thread; as before, `ModalManager` is not synchronized with background calls such as `closeAllModalsEverywhere`.

Two visible effects when there is no active user:
- Clicking the row now only hides the picker, as opening any modal does. The modal closes itself, and the picker is not reopened.
- After the mobile disconnects with the address screen open, the picker does not reopen by itself (observed in testing). The app checks for open left-panel modals when the user becomes null, and the address modal closes itself only after that check. Master does the same when any other left-panel screen is open.

The change is in common code. On Android the user picker is not reachable without an active user, because deleting the last visible profile returns to onboarding. The only Android path is case 3, where the screen now closes itself instead of crashing. With an active user, behaviour does not change on any platform.

An early `return@showCustomModal`, as the "Chat preferences" row does, would avoid the crash. But it would leave an invisible modal on the left-panel stack, with no back button. It stays until something closes the left-panel modals, such as a click on the centre panel. If a profile is created first, it comes back as the address screen. The "Chat preferences" row has this behaviour on master, and this change leaves it as is.

## Verification

Desktop AppImages from master and from this branch, each with a fresh database. The v7.1.0-beta.6 CLI acts as the linked mobile (`/connect remote ctrl`). "Leftover" is checked by creating a profile afterwards: a leftover modal reappears as the address screen when onboarding finishes.

- master, case 1: NPE at `UserPicker.kt:223`.
- master, case 2: NPE at `UserPicker.kt:223` with the reported stack (`showCustomModal` 157, `ModalView.kt:218`).
- This branch, case 1 with the address screen on top: no crash, and the screen closes.
- This branch, case 1 with the "Address or 1-time link?" screen (opened from "SimpleX address or 1-time link?") on top: no crash. That screen stays open after the disconnect, as on master. Going back closes the address screen and shows the chat list.
- This branch, case 2, double click (seven runs, 8 to 40 ms apart): no crash, and no leftover. With `LaunchedEffect(Unit)` the address screen reappeared after onboarding in 2 of 3 runs at 30 ms.
- This branch, case 2, "Settings" then "Create SimpleX address" 100 ms apart: no crash, and Settings stays open. With the close keyed on the modal count but without the `isLastModal` check, Settings was closed too.
- This branch, with a local profile: the address screen opens and closes normally. When the mobile disconnects with its address screen open, the screen closes through `UserAddressView`'s own user-change effect, and the app returns to the local profile.
