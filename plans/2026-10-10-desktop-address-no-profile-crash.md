# Crash in the SimpleX address screen when there is no profile

## Problem

A Windows user who uses the desktop app only through a linked mobile reported this crash:

```
java.lang.NullPointerException
  at chat.simplex.common.views.chatlist.ComposableSingletons$UserPickerKt$lambda$-1608374373$1.invoke(UserPicker.kt:223)
  at chat.simplex.common.views.chatlist.UserPickerKt$UserPicker$6$showCustomModal$1$1$1.invoke(UserPicker.kt:157)
  at chat.simplex.common.views.helpers.ModalManager$showInView$4$1$1.invoke(ModalView.kt:218)
```

The "SimpleX address" screen crashes whenever it is shown while there is no active user.

## Solution summary

When there is no current user, the address modal closes itself instead of composing `UserAddressView`. It closes only while it is the top modal, so it does not close a different screen (for stack changes on the main thread). The new `ModalManager.isLastModal` checks that.

## Technical design

### Cause

The "SimpleX address" row of the user picker opens `UserAddressView` with `shareViaProfile = it.currentUser.value!!.addressShared`. The modal content reads `currentUser`, so it recomposes whenever `currentUser` changes. It throws whenever it is composed while `currentUser` is null, that is, when there is no active user. A desktop that was only used with a linked mobile has no active user of its own.

The screen can be shown in that state in two ways:

1. **The mobile disconnects.** The screen was opened for the mobile's profile, and then the mobile disconnects. `switchUIRemoteHost(null)` sets `currentUser` to the local user, which is null, and the left-panel modals stay open.
   - If the address screen is on top, it crashes when it recomposes.
   - If another screen is on top of it, it crashes when the user goes back to it.

   `UserAddressView` closes itself when the user changes (`KeyChangeEffect` on the user), but with a null user it is never reached.
2. **The row is clicked with no active user.** With no active profile, for example with no profile and no connected mobile, the user picker still shows the "Create SimpleX address" row. With no profile and no connected mobile, the picker even opens by itself, at startup or when the state begins while no left-panel screen is open (`App.kt`, `desktopNoUserNoRemote`). Clicking the row crashes immediately.

   At startup, no profile and no connected mobile needs a mobile that was linked before; with no linked mobile, startup shows onboarding instead (`Core.kt`).

   No active user also follows from deleting the active profile when no other visible profile remains, including the only profile, and including the last visible profile of a connected mobile. `doRemoveUser` (`UserProfilesView.kt`) then calls `changeActiveUser_` with no user, which sets `currentUser` from `apiGetActiveUser`. After the deletion there is no active user, so it is null.

A third path is possible but not confirmed: entering the self-destruct passcode while the screen is open. `deleteStorageAndRestart` (`LocalAuthView.kt`) calls `reinitChatController`. That calls `initChatController`, which sets `currentUser` from the new empty database to null (`Core.kt`). Only later does `deleteStorageAndRestart` create the new profile and call `closeAllModalsEverywhere`.
- On Android, the shared modal stack stays composed under the lock screen, so the address screen would recompose with a null user. Android's main loop catches exceptions (`SimplexApp.kt`). `GlobalExceptionsHandler` (`UI.android.kt`) then closes the top modal, recreates the activity and posts a crash alert, and the self-destruct flow hides all alerts at its end. A test on an Android build of master showed no visible crash. That is consistent with this recovery, but it does not show whether the exception was thrown; logcat was not checked.
- On desktop, `initChatController` replaces the main screen with the splash screen shortly afterwards (`localUserCreated = null`), so the crash needs a frame to fall in that short window. This was not tested.

### Self-closing modal

`ModalManager.closeModal` closes the top modal that is not already being removed, whichever modal calls it. So the effect closes only when `ModalManager.isLastModal(data)` reports that this modal is that one.

The effect is keyed on the modal and on the modal count of `ModalManager.start` (the left panel on desktop, the shared stack on Android). So it checks again when the count changes, or when its composition is reused for another modal, while this content is still composed. Other than the top modal, a modal stays composed only while it animates out (250 ms, longer if the animation is interrupted). For example, a quick second click on the row reuses the composition of the first modal, which is still animating out.

A modal that is covered after an animation ends is disposed. It is composed afresh when it becomes the top again, for example when the user goes back to it, and the effect runs then.

With the `isLastModal` check, the effect does not close a different screen, such as one opened just before or just after the address screen during an animation. This holds for stack changes on the main thread. As before, `ModalManager` is not synchronized with background calls such as `closeAllModalsEverywhere`; see GAP-09.

### Visible behaviour

When there is no active user:
- Clicking the row now only hides the picker, as opening any modal does. The modal closes itself, and the picker is not reopened.
- After the mobile disconnects with the address screen open, the picker does not reopen by itself (observed in testing). When the state with no user and no connected mobile begins, the app checks for open left-panel modals, and the address modal closes itself only after that check. Master does the same when any other left-panel screen is open.

The change is in common code. On Android the user picker is not reachable without an active user, because deleting the last visible profile returns to onboarding. If the self-destruct path above does throw on Android, the branch closes the screen itself instead of recovering through `GlobalExceptionsHandler`. With an active user, behaviour does not change on any platform.

### Security

The change is UI-only and crosses no trust boundary: it reads local state and closes a local screen. It sends nothing and changes no stored data.

### Rejected alternative

An early `return@showCustomModal`, as the "Chat preferences" row does, would avoid the crash. But it would leave an invisible modal on the left-panel stack, with no back button. It stays until something closes the left-panel modals, such as a click on the centre panel. If a profile is created first, it comes back as the address screen. The "Chat preferences" row has this behaviour on master, and this change leaves it as is; see GAP-08.

## Implementation steps

1. `views/helpers/ModalView.kt`: add `ModalManager.isLastModal(data)`, true when `data` belongs to the top modal that is not being removed.
2. `views/chatlist/UserPicker.kt`: in the "SimpleX address" row, compose `UserAddressView` only when `currentUser` is not null. Otherwise run `LaunchedEffect(modal, ModalManager.start.modalCount.value)`, which closes the modal when `isLastModal(modal)` is true.
3. Documentation, under `apps/multiplatform/`:
   - `spec/client/navigation.md`: list `isLastModal` among the `ModalManager` methods, add a `[GAP]` note for GAP-09, link the `ModalManager` location, and update stale line links and line numbers.
   - `spec/client/chat-list.md`: in the UserPicker section, describe the address row closing itself with no active user and add a `[GAP]` note for GAP-08; elsewhere, update stale line links and line numbers.
   - `spec/README.md`: update the number of known gaps.
   - `product/views/user-profiles.md`: describe the address row with no active profile.
   - `product/gaps.md`: add GAP-08 ("Chat preferences" leaves an invisible modal with no active user) and GAP-09 (`ModalManager` is not fully synchronized with background threads).

## Verification

Desktop AppImages from master and from this branch, each with a fresh database, unless stated otherwise. The v7.1.0-beta.6 CLI acts as the linked mobile (`/connect remote ctrl`). "Leftover" is checked by creating a profile afterwards: a leftover modal reappears as the address screen when onboarding finishes.

- master, case 1: NPE at `UserPicker.kt:223`.
- master, case 2: NPE at `UserPicker.kt:223` with the reported stack (`showCustomModal` 157, `ModalView.kt:218`).
- This branch, case 1 with the address screen on top: no crash, and the screen closes.
- This branch, case 1 with the "Address or 1-time link?" screen (opened from "SimpleX address or 1-time link?") on top: no crash. That screen stays open after the disconnect, as on master. Going back closes the address screen and shows the chat list.
- This branch, case 2, double click (seven runs, 8 to 40 ms apart): no crash, and no leftover. With `LaunchedEffect(Unit)` the address screen reappeared after onboarding in 2 of 3 runs at 30 ms.
- This branch, case 2, "Settings" then "Create SimpleX address" 100 ms apart: no crash, and Settings stays open. With the close keyed on the modal count but without the `isLastModal` check, Settings was closed too.
- This branch, with a local profile: the address screen opens and closes normally. When the mobile disconnects with its address screen open, the screen closes through `UserAddressView`'s own user-change effect, and the app returns to the local profile.
- Android build of master (not of this branch), self-destruct passcode entered with the address screen opened from the user picker: no visible crash; logcat not checked.
