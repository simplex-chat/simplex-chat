# Desktop: close left-panel screens when switching between a linked mobile and this device

## Problem

Screens opened in the left panel for one profile stay open after the desktop switches to another device. If a linked mobile disconnects while its "SimpleX address" screen is open, that screen stays on screen for the desktop's own profile. If the desktop has no profile of its own, the screen crashes with an NPE at `UserPicker.kt:223`.

## Cause

`switchUIRemoteHost` clears the selected chat and closes the center and end modals, but not `ModalManager.start`. `RemoteHostConnected` already calls `ModalManager.start.closeModals()` before `switchUIRemoteHost`, and switching hosts from the user picker calls `closeAllModalsEverywhere()`. The other callers do neither:
- the `RemoteHostStopped` event, raised when the mobile disconnects;
- "This device" and switching to a connected mobile in "Linked mobiles";
- disconnecting from the user picker;
- creating a profile with no local profile.

## Fix

Close `ModalManager.start` in `switchUIRemoteHost` together with the center and end modals.

As a result, "Linked mobiles" now also closes when "This device" or a connected mobile is chosen in it. Connecting a mobile already closes it.

The crash for the "Create SimpleX address" row when there is no profile is fixed separately.

## Verification

Desktop AppImages from master and from this branch, each with a fresh database. The v7.1.0-beta.6 CLI acts as the linked mobile.

- master: the mobile disconnects with its address screen open, and the app crashes with the NPE at `UserPicker.kt:223`.
- This branch, no crash in each of these, and the left panel is closed after the disconnect:
  - the mobile disconnects with its address screen open;
  - the mobile disconnects with another screen on top of the address screen.
- Branch combined with the fix for the no-profile crash, with no crashes in any case:
  - linking from onboarding and reconnecting from "Linked mobiles";
  - switching with the "This device" and mobile chips;
  - disconnecting from the user picker;
  - creating a profile after the disconnect, then finishing onboarding; the new profile's address screen opens;
  - with a local profile: "This device" and the connected mobile in "Linked mobiles";
  - with a local profile, the mobile disconnects with its address screen open, and the app returns to the local profile with the screen closed.
