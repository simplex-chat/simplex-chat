# Hide the address and preferences rows when there is no profile and no mobile in use

## Problem

On a desktop with no active profile and no mobile in use, the user picker shows "Create SimpleX address" as its first option row, then "Chat preferences" and, below a divider, "Create chat profile". The picker opens by itself in this state when no left-panel screen is open (`App.kt`). Neither of the first two rows can work without a profile:

- "Create SimpleX address" crashed with a NullPointerException at `UserPicker.kt:223` (`:216` in v6.1, `:222` in v6.2–v6.4) before #7679. GitHub reports from using the row: #5068, #5476, #6194, #7067 (Linux; comments on #5068 and #6194 report the same on Windows 11).
- "Chat preferences" left an invisible modal with no back button.
- With #7679 both screens close themselves instead, so a click only hides the picker and nothing opens.

Users have told the team that the crash stops them from creating a profile. On v7.0.4 a profile can still be created from "Create chat profile" after the crash, but the first row the user tries is one that cannot work.

The same crash also happens when a mobile disconnects while its address screen is open (#6452, macOS: the same stack, reported alongside phone disconnects). That path is fixed by #7679, not by this change.

## Solution summary

Hide the "SimpleX address" and "Chat preferences" rows when the desktop has no active user and no mobile in use (`chatModel.desktopNoUserNoRemote`). The divider below them is hidden with them. This is the state in which the picker already shows "Create chat profile" instead of "Your chat profiles". While a connected mobile is in use, both rows stay and act on the mobile's profile; if the mobile has no active profile, the screens close themselves (#7679).

Before the new user picker (#4796), `SettingsView` showed both rows only inside `if (profile != null)`, with "Create chat profile" in the `else if (chatModel.localUserCreated.value == false)` branch.

## Technical design

`SecondSection` in `UserPicker.kt` emits both rows inside `if (!chatModel.desktopNoUserNoRemote)`. The desktop divider below them belongs to the same group and is drawn under the same condition. The composable getter reads `currentUser` and `currentRemoteHost` state, so the picker updates as soon as a profile is created, a mobile is connected, selected or disconnected, or the desktop is switched to "This device" with no profile. On Android `desktopNoUserNoRemote` is always false, so nothing changes there.

The self-closing modals from #7679 stay. Hiding the rows only removes the entry point in that state. A screen shown with no active user in any other state still relies on #7679's handling; #7679's plan lists the paths. For example:
- a screen opened while there was a user, which then loses it: a mobile disconnects, or the self-destruct passcode is entered, with the address or preferences screen open;
- a mobile in use with no active profile (its last visible profile deleted): the rows are shown, and the screens close themselves.

### Security

UI-only: it hides two rows and the divider below them, based on local state. No data or trust boundary is involved.

## Implementation steps

1. `views/chatlist/UserPicker.kt`: in `SecondSection`, wrap the "SimpleX address" and "Chat preferences" rows in `if (!chatModel.desktopNoUserNoRemote)`, and draw the desktop divider below them under the same condition.
2. Documentation, under `apps/multiplatform/`:
   - `spec/client/chat-list.md`, UserPicker section: when the two rows are hidden.
   - `product/views/user-profiles.md`: when both rows are hidden.

## Verification

Desktop AppImage of this branch, fresh database, the v7.1.0-beta.6 CLI as the linked mobile; Android debug APK of this branch on Waydroid.

- Mobile connected, no desktop profile: both rows are shown for the mobile's profile. "Create SimpleX address" creates the address on the mobile (`/show_address` on the CLI returns it).
- Mobile connected, switched to "This device" (no profile): both rows are hidden and "Create chat profile" is shown. Switching back to the mobile shows them again.
- Address screen open, then the mobile disconnects: the screen closes (#7679), no crash, and the picker then shows no address or preferences rows.
- "Create chat profile" from that state: the profile is created, both rows appear and open their screens.
- No profile, mobile linked but not in use: the device row, then "Create chat profile", with no divider. After deleting the linked mobile ("Linked mobiles", right-click, "Delete"): "Create chat profile" is the first row. The spacing matches the gap between rows. After creating a profile, both rows and the divider are back.
- Android, with a profile: both rows are shown and open their screens; no crash in logcat.
