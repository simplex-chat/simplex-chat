# User Profiles

> **Related spec:** [spec/client/navigation.md](../../spec/client/navigation.md)

## Purpose

Manage multiple chat profiles within a single app instance. Users can create, switch between, hide, mute, and delete profiles. Hidden profiles are protected by password. The UserPicker provides quick profile switching from the chat list, while UserProfilesView offers full profile management.

## Route / Navigation

- **Entry point**: Tap user avatar in `ChatListView` toolbar -> `UserPicker` -> "Your chat profiles"
- **Presented by**: `UserProfilesView` composable via `ModalManager.start.showCustomModal` with search bar
- **Navigation title**: "Your chat profiles" (`AppBarTitle`)
- **Sub-navigation**:
  - Create profile -> `CreateProfile` (via `ModalManager.center`)
  - Edit active profile -> `UserProfileView` (via UserPicker tap on active user)
  - User address -> `UserAddressView` (via UserPicker)
  - Chat preferences -> `PreferencesView` (via UserPicker)

## Page Sections

### UserPicker (`UserPicker.kt`)

Overlay panel triggered from `ChatListView` toolbar:

| Section | Description |
|---|---|
| Device picker row | `DevicePickerRow` showing local device and connected remote hosts (Desktop only); pill-shaped buttons with connect/disconnect actions |
| Active user profile | `ProfilePreview` of current user (Desktop: single row; Android: full user list) |
| User list | `UserPickerUsersSection` with all visible non-hidden profiles; tap to switch, long-press disabled |
| SimpleX address | Row to open `UserAddressView` (create or view address) |
| Chat preferences | Row to open `PreferencesView` |
| Chat profiles | Row to open `UserProfilesView` (or `CreateProfile` when no users exist on Desktop) |
| Use from desktop/mobile | Android: "Use from desktop" (`ConnectDesktopView`); Desktop: "Link a mobile" / "Linked mobiles" (`ConnectMobileView`) |
| Settings | Row to open `SettingsView` with `ColorModeSwitcher` trailing |

Platform behavior:
- **Android**: `PlatformUserPicker` renders as bottom sheet with `AnimatedViewState` transitions; shows all users inline
- **Desktop**: Sidebar panel; shows only active user in header, inactive users in separate section below divider

### UserProfilesView

Full profile management screen with search/password field:

#### Search / Password Field

Combined text field at the top (`searchTextOrPassword`):
- In normal mode: Filters visible profiles by name
- For hidden profiles: Acts as password entry to reveal hidden profiles
- Trimmed search text compared against `user.anyNameContains()` and `correctPassword()`

#### Profile List

Each row rendered by `UserView` -> `UserProfilePickerItem`:

| Element | Description |
|---|---|
| Active indicator | Checkmark icon (`ic_done_filled`) for the current active profile |
| Profile image | 54dp avatar with `fontSizeSqrtMultiplier` scaling |
| Display name | Profile's display name; bold for active, normal for inactive |
| Unread count | Badge showing unread message count (`unreadCountStr`) with primary/secondary color based on mute state |
| Muted indicator | `ic_notifications_off` icon when profile notifications are muted |
| Hidden indicator | `ic_lock` icon for hidden profiles (only shown when revealed via password) |

#### Profile Actions (Context Menu)

Available via long-press / right-click on a profile row (`DefaultDropdownMenu`):

| Action | Condition | Description |
|---|---|---|
| Switch active | Different from current | `changeActiveUser()` activates the selected profile; all chats switch context |
| Mute | Visible, notifications on | `apiMuteUser()` mutes notifications; shows `showMuteProfileAlert` on first use |
| Unmute | Visible, notifications off | `apiUnmuteUser()` restores notifications |
| Hide | Visible, not active, multiple visible users | Opens `HiddenProfileView` to set password |
| Unhide | Hidden profile | `apiUnhideUser()` with password entry (`ProfileActionView` with `UserProfileAction.UNHIDE`) |
| Delete | Any non-sole profile | Delete with confirmation dialog; options: "Delete with connections" (removes SMP queues) or "Delete data only" |

#### Add Profile

| Element | Description |
|---|---|
| Add button | "+" icon with "Add profile" text at bottom of list (hidden when searching) |
| Auth required | Profile creation requires authentication via `withAuth` |
| Create view | Opens `CreateProfile` in `ModalManager.center` |

#### Profile Deletion (`removeUser`)

Deletion flow:
1. If hidden profile requiring password: opens `ProfileActionView` with `UserProfileAction.DELETE`
2. If active profile: switches to another visible user first via `changeActiveUser_`, then deletes
3. If last visible profile with hidden profiles: deletes user, then changes active to null; on Android, stops chat and resets to onboarding
4. Cleans up wallpaper files and cancels notifications for the deleted user

#### Hidden Profile Notice

Shown once via `showHiddenProfilesNotice` preference:

| Element | Description |
|---|---|
| Alert title | "Make profile private" |
| Alert text | "You can hide or mute user profile" |
| "Don't show again" | Disables the notice permanently |

### Profile Password Validation

| Function | Description |
|---|---|
| `correctPassword()` | Validates password against `user.viewPwdHash` using `chatPasswordHash(pwd, salt)` |
| `passwordEntryRequired()` | Returns true if user is hidden, active, and password does not match current search text |
| `userViewPassword()` | Extracts view password from search text for hidden user operations |

## Source Files

| File | Path |
|---|---|
| `UserProfilesView.kt` | `views/usersettings/UserProfilesView.kt` |
| `UserPicker.kt` | `views/chatlist/UserPicker.kt` |
