# User Profiles

## Purpose

Manage multiple chat profiles within a single app instance. Users can create, switch between, hide, mute, and delete profiles. Hidden profiles are protected by password and support a self-destruct password option.

## Route / Navigation

- **Entry point**: Tap user avatar in `ChatListView` toolbar -> `UserPicker` -> "Your chat profiles"
- **Presented by**: `UserPickerSheetView(sheet: .chatProfiles)` wrapping `UserProfilesView` in a `NavigationView`
- **Navigation title**: "Your chat profiles"
- **Sub-navigation**:
  - Create profile -> `CreateProfile`
  - Edit profile -> profile detail view (via `selectedUser`)
  - User address -> `UserAddressView` (via UserPicker `.address` sheet)

## Page Sections

### Search / Password Field

Combined text field at the top (`searchTextOrPassword`):
- In normal mode: Filters visible profiles by name
- For hidden profiles: Acts as password entry to reveal hidden profiles
- Trimmed search text compared against profile names and hidden profile passwords

### Profile List

Each row rendered by `userView()`:

| Element | Description |
|---|---|
| Active indicator | Checkmark or highlighted state for the current active profile |
| Profile image | Avatar circle with profile image or colored initials |
| Display name | Profile's display name |
| Unread count | Badge showing unread message count across all chats for this profile |
| Muted indicator | Bell-slash icon if profile notifications are muted |
| Hidden indicator | Lock icon for hidden profiles (only shown when revealed via password) |

### Profile Actions

Available via tap on a profile row:

| Action | Condition | Description |
|---|---|---|
| Switch active | Different from current | Activates the selected profile; all chats switch context |
| Mute / Unmute | Any profile | Toggle notification muting for the profile; shows alert on first mute (`showMuteProfileAlert`) |
| Hide / Unhide | Non-active profile | Hide with password or reveal a hidden profile |
| Delete | Non-active profile | Delete with confirmation; option to delete data from servers |

### Add Profile Button

| Element | Description |
|---|---|
| "Add profile" label | `Label("Add profile", systemImage: "plus")` |
| Navigation | `NavigationLink` to `CreateProfile` view |
| Auth required | Requires local authentication before creating |

Only shown when `trimmedSearchTextOrPassword` is empty (not searching/entering password).

### Hidden Profile Banner

Shown when `profileHidden` is true (a profile was just hidden):

| Element | Description |
|---|---|
| Lock icon | `lock.open` system image |
| Message | "Enter password above to show!" |
| Tap action | Dismisses the banner with animation |

### Create Profile (`CreateProfile`)

| Field | Description |
|---|---|
| Display name | Required text field with validation (`mkValidName`) |
| Bio | Optional bio text (max 160 bytes) |
| Create button | Disabled until valid name entered and bio within limit |

Validation alerts: `duplicateUserError`, `invalidDisplayNameError`, `createUserError`, `invalidNameError`.

## Profile Visibility

| Visibility | Description |
|---|---|
| Public | Normal profile, always visible in the list |
| Hidden | Protected by password; not shown unless password entered in search field |
| Muted | Notifications suppressed; visual indicator in profile list |

### Hidden Profile Password Management

- Set password when hiding a profile
- Password verified when entering in the search/password field
- `UserProfileAction.unhideUser` requires password entry
- Self-destruct password: Optional secondary password (`DEFAULT_LA_SELF_DESTRUCT`) that wipes all app data when entered

### Delete Profile

Two-stage confirmation:

1. `confirmDeleteUser()` shows initial confirmation
2. `UserProfilesAlert.deleteUser(user:, delSMPQueues:)` with option to delete queues from servers
3. Requires local authentication (`withAuth`) before proceeding

## Loading / Error States

| State | Behavior |
|---|---|
| Authentication required | `authorized` state; prompts biometric/passcode before profile operations |
| Profile switch | Async operation; profile switch errors shown via `activateUserError` alert |
| Delete in progress | Profile removed from list; server queue deletion is async |
| Errors | Alert with localized error title and description |

## Alerts

| Alert | Trigger |
|---|---|
| `deleteUser` | Confirm profile deletion |
| `hiddenProfilesNotice` | First-time hidden profiles explanation (`showHiddenProfilesNotice`) |
| `muteProfileAlert` | First-time mute explanation (`showMuteProfileAlert`) |
| `activateUserError` | Profile switch failure |
| `error` | General error display |

## Related Specs

- `spec/api.md` -- User management API commands (create user, delete user, activate user, hide user)
- `spec/state.md` -- Application state: `chatModel.users`, `chatModel.currentUser`
- [Chat List](chat-list.md) -- Reflects active profile's chats
- [Settings](settings.md) -- Accessed from same UserPicker menu
- [Onboarding](onboarding.md) -- Initial profile creation during first launch

## Source Files

- `Shared/Views/UserSettings/UserProfilesView.swift` -- Main profiles list, search/password, profile actions, delete confirmation
- `Shared/Views/Onboarding/CreateProfile.swift` -- Profile creation form (shared with onboarding and profiles view)
- `Shared/Views/UserSettings/UserAddressView.swift` -- User's SimpleX address management (create, share, delete)
- `Shared/Views/ChatList/UserPicker.swift` -- Profile switcher sheet that navigates to this view
