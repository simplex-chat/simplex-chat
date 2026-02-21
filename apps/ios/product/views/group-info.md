# Group Chat Info

> **Related spec:** [spec/client/chat-view.md](../../spec/client/chat-view.md)

## Purpose

View and manage group settings, member list, group preferences, group links, member admission, welcome messages, and moderation features. The scope of available actions depends on the user's role within the group (member, moderator, admin, owner).

## Route / Navigation

- **Entry point**: Tap the info button in `ChatView` navigation bar (when viewing a group chat)
- **Presented by**: `NavigationView` sheet from `ChatView` via `showChatInfoSheet`
- **Sub-navigation**:
  - Edit group profile -> `GroupProfileView`
  - Add members -> `AddGroupMembersView`
  - Group link -> `GroupLinkView`
  - Group preferences -> `GroupPreferencesView` (via `GroupPreferencesButton`)
  - Welcome message -> `GroupWelcomeView`
  - Member info -> `GroupMemberInfoView`
  - Chat wallpaper -> `ChatWallpaperEditorSheet`
  - Member support -> `MemberSupportView`
  - Group reports -> `GroupReportsChatNavLink`

## Page Sections

### Group Info Header

| Element | Description |
|---|---|
| Group image | Large circular profile image |
| Group name | Display name (editable by owners) |
| Member count | "N members" label |
| Full name | Optional secondary name |
| Description | Group description text (if set) |

### Local Alias

Editable text field for a local-only alias (not shared with other members). Focused via `aliasTextFieldFocused`.

### Action Buttons

Horizontal row of action buttons:

| Button | Description |
|---|---|
| Search | Triggers `onSearch` callback to search messages in chat |
| Mute/Unmute | Toggle notification mode (`nextNtfMode`) |

### Group Management Section

| Element | Condition | Description |
|---|---|---|
| Group link | `canAddMembers` and not business chat | Navigate to `GroupLinkView` to create/manage invitation link |
| Member support | Not business chat, role >= moderator | Navigate to member support chat view |
| Group reports | `canModerate` | Navigate to group reports chat |
| User support chat | Member active, role < moderator or has support chat | Navigate to own support chat with moderators |

### Group Profile Section

| Element | Condition | Description |
|---|---|---|
| Edit group | Owner, not business chat | Navigate to `GroupProfileView` for editing name, image, description |
| Welcome message | Has description or is owner (not business) | Navigate to `GroupWelcomeView` for add/edit |
| Group preferences | Always | Navigate to `GroupPreferencesView` -- timed messages, reactions, voice, files, direct messages, history visibility |

Footer: "Only group owners can change group preferences." (or "Only chat owners can change preferences." for business chats)

### Chat Settings Section

| Element | Description |
|---|---|
| Send receipts | Toggle delivery receipts; disabled for groups > 20 current members with explanation |
| Chat theme | Navigate to `ChatWallpaperEditorSheet` |
| Chat TTL | `ChatTTLOption` -- set auto-deletion timer for messages on device |

Footer: "Delete chat messages from your device."

### Member List Section

Header shows total member count (e.g., "25 members").

| Element | Description |
|---|---|
| Invite members button | Shown if `canAddMembers`; disabled with tap alert if incognito |
| Search field | Filter members by name (`searchText`) |
| Member rows | Each shows: avatar, display name, role badge (owner/admin/moderator/observer), online status indicator, connection status |
| Member tap | Navigates to `GroupMemberInfoView` |
| Member swipe actions | Block/unblock member, block/unblock for all (moderators) |

Member list is sorted by role (owners first) and filtered to exclude `memLeft` and `memRemoved` statuses.

### Danger Zone Section

| Action | Description |
|---|---|
| Clear chat | Deletes all messages locally (with confirmation alert) |
| Leave group | Leave the group (with confirmation alert) |
| Delete group | Delete entire group -- only for owners (with confirmation alert) |

### Developer Section

Shown when `developerTools` is enabled:

| Element | Description |
|---|---|
| Local name | Internal chat local display name |
| Database ID | API entity ID |

## Loading / Error States

| State | Behavior |
|---|---|
| Loading members | Member list populated from `chatModel.groupMembers` |
| Progress indicator | `ProgressView` overlay when `progressIndicator` is true (during TTL changes) |
| Large group receipts | Receipts option disabled with "Disabled for large groups" label and info alert |
| Incognito invite blocked | Alert: "Can't invite contacts when incognito" |
| Errors | Alert with localized title and error description |

## Alerts

| Alert | Trigger |
|---|---|
| `deleteGroupAlert` | Tap delete group |
| `clearChatAlert` | Tap clear chat |
| `leaveGroupAlert` | Tap leave group |
| `cantInviteIncognitoAlert` | Tap invite members while incognito |
| `largeGroupReceiptsDisabled` | Tap receipts info on large group |
| `blockMemberAlert` / `unblockMemberAlert` | Block/unblock member actions |
| `blockForAllAlert` / `unblockForAllAlert` | Moderator block/unblock for all members |

## Related Specs

- `spec/api.md` -- Group API commands (create, update, add/remove members, roles, links)
- [Chat](chat.md) -- Parent chat view
- [Contact Info](contact-info.md) -- Similar pattern for direct contact info

## Source Files

- `Shared/Views/Chat/Group/GroupChatInfoView.swift` -- Main group info view with all sections
- `Shared/Views/Chat/Group/GroupProfileView.swift` -- Edit group name, image, description
- `Shared/Views/Chat/Group/AddGroupMembersView.swift` -- Member invitation view
- `Shared/Views/Chat/Group/GroupLinkView.swift` -- Group link creation and management
- `Shared/Views/Chat/Group/GroupPreferencesView.swift` -- Group feature preferences
- `Shared/Views/Chat/Group/GroupWelcomeView.swift` -- Welcome message editor
- `Shared/Views/Chat/Group/MemberAdmissionView.swift` -- Member admission policy settings
- `Shared/Views/Chat/Group/GroupMemberInfoView.swift` -- Individual member info and actions
- `Shared/Views/Chat/Group/GroupMentions.swift` -- @mention support in groups
