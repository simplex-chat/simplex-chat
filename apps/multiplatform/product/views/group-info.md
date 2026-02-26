# Group Chat Info

> **Related spec:** [spec/client/chat-view.md](../../spec/client/chat-view.md)

## Purpose

View and manage group settings, member list, group preferences, group links, member admission, welcome messages, and moderation features. The scope of available actions depends on the user's role within the group (member, moderator, admin, owner).

## Route / Navigation

- **Entry point**: Tap the info button in `ChatView` navigation bar (when viewing a group chat)
- **Presented by**: `GroupChatInfoView` composable shown via `ModalManager.end` from `ChatView`
- **Sub-navigation**:
  - Edit group profile -> `GroupProfileView` (via `ModalManager.end`)
  - Add members -> `AddGroupMembersView` (via `ModalManager.end`)
  - Group link -> `GroupLinkView` (via `ModalManager.end`)
  - Group preferences -> `GroupPreferencesView` (via `ModalManager.end`)
  - Welcome message -> `GroupWelcomeView` (via `ModalManager.end`)
  - Member info -> `GroupMemberInfoView` (via `ModalManager.end`)
  - Chat wallpaper -> wallpaper editor
  - Member support -> `MemberSupportView` (via `ModalManager.end`)

## Page Sections

### Group Info Header

| Element | Description |
|---|---|
| Group image | Large circular profile image |
| Group name | Display name (editable by owners via `GroupProfileView`) |
| Member count | "N members" label from `activeSortedMembers` |
| Full name | Optional secondary name |
| Description | Group description text (if set) |

### Local Alias

Editable text field for a local-only alias (not shared with other members). Changes saved via `setGroupAlias()`.

### Action Buttons

Horizontal row of action buttons:

| Button | Description |
|---|---|
| Search | Triggers `onSearchClicked` callback to search messages in chat |
| Mute/Unmute | Toggle notification mode |
| Add members | Opens `AddGroupMembersView` (shown when user has admin+ role and `groupInfo.canAddMembers`) |

### Group Management Section

Available actions depend on role (`GroupMemberRole`):

| Action | Minimum Role | Description |
|---|---|---|
| Edit group profile | Owner | Opens `GroupProfileView` to edit name, image, description |
| Add members | Admin | Opens `AddGroupMembersView` to invite contacts |
| Manage group link | Admin | Opens `GroupLinkView` to create/share/delete group link |
| Member support | Moderator | Opens `MemberSupportView` to manage member support chats |
| Edit welcome message | Owner | Opens `GroupWelcomeView` to set the auto-sent welcome text |
| Group preferences | Any | Opens `GroupPreferencesView` (read-only; only owners can change settings) |

### Chat Preferences

| Setting | Description |
|---|---|
| Send receipts | Per-group delivery receipt setting (`SendReceipts`); limited to groups under `SMALL_GROUPS_RCPS_MEM_LIMIT` (20 members) |
| Chat item TTL | Per-group message retention setting with confirmation alert via `setChatTTLAlert` |

### Member List

Displays `activeSortedMembers` (excluding left/removed members, sorted by role descending):

| Element | Description |
|---|---|
| Member avatar | `MEMBER_ROW_AVATAR_SIZE` (42dp) profile image |
| Member name | Display name with role badge |
| Member role | Owner, Admin, Moderator, Member, Observer |
| Member status | Active, connecting, pending, left, removed |
| Tap action | Opens `GroupMemberInfoView` with connection stats and verification code |

### Group Link (`GroupLinkView`)

| Element | Description |
|---|---|
| Create link button | `apiCreateGroupLink` generates a shareable group invitation link |
| QR code display | QR code rendering of the group link |
| Short link toggle | Switch between short and full link display |
| Share button | System share for the link |
| Copy button | Copy link to clipboard |
| Member role selector | Set the default role for members joining via link (`acceptMemberRole`) |
| Add short link | `apiAddGroupShortLink` creates a short link that includes group profile |
| Delete link | Remove the group link with confirmation |

### Add Members (`AddGroupMembersView`)

| Element | Description |
|---|---|
| Contact list | Filterable list of contacts to invite |
| Role selector | Set the role for invited members |
| Invite button | Sends group invitations to selected contacts |
| Group link option | Alternative to direct invitation |

### Group Member Info (`GroupMemberInfoView`)

| Element | Description |
|---|---|
| Member profile | Avatar, name, role |
| Connection stats | Server information, connection status |
| Security code | Verification code for the member connection |
| Role change | Change member role (admin+ only) |
| Remove member | Remove from group (admin+ only) |
| Block member | Block member for self |
| Direct message | Open direct chat with member |

### Developer Tools Section

Shown when `developerTools` preference is enabled:

| Element | Description |
|---|---|
| Database ID | Group's internal database identifier |

### Destructive Actions

| Action | Condition | Description |
|---|---|---|
| Clear chat | Any member | Deletes all messages locally (`clearChatDialog`) |
| Leave group | Non-owner | Leave the group (`leaveGroupDialog`) |
| Delete group | Owner or non-current member | Delete group for all (owner) or for self (`deleteGroupDialog`) |

Business chats use alternative labels: "Delete chat" instead of "Delete group".

## Source Files

| File | Path |
|---|---|
| `GroupChatInfoView.kt` | `views/chat/group/GroupChatInfoView.kt` |
| `GroupMemberInfoView.kt` | `views/chat/group/GroupMemberInfoView.kt` |
| `AddGroupMembersView.kt` | `views/chat/group/AddGroupMembersView.kt` |
| `GroupLinkView.kt` | `views/chat/group/GroupLinkView.kt` |
| `GroupProfileView.kt` | `views/chat/group/GroupProfileView.kt` |
| `GroupPreferences.kt` | `views/chat/group/GroupPreferences.kt` |
| `WelcomeMessageView.kt` | `views/chat/group/WelcomeMessageView.kt` |
| `MemberAdmission.kt` | `views/chat/group/MemberAdmission.kt` |
| `MemberSupportView.kt` | `views/chat/group/MemberSupportView.kt` |
