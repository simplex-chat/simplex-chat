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

## Channel Adaptations

When `groupInfo.useRelays == true`, the group info view adapts to channel semantics. All sections below describe differences from the standard group behavior above.

### Channel Info Layout

The top section splits into a channel-specific branch:

| Element | Owner | Non-owner |
|---|---|---|
| Channel link | NavigationLink "Channel link" to `GroupLinkView` | Inline QR code (`SimpleXLinkQRCode`) + "Share link" button (if `groupProfile.publicGroup?.groupLink` exists) |
| Members | NavigationLink "Owners & subscribers" to `ChannelMembersView` | NavigationLink "Owners" to `ChannelMembersView` |
| Relays | NavigationLink "Chat relays" to `ChannelRelaysView` | NavigationLink "Chat relays" to `ChannelRelaysView` |

### Channel Action Bar

| Button | Channel behavior |
|---|---|
| Link button | Replaces "Add members" for channel owners; navigates to `GroupLinkView` |
| Add members | Hidden for channels |

### Hidden Sections for Channels

The following are hidden when `groupInfo.useRelays == true`:

- Group preferences button and footer
- Send receipts toggle
- Member list section (replaced by ChannelMembersView navigation)
- Non-admin block section (in GroupMemberInfoView)

### Channel Leave/Delete Rules

- Sole channel owner cannot leave (button hidden when `isOwner && no other owners`)
- "Leave group" -> "Leave channel"; "Delete group" -> "Delete channel"; "Edit group profile" -> "Edit channel profile"
- `deleteGroupAlert`: "Delete channel?" / "Channel will be deleted for all subscribers - this cannot be undone!" (current member) or "Channel will be deleted for you - this cannot be undone!" (non-current member)
- `leaveGroupAlert`: "Leave channel?" / "You will stop receiving messages from this channel. Chat history will be preserved."
- `showRemoveMemberAlert`: "Remove subscriber?" / "Subscriber will be removed from channel - this cannot be undone!"

### Channel Members View (`ChannelMembersView`)

New view accessible from channel info, showing:

| Section | Content | Visibility |
|---|---|---|
| Owners | Members with role >= `.owner`, plus current user if owner | Always |
| Subscribers | Members with role < `.owner` and != `.relay` | Owner only |

- Excludes `memLeft`, `memRemoved`, and current user from member list
- Each row: profile image, verified badge, name; taps navigate to `GroupMemberInfoView`
- Empty state: "No subscribers" when subscriber list is empty

### Channel Relays View (`ChannelRelaysView`)

New view accessible from channel info, showing relay members (role == `.relay`):

| Element | Description |
|---|---|
| Relay list | Filtered from `chatModel.groupMembers` by `.relay` role |
| Relay row | Profile image, relay display name, status text (`RelayStatus` or connection status) |
| Relay tap | NavigationLink to `GroupMemberInfoView` with `groupRelay:` parameter |
| Empty state | "No chat relays" |
| Footer | "Chat relays forward messages to channel subscribers." |

Owner sees relay status from `apiGetGroupRelays`; non-owner sees connection status only.

### Channel Link View (`GroupLinkView` with `isChannel: true`)

| Change | Channel behavior |
|---|---|
| Title | "Channel link" (not "Group link") |
| Description | "Anybody will be able to join the channel" (omits "You won't lose members...") |
| Initial role picker | Hidden |
| Upgrade link button | Hidden |
| Delete link button | Hidden (channel link deletion only via channel deletion) |
| Short/full link toggle | Hidden |
| Share button | Shares directly (no upgrade-and-share alert) |

### Channel Member Info (`GroupMemberInfoView` adaptations)

| Change | Channel behavior |
|---|---|
| Section header | "Relay" / "Owner" / "Subscriber" (based on member role) instead of "Member" |
| Group label | "Channel" instead of "Group" / "Chat" |
| Action buttons | Hidden (message/audio/video/search) |
| Role change picker | Hidden |
| Verify code button | Hidden for relay members |
| Block section | Hidden for non-moderator users |
| Remove button | Hidden for relay members |
| "Remove member" label | "Remove subscriber" |
| "Block for all?" alert | "Block subscriber for all?" |
| "Unblock for all?" alert | "Unblock subscriber for all?" |
| Relay link info row | Shown when `member.relayLink` exists, displays `hostFromRelayLink(link)` |
| Relay address info row | Shown when `groupRelay?.userChatRelay.address` exists, with "Share relay address" button |
| Relay footer | Owner: "Subscribers use relay link to connect to the channel. Relay address was used to set up this relay for the channel." Non-owner: "You connected to the channel via this relay link." |

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
- `Shared/Views/Chat/Group/ChannelMembersView.swift` -- Channel owners/subscribers list
- `Shared/Views/Chat/Group/ChannelRelaysView.swift` -- Channel relay status list
