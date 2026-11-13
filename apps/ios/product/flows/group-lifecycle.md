# Group Lifecycle Flow

> **Related spec:** [spec/api.md](../../spec/api.md) | [spec/database.md](../../spec/database.md)

## Overview

Complete group management in SimpleX Chat iOS: creating groups, inviting members, joining via links, managing roles and admission, and group deletion. Groups use the same E2E encryption as direct messages -- each member pair has independent encrypted channels. Group metadata (name, image, preferences) is distributed via the group protocol.

## Prerequisites

- User profile created and chat engine running
- At least one established contact (to invite to a group)
- For joining via link: a valid group link or invitation

## Step-by-Step Processes

### 1. Create Group

1. User taps "+" in `ChatListView` -> `NewChatMenuButton` -> "Create group".
2. `AddGroupView` is presented for entering group name, optional image, and description.
3. User fills in `GroupProfile(displayName:fullName:image:description:)` and taps "Create".
4. Calls `apiNewGroup(incognito:groupProfile:)`:
   ```swift
   func apiNewGroup(incognito: Bool, groupProfile: GroupProfile) throws -> GroupInfo
   ```
5. Sends `ChatCommand.apiNewGroup(userId:incognito:groupProfile:)` to core (synchronous).
6. Core returns `ChatResponse2.groupCreated(user, groupInfo)`.
7. `GroupInfo` contains the new group's ID, profile, and the creator as owner.
8. User is navigated to `AddGroupMembersView` to optionally invite contacts.
9. User can also create a group link at this stage.

### 1a. Create Public Group (Channel)

1. Alternative to standard group creation for relay-backed channels.
2. Calls `apiNewPublicGroup(incognito:relayIds:groupProfile:)`:
   ```swift
   func apiNewPublicGroup(incognito: Bool, relayIds: [Int64], groupProfile: GroupProfile) async throws -> (GroupInfo, GroupLink, [GroupRelay])
   ```
3. Sends `ChatCommand.apiNewPublicGroup(userId:incognito:relayIds:groupProfile:)` to core.
4. Core returns `ChatResponse2.publicGroupCreated(user, groupInfo, groupLink, groupRelays)`.
5. The resulting `GroupInfo` has `useRelays == true` and includes a group link.
6. Channel relay members (with role `.relay`) are managed by the core.

### 2. Invite Members

1. From `GroupChatInfoView`, user taps "Add members" -> `AddGroupMembersView`.
2. `filterMembersToAdd` filters contacts already in the group.
3. User selects contacts and assigns roles (default: `.member`).
4. For each selected contact, calls `apiAddMember(groupId:contactId:memberRole:)`:
   ```swift
   func apiAddMember(_ groupId: Int64, _ contactId: Int64, _ memberRole: GroupMemberRole) async throws -> GroupMember
   ```
5. Core sends group invitation to the contact and returns `ChatResponse2.sentGroupInvitation(user, _, _, member)`.
6. The invited contact receives a `CIGroupInvitationView` in their chat.
7. Invited member's status is `.invited` until they accept.

### 3. Join via Link

1. User receives a group link (scanned or pasted).
2. `apiConnectPlan` validates the link and identifies it as a group link.
3. For prepared groups (short links): `apiPrepareGroup(connLink:directLink:groupShortLinkData:)` shows group info before joining. `directLink` is `true` for standard group links, `false` for channel relay links.
4. `apiConnectPreparedGroup(groupId:incognito:msg:)` or `apiConnect(incognito:connLink:)` initiates joining.
5. Core processes the join request. Depending on group admission settings:
   - **Auto-join**: Member is added immediately.
   - **Approval required**: Member enters pending admission queue.
6. `apiJoinGroup(groupId:)` is called for invitation-based joins:
   ```swift
   func apiJoinGroup(_ groupId: Int64) async throws -> JoinGroupResult?
   ```
7. Returns one of:
   - `.joined(groupInfo:)` -- successfully joined
   - `.invitationRemoved` -- invitation was revoked (SMP AUTH error)
   - `.groupNotFound` -- group no longer exists

### 4. Member Admission

1. Group has admission settings configured via `MemberAdmissionView`.
2. When a new member joins a group requiring approval, admins see pending members.
3. Admin reviews pending member in the member list.
4. To accept: `apiAcceptMember(groupId:groupMemberId:memberRole:)`:
   ```swift
   func apiAcceptMember(_ groupId: Int64, _ groupMemberId: Int64, _ memberRole: GroupMemberRole) async throws -> (GroupInfo, GroupMember)
   ```
5. Core returns `ChatResponse2.memberAccepted(user, groupInfo, member)`.
6. To reject: remove the pending member (same as member removal).
7. Member support chat (`MemberSupportView`, `MemberSupportChatToolbar`) allows admins to communicate with pending members.

### 5. Change Member Roles

1. Admin/owner navigates to member info in `GroupChatInfoView`.
2. Selects new role for the member.
3. Calls `apiMembersRole(groupId:memberIds:memberRole:)`:
   ```swift
   func apiMembersRole(_ groupId: Int64, _ memberIds: [Int64], _ memberRole: GroupMemberRole) async throws -> [GroupMember]
   ```
4. Core returns `ChatResponse2.membersRoleUser(user, _, members, _)`.
5. Available roles (in hierarchy order):
   - `.owner` -- full control, can delete group
   - `.admin` -- can manage members, change roles (below admin)
   - `.moderator` -- can delete messages, moderate content
   - `.member` -- standard participant, can send messages
   - `.observer` -- read-only access
6. Role changes are broadcast to all group members as group events.

### 6. Remove Member

1. Admin/owner navigates to member info -> taps "Remove".
2. Calls `apiRemoveMembers(groupId:memberIds:withMessages:notice:)`:
   ```swift
   func apiRemoveMembers(_ groupId: Int64, _ memberIds: [Int64], _ withMessages: Bool, notice: LinkNotice? = nil) async throws -> (GroupInfo, [GroupMember])
   ```
3. `withMessages: true` also deletes all messages from that member.
4. Core returns `ChatResponse2.userDeletedMembers(user, updatedGroupInfo, members, withMessages)`.
5. Without `notice`, the removed member receives a notification and loses access.
6. With `notice` (a blocked member, or "Block and remove"), the removal is silent: members on version 23 or later see no event, apps on earlier versions show the removal, the removed member is not told unless it is an admin, and the removed member's app does not rejoin via the group link for the chosen duration (`BanDuration`: 1 day, 1 week, Forever). Forever asks to type "yes" in "Remove forever?" (`removeBannedMember`).
7. A notice is offered only when the member supports `LINK_NOTICE_VERSION`.

### 7. Block Member for All

1. Moderator, admin or owner taps "Block for all" in member info or in the member swipe actions.
2. "Block member for all?" (`showBanMemberSheet`) offers "Block for" 1 day (default), 1 week or Forever; admins and owners also get "Block and remove" and "Block, remove and delete messages". The footer: "**<name>** won't be notified, and the messages will be hidden for all." for moderators, "**<name>** won't be notified, and if removed, can't rejoin while blocked." for admins and owners.
3. Calls `apiBlockMembersForAll(groupId:memberIds:blocked:ttl:)`; `ttl` is absent for Forever:
   ```swift
   func apiBlockMembersForAll(_ groupId: Int64, _ memberIds: [Int64], _ blocked: Bool, ttl: Int64? = nil) async throws -> [GroupMember]
   ```
4. Core returns `ChatResponse2.membersBlockedForAllUser(user, _, members, _)`; `GroupMember.blockedUntil` is the end time.
5. Blocked members' messages are hidden for all members; only moderators and above see placeholders.

### 8. Leave Group

1. User navigates to `GroupChatInfoView` -> taps "Leave group".
2. Confirmation dialog is presented.
3. Calls `leaveGroup(groupId:)` which wraps `apiLeaveGroup(groupId:)`:
   ```swift
   func apiLeaveGroup(_ groupId: Int64) async throws -> GroupInfo
   ```
4. Core returns `ChatResponse2.leftMemberUser(user, groupInfo)`.
5. `ChatModel.shared.updateGroup(groupInfo)` updates the UI.
6. User retains local chat history but can no longer send/receive.

### 9. Delete Group

1. Owner navigates to `GroupChatInfoView` -> taps "Delete group", or uses "Delete" in the chat list swipe; both show `deleteGroupAlert` (`GroupChatInfoView.swift`).
2. For an active owner (`isOwner` and `membership.memberActive`) of a group or channel, and for the active owner of a business chat with a customer, a sheet with "Keep conversation", Delete and Cancel follows (`KeepConversationDialog`); a member who left or was removed, and an owner whose membership is not active (for example `memAccepted`), delete after the alert only, as the core keeps the conversation only for an active owner.
3. Calls `apiDeleteChat(type: .group, id: groupId, chatDeleteMode:)` through `deleteChat`; "Keep conversation" off sends `.full`, on sends `.entity`:
   ```swift
   func apiDeleteChat(type: ChatType, id: Int64, chatDeleteMode: ChatDeleteMode = .full(notify: true), notice: LinkNotice? = nil) async throws -> GroupInfo?
   ```
4. Core notifies all members. With `.full` the chat is removed from `ChatModel.shared.chats`; with `.entity` core returns the group with membership status `memGroupDeleted`, and `ChatModel.updateGroup` keeps the chat in the list.
5. Business chat of the owner: the sheet says "**<customer>** will be notified.", or "**<customer>** will be notified, and can reconnect via your address." when the user's address is a business address (`userAddress.addressSettings.businessAddress`).
6. "Delete and block" for the owner of a business chat (`deleteAndBanCustomerDialog`): the dialogs of [connection flow 8](connection.md#8-delete-archive-or-block-a-contact) with the customer's name; `apiDeleteChat` with a `LinkNotice`, so the core removes the customer silently with the notice and then deletes the chat.

### 10. Group Link Management

**Create group link:**
1. From `GroupLinkView` (accessible via `GroupChatInfoView`).
2. Calls `apiCreateGroupLink(groupId:memberRole:)`:
   ```swift
   func apiCreateGroupLink(_ groupId: Int64, memberRole: GroupMemberRole = .member) async throws -> GroupLink?
   ```
3. Returns `GroupLink` containing the link URI and member role.
4. Optional: `apiAddGroupShortLink(groupId:)` generates an additional short link.

**Update link role:**
- `apiGroupLinkMemberRole(groupId:memberRole:)` changes the default role for new joiners.

**Delete group link:**
- `apiDeleteGroupLink(groupId:)` invalidates the link.

**Get existing link:**
- `apiGetGroupLink(groupId:)` retrieves the current link (returns `nil` if none exists).

### 11. Group Preferences

1. `GroupPreferencesView` allows configuring per-feature preferences.
2. Features controlled include:
   - Timed/disappearing messages
   - Message reactions
   - Voice messages
   - File sharing
   - Direct messages between members
   - Full message deletion
   - Message history visibility for new members
3. Changes are saved via `apiUpdateGroup(groupId:groupProfile:)` with updated preferences.
4. `GroupWelcomeView` manages the welcome message shown to new joiners.

## Data Structures

| Type | Location | Description |
|------|----------|-------------|
| `GroupInfo` | `SimpleXChat/ChatTypes.swift` | Full group model: ID, profile, membership, preferences, business chat info |
| `GroupProfile` | `SimpleXChat/ChatTypes.swift` | Name, full name, image, description, preferences |
| `GroupMember` | `SimpleXChat/ChatTypes.swift` | Member model: role, status, profile, connection info |
| `GroupMemberRole` | `SimpleXChat/ChatTypes.swift` | `.owner`, `.admin`, `.moderator`, `.member`, `.observer`, `.relay` |
| `GroupMemberStatus` | `SimpleXChat/ChatTypes.swift` | Member lifecycle: `.invited`, `.accepted`, `.connected`, `.complete`, etc. |
| `GroupLink` | `Shared/Model/AppAPITypes.swift` | Group link with URI, member role, and short link data |
| `BusinessChatInfo` | `SimpleXChat/ChatTypes.swift` | Business chat metadata for commercial group chats |
| `JoinGroupResult` | `Shared/Model/SimpleXAPI.swift` | `.joined(groupInfo)`, `.invitationRemoved`, `.groupNotFound` |
| `GMember` | `Shared/Views/Chat/Group/` | View-layer wrapper around `GroupMember` for list display |

## Error Cases

| Error | Cause | Handling |
|-------|-------|----------|
| `errorStore(.groupNotFound)` | Group deleted or not accessible | `JoinGroupResult.groupNotFound` |
| `errorAgent(.SMP(_, .AUTH))` | Invitation revoked | `JoinGroupResult.invitationRemoved` |
| `errorStore(.groupLinkNotFound)` | No group link exists | `apiGetGroupLink` returns `nil` |
| `duplicateGroupLink` | Link already exists for group | Show alert |
| `errorAgent(.NOTICE(server, preset, expires))` | Server notice during link creation | `showClientNotice` alert |
| Network errors | SMP/XFTP server unreachable | Retryable via `chatApiSendCmdWithRetry` |

## Key Files

| File | Purpose |
|------|---------|
| `Shared/Views/NewChat/AddGroupView.swift` | Group creation UI |
| `Shared/Views/Chat/Group/AddGroupMembersView.swift` | Member invitation UI |
| `Shared/Views/Chat/Group/GroupLinkView.swift` | Group link management UI |
| `Shared/Views/Chat/Group/GroupProfileView.swift` | Group profile editing |
| `Shared/Views/Chat/Group/GroupPreferencesView.swift` | Feature preferences UI |
| `Shared/Views/Chat/Group/GroupWelcomeView.swift` | Welcome message editing |
| `Shared/Views/Chat/Group/MemberAdmissionView.swift` | Admission settings UI |
| `Shared/Views/Chat/Group/MemberSupportView.swift` | Admin-to-pending-member chat |
| `Shared/Views/Chat/Group/MemberSupportChatToolbar.swift` | Support chat accept/reject toolbar |
| `Shared/Views/Chat/Group/SecondaryChatView.swift` | Secondary chat view for member support |
| `Shared/Model/SimpleXAPI.swift` | All group API functions |
| `Shared/Model/AppAPITypes.swift` | `GroupLink`, `ConnectionPlan` |
| `SimpleXChat/ChatTypes.swift` | `GroupInfo`, `GroupProfile`, `GroupMember`, `GroupMemberRole` |

## Related Specifications

- `apps/ios/product/README.md` -- Product overview: Groups capability map
- `apps/ios/product/flows/connection.md` -- Connection flow (group links use the same connect mechanism)
- `apps/ios/product/flows/messaging.md` -- Messaging within groups
