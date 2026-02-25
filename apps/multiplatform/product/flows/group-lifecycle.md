# Group Lifecycle Flow

> **Related spec:** [spec/api.md](../../spec/api.md) | [spec/client/chat-view.md](../../spec/client/chat-view.md)

## Overview

Groups in SimpleX Chat are decentralized: there is no central group server. The group owner's device coordinates membership, and messages are delivered via pairwise SMP connections between members. Groups support roles, invitation links, member admission review, blocking, and profile updates.

## Prerequisites

- Chat is initialized and running.
- An active user profile exists.
- For creating a group: no special requirements.
- For joining: a group invitation link or a direct invitation from an existing member.

---

## 1. Creating a Group

### 1.1 Create Group

1. User navigates to "New Chat" and selects "Create Group".
2. The `AddGroupView` collects a group profile: display name, full name, optional image, and optional description.
3. `ChatController.apiNewGroup(rh, incognito, groupProfile)` is called:

```kotlin
suspend fun apiNewGroup(rh: Long?, incognito: Boolean, groupProfile: GroupProfile): GroupInfo?
```

4. `CC.ApiNewGroup(userId, incognito, groupProfile)` is sent to the core.
5. The core creates the group and returns `CR.GroupCreated` with a `GroupInfo` object.
6. The creating user is automatically assigned the `Owner` role.
7. The new group appears in the chat list.
8. If `incognito = true`, a random profile is used for the user within this group.

### 1.2 Update Group Profile

```kotlin
suspend fun apiUpdateGroup(rh: Long?, groupId: Long, groupProfile: GroupProfile): GroupInfo?
```

1. Owner or Admin navigates to group info and edits the profile.
2. `CC.ApiUpdateGroupProfile(groupId, groupProfile)` is sent to the core.
3. On success, `CR.GroupUpdated` returns the updated `GroupInfo` with `toGroup`.
4. The chat model is updated via `chatModel.chatsContext.updateGroup(rh, groupInfo)`.
5. Profile changes are propagated to all connected members.

---

## 2. Adding Members

### 2.1 Invite a Contact

1. Owner or Admin opens group info and taps "Add Members".
2. `AddGroupMembersView` displays the user's contacts eligible for invitation.
3. A role is selected for the invitee (default: `Member`).
4. `ChatController.apiAddMember(rh, groupId, contactId, memberRole)` is called:

```kotlin
suspend fun apiAddMember(rh: Long?, groupId: Long, contactId: Long, memberRole: GroupMemberRole): GroupMember?
```

5. `CC.ApiAddMember(groupId, contactId, memberRole)` is sent to the core.
6. The core sends a group invitation to the contact via their direct SMP connection.
7. `CR.SentGroupInvitation` returns a `GroupMember` in `Invited` status.
8. The member list updates to show the pending invitation.

### 2.2 Invitee Joins

1. The invited contact receives a group invitation event.
2. A group invitation chat item appears in their chat list.
3. The invitee taps "Join" to accept.
4. `ChatController.apiJoinGroup(rh, groupId)` is called.
5. `CC.ApiJoinGroup(groupId)` is sent to the core.
6. `CR.UserAcceptedGroupSent` confirms the join request was sent.
7. The owner's/admin's device processes the join and establishes pairwise connections with existing members.
8. `CR.MemberConnected` events fire as connections to each member are established.

---

## 3. Member Roles

### 3.1 Role Hierarchy

```kotlin
enum class GroupMemberRole(val memberRole: String) {
  Observer("observer"),   // Can only read messages
  Author("author"),       // Can send messages but limited
  Member("member"),       // Standard member
  Moderator("moderator"), // Can moderate content
  Admin("admin"),         // Can manage members
  Owner("owner")          // Full control, can delete group
}
```

Selectable roles for assignment: `Observer`, `Member`, `Moderator`, `Admin`, `Owner`.

### 3.2 Change Member Role

```kotlin
suspend fun apiMembersRole(rh: Long?, groupId: Long, memberIds: List<Long>, memberRole: GroupMemberRole): List<GroupMember>
```

1. Owner or Admin navigates to member info in `GroupMemberInfoView`.
2. Selects a new role from the role picker.
3. `CC.ApiMembersRole(groupId, memberIds, memberRole)` is sent to the core.
4. The core responds with `CR.MembersRoleUser` returning updated `GroupMember` objects.
5. The change is propagated to all group members.
6. Supports batch role changes (multiple `memberIds`).

---

## 4. Removing and Blocking Members

### 4.1 Remove Members

```kotlin
suspend fun apiRemoveMembers(rh: Long?, groupId: Long, memberIds: List<Long>, withMessages: Boolean): Pair<GroupInfo, List<GroupMember>>?
```

1. Owner or Admin selects a member and taps "Remove".
2. `CC.ApiRemoveMembers(groupId, memberIds, withMessages)` is sent.
3. If `withMessages = true`, the removed member's messages are also deleted from all members.
4. `CR.UserDeletedMembers` returns the updated `GroupInfo` and removed `GroupMember` list.
5. The removed member receives a notification and loses access to the group.

### 4.2 Block Members for All

```kotlin
suspend fun apiBlockMembersForAll(rh: Long?, groupId: Long, memberIds: List<Long>, blocked: Boolean): List<GroupMember>
```

1. Owner, Admin, or Moderator selects a member and taps "Block for all".
2. `CC.ApiBlockMembersForAll(groupId, memberIds, blocked)` is sent.
3. `blocked = true` blocks; `blocked = false` unblocks.
4. `CR.MembersBlockedForAllUser` returns the updated member list.
5. Blocked members' messages are hidden from all group members.
6. The blocked member can still see the group but their messages are not delivered.

---

## 5. Group Links

### 5.1 Create Group Link

```kotlin
suspend fun apiCreateGroupLink(rh: Long?, groupId: Long, memberRole: GroupMemberRole = GroupMemberRole.Member): GroupLink?
```

1. Owner or Admin navigates to group info and taps "Create Group Link".
2. `CC.APICreateGroupLink(groupId, memberRole)` is sent.
3. A default role for joiners is specified (default: `Member`).
4. `CR.GroupLinkCreated` returns a `GroupLink` containing the link URI.
5. The link is displayed in `GroupLinkView` as a QR code and copyable text.

### 5.2 Update Group Link Role

```kotlin
suspend fun apiGroupLinkMemberRole(rh: Long?, groupId: Long, memberRole: GroupMemberRole = GroupMemberRole.Member): GroupLink?
```

1. Owner or Admin changes the default role for new members joining via link.
2. `CC.APIGroupLinkMemberRole(groupId, memberRole)` is sent.
3. `CR.CRGroupLink` returns the updated link with the new default role.

### 5.3 Get Group Link

```kotlin
suspend fun apiGetGroupLink(rh: Long?, groupId: Long): GroupLink?
```

1. Retrieves the existing group link for display.
2. `CC.APIGetGroupLink(groupId)` is sent.
3. Returns `null` if no link exists.

### 5.4 Delete Group Link

```kotlin
suspend fun apiDeleteGroupLink(rh: Long?, groupId: Long): Boolean
```

1. Owner or Admin navigates to group link settings and taps "Delete Link".
2. `CC.APIDeleteGroupLink(groupId)` is sent.
3. `CR.GroupLinkDeleted` confirms deletion.
4. The link becomes invalid; anyone with the old link can no longer join.

---

## 6. Member Admission Workflow

### 6.1 Admission Configuration

Group owners can require review of new members before they are fully admitted:

```kotlin
data class GroupMemberAdmission(
  val review: MemberCriteria? = null
)

enum class MemberCriteria {
  All  // All joining members require review
}
```

1. Owner opens group info and navigates to "Member Admission" (`MemberAdmissionView`).
2. The `review` field is set to `MemberCriteria.All` to require review of all new members.
3. The admission configuration is saved by updating the group profile:
   - `groupProfile.copy(memberAdmission = admission)` is passed to `apiUpdateGroup`.
4. Changes are tracked with unsaved-changes detection (save/discard prompt on navigation).

### 6.2 Accept a Pending Member

```kotlin
suspend fun apiAcceptMember(rh: Long?, groupId: Long, groupMemberId: Long, memberRole: GroupMemberRole): Pair<GroupInfo, GroupMember>?
```

1. When admission review is enabled, new members joining via link arrive in a pending state.
2. Owner or Admin sees pending members in the member support chat / member list.
3. User selects "Accept" and optionally adjusts the role.
4. `CC.ApiAcceptMember(groupId, groupMemberId, memberRole)` is sent.
5. `CR.MemberAccepted` returns the updated `GroupInfo` and accepted `GroupMember`.
6. The member is now fully connected and can participate in the group.

### 6.3 Reject a Pending Member

1. Owner or Admin selects "Reject" on a pending member.
2. The member is removed via `apiRemoveMembers`.
3. The rejected member receives a removal notification.

---

## 7. Leaving a Group

```kotlin
suspend fun apiLeaveGroup(rh: Long?, groupId: Long): GroupInfo?
```

1. User navigates to group info and taps "Leave Group".
2. A confirmation dialog is shown.
3. `CC.ApiLeaveGroup(groupId)` is sent to the core.
4. `CR.LeftMemberUser` returns the updated `GroupInfo`.
5. The user's membership status changes and they can no longer send or receive messages.
6. The group remains in the chat list in a "left" state, and can be deleted locally.

---

## 8. Listing Members

```kotlin
suspend fun apiListMembers(rh: Long?, groupId: Long): List<GroupMember>
```

1. When opening group info or the member list, `apiListMembers` is called.
2. `CC.ApiListMembers(groupId)` is sent to the core.
3. `CR.GroupMembers` returns the member list.
4. `ChatModel.groupMembers` and `ChatModel.groupMembersIndexes` are updated.
5. `ChatModel.membersLoaded` is set to `true`.

---

## 9. Group Chat Scope (Support Channels)

Groups support scoped conversations for member support:

- `GroupChatScope` parameter on message APIs allows sending messages within a specific scope (e.g., member support chat).
- `MemberSupportChatView` and `MemberSupportView` provide UI for admin-to-member private conversations within the group context.
- `GroupReportsView` shows moderation reports scoped to the group.

---

## Key Types Reference

| Type | Location | Purpose |
|------|----------|---------|
| `GroupInfo` | `model/ChatModel.kt` | Group metadata: groupId, groupProfile, membership, fullGroupPreferences |
| `GroupProfile` | `model/ChatModel.kt` | Group display info: displayName, fullName, description, image, memberAdmission |
| `GroupMember` | `model/ChatModel.kt` | Member info: groupMemberId, memberRole, memberStatus, memberProfile |
| `GroupMemberRole` | `model/ChatModel.kt` | Enum: Observer, Author, Member, Moderator, Admin, Owner |
| `GroupMemberAdmission` | `model/ChatModel.kt` | Admission settings: review criteria |
| `MemberCriteria` | `model/ChatModel.kt` | Enum: All (require review for all) |
| `GroupLink` | `model/SimpleXAPI.kt` | Group link: connLinkContact, memberRole |
| `GroupChatScope` | `model/SimpleXAPI.kt` | Scoped conversation within a group |
| `ConnectionPlan.GroupLink` | `model/SimpleXAPI.kt` | Plan result when connecting via a group link |
