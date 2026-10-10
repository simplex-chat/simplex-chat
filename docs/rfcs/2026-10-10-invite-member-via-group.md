# Inviting group members to other groups

## Problem

A group admin can invite only a contact to a group.
To invite a member of group A to group B, the admin first creates a contact with this member:

1. Create a member contact (`APICreateMemberContact`).
2. Send the contact invitation in group A (`x.grp.direct.inv`).
3. Wait until the contact connection is established.
4. Invite the contact to group B (`APIAddMember`, `x.grp.inv`).

Steps 1-3 are possible only when direct messages are allowed in group A.
A direct chat is created with each invited member.

## Design

An admin of groups A and B invites a member of group A to group B.
The invitation is sent as `x.grp.inv` over the connection with this member in group A.
Only the invited member receives the invitation.
The invited member sees it in group A and joins group B in the same way as after an invitation from a contact.

### Protocol

`x.grp.inv` is accepted on group member connections, in addition to contact connections.

```
event           x.grp.inv
params          groupInvitation, unchanged
connection      connection with the invited member in group A
forwarding      excluded
group history   excluded
```

Sender requirements:

```
group A preferences     direct messages allowed for the user's role
role in group A         author or above
role in group B         admin or owner, and not lower than the invited role
group A                 group, not channel, different from group B
group B                 group, not channel
membership in A         main profile
membership in B         main profile
invited member          current member with an active connection
invited member version  22 or later
```

The receiving client ignores the invitation when:

```
group A                 channel
group B                 channel
group A preferences     direct messages not allowed for the sender's role
sender                  blocked
fromMember role         below admin, or below the invited role
fromMember id           equal to the invited member id
```

### Sending

New command, next to `APIAddMember`:

```haskell
APIAddMemberViaGroup
  { groupId :: GroupId,
    viaGroupId :: GroupId,
    groupMemberId :: GroupMemberId,
    memberRole :: GroupMemberRole
  }
```

```
groupId         group B
viaGroupId      group A
groupMemberId   member of group A
memberRole      role in group B
```

Syntax: `/_add #<groupId> #<viaGroupId> <groupMemberId> <memberRole>`

For a member outside group A, `SEGroupMemberNotFound` is returned.

New response, next to `CRSentGroupInvitation`:

```haskell
CRSentGroupInvitationViaGroup
  { user :: User,
    groupInfo :: GroupInfo,
    member :: GroupMember,
    viaGroupInfo :: GroupInfo,
    viaMember :: GroupMember
  }
```

```
groupInfo       group B
member          record of the invited member in group B
viaGroupInfo    group A
viaMember       member of group A
```

In group B, the record of the invited member is linked to the member of group A.
Until the member joins, the same invitation is re-sent on a repeated command.
After the member joins, `CEGroupDuplicateMember` is returned.
After the member leaves, is removed or is rejected, a new invitation is sent.

The sent invitation is saved in group A as `CISndGroupInvitation`, visible to the sender only.
The name of the member in group A is stored in the item (`CIGroupInvitation.localDisplayName`), as the contact name in direct chats.

### Receiving

New event, next to `CEvtReceivedGroupInvitation`:

```haskell
CEvtReceivedGroupInvitationViaGroup
  { user :: User,
    groupInfo :: GroupInfo,
    viaGroupInfo :: GroupInfo,
    viaMember :: GroupMember,
    fromMemberRole :: GroupMemberRole,
    memberRole :: GroupMemberRole
  }
```

```
groupInfo        group B
viaGroupInfo     group A
viaMember        sender, member of group A
fromMemberRole   sender role in group B
memberRole       invited role in group B
```

```
group B      created with status "invited", or found by the invitation link
chat item    CIRcvGroupInvitation in group A, main scope, from the sender
```

With automatic acceptance of group invitations enabled, the client joins group B and saves the item with status "accepted".

With an incognito membership in group A, the same incognito profile is used in group B.

In group B, copies of the group A profiles are stored for the sender and for the invited member.
Badge proofs bound to group A are dropped from the copies.
The copies are replaced by the profiles sent in the connection handshake (`x.grp.acpt`, `x.grp.mem.info`).

### Joining

The invited member joins group B with `APIJoinGroup`, or by tapping the invitation in group A.

Status of the invitation item:

```
pending    received
accepted   joined
rejected   group B deleted before joining
```

With disappearing messages on in group A, the item disappears after the status changes, as in direct chats.

The connection with the host is created from the invitation link, independently of the inviter's connection.
So an invitation from a contact deleted before joining is accepted in the same way.
Before this change, joining failed with `SEContactNotFoundByMemberId`.

### Clients

```
group A      existing group invitation card ("Tap to join"), with a label
chat list    group B, added on CEvtReceivedGroupInvitationViaGroup
sending      API only
```

Labels, on group invitation cards in group chats only:

```
received card   For you only
sent card       For <name> only     name of the member in group A
```

### Bots API

```
APIAddMemberViaGroup                  new command
CRSentGroupInvitationViaGroup         new response
CEvtReceivedGroupInvitationViaGroup   new event
```

`APIAddMember`, `CRSentGroupInvitation` and `CEvtReceivedGroupInvitation`: unchanged.

### Version

```
chat version    22
```

Version 22 is shared with owner-signed relay invitations, in one constant `groupInvVersion`.
Version 22 is not in any release: v7.1.0-beta.6 uses version 21.

## Out of scope

- Role changes of members invited via a group before they join.
- "Open" action on accepted invitations.
- Invitations to members pending approval in group A.
- User interface for sending invitations.
