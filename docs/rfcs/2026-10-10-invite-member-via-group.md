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
role in group A         admin or owner
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
sender role in group A  below admin
sender                  blocked
fromMember role         below admin, or below the invited role
fromMember id           equal to the invited member id
```

### Sending

New command:

```haskell
APIInviteMember
  { groupId :: GroupId,
    fromGroupId :: GroupId,
    groupMemberId :: GroupMemberId,
    memberRole :: GroupMemberRole
  }
```

```
groupId         group B
fromGroupId     group A
groupMemberId   member of group A
memberRole      role in group B
```

Syntax: `/_invite #<groupId> #<fromGroupId> <groupMemberId> <memberRole>`

For a member outside group A, `SEGroupMemberNotFound` is returned.

Response: `CRSentGroupInvitation`, with `contact` omitted.

In group B, the record of the invited member is linked to the member of group A.
Until the member joins, the same invitation is re-sent on a repeated command.
After the member joins, `CEGroupDuplicateMember` is returned.

The sent invitation is saved in group A as `CISndGroupInvitation`, visible to the sender only.

### Receiving

```
group B      created with status "invited", or found by the invitation link
chat item    CIRcvGroupInvitation in group A, from the sender
event        CEvtReceivedGroupInvitation, contact omitted, member_ = sender
```

With automatic acceptance of group invitations enabled, the client joins group B and saves the item with status "accepted".

With an incognito membership in group A, the same incognito profile is used in group B.

In group B, copies of the group A profiles are stored for the sender and for the invited member, without badges.

### Joining

The invited member joins group B with `APIJoinGroup`, or by tapping the invitation in group A.

Status of the invitation item:

```
pending    received
accepted   joined
rejected   group B deleted before joining
```

### Clients

```
group A          existing group invitation card ("Tap to join")
chat list        group B, as for invitations from contacts
event types      CEvtReceivedGroupInvitation.contact optional (Kotlin, Swift)
sending          API only
```

### Bots API

```
APIInviteMember               new command
CRSentGroupInvitation         contact: optional
CEvtReceivedGroupInvitation   contact: optional
                              member_: new, optional
```

Bots that join groups on `receivedGroupInvitation` accept these invitations without changes.
The directory service ignores invitations without `contact`.

### Version

```
chat version    22
```

Version 22 is also used in branch `ep/profile-badges` (`signedRelayInvVersion`).
Version 23 is assigned to the branch merged second.

## Out of scope

- Role changes of members invited via a group before they join.
- "Only you can see it" label on the invitation card.
- "Open" action on accepted invitations.
- Invitations to members pending approval in group A.
- User interface for sending invitations.
