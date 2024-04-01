# Large public grups / channels

This document describes specific design elements for the MVP of [the groups based on super-peers](./2024-03-14-super-peers.md).

## Super-peer members

There are two possible design approaches for super-peer members:

1. Separate non-participating members that can only be added as a super-peers and do not have their own roles in the group, can't send their own messages or administer the group.

2. Super-peer being a function of any member, irrespective of their role.

While approach 1 can be simpler, it has its downsides:
- more complex migration of the existing groups - e.g., directory service cannot become a super-peer while remaining group admin (or moderator, if we add a new role).
- less usable clients - users can have desktop clients with good internet connectivity and sufficient computing resources to effectively host several medium size groups, even with SQLite database.
- it makes super-peers more like servers, being both unusable as clients and also requiring larger concurrency, and therefore increasing centralization.

Therefore, the approach 2 looks more attractive, when super-peers must have some role in the group, and the super peers that cannot send messages will have observer role.

As a side note, it also implies that it is beneficial to see the permission to moderate messages not as a role somewhere between Member and Admin, but as a separate privilege that admins and owners have by default for the messages from the members up to their role, but in general it's a separate member profile setting that allows to moderate messages up to a certain role. That approach would help automatic moderation as well, when moderators may be allowed to moderate messages of admins, without being able to remove members.

Proposed protocol modifications:

```haskell
-- this type is used in XGrpMemNew, XGrpMemIntro and XGrpMemFwd for member introductions
data MemberInfo = MemberInfo
  { memberId :: MemberId,
    memberRole :: GroupMemberRole,
    rank :: Maybe Word8, -- new field, 0 for usual members, 1 for super-peers, allows to build additional distribution hierarhies if needed.
    perms :: Maybe MemberPermissions, -- new field
    v :: Maybe ChatVersionRange,
    profile :: Profile
  }

-- this type is used in XGrpInv (in GroupInvitation) and XGrpLinkInv (in GroupLinkInvitation)
-- to invite members to the group
data MemberIdRole = MemberIdRole
  { memberId :: MemberId,
    memberRole :: GroupMemberRole,
    rank :: Maybe Word8, -- new field
    perms :: Maybe MemberPermissions -- new field
  }

-- new type
data MemberPermissions = MemberPermissions
  { moderate :: MemberPermissionTarget -- could be extended to array in the future
  }

-- new type
data MemberPermissionTarget = MemberPermissionTarget
  { maxRole :: Maybe GroupMemberRole -- if absent, can moderate all messages
  }
```

For backwards compatibility, `admin` role implies `{moderate: {maxRole: admin}}` permission and `owner` - `{moderate: {maxRole: owner}}`, which probably can be overridden with `{moderate: {maxRole: observer}}`.

It is also proposed to migrate `memberId` and `memberRole` to `id` and `role` in the parser (in a forward/backward compatible way), without changing serializers.

## Group routing mode

Irrespective of the presense of super peers in the group, the group itself has to be switched to super-peers routing at some point via a group profile update.

```haskell
data GroupProfile = GroupProfile
  { displayName :: GroupName,
    fullName :: Text,
    description :: Maybe Text,
    image :: Maybe ImageData,
    groupPreferences :: Maybe GroupPreferences,
    rank :: Maybe Int, -- new field, 0 for flat groups, 1 for groups with super peers
    redundancy :: Maybe GroupRedundancy, -- new field, only used when rank > 0
    consensus :: Maybe GroupConsensus -- new field, see below in 
  }

-- each field defines an average number of super-peers that will deliver messages (events) related to a specific scope,
-- by default all super peers will deliver the events in that scope.
data GroupRedundancy = GroupRedundancy
  { messages :: Maybe Double, -- messages and message changes, including reactions and comments
    members :: Maybe Double, -- member additions and permission changes
    group :: Maybe Double -- group profile and other changes
  }
```

## Decisions about message or another event delivery

In groups with rank 0 (current groups) all members aim to establish connections with all other members, making it hard to scale. Admins who added the members play a temporary role of forwarding messages, but only until members are connected.

In groups with rank 1 super-peers connect to all members, but each message or event may be delivered by some rather than by all super-peers, to avoid substantial traffic increase. E.g., in groups with 2 super-peers it would be desirable to deliver each message 1.33 (4/3) times on average, while for groups with 3 super peers it can be 2 or 1.667 (5/3) times.

The decision whether a given super peer should deliver the message would depend on these factors:
- deterministic (see below) message hash, `h`.
- receiving member ID, `r`.
- super-peer member ID, `s`.
- target message redundancy `d` (the desired number of super-peers to deliver the message).
- number of super-peers connected to member `r` (and known as connected to super-peer making the decision), `n`.
- 0-based index of the current super-peer in the sorted array of known super peer IDs, `i`. It does not require sorting all peer IDs, it's enough to count how many peers have smaller or larger IDs.

The assumption here is that member ID is unique within the group, and that message hashes are also unique. Also, message hashes rather than sending member IDs are used to ensure that the decision is made differently for the same sender/recipient pairs, allowing recipients to identify integrity violations (e.g., if some of the super-peers decides to change the messages or fail to deliver it).

For simplicity, all parameters are normalized to 0..1 range from their respective binary ranges.

The algorithm to decide whether the message should be delivered by a given super-peer:

```
if (d >= n || n == 1) deliver;
else
  prob = d/n ; delivery probability based on target delivery redundancy
  point = (h + r)/2 ; as member ID and message hash are uniformly random in 0..1 range, `point` will also be uniformly random in 0..1 range
  start = (1 - prob) * i / (n - 1) ; `start` for the peer with `i == 0` will be `0` and with `i == n - 1` will be `1 - prob`
  end = start + prob ; `end` for peer 0 will be `prob` and for peer `n - 1` will be 1
  ; for all peers the range `start..end` will have width `prob`
  if (point >= start && point < end) deliver
  else skip;
```

This algorithm can be proven to result in target average delivery redundancy.

It also requires all super peers to inform other super peers about:
- establishing or losing the connection with other members (e.g., when AUTH is received).
- informing about their decision to stop being super peers in advance.
- most likely using delivery receipts in communications between super-peers that would include message hashes in RCVD info.

## Authorising administrative changes

As members no longer send messages directly to other members, in addition to the risk of the initial MITM by admin (which is now mitigated by having multiple super-peers) there is a risk of super-peers being compromised at a later stage, and in case of administrative changes (member or group level changes) we have these options:

1. have all members postpone these changes until they are communicated by a sufficient number of super-peers (that would require configuring consensus on the group level).
2. have admins and owners sign administrative changes with the public key included in their profile during member introduction.

The downside of approach 1 is that administrative actions will be delayed and have to be executed not at the time the message is delivered, but at the time message is confirmed by other super-peers. That is separate and in addition to member consensus for some changes (see below). That also means that groups with one super-peer will have no defence mechanism against super-peer being compromised. That also means that groups with two super-peers will either also have no such defence (in case required consensus level is 1) or will require that both super-peers are available, and no administrative actions will be executed unless both super-peers are available.

The downside of approach 2 is the lack of repudiation, in fact, there is a non-repudiation quality of such administrative changes as role changes, member additions and deletions, and group changes.

Overall, while repudiation of sent messages appears as important, it seems much less important for administrative changes, and signing member and group changes while relying on Merkle DAG for message history integrity appears to be an optimal tradeoff.

To support this functionality the `admin` and `owner` members have to add public keys to their profiles and communicate profile updates to all members (before or after group is switched to super-peers, but probably before is better).

The middle ground here is moderation, and trade-off here is more nuanced. Practically, whether message is fully removed or marked as removed is the group policy decision, and it can be made based on whether it's more important to preserve content or to remove undesirable content without trace, and also given that super-peers are likely to be give automatic moderation capabilities anyway, preserving deniability for moderation events and relying on Merkle DAG to identify integrity violation seems a better alternative than signing moderation messages. Although it can also be a part of group policy whether to require signing moderation events.

The change to member profile will be:

```haskell
data Profile = Profile
  { displayName :: ContactName,
    fullName :: Text,
    image :: Maybe ImageData,
    contactLink :: Maybe ConnReqContact,
    preferences :: Maybe Preferences,
    authKey :: Maybe AuthKey -- new field
  }

-- this is rather ad hoc and tries to allow two things:
-- - verify that the member has the private key.
-- - allow key rotations on profile changes, without signing the whole profile change.
-- A better option could be to use certificates, although they are much large in size,
-- and also to simply sign profile changes that include key change (then prevKeySignature won't be needed)
data AuthKey = AuthKey
  { key :: PublicKeyEd25519,
    signature :: SignatureEd25519, -- signature of the key itself
    prevKeySignature :: Maybe SignatureEd25519
  }
```

## Role, rank and moderation permission changes of members and group profile changes

There are two problems with the current approach to changing permission, when a single member makes this decision:
- member whose role changes may disagree. E.g., the member may not be willing to have owner or admin role in some groups. Even more so, the member may be unable to perform super-peer functions (have rank 1).
- other owners or admins may disagree with the change or it may have been made by mistake.

With the move to super-peers it is additionally complicated by the fact that super-peers will be forwarding these messages, and they could be compromised, thus disrupting group functioning - this risk currently exists with the existing group directory that plays admin role and in case it is compromised it can remove all members from the group.

Part of this problem is addressed in the previous point by requiring to sign administrative changes.

Another part is about the members agreeing to changes that affect them when additional privileges are granted, and also by requiring the consensus between group owners and admins for any privilege changes.

There are 2 options for proposals/acceptance/approvals flow design:
1. introduce additional protocol messages for each stage, and for each type of change to complement existing messages.
2. manage these stages in orthogonal way, by adding these stages on the top level of the protocol.

The option 2 is likely to result in a more concise protocol as it will separate approval from from the events, also allowing to include authorizations in a standardized way. This also fits well in `XGrpMsgForward` event that includes that message inside, so all authorizations will be forwarded.

```haskell
-- fields are the number of member approvals required for actions with that role.
-- As target member acceptance is required only for privilege increases it won't be among approvals required for consensus.
-- For example, for group with consensus = {admin: 3, owners: 2}, a new member can be made admin with the decision of 3 other admins or with the decision of single owner (as owner role is higher), or the group can be removed, a new admin is added or group consensus changed with the decision of 2 admins.
-- Owners leaving without changing the consensus can result in consensus becoming unreachable - this is not different from losing a single member, and it only prevents accidental or malicious destructive actions, but does not prevent losing access.
data GroupConsensus = GroupConsensus
  { admin :: Maybe Word16, -- possibly, this field is not needed
    owner :: Maybe Word16 -- 1 by default
  }

-- alternatively, the type could be more flexible than that, but it can be extended if needed.

data ChatMessage e = ChatMessage
  { chatVRange :: VersionRangeChat,
    msgId :: Maybe SharedMsgId,
    chatMsgEvent :: ChatMsgEvent e,
    stage :: Maybe MessageStage, -- new field, Nothing for broadcasted messages
    bcast :: Maybe MessageBroadcast, -- new field instructing super-peers how to broadcast the message
    auth :: Maybe (Either MemberApproval [MemberApproval]) -- approvals for broadcasted messages from members, Either won't be in encoding
  }

data MessageStage = MSProposed | MSApproved

-- possibly, this could include approval stage but it is implied by the context, message needs to be approved by:
-- - the sender with the sufficient permissions
-- - the target of the change when privileges are granted
-- - other admins or owners as required by `consensus` property in group profile.
data MemberApproval = MemberApproval
  { memberId :: MemberId, -- can be encoded as id?
    auth :: SignatureEd25519
  }

data MessageBroadcast = MessageBroadcast
  { from :: Maybe MessageFrom, -- MFSender by default
    auth :: Maybe Bool, -- whether to keep auth if present, True by default, False means to validate and remove auth
    schedule :: Maybe MessageSchedule
  }

data MessageFrom = MFSender | MFApprovers

data MessageSchedule = MessageSchedule
  { deliverAt :: Maybe UTCTime, -- when received by super-peer by default
    minDelay :: Maybe Int, -- seconds, added to start, 0 by default
    maxDelay :: Maybe Int -- seconds, added to start, 0 by default
  }

-- e.g., to deliver at least 2 hours after received by super-peer, the MessageSchedule would be {minDelay :: 7200}
-- or, to deliver all messages 20-40 seconds after it is received, to complicate traffic correlation it would be {minDelay: 20, maxDelay: 40}
-- or, to deliver at a scheduled time {deliverAt: "2024-04-01T00:00:00Z"}
```

The signature is computed over deterministic message hash excluding `auth` property. This structure would also allow asking super-peers to forward the message as originating from multiple owners or admins without showing who originated the message, as long as necessary approvals are present.

That will require extending `XGrpMsgForward` to support array of MemberId and allow UI to show multiple senders of the message:

```haskell
  | XGrpMsgForward :: [MemberId] -> ChatMessage 'Json -> UTCTime -> ChatMsgEvent 'Json
```

The messages send from multiple owners can have the benefit of avoiding targeted attacks on the member who originated it - it will only be known to other members sending the message, but not to other members, creating mutual responsibility for some changes and/or announcements.

## Choosing MVP scope

With all these ideas for improvements, the challenge is to choose some valuable MVP shippable in the shortest time.

From the UX point of view, the most lacking parts are:

- broadcasting messages via super-peers, including:
  - protocol to accept and approve role changes is necessary, as it should not be possible to unilaterally appoint some member to be a super-peer.
  - computing deterministic message hashes and signatures (and only member with the key in profile can be made super-peer).
  - protocol to communicate connections with members between super-peers, yet TBD.
  - algorithm to make decision whether to deliver message.
- an equivalent of Telegram channels - groups where members can only send comments and set reactions, but cannot send messages. Protocol extensions yet TBD.

Optionally, we could include:
- Merkle tree integrity validation.
- Owners/admins consensus.
- Messages from multiple members (don't have to be owners, can be any members, irrespective of the need for consensus).
- Delayed messages.

None of this optional list is required to launch.
