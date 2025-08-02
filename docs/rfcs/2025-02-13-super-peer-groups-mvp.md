# Super-peer groups MVP

## Priorities

1. migration to super-peer protocol for groups, min. protocol for choice of super-peer member
2. decrease number of connections, decrease traffic for senders - super-peer message delivery
3. optimize postgres client
4. availability of past messages, skipped messages - conversation pagination via requests to super-peers
5. \* comment threads (subchannels)
6. \* rework group links -> shared group link - ownership to owners, operation to super-peers? or point 10? (initially HA admin is ok)
7. \* multiple super-peers for redundancy, load balancing (decision to deliver based on number of super-peers)
8. \* more advanced management of super-peer list (create, add, choose, delete)
9. \* protect super-peers from abuse - rate limiting, banning (via client restrictions?)
10. \* discovery server (member) language and image recognition, pre-moderation of messages
    > an advertised link to join the group should be controlled by discovery server (so that the group owners can't make discovery server see a content that is different from the actual group content).
11. \* features - ability to conceal member list, scheduled posts
12. \* automated moderation via members reputation score (observer -> member, image posts, more frequent posts)
13. \* authorization of admin changes (have owners and admins sign / wait for delivery from multiple super-peers)
14. \* super-peers to prioritize processing owner > admin > moderator commands/messages (priority to connections)
15. \* integrity of group state (member list > profile > messages) between super-peers

\* not MVP

## Design and implementation ideas

### 1. Migration to super-peer protocol for groups, min. protocol for choice of super-peer member

- Allow owner to assign member as super-peer. In practice many groups have single owner.
- Alternatively protocol to accept and approve role changes is necessary, as it should not be possible to unilaterally appoint some member to be a super-peer? Not necessary.
- Client to have choice to accept/reject becoming super-peer.
- Not necessary to do UI, as regular users aren't highly available clients anyway. They will ignore offer to be super-peer.
- Client to have configuration to auto-accept (or manually) super-peer offer. HA clients can be run with it - directory service.
- In practice to make transition we would recommend owners of current directory groups to choose directory service as super-peer. Another advantage of this approach is that directory service already hosts links.
- Possibly create a "super-peer" bot that can be chosen. Recommend (hardcode?) creating a link via it.
- Multi-host group links are not necessary initially - users will be joining directly to single super-peer.
- Remove ability to add contact via interface? Or for super-peer group it should share group link / introduce to super-peer, and allow all members to invite? Not necessarily MVP, maybe removing ability to add for admins is enough. Instead we could have super-peer link to be included in group profile / description. Pinned?
- Clients to delete connections (probably background job) once super-peer is appointed. Potentially disruptive but will greatly reduce subscription load. We could make warning to owner that it's experimental feature and it's irreversible, at least during beta. Also automatically post warning in group with super-peer's group link to rejoin in case of disruption.

### 2. Decrease number of connections, decrease traffic for senders - super-peer message delivery

- We already have group forwarding - simply have to change rules for forwarding for super-peer groups: only super-peer forwards; other admins don't forward; super-peer forwards messages always, not only for not connected members (simplifies filtering on forwarding).
- New members are never introduced to other members in order to establish connections. Instead all new members join to super-peer via its group link.
- Instead introduction (in previous sense) is replaced with sharing "member records" - profiles and member IDs. Existing members receive new "member record".
- Sub protocol for inviting new members via sharing super-peer link (same considerations as above).
- We may need more robust processing for forwarded messages - more events / edge cases.

### 3. Optimize postgres client

- Connection pool.
- Optimization of indexes (different from SQLite) may be required.
- Many small queries may have to be reworked into large queries in some cases.

### 4. Availability of past messages, skipped messages - conversation pagination via requests to super-peers

- Protocol to paginate group conversation.
- Based on shared msg id? item ts?
- Response batches messages.
- Rate limit? Probably not MVP.

## Super-peer agreement protocol

This protocol draft is for larger scope MVP that includes short links.

The main idea is that public groups should have identity, that is defined via a permanent link to them, and that only owner(s) should be able to control group identity and link. This link is a short link either to SMP blob (see simplexmq rfcs on blob extensions), or to an XFTP file (requires indefinitely long storage). For clarity, to distinguish from per super-peer group links used for establishing connections with super-peers, it will be further called "short link".

Short link points to a blob/file containing:

- Super-peer group links,
- Owners' public keys for verifying ownership transfer and other administrative actions,
- Other group metadata as required by further clarifications to protocol.

UX for creating a public group should be straightforward:

1. Select operators, whose super-peers to use, or optionally custom configured super-peers.
2. Create group, which generates short link and invites super-peers to it. Connection progress is shown for each super-peer.
3. Confirm creation, once super-peers connected. Super-peer can fail to connect, at least one connected super-peer is required for confirmation.
4. Share link for members to join.

Super-peers are pre-configured in app for preset operators. User can also add self-hosted or other known super-peers to custom configuration. Super-peer should have a SimpleX address to receive group join requests.

Protocol for creating public group (happy path):

1. Group owner's client (further owner) creates group record locally.
2. Owner sends contact requests to selected super-peers SimpleX addresses. These contact requests are essentially invitations to be super-peers in this group. ConnInfo sent in these contact requests (INV) contains group invitation details (XGrpInv with added fields, or new specific protocol message - TBC).
3. Super-peers receive these requests. They generate new group links specifically for this group, to serve as a point of connection to them for new members.
4. Super-peers accept requests, sending generated group links in confirmation (CONF) ConnInfos.
5. Owner packages super-peers group links and other group metadata into blob.
6. Owner uploads this blob to one of their SMP servers, creating short link.
7. Owner shares short link publicly or with selected new members.
8. New members retrieve blob via short link.
9. New members connect to super-peers via group links specified in the blob.

```
Owner SMP             Owner            Super-peers       Super-peers SMP      New members
    |                   |                   |                   |                   |
    |           |1. create group|           |                   |                   |
    |                   |                   |                   |                   |
    |                   |2. contact requests|                   |                   |
    |                   | (INV, group inv.) |                   |                   |
    |                   |------------------>|                   |                   |
    |                   |                   |  3. create group  |                   |
    |                   |                   |       links       |                   |
    |                   |                   |------------------>|                   |
    |                   |                   |<------------------|                   |
    |                   |                   |   new addresses   |                   |
    |                   |4. accept requests |                   |                   |
    |                   |(CONF, group links)|                   |                   |
    |                   |<------------------|                   |                   |
    |                   |                   |                   |                   |
    |           |5. package blob|           |                   |                   |
    |                   |                   |                   |                   |
    |  6. upload blob   |                   |                   |                   |
    |<------------------|                   |                   |                   |
    |------------------>|                   |                   |                   |
    |    short link     |                   |                   |                   |
    |                   |                    7. short link (oob)                    |
    |                   |~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ >|
    |                   |                   |                   |                   |
    |                                8. retrieve blob                               |
    |<------------------------------------------------------------------------------|
    |------------------------------------------------------------------------------>|
    |                       blob with super-peers group links                       |
    |                   |                   |                   |                   |
    |                   |                   |      9. connect via group links       |
    |                   |                   |<--------------------------------------|
    |                   |                   |                   |                   |
    *                   *                   *                   *                   *
```

On client side super-peer is a specific user profile, which can be created in a hosted CLI client, or in a highly available desktop app. Super-peer user is programmed to accept incoming requests to join groups as super-peer. It's hidden from regular user list and requires specific API and UI for management.

```sql
ALTER TABLE users ADD COLUMN superpeer INTEGER NOT NULL DEFAULT 0;
```

In user clients super-peer is an invisible group member, that is prohibited to send its own messages, but allowed to forward all messages between group members. It's also hidden from group member list.

```sql
ALTER TABLE group_members ADD COLUMN superpeer INTEGER NOT NULL DEFAULT 0;
```

If group has more than one active super-peer, owner can remove super-peer from group by removing its group link from short link's blob, which means new members will not use it to connect. Owner also should issue message to members to delete connections with removed super-peer, and to super-peer to leave group.

Problems:

- To prevent abuse from owner, super-peer can periodically or on each group join check short link for presence of its group link. If it's absent, it should delete group link, and after some time stop forwarding messages in group and leave it, see next point.
- If group has only one super-peer, fully removing it from group should not be possible until new super-peer \[fully?\] connects to \[all? active?\] existing group members. For this, current super-peer has to remain in group in order to introduce members to new super-peer. At the very least, current super-peer should forward owner's message with new super peer's group link to members. Better, to prevent downtimes or failures in delivery, current super-peer should wait for confirmations of connection from members.
- If group has a single super-peer, or only super-peers of select operators, nothing prevents these super-peers (operators) from effectively deleting group by destroying all connections with members. So if operators adhere to the same moderating/banning policies, group is not protected from censorship unless it uses self-hosted or other custom configured super-peers. Even then, if group used at least one super-peer of select operators, group owner is subject to potential client restrictions.

### Removing super-peer from group

If group has a single super-peer, owner has to add a new one before removing it. Protocol for removing single super-peer from group:

1. Owner adds new super-peer to group and connects with it (as above).
2. New super-peer connects with current super-peer and starts to synchronize its group state, including group history and member profiles.
    - TBC group state to synchronize: full or partial history, additional metadata?
3. Owner updates short link: adds new super-peer group link to it, removes or marks as disabled current super-peer.
4. Owner announces deleting current and adding new super-peer to group.
    - Via both super-peers - current would forward to existing members, new would forward to newly joined.
    - Separate messages or single specialized message ("replace")?
5. Members that have received this message start connecting with new super-peer.
6. Once group state is synchronized, current (removed) super-peer deletes connections with members.
    - To be clarified, protocol for synchronizing group state between super-peers.
7. Members that haven't received announcement via removed super-peer (for example, they were offline), receive AUTH errors on subscription to connection with it. Knowing it is a super-peer connection to a specific group, they retrieve new super-peer group link from updated blob via short link and connect to it.

```
Owner SMP             Owner         Current super-peer   New super-peer          Members
    |                   |                   |                   |                   |
    |                   |       1. connect (add to group)       |                   |
    |                   |-------------------------------------->|                   |
    |                   |<--------------------------------------|                   |
    |                   |      new address for this group       |                   |
    |                   |                   |                   |                   |
    |                   |                   |    2. connect,    |                   |
    |                   |                   |start synchronizing|                   |
    |                   |                   |<----------------->|                   |
    |  3. update blob   |                   |                   |                   |
    |   by short link   |                   |                   |                   |
    |  (new, disabled   |                   |                   |                   |
    |   super-peers)    |                   |                   |                   |
    |<------------------|                   |                   |                   |
    |------------------>|                   |                   |                   |
    |        OK         |                   |                   |                   |
    |                   |             4. announce removing current and              |
    |                   |                  adding new super-peer                    |
    |                   |---------------------------------------------------------->|
    |                   |                   |                   |                   |
    |                   |                   |                   |    5. connect     |
    |                   |                   |                   | (members who re-  |
    |                   |                   |                   |ceived announcemnt)|
    |                   |                   |                   |<----------------->|
    |                   |                   |                   |                   |
    |                   |                 [ state is synchronized ]                 |
    |                   |                   |                   |                   |
    |                   |        |6. delete connections|        |                   |
    |                   |                   |                   |                   |
    |                   |                   |      7. subscribe (members who        |
    |                   |                   |     didn't receive announcement)      |
    |                   |                   |<--------------------------------------|
    |                   |                   |-------------------------------------->|
    |                   |                   |        AUTH error (from SMP)          |
    |                           7. retrieve updated blob                            |
    |<------------------------------------------------------------------------------|
    |------------------------------------------------------------------------------>|
    |                       blob with new super-peer group link                     |
    |                   |                   |                   |    7. connect     |
    |                   |                   |                   |<----------------->|
    |                   |                   |                   |                   |
    *                   *                   *                   *                   *
```

The advantage of this approach is that current super-peer doesn't have to wait for new super-peer to connect to existing members, which can take arbitrary time if they are offline, or even never complete if they don't come online.

If group has more than one active super-peer, owner can remove a super-peer from group immediately.

### Member profile accounting, group statistics

Super-peers don't broadcast all member profiles on introduction, instead they keep accounting of which member profiles were shared to which members, and when forwarding messages also send profiles to members who haven't received them before. Regular members don't see full list of member profiles, only overall number of members and list of profiles of actively participating members (who send messages).

Even without addition of new super-peer, current super-peers can synchronize state by forwarding all messages to each other.

Periodically super-peers send group statistics to owner:

- Number of actively participating members - those who send messages and whose profiles were shared to other members (known via shared profile accounting).
- Number of connected members - members to whom super-peer forwards messages.
- Number of inactive members - members to whom super-peer currently doesn't forward messages due to inactivity. Super-peer considers member inactive if it received their profile from previous super-peer and new member hasn't connected, or due to QUOTA error inactivity. These reasons could be differentiated. Perhaps number of members with QUOTA error inactivity should be a sub-count of connected members.

Owner client can show aggregated statistics and detailed statistics for each super-peer.
