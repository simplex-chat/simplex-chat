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
