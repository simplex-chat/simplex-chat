# Large public grups / channels

## Background

SimpleX Chat users participate in public groups that were created for small, fully connected p2p groups - working groups, teams, etc. The ability to join the groups via the links was added as an afterthought, without forward looking design, simply to accomodate the interest from the users to use SimpleX platform for public groups and communities. Overall, it's correct to say that the emergence of public groups was unexpected in the context of private messaging, and it shows that protecting participants and publishers identity, and having per-group identity is important for many people.

## Problems of the current p2p design

### It doesn't scale to large size

Current design assumes that each peer is connected to each peer and sends messages to all. It creates non-trivial cost of establishing the connection, as the group grows, some abandoned connections when some members remain in "connecting" state and also linearly growing traffic to send each message.

Historically, there were p2p designs when peers connected not to all but some members, but they were mostly used by desktop clients with more persistent network connections, did not provide any asynchronous delivery and were trading lower traffic for latency and availability. Such designs were viable for frequireile sharing across desktop devices, but it doesn't work for dynamic real-time communities with active participation from a small share of members and the design of other members to observe the conversation as it happens.

### It doesn't account for participation asymmetry

Most members of large public groups do not send messages, so connecting them to all other members directly appears unnecessary and costly, and it also requires tracking when members became inactive to stop sending messages to them.

## Objectives for the new design

### Transcript integrity

This issue is covered in detail in [Group integrity](./2023-10-20-group-integrity.md).

### Asynchronous delivery to group

Asynchrnonous delivery is important to protect participants privacy from traffic observation In addition to that, sending scheduled posts is quite often a convenient feature to schedule multiple updates for a longer period of time - for example, schedule daily updates for a week once a week.

### Ability to conceal members list

This is a rather common request for the current groups, and while it could be possible of course to hide it on the client level, this still makes data available via the database, and puts less technical users in unfair position.

### Support pre-moderation

As the group size grows, so does the activity of the bad actors. Some groups will benefit from switching all, some, most, or new members to pre-moderation - when each post needs to be approved by admin before it becomes visible to all members. It would slow down the conversations, but it would allow a better content quality and owners' control of the content.

## Channels based on super-peers

The proposal is to model the new UX design from Telegram channels, with optional subchannels, and granular participation rights for the members / followers.

Another consideration is to create democratically governed communities when creators don't own the community but only appoint the initial administrators, but as the community grows it can elect the new admins or moderators from the existing members, where voting power is somehow determined by community score (which is necessary to compensate from anonymous participants who could subvert the vote if plain vote count was made). This is probably out of scope for the initial implementation, but this idea is very appealing and it doesn't exist in any other decentralised platform.

Technologically, the channel or group would determine which super-peers would host the group or channel, with group content being a merkle tree with ability to remove some content creating holes - which seems to be very important quality, both to remove undesirable content and to protect participants privacy.

Super-peer would manage this merkle-tree state based on the messages from owners, admins and members, with the ability to make some destructive actions confirmed by more than one command. E.g., group/channel deletion may require at least 2 or 3 votes (respectively, for 3 and 5 owners), thus protecting both from accidental deletions and from attacks via owner - one of the owners being compromised won't result in group deletion if 2 votes are required. As the group size grows, owners can also modify rules (which in itself can also require m of n votes).

## Joining group

The current model when the link to join the group is, effectively, an address of one of the admins, is not censorship-resistant, reliable or convenient - the admin can be offline, be removed, etc. So we want to somehow include addresses of multiple super-peers to join the group. Without identity-layer, the addresses are quite large already, and including multiple addresses in one link, while possible, would make the qr code very hard to scan. Practically, without creating identity layer, we can use up to 2-3 super-peer addresses for the group, and increase it later. 2 addresses is likely to be satisfactory, as one of them could be super-peer hosted by SimpleX Chat, and another - by group owners.

The client then will be connecting to all super-peers in the address. Once connected, these super-peers could send the addresses of the additional super-peers, but this is probably unnecessary for the initial release.

## MVP

The challenge is to decide what should be in sicope for the initial release, to make it a valuable upgrade and a viable starting point, without overloading it with the functions that can be added later.

MVP scope:

- Core functioning of the group - creation/deletion/choosing and changing super-peers. While a large scope, it appears essential.
- Message delivery via super-peers.
- Super-peer protocol extension. Most likely super-peer would receive ordinary chat messages, but some operations should be added and require additional protocol messages - adding/removing super-peers to groups.
- Protocol extensions for owner actions with approvals - we already had several accidental deletions or lost owner accounts.
- Search and history navigation. Current decision to send 100 messages both creates unnecessary traffic spikes, and also doesn't provide access to older history and search functions. But, possibly, it should also be in follow up improvements, and only should be included as the initial protocol design.
- New format of the group address to include more than one super-peer.
- Granular permissions and management model. While the user interface can evolve, the protocol and the scenarios, and also rules models seems better to be added from the beginning.

Follow-up / improvements:
- More scalable client - we already observe scalability issues with directory service, so replacing SQLite with Postgres, if the group participation starts growing seems very important.

Out of scope:
- additional super-peers.
- smart-contacts. While very tempting to generalise permissions and management model via smart contracts, that would radically increase complexity and delivery time.