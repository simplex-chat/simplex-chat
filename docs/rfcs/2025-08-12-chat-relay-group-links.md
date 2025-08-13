# Group links for large groups

## Problems

We want to make group links independent from the user who created it, and that members can join via several chat relays referenced by this link, without allowing these chat relays to perform MITM attack on the group.

## Solution

To make group shared, we would make it a part of group profile, with a lot of UX friction to remove it. It will be created at a point a new public group or channel is created (the type is part of the link, it's either /g or /c) and it will be delegated to chat relays  during the creation.

Chat relay is the special mode of chat client operation. Users will connect to chat relay via its address (we need an additional uri for it, /r, so that clients recognize it). UI should prohibit connections to relays, it's not a chat bot. When chat relay address is used in the UI, the app should offer adding it to configuration (while we are at it, we can do the same for other types of servers).

During group creation we will let users choose one of configured chat relays, possibly allowing to add a new chat relay here as well. The client would implement a protocol for adding chat relay to the group via its address.

The criteria for this protocol:
- chat relay should not know which groups are registered by the same user.
- chat relay should create a separate address for each group, to be able to process requests in parallel, and to prevent high load in one group slowing down all groups (at least, on the message queue level).
- protocol should be idempotent, so that the repeated requests to provide address for the same group did not result in creation of the new addresses.

It means that user's connection with chat relay cannot be a persistent connection, but rather each group should be added via a separate per-group connection, which can be used both for future requests about this group (e.g., suspend/delete), and also for relay to send service messages to the group owners.

We several options here:
- connection with relay is a "connection", and it has no presence in the UI. The downside is that this connection will be not manageable in the UI, and no history of group operations will be visible.
- connection with relay is a "contact", and it has presence in the UI as a conversation. It allows to implement both commands and UI actions, so more flexible, but the downside is that this connection will be owned by one owner, and cannot be shared, so if owner disappears it would be hard to transfer management to other owners.
- connection with relay is a special "service group" for public group, and each new owner is added to this group. This is probably the best option, as it also would give owners the space to discuss any matters about the group and also send commands to the relay. But the downside here is that we don't support any e2e encryption in this group, and it won't be obvious to the users, so we should probably disable messaging until we have e2e encryption in support and this new "owners" scopes for new channels. But we still can support commands and other UI actions with relay (e.g., suspend accepting new members or removing the relay), and all owners will see these changes in this channel.

One more challenge here is that it's also not per group, but per relay, so probably it's wrong to use conversation semantics here - while we can have it as a group, and show log of changes, using group entity, we should not allow general messaging here.

So it would be just a "log" associated with each relay and under the hood it would be a group.

The protocol for adding relay to a public group channel:
- owner creates a "relay service group" for this relay and this public group/channel/.
- owner sends a special request to relay's contact address to join this service group (it's "invitation" in SMP protocol terms, with group profile in metadata)
- relay would join the group
- relay can analyze the invitation and reject if it doesn't like the group profile (it should still join the group, as currently we don't have rejection mechanism that would work without connecting, but we can reconsider it),
- if relay is happy to service this group, it would generate it's dedicated address for this group and send back in a special message.
- owner would add this address in delegate section in profile data - it's outside of group profile.
- owner may notify relay that group profile is updated.
- relay would validate that it's address is present in profile and update the state.

Questions:
- should relay accept requests to group address prior to being notified and validating that address is added? Probably yes, to avoid race conditions. But we can simply receive these requests and hold them as unprocessed. That also suggest some worker mechanism to process requests to avoid their loss even in auto-accept scenarios.

Owners credentials (signature keys) are embedded into group address, and cannot be replace by the relay. How could we similarly protect relay MITM attack on member credentials? While it's not critical for channel phase, it will be important for groups - probably we should either include one public X25519 key that can be used to encrypt/authenticate member's key(s), where private counterpart would be shared across owners, or to have one X25519 key per owner.

Owners probably need a separate communication channel related to the group that is independent of relays, so they can exchange any keys (e.g., recipient keys for the queue), or this X25519 key, without the need to establish e2e encryption via relay. It can also allow discussing group/channel matters privately and independently of relays.
