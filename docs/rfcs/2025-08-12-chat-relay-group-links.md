# Group links for large groups

## Problems

We want to make group links independent from the user who created it, and that members can join via several chat relays referenced by this link, without allowing these chat relays to perform MITM attack on the group.

We also want to establish shared secret, or even a pairwise ratchet with each active group owner, so that we can send e2e encrypted support messages.

## Solution

To make group shared, we would make it a part of group profile, with a lot of UX friction to remove it. It will be created at a point a new public group or channel is created (the type is part of the link, it's either /g or /c) and it will be delegated to chat relays  during the creation.

Chat relay is the special mode of chat client operation. Users will connect to chat relay via its address (we need an additional uri for it, /r, so that clients recognize it). UI should prohibit connections to relays, it's not a chat bot. When chat relay address is used in the UI, the app should offer adding it to configuration (while we are at it, we can do the same for other types of servers).

During group creation we will let users choose one of configured chat relays, possibly allowing to add a new chat relay here as well. The client would implement a protocol for adding chat relay to the group via its address.

The criteria for this protocol:
- chat relay should not know which groups are registered by the same user.
- chat relay should create a separate address for each group, to be able to process requests in parallel, and to prevent high load in one group slowing down all groups (at least, on the message queue level).
- protocol should be idempotent, so that the repeated requests to provide address for the same group did not result in creation of the new addresses.

Owners connection with each chat relay in a group will be a special "service group", where owners can only send pre-agreed commands to the relay, receive service notifications from relay, and messages from relay operators.

This service group will be established at the point relay is added, and the original message sent to relay's contact address would have SMP invitation (possibly, a short version) to join this service group. If it's a short invitation, it may include service group profile that would be required to reference the original group in some way, e.g., reference its link.

In addition to that, each public group would have an associated group between owners that they would use to run a consensus protocol to agree any group changes, share any secrets and keys (e.g., recipient keys to be added to queue), and can also use for ad-hoc messages. This only needs to exist if there is more than one owner in the group (which is recommended).

The steps to create a public group:
- owner creates a group profile and a link to join the group. This link will be marked as inactive, as there will be no relays in it, so it won't be possible to join via it until relays are added. Probably, this link won't be even visible in the UI at this point.
- for each relay chosen to be used in the group, owner creates a "relay service group" for this relay and this public group/channel.
- owner sends a special request to relay's contact address to join this service group (it's "invitation" in SMP protocol terms, with group profile in metadata)
- relay would join the group
- relay can analyze the invitation and reject if it doesn't like the group profile (it should still join the group, as currently we don't have rejection mechanism that would work without connecting, but we can reconsider it),
- if relay is happy to provide service to this group, it would generate it's dedicated address for this group and send back in a special message. At this point relay may start accepting members to this group.
- owner would add this address in delegate section in link data.
- owner may notify relay that group profile is updated, the relay would verify it.
- relay would validate that it's address is present in profile and update its state.

Steps for a member to join the group:
- a member would load group profile and would see that it is a public and delegated group (direct connections are prohibited, delegates are present).
- a member would connect to all delegates (relays), that would know that it is a request to join the group.
- a relay would send owners profiles, signed by group owner keys present in group link (or its extension, see below). Question - at what point a relay would see signed owners profile? Probably all profiles sent in a group should be signed by member keys, and these keys should be the same as in the link (or signed by the key in the link). It means that relay should retain signed profiles.
- a member would send back both its public signature key and X25519 key for future e2e encryption (or, possibly, DR keys for double ratchet agreement via relay?), either for each owner separately or one set of keys for all owners? Probably same set of member identity keys and 1-time key per owner, as in DR's X3DH agreement.
- an owner would compute shared secret or DR state, and sign it with its key and would send back. Owner would also sign member's signature key to authenticate it.
- member would validate signature, both on DR agreement and on the signature key, to confirm integrity of the exchange - that would allow sending e2e encrypted messages (e.g., in support scope) via relay to owners.

The requirement to have 1-time key complicates messaging, as relay has to forward each message to only one owner, so it sort of requires a smaller scope per member - effectively a DM scope. But this can be useful for establishing DMs via relays as well as each member's profile would authenticated by the owner and once we start sending member profiles to other members (although it'll probably would happen with channels already, they will be authenticated by some or all owners). So, effectively owners would play the role of authentication service in this exchange enabling member to member DMs, without relay trust. Also any important messages from the members - such as profile update, and leaving the group, would have to be signed by the member, and the signature key would be authenticated by the owners too.

As owners credentials (signature keys) are embedded into group address, they cannot be replace by the relay. The above agreement protects from MITM attack by relay completely, and only allows attack by owners, but the relays would protect from owners - so effectively only the group when owners and all relays are in collusion would be able to MITM direct member connections, and in all other scenarios it would be mitigated - relays protect from owners (and each other), and owners protect from relays.
