# Contact merge issues, eradication, improvement

## Problem

Contact merge (see mergeContactRecords) keeps connections of both contacts by assigning both connections to the merged contact.

When contact is read, last ready connection is selected as contact's active connection (see, for example, getContact - connections are ordered by connection_id in descending order). This may lead to contacts reading different connections, and as a result message delivery failing. Consider following scenarios:
- connection IDs may be in different order for each side, if one of sides inserted connection in gap of IDs after an older (different) connection was deleted. If contacts consider different connections as active for each other, they would subscribe to and send to different connections, completely destabilizing delivery.
- XInfoProbeOk may never be delivered due to server error or processed due to client error. In this case probe sender has 2 separate contacts and user may still use old contact for sending; other side now has a single merged contact record with the latest connection as active - if they restart or resubscribe they would stop receiving messages from the old connection. Perhaps it's less of an issue because that second side would at least send to a new connection which first side (probe sender) considers active for a separate contact, and would receive messages to that new contact. Also if they're establishing a second contact-pair, they may be more likely to use this new contact-pair anyway.

## Solution ideas

### Don't merge contacts

Still merge members and contacts.

Pros:
- no risk of destabilizing existing conversation.
- eventually (paired with a migration for legacy merged contacts) contact reading queries may be simplified to read a single connection, as well several chat logic flows accounting for multiple contact connections.
- direct connection not being replaced with a new connection established via group (in case of "send direct message" feature, or pre 5.3 in case of direct connections established in group).
- connection verification status not being reset (same + duplicate direct connections).
- feature of having separate parallel conversations with same contact is kept.
- easy to implement:
  - in probeMatchingContactsAndMembers only match members (getMatchingMembers), don't match contacts (remove getMatchingContacts).
  - in probeMatch prohibit merging contacts (cgm1 is COMContact, cgm2 is COMContact case; also member with associated contact case).
  - in xInfoProbeOk - same.

Cons:
- "split identity" of contacts - though in many cases it's status quo without latest change (https://github.com/simplex-chat/simplex-chat/pull/3173). It may be not a big enough issue to justify risking connection stability.
- members may be matched to arbitrary contacts - need to consider consequences more thoroughly. Though at least they should match to same contact-pair on both sides. "Send direct message" member contacts will not be matched to existing contacts, though going forward with contact-member merge working it shouldn't be an issue until contact is deleted on one of sides.
- group link host not being merged - member knowing host as a contact would have him as a separate contact now. Perhaps we could rework group link protocol to avoid contact creation - new connection would already be created for host-invitee group connection, with some new connection entity "pending group member" created on both sides (starting with next protocol version support on both sides). This member would then be merged to the existing contact. This would be the most expensive part of this change, if we choose not to ignore it.

### Improve contact merge protocol

- instead of relying on ordering when selecting active contact connection, add flag active_contact_conn, set it on mergeContactRecords:

  ~~verified connection >~~ direct connection > newer connection

  we can't set to verified connection because it's not necessarily symmetric

  ~~direct connection >~~ newer connection

  for backwards compatibility we can't set to direct - other side may still set to newer. Unless we bump protocol version and choose based on that.

  choose "newer" connection based on created_at instead of connection_id - it should be more likely to have the same order.

- initial probe sender to send new message XInfoProbeComplete after processing XInfoProbeOk to signal that old connection is good to delete.

- probe receiver to keep subscribing to both connections until they receive XInfoProbeComplete.

- negotiate connection to use - shared connection id?

```
                   Alice                      Bob
       2 contacts    |                         | 2 contacts
       2 connections |                         | 2 connections
                     |       XInfoProbe        |
                     |------------------------>|
                     |    XInfoProbeCheck      |
                     |------------------------>|
                     |                         | match probe and hash;
                     |                         | merge contacts
                     |                         | 1 contact
                     |                         | 2 connections
                     |                         | (1 active, subscribe to both)
                     |      XInfoProbeOk       |
                     |<------------------------|
 merge contacts      |                         |
 1 contact           |                         |
 1 connection        |                         |
 (delete non active) |                         |
                     |   XInfoProbeComplete    |
                     |------------------------>|
                     |                         | 1 contact
                     |                         | 1 connection
                     |                         | (delete non active)
                     *                         *
```

Pros:
- contact merge is already implemented.
- maintains contact "identity".
- members are matched to a single contact.
- no need to rework group links.

Cons:
- more complex logic - more prone to error.
- likely still risks destabilizing connection.
- continue to maintain and add new chat logic flows accounting for multiple connections (for instance, subscription).
- newer connection replace verified and/or direct connections. These new connections are still possible to establish via indirect and unverified group connection ("send direct message").
- removes "parallel conversations" feature. It's not a hard loss though.
- existing contacts with aliases still not to be merged - to be implemented. Not a con, but a special case for contact merge.
