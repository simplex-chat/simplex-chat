# Restore group events and members

## Restoring group events

### Requesting missing events

Clients store local graphs of group events.

When receiving an event with unknown parents, client schedules a request for missing events. If by the scheduled time some of the missing parents are not yet received, client sends request to group admins. Request (`XGrpReqMsg`) contains unknown parent IDs, and last known graph leaves IDs at the time of sending.

Client may receive another event with an intersecting list of unknown parents, in this case it shouldn't repeatedly request same parent IDs.

Data structures to support this logic:

```
parentsRequests :: Map <parentId, requestId>
requestsParents :: Map <requestId, [parentId]>
requestThreads :: Map <requestId, requestThread>
```

Some considerations:
- For missing parents received in an event, filter out parentIds already in parentsRequests, create a new request with remaining missing ids:
  - add records to parentsRequests,
  - add record to requestsParents,
  - start requestThread with delay (see disappearing messages logic).
- Don't persist requests, instead on next start gather all missing parentIds into a single new request. This new request should be again delayed in case some new missing parents are received after offline wait time.
- How long should initial request delay be? It should be long enough to account for usual message catch-up on subscriptions, and short enough to be scheduled while client is online (i.e. until mobile user closes the app again). Perhaps 30s interval is ok, same as for message expiration.
- How long should restored request delay be on client start? It, again, should account for the same subscription and catch-up time, so it could be the same.
- Small delay wouldn't account for long offline period and long catch-up for a large amount of messages, especially on slower clients.
- Receiving missing parent should remove its record from parentsRequests, and an element from requestsParents list of parentIds of the corresponding request. If request's list of parentIds becomes empty, it should kill requestThread and remove it from requestThreads.

### Providing missing events

Admin client receiving request for missing events, should build chain (chains?) of missing events from requesting client's last known leaves to missing parents, and send them in order (`XGrpFwdMsg`).

- Traverse graph from last known leaves down to missing parents, or vice versa?
- If a missing parent has several of its own parents, all should be included (so the resulting structure is a subgraph, not a chain).
- If any of the request message IDs are missing for admin as well, report to requesting client or ignore?

Alternatively, instead of traversing graph in attempt to infer all missing events, admin client could simply send all requested missing parents together with their parent IDs, and requesting client could iteratively repeat request for those. Requesting client doesn't need to include last known leaves in this case. One of the downsides is that it leaves more room for out-of-order processing of missed events.

### Receiving missing events

- At time of receiving forwarded event, it may be no longer missing, for example if it was earlier received from author or another admin. In this case it's ignored.
- Received forwarded events should be incorporated into event graph.
- Single processing queue for forwarded events and regular events. This happens automatically due to `XGrpFwdMsg` itself being received as an event.
- Chat item ordering - rework to base on graph, or continue to order on timestamp? Admin could include original event timestamp together with forwarded event (`MsgForward.msgTs`).

## Restoring member connections

- When receiving an event parent with an unknown memberId, client would create a placeholder group member (from `GroupEventParent.displayName`).
  - Events forwarded by admins can be attached to this placeholder member.
  - Should admins communicate member's role before forwarding events, so that it can be taken into account for processing role sensitive events? (So even regular messages, because member could be observer)
- Client would request connection with this group member from group admins (`XGrpReqConn`), including invitation link to be forwarded.
  - Probably it also should be delayed similarly to `XGrpReqMsg` above.
- Admin receiving `XGrpReqConn` would forward this invitation link to requested member.
  - Use `XGrpMemFwd`?
- If both unconnected members clients simultaneously request connection and receive `XGrpMemFwd` for each other, a race can occur when they'll be trying to establish different connections.
  - In this case when receiving `XGrpMemFwd`, if member record already has associated connection initiated by client, they should decide whether to join new connection and override existing one based on connection invitation hash comparison (similar to solving "create member contact" race).
