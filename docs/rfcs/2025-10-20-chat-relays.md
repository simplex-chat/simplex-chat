# Chat relays

## Protocol for adding chat relays to group

```mermaid
sequenceDiagram
  participant OSMP as Owner's<br>SMP server
  participant O as Owner
  participant R as Chat relay(s)
  participant RSMP as Chat relays'<br>SMP server(s)
  participant M as Member

note over OSMP, RSMP: Owner creates new group, adds chat relays

O -->> O: 1. Create new group
O ->> OSMP: 2. Create short link<br>(group entry point)
OSMP -->> O:
O -->> O: 3. Add link to group profile
O -->> O: 4. Choose chat relays
par With each relay
    O ->> R: 5. Contact request<br>(x.grp.relay.inv)
    R ->> RSMP: 6. Create relay link
    RSMP -->> R:
    R ->> O: 7. Accept request<br>(x.grp.relay.acpt)
    O ->> OSMP: 8. Update short link<br>(add relay link)
    OSMP -->> O:
    O ->> R: 9. Notify relay link added<br>(x.grp.info)
    R ->> OSMP: 10. Retrieve short link data
    OSMP -->> R:
    R -->> R: 11. Check relay link added
    R ->> O: 12. Confirm to owner<br>(x.grp.relay.ready)
end
O -->> O: 13. Share group short link<br>(social, out-of-band)

note over OSMP, M: New member connects

M ->> OSMP: 1. Retrieve short link data
OSMP -->> M:
M ->> R: 2. Connect via relay link(s)
R -->> M:
```

Notes:

- On creating group short link beforehand (step 2).

  We do it for protocol simplicity - to have same logic of updating link data with each relay's link.

  Alternatively owner client could create group link with first relay's link, saving a network request. This would require owner client to track state of creating the link to avoid race on receiving multiple relay links.

- On adding group short link to group profile (step 3).

  For protocol purposes it's only a means of informing chat relays about it in step 9 (x.grp.info).

  Alternatively it could be sent as a standalone object in initial contact request to relay (step 5, x.grp.relay.inv), or in step 9 in special event.

  However, there are other arguments for having group link in profile:

  - Strengthening association between link and profile. Link already contains profile in attached data, but from perspective of group profile link itself is detached. All members "see" the same link they joined via in group profile. Chat relays "see" the same link they created relay links for, and can check it for presence of their relay link at any point.

  - Link is recoverable from profile, e.g. for purpose of restoring connection with group via new chat relays.

  Overall it just seems a natural and convenient way to store group link for all members, rather than having it separately.
