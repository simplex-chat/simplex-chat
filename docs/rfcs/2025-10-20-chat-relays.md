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
    R ->> O: 7. Accept request<br>(x.grp.relay.acpt<br>incl. relay link)
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

M ->> OSMP: 14. Retrieve short link data
OSMP -->> M:
M -->> M: 15. Prepare group record,<br>save relay links
M ->> R: 16. Connect via relay link(s)
R -->> M:

note over O, M: Message forwarding

O ->> R: 17. Send message
R ->> M: 18. Forward message
M -->> M: 19. Deduplicate message
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

- Owner should reject contact requests to their group link.

- Chat relay should reject contact requests to its relay link until chat relay confirms it is attached to the group link data.


## Protocol for removing chat relay from group, restoring connection to group

```mermaid
sequenceDiagram
  participant OSMP as Owner's<br>SMP server
  participant O as Owner
  participant R as Chat relay
  participant RSMP as Chat relays'<br>SMP server(s)
  participant M as Member

note over OSMP, M: Scenario 1. Owner deletes chat relay, notifies relay

par Owner to chat relay
    O ->> R: 1a. Delete chat relay<br>(x.grp.mem.del)
    par Chat relay to SMP
        R ->> RSMP: 2a. Delete relay link
    and Chat relay to members
        R ->> M: 2b. Forward relay is deleted
        M -->> M: 3. Delete connections with relay
    end
and Owner to SMP
    O ->> OSMP: 1b. Remove relay link<br>(update group link data)
    OSMP -->> O:
and
    O -->> O: 1c. Delete connection with relay
end

note over OSMP, M: Scenario 2. Owner deletes chat relay, fails to notify relay

par Owner to chat relay
    O --x R: 1a. Fail to notify relay
and Owner to SMP
    O ->> OSMP: 1b. Remove relay link<br>(update group link data)
    OSMP -->> O:
and
    O -->> O: 1c. Delete connection with relay
end
loop Chat relay periodic checks
    R ->> OSMP: 2. Retrieve short link data for served gorup
    OSMP -->> R:
    R --x R: 3. Check relay link added
end
par Chat relay to SMP
    destroy RSMP
    R ->> RSMP: 4a. Delete relay link 
and Chat relay to members
    destroy R
    R ->> M: 4b. Notify relay is deleted
    M -->> M: 5. Delete connections with relay
end

note over OSMP, M: Last relay is deleted

O --x M: Owner can't send messages to members
M -->> M: Attempt to restore<br>connection to group (manual)
M ->> OSMP: Retrieve short link data
OSMP -->> M:
M --x M: Members can't restore connection to group

note over OSMP, M: Restore connection to group

create participant NR as New chat relay
O <<-->> NR: 1. Add new relay, relay creates and sends link
O <<-->> OSMP: 2. Update short link<br>(add relay link)
M -->> M: 3. Attempt to restore<br>connection to group (manual)
M ->> OSMP: 4. Retrieve short link data
OSMP -->> M:
M ->> NR: 5. Connect via relay link
NR -->> M:
O ->> NR: 6. Send message
NR ->> M: 7. Forward message
M -->> M: 8. Deduplicate message
```

Notes:

- New relay doesn't have group history.
  - We can prohibit to remove last relay without adding new one.
  - Relays can synchronize history.
  - Can be considered after MVP.
