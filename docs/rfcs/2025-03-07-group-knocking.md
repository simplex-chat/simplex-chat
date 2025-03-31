# Group knocking

## Problem

In v6.3 release we added an option to "manually" approve members before introducing to group, based on decision made via `acceptMember` chat hook. Currently it's not supported in UI clients, and only used for directory service bot captcha challenge.

The goal of next improvement is to let:
- admins screen members before admitting to group, while not limiting communication with joining member to a single admin (and also removing the need for admin to be a highly available host of the group link);
- and group owners set up other arbitrary automated challenges or entry rules, while still being able to advertise groups in directory service.

## Solution

Group link host (further host), knowing group requires admin approval, would initially only introduce member pending approval to admins. Admins can connect with member for screening, meanwhile host would be forwarding messages as usual between connecting members. As a result of screening, pending member can either be removed, or approved by admins.

Upon acceptance, for further member connections to not depend on availability of admins, host should not only forward acceptance message, but also introduce remaining members to now accepted joining member. Respectively, admins' clients should not introduce members for approved members who are not their invitees.

For group owners to be able to set up alternative automated challenges, these are some possible alternatives:
- We could add a new role `Approver`, so that instead of adding all \[possibly human\] admins, host would initially introduce only approvers.
- It could be an orthogonal to role member setting (would require protocol extension).
- List of "approver" member IDs could be communicated to host client.

### Implementation details draft

Host needs to have knowledge whether to automatically accept, or only introduce admins/approvers.

```sql
ALTER TABLE group_profiles ADD COLUMN approval TEXT; -- comma separated member IDs; null - automatic introduction

-- or

ALTER TABLE group_profiles ADD COLUMN approval INTEGER; -- if based on `Approver` role
```

Alternatively, a different extension of protocol could be done in order to communicate group approval rule from owner to host outside of group profile (special messages).

Admins/approvers need to have separate conversation per pending member, requires adding scope to chat items.

Host to have specific processing of forwarded `XGrpLinkAcpt` - continue introduction of remaining members.
