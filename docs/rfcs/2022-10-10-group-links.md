# Group links

## Problem

Friction in group discovery and invitation/joining. Currently each group member has to be invited manually, and has first to be connected to an existing group member, even if group is considered as open/public by its owners.

## Solution

Allow to create group link(s) - a group link is a new type of contact address link, with enabled auto accept. After receiving and automatically accepting a group join request via such link, a contact is automatically invited to group. This allows to publish such link on a site or another platform and reduce friction required to join a "public" group.

## Design considerations

Add group link metadata to contact link?

- It can be used to display information about the group on the link web page, e.g. a group profile or its part, though it introduces a privacy concern.
- Can be used to display only the fact that it's a group invitation link.
- If we use a separate contact address for each group link, and we don't do auto-join on receiving group invitation corresponding to the group link, group link metadata in contact link is not necessary.

Use group link metadata when accepting / joining?

- When accepting we can use the fact that this contact request link is tied to a specific group to automatically invite to group, it's not necessary to parse out this part in agent.
- When joining it could be parsed out in agent and communicated to chat for chat to know it has to automatically accept the corresponding group invite that should follow. Alternatively we can ignore this metadata in agent/chat and let the user to accept incoming group invitation manually. The advantages of the second approach are that it's simpler and that the user can inspect received group profile before accepting. The disadvantage is more friction, though it's on joining side, not on the inviting side (join is performed only once).

Allow each group member who can invite to create their own link, or have such link be a part of group's profile, so it can be created by owners?

- The advantage of the second approach is that owners can set up an always online client, so the connectivity in group would be better compared to one that could be provided by regular clients. Another advantage is that link can be shared by any member role, even without permission to invite members.
- The disadvantage is that if group has no owners, the link can no longer be created. Also since all member would be using the same link for sharing, it would become a group identifier signalling they're members of the same group.
- There're probably other advantages/disadvantages to be considered. Other considerations:
  - Group owners can overwrite each others links.
  - When member leaves group or is removed from group, he should remove corresponding contact address to avoid accepting respective contact requests.
  - When member role is changed, link/contact address should be conditionally removed if new role doesn't allow adding members.

Allow to create a link with incognito membership?

- Even though we currently don't allow to add members for incognito memberships to avoid incognito / non incognito profiles confusion, when accepting group join requests via group links, same incognito profile that is used for membership could be shared.
- Take into account the plan to allow configuring auto-accept on regular contact requests so that accepting is always incognito, regardless of incognito mode. For group links it has to be yet another specific mode of auto accept so that the same profile is re-used instead of usual per request incognito profile.

## Implementation

```
ALTER TABLE user_contact_links ADD COLUMN group_id INTEGER REFERENCES groups ON DELETE CASCADE;
```

API:

- APICreateGroupLink GroupId
- APIDeleteGroupLink GroupId
- APIShowGroupLink GroupId
- CreateGroupLink GroupName  -- for terminal
- DeleteGroupLink GroupName  -- for terminal
- ShowGroupLink GroupName  -- for terminal
- CRGroupLinkCreated ConnReqContact  -- response on create
- CRGroupLink ConnReqContact  -- response on show
