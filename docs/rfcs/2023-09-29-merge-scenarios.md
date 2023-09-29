# Merge scenarios

## Problem

Chat client allows multiple contact and group members records referring to the same "identity" be disassociated from one another. In some cases initially there is knowledge about the fact, and in some other cases it could be established via probe mechanism.

There are cases already addressing this problem:
- Contact merge or contact and group member merge (depending on creation of direct connection between members) when new member joins group, via probe mechanism.
- Contact merge when connecting via group link.
- Contact and group member association when sending direct message to a new contact.
- Existing / deleted contact being preserved when receiving invitation direct message (XGrpDirectInv) from a group member.
- Repeat contact requests to the already connected contact being prohibited; repeat contact requests being squashed on the receiving side (XContactId mechanism).

Cases ignoring this problem:
- Duplicate contacts on repeat connections via invitation links.
- Duplicate contacts on repeat connection via contact request, when the existing contact was not created via a contact request to the same address (it could be created via any other means - via invitation link, via request to an old address, via group member).
- Contact and group member records (possibly for many groups) not being merged when contact connects and group member records already exist.
- Group member records in different groups not being merged if contact doesn't exist.
- Duplicate contacts, or contact and group member records not being merged when connecting via contact address present in contact/group member profile. Probing could be avoided in this case as client already has knowledge of contact address "identity".

This problem is a direct consequence of lack of user identity in the platform, and in some cases we even consider it a feature. For example, duplicate contacts via repeat connections can be used for having conversation scopes. Though in general it seems to bring more confusion. It also limits some interactions, such as sending direct message to group members, or viewing a list of groups contact is member of (not implemented).

On the other hand, solving all these cases reduces the privacy of the main profile, since the client cooperates with other probing clients blindly. To keep this property, we can add an opt-out "Merge contacts" client setting which would affect existing and new probe mechanisms. (It would act as an Incognito mode currently does - launch probes w/t launching probe hashes, and never confirm received probes) We can also ignore it, since this can be worked around via Incognito profiles or multiple user profiles.

There is one more problem that could be addressed in the same scope - repeat group join via group link fails if the contact with host wasn't deleted. This happens due to group links re-using contact address machinery together with prohibiting repeat connections. This could be treated in a similar way to how it is treated for contact addresses: instead of simply prohibiting repeat connection, if group exists, it would be opened; if group doesn't exist, client would send host a request to re-invite them.

## Solution

### Duplicate contacts on repeat connections via invitation links

Can be solved by probing contacts.

### Duplicate contacts on repeat connection via contact request

Can be solved by probing contacts.

Special case - records can be directly associated w/t probing when address is known in a contact / group member profile, see below.

### Contact and group member records not merged when contact connects and group member records already exist

Can be solved by probing group members.

Send probe hashes to all viable group members? Or should group member records already have been merged between each other? (see below)

In all cases above - who should initiate probing? It doesn't matter much, but possibly the contact that started connection.

### Group member records in different groups not being merged if contact doesn't exist

Currently multiple group members share the same "identity" by being associated to the same contact. However, it is allowed to have a group member record not associated contact. It can happen if group member's associated contact is deleted, or, with the latest changes that enable skipping creation of direct connections between group members, it could never exist in the first place.

Can be solved by probing group members, merge could be done in one of the following ways:
- Have surrogate contact records always associated to group member records, not available for use as regular contacts and using group member connections for probing.
- Merge on the level of contact profiles.

The latter seems more straightforward.

Some more factors to consider:
- Group member record may have both associated contact to probe via, and matching group member records in other groups.
- Matching group member records in other groups may have associated contact records, which in turn may have associated contact records different from contact record associated to group member in question.
- Client to which probes are sent may have contact record deleted, but have group member connection - in this case merge would be possible only if probe is sent to group member.
- Member connections shouldn't be merged, because generally they're established via hosts, and hosts of different groups may have had different level of trust.

Probably the solution is to:
- send probe to associated contact if it exists (implemented currently)
- if associated contact doesn't exist send probe to group member (not implemented?)
- send probe hashes to all matching contacts (implemented)
- send probe hashes to all matching group members in other groups that don't have associated contact records (not implemented)
- merge all contacts that confirmed (currently only the first confirming contact is merged)
- merge all group members that confirmed (not implemented, merge profiles?)

Check: if both group member and associated confirm probe, will they be properly merged?

### Connecting via profile address

There are two possible scenarios: contact request is made via member info (associated group member is already in UI scope); and contact request is made via any other means but contact address is known.

**1. Contact request is made via button in group member info**

(contact chat info doesn't have such button currently, as we're already in a chat with contact)

Contact record can be associated directly to a group member record.

It can be simplified to reusing "send direct message" logic here (though there should be no message if direct messages preference is off in group), or even hiding this button now that "send direct message" exists.

To consider:

Currently group member address is shown even if direct messages are prohibited in group. There're two reasons this preference is usually enabled in a group:
- to prevent abuse (in public groups),
- to prevent members from direct communication.

Member address being shown regardless of this preference undermines the second use case.

**2. Contact request is made via any other means**

- On any contact request, search contact_profiles table for matching address (add index on contact_link).

- If profile is not found:

  - Send contact request as usual.

- If profile is found:

  - Search for contact by contact_profile_id.

  - If contact is not found:

    - Send contact request, associating it with existing profile.
    - What if contact is accepted with a different profile? Update profile?

  - If contact is found:

    - Open existing contact.

It's unlikely but possible (?) that contact deleted their contact address, didn't notify about profile change, then another user created link with the same address. If it is then used to make a contact request wrong record may be associated. Probability of that is very low, but maybe we shouldn't make such direct associations and instead always rely on probing?

### Repeat group join via group link

**If group still exists:**

Open group.

How to check for group existence? Currently we save group_link_id on host contact's connection. It may have been deleted by the time of repeated connection via group link. Probably we should also save group_link_id or even the full contact address on the group record itself.

**If group doesn't exist:**

If host contact was deleted, make contact request.

If host contact wasn't deleted, it's possible to request repeat invite via host contact, if it wasn't also deleted on the host's side.

```haskell
XGrpRequestInv :: ConnReqContact -> ChatMsgEvent 'Json
```

Pass full link for host to search group by. Group link id may be enough.

If host deleted contact and didn't notify, this approach wouldn't work - repeat contact request is required.

The solution may be the following: instead of trying to send XGrpRequestInv to host contact, always send contact request with XGrpRequestInv (modified) instead of XContact:

```haskell
-- Maybe XContactId?
XGrpRequestInv :: Profile -> XContactId -> Bool -> ChatMsgEvent 'Json
```

Last Bool to let host know whether requesting client has contact.

If host has contact with XContactId and requesting client has host's contact (host decides by flag in XGrpRequestInv), send group invitation to contact, otherwise accept contact request regularly.

This seems quite complicated, and since clients now notify about contact deletion, maybe it's simpler to send XGrpRequestInv described above to host's contact if it exists and active. We could create chat item for host's contact and make it used to appear in chat list, so that at least it's easier to delete it if something goes wrong and group join gets stuck.
