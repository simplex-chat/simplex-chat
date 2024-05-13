# Sending profile update to group members

## Problem

Profile updates are only sent to direct contacts, as sending them to all group member connections is prohibitively expensive. This results in group members not receiving profile updates. Previously the issue was less acute as all group members were created with two sets of connections, one being used as direct connection for their respective contacts (though the traffic issue was more pronounced due to that); also contacts were merged across group members. Since client started to support deletion of group member contact records, and later stopped creating direct connections for group members altogether, it became less likely for group members to receive profile updates. Still even in the latest versions group members can receive profile updates after creating direct contacts via "Send direct message" button, or connecting out-of-band and merging contact and member records.

## Solution

Keep track of which members received latest profile updates. Send profile updates when user is active in group.

### How to track

- users.user_member_profile_updated_at
- group_members.user_member_profile_sent_at
- when user updates profile, remember new user_member_profile_updated_at, later to be compared against group_members.user_member_profile_sent_at

### What to track

- not all profile fields make sense to send in profile update to group members
- changes to displayName, fullName, image should be sent
- changes to preferences aren't necessary to send as they only apply to user contacts
- changes to contactLink may be sent, but can also be excluded for purposes of privacy
  - some users don't expect that sharing address (contactLink) shares it not only with contacts, but also group members
  - this is a broader issue, as the user's contact link may also be sent in user's profile by admin when introducing members - it makes sense to either ignore this for the purposes of this feature, of change it in group handshake as well
- it then makes sense to remember new timestamp on user record only if name or image is changed

### When/To whom to send

- when user is active in group (i.e. broadcasts message via sendGroupMessage), compare group_members.user_member_profile_sent_at against users.user_member_profile_updated_at to determine whether latest profile update wasn't yet sent
- don't send to members in groups where user is incognito
- don't send to members with whom user has direct contact (as it would overwrite full profile update sent to contact)?
  - alternatively it may be better to send the same pruned profile to such members, and for them to ignore this update (or only apply name and image updates, in case sender has silently deleted them as contact without notifying?):
    - this would ensure that they do receive it in case they silently deleted contact without notifying user
    - it simplifies processing, as then the same message is sent to all group members
    - may remember "profile update hashes" on receiving side to not apply profile updates received via member connection to contact profile, if they arrive after previously processed updates received via contact connection (e.g. update that was received late would overwrite more up-to-date updates received via contact connection, until following messages arrive)
- it seems unnecessary to send profile updates on service messages to individual members:
  - it would otherwise lead to members having different profiles of user at different points in time
  - not all of these messages create chat items anyway (forward, intro messages), so user name/image wouldn't matter
  - most if not all of these messages are sent by admins, who are likely to send either some content messages, group updates, or announce new members (x.grp.mem.new, which is also broadcasted)
  - it simplifies processing, as then profile update is sent to all current members
- considering above points, perhaps we can simplify to track user_member_profile_sent_at on groups instead of group_members
  - group_members.user_member_profile_sent_at -> groups.user_member_profile_sent_at

### How to send

Two options:
- send as a separate message, don't special case
- send batched with the main message (using chat protocol batching mechanism), it would avoid broadcasting additional message for users without profile images, and likely in some cases (when main message is short) even with them
  - conflicts with forwarding as forwarding of batched messages is not supported
  - simply implementing forwarding of batched messages is not enough, because currently there is no way to differentiate between history and other batched messages (and received history shouldn't be forwarded)
