# Sending profile update to group members

## Problem

Profile updates are only sent to direct contacts, as sending them to all group member connections is prohibitively expensive. This results in group members not receiving profile updates. Previously the issue was less acute as all group members were created with two sets of connections, one being used as direct connection for their respective contacts (though the traffic issue was more pronounced due to that); also contacts were merged across group members. Since client started to support deletion of group member contact records, and later stopped creating direct connections for group members altogether, it became less likely for group members to receive profile updates. Still even in the latest versions group members can receive profile updates after creating direct contacts via "Send direct message" button, or connecting out-of-band and merging contact and member records.

## Solution

Keep track of which members received latest profile updates. Send profile updates when user is active in group.

### How to track

- users.last_profile_update_ts
- group_members.last_profile_sent_ts
- when user updates profile, remember new last_profile_update_ts, later to be compared against group_members.last_profile_sent_ts

### What to track

- not all profile fields make sense to send in profile update to group members
- changes to displayName, fullName, image should be sent
- changes to preferences aren't necessary to send as they only apply to user contacts
- changes to contactLink may be sent, but can also be excluded for purposes of privacy
  - some users don't expect that sharing address (contactLink) shares it not only with contacts, but also group members
  - this is a broader issue, as the user's contact link may also be sent in user's profile by admin when introducing members - it makes sense to either ignore this for the purposes of this feature, of change it in handshake as well
- it then makes sense to remember new timestamp on users record only if name or image is changed
  - users.last_profile_update_ts -> users.last_name_or_image_update_ts?

### When to send

- when user is active in group (i.e. broadcasts message via sendGroupMessage), compare group_members.last_profile_sent_ts against users.last_name_or_image_update_ts to determine whether latest profile update wasn't yet sent
- perhaps it doesn't make sense to send profile updates when sending service messages to individual members, as 1. members would have different profiles of user in different time points
