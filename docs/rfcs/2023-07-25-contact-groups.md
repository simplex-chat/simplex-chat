# Contact groups

## Problem

Bad UX when communicating from multiple devices:
- Lack of data synchronization between devices.
- Participating in the same conversation from multiple devices requires manually adding each device to a group.
- For a direct (1-to-1) conversation, it has to be initially created as a group - a contact does not support adding devices to conversation. Not obvious use of groups.

## Solution

Full device synchronization is a complex feature and will not be discussed in this rfc. Instead a proposed solution is to support adding devices to a 1-to-1 conversation, using existing group technology.

Pros:
- Fully decentralized, no leading and following devices.
- No complex full state synchronization.
- More obvious UX than using groups for 1-to-1 conversations.
- Can be automated to include user devices into all \[new\] conversations.

Cons:
- Same downsides groups have - increased traffic, diverging group state, possible failures to establish connections (requires out-of-band "connection doctor"?).
- Muddies existing abstracts of contacts and groups. Adds new orthogonal complexity to many existing features - preferences, incognito, multi-profile, calls, etc.
- Requires cooperation of contact's client - to support new protocol message for contact-to-group upgrade / contact-group invitation link.
- Still not full data synchronization. Devices have different profiles and likely different lists of conversations.

### Possible approaches

1. Add device to contact conversation - upgrade contact to group. Optionally automate adding user devices to other conversations.
2. Managed list of user devices - non optionally automate adding to other conversations.

#### Upgrade contact to group

Pros:
- Allows to migrate existing contacts to multi-device conversation.

Cons:
- Button would be hidden in chat info.
- Requires upgrading contact to group on both sides of the conversation:
  - Replace contact with group, update entities - chat items, files, etc. How will files in progress migrate?
  - New protocol message for contact to perform same upgrade + introduction of new device.
- Implicit list of "my devices", unclear whether to automate adding devices for other contacts? Not as useful if it's not automated?

#### Managed list of user devices

Pros:
- New button in Settings - easier to discover than chat info button.
- User devices are automatically added to all new conversations.
- Start each conversation as a group? -> Doesn't require upgrading contact to group.
  - New invitation link type - contact-group.
  - Expanded info in confirmation (when invitation link is for regular contact but joining client has other devices).
- Can automatically add own devices to existing groups.
- Doesn't require tricky contact to group upgrade.

Cons:
- Cannot migrate existing contacts. Create all contacts going forward as contact-groups?
