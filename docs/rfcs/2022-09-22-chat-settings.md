# Chat settings

Scope: this doc covers permissions and configuration that is specific for one contact or group, and should or should not be taken into account when sending messages.

## Problem

Certain chat features are not desirable for some contacts/groups, some other require contact specific configuration.

These settings can be symmetric or asymmetric. Symmetric settings require mutual agreement. Asymmetric can be set by one party independently of the other. The focus of this RFC is asymmetric settings, as they are simpler - they require one-way notification for their change.

These settings can be local or remote. Local settings only affect chat functionality for a given contact or group, but are not known to the remote parties. For each type of local setting user only needs one value per chat. Remote settings should be taken into account when sending the message, to avoid rejected/ignored messages. For each type of remote setting user needs two values per chat - the value received from the other party (it should be taken into account when sending the message) and the te value sent to other party (that should be taken into account when processing received messages).

Local settings can be implemented as an extension of global user settings, they are not the focus of this RFC.

This RFC focus is asymmetric remote settings that include:

- permission to send voice messages - some users prefer to not receive them.
- permission to delete sent messages and the maximum duration when it is allowed.
- permission to send images - e.g. some automatic clients/bots may not support images, and this would explicitly prohibit it.
- permission to edit sent messages and the maximum duration when it is allowed.

These permissions are not taken into account for group memberships, instead group permissions are that are set by group owners.

## Solution

Protocol:

Broadcast user default permissions in the same way as profile updates by adding `permissions` property to the user profile - it will be sent as part of `x.info` message.

For groups these are also added to group profile and sent via `x.grp.info` message.

`permissions` property is a dictionary with boolean or number values - clients must ignore unknown values.

Current schema for `permissions` member of user profile is:

```json
{
  "properties": {
    "voice": {"type": "boolean"},
    "image": {"type": "boolean"},
    "file": {"type": "boolean"},
    "delete": {"type": "boolean"},
    "edit": {"type": "boolean"}
  },
  "additionalProperties": true
}
```

Every time user updates the settings and update profile should be sent to affected contacts.

Database schema:

```sql
ALTER TABLE contact_profiles ADD COLUMN permissions TEXT CHECK (permissions NOT NULL);
UPDATE contact_profiles SET permissions = '{}';

ALTER TABLE group_profiles ADD COLUMN permissions TEXT CHECK (permissions NOT NULL);
UPDATE group_profiles SET permissions = '{}';

ALTER TABLE contacts ADD COLUMN permissions TEXT NULL;
```

An alternative option is to start storing the whole profile in the existing properties field - that would better allow for forward compatibility in case some other property is added.
