# Member mentions

## Problem

Mention members in the messages.

## Solution

Message text can include references to the shared member IDs in base64 format:

```
Hello @'abcd1234'
```

In addition, the message JSON will include the array of mentions, as objects with property `memberId`. This is to ensure the intent and that the fragments of text are treated as mentions.

Using an immutable `memberId` would prevent any race conditions and duplicate display names. The receiving client would show a local view name (display name or an alias), and would open a correct member card when mention is tapped.

There should be a reasonable limit on the number of mentions in a group, e.g. 3. This is to prevent abuse, expensive processing both in the client and in super-peers that would have to forward member profiles if they were not forwarded before. This limit has to be enforced both on sending and receiving ends.

## UX

When a member types '@' character in the entry field, the app would show the paginated list of most recently active members, with search. This requires a separate API, and the same API can be used to show a paginated member list - loading the full list is already quite expensive with groups over 1-2k members.

## UI

Item text will include markdown elements for mentioned members. This will be used when rendering to show member display names or local aliases.

Chat items data will include the list of members used in the chat item, including view names and member IDs.

## Forwarding and saving to local items

When forwarding a message with mentions to another conversation the app should forward:
- display names instead of member IDs in the text.
- remove mentions from the message data.

When saving a message to local chat items the app retains the reference to the original group, so it could include member IDs. It means that member data should be loaded from the correct group. Alternatively, the reference could include one of:
- both member ID and display name at the time of saving to the local item.
- only display name, and no mentions.
