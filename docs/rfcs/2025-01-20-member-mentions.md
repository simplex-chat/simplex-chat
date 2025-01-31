# Member mentions

## Problem

Mention members in the messages.

There are several UX objectives that mentions must deliver:
- to notify the user about the messages that mention the user or to reply to user's messages - groups must have this notification mode (already present in the API),
- to allow the user to navigate to unread mentions and replies,
- to highlight mentions and allow all users to open mentioned member profile - this is the least important objective.

## Solution

Message text should include the reference to the shared member display name:

```
Hello @name
```

or

```
Hello @'member name'
```

This is the same format that people already use and that is currently supported in the API. The name in the message should use the display name at the time of mention, both for backwards compatibility and for better view in compose field, and the message should additionally include the mapping from used display names to shared group member IDs, and the UI would show the current display name (at the time of loading the message to the view).

For this mapping the message JSON will include the array of mentions, as objects with properties `displayName` and `memberId`. This is to ensure the intent and that the fragments of text are treated as mentions.

Using an immutable `memberId` would prevent any race conditions and duplicate display names. The receiving client would show a local view name (display name or an alias), and might open a correct member card when mention is tapped.

As display names are not unique in the group, we should convert them to locally-unique names (per message), by appending _1, _2, as necessary, and the same locally unique names should be used in the mapping to member IDs. These locally unique names must NOT use local user aliases, and must NOT use localDisplayName, as otherwise it may leak information that is known only to the user's client.

There should be a reasonable limit on the number of mentions per message, e.g. 3. This is to prevent abuse, expensive processing both in the client and in super-peers that would have to forward member profiles if they were not forwarded before. This limit has to be enforced both on sending and receiving ends.

## UX for sending mentions

When a member types '@' character in the entry field, the app would show the paginated list of most recently active members, with search. This requires a separate API, and the same API can be used to show a paginated member list - loading the full list is already quite expensive with groups over 1-2k members.

## UX for navigating to mentions

The current circles with unread messages should indicate the number of unread mentions (including replies) above and below the view. Tapping the circle should navigate to the next unread mention, and not to the bottom/top of the conversation. Long-pressing the circle should offer the option to navigate to the top/bottom. In the absense of mentions, tapping circles would navigate to top/bottom.

## Message UI

Item text will include markdown elements for mentioned members. This will be used when rendering to show member display names or local aliases.

Chat items data will include the list of members used in the chat item, including view names and member IDs.

## Forwarding and saving to local items

When forwarding to another conversation or saving to notes a message with mentions the app should use:
- current display names instead of display names used in the message.
- remove mentions mapping from the message data.

## Schema

Two new columns for chat_items table:
- user_mention - 0 or 1 to indicate whether a message is a reply to user's message or mentions user.
- member_mentions - the object mapping display names to member IDs, either as JSON, or in a more economical comma-separated list of "ID:name" strings (or "ID:'member name'). This field can be processed to load mention information, with the limit of 3 mentions per message it's sufficient.
