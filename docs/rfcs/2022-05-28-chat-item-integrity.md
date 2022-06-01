# Chat item integrity

## Problem

SMP agent reports skipped, duplicate, incorrect IDs and bad message hashes, and this event is also shown to the terminal users.

This is not shown to mobile app users at the moment, as there is nothing in the data model to persist this information.

While message hash violations have never happened so far, skipped messages happen every time we restart the server, until we introduce server redundancy.

It would be helpful to the users to know when they have skipped messages rather than to check with all their contacts if they do.

## Solution

The proposed types/data model differentiates the integrity errors that are related to a particular item (they are saved to item meta-data, and should be shown as item status in the UI) and the errors that indicate skipped messages (these are created as separate chat items, and should be shown in the UI as a separate chat item).

This [PR #705](https://github.com/simplex-chat/simplex-chat/pull/705) only implements chat item for skipped messages, for the remaining message integrity errors it still uses the event CRMsgIntegrityError that is only displayed in the terminal.
