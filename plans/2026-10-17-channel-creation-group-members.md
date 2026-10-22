# Creating a channel replaces the open group's members

Follow-up of #7726 (builds on its branch).

## Problem

On desktop the channel creation flow runs in the left pane while a chat stays open next to it. With a group's info open, creating a public channel made the group's info list the channel's relays as its members (e.g. "4 members" — you and 3 relays — instead of "2 members"). After "Cancel and delete channel" the group's info kept them until it was shown again.

On Android the same happened when a group was opened from a notification while the channel was being created: the notification closes the creation screen but leaves the channel marked as being created, and its relay updates then replaced the open group's members.

Reproduced on master with the GUI (desktop); the Android path is from the code.

## Cause

`ChatModel.ChatsContext.upsertGroupMember` updated `groupMembers` for the open chat or for the channel being created (`chatId == groupInfo.id || creatingChannelId == groupInfo.id`). `groupMembers` is one list, rendered by the open group's info. The first relay update of the channel being created found members of another group in it, treated them as stale and cleared them, then added the relays. `membersLoaded` stayed true, so nothing reloaded them. The creation screen read the same list for relay connection errors (`relayMemberConnFailed`).

The same list also feeds the open group's mentions, member support and, for a channel, its relay bar, which were affected the same way. While the open group's members were loaded or updated, the creation screen in turn lost the relay connection errors it had read from the shared list.

## Fix

Keep the members of the channel being created in their own list, `ChatModel.creatingChannelMembers`:
- `upsertGroupMember` adds or replaces a member there when it belongs to the channel being created, and updates `groupMembers` only for the open chat.
- `relayMemberConnFailed` reads the new list.
- The list is cleared when a creation starts, before the channel is marked as being created, so members of an earlier creation (whose ids can be reused after the cancelled channel's rows are deleted) are never read.

Alternatives considered:
- Store the relays in `groupMembers` only while no chat is open. Rejected: with a group open, the creation screen would lose relay connection errors.
- Reload the open group's members after the creation ends. Rejected: the wrong list still shows during the creation.

Not changed here:
- `ChannelRelaysModel` is also shared between the channel being created and an open channel you own (its relay bar disappears during creation); fixed separately, see `2026-10-17-channel-creation-relays.md`.
- When the creation screen is closed from outside (e.g. a notification on Android), the channel stays marked as being created; its member updates now go only to the separate list.

## Verification

Desktop, #7726's branch head (before) vs this branch (after), the same test plan on fresh profiles (two contacts; two groups of 2 members), channels created with the default chat relays:

| # | Check | before | after |
|---|---|---|---|
| E1 | Group A's info open, create a channel; while relays connect | the count grows with the relays (2 → 3 → 4 members) | "2 members" |
| E1 | Then "Cancel and delete channel", 5 s later | "2 members": you and "SimpleX Chat Relay 2", A's member missing | "2 members": you and A's member |
| E1 | Info shown again | correct | correct |

Identical on both: a channel whose creation completes opens with its info (subscribers, chat relays) and its channel link; going back to group A shows its members; the creation progress ("N/3 relays active", the relays list, the cancel dialog); a member leaving the open group updates its count; switching between groups with the info open shows each group's own members. No crashes; no `E:` log lines other than `apiGetGroupLink … groupLinkNotFound`.

Android (Waydroid debug APKs, #7726's head vs this branch, from the same app snapshot), identical on both: channel creation progress, completion (the channel's subscribers and chat relays), cancel (the open group keeps its members). The Android path of the bug (a group opened from a notification during the creation) is traced in code; one attempt on the GUI did not hit it on either build (the group showed its own members).
