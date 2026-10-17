# Channel creation stops updating when an owned channel's relays are opened

Follow-up of #7746 (builds on its branch).

## Problem

On desktop the channel creation flow runs in the left pane while a chat stays open next to it. With a channel you own open, opening its "Chat relays" while another channel was being created left the creation at "0/3 relays active": it never updated again, also after the relays view was closed, although the channel was ready (after a restart it was a normal channel with its relays). A user would likely cancel it, which deletes the channel.

In the other direction, a creation hid the open channel's relay bar (shown while some of its relays are not active) during the creation and, after a cancel, until the channel was opened again.

Reproduced on master with the GUI (desktop).

## Cause

`ChannelRelaysModel` holds the relays of one channel (`groupId` + `groupRelays`). It was used both by the channel being created (`AddChannelView`: `set` when it is created, read by the progress step, `reset` on cancel and when ready) and by an open channel you own (`ChatView` when it opens, `ChannelRelaysView`, `AddGroupRelayView`; read by the relay bar in `ComposeView`). `GroupRelayUpdated` and `GroupLinkDataUpdated` update it only for its current `groupId`.

When the open channel's relays view loaded its relays, the model switched to that channel; the progress step ignores values of another channel and the events for the channel being created were dropped, so its progress stopped. The creation's `set` and `reset` in turn replaced or cleared the open channel's relays.

## Fix

Keep the relays of the channel being created in their own list, `ChatModel.creatingChannelRelays` (like `creatingChannelMembers` in #7746):
- set from the creation result, before the channel is marked as being created;
- `GroupLinkDataUpdated` replaces it and `GroupRelayUpdated` adds or replaces a relay when the event is for the channel being created;
- the progress step reads it; `AddChannelView` no longer writes `ChannelRelaysModel`.

Alternatives considered:
- Only drop the progress step's `groupId` check: it would show, and could complete on, the open channel's relays.
- A per-channel map in `ChannelRelaysModel`: changes every reader.
- Updating the new list inside `ChannelRelaysModel.updateRelay`: ties it to `ChatModel`; the `GroupLinkDataUpdated` update stays in `SimpleXAPI.kt` anyway.

Behavior change: after "Continue" with relays still pending, the new channel's relay bar shows once its chat has loaded its relays (as after an automatic completion), not at once.

Not changed here:
- iOS likely has the same bug: it shares its `ChannelRelaysModel` between the channel being created and an open channel the same way (`AddChannelView.swift`, `ChannelRelaysView.swift`, `ChatView.swift`).
- `ChatView`'s cleanup on close still skips while a channel is being created; with this change and #7746 the channel being created no longer uses that state.

## Verification

Desktop Linux AppImage, #7746's head (before) vs this branch (after).

The bug needs the relays view to load while the creation is in progress; relays usually connect in ~10 s, so on plain builds it was hit once in 5 attempts on master and not at all in 2 counted attempts on #7746's head. The bug check therefore ran on test builds of both that hold back each relay update of the channel being created by 5 s (not part of this change):

| # | Check | before | after |
|---|---|---|---|
| D0 | Creation with no other action | completes (~40 s) | completes (~40 s) |
| D1 | Channel chX open, create chY, open chX's "Chat relays" at 0/3 | chY stays at "0/3 relays active", all relays "invited", for 150 s, and 30 s more after the view is closed; after a restart chY is a normal channel with 3 active relays | 1/3, then chY opens ~17 s later |

Plain builds, identical on both: chX's "Chat relays" lists its 3 active relays; a creation completes, opens the channel with its link page, its info lists 3 relays and messages send; cancel removes the channel and chX's messages send; no relay bar above chX's composer (all relays active) before and after a creation and a cancel. No crashes or `E:` log lines.

Android (Waydroid debug APKs, #7746's head vs this branch, same snapshot), identical: creation, "Continue" to the channel and sending, cancel, an owned channel's "Chat relays" and sending. No crashes.
