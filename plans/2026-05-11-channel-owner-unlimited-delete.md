# Channel editorial delete from history

## Problem

Channel owners cannot delete content older than 24 hours. The limit makes sense in p2p groups (no authority, each member holds independent copy). In channels, the owner is the authority - their content, their right to remove it.

## Design

Rather than bypassing the 24-hour time limit for broadcast deletes, we add a new delete mode: "delete from history." The relay removes the message from its store but does not forward the deletion to subscribers. Subscribers who already received the message keep it - the operation cleans the relay's history, not the subscriber's device.

This is the right separation: within 24 hours, broadcast delete reaches subscribers and removes from relays. After 24 hours, history delete cleans relays only - no retroactive rewriting of subscriber devices.

## Changes

### Protocol.hs

`XMsgDel` gains `onlyHistory :: Bool` field. Defaults to `False` (backward compatible - old clients don't send it, parser defaults missing field to `False`). Encoded only when `True` via `justTrue`.

### CIContent.hs

New `CIDMHistory` delete mode alongside `CIDMBroadcast`, `CIDMInternal`, `CIDMInternalMark`.

### Commands.hs

`APIDeleteChatItem` group path:
- `CIDMHistory` - validates `publicGroupEditor` (channel + role >= GRModerator), sends `XMsgDel` with `onlyHistory = True`, no time check. Rejected for non-channels and insufficient role.
- `CIDMInternal` - rejected for channel editorial roles (they should use `CIDMHistory` instead).
- `CIDMBroadcast` - unchanged (24-hour time check applies to everyone).

### Subscriber.hs

Relay `processEvent`: when `XMsgDel` has `onlyHistory = True`, processes the delete locally (marks item in relay's store) but returns `Nothing` for the delivery task - no forwarding to subscribers.

Subscriber `processForwardedMsg`: ignores the `onlyHistory` flag (wildcard match) - if a message somehow arrives, process as normal delete.

### Types.hs

`publicGroupEditor :: GroupInfo -> GroupMember -> Bool` - shared predicate for channel editorial role check.

### UI (iOS + Kotlin)

Delete dialog for channel editorial roles (owner/admin/moderator):
- First button: "Delete from history" (iOS) / "From history" (Kotlin) - always available, sends `CIDMHistory`
- Second button: "For everyone" - only within 24-hour window, sends `CIDMBroadcast`
- No "Delete for me" option for editorial roles

Batch selection follows the same logic - "From history" replaces "Delete for me" for editorial roles.

API error handling: `apiDeleteChatItems` and `apiDeleteMemberChatItems` now show error alerts on failure (was silently swallowed on Android).

## Backward compatibility

- Old relays: don't recognize `onlyHistory`, process `XMsgDel` normally (forward to subscribers). Acceptable - the delete reaches subscribers, which is more than the owner intended but not harmful.
- Old subscribers: `onlyHistory` field is ignored (not in their parser, and the relay won't forward it anyway).
- Old owners: never send `onlyHistory = True`, behavior unchanged.

## Test

`testChannelMessageDeleteFromHistory` - owner sends message, deletes with `history` mode, verifies relay processes locally but subscribers don't receive deletion, verifies `internal` mode is rejected for channel editorial roles.
