# Open Member Profile Without Loading All Group Members

## Context

Tapping a member's avatar in chat history takes several seconds in a group with
10000 members, on every tap, on Android and desktop.

**Root cause**: `showMemberInfo` (ChatView.kt:499) awaits three API calls before
showing the modal:

1. `apiGroupMemberInfo` — single member, O(1) in group size
2. `apiGetGroupMemberCode` — single member, O(1)
3. `setGroupMembers` (ChatListNavLinkView.kt:254) — `apiListMembers`, the **whole
   member list**

Call 3 is the cost. `APIListMembers` (Commands.hs:3212) runs `getGroup`, which
loads every member with its profile (Groups.hs:938, 1222), the profile includes
the avatar (`p.image` in `groupMemberQuery`, Shared.hs:762), and the result is
encoded to JSON, passed across the FFI boundary and decoded into 10000
`GroupMember` objects by the client. The codebase already annotates this call as
"very heavy query in large groups" (SimpleXAPI.kt:846).

There is no `membersLoaded` guard on this call, unlike `GroupMentions.kt:116`, so
the full list is re-loaded on *every* tap even when it is already in the model.

Measured on a 10009-member group (real member rows, half with a 11.7 KB avatar):
the SQL itself takes 0.21 s and returns ~64 MB of column data (5.1 MB without
avatars). The seconds are the JSON encode/transfer/decode of that payload.

The full member list is not needed to show one member's profile. It is loaded
only so that `chatModel.getGroupMember` (ChatModel.kt:357) resolves the member
for the modal (ChatView.kt:521) and for the "Verify security code" screen
(GroupMemberInfoView.kt:209).

## Solution Summary

Do not load the member list on this path. Add the opened member to the model
instead, and show the modal — this is what the iOS app already does
(ChatView.swift:2051-2058, since 03bc4e5d0, "ios: display reactions in groups by
member"). The Kotlin path was never updated to match.

```kotlin
val (updatedMember, code) = if (member.memberActive) {
  val memCode = chatModel.controller.apiGetGroupMemberCode(...)
  (memCode?.first ?: r?.first ?: member) to memCode?.second
} else {
  (r?.first ?: member) to null
}
if (!isActive || chatModel.chatId.value != groupInfo.id) return@launch
withContext(Dispatchers.Main) {
  chatModel.chatsContext.upsertGroupMember(chatRh, groupInfo, updatedMember)
}
```

After the change the tap runs two single-row queries. Measured against the core
with 10009 members and 100035 messages in the group: `APIGroupMemberInfo` takes
1-2 ms, first call included, and does not depend on group size.

## Technical Design

### Which member is added to the model

The member returned by `apiGetGroupMemberCode` is preferred over the one from
`apiGroupMemberInfo`. `APIGetGroupMemberCode` (Commands.hs:2016) clears
verification in the database when the peer's security code no longer matches
(`setGroupMemberVerified ... Nothing` / `setConnectionVerified ... Nothing`) and
returns the updated member. `apiGroupMemberInfo` runs before that, so its member
can still show the connection as verified. The previous code re-read all members
from the database *after* the code call, so the model saw the cleared state;
using the code call's member preserves that behaviour, and the verified shield
(GroupMemberInfoView.kt:736) does not go stale.

### Guard on the open chat

`upsertGroupMember` (ChatModel.kt:927) is a no-op when the open chat changed
while the two calls were in flight, which would leave `getGroupMember` null and
open an empty card. The explicit `chatModel.chatId.value != groupInfo.id` check
closes the modal path in that case instead. The previous code filled the model in
that race by writing the *previous* group's members into it — the stale data
hazard that `upsertGroupMember`'s own comment warns about (ChatModel.kt:936).

### Duplicate protection is retained

`#5462` ("improving group members loading to prevent crashes") made the wholesale
replacement safe against duplicated entries crashing `LazyColumn`.
`upsertGroupMember` carries the same protection: it clears the list when the
first member belongs to another group (ChatModel.kt:936) and looks the member up
by index before appending (ChatModel.kt:940, 956-966).

## Consequences

`chatModel.groupMembers` can now hold a partial list (previously it was either
empty or complete). This state already existed — channel creation writes a
relays-only list (ComposeView.kt:693) — and `membersLoaded` is deliberately left
`false`, so every screen that needs the full list still loads it:
`GroupChatInfoView.kt:117`, `ChannelMembersView`, `ChannelRelaysView.kt:38`,
`MemberSupportView.kt:45`, `addGroupMembers` (ChatView.kt:3217), and
`GroupMentions.kt:116` which checks the flag.

Two behaviours change:

- **Relay removal warning.** `activeRelays.size <= 1`
  (GroupMemberInfoView.kt:250, GroupChatInfoView.kt:277) is computed from the
  model. A partial list is a subset, so the count can only be lower, and the
  warning can only fire when it should not — under-warning is impossible. It
  affects the confirmation text only, not the removal itself, and applies only to
  relay channels, which load all members on chat open (ChatView.kt:213).
- **Mention picker.** Until `@` triggers the load, the picker briefly lists the
  members opened so far instead of nothing. Self-correcting.

The bulk refresh of all members that happened as a side effect of every tap is
gone; each screen refreshes its own data on open.

## Alternatives Rejected

- **Guard the load with `!membersLoaded`** — cures repeat taps, leaves the first
  tap in a group costing seconds.
- **No model write, fall back to the chat item's member in the modal** — smaller
  diff, but "Verify security code" (GroupMemberInfoView.kt:209) resolves the
  member through the model too and would open empty; it would need a second
  fallback in another file.
- **Move `apiGroupMemberInfo`/`apiGetGroupMemberCode` into `GroupMemberInfoView`
  behind a `connectionLoaded` gate (full iOS parity, GroupMemberInfoView.swift:291)** —
  opens the card with no API calls at all, but changes the view's signature, three
  call sites and two previews, and makes the connection rows appear after the card
  on every open. Worth doing separately; it does not affect the cost removed here.

## Out of Scope

- The first profile opened after entering a large group is still slower than the
  rest. The core is not the cause (1-2 ms measured, agent call included); the tap
  waits for the UI thread, which is busy composing the just-loaded page of
  messages (`MergedItems.create` over all loaded items, ChatView.kt:1800) and
  decoding avatars. Fixing it means making chat opening cheaper, or adding the
  member to the model synchronously in the click handler as iOS does.
- Opening a second member's profile while one is already open does not switch the
  card: `showInView` drives `AnimatedContent` from `modalCount` alone
  (ModalView.kt:207), while `modalViews` is a plain list, so a close and open in
  one frame leaves the target state unchanged. Pre-existing, unrelated to this
  change.
- Message *Info* (ChatView.kt:706) still loads all members; it needs them to
  resolve delivery recipients (ChatItemInfoView.kt:550). A narrower core API
  would be required.
