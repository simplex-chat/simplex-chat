# Perf — index member merge in `setGroupMembers` to O(n)

**PR:** #7061 (`nd/group-members-merge-on2`)
**Scope:** client-only (multiplatform: android + desktop). One-line change, no behavioral change.
**File:** `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chatlist/ChatListNavLinkView.kt:254`

## Root cause (verified)

`setGroupMembers` is the shared loader for the in-memory member list. After fetching
members from core (`apiListMembers`) it merges the freshly-loaded list with the
connection stats already held in memory, so an in-flight `connectionStats` isn't lost
when the list is reloaded — `ChatListNavLinkView.kt:257-267`:

```kotlin
val currentMembers = chatModel.groupMembers.value
val newMembers = groupMembers.map { newMember ->
  val currentMember = currentMembers.find { it.id == newMember.id }   // O(n) scan, inside an O(n) map → O(n²)
  ...
}
```

Two compounding costs:

1. **O(n²) merge.** `currentMembers.find { it.id == newMember.id }` is a linear scan
   run once per new member — `n` lookups × `n` scan = O(n²).
2. **~n² String allocations + GC pressure.** `GroupMember.id` is a *computed* property,
   not a stored field — `ChatModel.kt:2424`:

   ```kotlin
   val id: String get() = "#$groupId @$groupMemberId"
   ```

   Every `it.id` and `newMember.id` access allocates a fresh `String`. The nested
   `find` evaluates `it.id` for (worst case) every current member on every iteration,
   so the merge allocates on the order of n² short-lived strings, each compared by
   value. In groups with thousands of members this is a visible main-thread lag spike.

## Worst case observed

`setGroupMembers` reloads `chatModel.groupMembers` (and runs the merge) whenever the
member list is (re)loaded while members are already in memory. The most noticeable
case: the **Chats with members** support-chat modal (`MemberSupportView`) reloads the
whole list via `LaunchedEffect(Unit) { setGroupMembers(...) }` every time it (re)enters
composition — e.g. after reading and closing a member's support chat — so it pays the
full O(n²) merge and produces a lag spike on close in large groups
(`MemberSupportView.kt:44`).

## The fix (minimal — one change)

Index the current members by id **once**, then look up in O(1):

```kotlin
val currentMembersById = chatModel.groupMembers.value.associateBy { it.id }
val newMembers = groupMembers.map { newMember ->
  val currentMember = currentMembersById[newMember.id]
  ...
}
```

- `associateBy { it.id }` builds the index in a single O(n) pass; each subsequent
  lookup is O(1). Total merge cost drops from O(n²) to O(n).
- String allocations drop from ~n² to ~2n (one `it.id` per current member while
  building the map, one `newMember.id` per lookup).

## Why it's safe (no behavioral change)

- **Member ids are unique** — `id = "#$groupId @$groupMemberId"` is unique per member
  within a group, and `setGroupMembers` always works within a single `groupInfo`. So
  `associateBy { it.id }` cannot collide; `map[id]` returns exactly what
  `find { it.id == id }` returned.
- Same result set, same merged `GroupMember` objects, same order of `newMembers`
  (the `map` over `groupMembers` is unchanged). Only the lookup strategy changes.
- Everything downstream of the merge is untouched — `groupMembersIndexes`,
  `groupMembers.value`, `membersLoaded`, `populateGroupMembersIndexes()`
  (`ChatListNavLinkView.kt:268-271`).

## Also sped up (same shared loader)

`setGroupMembers` is the common in-memory member loader, called from several screens;
all of them get the same speedup in large groups (identical results, just O(n)):

- Group member list / member management — `GroupChatInfoView` (`:121, :1250, :1267`)
- @-mention autocomplete — `GroupMentions` (`:119, :134`)
- Channel relays — `ChannelRelaysView` (`:38, :124`)
- Add members — `AddGroupView` (`:52`)
- The group chat's member load on open — `ChatView` (multiple sites)
- Chats with members — `MemberSupportView` (`:45, :67`)

## Verification

- Reasoned: ids unique within a group → `associateBy`/lookup is semantically identical
  to the linear `find`. No caller observes a difference.
- Manual: open **Chats with members** in a large group, read and close a member's
  support chat repeatedly — the lag spike on close should be gone. Member list,
  @-mentions, relays, and add-members screens should remain identical, just faster.
