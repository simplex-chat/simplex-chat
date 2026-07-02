# Fix: bulk "remove members" freezes UI for minutes (O(members × items))

Date: 2026-06-29
Branch: nd/fix-bulk-remove-members-perf

## Symptom

Slow-execution diagnostic fired on desktop:

```
Execution of function takes too long time: 353 seconds: java.lang.Exception
    at chat.simplex.common.views.helpers.UtilsKt.withBGApi(Utils.kt:39)
    at chat.simplex.common.views.chat.group.GroupChatInfoViewKt.removeMembers(GroupChatInfoView.kt:1366)
```

`withBGApi` (`Utils.kt:38`) wraps work in `wrapWithLogging`, which logs an exception when the block
exceeds the `slow` threshold (20 s). The whole `removeMembers { … }` block ran for 353 s, freezing the
UI (all of the work runs on `Dispatchers.Main`).

## Root cause (multiplatform — Android + desktop)

`removeMembers` (`GroupChatInfoView.kt`) is reachable from **bulk multi-select removal**
(`GroupChatInfoView.kt:838`, `removeMembersAlert(rhId, groupInfo, selectedItems.value!!.sorted())`),
so the member list `M` can be large ("select all → remove").

The body looped over every removed member and, **per member**, rebuilt the entire open-chat item list:

- `ChatsContext.upsertGroupMember` (`ChatModel.kt`): `chatItems.value.map { … }` (O(N)) +
  `replaceAll` (O(N)) — `replaceAll` allocates a brand-new `SnapshotStateList<ChatItem>` and copies all
  N items (`ChatModel.kt`, `fun MutableState<SnapshotStateList<T>>.replaceAll`).
- `ChatsContext.removeMemberItems` (`ChatModel.kt`), when `withMessages` and `fullDelete`:
  `removeAllAndNotify` — another full O(N) `SnapshotStateList` rebuild.

With `M` removed members and `N` items in the open chat this is **O(M × N)** snapshot rebuilds, plus `M`
LazyColumn recompositions, all on the Main thread → 353 s.

Secondary bug: the second `withContext(Dispatchers.Main)` block (intended for `secondaryChatsContext`)
called `chatModel.chatsContext.removeMemberItems(...)` (primary context) instead of
`secondaryChatsContext` — so removeMemberItems ran on the primary context **twice** and was skipped on
the secondary context.

## Fix (batch the model ops → O(N))

Process the open-chat item list **once** for the whole set of removed members instead of once per member.

### `common/.../model/ChatModel.kt`

- Added batched `removeMemberItems(rhId, removedMembers: List<GroupMember>, byMember, groupInfo)`:
  a single pass over chat items, matching any item whose member is in the removed set
  (`isRemovedMemberItem` now tests membership against a `Set<Long>` of removed ids).
  The original single-member overload now delegates:
  `removeMemberItems(rhId, removedMember, …) = removeMemberItems(rhId, listOf(removedMember), …)`
  — provably equivalent (for one id, `id in {x}` ≡ `id == x`).
- Added batched `upsertGroupMembers(rhId, groupInfo, members: List<GroupMember>)`:
  one pass over `chatItems` (single `replaceAll` only if something changed) and one rebuild of the
  members list + indexes for all members. The single-member `upsertGroupMember` is left untouched
  (used by many event handlers); its `Boolean` return is unused by all callers.

### `common/.../views/chat/group/GroupChatInfoView.kt`

`removeMembers` now calls the batched functions once per context, and the second block correctly targets
`secondaryChatsContext` (bug fixed):

```kotlin
withContext(Dispatchers.Main) {
  chatModel.chatsContext.updateGroup(rhId, updatedGroupInfo)
  chatModel.chatsContext.upsertGroupMembers(rhId, updatedGroupInfo, updatedMembers)
  if (withMessages) {
    chatModel.chatsContext.removeMemberItems(rhId, updatedMembers, byMember = groupInfo.membership, groupInfo)
  }
}
withContext(Dispatchers.Main) {
  chatModel.secondaryChatsContext.value?.upsertGroupMembers(rhId, updatedGroupInfo, updatedMembers)
  if (withMessages) {
    chatModel.secondaryChatsContext.value?.removeMemberItems(rhId, updatedMembers, byMember = groupInfo.membership, groupInfo)
  }
}
```

Complexity: **O(M × N) → O(N + M)**.

The `withBGApi` slow threshold was intentionally left unchanged — with the quadratic gone the diagnostic
will not fire for normal removals, and keeping the 20 s threshold preserves its value as a real-problem
signal.

## iOS: not affected (no fix needed)

The same per-member loop exists in iOS (`apps/ios/.../GroupChatInfoView.swift` `removeMember`,
`apps/ios/.../ChatModel.swift` `removeMemberItems`/`upsertGroupMember`), **but the quadratic cannot
occur on iOS**:

- iOS has **no bulk multi-select member removal**. `apiRemoveMembers` is only ever called with a single
  member: `apiRemoveMembers(groupInfo.groupId, [mem.groupMemberId], withMessages)`
  (`GroupChatInfoView.swift:1006` — the only call site in the app). So `M` is always 1 and the loop runs
  once → O(N).
- iOS `upsertGroupMember` (`ChatModel.swift:1280`) only updates the `groupMembers` array; it does **not**
  scan `reversedChatItems`, so it lacks even the per-call O(N) item pass that the desktop version has.

If iOS ever gains bulk member removal, mirror the multiplatform batching (batched `removeMemberItems` +
batched `upsertGroupMembers`) at that time.

## Verification

- `./gradlew :common:compileKotlinDesktop` — BUILD SUCCESSFUL (only pre-existing deprecation warnings).
- `./gradlew :common:desktopTest --tests "chat.simplex.app.RemoveMembersPerfTest"` — passes.

### Performance test

`RemoveMembersPerfTest` builds a group with 100 members whose open chat holds 2050 items,
then runs the OLD per-member loop and the NEW batched calls on identical fresh state. It
asserts both produce the same (and correct) result — only the non-removed member's items
remain, and removed members are marked removed — and that the batched path is faster.

Measured (JVM, one run):

```
========= remove-members performance =========
 members removed : 100
 chat items      : 2050
 old (per-member): 1291.3 ms   O(members * items)
 new (batched)   : 12.1 ms     O(members + items)
 speedup         : 107.0x
==============================================
```

## Files changed

- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/model/ChatModel.kt`
- `apps/multiplatform/common/src/commonMain/kotlin/chat/simplex/common/views/chat/group/GroupChatInfoView.kt`
- `apps/multiplatform/common/src/desktopTest/kotlin/chat/simplex/app/RemoveMembersPerfTest.kt`
