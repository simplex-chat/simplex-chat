# Deduplication Plan: Channel Message Functions

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Findings by File](#findings-by-file)
3. [Architectural Note: CIChannelRcv Constructor](#architectural-note)
4. [Implementation Order](#implementation-order)

---

## Executive Summary

The PR introduces channel message support by creating parallel channel-specific functions that duplicate 60-80% of existing group functions. The core pattern: channel messages are group messages without a member sender. Most channel functions are the group function with `Just member` → `Nothing`, `CIGroupRcv m` → `CIChannelRcv`, and moderation/blocking guards removed.

**High-value deduplication targets** (ordered by impact):

| # | Candidate | Feasibility | Shared code |
|---|-----------|-------------|-------------|
| 1 | `channelMessageUpdate_` → merge into `groupMessageUpdate` | HIGH | ~36 lines |
| 2 | `fwdChannelReaction` → extract shared helper with `groupMsgReaction` | MEDIUM | ~15 lines inner function |
| 3 | `newChannelContentMessage_` → parameterize `newGroupContentMessage` | MEDIUM | ~12 lines happy path |
| 4 | `processForwardedChannelMsg` → merge into `processForwardedMsg` | MEDIUM | depends on 1-3 |
| 5 | `getGroupCIBySharedMsgId'` → parameterize `getGroupChatItemBySharedMsgId` | HIGH | eliminates function |
| 6 | `channelMessageDelete` → parameterize `groupMessageDelete` | LOW | ~5 lines; group has 60+ lines moderation |
| 7 | `saveRcvChatItem'` CDChannelRcv branches | HIGH | ~14 lines across 3 spots |
| 8 | `processContentItem` CIChannelRcv branch | HIGH | ~3 lines |
| 9 | View.hs/Store/Internal pattern match branches | DEFERRED | ~24 branches; requires constructor change |

---

## Findings by File

### Subscriber.hs

**D1: `channelMessageUpdate_` vs `groupMessageUpdate`**

The `updateRcvChatItem` inner function is nearly line-for-line identical between both (~36 shared lines). Differences:
- Lookup: `getGroupChatItemBySharedMsgId` (by member) vs `getGroupCIBySharedMsgId'` (no member) — parameterizable by `Maybe GroupMemberId` (see D5)
- Pattern match: `CIGroupRcv m'` with `sameMemberId` check vs `CIChannelRcv` — branch on `Maybe GroupMember`
- `getGroupCIReactions`: `Just memberId` vs `Nothing` — already parameterized
- Chat direction in fallback: `CDGroupRcv` vs `CDChannelRcv` — branch on `Maybe GroupMember`
- `channelMessageUpdate_` has explicit `forwarded` param; `groupMessageUpdate` always uses `rcvGroupCITimed gInfo ttl_` — the merged function needs to accept `forwarded :: Bool` (or always `False` from the non-forwarded path)
- `groupMessageUpdate` has `prohibitedSimplexLinks` and `blockedMemberCI` guards — skip when member is `Nothing`
- Mentions handling: `groupMessageUpdate` has `mentions' = if memberBlocked m then [] else mentions`; `channelMessageUpdate_` passes `mentions` directly — when member is `Nothing`, use `mentions` directly (no blocking check needed)

**Solution:** Extend `groupMessageUpdate` to take `Maybe GroupMember`. When `Nothing`: skip prohibited links check, skip blocked member CI, use `CDChannelRcv`, use `getGroupChatItemBySharedMsgId` with `Nothing`, pass mentions directly. Delete `channelMessageUpdate_`.

---

**D2: `fwdChannelReaction` vs `groupMsgReaction`**

These functions share the `updateChatItemReaction` inner function shape (~15 lines), but are **structurally different** in their outer logic:

- **Parameter types**: `groupMsgReaction` takes a concrete `GroupMember` + `Maybe MemberId` (item member) + `Maybe MsgScope`; `fwdChannelReaction` takes `Maybe GroupMember` (reactor) and always passes `Nothing` as item member
- **Return type**: `groupMsgReaction` returns `CM (Maybe DeliveryJobScope)` — used by the main dispatch for delivery job routing; `fwdChannelReaction` returns `CM ()` — forwarded context doesn't need delivery jobs
- **CIReaction constructor**: `groupMsgReaction` always uses `CIGroupRcv m`; `fwdChannelReaction` uses `maybe CIChannelRcv CIGroupRcv reactor_` — semantically different when reactor is `Nothing`
- **catchCINotFound fallback**: `groupMsgReaction` has scope-aware delivery job logic; `fwdChannelReaction` does bare `setGroupReaction`
- **Reactor**: `groupMsgReaction` uses `m` directly; `fwdChannelReaction` computes `fromMaybe membership reactor_`

`fwdChannelReaction` is NOT a rename of `groupMsgReaction`. Calling `void $ groupMsgReaction` from forwarded contexts would be **semantically wrong**: it would attribute channel reactions to the membership member via `CIGroupRcv` instead of showing them as `CIChannelRcv`, and would trigger unnecessary delivery job scope logic.

**Solution:** Extract the shared `updateChatItemReaction` body (~15 lines) into a helper parameterized by the `CIReaction` constructor and reactor member. Both `groupMsgReaction` and `fwdChannelReaction` call this helper with their respective parameters. This preserves the distinct outer logic while eliminating the inner body duplication.

---

**D3: `newChannelContentMessage_` vs `newGroupContentMessage`**

The channel version is the "happy path" of the group version with all member-specific guards removed:
- No `blockedByAdmin` check
- No `prohibitedGroupContent` check
- No `getCIModeration` / moderation logic (~40 lines)
- No scope resolution (`mkGetMessageChatScope`)
- No `blockedMemberCI`
- No member-conditional mentions filtering / autoAcceptFile guard

The shared "save-view-react-accept" core is ~12 lines.

**Solution:** Extract a shared `saveGroupContentItem` helper containing: process file invitation, save chat item, get reactions, view, auto-accept, return scope. `newGroupContentMessage` calls it after its checks; `newChannelContentMessage_` calls it directly. This keeps `newGroupContentMessage`'s complex flow intact while eliminating the body duplication.

Alternatively: extend `newGroupContentMessage` to take `Maybe GroupMember`. When `Nothing`: skip all member-specific guards and use `CDChannelRcv`. This is cleaner but changes the function's signature and control flow significantly.

---

**D4: `processForwardedChannelMsg` vs `processForwardedMsg`**

These are dispatch tables with identical structure. Each event arm calls the group or channel variant:

```
processForwardedMsg author:                    processForwardedChannelMsg:
  XMsgNew → newGroupContentMessage               XMsgNew → newChannelContentMessage_
  XMsgFileDescr → groupMessageFileDescription    XMsgFileDescr → channelMessageFileDescription
  XMsgUpdate → groupMessageUpdate                XMsgUpdate → channelMessageUpdate_
  ...                                            ...
```

If the underlying functions (D1-D3) are parameterized by `Maybe GroupMember`, this dispatch unifies automatically. The extra group-management events (`XInfo`, `XGrpMemNew`, etc.) are guarded by `Just author`.

**Subtlety: `XMsgReact` handling.** The `XMsgReact` arm has a three-way split:
- `processForwardedMsg` with `Just memId` → `groupMsgReaction` (member reaction with scope/delivery-job logic)
- `processForwardedMsg` with `Nothing` memId → `fwdChannelReaction gInfo (Just author)` (channel reaction from known author)
- `processForwardedChannelMsg` → `fwdChannelReaction gInfo Nothing` (channel reaction, no author)

This three-way split needs careful handling in the merged function, since `fwdChannelReaction` differs structurally from `groupMsgReaction` (see D2).

**Solution:** After D1-D3, merge into `processForwardedMsg` taking `Maybe GroupMember`. When `Nothing`, skip group-management events. The `XMsgReact` arm passes the author to `fwdChannelReaction` when in channel mode. Delete `processForwardedChannelMsg`.

---

**D5: `channelMessageDelete` vs `groupMessageDelete`**

`groupMessageDelete` has ~60 lines of moderation logic (moderate, checkRole, archiveMessageReports, CIModeration creation) that `channelMessageDelete` does not need. The shared portion is only ~5-7 lines (delete/mark-deleted + view). Additionally, the lookup functions differ: `channelMessageDelete` uses `getGroupCIBySharedMsgId'` (no member); `groupMessageDelete` uses `getGroupMemberCIBySharedMsgId` (JOINs group_members by MemberId). The delete condition also differs: `groupFeatureAllowed` vs `groupFeatureMemberAllowed`.

**Solution:** LOW priority. The functions are architecturally different enough that forced unification would harm readability. If desired, extend `groupMessageDelete` with a `Maybe GroupMember` parameter where `Nothing` takes the simple "channel delete" path early. But the code clarity cost may exceed the deduplication benefit.

---

### Store/Messages.hs

**D6: `getGroupCIBySharedMsgId'` vs `getGroupChatItemBySharedMsgId`**

`getGroupChatItemBySharedMsgId` filters by `group_member_id = ?`.
`getGroupCIBySharedMsgId'` omits the `group_member_id` filter entirely (matches any row regardless of member).

Channel items store `group_member_id = NULL`. Parameterizing with `Maybe GroupMemberId` and `IS NOT DISTINCT FROM` would:
- `Just gmId` → only that member (existing behavior)
- `Nothing` → only NULL rows (channel items)

This is **stricter** than `getGroupCIBySharedMsgId'`'s current behavior (which matches any member's items too), but this is actually a correctness improvement — all four callers (Subscriber.hs lines 1846, 1962, 1988, 3233) are channel-specific contexts where items have `group_member_id = NULL`.

**Solution:** Change `getGroupChatItemBySharedMsgId` to take `Maybe GroupMemberId`. SQL becomes:
```sql
WHERE user_id = ? AND group_id = ? AND group_member_id IS NOT DISTINCT FROM ? AND shared_msg_id = ?
```
Delete `getGroupCIBySharedMsgId'`. Update all callers to pass `Just gmId` or `Nothing`.

**Note:** `getGroupMemberCIBySharedMsgId` is a different function (takes `MemberId`, JOINs `group_members` to resolve). It is NOT a duplicate and should be kept.

**Additional Store/Messages.hs duplications** (minor, collapse with constructor change):
- `createNewRcvChatItem` quoteRow (lines 560-563): `CDGroupRcv` and `CDChannelRcv` branches are verbatim identical
- `getChatItemQuote_` (lines 649-654): `CDChannelRcv` branch is a subset of `CDGroupRcv` (missing sender-specific case)
- `createNewChatItem_` idsRow/groupScope: `CDChannelRcv` branches repeat `CDGroupSnd`-like tuples

These are inherent to the separate constructor and collapse automatically with the architectural change (see note below). Not worth addressing independently.

---

### Library/Internal.hs

**D7: `saveRcvChatItem'` CDChannelRcv branches**

Three duplicate spots within this function, all verbatim copies of CDGroupRcv branches:

1. **Mentions/userMention computation** (~7 lines): `getRcvCIMentions`, `userReply` via `cmToQuotedMsg`, `userMention'` via membership check. Verbatim identical between CDGroupRcv and CDChannelRcv.

2. **createGroupCIMentions** (~2 lines): Both branches call `createGroupCIMentions db g ci mentions'` guarded by `not (null mentions')`. Identical.

3. **memberChatStats / memberAttentionChange** (~3 lines): Only difference is `Just m` vs `Nothing` passed to `memberAttentionChange`.

Total: ~14 lines of duplication across 3 spots.

**Solution:** Extract `GroupInfo` and `Maybe GroupMember` from either constructor at the top:
```haskell
case cd of
  CDGroupRcv g _s m -> (g, Just m)
  CDChannelRcv g _s -> (g, Nothing)
```
Then use the extracted values for all three spots. The `memberAttentionChange` call already takes `Maybe GroupMember`.

---

**D8: `processContentItem` CIChannelRcv branch**

Near-duplicate of `CIGroupRcv` branch (lines 1196-1199 vs 1200-1202). Only difference: no `blockedByAdmin` guard, passes `Nothing` instead of `Just sender`.

**Solution:** Merge the two branches:
```haskell
(CChatItem SMDRcv ci@ChatItem {chatDir, content = CIRcvMsgContent mc, file})
  | maybe True (not . blockedByAdmin) sender_ -> do
      fInvDescr_ <- join <$> forM file getRcvFileInvDescr
      processContentItem sender_ ci mc fInvDescr_
  where sender_ = case chatDir of CIGroupRcv m -> Just m; CIChannelRcv -> Nothing; _ -> Nothing
```

**Additional Internal.hs duplication** (minor):
- `quoteData` (lines 228-229): `CIGroupRcv m` returns `(qmc, CIQGroupRcv $ Just m, False, Just m)`, `CIChannelRcv` returns `(qmc, CIQGroupRcv Nothing, False, Nothing)`. Two one-liners differing only in `Just m` vs `Nothing`. Trivial but noted.

---

### View.hs

**D9: View.hs pattern match duplication**

The actual count of `CIChannelRcv` pattern match branches:
- **View.hs**: 6 branches (chatDirNtf, viewChatItem new, viewChatItem updated, reaction display, sentByMember', fileFrom)
- **Terminal/Output.hs**: 1 branch
- **Commands.hs**: 2 branches (itemDeletable, itemsMsgMemIds)
- **Internal.hs**: 2 branches (quoteData, processContentItem)
- **Subscriber.hs**: ~6 branches (scattered)
- **Store/Messages.hs**: ~4 branches (toGroupChatItem, createNewRcvChatItem, createNewChatItem_, getChatItemQuote_)

Total: **~24 pattern match sites** across all files (~17 `CIChannelRcv` + ~7 `CDChannelRcv`). Each mirrors the corresponding `CIGroupRcv m` / `CDGroupRcv` branch passing `Nothing` instead of `Just m`.

The `ttyFromGroup*` family of functions in View.hs was correctly generalized to take `Maybe GroupMember` — the duplication is at the call sites, not in the helper functions.

**Solution:** This duplication is **inherent to the separate constructor choice** and can only be eliminated by the architectural change (merging `CIChannelRcv` into `CIGroupRcv (Maybe GroupMember)`). Without that change, the branches must remain. Extracting local helpers at each call site would add complexity without reducing total code.

---

### Other Files (no significant deduplication needed)

- **Commands.hs:** Parameter threading (`ShowGroupAsSender`, `SRGroup`). Clean, no duplication.
- **Protocol.hs:** Wire protocol changes (`ExtMsgContent.asGroup`, `XGrpMsgForward Maybe MemberId`). Necessary.
- **Delivery.hs:** `FwdSender` type replaces separate fields. Could be `Maybe (MemberId, ContactName)` but not a priority.
- **Store/Files.hs:** `createRcvGroupFileTransfer` takes `Maybe GroupMember`. Clean parameterization.
- **Store/Groups.hs:** `createPreparedGroup` returns `Maybe GroupMember`. Necessary for relay groups.
- **Types.hs:** `sendAsGroup'`, `groupId'` utilities. Minor.

---

## Architectural Note: CIChannelRcv Constructor {#architectural-note}

The deepest source of duplication is the choice to add `CIChannelRcv` / `CDChannelRcv` as separate constructors rather than parameterizing `CIGroupRcv :: Maybe GroupMember -> CIDirection 'CTGroup 'MDRcv` and `CDGroupRcv :: GroupInfo -> Maybe GroupChatScopeInfo -> Maybe GroupMember -> ChatDirection 'CTGroup 'MDRcv`.

This creates ~24 pattern match branches across the codebase, almost all passing `Nothing` where `CIGroupRcv` passes `Just m`. The `chatItemMember` function already returns `Maybe GroupMember`, confirming the abstraction is correct.

**However**, changing these constructors is a large cross-cutting refactor affecting Messages.hs, View.hs, Commands.hs, Internal.hs, Subscriber.hs, Store/Messages.hs, and tests. It may be better suited as a follow-up PR.

**Decision needed from user:** Merge `CIChannelRcv` into `CIGroupRcv (Maybe GroupMember)` in this PR, or defer?

---

## Implementation Order

### Phase 1: Store layer (D6)
1. Parameterize `getGroupChatItemBySharedMsgId` with `Maybe GroupMemberId` + `IS NOT DISTINCT FROM`
2. Delete `getGroupCIBySharedMsgId'`
3. Update all callers (pass `Just gmId` or `Nothing`)

### Phase 2: Subscriber.hs function merges (D1, D2, D3)
4. Merge `channelMessageUpdate_` into `groupMessageUpdate` (takes `Maybe GroupMember`)
5. Extract shared `updateChatItemReaction` helper from `groupMsgReaction` and `fwdChannelReaction`
6. Merge `newChannelContentMessage_` into `newGroupContentMessage` (extract shared save-view helper or take `Maybe GroupMember`)

### Phase 3: Dispatch unification (D4)
7. Merge `processForwardedChannelMsg` into `processForwardedMsg` (takes `Maybe GroupMember`; handle `XMsgReact` three-way split)

### Phase 4: Internal cleanup (D7, D8)
8. Deduplicate `saveRcvChatItem'` CDChannelRcv branches (3 spots)
9. Merge `processContentItem` CIChannelRcv branch

### Phase 5 (deferred unless approved): Constructor change (D9)
10. Merge `CIChannelRcv` into `CIGroupRcv (Maybe GroupMember)` — eliminates ~24 pattern match branches across all files

### Phase 6 (optional): channelMessageDelete (D5)
11. Only if user wants it — extend `groupMessageDelete` with `Maybe GroupMember`
