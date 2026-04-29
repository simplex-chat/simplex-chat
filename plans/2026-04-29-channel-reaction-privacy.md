# Channel Reaction Privacy — Implementation Plan

## Problem

When a subscriber reacts to a message in a channel (relay group), the relay forwards the reaction via `XGrpMsgForward` with `FwdMember {memberId, memberName}`. This causes `getCreateUnknownGMByMemberId` to create an unknown member record on every other subscriber's device, leaking the reacting member's name.

**Goal**: Channel owners see full reaction details (who reacted). Subscribers see only reaction counts (emoji + count), with no member identity leaked.

## Root Cause

1. Subscriber sends `XMsgReact` to relay
2. Relay's `groupMsgReaction` returns `DeliveryTaskContext (DJSGroup {DJDeliveryJob}) False` — `sentAsGroup = False`
3. Task worker constructs `fwdSender = FwdMember senderMemberId senderMemberName` (Store/Delivery.hs:156)
4. Job worker sends same body to ALL members via `getGroupMembersByCursor` — no role filter
5. Receiving subscriber's `xGrpMsgForward` → `FwdMember` → `getCreateUnknownGMByMemberId` creates unknown member

## Design: Split Delivery for Reactions

The relay creates **two delivery tasks** for reactions in channels:
- **Subscriber task**: `sentAsGroup = True` → `FwdChannel` (no identity)
- **Owner task**: `sentAsGroup = False` → `FwdMember` (full identity), delivered only to owners

This reuses the existing `sentAsGroup`/`FwdChannel` mechanism. The new part is a `DJOwnerOnlyJob` delivery job spec that restricts delivery to owners.

### Why two tasks instead of one FwdChannel task?

Owners need to see who reacted — this is the stated requirement. Sending `FwdChannel` to everyone would strip identity from owners too.

### Why not a new event type for subscribers?

The existing `XMsgReact` + `FwdChannel` combination already carries everything needed: the reaction, the target message, add/remove flag. The subscriber doesn't need the reacting member's identity. No new protocol event is required — `FwdChannel` already means "anonymous sender."

### Batching compatibility

Tasks are batched by matching `jobScope` (Store/Delivery.hs:188-199). `DJOwnerOnlyJob` is a distinct `DeliveryJobSpec`, so owner tasks batch separately from subscriber tasks. No cross-contamination.

## Changes

### 1. New DeliveryJobSpec variant: `DJOwnerOnlyJob`

**File**: `src/Simplex/Chat/Delivery.hs`

Add `DJOwnerOnlyJob` to `DeliveryJobSpec`:
```haskell
data DeliveryJobSpec
  = DJDeliveryJob {includePending :: Bool}
  | DJOwnerOnlyJob
  | DJRelayRemoved
```

Add `DJSTOwnerOnlyJob` to `DeliveryJobSpecTag` with text encoding `"owner_only_job"`.

Update `jobScopeImpliedSpec`, `isRelayRemoved` — no changes needed (they match on specific constructors).

**File**: `src/Simplex/Chat/Store/Delivery.hs`

Update `jobScopeRow_` and `toJobScope_` for the new tag.

### 2. Owner-only member query

**File**: `src/Simplex/Chat/Store/Delivery.hs`

Add `getGroupOwnersByCursor` — same as `getGroupMembersByCursor` but with `AND member_role = 'owner'` filter (or `>= GROwner`). Could also be a parameter to `getGroupMembersByCursor` but a separate function is clearer given the single use.

### 3. Job worker: dispatch DJOwnerOnlyJob

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (job worker, ~line 3589)

In `processDeliveryJob`, the `DJSGroup` channel branch currently only handles `DJDeliveryJob`. Add a case for `DJOwnerOnlyJob` that uses `getGroupOwnersByCursor` instead of `getGroupMembersByCursor`.

### 4. Task worker: dispatch DJOwnerOnlyJob

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (task worker, ~line 3524)

`processDeliveryTask` dispatches on `jobScopeImpliedSpec`. Add `DJOwnerOnlyJob` case — same batching logic as `DJDeliveryJob`.

### 5. Relay-side: return two tasks for reactions in channels

**File**: `src/Simplex/Chat/Library/Subscriber.hs`

Change `processEvent` return type from `Maybe NewMessageDeliveryTask` to `[NewMessageDeliveryTask]`:

```haskell
processEvent :: forall e. MsgEncodingI e => GroupInfo -> GroupMember -> VerifiedMsg e -> CM [NewMessageDeliveryTask]
```

All existing `pure $ Just $ DeliveryTaskContext ...` become `pure [DeliveryTaskContext ...]`, `pure Nothing` becomes `pure []`.

In `processAChatMsg` (line 964), change accumulation:
```haskell
newTasks <- join <$> withVerifiedMsg ...
  (\verifiedMsg -> processEvent gInfo' m' verifiedMsg ...)
pure $ maybe id (++) newTasks newDeliveryTasks  -- was: maybe id (:)
```

Wait — actually, the simpler approach: keep `processEvent` returning `Maybe NewMessageDeliveryTask`, and handle the split specifically in `groupMsgReaction`.

**Better approach**: Change only `groupMsgReaction` to return `[DeliveryTaskContext]` (instead of `Maybe DeliveryTaskContext`), and adapt the single call site in `processEvent` (line 998).

In `processEvent` for `XMsgReact` (line 998):
```haskell
XMsgReact sharedMsgId memberId scope_ reaction add -> do
  taskContexts <- groupMsgReaction gInfo' m'' sharedMsgId memberId scope_ reaction add msg brokerTs
  pure taskContexts  -- [DeliveryTaskContext]
```

But `processEvent` returns `Maybe NewMessageDeliveryTask`. To return multiple, either:
- (a) Change return type to `[NewMessageDeliveryTask]` — touches every branch
- (b) Return the first task from processEvent, and create the second task directly — ugly

Option (a) is cleaner despite touching more lines, because the change is mechanical (`Just ctx` → `[ctx]`, `Nothing` → `[]`) and `createDeliveryTasks` already takes `[NewMessageDeliveryTask]`.

**Decision**: Change `processEvent` to return `[NewMessageDeliveryTask]`.

### 6. groupMsgReaction: return two tasks for channels

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (~line 1922)

Change `groupMsgReaction` return type from `Maybe DeliveryTaskContext` to `[DeliveryTaskContext]`.

For channel groups (`useRelays' g`), in the `Nothing` scope branch (line 1936-1938):
```haskell
Nothing -> do
  withStore' $ \db -> setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
  pure
    [ DeliveryTaskContext (DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}) True    -- subscribers: FwdChannel
    , DeliveryTaskContext (DJSGroup {jobSpec = DJOwnerOnlyJob}) False                           -- owners: FwdMember
    ]
```

For non-channel groups, return single-element list as before.

The `updateChatItemReaction` path (line 1942-1958) similarly returns two tasks for channels.

### 7. Subscriber receiving side: handle FwdChannel + XMsgReact

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (~line 3349)

Currently:
```haskell
XMsgReact sharedMsgId memId scope_ reaction add -> withAuthor XMsgReact_ $ \author -> void $ groupMsgReaction gInfo author ...
```

`withAuthor` fails when `author_ = Nothing` (FwdChannel case). Change to:
```haskell
XMsgReact sharedMsgId memId scope_ reaction add -> case author_ of
  Just author -> void $ groupMsgReaction gInfo author sharedMsgId memId scope_ reaction add rcvMsg msgTs
  Nothing -> void $ groupMsgReactionAnon gInfo sharedMsgId memId scope_ reaction add rcvMsg msgTs
```

### 8. Anonymous reaction processing

**File**: `src/Simplex/Chat/Library/Subscriber.hs`

New function `groupMsgReactionAnon` — simplified version of `groupMsgReaction` for channel subscribers receiving anonymous reactions:

```haskell
groupMsgReactionAnon :: GroupInfo -> SharedMsgId -> Maybe MemberId -> Maybe MsgScope -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> CM [DeliveryTaskContext]
```

- Skip `getGroupReactions` dedup check (no `group_member_id` to check against) — or use a NULL-member variant
- Call `setGroupReactionAnon` (new) to store with `group_member_id = NULL`
- Update `CIReactionCount` via `getGroupCIReactions` (already aggregates, NULL member_id works)
- Return `[]` (no further forwarding — this is a subscriber, not a relay)

Actually, this function doesn't need to return delivery tasks at all — it's on the receiving subscriber side, not the relay. It should return `CM ()` or similar — it just processes and stores the reaction locally.

### 9. Anonymous reaction storage

**File**: `src/Simplex/Chat/Store/Messages.hs`

Add `setGroupReactionAnon`:
```haskell
setGroupReactionAnon :: DB.Connection -> GroupInfo -> Maybe MemberId -> SharedMsgId -> MsgReaction -> Bool -> MessageId -> UTCTime -> IO ()
```

- INSERT: `group_member_id = NULL`
- DELETE: use `LIMIT 1` subquery since multiple anonymous reactions of same type can exist:
  ```sql
  DELETE FROM chat_item_reactions
  WHERE chat_item_reaction_id = (
    SELECT chat_item_reaction_id FROM chat_item_reactions
    WHERE group_id = ? AND group_member_id IS NULL AND shared_msg_id = ? AND item_member_id IS NOT DISTINCT FROM ? AND reaction_sent = 0 AND reaction = ?
    LIMIT 1
  )
  ```

Also add `getGroupReactionsAnon` for dedup — count of anonymous reactions of this type for this message, to enforce `maxMsgReactions`.

### 10. iOS UI: hide reaction member list for channel subscribers

**File**: `apps/ios/Shared/Views/Chat/ChatView.swift` (~line 2248)

Change the `.group` context menu case to suppress `ReactionContextMenu` for non-owner channel subscribers:

```swift
case let .group(groupInfo, _):
    if groupInfo.useRelays && !groupInfo.isOwner {
        v  // no context menu — just the pill
    } else {
        v.contextMenu {
            ReactionContextMenu(...)
        }
    }
```

### 11. Backend API: restrict reaction members for channel subscribers

**File**: `src/Simplex/Chat/Library/Commands.hs` (~line 884)

In `APIGetReactionMembers`, check if the group is a channel and the user is not an owner. If so, return empty list:

```haskell
APIGetReactionMembers userId groupId itemId reaction -> withUserId userId $ \user -> do
  gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
  memberReactions <- if useRelays' gInfo && not (isOwner gInfo)
    then pure []
    else withStore $ \db -> do
      CChatItem _ ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}} <- getGroupChatItem db user groupId itemId
      liftIO $ getReactionMembers db vr user groupId itemSharedMId reaction
  pure $ CRReactionMembers user memberReactions
```

where `isOwner` checks `membership.memberRole >= GROwner && memberCurrent membership`.

## Alternatives Considered

### A. New protocol event (e.g., XMsgReactCount)

Instead of forwarding `XMsgReact` with `FwdChannel`, send a new event like `XMsgReactCount sharedMsgId [(reaction, count)]` that carries aggregated counts.

**Rejected because**:
- Requires new protocol event definition, encoding/decoding, versioning
- The relay would need to aggregate reactions before forwarding — stateful, complex
- Individual add/remove of reactions is lost — subscriber can't toggle their own reaction
- `XMsgReact` + `FwdChannel` already carries exactly the right information (reaction, target, add/remove) without identity

### B. Single FwdChannel task for everyone (owners lose identity)

Send `FwdChannel` to all members including owners. Owners wouldn't see who reacted.

**Rejected because**: Violates the requirement that owners see full reaction details.

### C. Filter in getGroupMembersByCursor instead of new job spec

Add a role filter parameter to the existing cursor query instead of creating `DJOwnerOnlyJob`.

**Rejected because**: The filter is a property of the job, not a parameter of the query function. A dedicated job spec makes the intent explicit and doesn't complicate the general-purpose cursor query.

### D. Don't forward reactions to subscribers at all

Subscribers wouldn't see any reactions.

**Rejected because**: Reaction counts are a core UX feature. The RFC mentions "send reaction counts" as the minimum viable approach.

## Adversarial Review

### Pass 1

1. **Race condition on reaction toggle**: Subscriber sends reaction, receives anonymous forwarded copy of own reaction → double-counted? No — the subscriber's own reaction is stored with `reaction_sent = True`, the forwarded one with `reaction_sent = False`. `getGroupCIReactions` aggregates by reaction type across both. The `userReacted` field in `CIReactionCount` is `MAX(reaction_sent)` — if user reacted, it's True. The count includes both sent and received. **Issue**: subscriber's own reaction would be counted twice (once as sent, once as anonymous received). **Fix**: The relay should exclude the sender from the subscriber delivery, same as it already does via `singleSenderGMId_` in the cursor query (Store/Delivery.hs:369 — `AND group_member_id IS DISTINCT FROM ?`). The subscriber task's `singleSenderGMId_` is the reacting member's GMId, so the reacting subscriber won't receive their own anonymous reaction back. Confirmed: existing mechanism handles this.

2. **Owner receiving both tasks**: Owner is a member. If both tasks deliver to them, they'd get the reaction twice — once via `DJOwnerOnlyJob` (FwdMember) and once via `DJDeliveryJob` (FwdChannel). **Fix**: The subscriber task (`DJDeliveryJob`) cursor query must EXCLUDE owners. Add `AND member_role < 'owner'` (or `< GROwner`) to the subscriber task's query. But `getGroupMembersByCursor` is a general-purpose function used for all forwarding, not just reactions. **Better fix**: Don't change the general query. Instead, for reaction subscriber tasks specifically, use a new query `getGroupSubscribersByCursor` that excludes owners. This means `DJDeliveryJob` for reactions needs different member selection than `DJDeliveryJob` for messages. **Problem**: The job spec doesn't distinguish reaction vs message delivery jobs — both are `DJDeliveryJob`. **Solution**: Use a different job spec for the subscriber reaction task too. Add `DJSubscriberOnlyJob` variant, or parameterize `DJDeliveryJob` with an optional role filter.

   Actually, simplest: the subscriber reaction task uses `DJDeliveryJob` as before, but in the channel case of `sendBodyToMembers`, when the job body contains only FwdChannel reactions, the query excludes owners. But the job worker doesn't know the content semantics.

   **Revised approach**: Instead of `DJOwnerOnlyJob`, use `DJOwnerOnlyJob` for owners AND change the subscriber task to also be a new spec `DJSubscriberReactionJob` that excludes owners. This is getting complex.

   **Simpler revised approach**: Single new job spec `DJOwnerOnlyJob`. For the subscriber task, keep `DJDeliveryJob` as-is (delivers to everyone). Owners receive BOTH the anonymous reaction (FwdChannel) and the identified reaction (FwdMember). On the receiving side, the `FwdMember` reaction creates the member record and stores with `group_member_id`. The `FwdChannel` anonymous reaction would try to add a second reaction for the same emoji on the same message — which `reactionAllowed` would reject (reaction already present). So the duplicate is harmlessly dropped.

   **This is the simplest correct approach**: owners get both, the FwdMember one wins (processed first or dedup catches it), the FwdChannel one is a no-op.

   Wait — ordering isn't guaranteed. If FwdChannel arrives first, it stores anonymously. Then FwdMember arrives — `reactionAllowed` checks if reaction already exists for this member, finds none (anonymous has NULL member_id), adds it. Now there are two: one anonymous, one identified. Count is inflated.

   **Fix**: On the owner side, don't process anonymous reactions at all. In `xGrpMsgForward`, when `FwdChannel` + `XMsgReact` and the user is an owner, skip processing:

   ```haskell
   Nothing -> if isOwner gInfo  -- FwdChannel case
     then pure ()  -- owner will receive identified reaction via DJOwnerOnlyJob
     else groupMsgReactionAnon ...
   ```

   This is clean. Owners ignore FwdChannel reactions because they'll get the FwdMember version.

3. **processEvent return type change**: Mechanical but touches ~20 branches. Each `Just ctx` → `[ctx]`, `Nothing` → `[]`. Risk of missing one. Mitigation: compiler will catch type mismatches.

4. **Member support scope reactions**: `groupMsgReaction` has a `Just (MSMember scopeMemberId)` branch (line 1929-1935) that returns `DJSMemberSupport`. This is for support scope, not channel main scope. The split delivery should only apply to the `Nothing` scope (main channel scope) and the `updateChatItemReaction` path. The member support path is unaffected.

### Pass 2

5. **getGroupReactionsAnon dedup**: For anonymous reactions, we can't check per-member dedup. But `reactionAllowed` in `groupMsgReaction` uses `getGroupReactions` which filters by `group_member_id`. For anonymous processing, we need a variant that counts total anonymous reactions for this message+emoji, and checks `maxMsgReactions` against all anonymous reactions. This prevents a flood of anonymous reactions from inflating counts.

   Actually, `reactionAllowed` checks: `(reaction `elem` rs) /= add && not (add && length rs >= maxMsgReactions)`. For anonymous reactions, `rs` should be all anonymous reactions for this message. An add is allowed only if this reaction isn't already present anonymously beyond the max. But since each forwarded anonymous reaction is from a unique original sender, and the relay already does dedup per-sender, the subscriber can trust the relay's dedup. Still, a limit check is prudent.

6. **Schema**: `chat_item_reactions.group_member_id` — is it `NOT NULL`? Need to check. If NOT NULL, need a migration to make it nullable.

7. **Existing tests**: Need to verify no tests break from `processEvent` return type change. The function is internal to `processAChatMsg`, not directly tested. But `groupMsgReaction` may be referenced in tests.

### Pass 2 — continued, no new issues found.

## Summary of Files Changed

| File | Change |
|------|--------|
| `src/Simplex/Chat/Delivery.hs` | Add `DJOwnerOnlyJob` to `DeliveryJobSpec`, `DJSTOwnerOnlyJob` to tag |
| `src/Simplex/Chat/Store/Delivery.hs` | `jobScopeRow_`/`toJobScope_` for new tag; `getGroupOwnersByCursor` query |
| `src/Simplex/Chat/Library/Subscriber.hs` | `processEvent` returns `[NewMessageDeliveryTask]`; `groupMsgReaction` returns `[DeliveryTaskContext]` with split for channels; `xGrpMsgForward` handles FwdChannel+XMsgReact; `groupMsgReactionAnon` new function; task/job workers handle `DJOwnerOnlyJob` |
| `src/Simplex/Chat/Store/Messages.hs` | `setGroupReactionAnon`, `getGroupReactionsAnon` |
| `src/Simplex/Chat/Library/Commands.hs` | `APIGetReactionMembers` returns empty for non-owner channel members |
| `apps/ios/Shared/Views/Chat/ChatView.swift` | Suppress `ReactionContextMenu` for channel subscribers |
| Migration file | Make `chat_item_reactions.group_member_id` nullable (if not already) |
