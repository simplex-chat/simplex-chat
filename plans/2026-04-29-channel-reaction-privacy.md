# Channel Reaction Privacy — Implementation Plan

## Problem

When a subscriber reacts to a message in a channel (relay group), the relay forwards the reaction via `XGrpMsgForward` with `FwdMember {memberId, memberName}`. This causes `getCreateUnknownGMByMemberId` to create unknown member records on other subscribers' devices, leaking the reacting member's name.

**Goal**: Channel owners see full reaction details (who reacted). Subscribers see only reaction counts, with no member identity leaked.

## Root cause

1. Subscriber sends `XMsgReact` to relay
2. Relay's `groupMsgReaction` returns `DeliveryTaskContext (DJSGroup {DJDeliveryJob}) False` — `sentAsGroup = False`
3. Task worker constructs `fwdSender = FwdMember senderMemberId senderMemberName` (Store/Delivery.hs:156)
4. Job worker sends same body to ALL members via `getGroupMembersByCursor` — no role filter
5. Receiving subscriber's `xGrpMsgForward` → `FwdMember` → `getCreateUnknownGMByMemberId` creates unknown member

## Design

Add a new `DJReaction` delivery job spec. When the relay processes a reaction in a channel, `groupMsgReaction` returns `DJReaction` instead of `DJDeliveryJob`. A single delivery task is created; the split happens at the job level.

**Task worker** (`DJReaction`): Encodes two bodies from the same batch of tasks:
- Owner body: uses each task's `fwdSender` (`FwdMember`) — preserves member identity
- Subscriber body: uses `FwdChannel` — no member identity

Creates a single delivery job carrying both bodies.

**Job worker** (`DJReaction` in channels): Iterates members via cursor. For each bucket, partitions members into owners (`memberRole >= GROwner`) and non-owners, sends the owner body to owners and the subscriber body to non-owners.

**Receiving subscriber**: When `XMsgReact` arrives via `FwdChannel` (no author), `groupMsgReaction` receives `Nothing` for the member parameter and takes the channel reaction path — stores with `group_member_id = NULL`, emits a `CIChannelRcv` UI event. This follows the established pattern where all forwarded message handlers (`newGroupContentMessage`, `groupMessageUpdate`, `groupMessageDelete`, `groupMessageFileDescription`) accept `Maybe GroupMember` and branch on `Nothing`.

The existing `sentAsGroup`/`message_from_channel` mechanism is not involved — `DJReaction` handles the split directly in the task/job workers.

## Alternatives considered

### A. Two delivery tasks with separate job specs

Instead of one job with two bodies, create two delivery tasks from `groupMsgReaction`: one subscriber task (`DJDeliveryJob` with `sentAsGroup = True` → `FwdChannel`) and one owner task (new `DJOwnerOnlyJob` → `FwdMember`). Each becomes a separate job with its own cursor-based delivery.

**Rejected because**: Requires changing `processEvent` return type from `Maybe NewMessageDeliveryTask` to `[NewMessageDeliveryTask]` (mechanical but touches ~20 branches). Two cursor passes through the same member table (redundant). Two job records per reaction. Mixes reaction privacy into the `sentAsGroup`/`message_from_channel` abstraction, which was designed specifically for "owner sends as channel." Overall higher complexity for the same result.

### B. Two jobs from a single task

Keep a single `DJReaction` task, but the task worker creates two jobs — one for owners, one for subscribers. Each job uses a different cursor query (owners-only vs non-owners-only).

**Rejected because**: Approximately the same complexity as option A at the job level. Requires either two new `DeliveryJobSpec` variants (`DJReactionOwner`, `DJReactionSubscriber`) or an ad-hoc flag column to distinguish them. Two cursor passes. The one-job-two-bodies approach (chosen design) achieves the same with a single cursor pass and in-memory partition.

### C. Job worker re-encodes body for subscribers

Single task, single job with one body (FwdMember). The job worker parses the binary batch, strips member identity from `GrpMsgForward` elements, and re-encodes for subscriber recipients.

**Rejected because**: Fragile — requires parsing opaque binary batch at the job worker level, which currently treats the body as opaque bytes. Encoding/decoding at delivery time introduces failure modes. Pre-computing both bodies at task worker time is more robust.

### D. New aggregated reaction count event (`XMsgReactCount`)

Instead of forwarding `XMsgReact` to subscribers, the relay aggregates current reaction counts and sends a new `XMsgReactCount {sharedMsgId, itemMemberId, [(reaction, count)]}` event.

**Rejected because**: Does not eliminate the delivery split — owners still need full `XMsgReact` with `FwdMember`, subscribers need the new event. So the core problem (different content to different recipients) remains. Additionally requires: new protocol event definition with encoding/decoding/versioning, relay-side aggregation queries before forwarding, new message records for relay-generated events, and the delivery task system currently ties tasks to received messages — a relay-generated event would need rework (the RFC envisions this as a future architectural change). Strictly more complex than the chosen approach.

### E. Reuse `sentAsGroup` / `FwdChannel` via existing mechanism

Set `sentAsGroup = True` on the reaction delivery task so it naturally produces `FwdChannel` on the subscriber body. Owners would receive a separate task with `sentAsGroup = False`.

**Rejected because**: `sentAsGroup`/`message_from_channel` was designed specifically for "owner sends as channel" — it drives `CIChannelRcv` direction, chat binding validation, and other channel-message-specific logic. Repurposing it for reaction privacy conflates two distinct concepts. The chosen design (`DJReaction`) handles the split in the task/job workers without involving the `sentAsGroup` flag.

## Changes

### 1. New DeliveryJobSpec variant

**File**: `src/Simplex/Chat/Delivery.hs`

Add `DJReaction` to `DeliveryJobSpec`:

```haskell
data DeliveryJobSpec
  = DJDeliveryJob {includePending :: Bool}
  | DJReaction
  | DJRelayRemoved
```

Add `DJSTReaction` to `DeliveryJobSpecTag` with text encoding `"reaction"`.

Add `DJReaction` case to `jobSpecImpliedPending`:

```haskell
DJReaction -> False
```

No changes needed to `jobScopeImpliedSpec` (it passes through `jobSpec` from `DJSGroup`), `isRelayRemoved` (matches `DJRelayRemoved` specifically), or `toWorkerScope` (dispatches on `DJSGroup` constructor).

Add `subscriberBody_` field to `MessageDeliveryJob`:

```haskell
data MessageDeliveryJob = MessageDeliveryJob
  { jobId :: Int64,
    jobScope :: DeliveryJobScope,
    singleSenderGMId_ :: Maybe GroupMemberId,
    body :: ByteString,
    subscriberBody_ :: Maybe ByteString,
    cursorGMId_ :: Maybe GroupMemberId
  }
```

### 2. Delivery store changes

**File**: `src/Simplex/Chat/Store/Delivery.hs`

Update `jobScopeRow_` and `toJobScope_` for new tag:

```haskell
-- in jobScopeRow_:
DJReaction -> (DWSGroup, Just DJSTReaction, Nothing, Nothing)

-- in toJobScope_:
(DWSGroup, Just DJSTReaction, Nothing, Nothing) -> Just $ DJSGroup {jobSpec = DJReaction}
```

Extend `createMsgDeliveryJob` with optional subscriber body parameter:

```haskell
createMsgDeliveryJob :: DB.Connection -> GroupInfo -> DeliveryJobScope -> Maybe GroupMemberId -> ByteString -> Maybe ByteString -> IO ()
```

Add `subscriber_body` to the INSERT. Existing call sites (2 in Subscriber.hs task worker, 1 in Commands.hs relay removal) pass `Nothing`.

Update `MessageDeliveryJobRow`, `getNextDeliveryJob` query, and `toDeliveryJob` to read `subscriber_body` column into the new `subscriberBody_` field.

### 3. DB migration

```sql
ALTER TABLE delivery_jobs ADD COLUMN subscriber_body BLOB;
```

(`chat_item_reactions.group_member_id` is already nullable — verified from schema.)

### 4. `groupMsgReaction`: accept `Maybe GroupMember`, add channel reaction path

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (~line 1922)

Change signature from `GroupMember` to `Maybe GroupMember`, consistent with sibling forwarded message handlers:

```haskell
groupMsgReaction :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> Maybe MemberId -> Maybe MsgScope -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> CM (Maybe DeliveryTaskContext)
groupMsgReaction g m_ sharedMsgId itemMemberId scope_ reaction add RcvMessage {msgId} brokerTs
  | groupFeatureAllowed SGFReactions g = case m_ of
      Nothing -> -- channel reaction without member identity (subscriber receiving FwdChannel)
        ...
      Just m -> -- existing logic (relay processing or forwarded with FwdMember)
        ...
  | otherwise = pure Nothing
```

**`Nothing` branch** (new — channel reaction on receiving subscriber):

```haskell
Nothing ->
  updateChannelReaction `catchCINotFound` \_ ->
    withStore' (\db -> setGroupReactionNoMember db g itemMemberId sharedMsgId reaction add msgId brokerTs)
      $> Nothing
```

Where `updateChannelReaction`:

```haskell
updateChannelReaction = do
  (CChatItem md ci, scopeInfo) <- withStore $ \db -> do
    cci <- case itemMemberId of
      Just itemMemberId' -> getGroupMemberCIBySharedMsgId db user g itemMemberId' sharedMsgId
      Nothing -> getGroupChatItemBySharedMsgId db user g Nothing sharedMsgId
    scopeInfo <- getGroupChatScopeInfoForItem db vr user g (cChatItemId cci)
    pure (cci, scopeInfo)
  when (ciReactionAllowed ci) $ do
    reactions <- withStore' $ \db -> do
      setGroupReactionNoMember db g itemMemberId sharedMsgId reaction add msgId brokerTs
      getGroupCIReactions db g itemMemberId sharedMsgId
    let ci' = CChatItem md ci {reactions}
        r = ACIReaction SCTGroup SMDRcv (GroupChat g scopeInfo) $ CIReaction CIChannelRcv ci' brokerTs reaction
    toView $ CEvtChatItemReaction user add r
  pure Nothing
```

Key differences from the `Just m` branch:
- No per-member dedup check — trusts relay's existing `reactionAllowed` validation
- Stores with `group_member_id = NULL` via `setGroupReactionNoMember`
- Uses `CIChannelRcv` direction in the UI event (no member attribution)
- Returns `Nothing` — no delivery context (subscriber doesn't forward)
- No member support scope handling (irrelevant for channel subscriber reactions)

**`Just m` branch** (existing logic, two changes for `DJReaction`):

The `catchCINotFound` fallback, `Nothing` scope sub-branch (line 1936–1938):

```haskell
Nothing -> do
  withStore' $ \db -> setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
  let jobSpec = if useRelays' g then DJReaction else DJDeliveryJob {includePending = False}
  pure $ Just $ DeliveryTaskContext (DJSGroup {jobSpec}) False
```

The `updateChatItemReaction` return path (line 1949–1957):

```haskell
pure $ Just $ case scopeInfo of
  Nothing | useRelays' g -> DeliveryTaskContext (DJSGroup {jobSpec = DJReaction}) False
  _ -> infoToDeliveryContext g scopeInfo False
```

The member support scope branch (line 1929–1934) is unaffected — it returns `DJSMemberSupport`, a different delivery scope.

**Call site updates**:

- Line 998 (direct call in `processGroupMessage`): `groupMsgReaction gInfo' m''` → `groupMsgReaction gInfo' (Just m'')`
- Line 3349 (forwarded call in `processForwardedMsg`): see section 7

### 5. Task worker: `DJReaction` handling

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (~line 3524)

In `processDeliveryTask`, add `DJReaction` case alongside existing `DJDeliveryJob`:

```haskell
DJReaction ->
  withWorkItems a doWork (withStore' $ \db -> getNextDeliveryTasks db gInfo task) $ \nextTasks -> do
    let (body, taskIds, largeTaskIds) = batchDeliveryTasks1 vr maxEncodedMsgLength nextTasks
        subscriberBody = encodeBinaryBatch
          [ encodeFwdElement (GrpMsgForward FwdChannel fwdBrokerTs) verifiedMsg
          | MessageDeliveryTask {taskId = tId, brokerTs = fwdBrokerTs, verifiedMsg} <- L.toList nextTasks
          , tId `elem` taskIds
          ]
    withStore' $ \db -> do
      createMsgDeliveryJob db gInfo jobScope (singleSenderGMId_ nextTasks) body (Just subscriberBody)
      forM_ taskIds $ \taskId -> updateDeliveryTaskStatus db taskId DTSProcessed
      forM_ largeTaskIds $ \taskId -> setDeliveryTaskErrStatus db taskId "large"
    lift . void $ getDeliveryJobWorker True deliveryKey
```

The owner body (`body`) uses each task's `fwdSender` (which is `FwdMember` because `sentAsGroup = False`). The subscriber body re-encodes the same included tasks with `FwdChannel`.

### 6. Job worker: `DJReaction` handling

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (~line 3589)

In `processDeliveryJob`, add `DJReaction` case:

```haskell
DJReaction -> do
  sendBodyToMembersReaction
  withStore' $ \db -> updateDeliveryJobStatus db jobId DJSComplete
```

Where `sendBodyToMembersReaction` for channels:

```haskell
sendBodyToMembersReaction
  | useRelays' gInfo = do
      bucketSize <- asks $ deliveryBucketSize . config
      sendLoopReaction bucketSize startingCursor
  | otherwise = -- fallback for non-channel: deliver body to all (shouldn't happen)
      ...
  where
    sendLoopReaction bucketSize cursorGMId_ = do
      mems <- withStore' $ \db -> getGroupMembersByCursor db vr user gInfo cursorGMId_ singleSenderGMId_ bucketSize
      unless (null mems) $ do
        let (owners, nonOwners) = partition (\m -> memberRole' m >= GROwner) mems
        unless (null owners) $ deliver body owners
        let subBody = fromMaybe body subscriberBody_
        unless (null nonOwners) $ deliver subBody nonOwners
        let cursorGMId' = groupMemberId' $ last mems
        withStore' $ \db -> updateDeliveryJobCursor db jobId cursorGMId'
        unless (length mems < bucketSize) $ sendLoopReaction bucketSize (Just cursorGMId')
```

Single cursor pass. Members partitioned in memory. Each group receives the appropriate body.

### 7. Receiving side: pass `author_` directly to `groupMsgReaction`

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (~line 3349)

In `processForwardedMsg`, replace `withAuthor` wrapper with direct call, matching sibling handlers:

```haskell
XMsgReact sharedMsgId memId scope_ reaction add -> void $ groupMsgReaction gInfo author_ sharedMsgId memId scope_ reaction add rcvMsg msgTs
```

When `author_` is `Nothing` (FwdChannel), `groupMsgReaction` takes the channel reaction path (section 4). When `Just author` (FwdMember), it takes the existing path. The `void` discards the delivery context — the receiving subscriber doesn't forward.

### 8. New storage function: `setGroupReactionNoMember`

**File**: `src/Simplex/Chat/Store/Messages.hs`

```haskell
setGroupReactionNoMember :: DB.Connection -> GroupInfo -> Maybe MemberId -> SharedMsgId -> MsgReaction -> Bool -> MessageId -> UTCTime -> IO ()
setGroupReactionNoMember db GroupInfo {groupId} itemMemberId itemSharedMId reaction add msgId reactionTs
  | add =
      DB.execute db
        [sql|
          INSERT INTO chat_item_reactions
            (group_id, group_member_id, item_member_id, shared_msg_id, reaction_sent, reaction, created_by_msg_id, reaction_ts)
            VALUES (?,NULL,?,?,0,?,?,?)
        |]
        (groupId, itemMemberId, itemSharedMId, reaction, msgId, reactionTs)
  | otherwise =
      DB.execute db
        [sql|
          DELETE FROM chat_item_reactions
          WHERE chat_item_reaction_id = (
            SELECT chat_item_reaction_id FROM chat_item_reactions
            WHERE group_id = ? AND group_member_id IS NULL AND shared_msg_id = ?
              AND item_member_id IS NOT DISTINCT FROM ? AND reaction_sent = 0 AND reaction = ?
            LIMIT 1
          )
        |]
        (groupId, itemSharedMId, itemMemberId, reaction)
```

INSERT always uses `NULL` for `group_member_id` and `0` for `reaction_sent`.

DELETE uses `LIMIT 1` subquery — all NULL-member reactions of the same type are equivalent, so removing any one decrements the count correctly.

### 9. API restriction

**File**: `src/Simplex/Chat/Library/Commands.hs` (~line 884)

Restrict `APIGetReactionMembers` for non-owner channel subscribers:

```haskell
APIGetReactionMembers userId groupId itemId reaction -> withUserId userId $ \user -> do
  gInfo <- withStore $ \db -> getGroupInfo db vr user groupId
  memberReactions <-
    if useRelays' gInfo && memberRole' (membership gInfo) < GROwner
      then pure []
      else withStore $ \db -> do
        CChatItem _ ChatItem {meta = CIMeta {itemSharedMsgId = Just itemSharedMId}} <- getGroupChatItem db user groupId itemId
        liftIO $ getReactionMembers db vr user groupId itemSharedMId reaction
  pure $ CRReactionMembers user memberReactions
```

### 10. iOS UI: suppress reaction context menu

**File**: `apps/ios/Shared/Views/Chat/ChatView.swift` (~line 2248)

In `chatItemReactions`, change the `.group` case:

```swift
case let .group(groupInfo, _):
    if groupInfo.useRelays && !groupInfo.isOwner {
        v
    } else {
        v.contextMenu {
            ReactionContextMenu(
                groupInfo: groupInfo,
                itemId: ci.id,
                reactionCount: r,
                selectedMember: $selectedMember,
                profileRadius: profileRadius
            )
        }
    }
```

Reaction pills (emoji + count) remain visible. Only the long-press context menu showing member names is suppressed.

## Files changed summary

| File | Change |
|------|--------|
| `src/Simplex/Chat/Delivery.hs` | Add `DJReaction` to `DeliveryJobSpec`/tag; add `subscriberBody_` to `MessageDeliveryJob` |
| `src/Simplex/Chat/Store/Delivery.hs` | `jobScopeRow_`/`toJobScope_` for new tag; `createMsgDeliveryJob` new parameter; `getNextDeliveryJob` reads new column |
| `src/Simplex/Chat/Library/Subscriber.hs` | `groupMsgReaction` accepts `Maybe GroupMember`, adds channel reaction path, returns `DJReaction` for channels; task/job workers handle `DJReaction`; `processForwardedMsg` passes `author_` directly |
| `src/Simplex/Chat/Store/Messages.hs` | New `setGroupReactionNoMember` |
| `src/Simplex/Chat/Library/Commands.hs` | `APIGetReactionMembers` restricted for non-owner channel members |
| `apps/ios/Shared/Views/Chat/ChatView.swift` | Suppress `ReactionContextMenu` for channel subscribers |
| DB migration | `ALTER TABLE delivery_jobs ADD COLUMN subscriber_body BLOB` |

## Adversarial review

### Pass 1

1. **Owner duplicate delivery**: Owner is a member. In the cursor loop, they're partitioned into the `owners` group and receive `body` (FwdMember). They do NOT receive `subscriberBody`. No duplicate reactions.

2. **Subscriber's own reaction**: The reacting subscriber is excluded from delivery via `singleSenderGMId_` in `getGroupMembersByCursor` (`AND group_member_id IS DISTINCT FROM ?`). No self-delivery, no double-counting.

3. **Owner's reaction identity hidden from subscribers**: When an owner reacts, the relay processes it and returns `DJReaction`. The subscriber body uses `FwdChannel`, hiding the owner's identity from subscribers. Owners see each other's identities via `body`. Correct.

4. **Member support scope reactions**: The member support scope branch in `groupMsgReaction` (line 1929–1934) returns `DJSMemberSupport`, an entirely different delivery scope processed by a different job worker path. Unaffected.

5. **Non-channel groups**: `groupMsgReaction` returns `DJReaction` only when `useRelays' g`. Regular groups continue using `DJDeliveryJob`. Unaffected.

6. **Batching**: `getNextDeliveryTasks` filters by `jobScopeRow_`. `DJReaction` tasks have tag `"reaction"` — they batch with each other, NOT with `DJDeliveryJob` tasks. Multiple rapid reactions are correctly batched together, and both owner and subscriber bodies contain all batched elements.

7. **`getGroupCIReactions` counts NULL-member reactions**: The aggregation query groups by reaction and counts `chat_item_reaction_id` with no filter on `group_member_id`. NULL-member rows are counted. `userReacted = MAX(reaction_sent)` — for received anonymous reactions `reaction_sent = 0`, for the user's own reaction `reaction_sent = 1`. Correct.

8. **`getReactionMembers` on owner's device**: On the owner's device, reactions arrive via `FwdMember` and are stored with real `group_member_id` values. `getReactionMembers` fetches by `group_member_id` and joins `group_members` — works as before. On the subscriber's device, `APIGetReactionMembers` returns empty (API restriction), so NULL-member reactions are never queried for member details.

9. **Reaction removal (add=False)**: Relay processes removal, returns `DJReaction`. Task/job workers encode both bodies. Owner receives `FwdMember` removal — `setGroupReaction` deletes the specific member's reaction row. Subscriber receives `FwdChannel` removal — `groupMsgReaction` `Nothing` branch calls `setGroupReactionNoMember`, which deletes one NULL-member row via `LIMIT 1`. Correct.

10. **`saveGroupFwdRcvMsg` with `Nothing` author**: Already works — `refAuthorId = groupMemberId' <$> refAuthorMember_` handles `Nothing` (Internal.hs:2283). Dedup via `sharedMsgId_` is unaffected.

11. **`createMsgDeliveryJob` call sites**: Three existing call sites (Subscriber.hs:3531, 3548; Commands.hs:2927) need `Nothing` as the new subscriber body parameter. Mechanical change.

No issues found.

### Pass 2

12. **Subscriber body encoding size**: `FwdChannel` encodes as `"C"` (1 byte) vs `FwdMember` which is `"M" + memberId + memberName` (variable, larger). If the owner body fits within `maxEncodedMsgLength`, the subscriber body will also fit (same content, smaller wrapper). No overflow risk.

13. **`catchCINotFound` in `groupMsgReaction` `Nothing` branch**: When the chat item doesn't exist yet, the fallback stores the reaction with NULL member via `setGroupReactionNoMember`. When the chat item eventually arrives, `getGroupCIReactions` picks up all stored reactions including NULL-member ones. Counts are correct.

14. **`CIChannelRcv` direction in reaction UI event**: `CIReaction` uses `chatDir :: CIDirection c d`. `CIChannelRcv` is a valid `CIDirection 'CTGroup 'MDRcv`. The iOS event handler (`.chatItemReaction`) just calls `m.updateChatItem(r.chatInfo, r.chatReaction.chatItem)` — it updates the chat item's reaction counts regardless of direction. No UI issue.

15. **Old client compatibility**: Old clients that receive `FwdChannel` + `XMsgReact` parse `FwdChannel` correctly (it's an existing encoding). They still use `withAuthor XMsgReact_` → error logged, reaction silently dropped. Acceptable degradation — no crash, reaction count is marginally lower until client update.

16. **`subscriberBody_` for non-`DJReaction` jobs**: The field is `Maybe ByteString`, defaults to `Nothing` for all other job types. The `fromMaybe body subscriberBody_` fallback in the job worker ensures non-owners get `body` if no subscriber body exists. Safe.

No issues found. Two consecutive clean passes.
