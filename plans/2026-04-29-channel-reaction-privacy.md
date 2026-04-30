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

In `getNextDeliveryTasks`, add sender filtering for DJReaction tasks on relay groups (via `isSenderFiltered` helper). This ensures `singleSenderGMId_` is always Just for DJReaction jobs, preventing subscribers from receiving their own reaction back via FwdChannel in multi-sender batches.

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
| `src/Simplex/Chat/Store/Delivery.hs` | `jobScopeRow_`/`toJobScope_` for new tag; `createMsgDeliveryJob` new parameter; `getNextDeliveryJob` reads new column; `getNextDeliveryTasks` sender-filters DJReaction |
| `src/Simplex/Chat/Library/Subscriber.hs` | `groupMsgReaction` accepts `Maybe GroupMember`, adds channel reaction path, returns `DJReaction` for channels; task/job workers handle `DJReaction`; `processForwardedMsg` passes `author_` directly |
| `src/Simplex/Chat/Store/Messages.hs` | New `setGroupReactionNoMember` |
| `src/Simplex/Chat/Library/Commands.hs` | `APIGetReactionMembers` restricted for non-owner channel members |
| `apps/ios/Shared/Views/Chat/ChatView.swift` | Suppress `ReactionContextMenu` for channel subscribers |
| DB migration | `ALTER TABLE delivery_jobs ADD COLUMN subscriber_body BLOB` |

## Adversarial review

### Pass 1

1. **Owner duplicate delivery**: Owner partitioned into `owners` group in `sendBodyToMembersReaction` via `memberRole' m >= GROwner`. Receives `body` (FwdMember) only. No subscriber body sent to owners.

2. **Subscriber's own reaction excluded**: `isSenderFiltered` in `getNextDeliveryTasks` ensures DJReaction tasks are always sender-filtered (even for relay groups). All tasks in a DJReaction batch are from the same sender, so `singleSenderGMId_` is always Just. `getGroupMembersByCursor` excludes the sender via `AND group_member_id IS DISTINCT FROM ?`.

3. **Multi-sender batch dedup** (found and fixed during review): Without sender filtering, if multiple senders' DJReaction tasks were batched, `singleSenderGMId_` would be Nothing and senders would receive their own reactions back as FwdChannel, causing duplicate NULL-member reaction rows. Fixed by adding `isSenderFiltered` to always use sender-filtered query for DJReaction. Tasks from other senders remain DTSNew and are processed in subsequent iterations.

4. **Owner's reaction identity hidden from subscribers**: Owner reacts → relay returns DJReaction → subscriber body uses FwdChannel → subscribers see only emoji + count, no member identity.

5. **Non-channel groups unaffected**: `DJReaction` only returned when `useRelays' g`. Regular groups use `DJDeliveryJob`. `isSenderFiltered` returns False for non-DJReaction scopes.

6. **Member support scope unaffected**: Returns `DJSMemberSupport`, different delivery scope and worker. Never interacts with DJReaction.

7. **Batching**: DJReaction tasks batch same-scope AND same-sender. Tag `"reaction"` separates from `DJDeliveryJob`. Both owner and subscriber bodies encode the same set of tasks.

8. **`getGroupCIReactions` counts NULL-member reactions**: Aggregation groups by reaction, counts all rows. `userReacted = MAX(reaction_sent)` — 0 for anonymous, 1 for user's own.

9. **API restriction consistent with UI**: Both use `< GROwner` threshold. `APIGetReactionMembers` returns `[]` for non-owner channel subscribers. iOS suppresses context menu.

10. **Reaction removal (add=False)**: `setGroupReactionNoMember` DELETE uses LIMIT 1 subquery. Finds one NULL-member row or no-ops if none exists.

11. **`processForwardedMsg` passes `author_` directly**: FwdMember → Just author → `groupMsgReaction (Just m)`. FwdChannel → Nothing → `groupMsgReaction Nothing`. Matches sibling handlers.

12. **Migration**: `subscriber_body` column nullable. Existing rows get NULL. `toDeliveryJob` reads as `Maybe (Binary ByteString)`.

No issues found.

### Pass 2

13. **`isSenderFiltered` correctness**: Only matches `DJSGroup {jobSpec = DJReaction}`. DJDeliveryJob and DJRelayRemoved on relay groups continue without sender filter. DJRelayRemoved doesn't use `getNextDeliveryTasks` (single-task processing). Only DJDeliveryJob and DJReaction go through the plural version. Behavior preserved for all existing paths.

14. **Subscriber body encoding includes signatures**: For signed messages, `encodeFwdElement` includes signature bytes. On receiving side, FwdChannel path discards them with `VMUnsigned chatMsg`. Extra bytes but functionally correct. No information leak.

15. **`partition` threshold `>= GROwner`**: Only GROwner gets owner body. GRAdmin, GRModerator, GRMember, GRObserver get subscriber body. Correct for channel privacy model.

16. **Subscriber body encoding size**: FwdChannel encodes smaller than FwdMember. If owner body fits in `maxEncodedMsgLength`, subscriber body fits too.

17. **`catchCINotFound` fallback in `Nothing` branch**: Stores reaction without UI event when chat item doesn't exist. Matches `Just m` branch fallback behavior.

18. **`CIChannelRcv` direction in reaction UI event**: Valid `CIDirection 'CTGroup 'MDRcv`. iOS event handler updates reaction counts regardless of direction.

19. **Old client compatibility**: FwdChannel is existing encoding. Old clients hit `withAuthor XMsgReact_` → error logged, reaction dropped. No crash. Acceptable degradation.

20. **`subscriberBody_` for non-`DJReaction` jobs**: `Maybe ByteString`, defaults to `Nothing`. `fromMaybe body subscriberBody_` fallback safe.

21. **Column mapping**: SELECT lists 9 columns matching `MessageDeliveryJobRow` type. `subscriber_body` at position 8 maps to `Maybe (Binary ByteString)`.

22. **`sentAsGroup = False` for DJReaction tasks**: Task worker reads `showGroupAsSender = False` → `fwdSender = FwdMember`. Owner body has member identity. Subscriber body explicitly overrides with FwdChannel.

No issues found. Two consecutive clean passes.
