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

**Receiving subscriber**: When `XMsgReact` arrives via `FwdChannel` (no author), a new function `groupMsgReactionNoMember` processes and stores the reaction with `group_member_id = NULL`. The UI shows updated counts without member attribution.

The existing `sentAsGroup`/`message_from_channel` mechanism is not involved — `DJReaction` handles the split directly in the task/job workers.

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

### 4. Relay-side: `groupMsgReaction` returns `DJReaction` for channels

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (~line 1922)

In `groupMsgReaction`, for channel groups (`useRelays' g`), return `DJReaction` instead of `DJDeliveryJob`.

The `catchCINotFound` fallback, `Nothing` scope branch (line 1936–1938):

```haskell
Nothing -> do
  withStore' $ \db -> setGroupReaction db g m itemMemberId sharedMsgId False reaction add msgId brokerTs
  let jobSpec = if useRelays' g then DJReaction else DJDeliveryJob {includePending = False}
  pure $ Just $ DeliveryTaskContext (DJSGroup {jobSpec}) False
```

The `updateChatItemReaction` path (line 1949–1957):

```haskell
pure $ Just $ case scopeInfo of
  Nothing | useRelays' g -> DeliveryTaskContext (DJSGroup {jobSpec = DJReaction}) False
  _ -> infoToDeliveryContext g scopeInfo False
```

The member support scope branch (line 1929–1934) is unaffected — it returns `DJSMemberSupport`, a different delivery scope.

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

### 7. Receiving side: handle `FwdChannel` + `XMsgReact`

**File**: `src/Simplex/Chat/Library/Subscriber.hs` (~line 3349)

In `processForwardedMsg`, change `XMsgReact` handling from `withAuthor` to case on `author_`:

```haskell
XMsgReact sharedMsgId memId scope_ reaction add -> case author_ of
  Just author -> void $ groupMsgReaction gInfo author sharedMsgId memId scope_ reaction add rcvMsg msgTs
  Nothing -> groupMsgReactionNoMember gInfo sharedMsgId memId reaction add rcvMsg msgTs
```

### 8. New function: `groupMsgReactionNoMember`

**File**: `src/Simplex/Chat/Library/Subscriber.hs`

New function in the `processGroupMessage` where-clause (same scope as `groupMsgReaction`, has access to `user`, `vr`, `toView`, `catchCINotFound`):

```haskell
groupMsgReactionNoMember :: GroupInfo -> SharedMsgId -> Maybe MemberId -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> CM ()
groupMsgReactionNoMember g sharedMsgId itemMemberId reaction add RcvMessage {msgId} brokerTs
  | groupFeatureAllowed SGFReactions g =
      updateChatItemReaction `catchCINotFound` \_ ->
        withStore' $ \db -> setGroupReactionNoMember db g itemMemberId sharedMsgId reaction add msgId brokerTs
  | otherwise = pure ()
  where
    updateChatItemReaction = do
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
```

Key differences from `groupMsgReaction`:
- No per-member dedup check — trusts relay's existing `reactionAllowed` validation
- Stores with `group_member_id = NULL` via `setGroupReactionNoMember`
- Uses `CIChannelRcv` direction in the UI event (no member attribution)
- Returns `()` — no delivery context (subscriber doesn't forward)
- No member support scope handling (irrelevant for channel subscriber reactions)

### 9. New storage function: `setGroupReactionNoMember`

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

### 10. API restriction

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

### 11. iOS UI: suppress reaction context menu

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
| `src/Simplex/Chat/Library/Subscriber.hs` | `groupMsgReaction` returns `DJReaction` for channels; task/job workers handle `DJReaction`; `processForwardedMsg` handles `FwdChannel`+`XMsgReact`; new `groupMsgReactionNoMember` |
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

9. **Reaction removal (add=False)**: Relay processes removal, returns `DJReaction`. Task/job workers encode both bodies. Owner receives `FwdMember` removal — `setGroupReaction` deletes the specific member's reaction row. Subscriber receives `FwdChannel` removal — `setGroupReactionNoMember` deletes one NULL-member row via `LIMIT 1`. Correct.

10. **`saveGroupFwdRcvMsg` with `Nothing` author**: Already works — `refAuthorId = groupMemberId' <$> refAuthorMember_` handles `Nothing` (Internal.hs:2283). Dedup via `sharedMsgId_` is unaffected.

11. **`createMsgDeliveryJob` call sites**: Three existing call sites (Subscriber.hs:3531, 3548; Commands.hs:2927) need `Nothing` as the new subscriber body parameter. Mechanical change.

No issues found.

### Pass 2

12. **Subscriber body encoding size**: `FwdChannel` encodes as `"C"` (1 byte) vs `FwdMember` which is `"M" + memberId + memberName` (variable, larger). If the owner body fits within `maxEncodedMsgLength`, the subscriber body will also fit (same content, smaller wrapper). No overflow risk.

13. **`catchCINotFound` in `groupMsgReactionNoMember`**: When the chat item doesn't exist yet, the fallback stores the reaction with NULL member via `setGroupReactionNoMember`. When the chat item eventually arrives, `getGroupCIReactions` picks up all stored reactions including NULL-member ones. Counts are correct.

14. **`CIChannelRcv` direction in reaction UI event**: `CIReaction` uses `chatDir :: CIDirection c d`. `CIChannelRcv` is a valid `CIDirection 'CTGroup 'MDRcv`. The iOS event handler (`.chatItemReaction`) just calls `m.updateChatItem(r.chatInfo, r.chatReaction.chatItem)` — it updates the chat item's reaction counts regardless of direction. No UI issue.

15. **Old client compatibility**: Old clients that receive `FwdChannel` + `XMsgReact` parse `FwdChannel` correctly (it's an existing encoding). They hit `withAuthor XMsgReact_` → error logged, reaction silently dropped. Acceptable degradation — no crash, reaction count is marginally lower until client update.

16. **`subscriberBody_` for non-`DJReaction` jobs**: The field is `Maybe ByteString`, defaults to `Nothing` for all other job types. The `fromMaybe body subscriberBody_` fallback in the job worker ensures non-owners get `body` if no subscriber body exists. Safe.

No issues found. Two consecutive clean passes.
