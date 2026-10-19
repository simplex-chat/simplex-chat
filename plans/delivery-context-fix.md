# Plan: Fix Channel Message Delivery Architecture

## Table of Contents
1. [Context](#context)
2. [Executive Summary](#executive-summary)
3. [Issue 1: Eliminate memberForChannel/memberIdForChannel](#issue-1)
4. [Issue 2: groupMsgReaction required GroupMember](#issue-2)
5. [Issue 3: Fix groupMessageUpdate lookup](#issue-3)
6. [Issue 4: DeliveryTaskContext type](#issue-4)
7. [Issue 5: Fix testChannelReactionAttribution](#issue-5)
8. [Issue 6: Fix testChannelUpdateFallbackSendAsGroup comment](#issue-6)
9. [Other: sendAsGroup parameter ordering](#other-issue)
10. [Verification](#verification)

## Context

The current implementation on `ep/channel-messages-2` determines delivery context (whether to forward messages as channel or as member) using `isChannelOwner` — inferring from the sender's role whether they're the channel owner. This is architecturally wrong: the delivery context should be determined **from the item's direction** (`CIChannelRcv` vs `CIGroupRcv`), not from who sent it. The `f/msg-from-channel` branch has the correct approach.

## Executive Summary

7 changes across 7 files:
1. **Delivery.hs** — Add `DeliveryTaskContext` type, update `NewMessageDeliveryTask` only (`MessageDeliveryTask` unchanged)
2. **Subscriber.hs** — Eliminate `isChannelOwner`/`memberForChannel`/`memberIdForChannel`; all processing functions return `Maybe DeliveryTaskContext`; determine `sentAsGroup` from item direction; `groupMsgReaction` takes required `GroupMember`; add `withAuthor` in forwarded handler
3. **Store/Delivery.hs** — Update SQL row mapping for `taskContext`
4. **Commands.hs** — Reorder `sendAsGroup` param in `APIForwardChatItems`
5. **Store/Messages.hs** — Reorder `showGroupAsSender` param in `createNewSndChatItem`
6. **Internal.hs** — Reorder `showGroupAsSender` param in `saveSndChatItems`, `prepareGroupMsg`
7. **Tests** — Fix reaction test comment/expectations, fix update fallback test comment

---

## Issue 1: Eliminate memberForChannel/memberIdForChannel {#issue-1}

**File:** `src/Simplex/Chat/Library/Subscriber.hs` lines 935-937, 939-991

**Problem:** `isChannelOwner`, `memberForChannel`, `memberIdForChannel` computed at lines 935-937 and passed to processing functions. This pre-infers delivery context from member role.

**Fix:** Remove these three bindings entirely. Always pass `(Just m'')` to functions that take `Maybe GroupMember`. Functions determine `sentAsGroup` from item direction internally.

**Direct handler changes (lines 939-991):**
```
-- BEFORE:
let isChannelOwner = useRelays' gInfo' && memberRole' m'' == GROwner
    memberForChannel = if isChannelOwner then Nothing else Just m''
    memberIdForChannel = memberId' <$> memberForChannel
(deliveryJobScope_, showGroupAsSender') <- case event of
  ...
forM deliveryJobScope_ $ \jobScope ->
  pure $ NewMessageDeliveryTask {messageId = msgId, jobScope, showGroupAsSender = showGroupAsSender'}

-- AFTER:
deliveryTaskContext_ <- case event of
  XMsgNew mc -> ...  -- returns Maybe DeliveryTaskContext
  XMsgFileDescr ... -> groupMessageFileDescription gInfo' (Just m'') sharedMsgId fileDescr
  XMsgUpdate ... -> memberCanSend m'' msgScope Nothing $ groupMessageUpdate gInfo' (Just m'') sharedMsgId ...
  XMsgDel ... -> groupMessageDelete gInfo' (Just m'') sharedMsgId ...
  XMsgReact ... -> groupMsgReaction gInfo' m'' sharedMsgId ...  -- required member
  XFileCancel sharedMsgId -> xFileCancelGroup gInfo' (Just m'') sharedMsgId
  ...other events -> Just <$> memberEventDeliveryContext m'' / Nothing
forM deliveryTaskContext_ $ \taskContext ->
  pure $ NewMessageDeliveryTask {messageId = msgId, taskContext}
```

**Processing function signature changes:**
- `groupMessageFileDescription :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> FileDescr -> CM (Maybe DeliveryTaskContext)` — drop both `Maybe MemberId` params, pass `Maybe GroupMember`, determine `sentAsGroup` from `chatDir` of found item
- `groupMessageUpdate :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> ... -> Maybe Bool -> CM (Maybe DeliveryTaskContext)` — drop `senderGMId_` param
- `groupMessageDelete :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> ... -> CM (Maybe DeliveryTaskContext)` — drop `senderGMId_` param; fix `findOwnerCI` dual-lookup (lines 2028-2035) same as Issue 3: when `m_ = Nothing` search with `Nothing`, when `m_ = Just m` use member lookup directly
- `xFileCancelGroup :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> CM (Maybe DeliveryTaskContext)` — drop both `Maybe MemberId` params

**`validSender` simplification:** Remove second `Maybe MemberId` parameter. With `(Just m'')` always passed, validation is just:
```haskell
validSender :: Maybe MemberId -> CIDirection 'CTGroup 'MDRcv -> Bool
validSender (Just mId) (CIGroupRcv m) = sameMemberId mId m
validSender Nothing CIChannelRcv = True
validSender _ _ = False
```

**`isChannelDir` helper** remains as-is (line 1870-1872) — used to derive `sentAsGroup` from item's `chatDir`.

**`memberCanSend`** (line 1436): Generic signature `a -> CM a -> CM a` — no change needed. Default values at call sites change from `(Nothing, False)` to `Nothing`.

**`memberCanSend'`** (line 1448): Return type changes from `CM (Maybe DeliveryJobScope)` to `CM (Maybe DeliveryTaskContext)`. Used in forwarded handler (lines 3153, 3159).

---

## Issue 2: groupMsgReaction required GroupMember {#issue-2}

**File:** `src/Simplex/Chat/Library/Subscriber.hs` line 1814

**Problem:** `groupMsgReaction :: GroupInfo -> Maybe GroupMember -> ...` allows `Nothing`, uses `fromMaybe membership m_` fallback.

**Fix:** Change to required `GroupMember`:
```haskell
groupMsgReaction :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe MemberId -> Maybe MsgScope -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> CM (Maybe DeliveryTaskContext)
```

- No `reactor` binding needed — use `m` directly (eliminates `fromMaybe membership m_` fallback)
- `ciDir = CIGroupRcv (Just m)` (reactions always attributed to member)
- Always return `sentAsGroup = False` — reactions are never from channel
- Return type: `Maybe DeliveryTaskContext` (not tuple)

**Direct handler call site (line 958-960):**
```haskell
XMsgReact sharedMsgId memberId scope_ reaction add ->
  groupMsgReaction gInfo' m'' sharedMsgId memberId scope_ reaction add msg brokerTs
```

**Forwarded handler call site (line 3162-3163):**
```haskell
XMsgReact sharedMsgId memId_ scope_ reaction add ->
  withAuthor XMsgReact_ $ \author -> groupMsgReaction gInfo author sharedMsgId memId_ scope_ reaction add rcvMsg msgTs
```

---

## Issue 3: Fix groupMessageUpdate lookup {#issue-3}

**File:** `src/Simplex/Chat/Library/Subscriber.hs` lines 1973-1994

**Problem:** Dual-lookup with `catchError` tries `Nothing` first, then falls back to `senderGMId_`. This is wrong — the `asGroup_` flag from XMsgUpdate should drive the search.

**Fix:** Use `asGroup_` (the wire flag) to determine search strategy. No `senderGMId_` parameter needed:
```haskell
updateRcvChatItem = do
  (cci, scopeInfo) <- withStore $ \db -> do
    cci <- case m_ of
      Just m -> getGroupMemberCIBySharedMsgId db user gInfo (memberId' m) sharedMsgId
      Nothing -> getGroupChatItemBySharedMsgId db user gInfo Nothing sharedMsgId
    (cci,) <$> getGroupChatScopeInfoForItem db vr user gInfo (cChatItemId cci)
```

When `m_ = Nothing` (channel owner as channel), search with `Nothing` group_member_id → finds channel items.
When `m_ = Just m` (attributed member message), search with member's `memberId` → finds member items.

The `isSender` check also simplifies — just check `m_` matches the found item's member.

**Fallback path** (lines 1948-1968, `catchCINotFound`): When item not found, `showGroupAsSender` is derived from `asGroup_` flag (or defaults based on `m_`), which maps to `sentAsGroup` in the `DeliveryTaskContext`.

---

## Issue 4: DeliveryTaskContext type {#issue-4}

**File:** `src/Simplex/Chat/Delivery.hs`

### 4a. Add DeliveryTaskContext type
```haskell
data DeliveryTaskContext = DeliveryTaskContext
  { jobScope :: DeliveryJobScope,
    sentAsGroup :: ShowGroupAsSender
  }
  deriving (Show)
```

Uses existing `type ShowGroupAsSender = Bool` from Messages.hs.

### 4b. Modify existing helpers
Rename `infoToDeliveryScope` → `infoToDeliveryContext`, inline the scope logic, add `ShowGroupAsSender` parameter:
```haskell
infoToDeliveryContext :: GroupInfo -> Maybe GroupChatScopeInfo -> ShowGroupAsSender -> DeliveryTaskContext
infoToDeliveryContext GroupInfo {membership} scopeInfo sentAsGroup = DeliveryTaskContext {jobScope, sentAsGroup}
  where
    jobScope = case scopeInfo of
      Nothing -> DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}
      Just GCSIMemberSupport {groupMember_} ->
        let supportGMId = groupMemberId' $ fromMaybe membership groupMember_
         in DJSMemberSupport {supportGMId}
```
Remove `infoToDeliveryScope` entirely.

Rename `memberEventDeliveryScope` → `memberEventDeliveryContext`, change return type:
```haskell
memberEventDeliveryContext :: GroupMember -> Maybe DeliveryTaskContext
memberEventDeliveryContext m@GroupMember {memberRole, memberStatus}
  | memberStatus == GSMemPendingApproval = Nothing
  | memberStatus == GSMemPendingReview = Just $ DeliveryTaskContext {jobScope = DJSMemberSupport {supportGMId = groupMemberId' m}, sentAsGroup = False}
  | memberRole >= GRModerator = Just $ DeliveryTaskContext {jobScope = DJSGroup {jobSpec = DJDeliveryJob {includePending = True}}, sentAsGroup = False}
  | otherwise = Just $ DeliveryTaskContext {jobScope = DJSGroup {jobSpec = DJDeliveryJob {includePending = False}}, sentAsGroup = False}
```

### 4c. Update NewMessageDeliveryTask
```haskell
data NewMessageDeliveryTask = NewMessageDeliveryTask
  { messageId :: MessageId,
    taskContext :: DeliveryTaskContext
  }
  deriving (Show)
```

### 4d. MessageDeliveryTask — no change

`MessageDeliveryTask` stays as-is. It's constructed from DB rows in `getMsgDeliveryTask_` and consumed by relay forwarding code — those consumers need `jobScope` and `fwdSender` directly, not `DeliveryTaskContext`. `DeliveryTaskContext` is only for the path from processing functions → `NewMessageDeliveryTask` creation.

### 4e. Update Store/Delivery.hs

**`createMsgDeliveryTask`** (line 71-87): Extract `jobScope` and `sentAsGroup` from `taskContext` instead of separate `jobScope`/`showGroupAsSender` fields.

**`getMsgDeliveryTask_`** — no change needed (`MessageDeliveryTask` unchanged).

### 4f. Consumers of MessageDeliveryTask — no change needed

**Subscriber.hs** lines ~3325-3333 and **Messages/Batch.hs** lines ~77-80 already pattern match on `FwdSender` and use `jobScope` from `MessageDeliveryTask`. Since `MessageDeliveryTask` is unchanged, no updates needed.

### 4g. Return type changes in processing functions

All functions currently returning `(Maybe DeliveryJobScope, ShowGroupAsSender)` change to `Maybe DeliveryTaskContext`:
- `groupMessageFileDescription` → `CM (Maybe DeliveryTaskContext)`
- `groupMessageUpdate` → `CM (Maybe DeliveryTaskContext)`
- `groupMessageDelete` → `CM (Maybe DeliveryTaskContext)`
- `xFileCancelGroup` → `CM (Maybe DeliveryTaskContext)`
- `groupMsgReaction` → `CM (Maybe DeliveryTaskContext)`

Events that return `(Nothing, False)` or `(Just scope, False)` are updated:
- `(Nothing, False)` → `Nothing`
- `(Just scope, False)` → `Just $ DeliveryTaskContext scope False` (or use `memberEventDeliveryContext`)
- `(Just scope, showGroupAsSender)` → `Just $ DeliveryTaskContext scope showGroupAsSender` (or use `infoToDeliveryContext`)

---

## Issue 5: Fix testChannelReactionAttribution {#issue-5}

**File:** `tests/ChatTests/Groups.hs` lines 9057-9084

**Problem:** Comment says "reaction is forwarded as channel (owner is anonymous)" and expects `#team>`. Owner should react **as member** — reactions are always `sentAsGroup = False`.

**Fix:** Change comment and expectations:
```haskell
-- owner reacts to own member message - reaction is forwarded as member
alice ##> "+1 #team hello"
alice <## "added 👍"
bob <# "#team alice> > alice hello"
bob <## "    + 👍"
concurrentlyN_
  [ do cath <# "#team alice> > alice hello"
       cath <## "    + 👍",
    do dan <# "#team alice> > alice hello"
       dan <## "    + 👍",
    do eve <# "#team alice> > alice hello"
       eve <## "    + 👍"
  ]
```

---

## Issue 6: Fix testChannelUpdateFallbackSendAsGroup comment {#issue-6}

**File:** `tests/ChatTests/Groups.hs` line 9127

**Problem:** Comment says "bob's internally deleted item is still in DB, update finds it with correct member direction". This is wrong — the item was internally deleted, then XMsgUpdate re-creates it via the `catchCINotFound` fallback.

**Fix:** Change comment to:
```haskell
-- bob's internally deleted item is re-created as from member (sendAsGroup=False)
```

---

## Other: sendAsGroup parameter ordering {#other-issue}

**Problem:** `sendAsGroup`/`ShowGroupAsSender` should come right after direction/scope, not at the end.

### 7a. `APIForwardChatItems` constructor

**File:** `src/Simplex/Chat/Library/Commands.hs` (ChatCommand type definition + parser)

Current: `APIForwardChatItems toChat fromChat itemIds itemTTL sendAsGroup`
New: `APIForwardChatItems toChat sendAsGroup fromChat itemIds itemTTL`

Affects:
- Constructor definition in `src/Simplex/Chat/Controller.hs` line 341
- Parser at line 4639
- Call sites at lines 930, 2192, 2198, 2204

### 7b. `createNewSndChatItem`

**File:** `src/Simplex/Chat/Store/Messages.hs` line 528

Current: `createNewSndChatItem db user chatDirection msg ciContent quotedItem itemForwarded timed live hasLink showGroupAsSender createdAt`
New: `createNewSndChatItem db user chatDirection showGroupAsSender msg ciContent quotedItem itemForwarded timed live hasLink createdAt`

Move `showGroupAsSender` right after `chatDirection` (direction context).

Affects call site in `Internal.hs` line 2276.

### 7c. `saveSndChatItems`

**File:** `src/Simplex/Chat/Library/Internal.hs` line 2256-2265

Current param order: `user -> cd -> itemsData -> itemTimed -> live -> showGroupAsSender`
New: `user -> cd -> showGroupAsSender -> itemsData -> itemTimed -> live`

Move `showGroupAsSender` right after `cd` (direction context).

Affects call sites: Internal.hs line 2242, Commands.hs lines 2561, 2608 (and the `saveSndChatItem'` wrapper at line 2240).

### 7d. `prepareGroupMsg`

**File:** `src/Simplex/Chat/Library/Internal.hs` line 203

Current: `prepareGroupMsg db user gInfo msgScope mc mentions quotedItemId_ itemForwarded fInv_ timed_ live showGroupAsSender`
New: `prepareGroupMsg db user gInfo msgScope showGroupAsSender mc mentions quotedItemId_ itemForwarded fInv_ timed_ live`

Move `showGroupAsSender` right after `msgScope` (scope context).

Affects call sites: Internal.hs line 1249, Commands.hs line 4094.

---

## Forwarded handler (xGrpMsgForward) changes

**File:** `src/Simplex/Chat/Library/Subscriber.hs` lines 3136-3173

Add `withAuthor` helper to replace ad-hoc `| Just author <- author_` guards:
```haskell
where
  withAuthor :: CMEventTag e -> (GroupMember -> CM ()) -> CM ()
  withAuthor tag action = case author_ of
    Just author -> action author
    Nothing -> messageError $ "x.grp.msg.forward: event " <> tshow tag <> " requires author"
```

Update forwarded event handling:
- `XMsgFileDescr` → pass `author_` (Maybe GroupMember) directly
- `XMsgUpdate` → pass `author_` directly, void result
- `XMsgDel` → pass `author_` directly, void result
- `XMsgReact` → use `withAuthor` (required member)
- `XFileCancel` → pass `author_` directly
- Other events with `| Just author <- author_` → use `withAuthor`

---

## Files Modified

| File | Changes |
|------|---------|
| `src/Simplex/Chat/Delivery.hs` | Add `DeliveryTaskContext`, update `NewMessageDeliveryTask` only |
| `src/Simplex/Chat/Store/Delivery.hs` | Update `createMsgDeliveryTask` to extract from `taskContext` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Eliminate `isChannelOwner`/`memberForChannel`/`memberIdForChannel`; change function signatures to return `Maybe DeliveryTaskContext`; add `withAuthor`; simplify `validSender`; `groupMsgReaction` required member; fix lookup |
| `src/Simplex/Chat/Controller.hs` | Reorder `sendAsGroup` in `APIForwardChatItems` constructor |
| `src/Simplex/Chat/Library/Commands.hs` | Reorder `sendAsGroup` in `APIForwardChatItems` parser + call sites |
| `src/Simplex/Chat/Store/Messages.hs` | Reorder `showGroupAsSender` in `createNewSndChatItem` |
| `src/Simplex/Chat/Library/Internal.hs` | Reorder `showGroupAsSender` in `saveSndChatItems`, `prepareGroupMsg` |
| `src/Simplex/Chat/Messages/Batch.hs` | No change needed (`MessageDeliveryTask` unchanged) |
| `tests/ChatTests/Groups.hs` | Fix reaction test expectations + update fallback comment |

---

## Verification

1. `cabal build --ghc-options=-O0` — must compile clean
2. Run channel test suite: `cabal test simplex-chat-test --test-option='-m "channels"' --ghc-options=-O0`
3. Adversarial self-review loop until 2 consecutive clean passes
4. Verify no `isChannelOwner` references remain in Subscriber.hs direct handler
5. Verify `groupMsgReaction` signature has required `GroupMember` (no Maybe)
6. Verify no dual-lookup with `catchError` in `groupMessageUpdate`
