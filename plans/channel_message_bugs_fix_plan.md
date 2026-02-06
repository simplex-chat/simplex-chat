# Plan: Channel Message Bugs Fix

## Table of Contents
1. [Executive Summary](#executive-summary)
2. [Bug 1: Delivery Context Flag](#bug-1-delivery-context-flag)
3. [Bug 2: Reaction Attribution](#bug-2-reaction-attribution)
4. [Bug 3: Update Fallback Default](#bug-3-update-fallback-default)
5. [Bug 4: Forward API Parameter](#bug-4-forward-api-parameter)
6. [Bug 5: CLI Forward Hardcode](#bug-5-cli-forward-hardcode)
7. [Test Plan](#test-plan)
8. [Implementation Order](#implementation-order)

---

## Executive Summary

**5 bugs identified** in channel message handling:

| # | Location | Bug | Severity |
|---|----------|-----|----------|
| 1 | Subscriber.hs:935-945 | Events use `isChannelOwner` instead of item's `showGroupAsSender` | Critical |
| 2 | Subscriber.hs:1818-1842 | Reactions attributed to membership instead of channel | High |
| 3 | Subscriber.hs:1950-1969 | Update fallback creates item without correct sendAsGroup flag | Medium |
| 4 | Commands.hs:930,944 | Forward API ignores `_sendAsGroup` parameter | High |
| 5 | Commands.hs:2191,2196,2201,4633 | CLI forward hardcodes False | Medium |

**Decision points requiring user input:**
- Bug 2: How to represent "channel" as reactor (4 options)
- Bug 3: Where to source sendAsGroup flag for fallback (2 options)

---

## Bug 1: Delivery Context Flag

### Current Code (Subscriber.hs:935-945)
```haskell
let isChannelOwner = useRelays' gInfo' && memberRole' m'' == GROwner
    showGroupAsSender' = case event of
      XMsgNew mc -> fromMaybe False (asGroup (mcExtMsgContent mc))
      XMsgUpdate {} -> isChannelOwner    -- BUG: should use item's flag
      XMsgDel {} -> isChannelOwner       -- BUG
      XMsgReact {} -> isChannelOwner     -- BUG
      XMsgFileDescr {} -> isChannelOwner -- BUG
      XFileCancel {} -> isChannelOwner   -- BUG
      _ -> False
```

### Problem
Events referencing existing items (update, delete, react, file) compute `showGroupAsSender'` from **current sender role** (`isChannelOwner`) instead of **item's stored `showGroupAsSender` flag**.

### Fix
Extract `showGroupAsSender` from the chat item being referenced:

```haskell
showGroupAsSender' = case event of
  XMsgNew mc -> fromMaybe False (asGroup (mcExtMsgContent mc))
  XMsgUpdate {} -> itemShowGroupAsSender ci  -- from item lookup
  XMsgDel {} -> itemShowGroupAsSender ci
  XMsgReact {} -> itemShowGroupAsSender ci
  XMsgFileDescr {} -> itemShowGroupAsSender ci
  XFileCancel {} -> itemShowGroupAsSender ci
  _ -> False
```

**Note:** The item is already looked up for these events. Need to thread the `showGroupAsSender` field from CIMeta through to this point.

### Files Modified
- `src/Simplex/Chat/Library/Subscriber.hs`: Lines 935-945

---

## Bug 2: Reaction Attribution

### Current Code (Subscriber.hs:1818-1842)
```haskell
groupMsgReaction :: GroupInfo -> Maybe GroupMember -> SharedMsgId -> Maybe MemberId -> Maybe MsgScope -> MsgReaction -> Bool -> RcvMessage -> UTCTime -> CM (Maybe DeliveryJobScope)
groupMsgReaction g m_ sharedMsgId itemMemberId scope_ reaction add RcvMessage {msgId} brokerTs
  ...
  where
    GroupInfo {membership} = g
    reactor = fromMaybe membership m_  -- BUG (line 1842): uses membership when m_ is Nothing
    ciDir = maybe CIChannelRcv CIGroupRcv m_
```

### Problem
When `m_` is `Nothing` (channel message), reactor becomes `membership` (user's own member record). Channel reactions should be attributed **to the channel**, not to the user.

### Options (REQUIRES USER DECISION)

#### Option A: Use `Nothing` for reactor
- Change type to `Maybe GroupMember` where `Nothing` = channel
- Requires signature changes to `getGroupReactions`, `updateGroupReaction`, etc.
- **Pros:** Clean representation, no fake data
- **Cons:** Many function signature changes, database schema may need update

#### Option B: Use group owner's member record
- For channel reactions, use the group owner's member record as reactor
- **Pros:** Minimal code changes, no schema changes
- **Cons:** Semantically incorrect (owner didn't react, channel did)

#### Option C: Store `sendAsGroup` flag on reaction record
- Add `sendAsGroup :: Bool` field to reaction storage
- **Pros:** Explicit flag, mirrors message approach
- **Cons:** Database migration required

#### Option D: Use a special synthetic member record
- Create a marker member record representing "channel"
- **Pros:** Works with existing types
- **Cons:** Pollutes member space with fake data

**Recommendation:** Option A (cleanest) or Option C (mirrors message pattern)

---

## Bug 3: Update Fallback Default

### Current Code (Subscriber.hs:1950-1969)
```haskell
updateRcvChatItem `catchCINotFound` \_ -> do
  (chatDir, mentions', scopeInfo) <- case m_ of
    Just m -> ...
    Nothing -> pure (CDChannelRcv gInfo Nothing, M.empty, Nothing)  -- BUG: no sendAsGroup info
  (ci, cInfo) <- saveRcvChatItem' user chatDir msg ...
```

### Problem
When `x.msg.update` arrives for a locally-deleted item in a channel (`m_` is `Nothing`), the fallback creates a new item with `CDChannelRcv gInfo Nothing` but doesn't know the original item's `sendAsGroup` flag.

### Options (REQUIRES USER DECISION)

#### Option A: Default to "sent as group" when from owner in channel
- Logic: `if isChannelOwner then sendAsGroup=True else sendAsGroup=False`
- **Pros:** No protocol change, reasonable heuristic
- **Cons:** Can be wrong if owner originally sent as member

#### Option B: Require sender to include flag in the event
- Add `asGroup` field to `XMsgUpdate` message format
- **Pros:** Explicit, always correct
- **Cons:** Protocol change, backward compatibility

**Recommendation:** Option A for simplicity (update events from owner in channel are almost always for channel messages)

---

## Bug 4: Forward API Parameter

### Current Code (Commands.hs:930,944)
```haskell
APIForwardChatItems ... _sendAsGroup -> withUser $ \user -> case toCType of
  CTGroup -> do
    ...
    sendGroupContentMessages user gInfo toScope (sendAsGroup' gInfo) False itemTTL cmrs'
    --                                          ^^^^^^^^^^^^^^^^^^^ BUG: ignores _sendAsGroup
```

### Problem
The `_sendAsGroup` parameter is received but ignored. The function computes its own `sendAsGroup' gInfo` instead.

### Fix
```haskell
APIForwardChatItems ... sendAsGroup -> withUser $ \user -> case toCType of
  CTGroup -> do
    ...
    sendGroupContentMessages user gInfo toScope sendAsGroup False itemTTL cmrs'
```

### Files Modified
- `src/Simplex/Chat/Library/Commands.hs`: Line 930 (rename parameter), Line 944 (use parameter)

---

## Bug 5: CLI Forward Hardcode

### Current Code (Commands.hs)
```haskell
-- Line 2191
processChatCommand vr nm $ APIForwardChatItems toChatRef (ChatRef CTDirect contactId Nothing) (forwardedItemId :| []) Nothing False

-- Line 2196
processChatCommand vr nm $ APIForwardChatItems toChatRef (ChatRef CTGroup groupId Nothing) (forwardedItemId :| []) Nothing False

-- Line 2201
processChatCommand vr nm $ APIForwardChatItems toChatRef (ChatRef CTLocal folderId Nothing) (forwardedItemId :| []) Nothing False

-- Line 4633
"/_forward " *> (APIForwardChatItems <$> chatRefP <* A.space <*> chatRefP <*> _strP <*> sendMessageTTLP <*> pure False),
```

### Problem
All CLI forward commands hardcode `False` for `sendAsGroup` instead of computing based on destination.

### Fix
Compute `sendAsGroup` before calling API based on destination group's channel status:

```haskell
-- Lines 2191, 2196, 2201: Need to determine sendAsGroup based on toChatRef
-- If toChatRef is a channel and user is owner, sendAsGroup should default to True

-- Line 4633: Parser should accept optional flag or default based on context
```

### Files Modified
- `src/Simplex/Chat/Library/Commands.hs`: Lines 2191, 2196, 2201, 4633

---

## Test Plan

### New Tests (6 total)

#### Test 1: `testChannelEventDeliveryContext`
**Objective:** Verify x.msg.update, x.msg.delete use item's sendAsGroup, not current role.

**Scenario:**
1. Owner sends message "as channel" (sendAsGroup=True)
2. Owner updates message
3. Verify update delivery uses sendAsGroup=True (from item), not recomputed
4. Repeat with message sent "as member" (sendAsGroup=False)
5. Verify update delivery uses sendAsGroup=False

**Coverage:** Bug 1

---

#### Test 2: `testChannelReactionAttribution`
**Objective:** Verify channel reactions are attributed correctly (not to membership).

**Scenario:**
1. Owner sends channel message
2. Owner adds reaction
3. Verify reaction is attributed to channel, not membership member record
4. Member adds reaction to channel message
5. Verify member reaction is attributed correctly

**Coverage:** Bug 2

---

#### Test 3: `testChannelUpdateFallbackSendAsGroup`
**Objective:** Verify update on deleted item creates correct sendAsGroup.

**Scenario:**
1. Owner sends channel message
2. Member receives and locally deletes
3. Owner updates message
4. Verify member's recreated item has correct sendAsGroup flag

**Coverage:** Bug 3

---

#### Test 4: `testForwardAPIUsesParameter`
**Objective:** Verify Forward API respects sendAsGroup parameter.

**Scenario:**
1. Create channel with owner
2. Forward message to channel with sendAsGroup=True
3. Verify message sent as channel
4. Forward message with sendAsGroup=False
5. Verify message sent as member

**Coverage:** Bug 4

---

#### Test 5: `testForwardCLISendAsGroup`
**Objective:** Verify CLI forward commands compute sendAsGroup correctly.

**Scenario:**
1. Create channel with owner
2. Use `/forward` to forward to channel
3. Verify sendAsGroup computed correctly (True for owner in channel)

**Coverage:** Bug 5

---

#### Test 6: `testChannelDeleteDeliveryContext`
**Objective:** Verify x.msg.del uses item's sendAsGroup flag.

**Scenario:**
1. Owner sends message as channel
2. Owner deletes message
3. Verify delete event uses item's sendAsGroup=True
4. Owner sends message as member
5. Owner deletes message
6. Verify delete event uses item's sendAsGroup=False

**Coverage:** Bug 1 (additional coverage)

---

## Implementation Order

### Phase 1: Critical Fix (Bug 1)
1. Fix delivery context in Subscriber.hs
2. Add Test 1 and Test 6

### Phase 2: API Fixes (Bugs 4, 5)
1. Fix Forward API parameter usage
2. Fix CLI forward hardcodes
3. Add Tests 4 and 5

### Phase 3: Behavior Fixes (Bugs 2, 3)
**Requires user decisions first**
1. Implement chosen option for reaction attribution
2. Implement chosen option for update fallback
3. Add Tests 2 and 3

---

## Files Summary

| File | Changes |
|------|---------|
| `src/Simplex/Chat/Library/Subscriber.hs` | Lines 935-945 (Bug 1), 1818-1842 (Bug 2), 1950-1969 (Bug 3) |
| `src/Simplex/Chat/Library/Commands.hs` | Lines 930,944 (Bug 4), 2191,2196,2201,4633 (Bug 5) |
| `tests/ChatTests/Groups.hs` | Add 6 new tests |

---

## Decision Points

Please confirm your choice for:

1. **Bug 2 (Reaction Attribution):**
   - [ ] Option A: Use `Nothing` for reactor (cleanest, requires more changes)
   - [ ] Option B: Use group owner's member record
   - [ ] Option C: Store `sendAsGroup` flag on reaction (mirrors message pattern)
   - [ ] Option D: Use synthetic member record

2. **Bug 3 (Update Fallback):**
   - [ ] Option A: Default to sendAsGroup=True when from owner in channel
   - [ ] Option B: Add `asGroup` field to XMsgUpdate protocol
