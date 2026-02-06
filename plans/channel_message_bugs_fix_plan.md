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
| 2 | Subscriber.hs:1818-1842 | Reactions allow `m_=Nothing` and fall back to membership | High |
| 3 | Subscriber.hs:1950-1969 | Update fallback creates item without correct sendAsGroup flag | Medium |
| 4 | Commands.hs:930,944 | Forward API ignores `_sendAsGroup` parameter | High |
| 5 | Commands.hs:2191,2196,2201,4633 | CLI forward hardcodes False | Medium |

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

**Note:** Use `chatDir` from ChatItem and pattern match on `CIChannelRcv` to determine sendAsGroup flag.

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
When `m_` is `Nothing`, reactor incorrectly falls back to `membership` (user's own member record). However, reactions should always come from an identifiable member - the `m_` parameter should never be `Nothing` for reactions.

### Fix
Reactions can only come from members (including owners), never from channels. XMsgReact handler must be reworked to require `GroupMember` instead of `Maybe GroupMember`. The `m_` parameter should not be optional for reactions.

### Files Modified
- `src/Simplex/Chat/Library/Subscriber.hs`: Lines 1818-1842

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

### Fix (Option B: Require sender to include flag in the event)
Add `asGroup` field to `XMsgUpdate` message format.

**Rationale:** We don't know what owner wants otherwise - it may send as channel or it may send as owner, and different members must have the same view (e.g. when multiple relays are used, it would be random).

### Files Modified
- `src/Simplex/Chat/Library/Subscriber.hs`: Lines 1950-1969
- Protocol message format (XMsgUpdate)

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

-- Line 4633: Parser should accept optional flag (parser cannot know context)
```

### Files Modified
- `src/Simplex/Chat/Library/Commands.hs`: Lines 2191, 2196, 2201, 4633

---

## Test Plan

### New Tests (6 total)

#### Test 1: `testChannelEventDeliveryContext`
**Objective:** Verify x.msg.update, x.msg.delete, x.msg.file.descr, and x.file.cancel use item's sendAsGroup, not current role.

**Scenario:**
1. Owner sends message "as channel" (sendAsGroup=True) with file attachment
2. Owner updates message
3. Verify update delivery uses sendAsGroup=True (from item), not recomputed
4. Owner sends file description update
5. Verify x.msg.file.descr uses sendAsGroup=True (from item)
6. Owner cancels file
7. Verify x.file.cancel uses sendAsGroup=True (from item)
8. Repeat steps 1-7 with message sent "as member" (sendAsGroup=False)
9. Verify all events use sendAsGroup=False

**Coverage:** Bug 1

---

#### Test 2: `testChannelReactionAttribution`
**Objective:** Verify reactions require a member sender (not optional).

**Scenario:**
1. Owner sends channel message
2. Owner adds reaction (as member, not as channel)
3. Verify reaction is attributed to owner's member record
4. Member adds reaction to channel message
5. Verify member reaction is attributed correctly
6. Verify channel cannot send reactions (m_ must be Just)

**Coverage:** Bug 2

---

#### Test 3: `testChannelUpdateFallbackSendAsGroup`
**Objective:** Verify update on deleted item creates correct sendAsGroup from protocol field.

**Scenario:**
1. Owner sends channel message (sendAsGroup=True)
2. Member receives and locally deletes
3. Owner updates message (XMsgUpdate includes asGroup=True)
4. Verify member's recreated item has sendAsGroup=True
5. Owner sends message as member (sendAsGroup=False)
6. Member receives and locally deletes
7. Owner updates message (XMsgUpdate includes asGroup=False)
8. Verify member's recreated item has sendAsGroup=False

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
1. Rework XMsgReact handler to require GroupMember (not Maybe GroupMember)
2. Add asGroup field to XMsgUpdate protocol message
3. Add Tests 2 and 3

---

## Files Summary

| File | Changes |
|------|---------|
| `src/Simplex/Chat/Library/Subscriber.hs` | Lines 935-945 (Bug 1), 1818-1842 (Bug 2), 1950-1969 (Bug 3) |
| `src/Simplex/Chat/Library/Commands.hs` | Lines 930,944 (Bug 4), 2191,2196,2201,4633 (Bug 5) |
| Protocol message types | Add asGroup field to XMsgUpdate (Bug 3) |
| `tests/ChatTests/Groups.hs` | Add 6 new tests |
