# Diff Minimization Plan for Subscriber.hs

## Executive Summary

The current diff introduces channel message support but creates unnecessary noise through:
1. Variable renames that don't serve any purpose
2. Extracting shared helpers when the original functions could be extended in-place
3. Creating wrapper functions that invert the natural call hierarchy

The key insight: **instead of creating `groupOrChannelX` helpers and making original functions into wrappers, we should extend the original functions to handle the channel case directly**. This minimizes diff and keeps the code structure familiar.

## Table of Contents

1. [Issue Analysis](#issue-analysis)
2. [Proposed Changes](#proposed-changes)
3. [Implementation Order](#implementation-order)
4. [Expected Diff Reduction](#expected-diff-reduction)

---

## Issue Analysis

### Issue 1: Unnecessary Variable Renames

**Current state:**
- `itemMemberId` renamed to `itemMemberId_` in `groupOrChannelReaction`
- `memberId` renamed to `memberId_` in `groupOrChannelFileDescription` and `xFileCancelGroupOrChannel`
- `aci` renamed to `aci'` in helper functions

**Problem:** These renames add diff noise without semantic benefit.

**Solution:** Keep original variable names. The `Maybe` wrapper on the type already indicates optionality - no need to signal it again in the name.

### Issue 2: Inverted Function Hierarchy (Reactions)

**Current state:**
```haskell
groupMsgReaction g m sharedMsgId itemMemberId ... =
  groupOrChannelReaction g m (Just itemMemberId) sharedMsgId ...

groupOrChannelReaction g m itemMemberId_ sharedMsgId ... =
  -- full implementation
```

**Problem:**
- Creates a new function name (`groupOrChannelReaction`) that callers must learn
- Makes `groupMsgReaction` a trivial wrapper
- Adds unnecessary indirection

**Solution:** Extend `groupMsgReaction` signature to take `Maybe MemberId` directly. The function name is still accurate - it handles group message reactions, which now includes channel messages (a special case of group messages).

```haskell
-- Change signature, keep name
groupMsgReaction :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe MemberId -> Maybe MsgScope -> ...
groupMsgReaction g m sharedMsgId itemMemberId scope_ ... =
  -- full implementation with Maybe handling
```

Call sites:
- `XMsgReact sharedMsgId (Just memberId) ...` → `groupMsgReaction ... (Just memberId) ...`
- `XMsgReact sharedMsgId Nothing ...` → `groupMsgReaction ... Nothing ...`

### Issue 3: Inverted Function Hierarchy (File Description)

**Current state:**
```haskell
groupMessageFileDescription g GroupMember {memberId} sharedMsgId fileDescr =
  groupOrChannelFileDescription g (Just memberId) sharedMsgId fileDescr

groupOrChannelFileDescription g memberId_ sharedMsgId fileDescr =
  -- full implementation
```

**Problem:** Same as Issue 2 - inverted hierarchy with unnecessary wrapper.

**Solution:** Extend `groupMessageFileDescription` to take `Maybe MemberId`:

```haskell
groupMessageFileDescription :: GroupInfo -> Maybe MemberId -> SharedMsgId -> FileDescr -> CM (Maybe DeliveryJobScope)
groupMessageFileDescription g memberId sharedMsgId fileDescr = do
  -- existing implementation with Maybe handling added
```

The new `channelMessageFileDescription` becomes a simple call:
```haskell
channelMessageFileDescription gInfo sharedMsgId fileDescr =
  groupMessageFileDescription gInfo Nothing sharedMsgId fileDescr
```

### Issue 4: Inverted Function Hierarchy (File Cancel)

**Current state:**
```haskell
xFileCancelGroup g GroupMember {memberId} sharedMsgId =
  xFileCancelGroupOrChannel g (Just memberId) sharedMsgId

xFileCancelGroupOrChannel g memberId_ sharedMsgId =
  -- full implementation
```

**Problem:** Same pattern - inverted hierarchy.

**Solution:** Extend `xFileCancelGroup` to take `Maybe MemberId`:

```haskell
xFileCancelGroup :: GroupInfo -> Maybe MemberId -> SharedMsgId -> CM (Maybe DeliveryJobScope)
xFileCancelGroup g memberId sharedMsgId = do
  -- existing implementation with Maybe handling
```

The new `xFileCancelChannel` becomes:
```haskell
xFileCancelChannel gInfo sharedMsgId =
  xFileCancelGroup gInfo Nothing sharedMsgId
```

### Issue 5: Extracted `process` and `cancel` Helpers

**Current state:** The `process` helper in `groupOrChannelFileDescription` and `cancel` helper in `xFileCancelGroupOrChannel` were extracted from inline code.

**Problem:** Moving code to `where` clause changes the diff structure unnecessarily.

**Solution:** Keep the code inline within the case branches, as it was originally. The duplication between `(Just memberId, CIGroupRcv m)` and `(Nothing, CIChannelRcv)` branches is minimal and acceptable per CODE.md guidelines (type-driven development: duplicate pattern match arms across semantic constructors are acceptable).

### Issue 6: fwdChannelReaction Placement and Duplication

**Current state:** `fwdChannelReaction` is defined at the end of `xGrpMsgForward`, far from other reaction functions.

**Analysis:** `fwdChannelReaction` handles forwarded channel reactions differently from `groupMsgReaction` because:
- It doesn't return `Maybe DeliveryJobScope` (returns `()`)
- It uses `membership` as default reactor
- It builds `CIReaction` with `ciReaction` which can be `CIChannelRcv` or `CIGroupRcv`

**Problem:** The logic is ~90% similar to `groupMsgReaction`'s forwarded case but with subtle differences.

**Solution:** This is a legitimate new function. Keep it, but consider if `groupMsgReaction` could be reused by having callers discard the result. However, the `ciReaction` construction differs semantically. **Keep as-is** - this is genuine new functionality, not unnecessary duplication.

---

## Proposed Changes

### Change 1: Inline `groupOrChannelReaction` into `groupMsgReaction`

**Before:**
```haskell
groupMsgReaction g m sharedMsgId itemMemberId scope_ reaction add msg brokerTs =
  groupOrChannelReaction g m (Just itemMemberId) sharedMsgId scope_ reaction add msg brokerTs

groupOrChannelReaction :: GroupInfo -> GroupMember -> Maybe MemberId -> SharedMsgId -> ...
groupOrChannelReaction g m@GroupMember {memberRole} itemMemberId_ sharedMsgId ... = ...
```

**After:**
```haskell
groupMsgReaction :: GroupInfo -> GroupMember -> SharedMsgId -> Maybe MemberId -> Maybe MsgScope -> ...
groupMsgReaction g m@GroupMember {memberRole} sharedMsgId itemMemberId scope_ reaction add RcvMessage {msgId} brokerTs
  | groupFeatureAllowed SGFReactions g = do
      rs <- withStore' $ \db -> getGroupReactions db g m itemMemberId sharedMsgId False
      -- ... rest of implementation using itemMemberId directly (not itemMemberId_)
```

**Call site changes:**
- `XMsgReact sharedMsgId (Just memberId) scope_ reaction add -> groupMsgReaction gInfo' m'' sharedMsgId (Just memberId) scope_ reaction add msg brokerTs`
- `XMsgReact sharedMsgId Nothing scope_ reaction add -> groupMsgReaction gInfo' m'' sharedMsgId Nothing scope_ reaction add msg brokerTs`

### Change 2: Inline `groupOrChannelFileDescription` into `groupMessageFileDescription`

**Before:**
```haskell
groupMessageFileDescription g GroupMember {memberId} sharedMsgId fileDescr =
  groupOrChannelFileDescription g (Just memberId) sharedMsgId fileDescr

groupOrChannelFileDescription :: GroupInfo -> Maybe MemberId -> SharedMsgId -> FileDescr -> ...
```

**After:**
```haskell
groupMessageFileDescription :: GroupInfo -> Maybe MemberId -> SharedMsgId -> FileDescr -> CM (Maybe DeliveryJobScope)
groupMessageFileDescription g@GroupInfo {groupId} memberId sharedMsgId fileDescr = do
  (fileId, aci) <- withStore $ \db -> do
    fileId <- getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
    aci <- getChatItemByFileId db vr user fileId
    pure (fileId, aci)
  case aci of
    AChatItem SCTGroup SMDRcv (GroupChat _g scopeInfo) ChatItem {chatDir} -> case (memberId, chatDir) of
      (Just memberId', CIGroupRcv m)
        | sameMemberId memberId' m -> do
            processFDMessage fileId aci fileDescr `catchAllErrors` \_ -> pure ()
            pure $ Just $ infoToDeliveryScope g scopeInfo
        | otherwise -> messageError "x.msg.file.descr: file of another member" $> Nothing
      (Nothing, CIChannelRcv) -> do
        processFDMessage fileId aci fileDescr `catchAllErrors` \_ -> pure ()
        pure $ Just $ infoToDeliveryScope g scopeInfo
      _ -> messageError "x.msg.file.descr: invalid file description part" $> Nothing
    _ -> messageError "x.msg.file.descr: invalid file description part" $> Nothing
```

**Note:** Keep the comment about `processFDMessage` inline in both branches (small duplication is acceptable per type-driven development).

**`channelMessageFileDescription` becomes:**
```haskell
channelMessageFileDescription gInfo sharedMsgId fileDescr =
  groupMessageFileDescription gInfo Nothing sharedMsgId fileDescr
```

### Change 3: Inline `xFileCancelGroupOrChannel` into `xFileCancelGroup`

**Before:**
```haskell
xFileCancelGroup g GroupMember {memberId} sharedMsgId =
  xFileCancelGroupOrChannel g (Just memberId) sharedMsgId

xFileCancelGroupOrChannel :: GroupInfo -> Maybe MemberId -> SharedMsgId -> ...
```

**After:**
```haskell
xFileCancelGroup :: GroupInfo -> Maybe MemberId -> SharedMsgId -> CM (Maybe DeliveryJobScope)
xFileCancelGroup g@GroupInfo {groupId} memberId sharedMsgId = do
  (fileId, aci) <- withStore $ \db -> do
    fileId <- getGroupFileIdBySharedMsgId db userId groupId sharedMsgId
    (fileId,) <$> getChatItemByFileId db vr user fileId
  case aci of
    AChatItem SCTGroup SMDRcv (GroupChat _g scopeInfo) ChatItem {chatDir} -> case (memberId, chatDir) of
      (Just memberId', CIGroupRcv m)
        | sameMemberId memberId' m -> do
            ft <- withStore $ \db -> getRcvFileTransfer db user fileId
            unless (rcvFileCompleteOrCancelled ft) $ do
              cancelRcvFileTransfer user ft
              toView $ CEvtRcvFileSndCancelled user aci ft
            pure $ Just $ infoToDeliveryScope g scopeInfo
        -- shouldn't happen now that query includes group member id
        | otherwise -> messageError "x.file.cancel: group member attempted to cancel file of another member" $> Nothing
      (Nothing, CIChannelRcv) -> do
        ft <- withStore $ \db -> getRcvFileTransfer db user fileId
        unless (rcvFileCompleteOrCancelled ft) $ do
          cancelRcvFileTransfer user ft
          toView $ CEvtRcvFileSndCancelled user aci ft
        pure $ Just $ infoToDeliveryScope g Nothing
      _ -> messageError "x.file.cancel: group member attempted invalid file cancel" $> Nothing
    _ -> messageError "x.file.cancel: group member attempted invalid file cancel" $> Nothing
```

**`xFileCancelChannel` becomes:**
```haskell
xFileCancelChannel gInfo sharedMsgId =
  xFileCancelGroup gInfo Nothing sharedMsgId
```

### Change 4: Update Call Sites

Update dispatch in `processAgentMessageConn`:
```haskell
XMsgFileDescr sharedMsgId fileDescr
  | isChannelOwner -> groupMessageFileDescription gInfo' Nothing sharedMsgId fileDescr
  | otherwise -> groupMessageFileDescription gInfo' (Just $ memberId' m'') sharedMsgId fileDescr
XMsgReact sharedMsgId memberId scope_ reaction add ->
  groupMsgReaction gInfo' m'' sharedMsgId memberId scope_ reaction add msg brokerTs
XFileCancel sharedMsgId
  | isChannelOwner -> xFileCancelGroup gInfo' Nothing sharedMsgId
  | otherwise -> xFileCancelGroup gInfo' (Just $ memberId' m'') sharedMsgId
```

Update forwarded message handling similarly.

---

## Implementation Order

1. **Change 1:** Inline `groupOrChannelReaction` into `groupMsgReaction`
   - Delete `groupOrChannelReaction`
   - Update `groupMsgReaction` signature and implementation
   - Update all call sites
   - Build and test

2. **Change 2:** Inline `groupOrChannelFileDescription` into `groupMessageFileDescription`
   - Delete `groupOrChannelFileDescription`
   - Update `groupMessageFileDescription` signature and implementation
   - Update call sites (including `channelMessageFileDescription`)
   - Build and test

3. **Change 3:** Inline `xFileCancelGroupOrChannel` into `xFileCancelGroup`
   - Delete `xFileCancelGroupOrChannel`
   - Update `xFileCancelGroup` signature and implementation
   - Update call sites (including `xFileCancelChannel`)
   - Build and test

4. **Final review:** Check for any remaining unnecessary renames or movements

---

## Expected Diff Reduction

Current diff: ~518 lines (235 additions, 61 deletions)

After changes:
- Removing 3 wrapper functions and their intermediate helpers: ~-40 lines of additions
- Eliminating variable renames: ~-20 lines of changes
- Keeping code inline instead of extracting to `where`: ~-15 lines

Expected final diff: ~450-470 lines with clearer structure showing only the genuinely new channel functionality.

The key improvement is **conceptual clarity**: readers will see `groupMsgReaction` extended to handle `Maybe MemberId` rather than a new `groupOrChannelReaction` function. The existing function name communicates that channel reactions are a special case of group reactions.
