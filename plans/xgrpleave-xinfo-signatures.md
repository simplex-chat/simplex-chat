# Plan: XGrpLeave and XInfo Signatures for Channels

## Context

Channel subscribers currently cannot sign their messages. Admin-level events (XGrpDel, XGrpInfo, etc.) are signed and verified, but member-level events (XGrpLeave, XInfo) are not. This means:
- A compromised relay could forge "member left" or profile update events
- Moderators+ cannot verify that a subscriber truly left or changed their profile
- There is no cryptographic binding between a subscriber and their actions

Additionally, the relay doesn't announce new subscribers to moderators+, so moderators have no way to learn subscriber public keys for verification.

This change adds:
1. Subscriber key consistency (fix existing TODOs where keys are generated but discarded)
2. Channel introduction protocol: relay announces subscribers to moderators+ via XGrpMemNew
3. Signatures on XGrpLeave and XInfo with appropriate verification and item suppression

## Part 1: Store Subscriber Keys (fix TODOs)

### 1a. Subscriber side: use stored key instead of generating new one

**Files**: `Subscriber.hs:1111`, `Commands.hs:3424-3428`

**Problem**: `APIConnectPreparedGroup` (Commands.hs:2007-2010) already generates a key pair and stores it via `updateGroupMemberKeys`, populating `GroupKeys`. But later, at Subscriber.hs:1111 and Commands.hs:3426, NEW key pairs are generated — the pubKey is sent via `XMember` while the privKey is discarded. The key sent to the relay differs from the key stored in the subscriber's DB.

**Fix at Subscriber.hs:1111**:
```haskell
-- Before:
(memberPubKey, _memberPrivKey) <- atomically $ C.generateKeyPair g
-- TODO [member keys] store memberPrivKey ...
dm <- encodeConnInfo $ XMember profileToSend membershipMemId (MemberKey memberPubKey)

-- After:
let memberPubKey = case groupKeys gInfo of
      Just GroupKeys {memberPrivKey} -> C.publicKey memberPrivKey
      Nothing -> error "no group keys for channel member" -- shouldn't happen
dm <- encodeConnInfo $ XMember profileToSend membershipMemId (MemberKey memberPubKey)
```
Remove the `g <- asks random` line (no longer needed if only used here).

**Fix at Commands.hs:3424-3428**: Same pattern — read from `groupKeys gInfo` instead of generating.

**Fix at Commands.hs:2006-2014**: The `gInfo'` passed to `connectToRelay` at line 2014 was fetched at line 2006, BEFORE keys are stored at line 2010. So `groupKeys gInfo'` is `Nothing`. Fix: re-fetch `gInfo` from DB after storing keys (between lines 2010 and 2011) and use the refreshed `gInfo` for `connectToRelay` and subsequent code. This ensures `joinContact` at line 3424 receives a `gInfo` with `groupKeys` populated.

### 1b. Relay side: store subscriber's pubKey from XMember

**File**: `Subscriber.hs:1437-1448` (`memberJoinRequestViaRelay`)

**Problem**: Relay receives `joiningMemberKey` but discards it (TODO at line 1438).

**Fix**: Pass `joiningMemberKey` through `acceptGroupJoinRequestAsync` → `createJoiningMember` so the pubKey is stored atomically with the member record creation. This avoids a theoretical race between the separate UPDATE and the CON event arriving.

Changes:
- `createJoiningMember` (Groups.hs:1525): add `Maybe C.PublicKeyEd25519` parameter, include in INSERT
- `acceptGroupJoinRequestAsync` (Internal.hs:926): add `Maybe MemberKey` parameter, pass to `createJoiningMember`
- `memberJoinRequestViaRelay` (Subscriber.hs:1443): pass `Just joiningMemberKey`
- Other callers of `acceptGroupJoinRequestAsync` (line 1421) and `createJoiningMember` (line 978): pass `Nothing`

## Part 2: Channel Introduction Protocol Change

### 2a. Rename `introduceModerators` → `introduceInChannel`

**Files**:
- `Internal.hs:1142` — function definition
- `Subscriber.hs:856` — call site

Rename only. No logic change.

### 2b. Extend `introduceInChannel` to announce subscriber to moderators+

**File**: `Internal.hs:1142-1148`

After sending XGrpMemIntro about moderators TO subscriber (existing logic), also send XGrpMemNew about subscriber TO moderators+:

```haskell
introduceInChannel :: VersionRangeChat -> User -> GroupInfo -> GroupMember -> CM ()
introduceInChannel _ _ _ GroupMember {activeConn = Nothing} = throwChatError $ CEInternalError "member connection not active"
introduceInChannel vr user gInfo subscriber@GroupMember {activeConn = Just conn} = do
  modMs <- withStore' $ \db -> getGroupModerators db vr user gInfo
  -- Send XGrpMemIntro about each moderator TO subscriber (existing)
  let introEvents = map (memberIntroEvt gInfo) modMs
  forM_ (L.nonEmpty introEvents) $ \events' ->
    sendGroupMemberMessages user gInfo conn events'
  -- Send XGrpMemNew about subscriber TO moderators+ (new)
  when (isJust $ memberPubKey subscriber) $
    void $ sendGroupMessage' user gInfo modMs $ XGrpMemNew (memberInfo gInfo subscriber) Nothing
```

The `when (isJust $ memberPubKey subscriber)` guard ensures we only announce if the relay has the subscriber's key (from Part 1b).

### 2c. Handle duplicate XGrpMemNew gracefully in relay groups

**File**: `Subscriber.hs:2765`

```haskell
-- Before:
Right _ -> messageError "x.grp.mem.new error: member already exists" $> Nothing

-- After:
Right existingMember
  | useRelays' gInfo -> do
      void $ withStore $ \db -> updateAnnouncedMember db vr user existingMember memInfo
      pure Nothing
  | otherwise -> messageError "x.grp.mem.new error: member already exists" $> Nothing
```

Where `updateAnnouncedMember` updates profile, role, version, and pubKey without changing status (since the member may already be at a higher status like GSMemConnected).

### 2d. Add member_pub_key to member update functions

**File**: `Groups.hs`

Add `member_pub_key` update to `updateIntroducedMember` (line 2854) and `updateUnknownMemberAnnounced` (line 2875). Both receive `MemberInfo` which carries `memberKey :: Maybe MemberKey`.

Also add new `updateAnnouncedMember` function for the duplicate XGrpMemNew case — updates profile, role, version, and pubKey, but preserves current status.

### 2e. Fix existing tests

Run all channel operation tests. The new XGrpMemNew messages to moderators may produce additional output that needs to be consumed in tests.

## Part 3: Signature Support

### 3a. New predicate `memberSignableEvent`

**File**: `Protocol.hs` (after `requiresSignature` at line 1202)

```haskell
memberSignableEvent :: CMEventTag e -> Bool
memberSignableEvent = \case
  XGrpLeave_ -> True
  XInfo_ -> True
  _ -> False
```

### 3b. Modify `groupMsgSigning`

**File**: `Internal.hs:1872-1876`

Change signing condition:
```haskell
-- Before:
| useRelays' gInfo && requiresSignature (toCMEventTag evt) =

-- After:
| useRelays' gInfo && shouldSign (toCMEventTag evt) =
  ...
  where shouldSign tag = requiresSignature tag || memberSignableEvent tag
```

### 3c. Modify `withVerifiedMsg` verification logic

**File**: `Subscriber.hs:3216-3226`

```haskell
-- Before:
verified = case signedMsg_ of
  Just SignedMsg {chatBinding, signatures, signedBody}
    | GroupMember {memberPubKey = Just pubKey, memberId} <- member ->
        -- verify signature (unchanged)
    | otherwise -> signatureOptional
  Nothing -> signatureOptional
signatureOptional = not (useRelays' gInfo && requiresSignature (toCMEventTag chatMsgEvent))

-- After:
verified = case signedMsg_ of
  Just SignedMsg {chatBinding, signatures, signedBody}
    | GroupMember {memberPubKey = Just pubKey, memberId} <- member ->
        -- verify signature (unchanged)
    | otherwise -> unknownMemberAccepted
  Nothing -> signatureOptional
  where
    tag = toCMEventTag chatMsgEvent
    signatureOptional = not (useRelays' gInfo && (requiresSignature tag || memberSignableEvent tag))
    unknownMemberAccepted = not (useRelays' gInfo && requiresSignature tag)
```

Verification matrix:

| Scenario | signatureOptional | unknownMemberAccepted | Result |
|---|---|---|---|
| Unsigned XGrpLeave in relay group | False | — | REJECT |
| Signed XGrpLeave, no pubKey | — | True | ACCEPT |
| Signed XGrpLeave, has pubKey | — | — | VERIFY |
| Unsigned XGrpDel in relay group | False | — | REJECT |
| Signed XGrpDel, no pubKey | — | False | REJECT |
| Signed XGrpDel, has pubKey | — | — | VERIFY |

### 3d. Suppress service items for unknown senders

**Parameterize `xInfoMember`** (Subscriber.hs:2397):
```haskell
-- Before:
xInfoMember gInfo m p' brokerTs = do
  void $ processMemberProfileUpdate gInfo m p' True (Just brokerTs)

-- After:
xInfoMember gInfo m p' createItems brokerTs = do
  void $ processMemberProfileUpdate gInfo m p' createItems (Just brokerTs)
```

Update call sites:
- Direct path (line 971): `xInfoMember gInfo' m'' p True brokerTs`
- Forwarded path (line 3190): `xInfoMember gInfo author p (isJust $ memberPubKey author) msgTs`

**Also guard `CEvtGroupMemberUpdated`** in `processMemberProfileUpdate` (Subscriber.hs:2479, 2487):
`processMemberProfileUpdate` emits `toView $ CEvtGroupMemberUpdated` independently of `createItems`. Wrap these with `when createItems` too, so the UI isn't notified about profile changes from unknown subscribers. The database profile update (`updateMemberProfile`) still happens regardless — only the view notification is suppressed.

**Parameterize `xGrpLeave`** (Subscriber.hs:3018):
```haskell
-- Before:
xGrpLeave gInfo m msg brokerTs = do
  deleteMemberConnection m
  gInfo' <- updateMemberRecordDeleted user gInfo m GSMemLeft
  (gInfo'', m', scopeInfo) <- mkGroupChatScope gInfo' m
  (ci, cInfo) <- saveRcvChatItemNoParse ...
  groupMsgToView cInfo ci
  toView $ CEvtLeftMember ...
  pure $ memberEventDeliveryScope m

-- After:
xGrpLeave gInfo m createItems msg brokerTs = do
  deleteMemberConnection m
  gInfo' <- updateMemberRecordDeleted user gInfo m GSMemLeft
  when createItems $ do
    (gInfo'', m', scopeInfo) <- mkGroupChatScope gInfo' m
    (ci, cInfo) <- saveRcvChatItemNoParse ...
    groupMsgToView cInfo ci
    toView $ CEvtLeftMember ...
  pure $ memberEventDeliveryScope m
```

Update call sites:
- Direct path (line 984): `xGrpLeave gInfo' m'' True msg brokerTs`
- Forwarded path (line 3195): `xGrpLeave gInfo author (isJust $ memberPubKey author) rcvMsg msgTs`

The `isJust (memberPubKey author)` check correctly maps to the user's requirement:
- Moderator receives XGrpLeave from subscriber → has pubKey (from XGrpMemNew) → items ✓
- Subscriber receives XGrpLeave from moderator → has pubKey (from XGrpMemIntro) → items ✓
- Subscriber receives XGrpLeave from subscriber → no pubKey → suppressed ✓

## Part 4: Tests

### Test: Owner leaves channel (signed)
- `testChannelOwnerLeavesSigned` — Owner sends `/leave #channel`, all see "(signed)"
- Verifies signature verification works for owner's XGrpLeave

### Test: Subscriber leaves channel (signed)
- `testChannelSubscriberLeavesSigned` — Subscriber leaves, moderators+ see service items, regular subscribers don't
- Verifies: (a) subscriber signs XGrpLeave, (b) moderators verify, (c) regular subscribers accept silently

### Test: Owner updates profile (signed)
- `testChannelOwnerProfileUpdateSigned` — Owner changes display name, verified by all

### Test: Subscriber updates profile (signed)
- `testChannelSubscriberProfileUpdateSigned` — Subscriber changes name, moderators+ see event, regular subscribers get silent profile update

## Implementation Order

1. **Part 1** (subscriber keys) — prerequisite for everything else
2. **Part 2** (protocol change + duplicate handling) — run existing tests, fix breakages
3. **Part 3** (signatures + verification + item suppression) — core feature
4. **Part 4** (tests) — validation
5. **Adversarial review** — 2 consecutive clean passes

## Risk Areas

- **Key consistency**: Subscriber.hs:1111 must use the SAME key that's stored in GroupKeys, not generate a new one. The `gInfo` at that point must have `groupKeys` populated (it should, since `APIConnectPreparedGroup` stores keys before connecting to relays).
- **Timing of introduceInChannel**: The relay must store the subscriber's pubKey (Part 1b) BEFORE calling `introduceInChannel` (Part 2b). The current flow: relay receives XMember → creates member → connects → calls `introduceInChannel`. Part 1b stores the key during member creation, so it's available when `introduceInChannel` runs.
- **Test output changes**: Adding XGrpMemNew to moderators will produce new output in existing tests that needs to be consumed.
- **xGrpLeave item suppression**: Must preserve `deleteMemberConnection` and `updateMemberRecordDeleted` side effects even when suppressing items.

## Files to Modify

| File | Changes |
|---|---|
| `src/Simplex/Chat/Protocol.hs` | Add `memberSignableEvent` predicate |
| `src/Simplex/Chat/Library/Subscriber.hs` | Fix key at :1111, store key at :1438, handle dup XGrpMemNew at :2765, parameterize xInfoMember/xGrpLeave, modify withVerifiedMsg |
| `src/Simplex/Chat/Library/Internal.hs` | Rename+extend introduceInChannel, modify groupMsgSigning |
| `src/Simplex/Chat/Library/Commands.hs` | Fix key at :3426 |
| `src/Simplex/Chat/Store/Groups.hs` | Add pubKey to update functions, add updateAnnouncedMember |
| `tests/ChatTests/Groups.hs` | Add 4 new tests, fix existing test output |
