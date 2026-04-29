# Plan: Member Profile Sending in Channels

## Context

In channels (relayed groups), subscribers don't know profiles of other subscribers. When subscriber A sends a reaction/message that gets forwarded to subscriber B, B creates an "unknown member" record with a synthesized name. This degrades UX — subscribers see "unknown member" instead of real profiles.

We can't eagerly send all subscriber profiles to all subscribers (doesn't scale to 100K+ channels). We need on-demand, deduplicated profile delivery: the relay tracks which subscribers have received which sender's profile, and prepends profile info when forwarding a message from a sender the recipient doesn't know.

## Approach: Vector-tracked profile delivery

### Core idea

Each member record on the relay stores a `sent_profile_vector BLOB` — a byte vector where position `i` represents the recipient at `index_in_group = i`. Value 0 = profile not sent, non-zero = sent.

When the relay forwards a message from sender X:
1. Load X's `sent_profile_vector`
2. For each batch of recipients, partition into `needsProfile` (bit=0) and `hasProfile` (bit!=0)
3. For `needsProfile` recipients: prepend `XGrpMemNew` (carrying X's full profile) as a direct batch element before the forwarded messages
4. For `hasProfile` recipients: send the forwarded messages only
5. Update X's vector to mark recipients who received the profile

When sender X updates their profile (relay receives `XInfo`): clear X's `sent_profile_vector`, so the updated profile is re-sent on next message.

### Why this approach

**Considered alternatives:**
- **Include profile in every FwdSender**: Violates requirement to not excessively send profile on each message. Wastes bandwidth.
- **Subscriber requests profile from relay**: Adds latency (round-trip) and new request-response protocol complexity.
- **Separate delivery worker** (using commented-out `DWSMemberProfileUpdate` stubs): Harder to guarantee ordering (profile must arrive before message).
- **Bloom filters / epoch-based**: Same storage complexity as vectors, more complex to implement, probabilistic (false positives).

**Advantages of prepend-to-batch approach:**
- Profile + forwarded message arrive in a single SMP message (no extra 16KB block overhead)
- SMP guarantees in-order processing within a batch
- No protocol changes — `XGrpMemNew` is already handled by subscribers
- No subscriber-side code changes for receiving

### Alternatives to discuss with team

1. **Bit-level vs byte-level vector**: Byte-per-position is consistent with `member_relations_vector` but uses 8x more space. For 100K members: byte=100KB/sender, bit=12.5KB/sender. With 1000 active senders: byte=100MB, bit=12.5MB. Byte is simpler; bit is more space-efficient. **Recommend: byte-level for consistency, optimize to bit-level later if needed.**

2. **Single-sender vs multi-sender delivery jobs**: Currently channels batch tasks from multiple senders into one job (`singleSenderGMId_ = Nothing`). Profile tracking requires knowing the sender. Two options:
   - Force single-sender jobs for channels (filter `getNextDeliveryTasks` by `sender_group_member_id`) — simpler
   - Parse sender list from batch body for multi-sender jobs — more complex, preserves batching efficiency
   **Recommend: single-sender jobs for channels.** Most channel traffic is single-sender (owner messages, individual reactions). Multi-sender batches are rare and the overhead of separate jobs is minimal.

---

## Detailed changes

### 1. Database migration

New migration file: `M{date}_sent_profile_vector.hs`

```sql
ALTER TABLE group_members ADD COLUMN sent_profile_vector BLOB;
```

**Files:**
- `src/Simplex/Chat/Store/SQLite/Migrations/M{date}_sent_profile_vector.hs` (new)
- `src/Simplex/Chat/Store/SQLite/Migrations.hs` (register migration)
- `src/Simplex/Chat/Store/Postgres/Migrations/M{date}_sent_profile_vector.hs` (new)
- `src/Simplex/Chat/Store/Postgres/Migrations.hs` (register migration)
- `simplex-chat.cabal` (add module)

### 2. Sent profile vector operations

New functions in `src/Simplex/Chat/Store/Groups.hs`:

```haskell
-- Read sender's sent_profile_vector
getSentProfileVector :: DB.Connection -> GroupMemberId -> IO ByteString

-- Set bytes at given indexes to 1 (profile sent)
-- Expands vector if needed (same pattern as setRelation)
markProfilesSentToMembers :: DB.Connection -> GroupMemberId -> [Int64] -> IO ()

-- Clear vector to empty (on profile update)
clearSentProfileVector :: DB.Connection -> GroupMemberId -> IO ()
```

Pure helper (could live in a new module or alongside the store functions):
```haskell
-- Check if profile has been sent to member at given index
isProfileSentTo :: ByteString -> Int64 -> Bool
isProfileSentTo vec idx
  | idx < 0 || fromIntegral idx >= B.length vec = False
  | otherwise = B.index vec (fromIntegral idx) /= 0

-- Set multiple positions to 1, expanding if needed
markSentPositions :: [Int64] -> ByteString -> ByteString
```

These follow the same expand-on-write pattern as `setRelation` in `Types/MemberRelations.hs`.

### 3. Profile batch element encoding

New function in `src/Simplex/Chat/Messages/Batch.hs`:

```haskell
-- Prepend an element to an existing binary batch body
-- batchBody format: '=' <count:Word16> (<len:Word16> <element>)*
prependBatchElement :: ByteString -> ByteString -> ByteString

-- Encode XGrpMemNew as a batch-ready element for a given member
encodeMemberProfileElement :: VersionRangeChat -> GroupMember -> ByteString
```

`prependBatchElement` efficiently modifies the binary batch by incrementing the count and inserting the new element at the front, without parsing/re-encoding existing elements.

`encodeMemberProfileElement` constructs a `ChatMessage` with `XGrpMemNew (memberToMemberInfo member) Nothing`, encodes it as JSON, and returns the raw bytes suitable for batch prepending.

Also need: a helper to construct `MemberInfo` from `GroupMember` for the relay. This may already exist — check `memberInfo` or similar functions.

### 4. Delivery job worker changes

**File:** `src/Simplex/Chat/Library/Subscriber.hs` — `processDeliveryJob` / `sendBodyToMembers`

In the channel path (`useRelays' gInfo`, `DJSGroup {}`):

**Before the cursor loop**, load sender profile data:
```haskell
-- Load sender info and profile vector (only for single-sender jobs)
(profileBody_, profileVec) <- case singleSenderGMId_ of
  Just senderGMId -> do
    sender <- withStore $ \db -> getGroupMemberById db vr user senderGMId
    vec <- withStore' $ \db -> getSentProfileVector db senderGMId
    let profileElement = encodeMemberProfileElement vr sender
        extBody = prependBatchElement profileElement body
    pure (Just (senderGMId, extBody), vec)
  Nothing -> pure (Nothing, B.empty)
```

**In the cursor loop**, partition recipients:
```haskell
sendLoop bucketSize cursorGMId_ = do
  mems <- withStore' $ \db -> getGroupMembersByCursor ...
  unless (null mems) $ do
    case profileBody_ of
      Just (senderGMId, extBody) -> do
        let (needsProfile, hasProfile) = partition
              (\m -> not $ isProfileSentTo profileVec (indexInGroup' m))
              mems
        unless (null needsProfile) $ deliver extBody needsProfile
        unless (null hasProfile) $ deliver body hasProfile
        -- Update vector
        withStore' $ \db -> markProfilesSentToMembers db senderGMId
            (map indexInGroup' mems)  -- mark ALL as sent (including hasProfile, which is idempotent)
      Nothing -> deliver body mems
    -- cursor update (unchanged)
    ...
```

### 5. Force single-sender delivery jobs for channels

**File:** `src/Simplex/Chat/Store/Delivery.hs` — `getNextDeliveryTasks`

For channels (`useRelays' gInfo`), add `sender_group_member_id = ?` filter to the query, same as fully connected groups. This ensures each delivery job has a known single sender.

The `singleSenderGMId_` computation in `processDeliveryTask` will then always be `Just` for channels.

### 6. Clear vector on profile update

**File:** `src/Simplex/Chat/Library/Subscriber.hs` — `xInfoMember`

After `processMemberProfileUpdate`, if the group uses relays and the relay is the user (membership is relay), clear the sender's vector:

```haskell
xInfoMember gInfo m p' msg brokerTs = do
  void $ processMemberProfileUpdate gInfo m p' (Just (msg, brokerTs))
  -- Clear sent profile vector on relay when subscriber updates profile
  when (useRelays' gInfo && isRelay (membership gInfo)) $
    withStore' $ \db -> clearSentProfileVector db (groupMemberId' m)
  pure $ memberEventDeliveryScope m
```

### 7. Set vector bits when relay announces members at join time

When a new subscriber joins and the relay sends `XGrpMemNew` for owners/existing announced members, the relay should also set the corresponding bits in those members' `sent_profile_vector` for the new subscriber's index.

This happens in the existing member announcement code path on the relay. The exact location needs to be identified during implementation — look for where the relay processes new member joins and sends XGrpMemNew announcements.

### 8. Update channel tests

**File:** `tests/ChatTests/Groups.hs`

Update `testChannels1RelayDeliver` and related tests:
- After cath sends a reaction, dan and eve should NO LONGER see "forwarded a message from an unknown member, creating unknown member record cath"
- Instead, they should receive cath's profile via XGrpMemNew (processed silently before the reaction)
- The test assertions for dan and eve should just show the reaction with cath's name

Add new tests:
- Profile update triggers re-announcement (clear vector → re-send on next message)
- New subscriber joining after a sender has been active gets the profile on first forwarded message
- Multiple senders: each sender's profile is independently tracked

---

## Files to modify

| File | Change |
|------|--------|
| `src/Simplex/Chat/Store/SQLite/Migrations/M{date}_sent_profile_vector.hs` | New migration |
| `src/Simplex/Chat/Store/SQLite/Migrations.hs` | Register migration |
| `src/Simplex/Chat/Store/Postgres/Migrations/M{date}_sent_profile_vector.hs` | New migration |
| `src/Simplex/Chat/Store/Postgres/Migrations.hs` | Register migration |
| `simplex-chat.cabal` | Add migration module |
| `src/Simplex/Chat/Store/Groups.hs` | Vector CRUD operations |
| `src/Simplex/Chat/Messages/Batch.hs` | `prependBatchElement`, `encodeMemberProfileElement` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Delivery job worker profile logic, xInfoMember vector clear |
| `src/Simplex/Chat/Store/Delivery.hs` | Single-sender jobs for channels |
| `tests/ChatTests/Groups.hs` | Update channel tests |

## Subscriber-side impact

**None required for receiving.** The subscriber already handles:
- `XGrpMemNew` from relay → creates member record with full profile
- `XGrpMsgForward` → finds existing member record
- Mixed batch elements (direct + forwarded) processed in order

The only subscriber-side change is the test expectations.

## Verification

1. **Build**: `cabal build --ghc-options=-O0`
2. **Run channel tests**: `cabal test simplex-chat-test --test-options='-m "channels"'`
3. **Manual verification scenarios**:
   - New subscriber sends reaction → other subscribers receive profile + reaction (no "unknown member")
   - Subscriber updates profile → next message re-sends updated profile
   - New subscriber joins after sender was active → first forwarded message from that sender includes profile

## Adversarial review

### Pass 1 findings (fixed in plan):

1. **Multi-sender batch issue**: Original plan didn't address multi-sender delivery jobs. Fixed by forcing single-sender jobs for channels (section 5).

2. **Profile not sent at join time**: When subscriber joins and relay announces owners, the owners' sent_profile_vector should be updated for the new subscriber. Added section 7.

3. **Race: vector clear vs delivery**: If profile update and message delivery overlap, the vector is cleared then the delivery sees empty vector → sends profile. This is correct: the delivery uses the current (updated) profile, so recipients get the new profile.

4. **Redundant XGrpMemNew on profile update**: When vector is cleared and XInfo is forwarded, the delivery prepends XGrpMemNew (profile) before the forwarded XInfo. Recipients process both (XGrpMemNew creates/updates record, then XInfo updates again). Slightly redundant but correct and harmless. Optimization deferred.

### Pass 2 findings:

5. **Vector expansion on high index_in_group**: If a member has `index_in_group = 100000`, the vector expands to 100KB. `markSentPositions` must handle expansion correctly — follow `setRelation` pattern. ✓ Already noted in section 2.

6. **Members without connections (readyMemberConn = Nothing)**: The `deliver` function filters for members with ready connections. But `markProfilesSentToMembers` marks ALL members in the batch. If a member's connection isn't ready, they don't receive the profile, but their bit is set. On next delivery, the bit is already set and the profile won't be resent. **Fix**: Only mark bits for members who were actually delivered to. Adjust to mark only members with `readyMemberConn`.

7. **Support scope (DJSMemberSupport)**: The support scope delivers to moderators + scope member. Should profile tracking apply there? Profile sending is most important for the main group scope. For MVP, profile tracking only for `DJSGroup` scope — support scope messages go to moderators who likely already know all members. ✓ The plan only modifies the `DJSGroup` path.

8. **`getGroupMembersByCursor` already filters out the sender** via `singleSenderGMId_` in the WHERE clause. So the sender themselves are never in the recipient list — no self-profile issue. ✓

### Pass 3: Clean — no new issues found.
### Pass 4: Clean — no new issues found.
