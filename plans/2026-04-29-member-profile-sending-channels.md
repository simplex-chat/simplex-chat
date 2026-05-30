# Plan: Member Profile Sending in Channels

## Implementation note (2026-05-18)

The shipped implementation is **monotonic and reuses `member_relations_vector`**, not a new `sent_profile_vector` column:

- The introduction bit lives in `group_members.member_relations_vector` with status `MRIntroduced`. The M20251117 backfill already populates this column for channel rows (relay role is not admin/owner), and `createNewGroupMember` writes `Binary B.empty` for new members.
- Bits flip 0 → 1 when the relay first announces the member to a recipient via prepended `XGrpMemNew` (or via `XGrpMemIntro` in `introduceInChannel`'s join-time direct path). They are **never cleared**.
- Profile updates propagate via the sender's own signed `XInfo`, forwarded unchanged by the relay. The relay updates its DB on receipt; subscribers verify with the key obtained from the earlier `XGrpMemNew`. Section 5 below ("Clear vector on profile update") is superseded by this — no clearing happens.
- The mutually-exclusive two-column delivery-jobs storage (`single_sender_group_member_id` + `sender_group_member_ids`) collapses into a single nullable `sender_group_member_ids BYTEA` column: `[s]` for single-sender jobs, `[s1, s2, ...]` for multi-sender batches, NULL for sender-less jobs (`DJRelayRemoved`).

The plan body below is preserved for historical context.

## Context

In channels (relayed groups), subscribers don't know profiles of other subscribers. When subscriber A sends a reaction/message that gets forwarded to subscriber B, B creates an "unknown member" record with a synthesized name. This degrades UX — subscribers see "unknown member" instead of real profiles.

We can't eagerly send all subscriber profiles to all subscribers (doesn't scale to 100K+ channels). We need on-demand, deduplicated profile delivery: the relay tracks which subscribers have received which sender's profile, and prepends profile info when forwarding a message from a sender the recipient doesn't know.

## Approach: Vector-tracked profile delivery

### Core idea

Each member record on the relay stores a `sent_profile_vector BLOB` — a byte vector where position `i` represents the recipient at `index_in_group = i`. Value 0 = profile not sent, non-zero = sent.

When the relay forwards a batch (possibly from multiple senders):
1. Collect distinct senders in the batch. Load each sender's `sent_profile_vector`.
2. For each cursor-batch of recipients, partition into two groups:
   - **Knows all**: recipient's index is marked as sent in every sender's vector → gets bare batch
   - **Needs profiles**: recipient's index is unmarked in at least one sender's vector → gets batch with all sender profiles prepended as `XGrpMemNew` elements
3. Update all senders' vectors to mark recipients who were delivered to.

When a sender updates their profile (relay receives `XInfo`): clear that sender's `sent_profile_vector`, so the updated profile is re-sent on next forwarded message.

In steady state, most long-standing subscribers have received all active senders' profiles from previous deliveries. The "knows all" group dominates; the "needs profiles" group consists mainly of newcomers and is small. The partition converges quickly to near-zero redundancy.

### Why this approach

**Considered alternatives:**
- **Include profile in every FwdSender**: Wastes bandwidth sending profile on every message.
- **Subscriber requests profile from relay**: Adds latency (round-trip) and new request-response protocol complexity.
- **Separate delivery worker** (using commented-out `DWSMemberProfileUpdate` stubs): Harder to guarantee ordering (profile must arrive before message).
- **Bloom filters / epoch-based**: Same storage complexity as vectors, more complex to implement, probabilistic (false positives).

**Advantages of prepend-to-batch approach:**
- Profile + forwarded message arrive in a single SMP message (no extra 16KB block overhead)
- SMP guarantees in-order processing within a batch
- No protocol changes — `XGrpMemNew` is already handled by subscribers
- No subscriber-side code changes for receiving

### Design decisions to discuss

**1. Bit-level vs byte-level vector**

Byte-per-position is consistent with `member_relations_vector` but uses 8x more space. For 100K members: byte=100KB/sender, bit=12.5KB/sender. With 1000 active senders: byte=100MB, bit=12.5MB. Byte is simpler; bit is more space-efficient. **Recommend: byte-level for consistency, optimize to bit-level later if needed.**

**2. Multi-sender batch profile strategy**

Channels batch tasks from multiple senders into one job (`singleSenderGMId_ = Nothing`). Profile tracking requires knowing which senders' profiles each recipient has seen. Three approaches:

**Option A — Per-sender precise targeting (rejected)**: For a batch with senders {A, B, C}, construct a separate batch variant for each combination of missing profiles: recipients missing only A get `profile(A) + batch`, those missing A and C get `profile(A) + profile(C) + batch`, etc. This produces up to 2^k batch variants for k senders — a combinatorial explosion that is fundamentally at odds with batching efficiency. Constructing nearly per-recipient blobs is worse than not batching at all. **Rejected.**

**Option B — All-or-nothing profile sidecar (probably preferable)**: Partition recipients into two groups: those who know ALL senders (get bare batch) and those missing ANY sender profile (get all sender profiles prepended). Only 2 batch variants regardless of sender count. Preserves current multi-sender batching — no changes to `getNextDeliveryTasks`. Some recipients may receive profiles they already know, but XGrpMemNew is idempotent (~200-500 bytes per profile), and this redundancy only occurs at the rare intersection of a multi-sender batch AND a partially-informed recipient. In steady state, long-standing subscribers know all active senders, so the "needs profiles" group shrinks to just newcomers.
- Pros: preserves current batching, smaller diff (no `Store/Delivery.hs` changes), 2 variants only, fast convergence to zero-redundancy steady state
- Cons: slight redundancy for partially-informed recipients in multi-sender batches (rare and transient)

**Option C — Force single-sender jobs**: Add `sender_group_member_id` filter to `getNextDeliveryTasks` for channels, same as fully connected groups. Each delivery job has exactly one sender, so profile sidecar is always one XGrpMemNew. Clean binary partition with zero redundancy.
- Pros: zero redundant profiles, simplest per-job logic
- Cons: changes delivery task query logic, slightly less batching efficiency (separate jobs per sender), though multi-sender batches are rare anyway

---

## Detailed changes

The code below assumes Option B (all-or-nothing sidecar). Option C would simplify section 4 (always one sender) and add a query change in `Store/Delivery.hs`.

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
getSentProfileVector :: DB.Connection -> GroupMemberId -> IO ByteString

-- Expands vector if needed (same expand-on-write pattern as setRelation in Types/MemberRelations.hs)
markProfilesSentToMembers :: DB.Connection -> GroupMemberId -> [Int64] -> IO ()

clearSentProfileVector :: DB.Connection -> GroupMemberId -> IO ()
```

Pure helpers:
```haskell
isProfileSentTo :: ByteString -> Int64 -> Bool
isProfileSentTo vec idx
  | idx < 0 || fromIntegral idx >= B.length vec = False
  | otherwise = B.index vec (fromIntegral idx) /= 0

markSentPositions :: [Int64] -> ByteString -> ByteString
```

### 3. Profile batch element encoding

New functions in `src/Simplex/Chat/Messages/Batch.hs`:

```haskell
-- Prepend an element to an existing binary batch body
-- batchBody format: '=' <count:Word16> (<len:Word16> <element>)*
-- Increments count and inserts element at front without parsing/re-encoding existing elements
prependBatchElement :: ByteString -> ByteString -> ByteString

-- Encode XGrpMemNew as a batch-ready element for a given member
-- Constructs ChatMessage with XGrpMemNew (memberToMemberInfo member) Nothing
encodeMemberProfileElement :: VersionRangeChat -> GroupMember -> ByteString
```

Check whether `memberInfo` or similar helper already exists for constructing `MemberInfo` from `GroupMember`.

### 4. Delivery job worker changes

**File:** `src/Simplex/Chat/Library/Subscriber.hs` — `processDeliveryJob` / `sendBodyToMembers`

In the channel path (`useRelays' gInfo`, `DJSGroup {}`):

**Before the cursor loop**, collect distinct senders from delivery tasks and load their profile data:
```haskell
senderProfiles <- forM (nub senderGMIds) $ \senderGMId -> do
  sender <- withStore $ \db -> getGroupMemberById db vr user senderGMId
  vec <- withStore' $ \db -> getSentProfileVector db senderGMId
  pure (senderGMId, sender, vec)

let profileElements = map (\(_, sender, _) -> encodeMemberProfileElement vr sender) senderProfiles
    extBody = foldl' (flip prependBatchElement) body profileElements
```

**In the cursor loop**, partition recipients:
```haskell
sendLoop bucketSize cursorGMId_ = do
  mems <- withStore' $ \db -> getGroupMembersByCursor ...
  unless (null mems) $ do
    if null senderProfiles
      then deliver body mems
      else do
        let knowsAll m = all (\(_, _, vec) -> isProfileSentTo vec (indexInGroup' m)) senderProfiles
            (hasAllProfiles, needsProfiles) = partition knowsAll mems
        unless (null needsProfiles) $ deliver extBody needsProfiles
        unless (null hasAllProfiles) $ deliver body hasAllProfiles
        forM_ senderProfiles $ \(senderGMId, _, _) ->
          withStore' $ \db -> markProfilesSentToMembers db senderGMId
              (map indexInGroup' deliveredMems)
    ...
```

Only mark vector bits for members who were actually delivered to (those with `readyMemberConn`), not all members in the cursor batch — otherwise members without ready connections get marked as "profile sent" without receiving it.

### 5. Clear vector on profile update

**File:** `src/Simplex/Chat/Library/Subscriber.hs` — `xInfoMember`

After `processMemberProfileUpdate`, if the group uses relays and the user is the relay, clear the sender's vector:

```haskell
xInfoMember gInfo m p' msg brokerTs = do
  void $ processMemberProfileUpdate gInfo m p' (Just (msg, brokerTs))
  when (useRelays' gInfo && isRelay (membership gInfo)) $
    withStore' $ \db -> clearSentProfileVector db (groupMemberId' m)
  pure $ memberEventDeliveryScope m
```

When the vector is cleared and XInfo is forwarded, the delivery prepends XGrpMemNew before the forwarded XInfo. Recipients process both — XGrpMemNew creates/updates the member record, then XInfo updates it again. Slightly redundant but correct and harmless.

### 6. Set vector bits when relay announces members at join time

When a new subscriber joins and the relay sends `XGrpMemNew` for owners/existing announced members, set the corresponding bits in those members' `sent_profile_vector` for the new subscriber's index. The exact location needs to be identified during implementation — look for where the relay processes new member joins and sends XGrpMemNew announcements.

### 7. Update channel tests

**File:** `tests/ChatTests/Groups.hs`

Update `testChannels1RelayDeliver` and related tests:
- After cath sends a reaction, dan and eve should no longer see "forwarded a message from an unknown member, creating unknown member record cath"
- Instead, they receive cath's profile via XGrpMemNew (processed silently before the reaction)
- Test assertions for dan and eve should show the reaction with cath's name

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
| `src/Simplex/Chat/Store/Delivery.hs` | Only if Option C chosen (single-sender jobs) |
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
3. **Verification scenarios**:
   - New subscriber sends reaction → other subscribers receive profile + reaction (no "unknown member")
   - Subscriber updates profile → next message re-sends updated profile
   - New subscriber joins after sender was active → first forwarded message from that sender includes profile

## Known considerations

1. **Vector expansion**: A member with `index_in_group = 100000` causes vector expansion to 100KB. `markSentPositions` handles this via the same expand-on-write pattern as `setRelation` in `Types/MemberRelations.hs`.

2. **Delivery filtering**: Only mark vector bits for members who were actually delivered to (those with `readyMemberConn`). The `deliver` function filters for ready connections — if `markProfilesSentToMembers` marked all cursor members including those without connections, disconnected members would never receive the profile on reconnection.

3. **Scope**: Profile tracking applies only to `DJSGroup` scope. Support scope (`DJSMemberSupport`) delivers to moderators who already know members — no profile tracking needed there.

4. **Sender exclusion**: `getGroupMembersByCursor` already filters out the sender via `singleSenderGMId_` in the WHERE clause, so no self-profile issue arises.

5. **Race: vector clear vs delivery**: If profile update and message delivery overlap, the delivery sees an empty vector and sends the profile. This is correct — the delivery uses the current (updated) profile, so recipients get the new profile.
