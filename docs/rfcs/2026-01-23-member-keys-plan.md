# Implementation Plan: Member Keys and Signatures for Simplex Chat

## Overview

Add cryptographic signatures to Simplex Chat messages to prevent relay impersonation and roster manipulation in public groups with chat relays.

## Design Approach

Following **RFC Option 2: Multi-stage encoding** (recommended in docs/rfcs/2025-04-14-signing-messages.md):
- Encoded JSON body (non-deterministic key ordering OK)
- Conversation binding (group root key + sender member ID for groups)
- Array of (key reference, signature) tuples

## Key Files to Modify

### Core Types
- `src/Simplex/Chat/Types.hs` - Add `MemberKey` type, add `memberKey` to `MemberInfo`
- `src/Simplex/Chat/Protocol.hs` - Add member keys to `XMember`, `XGrpLinkMem`; signed message envelope, encoding/decoding

### Protocol Handling
- `src/Simplex/Chat/Library/Commands.hs` - Sign messages when sending
- `src/Simplex/Chat/Library/Subscriber.hs` - Verify signatures when receiving
- `src/Simplex/Chat/Library/Internal.hs` - Chat-level signature utilities (working with Member profiles, messages)

### Agent API (simplexmq repo) - New Functions
- `../simplexmq/src/Simplex/Messaging/Agent.hs`:
  - `prepareConnectionWithLink` - NEW: commits to server, generates link address + root key locally (no network)
  - `createConnectionWithLink` - NEW: accepts server + root key, creates queue (single network call)
- `../simplexmq/src/Simplex/Messaging/Agent/Client.hs` - Implement new functions

### Database
- New migration: `src/Simplex/Chat/Store/SQLite/Migrations/M20260124_member_keys.hs`
- New migration: `src/Simplex/Chat/Store/Postgres/Migrations/M20260124_member_keys.hs`
- `src/Simplex/Chat/Store/Profiles.hs` - Store/retrieve member keys

## New Types

### 1. Member Key Type (Types.hs)

```haskell
newtype MemberKey = MemberKey C.PublicKeyEd25519
  deriving (Eq, Show)

-- IMPORTANT: memberKey is NOT in Profile - profiles can be updated independently
-- Member keys are fixed at join time and sent via member announcement messages

-- Add memberKey to MemberInfo (used in XGrpMemNew, XGrpMemIntro, XGrpMemFwd)
data MemberInfo = MemberInfo
  { memberId :: MemberId,
    memberRole :: GroupMemberRole,
    v :: Maybe ChatVersionRange,
    profile :: Profile,
    memberKey :: Maybe MemberKey          -- NEW: member's signing key
  }
  deriving (Eq, Show)
```

### 2. Protocol Messages with Member Keys (Protocol.hs)

Member keys are communicated via member identification/announcement messages, NOT profile updates:

```haskell
-- Member self-identification when joining group
-- newMemberKey is required (not Maybe) - every new member must have a key
XMember :: {profile :: Profile, newMemberId :: MemberId, newMemberKey :: MemberKey} -> ChatMsgEvent 'Json

-- Member joining via group link
XGrpLinkMem :: Profile -> Maybe MemberKey -> ChatMsgEvent 'Json

-- Member announcements use MemberInfo which now includes memberKey
-- XGrpMemNew, XGrpMemIntro, XGrpMemFwd all use MemberInfo

-- Profile updates do NOT include memberKey - key is fixed at join time
XGrpMemInfo :: MemberId -> Profile -> ChatMsgEvent 'Json  -- unchanged
```

**Key points:**
- `XMember.newMemberKey` is required (not Maybe) - joining member must provide key
- `XGrpLinkMem` has `Maybe MemberKey` for backward compatibility
- `MemberInfo.memberKey` is `Maybe` for backward compatibility with existing members
- Profile updates (`XGrpMemInfo`) don't include key - it's fixed at join time

### 3. Member Key Storage

- Private key stored in `groups.member_priv_key` (current user's signing key for this group)
- Public key stored in `group_members.member_pub_key` (for all members)
- NOT stored in profiles table - member keys are per-group, not per-profile

### 5. Signed Message Types (Protocol.hs)

Types as implemented in Protocol.hs:

```haskell
-- Key reference for signature verification
-- Can be extended to support profile identity keys (e.g., secp256k1 for Nostr)
data KeyRef = KRMember MemberId
  deriving (Eq, Show)

-- Conversation binding for signature scope
data ChatBinding
  = CBDirect {securityCode :: ByteString}
  | CBGroup {groupRootKey :: C.PublicKeyEd25519, senderMemberId :: MemberId}
  deriving (Eq, Show)

-- Signature with key reference
data MsgSignature = MsgSignature KeyRef C.ASignature
  deriving (Show)

-- Signatures with the signed bytes preserved
data MsgSignatures = MsgSignatures
  { chatBinding :: ChatBinding,
    signatures :: NonEmpty MsgSignature
  }

-- Container for parsed messages with optional signatures
data SignedChatMessages = SignedChatMessages
  { signatures :: Maybe MsgSignatures,
    messages :: [AChatMessage]
  }
  deriving (Show)
```

**Key insight:** `MsgSignatures.signed` field preserves the exact bytes that were signed, enabling signature verification even after the message has been parsed. This is critical for forwarded messages.

### 6. Key Resolution and Validation

```haskell
-- Key resolution: lookup member's public key from GroupMember record
resolveKeyRef :: GroupInfo -> KeyRef -> Either String C.APublicVerifyKey
resolveKeyRef gInfo (KRMember mid) =
  case findMemberByMemberId mid gInfo >>= memberKey of
    Just (MemberKey k) -> Right $ C.APublicVerifyKey C.SEd25519 k
    Nothing -> Left $ "unknown member key: " <> show mid

-- findMemberByMemberId looks up GroupMember by MemberId in GroupInfo
-- memberKey is stored in GroupMember record (from group_members.member_pub_key)

-- Owner validation: verify member's key matches OwnerAuth chain
-- Called when processing roster-modifying messages from owners
validateOwnerMember :: GroupInfo -> MemberId -> MemberKey -> Either String ()
validateOwnerMember gInfo memberId memberKey = do
  case findOwnerAuth memberId (groupOwners gInfo) of
    Nothing -> Left "member is not an owner"
    Just OwnerAuth {ownerId, ownerKey} -> do
      when (ownerId /= memberId) $
        Left "owner ID mismatch"
      case memberKey of
        MemberKey k | k == ownerKey -> Right ()
        _ -> Left "owner key doesn't match member key"
```

### Owner Verification Strategy (future multi-owner support)

**Question:** How to validate that a member is a legitimate owner?

**Option A: Request link data from server**
- Fetch current `UserContactData.owners` from SMP server
- Expensive: network roundtrip for each verification

**Option B: Store OwnerAuth chain locally, verify via signatures** ✓
- When joining group: receive OwnerAuth chain (from link data or group info)
- When new owner added: receive signed OwnerAuth (signed by root or existing owner)
- Verify locally using signature chain - no network needed
- Store chain in `group_owners` table

**Current implementation (single owner):**
- Group creator is sole owner
- OwnerAuth created at group creation, stored in link data
- Members receive owner info when joining
- No multi-owner support yet (deferred)

### 7. Message Batching Analysis

Analysis of current batching behavior (determines new format requirements):

**Q1: Can there be multiple compressed parts in one wire message?**

**NO** - only ONE compressed block is ever created.
- `compressedBatchMsgBody_` (Protocol.hs:712) creates singleton list: `(L.:| []) . compress1`
- Called only in Internal.hs:1901 (connection info) and Internal.hs:1941 (message body)
- Decoder supports `NonEmpty Compressed` for forward compatibility, but encoding always produces 1 block

**Q2: Can messages from multiple members be batched together?**

**YES** - in both relay and non-relay groups:
- Relay groups: Delivery.hs:168-184 - `getNextDeliveryTasks` does NOT filter by sender
- Non-relay groups: `sendHistory` (Internal.hs:1171-1184) batches history items from multiple senders

**Q3: Can forwarded and non-forwarded messages be batched together?**

**YES** - in `sendHistory` (Internal.hs:1176-1184):
- `XMsgNew` (welcome/description) appended to `XGrpMsgForward` events
- Both sent together via `sendGroupMemberMessages`

### 8. Wire Format (Protocol.hs)

#### Current Format (JSON-based batching)

```abnf
; Current wire format
wireMessage = compressedMsg / jsonMsg
compressedMsg = %s"X" compressedBlock     ; single compressed block
jsonMsg = singleJson / jsonArray
singleJson = %s"{" *OCTET                 ; single JSON object
jsonArray = %s"[" *OCTET                  ; JSON array of messages
```

JSON array batching uses `[msg1,msg2,...]` format - simple but cannot preserve exact bytes for signatures.

#### New Format (Binary batching for signatures)

For relay-based groups where signatures are required, use binary batching that preserves exact message bytes:

```abnf
; Extended wire format (parser accepts all formats)
wireMessage = compressedMsg / binaryBatch / forwardEnvelope / jsonMsg

; New binary batch format - preserves exact bytes for signature verification
binaryBatch = %s"=" elementCount *batchElement
elementCount = 1*1 OCTET                  ; 1-255 elements
batchElement = elementLen elementBody
elementLen = 2*2 OCTET                    ; 16-bit big-endian length
elementBody = signedElement / forwardElement / unsignedElement

; Signed element - signature prefix before JSON
signedElement = %s"S" msgSignatures jsonBody
jsonBody = *OCTET                         ; JSON bytes (length from elementLen)

; Forward element - relay forwarding with preserved bytes (relay groups only)
forwardElement = %s"F" forwardMeta originalBytes
forwardMeta = senderMemberId senderMemberName brokerTs
brokerTs = 8*8 OCTET                      ; UTC timestamp, big-endian microseconds
originalBytes = *OCTET                    ; original message bytes (signed or unsigned)

; Unsigned element - plain JSON (backward compatibility)
unsignedElement = %s"{" *OCTET            ; JSON object

; Single forward envelope (not batched)
forwardEnvelope = %s"F" forwardMeta originalBytes

; Signature data
msgSignatures = chatBinding sigCount *msgSignature
chatBinding = directBinding / groupBinding
directBinding = %s"D" securityCode
securityCode = shortString
groupBinding = %s"G" groupRootKey senderMemberId
groupRootKey = 32*32 OCTET                ; Ed25519 public key
senderMemberId = shortString

sigCount = 1*1 OCTET                      ; 1-255 signatures
msgSignature = keyRef sigBytes
keyRef = memberKeyRef
memberKeyRef = %s"M" memberId
memberId = shortString
sigBytes = 64*64 OCTET                    ; Ed25519 signature

shortString = length *OCTET
length = 1*1 OCTET

; Compressed format unchanged - compression wraps the batch
compressedMsg = %s"X" compressedBlock
; After decompression: binaryBatch / jsonMsg
```

**Overhead comparison:**
- JSON array: `[` + `]` + `,` between = n+1 bytes for n elements
- Binary batch: `B` + count + 2-byte length per element = 1 + 1 + 2n = 2 + 2n bytes
- Difference: ~1 extra byte per element - acceptable for signature support

**Format selection:**
- Relay-based groups: Use binary batch (`B` prefix) - preserves bytes for signatures
- Non-relay groups: Use JSON array (`[...]`) - backward compatible, no signatures needed
- Old groups with old members: Use JSON array - full backward compatibility

**Parser behavior (`parseChatMessages`):**
- `'='` prefix → binary batch (new format)
- `'{'` prefix → single JSON object
- `'['` prefix → JSON array
- `'X'` prefix → compressed (decompress, then re-parse)
- All formats accepted regardless of version for forward/backward compatibility

**Batcher behavior (`Messages/Batch.hs`):**
- Accept `BatchMode` parameter: `BMJsonArray` or `BMBinaryBatch`
- `BMJsonArray`: Current JSON array encoding
- `BMBinaryBatch`: Binary format with length prefixes, preserves exact bytes

```haskell
data BatchMode = BMJsonArray | BMBinaryBatch

batchMessages :: BatchMode -> Int -> [Either ChatError SndMessage] -> [Either ChatError MsgBatch]
batchDeliveryTasks1 :: BatchMode -> VersionRangeChat -> Int -> NonEmpty MessageDeliveryTask -> (ByteString, [Int64], [Int64])
```

**Key insight:** The binary batch format allows:
1. Each element's exact bytes preserved (length-prefixed, not re-encoded)
2. Mixed signed/unsigned elements in same batch
3. Forwarded messages preserve original sender's signature
4. Relay adds no signature - just wraps in forwarding envelope

**Forwarding in binary batch (relay groups):**

For relay-based groups, forwarding is NOT via `XGrpMsgForward` ChatMsgEvent (which would re-encode the inner message). Instead, forwarding is a **top-level binary format** that preserves exact bytes:

```abnf
; Forwarding envelope - top level, NOT a ChatMsgEvent
forwardEnvelope = %s"F" forwardMeta originalBytes
forwardMeta = senderMemberId senderMemberName brokerTs
senderMemberId = shortString
senderMemberName = shortString            ; may be empty
brokerTs = 8*8 OCTET                      ; UTC timestamp, big-endian microseconds
originalBytes = *OCTET                    ; original signed message bytes (verbatim)
```

**Flow:**

1. **Sender** creates signed message:
   ```
   S<binding><sigs><{"event":"x.msg.new",...}>
   ```

2. **Relay** receives, parses to validate, stores original bytes in `msg_body`

3. **Relay** forwards using binary envelope:
   ```
   F<memberId><memberName><brokerTs><original-bytes-verbatim>
   ```
   Or batched:
   ```
   B<count><len1><F...><len2><F...>...
   ```

4. **Recipient** parses forward envelope, extracts `originalBytes`, verifies sender's signature

**Key difference from current approach:**
- Current: `XGrpMsgForward` nests **parsed** `ChatMessage 'Json` → re-encoded on send → bytes change
- New: Forward envelope contains **raw bytes** → never re-encoded → signature remains valid

**Backward compatibility:**
- Old groups (non-relay): Continue using `XGrpMsgForward` ChatMsgEvent (JSON array batching)
- New relay groups: Use binary forward envelope (`F` prefix)
- Parser accepts both formats

**Key resolution:**
- `'M'` (member key ref): Look up member's public key from `group_members.member_pub_key`

## Messages Requiring Signatures

### Owner/Admin Signatures (roster changes)
- `XGrpRelayInv` - Owner inviting relay (relay validates)
- `XGrpMemNew` - Adding new member
- `XGrpMemRole` - Changing member role
- `XGrpMemDel` - Removing member
- `XGrpInfo` - Updating group profile
- `XGrpPrefs` - Updating group preferences
- `XGrpDel` - Deleting group

### Member Signatures (future, for message authenticity)
- `XMsgNew` - Regular messages (optional, for proof of authorship)

## Database Migration

```sql
-- SQLite migration M20260124_member_keys.hs

-- Group-level keys (current user's keys for this group)
ALTER TABLE groups ADD COLUMN shared_group_id BLOB;            -- saved in link fixed data as entity ID
ALTER TABLE groups ADD COLUMN root_priv_key BLOB;              -- root private key (only if user is the owner and group creator)
ALTER TABLE groups ADD COLUMN root_pub_key BLOB;               -- needed for all members of public groups to verify ownership chains
ALTER TABLE groups ADD COLUMN member_priv_key BLOB;            -- current user's member private key for this group

-- Member public keys (for all members, including current user)
-- Public key is sent via MemberInfo/XMember and stored for signature verification
ALTER TABLE group_members ADD COLUMN member_pub_key BLOB;      -- public key (all members)

-- Note: root_priv_key is the root key from group link (immutable group identity), only for owner/creator
-- Note: root_pub_key is needed for all members of public groups to verify ownership chains
-- Note: member_priv_key is the current user's signing key for this group (unique per group)
-- Note: member_pub_key is received via MemberInfo (XGrpMemNew, etc.) or XMember message
```

## Root Key Management (Analysis Required)

Currently, root key is generated in Agent (`ShortLinkCreds.linkPrivSigKey`) and stored in agent schema (`rcv_queues.link_priv_sig_key`).

For Chat to sign owner messages, we need access to either:
- The root key (for initial owner)
- The owner key (for subsequent owners in chain)

**Current Problem: Two-Step Group Creation (2 roundtrips)**

Current flow in Commands.hs:
1. Chat creates connection → server roundtrip → gets link
2. Chat updates group profile to include link
3. Chat updates link data → another server roundtrip

Problems:
- Double requests increase latency
- Risk of failing halfway (needs recovery management)
- Can't include signed owner key in initial link data

**Solution: New Agent API with Prepare + Create Pattern**

Two new Agent functions:

```haskell
-- Prepared link data returned by prepare step (NO network, NO database)
-- Contains everything needed to: (a) construct the short link, (b) create the connection later
data PreparedConnLink c = PreparedConnLink
  { pclServer :: SMPServerWithAuth,            -- Committed server from config
    pclNonce :: C.CbNonce,                     -- Nonce (corrId) - determines sender ID
    pclRootKeyPair :: C.KeyPairEd25519,        -- Root signing key for link
    pclE2eKeyPair :: C.KeyPairX25519,          -- E2E DH key for queue address
    pclFixedLinkData :: FixedLinkData c,       -- Contains connReq (with ratchet params for invitations)
    pclLinkKey :: LinkKey,                     -- Derived from FixedLinkData: sha3_256(encoded fixedData)
    pclPrivSigKey :: C.PrivateKeyEd25519       -- For signing link data (same as snd of pclRootKeyPair)
  }

-- 1. prepareConnectionLink: Generates all link parameters locally (NO network, NO database)
-- Returns PreparedConnLink + the actual short link that can be used in addresses
prepareConnectionLink :: ConnectionModeI c
  => AgentClient -> UserId -> SConnectionMode c -> Maybe CRClientData -> CR.InitialKeys
  -> AM (PreparedConnLink c, ConnShortLink c)
-- Does:
--   - Selects server from config (getSMPServer)
--   - Generates nonce, derives sender ID: sha3_384(corrId)[:24]
--   - Generates root key pair (Ed25519) for signing
--   - Generates e2e DH key pair (X25519) for queue address
--   - For invitations: generates E2E ratchet params
--   - Builds ConnectionRequestUri (contains queue address + ratchet params)
--   - Builds FixedLinkData (contains connReq + rootKey + agentVRange)
--   - Derives linkKey = sha3_256(encoded fixedData)
--   - Constructs ConnShortLink (CSLContact or CSLInvitation) with linkKey
-- Returns (PreparedConnLink, ConnShortLink) - both can be roundtripped, nothing saved

-- 2. createConnectionWithPreparedLink: Creates connection using prepared link
-- Single network call to create queue with pre-determined sender ID
createConnectionWithPreparedLink :: ConnectionModeI c =>
  AgentClient -> NetworkRequestMode -> UserId -> Bool -> Bool ->
  PreparedConnLink c -> UserConnLinkData c -> SubscriptionMode ->
  AM (ConnId, (CreatedConnLink c, Maybe ClientServiceId))
-- Accepts:
--   - PreparedConnLink from prepare step (contains all crypto material)
--   - UserConnLinkData with signed OwnerAuth array (mutable part)
-- Does:
--   - Uses pclNonce to get deterministic sender ID
--   - Creates connection record (newConnNoQueues)
--   - Creates queue on server with prepared nonce → same sender ID
--   - Encrypts & uploads link data (fixed + user data)
-- Returns same as createConnection
```

**Key insights (from RFC 2025-03-16-smp-queues.md):**
- Sender ID = `sha3_384(nonce)[:24]` - derived locally from correlation ID (nonce)
- `FixedLinkData` contains `ConnectionRequestUri` (includes ratchet params for invitations)
- `LinkKey` = `sha3_256(encoded fixedData)` - derived from fixed data hash
- For **contact addresses**: `(link_id, enc_key) = HKDF(link_key, 56)` - fully deterministic
- For **1-time invitations**: `link_id` is server-generated, `enc_key = HKDF(link_key, 32)`
- Public groups use contact mode → short link address fully known at prepare step
- Everything can be roundtripped - no database needed for prepare step

**New Flow (single roundtrip):**

```haskell
-- In Chat (Commands.hs) when creating public group:
createPublicGroupWithRelays :: ... -> CM GroupInfo
createPublicGroupWithRelays ... = do
  -- 1. Prepare link parameters (NO network, NO database)
  -- Returns PreparedConnLink + the short link for use in group address
  (preparedLink@PreparedConnLink {pclRootKeyPair = (rootPubKey, rootPrivKey)}, shortLink) <-
    prepareConnectionLink c userId SCMContact clientData pqInitKeys

  -- 2. Generate owner's member key pair
  (memberPubKey, memberPrivKey) <- liftIO $ atomically $ C.generateKeyPair g

  -- 3. Create signed OwnerAuth (Chat signs with root key)
  let ownerAuth = OwnerAuth
        { ownerId = memberId,
          ownerKey = memberPubKey,
          authOwnerSig = C.sign' rootPrivKey (memberId <> C.encodePubKey memberPubKey)
        }

  -- 4. Create UserConnLinkData with owners array
  let userLinkData = UserContactLinkData $ UserContactData { owners = [ownerAuth], direct = True }

  -- 5. Create connection with prepared link (SINGLE network call)
  (connId, (createdLink, _)) <- createConnectionWithPreparedLink c NRMNormal userId
      enableNtfs checkNotices preparedLink userLinkData SMSubscribe

  -- 6. Store keys in groups table
  updateGroupKeys groupId rootPubKey rootPrivKey memberPrivKey
  -- groups.root_pub_key = rootPubKey (for all members of public groups)
  -- groups.root_priv_key = rootPrivKey (only for owner/creator)
  -- groups.member_priv_key = memberPrivKey (current user's signing key)
  -- group_members.member_pub_key = memberPubKey (for current user's membership)

  -- Note: shortLink can be used immediately in group profile/address
  -- The link address is determined at step 1, not step 5
```

**Key Points:**
- `prepareConnectionLink` generates all link parameters locally (no network, no DB)
- Returns `(PreparedConnLink, ConnShortLink)` - short link address is known immediately
- Sender ID is deterministic: `sha3_384(nonce)[:24]` - derived locally
- `FixedLinkData` contains `ConnectionRequestUri` (includes ratchet params for invitations)
- `LinkKey` derived from `FixedLinkData`, short link address derived from `LinkKey`
- Chat uses root key to sign owner's member key → OwnerAuth
- `createConnectionWithPreparedLink` makes single network roundtrip with complete link data
- `groups` table: `root_priv_key` (owner only), `root_pub_key` (all members), `member_priv_key` (current user)
- `group_members` table: `member_pub_key` (all members)

## Current Public Group Creation (to be refactored)

Review `src/Simplex/Chat/Library/Commands.hs` - current two-step process:
1. `APICreateGroup` / `createPreparedGroup` - creates group with connection
2. Server roundtrip to create link
3. Update profile with link
4. Update link data (another roundtrip)

This needs refactoring to use new Agent API for single-roundtrip creation.

## Implementation Steps

### Phase 0: Agent API Changes (simplexmq)
1. Add `prepareConnectionWithLink` function - commits to server, generates link + root key locally
2. Add `createConnectionWithLink` function - accepts server + root key, single network call
3. Update Agent store to handle new flow (connection record without queue record)

### Phase 1: Types and Encoding
1. Add `MemberKey` type and JSON encoding in Types.hs
2. Add `memberKey :: Maybe MemberKey` field to `MemberInfo` type
3. Add `newMemberKey :: MemberKey` to `XMember` message (required, not Maybe)
4. Add `Maybe MemberKey` parameter to `XGrpLinkMem` message
5. Types already added to Protocol.hs: `KeyRef`, `ChatBinding`, `MsgSignature`, `MsgSignatures`, `SignedChatMessages`
6. Implement binary batch encoding/decoding with '=' prefix
7. Update `parseChatMessages` to accept both JSON array and binary batch formats
8. Add `BatchMode` parameter to batching functions in Messages/Batch.hs

### Phase 2: Key Generation and Storage
1. Add database migration for `member_pub_key` in group_members, `member_priv_key` in groups
2. Generate Ed25519 key pair when joining/creating group
3. Store private key in groups.member_priv_key (current user's key for this group)
4. Store public key in group_members.member_pub_key (for all members)
5. Include public key in XMember/XGrpLinkMem/MemberInfo when sending

### Phase 3: Signing Messages
1. Add `signChatMessage` function in Internal.hs
2. Modify `sendGroupMessage` to sign roster-modifying messages
3. Add owner key to group link when creating public group
4. Sign `XGrpRelayInv` with owner key

### Phase 4: Signature Verification
1. Add `verifyChatMessage` function in Internal.hs
2. Modify message reception to verify signatures
3. Add relay validation for `XGrpRelayInv` in Subscriber.hs
4. Implement key resolution from member profiles

### Phase 5: Version Gating
1. Add new chat version (e.g., `memberSignaturesVersion = VersionChat 17`)
2. Gate signature features behind version check
3. Accept unsigned messages from older clients
4. Send signed messages only to clients supporting new version

## Signature Verification Logic

```haskell
verifyGroupMessage :: GroupInfo -> ChatMessage 'Json -> SignedChatMessage 'Json -> Either String ()
verifyGroupMessage gInfo msg SignedChatMessage {scmBinding, scmSignatures} = do
  -- 1. Validate binding matches group
  case scmBinding of
    CBGroup {groupRootKey, senderMemberId} -> do
      when (groupRootKey /= expectedRootKey gInfo) $
        Left "group root key mismatch"
      when (senderMemberId /= expectedMemberId) $
        Left "sender member ID mismatch"
    _ -> Left "wrong binding type for group"

  -- 2. Resolve keys and verify signatures
  forM_ scmSignatures $ \MsgSignature {keyRef, signature} -> do
    pubKey <- resolveKeyRef gInfo keyRef
    let signedData = encodeForSigning msg scmBinding
    unless (C.verify' pubKey signature signedData) $
      Left "signature verification failed"
```

## Owner Key Integration with Group Link (Separate Key Model)

When creating a public group:
1. Generate group root key (Ed25519 key pair) - stored in group link's immutable FixedLinkData
2. Generate owner's member key (Ed25519 key pair) - stored in groups.member_priv_key and group_members.member_pub_key
3. Create OwnerAuth entry: `OwnerAuth { ownerId = memberId, ownerKey = memberKey, authOwnerSig = sig(memberId || memberKey, rootKey) }`
4. Add OwnerAuth to group link's mutable UserContactData.owners list

This model:
- Root key is immutable (defines group identity)
- Owner key is in OwnerAuth chain (supports ownership transfer)
- Member keys are per-group, stored in groups/group_members tables (NOT in profiles)
- New owners can be added by existing owners signing their authorization

```haskell
-- When creating public group
createPublicGroup :: ... -> CM GroupInfo
createPublicGroup ... = do
  -- 1. Generate root key for group identity
  (rootPubKey, rootPrivKey) <- generateKeyPair Ed25519

  -- 2. Generate owner's member key for this group
  (memberPubKey, memberPrivKey) <- generateKeyPair Ed25519

  -- 3. Create owner authorization signed by root
  let ownerAuth = OwnerAuth
        { ownerId = memberId membership,
          ownerKey = memberPubKey,
          authOwnerSig = sign rootPrivKey (memberId <> encodePubKey memberPubKey)
        }

  -- 4. Store keys: root_priv_key and member_priv_key in groups table
  --    member_pub_key in group_members table
  -- 5. Add ownerAuth to link data
  ...
```

## Testing Considerations

1. **Unit tests**: Encoding/decoding round-trips for signed messages
2. **Integration tests**: Message signing and verification flow
3. **Compatibility tests**: Old clients receiving signed messages
4. **Relay tests**: Signature validation in relay invitation flow
5. **Key rotation tests**: Profile updates with new member key

## Backward Compatibility

- **Hard fail mode**: Messages requiring signatures (roster changes) MUST be signed. Unsigned/invalid = rejected.
- Version-gated: Add `memberSignaturesVersion = VersionChat 17`
- New clients: Send signed roster messages, reject unsigned roster messages from new clients
- Old clients: Cannot send roster messages to new-version groups (version negotiation prevents this)
- Migration path: Existing groups without signatures continue working; new public groups require signatures

## Design Decisions (Confirmed)

1. **Message signing scope**: Only roster-modifying messages (XGrpRelayInv, XGrpMemNew, XGrpMemRole, XGrpMemDel, XGrpInfo, XGrpPrefs, XGrpDel). Regular messages (XMsgNew) not signed initially.

2. **Signature failure handling**: Hard fail for all signed message types. Reject any message that should be signed but isn't or has invalid signature.

3. **Key model**: Separate keys - root key is fixed in group link, owner is authorized via OwnerAuth chain. Supports ownership transfer without breaking group identity. Matches simplexmq pattern.
