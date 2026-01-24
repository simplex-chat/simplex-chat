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

### 4. Signed Message Envelope (Protocol.hs)

```haskell
-- Key reference for signature verification
-- Can be extended to support profile identity keys (e.g., secp256k1 for Nostr)
data KeyRef = KRMember MemberId
  deriving (Eq, Show)

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
  -- Find owner in OwnerAuth chain
  case findOwnerAuth memberId (groupOwners gInfo) of
    Nothing -> Left "member is not an owner"
    Just OwnerAuth {ownerId, ownerKey} -> do
      -- Verify IDs match
      when (ownerId /= memberId) $
        Left "owner ID mismatch"
      -- Verify keys match (owner key in chain == member key in profile)
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

```haskell
-- Conversation binding for signature scope
data ConversationBinding
  = CBDirect SecurityCode       -- Direct chat: security code
  | CBGroup                     -- Group chat
      { groupRootKey :: C.PublicKeyEd25519,
        senderMemberId :: MemberId
      }
  deriving (Eq, Show)

-- Signature tuple
-- Wire format: keyRef ++ sigBytes (64 bytes)
-- Algorithm determined by keyRef: member lookup → key's algorithm, secp256k1 ref → Schnorr
data MsgSignature = MsgSignature
  { keyRef :: KeyRef,
    signature :: C.ASignature            -- Constructed from sigBytes + resolved algorithm
  }
  deriving (Eq, Show)

-- Signed message wrapper
data SignedChatMessage e = SignedChatMessage
  { scmMessage :: ChatMessage e,       -- Original message
    scmBinding :: ConversationBinding, -- Conversation binding
    scmSignatures :: [MsgSignature]    -- Signatures (may be empty)
  }
```

### 3. Wire Format (Protocol.hs)

The signed message format is **inside** the compressed envelope (after decompression). The compression layer (`'X' || compressed`) is unchanged.

Signature-related data is placed **before** the JSON body, avoiding the need to encode JSON length separately - JSON naturally extends to end of message.

```abnf
; After decompression, message body is one of:
chatMsgBody = unsignedMsg / signedMsg

; Current format (unchanged for unsigned messages)
unsignedMsg = jsonBody
jsonBody = *OCTET                        ; JSON bytes, extends to end

; New signed format - signature data before JSON
signedMsg = %s"S" binding signatures jsonBody

; Conversation binding (scopes signature to conversation)
binding = directBinding / groupBinding
directBinding = %s"D" securityCode
securityCode = 32*32 OCTET               ; 32 bytes
groupBinding = %s"G" groupRootKey senderMemberId
groupRootKey = 32*32 OCTET               ; Ed25519 public key, 32 bytes
senderMemberId = shortString             ; length-prefixed

; Signatures array
signatures = sigCount *signature
sigCount = 1*1 OCTET                     ; 0-255 signatures

; Signature with key reference
signature = keyRef sigBytes
sigBytes = 64*64 OCTET                   ; 64 bytes (Ed25519 or Schnorr)
  ; Note: Algorithm determined by key ref - member key lookup or secp256k1 implies Schnorr

; Key reference types
keyRef = memberKeyRef / secp256k1KeyRef
memberKeyRef = %s"M" memberId
memberId = shortString
; secp256k1KeyRef reserved for future profile identity key support (out of scope for this RFC)
secp256k1KeyRef = %s"S" secp256k1PubKey
secp256k1PubKey = 32*32 OCTET            ; x-only pubkey, 32 bytes

shortString = length *OCTET              ; length-prefixed bytearray 0-255 bytes
length = 1*1 OCTET
```

**Decoding logic:**
- Messages starting with `'S'` are signed
- Messages starting with `'{'` or `'['` are unsigned JSON
- `'X'` prefix for compressed batches is handled at higher layer (before this parsing)

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
5. Implement `SignedChatMessage` type with Encoding instance
6. Add `KeyRef`, `ConversationBinding`, `MsgSignature` types
7. Implement wire format encoding/decoding with 'S' prefix detection

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
