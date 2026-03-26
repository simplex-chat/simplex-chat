# Relay Key in Fixed Link Data — Research Notes

## Problem

Relay generates a key pair for a given group and distributes the public key via separate ad-hoc protocol messages:
- To owner: `XGrpRelayAcpt relayLink memberKey` (Subscriber.hs:738-746)
- To subscriber: `XGrpLinkMem profile (Just memberKey)` (Subscriber.hs:870-875)

Both `MemberKey` in `XGrpRelayAcpt` and `Maybe MemberKey` in `XGrpLinkMem` were added in the channels branch (confirmed via diff against stable).

Since relay sends key via separate messages to separate parties, it can send different keys to different parties — enabling manipulation of which parties accept/reject relay-signed messages.

## Solution

Put relay key and memberId into EXISTING fields of `FixedLinkData` (immutable part of relay link's short link data):
- `FixedLinkData.rootKey` = relay's public key for the group (Ed25519)
- `FixedLinkData.linkEntityId` = relay's MemberId (as ByteString)
- Relay profile goes in mutable part (`UserContactData` → `UserLinkData`), signed by rootKey

No new fields needed in FixedLinkData. Existing structure suffices.

## Key Type Definitions (at simplex-chat's dependent commit 782cacfb)

### FixedLinkData (simplexmq — Protocol.hs:1731-1735)
```haskell
data FixedLinkData c = FixedLinkData
  { agentVRange :: VersionRangeSMPA,
    rootKey :: C.PublicKeyEd25519,
    linkConnReq :: ConnectionRequestUri c,
    linkEntityId :: Maybe ByteString
  }
```

NOTE: Local simplexmq at /home/builder/code/simplexmq is OLDER than the dependent commit. The dependent commit (782cacfb) has `linkConnReq` (renamed from `connReq`) and `linkEntityId` (new field). Always use `git show 782cacfb:path` to see correct version.

### Encoding instance (forward-compatible)
```haskell
smpEncode FixedLinkData {agentVRange, rootKey, linkConnReq, linkEntityId} =
  smpEncode (agentVRange, rootKey, linkConnReq) <> maybe "" smpEncode linkEntityId
smpP = do
  (agentVRange, rootKey, linkConnReq) <- smpP
  linkEntityId <- optional smpP <* A.takeByteString -- ignoring tail for forward compatibility
  pure FixedLinkData {agentVRange, rootKey, linkConnReq, linkEntityId}
```

### encodeSignLinkData (ShortLink.hs:55-57 at 782cacfb)
```haskell
encodeSignLinkData :: forall c. ConnectionModeI c => C.KeyPairEd25519 -> VersionRangeSMPA -> ConnectionRequestUri c -> Maybe ByteString -> UserConnLinkData c -> (LinkKey, (ByteString, ByteString))
encodeSignLinkData keys@(_, pk) agentVRange linkConnReq linkEntityId userData =
  let (linkKey, fd) = encodeSignFixedData keys agentVRange linkConnReq linkEntityId
```

### encodeSignFixedData (ShortLink.hs:61-63 at 782cacfb)
```haskell
encodeSignFixedData :: ConnectionModeI c => C.KeyPairEd25519 -> VersionRangeSMPA -> ConnectionRequestUri c -> Maybe ByteString -> (LinkKey, ByteString)
encodeSignFixedData (rootKey, pk) agentVRange linkConnReq linkEntityId =
  let fd = smpEncode FixedLinkData {agentVRange, rootKey, linkConnReq, linkEntityId}
```

### newRcvConnSrv (Agent.hs at 782cacfb)
Line 1175: `let (linkKey, linkData) = SL.encodeSignLinkData sigKeys smpAgentVRange connReq Nothing userLinkData`
The `Nothing` is `linkEntityId` — always Nothing currently in newRcvConnSrv.

Line 1134: `newRcvConnSrv :: forall c. ConnectionModeI c => AgentClient -> NetworkRequestMode -> UserId -> ConnId -> Bool -> SConnectionMode c -> Maybe (UserConnLinkData c) -> Maybe CRClientData -> CR.InitialKeys -> SubscriptionMode -> SMPServerWithAuth -> AM (CreatedConnLink c, Maybe ClientServiceId)`

### LDATA event (Agent Protocol at 782cacfb)
```haskell
LDATA :: FixedLinkData 'CMContact -> ConnLinkData 'CMContact -> AEvent AEConn
```

### MemberKey type (Types.hs:895)
```haskell
newtype MemberKey = MemberKey C.PublicKeyEd25519
```

### GroupRelayInvitation (Types.hs:848-854)
```haskell
data GroupRelayInvitation = GroupRelayInvitation
  { fromMember :: MemberIdRole,
    fromMemberProfile :: Profile,
    relayMemberId :: MemberId,
    groupLink :: ShortLinkContact
  }
```

### UserContactData (simplexmq Protocol.hs at 782cacfb)
```haskell
data UserContactData = UserContactData
  { direct :: Bool,
    owners :: [OwnerAuth],
    relays :: [ConnShortLink 'CMContact],
    userData :: UserLinkData
  }
```

## Current Relay Key Flow (to be changed)

### Relay creates link (Subscriber.hs:3609-3646)
```
getLinkDataCreateRelayLink:
  1. Fetch owner's group link data
  2. Generate SEPARATE key pair: (_, memberPrivKey) <- C.generateKeyPair  (line 3617)
  3. Store keys via updateRelayGroupKeys
  4. Create relay link via createConnection (internally generates DIFFERENT rootKey)
  5. Send MemberKey (from step 2) to owner via XGrpRelayAcpt
```
Problem: Two different key pairs — memberPrivKey != relay link's rootKey.

### Owner accepts relay (Subscriber.hs:738-747)
```
XGrpRelayAcpt relayLink memberKey:
  1. Get group relay record
  2. Update member status to GSMemAccepted
  3. Store relayLink and memberKey via setRelayLinkAccepted
  4. allowAgentConnectionAsync
```

### Subscriber connects to relay (Commands.hs:2068-2083)
```
connectToRelay:
  1. Create dummy relay member: getCreateRelayForMember (random profile, random memberId)
  2. Fetch relay link data: getShortLinkConnReq (ignores _cData)
  3. Connect via contact
```

### Relay sends key to subscriber (Subscriber.hs:870-875)
```
sendXGrpLinkMem:
  memberKey = MemberKey <$> memberPubKey membership'
  sendDirectMemberMessage conn (XGrpLinkMem profileToSend memberKey)
```

### Subscriber receives key (Subscriber.hs:2407-2416)
```
xGrpLinkMem:
  1. Process profile update
  2. setXGrpLinkMemReceived db groupMemberId True memberKey_
```

## Plans (finalized)

See separate plan files:
- `simplexmq-relay-key.md` — Thread `linkEntityId` through createConnection, add `Maybe ConnId` to `getConnShortLinkAsync`
- `simplex-chat-relay-key.md` — 21 changes across 7 files

### Key decisions from planning:
- Owner fetches relay link data ASYNCHRONOUSLY — CONF is one-shot, sync network call failure = permanently dead connection
- Owner reuses existing relay connection for async fetch — `getConnShortLinkAsync` accepts `Maybe ConnId`, LDATA fires on same connection
- `CFGetShortLink` renamed to `CFGetRelayLinkOnJoin` (subscriber), new `CFGetRelayLinkOnAccept` (owner)
- `confId` + `relayLink` stored in `group_relays` for async continuation (LDATA fires on same connection)
- `getAgentConnShortLinkAsync` parameterized with `CommandFunction` and optional `Connection`
- `getCreateRelayForMember` keeps placeholder creation for retry support
- Member updated with real data inside tryAllErrors (sync) and LDATA handler (retry)
- `createRelayLink` receives signing keys from `createConnection` return (no external key generation)
- `linkEntityId :: Maybe ByteString` passed to `createConnection`, signing keys returned — follows `prepareConnectionLink` pattern

## Key DB Functions

- `setRelayLinkAccepted` (Groups.hs:1420-1439): Updates group_relays and group_members with relay_link and member_pub_key
- `setXGrpLinkMemReceived` (Groups.hs:2849-2863): Updates group_members with member_pub_key
- `getCreateRelayForMember` (Groups.hs:1352-1383): Creates dummy relay member with random profile/memberId
- `updateRelayGroupKeys` (Groups.hs): Stores relay group keys including memberPrivKey

## Important Verification Points

- `XGrpLinkMem`'s `Maybe MemberKey` is ONLY used for relay groups (non-relay groups have memberPubKey = Nothing) — safe to remove
- `XMember` carries subscriber's own key TO relay (opposite direction) — NOT affected by this change
- No backward compatibility concerns — channels/relays is unreleased
- Async handling is non-issue — allowAgentConnectionAsync already async, agent handles retries

## File Locations
- Subscriber.hs: src/Simplex/Chat/Library/Subscriber.hs
- Commands.hs: src/Simplex/Chat/Library/Commands.hs
- Internal.hs: src/Simplex/Chat/Library/Internal.hs
- Protocol.hs: src/Simplex/Chat/Protocol.hs
- Types.hs: src/Simplex/Chat/Types.hs
- Groups.hs: src/Simplex/Chat/Store/Groups.hs
- simplexmq Agent.hs: /home/builder/code/simplexmq/src/Simplex/Messaging/Agent.hs
- simplexmq Protocol.hs: /home/builder/code/simplexmq/src/Simplex/Messaging/Agent/Protocol.hs
- simplexmq ShortLink.hs: /home/builder/code/simplexmq/src/Simplex/Messaging/Crypto/ShortLink.hs
