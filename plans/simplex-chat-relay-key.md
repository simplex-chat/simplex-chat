# simplex-chat: Relay key from FixedLinkData

## Goal

Remove relay key distribution via ad-hoc protocol messages (`XGrpRelayAcpt`, `XGrpLinkMem`). Instead, both owner and subscriber extract the relay's key, memberId, and profile from the relay link's `FixedLinkData` (immutable, signed) and mutable user data.

## Security model

**Before**: Relay sends key via separate messages to owner and subscriber — can send different keys, enabling selective message rejection.

**After**: Relay's key is `rootKey` in `FixedLinkData`, signed and immutable. Both parties extract the same key from the same tamper-proof source.

## Data mapping

| Relay data | Location | Field |
|---|---|---|
| Relay public key | FixedLinkData (immutable) | `rootKey` |
| Relay memberId | FixedLinkData (immutable) | `linkEntityId` |
| Relay profile | UserLinkData (mutable, signed by rootKey) | `RelayShortLinkData.relayProfile` |

## Changes by file

### Protocol.hs

**1. `XGrpRelayAcpt` — remove `MemberKey`**

```haskell
-- Before (line 438):
XGrpRelayAcpt :: ShortLinkContact -> MemberKey -> ChatMsgEvent 'Json

-- After:
XGrpRelayAcpt :: ShortLinkContact -> ChatMsgEvent 'Json
```

Update all 6 locations: constructor, event tag pattern, tag string, tag parse, JSON encode, JSON decode.

```haskell
-- Decode (line 1284): remove `<*> p "memberKey"`
XGrpRelayAcpt_ -> XGrpRelayAcpt <$> p "relayLink"

-- Encode (line 1351): remove `"memberKey" .= memberKey`
XGrpRelayAcpt relayLink -> o ["relayLink" .= relayLink]

-- Pattern (line 1132):
XGrpRelayAcpt _ -> XGrpRelayAcpt_
```

**2. `XGrpLinkMem` — remove `Maybe MemberKey`**

```haskell
-- Before (line 435):
XGrpLinkMem :: Profile -> Maybe MemberKey -> ChatMsgEvent 'Json

-- After:
XGrpLinkMem :: Profile -> ChatMsgEvent 'Json
```

Update all 6 locations similarly.

```haskell
-- Decode (line 1281): remove `<*> opt "memberKey"`
XGrpLinkMem_ -> XGrpLinkMem <$> p "profile"

-- Encode (line 1348): remove memberKey conditional
XGrpLinkMem profile -> o ["profile" .= profile]

-- Pattern (line 1129):
XGrpLinkMem _ -> XGrpLinkMem_
```

**3. New type `RelayShortLinkData`** (near `GroupShortLinkData`, line ~1430)

```haskell
data RelayShortLinkData = RelayShortLinkData
  { relayProfile :: Profile
  }

$(JQ.deriveJSON defaultJSON ''RelayShortLinkData)
```

### Internal.hs

**4. `acceptRelayJoinRequestAsync` (line 1035) — remove `memberKey`**

```haskell
-- Before:
acceptRelayJoinRequestAsync :: User -> Int64 -> GroupInfo -> GroupMember -> InvitationId -> VersionRangeChat -> ShortLinkContact -> MemberKey -> CM (GroupInfo, GroupMember)
acceptRelayJoinRequestAsync user uclId gInfo _ownerMember cReqInvId cReqChatVRange relayLink memberKey = do
    let msg = XGrpRelayAcpt relayLink memberKey

-- After:
acceptRelayJoinRequestAsync :: User -> Int64 -> GroupInfo -> GroupMember -> InvitationId -> VersionRangeChat -> ShortLinkContact -> CM (GroupInfo, GroupMember)
acceptRelayJoinRequestAsync user uclId gInfo _ownerMember cReqInvId cReqChatVRange relayLink = do
    let msg = XGrpRelayAcpt relayLink
```

### Subscriber.hs — Relay side

**5. `createRelayLink` (line 3629) — pass linkEntityId to createConnection, receive signing keys**

```haskell
-- Before:
createRelayLink :: GroupInfo -> CM ShortLinkContact
createRelayLink gi@GroupInfo {groupProfile} = do
    let userData = encodeShortLinkData $ GroupShortLinkData {groupProfile, publicGroupData = Nothing}
        ...
    (connId, (ccLink, _serviceId)) <- withAgent $ \a' -> createConnection a' NRMBackground (aUserId user) True True SCMContact (Just userLinkData) (Just crClientData) CR.IKPQOff subMode

-- After:
createRelayLink :: GroupInfo -> CM (C.KeyPairEd25519, ShortLinkContact)
createRelayLink gi = do
    let GroupInfo {membership = relayMembership} = gi
        userData = encodeShortLinkData $ RelayShortLinkData {relayProfile = memberProfile relayMembership}
        userLinkData = UserContactLinkData UserContactData {direct = True, owners = [], relays = [], userData}
        crClientData = encodeJSON $ CRDataGroup groupLinkId
        MemberId relayMemberIdBS = memberId relayMembership
    (Just sigKeys, (connId, (ccLink, _serviceId))) <- withAgent $ \a' -> createConnection a' NRMBackground (aUserId user) (Just relayMemberIdBS) True True SCMContact (Just userLinkData) (Just crClientData) CR.IKPQOff subMode
    ...
    pure (sigKeys, sLnk)
```

Key changes: (a) mutable data is `RelayShortLinkData` (relay profile) instead of `GroupShortLinkData`; (b) `linkEntityId = Just relayMemberIdBS` passed to `createConnection`; (c) signing keys received back from `createConnection` (first element of return tuple); (d) returns `(sigKeys, sLnk)` so parent can store the keys.

**6. `getLinkDataCreateRelayLink` (line 3609) — reorder: createRelayLink before updateRelayGroupKeys**

Current order:
```haskell
(_, memberPrivKey) <- C.generateKeyPair gVar              -- 1. generate keys
gInfo' <- withStore $ \db -> do
    void $ updateGroupProfile db user gInfo gp             -- 2. update profile
    updateRelayGroupKeys db user gInfo ... memberPrivKey .. -- 3. store keys
    getGroupInfo db vr user groupId
sLnk <- createRelayLink gInfo'                             -- 4. create link (generates DIFFERENT rootKey)
```

New order — `createRelayLink` first (it no longer depends on `updateGroupProfile` since it uses `RelayShortLinkData` with the relay's own profile, not `GroupShortLinkData` with the group profile):
```haskell
-- No key generation here — keys come from createConnection via createRelayLink
(sigKeys@(_, memberPrivKey), sLnk) <- createRelayLink gInfo  -- 1. create link, get signing keys
gInfo' <- withStore $ \db -> do
    void $ updateGroupProfile db user gInfo gp                -- 2. update profile
    updateRelayGroupKeys db user gInfo ... memberPrivKey ...   -- 3. store keys (now the SAME as rootKey)
    getGroupInfo db vr user groupId
pure (gInfo', sLnk, sigKeys)
```

The key pair now originates from `createConnection` → `prepareLinkData` (inside simplexmq), ensuring `rootKey` in `FixedLinkData` is exactly the relay's signing key. `updateRelayGroupKeys` stores the same private key whose public counterpart is `rootKey` in the link.

Return type changes from `MemberKey` to `C.KeyPairEd25519` — the `updateRelayGroupKeys` caller needs the private key for signing, and the public key is already in `FixedLinkData`.

**7. `acceptOwnerConnection` (line 3647) — remove `memberKey`**

```haskell
-- Before:
acceptOwnerConnection :: RelayRequestData -> GroupInfo -> ShortLinkContact -> MemberKey -> CM ()
acceptOwnerConnection RelayRequestData {relayInvId, reqChatVRange} gi relayLink memberKey = do
    ownerMember <- withStore $ \db -> getHostMember db vr user groupId
    void $ acceptRelayJoinRequestAsync user uclId gi ownerMember relayInvId reqChatVRange relayLink memberKey

-- After:
acceptOwnerConnection :: RelayRequestData -> GroupInfo -> ShortLinkContact -> CM ()
acceptOwnerConnection RelayRequestData {relayInvId, reqChatVRange} gi relayLink = do
    ownerMember <- withStore $ \db -> getHostMember db vr user groupId
    void $ acceptRelayJoinRequestAsync user uclId gi ownerMember relayInvId reqChatVRange relayLink
```

Update both call sites (line 3602 recovery case, line 3607 new case).

Recovery case (lines 3600-3604): the inner `case` on `(sLnk_, memberPubKey)` tuple is no longer needed — just match on `sLnk_`:
```haskell
Right GroupLink {connLinkContact = CCLink _ sLnk_} ->
  case sLnk_ of
    Just sLnk -> acceptOwnerConnection rrd gInfo sLnk
    Nothing -> throwChatError $ CEException "processRelayRequest: relay link doesn't have short link"
```

### Subscriber.hs — Owner side

**8. CONF handler for `XGrpRelayAcpt` (line 738) — start async link data fetch**

The CONF event is one-shot and non-retriable. If the handler throws before `allowAgentConnectionAsync`, the relay connection is permanently dead. Therefore: NO network calls in the CONF handler — use async fetch with continuation.

```haskell
-- Before (lines 738-746):
XGrpRelayAcpt relayLink memberKey
    | memberRole' membership == GROwner && isRelay m -> do
        withStore $ \db -> do
            relay <- getGroupRelayByGMId db (groupMemberId' m)
            liftIO $ updateGroupMemberStatus db userId m GSMemAccepted
            void $ liftIO $ setRelayLinkAccepted db relay relayLink memberKey
        allowAgentConnectionAsync user conn' confId XOk

-- After:
XGrpRelayAcpt relayLink
    | memberRole' membership == GROwner && isRelay m -> do
        withStore $ \db -> do
            relay <- getGroupRelayByGMId db (groupMemberId' m)
            liftIO $ setRelayConfId db relay confId relayLink
        void $ getAgentConnShortLinkAsync user CFGetRelayLinkOnAccept (Just conn') relayLink
```

Zero network calls in the handler. `confId` and `relayLink` are persisted in `group_relays` for the continuation. The async fetch reuses the existing relay connection (`conn'`) via `getConnShortLinkAsync` with `Just connId`. When LDATA arrives on this same connection, the continuation (change #11b) completes the acceptance.

### Subscriber.hs — Subscriber side

**9. `sendXGrpLinkMem` (line 870) — remove memberKey**

```haskell
-- Before:
sendXGrpLinkMem gInfo'' = do
    let GroupInfo {membership = membership'} = gInfo''
        ...
        memberKey = MemberKey <$> memberPubKey membership'
    void $ sendDirectMemberMessage conn (XGrpLinkMem profileToSend memberKey) groupId

-- After:
sendXGrpLinkMem gInfo'' = do
    let GroupInfo {membership = membership'} = gInfo''
        ...
    void $ sendDirectMemberMessage conn (XGrpLinkMem profileToSend) groupId
```

**10. `xGrpLinkMem` (line 2407) — remove memberKey_ parameter**

```haskell
-- Before:
xGrpLinkMem :: GroupInfo -> GroupMember -> Connection -> Profile -> Maybe MemberKey -> CM ()
xGrpLinkMem ... p' memberKey_ = do
    ...
    withStore' $ \db -> setXGrpLinkMemReceived db groupMemberId True memberKey_

-- After:
xGrpLinkMem :: GroupInfo -> GroupMember -> Connection -> Profile -> CM ()
xGrpLinkMem ... p' = do
    ...
    withStore' $ \db -> setXGrpLinkMemReceived db groupMemberId True
```

Update caller to pass 4 args instead of 5.

**11. LDATA handler (line 1093) — dispatch on command function**

Destructure full `FixedLinkData` and `cData`:
```haskell
-- Before:
LDATA FixedLinkData {linkConnReq = cReq} _cData ->
    withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
      case cmdFunction of
        CFGetShortLink -> ... (current subscriber logic)
        _ -> throwChatError $ CECommandError "unexpected cmdFunction"

-- After:
LDATA fd@FixedLinkData {linkConnReq = cReq, rootKey = relayKey, linkEntityId} cData ->
    withCompletedCommand conn agentMsg $ \CommandData {cmdFunction} ->
      case cmdFunction of
        CFGetRelayLinkOnJoin -> ... (change #11a: subscriber join — existing logic + relay data update)
        CFGetRelayLinkOnAccept -> ... (change #11b: owner accept — new continuation)
        _ -> throwChatError $ CECommandError "unexpected cmdFunction"
```

**11a. `CFGetRelayLinkOnJoin` branch — subscriber joins relay (existing logic + relay data update)**

Before the existing `updateConnLinkData` / `XMember` / `joinAgentConnectionAsync` logic, update relay member with data from the link:
```haskell
CFGetRelayLinkOnJoin -> do
    -- Update relay member with verified data from link (covers retry case)
    relayLinkData_ <- liftIO $ decodeLinkUserData cData
    forM_ (relayLinkData_ :: Maybe RelayShortLinkData) $ \RelayShortLinkData {relayProfile} ->
        withStore' $ \db -> updateRelayMemberData db (groupMemberId' m) (MemberId <$> linkEntityId) relayKey relayProfile
    -- Then existing logic: updateConnLinkData, XMember, joinAgentConnectionAsync
    case cReq of
      CRContactUri crData@ConnReqUriData {crClientData} -> do
        ...
```

**11b. `CFGetRelayLinkOnAccept` branch — owner accepts relay (new continuation)**

This is the continuation for the owner's async link data fetch started in the CONF handler (change #8). LDATA arrives on the SAME connection that received CONF (because `getConnShortLinkAsync` was called with `Just connId` — see change #8 and simplexmq change #5).

```haskell
CFGetRelayLinkOnAccept -> do
    let MemberId expectedMemberId = memberId m
    unless (linkEntityId == Just expectedMemberId) $
        messageError "relay link: relay member ID mismatch"
    relayProfile <- liftIO (decodeLinkUserData cData) >>= \case
        Just RelayShortLinkData {relayProfile = p} -> pure p
        Nothing -> throwChatError $ CEException "relay link: no relay link data"
    relay <- withStore $ \db -> getGroupRelayByGMId db (groupMemberId' m)
    (storedConfId, storedRelayLink) <- maybe (throwChatError $ CEException "relay link: no confId") pure $
        liftA2 (,) (relayConfId relay) (relayLink relay)
    withStore $ \db -> do
        liftIO $ updateGroupMemberStatus db userId m GSMemAccepted
        void $ liftIO $ setRelayLinkAccepted db relay storedRelayLink relayKey relayProfile
    allowAgentConnectionAsync user conn storedConfId XOk
```

`conn` here is the original relay connection — LDATA was delivered on it because the CONF handler passed `Just conn'` to `getAgentConnShortLinkAsync` (change #8). `confId` and `relayLink` were persisted by `setRelayConfId` in the CONF handler.

### Commands.hs

**12. `connectToRelay` (line 2068) — extract relay data from link, update member**

```haskell
-- Before:
connectToRelay gInfo' relayLink = do
    gVar <- asks random
    relayMember <- withFastStore $ \db -> getCreateRelayForMember db vr gVar user gInfo' relayLink
    r <- tryAllErrors $ do
        (fd, _cData) <- getShortLinkConnReq nm user relayLink
        let cReq = linkConnReq fd
            relayLinkToConnect = CCLink cReq (Just relayLink)
        void $ connectViaContact user (Just $ PCEGroup gInfo' relayMember) incognito relayLinkToConnect Nothing Nothing
    relayMember' <- withFastStore $ \db -> getGroupMember db vr user groupId (groupMemberId' relayMember)
    pure (relayLink, relayMember', r)

-- After:
connectToRelay gInfo' relayLink = do
    gVar <- asks random
    -- Create member with placeholder first (required for retry support)
    relayMember <- withFastStore $ \db -> getCreateRelayForMember db vr gVar user gInfo' relayLink
    r <- tryAllErrors $ do
        (fd@FixedLinkData {rootKey = relayKey, linkEntityId}, cData) <- getShortLinkConnReq nm user relayLink
        -- Update relay member with verified data from link
        relayLinkData_ <- liftIO $ decodeLinkUserData cData
        forM_ (relayLinkData_ :: Maybe RelayShortLinkData) $ \RelayShortLinkData {relayProfile} ->
            withFastStore' $ \db -> updateRelayMemberData db (groupMemberId' relayMember) (MemberId <$> linkEntityId) relayKey relayProfile
        let cReq = linkConnReq fd
            relayLinkToConnect = CCLink cReq (Just relayLink)
        void $ connectViaContact user (Just $ PCEGroup gInfo' relayMember) incognito relayLinkToConnect Nothing Nothing
    relayMember' <- withFastStore $ \db -> getGroupMember db vr user groupId (groupMemberId' relayMember)
    pure (relayLink, relayMember', r)
```

Key design: Member is created with placeholder data BEFORE `tryAllErrors` (required for retry — `retryRelayConnectionAsync` expects the member to exist). Inside `tryAllErrors`, link data is fetched and the member is updated with real relay data. If the fetch fails with a temp error, the member exists with placeholder data, and retry via LDATA handler will update it.

New DB function needed: `updateRelayMemberData` — updates `member_id`, `member_pub_key`, and profile on an existing relay member.

**13. All `createConnection` callers — add `Nothing` for linkEntityId, update return destructuring**

Every call to `createConnection` in Commands.hs (lines 1844, 1886, 2168, 2434, 2858, 2899) and Subscriber.hs (line 3639, handled in change #5):
- Add `Nothing` for `Maybe ByteString` (linkEntityId) after `aUserId user`
- Update return destructuring: `(connId, (ccLink, ...))` → `(_, (connId, (ccLink, ...)))`

Only the relay link creation (change #5) passes `Just relayMemberIdBS` and uses the returned signing keys.

### Groups.hs

**14. `setRelayLinkAccepted` (line 1420) — accept rootKey + profile instead of MemberKey**

```haskell
-- Before:
setRelayLinkAccepted :: DB.Connection -> GroupRelay -> ShortLinkContact -> MemberKey -> IO GroupRelay
setRelayLinkAccepted db relay@GroupRelay {groupRelayId, groupMemberId} relayLink (MemberKey k) = do
    ...
    DB.execute db "UPDATE group_members SET relay_link = ?, member_pub_key = ?, ..." (relayLink, k, ...)

-- After:
setRelayLinkAccepted :: DB.Connection -> GroupRelay -> ShortLinkContact -> C.PublicKeyEd25519 -> Profile -> IO GroupRelay
setRelayLinkAccepted db relay@GroupRelay {groupRelayId, groupMemberId} relayLink relayKey relayProfile = do
    ...
    -- Store relay_link and member_pub_key
    DB.execute db "UPDATE group_members SET relay_link = ?, member_pub_key = ?, ..." (relayLink, relayKey, ...)
    -- Update relay member profile
    -- (update contact_profiles for the relay member)
```

**15. `setXGrpLinkMemReceived` (line 2854) — remove Maybe MemberKey**

```haskell
-- Before:
setXGrpLinkMemReceived :: DB.Connection -> GroupMemberId -> Bool -> Maybe MemberKey -> IO ()
setXGrpLinkMemReceived db mId xGrpLinkMemReceived memberKey_ = do
    let k = (\(MemberKey k') -> k') <$> memberKey_
    DB.execute db "UPDATE group_members SET xgrplinkmem_received = ?, member_pub_key = ?, ..." (BI xGrpLinkMemReceived, k, ...)

-- After:
setXGrpLinkMemReceived :: DB.Connection -> GroupMemberId -> Bool -> IO ()
setXGrpLinkMemReceived db mId xGrpLinkMemReceived = do
    DB.execute db "UPDATE group_members SET xgrplinkmem_received = ?, ..." (BI xGrpLinkMemReceived, ...)
```

Remove `member_pub_key` from the UPDATE — key is now set during connection (from link data), not from `XGrpLinkMem`.

**16. `getCreateRelayForMember` (line 1352) — unchanged**

Keep current signature and behavior (creates member with random placeholder data). The member is updated with real data inside `tryAllErrors` via `updateRelayMemberData` (change #12) or via the LDATA handler on retry.

This preserves retry support: the member must exist with `relay_link` set before any connection attempt, so `retryRelayConnectionAsync` can find it.

**17. New: `updateRelayMemberData` in Groups.hs**

```haskell
updateRelayMemberData :: DB.Connection -> GroupMemberId -> Maybe MemberId -> C.PublicKeyEd25519 -> Profile -> IO ()
```

Updates an existing relay member with verified data from the relay link:
- `member_id` (if provided, replaces placeholder)
- `member_pub_key` (relay's public key = rootKey from FixedLinkData)
- Profile (relay's profile from RelayShortLinkData)

**18. Subscriber.hs line 973 — update `XGrpLinkMem` call site**

```haskell
-- Before:
XGrpLinkMem p memberKey -> Nothing <$ xGrpLinkMem gInfo' m'' conn' p memberKey

-- After:
XGrpLinkMem p -> Nothing <$ xGrpLinkMem gInfo' m'' conn' p
```

### Types.hs

**19. Rename `CFGetShortLink` → `CFGetRelayLinkOnJoin`, add `CFGetRelayLinkOnAccept`**

```haskell
-- Before:
  | CFGetShortLink

-- After:
  | CFGetRelayLinkOnJoin
  | CFGetRelayLinkOnAccept
```

Update `textDecode`/`textEncode`:
```haskell
"get_relay_link_on_join" -> Just CFGetRelayLinkOnJoin
"get_relay_link_on_accept" -> Just CFGetRelayLinkOnAccept

CFGetRelayLinkOnJoin -> "get_relay_link_on_join"
CFGetRelayLinkOnAccept -> "get_relay_link_on_accept"
```

Update `commandExpectedResponse`:
```haskell
CFGetRelayLinkOnJoin -> t LDATA_
CFGetRelayLinkOnAccept -> t LDATA_
```

### Internal.hs

**20. Parameterize `getAgentConnShortLinkAsync` with `CommandFunction` and optional `Connection`**

```haskell
-- Before:
getAgentConnShortLinkAsync :: User -> ShortLinkContact -> CM (CommandId, ConnId)
getAgentConnShortLinkAsync user shortLink = do
    ...
    cmdId <- withStore' $ \db -> createCommand db user Nothing CFGetShortLink
    connId <- withAgent $ \a -> getConnShortLinkAsync a (aUserId user) (aCorrId cmdId) shortLink'
    ...

-- After:
getAgentConnShortLinkAsync :: User -> CommandFunction -> Maybe Connection -> ShortLinkContact -> CM (CommandId, ConnId)
getAgentConnShortLinkAsync user cmdFunc conn_ shortLink = do
    ...
    cmdId <- withStore' $ \db -> createCommand db user (dbConnId <$> conn_) cmdFunc
    connId <- withAgent $ \a -> getConnShortLinkAsync a (aUserId user) (aCorrId cmdId) (aConnId <$> conn_) shortLink'
    ...
```

When `conn_ = Just conn`: command is linked to the existing connection, agent's `getConnShortLinkAsync` reuses it, LDATA fires on that connection.

When `conn_ = Nothing`: current behavior — new temp connection created, command has no connection ID.

Update caller in `retryRelayConnectionAsync` (Commands.hs:2089):
```haskell
newConnIds <- getAgentConnShortLinkAsync user CFGetRelayLinkOnJoin Nothing relayLink
```

### Groups.hs

**21. Add `confId` and `relayLink` storage to `group_relays`**

New function to store confId and relay link when CONF arrives:
```haskell
setRelayConfId :: DB.Connection -> GroupRelay -> ConfirmationId -> ShortLinkContact -> IO ()
```

Updates `group_relays` with `conf_id` (ByteString) and `relay_link`. The LDATA continuation (change #11b) retrieves both via `relayConfId` and `relayLink` fields.

Update `GroupRelay` type and `toGroupRelay` to include `relayConfId :: Maybe ConfirmationId`.

## Key design decisions

1. **Owner fetches relay link data asynchronously** — CONF is one-shot and non-retriable. A synchronous network call that fails in the CONF handler permanently kills the relay connection. The async pattern (store confId, start fetch, continue on LDATA) is resilient to network failures.

2. **Owner reuses existing relay connection for async fetch** — `getConnShortLinkAsync` (simplexmq change #5) accepts `Maybe ConnId`. The owner passes the existing relay connection, so LDATA fires on that same connection.

3. **`CFGetRelayLinkOnJoin` / `CFGetRelayLinkOnAccept`** — two command tags dispatched in the LDATA handler. Subscriber join uses one, owner accept uses the other. Both fetch the same data but run different continuations.

4. **`confId` persisted in `group_relays`** — the LDATA continuation needs confId to call `allowAgentConnectionAsync`. Stored in DB to survive restarts.

5. **`RelayShortLinkData` is minimal** — only `relayProfile`. Key and memberId are in `FixedLinkData` fields, not duplicated in mutable data.

6. **No backward compatibility** — channels/relays is unreleased. All changes are breaking.

## Verification checklist

- [ ] Relay creates link: passes `linkEntityId = Just relayMemberIdBS` to `createConnection`, receives signing keys back (keys become `rootKey` in `FixedLinkData`)
- [ ] Owner CONF handler: zero network calls, stores confId + relayLink, starts async fetch with `CFGetRelayLinkOnAccept` on existing relay connection
- [ ] Owner LDATA continuation (`CFGetRelayLinkOnAccept`): verifies memberId, extracts profile, calls `allowAgentConnectionAsync` on `conn` (the same relay connection) with stored confId
- [ ] Subscriber extracts from link: same key, memberId, and profile via `CFGetRelayLinkOnJoin`
- [ ] `XGrpRelayAcpt` no longer carries `MemberKey`
- [ ] `XGrpLinkMem` no longer carries `Maybe MemberKey`
- [ ] `XGrpLinkMem` call site (Subscriber.hs:973) updated
- [ ] All `createConnection` callers updated with `Nothing` for linkEntityId and `(_, (connId, ...))` return destructuring
- [ ] Recovery case in `processRelayRequest` simplified — only checks `sLnk_`
- [ ] Async retry path (subscriber LDATA via `CFGetRelayLinkOnJoin`) still works — member created with placeholder, updated on retry
- [ ] `getCreateRelayForMember` keeps placeholder creation for retry support
- [ ] `updateRelayGroupKeys` unchanged — still stores owner's data + relay's private key
- [ ] `XMember` (subscriber → relay) unchanged — carries subscriber's own key, opposite direction
- [ ] `createRelayLink` receives signing keys from `createConnection` return value (no external key generation)
- [ ] `getLinkDataCreateRelayLink` no longer generates key pair — gets it from `createRelayLink`
- [ ] `updateRelayGroupKeys` updated to accept `C.KeyPairEd25519` from `createRelayLink`
- [ ] `getAgentConnShortLinkAsync` parameterized with `CommandFunction` and optional `Connection`
- [ ] `CFGetShortLink` renamed to `CFGetRelayLinkOnJoin` in all locations
- [ ] `CFGetRelayLinkOnAccept` added with `LDATA_` expected response
- [ ] `group_relays` stores confId and relayLink for async continuation
