# simplexmq: Thread linkEntityId through createConnection, return signing keys

## Goal

Allow callers of `createConnection` to provide a `linkEntityId` value and receive back the signing keys generated internally. Currently, `prepareLinkData` (inside `newRcvConnSrv`) generates signing keys internally, hardcodes `linkEntityId = Nothing`, and does not expose the keys.

## Motivation

The simplex-chat relay needs the link's `rootKey` to be the relay's signing key, and `linkEntityId` to be the relay's `MemberId`. By passing `linkEntityId` in and getting signing keys out — the same pattern as `prepareConnectionLink` — the relay can store the keys and embed its identity in the tamper-proof `FixedLinkData`.

## Design: follow `prepareConnectionLink` pattern

`prepareConnectionLink` already solves this problem for the two-step link creation path:
```haskell
prepareConnectionLink :: AgentClient -> UserId -> Maybe ByteString -> Bool -> Maybe CRClientData
                      -> AE (C.KeyPairEd25519, CreatedConnLink 'CMContact, PreparedLinkParams)
```
- `linkEntityId :: Maybe ByteString` goes in (3rd param, after userId)
- `C.KeyPairEd25519` comes out (1st element of return tuple)

Apply the same pattern to the `createConnection` → `newConn` → `newRcvConnSrv` → `prepareLinkData` chain.

## Changed functions

### 1. `prepareLinkData` (local in `newRcvConnSrv`, line ~1165)

Add `Maybe ByteString` as first parameter (linkEntityId). Return `C.KeyPairEd25519` as first element.

```haskell
-- Before:
prepareLinkData :: UserConnLinkData c -> C.PublicKeyX25519 -> AM (C.CbNonce, SMPQueueUri, ConnectionRequestUri c, ClntQueueReqData)
prepareLinkData userLinkData e2eDhKey = do
    ...
    sigKeys@(_, privSigKey) <- atomically $ C.generateKeyPair @'C.Ed25519 g
    ...
    let (linkKey, linkData) = SL.encodeSignLinkData sigKeys smpAgentVRange connReq Nothing userLinkData
    ...
    pure (nonce, qUri, connReq, qd)

-- After:
prepareLinkData :: Maybe ByteString -> UserConnLinkData c -> C.PublicKeyX25519 -> AM (C.KeyPairEd25519, C.CbNonce, SMPQueueUri, ConnectionRequestUri c, ClntQueueReqData)
prepareLinkData linkEntityId userLinkData e2eDhKey = do
    ...
    sigKeys@(_, privSigKey) <- atomically $ C.generateKeyPair @'C.Ed25519 g
    ...
    let (linkKey, linkData) = SL.encodeSignLinkData sigKeys smpAgentVRange connReq linkEntityId userLinkData
    ...
    pure (sigKeys, nonce, qUri, connReq, qd)
```

Same key generation as before — just threaded `linkEntityId` to `encodeSignLinkData` and returned `sigKeys`.

### 2. `newRcvConnSrv` (line 1134)

Add `Maybe ByteString` parameter after `UserId`. Return `Maybe C.KeyPairEd25519` as first element.

```haskell
-- Before:
newRcvConnSrv :: ... -> AgentClient -> NetworkRequestMode -> UserId -> ConnId -> Bool -> SConnectionMode c -> Maybe (UserConnLinkData c) -> Maybe CRClientData -> CR.InitialKeys -> SubscriptionMode -> SMPServerWithAuth -> AM (CreatedConnLink c, Maybe ClientServiceId)

-- After:
newRcvConnSrv :: ... -> AgentClient -> NetworkRequestMode -> UserId -> Maybe ByteString -> ConnId -> Bool -> SConnectionMode c -> Maybe (UserConnLinkData c) -> Maybe CRClientData -> CR.InitialKeys -> SubscriptionMode -> SMPServerWithAuth -> AM (Maybe C.KeyPairEd25519, (CreatedConnLink c, Maybe ClientServiceId))
```

In the body, `Just d` branch:
```haskell
    Just d -> do
      (sigKeys, nonce, qUri, cReq, qd) <- prepareLinkData linkEntityId d $ fst e2eKeys
      ...
      pure (Just sigKeys, (ccLink, clientServiceId rq))
    Nothing -> do
      ...
      pure (Nothing, ...)
```

### 3. `newConn` (line 911)

Add `Maybe ByteString` parameter after `UserId`. Return `Maybe C.KeyPairEd25519` as first element.

```haskell
-- Before:
newConn :: ... -> AM (ConnId, (CreatedConnLink c, Maybe ClientServiceId))
newConn c nm userId enableNtfs checkNotices cMode linkData_ clientData pqInitKeys subMode = do
  ...
  (connId,)
    <$> newRcvConnSrv c nm userId connId enableNtfs cMode linkData_ clientData pqInitKeys subMode srv

-- After:
newConn :: ... -> AM (Maybe C.KeyPairEd25519, (ConnId, (CreatedConnLink c, Maybe ClientServiceId)))
newConn c nm userId linkEntityId enableNtfs checkNotices cMode linkData_ clientData pqInitKeys subMode = do
  ...
  (sigKeys_, r) <- newRcvConnSrv c nm userId linkEntityId connId enableNtfs cMode linkData_ clientData pqInitKeys subMode srv
      `catchE` \e -> withStore' c (`deleteConnRecord` connId) >> throwE e
  pure (sigKeys_, (connId, r))
```

### 4. `createConnection` (line 399)

Add `Maybe ByteString` parameter after `UserId`. Return `Maybe C.KeyPairEd25519` as first element.

```haskell
-- Before:
createConnection :: ConnectionModeI c => AgentClient -> NetworkRequestMode -> UserId -> Bool -> Bool -> SConnectionMode c -> Maybe (UserConnLinkData c) -> Maybe CRClientData -> CR.InitialKeys -> SubscriptionMode -> AE (ConnId, (CreatedConnLink c, Maybe ClientServiceId))
createConnection c nm userId enableNtfs checkNotices = withAgentEnv c .::. newConn c nm userId enableNtfs checkNotices

-- After:
createConnection :: ConnectionModeI c => AgentClient -> NetworkRequestMode -> UserId -> Maybe ByteString -> Bool -> Bool -> SConnectionMode c -> Maybe (UserConnLinkData c) -> Maybe CRClientData -> CR.InitialKeys -> SubscriptionMode -> AE (Maybe C.KeyPairEd25519, (ConnId, (CreatedConnLink c, Maybe ClientServiceId)))
createConnection c nm userId linkEntityId enableNtfs checkNotices = withAgentEnv c .::. newConn c nm userId linkEntityId enableNtfs checkNotices
```

## Existing callers

All pass `Nothing` for `linkEntityId` and destructure with `(_, (connId, ...))`:

```haskell
-- Before:
(connId, (ccLink, serviceId)) <- createConnection a nm userId True True SCMContact ...

-- After:
(_, (connId, (ccLink, serviceId))) <- createConnection a nm userId Nothing True True SCMContact ...
```

### 5. `getConnShortLinkAsync` (line 358)

Add optional `Maybe ConnId` parameter. When provided, use existing connection instead of creating a new one.

```haskell
-- Before:
getConnShortLinkAsync :: AgentClient -> UserId -> ACorrId -> ConnShortLink 'CMContact -> AE ConnId

-- After:
getConnShortLinkAsync :: AgentClient -> UserId -> ACorrId -> Maybe ConnId -> ConnShortLink 'CMContact -> AE ConnId
```

Internal implementation (`getConnShortLinkAsync'`, line 1004):
```haskell
-- Before:
getConnShortLinkAsync' c userId corrId shortLink@(CSLContact _ _ srv _) = do
  g <- asks random
  connId <- withStore c $ \db -> do
    void $ createServer db srv
    prepareNewConn db g
  enqueueCommand c corrId connId (Just srv) $ AClientCommand $ LGET shortLink
  pure connId

-- After:
getConnShortLinkAsync' c userId corrId connId_ shortLink@(CSLContact _ _ srv _) = do
  connId <- case connId_ of
    Just existingConnId -> do
      -- connId and srv can be unrelated: connId is used as "mailbox" for LDATA delivery,
      -- while srv is the short link's server for the LGET request.
      -- E.g., owner's relay connection (connId, on server A) fetches relay's group link data (srv = server B).
      -- This works because enqueueCommand stores (connId, srv) independently in the commands table,
      -- the network request targets srv, and event delivery uses connId via corrId correlation.
      withStore c $ \db -> void $ createServer db srv
      pure existingConnId
    Nothing -> do
      g <- asks random
      withStore c $ \db -> do
        void $ createServer db srv
        prepareNewConn db g
  enqueueCommand c corrId connId (Just srv) $ AClientCommand $ LGET shortLink
  pure connId
```

When `connId_ = Just existingConnId`: skips `prepareNewConn`, enqueues LGET on the existing connection. LDATA fires on that same connection. The connection's server and the short link's server are independent — `enqueueCommand` stores them separately, the LGET request goes to `srv`, and the LDATA response is delivered on `connId` via `corrId` correlation. **Add the comment shown above to the implementation.**

When `connId_ = Nothing`: current behavior — creates new connection.

### Existing callers of `getConnShortLinkAsync`

All pass `Nothing` for the new parameter — no behavioral change.

## Not changed

- `prepareConnectionLink` / `createConnectionForLink` — already supports `linkEntityId` parameter and returns signing keys. Used by owner's channel creation. No change needed.
- `setConnShortLink` — separate path, not affected.
- Key generation logic in `prepareLinkData` — unchanged, keys still generated internally.
