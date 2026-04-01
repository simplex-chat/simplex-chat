# Plan: APITestChatRelay — Relay Liveness + Identity Verification

**Date: 2026-04-01**

## Context

Channel owners configure relays by address but have no way to verify a relay is alive, authentic, or to discover its profile before creating a channel. A broken or impersonated relay means a broken channel.

`APITestChatRelay` solves this by:
1. Fetching the relay's short link data (validates SMP server reachability + retrieves relay profile)
2. Running a challenge-response handshake (`XGrpRelayTest`) that proves the relay controls its address private key (`linkPrivSigKey`)
3. Returning the relay profile and test result to the UI

The test can run before any `chat_relays` DB record exists — the UI uses the returned profile to populate the relay name field.

No DB schema changes are needed — `name` remains on `chat_relays`, and the relay profile is returned to the UI only.

---

## Data Flow

```
Owner                              SMP Server                         Relay
  |                                    |                                |
  |--- getShortLinkConnReq ----------->|                                |
  |<-- FixedLinkData{rootKey,cReq} ----|                                |
  |    + ConnLinkData{RelayAddressLinkData{relayProfile}}               |
  |                                    |                                |
  |--- joinConnection(XGrpRelayTest{challenge}) ---------------------->|
  |                                    |          REQ with challenge    |
  |                                    |          relay signs challenge |
  |                                    |          with linkPrivSigKey   |
  |<-- CONF(XGrpRelayTest{signature}) ----------------------------------|
  |    verify: C.verify' rootKey sig challenge                          |
  |    cleanup connections on both sides                                |
```

---

## Types

### RelayProfile (Protocol.hs)

```haskell
data RelayProfile = RelayProfile {name :: ContactName}
  deriving (Eq, Show)

$(JQ.deriveJSON defaultJSON ''RelayProfile)
```

Simpler than `Profile` — relay identity needs only a name. Can be extended later with image, description, etc.

### RelayAddressLinkData (Protocol.hs)

```haskell
data RelayAddressLinkData = RelayAddressLinkData {relayProfile :: RelayProfile}
  deriving (Show)

$(JQ.deriveJSON defaultJSON ''RelayAddressLinkData)
```

Stored as `userData` in the relay's contact address short link data. Separate from `ContactShortLinkData` (which has irrelevant `message`/`business` fields) and `RelayShortLinkData` (per-group relay links).

### XGrpRelayTest (Protocol.hs)

```haskell
XGrpRelayTest :: ByteString -> Maybe (C.Signature 'C.Ed25519) -> ChatMsgEvent 'Json
```

Single constructor used in both directions:
- **Owner → Relay** (in joinConnection connInfo): `XGrpRelayTest challenge Nothing`
- **Relay → Owner** (in acceptContact connInfo): `XGrpRelayTest challenge (Just signature)`

The relay profile is NOT included — the owner already has it from `RelayAddressLinkData` in the short link's `userData` (retrieved in step 1 via `decodeLinkUserData`).

JSON encoding (follows `(.=?)` chain pattern, e.g. `XGrpMemDel`):
```haskell
XGrpRelayTest challenge sig_ -> o $
  ("signature" .=? (B64UrlByteString . C.signatureBytes <$> sig_))
  ["challenge" .= B64UrlByteString challenge]
```

JSON parsing:
```haskell
XGrpRelayTest_ -> do
  B64UrlByteString challenge <- v .: "challenge"
  sig_ <- mapM decodeSig =<< opt "signature"
  pure $ XGrpRelayTest challenge sig_
```

Where `decodeSig` converts `B64UrlByteString` to `C.Signature 'C.Ed25519`:
```haskell
decodeSig :: B64UrlByteString -> Either String (C.Signature 'C.Ed25519)
decodeSig (B64UrlByteString s) = C.decodeSignature s
```

Note: `B64UrlByteString` is defined in `Types.hs:151` — add import to Protocol.hs if not already imported.

### RelayTestError (Controller.hs)

```haskell
data RelayTestStep
  = RTSGetLink       -- fetching short link data from SMP server
  | RTSDecodeLink    -- decoding RelayAddressLinkData from link userData
  | RTSConnect       -- preparing and joining connection
  | RTSWaitResponse  -- waiting for relay's signed response
  | RTSVerify        -- verifying relay's signature
  deriving (Show)

data RelayTestFailure = RelayTestFailure
  { rtfStep :: RelayTestStep,
    rtfDescription :: String
  }
  deriving (Show)
```

Pattern follows `ProtocolTestFailure {testStep, testError}` from simplexmq.

### RelayTest (Controller.hs)

```haskell
data RelayTest = RelayTest
  { challenge :: ByteString,
    rootKey :: C.PublicKeyEd25519,
    result :: TMVar (Maybe RelayTestFailure)
  }
```

- `challenge` — random bytes sent to relay
- `rootKey` — from `FixedLinkData`, used to verify relay's signature
- `result` — `Nothing` = success, `Just failure` = error

### ChatController field

```haskell
chatRelayTests :: TMap ConnId RelayTest,
```

### ChatCommand

```haskell
| APITestChatRelay UserId ShortLinkContact
| TestChatRelay ShortLinkContact
```

Takes a `ShortLinkContact` (`ConnShortLink 'CMContact`) — relay addresses are always short links. This matches `UserChatRelay.address :: ShortLinkContact` and is directly accepted by `getShortLinkConnReq :: ... -> ConnShortLink m -> ...`.

### ChatResponse

```haskell
| CRChatRelayTestResult {user :: User, relayProfile :: Maybe RelayProfile, testFailure :: Maybe RelayTestFailure}
```

- On success: `relayProfile = Just p, testFailure = Nothing`
- On failure at link fetch/decode: `relayProfile = Nothing, testFailure = Just err` (profile not yet available)
- On failure at connect/verify: `relayProfile = Just p, testFailure = Just err` (profile from link data)

---

## Implementation

### Phase 1: Protocol — XGrpRelayTest + RelayAddressLinkData + RelayProfile

**File: `src/Simplex/Chat/Protocol.hs`**

1. Add `RelayProfile` type (near `RelayShortLinkData`, ~line 1444):
   - `data RelayProfile = RelayProfile {name :: ContactName}`
   - `deriveJSON`

2. Add `RelayAddressLinkData` type (after `RelayShortLinkData`):
   - `data RelayAddressLinkData = RelayAddressLinkData {relayProfile :: RelayProfile}`
   - `deriveJSON`

3. Add `XGrpRelayTest` constructor (after `XGrpRelayAcpt`, ~line 438):
   - `XGrpRelayTest :: ByteString -> Maybe (C.Signature 'C.Ed25519) -> ChatMsgEvent 'Json`

4. Add event tag `XGrpRelayTest_` (after `XGrpRelayAcpt_`, ~line 966)

5. Add tag string `"x.grp.relay.test"` (after `"x.grp.relay.acpt"`, ~line 1022)

6. Add tag parsing (after `XGrpRelayAcpt_` parse, ~line 1079)

7. Add event-to-tag mapping (after `XGrpRelayAcpt` mapping, ~line 1132):
   - `XGrpRelayTest {} -> XGrpRelayTest_`

8. Add JSON parsing (~line 1284):
   ```haskell
   XGrpRelayTest_ -> do
     B64UrlByteString challenge <- v .: "challenge"
     sig_ <- mapM decodeSig =<< opt "signature"
     pure $ XGrpRelayTest challenge sig_
   ```
   Where `decodeSig (B64UrlByteString s) = C.decodeSignature s`.

9. Add JSON encoding (~line 1351):
   ```haskell
   XGrpRelayTest challenge sig_ -> o $
     ("signature" .=? (B64UrlByteString . C.signatureBytes <$> sig_))
     ["challenge" .= B64UrlByteString challenge]
   ```

### Phase 2: Controller types — RelayTest, RelayTestFailure, commands, response

**File: `src/Simplex/Chat/Controller.hs`**

1. Add `RelayTestStep` and `RelayTestFailure` types (near `ProtocolTestFailure` usage)

2. Add `RelayTest` type

3. Add `chatRelayTests :: TMap ConnId RelayTest` field to `ChatController` (after `relayRequestWorkers`, ~line 252)

4. Uncomment and update `APITestChatRelay` (lines 401-403):
   ```haskell
   | APITestChatRelay UserId ShortLinkContact
   | TestChatRelay ShortLinkContact
   ```

5. Add `CRChatRelayTestResult` to `ChatResponse` (after `CRServerTestResult`, ~line 667):
   ```haskell
   | CRChatRelayTestResult {user :: User, relayProfile :: Maybe RelayProfile, testFailure :: Maybe RelayTestFailure}
   ```

**File: `src/Simplex/Chat.hs`**

6. Initialize `chatRelayTests` in `newChatController` (after `relayRequestWorkers`, ~line 175):
   ```haskell
   chatRelayTests <- TM.emptyIO
   ```
   Add `chatRelayTests` to the record construction (~line 218).

### Phase 3: Agent API — getConnLinkPrivKey (simplexmq change)

The relay needs to sign the challenge with `ShortLinkCreds.linkPrivSigKey`, which is stored in the agent's DB on `RcvQueue`. The chat layer has no direct access to the key.

**New agent API function in `simplexmq/src/Simplex/Messaging/Agent.hs`:**

```haskell
getConnLinkPrivKey :: AgentClient -> ConnId -> AE (Maybe C.PrivateKeyEd25519)
```

Implementation:
1. Look up `SomeConn` by `ConnId` via `withStore c getConn`
2. Pattern match on `ContactConnection _ rq` or `RcvConnection _ rq`
3. Return `linkPrivSigKey <$> shortLink rq` (returns `Nothing` if no short link creds)

The chat layer then signs: `C.sign' privKey challenge`.

This is a local operation (no network IO), so it's synchronous.

**Separate plan file:** `plans/agent-sign-for-address.md`

### Phase 4: Commands.hs — APITestChatRelay handler

**File: `src/Simplex/Chat/Library/Commands.hs`**

Add `import System.Timeout (timeout)`.

Add handler after `APITestProtoServer` (~line 1491):

```haskell
APITestChatRelay userId address -> withUserId userId $ \user -> do
  -- Step 1: Fetch link data (validates SMP server + gets profile)
  let failAt step desc = pure $ CRChatRelayTestResult user Nothing (Just $ RelayTestFailure step desc)
  r <- tryAllErrors $ getShortLinkConnReq nm user address
  case r of
    Left e -> failAt RTSGetLink (show e)
    Right (FixedLinkData {rootKey, linkConnReq = cReq}, cData) -> do
      -- Step 2: Decode relay profile from link data
      relayProfile_ <- liftIO $ decodeLinkUserData cData
      case relayProfile_ of
        Nothing -> failAt RTSDecodeLink "no relay address link data"
        Just RelayAddressLinkData {relayProfile} -> do
          let failWithProfile step desc =
                pure $ CRChatRelayTestResult user (Just relayProfile) (Just $ RelayTestFailure step desc)
          -- Step 3: Generate challenge + prepare connection
          gVar <- asks random
          challenge <- liftIO $ atomically $ C.randomBytes 32 gVar
          lift (withAgent' $ \a -> connRequestPQSupport a PQSupportOff cReq) >>= \case
            Nothing -> failWithProfile RTSConnect "invalid connection request"
            Just (agentV, _) -> do
              let chatV = agentToChatVersion agentV
              subMode <- chatReadVar subscriptionMode
              connId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True cReq PQSupportOff
              conn@Connection {connId = dbConnId} <- withFastStore $ \db ->
                createRelayTestConnection db vr user connId ConnPrepared chatV subMode
              -- Register test in TMap
              testVar <- newEmptyTMVarIO
              let acId = aConnId conn
                  relayTest = RelayTest {challenge, rootKey, result = testVar}
              chatRelayTests_ <- asks chatRelayTests
              atomically $ TM.insert acId relayTest chatRelayTests_
              -- Join with challenge, wrapped in tryAllErrors for cleanup safety
              testResult <- tryAllErrors $ do
                dm <- encodeConnInfo $ XGrpRelayTest challenge Nothing
                void $ withAgent $ \a -> joinConnection a nm (aUserId user) acId True cReq dm PQSupportOff subMode
                liftIO $ timeout 40_000_000 $ atomically $ takeTMVar testVar
              -- Cleanup always (even on error)
              atomically $ TM.delete acId chatRelayTests_
              withFastStore' $ \db -> deleteConnectionRecord db user dbConnId
              deleteAgentConnectionAsync acId
              case testResult of
                Left e -> failWithProfile RTSConnect (show e)
                Right Nothing -> failWithProfile RTSWaitResponse "timeout"
                Right (Just Nothing) -> pure $ CRChatRelayTestResult user (Just relayProfile) Nothing
                Right (Just (Just failure)) -> pure $ CRChatRelayTestResult user (Just relayProfile) (Just failure)
TestChatRelay address -> withUser $ \User {userId} ->
  processChatCommand vr nm $ APITestChatRelay userId address
```

Also add CLI parsing for `TestChatRelay` in the command parser.

Key points:
- `address :: ShortLinkContact` — passes directly to `getShortLinkConnReq` (no type mismatch)
- `conn@Connection {connId = dbConnId}` — explicit pattern match avoids `DuplicateRecordFields` ambiguity
- `tryAllErrors` wraps only the join+wait block; cleanup runs unconditionally after it
- `tryAllErrors` (from `Simplex.Messaging.Util`) catches ALL exceptions via `UE.catch`, not just `ChatError`
- `void $ withAgent $ \a -> joinConnection ...` — discards `(SndQueueSecured, Maybe ClientServiceId)` return

### Phase 5: Subscriber.hs — Event handlers

**File: `src/Simplex/Chat/Library/Subscriber.hs`**

#### Owner side: processDirectMessage CONF handler (contact_ = Nothing)

Modify the CONF handler at lines 407-417. Before the existing flow, check if this connection is a relay test:

```haskell
Nothing -> case agentMsg of
  CONF confId pqSupport _ connInfo -> do
    -- Check if this is a relay test connection
    chatRelayTests_ <- asks chatRelayTests
    relayTest_ <- atomically $ TM.lookup agentConnId chatRelayTests_
    case relayTest_ of
      Just RelayTest {challenge, rootKey, result = testVar} -> do
        -- Parse response
        r <- tryAllErrors $ do
          ChatMessage {chatMsgEvent} <- parseChatMessage conn connInfo
          case chatMsgEvent of
            XGrpRelayTest _challenge sig_ ->
              case sig_ of
                Just sig
                  | C.verify' rootKey sig challenge ->
                      atomically $ putTMVar testVar Nothing  -- success
                  | otherwise ->
                      atomically $ putTMVar testVar (Just $ RelayTestFailure RTSVerify "invalid signature")
                Nothing ->
                  atomically $ putTMVar testVar (Just $ RelayTestFailure RTSVerify "no signature in response")
            _ ->
              atomically $ putTMVar testVar (Just $ RelayTestFailure RTSWaitResponse "unexpected message type")
        case r of
          Left e ->
            atomically $ putTMVar testVar (Just $ RelayTestFailure RTSWaitResponse (show e))
          Right () -> pure ()
      Nothing -> do
        -- Existing flow (unchanged)
        conn' <- processCONFpqSupport conn pqSupport
        (conn'', gInfo_) <- saveConnInfo conn' connInfo
        ...
```

Note: `agentConnId` is in scope from the `processAgentMessageConn` closure (Subscriber.hs:354).

#### Relay side: processContactConnMessage REQ handler

Add `XGrpRelayTest` case after `XGrpRelayInv` at line 1247:

```haskell
XGrpRelayTest challenge _ -> xGrpRelayTest invId chatVRange challenge
```

Add `xGrpRelayTest` function near `xGrpRelayInv` (~line 1450):

```haskell
xGrpRelayTest :: InvitationId -> VersionRangeChat -> ByteString -> CM ()
xGrpRelayTest invId chatVRange challenge = do
  -- Retrieve private key from address connection's short link creds, sign in chat layer
  privKey_ <- withAgent $ \a -> getConnLinkPrivKey a (aConnId conn)
  case privKey_ of
    Nothing -> eToView $ ChatError (CEInternalError "no short link key for relay address")
    Just privKey -> do
      let sig = C.sign' privKey challenge
          msg = XGrpRelayTest challenge (Just sig)
      subMode <- chatReadVar subscriptionMode
      vr <- chatVersionRange
      let chatV = vr `peerConnChatVersion` chatVRange
      void $ agentAcceptContactAsync user True invId msg subMode PQSupportOff chatV
```

Note: `conn` is the user contact address connection (from `processContactConnMessage` closure). Its `aConnId` is the agent `ConnId` that holds `ShortLinkCreds` with `linkPrivSigKey`. The agent returns `Maybe` — `Nothing` if the connection has no short link credentials (shouldn't happen for a properly configured relay, but handled gracefully — owner will timeout with `RTSWaitResponse`).

### Phase 6: Store — createRelayTestConnection

**File: `src/Simplex/Chat/Store/Direct.hs`**

Add function to create a ConnContact connection without entity:

```haskell
createRelayTestConnection :: DB.Connection -> VersionRangeChat -> User -> ConnId -> ConnStatus -> VersionChat -> SubscriptionMode -> ExceptT StoreError IO Connection
createRelayTestConnection db vr user@User {userId} agentConnId connStatus chatV subMode = do
  currentTs <- liftIO getCurrentTime
  liftIO $ DB.execute db
    [sql|
      INSERT INTO connections (
        user_id, agent_conn_id, conn_level, conn_status, conn_type,
        conn_chat_version, to_subscribe, pq_support, pq_encryption,
        created_at, updated_at
      ) VALUES (?,?,?,?,?,?,?,?,?,?,?)
    |]
    ( (userId, agentConnId, 0 :: Int, connStatus, ConnContact)
        :. (chatV, BI (subMode == SMOnlyCreate), PQSupportOff, PQSupportOff)
        :. (currentTs, currentTs)
    )
  connId <- liftIO $ insertedRowId db
  getConnectionById db vr user connId
```

Pattern: same as `createRelayConnection` (Store/Groups.hs:1388) but `ConnContact` type with no `group_member_id`.

The resulting row has `contact_id = NULL`, `contact_conn_initiated = 0` (column default), `xcontact_id = NULL`, `via_contact_uri = NULL`. This distinguishes it from `createConnReqConnection` rows which always set `contact_conn_initiated = 1`, `xcontact_id`, and `via_contact_uri`.

### Phase 7: APICreateMyAddress — Use RelayAddressLinkData

**File: `src/Simplex/Chat/Library/Commands.hs`**

Update `APICreateMyAddress` (~line 2162-2176) for relay users:

```haskell
-- Current code (line 2168-2169):
-- TODO [relays] relay: add relay profile, identity, key to link data?
let userData = contactShortLinkData (userProfileDirect user Nothing Nothing True) Nothing

-- New code for relay users:
let userData = if isTrue userChatRelay
      then encodeShortLinkData $ RelayAddressLinkData
        { relayProfile = RelayProfile {name = displayName (fromLocalProfile $ profile' user)}
        }
      else contactShortLinkData (userProfileDirect user Nothing Nothing True) Nothing
```

### Phase 8: Test connection cleanup

Test connections are `ConnContact` with no entity (`contact_id = NULL`). They should be cleaned up if the test API handler crashes or times out without cleanup.

Add `cleanupStaleRelayTestConns` step to `cleanupUser` in `cleanupManager` (after `cleanupInProgressGroups`, ~line 4500):

```haskell
cleanupStaleRelayTestConns user `catchAllErrors` eToView
liftIO $ threadDelay' stepDelay
```

Implementation:
```haskell
cleanupStaleRelayTestConns user = do
  ts <- liftIO getCurrentTime
  let cutoffTs = addUTCTime (-300) ts  -- 5 minutes
  staleConns <- withStore' $ \db -> getStaleRelayTestConns db user cutoffTs
  forM_ staleConns $ \acId -> do
    deleteAgentConnectionAsync acId
    withStore' $ \db -> deleteConnectionByAgentConnId db user acId
```

Where `getStaleRelayTestConns` queries:
```sql
SELECT agent_conn_id FROM connections
WHERE user_id = ? AND conn_type = 'contact' AND contact_id IS NULL
  AND conn_status = 'prepared' AND contact_conn_initiated = 0
  AND created_at < ?
```

This uniquely identifies stale test connections. The `contact_conn_initiated = 0` discriminator is critical because `createConnReqConnection` (Store/Direct.hs:164) also creates `ConnContact` rows with `contact_id = NULL` and `conn_status = ConnPrepared`, but it always sets `contact_conn_initiated = True` (line 175). Test connections from `createRelayTestConnection` inherit the column default of 0.

**No new DB column needed.**

### Phase 9: Views (iOS + Android/Desktop)

**iOS:**
- `apps/ios/Shared/Views/UserSettings/NetworkAndServers/ChatRelayView.swift`
- `apps/ios/Shared/Views/NewChat/AddChannelView.swift`

**Android/Desktop:**
- `apps/multiplatform/.../ChatRelayView.kt`
- `apps/multiplatform/.../AddChannelView.kt`

Changes:
1. Add "Test" button next to relay address that calls `APITestChatRelay address`
2. On success: show relay profile name, optionally auto-fill name field
3. On failure: show error description from `RelayTestFailure`
4. Show relay status indicator: untested / tested-ok / tested-failed

### Phase 10: Tests

**File: `tests/ChatTests/ChatRelays.hs`**

1. Add `testRelayChatRelayTest`:
   - Create relay user, set as chat relay, create address
   - Owner tests relay address → verify success with relay profile
   - Owner tests non-existent address → verify SMP error
   - Owner tests address of non-relay user → verify decode error (no RelayAddressLinkData)

2. Existing tests don't need changes — `name` field unchanged in DB.

---

## Files Modified

| File | Changes |
|------|---------|
| `src/Simplex/Chat/Protocol.hs` | `RelayProfile`, `RelayAddressLinkData`, `XGrpRelayTest` + tags + parsing + encoding |
| `src/Simplex/Chat/Controller.hs` | `RelayTestStep`, `RelayTestFailure`, `RelayTest`, `chatRelayTests`, `APITestChatRelay`, `CRChatRelayTestResult` |
| `src/Simplex/Chat.hs` | Initialize `chatRelayTests` in `newChatController` |
| `src/Simplex/Chat/Library/Commands.hs` | `APITestChatRelay` handler, `APICreateMyAddress` relay link data, CLI parsing, `cleanupManager` |
| `src/Simplex/Chat/Library/Subscriber.hs` | Owner CONF handler pre-check, relay REQ handler `XGrpRelayTest` |
| `src/Simplex/Chat/Store/Direct.hs` | `createRelayTestConnection` |
| `src/Simplex/Chat/Store/Profiles.hs` | `getStaleRelayTestConns` (for cleanup) |
| `apps/ios/.../ChatRelayView.swift` | Test button + result display |
| `apps/ios/.../AddChannelView.swift` | Test integration |
| `apps/multiplatform/.../ChatRelayView.kt` | Test button + result display |
| `apps/multiplatform/.../AddChannelView.kt` | Test integration |
| `tests/ChatTests/ChatRelays.hs` | `testRelayChatRelayTest` |

**Separate simplexmq change:**
| `simplexmq/src/Simplex/Messaging/Agent.hs` | `getConnLinkPrivKey` API |

---

## Key Functions Reused

- `getShortLinkConnReq` (Internal.hs:1339) — fetch link data + validate SMP + get connReq
- `decodeLinkUserData` (Internal.hs:1361) — decode `RelayAddressLinkData` from `ConnLinkData`
- `encodeShortLinkData` (Internal.hs:1351) — encode `RelayAddressLinkData` for link userData
- `prepareConnectionToJoin` (agent) — prepare agent connection for joining
- `joinConnection` (agent) — join relay's contact address
- `encodeConnInfo` (Internal.hs:1929) — encode `XGrpRelayTest` as connInfo
- `parseChatMessage` (Internal.hs:1563) — parse connInfo in CONF handler
- `agentAcceptContactAsync` (Internal.hs:2421) — relay accepts test connection
- `deleteAgentConnectionAsync` (Internal.hs:2428) — cleanup connections
- `deleteConnectionRecord` (Store/Shared.hs:895) — cleanup DB connection record (takes `Int64` DB connection_id)
- `getConnLinkPrivKey` (agent, new) — retrieve `linkPrivSigKey` from connection's short link creds
- `C.verify'` (simplexmq Crypto:1270) — `PublicKey a -> Signature a -> ByteString -> Bool`
- `C.sign'` (simplexmq Crypto:1175) — `PrivateKey a -> ByteString -> Signature a`
- `C.randomBytes` (simplexmq Crypto:1401) — `Int -> TVar ChaChaDRG -> STM ByteString`
- `eToView` (Controller.hs:1537) — `ChatError -> CM ()` — report error to view

---

## Verification

### Build
```bash
cabal build --ghc-options=-O0
```

### Test
```bash
cabal test simplex-chat-test --test-options='-m "channels"'
cabal test simplex-chat-test --test-options='-m "chat relays"'
```

### Manual verification
1. Start relay user, set as chat relay, create address
2. Start owner user
3. Owner tests relay address → verify CRChatRelayTestResult with profile, no failure
4. Owner tests invalid address → verify failure at RTSGetLink
5. Kill owner during test → verify cleanup by cleanupManager after 5 min

---

## Adversarial Self-Review

### Pass 1

**Issue: Signature type in JSON** — `C.Signature 'C.Ed25519` is a GADT constructor. Need to verify it has JSON/Encoding instances and can be transmitted in a JSON chat message.
**Analysis:** `Signature` has no native JSON instance. For JSON, encode as base64 ByteString using `B64UrlByteString . C.signatureBytes`. For parsing, decode `B64UrlByteString` then `C.decodeSignature :: ByteString -> Either String (Signature 'C.Ed25519)` (Crypto.hs:849). The `(.=?)` pattern handles `Maybe` — only included when `Just`.
**Fix:** Encoding uses `B64UrlByteString . C.signatureBytes <$> sig_`. Parsing uses `mapM decodeSig =<< opt "signature"` where `decodeSig (B64UrlByteString s) = C.decodeSignature s`. No relay profile in message — owner gets it from link data.

**Issue: `DuplicateRecordFields` on `connId`** — `connId :: Int64` appears on `Connection`, `PendingContactConnection`, and `UserContactRequest`. With `DuplicateRecordFields` enabled, `connId conn` won't compile as a field selector.
**Analysis:** Must use pattern matching. The handler uses `conn@Connection {connId = dbConnId}`.
**Fix:** Already applied in Phase 4 handler code.

**Issue: `getConnLinkPrivKey` conn access** — In `xGrpRelayTest`, we call `getConnLinkPrivKey a (aConnId conn)` where `conn` is the user contact address connection. Does the agent's `getConn` find it by the correct ConnId?
**Analysis:** `processContactConnMessage` receives `conn :: Connection` which is the chat-layer connection record. `aConnId conn` gives the agent's `ConnId`. The agent stores `ShortLinkCreds` on the `RcvQueue` of the `ContactConnection` for this `ConnId`. The agent function pattern-matches on `ContactConnection _ rq` and returns `linkPrivSigKey <$> shortLink rq`. This is correct.
**Fix:** No fix needed.

**Issue: `getConnLinkPrivKey` returns Nothing** — If the relay's address connection has no short link credentials, the relay-side handler logs an error via `eToView` and does not accept the test connection.
**Analysis:** This shouldn't happen for a properly configured relay (creating the address creates short link creds via `createConnection` in the agent). Handled gracefully — the owner will timeout with `RTSWaitResponse`.
**Fix:** No fix needed.

**Issue: Test connection routing on relay side** — After the relay accepts the test via `agentAcceptContactAsync`, the agent creates a new connection. Future events on this connection (JOINED, etc.) arrive at `processAgentMessageConn`. Since there's no DB connection record, `getConnectionEntity` will fail with `SEConnectionNotFound`, producing error in `eToView`. This is log noise.
**Analysis:** Acceptable for MVP. The agent will eventually GC the connection. The error is harmless and happens for the relay only. The owner's connection is cleaned up by the handler.
**Fix:** Document as known behavior.

**Issue: `tryAllErrors` behavior** — Does `tryAllErrors` catch all exceptions or just `ChatError`?
**Analysis:** `tryAllErrors` (Util.hs:249) uses `UE.catch` which catches `SomeException` — ALL exceptions, not just `ChatError`. It converts via `fromSomeException` into the error type. This is important: if `joinConnection` throws an IO exception, it's still caught and the cleanup runs.
**Fix:** No fix needed — the behavior is correct.

**Issue: Multiple CONFs** — Could the owner receive multiple CONF events for the same connection? If yes, the second `putTMVar` would block.
**Analysis:** The SMP protocol sends exactly one CONF per connection. Multiple CONFs would be a protocol violation.
**Fix:** No fix needed.

**Issue: Cleanup on timeout** — If the timeout fires (40s), the handler deletes the DB connection and agent connection. But the relay's response might arrive AFTER cleanup.
**Analysis:** After timeout, the TMap entry is deleted. A late CONF arriving at the subscriber finds no TMap entry, falls through to the existing flow, fails at `getConnectionEntity` (connection deleted). Harmless — `catchAllErrors eToView` absorbs it.
**Fix:** No fix needed. The cleanup sequence (delete TMap → delete DB → delete agent) is safe in all interleavings.

### Pass 2

**Issue: `decodeLinkUserData cData`** — For relay addresses, `cData` is `ContactLinkData vr UserContactData{..}`. Does `decodeLinkUserData` decode the right field?
**Analysis:** `decodeLinkUserData` (Internal.hs:1361) is polymorphic — uses `JQ.decode` on the `userData` bytes from `UserContactData`. The caller constrains the type via the binding `Just RelayAddressLinkData {relayProfile}`. The `FromJSON` instance is provided by `deriveJSON`.
**Fix:** No fix needed.

**Issue: `encodeShortLinkData`** — Will it work for `RelayAddressLinkData`?
**Analysis:** `encodeShortLinkData` (Internal.hs:1351) is polymorphic — `J.ToJSON a => a -> UserLinkData`. Uses `J.encode` and wraps in `UserLinkData`. Works for any type with `ToJSON`.
**Fix:** No fix needed.

**Issue: Cleanup identification query safety** — `getStaleRelayTestConns` uses: `ConnContact + contact_id IS NULL + ConnPrepared + contact_conn_initiated = 0 + old created_at`. Could this match non-test connections?
**Analysis:** All code paths that create `ConnContact` with `contact_id = NULL`:
- `createConnReqConnection` (Direct.hs:158): sets `ConnPrepared` (line 164) BUT also sets `contact_conn_initiated = True` (line 175, `BI True`), `xcontact_id`, and `via_contact_uri`. The `contact_conn_initiated = 0` condition excludes these.
- `createRelayTestConnection` (new): sets `ConnPrepared`, inherits `contact_conn_initiated = 0` default. Matches the query.
- No other code path creates `ConnContact` with `contact_id = NULL` and `contact_conn_initiated = 0`.
**Fix:** The query is safe with the `contact_conn_initiated = 0` discriminator.

**Issue: Partial failure cleanup** — If `prepareConnectionToJoin` succeeds but the `withFastStore` for `createRelayTestConnection` fails, the agent connection leaks.
**Analysis:** The `prepareConnectionToJoin` call happens before the `tryAllErrors` block. If `createRelayTestConnection` throws, we never reach cleanup. The agent connection from `prepareConnectionToJoin` would leak until restart. However, `createRelayTestConnection` is a simple INSERT — it's unlikely to fail. And if it does, `cleanupManager` won't catch it because no DB row was created. The agent-level connection will be cleaned up on agent restart.
**Fix:** Acceptable for MVP. Could wrap in a broader try-catch, but the failure mode is extremely unlikely and the consequence (one leaked agent connection) is minor.

**Issue: `void $ withAgent $ \a -> joinConnection ...`** — The return type of `joinConnection` is `AE (SndQueueSecured, Maybe ClientServiceId)`. Using `void` discards both values.
**Analysis:** For the test connection, we don't need `SndQueueSecured` or `ClientServiceId`. The `addRelay` function (Commands.hs:3776) uses the return value to update connection status, but the test connection is deleted immediately anyway.
**Fix:** No fix needed.

Both passes clean. No further issues found.
