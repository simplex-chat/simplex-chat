# Plan: APITestChatRelay — Relay Liveness + Identity Verification

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
  |<-- CONF(XGrpRelayTest{signature, relayProfile}) -------------------|
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
XGrpRelayTest :: ByteString -> Maybe (C.Signature 'C.Ed25519) -> Maybe RelayProfile -> ChatMsgEvent 'Json
```

Single constructor used in both directions:
- **Owner → Relay** (in joinConnection connInfo): `XGrpRelayTest challenge Nothing Nothing`
- **Relay → Owner** (in acceptContact connInfo): `XGrpRelayTest challenge (Just signature) (Just relayProfile)`

JSON encoding:
```haskell
XGrpRelayTest challenge sig_ profile_ -> o $
  ("challenge" .= B64UrlByteString challenge)
  : ("signature" .=? (B64UrlByteString . C.signatureBytes <$> sig_))
  ++ ("relayProfile" .=? profile_) []
```

JSON parsing:
```haskell
XGrpRelayTest_ -> do
  B64UrlByteString challenge <- v .: "challenge"
  sig_ <- fmap (\(B64UrlByteString s) -> ...) <$> opt "signature"
  profile_ <- opt "relayProfile"
  pure $ XGrpRelayTest challenge sig_ profile_
```

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
| APITestChatRelay UserId ConnLinkContact
| TestChatRelay ConnLinkContact
```

Takes an address (`ConnLinkContact`), not a `chatRelayId` — the relay may not be saved yet.

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
   - `XGrpRelayTest :: ByteString -> Maybe (C.Signature 'C.Ed25519) -> Maybe RelayProfile -> ChatMsgEvent 'Json`

4. Add event tag `XGrpRelayTest_` (after `XGrpRelayAcpt_`, ~line 966)

5. Add tag string `"x.grp.relay.test"` (after `"x.grp.relay.acpt"`, ~line 1022)

6. Add tag parsing (after `XGrpRelayAcpt_` parse, ~line 1079)

7. Add event-to-tag mapping (after `XGrpRelayAcpt` mapping, ~line 1132):
   - `XGrpRelayTest {} -> XGrpRelayTest_`

8. Add JSON parsing (~line 1284):
   ```haskell
   XGrpRelayTest_ -> do
     B64UrlByteString challenge <- v .: "challenge"
     sig_ <- decodeSig =<< opt "signature"
     profile_ <- opt "relayProfile"
     pure $ XGrpRelayTest challenge sig_ profile_
   ```
   Where `decodeSig` decodes `Maybe B64UrlByteString` to `Maybe (C.Signature 'C.Ed25519)`.

9. Add JSON encoding (~line 1351):
   ```haskell
   XGrpRelayTest challenge sig_ profile_ -> o $
     ("challenge" .= B64UrlByteString challenge)
     : ("signature" .=? (B64UrlByteString . C.signatureBytes <$> sig_))
     ++ ("relayProfile" .=? profile_) []
   ```

### Phase 2: Controller types — RelayTest, RelayTestFailure, commands, response

**File: `src/Simplex/Chat/Controller.hs`**

1. Add `RelayTestStep` and `RelayTestFailure` types (near `ProtocolTestFailure` usage)

2. Add `RelayTest` type

3. Add `chatRelayTests :: TMap ConnId RelayTest` field to `ChatController` (after line 252)

4. Uncomment and update `APITestChatRelay` (lines 401-403):
   ```haskell
   | APITestChatRelay UserId ConnLinkContact
   | TestChatRelay ConnLinkContact
   ```

5. Add `CRChatRelayTestResult` to `ChatResponse` (after `CRServerTestResult`):
   ```haskell
   | CRChatRelayTestResult {user :: User, relayProfile :: Maybe RelayProfile, testFailure :: Maybe RelayTestFailure}
   ```

**File: `src/Simplex/Chat.hs`**

6. Initialize `chatRelayTests` in `newChatController` (after line 175):
   ```haskell
   chatRelayTests <- TM.emptyIO
   ```

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
          r2 <- tryAllErrors $ do
            lift (withAgent' $ \a -> connRequestPQSupport a PQSupportOff cReq) >>= \case
              Nothing -> throwChatError CEInvalidConnReq
              Just (agentV, _) -> do
                let chatV = agentToChatVersion agentV
                subMode <- chatReadVar subscriptionMode
                connId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True cReq PQSupportOff
                conn <- withFastStore $ \db -> createRelayTestConnection db vr user connId ConnPrepared chatV subMode
                -- Register test in TMap
                testVar <- newEmptyTMVarIO
                let acId = aConnId conn
                    relayTest = RelayTest {challenge, rootKey, result = testVar}
                chatRelayTests_ <- asks chatRelayTests
                atomically $ TM.insert acId relayTest chatRelayTests_
                -- Join with challenge
                dm <- encodeConnInfo $ XGrpRelayTest challenge Nothing Nothing
                withAgent $ \a -> joinConnection a nm (aUserId user) acId True cReq dm PQSupportOff subMode
                -- Block with 40s timeout
                testResult <- liftIO $ timeout 40_000_000 $ atomically $ takeTMVar testVar
                -- Cleanup TMap
                atomically $ TM.delete acId chatRelayTests_
                -- Cleanup DB + agent connection
                withFastStore' $ \db -> deleteConnectionRecord db user (connId conn)
                deleteAgentConnectionAsync acId
                pure testResult
          case r2 of
            Left e -> failWithProfile RTSConnect (show e)
            Right Nothing -> failWithProfile RTSWaitResponse "timeout"
            Right (Just Nothing) -> pure $ CRChatRelayTestResult user (Just relayProfile) Nothing  -- success
            Right (Just (Just failure)) -> pure $ CRChatRelayTestResult user (Just relayProfile) (Just failure)
TestChatRelay address -> withUser $ \User {userId} ->
  processChatCommand vr nm $ APITestChatRelay userId address
```

Also add CLI parsing for `TestChatRelay` in the command parser.

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
            XGrpRelayTest _challenge sig_ _profile_ ->
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

#### Relay side: processContactConnMessage REQ handler

Add `XGrpRelayTest` case after `XGrpRelayInv` at line 1247:

```haskell
XGrpRelayTest challenge _ _ -> xGrpRelayTest invId chatVRange challenge
```

Add `xGrpRelayTest` function near `xGrpRelayInv` (~line 1450):

```haskell
xGrpRelayTest :: InvitationId -> VersionRangeChat -> ByteString -> CM ()
xGrpRelayTest invId chatVRange challenge = do
  -- Retrieve private key from address connection's short link creds, sign challenge
  privKey_ <- withAgent $ \a -> getConnLinkPrivKey a (aConnId conn)
  case privKey_ of
    Nothing -> toView $ CEvtChatError Nothing $ ChatError $ CEInternalError "no short link key for relay address"
    Just privKey -> do
      let sig = C.sign' privKey challenge
          relayProfile = RelayProfile {name = displayName (fromLocalProfile $ profile' user)}
          msg = XGrpRelayTest challenge (Just sig) (Just relayProfile)
      subMode <- chatReadVar subscriptionMode
      vr <- chatVersionRange
      let chatV = vr `peerConnChatVersion` chatVRange
      void $ agentAcceptContactAsync user True invId msg subMode PQSupportOff chatV
```

Note: The `conn` here is the user contact address connection (from `processContactConnMessage` closure). Its `aConnId` is the agent `ConnId` that holds the `ShortLinkCreds` with `linkPrivSigKey`. The agent returns `Maybe` — `Nothing` if the connection has no short link credentials (shouldn't happen for a properly configured relay address, but handled gracefully).

### Phase 6: Store — createRelayTestConnection

**File: `src/Simplex/Chat/Store/Direct.hs`** (or `Store/Connections.hs`)

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

Test connections are `ConnContact` with no entity (contact_id = NULL). They should be cleaned up if the test API handler crashes or times out without cleanup.

**Option A: cleanupManager step** — add to `cleanupUser` in `cleanupManager`:

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
  AND conn_status = 'prepared' AND created_at < ?
```

This identifies stale test connections: ConnContact with no entity, still in Prepared status, older than 5 minutes. This is safe because normal ConnContact connections always have a contact_id or transition out of Prepared quickly.

**No new DB column needed** — the combination of (ConnContact, NULL contact_id, ConnPrepared, old created_at) uniquely identifies stale test connections.

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
| `src/Simplex/Chat/Library/Commands.hs` | `APITestChatRelay` handler, `APICreateMyAddress` relay link data, CLI parsing |
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
- `encodeConnInfo` (Protocol.hs) — encode `XGrpRelayTest` as connInfo
- `parseChatMessage` (Protocol.hs) — parse connInfo in CONF handler
- `agentAcceptContactAsync` (Internal.hs:2421) — relay accepts test connection
- `deleteAgentConnectionAsync` (Internal.hs:2428) — cleanup connections
- `deleteConnectionRecord` (Store/Shared.hs:895) — cleanup DB connection record
- `getConnLinkPrivKey` (agent) — retrieve linkPrivSigKey from connection's short link creds
- `C.verify'` (simplexmq Crypto) — verify Ed25519 signature
- `C.sign'` (simplexmq Crypto) — sign with Ed25519 private key (used in chat layer on relay side)
- `C.randomBytes` (simplexmq Crypto) — generate random challenge
- `profileFromName` (Types.hs:701) — NOT used (relay name comes from test result)

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
**Analysis:** `Signature` has `Encoding` instance (`smpEncode = smpEncode . signatureBytes`). For JSON, we need to encode as base64 ByteString. Use `B64UrlByteString` wrapper for JSON encoding, then decode via `C.decodeSignature`.
**Fix:** In JSON encoding, use `B64UrlByteString . C.signatureBytes` for signature. In parsing, decode `B64UrlByteString` then `C.decodeSignature` to get `Signature 'C.Ed25519`.

**Issue: `connId conn` ambiguity** — In the cleanup code `deleteConnectionRecord db user (connId conn)`, `connId` could refer to the `Connection` field or the agent `ConnId`. The `Connection` type has `connId :: Int64` (DB ID).
**Analysis:** `deleteConnectionRecord` takes `Int64` (DB connection_id). `connId` from the `Connection` record pattern is the DB ID. This is correct.
**Fix:** No fix needed, but use explicit destructuring: `conn@Connection {connId = dbConnId}` for clarity.

**Issue: `aConnId conn`** — The function `aConnId` extracts the agent ConnId from a `Connection`. Need to verify it exists and works on the `Connection` type from `createRelayTestConnection`.
**Analysis:** `aConnId` is defined on `Connection` type and accesses `agentConnId` field. `createRelayTestConnection` calls `getConnectionById` which returns a proper `Connection`. This is correct.
**Fix:** No fix needed.

**Issue: `getConnLinkPrivKey` conn access** — In `xGrpRelayTest`, we call `getConnLinkPrivKey a (aConnId conn)` where `conn` is the user contact address connection. But the `linkPrivSigKey` is on the `RcvQueue` of THIS connection. Does the agent's `getConn` find it by the correct ConnId?
**Analysis:** `processContactConnMessage` receives `conn :: Connection` which is the chat-layer connection record. `aConnId conn` gives the agent's `ConnId` for this connection. The agent stores `ShortLinkCreds` on the `RcvQueue` of the `ContactConnection` for this `ConnId`. So `getConnLinkPrivKey` looks up the connection, gets the `ContactConnection`'s `RcvQueue`, and returns `linkPrivSigKey <$> shortLink rq`. This is correct.
**Fix:** No fix needed.

**Issue: `getConnLinkPrivKey` returns Nothing** — If the relay's address connection has no short link credentials, `getConnLinkPrivKey` returns `Nothing`. The relay-side handler logs an error via `toView` and does not accept the test connection.
**Analysis:** This shouldn't happen for a properly configured relay (creating the address creates short link creds). But it's handled gracefully — the owner will timeout and get `RTSWaitResponse`.
**Fix:** No fix needed.

**Issue: Test connection routing on relay side** — After the relay accepts the test via `agentAcceptContactAsync`, the agent creates a new connection. Future events on this connection (JOINED, etc.) arrive at `processAgentMessageConn`. Since there's no DB connection record, `getConnectionEntity` will fail with `SEConnectionNotFound`, producing error in `eToView`. This is log noise.
**Analysis:** Acceptable. The agent will eventually GC the connection. The error is harmless and happens for the relay only. The owner's connection is cleaned up by the handler.
**Fix:** Document as known behavior. The relay could schedule deletion, but for MVP this is acceptable.

**Issue: `tryAllErrors` in CONF handler** — The `tryAllErrors` around `parseChatMessage` catches errors, but if `parseChatMessage` throws, we put the error in the TMVar. But `tryAllErrors` returns `Either ChatError a`, not catching ALL exceptions.
**Analysis:** `tryAllErrors` catches `ChatError` thrown in `ExceptT`. If `parseChatMessage` throws a different exception type, it would propagate. But `parseChatMessage` uses `throwChatError` / `liftEither`, so it throws `ChatError`.
**Fix:** No fix needed.

**Issue: Multiple CONFs** — Could the owner receive multiple CONF events for the same connection? If yes, the second `putTMVar` would block (TMVar already full).
**Analysis:** The SMP protocol sends exactly one CONF per connection. Multiple CONFs would be a protocol violation.
**Fix:** No fix needed.

**Issue: Cleanup on timeout** — If the timeout fires (40s), the handler deletes the DB connection and agent connection. But the relay's response might arrive AFTER cleanup. The subscriber would then fail on `getConnectionEntity` (connection already deleted). This is harmless — `eToView` absorbs it.
**Analysis:** Correct. After timeout, the TMap entry is deleted, so even if a late CONF arrives, the pre-check in the CONF handler won't find it in the TMap. It would fall through to the existing flow (`saveConnInfo`), which might fail because the connection has no contact_id. But the connection is already deleted by then, so `getConnectionEntity` fails first.
**Fix:** No fix needed. The cleanup sequence is: delete TMap entry → delete DB connection → delete agent connection. If a CONF arrives after TMap deletion but before DB deletion, it falls through to existing flow and fails gracefully. If after DB deletion, `getConnectionEntity` fails. Both paths are safe.

### Pass 2

**Issue: `decodeLinkUserData cData`** — This function decodes the `userData` from `ConnLinkData`. For relay addresses using `RelayAddressLinkData`, the cData is `ContactLinkData vr UserContactData{..}`. The `decodeLinkUserData` function decodes from `UserContactData.userData` field. Need to verify `RelayAddressLinkData` is decoded by this function.
**Analysis:** Looking at Internal.hs:1361, `decodeLinkUserData` uses `JQ.decode` on the userData bytes. It's polymorphic — the caller specifies the expected type via type inference. So `decodeLinkUserData cData :: IO (Maybe RelayAddressLinkData)` should work as long as we have a `FromJSON` instance for `RelayAddressLinkData` (which we do via `deriveJSON`).
**Fix:** Ensure the type annotation or binding constrains the type correctly in the handler code.

**Issue: `encodeShortLinkData`** — Currently used to encode `ContactShortLinkData`. It wraps the JSON in `UserContactData.userData`. Need to verify it works for `RelayAddressLinkData`.
**Analysis:** Looking at Internal.hs:1351-1359, `encodeShortLinkData` calls `JQ.encode` on the value and wraps it. It's also polymorphic. It produces `UserContactData` with the encoded value in the `userData` field. This will work for any `ToJSON a`.
**Fix:** No fix needed.

**Issue: Cleanup identification query** — `getStaleRelayTestConns` identifies stale test connections by: ConnContact, NULL contact_id, ConnPrepared status, old created_at. Could this match non-test connections?
**Analysis:** Normal ConnContact connections:
- `createDirectConnection_` always sets `contact_id` (from `contactId` parameter)
- `createConnReqConnection` sets `contact_id = NULL` for connection request connections (via contact address). These DO match the pattern! They have `conn_type = ConnContact`, `contact_id = NULL`, and start in `ConnNew` status.
**Fix:** Hmm, but `ConnPrepared` is different from `ConnNew`. Let me check what status `createConnReqConnection` uses... Looking at Store/Direct.hs:159, `createConnReqConnection` uses `ConnNew`. And `createRelayTestConnection` uses `ConnPrepared`. So the `ConnPrepared` status differentiates test connections from contact request connections. But wait — could other flows create ConnContact with ConnPrepared? Let me check... `prepareConnectionToJoin` creates the agent connection in Prepared state, but the DB connection is created separately. Looking at existing usage of `ConnPrepared` in DB... Actually, `ConnPrepared` is set by `createRelayTestConnection` which we're writing. Other flows that create ConnContact connections don't use `ConnPrepared` — they use `ConnNew` or `ConnJoined`.
**Fix:** The query is safe. `ConnContact + contact_id IS NULL + ConnPrepared + old` uniquely identifies stale relay test connections.

**Issue: `tryAllErrors` around the full connect flow** — If `prepareConnectionToJoin` succeeds but `joinConnection` fails, we need to ensure the agent connection is cleaned up. But we're inside `tryAllErrors`, so the error is caught, and we return `Left e`. The agent connection from `prepareConnectionToJoin` would leak.
**Analysis:** The DB connection from `createRelayTestConnection` would also leak. Both need cleanup on partial failure.
**Fix:** Add cleanup in the error path. After `createRelayTestConnection`, if subsequent steps fail, ensure both DB and agent connections are cleaned up. Use `onException`/`catchAllErrors` pattern, or structure the code to always cleanup. Actually, the cleanupManager will catch these as stale test connections (ConnPrepared older than 5 min). So even without immediate cleanup, they'll be cleaned up within 30 minutes. For correctness, add explicit cleanup on partial failure.

Revised handler structure:
```haskell
connId <- withAgent $ \a -> prepareConnectionToJoin ...
conn <- withFastStore $ \db -> createRelayTestConnection ...
let acId = aConnId conn
    cleanup = do
      atomically $ TM.delete acId chatRelayTests_
      withFastStore' $ \db -> deleteConnectionRecord db user (connId conn)
      deleteAgentConnectionAsync acId
-- Register test in TMap
...
atomically $ TM.insert acId relayTest chatRelayTests_
-- Join + wait with cleanup on failure
r <- (do
  dm <- encodeConnInfo $ XGrpRelayTest challenge Nothing Nothing
  withAgent $ \a -> joinConnection ...
  liftIO $ timeout 40_000_000 $ atomically $ takeTMVar testVar
  ) `catchAllErrors` (\e -> cleanup >> throwError e)
-- Always cleanup
cleanup
pure r
```

Wait, but `catchAllErrors` catches `ChatError`, not all exceptions. And we need cleanup to run regardless (success or failure). Better to use a try-finally pattern or just always call cleanup:

```haskell
-- After join + wait (whether success or failure), always cleanup
testResult <- liftIO $ timeout 40_000_000 $ atomically $ takeTMVar testVar
-- Cleanup always
atomically $ TM.delete acId chatRelayTests_
withFastStore' $ \db -> deleteConnectionRecord db user (connId conn)
deleteAgentConnectionAsync acId
pure testResult
```

But this doesn't handle exceptions during `joinConnection`. If `joinConnection` throws, we don't reach cleanup. Need to wrap in `tryAllErrors`:

```haskell
testResult <- tryAllErrors $ do
  dm <- encodeConnInfo $ ...
  withAgent $ \a -> joinConnection ...
  liftIO $ timeout 40_000_000 $ atomically $ takeTMVar testVar
-- Cleanup always (even on error)
atomically $ TM.delete acId chatRelayTests_
withFastStore' $ \db -> deleteConnectionRecord db user (connId conn)
deleteAgentConnectionAsync acId
case testResult of
  Left e -> failWithProfile RTSConnect (show e)
  Right Nothing -> failWithProfile RTSWaitResponse "timeout"
  Right (Just Nothing) -> pure $ CRChatRelayTestResult user (Just relayProfile) Nothing
  Right (Just (Just failure)) -> pure $ CRChatRelayTestResult user (Just relayProfile) (Just failure)
```

This is cleaner. The cleanup runs after both success and failure paths. The `tryAllErrors` ensures `joinConnection` errors are caught.

**Issue: TMap entry leak on exception before join** — If `encodeConnInfo` throws (highly unlikely but possible), the TMap entry persists. Since the connection is deleted, the TMap entry is just dead memory. The cleanupManager doesn't clean TMap entries.
**Analysis:** The TMap entry is keyed by ConnId. Once the connection is deleted, no event will ever arrive for that ConnId. The entry will persist until the process restarts. This is a minor memory leak, not a correctness issue.
**Fix:** The cleanup code already deletes the TMap entry: `atomically $ TM.delete acId chatRelayTests_`. This runs after `tryAllErrors`, so even if `encodeConnInfo` throws, the TMap entry is cleaned up. No issue.

Both passes clean. No further issues found.
