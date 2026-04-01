# Plan: APITestChatRelay — Relay Liveness Test + Profile Retrieval

## Context

Channel owners configure relays via `UserChatRelay` records but currently cannot verify relay liveness or obtain relay profiles before creating channels. The `name` field on relay config is a user-supplied string used only to generate placeholder profiles (`profileFromName name`). Once we retrieve the relay's actual profile via testing, `name` becomes redundant.

**This change solves two problems:**
1. No way to verify a relay is alive and functional before creating a channel
2. Relay member profiles are placeholders, not the relay's actual profile

**Outcome:** `APITestChatRelay userId chatRelayId` connects to the relay via `XGrpRelayTest` handshake, verifies liveness, retrieves the relay's profile, and saves it on the config record for later use when creating channels.

---

## Implementation Plan

### Phase 1: Protocol — XGrpRelayTest + RelayAddressData

**File: `src/Simplex/Chat/Protocol.hs`**

1. Add `XGrpRelayTest` constructor (after line 438, next to XGrpRelayAcpt):
   ```haskell
   XGrpRelayTest :: Maybe Profile -> ChatMsgEvent 'Json
   ```
   - Owner → Relay: `XGrpRelayTest Nothing` (signals "this is a test")
   - Relay → Owner: `XGrpRelayTest (Just profile)` (carries relay's current profile)

2. Add event tag `XGrpRelayTest_` (after line 966)

3. Add tag string `"x.grp.relay.test"` (after line 1022)

4. Add tag parsing (after line 1079)

5. Add event-to-tag mapping `XGrpRelayTest _ -> XGrpRelayTest_` (after line 1132)

6. Add JSON parsing (after line 1284):
   ```haskell
   XGrpRelayTest_ -> XGrpRelayTest <$> opt "profile"
   ```

7. Add JSON encoding (after line 1351):
   ```haskell
   XGrpRelayTest profile -> o $ ("profile" .=? profile) []
   ```

8. Add `RelayAddressData` type (after line 1444, next to RelayShortLinkData):
   ```haskell
   data RelayAddressData = RelayAddressData
     { relayProfile :: Profile
     }
     deriving (Show)

   $(JQ.deriveJSON defaultJSON ''RelayAddressData)
   ```

### Phase 2: DB Migration — Add profile, remove name

**New migration file: `src/Simplex/Chat/Store/SQLite/Migrations/M20260401_relay_test.hs`**
**New migration file: `src/Simplex/Chat/Store/Postgres/Migrations/M20260401_relay_test.hs`**

Migration SQL:
```sql
ALTER TABLE chat_relays ADD COLUMN contact_profile_id INTEGER REFERENCES contact_profiles ON DELETE SET NULL;
ALTER TABLE chat_relays ADD COLUMN test_conn_agent_id TEXT;
DROP INDEX idx_chat_relays_user_id_name;
ALTER TABLE chat_relays DROP COLUMN name;
```

- `contact_profile_id` — FK to `contact_profiles`, stores relay profile retrieved via test
- `test_conn_agent_id` — agent connection ID of in-progress test (for crash cleanup)
- Drop `name` column and its UNIQUE index

Register in both `Migrations.hs` files and add to `simplex-chat.cabal`.

### Phase 3: Types — UserChatRelay, CLINewRelay, presets

**File: `src/Simplex/Chat/Operators.hs`**

1. Update `UserChatRelay'` (line 263-272):
   - Remove `name :: Text`
   - Add `relayProfile :: Maybe Profile`

2. Update `CLINewRelay` (line 287-291):
   - Remove `name :: Text` — relay is identified by address only

3. Update `presetChatRelay` (line 333) and `newChatRelay_` (line 341-343):
   - Remove `name` parameter
   - Add optional `Maybe Profile` parameter for preset relay profiles

4. Update `chatRelayAddress` accessor (used in relay matching):
   - Already exists, no change needed

**File: `src/Simplex/Chat/Operators/Presets.hs`**

5. Update `simplexChatRelays` (line 92-97):
   - Remove name strings ("chat_relay_1", etc.)
   - Optionally include preset profiles (can be `Nothing` — filled by testing)

### Phase 4: Store Operations — Profile save/load

**File: `src/Simplex/Chat/Store/Profiles.hs`**

1. Update `toChatRelay` (line 635-637):
   - Remove `name` field, add profile join/load

2. Update `getChatRelays` / `getChatRelayById` queries (lines 623-649):
   - Remove `name` from SELECT
   - JOIN `contact_profiles` on `contact_profile_id` to load relay profile
   - Or load profile separately via `contact_profile_id`

3. Update `insertChatRelay` (line 651-664):
   - Remove `name` from INSERT
   - Include `contact_profile_id` if profile is provided

4. Update `updateChatRelay` (line 666-676):
   - Remove `name` from UPDATE

5. Add new store operations:
   ```haskell
   updateChatRelayProfile :: DB.Connection -> User -> Int64 -> Profile -> IO ProfileId
   -- Creates/updates profile in contact_profiles, sets contact_profile_id on chat_relays

   updateChatRelayTested :: DB.Connection -> Int64 -> Maybe Bool -> IO ()
   -- Sets tested flag on chat_relays

   setChatRelayTestConn :: DB.Connection -> Int64 -> Maybe ConnId -> IO ()
   -- Sets/clears test_conn_agent_id for crash cleanup

   getStaleRelayTestConns :: DB.Connection -> IO [(Int64, ConnId)]
   -- Returns (chatRelayId, agentConnId) for crash cleanup
   ```

**File: `src/Simplex/Chat/Store/Connections.hs`** (or Store/Direct.hs)

6. Add `createRelayTestConnection` — creates ConnContact without entity for test routing:
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
   Pattern: same as `createRelayConnection` (Store/Groups.hs:1388) but `ConnContact` type, no `group_member_id`.

7. Update `toGroupRelay` (Store/Groups.hs line 1328-1331):
   - Remove `name` from tuple destructuring

### Phase 5: Controller — API command, response, TMVar state

**File: `src/Simplex/Chat/Controller.hs`**

1. Uncomment and update `APITestChatRelay` (line 401-403):
   ```haskell
   | APITestChatRelay UserId Int64  -- chatRelayId
   | TestChatRelay Int64
   ```

2. Add `CRChatRelayTestResult` response (after line 667):
   ```haskell
   | CRChatRelayTestResult {user :: User, chatRelay :: UserChatRelay, testFailure :: Maybe String}
   ```

3. Add `chatRelayTests` field to `ChatController` (after line 252):
   ```haskell
   chatRelayTests :: TMap ConnId (TMVar (Either ChatError Profile)),
   ```

**File: `src/Simplex/Chat.hs`**

4. Initialize `chatRelayTests` in `newChatController` (after line 175):
   ```haskell
   chatRelayTests <- TM.emptyIO
   ```

### Phase 6: Commands.hs — APITestChatRelay handler

**File: `src/Simplex/Chat/Library/Commands.hs`**

1. Add `APITestChatRelay` handler (after `TestProtoServer`, around line 1491):
   ```haskell
   APITestChatRelay userId chatRelayId -> withUserId userId $ \user -> do
     relay@UserChatRelay {address} <- withFastStore $ \db -> getChatRelayById db user chatRelayId
     -- Guard: reject if test already in progress
     -- Step 1: Fetch relay address data (validates SMP server + link integrity + gets connReq)
     (FixedLinkData {linkConnReq = cReq}, _cData) <- getShortLinkConnReq nm user address
     -- Step 2: Prepare connection and join with XGrpRelayTest
     lift (withAgent' $ \a -> connRequestPQSupport a PQSupportOff cReq) >>= \case
       Nothing -> throwChatError CEInvalidConnReq
       Just (agentV, _) -> do
         let chatV = agentToChatVersion agentV
         subMode <- chatReadVar subscriptionMode
         connId <- withAgent $ \a -> prepareConnectionToJoin a (aUserId user) True cReq PQSupportOff
         -- Create ConnContact DB record (without entity) for CONF routing
         conn <- withFastStore $ \db -> createRelayTestConnection db vr user connId ConnPrepared chatV subMode
         -- Store TMVar for blocking + record test conn for crash cleanup
         testVar <- newEmptyTMVarIO
         relayTests <- asks chatRelayTests
         let acId = aConnId conn
         atomically $ TM.insert acId testVar relayTests
         withFastStore' $ \db -> setChatRelayTestConn db chatRelayId (Just acId)
         -- Join connection with XGrpRelayTest
         dm <- encodeConnInfo $ XGrpRelayTest Nothing
         withAgent $ \a -> joinConnection a nm (aUserId user) acId True cReq dm PQSupportOff subMode
         -- Block on TMVar with timeout (30s)
         result <- liftIO $ timeout 30_000_000 $ atomically $ takeTMVar testVar
         -- Cleanup: TMap entry, test conn marker, DB+agent connection
         atomically $ TM.delete acId relayTests
         withFastStore' $ \db -> do
           setChatRelayTestConn db chatRelayId Nothing
           deleteConnectionRecord db user (connId conn)
         deleteAgentConnectionAsync acId
         -- Process result
         case result of
           Nothing -> do  -- timeout
             withFastStore' $ \db -> updateChatRelayTested db chatRelayId (Just False)
             relay' <- withFastStore $ \db -> getChatRelayById db user chatRelayId
             pure $ CRChatRelayTestResult user relay' (Just "timeout")
           Just (Left err) -> do
             withFastStore' $ \db -> updateChatRelayTested db chatRelayId (Just False)
             relay' <- withFastStore $ \db -> getChatRelayById db user chatRelayId
             pure $ CRChatRelayTestResult user relay' (Just $ show err)
           Just (Right profile) -> do
             withFastStore' $ \db -> do
               void $ updateChatRelayProfile db user chatRelayId profile
               updateChatRelayTested db chatRelayId (Just True)
             relay' <- withFastStore $ \db -> getChatRelayById db user chatRelayId
             pure $ CRChatRelayTestResult user relay' Nothing
   TestChatRelay chatRelayId -> withUser $ \User {userId} ->
     processChatCommand vr nm $ APITestChatRelay userId chatRelayId
   ```

2. Add CLI parsing for `TestChatRelay` in the command parser.

3. Update `APICreateMyAddress` (line 2162-2176):
   - For relay users (`isTrue userChatRelay`), use `RelayAddressData` instead of `ContactShortLinkData`:
     ```haskell
     let userData = if isTrue userChatRelay
           then encodeShortLinkData $ RelayAddressData {relayProfile = userProfileDirect user Nothing Nothing True}
           else contactShortLinkData (userProfileDirect user Nothing Nothing True) Nothing
     ```

4. Update `addRelay` (line 3747-3780):
   - Use relay profile from `UserChatRelay.relayProfile` instead of `name`
   - Reuse existing `createRelayForOwner` with profile parameter (see Phase 7)

5. Update `SetUserChatRelays` / `SetChatRelays` CLI parsing:
   - Remove `name` from `CLINewRelay` parsing
   - Relay is specified by address only: `/relays <address1> <address2>`

### Phase 7: Subscriber.hs — Event handlers

**File: `src/Simplex/Chat/Library/Subscriber.hs`**

#### Relay side: processContactConnMessage REQ handler

1. Add `XGrpRelayTest` case (after line 1247):
   ```haskell
   XGrpRelayTest _ -> xGrpRelayTest invId chatVRange
   ```

2. Add `xGrpRelayTest` function (near `xGrpRelayInv`, around line 1450):
   ```haskell
   xGrpRelayTest :: InvitationId -> VersionRangeChat -> CM ()
   xGrpRelayTest invId chatVRange = do
     let profile = userProfileDirect user Nothing Nothing True
         msg = XGrpRelayTest (Just profile)
     subMode <- chatReadVar subscriptionMode
     vr <- chatVersionRange
     let chatV = vr `peerConnChatVersion` chatVRange
     (_cmdId, connId) <- agentAcceptContactAsync user True invId msg subMode PQSupportOff chatV
     -- Schedule connection cleanup — no DB record created, agent connection will
     -- produce errors on future events (harmless), and agent will eventually GC it
     -- TODO consider immediate deletion after acceptance is confirmed
     pure ()
   ```

#### Owner side: processDirectMessage CONF handler

3. Modify CONF handler in `processDirectMessage` for `contact_ = Nothing` (line 407-417):
   - Before existing flow, check if this connection is a relay test:
   ```haskell
   Nothing -> case agentMsg of
     CONF confId pqSupport _ connInfo -> do
       -- Check if this is a relay test connection
       chatRelayTests_ <- asks chatRelayTests
       relayTest_ <- atomically $ TM.lookup agentConnId chatRelayTests_
       case relayTest_ of
         Just testVar -> do
           -- Relay test connection — extract profile from XGrpRelayTest
           ChatMessage {chatMsgEvent} <- parseChatMessage conn connInfo
           case chatMsgEvent of
             XGrpRelayTest (Just profile) ->
               atomically $ putTMVar testVar (Right profile)
             XGrpRelayTest Nothing ->
               atomically $ putTMVar testVar (Left $ ChatError $ CEException "relay test: no profile in response")
             _ ->
               atomically $ putTMVar testVar (Left $ ChatError $ CEException "relay test: unexpected message")
           -- Delete connection
           deleteAgentConnectionAsync agentConnId
         Nothing -> do
           -- Existing flow (unchanged)
           conn' <- processCONFpqSupport conn pqSupport
           (conn'', gInfo_) <- saveConnInfo conn' connInfo
           ...
   ```

### Phase 8: Store/Groups.hs — Use saved relay profile

**File: `src/Simplex/Chat/Store/Groups.hs`**

1. Update `createRelayForOwner` (line 1333-1353):
   - Change parameter from `UserChatRelay {name}` to `UserChatRelay {relayProfile}`
   - Use `relayProfile` directly (or `profileFromName displayName` as fallback if profile is Nothing):
   ```haskell
   createRelayForOwner db vr gVar user gInfo UserChatRelay {relayProfile = relayProfile_} = do
     let profile = fromMaybe (profileFromName "unknown_relay") relayProfile_
     (localDisplayName, memProfileId) <- createNewMemberProfile_ db user profile currentTs
     ...
   ```

### Phase 9: Startup cleanup

**File: `src/Simplex/Chat/Library/Commands.hs`** (in `startChat` or startup sequence)

1. On startup, find stale relay test connections and delete them:
   ```haskell
   -- In chat startup sequence
   staleTestConns <- withStore' getStaleRelayTestConns
   forM_ staleTestConns $ \(chatRelayId, agentConnId) -> do
     deleteAgentConnectionAsync agentConnId
     withStore' $ \db -> setChatRelayTestConn db chatRelayId Nothing
   ```

### Phase 10: Views (iOS + Android/Desktop)

**iOS: `apps/ios/Shared/Views/UserSettings/NetworkAndServers/ChatRelayView.swift`**
**iOS: `apps/ios/Shared/Views/NewChat/AddChannelView.swift`**

**Android/Desktop: `apps/multiplatform/.../ChatRelayView.kt`**
**Android/Desktop: `apps/multiplatform/.../AddChannelView.kt`**

1. Remove name text field from relay configuration views
2. Show relay's `relayProfile.displayName` where name was shown (if profile exists)
3. Before testing: show address domain as placeholder (e.g., "smp111.simplex.im")
4. Add "Test" button that calls `APITestChatRelay`
5. Show test result (success with profile, or failure with error)

### Phase 11: Tests

**File: `tests/ChatTests/ChatRelays.hs`**

1. Add `testRelayChatRelayTest` test:
   - Create relay user with address
   - Owner adds relay to config (CLI command, no name)
   - Owner tests relay → verify success, profile saved
   - Owner creates channel → verify relay profile is used

2. Update existing tests for name removal:
   - `testGetSetChatRelays` — update CLI commands
   - `prepareChannel1Relay` / `prepareChannel2Relays` — update CLI commands

---

## Files Modified (summary)

| File | Changes |
|------|---------|
| `src/Simplex/Chat/Protocol.hs` | XGrpRelayTest constructor + tags + parsing + encoding; RelayAddressData type |
| `src/Simplex/Chat/Controller.hs` | APITestChatRelay command; CRChatRelayTestResult response; chatRelayTests field |
| `src/Simplex/Chat.hs` | Initialize chatRelayTests in newChatController |
| `src/Simplex/Chat/Operators.hs` | UserChatRelay: remove name, add relayProfile; CLINewRelay: remove name; presetChatRelay: remove name |
| `src/Simplex/Chat/Operators/Presets.hs` | simplexChatRelays: remove name strings |
| `src/Simplex/Chat/Library/Commands.hs` | APITestChatRelay handler; APICreateMyAddress RelayAddressData; addRelay profile; CLI parsing |
| `src/Simplex/Chat/Library/Subscriber.hs` | REQ handler: XGrpRelayTest; CONF handler: relay test pre-check |
| `src/Simplex/Chat/Library/Internal.hs` | (minor) exports if needed |
| `src/Simplex/Chat/Store/Profiles.hs` | getChatRelays, getChatRelayById, insertChatRelay, updateChatRelay: remove name, add profile ops |
| `src/Simplex/Chat/Store/Groups.hs` | createRelayForOwner: use saved profile; toGroupRelay: remove name |
| `src/Simplex/Chat/Store/SQLite/Migrations/M20260401_relay_test.hs` | New migration |
| `src/Simplex/Chat/Store/Postgres/Migrations/M20260401_relay_test.hs` | New migration |
| `src/Simplex/Chat/Store/SQLite/Migrations.hs` | Register migration |
| `src/Simplex/Chat/Store/Postgres/Migrations.hs` | Register migration |
| `simplex-chat.cabal` | Add migration modules |
| `tests/ChatTests/ChatRelays.hs` | New test + update existing tests |
| `apps/ios/.../ChatRelayView.swift` | Remove name, show profile |
| `apps/ios/.../AddChannelView.swift` | Remove name input |
| `apps/multiplatform/.../ChatRelayView.kt` | Remove name, show profile |
| `apps/multiplatform/.../AddChannelView.kt` | Remove name input |

---

## Key Existing Functions Reused

- `getShortLinkConnReq` (Internal.hs:1339) — fetch link data from relay address
- `prepareConnectionToJoin` (agent) — prepare agent connection
- `createRelayTestConnection` (new, pattern from Store/Groups.hs:1388) — create ConnContact DB record for test routing
- `encodeConnInfo` (Protocol.hs) — encode XGrpRelayTest as connInfo
- `joinConnection` (agent) — join relay's contact address
- `parseChatMessage` (Protocol.hs) — parse connInfo in CONF handler
- `agentAcceptContactAsync` (Internal.hs:2421) — relay accepts test connection
- `deleteAgentConnectionAsync` (Internal.hs:2428) — cleanup connections
- `encodeShortLinkData` (Internal.hs:1351) — encode RelayAddressData
- `decodeLinkUserData` (Internal.hs:1361) — decode RelayAddressData
- `createNewMemberProfile_` (Store/Groups.hs:1961) — create profile in contact_profiles
- `userProfileDirect` — get relay's current profile for test response
- `profileFromName` (Types.hs:701) — fallback profile when relay profile not yet tested

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
1. Start relay user, create address
2. Start owner user, add relay (by address only)
3. Test relay → verify CRChatRelayTestResult with profile, tested=True
4. Create channel with tested relay → verify relay member has actual profile
5. Kill and restart owner during test → verify cleanup on startup

---

## Adversarial Self-Review

### Pass 1

**Issue: TMVar semantics** — `newTMVarIO` creates a FULL TMVar. We need `newEmptyTMVarIO` for blocking.
**Fix:** Use `newEmptyTMVarIO` in the handler and `putTMVar` in the subscriber.

**Issue: `odUserId`** — The function `odUserId` doesn't exist. Should use `userId` from User pattern.
**Fix:** Use `userId` from user destructuring pattern.

**Issue: `aConnId conn` vs `agentConnId`** — In the CONF handler, the connection's agent conn ID is accessed via `agentConnId` (from processAgentMessageConn closure), not from the conn record.
**Fix:** Use `agentConnId` consistently in the subscriber handler.

**Issue: Connection deletion in CONF handler** — `deleteAgentConnectionAsync` takes a `ConnId` (agent ConnId), which is `agentConnId` in the subscriber context. Also need to delete the DB connection record.
**Fix:** Delete both agent connection and DB connection in the CONF handler. Use `withStore' $ \db -> deleteConnectionRecord db user connId` for DB cleanup.

**Issue: SQLite DROP COLUMN** — SQLite < 3.35.0 doesn't support DROP COLUMN. Need to verify the project's minimum SQLite version.
**Fix:** The existing migration `down_m20260222_chat_relays` already uses DROP COLUMN (line 92+), so SQLite 3.35.0+ is assumed. Proceed with DROP COLUMN.

**Issue: Relay-side connection cleanup** — After accepting the test, the relay creates an agent connection but no DB connection. Future events on this connection will fail in `getConnectionEntity` with `SEConnectionNotFound`, which goes to `eToView`. This log noise is acceptable but not ideal.
**Fix:** Note for future improvement. The relay could schedule deletion after a short delay. For now, the agent will eventually GC the connection.

### Pass 2

**Issue: `createDirectConnection'` signature** — The function takes `CreatedLinkInvitation` not `CCLink`. Let me re-verify.
**Verified:** `createDirectConnection' :: DB.Connection -> UserId -> ConnId -> CreatedLinkInvitation -> Maybe ContactId -> ConnStatus -> Maybe Profile -> SubscriptionMode -> VersionChat -> PQSupport -> IO Connection`. The `CCLink cReq Nothing` would need to be wrapped in the right type. Actually, `CreatedLinkInvitation` is likely `CCLink`. Let me check... Looking at the code, the `ccLink` parameter is of type `CreatedLinkInvitation` which is a type alias or newtype for `CreatedConnLink 'CMInvitation`. For the test, we may need to create a minimal `CreatedLinkInvitation`.
**Fix:** Actually, for the test connection, we can use a simpler approach. Create the connection record directly with an INSERT, similar to `createRelayConnection` but with `ConnContact` type and no entity ID. This avoids needing a `CreatedLinkInvitation`.

**Issue: Test connection persistence** — The test connection needs to survive long enough for the CONF to be routed. If we delete the DB record too early (before CONF arrives), `getConnectionEntity` won't find it. The deletion should happen AFTER the CONF is processed (in the subscriber handler).
**Fix:** The API handler creates the connection, then blocks on TMVar. The subscriber handler processes CONF, puts result in TMVar, THEN deletes the connection. The API handler unblocks AFTER the subscriber has already processed the CONF. Order: subscriber puts result → API handler reads TMVar → API handler does cleanup (redundant but safe).

**Issue: Concurrent tests on same relay** — If two tests run concurrently for the same relay, they'd both try to set `test_conn_agent_id`. The second test would overwrite the first's value. On crash, only the second test's connection would be cleaned up.
**Fix:** Use `test_conn_agent_id` as a guard — if non-null, reject the test with "test already in progress". This serializes tests per relay, which is the correct behavior.

Both passes now clean. No further issues found.
