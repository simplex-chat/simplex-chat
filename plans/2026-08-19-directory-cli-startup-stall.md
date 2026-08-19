# Directory service: CLI terminal does not start after upgrade

## Symptom

The service is started with `--run-cli`. With an older binary the `>` prompt works. With a newer
binary the prompt never appears and input is ignored, while the bot keeps answering users and
printing `sent 1 messages to SRDirect <id>`.

`N` below is the number of non-deleted contacts of the service user.

## Cause

### 1. The bot and the terminal UI are separate threads

`apps/simplex-directory-service/src/Directory/Service.hs:164-172`

```haskell
  raceAny_ $
    [ simplexChatCLI' terminalChatConfig {chatHooks, updateGroupLinksFromApp = True} (mkChatOpts opts) Nothing,
      processEvents env
    ]
      <> maybeToList (updateListingsThread_ opts env)
      <> maybeToList (linkCheckThread_ opts env)
  where
    processEvents env@ServiceState {eventQ} = do
      cc <- atomically $ readTMVar $ serviceCC env
```

`processEvents` is the bot. It does not depend on the terminal UI, only on `serviceCC`.

### 2. The terminal UI starts only after the post-start hook returns

`src/Simplex/Chat/Core.hs:92-97`

```haskell
  | otherwise = do
      a1 <- runReaderT (startChatController True True False) cc
      when (chatRelay && not testView) $ askCreateRelayAddress cc u chatRelayServer headless
      forM_ (postStartHook chatHooks) ($ cc)
      a2 <- async $ chat u cc
      waitEither_ a1 a2
```

Line 95 is synchronous. `chat u cc` at line 96 is the terminal UI (`newChatTerminal` +
`runChatTerminal`, `src/Simplex/Chat/Terminal.hs:72-76`). If the hook does not return, the terminal
UI is never created.

### 3. The hook publishes `serviceCC` before the point where it blocks

`apps/simplex-directory-service/src/Directory/Service.hs:215-230`

```haskell
directoryPostStartHook opts@DirectoryOpts {noAddress, testing} env cc =
  readTVarIO (currentUser cc) >>= \case
    Nothing -> putStrLn "No current user" >> exitFailure
    Just User {userId, profile = p@LocalProfile {preferences}} -> do
      unless noAddress $ initializeBotAddress' (not testing) cc
      void $ atomically $ tryPutTMVar (serviceCC env) cc
      listingsUpdated env
      let cmds = fromMaybe [] $ preferences >>= commands_
      unless (cmds == directoryCommands) $ do
        let prefs = (fromMaybe emptyChatPrefs preferences) {files = Just FilesPreference {allow = FANo}, commands = Just directoryCommands} :: Preferences
            p' = (fromLocalProfile p) {displayName = serviceName opts, peerType = Just CPTBot, preferences = Just prefs} :: Profile
        liftIO $
          sendChatCmd cc (APIUpdateProfile userId p') >>= \case
            Right CRUserProfileUpdated {} -> putStrLn "Updated directory commands"
            Right r -> putStrLn ("Error: unexpected response " <> show r) >> exitFailure
            Left e -> putStrLn ("Error: " <> show e) >> exitFailure
```

Line 220 unblocks the bot. Line 227 can block without stopping it. Hence: bot active, no prompt.

### 4. Why the new binary reaches line 227 and the old one does not

Commit `5e45fe1f0` ("directory: only create group links after approval") changed one label in
`directoryCommands`, `apps/simplex-directory-service/src/Directory/Service.hs:241`:

```diff
-        CBCCommand "link" "View and upgrade group link" idParam,
+        CBCCommand "link" "View group link" idParam,
```

The old binary's list equals the one stored in the service profile, so `cmds == directoryCommands`
at line 223 is `True` and the block is skipped. The new binary's list differs, so `APIUpdateProfile`
runs. The contact count is irrelevant for the old binary — it never enters this path.

### 5. `APIUpdateProfile` scans and rewrites every contact

`src/Simplex/Chat/Library/Commands.hs:3976-3987`

```haskell
  | otherwise = do
      when (n /= n') $ checkValidName n'
      checkProfileImageSize img'
      checkProfileSize p'
      -- read contacts before user update to correctly merge preferences
      contacts <- withFastStore' $ \db -> getUserContacts db cxt user
      user' <- updateUser
      asks currentUser >>= atomically . (`writeTVar` Just user')
      withChatLock "updateProfile" $ do
        when shouldUpdateAddressData $ setMyAddressData' user'
        summary <- sendUpdateToContacts user' contacts
        pure $ CRUserProfileUpdated user' (fromLocalProfile p) p' summary
```

`src/Simplex/Chat/Store/Direct.hs:818-822`

```haskell
getUserContacts :: DB.Connection -> StoreCxt -> User -> IO [Contact]
getUserContacts db cxt user@User {userId} = do
  contactIds <- map fromOnly <$> DB.query db "SELECT contact_id FROM contacts WHERE user_id = ? AND deleted = 0" (Only userId)
  contacts <- rights <$> mapM (runExceptT . getContact db cxt user) contactIds
  pure $ filter (\Contact {activeConn} -> isJust activeConn) contacts
```

Each `getContact` issues two round-trips — `getDirectChatTags` and the contact query
(`src/Simplex/Chat/Store/Direct.hs:965-992`) — so line 821 performs `2N` sequential round-trips on
one pooled connection and holds all `N` contacts in memory.

`sendUpdateToContacts` (line 3986) then sends `XInfo` to every contact whose merged profile changed.
Bot commands are part of the merged profile and have no per-contact value,
`src/Simplex/Chat/Types/Preferences.hs:1022`:

```haskell
      commands = ListDef $ fromMaybe [] $ (contactPrefs >>= commands_) <|> (userPreferences >>= commands_)
```

so all `N` contacts are marked as changed.

### 6. Why the bot keeps sending during the scan

The global chat lock is taken at line 3984, after the scan. Sending takes a contact lock,
`src/Simplex/Chat/Library/Commands.hs:721-722`:

```haskell
      withContactLock "sendMessage" chatId $
        sendContactContentMessages user chatId live itemTTL (L.map composedMessageReq cms)
```

which waits only while the global lock is held, `src/Simplex/Chat/Library/Internal.hs:127-132`:

```haskell
withEntityLock :: Text -> ChatLockEntity -> CM a -> CM a
withEntityLock name entity action = do
  chatLock <- asks chatLock
  ls <- asks entityLocks
  atomically $ unlessM (isEmptyTMVar chatLock) retry
  withLockMap ls entity name action
```

## Sequence after startup

1. `getUserContacts` scans all contacts. No terminal UI. Bot still answers.
2. `updateUser` (line 3982) writes the new profile.
3. `sendUpdateToContacts` delivers `N` profile updates under the global chat lock. Bot stops
   answering.
4. `Updated directory commands` printed, hook returns, terminal UI starts.

The reported symptom is phase 1. The stall is bounded, not permanent, but for large `N` it is long
and phase 3 sends `N` messages. Restarting during phase 1 repeats everything, because the profile is
written only after the scan.

## How to confirm

```sql
-- N
SELECT u.user_id, count(c.contact_id)
FROM users u LEFT JOIN contacts c ON c.user_id = u.user_id AND c.deleted = 0
GROUP BY u.user_id;

-- in phase 1, one connection continuously runs single-contact queries
SELECT state, wait_event_type, query FROM pg_stat_activity WHERE datname = current_database();

-- in phase 1, this still contains "View and upgrade group link"
SELECT p.preferences FROM users u JOIN profiles p ON p.profile_id = u.contact_profile_id
WHERE u.user_id = :user_id;
```

Contact ids in log lines are identifiers, not counts.

## Options

1. **No code change.** Replace `View and upgrade group link` with `View group link` in the
   `preferences` column of the service profile. The check at line 223 then passes, the hook returns
   at once, and the `N` profile updates are skipped. Contacts keep the old label until some later
   profile update.

2. **Do not block startup.** Run the block at `Directory/Service.hs:223` in a separate thread.

3. **Remove the per-contact query loop** at `Store/Direct.hs:821`, replacing it with a single query.

4. **Do not broadcast when only commands change.** Compare profiles excluding `commands`. Changes
   when clients learn about new bot commands.

Options 2 and 3 are complementary: one fixes a hook that blocks startup, the other a query loop that
scales with the number of contacts.
