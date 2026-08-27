{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistence for wallet seeds and per-profile accounts.
--
-- The schema holds several seeds and binds each chat profile to one of them
-- plus its own account index. Only the single-seed case is reachable from the
-- UI: 'getOrCreateAccountRef' reuses the database's first seed and allocates the
-- next free account index, while 'boundAccount' only reads. Those two are the
-- exports; the helpers below stay internal until they have a caller.
module Simplex.Chat.Store.Wallets
  ( getOrCreateAccountRef,
    boundAccount,
    takeNameIndex,
    recordNameKey,
    getNameKeys,
    listSeeds,
    createSeed,
    setCurrentSeed,
    currentSeed,
    markBackedUp,
    seedOfName,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Chat.Types (User (..))
import Simplex.Chat.Wallet (AccountIndex, AccountRef (..), NameIndex, SeedId (..), WalletSeed (..))
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
#else
import Database.SQLite.Simple (Only (..))
#endif

toSeed :: (Int64, ByteString) -> WalletSeed
toSeed (sId, seed) = WalletSeed {wsId = SeedId sId, wsEntropy = seed}

getWalletSeeds :: DB.Connection -> IO [WalletSeed]
getWalletSeeds db =
  map toSeed
    <$> DB.query_ db "SELECT wallet_seed_id, seed FROM wallet_seeds ORDER BY wallet_seed_id"

getWalletSeed :: DB.Connection -> SeedId -> IO (Maybe WalletSeed)
getWalletSeed db (SeedId sId) =
  maybeFirstRow toSeed $
    DB.query db "SELECT wallet_seed_id, seed FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)

-- | Insert a seed. Callers generate the entropy; this module never does, so the
-- DRG stays with the agent.
createWalletSeed :: DB.Connection -> ByteString -> IO WalletSeed
createWalletSeed db seed = do
  DB.execute db "INSERT INTO wallet_seeds (seed) VALUES (?)" (Only seed)
  sId <- insertedRowId db
  pure WalletSeed {wsId = SeedId sId, wsEntropy = seed}

getAccountRef :: DB.Connection -> User -> IO (Maybe AccountRef)
getAccountRef db User {userId} = do
  r <-
    maybeFirstRow id $
      DB.query db "SELECT wallet_seed_id, wallet_account_index FROM users WHERE user_id = ?" (Only userId)
  pure $ case r of
    Just (Just sId, Just ix) -> Just AccountRef {arSeedId = SeedId sId, arIndex = fromIntegral (ix :: Int64)}
    _ -> Nothing

bindAccount :: DB.Connection -> User -> AccountRef -> IO ()
bindAccount db User {userId} AccountRef {arSeedId = SeedId sId, arIndex} =
  DB.execute
    db
    "UPDATE users SET wallet_seed_id = ?, wallet_account_index = ? WHERE user_id = ?"
    (sId, fromIntegral arIndex :: Int64, userId)

-- | The seed and account this profile is bound to, or Nothing if it has never
-- used the wallet. Creates nothing: callers that need a wallet ask the user
-- first, so a profile is never given keys as a side effect of reading.
boundAccount :: DB.Connection -> User -> IO (Maybe (WalletSeed, AccountRef))
boundAccount db user =
  getAccountRef db user >>= \case
    Nothing -> pure Nothing
    Just r -> fmap (\s -> (s, r)) <$> getWalletSeed db (arSeedId r)

-- | Bind this profile to a seed, creating one from @mkSeed@ if the database has
-- none yet, and allocating the next free account index.
--
-- Single-seed by construction: it always picks the first existing seed. When
-- multiple seeds become selectable this is the one function that changes.
getOrCreateAccountRef :: DB.Connection -> User -> IO ByteString -> IO (WalletSeed, AccountRef)
getOrCreateAccountRef db user mkSeed = do
  existing <- getAccountRef db user
  -- Load the seed this profile is actually bound to. Picking the first row in
  -- the table instead would silently re-bind a profile whenever a second seed
  -- exists - which is exactly what importing a recovery key creates - throwing
  -- away the imported key and moving the profile to a new account index, so
  -- the names it already owned stop being derivable too.
  bound <- case existing of
    Just r -> fmap (\s -> (r, s)) <$> getWalletSeed db (arSeedId r)
    Nothing -> pure Nothing
  case bound of
    Just (r, s) -> pure (s, r)
    Nothing -> do
      seeds <- getWalletSeeds db
      s <- case listToMaybe seeds of
        Just s -> pure s
        Nothing -> mkSeed >>= createWalletSeed db
      ix <- takeAccountIndex db (wsId s)
      let r = AccountRef {arSeedId = wsId s, arIndex = ix}
      bindAccount db user r
      pure (s, r)

-- | Take the next account index and advance the seed's high-water mark.
--
-- The mark is stored rather than computed as @MAX(users.wallet_account_index)@,
-- because after recovery from the phrase alone the @users@ table is empty while
-- accounts @0..N@ already hold names on chain. Computing it would hand the first
-- newly created profile index 0 and, with it, a recovered account's keys.
takeAccountIndex :: DB.Connection -> SeedId -> IO AccountIndex
takeAccountIndex db sId@(SeedId sId') = do
  ix <- getNextAccountIndex db sId
  DB.execute db "UPDATE wallet_seeds SET next_account_index = ? WHERE wallet_seed_id = ?" (fromIntegral ix + 1 :: Int64, sId')
  pure ix

getNextAccountIndex :: DB.Connection -> SeedId -> IO AccountIndex
getNextAccountIndex db (SeedId sId) =
  maybe 0 (fromIntegral :: Int64 -> AccountIndex)
    <$> ( maybeFirstRow fromOnly $
            DB.query db "SELECT next_account_index FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)
        )

-- | Take the next name index for this profile and advance its high-water mark.
--
-- Names are BIP-44 address indices under the profile's account, so this counter
-- is per profile, not per seed. Stored for the same reason as
-- 'next_account_index': after recovery from the phrase alone nothing else knows
-- which indices already own names on chain.
takeNameIndex :: DB.Connection -> User -> IO NameIndex
takeNameIndex db User {userId} = do
  ix <-
    maybe 0 (fromIntegral :: Int64 -> NameIndex)
      <$> ( maybeFirstRow fromOnly $
              DB.query db "SELECT wallet_next_name_index FROM users WHERE user_id = ?" (Only userId)
          )
  DB.execute db "UPDATE users SET wallet_next_name_index = ? WHERE user_id = ?" (fromIntegral ix + 1 :: Int64, userId)
  pure ix

-- | Record which key owns a name, once it is registered. Without this the
-- client cannot tell which of a profile's keys owns which name: the binding is
-- not on chain and is not derivable.
recordNameKey :: DB.Connection -> SeedId -> Text -> Text -> IO ()
recordNameKey db (SeedId sId) path name =
  DB.execute
    db
    "INSERT INTO wallet_name_keys (wallet_seed_id, derivation_path, name) VALUES (?, ?, ?)"
    (sId, path, name)

-- | Every name this seed owns, with the path its key was derived at.
getNameKeys :: DB.Connection -> SeedId -> IO [(Text, Text)]
getNameKeys db (SeedId sId) =
  DB.query
    db
    "SELECT name, derivation_path FROM wallet_name_keys WHERE wallet_seed_id = ? ORDER BY wallet_name_key_id"
    (Only sId)


-- | Every seed on this device, oldest first. The UI numbers them from 1.
listSeeds :: DB.Connection -> IO [(WalletSeed, Bool)]
listSeeds db =
  map (\(sId, seed, b) -> (toSeed (sId, seed), (b :: Int64) /= 0))
    <$> DB.query_ db "SELECT wallet_seed_id, seed, backed_up FROM wallet_seeds ORDER BY wallet_seed_id"

-- | Add a seed. Importing never replaces: a device that already holds one and
-- imports a recovery key ends up with both, or the imported names become
-- underivable.
createSeed :: DB.Connection -> ByteString -> IO WalletSeed
createSeed = createWalletSeed

markBackedUp :: DB.Connection -> SeedId -> IO ()
markBackedUp db (SeedId sId) =
  DB.execute db "UPDATE wallet_seeds SET backed_up = 1 WHERE wallet_seed_id = ?" (Only sId)

setCurrentSeed :: DB.Connection -> User -> SeedId -> IO ()
setCurrentSeed db User {userId} (SeedId sId) =
  DB.execute db "UPDATE users SET wallet_current_seed_id = ? WHERE user_id = ?" (sId, userId)

-- | Which seed a purchase goes under.
--
-- With one seed there is nothing to choose and it is used. With several and
-- none selected this returns Nothing, and the caller refuses with the list
-- rather than guessing — selection is a stored pointer, like the active user,
-- never a prompt.
currentSeed :: DB.Connection -> User -> IO (Maybe WalletSeed)
currentSeed db User {userId} = do
  sel <-
    maybeFirstRow fromOnly $
      DB.query db "SELECT wallet_current_seed_id FROM users WHERE user_id = ?" (Only userId)
  case sel of
    Just (Just sId) -> getWalletSeed db (SeedId (sId :: Int64))
    _ ->
      listSeeds db >>= \case
        [(s, _)] -> pure (Just s)
        _ -> pure Nothing

-- | The seed that owns a name, and the path its key sits at.
seedOfName :: DB.Connection -> Text -> IO (Maybe (WalletSeed, Text))
seedOfName db name = do
  r <-
    maybeFirstRow id $
      DB.query db "SELECT wallet_seed_id, derivation_path FROM wallet_name_keys WHERE name = ?" (Only name)
  case r of
    Nothing -> pure Nothing
    Just (sId, path) -> fmap (\s -> (s, path)) <$> getWalletSeed db (SeedId (sId :: Int64))
