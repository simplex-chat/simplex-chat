{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistence for wallet seeds and per-profile accounts.
--
-- The schema holds several seeds and binds each chat profile to one of them
-- plus its own account index. 'getOrCreateAccountRef' resolves that binding for
-- a purchase, moving the profile when another seed has been selected;
-- 'boundAccount' only reads.
module Simplex.Chat.Store.Wallets
  ( getOrCreateAccountRef,
    boundAccount,
    takeNameIndex,
    setNextNameIndex,
    raiseNextNameIndex,
    raiseNextAccountIndex,
    bindSeedAccount,
    nameKeyPathTaken,
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
import Data.Maybe (isJust, listToMaybe)
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

-- | Bind this profile to @sel_@, or to the seed it is already on when nothing
-- is selected, creating a seed from @mkSeed@ if the database has none yet.
--
-- A profile moves only when a seed is selected: picking the first row in the
-- table instead would silently re-bind a profile whenever a second seed exists
-- - which is exactly what importing a recovery key creates - moving it to a new
-- account index nobody asked for. Moving on selection is safe by contrast:
-- names re-derive from the literal path in wallet_name_keys, so the ones this
-- profile already owns stay reachable under the seed that owns them.
getOrCreateAccountRef :: DB.Connection -> User -> Maybe WalletSeed -> IO ByteString -> IO (WalletSeed, AccountRef)
getOrCreateAccountRef db user sel_ mkSeed = do
  existing <- getAccountRef db user
  bound <- case existing of
    Just r -> fmap (\s -> (r, s)) <$> getWalletSeed db (arSeedId r)
    Nothing -> pure Nothing
  case bound of
    Just (r, s) | maybe True ((wsId s ==) . wsId) sel_ -> pure (s, r)
    _ -> do
      s <- case sel_ of
        Just s -> pure s
        Nothing -> do
          seeds <- getWalletSeeds db
          case listToMaybe seeds of
            Just s -> pure s
            Nothing -> mkSeed >>= createWalletSeed db
      ix <- takeAccountIndex db (wsId s)
      let r = AccountRef {arSeedId = wsId s, arIndex = ix}
      bindAccount db user r
      pure (s, r)

-- | Pin this profile to an account index the user named, rather than taking the
-- next free one. Which profile held which account is not on chain and not in
-- the phrase, so after recovery on a new device only the user knows it.
--
-- The seed's mark moves past the pinned index too, or the next profile to bind
-- would be handed the same account and derive the same name keys.
bindSeedAccount :: DB.Connection -> User -> WalletSeed -> AccountIndex -> IO ()
bindSeedAccount db user s ix = do
  bindAccount db user AccountRef {arSeedId = wsId s, arIndex = ix}
  raiseNextAccountIndex db (wsId s) (ix + 1)

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

-- | Move a mark forward, never back. Used by a recovery scan, which learns from
-- the paths it found which indices are already spoken for: nothing else
-- restores these after an import, and a mark left at 0 hands the next purchase
-- a key a recovered name already owns.
raiseNextAccountIndex :: DB.Connection -> SeedId -> AccountIndex -> IO ()
raiseNextAccountIndex db (SeedId sId) ix =
  DB.execute
    db
    "UPDATE wallet_seeds SET next_account_index = ? WHERE wallet_seed_id = ? AND next_account_index < ?"
    (ix', sId, ix')
  where
    ix' = fromIntegral ix :: Int64

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

-- | See 'raiseNextAccountIndex'. Raised for the profile running the scan: which
-- profile a recovered name belonged to is not recorded anywhere, so the only
-- safe reading is that every index found is taken.
raiseNextNameIndex :: DB.Connection -> User -> NameIndex -> IO ()
raiseNextNameIndex db User {userId} ix =
  DB.execute
    db
    "UPDATE users SET wallet_next_name_index = ? WHERE user_id = ? AND wallet_next_name_index < ?"
    (ix', userId, ix')
  where
    ix' = fromIntegral ix :: Int64

-- | Point the next purchase at an index the user named. Unlike
-- 'raiseNextNameIndex' this moves the mark either way: the user is placing a
-- name deliberately, and may be filling a gap left by a failed attempt.
setNextNameIndex :: DB.Connection -> User -> NameIndex -> IO ()
setNextNameIndex db User {userId} ix =
  DB.execute db "UPDATE users SET wallet_next_name_index = ? WHERE user_id = ?" (fromIntegral ix :: Int64, userId)

-- | Whether a key at this path already owns a name. Checked before a purchase,
-- because 'recordNameKey' runs after the code has been spent: a clash caught by
-- the UNIQUE constraint there costs the user the code.
nameKeyPathTaken :: DB.Connection -> SeedId -> Text -> IO Bool
nameKeyPathTaken db (SeedId sId) path = do
  r <-
    maybeFirstRow fromOnly $
      DB.query db "SELECT 1 FROM wallet_name_keys WHERE wallet_seed_id = ? AND derivation_path = ?" (sId, path)
  pure $ isJust (r :: Maybe Int64)

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
