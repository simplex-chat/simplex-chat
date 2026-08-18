{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistence for wallet seeds and per-profile accounts.
--
-- The schema holds several seeds and binds each chat profile to one of them
-- plus its own account index. Only the single-seed case is reachable from the
-- UI: 'getOrCreateAccountRef' reuses the database's first seed and allocates the
-- next free account index.
module Simplex.Chat.Store.Wallets
  ( getWalletSeeds,
    getWalletSeed,
    createWalletSeed,
    getAccountRef,
    bindAccount,
    getOrCreateAccountRef,
    getNextAccountIndex,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Chat.Types (User (..))
import Simplex.Chat.Wallet (AccountIndex, AccountRef (..), SeedId (..), WalletSeed (..))
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
#else
import Database.SQLite.Simple (Only (..))
#endif

toSeed :: (Int64, ByteString, Bool) -> WalletSeed
toSeed (sId, seed, backedUp) = WalletSeed {wsId = SeedId sId, wsEntropy = seed, wsBackedUp = backedUp}

getWalletSeeds :: DB.Connection -> IO [WalletSeed]
getWalletSeeds db =
  map toSeed
    <$> DB.query_ db "SELECT wallet_seed_id, seed, backed_up FROM wallet_seeds ORDER BY wallet_seed_id"

getWalletSeed :: DB.Connection -> SeedId -> IO (Maybe WalletSeed)
getWalletSeed db (SeedId sId) =
  maybeFirstRow toSeed $
    DB.query db "SELECT wallet_seed_id, seed, backed_up FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)

-- | Insert a seed. Callers generate the entropy; this module never does, so the
-- DRG stays with the agent.
createWalletSeed :: DB.Connection -> ByteString -> IO WalletSeed
createWalletSeed db seed = do
  DB.execute db "INSERT INTO wallet_seeds (seed) VALUES (?)" (Only seed)
  sId <- insertedRowId db
  pure WalletSeed {wsId = SeedId sId, wsEntropy = seed, wsBackedUp = False}

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
