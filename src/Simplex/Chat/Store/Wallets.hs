{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    setSeedBackedUp,
    getAccountRef,
    getOrCreateAccountRef,
    bindAccount,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
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

setSeedBackedUp :: DB.Connection -> SeedId -> Bool -> IO ()
setSeedBackedUp db (SeedId sId) backedUp =
  DB.execute db "UPDATE wallet_seeds SET backed_up = ? WHERE wallet_seed_id = ?" (backedUp, sId)

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
  seeds <- getWalletSeeds db
  s <- case listToMaybe seeds of
    Just s -> pure s
    Nothing -> liftIO mkSeed >>= createWalletSeed db
  case existing of
    Just r | arSeedId r == wsId s -> pure (s, r)
    _ -> do
      ix <- nextAccountIndex db (wsId s)
      let r = AccountRef {arSeedId = wsId s, arIndex = ix}
      bindAccount db user r
      pure (s, r)

nextAccountIndex :: DB.Connection -> SeedId -> IO AccountIndex
nextAccountIndex db (SeedId sId) = do
  used <-
    maybeFirstRow fromOnly $
      DB.query db "SELECT MAX(wallet_account_index) FROM users WHERE wallet_seed_id = ?" (Only sId)
  pure $ maybe 0 (\m -> fromIntegral (m :: Int64) + 1) (fromMaybe Nothing used)
