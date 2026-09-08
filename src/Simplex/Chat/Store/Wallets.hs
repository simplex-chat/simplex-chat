{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Persistence for wallet seeds and per-profile accounts.
--
-- The schema holds several seeds and binds each chat profile to one of them
-- plus its own account index. One seed per device is reachable today, so
-- 'deviceSeed' is the seed, and 'getOrCreateAccountRef' creates it on first use.
module Simplex.Chat.Store.Wallets
  ( deviceSeed,
    boundAccount,
    getOrCreateAccountRef,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int64)
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

toSeed :: (Int64, ByteString) -> WalletSeed
toSeed (sId, seed) = WalletSeed {wsId = SeedId sId, wsEntropy = seed}

-- | The seed on this device, or Nothing if the wallet has never been used.
deviceSeed :: DB.Connection -> IO (Maybe WalletSeed)
deviceSeed db =
  maybeFirstRow toSeed $
    DB.query_ db "SELECT wallet_seed_id, seed FROM wallet_seeds ORDER BY wallet_seed_id LIMIT 1"

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
-- used the wallet. Creates nothing: a profile is never given keys as a side
-- effect of reading.
boundAccount :: DB.Connection -> User -> IO (Maybe (WalletSeed, AccountRef))
boundAccount db user =
  getAccountRef db user >>= \case
    Nothing -> pure Nothing
    Just r -> fmap (\s -> (s, r)) <$> getWalletSeed db (arSeedId r)

-- | Bind this profile to the device's seed, creating that seed from @mkSeed@ if
-- there is none yet. Every profile gets its own account index within it.
getOrCreateAccountRef :: DB.Connection -> User -> IO ByteString -> IO (WalletSeed, AccountRef)
getOrCreateAccountRef db user mkSeed =
  boundAccount db user >>= \case
    Just bound -> pure bound
    Nothing -> do
      s <- deviceSeed db >>= maybe (mkSeed >>= createWalletSeed db) pure
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
