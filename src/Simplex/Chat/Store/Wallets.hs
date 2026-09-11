{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Store.Wallets
  ( getDeviceSeed,
    getNextNameIndex,
    createSeed,
    deleteSeed,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Simplex.Chat.Wallet (NameIndex, SeedId, WalletSeed (..))
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
#else
import Database.SQLite.Simple (Only (..))
#endif

toSeed :: (Int64, ByteString) -> WalletSeed
toSeed (sId, seed) = WalletSeed {wsId = sId, wsEntropy = seed}

getDeviceSeed :: DB.Connection -> IO (Maybe WalletSeed)
getDeviceSeed db =
  maybeFirstRow toSeed $
    DB.query_ db "SELECT wallet_seed_id, entropy FROM wallet_seeds ORDER BY wallet_seed_id LIMIT 1"

-- | The index the next name bought on this device takes.
getNextNameIndex :: DB.Connection -> SeedId -> IO NameIndex
getNextNameIndex db sId =
  maybe 1 (fromIntegral :: Int64 -> NameIndex)
    <$> ( maybeFirstRow fromOnly $
            DB.query db "SELECT next_name_index FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)
        )

-- | False if the device already has a seed.
createSeed :: DB.Connection -> ByteString -> IO Bool
createSeed db entropy =
  getDeviceSeed db >>= \case
    Just _ -> pure False
    Nothing -> True <$ DB.execute db "INSERT INTO wallet_seeds (entropy) VALUES (?)" (Only $ DB.Binary entropy)

deleteSeed :: DB.Connection -> SeedId -> IO ()
deleteSeed db sId = DB.execute db "DELETE FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)
