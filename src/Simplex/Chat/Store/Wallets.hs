{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Wallets
  ( getDeviceSeed,
    getSeedAccounts,
    getOrCreateAccountRef,
    importSeed,
    deleteSeed,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Chat.Types (User (..))
import Simplex.Chat.Wallet (AccountIndex, AccountRef (..), SeedId (..), WalletSeed (..))
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import Simplex.Messaging.Agent.Store.DB (BoolInt (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
#endif

toSeed :: (Int64, ByteString) -> WalletSeed
toSeed (sId, seed) = WalletSeed {wsId = SeedId sId, wsEntropy = seed}

getDeviceSeed :: DB.Connection -> IO (Maybe WalletSeed)
getDeviceSeed db =
  maybeFirstRow toSeed $
    DB.query_ db "SELECT wallet_seed_id, seed FROM wallet_seeds ORDER BY wallet_seed_id LIMIT 1"

getWalletSeed :: DB.Connection -> SeedId -> IO (Maybe WalletSeed)
getWalletSeed db (SeedId sId) =
  maybeFirstRow toSeed $
    DB.query db "SELECT wallet_seed_id, seed FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)

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

getBoundAccount :: DB.Connection -> User -> IO (Maybe (WalletSeed, AccountRef))
getBoundAccount db user =
  getAccountRef db user >>= \case
    Nothing -> pure Nothing
    Just r -> fmap (\s -> (s, r)) <$> getWalletSeed db (arSeedId r)

getSeedAccounts :: DB.Connection -> SeedId -> IO [(Text, AccountIndex, Bool, Bool)]
getSeedAccounts db (SeedId sId) =
  map toRow
    <$> DB.query
      db
      [sql|
        SELECT local_display_name, wallet_account_index, active_user, view_pwd_hash
        FROM users WHERE wallet_seed_id = ? ORDER BY wallet_account_index
      |]
      (Only sId)
  where
    toRow (n, ix, BI active, pwdHash) = (n, fromIntegral (ix :: Int64), active, isJust (pwdHash :: Maybe ByteString))

getOrCreateAccountRef :: DB.Connection -> User -> ByteString -> IO (WalletSeed, AccountRef)
getOrCreateAccountRef db user entropy =
  getBoundAccount db user >>= \case
    Just bound -> pure bound
    Nothing -> getDeviceSeed db >>= maybe (createWalletSeed db entropy) pure >>= bindNewAccount db user

-- | Nothing if the device already has a key.
importSeed :: DB.Connection -> User -> ByteString -> IO (Maybe (WalletSeed, AccountRef))
importSeed db user entropy =
  getDeviceSeed db >>= \case
    Just _ -> pure Nothing
    Nothing -> Just <$> (createWalletSeed db entropy >>= bindNewAccount db user)

bindNewAccount :: DB.Connection -> User -> WalletSeed -> IO (WalletSeed, AccountRef)
bindNewAccount db user s = do
  ix <- takeAccountIndex db (wsId s)
  let r = AccountRef {arSeedId = wsId s, arIndex = ix}
  bindAccount db user r
  pure (s, r)

createWalletSeed :: DB.Connection -> ByteString -> IO WalletSeed
createWalletSeed db entropy = do
  DB.execute db "INSERT INTO wallet_seeds (seed) VALUES (?)" (Only entropy)
  sId <- insertedRowId db
  pure WalletSeed {wsId = SeedId sId, wsEntropy = entropy}

-- | Incremented in SQL, so two profiles cannot be handed the same account.
takeAccountIndex :: DB.Connection -> SeedId -> IO AccountIndex
takeAccountIndex db sId@(SeedId sId') = do
  DB.execute db "UPDATE wallet_seeds SET next_account_index = next_account_index + 1 WHERE wallet_seed_id = ?" (Only sId')
  subtract 1 <$> getNextAccountIndex db sId

getNextAccountIndex :: DB.Connection -> SeedId -> IO AccountIndex
getNextAccountIndex db (SeedId sId) =
  maybe 0 (fromIntegral :: Int64 -> AccountIndex)
    <$> ( maybeFirstRow fromOnly $
            DB.query db "SELECT next_account_index FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)
        )

-- | Profiles are unbound first, as the foreign key is ON DELETE RESTRICT.
deleteSeed :: DB.Connection -> SeedId -> IO ()
deleteSeed db (SeedId sId) = do
  DB.execute db "UPDATE users SET wallet_seed_id = NULL, wallet_account_index = NULL WHERE wallet_seed_id = ?" (Only sId)
  DB.execute db "DELETE FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)
