{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | The device seed, and which chat profile each account belongs to.
module Simplex.Chat.Store.Wallets
  ( SeedId,
    WalletSeed (..),
    getWalletSeed,
    createWalletSeed,
    deleteWalletSeed,
    resolveAccount,
    getUserAccounts,
    accountHeldByOther,
    bindAccount,
    recordScan,
  )
where

import Control.Monad (join, unless)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Simplex.Chat.Wallet (AccountIndex, WalletError (..), checkAccountIndex)
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
#endif

type SeedId = Int64

-- | The device seed row. The entropy is 'BA.ScrubbedBytes', so a derived 'Show' does not print it.
data WalletSeed = WalletSeed
  { wsId :: SeedId,
    wsEntropy :: BA.ScrubbedBytes
  }
  deriving (Show)

toSeed :: (Int64, ByteString) -> WalletSeed
toSeed (sId, entropy) = WalletSeed {wsId = sId, wsEntropy = BA.convert entropy}

getWalletSeed :: DB.Connection -> IO (Maybe WalletSeed)
getWalletSeed db =
  maybeFirstRow toSeed $
    DB.query_ db "SELECT wallet_seed_id, entropy FROM wallet_seeds ORDER BY wallet_seed_id LIMIT 1"

-- | False if the device already has a seed. The counter is 'Nothing' for an imported phrase.
createWalletSeed :: DB.Connection -> BA.ScrubbedBytes -> Maybe AccountIndex -> IO Bool
createWalletSeed db entropy nextAccount =
  getWalletSeed db >>= \case
    Just _ -> pure False
    Nothing ->
      True
        <$ DB.execute
          db
          "INSERT INTO wallet_seeds (entropy, next_account_index) VALUES (?, ?)"
          (DB.Binary (BA.convert entropy :: ByteString), accountIndexCol <$> nextAccount)

-- | False if the device had no seed to delete. The account rows go with it.
deleteWalletSeed :: DB.Connection -> IO Bool
deleteWalletSeed db =
  getWalletSeed db >>= \case
    Nothing -> pure False
    Just WalletSeed {wsId} ->
      True <$ DB.execute db "DELETE FROM wallet_seeds WHERE wallet_seed_id = ?" (Only wsId)

-- | The seed, and the account named or the next free one from the counter. Refuses an index BIP-32 cannot harden.
resolveAccount :: DB.Connection -> Maybe AccountIndex -> IO (Either WalletError (WalletSeed, AccountIndex))
resolveAccount db accountIdx_ = runExceptT $ do
  seed <- ExceptT $ maybe (Left WENoMaster) Right <$> getWalletSeed db
  n <- maybe (nextFreeAccount $ wsId seed) pure accountIdx_
  liftEither $ checkAccountIndex n
  pure (seed, n)
  where
    nextFreeAccount sId = ExceptT $ maybe (Left WECounterUnknown) Right <$> getNextAccountIndex db sId

-- | The index the next account takes. Nothing after an import.
getNextAccountIndex :: DB.Connection -> SeedId -> IO (Maybe AccountIndex)
getNextAccountIndex db sId =
  fmap (fromIntegral @Int64) . join
    <$> maybeFirstRow fromOnly (DB.query db "SELECT next_account_index FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId))

-- | The accounts a profile holds, in index order.
getUserAccounts :: DB.Connection -> SeedId -> UserId -> IO [AccountIndex]
getUserAccounts db sId userId =
  map (fromIntegral @Int64 . fromOnly)
    <$> DB.query
      db
      [sql|
        SELECT account_index FROM wallet_accounts
        WHERE wallet_seed_id = ? AND user_id = ? AND account_index IS NOT NULL
        ORDER BY account_index
      |]
      (sId, userId)

-- | Which profile holds an account: 'Nothing' when it is unknown, @Just Nothing@ when no profile holds it.
accountUser :: DB.Connection -> SeedId -> AccountIndex -> IO (Maybe (Maybe Int64))
accountUser db sId n =
  maybeFirstRow (fromOnly @(Maybe Int64)) $
    DB.query db "SELECT user_id FROM wallet_accounts WHERE wallet_seed_id = ? AND account_index = ?" (sId, accountIndexCol n)

-- | True when another profile holds the account. A guard, not a boundary: @export master@ reaches every account.
heldByOther :: UserId -> Maybe (Maybe Int64) -> Bool
heldByOther userId = \case
  Just (Just heldBy) -> heldBy /= userId
  _ -> False

accountHeldByOther :: DB.Connection -> SeedId -> UserId -> AccountIndex -> IO Bool
accountHeldByOther db sId userId n = heldByOther userId <$> accountUser db sId n

-- | Bind an account to a profile, the next free one when no index is given. One transaction, so none is taken twice.
bindAccount :: DB.Connection -> UserId -> Maybe AccountIndex -> IO (Either WalletError ())
bindAccount db userId accountIdx_ = runExceptT $ do
  (WalletSeed {wsId = sId}, n) <- ExceptT $ resolveAccount db accountIdx_
  taken <- liftIO $ accountUser db sId n >>= \case
    Just (Just heldBy) -> pure $ heldBy == userId
    -- the update takes the account only while no profile holds it, the read after says whether this one got it
    Just Nothing -> setAccountUser db sId userId n >> accountHeldBy db sId userId n
    Nothing -> True <$ insertAccount db sId userId n
  unless taken $ throwError WEAccountBound
  liftIO $ raiseNextAccount db sId n

setAccountUser :: DB.Connection -> SeedId -> UserId -> AccountIndex -> IO ()
setAccountUser db sId userId n =
  DB.execute
    db
    [sql|
      UPDATE wallet_accounts SET user_id = ?
      WHERE wallet_seed_id = ? AND account_index = ? AND user_id IS NULL
    |]
    (userId, sId, accountIndexCol n)

accountHeldBy :: DB.Connection -> SeedId -> UserId -> AccountIndex -> IO Bool
accountHeldBy db sId userId n = (== Just (Just userId)) <$> accountUser db sId n

-- | What a scan found: the accounts in use, bound to the profile that ran it because the chain does not say whose they are, and the counter past them.
-- The counter is taken from the recorded accounts, not from the scan alone, so it never lands on one another profile already holds, and it only ever rises.
recordScan :: DB.Connection -> SeedId -> UserId -> [AccountIndex] -> IO ()
recordScan db sId userId inUse = do
  mapM_ insertScanned inUse
  DB.execute
    db
    [sql|
      UPDATE wallet_seeds SET next_account_index = COALESCE((SELECT max(account_index) + 1 FROM wallet_accounts WHERE wallet_seed_id = ?), 0)
      WHERE wallet_seed_id = ?
        AND (next_account_index IS NULL OR next_account_index < COALESCE((SELECT max(account_index) + 1 FROM wallet_accounts WHERE wallet_seed_id = ?), 0))
    |]
    (sId, sId, sId)
  where
    insertScanned n =
      DB.execute
        db
        [sql|
          INSERT INTO wallet_accounts (wallet_seed_id, account_index, user_id) VALUES (?, ?, ?)
          ON CONFLICT (wallet_seed_id, account_index) DO NOTHING
        |]
        (sId, accountIndexCol n, userId)

-- | Keep the counter a high-water mark. Never lowers it, never gives one to an imported phrase that has none.
raiseNextAccount :: DB.Connection -> SeedId -> AccountIndex -> IO ()
raiseNextAccount db sId n =
  DB.execute
    db
    [sql|
      UPDATE wallet_seeds SET next_account_index = ?
      WHERE wallet_seed_id = ? AND next_account_index IS NOT NULL AND next_account_index <= ?
    |]
    (accountIndexCol n + 1, sId, accountIndexCol n)

insertAccount :: DB.Connection -> SeedId -> UserId -> AccountIndex -> IO ()
insertAccount db sId userId n =
  DB.execute db "INSERT INTO wallet_accounts (wallet_seed_id, account_index, user_id) VALUES (?, ?, ?)" (sId, accountIndexCol n, userId)

accountIndexCol :: AccountIndex -> Int64
accountIndexCol = fromIntegral
