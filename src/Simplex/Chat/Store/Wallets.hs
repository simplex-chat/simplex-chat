{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.Store.Wallets
  ( SeedId,
    WalletSeed (..),
    getWalletSeed,
    createWalletSeed,
    deleteWalletSeed,
    resolveAccount,
    getUserAccounts,
    accountHeldBy,
    bindAccount,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (isJust)
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

data WalletSeed = WalletSeed
  { wsId :: SeedId,
    wsEntropy :: BA.ScrubbedBytes,
    wsNextAccount :: Maybe AccountIndex
  }

toSeed :: (SeedId, ByteString, Maybe AccountIndex) -> WalletSeed
toSeed (wsId, entropy, wsNextAccount) = WalletSeed {wsId, wsEntropy = BA.convert entropy, wsNextAccount}

getWalletSeed :: DB.Connection -> IO (Maybe WalletSeed)
getWalletSeed db =
  maybeFirstRow toSeed $
    DB.query_ db "SELECT wallet_seed_id, entropy, next_account_index FROM wallet_seeds ORDER BY wallet_seed_id LIMIT 1"

createWalletSeed :: DB.Connection -> BA.ScrubbedBytes -> Maybe AccountIndex -> IO Bool
createWalletSeed db entropy nextAccount =
  fmap isJust . maybeFirstRow (fromOnly @SeedId) $
    DB.query
      db
      [sql|
        INSERT INTO wallet_seeds (entropy, next_account_index) VALUES (?, ?)
        ON CONFLICT (single_seed) DO NOTHING
        RETURNING wallet_seed_id
      |]
      (DB.Binary (BA.convert entropy :: ByteString), nextAccount)

deleteWalletSeed :: DB.Connection -> IO Bool
deleteWalletSeed db =
  fmap isJust . maybeFirstRow (fromOnly @SeedId) $
    DB.query_ db "DELETE FROM wallet_seeds RETURNING wallet_seed_id"

resolveAccount :: DB.Connection -> Maybe AccountIndex -> IO (Either WalletError (WalletSeed, AccountIndex))
resolveAccount db accountIdx_ = runExceptT $ do
  seed@WalletSeed {wsNextAccount} <- ExceptT $ maybe (Left WENoMaster) Right <$> getWalletSeed db
  n <- liftEither $ maybe (Left WECounterUnknown) Right (accountIdx_ <|> wsNextAccount)
  liftEither $ checkAccountIndex n
  pure (seed, n)

getUserAccounts :: DB.Connection -> SeedId -> UserId -> IO [AccountIndex]
getUserAccounts db sId userId =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT account_index FROM wallet_accounts
        WHERE wallet_seed_id = ? AND user_id = ? AND account_index IS NOT NULL
        ORDER BY account_index
      |]
      (sId, userId)

-- | Which profile holds an account: 'Nothing' when there is no row for it, @Just Nothing@ when no profile holds it.
accountUser :: DB.Connection -> SeedId -> AccountIndex -> IO (Maybe (Maybe UserId))
accountUser db sId n =
  maybeFirstRow fromOnly $
    DB.query db "SELECT user_id FROM wallet_accounts WHERE wallet_seed_id = ? AND account_index = ?" (sId, n)

bindAccount :: DB.Connection -> UserId -> Maybe AccountIndex -> IO (Either WalletError (WalletSeed, AccountIndex))
bindAccount db userId accountIdx_ = runExceptT $ do
  r@(WalletSeed {wsId}, n) <- ExceptT $ resolveAccount db accountIdx_
  held <- liftIO $ accountUser db wsId n >>= \case
    Just (Just heldBy) -> pure $ heldBy == userId
    Just Nothing -> setAccountUser db wsId userId n
    Nothing -> True <$ insertAccount db wsId userId n
  unless held $ throwError WEAccountBound
  liftIO $ raiseNextAccount db wsId n
  pure r

setAccountUser :: DB.Connection -> SeedId -> UserId -> AccountIndex -> IO Bool
setAccountUser db sId userId n =
  fmap isJust . maybeFirstRow (fromOnly @Int64) $
    DB.query
      db
      [sql|
        UPDATE wallet_accounts SET user_id = ?
        WHERE wallet_seed_id = ? AND account_index = ? AND user_id IS NULL
        RETURNING wallet_account_id
      |]
      (userId, sId, n)

accountHeldBy :: DB.Connection -> SeedId -> UserId -> AccountIndex -> IO Bool
accountHeldBy db sId userId n = (== Just (Just userId)) <$> accountUser db sId n

raiseNextAccount :: DB.Connection -> SeedId -> AccountIndex -> IO ()
raiseNextAccount db sId n =
  DB.execute
    db
    [sql|
      UPDATE wallet_seeds SET next_account_index = ?
      WHERE wallet_seed_id = ? AND next_account_index IS NOT NULL AND next_account_index <= ?
    |]
    (n + 1, sId, n)

insertAccount :: DB.Connection -> SeedId -> UserId -> AccountIndex -> IO ()
insertAccount db sId userId n =
  DB.execute db "INSERT INTO wallet_accounts (wallet_seed_id, account_index, user_id) VALUES (?, ?, ?)" (sId, n, userId)
