{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simplex.Chat.Store.Wallets
  ( getWalletEntropy,
    createWallet,
    deleteWallet,
    getWalletInfo,
    createAccount,
  )
where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Types (User (..))
import Simplex.Chat.Wallet (WalletInfo (..))
import Simplex.Messaging.Agent.Store.AgentStore (firstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.BIP39 (WalletEntropy)
import Simplex.Messaging.Crypto.BIP44 (AccountIndex)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
#endif

getWalletEntropy :: DB.Connection -> ExceptT StoreError IO WalletEntropy
getWalletEntropy db =
  ExceptT . firstRow fromOnly SEWalletNotFound $
    DB.query_ db "SELECT entropy FROM wallet_seeds"

createWallet :: DB.Connection -> WalletEntropy -> ExceptT StoreError IO WalletInfo
createWallet db entropy =
  ExceptT . firstRow (WalletInfo [] . fromOnly) SEDuplicateWallet $
    DB.query
      db
      [sql|
        INSERT INTO wallet_seeds (entropy, next_account_index) VALUES (?,1)
        ON CONFLICT (single_seed) DO NOTHING
        RETURNING next_account_index
      |]
      (Only entropy)

deleteWallet :: DB.Connection -> ExceptT StoreError IO ()
deleteWallet db = do
  deleted :: [Only Int64] <- liftIO $ DB.query_ db "DELETE FROM wallet_seeds RETURNING wallet_seed_id"
  when (null deleted) $ throwError SEWalletNotFound

getWalletInfo :: DB.Connection -> User -> ExceptT StoreError IO WalletInfo
getWalletInfo db User {userId} = do
  (walletSeedId :: Int64, nextAccountIndex) <-
    ExceptT . firstRow id SEWalletNotFound $
      DB.query_ db "SELECT wallet_seed_id, next_account_index FROM wallet_seeds"
  accountIndexes <-
    liftIO $
      map fromOnly
        <$> DB.query
          db
          [sql|
            SELECT account_index FROM wallet_accounts
            WHERE wallet_seed_id = ? AND user_id = ?
            ORDER BY account_index
          |]
          (walletSeedId, userId)
  pure WalletInfo {accountIndexes, nextAccountIndex}

createAccount :: DB.Connection -> User -> ExceptT StoreError IO (WalletEntropy, AccountIndex)
createAccount db User {userId} = do
  (walletSeedId :: Int64, entropy, n) <-
    ExceptT . firstRow id SEWalletNotFound $
      DB.query_
        db
        [sql|
          UPDATE wallet_seeds SET next_account_index = next_account_index + 1
          RETURNING wallet_seed_id, entropy, next_account_index - 1
        |]
  liftIO $ DB.execute db "INSERT INTO wallet_accounts (wallet_seed_id, account_index, user_id) VALUES (?,?,?)" (walletSeedId, n, userId)
  pure (entropy, n)
