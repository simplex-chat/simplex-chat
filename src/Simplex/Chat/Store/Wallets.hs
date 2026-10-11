{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Wallets
  ( Wallet (..),
    getWallet,
    createWallet,
    deleteWallet,
    getWalletInfo,
    bindAccount,
    heldAccount,
  )
where

import Control.Monad (when)
import Data.Int (Int64)
import Data.Word (Word32)
import Simplex.Chat.Types (User (..))
import Simplex.Chat.Wallet (WalletInfo (..))
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.BIP39 (WalletEntropy)
import Simplex.Messaging.Crypto.BIP44 (AccountIndex, unAccountIndex)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
#endif

data Wallet = Wallet
  { walletSeedId :: Int64,
    entropy :: WalletEntropy,
    nextAccountIndex :: Maybe Word32
  }

getWallet :: DB.Connection -> IO (Maybe Wallet)
getWallet db =
  maybeFirstRow toWallet $
    DB.query_
      db
      ( "SELECT wallet_seed_id, entropy, next_account_index FROM wallet_seeds"
#if defined(dbPostgres)
          <> " FOR UPDATE"
#endif
      )
  where
    toWallet (walletSeedId, entropy, nextAccountIndex) = Wallet {walletSeedId, entropy, nextAccountIndex}

createWallet :: DB.Connection -> WalletEntropy -> Maybe Word32 -> IO Bool
createWallet db entropy nextAccountIndex =
  getWallet db >>= \case
    Just _ -> pure False
    Nothing -> do
      DB.execute db "INSERT INTO wallet_seeds (entropy, next_account_index) VALUES (?,?)" (entropy, nextAccountIndex)
      pure True

deleteWallet :: DB.Connection -> Wallet -> IO ()
deleteWallet db Wallet {walletSeedId} =
  DB.execute db "DELETE FROM wallet_seeds WHERE wallet_seed_id = ?" (Only walletSeedId)

getWalletInfo :: DB.Connection -> User -> Wallet -> IO WalletInfo
getWalletInfo db User {userId} Wallet {walletSeedId, nextAccountIndex} =
  toWalletInfo . map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT account_index FROM wallet_accounts
        WHERE wallet_seed_id = ? AND user_id = ?
        ORDER BY account_index
      |]
      (walletSeedId, userId)
  where
    toWalletInfo accountIndexes = WalletInfo {accountIndexes, nextAccountIndex}

bindAccount :: DB.Connection -> User -> Wallet -> AccountIndex -> IO Bool
bindAccount db user@User {userId} w@Wallet {walletSeedId} n = do
  DB.execute
    db
    [sql|
      INSERT INTO wallet_accounts (wallet_seed_id, account_index, user_id) VALUES (?,?,?)
      ON CONFLICT (wallet_seed_id, account_index) DO UPDATE SET user_id = excluded.user_id
      WHERE wallet_accounts.user_id IS NULL
    |]
    (walletSeedId, n, userId)
  held <- heldAccount db user w n
  when held $
    DB.execute
      db
      "UPDATE wallet_seeds SET next_account_index = ? WHERE wallet_seed_id = ? AND next_account_index <= ?"
      (unAccountIndex n + 1, walletSeedId, n)
  pure held

heldAccount :: DB.Connection -> User -> Wallet -> AccountIndex -> IO Bool
heldAccount db User {userId} Wallet {walletSeedId} n =
  fromOnly . head
    <$> DB.query
      db
      "SELECT EXISTS (SELECT 1 FROM wallet_accounts WHERE wallet_seed_id = ? AND account_index = ? AND user_id = ?)"
      (walletSeedId, n, userId)
