{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Word (Word32)
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Wallet (WalletInfo (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.BIP39 (WalletEntropy, mkEntropy, unEntropy)
import Simplex.Messaging.Crypto.BIP44 (AccountIndex, unAccountIndex)
import Simplex.Messaging.Util (liftEitherWith)

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

getWallet :: DB.Connection -> ExceptT StoreError IO (Maybe Wallet)
getWallet db =
  liftIO (maybeFirstRow id $ DB.query_ db "SELECT wallet_seed_id, entropy, next_account_index FROM wallet_seeds")
    >>= mapM toWallet
  where
    toWallet :: (Int64, ByteString, Maybe Word32) -> ExceptT StoreError IO Wallet
    toWallet (walletSeedId, entBytes, nextAccountIndex) =
      liftEitherWith SEInternalError $ (\entropy -> Wallet {walletSeedId, entropy, nextAccountIndex}) <$> mkEntropy (BA.convert entBytes)

createWallet :: DB.Connection -> WalletEntropy -> Maybe Word32 -> IO Bool
createWallet db entropy nextAccount =
  rowReturned $
    DB.query
      db
      [sql|
        INSERT INTO wallet_seeds (entropy, next_account_index) VALUES (?, ?)
        ON CONFLICT (single_seed) DO NOTHING
        RETURNING wallet_seed_id
      |]
      (DB.Binary (BA.convert (unEntropy entropy) :: ByteString), nextAccount)

deleteWallet :: DB.Connection -> IO Bool
deleteWallet db =
  rowReturned $ DB.query_ db "DELETE FROM wallet_seeds RETURNING wallet_seed_id"

getWalletInfo :: DB.Connection -> UserId -> Wallet -> IO WalletInfo
getWalletInfo db userId Wallet {walletSeedId, nextAccountIndex} =
  (\accountIndexes -> WalletInfo {accountIndexes, nextAccountIndex}) . map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT account_index FROM wallet_accounts
        WHERE wallet_seed_id = ? AND user_id = ?
        ORDER BY account_index
      |]
      (walletSeedId, userId)

bindAccount :: DB.Connection -> UserId -> Wallet -> AccountIndex -> IO Bool
bindAccount db userId Wallet {walletSeedId} n = do
  held <-
    rowReturned $
      DB.query
        db
        [sql|
          INSERT INTO wallet_accounts (wallet_seed_id, account_index, user_id) VALUES (?, ?, ?)
          ON CONFLICT (wallet_seed_id, account_index) DO UPDATE SET user_id = excluded.user_id
          WHERE wallet_accounts.user_id IS NULL OR wallet_accounts.user_id = excluded.user_id
          RETURNING wallet_account_id
        |]
        (walletSeedId, n, userId)
  when held $
    DB.execute
      db
      "UPDATE wallet_seeds SET next_account_index = ? WHERE wallet_seed_id = ? AND next_account_index <= ?"
      (unAccountIndex n + 1, walletSeedId, n)
  pure held

heldAccount :: DB.Connection -> UserId -> Wallet -> AccountIndex -> IO Bool
heldAccount db userId Wallet {walletSeedId} n =
  rowReturned $
    DB.query db "SELECT wallet_account_id FROM wallet_accounts WHERE wallet_seed_id = ? AND account_index = ? AND user_id = ?" (walletSeedId, n, userId)

rowReturned :: IO [Only Int64] -> IO Bool
rowReturned = fmap (not . null)
