{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Store.Wallets
  ( SeedId,
    Wallet (..),
    getWallet,
    createWallet,
    deleteWallet,
    resolveAccount,
    getUserAccounts,
    heldAccount,
    bindAccount,
  )
where

import Control.Monad.Except
import Data.Bifunctor (first)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Word (Word32)
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Wallet (WalletError (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.BIP32 (WalletMaster, masterBytes, masterEntropy, parseWalletMaster)
import Simplex.Messaging.Crypto.BIP39 (unEntropy)
import Simplex.Messaging.Crypto.BIP44 (AccountIndex, mkAccountIndex, unAccountIndex)
import Simplex.Messaging.Util (liftEitherWith)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
#endif

type SeedId = Int64

data Wallet = Wallet
  { walletId :: SeedId,
    walletMaster :: WalletMaster,
    nextAccountIndex :: Maybe Word32
  }

getWallet :: DB.Connection -> ExceptT StoreError IO (Maybe Wallet)
getWallet db =
  liftIO (maybeFirstRow id $ DB.query_ db "SELECT wallet_seed_id, entropy, master, next_account_index FROM wallet_seeds ORDER BY wallet_seed_id LIMIT 1")
    >>= mapM toWallet
  where
    toWallet :: (SeedId, ByteString, ByteString, Maybe Word32) -> ExceptT StoreError IO Wallet
    toWallet (walletId, entropy, master, nextAccountIndex) =
      liftEitherWith SEInternalError $ (\walletMaster -> Wallet {walletId, walletMaster, nextAccountIndex}) <$> parseWalletMaster (BA.convert entropy) (BA.convert master)

createWallet :: DB.Connection -> WalletMaster -> Maybe Word32 -> IO Bool
createWallet db master nextAccount =
  rowReturned $
    DB.query
      db
      [sql|
        INSERT INTO wallet_seeds (entropy, master, next_account_index) VALUES (?, ?, ?)
        ON CONFLICT (single_seed) DO NOTHING
        RETURNING wallet_seed_id
      |]
      (DB.Binary (BA.convert (unEntropy $ masterEntropy master) :: ByteString), DB.Binary (BA.convert (masterBytes master) :: ByteString), nextAccount)

deleteWallet :: DB.Connection -> IO Bool
deleteWallet db =
  rowReturned $ DB.query_ db "DELETE FROM wallet_seeds RETURNING wallet_seed_id"

resolveAccount :: DB.Connection -> Maybe AccountIndex -> ExceptT StoreError IO (Either WalletError (WalletMaster, AccountIndex))
resolveAccount db accountIdx_ = fmap (first walletMaster) <$> resolveAccount_ db accountIdx_

resolveAccount_ :: DB.Connection -> Maybe AccountIndex -> ExceptT StoreError IO (Either WalletError (Wallet, AccountIndex))
resolveAccount_ db accountIdx_ =
  getWallet db >>= \case
    Nothing -> pure $ Left WENoMaster
    Just w@Wallet {nextAccountIndex} -> pure $ (w,) <$> maybe next Right accountIdx_
      where
        next = maybe (Left WECounterUnknown) (first (const WEAccountsExhausted) . mkAccountIndex) nextAccountIndex

getUserAccounts :: DB.Connection -> UserId -> SeedId -> IO [AccountIndex]
getUserAccounts db userId sId =
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

bindAccount :: DB.Connection -> UserId -> Maybe AccountIndex -> ExceptT StoreError IO (Either WalletError (WalletMaster, AccountIndex))
bindAccount db userId accountIdx_ = resolveAccount_ db accountIdx_ >>= either (pure . Left) (liftIO . bind)
  where
    bind (Wallet {walletId, walletMaster}, n) = do
      held <-
        accountUser db walletId n >>= \case
          Just (Just heldBy) -> pure $ heldBy == userId
          Just Nothing -> setAccountUser db userId walletId n
          Nothing -> True <$ insertAccount db userId walletId n
      if held then Right (walletMaster, n) <$ raiseNextAccount db walletId n else pure $ Left WEAccountBound

setAccountUser :: DB.Connection -> UserId -> SeedId -> AccountIndex -> IO Bool
setAccountUser db userId sId n =
  rowReturned $
    DB.query
      db
      [sql|
        UPDATE wallet_accounts SET user_id = ?
        WHERE wallet_seed_id = ? AND account_index = ? AND user_id IS NULL
        RETURNING wallet_account_id
      |]
      (userId, sId, n)

heldAccount :: DB.Connection -> UserId -> AccountIndex -> ExceptT StoreError IO (Either WalletError WalletMaster)
heldAccount db userId n =
  getWallet db >>= \case
    Nothing -> pure $ Left WENoMaster
    Just Wallet {walletId, walletMaster} -> liftIO $ (\held -> if held == Just (Just userId) then Right walletMaster else Left WEAccountNotHeld) <$> accountUser db walletId n

raiseNextAccount :: DB.Connection -> SeedId -> AccountIndex -> IO ()
raiseNextAccount db sId n =
  DB.execute
    db
    [sql|
      UPDATE wallet_seeds SET next_account_index = ?
      WHERE wallet_seed_id = ? AND next_account_index IS NOT NULL AND next_account_index <= ?
    |]
    (unAccountIndex n + 1, sId, n)

insertAccount :: DB.Connection -> UserId -> SeedId -> AccountIndex -> IO ()
insertAccount db userId sId n =
  DB.execute db "INSERT INTO wallet_accounts (wallet_seed_id, account_index, user_id) VALUES (?, ?, ?)" (sId, n, userId)

rowReturned :: IO [Only Int64] -> IO Bool
rowReturned = fmap (not . null)
