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
    accountHeldBy,
    bindAccount,
  )
where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Word (Word32)
import Simplex.Chat.Store.Shared (StoreError (..))
import Simplex.Chat.Wallet (AccountIndex, WalletError (..))
import Simplex.Messaging.Agent.Protocol (UserId)
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.BIP32 (WalletMaster, masterBytes, masterEntropy, parseWalletMaster)
import Simplex.Messaging.Crypto.BIP39 (unEntropy)
import Simplex.Messaging.Crypto.BIP44 (mkAccountIndex, unAccountIndex)
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
    nextAccount :: Maybe Word32
  }

getWallet :: DB.Connection -> ExceptT StoreError IO (Maybe Wallet)
getWallet db =
  liftIO (maybeFirstRow id $ DB.query_ db "SELECT wallet_seed_id, entropy, master, next_account_index FROM wallet_seeds ORDER BY wallet_seed_id LIMIT 1")
    >>= mapM toWallet
  where
    toWallet :: (SeedId, ByteString, ByteString, Maybe Word32) -> ExceptT StoreError IO Wallet
    toWallet (walletId, entropy, master, nextAccount) =
      liftEitherWith SEInternalError $ (\walletMaster -> Wallet {walletId, walletMaster, nextAccount}) <$> parseWalletMaster (BA.convert entropy) (BA.convert master)

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

resolveAccount :: DB.Connection -> Maybe AccountIndex -> ExceptT StoreError IO (Either WalletError (Wallet, AccountIndex))
resolveAccount db accountIdx_ =
  getWallet db >>= \case
    Nothing -> pure $ Left WENoMaster
    Just w@Wallet {nextAccount} -> pure $ (w,) <$> maybe next Right accountIdx_
      where
        next = maybe (Left WECounterUnknown) (maybe (Left WEAccountsExhausted) Right . mkAccountIndex) nextAccount

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

bindAccount :: DB.Connection -> UserId -> Maybe AccountIndex -> ExceptT StoreError IO (Either WalletError (Wallet, AccountIndex))
bindAccount db userId accountIdx_ = resolveAccount db accountIdx_ >>= either (pure . Left) (liftIO . bind)
  where
    bind r@(Wallet {walletId}, n) = do
      held <-
        accountUser db walletId n >>= \case
          Just (Just heldBy) -> pure $ heldBy == userId
          Just Nothing -> setAccountUser db walletId userId n
          Nothing -> True <$ insertAccount db walletId userId n
      if held then Right r <$ raiseNextAccount db walletId n else pure $ Left WEAccountBound

setAccountUser :: DB.Connection -> SeedId -> UserId -> AccountIndex -> IO Bool
setAccountUser db sId userId n =
  rowReturned $
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
    (unAccountIndex n + 1, sId, n)

insertAccount :: DB.Connection -> SeedId -> UserId -> AccountIndex -> IO ()
insertAccount db sId userId n =
  DB.execute db "INSERT INTO wallet_accounts (wallet_seed_id, account_index, user_id) VALUES (?, ?, ?)" (sId, n, userId)

rowReturned :: IO [Only Int64] -> IO Bool
rowReturned = fmap (not . null)
