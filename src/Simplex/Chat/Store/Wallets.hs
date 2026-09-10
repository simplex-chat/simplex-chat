{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Simplex.Chat.Store.Wallets
  ( getDeviceSeed,
    getAccountIndex,
    getSeedProfiles,
    createSeed,
    bindAccountIndex,
    deleteSeed,
  )
where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Simplex.Chat.Types (User (..))
import Simplex.Chat.Wallet (AccountIndex, SeedId, WalletSeed (..))
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
#endif

toSeed :: (Int64, ByteString) -> WalletSeed
toSeed (sId, seed) = WalletSeed {wsId = sId, wsEntropy = seed}

getDeviceSeed :: DB.Connection -> IO (Maybe WalletSeed)
getDeviceSeed db =
  maybeFirstRow toSeed $
    DB.query_ db "SELECT wallet_seed_id, seed FROM wallet_seeds ORDER BY wallet_seed_id LIMIT 1"

getAccountIndex :: DB.Connection -> User -> IO (Maybe AccountIndex)
getAccountIndex db User {userId} = do
  r <-
    maybeFirstRow fromOnly $
      DB.query db "SELECT wallet_account_index FROM users WHERE user_id = ?" (Only userId)
  pure $ case r of
    Just (Just ix) -> Just $ fromIntegral (ix :: Int64)
    _ -> Nothing

-- | Hidden profiles are left out, as they are by /users.
getSeedProfiles :: DB.Connection -> SeedId -> User -> IO [Text]
getSeedProfiles db sId User {userId} =
  map fromOnly
    <$> DB.query
      db
      [sql|
        SELECT local_display_name FROM users
        WHERE wallet_seed_id = ? AND user_id != ? AND view_pwd_hash IS NULL
        ORDER BY local_display_name
      |]
      (sId, userId)

bindUser :: DB.Connection -> Int64 -> SeedId -> Int64 -> IO ()
bindUser db uId sId acct =
  DB.execute
    db
    "UPDATE users SET wallet_seed_id = ?, wallet_account_index = ? WHERE user_id = ?"
    (sId, acct, uId)

-- | False if the device already has a key. No profile is bound, as which
-- account a profile takes is said with bind.
createSeed :: DB.Connection -> ByteString -> IO Bool
createSeed db entropy =
  getDeviceSeed db >>= \case
    Just _ -> pure False
    Nothing -> True <$ DB.execute db "INSERT INTO wallet_seeds (seed) VALUES (?)" (Only $ DB.Binary entropy)

-- | Without an account the next free one is taken, which a profile that has
-- one does not need: moving to another account is asked for by number.
bindAccountIndex :: DB.Connection -> User -> SeedId -> Maybe AccountIndex -> IO (Either String ())
bindAccountIndex db user@User {userId} sId = \case
  Nothing ->
    getAccountIndex db user >>= \case
      Just _ -> pure $ Left "this profile already has an account"
      Nothing -> do
        acct <- takeAccountIndex db sId
        -- BIP-32 hardens at 2^31, and every index above it is the same key again
        if acct >= 0x80000000
          then pure $ Left "no free account on this key"
          else Right () <$ bindUser db userId sId acct
  Just acct
    | acct >= 0x80000000 -> pure $ Left "account index too large"
    | otherwise -> do
        taken <-
          maybeFirstRow fromOnly $
            DB.query
              db
              "SELECT 1 FROM users WHERE wallet_seed_id = ? AND wallet_account_index = ? AND user_id != ?"
              (sId, fromIntegral acct :: Int64, userId)
        case (taken :: Maybe Int64) of
          Just _ -> pure $ Left "another profile uses this account"
          Nothing -> do
            bindUser db userId sId (fromIntegral acct)
            -- the counter moves past it, so the next profile is not handed the same one
            setNextAccountIndex db sId (fromIntegral acct + 1)
            pure $ Right ()

-- | So two profiles cannot be handed the same account.
takeAccountIndex :: DB.Connection -> SeedId -> IO Int64
takeAccountIndex db sId = do
  DB.execute db "UPDATE wallet_seeds SET next_account_index = next_account_index + 1 WHERE wallet_seed_id = ?" (Only sId)
  maybe 0 (subtract 1)
    <$> ( maybeFirstRow fromOnly $
            DB.query db "SELECT next_account_index FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)
        )

setNextAccountIndex :: DB.Connection -> SeedId -> Int64 -> IO ()
setNextAccountIndex db sId acct =
  DB.execute
    db
    "UPDATE wallet_seeds SET next_account_index = ? WHERE wallet_seed_id = ? AND next_account_index < ?"
    (acct, sId, acct)

-- | Profiles are unbound first, as the foreign key is ON DELETE RESTRICT.
deleteSeed :: DB.Connection -> SeedId -> IO ()
deleteSeed db sId = do
  DB.execute db "UPDATE users SET wallet_seed_id = NULL, wallet_account_index = NULL WHERE wallet_seed_id = ?" (Only sId)
  DB.execute db "DELETE FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)
