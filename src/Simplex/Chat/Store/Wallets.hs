{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Persistence for wallet seeds and per-profile accounts.
--
-- The schema holds several seeds and binds each chat profile to one of them
-- plus its own account index. Only the single-seed case is reachable from the
-- UI: 'getOrCreateAccountRef' reuses the database's first seed and allocates the
-- next free account index.
module Simplex.Chat.Store.Wallets
  ( getWalletSeeds,
    getWalletSeed,
    createWalletSeed,
    setSeedBackedUp,
    getAccountRef,
    getOrCreateAccountRef,
    boundAccount,
    bindNewAccountOnSeed,
    bindAccount,
    getNextAccountIndex,
    reserveAccounts,
    OneTimeAddress (..),
    recordOneTimeAddress,
    getIncomingAddresses,
    getAcceptedAddresses,
    getOneTimeAddress,
    acceptOneTimeAddress,
    declineOneTimeAddress,
    getScannedTo,
    setScannedTo,
  )
where

import Control.Monad (join, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.Int (Int64)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Chat.Store.Shared (insertedRowId)
import Simplex.Chat.Types (User (..))
import Simplex.Chat.Wallet (AccountIndex, AccountRef (..), Chain, SeedId (..), WalletSeed (..), chainText, parseChain)
import Simplex.Messaging.Agent.Store.AgentStore (maybeFirstRow)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Eth.Address (Address, mkAddress, unAddress)

#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
import Database.PostgreSQL.Simple.SqlQQ (sql)
#else
import Database.SQLite.Simple (Only (..))
import Database.SQLite.Simple.QQ (sql)
#endif

toSeed :: (Int64, ByteString, Bool) -> WalletSeed
toSeed (sId, seed, backedUp) = WalletSeed {wsId = SeedId sId, wsEntropy = seed, wsBackedUp = backedUp}

getWalletSeeds :: DB.Connection -> IO [WalletSeed]
getWalletSeeds db =
  map toSeed
    <$> DB.query_ db "SELECT wallet_seed_id, seed, backed_up FROM wallet_seeds ORDER BY wallet_seed_id"

getWalletSeed :: DB.Connection -> SeedId -> IO (Maybe WalletSeed)
getWalletSeed db (SeedId sId) =
  maybeFirstRow toSeed $
    DB.query db "SELECT wallet_seed_id, seed, backed_up FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)

-- | Insert a seed. Callers generate the entropy; this module never does, so the
-- DRG stays with the agent.
createWalletSeed :: DB.Connection -> ByteString -> IO WalletSeed
createWalletSeed db seed = do
  DB.execute db "INSERT INTO wallet_seeds (seed) VALUES (?)" (Only seed)
  sId <- insertedRowId db
  pure WalletSeed {wsId = SeedId sId, wsEntropy = seed, wsBackedUp = False}

setSeedBackedUp :: DB.Connection -> SeedId -> Bool -> IO ()
setSeedBackedUp db (SeedId sId) backedUp =
  DB.execute db "UPDATE wallet_seeds SET backed_up = ? WHERE wallet_seed_id = ?" (backedUp, sId)

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

-- | The seed and account this profile is bound to, or Nothing if it has never
-- used the wallet. Creates nothing: callers that need a wallet ask the user
-- first, so a profile is never given keys as a side effect of reading.
boundAccount :: DB.Connection -> User -> IO (Maybe (WalletSeed, AccountRef))
boundAccount db user =
  getAccountRef db user >>= \case
    Nothing -> pure Nothing
    Just r -> fmap (\s -> (s, r)) <$> getWalletSeed db (arSeedId r)

-- | Bind this profile to an existing seed at a fresh account index.
--
-- Additive by construction: it inserts nothing into @wallet_seeds@ and rewrites
-- only this profile's row, so no other profile's binding and no stored seed can
-- be affected. The index comes from the seed's high-water mark, so two profiles
-- on one seed can never share an account.
bindNewAccountOnSeed :: DB.Connection -> User -> WalletSeed -> IO AccountRef
bindNewAccountOnSeed db user s = do
  ix <- takeAccountIndex db (wsId s)
  let r = AccountRef {arSeedId = wsId s, arIndex = ix}
  bindAccount db user r
  pure r

-- | Bind this profile to a seed, creating one from @mkSeed@ if the database has
-- none yet, and allocating the next free account index.
--
-- Single-seed by construction: it always picks the first existing seed. When
-- multiple seeds become selectable this is the one function that changes.
getOrCreateAccountRef :: DB.Connection -> User -> IO ByteString -> IO (WalletSeed, AccountRef)
getOrCreateAccountRef db user mkSeed = do
  existing <- getAccountRef db user
  -- Load the seed this profile is actually bound to. Picking the first row in
  -- the table instead would silently re-bind a profile whenever a second seed
  -- exists - which is exactly what importing a recovery key creates - throwing
  -- away the imported key and moving the profile to a new account index, so
  -- the names it already owned stop being derivable too.
  bound <- case existing of
    Just r -> fmap (\s -> (r, s)) <$> getWalletSeed db (arSeedId r)
    Nothing -> pure Nothing
  case bound of
    Just (r, s) -> pure (s, r)
    Nothing -> do
      seeds <- getWalletSeeds db
      s <- case listToMaybe seeds of
        Just s -> pure s
        Nothing -> liftIO mkSeed >>= createWalletSeed db
      ix <- takeAccountIndex db (wsId s)
      let r = AccountRef {arSeedId = wsId s, arIndex = ix}
      bindAccount db user r
      pure (s, r)

-- | Take the next account index and advance the seed's high-water mark.
--
-- The mark is stored rather than computed as @MAX(users.wallet_account_index)@,
-- because after recovery from the phrase alone the @users@ table is empty while
-- accounts @0..N@ already hold names on chain. Computing it would hand the first
-- newly created profile index 0 and, with it, a recovered account's keys and
-- published meta-address. 'reserveAccounts' is what the recovery probe calls to
-- raise the mark past everything it found.
takeAccountIndex :: DB.Connection -> SeedId -> IO AccountIndex
takeAccountIndex db sId@(SeedId sId') = do
  ix <- getNextAccountIndex db sId
  DB.execute db "UPDATE wallet_seeds SET next_account_index = ? WHERE wallet_seed_id = ?" (fromIntegral ix + 1 :: Int64, sId')
  pure ix

getNextAccountIndex :: DB.Connection -> SeedId -> IO AccountIndex
getNextAccountIndex db (SeedId sId) =
  maybe 0 (fromIntegral :: Int64 -> AccountIndex)
    <$> ( maybeFirstRow fromOnly $
            DB.query db "SELECT next_account_index FROM wallet_seeds WHERE wallet_seed_id = ?" (Only sId)
        )

-- | Raise the high-water mark so that @count@ accounts are treated as taken.
-- Called by recovery once the probe has established how many were in use; never
-- lowers it.
reserveAccounts :: DB.Connection -> SeedId -> AccountIndex -> IO ()
reserveAccounts db sId@(SeedId sId') count = do
  cur <- getNextAccountIndex db sId
  when (count > cur) $
    DB.execute db "UPDATE wallet_seeds SET next_account_index = ? WHERE wallet_seed_id = ?" (fromIntegral count :: Int64, sId')

-- One-time addresses.
--
-- Rows are created when a sender's message arrives, or when a recovery scan
-- rediscovers one. They hold no private key: 'ephemeral_pub_key' plus the seed
-- re-derives it, so this table is a cache and losing it costs a rescan rather
-- than an asset.
--
-- 'accepted_at' is NULL until the user accepts. An unaccepted row must never be
-- shown as a name the user owns: accepting is what creates the on-chain link
-- between them and the name, and it is theirs to decline.

data OneTimeAddress = OneTimeAddress
  { otaChain :: Chain,
    otaAddress :: Address,
    otaEphemeralPubKey :: ByteString,
    otaAccepted :: Bool
  }
  deriving (Eq, Show)

toOneTimeAddress :: (Text, ByteString, ByteString, Maybe Text) -> Either String OneTimeAddress
toOneTimeAddress (chain, addr, eph, acceptedAt) = do
  c <- maybe (Left $ "unknown chain: " <> T.unpack chain) Right $ parseChain chain
  a <- mkAddress addr
  pure OneTimeAddress {otaChain = c, otaAddress = a, otaEphemeralPubKey = eph, otaAccepted = isJust acceptedAt}

-- | Record a destination. Idempotent: the same announcement may arrive by
-- message and again by rescan.
recordOneTimeAddress :: DB.Connection -> User -> Chain -> Address -> ByteString -> IO ()
recordOneTimeAddress db User {userId} c addr eph =
  DB.execute
    db
    [sql|
      INSERT INTO wallet_one_time_addresses (user_id, chain, address, ephemeral_pub_key)
      VALUES (?, ?, ?, ?)
      ON CONFLICT (user_id, chain, address) DO NOTHING
    |]
    (userId, chainText c, unAddress addr, eph)

-- | Destinations awaiting a decision.
getIncomingAddresses :: DB.Connection -> User -> Chain -> IO [OneTimeAddress]
getIncomingAddresses db User {userId} c =
  rights . map toOneTimeAddress
    <$> DB.query
      db
      [sql|
        SELECT chain, address, ephemeral_pub_key, accepted_at
        FROM wallet_one_time_addresses
        WHERE user_id = ? AND chain = ? AND accepted_at IS NULL
        ORDER BY wallet_one_time_address_id
      |]
      (userId, chainText c)

-- | Destinations the user accepted. These hold names they own just as much as
-- the main account does, so anything listing "your names" must include them.
getAcceptedAddresses :: DB.Connection -> User -> Chain -> IO [OneTimeAddress]
getAcceptedAddresses db User {userId} c =
  rights . map toOneTimeAddress
    <$> DB.query
      db
      [sql|
        SELECT chain, address, ephemeral_pub_key, accepted_at
        FROM wallet_one_time_addresses
        WHERE user_id = ? AND chain = ? AND accepted_at IS NOT NULL
        ORDER BY wallet_one_time_address_id
      |]
      (userId, chainText c)

getOneTimeAddress :: DB.Connection -> User -> Chain -> Address -> IO (Maybe OneTimeAddress)
getOneTimeAddress db User {userId} c addr = do
  r <-
    maybeFirstRow id $
      DB.query
        db
        [sql|
          SELECT chain, address, ephemeral_pub_key, accepted_at
          FROM wallet_one_time_addresses
          WHERE user_id = ? AND chain = ? AND address = ?
        |]
        (userId, chainText c, unAddress addr)
  pure $ either (const Nothing) Just . toOneTimeAddress =<< r

-- | Accepting is deliberate and, on chain, irreversible in its effect: it is
-- what links this profile to the name.
acceptOneTimeAddress :: DB.Connection -> User -> Chain -> Address -> IO ()
acceptOneTimeAddress db User {userId} c addr =
  DB.execute
    db
    [sql|
      UPDATE wallet_one_time_addresses SET accepted_at = CURRENT_TIMESTAMP
      WHERE user_id = ? AND chain = ? AND address = ? AND accepted_at IS NULL
    |]
    (userId, chainText c, unAddress addr)

-- | Declining touches no chain state, so it is a local delete. A rescan would
-- surface the same destination again, which is correct: the name really is
-- still sitting there.
declineOneTimeAddress :: DB.Connection -> User -> Chain -> Address -> IO ()
declineOneTimeAddress db User {userId} c addr =
  DB.execute
    db
    "DELETE FROM wallet_one_time_addresses WHERE user_id = ? AND chain = ? AND address = ? AND accepted_at IS NULL"
    (userId, chainText c, unAddress addr)

-- | Where the last recovery scan reached, so a repeat scan resumes.
getScannedTo :: DB.Connection -> User -> IO (Maybe Text)
getScannedTo db User {userId} =
  join <$> maybeFirstRow fromOnly (DB.query db "SELECT wallet_scanned_to FROM users WHERE user_id = ?" (Only userId))

setScannedTo :: DB.Connection -> User -> Text -> IO ()
setScannedTo db User {userId} cursor =
  DB.execute db "UPDATE users SET wallet_scanned_to = ? WHERE user_id = ?" (cursor, userId)
