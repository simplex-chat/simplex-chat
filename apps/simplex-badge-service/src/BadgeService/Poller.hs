{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Poller
  ( PollerEnv (..),
    ReadHints,
    newReadHints,
    queueReadHint,
    hintQueueSize,
    newPollerEnv,
    runOnePass,
    drainHints,
    passDue,
    runPoller,
    passDelay,
    SkipOwner (..),
    skipOwner,
    dueToWarn,
    expiryGrace,
    readsPerPass,
    skipWarnInterval,
    maxSkipReasons,
  )
where

import BadgeService.Config (PollConfig (..))
import BadgeService.Orders (decide, settleOrder)
import BadgeService.Providers (ListPass (..), PaymentSignal (..), Provider (..), ProviderError (..), Received (..), settleWindow)
import BadgeService.Store.Invoices (InvoiceRow (..), expireOverdue, getInvoiceByProviderRef, providerText, unpaidRefs)
import BadgeService.Waiters (Waiters, publish, waitingCount, waitingCountSTM)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import BadgeService.Log (logError, logInfo, logWarn)
import Control.Monad (forever, unless, void, when)
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Numeric.Natural (Natural)
import Simplex.Chat.PaymentService.Types (InvoiceStatus (..), PaymentProvider)
import Simplex.Messaging.Agent.Store.Common (DBStore)
import Simplex.Messaging.Util (tshow)

-- | Allows for our clock running ahead of the provider's; an expired invoice can still be marked paid.
expiryGrace :: NominalDiffTime
expiryGrace = 600

skipWarnInterval :: NominalDiffTime
skipWarnInterval = 3600

maxSkipReasons :: Int
maxSkipReasons = 4096

-- | A queue, not a call, so settlement stays on this one thread.
newtype ReadHints = ReadHints (TBQueue Text)

hintQueueSize :: Natural
hintQueueSize = 256

newReadHints :: IO ReadHints
newReadHints = ReadHints <$> newTBQueueIO hintQueueSize

queueReadHint :: ReadHints -> Text -> IO Bool
queueReadHint (ReadHints q) ref = atomically $ do
  full <- isFullTBQueue q
  if full then pure False else True <$ writeTBQueue q ref

minCadenceSeconds :: Int
minCadenceSeconds = 1

data PollerEnv = PollerEnv
  { peStore :: DBStore,
    peWaiters :: Waiters,
    peHints :: ReadHints,
    peProviders :: [Provider],
    pePoll :: PollConfig,
    peSkipped :: TVar (Map Text UTCTime),
    peListedAt :: TVar (Maybe UTCTime),
    peStrayEvery :: NominalDiffTime
  }

newPollerEnv :: DBStore -> Waiters -> ReadHints -> [Provider] -> PollConfig -> IO PollerEnv
newPollerEnv peStore peWaiters peHints peProviders pePoll = do
  peSkipped <- newTVarIO M.empty
  peListedAt <- newTVarIO Nothing
  pure PollerEnv {peStore, peWaiters, peHints, peProviders, pePoll, peSkipped, peListedAt, peStrayEvery = strayListInterval}

-- | Read providers before the sweep, and skip the sweep entirely when a read failed, so an
-- invoice with money in it is never expired.
runOnePass :: PollerEnv -> IO ()
runOnePass env@PollerEnv {peStore, peProviders} = do
  now <- getCurrentTime
  rows <- unpaidRefs peStore (addUTCTime (negate settleWindow) now)
  let bulk = length rows > readsPerPass
  listNow <- listDue env now bulk
  accounted <-
    if bulk
      then do
        covered <- and <$> mapM (rowIsCovered env now) rows
        (covered &&) . and <$> mapM (listPass env now) peProviders
      else do
        read' <- readRows env now rows
        when listNow $ mapM_ (listPass env now) peProviders
        pure read'
  pruneSkipLog env now
  when (accounted && not (null peProviders)) $ sweepExpired env now

-- | The cases the stray list catches are rare and not urgent, so minutes, not the pass cadence.
strayListInterval :: NominalDiffTime
strayListInterval = 60

listDue :: PollerEnv -> UTCTime -> Bool -> IO Bool
listDue PollerEnv {peListedAt, peStrayEvery} now forced = atomically $ do
  last' <- readTVar peListedAt
  let due = forced || maybe True (\at -> diffUTCTime now at >= peStrayEvery) last'
  when due $ writeTVar peListedAt (Just now)
  pure due

-- | Past this many open invoices one list is fewer bytes and requests than reading each.
readsPerPass :: Int
readsPerPass = 25

coveringProvider :: PollerEnv -> UTCTime -> (Text, Text) -> IO (Maybe Provider)
coveringProvider env@PollerEnv {peProviders} now (provider, ref) =
  case find ((== provider) . providerText . pProvider) peProviders of
    Just p -> pure (Just p)
    Nothing -> do
      due <- dueToWarn env now ("no provider for " <> provider)
      when due $ logError ("badge poller: invoice " <> ref <> " names provider " <> provider <> ", which this build has none of")
      pure Nothing

rowIsCovered :: PollerEnv -> UTCTime -> (Text, Text) -> IO Bool
rowIsCovered env now row = isJust <$> coveringProvider env now row

readRows :: PollerEnv -> UTCTime -> [(Text, Text)] -> IO Bool
readRows env now rows =
  and <$> mapM (\r -> safelyWith (readWhat r) False (readRow r)) rows
  where
    readWhat (provider, ref) = "reading " <> provider <> " invoice " <> ref
    readRow row@(_, ref) = coveringProvider env now row >>= maybe (pure False) (`readOne` ref)
    readOne p ref =
      pReadInvoice p ref >>= \case
        Left (ProviderError e) -> do
          due <- dueToWarn env now ("read failed: " <> tshow (pProvider p))
          when due $ logWarn ("badge poller: " <> tshow (pProvider p) <> " reads are failing; every invoice waits for the next pass: " <> e)
          pure False
        Right Nothing -> pure True
        Right (Just signal) -> True <$ settleMoved env (pProvider p) now (ref, signal)

-- | False when the pass cannot account for every invoice sold, so the sweep does not expire one
-- over money it missed.
listPass :: PollerEnv -> UTCTime -> Provider -> IO Bool
listPass env now p =
  pListOpen p >>= \case
    Left (ProviderError e) -> do
      due <- dueToWarn env now ("list failed: " <> tshow (pProvider p))
      when due $ logWarn ("badge poller: " <> tshow (pProvider p) <> " list failed; every invoice waits for the next pass: " <> e)
      pure False
    Right ListPass {lpMoved, lpSkipped} -> do
      owners <- mapM (\s -> safelyWith (skipWhat s) SkipUnaccounted (reportSkip env (pProvider p) now s)) lpSkipped
      settled <- mapM (\m -> safely (settleWhat m) (settleMoved env (pProvider p) now m)) lpMoved
      pure (all (== SkipStranger) owners && and settled)
      where
        settleWhat (ref, _) = "settling " <> tshow (pProvider p) <> " invoice " <> ref
        skipWhat (ref, _) = "reading the skipped " <> tshow (pProvider p) <> " invoice " <> fromMaybe "the provider did not name" ref

-- | provider_ref is unique table-wide, not per provider, so check the provider too.
settleMoved :: PollerEnv -> PaymentProvider -> UTCTime -> (Text, PaymentSignal) -> IO ()
settleMoved env@PollerEnv {peStore, peWaiters} provider now (ref, signal) =
  getInvoiceByProviderRef peStore ref >>= \case
    Just InvoiceRow {irInvoiceId, irStatus, irProvider} | irProvider == provider ->
      when (isJust (decide irStatus signal)) $
        settleOrder peStore peWaiters irInvoiceId signal now >>= \case
          Left e -> logError ("badge poller: settling order " <> ref <> " failed: " <> e)
          Right status -> reportSettled irStatus status
    _ -> pure ()
  where
    reportSettled before after
      -- checked before the no-change case, so a refund still alerts when money lands after expiry
      | after == ISExpired, SigClosed Received {rcvCrypto = Just paid} <- signal = do
          let alert = "badge poller: order " <> ref <> " expired holding " <> paid <> ", which needs a refund"
          due <- dueToWarn env now alert
          when due $ logError alert
      | before == after = pure ()
      | otherwise = logInfo ("badge poller: order " <> ref <> " " <> tshow before <> " -> " <> tshow after)

serveHint :: PollerEnv -> Text -> IO ()
serveHint env ref = do
  now <- getCurrentTime
  hintSafely env now ref

readHint :: PollerEnv -> UTCTime -> Text -> IO ()
readHint env@PollerEnv {peStore, peProviders} now ref =
  getInvoiceByProviderRef peStore ref >>= \case
    Nothing -> pure ()
    Just InvoiceRow {irProvider} -> case find ((== irProvider) . pProvider) peProviders of
      Nothing -> pure ()
      Just p ->
        pReadInvoice p ref >>= \case
          Left (ProviderError e) ->
            logWarn ("badge poller: the hinted read of " <> ref <> " failed; the next pass will run: " <> e)
          Right Nothing -> pure ()
          Right (Just signal) -> settleMoved env irProvider now (ref, signal)

sweepExpired :: PollerEnv -> UTCTime -> IO ()
sweepExpired PollerEnv {peStore, peWaiters} now = do
  expired <- expireOverdue peStore (addUTCTime (negate expiryGrace) now)
  -- publish only after the write commits, or a woken reader will not see it
  unless (null expired) $ do
    atomically $ mapM_ (\invId -> publish peWaiters invId ISExpired) expired
    logInfo ("badge poller: expired " <> tshow (length expired) <> " invoice(s) past their window")

data SkipOwner
  = SkipOurs
  | SkipStranger
  | -- | The provider named no invoice, so this skip could be any of ours.
    SkipUnaccounted
  deriving (Eq, Show)

skipOwner :: PollerEnv -> PaymentProvider -> Maybe Text -> IO SkipOwner
skipOwner PollerEnv {peStore} provider = \case
  Nothing -> pure SkipUnaccounted
  Just ref ->
    getInvoiceByProviderRef peStore ref >>= \case
      Just InvoiceRow {irProvider} | irProvider == provider -> pure SkipOurs
      _ -> pure SkipStranger

reportSkip :: PollerEnv -> PaymentProvider -> UTCTime -> (Maybe Text, Text) -> IO SkipOwner
reportSkip env provider now (ref, reason) = do
  owner <- skipOwner env provider ref
  due <- dueToWarn env now reason
  when due $ case owner of
    SkipOurs -> logError ("badge poller: an invoice this service sold was not read, so its payment cannot be detected: " <> reason)
    SkipUnaccounted -> logError ("badge poller: part of the window was not read, so a payment to any invoice in it cannot be detected: " <> reason)
    SkipStranger -> logWarn ("badge poller: the list pass could not read everything: " <> reason)
  pure owner

dueToWarn :: PollerEnv -> UTCTime -> Text -> IO Bool
dueToWarn PollerEnv {peSkipped} now reason = atomically $ do
  seen <- readTVar peSkipped
  let due = case M.lookup reason seen of
        Nothing -> True
        Just at -> diffUTCTime now at >= skipWarnInterval
  when due $ writeTVar peSkipped (M.insert reason now seen)
  pure due

pruneSkipLog :: PollerEnv -> UTCTime -> IO ()
pruneSkipLog PollerEnv {peSkipped} now = atomically $ modifyTVar' peSkipped prune
  where
    prune seen
      | M.size seen <= maxSkipReasons = seen
      | M.size fresh <= maxSkipReasons = fresh
      | otherwise = M.fromList (drop (M.size fresh - maxSkipReasons) (sortOn snd (M.toList fresh)))
      where
        fresh = M.filter (\at -> diffUTCTime now at < skipWarnInterval) seen

passDelay :: PollConfig -> Int -> Int
passDelay PollConfig {pWaitingSeconds, pIdleSeconds} waiting =
  1000000 * max minCadenceSeconds (if waiting > 0 then pWaitingSeconds else pIdleSeconds)

waitingDelay :: PollConfig -> Int
waitingDelay cfg = passDelay cfg 1

-- | Both timers start now, not when a browser arrives, so browsers cannot make us poll faster than the short cadence.
passDue :: PollerEnv -> IO (STM ())
passDue PollerEnv {peWaiters, pePoll} = do
  waiting <- waitingCount peWaiters
  soonest <- registerDelay (waitingDelay pePoll)
  full <- registerDelay (passDelay pePoll waiting)
  pure $
    (readTVar full >>= check)
      `orElse` ( do
                   readTVar soonest >>= check
                   waitingCountSTM peWaiters >>= \n -> check (n > 0)
               )

runPoller :: PollerEnv -> IO ()
runPoller env = forever $ do
  passSafely env
  passDue env >>= serveHints env

-- | The deadline is checked per hint, not per batch, so a redelivery backlog cannot block the pass for the sum of their timeouts.
serveHints :: PollerEnv -> STM () -> IO ()
serveHints env@PollerEnv {peHints = ReadHints q} due = do
  next <- atomically ((Nothing <$ due) `orElse` (Just <$> readTBQueue q))
  case next of
    Nothing -> pure ()
    Just ref -> serveHint env ref >> serveHints env due

drainHints :: PollerEnv -> IO ()
drainHints env@PollerEnv {peHints = ReadHints q} = serveHints env (isEmptyTBQueue q >>= check)

passSafely :: PollerEnv -> IO ()
passSafely env = void $ safely "the pass" (runOnePass env)

hintSafely :: PollerEnv -> UTCTime -> Text -> IO ()
hintSafely env now ref = void $ safely ("the hinted read of " <> ref) (readHint env now ref)

safely :: Text -> IO () -> IO Bool
safely what action = safelyWith what False (True <$ action)

-- | Asynchronous exceptions are rethrown, since that is how the race stops this thread.
safelyWith :: Text -> a -> IO a -> IO a
safelyWith what fallback action =
  try action >>= \case
    Right a -> pure a
    Left (e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
      Just _ -> throwIO e
      Nothing -> fallback <$ logError ("badge poller: " <> what <> " failed; the next pass will run: " <> tshow e)
