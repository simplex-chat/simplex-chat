{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
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
    cancelGiveUpDelay,
    readsPerPass,
    skipWarnInterval,
    maxSkipReasons,
  )
where

import BadgeService.Config (PollConfig (..))
import BadgeService.Orders (decide, settleOrder)
import BadgeService.Providers (ListPass (..), PaymentSignal (..), Provider (..), ProviderError (..), Received (..), expiresItself, settleWindow)
import BadgeService.Store.Invoices (InvoiceRow (..), OverdueInvoice (..), expireOverdue, getInvoiceByProviderRef, overdueInvoices, providerText, unpaidRefs)
import BadgeService.Waiters (Waiters, publish, waitingCount, waitingCountSTM)
import Control.Concurrent.STM
import Control.Exception (SomeAsyncException, SomeException, fromException, throwIO, try)
import BadgeService.Log (logError, logInfo, logWarn)
import Control.Monad (filterM, forever, unless, void, when)
import Data.List (find, partition, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Numeric.Natural (Natural)
import Simplex.Chat.PaymentService.Types (InvoiceStatus (..), PaymentProvider (..))
import Simplex.Messaging.Agent.Store.Common (DBStore)
import Simplex.Messaging.Util (tshow)

-- | Allows for our clock running ahead of the provider's; an expired invoice can still be marked paid.
expiryGrace :: NominalDiffTime
expiryGrace = 600

-- | Give up on a failing cancel only after an hour, not on the first failure.
cancelGiveUpDelay :: NominalDiffTime
cancelGiveUpDelay = 3600

skipWarnInterval :: NominalDiffTime
skipWarnInterval = 3600

maxSkipReasons :: Int
maxSkipReasons = 4096

-- | Webhooks queue reads here, so all settling happens on the poller thread.
-- An order already waiting in the queue is not added again, so repeated webhooks don't pile up reads.
data ReadHints = ReadHints (TBQueue Text) (TVar (Set Text))

hintQueueSize :: Natural
hintQueueSize = 256

newReadHints :: IO ReadHints
newReadHints = ReadHints <$> newTBQueueIO hintQueueSize <*> newTVarIO S.empty

queueReadHint :: ReadHints -> Text -> IO Bool
queueReadHint (ReadHints q queued) ref = atomically $ do
  already <- S.member ref <$> readTVar queued
  full <- isFullTBQueue q
  if
    | already -> pure True
    | full -> pure False
    | otherwise -> do
        writeTBQueue q ref
        modifyTVar' queued (S.insert ref)
        pure True

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
    peStrayEvery :: NominalDiffTime,
    -- When the cancel first failed, for orders older than 72 hours.
    peCancelFailed :: TVar (Map Text UTCTime)
  }

newPollerEnv :: DBStore -> Waiters -> ReadHints -> [Provider] -> PollConfig -> IO PollerEnv
newPollerEnv peStore peWaiters peHints peProviders pePoll = do
  peSkipped <- newTVarIO M.empty
  peListedAt <- newTVarIO Nothing
  peCancelFailed <- newTVarIO M.empty
  pure PollerEnv {peStore, peWaiters, peHints, peProviders, pePoll, peSkipped, peListedAt, peStrayEvery = strayListInterval, peCancelFailed}

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
      reportStrangers env (pProvider p) now [reason | (SkipStranger, (_, reason)) <- zip owners lpSkipped]
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
sweepExpired env@PollerEnv {peStore, peWaiters, peCancelFailed} now = do
  let cutoff = addUTCTime (negate expiryGrace) now
  (selfExpiring, payable) <- partition (expiresItself . oiProvider) <$> overdueInvoices peStore cutoff
  atomically $ modifyTVar' peCancelFailed (`M.restrictKeys` S.fromList (map oiProviderRef payable))
  -- Cancel only a few orders per pass, so a long list does not block other work.
  cancelled <- filterM (closedAtProvider env now) (take readsPerPass payable)
  expired <- expireOverdue peStore cutoff (map oiInvoiceId (selfExpiring <> cancelled))
  -- publish only after the write commits, or a woken reader will not see it
  unless (null expired) $ do
    atomically $ mapM_ (\invId -> publish peWaiters invId ISExpired) expired
    logInfo ("badge poller: expired " <> tshow (length expired) <> " invoice(s) past their window")

-- | Cancel the order at the provider first, so the buyer can no longer pay it.
closedAtProvider :: PollerEnv -> UTCTime -> OverdueInvoice -> IO Bool
closedAtProvider env@PollerEnv {peProviders} now oi@OverdueInvoice {oiProvider, oiProviderRef = ref} =
  safelyWith ("cancelling " <> tshow oiProvider <> " invoice " <> ref) False $
    case find ((== oiProvider) . pProvider) peProviders of
      Nothing -> uncancelled env now oi "no provider is configured for it"
      Just p ->
        pCancelInvoice p ref >>= \case
          Right () -> pure True
          -- The provider won't cancel an order that is already paid or closed. Reading it shows which.
          Left (ProviderError cancelError) ->
            pReadInvoice p ref >>= \case
              Right (Just signal) -> False <$ settleMoved env oiProvider now (ref, signal)
              Right Nothing -> uncancelled env now oi cancelError
              Left (ProviderError readError) -> uncancelled env now oi (cancelError <> "; reading it failed too: " <> readError)

-- | After 72 hours the poller only reads an order when a webhook asks. If an older order still
-- can't be cancelled after an hour of trying, mark it expired here and log an error.
uncancelled :: PollerEnv -> UTCTime -> OverdueInvoice -> Text -> IO Bool
uncancelled env@PollerEnv {peCancelFailed} now OverdueInvoice {oiProvider, oiProviderRef = ref, oiCreatedAt} e
  | diffUTCTime now oiCreatedAt <= settleWindow = keepOpen "it stays open until a cancel succeeds or it settles"
  | otherwise = do
      firstFailed <- atomically $ stateTVar peCancelFailed $ \failed -> (M.lookup ref failed, M.insertWith (\_ first -> first) ref now failed)
      case firstFailed of
        Just at | diffUTCTime now at >= cancelGiveUpDelay -> do
          logError ("badge poller: overdue " <> tshow oiProvider <> " invoice " <> ref <> " could not be cancelled at the provider, so it was expired here only; check it in the provider's dashboard: " <> e)
          pure True
        _ -> keepOpen "it is expired here if the cancel still fails an hour after its first failure"
  where
    keepOpen outcome = do
      due <- dueToWarn env now ("cancel failed: " <> tshow oiProvider <> " invoice " <> ref)
      when due $ logWarn ("badge poller: cancelling overdue " <> tshow oiProvider <> " invoice " <> ref <> " failed; " <> outcome <> ": " <> e)
      pure False

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
  let raise msg = dueToWarn env now reason >>= (`when` logError (msg <> reason))
  case owner of
    SkipOurs -> raise "badge poller: an invoice this service sold was not read, so its payment cannot be detected: "
    SkipUnaccounted -> raise "badge poller: part of the window was not read, so a payment to any invoice in it cannot be detected: "
    SkipStranger -> pure ()
  pure owner

-- | One limiter key per provider, so a store full of invoices sold elsewhere costs one line an hour, not one per invoice.
reportStrangers :: PollerEnv -> PaymentProvider -> UTCTime -> [Text] -> IO ()
reportStrangers env provider now = \case
  [] -> pure ()
  reasons@(example : _) -> do
    due <- dueToWarn env now ("stranger skips: " <> tshow provider)
    when due $ logWarn ("badge poller: " <> tshow (length reasons) <> " unreadable invoice(s) not created by this service; use a dedicated " <> home <> "; first: " <> example)
  where
    home = case provider of
      PPCrypto -> "BTCPay store"
      PPStripe -> "Stripe account"
      other -> providerText other <> " account"

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
serveHints env@PollerEnv {peHints = ReadHints q queued} due = do
  next <- atomically ((Nothing <$ due) `orElse` (Just <$> takeHint))
  case next of
    Nothing -> pure ()
    Just ref -> serveHint env ref >> serveHints env due
  where
    takeHint = do
      ref <- readTBQueue q
      modifyTVar' queued (S.delete ref)
      pure ref

drainHints :: PollerEnv -> IO ()
drainHints env@PollerEnv {peHints = ReadHints q _} = serveHints env (isEmptyTBQueue q >>= check)

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
