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
import Control.Logger.Simple (logError, logInfo, logWarn)
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

-- | Allows for our clock running ahead of the provider's. Expiring early is safe:
-- an expired invoice can still be marked paid.
expiryGrace :: NominalDiffTime
expiryGrace = 600

skipWarnInterval :: NominalDiffTime
skipWarnInterval = 3600

-- | The most reasons the warning log carries between passes. Each holds an invoice id, so a provider
-- selling through a method this build does not know produces one per invoice.
maxSkipReasons :: Int
maxSkipReasons = 4096

-- | A queue rather than a call, so the webhook route can answer without touching
-- BTCPay and settlement stays on this one thread.
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
    -- when a list last ran, whether the stray cadence or the row count called for it
    peListedAt :: TVar (Maybe UTCTime),
    peStrayEvery :: NominalDiffTime
  }

newPollerEnv :: DBStore -> Waiters -> ReadHints -> [Provider] -> PollConfig -> IO PollerEnv
newPollerEnv peStore peWaiters peHints peProviders pePoll = do
  peSkipped <- newTVarIO M.empty
  peListedAt <- newTVarIO Nothing
  pure PollerEnv {peStore, peWaiters, peHints, peProviders, pePoll, peSkipped, peListedAt, peStrayEvery = strayListInterval}

-- | Providers before the sweep, so an invoice paid minutes ago is not expired over. A pass
-- that could not read a provider does not sweep at all: the payments it failed to fetch are
-- exactly what stops the sweep expiring an invoice with money in it.
runOnePass :: PollerEnv -> IO ()
runOnePass env@PollerEnv {peStore, peProviders} = do
  now <- getCurrentTime
  -- A pass reads what we are waiting on, which we know from our own rows: an idle service asks
  -- the provider nothing at all. Under that is the list, for a payment landing after an invoice
  -- closed and for anything at the provider we have no row for, neither of which needs
  -- three-second latency. Past the threshold below, the list is cheaper than the reads and
  -- stands in for them.
  rows <- unpaidRefs peStore (addUTCTime (negate settleWindow) now)
  -- Past the threshold the list is both cheaper and the pass's own accounting, so it stands in
  -- for the reads rather than running beside them: one list a pass, never two.
  let bulk = length rows > readsPerPass
  listNow <- listDue env now bulk
  accounted <-
    if bulk
      -- The list cannot say a row went unread: it only reports on the providers it was asked
      -- about. So the rows are checked here, the way the read lane checks them one at a time.
      then do
        covered <- and <$> mapM (rowIsCovered env now) rows
        (covered &&) . and <$> mapM (listPass env now) peProviders
      else do
        read' <- readRows env now rows
        -- after the reads, which settle what we are waiting on with the least delay; the net
        -- catches only what they cannot see, and its failure has no bearing on them
        when listNow $ mapM_ (listPass env now) peProviders
        pure read'
  pruneSkipLog env now
  -- gated on whichever lane accounted for our rows; a stray list that failed has no bearing on
  -- them, and it has already logged on its own cadence
  when accounted $ sweepExpired env now

-- | How often the stray list runs. A payment after the close and an invoice we never recorded
-- are both rare and neither is urgent, so this is minutes rather than the pass cadence.
strayListInterval :: NominalDiffTime
strayListInterval = 60

-- | Whether to list this pass: on the stray cadence, or because @forced@ says the pass is
-- listing anyway. A forced list resets the cadence too, since the net has just been cast.
listDue :: PollerEnv -> UTCTime -> Bool -> IO Bool
listDue PollerEnv {peListedAt, peStrayEvery} now forced = atomically $ do
  last' <- readTVar peListedAt
  let due = forced || maybe True (\at -> diffUTCTime now at >= peStrayEvery) last'
  when due $ writeTVar peListedAt (Just now)
  pure due

-- | Past this many open invoices one list is fewer bytes and fewer requests than reading each,
-- so the pass switches to it. Below it, reading only what we are waiting on costs nothing when
-- nobody is paying and never sends us an invoice that settled days ago.
readsPerPass :: Int
readsPerPass = 25

-- | The provider a row names, or Nothing with a line saying so: nothing can account for such a
-- row, and the sweep must not expire other invoices over it. Rate limited like every other
-- provider-wide reason.
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

-- | The invoices we are still waiting on, read one by one by the provider's own id. False when
-- any of them could not be read: the sweep must not expire an invoice whose payment we missed.
readRows :: PollerEnv -> UTCTime -> [(Text, Text)] -> IO Bool
readRows env now rows =
  and <$> mapM (\r -> safelyWith (readWhat r) False (readRow r)) rows
  where
    readWhat (provider, ref) = "reading " <> provider <> " invoice " <> ref
    readRow row@(_, ref) = coveringProvider env now row >>= maybe (pure False) (`readOne` ref)
    -- keyed on the provider, not the invoice: an outage that takes every read with it should
    -- not take the log with it too
    readOne p ref =
      pReadInvoice p ref >>= \case
        Left (ProviderError e) -> do
          due <- dueToWarn env now ("read failed: " <> tshow (pProvider p))
          when due $ logWarn ("badge poller: " <> tshow (pProvider p) <> " reads are failing; every invoice waits for the next pass: " <> e)
          pure False
        Right Nothing -> pure True
        Right (Just signal) -> True <$ settleMoved env (pProvider p) now (ref, signal)

-- | False when this pass cannot account for every invoice we sold: the list failed outright,
-- it came back with one of ours unread, or it stopped before the end and cannot say whose it
-- missed. All three mean a payment may have landed where we cannot see it, and the sweep must
-- not expire an invoice over money it missed.
listPass :: PollerEnv -> UTCTime -> Provider -> IO Bool
listPass env now p =
  pListOpen p >>= \case
    Left (ProviderError e) -> do
      -- rate limited like every other reason: the cadence drops to three seconds while a
      -- browser is holding, and a provider stays down for longer than that. Not keyed on the
      -- error: a network failure prints the whole request, whose startDate moves every pass,
      -- and the limiter would never see one key twice.
      due <- dueToWarn env now ("list failed: " <> tshow (pProvider p))
      when due $ logWarn ("badge poller: " <> tshow (pProvider p) <> " list failed; every invoice waits for the next pass: " <> e)
      pure False
    Right ListPass {lpMoved, lpSkipped} -> do
      -- One invoice that cannot be read must not take the rest of the pass with it: the sweep and
      -- every other invoice are behind these two loops, and a row that throws once throws every
      -- pass. Both failures count as unaccounted for, which holds the sweep back. The skip loop
      -- reads rows too, so it needs the guard as much as the settle loop does.
      owners <- mapM (\s -> safelyWith (skipWhat s) SkipUnaccounted (reportSkip env (pProvider p) now s)) lpSkipped
      settled <- mapM (\m -> safely (settleWhat m) (settleMoved env (pProvider p) now m)) lpMoved
      pure (all (== SkipStranger) owners && and settled)
      where
        settleWhat (ref, _) = "settling " <> tshow (pProvider p) <> " invoice " <> ref
        skipWhat (ref, _) = "reading the skipped " <> tshow (pProvider p) <> " invoice " <> fromMaybe "the provider did not name" ref

-- | provider_ref is unique table-wide, not per provider, so check the provider too.
-- Logs the provider's own reference: our invoice id is a bearer token and stays out of the
-- logs, and the provider ref is what an operator searches to refund the order.
settleMoved :: PollerEnv -> PaymentProvider -> UTCTime -> (Text, PaymentSignal) -> IO ()
settleMoved env@PollerEnv {peStore, peWaiters} provider now (ref, signal) =
  getInvoiceByProviderRef peStore ref >>= \case
    Just InvoiceRow {irInvoiceId, irStatus, irProvider} | irProvider == provider ->
      -- The row we already hold answers this. The provider keeps listing a closed invoice for
      -- days, so once the window fills with finished sales most of a pass is write transactions
      -- opened only to be told there is nothing to write, on the one connection everything shares.
      when (isJust (decide irStatus signal)) $
        settleOrder peStore peWaiters irInvoiceId signal now >>= \case
          Left e -> logError ("badge poller: settling order " <> ref <> " failed: " <> e)
          Right status -> reportSettled irStatus status
    _ -> pure ()
  where
    reportSettled before after
      -- first, so an invoice already at expired still raises it: money can land after the
      -- close, and nobody is prompted to refund unless this is louder than an ordinary expiry
      -- the provider re-lists a closed invoice for days, so this is rate limited like a skip:
      -- one line an hour, rather than one per pass for as long as the money sits there
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
  | -- | The provider named no invoice, so this skip could be any of ours. The list stopping
    -- at its page cap is the one that reaches here, and it hides the rest of the window.
    SkipUnaccounted
  deriving (Eq, Show)

skipOwner :: PollerEnv -> PaymentProvider -> Maybe Text -> IO SkipOwner
skipOwner PollerEnv {peStore} provider = \case
  Nothing -> pure SkipUnaccounted
  Just ref ->
    getInvoiceByProviderRef peStore ref >>= \case
      Just InvoiceRow {irProvider} | irProvider == provider -> pure SkipOurs
      _ -> pure SkipStranger

-- | Answers who the skipped invoice belongs to, which the caller needs on every pass; the
-- warning itself is rate limited, so the log does not repeat every three seconds.
reportSkip :: PollerEnv -> PaymentProvider -> UTCTime -> (Maybe Text, Text) -> IO SkipOwner
reportSkip env provider now (ref, reason) = do
  owner <- skipOwner env provider ref
  due <- dueToWarn env now reason
  when due $ case owner of
    SkipOurs -> logError ("badge poller: an invoice this service sold was not read, so its payment cannot be detected: " <> reason)
    SkipUnaccounted -> logError ("badge poller: part of the window was not read, so a payment to any invoice in it cannot be detected: " <> reason)
    SkipStranger -> logWarn ("badge poller: the list pass could not read everything: " <> reason)
  pure owner

-- | A lookup and at most an insert: this runs once per skipped invoice, and a pass can skip
-- thousands, so anything that touched the whole map here would make the pass quadratic in them.
-- `pruneSkipLog` is what bounds it, once a pass.
dueToWarn :: PollerEnv -> UTCTime -> Text -> IO Bool
dueToWarn PollerEnv {peSkipped} now reason = atomically $ do
  seen <- readTVar peSkipped
  let due = case M.lookup reason seen of
        Nothing -> True
        Just at -> diffUTCTime now at >= skipWarnInterval
  when due $ writeTVar peSkipped (M.insert reason now seen)
  pure due

-- | Once a pass, so its cost is paid once rather than per skipped invoice. Entries past the
-- interval go first; if that is not enough the oldest go too, and they warn again, which is
-- what the cap is worth paying to stay bounded.
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

-- | Both timers start now rather than when a browser arrives, so browsers coming and
-- going cannot make us poll faster than the short cadence.
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

-- | @due@ is checked per hint, not per batch: a hint is a provider read with a 30s
-- timeout, so a redelivery backlog would block the pass for the sum of them.
serveHints :: PollerEnv -> STM () -> IO ()
serveHints env@PollerEnv {peHints = ReadHints q} due = do
  next <- atomically ((Nothing <$ due) `orElse` (Just <$> readTBQueue q))
  case next of
    Nothing -> pure ()
    Just ref -> serveHint env ref >> serveHints env due

-- | Serves what is queued now and returns. 'runPoller' runs the same loop against the pass
-- deadline; only the stop signal differs.
drainHints :: PollerEnv -> IO ()
drainHints env@PollerEnv {peHints = ReadHints q} = serveHints env (isEmptyTBQueue q >>= check)

passSafely :: PollerEnv -> IO ()
passSafely env = void $ safely "the pass" (runOnePass env)

hintSafely :: PollerEnv -> UTCTime -> Text -> IO ()
hintSafely env now ref = void $ safely ("the hinted read of " <> ref) (readHint env now ref)

-- | False when the action failed, for a caller that has to hold something back because of it.
safely :: Text -> IO () -> IO Bool
safely what action = safelyWith what False (True <$ action)

-- | The action's answer, or @fallback@ if it threw. Asynchronous exceptions are rethrown, since
-- that is how the race stops this thread.
safelyWith :: Text -> a -> IO a -> IO a
safelyWith what fallback action =
  try action >>= \case
    Right a -> pure a
    Left (e :: SomeException) -> case fromException e :: Maybe SomeAsyncException of
      Just _ -> throwIO e
      Nothing -> fallback <$ logError ("badge poller: " <> what <> " failed; the next pass will run: " <> tshow e)
