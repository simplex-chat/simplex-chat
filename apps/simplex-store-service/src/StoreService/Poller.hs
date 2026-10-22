{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StoreService.Poller
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
    cleanupInterval,
    cleanupReadsPerPass,
    unpaidRetention,
    cleanupUnpaid,
    creditPass,
    forgetRates,
  )
where

import StoreService.Config (PollConfig (..))
import StoreService.Names.LateCredits (CreditOutcome (..), creditLateInvoice)
import StoreService.Names.Phases (Phases, RegistrationId (..))
import StoreService.Names.Store (CleanupKept (..), LateAlert (..), LateCredit (..), LateCreditState (..), LatePending (..), LateSupport (..), claimLateAlert, deleteUnpaidRegistration, lateAlertDue, latePending, uncreditedLateInvoices, unpaidCleanupCandidates)
import StoreService.Orders (decide, expireOverdue, settleOrder)
import StoreService.Providers (ListPass (..), PaymentSignal (..), Provider (..), ProviderError (..), Rate (..), Received (..), expiresItself, settleWindow)
import StoreService.Store.Invoices (InvoiceProduct (..), InvoiceRow (..), OverdueInvoice (..), cryptoCurrencyText, getInvoice, getInvoiceByProviderRef, overdueInvoices, providerText, unpaidRefs)
import StoreService.Waiters (Waiters, waitingCount, waitingCountSTM)
import Control.Concurrent.STM
import StoreService.Log (logError, logInfo, logWarn, safely, safelyWith)
import Control.Monad (filterM, forM_, forever, unless, void, when)
import Data.List (find, partition, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Numeric.Natural (Natural)
import Simplex.Chat.PaymentService.Types (CryptoCurrency (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Util (tshow)

-- | Allows for our clock running ahead of the provider's; an expired invoice can still be marked paid.
expiryGrace :: NominalDiffTime
expiryGrace = 600

-- | How often unpaid name orders are cleaned up; a provider that does not answer keeps an order until the next run.
cleanupInterval :: NominalDiffTime
cleanupInterval = 24 * 3600

-- | The provider reads one pass's cleanup makes, so settlement reads are not held up; the rest wait for the next pass.
cleanupReadsPerPass :: Int
cleanupReadsPerPass = 25

-- | How long an unpaid name order is kept after its invoice expired.
unpaidRetention :: NominalDiffTime
unpaidRetention = 30 * 24 * 3600

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
    -- | Nothing when the service sells no names.
    pePhases :: Maybe Phases,
    peHints :: ReadHints,
    peProviders :: [Provider],
    pePoll :: PollConfig,
    peSkipped :: TVar (Map Text UTCTime),
    peListedAt :: TVar (Maybe UTCTime),
    peStrayEvery :: NominalDiffTime,
    -- When the cancel first failed, for orders older than 72 hours.
    peCancelFailed :: TVar (Map Text UTCTime),
    peCleanedAt :: TVar (Maybe UTCTime),
    -- | The orders of the day's cleanup not read yet.
    peCleanupLeft :: TVar [(RegistrationId, InvoiceId)],
    peCreditedAt :: TVar (Maybe UTCTime),
    -- | The rates read in this pass or hinted read, by provider and coin.
    peRates :: TVar (Map (Text, Text) (Either ProviderError Rate))
  }

newPollerEnv :: DBStore -> Waiters -> Maybe Phases -> ReadHints -> [Provider] -> PollConfig -> IO PollerEnv
newPollerEnv peStore peWaiters pePhases peHints peProviders pePoll = do
  peSkipped <- newTVarIO M.empty
  peListedAt <- newTVarIO Nothing
  peCancelFailed <- newTVarIO M.empty
  peCleanedAt <- newTVarIO Nothing
  peCleanupLeft <- newTVarIO []
  peCreditedAt <- newTVarIO Nothing
  peRates <- newTVarIO M.empty
  pure PollerEnv {peStore, peWaiters, pePhases, peHints, peProviders, pePoll, peSkipped, peListedAt, peStrayEvery = strayListInterval, peCancelFailed, peCleanedAt, peCleanupLeft, peCreditedAt, peRates}

-- | Read providers before the sweep, and leave out of the sweep every invoice whose read or settlement failed,
-- so an invoice with money in it is never expired. A list that cannot account for every invoice skips the sweep.
runOnePass :: PollerEnv -> IO ()
runOnePass env@PollerEnv {peStore, peProviders} = do
  now <- getCurrentTime
  forgetRates env
  rows <- unpaidRefs peStore (addUTCTime (negate settleWindow) now)
  let bulk = length rows > readsPerPass
  listNow <- listDue env now bulk
  missed <-
    if bulk
      then do
        uncovered <- S.fromList . map snd <$> filterM (fmap not . rowIsCovered env now) rows
        fmap (S.unions . (uncovered :)) . sequence <$> mapM (listPass env now) peProviders
      else do
        missed <- readRows env now rows
        when listNow $ mapM_ (listPass env now) peProviders
        pure (Just missed)
  pruneSkipLog env now
  -- the cleanup runs whatever the sweep met
  unless (null peProviders) $ forM_ missed $ \refs -> safely "badge poller: the sweep" (sweepExpired env now refs)
  void $ safely "names: the late credit pass" (creditPass env now)
  cleanupUnpaid env now

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

-- | The refs whose read or settlement failed.
readRows :: PollerEnv -> UTCTime -> [(Text, Text)] -> IO (Set Text)
readRows env now rows =
  S.fromList . map snd <$> filterM (\r -> not <$> safelyWith (readWhat r) False (readRow r)) rows
  where
    readWhat (provider, ref) = "badge poller: reading " <> provider <> " invoice " <> ref
    readRow row@(_, ref) = coveringProvider env now row >>= maybe (pure False) (`readOne` ref)
    readOne p ref =
      pReadInvoice p ref >>= \case
        Left (ProviderError e) -> do
          due <- dueToWarn env now ("read failed: " <> tshow (pProvider p))
          when due $ logWarn ("badge poller: " <> tshow (pProvider p) <> " reads are failing; every invoice waits for the next pass: " <> e)
          pure False
        Right Nothing -> pure True
        Right (Just signal) -> True <$ settleMoved env (pProvider p) now (ref, signal)

-- | The refs of ours the list did not account for, so the sweep does not expire one over money it missed;
-- Nothing when it cannot name them all.
listPass :: PollerEnv -> UTCTime -> Provider -> IO (Maybe (Set Text))
listPass env now p =
  pListOpen p >>= \case
    Left (ProviderError e) -> do
      due <- dueToWarn env now ("list failed: " <> tshow (pProvider p))
      when due $ logWarn ("badge poller: " <> tshow (pProvider p) <> " list failed; every invoice waits for the next pass: " <> e)
      pure Nothing
    Right ListPass {lpMoved, lpSkipped} -> do
      owners <- mapM (\s -> safelyWith (skipWhat s) SkipUnaccounted (reportSkip env (pProvider p) now s)) lpSkipped
      reportStrangers env (pProvider p) now [reason | (SkipStranger, (_, reason)) <- zip owners lpSkipped]
      unsettled <- filterM (\m -> not <$> safely (settleWhat m) (settleMoved env (pProvider p) now m)) lpMoved
      let skippedOurs = [ref | (SkipOurs, (ref, _)) <- zip owners lpSkipped]
      pure $
        if SkipUnaccounted `elem` owners
          then Nothing
          else Just $ S.fromList $ map fst unsettled <> catMaybes skippedOurs
      where
        settleWhat (ref, _) = "badge poller: settling " <> tshow (pProvider p) <> " invoice " <> ref
        skipWhat (ref, _) = "badge poller: reading the skipped " <> tshow (pProvider p) <> " invoice " <> fromMaybe "the provider did not name" ref

-- | provider_ref is unique table-wide, not per provider, so check the provider too.
-- A name invoice's late money is credited after the settlement that recorded it commits.
settleMoved :: PollerEnv -> PaymentProvider -> UTCTime -> (Text, PaymentSignal) -> IO ()
settleMoved env@PollerEnv {peStore, peWaiters, pePhases, peProviders} provider now (ref, signal) =
  getInvoiceByProviderRef peStore ref >>= \case
    Just InvoiceRow {irInvoiceId, irStatus, irProvider, irProduct} | irProvider == provider ->
      when (isJust (decide irStatus signal)) $
        settleOrder peStore peWaiters pePhases irInvoiceId signal now >>= \case
          Left e -> logError ("badge poller: settling order " <> ref <> " failed: " <> e)
          Right status -> do
            reportSettled irProduct irStatus status
            case (irProduct, find ((== provider) . pProvider) peProviders) of
              (IPName _, Just p) ->
                creditLate env p now irInvoiceId >>= \case
                  CONothingDue -> pendingLate env irInvoiceId
                  _ -> pure ()
              _ -> pure ()
    _ -> pure ()
  where
    reportSettled sold before after
      -- checked before the no-change case, so a refund still alerts when money lands after expiry;
      -- a name order's late payment is credited to its balance instead
      | IPBadge {} <- sold, after == ISExpired, SigClosed Received {rcvCrypto = Just paid} <- signal = do
          let alert = "badge poller: order " <> ref <> " expired holding " <> paid <> ", which needs a refund"
          due <- dueToWarn env now alert
          when due $ logError alert
      | before == after = pure ()
      | otherwise = logInfo ("badge poller: order " <> ref <> " " <> tshow before <> " -> " <> tshow after)

serveHint :: PollerEnv -> Text -> IO ()
serveHint env ref = do
  now <- getCurrentTime
  forgetRates env
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

-- | Leaves out the invoices whose read or settlement failed this pass.
sweepExpired :: PollerEnv -> UTCTime -> Set Text -> IO ()
sweepExpired env@PollerEnv {peStore, peWaiters, pePhases, peCancelFailed} now missed = do
  let cutoff = addUTCTime (negate expiryGrace) now
  (selfExpiring, payable) <- partition (expiresItself . oiProvider) . filter ((`S.notMember` missed) . oiProviderRef) <$> overdueInvoices peStore cutoff
  atomically $ modifyTVar' peCancelFailed (`M.restrictKeys` S.fromList (map oiProviderRef payable))
  -- Cancel only a few orders per pass, so a long list does not block other work.
  cancelled <- filterM (closedAtProvider env now) (take readsPerPass payable)
  expired <- expireOverdue peStore peWaiters pePhases now cutoff (map oiInvoiceId (selfExpiring <> cancelled))
  unless (null expired) $ logInfo ("badge poller: expired " <> tshow (length expired) <> " invoice(s) past their window")

cleanupDue :: PollerEnv -> UTCTime -> IO Bool
cleanupDue PollerEnv {peCleanedAt} now = atomically $ do
  due <- maybe True (\at -> diffUTCTime now at >= cleanupInterval) <$> readTVar peCleanedAt
  when due $ writeTVar peCleanedAt (Just now)
  pure due

-- | The poller reads an invoice only within settleWindow of its creation, so each order's invoice is read once more
-- before the order goes: a payment whose webhook was lost is recorded then, and the order is kept with its balance.
-- Each pass reads at most cleanupReadsPerPass of the day's orders. The day's alerts on late money go with it.
cleanupUnpaid :: PollerEnv -> UTCTime -> IO ()
cleanupUnpaid env@PollerEnv {peStore, peCleanupLeft} now = do
  due <- cleanupDue env now
  when due $ do
    withTransaction peStore (`unpaidCleanupCandidates` addUTCTime (negate unpaidRetention) now) >>= atomically . writeTVar peCleanupLeft
    void $ safely "names: the late payment alerts" (alertLatePayments env now)
  candidates <- atomically $ stateTVar peCleanupLeft (splitAt cleanupReadsPerPass)
  deleted <- filterM (\c@(RegistrationId rid, _) -> safelyWith ("names cleanup: registration " <> rid) False (cleanOne c)) candidates
  unless (null deleted) $ logInfo ("names cleanup: deleted " <> tshow (length deleted) <> " unpaid order(s)")
  where
    cleanOne (rid@(RegistrationId ridText), invId@(InvoiceId invText)) =
      getInvoice peStore invId >>= \case
        Nothing -> pure False
        Just InvoiceRow {irProvider, irProviderRef} -> case find ((== irProvider) . pProvider) (peProviders env) of
          Nothing -> keep ("no " <> tshow irProvider <> " provider is configured to read invoice " <> invText)
          Just p ->
            pReadInvoice p irProviderRef >>= \case
              Left (ProviderError e) -> keep ("reading invoice " <> invText <> " failed: " <> e)
              Right signal_ -> do
                mapM_ (\signal -> settleMoved env irProvider now (irProviderRef, signal)) signal_
                withTransaction peStore (`deleteUnpaidRegistration` rid) >>= \case
                  Right () -> pure True
                  Left CKPaid -> False <$ logInfo ("names cleanup: unpaid registration " <> ridText <> " is kept with its balance, because invoice " <> invText <> " received a payment")
                  Left (CKHolder (RegistrationId source)) -> False <$ logError ("names cleanup: unpaid registration " <> ridText <> " is kept because it still holds the balance of registration " <> source)
      where
        keep why = False <$ logWarn ("names cleanup: unpaid registration " <> ridText <> " is kept until the next run, " <> why)

-- | At the idle cadence, so a failing rate read costs one read per coin per pass while browsers wait.
creditDue :: PollerEnv -> UTCTime -> IO Bool
creditDue PollerEnv {peCreditedAt, pePoll = PollConfig {pIdleSeconds}} now = atomically $ do
  due <- maybe True (\at -> diffUTCTime now at >= fromIntegral pIdleSeconds) <$> readTVar peCreditedAt
  when due $ writeTVar peCreditedAt (Just now)
  pure due

-- | Credits every closed order's confirmed money not all credited, so a crash or a failed rate read after a settlement loses nothing.
creditPass :: PollerEnv -> UTCTime -> IO ()
creditPass env@PollerEnv {peStore, peProviders} now = do
  due <- creditDue env now
  when due $ do
    invoices <- withTransaction peStore uncreditedLateInvoices
    forM_ invoices $ \invId@(InvoiceId invText) ->
      safely ("names: crediting invoice " <> invText) $
        getInvoice peStore invId >>= \case
          Just InvoiceRow {irProvider} | Just p <- find ((== irProvider) . pProvider) peProviders -> void $ creditLate env p now invId
          _ -> do
            due' <- dueToWarn env now ("no rate provider: " <> invText)
            when due' $ logWarn ("names: invoice " <> invText <> " has late money to credit, but no provider is configured to read its rate")

-- | The pass's rate of a coin at a provider, read at most once.
cachedRate :: PollerEnv -> Provider -> CryptoCurrency -> IO (Either ProviderError Rate)
cachedRate PollerEnv {peRates} Provider {pProvider, pCurrentRate} coin =
  M.lookup key <$> readTVarIO peRates >>= \case
    Just rate -> pure rate
    Nothing -> do
      rate <- maybe (pure $ Left $ ProviderError $ providerText pProvider <> " offers no rates") ($ coin) pCurrentRate
      rate <$ atomically (modifyTVar' peRates (M.insert key rate))
  where
    key = (providerText pProvider, cryptoCurrencyText coin)

-- | Each pass and hinted read reads the rates again.
forgetRates :: PollerEnv -> IO ()
forgetRates PollerEnv {peRates} = atomically $ writeTVar peRates M.empty

creditLate :: PollerEnv -> Provider -> UTCTime -> InvoiceId -> IO CreditOutcome
creditLate env@PollerEnv {peStore} p now invId@(InvoiceId invText) = do
  outcome <- creditLateInvoice peStore (cachedRate env p) invId now
  case outcome of
    CONothingDue -> pure ()
    COCredited LateCredit {lcRegistration = RegistrationId rid, lcUnits, lcInvoiceRate, lcCurrentRate, lcRateUsed, lcCents} ->
      logInfo $
        "names: invoice " <> invText <> " credited " <> tshow lcCents <> " cents to the balance of registration " <> rid
          <> maybe "" (\units -> " for " <> tshow units <> " units at " <> fromMaybe "?" lcRateUsed <> " (invoice " <> fromMaybe "?" lcInvoiceRate <> ", current " <> fromMaybe "?" lcCurrentRate <> ")") lcUnits
    CONoInvoiceRate -> do
      due <- dueToWarn env now ("no invoice rate: " <> invText)
      when due $ logError ("names: invoice " <> invText <> " has confirmed late money, but no invoice rate was recorded to credit it at; check it at the provider")
    CORateFailed (ProviderError e) -> do
      due <- dueToWarn env now ("rate read failed: " <> tshow (pProvider p))
      when due $ logWarn ("names: late credits wait, as the current rate cannot be read; each pass tries again: " <> e)
    CORateFloor invoiceRate Rate {rateText} -> do
      due <- dueToWarn env now ("rate floor: " <> invText)
      when due $ logError ("names: invoice " <> invText <> " is not credited: the current rate " <> rateText <> " is below half its rate " <> invoiceRate <> "; it is never credited by itself, and support refunds it by hand")
    CORefused -> logError ("names: the credit of invoice " <> invText <> " was refused, its credit number taken meanwhile; the next pass tries again")
  pure outcome

-- | Money not yet confirmed waits for the settlement that confirms it.
pendingLate :: PollerEnv -> InvoiceId -> IO ()
pendingLate PollerEnv {peStore} invId@(InvoiceId invText) =
  withTransaction peStore (`latePending` invId) >>= \case
    Just pending | pending `elem` [LPConfirming, LPUnconfirmed] -> logInfo ("names: invoice " <> invText <> " holds money of its closed order not yet confirmed; it is credited once confirmed")
    _ -> pure ()

-- | Late money still not all credited when its payment row is older than the read window, once per invoice and again when more arrives;
-- the claim keeps it to one instance.
alertLatePayments :: PollerEnv -> UTCTime -> IO ()
alertLatePayments env@PollerEnv {peStore} now = do
  due <- withTransaction peStore (`lateAlertDue` addUTCTime (negate settleWindow) now)
  forM_ due $ \alert@LateAlert {laState = LateCreditState {lsInvoice = InvoiceId invText}} ->
    safely ("names: the alert on invoice " <> invText) $ do
      (reason, support) <- lateReason env alert
      claimed <- withTransaction peStore $ \db -> claimLateAlert db alert support now
      when claimed $ logError (lateAlertText alert reason)

-- | Confirmed money waiting comes before a stored reason, as in pendingCase, so its alert never asks for a refund by hand.
lateReason :: PollerEnv -> LateAlert -> IO (Text, Maybe LateSupport)
lateReason env@PollerEnv {peStore, peProviders} LateAlert {laPending, laState = LateCreditState {lsInvoice, lsCoin, lsSupport, lsInvoiceRate}} = case laPending of
  LPUncounted -> pure (uncounted, Nothing)
  LPUncredited -> case lsCoin of
    Nothing -> pure (cardWaits, Nothing)
    Just coin
      | lsSupport == Just LSRateFloor ->
          getInvoice peStore lsInvoice >>= \case
            Just InvoiceRow {irProvider} | Just p <- find ((== irProvider) . pProvider) peProviders -> (\r -> (rateFloor (either (const "unread") rateText r), Nothing)) <$> cachedRate env p coin
            _ -> pure (rateFloor "unread", Nothing)
      | otherwise -> pure (rateRead, Nothing)
  _ | lsSupport == Just LSHighSpeed -> pure (highSpeed, Nothing)
  _ | lsSupport == Just LSNoConfirmation -> pure (noConfirmation, Nothing)
  _ -> pure (unconfirmed, Just LSUnconfirmed)
  where
    unconfirmed = "refund by hand only a transaction confirmed on chain that BTCPay no longer monitors; one still Processing there waits"
    highSpeed = "the invoice was made under HighSpeed, so its money is never credited by itself; refund by hand only a transaction confirmed on chain"
    noConfirmation = "the Monero plugin shows no confirmation of this money (the invoice's settled-confirmation threshold is 0 or not given), so it is never credited by itself; refund by hand only a transaction confirmed on chain"
    rateRead = "confirmed, the credit waits on the rate read; do not refund; fix the rate read"
    cardWaits = "confirmed, the credit waits on the poller's credit pass; do not refund; check the service log"
    rateFloor current = "confirmed; the coin's price fell by more than half since the invoice (" <> fromMaybe "?" lsInvoiceRate <> ", " <> current <> "); it is never credited by itself; refund by hand only a transaction confirmed on chain"
    uncounted = "an amount BTCPay reported with more decimals than its unit; check it at the provider"

lateAlertText :: LateAlert -> Text -> Text
lateAlertText LateAlert {laState = LateCreditState {lsInvoice = InvoiceId invText, lsRegistration = RegistrationId rid, lsCoin, lsReceivedCents, lsConfirmedCents, lsCreditedCents, lsReceivedUnits, lsConfirmedUnits, lsCreditedUnits}} reason =
  "names: invoice " <> invText <> " of order " <> rid <> ": " <> figures <> "; " <> reason
  where
    figures = case (lsCoin, lsReceivedUnits) of
      (Nothing, _) -> amounts "cents" (toInteger lsReceivedCents) (toInteger lsConfirmedCents) (toInteger lsCreditedCents)
      (Just coin, Just received) -> amounts (unitName coin) (toInteger received) (maybe 0 toInteger lsConfirmedUnits) (toInteger lsCreditedUnits)
      (Just _, Nothing) -> tshow lsReceivedCents <> " cents received at the invoice's rate, not counted in units"
    amounts unit received confirmed credited =
      tshow received <> " " <> unit <> " received, " <> tshow confirmed <> " " <> unit <> " confirmed, " <> tshow credited <> " " <> unit <> " credited"
    unitName = \case
      CCBtc -> "satoshi"
      CCXmr -> "piconero"

-- | Cancel the order at the provider first, so the buyer can no longer pay it.
closedAtProvider :: PollerEnv -> UTCTime -> OverdueInvoice -> IO Bool
closedAtProvider env@PollerEnv {peProviders} now oi@OverdueInvoice {oiProvider, oiProviderRef = ref} =
  safelyWith ("badge poller: cancelling " <> tshow oiProvider <> " invoice " <> ref) False $
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
passSafely env = void $ safely "badge poller: the pass" (runOnePass env)

hintSafely :: PollerEnv -> UTCTime -> Text -> IO ()
hintSafely env now ref = void $ safely ("badge poller: the hinted read of " <> ref) (readHint env now ref)
