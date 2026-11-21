{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Orders
  ( RegistrationChanged (..),
    settleOrder,
    cancelOpenInvoice,
    cancelNameInvoices,
    expireOverdue,
    publishRegistration,
    decide,
    signalReceived,
    codeLifetime,
  )
where

import StoreService.Names.Phases (Phases, RegistrationId (..))
import qualified StoreService.Names.Phases as Phases
import StoreService.Names.Store (BrokenInvariant (..), InvoiceClose (..), LateSupport (..), NamePhase (..), NameRegistration (..), OrderPhase (..), VersionWrite (..), creditBalance, getRegistration, labelHolder, namePhase, openInvoicesOfHeldName, reversalRecorded, setLateSupport, setPaidAt, updateRegistration)
import StoreService.Providers (Confirmation (..), Confirmed (..), CryptoReceived (..), Funded (..), PaymentSignal (..), Provider (..), Received (..), providerErrorText, providerNamed, receivedAmount, receivedConfirmed)
import StoreService.Store.Invoices (InvoicePayment (..), InvoiceProduct (..), InvoiceRow (..), OverdueInvoice (..), currentSecond, expireOverdueInvoice, lockInvoice, markCodePaid, paymentHolds, paymentStatusText, settlementCodeHash, settlementInvoice, truncateToSecond, updateInvoiceStatus, upsertPayment)
import StoreService.Waiters (Waiters, publish, publishPayment)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Logger.Simple (logError, logNote, logWarn)
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentStatus (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Util (catchOwn', tshow)

-- | Written onto the row at settlement and read back, so changing it moves only codes sold afterwards.
codeLifetime :: NominalDiffTime
codeLifetime = 365 * 24 * 60 * 60

-- | Thrown inside the transaction that read the registration, so the invoice's own change is rolled back with it.
newtype RegistrationChanged = RegistrationChanged RegistrationId
  deriving (Eq, Show)

instance Exception RegistrationChanged

data Write = Write
  { wStatus :: Maybe InvoiceStatus,
    wPayment :: PaymentStatus,
    wCode :: Bool
  }

decide :: InvoiceStatus -> PaymentSignal -> Maybe Write
decide ISPaid _ = Nothing
decide _ SigSettled {} = Just Write {wStatus = Just ISPaid, wPayment = PSSettled, wCode = True}
decide _ SigFunded {} = Just Write {wStatus = Nothing, wPayment = PSPending, wCode = False}
decide ISOpen SigClosed {} = Just Write {wStatus = Just ISExpired, wPayment = PSPending, wCode = False}
decide ISExpired SigClosed {} = Just Write {wStatus = Nothing, wPayment = PSPending, wCode = False}

paidInFull :: PaymentSignal -> Bool
paidInFull = \case
  SigFunded _ f -> f == PaidInFull
  SigSettled {} -> True
  SigClosed _ f -> f == PaidInFull

signalReceived :: PaymentSignal -> Received
signalReceived = \case
  SigFunded r _ -> r
  SigSettled r _ -> r
  SigClosed r _ -> r

-- | Uses the provider's settlement instant, not now, so an outage does not push deadlines out;
-- an implausible value (zero, milliseconds, negative) falls back to now.
settledInstant :: PaymentSignal -> UTCTime -> UTCTime
settledInstant signal now = case signal of
  SigSettled _ at | at <= now, at >= addUTCTime (negate maxBackdate) now -> at
  _ -> now

-- | Longer than any outage the poller must survive, far short of the code's lifetime.
maxBackdate :: NominalDiffTime
maxBackdate = 30 * 24 * 60 * 60

data Published = PubNothing | PubStatus InvoiceStatus | PubPayment

-- | What the invoice pays for, read before any write.
data Product = PBadge (Maybe ByteString) | PName NameRegistration

-- | A name order's change at settlement: its new version; money received after it closed, which stays unpaid and is credited to its balance;
-- or an order closed because another live order holds its commitment, or its owner and name. The text is the provider's reference of the order's invoice.
data NameOutcome = NOWritten RegistrationId Int64 | NOLate Text | NOLiveOrderExists Text | NOTaken Text | NOReversed Text

data SettledMoney = SMNone | SMHeld | SMNewPayment
  deriving (Eq)

settleOrder :: DBStore -> Waiters -> Maybe Phases -> InvoiceId -> PaymentSignal -> UTCTime -> IO InvoiceStatus
settleOrder st waiters phases invId@(InvoiceId invText) signal now' = do
  (status, toPublish, name) <- withTransaction st $ \db ->
    settlementInvoice db invId >>= \case
      Just InvoiceRow {irStatus, irProduct = IPName _}
        | isJust (decide irStatus signal) -> do
            -- On Postgres, a concurrent overdue close of the name order holding the row is waited for, and the read below sees
            -- its writes; a close that comes later waits for this payment and then sees it.
            lockInvoice db invId now
            settlementInvoice db invId >>= settleRow db
      row_ -> settleRow db row_
  let (publications, logs) = unzip $ map announced name
  -- after the commit, or a woken reader will not see the write
  atomically $ do
    case toPublish of
      PubStatus s -> publish waiters invId s
      PubPayment -> publishPayment waiters invId
      PubNothing -> pure ()
    sequence_ publications
  sequence_ logs
  pure status
  where
    announced :: NameOutcome -> (STM (), IO ())
    announced = \case
      NOWritten rid v -> (publishRegistration phases (rid, v), pure ())
      NOLate ref -> (pure (), latePayment ref)
      NOLiveOrderExists ref -> (pure (), liveOrderExists ref)
      NOTaken ref -> (pure (), takenAtSettlement ref)
      NOReversed ref -> (pure (), reversedAtSettlement ref)
    now = truncateToSecond now'
    at = truncateToSecond (settledInstant signal now)
    settleRow db = \case
      Nothing -> brokenInvariant "is missing"
      Just row@InvoiceRow {irStatus} -> case decide irStatus signal of
        Nothing -> pure (irStatus, PubNothing, [])
        -- read the product first, so an invoice without one is not marked paid with nothing to show for it
        Just w -> productOf db row w >>= settle db row w
    productOf :: DB.Connection -> InvoiceRow -> Write -> IO Product
    productOf db InvoiceRow {irProduct} Write {wCode} = case irProduct of
      IPBadge {}
        | not wCode -> pure (PBadge Nothing)
        | otherwise -> settlementCodeHash db invId >>= maybe (brokenInvariant "has no code hash") (pure . PBadge . Just)
      IPName rid -> getRegistration db rid >>= maybe (brokenInvariant "has no registration") (pure . PName)
    brokenInvariant :: Text -> IO a
    brokenInvariant what = E.throwIO $ BrokenInvariant $ "the settled invoice " <> invText <> " " <> what
    newPayment :: InvoiceRow -> Write -> Bool
    -- A rise of the confirmed money is a new payment with the totals unchanged, so a late payment that confirms is credited.
    newPayment InvoiceRow {irPayment} Write {wPayment}
      -- Monero reports an invoice as confirming while its figures are still zero, so a funded verdict earns a row
      | not (paidInFull signal) && crypto == Nothing && minor == 0 = False
      | otherwise = case irPayment of
          Nothing -> True
          -- the write is monotonic, so a lower figure or withdrawn verdict is not a new payment
          Just InvoicePayment {ipAmount, ipCryptoPaid, ipPaidInFull, ipStatus, ipConfirmed = Confirmed {cfCents = CurrencyAmount heldCents, cfUnits = heldUnits}}
            | ipStatus == paymentStatusText PSSettled -> False
            | otherwise ->
                maybe True (\(CurrencyAmount held) -> held < minor) ipAmount
                  || (ipCryptoPaid == Nothing && crypto /= Nothing)
                  || (paidInFull signal && not ipPaidInFull)
                  || ipStatus /= paymentStatusText wPayment
                  || confirmedCents > heldCents
                  || maybe False (> fromMaybe 0 heldUnits) confirmedUnits
      where
        received = signalReceived signal
        CurrencyAmount minor = receivedAmount received
        Confirmed {cfCents = CurrencyAmount confirmedCents, cfUnits = confirmedUnits} = receivedConfirmed received
        crypto = case received of
          RCard {} -> Nothing
          RCrypto CryptoReceived {crPaid} -> crPaid
    settle :: DB.Connection -> InvoiceRow -> Write -> Product -> IO (InvoiceStatus, Published, [NameOutcome])
    settle db row@InvoiceRow {irStatus} w@Write {wStatus, wPayment} sold = do
      let money = signalReceived signal
          uncreditable = case money of
            RCrypto CryptoReceived {crConfirmation = Uncreditable reason} -> Just reason
            _ -> Nothing
          wrotePayment = newPayment row w
          settledMoney
            | wrotePayment = SMNewPayment
            | maybe False paymentHolds (irPayment row) = SMHeld
            | otherwise = SMNone
      when wrotePayment $ do
        upsertPayment db row wPayment money (paidInFull signal) at
        case sold of
          PName _ -> mapM_ (setLateSupport db invId . LSUncreditable) uncreditable
          PBadge _ -> pure ()
      moved <- maybe (pure True) (\new -> updateInvoiceStatus db invId irStatus new at) wStatus
      if moved
        then do
          name <- case sold of
            PBadge codeHash -> [] <$ mapM_ (\h -> markCodePaid db h (addUTCTime codeLifetime at)) codeHash
            PName r -> nameSettled db (irProviderRef row) irStatus wStatus settledMoney r at now
          pure (fromMaybe irStatus wStatus, maybe (if wrotePayment then PubPayment else PubNothing) PubStatus wStatus, name)
        else do
          -- another writer moved the row first, so publish what we found
          current <- maybe irStatus (\InvoiceRow {irStatus = s} -> s) <$> settlementInvoice db invId
          pure (current, PubStatus current, [])

-- | A paid open invoice queues its order only while the order is live; a payment of a closed one is late.
-- An invoice the provider closed closes its order, and is late once if it holds money, read now or before.
-- BTCPay reports money paid into a closed invoice as Expired or Invalid, not Settled, so a new payment figure
-- on a closed invoice is late too; the monotonic payment write reports each figure once.
nameSettled :: DB.Connection -> Text -> InvoiceStatus -> Maybe InvoiceStatus -> SettledMoney -> NameRegistration -> UTCTime -> UTCTime -> IO [NameOutcome]
nameSettled db ref before after money r@NameRegistration {nrId, nrLabelHash, nrPhase, nrPrice} paidAt now
  | namePhase nrPhase `notElem` [PUnpaid, PClosed] = pure []
  | otherwise = case after of
      Just ISPaid
        | before /= ISOpen -> pure [NOLate ref]
        | otherwise ->
            reversalRecorded db nrId >>= \case
              True ->
                updateRegistration db r {nrPhase = OClosed ICReversed, nrReveal = Nothing} now >>= \case
                  Written v -> do
                    setPaidAt db nrId paidAt
                    pure [NOWritten nrId v, NOReversed ref]
                  _ -> E.throwIO (RegistrationChanged nrId)
              False -> do
                held <- isJust <$> labelHolder db nrLabelHash
                updateRegistration db r {nrPhase = if held then OTaken else OQueued} now >>= \case
                  Written v -> do
                    setPaidAt db nrId paidAt
                    when held $ creditBalance db nrId nrPrice now
                    pure $ NOWritten nrId v : [NOTaken ref | held]
                  NotLive -> pure [NOLate ref]
                  -- an invariant broke; left live, the order would keep its hold for ever, as no sweep closes a paid invoice
                  LiveOrderExists -> (<> [NOLate ref, NOLiveOrderExists ref]) <$> closed
                  _ -> E.throwIO (RegistrationChanged nrId)
      Just ISExpired -> (<> [NOLate ref | money /= SMNone]) <$> closed
      _ -> pure [NOLate ref | before /= ISOpen, money == SMNewPayment]
  where
    closed = maybe [] (\(rid, v) -> [NOWritten rid v]) <$> closeOrder db ICExpired r now

-- | Closes a live unpaid order, as its invoice closes in the same transaction.
closeOrder :: DB.Connection -> InvoiceClose -> NameRegistration -> UTCTime -> IO (Maybe (RegistrationId, Int64))
closeOrder db how r@NameRegistration {nrId, nrPhase} now = case nrPhase of
  OUnpaid _ ->
    updateRegistration db r {nrPhase = OClosed how, nrReveal = Nothing} now >>= \case
      Written v -> pure $ Just (nrId, v)
      _ -> E.throwIO (RegistrationChanged nrId)
  _ -> pure Nothing

-- | The order of a name invoice whose status this transaction moved to expired, by a cancel or by its expiry.
closeInvoiceOrder :: DB.Connection -> InvoiceClose -> InvoiceId -> UTCTime -> IO (Maybe (RegistrationId, Int64))
closeInvoiceOrder db how invId now =
  settlementInvoice db invId >>= \case
    Just InvoiceRow {irProduct = IPName rid} -> getRegistration db rid >>= maybe (pure Nothing) (\r -> closeOrder db how r now)
    _ -> pure Nothing

latePayment :: Text -> IO ()
latePayment ref =
  logNote ("names: invoice " <> ref <> " received money after its order closed; it stays unpaid, and its money is credited to the order's balance once confirmed, else alerted")

takenAtSettlement :: Text -> IO ()
takenAtSettlement ref =
  logNote $ "names: invoice " <> ref <> " was paid while another paid order holds its name, so its order is taken and the money paid for it is added to its balance"

reversedAtSettlement :: Text -> IO ()
reversedAtSettlement ref =
  logError $ "names: invoice " <> ref <> " was paid after a reversal of its payment was recorded, so its order is closed as reversed, holds no name and is credited nothing"

liveOrderExists :: Text -> IO ()
liveOrderExists ref =
  logError $
    "names: invoice " <> ref <> " was paid while another live order held its order's commitment, or its owner and name"
      <> "; the order was closed and its money is credited to its balance; the unique live indexes should have prevented this"

-- | False when the invoice was no longer open. Throws RegistrationChanged, changing nothing, when its order changed meanwhile.
-- The provider has taken the cancel, so a name invoice a payment landed on meanwhile expires too, and its payment is late.
cancelOpenInvoice :: DBStore -> Waiters -> Maybe Phases -> InvoiceId -> UTCTime -> IO Bool
cancelOpenInvoice st waiters phases invId now = do
  closed <- withTransaction st $ \db -> do
    -- On Postgres, a settlement holding the row is waited for, and the read below sees its payment.
    lockInvoice db invId now
    settlementInvoice db invId >>= \case
      Just row@InvoiceRow {irStatus = ISOpen} -> do
        cancelled <- updateInvoiceStatus db invId ISOpen ISExpired now
        if cancelled then Just . (,lateOf row) <$> closeInvoiceOrder db ICCancelled invId now else pure Nothing
      _ -> pure Nothing
  forM_ closed $ \(order, late) -> do
    atomically $ publish waiters invId ISExpired >> mapM_ (publishRegistration phases) order
    mapM_ latePayment late
  pure (isJust closed)
  where
    lateOf InvoiceRow {irProduct, irProviderRef, irPayment} = case irProduct of
      IPName _ | maybe False paymentHolds irPayment -> Just irProviderRef
      _ -> Nothing

cancelNameInvoices :: DBStore -> Waiters -> Maybe Phases -> [Provider] -> RegistrationId -> IO ()
cancelNameInvoices st waiters phases providers rid =
  withTransaction st (`openInvoicesOfHeldName` rid) >>= mapM_ cancel
  where
    cancel (invId, provider, ref) = case providerNamed providers provider of
      Nothing -> uncancelled ref "no provider is configured for it"
      Just Provider {pCancelInvoice} ->
        pCancelInvoice ref >>= \case
          Left e -> uncancelled ref $ providerErrorText e
          Right () -> do
            now <- currentSecond
            closed <- cancelOpenInvoice st waiters phases invId now `catchOwn'` \e -> False <$ logError ("names: closing invoice " <> ref <> " failed: " <> tshow e)
            when closed $ logNote $ "names: invoice " <> ref <> " is cancelled, as another order paid for its name"
    uncancelled ref why = logWarn $ "names: invoice " <> ref <> " of a name another order paid for is not cancelled, and expires by itself: " <> why

-- | Of the given invoices, expires those still open, with no money received, and past the cutoff, closing their orders.
-- Each in its own transaction, so an invoice that fails is logged and skipped, and the others still expire.
expireOverdue :: DBStore -> Waiters -> Maybe Phases -> UTCTime -> UTCTime -> [OverdueInvoice] -> IO [InvoiceId]
expireOverdue st waiters phases now cutoff = fmap catMaybes . mapM expire
  where
    expire OverdueInvoice {oiInvoiceId = invId, oiProviderRef} = (`catchOwn'` \e -> Nothing <$ logError ("expiring invoice " <> oiProviderRef <> " failed; the next pass tries it again: " <> tshow e)) $ do
      expired <- withTransaction st $ \db -> do
        moved <- expireOverdueInvoice db cutoff invId
        if moved then Just <$> closeInvoiceOrder db ICExpired invId now else pure Nothing
      forM_ expired $ \order -> atomically $ publish waiters invId ISExpired >> mapM_ (publishRegistration phases) order
      pure (invId <$ expired)

publishRegistration :: Maybe Phases -> (RegistrationId, Int64) -> STM ()
publishRegistration phases (rid, v) = mapM_ (\p -> Phases.publish p rid v) phases
