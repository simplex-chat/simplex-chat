{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Orders
  ( RegistrationChanged (..),
    settleOrder,
    cancelOpenInvoice,
    expireOverdue,
    publishRegistration,
    decide,
    codeLifetime,
  )
where

import StoreService.Log (logError, logInfo, safelyWith)
import StoreService.Names.Phases (Phases, RegistrationId (..))
import qualified StoreService.Names.Phases as Phases
import StoreService.Names.Store (LateSupport (..), NamePhase (..), NameRegistration (..), VersionWrite (..), getRegistration, releaseHeldBalance, setLateSupport, spendHeldBalance, updateRegistration)
import StoreService.Providers (Confirmed (..), Funded (..), PaymentSignal (..), Received (..), Uncreditable (..))
import StoreService.Store.Invoices (InvoicePayment (..), InvoiceProduct (..), InvoiceRow (..), expireOverdueInvoice, lockInvoice, markCodePaid, paymentHolds, paymentStatusText, settlementCodeHash, settlementInvoice, truncateToSecond, updateInvoiceStatus, upsertPayment)
import StoreService.Waiters (Waiters, publish, publishPayment)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (Exception)
import qualified Control.Exception as E
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentStatus (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB

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
  SigClosed {} -> False

received :: PaymentSignal -> Received
received = \case
  SigFunded r _ -> r
  SigSettled r _ -> r
  SigClosed r -> r

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
-- a holder queued whose held balance could not be spent (the holder, then the balance's order); or an order closed because another live order
-- holds its commitment, or its owner and name.
data NameOutcome = NOWritten RegistrationId Int64 | NOLate RegistrationId | NOHoldNotSpent RegistrationId RegistrationId | NOLiveOrderExists RegistrationId

settleOrder :: DBStore -> Waiters -> Maybe Phases -> InvoiceId -> PaymentSignal -> UTCTime -> IO (Either Text InvoiceStatus)
settleOrder st waiters phases invId signal now' = do
  outcome <- withTransaction st $ \db ->
    settlementInvoice db invId >>= \case
      Just InvoiceRow {irStatus}
        | isJust (decide irStatus signal) -> do
            -- On Postgres, a concurrent overdue close holding the row is waited for, and the read below sees its writes;
            -- a close that comes later waits for this payment and then sees it.
            lockInvoice db invId now
            settlementInvoice db invId >>= settleRow db
      row_ -> settleRow db row_
  case outcome of
    Left e -> pure (Left e)
    Right (status, toPublish, name) -> do
      -- after the commit, or a woken reader will not see the write
      atomically $ do
        case toPublish of
          PubStatus s -> publish waiters invId s
          PubPayment -> publishPayment waiters invId
          PubNothing -> pure ()
        forM_ name $ \case
          NOWritten rid v -> publishRegistration phases (rid, v)
          _ -> pure ()
      forM_ name $ \case
        NOLate rid -> latePayment invId rid
        NOHoldNotSpent holder source -> holdNotSpent invId holder source
        NOLiveOrderExists rid -> liveOrderExists invId rid
        NOWritten {} -> pure ()
      pure (Right status)
  where
    now = truncateToSecond now'
    at = truncateToSecond (settledInstant signal now)
    settleRow db = \case
      Nothing -> pure (Left "no such invoice")
      Just row@InvoiceRow {irStatus} -> case decide irStatus signal of
        Nothing -> pure (Right (irStatus, PubNothing, []))
        -- read the product first, so an invoice without one is not marked paid with nothing to show for it
        Just w -> productOf db row w >>= either (pure . Left) (settle db row w)
    productOf :: DB.Connection -> InvoiceRow -> Write -> IO (Either Text Product)
    productOf db InvoiceRow {irProduct} Write {wCode} = case irProduct of
      IPBadge {}
        | not wCode -> pure (Right (PBadge Nothing))
        | otherwise -> maybe (Left "settled invoice has no code hash") (Right . PBadge . Just) <$> settlementCodeHash db invId
      IPName rid -> maybe (Left "name invoice has no registration") (Right . PName) <$> getRegistration db rid
    newPayment :: InvoiceRow -> Write -> Bool
    -- A rise of the confirmed money is a new payment with the totals unchanged, so a late payment that confirms is credited.
    newPayment InvoiceRow {irPayment} Write {wPayment}
      -- Monero reports an invoice as confirming while its figures are still zero, so a funded verdict earns a row
      | not (paidInFull signal) && rcvCrypto == Nothing && rcvAmount == CurrencyAmount 0 = False
      | otherwise = case irPayment of
          Nothing -> True
          -- the write is monotonic, so a lower figure or withdrawn verdict is not a new payment
          Just InvoicePayment {ipAmount, ipCryptoPaid, ipPaidInFull, ipStatus, ipConfirmed = Confirmed {cfCents = CurrencyAmount heldCents, cfUnits = heldUnits}}
            | ipStatus == paymentStatusText PSSettled -> False
            | otherwise ->
                maybe True (\(CurrencyAmount held) -> held < minor) ipAmount
                  || (ipCryptoPaid == Nothing && rcvCrypto /= Nothing)
                  || (paidInFull signal && not ipPaidInFull)
                  || ipStatus /= paymentStatusText wPayment
                  || confirmedCents > heldCents
                  || maybe False (> fromMaybe 0 heldUnits) confirmedUnits
      where
        Received {rcvAmount, rcvCrypto, rcvConfirmed = Confirmed {cfCents = CurrencyAmount confirmedCents, cfUnits = confirmedUnits}} = received signal
        CurrencyAmount minor = rcvAmount
    settle :: DB.Connection -> InvoiceRow -> Write -> Product -> IO (Either Text (InvoiceStatus, Published, [NameOutcome]))
    settle db row@InvoiceRow {irStatus} w@Write {wStatus, wPayment} sold = do
      let money@Received {rcvUncreditable} = received signal
          wrotePayment = newPayment row w
      when wrotePayment $ do
        upsertPayment db row wPayment money (paidInFull signal) at
        case sold of
          PName _ -> mapM_ (setLateSupport db invId . lateSupportOf) rcvUncreditable
          PBadge _ -> pure ()
      moved <- maybe (pure True) (\new -> updateInvoiceStatus db invId irStatus new at) wStatus
      if moved
        then do
          name <- case sold of
            PBadge codeHash -> [] <$ mapM_ (\h -> markCodePaid db h (addUTCTime codeLifetime at)) codeHash
            PName r -> nameSettled db irStatus wStatus wrotePayment (wrotePayment || maybe False paymentHolds (irPayment row)) r now
          pure (Right (fromMaybe irStatus wStatus, maybe (if wrotePayment then PubPayment else PubNothing) PubStatus wStatus, name))
        else do
          -- another writer moved the row first, so publish what we found
          current <- maybe irStatus (\InvoiceRow {irStatus = s} -> s) <$> settlementInvoice db invId
          pure (Right (current, PubStatus current, []))

-- | A paid open invoice queues its order only while the order is live; a payment of a closed one is late.
-- An invoice the provider closed closes its order, and is late once if it holds money, read now or before.
-- BTCPay reports money paid into a closed invoice as Expired or Invalid, not Settled, so a new payment figure
-- on a closed invoice is late too; the monotonic payment write reports each figure once.
nameSettled :: DB.Connection -> InvoiceStatus -> Maybe InvoiceStatus -> Bool -> Bool -> NameRegistration -> UTCTime -> IO [NameOutcome]
nameSettled db before after wrotePayment holdsMoney r@NameRegistration {nrId, nrPhase, nrBalanceSource, nrBalanceUsed} now
  | nrPhase /= PUnpaid = pure []
  | otherwise = case after of
      Just ISPaid
        | before /= ISOpen -> pure [NOLate nrId]
        | otherwise ->
            updateRegistration db r {nrPhase = PQueued} now >>= \case
              Written v -> (NOWritten nrId v :) <$> spendHold
              NotLive -> pure [NOLate nrId]
              -- an invariant broke; left live, the order would keep its hold for ever, as no sweep closes a paid invoice
              LiveOrderExists -> (<> [NOLate nrId, NOLiveOrderExists nrId]) <$> closed
              _ -> E.throwIO (RegistrationChanged nrId)
      Just ISExpired -> (<> [NOLate nrId | holdsMoney]) <$> closed
      _ -> pure [NOLate nrId | before /= ISOpen, wrotePayment]
  where
    closed = maybe [] (\(rid, v) -> [NOWritten rid v]) <$> closeOrder db r now
    -- the buyer paid the difference, so the order stays queued even when its hold cannot be spent
    spendHold = case nrBalanceSource of
      Just source -> (\spent -> [NOHoldNotSpent nrId source | not spent]) <$> spendHeldBalance db nrId nrBalanceUsed now
      Nothing -> pure []

-- | Clears a live unpaid order's live flag, as its invoice closes in the same transaction, and releases a balance it held.
closeOrder :: DB.Connection -> NameRegistration -> UTCTime -> IO (Maybe (RegistrationId, Int64))
closeOrder db r@NameRegistration {nrId, nrPhase, nrLive} now
  | nrPhase /= PUnpaid || not nrLive = pure Nothing
  | otherwise =
      updateRegistration db r {nrLive = False} now >>= \case
        Written v -> Just (nrId, v) <$ releaseHeldBalance db nrId now
        _ -> E.throwIO (RegistrationChanged nrId)

-- | The order of a name invoice whose status this transaction moved to expired.
closeInvoiceOrder :: DB.Connection -> InvoiceId -> UTCTime -> IO (Maybe (RegistrationId, Int64))
closeInvoiceOrder db invId now =
  settlementInvoice db invId >>= \case
    Just InvoiceRow {irProduct = IPName rid} -> getRegistration db rid >>= maybe (pure Nothing) (\r -> closeOrder db r now)
    _ -> pure Nothing

lateSupportOf :: Uncreditable -> LateSupport
lateSupportOf = \case
  UCHighSpeed -> LSHighSpeed
  UCUncounted -> LSUnits
  UCNoConfirmationShown -> LSNoConfirmation

latePayment :: InvoiceId -> RegistrationId -> IO ()
latePayment (InvoiceId invText) (RegistrationId rid) =
  logInfo ("names: invoice " <> invText <> " received money after registration " <> rid <> " closed; it stays unpaid, and its confirmed money is credited to its balance")

holdNotSpent :: InvoiceId -> RegistrationId -> RegistrationId -> IO ()
holdNotSpent (InvoiceId invText) (RegistrationId holder) (RegistrationId source) =
  logError $
    "names: invoice " <> invText <> " queued registration " <> holder <> " as paid, but the balance of registration " <> source
      <> " it held could not be spent; check that balance and release or spend its hold by hand"

liveOrderExists :: InvoiceId -> RegistrationId -> IO ()
liveOrderExists (InvoiceId invText) (RegistrationId rid) =
  logError $
    "names: invoice " <> invText <> " was paid while another live order held the commitment, or the owner and name, of registration " <> rid
      <> "; it was closed and its money is credited to its balance; the unique live indexes should have prevented this"

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
        if cancelled then Just . (,lateOf row) <$> closeInvoiceOrder db invId now else pure Nothing
      _ -> pure Nothing
  forM_ closed $ \(order, late) -> do
    atomically $ publish waiters invId ISExpired >> mapM_ (publishRegistration phases) order
    mapM_ (latePayment invId) late
  pure (isJust closed)
  where
    lateOf InvoiceRow {irProduct, irPayment} = case irProduct of
      IPName rid | maybe False paymentHolds irPayment -> Just rid
      _ -> Nothing

-- | Of the given invoices, expires those still open, with no money received, and past the cutoff, closing their orders.
-- Each in its own transaction, so an invoice that fails is logged and skipped, and the others still expire.
expireOverdue :: DBStore -> Waiters -> Maybe Phases -> UTCTime -> UTCTime -> [InvoiceId] -> IO [InvoiceId]
expireOverdue st waiters phases now cutoff = fmap catMaybes . mapM expire
  where
    expire invId@(InvoiceId invText) = safelyWith ("expiring invoice " <> invText) Nothing $ do
      expired <- withTransaction st $ \db -> do
        moved <- expireOverdueInvoice db cutoff invId
        if moved then Just <$> closeInvoiceOrder db invId now else pure Nothing
      forM_ expired $ \order -> atomically $ publish waiters invId ISExpired >> mapM_ (publishRegistration phases) order
      pure (invId <$ expired)

publishRegistration :: Maybe Phases -> (RegistrationId, Int64) -> STM ()
publishRegistration phases (rid, v) = mapM_ (\p -> Phases.publish p rid v) phases
