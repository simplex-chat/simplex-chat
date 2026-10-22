{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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

import StoreService.Log (logError, safelyWith)
import StoreService.Names.Phases (Phases, RegistrationId (..))
import qualified StoreService.Names.Phases as Phases
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), VersionWrite (..), getRegistration, updateRegistration)
import StoreService.Providers (Funded (..), PaymentSignal (..), Received (..))
import StoreService.Store.Invoices (InvoicePayment (..), InvoiceProduct (..), InvoiceRow (..), expireOverdueInvoice, markCodePaid, paymentHolds, paymentStatusText, settlementCodeHash, settlementInvoice, truncateToSecond, updateInvoiceStatus, upsertPayment)
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

-- | A name order's change at settlement: its new version, or money received after it closed, which stays unpaid.
data NameOutcome = NOWritten RegistrationId Int64 | NOLate RegistrationId

settleOrder :: DBStore -> Waiters -> Maybe Phases -> InvoiceId -> PaymentSignal -> UTCTime -> IO (Either Text InvoiceStatus)
settleOrder st waiters phases invId@(InvoiceId invText) signal now' = do
  outcome <- withTransaction st $ \db ->
    settlementInvoice db invId >>= \case
      Nothing -> pure (Left "no such invoice")
      Just row@InvoiceRow {irStatus} -> case decide irStatus signal of
        Nothing -> pure (Right (irStatus, PubNothing, []))
        -- read the product first, so an invoice without one leaves the transaction empty, not paid with nothing to show for it
        Just w -> productOf db row w >>= either (pure . Left) (settle db row w)
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
          NOLate _ -> pure ()
      forM_ name $ \case
        NOLate (RegistrationId rid) -> logError ("names: invoice " <> invText <> " was paid after its order closed, so registration " <> rid <> " stays unpaid; refund the payment")
        NOWritten {} -> pure ()
      pure (Right status)
  where
    now = truncateToSecond now'
    at = truncateToSecond (settledInstant signal now)
    productOf :: DB.Connection -> InvoiceRow -> Write -> IO (Either Text Product)
    productOf db InvoiceRow {irProduct} Write {wCode} = case irProduct of
      IPBadge {}
        | not wCode -> pure (Right (PBadge Nothing))
        | otherwise -> maybe (Left "settled invoice has no code hash") (Right . PBadge . Just) <$> settlementCodeHash db invId
      IPName rid -> maybe (Left "name invoice has no registration") (Right . PName) <$> getRegistration db rid
    newPayment :: InvoiceRow -> Write -> Bool
    newPayment InvoiceRow {irPayment} Write {wPayment}
      -- Monero reports an invoice as confirming while its figures are still zero, so a funded verdict earns a row
      | not (paidInFull signal) && rcvCrypto == Nothing && rcvAmount == CurrencyAmount 0 = False
      | otherwise = case irPayment of
          Nothing -> True
          -- the write is monotonic, so a lower figure or withdrawn verdict is not a new payment
          Just InvoicePayment {ipAmount, ipCryptoPaid, ipPaidInFull, ipStatus}
            | ipStatus == paymentStatusText PSSettled -> False
            | otherwise ->
                maybe True (\(CurrencyAmount held) -> held < minor) ipAmount
                  || (ipCryptoPaid == Nothing && rcvCrypto /= Nothing)
                  || (paidInFull signal && not ipPaidInFull)
                  || ipStatus /= paymentStatusText wPayment
      where
        Received {rcvAmount, rcvCrypto} = received signal
        CurrencyAmount minor = rcvAmount
    settle :: DB.Connection -> InvoiceRow -> Write -> Product -> IO (Either Text (InvoiceStatus, Published, [NameOutcome]))
    settle db row@InvoiceRow {irStatus} w@Write {wStatus, wPayment} sold = do
      let Received {rcvAmount, rcvCrypto, rcvDue} = received signal
          wrotePayment = newPayment row w
      when wrotePayment $ upsertPayment db row wPayment rcvAmount rcvCrypto rcvDue (paidInFull signal) at
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
nameSettled db before after wrotePayment holdsMoney r@NameRegistration {nrId, nrPhase} now
  | nrPhase /= PUnpaid = pure []
  | otherwise = case after of
      Just ISPaid
        | before /= ISOpen -> pure [NOLate nrId]
        | otherwise ->
            updateRegistration db r {nrPhase = PQueued} now >>= \case
              Written v -> pure [NOWritten nrId v]
              NotLive -> pure [NOLate nrId]
              -- another live order holds the name, so this payment can only be refunded; it is still recorded
              LiveOrderExists -> pure [NOLate nrId]
              _ -> E.throwIO (RegistrationChanged nrId)
      Just ISExpired -> (<> [NOLate nrId | holdsMoney]) . maybe [] (\(rid, v) -> [NOWritten rid v]) <$> closeOrder db r now
      _ -> pure [NOLate nrId | before /= ISOpen, wrotePayment]

-- | Clears a live unpaid order's live flag, as its invoice closes in the same transaction.
closeOrder :: DB.Connection -> NameRegistration -> UTCTime -> IO (Maybe (RegistrationId, Int64))
closeOrder db r@NameRegistration {nrId, nrPhase, nrLive} now
  | nrPhase /= PUnpaid || not nrLive = pure Nothing
  | otherwise =
      updateRegistration db r {nrLive = False} now >>= \case
        Written v -> pure (Just (nrId, v))
        _ -> E.throwIO (RegistrationChanged nrId)

-- | The order of a name invoice whose status this transaction moved to expired.
closeInvoiceOrder :: DB.Connection -> InvoiceId -> UTCTime -> IO (Maybe (RegistrationId, Int64))
closeInvoiceOrder db invId now =
  settlementInvoice db invId >>= \case
    Just InvoiceRow {irProduct = IPName rid} -> getRegistration db rid >>= maybe (pure Nothing) (\r -> closeOrder db r now)
    _ -> pure Nothing

-- | False when the invoice was no longer open. Throws RegistrationChanged, changing nothing, when its order changed meanwhile.
cancelOpenInvoice :: DBStore -> Waiters -> Maybe Phases -> InvoiceId -> UTCTime -> IO Bool
cancelOpenInvoice st waiters phases invId now = do
  closed <- withTransaction st $ \db -> do
    cancelled <- updateInvoiceStatus db invId ISOpen ISExpired now
    if cancelled then Just <$> closeInvoiceOrder db invId now else pure Nothing
  forM_ closed $ \order -> atomically $ publish waiters invId ISExpired >> mapM_ (publishRegistration phases) order
  pure (isJust closed)

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
