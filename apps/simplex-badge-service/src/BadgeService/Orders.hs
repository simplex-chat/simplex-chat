{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.Orders (settleOrder, decide, codeLifetime) where

import BadgeService.Providers (Funded (..), PaymentSignal (..), Received (..))
import BadgeService.Store.Invoices (InvoicePayment (..), InvoiceRow (..), markCodePaid, paymentStatusText, settlementCodeHash, settlementInvoice, truncateToSecond, updateInvoiceStatus, upsertPayment)
import BadgeService.Waiters (Waiters, publish, publishPayment)
import Control.Concurrent.STM (atomically)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId, InvoiceStatus (..), PaymentStatus (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB

-- | Written onto the row at settlement and read back, so changing it moves only codes sold afterwards.
codeLifetime :: NominalDiffTime
codeLifetime = 365 * 24 * 60 * 60

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

settleOrder :: DBStore -> Waiters -> InvoiceId -> PaymentSignal -> UTCTime -> IO (Either Text InvoiceStatus)
settleOrder st waiters invId signal now' = do
  outcome <- withTransaction st $ \db ->
    settlementInvoice db invId >>= \case
      Nothing -> pure (Left "no such invoice")
      Just row@InvoiceRow {irStatus} -> case decide irStatus signal of
        Nothing -> pure (Right (irStatus, PubNothing))
        -- look up the code first, so an invoice with none leaves the transaction empty, not paid with an unpaid code
        Just w -> codeToMark db w >>= either (pure . Left) (settle db row w)
  case outcome of
    Left e -> pure (Left e)
    Right (status, toPublish) -> do
      -- after the commit, or a woken reader will not see the write
      atomically $ case toPublish of
        PubStatus s -> publish waiters invId s
        PubPayment -> publishPayment waiters invId
        PubNothing -> pure ()
      pure (Right status)
  where
    now = truncateToSecond now'
    at = truncateToSecond (settledInstant signal now)
    codeToMark :: DB.Connection -> Write -> IO (Either Text (Maybe ByteString))
    codeToMark db Write {wCode}
      | not wCode = pure (Right Nothing)
      | otherwise = maybe (Left "settled invoice has no code hash") (Right . Just) <$> settlementCodeHash db invId
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
    settle :: DB.Connection -> InvoiceRow -> Write -> Maybe ByteString -> IO (Either Text (InvoiceStatus, Published))
    settle db row@InvoiceRow {irStatus} w@Write {wStatus, wPayment} codeHash = do
      let Received {rcvAmount, rcvCrypto, rcvDue} = received signal
          wrotePayment = newPayment row w
      when wrotePayment $ upsertPayment db row wPayment rcvAmount rcvCrypto rcvDue (paidInFull signal) at
      moved <- maybe (pure True) (\new -> updateInvoiceStatus db invId irStatus new at) wStatus
      if moved
        then do
          mapM_ (\h -> markCodePaid db h (addUTCTime codeLifetime at)) codeHash
          pure (Right (fromMaybe irStatus wStatus, maybe (if wrotePayment then PubPayment else PubNothing) PubStatus wStatus))
        else do
          -- another writer moved the row first, so publish what we found
          current <- maybe irStatus (\InvoiceRow {irStatus = s} -> s) <$> settlementInvoice db invId
          pure (Right (current, PubStatus current))
