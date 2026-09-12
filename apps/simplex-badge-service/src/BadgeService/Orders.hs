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

-- | How long a settled code may be redeemed for. Written onto the row at settlement and read
-- back from it, so changing this moves the deadline for codes sold afterwards and leaves the
-- ones already sold on the deadline they were given.
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

-- | A settlement is paid by definition; a funded signal carries the provider's verdict.
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

-- | We only learn about payments when we poll, so using @now@ would push out every
-- receipt and retention deadline by however long we were not looking. All three writes
-- share this value, or an order and its code would expire at different times.
--
-- Believed only where it could be true. 'toMinorUnits' clamps what the provider says about
-- money; this clamps what it says about time, which lands in two rows and in the code's
-- redemption deadline. A zero, a value in milliseconds or a negative one is not a clock we
-- can use: it would sell a code that expired years ago, or write a timestamp the row cannot
-- be read back through.
settledInstant :: PaymentSignal -> UTCTime -> UTCTime
settledInstant signal now = case signal of
  SigSettled _ at | at <= now, at >= addUTCTime (negate maxBackdate) now -> at
  _ -> now

-- | Longer than any outage the poller is meant to survive, and far short of the code's own
-- lifetime, so a believable instant is still believed after days of not looking.
maxBackdate :: NominalDiffTime
maxBackdate = 30 * 24 * 60 * 60

-- | What a settling transaction leaves for the waiters: a status they must be told about,
-- a payment that did not move the status, or nothing at all.
data Published = PubNothing | PubStatus InvoiceStatus | PubPayment

settleOrder :: DBStore -> Waiters -> InvoiceId -> PaymentSignal -> UTCTime -> IO (Either Text InvoiceStatus)
settleOrder st waiters invId signal now' = do
  outcome <- withTransaction st $ \db ->
    settlementInvoice db invId >>= \case
      Nothing -> pure (Left "no such invoice")
      Just row@InvoiceRow {irStatus} -> case decide irStatus signal of
        Nothing -> pure (Right (irStatus, PubNothing))
        -- look up the code first, so an invoice with none leaves the transaction empty
        -- rather than paid with its code still unpaid
        Just w -> codeToMark db w >>= either (pure . Left) (settle db row w)
  case outcome of
    Left e -> pure (Left e)
    Right (status, toPublish) -> do
      -- after the commit, or a woken reader will not see the write
      atomically $ case toPublish of
        PubStatus s -> publish waiters invId s
        -- the status did not move, but a payment landed: the page has a screen for that
        PubPayment -> publishPayment waiters invId
        -- nothing was written, and waking a hold to answer the same thing is churn
        PubNothing -> pure ()
      pure (Right status)
  where
    now = truncateToSecond now'
    at = truncateToSecond (settledInstant signal now)
    codeToMark :: DB.Connection -> Write -> IO (Either Text (Maybe ByteString))
    codeToMark db Write {wCode}
      | not wCode = pure (Right Nothing)
      | otherwise = maybe (Left "settled invoice has no code hash") (Right . Just) <$> settlementCodeHash db invId
    -- The provider keeps listing a closed invoice for days and re-sends what it already
    -- sent. Rewriting the row each pass wakes every held reader as if a payment had landed,
    -- and an invoice nobody paid does not get a payment row at all.
    newPayment :: InvoiceRow -> Write -> Bool
    newPayment InvoiceRow {irPayment} Write {wPayment}
      -- a funded verdict is worth a row on its own: Monero reports an invoice as confirming
      -- while its figures are still zero, and without the row the page says "waiting for payment"
      | not (paidInFull signal) && rcvCrypto == Nothing && rcvAmount == CurrencyAmount 0 = False
      | otherwise = case irPayment of
          Nothing -> True
          -- the write is monotonic, so "different" is not "would move": a lower figure or a
          -- withdrawn verdict leaves the row alone, and calling that new republishes it forever
          Just InvoicePayment {ipAmount, ipCryptoAmount, ipPaidInFull, ipStatus}
            | ipStatus == paymentStatusText PSSettled -> False
            | otherwise ->
                maybe True (\(CurrencyAmount held) -> held < minor) ipAmount
                  || (ipCryptoAmount == Nothing && rcvCrypto /= Nothing)
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
          -- another writer moved the row first, so publish what we found rather than assume
          -- it did: publishing twice wakes a reader that is already awake, which costs nothing.
          current <- maybe irStatus (\InvoiceRow {irStatus = s} -> s) <$> settlementInvoice db invId
          pure (Right (current, PubStatus current))
