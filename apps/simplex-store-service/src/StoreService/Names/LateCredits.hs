{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The credit of a name invoice's money to its order's balance: a closed order's late money, or the extra of an invoice paid above what
-- it asked. Only confirmed money not yet credited, crypto as its share of the invoice's amount at the lower of the invoice's rate and
-- the current one. The rate is read outside any transaction, so the credit runs after the settlement that recorded the money commits,
-- and again from the poller's pass for whatever is still not credited.
module StoreService.Names.LateCredits
  ( CreditOutcome (..),
    LateDue (..),
    LateMoney (..),
    minCurrentRateFraction,
    creditLateInvoice,
    lateCreditOf,
    lateMoneyOf,
  )
where

import qualified Control.Exception as E
import Control.Monad (mfilter)
import Data.Ratio ((%))
import Data.Time.Clock (UTCTime)
import Simplex.Chat.PaymentService.Types (CryptoCurrency, InvoiceId (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import StoreService.Names.Store (BrokenInvariant (..), CreditAmount (..), LateCredit (..), LateCreditState (..), LateFigures (..), LateSupport (..), NamePhase (..), creditLatePayment, lateCreditState, setLateSupport)
import StoreService.Providers (ProviderError (..), Rate (..))
import StoreService.Store.Invoices (lockInvoice)

-- | A current rate below this fraction of the invoice's is not used: it may be a glitch or a real fall, which the owner decides on.
minCurrentRateFraction :: Rational
minCurrentRateFraction = 1 % 2

-- | A closed order's late money, or the extra crypto of an invoice that paid its order.
data LateMoney = LMLate | LMOverpaid
  deriving (Eq, Show)

-- | CONoInvoiceFigures: crypto due with no invoice rate or invoiced units recorded to credit it at.
data CreditOutcome
  = CONothingDue
  | COCredited LateMoney LateCredit
  | CONoInvoiceFigures
  | CORateFailed ProviderError
  | CORateFloor Rate Rate
  deriving (Eq, Show)

data LateDue
  = LDNothing
  | LDNoInvoiceFigures
  | LDNeedsRate CryptoCurrency Rate
  | LDCredit LateCredit
  deriving (Eq, Show)

creditLateInvoice :: DBStore -> (CryptoCurrency -> IO (Either ProviderError Rate)) -> InvoiceId -> UTCTime -> IO CreditOutcome
creditLateInvoice st readRate invId@(InvoiceId invText) now =
  withTransaction st (`lateCreditState` invId) >>= \case
    Just s | creditable s -> case lateCreditOf s Nothing of
      LDCredit _ -> credit Nothing
      LDNoInvoiceFigures -> pure CONoInvoiceFigures
      LDNeedsRate coin invoiceRate ->
        readRate coin >>= \case
          Left e -> pure $ CORateFailed e
          Right rate
            | toRational (rateValue rate) < minCurrentRateFraction * toRational (rateValue invoiceRate) ->
                CORateFloor invoiceRate rate <$ withTransaction st (\db -> setLateSupport db invId LSRateFloor)
            | otherwise -> credit (Just rate)
      LDNothing -> pure CONothingDue
    _ -> pure CONothingDue
  where
    credit rate = withTransaction st $ \db -> do
      lockInvoice db invId now
      lateCreditState db invId >>= \case
        Just s | creditable s, LDCredit c <- lateCreditOf s rate -> do
          applied <- creditLatePayment db c now
          if applied then pure $ COCredited (lateMoneyOf s) c else E.throwIO $ BrokenInvariant $ "the credit of invoice " <> invText <> " found its number taken under the invoice's lock"
        _ -> pure CONothingDue

-- | An order its invoice paid gets credits only for the crypto paid above what the invoice asked.
lateMoneyOf :: LateCreditState -> LateMoney
lateMoneyOf LateCreditState {lsPhase} = if lsPhase == PUnpaid then LMLate else LMOverpaid

-- | A closed order's money, or an overpaid order's crypto, unless a rate below the floor sent it to support for good, so a later
-- recovery cannot credit money refunded by hand.
creditable :: LateCreditState -> Bool
creditable s@LateCreditState {lsLive, lsSupport} =
  lsSupport /= Just LSRateFloor && case lateMoneyOf s of
    LMLate -> not lsLive
    LMOverpaid -> True

-- | The next credit of the money not yet credited: card cents as they are, crypto units as their share of the invoice's amount at the
-- lower rate, rounded down to the cent.
lateCreditOf :: LateCreditState -> Maybe Rate -> LateDue
lateCreditOf s@LateCreditState {lsInvoice, lsRegistration, lsCredits, lsInvoiceAmount, lsCreditedCents, lsFigures} rate = case lsFigures of
  LFCard {lfConfirmedCents}
    | lateMoneyOf s == LMLate && lfConfirmedCents > lsCreditedCents -> LDCredit $ credit $ CACard (lfConfirmedCents - lsCreditedCents)
    | otherwise -> LDNothing
  LFCrypto {lfCoin, lfConfirmedUnits, lfInvoicedUnits, lfInvoiceRate, lfCreditedUnits}
    | units <= 0 -> LDNothing
    | otherwise -> case (,) <$> lfInvoiceRate <*> mfilter (> 0) lfInvoicedUnits of
        Nothing -> LDNoInvoiceFigures
        Just (invoiceRate, invoiced) -> case rate of
          Nothing -> LDNeedsRate lfCoin invoiceRate
          Just current ->
            let used = if rateValue current < rateValue invoiceRate then current else invoiceRate
                share = toInteger lsInvoiceAmount * units % toInteger invoiced
                cents = floor (share * toRational (rateValue used) / toRational (rateValue invoiceRate))
             in LDCredit $ credit CACrypto {caUnits = fromInteger units, caInvoiceRate = invoiceRate, caCurrentRate = current, caRateUsed = used, caCents = cents}
    where
      confirmed = maybe 0 toInteger lfConfirmedUnits
      creditableUnits = case lateMoneyOf s of
        LMLate -> confirmed
        LMOverpaid -> maybe 0 (max 0 . (confirmed -) . toInteger) lfInvoicedUnits
      units = creditableUnits - toInteger lfCreditedUnits
  where
    credit lcAmount = LateCredit {lcInvoice = lsInvoice, lcCreditNo = lsCredits + 1, lcRegistration = lsRegistration, lcAmount}
