{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The credit of a name invoice's money to its order's balance: a closed order's late money, or the extra of an invoice paid above what
-- it asked. Only confirmed money not yet credited, crypto as its share of the invoice's amount at the lower of the invoice's rate and
-- the current one. The rate is read outside any transaction, so the credit runs after the settlement that recorded the money commits,
-- and again from the poller's pass for whatever is still not credited.
module StoreService.Names.LateCredits
  ( CreditOutcome (..),
    LateMoney (..),
    minCurrentRateFraction,
    creditLateInvoice,
    lateCreditOf,
    lateMoneyOf,
  )
where

import qualified Control.Exception as E
import Data.Ratio ((%))
import Data.Time.Clock (UTCTime)
import Simplex.Chat.PaymentService.Types (CryptoCurrency, InvoiceId (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Names.Record (USDCents)
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

creditLateInvoice :: DBStore -> (CryptoCurrency -> IO (Either ProviderError Rate)) -> InvoiceId -> UTCTime -> IO CreditOutcome
creditLateInvoice st readRate invId@(InvoiceId invText) now =
  withTransaction st (`lateCreditState` invId) >>= \case
    Just s@LateCreditState {lsFigures} | creditable s && moneyDue s -> case lsFigures of
      LFCard {} -> credit Nothing
      LFCrypto {lfCoin} -> case invoiceFigures lsFigures of
        Nothing -> pure CONoInvoiceFigures
        Just (invoiceRate, _) ->
          readRate lfCoin >>= \case
            Left e -> pure $ CORateFailed e
            Right rate
              | toRational (rateValue rate) < minCurrentRateFraction * toRational (rateValue invoiceRate) ->
                  CORateFloor invoiceRate rate <$ withTransaction st (\db -> setLateSupport db invId LSRateFloor)
              | otherwise -> credit (Just rate)
    _ -> pure CONothingDue
  where
    credit rate = withTransaction st $ \db -> do
      lockInvoice db invId now
      lateCreditState db invId >>= \case
        Just s | creditable s, Just c <- lateCreditOf s rate -> do
          applied <- creditLatePayment db c now
          if applied then pure $ COCredited (lateMoneyOf s) c else E.throwIO $ BrokenInvariant $ "the credit of invoice " <> invText <> " found its number taken under the invoice's lock"
        _ -> pure CONothingDue

-- | An order its invoice paid gets credits only for the crypto paid above what the invoice asked.
lateMoneyOf :: LateCreditState -> LateMoney
lateMoneyOf LateCreditState {lsPhase} = if lsPhase == PUnpaid then LMLate else LMOverpaid

-- | A closed order's money, or an overpaid order's crypto, unless a rate below the floor sent it to support for good, so a later
-- recovery cannot credit money refunded by hand.
creditable :: LateCreditState -> Bool
creditable s@LateCreditState {lsLive, lsFigures, lsSupport} =
  lsSupport /= Just LSRateFloor && case lateMoneyOf s of
    LMLate -> not lsLive
    LMOverpaid -> case lsFigures of
      LFCard {} -> False
      LFCrypto {} -> True

moneyDue :: LateCreditState -> Bool
moneyDue s@LateCreditState {lsFigures} = case lsFigures of
  LFCard {} -> centsDue s > 0
  LFCrypto {} -> unitsDue s > 0

-- | Card money is never paid above its invoice, so only a closed order's counts.
centsDue :: LateCreditState -> USDCents
centsDue s@LateCreditState {lsFigures, lsCreditedCents} = case (lateMoneyOf s, lsFigures) of
  (LMLate, LFCard {lfConfirmedCents}) -> lfConfirmedCents - lsCreditedCents
  _ -> 0

-- | All of a closed order's confirmed units, and only those above the invoiced units of an overpaid one, less what was credited.
unitsDue :: LateCreditState -> Integer
unitsDue s@LateCreditState {lsFigures} = case lsFigures of
  LFCard {} -> 0
  LFCrypto {lfConfirmedUnits, lfInvoicedUnits, lfCreditedUnits} ->
    let confirmed = maybe 0 toInteger lfConfirmedUnits
        creditableUnits = case lateMoneyOf s of
          LMLate -> confirmed
          LMOverpaid -> maybe 0 (max 0 . (confirmed -) . toInteger) lfInvoicedUnits
     in creditableUnits - toInteger lfCreditedUnits

-- | The invoice's rate, and its invoiced units, above 0.
invoiceFigures :: LateFigures -> Maybe (Rate, Integer)
invoiceFigures = \case
  LFCrypto {lfInvoiceRate = Just rate, lfInvoicedUnits = Just invoiced} | invoiced > 0 -> Just (rate, toInteger invoiced)
  _ -> Nothing

-- | The next credit of the money not yet credited: card cents as they are, crypto units as their share of the invoice's amount at the
-- lower rate, rounded down to the cent.
lateCreditOf :: LateCreditState -> Maybe Rate -> Maybe LateCredit
lateCreditOf s@LateCreditState {lsInvoice, lsRegistration, lsCredits, lsInvoiceAmount, lsFigures} rate = case (lsFigures, rate, invoiceFigures lsFigures) of
  (LFCard {}, _, _)
    | centsDue s > 0 -> Just (credit (CACard (centsDue s)))
  (LFCrypto {}, Just current, Just (invoiceRate, invoiced))
    | unitsDue s > 0 ->
        let used = if rateValue current < rateValue invoiceRate then current else invoiceRate
            share = toInteger lsInvoiceAmount * unitsDue s % invoiced
            cents = floor (share * toRational (rateValue used) / toRational (rateValue invoiceRate))
         in Just (credit CACrypto {caUnits = fromInteger $ unitsDue s, caInvoiceRate = invoiceRate, caCurrentRate = current, caRateUsed = used, caCents = cents})
  _ -> Nothing
  where
    credit lcAmount = LateCredit {lcInvoice = lsInvoice, lcCreditNo = lsCredits + 1, lcRegistration = lsRegistration, lcAmount}
