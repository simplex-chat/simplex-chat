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

import Data.Ratio ((%))
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Simplex.Chat.PaymentService.Types (CryptoCurrency, InvoiceId)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import StoreService.Names.Store (LateCredit (..), LateCreditState (..), LateSupport (..), NamePhase (..), creditLatePayment, lateCreditState, setLateSupport)
import StoreService.Providers (ProviderError (..), Rate (..))
import StoreService.Store.Invoices (lockInvoice)
import Text.Read (readMaybe)

-- | A current rate below this fraction of the invoice's is not used: it may be a glitch or a real fall, which the owner decides on.
minCurrentRateFraction :: Rational
minCurrentRateFraction = 1 % 2

-- | A closed order's late money, or the extra crypto of an invoice that paid its order.
data LateMoney = LMLate | LMOverpaid
  deriving (Eq, Show)

-- | CONoInvoiceFigures: crypto due with no invoice rate or invoiced units recorded to credit it at. CORefused: the credit's number was
-- taken meanwhile, which the invoice's lock rules out.
data CreditOutcome
  = CONothingDue
  | COCredited LateMoney LateCredit
  | CONoInvoiceFigures
  | CORateFailed ProviderError
  | CORateFloor Text Rate
  | CORefused
  deriving (Eq, Show)

creditLateInvoice :: DBStore -> (CryptoCurrency -> IO (Either ProviderError Rate)) -> InvoiceId -> UTCTime -> IO CreditOutcome
creditLateInvoice st readRate invId now =
  withTransaction st (`lateCreditState` invId) >>= \case
    Just s | creditable s && moneyDue s -> case lsCoin s of
      Nothing -> credit Nothing
      Just coin -> case invoiceFigures s of
        Nothing -> pure CONoInvoiceFigures
        Just (invoiceText, invoiceValue, _) ->
          readRate coin >>= \case
            Left e -> pure $ CORateFailed e
            Right rate
              | toRational (rateValue rate) < minCurrentRateFraction * toRational invoiceValue ->
                  CORateFloor invoiceText rate <$ withTransaction st (\db -> setLateSupport db invId LSRateFloor)
              | otherwise -> credit (Just rate)
    _ -> pure CONothingDue
  where
    credit rate = withTransaction st $ \db -> do
      lockInvoice db invId now
      lateCreditState db invId >>= \case
        Just s | creditable s, Just c <- lateCreditOf s rate -> do
          applied <- creditLatePayment db c now
          pure $ if applied then COCredited (lateMoneyOf s) c else CORefused
        _ -> pure CONothingDue

-- | An order its invoice paid gets credits only for the crypto paid above what the invoice asked.
lateMoneyOf :: LateCreditState -> LateMoney
lateMoneyOf LateCreditState {lsPhase} = if lsPhase == PUnpaid then LMLate else LMOverpaid

-- | A closed order's money, or an overpaid order's crypto, unless a rate below the floor sent it to support for good, so a later
-- recovery cannot credit money refunded by hand.
creditable :: LateCreditState -> Bool
creditable s@LateCreditState {lsLive, lsCoin, lsSupport} =
  lsSupport /= Just LSRateFloor && case lateMoneyOf s of
    LMLate -> not lsLive
    LMOverpaid -> lsCoin /= Nothing

moneyDue :: LateCreditState -> Bool
moneyDue s = case lsCoin s of
  Nothing -> centsDue s > 0
  Just _ -> unitsDue s > 0

-- | Card money is never paid above its invoice, so only a closed order's counts.
centsDue :: LateCreditState -> Int
centsDue s@LateCreditState {lsConfirmedCents, lsCreditedCents} = case lateMoneyOf s of
  LMLate -> lsConfirmedCents - lsCreditedCents
  LMOverpaid -> 0

-- | All of a closed order's confirmed units, and only those above the invoiced units of an overpaid one, less what was credited.
unitsDue :: LateCreditState -> Integer
unitsDue s@LateCreditState {lsConfirmedUnits, lsInvoicedUnits, lsCreditedUnits} = creditableUnits - toInteger lsCreditedUnits
  where
    confirmed = maybe 0 toInteger lsConfirmedUnits
    creditableUnits = case lateMoneyOf s of
      LMLate -> confirmed
      LMOverpaid -> maybe 0 (max 0 . (confirmed -) . toInteger) lsInvoicedUnits

-- | The invoice's rate as written and as a decimal, and its invoiced units, above 0.
invoiceFigures :: LateCreditState -> Maybe (Text, Scientific, Integer)
invoiceFigures LateCreditState {lsInvoiceRate, lsInvoicedUnits} = do
  t <- lsInvoiceRate
  v <- readMaybe (T.unpack t)
  invoiced <- toInteger <$> lsInvoicedUnits
  if invoiced > 0 then Just (t, v, invoiced) else Nothing

-- | The next credit of the money not yet credited: card cents as they are, crypto units as their share of the invoice's amount at the
-- lower rate, rounded down to the cent.
lateCreditOf :: LateCreditState -> Maybe Rate -> Maybe LateCredit
lateCreditOf s@LateCreditState {lsInvoice, lsRegistration, lsCredits, lsInvoiceAmount} rate = case (lsCoin s, rate, invoiceFigures s) of
  (Nothing, _, _)
    | centsDue s > 0 -> Just (credit Nothing Nothing Nothing Nothing (centsDue s))
  (Just _, Just Rate {rateText, rateValue}, Just (invoiceText, invoiceValue, invoiced))
    | unitsDue s > 0 ->
        let (usedText, usedValue) = if rateValue < invoiceValue then (rateText, rateValue) else (invoiceText, invoiceValue)
            share = toInteger lsInvoiceAmount * unitsDue s % invoiced
            cents = floor (share * toRational usedValue / toRational invoiceValue)
         in Just (credit (Just $ fromInteger $ unitsDue s) (Just invoiceText) (Just rateText) (Just usedText) cents)
  _ -> Nothing
  where
    credit lcUnits lcInvoiceRate lcCurrentRate lcRateUsed lcCents =
      LateCredit {lcInvoice = lsInvoice, lcCreditNo = lsCredits + 1, lcRegistration = lsRegistration, lcUnits, lcInvoiceRate, lcCurrentRate, lcRateUsed, lcCents}
