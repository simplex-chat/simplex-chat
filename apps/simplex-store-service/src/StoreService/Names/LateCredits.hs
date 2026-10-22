{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The credit of a closed order's late money to its balance: only confirmed money not yet credited, crypto at the lower
-- of the invoice's rate and the current one. The rate is read outside any transaction, so the credit runs after the
-- settlement that recorded the money commits, and again from the poller's pass for whatever is still not credited.
module StoreService.Names.LateCredits
  ( CreditOutcome (..),
    minCurrentRateFraction,
    creditLateInvoice,
    lateCreditOf,
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
import StoreService.Providers.BTCPay (unitDecimals)
import StoreService.Store.Invoices (lockInvoice)
import Text.Read (readMaybe)

-- | A current rate below this fraction of the invoice's is not used: it may be a glitch or a real fall, which the owner decides on.
minCurrentRateFraction :: Rational
minCurrentRateFraction = 1 % 2

-- | CONoInvoiceRate: crypto due with no invoice rate recorded to credit it at. CORefused: the credit's number was taken meanwhile,
-- which the invoice's lock rules out.
data CreditOutcome
  = CONothingDue
  | COCredited LateCredit
  | CONoInvoiceRate
  | CORateFailed ProviderError
  | CORateFloor Text Rate
  | CORefused
  deriving (Eq, Show)

creditLateInvoice :: DBStore -> (CryptoCurrency -> IO (Either ProviderError Rate)) -> InvoiceId -> UTCTime -> IO CreditOutcome
creditLateInvoice st readRate invId now =
  withTransaction st (`lateCreditState` invId) >>= \case
    Just s | creditable s && moneyDue s -> case lsCoin s of
      Nothing -> credit Nothing
      Just coin -> case invoiceRate s of
        Nothing -> pure CONoInvoiceRate
        Just (invoiceText, invoiceValue) ->
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
          pure $ if applied then COCredited c else CORefused
        _ -> pure CONothingDue

-- | A closed order's money, unless a rate below the floor sent it to support for good, so a later recovery cannot credit money refunded by hand.
creditable :: LateCreditState -> Bool
creditable LateCreditState {lsPhase, lsLive, lsSupport} = lsPhase == PUnpaid && not lsLive && lsSupport /= Just LSRateFloor

moneyDue :: LateCreditState -> Bool
moneyDue s@LateCreditState {lsConfirmedCents, lsCreditedCents} = case lsCoin s of
  Nothing -> lsConfirmedCents > lsCreditedCents
  Just _ -> unitsDue s > 0

unitsDue :: LateCreditState -> Integer
unitsDue LateCreditState {lsConfirmedUnits, lsCreditedUnits} = maybe 0 toInteger lsConfirmedUnits - toInteger lsCreditedUnits

invoiceRate :: LateCreditState -> Maybe (Text, Scientific)
invoiceRate LateCreditState {lsInvoiceRate} = lsInvoiceRate >>= \t -> (t,) <$> readMaybe (T.unpack t)

-- | The next credit of the money not yet credited: card cents as they are, crypto units at the lower rate, rounded down to the cent.
lateCreditOf :: LateCreditState -> Maybe Rate -> Maybe LateCredit
lateCreditOf s@LateCreditState {lsInvoice, lsRegistration, lsCredits, lsConfirmedCents, lsCreditedCents} rate = case (lsCoin s, rate, invoiceRate s) of
  (Nothing, _, _)
    | lsConfirmedCents > lsCreditedCents -> Just (credit Nothing Nothing Nothing Nothing (lsConfirmedCents - lsCreditedCents))
  (Just coin, Just Rate {rateText, rateValue}, Just (invoiceText, invoiceValue))
    | unitsDue s > 0 ->
        let (usedText, usedValue) = if rateValue < invoiceValue then (rateText, rateValue) else (invoiceText, invoiceValue)
            cents = floor (fromInteger (unitsDue s) * toRational usedValue * 100 / 10 ^ unitDecimals coin)
         in Just (credit (Just $ fromInteger $ unitsDue s) (Just invoiceText) (Just rateText) (Just usedText) cents)
  _ -> Nothing
  where
    credit lcUnits lcInvoiceRate lcCurrentRate lcRateUsed lcCents =
      LateCredit {lcInvoice = lsInvoice, lcCreditNo = lsCredits + 1, lcRegistration = lsRegistration, lcUnits, lcInvoiceRate, lcCurrentRate, lcRateUsed, lcCents}
