{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Names.Balances
  ( RefundRefusal (..),
    RefundRecord (..),
    recordRefund,
    Spend (..),
    spendTerms,
    checkSpendBalance,
    coveredOrderInsert,
    differenceOrderInsert,
  )
where

import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Chat.PaymentService.Types (ServicePaymentMethod)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import StoreService.Names.Orders (OrderRefusal (..))
import StoreService.Names.Phases (RegistrationId)
import StoreService.Names.Store (Balance (..), BalanceHold, BalanceLink, NameRegistration, NewRegistration, OrderTerms (..), PaidOrder, RegistrationRefused (..), balanceByReference, balanceChain, createBalanceRegistration, createPaidRegistration, getBalance, heldBy, overdueHolder, refundBalance)
import StoreService.Names.Turns (Turns, withBalanceTurn)

-- | RefundAmount: the balance's amount now, which is not the cents given. RefundHeld: an order holds it, overdue or not,
-- since only the poller's sweep can close a holder and cancel its invoice at the provider.
data RefundRefusal = RefundNoOrder | RefundNoBalance | RefundAmount Int | RefundHeld BalanceHold
  deriving (Eq, Show)

-- | The chain names every order the money came through, for support to pay it back by the methods that paid it.
data RefundRecord = RefundRecord
  { rdRegistration :: RegistrationId,
    rdCents :: Int,
    rdChain :: [BalanceLink]
  }
  deriving (Eq, Show)

-- | The reference is the order's registration id or its name invoice's id. In the balance's turn and one transaction.
recordRefund :: Turns -> DBStore -> Text -> Int -> UTCTime -> IO (Either RefundRefusal RefundRecord)
recordRefund turns st ref cents now =
  withTransaction st (\db -> balanceByReference db ref now) >>= \case
    Nothing -> pure $ Left RefundNoOrder
    Just (rid, _) -> withBalanceTurn turns rid $ withTransaction st (refund True)
  where
    refund retry db =
      balanceByReference db ref now >>= \case
        Nothing -> pure $ Left RefundNoOrder
        Just (rid, balance) -> case refusal balance of
          Just r -> pure $ Left r
          Nothing ->
            refundBalance db rid cents now >>= \case
              True -> Right . RefundRecord rid cents <$> balanceChain db rid
              -- only a writer outside the turn, as a late credit, changes the balance between the read and the write, so it is read again once
              False
                | retry -> refund False db
                | otherwise -> pure $ Left $ RefundAmount $ maybe 0 blAmount balance
    refusal :: Maybe Balance -> Maybe RefundRefusal
    refusal = \case
      Nothing -> Just RefundNoBalance
      Just Balance {blHold = Just hold} -> Just $ RefundHeld hold
      Just Balance {blAmount}
        | blAmount /= cents || blAmount <= 0 -> Just $ RefundAmount blAmount
        | otherwise -> Nothing

-- | A new order paid from a balance. Its hash covers the paying order, the price, the balance the page read,
-- and the method, present exactly when the price is more than that balance.
data Spend = Spend
  { spSource :: RegistrationId,
    spPrice :: Int,
    spBalance :: Int,
    spMethod :: Maybe ServicePaymentMethod
  }

spendTerms :: Spend -> OrderTerms
spendTerms Spend {spSource, spPrice, spBalance, spMethod} = OTBalance spSource spPrice spBalance spMethod

-- | The balance has the amount the page read and no open order holds it. A hold by an overdue order reads as none,
-- and that order is returned for the new order's transaction to close.
checkSpendBalance :: DBStore -> Spend -> UTCTime -> IO (Either OrderRefusal (Maybe NameRegistration))
checkSpendBalance st Spend {spSource, spBalance} now =
  withTransaction st $ \db ->
    getBalance db spSource now >>= \case
      Just balance@Balance {blAmount}
        | blAmount == spBalance && isNothing (heldBy balance) -> Right <$> overdueHolder db spSource now
      balance -> pure $ Left $ ORBalanceChanged balance

-- | The order queued, its price spent from the balance read.
coveredOrderInsert :: Spend -> NewRegistration -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
coveredOrderInsert Spend {spSource, spPrice, spBalance} new db now =
  balanceRefusal db spSource now =<< createBalanceRegistration db new spSource spPrice spBalance now

-- | The order unpaid with its invoice for the difference, holding all of the balance read.
differenceOrderInsert :: Spend -> NewRegistration -> PaidOrder -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
differenceOrderInsert spend@Spend {spSource} new paid db now =
  balanceRefusal db spSource now =<< createPaidRegistration db new (spendTerms spend) paid now

-- | A write refused at the balance answers with the balance as this transaction reads it.
balanceRefusal :: DB.Connection -> RegistrationId -> UTCTime -> Either RegistrationRefused () -> IO (Either OrderRefusal ())
balanceRefusal db source now = \case
  Right () -> pure $ Right ()
  Left RRBalanceChanged -> Left . ORBalanceChanged <$> getBalance db source now
  Left RRLiveOrder -> pure $ Left OROrderExists
  -- a spend reserves no code
  Left RRCodeNotReservable -> pure $ Left OROrderExists
