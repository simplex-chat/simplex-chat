{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Balances
  ( RefundRefusal (..),
    RefundRecord (..),
    recordRefund,
    Spend (..),
    checkSpendBalance,
    coveredOrderInsert,
    differenceOrderInsert,
  )
where

import qualified Control.Exception as E
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Names.Record (USDCents)
import StoreService.Names.Orders (OrderRefusal (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Store (Balance (..), BalanceHold (..), BalanceInsertRefused (..), BalanceLink, BalanceUse (..), BrokenInvariant (..), NameRegistration, NewRegistration, PaidOrder, Spend (..), balanceByReference, balanceChain, createBalanceRegistration, createPaidRegistration, getBalance, getRegistration, heldBy, refundBalance)
import StoreService.Names.Turns (Turns, withBalanceTurn)

-- | RefundAmount: the balance's amount now, which is not the cents given. RefundHeld: an order holds it, overdue or not,
-- since only the poller's sweep can close a holder and cancel its invoice at the provider.
data RefundRefusal = RefundNoOrder | RefundNoBalance | RefundAmount USDCents | RefundHeld BalanceHold
  deriving (Eq, Show)

-- | The chain names every order the money came through, for support to pay it back by the methods that paid it.
data RefundRecord = RefundRecord
  { rdRegistration :: RegistrationId,
    rdCents :: USDCents,
    rdChain :: [BalanceLink]
  }
  deriving (Eq, Show)

-- | The reference is the order's registration id or its name invoice's id. In the balance's turn and one transaction.
recordRefund :: Turns -> DBStore -> Text -> USDCents -> UTCTime -> IO (Either RefundRefusal RefundRecord)
recordRefund turns st ref cents now =
  withTransaction st (\db -> balanceByReference db ref now) >>= \case
    Nothing -> pure $ Left RefundNoOrder
    Just (rid, _) -> withBalanceTurn turns rid $ withTransaction st refund
  where
    refund db =
      refundable db $ \rid@(RegistrationId r) ->
        refundBalance db rid cents now >>= \case
          True -> Right . RefundRecord rid cents <$> balanceChain db rid
          -- only a writer outside the turn, as a late credit, changes the balance between the read and the write, so it is read again once
          False -> refundable db $ \_ -> E.throwIO $ BrokenInvariant $ "the refund of order " <> r <> " was refused, though its balance reads refundable"
    refundable db refundWith =
      balanceByReference db ref now >>= \case
        Nothing -> pure $ Left RefundNoOrder
        Just (rid, balance) -> maybe (refundWith rid) (pure . Left) (refusal balance)
    refusal :: Maybe Balance -> Maybe RefundRefusal
    refusal = \case
      Nothing -> Just RefundNoBalance
      Just Balance {blHold = Just hold} -> Just $ RefundHeld hold
      Just Balance {blAmount}
        | blAmount /= cents || blAmount <= 0 -> Just $ RefundAmount blAmount
        | otherwise -> Nothing

-- | The balance has the amount the page read and no open order holds it. A hold by an overdue order reads as none,
-- and that order is returned for the new order's transaction to close.
checkSpendBalance :: DBStore -> Spend -> UTCTime -> IO (Either OrderRefusal (Maybe NameRegistration))
checkSpendBalance st Spend {spSource, spBalance} now =
  withTransaction st $ \db ->
    getBalance db spSource now >>= \case
      Just balance@Balance {blAmount, blHold}
        | blAmount == spBalance && isNothing (heldBy balance) -> Right <$> maybe (pure Nothing) (getRegistration db . bhHolder) blHold
      balance -> pure $ Left $ ORBalanceChanged balance

-- | The order queued, its price spent from the balance read.
coveredOrderInsert :: Spend -> NewRegistration -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
coveredOrderInsert Spend {spSource, spPrice, spBalance} new db now =
  balanceRefusal db spSource now =<< createBalanceRegistration db new BalanceUse {buSource = spSource, buUsed = spPrice} spBalance now

-- | The order unpaid with its invoice for the difference, holding all of the balance read.
differenceOrderInsert :: Spend -> NewRegistration -> PaidOrder -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
differenceOrderInsert Spend {spSource, spBalance} new paid db now =
  balanceRefusal db spSource now =<< createPaidRegistration db new (Just BalanceUse {buSource = spSource, buUsed = spBalance}) paid now

-- | A write refused at the balance answers with the balance as this transaction reads it.
balanceRefusal :: DB.Connection -> RegistrationId -> UTCTime -> Either BalanceInsertRefused () -> IO (Either OrderRefusal ())
balanceRefusal db source now = \case
  Right () -> pure $ Right ()
  Left BIRBalanceChanged -> Left . ORBalanceChanged <$> getBalance db source now
  Left BIRLiveOrder -> pure $ Left OROrderExists
