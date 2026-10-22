{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module StoreService.Names.Balances
  ( RefundRefusal (..),
    RefundRecord (..),
    recordRefund,
  )
where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import StoreService.Names.Phases (RegistrationId)
import StoreService.Names.Store (Balance (..), BalanceHold, BalanceLink, balanceByReference, balanceChain, refundBalance)
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
