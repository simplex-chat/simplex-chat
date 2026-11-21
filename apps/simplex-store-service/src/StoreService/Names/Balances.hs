{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Names.Balances
  ( RefundRefusal (..),
    RefundRecord (..),
    recordRefund,
    Spend (..),
    checkSpendBalance,
    coveredOrderInsert,
    differenceOrderInsert,
    reverseOrder,
    warnOfFraud,
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Logger.Simple (logError, logNote)
import Control.Monad (forM, unless)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String (StrEncoding (..), textEncode)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Names.Orders (OrderRefusal (..), OrdersEnv (..))
import StoreService.Names.Phases (publish)
import StoreService.Names.Registration (RegistrationId (..), TermsAcceptance (..))
import StoreService.Names.Store (Balance (..), BalanceHold (..), BalanceInsertRefused (..), BalanceLink, BalanceUse (..), BrokenInvariant (..), HoldState (..), InvoiceClose (..), NamePhase (..), NameRegistration (..), NameReveal (..), NameTransaction (..), NewRegistration, OrderPhase (..), PaidOrder, RegistrationTx (..), Spend (..), TxFor (..), TxSettlement (..), TxState (..), VersionWrite (..), balanceByReference, balanceChain, createBalanceRegistration, createPaidRegistration, getBalance, getRegistration, heldBy, lockRegistration, namePhase, orderLive, paymentBalance, paymentInvoice, recordReversal, refundBalance, registerTxSent, registrationTransactions, returnBalance, reversalEventRecorded, takeBackBalance, updateRegistration)
import StoreService.Names.Turns (Turns, withBalanceTurn, withRegistrationTurn)
import StoreService.Names.View (addressText)
import StoreService.Orders (RegistrationChanged (..), cancelOpenInvoice)
import StoreService.Providers (Provider (..), Reversal (..), providerErrorText, providerNamed)
import StoreService.Store.Invoices (InvoiceRow (..), currentSecond, getInvoice)
import StoreService.Waiters (Waiters)

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

data ReversalCase = RCUnpaid | RCCancel | RCRefunded | RCTakeBack

reversalCase :: NameRegistration -> Bool -> Bool -> ReversalCase
reversalCase r@NameRegistration {nrPhase} sent refunded
  | namePhase nrPhase == PUnpaid = RCUnpaid
  | orderLive r && not sent = RCCancel
  | refunded = RCRefunded
  | otherwise = RCTakeBack

reverseOrder :: OrdersEnv -> [Provider] -> Waiters -> InvoiceRow -> RegistrationId -> Text -> Reversal -> IO ()
reverseOrder oe@OrdersEnv {oeNow, oeStore, oePhases} providers waiters row@InvoiceRow {irInvoiceId} rid event reversal = do
  released <- releaseHold
  now <- oeNow
  inOrderTurn oe row rid what $ \db r ->
    recordReversal db event irInvoiceId reversal now >>= traverse (fmap (fmap (<> released)) . reversed db now r)
  where
    what = case reversal of
      RVDispute dispute (CurrencyAmount amount) -> "card dispute " <> dispute <> " of " <> tshow amount <> " cents"
      RVRefund charge (CurrencyAmount total) -> "refund of charge " <> charge <> " to " <> tshow total <> " cents in all"
    releaseHold = do
      now <- oeNow
      holder <- withTransaction oeStore $ \db ->
        reversalEventRecorded db irInvoiceId event >>= \case
          True -> pure Nothing
          False ->
            getRegistration db rid >>= \case
              Nothing -> pure Nothing
              Just r -> do
                sent <- registerTxSent db rid
                balance <- getBalance db rid now
                let refunded = maybe False ((> 0) . blRefunded) balance
                pure $ case (reversalCase r sent refunded, heldBy =<< balance) of
                  (RCCancel, Just BalanceHold {bhHolder, bhState = HSOpen _}) -> Just bhHolder
                  (RCTakeBack, Just BalanceHold {bhHolder, bhState = HSOpen _}) -> Just bhHolder
                  _ -> Nothing
      maybe (pure "") cancelHolder holder
    cancelHolder holder@(RegistrationId h) =
      (withTransaction oeStore (`getRegistration` holder) >>= traverse (getInvoice oeStore) . (paymentInvoice . nrPayment =<<)) >>= \case
        Just (Just InvoiceRow {irInvoiceId = holderInvoice, irStatus = ISOpen, irProvider, irProviderRef}) -> case providerNamed providers irProvider of
          Nothing -> E.throwIO $ userError $ T.unpack $ "no provider is configured for invoice " <> irProviderRef <> " of order " <> h <> ", which holds the balance"
          Just Provider {pCancelInvoice} ->
            pCancelInvoice irProviderRef >>= \case
              Left e -> E.throwIO $ userError $ T.unpack $ "invoice " <> irProviderRef <> " of order " <> h <> ", which holds the balance, could not be cancelled: " <> providerErrorText e
              Right () -> do
                now <- currentSecond
                closed <- cancelOpenInvoice oeStore waiters (Just oePhases) holderInvoice now
                pure $ if closed then "; the open invoice of order " <> h <> ", which held its balance, is cancelled" else ""
        _ -> pure ""
    reversed db now r@NameRegistration {nrId, nrPayment} cents = do
      locked <- lockRegistration db r
      unless locked $ E.throwIO $ RegistrationChanged nrId
      sent <- registerTxSent db nrId
      refunded <- maybe False ((> 0) . blRefunded) <$> getBalance db nrId now
      let takeBack = takeBackBalance db irInvoiceId nrId cents now
          takenText taken = if taken > 0 then "; " <> centsText taken <> " taken back from its balance" else ""
      case reversalCase r sent refunded of
        RCUnpaid -> pure (Nothing, "its payment is not recorded yet, so it is closed as reversed when it is")
        RCCancel -> do
          v <-
            updateRegistration db r {nrPhase = OClosed ICReversed, nrReveal = Nothing} now >>= \case
              Written v -> pure v
              _ -> E.throwIO $ RegistrationChanged nrId
          returned <- forM (paymentBalance nrPayment) $ \BalanceUse {buSource = source@(RegistrationId s), buUsed} -> do
            back <- returnBalance db source buUsed now
            unless back $ E.throwIO $ BrokenInvariant $ "the balance of order " <> s <> " that paid order " <> idText nrId <> " is missing"
            pure $ "; " <> centsText buUsed <> " returned to the balance of order " <> s
          taken <- takeBack
          pure (Just v, "it is cancelled, as no register transaction of it was sent, and its name is released" <> fromMaybe "" returned <> takenText taken)
        RCRefunded -> pure (Nothing, "support recorded its refund, so nothing changed")
        RCTakeBack -> do
          taken <- takeBack
          let kept = if sent then "its register transaction was sent" else "it is " <> textEncode (namePhase $ nrPhase r)
          pure (Nothing, "it is not cancelled, as " <> kept <> takenText taken)

warnOfFraud :: OrdersEnv -> InvoiceRow -> RegistrationId -> Text -> IO ()
warnOfFraud oe row rid warning = inOrderTurn oe row rid ("early fraud warning, " <> warning) $ \_ _ -> pure $ Just (Nothing, "nothing changed")

inOrderTurn :: OrdersEnv -> InvoiceRow -> RegistrationId -> Text -> (DB.Connection -> NameRegistration -> IO (Maybe (Maybe Int64, Text))) -> IO ()
inOrderTurn oe@OrdersEnv {oeStore, oeTurns, oePhases, oeReport} row@InvoiceRow {irProviderRef} rid what change =
  withTransaction oeStore (`getRegistration` rid) >>= \case
    Nothing -> logError $ "names: the " <> what <> " on payment " <> irProviderRef <> " has no order " <> idText rid
    Just r ->
      withRegistrationTurn oeStore oeTurns r (\current -> withTransaction oeStore $ \db -> change db current >>= traverse (\(v, outcome) -> (v,outcome,) <$> orderEvidence db row current)) >>= \case
        Just (Right (Just (v, outcome, evidence))) -> do
          mapM_ (atomically . publish oePhases rid) v
          let alert = "names: " <> what <> " on payment " <> irProviderRef <> " of order " <> idText rid <> ": " <> outcome <> ". Evidence: " <> evidence
          logError alert
          oeReport alert
        Just (Right Nothing) -> logNote $ "names: the " <> what <> " on payment " <> irProviderRef <> " was handled before"
        Just (Left _) -> inOrderTurn oe row rid what change
        Nothing -> logError $ "names: the " <> what <> " on payment " <> irProviderRef <> " has no order " <> idText rid

orderEvidence :: DB.Connection -> InvoiceRow -> NameRegistration -> IO Text
orderEvidence db InvoiceRow {irProviderRef, irAmount = CurrencyAmount amount, irStatus} NameRegistration {nrId, nrOwner, nrOrderHash, nrPaidAt, nrReveal} = do
  txs <- registrationTransactions db nrId
  pure $
    T.intercalate
      "; "
      [ "payment " <> irProviderRef <> " of " <> tshow amount <> " cents, invoice " <> textEncode irStatus <> maybe ", payment time not recorded" ((", paid at " <>) . tshow) nrPaidAt,
        "ordered by " <> addressText nrOwner <> ", order hash " <> encoded nrOrderHash,
        maybe "its signed registration is no longer stored" (\NameReveal {rvSignature, rvAcceptance = TermsAcceptance {termsHash}} -> "registration signed " <> encoded rvSignature <> " with terms " <> encoded termsHash) nrReveal,
        transactions "commit" RTCommit txs,
        transactions "register" RTRegister txs
      ]
  where
    transactions name kind txs = case [tx | tx@NameTransaction {ntFor = TxRegistration k _} <- txs, k == kind] of
      [] -> "no " <> name <> " transaction sent"
      sent -> name <> " " <> T.intercalate ", " (map transaction sent)
    transaction NameTransaction {ntHash, ntState} = encoded ntHash <> " " <> case ntState of
      TxPending -> "pending"
      TxSettled (TSMinedAt block) -> "mined at block " <> tshow block
      TxSettled (TSRevertedAt block) -> "reverted at block " <> tshow block
      TxSettled TSDroppedTx -> "dropped"
    encoded :: StrEncoding a => a -> Text
    encoded = safeDecodeUtf8 . strEncode

idText :: RegistrationId -> Text
idText (RegistrationId t) = t

centsText :: USDCents -> Text
centsText cents = tshow (toInteger cents) <> " cents"
