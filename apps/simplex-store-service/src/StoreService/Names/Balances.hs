{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Names.Balances
  ( RefundRefusal (..),
    RefundRecord (..),
    recordRefund,
    reverseOrder,
    warnOfFraud,
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Logger.Simple (logError, logNote)
import Control.Monad (unless)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String (StrEncoding (..), textEncode)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Phases (RegistrationId (..), publish)
import StoreService.Names.Registration (TermsAcceptance (..))
import StoreService.Names.Store (Balance (..), BalanceLink, BrokenInvariant (..), InvoiceClose (..), NamePhase (..), NameRegistration (..), NameReveal (..), NameTransaction (..), OrderPhase (..), RegistrationTx (..), TxFor (..), TxSettlement (..), TxState (..), VersionWrite (..), balanceByReference, balanceLink, getBalance, getRegistration, lockRegistration, namePhase, orderLive, recordReversal, refundBalance, registerTxSent, registrationTransactions, takeBackBalance, updateRegistration)
import StoreService.Names.Turns (Turns, withBalanceTurn, withRegistrationTurn)
import StoreService.Names.View (addressText)
import StoreService.Orders (RegistrationChanged (..))
import StoreService.Providers (Reversal (..))
import StoreService.Store.Invoices (InvoiceRow (..), lockInvoice)

-- | RefundAmount: the balance's amount now, which is not the cents given.
data RefundRefusal = RefundNoOrder | RefundNoBalance | RefundAmount USDCents
  deriving (Eq, Show)

-- | The order and the money paid for it, for support to pay it back by the method that paid it.
data RefundRecord = RefundRecord
  { rdRegistration :: RegistrationId,
    rdCents :: USDCents,
    rdLink :: Maybe BalanceLink
  }
  deriving (Eq, Show)

-- | The reference is the order's registration id or its name invoice's id. In the balance's turn and one transaction.
recordRefund :: Turns -> DBStore -> Text -> USDCents -> UTCTime -> IO (Either RefundRefusal RefundRecord)
recordRefund turns st ref cents now =
  withTransaction st (`balanceByReference` ref) >>= \case
    Nothing -> pure $ Left RefundNoOrder
    Just (rid, _) -> withBalanceTurn turns rid $ withTransaction st refund
  where
    refund db =
      refundable db $ \rid@(RegistrationId r) ->
        refundBalance db rid cents now >>= \case
          True -> Right . RefundRecord rid cents <$> balanceLink db rid
          -- only a writer outside the turn, as a late credit, changes the balance between the read and the write, so it is read again once
          False -> refundable db $ \_ -> E.throwIO $ BrokenInvariant $ "the refund of order " <> r <> " was refused, though its balance reads refundable"
    refundable db refundWith =
      balanceByReference db ref >>= \case
        Nothing -> pure $ Left RefundNoOrder
        Just (rid, balance) -> maybe (refundWith rid) (pure . Left) (refusal balance)
    refusal :: Maybe Balance -> Maybe RefundRefusal
    refusal = \case
      Nothing -> Just RefundNoBalance
      Just Balance {blAmount}
        | blAmount /= cents || blAmount <= 0 -> Just $ RefundAmount blAmount
        | otherwise -> Nothing

data ReversalCase = RCUnpaid | RCCancel | RCRefunded | RCTakeBack

reversalCase :: NameRegistration -> Bool -> Bool -> ReversalCase
reversalCase r@NameRegistration {nrPhase} sent refunded
  | namePhase nrPhase == PUnpaid = RCUnpaid
  | orderLive r && not sent = RCCancel
  | refunded = RCRefunded
  | otherwise = RCTakeBack

reverseOrder :: OrdersEnv -> InvoiceRow -> RegistrationId -> Text -> Reversal -> IO ()
reverseOrder oe@OrdersEnv {oeNow} row@InvoiceRow {irInvoiceId} rid event reversal = do
  now <- oeNow
  inOrderTurn oe row rid what $ \db r -> do
    lockInvoice db irInvoiceId now
    recordReversal db event irInvoiceId reversal >>= traverse (reversed db now r)
  where
    what = case reversal of
      RVDispute dispute (CurrencyAmount amount) -> "card dispute " <> dispute <> " of " <> tshow amount <> " cents"
      RVRefund charge (CurrencyAmount total) -> "refund of charge " <> charge <> " to " <> tshow total <> " cents in all"
    reversed db now r@NameRegistration {nrId} cents = do
      locked <- lockRegistration db r
      unless locked $ E.throwIO $ RegistrationChanged nrId
      sent <- registerTxSent db nrId
      refunded <- maybe False ((> 0) . blRefunded) <$> getBalance db nrId
      let takeBack = takeBackBalance db irInvoiceId nrId cents now
          takenText taken = if taken > 0 then "; " <> centsText taken <> " taken back from its balance" else ""
      case reversalCase r sent refunded of
        RCUnpaid -> pure (Nothing, "its payment is not recorded yet, so it is closed as reversed when it is")
        RCCancel -> do
          v <-
            updateRegistration db r {nrPhase = OClosed ICReversed, nrReveal = Nothing} now >>= \case
              Written v -> pure v
              _ -> E.throwIO $ RegistrationChanged nrId
          taken <- takeBack
          pure (Just v, "it is cancelled, as no register transaction of its current name was sent, and its name is released" <> takenText taken)
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
orderEvidence db InvoiceRow {irProviderRef, irAmount = CurrencyAmount amount, irStatus} NameRegistration {nrId, nrOwner, nrOrderHash, nrSettledAt, nrReveal} = do
  txs <- registrationTransactions db nrId
  pure $
    T.intercalate
      "; "
      [ "payment " <> irProviderRef <> " of " <> tshow amount <> " cents, invoice " <> textEncode irStatus <> maybe ", payment time not recorded" ((", paid at " <>) . tshow) nrSettledAt,
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
