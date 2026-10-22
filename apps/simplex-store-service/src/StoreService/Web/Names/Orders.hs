{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names.Orders (nameInvoiceHandler, nameInvoiceView, orderRefusalResponse, registrationChangedResponse) where

import Control.Concurrent.STM (atomically)
import Control.Monad (forM_, unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Data.Time.Clock (addUTCTime)
import Data.Word (Word32)
import Network.HTTP.Types (status200, status409, status503)
import Network.Wai (Response, ResponseReceived)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), ServicePaymentMethod)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Util (tshow)
import StoreService.Catalog (catalogCurrency)
import StoreService.Config (NamesConfig (..), ServiceConfig)
import StoreService.Log (logError, logInfo, logWarn)
import StoreService.Names.Orders
import StoreService.Names.Phases (RegistrationId)
import StoreService.Names.Signatures (OrderFields (..))
import StoreService.Names.Store (NameRegistration (..), NewRegistration (..), OrderTerms (..), PaidOrder (..), RegistrationPayment (..), createPaidRegistration, getRegistration, newRegistrationId, orderHash, textMethod)
import StoreService.Names.Turns (withLabelTurn)
import StoreService.Names.View (registrationView)
import StoreService.Orders (publishRegistration)
import StoreService.Providers (OrderDraft (..), Provider (..), ProviderError (..), ProviderInvoice (..), expiresItself)
import StoreService.Store.Invoices (InvoiceRow (..), NewNameInvoice (..), getInvoice, insertNameInvoice, newInvoiceId, truncateToSecond)
import StoreService.Waiters (Waiters, publish)
import StoreService.Web.Http (Respond, badRequest, confirmationsFor, internalError, invalidName, invoicePairs, invoiceWindow, jsonError, jsonResponse, notFound, providerFor, providerNamed, providerUnavailable)

data InvoiceOrder = InvoiceOrder
  { ioRequest :: OrderRequest,
    ioMethod :: ServicePaymentMethod,
    ioPrice :: Word32
  }

parseInvoiceOrder :: J.Object -> Either OrderRefusal InvoiceOrder
parseInvoiceOrder o = do
  order@InvoiceOrder {ioRequest} <- maybe (Left ORBadRequest) Right $ do
    ioRequest <- parseOrderRequest o
    ioMethod <- case KM.lookup "method" o of
      Just (J.String t) -> textMethod t
      _ -> Nothing
    ioPrice <- integerField =<< KM.lookup "price" o
    pure InvoiceOrder {ioRequest, ioMethod, ioPrice}
  order <$ checkLength ioRequest

-- | POST /api/invoice for a name, its body already read and its kind checked.
nameInvoiceHandler :: OrdersEnv -> ServiceConfig -> [Provider] -> Waiters -> J.Object -> Respond -> IO ResponseReceived
nameInvoiceHandler env@OrdersEnv {oeStore, oeTurns, oeNow} cfg providers waiters body respond =
  case parseInvoiceOrder body of
    Left refusal -> refuse refusal
    Right order@InvoiceOrder {ioRequest = req@OrderRequest {orOwner, orFields}, ioMethod, ioPrice} ->
      checkOrderSigner req >>= \case
        Left refusal -> refuse refusal
        Right () -> withLabelTurn oeTurns (ofLabelHash orFields) $ do
          now <- oeNow
          let hash = orderHash orOwner orFields (OTInvoice ioMethod $ fromIntegral ioPrice)
          withTransaction oeStore (\db -> existingOrder db req hash now) >>= \case
            EOResent r -> answerResend r
            EOMeets -> refuse OROrderExists
            EONew _ -> newOrder order
  where
    refuse refusal = do
      logInfo ("POST /api/invoice (name) refused: " <> tshow refusal)
      respond $ orderRefusalResponse refusal
    answerResend r@NameRegistration {nrPayment} = case nrPayment of
      RPInvoice _ invId -> getInvoice oeStore invId >>= maybe (refuse OROrderExists) (\row -> respondView row r)
      RPCode _ -> refuse OROrderExists
    respondView row r = respond . jsonResponse status200 =<< nameInvoiceView env (confirmationsFor cfg) row r
    newOrder order@InvoiceOrder {ioRequest = OrderRequest {orFields = OrderFields {ofLabelHash, ofLength, ofDuration}, orTimestamp}, ioMethod, ioPrice} = do
      now <- oeNow
      let years = nYears $ oeConfig env
          checks =
            [ pure $ checkTimestamp now orTimestamp,
              pure $ checkDuration (oeConfig env) now years ofDuration,
              checkNameFree (oeContracts env) oeStore ofLabelHash,
              checkPrice (oeContracts env) ofLength years (fromIntegral ioPrice)
            ]
      firstRefusal checks >>= \case
        Just refusal -> refuse refusal
        Nothing -> case providerFor providers ioMethod of
          Nothing -> do
            logInfo ("POST /api/invoice (name) refused: no provider configured for " <> tshow ioMethod)
            respond providerUnavailable
          Just provider -> createAtProvider order provider
    createAtProvider InvoiceOrder {ioRequest = req@OrderRequest {orOwner, orFields}, ioMethod, ioPrice} provider = do
      invId <- newInvoiceId
      now <- truncateToSecond <$> oeNow
      let amount = CurrencyAmount ioPrice
      pCreateInvoice provider ioMethod OrderDraft {odAmount = amount, odCurrency = catalogCurrency} >>= \case
        Left (ProviderError e) -> do
          logError ("POST /api/invoice (name) failed: provider refused to create an invoice: " <> e)
          respond providerUnavailable
        Right ProviderInvoice {piProviderRef, piDestination} -> do
          rid <- newRegistrationId
          let invoice = NewNameInvoice {nniInvoiceId = invId, nniProvider = pProvider provider, nniAmount = amount, nniCurrency = catalogCurrency, nniDestination = piDestination, nniExpiresAt = addUTCTime (invoiceWindow cfg ioMethod) now, nniCreatedAt = now}
              registration = NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = orFields, nrgYears = nYears $ oeConfig env}
              paid = PaidOrder {poMethod = ioMethod, poPrice = fromIntegral ioPrice, poInvoiceId = invId, poProviderRef = piProviderRef}
          placeOrder env req (\db at -> insertNameInvoice db invoice >> createPaidRegistration db registration paid at) >>= \case
            Left refusal -> do
              logWarn ("POST /api/invoice (name): the order lost to another, so invoice " <> piProviderRef <> " is cancelled at the provider")
              cancelAtProvider provider piProviderRef
              refuse refusal
            Right closed -> do
              closedOrders closed
              row_ <- getInvoice oeStore invId
              registration_ <- withTransaction oeStore (`getRegistration` rid)
              case (row_, registration_) of
                (Just row, Just r) -> respondView row r
                _ -> logError ("POST /api/invoice (name): order " <> invText invId <> " is not readable after its commit") >> respond internalError
    cancelAtProvider Provider {pCancelInvoice} ref =
      pCancelInvoice ref >>= \case
        Left (ProviderError e) -> logWarn ("names: cancelling invoice " <> ref <> " at the provider failed: " <> e)
        Right () -> pure ()
    -- The provider's cancel follows the commit, best effort, as the poller's sweep would send it.
    closedOrders closed = do
      atomically $ forM_ closed $ \ClosedOrder {coRegistration = NameRegistration {nrId, nrPayment}, coVersion} -> do
        forM_ (paidInvoice nrPayment) $ \invId -> publish waiters invId ISExpired
        publishRegistration (Just $ oePhases env) (nrId, coVersion)
      forM_ closed $ \ClosedOrder {coRegistration = NameRegistration {nrPayment}} ->
        forM_ (paidInvoice nrPayment) $ \invId ->
          getInvoice oeStore invId >>= mapM_ (\InvoiceRow {irProvider, irProviderRef} -> forM_ (providerNamed providers irProvider) $ \p -> unless (expiresItself irProvider) $ cancelAtProvider p irProviderRef)
    paidInvoice = \case
      RPInvoice _ invId -> Just invId
      RPCode _ -> Nothing

invText :: InvoiceId -> Text
invText (InvoiceId t) = t

firstRefusal :: [IO (Either OrderRefusal ())] -> IO (Maybe OrderRefusal)
firstRefusal = \case
  [] -> pure Nothing
  check : rest -> check >>= either (pure . Just) (const $ firstRefusal rest)

nameInvoiceView :: OrdersEnv -> Maybe Int -> InvoiceRow -> NameRegistration -> IO J.Value
nameInvoiceView OrdersEnv {oeStore, oeConfig, oeContracts} confirmations row@InvoiceRow {irInvoiceId = InvoiceId invId} r = do
  view <- registrationView oeStore oeConfig oeContracts r
  pure $ J.object $ ["invoiceId" .= invId, "kind" .= ("name" :: Text), "registration" .= view] <> invoicePairs confirmations row

-- | The registration's current view, for a request that lost the race to change it.
registrationChangedResponse :: OrdersEnv -> RegistrationId -> IO Response
registrationChangedResponse OrdersEnv {oeStore, oeConfig, oeContracts} rid =
  withTransaction oeStore (`getRegistration` rid) >>= \case
    Nothing -> pure notFound
    Just r -> do
      view <- registrationView oeStore oeConfig oeContracts r
      pure $ jsonResponse status409 $ J.object ["error" .= ("registration_changed" :: Text), "registration" .= view]

orderRefusalResponse :: OrderRefusal -> Response
orderRefusalResponse = \case
  ORBadRequest -> badRequest
  ORInvalidName -> invalidName
  OROrderExists -> jsonError status409 "order_exists"
  ORNameTaken -> jsonError status409 "name_taken"
  ORNameCheckUnavailable -> jsonError status503 "name_check_unavailable"
  ORCatalogChanged price -> jsonResponse status409 $ J.object ["error" .= ("catalog_changed" :: Text), "price" .= price]
