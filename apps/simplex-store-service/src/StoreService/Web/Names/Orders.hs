{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names.Orders
  ( Answer (..),
    answerNow,
    sendAnswer,
    firstRefusal,
    nameInvoiceHandler,
    codeOrderHandler,
    nameInvoiceView,
    orderRefusalResponse,
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad (forM_, unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Clock (addUTCTime)
import Data.Word (Word32)
import Network.HTTP.Types (status200, status403, status409, status503)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Chat.Badges.Code (NameCode (..), parseNameCode)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), ServicePaymentMethod)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Util (tshow)
import StoreService.Catalog (catalogCurrency)
import StoreService.Config (NamesConfig (..), ServiceConfig)
import StoreService.Log (logError, logInfo, logWarn)
import StoreService.Names.Abi (hex)
import StoreService.Names.Codes (CodeRefusal (..), readPrepaidCode)
import StoreService.Names.Orders
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Signatures (OrderFields (..))
import StoreService.Names.Store (NameRegistration (..), NewRegistration (..), OrderTerms (..), PaidOrder (..), PrepaidCode (..), RegistrationPayment (..), createPaidRegistration, getRegistration, newRegistrationId, orderHash, textMethod)
import StoreService.Names.Turns (withLabelTurn)
import StoreService.Names.View (registrationAt, registrationView)
import StoreService.Orders (publishRegistration)
import StoreService.Providers (OrderDraft (..), Provider (..), ProviderError (..), ProviderInvoice (..), expiresItself)
import StoreService.Store.Invoices (InvoiceRow (..), NewNameInvoice (..), getInvoice, insertNameInvoice, newInvoiceId, truncateToSecond)
import StoreService.Waiters (Waiters, publish)
import StoreService.Web.Http (Respond, badRequest, confirmationsFor, internalError, invalidName, invoiceAt, invoicePairs, invoiceWindow, jsonError, jsonResponse, nameBodyBytes, providerFor, providerNamed, providerUnavailable, readBoundedBody)
import StoreService.Web.Names.Codes (codeRefusalResponse)

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
  order <$ checkLength (orFields ioRequest)

-- | An order's answer and the work its commit leaves, both done after the name's turn, so a slow client holds no turn.
data Answer = Answer {aResponse :: Response, aAfter :: IO ()}

answerNow :: Response -> Answer
answerNow aResponse = Answer {aResponse, aAfter = pure ()}

sendAnswer :: Respond -> Answer -> IO ResponseReceived
sendAnswer respond Answer {aResponse, aAfter} = aAfter >> respond aResponse

-- | POST /api/invoice for a name, its body already read and its kind checked.
nameInvoiceHandler :: OrdersEnv -> ServiceConfig -> [Provider] -> Waiters -> J.Object -> Respond -> IO ResponseReceived
nameInvoiceHandler env@OrdersEnv {oeStore, oeTurns, oeNow} cfg providers waiters body respond
  | nCodesOnly (oeConfig env) = sendAnswer respond =<< refuse ORCodesOnly
  | otherwise = sendAnswer respond =<< case parseInvoiceOrder body of
    Left refusal -> refuse refusal
    Right order@InvoiceOrder {ioRequest = req@OrderRequest {orOwner, orFields}, ioMethod, ioPrice} ->
      checkOrderSigner req >>= \case
        Left refusal -> refuse refusal
        Right () -> withLabelTurn oeTurns (ofLabelHash orFields) $ do
          now <- oeNow
          let hash = orderHash orOwner orFields (OTInvoice ioMethod $ fromIntegral ioPrice)
          withTransaction oeStore (\db -> existingOrder db req hash now) >>= \case
            EOResent r -> answerResend now r
            EOMeets -> refuse OROrderExists
            EONew _ -> newOrder order
  where
    refuse refusal = do
      logInfo ("POST /api/invoice (name) refused: " <> tshow refusal)
      pure $ answerNow $ orderRefusalResponse refusal
    answerResend now r@NameRegistration {nrPayment} = case nrPayment of
      RPInvoice _ invId -> getInvoice oeStore invId >>= maybe (refuse OROrderExists) (fmap answerNow . viewOf (registrationAt now r) . invoiceAt now)
      RPCode _ -> refuse OROrderExists
    viewOf r row = jsonResponse status200 <$> nameInvoiceView env (confirmationsFor cfg) row r
    newOrder order@InvoiceOrder {ioRequest = OrderRequest {orFields = fields@OrderFields {ofLabelHash, ofLength, ofDuration}, orTimestamp}, ioMethod, ioPrice} = do
      now <- oeNow
      let years = nYears $ oeConfig env
          checks =
            [ pure $ checkTimestamp now orTimestamp,
              pure $ checkDuration (oeConfig env) now years ofDuration,
              pure $ checkTerms (oeConfig env) fields,
              checkNameFree (oeContracts env) oeStore ofLabelHash,
              checkPrice (oeContracts env) ofLength years (fromIntegral ioPrice)
            ]
      firstRefusal checks >>= \case
        Just refusal -> refuse refusal
        Nothing -> case providerFor providers ioMethod of
          Nothing -> do
            logInfo ("POST /api/invoice (name) refused: no provider configured for " <> tshow ioMethod)
            pure $ answerNow providerUnavailable
          Just provider -> createAtProvider order provider
    createAtProvider InvoiceOrder {ioRequest = req@OrderRequest {orOwner, orFields}, ioMethod, ioPrice} provider = do
      invId <- newInvoiceId
      now <- truncateToSecond <$> oeNow
      let amount = CurrencyAmount ioPrice
      pCreateInvoice provider ioMethod OrderDraft {odAmount = amount, odCurrency = catalogCurrency, odReference = "name:" <> hex (orderHash orOwner orFields (OTInvoice ioMethod $ fromIntegral ioPrice))} >>= \case
        Left (ProviderError e) -> do
          logError ("POST /api/invoice (name) failed: provider refused to create an invoice: " <> e)
          pure $ answerNow providerUnavailable
        Right ProviderInvoice {piProviderRef, piDestination} -> do
          rid <- newRegistrationId
          let invoice = NewNameInvoice {nniInvoiceId = invId, nniProvider = pProvider provider, nniAmount = amount, nniCurrency = catalogCurrency, nniDestination = piDestination, nniExpiresAt = addUTCTime (invoiceWindow cfg ioMethod) now, nniCreatedAt = now}
              registration = NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = orFields, nrgYears = nYears $ oeConfig env}
              paid = PaidOrder {poMethod = ioMethod, poPrice = fromIntegral ioPrice, poInvoiceId = invId, poProviderRef = piProviderRef}
          placeOrder env req (\db at -> insertNameInvoice db invoice >> first (const OROrderExists) <$> createPaidRegistration db registration paid at)
            `E.catch` strandedBy provider piProviderRef
            >>= \case
            Left refusal -> do
              logWarn ("POST /api/invoice (name): the order lost to another, so invoice " <> piProviderRef <> " is cancelled at the provider")
              (\a -> a {aAfter = cancelAtProvider provider piProviderRef}) <$> refuse refusal
            Right closed -> do
              row_ <- getInvoice oeStore invId
              registration_ <- withTransaction oeStore (`getRegistration` rid)
              response <- case (row_, registration_) of
                (Just row, Just r) -> viewOf r row
                _ -> internalError <$ logError ("POST /api/invoice (name): order " <> invText invId <> " is not readable after its commit")
              pure Answer {aResponse = response, aAfter = announceClosed env providers waiters closed}
    -- An invoice created at the provider for an order that was not written would stay payable with nothing watching it.
    strandedBy :: Provider -> Text -> E.SomeException -> IO a
    strandedBy provider ref e = do
      unless (isAsync e) $ do
        logError ("POST /api/invoice (name): the order was not written, so invoice " <> ref <> " is cancelled at the provider: " <> tshow e)
        cancelAtProvider provider ref
      E.throwIO e
    isAsync e = isJust (E.fromException e :: Maybe E.SomeAsyncException)

-- | POST /api/name/register, an order paid with a prepaid code, in the contract's order of checks.
codeOrderHandler :: OrdersEnv -> [Provider] -> Waiters -> Request -> Respond -> IO ResponseReceived
codeOrderHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oeTurns, oeNow} providers waiters req respond =
  readBoundedBody nameBodyBytes req >>= ordered >>= sendAnswer respond
  where
    ordered = \case
      Just body | Just (J.Object o) <- J.decode body -> either refuse withCode (parseCodeOrder o)
      _ -> refuse ORBadRequest
    refuse refusal = do
      logInfo ("POST /api/name/register refused: " <> tshow refusal)
      pure $ answerNow $ orderRefusalResponse refusal
    withCode (order, codeText) =
      issuedCode codeText >>= \case
        Nothing -> refuse $ ORCode CRInvalid
        Just code -> checkOrderSigner order >>= either refuse (const $ signed order code)
    issuedCode t = case parseNameCode t of
      Just (NCPrepaid code) -> readPrepaidCode oeStore code
      _ -> pure Nothing
    signed order@OrderRequest {orOwner, orFields} code@PrepaidCode {pcCodeHash} = withLabelTurn oeTurns (ofLabelHash orFields) $ do
      now <- oeNow
      withTransaction oeStore (\db -> existingOrder db order (orderHash orOwner orFields (OTCode pcCodeHash)) now) >>= \case
        EOResent NameRegistration {nrId} -> pure $ answerNow $ registrationAnswer nrId
        EOMeets -> refuse OROrderExists
        EONew _ -> newOrder now order code
    newOrder now order@OrderRequest {orFields = fields@OrderFields {ofLabelHash, ofDuration}, orTimestamp} code@PrepaidCode {pcYears} = do
      let checks =
            [ pure $ checkTimestamp now orTimestamp,
              pure $ checkDuration oeConfig now pcYears ofDuration,
              pure $ checkTerms oeConfig fields,
              checkNameFree oeContracts oeStore ofLabelHash
            ]
      firstRefusal checks >>= \case
        Just refusal -> refuse refusal
        Nothing -> do
          rid <- newRegistrationId
          placeCodeOrder env order rid code >>= \case
            Left refusal -> refuse refusal
            Right closed -> pure Answer {aResponse = registrationAnswer rid, aAfter = announceClosed env providers waiters closed}
    registrationAnswer (RegistrationId rid) = jsonResponse status200 $ J.object ["registrationId" .= rid]

parseCodeOrder :: J.Object -> Either OrderRefusal (OrderRequest, Text)
parseCodeOrder o = do
  order@(req, _) <- maybe (Left ORBadRequest) Right $ (,) <$> parseOrderRequest o <*> textField o "code"
  order <$ checkLength (orFields req)

-- | After the commit, the closed orders' invoices and versions are published, and their invoices cancelled at the provider,
-- best effort, as the poller's sweep would cancel them.
announceClosed :: OrdersEnv -> [Provider] -> Waiters -> [ClosedOrder] -> IO ()
announceClosed OrdersEnv {oeStore, oePhases} providers waiters closed = do
  atomically $ forM_ closed $ \ClosedOrder {coRegistration = NameRegistration {nrId, nrPayment}, coVersion} -> do
    forM_ (paidInvoice nrPayment) $ \invId -> publish waiters invId ISExpired
    publishRegistration (Just oePhases) (nrId, coVersion)
  forM_ closed $ \ClosedOrder {coRegistration = NameRegistration {nrPayment}} ->
    forM_ (paidInvoice nrPayment) $ \invId ->
      getInvoice oeStore invId >>= mapM_ (\InvoiceRow {irProvider, irProviderRef} -> forM_ (providerNamed providers irProvider) $ \p -> unless (expiresItself irProvider) $ cancelAtProvider p irProviderRef)
  where
    paidInvoice = \case
      RPInvoice _ invId -> Just invId
      RPCode _ -> Nothing

cancelAtProvider :: Provider -> Text -> IO ()
cancelAtProvider Provider {pCancelInvoice} ref =
  pCancelInvoice ref >>= \case
    Left (ProviderError e) -> logWarn ("names: cancelling invoice " <> ref <> " at the provider failed: " <> e)
    Right () -> pure ()

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

orderRefusalResponse :: OrderRefusal -> Response
orderRefusalResponse = \case
  ORBadRequest -> badRequest
  ORInvalidName -> invalidName
  OROrderExists -> jsonError status409 "order_exists"
  ORNameTaken -> jsonError status409 "name_taken"
  ORNameCheckUnavailable -> jsonError status503 "name_check_unavailable"
  ORCatalogChanged price -> jsonResponse status409 $ J.object ["error" .= ("catalog_changed" :: Text), "price" .= price]
  ORCode refusal -> codeRefusalResponse refusal
  ORTermsChanged terms community -> jsonResponse status409 $ J.object ["error" .= ("terms_changed" :: Text), "terms" .= terms, "community" .= community]
  ORCodesOnly -> jsonError status403 "codes_only"
