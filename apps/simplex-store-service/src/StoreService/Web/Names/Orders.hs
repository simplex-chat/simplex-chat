{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Web.Names.Orders
  ( Answer (..),
    answerNow,
    refusalAnswer,
    sendAnswer,
    refusalResponse,
    refused,
    nameInvoiceHandler,
    codeOrderHandler,
    purchaseChecked,
    announceHeld,
    nameInvoiceView,
    orderRefusal,
    paymentMethod,
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Logger.Simple (logError, logNote, logWarn)
import Control.Monad (forM_, unless)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString.Char8 as B
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Word (Word32)
import Network.HTTP.Types (status200, status400, status403, status404, status409, status429, status500, status503, status504)
import Network.Wai (Request, Response, ResponseReceived, responseLBS)
import Simplex.Chat.Badges.Code (parseNameCode)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String (strEncode, textParseJSON)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.Util (isAsyncCancellation, safeDecodeUtf8, tshow)
import StoreService.Catalog (catalogCurrency)
import StoreService.Config (NamesConfig (..), ServiceConfig)
import StoreService.Names.Codes (CodeRefusal (..), readPrepaidCode)
import StoreService.Names.Community (termsLink)
import StoreService.Names.Orders
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Reveal (RevealRequest, orderReveal)
import StoreService.Names.Signatures (OrderFields (..), orderMessage)
import StoreService.Names.Store (BrokenInvariant (..), NameRegistration (..), NameReveal, NewRegistration (..), OrderHash, OrderPayment (..), OrderTerms (..), PaidOrder (..), PrepaidCode (..), createPaidRegistration, getRegistration, newRegistrationId, orderHash, paymentInvoice)
import StoreService.Names.Turns (withLabelTurn)
import StoreService.Names.View (NameRefusal (..), registrationView)
import StoreService.Orders (cancelNameInvoices, publishRegistration)
import StoreService.Providers (OrderDraft (..), Provider (..), ProviderInvoice (..), expiresItself, providerErrorText, providerNamed)
import StoreService.Store.Invoices (InvoiceRow (..), NewNameInvoice (..), getInvoice, insertNameInvoice, newInvoiceId)
import StoreService.Waiters (Waiters, publish)
import StoreService.Web.Http (CheckoutClient, HttpEnv, Respond, apiHeaders, confirmationsFor, invoiceAt, invoicePairs, invoiceWindow, jsonResponse, limitedCheckout, nameBodyBytes, providerFor, readBoundedBody)

data InvoiceOrder = InvoiceOrder
  { ioOrder :: SentOrder,
    ioMethod :: ServicePaymentMethod,
    ioPrice :: Word32,
    ioReveal :: RevealRequest
  }

data CodeOrder = CodeOrder {order :: SentOrder, code :: Text, reveal :: RevealRequest}

$(JQ.deriveFromJSON defaultJSON ''CodeOrder)

instance J.FromJSON InvoiceOrder where
  parseJSON = J.withObject "order" $ \o -> InvoiceOrder <$> o .: "order" <*> JT.explicitParseField paymentMethod o "method" <*> o .: "price" <*> o .: "reveal"

paymentMethod :: J.Value -> JT.Parser ServicePaymentMethod
paymentMethod = textParseJSON "payment method"

-- | An order's answer and the work its commit leaves, both done after the name's turn, so a slow client holds no turn.
data Answer = Answer {aResponse :: Either NameRefusal Response, aAfter :: IO ()}

answerNow :: Response -> Answer
answerNow r = Answer {aResponse = Right r, aAfter = pure ()}

refusalAnswer :: NameRefusal -> Answer
refusalAnswer r = Answer {aResponse = Left r, aAfter = pure ()}

sendAnswer :: Respond -> Answer -> IO ResponseReceived
sendAnswer respond Answer {aResponse, aAfter} = aAfter >> respond (either refusalResponse id aResponse)

refusalResponse :: NameRefusal -> Response
refusalResponse r = responseLBS status (retryAfterHeader <> apiHeaders) (J.encode r)
  where
    status = case r of
      NRBadRequest -> status400
      NRInvalidName -> status400
      NRInvalidLink {} -> status400
      NRNotFound -> status404
      NRInternal -> status500
      NROrderExists -> status409
      NRNameTaken -> status409
      NRNameCheckUnavailable -> status503
      NRCatalogChanged {} -> status409
      NRTermsChanged {} -> status409
      NRCodesOnly -> status403
      NRCodeInvalid -> status400
      NRCodeUsed -> status409
      NRCodeReserved -> status409
      NRCodeExpired -> status409
      NRCodeNotCovering -> status409
      NRPaymentPending -> status409
      NRRegistrationChanged {} -> status409
      NRStaleNonce -> status409
      NRRecordReverted -> status409
      NREditLimit {} -> status409
      NRRateLimited {} -> status429
      NRProviderUnavailable -> status503
      NRTermsUnavailable {} -> status503
      NREditPending -> status504
    retryAfterHeader = case r of
      NRRateLimited {retryAfter} -> [("Retry-After", B.pack $ show retryAfter)]
      NRTermsUnavailable {wait = Just seconds} -> [("Retry-After", B.pack $ show seconds)]
      _ -> []

-- | POST /api/invoice for a name, its body already read and its kind checked.
nameInvoiceHandler :: OrdersEnv -> HttpEnv -> CheckoutClient -> ServiceConfig -> [Provider] -> Waiters -> J.Object -> Respond -> IO ResponseReceived
nameInvoiceHandler env http client cfg providers waiters body respond
  | nCodesOnly (oeConfig env) = sendAnswer respond =<< refuse ORCodesOnly
  | otherwise = sendAnswer respond =<< case parseBody "order" (J.Object body) >>= invoiceOrder of
    Left refusal -> refuse refusal
    Right (req, method, price, reveal) ->
      answerOrder env what req (OTInvoice method price) (resentOrder env cfg) (newOrder req method price reveal)
  where
    what = "POST /api/invoice (name)"
    refuse = refused what
    invoiceOrder InvoiceOrder {ioOrder, ioMethod, ioPrice, ioReveal} = (\req -> (req, ioMethod, fromIntegral ioPrice, ioReveal)) <$> orderRequest ioOrder
    newOrder req@OrderRequest {orFields} method price reveal hash now = do
      let insert new paid db at =
            createPaidRegistration db new paid at >>= \case
              True -> pure $ Right ()
              False -> pure $ Left OROrderExists
      purchaseChecked env now req (orderYears orFields $ nYears $ oeConfig env) (Just price) Nothing reveal >>= \case
        Left refusal -> refuse refusal
        Right rv -> createAtProvider env http client cfg providers waiters what now InvoicedOrder {ivRequest = req, ivReveal = rv, ivOrderHash = hash, ivMethod = method, ivPrice = price, ivInsert = insert}

-- | An order paid by invoice: its order hash, the order's price, which its invoice asks, and its insert after the invoice row's.
data InvoicedOrder = InvoicedOrder
  { ivRequest :: OrderRequest,
    ivReveal :: NameReveal,
    ivOrderHash :: OrderHash,
    ivMethod :: ServicePaymentMethod,
    ivPrice :: USDCents,
    ivInsert :: NewRegistration -> PaidOrder -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
  }

-- | The invoice is made at the provider first, then the order is written with its invoice row; an order not written has its invoice cancelled there.
createAtProvider :: OrdersEnv -> HttpEnv -> CheckoutClient -> ServiceConfig -> [Provider] -> Waiters -> Text -> UTCTime -> InvoicedOrder -> IO Answer
createAtProvider env@OrdersEnv {oeStore, oeConfig} http client cfg providers waiters what now InvoicedOrder {ivRequest = req@OrderRequest {orOwner, orFields}, ivReveal, ivOrderHash, ivMethod, ivPrice, ivInsert} =
  case providerFor providers ivMethod of
    Nothing -> do
      logNote (what <> " refused: no provider configured for " <> tshow ivMethod)
      pure $ refusalAnswer NRProviderUnavailable
    Just provider -> limitedCheckout http client ivMethod (pure . refusalAnswer . NRRateLimited) $ do
      invId <- newInvoiceId
      let amount = CurrencyAmount $ fromIntegral ivPrice
      pCreateInvoice provider ivMethod OrderDraft {odAmount = amount, odCurrency = catalogCurrency, odReference = "name:" <> safeDecodeUtf8 (strEncode ivOrderHash)} >>= \case
        Left e -> do
          logError (what <> " failed at the provider: " <> providerErrorText e)
          pure $ refusalAnswer NRProviderUnavailable
        Right ProviderInvoice {piProviderRef, piDestination} -> do
          rid <- newRegistrationId
          let invoice = NewNameInvoice {nniInvoiceId = invId, nniProvider = pProvider provider, nniProviderRef = piProviderRef, nniAmount = amount, nniCurrency = catalogCurrency, nniDestination = piDestination, nniExpiresAt = addUTCTime (invoiceWindow cfg ivMethod) now, nniCreatedAt = now}
              registration = NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = orFields, nrgReveal = ivReveal, nrgYears = nYears oeConfig, nrgOrderHash = ivOrderHash}
              paid = PaidOrder {poMethod = ivMethod, poPrice = ivPrice, poInvoiceId = invId}
          placeOrder env now req (\db at -> insertNameInvoice db invoice >> ivInsert registration paid db at)
            `E.catch` strandedBy provider piProviderRef
            >>= \case
            Left refusal -> do
              logWarn (what <> ": the order was refused, so invoice " <> piProviderRef <> " is cancelled at the provider")
              (\a -> a {aAfter = cancelAtProvider provider piProviderRef}) <$> refused what refusal
            Right closed -> do
              row_ <- getInvoice oeStore invId
              registration_ <- withTransaction oeStore (`getRegistration` rid)
              response <- case (row_, registration_) of
                (Just row, Just r) -> Right <$> invoiceViewResponse env cfg r row
                _ -> Left NRInternal <$ logError (what <> ": the order of invoice " <> piProviderRef <> " is not readable after its commit")
              pure Answer {aResponse = response, aAfter = announceClosed env providers waiters closed}
  where
    -- An invoice created at the provider for an order that was not written would stay payable with nothing watching it.
    strandedBy :: Provider -> Text -> E.SomeException -> IO a
    strandedBy provider ref e = do
      unless (isAsyncCancellation e) $ do
        logError (what <> ": the order was not written, so invoice " <> ref <> " is cancelled at the provider: " <> tshow e)
        cancelAtProvider provider ref
      E.throwIO e

-- | An exact resend, answered with its order as it reads now: the invoice view, or the id of an order without an invoice.
resentOrder :: OrdersEnv -> ServiceConfig -> UTCTime -> NameRegistration -> IO Answer
resentOrder env@OrdersEnv {oeStore} cfg now r@NameRegistration {nrId = RegistrationId ridText, nrPayment} = case nrPayment of
  OPInvoice _ invId -> invoiceAnswer invId
  -- the hash of an invoice order never matches a code order's
  OPCode _ -> E.throwIO $ BrokenInvariant $ "an invoice order's hash matched code order " <> ridText
  where
    invoiceAnswer invId = getInvoice oeStore invId >>= maybe (E.throwIO $ BrokenInvariant $ "the invoice of order " <> ridText <> " is missing") (fmap answerNow . invoiceViewResponse env cfg r . invoiceAt now)

invoiceViewResponse :: OrdersEnv -> ServiceConfig -> NameRegistration -> InvoiceRow -> IO Response
invoiceViewResponse env cfg r row = jsonResponse status200 <$> nameInvoiceView env (confirmationsFor cfg) row r

registrationAnswer :: RegistrationId -> Response
registrationAnswer rid = jsonResponse status200 $ J.object ["registrationId" .= rid]

refused :: Text -> OrderRefusal -> IO Answer
refused what refusal = refusalAnswer (orderRefusal refusal) <$ logNote (what <> " refused: " <> tshow refusal)

notObject :: Text
notObject = "a body that is not a JSON object within its size limit"

answerOrder :: OrdersEnv -> Text -> OrderRequest -> OrderTerms -> (UTCTime -> NameRegistration -> IO Answer) -> (OrderHash -> UTCTime -> IO Answer) -> IO Answer
answerOrder OrdersEnv {oeStore, oeTurns, oeNow} what order@OrderRequest {orOwner, orFields, orTimestamp, orSignature} terms resent new =
  checkSigner orOwner message orSignature >>= \case
    Left refusal -> refused what refusal
    Right () -> withLabelTurn oeTurns (ofLabelHash orFields) $ do
      now <- oeNow
      withTransaction oeStore (\db -> existingOrder db order hash now) >>= \case
        EOResent r -> resent now r
        EOMeets -> refused what OROrderExists
        EONew -> new hash now
  where
    hash = orderHash orOwner orFields terms
    message = orderMessage orFields orTimestamp

-- | POST /api/name/register, an order paid with a prepaid code, in the contract's order of checks.
codeOrderHandler :: OrdersEnv -> [Provider] -> Waiters -> Request -> Respond -> IO ResponseReceived
codeOrderHandler env@OrdersEnv {oeStore} providers waiters req respond =
  readBoundedBody nameBodyBytes req >>= ordered >>= sendAnswer respond
  where
    what = "POST /api/name/register"
    ordered = \case
      Just body | Just v <- J.decode body -> either refuse withCode (parseBody "code order" v >>= \CodeOrder {order, code, reveal} -> (,code,reveal) <$> orderRequest order)
      _ -> refuse $ ORBadRequest notObject
    refuse = refused what
    withCode (order, codeText, reveal) =
      issuedCode codeText >>= \case
        Nothing -> refuse $ ORCode CRInvalid
        Just code@PrepaidCode {pcCodeHash} ->
          answerOrder env what order (OTCode pcCodeHash) (\_ NameRegistration {nrId} -> pure $ answerNow $ registrationAnswer nrId) (\hash now -> newOrder now order reveal hash code)
    issuedCode = maybe (pure Nothing) (readPrepaidCode oeStore) . parseNameCode
    newOrder now order@OrderRequest {orOwner, orFields} reveal hash code@PrepaidCode {pcYears} =
      purchaseChecked env now order (orderYears orFields pcYears) Nothing Nothing reveal >>= \case
        Left refusal -> refuse refusal
        Right rv -> do
          rid <- newRegistrationId
          placeCodeOrder env order rid code (codeOrderInsert NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = orFields, nrgReveal = rv, nrgYears = pcYears, nrgOrderHash = hash}) >>= \case
            Left refusal -> refuse refusal
            Right closed -> pure Answer {aResponse = Right $ registrationAnswer rid, aAfter = announceHeld env providers waiters closed rid}

purchaseChecked :: OrdersEnv -> UTCTime -> OrderRequest -> Int -> Maybe USDCents -> Maybe NameReveal -> RevealRequest -> IO (Either OrderRefusal NameReveal)
purchaseChecked OrdersEnv {oeStore, oeConfig, oeContracts} now OrderRequest {orOwner, orFields = fields@OrderFields {ofLabelHash, ofLength, ofDuration}, orTimestamp} years price accepted reveal =
  runExceptT $ do
    mapM_ ExceptT $
      [ pure $ checkTimestamp now orTimestamp,
        pure $ checkDuration oeConfig now years ofDuration,
        checkTerms oeConfig oeContracts fields,
        checkNameFree oeContracts oeStore ofLabelHash
      ]
        <> map (checkPrice oeContracts ofLength years) (toList price)
    ExceptT $ orderReveal oeConfig oeContracts now orOwner fields accepted reveal

-- | After the commit, the closed orders' invoices and versions are published, and their invoices cancelled at the provider,
-- best effort, as the poller's sweep would cancel them.
announceClosed :: OrdersEnv -> [Provider] -> Waiters -> [ClosedOrder] -> IO ()
announceClosed OrdersEnv {oeStore, oePhases} providers waiters closed = do
  atomically $ forM_ closed $ \ClosedOrder {coRegistration = NameRegistration {nrId, nrPayment}, coVersion} -> do
    forM_ (paymentInvoice nrPayment) $ \invId -> publish waiters invId ISExpired
    publishRegistration (Just oePhases) (nrId, coVersion)
  forM_ closed $ \ClosedOrder {coRegistration = NameRegistration {nrPayment}} ->
    forM_ (paymentInvoice nrPayment) $ \invId ->
      getInvoice oeStore invId >>= mapM_ (\InvoiceRow {irProvider, irProviderRef} -> forM_ (providerNamed providers irProvider) $ \p -> unless (expiresItself irProvider) $ cancelAtProvider p irProviderRef)

announceHeld :: OrdersEnv -> [Provider] -> Waiters -> [ClosedOrder] -> RegistrationId -> IO ()
announceHeld env@OrdersEnv {oeStore, oePhases} providers waiters closed rid = do
  announceClosed env providers waiters closed
  cancelNameInvoices oeStore waiters (Just oePhases) providers rid

cancelAtProvider :: Provider -> Text -> IO ()
cancelAtProvider Provider {pCancelInvoice} ref =
  pCancelInvoice ref >>= \case
    Left e -> logWarn ("names: cancelling invoice " <> ref <> " at the provider failed: " <> providerErrorText e)
    Right () -> pure ()

nameInvoiceView :: OrdersEnv -> Maybe Int -> InvoiceRow -> NameRegistration -> IO J.Value
nameInvoiceView OrdersEnv {oeStore, oeConfig, oeContracts, oeNow} confirmations row@InvoiceRow {irInvoiceId = InvoiceId invId} r = do
  now <- oeNow
  view <- registrationView oeStore oeConfig oeContracts now r
  pure $ J.object $ ["invoiceId" .= invId, "kind" .= ("name" :: Text), "registration" .= view] <> invoicePairs confirmations row

orderRefusal :: OrderRefusal -> NameRefusal
orderRefusal = \case
  ORBadRequest _ -> NRBadRequest
  ORInvalidName -> NRInvalidName
  ORInvalidLink kind -> NRInvalidLink $ Just kind
  OROrderExists -> NROrderExists
  ORNameTaken -> NRNameTaken
  ORNameCheckUnavailable -> NRNameCheckUnavailable
  ORCatalogChanged price -> NRCatalogChanged price
  ORCode refusal -> case refusal of
    CRInvalid -> NRCodeInvalid
    CRUsed -> NRCodeUsed
    CRReserved -> NRCodeReserved
    CRExpired -> NRCodeExpired
    CRPaymentPending -> NRPaymentPending
    CRNotCovering -> NRCodeNotCovering
  ORTermsChanged terms community -> NRTermsChanged (termsLink terms) community
  ORTermsUnavailable wait -> NRTermsUnavailable wait
  ORCodesOnly -> NRCodesOnly
