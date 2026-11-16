{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Names.Orders
  ( Answer (..),
    answerNow,
    sendAnswer,
    refused,
    nameInvoiceHandler,
    codeOrderHandler,
    spendHandler,
    nameInvoiceView,
    orderRefusalResponse,
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Logger.Simple (logError, logNote, logWarn)
import Control.Monad (forM_, unless)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JT
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Word (Word32)
import Network.HTTP.Types (status200, status403, status409, status503)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Chat.Badges.Code (parseNameCode)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String (textDecode)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Util (tshow)
import StoreService.Catalog (catalogCurrency)
import StoreService.Config (NamesConfig (..), ServiceConfig)
import StoreService.Names.Abi (hex)
import StoreService.Names.Balances (Spend (..), checkSpendBalance, coveredOrderInsert, differenceOrderInsert)
import StoreService.Names.Codes (CodeRefusal (..), readPrepaidCode)
import StoreService.Names.Orders
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Signatures (OrderFields (..), spendMessage)
import StoreService.Names.Store (BalanceInsertRefused (..), BrokenInvariant (..), NameRegistration (..), NewRegistration (..), OrderHash, OrderPayment (..), OrderTerms (..), PaidOrder (..), PrepaidCode (..), createPaidRegistration, getRegistration, newRegistrationId, orderHash, paymentInvoice)
import StoreService.Names.Turns (withBalanceTurn, withLabelTurn)
import StoreService.Names.View (balanceJSON, registrationAt, registrationView)
import StoreService.Orders (publishRegistration)
import StoreService.Providers (OrderDraft (..), Provider (..), ProviderInvoice (..), expiresItself, providerErrorText)
import StoreService.Store.Invoices (InvoiceRow (..), NewNameInvoice (..), getInvoice, insertNameInvoice, newInvoiceId, truncateToSecond)
import StoreService.Waiters (Waiters, publish)
import StoreService.Web.Http (CheckoutClient, HttpEnv, Respond, badRequest, checkoutClient, confirmationsFor, internalError, invalidName, invoiceAt, invoicePairs, invoiceWindow, jsonError, jsonResponse, limitedCheckout, nameBodyBytes, notFound, providerFor, providerNamed, providerUnavailable, readBoundedBody, termsUnavailable)
import StoreService.Web.Names.Codes (codeRefusalResponse)

data InvoiceOrder = InvoiceOrder
  { ioRequest :: OrderRequest,
    ioMethod :: ServicePaymentMethod,
    ioPrice :: USDCents
  }

parseInvoiceOrder :: J.Object -> Either OrderRefusal InvoiceOrder
parseInvoiceOrder o = do
  order@InvoiceOrder {ioRequest} <- parseBody "order" $ do
    ioRequest <- parseOrderRequest o
    ioMethod <- jsonField o "method" paymentMethod
    ioPrice <- jsonField o "price" invoiceCents
    pure InvoiceOrder {ioRequest, ioMethod, ioPrice}
  order <$ checkLength (orFields ioRequest)

paymentMethod :: J.Value -> JT.Parser ServicePaymentMethod
paymentMethod = textWith $ maybe (Left "not a payment method") Right . textDecode

invoiceCents :: J.Value -> JT.Parser USDCents
invoiceCents v = fromIntegral <$> (boundedInteger v :: JT.Parser Word32)

-- | An order's answer and the work its commit leaves, both done after the name's turn, so a slow client holds no turn.
data Answer = Answer {aResponse :: Response, aAfter :: IO ()}

answerNow :: Response -> Answer
answerNow aResponse = Answer {aResponse, aAfter = pure ()}

sendAnswer :: Respond -> Answer -> IO ResponseReceived
sendAnswer respond Answer {aResponse, aAfter} = aAfter >> respond aResponse

-- | POST /api/invoice for a name, its body already read and its kind checked.
nameInvoiceHandler :: OrdersEnv -> HttpEnv -> CheckoutClient -> ServiceConfig -> [Provider] -> Waiters -> J.Object -> Respond -> IO ResponseReceived
nameInvoiceHandler env@OrdersEnv {oeStore, oeTurns} http client cfg providers waiters body respond
  | nCodesOnly (oeConfig env) = sendAnswer respond =<< refuse ORCodesOnly
  | otherwise = sendAnswer respond =<< case parseInvoiceOrder body of
    Left refusal -> refuse refusal
    Right order@InvoiceOrder {ioRequest = req@OrderRequest {orOwner, orFields}, ioMethod, ioPrice} ->
      checkOrderSigner req >>= \case
        Left refusal -> refuse refusal
        Right () -> do
          let hash = orderHash orOwner orFields (OTInvoice ioMethod ioPrice)
          withLabelTurn oeTurns (ofLabelHash orFields) $
            answerOrder env what req hash (resentOrder env cfg) (newOrder order hash)
  where
    what = "POST /api/invoice (name)"
    refuse = refused what
    newOrder InvoiceOrder {ioRequest = req@OrderRequest {orFields = fields@OrderFields {ofLabelHash, ofLength, ofDuration}, orTimestamp}, ioMethod, ioPrice} hash now = do
      let years = orderYears fields $ nYears $ oeConfig env
          checks =
            [ pure $ checkTimestamp now orTimestamp,
              pure $ checkDuration (oeConfig env) now years ofDuration,
              checkTerms (oeConfig env) (oeContracts env) fields,
              checkNameFree (oeContracts env) oeStore ofLabelHash,
              checkPrice (oeContracts env) ofLength years ioPrice
            ]
          insert new paid db at =
            createPaidRegistration db new Nothing paid at >>= \case
              Right () -> pure $ Right ()
              Left BIRLiveOrder -> pure $ Left OROrderExists
              Left BIRBalanceChanged -> E.throwIO $ BrokenInvariant "an order holding no balance was refused at a balance"
      runExceptT (mapM_ ExceptT checks) >>= \case
        Left refusal -> refuse refusal
        Right () -> createAtProvider env http client cfg providers waiters what InvoicedOrder {ivRequest = req, ivOrderHash = hash, ivHolders = [], ivMethod = ioMethod, ivPrice = ioPrice, ivAmount = ioPrice, ivInsert = insert}

-- | An order paid by invoice: its order hash, the holders its transaction closes, the order's price, what its invoice asks
-- (less than the price when a balance paid the rest), and its insert after the invoice row's.
data InvoicedOrder = InvoicedOrder
  { ivRequest :: OrderRequest,
    ivOrderHash :: OrderHash,
    ivHolders :: [NameRegistration],
    ivMethod :: ServicePaymentMethod,
    ivPrice :: USDCents,
    ivAmount :: USDCents,
    ivInsert :: NewRegistration -> PaidOrder -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
  }

-- | The invoice is made at the provider first, then the order is written with its invoice row; an order not written has its invoice cancelled there.
createAtProvider :: OrdersEnv -> HttpEnv -> CheckoutClient -> ServiceConfig -> [Provider] -> Waiters -> Text -> InvoicedOrder -> IO Answer
createAtProvider env@OrdersEnv {oeStore, oeConfig, oeNow} http client cfg providers waiters what InvoicedOrder {ivRequest = req@OrderRequest {orOwner, orFields}, ivOrderHash, ivHolders, ivMethod, ivPrice, ivAmount, ivInsert} =
  case providerFor providers ivMethod of
    Nothing -> do
      logNote (what <> " refused: no provider configured for " <> tshow ivMethod)
      pure $ answerNow providerUnavailable
    Just provider -> limitedCheckout http client ivMethod (pure . answerNow) $ do
      invId <- newInvoiceId
      now <- truncateToSecond <$> oeNow
      let amount = CurrencyAmount $ fromIntegral ivAmount
      pCreateInvoice provider ivMethod OrderDraft {odAmount = amount, odCurrency = catalogCurrency, odReference = "name:" <> hex ivOrderHash} >>= \case
        Left e -> do
          logError (what <> " failed at the provider: " <> providerErrorText e)
          pure $ answerNow providerUnavailable
        Right ProviderInvoice {piProviderRef, piDestination} -> do
          rid <- newRegistrationId
          let invoice = NewNameInvoice {nniInvoiceId = invId, nniProvider = pProvider provider, nniAmount = amount, nniCurrency = catalogCurrency, nniDestination = piDestination, nniExpiresAt = addUTCTime (invoiceWindow cfg ivMethod) now, nniCreatedAt = now}
              registration = NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = orFields, nrgYears = nYears oeConfig, nrgOrderHash = ivOrderHash}
              paid = PaidOrder {poMethod = ivMethod, poPrice = ivPrice, poInvoiceId = invId, poProviderRef = piProviderRef}
          placeOrder env req ivHolders (\db at -> insertNameInvoice db invoice >> ivInsert registration paid db at)
            `E.catch` strandedBy provider piProviderRef
            >>= \case
            Left refusal -> do
              logWarn (what <> ": the order was refused, so invoice " <> piProviderRef <> " is cancelled at the provider")
              (\a -> a {aAfter = cancelAtProvider provider piProviderRef}) <$> refused what refusal
            Right closed -> do
              row_ <- getInvoice oeStore invId
              registration_ <- withTransaction oeStore (`getRegistration` rid)
              response <- case (row_, registration_) of
                (Just row, Just r) -> invoiceViewResponse env cfg r row
                _ -> internalError <$ logError (what <> ": the order of invoice " <> piProviderRef <> " is not readable after its commit")
              pure Answer {aResponse = response, aAfter = announceClosed env providers waiters closed}
  where
    -- An invoice created at the provider for an order that was not written would stay payable with nothing watching it.
    strandedBy :: Provider -> Text -> E.SomeException -> IO a
    strandedBy provider ref e = do
      unless (isAsync e) $ do
        logError (what <> ": the order was not written, so invoice " <> ref <> " is cancelled at the provider: " <> tshow e)
        cancelAtProvider provider ref
      E.throwIO e
    isAsync e = isJust (E.fromException e :: Maybe E.SomeAsyncException)

-- | An exact resend, answered with its order as it reads now: the invoice view, or the id of an order without an invoice.
resentOrder :: OrdersEnv -> ServiceConfig -> UTCTime -> NameRegistration -> IO Answer
resentOrder env@OrdersEnv {oeStore} cfg now r@NameRegistration {nrId = rid@(RegistrationId ridText), nrPayment} = case nrPayment of
  OPInvoice _ invId -> invoiceAnswer invId
  OPDifference _ _ invId -> invoiceAnswer invId
  OPBalance _ -> pure $ answerNow $ registrationAnswer rid
  -- the hash of an invoice or spend order never matches a code order's
  OPCode _ -> E.throwIO $ BrokenInvariant $ "an invoice or spend order's hash matched code order " <> ridText
  where
    invoiceAnswer invId = getInvoice oeStore invId >>= maybe (E.throwIO $ BrokenInvariant $ "the invoice of order " <> ridText <> " is missing") (fmap answerNow . invoiceViewResponse env cfg (registrationAt now r) . invoiceAt now)

invoiceViewResponse :: OrdersEnv -> ServiceConfig -> NameRegistration -> InvoiceRow -> IO Response
invoiceViewResponse env cfg r row = jsonResponse status200 <$> nameInvoiceView env (confirmationsFor cfg) row r

registrationAnswer :: RegistrationId -> Response
registrationAnswer (RegistrationId rid) = jsonResponse status200 $ J.object ["registrationId" .= rid]

refused :: Text -> OrderRefusal -> IO Answer
refused what refusal = answerNow (orderRefusalResponse refusal) <$ logNote (what <> " refused: " <> tshow refusal)

notObject :: Text
notObject = "a body that is not a JSON object within its size limit"

answerOrder :: OrdersEnv -> Text -> OrderRequest -> OrderHash -> (UTCTime -> NameRegistration -> IO Answer) -> (UTCTime -> IO Answer) -> IO Answer
answerOrder OrdersEnv {oeStore, oeNow} what order hash resent new = do
  now <- oeNow
  withTransaction oeStore (\db -> existingOrder db order hash now) >>= \case
    EOResent r -> resent now r
    EOMeets -> refused what OROrderExists
    EONew -> new now

-- | POST /api/name/register, an order paid with a prepaid code, in the contract's order of checks.
codeOrderHandler :: OrdersEnv -> [Provider] -> Waiters -> Request -> Respond -> IO ResponseReceived
codeOrderHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oeTurns} providers waiters req respond =
  readBoundedBody nameBodyBytes req >>= ordered >>= sendAnswer respond
  where
    what = "POST /api/name/register"
    ordered = \case
      Just body | Just (J.Object o) <- J.decode body -> either refuse withCode (parseCodeOrder o)
      _ -> refuse $ ORBadRequest notObject
    refuse = refused what
    withCode (order, codeText) =
      issuedCode codeText >>= \case
        Nothing -> refuse $ ORCode CRInvalid
        Just code -> checkOrderSigner order >>= either refuse (const $ signed order code)
    issuedCode = maybe (pure Nothing) (readPrepaidCode oeStore) . parseNameCode
    signed order@OrderRequest {orOwner, orFields} code@PrepaidCode {pcCodeHash} = do
      let hash = orderHash orOwner orFields (OTCode pcCodeHash)
      withLabelTurn oeTurns (ofLabelHash orFields) $
        answerOrder env what order hash (\_ NameRegistration {nrId} -> pure $ answerNow $ registrationAnswer nrId) (\now -> newOrder now order hash code)
    newOrder now order@OrderRequest {orFields = fields@OrderFields {ofLabelHash, ofDuration}, orTimestamp} hash code@PrepaidCode {pcYears} = do
      let checks =
            [ pure $ checkTimestamp now orTimestamp,
              pure $ checkDuration oeConfig now (orderYears fields pcYears) ofDuration,
              checkTerms oeConfig oeContracts fields,
              checkNameFree oeContracts oeStore ofLabelHash
            ]
      runExceptT (mapM_ ExceptT checks) >>= \case
        Left refusal -> refuse refusal
        Right () -> do
          rid <- newRegistrationId
          placeCodeOrder env order hash rid code >>= \case
            Left refusal -> refuse refusal
            Right closed -> pure Answer {aResponse = registrationAnswer rid, aAfter = announceClosed env providers waiters closed}

parseCodeOrder :: J.Object -> Either OrderRefusal (OrderRequest, Text)
parseCodeOrder o = do
  order@(req, _) <- parseBody "code order" $ (,) <$> parseOrderRequest o <*> o .: "code"
  order <$ checkLength (orFields req)

-- | POST /api/registration/:id/spend, another name paid from this order's balance, in the contract's order of checks,
-- under the new name's turn and then the balance's.
spendHandler :: OrdersEnv -> HttpEnv -> ServiceConfig -> [Provider] -> Waiters -> Text -> Request -> Respond -> IO ResponseReceived
spendHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oeTurns} http cfg providers waiters ridText req respond
  | nCodesOnly oeConfig = sendAnswer respond =<< refuse ORCodesOnly
  | otherwise = readBoundedBody nameBodyBytes req >>= requested >>= sendAnswer respond
  where
    source = RegistrationId ridText
    what = "POST /api/registration/:id/spend"
    refuse = refused what
    requested = \case
      Just body | Just (J.Object o) <- J.decode body -> either refuse paying (parseSpend source o)
      _ -> refuse $ ORBadRequest notObject
    paying ((fields, timestamp, signature), spend) =
      withTransaction oeStore (`getRegistration` source) >>= \case
        Nothing -> pure $ answerNow notFound
        Just NameRegistration {nrOwner} ->
          checkSigner nrOwner (spendMessage source fields timestamp) signature >>= \case
            Left refusal -> refuse refusal
            Right () -> do
              let order = OrderRequest {orOwner = nrOwner, orFields = fields, orTimestamp = timestamp, orSignature = signature}
              withLabelTurn oeTurns (ofLabelHash fields) $ withBalanceTurn oeTurns source $ inTurns order spend
    inTurns order@OrderRequest {orOwner, orFields} spend = do
      let hash = orderHash orOwner orFields (OTBalance spend)
      answerOrder env what order hash (resentOrder env cfg) $ \now ->
        runExceptT (checked now order spend) >>= either refuse (place order hash spend)
    checked now OrderRequest {orFields = fields@OrderFields {ofLabelHash, ofLength, ofDuration}, orTimestamp} spend@Spend {spPrice} = do
      let years = orderYears fields $ nYears oeConfig
      liftEither $ checkTimestamp now orTimestamp
      liftEither $ checkDuration oeConfig now years ofDuration
      ExceptT $ checkTerms oeConfig oeContracts fields
      holder <- ExceptT $ checkSpendBalance oeStore spend now
      ExceptT $ checkNameFree oeContracts oeStore ofLabelHash
      ExceptT $ checkPrice oeContracts ofLength years spPrice
      pure holder
    place order@OrderRequest {orOwner, orFields} hash spend@Spend {spPrice, spBalance, spMethod} holder = case spMethod of
      Nothing -> do
        rid <- newRegistrationId
        let new = NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = orFields, nrgYears = nYears oeConfig, nrgOrderHash = hash}
        placeOrder env order (maybeToList holder) (coveredOrderInsert spend new) >>= \case
          Left refusal -> refuse refusal
          Right closed -> pure Answer {aResponse = registrationAnswer rid, aAfter = announceClosed env providers waiters closed}
      Just (SPMCard _)
        | spPrice - spBalance < minCardChargeCents -> refuse $ ORBadRequest "a card difference under the card minimum"
      Just method ->
        createAtProvider env http (checkoutClient http req) cfg providers waiters what $
          InvoicedOrder {ivRequest = order, ivOrderHash = hash, ivHolders = maybeToList holder, ivMethod = method, ivPrice = spPrice, ivAmount = spPrice - spBalance, ivInsert = differenceOrderInsert spend}

-- | Stripe refuses a USD charge under 50 cents, so the page offers only crypto for a smaller difference.
minCardChargeCents :: USDCents
minCardChargeCents = 50

-- | Every format check, the method present exactly for a difference, then the length's range.
parseSpend :: RegistrationId -> J.Object -> Either OrderRefusal (SignedOrderFields, Spend)
parseSpend spSource o = do
  parsed@((fields, _, _), _) <- parseBody "spend" $ do
    signed <- parseOrderFields o
    spPrice <- cents "price"
    spBalance <- cents "balance"
    spMethod <- case (KM.member "method" o, spPrice > spBalance) of
      (False, False) -> pure Nothing
      (True, True) -> Just <$> jsonField o "method" paymentMethod
      _ -> fail "the method is present exactly when the price is above the balance"
    pure (signed, Spend {spSource, spPrice, spBalance, spMethod})
  parsed <$ checkLength fields
  where
    -- positive, and within an invoice amount's range
    cents :: J.Key -> JT.Parser USDCents
    cents k = jsonField o k $ \v -> invoiceCents v >>= \n -> if n > 0 then pure n else fail "not above 0"

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

orderRefusalResponse :: OrderRefusal -> Response
orderRefusalResponse = \case
  ORBadRequest _ -> badRequest
  ORInvalidName -> invalidName
  OROrderExists -> jsonError status409 "order_exists"
  ORNameTaken -> jsonError status409 "name_taken"
  ORNameCheckUnavailable -> jsonError status503 "name_check_unavailable"
  ORCatalogChanged price -> jsonResponse status409 $ J.object ["error" .= ("catalog_changed" :: Text), "price" .= price]
  ORCode refusal -> codeRefusalResponse refusal
  ORPriceTooHigh price -> jsonResponse status409 $ J.object ["error" .= ("price_too_high" :: Text), "price" .= price]
  ORBalanceChanged balance -> jsonResponse status409 $ J.object ["error" .= ("balance_changed" :: Text), "balance" .= (balanceJSON <$> balance)]
  ORTermsChanged terms community -> jsonResponse status409 $ J.object ["error" .= ("terms_changed" :: Text), "terms" .= terms, "community" .= community]
  ORTermsUnavailable wait -> maybe (jsonError status503 "terms_unavailable") termsUnavailable wait
  ORCodesOnly -> jsonError status403 "codes_only"
