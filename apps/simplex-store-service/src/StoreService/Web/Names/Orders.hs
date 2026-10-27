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
    spendHandler,
    nameInvoiceView,
    orderRefusalResponse,
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Monad (forM_, unless)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Word (Word32, Word64)
import Network.HTTP.Types (status200, status409, status503)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Chat.Badges.Code (NameCode (..), parseNameCode)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Util (tshow)
import StoreService.Catalog (catalogCurrency)
import StoreService.Config (NamesConfig (..), ServiceConfig)
import StoreService.Log (logError, logInfo, logWarn)
import StoreService.Names.Abi (hex)
import StoreService.Names.Balances (Spend (..), checkSpendBalance, coveredOrderInsert, differenceOrderInsert, spendTerms)
import StoreService.Names.Codes (CodeRefusal (..), readPrepaidCode)
import StoreService.Names.Orders
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Signatures (OrderFields (..), spendMessage)
import StoreService.Names.Store (NameRegistration (..), NewRegistration (..), OrderTerms (..), PaidOrder (..), PrepaidCode (..), RegistrationPayment (..), createPaidRegistration, getRegistration, newRegistrationId, orderHash, textMethod)
import StoreService.Names.Turns (withBalanceTurn, withLabelTurn)
import StoreService.Names.View (balanceJSON, registrationAt, registrationView)
import StoreService.Orders (publishRegistration)
import StoreService.Providers (OrderDraft (..), Provider (..), ProviderError (..), ProviderInvoice (..), expiresItself)
import StoreService.Store.Invoices (InvoiceRow (..), NewNameInvoice (..), getInvoice, insertNameInvoice, newInvoiceId, truncateToSecond)
import StoreService.Waiters (Waiters, publish)
import StoreService.Web.Http (Respond, badRequest, confirmationsFor, internalError, invalidName, invoiceAt, invoicePairs, invoiceWindow, jsonError, jsonResponse, nameBodyBytes, notFound, providerFor, providerNamed, providerUnavailable, readBoundedBody)
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
nameInvoiceHandler env@OrdersEnv {oeStore, oeTurns, oeNow} cfg providers waiters body respond =
  sendAnswer respond =<< case parseInvoiceOrder body of
    Left refusal -> refuse refusal
    Right order@InvoiceOrder {ioRequest = req@OrderRequest {orOwner, orFields}, ioMethod, ioPrice} ->
      checkOrderSigner req >>= \case
        Left refusal -> refuse refusal
        Right () -> withLabelTurn oeTurns (ofLabelHash orFields) $ do
          now <- oeNow
          let hash = orderHash orOwner orFields (OTInvoice ioMethod $ fromIntegral ioPrice)
          withTransaction oeStore (\db -> existingOrder db req hash now) >>= \case
            EOResent r -> resentOrder env cfg what now r
            EOMeets -> refuse OROrderExists
            EONew _ -> newOrder order
  where
    what = "POST /api/invoice (name)"
    refuse = refused what
    newOrder InvoiceOrder {ioRequest = req@OrderRequest {orOwner, orFields = fields@OrderFields {ofLabelHash, ofLength, ofDuration}, orTimestamp}, ioMethod, ioPrice} = do
      now <- oeNow
      let years = nYears $ oeConfig env
          price = fromIntegral ioPrice
          checks =
            [ pure $ checkTimestamp now orTimestamp,
              pure $ checkDuration (oeConfig env) now years ofDuration,
              checkTerms (oeConfig env) (oeContracts env) fields,
              checkNameFree (oeContracts env) oeStore ofLabelHash,
              checkPrice (oeContracts env) ofLength years price
            ]
          insert new paid db at = first (const OROrderExists) <$> createPaidRegistration db new (OTInvoice ioMethod price) paid at
      firstRefusal checks >>= \case
        Just refusal -> refuse refusal
        Nothing -> createAtProvider env cfg providers waiters what InvoicedOrder {ivRequest = req, ivOrderHash = orderHash orOwner fields (OTInvoice ioMethod price), ivHolders = [], ivMethod = ioMethod, ivPrice = price, ivAmount = ioPrice, ivInsert = insert}

-- | An order paid by invoice: its order hash, the holders its transaction closes, the order's price, what its invoice asks
-- (less than the price when a balance paid the rest), and its insert after the invoice row's.
data InvoicedOrder = InvoicedOrder
  { ivRequest :: OrderRequest,
    ivOrderHash :: ByteString,
    ivHolders :: [NameRegistration],
    ivMethod :: ServicePaymentMethod,
    ivPrice :: Int,
    ivAmount :: Word32,
    ivInsert :: NewRegistration -> PaidOrder -> DB.Connection -> UTCTime -> IO (Either OrderRefusal ())
  }

-- | The invoice is made at the provider first, then the order is written with its invoice row; an order not written has its invoice cancelled there.
createAtProvider :: OrdersEnv -> ServiceConfig -> [Provider] -> Waiters -> Text -> InvoicedOrder -> IO Answer
createAtProvider env@OrdersEnv {oeStore, oeConfig, oeNow} cfg providers waiters what InvoicedOrder {ivRequest = req@OrderRequest {orOwner, orFields}, ivOrderHash, ivHolders, ivMethod, ivPrice, ivAmount, ivInsert} =
  case providerFor providers ivMethod of
    Nothing -> do
      logInfo (what <> " refused: no provider configured for " <> tshow ivMethod)
      pure $ answerNow providerUnavailable
    Just provider -> do
      invId <- newInvoiceId
      now <- truncateToSecond <$> oeNow
      let amount = CurrencyAmount ivAmount
      pCreateInvoice provider ivMethod OrderDraft {odAmount = amount, odCurrency = catalogCurrency, odReference = "name:" <> hex ivOrderHash} >>= \case
        Left (ProviderError e) -> do
          logError (what <> " failed: provider refused to create an invoice: " <> e)
          pure $ answerNow providerUnavailable
        Right ProviderInvoice {piProviderRef, piDestination} -> do
          rid <- newRegistrationId
          let invoice = NewNameInvoice {nniInvoiceId = invId, nniProvider = pProvider provider, nniAmount = amount, nniCurrency = catalogCurrency, nniDestination = piDestination, nniExpiresAt = addUTCTime (invoiceWindow cfg ivMethod) now, nniCreatedAt = now}
              registration = NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = orFields, nrgYears = nYears oeConfig}
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
                _ -> internalError <$ logError (what <> ": order " <> invText invId <> " is not readable after its commit")
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
resentOrder :: OrdersEnv -> ServiceConfig -> Text -> UTCTime -> NameRegistration -> IO Answer
resentOrder env@OrdersEnv {oeStore} cfg what now r@NameRegistration {nrId, nrPayment} = case nrPayment of
  RPInvoice _ invId -> getInvoice oeStore invId >>= maybe (refused what OROrderExists) (fmap answerNow . invoiceViewResponse env cfg (registrationAt now r) . invoiceAt now)
  RPBalance -> pure $ answerNow $ registrationAnswer nrId
  -- the hash of an invoice or spend order never matches a code order's
  RPCode _ -> refused what OROrderExists

invoiceViewResponse :: OrdersEnv -> ServiceConfig -> NameRegistration -> InvoiceRow -> IO Response
invoiceViewResponse env cfg r row = jsonResponse status200 <$> nameInvoiceView env (confirmationsFor cfg) row r

registrationAnswer :: RegistrationId -> Response
registrationAnswer (RegistrationId rid) = jsonResponse status200 $ J.object ["registrationId" .= rid]

refused :: Text -> OrderRefusal -> IO Answer
refused what refusal = answerNow (orderRefusalResponse refusal) <$ logInfo (what <> " refused: " <> tshow refusal)

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
              checkTerms oeConfig oeContracts fields,
              checkNameFree oeContracts oeStore ofLabelHash
            ]
      firstRefusal checks >>= \case
        Just refusal -> refuse refusal
        Nothing -> do
          rid <- newRegistrationId
          placeCodeOrder env order rid code >>= \case
            Left refusal -> refuse refusal
            Right closed -> pure Answer {aResponse = registrationAnswer rid, aAfter = announceClosed env providers waiters closed}

parseCodeOrder :: J.Object -> Either OrderRefusal (OrderRequest, Text)
parseCodeOrder o = do
  order@(req, _) <- maybe (Left ORBadRequest) Right $ (,) <$> parseOrderRequest o <*> textField o "code"
  order <$ checkLength (orFields req)

-- | POST /api/registration/:id/spend, another name paid from this order's balance, in the contract's order of checks,
-- under the new name's turn and then the balance's.
spendHandler :: OrdersEnv -> ServiceConfig -> [Provider] -> Waiters -> Text -> Request -> Respond -> IO ResponseReceived
spendHandler env@OrdersEnv {oeStore, oeConfig, oeContracts, oeTurns, oeNow} cfg providers waiters ridText req respond =
  readBoundedBody nameBodyBytes req >>= requested >>= sendAnswer respond
  where
    source = RegistrationId ridText
    what = "POST /api/registration/:id/spend"
    refuse = refused what
    requested = \case
      Just body | Just (J.Object o) <- J.decode body -> either refuse paying (parseSpend source o)
      _ -> refuse ORBadRequest
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
      now <- oeNow
      withTransaction oeStore (\db -> existingOrder db order (orderHash orOwner orFields $ spendTerms spend) now) >>= \case
        EOResent r -> resentOrder env cfg what now r
        EOMeets -> refuse OROrderExists
        EONew _ -> runExceptT (checked now order spend) >>= either refuse (place order spend)
    checked now OrderRequest {orFields = fields@OrderFields {ofLabelHash, ofLength, ofDuration}, orTimestamp} spend@Spend {spPrice} = do
      let years = nYears oeConfig
      liftEither $ checkTimestamp now orTimestamp
      liftEither $ checkDuration oeConfig now years ofDuration
      ExceptT $ checkTerms oeConfig oeContracts fields
      holder <- ExceptT $ checkSpendBalance oeStore spend now
      ExceptT $ checkNameFree oeContracts oeStore ofLabelHash
      ExceptT $ checkPrice oeContracts ofLength years spPrice
      pure holder
    place order@OrderRequest {orOwner, orFields} spend@Spend {spPrice, spBalance, spMethod} holder = case spMethod of
      Nothing -> do
        rid <- newRegistrationId
        let new = NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = orFields, nrgYears = nYears oeConfig}
        placeOrder env order (maybeToList holder) (coveredOrderInsert spend new) >>= \case
          Left refusal -> refuse refusal
          Right closed -> pure Answer {aResponse = registrationAnswer rid, aAfter = announceClosed env providers waiters closed}
      Just (SPMCard _)
        | spPrice - spBalance < minCardChargeCents -> refuse ORBadRequest
      Just method ->
        createAtProvider env cfg providers waiters what $
          InvoicedOrder {ivRequest = order, ivOrderHash = orderHash orOwner orFields (spendTerms spend), ivHolders = maybeToList holder, ivMethod = method, ivPrice = spPrice, ivAmount = fromIntegral (spPrice - spBalance), ivInsert = differenceOrderInsert spend}

-- | Stripe refuses a USD charge under 50 cents, so the page offers only crypto for a smaller difference.
minCardChargeCents :: Int
minCardChargeCents = 50

-- | Every format check, the method present exactly for a difference, then the length's range.
parseSpend :: RegistrationId -> J.Object -> Either OrderRefusal ((OrderFields, Word64, RecoverableSignature), Spend)
parseSpend spSource o = do
  parsed@((fields, _, _), _) <- maybe (Left ORBadRequest) Right $ do
    signed <- parseOrderFields o
    spPrice <- cents "price"
    spBalance <- cents "balance"
    spMethod <- case (KM.lookup "method" o, spPrice > spBalance) of
      (Nothing, False) -> Just Nothing
      (Just (J.String t), True) -> Just <$> textMethod t
      _ -> Nothing
    pure (signed, Spend {spSource, spPrice, spBalance, spMethod})
  parsed <$ checkLength fields
  where
    -- positive, and within an invoice amount's range
    cents :: J.Key -> Maybe Int
    cents k = KM.lookup k o >>= integerField >>= \n -> if n > (0 :: Word32) then Just (fromIntegral n) else Nothing

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
      RPBalance -> Nothing

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
nameInvoiceView OrdersEnv {oeStore, oeConfig, oeContracts, oeNow} confirmations row@InvoiceRow {irInvoiceId = InvoiceId invId} r = do
  now <- oeNow
  view <- registrationView oeStore oeConfig oeContracts now r
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
  ORPriceTooHigh price -> jsonResponse status409 $ J.object ["error" .= ("price_too_high" :: Text), "price" .= price]
  ORBalanceChanged balance -> jsonResponse status409 $ J.object ["error" .= ("balance_changed" :: Text), "balance" .= (balanceJSON <$> balance)]
  ORTermsChanged terms community -> jsonResponse status409 $ J.object ["error" .= ("terms_changed" :: Text), "terms" .= terms, "community" .= community]
