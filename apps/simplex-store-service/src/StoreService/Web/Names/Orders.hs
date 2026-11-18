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
    refused,
    nameInvoiceHandler,
    codeOrderHandler,
    spendHandler,
    nameInvoiceView,
    orderRefusal,
    paymentMethod,
  )
where

import Control.Concurrent.STM (atomically)
import qualified Control.Exception as E
import Control.Logger.Simple (logError, logNote, logWarn)
import Control.Monad (forM_, unless)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Word (Word32)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, ResponseReceived)
import Simplex.Chat.Badges.Code (parseNameCode)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Encoding.String (strEncode, textDecode)
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix)
import Simplex.Messaging.Util (isAsyncCancellation, safeDecodeUtf8, tshow)
import StoreService.Catalog (catalogCurrency)
import StoreService.Config (NamesConfig (..), ServiceConfig)
import StoreService.Names.Balances (Spend (..), checkSpendBalance, coveredOrderInsert, differenceOrderInsert)
import StoreService.Names.Codes (CodeRefusal (..), readPrepaidCode)
import StoreService.Names.Community (termsLink)
import StoreService.Names.Orders
import StoreService.Names.Registration (OrderFields (..), RegistrationId (..), orderMessage, spendMessage)
import StoreService.Names.Store (BalanceInsertRefused (..), BrokenInvariant (..), NameRegistration (..), NewRegistration (..), OrderHash, OrderPayment (..), OrderTerms (..), PaidOrder (..), PrepaidCode (..), createPaidRegistration, getRegistration, newRegistrationId, orderHash, paymentInvoice)
import StoreService.Names.Turns (withBalanceTurn, withLabelTurn)
import StoreService.Names.View (NameRefusal (..), balanceView, refusalResponse, registrationView)
import StoreService.Orders (cancelNameInvoices, publishRegistration)
import StoreService.Providers (OrderDraft (..), Provider (..), ProviderInvoice (..), expiresItself, providerErrorText)
import StoreService.Store.Invoices (InvoiceRow (..), NewNameInvoice (..), getInvoice, insertNameInvoice, newInvoiceId)
import StoreService.Waiters (Waiters, publish)
import StoreService.Web.Http (CheckoutClient, HttpEnv, Respond, checkoutClient, confirmationsFor, invoiceAt, invoicePairs, invoiceWindow, jsonResponse, limitedCheckout, nameBodyBytes, providerFor, providerNamed, readBoundedBody)

data InvoiceOrder = InvoiceOrder
  { ioOrder :: SentOrder,
    ioMethod :: ServicePaymentMethod,
    ioPrice :: Word32
  }

data CodeOrder = CodeOrder {coOrder :: SentOrder, coCode :: Text}

data SpendRequest = SpendRequest
  { srSigned :: SignedFields,
    srPrice :: Word32,
    srBalance :: Word32,
    srMethod :: Maybe ServicePaymentMethod
  }

$(JQ.deriveFromJSON defaultJSON {J.fieldLabelModifier = dropPrefix "co"} ''CodeOrder)

instance J.FromJSON InvoiceOrder where
  parseJSON = J.withObject "order" $ \o -> InvoiceOrder <$> o .: "order" <*> jsonField o "method" paymentMethod <*> o .: "price"

instance J.FromJSON SpendRequest where
  parseJSON = J.withObject "spend" $ \o -> SpendRequest <$> o .: "signed" <*> o .: "price" <*> o .: "balance" <*> JT.explicitParseFieldMaybe paymentMethod o "method"

paymentMethod :: J.Value -> JT.Parser ServicePaymentMethod
paymentMethod = textWith $ maybe (Left "not a payment method") Right . textDecode

-- | An order's answer and the work its commit leaves, both done after the name's turn, so a slow client holds no turn.
data Answer = Answer {aResponse :: Either NameRefusal Response, aAfter :: IO ()}

answerNow :: Response -> Answer
answerNow r = Answer {aResponse = Right r, aAfter = pure ()}

refusalAnswer :: NameRefusal -> Answer
refusalAnswer r = Answer {aResponse = Left r, aAfter = pure ()}

sendAnswer :: Respond -> Answer -> IO ResponseReceived
sendAnswer respond Answer {aResponse, aAfter} = aAfter >> respond (either refusalResponse id aResponse)

-- | POST /api/invoice for a name, its body already read and its kind checked.
nameInvoiceHandler :: OrdersEnv -> HttpEnv -> CheckoutClient -> ServiceConfig -> [Provider] -> Waiters -> J.Object -> Respond -> IO ResponseReceived
nameInvoiceHandler env@OrdersEnv {oeStore} http client cfg providers waiters body respond
  | nCodesOnly (oeConfig env) = sendAnswer respond =<< refuse ORCodesOnly
  | otherwise = sendAnswer respond =<< case parseBody "order" (J.Object body) >>= invoiceOrder of
    Left refusal -> refuse refusal
    Right (req, method, price) ->
      answerOrder env what req (OTInvoice method price) (resentOrder env cfg) (newOrder req method price)
  where
    what = "POST /api/invoice (name)"
    refuse = refused what
    invoiceOrder InvoiceOrder {ioOrder, ioMethod, ioPrice} = (\req -> (req, ioMethod, fromIntegral ioPrice)) <$> orderRequest ioOrder
    newOrder req@OrderRequest {orFields = fields@OrderFields {ofLabelHash, ofLength, ofDuration}, orTimestamp} method price hash now = do
      let years = orderYears fields $ nYears $ oeConfig env
          checks =
            [ pure $ checkTimestamp now orTimestamp,
              pure $ checkDuration (oeConfig env) now years ofDuration,
              checkTerms (oeConfig env) (oeContracts env) fields,
              checkNameFree (oeContracts env) oeStore Nothing ofLabelHash,
              checkPrice (oeContracts env) ofLength years price
            ]
          insert new paid db at =
            createPaidRegistration db new Nothing paid at >>= \case
              Right () -> pure $ Right ()
              Left BIRLiveOrder -> pure $ Left OROrderExists
              Left BIRBalanceChanged -> E.throwIO $ BrokenInvariant "an order holding no balance was refused at a balance"
      runExceptT (mapM_ ExceptT checks) >>= \case
        Left refusal -> refuse refusal
        Right () -> createAtProvider env http client cfg providers waiters what now InvoicedOrder {ivRequest = req, ivOrderHash = hash, ivHolders = [], ivMethod = method, ivPrice = price, ivAmount = price, ivInsert = insert}

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
createAtProvider :: OrdersEnv -> HttpEnv -> CheckoutClient -> ServiceConfig -> [Provider] -> Waiters -> Text -> UTCTime -> InvoicedOrder -> IO Answer
createAtProvider env@OrdersEnv {oeStore, oeConfig} http client cfg providers waiters what now InvoicedOrder {ivRequest = req@OrderRequest {orOwner, orFields}, ivOrderHash, ivHolders, ivMethod, ivPrice, ivAmount, ivInsert} =
  case providerFor providers ivMethod of
    Nothing -> do
      logNote (what <> " refused: no provider configured for " <> tshow ivMethod)
      pure $ refusalAnswer NRProviderUnavailable
    Just provider -> limitedCheckout http client ivMethod (pure . refusalAnswer . NRRateLimited) $ do
      invId <- newInvoiceId
      let amount = CurrencyAmount $ fromIntegral ivAmount
      pCreateInvoice provider ivMethod OrderDraft {odAmount = amount, odCurrency = catalogCurrency, odReference = "name:" <> safeDecodeUtf8 (strEncode ivOrderHash)} >>= \case
        Left e -> do
          logError (what <> " failed at the provider: " <> providerErrorText e)
          pure $ refusalAnswer NRProviderUnavailable
        Right ProviderInvoice {piProviderRef, piDestination} -> do
          rid <- newRegistrationId
          let invoice = NewNameInvoice {nniInvoiceId = invId, nniProvider = pProvider provider, nniAmount = amount, nniCurrency = catalogCurrency, nniDestination = piDestination, nniExpiresAt = addUTCTime (invoiceWindow cfg ivMethod) now, nniCreatedAt = now}
              registration replaced = NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = orFields, nrgYears = nYears oeConfig, nrgOrderHash = ivOrderHash, nrgReplaces = replaced}
              paid = PaidOrder {poMethod = ivMethod, poPrice = ivPrice, poInvoiceId = invId, poProviderRef = piProviderRef}
          placeOrder env now req ivHolders (\replaced db at -> insertNameInvoice db invoice >> ivInsert (registration replaced) paid db at)
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
resentOrder env@OrdersEnv {oeStore} cfg now r@NameRegistration {nrId = rid@(RegistrationId ridText), nrPayment} = case nrPayment of
  OPInvoice _ invId -> invoiceAnswer invId
  OPDifference _ _ invId -> invoiceAnswer invId
  OPBalance _ -> pure $ answerNow $ registrationAnswer rid
  -- the hash of an invoice or spend order never matches a code order's
  OPCode _ -> E.throwIO $ BrokenInvariant $ "an invoice or spend order's hash matched code order " <> ridText
  where
    invoiceAnswer invId = getInvoice oeStore invId >>= maybe (E.throwIO $ BrokenInvariant $ "the invoice of order " <> ridText <> " is missing") (fmap answerNow . invoiceViewResponse env cfg r . invoiceAt now)

invoiceViewResponse :: OrdersEnv -> ServiceConfig -> NameRegistration -> InvoiceRow -> IO Response
invoiceViewResponse env cfg r row = jsonResponse status200 <$> nameInvoiceView env (confirmationsFor cfg) row r

registrationAnswer :: RegistrationId -> Response
registrationAnswer (RegistrationId rid) = jsonResponse status200 $ J.object ["registrationId" .= rid]

refused :: Text -> OrderRefusal -> IO Answer
refused what refusal = refusalAnswer (orderRefusal refusal) <$ logNote (what <> " refused: " <> tshow refusal)

notObject :: Text
notObject = "a body that is not a JSON object within its size limit"

answerOrder :: OrdersEnv -> Text -> OrderRequest -> OrderTerms -> (UTCTime -> NameRegistration -> IO Answer) -> (OrderHash -> UTCTime -> IO Answer) -> IO Answer
answerOrder OrdersEnv {oeStore, oeTurns, oeNow} what order@OrderRequest {orOwner, orFields, orTimestamp, orSignature} terms resent new =
  checkSigner orOwner message orSignature >>= \case
    Left refusal -> refused what refusal
    Right () -> withLabelTurn oeTurns (ofLabelHash orFields) $ inTurns $ do
      now <- oeNow
      withTransaction oeStore (\db -> existingOrder db order hash now) >>= \case
        EOResent r -> resent now r
        EOMeets -> refused what OROrderExists
        EONew -> new hash now
  where
    hash = orderHash orOwner orFields terms
    (message, inTurns) = case terms of
      OTInvoice {} -> (orderMessage orFields orTimestamp, id)
      OTCode _ -> (orderMessage orFields orTimestamp, id)
      OTBalance Spend {spSource} -> (spendMessage spSource orFields orTimestamp, withBalanceTurn oeTurns spSource)

-- | POST /api/name/register, an order paid with a prepaid code, in the contract's order of checks.
codeOrderHandler :: OrdersEnv -> [Provider] -> Waiters -> Request -> Respond -> IO ResponseReceived
codeOrderHandler env@OrdersEnv {oeStore, oeConfig, oeContracts} providers waiters req respond =
  readBoundedBody nameBodyBytes req >>= ordered >>= sendAnswer respond
  where
    what = "POST /api/name/register"
    ordered = \case
      Just body | Just v <- J.decode body -> either refuse withCode (parseBody "code order" v >>= \CodeOrder {coOrder, coCode} -> (,coCode) <$> orderRequest coOrder)
      _ -> refuse $ ORBadRequest notObject
    refuse = refused what
    withCode (order, codeText) =
      issuedCode codeText >>= \case
        Nothing -> refuse $ ORCode CRInvalid
        Just code@PrepaidCode {pcCodeHash} ->
          answerOrder env what order (OTCode pcCodeHash) (\_ NameRegistration {nrId} -> pure $ answerNow $ registrationAnswer nrId) (\hash now -> newOrder now order hash code)
    issuedCode = maybe (pure Nothing) (readPrepaidCode oeStore) . parseNameCode
    newOrder now order@OrderRequest {orFields = fields@OrderFields {ofLabelHash, ofDuration}, orTimestamp} hash code@PrepaidCode {pcYears} = do
      let checks =
            [ pure $ checkTimestamp now orTimestamp,
              pure $ checkDuration oeConfig now (orderYears fields pcYears) ofDuration,
              checkTerms oeConfig oeContracts fields,
              checkNameFree oeContracts oeStore Nothing ofLabelHash
            ]
      runExceptT (mapM_ ExceptT checks) >>= \case
        Left refusal -> refuse refusal
        Right () -> do
          rid <- newRegistrationId
          placeCodeOrder env order hash rid code >>= \case
            Left refusal -> refuse refusal
            Right closed -> pure Answer {aResponse = Right $ registrationAnswer rid, aAfter = announceHeld env providers waiters closed rid}

-- | POST /api/registration/:id/spend, another name paid from this order's balance, in the contract's order of checks,
-- under the new name's turn and then the balance's.
spendHandler :: OrdersEnv -> HttpEnv -> ServiceConfig -> [Provider] -> Waiters -> RegistrationId -> Request -> Respond -> IO ResponseReceived
spendHandler env@OrdersEnv {oeStore, oeConfig, oeContracts} http cfg providers waiters source req respond
  | nCodesOnly oeConfig = sendAnswer respond =<< refuse ORCodesOnly
  | otherwise = readBoundedBody nameBodyBytes req >>= requested >>= sendAnswer respond
  where
    what = "POST /api/registration/:id/spend"
    refuse = refused what
    requested = \case
      Just body | Just v <- J.decode body -> either refuse paying (parseBody "spend" v >>= spendOf source)
      _ -> refuse $ ORBadRequest notObject
    paying ((fields, timestamp, signature), spend) =
      withTransaction oeStore (`getRegistration` source) >>= \case
        Nothing -> pure $ refusalAnswer NRNotFound
        Just NameRegistration {nrOwner} -> do
          let order = OrderRequest {orOwner = nrOwner, orFields = fields, orTimestamp = timestamp, orSignature = signature}
          answerOrder env what order (OTBalance spend) (resentOrder env cfg) $ \hash now ->
            runExceptT (checked now order spend) >>= either refuse (place now order hash spend)
    checked now OrderRequest {orFields = fields@OrderFields {ofLabelHash, ofLength, ofDuration}, orTimestamp} spend@Spend {spPrice} = do
      let years = orderYears fields $ nYears oeConfig
      liftEither $ checkTimestamp now orTimestamp
      liftEither $ checkDuration oeConfig now years ofDuration
      ExceptT $ checkTerms oeConfig oeContracts fields
      holder <- ExceptT $ checkSpendBalance oeStore spend now
      ExceptT $ checkNameFree oeContracts oeStore Nothing ofLabelHash
      ExceptT $ checkPrice oeContracts ofLength years spPrice
      pure holder
    place now order@OrderRequest {orOwner, orFields} hash spend@Spend {spPrice, spBalance, spMethod} holder = case spMethod of
      Nothing -> do
        rid <- newRegistrationId
        let new replaced = NewRegistration {nrgId = rid, nrgOwner = orOwner, nrgOrder = orFields, nrgYears = nYears oeConfig, nrgOrderHash = hash, nrgReplaces = replaced}
        placeOrder env now order (maybeToList holder) (coveredOrderInsert spend . new) >>= \case
          Left refusal -> refuse refusal
          Right closed -> pure Answer {aResponse = Right $ registrationAnswer rid, aAfter = announceHeld env providers waiters closed rid}
      Just (SPMCard _)
        | spPrice - spBalance < minCardChargeCents -> refuse $ ORBadRequest "a card difference under the card minimum"
      Just method ->
        createAtProvider env http (checkoutClient http req) cfg providers waiters what now $
          InvoicedOrder {ivRequest = order, ivOrderHash = hash, ivHolders = maybeToList holder, ivMethod = method, ivPrice = spPrice, ivAmount = spPrice - spBalance, ivInsert = differenceOrderInsert spend}

-- | Stripe refuses a USD charge under 50 cents, so the page offers only crypto for a smaller difference.
minCardChargeCents :: USDCents
minCardChargeCents = 50

-- | Every format check, the method present exactly for a difference, then the length's range.
spendOf :: RegistrationId -> SpendRequest -> Either OrderRefusal (SignedOrderFields, Spend)
spendOf spSource SpendRequest {srSigned, srPrice, srBalance, srMethod}
  | srPrice == 0 || srBalance == 0 = Left $ ORBadRequest "a malformed spend: a price or balance not above 0"
  | isJust srMethod /= (srPrice > srBalance) = Left $ ORBadRequest "a malformed spend: the method is present exactly when the price is above the balance"
  | otherwise = (,Spend {spSource, spPrice = fromIntegral srPrice, spBalance = fromIntegral srBalance, spMethod = srMethod}) <$> signedFields srSigned

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
  ORPriceTooHigh price -> NRPriceTooHigh price
  ORBalanceChanged balance -> NRBalanceChanged $ balanceView <$> balance
  ORTermsChanged terms community -> NRTermsChanged (termsLink terms) community
  ORTermsUnavailable wait -> NRTermsUnavailable wait
  ORCodesOnly -> NRCodesOnly
