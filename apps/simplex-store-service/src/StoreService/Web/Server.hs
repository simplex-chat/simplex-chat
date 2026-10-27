{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StoreService.Web.Server
  ( WebEnv (..),
    newWebEnv,
    exportWebapp,
    maxWebhookBytes,
    webApp,
    webListeners,
    webSettings,
    runWebListeners,
    checkSiteDirs,
  )
where

import StoreService.Catalog (PricedOffer (..), priceOffer)
import StoreService.Config (ListenerConfig (..), ServiceConfig (..), StripeConfig (..))
import StoreService.Names.Abi (hex)
import StoreService.Names.Env (NamesEnv)
import StoreService.Names.Orders (OrdersEnv (..), ordersEnv)
import StoreService.Names.Phases (RegistrationId, awaitChange)
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), getRegistration, textMethod)
import StoreService.Names.View (lapseMoment, registrationAt, registrationVersion)
import StoreService.Orders (RegistrationChanged (..), cancelOpenInvoice)
import StoreService.Poller (ReadHints, queueReadHint)
import StoreService.Providers (OrderDraft (..), Provider (..), ProviderError (..), ProviderInvoice (..), WebhookError (..))
import StoreService.Store.Invoices (CreateError (..), InvoicePayment (..), InvoiceProduct (..), InvoiceRow (..), NewInvoice (..), codeHashExists, createInvoiceRows, getInvoice, getInvoiceByProviderRef, newInvoiceId, paymentHolds, readCatalogRows, textToInvoiceStatus, truncateToSecond)
import StoreService.Waiters (Seen, Waiters, awaitStatus)
import StoreService.Web.Http
import StoreService.Web.Names (namesApp, namesRoutes)
import StoreService.Web.Names.Orders (nameInvoiceHandler, nameInvoiceView)
import StoreService.Web.Names.Registration (heldFor, registrationChangedResponse, versionParam)
import Control.Concurrent.Async (race)
import qualified Control.Exception as E
import StoreService.Log (logError, logInfo, logWarn)
import Control.Monad (filterM, forM_, void, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Word (Word8)
import Network.HTTP.Types (Header, Status, hCacheControl, hContentType, status200, status400, status409, status413)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.Badges (BadgeType)
import Simplex.Chat.Badges.Types (BadgeOfferId (..), BadgePriceId (..))
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), ServicePaymentDestination (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Util (raceAny_, tshow)
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removePathForcibly)
import System.FilePath (pathSeparator, takeExtension, (</>))

data WebEnv = WebEnv
  { weStore :: DBStore,
    weConfig :: ServiceConfig,
    weWaiters :: Waiters,
    weProviders :: [Provider],
    weHoldMicros :: Int,
    weHints :: ReadHints,
    weHttp :: HttpEnv,
    -- | @Nothing@ when @serve_webapp@ is off.
    weBadgesSite :: Maybe Site,
    -- | @Nothing@ unless the names site is served on @names_port@.
    weNamesSite :: Maybe Site,
    -- | @Nothing@ when the ini has no @[names]@ section.
    weNames :: Maybe NamesEnv,
    -- | The name orders' part of weNames, which tests set without a chat controller.
    weOrders :: Maybe OrdersEnv
  }

-- | A built site, served when @serve_webapp@ is on.
data Site = Site
  { siteDir :: FilePath,
    -- | @Nothing@ when no Stripe key is configured, and the pristine shell is served instead.
    siteShell :: Maybe LB.ByteString
  }

newWebEnv :: DBStore -> ServiceConfig -> Waiters -> ReadHints -> [Provider] -> Maybe NamesEnv -> IO WebEnv
newWebEnv weStore weConfig weWaiters weHints weProviders weNames = do
  weHttp <- newHttpEnv lc
  weBadgesSite <- if lServeWebapp then Just <$> loadSite lBadgesStaticDir else pure Nothing
  weNamesSite <- mapM loadSite (lNamesPort *> lNamesStaticDir)
  pure WebEnv {weStore, weConfig, weWaiters, weProviders, weHoldMicros = holdMicros, weHints, weHttp, weBadgesSite, weNamesSite, weNames, weOrders = ordersEnv <$> weNames}
  where
    lc@ListenerConfig {lServeWebapp, lBadgesStaticDir, lNamesStaticDir, lNamesPort} = listener weConfig
    loadSite dir = Site dir <$> prepareShell dir (stripe weConfig)

listenerConfig :: WebEnv -> ListenerConfig
listenerConfig WebEnv {weConfig} = listener weConfig

maxWebhookBytes :: Int
maxWebhookBytes = 64 * 1024

catalogChanged :: Response
catalogChanged = jsonError status400 "catalog_changed"

webhookOk, webhookRefused, webhookTooLarge :: Response
webhookOk = webhookResponse status200
webhookRefused = webhookResponse status400
webhookTooLarge = webhookResponse status413

webhookResponse :: Status -> Response
webhookResponse st = responseLBS st [(hCacheControl, "no-store")] ""

-- | The badges port's app.
webApp :: WebEnv -> Application
webApp env = siteApp env (weBadgesSite env)

-- | Every port answers the API and the webhooks, and serves its own site's files.
siteApp :: WebEnv -> Maybe Site -> Application
siteApp env@WebEnv {weHttp} served req respond = case pathInfo req of
  path | Just site <- served, sitePath path -> only "GET" req respond $ serveSite site path respond
  ["api", "invoice"] -> only "POST" req respond $ limited weHttp createLimit req respond $ createInvoiceHandler env req respond
  ["api", "invoice", iid] -> only "GET" req respond $ limited weHttp readLimit req respond $ readInvoiceHandler env (InvoiceId iid) req respond
  ["api", "invoice", iid, "cancel"] -> only "POST" req respond $ limited weHttp createLimit req respond $ cancelInvoiceHandler env (InvoiceId iid) respond
  ["webhooks", "btcpay"] -> only "POST" req respond $ webhookHandler env PPCrypto "POST /webhooks/btcpay" req respond
  ["webhooks", "stripe"] -> only "POST" req respond $ webhookHandler env PPStripe "POST /webhooks/stripe" req respond
  _ -> fromMaybe (respond notFound) names
  where
    names = weNames env >>= \n -> namesApp weHttp (namesRoutes (weConfig env) n) req respond

-- | The names build keeps files at its root, so every path outside the API and the webhooks is the site's.
sitePath :: [Text] -> Bool
sitePath = \case
  "api" : _ -> False
  "webhooks" : _ -> False
  _ -> True

-- | A @".."@ check would miss separators WAI has already decoded, an absolute path, and a symlink out of the tree.
serveSite :: Site -> [Text] -> Respond -> IO ResponseReceived
serveSite Site {siteDir, siteShell} path respond =
  resolveInside siteDir (if null path then [T.pack shellFile] else path) >>= \case
    Nothing -> respond notFound
    Just (root, file)
      | Just shell <- siteShell, file == root </> shellFile -> respond (responseLBS status200 (staticHeaders root file) shell)
      | otherwise -> respond (responseFile status200 (staticHeaders root file) file Nothing)

shellFile :: FilePath
shellFile = "index.html"

-- | The badges shell also has an @id@ before @name@, and the names shell has none.
publishableKeyMeta :: Text
publishableKeyMeta = "name=\"stripe-publishable-key\" content=\""

-- | @Nothing@ (serve the pristine file) when no key is set, no shell is present, or it cannot be read.
prepareShell :: FilePath -> Maybe StripeConfig -> IO (Maybe LB.ByteString)
prepareShell _ Nothing = pure Nothing
prepareShell dir (Just StripeConfig {sPublishableKey}) =
  E.try attempt >>= \case
    Right out -> pure out
    Left (e :: E.IOException) -> do
      logWarn ("could not read the shell in " <> T.pack dir <> ", serving the pristine one: " <> tshow e)
      pure Nothing
  where
    attempt :: IO (Maybe LB.ByteString)
    attempt = do
      let shell = dir </> shellFile
      present <- doesFileExist shell
      if not present
        then pure Nothing
        else do
          -- TIO.readFile would decode in the locale's encoding, which fails outright under a C locale.
          bytes <- BS.readFile shell
          case decodeUtf8' bytes of
            Left e -> do
              logWarn ("the shell in " <> T.pack dir <> " is not valid UTF-8, serving the pristine one: " <> tshow e)
              pure Nothing
            Right html -> pure (Just (LB.fromStrict (encodeUtf8 (injectPublishableKey sPublishableKey html))))

exportWebapp :: ListenerConfig -> Maybe StripeConfig -> IO ()
exportWebapp ListenerConfig {lBadgesStaticDir, lBadgesExportDir, lNamesStaticDir, lNamesExportDir} stripeCfg = do
  forM_ lBadgesExportDir $ exportSite lBadgesStaticDir
  forM_ ((,) <$> lNamesStaticDir <*> lNamesExportDir) $ uncurry exportSite
  where
    exportSite dir out = do
      emptyDir out
      copyTree dir out
      prepareShell dir stripeCfg >>= mapM_ (LB.writeFile (out </> shellFile))
      logInfo ("store service: exported " <> T.pack dir <> " to " <> T.pack out)

-- | Contents only: the directory itself may be a bind or volume mount point that cannot be removed.
emptyDir :: FilePath -> IO ()
emptyDir dir = do
  createDirectoryIfMissing True dir
  entries <- listDirectory dir
  forM_ entries (removePathForcibly . (dir </>))

copyTree :: FilePath -> FilePath -> IO ()
copyTree src dst = do
  isDir <- doesDirectoryExist src
  if isDir
    then do
      createDirectoryIfMissing True dst
      listDirectory src >>= mapM_ (\e -> copyTree (src </> e) (dst </> e))
    else copyFile src dst

injectPublishableKey :: Text -> Text -> Text
injectPublishableKey key = T.replace (publishableKeyMeta <> "\"") (publishableKeyMeta <> key <> "\"")

resolveInside :: FilePath -> [Text] -> IO (Maybe (FilePath, FilePath))
resolveInside dir segments = either ioFailed id <$> E.try attempt
  where
    -- a NUL byte in the path makes the calls below throw
    ioFailed :: E.IOException -> Maybe (FilePath, FilePath)
    ioFailed _ = Nothing
    attempt :: IO (Maybe (FilePath, FilePath))
    attempt = do
      root <- canonicalizePath dir
      file <- canonicalizePath (foldl (\acc s -> acc </> T.unpack s) root segments)
      exists <- doesFileExist file
      pure $ if exists && inside root file then Just (root, file) else Nothing
    inside :: FilePath -> FilePath -> Bool
    inside root file = (root <> [pathSeparator]) `isPrefixOf` file

-- | The page at @\/@ must not be cached, or a browser would keep asking for assets from a build we no longer have.
staticHeaders :: FilePath -> FilePath -> [Header]
staticHeaders root file =
  [ (hContentType, contentTypeFor file),
    (hCacheControl, if hashedAsset then "public, max-age=31536000, immutable" else "no-cache")
  ]
  where
    -- read off the resolved path, not the request: `/assets/<hash>/%2e%2e/%2e%2e/index.html` resolves to the shell
    hashedAsset = (root </> "assets" <> [pathSeparator]) `isPrefixOf` file

contentTypeFor :: FilePath -> ByteString
contentTypeFor file = case map toLower (takeExtension file) of
  ".html" -> "text/html; charset=utf-8"
  ".css" -> "text/css; charset=utf-8"
  ".js" -> "text/javascript; charset=utf-8"
  ".json" -> "application/json"
  ".webmanifest" -> "application/manifest+json"
  ".svg" -> "image/svg+xml"
  ".png" -> "image/png"
  ".ico" -> "image/vnd.microsoft.icon"
  ".woff2" -> "font/woff2"
  ".txt" -> "text/plain; charset=utf-8"
  _ -> "application/octet-stream"

readInvoiceHandler :: WebEnv -> InvoiceId -> Request -> Respond -> IO ResponseReceived
readInvoiceHandler env@WebEnv {weStore} invId req respond =
  getInvoice weStore invId >>= \case
    Nothing -> respond notFound
    Just row@InvoiceRow {irProduct = IPName rid} | Just orders <- weOrders env -> readNameInvoice env orders row rid req respond
    Just row -> case holdFor row of
      Nothing -> respondInvoice env row respond
      Just seen -> do
        -- Settlement publishes after it commits, so the row read below is at least as new as whatever woke us.
        _ <- awaitStatus (weWaiters env) invId (readSeen env invId (pure id)) seen (weHoldMicros env)
        respondCurrent env invId respond
  where
    holdFor :: InvoiceRow -> Maybe Seen
    holdFor row@InvoiceRow {irStatus} = case waitParam req of
      Just seen | seen == irStatus, seen /= ISPaid, samePayment req row -> Just (seenOf row)
      _ -> Nothing

-- | A name invoice also holds while its registration's version is seenVersion, a paid one on that alone,
-- except one paid after its order closed, which has nothing more to wait for.
readNameInvoice :: WebEnv -> OrdersEnv -> InvoiceRow -> RegistrationId -> Request -> Respond -> IO ResponseReceived
readNameInvoice env@WebEnv {weConfig, weWaiters, weHoldMicros} orders@OrdersEnv {oeStore, oeNow, oePhases} stored@InvoiceRow {irInvoiceId = invId} rid req respond =
  readName orders stored rid >>= \case
    Nothing -> respond notFound
    Just (now, row, r) -> case waitParam req of
      Just wait | holds wait row r -> hold now row r >> respondCurrent env invId respond
      _ -> respond . jsonResponse status200 =<< nameInvoiceView orders (confirmationsFor weConfig) row r
  where
    seenVersion = versionParam req
    holds wait row@InvoiceRow {irStatus} NameRegistration {nrPhase, nrVersion} =
      wait == irStatus
        && not (irStatus == ISPaid && nrPhase == PUnpaid)
        && samePayment req row
        && maybe (irStatus /= ISPaid) (== nrVersion) seenVersion
    hold now row r = do
      let micros = heldFor weHoldMicros now $ catMaybes [expiryMoment row, lapseMoment r]
          onStatus = awaitStatus weWaiters invId (readSeen env invId (invoiceAt <$> oeNow)) (seenOf row) micros
          onVersion v = awaitChange oePhases rid (registrationVersion oeStore rid) v micros
      case (irStatus row, seenVersion) of
        (ISPaid, Just v) -> void $ onVersion v
        (_, Just v) -> void $ race onStatus (onVersion v)
        (_, Nothing) -> void onStatus

-- | The invoice and its registration as a read now shows them.
readName :: OrdersEnv -> InvoiceRow -> RegistrationId -> IO (Maybe (UTCTime, InvoiceRow, NameRegistration))
readName OrdersEnv {oeStore, oeNow} row rid = do
  now <- oeNow
  fmap (\r -> (now, invoiceAt now row, registrationAt now r)) <$> withTransaction oeStore (`getRegistration` rid)

-- | The counter a hold watches starts at zero, so a payment recorded before this request arrived cannot wake it.
samePayment :: Request -> InvoiceRow -> Bool
samePayment req row = maybe True (== paymentMark row) (paidParam req)

seenOf :: InvoiceRow -> Seen
seenOf row@InvoiceRow {irStatus} = (irStatus, paymentMark row)

-- | What a hold compares, read as the answer would show it.
readSeen :: WebEnv -> InvoiceId -> IO (InvoiceRow -> InvoiceRow) -> IO Seen
readSeen WebEnv {weStore} invId reading = do
  readAs <- reading
  maybe (ISOpen, ("", False)) (seenOf . readAs) <$> getInvoice weStore invId

respondCurrent :: WebEnv -> InvoiceId -> Respond -> IO ResponseReceived
respondCurrent env@WebEnv {weStore} invId respond = getInvoice weStore invId >>= maybe (respond notFound) (\row -> respondInvoice env row respond)

respondInvoice :: WebEnv -> InvoiceRow -> Respond -> IO ResponseReceived
respondInvoice env row respond = invoiceViewOf env row >>= respond . either id (jsonResponse status200)

-- | A badge's view, or a name's with its registration.
invoiceViewOf :: WebEnv -> InvoiceRow -> IO (Either Response J.Value)
invoiceViewOf env@WebEnv {weConfig} row@InvoiceRow {irInvoiceId = InvoiceId invId, irProduct} = case irProduct of
  IPBadge badgeType months -> pure $ Right $ badgeInvoiceView (confirmationsFor weConfig) badgeType months row
  IPName rid -> case weOrders env of
    Nothing -> Left providerUnavailable <$ namesOff invId
    Just orders ->
      readName orders row rid >>= \case
        Nothing -> pure $ Left notFound
        Just (_, row', r) -> Right <$> nameInvoiceView orders (confirmationsFor weConfig) row' r

namesOff :: Text -> IO ()
namesOff invId = logError ("invoice " <> invId <> " is a name's, and this service sells no names")

-- | Monero reports a payment as confirming while its figures are zero, so the verdict counts as much as the figure.
paymentMark :: InvoiceRow -> (Text, Bool)
paymentMark InvoiceRow {irPayment} =
  (maybe "" (fromMaybe "" . ipCryptoPaid) irPayment, maybe False ipPaidInFull irPayment)

-- | The provider is cancelled first, because cancelling our record first would leave an address the buyer can still pay into with nothing watching it.
cancelInvoiceHandler :: WebEnv -> InvoiceId -> Respond -> IO ResponseReceived
cancelInvoiceHandler env@WebEnv {weStore, weProviders} invId@(InvoiceId invText) respond =
  getInvoice weStore invId >>= \case
    Nothing -> respond notFound
    Just InvoiceRow {irStatus} | irStatus /= ISOpen -> respond (jsonError status409 "not_open")
    Just row | funded row -> respond (jsonError status409 "funded")
    -- refused before the provider's cancel, since the answer could not show the order
    Just InvoiceRow {irProduct = IPName _} | Nothing <- weOrders env -> namesOff invText >> respond providerUnavailable
    Just InvoiceRow {irProvider, irProviderRef} -> case providerNamed weProviders irProvider of
      Nothing -> respond providerUnavailable
      Just Provider {pCancelInvoice} ->
        pCancelInvoice irProviderRef >>= \case
          Left (ProviderError e) -> do
            logError $ "cancel order " <> irProviderRef <> ": " <> e
            respond providerUnavailable
          Right () -> finish
  where
    funded InvoiceRow {irPayment} = maybe False paymentHolds irPayment
    finish = do
      now <- getCurrentTime
      E.try (cancelOpenInvoice weStore (weWaiters env) (oePhases <$> weOrders env) invId now) >>= \case
        Left (RegistrationChanged rid) -> maybe (pure providerUnavailable) (`registrationChangedResponse` rid) (weOrders env) >>= respond
        Right _ -> respondCurrent env invId respond

paidParam :: Request -> Maybe (Text, Bool)
paidParam req = case lookup "seenPaid" (queryString req) of
  Just raw -> (\seen -> (seen, fullParam)) <$> either (const Nothing) Just (decodeUtf8' (fromMaybe "" raw))
  Nothing -> Nothing
  where
    fullParam = lookup "seenFull" (queryString req) == Just (Just "1")

waitParam :: Request -> Maybe InvoiceStatus
waitParam req = case lookup "wait" (queryString req) of
  Just (Just raw) -> either (const Nothing) textToInvoiceStatus (decodeUtf8' raw)
  _ -> Nothing

badgeInvoiceView :: Maybe Int -> BadgeType -> Word8 -> InvoiceRow -> J.Value
badgeInvoiceView confirmations badgeType months row =
  J.object $ ["badgeType" .= (textEncode badgeType :: Text), "months" .= months] <> invoicePairs confirmations row

data CreateRequest = CreateRequest
  { crPriceId :: BadgePriceId,
    crOfferId :: Maybe BadgeOfferId,
    crMethod :: ServicePaymentMethod,
    crCodeHash :: ByteString
  }

-- | A name order carries its kind and a sealed name code, so it alone may use the larger body cap.
createInvoiceHandler :: WebEnv -> Request -> Respond -> IO ResponseReceived
createInvoiceHandler env@WebEnv {weStore, weConfig, weProviders, weWaiters} req respond =
  readBoundedBody nameBodyBytes req >>= \case
    Nothing -> refuse "body over the size limit" badRequest
    Just body -> case J.decode body of
      Just (J.Object o) -> case KM.lookup "kind" o of
        Nothing
          | LB.length body > fromIntegral maxBodyBytes -> refuse "body over the size limit" badRequest
          | otherwise -> maybe (refuse "malformed body" badRequest) badgeInvoice (parseCreateRequest o)
        Just (J.String "name") | Just orders <- weOrders env -> nameInvoiceHandler orders weConfig weProviders weWaiters o respond
        Just kind -> refuse ("no order of kind " <> tshow kind) badRequest
      _ -> refuse "malformed body" badRequest
  where
    refuse :: Text -> Response -> IO ResponseReceived
    refuse why response = logInfo ("POST /api/invoice refused: " <> why) >> respond response
    badgeInvoice cr@CreateRequest {crPriceId, crOfferId, crMethod, crCodeHash} = do
      (prices, offers) <- readCatalogRows weStore
      case priceOffer prices offers crPriceId crOfferId of
        Left reason -> refuse (tshow reason) catalogChanged
        Right priced ->
          codeHashExists weStore crCodeHash >>= \case
            True -> refuse "code hash already sold" codeConflict
            False -> case providerFor weProviders crMethod of
              Nothing -> refuse ("no provider configured for " <> tshow crMethod) providerUnavailable
              Just provider -> createAtProvider env provider cr priced respond

-- | Once the provider call succeeds an invoice exists at BTCPay, and any path below that does not write our rows leaves it stranded.
createAtProvider :: WebEnv -> Provider -> CreateRequest -> PricedOffer -> Respond -> IO ResponseReceived
createAtProvider WebEnv {weStore, weConfig} provider CreateRequest {crPriceId, crOfferId, crMethod, crCodeHash} priced respond =
  do
      invId <- newInvoiceId
      -- truncate once, so the response, the row and the provider get the same value
      now <- truncateToSecond <$> getCurrentTime
      let expiresAt = addUTCTime (invoiceWindow weConfig crMethod) now
          draft = OrderDraft {odAmount = poAmount priced, odCurrency = poCurrency priced, odReference = "badge:" <> hex crCodeHash}
      pCreateInvoice provider crMethod draft >>= \case
        Left (ProviderError e) -> failed ("provider refused to create an invoice: " <> e) providerUnavailable
        Right ProviderInvoice {piProviderRef, piDestination} -> do
          let atProvider = "providerRef " <> piProviderRef
              ni =
                NewInvoice
                  { niInvoiceId = invId,
                    niProviderRef = piProviderRef,
                    niCodeHash = crCodeHash,
                    niPriceId = crPriceId,
                    niOfferId = crOfferId,
                    niBadgeType = poBadgeType priced,
                    niMonths = poMonths priced,
                    niPrice = poPrice priced,
                    niAmount = poAmount priced,
                    niCurrency = poCurrency priced,
                    niProvider = pProvider provider,
                    niDestination = piDestination,
                    niExpiresAt = expiresAt,
                    niCreatedAt = now
                  }
          createInvoiceRows weStore ni >>= \case
            Left CECodeConflict -> logInfo ("POST /api/invoice: code hash lost the race, invoice abandoned at the provider (" <> atProvider <> ")") >> respond codeConflict
            Left e -> failed ("invoice rows not written, invoice abandoned at the provider (" <> atProvider <> "): " <> tshow e) internalError
            Right () -> respond (jsonResponse status200 (createdInvoice invId priced expiresAt piDestination))
  where
    failed :: Text -> Response -> IO ResponseReceived
    failed why response = logError ("POST /api/invoice failed: " <> why) >> respond response

createdInvoice :: InvoiceId -> PricedOffer -> UTCTime -> ServicePaymentDestination -> J.Value
createdInvoice (InvoiceId invId) PricedOffer {poBadgeType, poMonths, poAmount, poCurrency} expiresAt destination =
  J.object $
    [ "invoiceId" .= invId,
      "badgeType" .= (textEncode poBadgeType :: Text),
      "months" .= poMonths,
      "amount" .= amountJSON poAmount,
      "currency" .= poCurrency,
      "expiresAt" .= expiresAt
    ]
      <> destinationPairs Nothing destination

parseCreateRequest :: J.Object -> Maybe CreateRequest
parseCreateRequest o = do
  crPriceId <- BadgePriceId <$> textField "priceId"
  crOfferId <- offerField
  crMethod <- textMethod =<< textField "method"
  crCodeHash <- parseCodeHash =<< textField "codeHash"
  pure CreateRequest {crPriceId, crOfferId, crMethod, crCodeHash}
  where
    textField k = case KM.lookup k o of
      Just (J.String t) | not (T.null t) -> Just t
      _ -> Nothing
    offerField = case KM.lookup "offerId" o of
      Nothing -> Just Nothing
      Just J.Null -> Just Nothing
      Just (J.String t) | not (T.null t) -> Just (Just (BadgeOfferId t))
      _ -> Nothing

-- | A provider retries a delivery it sees fail, so anything thrown below is caught and answered 200 anyway.
webhookHandler :: WebEnv -> PaymentProvider -> Text -> Request -> Respond -> IO ResponseReceived
webhookHandler env@WebEnv {weStore, weHints} provider route req respond =
  E.try deliver >>= \case
    Right received -> pure received
    Left e -> case E.fromException e of
      Just async' -> E.throwIO (async' :: E.SomeAsyncException)
      Nothing -> do
        logError (route <> ": the delivery could not be handled, so no read was queued: " <> tshow (e :: E.SomeException))
        respond webhookOk
  where
    deliver :: IO ResponseReceived
    deliver = case providerNamed (weProviders env) provider of
      Nothing -> do
        logWarn (route <> ": no " <> tshow provider <> " adapter is configured, so every delivery is refused")
        respond webhookRefused
      Just p ->
        readBoundedBody maxWebhookBytes req >>= \case
          Nothing -> logInfo (route <> ": body over the " <> tshow maxWebhookBytes <> "-byte cap") >> respond webhookTooLarge
          Just body -> do
            now <- getCurrentTime
            -- a provider signs the exact bytes it sent, so parsing and re-encoding here would fail the signature on every event
            case pVerifyWebhook p now (requestHeaders req) (LB.toStrict body) of
              Left (WebhookError e) -> refuse e
              Left (WebhookStale e) -> logWarn (route <> " refused: " <> e) >> respond webhookRefused
              Right Nothing -> ignore body "nothing this service acts on"
              Right (Just ref) -> resolve body ref
    refuse :: Text -> IO ResponseReceived
    refuse why = logInfo (route <> " refused: " <> why) >> respond webhookRefused
    ignore :: LB.ByteString -> Text -> IO ResponseReceived
    ignore body why = logEvent body why >> respond webhookOk
    logEvent :: LB.ByteString -> Text -> IO ()
    logEvent body what = case eventTypeOf body of
      Just t -> logInfo (route <> ": " <> t <> ", " <> what)
      Nothing -> logWarn (route <> ": " <> noEventType <> ", " <> what)
    noEventType :: Text
    noEventType = "a verified payload with no readable \"type\""
    resolve :: LB.ByteString -> Text -> IO ResponseReceived
    resolve body ref =
      getInvoiceByProviderRef weStore ref >>= \case
        Nothing -> ignore body ("no invoice holds provider_ref " <> ref)
        Just InvoiceRow {irProvider}
          -- provider_ref is unique table-wide, not per provider, so without this a collision could credit the wrong order
          | irProvider /= provider -> ignore body ("provider_ref " <> ref <> " belongs to " <> tshow irProvider)
          | otherwise -> queue body ref
    queue :: LB.ByteString -> Text -> IO ResponseReceived
    queue body ref = do
      queued <- queueReadHint weHints ref
      if queued
        then logEvent body ("queued a read of " <> ref)
        else -- not an error, because the next pass finds this invoice anyway
          logWarn (route <> ": " <> fromMaybe noEventType (eventTypeOf body) <> ", the read queue is full, so the read of " <> ref <> " waits for the next pass")
      respond webhookOk

-- | For the log only. A rename of the @type@ field would make every delivery look successful.
eventTypeOf :: LB.ByteString -> Maybe Text
eventTypeOf body = case J.decode body of
  Just (J.Object o) | Just (J.String t) <- KM.lookup "type" o -> Just t
  _ -> Nothing

-- | Without the port, which each listener sets.
webSettings :: ListenerConfig -> Warp.Settings
webSettings ListenerConfig {lHost} =
  Warp.setHost (fromString (T.unpack lHost))
    . Warp.setOnExceptionResponse (const internalError)
    $ Warp.defaultSettings

-- | Each port with its app: the badges site's, and the names site's when it is served.
webListeners :: WebEnv -> [(Int, Application)]
webListeners env@WebEnv {weNamesSite} = (lBadgesPort, webApp env) : [(port, siteApp env weNamesSite) | Just port <- [lNamesPort]]
  where
    ListenerConfig {lBadgesPort, lNamesPort} = listenerConfig env

runWebListeners :: WebEnv -> IO ()
runWebListeners env = do
  warnIfHeaderTrustIsExposed lc
  raceAny_ [Warp.runSettings (Warp.setPort port (webSettings lc)) app | (port, app) <- webListeners env]
  where
    lc = listenerConfig env

-- | Each build that is served or exported must be a directory, or the service would serve nothing or fail its export.
checkSiteDirs :: ListenerConfig -> IO (Either String ())
checkSiteDirs ListenerConfig {lServeWebapp, lBadgesStaticDir, lBadgesExportDir, lNamesStaticDir, lNamesExportDir, lNamesPort} =
  filterM (fmap not . doesDirectoryExist . snd) used <&> \case
    (site, dir) : _ -> Left ("listener." <> site <> "_static_dir = " <> dir <> " is not a directory; set it to the " <> site <> " build, since the default is the Docker image's path")
    [] -> Right ()
  where
    used =
      [("badges", lBadgesStaticDir) | lServeWebapp || isJust lBadgesExportDir]
        <> [("names", dir) | Just dir <- [lNamesStaticDir], isJust lNamesPort || isJust lNamesExportDir]

warnIfHeaderTrustIsExposed :: ListenerConfig -> IO ()
warnIfHeaderTrustIsExposed ListenerConfig {lHost, lTrustForwardedFor} =
  when (lTrustForwardedFor && not (isLoopbackHost lHost)) $
    logWarn ("store service: trust_forwarded_for is on and the listener binds " <> lHost <> "; every caller can then choose its own rate limit bucket unless a proxy sets the header")

isLoopbackHost :: Text -> Bool
isLoopbackHost h = h `elem` ["127.0.0.1", "::1", "localhost"]
