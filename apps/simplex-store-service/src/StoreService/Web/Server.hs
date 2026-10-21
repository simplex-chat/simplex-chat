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
    webSettings,
    runWebListener,
  )
where

import StoreService.Catalog (PricedOffer (..), priceOffer)
import StoreService.Config (BTCPayConfig (..), ListenerConfig (..), ServiceConfig (..), SpeedPolicy (..), StripeConfig (..), defaultExpiryMinutes, defaultSessionMinutes)
import StoreService.Poller (ReadHints, queueReadHint)
import StoreService.Providers (OrderDraft (..), Provider (..), ProviderError (..), ProviderInvoice (..), WebhookError (..))
import StoreService.Store.Invoices (CreateError (..), InvoicePayment (..), InvoiceRow (..), NewInvoice (..), cancelOpenInvoice, codeHashExists, createInvoiceRows, cryptoCurrencyText, getInvoice, getInvoiceByProviderRef, invoiceStatusText, newInvoiceId, paymentHolds, readCatalogRows, textToInvoiceStatus, truncateToSecond)
import StoreService.Names.Env (NamesEnv)
import StoreService.Waiters (Seen, Waiters, awaitStatus, publish)
import StoreService.Web.Http
import StoreService.Web.Names (namesApp, namesRoutes)
import Control.Concurrent.STM
import qualified Control.Exception as E
import StoreService.Log (logError, logInfo, logWarn)
import Control.Monad (forM_, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Char (toLower)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Types (Header, Status, hCacheControl, hContentType, status200, status400, status409, status413)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.Badges.Types (BadgeOfferId (..), BadgePriceId (..))
import Simplex.Chat.PaymentService.Types (CardProvider (..), CryptoCurrency (..), CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), ServicePaymentDestination (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (DBStore)
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Util (tshow)
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
    -- | @Nothing@ when no Stripe key is configured, and the pristine @static_dir@ shell is served instead.
    weShell :: Maybe LB.ByteString,
    -- | @Nothing@ when the ini has no @[names]@ section.
    weNames :: Maybe NamesEnv
  }

newWebEnv :: DBStore -> ServiceConfig -> Waiters -> ReadHints -> [Provider] -> Maybe NamesEnv -> IO WebEnv
newWebEnv weStore weConfig weWaiters weHints weProviders weNames = do
  weHttp <- newHttpEnv (listener weConfig)
  weShell <- prepareShell (listener weConfig) (stripe weConfig)
  pure WebEnv {weStore, weConfig, weWaiters, weProviders, weHoldMicros = holdMicros, weHints, weHttp, weShell, weNames}

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

webApp :: WebEnv -> Application
webApp env@WebEnv {weHttp} req respond = case pathInfo req of
  [] | serving -> only "GET" req respond $ case weShell env of
    Just shell -> respond (responseLBS status200 shellHeaders shell)
    Nothing -> serveStatic env ["index.html"] respond
  "assets" : rest | serving -> only "GET" req respond $ serveStatic env ("assets" : rest) respond
  ["sw.js"] | serving -> only "GET" req respond $ serveStatic env ["sw.js"] respond
  ["api", "invoice"] -> only "POST" req respond $ limited weHttp createLimit req respond $ createInvoiceHandler env req respond
  ["api", "invoice", iid] -> only "GET" req respond $ limited weHttp readLimit req respond $ readInvoiceHandler env (InvoiceId iid) req respond
  ["api", "invoice", iid, "cancel"] -> only "POST" req respond $ limited weHttp createLimit req respond $ cancelInvoiceHandler env (InvoiceId iid) respond
  ["webhooks", "btcpay"] -> only "POST" req respond $ webhookHandler env PPCrypto "POST /webhooks/btcpay" req respond
  ["webhooks", "stripe"] -> only "POST" req respond $ webhookHandler env PPStripe "POST /webhooks/stripe" req respond
  _ -> fromMaybe (respond notFound) names
  where
    serving = lServeWebapp (listenerConfig env)
    names = weNames env >>= \n -> namesApp weHttp (namesRoutes n) req respond

-- | A @".."@ check would miss separators WAI has already decoded, an absolute path, and a symlink out of the tree.
serveStatic :: WebEnv -> [Text] -> Respond -> IO ResponseReceived
serveStatic env segments respond =
  resolveInside (lStaticDir (listenerConfig env)) segments >>= \case
    Nothing -> respond notFound
    Just (root, file) -> respond (responseFile status200 (staticHeaders root file) file Nothing)

shellHeaders :: [Header]
shellHeaders = [(hContentType, "text/html; charset=utf-8"), (hCacheControl, "no-cache")]

publishableKeyMeta :: Text
publishableKeyMeta = "id=\"stripe-publishable-key\" name=\"stripe-publishable-key\" content=\""

-- | @Nothing@ (serve the pristine file) when no key is set, no shell is present, or it cannot be read.
prepareShell :: ListenerConfig -> Maybe StripeConfig -> IO (Maybe LB.ByteString)
prepareShell _ Nothing = pure Nothing
prepareShell ListenerConfig {lStaticDir} (Just StripeConfig {sPublishableKey}) =
  E.try attempt >>= \case
    Right out -> pure out
    Left (e :: E.IOException) -> do
      logWarn ("could not read the shell in " <> T.pack lStaticDir <> ", serving the pristine one: " <> tshow e)
      pure Nothing
  where
    attempt :: IO (Maybe LB.ByteString)
    attempt = do
      let shell = lStaticDir </> "index.html"
      present <- doesFileExist shell
      if not present
        then pure Nothing
        else do
          -- TIO.readFile would decode in the locale's encoding, which fails outright under a C locale.
          bytes <- BS.readFile shell
          case decodeUtf8' bytes of
            Left e -> do
              logWarn ("the shell in " <> T.pack lStaticDir <> " is not valid UTF-8, serving the pristine one: " <> tshow e)
              pure Nothing
            Right html -> pure (Just (LB.fromStrict (encodeUtf8 (injectPublishableKey sPublishableKey html))))

exportWebapp :: ListenerConfig -> Maybe StripeConfig -> IO ()
exportWebapp lc@ListenerConfig {lStaticDir, lWebappExportDir} stripeCfg =
  forM_ lWebappExportDir $ \out -> do
    emptyDir out
    copyTree lStaticDir out
    prepareShell lc stripeCfg >>= mapM_ (LB.writeFile (out </> "index.html"))
    logInfo ("store service: exported the webapp to " <> T.pack out)

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
    Just row -> case holdFor row of
      Nothing -> respond (jsonResponse status200 (invoiceView (confirmationsFor env) row))
      Just seen -> do
        -- Settlement publishes after it commits, so the row read below is at least as new as whatever woke us.
        _ <- awaitStatus (weWaiters env) invId readSeen seen (weHoldMicros env)
        getInvoice weStore invId >>= \case
          Nothing -> respond notFound
          Just row' -> respond (jsonResponse status200 (invoiceView (confirmationsFor env) row'))
  where
    holdFor :: InvoiceRow -> Maybe Seen
    holdFor row@InvoiceRow {irStatus} = case waitParam req of
      Just seen | seen == irStatus, seen /= ISPaid, samePayment -> Just (irStatus, paymentMark row)
      _ -> Nothing
      where
        -- The counter a hold watches starts at zero, so a payment recorded before this request arrived cannot wake it.
        samePayment = case paidParam req of
          Nothing -> True
          Just seen -> seen == paymentMark row
    readSeen :: IO Seen
    readSeen = maybe (ISOpen, ("", False)) (\row -> (irStatus row, paymentMark row)) <$> getInvoice weStore invId

-- | Monero reports a payment as confirming while its figures are zero, so the verdict counts as much as the figure.
paymentMark :: InvoiceRow -> (Text, Bool)
paymentMark InvoiceRow {irPayment} =
  (maybe "" (fromMaybe "" . ipCryptoPaid) irPayment, maybe False ipPaidInFull irPayment)

-- | The provider is cancelled first, because cancelling our record first would leave an address the buyer can still pay into with nothing watching it.
cancelInvoiceHandler :: WebEnv -> InvoiceId -> Respond -> IO ResponseReceived
cancelInvoiceHandler env@WebEnv {weStore} invId respond =
  getInvoice weStore invId >>= \case
    Nothing -> respond notFound
    Just InvoiceRow {irStatus} | irStatus /= ISOpen -> respond (jsonError status409 "not_open")
    Just row | funded row -> respond (jsonError status409 "funded")
    Just InvoiceRow {irProvider, irProviderRef} -> case providerNamed env irProvider of
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
      cancelled <- cancelOpenInvoice weStore invId now
      when cancelled $ atomically $ publish (weWaiters env) invId ISExpired
      getInvoice weStore invId >>= \case
        Nothing -> respond notFound
        Just row -> respond (jsonResponse status200 (invoiceView (confirmationsFor env) row))

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

-- | Greenfield reports no confirmation count, so this comes from the store's speed policy.
confirmationsFor :: WebEnv -> Maybe Int
confirmationsFor WebEnv {weConfig} = speedPolicyConfirmations . bSpeedPolicy <$> btcpay weConfig

-- | BTCPay's own store setting, whose numbering is not in speed order.
speedPolicyConfirmations :: SpeedPolicy -> Int
speedPolicyConfirmations = \case
  HighSpeed -> 0
  MediumSpeed -> 1
  LowMediumSpeed -> 2
  LowSpeed -> 6

invoiceView :: Maybe Int -> InvoiceRow -> J.Value
invoiceView confirmations InvoiceRow {irBadgeType, irMonths, irAmount, irCurrency, irDestination, irExpiresAt, irStatus, irPayment} =
  J.object $
    [ "status" .= invoiceStatusText irStatus,
      "badgeType" .= (textEncode irBadgeType :: Text),
      "months" .= irMonths,
      "amount" .= amountJSON irAmount,
      "currency" .= irCurrency,
      "expiresAt" .= irExpiresAt
    ]
      <> destinationPairs confirmations irDestination
      <> paidPairs
  where
    paidPairs = maybe [] paymentPairs irPayment
    paymentPairs p =
      concat
        [ maybe [] (\a -> ["amountPaid" .= amountJSON a]) (ipAmount p),
          maybe [] (\a -> ["cryptoAmountPaid" .= a]) (ipCryptoPaid p),
          maybe [] (\a -> ["cryptoAmountDue" .= a]) (ipCryptoDue p),
          ["paidInFull" .= ipPaidInFull p],
          ["settledAt" .= ipUpdatedAt p | irStatus == ISPaid]
        ]

destinationPairs :: Maybe Int -> ServicePaymentDestination -> [Pair]
destinationPairs confirmations = \case
  SPDCard _ url -> ["clientSecret" .= url]
  SPDCrypto currency address cryptoAmount ->
    [ "address" .= address,
      "cryptoAmount" .= cryptoAmount,
      "cryptoCurrency" .= cryptoCurrencyText currency
    ]
      <> maybe [] (\n -> ["requiredConfirmations" .= n]) confirmations

amountJSON :: CurrencyAmount -> J.Value
amountJSON (CurrencyAmount a) = J.toJSON a

data CreateRequest = CreateRequest
  { crPriceId :: BadgePriceId,
    crOfferId :: Maybe BadgeOfferId,
    crMethod :: ServicePaymentMethod,
    crCodeHash :: ByteString
  }

createInvoiceHandler :: WebEnv -> Request -> Respond -> IO ResponseReceived
createInvoiceHandler env@WebEnv {weStore} req respond =
  readBoundedBody maxBodyBytes req >>= \case
    Nothing -> refuse "body over the size limit" badRequest
    Just body -> case parseCreateRequest body of
      Nothing -> refuse "malformed body" badRequest
      Just cr@CreateRequest {crPriceId, crOfferId, crMethod, crCodeHash} -> do
        (prices, offers) <- readCatalogRows weStore
        case priceOffer prices offers crPriceId crOfferId of
          Left reason -> refuse (tshow reason) catalogChanged
          Right priced ->
            codeHashExists weStore crCodeHash >>= \case
              True -> refuse "code hash already sold" codeConflict
              False -> case providerFor env crMethod of
                Nothing -> refuse ("no provider configured for " <> tshow crMethod) providerUnavailable
                Just provider -> createAtProvider env provider cr priced respond
  where
    refuse :: Text -> Response -> IO ResponseReceived
    refuse why response = logInfo ("POST /api/invoice refused: " <> why) >> respond response

-- | Once the provider call succeeds an invoice exists at BTCPay, and any path below that does not write our rows leaves it stranded.
createAtProvider :: WebEnv -> Provider -> CreateRequest -> PricedOffer -> Respond -> IO ResponseReceived
createAtProvider WebEnv {weStore, weConfig} provider CreateRequest {crPriceId, crOfferId, crMethod, crCodeHash} priced respond =
  do
      invId <- newInvoiceId
      -- truncate once, so the response, the row and the provider get the same value
      now <- truncateToSecond <$> getCurrentTime
      let expiresAt = addUTCTime (invoiceWindow weConfig crMethod) now
          draft = OrderDraft {odAmount = poAmount priced, odCurrency = poCurrency priced}
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

secondsPerMinute :: Int
secondsPerMinute = 60

-- | The expiry shown and stored must come from the same key the provider sets the invoice's real expiry from.
invoiceWindow :: ServiceConfig -> ServicePaymentMethod -> NominalDiffTime
invoiceWindow cfg = \case
  SPMCard CPStripe -> minutes (maybe defaultSessionMinutes sSessionMinutes (stripe cfg))
  SPMCrypto _ -> minutes (maybe defaultExpiryMinutes bExpiryMinutes (btcpay cfg))
  where
    minutes n = fromIntegral (secondsPerMinute * n)

providerFor :: WebEnv -> ServicePaymentMethod -> Maybe Provider
providerFor env method = providerNamed env (providerOf method)

providerNamed :: WebEnv -> PaymentProvider -> Maybe Provider
providerNamed WebEnv {weProviders} provider = find ((== provider) . pProvider) weProviders

providerOf :: ServicePaymentMethod -> PaymentProvider
providerOf = \case
  SPMCard CPStripe -> PPStripe
  SPMCrypto _ -> PPCrypto

parseCreateRequest :: LB.ByteString -> Maybe CreateRequest
parseCreateRequest body = case J.decode body of
  Just (J.Object o) -> do
    crPriceId <- BadgePriceId <$> textField o "priceId"
    crOfferId <- offerField o
    crMethod <- methodFromText =<< textField o "method"
    crCodeHash <- parseCodeHash =<< textField o "codeHash"
    pure CreateRequest {crPriceId, crOfferId, crMethod, crCodeHash}
  _ -> Nothing
  where
    textField o k = case KM.lookup k o of
      Just (J.String t) | not (T.null t) -> Just t
      _ -> Nothing
    offerField o = case KM.lookup "offerId" o of
      Nothing -> Just Nothing
      Just J.Null -> Just Nothing
      Just (J.String t) | not (T.null t) -> Just (Just (BadgeOfferId t))
      _ -> Nothing

methodFromText :: Text -> Maybe ServicePaymentMethod
methodFromText = \case
  "card" -> Just (SPMCard CPStripe)
  "btc" -> Just (SPMCrypto CCBtc)
  "xmr" -> Just (SPMCrypto CCXmr)
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
    deliver = case providerNamed env provider of
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

webSettings :: ListenerConfig -> Warp.Settings
webSettings ListenerConfig {lHost, lPort} =
  Warp.setHost (fromString (T.unpack lHost))
    . Warp.setPort lPort
    . Warp.setOnExceptionResponse (const internalError)
    $ Warp.defaultSettings

runWebListener :: WebEnv -> IO ()
runWebListener env = do
  warnIfHeaderTrustIsExposed (listenerConfig env)
  Warp.runSettings (webSettings (listenerConfig env)) (webApp env)

warnIfHeaderTrustIsExposed :: ListenerConfig -> IO ()
warnIfHeaderTrustIsExposed ListenerConfig {lHost, lTrustForwardedFor} =
  when (lTrustForwardedFor && not (isLoopbackHost lHost)) $
    logWarn ("store service: trust_forwarded_for is on and the listener binds " <> lHost <> "; every caller can then choose its own rate limit bucket unless a proxy sets the header")

isLoopbackHost :: Text -> Bool
isLoopbackHost h = h `elem` ["127.0.0.1", "::1", "localhost"]
