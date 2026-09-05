{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Web.Server
  ( WebEnv (..),
    Limit (..),
    newWebEnv,
    takeToken,
    maxBuckets,
    maxWebhookBytes,
    webApp,
    webSettings,
    runWebListener,
    holdMicros,
    readLimit,
  )
where

import BadgeService.Catalog (PricedOffer (..), priceOffer)
import BadgeService.Config (BTCPayConfig (..), ListenerConfig (..), ServiceConfig (..), SpeedPolicy (..), defaultExpiryMinutes)
import BadgeService.Poller (ReadHints, queueReadHint)
import BadgeService.Providers (OrderDraft (..), Provider (..), ProviderError (..), ProviderInvoice (..), WebhookError (..))
import BadgeService.Store.Invoices (CreateError (..), InvoicePayment (..), InvoiceRow (..), NewInvoice (..), cancelOpenInvoice, codeHashExists, createInvoiceRows, cryptoCurrencyText, getInvoice, getInvoiceByProviderRef, invoiceStatusText, newInvoiceId, paymentHolds, readCatalogRows, textToInvoiceStatus, truncateToSecond)
import BadgeService.Waiters (Seen, Waiters, awaitStatus, publish)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Logger.Simple (logError, logInfo, logWarn)
import Control.Monad (when)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (toLower)
import Data.List (find, isPrefixOf, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word16)
import Network.HTTP.Types (Header, Status, hCacheControl, hContentType, status200, status400, status404, status405, status409, status413, status429, status500, status503)
import Network.Socket (SockAddr (..), hostAddress6ToTuple, hostAddressToTuple)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Numeric (showHex)
import Simplex.Chat.Badges.Types (BadgeOfferId (..), BadgePriceId (..))
import Simplex.Chat.PaymentService.Types (CardProvider (..), CryptoCurrency (..), CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..), PaymentProvider (..), ServicePaymentDestination (..), ServicePaymentMethod (..))
import Simplex.Messaging.Agent.Store.Common (DBStore)
import Simplex.Messaging.Encoding.String (textEncode)
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (pathSeparator, takeExtension, (</>))
import Text.Read (readMaybe)

data WebEnv = WebEnv
  { weStore :: DBStore,
    weConfig :: ServiceConfig,
    weWaiters :: Waiters,
    weProviders :: [Provider],
    weHoldMicros :: Int,
    weHints :: ReadHints,
    weBuckets :: TVar (Map Text Bucket)
  }

newWebEnv :: DBStore -> ServiceConfig -> Waiters -> ReadHints -> [Provider] -> IO WebEnv
newWebEnv weStore weConfig weWaiters weHints weProviders = do
  weBuckets <- newTVarIO Map.empty
  pure WebEnv {weStore, weConfig, weWaiters, weProviders, weHoldMicros = holdMicros, weHints, weBuckets}

listenerConfig :: WebEnv -> ListenerConfig
listenerConfig WebEnv {weConfig} = listener weConfig

type Respond = Response -> IO ResponseReceived

-- | Short enough to stay under the idle timeout of any proxy in front of us.
holdMicros :: Int
holdMicros = 30 * 1000000

readLimit :: Limit
readLimit = Limit {lmName = "read", lmPerMinute = 60}

createLimit :: Limit
createLimit = Limit {lmName = "create", lmPerMinute = 5}

maxBodyBytes :: Int
maxBodyBytes = 8192

-- | The only limit on that route, which carries no rate limit of its own.
maxWebhookBytes :: Int
maxWebhookBytes = 64 * 1024

sha256Bytes :: Int
sha256Bytes = 32

codeHashChars :: Int
codeHashChars = 43

data Limit = Limit {lmName :: Text, lmPerMinute :: Int}
  deriving (Eq, Show)

data Bucket = Bucket {bkCount :: Int, bkStarted :: UTCTime}

limitWindow :: NominalDiffTime
limitWindow = 60

maxBuckets :: Int
maxBuckets = 8192

-- | A flood of distinct clients inside one minute leaves every bucket fresh, so sweeping expired
-- ones frees nothing, and dropping the map would clear every count, leaving a flooder unmetered.
-- The least-used half goes instead, keeping the clients nearest their limit.
reclaim :: UTCTime -> Map Text Bucket -> Map Text Bucket
reclaim now buckets
  | Map.size buckets < maxBuckets = buckets
  | Map.size live * 2 <= maxBuckets = live
  | otherwise = Map.fromList (drop (Map.size live `div` 2) (sortOn (bkCount . snd) (Map.toList live)))
  where
    live = Map.filter (fresh now) buckets

fresh :: UTCTime -> Bucket -> Bool
fresh now Bucket {bkStarted} = diffUTCTime now bkStarted < limitWindow

takeToken :: WebEnv -> Limit -> Text -> IO (Maybe Int)
takeToken WebEnv {weBuckets} Limit {lmName, lmPerMinute} client = do
  now <- getCurrentTime
  atomically $ do
    buckets <- reclaim now <$> readTVar weBuckets
    let key = lmName <> "\t" <> client
        count bucket = writeTVar weBuckets (Map.insert key bucket buckets) >> pure Nothing
        -- write the reclaimed map back here too, or a refused request redoes the
        -- filtering every time and the map stays at the limit
        refuse seconds = writeTVar weBuckets buckets >> pure (Just seconds)
    case Map.lookup key buckets of
      Just bucket@Bucket {bkCount, bkStarted}
        | fresh now bucket ->
            if bkCount < lmPerMinute
              then count bucket {bkCount = bkCount + 1}
              else refuse (secondsLeft now bkStarted)
      _ -> count Bucket {bkCount = 1, bkStarted = now}
  where
    secondsLeft :: UTCTime -> UTCTime -> Int
    secondsLeft now started = max 1 (ceiling (limitWindow - diffUTCTime now started))

limited :: WebEnv -> Limit -> Request -> Respond -> IO ResponseReceived -> IO ResponseReceived
limited env limit req respond action =
  takeToken env limit (clientKey env req) >>= \case
    Nothing -> action
    Just seconds -> respond (rateLimited seconds)

-- | @X-Forwarded-For@ is trusted only where the operator says a proxy sets it. The header is
-- a list the caller can prepend to, and a proxy appends the peer it saw, so the last entry is
-- the one our proxy wrote. Every line is joined first: a proxy that adds a second header line
-- rather than editing the first would otherwise leave the caller's line the one we read.
clientKey :: WebEnv -> Request -> Text
clientKey env req = fromMaybe (peerText (remoteHost req)) forwarded
  where
    forwarded
      | not (lTrustForwardedFor (listenerConfig env)) = Nothing
      | otherwise = case reverse (concatMap entries (requestHeaders req)) of
          (ip : _) | isIpAddress ip -> Just ip
          _ -> Nothing
    entries (name, raw)
      | name /= "x-forwarded-for" = []
      | otherwise = either (const []) (filter (not . T.null) . map T.strip . T.splitOn ",") (decodeUtf8' raw)

isIpAddress :: Text -> Bool
isIpAddress t = isIPv4 t || isIPv6 t

asciiDigit :: Char -> Bool
asciiDigit c = c >= '0' && c <= '9'

-- Data.Char.isDigit and isHexDigit accept Unicode digits, which are not addresses.
asciiHexDigit :: Char -> Bool
asciiHexDigit c = asciiDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | A leading zero is refused, so "010.0.0.1" cannot be a second key for "10.0.0.1".
isIPv4 :: Text -> Bool
isIPv4 t = case T.splitOn "." t of
  [a, b, c, d] -> all octet [a, b, c, d]
  _ -> False
  where
    octet s =
      not (T.null s)
        && T.length s <= 3
        && T.all asciiDigit s
        && (T.length s == 1 || T.head s /= '0')
        && maybe False (<= 255) (readMaybe (T.unpack s) :: Maybe Int)

isIPv6 :: Text -> Bool
isIPv6 t
  | T.any (== '%') t = False
  | otherwise = case T.splitOn "::" t of
      [full] -> groupCount True (segments full) == Just 8
      [before, after] -> case (groupCount False (segments before), groupCount True (segments after)) of
        (Just b, Just a) -> b + a <= 7 -- "::" stands for at least one omitted group
        _ -> False
      _ -> False
  where
    segments s = if T.null s then [] else T.splitOn ":" s

groupCount :: Bool -> [Text] -> Maybe Int
groupCount endsAddress gs
  | null gs = Just 0
  | endsAddress && isIPv4 (last gs) = (+ 2) <$> plainGroups (init gs)
  | otherwise = plainGroups gs
  where
    plainGroups ps = if all hexGroup ps then Just (length ps) else Nothing
    hexGroup g = not (T.null g) && T.length g <= 4 && T.all asciiHexDigit g

peerText :: SockAddr -> Text
peerText = \case
  SockAddrInet _ addr ->
    let (a, b, c, d) = hostAddressToTuple addr
     in T.intercalate "." (map tshowInt [a, b, c, d])
  SockAddrInet6 _ _ addr _ ->
    let (a, b, c, d, e, f, g, h) = hostAddress6ToTuple addr
     in T.intercalate ":" (map hex16 [a, b, c, d, e, f, g, h])
  SockAddrUnix p -> "unix:" <> T.pack p
  where
    tshowInt :: Show a => a -> Text
    tshowInt = T.pack . show
    hex16 :: Word16 -> Text
    hex16 w = T.pack (showHex w "")

apiHeaders :: [Header]
apiHeaders = [(hContentType, "application/json"), (hCacheControl, "no-store")]

jsonResponse :: Status -> J.Value -> Response
jsonResponse st = responseLBS st apiHeaders . J.encode

jsonError :: Status -> Text -> Response
jsonError st code = jsonResponse st (J.object ["error" .= code])

notFound :: Response
notFound = jsonError status404 "not_found"

badRequest :: Response
badRequest = jsonError status400 "bad_request"

catalogChanged :: Response
catalogChanged = jsonError status400 "catalog_changed"

codeConflict :: Response
codeConflict = jsonError status409 "code_conflict"

providerUnavailable :: Response
providerUnavailable = jsonError status503 "provider_unavailable"

rateLimited :: Int -> Response
rateLimited seconds =
  responseLBS
    status429
    (("Retry-After", B.pack (show seconds)) : apiHeaders)
    (J.encode (J.object ["error" .= ("rate_limited" :: Text)]))

methodNotAllowed :: ByteString -> Response
methodNotAllowed allow = responseLBS status405 (("Allow", allow) : apiHeaders) (J.encode (J.object ["error" .= ("method_not_allowed" :: Text)]))

internalError :: Response
internalError = jsonError status500 "internal"

webhookOk, webhookRefused, webhookTooLarge :: Response
webhookOk = webhookResponse status200
webhookRefused = webhookResponse status400
webhookTooLarge = webhookResponse status413

webhookResponse :: Status -> Response
webhookResponse st = responseLBS st [(hCacheControl, "no-store")] ""

webApp :: WebEnv -> Application
webApp env req respond = case pathInfo req of
  [] -> only "GET" $ serveStatic env ["index.html"] respond
  "assets" : rest -> only "GET" $ serveStatic env ("assets" : rest) respond
  ["sw.js"] -> only "GET" $ serveStatic env ["sw.js"] respond
  ["api", "invoice"] -> only "POST" $ limited env createLimit req respond $ createInvoiceHandler env req respond
  ["api", "invoice", iid] -> only "GET" $ limited env readLimit req respond $ readInvoiceHandler env (InvoiceId iid) req respond
  ["api", "invoice", iid, "cancel"] -> only "POST" $ limited env createLimit req respond $ cancelInvoiceHandler env (InvoiceId iid) respond
  -- the one API route with no rate limit: refusing a delivery is worse than serving it, since BTCPay
  -- retries what it cannot deliver. The work per request is bounded instead, by `maxWebhookBytes`
  -- before anything parses and by a signature check that reaches no database.
  ["webhooks", "btcpay"] -> only "POST" $ webhookHandler env req respond
  _ -> respond notFound
  where
    only method action
      | requestMethod req == method = action
      | otherwise = respond (methodNotAllowed method)

-- | Canonicalises and refuses anything outside @static_dir@. Looking for @".."@ would
-- miss separators WAI has already decoded, an absolute path, and a symlink out of the
-- tree.
serveStatic :: WebEnv -> [Text] -> Respond -> IO ResponseReceived
serveStatic env segments respond =
  resolveInside (lStaticDir (listenerConfig env)) segments >>= \case
    Nothing -> respond notFound
    Just (root, file) -> respond (responseFile status200 (staticHeaders root file) file Nothing)

-- | The canonical root alongside the file, since what may be cached forever is decided from
-- where the file really is, not from how the request spelled its way there.
resolveInside :: FilePath -> [Text] -> IO (Maybe (FilePath, FilePath))
resolveInside dir segments = either ioFailed id <$> E.try attempt
  where
    -- a NUL byte in the path makes the calls below throw, which is a refusal like any
    -- other
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

-- | Assets live under the build hash and can be cached forever. The page at @\/@ cannot:
-- a browser caching it would go on asking for assets from a build we no longer have.
staticHeaders :: FilePath -> FilePath -> [Header]
staticHeaders root file =
  [ (hContentType, contentTypeFor file),
    (hCacheControl, if hashedAsset then "public, max-age=31536000, immutable" else "no-cache")
  ]
  where
    -- read off the resolved path, not the request: `/assets/<hash>/%2e%2e/%2e%2e/index.html`
    -- resolves to the shell, and a year in a shared cache is the skew `sw.js` exists to prevent
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
        -- awaitStatus decides when to answer, not what. Settlement publishes after it
        -- commits, so the row read below is at least as new as whatever woke us.
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
        -- a payment recorded before this request arrived cannot wake it: the counter a hold
        -- watches starts at zero. Holding then leaves the page saying "waiting for the payment"
        -- for the whole timeout with the money already in.
        samePayment = case paidParam req of
          Nothing -> True -- a client that does not say what it saw keeps the old behaviour
          Just seen -> seen == paymentMark row
    readSeen :: IO Seen
    readSeen = maybe (ISOpen, ("", False)) (\row -> (irStatus row, paymentMark row)) <$> getInvoice weStore invId

-- | The figure a browser drew and the provider's verdict it drew it under. The verdict counts
-- as much as the figure: Monero reports a payment as confirming while its figures are zero.
paymentMark :: InvoiceRow -> (Text, Bool)
paymentMark InvoiceRow {irPayment} =
  (maybe "" (fromMaybe "" . ipCryptoAmount) irPayment, maybe False ipPaidInFull irPayment)

-- | The provider is told first: if that fails the invoice stays open at both ends, which is
-- recoverable, where cancelling here first would leave an address the buyer can still pay into
-- and nothing watching it.
cancelInvoiceHandler :: WebEnv -> InvoiceId -> Respond -> IO ResponseReceived
cancelInvoiceHandler env@WebEnv {weStore} invId respond =
  getInvoice weStore invId >>= \case
    Nothing -> respond notFound
    Just InvoiceRow {irStatus} | irStatus /= ISOpen -> respond (jsonError status409 "not_open")
    -- an invoice with money in it is awaiting confirmation, not waiting to be paid:
    -- invalidating it at the provider would strand what the buyer already sent
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

-- | What the browser last rendered of the payment: the figure it showed and the provider's
-- verdict it showed it under. Absent from an older build's request, which then holds as before.
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

-- | Greenfield reports no confirmation count, so the page can only state what settlement
-- needs, which is the store's speed policy and ours to know.
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
      <> destinationPairs irDestination
      <> paidPairs
      <> confirmationPairs
  where
    confirmationPairs = case irDestination of
      SPDCrypto {} -> maybe [] (\n -> ["requiredConfirmations" .= n]) confirmations
      SPDCard {} -> []
    paidPairs = maybe [] paymentPairs irPayment
    paymentPairs p =
      concat
        [ maybe [] (\a -> ["amountPaid" .= amountJSON a]) (ipAmount p),
          maybe [] (\a -> ["cryptoAmountPaid" .= a]) (ipCryptoAmount p),
          maybe [] (\a -> ["cryptoAmountDue" .= a]) (ipCryptoDue p),
          ["paidInFull" .= ipPaidInFull p],
          ["settledAt" .= ipUpdatedAt p | irStatus == ISPaid]
        ]

destinationPairs :: ServicePaymentDestination -> [Pair]
destinationPairs = \case
  SPDCard _ url -> ["clientSecret" .= url]
  SPDCrypto currency address cryptoAmount ->
    [ "address" .= address,
      "cryptoAmount" .= cryptoAmount,
      "cryptoCurrency" .= cryptoCurrencyText currency
    ]

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

-- | Once the provider call succeeds an invoice exists at BTCPay, and any path below that
-- does not write our rows leaves it stranded. There is no idempotency key, so these log
-- lines are all an operator has: they carry the provider's own id, never ours, which is a
-- bearer token.
createAtProvider :: WebEnv -> Provider -> CreateRequest -> PricedOffer -> Respond -> IO ResponseReceived
createAtProvider WebEnv {weStore, weConfig} provider CreateRequest {crPriceId, crOfferId, crMethod, crCodeHash} priced respond =
  do
      invId <- newInvoiceId
      -- truncate once, so the response, the row and the provider get the same value
      now <- truncateToSecond <$> getCurrentTime
      let expiresAt = addUTCTime (invoiceWindow weConfig) now
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
      <> destinationPairs destination

invoiceWindow :: ServiceConfig -> NominalDiffTime
invoiceWindow ServiceConfig {btcpay} = fromIntegral (60 * maybe defaultExpiryMinutes bExpiryMinutes btcpay)

providerFor :: WebEnv -> ServicePaymentMethod -> Maybe Provider
providerFor env method = providerNamed env (providerOf method)

providerNamed :: WebEnv -> PaymentProvider -> Maybe Provider
providerNamed WebEnv {weProviders} provider = find ((== provider) . pProvider) weProviders

providerOf :: ServicePaymentMethod -> PaymentProvider
providerOf = \case
  SPMCard CPStripe -> PPStripe
  SPMCrypto _ -> PPCrypto

-- | Stops reading at @cap@ rather than reading it all and measuring, and returns the
-- bytes exactly as they arrived, since the webhook signature is over those bytes.
readBoundedBody :: Int -> Request -> IO (Maybe LB.ByteString)
readBoundedBody cap req = go 0 []
  where
    go :: Int -> [ByteString] -> IO (Maybe LB.ByteString)
    go read' acc = do
      chunk <- getRequestBodyChunk req
      if BS.null chunk
        then pure (Just (LB.fromChunks (reverse acc)))
        else
          let read'' = read' + BS.length chunk
           in if read'' > cap then pure Nothing else go read'' (chunk : acc)

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

-- | The re-encode is belt and braces: the last character of a 43-character base64 string
-- has two bits no digest byte uses, so a lax decoder would accept four spellings of
-- the same digest. base64-bytestring 1.2 rejects them, but our bound allows ones that do
-- not.
parseCodeHash :: Text -> Maybe ByteString
parseCodeHash t
  | T.length t /= codeHashChars = Nothing
  | otherwise = case B64U.decode (encodeUtf8 (t <> "=")) of
      Right bytes | BS.length bytes == sha256Bytes, canonical bytes == t -> Just bytes
      _ -> Nothing
  where
    canonical = T.filter (/= '=') . safeDecodeUtf8 . B64U.encode

-- | Verifies, resolves the reference, queues a read, answers. It never calls the provider,
-- settles, opens a transaction or waits. BTCPay retries on a 5xx, so anything thrown below is
-- caught and answered 200 anyway.
webhookHandler :: WebEnv -> Request -> Respond -> IO ResponseReceived
webhookHandler env@WebEnv {weStore, weHints} req respond =
  E.try deliver >>= \case
    Right received -> pure received
    Left e -> case E.fromException e of
      Just async' -> E.throwIO (async' :: E.SomeAsyncException)
      Nothing -> do
        logError (route <> ": the delivery could not be handled, so no read was queued: " <> tshow (e :: E.SomeException))
        respond webhookOk
  where
    deliver :: IO ResponseReceived
    deliver = case providerNamed env PPCrypto of
      Nothing -> do
        logWarn (route <> ": no crypto adapter is configured, so every delivery is refused")
        respond webhookRefused
      Just p ->
        readBoundedBody maxWebhookBytes req >>= \case
          Nothing -> logInfo (route <> ": body over the " <> tshow maxWebhookBytes <> "-byte cap") >> respond webhookTooLarge
          Just body ->
            -- BTCPay signs the indented payload it sent, so parsing and re-encoding here
            -- would fail the signature on every event
            case pVerifyWebhook p (requestHeaders req) (LB.toStrict body) of
              Left (WebhookError e) -> refuse e
              Right Nothing -> ignore body "nothing this service acts on"
              Right (Just ref) -> resolve body ref
    route :: Text
    route = "POST /webhooks/btcpay"
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
          -- provider_ref is unique table-wide, not per provider, so without this a
          -- collision could credit the wrong order
          | irProvider /= PPCrypto -> ignore body ("provider_ref " <> ref <> " belongs to " <> tshow irProvider)
          | otherwise -> queue body ref
    queue :: LB.ByteString -> Text -> IO ResponseReceived
    queue body ref = do
      queued <- queueReadHint weHints ref
      if queued
        then logEvent body ("queued a read of " <> ref)
        else -- not an error: the next pass finds this invoice anyway, and waiting for room
          logWarn (route <> ": " <> fromMaybe noEventType (eventTypeOf body) <> ", the read queue is full, so the read of " <> ref <> " waits for the next pass")
      respond webhookOk

-- | For the log only. The adapter returns @Right Nothing@ both for an event we do not
-- act on and for one it could not read, and being pure it cannot log the difference, so
-- a rename of @type@ or @invoiceId@ at BTCPay would make every delivery look successful.
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

-- | Trusting the header hands the rate limiter's key to whoever wrote it, which is only safe
-- where a proxy is the one writing it. Nothing here can prove a proxy is in front, so a bind
-- that is not loopback says so at startup rather than silently counting nobody.
warnIfHeaderTrustIsExposed :: ListenerConfig -> IO ()
warnIfHeaderTrustIsExposed ListenerConfig {lHost, lTrustForwardedFor} =
  when (lTrustForwardedFor && not (isLoopbackHost lHost)) $
    logWarn ("badge service: trust_forwarded_for is on and the listener binds " <> lHost <> "; every caller can then choose its own rate limit bucket unless a proxy sets the header")

isLoopbackHost :: Text -> Bool
isLoopbackHost h = h `elem` ["127.0.0.1", "::1", "localhost"]
