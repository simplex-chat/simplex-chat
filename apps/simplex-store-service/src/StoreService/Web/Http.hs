{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Web.Http
  ( HttpEnv (..),
    newHttpEnv,
    Respond,
    Limit (..),
    Bucket (..),
    takeFrom,
    takeToken,
    limited,
    only,
    CheckoutClient,
    checkoutClient,
    sockHost,
    maxBuckets,
    readLimit,
    createLimit,
    searchLimit,
    previewLimit,
    codeLimit,
    findLimit,
    recordsLimit,
    checkoutLimit,
    invoicesLimit,
    limitedCheckout,
    holdMicros,
    maxHolds,
    withHoldSlot,
    maxSearches,
    searchSlot,
    holdsFull,
    jsonResponse,
    jsonError,
    notFound,
    badRequest,
    invalidName,
    invalidLink,
    codeConflict,
    providerUnavailable,
    termsUnavailable,
    rateLimited,
    methodNotAllowed,
    internalError,
    readBoundedBody,
    maxBodyBytes,
    nameBodyBytes,
    parseCodeHash,
    invoicePairs,
    invoiceAt,
    expiryMoment,
    destinationPairs,
    amountJSON,
    confirmationsFor,
    invoiceWindow,
    providerFor,
    providerNamed,
  )
where

import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (foldM, when)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.Aeson.Types (Pair)
import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word8)
import Network.HTTP.Types (Header, Method, Status, hCacheControl, hContentType, status400, status404, status405, status409, status429, status500, status503)
import Network.Socket (SockAddr (..), hostAddressToTuple)
import Network.Wai
import Numeric (showHex)
import Simplex.Chat.PaymentService.Types (CardProvider (..), CurrencyAmount (..), InvoiceStatus (..), PaymentProvider (..), ServicePaymentDestination (..), ServicePaymentMethod (..))
import Simplex.Messaging.Encoding.String (strDecode, strEncode, textEncode)
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Util (safeDecodeUtf8, stateTVar', tshow)
import StoreService.Config (BTCPayConfig (..), ListenerConfig (..), ServiceConfig (..), SpeedPolicy (..), StripeConfig (..), defaultExpiryMinutes, defaultSessionMinutes)
import StoreService.Providers (Provider (..))
import StoreService.Store.Invoices (InvoicePayment (..), InvoiceRow (..), paymentHolds)

data HttpEnv = HttpEnv
  { heBuckets :: TVar (Map Text Bucket),
    heTrustForwardedFor :: Bool,
    -- | the long polls held now, of every client
    heHolds :: TVar Int,
    -- | the name searches reading the chain now, of every client
    heSearches :: TVar Int
  }

newHttpEnv :: ListenerConfig -> IO HttpEnv
newHttpEnv ListenerConfig {lTrustForwardedFor} = do
  heBuckets <- newTVarIO Map.empty
  heHolds <- newTVarIO 0
  heSearches <- newTVarIO 0
  pure HttpEnv {heBuckets, heTrustForwardedFor = lTrustForwardedFor, heHolds, heSearches}

type Respond = Response -> IO ResponseReceived

-- | Short enough to stay under the idle timeout of any proxy in front of us.
holdMicros :: Int
holdMicros = 30 * 1000000

-- | Held long polls of every client together, each a connection and a waiter.
maxHolds :: Int
maxHolds = 500

-- | When every hold is taken, how soon a poll is asked to come back.
holdsFullSeconds :: Int
holdsFullSeconds = 5

-- | The hold in one of the shared slots, Nothing when every slot is taken.
withHoldSlot :: HttpEnv -> IO a -> IO (Maybe a)
withHoldSlot HttpEnv {heHolds} = inSlot heHolds maxHolds

holdsFull :: Response
holdsFull = rateLimited holdsFullSeconds

-- | Name searches of every client reading the chain at once, so a flood cannot multiply into the chain node's load.
maxSearches :: Int
maxSearches = 16

-- | A search's chain reads, run while a slot is free; Nothing when none is.
searchSlot :: HttpEnv -> IO a -> IO (Maybe a)
searchSlot HttpEnv {heSearches} = inSlot heSearches maxSearches

inSlot :: TVar Int -> Int -> IO a -> IO (Maybe a)
inSlot slots most action = bracket takeSlot releaseSlot $ \taken -> if taken then Just <$> action else pure Nothing
  where
    takeSlot = atomically $ stateTVar' slots $ \n -> if n < most then (True, n + 1) else (False, n)
    releaseSlot taken = when taken $ atomically $ modifyTVar' slots (subtract 1)

readLimit, createLimit, searchLimit, previewLimit, codeLimit, findLimit, recordsLimit :: Limit
readLimit = Limit {lmName = "read", lmPerMinute = 60}
createLimit = Limit {lmName = "create", lmPerMinute = 5}
searchLimit = Limit {lmName = "search", lmPerMinute = 30}
previewLimit = Limit {lmName = "preview", lmPerMinute = 20}
codeLimit = Limit {lmName = "code", lmPerMinute = 10}
findLimit = Limit {lmName = "find", lmPerMinute = 5}
recordsLimit = Limit {lmName = "records", lmPerMinute = 10}

checkoutLimit :: Limit
checkoutLimit = Limit {lmName = "checkout", lmPerMinute = 5}

-- | Checkouts of every client together, each a provider invoice or card PaymentIntent, so a flood from many addresses
-- still meets a ceiling.
invoicesLimit :: ServicePaymentMethod -> Limit
invoicesLimit = \case
  SPMCard CPStripe -> Limit {lmName = "card invoices", lmPerMinute = 60}
  SPMCrypto _ -> Limit {lmName = "crypto invoices", lmPerMinute = 30}

maxBodyBytes :: Int
maxBodyBytes = 8192

-- | The cap of the names endpoints whose bodies carry a sealed name code.
nameBodyBytes :: Int
nameBodyBytes = 16384

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
maxBuckets = 32768

reclaim :: UTCTime -> Map Text Bucket -> Map Text Bucket
reclaim now buckets
  | Map.size buckets < maxBuckets = buckets
  | Map.size live * 2 <= maxBuckets = live
  | otherwise = Map.fromList (drop (Map.size live `div` 2) (sortOn (bkCount . snd) (Map.toList live)))
  where
    live = Map.filter (fresh now) buckets

fresh :: UTCTime -> Bucket -> Bool
fresh now Bucket {bkStarted} = diffUTCTime now bkStarted < limitWindow

-- | A request against a minute's limit: the bucket after it, or how long until the minute ends.
takeFrom :: Int -> UTCTime -> Maybe Bucket -> Either NominalDiffTime Bucket
takeFrom limit now = \case
  Just b@Bucket {bkCount, bkStarted}
    | fresh now b -> if bkCount < limit then Right b {bkCount = bkCount + 1} else Left (limitWindow - diffUTCTime now bkStarted)
  _ -> Right Bucket {bkCount = 1, bkStarted = now}

takeToken :: HttpEnv -> Limit -> Text -> IO (Maybe Int)
takeToken env limit client = takeTokens env [(limit, client)]

takeTokens :: HttpEnv -> [(Limit, Text)] -> IO (Maybe Int)
takeTokens HttpEnv {heBuckets} limits = do
  now <- getCurrentTime
  atomically $ do
    buckets <- reclaim now <$> readTVar heBuckets
    let takeOne taken (Limit {lmName, lmPerMinute}, client) =
          let key = lmName <> "\t" <> client
           in (\bucket -> Map.insert key bucket taken) <$> takeFrom lmPerMinute now (Map.lookup key taken)
        count taken = writeTVar heBuckets taken >> pure Nothing
        -- write the reclaimed map back here too, or a refused request redoes the filtering every time and the map stays at the limit
        refuse seconds = writeTVar heBuckets buckets >> pure (Just seconds)
    either (refuse . max 1 . ceiling) count (foldM takeOne buckets limits)

limited :: HttpEnv -> Limit -> Request -> Respond -> IO ResponseReceived -> IO ResponseReceived
limited env limit req respond action =
  takeToken env limit (clientKey hostPrefix env req) >>= \case
    Nothing -> action
    Just seconds -> respond (rateLimited seconds)

-- | The client network's limit and the one every client shares, taken just before the provider is asked, so refused requests do not use them.
limitedCheckout :: HttpEnv -> CheckoutClient -> ServicePaymentMethod -> (Response -> IO a) -> IO a -> IO a
limitedCheckout env (CheckoutClient client) method refuse action =
  takeTokens env [(checkoutLimit, client), (invoicesLimit method, "")] >>= maybe action (refuse . rateLimited)

only :: Method -> Request -> Respond -> IO ResponseReceived -> IO ResponseReceived
only method req respond action
  | requestMethod req == method = action
  | otherwise = respond (methodNotAllowed method)

data KeyPrefix = KeyPrefix {kpOctets :: Int, kpGroups :: Int}

-- | An IPv6 client is keyed by its /64, as one host holds a whole /64.
hostPrefix :: KeyPrefix
hostPrefix = KeyPrefix {kpOctets = 4, kpGroups = 4}

networkPrefix :: KeyPrefix
networkPrefix = KeyPrefix {kpOctets = 3, kpGroups = 3}

newtype CheckoutClient = CheckoutClient Text
  deriving (Eq, Show)

checkoutClient :: HttpEnv -> Request -> CheckoutClient
checkoutClient env = CheckoutClient . clientKey networkPrefix env

-- | Every line is joined first, so a second header line a caller adds cannot leave its own line the one we read.
clientKey :: KeyPrefix -> HttpEnv -> Request -> Text
clientKey prefix HttpEnv {heTrustForwardedFor} req = maybe peerKey (hostKey prefix) forwarded
  where
    forwarded
      | not heTrustForwardedFor = Nothing
      | otherwise = case reverse (concatMap entries (requestHeaders req)) of
          (ip : _) -> forwardedHost ip
          _ -> Nothing
    entries (name, raw)
      | name /= "x-forwarded-for" = []
      | otherwise = either (const []) (filter (not . T.null) . map T.strip . T.splitOn ",") (decodeUtf8' raw)
    forwardedHost ip = case strDecode s of
      Right h@(THIPv4 _) | strEncode h == s -> Just h
      Right h@(THIPv6 _) -> Just h
      _ -> Nothing
      where
        s = encodeUtf8 ip
    peerKey = maybe (tshow $ remoteHost req) (hostKey prefix) $ sockHost $ remoteHost req

sockHost :: SockAddr -> Maybe TransportHost
sockHost = \case
  SockAddrInet _ a -> Just $ THIPv4 $ hostAddressToTuple a
  SockAddrInet6 _ _ a _ -> Just $ THIPv6 a
  SockAddrUnix _ -> Nothing

-- | The prefix of an address, or of the IPv4 address in an IPv4-mapped one.
hostKey :: KeyPrefix -> TransportHost -> Text
hostKey KeyPrefix {kpOctets, kpGroups} = \case
  THIPv4 (a, b, c, d) -> v4Key [a, b, c, d]
  THIPv6 (0, 0, 0xffff, w) -> v4Key [fromIntegral $ w `shiftR` n | n <- [24, 16, 8, 0]]
  THIPv6 (w0, w1, w2, w3) -> T.intercalate ":" (map (T.pack . (`showHex` "")) $ take kpGroups $ concatMap halves [w0, w1, w2, w3]) <> "::"
  host -> safeDecodeUtf8 $ strEncode host
  where
    v4Key :: [Word8] -> Text
    v4Key = T.intercalate "." . map tshow . take kpOctets
    halves w = [w `shiftR` 16, w .&. 0xffff]

apiHeaders :: [Header]
apiHeaders = [(hContentType, "application/json"), (hCacheControl, "no-store")]

jsonResponse :: J.ToJSON a => Status -> a -> Response
jsonResponse st = responseLBS st apiHeaders . J.encode

jsonError :: Status -> Text -> Response
jsonError st code = jsonResponse st (J.object ["error" .= code])

notFound :: Response
notFound = jsonError status404 "not_found"

badRequest :: Response
badRequest = jsonError status400 "bad_request"

invalidName :: Response
invalidName = jsonError status400 "invalid_name"

invalidLink :: Response
invalidLink = jsonError status400 "invalid_link"

codeConflict :: Response
codeConflict = jsonError status409 "code_conflict"

providerUnavailable :: Response
providerUnavailable = jsonError status503 "provider_unavailable"

termsUnavailable :: Int -> Response
termsUnavailable = retryAfterError status503 "terms_unavailable"

rateLimited :: Int -> Response
rateLimited = retryAfterError status429 "rate_limited"

retryAfterError :: Status -> Text -> Int -> Response
retryAfterError st code seconds =
  responseLBS
    st
    (("Retry-After", B.pack (show seconds)) : apiHeaders)
    (J.encode (J.object ["error" .= code]))

methodNotAllowed :: Method -> Response
methodNotAllowed allow = responseLBS status405 (("Allow", allow) : apiHeaders) (J.encode (J.object ["error" .= ("method_not_allowed" :: Text)]))

internalError :: Response
internalError = jsonError status500 "internal"

-- | Returns the bytes exactly as they arrived, since the webhook signature is over those bytes.
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

-- | The last character of a 43-character base64 string has two bits no digest byte uses, so a lax
-- decoder would accept four spellings of the same digest, which the re-encode check rejects.
parseCodeHash :: Text -> Maybe ByteString
parseCodeHash t
  | T.length t /= codeHashChars = Nothing
  | otherwise = case B64U.decode (encodeUtf8 (t <> "=")) of
      Right bytes | BS.length bytes == sha256Bytes, canonical bytes == t -> Just bytes
      _ -> Nothing
  where
    canonical = T.filter (/= '=') . safeDecodeUtf8 . B64U.encode

-- | An open name invoice past its expiry with no money received reads expired before the sweep marks it.
invoiceAt :: UTCTime -> InvoiceRow -> InvoiceRow
invoiceAt now row = case expiryMoment row of
  Just expiry | now > expiry -> row {irStatus = ISExpired}
  _ -> row

-- | A funded open invoice stays open until it settles.
expiryMoment :: InvoiceRow -> Maybe UTCTime
expiryMoment InvoiceRow {irStatus, irExpiresAt, irPayment}
  | irStatus == ISOpen && not (maybe False paymentHolds irPayment) = Just irExpiresAt
  | otherwise = Nothing

-- | The fields every invoice view has, a badge's and a name's.
invoicePairs :: Maybe Int -> InvoiceRow -> [Pair]
invoicePairs confirmations InvoiceRow {irAmount, irCurrency, irDestination, irExpiresAt, irStatus, irPayment} =
  [ "status" .= (textEncode irStatus :: Text),
    "amount" .= amountJSON irAmount,
    "currency" .= irCurrency,
    "expiresAt" .= irExpiresAt
  ]
    <> destinationPairs confirmations irDestination
    <> maybe [] paymentPairs irPayment
  where
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
      "cryptoCurrency" .= (textEncode currency :: Text)
    ]
      <> maybe [] (\n -> ["requiredConfirmations" .= n]) confirmations

amountJSON :: CurrencyAmount -> J.Value
amountJSON (CurrencyAmount a) = J.toJSON a

-- | Greenfield reports no confirmation count, so this comes from the store's speed policy.
confirmationsFor :: ServiceConfig -> Maybe Int
confirmationsFor cfg = speedPolicyConfirmations . bSpeedPolicy <$> btcpay cfg

-- | BTCPay's own store setting, whose numbering is not in speed order.
speedPolicyConfirmations :: SpeedPolicy -> Int
speedPolicyConfirmations = \case
  HighSpeed -> 0
  MediumSpeed -> 1
  LowMediumSpeed -> 2
  LowSpeed -> 6

secondsPerMinute :: Int
secondsPerMinute = 60

-- | The expiry shown and stored must come from the same key the provider sets the invoice's real expiry from.
invoiceWindow :: ServiceConfig -> ServicePaymentMethod -> NominalDiffTime
invoiceWindow cfg = \case
  SPMCard CPStripe -> minutes (maybe defaultSessionMinutes sSessionMinutes (stripe cfg))
  SPMCrypto _ -> minutes (maybe defaultExpiryMinutes bExpiryMinutes (btcpay cfg))
  where
    minutes n = fromIntegral (secondsPerMinute * n)

providerFor :: [Provider] -> ServicePaymentMethod -> Maybe Provider
providerFor providers method = providerNamed providers (providerOf method)

providerNamed :: [Provider] -> PaymentProvider -> Maybe Provider
providerNamed providers provider = find ((== provider) . pProvider) providers

providerOf :: ServicePaymentMethod -> PaymentProvider
providerOf = \case
  SPMCard CPStripe -> PPStripe
  SPMCrypto _ -> PPCrypto
