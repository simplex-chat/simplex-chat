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
    clientKey,
    maxBuckets,
    readLimit,
    createLimit,
    searchLimit,
    previewLimit,
    holdMicros,
    jsonResponse,
    jsonError,
    notFound,
    badRequest,
    invalidName,
    invalidLink,
    codeConflict,
    providerUnavailable,
    rateLimited,
    methodNotAllowed,
    internalError,
    readBoundedBody,
    maxBodyBytes,
    parseCodeHash,
  )
where

import Control.Concurrent.STM
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word16)
import Network.HTTP.Types (Header, Method, Status, hCacheControl, hContentType, status400, status404, status405, status409, status429, status500, status503)
import Network.Socket (SockAddr (..), hostAddress6ToTuple, hostAddressToTuple)
import Network.Wai
import Numeric (showHex)
import Simplex.Messaging.Util (safeDecodeUtf8)
import StoreService.Config (ListenerConfig (..))
import Text.Read (readMaybe)

data HttpEnv = HttpEnv
  { heBuckets :: TVar (Map Text Bucket),
    heTrustForwardedFor :: Bool
  }

newHttpEnv :: ListenerConfig -> IO HttpEnv
newHttpEnv ListenerConfig {lTrustForwardedFor} = do
  heBuckets <- newTVarIO Map.empty
  pure HttpEnv {heBuckets, heTrustForwardedFor = lTrustForwardedFor}

type Respond = Response -> IO ResponseReceived

-- | Short enough to stay under the idle timeout of any proxy in front of us.
holdMicros :: Int
holdMicros = 30 * 1000000

readLimit, createLimit, searchLimit, previewLimit :: Limit
readLimit = Limit {lmName = "read", lmPerMinute = 60}
createLimit = Limit {lmName = "create", lmPerMinute = 5}
searchLimit = Limit {lmName = "search", lmPerMinute = 30}
previewLimit = Limit {lmName = "preview", lmPerMinute = 20}

maxBodyBytes :: Int
maxBodyBytes = 8192

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
takeToken HttpEnv {heBuckets} Limit {lmName, lmPerMinute} client = do
  now <- getCurrentTime
  atomically $ do
    buckets <- reclaim now <$> readTVar heBuckets
    let key = lmName <> "\t" <> client
        count bucket = writeTVar heBuckets (Map.insert key bucket buckets) >> pure Nothing
        -- write the reclaimed map back here too, or a refused request redoes the filtering every time and the map stays at the limit
        refuse seconds = writeTVar heBuckets buckets >> pure (Just seconds)
    either (refuse . max 1 . ceiling) count (takeFrom lmPerMinute now (Map.lookup key buckets))

limited :: HttpEnv -> Limit -> Request -> Respond -> IO ResponseReceived -> IO ResponseReceived
limited env limit req respond action =
  takeToken env limit (clientKey env req) >>= \case
    Nothing -> action
    Just seconds -> respond (rateLimited seconds)

only :: Method -> Request -> Respond -> IO ResponseReceived -> IO ResponseReceived
only method req respond action
  | requestMethod req == method = action
  | otherwise = respond (methodNotAllowed method)

-- | Every line is joined first, so a second header line a caller adds cannot leave its own line the one we read.
clientKey :: HttpEnv -> Request -> Text
clientKey HttpEnv {heTrustForwardedFor} req = fromMaybe (peerText (remoteHost req)) forwarded
  where
    forwarded
      | not heTrustForwardedFor = Nothing
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

invalidName :: Response
invalidName = jsonError status400 "invalid_name"

invalidLink :: Response
invalidLink = jsonError status400 "invalid_link"

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
