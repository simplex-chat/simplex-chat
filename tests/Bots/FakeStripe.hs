{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bots.FakeStripe
  ( FakeStripe (..),
    FakeRequest (..),
    withFakeStripe,
    fakeSessionMinutes,
    fakeReceiptEmail,
    setSessionState,
    failNextCalls,
    useListPageSize,
    useListFixture,
    apiRequests,
    fakeSessionIds,
    stripeSigHeader,
    stripeHexSig,
    stripeEvent,
  )
where

import BadgeService.Config (StripeConfig (..))
import Control.Concurrent.STM
import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.Aeson as J
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.ByteArray.Encoding (Base (Base16, Base64), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
-- http-client is imported qualified: its Request has requestHeaders, requestBody and
-- queryString just as WAI's does.
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
  ( Query,
    Status,
    badRequest400,
    hAuthorization,
    hContentType,
    methodGet,
    methodPost,
    mkStatus,
    notFound404,
    ok200,
    parseSimpleQuery,
    statusCode,
    statusMessage,
    unauthorized401,
  )
import Network.HTTP.Types.Header (Header)
import Network.Wai (Application, Response, pathInfo, queryString, requestHeaders, requestMethod, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.Util (tshow)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | Responses are read from committed fixtures, so they can be replaced with captures
-- from a real instance without changing any Haskell.
fixtureDir :: FilePath
fixtureDir = "apps" </> "simplex-badge-service" </> "test-fixtures" </> "stripe"

-- | A restricted key (`rk_`) shaped like Stripe's, the kind the service is configured with, never a live one.
fakeSecretKey :: Text
fakeSecretKey = "rk_test_51QfakeKEY0000000000000000"

fakeWebhookSecret :: Text
fakeWebhookSecret = "whsec_3d8f5c6a2b1e4f7089abcdef01234567"

fakeSessionMinutes :: Int
fakeSessionMinutes = 60

data FakeRequest = FakeRequest
  { frMethod :: ByteString,
    frPath :: [Text],
    frQuery :: Query,
    frHeaders :: [Header],
    frBody :: LB.ByteString
  }
  deriving (Eq, Show)

data SessionState = SessionState
  { ssStatus :: Text,
    ssPaymentStatus :: Text,
    ssAmountTotal :: Int,
    ssCurrency :: Text
  }
  deriving (Eq, Show)

data FailPlan = FailPlan {fpCalls :: Int, fpStatus :: Int}
  deriving (Eq, Show)

data FakeState = FakeState
  { fsSessions :: M.Map Text SessionState,
    fsNextId :: Int,
    fsFail :: FailPlan,
    fsList :: Text,
    fsPageSize :: Maybe Int,
    fsRequests :: [FakeRequest]
  }

initialState :: FakeState
initialState =
  FakeState
    { fsSessions = M.empty,
      fsNextId = 1,
      fsFail = FailPlan {fpCalls = 0, fpStatus = 0},
      fsList = "session-list",
      fsPageSize = Nothing,
      fsRequests = []
    }

data FakeStripe = FakeStripe
  { -- | Points the adapter at this listener, with the credentials it accepts.
    fsConfig :: StripeConfig,
    fsBaseUrl :: String,
    fsManager :: Manager,
    fsState :: TVar FakeState
  }

-- | A port Warp chooses. A fixed one would collide with a server left over from an
-- earlier run and the test would pass against that instead.
withFakeStripe :: (FakeStripe -> IO a) -> IO a
withFakeStripe action = do
  fsState <- newTVarIO initialState
  fsManager <- HTTP.newManager HTTP.defaultManagerSettings
  Warp.testWithApplication (pure (fakeApp fsState)) $ \prt -> do
    let fsBaseUrl = "http://127.0.0.1:" <> show prt
    action FakeStripe {fsConfig = fakeConfig (T.pack fsBaseUrl), fsBaseUrl, fsManager, fsState}

fakeConfig :: Text -> StripeConfig
fakeConfig host =
  StripeConfig
    { sSecretKey = fakeSecretKey,
      sPublishableKey = "pk_test_x",
      sWebhookSecret = fakeWebhookSecret,
      sReceiptEmail = fakeReceiptEmail,
      sSessionMinutes = fakeSessionMinutes,
      sHost = host
    }

fakeReceiptEmail :: Text
fakeReceiptEmail = "card@example.test"

-- | Stripe-Signature: t=<unix>,v1=<hex HMAC-SHA256 of "t.body">.
stripeSigHeader :: Text -> Int -> LB.ByteString -> [Header]
stripeSigHeader secret t body =
  [("Stripe-Signature", "t=" <> B8.pack (show t) <> ",v1=" <> stripeHexSig secret t body)]

stripeHexSig :: Text -> Int -> LB.ByteString -> ByteString
stripeHexSig secret t body = convertToBase Base16 digest
  where
    signed = TE.encodeUtf8 (T.pack (show t) <> ".") <> LB.toStrict body
    digest :: Digest SHA256
    digest = hmacGetDigest (hmac (TE.encodeUtf8 secret) signed :: HMAC SHA256)

-- | A minimal Checkout Session event: @{type, data:{object:{id}}}@. The verify signs these exact
-- bytes, so a valid signature cannot mean two different things.
stripeEvent :: Text -> Text -> LB.ByteString
stripeEvent eventType sid =
  J.encode (J.object ["type" J..= eventType, "data" J..= J.object ["object" J..= J.object ["id" J..= sid]]])

fixtureResponse :: Text -> IO J.Value
fixtureResponse name = do
  raw <- LB.readFile path
  case J.eitherDecode raw of
    Left e -> fail (path <> ": " <> e)
    Right (J.Object o) -> case (KM.lookup "_fixture" o, KM.lookup "response" o) of
      (Just (J.String _), Just v) -> pure v
      _ -> fail (path <> ": a fixture is {\"_fixture\": <provenance>, \"response\": <body>}")
    Right _ -> fail (path <> ": a fixture is a JSON object")
  where
    path = fixtureDir </> T.unpack name <> ".json"

-- | The served body is chosen by status and payment_status, then patched to the session's own
-- state. An unknown status falls back to the open body and is patched to whatever was set, so a
-- status this build has never seen still reaches the adapter verbatim.
sessionFixture :: SessionState -> Text
sessionFixture SessionState {ssStatus, ssPaymentStatus} = case (ssStatus, ssPaymentStatus) of
  ("complete", "paid") -> "session-complete-paid"
  ("complete", _) -> "session-complete-unpaid"
  ("expired", _) -> "session-expired"
  _ -> "session-open"

patchSession :: Text -> SessionState -> J.Value -> J.Value
patchSession sid SessionState {ssStatus, ssPaymentStatus, ssAmountTotal, ssCurrency} =
  setField "id" (J.String sid)
    . setField "status" (J.String ssStatus)
    . setField "payment_status" (J.String ssPaymentStatus)
    . setField "amount_total" (J.toJSON ssAmountTotal)
    . setField "currency" (J.String ssCurrency)

setField :: Key -> J.Value -> J.Value -> J.Value
setField k v = \case
  J.Object o -> J.Object (KM.insert k v o)
  other -> other

fakeApp :: TVar FakeState -> Application
fakeApp stv req respond = do
  body <- strictRequestBody req
  case pathInfo req of
    ["_state", sid] | isPost -> control body (setState sid)
    ["_fail"] | isPost -> control body setFail
    ["_paging"] | isPost -> control body setPaging
    ["_fixtures"] | isPost -> control body setFixtures
    "v1" : "checkout" : "sessions" : rest -> apiCall body rest
    _ -> refuse notFound404 "no such path on the fake stripe"
  where
    verb = requestMethod req
    isPost = verb == methodPost
    isGet = verb == methodGet
    refuse st message = respond (errorResponse st message)
    ok = respond (jsonResponse ok200 (J.object ["ok" J..= True]))

    control b act = case J.decode b of
      Just (J.Object o) ->
        act o >>= \case
          Nothing -> ok
          Just message -> refuse badRequest400 message
      _ -> refuse badRequest400 "a control call takes a JSON object"

    setState sid o = case unknownKeys stateKeys o of
      Just message -> pure (Just message)
      Nothing -> atomically $ do
        st <- readTVar stv
        case M.lookup sid (fsSessions st) of
          Nothing -> pure (Just ("no session " <> sid <> " was created here"))
          Just s -> do
            let s' =
                  s
                    { ssStatus = fromMaybe (ssStatus s) (textField "status" o),
                      ssPaymentStatus = fromMaybe (ssPaymentStatus s) (textField "payment_status" o)
                    }
            writeTVar stv st {fsSessions = M.insert sid s' (fsSessions st)}
            pure Nothing

    setFail o = case unknownKeys ["calls", "status"] o of
      Just message -> pure (Just message)
      Nothing -> case (intField "calls" o, intField "status" o) of
        (Just calls, Just code) -> do
          atomically (modifyTVar' stv (\st -> st {fsFail = FailPlan {fpCalls = calls, fpStatus = code}}))
          pure Nothing
        _ -> pure (Just "_fail takes {\"calls\": <n>, \"status\": <http status>}")

    setPaging o = case unknownKeys ["size"] o of
      Just message -> pure (Just message)
      Nothing -> case intField "size" o of
        Just size -> do
          atomically (modifyTVar' stv (\st -> st {fsPageSize = Just size}))
          pure Nothing
        Nothing -> pure (Just "_paging takes {\"size\": <n>}")

    setFixtures o = case unknownKeys ["list"] o of
      Just message -> pure (Just message)
      Nothing -> case textField "list" o of
        Just name -> do
          atomically (modifyTVar' stv (\st -> st {fsList = name}))
          pure Nothing
        Nothing -> pure (Just "_fixtures takes {\"list\": <fixture name>}")

    apiCall b rest = do
      atomically $
        modifyTVar' stv $ \st ->
          st {fsRequests = FakeRequest verb (pathInfo req) (queryString req) (requestHeaders req) b : fsRequests st}
      case lookup hAuthorization (requestHeaders req) of
        Just given | given == expectedAuth -> injectingFailure (route b rest)
        _ -> refuse unauthorized401 "Authorization must be HTTP Basic with the secret key as username"

    -- Stripe authenticates the secret key as the Basic username with an empty password, which is
    -- how http-client's applyBasicAuth builds the header.
    expectedAuth = "Basic " <> convertToBase Base64 (TE.encodeUtf8 (fakeSecretKey <> ":"))

    injectingFailure act = do
      failing <- atomically $ do
        st <- readTVar stv
        case fsFail st of
          plan@FailPlan {fpCalls, fpStatus} | fpCalls > 0 -> do
            writeTVar stv st {fsFail = plan {fpCalls = fpCalls - 1}}
            pure (Just fpStatus)
          _ -> pure Nothing
      case failing of
        Just code -> refuse (mkStatus code "Injected Failure") "the fake was told to fail this call"
        Nothing -> act

    route b = \case
      [] | isPost -> createSession b
      [] | isGet -> listSessions
      [sid] | isGet -> withSession sid $ \s -> serve (sessionFixture s) (patchSession sid s)
      [sid, "expire"] | isPost -> withSession sid $ \_ -> respond (jsonResponse ok200 (J.object ["id" J..= sid, "status" J..= ("expired" :: Text)]))
      _ -> refuse notFound404 "no such checkout path on the fake stripe"

    createSession b = do
      let form = parseSimpleQuery (LB.toStrict b)
          amount = fromMaybe 0 (lookup "line_items[0][price_data][unit_amount]" form >>= readMaybe . B8.unpack)
          currency = maybe "usd" TE.decodeUtf8 (lookup "line_items[0][price_data][currency]" form)
      (sid, s) <- atomically $ do
        st <- readTVar stv
        let sid = "cs_test_" <> T.justifyRight 4 '0' (tshow (fsNextId st))
            s = SessionState {ssStatus = "open", ssPaymentStatus = "unpaid", ssAmountTotal = amount, ssCurrency = currency}
        writeTVar stv st {fsNextId = fsNextId st + 1, fsSessions = M.insert sid s (fsSessions st)}
        pure (sid, s)
      serve (sessionFixture s) (patchSession sid s)

    -- Serves session-list.json. With a page size set the `data` array is paginated by
    -- `starting_after` and `has_more`, so a test can drive the adapter across pages.
    listSessions = do
      st <- readTVarIO stv
      full <- fixtureResponse (fsList st)
      let (page, more) = pageItems (fsPageSize st) (queryText "starting_after" (queryString req)) (listItems full)
      respond (jsonResponse ok200 (J.object ["object" J..= ("list" :: Text), "data" J..= page, "has_more" J..= more]))

    serve name patch = do
      v <- fixtureResponse name
      respond (jsonResponse ok200 (patch v))

    withSession sid act = do
      sessions <- fsSessions <$> readTVarIO stv
      case M.lookup sid sessions of
        Just s -> act s
        Nothing -> refuse notFound404 ("no session " <> sid <> " on the fake stripe")

listItems :: J.Value -> [J.Value]
listItems = \case
  J.Object o -> case KM.lookup "data" o of
    Just (J.Array vs) -> foldr (:) [] vs
    _ -> []
  _ -> []

-- | No page size serves the whole array. With one, `starting_after` names the id after which
-- the page begins, and `has_more` says whether anything is left.
pageItems :: Maybe Int -> Maybe Text -> [J.Value] -> ([J.Value], Bool)
pageItems Nothing _ items = (items, False)
pageItems (Just size) after items = (take size rest, length rest > size)
  where
    rest = case after of
      Nothing -> items
      Just a -> drop 1 (dropWhile ((/= Just a) . sessionId) items)

sessionId :: J.Value -> Maybe Text
sessionId = \case
  J.Object o -> case KM.lookup "id" o of
    Just (J.String i) -> Just i
    _ -> Nothing
  _ -> Nothing

queryText :: ByteString -> Query -> Maybe Text
queryText k q = case lookup k q of
  Just (Just v) -> Just (TE.decodeUtf8 v)
  _ -> Nothing

jsonResponse :: Status -> J.Value -> Response
jsonResponse st v = responseLBS st [(hContentType, "application/json")] (J.encode v)

errorResponse :: Status -> Text -> Response
errorResponse st message = jsonResponse st (J.object ["error" J..= J.object ["message" J..= message, "code" J..= TE.decodeUtf8 (statusMessage st)]])

stateKeys :: [Key]
stateKeys = ["status", "payment_status"]

unknownKeys :: [Key] -> J.Object -> Maybe Text
unknownKeys known o = case filter (`notElem` known) (KM.keys o) of
  k : _ -> Just ("this control call does not set " <> K.toText k)
  [] -> Nothing

textField :: Key -> J.Object -> Maybe Text
textField k o = case KM.lookup k o of
  Just (J.String t) -> Just t
  _ -> Nothing

intField :: Key -> J.Object -> Maybe Int
intField k o = case KM.lookup k o of
  Just n -> case J.fromJSON n of
    J.Success i -> Just i
    J.Error _ -> Nothing
  Nothing -> Nothing

controlPost :: FakeStripe -> String -> J.Value -> IO ()
controlPost FakeStripe {fsBaseUrl, fsManager} path v = do
  req <- HTTP.parseRequest (fsBaseUrl <> path)
  r <- HTTP.httpLbs req {HTTP.method = methodPost, HTTP.requestBody = HTTP.RequestBodyLBS (J.encode v)} fsManager
  case statusCode (HTTP.responseStatus r) of
    200 -> pure ()
    code -> fail ("fake stripe " <> path <> " answered " <> show code <> ": " <> show (HTTP.responseBody r))

setSessionState :: FakeStripe -> Text -> [Pair] -> IO ()
setSessionState fake sid fields = controlPost fake ("/_state/" <> T.unpack sid) (J.object fields)

failNextCalls :: FakeStripe -> Int -> Int -> IO ()
failNextCalls fake calls code = controlPost fake "/_fail" (J.object ["calls" J..= calls, "status" J..= code])

-- | Paginate the served list at this many sessions per page, so a test can walk starting_after.
useListPageSize :: FakeStripe -> Int -> IO ()
useListPageSize fake size = controlPost fake "/_paging" (J.object ["size" J..= size])

-- | Serve a different list fixture for the next list calls.
useListFixture :: FakeStripe -> Text -> IO ()
useListFixture fake name = controlPost fake "/_fixtures" (J.object ["list" J..= name])

fakeRequests :: FakeStripe -> IO [FakeRequest]
fakeRequests FakeStripe {fsState} = reverse . fsRequests <$> readTVarIO fsState

fakeSessionIds :: FakeStripe -> IO [Text]
fakeSessionIds FakeStripe {fsState} = M.keys . fsSessions <$> readTVarIO fsState

apiRequests :: FakeStripe -> ByteString -> [Text] -> IO [FakeRequest]
apiRequests fake verb segments = filter matching <$> fakeRequests fake
  where
    matching FakeRequest {frMethod, frPath} =
      frMethod == verb && frPath == ["v1", "checkout", "sessions"] <> segments
