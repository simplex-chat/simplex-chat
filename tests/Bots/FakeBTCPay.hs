{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bots.FakeBTCPay
  ( FakeBTCPay (..),
    FakeRequest (..),
    withFakeBTCPay,
    fakeApiKey,
    fakeExpiryMinutes,
    setInvoiceState,
    failNextCalls,
    failAfterCalls,
    useListFixture,
    usePaymentMethodsFixture,
    ignoreListPaging,
    answerOversize,
    fakeRequests,
    apiRequests,
    fakeInvoiceIds,
    fixtureResponse,
    webhookEvent,
    webhookSigHeader,
    webhookHexSig,
  )
where

import BadgeService.Config (BTCPayConfig (..), SpeedPolicy (..))
import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Monad (join)
import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.Aeson as J
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.List (isPrefixOf, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
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
fixtureDir = "apps" </> "simplex-badge-service" </> "test-fixtures" </> "btcpay"

fakeApiKey :: Text
fakeApiKey = "7f3c1d9a2e5b48c6905ad1e2f3b4c5d6e7f80912"

fakeStoreId :: Text
fakeStoreId = "BqZKtkeSN9JgLLdCJRJfXQjLwCLDNCVLdxLTTdMzTHnT"

fakeWebhookSecret :: Text
fakeWebhookSecret = "3d8f5c6a2b1e4f7089abcdef01234567"

fakeExpiryMinutes :: Int
fakeExpiryMinutes = 60

data FakeRequest = FakeRequest
  { frMethod :: ByteString,
    frPath :: [Text],
    frQuery :: Query,
    frHeaders :: [Header],
    frBody :: LB.ByteString
  }
  deriving (Eq, Show)

data InvoiceState = InvoiceState
  { isMethodId :: Text,
    isStatus :: Text,
    isAdditionalStatus :: Text,
    isPaid :: Maybe Text,
    isDue :: Maybe Text
  }
  deriving (Eq, Show)

data FailPlan = FailPlan {fpSkip :: Int, fpCalls :: Int, fpStatus :: Int}
  deriving (Eq, Show)

data FakeState = FakeState
  { fsInvoices :: M.Map Text InvoiceState,
    fsNextId :: Int,
    fsFail :: FailPlan,
    fsList :: Text,
    fsMethods :: Maybe Text,
    fsIgnorePaging :: Bool,
    fsOversize :: Bool,
    fsRequests :: [FakeRequest]
  }

initialState :: FakeState
initialState =
  FakeState
    { fsInvoices = M.empty,
      fsNextId = 1,
      fsFail = FailPlan {fpSkip = 0, fpCalls = 0, fpStatus = 0},
      fsList = "invoice-list",
      fsMethods = Nothing,
      fsIgnorePaging = False,
      fsOversize = False,
      fsRequests = []
    }

data FakeBTCPay = FakeBTCPay
  { -- | Points the adapter at this listener, with the credentials it accepts.
    fbConfig :: BTCPayConfig,
    fbBaseUrl :: String,
    fbManager :: Manager,
    fbState :: TVar FakeState
  }

-- | A port Warp chooses. A fixed one would collide with a server left over from an
-- earlier run and the test would pass against that instead.
withFakeBTCPay :: (FakeBTCPay -> IO a) -> IO a
withFakeBTCPay action = do
  fbState <- newTVarIO initialState
  fbManager <- HTTP.newManager HTTP.defaultManagerSettings
  Warp.testWithApplication (pure (fakeApp fbState)) $ \prt -> do
    let fbBaseUrl = "http://127.0.0.1:" <> show prt
    action FakeBTCPay {fbConfig = fakeConfig (T.pack fbBaseUrl), fbBaseUrl, fbManager, fbState}

fakeConfig :: Text -> BTCPayConfig
fakeConfig host =
  BTCPayConfig
    { bHost = host,
      bApiKey = fakeApiKey,
      bStoreId = fakeStoreId,
      bWebhookSecret = fakeWebhookSecret,
      bExpiryMinutes = fakeExpiryMinutes,
      bSpeedPolicy = MediumSpeed,
      bPaymentTolerance = 0.5
    }

-- | A BTCPay webhook delivery, as its documentation shows one. Both suites sign this exact
-- envelope, so what a valid signature is over cannot mean two different things.
webhookEvent :: Text -> Text -> LB.ByteString
webhookEvent eventType invoiceRef =
  LB.fromStrict . TE.encodeUtf8 . T.unlines $
    [ "{",
      "  \"deliveryId\": \"vBYbSCVSMFXFqSuCZ2LEHt\",",
      "  \"webhookId\": \"5GtRTUCYPbVAWJmYcgZ8wK\",",
      "  \"originalDeliveryId\": \"vBYbSCVSMFXFqSuCZ2LEHt\",",
      "  \"isRedelivery\": false,",
      "  \"type\": \"" <> eventType <> "\",",
      "  \"timestamp\": 1700000000,",
      "  \"storeId\": \"" <> fakeStoreId <> "\",",
      "  \"invoiceId\": \"" <> invoiceRef <> "\"",
      "}"
    ]

-- | The header BTCPay sends with it: HMAC-SHA256 over the bytes as written above.
webhookSigHeader :: Text -> LB.ByteString -> [Header]
webhookSigHeader secret body = [("BTCPay-Sig", "sha256=" <> webhookHexSig secret body)]

-- | The hex digest alone, for the tests that malform the header around it.
webhookHexSig :: Text -> LB.ByteString -> ByteString
webhookHexSig secret body = convertToBase Base16 digest
  where
    digest :: Digest SHA256
    digest = hmacGetDigest (hmac (TE.encodeUtf8 secret) (LB.toStrict body) :: HMAC SHA256)

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

invoiceFixture :: Text -> Text
invoiceFixture = \case
  "Processing" -> "invoice-processing"
  "Settled" -> "invoice-settled"
  "Expired" -> "invoice-expired"
  "Invalid" -> "invoice-invalid"
  _ -> "invoice-new"

methodsFixture :: Text -> Text
methodsFixture methodId
  | methodId == xmrMethod = "payment-methods-xmr"
  | otherwise = "payment-methods-btc"

-- the adapter's own ids, restated rather than imported: a fake that shared the constant
-- would agree with a wrong change to it
btcMethod, xmrMethod :: Text
btcMethod = "BTC-CHAIN"
xmrMethod = "XMR-CHAIN"

patchInvoice :: Text -> InvoiceState -> J.Value -> J.Value
patchInvoice invId InvoiceState {isStatus, isAdditionalStatus} =
  setField "id" (J.String invId)
    . setField "status" (J.String isStatus)
    . setField "additionalStatus" (J.String isAdditionalStatus)

patchMethods :: InvoiceState -> J.Value -> J.Value
patchMethods InvoiceState {isPaid, isDue} = \case
  J.Array vs -> J.Array (fmap one vs)
  other -> other
  where
    one = \case
      J.Object o ->
        let paid = fromMaybe (fixturePaid o) isPaid
            -- with nothing received the whole amount is owed. The fixtures were written for a
            -- settled invoice, so without this a freshly created one would serve their `due` of
            -- zero and the double would bless "nothing left to send" on an unpaid invoice.
            due = if isZero paid then isDue <|> textField "amount" o else isDue
            withPaid = KM.insert "paymentMethodPaid" (J.String paid) o
            withDue = maybe withPaid (\d -> KM.insert "due" (J.String d) withPaid) due
         in J.Object (if isZero paid then KM.insert "payments" (J.Array mempty) withDue else withDue)
      v -> v
    fixturePaid o = case KM.lookup "paymentMethodPaid" o of
      Just (J.String t) -> t
      _ -> "0"

isZero :: Text -> Bool
isZero t = (J.decodeStrict (TE.encodeUtf8 t) :: Maybe Scientific) == Just 0

setField :: Key -> J.Value -> J.Value -> J.Value
setField k v = \case
  J.Object o -> J.Object (KM.insert k v o)
  other -> other

fakeApp :: TVar FakeState -> Application
fakeApp stv req respond = do
  body <- strictRequestBody req
  case pathInfo req of
    ["_state", invId] | isPost -> control body (setState invId)
    ["_fail"] | isPost -> control body setFail
    ["_fixtures"] | isPost -> control body setFixtures
    ["_paging"] | isPost -> control body setPaging
    ["_oversize"] | isPost -> control body setOversize
    "api" : "v1" : "stores" : storeId : rest -> apiCall body storeId rest
    _ -> refuse notFound404 "no such path on the fake btcpay"
  where
    verb = requestMethod req
    isPost = verb == methodPost
    isGet = verb == methodGet
    refuse st message = respond (errorResponse st message)
    ok = respond (jsonResponse ok200 (J.object ["ok" J..= True]))

    control body act = case J.decode body of
      Just (J.Object o) ->
        act o >>= \case
          Nothing -> ok
          Just message -> refuse badRequest400 message
      _ -> refuse badRequest400 "a control call takes a JSON object"

    setState invId o = case unknownKeys stateKeys o of
      Just message -> pure (Just message)
      Nothing -> atomically $ do
        st <- readTVar stv
        case M.lookup invId (fsInvoices st) of
          Nothing -> pure (Just ("no invoice " <> invId <> " was created here"))
          Just inv -> do
            let inv' =
                  inv
                    { isStatus = fromMaybe (isStatus inv) (textField "status" o),
                      isAdditionalStatus = fromMaybe (isAdditionalStatus inv) (textField "additionalStatus" o),
                      isPaid = maybe (isPaid inv) Just (textField "paymentMethodPaid" o),
                      isDue = maybe (isDue inv) Just (textField "due" o)
                    }
            writeTVar stv st {fsInvoices = M.insert invId inv' (fsInvoices st)}
            pure Nothing

    setFail o = case unknownKeys ["calls", "status", "skip"] o of
      Just message -> pure (Just message)
      Nothing -> case (intField "calls" o, intField "status" o) of
        (Just calls, Just code) -> do
          let plan = FailPlan {fpSkip = fromMaybe 0 (intField "skip" o), fpCalls = calls, fpStatus = code}
          atomically (modifyTVar' stv (\st -> st {fsFail = plan}))
          pure Nothing
        _ -> pure (Just "_fail takes {\"calls\": <n>, \"status\": <http status>, \"skip\": <n>}")

    setFixtures o = case unknownKeys ["list", "paymentMethods"] o of
      Just message -> pure (Just message)
      Nothing -> case (named "list" "invoice-list" fsListOf, named "paymentMethods" "payment-methods" fsMethodsOf) of
        (Left message, _) -> pure (Just message)
        (_, Left message) -> pure (Just message)
        (Right setL, Right setM)
          | null (KM.keys o) -> pure (Just "_fixtures takes a `list` or a `paymentMethods` fixture name")
          | otherwise -> do
              atomically (modifyTVar' stv (setM . setL))
              pure Nothing
      where
        named k prefix set = case textField k o of
          Nothing -> Right id
          Just name
            | prefix `isPrefixOf` T.unpack name -> Right (set name)
            | otherwise -> Left ("not a " <> K.toText k <> " fixture: " <> name)
        fsListOf name st = st {fsList = name}
        fsMethodsOf name st = st {fsMethods = Just name}

    setOversize o = case unknownKeys ["on"] o of
      Just message -> pure (Just message)
      Nothing -> case KM.lookup "on" o of
        Just (J.Bool on) -> do
          atomically (modifyTVar' stv (\st -> st {fsOversize = on}))
          pure Nothing
        _ -> pure (Just "_oversize takes {\"on\": <bool>}")

    setPaging o = case unknownKeys ["ignore"] o of
      Just message -> pure (Just message)
      Nothing -> case KM.lookup "ignore" o of
        Just (J.Bool ignore) -> do
          atomically (modifyTVar' stv (\st -> st {fsIgnorePaging = ignore}))
          pure Nothing
        _ -> pure (Just "_paging takes {\"ignore\": <bool>}")

    apiCall body storeId rest = do
      atomically $
        modifyTVar' stv $ \st ->
          st {fsRequests = FakeRequest verb (pathInfo req) (queryString req) (requestHeaders req) body : fsRequests st}
      if storeId /= fakeStoreId
        then refuse notFound404 ("no store " <> storeId <> " on the fake btcpay")
        else case lookup hAuthorization (requestHeaders req) of
          Just given | given == expectedAuth -> injectingFailure (greenfield body rest)
          _ -> refuse unauthorized401 "Authorization must be `token <api-key>`"

    expectedAuth = "token " <> TE.encodeUtf8 fakeApiKey

    injectingFailure act = do
      failing <- atomically $ do
        st <- readTVar stv
        case fsFail st of
          plan@FailPlan {fpSkip} | fpSkip > 0 -> do
            writeTVar stv st {fsFail = plan {fpSkip = fpSkip - 1}}
            pure Nothing
          plan@FailPlan {fpCalls, fpStatus} | fpCalls > 0 -> do
            writeTVar stv st {fsFail = plan {fpCalls = fpCalls - 1}}
            pure (Just fpStatus)
          _ -> pure Nothing
      case failing of
        Just code -> refuse (mkStatus code "Injected Failure") "the fake was told to fail this call"
        Nothing -> act

    greenfield body = \case
      ["payment-methods"] | isGet -> serve "store-payment-methods" id
      ["invoices"] | isPost -> createInvoice body
      ["invoices"] | isGet -> listInvoices
      ["invoices", invId] | isGet ->
        withInvoice invId $ \inv -> serve (invoiceFixture (isStatus inv)) (patchInvoice invId inv)
      ["invoices", invId, "payment-methods"] | isGet ->
        withInvoice invId $ \inv -> do
          override <- fsMethods <$> readTVarIO stv
          serve (fromMaybe (methodsFixture (isMethodId inv)) override) (patchMethods inv)
      _ -> refuse notFound404 "no such greenfield path on the fake btcpay"

    listInvoices = do
      st <- readTVarIO stv
      fixture <- fixtureResponse (fsList st)
      case arrayEntries fixture of
        Nothing -> respond (jsonResponse ok200 fixture)
        Just fixed -> do
          let invs = M.toAscList (fsInvoices st)
          cache <- readFixtures (concatMap (fixtureNames (fsMethods st)) invs)
          let entries = fixed <> map (listedInvoice cache (fsMethods st)) invs
              page = if fsIgnorePaging st then entries else paged (queryString req) entries
          respond (jsonResponse ok200 (J.toJSON page))

    readFixtures names = M.fromList <$> mapM (\n -> (,) n <$> fixtureResponse n) (nub names)

    fixtureNames override (_, inv) = [invoiceFixture (isStatus inv), methodsName override inv]

    methodsName override inv = fromMaybe (methodsFixture (isMethodId inv)) override

    listedInvoice cache override (invId, inv) =
      setField "paymentMethods" (patchMethods inv (cached (methodsName override inv))) $
        patchInvoice invId inv (cached (invoiceFixture (isStatus inv)))
      where
        cached name = fromMaybe J.Null (M.lookup name cache)

    serve name patch = do
      oversize <- fsOversize <$> readTVarIO stv
      if oversize
        then respond (jsonResponse ok200 (J.String (T.replicate (12 * 1024 * 1024) "x")))
        else do
          v <- fixtureResponse name
          respond (jsonResponse ok200 (patch v))

    withInvoice invId act = do
      invoices <- fsInvoices <$> readTVarIO stv
      case M.lookup invId invoices of
        Just inv -> act inv
        Nothing -> refuse notFound404 ("no invoice " <> invId <> " on the fake btcpay")

    createInvoice body = case J.decode body >>= chosenMethod of
      Nothing -> refuse badRequest400 "checkout.paymentMethods must name exactly one of BTC-CHAIN, XMR-CHAIN"
      Just methodId -> do
        (invId, inv) <- atomically $ do
          st <- readTVar stv
          let invId = "FakeInvoiceRef" <> T.justifyRight 4 '0' (tshow (fsNextId st))
              inv =
                InvoiceState
                  { isMethodId = methodId,
                    isStatus = "New",
                    isAdditionalStatus = "None",
                    isPaid = Just "0.00000000", isDue = Nothing
                  }
          writeTVar stv st {fsNextId = fsNextId st + 1, fsInvoices = M.insert invId inv (fsInvoices st)}
          pure (invId, inv)
        serve (invoiceFixture (isStatus inv)) (patchInvoice invId inv)

arrayEntries :: J.Value -> Maybe [J.Value]
arrayEntries = \case
  J.Array vs -> Just (foldr (:) [] vs)
  _ -> Nothing

paged :: Query -> [J.Value] -> [J.Value]
paged q entries = maybe id take (intQuery "take" q) (drop (fromMaybe 0 (intQuery "skip" q)) entries)

intQuery :: ByteString -> Query -> Maybe Int
intQuery k q = join (lookup k q) >>= readMaybe . B8.unpack

stateKeys :: [Key]
stateKeys = ["status", "additionalStatus", "paymentMethodPaid", "due"]

unknownKeys :: [Key] -> J.Object -> Maybe Text
unknownKeys known o = case filter (`notElem` known) (KM.keys o) of
  k : _ -> Just ("this control call does not set " <> K.toText k)
  [] -> Nothing

chosenMethod :: J.Value -> Maybe Text
chosenMethod v = case objectField "checkout" v >>= objectField "paymentMethods" of
  Just (J.Array ms) -> case foldr (:) [] ms of
    [J.String m] | m `elem` [btcMethod, xmrMethod] -> Just m
    _ -> Nothing
  _ -> Nothing

objectField :: Key -> J.Value -> Maybe J.Value
objectField k = \case
  J.Object o -> KM.lookup k o
  _ -> Nothing

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

jsonResponse :: Status -> J.Value -> Response
jsonResponse st v = responseLBS st [(hContentType, "application/json")] (J.encode v)

errorResponse :: Status -> Text -> Response
errorResponse st message = jsonResponse st (J.object ["code" J..= TE.decodeUtf8 (statusMessage st), "message" J..= message])

controlPost :: FakeBTCPay -> String -> J.Value -> IO ()
controlPost FakeBTCPay {fbBaseUrl, fbManager} path v = do
  req <- HTTP.parseRequest (fbBaseUrl <> path)
  r <- HTTP.httpLbs req {HTTP.method = methodPost, HTTP.requestBody = HTTP.RequestBodyLBS (J.encode v)} fbManager
  case statusCode (HTTP.responseStatus r) of
    200 -> pure ()
    code -> fail ("fake btcpay " <> path <> " answered " <> show code <> ": " <> show (HTTP.responseBody r))

setInvoiceState :: FakeBTCPay -> Text -> [Pair] -> IO ()
setInvoiceState fake invId fields = controlPost fake ("/_state/" <> T.unpack invId) (J.object fields)

failNextCalls :: FakeBTCPay -> Int -> Int -> IO ()
failNextCalls = failAfterCalls 0

failAfterCalls :: Int -> FakeBTCPay -> Int -> Int -> IO ()
failAfterCalls skip fake calls code =
  controlPost fake "/_fail" (J.object ["calls" J..= calls, "status" J..= code, "skip" J..= skip])

-- | The next reads answer with a body far past what the adapter will hold.
answerOversize :: FakeBTCPay -> Bool -> IO ()
answerOversize fake on = controlPost fake "/_oversize" (J.object ["on" J..= on])

useListFixture :: FakeBTCPay -> Text -> IO ()
useListFixture fake name = controlPost fake "/_fixtures" (J.object ["list" J..= name])

usePaymentMethodsFixture :: FakeBTCPay -> Text -> IO ()
usePaymentMethodsFixture fake name = controlPost fake "/_fixtures" (J.object ["paymentMethods" J..= name])

ignoreListPaging :: FakeBTCPay -> IO ()
ignoreListPaging fake = controlPost fake "/_paging" (J.object ["ignore" J..= True])

fakeRequests :: FakeBTCPay -> IO [FakeRequest]
fakeRequests FakeBTCPay {fbState} = reverse . fsRequests <$> readTVarIO fbState

fakeInvoiceIds :: FakeBTCPay -> IO [Text]
fakeInvoiceIds FakeBTCPay {fbState} = M.keys . fsInvoices <$> readTVarIO fbState

apiRequests :: FakeBTCPay -> ByteString -> [Text] -> IO [FakeRequest]
apiRequests fake verb segments = filter matching <$> fakeRequests fake
  where
    matching FakeRequest {frMethod, frPath} =
      frMethod == verb && frPath == ["api", "v1", "stores", fakeStoreId] <> segments
