{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Providers.BTCPay
  ( btcpayProvider,
    btcMethodId,
    xmrMethodId,
    minorToDecimal,
    paymentMethodsSignal,
    listSignals,
    verifyBTCPaySig,
    listPageSize,
    maxListPages,
  )
where

import BadgeService.Config (BTCPayConfig (..), speedPolicyName)
import BadgeService.Providers
  ( ListPass (..),
    OrderDraft (..),
    PaymentSignal (..),
    Provider (..),
    ProviderError (..),
    ProviderInvoice (..),
    Funded (..),
    Received (..),
    settleWindow,
    WebhookError (..),
  )
import Control.Exception (try)
import Control.Logger.Simple (logError, logInfo, logWarn)
import Control.Monad (unless)
import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JT
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (FPFormat (Fixed), Scientific, base10Exponent, formatScientific)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word32)
import Network.HTTP.Client
  ( HttpException,
    Manager,
    Request (..),
    RequestBody (..),
    Response (..),
    brReadSome,
    parseRequest,
    withResponse,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Header, HeaderName, Method, Query, Status (..), methodGet, methodPost, renderQuery, urlEncode)
import Simplex.Chat.PaymentService.Types
  ( CryptoCurrency (..),
    CurrencyAmount (..),
    PaymentProvider (..),
    ServicePaymentDestination (..),
    ServicePaymentMethod (..),
  )
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)

-- | Not configurable, so we log them at startup beside the ids the store reports.
btcMethodId, xmrMethodId :: Text
btcMethodId = "BTC-CHAIN"
xmrMethodId = "XMR-CHAIN"

knownMethodIds :: [Text]
knownMethodIds = [btcMethodId, xmrMethodId]

-- | The deliveries worth queueing a read for. Anything else BTCPay sends says nothing this
-- service acts on, and a hint it cannot use costs a queue slot.
actedOnEventTypes :: [Text]
actedOnEventTypes = ["InvoiceProcessing", "InvoiceSettled", "InvoiceExpired", "InvoiceInvalid"]

maxErrorBytes :: Int64
maxErrorBytes = 4000

-- | A page of a hundred invoices with their payment methods is tens of kilobytes. Ten megabytes
-- is far above anything BTCPay sends and far below what would cost the poller its thread.
maxProviderBytes :: Int64
maxProviderBytes = 10 * 1024 * 1024

listPageSize :: Int
listPageSize = 100

-- | A server that kept returning full pages, which it would if it ignored @take@, would
-- keep this pass running and the poller would never reach its expiry sweep.
maxListPages :: Int
maxListPages = 50

pageCapReason :: Text
pageCapReason =
  "btcpay: the list stopped at "
    <> tshow maxListPages
    <> " pages, so any open invoice past the first "
    <> tshow (maxListPages * listPageSize)
    <> " was not read — and will not be read by a later pass either"

-- | Integer arithmetic only. A rounding mistake here charges the wrong amount.
minorToDecimal :: CurrencyAmount -> Text
minorToDecimal (CurrencyAmount a) =
  T.pack (show (a `div` 100)) <> "." <> T.justifyRight 2 '0' (T.pack (show (a `mod` 100)))

cryptoTarget :: ServicePaymentMethod -> Either ProviderError (Text, CryptoCurrency)
cryptoTarget = \case
  SPMCrypto CCBtc -> Right (btcMethodId, CCBtc)
  SPMCrypto CCXmr -> Right (xmrMethodId, CCXmr)
  SPMCard _ -> Left (ProviderError "btcpay offers no card payment method")

-- | BTCPay sends numbers as JSON strings. We keep the text exactly as it arrived, since
-- that is what we store and show, plus an exact decimal to calculate with.
data WireNum = WireNum {wnText :: Text, wnValue :: Scientific}
  deriving (Eq, Show)

instance J.FromJSON WireNum where
  parseJSON = \case
    J.String t -> case J.decodeStrict (TE.encodeUtf8 t) of
      Just v | inRange v -> pure WireNum {wnText = t, wnValue = v}
      Just v -> outOfRange v
      Nothing -> fail ("not a decimal number: " <> show t)
    J.Number v
      | inRange v -> pure WireNum {wnText = T.pack (formatScientific Fixed (Just (wireDecimals v)) v), wnValue = v}
      | otherwise -> outOfRange v
    v -> JT.typeMismatch "numeric string" v
    where
      -- the exponent decides before anything rounds or formats: `1e1000000000` parses fine and
      -- then asks for a billion digits, which exhausts the heap on the poller thread. The
      -- refusal names the exponent alone for the same reason.
      inRange v = base10Exponent v >= minExponent && base10Exponent v <= maxExponent
      outOfRange v = fail ("decimal exponent out of range: " <> show (base10Exponent v))

-- | Not a check on the figure, which `toMinorUnits` clamps and the catalog bounds: only on the
-- magnitude, so nothing asks for a number with more digits than a machine can hold. A rate
-- carrying a repeating division at full decimal scale is well inside this.
minExponent, maxExponent :: Int
minExponent = -64
maxExponent = 64

wireDecimals :: Scientific -> Int
wireDecimals = max 0 . negate . base10Exponent

data GInvoice = GInvoice
  { giId :: Text,
    giStatus :: Text,
    giAdditionalStatus :: Maybe Text,
    giPaymentMethods :: Maybe [GPaymentMethod]
  }
  deriving (Show)

instance J.FromJSON GInvoice where
  parseJSON = J.withObject "invoice" $ \o ->
    GInvoice
      <$> o J..: "id"
      <*> o J..: "status"
      <*> o J..:? "additionalStatus"
      <*> o J..:? "paymentMethods"

-- | @totalPaid@ is deliberately not decoded: it is everything paid on the invoice
-- converted into this method's currency, so it is non-zero even when this method got
-- nothing.
data GPaymentMethod = GPaymentMethod
  { gpmId :: Text,
    gpmDestination :: Maybe Text,
    gpmAmount :: Maybe WireNum,
    gpmRate :: Maybe WireNum,
    gpmPaid :: Maybe WireNum,
    gpmDue :: Maybe WireNum,
    gpmPayments :: [GPayment]
  }
  deriving (Show)

instance J.FromJSON GPaymentMethod where
  parseJSON = J.withObject "payment method" $ \o ->
    GPaymentMethod
      <$> o J..: "paymentMethodId"
      <*> o J..:? "destination"
      <*> o J..:? "amount"
      <*> o J..:? "rate"
      <*> o J..:? "paymentMethodPaid"
      <*> o J..:? "due"
      <*> (fromMaybe [] <$> o J..:? "payments")

data GPayment = GPayment
  { gpStatus :: Text,
    gpReceivedDate :: WireNum
  }
  deriving (Show)

instance J.FromJSON GPayment where
  parseJSON = J.withObject "payment" $ \o ->
    GPayment <$> o J..: "status" <*> o J..: "receivedDate"

newtype GCreated = GCreated {gcId :: Text}
  deriving (Show)

instance J.FromJSON GCreated where
  parseJSON = J.withObject "created invoice" $ \o -> GCreated <$> o J..: "id"

newtype GStoreMethod = GStoreMethod {gsmId :: Text}
  deriving (Show)

instance J.FromJSON GStoreMethod where
  parseJSON = J.withObject "store payment method" $ \o -> GStoreMethod <$> o J..: "paymentMethodId"

data GEvent = GEvent {geType :: Text, geInvoiceId :: Text}
  deriving (Show)

instance J.FromJSON GEvent where
  parseJSON = J.withObject "webhook event" $ \o -> GEvent <$> o J..: "type" <*> o J..: "invoiceId"

data BTCPayEnv = BTCPayEnv {beCfg :: BTCPayConfig, beManager :: Manager}

btcpayProvider :: BTCPayConfig -> IO Provider
btcpayProvider cfg = do
  beManager <- newTlsManager
  let env = BTCPayEnv {beCfg = cfg, beManager}
  logStoreMethods env
  pure
    Provider
      { pProvider = PPCrypto,
        pCreateInvoice = createInvoice env,
        pReadInvoice = readInvoice env,
        pCancelInvoice = cancelInvoice env,
        pListOpen = listOpen env,
        pVerifyWebhook = verifyBTCPaySig (bWebhookSecret cfg)
      }

-- | `enabledOnly`, because the unfiltered list includes methods that are configured and
-- switched off, and a checkout on one of those is refused with "no matching payment method".
logStoreMethods :: BTCPayEnv -> IO ()
logStoreMethods env@BTCPayEnv {beCfg} = do
  r <- greenfield env what methodGet ["payment-methods"] [("enabledOnly", Just "true")] Nothing
  case r >>= decodeGreenfield what of
    Left (ProviderError e) -> do
      logWarn ("btcpay: could not read the payment methods of store " <> bStoreId beCfg <> ": " <> e)
      logInfo ("btcpay: this build offers " <> T.intercalate ", " knownMethodIds)
    Right ms -> do
      let enabled = map gsmId ms
          missing = filter (`notElem` enabled) knownMethodIds
      logInfo ("btcpay: store " <> bStoreId beCfg <> " has enabled " <> T.intercalate ", " enabled)
      unless (null missing) $
        logError $
          "btcpay: this build offers "
            <> T.intercalate ", " missing
            <> ", which the store has not enabled; every checkout on those will be refused"
  where
    what = "read store payment methods"

createInvoice :: BTCPayEnv -> ServicePaymentMethod -> OrderDraft -> IO (Either ProviderError ProviderInvoice)
createInvoice env@BTCPayEnv {beCfg} spm OrderDraft {odAmount, odCurrency} =
  case cryptoTarget spm of
    Left e -> pure (Left e)
    Right (methodId, cc) -> do
      created <- greenfield env what methodPost ["invoices"] [] (Just (body methodId))
      case created >>= decodeGreenfield what of
        -- the request too, not only the answer: a refusal we cannot reproduce is a refusal we
        -- cannot fix, and this body carries no secret
        Left e@(ProviderError m) -> do
          logWarn ("btcpay: the refused request was " <> safeDecodeUtf8 (LB.toStrict (J.encode (body methodId))) <> " -- " <> m)
          pure (Left e)
        Right GCreated {gcId} -> do
          ms <- greenfield env what methodGet ["invoices", gcId, "payment-methods"] [] Nothing
          pure (ms >>= destination methodId cc gcId)
  where
    what = "create invoice"
    body methodId =
      J.object
        [ "amount" J..= minorToDecimal odAmount,
          "currency" J..= T.toUpper odCurrency,
          "checkout"
            J..= J.object
              [ -- the listener derives the row's expires_at from this same key
                "expirationMinutes" J..= bExpiryMinutes beCfg,
                "speedPolicy" J..= speedPolicyName (bSpeedPolicy beCfg),
                "paymentTolerance" J..= bPaymentTolerance beCfg,
                "paymentMethods" J..= [methodId]
              ]
        ]
    destination methodId cc ref ms = do
      m <- findMethod what ref methodId ms
      addr <- required "destination" (gpmDestination m)
      payable <- required "amount" (gpmAmount m)
      pure
        ProviderInvoice
          { piProviderRef = ref,
            piDestination = SPDCrypto cc addr (wnText payable)
          }
      where
        required field =
          maybe (Left (ProviderError (what <> ": " <> methodId <> " on invoice " <> ref <> " has no " <> field))) Right

-- | Needs the API key's canmodifyinvoices permission, which the read and create calls do not.
cancelInvoice :: BTCPayEnv -> Text -> IO (Either ProviderError ())
cancelInvoice env ref =
  fmap (fmap (const ())) $ greenfield env what methodPost ["invoices", ref, "status"] [] (Just body)
  where
    what = "cancel invoice"
    -- Invalid, not Archive: archiving only hides the invoice, and BTCPay keeps crediting
    -- payments to it
    body = J.object ["status" J..= ("Invalid" :: Text)]

readInvoice :: BTCPayEnv -> Text -> IO (Either ProviderError (Maybe PaymentSignal))
readInvoice env ref = do
  inv <- greenfield env what methodGet ["invoices", ref] [] Nothing
  case inv >>= decodeGreenfield what of
    Left e -> pure (Left e)
    Right GInvoice {giStatus, giAdditionalStatus} -> do
      logAdditionalStatus ref giAdditionalStatus
      ms <- greenfield env what methodGet ["invoices", ref, "payment-methods"] [] Nothing
      now <- getCurrentTime
      pure (ms >>= paymentMethodsSignal now ref giStatus)
  where
    what = "read invoice"

listOpen :: BTCPayEnv -> IO (Either ProviderError ListPass)
listOpen env@BTCPayEnv {beCfg} = do
  now <- getCurrentTime
  let oldest = addUTCTime (negate (settleWindow + fromIntegral (60 * bExpiryMinutes beCfg))) now
  fetch now (B8.pack (show (unixSeconds oldest))) 0 maxListPages (ListPass {lpMoved = [], lpSkipped = []})
  where
    unixSeconds :: UTCTime -> Integer
    unixSeconds = floor . utcTimeToPOSIXSeconds
    query startDate skip =
      [ ("includePaymentMethods", Just "true"),
        ("startDate", Just startDate),
        ("take", Just (B8.pack (show listPageSize))),
        ("skip", Just (B8.pack (show skip)))
      ]
    fetch :: UTCTime -> B.ByteString -> Int -> Int -> ListPass -> IO (Either ProviderError ListPass)
    fetch now startDate skip pagesLeft acc
      | pagesLeft <= 0 = pure (Right acc {lpSkipped = lpSkipped acc <> [(Nothing, pageCapReason)]})
      | otherwise = do
          listed <- greenfield env listWhat methodGet ["invoices"] (query startDate skip) Nothing
          case listed >>= decodeGreenfield listWhat of
            Left e -> pure (Left e)
            Right invs -> case invoicesPass now invs of
              Left e -> pure (Left e)
              Right pass
                | length invs < listPageSize -> pure (Right (merge acc pass))
                | otherwise -> fetch now startDate (skip + listPageSize) (pagesLeft - 1) (merge acc pass)
    merge a b = ListPass {lpMoved = lpMoved a <> lpMoved b, lpSkipped = lpSkipped a <> lpSkipped b}

listWhat :: Text
listWhat = "list invoices"

listSignals :: UTCTime -> LB.ByteString -> Either ProviderError ListPass
listSignals now body = decodeGreenfield listWhat body >>= invoicesPass now

-- | An invoice we do not understand is skipped rather than failing the pass, since a
-- status we do not know will not start working later and every pass would fail alike. That
-- covers one we cannot even parse: the elements are read one at a time, or a single malformed
-- @payments@ entry would fail the decode of the whole page and no invoice would ever settle.
-- @paymentMethods@ absent or null does fail: it means @includePaymentMethods@ was ignored, and
-- skipping would leave us reporting healthy empty passes. One of a shape this build cannot read
-- is a skip like any other, which still holds the sweep back.
invoicesPass :: UTCTime -> [J.Value] -> Either ProviderError ListPass
invoicesPass now invs = foldr add (Right ListPass {lpMoved = [], lpSkipped = []}) invs
  where
    add _ (Left e) = Left e
    add v (Right pass) = case J.fromJSON v of
      J.Error e -> Right pass {lpSkipped = (invoiceIdOf v, T.pack e) : lpSkipped pass}
      J.Success gi -> addInvoice gi pass
    addInvoice GInvoice {giId, giStatus, giPaymentMethods} pass = case giPaymentMethods of
      Nothing ->
        Left . ProviderError $
          listWhat <> ": invoice " <> giId <> " carries no paymentMethods, so includePaymentMethods was not honoured"
      Just ms -> case invoiceSignal now giId giStatus ms of
        Left (ProviderError e) -> Right pass {lpSkipped = (Just giId, e) : lpSkipped pass}
        Right Nothing -> Right pass
        Right (Just sig) -> Right pass {lpMoved = (giId, sig) : lpMoved pass}

-- | The id of an invoice we could not otherwise read, so the skip can name it. Without one the
-- pass counts as unaccounted for, which is what holds the sweep back.
invoiceIdOf :: J.Value -> Maybe Text
invoiceIdOf v = case J.fromJSON v :: J.Result (KM.KeyMap J.Value) of
  J.Success o -> case KM.lookup "id" o of
    Just (J.String i) -> Just i
    _ -> Nothing
  J.Error _ -> Nothing

logAdditionalStatus :: Text -> Maybe Text -> IO ()
logAdditionalStatus ref = \case
  Just s | s /= "None" -> logInfo ("btcpay: invoice " <> ref <> " additionalStatus " <> s)
  _ -> pure ()

paymentMethodsSignal :: UTCTime -> Text -> Text -> LB.ByteString -> Either ProviderError (Maybe PaymentSignal)
paymentMethodsSignal now ref status body =
  decodeGreenfield "read invoice" body >>= invoiceSignal now ref status

-- | There is no @Complete@ case: that is the old API's name and would never match. An
-- unknown status is an error, since 'Nothing' would claim the invoice had not changed.
invoiceSignal :: UTCTime -> Text -> Text -> [GPaymentMethod] -> Either ProviderError (Maybe PaymentSignal)
invoiceSignal now ref status ms = do
  m@GPaymentMethod {gpmRate, gpmPaid, gpmDue} <- chooseMethod ref ms
  rate <- priced "rate" gpmRate
  paid <- priced "paymentMethodPaid" gpmPaid
  let received = receivedOf rate paid gpmDue
  case status of
    "Settled" -> Right (Just (SigSettled received (fromMaybe now (latestSettledAt m))))
    "Processing" -> Right (Just (SigFunded received PaidInFull))
    "Expired" -> Right (Just (SigClosed received))
    "Invalid" -> Right (Just (SigClosed received))
    "New" -> Right (SigFunded received PaidInPart <$ rcvCrypto received)
    other -> Left (ProviderError ("btcpay invoice " <> ref <> ": unknown status " <> other))
  where
    priced field =
      maybe (Left (ProviderError ("btcpay invoice " <> ref <> ": its payment method carries no " <> field))) Right

chooseMethod :: Text -> [GPaymentMethod] -> Either ProviderError GPaymentMethod
chooseMethod ref ms = case filter ((`elem` knownMethodIds) . gpmId) ms of
  m : _ -> Right m
  [] ->
    Left . ProviderError $
      "btcpay invoice "
        <> ref
        <> ": no known payment method in ["
        <> T.intercalate ", " (map gpmId ms)
        <> "]; this build knows "
        <> T.intercalate ", " knownMethodIds

findMethod :: Text -> Text -> Text -> LB.ByteString -> Either ProviderError GPaymentMethod
findMethod what ref methodId body = do
  ms <- decodeGreenfield what body
  case find ((methodId ==) . gpmId) ms of
    Just m -> Right m
    Nothing ->
      Left . ProviderError $
        what
          <> ": invoice "
          <> ref
          <> " offers no "
          <> methodId
          <> ", only ["
          <> T.intercalate ", " (map gpmId ms)
          <> "]"

receivedOf :: WireNum -> WireNum -> Maybe WireNum -> Received
receivedOf rate paid due =
  Received
    { rcvAmount = toMinorUnits (wnValue paid * wnValue rate),
      rcvCrypto = if wnValue paid > 0 then Just (wnText paid) else Nothing,
      rcvDue = wnText <$> due
    }

-- | Rounds half to even, so half a cent does not become a cent nobody sent. The clamp
-- stops a wildly wrong figure wrapping a Word32 and coming out small.
toMinorUnits :: Scientific -> CurrencyAmount
toMinorUnits s = CurrencyAmount (fromInteger (max 0 (min largestAmount (round (s * 100)))))
  where
    largestAmount = toInteger (maxBound :: Word32)

latestSettledAt :: GPaymentMethod -> Maybe UTCTime
latestSettledAt GPaymentMethod {gpmPayments} = case mapMaybe settledAt gpmPayments of
  [] -> Nothing
  ts -> Just (posixSecondsToUTCTime (maximum ts))
  where
    settledAt GPayment {gpStatus, gpReceivedDate}
      | gpStatus == "Settled" = Just (fromRational (toRational (wnValue gpReceivedDate)))
      | otherwise = Nothing

sigHeaderName :: HeaderName
sigHeaderName = "BTCPay-Sig"

sigPrefix :: B.ByteString
sigPrefix = "sha256="

-- | Constant-time. The body must be the bytes as they arrived: BTCPay sends its payload
-- indented, so parsing and re-encoding would never match.
verifyBTCPaySig :: Text -> [Header] -> B.ByteString -> Either WebhookError (Maybe Text)
verifyBTCPaySig secret hdrs body = do
  provided <- note "missing BTCPay-Sig header" (lookup sigHeaderName hdrs)
  hex <- note "BTCPay-Sig is not sha256=<hex>" (B.stripPrefix sigPrefix provided)
  given <- case convertFromBase Base16 (B8.map toLower hex) of
    Right (bs :: B.ByteString) -> Right bs
    Left (_ :: String) -> Left (WebhookError "BTCPay-Sig is not hex")
  if constEq expected given
    then Right actedOn
    else Left (WebhookError "BTCPay-Sig does not verify")
  where
    expected :: Digest SHA256
    expected = hmacGetDigest (hmac (TE.encodeUtf8 secret) body :: HMAC SHA256)
    note e = maybe (Left (WebhookError e)) Right
    actedOn = do
      GEvent {geType, geInvoiceId} <- J.decodeStrict' body
      if geType `elem` actedOnEventTypes then Just geInvoiceId else Nothing

greenfield :: BTCPayEnv -> Text -> Method -> [Text] -> Query -> Maybe J.Value -> IO (Either ProviderError LB.ByteString)
greenfield BTCPayEnv {beCfg, beManager} what verb segments query body = do
  r <- try $ do
    req <- parseRequest (T.unpack url)
    let asked =
          req
            { method = verb,
              requestHeaders =
                [ ("Authorization", "token " <> TE.encodeUtf8 (bApiKey beCfg)),
                  ("Accept", "application/json"),
                  ("Content-Type", "application/json")
                ],
              requestBody = RequestBodyLBS (maybe LB.empty J.encode body)
            }
    -- read to a bound rather than whole: a page of invoices is tens of kilobytes, and a
    -- provider answering with something enormous would otherwise be held in memory entire
    -- on the poller thread, which is the one thread that settles orders
    withResponse asked beManager $ \resp -> do
      taken <- brReadSome (responseBody resp) (fromIntegral maxProviderBytes + 1)
      pure (statusCode (responseStatus resp), taken)
  pure $ case r of
    -- http-client hides the Authorization header when showing a Request
    Left (e :: HttpException) -> Left (ProviderError (what <> " failed: " <> tshow e))
    Right (code, taken)
      | LB.length taken > maxProviderBytes ->
          Left . ProviderError $
            what <> " failed: HTTP " <> tshow code <> ", and the answer is over " <> tshow maxProviderBytes <> " bytes"
      | code >= 200 && code < 300 -> Right taken
      | otherwise ->
          Left . ProviderError $
            what <> " failed: HTTP " <> tshow code <> " " <> snippet taken
  where
    url =
      T.dropWhileEnd (== '/') (bHost beCfg)
        <> "/api/v1/stores/"
        <> T.intercalate "/" (map escape (bStoreId beCfg : segments))
        <> TE.decodeUtf8 (renderQuery True query)
    escape = TE.decodeUtf8 . urlEncode False . TE.encodeUtf8
    -- BTCPay answers a refusal with its own creation log inline, and the reason is at the end
    -- of it. Truncating to a couple of hundred bytes hid why a sale could not be made.
    snippet = TE.decodeUtf8With (\_ _ -> Just '?') . LB.toStrict . LB.take maxErrorBytes

decodeGreenfield :: J.FromJSON a => Text -> LB.ByteString -> Either ProviderError a
decodeGreenfield what body = case J.eitherDecode' body of
  Right v -> Right v
  Left e -> Left (ProviderError (what <> ": could not read the response: " <> T.pack e))
