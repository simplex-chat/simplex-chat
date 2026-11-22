{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module StoreService.Providers.Stripe
  ( stripeProvider,
    signalOf,
    ChargeStatus (..),
    IntentStatus (..),
    IntentRead (..),
  )
where

import StoreService.Config (StripeConfig (..))
import StoreService.Providers
  ( Funded (..),
    ListPass (..),
    OrderDraft (..),
    PaymentSignal (..),
    Provider (..),
    ProviderError (..),
    ProviderInvoice (..),
    Received (..),
    Reversal (..),
    decodeAnswer,
    objectId,
    providerCall,
    providerErrorText,
    settleWindow,
    WebhookError (..),
    WebhookEvent (..),
  )
import Control.Logger.Simple (logWarn)
import Control.Monad (unless)
import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock.System (systemSeconds, utcToSystemTime)
import Data.Word (Word32)
import Network.HTTP.Client (Manager, Request (..), RequestBody (..), applyBasicAuth)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Header, HeaderName, Method, Query, methodGet, methodPost, renderQuery, renderSimpleQuery)
import Simplex.Chat.PaymentService.Types
  ( CardProvider (..),
    CurrencyAmount (..),
    PaymentProvider (..),
    ServicePaymentDestination (..),
    ServicePaymentMethod (..),
  )
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix)
import Simplex.Messaging.Util (tshow)

data ChargeStatus = CHSucceeded | CHPending | CHFailed | CHUnknown Text
  deriving (Eq, Show)

data IntentStatus
  = PIRequiresPaymentMethod
  | PIRequiresConfirmation
  | PIRequiresAction
  | PIProcessing
  | PIRequiresCapture
  | PICanceled
  | PISucceeded
  | PIUnknown Text
  deriving (Eq, Show)

$(JQ.deriveFromJSON (defaultJSON {J.sumEncoding = J.UntaggedValue, J.constructorTagModifier = dropPrefix "CH"}) ''ChargeStatus)

$(JQ.deriveFromJSON (defaultJSON {J.sumEncoding = J.UntaggedValue, J.constructorTagModifier = J.camelTo2 '_' . dropPrefix "PI"}) ''IntentStatus)

secondsPerMinute :: Int
secondsPerMinute = 60

listPageSize :: Int
listPageSize = 100

-- Caps a server that ignores @limit@, so the poller still reaches its expiry sweep.
maxListPages :: Int
maxListPages = 50

pageCapReason :: Text
pageCapReason =
  "stripe: the list stopped at "
    <> tshow maxListPages
    <> " pages, so any intent past the first "
    <> tshow (maxListPages * listPageSize)
    <> " was not read — and will not be read by a later pass either"

untraversableReason :: Text
untraversableReason =
  "stripe: the list reports more pages but the last row carries no id to page from, so any intent past this page was not read"

actedOnStripeEvents :: [Text]
actedOnStripeEvents = ["payment_intent.succeeded", "payment_intent.payment_failed", "payment_intent.canceled"]

sigHeaderName :: HeaderName
sigHeaderName = "Stripe-Signature"

listWhat :: Text
listWhat = "list intents"

-- Pin the API version so response shapes this adapter parses do not shift under it.
stripeApiVersion :: ByteString
stripeApiVersion = "2026-03-25.dahlia"

data StripeEnv = StripeEnv {seCfg :: StripeConfig, seManager :: Manager}

stripeProvider :: StripeConfig -> IO Provider
stripeProvider cfg = do
  seManager <- newTlsManager
  let env = StripeEnv {seCfg = cfg, seManager}
  pure
    Provider
      { pProvider = PPStripe,
        pCreateInvoice = createInvoice env,
        pReadInvoice = readInvoice env,
        pCancelInvoice = cancelInvoice env,
        pListOpen = listOpen env,
        pCurrentRate = Nothing,
        pVerifyWebhook = verifyStripeSig (sWebhookSecret cfg)
      }

listOpen :: StripeEnv -> IO (Either ProviderError ListPass)
listOpen env@StripeEnv {seCfg} = do
  now <- getCurrentTime
  let oldest = addUTCTime (negate (settleWindow + fromIntegral (sSessionMinutes seCfg * secondsPerMinute))) now
      createdGte = B8.pack (show (systemSeconds $ utcToSystemTime oldest))
  go now createdGte Nothing maxListPages (ListPass [] [])
  where
    go now createdGte after pagesLeft acc
      | pagesLeft <= 0 = pure (Right acc {lpSkipped = lpSkipped acc <> [(Nothing, pageCapReason)]})
      | otherwise = do
          let q =
                [("created[gte]", Just createdGte), ("limit", Just (B8.pack (show listPageSize)))]
                  <> maybe [] (\a -> [("starting_after", Just (TE.encodeUtf8 a))]) after
          got <- stripeApi env listWhat methodGet ["v1", "payment_intents"] q Nothing
          case got >>= decodeAnswer listWhat of
            Left e -> pure (Left e)
            Right IntentList {ilData, ilHasMore} ->
              let acc' = merge acc (intentsPass now ilData)
               in if ilHasMore
                    then case lastId ilData of
                      Just next -> go now createdGte (Just next) (pagesLeft - 1) acc'
                      Nothing -> pure (Right acc' {lpSkipped = lpSkipped acc' <> [(Nothing, untraversableReason)]})
                    else pure (Right acc')
    merge a b = ListPass {lpMoved = lpMoved a <> lpMoved b, lpSkipped = lpSkipped a <> lpSkipped b}

intentsPass :: UTCTime -> [J.Value] -> ListPass
intentsPass now = foldr add (ListPass [] [])
  where
    add v pass = case J.fromJSON v :: J.Result IntentRead of
      J.Error e -> pass {lpSkipped = (objectId v, T.pack e) : lpSkipped pass}
      J.Success ir -> case signalOf now ir of
        Left e -> pass {lpSkipped = (Just (irId ir), providerErrorText e) : lpSkipped pass}
        Right Nothing -> pass
        Right (Just sig) -> pass {lpMoved = (irId ir, sig) : lpMoved pass}

lastId :: [J.Value] -> Maybe Text
lastId vs = case vs of
  [] -> Nothing
  _ -> objectId (last vs)

data IntentList = IntentList {ilData :: [J.Value], ilHasMore :: Bool}

instance J.FromJSON IntentList where
  parseJSON = J.withObject "intent list" $ \o ->
    IntentList <$> o J..: "data" <*> o J..:? "has_more" J..!= False

-- | Stripe signs each retry again with the current time, so an old timestamp means a replayed request.
webhookTolerance :: NominalDiffTime
webhookTolerance = 15 * 60

-- Constant-time over the raw bytes. Stripe signs "{t}.{body}", so hash the timestamp, a dot, then the body as it arrived.
verifyStripeSig :: Text -> UTCTime -> [Header] -> ByteString -> Either WebhookError (Maybe WebhookEvent)
verifyStripeSig secret now hdrs body = do
  raw <- note "missing Stripe-Signature header" (lookup sigHeaderName hdrs)
  (t, v1hexes) <- note "Stripe-Signature is not t=..,v1=.." (parseStripeSig raw)
  givens <- mapM decodeHex v1hexes
  let expected :: Digest SHA256
      expected = hmacGetDigest (hmac (TE.encodeUtf8 secret) (t <> "." <> body) :: HMAC SHA256)
  -- during signing-secret rotation Stripe sends one v1 per active secret; any match verifies
  unless (any (constEq expected) givens) $ Left (WebhookError "Stripe-Signature does not verify")
  signedAt <- note "Stripe-Signature t is not a whole number of seconds" (fullInt t)
  if abs (diffUTCTime now (posixSecondsToUTCTime (fromIntegral signedAt))) <= webhookTolerance
    then Right actedOn
    else Left (WebhookStale ("Stripe-Signature t is more than " <> tshow (round webhookTolerance `div` 60 :: Int) <> " minutes from now; check the server clock"))
  where
    fullInt s = case B8.readInt s of
      Just (n, rest) | B8.null rest -> Just n
      _ -> Nothing
    note e = maybe (Left (WebhookError e)) Right
    decodeHex v1hex = case convertFromBase Base16 (B8.map toLower v1hex) of
      Right (bs :: ByteString) -> Right bs
      Left (_ :: String) -> Left (WebhookError "Stripe-Signature v1 is not hex")
    actedOn = J.decodeStrict' body >>= stripeEvent

parseStripeSig :: ByteString -> Maybe (ByteString, [ByteString])
parseStripeSig raw = do
  let pairs = [(k, B8.drop 1 rest) | part <- B8.split ',' raw, let (k, rest) = B8.break (== '=') part, not (B8.null rest)]
  t <- lookup "t" pairs
  case [v | ("v1", v) <- pairs] of
    [] -> Nothing
    v1s -> Just (t, v1s)

data SEvent = SEvent {seId :: Maybe Text, seType :: Text, seObject :: J.Object}

instance J.FromJSON SEvent where
  parseJSON = J.withObject "stripe event" $ \o -> do
    seId <- o J..:? "id"
    seType <- o J..: "type"
    dataO <- o J..: "data"
    seObject <- dataO J..: "object"
    pure SEvent {seId, seType, seObject}

stripeEvent :: SEvent -> Maybe WebhookEvent
stripeEvent SEvent {seId, seType, seObject}
  | seType `elem` actedOnStripeEvents = WEPayment <$> field "id"
  | otherwise = case seType of
      "charge.dispute.created" -> reversal RVDispute "amount"
      "charge.refunded" -> reversal RVRefund "amount_refunded"
      "radar.early_fraud_warning.created" -> WEFraudWarning <$> intent <*> field "fraud_type"
      _ -> Nothing
  where
    field :: J.FromJSON a => J.Key -> Maybe a
    field k = JT.parseMaybe (J..: k) seObject
    intent = field "payment_intent"
    reversal kind amount = WEReversal <$> seId <*> intent <*> (kind <$> field "id" <*> (amountFrom <$> field amount))

data CreatedIntent = CreatedIntent {ciId :: Text, ciClientSecret :: Text}

instance J.FromJSON CreatedIntent where
  parseJSON = J.withObject "payment_intent" $ \o ->
    CreatedIntent <$> o J..: "id" <*> o J..: "client_secret"

createInvoice :: StripeEnv -> ServicePaymentMethod -> OrderDraft -> IO (Either ProviderError ProviderInvoice)
createInvoice _ (SPMCrypto _) _ = pure (Left (ProviderError "stripe offers no crypto payment method"))
createInvoice env (SPMCard CPStripe) OrderDraft {odAmount = CurrencyAmount minor, odCurrency, odReference} = do
  let amount = B8.pack (show minor)
      currency = TE.encodeUtf8 (T.toLower odCurrency)
      reference = TE.encodeUtf8 odReference
      form =
        [ ("amount", amount),
          ("currency", currency),
          -- Only card is offered, because a redirect-based method would navigate the top window out of the embedded frame.
          ("allowed_payment_method_types[]", "card"),
          ("metadata[order]", reference)
        ]
      checkout = env {seCfg = (seCfg env) {sTimeoutMicros = sCheckoutTimeoutMicros (seCfg env)}}
      create headers = stripeSend checkout what methodPost ["v1", "payment_intents"] [] headers (Just form)
  -- a name order sent again after a lost answer gets the intent its first create made
  (replayed, keyed) <- create [("Idempotency-Key", B8.intercalate ":" [reference, amount, currency])]
  created <- if replayed then replayedIntent (snd <$> create []) keyed else pure keyed
  pure $ created >>= decodeAnswer what <&> \CreatedIntent {ciId, ciClientSecret} ->
    ProviderInvoice {piProviderRef = ciId, piDestination = SPDCard CPStripe ciClientSecret}
  where
    what = "create intent"
    -- Stripe replays a key's first answer for 24 hours, so an error or an intent cancelled since would fail every resend.
    -- An intent that succeeded holds the order's payment, and one that cannot be read may, so neither is replaced.
    replayedIntent fresh keyed = case keyed >>= decodeAnswer what of
      Left _ -> fresh
      Right CreatedIntent {ciId} ->
        readInvoice env ciId >>= \case
          Right (Just SigClosed {}) -> fresh
          Right _ -> pure keyed
          Left e -> pure (Left e)

cancelInvoice :: StripeEnv -> Text -> IO (Either ProviderError ())
cancelInvoice env pid =
  fmap (fmap (const ())) $ stripeApi env "cancel intent" methodPost ["v1", "payment_intents", pid, "cancel"] [] (Just [])

-- | Declined card payments one intent takes before it is cancelled, so stolen cards cannot be tried on it one after another.
maxDeclines :: Int
maxDeclines = 3

-- | An intent waiting for a card after a charge is cancelled once its last maxDeclines charges all failed.
readInvoice :: StripeEnv -> Text -> IO (Either ProviderError (Maybe PaymentSignal))
readInvoice env pid = do
  now <- getCurrentTime
  got <- stripeApi env what methodGet ["v1", "payment_intents", pid] [("expand[]", Just "latest_charge")] Nothing
  case got >>= decodeAnswer what of
    Right intent@IntentRead {irStatus = PIRequiresPaymentMethod, irChargeCreated = Just _} ->
      declines >>= \case
        Right n | n >= maxDeclines -> do
          logWarn $ "stripe: intent " <> pid <> " is cancelled after " <> tshow n <> " declined card payments in a row"
          (Just canceledSignal <$) <$> cancelInvoice env pid
        Right _ -> pure $ signalOf now intent
        Left e -> pure $ Left e
    intent -> pure $ intent >>= signalOf now
  where
    what = "read intent " <> pid
    declines =
      fmap (length . filter (== CHFailed) . clStatuses) . (>>= decodeAnswer "list charges")
        <$> stripeApi env "list charges" methodGet ["v1", "charges"] [("payment_intent", Just $ TE.encodeUtf8 pid), ("limit", Just $ B8.pack $ show maxDeclines)] Nothing

newtype ChargeList = ChargeList {clStatuses :: [ChargeStatus]}

instance J.FromJSON ChargeList where
  parseJSON = J.withObject "charge list" $ \o -> ChargeList <$> (mapM (J.withObject "charge" (J..: "status")) =<< o J..: "data")

signalOf :: UTCTime -> IntentRead -> Either ProviderError (Maybe PaymentSignal)
signalOf now IntentRead {irStatus, irAmountReceived, irChargeCreated} =
  case irStatus of
    PISucceeded -> Right (Just (SigSettled (intentReceived irAmountReceived irAmountReceived) settledAt))
    PICanceled -> Right (Just canceledSignal)
    PIRequiresPaymentMethod -> Right Nothing
    PIRequiresConfirmation -> Right Nothing
    PIRequiresAction -> Right Nothing
    PIProcessing -> Right Nothing
    PIRequiresCapture -> Right Nothing
    PIUnknown s -> Left (ProviderError ("stripe payment_intent: unknown status " <> s))
  where
    settledAt = maybe now posixSecondsToUTCTime irChargeCreated

canceledSignal :: PaymentSignal
canceledSignal = SigClosed (intentReceived 0 0) PaidInPart

-- | Only a succeeded intent's money is confirmed.
intentReceived :: Int64 -> Int64 -> Received
intentReceived amt confirmed = RCard {rcAmount = amountFrom amt, rcConfirmed = amountFrom confirmed}

-- Clamp stops a wildly wrong figure wrapping Word32 and coming out small.
amountFrom :: Int64 -> CurrencyAmount
amountFrom n = CurrencyAmount (fromInteger (max 0 (min largestAmount (toInteger n))))
  where
    largestAmount = toInteger (maxBound :: Word32)

data IntentRead = IntentRead
  { irId :: Text,
    irStatus :: IntentStatus,
    irAmountReceived :: Int64,
    irChargeCreated :: Maybe POSIXTime
  }

instance J.FromJSON IntentRead where
  parseJSON = J.withObject "payment_intent" $ \o -> do
    irId <- o J..: "id"
    irStatus <- o J..: "status"
    irAmountReceived <- o J..:? "amount_received" J..!= 0
    -- expand[]=latest_charge makes this the charge object; unexpanded it is a string id we ignore
    latest <- o J..:? "latest_charge"
    let irChargeCreated = fmap fromInteger (chargeCreated latest)
    pure IntentRead {irId, irStatus, irAmountReceived, irChargeCreated}

chargeCreated :: Maybe J.Value -> Maybe Integer
chargeCreated = \case
  Just (J.Object c) -> KM.lookup "created" c >>= asInteger
  _ -> Nothing

asInteger :: J.Value -> Maybe Integer
asInteger = \case
  J.Number n -> either (const Nothing) Just (floatingOrInteger n :: Either Double Integer)
  _ -> Nothing

stripeApi ::
  StripeEnv ->
  Text ->
  Method ->
  [Text] ->
  Query ->
  Maybe [(ByteString, ByteString)] ->
  IO (Either ProviderError LB.ByteString)
stripeApi env what verb segments query form = snd <$> stripeSend env what verb segments query [] form

-- | The answer, and whether Stripe replayed it as its saved answer to an Idempotency-Key it has seen.
stripeSend ::
  StripeEnv ->
  Text ->
  Method ->
  [Text] ->
  Query ->
  [Header] ->
  Maybe [(ByteString, ByteString)] ->
  IO (Bool, Either ProviderError LB.ByteString)
stripeSend StripeEnv {seCfg, seManager} what verb segments query headers form =
  (\(answered, r) -> (lookup "Idempotent-Replayed" answered == Just "true", r)) <$> providerCall seManager (sTimeoutMicros seCfg) what url asked
  where
    -- applyBasicAuth must wrap the record update, because updating requestHeaders after it would drop the header it adds.
    asked req =
      applyBasicAuth (TE.encodeUtf8 (sSecretKey seCfg)) "" $
        req
          { method = verb,
            requestHeaders =
              ("Accept", "application/json")
                : ("Stripe-Version", stripeApiVersion)
                : [("Content-Type", "application/x-www-form-urlencoded") | isJust form]
                <> headers,
            requestBody = RequestBodyBS (maybe "" (renderSimpleQuery False) form)
          }
    url =
      T.dropWhileEnd (== '/') (sHost seCfg)
        <> "/"
        <> T.intercalate "/" segments
        <> TE.decodeUtf8 (renderQuery True query)
