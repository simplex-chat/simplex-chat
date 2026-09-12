{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Providers.Stripe
  ( stripeProvider,
    signalOf,
    IntentRead (..),
  )
where

import BadgeService.Config (StripeConfig (..))
import BadgeService.Providers
  ( ListPass (..),
    OrderDraft (..),
    PaymentSignal (..),
    Provider (..),
    ProviderError (..),
    ProviderInvoice (..),
    Received (..),
    settleWindow,
    WebhookError (..),
  )
import Control.Exception (try)
import Crypto.Hash (Digest, SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
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
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word32)
import Network.HTTP.Client
  ( HttpException,
    Manager,
    Request (..),
    RequestBody (..),
    Response (..),
    applyBasicAuth,
    brReadSome,
    parseRequest,
    withResponse,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Header, HeaderName, Method, Query, Status (..), methodGet, methodPost, renderQuery, renderSimpleQuery)
import Simplex.Chat.PaymentService.Types
  ( CardProvider (..),
    CurrencyAmount (..),
    PaymentProvider (..),
    ServicePaymentDestination (..),
    ServicePaymentMethod (..),
  )
import Simplex.Messaging.Util (safeDecodeUtf8, tshow)

maxErrorBytes :: Int64
maxErrorBytes = 4000

-- | A PaymentIntent is a couple of kilobytes; ten megabytes is far above anything Stripe
-- sends and far below what would cost the poller its thread.
maxProviderBytes :: Int64
maxProviderBytes = 10 * 1024 * 1024

secondsPerMinute :: Int
secondsPerMinute = 60

listPageSize :: Int
listPageSize = 100

-- | A server that kept returning full pages, which it would if it ignored @limit@, would keep
-- this pass running and the poller would never reach its expiry sweep.
maxListPages :: Int
maxListPages = 50

pageCapReason :: Text
pageCapReason =
  "stripe: the list stopped at "
    <> tshow maxListPages
    <> " pages, so any intent past the first "
    <> tshow (maxListPages * listPageSize)
    <> " was not read — and will not be read by a later pass either"

-- | Stripe pages by the last row's id. A page whose last row carries no id leaves no cursor, so
-- the rest cannot be walked; recorded rather than reported as a clean, fully accounted pass.
untraversableReason :: Text
untraversableReason =
  "stripe: the list reports more pages but the last row carries no id to page from, so any intent past this page was not read"

-- | The events worth queueing a read for. Anything else Stripe sends says nothing this service
-- acts on, and a hint it cannot use costs a queue slot.
actedOnStripeEvents :: [Text]
actedOnStripeEvents = ["payment_intent.succeeded", "payment_intent.payment_failed", "payment_intent.canceled"]

sigHeaderName :: HeaderName
sigHeaderName = "Stripe-Signature"

listWhat :: Text
listWhat = "list intents"

-- | Pins every call off the account's default version, so the PaymentIntent response shapes this
-- adapter parses do not shift under it.
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
        pVerifyWebhook = verifyStripeSig (sWebhookSecret cfg)
      }

-- | Lists PaymentIntents created within the settle window and pages with @starting_after@ while
-- Stripe reports more. The list carries every status and 'signalOf' classifies each row, so a
-- settlement the webhook missed is still caught. The window mirrors the BTCPay list: the settle
-- window plus an invoice's own lifetime, since an intent created that long ago can still be paid.
-- List rows carry no expanded charge, so a settled row settles at read time via 'signalOf'. A
-- non-2xx from the shared helper, a 429 among them, aborts the whole pass: a payment may have
-- landed where a partial list cannot see it, and the poller must not expire an order over money
-- missed.
listOpen :: StripeEnv -> IO (Either ProviderError ListPass)
listOpen env@StripeEnv {seCfg} = do
  now <- getCurrentTime
  let oldest = addUTCTime (negate (settleWindow + fromIntegral (sSessionMinutes seCfg * secondsPerMinute))) now
      createdGte = B8.pack (show (unixSeconds oldest))
  go now createdGte Nothing maxListPages (ListPass [] [])
  where
    unixSeconds :: UTCTime -> Integer
    unixSeconds = floor . utcTimeToPOSIXSeconds
    go now createdGte after pagesLeft acc
      | pagesLeft <= 0 = pure (Right acc {lpSkipped = lpSkipped acc <> [(Nothing, pageCapReason)]})
      | otherwise = do
          let q =
                [("created[gte]", Just createdGte), ("limit", Just (B8.pack (show listPageSize)))]
                  <> maybe [] (\a -> [("starting_after", Just (TE.encodeUtf8 a))]) after
          got <- stripeApi env listWhat methodGet ["v1", "payment_intents"] q Nothing
          case got >>= decodeStripe listWhat of
            Left e -> pure (Left e)
            Right IntentList {ilData, ilHasMore} ->
              let acc' = merge acc (intentsPass now ilData)
               in if ilHasMore
                    then case lastId ilData of
                      Just next -> go now createdGte (Just next) (pagesLeft - 1) acc'
                      Nothing -> pure (Right acc' {lpSkipped = lpSkipped acc' <> [(Nothing, untraversableReason)]})
                    else pure (Right acc')
    merge a b = ListPass {lpMoved = lpMoved a <> lpMoved b, lpSkipped = lpSkipped a <> lpSkipped b}

-- | One intent at a time, so a single row this build cannot read is skipped rather than failing
-- the whole page: a row with no @id@ is a skip the pass cannot name, one whose status this build
-- does not know is a skip that names it. No row fails the pass, so this is total.
intentsPass :: UTCTime -> [J.Value] -> ListPass
intentsPass now = foldr add (ListPass [] [])
  where
    add v pass = case J.fromJSON v :: J.Result IntentRead of
      J.Error e -> pass {lpSkipped = (intentIdOf v, T.pack e) : lpSkipped pass}
      J.Success ir -> case signalOf now ir of
        Left (ProviderError e) -> pass {lpSkipped = (Just (irId ir), e) : lpSkipped pass}
        Right Nothing -> pass
        Right (Just sig) -> pass {lpMoved = (irId ir, sig) : lpMoved pass}

-- | The id of an intent we could not otherwise read, so the skip can name it.
intentIdOf :: J.Value -> Maybe Text
intentIdOf v = case J.fromJSON v :: J.Result (KM.KeyMap J.Value) of
  J.Success o -> case KM.lookup "id" o of
    Just (J.String i) -> Just i
    _ -> Nothing
  J.Error _ -> Nothing

lastId :: [J.Value] -> Maybe Text
lastId vs = case vs of
  [] -> Nothing
  _ -> intentIdOf (last vs)

data IntentList = IntentList {ilData :: [J.Value], ilHasMore :: Bool}

instance J.FromJSON IntentList where
  parseJSON = J.withObject "intent list" $ \o ->
    IntentList <$> o J..: "data" <*> o J..:? "has_more" J..!= False

-- | Constant-time over the raw bytes. Stripe signs @"{t}.{body}"@, so the signed payload is the
-- timestamp, a literal dot, then the body exactly as it arrived. The interface is pure and holds
-- no clock, so the 300 s replay window Stripe documents is not enforced here; that check is
-- deferred to a layer that has the current time, exactly as the BTCPay adapter leaves it.
verifyStripeSig :: Text -> [Header] -> ByteString -> Either WebhookError (Maybe Text)
verifyStripeSig secret hdrs body = do
  raw <- note "missing Stripe-Signature header" (lookup sigHeaderName hdrs)
  (t, v1hexes) <- note "Stripe-Signature is not t=..,v1=.." (parseStripeSig raw)
  givens <- mapM decodeHex v1hexes
  let expected :: Digest SHA256
      expected = hmacGetDigest (hmac (TE.encodeUtf8 secret) (t <> "." <> body) :: HMAC SHA256)
  -- during signing-secret rotation Stripe sends one v1 per active secret; any match verifies
  if any (constEq expected) givens
    then Right actedOn
    else Left (WebhookError "Stripe-Signature does not verify")
  where
    note e = maybe (Left (WebhookError e)) Right
    decodeHex v1hex = case convertFromBase Base16 (B8.map toLower v1hex) of
      Right (bs :: ByteString) -> Right bs
      Left (_ :: String) -> Left (WebhookError "Stripe-Signature v1 is not hex")
    actedOn = do
      SEvent {seType, seRef} <- J.decodeStrict' body
      if seType `elem` actedOnStripeEvents then Just seRef else Nothing

-- | Reads @t@ and every @v1@ from Stripe's comma-separated @k=v@ header. A rotation sends one
-- @v1@ per active secret, so all are kept. Extra schemes (@v0@, and the rest) are ignored; a
-- missing @t@ or no @v1@ at all is a malformed header.
parseStripeSig :: ByteString -> Maybe (ByteString, [ByteString])
parseStripeSig raw = do
  let pairs = [(k, B8.drop 1 rest) | part <- B8.split ',' raw, let (k, rest) = B8.break (== '=') part, not (B8.null rest)]
  t <- lookup "t" pairs
  case [v | ("v1", v) <- pairs] of
    [] -> Nothing
    v1s -> Just (t, v1s)

-- | The PaymentIntent id (@data.object.id@, a @pi_…@) an acted-on event carries, which the poller
-- reads back. The provider ref is that id, so no metadata round-trip is needed.
data SEvent = SEvent {seType :: Text, seRef :: Text}

instance J.FromJSON SEvent where
  parseJSON = J.withObject "stripe event" $ \o -> do
    seType <- o J..: "type"
    dataO <- o J..: "data"
    obj <- dataO J..: "object"
    seRef <- obj J..: "id"
    pure SEvent {seType, seRef}

data CreatedIntent = CreatedIntent {ciId :: Text, ciClientSecret :: Text}

instance J.FromJSON CreatedIntent where
  parseJSON = J.withObject "payment_intent" $ \o ->
    CreatedIntent <$> o J..: "id" <*> o J..: "client_secret"

createInvoice :: StripeEnv -> ServicePaymentMethod -> OrderDraft -> IO (Either ProviderError ProviderInvoice)
createInvoice _ (SPMCrypto _) _ = pure (Left (ProviderError "stripe offers no crypto payment method"))
createInvoice env (SPMCard CPStripe) OrderDraft {odAmount = CurrencyAmount minor, odCurrency} = do
  let form =
        [ ("amount", B8.pack (show minor)),
          ("currency", TE.encodeUtf8 (T.toLower odCurrency)),
          -- card only: no redirect-based method is offered, so the client confirm never navigates the
          -- top window; the buyer stays in the embedded frame
          ("allowed_payment_method_types[]", "card")
        ]
  created <- stripeApi env what methodPost ["v1", "payment_intents"] [] (Just form)
  pure $ created >>= decodeStripe what <&> \CreatedIntent {ciId, ciClientSecret} ->
    ProviderInvoice {piProviderRef = ciId, piDestination = SPDCard CPStripe ciClientSecret}
  where
    what = "create intent"

cancelInvoice :: StripeEnv -> Text -> IO (Either ProviderError ())
cancelInvoice env pid =
  fmap (fmap (const ())) $ stripeApi env "cancel intent" methodPost ["v1", "payment_intents", pid, "cancel"] [] (Just [])

readInvoice :: StripeEnv -> Text -> IO (Either ProviderError (Maybe PaymentSignal))
readInvoice env pid = do
  now <- getCurrentTime
  got <- stripeApi env what methodGet ["v1", "payment_intents", pid] [("expand[]", Just "latest_charge")] Nothing
  pure $ got >>= decodeStripe what >>= signalOf now
  where
    what = "read intent " <> pid

-- | Settlement keys on the PaymentIntent status read from the provider, never on status alone in the
-- record. There is no card partial, so this never produces 'SigFunded'. Shared with the list pass, so
-- it is top-level and takes the read time (a list row carries no expanded charge). @amount_received@
-- is what the buyer actually paid; a canceled intent captured nothing, so its received amount is zero.
signalOf :: UTCTime -> IntentRead -> Either ProviderError (Maybe PaymentSignal)
signalOf now IntentRead {irStatus, irAmountReceived, irChargeCreated} =
  case irStatus of
    "succeeded" -> Right (Just (SigSettled (received irAmountReceived) settledAt))
    "canceled" -> Right (Just (SigClosed (received 0)))
    s | s `elem` openStatuses -> Right Nothing
      | otherwise -> Left (ProviderError ("stripe payment_intent: unknown status " <> s))
  where
    openStatuses = ["requires_payment_method", "requires_confirmation", "requires_action", "processing", "requires_capture"]
    received amt = Received {rcvAmount = amountFrom amt, rcvCrypto = Nothing, rcvDue = Nothing}
    settledAt = maybe now posixSecondsToUTCTime irChargeCreated

-- | Stripe sends the total in minor units as an integer. The clamp stops a wildly wrong figure
-- wrapping a Word32 and coming out small.
amountFrom :: Int64 -> CurrencyAmount
amountFrom n = CurrencyAmount (fromInteger (max 0 (min largestAmount (toInteger n))))
  where
    largestAmount = toInteger (maxBound :: Word32)

data IntentRead = IntentRead
  { irId :: Text,
    irStatus :: Text,
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
stripeApi StripeEnv {seCfg, seManager} what verb segments query form = do
  r <- try $ do
    req0 <- parseRequest (T.unpack url)
    -- applyBasicAuth appends the Authorization header, so it must wrap the record update rather
    -- than precede it: updating requestHeaders after it would drop the header it added.
    let req =
          applyBasicAuth (TE.encodeUtf8 (sSecretKey seCfg)) "" $
            req0
              { method = verb,
                requestHeaders =
                  ("Accept", "application/json")
                    : ("Stripe-Version", stripeApiVersion)
                    : [("Content-Type", "application/x-www-form-urlencoded") | isJust form],
                requestBody = RequestBodyBS (maybe "" (renderSimpleQuery False) form)
              }
    withResponse req seManager $ \resp -> do
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
      T.dropWhileEnd (== '/') (sHost seCfg)
        <> "/"
        <> T.intercalate "/" segments
        <> TE.decodeUtf8 (renderQuery True query)
    snippet = safeDecodeUtf8 . LB.toStrict . LB.take maxErrorBytes

decodeStripe :: J.FromJSON a => Text -> LB.ByteString -> Either ProviderError a
decodeStripe what body = case J.eitherDecode' body of
  Right v -> Right v
  Left e -> Left (ProviderError (what <> ": could not read the response: " <> T.pack e))
