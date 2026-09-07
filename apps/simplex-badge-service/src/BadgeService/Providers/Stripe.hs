{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.Providers.Stripe
  ( stripeProvider,
    signalOf,
    SessionRead (..),
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
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
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

-- | A Checkout Session is a couple of kilobytes; ten megabytes is far above anything Stripe
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
    <> " pages, so any session past the first "
    <> tshow (maxListPages * listPageSize)
    <> " was not read — and will not be read by a later pass either"

-- | Stripe pages by the last row's id. A page whose last row carries no id leaves no cursor, so
-- the rest cannot be walked; recorded rather than reported as a clean, fully accounted pass.
untraversableReason :: Text
untraversableReason =
  "stripe: the list reports more pages but the last row carries no id to page from, so any session past this page was not read"

-- | The events worth queueing a read for. Anything else Stripe sends says nothing this service
-- acts on, and a hint it cannot use costs a queue slot.
actedOnStripeEvents :: [Text]
actedOnStripeEvents = ["checkout.session.completed", "checkout.session.expired"]

sigHeaderName :: HeaderName
sigHeaderName = "Stripe-Signature"

listWhat :: Text
listWhat = "list sessions"

productName :: ByteString
productName = "SimpleX supporter badge"

-- | @ui_mode=elements@ and the response shapes this adapter parses require this API version or
-- later (before it the value was @custom@). Pinning it keeps create off the account's default
-- version, which could be older and would 400 the create.
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

-- | Lists sessions created within the settle window and pages with @starting_after@ while Stripe
-- reports more. Not filtered on @status=open@: that would return only open sessions, which carry
-- no signal, so a settlement missed by the webhook would never be caught. The window mirrors the
-- BTCPay list: the settle window plus a session's own lifetime, since a session created that long
-- ago can still be paid. List rows carry no expanded charge, so a settled row settles at read time
-- via 'signalOf'. A non-2xx from the shared helper, a 429 among them, aborts the whole pass: a
-- payment may have landed where a partial list cannot see it, and the poller must not expire an
-- order over money missed.
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
          got <- stripeApi env listWhat methodGet ["v1", "checkout", "sessions"] q Nothing
          case got >>= decodeStripe listWhat of
            Left e -> pure (Left e)
            Right SessionList {slData, slHasMore} ->
              let acc' = merge acc (sessionsPass now slData)
               in if slHasMore
                    then case lastId slData of
                      Just next -> go now createdGte (Just next) (pagesLeft - 1) acc'
                      Nothing -> pure (Right acc' {lpSkipped = lpSkipped acc' <> [(Nothing, untraversableReason)]})
                    else pure (Right acc')
    merge a b = ListPass {lpMoved = lpMoved a <> lpMoved b, lpSkipped = lpSkipped a <> lpSkipped b}

-- | One session at a time, so a single row this build cannot read is skipped rather than failing
-- the whole page: a row with no @id@ is a skip the pass cannot name, one whose status this build
-- does not know is a skip that names it. No row fails the pass, so this is total.
sessionsPass :: UTCTime -> [J.Value] -> ListPass
sessionsPass now = foldr add (ListPass [] [])
  where
    add v pass = case J.fromJSON v :: J.Result SessionRead of
      J.Error e -> pass {lpSkipped = (sessionIdOf v, T.pack e) : lpSkipped pass}
      J.Success sr -> case signalOf now sr of
        Left (ProviderError e) -> pass {lpSkipped = (Just (srId sr), e) : lpSkipped pass}
        Right Nothing -> pass
        Right (Just sig) -> pass {lpMoved = (srId sr, sig) : lpMoved pass}

-- | The id of a session we could not otherwise read, so the skip can name it.
sessionIdOf :: J.Value -> Maybe Text
sessionIdOf v = case J.fromJSON v :: J.Result (KM.KeyMap J.Value) of
  J.Success o -> case KM.lookup "id" o of
    Just (J.String i) -> Just i
    _ -> Nothing
  J.Error _ -> Nothing

lastId :: [J.Value] -> Maybe Text
lastId vs = case vs of
  [] -> Nothing
  _ -> sessionIdOf (last vs)

data SessionList = SessionList {slData :: [J.Value], slHasMore :: Bool}

instance J.FromJSON SessionList where
  parseJSON = J.withObject "session list" $ \o ->
    SessionList <$> o J..: "data" <*> o J..:? "has_more" J..!= False

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
      SEvent {seType, seSessionId} <- J.decodeStrict' body
      if seType `elem` actedOnStripeEvents then Just seSessionId else Nothing

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

data SEvent = SEvent {seType :: Text, seSessionId :: Text}

instance J.FromJSON SEvent where
  parseJSON = J.withObject "stripe event" $ \o -> do
    seType <- o J..: "type"
    dataO <- o J..: "data"
    obj <- dataO J..: "object"
    seSessionId <- obj J..: "id"
    pure SEvent {seType, seSessionId}

data CreatedSession = CreatedSession {csId :: Text, csClientSecret :: Text}

instance J.FromJSON CreatedSession where
  parseJSON = J.withObject "checkout session" $ \o ->
    CreatedSession <$> o J..: "id" <*> o J..: "client_secret"

createInvoice :: StripeEnv -> ServicePaymentMethod -> OrderDraft -> IO (Either ProviderError ProviderInvoice)
createInvoice _ (SPMCrypto _) _ = pure (Left (ProviderError "stripe offers no crypto payment method"))
createInvoice env (SPMCard CPStripe) OrderDraft {odAmount = CurrencyAmount minor, odCurrency} = do
  now <- getPOSIXTime
  let expiresAt = round now + toInteger (sSessionMinutes (seCfg env)) * toInteger secondsPerMinute :: Integer
      form =
        [ ("mode", "payment"),
          ("ui_mode", "elements"),
          ("expires_at", B8.pack (show expiresAt)),
          -- a fixed address, so Stripe's mandatory confirm email is met without the buyer entering one
          ("customer_email", TE.encodeUtf8 (sReceiptEmail (seCfg env))),
          ("line_items[0][quantity]", "1"),
          ("line_items[0][price_data][currency]", TE.encodeUtf8 (T.toLower odCurrency)),
          ("line_items[0][price_data][unit_amount]", B8.pack (show minor)),
          ("line_items[0][price_data][product_data][name]", productName)
        ]
  created <- stripeApi env what methodPost ["v1", "checkout", "sessions"] [] (Just form)
  pure $ created >>= decodeStripe what <&> \CreatedSession {csId, csClientSecret} ->
    ProviderInvoice {piProviderRef = csId, piDestination = SPDCard CPStripe csClientSecret}
  where
    what = "create session"

cancelInvoice :: StripeEnv -> Text -> IO (Either ProviderError ())
cancelInvoice env sid =
  fmap (fmap (const ())) $ stripeApi env "expire session" methodPost ["v1", "checkout", "sessions", sid, "expire"] [] (Just [])

readInvoice :: StripeEnv -> Text -> IO (Either ProviderError (Maybe PaymentSignal))
readInvoice env sid = do
  now <- getCurrentTime
  got <- stripeApi env what methodGet ["v1", "checkout", "sessions", sid] [("expand[]", Just "payment_intent.latest_charge")] Nothing
  pure $ got >>= decodeStripe what >>= signalOf now
  where
    what = "read session " <> sid

-- | Settlement keys on payment_status read from the provider, never on status alone. There is no
-- card partial, so this never produces 'SigFunded'. Shared with the list pass, so it is top-level
-- and takes the read time (a list row carries no expanded charge).
signalOf :: UTCTime -> SessionRead -> Either ProviderError (Maybe PaymentSignal)
signalOf now SessionRead {srStatus, srPaymentStatus, srAmountTotal, srChargeCreated} =
  case (srStatus, srPaymentStatus) of
    ("complete", "paid") -> Right (Just (SigSettled (received srAmountTotal) settledAt))
    ("complete", _) -> Right Nothing
    -- an expired card session captured nothing (a paid one becomes complete, never expired), so its
    -- received amount is zero: @amount_total@ here is owed, not paid, and a nonzero amount would write
    -- a phantom payment row, since the settlement store only suppresses that row at zero received.
    ("expired", _) -> Right (Just (SigClosed (received 0)))
    ("open", _) -> Right Nothing
    (other, _) -> Left (ProviderError ("stripe session: unknown status " <> other))
  where
    received amt = Received {rcvAmount = amountFrom amt, rcvCrypto = Nothing, rcvDue = Nothing}
    settledAt = maybe now posixSecondsToUTCTime srChargeCreated

-- | Stripe sends the total in minor units as an integer. The clamp stops a wildly wrong figure
-- wrapping a Word32 and coming out small.
amountFrom :: Int64 -> CurrencyAmount
amountFrom n = CurrencyAmount (fromInteger (max 0 (min largestAmount (toInteger n))))
  where
    largestAmount = toInteger (maxBound :: Word32)

data SessionRead = SessionRead
  { srId :: Text,
    srStatus :: Text,
    srPaymentStatus :: Text,
    srAmountTotal :: Int64,
    srChargeCreated :: Maybe POSIXTime
  }

instance J.FromJSON SessionRead where
  parseJSON = J.withObject "checkout session" $ \o -> do
    srId <- o J..: "id"
    srStatus <- o J..: "status"
    srPaymentStatus <- o J..:? "payment_status" J..!= "unpaid"
    srAmountTotal <- o J..:? "amount_total" J..!= 0
    pi_ <- o J..:? "payment_intent"
    let srChargeCreated = fmap fromInteger (chargeCreated pi_)
    pure SessionRead {srId, srStatus, srPaymentStatus, srAmountTotal, srChargeCreated}

chargeCreated :: Maybe J.Value -> Maybe Integer
chargeCreated = \case
  Just (J.Object p) -> case KM.lookup "latest_charge" p of
    Just (J.Object c) -> KM.lookup "created" c >>= asInteger
    _ -> Nothing
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
