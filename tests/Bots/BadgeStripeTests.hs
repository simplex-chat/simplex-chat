{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeStripeTests (badgeStripeTests) where

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
import BadgeService.Providers.Stripe (SessionRead (..), signalOf, stripeProvider)
import Bots.FakeStripe
import Control.Monad (join)
import Data.Aeson ((.=))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Network.HTTP.Types (hAuthorization, parseSimpleQuery)
import Text.Read (readMaybe)
import Simplex.Chat.PaymentService.Types
  ( CardProvider (..),
    CryptoCurrency (..),
    CurrencyAmount (..),
    ServicePaymentDestination (..),
    ServicePaymentMethod (..),
  )
import System.Timeout (timeout)
import Test.Hspec

badgeStripeTests :: Spec
badgeStripeTests = describe "badge stripe adapter" $ do
  describe "against the fake Stripe" $ do
    it "creates a checkout session and reads back the client secret" testFakeCreatesCard
    it "sends the create form Stripe documents, over HTTP Basic" testFakeCreateBody
    it "is refused when the secret key is wrong, rather than passing silently" testFakeWrongSecretKey
    it "walks open to complete/unpaid to complete/paid, then cancels" testFakeLifecycle
    it "reads the expand query on every read" testFakeReadExpands
    it "closes on an expired session" testFakeExpired
    it "makes a 500 at checkout a ProviderError, creating nothing" testFakeCreate500
    it "makes a 500 on a read a ProviderError the next read recovers from" testFakeRead500
    it "refuses a crypto method, since Stripe offers none" testRefusesCrypto
    it "makes an unknown session status a ProviderError, not a silent no-signal" testFakeReadUnknownStatus
    it "settles a paid session with no charge at read time" testSettledNoChargeReadTime
  describe "listing sessions to settle" $ do
    it "lists by a created window, never status=open" testFakeListWindow
    it "moves the settled and expired sessions, at read time, leaving the open one" testFakeListsOpen
    it "skips a listed session that carries no id" testFakeListSkipsIdless
    it "walks starting_after across pages, gathering every session" testFakeListPages
    it "records an anomaly when a page ends with no cursor id" testFakeListCursorGap
    it "aborts the whole pass on a 429" testFakeList429
  describe "verifying the webhook signature" $ do
    it "accepts a signed completed event and names its session" testWebhookVerifies
    it "answers a valid but unhandled event with no session" testWebhookUnhandled
    it "accepts any v1 during a rotation, refusing only when none matches" testWebhookRotatedSignature
    it "refuses a missing, malformed, or tampered signature" testWebhookMalformed

fiftyFourDollars :: OrderDraft
fiftyFourDollars = OrderDraft {odAmount = CurrencyAmount 5400, odCurrency = "usd"}

fiftyFourDollarsReceived :: Received
fiftyFourDollarsReceived = Received {rcvAmount = CurrencyAmount 5400, rcvCrypto = Nothing, rcvDue = Nothing}

-- | A closed card session captured nothing, so it carries no received amount.
nothingReceived :: Received
nothingReceived = Received {rcvAmount = CurrencyAmount 0, rcvCrypto = Nothing, rcvDue = Nothing}

-- | The `created` on the paid fixture's latest charge.
fixtureChargeAt :: UTCTime
fixtureChargeAt = posixSecondsToUTCTime 1700000000

exampleCeiling :: Int
exampleCeiling = 20000000

failWith :: HasCallStack => String -> IO a
failWith msg = expectationFailure msg >> error msg

withProvider :: HasCallStack => (FakeStripe -> Provider -> IO a) -> IO a
withProvider action = bounded $ withFakeStripe $ \fake -> stripeProvider (fsConfig fake) >>= action fake
  where
    bounded act = timeout exampleCeiling act >>= maybe (failWith "the fake stripe did not answer within 20s") pure

createdInvoice :: HasCallStack => Provider -> ServicePaymentMethod -> IO ProviderInvoice
createdInvoice p spm =
  pCreateInvoice p spm fiftyFourDollars >>= \case
    Right inv -> pure inv
    Left e -> failWith ("expected an invoice, got " <> show e)

isCardSecret :: ServicePaymentDestination -> Bool
isCardSecret = \case
  SPDCard CPStripe secret -> secret /= ""
  _ -> False

testFakeCreatesCard :: IO ()
testFakeCreatesCard = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef, piDestination} <- createdInvoice p (SPMCard CPStripe)
  piDestination `shouldSatisfy` isCardSecret
  fakeSessionIds fake `shouldReturn` [piProviderRef]
  posts <- apiRequests fake "POST" []
  length posts `shouldBe` 1

testFakeCreateBody :: IO ()
testFakeCreateBody = withProvider $ \fake p -> do
  _ <- createdInvoice p (SPMCard CPStripe)
  posts <- apiRequests fake "POST" []
  case posts of
    [created] -> do
      let form = parseSimpleQuery (LB.toStrict (frBody created))
      lookup "mode" form `shouldBe` Just "payment"
      lookup "ui_mode" form `shouldBe` Just "elements"
      lookup "line_items[0][quantity]" form `shouldBe` Just "1"
      lookup "line_items[0][price_data][currency]" form `shouldBe` Just "usd"
      lookup "line_items[0][price_data][unit_amount]" form `shouldBe` Just "5400"
      lookup "expires_at" form `shouldSatisfy` maybe False (all (`elem` ['0' .. '9']) . B8.unpack)
      lookup hAuthorization (frHeaders created) `shouldSatisfy` maybe False ("Basic " `B8.isPrefixOf`)
    _ -> expectationFailure ("expected one create, got " <> show (length posts))

testFakeWrongSecretKey :: IO ()
testFakeWrongSecretKey = bounded $ withFakeStripe $ \fake -> do
  p <- stripeProvider (fsConfig fake) {sSecretKey = "sk_test_wrong"}
  r <- pCreateInvoice p (SPMCard CPStripe) fiftyFourDollars
  r `shouldSatisfy` namesInError "401"
  fakeSessionIds fake `shouldReturn` []
  where
    bounded act = timeout exampleCeiling act >>= maybe (failWith "the fake stripe did not answer within 20s") pure

testFakeLifecycle :: IO ()
testFakeLifecycle = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = sid} <- createdInvoice p (SPMCard CPStripe)
  pReadInvoice p sid `shouldReturn` Right Nothing
  setSessionState fake sid ["status" .= ("complete" :: Text), "payment_status" .= ("unpaid" :: Text)]
  pReadInvoice p sid `shouldReturn` Right Nothing
  setSessionState fake sid ["payment_status" .= ("paid" :: Text)]
  pReadInvoice p sid `shouldReturn` Right (Just (SigSettled fiftyFourDollarsReceived fixtureChargeAt))
  pCancelInvoice p sid `shouldReturn` Right ()
  expires <- apiRequests fake "POST" [sid, "expire"]
  length expires `shouldBe` 1

testFakeReadExpands :: IO ()
testFakeReadExpands = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = sid} <- createdInvoice p (SPMCard CPStripe)
  _ <- pReadInvoice p sid
  gets <- apiRequests fake "GET" [sid]
  case gets of
    [g] -> lookup "expand[]" (frQuery g) `shouldBe` Just (Just "payment_intent.latest_charge")
    _ -> expectationFailure ("expected one read, got " <> show (length gets))

testFakeExpired :: IO ()
testFakeExpired = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = sid} <- createdInvoice p (SPMCard CPStripe)
  setSessionState fake sid ["status" .= ("expired" :: Text)]
  -- a nonzero received here would write a phantom payment for money the buyer never sent
  pReadInvoice p sid `shouldReturn` Right (Just (SigClosed nothingReceived))

testFakeCreate500 :: IO ()
testFakeCreate500 = withProvider $ \fake p -> do
  failNextCalls fake 1 500
  r <- pCreateInvoice p (SPMCard CPStripe) fiftyFourDollars
  r `shouldSatisfy` namesInError "500"
  fakeSessionIds fake `shouldReturn` []

testFakeRead500 :: IO ()
testFakeRead500 = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = sid} <- createdInvoice p (SPMCard CPStripe)
  setSessionState fake sid ["status" .= ("complete" :: Text), "payment_status" .= ("paid" :: Text)]
  failNextCalls fake 1 500
  r <- pReadInvoice p sid
  r `shouldSatisfy` namesInError "500"
  pReadInvoice p sid `shouldReturn` Right (Just (SigSettled fiftyFourDollarsReceived fixtureChargeAt))

testRefusesCrypto :: IO ()
testRefusesCrypto = withProvider $ \_ p ->
  pCreateInvoice p (SPMCrypto CCBtc) fiftyFourDollars >>= (`shouldSatisfy` isLeft)

-- | A status this build has never seen is an error, not 'Right Nothing': the latter would claim
-- the session had not changed, and the poller would leave the order to expire.
testFakeReadUnknownStatus :: IO ()
testFakeReadUnknownStatus = withProvider $ \fake p -> do
  ProviderInvoice {piProviderRef = sid} <- createdInvoice p (SPMCard CPStripe)
  setSessionState fake sid ["status" .= ("frozen" :: Text)]
  pReadInvoice p sid >>= (`shouldSatisfy` namesInError "unknown status")

-- | Stripe can report a session paid before its charge is expanded. With no charge to date it,
-- the settlement instant falls back to the read time rather than an epoch.
testSettledNoChargeReadTime :: IO ()
testSettledNoChargeReadTime = do
  now <- getCurrentTime
  let sr = SessionRead {srId = "cs_test_x", srStatus = "complete", srPaymentStatus = "paid", srAmountTotal = 5400, srChargeCreated = Nothing}
  signalOf now sr `shouldBe` Right (Just (SigSettled fiftyFourDollarsReceived now))

-- | The poll must list by creation time, not status=open: a status=open query returns only open
-- sessions, which carry no signal, so a settlement the webhook missed would never be caught. The
-- window reaches back the settle window plus a session's own lifetime.
testFakeListWindow :: IO ()
testFakeListWindow = withProvider $ \fake p -> do
  askedAt <- getCurrentTime
  _ <- pListOpen p
  answeredAt <- getCurrentTime
  gets <- apiRequests fake "GET" []
  case gets of
    (listed : _) -> do
      lookup "status" (frQuery listed) `shouldBe` Nothing
      case join (lookup "created[gte]" (frQuery listed)) >>= readMaybe . B8.unpack of
        Nothing -> expectationFailure ("no readable created[gte] in " <> show (frQuery listed))
        Just sent -> do
          let window = truncate settleWindow + 60 * toInteger fakeSessionMinutes
              seconds t = floor (utcTimeToPOSIXSeconds t) :: Integer
          sent `shouldSatisfy` \s -> s >= seconds askedAt - window && s <= seconds answeredAt - window
    [] -> expectationFailure "expected at least one list request"

-- | A list carries no expanded charge, so a settled row dates its settlement at read time, not an
-- epoch. The open row yields no signal and stays out of lpMoved.
testFakeListsOpen :: IO ()
testFakeListsOpen = withProvider $ \_ p -> do
  askedAt <- getCurrentTime
  r <- pListOpen p
  answeredAt <- getCurrentTime
  case r of
    Right ListPass {lpMoved} -> do
      map fst lpMoved `shouldSatisfy` elem "cs_test_a"
      map fst lpMoved `shouldSatisfy` elem "cs_test_b"
      map fst lpMoved `shouldSatisfy` notElem "cs_test_open"
      case lookup "cs_test_a" lpMoved of
        Just (SigSettled rcv settledAt) -> do
          rcv `shouldBe` fiftyFourDollarsReceived
          settledAt `shouldSatisfy` \t -> t >= askedAt && t <= answeredAt
        other -> failWith ("cs_test_a should settle at read time, got " <> show other)
      lookup "cs_test_b" lpMoved `shouldBe` Just (SigClosed nothingReceived)
    Left e -> failWith ("expected a list pass, got " <> show e)

testFakeListSkipsIdless :: IO ()
testFakeListSkipsIdless = withProvider $ \_ p ->
  pListOpen p >>= \case
    Right ListPass {lpSkipped} -> map fst lpSkipped `shouldBe` [Nothing]
    Left e -> failWith ("expected a list pass, got " <> show e)

testFakeListPages :: IO ()
testFakeListPages = withProvider $ \fake p -> do
  useListPageSize fake 1
  pListOpen p >>= \case
    Right ListPass {lpMoved} -> do
      map fst lpMoved `shouldSatisfy` elem "cs_test_a"
      map fst lpMoved `shouldSatisfy` elem "cs_test_b"
    Left e -> failWith ("expected a list pass, got " <> show e)

-- | Stripe pages by the last row's id. A page whose last row has no id leaves no cursor, so the
-- rest cannot be walked; that must surface as an anomaly, not a clean pass that silently drops
-- the sessions beyond it.
testFakeListCursorGap :: IO ()
testFakeListCursorGap = withProvider $ \fake p -> do
  useListFixture fake "session-list-cursor-gap"
  useListPageSize fake 2
  pListOpen p >>= \case
    Right ListPass {lpMoved, lpSkipped} -> do
      map fst lpMoved `shouldSatisfy` elem "cs_test_a"
      map fst lpMoved `shouldSatisfy` notElem "cs_test_c"
      map snd lpSkipped `shouldSatisfy` any ("no id to page from" `T.isInfixOf`)
    Left e -> failWith ("expected a list pass, got " <> show e)

testFakeList429 :: IO ()
testFakeList429 = withProvider $ \fake p -> do
  failNextCalls fake 1 429
  pListOpen p >>= (`shouldSatisfy` namesInError "429")

testWebhookVerifies :: IO ()
testWebhookVerifies = withProvider $ \fake p -> do
  let secret = sWebhookSecret (fsConfig fake)
      body = stripeEvent "checkout.session.completed" "cs_test_a"
  pVerifyWebhook p (stripeSigHeader secret 1700000000 body) (LB.toStrict body) `shouldBe` Right (Just "cs_test_a")
  pVerifyWebhook p (stripeSigHeader (secret <> "0") 1700000000 body) (LB.toStrict body) `shouldSatisfy` isRefused

testWebhookUnhandled :: IO ()
testWebhookUnhandled = withProvider $ \fake p -> do
  let secret = sWebhookSecret (fsConfig fake)
      body = stripeEvent "charge.refunded" "cs_test_a"
  pVerifyWebhook p (stripeSigHeader secret 1700000000 body) (LB.toStrict body) `shouldBe` Right Nothing

-- | Stripe sends one v1 per active secret while a signing secret is being rotated, so a valid
-- signature can be any of them, not only the first. Only the second here is correct.
testWebhookRotatedSignature :: IO ()
testWebhookRotatedSignature = withProvider $ \fake p -> do
  let secret = sWebhookSecret (fsConfig fake)
      t = 1700000000 :: Int
      body = stripeEvent "checkout.session.completed" "cs_test_a"
      good = stripeHexSig secret t body
      wrong = stripeHexSig (secret <> "0") t body
      header sigs = [("Stripe-Signature", "t=" <> B8.pack (show t) <> B8.concat [",v1=" <> s | s <- sigs])]
  pVerifyWebhook p (header [wrong, good]) (LB.toStrict body) `shouldBe` Right (Just "cs_test_a")
  pVerifyWebhook p (header [wrong, wrong]) (LB.toStrict body) `shouldSatisfy` isRefused

testWebhookMalformed :: IO ()
testWebhookMalformed = withProvider $ \fake p -> do
  let secret = sWebhookSecret (fsConfig fake)
      body = stripeEvent "checkout.session.completed" "cs_test_a"
      raw = LB.toStrict body
  pVerifyWebhook p [] raw `shouldSatisfy` isRefused
  pVerifyWebhook p [("Stripe-Signature", "t=1700000000")] raw `shouldSatisfy` isRefused
  pVerifyWebhook p [("Stripe-Signature", "t=1700000000,v1=not hex")] raw `shouldSatisfy` isRefused
  pVerifyWebhook p (stripeSigHeader secret 1700000000 body) (raw <> "x") `shouldSatisfy` isRefused

namesInError :: Text -> Either ProviderError a -> Bool
namesInError what = \case
  Left (ProviderError e) -> what `T.isInfixOf` e
  Right _ -> False

isRefused :: Either WebhookError (Maybe Text) -> Bool
isRefused = \case
  Left (WebhookError _) -> True
  Right _ -> False
