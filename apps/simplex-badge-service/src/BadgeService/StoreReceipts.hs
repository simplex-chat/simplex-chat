{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module BadgeService.StoreReceipts
  ( VerifiedStoreTransaction (..),
    StoreRefusal (..),
    StoreVerifier (..),
    StoreChecks (..),
    StoreReceipt (..),
    SubscriptionRef (..),
    SubscriptionCheck (..),
    StoreCharge (..),
    StoreVerifierFailure (..),
    noStoreVerifier,
    toStoreReceipt,
    checkStoreSubscription,
    storeRequest,
  )
where

import BadgeService.Log (logInfo, logWarn)
import Control.Exception (Exception, catch, evaluate, try)
import qualified Data.ByteString.Lazy as LB
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isControl, isDigit, isPrint, isSpace)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Network.HTTP.Client (HttpException (..), Manager, Request, Response (..), brReadSome, parseRequest, withResponse)
import Network.HTTP.Types (Status (..))
import Simplex.Chat.Badges.Service (SubscriptionState)
import Simplex.Chat.PaymentService (ServicePayment (..), appleTransactionRef, googlePurchaseRef)
import Simplex.Chat.PaymentService.Types (CurrencyAmount, PaymentProvider (..), StoreTransactionRef (..))
import Simplex.Messaging.Util (catchOwn', tshow)
import System.Timeout (timeout)

-- | What a store vouches for about one completed transaction.
-- TODO [badges] PaymentFunding's PFApple and PFGoogle sketch the same thing and have no producer;
-- delete them, or build them out and reconcile.
data VerifiedStoreTransaction = VerifiedStoreTransaction
  { transactionRef :: Text, -- from what was verified: applePayloadRef of Apple's payload, googlePurchaseRef of the token asked about
    productId :: Text,
    quantity :: Int, -- as the store reported it; the service refuses any but 1, as the apps never buy more
    -- costs the buyer nothing: Apple's Sandbox, which a public TestFlight build buys in, and Google's license testers
    testPurchase :: Bool,
    paid :: Maybe (CurrencyAmount, Text), -- in minor units; Google's purchase record carries no price
    alreadyAcknowledged :: Bool -- Play's record says acknowledged or consumed; other stores have nothing to acknowledge
  }
  deriving (Eq, Show)

-- | The reasons are for the service's log alone and must never quote the receipt.
-- SRInvalid and SRTestPurchase are terminal: the client drops the purchase's keys and nothing
-- presents it again. So only a verdict no later attempt could change is either; when in doubt, SRUnreachable.
data StoreRefusal
  = SRInvalid Text -- a verdict that cannot change, and the client consumes the purchase: forged, malformed, refunded
  | SRTestPurchase -- the store vouched for it, but it cost nothing; terminal, and named so the app can say why
  | SRPending -- a real purchase the store has not settled; it may yet
  | SRUnreachable Text -- no verdict: the store was not asked, did not answer, or does not know the token (a Play 404 may be lag)
  | SRVerifierFailed Text -- this service's own failure, its bug or misconfiguration, never the store's answer
  | SRNotConfigured -- no verifier for this store is deployed; the purchase may be real
  deriving (Eq, Show)

-- | What a refusal was, for a log. The reasons carry no receipt, token or key: a store request's
-- exceptions are reduced to their kind, and an unsendable token to its shape.
refusalLabel :: StoreRefusal -> Text
refusalLabel = \case
  SRInvalid reason -> "invalid: " <> reason
  SRTestPurchase -> "a test purchase"
  SRPending -> "not settled yet"
  SRUnreachable reason -> "no answer: " <> reason
  SRVerifierFailed reason -> "this service failed: " <> reason
  SRNotConfigured -> "no verifier configured"

-- | Apple's receipt is verified offline, from the certificate chain it ships; a Play token is opaque
-- and must be asked about, and a subscription's charges are known only to its store, so those fields are in IO.
data StoreVerifier = StoreVerifier
  { -- the JWS
    verifyApple :: Maybe (Text -> Either StoreRefusal VerifiedStoreTransaction),
    verifyGoogle :: Maybe (Text -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction)), -- the product id and the token
    acknowledgeGoogle :: Maybe (Text -> Text -> IO (Either StoreRefusal ())),
    acknowledgeGoogleSubscription :: Maybe (Text -> Text -> IO (Either StoreRefusal ())),
    -- with the product the presented receipt names, absent on a renewal
    checkSubscription :: Maybe (SubscriptionRef -> Maybe Text -> IO (Either StoreRefusal SubscriptionCheck)),
    -- microseconds; a lane answers one request at a time, so a verifier that does not finish holds up the rest of its lane
    verifyTimeout :: Int,
    storeChecks :: StoreChecks,
    -- Local testing only: credits a purchase the store itself calls a test, which no real buyer pays for.
    creditTestPurchases :: Bool
  }

-- | When a subscription's store is asked about it. A real store's next charge has not started while coverage
-- runs, and asking spends the quota every subscriber shares, so only a dev deployment checks at every request.
data StoreChecks = ChecksAfterCoverage | ChecksAtEveryRequest
  deriving (Eq, Show)

-- | A store payment, named by the store's own reference before anything is verified.
data StoreReceipt = StoreReceipt
  { txRef :: StoreTransactionRef,
    receiptProductId :: Maybe Text, -- named by a Play receipt; Apple's is known only once its JWS is verified
    subscriptionRef :: SubscriptionRef,
    verifyReceipt :: IO (Either StoreRefusal VerifiedStoreTransaction),
    acknowledgeReceipt :: Maybe (VerifiedStoreTransaction -> IO (Either StoreRefusal ())),
    acknowledgeSubscription :: Maybe (IO (Either StoreRefusal ()))
  }

-- | What a store is asked about a subscription by: Apple's original transaction id, or Play's token.
-- The token is a bearer secret, so this has no Show.
data SubscriptionRef = SubscriptionApple Text | SubscriptionGoogle Text

data SubscriptionCheck = SubscriptionCheck
  { subscriptionState :: SubscriptionState,
    testSubscription :: Bool, -- costs the buyer nothing, as a testPurchase does
    latestCharge :: Maybe StoreCharge -- the latest paid period, if the store charged at all
  }
  deriving (Show)

data StoreCharge = StoreCharge
  { chargeRef :: Text, -- Apple's transactionId, a hash of Play's order id
    planId :: Text, -- Apple's product id, Play's base plan
    periodStart :: UTCTime,
    periodEnd :: UTCTime,
    chargePaid :: Maybe (CurrencyAmount, Text), -- in minor units; Play reports no price
    revoked :: Bool, -- refunded or revoked
    -- Apple: any offerType, so an introductory offer (free trial, pay as you go, pay up front), a promotional offer,
    -- an offer code or a win-back offer; Play: a free-trial or introductory-price phase
    offer :: Bool
  }
  deriving (Show)

noStoreVerifier :: StoreVerifier
noStoreVerifier = StoreVerifier {verifyApple = Nothing, verifyGoogle = Nothing, acknowledgeGoogle = Nothing, acknowledgeGoogleSubscription = Nothing, checkSubscription = Nothing, verifyTimeout = 10000000, storeChecks = ChecksAfterCoverage, creditTestPurchases = False}

-- | Nothing for a payment no store made. Exceptions are not logged, since they can quote the receipt
-- or, from Google, a URL holding the token.
toStoreReceipt :: StoreVerifier -> ServicePayment -> Maybe (Either StoreRefusal StoreReceipt)
toStoreReceipt StoreVerifier {verifyApple, verifyGoogle, acknowledgeGoogle, acknowledgeGoogleSubscription, verifyTimeout} = \case
  SPApple {jws} -> Just $ case appleTransactionRef jws of
    Nothing -> Left $ SRInvalid "names no transaction"
    Just ref ->
      Right
        StoreReceipt
          { txRef = StoreTransactionRef PPApple ref,
            receiptProductId = Nothing,
            subscriptionRef = SubscriptionApple ref,
            verifyReceipt = maybe unconfigured (\verify -> offline $ verify jws) verifyApple,
            acknowledgeReceipt = Nothing,
            acknowledgeSubscription = Nothing
          }
  SPGoogle {productId, token}
    -- the claim is the token's hash, so neither string may name any purchase but the one it claims,
    -- whatever path a verifier builds from them
    | not (googleProductId productId) -> Just $ Left $ SRInvalid "not a Play product id"
    -- Play documents no token grammar, so this is our guess, and refusing to ask Play is not its verdict
    | not (googleToken token) -> Just $ Left $ SRUnreachable $ "a Play token this service will not send: " <> tokenShape token
    | otherwise ->
        Just $
          Right
            StoreReceipt
              { txRef = StoreTransactionRef PPGoogle (googlePurchaseRef token),
                receiptProductId = Just productId,
                subscriptionRef = SubscriptionGoogle token,
                verifyReceipt = maybe unconfigured (\verify -> online "google verifier" $ verify productId token) verifyGoogle,
                acknowledgeReceipt = (\acknowledge VerifiedStoreTransaction {alreadyAcknowledged} -> if alreadyAcknowledged then pure (Right ()) else online "acknowledging: google" $ acknowledge productId token) <$> acknowledgeGoogle,
                acknowledgeSubscription = (\acknowledge -> online "acknowledging: google" $ acknowledge productId token) <$> acknowledgeGoogleSubscription
              }
  SPInvoice {} -> Nothing
  SPReceipt {} -> Nothing
  where
    unconfigured = pure $ Left SRNotConfigured
    -- nothing was fetched, so a throw or an overrun is this service's failure or a shape it does not know, never an outage
    offline verdict = do
      r <-
        (fromMaybe (Left $ SRVerifierFailed "apple verifier timed out") <$> timeout verifyTimeout (forced verdict))
          `catchOwn'` \_ -> pure $ Left $ SRVerifierFailed "apple verifier threw"
      logInfo $ "apple receipt: " <> either refusalLabel (const "vouched") r
      pure r
    online = askStore verifyTimeout

-- | Both stores are asked online about a subscription, so a throw or an overrun is no verdict. A test
-- purchase is refused here, for a first purchase and a renewal alike.
checkStoreSubscription :: StoreVerifier -> SubscriptionRef -> Maybe Text -> IO (Either StoreRefusal SubscriptionCheck)
checkStoreSubscription StoreVerifier {checkSubscription, verifyTimeout, creditTestPurchases} ref product_ = case checkSubscription of
  Nothing -> pure $ Left SRNotConfigured
  Just check -> askStore verifyTimeout "subscription check" (check ref product_) >>= either (pure . Left) notTest
  where
    notTest checked@SubscriptionCheck {testSubscription}
      | not testSubscription = pure $ Right checked
      | creditTestPurchases = logWarn "[dev] a store test subscription is taken as paid: credit_test_purchases is on" $> Right checked
      | otherwise = pure $ Left SRTestPurchase

-- | Thrown by a verifier that cannot place what the store signed: its own failure, never the store's answer or silence.
newtype StoreVerifierFailure = StoreVerifierFailure Text
  deriving (Show)

instance Exception StoreVerifierFailure

askStore :: Int -> Text -> IO (Either StoreRefusal a) -> IO (Either StoreRefusal a)
askStore limit what call = do
  r <-
    (fromMaybe (Left $ SRUnreachable $ what <> " timed out") <$> timeout limit ((call >>= forced) `catch` verifierFailed))
      `catchOwn'` \_ -> pure $ Left $ SRUnreachable $ what <> " threw"
  -- that the store was asked and what came of it, since not every path answers with the refusal
  logInfo $ what <> ": " <> either refusalLabel (const "vouched") r
  pure r
  where
    verifierFailed (StoreVerifierFailure reason) = pure $ Left $ SRVerifierFailed reason

-- | A request to a store: its status and a body of at most a mebibyte. http-client's exceptions show
-- the request, whose URL can hold a purchase token, so only their kind is kept.
storeRequest :: Manager -> Text -> (Request -> Request) -> Text -> IO (Either StoreRefusal (Int, LB.ByteString))
storeRequest manager store prepare url =
  try (parseRequest (T.unpack url) >>= \req -> withResponse (prepare req) manager readBody) >>= \case
    Left (e :: HttpException) -> pure $ Left $ SRUnreachable $ "request to " <> store <> " failed: " <> failureKind e
    Right (code, body)
      | LB.length body > maxStoreBytes -> pure $ Left $ SRVerifierFailed $ store <> " answered HTTP " <> tshow code <> " with over " <> tshow maxStoreBytes <> " bytes"
      | otherwise -> pure $ Right (code, body)
  where
    readBody resp = (statusCode (responseStatus resp),) <$> brReadSome (responseBody resp) (fromIntegral maxStoreBytes + 1)
    failureKind = \case
      HttpExceptionRequest _ content -> T.pack $ takeWhile (/= ' ') $ show content
      InvalidUrlException _ _ -> "InvalidUrlException"

maxStoreBytes :: Int64
maxStoreBytes = 1024 * 1024

-- | A verdict holding a thunk that throws would otherwise throw later, outside the handlers.
forced :: Either StoreRefusal a -> IO (Either StoreRefusal a)
forced = either (fmap Left . evaluate) (fmap Right . evaluate)

googleProductId :: Text -> Bool
googleProductId pid = case T.uncons pid of
  Just (c, _) -> T.length pid <= 150 && (isAsciiLower c || isDigit c) && T.all (\x -> isAsciiLower x || isDigit x || x == '_' || x == '.') pid
  Nothing -> False

-- | Not empty or only dots, which a URL path resolves away.
googleToken :: Text -> Bool
googleToken t = not (T.all (== '.') t) && T.length t <= 4096 && T.all (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || x == '.' || x == '_' || x == '-') t

-- | What the log may say about a token: its length and the kinds of character in it, never any of them.
tokenShape :: Text -> Text
tokenShape t = tshow (T.length t) <> " characters: " <> T.intercalate ", " [kind | (kind, isKind) <- kinds, T.any isKind t]
  where
    kinds =
      [ ("lowercase", isAsciiLower),
        ("uppercase", isAsciiUpper),
        ("digits", isDigit),
        ("'.'", (== '.')),
        ("'_'", (== '_')),
        ("'-'", (== '-')),
        ("other printable ASCII", \c -> isAscii c && isPrint c && not (isAlphaNum c) && c `notElem` ("._-" :: String)),
        ("whitespace or control", \c -> isSpace c || isControl c),
        ("non-ASCII", not . isAscii)
      ]
