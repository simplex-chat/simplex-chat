{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.BadgeService.FakeStore
  ( FakeStore (..),
    newFakeStore,
    googleVerifierWithHook,
    subscriptionCheckWithHook,
    settlePending,
    setGoogleDown,
    googleSupporterToken,
    googlePendingToken,
    googleUnreachableToken,
    googleThrowingToken,
    googleHangingToken,
    googleSubscriptionToken,
    googleUnknownProductToken,
    appleSubscriptionOriginalId,
    appleSecondSubscriptionOriginalId,
    setSubscription,
    subscriptionChecks,
    googlePayment,
    subscriptionJWS,
    unsignedJWS,
  )
where

import BadgeService.StoreReceipts
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Aeson as J
import Data.Aeson.Types (Pair)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Int (Int64)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Simplex.Chat.PaymentService (ServicePayment (..), appleTransactionRef, googlePurchaseRef)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Simplex.Messaging.Util (safeDecodeUtf8)
import System.FilePath ((</>))

fixtureDir :: FilePath
fixtureDir = "apps" </> "simplex-badge-service" </> "test-fixtures" </> "apple"

-- | The Apple JWS are the fixtures under no real signature, so they are accepted as these exact
-- strings only, never because a payload decodes - which a real verifier must refuse to do.
data FakeStore = FakeStore
  { appleSupporterJWS :: Text,
    appleLegendJWS :: Text,
    appleSandboxJWS :: Text,
    appleThrowingJWS :: Text,
    appleMisnamedJWS :: Text,
    appleQuantityJWS :: Text,
    -- two transactions of the subscription appleSubscriptionOriginalId names, the renewal bought later, one
    -- from Apple's Sandbox, and a later one Apple revoked, which the fake does not vouch for
    appleSubscriptionJWS :: Text,
    appleRenewalSubscriptionJWS :: Text,
    appleSandboxSubscriptionJWS :: Text,
    appleRevokedSubscriptionJWS :: Text,
    -- a transaction of the same subscription after it was changed to annual Legend
    appleLegendSubscriptionJWS :: Text,
    -- two other subscriptions, the first under appleSecondSubscriptionOriginalId and the second one the store does not know
    appleSecondSubscriptionJWS :: Text,
    appleThirdSubscriptionJWS :: Text,
    pendingSettled :: IORef Bool,
    googleDown :: IORef Bool,
    -- what the store answers about each subscription: Apple's by original id, Google's by token
    subscriptions :: IORef (M.Map (Either Text Text) SubscriptionCheck),
    subscriptionCheckCount :: IORef Int,
    fakeVerifier :: StoreVerifier
  }

newFakeStore :: IO FakeStore
newFakeStore = do
  appleSupporterJWS <- fixtureJWS "transaction-supporter.json"
  appleLegendJWS <- fixtureJWS "transaction-legend.json"
  appleSandboxJWS <- fixtureJWS "transaction-sandbox.json"
  let appleThrowingJWS = unsignedJWS "{\"transactionId\":\"2000000812345679\",\"productId\":\"BADGE_SUPPORTER_01\"}"
      appleMisnamedJWS = unsignedJWS "{\"transactionId\":\"2000000812345698\",\"productId\":\"BADGE_SUPPORTER_01\"}"
      appleQuantityJWS = unsignedJWS "{\"transactionId\":\"2000000812345697\",\"productId\":\"BADGE_SUPPORTER_01\"}"
      appleSubscriptionJWS = subscriptionJWS appleSubscriptionOriginalId "2000000812345681" [purchasedAt 1767225600000]
      appleRenewalSubscriptionJWS = subscriptionJWS appleSubscriptionOriginalId "2000000812345683" [purchasedAt 1769904000000]
      appleSandboxSubscriptionJWS = subscriptionJWS appleSubscriptionOriginalId "2000000812345682" [purchasedAt 1767225600000]
      appleRevokedSubscriptionJWS = subscriptionJWS appleSubscriptionOriginalId "2000000812345684" [purchasedAt 1769904000000, "revocationDate" J..= (1770000000000 :: Int64)]
      appleLegendSubscriptionJWS = subscriptionJWS appleSubscriptionOriginalId "2000000812345688" [purchasedAt 1769904000000, "productId" J..= ("SUBSCR_BADGE_LEGEND_YEAR_01" :: Text)]
      appleSecondSubscriptionJWS = subscriptionJWS appleSecondSubscriptionOriginalId "2000000812345701" [purchasedAt 1767225600000]
      appleThirdSubscriptionJWS = subscriptionJWS "2000000812345710" "2000000812345711" [purchasedAt 1767225600000]
  pendingSettled <- newIORef False
  googleDown <- newIORef False
  subscriptions <- newIORef M.empty
  subscriptionCheckCount <- newIORef 0
  let appleReceipts =
        [ (appleSupporterJWS, appleTransaction "2000000812345671" "BADGE_SUPPORTER_01" False 700),
          (appleLegendJWS, appleTransaction "2000000812345672" "BADGE_LEGEND_01" False 7000),
          (appleSandboxJWS, appleTransaction "2000000812345673" "BADGE_LEGEND_01" True 7000),
          -- a verifier vouching for another transaction than the one the evidence names
          (appleMisnamedJWS, appleTransaction "2000000812345671" "BADGE_SUPPORTER_01" False 700),
          (appleQuantityJWS, (appleTransaction "2000000812345697" "BADGE_SUPPORTER_01" False 1400) {quantity = 2}),
          -- vouched by the ref the claim reads, the subscription's original id
          (appleSubscriptionJWS, appleSubscription appleSubscriptionJWS False),
          (appleRenewalSubscriptionJWS, appleSubscription appleRenewalSubscriptionJWS False),
          (appleSandboxSubscriptionJWS, appleSubscription appleSandboxSubscriptionJWS True),
          (appleLegendSubscriptionJWS, appleTransaction appleSubscriptionOriginalId "SUBSCR_BADGE_LEGEND_YEAR_01" False 7000),
          (appleSecondSubscriptionJWS, appleSubscription appleSecondSubscriptionJWS False),
          (appleThirdSubscriptionJWS, appleSubscription appleThirdSubscriptionJWS False)
        ]
      verifyApple jws
        | jws == appleThrowingJWS = error "fake verifier bug"
        | otherwise = maybe (Left $ SRInvalid "not a fake receipt") Right $ lookup jws appleReceipts
      verifyGoogle = googleVerdict pendingSettled googleDown
      checkSubscription ref _ = do
        modifyIORef' subscriptionCheckCount (+ 1)
        down <- readIORef googleDown
        case ref of
          SubscriptionGoogle _ | down -> pure $ Left $ SRUnreachable "fake store is down"
          _ -> maybe (Left $ SRUnreachable "the fake store knows no such subscription") Right . M.lookup (subscriptionKey ref) <$> readIORef subscriptions
  pure
    FakeStore
      { appleSupporterJWS,
        appleLegendJWS,
        appleSandboxJWS,
        appleThrowingJWS,
        appleMisnamedJWS,
        appleQuantityJWS,
        appleSubscriptionJWS,
        appleRenewalSubscriptionJWS,
        appleSandboxSubscriptionJWS,
        appleRevokedSubscriptionJWS,
        appleLegendSubscriptionJWS,
        appleSecondSubscriptionJWS,
        appleThirdSubscriptionJWS,
        pendingSettled,
        googleDown,
        subscriptions,
        subscriptionCheckCount,
        fakeVerifier = StoreVerifier {verifyApple = Just verifyApple, verifyGoogle = Just verifyGoogle, acknowledgeGoogle = Nothing, acknowledgeGoogleSubscription = Nothing, checkSubscription = Just checkSubscription, verifyTimeout = 500000, storeChecks = ChecksAfterCoverage, creditTestPurchases = False}
      }
  where
    fixtureJWS name = unsignedJWS <$> B.readFile (fixtureDir </> name)
    appleTransaction transactionRef productId testPurchase cents =
      VerifiedStoreTransaction {transactionRef, productId, quantity = 1, testPurchase, paid = Just (CurrencyAmount cents, "USD"), alreadyAcknowledged = False}
    purchasedAt ms = "purchaseDate" J..= (ms :: Int64)
    appleSubscription jws testPurchase =
      VerifiedStoreTransaction {transactionRef = fromMaybe "" (appleTransactionRef jws), productId = "SUBSCR_BADGE_SUPPORTER_MONTH_01", quantity = 1, testPurchase, paid = Just (CurrencyAmount 700, "USD"), alreadyAcknowledged = False}

-- | A transaction of the subscription the original id names, with the payload fields given.
subscriptionJWS :: Text -> Text -> [Pair] -> Text
subscriptionJWS originalId transactionId fields =
  unsignedJWS $
    LB.toStrict $
      J.encode $
        J.object $
          [ "transactionId" J..= transactionId,
            "originalTransactionId" J..= originalId,
            "type" J..= ("Auto-Renewable Subscription" :: Text),
            "productId" J..= ("SUBSCR_BADGE_SUPPORTER_MONTH_01" :: Text)
          ]
            <> fields

subscriptionKey :: SubscriptionRef -> Either Text Text
subscriptionKey = \case
  SubscriptionApple originalId -> Left originalId
  SubscriptionGoogle token -> Right token

setSubscription :: FakeStore -> SubscriptionRef -> SubscriptionCheck -> IO ()
setSubscription FakeStore {subscriptions} ref checked = modifyIORef' subscriptions $ M.insert (subscriptionKey ref) checked

subscriptionChecks :: FakeStore -> IO Int
subscriptionChecks FakeStore {subscriptionCheckCount} = readIORef subscriptionCheckCount

googleVerdict :: IORef Bool -> IORef Bool -> Text -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction)
googleVerdict pendingSettled googleDown productId token =
  readIORef googleDown >>= \case
    True -> pure $ Left $ SRUnreachable "fake store is down"
    False
      | (productId, token) == ("badge_supporter_01", googleSupporterToken) -> pure $ Right purchased
      | (productId, token) == ("badge_unknown_01", googleUnknownProductToken) -> pure $ Right purchased
      | (productId, token) == ("badge_supporter_01", googlePendingToken) -> do
          settled <- readIORef pendingSettled
          pure $ if settled then Right purchased else Left SRPending
      | token == googleUnreachableToken -> pure $ Left $ SRUnreachable "fake store is down"
      | token == googleThrowingToken -> ioError $ userError "fake connection reset"
      | token == googleHangingToken -> forever $ threadDelay 1000000
      | otherwise -> pure $ Left $ SRInvalid "not a fake purchase"
  where
    purchased = VerifiedStoreTransaction {transactionRef = googlePurchaseRef token, productId, quantity = 1, testPurchase = False, paid = Nothing, alreadyAcknowledged = False}

-- | The fake store, running the hook's action once, inside the first Google verification, before it answers.
googleVerifierWithHook :: IORef (IO ()) -> FakeStore -> StoreVerifier
googleVerifierWithHook hook FakeStore {fakeVerifier = v@StoreVerifier {verifyGoogle}} =
  v {verifyGoogle = hooked <$> verifyGoogle, verifyTimeout = 10000000}
  where
    hooked verify productId token = do
      action <- atomicModifyIORef' hook (pure (),)
      action
      verify productId token

-- | The fake store, running the hook's action once, inside the first subscription check, before it answers.
subscriptionCheckWithHook :: IORef (IO ()) -> FakeStore -> StoreVerifier
subscriptionCheckWithHook hook FakeStore {fakeVerifier = v@StoreVerifier {checkSubscription}} =
  v {checkSubscription = hooked <$> checkSubscription, verifyTimeout = 10000000}
  where
    hooked check ref product_ = do
      action <- atomicModifyIORef' hook (pure (),)
      action
      check ref product_

settlePending :: FakeStore -> IO ()
settlePending FakeStore {pendingSettled} = writeIORef pendingSettled True

setGoogleDown :: FakeStore -> Bool -> IO ()
setGoogleDown FakeStore {googleDown} = writeIORef googleDown

googleSupporterToken, googlePendingToken, googleUnreachableToken, googleThrowingToken, googleHangingToken, googleSubscriptionToken, googleUnknownProductToken :: Text
googleSupporterToken = "fake-play-token-supporter.AO-J1Oz9x2kqE7wYt3"
googlePendingToken = "fake-play-token-pending.AO-J1Oy8w1jpD6vXs2"
googleUnreachableToken = "fake-play-token-unreachable.AO-J1Ox7v0ioC5uWr1"
googleThrowingToken = "fake-play-token-throwing.AO-J1Ov5t8gmA3sUp9"
googleHangingToken = "fake-play-token-hanging.AO-J1Ou4s7flZ2rTo8"
googleSubscriptionToken = "fake-play-token-subscription.AO-J1Ow6u9hnB4tVq0"
googleUnknownProductToken = "fake-play-token-unknown-product.AO-J1Ot3r6ekY1qSn7"

appleSubscriptionOriginalId, appleSecondSubscriptionOriginalId :: Text
appleSubscriptionOriginalId = "2000000812345680"
appleSecondSubscriptionOriginalId = "2000000812345700"

googlePayment :: Text -> Text -> ServicePayment
googlePayment productId token = SPGoogle {productId, token}

unsignedJWS :: B.ByteString -> Text
unsignedJWS payload = safeDecodeUtf8 $ B.intercalate "." $ map B64U.encodeUnpadded [header, payload, "not a signature"]
  where
    header = LB.toStrict $ J.encode $ J.object ["alg" J..= ("ES256" :: Text), "x5c" J..= (["leaf", "intermediate", "root"] :: [Text])]
