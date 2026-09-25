{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeService.FakeStore
  ( FakeStore (..),
    newFakeStore,
    settlePending,
    setGoogleDown,
    googleSupporterToken,
    googlePendingToken,
    googleUnreachableToken,
    googleThrowingToken,
    googleHangingToken,
    googleSubscriptionToken,
    googlePayment,
    unsignedJWS,
  )
where

import BadgeService.StoreReceipts
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.Aeson as J
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Simplex.Chat.PaymentService (ServicePayment (..))
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
    pendingSettled :: IORef Bool,
    googleDown :: IORef Bool,
    fakeVerifier :: StoreVerifier
  }

newFakeStore :: IO FakeStore
newFakeStore = do
  appleSupporterJWS <- fixtureJWS "transaction-supporter.json"
  appleLegendJWS <- fixtureJWS "transaction-legend.json"
  appleSandboxJWS <- fixtureJWS "transaction-sandbox.json"
  let appleThrowingJWS = unsignedJWS "{\"transactionId\":\"2000000812345679\",\"productId\":\"BADGE_SUPPORTER_01\"}"
  pendingSettled <- newIORef False
  googleDown <- newIORef False
  let appleReceipts =
        [ (appleSupporterJWS, appleTransaction "BADGE_SUPPORTER_01" SEProduction 700),
          (appleLegendJWS, appleTransaction "BADGE_LEGEND_01" SEProduction 7000),
          (appleSandboxJWS, appleTransaction "BADGE_LEGEND_01" SETest 7000)
        ]
      verifyApple jws
        | jws == appleThrowingJWS = error "fake verifier bug"
        | otherwise = maybe (Left "not a fake receipt") Right $ lookup jws appleReceipts
      verifyGoogle = googleVerdict pendingSettled googleDown
  pure
    FakeStore
      { appleSupporterJWS,
        appleLegendJWS,
        appleSandboxJWS,
        appleThrowingJWS,
        pendingSettled,
        googleDown,
        fakeVerifier = StoreVerifier {verifyApple = Just verifyApple, verifyGoogle = Just verifyGoogle}
      }
  where
    fixtureJWS name = unsignedJWS <$> B.readFile (fixtureDir </> name)
    appleTransaction productId environment cents =
      StoreTransaction {productId, quantity = 1, environment, paid = Just (CurrencyAmount cents, "USD")}

googleVerdict :: IORef Bool -> IORef Bool -> Text -> Text -> IO (Either StoreRefusal StoreTransaction)
googleVerdict pendingSettled googleDown productId token =
  readIORef googleDown >>= \case
    True -> pure $ Left $ SRUnreachable "fake store is down"
    False
      | (productId, token) == ("badge_supporter_01", googleSupporterToken) -> pure $ Right purchased
      | (productId, token) == ("subscr_badge_supporter_01", googleSubscriptionToken) -> pure $ Right purchased
      | (productId, token) == ("badge_supporter_01", googlePendingToken) -> do
          settled <- readIORef pendingSettled
          pure $ if settled then Right purchased else Left SRPending
      | token == googleUnreachableToken -> pure $ Left $ SRUnreachable "fake store is down"
      | token == googleThrowingToken -> ioError $ userError "fake connection reset"
      | token == googleHangingToken -> forever $ threadDelay 1000000
      | otherwise -> pure $ Left $ SRInvalid "not a fake purchase"
  where
    purchased = StoreTransaction {productId, quantity = 1, environment = SEProduction, paid = Nothing}

settlePending :: FakeStore -> IO ()
settlePending FakeStore {pendingSettled} = writeIORef pendingSettled True

setGoogleDown :: FakeStore -> Bool -> IO ()
setGoogleDown FakeStore {googleDown} = writeIORef googleDown

googleSupporterToken, googlePendingToken, googleUnreachableToken, googleThrowingToken, googleHangingToken, googleSubscriptionToken :: Text
googleSupporterToken = "fake-play-token-supporter.AO-J1Oz9x2kqE7wYt3"
googlePendingToken = "fake-play-token-pending.AO-J1Oy8w1jpD6vXs2"
googleUnreachableToken = "fake-play-token-unreachable.AO-J1Ox7v0ioC5uWr1"
googleThrowingToken = "fake-play-token-throwing.AO-J1Ov5t8gmA3sUp9"
googleHangingToken = "fake-play-token-hanging.AO-J1Ou4s7flZ2rTo8"
googleSubscriptionToken = "fake-play-token-subscription.AO-J1Ow6u9hnB4tVq0"

googlePayment :: Text -> Text -> ServicePayment
googlePayment productId token = SPGoogle {productId, token}

unsignedJWS :: B.ByteString -> Text
unsignedJWS payload = safeDecodeUtf8 $ B.intercalate "." $ map B64U.encodeUnpadded [header, payload, "not a signature"]
  where
    header = LB.toStrict $ J.encode $ J.object ["alg" J..= ("ES256" :: Text), "x5c" J..= (["leaf", "intermediate", "root"] :: [Text])]
