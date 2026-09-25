{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeService.FakeStore
  ( FakeStore (..),
    newFakeStore,
    settlePending,
    googleSupporterToken,
    googlePendingToken,
    googleUnreachableToken,
    googleSubscriptionToken,
    googlePayment,
    unsignedJWS,
  )
where

import BadgeService.StoreReceipts
import qualified Data.Aeson as J
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.PaymentService (ServicePayment (..))
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Util (safeDecodeUtf8)
import System.FilePath ((</>))

fixtureDir :: FilePath
fixtureDir = "apps" </> "simplex-badge-service" </> "test-fixtures" </> "apple"

-- | Stores that vouch for a fixed set of receipts. The Apple JWS are the fixtures wrapped in a JWS
-- envelope with no real signature, and are accepted as these exact strings only, never because a
-- payload decodes - which is what a real verifier must refuse to do.
data FakeStore = FakeStore
  { appleSupporterJWS :: Text,
    appleLegendJWS :: Text,
    pendingSettled :: IORef Bool,
    fakeVerifier :: StoreVerifier
  }

newFakeStore :: IO FakeStore
newFakeStore = do
  appleSupporterJWS <- unsignedJWS <$> B.readFile (fixtureDir </> "transaction-supporter.json")
  appleLegendJWS <- unsignedJWS <$> B.readFile (fixtureDir </> "transaction-legend.json")
  pendingSettled <- newIORef False
  let appleReceipts =
        [ (appleSupporterJWS, appleTransaction "2000000812345671" "BADGE_SUPPORTER_01" 700),
          (appleLegendJWS, appleTransaction "2000000812345672" "BADGE_LEGEND_01" 7000)
        ]
      verifyApple jws = pure $ maybe (Left $ SRInvalid "not a fake receipt") Right $ lookup jws appleReceipts
      verifyGoogle productId token = googleVerdict pendingSettled productId token
  pure FakeStore {appleSupporterJWS, appleLegendJWS, pendingSettled, fakeVerifier = StoreVerifier {verifyApple, verifyGoogle}}
  where
    appleTransaction providerRef productId cents =
      StoreTransaction {providerRef, productId, quantity = 1, paid = Just (CurrencyAmount cents, "USD")}

googleVerdict :: IORef Bool -> Text -> Text -> IO (Either StoreRefusal StoreTransaction)
googleVerdict pendingSettled productId token
  | (productId, token) == ("badge_supporter_01", googleSupporterToken) = pure $ Right purchased
  | (productId, token) == ("subscr_badge_supporter_01", googleSubscriptionToken) = pure $ Right purchased
  | (productId, token) == ("badge_supporter_01", googlePendingToken) = do
      settled <- readIORef pendingSettled
      pure $ if settled then Right purchased else Left SRPending
  | token == googleUnreachableToken = pure $ Left $ SRUnreachable "fake store is down"
  | otherwise = pure $ Left $ SRInvalid "not a fake purchase"
  where
    purchased = StoreTransaction {providerRef = tokenRef, productId, quantity = 1, paid = Nothing}
    tokenRef = safeDecodeUtf8 $ strEncode $ C.sha256Hash $ encodeUtf8 token

settlePending :: FakeStore -> IO ()
settlePending FakeStore {pendingSettled} = writeIORef pendingSettled True

googleSupporterToken, googlePendingToken, googleUnreachableToken, googleSubscriptionToken :: Text
googleSupporterToken = "fake-play-token-supporter.AO-J1Oz9x2kqE7wYt3"
googlePendingToken = "fake-play-token-pending.AO-J1Oy8w1jpD6vXs2"
googleUnreachableToken = "fake-play-token-unreachable.AO-J1Ox7v0ioC5uWr1"
googleSubscriptionToken = "fake-play-token-subscription.AO-J1Ow6u9hnB4tVq0"

googlePayment :: Text -> Text -> ServicePayment
googlePayment productId token = SPGoogle {productId, token}

unsignedJWS :: B.ByteString -> Text
unsignedJWS payload = safeDecodeUtf8 $ B.intercalate "." $ map B64U.encodeUnpadded [header, payload, "not a signature"]
  where
    header = LB.toStrict $ J.encode $ J.object ["alg" J..= ("ES256" :: Text), "x5c" J..= (["leaf", "intermediate", "root"] :: [Text])]
