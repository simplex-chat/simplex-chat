{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeService.StoreReceipts.Mock (newMockStoreVerifier) where

import BadgeService.Catalog (playMonthlyPlan)
import BadgeService.StoreReceipts
import Control.Applicative ((<|>))
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (guard)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JM
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime)
import Simplex.Chat.Badges.Ledger (addMonths)
import Simplex.Chat.Badges.Service (SubscriptionState (..))
import Simplex.Chat.Badges.StorePlan (playBasePlan, storePlan)
import Simplex.Chat.PaymentService (appleTransactionRef, googlePurchaseRef)
import Simplex.Chat.PaymentService.Types (PaymentProvider (..))
import Simplex.Messaging.Util (eitherToMaybe, tshow)

-- | Vouches for any well-formed receipt without verifying anything. Each transaction is named by
-- the same function its claim is named by, so the two cannot differ. A subscription is charged for
-- the plan its first purchase or a tier raise names, and that plan is kept in memory to charge each renewal again.
newMockStoreVerifier :: IO UTCTime -> IO StoreVerifier
newMockStoreVerifier clock = do
  plans <- newTVarIO M.empty
  pure noStoreVerifier {verifyApple = Just mockApple, verifyGoogle = Just mockGoogle, checkSubscription = Just $ mockSubscription clock plans}

-- | Keyed by Apple's original transaction id or by Play's token, which only this process holds.
mockSubscription :: IO UTCTime -> TVar (M.Map (Either Text Text) Text) -> SubscriptionRef -> Maybe Text -> IO (Either StoreRefusal SubscriptionCheck)
mockSubscription clock plans ref = \case
  Just productId -> case firstPlan productId of
    Nothing -> pure $ Left $ SRUnreachable "the mock knows no plan for this subscription"
    Just planId -> do
      atomically $ modifyTVar' plans $ M.insert key planId
      renewing planId
  Nothing ->
    M.lookup key <$> readTVarIO plans >>= \case
      Nothing -> pure $ Right SubscriptionCheck {subscriptionState = SSEnded, testSubscription = False, latestCharge = Nothing}
      Just planId -> renewing planId
  where
    (provider, key) = case ref of
      SubscriptionApple originalId -> (PPApple, Left originalId)
      SubscriptionGoogle token -> (PPGoogle, Right token)
    firstPlan productId = case ref of
      SubscriptionApple _ -> productId <$ storePlan PPApple productId
      SubscriptionGoogle token -> testTokenPlan productId token <|> playMonthlyPlan productId
    -- a test token names its base plan, test.<basePlanId>.<uuid>, as the apps' test products sign it
    testTokenPlan productId token = do
      (basePlanId, rest) <- T.breakOn "." <$> T.stripPrefix "test." token
      guard $ T.length rest > 1
      (planProductId, _) <- playBasePlan basePlanId
      basePlanId <$ guard (planProductId == productId)
    renewing planId = do
      now <- clock
      let periodStart = addUTCTime (-60) now
          months = maybe 0 snd $ storePlan provider planId
          charge = StoreCharge {chargeRef = "mock-" <> tshow periodStart, planId, periodStart, periodEnd = addMonths (fromIntegral months) periodStart, chargePaid = Nothing, revoked = False, offer = False}
      pure $ Right SubscriptionCheck {subscriptionState = SSRenewing, testSubscription = False, latestCharge = Just charge}

mockApple :: Text -> Either StoreRefusal VerifiedStoreTransaction
mockApple signed = case T.splitOn "." signed of
  [_, payload, _] -> do
    o <- case J.decodeStrict' =<< eitherToMaybe (B64U.decodeUnpadded $ encodeUtf8 payload) of
      Just (J.Object obj) -> Right obj
      _ -> Left $ SRInvalid "payload is not base64url JSON"
    productId <- case JM.lookup "productId" o of
      Just (J.String p) -> Right p
      _ -> Left $ SRInvalid "no productId"
    transactionRef <- maybe (Left $ SRInvalid "no transactionId") Right $ appleTransactionRef signed
    Right $ vouched transactionRef productId
  _ -> Left $ SRInvalid "not three dot-separated parts"

mockGoogle :: Text -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction)
mockGoogle productId token = pure $ Right $ vouched (googlePurchaseRef token) productId

vouched :: Text -> Text -> VerifiedStoreTransaction
vouched transactionRef productId =
  -- not a test purchase, though nothing was paid: a test purchase is refused as test_purchase, which is
  -- terminal and drops the client's keys, so every dev purchase would fail for good
  VerifiedStoreTransaction {transactionRef, productId, quantity = 1, testPurchase = False, paid = Nothing, alreadyAcknowledged = False}
