{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module BadgeService.StoreReceipts.Google (PlayVerifier (..), playStoreVerifier) where

import BadgeService.Config (PlayStoreConfig (..))
import BadgeService.StoreReceipts (StoreCharge (..), StoreRefusal (..), SubscriptionCheck (..), VerifiedStoreTransaction (..), storeRequest)
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Control.Monad (forM)
import Crypto.Hash.Algorithms (SHA256 (..))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as LB
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.X509 (PrivKey (..))
import Data.X509.Memory (readKeyFileFromMemory)
import Network.HTTP.Client (Manager, Request (..), RequestBody (..))
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hAuthorization, hContentType, methodPost, renderSimpleQuery)
import Simplex.Chat.Badges.Ledger (addMonths)
import Simplex.Chat.Badges.Service (SubscriptionState (..))
import Simplex.Chat.Badges.StorePlan (playBasePlan)
import Simplex.Chat.PaymentService (googlePurchaseRef)
import Simplex.Messaging.Util (tshow)

data ServiceAccount = ServiceAccount {clientEmail :: Text, privateKey :: RSA.PrivateKey}

data PlayEnv = PlayEnv
  { config :: PlayStoreConfig,
    account :: ServiceAccount,
    manager :: Manager,
    accessToken :: TVar (Maybe (ByteString, UTCTime))
  }

-- | Each call takes the product id and the token, but the subscription check: subscriptionsv2 reads by token alone.
data PlayVerifier = PlayVerifier
  { verifyPlayPurchase :: Text -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction),
    acknowledgePlayPurchase :: Text -> Text -> IO (Either StoreRefusal ()),
    checkPlaySubscription :: Text -> IO (Either StoreRefusal SubscriptionCheck),
    acknowledgePlaySubscription :: Text -> Text -> IO (Either StoreRefusal ())
  }

playStoreVerifier :: PlayStoreConfig -> IO (Either String PlayVerifier)
playStoreVerifier config@PlayStoreConfig {gServiceAccountFile} =
  readServiceAccount gServiceAccountFile >>= \case
    Left e -> pure $ Left e
    Right account -> do
      manager <- newTlsManager
      accessToken <- newTVarIO Nothing
      let env = PlayEnv {config, account, manager, accessToken}
      pure $
        Right
          PlayVerifier
            { verifyPlayPurchase = verifyPurchase env,
              acknowledgePlayPurchase = acknowledgePurchase env,
              checkPlaySubscription = checkSubscription env,
              acknowledgePlaySubscription = acknowledgeSubscription env
            }

readServiceAccount :: FilePath -> IO (Either String ServiceAccount)
readServiceAccount path =
  try (B.readFile path) >>= \case
    Left (_ :: IOException) -> pure $ Left $ path <> ": could not be read"
    Right s -> pure $ case J.decodeStrict' s of
      Just (ServiceAccountFile clientEmail pem) -> case readKeyFileFromMemory (encodeUtf8 pem) of
        [PrivKeyRSA privateKey] -> Right ServiceAccount {clientEmail, privateKey}
        _ -> Left $ path <> ": private_key is not one RSA key"
      Nothing -> Left $ path <> ": is not a service account key with client_email and private_key"

data ServiceAccountFile = ServiceAccountFile Text Text

instance J.FromJSON ServiceAccountFile where
  parseJSON = J.withObject "service account key" $ \o -> ServiceAccountFile <$> o J..: "client_email" <*> o J..: "private_key"

verifyPurchase :: PlayEnv -> Text -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction)
verifyPurchase env productId token = (>>= purchaseVerdict productId token) <$> readPurchase env productId token

-- | Acknowledged, never consumed: consuming drops the purchase from the app's purchase query,
-- which is how the app finds a purchase it has not yet settled.
acknowledgePurchase :: PlayEnv -> Text -> Text -> IO (Either StoreRefusal ())
acknowledgePurchase env productId token =
  bimap acknowledging (const ()) <$> playCall env (\req -> req {method = methodPost}) (purchaseUrl env productId token <> ":acknowledge")

-- | As a one-time purchase is: once, before it is credited, so Play's three-day refund never hits a credited one.
acknowledgeSubscription :: PlayEnv -> Text -> Text -> IO (Either StoreRefusal ())
acknowledgeSubscription env productId token =
  readSubscription env token >>= \case
    Left refusal -> pure $ Left $ acknowledging refusal
    Right SubscriptionPurchase {subscriptionAcknowledged = True} -> pure $ Right ()
    Right _ -> bimap acknowledging (const ()) <$> playCall env (\req -> req {method = methodPost}) (playUrl env ["purchases/subscriptions", productId, "tokens", token <> ":acknowledge"])

acknowledging :: StoreRefusal -> StoreRefusal
acknowledging = \case
  SRUnreachable reason -> SRUnreachable $ "acknowledging: " <> reason
  SRVerifierFailed reason -> SRVerifierFailed $ "acknowledging: " <> reason
  refusal -> refusal

readPurchase :: PlayEnv -> Text -> Text -> IO (Either StoreRefusal ProductPurchase)
readPurchase env productId token = (>>= decoded) <$> playCall env id (purchaseUrl env productId token)
  where
    decoded = either (const $ Left $ SRVerifierFailed "could not read Play's purchase record") Right . J.eitherDecode'

purchaseUrl :: PlayEnv -> Text -> Text -> Text
purchaseUrl env productId token = playUrl env ["purchases/products", productId, "tokens", token]

playUrl :: PlayEnv -> [Text] -> Text
playUrl PlayEnv {config = PlayStoreConfig {gApiHost, gPackageName}} path =
  T.intercalate "/" $ [gApiHost, "androidpublisher/v3/applications", gPackageName] <> path

checkSubscription :: PlayEnv -> Text -> IO (Either StoreRefusal SubscriptionCheck)
checkSubscription env token =
  playResponse env id (subscriptionUrl env token) >>= \case
    -- Play answers 410 for a subscription that expired too long ago to be read
    Right (410, _) -> pure $ Right SubscriptionCheck {subscriptionState = SSEnded, testSubscription = False, latestCharge = Nothing}
    response -> (>>= subscriptionVerdict) . (>>= decodedSubscription) <$> either (pure . Left) (playAnswer env) response

readSubscription :: PlayEnv -> Text -> IO (Either StoreRefusal SubscriptionPurchase)
readSubscription env token = (>>= decodedSubscription) <$> playCall env id (subscriptionUrl env token)

subscriptionUrl :: PlayEnv -> Text -> Text
subscriptionUrl env token = playUrl env ["purchases/subscriptionsv2/tokens", token]

decodedSubscription :: LB.ByteString -> Either StoreRefusal SubscriptionPurchase
decodedSubscription = either (const $ Left $ SRVerifierFailed "could not read Play's subscription record") Right . J.eitherDecode'

-- | Play reports no current period start, so it is the period end less the plan's length; a plan this
-- service does not sell has no length, and its charge, which is never granted, is given no period.
-- The service grants a Play charge by its base plan alone, so a base plan it sells under another product is refused.
-- Revoking an order shows only as the subscription ending, so a Play charge is never revoked.
subscriptionVerdict :: SubscriptionPurchase -> Either StoreRefusal SubscriptionCheck
subscriptionVerdict SubscriptionPurchase {subscriptionStateName, testSubscription, lineItems}
  | subscriptionStateName == "SUBSCRIPTION_STATE_PENDING" = Left SRPending
  | otherwise = do
      subscriptionState <- case subscriptionStateName of
        "SUBSCRIPTION_STATE_ACTIVE" -> renewalState <$> onlyLineItem
        "SUBSCRIPTION_STATE_PAUSED" -> renewalState <$> onlyLineItem
        "SUBSCRIPTION_STATE_IN_GRACE_PERIOD" -> Right SSBillingRetry
        "SUBSCRIPTION_STATE_ON_HOLD" -> Right SSBillingRetry
        "SUBSCRIPTION_STATE_CANCELED" -> Right SSCancelled
        "SUBSCRIPTION_STATE_EXPIRED" -> Right SSEnded
        "SUBSCRIPTION_STATE_PENDING_PURCHASE_CANCELED" -> Right SSEnded
        _ -> Left $ SRVerifierFailed "a subscriptionState this service does not know"
      latestCharge <- case lineItems of
        [] | subscriptionState == SSEnded -> Right Nothing
        _ -> onlyLineItem >>= lineItemCharge
      pure SubscriptionCheck {subscriptionState, testSubscription, latestCharge}
  where
    onlyLineItem = case lineItems of
      [item] -> Right item
      _ -> Left $ SRVerifierFailed "Play answered a subscription without exactly one line item"
    renewalState LineItem {autoRenewEnabled} = if autoRenewEnabled then SSRenewing else SSCancelled
    lineItemCharge LineItem {expiryTime, itemProductId, basePlanId, latestSuccessfulOrderId, inOffer} =
      forM latestSuccessfulOrderId $ \orderId -> case (expiryTime, itemProductId, basePlanId) of
        (Just periodEnd, Just productId, Just planId) -> case playBasePlan planId of
          Just (planProductId, _) | planProductId /= productId -> Left $ SRVerifierFailed "Play answered a base plan of another product"
          plan_ ->
            let periodStart = maybe periodEnd (\(_, (_, months)) -> addMonths (negate $ toInteger months) periodEnd) plan_
             -- hashed so the row does not show the order id; unkeyed, so a holder of the buyer's receipt can still find it
             in Right StoreCharge {chargeRef = googlePurchaseRef orderId, planId, periodStart, periodEnd, chargePaid = Nothing, revoked = False, offer = inOffer}
        _ -> Left $ SRVerifierFailed "Play answered a charge without its expiryTime, product or base plan"

data SubscriptionPurchase = SubscriptionPurchase
  { subscriptionStateName :: Text,
    subscriptionAcknowledged :: Bool,
    testSubscription :: Bool,
    lineItems :: [LineItem]
  }

instance J.FromJSON SubscriptionPurchase where
  parseJSON = J.withObject "SubscriptionPurchaseV2" $ \o -> do
    subscriptionStateName <- o J..: "subscriptionState"
    subscriptionAcknowledged <- (== Just ("ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED" :: Text)) <$> o J..:? "acknowledgementState"
    testPurchase :: Maybe J.Value <- o J..:? "testPurchase"
    let testSubscription = isJust testPurchase
    lineItems <- o J..:? "lineItems" J..!= []
    pure SubscriptionPurchase {subscriptionStateName, subscriptionAcknowledged, testSubscription, lineItems}

data LineItem = LineItem
  { expiryTime :: Maybe UTCTime,
    autoRenewEnabled :: Bool,
    itemProductId :: Maybe Text,
    basePlanId :: Maybe Text,
    latestSuccessfulOrderId :: Maybe Text,
    -- from the current phase: offerDetails names the offer it was bought with for its whole life
    inOffer :: Bool
  }

instance J.FromJSON LineItem where
  parseJSON = J.withObject "SubscriptionPurchaseLineItem" $ \o -> do
    expiryTime <- o J..:? "expiryTime"
    autoRenewEnabled <- maybe (pure False) (\p -> p J..:? "autoRenewEnabled" J..!= False) =<< o J..:? "autoRenewingPlan"
    itemProductId <- o J..:? "productId"
    basePlanId <- maybe (pure Nothing) (J..:? "basePlanId") =<< o J..:? "offerDetails"
    latestSuccessfulOrderId <- o J..:? "latestSuccessfulOrderId"
    phase :: Maybe J.Object <- o J..:? "offerPhase"
    let inOffer = maybe False (\p -> KM.member "freeTrial" p || KM.member "introductoryPrice" p) phase
    pure LineItem {expiryTime, autoRenewEnabled, itemProductId, basePlanId, latestSuccessfulOrderId, inOffer}

playCall :: PlayEnv -> (Request -> Request) -> Text -> IO (Either StoreRefusal LB.ByteString)
playCall env prepare url = playResponse env prepare url >>= either (pure . Left) (playAnswer env)

playResponse :: PlayEnv -> (Request -> Request) -> Text -> IO (Either StoreRefusal (Int, LB.ByteString))
playResponse env prepare url =
  bearerToken env >>= \case
    Left refusal -> pure $ Left refusal
    Right bearer -> playRequest env (\req -> (prepare req) {requestHeaders = [(hAuthorization, "Bearer " <> bearer)]}) url

playAnswer :: PlayEnv -> (Int, LB.ByteString) -> IO (Either StoreRefusal LB.ByteString)
playAnswer PlayEnv {accessToken} = \case
  (code, body) | code >= 200 && code < 300 -> pure $ Right body
  (401, _) -> atomically (writeTVar accessToken Nothing) $> Left (SRVerifierFailed "Play refused this service's access token")
  (403, _) -> pure $ Left $ SRVerifierFailed "the service account lacks the Play Console permission for this call"
  -- a 404 among them, which Play may answer for a token it issued but has not yet recorded
  (code, _) -> pure $ Left $ SRUnreachable $ "Play answered HTTP " <> tshow code

purchaseVerdict :: Text -> Text -> ProductPurchase -> Either StoreRefusal VerifiedStoreTransaction
purchaseVerdict productId token ProductPurchase {purchaseState, purchaseType, purchaseQuantity, purchaseProductId, alreadyAcknowledged}
  | maybe False (/= productId) purchaseProductId = Left $ SRVerifierFailed "Play answered about another product"
  | otherwise = case purchaseState of
      0 -> purchased <$> testPurchaseOf purchaseType
      -- a canceled purchase stays canceled, and buying again issues a new token
      1 -> Left $ SRInvalid "canceled"
      2 -> Left SRPending
      s -> Left $ SRVerifierFailed $ "an unknown purchaseState " <> tshow s
  where
    purchased testPurchase =
      VerifiedStoreTransaction
        { transactionRef = googlePurchaseRef token,
          productId,
          quantity = fromMaybe 1 purchaseQuantity,
          testPurchase,
          paid = Nothing,
          alreadyAcknowledged
        }
    testPurchaseOf = \case
      Nothing -> Right False
      Just 0 -> Right True
      -- a promo code, which only this app's developer can issue
      Just 1 -> Right False
      Just t -> Left $ SRVerifierFailed $ "a purchaseType this service does not credit: " <> tshow t

data ProductPurchase = ProductPurchase
  { purchaseState :: Int,
    purchaseType :: Maybe Int,
    purchaseQuantity :: Maybe Int,
    purchaseProductId :: Maybe Text,
    alreadyAcknowledged :: Bool
  }

instance J.FromJSON ProductPurchase where
  parseJSON = J.withObject "ProductPurchase" $ \o -> do
    purchaseState <- o J..: "purchaseState"
    purchaseType <- o J..:? "purchaseType"
    purchaseQuantity <- o J..:? "quantity"
    purchaseProductId <- o J..:? "productId"
    acknowledgementState <- o J..:? "acknowledgementState"
    consumptionState <- o J..:? "consumptionState"
    let alreadyAcknowledged = acknowledgementState == Just (1 :: Int) || consumptionState == Just (1 :: Int)
    pure ProductPurchase {purchaseState, purchaseType, purchaseQuantity, purchaseProductId, alreadyAcknowledged}

bearerToken :: PlayEnv -> IO (Either StoreRefusal ByteString)
bearerToken env@PlayEnv {accessToken} = do
  now <- getCurrentTime
  readTVarIO accessToken >>= \case
    Just (bearer, validUntil) | now < validUntil -> pure $ Right bearer
    _ -> requestAccessToken env now

requestAccessToken :: PlayEnv -> UTCTime -> IO (Either StoreRefusal ByteString)
requestAccessToken env@PlayEnv {config = PlayStoreConfig {gTokenUrl}, account, accessToken} now =
  signedAssertion account gTokenUrl now >>= \case
    Nothing -> pure $ Left $ SRVerifierFailed "could not sign the service account assertion"
    Just assertion ->
      playRequest env (tokenForm assertion) gTokenUrl >>= \case
        Left refusal -> pure $ Left refusal
        Right (200, body) -> case J.eitherDecode' body of
          Right (GrantedToken bearer expiresIn) -> do
            atomically $ writeTVar accessToken $ Just (bearer, addUTCTime (fromIntegral expiresIn - refreshMargin) now)
            pure $ Right bearer
          Left _ -> pure $ Left $ SRVerifierFailed "could not read Google's access token"
        Right (code, _)
          | code >= 500 || code == 429 -> pure $ Left $ SRUnreachable $ "Google's token endpoint answered HTTP " <> tshow code
          | otherwise -> pure $ Left $ SRVerifierFailed $ "Google refused the service account assertion: HTTP " <> tshow code
  where
    tokenForm assertion req =
      req
        { method = methodPost,
          requestHeaders = [(hContentType, "application/x-www-form-urlencoded")],
          requestBody = RequestBodyBS $ renderSimpleQuery False [("grant_type", "urn:ietf:params:oauth:grant-type:jwt-bearer"), ("assertion", assertion)]
        }

refreshMargin :: NominalDiffTime
refreshMargin = 300

data GrantedToken = GrantedToken ByteString Int

instance J.FromJSON GrantedToken where
  parseJSON = J.withObject "access token" $ \o -> GrantedToken . encodeUtf8 <$> o J..: "access_token" <*> o J..: "expires_in"

signedAssertion :: ServiceAccount -> Text -> UTCTime -> IO (Maybe ByteString)
signedAssertion ServiceAccount {clientEmail, privateKey} audience now =
  either (const Nothing) (\sig -> Just $ signingInput <> "." <> B64U.encodeUnpadded sig) <$> RSA.signSafer (Just SHA256) privateKey signingInput
  where
    issuedAt = floor (utcTimeToPOSIXSeconds now) :: Int64
    signingInput = segment (J.object ["alg" J..= ("RS256" :: Text), "typ" J..= ("JWT" :: Text)]) <> "." <> segment claims
    claims =
      J.object
        [ "iss" J..= clientEmail,
          "scope" J..= ("https://www.googleapis.com/auth/androidpublisher" :: Text),
          "aud" J..= audience,
          "iat" J..= issuedAt,
          "exp" J..= (issuedAt + 3600)
        ]
    segment = B64U.encodeUnpadded . LB.toStrict . J.encode

playRequest :: PlayEnv -> (Request -> Request) -> Text -> IO (Either StoreRefusal (Int, LB.ByteString))
playRequest PlayEnv {manager} = storeRequest manager "Google"
