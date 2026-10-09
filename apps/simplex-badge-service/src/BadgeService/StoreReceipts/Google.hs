{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module BadgeService.StoreReceipts.Google (playStoreVerifier) where

import BadgeService.Config (PlayStoreConfig (..))
import BadgeService.StoreReceipts (StoreRefusal (..), VerifiedStoreTransaction (..))
import Control.Concurrent.STM
import Control.Exception (IOException, try)
import Crypto.Hash.Algorithms (SHA256 (..))
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import qualified Data.Aeson as J
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as LB
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.X509 (PrivKey (..))
import Data.X509.Memory (readKeyFileFromMemory)
import Network.HTTP.Client
  ( HttpException (..),
    Manager,
    Request (..),
    RequestBody (..),
    Response (..),
    brReadSome,
    parseRequest,
    withResponse,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (..), hAuthorization, hContentType, methodPost, renderSimpleQuery)
import Simplex.Chat.PaymentService (googlePurchaseRef)
import Simplex.Messaging.Util (tshow)

data ServiceAccount = ServiceAccount {clientEmail :: Text, privateKey :: RSA.PrivateKey}

data PlayEnv = PlayEnv
  { config :: PlayStoreConfig,
    account :: ServiceAccount,
    manager :: Manager,
    accessToken :: TVar (Maybe (ByteString, UTCTime))
  }

playStoreVerifier :: PlayStoreConfig -> IO (Either String (Text -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction), Text -> Text -> IO (Either StoreRefusal ())))
playStoreVerifier config@PlayStoreConfig {gServiceAccountFile} =
  readServiceAccount gServiceAccountFile >>= \case
    Left e -> pure $ Left e
    Right account -> do
      manager <- newTlsManager
      accessToken <- newTVarIO Nothing
      let env = PlayEnv {config, account, manager, accessToken}
      pure $ Right (verifyPurchase env, acknowledgePurchase env)

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
verifyPurchase env productId token
  | T.all (== '.') token = pure $ Left $ SRUnreachable "a Play token this service will not send: only dots, which a URL path resolves away"
  | otherwise = (>>= purchaseVerdict productId token) <$> readPurchase env productId token

-- | Acknowledged, never consumed: consuming drops the purchase from the app's purchase query,
-- which is how the app finds a purchase it has not yet settled.
acknowledgePurchase :: PlayEnv -> Text -> Text -> IO (Either StoreRefusal ())
acknowledgePurchase env productId token =
  readPurchase env productId token >>= \case
    Left refusal -> pure $ Left refusal
    Right ProductPurchase {alreadyAcknowledged = True} -> pure $ Right ()
    Right _ -> bimap acknowledging (const ()) <$> playCall env (\req -> req {method = methodPost}) (purchaseUrl env productId token <> ":acknowledge")
  where
    acknowledging = \case
      SRUnreachable reason -> SRUnreachable $ "acknowledging: " <> reason
      SRVerifierFailed reason -> SRVerifierFailed $ "acknowledging: " <> reason
      refusal -> refusal

readPurchase :: PlayEnv -> Text -> Text -> IO (Either StoreRefusal ProductPurchase)
readPurchase env productId token = (>>= decoded) <$> playCall env id (purchaseUrl env productId token)
  where
    decoded = either (const $ Left $ SRVerifierFailed "could not read Play's purchase record") Right . J.eitherDecode'

purchaseUrl :: PlayEnv -> Text -> Text -> Text
purchaseUrl PlayEnv {config = PlayStoreConfig {gApiHost, gPackageName}} productId token =
  T.intercalate "/" [gApiHost, "androidpublisher/v3/applications", gPackageName, "purchases/products", productId, "tokens", token]

playCall :: PlayEnv -> (Request -> Request) -> Text -> IO (Either StoreRefusal LB.ByteString)
playCall env@PlayEnv {accessToken} prepare url =
  bearerToken env >>= \case
    Left refusal -> pure $ Left refusal
    Right bearer ->
      playRequest env (\req -> (prepare req) {requestHeaders = [(hAuthorization, "Bearer " <> bearer)]}) url >>= \case
        Left refusal -> pure $ Left refusal
        Right (code, body) | code >= 200 && code < 300 -> pure $ Right body
        Right (401, _) -> atomically (writeTVar accessToken Nothing) $> Left (SRVerifierFailed "Play refused this service's access token")
        Right (403, _) -> pure $ Left $ SRVerifierFailed "the service account lacks the Play Console permission for this call"
        -- a 404 among them: Play answers it for a token it issued but has not yet recorded
        Right (code, _) -> pure $ Left $ SRUnreachable $ "Play answered HTTP " <> tshow code

purchaseVerdict :: Text -> Text -> ProductPurchase -> Either StoreRefusal VerifiedStoreTransaction
purchaseVerdict productId token ProductPurchase {purchaseState, purchaseType, purchaseQuantity, purchaseProductId}
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
          paid = Nothing
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
playRequest PlayEnv {manager} prepare url =
  try (parseRequest (T.unpack url) >>= \req -> withResponse (prepare req) manager readBody) >>= \case
    -- http-client's exceptions show the request, whose URL can hold the purchase token, so only the kind is kept
    Left (e :: HttpException) -> pure $ Left $ SRUnreachable $ "request to Google failed: " <> failureKind e
    Right (code, body)
      | LB.length body > maxPlayBytes -> pure $ Left $ SRVerifierFailed $ "Google answered HTTP " <> tshow code <> " with over " <> tshow maxPlayBytes <> " bytes"
      | otherwise -> pure $ Right (code, body)
  where
    readBody resp = (statusCode (responseStatus resp),) <$> brReadSome (responseBody resp) (fromIntegral maxPlayBytes + 1)
    failureKind = \case
      HttpExceptionRequest _ content -> T.pack $ takeWhile (/= ' ') $ show content
      InvalidUrlException _ _ -> "InvalidUrlException"

maxPlayBytes :: Int64
maxPlayBytes = 1024 * 1024
