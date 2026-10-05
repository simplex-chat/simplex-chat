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

playStoreVerifier :: PlayStoreConfig -> IO (Either String (Text -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction)))
playStoreVerifier config@PlayStoreConfig {gServiceAccountFile} =
  readServiceAccount gServiceAccountFile >>= \case
    Left e -> pure $ Left e
    Right account -> do
      manager <- newTlsManager
      accessToken <- newTVarIO Nothing
      pure $ Right $ verifyPurchase PlayEnv {config, account, manager, accessToken}

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
verifyPurchase env@PlayEnv {config = PlayStoreConfig {gApiHost, gPackageName}, accessToken} productId token
  | T.all (== '.') token = pure $ Left $ SRUnreachable "a Play token this service will not send: only dots, which a URL path resolves away"
  | otherwise =
      bearerToken env >>= \case
        Left refusal -> pure $ Left refusal
        Right bearer ->
          playRequest env (\req -> req {requestHeaders = [(hAuthorization, "Bearer " <> bearer)]}) purchaseUrl >>= \case
            Left refusal -> pure $ Left refusal
            Right (200, body) -> pure $ purchaseVerdict productId token body
            Right (401, _) -> atomically (writeTVar accessToken Nothing) $> Left (SRVerifierFailed "Play refused this service's access token")
            Right (403, _) -> pure $ Left $ SRVerifierFailed "the service account may not read this app's purchases"
            -- a 404 among them: Play answers it for a token it issued but has not yet recorded
            Right (code, _) -> pure $ Left $ SRUnreachable $ "Play answered HTTP " <> tshow code
  where
    purchaseUrl = T.intercalate "/" [gApiHost, "androidpublisher/v3/applications", gPackageName, "purchases/products", productId, "tokens", token]

purchaseVerdict :: Text -> Text -> LB.ByteString -> Either StoreRefusal VerifiedStoreTransaction
purchaseVerdict productId token body = case J.eitherDecode' body of
  Left _ -> Left $ SRVerifierFailed "could not read Play's purchase record"
  Right ProductPurchase {purchaseState, purchaseType, purchaseQuantity, purchaseProductId}
    | maybe False (/= productId) purchaseProductId -> Left $ SRVerifierFailed "Play answered about another product"
    | otherwise -> case purchaseState of
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
  where
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
    purchaseProductId :: Maybe Text
  }

instance J.FromJSON ProductPurchase where
  parseJSON = J.withObject "ProductPurchase" $ \o ->
    ProductPurchase <$> o J..: "purchaseState" <*> o J..:? "purchaseType" <*> o J..:? "quantity" <*> o J..:? "productId"

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
