{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.StoreReceipts.Apple
  ( verifyAppleTransaction,
    appleSubscriptionChecker,
    readAppleRoot,
  )
where

import BadgeService.Config (AppleServerApi (..))
import BadgeService.StoreReceipts (StoreCharge (..), StoreRefusal (..), StoreVerifierFailure (..), SubscriptionCheck (..), VerifiedStoreTransaction (..), storeRequest)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (IOException, evaluate, throwIO, try)
import Control.Monad (guard, unless, when)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as JT
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as LB
import Data.Char (isDigit)
import Data.Foldable (toList)
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word32)
import Data.X509
import Data.X509.EC (unserializePoint)
import Data.X509.Memory (readKeyFileFromMemory)
import Data.X509.Validation (SignatureFailure (..), SignatureVerification (..), verifySignedSignature)
import Network.HTTP.Client (Manager, Request (..))
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hAuthorization)
import Simplex.Chat.Badges.Service (SubscriptionState (..))
import Simplex.Chat.PaymentService (applePayloadRef)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Simplex.Messaging.Util (catchOwn', eitherToMaybe, tshow)

verifyAppleTransaction :: NonEmpty SignedCertificate -> Text -> Text -> Either StoreRefusal VerifiedStoreTransaction
verifyAppleTransaction roots ourBundleId jws = do
  o <- verifiedPayload roots jws
  tx <- either (const $ failed "the payload Apple signed is not a transaction") Right $ JT.parseEither appleTransactionP o
  transaction ourBundleId tx

-- | The payload of a JWS that a chain to a configured root signed.
verifiedPayload :: NonEmpty SignedCertificate -> Text -> Either StoreRefusal J.Object
verifiedPayload roots jws = case T.splitOn "." jws of
  [header, payload, signature] -> do
    leaf <- signingChain roots =<< certificateChain header
    sig <- jwsSignature signature
    key <- leafKey leaf
    unless (ECDSA.verify SHA256 key sig (encodeUtf8 $ header <> "." <> payload)) $
      refused "the signature does not verify"
    maybe (failed "the payload Apple signed is not JSON") Right $ jsonObject payload
  _ -> refused "not three dot-separated parts"

-- | A header or chain in a shape Apple may yet adopt proves no forgery, so it fails rather than refuses.
certificateChain :: Text -> Either StoreRefusal [ByteString]
certificateChain header = do
  o <- maybe (refused "the header is not base64url JSON") Right $ jsonObject header
  case JM.lookup "alg" o of
    Just (J.String "ES256") -> Right ()
    _ -> failed "the header does not name ES256"
  case JM.lookup "x5c" o of
    Just (J.Array certs) -> traverse der $ toList certs
    _ -> failed "the header carries no x5c chain"
  where
    der = \case
      J.String c | Right bs <- B64.decode (encodeUtf8 c) -> Right bs
      _ -> refused "an x5c certificate is not base64"

-- | The root in the chain is ignored: trust comes from the configured ones alone.
signingChain :: NonEmpty SignedCertificate -> [ByteString] -> Either StoreRefusal SignedCertificate
signingChain roots = \case
  [leafDer, intermediateDer, _] -> do
    leaf <- certificate leafDer
    intermediate <- certificate intermediateDer
    -- a root may be missing or wrong as likely as the receipt forged, so this retries rather than refuses
    unless (any (`signedBy` intermediate) roots) $ failed "the intermediate is not signed by a configured root"
    case verifySignedSignature leaf (certPubKey $ getCertificate intermediate) of
      SignaturePass -> Right ()
      SignatureFailed SignatureInvalid -> refused "the leaf is not signed by the intermediate"
      SignatureFailed _ -> failed "the intermediate's key cannot check the leaf's signature algorithm"
    -- Apple issues other certificates under this root, developers' among them; only these two mark the receipt-signing chain
    unless (marked receiptSigningLeaf leaf && marked appleIntermediate intermediate) $
      failed "Apple issued this chain, but not to sign receipts"
    pure leaf
  _ -> failed "x5c is not leaf, intermediate and root"
  where
    certificate = either (const $ failed "an x5c certificate does not decode") Right . decodeSignedCertificate
    marked oid cert = case certExtensions (getCertificate cert) of
      Extensions (Just exts) -> any ((== oid) . extRawOID) exts
      Extensions Nothing -> False
    receiptSigningLeaf = [1, 2, 840, 113635, 100, 6, 11, 1]
    appleIntermediate = [1, 2, 840, 113635, 100, 6, 2, 1]

leafKey :: SignedCertificate -> Either StoreRefusal ECDSA.PublicKey
leafKey leaf = case certPubKey (getCertificate leaf) of
  PubKeyEC (PubKeyEC_Named ECC.SEC_p256r1 pt) | Just p <- unserializePoint p256 pt -> Right $ ECDSA.PublicKey p256 p
  _ -> failed "the leaf Apple issued holds no P-256 key"
  where
    p256 = ECC.getCurveByName ECC.SEC_p256r1

-- | JWS carries ES256 as r and s, 32 bytes each, not the DER a certificate uses.
jwsSignature :: Text -> Either StoreRefusal ECDSA.Signature
jwsSignature s = case B64U.decodeUnpadded (encodeUtf8 s) of
  Right bs | B.length bs == 64 -> let (r, s') = B.splitAt 32 bs in Right $ ECDSA.Signature (os2ip r) (os2ip s')
  _ -> refused "the signature is not 64 base64url bytes"

data AppleTransaction = AppleTransaction
  { transactionRef :: Text,
    productId :: Text,
    bundleId :: Text,
    environment :: Text,
    quantity :: Int,
    revoked :: Bool,
    price :: Maybe Integer,
    currency :: Maybe Text
  }

appleTransactionP :: J.Object -> JT.Parser AppleTransaction
appleTransactionP o = do
  -- the same function names the claim, so the transaction vouched for is the one claimed
  transactionRef <- maybe (fail "names no transaction") pure $ applePayloadRef o
  productId <- o J..: "productId"
  bundleId <- o J..: "bundleId"
  environment <- o J..: "environment"
  quantity <- o J..: "quantity"
  revoked <- isJust <$> (o J..:? "revocationDate" :: JT.Parser (Maybe J.Value))
  price <- o J..:? "price"
  currency <- o J..:? "currency"
  pure AppleTransaction {transactionRef, productId, bundleId, environment, quantity, revoked, price, currency}

transaction :: Text -> AppleTransaction -> Either StoreRefusal VerifiedStoreTransaction
transaction ourBundleId AppleTransaction {transactionRef, productId, bundleId, environment, quantity, revoked, price, currency}
  -- the configured bundle id may be the wrong one, and refusing for good would destroy every genuine purchase
  | bundleId /= ourBundleId = failed "another app's bundle id, or ours misconfigured"
  | revoked = refused "refunded or revoked"
  | otherwise = do
      testPurchase <- case environment of
        "Production" -> Right False
        "Sandbox" -> Right True
        _ -> failed "an environment this verifier does not know"
      Right
        VerifiedStoreTransaction
          { transactionRef,
            productId,
            quantity,
            testPurchase,
            paid = paidOf price currency,
            alreadyAcknowledged = False
          }

-- | Apple's price is in milliunits of the currency, whatever its minor unit.
paidOf :: Maybe Integer -> Maybe Text -> Maybe (CurrencyAmount, Text)
paidOf price_ currency_ = do
  milliunits <- price_
  code <- currency_
  digits <- minorUnitDigits code
  let (minor, rest) = (milliunits * 10 ^ digits) `divMod` 1000
  guard $ rest == 0 && minor >= 0 && minor <= toInteger (maxBound :: Word32)
  pure (CurrencyAmount $ fromInteger minor, code)

-- | ISO 4217 as of its 2026-09-17 list; four-digit and non-currency codes are not recorded.
minorUnitDigits :: Text -> Maybe Integer
minorUnitDigits c
  | c `elem` ["BIF", "CLP", "DJF", "GNF", "ISK", "JPY", "KMF", "KRW", "PYG", "RWF", "UGX", "VND", "VUV", "XAF", "XOF", "XPF"] = Just 0
  | c `elem` ["BHD", "IQD", "JOD", "KWD", "LYD", "OMR", "TND"] = Just 3
  | c `elem` ["CLF", "UYW", "UYI", "XAG", "XAU", "XBA", "XBB", "XBC", "XBD", "XDR", "XPD", "XPT", "XSU", "XTS", "XUA", "XXX"] = Nothing
  | T.length c == 3 && T.all (\x -> x >= 'A' && x <= 'Z') c = Just 2
  | otherwise = Nothing

jsonObject :: Text -> Maybe J.Object
jsonObject part = J.decodeStrict' =<< eitherToMaybe (B64U.decodeUnpadded $ encodeUtf8 part)

refused :: Text -> Either StoreRefusal a
refused = Left . SRInvalid

failed :: Text -> Either StoreRefusal a
failed = Left . SRVerifierFailed . ("apple verifier: " <>)

signedBy :: SignedCertificate -> SignedCertificate -> Bool
signedBy issuer cert = verifySignedSignature cert (certPubKey $ getCertificate issuer) == SignaturePass

readAppleRoot :: FilePath -> IO (Either String SignedCertificate)
readAppleRoot path =
  try (B.readFile path) >>= \case
    Left (_ :: IOException) -> pure $ Left $ path <> ": could not be read"
    Right der -> pure $ either (const $ Left $ path <> ": is not a DER certificate") checkRoot $ decodeSignedCertificate der
  where
    checkRoot root
      | signedBy root root = Right root
      | otherwise = Left $ path <> ": is not a self-signed root certificate"

data AppleApi = AppleApi
  { roots :: NonEmpty SignedCertificate,
    ourBundleId :: Text,
    serverApi :: AppleServerApi,
    privateKey :: ECDSA.PrivateKey,
    manager :: Manager,
    apiToken :: TVar (Maybe (ByteString, UTCTime))
  }

-- | Asks the App Store Server API about a subscription by its original transaction id. The key is read
-- here, at boot, as the Play service account is.
appleSubscriptionChecker :: NonEmpty SignedCertificate -> Text -> AppleServerApi -> IO (Either String (Text -> IO (Either StoreRefusal SubscriptionCheck)))
appleSubscriptionChecker roots ourBundleId serverApi@AppleServerApi {aPrivateKeyFile} =
  readApiKey aPrivateKeyFile >>= \case
    Left e -> pure $ Left e
    Right privateKey -> do
      manager <- newTlsManager
      apiToken <- newTVarIO Nothing
      pure $ Right $ subscriptionStatus AppleApi {roots, ourBundleId, serverApi, privateKey, manager, apiToken}

readApiKey :: FilePath -> IO (Either String ECDSA.PrivateKey)
readApiKey path =
  try (B.readFile path) >>= \case
    Left (_ :: IOException) -> pure $ Left $ path <> ": could not be read"
    Right pem -> pure $ case readKeyFileFromMemory pem of
      [PrivKeyEC (PrivKeyEC_Named ECC.SEC_p256r1 d)] -> Right $ ECDSA.PrivateKey (ECC.getCurveByName ECC.SEC_p256r1) d
      _ -> Left $ path <> ": is not one P-256 private key"

subscriptionStatus :: AppleApi -> Text -> IO (Either StoreRefusal SubscriptionCheck)
subscriptionStatus api@AppleApi {serverApi = AppleServerApi {aApiHost}} originalId
  | T.null originalId || not (T.all isDigit originalId) = pure $ failed "not an App Store transaction id"
  | otherwise =
      apiCall api (aApiHost <> "/inApps/v1/subscriptions/" <> originalId) >>= \case
        Left refusal -> pure $ Left refusal
        Right body -> checked $ statusVerdict api originalId body
  where
    -- a throw while checking what Apple signed is this verifier's failure, so it leaves as one
    checked verdict =
      (evaluate verdict >>= either (fmap Left . evaluate) (fmap Right . evaluate))
        `catchOwn'` \_ -> throwIO $ StoreVerifierFailure "apple verifier: threw checking a subscription status"

apiCall :: AppleApi -> Text -> IO (Either StoreRefusal LB.ByteString)
apiCall api@AppleApi {manager, apiToken} url = do
  bearer <- bearerToken api
  storeRequest manager "Apple" (\req -> req {requestHeaders = [(hAuthorization, "Bearer " <> bearer)]}) url >>= \case
    Left refusal -> pure $ Left refusal
    Right (code, body) | code >= 200 && code < 300 -> pure $ Right body
    Right (401, _) -> atomically (writeTVar apiToken Nothing) $> failed "Apple refused this service's API token"
    Right (403, _) -> pure $ failed "Apple refused this service's API key"
    -- a 404 among them, which Apple answers for a transaction id its environment does not hold
    Right (code, _) -> pure $ Left $ SRUnreachable $ "Apple answered HTTP " <> tshow code

-- | Signed here, so reused until shortly before it expires rather than signed for every call.
bearerToken :: AppleApi -> IO ByteString
bearerToken api@AppleApi {apiToken} = do
  now <- getCurrentTime
  readTVarIO apiToken >>= \case
    Just (bearer, validUntil) | now < validUntil -> pure bearer
    _ -> do
      bearer <- signedApiToken api now
      atomically $ writeTVar apiToken $ Just (bearer, addUTCTime (fromIntegral (apiTokenSeconds - 300)) now)
      pure bearer

apiTokenSeconds :: Int64
apiTokenSeconds = 1200

signedApiToken :: AppleApi -> UTCTime -> IO ByteString
signedApiToken AppleApi {privateKey, ourBundleId, serverApi = AppleServerApi {aKeyId, aIssuerId}} now = do
  ECDSA.Signature r s <- ECDSA.sign privateKey SHA256 signingInput
  pure $ signingInput <> "." <> B64U.encodeUnpadded (i2ospOf_ 32 r <> i2ospOf_ 32 s)
  where
    issuedAt = floor (utcTimeToPOSIXSeconds now) :: Int64
    signingInput = segment (J.object ["alg" J..= ("ES256" :: Text), "kid" J..= aKeyId, "typ" J..= ("JWT" :: Text)]) <> "." <> segment claims
    claims =
      J.object
        [ "iss" J..= aIssuerId,
          "iat" J..= issuedAt,
          "exp" J..= (issuedAt + apiTokenSeconds),
          "aud" J..= ("appstoreconnect-v1" :: Text),
          "bid" J..= ourBundleId
        ]
    segment = B64U.encodeUnpadded . LB.toStrict . J.encode

-- | Everything Apple signed in the answer is checked against the configured roots like a receipt, but a
-- body that fails it is this service's failure: Apple answered, and no client presented it.
statusVerdict :: AppleApi -> Text -> LB.ByteString -> Either StoreRefusal SubscriptionCheck
statusVerdict AppleApi {roots, ourBundleId} originalId body = do
  StatusResponse items <- either (const $ failed "could not read Apple's subscription status") Right $ J.eitherDecode' body
  -- an omission is no more a verdict than a 404, and refusing it would end a purchase the buyer paid for
  item <- maybe (Left $ SRUnreachable "Apple's answer names no such subscription") Right $ find ((== Just originalId) . itemOriginalId) items
  LastTransaction {status, signedTransactionInfo, signedRenewalInfo} <-
    either (const $ failed "could not read Apple's subscription status") Right $ JT.parseEither J.parseJSON item
  SubscriptionTransaction {transactionId, txOriginalId, productId, bundleId, environment, purchaseDate, expiresDate, revoked, offer, familyShared, price, currency} <-
    verifiedItem signedTransactionInfo subscriptionTransactionP
  RenewalInfo {renewalOriginalId, autoRenewStatus} <- verifiedItem signedRenewalInfo renewalInfoP
  when (bundleId /= ourBundleId) $ failed "another app's bundle id, or ours misconfigured"
  when (txOriginalId /= originalId || renewalOriginalId /= originalId) $ failed "Apple answered about another subscription"
  when familyShared $ refused "family-shared"
  testSubscription <- case environment of
    "Production" -> Right False
    "Sandbox" -> Right True
    _ -> failed "an environment this verifier does not know"
  subscriptionState <- case (status, autoRenewStatus) of
    (1, 1) -> Right SSRenewing
    (1, 0) -> Right SSCancelled
    (3, _) -> Right SSBillingRetry
    (4, _) -> Right SSBillingRetry
    (2, _) -> Right SSEnded
    (5, _) -> Right SSEnded
    _ -> failed $ "a status " <> tshow status <> " with autoRenewStatus " <> tshow autoRenewStatus <> " this verifier does not know"
  pure
    SubscriptionCheck
      { subscriptionState,
        testSubscription,
        latestCharge =
          Just
            StoreCharge
              { chargeRef = transactionId,
                planId = productId,
                periodStart = fromMillis purchaseDate,
                periodEnd = fromMillis expiresDate,
                chargePaid = paidOf price currency,
                revoked,
                offer
              }
      }
  where
    verifiedItem jws p = do
      o <- either (Left . asVerifierFailure) Right $ verifiedPayload roots jws
      either (const $ failed "the payload Apple signed is not a subscription transaction or renewal info") Right $ JT.parseEither p o
    asVerifierFailure = \case
      SRInvalid reason -> SRVerifierFailed $ "apple verifier: " <> reason
      refusal -> refusal
    fromMillis ms = posixSecondsToUTCTime $ fromIntegral ms / 1000

newtype StatusResponse = StatusResponse [J.Value]

instance J.FromJSON StatusResponse where
  parseJSON = J.withObject "StatusResponse" $ \o -> do
    groups :: [J.Value] <- o J..: "data"
    StatusResponse . concat <$> mapM (J.withObject "SubscriptionGroupIdentifierItem" (J..: "lastTransactions")) groups

itemOriginalId :: J.Value -> Maybe Text
itemOriginalId = JT.parseMaybe $ J.withObject "LastTransactionsItem" (J..: "originalTransactionId")

data LastTransaction = LastTransaction
  { status :: Int,
    signedTransactionInfo :: Text,
    signedRenewalInfo :: Text
  }

instance J.FromJSON LastTransaction where
  parseJSON = J.withObject "LastTransactionsItem" $ \o ->
    LastTransaction <$> o J..: "status" <*> o J..: "signedTransactionInfo" <*> o J..: "signedRenewalInfo"

data SubscriptionTransaction = SubscriptionTransaction
  { transactionId :: Text,
    txOriginalId :: Text,
    productId :: Text,
    bundleId :: Text,
    environment :: Text,
    purchaseDate :: Int64,
    expiresDate :: Int64,
    revoked :: Bool,
    offer :: Bool, -- any offerType: a free trial, an introductory or promotional price, an offer code
    familyShared :: Bool,
    price :: Maybe Integer,
    currency :: Maybe Text
  }

subscriptionTransactionP :: J.Object -> JT.Parser SubscriptionTransaction
subscriptionTransactionP o = do
  transactionId <- o J..: "transactionId"
  txOriginalId <- o J..: "originalTransactionId"
  productId <- o J..: "productId"
  bundleId <- o J..: "bundleId"
  environment <- o J..: "environment"
  purchaseDate <- o J..: "purchaseDate"
  expiresDate <- o J..: "expiresDate"
  revoked <- isJust <$> (o J..:? "revocationDate" :: JT.Parser (Maybe J.Value))
  offer <- isJust <$> (o J..:? "offerType" :: JT.Parser (Maybe J.Value))
  familyShared <- (== Just ("FAMILY_SHARED" :: Text)) <$> o J..:? "inAppOwnershipType"
  price <- o J..:? "price"
  currency <- o J..:? "currency"
  pure SubscriptionTransaction {transactionId, txOriginalId, productId, bundleId, environment, purchaseDate, expiresDate, revoked, offer, familyShared, price, currency}

data RenewalInfo = RenewalInfo {renewalOriginalId :: Text, autoRenewStatus :: Int}

renewalInfoP :: J.Object -> JT.Parser RenewalInfo
renewalInfoP o = RenewalInfo <$> o J..: "originalTransactionId" <*> o J..: "autoRenewStatus"
