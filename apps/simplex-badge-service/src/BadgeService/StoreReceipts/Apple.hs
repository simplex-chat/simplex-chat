{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeService.StoreReceipts.Apple
  ( verifyAppleTransaction,
    readAppleRoot,
  )
where

import BadgeService.StoreReceipts (VerifiedStoreTransaction (..))
import Control.Exception (Exception, IOException, throw, try)
import Control.Monad (guard, unless, when)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.Serialize (os2ip)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.Types as JT
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import Data.Foldable (toList)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Data.X509
import Data.X509.EC (unserializePoint)
import Data.X509.Validation (SignatureVerification (..), verifySignedSignature)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import Simplex.Messaging.Util (eitherToMaybe)

-- | Thrown, never returned, for evidence Apple did sign but in a shape this verifier does not know,
-- since every Left is a terminal refusal.
newtype AppleVerifierFailure = AppleVerifierFailure String
  deriving (Show)

instance Exception AppleVerifierFailure

verifyAppleTransaction :: SignedCertificate -> Text -> Text -> Either Text VerifiedStoreTransaction
verifyAppleTransaction root ourBundleId jws = case T.splitOn "." jws of
  [header, payload, signature] -> do
    leaf <- signingChain root =<< certificateChain header
    sig <- jwsSignature signature
    unless (ECDSA.verify SHA256 (leafKey leaf) sig (encodeUtf8 $ header <> "." <> payload)) $
      Left "the signature does not verify"
    transaction ourBundleId $ either (failed . ("the payload Apple signed is not a transaction: " <>)) id $ do
      o <- maybe (Left "not base64url JSON") Right $ jsonObject payload
      JT.parseEither appleTransactionP o
  _ -> Left "not three dot-separated parts"

certificateChain :: Text -> Either Text [ByteString]
certificateChain header = do
  o <- maybe (Left "the header is not base64url JSON") Right $ jsonObject header
  case JM.lookup "alg" o of
    Just (J.String "ES256") -> Right ()
    _ -> Left "the header does not name ES256"
  case JM.lookup "x5c" o of
    Just (J.Array certs) -> traverse der $ toList certs
    _ -> Left "the header carries no x5c chain"
  where
    der = \case
      J.String c | Right bs <- B64.decode (encodeUtf8 c) -> Right bs
      _ -> Left "an x5c certificate is not base64"

-- | The root in the chain is ignored: trust comes from the configured one alone.
signingChain :: SignedCertificate -> [ByteString] -> Either Text SignedCertificate
signingChain root = \case
  [leafDer, intermediateDer, _] -> do
    leaf <- certificate leafDer
    intermediate <- certificate intermediateDer
    -- the configured root may be stale or wrong as likely as the receipt forged, so this decides nothing
    when (certIssuerDN (getCertificate intermediate) /= certSubjectDN (getCertificate root)) $
      failed "the intermediate is issued by a root other than the configured one"
    signedBy root intermediate "the intermediate is not signed by the configured root"
    signedBy intermediate leaf "the leaf is not signed by the intermediate"
    -- Apple issues other certificates under this root, developers' among them; only these two mark the receipt-signing chain
    unless (marked receiptSigningLeaf leaf && marked appleIntermediate intermediate) $
      failed "Apple issued this chain, but not to sign receipts"
    pure leaf
  _ -> Left "x5c is not leaf, intermediate and root"
  where
    certificate = either (const $ Left "an x5c certificate does not decode") Right . decodeSignedCertificate
    signedBy issuer cert refusal = case verifySignedSignature cert (certPubKey $ getCertificate issuer) of
      SignaturePass -> Right ()
      SignatureFailed _ -> Left refusal
    marked oid cert = case certExtensions (getCertificate cert) of
      Extensions (Just exts) -> any ((== oid) . extRawOID) exts
      Extensions Nothing -> False
    receiptSigningLeaf = [1, 2, 840, 113635, 100, 6, 11, 1]
    appleIntermediate = [1, 2, 840, 113635, 100, 6, 2, 1]

leafKey :: SignedCertificate -> ECDSA.PublicKey
leafKey leaf = case certPubKey (getCertificate leaf) of
  PubKeyEC (PubKeyEC_Named ECC.SEC_p256r1 pt) | Just p <- unserializePoint p256 pt -> ECDSA.PublicKey p256 p
  _ -> failed "the leaf Apple issued holds no P-256 key"
  where
    p256 = ECC.getCurveByName ECC.SEC_p256r1

-- | JWS carries ES256 as r and s, 32 bytes each, not the DER a certificate uses.
jwsSignature :: Text -> Either Text ECDSA.Signature
jwsSignature s = case B64U.decodeUnpadded (encodeUtf8 s) of
  Right bs | B.length bs == 64 -> let (r, s') = B.splitAt 32 bs in Right $ ECDSA.Signature (os2ip r) (os2ip s')
  _ -> Left "the signature is not 64 base64url bytes"

data AppleTransaction = AppleTransaction
  { transactionId :: Text,
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
  transactionId <- o J..: "transactionId"
  productId <- o J..: "productId"
  bundleId <- o J..: "bundleId"
  environment <- o J..: "environment"
  quantity <- o J..: "quantity"
  revoked <- isJust <$> (o J..:? "revocationDate" :: JT.Parser (Maybe J.Value))
  price <- o J..:? "price"
  currency <- o J..:? "currency"
  pure AppleTransaction {transactionId, productId, bundleId, environment, quantity, revoked, price, currency}

transaction :: Text -> AppleTransaction -> Either Text VerifiedStoreTransaction
transaction ourBundleId AppleTransaction {transactionId, productId, bundleId, environment, quantity, revoked, price, currency}
  | bundleId /= ourBundleId = Left "another app's bundle id"
  | revoked = Left "refunded or revoked"
  | otherwise =
      Right
        VerifiedStoreTransaction
          { transactionRef = transactionId,
            productId,
            quantity,
            testPurchase,
            paid = paidOf price currency
          }
  where
    testPurchase = case environment of
      "Production" -> False
      "Sandbox" -> True
      "Xcode" -> True
      "LocalTesting" -> True
      _ -> failed "an environment this verifier does not know"

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

failed :: String -> a
failed = throw . AppleVerifierFailure

readAppleRoot :: FilePath -> IO (Either String SignedCertificate)
readAppleRoot path =
  try (B.readFile path) >>= \case
    Left (_ :: IOException) -> pure $ Left $ path <> ": could not be read"
    Right der -> pure $ either (const $ Left $ path <> ": is not a DER certificate") Right $ decodeSignedCertificate der
