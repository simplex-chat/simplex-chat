{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeService.StoreVerifierTests (badgeStoreVerifierTests) where

import BadgeService.StoreReceipts
import BadgeService.StoreReceipts.Apple (readAppleRoot, verifyAppleTransaction)
import BadgeService.StoreReceipts.Google (playStoreVerifier)
import Bots.BadgeService.FakePlay
import Control.Concurrent.STM (readTVarIO)
import Control.Monad (forM_)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Number.Serialize (i2osp, i2ospOf_)
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Crypto.PubKey.ECC.Generate as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as LB
import Data.Hourglass (Date (..), DateTime (..), Month (..), TimeOfDay (..))
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.X509
import Simplex.Chat.PaymentService (ServicePayment (..), appleTransactionId, googlePurchaseRef)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec
import UnliftIO.Temporary (withTempDirectory)

badgeStoreVerifierTests :: Spec
badgeStoreVerifierTests = describe "badge store verifiers" $ do
  describe "App Store signed transactions" $ do
    it "vouches for a transaction a chain to the trusted root signed, naming the transaction it was claimed by" testAppleVerdict
    it "fails, rather than refuses, an intermediate the trusted root did not sign, since the root may be misconfigured" testAppleRootNotTrusted
    it "refuses a leaf the intermediate did not sign, though the intermediate is genuine" testAppleForgedLeaf
    it "fails, rather than refuses, a leaf signature the library cannot check" testAppleLeafUncheckable
    it "refuses a tampered payload" testAppleTamperedPayload
    it "refuses a chain that is not exactly leaf, intermediate and root" testAppleChainLength
    it "refuses a header that names another algorithm" testAppleOtherAlgorithm
    it "fails, rather than refuses, another app's bundle id, since ours may be misconfigured" testAppleOtherBundle
    it "refuses a refunded transaction" testAppleRevoked
    it "reports Sandbox, Xcode and LocalTesting transactions as test purchases" testAppleSandbox
    it "fails, rather than refuses, a chain Apple did not mark for signing receipts" testAppleUnmarkedChain
    it "fails, rather than refuses, a payload Apple signed in a shape it does not know" testAppleUnknownShape
    it "records the price in the currency's minor units, or not at all" testApplePrice
    it "reads the trusted root from a DER file, and reports one it cannot read" testReadAppleRoot
    it "has nothing to acknowledge" testAppleNoAcknowledgement
  describe "Google Play purchases" $ do
    it "vouches for a purchased token, asking Play about that token in this app" testPlayPurchased
    it "reports a license tester's purchase as a test purchase, and a promo code's as paid" testPlayTestAndPromo
    it "refuses a canceled purchase" testPlayCanceled
    it "answers a pending purchase as pending" testPlayPending
    it "answers 404, 5xx and every other error but 401 and 403 as unreachable" testPlayErrorStatuses
    it "fails on 401 and 403, and asks for a new access token after a 401" testPlayCredentialsRefused
    it "fails when the token endpoint refuses the assertion, and is unreachable when it is down" testPlayTokenEndpoint
    it "fails on a body it cannot read, another product, or an unknown state" testPlayUnreadable
    it "answers a Play that does not answer, or cannot be reached, as unreachable" testPlayHangsOrIsGone
    it "reuses its access token while it is valid" testPlayTokenCached
    it "asks for a new access token once the one it holds is about to expire" testPlayTokenExpires
    it "sends no token that is only dots, and describes a token outside the grammar without quoting it" testPlayTokensNotSent
    it "acknowledges a purchase, unless it is acknowledged or consumed already" testPlayAcknowledges
    it "reports an acknowledgement Play refuses or does not answer" testPlayAcknowledgementFails

-- * App Store

data Outcome = Invalid | Pending | Unreachable | Failed | NotConfigured | Verdict
  deriving (Eq, Show)

outcome :: Either StoreRefusal a -> Outcome
outcome = \case
  Left (SRInvalid _) -> Invalid
  Left SRPending -> Pending
  Left (SRUnreachable _) -> Unreachable
  Left (SRVerifierFailed _) -> Failed
  Left SRNotConfigured -> NotConfigured
  Right _ -> Verdict

reason :: Either StoreRefusal a -> Text
reason = \case
  Left (SRInvalid r) -> r
  Left (SRUnreachable r) -> r
  Left (SRVerifierFailed r) -> r
  _ -> ""

seamVerdict :: StoreVerifier -> ServicePayment -> IO (Either StoreRefusal VerifiedStoreTransaction)
seamVerdict verifier payment = case toStoreReceipt verifier payment of
  Just (Right StoreReceipt {verifyReceipt}) -> verifyReceipt
  Just (Left refusal) -> pure $ Left refusal
  Nothing -> expectationFailure "not a store payment" >> undefined

ourBundleId :: Text
ourBundleId = "chat.simplex.app"

appleVerdict :: SignedCertificate -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction)
appleVerdict root jws = seamVerdict noStoreVerifier {verifyApple = Just (verifyAppleTransaction root ourBundleId)} SPApple {jws}

data TestChain = TestChain
  { rootCert :: SignedCertificate,
    intermediateCert :: SignedCertificate,
    leafCert :: SignedCertificate,
    leafKey :: ECDSA.PrivateKey
  }

appleRootName, otherRootName, intermediateName, leafName :: DistinguishedName
appleRootName = DistinguishedName [([2, 5, 4, 3], "Apple Root CA - G3")]
otherRootName = DistinguishedName [([2, 5, 4, 3], "Some Other Root")]
intermediateName = DistinguishedName [([2, 5, 4, 3], "Apple Worldwide Developer Relations Certification Authority")]
leafName = DistinguishedName [([2, 5, 4, 3], "Prod ECC Mac App Store and iTunes Store Receipt Signing")]

receiptLeafMarker, intermediateMarker :: [Integer]
receiptLeafMarker = [1, 2, 840, 113635, 100, 6, 11, 1]
intermediateMarker = [1, 2, 840, 113635, 100, 6, 2, 1]

-- | Shaped as Apple's chain, under a root of the given name; the intermediate and the leaf carry the given markers.
newChain :: DistinguishedName -> [[Integer]] -> [[Integer]] -> IO TestChain
newChain rootName intermediateMarkers leafMarkers = do
  (rootKey, rootPub) <- newKeyPair
  (intermediateKey, intermediatePub) <- newKeyPair
  (leafKey, leafPub) <- newKeyPair
  rootCert <- certify rootKey rootName rootName rootPub []
  intermediateCert <- certify rootKey rootName intermediateName intermediatePub intermediateMarkers
  leafCert <- certify intermediateKey intermediateName leafName leafPub leafMarkers
  pure TestChain {rootCert, intermediateCert, leafCert, leafKey}

trustedChain :: IO TestChain
trustedChain = newChain appleRootName [intermediateMarker] [receiptLeafMarker]

newKeyPair :: IO (ECDSA.PrivateKey, PubKey)
newKeyPair = do
  (ECDSA.PublicKey _ q, k) <- ECC.generate p256
  case q of
    ECC.Point x y -> pure (k, PubKeyEC $ PubKeyEC_Named ECC.SEC_p256r1 $ SerializedPoint $ B.cons 4 $ i2ospOf_ 32 x <> i2ospOf_ 32 y)
    ECC.PointO -> newKeyPair

p256 :: ECC.Curve
p256 = ECC.getCurveByName ECC.SEC_p256r1

certify :: ECDSA.PrivateKey -> DistinguishedName -> DistinguishedName -> PubKey -> [[Integer]] -> IO SignedCertificate
certify = certifyWith $ SignatureALG HashSHA256 PubKeyALG_EC

certifyWith :: SignatureALG -> ECDSA.PrivateKey -> DistinguishedName -> DistinguishedName -> PubKey -> [[Integer]] -> IO SignedCertificate
certifyWith alg issuerKey issuerName subjectName subjectKey markers =
  objectToSignedExactF sign certificate
  where
    certificate =
      Certificate
        { certVersion = 2,
          certSerial = 1,
          certSignatureAlg = alg,
          certIssuerDN = issuerName,
          certValidity = (DateTime (Date 2020 January 1) (TimeOfDay 0 0 0 0), DateTime (Date 2040 January 1) (TimeOfDay 0 0 0 0)),
          certSubjectDN = subjectName,
          certPubKey = subjectKey,
          certExtensions = Extensions $ Just [ExtensionRaw oid False "\x05\x00" | oid <- markers]
        }
    sign tbs = (\(ECDSA.Signature r s) -> (derSignature r s, alg)) <$> ECDSA.sign issuerKey SHA256 tbs
    derSignature r s = der 0x30 (der 0x02 (unsigned r) <> der 0x02 (unsigned s))
    der tag content = B.pack [tag, fromIntegral (B.length content)] <> content
    unsigned n = let bs = i2osp n in if B.head bs >= 0x80 then B.cons 0 bs else bs

transactionPayload :: [(J.Key, J.Value)] -> [J.Key] -> J.Value
transactionPayload overrides removed =
  J.Object $ foldr KM.delete (KM.fromList overrides `KM.union` base) removed
  where
    base =
      KM.fromList
        [ ("transactionId", "2000000900000001"),
          ("originalTransactionId", "2000000900000001"),
          ("bundleId", J.String ourBundleId),
          ("productId", "BADGE_SUPPORTER_01"),
          ("purchaseDate", J.toJSON (1790000000000 :: Int)),
          ("quantity", J.toJSON (1 :: Int)),
          ("type", "Consumable"),
          ("inAppOwnershipType", "PURCHASED"),
          ("signedDate", J.toJSON (1790000001000 :: Int)),
          ("environment", "Production"),
          ("transactionReason", "PURCHASE"),
          ("storefront", "USA"),
          ("price", J.toJSON (7000 :: Int)),
          ("currency", "USD")
        ]

signedTransaction :: TestChain -> [SignedCertificate] -> J.Value -> J.Value -> IO Text
signedTransaction TestChain {leafKey} x5c header payload = do
  let signingInput = segment header' <> "." <> segment payload
      header' = case header of
        J.Object h -> J.Object $ KM.insert "x5c" (J.toJSON $ map (decodeUtf8 . B64.encode . encodeSignedObject) x5c) h
        v -> v
  ECDSA.Signature r s <- ECDSA.sign leafKey SHA256 signingInput
  pure $ decodeUtf8 $ signingInput <> "." <> B64U.encodeUnpadded (i2ospOf_ 32 r <> i2ospOf_ 32 s)

segment :: J.Value -> ByteString
segment = B64U.encodeUnpadded . LB.toStrict . J.encode

es256 :: J.Value
es256 = J.object ["alg" J..= ("ES256" :: Text)]

signedBy :: TestChain -> J.Value -> IO Text
signedBy chain@TestChain {rootCert, intermediateCert, leafCert} = signedTransaction chain [leafCert, intermediateCert, rootCert] es256

testAppleVerdict :: IO ()
testAppleVerdict = do
  chain <- trustedChain
  jws <- signedBy chain $ transactionPayload [] []
  appleVerdict (rootCert chain) jws
    `shouldReturn` Right VerifiedStoreTransaction {transactionRef = "2000000900000001", productId = "BADGE_SUPPORTER_01", quantity = 1, testPurchase = False, paid = Just (CurrencyAmount 700, "USD")}
  appleTransactionId jws `shouldBe` Just "2000000900000001"

testAppleRootNotTrusted :: IO ()
testAppleRootNotTrusted = do
  trusted <- trustedChain
  forM_ [appleRootName, otherRootName] $ \rootName -> do
    untrusted <- newChain rootName [intermediateMarker] [receiptLeafMarker]
    jws <- signedBy untrusted $ transactionPayload [] []
    r <- appleVerdict (rootCert trusted) jws
    outcome r `shouldBe` Failed
    reason r `shouldSatisfy` ("configured root" `T.isInfixOf`)

testAppleForgedLeaf :: IO ()
testAppleForgedLeaf = do
  chain@TestChain {rootCert, intermediateCert} <- trustedChain
  (forgedKey, forgedPub) <- newKeyPair
  forgedLeaf <- certify forgedKey intermediateName leafName forgedPub [receiptLeafMarker]
  jws <- signedTransaction chain {leafKey = forgedKey} [forgedLeaf, intermediateCert, rootCert] es256 $ transactionPayload [] []
  outcome <$> appleVerdict rootCert jws `shouldReturn` Invalid

testAppleLeafUncheckable :: IO ()
testAppleLeafUncheckable = do
  chain@TestChain {rootCert, intermediateCert} <- trustedChain
  (otherKey, otherPub) <- newKeyPair
  rsaLabelled <- certifyWith (SignatureALG HashSHA256 PubKeyALG_RSA) otherKey intermediateName leafName otherPub [receiptLeafMarker]
  jws <- signedTransaction chain {leafKey = otherKey} [rsaLabelled, intermediateCert, rootCert] es256 $ transactionPayload [] []
  r <- appleVerdict rootCert jws
  outcome r `shouldBe` Failed
  reason r `shouldSatisfy` ("cannot check" `T.isInfixOf`)

testAppleTamperedPayload :: IO ()
testAppleTamperedPayload = do
  chain <- trustedChain
  jws <- signedBy chain $ transactionPayload [] []
  case T.splitOn "." jws of
    [header, _, signature] -> do
      let tampered = T.intercalate "." [header, decodeUtf8 $ segment $ transactionPayload [("productId", "BADGE_LEGEND_01")] [], signature]
      outcome <$> appleVerdict (rootCert chain) tampered `shouldReturn` Invalid
    _ -> expectationFailure "not a JWS"

testAppleChainLength :: IO ()
testAppleChainLength = do
  chain@TestChain {rootCert, intermediateCert, leafCert} <- trustedChain
  forM_ [[leafCert, rootCert], [leafCert, intermediateCert], [leafCert, intermediateCert, rootCert, rootCert]] $ \x5c -> do
    jws <- signedTransaction chain x5c es256 $ transactionPayload [] []
    outcome <$> appleVerdict rootCert jws `shouldReturn` Invalid

testAppleOtherAlgorithm :: IO ()
testAppleOtherAlgorithm = do
  chain@TestChain {rootCert, intermediateCert, leafCert} <- trustedChain
  let x5c = [leafCert, intermediateCert, rootCert]
  forM_ ["none", "HS256", "ES384"] $ \alg -> do
    jws <- signedTransaction chain x5c (J.object ["alg" J..= (alg :: Text)]) $ transactionPayload [] []
    outcome <$> appleVerdict rootCert jws `shouldReturn` Invalid

testAppleOtherBundle :: IO ()
testAppleOtherBundle = do
  chain <- trustedChain
  jws <- signedBy chain $ transactionPayload [("bundleId", "com.example.other")] []
  r <- appleVerdict (rootCert chain) jws
  outcome r `shouldBe` Failed
  reason r `shouldSatisfy` ("bundle id" `T.isInfixOf`)

testAppleRevoked :: IO ()
testAppleRevoked = do
  chain <- trustedChain
  jws <- signedBy chain $ transactionPayload [("revocationDate", J.toJSON (1790000500000 :: Int)), ("revocationReason", J.toJSON (0 :: Int))] []
  outcome <$> appleVerdict (rootCert chain) jws `shouldReturn` Invalid

testAppleSandbox :: IO ()
testAppleSandbox = do
  chain <- trustedChain
  forM_ ["Sandbox", "Xcode", "LocalTesting"] $ \environment -> do
    jws <- signedBy chain $ transactionPayload [("environment", J.String environment)] []
    fmap testPurchase <$> appleVerdict (rootCert chain) jws `shouldReturn` Right True

testAppleUnmarkedChain :: IO ()
testAppleUnmarkedChain =
  forM_ [([intermediateMarker], []), ([], [receiptLeafMarker])] $ \(intermediateMarkers, leafMarkers) -> do
    unmarked <- newChain appleRootName intermediateMarkers leafMarkers
    jws <- signedBy unmarked $ transactionPayload [] []
    outcome <$> appleVerdict (rootCert unmarked) jws `shouldReturn` Failed

testAppleUnknownShape :: IO ()
testAppleUnknownShape = do
  chain <- trustedChain
  let failsOn payload = do
        jws <- signedBy chain payload
        outcome <$> appleVerdict (rootCert chain) jws `shouldReturn` Failed
  failsOn $ transactionPayload [("environment", "Staging")] []
  failsOn $ transactionPayload [] ["productId"]
  failsOn $ transactionPayload [("quantity", "1")] []

testApplePrice :: IO ()
testApplePrice = do
  chain <- trustedChain
  let paidFor fields removed = do
        jws <- signedBy chain $ transactionPayload fields removed
        fmap paid <$> appleVerdict (rootCert chain) jws
      priced price currency = paidFor [("price", J.toJSON (price :: Int)), ("currency", currency)] []
  -- Apple's own examples: $1.99, JPY 300 and KRW 3300
  priced 1990 "USD" `shouldReturn` Right (Just (CurrencyAmount 199, "USD"))
  priced 300000 "JPY" `shouldReturn` Right (Just (CurrencyAmount 300, "JPY"))
  priced 3300000 "KRW" `shouldReturn` Right (Just (CurrencyAmount 3300, "KRW"))
  priced 1234 "KWD" `shouldReturn` Right (Just (CurrencyAmount 1234, "KWD"))
  priced 1995 "USD" `shouldReturn` Right Nothing
  priced 1000 "CLF" `shouldReturn` Right Nothing
  priced 1000 "usd" `shouldReturn` Right Nothing
  paidFor [] ["price", "currency"] `shouldReturn` Right Nothing

testReadAppleRoot :: IO ()
testReadAppleRoot = do
  TestChain {rootCert} <- trustedChain
  createDirectoryIfMissing True "tests/tmp"
  withTempDirectory "tests/tmp" "apple-root" $ \d -> do
    B.writeFile (d </> "root.cer") (encodeSignedObject rootCert)
    readAppleRoot (d </> "root.cer") `shouldReturn` Right rootCert
    B.writeFile (d </> "root.pem") "-----BEGIN CERTIFICATE-----\n"
    readAppleRoot (d </> "root.pem") >>= (`shouldSatisfy` either ("not a DER certificate" `isInfixOf`) (const False))
    readAppleRoot (d </> "absent.cer") >>= (`shouldSatisfy` either ("could not be read" `isInfixOf`) (const False))

testAppleNoAcknowledgement :: IO ()
testAppleNoAcknowledgement = do
  chain <- trustedChain
  jws <- signedBy chain $ transactionPayload [] []
  withFakePlay $ \fake -> do
    verifier <- playVerifier fake
    acknowledgement verifier {verifyApple = Just (verifyAppleTransaction (rootCert chain) ourBundleId)} SPApple {jws} `shouldReturn` Nothing

-- * Google Play

playVerifier :: FakePlay -> IO StoreVerifier
playVerifier fake =
  playStoreVerifier (fpConfig fake) >>= \case
    Right (verify, acknowledge) -> pure noStoreVerifier {verifyGoogle = Just verify, acknowledgeGoogle = Just acknowledge, verifyTimeout = 2000000}
    Left e -> expectationFailure e >> undefined

acknowledgement :: StoreVerifier -> ServicePayment -> IO (Maybe (Either StoreRefusal ()))
acknowledgement verifier payment = case toStoreReceipt verifier payment of
  Just (Right StoreReceipt {acknowledgeReceipt}) -> sequence acknowledgeReceipt
  _ -> expectationFailure "not a store receipt" >> undefined

playVerdict :: StoreVerifier -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction)
playVerdict verifier token = seamVerdict verifier SPGoogle {productId = "badge_supporter_01", token}

purchaseRecord :: Int -> [(J.Key, J.Value)] -> J.Value
purchaseRecord state fields =
  J.Object $
    KM.fromList fields
      `KM.union` KM.fromList
        [ ("kind", "androidpublisher#productPurchase"),
          ("purchaseTimeMillis", "1790000000000"),
          ("purchaseState", J.toJSON state),
          ("consumptionState", J.toJSON (0 :: Int)),
          ("orderId", "GPA.3312-4521-8899-01234"),
          ("acknowledgementState", J.toJSON (0 :: Int)),
          ("regionCode", "US")
        ]

testToken :: Text
testToken = "fake-play-token.AO-J1Oz9x2kqE7wYt3"

-- | Every reason reaches the service's log, which must never hold the token.
answeredAs :: StoreVerifier -> Text -> Outcome -> IO ()
answeredAs verifier token expected = do
  r <- playVerdict verifier token
  outcome r `shouldBe` expected
  reason r `shouldNotSatisfy` (token `T.isInfixOf`)

testPlayPurchased :: IO ()
testPlayPurchased = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 []
  playVerdict verifier testToken
    `shouldReturn` Right VerifiedStoreTransaction {transactionRef = googlePurchaseRef testToken, productId = "badge_supporter_01", quantity = 1, testPurchase = False, paid = Nothing}
  readTVarIO (fpPurchasePaths fake)
    `shouldReturn` [["androidpublisher", "v3", "applications", fakePackageName, "purchases", "products", "badge_supporter_01", "tokens", testToken]]
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 [("quantity", J.toJSON (2 :: Int)), ("productId", "badge_supporter_01")]
  fmap quantity <$> playVerdict verifier testToken `shouldReturn` Right 2

testPlayTestAndPromo :: IO ()
testPlayTestAndPromo = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 [("purchaseType", J.toJSON (0 :: Int))]
  fmap testPurchase <$> playVerdict verifier testToken `shouldReturn` Right True
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 [("purchaseType", J.toJSON (1 :: Int))]
  fmap testPurchase <$> playVerdict verifier testToken `shouldReturn` Right False

testPlayCanceled :: IO ()
testPlayCanceled = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 1 []
  answeredAs verifier testToken Invalid

testPlayPending :: IO ()
testPlayPending = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 2 []
  answeredAs verifier testToken Pending

testPlayErrorStatuses :: IO ()
testPlayErrorStatuses = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answeredAs verifier testToken Unreachable
  forM_ [400, 404, 410, 429, 500, 503] $ \code -> do
    answerPurchase fake testToken $ PlayStatus code
    answeredAs verifier testToken Unreachable

testPlayCredentialsRefused :: IO ()
testPlayCredentialsRefused = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answerPurchase fake testToken $ PlayStatus 403
  answeredAs verifier testToken Failed
  answerPurchase fake testToken $ PlayStatus 401
  answeredAs verifier testToken Failed
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 []
  answeredAs verifier testToken Verdict
  length <$> readTVarIO (fpGrantedTokens fake) `shouldReturn` 2

testPlayTokenEndpoint :: IO ()
testPlayTokenEndpoint = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 []
  forM_ [400, 401] $ \code -> do
    answerTokenRequests fake $ Just code
    answeredAs verifier testToken Failed
  forM_ [429, 503] $ \code -> do
    answerTokenRequests fake $ Just code
    answeredAs verifier testToken Unreachable
  answerTokenRequests fake Nothing
  answeredAs verifier testToken Verdict
  readTVarIO (fpPurchasePaths fake) >>= (`shouldSatisfy` ((== 1) . length))

testPlayUnreadable :: IO ()
testPlayUnreadable = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  forM_
    [ PlayBody "<html>not json</html>",
      PlayBody "{\"purchaseState\":\"0\"}",
      PlayRecord $ J.object ["kind" J..= ("androidpublisher#productPurchase" :: Text)],
      PlayRecord $ purchaseRecord 0 [("productId", "badge_legend_01")],
      PlayRecord $ purchaseRecord 3 [],
      PlayRecord $ purchaseRecord 0 [("purchaseType", J.toJSON (2 :: Int))],
      PlayBody $ J.encode (purchaseRecord 0 []) <> LB.replicate (1024 * 1024) 32
    ]
    $ \answer -> do
      answerPurchase fake testToken answer
      answeredAs verifier testToken Failed

testPlayHangsOrIsGone :: IO ()
testPlayHangsOrIsGone = do
  gone <- withFakePlay $ \fake -> do
    verifier <- playVerifier fake
    answerPurchase fake testToken PlayHang
    answeredAs verifier testToken Unreachable
    pure verifier
  answeredAs gone testToken Unreachable

testPlayTokenCached :: IO ()
testPlayTokenCached = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 []
  answeredAs verifier testToken Verdict
  answeredAs verifier testToken Verdict
  length <$> readTVarIO (fpGrantedTokens fake) `shouldReturn` 1

testPlayTokenExpires :: IO ()
testPlayTokenExpires = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 []
  grantTokensFor fake 300
  answeredAs verifier testToken Verdict
  answeredAs verifier testToken Verdict
  length <$> readTVarIO (fpGrantedTokens fake) `shouldReturn` 2

testPlayTokensNotSent :: IO ()
testPlayTokensNotSent = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answeredAs verifier ".." Unreachable
  let outside = "fake/play+token=AO-J1Oz9x2kqE7wYt3"
  r <- playVerdict verifier outside
  outcome r `shouldBe` Unreachable
  reason r `shouldBe` "a Play token this service will not send: 34 characters: lowercase, uppercase, digits, '-', other printable ASCII"
  readTVarIO (fpPurchasePaths fake) `shouldReturn` []

testPlayAcknowledges :: IO ()
testPlayAcknowledges = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  let acknowledging = acknowledgement verifier SPGoogle {productId = "badge_supporter_01", token = testToken}
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 []
  acknowledging `shouldReturn` Just (Right ())
  readTVarIO (fpAcknowledged fake) `shouldReturn` [testToken]
  forM_ ["acknowledgementState", "consumptionState"] $ \state -> do
    answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 [(state, J.toJSON (1 :: Int))]
    acknowledging `shouldReturn` Just (Right ())
  readTVarIO (fpAcknowledged fake) `shouldReturn` [testToken]

testPlayAcknowledgementFails :: IO ()
testPlayAcknowledgementFails = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  let acknowledgedAs expected = do
        r <- acknowledgement verifier SPGoogle {productId = "badge_supporter_01", token = testToken}
        outcome <$> r `shouldBe` Just expected
        maybe "" reason r `shouldNotSatisfy` (testToken `T.isInfixOf`)
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 []
  answerAcknowledgements fake $ Just 403
  acknowledgedAs Failed
  answerAcknowledgements fake $ Just 503
  acknowledgedAs Unreachable
  answerAcknowledgements fake Nothing
  answerPurchase fake testToken PlayHang
  acknowledgedAs Unreachable
  readTVarIO (fpAcknowledged fake) `shouldReturn` []

