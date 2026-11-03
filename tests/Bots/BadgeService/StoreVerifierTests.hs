{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeService.StoreVerifierTests
  ( badgeStoreVerifierTests,
    TestChain (..),
    trustedChain,
    signedBy,
    subscriptionPayload,
    transactionPayload,
    ourBundleId,
    purchaseRecord,
    subscriptionRecord,
    lineItem,
    basePlan,
  )
where

import BadgeService.Config (AppleServerApi (..), AppleStoreConfig (..), PlayStoreConfig (..), ServiceConfig (..), readServiceConfig)
import BadgeService.Service (ServiceState (..), configureStoreVerifier, newServiceState)
import BadgeService.StoreReceipts
import BadgeService.StoreReceipts.Apple (appleSubscriptionChecker, readAppleRoot, verifyAppleTransaction)
import BadgeService.StoreReceipts.Google (PlayVerifier (..), playStoreVerifier)
import Bots.BadgeService.FakeAppStore
import Bots.BadgeService.FakePlay
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (throwIO)
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
import Data.Either (isRight)
import Data.Hourglass (Date (..), DateTime (..), Month (..), TimeOfDay (..))
import Data.List (isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.X509
import Simplex.Chat.Badges.Service (SubscriptionState (..))
import Simplex.Chat.PaymentService (ServicePayment (..), appleTransactionRef, googlePurchaseRef)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec
import UnliftIO.Temporary (withTempDirectory)

badgeStoreVerifierTests :: Spec
badgeStoreVerifierTests = describe "badge store verifiers" $ do
  describe "App Store signed transactions" $ do
    it "vouches for a transaction a chain to the trusted root signed, naming the transaction it was claimed by" testAppleVerdict
    it "vouches for a subscription renewal by its original transaction id, the ref it is claimed by" testAppleRenewalVerdict
    it "verifies a transaction under any of the trusted roots" testAppleAnyTrustedRoot
    it "fails, rather than refuses, an intermediate the trusted root did not sign, since the root may be misconfigured" testAppleRootNotTrusted
    it "refuses a leaf the intermediate did not sign, though the intermediate is genuine" testAppleForgedLeaf
    it "fails, rather than refuses, a leaf signature the library cannot check" testAppleLeafUncheckable
    it "refuses a tampered payload" testAppleTamperedPayload
    it "fails, rather than refuses, a chain that is not exactly leaf, intermediate and root, a shape Apple may yet adopt" testAppleChainLength
    it "fails, rather than refuses, a header that names another algorithm, a shape Apple may yet adopt" testAppleOtherAlgorithm
    it "fails, rather than refuses, another app's bundle id, since ours may be misconfigured" testAppleOtherBundle
    it "refuses a refunded transaction" testAppleRevoked
    it "reports a Sandbox transaction as a test purchase" testAppleSandbox
    it "fails, rather than refuses, a chain Apple did not mark for signing receipts" testAppleUnmarkedChain
    it "fails, rather than refuses, a payload Apple signed in a shape it does not know" testAppleUnknownShape
    it "fails, rather than refuses, a leaf holding a key it cannot read" testAppleLeafKey
    it "records the price in the currency's minor units, or not at all" testApplePrice
    it "reads the trusted root from a DER file, and reports one it cannot read or that is not a root" testReadAppleRoot
    it "has nothing to acknowledge" testAppleNoAcknowledgement
  describe "App Store subscriptions" $ do
    it "maps each status, renewing or cancelled by autoRenewStatus, and the charge to the latest transaction, with one token reused" testAppleSubscriptionStatuses
    it "reads the subscription asked about from an answer that lists another one first" testAppleSubscriptionAmongOthers
    it "reports a revoked charge and one in an offer, refuses a family-shared one, and names a Sandbox one a test purchase" testAppleSubscriptionCharges
    it "fails, rather than refuses, a status Apple did not sign or that was changed after signing" testAppleSubscriptionUnsigned
    it "fails on a verifier that cannot place what the store signed, and is unreachable on any other throw" testSubscriptionCheckThrows
    it "refuses a check the store marks as a test, and passes it through only with credit_test_purchases on" testSubscriptionCheckCreditsTest
    it "fails on 401 and 403, and is unreachable on 404, 429, 5xx, no answer and an answer naming no such subscription" testAppleSubscriptionHttp
    it "fails on a key Apple refuses, and at boot on a key file it cannot read" testAppleSubscriptionKey
    it "checks subscriptions only where [apple] sets the API key, and one-time purchases either way" testAppleConfiguredSubscriptions
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
    it "reads a purchase once to verify and acknowledge it, and acknowledges only what that read found unacknowledged" testPlayReadsPurchaseOnce
    it "reports an acknowledgement Play refuses or does not answer" testPlayAcknowledgementFails
    it "acknowledges through the verifier a [google] section configures" testPlayConfiguredAcknowledges
  describe "Google Play subscriptions" $ do
    it "maps each state, renewing or cancelled by autoRenewEnabled, asking Play by the token alone" testPlaySubscriptionStates
    it "answers a pending subscription as pending, and a license tester's as a test purchase" testPlaySubscriptionPendingOrTest
    it "reads the charge from the line item, its period starting the plan's length before it ends" testPlaySubscriptionCharge
    it "flags a charge in a free trial or an introductory price, and not one bought on an offer now at its base price" testPlaySubscriptionOffer
    it "fails on a record it cannot read, an unknown state, or a charge without its period or plan" testPlaySubscriptionUnreadable
    it "fails on 401 and 403, is unreachable on 404, 429, 5xx and no answer, and ended on 410" testPlaySubscriptionHttp
    it "acknowledges a subscription once, through the subscriptions endpoint, unless Play reports it acknowledged" testPlaySubscriptionAcknowledges
    it "reports a subscription acknowledgement Play refuses or does not answer" testPlaySubscriptionAcknowledgementFails
    it "checks Play subscriptions only where [google] is configured, beside the App Store's" testPlayConfiguredSubscriptions

-- * App Store

-- TestPurchase is the service's own refusal after a verifier vouched, so no verifier returns it here
data Outcome = Invalid | TestPurchase | Pending | Unreachable | Failed | NotConfigured | Verdict
  deriving (Eq, Show)

outcome :: Either StoreRefusal a -> Outcome
outcome = \case
  Left (SRInvalid _) -> Invalid
  Left SRTestPurchase -> TestPurchase
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
appleVerdict root = appleVerdictUnder (root :| [])

appleVerdictUnder :: NonEmpty SignedCertificate -> Text -> IO (Either StoreRefusal VerifiedStoreTransaction)
appleVerdictUnder roots jws = seamVerdict noStoreVerifier {verifyApple = Just (verifyAppleTransaction roots ourBundleId)} SPApple {jws}

data TestChain = TestChain
  { rootCert :: SignedCertificate,
    intermediateCert :: SignedCertificate,
    leafCert :: SignedCertificate,
    intermediateKey :: ECDSA.PrivateKey,
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
  pure TestChain {rootCert, intermediateCert, leafCert, intermediateKey, leafKey}

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
    `shouldReturn` Right VerifiedStoreTransaction {transactionRef = "2000000900000001", productId = "BADGE_SUPPORTER_01", quantity = 1, testPurchase = False, paid = Just (CurrencyAmount 700, "USD"), alreadyAcknowledged = False}
  appleTransactionRef jws `shouldBe` Just "2000000900000001"

-- the service answers internal when the verified ref is not the claimed one, so a renewal must be vouched for by the claim's own ref
testAppleRenewalVerdict :: IO ()
testAppleRenewalVerdict = do
  chain <- trustedChain
  jws <- signedBy chain $ transactionPayload [("transactionId", "2000000900000002"), ("type", "Auto-Renewable Subscription"), ("productId", "SUBSCR_BADGE_SUPPORTER_MONTH_01")] []
  r <- appleVerdict (rootCert chain) jws
  fmap transactionRef r `shouldBe` Right "2000000900000001"
  appleTransactionRef jws `shouldBe` Just "2000000900000001"
  -- an auto-renewable transaction without its original id names no subscription
  noOriginal <- signedBy chain $ transactionPayload [("type", "Auto-Renewable Subscription")] ["originalTransactionId"]
  outcome <$> appleVerdict (rootCert chain) noOriginal `shouldReturn` Invalid

testAppleAnyTrustedRoot :: IO ()
testAppleAnyTrustedRoot = do
  old <- trustedChain
  new <- newChain otherRootName [intermediateMarker] [receiptLeafMarker]
  forM_ [old, new] $ \chain -> do
    jws <- signedBy chain $ transactionPayload [] []
    fmap transactionRef <$> appleVerdictUnder (rootCert old :| [rootCert new]) jws `shouldReturn` Right "2000000900000001"

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
    outcome <$> appleVerdict rootCert jws `shouldReturn` Failed

testAppleOtherAlgorithm :: IO ()
testAppleOtherAlgorithm = do
  chain@TestChain {rootCert, intermediateCert, leafCert} <- trustedChain
  let x5c = [leafCert, intermediateCert, rootCert]
  forM_ ["none", "HS256", "ES384"] $ \alg -> do
    jws <- signedTransaction chain x5c (J.object ["alg" J..= (alg :: Text)]) $ transactionPayload [] []
    outcome <$> appleVerdict rootCert jws `shouldReturn` Failed

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
  jws <- signedBy chain $ transactionPayload [("environment", "Sandbox")] []
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
  failsOn $ transactionPayload [("environment", "Xcode")] []
  failsOn $ transactionPayload [("environment", "LocalTesting")] []
  failsOn $ transactionPayload [] ["productId"]
  failsOn $ transactionPayload [("quantity", "1")] []

-- | The key is read before the signature it would check, so the chain and the payload are genuine.
testAppleLeafKey :: IO ()
testAppleLeafKey = do
  chain@TestChain {rootCert, intermediateCert, intermediateKey} <- trustedChain
  let failsOnLeafKey pub = do
        leaf <- certify intermediateKey intermediateName leafName pub [receiptLeafMarker]
        jws <- signedTransaction chain [leaf, intermediateCert, rootCert] es256 $ transactionPayload [] []
        outcome <$> appleVerdict rootCert jws `shouldReturn` Failed
  failsOnLeafKey $ PubKeyEC $ PubKeyEC_Named ECC.SEC_p384r1 $ SerializedPoint $ B.cons 4 $ B.replicate 64 1
  failsOnLeafKey $ PubKeyEC $ PubKeyEC_Named ECC.SEC_p256r1 $ SerializedPoint "\x04"

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
  TestChain {rootCert, intermediateCert} <- trustedChain
  readAppleRoot ("apps" </> "simplex-badge-service" </> "test-fixtures" </> "apple" </> "AppleRootCA-G3.cer") >>= (`shouldSatisfy` isRight)
  createDirectoryIfMissing True "tests/tmp"
  withTempDirectory "tests/tmp" "apple-root" $ \d -> do
    B.writeFile (d </> "root.cer") (encodeSignedObject rootCert)
    readAppleRoot (d </> "root.cer") `shouldReturn` Right rootCert
    B.writeFile (d </> "root.pem") "-----BEGIN CERTIFICATE-----\n"
    readAppleRoot (d </> "root.pem") >>= (`shouldSatisfy` either ("not a DER certificate" `isInfixOf`) (const False))
    readAppleRoot (d </> "absent.cer") >>= (`shouldSatisfy` either ("could not be read" `isInfixOf`) (const False))
    B.writeFile (d </> "intermediate.cer") (encodeSignedObject intermediateCert)
    readAppleRoot (d </> "intermediate.cer") >>= (`shouldSatisfy` either ("not a self-signed root certificate" `isInfixOf`) (const False))

testAppleNoAcknowledgement :: IO ()
testAppleNoAcknowledgement = do
  chain <- trustedChain
  jws <- signedBy chain $ transactionPayload [] []
  withFakePlay $ \fake -> do
    verifier <- playVerifier fake
    acknowledgement verifier {verifyApple = Just (verifyAppleTransaction (rootCert chain :| []) ourBundleId)} SPApple {jws} `shouldReturn` Nothing

-- * App Store subscriptions

subscriptionId :: Text
subscriptionId = "2000000900000001"

periodStartMs, periodEndMs :: Int
periodStartMs = 1790000000000
periodEndMs = periodStartMs + 30 * 86400000

-- | A renewal of the subscription subscriptionId names: a transaction of its own, under the original one.
subscriptionPayload :: [(J.Key, J.Value)] -> [J.Key] -> J.Value
subscriptionPayload overrides =
  transactionPayload $
    [ ("transactionId", "2000000900000002"),
      ("type", "Auto-Renewable Subscription"),
      ("productId", "SUBSCR_BADGE_SUPPORTER_MONTH_01"),
      ("purchaseDate", J.toJSON periodStartMs),
      ("expiresDate", J.toJSON periodEndMs),
      ("transactionReason", "RENEWAL")
    ]
      <> overrides

renewalPayload :: [(J.Key, J.Value)] -> J.Value
renewalPayload overrides =
  J.Object $
    KM.fromList $
      [ ("originalTransactionId", J.String subscriptionId),
        ("autoRenewStatus", J.toJSON (1 :: Int)),
        ("autoRenewProductId", "SUBSCR_BADGE_SUPPORTER_MONTH_01"),
        ("productId", "SUBSCR_BADGE_SUPPORTER_MONTH_01"),
        ("environment", "Production"),
        ("signedDate", J.toJSON (1790000001000 :: Int))
      ]
        <> overrides

-- | Get All Subscription Statuses as Apple answers it, with the subscription's latest transaction.
statusBody :: Int -> Text -> Text -> J.Value
statusBody status signedTx signedRenewal = statusListing [lastTransaction subscriptionId status signedTx signedRenewal]

-- | Get All Subscription Statuses listing these subscriptions, all in one subscription group.
statusListing :: [J.Value] -> J.Value
statusListing lastTransactions =
  J.object
    [ "environment" J..= ("Production" :: Text),
      "bundleId" J..= ourBundleId,
      "data"
        J..= [ J.object
                 [ "subscriptionGroupIdentifier" J..= ("21000000" :: Text),
                   "lastTransactions" J..= lastTransactions
                 ]
             ]
    ]

lastTransaction :: Text -> Int -> Text -> Text -> J.Value
lastTransaction originalId status signedTx signedRenewal =
  J.object ["originalTransactionId" J..= originalId, "status" J..= status, "signedTransactionInfo" J..= signedTx, "signedRenewalInfo" J..= signedRenewal]

-- | The check the service runs, through the same runner as a purchase and a renewal.
withAppleChecker :: TestChain -> (FakeAppStore -> (Text -> IO (Either StoreRefusal SubscriptionCheck)) -> IO a) -> IO a
withAppleChecker chain action = withFakeAppStore ourBundleId $ \fake -> do
  check <- appleChecker chain (fasApi fake)
  action fake $ \originalId -> checkStoreSubscription noStoreVerifier {checkSubscription = Just (\ref _ -> check ref), verifyTimeout = 2000000} (SubscriptionApple originalId) Nothing

appleChecker :: TestChain -> AppleServerApi -> IO (SubscriptionRef -> IO (Either StoreRefusal SubscriptionCheck))
appleChecker chain serverApi =
  appleSubscriptionChecker (rootCert chain :| []) ourBundleId serverApi >>= \case
    Right check -> pure $ \case
      SubscriptionApple originalId -> check originalId
      SubscriptionGoogle _ -> pure $ Left SRNotConfigured
    Left e -> expectationFailure e >> undefined

-- | Signs the transaction and the renewal info with the given chain, and serves them as the subscription's status.
serveStatus :: FakeAppStore -> TestChain -> Int -> J.Value -> J.Value -> IO ()
serveStatus fake chain status transaction renewal = do
  signedTx <- signedBy chain transaction
  signedRenewal <- signedBy chain renewal
  answerStatus fake subscriptionId $ AppStoreBody $ statusBody status signedTx signedRenewal

stateOf :: Either StoreRefusal SubscriptionCheck -> Either StoreRefusal SubscriptionState
stateOf = fmap $ \SubscriptionCheck {subscriptionState} -> subscriptionState

chargeOf :: Either StoreRefusal SubscriptionCheck -> Either StoreRefusal (Maybe (Text, Text, UTCTime, UTCTime, Maybe (CurrencyAmount, Text), Bool, Bool))
chargeOf = fmap $ \SubscriptionCheck {latestCharge} ->
  (\StoreCharge {chargeRef, planId, periodStart, periodEnd, chargePaid, revoked, offer} -> (chargeRef, planId, periodStart, periodEnd, chargePaid, revoked, offer)) <$> latestCharge

testAppleSubscriptionStatuses :: IO ()
testAppleSubscriptionStatuses = do
  chain <- trustedChain
  withAppleChecker chain $ \fake check -> do
    let stateFor status autoRenew = do
          serveStatus fake chain status (subscriptionPayload [] []) (renewalPayload [("autoRenewStatus", J.toJSON (autoRenew :: Int))])
          stateOf <$> check subscriptionId
    stateFor 1 1 `shouldReturn` Right SSRenewing
    stateFor 1 0 `shouldReturn` Right SSCancelled
    stateFor 3 1 `shouldReturn` Right SSBillingRetry
    stateFor 4 1 `shouldReturn` Right SSBillingRetry
    stateFor 2 0 `shouldReturn` Right SSEnded
    stateFor 5 0 `shouldReturn` Right SSEnded
    outcome <$> stateFor 6 1 `shouldReturn` Failed
    -- the charge is the latest transaction: its own id, its product as the plan, and its period
    serveStatus fake chain 1 (subscriptionPayload [] []) (renewalPayload [])
    chargeOf <$> check subscriptionId
      `shouldReturn` Right (Just ("2000000900000002", "SUBSCR_BADGE_SUPPORTER_MONTH_01", millis periodStartMs, millis periodEndMs, Just (CurrencyAmount 700, "USD"), False, False))
    -- each call carried a token the fake accepted, signed once and reused
    tokens <- readTVarIO (fasTokens fake)
    length tokens `shouldBe` 8
    length (nub tokens) `shouldBe` 1
  where
    millis ms = posixSecondsToUTCTime $ fromIntegral ms / 1000

-- | Apple lists every subscription the customer holds in the app, a family member's shared one included.
testAppleSubscriptionAmongOthers :: IO ()
testAppleSubscriptionAmongOthers = do
  chain <- trustedChain
  withAppleChecker chain $ \fake check -> do
    let sharedId = "2000000900000011"
    sharedTx <- signedBy chain $ subscriptionPayload [("originalTransactionId", J.String sharedId), ("transactionId", "2000000900000012"), ("inAppOwnershipType", "FAMILY_SHARED")] []
    sharedRenewal <- signedBy chain $ renewalPayload [("originalTransactionId", J.String sharedId)]
    ownTx <- signedBy chain $ subscriptionPayload [] []
    ownRenewal <- signedBy chain $ renewalPayload []
    answerStatus fake subscriptionId $ AppStoreBody $ statusListing [lastTransaction sharedId 1 sharedTx sharedRenewal, lastTransaction subscriptionId 1 ownTx ownRenewal]
    stateOf <$> check subscriptionId `shouldReturn` Right SSRenewing

testAppleSubscriptionCharges :: IO ()
testAppleSubscriptionCharges = do
  chain <- trustedChain
  withAppleChecker chain $ \fake check -> do
    let answerFor transaction = serveStatus fake chain 1 transaction (renewalPayload []) >> check subscriptionId
        flags = fmap (fmap (\(_, _, _, _, _, revoked, offer) -> (revoked, offer))) . chargeOf
    flags <$> answerFor (subscriptionPayload [("revocationDate", J.toJSON (1790000500000 :: Int))] []) `shouldReturn` Right (Just (True, False))
    flags <$> answerFor (subscriptionPayload [("offerType", J.toJSON (1 :: Int))] []) `shouldReturn` Right (Just (False, True))
    -- family sharing is off, so a family-shared transaction is no purchase of this badge
    outcome <$> answerFor (subscriptionPayload [("inAppOwnershipType", "FAMILY_SHARED")] []) `shouldReturn` Invalid
    -- a defence: the production host never answers about a Sandbox transaction
    outcome <$> answerFor (subscriptionPayload [("environment", "Sandbox")] []) `shouldReturn` TestPurchase
    outcome <$> answerFor (subscriptionPayload [("bundleId", "com.example.other")] []) `shouldReturn` Failed
    outcome <$> answerFor (subscriptionPayload [("originalTransactionId", "2000000900000009")] []) `shouldReturn` Failed
    outcome <$> answerFor (subscriptionPayload [] ["expiresDate"]) `shouldReturn` Failed

-- | Apple answered, and no client presented any of it, so a status that fails the chain check is this
-- service's failure: never a refusal, which would drop a buyer's keys.
testAppleSubscriptionUnsigned :: IO ()
testAppleSubscriptionUnsigned = do
  chain@TestChain {rootCert, intermediateCert} <- trustedChain
  withAppleChecker chain $ \fake check -> do
    let failsOn transaction renewal = do
          answerStatus fake subscriptionId $ AppStoreBody $ statusBody 1 transaction renewal
          outcome <$> check subscriptionId `shouldReturn` Failed
    signedRenewal <- signedBy chain $ renewalPayload []
    signedTx <- signedBy chain $ subscriptionPayload [] []
    untrusted <- newChain otherRootName [intermediateMarker] [receiptLeafMarker]
    signedBy untrusted (subscriptionPayload [] []) >>= \tx -> failsOn tx signedRenewal
    (forgedKey, forgedPub) <- newKeyPair
    forgedLeaf <- certify forgedKey intermediateName leafName forgedPub [receiptLeafMarker]
    signedTransaction chain {leafKey = forgedKey} [forgedLeaf, intermediateCert, rootCert] es256 (subscriptionPayload [] []) >>= \tx -> failsOn tx signedRenewal
    failsOn (tampered signedTx $ subscriptionPayload [("productId", "SUBSCR_BADGE_LEGEND_YEAR_01")] []) signedRenewal
    failsOn signedTx (tampered signedRenewal $ renewalPayload [("autoRenewStatus", J.toJSON (0 :: Int))])
  where
    tampered jws payload = case T.splitOn "." jws of
      [header, _, signature] -> T.intercalate "." [header, decodeUtf8 $ segment payload, signature]
      _ -> jws

-- | A verifier that cannot place what the store signed throws StoreVerifierFailure, which is its own
-- failure; any other throw is the network's, as for Play.
testSubscriptionCheckThrows :: IO ()
testSubscriptionCheckThrows = do
  let throwing act = checkStoreSubscription noStoreVerifier {checkSubscription = Just (\_ _ -> act), verifyTimeout = 2000000} (SubscriptionApple subscriptionId) Nothing
  outcome <$> throwing (throwIO $ StoreVerifierFailure "apple verifier: a chain it cannot place") `shouldReturn` Failed
  outcome <$> throwing (ioError $ userError "connection reset") `shouldReturn` Unreachable

testSubscriptionCheckCreditsTest :: IO ()
testSubscriptionCheckCreditsTest = do
  let testCheck = SubscriptionCheck {subscriptionState = SSRenewing, testSubscription = True, latestCharge = Nothing}
      checkCrediting credit = checkStoreSubscription noStoreVerifier {checkSubscription = Just (\_ _ -> pure (Right testCheck)), creditTestPurchases = credit} (SubscriptionApple subscriptionId) Nothing
  outcome <$> checkCrediting False `shouldReturn` TestPurchase
  fmap (\SubscriptionCheck {subscriptionState, testSubscription} -> (subscriptionState, testSubscription)) <$> checkCrediting True `shouldReturn` Right (SSRenewing, True)

testAppleSubscriptionHttp :: IO ()
testAppleSubscriptionHttp = do
  chain <- trustedChain
  withAppleChecker chain $ \fake check -> do
    let answeredWith answer = answerStatus fake subscriptionId answer >> (outcome <$> check subscriptionId)
    answeredWith (AppStoreStatus 401) `shouldReturn` Failed
    answeredWith (AppStoreStatus 403) `shouldReturn` Failed
    forM_ [404, 429, 500, 503] $ \code -> answeredWith (AppStoreStatus code) `shouldReturn` Unreachable
    answeredWith AppStoreHang `shouldReturn` Unreachable
    answeredWith (AppStoreBody $ J.object ["data" J..= ([] :: [J.Value])]) `shouldReturn` Unreachable
    -- an id that is not one names no App Store transaction, and is not sent
    tokens <- length <$> readTVarIO (fasTokens fake)
    outcome <$> check "2000000900000001/../x" `shouldReturn` Failed
    length <$> readTVarIO (fasTokens fake) `shouldReturn` tokens

testAppleSubscriptionKey :: IO ()
testAppleSubscriptionKey = do
  chain <- trustedChain
  withFakeAppStore ourBundleId $ \fake@FakeAppStore {fasApi = api@AppleServerApi {aPrivateKeyFile}} -> do
    serveStatus fake chain 1 (subscriptionPayload [] []) (renewalPayload [])
    -- a key App Store Connect did not issue for this account is refused, and is this service's failure
    let otherKeyFile = takeDirectory aPrivateKeyFile </> "OtherKey.p8"
    _ <- writeApiKey otherKeyFile
    otherKey <- appleChecker chain api {aPrivateKeyFile = otherKeyFile}
    outcome <$> otherKey (SubscriptionApple subscriptionId) `shouldReturn` Failed
    readTVarIO (fasTokens fake) `shouldReturn` []
    appleSubscriptionChecker (rootCert chain :| []) ourBundleId api {aPrivateKeyFile = takeDirectory aPrivateKeyFile </> "absent.p8"}
      >>= either (`shouldContain` "could not be read") (const $ expectationFailure "an absent key file must fail at boot")
    B.writeFile otherKeyFile "-----BEGIN PRIVATE KEY-----\n-----END PRIVATE KEY-----\n"
    appleSubscriptionChecker (rootCert chain :| []) ourBundleId api {aPrivateKeyFile = otherKeyFile}
      >>= either (`shouldContain` "P-256") (const $ expectationFailure "a file holding no key must fail at boot")

-- | Without the API key [apple] verifies one-time purchases as before, and no subscription.
testAppleConfiguredSubscriptions :: IO ()
testAppleConfiguredSubscriptions = do
  chain <- trustedChain
  withFakeAppStore ourBundleId $ \fake@FakeAppStore {fasApi} -> do
    let dir = takeDirectory (aPrivateKeyFile fasApi)
        rootFile = dir </> "root.cer"
        ini = dir </> "badge_service.ini"
        apple serverApi = Just AppleStoreConfig {aBundleId = ourBundleId, aRootCertificates = rootFile :| [], aServerApi = serverApi}
    B.writeFile rootFile (encodeSignedObject $ rootCert chain)
    writeFile ini "[listener]\nstatic_dir = /srv/badges\n"
    Right cfg <- readServiceConfig ini
    ServiceState {storeVerifier = withKey} <- newServiceState >>= configureStoreVerifier (Just cfg {appleStore = apple (Just fasApi)})
    ServiceState {storeVerifier = withoutKey} <- newServiceState >>= configureStoreVerifier (Just cfg {appleStore = apple Nothing})
    serveStatus fake chain 1 (subscriptionPayload [] []) (renewalPayload [])
    stateOf <$> checkStoreSubscription withKey (SubscriptionApple subscriptionId) Nothing `shouldReturn` Right SSRenewing
    outcome <$> checkStoreSubscription withoutKey (SubscriptionApple subscriptionId) Nothing `shouldReturn` NotConfigured
    jws <- signedBy chain $ transactionPayload [] []
    outcome <$> seamVerdict withoutKey SPApple {jws} `shouldReturn` Verdict

-- * Google Play

playVerifier :: FakePlay -> IO StoreVerifier
playVerifier fake =
  playStoreVerifier (fpConfig fake) >>= \case
    Right PlayVerifier {verifyPlayPurchase, acknowledgePlayPurchase, checkPlaySubscription, acknowledgePlaySubscription} ->
      pure
        noStoreVerifier
          { verifyGoogle = Just verifyPlayPurchase,
            acknowledgeGoogle = Just acknowledgePlayPurchase,
            acknowledgeGoogleSubscription = Just acknowledgePlaySubscription,
            checkSubscription = Just $ \ref _ -> case ref of
              SubscriptionGoogle token -> checkPlaySubscription token
              SubscriptionApple _ -> pure $ Left SRNotConfigured,
            verifyTimeout = 2000000
          }
    Left e -> expectationFailure e >> undefined

acknowledgement :: StoreVerifier -> ServicePayment -> IO (Maybe (Either StoreRefusal ()))
acknowledgement = acknowledgementBy $ \StoreReceipt {verifyReceipt, acknowledgeReceipt} ->
  verifyReceipt >>= \case
    Right tx -> traverse ($ tx) acknowledgeReceipt
    Left refusal -> expectationFailure ("not verified: " <> show refusal) >> undefined

subscriptionAcknowledgement :: StoreVerifier -> ServicePayment -> IO (Maybe (Either StoreRefusal ()))
subscriptionAcknowledgement = acknowledgementBy $ \StoreReceipt {acknowledgeSubscription} -> sequence acknowledgeSubscription

acknowledgementBy :: (StoreReceipt -> IO (Maybe (Either StoreRefusal ()))) -> StoreVerifier -> ServicePayment -> IO (Maybe (Either StoreRefusal ()))
acknowledgementBy acknowledge verifier payment = case toStoreReceipt verifier payment of
  Just (Right receipt) -> acknowledge receipt
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
    `shouldReturn` Right VerifiedStoreTransaction {transactionRef = googlePurchaseRef testToken, productId = "badge_supporter_01", quantity = 1, testPurchase = False, paid = Nothing, alreadyAcknowledged = False}
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
testPlayHangsOrIsGone = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answerPurchase fake testToken PlayHang
  r <- playVerdict verifier testToken
  (outcome r, reason r) `shouldBe` (Unreachable, "google verifier timed out")
  -- not a stopped stub's port, which a test running alongside may already have been given
  gone <- playVerifier fake {fpConfig = (fpConfig fake) {gApiHost = "http://127.0.0.1:1"}}
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

testPlayReadsPurchaseOnce :: IO ()
testPlayReadsPurchaseOnce = do
  requestsFor [] `shouldReturn` (1, [testToken])
  forM_ ["acknowledgementState", "consumptionState"] $ \state ->
    requestsFor [(state, J.toJSON (1 :: Int))] `shouldReturn` (1, [])
  where
    requestsFor fields = withFakePlay $ \fake -> do
      verifier <- playVerifier fake
      answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 fields
      acknowledgement verifier SPGoogle {productId = "badge_supporter_01", token = testToken} `shouldReturn` Just (Right ())
      (,) <$> (length <$> readTVarIO (fpPurchasePaths fake)) <*> readTVarIO (fpAcknowledged fake)

testPlayAcknowledgementFails :: IO ()
testPlayAcknowledgementFails = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  let acknowledgedAs expected = do
        r <- acknowledgement verifier SPGoogle {productId = "badge_supporter_01", token = testToken}
        outcome <$> r `shouldBe` Just expected
        maybe "" reason r `shouldSatisfy` ("acknowledging: " `T.isPrefixOf`)
        maybe "" reason r `shouldNotSatisfy` (testToken `T.isInfixOf`)
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 []
  answerAcknowledgements fake $ Just $ PlayStatus 403
  acknowledgedAs Failed
  answerAcknowledgements fake $ Just $ PlayStatus 503
  acknowledgedAs Unreachable
  answerAcknowledgements fake $ Just PlayHang
  acknowledgedAs Unreachable
  readTVarIO (fpAcknowledged fake) `shouldReturn` []

testPlayConfiguredAcknowledges :: IO ()
testPlayConfiguredAcknowledges = withFakePlay $ \fake@FakePlay {fpConfig} -> do
  let ini = takeDirectory (gServiceAccountFile fpConfig) </> "badge_service.ini"
  writeFile ini "[listener]\nstatic_dir = /srv/badges\n"
  Right cfg <- readServiceConfig ini
  ServiceState {storeVerifier} <- newServiceState >>= configureStoreVerifier (Just cfg {playStore = Just fpConfig})
  answerPurchase fake testToken $ PlayRecord $ purchaseRecord 0 []
  acknowledgement storeVerifier SPGoogle {productId = "badge_supporter_01", token = testToken} `shouldReturn` Just (Right ())
  readTVarIO (fpAcknowledged fake) `shouldReturn` [testToken]

-- * Google Play subscriptions

subscriptionToken :: Text
subscriptionToken = "fake-play-subscription-token.AO-J1Oz9x2kqE7wYt4"

subscriptionProductId :: Text
subscriptionProductId = "subscr_badge_supporter_01"

-- | purchases.subscriptionsv2 as Play answers it, unacknowledged.
subscriptionRecord :: Text -> [(J.Key, J.Value)] -> [J.Value] -> J.Value
subscriptionRecord state overrides items =
  J.Object $
    KM.fromList overrides
      `KM.union` KM.fromList
        [ ("kind", "androidpublisher#subscriptionPurchaseV2"),
          ("regionCode", "US"),
          ("startTime", "2026-09-14T10:00:00.123456789Z"),
          ("subscriptionState", J.String state),
          ("latestOrderId", "GPA.3312-4521-8899-01234..1"),
          ("acknowledgementState", "ACKNOWLEDGEMENT_STATE_PENDING"),
          ("lineItems", J.toJSON items)
        ]

-- | A monthly supporter plan's line item, renewing at the base price.
lineItem :: [(J.Key, J.Value)] -> [J.Key] -> J.Value
lineItem overrides removed =
  J.Object $ foldr KM.delete (KM.fromList overrides `KM.union` base) removed
  where
    base =
      KM.fromList
        [ ("productId", J.String subscriptionProductId),
          ("expiryTime", "2026-11-14T10:00:00.123456789Z"),
          ("autoRenewingPlan", J.object ["autoRenewEnabled" J..= True]),
          ("offerDetails", J.object ["basePlanId" J..= ("subscr-badge-supporter-month-02" :: Text)]),
          ("latestSuccessfulOrderId", "GPA.3312-4521-8899-01234..1"),
          ("offerPhase", J.object ["basePrice" J..= J.object []])
        ]

basePlan :: Text -> (J.Key, J.Value)
basePlan planId = ("offerDetails", J.object ["basePlanId" J..= planId])

answerSubscription :: FakePlay -> J.Value -> IO ()
answerSubscription fake = answerPurchase fake subscriptionToken . PlayRecord

-- | Through the same runner as a purchase and a renewal; every reason reaches the service's log, which must never hold the token.
playCheck :: StoreVerifier -> IO (Either StoreRefusal SubscriptionCheck)
playCheck verifier = do
  r <- checkStoreSubscription verifier (SubscriptionGoogle subscriptionToken) Nothing
  reason r `shouldNotSatisfy` (subscriptionToken `T.isInfixOf`)
  pure r

testPlaySubscriptionStates :: IO ()
testPlaySubscriptionStates = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  let stateFor state autoRenew = do
        answerSubscription fake $ subscriptionRecord state [] [lineItem [("autoRenewingPlan", J.object ["autoRenewEnabled" J..= autoRenew])] []]
        stateOf <$> playCheck verifier
  stateFor "SUBSCRIPTION_STATE_ACTIVE" True `shouldReturn` Right SSRenewing
  stateFor "SUBSCRIPTION_STATE_ACTIVE" False `shouldReturn` Right SSCancelled
  stateFor "SUBSCRIPTION_STATE_PAUSED" True `shouldReturn` Right SSRenewing
  stateFor "SUBSCRIPTION_STATE_PAUSED" False `shouldReturn` Right SSCancelled
  stateFor "SUBSCRIPTION_STATE_IN_GRACE_PERIOD" True `shouldReturn` Right SSBillingRetry
  stateFor "SUBSCRIPTION_STATE_ON_HOLD" True `shouldReturn` Right SSBillingRetry
  stateFor "SUBSCRIPTION_STATE_CANCELED" False `shouldReturn` Right SSCancelled
  stateFor "SUBSCRIPTION_STATE_EXPIRED" False `shouldReturn` Right SSEnded
  stateFor "SUBSCRIPTION_STATE_PENDING_PURCHASE_CANCELED" False `shouldReturn` Right SSEnded
  -- a plan that does not renew itself, as a prepaid one
  answerSubscription fake $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [lineItem [] ["autoRenewingPlan"]]
  stateOf <$> playCheck verifier `shouldReturn` Right SSCancelled
  paths <- readTVarIO (fpPurchasePaths fake)
  length paths `shouldBe` 10
  nub paths `shouldBe` [["androidpublisher", "v3", "applications", fakePackageName, "purchases", "subscriptionsv2", "tokens", subscriptionToken]]

testPlaySubscriptionPendingOrTest :: IO ()
testPlaySubscriptionPendingOrTest = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  answerSubscription fake $ subscriptionRecord "SUBSCRIPTION_STATE_PENDING" [] [lineItem [] []]
  outcome <$> playCheck verifier `shouldReturn` Pending
  answerSubscription fake $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [("testPurchase", J.object [])] [lineItem [] []]
  outcome <$> playCheck verifier `shouldReturn` TestPurchase

testPlaySubscriptionCharge :: IO ()
testPlaySubscriptionCharge = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  let chargeFor item = answerSubscription fake (subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [item]) >> (chargeOf <$> playCheck verifier)
      at y m d = UTCTime (fromGregorian y m d)
      orderRef = googlePurchaseRef "GPA.3312-4521-8899-01234..1"
  chargeFor (lineItem [] [])
    `shouldReturn` Right (Just (orderRef, "subscr-badge-supporter-month-02", at 2026 10 14 36000.123456789, at 2026 11 14 36000.123456789, Nothing, False, False))
  chargeFor (lineItem [("productId", "subscr_badge_legend_01"), basePlan "subscr-badge-legend-year-01", ("expiryTime", "2027-10-14T10:00:00Z")] [])
    `shouldReturn` Right (Just (orderRef, "subscr-badge-legend-year-01", at 2026 10 14 36000, at 2027 10 14 36000, Nothing, False, False))
  -- a plan this service does not sell has no length, so its charge is given no period, and is never granted
  chargeFor (lineItem [basePlan "subscr-badge-supporter-month-99"] [])
    `shouldReturn` Right (Just (orderRef, "subscr-badge-supporter-month-99", at 2026 11 14 36000.123456789, at 2026 11 14 36000.123456789, Nothing, False, False))
  chargeFor (lineItem [] ["latestSuccessfulOrderId"]) `shouldReturn` Right Nothing

testPlaySubscriptionOffer :: IO ()
testPlaySubscriptionOffer = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  let offerFor item = do
        answerSubscription fake $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [item]
        fmap (fmap (\(_, _, _, _, _, _, offer) -> offer)) . chargeOf <$> playCheck verifier
      offerIn phase = offerFor $ lineItem [offerDetails, ("offerPhase", J.object [phase J..= J.object []])] []
      offerDetails = ("offerDetails", J.object ["basePlanId" J..= ("subscr-badge-supporter-month-02" :: Text), "offerId" J..= ("intro-3-months" :: Text)])
  offerIn "freeTrial" `shouldReturn` Right (Just True)
  offerIn "introductoryPrice" `shouldReturn` Right (Just True)
  -- offerDetails names the offer it was bought with for its whole life, so a full-price renewal still carries it
  offerIn "basePrice" `shouldReturn` Right (Just False)
  offerIn "prorationPeriod" `shouldReturn` Right (Just False)
  offerFor (lineItem [] ["offerPhase"]) `shouldReturn` Right (Just False)

testPlaySubscriptionUnreadable :: IO ()
testPlaySubscriptionUnreadable = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  forM_
    [ PlayBody "<html>not json</html>",
      PlayRecord $ J.object ["lineItems" J..= [lineItem [] []]],
      PlayRecord $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [],
      PlayRecord $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [lineItem [] [], lineItem [] []],
      PlayRecord $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [lineItem [] ["expiryTime"]],
      PlayRecord $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [lineItem [] ["offerDetails"]],
      PlayRecord $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [lineItem [("expiryTime", "next month")] []],
      PlayBody $ J.encode (subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [lineItem [] []]) <> LB.replicate (1024 * 1024) 32
    ]
    $ \answer -> do
      answerPurchase fake subscriptionToken answer
      outcome <$> playCheck verifier `shouldReturn` Failed
  -- a state Play may yet add is this service's to learn, and its name is not logged
  answerSubscription fake $ subscriptionRecord "SUBSCRIPTION_STATE_PAUSED_FOREVER" [] [lineItem [] []]
  r <- playCheck verifier
  outcome r `shouldBe` Failed
  reason r `shouldNotSatisfy` ("PAUSED_FOREVER" `T.isInfixOf`)

testPlaySubscriptionHttp :: IO ()
testPlaySubscriptionHttp = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  let answeredWith answer = answerPurchase fake subscriptionToken answer >> (outcome <$> playCheck verifier)
  answeredWith (PlayStatus 401) `shouldReturn` Failed
  answeredWith (PlayStatus 403) `shouldReturn` Failed
  forM_ [400, 404, 429, 500, 503] $ \code -> answeredWith (PlayStatus code) `shouldReturn` Unreachable
  -- Play no longer answers about a subscription that expired long ago
  answerPurchase fake subscriptionToken (PlayStatus 410)
  ended <- playCheck verifier
  (stateOf ended, chargeOf ended) `shouldBe` (Right SSEnded, Right Nothing)
  answerPurchase fake subscriptionToken PlayHang
  r <- playCheck verifier
  (outcome r, reason r) `shouldBe` (Unreachable, "subscription check timed out")

testPlaySubscriptionAcknowledges :: IO ()
testPlaySubscriptionAcknowledges = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  let acknowledging = subscriptionAcknowledgement verifier SPGoogle {productId = subscriptionProductId, token = subscriptionToken}
  answerSubscription fake $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [lineItem [] []]
  acknowledging `shouldReturn` Just (Right ())
  readTVarIO (fpSubscriptionAcknowledged fake) `shouldReturn` [(subscriptionProductId, subscriptionToken)]
  answerSubscription fake $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [("acknowledgementState", "ACKNOWLEDGEMENT_STATE_ACKNOWLEDGED")] [lineItem [] []]
  acknowledging `shouldReturn` Just (Right ())
  readTVarIO (fpSubscriptionAcknowledged fake) `shouldReturn` [(subscriptionProductId, subscriptionToken)]
  readTVarIO (fpAcknowledged fake) `shouldReturn` []

testPlaySubscriptionAcknowledgementFails :: IO ()
testPlaySubscriptionAcknowledgementFails = withFakePlay $ \fake -> do
  verifier <- playVerifier fake
  let acknowledgedAs expected = do
        r <- subscriptionAcknowledgement verifier SPGoogle {productId = subscriptionProductId, token = subscriptionToken}
        outcome <$> r `shouldBe` Just expected
        maybe "" reason r `shouldSatisfy` ("acknowledging: " `T.isPrefixOf`)
        maybe "" reason r `shouldNotSatisfy` (subscriptionToken `T.isInfixOf`)
  answerSubscription fake $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [lineItem [] []]
  answerAcknowledgements fake $ Just $ PlayStatus 403
  acknowledgedAs Failed
  answerAcknowledgements fake $ Just $ PlayStatus 503
  acknowledgedAs Unreachable
  answerAcknowledgements fake Nothing
  answerPurchase fake subscriptionToken PlayHang
  acknowledgedAs Unreachable
  readTVarIO (fpSubscriptionAcknowledged fake) `shouldReturn` []

-- | Without [google] a Play subscription is not configured, whether or not [apple] checks the App Store's.
testPlayConfiguredSubscriptions :: IO ()
testPlayConfiguredSubscriptions = do
  chain <- trustedChain
  withFakePlay $ \play@FakePlay {fpConfig} -> withFakeAppStore ourBundleId $ \appStore@FakeAppStore {fasApi} -> do
    let dir = takeDirectory (gServiceAccountFile fpConfig)
        rootFile = dir </> "root.cer"
        ini = dir </> "badge_service.ini"
        apple = Just AppleStoreConfig {aBundleId = ourBundleId, aRootCertificates = rootFile :| [], aServerApi = Just fasApi}
        configured c = (\ServiceState {storeVerifier} -> storeVerifier) <$> (newServiceState >>= configureStoreVerifier (Just c))
        outcomes v = (,) <$> (outcome <$> checkStoreSubscription v (SubscriptionGoogle subscriptionToken) Nothing) <*> (outcome <$> checkStoreSubscription v (SubscriptionApple subscriptionId) Nothing)
        acknowledging v = subscriptionAcknowledgement v SPGoogle {productId = subscriptionProductId, token = subscriptionToken}
    B.writeFile rootFile (encodeSignedObject $ rootCert chain)
    writeFile ini "[listener]\nstatic_dir = /srv/badges\n"
    Right cfg <- readServiceConfig ini
    both <- configured cfg {appleStore = apple, playStore = Just fpConfig}
    playOnly <- configured cfg {playStore = Just fpConfig}
    appleOnly <- configured cfg {appleStore = apple}
    answerSubscription play $ subscriptionRecord "SUBSCRIPTION_STATE_ACTIVE" [] [lineItem [] []]
    serveStatus appStore chain 1 (subscriptionPayload [] []) (renewalPayload [])
    outcomes both `shouldReturn` (Verdict, Verdict)
    outcomes playOnly `shouldReturn` (Verdict, NotConfigured)
    outcomes appleOnly `shouldReturn` (NotConfigured, Verdict)
    acknowledging appleOnly `shouldReturn` Nothing
    acknowledging playOnly `shouldReturn` Just (Right ())
    readTVarIO (fpSubscriptionAcknowledged play) `shouldReturn` [(subscriptionProductId, subscriptionToken)]

