{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BadgeTests (badgeTests) where

import Control.Concurrent.STM (atomically)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JM
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Chat.Badges
import qualified Simplex.Chat.Badges as CB
import Simplex.Chat.Badges.Service
import Simplex.Chat.Badges.Types
-- PaymentStatus is hidden: its PSFailed collides with BadgePurchaseStatus's, and the column
-- spellings of both are pinned below. The payment statuses are reached through PT instead.
import Simplex.Chat.PaymentService hiding (PaymentStatus (..))
import qualified Simplex.Chat.PaymentService.Types as PT
import Simplex.Messaging.Crypto.BBS
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import Test.Hspec

badgeTests :: Spec
badgeTests = do
  it "full workflow: request, issue, verify credential, generate and verify proof" testFullWorkflow
  it "should reject badge with tampered type" testTamperedType
  it "should reject badge with tampered expiry" testTamperedExpiry
  it "should reject badge with wrong server key" testWrongKey
  it "should report a key index missing from configured keys" testUnknownKeyIdx
  it "should compute badge status correctly" testExpiryCheck
  it "should treat lifetime badges as always active" testLifetimeBadge
  it "should accept unknown badge types" testUnknownBadgeType
  it "credential serializes to a paste-able token and back" testCredentialSerialization
  it "round-trips BadgeItemStatus JSON with the documented spellings" testBadgeItemStatusJSON
  it "round-trips CardProvider and CryptoCurrency JSON" testCardCryptoJSON
  it "round-trips BadgePurchaseStatus, InvoiceStatus and PaymentStatus DB column spellings" testBadgeStatusColumnSpelling
  it "round-trips OfferDiscount and service payment method/destination JSON" testOfferDiscountAndPaymentMethodJSON
  it "round-trips every ServicePayment constructor" testServicePaymentJSON
  it "round-trips every known StatementCreditType constructor" testStatementCreditTypeJSON
  it "decodes an unrecognised ledgerCredit tag into SCUnknown and re-encodes it byte-identically" testStatementCreditUnknownTag
  it "round-trips every known StatementDebitType constructor" testStatementDebitTypeJSON
  it "decodes an unrecognised ledgerDebit tag into SDUnknown and re-encodes it byte-identically" testStatementDebitUnknownTag
  it "round-trips StatementEntry, BadgeBalance and BadgeStatement JSON" testStatementEntryJSON
  it "round-trips BadgePrice, BadgeOffer and BadgeCatalog JSON" testCatalogJSON
  it "round-trips BadgeUpgrade and ServiceInvoice JSON" testBadgeUpgradeAndInvoiceJSON
  it "round-trips every BadgeServiceCommand constructor" testBadgeServiceCommandJSON
  it "round-trips every BadgeServiceResponse constructor" testBadgeServiceResponseJSON
  it "round-trips BadgeServiceRequest JSON" testBadgeServiceRequestJSON

proofOf :: BadgeProof -> BBSProof
proofOf (BadgeProof _ _ p _) = p

proofInfo :: BadgeProof -> BadgeInfo
proofInfo (BadgeProof _ _ _ i) = i

testKeyIdx :: Int
testKeyIdx = 1

keysFor :: BBSPublicKey -> Map Int BBSPublicKey
keysFor = M.singleton testKeyIdx

testFullWorkflow :: IO ()
testFullWorkflow = do
  Right (pk, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let req = BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = Just futureTime, badgeExtra = ""}}
  Just vreq <- verifyPayment (BPRedeemCode "TEST") req
  Right cred <- issueBadge testKeyIdx sk vreq
  let BadgeCredential idx mk' _ _ = cred
  idx `shouldBe` testKeyIdx
  mk' `shouldBe` mk
  verifyCredential pk cred >>= (`shouldBe` True)
  Right badge <- generateBadgeProof pk cred (BBSPresHeader "nonce-1")
  -- the proof inherits the credential's key index, so receivers find the right key
  let BadgeProof {badgeKeyIdx} = badge
  badgeKeyIdx `shouldBe` testKeyIdx
  verifyBadge (keysFor pk) badge >>= (`shouldBe` Just True)
  Right badge2 <- generateBadgeProof pk cred (BBSPresHeader "nonce-2")
  verifyBadge (keysFor pk) badge2 >>= (`shouldBe` Just True)
  proofOf badge `shouldNotBe` proofOf badge2

testTamperedType :: IO ()
testTamperedType = do
  (pk, BadgeProof idx ph p info) <- issueBadgeProof BTSupporter (Just futureTime)
  verifyBadge (keysFor pk) (BadgeProof idx ph p info {CB.badgeType = BTLegend}) >>= (`shouldBe` Just False)

testTamperedExpiry :: IO ()
testTamperedExpiry = do
  (pk, BadgeProof idx ph p info) <- issueBadgeProof BTSupporter (Just futureTime)
  verifyBadge (keysFor pk) (BadgeProof idx ph p info {CB.badgeExpiry = Just pastTime}) >>= (`shouldBe` Just False)

testWrongKey :: IO ()
testWrongKey = do
  (_, badge) <- issueBadgeProof BTSupporter (Just futureTime)
  Right (pk2, _) <- bbsKeyGen
  verifyBadge (keysFor pk2) badge >>= (`shouldBe` Just False)

testUnknownKeyIdx :: IO ()
testUnknownKeyIdx = do
  (pk, badge) <- issueBadgeProof BTSupporter (Just futureTime)
  -- a key index not in the configured keys cannot be verified at all (Nothing)
  verifyBadge (M.singleton (testKeyIdx + 1) pk) badge >>= (`shouldBe` Nothing)

testExpiryCheck :: IO ()
testExpiryCheck = do
  now <- getCurrentTime
  let info expiry = BadgeInfo {badgeType = BTSupporter, badgeExpiry = expiry, badgeExtra = ""}
      futureInfo = info (Just futureTime)
  mkBadgeStatus now (Just True) futureInfo `shouldBe` BSActive
  mkBadgeStatus now (Just True) (info (Just (addUTCTime (-nominalDay) now))) `shouldBe` BSExpired
  mkBadgeStatus now (Just True) (info (Just pastTime)) `shouldBe` BSExpiredOld
  mkBadgeStatus now (Just False) futureInfo `shouldBe` BSFailed
  mkBadgeStatus now Nothing futureInfo `shouldBe` BSUnknownKey

testLifetimeBadge :: IO ()
testLifetimeBadge = do
  now <- getCurrentTime
  (pk, badge) <- issueBadgeProof BTInvestor Nothing
  verifyBadge (keysFor pk) badge >>= (`shouldBe` Just True)
  mkBadgeStatus now (Just True) (proofInfo badge) `shouldBe` BSActive

testUnknownBadgeType :: IO ()
testUnknownBadgeType = do
  (pk, badge) <- issueBadgeProof (BTUnknown "future_type") (Just futureTime)
  verifyBadge (keysFor pk) badge >>= (`shouldBe` Just True)

testCredentialSerialization :: IO ()
testCredentialSerialization = do
  Right (pk, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let mkCred expiry = do
        Right cred <- issueBadge testKeyIdx sk (VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = expiry, badgeExtra = ""}})
        pure cred
  dated <- mkCred (Just futureTime)
  lifetime <- mkCred Nothing
  J.eitherDecode (J.encode dated) `shouldBe` Right dated
  J.eitherDecode (J.encode lifetime) `shouldBe` Right lifetime
  -- a decoded credential still verifies against the issuing key
  case J.eitherDecode (J.encode dated) of
    Right cred -> verifyCredential pk cred >>= (`shouldBe` True)
    Left e -> expectationFailure e

-- Badge protocol JSON (A2)

-- encode, decode, re-encode: checks both that FromJSON accepts what ToJSON produces and that the
-- re-encoding is byte-identical to the original (the specific property SCUnknown/SDUnknown need)
roundtripsBytes :: forall a. (ToJSON a, FromJSON a) => a -> Expectation
roundtripsBytes v = case J.eitherDecode bytes :: Either String a of
  Left e -> expectationFailure e
  Right v' -> J.encode v' `shouldBe` bytes
  where
    bytes = J.encode v

mkKeyPair :: IO (C.PublicKeyEd25519, C.PrivateKeyEd25519)
mkKeyPair = do
  drg <- C.newRandom
  (pub, priv :: C.PrivateKeyEd25519) <- atomically $ C.generateKeyPair drg
  pure (pub, priv)

jsonBadgeInfo :: BadgeInfo
jsonBadgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = Just futureTime, badgeExtra = ""}

jsonCredential :: IO BadgeCredential
jsonCredential = do
  Right (_, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  Right cred <- issueBadge testKeyIdx sk (VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeInfo = jsonBadgeInfo})
  pure cred

sampleEntry :: IO StatementEntry
sampleEntry = do
  now <- getCurrentTime
  pure
    StatementEntry
      { entryId = "entry-1",
        changeMonths = 1,
        balanceMonths = 12,
        balanceStartTs = now,
        balanceBadgeType = BTSupporter,
        wasPausedSince = Nothing,
        createdAt = now,
        entryType = SECredit {credit = SCOpening}
      }

sampleBalance :: IO BadgeBalance
sampleBalance = BadgeBalance <$> sampleEntry

samplePaymentDestination :: ServicePaymentDestination
samplePaymentDestination = SPDCard {provider = CPStripe, url = "https://pay.example/session"}

testBadgeItemStatusJSON :: IO ()
testBadgeItemStatusJSON = do
  mapM_ roundtripsBytes [BISActive, BISDeprecated, BISDisabled]
  J.eitherDecode (J.encode BISActive) `shouldBe` Right ("active" :: Text)
  J.eitherDecode (J.encode BISDeprecated) `shouldBe` Right ("deprecated" :: Text)
  J.eitherDecode (J.encode BISDisabled) `shouldBe` Right ("disabled" :: Text)

testCardCryptoJSON :: IO ()
testCardCryptoJSON = do
  roundtripsBytes CPStripe
  mapM_ roundtripsBytes [CCBtc, CCXmr]
  J.eitherDecode (J.encode CPStripe) `shouldBe` Right ("stripe" :: Text)
  J.eitherDecode (J.encode CCBtc) `shouldBe` Right ("btc" :: Text)
  J.eitherDecode (J.encode CCXmr) `shouldBe` Right ("xmr" :: Text)

testBadgeStatusColumnSpelling :: IO ()
testBadgeStatusColumnSpelling = do
  mapM_ roundtripsBytes [PSAcquiring, PSIssued, PSSuperseded, PSFailed]
  J.eitherDecode (J.encode PSAcquiring) `shouldBe` Right ("acquiring" :: Text)
  J.eitherDecode (J.encode PSIssued) `shouldBe` Right ("issued" :: Text)
  J.eitherDecode (J.encode PSSuperseded) `shouldBe` Right ("superseded" :: Text)
  J.eitherDecode (J.encode PSFailed) `shouldBe` Right ("failed" :: Text)
  -- invoices.status and payments.status have no JSON at all: they never cross the wire, so
  -- TextEncoding is their whole contract and it is asserted directly.
  map textEncode [ISOpen, ISPaid, ISExpired] `shouldBe` ["open", "paid", "expired"]
  map textDecode ["open", "paid", "expired", "settled"]
    `shouldBe` [Just ISOpen, Just ISPaid, Just ISExpired, Nothing]
  map textEncode [PT.PSPending, PT.PSSettled, PT.PSFailed "card declined"]
    `shouldBe` ["pending", "settled", "failed"]
  -- PaymentStatus has no Eq, and textDecode cannot recover the failure text -- it is
  -- payments.exception, a column of its own -- so the spelling is what is compared back.
  map (fmap textEncode . paymentStatus) ["pending", "settled", "failed", "new"]
    `shouldBe` [Just "pending", Just "settled", Just "failed", Nothing]
  where
    paymentStatus :: Text -> Maybe PT.PaymentStatus
    paymentStatus = textDecode

testOfferDiscountAndPaymentMethodJSON :: IO ()
testOfferDiscountAndPaymentMethodJSON = do
  roundtripsBytes (ODFreeMonths {freeMonths = 3})
  roundtripsBytes (ODDiscount {discount = 20})
  roundtripsBytes (SPMCard {provider = CPStripe})
  roundtripsBytes (SPMCrypto {currency = CCBtc})
  roundtripsBytes samplePaymentDestination
  roundtripsBytes (SPDCrypto {currency = CCXmr, address = "4Axxxxxxxxxxxxxxxxxxxxxxxxxxxxx", cryptoAmount = "0.5"})

testServicePaymentJSON :: IO ()
testServicePaymentJSON = do
  roundtripsBytes (SPApple {jws = "jws-token"})
  roundtripsBytes (SPGoogle {token = "google-token"})
  roundtripsBytes (SPInvoice {invoiceId = InvoiceId "inv-1"})
  roundtripsBytes (SPCode {code = "CODE123"})
  roundtripsBytes (SPReceipt {receipt = "receipt-blob"})

testStatementCreditTypeJSON :: IO ()
testStatementCreditTypeJSON = do
  roundtripsBytes (SCPayment {invoiceId = Just (InvoiceId "inv-1")})
  roundtripsBytes (SCPayment {invoiceId = Nothing})
  roundtripsBytes (SCCharge {chargeId = "charge-1"})
  roundtripsBytes SCSupport
  (pub, _) <- mkKeyPair
  roundtripsBytes (SCTransferIn {fromPurchaseKey = pub})
  roundtripsBytes SCOpening
  -- ruling 1: chargeId must serialize as a JSON string (schema: "type": "string"), not a number
  case J.toJSON (SCCharge {chargeId = "c1"}) of
    J.Object o -> JM.lookup "chargeId" o `shouldBe` Just (J.String "c1")
    v -> expectationFailure ("expected a JSON object, got " <> show v)

testStatementCreditUnknownTag :: IO ()
testStatementCreditUnknownTag = do
  let bytes = LB.pack "{\"amount\":42,\"type\":\"futureThing\"}"
  case J.eitherDecode bytes :: Either String StatementCreditType of
    Left e -> expectationFailure e
    Right v@(SCUnknown {tag}) -> do
      tag `shouldBe` "futureThing"
      J.encode v `shouldBe` bytes
    Right other -> expectationFailure ("expected SCUnknown, got " <> show other)

testStatementDebitTypeJSON :: IO ()
testStatementDebitTypeJSON = do
  roundtripsBytes SDRefund
  (pub, _) <- mkKeyPair
  roundtripsBytes (SDUpgrade {toPurchaseKey = pub})
  roundtripsBytes (SDTransferOut {toPurchaseKey = pub})
  roundtripsBytes SDSupport
  roundtripsBytes SDBadge
  roundtripsBytes SDLapse

testStatementDebitUnknownTag :: IO ()
testStatementDebitUnknownTag = do
  let bytes = LB.pack "{\"amount\":7,\"type\":\"somethingNew\"}"
  case J.eitherDecode bytes :: Either String StatementDebitType of
    Left e -> expectationFailure e
    Right v@(SDUnknown {tag}) -> do
      tag `shouldBe` "somethingNew"
      J.encode v `shouldBe` bytes
    Right other -> expectationFailure ("expected SDUnknown, got " <> show other)

testStatementEntryJSON :: IO ()
testStatementEntryJSON = do
  roundtripsBytes (SECredit {credit = SCOpening})
  roundtripsBytes (SEDebit {debit = SDRefund})
  entry <- sampleEntry
  roundtripsBytes entry
  roundtripsBytes (BadgeBalance {lastEntry = entry})
  roundtripsBytes (BadgeStatement {entries = [entry], previousEntryId = Just "entry-0"})
  roundtripsBytes (BadgeStatement {entries = [entry], previousEntryId = Nothing})

testCatalogJSON :: IO ()
testCatalogJSON = do
  now <- getCurrentTime
  let price =
        BadgePrice
          { priceId = BadgePriceId "price-1",
            badgeType = BTSupporter,
            monthPrice = CurrencyAmount 500,
            currency = "usd",
            status = BISActive,
            createdAt = now
          }
      offer =
        BadgeOffer
          { offerId = BadgeOfferId "offer-1",
            priceId = Just (BadgePriceId "price-1"),
            months = 12,
            discount = ODDiscount {discount = 10},
            status = BISActive,
            createdAt = now,
            total = Just (CurrencyAmount 5400)
          }
  roundtripsBytes price
  roundtripsBytes offer
  roundtripsBytes (offer {total = Nothing}) -- absent until catalogTotals fills it in (A4)
  roundtripsBytes (BadgeCatalog {prices = [price], offers = [offer]})

testBadgeUpgradeAndInvoiceJSON :: IO ()
testBadgeUpgradeAndInvoiceJSON = do
  now <- getCurrentTime
  balance <- sampleBalance
  (pub, sk) <- mkKeyPair
  let upgrade =
        BadgeUpgrade
          { fromPurchaseKey = pub,
            receipt = "receipt-text",
            receiptSignature = C.sign' sk "receipt-text",
            balance
          }
  roundtripsBytes upgrade
  let invoice =
        ServiceInvoice
          { invoiceId = InvoiceId "inv-1",
            price = CurrencyAmount 1000,
            discount = Just (CurrencyAmount 100),
            credit = Nothing,
            amount = CurrencyAmount 900,
            currency = "usd",
            expiresAt = now,
            paymentTo = samplePaymentDestination
          }
  roundtripsBytes invoice

testBadgeServiceCommandJSON :: IO ()
testBadgeServiceCommandJSON = do
  balance <- sampleBalance
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let badgeRequest = BadgeRequest {masterKey = mk, badgeInfo = jsonBadgeInfo}
  (pub, sk) <- mkKeyPair
  let upgrade =
        BadgeUpgrade
          { fromPurchaseKey = pub,
            receipt = "r",
            receiptSignature = C.sign' sk "r",
            balance
          }
  roundtripsBytes BSCGetBadgeCatalog
  roundtripsBytes
    BSCGetBadgeInvoice
      { priceId = BadgePriceId "price-1",
        offerId = Just (BadgeOfferId "offer-1"),
        badgeInfo = jsonBadgeInfo,
        paymentVia = SPMCard {provider = CPStripe},
        upgrade = Just upgrade
      }
  roundtripsBytes BSCPurchaseBadge {badgeRequest, payment = SPCode {code = "CODE"}, upgrade = Nothing}
  roundtripsBytes BSCUpgradeBadgeSubscription {badgeRequest, payment = SPInvoice {invoiceId = InvoiceId "inv-1"}, balance}
  roundtripsBytes BSCIssueBadge {badgeRequest, balance}
  roundtripsBytes BSCPauseBadge

testBadgeServiceResponseJSON :: IO ()
testBadgeServiceResponseJSON = do
  now <- getCurrentTime
  entry <- sampleEntry
  cred <- jsonCredential
  let statement = BadgeStatement {entries = [entry], previousEntryId = Nothing}
      catalog = BadgeCatalog {prices = [], offers = []}
      invoice =
        ServiceInvoice
          { invoiceId = InvoiceId "inv-1",
            price = CurrencyAmount 1000,
            discount = Nothing,
            credit = Nothing,
            amount = CurrencyAmount 1000,
            currency = "usd",
            expiresAt = now,
            paymentTo = samplePaymentDestination
          }
  roundtripsBytes (BSPBadgeCatalog {catalog, badgeStatement = Just statement})
  roundtripsBytes (BSPBadgeCatalog {catalog, badgeStatement = Nothing})
  roundtripsBytes (BSPBadgeInvoice {invoice, badgeType = BTSupporter, months = 12})
  roundtripsBytes (BSPBadgeCredential {credential = Just cred, receipt = Just "r", statement})
  roundtripsBytes (BSPBadgeCredential {credential = Nothing, receipt = Nothing, statement})
  roundtripsBytes (BSPError {code = BSEBadRequest, message = Just "bad request", retryAfter = Just 30})
  roundtripsBytes (BSPError {code = BSEUnknown "future_error", message = Nothing, retryAfter = Nothing})

testBadgeServiceRequestJSON :: IO ()
testBadgeServiceRequestJSON = do
  (pub, _) <- mkKeyPair
  roundtripsBytes (BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Just pub, request = BSCPauseBadge})
  roundtripsBytes (BadgeServiceRequest {version = VersionBadgeService 1, purchaseKey = Nothing, request = BSCGetBadgeCatalog})

-- Helpers

futureTime :: UTCTime
futureTime = posixSecondsToUTCTime 4102444800 -- 2099-12-31

pastTime :: UTCTime
pastTime = posixSecondsToUTCTime 1577836800 -- 2020-01-01

issueBadgeProof :: BadgeType -> Maybe UTCTime -> IO (BBSPublicKey, BadgeProof)
issueBadgeProof bt expiry = do
  Right (pk, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let vreq = VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = bt, badgeExpiry = expiry, badgeExtra = ""}}
  Right cred <- issueBadge testKeyIdx sk vreq
  Right badge <- generateBadgeProof pk cred (BBSPresHeader "test-nonce")
  pure (pk, badge)
