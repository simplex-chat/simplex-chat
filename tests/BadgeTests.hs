{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

module BadgeTests (badgeTests) where

import Control.Concurrent.STM (atomically)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Aeson as J
import qualified Simplex.Messaging.Crypto as C
import Simplex.Chat.Badges
import Simplex.Chat.Badges.Code
import Simplex.Chat.Badges.Service
import Simplex.Messaging.Crypto.BBS
import Simplex.Messaging.Version.Internal (Version (..))
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
  describe "redemption codes" $ do
    it "a generated code reads back" testCodeRoundTrip
    it "reads a code as typed - any case, separators, ambiguous characters" testCodeNormalisation
    it "rejects a code whose check character does not match" testCodeCheckCharacter
    it "hashes the canonical form, whatever was typed" testCodeHash
  describe "service protocol JSON" $ do
    it "redeemBadgeCode request matches the schema" testRedeemRequestJSON
    it "badgeCredential response matches the schema" testCredentialResponseJSON
    it "error response matches the schema" testErrorResponseJSON
    it "statement entries round-trip, unknown entry types verbatim" testStatementJSON

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
  verifyBadge (keysFor pk) (BadgeProof idx ph p info {badgeType = BTLegend}) >>= (`shouldBe` Just False)

testTamperedExpiry :: IO ()
testTamperedExpiry = do
  (pk, BadgeProof idx ph p info) <- issueBadgeProof BTSupporter (Just futureTime)
  verifyBadge (keysFor pk) (BadgeProof idx ph p info {badgeExpiry = Just pastTime}) >>= (`shouldBe` Just False)

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

-- Redemption codes

testCodeRoundTrip :: IO ()
testCodeRoundTrip = do
  drg <- C.newRandom
  code <- randomBadgeCode drg
  let formatted = formatBadgeCode code
  T.length formatted `shouldBe` 27 -- SXB-XXXXX-XXXXX-XXXXX-XXXXX
  T.take 4 formatted `shouldBe` "SXB-"
  T.length (badgeCodeText code) `shouldBe` 23 -- the canonical form drops the separators
  parseBadgeCode formatted `shouldBe` Just code
  parseBadgeCode (badgeCodeText code) `shouldBe` Just code

testCodeNormalisation :: IO ()
testCodeNormalisation = do
  drg <- C.newRandom
  code <- randomBadgeCode drg
  parseBadgeCode (T.toLower $ badgeCodeText code) `shouldBe` Just code
  parseBadgeCode (T.replace "-" " " $ formatBadgeCode code) `shouldBe` Just code
  -- a fixed code, because a random one contains no 0 or 1 about a quarter of the time and the
  -- folding would then be asserted against nothing
  let folded = T.map ambiguous fixedCode
  folded `shouldNotBe` fixedCode
  parseBadgeCode folded `shouldBe` parseBadgeCode fixedCode
  parseBadgeCode fixedCode `shouldNotBe` Nothing
  where
    fixedCode = "SXB-0C0QS-XAQW1-N1VSA-R00Y3"
    ambiguous = \case
      '1' -> 'I'
      '0' -> 'O'
      c -> c

testCodeCheckCharacter :: IO ()
testCodeCheckCharacter = do
  drg <- C.newRandom
  code <- randomBadgeCode drg
  let canonical = badgeCodeText code
      -- every other value for the last character fails the check
      wrong = T.init canonical <> T.singleton (if T.last canonical == 'Z' then 'Y' else 'Z')
  parseBadgeCode wrong `shouldBe` Nothing
  parseBadgeCode "" `shouldBe` Nothing
  parseBadgeCode "SXB-00000-00000-00000-0000" `shouldBe` Nothing
  parseBadgeCode (T.drop 3 canonical) `shouldBe` Nothing

testCodeHash :: IO ()
testCodeHash = do
  drg <- C.newRandom
  code <- randomBadgeCode drg
  Just typed <- pure $ parseBadgeCode $ T.toLower $ formatBadgeCode code
  badgeCodeHash typed `shouldBe` badgeCodeHash code

-- Service protocol JSON, against docs/protocol/badges-rpc.schema.json

testRedeemRequestJSON :: IO ()
testRedeemRequestJSON = do
  drg <- C.newRandom
  mk <- generateMasterKey drg
  (k, _) <- atomically $ C.generateKeyPair drg :: IO (C.KeyPair 'C.Ed25519)
  code <- randomBadgeCode drg
  let req = BadgeServiceRequest {version = Version 1, purchaseKey = Just k, request = BSCRedeemBadgeCode {masterKey = mk, code = badgeCodeText code}}
  J.toJSON req
    `shouldBe` J.object
      [ "version" J..= (1 :: Int),
        "purchaseKey" J..= k,
        "request" J..= J.object ["type" J..= ("redeemBadgeCode" :: T.Text), "masterKey" J..= mk, "code" J..= badgeCodeText code]
      ]
  -- purchaseKey is optional in the schema, and a nullary command is a bare tagged object
  J.toJSON BadgeServiceRequest {version = Version 1, purchaseKey = Nothing, request = BSCGetBadgeCatalog}
    `shouldBe` J.object ["version" J..= (1 :: Int), "request" J..= J.object ["type" J..= ("getBadgeCatalog" :: T.Text)]]
  roundTrips req

testCredentialResponseJSON :: IO ()
testCredentialResponseJSON = do
  Right (_, sk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let info = BadgeInfo {badgeType = BTSupporter, badgeExpiry = Just futureTime, badgeExtra = ""}
  Right cred <- issueBadge testKeyIdx sk (VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeInfo = info})
  let resp = BSPBadgeCredential {credential = Just cred, receipt = Nothing, statement = BadgeStatement {entries = [], previousEntryId = Nothing}}
  J.toJSON resp
    `shouldBe` J.object
      [ "type" J..= ("badgeCredential" :: T.Text),
        "credential" J..= cred,
        "statement" J..= J.object ["entries" J..= ([] :: [J.Value])]
      ]
  roundTrips resp

testErrorResponseJSON :: IO ()
testErrorResponseJSON = do
  let resp = BSPError {code = BSECodeInvalid, message = Nothing, retryAfter = Nothing}
  J.toJSON resp `shouldBe` J.object ["type" J..= ("error" :: T.Text), "code" J..= ("code_invalid" :: T.Text)]
  J.toJSON BSPError {code = BSERateLimited, message = Just "slow down", retryAfter = Just 30}
    `shouldBe` J.object ["type" J..= ("error" :: T.Text), "code" J..= ("rate_limited" :: T.Text), "message" J..= ("slow down" :: T.Text), "retryAfter" J..= (30 :: Int)]

testStatementJSON :: IO ()
testStatementJSON = do
  let entry =
        StatementEntry
          { entryId = "e1",
            changeMonths = 3,
            balanceMonths = 3,
            balanceStartTs = futureTime,
            balanceBadgeType = BTSupporter,
            wasPausedSince = Nothing,
            createdAt = futureTime,
            entryType = SECredit {credit = SCPayment {invoiceId = Nothing}}
          }
  -- the whole entry: the required fields, and wasPausedSince omitted rather than sent as null
  J.toJSON entry
    `shouldBe` J.object
      [ "entryId" J..= ("e1" :: T.Text),
        "changeMonths" J..= (3 :: Int),
        "balanceMonths" J..= (3 :: Int),
        "balanceStartTs" J..= futureTime,
        "balanceBadgeType" J..= ("supporter" :: T.Text),
        "createdAt" J..= futureTime,
        "entryType" J..= entryType entry
      ]
  J.toJSON entry {wasPausedSince = Just pastTime} `shouldNotBe` J.toJSON entry
  J.toJSON (entryType entry) `shouldBe` J.object ["type" J..= ("credit" :: T.Text), "credit" J..= J.object ["type" J..= ("payment" :: T.Text)]]
  J.toJSON SEDebit {debit = SDBadge} `shouldBe` J.object ["type" J..= ("debit" :: T.Text), "debit" J..= J.object ["type" J..= ("badge" :: T.Text)]]
  -- an entry type from a newer service is stored and re-emitted unchanged
  let futureCredit = J.object ["type" J..= ("grant" :: T.Text), "grantedBy" J..= ("operator" :: T.Text)]
  case J.fromJSON futureCredit of
    J.Success c@SCUnknown {tag} -> do
      tag `shouldBe` "grant"
      J.toJSON c `shouldBe` futureCredit
    r -> expectationFailure $ "expected SCUnknown, got " <> show (fmap (const ()) r)

-- decoding and re-encoding reproduces the encoding, without Eq on the protocol types
roundTrips :: (HasCallStack, J.ToJSON a, J.FromJSON a) => a -> IO ()
roundTrips x = case J.eitherDecode (J.encode x) of
  Right x' -> J.toJSON (x' `asTypeOf` x) `shouldBe` J.toJSON x
  Left e -> expectationFailure e
