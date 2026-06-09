{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BadgeTests (badgeTests) where

import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Chat.Badges
import Simplex.Messaging.Crypto.BBS
import Test.Hspec

badgeTests :: Spec
badgeTests = do
  it "full workflow: request, issue, verify credential, generate and verify proof" testFullWorkflow
  it "should reject badge with tampered type" testTamperedType
  it "should reject badge with tampered expiry" testTamperedExpiry
  it "should reject badge with wrong server key" testWrongKey
  it "should compute badge status correctly" testExpiryCheck
  it "should treat lifetime badges as always active" testLifetimeBadge
  it "should accept unknown badge types" testUnknownBadgeType

proofOf :: Badge 'BCProof -> BBSProof
proofOf (BadgeProof _ p _) = p

proofInfo :: Badge 'BCProof -> BadgeInfo
proofInfo (BadgeProof _ _ i) = i

testFullWorkflow :: IO ()
testFullWorkflow = do
  Right (sk, pk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let req = BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = Just futureTime, badgeExtra = ""}}
  Just vreq <- verifyPayment (BPRedeemCode "TEST") req
  Right cred <- issueBadge sk pk vreq
  let BadgeCredential mk' _ _ = cred
  mk' `shouldBe` mk
  verifyCredential pk cred >>= (`shouldBe` True)
  Right badge <- generateBadgeProof pk cred (BBSPresHeader "nonce-1")
  verifyBadge pk badge >>= (`shouldBe` True)
  Right badge2 <- generateBadgeProof pk cred (BBSPresHeader "nonce-2")
  verifyBadge pk badge2 >>= (`shouldBe` True)
  proofOf badge `shouldNotBe` proofOf badge2

testTamperedType :: IO ()
testTamperedType = do
  (pk, BadgeProof ph p info) <- issueBadgeProof BTSupporter (Just futureTime)
  verifyBadge pk (BadgeProof ph p info {badgeType = BTBusiness}) >>= (`shouldBe` False)

testTamperedExpiry :: IO ()
testTamperedExpiry = do
  (pk, BadgeProof ph p info) <- issueBadgeProof BTSupporter (Just futureTime)
  verifyBadge pk (BadgeProof ph p info {badgeExpiry = Just pastTime}) >>= (`shouldBe` False)

testWrongKey :: IO ()
testWrongKey = do
  (_, badge) <- issueBadgeProof BTSupporter (Just futureTime)
  Right (_, pk2) <- bbsKeyGen
  verifyBadge pk2 badge >>= (`shouldBe` False)

testExpiryCheck :: IO ()
testExpiryCheck = do
  now <- getCurrentTime
  let pastInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = Just pastTime, badgeExtra = ""}
      futureInfo = BadgeInfo {badgeType = BTSupporter, badgeExpiry = Just futureTime, badgeExtra = ""}
  mkBadgeStatus now True pastInfo `shouldBe` BSExpired
  mkBadgeStatus now True futureInfo `shouldBe` BSActive
  mkBadgeStatus now False futureInfo `shouldBe` BSFailed

testLifetimeBadge :: IO ()
testLifetimeBadge = do
  now <- getCurrentTime
  (pk, badge) <- issueBadgeProof BTInvestor Nothing
  verifyBadge pk badge >>= (`shouldBe` True)
  mkBadgeStatus now True (proofInfo badge) `shouldBe` BSActive

testUnknownBadgeType :: IO ()
testUnknownBadgeType = do
  (pk, badge) <- issueBadgeProof (BTUnknown "future_type") (Just futureTime)
  verifyBadge pk badge >>= (`shouldBe` True)

-- Helpers

futureTime :: UTCTime
futureTime = posixSecondsToUTCTime 4102444800 -- 2099-12-31

pastTime :: UTCTime
pastTime = posixSecondsToUTCTime 1577836800 -- 2020-01-01

issueBadgeProof :: BadgeType -> Maybe UTCTime -> IO (BBSPublicKey, Badge 'BCProof)
issueBadgeProof bt expiry = do
  Right (sk, pk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let vreq = VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeInfo = BadgeInfo {badgeType = bt, badgeExpiry = expiry, badgeExtra = ""}}
  Right cred <- issueBadge sk pk vreq
  Right badge <- generateBadgeProof pk cred (BBSPresHeader "test-nonce")
  pure (pk, badge)
