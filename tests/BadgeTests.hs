{-# LANGUAGE DisambiguateRecordFields #-}
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

testFullWorkflow :: IO ()
testFullWorkflow = do
  Right (sk, pk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let req = BadgeRequest {masterKey = mk, badgeType = BTSupporter, expiry = Just futureTime}
  Just vreq <- verifyPayment (BPRedeemCode "TEST") req
  Right cred <- issueBadge sk pk vreq
  let BadgeCredential {masterKey = mk'} = cred
  mk' `shouldBe` mk
  verifyBadgeSignature pk cred >>= (`shouldBe` True)
  Right badge <- generateBadgeProof pk cred (BBSPresHeader "nonce-1")
  verifyBadge pk badge >>= (`shouldBe` True)
  Right badge2 <- generateBadgeProof pk cred (BBSPresHeader "nonce-2")
  verifyBadge pk badge2 >>= (`shouldBe` True)
  proof badge `shouldNotBe` proof badge2

testTamperedType :: IO ()
testTamperedType = do
  (pk, SupporterBadge {proof, presHeader, badgeExpiry}) <- issueBadgeProof BTSupporter (Just futureTime)
  verifyBadge pk (SupporterBadge {proof, presHeader, badgeExpiry, badgeType = BTBusiness}) >>= (`shouldBe` False)

testTamperedExpiry :: IO ()
testTamperedExpiry = do
  (pk, SupporterBadge {proof, presHeader, badgeType}) <- issueBadgeProof BTSupporter (Just futureTime)
  verifyBadge pk (SupporterBadge {proof, presHeader, badgeExpiry = Just pastTime, badgeType}) >>= (`shouldBe` False)

testWrongKey :: IO ()
testWrongKey = do
  (_, badge) <- issueBadgeProof BTSupporter (Just futureTime)
  Right (_, pk2) <- bbsKeyGen
  verifyBadge pk2 badge >>= (`shouldBe` False)

testExpiryCheck :: IO ()
testExpiryCheck = do
  now <- getCurrentTime
  (_, past) <- issueBadgeProof BTSupporter (Just pastTime)
  mkBadgeStatus now True past `shouldBe` BSExpired
  (_, future) <- issueBadgeProof BTSupporter (Just futureTime)
  mkBadgeStatus now True future `shouldBe` BSActive
  mkBadgeStatus now False future `shouldBe` BSFailed

testLifetimeBadge :: IO ()
testLifetimeBadge = do
  now <- getCurrentTime
  (pk, badge) <- issueBadgeProof BTCFInvestor Nothing
  verifyBadge pk badge >>= (`shouldBe` True)
  mkBadgeStatus now True badge `shouldBe` BSActive

testUnknownBadgeType :: IO ()
testUnknownBadgeType = do
  (pk, badge) <- issueBadgeProof (BTUnknown "future_type") (Just futureTime)
  verifyBadge pk badge >>= (`shouldBe` True)

-- Helpers

futureTime :: UTCTime
futureTime = posixSecondsToUTCTime 4102444800 -- 2099-12-31

pastTime :: UTCTime
pastTime = posixSecondsToUTCTime 1577836800 -- 2020-01-01

issueBadgeProof :: BadgeType -> Maybe UTCTime -> IO (BBSPublicKey, SupporterBadge)
issueBadgeProof bt expiry = do
  Right (sk, pk) <- bbsKeyGen
  drg <- C.newRandom
  mk <- generateMasterKey drg
  let vreq = VerifiedBadgeRequest BadgeRequest {masterKey = mk, badgeType = bt, expiry}
  Right cred <- issueBadge sk pk vreq
  Right badge <- generateBadgeProof pk cred (BBSPresHeader "test-nonce")
  pure (pk, badge)
