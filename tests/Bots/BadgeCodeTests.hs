{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests for the pure, database-free redemption-code primitives (BadgeService.Codes).
-- Registered under the "Supporter badges" hspec path (not "SimpleX Badge service bot"): these
-- tests need no database and must run in CI. The rest of the RedeemOutcome mapping is covered
-- by B10, which drives every outcome through purchaseBadge.
module Bots.BadgeCodeTests (badgeCodeTests) where

import BadgeService.Codes
import Control.Concurrent.STM (atomically)
import qualified Data.ByteString as BS
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import qualified Simplex.Messaging.Crypto as C
import Test.Hspec

badgeCodeTests :: Spec
badgeCodeTests = do
  describe "deriveOrderCode / normalizeCode" $ do
    it "is deterministic for a given secret and orderId" $
      deriveOrderCode secretA "order-1" `shouldBe` deriveOrderCode secretA "order-1"
    it "differs across orderIds" $
      deriveOrderCode secretA "order-1" `shouldNotBe` deriveOrderCode secretA "order-2"
    it "differs across secrets" $
      deriveOrderCode secretA "order-1" `shouldNotBe` deriveOrderCode secretB "order-1"
    it "normalizeCode inverts the display formatting stably across calls" $ do
      let code = deriveOrderCode secretA "order-1"
      normalizeCode code `shouldBe` normalizeCode code
      T.length (normalizeCode code) `shouldBe` 20
    it "a derived code passes its own checksum" $
      verifyChecksum (normalizeCode (deriveOrderCode secretA "order-1")) `shouldBe` True

  describe "normalizeCode" $ do
    it "maps a lowercase spaced sxb-prefixed code and a dashed uppercase one to the same canonical value" $ do
      let canonical = "01234567890123456789" :: Text
          spaced = "sxb Oi23456789oL23456789" :: Text
          dashed = "SXB-OI234-56789-OL234-56789" :: Text
          bare = "Oi23456789oL23456789" :: Text
      normalizeCode spaced `shouldBe` canonical
      normalizeCode dashed `shouldBe` canonical
      normalizeCode bare `shouldBe` canonical
    it "does not truncate a bare 20-character code whose first three characters are SXB" $ do
      let bareSxb = "SXB23456789ABCDEFGHJ" :: Text
      T.length bareSxb `shouldBe` 20
      normalizeCode bareSxb `shouldBe` bareSxb

  describe "verifyChecksum" $
    it "catches every single-character substitution" $ do
      let code = normalizeCode (deriveOrderCode secretA "order-1")
          alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ" :: String
          substitutions =
            [ T.pack (prefix ++ (c' : drop 1 suffix))
              | i <- [0 .. T.length code - 1],
                let (prefix, suffix) = splitAt i (T.unpack code),
                c' <- alphabet,
                c' /= head suffix
            ]
      verifyChecksum code `shouldBe` True
      all (not . verifyChecksum) substitutions `shouldBe` True

  describe "classifyRedemption" $
    it "classifies a checksum-failing code as RedeemInvalid without ever calling the database lookup" $ do
      drg <- C.newRandom
      (purchaseKey, _priv :: C.PrivateKeyEd25519) <- atomically $ C.generateKeyPair drg
      let goodCode = normalizeCode (deriveOrderCode secretA "order-1")
          corrupted = T.cons (succ (T.head goodCode)) (T.tail goodCode)
          neverLookup :: t -> Identity (Maybe a)
          neverLookup _ = error "classifyRedemption must not look up a checksum-failing code"
      verifyChecksum corrupted `shouldBe` False
      runIdentity (classifyRedemption epoch purchaseKey neverLookup corrupted) `shouldBe` RedeemInvalid
  where
    secretA = BS.pack [1 .. 32]
    secretB = BS.pack (reverse [1 .. 32])
    epoch = UTCTime (fromGregorian 2026 1 1) 0
