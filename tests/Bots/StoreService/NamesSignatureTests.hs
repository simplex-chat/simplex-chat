{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesSignatureTests (namesSignatureTests) where

import Control.Monad (forM_)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature (..))
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address, recoverAddress)
import Simplex.Messaging.SimplexName (SimplexTLD (..), labelHash)
import StoreService.Names.Abi
import StoreService.Names.Registration
import StoreService.Names.Signatures
import Test.Hspec

namesSignatureTests :: Spec
namesSignatureTests = describe "StoreService names: signatures" $ do
  -- expected values are viem 2.57.5's for the fixtures below, the webapp's wallet (BIP-39 entropy 6f2d1c9a4b7e3f80d15a2c6e9b04f7a3, account 0)
  -- and the SimplexSponsor, SimplexController, SimplexResolver and SimplexPriceOracle ABIs of ens-contracts PR #36 at 23892cc
  describe "EIP-712" $ do
    it "commits to a registration with one contact link" $
      makeCommitment dynamis `shouldBe` commitment
    it "hashes RegisterName" $
      registerIntentDigest (sponsorDomain chainId sponsor) dynamis 0 1800000000 `shouldBe` hx "0x4b0bcc282db581446f6c25e0e3fbe2839409acbc011bb3e082523dd9eab85a6a"
  describe "EIP-191 messages" $ do
    -- the contract's Appendix C, which the webapp computed with viem and pins in test/terms-vectors.test.ts
    it "signs an order with the terms version" $
      testMessage
        (orderMessage termsOrder termsTimestamp)
        "simplex.domains order 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 2026-10-23 1792828800"
        "0x50dfb031e558f78b83fa07c7ea0207c02a98d73751e43bc754dd26b7bc92b840"
        "0x17107b11494e25098efc348aca64a2469ffb832bcc79dc57377a1b26a4004b6701ad73648f50af8b39d4079820a3de7e681ddf15f8cf15b5cff02b4a48cf1fe61c"
    it "signs an order with the community terms" $
      testMessage
        (orderMessage communityOrder termsTimestamp)
        "simplex.domains order 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 2026-10-23/community 1792828800"
        "0xf8521525761939c4cef8979cb54217e9e32b1a57999fc2f03e486f4eb1903802"
        "0x03eccb591b79ab4e3e6f22ccbeb77a6ac32c6dad8cb318557836258c390bc31d78852a3279c6c8cc2ef8d1025641d45ba4101f54dfd84c264deb2ab391c1cbc61c"
  describe "signature format" $ do
    it "refuses a high s, though recovery accepts it" testHighS
    it "refuses v other than 27 or 28" $
      forM_ ["00", "01", "1d"] $ \v -> parseSignature (T.dropEnd 2 orderSignature <> v) `shouldBe` Left SERecoveryId
    it "refuses uppercase hex, no 0x and a wrong length" $ do
      parseSignature ("0x" <> T.toUpper (T.drop 2 orderSignature)) `shouldBe` Left SEFormat
      parseSignature (T.drop 2 orderSignature) `shouldBe` Left SEFormat
      parseSignature (T.dropEnd 2 orderSignature) `shouldBe` Left SEFormat
      parseSignature (orderSignature <> "00") `shouldBe` Left SEFormat
  describe "addresses" $ do
    it "accepts the EIP-55 form in orders" $
      parseChecksumAddress ownerText `shouldBe` Right owner
    it "refuses a letter in the wrong case and one-case hex in orders" $ do
      parseChecksumAddress "0x014F9c09CcEeDEF1B713110d04D3a07d5B5D9b73" `shouldBe` Left ADChecksum
      parseChecksumAddress ownerLower `shouldBe` Left ADChecksum
      parseChecksumAddress ownerUpper `shouldBe` Left ADChecksum
    it "accepts any case in find" $
      forM_ [ownerText, ownerLower, ownerUpper, "0x014F9c09CcEeDEF1B713110d04D3a07d5B5D9b73"] $ \a ->
        parseAnyCaseAddress a `shouldBe` Right owner
    it "refuses what is not an address" $
      forM_ ["014f9c09cceedef1b713110d04d3a07d5b5d9b73", "0X014f9c09cceedef1b713110d04d3a07d5b5d9b73", T.dropEnd 1 ownerLower, ownerLower <> "0", "0x014f9c09cceedef1b713110d04d3a07d5b5d9b7g"] $ \a -> do
        parseAnyCaseAddress a `shouldBe` Left ADFormat
        parseChecksumAddress a `shouldBe` Left ADFormat
  describe "ABI return data" $ do
    it "decodes each type as viem encodes it" $ do
      decodeUint256 (hx "00000000000000000000000000000000000000018ee90ff6c373e0ee4e3f0ad2") `shouldBe` Right 123456789012345678901234567890
      decodeAddress (hx "000000000000000000000000014f9c09cceedef1b713110d04d3a07d5b5d9b73") `shouldBe` Right owner
      decodeBool (hx "0000000000000000000000000000000000000000000000000000000000000001") `shouldBe` Right True
      decodeBool (B.replicate 32 0) `shouldBe` Right False
      decodeBytes32 (hx "833cc73a7eae1ddee67ec8845ec3d39799406c7bc87fab7c504a7e0ab5cb89bb") `shouldBe` Right node
      decodeString retString `shouldBe` Right "https://smp8.simplex.im/a#contact"
      decodeString (hx "00000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000") `shouldBe` Right ""
      decodeBytes (hx "0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000010410f13a8c833cc73a7eae1ddee67ec8845ec3d39799406c7bc87fab7c504a7e0ab5cb89bb000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e636f6e746163740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002168747470733a2f2f736d70382e73696d706c65782e696d2f6123636f6e746163740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000") `shouldBe` Right (hx "10f13a8c833cc73a7eae1ddee67ec8845ec3d39799406c7bc87fab7c504a7e0ab5cb89bb000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e636f6e746163740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002168747470733a2f2f736d70382e73696d706c65782e696d2f6123636f6e7461637400000000000000000000000000000000000000000000000000000000000000")
    it "refuses data that does not hold the type" $ do
      decodeUint256 (B.replicate 31 0) `shouldBe` Left AEShort
      decodeAddress (B.singleton 1 <> B.drop 1 (hx "000000000000000000000000014f9c09cceedef1b713110d04d3a07d5b5d9b73")) `shouldBe` Left AENotAddress
      decodeBool (uint256 (2 :: Int)) `shouldBe` Left AENotBool
      decodeString (uint256 (4096 :: Int) <> B.drop 32 retString) `shouldBe` Left AEOffset
      decodeString (B.take 80 retString) `shouldBe` Left AEShort
      decodeString (abiTuple [Dynamic $ abiBytes "\xff"]) `shouldBe` Left AENotUtf8

testMessage :: ByteString -> ByteString -> Text -> Text -> IO ()
testMessage msg expected digest signature = do
  msg `shouldBe` expected
  personalDigest msg `shouldBe` hx digest
  sig <- either (fail . show) pure $ parseSignature signature
  recoverAddress sig (personalDigest msg) `shouldReturn` Right owner

testHighS :: IO ()
testHighS = do
  parseSignature highS `shouldBe` Left SEHighS
  let (rsR, (rsS, v)) = B.splitAt 32 <$> B.splitAt 32 (hx highS)
  recoverAddress RecoverableSignature {rsR, rsS, rsRecId = B.head v - 27} (personalDigest $ orderMessage termsOrder termsTimestamp)
    `shouldReturn` Right owner
  where
    -- the order vector's signature with s replaced by n - s and v flipped
    highS = "0x17107b11494e25098efc348aca64a2469ffb832bcc79dc57377a1b26a4004b67fe528c9b70af5074c62bf867df5c21805290fdd0b6798a85efe233428767215b1b"

-- fixtures

dynamis :: Registration
dynamis =
  Registration
    { rLabel = "dynamis",
      rTLD = TLDSimplex,
      rOwner = owner,
      rDuration = duration,
      rSecret = B.replicate 32 0x11,
      rResolver = resolver,
      rContactLink = Just "https://smp8.simplex.im/a#contact",
      rChannelLink = Nothing
    }

-- Appendix C's derived inputs: alicebakery with no links, its commitment as the webapp builds it, and a new seal under the fixture seed and a zero IV
termsOrder :: OrderFields
termsOrder =
  OrderFields
    { ofLabelHash = labelHash "alicebakery",
      ofLength = 11,
      ofDuration = 69292800,
      ofCommitment = hx "0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5",
      ofSealed = "AAAAAAAAAAAAAAAACWgxqMk-UKfpra_Zh6vc4KNBR6SX1WYpGb4COoWCCbM0bisYCH14WLzghp8hqi_-KyXSi4qf_DAdP_M4bFetD0s",
      ofAgreedTerms = "2026-10-23"
    }

communityOrder :: OrderFields
communityOrder = termsOrder {ofAgreedTerms = "2026-10-23/community"}

commitment :: ByteString
commitment = hx "0x9cf04ddef2eedfb02e6e7ba58f879c0636c584b6acc67ffe9f97a06bfb016ac1"

node :: ByteString
node = hx "0x833cc73a7eae1ddee67ec8845ec3d39799406c7bc87fab7c504a7e0ab5cb89bb"

-- 21 October 2026 09:15 UTC
-- 24 October 2026 08:00 UTC
termsTimestamp :: Word64
termsTimestamp = 1792828800

-- 2 years of 365 days and the free days until 1 January 2027
duration :: Word64
duration = 69259500

orderSignature :: Text
orderSignature = "0x17107b11494e25098efc348aca64a2469ffb832bcc79dc57377a1b26a4004b6701ad73648f50af8b39d4079820a3de7e681ddf15f8cf15b5cff02b4a48cf1fe61c"

retString :: ByteString
retString = hx "0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000002168747470733a2f2f736d70382e73696d706c65782e696d2f6123636f6e7461637400000000000000000000000000000000000000000000000000000000000000"

owner :: Address
owner = either error id $ strDecode $ encodeUtf8 ownerText

ownerText, ownerLower, ownerUpper :: Text
ownerText = "0x014f9c09CcEeDEF1B713110d04D3a07d5B5D9b73"
ownerLower = T.toLower ownerText
ownerUpper = "0x" <> T.toUpper (T.drop 2 ownerText)

sponsor, resolver :: Address
sponsor = either error id $ strDecode "0x9a676e781a523b5d0c0e43731313a708cb607508"
resolver = either error id $ strDecode "0x2279b7a0a67db372996a5fab50d91eaa73d2ebe6"

chainId :: Word64
chainId = 31337

hx :: Text -> ByteString
hx t = either error id $ convertFromBase Base16 $ encodeUtf8 $ fromMaybe t $ T.stripPrefix "0x" t
