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
import StoreService.Names.Phases (RegistrationId (..))
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
    it "hashes SetText" $
      setTextDigest (resolverDomain chainId resolver) (namehash TLDSimplex "dynamis") "simplex.contact" "https://x" 3 99 `shouldBe` hx "0xa62d4320889eed3713462d725dc298cd96885572b8e6b424e93a205a96b96fb5"
    it "hashes ClearRecords" $
      clearRecordsDigest (resolverDomain chainId resolver) (namehash TLDSimplex "dynamis") 4 99 `shouldBe` hx "0xca4bd243288f27f38e3b45384ba36ce685ff155352cf8125f031e6fb189a46dd"
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
    it "signs find with the owner in lowercase" $
      testMessage
        (findMessage owner timestamp)
        "simplex.domains find 0x014f9c09cceedef1b713110d04d3a07d5b5d9b73 1792574100"
        "0x08a989fbf0457984c062b44d7237be9c7948f2cf23d48410609777a09b515646"
        "0xfa0908260c3f21c8e47bd37e2c3cf3bd6bc62966591ab1a2c4e31695646548f47d2e79b34413be27b4b07330682de642f48a8f5e641449d7d79728eeb4ab14da1c"
    it "signs another name for an order with the terms version" $
      testMessage
        (retargetMessage registrationId termsOrder termsTimestamp)
        "simplex.domains name t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 2026-10-23 1792828800"
        "0x01eda78f875428396a940b658b910b6eb6dc0bce82860c17d59f8f16e7767d25"
        "0x1d29abf2489da963d59d0982074b5af770efbaf96be3c756afbd6a7fea672b507e756944da0901724e2a5d7cfe2419ccb7573310ae65e65f4fd35d7c6fe494b51c"
    it "signs another name for an order with the community terms" $
      testMessage
        (retargetMessage registrationId communityOrder termsTimestamp)
        "simplex.domains name t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 2026-10-23/community 1792828800"
        "0x4f1af02904f9f8863e92169f6e1a1d6733a3a25c5d22ffde7ac00f09a660c022"
        "0x30d027224b85cd9ac638edec1266748f21f46279eda72e68448f97b8fce73ca06c401035ad5416fc1e48f39646d0637fcaad1588ec74562ff842821b33e299361b"
    it "signs a spend from an order's balance with the terms version" $
      testMessage
        (spendMessage registrationId termsOrder termsTimestamp)
        "simplex.domains spend t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 2026-10-23 1792828800"
        "0x109f0bd4f5eb1aa054f17c82a0b04dbd5d1e7b23c5ee91cf69464fb1435eb034"
        "0x20e930097b35350861332afa4b0b5dcff746aa879875269cc784575e6bb871711d3f6b1e11437d0a616a37a8b5b1ebbdd4c98aee04e09405ac3fa55a6af6ff831b"
    it "signs a spend from an order's balance with the community terms" $
      testMessage
        (spendMessage registrationId communityOrder termsTimestamp)
        "simplex.domains spend t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 2026-10-23/community 1792828800"
        "0xd114398466a7a6d7d9a183feb4624e06c41d1dcdfb6b69e896ff9d74c70845c3"
        "0x90d2ab549b2cf5fd51442ac3f410b934e7fa45bd12853cfbfa2f8ab4876ed5600b9fe3a90459140f180949353e7dc3f56cd1447de31ae5c05e12f7bf6a113f061c"
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
timestamp :: Word64
timestamp = 1792574100

-- 24 October 2026 08:00 UTC
termsTimestamp :: Word64
termsTimestamp = 1792828800

-- 2 years of 365 days and the free days until 1 January 2027
duration :: Word64
duration = 69259500

registrationId :: RegistrationId
registrationId = RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g"

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
