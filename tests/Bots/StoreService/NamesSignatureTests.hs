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
    it "signs an order" $
      testMessage
        (orderMessage dynamisOrder timestamp)
        "simplex.domains order 0x78b602f03f164366531da963764c1f2fdfd588d0487daf2aa261ca799e5ffe33 7 69259500 0x9cf04ddef2eedfb02e6e7ba58f879c0636c584b6acc67ffe9f97a06bfb016ac1 0xb1971beb567c8dc1b2f01f0d823754c6168d8d6830db2221702a7678e1804674 1792574100"
        "0x5deee1b3589df9e53665e34aa65a491a4eabfd76a39207b0e9303457f7c44b7a"
        "0x73e32be461e99e64b093a4ad09b04b4d810e5623a1b0e9c9fdb4701ec2a6a2a84df9ffdccc39c5cb827cde4d846c7662f591665f1b868ab1c15778646d3f414e1b"
    it "signs find with the owner in lowercase" $
      testMessage
        (findMessage owner timestamp)
        "simplex.domains find 0x014f9c09cceedef1b713110d04d3a07d5b5d9b73 1792574100"
        "0x08a989fbf0457984c062b44d7237be9c7948f2cf23d48410609777a09b515646"
        "0xfa0908260c3f21c8e47bd37e2c3cf3bd6bc62966591ab1a2c4e31695646548f47d2e79b34413be27b4b07330682de642f48a8f5e641449d7d79728eeb4ab14da1c"
    it "signs another name for an order, with its duration" $
      testMessage
        (retargetMessage registrationId dynamisOrder {ofLabelHash = labelHash "another"} timestamp)
        "simplex.domains name t2Jk8RmQ4vXw9NcP3yLb6g 0x5bfbbf75311ec002834031d73fa30bfe8bdf675f4a3df72443841490c4b49d02 7 69259500 0x9cf04ddef2eedfb02e6e7ba58f879c0636c584b6acc67ffe9f97a06bfb016ac1 0xb1971beb567c8dc1b2f01f0d823754c6168d8d6830db2221702a7678e1804674 1792574100"
        "0x8bc79aa98fbd53013e8961db27554ecd17139e14ec168b86afbaf2fc910bfc52"
        "0xe3d680bcc62255179d5260beeb920319c86bd919f6b01927bee01509cf6c1fe3121151947fb9e3a310e6652c0c9a9bf7ceb48e8d903774a38a3162d916e8b4591b"
    it "signs a spend from an order's balance, as the webapp signs it" $
      testMessage
        (spendMessage registrationId spendOrder timestamp)
        "simplex.domains spend t2Jk8RmQ4vXw9NcP3yLb6g 0x5bfbbf75311ec002834031d73fa30bfe8bdf675f4a3df72443841490c4b49d02 7 69259500 0xbb87d2d7aa14251a04cdd68b5d878d0b4c44c6c76486d90a534dab9525105e33 0x51431a705eb4903084cc8dbf3c1b966c9372914511c4623c889d49fdb8047da4 1792574100"
        "0x72c2f4b7d84839fb97eb78668355a02fc0b18e211f264fca97d19a8f36decde3"
        "0x8668cb4b72295479b4f9bcc2fd07fabfec2b4b8ae9230f2553536acb93c3e0cf6b2dad928a9a11b003c43ab43edeec117ce01c9214b8e99fe75fd547425e74481b"
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
  recoverAddress RecoverableSignature {rsR, rsS, rsRecId = B.head v - 27} (personalDigest $ orderMessage dynamisOrder timestamp)
    `shouldReturn` Right owner
  where
    highS = "0x73e32be461e99e64b093a4ad09b04b4d810e5623a1b0e9c9fdb4701ec2a6a2a8b206002333c63a347d8321b27b93899bc51d768793c21589fe7ae62862f6fff31c"

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

dynamisOrder :: OrderFields
dynamisOrder =
  OrderFields
    { ofLabelHash = labelHash "dynamis",
      ofLength = 7,
      ofDuration = duration,
      ofCommitment = commitment,
      ofSealed = "q3Zk0Yx7c2VhbGVkLW9yZGVyLWZpeHR1cmU"
    }

-- the webapp's spend vector (test/flow.test.ts, SPEND): "another" with no links, sealed under a zero IV
spendOrder :: OrderFields
spendOrder =
  OrderFields
    { ofLabelHash = labelHash "another",
      ofLength = 7,
      ofDuration = duration,
      ofCommitment = hx "0xbb87d2d7aa14251a04cdd68b5d878d0b4c44c6c76486d90a534dab9525105e33",
      ofSealed = "AAAAAAAAAAAAAAAACWgxqMk-UKfpra_bgbzR57AIDvSNmCR_G7IYbN7DX70sL2FVRHB1VfC0wdF-tc3ajHM3o8koM159Ja8hyg"
    }

commitment :: ByteString
commitment = hx "0x9cf04ddef2eedfb02e6e7ba58f879c0636c584b6acc67ffe9f97a06bfb016ac1"

node :: ByteString
node = hx "0x833cc73a7eae1ddee67ec8845ec3d39799406c7bc87fab7c504a7e0ab5cb89bb"

-- 21 October 2026 09:15 UTC
timestamp :: Word64
timestamp = 1792574100

-- 2 years of 365 days and the free days until 1 January 2027
duration :: Word64
duration = 69259500

registrationId :: RegistrationId
registrationId = RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g"

orderSignature :: Text
orderSignature = "0x73e32be461e99e64b093a4ad09b04b4d810e5623a1b0e9c9fdb4701ec2a6a2a84df9ffdccc39c5cb827cde4d846c7662f591665f1b868ab1c15778646d3f414e1b"

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
