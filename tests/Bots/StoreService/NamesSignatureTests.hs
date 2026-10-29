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
    it "signs an order with the name terms' hash" $
      testMessage
        (orderMessage termsOrder termsTimestamp)
        "simplex.domains order 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 213536f106608428de763fe26b1e9e31f27244152e9a853614ad73c67002ee6f 1792828800"
        "0x0ba979ab2c6935848734b7d7ff5fa66800b1bd6814e34ee0ad4f5d62d226401b"
        "0xedd5071a12ad686a493bd8d9522cc95a292aea0e1970aaa3a4ea268696ed4f773b9dbfbf12d9aa79fd48bc1ea3825a720d8a09077bb4f35332480e1be1154fdf1c"
    it "signs an order with the community name terms' hash" $
      testMessage
        (orderMessage communityOrder termsTimestamp)
        "simplex.domains order 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 community/3d2141022b4e322767cb6c8dcac2daf21b9d2c9a2b93af726099a8bd4cc15bff 1792828800"
        "0x6d16887e061a31baedb6509fb0d01d7adc353c9496884b3af3d120b6aa850966"
        "0x866ffd975972fe6a668eed758e8a69b57900d5e0898aa2872d344216fb5dd5ff45e380b07792c5b0220ad9564a24c3513ad1d39ac30ff888168bbd1c8e17aa011b"
    it "signs find with the owner in lowercase" $
      testMessage
        (findMessage owner timestamp)
        "simplex.domains find 0x014f9c09cceedef1b713110d04d3a07d5b5d9b73 1792574100"
        "0x08a989fbf0457984c062b44d7237be9c7948f2cf23d48410609777a09b515646"
        "0xfa0908260c3f21c8e47bd37e2c3cf3bd6bc62966591ab1a2c4e31695646548f47d2e79b34413be27b4b07330682de642f48a8f5e641449d7d79728eeb4ab14da1c"
    it "signs another name for an order with the name terms' hash" $
      testMessage
        (retargetMessage registrationId termsOrder termsTimestamp)
        "simplex.domains name t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 213536f106608428de763fe26b1e9e31f27244152e9a853614ad73c67002ee6f 1792828800"
        "0x1f5cb2957192c0189be04450d1c1a39330e3e485b7d55bf07cfd2ad517d473b3"
        "0x8dbac86c2eb96cac72b0284c7221adf85816da3d9de83c1dc576f11531ba5d472dd6cd33d7ddaca7b3b8413746df8eda57eadfd29b14b52741a4079c0877a1061b"
    it "signs another name for an order with the community name terms' hash" $
      testMessage
        (retargetMessage registrationId communityOrder termsTimestamp)
        "simplex.domains name t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 community/3d2141022b4e322767cb6c8dcac2daf21b9d2c9a2b93af726099a8bd4cc15bff 1792828800"
        "0xbfe0f6e93445ebcee3b7c081c03c9cb6dcbeb9832b4917bd59ffbf3b87c94cc8"
        "0xd989c2fe071690005e34c8a42cd61fc4be2c1b7899259c08210ecf234808ed792cfc8ac7071d994ab3029983294e2060c01bdd8608dd8b16ab98763f99927aa11c"
    it "signs a spend from an order's balance with the name terms' hash" $
      testMessage
        (spendMessage registrationId termsOrder termsTimestamp)
        "simplex.domains spend t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 213536f106608428de763fe26b1e9e31f27244152e9a853614ad73c67002ee6f 1792828800"
        "0x88a513cf0aaeaa0d9e3de39eb45e2d5cf5d99add5c8f11a0b96b7e1a6931eea4"
        "0x888338b834c3f9f0eafb50366a0f436361fcb86813e8feda57501bfce6c00ace417a73b3956b6fbaebb29ee3983c6cdae52ba5dbff5b76d7bc7ed30c6006f1331b"
    it "signs a spend from an order's balance with the community name terms' hash" $
      testMessage
        (spendMessage registrationId communityOrder termsTimestamp)
        "simplex.domains spend t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 community/3d2141022b4e322767cb6c8dcac2daf21b9d2c9a2b93af726099a8bd4cc15bff 1792828800"
        "0x971b7bde57f40f45c075c4f83b882fe61e792783fbec5916e216be966e026650"
        "0x1bdb0d8d1f332cd8435d4d052f84060da55c8010a957e73fd38eb383925a5ba6606ebf04c09bce1697776670e4a7ad07d7cc2a18b4de3a7ea631e84f5cf77c6e1b"
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
    highS = "0xedd5071a12ad686a493bd8d9522cc95a292aea0e1970aaa3a4ea268696ed4f77c4624040ed26558602b743e15c7da58cad24d3df3393ace88d8a5070ef20f1621b"

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
      ofAgreedTerms = "213536f106608428de763fe26b1e9e31f27244152e9a853614ad73c67002ee6f"
    }

communityOrder :: OrderFields
communityOrder = termsOrder {ofAgreedTerms = "community/3d2141022b4e322767cb6c8dcac2daf21b9d2c9a2b93af726099a8bd4cc15bff"}

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
orderSignature = "0xedd5071a12ad686a493bd8d9522cc95a292aea0e1970aaa3a4ea268696ed4f773b9dbfbf12d9aa79fd48bc1ea3825a720d8a09077bb4f35332480e1be1154fdf1c"

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
