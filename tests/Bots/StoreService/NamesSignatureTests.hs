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
    it "signs an order with the general terms' hash" $
      testMessage
        (orderMessage termsOrder termsTimestamp)
        "simplex.domains order 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 67b989cccc7ae150142daa4029304c7b7e78ab0a4ccebeba2ec1ef8a30f8147e 1792828800"
        "0x2687afde089cba36f420243154bfdd22fe44e8f07c8cfee652bd61cd6d9271fe"
        "0x9fb22e344357679a4deffdd3d75d6bfb6be860e4d2d307661445812dc4e134c051b726fc8ae16aaf4df17c615b2eeac4e10953524691b186bff2e6b68268d0b31b"
    it "signs an order with the community terms' hash" $
      testMessage
        (orderMessage communityOrder termsTimestamp)
        "simplex.domains order 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 community/bbc52157d9b12f23af62ae8a7eacf2ea11e7db311fa2ed5bd48ea9ba01f77c4d 1792828800"
        "0x456ec129e7612ef629f004f3beaf82a58ebf01d59cc7c4e2616e01a72ca01c65"
        "0x11ade94c79f14bdfde60b23f60f217206b7fd6b29504a2e3020eaef5dabbf24c39330b92099ce6564c36c0a2e484c91a5a9cfa341285a05a169bafbb1dfd1cb01c"
    it "signs find with the owner in lowercase" $
      testMessage
        (findMessage owner timestamp)
        "simplex.domains find 0x014f9c09cceedef1b713110d04d3a07d5b5d9b73 1792574100"
        "0x08a989fbf0457984c062b44d7237be9c7948f2cf23d48410609777a09b515646"
        "0xfa0908260c3f21c8e47bd37e2c3cf3bd6bc62966591ab1a2c4e31695646548f47d2e79b34413be27b4b07330682de642f48a8f5e641449d7d79728eeb4ab14da1c"
    it "signs another name for an order with the general terms' hash" $
      testMessage
        (retargetMessage registrationId termsOrder termsTimestamp)
        "simplex.domains name t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 67b989cccc7ae150142daa4029304c7b7e78ab0a4ccebeba2ec1ef8a30f8147e 1792828800"
        "0x9545b17da4a1fcc96d66f53b8358629c5987ec2a67d16e44f4e0a298194dc0e9"
        "0xa1ae312f7178440aaf5a04789d7ed388af7215624b214ea7aa98d43209f0c98e10b64a1c553053e88c3ecd6d21769d34ff9b6a3485e04e2d23bd4b0faabb1dc01c"
    it "signs another name for an order with the community terms' hash" $
      testMessage
        (retargetMessage registrationId communityOrder termsTimestamp)
        "simplex.domains name t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 community/bbc52157d9b12f23af62ae8a7eacf2ea11e7db311fa2ed5bd48ea9ba01f77c4d 1792828800"
        "0x5a798c69de346efc5ede90d1b4ff30854ab0eb326539ba9b60eddea1ab08005f"
        "0xdd8794dd1c65c8e11955127408e44cd63cfc8d4b7050d5fee05b65d5abfb8a3b62f6648c2a337183adc822d129b2aba3030c8812cf87b7e969c690fb160a805f1c"
    it "signs a spend from an order's balance with the general terms' hash" $
      testMessage
        (spendMessage registrationId termsOrder termsTimestamp)
        "simplex.domains spend t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 67b989cccc7ae150142daa4029304c7b7e78ab0a4ccebeba2ec1ef8a30f8147e 1792828800"
        "0xf643f32bd75923423f92eeee3dc46c6503f4969ad37b66bddfab5587b7e7d62e"
        "0x584dfddcbd6f5a1ccaa0dd60b8f2929f629da471aa4ecae270bd429243f2853c77eff724a4820e95765ca006302c834fdb247d3b1c69c5db4074045c89957f541c"
    it "signs a spend from an order's balance with the community terms' hash" $
      testMessage
        (spendMessage registrationId communityOrder termsTimestamp)
        "simplex.domains spend t2Jk8RmQ4vXw9NcP3yLb6g 0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6 11 69292800 0x5e62a4cb7da596cb38169a6a552b5f69db2acee279b53be6693a78e03c3dfaa5 0x22709445157a2a1f7cb7e284e3ccf6c7ebc3b4d857002af452853c4a65907fe2 community/bbc52157d9b12f23af62ae8a7eacf2ea11e7db311fa2ed5bd48ea9ba01f77c4d 1792828800"
        "0x1c02a90eaa2073c155b816d9597ddbe694dfa5ded03caafeb88feb9ed9a077e1"
        "0x61f25926cf4250e6924cfbc63748cc60e01486434984988e8904a655c893afd22f9efabc4587890f522ec91a124f066ba0d9b8acfbba9c836009d8a41609624a1b"
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
    highS = "0x9fb22e344357679a4deffdd3d75d6bfb6be860e4d2d307661445812dc4e134c0ae48d903751e9550b20e839ea4d11539d9a5899468b6eeb4ffdf77d64dcd708e1c"

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
      ofAgreedTerms = "67b989cccc7ae150142daa4029304c7b7e78ab0a4ccebeba2ec1ef8a30f8147e"
    }

communityOrder :: OrderFields
communityOrder = termsOrder {ofAgreedTerms = "community/bbc52157d9b12f23af62ae8a7eacf2ea11e7db311fa2ed5bd48ea9ba01f77c4d"}

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
orderSignature = "0x9fb22e344357679a4deffdd3d75d6bfb6be860e4d2d307661445812dc4e134c051b726fc8ae16aaf4df17c615b2eeac4e10953524691b186bff2e6b68268d0b31b"

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
