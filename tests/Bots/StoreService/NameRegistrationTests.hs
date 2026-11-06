{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NameRegistrationTests (badgeNameRegistrationTests) where

import StoreService.Names.Registration
import Bots.StoreService.FakeEthNode
import Bots.StoreService.WebUIMock
import Control.Concurrent.STM (TVar)
import Crypto.Random (ChaChaDRG)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Secp256k1 (signRecoverable)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain)
import Simplex.Messaging.SimplexName (SimplexTLD (..))
import StoreService.Names.Signatures (parseSignature)
import Test.Hspec

badgeNameRegistrationTests :: Spec
badgeNameRegistrationTests = describe "StoreService names: registration" $ do
  g <- runIO C.newRandom
  -- expected encodings and signatures are viem's, against the SimplexController, SimplexSponsor and SimplexResolver ABIs
  describe "encoding" $ do
    it "encodes registerWithCredit with and without records" $ testRegisterCalldata g
    it "encodes commit with the commitment of a registration" testCommitCalldata
  it "verifies the owner's register intent, and rejects another signer, a past deadline and a secret not of 32 bytes" $ testVerifyIntent g
  -- viem's hashTypedData and encodeFunctionData against the SimplexNamePolicy ABI of ens-contracts 6ccb5c5
  describe "terms acceptance" $ do
    it "hashes AcceptGeneralTerms and AcceptCommunityTerms as the policy does" testAcceptanceDigest
    it "encodes commitUnderGeneralTermsWithSig and commitUnderCommunityTermsWithSig" testAcceptCalldata
    it "verifies the owner's acceptance, and rejects another signer, another deadline and the other kind of terms" $ testVerifyAcceptance g

testRegisterCalldata :: TVar ChaChaDRG -> IO ()
testRegisterCalldata g = do
  let calldata r = toHex . registerWithCreditCalldata <$> knownIntent g r
  calldata knownRegistration
    `shouldReturn` "c4bd2ab50000000000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f4865700000000000000000000000000000000000000000000000000000000000000046000000000000000000000000000000000000000000000000000000000000001000000000000000000000000009858effd232b4033e47d90003d41ec34ecaeda940000000000000000000000000000000000000000000000000000000003c2670011111111111111111111111111111111111111111111111111111111111111110000000000000000000000002279b7a0a67db372996a5fab50d91eaa73d2ebe6000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000762616b657279310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000016000000000000000000000000000000000000000000000000000000000000000e410f13a8c58c186cadf476923d25ea4950487d1aaf21906ba5574c87f5b9435b61b994370000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e636f6e746163740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002068747470733a2f2f736d702e73696d706c65782e696d2f6123636f6e746163740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e410f13a8c58c186cadf476923d25ea4950487d1aaf21906ba5574c87f5b9435b61b994370000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e6368616e6e656c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001d68747470733a2f2f736d70312e73696d706c65782e696d2f63236f6e65000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000041f310b273d0e0ab0975a66d383fe818a5ec3474fcd922dbf3fe70094a3c0ceae86559b13ea0728a330613c6fe236a89a026daf345dcc8c0616e9eda2c69cbeb091b00000000000000000000000000000000000000000000000000000000000000"
  calldata knownRegistration {rContactLink = Nothing, rChannelLink = Nothing}
    `shouldReturn` "c4bd2ab50000000000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f486570000000000000000000000000000000000000000000000000000000000000001e000000000000000000000000000000000000000000000000000000000000001000000000000000000000000009858effd232b4033e47d90003d41ec34ecaeda940000000000000000000000000000000000000000000000000000000003c2670011111111111111111111111111111111111111111111111111111111111111110000000000000000000000002279b7a0a67db372996a5fab50d91eaa73d2ebe6000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000762616b6572793100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000411d0b3a477c99fbcadada2937b71127bf6da202f5cd25856649829653d8cf6a8c0ce0afb8505e398e23b3b01fcb3b17ffcf82d8ba6d24e9b854506894b2864ecf1c00000000000000000000000000000000000000000000000000000000000000"

testCommitCalldata :: IO ()
testCommitCalldata =
  toHex (commitCalldata $ makeCommitment knownRegistration) `shouldBe` "f14fcbc8810f0645b8d9839e3744bb956f9da4fa12bb51ec97459e08739c576a36cba0df"

testVerifyIntent :: TVar ChaChaDRG -> IO ()
testVerifyIntent g = do
  owner <- newEphemeralWallet g
  other <- newEphemeralWallet g
  let registration = knownRegistration {rOwner = ewAddress owner}
  signRegisterIntent g domain owner registration deadline >>= verifyRegisterIntent domain deadline >>= (`shouldBe` Right ())
  signRegisterIntent g domain other registration deadline >>= verifyRegisterIntent domain deadline >>= (`shouldBe` Left "intent is not signed by the owner")
  knownIntent g knownRegistration >>= verifyRegisterIntent domain (deadline + 1) >>= (`shouldBe` Left "intent expired")
  knownIntent g knownRegistration {rSecret = B.replicate 31 0x11} >>= verifyRegisterIntent domain deadline >>= (`shouldBe` Left "secret: expected 32 bytes")

-- fixtures

knownRegistration :: Registration
knownRegistration =
  Registration
    { rLabel = "bakery1",
      rTLD = TLDSimplex,
      rOwner = address "0x9858EfFD232B4033E47d90003D41EC34EcaEda94",
      rDuration = 63072000,
      rSecret = B.replicate 32 0x11,
      rResolver = address "0x2279b7a0a67db372996a5fab50d91eaa73d2ebe6",
      rContactLink = Just "https://smp.simplex.im/a#contact",
      rChannelLink = Just "https://smp1.simplex.im/c#one"
    }

knownIntent :: TVar ChaChaDRG -> Registration -> IO RegisterIntent
knownIntent g r = do
  w <- walletFromCode g knownCode
  signRegisterIntent g domain w r deadline

knownCode :: Text
knownCode = "SN-00000-00000-00000-00000-00000-0VME2"

domain :: Eip712Domain
domain = sponsorDomain fakeChainId fakeSponsor

deadline :: Word64
deadline = 4102444800

acceptanceDomain :: Eip712Domain
acceptanceDomain = policyDomain fakeChainId fakeNamePolicy

-- viem: keccak256(toHex("SimpleX community terms v1")) and keccak256(toHex("alicebakery commitment"))
acceptedHash, acceptedCommitment :: ByteString
acceptedHash = fromHex "eed23f946d290583f5f55c2ae8255db70adac4fc4c5de714a920d35eed9c7dac"
acceptedCommitment = fromHex "26d4dc807322bcdc1f68a2459b580919c4e6c5629d07cb20c38a31f7b72b7e5d"

acceptanceDeadline :: Word64
acceptanceDeadline = 1767315600

testAcceptanceDigest :: IO ()
testAcceptanceDigest = do
  toHex (acceptanceDigest acceptanceDomain (termsKind False) acceptedCommitment acceptedHash 3 acceptanceDeadline) `shouldBe` "f486b536cdf4e28031b02fb4345f26678a088d84846a0a6c72896062af283065"
  toHex (acceptanceDigest acceptanceDomain (termsKind True) acceptedCommitment acceptedHash 3 acceptanceDeadline) `shouldBe` "6df583ad3bcbef3e4061599fa9d45174fcda5731764320cd08ade35c8324eef7"

testAcceptCalldata :: IO ()
testAcceptCalldata = do
  sig <- either (fail . show) pure $ parseSignature $ "0x" <> T.replicate 32 "11" <> T.replicate 32 "22" <> "1b"
  let acceptance = TermsAcceptance {taTermsHash = acceptedHash, taNonce = 3, taSignature = sig}
      calldata community = toHex (acceptCalldata (termsKind community) acceptedCommitment (address "0x14dC79964da2C08b23698B3D3cc7Ca32193d9955") acceptanceDeadline acceptance)
      args =
        "26d4dc807322bcdc1f68a2459b580919c4e6c5629d07cb20c38a31f7b72b7e5deed23f946d290583f5f55c2ae8255db70adac4fc4c5de714a920d35eed9c7dac\
        \00000000000000000000000014dc79964da2c08b23698b3d3cc7ca32193d99550000000000000000000000000000000000000000000000000000000000000003\
        \000000000000000000000000000000000000000000000000000000006957189000000000000000000000000000000000000000000000000000000000000000c0\
        \00000000000000000000000000000000000000000000000000000000000000411111111111111111111111111111111111111111111111111111111111111111\
        \22222222222222222222222222222222222222222222222222222222222222221b00000000000000000000000000000000000000000000000000000000000000"
  calldata False `shouldBe` "c50f972f" <> args
  calldata True `shouldBe` "4b1a8e04" <> args

testVerifyAcceptance :: TVar ChaChaDRG -> IO ()
testVerifyAcceptance g = do
  owner <- newEphemeralWallet g
  other <- newEphemeralWallet g
  sig <- signRecoverable g (ewKey owner) (acceptanceDigest acceptanceDomain (termsKind True) acceptedCommitment acceptedHash 3 acceptanceDeadline) >>= either fail pure
  let acceptance = TermsAcceptance {taTermsHash = acceptedHash, taNonce = 3, taSignature = sig}
      verify community w = verifyAcceptance acceptanceDomain (termsKind community) (ewAddress w) acceptedCommitment
  verify True owner acceptanceDeadline acceptance `shouldReturn` Right ()
  verify True other acceptanceDeadline acceptance `shouldReturn` Left "acceptance is not signed by the owner"
  verify True owner (acceptanceDeadline + 1) acceptance `shouldReturn` Left "acceptance is not signed by the owner"
  verify False owner acceptanceDeadline acceptance `shouldReturn` Left "acceptance is not signed by the owner"

toHex :: ByteString -> ByteString
toHex = convertToBase Base16

fromHex :: ByteString -> ByteString
fromHex = right . convertFromBase Base16

right :: Either String a -> a
right = either (error . ("unexpected Left: " <>)) id
