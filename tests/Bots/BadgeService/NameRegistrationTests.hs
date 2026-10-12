{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeService.NameRegistrationTests (badgeNameRegistrationTests) where

import BadgeService.Names.Chain
import BadgeService.Names.Registration
import Bots.BadgeService.FakeEthNode
import Bots.BadgeService.WebUIMock
import Control.Concurrent.STM (TVar, atomically)
import Crypto.Random (ChaChaDRG)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Word (Word64)
import qualified Simplex.Messaging.Crypto as C
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain)
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..), signEip1559Tx)
import Simplex.Messaging.SimplexName (SimplexTLD (..))
import Test.Hspec

-- expected encodings and signatures are viem's, against the SimplexController, SimplexSponsor and SimplexResolver ABIs
badgeNameRegistrationTests :: Spec
badgeNameRegistrationTests = describe "badge service name registration" $ do
  g <- runIO C.newRandom
  describe "encoding" $ do
    it "computes the commitment of a registration" testCommitment
    it "encodes registerWithCredit with records" $ testRegisterCalldataWithRecords g
    it "encodes registerWithCredit without records" $ testRegisterCalldataWithoutRecords g
    it "encodes commit" testCommitCalldata
  describe "verifying the register intent" $ do
    it "refuses an intent signed by another wallet" $ testVerifyOtherSigner g
    it "refuses an intent past its deadline" $ testVerifyExpired g
    it "refuses a secret that is not 32 bytes" $ testVerifyShortSecret g
  it "sends nothing when the dry run reverts" $ testRevertedDryRun g
  it "registers a name the client committed to without revealing it" $ testCommitThenRegister g

testCommitment :: IO ()
testCommitment =
  toHex (makeCommitment withRecords) `shouldBe` "43aeb4617085160f4dc7ad495d1119f39bca441276b1990123fd3ecb8a40a360"

testRegisterCalldataWithRecords :: TVar ChaChaDRG -> IO ()
testRegisterCalldataWithRecords g = do
  intent <- knownIntent g withRecords
  toHex (registerWithCreditCalldata intent)
    `shouldBe` "88596f95000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000f4865700000000000000000000000000000000000000000000000000000000000000046000000000000000000000000000000000000000000000000000000000000001000000000000000000000000009858effd232b4033e47d90003d41ec34ecaeda940000000000000000000000000000000000000000000000000000000003c2670011111111111111111111111111111111111111111111111111111111111111110000000000000000000000002279b7a0a67db372996a5fab50d91eaa73d2ebe6000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000762616b657279310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000016000000000000000000000000000000000000000000000000000000000000000e410f13a8c58c186cadf476923d25ea4950487d1aaf21906ba5574c87f5b9435b61b994370000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e636f6e746163740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002068747470733a2f2f736d702e73696d706c65782e696d2f6123636f6e7461637400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010410f13a8c58c186cadf476923d25ea4950487d1aaf21906ba5574c87f5b9435b61b994370000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e6368616e6e656c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003b68747470733a2f2f736d70312e73696d706c65782e696d2f63236f6e653b68747470733a2f2f736d70322e73696d706c65782e696d2f632374776f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000412de2e4a88b03d3f6ea59855699ac50d2c19ecf8ca705203ef407d3604bc585b5012ce66b1c22eced32a151acdfc3dab92789477caf68c9d5ea4a76923ad01b321b00000000000000000000000000000000000000000000000000000000000000"

testRegisterCalldataWithoutRecords :: TVar ChaChaDRG -> IO ()
testRegisterCalldataWithoutRecords g = do
  intent <- knownIntent g withRecords {rContactLinks = [], rChannelLinks = []}
  toHex (registerWithCreditCalldata intent)
    `shouldBe` "88596f95000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000f486570000000000000000000000000000000000000000000000000000000000000001c000000000000000000000000000000000000000000000000000000000000001000000000000000000000000009858effd232b4033e47d90003d41ec34ecaeda940000000000000000000000000000000000000000000000000000000003c2670011111111111111111111111111111111111111111111111111111111111111110000000000000000000000002279b7a0a67db372996a5fab50d91eaa73d2ebe6000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000762616b6572793100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000419c00341dd00caef7f36d04bdc23c033b7729787f3e4cb0768f27bbaa1eb86ec7622cc03ec147bc37efb953cf2029381ec399e1c393e5aefa44f6ca5d4c6995851b00000000000000000000000000000000000000000000000000000000000000"

testCommitCalldata :: IO ()
testCommitCalldata =
  toHex (commitCalldata $ B.replicate 32 0x22) `shouldBe` "f14fcbc82222222222222222222222222222222222222222222222222222222222222222"

testVerifyOtherSigner :: TVar ChaChaDRG -> IO ()
testVerifyOtherSigner g = do
  other <- newEphemeralWallet g
  intent <- right <$> signRegisterIntent g domain other withRecords deadline
  (isLeft <$> verifyRegisterIntent domain beforeDeadline intent) `shouldReturn` True

testVerifyExpired :: TVar ChaChaDRG -> IO ()
testVerifyExpired g = do
  intent <- knownIntent g withRecords
  (isLeft <$> verifyRegisterIntent domain (deadline + 1) intent) `shouldReturn` True

testVerifyShortSecret :: TVar ChaChaDRG -> IO ()
testVerifyShortSecret g = do
  intent <- knownIntent g withRecords {rSecret = B.replicate 31 0x11}
  (isLeft <$> verifyRegisterIntent domain beforeDeadline intent) `shouldReturn` True

testRevertedDryRun :: TVar ChaChaDRG -> IO ()
testRevertedDryRun g = withFakeEthNode $ \node -> do
  revertCalls node "reverted"
  intent <- knownIntent g withRecords
  sendRegister g (chainClient node) intent `shouldReturn` Left (CEReverted "reverted")
  sentTransactions node `shouldReturn` []

testCommitThenRegister :: TVar ChaChaDRG -> IO ()
testCommitThenRegister g = withFakeEthNode $ \node -> do
  w <- newEphemeralWallet g
  (fmap ewAddress <$> walletFromCode g (redemptionCode w)) `shouldReturn` Right (ewAddress w)
  secret <- atomically $ C.randomBytes 32 g
  let registration = withRecords {rOwner = ewAddress w, rSecret = secret}
      commitment = makeCommitment registration
  commitTx <- expectedTransaction g 0 controller $ commitCalldata commitment
  sendCommit g (chainClient node) commitment `shouldReturn` Right (TxHash $ C.keccak256 commitTx)
  intent <- right <$> signRegisterIntent g domain w registration deadline
  verifyRegisterIntent domain beforeDeadline intent `shouldReturn` Right ()
  registerTx <- expectedTransaction g 1 sponsor $ registerWithCreditCalldata intent
  sendRegister g (chainClient node) intent `shouldReturn` Right (TxHash $ C.keccak256 registerTx)
  sentTransactions node `shouldReturn` [commitTx, registerTx]

-- fixtures

withRecords :: Registration
withRecords =
  Registration
    { rLabel = "bakery1",
      rTLD = TLDSimplex,
      rOwner = address "0x9858EfFD232B4033E47d90003D41EC34EcaEda94",
      rDuration = 63072000,
      rSecret = B.replicate 32 0x11,
      rResolver = address "0x2279b7a0a67db372996a5fab50d91eaa73d2ebe6",
      rContactLinks = ["https://smp.simplex.im/a#contact"],
      rChannelLinks = ["https://smp1.simplex.im/c#one", "https://smp2.simplex.im/c#two"]
    }

knownIntent :: TVar ChaChaDRG -> Registration -> IO RegisterIntent
knownIntent g r = do
  w <- right <$> walletFromCode g knownCode
  right <$> signRegisterIntent g domain w r deadline

knownCode :: Text
knownCode = "SN-00000-00000-00000-00000-00000-0VME2"

controller, sponsor :: Address
controller = address "0x0165878a594ca255338adfa4d48449f69242eb8f"
sponsor = address "0x9a676e781a523b5d0c0e43731313a708cb607508"

domain :: Eip712Domain
domain = sponsorDomain chainId sponsor

chainId :: Word64
chainId = 31337

deadline, beforeDeadline :: Word64
deadline = 4102444800
beforeDeadline = 1800000000

registrarKey :: S.Secp256k1PrivateKey
registrarKey = right $ S.mkPrivateKey $ BA.replicate 32 0x46

chainClient :: FakeEthNode -> ChainClient
chainClient FakeEthNode {fnUrl, fnManager} =
  ChainClient {ccManager = fnManager, ccRpcUrl = fnUrl, ccChainId = chainId, ccController = controller, ccSponsor = sponsor, ccRegistrarKey = registrarKey}

expectedTransaction :: TVar ChaChaDRG -> Word64 -> Address -> ByteString -> IO ByteString
expectedTransaction g nonce to calldata =
  right
    <$> signEip1559Tx
      g
      registrarKey
      Eip1559Tx
        { txChainId = chainId,
          txNonce = nonce,
          txMaxPriorityFeePerGas = fromInteger fakeTip,
          txMaxFeePerGas = fromInteger $ 2 * fakeBaseFee + fakeTip,
          txGasLimit = fromInteger fakeGas,
          txTo = to,
          txValue = 0,
          txData = calldata
        }

address :: ByteString -> Address
address = right . strDecode

toHex :: ByteString -> ByteString
toHex = convertToBase Base16

right :: Either String a -> a
right = either (error . ("unexpected Left: " <>)) id
