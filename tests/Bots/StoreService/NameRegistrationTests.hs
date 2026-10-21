{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NameRegistrationTests (badgeNameRegistrationTests) where

import StoreService.Names.Chain
import StoreService.Names.Registration
import Bots.StoreService.FakeEthNode
import Bots.StoreService.WebUIMock
import Control.Concurrent.STM (TVar)
import Crypto.Random (ChaChaDRG)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
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

badgeNameRegistrationTests :: Spec
badgeNameRegistrationTests = describe "store service name registration" $ do
  g <- runIO C.newRandom
  -- expected encodings and signatures are viem's, against the SimplexController, SimplexSponsor and SimplexResolver ABIs
  describe "encoding" $ do
    it "encodes registerWithCredit with records" $ testRegisterCalldataWithRecords g
    it "encodes registerWithCredit without records" $ testRegisterCalldataWithoutRecords g
    it "encodes commit with the commitment of a registration" testCommitCalldata
  describe "verifying the register intent" $ do
    it "rejects an intent signed by another wallet" $ testVerifyOtherSigner g
    it "rejects an intent past its deadline" $ testVerifyExpired g
    it "rejects a secret that is not 32 bytes" $ testVerifyShortSecret g
  it "sends nothing when the dry run reverts" $ testRevertedDryRun g
  it "registers a name the client committed to without revealing it" $ testCommitThenRegister g

testRegisterCalldataWithRecords :: TVar ChaChaDRG -> IO ()
testRegisterCalldataWithRecords g = do
  intent <- knownIntent g knownRegistration
  toHex (registerWithCreditCalldata intent)
    `shouldBe` "c4bd2ab50000000000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f4865700000000000000000000000000000000000000000000000000000000000000046000000000000000000000000000000000000000000000000000000000000001000000000000000000000000009858effd232b4033e47d90003d41ec34ecaeda940000000000000000000000000000000000000000000000000000000003c2670011111111111111111111111111111111111111111111111111111111111111110000000000000000000000002279b7a0a67db372996a5fab50d91eaa73d2ebe6000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000762616b657279310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000016000000000000000000000000000000000000000000000000000000000000000e410f13a8c58c186cadf476923d25ea4950487d1aaf21906ba5574c87f5b9435b61b994370000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e636f6e746163740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002068747470733a2f2f736d702e73696d706c65782e696d2f6123636f6e746163740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000e410f13a8c58c186cadf476923d25ea4950487d1aaf21906ba5574c87f5b9435b61b994370000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e6368616e6e656c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001d68747470733a2f2f736d70312e73696d706c65782e696d2f63236f6e65000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000041f310b273d0e0ab0975a66d383fe818a5ec3474fcd922dbf3fe70094a3c0ceae86559b13ea0728a330613c6fe236a89a026daf345dcc8c0616e9eda2c69cbeb091b00000000000000000000000000000000000000000000000000000000000000"

testRegisterCalldataWithoutRecords :: TVar ChaChaDRG -> IO ()
testRegisterCalldataWithoutRecords g = do
  intent <- knownIntent g knownRegistration {rContactLink = Nothing, rChannelLink = Nothing}
  toHex (registerWithCreditCalldata intent)
    `shouldBe` "c4bd2ab50000000000000000000000000000000000000000000000000000000000000080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000f486570000000000000000000000000000000000000000000000000000000000000001e000000000000000000000000000000000000000000000000000000000000001000000000000000000000000009858effd232b4033e47d90003d41ec34ecaeda940000000000000000000000000000000000000000000000000000000003c2670011111111111111111111111111111111111111111111111111111111111111110000000000000000000000002279b7a0a67db372996a5fab50d91eaa73d2ebe6000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000762616b6572793100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000411d0b3a477c99fbcadada2937b71127bf6da202f5cd25856649829653d8cf6a8c0ce0afb8505e398e23b3b01fcb3b17ffcf82d8ba6d24e9b854506894b2864ecf1c00000000000000000000000000000000000000000000000000000000000000"

testCommitCalldata :: IO ()
testCommitCalldata =
  toHex (commitCalldata $ makeCommitment knownRegistration) `shouldBe` "f14fcbc8810f0645b8d9839e3744bb956f9da4fa12bb51ec97459e08739c576a36cba0df"

testVerifyOtherSigner :: TVar ChaChaDRG -> IO ()
testVerifyOtherSigner g = do
  other <- newEphemeralWallet g
  intent <- signRegisterIntent g domain other knownRegistration deadline
  verifyRegisterIntent domain deadline intent `shouldReturn` Left "intent is not signed by the owner"

testVerifyExpired :: TVar ChaChaDRG -> IO ()
testVerifyExpired g = do
  intent <- knownIntent g knownRegistration
  verifyRegisterIntent domain (deadline + 1) intent `shouldReturn` Left "intent expired"

testVerifyShortSecret :: TVar ChaChaDRG -> IO ()
testVerifyShortSecret g = do
  intent <- knownIntent g knownRegistration {rSecret = B.replicate 31 0x11}
  verifyRegisterIntent domain deadline intent `shouldReturn` Left "secret: expected 32 bytes"

testRevertedDryRun :: TVar ChaChaDRG -> IO ()
testRevertedDryRun g = withFakeEthNode $ \node -> do
  revertCalls node
  intent <- knownIntent g knownRegistration
  sendRegister g (chainClient node) intent `shouldReturn` Left (ChainError "execution reverted")
  sentTransactions node `shouldReturn` []

testCommitThenRegister :: TVar ChaChaDRG -> IO ()
testCommitThenRegister g = withFakeEthNode $ \node -> do
  w <- newEphemeralWallet g
  ewAddress <$> walletFromCode g (redemptionCode w) `shouldReturn` ewAddress w
  let registration = knownRegistration {rOwner = ewAddress w}
      commitment = makeCommitment registration
  commitTx <- expectedTransaction g 0 controller $ commitCalldata commitment
  sendCommit g (chainClient node) commitment `shouldReturn` Right (TxHash $ C.keccak256 commitTx)
  intent <- signRegisterIntent g domain w registration deadline
  verifyRegisterIntent domain deadline intent `shouldReturn` Right ()
  registerTx <- expectedTransaction g 1 sponsor $ registerWithCreditCalldata intent
  sendRegister g (chainClient node) intent `shouldReturn` Right (TxHash $ C.keccak256 registerTx)

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

controller, sponsor :: Address
controller = address "0x0165878a594ca255338adfa4d48449f69242eb8f"
sponsor = address "0x9a676e781a523b5d0c0e43731313a708cb607508"

domain :: Eip712Domain
domain = sponsorDomain chainId sponsor

chainId :: Word64
chainId = 31337

deadline :: Word64
deadline = 4102444800

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
          txMaxPriorityFeePerGas = fakeTip,
          txMaxFeePerGas = 2 * fakeBaseFee + fakeTip,
          txGasLimit = fakeGas,
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
