{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeService.NameRegistrationTests (badgeNameRegistrationTests) where

import BadgeService.Names.Chain
import BadgeService.Names.Registration
import Bots.BadgeService.FakeEthNode
import Bots.BadgeService.WebUIMock
import Control.Concurrent.STM (TVar, atomically)
import Control.Monad (forM_)
import Crypto.Random (ChaChaDRG)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word64)
import qualified Simplex.Messaging.Crypto as C
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain)
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..), signEip1559Tx)
import Simplex.Messaging.SimplexName (SimplexTLD (..))
import Test.Hspec

-- expected encodings, digests and signatures are viem's, against the SimplexController, SimplexSponsor and SimplexResolver ABIs
badgeNameRegistrationTests :: Spec
badgeNameRegistrationTests = describe "badge service name registration" $ do
  g <- runIO C.newRandom
  describe "encoding" $ do
    it "computes the commitment of a registration with records" testCommitmentWithRecords
    it "computes the commitment of a registration without records" testCommitmentWithoutRecords
    it "encodes registerWithCredit with records" $ testRegisterCalldataWithRecords g
    it "encodes registerWithCredit without records" $ testRegisterCalldataWithoutRecords g
    it "encodes commit" testCommitCalldata
  describe "web UI mock" $ do
    it "derives the wallet of a known redemption code" $ testKnownCode g
    it "recovers a new wallet from its redemption code" $ testCodeRoundTrip g
    it "refuses a malformed redemption code" $ testMalformedCode g
    it "signs the register intent as viem does" $ testSignIntent g
  describe "verifying the register intent" $ do
    it "accepts an intent signed by the owner" $ testVerifyAccepts g
    it "refuses an intent signed by another wallet" $ testVerifyOtherSigner g
    it "refuses an intent past its deadline" $ testVerifyExpired g
    it "refuses an intent signed for another chain" $ testVerifyOtherChain g
    it "refuses a secret that is not 32 bytes" $ testVerifyShortSecret g
  describe "sending transactions" $ do
    it "sends a commit after a dry run" $ testSendCommit g
    it "sends a registration after a dry run" $ testSendRegister g
    it "takes the next nonce for the next transaction" $ testNextNonce g
    it "sends nothing when the dry run reverts, and reports a taken name" $ testTakenName g
    it "reports an unknown revert with its data" $ testUnknownRevert g
    it "reports a node error that carries no revert data" $ testNodeError g
    it "reports an unreachable node" $ testUnreachableNode g
  it "registers a name the client committed to without revealing it" $ testCommitThenRegister g

testCommitmentWithRecords :: IO ()
testCommitmentWithRecords =
  toHex (makeCommitment TLDSimplex withRecords) `shouldBe` "3db21bcf81cbd618087a5de5663806b4183a9594de3432e99c1397baa0d42388"

testCommitmentWithoutRecords :: IO ()
testCommitmentWithoutRecords =
  toHex (makeCommitment TLDSimplex withoutRecords) `shouldBe` "2ad83879b550f411f866b54e40cb295faa985452a00988a5a5a8467a5e526a56"

testRegisterCalldataWithRecords :: TVar ChaChaDRG -> IO ()
testRegisterCalldataWithRecords g = do
  intent <- knownIntent g withRecords
  toHex (registerWithCreditCalldata TLDSimplex intent)
    `shouldBe` "88596f95000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000f4865700000000000000000000000000000000000000000000000000000000000000046000000000000000000000000000000000000000000000000000000000000001000000000000000000000000009858effd232b4033e47d90003d41ec34ecaeda940000000000000000000000000000000000000000000000000000000003c2670011111111111111111111111111111111111111111111111111111111111111110000000000000000000000002279b7a0a67db372996a5fab50d91eaa73d2ebe6000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000762616b657279310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000016000000000000000000000000000000000000000000000000000000000000000e410f13a8c58c186cadf476923d25ea4950487d1aaf21906ba5574c87f5b9435b61b994370000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e636f6e746163740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002068747470733a2f2f736d702e73696d706c65782e696d2f6123636f6e7461637400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010410f13a8c58c186cadf476923d25ea4950487d1aaf21906ba5574c87f5b9435b61b994370000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e6368616e6e656c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003b68747470733a2f2f736d70312e73696d706c65782e696d2f63236f6e652c68747470733a2f2f736d70322e73696d706c65782e696d2f632374776f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000416f3523d47b84472a87fb202747dd95453c5c0d89257a65fd8f083da8da55c90b6ffa608c3b7365f127e2a5b3f6c3abde911976f3fb5593041a2facc6f2c8499f1c00000000000000000000000000000000000000000000000000000000000000"

testRegisterCalldataWithoutRecords :: TVar ChaChaDRG -> IO ()
testRegisterCalldataWithoutRecords g = do
  intent <- knownIntent g withoutRecords
  toHex (registerWithCreditCalldata TLDSimplex intent)
    `shouldBe` "88596f95000000000000000000000000000000000000000000000000000000000000006000000000000000000000000000000000000000000000000000000000f486570000000000000000000000000000000000000000000000000000000000000001c000000000000000000000000000000000000000000000000000000000000001000000000000000000000000009858effd232b4033e47d90003d41ec34ecaeda940000000000000000000000000000000000000000000000000000000003c2670011111111111111111111111111111111111111111111111111111111111111110000000000000000000000002279b7a0a67db372996a5fab50d91eaa73d2ebe6000000000000000000000000000000000000000000000000000000000000014000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000762616b6572793100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000419c00341dd00caef7f36d04bdc23c033b7729787f3e4cb0768f27bbaa1eb86ec7622cc03ec147bc37efb953cf2029381ec399e1c393e5aefa44f6ca5d4c6995851b00000000000000000000000000000000000000000000000000000000000000"

testCommitCalldata :: IO ()
testCommitCalldata =
  toHex (commitCalldata $ B.replicate 32 0x22) `shouldBe` "f14fcbc82222222222222222222222222222222222222222222222222222222222222222"

testKnownCode :: TVar ChaChaDRG -> IO ()
testKnownCode g = do
  w <- knownWallet g
  ewAddress w `shouldBe` knownOwner
  redemptionCode w `shouldBe` knownCode

testCodeRoundTrip :: TVar ChaChaDRG -> IO ()
testCodeRoundTrip g = do
  w <- newEphemeralWallet g
  (fmap ewAddress <$> walletFromCode g (redemptionCode w)) `shouldReturn` Right (ewAddress w)

testMalformedCode :: TVar ChaChaDRG -> IO ()
testMalformedCode g =
  forM_ [T.replace "0VME2" "0VME3" knownCode, T.dropEnd 1 knownCode, T.drop 3 knownCode] $ \code ->
    (isLeft <$> walletFromCode g code) `shouldReturn` True

testSignIntent :: TVar ChaChaDRG -> IO ()
testSignIntent g = do
  w <- knownWallet g
  RegisterIntent {riSignature} <- right <$> signRegisterIntent g TLDSimplex domain w withRecords deadline
  riSignature
    `shouldBe` S.RecoverableSignature
      { S.rsR = hx "6f3523d47b84472a87fb202747dd95453c5c0d89257a65fd8f083da8da55c90b",
        S.rsS = hx "6ffa608c3b7365f127e2a5b3f6c3abde911976f3fb5593041a2facc6f2c8499f",
        S.rsRecId = 1
      }

testVerifyAccepts :: TVar ChaChaDRG -> IO ()
testVerifyAccepts g = do
  w <- knownWallet g
  intent <- right <$> signRegisterIntent g TLDSimplex domain w withRecords deadline
  (fmap (makeCommitment TLDSimplex) <$> verifyRegisterIntent TLDSimplex domain beforeDeadline intent)
    `shouldReturn` Right (makeCommitment TLDSimplex withRecords)

testVerifyOtherSigner :: TVar ChaChaDRG -> IO ()
testVerifyOtherSigner g = do
  other <- newEphemeralWallet g
  intent <- right <$> signRegisterIntent g TLDSimplex domain other withRecords deadline
  (isLeft <$> verifyRegisterIntent TLDSimplex domain beforeDeadline intent) `shouldReturn` True

testVerifyExpired :: TVar ChaChaDRG -> IO ()
testVerifyExpired g = do
  w <- knownWallet g
  intent <- right <$> signRegisterIntent g TLDSimplex domain w withRecords deadline
  (isLeft <$> verifyRegisterIntent TLDSimplex domain (deadline + 1) intent) `shouldReturn` True

testVerifyOtherChain :: TVar ChaChaDRG -> IO ()
testVerifyOtherChain g = do
  w <- knownWallet g
  intent <- right <$> signRegisterIntent g TLDSimplex (sponsorDomain 1 sponsor) w withRecords deadline
  (isLeft <$> verifyRegisterIntent TLDSimplex domain beforeDeadline intent) `shouldReturn` True

testVerifyShortSecret :: TVar ChaChaDRG -> IO ()
testVerifyShortSecret g = do
  w <- knownWallet g
  intent <- right <$> signRegisterIntent g TLDSimplex domain w withRecords {rSecret = B.replicate 31 0x11} deadline
  (isLeft <$> verifyRegisterIntent TLDSimplex domain beforeDeadline intent) `shouldReturn` True

testSendCommit :: TVar ChaChaDRG -> IO ()
testSendCommit g = withFakeEthNode $ \node -> do
  let calldata = commitCalldata commitment
  raw <- expectedTransaction g 0 controller calldata
  sendCommit g (chainClient node) commitment `shouldReturn` Right (TxHash $ C.keccak256 raw)
  sentTransactions node `shouldReturn` [raw]
  requestedMethods node `shouldReturn` sendMethods
  where
    commitment = makeCommitment TLDSimplex withRecords

testSendRegister :: TVar ChaChaDRG -> IO ()
testSendRegister g = withFakeEthNode $ \node -> do
  intent <- knownIntent g withRecords
  raw <- expectedTransaction g 0 sponsor $ registerWithCreditCalldata TLDSimplex intent
  sendRegister g (chainClient node) TLDSimplex intent `shouldReturn` Right (TxHash $ C.keccak256 raw)
  sentTransactions node `shouldReturn` [raw]
  requestedMethods node `shouldReturn` sendMethods

testNextNonce :: TVar ChaChaDRG -> IO ()
testNextNonce g = withFakeEthNode $ \node -> do
  let commitment = makeCommitment TLDSimplex withRecords
  intent <- knownIntent g withRecords
  raw0 <- expectedTransaction g 0 controller $ commitCalldata commitment
  raw1 <- expectedTransaction g 1 sponsor $ registerWithCreditCalldata TLDSimplex intent
  _ <- sendCommit g (chainClient node) commitment
  _ <- sendRegister g (chainClient node) TLDSimplex intent
  sentTransactions node `shouldReturn` [raw0, raw1]

testTakenName :: TVar ChaChaDRG -> IO ()
testTakenName g = withFakeEthNode $ \node -> do
  revertCalls node $ selector "NameNotAvailable(string)" <> B.replicate 96 0
  intent <- knownIntent g withRecords
  sendRegister g (chainClient node) TLDSimplex intent `shouldReturn` Left (CEReverted NameNotAvailable)
  sentTransactions node `shouldReturn` []
  requestedMethods node `shouldReturn` ["eth_call"]

testUnknownRevert :: TVar ChaChaDRG -> IO ()
testUnknownRevert g = withFakeEthNode $ \node -> do
  let revert = selector "NameReserved(string)" <> B.replicate 96 0
  revertCalls node revert
  intent <- knownIntent g withRecords
  sendRegister g (chainClient node) TLDSimplex intent `shouldReturn` Left (CEReverted $ UnknownRevert revert)
  sentTransactions node `shouldReturn` []

testNodeError :: TVar ChaChaDRG -> IO ()
testNodeError g = withFakeEthNode $ \node -> do
  failCalls node "Internal error"
  intent <- knownIntent g withRecords
  sendRegister g (chainClient node) TLDSimplex intent `shouldReturn` Left (CERpc "Internal error")
  sentTransactions node `shouldReturn` []

testUnreachableNode :: TVar ChaChaDRG -> IO ()
testUnreachableNode g = withFakeEthNode $ \node -> do
  r <- sendCommit g (chainClient node) {ccRpcUrl = "http://127.0.0.1:1"} $ B.replicate 32 0x22
  r `shouldSatisfy` \case
    Left (CERpc _) -> True
    _ -> False
  sentTransactions node `shouldReturn` []

testCommitThenRegister :: TVar ChaChaDRG -> IO ()
testCommitThenRegister g = withFakeEthNode $ \node -> do
  w <- newEphemeralWallet g
  secret <- atomically $ C.randomBytes 32 g
  let registration = withRecords {rOwner = ewAddress w, rSecret = secret}
      commitment = makeCommitment TLDSimplex registration
  (isRight <$> sendCommit g (chainClient node) commitment) `shouldReturn` True
  intent <- right <$> signRegisterIntent g TLDSimplex domain w registration deadline
  verified <- right <$> verifyRegisterIntent TLDSimplex domain beforeDeadline intent
  makeCommitment TLDSimplex verified `shouldBe` commitment
  (isRight <$> sendRegister g (chainClient node) TLDSimplex intent) `shouldReturn` True
  commitTx <- expectedTransaction g 0 controller $ commitCalldata commitment
  registerTx <- expectedTransaction g 1 sponsor $ registerWithCreditCalldata TLDSimplex intent
  sentTransactions node `shouldReturn` [commitTx, registerTx]
  encodeUtf8 (rLabel registration) `B.isInfixOf` commitTx `shouldBe` False

-- fixtures

withRecords :: Registration
withRecords =
  Registration
    { rLabel = "bakery1",
      rOwner = knownOwner,
      rDuration = 63072000,
      rSecret = B.replicate 32 0x11,
      rResolver = address "0x2279b7a0a67db372996a5fab50d91eaa73d2ebe6",
      rContactLinks = ["https://smp.simplex.im/a#contact"],
      rChannelLinks = ["https://smp1.simplex.im/c#one", "https://smp2.simplex.im/c#two"]
    }

withoutRecords :: Registration
withoutRecords = withRecords {rContactLinks = [], rChannelLinks = []}

knownIntent :: TVar ChaChaDRG -> Registration -> IO RegisterIntent
knownIntent g r = do
  w <- knownWallet g
  right <$> signRegisterIntent g TLDSimplex domain w r deadline

knownWallet :: TVar ChaChaDRG -> IO EphemeralWallet
knownWallet g = right <$> walletFromCode g knownCode

knownCode :: Text
knownCode = "SN-00000-00000-00000-00000-00000-0VME2"

knownOwner :: Address
knownOwner = address "0x9858EfFD232B4033E47d90003D41EC34EcaEda94"

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

sendMethods :: [Text]
sendMethods = ["eth_call", "eth_estimateGas", "eth_getTransactionCount", "eth_maxPriorityFeePerGas", "eth_getBlockByNumber", "eth_sendRawTransaction"]

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

hx :: ByteString -> ByteString
hx = right . convertFromBase Base16

toHex :: ByteString -> ByteString
toHex = convertToBase Base16

right :: Either String a -> a
right = either (error . ("unexpected Left: " <>)) id
