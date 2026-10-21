{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesChainTests (namesChainTests) where

import Bots.StoreService.FakeEthNode
import Control.Concurrent.STM (TVar)
import Crypto.Random (ChaChaDRG)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import qualified Simplex.Messaging.Crypto as C
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Abi (selector, uint256)
import StoreService.Names.Chain
import StoreService.Names.Revert
import Test.Hspec

namesChainTests :: Spec
namesChainTests = describe "StoreService names: chain" $ do
  g <- runIO C.newRandom
  describe "reverts" $ do
    it "names every contract error by the selector viem encodes it with" testRevertSelectors
    it "reads a dry run's revert with data by its name" $ testDryRunRevertNamed g
    it "reads an eth_call revert with data by its name" $ testCallRevertNamed g
    it "reads ownerOf of a name nobody holds as a revert" $ testOwnerOfRevert g
    it "reads a revert without code 3 or data as a revert" $ testRevertWithoutData g
    it "reads a revert of an unknown error as an unnamed revert" $ testUnknownRevert g
  describe "failures" $ do
    it "reads an RPC error as unavailable, without the URL" $ testRpcError g
    it "gives up on a node that does not answer within the call's timeout" $ testTimeout g
    it "reads an HTTP failure as unavailable" $ testHttpFailure g
    it "reads a refused connection as unavailable, without the URL" $ testRefused g
    it "refuses an answer over the size bound" $ testOversized g
  describe "reads" $ do
    it "answers eth_call from the node by contract and calldata" $ testEthCall g
    it "reads the latest block's number, time and base fee" $ testLatestBlock g
    it "follows a transaction from sent to mined with its receipt and the nonces" $ testFollowTransaction g
  describe "transactions" $ do
    it "signs a transaction whose hash is the node's keccak256 of the raw bytes" $ testSignedHash g
    it "refuses a gas estimate above the cap" $ testGasCap g

-- | The custom errors of the four contracts' compiled ABIs, with viem's encodeErrorResult selectors.
viemSelectors :: [(ByteString, RevertName)]
viemSelectors =
  [ ("5289653e", RNAlreadyFrozen),
    ("836588c9", RNCommitmentNotFound),
    ("74480cc9", RNCommitmentTooNew),
    ("256e2216", RNCommitmentTooOld),
    ("9a71997b", RNDurationTooShort),
    ("a8cab3d1", RNFrozen),
    ("2a1b2dd8", RNInsufficientAllowance),
    ("11011294", RNInsufficientValue),
    ("8d666f60", RNInvalidEVMAddress),
    ("756688fe", RNInvalidNonce),
    ("8baa579f", RNInvalidSignature),
    ("21134988", RNLabelTooLong),
    ("2d0c6794", RNMaxCommitmentAgeTooHigh),
    ("3e5aa838", RNMaxCommitmentAgeTooLow),
    ("ace8f2ce", RNMinCharLengthCanOnlyDecrease),
    ("ce47b48d", RNMinCharLengthZero),
    ("6357b884", RNMinCommitmentAgeZero),
    ("477707e8", RNNameNotAvailable),
    ("d17ee357", RNNameNotReserved),
    ("e1bf11bf", RNNameReserved),
    ("c4295eb6", RNNameTooShort),
    ("70f8a4c9", RNNftGateCanOnlyBeDisabled),
    ("6b1578c1", RNNftRequired),
    ("c30734cc", RNNodeNotOwnedByRegistrar),
    ("644d871f", RNNotBeneficiary),
    ("42f058b4", RNNotNameOwner),
    ("ff43b88f", RNNotOffered),
    ("4632a8bf", RNNotOwnerOrBeneficiary),
    ("df4795c7", RNNotSponsor),
    ("9a0aeacd", RNNotSubnameRegistrar),
    ("8a3c1cfb", RNOffsetOutOfBoundsError),
    ("a9efcc18", RNOwnerMustBeCaller),
    ("40866499", RNPublicSalesClosed),
    ("bc7fd331", RNReasonRequired),
    ("2bea818f", RNRecipientMustBeCaller),
    ("37ed32e8", RNReentrantCall),
    ("d3f605c4", RNResolverRequiredWhenDataSupplied),
    ("27af646c", RNReverseRecordNotSupported),
    ("0819bdcd", RNSignatureExpired),
    ("90b8ec18", RNTransferFailed),
    ("bf2f44f5", RNTransferToSelf),
    ("0a059d71", RNUnexpiredCommitmentExists),
    ("d92e233d", RNZeroAddress)
  ]

testRevertSelectors :: IO ()
testRevertSelectors = do
  map snd viemSelectors `shouldBe` [minBound .. maxBound]
  [(n, decodeRevert $ unhex sel <> uint256 (0 :: Int)) | (sel, n) <- viemSelectors] `shouldBe` [(n, Just n) | (_, n) <- viemSelectors]
  decodeRevert (B.take 3 $ unhex "74480cc9") `shouldBe` Nothing

testDryRunRevertNamed :: TVar ChaChaDRG -> IO ()
testDryRunRevertNamed g = withChain g $ \node c -> do
  setFault node "eth_estimateGas" $ revertFault insufficientAllowance
  estimateGas c fakeSponsor "\x01\x02\x03\x04" `shouldReturn` Left (ChainReverted $ Just RNInsufficientAllowance)
  sentTransactions node `shouldReturn` []

testCallRevertNamed :: TVar ChaChaDRG -> IO ()
testCallRevertNamed g = withChain g $ \node c -> do
  answerCall node fakeController availableCall $ Reverts nameNotAvailable
  ethCall c fakeController availableCall `shouldReturn` Left (ChainReverted $ Just RNNameNotAvailable)

testOwnerOfRevert :: TVar ChaChaDRG -> IO ()
testOwnerOfRevert g = withChain g $ \node c@ChainClient {ccConfig = ChainConfig {cBaseRegistrar}} -> do
  let ownerOf = selector "ownerOf(uint256)" <> C.keccak256 "bakery1"
  answerCall node cBaseRegistrar ownerOf $ Reverts ""
  ethCall c cBaseRegistrar ownerOf `shouldReturn` Left (ChainReverted Nothing)

testRevertWithoutData :: TVar ChaChaDRG -> IO ()
testRevertWithoutData g = withChain g $ \node c -> do
  setFault node "eth_call" $ RpcError (-32000) "execution reverted" Nothing
  ethCall c fakeController availableCall `shouldReturn` Left (ChainReverted Nothing)

testUnknownRevert :: TVar ChaChaDRG -> IO ()
testUnknownRevert g = withChain g $ \node c -> do
  -- viem's encodeErrorResult of Error("ERC721: invalid token ID"), not one of the contracts' own errors
  setFault node "eth_call" $ revertFault $ unhex "08c379a0000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000184552433732313a20696e76616c696420746f6b656e2049440000000000000000"
  ethCall c fakeController availableCall `shouldReturn` Left (ChainReverted Nothing)

testRpcError :: TVar ChaChaDRG -> IO ()
testRpcError g = withChain g $ \node c -> do
  setFault node "eth_call" $ RpcError (-32000) "header not found" Nothing
  r <- ethCall c fakeController availableCall
  r `shouldBe` Left (ChainUnavailable "eth_call: error -32000: header not found")
  r `shouldNotSatisfy` mentionsUrl c

testTimeout :: TVar ChaChaDRG -> IO ()
testTimeout g = withChain g $ \node c -> do
  ccTimeout c `shouldBe` 10000000
  setFault node "eth_getBlockByNumber" Hang
  let short = c {ccTimeout = 300000}
  start <- getCurrentTime
  r <- latestBlock short
  elapsed <- (`diffUTCTime` start) <$> getCurrentTime
  r `shouldBe` Left (ChainUnavailable "eth_getBlockByNumber: timed out")
  elapsed `shouldSatisfy` (< 2)

testHttpFailure :: TVar ChaChaDRG -> IO ()
testHttpFailure g = withChain g $ \node c -> do
  setFault node "eth_maxPriorityFeePerGas" HttpFailure
  maxPriorityFee c `shouldReturn` Left (ChainUnavailable "eth_maxPriorityFeePerGas: HTTP 502")

testRefused :: TVar ChaChaDRG -> IO ()
testRefused g = withFakeEthNode $ \node -> do
  -- port 1 is privileged and nothing listens on it in the test container
  c <- newChainClient g (fakeChainConfig node) {cRpcUrl = "http://127.0.0.1:1" <> secretPath}
  r <- latestBlock c
  r `shouldBe` Left (ChainUnavailable "eth_getBlockByNumber: connection failed")
  r `shouldNotSatisfy` mentionsUrl c

testOversized :: TVar ChaChaDRG -> IO ()
testOversized g = withChain g $ \node c -> do
  answerCall node fakeController availableCall $ Returns $ B.replicate (512 * 1024) 0
  ethCall c fakeController availableCall `shouldReturn` Left (ChainUnavailable "eth_call: response over 1048576 bytes")

testEthCall :: TVar ChaChaDRG -> IO ()
testEthCall g = withChain g $ \node c -> do
  answerCall node fakeController availableCall $ Returns $ uint256 (1 :: Int)
  answerCall node fakeSponsor availableCall $ Returns $ uint256 (0 :: Int)
  ethCall c fakeController availableCall `shouldReturn` Right (uint256 (1 :: Int))
  ethCall c fakeSponsor availableCall `shouldReturn` Right (uint256 (0 :: Int))
  ethCall c fakeController (selector "available(string)") `shouldReturn` Left (ChainUnavailable "eth_call: error -32000: no fake answer for this call")

testLatestBlock :: TVar ChaChaDRG -> IO ()
testLatestBlock g = withChain g $ \node c -> do
  setBlock node 21000000 1798761600
  latestBlock c `shouldReturn` Right Block {blockNumber = 21000000, blockTimestamp = 1798761600, blockBaseFee = fakeBaseFee}

testFollowTransaction :: TVar ChaChaDRG -> IO ()
testFollowTransaction g = withChain g $ \node c@ChainClient {ccRegistrar} -> do
  let unknown = TxHash $ B.replicate 32 0xee
  transactionByHash c unknown `shouldReturn` Right Nothing
  transactionReceipt c unknown `shouldReturn` Right Nothing
  SignedTx {stRaw, stHash = hash@(TxHash h)} <- signed g c 0
  sendRawTransaction c stRaw `shouldReturn` Right hash
  transactionByHash c hash `shouldReturn` Right (Just TxPending)
  transactionReceipt c hash `shouldReturn` Right Nothing
  transactionCount c ccRegistrar Latest `shouldReturn` Right 0
  transactionCount c ccRegistrar Pending `shouldReturn` Right 1
  block <- mine node [(h, False)]
  transactionByHash c hash `shouldReturn` Right (Just $ TxMined block)
  transactionReceipt c hash `shouldReturn` Right (Just Receipt {receiptSuccess = False, receiptBlock = block})
  transactionCount c ccRegistrar Latest `shouldReturn` Right 1

testSignedHash :: TVar ChaChaDRG -> IO ()
testSignedHash g = withChain g $ \node c -> do
  SignedTx {stRaw, stHash} <- signed g c 7
  stHash `shouldBe` TxHash (C.keccak256 stRaw)
  sendRawTransaction c stRaw `shouldReturn` Right stHash
  sentTransactions node `shouldReturn` [stRaw]

testGasCap :: TVar ChaChaDRG -> IO ()
testGasCap g = withFakeEthNode $ \node -> do
  let config = fakeChainConfig node
  atCap <- newChainClient g config {cGasLimitCap = fakeGas}
  estimateGas atCap fakeController availableCall `shouldReturn` Right fakeGas
  belowCap <- newChainClient g config {cGasLimitCap = fakeGas - 1}
  estimateGas belowCap fakeController availableCall `shouldReturn` Left (ChainGasAboveCap fakeGas)

-- fixtures

withChain :: TVar ChaChaDRG -> (FakeEthNode -> ChainClient -> IO a) -> IO a
withChain g action = withFakeEthNode $ \node -> do
  c <- newChainClient g (fakeChainConfig node) {cRpcUrl = fnUrl node <> secretPath}
  action node c

-- | Where a provider puts its API key.
secretPath :: String
secretPath = "/v2/provider-api-key"

mentionsUrl :: ChainClient -> Either ChainError a -> Bool
mentionsUrl ChainClient {ccConfig = ChainConfig {cRpcUrl}} = \case
  Left (ChainUnavailable t) -> any (`T.isInfixOf` t) [T.pack cRpcUrl, T.pack secretPath, "127.0.0.1"]
  _ -> False

signed :: TVar ChaChaDRG -> ChainClient -> Word64 -> IO SignedTx
signed g c nonce =
  either error id
    <$> signTransaction g c TxRequest {trTo = fakeController, trData = availableCall, trNonce = nonce, trGasLimit = fakeGas, trMaxFeePerGas = 2 * fakeBaseFee + fakeTip, trMaxPriorityFeePerGas = fakeTip}

-- | controller.available("bakery1"), viem's encodeFunctionData
availableCall :: ByteString
availableCall = unhex "aeb8ce9b0000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000762616b6572793100000000000000000000000000000000000000000000000000"

-- | viem's encodeErrorResult of InsufficientAllowance(10000, 2500)
insufficientAllowance :: ByteString
insufficientAllowance = unhex "2a1b2dd8000000000000000000000000000000000000000000000000000000000000271000000000000000000000000000000000000000000000000000000000000009c4"

-- | viem's encodeErrorResult of NameNotAvailable("bakery1")
nameNotAvailable :: ByteString
nameNotAvailable = unhex "477707e80000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000000762616b6572793100000000000000000000000000000000000000000000000000"

unhex :: ByteString -> ByteString
unhex = either error id . convertFromBase Base16
