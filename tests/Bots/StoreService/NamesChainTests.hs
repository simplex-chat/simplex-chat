{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesChainTests (namesChainTests) where

import Bots.StoreService.FakeEthNode
import Control.Concurrent.STM (TVar)
import Control.Exception (toException, try)
import Control.Monad (forM_)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson as J
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import qualified Network.HTTP.Client as HTTP
import Simplex.Chat.Names.Abi (uint256)
import qualified Simplex.Messaging.Crypto as C
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Chain
import StoreService.Names.Revert
import Test.Hspec

namesChainTests :: Spec
namesChainTests = describe "StoreService names: chain" $ do
  g <- runIO C.newRandom
  describe "reverts" $ do
    it "names every contract error by the selector viem encodes it with" testRevertSelectors
    it "writes a revert's name as the contracts declare it, none for a revert without data" testRevertText
    it "reads a dry run's revert with data by its name" $ testDryRunRevertNamed g
  describe "failures" $ do
    it "reads eth_call faults: a bare revert as one without data, a revert reason as unknown, an RPC error without the URL, a 2xx answer not JSON without quoting it, and a long message cut" $ testCallFaults g
    it "gives up on a node that does not answer within the call's timeout" $ testTimeout g
    it "reads a refused connection as unavailable, without the URL" $ testRefused g
    it "refuses an answer over the size bound" $ testOversized g
    it "refuses a quantity over 256 bits, and a long one within the call's timeout" $ testLongQuantity g
    it "keeps an HTTP failure's detail for the log, with the URL's host, path and query redacted" testFailureDetail
  describe "reads" $ do
    it "reads a block by its number, and one the node does not have as a failed read" $ testBlockAt g
    it "follows a transaction from sent to mined with its receipt and the nonces" $ testFollowTransaction g
    it "reads the tip as the median of the last blocks' median tips, refusing a block without one reward" $ testRecentTip g
  describe "transactions" $ do
    it "signs a transaction whose hash is the node's keccak256 of the raw bytes" $ testSignedHash g
    it "refuses a gas estimate above the cap" $ testGasCap g

-- | The custom errors the service tells apart, from the contracts' compiled ABIs, with viem's encodeErrorResult selectors.
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
    ("c4f189e2", RNTermsMismatch),
    ("bcdf753d", RNTermsNotAccepted),
    ("90b8ec18", RNTransferFailed),
    ("bf2f44f5", RNTransferToSelf),
    ("0a059d71", RNUnexpiredCommitmentExists),
    ("d92e233d", RNZeroAddress)
  ]

testRevertSelectors :: IO ()
testRevertSelectors = do
  map snd viemSelectors `shouldBe` filter (/= RNWithoutData) [minBound .. maxBound]
  [(n, decodeRevert $ unhex sel <> uint256 (0 :: Int)) | (sel, n) <- viemSelectors] `shouldBe` [(n, Just n) | (_, n) <- viemSelectors]
  decodeRevert (B.take 3 $ unhex "74480cc9") `shouldBe` Nothing
  decodeRevert "" `shouldBe` Just RNWithoutData

testRevertText :: IO ()
testRevertText = do
  revertText RNInvalidNonce `shouldBe` Just "InvalidNonce"
  revertText RNSignatureExpired `shouldBe` Just "SignatureExpired"
  revertText RNWithoutData `shouldBe` Nothing

testDryRunRevertNamed :: TVar ChaChaDRG -> IO ()
testDryRunRevertNamed g = withChain g $ \node c -> do
  setFault node "eth_estimateGas" $ revertFault insufficientAllowance
  estimateGas c fakeSponsor "\x01\x02\x03\x04" `shouldReturn` Left (ChainReverted $ Just RNInsufficientAllowance)
  sentTransactions node `shouldReturn` []

-- | A reason kept in the message names a revert that had data, so it is not read as one without.
testCallFaults :: TVar ChaChaDRG -> IO ()
testCallFaults g = withChain g $ \node c ->
  forM_
    [ (RpcError (-32000) "execution reverted" Nothing, ChainReverted $ Just RNWithoutData),
      (RpcError 3 "execution reverted: ERC721: invalid token ID" Nothing, ChainReverted Nothing),
      (RpcError (-32000) "header not found" Nothing, ChainUnavailable "eth_call: error -32000: header not found"),
      (NotJson, ChainUnavailable "eth_call: not a JSON-RPC response"),
      (RpcError (-32000) (T.replicate 1000 "x") Nothing, ChainUnavailable $ "eth_call: error -32000: " <> T.replicate 200 "x")
    ]
    $ \(fault, err) -> do
      setFault node "eth_call" fault
      ethCallAt c Latest fakeController availableCall `shouldReturn` Left err

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

testRefused :: TVar ChaChaDRG -> IO ()
testRefused g = withFakeEthNode $ \node -> do
  -- port 1 is privileged and nothing listens on it in the test container
  c <- newChainClient g (fakeChainConfig node) {cRpcUrl = "http://127.0.0.1:1" <> secretPath}
  latestBlock c `shouldReturn` Left (ChainUnavailable "eth_getBlockByNumber: connection failed")

testOversized :: TVar ChaChaDRG -> IO ()
testOversized g = withChain g $ \node c -> do
  answerCall node fakeController availableCall $ Returns $ B.replicate (512 * 1024) 0
  ethCallAt c Latest fakeController availableCall `shouldReturn` Left (ChainUnavailable "eth_call: response over 1048576 bytes")

testRecentTip :: TVar ChaChaDRG -> IO ()
testRecentTip g = withChain g $ \node c -> do
  let history :: [[T.Text]] -> IO (Either ChainError Word64)
      history rewards = setFault node "eth_feeHistory" (Answers $ J.object ["reward" J..= rewards]) >> recentTip c
      malformed reason = Left (ChainUnavailable $ "eth_feeHistory: not a JSON-RPC response: Error in $: " <> reason)
  history [["0x5"], ["0x1"], ["0x3"]] `shouldReturn` Right 3
  history [["0x0"], ["0x9"], ["0x2"], ["0x7"]] `shouldReturn` Right 7
  history [["0x1", "0x2"]] `shouldReturn` malformed "not one reward a block"
  history [] `shouldReturn` malformed "no rewards"

testLongQuantity :: TVar ChaChaDRG -> IO ()
testLongQuantity g = withChain g $ \node c -> do
  let fee digits = setFault node "eth_getTransactionCount" (Answers $ J.String $ "0x" <> digits) >> transactionCount c {ccTimeout = 300000} (ccRegistrar c) Latest
      notQuantity = Left (ChainUnavailable "eth_getTransactionCount: not a JSON-RPC response: Error in $: not a quantity")
  fee (T.replicate 63 "0" <> "7") `shouldReturn` Right 7
  fee (T.replicate 64 "0" <> "7") `shouldReturn` notQuantity
  start <- getCurrentTime
  fee (T.replicate 1000000 "f") `shouldReturn` notQuantity
  elapsed <- (`diffUTCTime` start) <$> getCurrentTime
  elapsed `shouldSatisfy` (< 1)

testFailureDetail :: IO ()
testFailureDetail = do
  let url = "https://provider.example/v2/provider-api-key?key=abc"
  req <- HTTP.parseRequest url
  let detail = httpFailureDetail url . HTTP.HttpExceptionRequest req
      redacted t = (t, filter (`T.isInfixOf` t) ["provider.example", "provider-api-key", "key=abc"]) `shouldBe` (t, [])
  redacted $ detail $ HTTP.InvalidDestinationHost "provider.example"
  redacted $ detail $ HTTP.ConnectionFailure $ toException $ userError $ "connect to " <> url
  redacted $ httpFailureDetail url $ HTTP.InvalidUrlException url "Invalid URL"
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  -- port 1 is privileged and nothing listens on it in the test container
  let refusedUrl = "http://127.0.0.1:1" <> secretPath
  refused <- try $ HTTP.parseRequest refusedUrl >>= (`HTTP.httpLbs` manager)
  case refused of
    Left e -> httpFailureDetail refusedUrl e `shouldSatisfy` ("ConnectionFailure" `T.isPrefixOf`)
    Right _ -> expectationFailure "connected to port 1"

testBlockAt :: TVar ChaChaDRG -> IO ()
testBlockAt g = withChain g $ \node c -> do
  setBlock node 100 1798761600
  mined <- mine node []
  blockAt c 100 `shouldReturn` Right Block {blockNumber = 100, blockTimestamp = 1798761600, blockBaseFee = fakeBaseFee}
  blockAt c mined `shouldReturn` Right Block {blockNumber = 101, blockTimestamp = 1798761612, blockBaseFee = fakeBaseFee}
  blockAt c 102 >>= (`shouldSatisfy` \case Left (ChainUnavailable _) -> True; _ -> False)

testFollowTransaction :: TVar ChaChaDRG -> IO ()
testFollowTransaction g = withChain g $ \node c@ChainClient {ccRegistrar} -> do
  let unknown = TxHash $ B.replicate 32 0xee
  transactionKnown c unknown `shouldReturn` Right False
  transactionReceipt c unknown `shouldReturn` Right Nothing
  SignedTx {stRaw, stHash = hash@(TxHash h)} <- signed g c 0
  sendRawTransaction c stRaw `shouldReturn` Right hash
  transactionKnown c hash `shouldReturn` Right True
  transactionReceipt c hash `shouldReturn` Right Nothing
  transactionCount c ccRegistrar Latest `shouldReturn` Right 0
  transactionCount c ccRegistrar Pending `shouldReturn` Right 1
  block <- mine node [(h, False)]
  transactionKnown c hash `shouldReturn` Right True
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

unhex :: ByteString -> ByteString
unhex = either error id . convertFromBase Base16
