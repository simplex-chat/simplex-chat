{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesSenderTests (namesSenderTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesStoreTests (issue, order, ownerA, placeCode, t0, usable)
import Bots.StoreService.WebTests (digestFixture, withServiceStore)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM
import Control.Monad (forM_)
import Crypto.Random (ChaChaDRG)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (sort)
import Data.String (fromString)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Data.Word (Word64, Word8)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Abi (selector, uint256)
import StoreService.Names.Chain
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Registration (commitCalldata)
import StoreService.Names.Revert (RevertName (..))
import StoreService.Names.Sender
import StoreService.Names.Store (NameTransaction (..), TxFor (..), TxStatus (..), pendingTransactions, registrationTransactions)
import System.Timeout (timeout)
import Test.Hspec

namesSenderTests :: Spec
namesSenderTests = describe "StoreService names: sender" $ do
  g <- runIO C.newRandom
  it "reads a signed transaction's request back from its raw bytes" $ testDecodeSignedTx g
  it "gives concurrent transactions their own nonces, each saved before it is sent" $ testConcurrentNonces g
  it "saves nothing and takes no nonce when the dry run reverts or the chain does not answer" $ testNothingSaved g
  it "refuses a second transaction for work with one in flight" $ testOneInFlight g
  it "settles a receipt only once it is confirmations deep, with its status" $ testConfirmationDepth g
  it "keeps a transaction whose send failed, and sends it in the next round" $ testFailedSend g
  it "replaces a stuck transaction once the market's fees pass its own, and settles it under the new hash" $ testFeeBump g
  it "rebroadcasts a saved raw transaction after a restart while the node does not know it" $ testRestartRebroadcast g
  it "hands a dropped transaction whose nonce was used to its owner, who judges it from chain state before any new signature" $ testDroppedNonceUsed g
  it "hands a mined transaction whose receipt was pruned to its owner, signing nothing more" $ testPrunedReceipt g
  it "logs a round that fails on the store, and settles the transaction in the next round" $ testFailedRound g

data Setup = Setup
  { node :: FakeEthNode,
    store :: DBStore,
    clock :: TVar UTCTime,
    chain :: ChainClient,
    random :: TVar ChaChaDRG
  }

-- | Two code orders, r1 and r2, and two confirmations.
withSetup :: TVar ChaChaDRG -> (Setup -> IO a) -> IO a
withSetup random action = withFakeEthNode $ \node -> withServiceStore $ \store -> do
  forM_ [(1, "r1", "alicebakery"), (2, "r2", "bobbakery")] $ \(n, rid, label) -> do
    code <- issue store n usable
    placeCode store rid ownerA (order label n) code `shouldReturn` Right ()
  clock <- newTVarIO t0
  chain <- newChainClient random (fakeChainConfig node) {cConfirmations = 2}
  action Setup {node, store, clock, chain, random}

start :: Setup -> IO Sender
start Setup {random, chain, store, clock} = newSender random chain store (readTVarIO clock)

passTime :: Setup -> NominalDiffTime -> IO ()
passTime Setup {clock} t = atomically $ modifyTVar' clock (addUTCTime t)

testDecodeSignedTx :: TVar ChaChaDRG -> IO ()
testDecodeSignedTx g = withFakeEthNode $ \node -> do
  c <- newChainClient g $ fakeChainConfig node
  -- calldata over 55 bytes takes a long RLP length
  let request = TxRequest {trTo = fakeSponsor, trData = B.replicate 300 0xab, trNonce = 70000, trGasLimit = 450000, trMaxFeePerGas = 31000000000, trMaxPriorityFeePerGas = 0}
  Right SignedTx {stRaw} <- signTransaction g c request
  decodeSignedTx stRaw `shouldBe` Right request
  decodeSignedTx (B.cons 0x01 $ B.drop 1 stRaw) `shouldBe` Left "transaction: not an EIP-1559 transaction"
  decodeSignedTx (B.take 100 stRaw) `shouldBe` Left "RLP: truncated"

testConcurrentNonces :: TVar ChaChaDRG -> IO ()
testConcurrentNonces g = withSetup g $ \s@Setup {node, store} -> do
  sender <- start s
  -- both dry runs pass before either reads the registrar's nonce
  setFault node "eth_getTransactionCount" $ Slow 300000
  (Right a, Right b) <- concurrently (submitTransaction sender (TxCommit r1) fakeController $ commit 1) (submitTransaction sender (TxCommit r2) fakeController $ commit 2)
  clearFault node "eth_getTransactionCount"
  sort [ptNonce a, ptNonce b] `shouldBe` [0, 1]
  saved <- withTransaction store pendingTransactions
  map (\NameTransaction {ntFor, ntNonce, ntHash} -> (ntFor, ntNonce, ntHash)) saved
    `shouldMatchList` [(TxCommit r1, ptNonce a, ptHash a), (TxCommit r2, ptNonce b, ptHash b)]
  sentTransactions node `shouldReturn` map ntRaw saved
  mapM_ (mineHash node) [ptHash a, ptHash b]
  _ <- mine node []
  followTransactions sender
  forM_ [a, b] $ \p -> outcomeOf p >>= (`shouldSatisfy` confirmedAs (ptHash p))
  Right c <- submitTransaction sender (TxRegister r1) fakeSponsor $ commit 3
  ptNonce c `shouldBe` 2

testNothingSaved :: TVar ChaChaDRG -> IO ()
testNothingSaved g = withSetup g $ \s@Setup {node, store} -> do
  sender <- start s
  setFault node "eth_estimateGas" $ revertFault insufficientAllowance
  submitTransaction sender (TxCommit r1) fakeController (commit 1) `refusedWith` SEChain (ChainReverted $ Just RNInsufficientAllowance)
  clearFault node "eth_estimateGas"
  setFault node "eth_maxPriorityFeePerGas" HttpFailure
  submitTransaction sender (TxCommit r1) fakeController (commit 1) `refusedWith` SEChain (ChainUnavailable "eth_maxPriorityFeePerGas: HTTP 502")
  clearFault node "eth_maxPriorityFeePerGas"
  withTransaction store pendingTransactions `shouldReturn` []
  sentTransactions node `shouldReturn` []
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  ptNonce p `shouldBe` 0

testOneInFlight :: TVar ChaChaDRG -> IO ()
testOneInFlight g = withSetup g $ \s@Setup {node} -> do
  sender <- start s
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  submitTransaction sender (TxCommit r1) fakeController (commit 1) `refusedWith` SEInFlight
  Right other <- submitTransaction sender (TxRegister r1) fakeSponsor (commit 2)
  ptNonce other `shouldBe` 1
  (fmap ptHash <$> inFlightTransaction sender (TxCommit r1)) `shouldReturn` Just (ptHash p)
  length <$> sentTransactions node `shouldReturn` 2

testConfirmationDepth :: TVar ChaChaDRG -> IO ()
testConfirmationDepth g = withSetup g $ \s@Setup {node, store} -> do
  sender <- start s
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  let TxHash h = ptHash p
  block <- mine node [(h, False)]
  followTransactions sender
  settled sender (TxCommit r1) `shouldReturn` False
  _ <- mine node []
  followTransactions sender
  outcomeOf p `shouldReturn` TOConfirmed (ptHash p) Receipt {receiptSuccess = False, receiptBlock = block}
  settled sender (TxCommit r1) `shouldReturn` True
  statuses store r1 `shouldReturn` [(ptHash p, TSReverted, Just block)]

testFailedSend :: TVar ChaChaDRG -> IO ()
testFailedSend g = withSetup g $ \s@Setup {node, store} -> do
  sender <- start s
  setFault node "eth_sendRawTransaction" HttpFailure
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  clearFault node "eth_sendRawTransaction"
  sentTransactions node `shouldReturn` []
  [NameTransaction {ntRaw, ntHash}] <- withTransaction store pendingTransactions
  ntHash `shouldBe` ptHash p
  followTransactions sender
  sentTransactions node `shouldReturn` [ntRaw]
  transactionByHash' s (ptHash p) `shouldReturn` Just TxPending

testFeeBump :: TVar ChaChaDRG -> IO ()
testFeeBump g = withSetup g $ \s@Setup {node, store} -> do
  sender <- start s
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  [firstRaw] <- sentTransactions node
  Right first@TxRequest {trMaxFeePerGas = firstMax, trMaxPriorityFeePerGas = firstTip} <- pure $ decodeSignedTx firstRaw
  (firstTip, firstMax) `shouldBe` (fakeTip, 2 * fakeBaseFee + fakeTip)
  passTime s $ replaceAfter - 1
  followTransactions sender
  passTime s 1
  followTransactions sender
  length <$> sentTransactions node `shouldReturn` 1
  setFees node fakeTip (2 * fakeBaseFee)
  followTransactions sender
  length <$> sentTransactions node `shouldReturn` 1
  passTime s replaceAfter
  followTransactions sender
  [_, bumpedRaw] <- sentTransactions node
  let bumpedHash = TxHash $ C.keccak256 bumpedRaw
  decodeSignedTx bumpedRaw `shouldBe` Right first {trMaxPriorityFeePerGas = fakeTip + fakeTip `div` 8 + 1, trMaxFeePerGas = 4 * fakeBaseFee + fakeTip}
  transactionByHash' s (ptHash p) `shouldReturn` Nothing
  transactionByHash' s bumpedHash `shouldReturn` Just TxPending
  map (\(h, status, _) -> (h, status)) <$> statuses store r1 `shouldReturn` [(ptHash p, TSPending), (bumpedHash, TSPending)]
  mineHash node bumpedHash
  _ <- mine node []
  followTransactions sender
  TOConfirmed minedHash Receipt {receiptSuccess = True, receiptBlock} <- outcomeOf p
  minedHash `shouldBe` bumpedHash
  statuses store r1 `shouldReturn` [(ptHash p, TSDropped, Nothing), (bumpedHash, TSMined, Just receiptBlock)]

testRestartRebroadcast :: TVar ChaChaDRG -> IO ()
testRestartRebroadcast g = withSetup g $ \s@Setup {node, store} -> do
  crashed <- start s
  Right p <- submitTransaction crashed (TxCommit r1) fakeController (commit 1)
  [raw] <- sentTransactions node
  let TxHash h = ptHash p
  dropTransaction node h
  restarted <- start s
  Just resumed <- inFlightTransaction restarted (TxCommit r1)
  (ptHash resumed, ptNonce resumed) `shouldBe` (ptHash p, 0)
  followTransactions restarted
  sentTransactions node `shouldReturn` [raw, raw]
  followTransactions restarted
  sentTransactions node `shouldReturn` [raw, raw]
  Right next <- submitTransaction restarted (TxCommit r2) fakeController (commit 2)
  ptNonce next `shouldBe` 1
  _ <- mine node [(h, True)]
  _ <- mine node []
  followTransactions restarted
  outcomeOf resumed >>= (`shouldSatisfy` confirmedAs (ptHash p))
  map (\(_, status, _) -> status) <$> statuses store r1 `shouldReturn` [TSMined]

testDroppedNonceUsed :: TVar ChaChaDRG -> IO ()
testDroppedNonceUsed g = withSetup g $ \s@Setup {node, store, chain} -> do
  sender <- start s
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  let TxHash h = ptHash p
  dropTransaction node h
  useNextNonce node
  followTransactions sender
  settled sender (TxCommit r1) `shouldReturn` False
  _ <- mine node []
  followTransactions sender
  outcomeOf p `shouldReturn` TONonceUsed
  statuses store r1 `shouldReturn` [(ptHash p, TSDropped, Nothing)]
  length <$> sentTransactions node `shouldReturn` 1
  -- the owner: the commitment reads 0, so the commit never mined and is sent again
  answerCall node fakeController commitmentsCall $ Returns $ uint256 (0 :: Int)
  ethCall chain fakeController commitmentsCall `shouldReturn` Right (uint256 (0 :: Int))
  Right again <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  ptNonce again `shouldBe` 1
  length <$> sentTransactions node `shouldReturn` 2

testPrunedReceipt :: TVar ChaChaDRG -> IO ()
testPrunedReceipt g = withSetup g $ \s@Setup {node, store} -> do
  sender <- start s
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  let TxHash h = ptHash p
  _ <- mine node [(h, True)]
  pruneReceipt node h
  followTransactions sender
  settled sender (TxCommit r1) `shouldReturn` False
  _ <- mine node []
  followTransactions sender
  outcomeOf p `shouldReturn` TONonceUsed
  statuses store r1 `shouldReturn` [(ptHash p, TSDropped, Nothing)]
  followTransactions sender
  length <$> sentTransactions node `shouldReturn` 1
  withTransaction store pendingTransactions `shouldReturn` []

testFailedRound :: TVar ChaChaDRG -> IO ()
testFailedRound g = withSetup g $ \s@Setup {node, store} -> do
  sender <- start s
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  mineHash node (ptHash p)
  _ <- mine node []
  renameTransactions store "sx_badge_service_name_transactions" "name_transactions_away"
  senderRound sender
  settled sender (TxCommit r1) `shouldReturn` False
  renameTransactions store "name_transactions_away" "sx_badge_service_name_transactions"
  senderRound sender
  outcomeOf p >>= (`shouldSatisfy` confirmedAs (ptHash p))
  map (\(_, status, _) -> status) <$> statuses store r1 `shouldReturn` [TSMined]
  where
    renameTransactions store from to = withTransaction store $ \db -> DB.execute_ db $ fromString $ "ALTER TABLE " <> from <> " RENAME TO " <> to

-- fixtures

r1, r2 :: RegistrationId
r1 = RegistrationId "r1"
r2 = RegistrationId "r2"

commit :: Word8 -> ByteString
commit = commitCalldata . digestFixture

commitmentsCall :: ByteString
commitmentsCall = selector "commitments(bytes32)" <> digestFixture 1

-- | viem's encodeErrorResult of InsufficientAllowance(10000, 2500)
insufficientAllowance :: ByteString
insufficientAllowance = unhex "2a1b2dd8000000000000000000000000000000000000000000000000000000000000271000000000000000000000000000000000000000000000000000000000000009c4"

mineHash :: FakeEthNode -> TxHash -> IO ()
mineHash node (TxHash h) = () <$ mine node [(h, True)]

confirmedAs :: TxHash -> TxOutcome -> Bool
confirmedAs hash = \case
  TOConfirmed h Receipt {receiptSuccess = True} -> h == hash
  _ -> False

outcomeOf :: PendingTx -> IO TxOutcome
outcomeOf p = timeout 2000000 (awaitOutcome p) >>= maybe (fail "the transaction is not settled") pure

settled :: Sender -> TxFor -> IO Bool
settled sender for = null <$> inFlightTransaction sender for

statuses :: DBStore -> RegistrationId -> IO [(TxHash, TxStatus, Maybe Word64)]
statuses store rid = withTransaction store $ \db -> map (\NameTransaction {ntHash, ntStatus, ntMinedBlock} -> (ntHash, ntStatus, ntMinedBlock)) <$> registrationTransactions db rid

transactionByHash' :: Setup -> TxHash -> IO (Maybe TxState)
transactionByHash' Setup {chain} h = either (error . show) id <$> transactionByHash chain h

refusedWith :: IO (Either SendError PendingTx) -> SendError -> Expectation
refusedWith submit expected =
  submit >>= \case
    Left e -> e `shouldBe` expected
    Right p -> expectationFailure $ "sent with nonce " <> show (ptNonce p)

unhex :: ByteString -> ByteString
unhex = either error id . convertFromBase Base16
