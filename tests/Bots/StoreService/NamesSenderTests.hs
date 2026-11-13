{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesSenderTests (namesSenderTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesStoreTests (issue, order, ownerA, placeCode, t0, usable)
import Bots.StoreService.WebTests (capturingStderr, digestFixture, withServiceStore)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM
import Control.Monad (forM_, replicateM_)
import Crypto.Random (ChaChaDRG)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (sort)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Data.Word (Word64, Word8)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Secp256k1 (mkPrivateKey)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Chain
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Registration (commitCalldata)
import StoreService.Names.Revert (RevertName (..))
import StoreService.Names.Sender
import StoreService.Names.Store (NameTransaction (..), TxFor (..), TxStatus (..), pendingTransactions, registrationTransactions)
import StoreService.Names.View (addressText)
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
  it "refuses a start on another key or chain while a transaction is pending, naming both, and accepts the new key once none is" $ testRotatedKeyStart g
  it "hands a dropped transaction whose nonce was used, or a mined one whose receipt was pruned, to its owner, who may send again at the next nonce" $ testDroppedNonceUsed g
  it "logs a round that fails on the store, and settles the transaction in the next round" $ testFailedRound g
  it "logs a send the node keeps refusing once per error, an underfunded key as an error, in anvil's capitalized form too" $ testRefusedSendLogged g
  it "refuses a transaction whose block needs more than the fee cap, saving nothing, and signs at most the cap" $ testFeeCapSubmit g
  it "clamps a replacement at the fee cap, and keeps rebroadcasting once a replacement would pass it" $ testFeeCapReplace g
  it "caps the max fee of a transaction and of its replacement so that its gas limit costs at most 1 ETH" $ testTxFeeBound g

data Setup = Setup
  { node :: FakeEthNode,
    store :: DBStore,
    clock :: TVar UTCTime,
    chain :: ChainClient,
    random :: TVar ChaChaDRG
  }

withSetup :: TVar ChaChaDRG -> (Setup -> IO a) -> IO a
withSetup = withCapSetup fakeMaxFeePerGas

-- | Two code orders, r1 and r2, two confirmations, and this max fee per gas.
withCapSetup :: Word64 -> TVar ChaChaDRG -> (Setup -> IO a) -> IO a
withCapSetup cap random action = withFakeEthNode $ \node -> withServiceStore $ \store -> do
  forM_ [(1, "r1", "alicebakery"), (2, "r2", "bobbakery")] $ \(n, rid, label) -> do
    code <- issue store n usable
    placeCode store rid ownerA (order label n) code `shouldReturn` Right ()
  clock <- newTVarIO t0
  chain <- newChainClient random (fakeChainConfig node) {cConfirmations = 2, cMaxFeePerGas = cap}
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
  transactionSigner (B.take 100 stRaw) `shouldReturn` Left "RLP: truncated"
  forM_ [0 .. 15] $ \nonce -> do
    Right SignedTx {stRaw = raw} <- signTransaction g c request {trNonce = nonce}
    (\signer -> (nonce, signer)) <$> transactionSigner raw `shouldReturn` (nonce, Right (fakeChainId, ccRegistrar c))
  ccRegistrar c `shouldBe` address "0x9d8A62f656a8d1615C1294fd71e9CFb3E4855A4F"
  transactionSigner viemTxYParity1ShortS `shouldReturn` Right (fakeChainId, ccRegistrar c)

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
  -- a wait for an outcome not reached yet ends at its bound
  timeout 5000000 (awaitOutcome 300000 p) `shouldReturn` Just Nothing
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
  known s (ptHash p) `shouldReturn` True

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
  known s (ptHash p) `shouldReturn` False
  known s bumpedHash `shouldReturn` True
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

testRotatedKeyStart :: TVar ChaChaDRG -> IO ()
testRotatedKeyStart g = withSetup g $ \s@Setup {node, store, chain = oldChain} -> do
  sender <- start s
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  let oldConfig = ccConfig oldChain
      newConfig = oldConfig {cServiceKey = either error id $ mkPrivateKey $ BA.replicate 32 0x47}
      startOn cfg = checkPendingSignatures g cfg store
      refusal signedFor configured =
        Left $ "a pending transaction of nonce 0 was signed " <> signedFor <> ", not " <> configured <> "; start with the key and chain that signed it until no transaction is pending"
  newRegistrar <- ccRegistrar <$> newChainClient g newConfig
  startOn oldConfig `shouldReturn` Right ()
  startOn newConfig `shouldReturn` refusal ("by " <> shown (ccRegistrar oldChain)) ("by " <> shown newRegistrar <> " of chain.service_key")
  startOn oldConfig {cChainId = 1} `shouldReturn` refusal ("for chain " <> show fakeChainId) "for chain.chain_id 1"
  mineHash node (ptHash p)
  _ <- mine node []
  followTransactions sender
  outcomeOf p >>= (`shouldSatisfy` confirmedAs (ptHash p))
  startOn newConfig `shouldReturn` Right ()
  where
    shown = T.unpack . addressText

testDroppedNonceUsed :: TVar ChaChaDRG -> IO ()
testDroppedNonceUsed g =
  forM_ [\node h -> dropTransaction node h >> useNextNonce node, \node h -> mine node [(h, True)] >> pruneReceipt node h] $ \lose ->
    withSetup g $ \s@Setup {node, store} -> do
      sender <- start s
      Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
      let TxHash h = ptHash p
      lose node h
      followTransactions sender
      settled sender (TxCommit r1) `shouldReturn` False
      _ <- mine node []
      followTransactions sender
      outcomeOf p `shouldReturn` TONonceUsed
      statuses store r1 `shouldReturn` [(ptHash p, TSDropped, Nothing)]
      length <$> sentTransactions node `shouldReturn` 1
      Right again <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
      ptNonce again `shouldBe` 1
      length <$> sentTransactions node `shouldReturn` 2

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

testRefusedSendLogged :: TVar ChaChaDRG -> IO ()
testRefusedSendLogged g = withSetup g $ \s@Setup {node} -> do
  sender <- start s
  -- the specs run in parallel, so this one's lines are told apart by a nonce no other spec reaches
  replicateM_ 9 $ useNextNonce node
  let underfunded = "insufficient funds for gas * price + value: balance 0, tx cost 4600000000000000, overshot 4600000000000000"
  logged <- capturingStderr $ do
    setFault node "eth_sendRawTransaction" $ RpcError (-32000) underfunded Nothing
    Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
    ptNonce p `shouldBe` 9
    followTransactions sender
    followTransactions sender
    setFault node "eth_sendRawTransaction" HttpFailure
    followTransactions sender
    followTransactions sender
    setFault node "eth_sendRawTransaction" $ RpcError (-32003) "Insufficient funds for gas * price + value" Nothing
    followTransactions sender
    followTransactions sender
    clearFault node "eth_sendRawTransaction"
    followTransactions sender
  let linesWith tag message = length $ filter (\l -> tag `T.isPrefixOf` l && message `T.isInfixOf` l) $ T.lines logged
  linesWith "[ERROR" "names sender: nonce 9: not sent: ChainUnavailable \"eth_sendRawTransaction: error -32000: insufficient funds" `shouldBe` 1
  linesWith "[WARN" "names sender: nonce 9: not sent: ChainUnavailable \"eth_sendRawTransaction: HTTP 502\"" `shouldBe` 1
  linesWith "[ERROR" "names sender: nonce 9: not sent: ChainUnavailable \"eth_sendRawTransaction: error -32003: Insufficient funds" `shouldBe` 1
  linesWith "[" "names sender: nonce 9" `shouldBe` 3
  linesWith "[" "names sender: rebroadcasting nonce 9" `shouldBe` 0
  length <$> sentTransactions node `shouldReturn` 1

testFeeCapSubmit :: TVar ChaChaDRG -> IO ()
testFeeCapSubmit g = withSetup g $ \s@Setup {node, store} -> do
  sender <- start s
  setFees node fakeTip (fakeMaxFeePerGas - fakeTip + 1)
  submitTransaction sender (TxCommit r1) fakeController (commit 1) `refusedWith` SEFeeAboveCap (fakeMaxFeePerGas + 1)
  -- twice the base fee is past Word64, which would wrap to a near-zero fee
  let hugeBaseFee = maxBound `div` 2 + 1 :: Word64
  setFees node fakeTip hugeBaseFee
  submitTransaction sender (TxCommit r1) fakeController (commit 1) `refusedWith` SEFeeAboveCap (toInteger hugeBaseFee + fakeTip)
  withTransaction store pendingTransactions `shouldReturn` []
  sentTransactions node `shouldReturn` []
  let baseFee = fakeMaxFeePerGas `div` 2
  setFees node fakeTip baseFee
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  ptNonce p `shouldBe` 0
  [raw] <- sentTransactions node
  (\TxRequest {trMaxPriorityFeePerGas, trMaxFeePerGas} -> (trMaxPriorityFeePerGas, trMaxFeePerGas)) <$> decodeSignedTx raw `shouldBe` Right (fakeTip, fakeMaxFeePerGas)

testFeeCapReplace :: TVar ChaChaDRG -> IO ()
testFeeCapReplace g = withCapSetup cap g $ \s@Setup {node} -> do
  sender <- start s
  Right p <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  [firstRaw] <- sentTransactions node
  Right first <- pure $ decodeSignedTx firstRaw
  trMaxFeePerGas first `shouldBe` 2 * fakeBaseFee + fakeTip
  setFees node fakeTip (2 * fakeBaseFee)
  passTime s replaceAfter
  followTransactions sender
  [_, bumpedRaw] <- sentTransactions node
  decodeSignedTx bumpedRaw `shouldBe` Right first {trMaxPriorityFeePerGas = fakeTip + fakeTip `div` 8 + 1, trMaxFeePerGas = cap}
  passTime s replaceAfter
  followTransactions sender
  length <$> sentTransactions node `shouldReturn` 2
  let bumpedHash = C.keccak256 bumpedRaw
  dropTransaction node bumpedHash
  followTransactions sender
  sentTransactions node `shouldReturn` [firstRaw, bumpedRaw, bumpedRaw]
  (fmap ptHash <$> inFlightTransaction sender (TxCommit r1)) `shouldReturn` Just (TxHash bumpedHash)
  ptNonce p `shouldBe` 0
  where
    -- above the first max fee and its bump, below the market's headroom fee and a second bump
    cap = 2 * fakeBaseFee + fakeTip + 1000000000

testTxFeeBound :: TVar ChaChaDRG -> IO ()
testTxFeeBound g = withCapSetup 10000000000000 g $ \s@Setup {node} -> do
  let gasLimit = fakeGas + fakeGas `div` 5 :: Word64
      bound = fromInteger $ 1000000000000000000 `div` toInteger gasLimit :: Word64
  sender <- start s
  setFees node fakeTip 5000000000000
  submitTransaction sender (TxCommit r1) fakeController (commit 1) `refusedWith` SEFeeAboveCap (5000000000000 + fakeTip)
  setFees node fakeTip 1000000000000
  Right _ <- submitTransaction sender (TxCommit r1) fakeController (commit 1)
  [firstRaw] <- sentTransactions node
  Right first <- pure $ decodeSignedTx firstRaw
  (trGasLimit first, trMaxFeePerGas first) `shouldBe` (gasLimit, 2 * 1000000000000 + fakeTip)
  setFees node fakeTip 3000000000000
  passTime s replaceAfter
  logged <- capturingStderr $ followTransactions sender
  logged `shouldSatisfy` T.isInfixOf ("names sender: replacing nonce 0 at max fee " <> tshow bound <> ", capped so that its gas limit " <> tshow gasLimit <> " costs at most 1 ETH")
  [_, bumpedRaw] <- sentTransactions node
  decodeSignedTx bumpedRaw `shouldBe` Right first {trMaxPriorityFeePerGas = fakeTip + fakeTip `div` 8 + 1, trMaxFeePerGas = bound}
  passTime s replaceAfter
  followTransactions sender
  length <$> sentTransactions node `shouldReturn` 2
  Right _ <- submitTransaction sender (TxCommit r2) fakeController (commit 2)
  [_, _, cappedRaw] <- sentTransactions node
  (\TxRequest {trGasLimit, trMaxFeePerGas} -> (trGasLimit, trMaxFeePerGas)) <$> decodeSignedTx cappedRaw `shouldBe` Right (gasLimit, bound)
  toInteger gasLimit * toInteger bound `shouldSatisfy` (<= 1000000000000000000)

-- fixtures

r1, r2 :: RegistrationId
r1 = RegistrationId "r1"
r2 = RegistrationId "r2"

commit :: Word8 -> ByteString
commit = commitCalldata . digestFixture

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
outcomeOf p = awaitOutcome 2000000 p >>= maybe (fail "the transaction is not settled") pure

settled :: Sender -> TxFor -> IO Bool
settled sender for = null <$> inFlightTransaction sender for

statuses :: DBStore -> RegistrationId -> IO [(TxHash, TxStatus, Maybe Word64)]
statuses store rid = withTransaction store $ \db -> map (\NameTransaction {ntHash, ntStatus, ntMinedBlock} -> (ntHash, ntStatus, ntMinedBlock)) <$> registrationTransactions db rid

known :: Setup -> TxHash -> IO Bool
known Setup {chain} h = either (error . show) id <$> transactionKnown chain h

refusedWith :: IO (Either SendError PendingTx) -> SendError -> Expectation
refusedWith submit expected =
  submit >>= \case
    Left e -> e `shouldBe` expected
    Right p -> expectationFailure $ "sent with nonce " <> show (ptNonce p)

unhex :: ByteString -> ByteString
unhex = either error id . convertFromBase Base16

viemTxYParity1ShortS :: ByteString
viemTxYParity1ShortS = unhex "02f86f827a693b843b9aca00850737be76008306ddd0945fbdb2315678afecb367f032d93f642f64180aa38082abcdc001a03e704053d6725b7b06476fca04827611f81b637f38b5ab351d62c236252ac06e9f9738e8cce6189740f438c699b4b77b8f9c4a15376ed3b3ef48fea57c459ed8"
