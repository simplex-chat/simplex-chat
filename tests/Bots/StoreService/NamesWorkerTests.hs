{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesWorkerTests (namesWorkerTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (availableCall, maxCommitmentAgeCall, minCommitmentAgeCall, registrarOf, unhex)
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesOwnerTests (answerExpiry, answerOwner, answerUnowned, byB, codeIn, committedPhase, dynamisOrder, failedPhase, labelBytes, ownerOfSelector, paidIn, rewrite, stored)
import Bots.StoreService.NamesPhasesTests (held, prompt, registrationPath, woken)
import Bots.StoreService.NamesStoreTests (reveal)
import Bots.StoreService.WebTests (bounded, capturingStderr, jsonObject, statusOf, webGet)
import Control.Applicative ((<|>))
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (replicateM_, void)
import Data.ByteString (ByteString)
import qualified Data.Aeson as J
import Data.IORef (readIORef)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, signRecoverable)
import StoreService.Names.Abi (abiBytes, abiTuple, addressWord, AbiValue (..), selector, uint256)
import StoreService.Names.Chain (Block (..), ChainClient, Receipt (..), TxHash (..), TxRequest (..), blockAt, decodeSignedTx, newChainClient)
import StoreService.Names.Orders (Landing (..), OrdersEnv (..), confirmLanded)
import StoreService.Names.Phases (awaitChange)
import StoreService.Names.Registration (RegisterIntent (..), commitCalldata, registerIntentDigest, registerWithCreditCalldata, sponsorDomain)
import StoreService.Names.Reveal (intentOf)
import StoreService.Names.Sender
import StoreService.Names.Store
import StoreService.Names.Turns (labelTurnCount, withLabelTurn)
import StoreService.Names.Worker
import StoreService.Web.Http (holdMicros, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Registration (registrationHandler)
import StoreService.Web.Server (WebEnv (..), webApp)
import System.Timeout (timeout)
import Test.Hspec

namesWorkerTests :: Spec
namesWorkerTests = describe "StoreService names: worker" $ do
  describe "commit" $ do
    it "commits, waits for the reveal and revealAfter, and registers a code order, spending its code and publishing each change" testCommitWaitRegister
    it "waits for a pending commit without sending another, following its fee-bumped replacement" testPendingCommit
    it "goes to committed when the commit reverts UnexpiredCommitmentExists, in its dry run or mined, with the commitment's times" testUnexpiredCommitment
    it "sends a commit mined out of gas again" testCommitOutOfGas
    it "waits a second at the lapse second, and again when the dry run still reverts, then commits" testLapseSecond
    it "clears a requeued order's stale intent before committing again after a lapse, and a third party's commitment's" testStaleIntent
    it "keeps the phase while the latest block needs more than the fee cap, then commits" testFeeAboveCap
  describe "writes" $ do
    it "writes nothing when the registration changed since it was read" testLostRace
    it "writes in the name's turn" testInNameTurn
  describe "register" $ do
    it "ends the later of two wallets' paid orders for one name taken, with its value kept" testTwoWallets
    it "ends taken on any revert while the name is not available, releasing a code" testRevertTaken
    it "keeps the phase on NameNotAvailable while the name still reads available, as for a pending competitor" testPendingCompetitor
    it "ends failed on a revert while the name is available, alerting on InsufficientAllowance and keeping a code" testRevertFailed
    it "clears the reveal on InvalidNonce, staying committed" testInvalidNonce
    it "relays no intent past its deadline, ending failed" testExpiredIntent
    it "keeps the phase when a read of the final check fails" testCheckUnread
    it "waits a second when the dry run reverts CommitmentTooNew" testCommitmentTooNew
    it "ends a register mined with status 0 taken while the name is not available, releasing a code" testMinedRevertTaken
    it "writes failed only once no register can still be mined, and registered when the owner holds the name" testFailedOnlySettled
  describe "lapse" $ do
    it "ends a committed order not revealed by revealBefore failed, keeping its code" testLapseFailed
    it "wakes a held poll with failed at the lapse, and answers an old version at once after it" testLapsePoll
  describe "restart" $ do
    it "resumes every phase after a restart, a commit saved but not recorded included" testRestart
    it "waits for a register saved but not recorded before a restart, sending no other" testRestartRegisterSaved
  describe "lanes" $ do
    it "starts a worker for each registration in its phases" testRunWorkers
    it "releases the codes of code orders failed for 30 days, and no others" testCodeSweep
  describe "confirmation depth" $
    it "judges a register landed by ownerOf only once the owner holds the name at the confirmation depth" testLandedAtDepth
  describe "sender" $
    it "bounds the wait for a transaction's outcome" testOutcomeBound

-- fixtures

data W = W
  { wCtx :: Ctx,
    wChain :: ChainClient,
    wSender :: Sender,
    wWorker :: Worker
  }

-- | The worker over the orders' store and fake node, with the registration poll as namesRoutes registers it.
withWorker :: (W -> IO a) -> IO a
withWorker action = withOrders $ \ctx@Ctx {cNode, cOrders, cWeb} -> do
  answerCall cNode fakeController minCommitmentAgeCall $ Returns $ uint256 minAge
  answerCall cNode fakeController maxCommitmentAgeCall $ Returns $ uint256 maxAge
  answerUnowned ctx "alicebakery"
  let web = cWeb {weOrders = Just cOrders}
      routes path = registrationRoute path <|> codeRoutes cOrders (weProviders web) (weWaiters web) path
      registrationRoute = \case
        ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler cOrders holdMicros rid}
        _ -> Nothing
      app req respond = fromMaybe (webApp web req respond) (namesApp (weHttp web) routes req respond)
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure app) $ \port -> started ctx {cWeb = web, cClient = (mgr, "http://127.0.0.1:" <> show port)} >>= action

-- | A new sender and worker over the same store, as after a restart.
started :: Ctx -> IO W
started wCtx@Ctx {cNode, cStore, cRandom, cClock, cOrders} = do
  wChain <- newChainClient cRandom $ fakeChainConfig cNode
  wSender <- newSender cRandom wChain cStore (readIORef cClock)
  wWorker <- newWorker cOrders wChain wSender
  pure W {wCtx, wChain, wSender, wWorker}

minAge, maxAge :: Word64
minAge = 60
maxAge = 86400

orderSeconds :: Word64
orderSeconds = floor $ utcTimeToPOSIXSeconds orderTime

expiry :: Word64
expiry = orderSeconds + 2 * 31536000

at :: Word64 -> UTCTime
at = posixSecondsToUTCTime . fromIntegral

day :: NominalDiffTime
day = 86400

-- | Committed a minute before the order.
openPhase :: NameRegistration -> NameRegistration
openPhase r = (committedPhase r) {nrRevealAfter = Just (orderSeconds - minAge), nrRevealBefore = Just (orderSeconds - minAge + maxAge)}

-- | The reveal the page sends for the label, its intent signed with the owner's key.
signedReveal :: Ctx -> Secp256k1PrivateKey -> Text -> NameRegistration -> IO NameReveal
signedReveal Ctx {cRandom, cOrders} key label r = do
  let rv = reveal {rvLabel = label}
      RegisterIntent {riRegistration} = intentOf (oeConfig cOrders) r rv
  sig <- either (error . show) id <$> signRecoverable cRandom key (registerIntentDigest (sponsorDomain fakeChainId fakeSponsor) riRegistration (rvNonce rv) (rvDeadline rv))
  pure rv {rvSignature = sig}

-- | The registration with the page's reveal accepted.
revealed :: Ctx -> Secp256k1PrivateKey -> Text -> NameRegistration -> IO NameRegistration
revealed ctx key label r = signedReveal ctx key label r >>= \rv -> rewrite ctx r (\o -> o {nrReveal = Just rv})

-- | Steps until the worker has nothing to do at once.
steps :: W -> NameRegistration -> IO Next
steps W {wWorker} NameRegistration {nrId} = go (10 :: Int)
  where
    go 0 = fail "the worker kept stepping"
    go n =
      stepRegistration wWorker nrId >>= \case
        Again -> go (n - 1)
        next -> pure next

shape :: Next -> String
shape = \case
  Done -> "Done"
  Again -> "Again"
  WaitUntil v t -> "WaitUntil " <> show v <> " " <> show t
  Retry v reason -> "Retry " <> show v <> " " <> T.unpack reason
  Await p -> "Await " <> show (ptHash p)

awaiting :: Next -> IO PendingTx
awaiting = \case
  Await p -> pure p
  next -> fail $ "not awaiting a transaction: " <> shape next

waitingUntil :: Next -> IO UTCTime
waitingUntil = \case
  WaitUntil _ t -> pure t
  next -> fail $ "not waiting: " <> shape next

retrying :: Next -> IO ()
retrying = \case
  Retry {} -> pure ()
  next -> fail $ "not retrying: " <> shape next

done :: Next -> IO ()
done = \case
  Done -> pure ()
  next -> fail $ "not done: " <> shape next

-- | The raw transactions the node received, in the order they were sent.
sent :: W -> IO [ByteString]
sent W {wCtx = Ctx {cNode}} = sentTransactions cNode

newestHash :: W -> IO TxHash
newestHash w = TxHash . keccak256 . last <$> sent w

-- | Mines the newest transaction sent, with this status, and the blocks after it that settle it; the receipt's block.
mineNewest :: W -> Bool -> IO Word64
mineNewest w@W {wCtx = Ctx {cNode}, wSender} ok = do
  TxHash h <- newestHash w
  block <- mine cNode [(h, ok)]
  replicateM_ 2 $ mine cNode []
  followTransactions wSender
  pure block

blockTime :: W -> Word64 -> IO Word64
blockTime W {wChain} n = either (error . show) blockTimestamp <$> blockAt wChain n

commitmentsCall :: NameRegistration -> ByteString
commitmentsCall NameRegistration {nrCommitment} = selector "commitments(bytes32)" <> nrCommitment

-- | The commitment's time on chain, 0 for none.
answerCommitment :: W -> NameRegistration -> Word64 -> IO ()
answerCommitment W {wCtx = Ctx {cNode}} r t = answerCall cNode fakeController (commitmentsCall r) $ Returns $ uint256 t

answerAvailable :: W -> Bool -> IO ()
answerAvailable W {wCtx = Ctx {cNode}} available = answerCall cNode fakeController availableCall $ Returns $ uint256 (if available then 1 else 0 :: Int)

-- | The dry runs revert with the contracts' error of this signature.
dryRunsRevert :: W -> ByteString -> ByteString -> IO ()
dryRunsRevert W {wCtx = Ctx {cNode}} signature args = setFault cNode "eth_estimateGas" $ revertFault $ selector signature <> args

dryRunsPass :: W -> IO ()
dryRunsPass W {wCtx = Ctx {cNode}} = clearFault cNode "eth_estimateGas"

-- | The action's result, failing unless it published the version it wrote to a poll held at the version before.
published :: W -> NameRegistration -> IO a -> IO a
published W {wCtx = ctx} r action = do
  NameRegistration {nrId, nrVersion} <- stored ctx r
  poll <- held ctx (0, 1) $ registrationPath nrId <> "?seenVersion=" <> show nrVersion
  a <- action
  (status, view) <- woken "the registration poll" poll
  NameRegistration {nrVersion = written} <- stored ctx r
  (status, field "version" view, written > nrVersion) `shouldBe` (200, Just (J.toJSON written), True)
  pure a

traverseFst :: (a -> IO c) -> (a, b) -> IO (c, b)
traverseFst f (a, b) = (,b) <$> f a

codeOf :: Ctx -> PrepaidCode -> IO PrepaidCode
codeOf Ctx {cStore} PrepaidCode {pcId} = withTransaction cStore $ \db -> fromJust <$> getPrepaidCodeById db pcId

nowOf :: Ctx -> IO UTCTime
nowOf Ctx {cClock} = readIORef cClock

-- tests

testCommitWaitRegister :: IO ()
testCommitWaitRegister = withWorker $ \w@W {wCtx = ctx} -> do
  (r, code) <- codeIn ctx 8 aliceOrder id
  answerExpiry ctx "alicebakery" expiry
  answerCommitment w r 0
  void $ published w r $ steps w r >>= awaiting
  [commitRaw] <- sent w
  committing <- stored ctx r
  (nrPhase committing, nrCommitTx committing, nrVersion committing) `shouldBe` (PCommitting, Just (TxHash $ keccak256 commitRaw), nrVersion r + 1)
  -- the commit in flight is waited for
  steps w r >>= awaiting >>= (`shouldBe` TxHash (keccak256 commitRaw)) . ptHash
  length <$> sent w `shouldReturn` 1
  commitBlock <- mineNewest w True
  t <- blockTime w commitBlock
  answerCommitment w r t
  -- committed, waiting for the reveal until the lapse
  published w r (steps w r >>= waitingUntil) `shouldReturn` at (t + maxAge + 1)
  committed <- stored ctx r
  (nrPhase committed, nrRevealAfter committed, nrRevealBefore committed, nrCommitTx committed, nrCommitBlock committed)
    `shouldBe` (PCommitted, Just (t + minAge), Just (t + maxAge), Just (TxHash $ keccak256 commitRaw), Just commitBlock)
  -- the page reveals before revealAfter
  void $ revealed ctx keyA "alicebakery" r
  steps w r >>= waitingUntil >>= (`shouldBe` at (t + minAge))
  advance ctx $ fromIntegral (t + minAge) - fromIntegral orderSeconds
  void $ published w r $ steps w r >>= awaiting
  registerTx <- newestHash w
  registering <- stored ctx r
  (nrPhase registering, nrRegisterTx registering) `shouldBe` (PRegistering, Just registerTx)
  registerBlock <- mineNewest w True
  now <- nowOf ctx
  published w r (steps w r) >>= done
  x <- stored ctx r
  (nrPhase x, nrRegisterTx x, nrRegisterBlock x, nrExpiresAt x, nrRegisteredAt x, nrLive x) `shouldBe` (PRegistered, Just registerTx, Just registerBlock, Just expiry, Just now, False)
  isJust . pcSpentAt <$> codeOf ctx code `shouldReturn` True
  steps w r >>= done

testPendingCommit :: IO ()
testPendingCommit = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}, wSender} -> do
  r <- paidIn ctx aliceOrder id
  answerCommitment w r 0
  first <- ptHash <$> (steps w r >>= awaiting)
  replicateM_ 3 $ steps w r >>= awaiting >>= (`shouldBe` first) . ptHash
  length <$> sent w `shouldReturn` 1
  -- the market's fees pass the commit's, so the sender replaces it
  setFees cNode (2 * fakeTip) (2 * fakeBaseFee)
  advance ctx 181
  followTransactions wSender
  replacement <- newestHash w
  replacement `shouldNotBe` first
  inFlight <- stored ctx r
  steps w r >>= awaiting >>= (`shouldBe` replacement) . ptHash
  x <- stored ctx r
  (nrPhase x, nrCommitTx x, nrVersion x) `shouldBe` (PCommitting, Just replacement, nrVersion inFlight + 1)
  length <$> sent w `shouldReturn` 2

testUnexpiredCommitment :: IO ()
testUnexpiredCommitment = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  -- an earlier commit of the commitment mined between the read and the dry run
  r <- paidIn ctx aliceOrder id
  let t = orderSeconds - 600
  answerCommitment w r t
  answerCallOnce cNode fakeController (commitmentsCall r) $ Returns $ uint256 (0 :: Int)
  dryRunsRevert w "UnexpiredCommitmentExists(bytes32)" (nrCommitment r)
  steps w r >>= waitingUntil >>= (`shouldBe` addUTCTime 1 orderTime)
  sent w `shouldReturn` []
  stored ctx r `shouldReturn` r
  dryRunsPass w
  steps w r >>= waitingUntil >>= (`shouldBe` at (t + maxAge + 1))
  x <- stored ctx r
  (nrPhase x, nrRevealAfter x, nrRevealBefore x, nrCommitTx x) `shouldBe` (PCommitted, Just (t + minAge), Just (t + maxAge), Nothing)
  -- a commit sent, mined with status 0 after another commit of it
  r2 <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 2} id
  answerCommitment w r2 0
  void $ steps w r2 >>= awaiting
  void $ mineNewest w False
  let t2 = orderSeconds - 30
  answerCommitment w r2 t2
  steps w r2 >>= waitingUntil >>= (`shouldBe` at (t2 + maxAge + 1))
  x2 <- stored ctx r2
  (nrPhase x2, nrRevealBefore x2, nrCommitTx x2, nrCommitBlock x2) `shouldBe` (PCommitted, Just (t2 + maxAge), Nothing, Nothing)
  length <$> sent w `shouldReturn` 1

testCommitOutOfGas :: IO ()
testCommitOutOfGas = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder id
  answerCommitment w r 0
  first <- ptHash <$> (steps w r >>= awaiting)
  void $ mineNewest w False
  second <- ptHash <$> (steps w r >>= awaiting)
  second `shouldNotBe` first
  [_, raw] <- sent w
  trNonce <$> either error pure (decodeSignedTx raw) `shouldReturn` 1
  x <- stored ctx r
  (nrPhase x, nrCommitTx x) `shouldBe` (PCommitting, Just second)

testLapseSecond :: IO ()
testLapseSecond = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder id
  -- a live commitment with less than the reveal margin left is not reused
  answerCommitment w r (orderSeconds + 600 - maxAge)
  steps w r >>= waitingUntil >>= (`shouldBe` addUTCTime 601 orderTime)
  -- at the lapse second it can neither register nor be committed again
  advance ctx 600
  steps w r >>= waitingUntil >>= (`shouldBe` addUTCTime 601 orderTime)
  stored ctx r `shouldReturn` r
  advance ctx 1
  -- the node's latest block is still at the lapse second
  dryRunsRevert w "UnexpiredCommitmentExists(bytes32)" (nrCommitment r)
  steps w r >>= waitingUntil >>= (`shouldBe` addUTCTime 602 orderTime)
  sent w `shouldReturn` []
  stored ctx r `shouldReturn` r
  dryRunsPass w
  advance ctx 1
  void $ steps w r >>= awaiting
  nrPhase <$> stored ctx r `shouldReturn` PCommitting
  length <$> sent w `shouldReturn` 1

testStaleIntent :: IO ()
testStaleIntent = withWorker $ \w@W {wCtx = ctx} -> do
  -- requeued by Try again with its intent, its commitment lapsed
  r <- paidIn ctx aliceOrder $ \o -> (openPhase o) {nrPhase = PQueued, nrReveal = Just reveal}
  answerCommitment w r (orderSeconds - maxAge - 1)
  void $ steps w r >>= awaiting
  x <- stored ctx r
  commitTx <- newestHash w
  (nrPhase x, nrReveal x, nrRevealAfter x, nrRevealBefore x, nrCommitTx x, nrVersion x) `shouldBe` (PCommitting, Nothing, Nothing, Nothing, Just commitTx, nrVersion r + 2)
  -- requeued with its intent while a third party's commit of the commitment is live: not ours, so the intent goes
  r2 <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 2} $ \o -> (openPhase o) {nrPhase = PQueued, nrReveal = Just reveal}
  answerCommitment w r2 (orderSeconds - 60)
  steps w r2 >>= waitingUntil >>= (`shouldBe` at (orderSeconds - 60 + maxAge + 1))
  x2 <- stored ctx r2
  (nrPhase x2, nrReveal x2, nrCommitTx x2, nrRevealBefore x2) `shouldBe` (PCommitted, Nothing, Nothing, Just (orderSeconds - 60 + maxAge))
  length <$> sent w `shouldReturn` 1

testFeeAboveCap :: IO ()
testFeeAboveCap = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  r <- paidIn ctx aliceOrder id
  answerCommitment w r 0
  setFees cNode fakeTip fakeMaxFeePerGas
  steps w r >>= retrying
  stored ctx r `shouldReturn` r
  sent w `shouldReturn` []
  setFees cNode fakeTip fakeBaseFee
  void $ steps w r >>= awaiting
  nrPhase <$> stored ctx r `shouldReturn` PCommitting

testLostRace :: IO ()
testLostRace = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}, wWorker} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  answerAvailable w False
  dryRunsRevert w "NameNotAvailable(string)" $ abiTuple [Dynamic $ abiBytes "alicebakery"]
  -- held at the register check's ownerOf read, after the registration was read
  (stepping, gate) <- pausedAt cNode "eth_call" $ stepRegistration wWorker (nrId r)
  changed <- rewrite ctx r $ \o -> o {nrRevealAfter = (+ 1) <$> nrRevealAfter o}
  releaseGate gate
  timeout prompt (wait stepping) >>= maybe (fail "the step did not finish") (pure . shape) >>= (`shouldBe` "Again")
  stored ctx r `shouldReturn` changed
  -- held at the commitment's read, a fresh worker's first chain read
  r2 <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 2} id
  answerCommitment w r2 $ orderSeconds - 60
  W {wWorker = fresh} <- started ctx
  (stepping', gate') <- pausedAt cNode "eth_call" $ stepRegistration fresh (nrId r2)
  changed' <- rewrite ctx r2 $ \o -> o {nrError = Just "changed"}
  releaseGate gate'
  timeout prompt (wait stepping') >>= maybe (fail "the step did not finish") (pure . shape) >>= (`shouldBe` "Again")
  stored ctx r2 `shouldReturn` changed'

testInNameTurn :: IO ()
testInNameTurn = withWorker $ \w@W {wCtx = ctx@Ctx {cOrders}} -> do
  r <- paidIn ctx aliceOrder committedPhase
  advance ctx $ day + 1
  holding <- newEmptyMVar
  release <- newEmptyMVar
  withAsync (withLabelTurn (oeTurns cOrders) (nrLabelHash r) $ putMVar holding () >> takeMVar release) $ \_ -> do
    takeMVar holding
    withAsync (steps w r) $ \stepping -> do
      -- the lapse's write waits for the turn the holder has
      timeout prompt (atomically $ labelTurnCount (oeTurns cOrders) (nrLabelHash r) >>= check . (== 2)) >>= maybe (fail "the worker did not queue on the turn") pure
      stored ctx r `shouldReturn` r
      putMVar release ()
      timeout prompt (wait stepping) >>= maybe (fail "the step did not finish") done
  nrPhase <$> stored ctx r `shouldReturn` PFailed

testTwoWallets :: IO ()
testTwoWallets = withWorker $ \w@W {wCtx = ctx} -> do
  a <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  b <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 2} openPhase >>= revealed ctx keyB "alicebakery"
  answerExpiry ctx "alicebakery" expiry
  void $ steps w a >>= awaiting
  void $ mineNewest w True
  steps w a >>= done
  nrPhase <$> stored ctx a `shouldReturn` PRegistered
  answerOwner ctx "alicebakery" (nrOwner a)
  answerAvailable w False
  dryRunsRevert w "NameNotAvailable(string)" $ abiTuple [Dynamic $ abiBytes "alicebakery"]
  published w b (steps w b) >>= done
  x <- stored ctx b
  (nrPhase x, nrLive x, nrPrice x, nrPayment x, nrError x) `shouldBe` (PTaken, True, nrPrice b, nrPayment b, Nothing)
  length <$> sent w `shouldReturn` 1

testPendingCompetitor :: IO ()
testPendingCompetitor = withWorker $ \w@W {wCtx = ctx} -> do
  (r, code) <- codeIn ctx 8 aliceOrder openPhase >>= traverseFst (revealed ctx keyA "alicebakery")
  -- a competitor's register in the node's pending block, never mined
  answerAvailable w True
  dryRunsRevert w "NameNotAvailable(string)" $ abiTuple [Dynamic $ abiBytes "alicebakery"]
  steps w r >>= retrying
  stored ctx r `shouldReturn` r
  pcReservedBy <$> codeOf ctx code `shouldReturn` Just (nrId r)
  -- the competitor's register dropped
  dryRunsPass w
  void $ steps w r >>= awaiting
  nrPhase <$> stored ctx r `shouldReturn` PRegistering

testRevertTaken :: IO ()
testRevertTaken = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  (r, code) <- codeIn ctx 8 aliceOrder openPhase >>= traverseFst (revealed ctx keyA "alicebakery")
  answerAvailable w False
  -- a reason kept in the message without data: not one of the contracts' errors
  setFault cNode "eth_estimateGas" $ RpcError 3 "execution reverted: ERC721: invalid token ID" Nothing
  published w r (steps w r) >>= done
  x <- stored ctx r
  (nrPhase x, nrLive x) `shouldBe` (PTaken, False)
  PrepaidCode {pcReservedBy, pcSpentAt} <- codeOf ctx code
  (pcReservedBy, pcSpentAt) `shouldBe` (Nothing, Nothing)
  sent w `shouldReturn` []

testRevertFailed :: IO ()
testRevertFailed = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  (r, code) <- codeIn ctx 8 aliceOrder openPhase >>= traverseFst (revealed ctx keyA "alicebakery")
  answerAvailable w True
  -- viem's encodeErrorResult of InsufficientAllowance(10000, 2500)
  setFault cNode "eth_estimateGas" $ revertFault $ unhex "2a1b2dd8000000000000000000000000000000000000000000000000000000000000271000000000000000000000000000000000000000000000000000000000000009c4"
  logs <- capturingStderr $ steps w r >>= done
  logs `shouldSatisfy` T.isInfixOf "setRegistrarAllowance"
  now <- nowOf ctx
  x <- stored ctx r
  (nrPhase x, nrFailedAt x, nrError x, nrLive x) `shouldBe` (PFailed, Just now, Just "The name could not be registered", True)
  pcReservedBy <$> codeOf ctx code `shouldReturn` Just (nrId r)
  -- a revert without data while the name is available fails a paid order too
  p <- paidIn ctx dynamisOrder openPhase >>= revealed ctx keyA "dynamis"
  answerCall cNode fakeController (selector "available(string)" <> abiTuple [Dynamic $ abiBytes "dynamis"]) $ Returns $ uint256 (1 :: Int)
  answerUnowned ctx "dynamis"
  setFault cNode "eth_estimateGas" $ RpcError (-32000) "execution reverted" Nothing
  steps w p >>= done
  nrPhase <$> stored ctx p `shouldReturn` PFailed
  sent w `shouldReturn` []

testInvalidNonce :: IO ()
testInvalidNonce = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  dryRunsRevert w "InvalidNonce()" ""
  published w r (steps w r >>= waitingUntil) `shouldReturn` at (orderSeconds - minAge + maxAge + 1)
  x <- stored ctx r
  (nrPhase x, nrReveal x, nrVersion x, nrRevealBefore x) `shouldBe` (PCommitted, Nothing, nrVersion r + 1, nrRevealBefore r)
  sent w `shouldReturn` []

testCheckUnread :: IO ()
testCheckUnread = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  setFault cNode "eth_estimateGas" $ RpcError (-32000) "execution reverted" Nothing
  -- no answer for available(label)
  steps w r >>= retrying
  stored ctx r `shouldReturn` r
  -- ownerOf reverting with data is a failed read, not an unowned name
  answerAvailable w False
  answerCall cNode (registrarOf cNode) (ownerOfSelector <> labelBytes "alicebakery") $ Reverts $ unhex "37ed32e8"
  steps w r >>= retrying
  stored ctx r `shouldReturn` r
  sent w `shouldReturn` []

testCommitmentTooNew :: IO ()
testCommitmentTooNew = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  answerAvailable w True
  -- the node's latest block still before revealAfter
  dryRunsRevert w "CommitmentTooNew(bytes32,uint256,uint256)" $ nrCommitment r <> uint256 (orderSeconds - minAge) <> uint256 orderSeconds
  steps w r >>= waitingUntil >>= (`shouldBe` addUTCTime 1 orderTime)
  stored ctx r `shouldReturn` r
  sent w `shouldReturn` []

testMinedRevertTaken :: IO ()
testMinedRevertTaken = withWorker $ \w@W {wCtx = ctx} -> do
  (r, code) <- codeIn ctx 8 aliceOrder openPhase >>= traverseFst (revealed ctx keyA "alicebakery")
  answerAvailable w True
  void $ steps w r >>= awaiting
  -- another register for the name mined first
  answerAvailable w False
  void $ mineNewest w False
  steps w r >>= done
  x <- stored ctx r
  (nrPhase x, nrLive x) `shouldBe` (PTaken, False)
  pcReservedBy <$> codeOf ctx code `shouldReturn` Nothing

testExpiredIntent :: IO ()
testExpiredIntent = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder (\o -> (openPhase o) {nrRevealBefore = Just (rvDeadline reveal)}) >>= revealed ctx keyA "alicebakery"
  answerAvailable w True
  advance ctx $ fromIntegral (rvDeadline reveal - orderSeconds) + 1
  steps w r >>= done
  nrPhase <$> stored ctx r `shouldReturn` PFailed
  sent w `shouldReturn` []

testFailedOnlySettled :: IO ()
testFailedOnlySettled = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}, wSender} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  r2 <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 2} (\o -> (openPhase o) {nrRevealBefore = Just (orderSeconds + 3 * 86400)}) >>= revealed ctx keyB "alicebakery"
  answerAvailable w True
  p <- steps w r >>= awaiting
  -- past revealBefore, the register in flight can still be mined
  advance ctx $ 2 * day
  steps w r >>= awaiting >>= (`shouldBe` ptHash p) . ptHash
  nrPhase <$> stored ctx r `shouldReturn` PRegistering
  void $ mineNewest w False
  steps w r >>= done
  nrPhase <$> stored ctx r `shouldReturn` PFailed
  -- a register whose nonce another transaction took, while the owner holds the name: it went through
  p2 <- steps w r2 >>= awaiting
  useNextNonce cNode
  followTransactions wSender
  steps w r2 >>= awaiting >>= (`shouldBe` ptHash p2) . ptHash
  replicateM_ 2 $ mine cNode []
  followTransactions wSender
  answerOwner ctx "alicebakery" (nrOwner r2)
  answerExpiry ctx "alicebakery" expiry
  steps w r2 >>= done
  -- judged from chain state with no hash known to have mined: no register transaction (contract section 6)
  x2 <- stored ctx r2
  (nrPhase x2, nrExpiresAt x2, nrRegisterTx x2, nrRegisterBlock x2) `shouldBe` (PRegistered, Just expiry, Nothing, Nothing)

testLapseFailed :: IO ()
testLapseFailed = withWorker $ \w@W {wCtx = ctx} -> do
  (r, code) <- codeIn ctx 8 aliceOrder committedPhase
  steps w r >>= waitingUntil >>= (`shouldBe` at (orderSeconds + maxAge + 1))
  advance ctx $ day + 1
  published w r (steps w r) >>= done
  now <- nowOf ctx
  x <- stored ctx r
  (nrPhase x, nrFailedAt x, nrError x, nrLive x) `shouldBe` (PFailed, Just now, Just "The name was not signed in time", True)
  pcReservedBy <$> codeOf ctx code `shouldReturn` Just (nrId r)

testLapsePoll :: IO ()
testLapsePoll = withWorker $ \w@W {wCtx = ctx@Ctx {cClient}} -> do
  r <- paidIn ctx aliceOrder committedPhase
  advance ctx $ day + 1
  let path = registrationPath (nrId r) <> "?seenVersion=" <> show (nrVersion r)
  -- the lapse alone changes no version, so a poll with the version seen before it is held
  poll <- held ctx (0, 1) path
  steps w r >>= done
  (status, view) <- woken "the poll held at the lapse" poll
  (status, field "version" view, field "phase" view) `shouldBe` (200, Just (J.toJSON $ nrVersion r + 1), Just "failed")
  started' <- getCurrentTime
  (status', view') <- timeout prompt (webGet cClient path >>= \resp -> (statusOf resp,) <$> jsonObject resp) >>= maybe (fail "the old version was held") pure
  waited <- (`diffUTCTime` started') <$> getCurrentTime
  (status', field "phase" view', waited < 2) `shouldBe` (200, Just "failed", True)

testRestart :: IO ()
testRestart = withWorker $ \w@W {wCtx = ctx@Ctx {cStore}, wSender} -> do
  (r, code) <- codeIn ctx 8 aliceOrder id
  answerExpiry ctx "alicebakery" expiry
  answerCommitment w r 0
  -- the commit saved and sent, the service stopped before recording committing
  Right p <- submitTransaction wSender (TxCommit $ nrId r) fakeController (commitCalldata $ nrCommitment r)
  w1 <- started ctx
  steps w1 r >>= awaiting >>= (`shouldBe` ptHash p) . ptHash
  x <- stored ctx r
  (nrPhase x, nrCommitTx x) `shouldBe` (PCommitting, Just (ptHash p))
  length <$> sent w `shouldReturn` 1
  commitBlock <- mineNewest w1 True
  t <- blockTime w1 commitBlock
  answerCommitment w r t
  w2 <- started ctx
  void $ steps w2 r >>= waitingUntil
  nrPhase <$> stored ctx r `shouldReturn` PCommitted
  void $ revealed ctx keyA "alicebakery" r
  advance ctx $ fromIntegral (t + minAge) - fromIntegral orderSeconds
  register <- steps w2 r >>= awaiting
  w3 <- started ctx
  steps w3 r >>= awaiting >>= (`shouldBe` ptHash register) . ptHash
  nrPhase <$> stored ctx r `shouldReturn` PRegistering
  void $ mineNewest w3 True
  w4 <- started ctx
  steps w4 r >>= done
  nrPhase <$> stored ctx r `shouldReturn` PRegistered
  isJust . pcSpentAt <$> codeOf ctx code `shouldReturn` True
  map ntStatus <$> withTransaction cStore (`registrationTransactions` nrId r) `shouldReturn` [TSMined, TSMined]
  length <$> sent w `shouldReturn` 2

testRestartRegisterSaved :: IO ()
testRestartRegisterSaved = withWorker $ \w@W {wCtx = ctx@Ctx {cOrders}, wSender} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  rv <- maybe (fail "no reveal") pure $ nrReveal r
  -- the register saved and sent, the service stopped before recording registering
  Right p <- submitTransaction wSender (TxRegister $ nrId r) fakeSponsor (registerWithCreditCalldata $ intentOf (oeConfig cOrders) r rv)
  w1 <- started ctx
  steps w1 r >>= awaiting >>= (`shouldBe` ptHash p) . ptHash
  x <- stored ctx r
  (nrPhase x, nrRegisterTx x) `shouldBe` (PRegistering, Just (ptHash p))
  steps w1 r >>= awaiting >>= (`shouldBe` ptHash p) . ptHash
  length <$> sent w `shouldReturn` 1

testRunWorkers :: IO ()
testRunWorkers = withWorker $ \w@W {wCtx = ctx@Ctx {cNode, cOrders}, wWorker} -> do
  r <- paidIn ctx aliceOrder id
  (unpaid, _) <- placed ctx (byB aliceOrder) {oCommitment = commitmentOf 2}
  answerCommitment w r 0
  withAsync (runWorkers wWorker) $ \_ -> do
    timeout prompt (awaitSent cNode 1) >>= maybe (fail "no commit was sent") (const $ pure ())
    changed <- awaitChange (oePhases cOrders) (nrId r) (nrVersion <$> stored ctx r) (nrVersion r) prompt
    changed `shouldBe` True
    nrPhase <$> stored ctx r `shouldReturn` PCommitting
  nrPhase <$> invoiceRegistration ctx unpaid `shouldReturn` PUnpaid
  length <$> sent w `shouldReturn` 1

testCodeSweep :: IO ()
testCodeSweep = withWorker $ \W {wCtx = ctx, wWorker} -> do
  (old, oldCode) <- codeIn ctx 8 aliceOrder failedPhase
  (recent, recentCode) <- codeIn ctx 7 dynamisOrder $ \x -> (failedPhase x) {nrFailedAt = Just $ addUTCTime (2 * day) orderTime}
  -- failed long ago, then requeued by Try again
  (requeued, requeuedCode) <- codeIn ctx 8 (byB aliceOrder) {oCommitment = commitmentOf 2} $ \x -> (failedPhase x) {nrPhase = PQueued}
  advance ctx $ 31 * day
  sweepFailedCodes wWorker
  map pcReservedBy <$> mapM (codeOf ctx) [oldCode, recentCode, requeuedCode] `shouldReturn` [Nothing, Just (nrId recent), Just (nrId requeued)]
  mapM (stored ctx) [old, recent, requeued] `shouldReturn` [old, recent, requeued]

testLandedAtDepth :: IO ()
testLandedAtDepth = withOrders $ \ctx@Ctx {cNode, cOrders} -> do
  r <- paidIn ctx aliceOrder failedPhase
  answerUnowned ctx "alicebakery"
  answerExpiry ctx "alicebakery" expiry
  -- the owner's own register, mined in the latest block
  latest <- mine cNode []
  answerCallFrom cNode latest (registrarOf cNode) (ownerOfSelector <> labelBytes "alicebakery") $ Returns $ addressWord $ nrOwner r
  confirmLanded cOrders r `shouldReturn` LNotLanded
  stored ctx r `shouldReturn` r
  -- confirmations 3: two more blocks put it at the depth
  replicateM_ 2 $ mine cNode []
  confirmLanded cOrders r `shouldReturn` LRegistered (nrVersion r + 1)
  x <- stored ctx r
  (nrPhase x, nrExpiresAt x, nrRegisterTx x) `shouldBe` (PRegistered, Just expiry, Nothing)

testOutcomeBound :: IO ()
testOutcomeBound = bounded "the outcome's wait" $ withOrders $ \ctx@Ctx {cNode, cStore, cRandom, cClock} -> do
  r@NameRegistration {nrId, nrCommitment} <- paidIn ctx aliceOrder id
  chain <- newChainClient cRandom $ fakeChainConfig cNode
  sender <- newSender cRandom chain cStore (readIORef cClock)
  Right p <- submitTransaction sender (TxCommit nrId) fakeController (commitCalldata nrCommitment)
  started' <- getCurrentTime
  awaitOutcome shortWait p `shouldReturn` Nothing
  waited <- (`diffUTCTime` started') <$> getCurrentTime
  waited `shouldSatisfy` (< 2)
  let TxHash h = ptHash p
  block <- mine cNode [(h, True)]
  replicateM_ 2 $ mine cNode []
  followTransactions sender
  awaitOutcome shortWait p `shouldReturn` Just (TOConfirmed (ptHash p) Receipt {receiptSuccess = True, receiptBlock = block})
  stored ctx r `shouldReturn` r
  where
    shortWait = 300000
