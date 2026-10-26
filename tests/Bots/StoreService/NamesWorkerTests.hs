{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesWorkerTests (namesWorkerTests) where

import Bots.StoreService.ConfigTests (termsVersion)
import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (availableCall, maxCommitmentAgeCall, minCommitmentAgeCall, noncesCall, registrarOf, unhex)
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesOwnerTests (answerExpiry, answerFree, answerOwner, answerUnowned, byB, codeIn, committedPhase, dynamisOrder, failedPhase, labelBytes, ownerOfSelector, paidIn, rewrite, sentRegister, stored)
import Bots.StoreService.NamesPhasesTests (held, prompt, registrationPath, woken)
import Bots.StoreService.NamesStoreTests (btc, insertInvoiceRow, ownerA, paidOrder, reveal)
import Bots.StoreService.WebTests (capturingStderr, jsonObject, statusOf, webGet, webRequestBody)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (wait, withAsync)
import qualified Control.Exception as E
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (forM_, replicateM, replicateM_, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (readIORef)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.SimplexName (labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Names.Abi (abiBytes, abiTuple, addressWord, AbiValue (..), hex, selector, uint256)
import StoreService.Names.Chain (Block (..), ChainClient, Receipt (..), TxHash (..), TxRequest (..), blockAt, decodeSignedTx, newChainClient)
import StoreService.Names.Orders (Landing (..), OrdersEnv (..), confirmLanded)
import StoreService.Names.Phases (RegistrationId (..), awaitChange)
import StoreService.Names.Registration (RegisterIntent (..), TermsAcceptance (..), acceptCalldata, commitCalldata, registerIntentDigest, registerWithCreditCalldata, sponsorDomain)
import StoreService.Names.Reveal (intentOf)
import StoreService.Names.Sender
import StoreService.Names.Signatures (OrderFields (..))
import StoreService.Names.Store
import StoreService.Names.Turns (labelTurnCount, withLabelTurn)
import StoreService.Names.Worker
import StoreService.Web.Http (createLimit, holdMicros, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Registration (registrationHandler, retryHandler)
import StoreService.Web.Server (WebEnv (..), webApp)
import System.Timeout (timeout)
import Test.Hspec

namesWorkerTests :: Spec
namesWorkerTests = workerTests >> takenBalanceTests

takenBalanceTests :: Spec
takenBalanceTests = describe "StoreService names: balance life" $ describe "taken" $ do
  it "credits a taken order paid wholly from a balance, and one paid partly, its whole price as a balance of its own, the source's left as it was" testTakenFromBalance
  it "leaves the order as it was when the taken write's credit is refused, in one transaction" testTakenCreditRefused
  it "wakes a held poll with the taken write, and not with a change of the order's balance" testTakenWakesPoll

workerTests :: Spec
workerTests = describe "StoreService names: worker" $ do
  describe "commit" $ do
    it "commits, waits for the reveal and revealAfter, and registers a code order, spending its code and publishing each change" testCommitWaitRegister
    it "waits for a pending commit without sending another, following its fee-bumped replacement" testPendingCommit
    it "goes to committed when the commit reverts UnexpiredCommitmentExists, in its dry run or mined, with the commitment's times" testUnexpiredCommitment
    it "sends a commit mined out of gas again" testCommitOutOfGas
    it "waits a second at the lapse second, and again when the dry run still reverts, then commits" testLapseSecond
    it "waits a second at most a few times in a row for UnexpiredCommitmentExists, logging each, then keeps the phase" testUnexpiredBounded
    it "clears a requeued order's stale intent before committing again after a lapse, and a third party's commitment's" testStaleIntent
    it "keeps the phase while the latest block needs more than the fee cap, then commits" testFeeAboveCap
  describe "writes" $ do
    it "writes nothing when the registration changed since it was read" testLostRace
    it "writes in the name's turn" testInNameTurn
  describe "register" $ do
    it "ends the later of two wallets' paid orders for one name taken, not live, with a balance of its price" testTwoWallets
    it "ends taken on any revert while the name is not available, releasing a code and keeping no balance" testRevertTaken
    it "keeps the phase on NameNotAvailable while the name still reads available, as for a pending competitor" testPendingCompetitor
    it "ends failed on a revert while the name is available, alerting on InsufficientAllowance and keeping a code" testRevertFailed
    it "clears the reveal on InvalidNonce, staying committed" testInvalidNonce
    it "relays no intent past its deadline, ending failed" testExpiredIntent
    it "keeps the phase when a read of the final check fails" testCheckUnread
    it "waits a second when the dry run reverts CommitmentTooNew" testCommitmentTooNew
    it "ends a register mined with status 0 taken while the name is not available, releasing a code" testMinedRevertTaken
    it "writes failed only once no register can still be mined, and registered when the owner holds the name" testFailedOnlySettled
    it "clears the reveal on TermsNotAccepted, staying committed" testTermsNotAccepted
  describe "terms acceptance" $ do
    it "relays the reveal's acceptance to the name policy as soon as it is stored, and registers once the policy holds it" testAcceptanceRelayed
    it "relays the acceptance before the wait for revealAfter, then waits for it" testAcceptanceBeforeWait
    it "clears the reveal when the relay's dry run refuses the acceptance for new terms or a used nonce, staying committed" testAcceptanceRefused
    it "retries the relay while the policy's acceptance for the commitment cannot be read, sending nothing" testAcceptanceUnread
    it "judges a relay past the acceptance's deadline as a register past it, failing the order" testAcceptanceExpired
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
  describe "retry" $ do
    it "refuses Try again outside failed with bad_request, changing nothing, and answers an unknown order 404" testRetryNotFailed
    it "refuses Try again of a failed order whose links were refused with bad_request, changing nothing" testRetryLinksRefused
    it "registers a failed order whose register went through: by its mined register after a redeem, spending its code, and by ownerOf with no register shown" testRetryLanded
    it "answers provider_unavailable when the register check or a read for the stored intent fails, changing nothing" testRetryUnread
    it "answers a released code's error while it cannot pay again, before any read for the intent, and reserves it again while unused" testRetryReleasedCode
    it "requeues clearing failed_at, error and the register's hashes, waking a held poll" testRetryRequeue
    it "keeps the intent while our commitment is live with revealMargin left, going straight to committed" testRetryLiveCommitment
    it "clears the label and intent when the sponsor nonce moved, our live commit keeping its hashes" testRetryNonceMoved
    it "clears the label and an intent the worker would refuse, while our commitment is live with revealMargin left" testRetryRefusedIntent
    it "clears the intent of a third party's live re-commit, and the worker the commit's hashes" testRetryThirdParty
    it "clears the intent with less than revealMargin left, committing again a second after revealBefore" testRetryMargin
    it "requeues a lapsed committed order, which reads failed, answering an old version at once after it" testRetryLapsed
    it "answers registration_changed with the current view when the order changed during its checks, writing nothing" testRetryLostRace
    it "runs in the name's turn, answering registration_changed for an order moved to another name meanwhile" testRetryInNameTurn
    it "takes one create token per request" testRetryLimit

-- fixtures

data W = W
  { wCtx :: Ctx,
    wChain :: ChainClient,
    wSender :: Sender,
    wWorker :: Worker
  }

-- | The worker over the orders' store and fake node, with the registration poll and Try again as namesRoutes registers them.
withWorker :: (W -> IO a) -> IO a
withWorker action = withOrders $ \ctx@Ctx {cNode, cOrders, cWeb} -> do
  answerCall cNode fakeController minCommitmentAgeCall $ Returns $ uint256 minAge
  answerCall cNode fakeController maxCommitmentAgeCall $ Returns $ uint256 maxAge
  answerUnowned ctx "alicebakery"
  let web = cWeb {weOrders = Just cOrders}
      routes path = registrationRoute path <|> codeRoutes cOrders (weProviders web) (weWaiters web) path
      registrationRoute = \case
        ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler cOrders holdMicros rid}
        ["api", "registration", rid, "retry"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = retryHandler cOrders rid}
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

-- | The reveal the page sends, its intent signed with the owner's key.
signedReveal :: Ctx -> Secp256k1PrivateKey -> NameReveal -> NameRegistration -> IO NameReveal
signedReveal Ctx {cRandom, cOrders} key rv r = do
  let RegisterIntent {riRegistration} = intentOf (oeConfig cOrders) r rv
  sig <- either (error . show) id <$> signRecoverable cRandom key (registerIntentDigest (sponsorDomain fakeChainId fakeSponsor) riRegistration (rvNonce rv) (rvDeadline rv))
  pure rv {rvSignature = sig}

-- | The registration with the page's reveal of the label accepted.
revealed :: Ctx -> Secp256k1PrivateKey -> Text -> NameRegistration -> IO NameRegistration
revealed ctx key label r = signedReveal ctx key reveal {rvLabel = label} r >>= \rv -> rewrite ctx r (\o -> o {nrReveal = Just rv})

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

testUnexpiredBounded :: IO ()
testUnexpiredBounded = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder id
  -- lapsed by the service's clock, while the node's pending block keeps refusing a new commit
  answerCommitment w r (orderSeconds - maxAge - 1)
  dryRunsRevert w "UnexpiredCommitmentExists(bytes32)" (nrCommitment r)
  logs <- capturingStderr $ forM_ [1 .. maxUnexpiredWaits] $ \n -> do
    steps w r >>= waitingUntil >>= (`shouldBe` addUTCTime (fromIntegral n) orderTime)
    advance ctx 1
  -- the capture can also hold earlier tests' lines, so only this order's count
  let RegistrationId rid = nrId r
  length (T.breakOnAll ("registration " <> rid <> ": the commit's dry run reverted UnexpiredCommitmentExists") logs) `shouldBe` maxUnexpiredWaits
  steps w r >>= retrying
  stored ctx r `shouldReturn` r
  sent w `shouldReturn` []
  dryRunsPass w
  void $ steps w r >>= awaiting
  nrPhase <$> stored ctx r `shouldReturn` PCommitting

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
  (nrPhase x, nrLive x, nrPrice x, nrPayment x, nrError x) `shouldBe` (PTaken, False, nrPrice b, nrPayment b, Nothing)
  fmap (\Balance {blAmount, blCredited, blHold} -> (blAmount, blCredited, blHold)) <$> withTransaction (cStore ctx) (\db -> getBalance db (nrId b) orderTime)
    `shouldReturn` Just (nrPrice b, nrPrice b, Nothing)
  length <$> sent w `shouldReturn` 1

-- | The order of alicebakery, by owner A, with this commitment.
aliceFields :: Int -> OrderFields
aliceFields n = OrderFields {ofLabelHash = labelHash "alicebakery", ofLength = 11, ofDuration = yearsDuration 2 orderTime, ofCommitment = commitmentOf n, ofSealed = "sealed-alicebakery-" <> tshow n, ofAgreedTerms = termsVersion}

aliceRegistration :: Text -> Int -> NewRegistration
aliceRegistration rid n = NewRegistration {nrgId = RegistrationId rid, nrgOwner = ownerA, nrgOrder = aliceFields n, nrgYears = 2}

-- | The amount and credited of the order's balance.
balanceFigures :: Ctx -> RegistrationId -> IO (Maybe (Int, Int))
balanceFigures Ctx {cStore} rid = fmap (\Balance {blAmount, blCredited} -> (blAmount, blCredited)) <$> withTransaction cStore (\db -> getBalance db rid orderTime)

-- | The register's dry run reverts on a name someone else holds, so the registration ends taken.
endsTaken :: W -> NameRegistration -> IO ()
endsTaken w@W {wCtx = Ctx {cNode}} r = do
  answerAvailable w False
  setFault cNode "eth_estimateGas" $ RpcError 3 "execution reverted: ERC721: invalid token ID" Nothing
  published w r (steps w r) >>= done

testTakenFromBalance :: IO ()
testTakenFromBalance = withWorker $ \w@W {wCtx = ctx@Ctx {cStore}} -> do
  let price = oPrice aliceOrder
  wholly <- paidIn ctx (byB dynamisOrder) failedPhase
  partly <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 5} failedPhase
  withTransaction cStore $ \db -> creditBalance db (nrId wholly) 30000 orderTime >> creditBalance db (nrId partly) 4000 orderTime
  withTransaction cStore (\db -> createBalanceRegistration db (aliceRegistration "covered" 1) (nrId wholly) price 30000 orderTime) `shouldReturn` Right ()
  covered <- registrationById ctx (RegistrationId "covered") >>= \r -> rewrite ctx r openPhase >>= revealed ctx keyA "alicebakery"
  endsTaken w covered
  (\x -> (nrPhase x, nrLive x, nrPayment x)) <$> stored ctx covered `shouldReturn` (PTaken, False, RPBalance)
  balanceFigures ctx (nrId covered) `shouldReturn` Just (price, price)
  balanceFigures ctx (nrId wholly) `shouldReturn` Just (30000 - price, 30000)
  withTransaction cStore $ \db -> do
    insertInvoiceRow db "inv-diff" (addUTCTime 3600 orderTime) "open"
    createPaidRegistration db (aliceRegistration "diff" 2) (OTBalance (nrId partly) price 4000 (Just btc)) (paidOrder "inv-diff") {poPrice = price} orderTime `shouldReturn` Right ()
  settleNow ctx (InvoiceId "inv-diff") settled `shouldReturn` Right ISPaid
  balanceFigures ctx (nrId partly) `shouldReturn` Just (0, 4000)
  diff <- registrationById ctx (RegistrationId "diff") >>= \r -> rewrite ctx r openPhase >>= revealed ctx keyA "alicebakery"
  endsTaken w diff
  nrPhase <$> stored ctx diff `shouldReturn` PTaken
  balanceFigures ctx (nrId diff) `shouldReturn` Just (price, price)
  balanceFigures ctx (nrId partly) `shouldReturn` Just (0, 4000)

-- | A price the balance's CHECK refuses makes the credit fail inside the taken write's transaction.
testTakenCreditRefused :: IO ()
testTakenCreditRefused = withWorker $ \w@W {wCtx = ctx@Ctx {cStore, cNode}} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  withTransaction cStore $ \db -> DB.execute db "UPDATE sx_badge_service_name_registrations SET price = -1 WHERE registration_id = ?" [(\(RegistrationId t) -> t) $ nrId r]
  unchanged <- stored ctx r
  answerAvailable w False
  setFault cNode "eth_estimateGas" $ RpcError 3 "execution reverted: ERC721: invalid token ID" Nothing
  void (E.try (steps w r) :: IO (Either E.SomeException Next))
  stored ctx r `shouldReturn` unchanged
  balanceFigures ctx (nrId r) `shouldReturn` Nothing

testTakenWakesPoll :: IO ()
testTakenWakesPoll = withWorker $ \w@W {wCtx = ctx@Ctx {cStore}} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  waiting <- held ctx (0, 1) $ registrationPath (nrId r) <> "?seenVersion=" <> show (nrVersion r)
  withTransaction cStore $ \db -> creditBalance db (nrId r) 500 orderTime
  threadDelay shortWait
  isJust <$> Async.poll waiting `shouldReturn` False
  endsTaken w r
  (status, view) <- woken "the registration poll" waiting
  (status, field "phase" view) `shouldBe` (200, Just "taken")
  balanceFigures ctx (nrId r) `shouldReturn` Just (500 + oPrice aliceOrder, 500 + oPrice aliceOrder)
  where
    shortWait = 500000

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
  balanceFigures ctx (nrId r) `shouldReturn` Nothing
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

testTermsNotAccepted :: IO ()
testTermsNotAccepted = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  dryRunsRevert w "TermsNotAccepted(string)" ""
  published w r (steps w r >>= waitingUntil) `shouldReturn` at (orderSeconds - minAge + maxAge + 1)
  x <- stored ctx r
  (nrPhase x, nrReveal x) `shouldBe` (PCommitted, Nothing)
  sent w `shouldReturn` []

-- | The page's reveal with its acceptance of the policy's terms.
acceptedReveal :: Ctx -> NameRegistration -> IO NameRegistration
acceptedReveal ctx r = do
  rv <- signedReveal ctx keyA reveal {rvLabel = "alicebakery"} r
  rewrite ctx r (\o -> o {nrReveal = Just rv {rvAcceptance = Just TermsAcceptance {taTermsHash = fakeCommunityTerms, taNonce = 0, taSignature = rvSignature rv}}})

-- | The terms hash the policy holds as accepted for the registration's commitment by its owner.
answerAccepted :: W -> NameRegistration -> ByteString -> IO ()
answerAccepted W {wCtx = Ctx {cNode}} NameRegistration {nrCommitment, nrOwner} h =
  answerCall cNode fakeNamePolicy (selector "acceptedTerms(bytes32,address)" <> nrCommitment <> addressWord nrOwner) $ Returns h

testAcceptanceRelayed :: IO ()
testAcceptanceRelayed = withWorker $ \w@W {wCtx = ctx} -> do
  r@NameRegistration {nrReveal = Just NameReveal {rvDeadline, rvAcceptance = Just acceptance}} <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  answerAccepted w r $ B.replicate 32 0
  relay <- steps w r >>= awaiting
  [acceptRaw] <- sent w
  ((trTo &&& trData) <$> decodeSignedTx acceptRaw) `shouldBe` Right (fakeNamePolicy, acceptCalldata (nrCommitment r) (nrOwner r) rvDeadline acceptance)
  -- in flight, it is waited for, and nothing else is sent
  steps w r >>= awaiting >>= (`shouldBe` ptHash relay) . ptHash
  stored ctx r `shouldReturn` r
  void $ mineNewest w True
  answerAccepted w r fakeCommunityTerms
  void $ published w r $ steps w r >>= awaiting
  [_, registerRaw] <- sent w
  (trTo <$> decodeSignedTx registerRaw) `shouldBe` Right fakeSponsor
  nrPhase <$> stored ctx r `shouldReturn` PRegistering

testAcceptanceBeforeWait :: IO ()
testAcceptanceBeforeWait = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder (\o -> (openPhase o) {nrRevealAfter = Just (orderSeconds + 60)}) >>= acceptedReveal ctx
  answerAccepted w r $ B.replicate 32 0
  void $ steps w r >>= awaiting
  [acceptRaw] <- sent w
  (trTo <$> decodeSignedTx acceptRaw) `shouldBe` Right fakeNamePolicy
  void $ mineNewest w True
  answerAccepted w r fakeCommunityTerms
  steps w r >>= waitingUntil >>= (`shouldBe` at (orderSeconds + 60))
  length <$> sent w `shouldReturn` 1

testAcceptanceRefused :: IO ()
testAcceptanceRefused = forM_ ["TermsMismatch(bytes32,bytes32)", "InvalidNonce()"] $ \revert -> withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  answerAccepted w r $ B.replicate 32 0
  dryRunsRevert w revert ""
  published w r (steps w r >>= waitingUntil) `shouldReturn` at (orderSeconds - minAge + maxAge + 1)
  x <- stored ctx r
  (nrPhase x, nrReveal x) `shouldBe` (PCommitted, Nothing)
  sent w `shouldReturn` []

testAcceptanceUnread :: IO ()
testAcceptanceUnread = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  steps w r >>= retrying
  stored ctx r `shouldReturn` r
  sent w `shouldReturn` []

testAcceptanceExpired :: IO ()
testAcceptanceExpired = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  answerAccepted w r $ B.replicate 32 0
  answerAvailable w True
  dryRunsRevert w "SignatureExpired()" ""
  steps w r >>= done
  now <- nowOf ctx
  x <- stored ctx r
  (nrPhase x, nrFailedAt x, nrError x) `shouldBe` (PFailed, Just now, Just "The name could not be registered")
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
  logs <- capturingStderr $ steps w r >>= done
  let RegistrationId rid = nrId r
  logs `shouldSatisfy` T.isInfixOf ("registration " <> rid <> ": its stored intent is refused, intent expired")
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
  -- prompt is far below the 30-second hold
  (status', view') <- timeout prompt (webGet cClient path >>= \resp -> (statusOf resp,) <$> jsonObject resp) >>= maybe (fail "the old version was held") pure
  (status', field "phase" view') `shouldBe` (200, Just "failed")

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
testOutcomeBound = withOrders $ \ctx@Ctx {cNode, cStore, cRandom, cClock} -> do
  r@NameRegistration {nrId, nrCommitment} <- paidIn ctx aliceOrder id
  chain <- newChainClient cRandom $ fakeChainConfig cNode
  sender <- newSender cRandom chain cStore (readIORef cClock)
  Right p <- submitTransaction sender (TxCommit nrId) fakeController (commitCalldata nrCommitment)
  timeout prompt (awaitOutcome shortWait p) `shouldReturn` Just Nothing
  let TxHash h = ptHash p
  block <- mine cNode [(h, True)]
  replicateM_ 2 $ mine cNode []
  followTransactions sender
  awaitOutcome shortWait p `shouldReturn` Just (TOConfirmed (ptHash p) Receipt {receiptSuccess = True, receiptBlock = block})
  stored ctx r `shouldReturn` r
  where
    shortWait = 300000

-- retry

retryOf :: Ctx -> NameRegistration -> IO (Int, Maybe J.Object)
retryOf ctx NameRegistration {nrId} = post ctx (registrationPath nrId <> "/retry") ""

-- | The view of an accepted Try again, failing the test otherwise.
retried :: Ctx -> NameRegistration -> IO J.Object
retried ctx r =
  retryOf ctx r >>= \case
    (200, Just v) -> pure v
    other -> fail $ "retry refused: " <> show other

viewOf :: Ctx -> NameRegistration -> IO J.Object
viewOf Ctx {cClient} NameRegistration {nrId} = webGet cClient (registrationPath nrId) >>= jsonObject

answerNonce :: W -> Word64 -> IO ()
answerNonce W {wCtx = Ctx {cNode}} n = answerCall cNode fakeSponsor noncesCall $ Returns $ uint256 n

txJSON :: Maybe TxHash -> Maybe J.Value
txJSON = Just . maybe J.Null (\(TxHash h) -> J.String $ hex h)

-- | Committed by the worker's own commit; its block's time.
committedByUs :: W -> NameRegistration -> IO (NameRegistration, Word64)
committedByUs w@W {wCtx = ctx} r = do
  answerCommitment w r 0
  void $ steps w r >>= awaiting
  t <- mineNewest w True >>= blockTime w
  answerCommitment w r t
  void $ steps w r >>= waitingUntil
  (,t) <$> stored ctx r

-- | Failed after its register reverted, with the intent the page signed for the commit's window.
failedAfterRegister :: W -> NameRegistration -> IO NameRegistration
failedAfterRegister W {wCtx = ctx} r = do
  rv <- signedReveal ctx keyA reveal {rvDeadline = fromJust $ nrRevealBefore r} r
  h <- sentRegister ctx r 2 TSReverted
  rewrite ctx r $ \o -> (failedPhase o) {nrReveal = Just rv, nrRegisterTx = Just h, nrRegisterBlock = Just 120}

testRetryNotFailed :: IO ()
testRetryNotFailed = withWorker $ \W {wCtx = ctx} -> do
  queued <- paidIn ctx aliceOrder id
  committed <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 2} committedPhase
  registeredOrder <- paidIn ctx dynamisOrder $ \o -> o {nrPhase = PRegistered, nrExpiresAt = Just expiry}
  taken <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 4} $ \o -> o {nrPhase = PTaken}
  forM_ [queued, committed, registeredOrder, taken] $ \r -> do
    (nrPhase r,) <$> retryOf ctx r `shouldReturn` (nrPhase r, refusedWith 400 "bad_request")
    stored ctx r `shouldReturn` r
  retryOf ctx queued {nrId = RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g"} `shouldReturn` refusedWith 404 "not_found"

testRetryLinksRefused :: IO ()
testRetryLinksRefused = withWorker $ \W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder $ \o -> (failedPhase o) {nrLinksRefusedAt = Just orderTime}
  -- unowned with no reveal stored, so only its refused links keep it from being queued again
  answerUnowned ctx "alicebakery"
  retryOf ctx r `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r

testRetryLanded :: IO ()
testRetryLanded = withWorker $ \W {wCtx = ctx} -> do
  -- redeemed since its register mined: the name left the owner, and the register consumed its commitment
  (redeemed, code) <- codeIn ctx 8 aliceOrder failedPhase
  TxHash minedHash <- sentRegister ctx redeemed 1 TSMined
  answerExpiry ctx "alicebakery" expiry
  v <- retried ctx redeemed
  (field "phase" v, field "registerTx" v, field "expiresAt" v, field "error" v) `shouldBe` (Just "registered", Just (J.String $ hex minedHash), Just (J.toJSON expiry), Just J.Null)
  x <- stored ctx redeemed
  (nrPhase x, nrVersion x, nrFailedAt x, nrLive x) `shouldBe` (PRegistered, nrVersion redeemed + 1, Nothing, False)
  isJust . pcSpentAt <$> codeOf ctx code `shouldReturn` True
  -- no register known to have mined: judged by ownerOf, none is shown
  owned <- paidIn ctx dynamisOrder failedPhase
  answerOwner ctx "dynamis" (nrOwner owned)
  answerExpiry ctx "dynamis" expiry
  v' <- retried ctx owned
  (field "phase" v', field "registerTx" v', field "expiresAt" v') `shouldBe` (Just "registered", Just J.Null, Just (J.toJSON expiry))

testRetryUnread :: IO ()
testRetryUnread = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  r <- paidIn ctx aliceOrder failedPhase
  -- a revert with data is a failed read, not an unowned name
  answerCall cNode (registrarOf cNode) (ownerOfSelector <> labelBytes "alicebakery") $ Reverts $ unhex "37ed32e8"
  retryOf ctx r `shouldReturn` refusedWith 503 "provider_unavailable"
  stored ctx r `shouldReturn` r
  -- with an intent stored, the sponsor nonce and then the commitment unanswered
  p <- paidIn ctx dynamisOrder openPhase >>= revealed ctx keyA "dynamis" >>= \x -> rewrite ctx x failedPhase
  answerUnowned ctx "dynamis"
  retryOf ctx p `shouldReturn` refusedWith 503 "provider_unavailable"
  answerNonce w $ rvNonce reveal
  retryOf ctx p `shouldReturn` refusedWith 503 "provider_unavailable"
  stored ctx p `shouldReturn` p

testRetryReleasedCode :: IO ()
testRetryReleasedCode = withWorker $ \W {wCtx = ctx@Ctx {cStore}} -> do
  spender <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 20} id
  holder <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 21} id
  let codeOrder n change = do
        let label = "codeorder" <> tshow n
        answerFree ctx label
        codeIn ctx 8 aliceOrder {oLabel = label, oCommitment = commitmentOf (30 + n)} change
      released (r, code) = (r, code) <$ (withTransaction cStore (\db -> releasePrepaidCode db (pcId code) (nrId r)) `shouldReturn` True)
      refusals :: [(Text, PrepaidCode -> DB.Connection -> IO (), (Int, Maybe J.Object))]
      refusals =
        [ ("spent", \PrepaidCode {pcId} db -> reservePrepaidCode db pcId (nrId spender) orderTime >> void (spendPrepaidCode db pcId (nrId spender) orderTime), refusedWith 409 "code_used"),
          ("held", \PrepaidCode {pcId} db -> void $ reservePrepaidCode db pcId (nrId holder) orderTime, refusedWith 409 "code_reserved"),
          ("revoked", \PrepaidCode {pcCodeHash} db -> void $ revokePrepaidCode db pcCodeHash orderTime, refusedWith 400 "code_invalid"),
          ("expired", \PrepaidCode {pcCodeHash} db -> expire db pcCodeHash, refusedWith 409 "code_expired")
        ]
      expire :: DB.Connection -> ByteString -> IO ()
      expire db codeHash = DB.execute db "UPDATE sx_badge_service_name_prepaid_codes SET expires_at = ? WHERE code_hash = ?" (orderTime, DB.Binary codeHash)
  forM_ (zip [1 :: Int ..] refusals) $ \(n, (what, change, refusal)) -> do
    (r, code) <- codeOrder n failedPhase >>= released
    withTransaction cStore $ change code
    (what,) <$> retryOf ctx r `shouldReturn` (what, refusal)
    stored ctx r `shouldReturn` r
  -- with an intent stored and the sponsor nonce unanswered, the code's error still comes first
  (withIntent, spentCode) <- codeOrder 5 openPhase >>= traverseFst (revealed ctx keyA "codeorder5") >>= traverseFst (\x -> rewrite ctx x failedPhase) >>= released
  -- a registration holds one code at most
  answerFree ctx "anotherpays"
  spender' <- paidIn ctx (byB aliceOrder) {oLabel = "anotherpays", oCommitment = commitmentOf 22} id
  withTransaction cStore $ \db -> reservePrepaidCode db (pcId spentCode) (nrId spender') orderTime >> void (spendPrepaidCode db (pcId spentCode) (nrId spender') orderTime)
  retryOf ctx withIntent `shouldReturn` refusedWith 409 "code_used"
  -- a code its order still holds stays good for it, though revoked and expired since
  (holding, heldCode@PrepaidCode {pcCodeHash}) <- codeOrder 6 failedPhase
  withTransaction cStore $ \db -> revokePrepaidCode db pcCodeHash orderTime >> expire db pcCodeHash
  field "phase" <$> retried ctx holding `shouldReturn` Just "queued"
  pcReservedBy <$> codeOf ctx heldCode `shouldReturn` Just (nrId holding)
  -- a released code still unused is reserved again
  (unused, unusedCode) <- codeOrder 7 failedPhase >>= released
  field "phase" <$> retried ctx unused `shouldReturn` Just "queued"
  (pcReservedBy &&& pcSpentAt) <$> codeOf ctx unusedCode `shouldReturn` (Just (nrId unused), Nothing)

testRetryRequeue :: IO ()
testRetryRequeue = withWorker $ \w@W {wCtx = ctx} -> do
  committed <- paidIn ctx aliceOrder openPhase
  h <- sentRegister ctx committed 1 TSReverted
  r <- rewrite ctx committed $ \o -> (failedPhase o) {nrRegisterTx = Just h, nrRegisterBlock = Just 120}
  v <- published w r $ retried ctx r
  (field "phase" v, field "registerTx" v, field "error" v, field "version" v) `shouldBe` (Just "queued", Just J.Null, Just J.Null, Just (J.toJSON $ nrVersion r + 1))
  x <- stored ctx r
  (nrPhase x, nrFailedAt x, nrError x, nrRegisterTx x, nrRegisterBlock x, nrLive x) `shouldBe` (PQueued, Nothing, Nothing, Nothing, Nothing, True)
  (nrCommitTx x, nrCommitBlock x, nrRevealAfter x, nrRevealBefore x) `shouldBe` (nrCommitTx r, nrCommitBlock r, nrRevealAfter r, nrRevealBefore r)

testRetryLiveCommitment :: IO ()
testRetryLiveCommitment = withWorker $ \w@W {wCtx = ctx} -> do
  (r, t) <- paidIn ctx aliceOrder id >>= committedByUs w
  f <- failedAfterRegister w r
  answerNonce w $ rvNonce reveal
  v <- retried ctx f
  (field "phase" v, field "label" v) `shouldBe` (Just "queued", Just "alicebakery")
  steps w f >>= waitingUntil >>= (`shouldBe` at (t + minAge))
  x <- stored ctx f
  (nrPhase x, nrReveal x, nrCommitTx x, nrCommitBlock x, nrRevealBefore x) `shouldBe` (PCommitted, nrReveal f, nrCommitTx f, nrCommitBlock f, Just (t + maxAge))
  length <$> sent w `shouldReturn` 1

testRetryNonceMoved :: IO ()
testRetryNonceMoved = withWorker $ \w@W {wCtx = ctx} -> do
  (r, t) <- paidIn ctx aliceOrder id >>= committedByUs w
  f <- failedAfterRegister w r
  answerNonce w $ rvNonce reveal + 1
  v <- retried ctx f
  (field "phase" v, field "label" v, field "commitTx" v) `shouldBe` (Just "queued", Just J.Null, txJSON (nrCommitTx f))
  nrReveal <$> stored ctx f `shouldReturn` Nothing
  steps w f >>= waitingUntil >>= (`shouldBe` at (t + maxAge + 1))
  x <- stored ctx f
  (nrPhase x, nrReveal x, nrCommitTx x, nrCommitBlock x, nrRevealBefore x) `shouldBe` (PCommitted, Nothing, nrCommitTx f, nrCommitBlock f, Just (t + maxAge))
  field "commitTx" <$> viewOf ctx f `shouldReturn` txJSON (nrCommitTx f)

testRetryRefusedIntent :: IO ()
testRetryRefusedIntent = withWorker $ \w@W {wCtx = ctx} -> do
  (r, _) <- paidIn ctx aliceOrder id >>= committedByUs w
  f <- failedAfterRegister w r
  -- not signed by the owner, as the worker's check reads an intent after a change of the [chain] settings
  rv <- signedReveal ctx keyB reveal {rvDeadline = fromJust $ nrRevealBefore r} r
  f' <- rewrite ctx f $ \o -> o {nrReveal = Just rv}
  answerNonce w $ rvNonce reveal
  v <- retried ctx f'
  (field "phase" v, field "label" v) `shouldBe` (Just "queued", Just J.Null)
  nrReveal <$> stored ctx f' `shouldReturn` Nothing

testRetryThirdParty :: IO ()
testRetryThirdParty = withWorker $ \w@W {wCtx = ctx} -> do
  (r, t) <- paidIn ctx aliceOrder id >>= committedByUs w
  f <- failedAfterRegister w r
  answerNonce w $ rvNonce reveal
  -- our commitment lapsed, and someone committed it again
  let t' = t + maxAge + 100
  answerCommitment w f t'
  advance ctx $ fromIntegral (t' + 10 - orderSeconds)
  v <- retried ctx f
  (field "label" v, field "commitTx" v) `shouldBe` (Just J.Null, txJSON (nrCommitTx f))
  steps w f >>= waitingUntil >>= (`shouldBe` at (t' + maxAge + 1))
  x <- stored ctx f
  (nrPhase x, nrReveal x, nrCommitTx x, nrCommitBlock x, nrRevealBefore x) `shouldBe` (PCommitted, Nothing, Nothing, Nothing, Just (t' + maxAge))
  field "commitTx" <$> viewOf ctx f `shouldReturn` Just J.Null
  length <$> sent w `shouldReturn` 1

testRetryMargin :: IO ()
testRetryMargin = withWorker $ \w@W {wCtx = ctx} -> do
  (r, t) <- paidIn ctx aliceOrder id >>= committedByUs w
  f <- failedAfterRegister w r
  answerNonce w $ rvNonce reveal
  let lapse = t + maxAge
  advance ctx $ fromIntegral (lapse - 600 - orderSeconds)
  field "label" <$> retried ctx f `shouldReturn` Just J.Null
  steps w f >>= waitingUntil >>= (`shouldBe` at (lapse + 1))
  advance ctx 601
  void $ steps w f >>= awaiting
  commitTx <- newestHash w
  x <- stored ctx f
  (nrPhase x, nrReveal x, nrCommitTx x, nrRevealBefore x) `shouldBe` (PCommitting, Nothing, Just commitTx, Nothing)
  length <$> sent w `shouldReturn` 2

testRetryLapsed :: IO ()
testRetryLapsed = withWorker $ \w@W {wCtx = ctx@Ctx {cClient}} -> do
  (r, code) <- codeIn ctx 8 aliceOrder committedPhase
  advance ctx $ day + 1
  -- the lapse alone changes no version, so a poll with the version seen before it is held
  let path = registrationPath (nrId r) <> "?seenVersion=" <> show (nrVersion r)
  poll <- held ctx (0, 1) path
  v <- retried ctx r
  (field "phase" v, field "version" v, field "error" v) `shouldBe` (Just "queued", Just (J.toJSON $ nrVersion r + 1), Just J.Null)
  (status, view) <- woken "the poll held at the lapse" poll
  (status, field "phase" view) `shouldBe` (200, Just "queued")
  -- prompt is far below the 30-second hold
  (status', view') <- timeout prompt (webGet cClient path >>= \resp -> (statusOf resp,) <$> jsonObject resp) >>= maybe (fail "the old version was held") pure
  (status', field "version" view') `shouldBe` (200, Just (J.toJSON $ nrVersion r + 1))
  pcReservedBy <$> codeOf ctx code `shouldReturn` Just (nrId r)
  -- the commitment lapsed, so the worker commits it again
  answerCommitment w r $ fromJust (nrRevealBefore r) - maxAge
  void $ steps w r >>= awaiting
  nrPhase <$> stored ctx r `shouldReturn` PCommitting

testRetryLostRace :: IO ()
testRetryLostRace = withWorker $ \W {wCtx = ctx@Ctx {cNode}} -> do
  r <- paidIn ctx aliceOrder failedPhase
  -- held at the register check's ownerOf read, in the name's turn
  (retrying', gate) <- pausedAt cNode "eth_call" $ retryOf ctx r
  changed <- rewrite ctx r id
  releaseGate gate
  answer <- timeout prompt (wait retrying') >>= maybe (fail "the retry did not finish") pure
  view <- viewOf ctx r
  answer `shouldBe` (409, Just $ KM.fromList [("error", "registration_changed"), ("registration", J.Object view)])
  field "version" view `shouldBe` Just (J.toJSON $ nrVersion r + 1)
  stored ctx r `shouldReturn` changed

testRetryInNameTurn :: IO ()
testRetryInNameTurn = withWorker $ \W {wCtx = ctx@Ctx {cOrders}} -> do
  r <- paidIn ctx aliceOrder failedPhase
  moving <- paidIn ctx (byB aliceOrder) {oCommitment = commitmentOf 2} failedPhase
  answerUnowned ctx "dynamis"
  let turns = oeTurns cOrders
      whileHeld :: IO a -> IO () -> IO a
      whileHeld request meanwhile = do
        holding <- newEmptyMVar
        release <- newEmptyMVar
        withAsync (withLabelTurn turns (nrLabelHash r) $ putMVar holding () >> takeMVar release) $ \_ -> do
          takeMVar holding
          withAsync request $ \requesting -> do
            timeout prompt (atomically $ labelTurnCount turns (nrLabelHash r) >>= check . (== 2)) >>= maybe (fail "the retry did not queue on the turn") pure
            meanwhile
            putMVar release ()
            timeout prompt (wait requesting) >>= maybe (fail "the retry did not finish") pure
  (status, v) <- whileHeld (retryOf ctx r) (stored ctx r `shouldReturn` r)
  (status, field "phase" =<< v) `shouldBe` (200, Just "queued")
  -- moved to another name while the retry waited for the old name's turn
  (status', v') <- whileHeld (retryOf ctx moving) (void $ rewrite ctx moving $ \o -> o {nrLabelHash = labelHash "dynamis", nrLength = 7})
  (status', field "error" =<< v') `shouldBe` (409, Just "registration_changed")
  nrPhase <$> stored ctx moving `shouldReturn` PFailed

testRetryLimit :: IO ()
testRetryLimit = withWorker $ \W {wCtx = ctx@Ctx {cClient}} -> do
  r <- paidIn ctx aliceOrder id
  let fromClient = statusOf <$> webRequestBody cClient "POST" (registrationPath (nrId r) <> "/retry") [("X-Forwarded-For", "203.0.113.8")] ""
  replicateM 5 fromClient `shouldReturn` replicate 5 400
  fromClient `shouldReturn` 429
