{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesWorkerTests (namesWorkerTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesBalancesTests (figures)
import Bots.StoreService.NamesContractsTests (availableCall, maxCommitmentAgeCall, minCommitmentAgeCall, newPolicy, registrarOf)
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesOwnerTests (answerExpiry, answerFree, answerOwner, answerUnowned, byB, codeIn, committedPhase, dynamisOrder, failedPhase, labelBytes, linksRefusedPhase, move, moveTo, ownerOfSelector, paidIn, rewrite, sentRegister, stored)
import Bots.StoreService.NamesPhasesTests (held, prompt, registrationPath, woken)
import Bots.StoreService.NamesStoreTests (btc, hashFixture, insertInvoiceRow, ownerA, paidOrder, reveal)
import Bots.StoreService.WebTests (answerOf, capturingStderr, fieldOf, jsonObject, nameRefusedWith, phaseOf, phaseValue, refusedWith, statusOf, stubCalls, webGet, withApp)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (wait, withAsync)
import qualified Control.Exception as E
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTVarIO)
import Control.Monad (forM_, replicateM_, void, when)
import Data.ByteString (ByteString)
import qualified Data.Aeson as J
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Simplex.Chat.Badges.Ledger (addMonths)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word8, Word64)
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..))
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.SimplexName (labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Balances (RefundRecord (..), RefundRefusal (..), recordRefund)
import StoreService.Names.Chain (AbiValue (..), Block (..), ChainClient, Hex32 (..), RevertName (..), RevertReason (..), TxHash (..), abiBytes, abiTuple, addressWord, blockAt, decodeSignedTx, newChainClient, selector, uint256)
import StoreService.Names.Contracts (namePolicy)
import StoreService.Names.Orders (Landing (..), OrdersEnv (..), confirmLanded)
import StoreService.Names.Phases (awaitChange)
import StoreService.Names.Registration (OrderFields (..), RegisterIntent (..), RegistrationId (..), TermsAcceptance (..), acceptCalldata, acceptanceDigest, commitCalldata, policyDomain, registerIntentDigest, registerWithCreditCalldata, sponsorDomain, termsKind)
import StoreService.Names.Reveal (intentOf)
import StoreService.Names.Sender
import StoreService.Names.Store
import StoreService.Names.Turns (labelTurnCount, turnsCount, withLabelTurn)
import StoreService.Names.Worker
import StoreService.Store.Invoices (InvoiceRow (..), getInvoice)
import StoreService.Web.Http (createLimit, holdMicros, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Owner (retargetHandler)
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
    it "registers a code order with codes_only on, from the code check to the registered view, refusing a paid order" testCodesOnlyRegister
    it "waits for a pending commit without sending another, following its fee-bumped replacement" testPendingCommit
    it "goes to committed when the commit reverts UnexpiredCommitmentExists, in its dry run or mined, with the commitment's times" testUnexpiredCommitment
    it "sends a commit mined out of gas again" testCommitOutOfGas
    it "retries, writing nothing, while the commitment's reveal window would pass 2^64 seconds" testRevealWindowPastRange
    it "waits a second at the lapse second, retries as usual when the dry run still reverts, then commits" testLapseSecond
    it "keeps a requeued order's intent while it commits again after a lapse, and over a third party's commitment" testIntentAcrossCommits
    it "keeps the phase while the latest block needs more than the fee cap, then commits" testFeeAboveCap
    it "commits again after commits mined with status 1 on every lapse while a move counts them, and fails an order whose commits reverted the most times an order may" testMinedCommitsUncounted
  describe "writes" $ do
    it "writes nothing when the registration changed since it was read" testLostRace
    it "writes in the name's turn" testInNameTurn
  describe "register" $ do
    it "ends taken on any revert while the name is not available, releasing a code and keeping no balance" testRevertTaken
    it "waits while the name reads taken only above the confirmation depth, and ends taken once it reads so at the depth" testTakenAtDepth
    it "keeps the phase on NameNotAvailable while the name still reads available, as for a pending competitor" testPendingCompetitor
    it "retries a revert while the name is available and the commitment live, and ends failed on it once the commitment lapsed, keeping a code" testRevertFailed
    it "keeps an order committed while the sponsor allowance is used up, and registers it after a top-up" testAllowanceUsedUp
    it "keeps an order waiting on the allowance past its commitment's lapse, sending nothing, then commits again and registers with its stored intent" testAllowanceAcrossLapse
    it "fails an order that sent the most registers an order may, sending nothing, refuses its Try again and a move, and registers it on a move once its owner holds the name" testSendsCapped
    it "clears the reveal on InvalidNonce or TermsNotAccepted, staying committed, and reads the name policy again on TermsNotAccepted" testInvalidNonce
    it "relays no intent past its deadline, clearing the reveal and staying committed" testExpiredIntent
    it "fails an order at once, logging and reporting it, when its stored signature is refused under another chain id, sending nothing" testSignatureUnderOtherConfig
    it "keeps the phase when a read of the final check fails" testCheckUnread
    it "waits a second when the dry run reverts CommitmentTooNew" testCommitmentTooNew
    it "ends a register mined with status 0 taken while the name is not available, releasing a code" testMinedRevertTaken
    it "sends a register again only once none can still be mined, and writes registered when the owner holds the name" testFailedOnlySettled
  describe "terms acceptance" $ do
    it "relays the reveal's acceptance of the terms of the name's kind to the name policy as soon as it is stored, and registers once the policy holds it" testAcceptanceRelayed
    it "relays the acceptance before the wait for revealAfter, then waits for it" testAcceptanceBeforeWait
    it "clears the reveal when the relay's dry run refuses the acceptance for new terms or a used nonce, staying committed" testAcceptanceRefused
    it "registers, keeping the reveal, when the relay's dry run reverts while the policy holds the acceptance another sent first" testAcceptanceSentFirst
    it "registers, keeping the reveal, when the relay is mined with status 0 while the policy holds the acceptance another sent first" testAcceptanceMinedFailed
    it "retries the relay while the policy's acceptance for the commitment cannot be read, sending nothing" testAcceptanceUnread
    it "clears the reveal of an acceptance signed for another policy or for the other kind of terms, sending nothing, for the page to sign again" testAcceptanceOtherPolicy
    it "judges a relay past the acceptance's deadline as a register past it, retrying it while the commitment is live" testAcceptanceExpired
    it "relays no acceptance once the commitment lapsed, so the policy commits nothing again" testAcceptanceLapsed
  describe "lapse" $ do
    it "keeps a committed order not revealed by revealBefore waiting for /retry, keeping its code, and ends one whose links were refused failed" testLapseFailed
  describe "restart" $ do
    it "resumes every phase after a restart, a commit saved but not recorded included" testRestart
    it "waits for a register saved but not recorded before a restart, sending no other" testRestartRegisterSaved
  describe "lanes" $ do
    it "starts a worker for each registration in its phases" testRunWorkers
    it "reports once each paid or code order not registered 3 hours after its payment" testUnregisteredAlert
    it "fails each order paid by invoice or balance, queued, committed or failed, a calendar month after its payment, its price kept as its balance and its name released, once" testMonthEnds
    it "ends the month on the last day of a shorter month" testMonthClipped
    it "releases the codes of code orders failed or lapsed unsigned for 30 days, closing them taken, and no others" testCodeSweep
  describe "confirmation depth" $
    it "judges a register landed by ownerOf only once the owner holds the name at the confirmation depth" testLandedAtDepth
  describe "retry" $ do
    it "refuses /retry of any order but a lapsed unsigned one with bad_request, a failed or signed one included, changing nothing, and answers an unknown order 404" testRetryNotFailed
    it "refuses /retry of a lapsed order whose links were refused with bad_request, changing nothing" testRetryLinksRefused
    it "requeues a lapsed committed order, whose view says it lapsed, answering an old version at once after it and keeping its code" testRetryLapsed
    it "runs in the name's turn, answering registration_changed for an order moved to another name meanwhile" testRetryInNameTurn
  describe "close" $ do
    it "closes a failed paid order taken, not live, its price credited and refunded, its reveal cleared, then refuses /retry, a move and a second refund, and accepts a new order for the name" testCloseFailed
    it "closes a lapsed unsigned committed order, naming its lapse as its end" testCloseLapsed
    it "closes a failed order paid from a balance, its chain naming the paying order" testCloseFromBalance
    it "registers a failed order whose owner holds the name, and one whose register row mined while the owner no longer holds it, refunding nothing" testCloseLanded
    it "refuses an unknown reference, an order not failed, a failed code order and a failed chain read, changing nothing" testCloseRefused
    it "refuses an order changed during its chain check as changed, writing nothing" testCloseLostRace
    it "refuses a balance another order holds, writing nothing, and refunds the whole balance once released" testCloseHeld
    it "rolls back the taken write and the credit when the refund is refused" testCloseRollback

-- fixtures

data W = W
  { wCtx :: Ctx,
    wChain :: ChainClient,
    wSender :: Sender,
    wWorker :: Worker
  }

-- | The worker over the orders' store and fake node, with the registration poll, Try again and another name as namesRoutes registers them.
withWorker :: (W -> IO a) -> IO a
withWorker = withWorkerAs id

-- | With the names settings changed, as the service reads them at startup.
withWorkerAs :: (NamesConfig -> NamesConfig) -> (W -> IO a) -> IO a
withWorkerAs settings action = withOrders $ \open -> do
  let ctx@Ctx {cNode, cOrders, cWeb} = configured settings open
  answerCall cNode fakeController minCommitmentAgeCall $ Returns $ uint256 minAge
  answerCall cNode fakeController maxCommitmentAgeCall $ Returns $ uint256 maxAge
  answerUnowned ctx "alicebakery"
  let web = cWeb {weOrders = Just cOrders}
      routes path = registrationRoute path <|> codeRoutes cOrders (weProviders web) (weWaiters web) path
      registrationRoute = \case
        ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler cOrders (weHttp web) holdMicros (RegistrationId rid)}
        ["api", "registration", rid, "retry"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = retryHandler cOrders (RegistrationId rid)}
        ["api", "registration", rid, "name"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = retargetHandler cOrders (RegistrationId rid)}
        _ -> Nothing
      app req respond = fromMaybe (webApp web req respond) (namesApp (weHttp web) routes req respond)
  withApp app $ \client -> started ctx {cWeb = web, cClient = client} >>= action

-- | A new sender and worker over the same store, as after a restart.
started :: Ctx -> IO W
started wCtx@Ctx {cNode, cStore, cRandom, cClock, cOrders} = do
  wChain <- newChainClient cRandom $ fakeChainConfig cNode
  wSender <- newSender cRandom wChain cStore (readIORef cClock)
  wWorker <- newWorker cOrders wSender
  pure W {wCtx, wChain, wSender, wWorker}

minAge, maxAge :: Word64
minAge = 60
maxAge = 86400

expiry :: Word64
expiry = orderSeconds + 2 * 31536000

at :: Word64 -> UTCTime
at = posixSecondsToUTCTime . fromIntegral

day :: NominalDiffTime
day = 86400

-- | Committed a minute before the order.
openPhase :: NameRegistration -> NameRegistration
openPhase r = r {nrPhase = OCommitted openCommit}

openCommit :: Commit
openCommit = Commit {cmTx = Just $ TxRef (TxHash $ hashFixture 8) 100, cmRevealAfter = orderSeconds - minAge, cmRevealBefore = orderSeconds - minAge + maxAge}

commitAt :: Word64 -> Maybe TxRef -> Commit
commitAt t tx = Commit {cmTx = tx, cmRevealAfter = t + minAge, cmRevealBefore = t + maxAge}

revealBeforeOf :: NameRegistration -> Maybe Word64
revealBeforeOf = fmap cmRevealBefore . phaseCommit . nrPhase

-- | The reveal the page sends, its intent signed with the owner's key.
signedReveal :: Ctx -> Secp256k1PrivateKey -> NameReveal -> NameRegistration -> IO NameReveal
signedReveal Ctx {cRandom, cOrders, cNode} key rv r@NameRegistration {nrCommitment, nrOwner} = do
  let RegisterIntent {riRegistration} = intentOf (oeConfig cOrders) r rv
  sig <- either (error . show) id <$> signRecoverable cRandom key (registerIntentDigest (sponsorDomain fakeChainId fakeSponsor) riRegistration (rvNonce rv) (rvDeadline rv))
  answerCall cNode fakeNamePolicy (selector "acceptedTerms(bytes32,address)" <> unHex32 nrCommitment <> addressWord nrOwner) $ Returns $ unHex32 $ taTermsHash $ rvAcceptance rv
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
  Retry v reason -> "Retry " <> show v <> " " <> show reason
  Stalled v reason -> "Stalled " <> show v <> " " <> show reason
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

stalled :: Next -> IO StepFailure
stalled = \case
  Stalled _ reason -> pure reason
  next -> fail $ "not stalled: " <> shape next

done :: Next -> IO ()
done = \case
  Done -> pure ()
  next -> fail $ "not done: " <> shape next

-- | The raw transactions the node received, in the order they were sent.
sent :: W -> IO [ByteString]
sent W {wCtx = Ctx {cNode}} = sentTransactions cNode

newestHash :: W -> IO TxHash
newestHash w = TxHash . Hex32 . keccak256 . last <$> sent w

-- | Mines the newest transaction sent, with this status, and the blocks after it that settle it; the receipt's block.
mineNewest :: W -> Bool -> IO Word64
mineNewest w@W {wCtx = Ctx {cNode}, wSender} ok = do
  TxHash (Hex32 h) <- newestHash w
  block <- mine cNode [(h, ok)]
  replicateM_ 2 $ mine cNode []
  followTransactions wSender
  pure block

blockTime :: W -> Word64 -> IO Word64
blockTime W {wChain} n = either (error . show) blockTimestamp <$> blockAt wChain n

commitmentsCall :: NameRegistration -> ByteString
commitmentsCall NameRegistration {nrCommitment} = selector "commitments(bytes32)" <> unHex32 nrCommitment

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
  (status, fieldOf view "version", written > nrVersion) `shouldBe` (200, Just (J.toJSON written), True)
  pure a

traverseFst :: (a -> IO c) -> (a, b) -> IO (c, b)
traverseFst f (a, b) = (,b) <$> f a

codeOf :: Ctx -> PrepaidCode -> IO PrepaidCode
codeOf Ctx {cStore} PrepaidCode {pcId} = withTransaction cStore (`getPrepaidCodeById` pcId)

nowOf :: Ctx -> IO UTCTime
nowOf Ctx {cClock} = readIORef cClock

-- tests

testCommitWaitRegister :: IO ()
testCommitWaitRegister = withWorker $ \w@W {wCtx = ctx} -> codeIn ctx 8 aliceOrder id >>= commitWaitRegister w

testCodesOnlyRegister :: IO ()
testCodesOnlyRegister = withWorkerAs codesOnly $ \w@W {wCtx = ctx@Ctx {cClient}} -> do
  (text, code) <- issueCode ctx 8 2 id
  (status, coverage) <- post ctx "/api/code/check" (J.encode $ J.object ["code" J..= text])
  (status, (`fieldOf` "state") =<< coverage) `shouldBe` (200, Just "unused")
  placeOrder ctx aliceOrder `shouldReturn` refusedWith 403 "codes_only"
  r <- registrationById ctx =<< registered ctx text aliceOrder
  commitWaitRegister w (r, code)
  (status', view) <- answerOf <$> webGet cClient (registrationPath $ nrId r)
  (status', phaseOf =<< view) `shouldBe` (200, Just "registered")
  stubCalls (cStub ctx) `shouldReturn` []

-- | A queued code order, committed, revealed and registered.
commitWaitRegister :: W -> (NameRegistration, PrepaidCode) -> IO ()
commitWaitRegister w@W {wCtx = ctx} (r, code) = do
  answerExpiry ctx "alicebakery" expiry
  answerCommitment w r 0
  void $ published w r $ steps w r >>= awaiting
  [commitRaw] <- sent w
  committing <- stored ctx r
  (nrPhase committing, nrVersion committing) `shouldBe` (OCommitting $ TxHash $ Hex32 $ keccak256 commitRaw, nrVersion r + 1)
  -- the commit in flight is waited for
  steps w r >>= awaiting >>= (`shouldBe` TxHash (Hex32 $ keccak256 commitRaw)) . ptHash
  length <$> sent w `shouldReturn` 1
  commitBlock <- mineNewest w True
  t <- blockTime w commitBlock
  answerCommitment w r t
  -- committed, waiting for the reveal until the lapse
  published w r (steps w r >>= waitingUntil) `shouldReturn` at (t + maxAge + 1)
  committed <- stored ctx r
  let commitHash = TxHash $ Hex32 $ keccak256 commitRaw
      onChain = commitAt t $ Just $ TxRef commitHash commitBlock
  nrPhase committed `shouldBe` OCommitted onChain
  -- the page reveals before revealAfter
  void $ revealed ctx keyA "alicebakery" r
  steps w r >>= waitingUntil >>= (`shouldBe` at (t + minAge))
  advance ctx $ fromIntegral (t + minAge) - fromIntegral orderSeconds
  void $ published w r $ steps w r >>= awaiting
  registerTx <- newestHash w
  registering <- stored ctx r
  nrPhase registering `shouldBe` ORegistering onChain registerTx
  void $ mineNewest w True
  now <- nowOf ctx
  published w r (steps w r) >>= done
  x <- stored ctx r
  (nrPhase x, nrHistory x, orderLive x) `shouldBe` (ORegistered expiry now, TxHistory {thCommitTx = Just commitHash, thRegisterTx = Just registerTx}, False)
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
  advance ctx 37
  followTransactions wSender
  replacement <- newestHash w
  replacement `shouldNotBe` first
  inFlight <- stored ctx r
  steps w r >>= awaiting >>= (`shouldBe` replacement) . ptHash
  x <- stored ctx r
  (nrPhase x, nrVersion x) `shouldBe` (OCommitting replacement, nrVersion inFlight + 1)
  length <$> sent w `shouldReturn` 2

testUnexpiredCommitment :: IO ()
testUnexpiredCommitment = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  -- an earlier commit of the commitment mined between the read and the dry run
  r <- paidIn ctx aliceOrder id
  let t = orderSeconds - 600
  answerCommitment w r t
  answerCallOnce cNode fakeController (commitmentsCall r) $ Returns $ uint256 (0 :: Int)
  dryRunsRevert w "UnexpiredCommitmentExists(bytes32)" (unHex32 $ nrCommitment r)
  steps w r >>= retrying
  sent w `shouldReturn` []
  stored ctx r `shouldReturn` r
  dryRunsPass w
  steps w r >>= waitingUntil >>= (`shouldBe` at (t + maxAge + 1))
  x <- stored ctx r
  nrPhase x `shouldBe` OCommitted (commitAt t Nothing)
  -- a commit sent, mined with status 0 after another commit of it
  r2 <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 2} id
  answerCommitment w r2 0
  void $ steps w r2 >>= awaiting
  void $ mineNewest w False
  let t2 = orderSeconds - 30
  answerCommitment w r2 t2
  steps w r2 >>= waitingUntil >>= (`shouldBe` at (t2 + maxAge + 1))
  x2 <- stored ctx r2
  nrPhase x2 `shouldBe` OCommitted (commitAt t2 Nothing)
  length <$> sent w `shouldReturn` 1

testRevealWindowPastRange :: IO ()
testRevealWindowPastRange = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  r <- paidIn ctx aliceOrder id
  answerCall cNode fakeController maxCommitmentAgeCall $ Returns $ uint256 (maxBound :: Word64)
  answerCommitment w r $ orderSeconds - 600
  steps w r >>= retrying
  stored ctx r `shouldReturn` r
  sent w `shouldReturn` []

testCommitOutOfGas :: IO ()
testCommitOutOfGas = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder id
  answerCommitment w r 0
  first <- ptHash <$> (steps w r >>= awaiting)
  void $ mineNewest w False
  second <- ptHash <$> (steps w r >>= awaiting)
  second `shouldNotBe` first
  [_, raw] <- sent w
  txNonce <$> either error pure (decodeSignedTx raw) `shouldReturn` 1
  x <- stored ctx r
  nrPhase x `shouldBe` OCommitting second

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
  dryRunsRevert w "UnexpiredCommitmentExists(bytes32)" (unHex32 $ nrCommitment r)
  steps w r >>= retrying
  sent w `shouldReturn` []
  stored ctx r `shouldReturn` r
  dryRunsPass w
  advance ctx 1
  void $ steps w r >>= awaiting
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PCommitting
  length <$> sent w `shouldReturn` 1

testIntentAcrossCommits :: IO ()
testIntentAcrossCommits = withWorker $ \w@W {wCtx = ctx} -> do
  -- requeued with its intent on CommitmentTooOld, its commitment lapsed
  r <- paidIn ctx aliceOrder $ \o -> o {nrPhase = OQueued, nrHistory = oldCommit, nrReveal = Just reveal}
  answerCommitment w r (orderSeconds - maxAge - 1)
  void $ steps w r >>= awaiting
  x <- stored ctx r
  commitTx <- newestHash w
  (nrPhase x, nrReveal x, nrVersion x) `shouldBe` (OCommitting commitTx, Just reveal, nrVersion r + 2)
  -- requeued with its intent while a third party's commit of the commitment is live: not ours, so only our commit goes
  r2 <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 2} $ \o -> o {nrPhase = OQueued, nrHistory = oldCommit, nrReveal = Just reveal}
  answerAccepted w r2 $ Returns $ unHex32 $ taTermsHash $ rvAcceptance reveal
  answerCommitment w r2 (orderSeconds - 30)
  steps w r2 >>= waitingUntil >>= (`shouldBe` at (orderSeconds - 30 + minAge))
  x2 <- stored ctx r2
  (nrPhase x2, nrReveal x2) `shouldBe` (OCommitted (commitAt (orderSeconds - 30) Nothing), Just reveal)
  length <$> sent w `shouldReturn` 1
  where
    oldCommit = noHistory {thCommitTx = Just $ TxHash $ hashFixture 8}

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
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PCommitting

sentCommits :: Ctx -> NameRegistration -> TxSettlement -> [Word8] -> IO ()
sentCommits Ctx {cStore} NameRegistration {nrId} settlement nonces = withTransaction cStore $ \db -> forM_ nonces $ \n -> do
  let h = TxHash $ hashFixture n
  void $ insertTransaction db (TxRegistration RTCommit nrId) NewTransaction {ntxRaw = "raw", ntxHash = h, ntxNonce = fromIntegral n} orderTime
  recordTransactionOutcome db h TSPending settlement `shouldReturn` True

testMinedCommitsUncounted :: IO ()
testMinedCommitsUncounted = withWorker $ \w@W {wCtx = ctx@Ctx {cOrders}} -> do
  r <- paidIn ctx aliceOrder id
  sentCommits ctx r (TSMinedAt 120) [1 .. fromIntegral maxSendsPerKind]
  sendsUsedUp cOrders r `shouldReturn` True
  answerCommitment w r 0
  void $ steps w r >>= awaiting
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PCommitting
  r2 <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 2} id
  sentCommits ctx r2 (TSRevertedAt 120) [11 .. 10 + fromIntegral maxSendsPerKind]
  answerCommitment w r2 0
  steps w r2 >>= done
  now <- nowOf ctx
  nrPhase <$> stored ctx r2 `shouldReturn` OFailed now FRSendsUsedUp
  length <$> sent w `shouldReturn` 1

testLostRace :: IO ()
testLostRace = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}, wWorker} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  answerAvailable w False
  dryRunsRevert w "NameNotAvailable(string)" $ abiTuple [Dynamic $ abiBytes "alicebakery"]
  (stepping, gate) <- pausedAt cNode "eth_call" $ stepRegistration wWorker (nrId r)
  changed <- rewrite ctx r $ \o -> o {nrPhase = OCommitted openCommit {cmRevealAfter = cmRevealAfter openCommit + 1}}
  releaseGate gate
  timeout prompt (wait stepping) >>= maybe (fail "the step did not finish") (pure . shape) >>= (`shouldBe` "Again")
  stored ctx r `shouldReturn` changed
  -- held at the commitment's read, a fresh worker's first chain read
  r2 <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 2} id
  answerCommitment w r2 $ orderSeconds - 60
  W {wWorker = fresh} <- started ctx
  (stepping', gate') <- pausedAt cNode "eth_call" $ stepRegistration fresh (nrId r2)
  changed' <- rewrite ctx r2 $ \o -> o {nrMoves = nrMoves o + 1}
  releaseGate gate'
  timeout prompt (wait stepping') >>= maybe (fail "the step did not finish") (pure . shape) >>= (`shouldBe` "Again")
  stored ctx r2 `shouldReturn` changed'

testInNameTurn :: IO ()
testInNameTurn = withWorker $ \w@W {wCtx = ctx@Ctx {cOrders}} -> do
  r <- paidIn ctx aliceOrder linksRefusedPhase
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
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PFailed

-- | The order of alicebakery, by owner A, with this commitment.
aliceFields :: Int -> OrderFields
aliceFields n = OrderFields {ofLabelHash = labelHash "alicebakery", ofLength = lengthOf 11, ofDuration = yearsDuration 2 orderTime, ofCommitment = commitmentOf n, ofSealed = "sealed-alicebakery-" <> tshow n, ofAgreedTerms = agreed plainTerms}

aliceRegistration :: Text -> Int -> NewRegistration
aliceRegistration rid n = NewRegistration {nrgId = RegistrationId rid, nrgOwner = ownerA, nrgOrder = aliceFields n, nrgYears = 2, nrgOrderHash = orderHash ownerA (aliceFields n) (OTInvoice btc $ oPrice aliceOrder), nrgReplaces = Nothing}

-- | The amount and credited of the order's balance.
balanceFigures :: Ctx -> RegistrationId -> IO (Maybe (USDCents, USDCents))
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
  answerFree ctx "carolbakery"
  partly <- paidIn ctx (byB aliceOrder) {oLabel = "carolbakery", oCommitment = commitmentOf 5} failedPhase
  withTransaction cStore $ \db -> creditBalance db (nrId wholly) 30000 orderTime >> creditBalance db (nrId partly) 4000 orderTime
  withTransaction cStore (\db -> createBalanceRegistration db (aliceRegistration "covered" 1) BalanceUse {buSource = nrId wholly, buUsed = price} 30000 orderTime) `shouldReturn` Right ()
  covered <- registrationById ctx (RegistrationId "covered") >>= \r -> rewrite ctx r openPhase >>= revealed ctx keyA "alicebakery"
  endsTaken w covered
  (\x -> (namePhase $ nrPhase x, nrPayment x)) <$> stored ctx covered `shouldReturn` (PTaken, OPBalance BalanceUse {buSource = nrId wholly, buUsed = price})
  balanceFigures ctx (nrId covered) `shouldReturn` Just (price, price)
  balanceFigures ctx (nrId wholly) `shouldReturn` Just (30000 - price, 30000)
  withTransaction cStore $ \db -> do
    insertInvoiceRow db "inv-diff" (addUTCTime 3600 orderTime) "open"
    createPaidRegistration db (aliceRegistration "diff" 2) (Just BalanceUse {buSource = nrId partly, buUsed = 4000}) (paidOrder "inv-diff") {poPrice = price} orderTime `shouldReturn` Right ()
  settleNow ctx (InvoiceId "inv-diff") settled `shouldReturn` ISPaid
  balanceFigures ctx (nrId partly) `shouldReturn` Just (0, 4000)
  diff <- registrationById ctx (RegistrationId "diff") >>= \r -> rewrite ctx r openPhase >>= revealed ctx keyA "alicebakery"
  endsTaken w diff
  namePhase . nrPhase <$> stored ctx diff `shouldReturn` PTaken
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
  (status, phaseOf view) `shouldBe` (200, Just "taken")
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
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PRegistering

testRevertTaken :: IO ()
testRevertTaken = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  (r, code) <- codeIn ctx 8 aliceOrder openPhase >>= traverseFst (revealed ctx keyA "alicebakery")
  answerAvailable w False
  -- a reason kept in the message without data: not one of the contracts' errors
  setFault cNode "eth_estimateGas" $ RpcError 3 "execution reverted: ERC721: invalid token ID" Nothing
  published w r (steps w r) >>= done
  x <- stored ctx r
  namePhase (nrPhase x) `shouldBe` PTaken
  PrepaidCode {pcReservedBy, pcSpentAt} <- codeOf ctx code
  (pcReservedBy, pcSpentAt) `shouldBe` (Nothing, Nothing)
  balanceFigures ctx (nrId r) `shouldReturn` Nothing
  sent w `shouldReturn` []

testTakenAtDepth :: IO ()
testTakenAtDepth = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  answerAvailable w True
  dryRunsRevert w "NameNotAvailable(string)" (abiTuple [Dynamic $ abiBytes "alicebakery"])
  -- a rival's register, mined in the latest block
  latest <- mine cNode []
  answerCallFrom cNode latest fakeController availableCall $ Returns $ uint256 (0 :: Int)
  steps w r >>= retrying
  (nrPhase <$> stored ctx r) `shouldReturn` nrPhase r
  -- confirmations 3: two more blocks put it at the depth
  replicateM_ 2 $ mine cNode []
  steps w r >>= done
  (namePhase . nrPhase <$> stored ctx r) `shouldReturn` PTaken
  sent w `shouldReturn` []

testRevertFailed :: IO ()
testRevertFailed = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  (r, code) <- codeIn ctx 8 aliceOrder openPhase >>= traverseFst (revealed ctx keyA "alicebakery")
  p <- paidIn ctx dynamisOrder openPhase >>= revealed ctx keyA "dynamis"
  answerAvailable w True
  dryRunsRevert w "SignatureExpired()" ""
  steps w r >>= stalled >>= (`shouldBe` SFRegisterRefused (Just $ RRNamed RNSignatureExpired))
  stored ctx r `shouldReturn` r
  advance ctx $ fromIntegral (cmRevealBefore openCommit + 1 - orderSeconds)
  steps w r >>= done
  now <- nowOf ctx
  x <- stored ctx r
  (nrPhase x, nrHistory x, orderLive x) `shouldBe` (OFailed now FRRegistryRefused, nrHistory r, True)
  pcReservedBy <$> codeOf ctx code `shouldReturn` Just (nrId r)
  -- a revert without data while the name is available, past the lapse, fails a paid order too
  answerCall cNode fakeController (selector "available(string)" <> abiTuple [Dynamic $ abiBytes "dynamis"]) $ Returns $ uint256 (1 :: Int)
  answerUnowned ctx "dynamis"
  setFault cNode "eth_estimateGas" $ RpcError (-32000) "execution reverted" Nothing
  steps w p >>= done
  namePhase . nrPhase <$> stored ctx p `shouldReturn` PFailed
  sent w `shouldReturn` []

testAllowanceUsedUp :: IO ()
testAllowanceUsedUp = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  answerAvailable w True
  -- viem's encodeErrorResult of InsufficientAllowance(10000, 2500)
  setFault cNode "eth_estimateGas" $ revertFault $ unhex "2a1b2dd8000000000000000000000000000000000000000000000000000000000000271000000000000000000000000000000000000000000000000000000000000009c4"
  steps w r >>= stalled >>= (`shouldBe` SFAllowanceUsedUp)
  x <- stored ctx r
  (nrPhase x, nrVersion x) `shouldBe` (nrPhase r, nrVersion r)
  sent w `shouldReturn` []
  dryRunsPass w
  void $ steps w r >>= awaiting
  length <$> sent w `shouldReturn` 1

testAllowanceAcrossLapse :: IO ()
testAllowanceAcrossLapse = withWorker $ \w@W {wCtx = ctx@Ctx {cNode, cOrders}} -> do
  let lapse = orderSeconds + 10
  r <- paidIn ctx aliceOrder (\o -> o {nrPhase = OCommitted openCommit {cmRevealBefore = lapse}}) >>= revealed ctx keyA "alicebakery"
  rv <- maybe (fail "no reveal") pure $ nrReveal r
  let registerCall = registerWithCreditCalldata $ intentOf (oeConfig cOrders) r rv
  answerAvailable w True
  answerCommitment w r (lapse - maxAge)
  revertDryRun cNode registerCall $ Just $ selector "InsufficientAllowance(uint256,uint256)"
  void $ steps w r >>= stalled
  advance ctx 11
  void $ steps w r >>= stalled
  stored ctx r `shouldReturn` r
  revertDryRun cNode registerCall $ Just $ selector "CommitmentTooOld(bytes32,uint256,uint256)"
  void $ steps w r >>= awaiting
  committing <- stored ctx r
  (namePhase $ nrPhase committing, nrReveal committing) `shouldBe` (PCommitting, Just rv)
  t <- mineNewest w True >>= blockTime w
  answerCommitment w r t
  revertDryRun cNode registerCall Nothing
  steps w r >>= waitingUntil >>= (`shouldBe` at (t + minAge))
  advance ctx $ fromIntegral (t + minAge - lapse - 1)
  void $ steps w r >>= awaiting
  x <- stored ctx r
  (namePhase $ nrPhase x, nrReveal x, revealBeforeOf x) `shouldBe` (PRegistering, Just rv, Just (t + maxAge))
  length <$> sent w `shouldReturn` 2

testSendsCapped :: IO ()
testSendsCapped = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  answerAvailable w True
  answerUnowned ctx "alicebakery"
  mapM_ (\n -> sentRegister ctx r n $ TSRevertedAt 120) [1 .. fromIntegral maxSendsPerKind]
  steps w r >>= done
  now <- nowOf ctx
  x <- stored ctx r
  nrPhase x `shouldBe` OFailed now FRSendsUsedUp
  sent w `shouldReturn` []
  fst <$> retryOf ctx x `shouldReturn` 400
  answerFree ctx "alicebakers"
  to <- moveTo ctx "alicebakers" 10
  fst <$> move ctx keyA x to `shouldReturn` 400
  stored ctx x `shouldReturn` x
  answerOwner ctx "alicebakery" (nrOwner x)
  answerExpiry ctx "alicebakery" expiry
  fst <$> move ctx keyA x to `shouldReturn` 409
  namePhase . nrPhase <$> stored ctx x `shouldReturn` PRegistered

testInvalidNonce :: IO ()
testInvalidNonce = forM_ [("InvalidNonce()", fakeNamePolicy), ("TermsNotAccepted(string)", newPolicy)] $ \(revert, policyAfter) -> withWorker $ \w@W {wCtx = ctx@Ctx {cNode, cOrders = OrdersEnv {oeContracts}}} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  namePolicy oeContracts `shouldReturn` Right fakeNamePolicy
  answerCall cNode fakeController (selector "namePolicy()") $ Returns $ addressWord newPolicy
  dryRunsRevert w revert ""
  published w r (steps w r >>= waitingUntil) `shouldReturn` at (orderSeconds - minAge + maxAge + 1)
  x <- stored ctx r
  (nrPhase x, nrReveal x, nrVersion x) `shouldBe` (nrPhase r, Nothing, nrVersion r + 1)
  sent w `shouldReturn` []
  namePolicy oeContracts `shouldReturn` Right policyAfter

-- | The page's reveal with its acceptance of the policy's general terms.
acceptedReveal :: Ctx -> NameRegistration -> IO NameRegistration
acceptedReveal = acceptedRevealFor fakeNamePolicy False fakeGeneralTerms

-- | The page's reveal with its acceptance of these terms signed for the given policy, as community terms or general terms.
acceptedRevealFor :: Address -> Bool -> ByteString -> Ctx -> NameRegistration -> IO NameRegistration
acceptedRevealFor policy community terms ctx@Ctx {cRandom} r = do
  rv <- signedReveal ctx keyA reveal {rvLabel = "alicebakery"} r
  sig <- either (error . show) id <$> signRecoverable cRandom keyA (acceptanceDigest (policyDomain fakeChainId policy) (termsKind community) (nrCommitment r) (Hex32 terms) 0 (rvDeadline rv))
  rewrite ctx r (\o -> o {nrReveal = Just rv {rvAcceptance = TermsAcceptance {taTermsHash = Hex32 terms, taNonce = 0, taSignature = sig}}})

-- | The terms hash the policy holds as accepted for the registration's commitment by its owner.
answerAccepted :: W -> NameRegistration -> CallAnswer -> IO ()
answerAccepted W {wCtx = Ctx {cNode}} NameRegistration {nrCommitment, nrOwner} =
  answerCall cNode fakeNamePolicy (selector "acceptedTerms(bytes32,address)" <> unHex32 nrCommitment <> addressWord nrOwner)

testAcceptanceRelayed :: IO ()
testAcceptanceRelayed = forM_ [(False, fakeGeneralTerms, plainTerms), (True, fakeCommunityTerms, communityTerms)] $ \(community, terms, signed) -> withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  when community $ listCommunityName cNode $ labelHash "alicebakery"
  r@NameRegistration {nrReveal = Just NameReveal {rvDeadline, rvAcceptance = acceptance}} <- paidIn ctx aliceOrder {oTerms = signed} openPhase >>= acceptedRevealFor fakeNamePolicy community terms ctx
  relay <- steps w r >>= awaiting
  [acceptRaw] <- sent w
  ((txTo &&& txData) <$> decodeSignedTx acceptRaw) `shouldBe` Right (fakeNamePolicy, acceptCalldata (termsKind community) (nrCommitment r) (nrOwner r) rvDeadline acceptance)
  -- in flight, it is waited for, and nothing else is sent
  steps w r >>= awaiting >>= (`shouldBe` ptHash relay) . ptHash
  stored ctx r `shouldReturn` r
  void $ mineNewest w True
  answerAccepted w r $ Returns terms
  void $ published w r $ steps w r >>= awaiting
  [_, registerRaw] <- sent w
  (txTo <$> decodeSignedTx registerRaw) `shouldBe` Right fakeSponsor
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PRegistering

testAcceptanceBeforeWait :: IO ()
testAcceptanceBeforeWait = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder (\o -> o {nrPhase = OCommitted openCommit {cmRevealAfter = orderSeconds + 60}}) >>= acceptedReveal ctx
  void $ steps w r >>= awaiting
  [acceptRaw] <- sent w
  (txTo <$> decodeSignedTx acceptRaw) `shouldBe` Right fakeNamePolicy
  void $ mineNewest w True
  answerAccepted w r $ Returns fakeGeneralTerms
  steps w r >>= waitingUntil >>= (`shouldBe` at (orderSeconds + 60))
  length <$> sent w `shouldReturn` 1

testAcceptanceRefused :: IO ()
testAcceptanceRefused = forM_ ["TermsMismatch(bytes32,bytes32)", "InvalidNonce()"] $ \revert -> withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  dryRunsRevert w revert ""
  published w r (steps w r >>= waitingUntil) `shouldReturn` at (orderSeconds - minAge + maxAge + 1)
  x <- stored ctx r
  (nrPhase x, nrReveal x) `shouldBe` (nrPhase r, Nothing)
  sent w `shouldReturn` []

testAcceptanceSentFirst :: IO ()
testAcceptanceSentFirst = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}} -> do
  r@NameRegistration {nrReveal = Just NameReveal {rvDeadline, rvAcceptance = acceptance}} <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  answerAccepted w r $ Returns fakeGeneralTerms
  answerCallOnce cNode fakeNamePolicy (selector "acceptedTerms(bytes32,address)" <> unHex32 (nrCommitment r) <> addressWord (nrOwner r)) $ Returns $ uint256 (0 :: Int)
  revertDryRun cNode (acceptCalldata (termsKind False) (nrCommitment r) (nrOwner r) rvDeadline acceptance) $ Just $ selector "InvalidNonce()"
  void $ steps w r >>= awaiting
  [registerRaw] <- sent w
  (txTo <$> decodeSignedTx registerRaw) `shouldBe` Right fakeSponsor
  x <- stored ctx r
  (namePhase $ nrPhase x, nrReveal x) `shouldBe` (PRegistering, nrReveal r)

testAcceptanceMinedFailed :: IO ()
testAcceptanceMinedFailed = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  void $ steps w r >>= awaiting
  void $ mineNewest w False
  answerAccepted w r $ Returns fakeGeneralTerms
  void $ published w r $ steps w r >>= awaiting
  [_, registerRaw] <- sent w
  (txTo <$> decodeSignedTx registerRaw) `shouldBe` Right fakeSponsor
  x <- stored ctx r
  (namePhase $ nrPhase x, nrReveal x) `shouldBe` (PRegistering, nrReveal r)

testAcceptanceUnread :: IO ()
testAcceptanceUnread = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  answerAccepted w r $ Reverts ""
  steps w r >>= retrying
  stored ctx r `shouldReturn` r
  sent w `shouldReturn` []

testAcceptanceOtherPolicy :: IO ()
testAcceptanceOtherPolicy = forM_ [acceptedRevealFor fakeSponsor False fakeGeneralTerms, acceptedRevealFor fakeNamePolicy True fakeGeneralTerms] $ \accepted -> withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= accepted ctx
  published w r (steps w r >>= waitingUntil) `shouldReturn` at (orderSeconds - minAge + maxAge + 1)
  x <- stored ctx r
  (nrPhase x, nrReveal x) `shouldBe` (nrPhase r, Nothing)
  sent w `shouldReturn` []

testAcceptanceExpired :: IO ()
testAcceptanceExpired = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  answerAvailable w True
  dryRunsRevert w "SignatureExpired()" ""
  steps w r >>= stalled >>= (`shouldBe` SFRegisterRefused (Just $ RRNamed RNSignatureExpired))
  stored ctx r `shouldReturn` r
  sent w `shouldReturn` []

testAcceptanceLapsed :: IO ()
testAcceptanceLapsed = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder (\o -> o {nrPhase = OCommitted openCommit {cmRevealBefore = orderSeconds - minAge}}) >>= acceptedReveal ctx
  void $ steps w r
  txs <- sent w
  (txTo <$>) . decodeSignedTx <$> txs `shouldNotContain` [Right fakeNamePolicy]

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
  dryRunsRevert w "CommitmentTooNew(bytes32,uint256,uint256)" $ unHex32 (nrCommitment r) <> uint256 (orderSeconds - minAge) <> uint256 orderSeconds
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
  namePhase (nrPhase x) `shouldBe` PTaken
  pcReservedBy <$> codeOf ctx code `shouldReturn` Nothing

testExpiredIntent :: IO ()
testExpiredIntent = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder (\o -> o {nrPhase = OCommitted openCommit {cmRevealBefore = rvDeadline reveal}}) >>= revealed ctx keyA "alicebakery"
  answerAvailable w True
  advance ctx $ fromIntegral (rvDeadline reveal - orderSeconds) + 1
  logs <- capturingStderr $ void $ published w r (steps w r) >>= waitingUntil
  let RegistrationId rid = nrId r
  logs `shouldSatisfy` T.isInfixOf ("registration " <> rid <> ": its stored signature's deadline passed, so its page signs again")
  x <- stored ctx r
  (nrPhase x, nrReveal x) `shouldBe` (nrPhase r, Nothing)
  sent w `shouldReturn` []

testSignatureUnderOtherConfig :: IO ()
testSignatureUnderOtherConfig = withWorker $ \W {wCtx = ctx@Ctx {cOrders}, wSender} -> do
  reports <- newTVarIO []
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  let NamesConfig {nChain} = oeConfig cOrders
      moved = cOrders {oeConfig = (oeConfig cOrders) {nChain = nChain {cChainId = cChainId nChain + 1}}, oeReport = \t -> atomically $ modifyTVar' reports (t :)}
  worker <- newWorker moved wSender
  logs <- capturingStderr $ void $ stepRegistration worker (nrId r)
  now <- nowOf ctx
  nrPhase <$> stored ctx r `shouldReturn` OFailed now FRSignatureRefused
  let RegistrationId rid = nrId r
      failed = T.isInfixOf ("registration " <> rid <> " failed: its stored signature is refused under the service's current configuration")
  logs `shouldSatisfy` failed
  map failed <$> readTVarIO reports `shouldReturn` [True]
  sentTransactions (cNode ctx) `shouldReturn` []

testFailedOnlySettled :: IO ()
testFailedOnlySettled = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}, wSender} -> do
  r <- paidIn ctx aliceOrder openPhase >>= \o -> signedReveal ctx keyA reveal {rvDeadline = 9007199254740991} o >>= \rv -> rewrite ctx o (\x -> x {nrReveal = Just rv})
  r2 <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 2} (\o -> o {nrPhase = OCommitted openCommit {cmRevealBefore = orderSeconds + 3 * 86400}}) >>= revealed ctx keyB "dynamis"
  answerAvailable w True
  p <- steps w r >>= awaiting
  -- past revealBefore, the register in flight can still be mined
  advance ctx $ 2 * day
  steps w r >>= awaiting >>= (`shouldBe` ptHash p) . ptHash
  nrPhase <$> stored ctx r `shouldReturn` ORegistering openCommit (ptHash p)
  void $ mineNewest w False
  again <- ptHash <$> (steps w r >>= awaiting)
  again `shouldNotBe` ptHash p
  x <- stored ctx r
  (nrPhase x, nrReveal x) `shouldBe` (ORegistering openCommit again, nrReveal r)
  void $ mineNewest w True
  -- a register whose nonce another transaction took, while the owner holds the name: it went through
  p2 <- steps w r2 >>= awaiting
  useNextNonce cNode
  followTransactions wSender
  steps w r2 >>= awaiting >>= (`shouldBe` ptHash p2) . ptHash
  replicateM_ 2 $ mine cNode []
  followTransactions wSender
  answerOwner ctx "dynamis" (nrOwner r2)
  answerExpiry ctx "dynamis" expiry
  steps w r2 >>= done
  -- judged from chain state with no hash known to have mined: no register transaction (contract section 6)
  now <- nowOf ctx
  x2 <- stored ctx r2
  (nrPhase x2, thRegisterTx $ nrHistory x2) `shouldBe` (ORegistered expiry now, Nothing)

testLapseFailed :: IO ()
testLapseFailed = withWorker $ \w@W {wCtx = ctx} -> do
  (r, code) <- codeIn ctx 8 aliceOrder committedPhase
  refused <- paidIn ctx dynamisOrder linksRefusedPhase
  steps w r >>= waitingUntil >>= (`shouldBe` at (orderSeconds + maxAge + 1))
  advance ctx $ day + 1
  now <- nowOf ctx
  steps w r >>= waitingUntil >>= (`shouldBe` addUTCTime 60 now)
  stored ctx r `shouldReturn` r
  pcReservedBy <$> codeOf ctx code `shouldReturn` Just (nrId r)
  published w refused (steps w refused) >>= done
  x <- stored ctx refused
  (nrPhase x, nrHistory x, orderLive x) `shouldBe` (OFailed now FRLinksRefused, nrHistory refused, True)
  sent w `shouldReturn` []

testRestart :: IO ()
testRestart = withWorker $ \w@W {wCtx = ctx@Ctx {cStore}, wSender} -> do
  (r, code) <- codeIn ctx 8 aliceOrder id
  answerExpiry ctx "alicebakery" expiry
  answerCommitment w r 0
  -- the commit saved and sent, the service stopped before recording committing
  Right p <- submitTransaction wSender (TxRegistration RTCommit $ nrId r) fakeController (commitCalldata $ nrCommitment r)
  w1 <- started ctx
  steps w1 r >>= awaiting >>= (`shouldBe` ptHash p) . ptHash
  x <- stored ctx r
  nrPhase x `shouldBe` OCommitting (ptHash p)
  length <$> sent w `shouldReturn` 1
  commitBlock <- mineNewest w1 True
  t <- blockTime w1 commitBlock
  answerCommitment w r t
  w2 <- started ctx
  void $ steps w2 r >>= waitingUntil
  nrPhase <$> stored ctx r `shouldReturn` OCommitted (commitAt t $ Just $ TxRef (ptHash p) commitBlock)
  void $ revealed ctx keyA "alicebakery" r
  advance ctx $ fromIntegral (t + minAge) - fromIntegral orderSeconds
  register <- steps w2 r >>= awaiting
  w3 <- started ctx
  steps w3 r >>= awaiting >>= (`shouldBe` ptHash register) . ptHash
  nrPhase <$> stored ctx r `shouldReturn` ORegistering (commitAt t $ Just $ TxRef (ptHash p) commitBlock) (ptHash register)
  void $ mineNewest w3 True
  w4 <- started ctx
  steps w4 r >>= done
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PRegistered
  isJust . pcSpentAt <$> codeOf ctx code `shouldReturn` True
  map (mined . ntState) <$> withTransaction cStore (`registrationTransactions` nrId r) `shouldReturn` [True, True]
  length <$> sent w `shouldReturn` 2
  where
    mined = \case
      TxSettled TSMinedAt {} -> True
      _ -> False

testRestartRegisterSaved :: IO ()
testRestartRegisterSaved = withWorker $ \w@W {wCtx = ctx@Ctx {cOrders}, wSender} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  rv <- maybe (fail "no reveal") pure $ nrReveal r
  -- the register saved and sent, the service stopped before recording registering
  Right p <- submitTransaction wSender (TxRegistration RTRegister $ nrId r) fakeSponsor (registerWithCreditCalldata $ intentOf (oeConfig cOrders) r rv)
  w1 <- started ctx
  steps w1 r >>= awaiting >>= (`shouldBe` ptHash p) . ptHash
  x <- stored ctx r
  nrPhase x `shouldBe` ORegistering openCommit (ptHash p)
  steps w1 r >>= awaiting >>= (`shouldBe` ptHash p) . ptHash
  length <$> sent w `shouldReturn` 1

testRunWorkers :: IO ()
testRunWorkers = withWorker $ \w@W {wCtx = ctx@Ctx {cNode, cOrders}, wWorker} -> do
  (unpaid, _) <- placed ctx (byB aliceOrder) {oCommitment = commitmentOf 2}
  r <- paidIn ctx aliceOrder id
  answerCommitment w r 0
  withAsync (runWorkers wWorker) $ \_ -> do
    timeout prompt (awaitSent cNode 1) >>= maybe (fail "no commit was sent") (const $ pure ())
    changed <- awaitChange (oePhases cOrders) (nrId r) (nrVersion <$> stored ctx r) (nrVersion r) prompt
    changed `shouldBe` True
    namePhase . nrPhase <$> stored ctx r `shouldReturn` PCommitting
  nrPhase <$> invoiceRegistration ctx unpaid `shouldReturn` OUnpaid unpaid
  length <$> sent w `shouldReturn` 1

testUnregisteredAlert :: IO ()
testUnregisteredAlert = withWorker $ \W {wCtx = ctx@Ctx {cOrders}, wSender} -> do
  reports <- newTVarIO []
  worker <- newWorker cOrders {oeReport = \t -> atomically $ modifyTVar' reports (t :)} wSender
  paid <- paidIn ctx aliceOrder id
  (code, _) <- codeIn ctx 7 dynamisOrder id
  answerFree ctx "alicebakers"
  void $ paidIn ctx aliceOrder {oLabel = "alicebakers", oCommitment = commitmentOf 5} $ \o -> o {nrPhase = ORegistered expiry orderTime}
  advance ctx $ 3 * 3600
  alertUnregisteredPaid worker
  readTVarIO reports `shouldReturn` []
  advance ctx 1
  alertUnregisteredPaid worker
  alertUnregisteredPaid worker
  let named NameRegistration {nrId = RegistrationId rid} = any (T.isInfixOf rid)
  sent' <- readTVarIO reports
  (length sent', named paid sent', named code sent') `shouldBe` (2, True, True)

testMonthEnds :: IO ()
testMonthEnds = withWorker $ \W {wCtx = ctx@Ctx {cStore}, wWorker} -> do
  queued <- paidIn ctx aliceOrder id
  failed <- paidIn ctx dynamisOrder failedPhase
  answerFree ctx "alicebakers"
  committing <- paidIn ctx aliceOrder {oLabel = "alicebakers", oCommitment = commitmentOf 5} $ \o -> o {nrPhase = OCommitting $ TxHash $ hashFixture 8}
  answerFree ctx "carolbakery"
  (code, _) <- codeIn ctx 8 (byB aliceOrder) {oLabel = "carolbakery", oCommitment = commitmentOf 6} id
  let ended = [queued, failed]
      untouched = [committing, code]
      holders = withTransaction cStore $ \db -> mapM (labelHolder db . nrLabelHash) ended
  advance ctx $ diffUTCTime (addMonths 1 orderTime) orderTime - 1
  endMonths wWorker
  mapM (stored ctx) (ended <> untouched) `shouldReturn` ended <> untouched
  holders `shouldReturn` map (Just . nrId) ended
  advance ctx 1
  now <- nowOf ctx
  endMonths wWorker
  endMonths wWorker
  map nrPhase <$> mapM (stored ctx) ended `shouldReturn` replicate 2 (OFailed now FRMonthEnded)
  mapM (stored ctx) untouched `shouldReturn` untouched
  mapM (balanceFigures ctx . nrId) ended `shouldReturn` map (\r -> Just (nrPrice r, nrPrice r)) ended
  holders `shouldReturn` [Nothing, Nothing]
  map orderLive <$> mapM (stored ctx) ended `shouldReturn` [False, False]
  closeOf ctx failed `shouldReturn` CLRefused CFMonthEnded

testMonthClipped :: IO ()
testMonthClipped = withWorker $ \W {wCtx = ctx@Ctx {cStore, cClock}, wWorker} -> do
  r <- paidIn ctx aliceOrder id
  let paidAt = posixSecondsToUTCTime 1801396800
      lastDay = posixSecondsToUTCTime 1803816000
  withTransaction cStore $ \db -> DB.execute db "UPDATE sx_badge_service_name_registrations SET paid_at = ? WHERE registration_id = ?" (paidAt, (\(RegistrationId t) -> t) $ nrId r)
  writeIORef cClock $ addUTCTime (-1) lastDay
  endMonths wWorker
  stored ctx r `shouldReturn` r
  writeIORef cClock lastDay
  endMonths wWorker
  nrPhase <$> stored ctx r `shouldReturn` OFailed lastDay FRMonthEnded

testCodeSweep :: IO ()
testCodeSweep = withWorker $ \W {wCtx = ctx, wWorker} -> do
  (old, oldCode) <- codeIn ctx 8 aliceOrder failedPhase
  (recent, recentCode) <- codeIn ctx 7 dynamisOrder $ \x -> x {nrPhase = OFailed (addUTCTime (2 * day) orderTime) FRRegistryRefused}
  -- lapsed long ago, then queued again by /retry
  mapM_ (answerFree ctx) ["carolbakery", "dynamit"]
  (requeued, requeuedCode) <- codeIn ctx 8 (byB aliceOrder) {oLabel = "carolbakery", oCommitment = commitmentOf 2} $ \x -> x {nrPhase = OQueued}
  (lapsed, lapsedCode) <- codeIn ctx 7 (byB dynamisOrder) {oLabel = "dynamit", oCommitment = commitmentOf 4} committedPhase
  answerFree ctx "alicebakers"
  (signed, signedCode) <- codeIn ctx 8 aliceOrder {oLabel = "alicebakers", oCommitment = commitmentOf 5} $ \x -> (committedPhase x) {nrReveal = Just reveal}
  advance ctx $ 32 * day
  sweepFailedCodes wWorker
  map pcReservedBy <$> mapM (codeOf ctx) [oldCode, recentCode, requeuedCode, lapsedCode, signedCode]
    `shouldReturn` [Nothing, Just (nrId recent), Just (nrId requeued), Nothing, Just (nrId signed)]
  map nrPhase <$> mapM (stored ctx) [old, lapsed] `shouldReturn` [OTaken, OTaken]
  mapM (stored ctx) [recent, requeued, signed] `shouldReturn` [recent, requeued, signed]

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
  now <- nowOf ctx
  x <- stored ctx r
  (nrPhase x, nrHistory x) `shouldBe` (ORegistered expiry now, noHistory)

-- retry

retryOf :: Ctx -> NameRegistration -> IO (Int, Maybe J.Object)
retryOf ctx NameRegistration {nrId} = post ctx (registrationPath nrId <> "/retry") ""

-- | The view of an accepted /retry, failing the test otherwise.
retried :: Ctx -> NameRegistration -> IO J.Object
retried ctx r =
  retryOf ctx r >>= \case
    (200, Just v) -> pure v
    other -> fail $ "retry refused: " <> show other

viewOf :: Ctx -> NameRegistration -> IO J.Object
viewOf Ctx {cClient} NameRegistration {nrId} = webGet cClient (registrationPath nrId) >>= jsonObject

testRetryNotFailed :: IO ()
testRetryNotFailed = withWorker $ \W {wCtx = ctx} -> do
  queued <- paidIn ctx aliceOrder id
  registeredOrder <- paidIn ctx dynamisOrder $ \o -> o {nrPhase = ORegistered expiry orderTime}
  taken <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 4} $ \o -> o {nrPhase = OTaken}
  live <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 2} $ \o -> o {nrPhase = OCommitted openCommit {cmRevealBefore = orderSeconds + 2 * maxAge}}
  answerFree ctx "alicebakers"
  failed <- paidIn ctx aliceOrder {oLabel = "alicebakers", oCommitment = commitmentOf 5} failedPhase
  answerFree ctx "carolbakery"
  signed <- paidIn ctx (byB aliceOrder) {oLabel = "carolbakery", oCommitment = commitmentOf 6} $ \o -> (committedPhase o) {nrReveal = Just reveal}
  advance ctx $ day + 1
  forM_ [queued, live, registeredOrder, taken, failed, signed] $ \r -> do
    (nrPhase r,) <$> retryOf ctx r `shouldReturn` (nrPhase r, nameRefusedWith 400 "bad_request")
    stored ctx r `shouldReturn` r
  retryOf ctx queued {nrId = RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g"} `shouldReturn` nameRefusedWith 404 "not_found"

testRetryLinksRefused :: IO ()
testRetryLinksRefused = withWorker $ \W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder linksRefusedPhase
  advance ctx $ day + 1
  retryOf ctx r `shouldReturn` nameRefusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r

testRetryLapsed :: IO ()
testRetryLapsed = withWorker $ \w@W {wCtx = ctx@Ctx {cClient}} -> do
  (r, code) <- codeIn ctx 8 aliceOrder committedPhase
  advance ctx $ day + 1
  (`fieldOf` "phase") <$> viewOf ctx r
    `shouldReturn` Just (phaseValue "committed" ["revealAfter" J..= (orderSeconds + 60), "revealBefore" J..= (orderSeconds + maxAge), "signature" J..= J.object ["type" J..= ("lapsed" :: Text)]])
  -- the lapse alone changes no version, so a poll with the version seen before it is held
  let path = registrationPath (nrId r) <> "?seenVersion=" <> show (nrVersion r)
  poll <- held ctx (0, 1) path
  v <- retried ctx r
  (fieldOf v "phase", fieldOf v "version") `shouldBe` (Just $ phaseValue "queued" [], Just (J.toJSON $ nrVersion r + 1))
  (status, view) <- woken "the poll held at the lapse" poll
  (status, phaseOf view) `shouldBe` (200, Just "queued")
  -- prompt is far below the 30-second hold
  (status', view') <- timeout prompt (webGet cClient path >>= \resp -> (statusOf resp,) <$> jsonObject resp) >>= maybe (fail "the old version was held") pure
  (status', fieldOf view' "version") `shouldBe` (200, Just (J.toJSON $ nrVersion r + 1))
  pcReservedBy <$> codeOf ctx code `shouldReturn` Just (nrId r)
  -- the commitment lapsed, so the worker commits it again
  answerCommitment w r $ fromJust (revealBeforeOf r) - maxAge
  void $ steps w r >>= awaiting
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PCommitting

testRetryInNameTurn :: IO ()
testRetryInNameTurn = withWorker $ \W {wCtx = ctx@Ctx {cOrders}} -> do
  r <- paidIn ctx aliceOrder committedPhase
  moving <- paidIn ctx (byB dynamisOrder) {oCommitment = commitmentOf 2} committedPhase
  advance ctx $ day + 1
  let turns = oeTurns cOrders
      whileHeld :: NameRegistration -> IO a -> IO () -> IO a
      whileHeld NameRegistration {nrLabelHash} request meanwhile = do
        holding <- newEmptyMVar
        release <- newEmptyMVar
        withAsync (withLabelTurn turns nrLabelHash $ putMVar holding () >> takeMVar release) $ \_ -> do
          takeMVar holding
          withAsync request $ \requesting -> do
            timeout prompt (atomically $ labelTurnCount turns nrLabelHash >>= check . (== 2)) >>= maybe (fail "the retry did not queue on the turn") pure
            meanwhile
            putMVar release ()
            timeout prompt (wait requesting) >>= maybe (fail "the retry did not finish") pure
  (status, v) <- whileHeld r (retryOf ctx r) (stored ctx r `shouldReturn` r)
  (status, phaseOf =<< v) `shouldBe` (200, Just "queued")
  -- moved to another name while the retry waited for the old name's turn
  (status', v') <- whileHeld moving (retryOf ctx moving) (void $ rewrite ctx moving $ \o -> o {nrLabelHash = labelHash "alicebakers", nrLength = lengthOf 11})
  (status', (`fieldOf` "type") =<< v') `shouldBe` (409, Just "registration_changed")
  nrPhase <$> stored ctx moving `shouldReturn` nrPhase moving

closeOf :: Ctx -> NameRegistration -> IO Closed
closeOf Ctx {cOrders = OrdersEnv {oeStore, oeContracts, oeTurns, oeNow}} NameRegistration {nrId = RegistrationId rid} = closeRegistration oeStore oeContracts oeTurns oeNow rid

figuresOf :: Ctx -> NameRegistration -> IO (Maybe (USDCents, USDCents, USDCents, Maybe BalanceHold))
figuresOf Ctx {cStore} NameRegistration {nrId = RegistrationId rid} = figures cStore rid orderTime

refundedWhole :: Ctx -> NameRegistration -> USDCents -> IO RefundRecord
refundedWhole ctx r price = (\ref -> RefundRecord {rdRegistration = nrId r, rdCents = price, rdChain = [BalanceLink {lkRegistration = nrId r, lkOwner = nrOwner r, lkPayment = nrPayment r, lkProviderRef = Just ref, lkPaid = price, lkFromBalance = 0, lkLateCredits = []}]}) <$> paidRef ctx r

paidRef :: Ctx -> NameRegistration -> IO Text
paidRef Ctx {cStore} r = case paymentInvoice (nrPayment r) of
  Just invId -> maybe (error "no invoice row") irProviderRef <$> getInvoice cStore invId
  Nothing -> error $ "not paid by invoice: " <> show (nrPayment r)

testCloseFailed :: IO ()
testCloseFailed = withWorker $ \W {wCtx = ctx@Ctx {cStore, cOrders}} -> do
  r <- paidIn ctx aliceOrder $ \o -> (failedPhase o) {nrReveal = Just reveal}
  let again = aliceOrder {oCommitment = commitmentOf 9, oSealed = "sealed-alicebakery-9"}
      price = oPrice aliceOrder
      RegistrationId rid = nrId r
  placeOrder ctx again `shouldReturn` refusedWith 409 "order_exists"
  refunded <- refundedWhole ctx r price
  closeOf ctx r `shouldReturn` CLRefunded orderTime refunded
  x <- stored ctx r
  (nrPhase x, nrReveal x, nrVersion x) `shouldBe` (OTaken, Nothing, nrVersion r + 1)
  figuresOf ctx r `shouldReturn` Just (0, price, price, Nothing)
  turnsCount (oeTurns cOrders) `shouldReturn` 0
  recordRefund (oeTurns cOrders) cStore rid price orderTime `shouldReturn` Left (RefundAmount 0)
  retryOf ctx x `shouldReturn` nameRefusedWith 400 "bad_request"
  answerFree ctx "alicebakers"
  to <- moveTo ctx "alicebakers" 10
  move ctx keyA x to `shouldReturn` nameRefusedWith 400 "bad_request"
  closeOf ctx x `shouldReturn` CLRefused CFNotFailedPaid
  stored ctx r `shouldReturn` x
  figuresOf ctx r `shouldReturn` Just (0, price, price, Nothing)
  fst <$> placeOrder ctx again `shouldReturn` 200

testCloseLapsed :: IO ()
testCloseLapsed = withWorker $ \W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder committedPhase
  advance ctx (2 * day)
  refunded <- refundedWhole ctx r (oPrice aliceOrder)
  closeOf ctx r `shouldReturn` CLRefunded (addUTCTime day orderTime) refunded
  x <- stored ctx r
  (nrPhase x, nrHistory x) `shouldBe` (OTaken, nrHistory r)

testCloseFromBalance :: IO ()
testCloseFromBalance = withWorker $ \W {wCtx = ctx@Ctx {cStore}} -> do
  paying <- paidIn ctx (byB dynamisOrder) failedPhase
  let price = oPrice aliceOrder
  withTransaction cStore $ \db -> creditBalance db (nrId paying) 30000 orderTime
  withTransaction cStore (\db -> createBalanceRegistration db (aliceRegistration "covered" 1) BalanceUse {buSource = nrId paying, buUsed = price} 30000 orderTime) `shouldReturn` Right ()
  covered <- registrationById ctx (RegistrationId "covered") >>= \r -> rewrite ctx r failedPhase
  payingRef <- paidRef ctx paying
  closeOf ctx covered
    `shouldReturn` CLRefunded
      orderTime
      RefundRecord
        { rdRegistration = nrId covered,
          rdCents = price,
          rdChain =
            [ BalanceLink {lkRegistration = nrId covered, lkOwner = nrOwner covered, lkPayment = OPBalance BalanceUse {buSource = nrId paying, buUsed = price}, lkProviderRef = Nothing, lkPaid = 0, lkFromBalance = price, lkLateCredits = []},
              BalanceLink {lkRegistration = nrId paying, lkOwner = nrOwner paying, lkPayment = nrPayment paying, lkProviderRef = Just payingRef, lkPaid = oPrice dynamisOrder, lkFromBalance = 0, lkLateCredits = []}
            ]
        }
  nrPhase <$> stored ctx covered `shouldReturn` OTaken
  figuresOf ctx covered `shouldReturn` Just (0, price, price, Nothing)
  figuresOf ctx paying `shouldReturn` Just (30000 - price, 30000, 0, Nothing)

testCloseLanded :: IO ()
testCloseLanded = withWorker $ \W {wCtx = ctx} -> do
  owned <- paidIn ctx aliceOrder failedPhase
  answerOwner ctx "alicebakery" (nrOwner owned)
  answerExpiry ctx "alicebakery" expiry
  closeOf ctx owned `shouldReturn` CLRegistered (nrId owned)
  now <- nowOf ctx
  x <- stored ctx owned
  (nrPhase x, nrHistory x, orderLive x) `shouldBe` (ORegistered expiry now, noHistory, False)
  figuresOf ctx owned `shouldReturn` Nothing
  minedOrder <- paidIn ctx dynamisOrder failedPhase
  answerUnowned ctx "dynamis"
  answerExpiry ctx "dynamis" expiry
  minedHash <- sentRegister ctx minedOrder 1 $ TSMinedAt 120
  closeOf ctx minedOrder `shouldReturn` CLRegistered (nrId minedOrder)
  y <- stored ctx minedOrder
  (nrPhase y, nrHistory y) `shouldBe` (ORegistered expiry now, noHistory {thRegisterTx = Just minedHash})
  figuresOf ctx minedOrder `shouldReturn` Nothing

testCloseRefused :: IO ()
testCloseRefused = withWorker $ \W {wCtx = ctx@Ctx {cNode, cOrders = OrdersEnv {oeStore, oeContracts, oeTurns, oeNow}}} -> do
  closeRegistration oeStore oeContracts oeTurns oeNow "t2Jk8RmQ4vXw9NcP3yLb6g" `shouldReturn` CLRefused CFNoOrder
  answerFree ctx "carolbakery"
  (failedCode, _) <- codeIn ctx 8 (byB aliceOrder) {oLabel = "carolbakery", oCommitment = commitmentOf 2} failedPhase
  queued <- paidIn ctx aliceOrder id
  unread <- paidIn ctx dynamisOrder failedPhase
  answerCall cNode (registrarOf cNode) (ownerOfSelector <> labelBytes "dynamis") $ Reverts $ unhex "37ed32e8"
  forM_ [(queued, CFNotFailedPaid), (failedCode, CFNotFailedPaid), (unread, CFUnread)] $ \(r, refusal) -> do
    (nrId r,) <$> closeOf ctx r `shouldReturn` (nrId r, CLRefused refusal)
    stored ctx r `shouldReturn` r
    figuresOf ctx r `shouldReturn` Nothing

testCloseLostRace :: IO ()
testCloseLostRace = withWorker $ \W {wCtx = ctx@Ctx {cNode}} -> do
  r <- paidIn ctx aliceOrder failedPhase
  (closing, gate) <- pausedAt cNode "eth_call" $ closeOf ctx r
  changed <- rewrite ctx r id
  releaseGate gate
  timeout prompt (wait closing) >>= maybe (fail "the close did not finish") pure >>= (`shouldBe` CLRefused CFChanged)
  stored ctx r `shouldReturn` changed
  figuresOf ctx r `shouldReturn` Nothing

testCloseRollback :: IO ()
testCloseRollback = withWorker $ \W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder $ \o -> (failedPhase o) {nrPrice = 0}
  closeOf ctx r `shouldReturn` CLRefused CFBalanceChanged
  stored ctx r `shouldReturn` r
  figuresOf ctx r `shouldReturn` Nothing

testCloseHeld :: IO ()
testCloseHeld = withWorker $ \W {wCtx = ctx@Ctx {cStore}} -> do
  r <- paidIn ctx dynamisOrder failedPhase
  answerUnowned ctx "dynamis"
  let diffPrice = oPrice aliceOrder
      expiresAt = addUTCTime 3600 orderTime
      hold = BalanceHold {bhHolder = RegistrationId "diff", bhState = HSOpen expiresAt}
      whole = 4000 + oPrice dynamisOrder
  withTransaction cStore $ \db -> do
    creditBalance db (nrId r) 4000 orderTime
    insertInvoiceRow db "inv-diff" expiresAt "open"
    createPaidRegistration db (aliceRegistration "diff" 2) (Just BalanceUse {buSource = nrId r, buUsed = 4000}) (paidOrder "inv-diff") {poPrice = diffPrice} orderTime `shouldReturn` Right ()
  closeOf ctx r `shouldReturn` CLRefused (CFHeld hold)
  stored ctx r `shouldReturn` r
  figuresOf ctx r `shouldReturn` Just (4000, 4000, 0, Just hold)
  withTransaction cStore (\db -> releaseHeldBalance db (RegistrationId "diff") orderTime) `shouldReturn` True
  closeOf ctx r >>= \case
    CLRefunded _ RefundRecord {rdCents} -> rdCents `shouldBe` whole
    other -> expectationFailure $ "not refunded: " <> show other
  nrPhase <$> stored ctx r `shouldReturn` OTaken
  figuresOf ctx r `shouldReturn` Just (0, whole, whole, Nothing)
