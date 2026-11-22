{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesWorkerTests (namesWorkerTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesBalancesTests (figures)
import Bots.StoreService.NamesContractsTests (availableCall, maxCommitmentAgeCall, minCommitmentAgeCall, newPolicy, registrarOf)
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesOwnerTests (answerExpiry, answerFree, answerOwner, answerUnowned, byB, codeIn, committedPhase, dynamisOrder, failedPhase, labelBytes, ownerOfSelector, paidIn, rewrite, select, selectionOf, sentRegister, stored)
import Bots.StoreService.NamesPhasesTests (held, prompt, registrationPath, woken)
import Bots.StoreService.NamesStoreTests (hashFixture, reveal)
import Bots.StoreService.WebTests (answerOf, capturingStderr, fieldOf, jsonObject, lengthOf, phaseOf, phaseValue, refusedWith, renameTable, statusOf, stubCalls, webGet, withApp)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (wait, withAsync)
import qualified Control.Exception as E
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO, retry)
import Control.Monad (forM_, replicateM_, void, when)
import Data.ByteString (ByteString)
import qualified Data.Aeson as J
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Simplex.Chat.Badges.Ledger (addMonths)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word8, Word64)
import Simplex.Chat.PaymentService.Types (CurrencyAmount (..), InvoiceId (..), InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..))
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.SimplexName (labelHash)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), SystemSeconds)
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Abi (AbiValue (..), abiBytes, abiTuple, addressWord, selector, uint256)
import StoreService.Names.Balances (RefundRecord (..), RefundRefusal (..), recordRefund)
import StoreService.Names.Chain (Block (..), ChainClient, ChainError (..), Hex32 (..), TxHash (..), blockAt, decodeSignedTx, newChainClient)
import StoreService.Names.Contracts (namePolicy)
import StoreService.Names.Orders (Landing (..), OrdersEnv (..), confirmLanded)
import StoreService.Names.Phases (RegistrationId (..), awaitChange)
import StoreService.Names.Registration (RegisterIntent (..), TermsAcceptance (..), acceptCalldata, acceptanceDigest, commitCalldata, policyDomain, registerIntentDigest, registerWithCreditCalldata, sponsorDomain, termsKind)
import StoreService.Names.Reveal (intentOf)
import StoreService.Names.Revert (RevertName (..), RevertReason (..), actedReverts)
import StoreService.Names.Sender
import StoreService.Names.Store
import StoreService.Names.Turns (labelTurnCount, turnsCount, withLabelTurn)
import StoreService.Names.Worker
import StoreService.Providers (Reversal (..))
import StoreService.Store.Invoices (InvoiceRow (..), getInvoice)
import StoreService.Web.Http (createLimit, holdMicros, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Owner (selectNameHandler)
import StoreService.Web.Names.Registration (registrationHandler, retryHandler)
import StoreService.Web.Server (WebEnv (..), webApp)
import System.Timeout (timeout)
import Test.Hspec

namesWorkerTests :: Spec
namesWorkerTests = workerTests >> takenBalanceTests

takenBalanceTests :: Spec
takenBalanceTests = describe "StoreService names: balance life" $ describe "taken" $ do
  it "leaves the order as it was when the taken write's credit is refused, in one transaction" testTakenCreditRefused
  it "wakes a held poll with the taken write, and not with a change of the order's balance" testTakenWakesPoll

workerTests :: Spec
workerTests = describe "StoreService names: worker" $ do
  describe "commit" $ do
    it "commits a code order, relays the acceptance its order stored, waits for revealAfter and registers it with the stored signature, spending its code and publishing each change" testCommitWaitRegister
    it "registers a paid order from its payment to the registered view with the signature its order stored, with no reveal request" testPaidRegister
    it "registers a code order with codes_only on, from the code check to the registered view, refusing a paid order" testCodesOnlyRegister
    it "waits for a pending commit without sending another, following its fee-bumped replacement" testPendingCommit
    it "goes to committed when the commit reverts UnexpiredCommitmentExists, in its dry run or mined, with the commitment's times" testUnexpiredCommitment
    it "sends a commit mined out of gas again, storing the attempt" testCommitOutOfGas
    it "fails an order at its fifth commit mined with status 0 for its commitment, sending nothing more" testCommitsCapped
    it "retries, writing nothing, while the commitment's reveal window would pass 2^64 seconds" testRevealWindowPastRange
    it "waits a second at the lapse second, retries as usual when the dry run still reverts, then commits" testLapseSecond
    it "keeps a requeued order's intent while it commits again after a lapse, and over a third party's commitment" testIntentAcrossCommits
    it "keeps the phase while the latest block needs more than the fee cap, then commits" testFeeAboveCap
    it "commits however many commits were sent before, mined with status 1 or reverted" testMinedCommitsUncounted
  describe "writes" $ do
    it "writes nothing when the registration changed since it was read" testLostRace
    it "writes in the name's turn" testInNameTurn
  describe "register" $ do
    it "ends taken on any revert while the name is not available, releasing a code and keeping no balance" testRevertTaken
    it "logs and reports an order taken on chain once, and none taken at settlement, alerting neither 3 hours after payment" testTakeReported
    it "waits while the name reads taken only above the confirmation depth, and ends taken once it reads so at the depth" testTakenAtDepth
    it "keeps the phase on NameNotAvailable while the name still reads available, as for a pending competitor" testPendingCompetitor
    it "stores each failed dry run, fails an order at the fifth for a permanent reason, keeping a code, and retries one without data past the lapse, storing it once" testRevertFailed
    it "keeps an order committed while the sponsor allowance is used up, and registers it after a top-up" testAllowanceUsedUp
    it "keeps an order waiting on the allowance past its commitment's lapse, sending nothing, then commits again and registers with its stored intent" testAllowanceAcrossLapse
    it "fails an order at its fifth register mined with status 0 for its commitment, sending nothing, and refuses its Try again and a selection of a name" testSendsCapped
    it "clears the reveal on InvalidNonce or TermsNotAccepted, staying committed, and reads the name policy again on TermsNotAccepted" testInvalidNonce
    it "relays no intent past its deadline, clearing the reveal and staying committed" testExpiredIntent
    it "fails an order at once, logging and reporting it, when its stored signature is refused under another chain id, sending nothing, and refuses a selection of any name for it" testSignatureUnderOtherConfig
    it "fails an order at once, before relaying its acceptance, when its stored signature is refused under another chain id, sending nothing" testRelayUnderOtherConfig
    it "keeps the phase when a read of the final check fails" testCheckUnread
    it "waits a second when the dry run reverts CommitmentTooNew" testCommitmentTooNew
    it "commits again with its reveal when the dry run reverts CommitmentNotFound, and when it reverts CommitmentTooOld past the lapse, reporting that" testCommitAgain
    it "classes the contracts' errors as permanent or temporary" testRevertClasses
    it "reports temporary failures in a row, of any reasons mixed, once they have failed for 10 minutes, with the latest reason, and a permanent one never" testFailureNotice
    it "ends a register mined with status 0 taken while the name is not available, releasing a code" testMinedRevertTaken
    it "sends a register again only once none can still be mined, and writes registered when the owner holds the name" testFailedOnlySettled
  describe "terms acceptance" $ do
    it "relays the reveal's acceptance of the terms of the name's kind to the name policy as soon as it is stored, and registers once the policy holds it" testAcceptanceRelayed
    it "relays the acceptance before the wait for revealAfter, then waits for it" testAcceptanceBeforeWait
    it "clears the reveal when the relay's dry run refuses the acceptance for new terms or a used nonce, staying committed" testAcceptanceRefused
    it "registers, keeping the reveal, when the relay's dry run reverts while the policy holds the acceptance another sent first" testAcceptanceSentFirst
    it "registers, keeping the reveal, when the relay is mined with status 0 while the policy holds the acceptance another sent first" testAcceptanceMinedFailed
    it "fails an order at its fifth relay mined with status 0 for its commitment, sending nothing more" testRelaysCapped
    it "counts no relay mined with status 0 before the commitment's newest commit, as an earlier commitment's after a reset and a selection" testRelayOfEarlierCommitment
    it "retries the relay while the policy's acceptance for the commitment cannot be read, sending nothing" testAcceptanceUnread
    it "clears the reveal of an acceptance signed for another policy or for the other kind of terms, sending nothing, for the page to sign again" testAcceptanceOtherPolicy
    it "clears the reveal when a dry run reverts SignatureExpired, for the page to sign again" testAcceptanceExpired
    it "relays no acceptance once the commitment lapsed, so the policy commits nothing again" testAcceptanceLapsed
  describe "lapse" $ do
    it "keeps a committed order not revealed by revealBefore waiting, for /retry, keeping its code" testLapseFailed
  describe "restart" $ do
    it "resumes every phase after a restart, a commit saved but not recorded included" testRestart
    it "waits for a register saved but not recorded before a restart, sending no other" testRestartRegisterSaved
  describe "lanes" $ do
    it "starts a worker for each registration in its phases" testRunWorkers
    it "keeps a registration's worker after an exception in its step, logging it once as a wait, runs the step again in 15 s and reports it after 10 minutes of failures" testStepException
    it "logs a scan that fails, and the next scan starts the registration's worker" testScanFails
    it "logs a check of unregistered paid orders that fails, claiming no alert, so the next check alerts" testUnregisteredAlertsFail
    it "logs a month pass that fails, so a pass failing from the start keeps the start's time past the alert's age, until a pass completes" testMonthPassFails
    it "reports once each paid or code order not registered 3 hours after its payment, a failed one included" testUnregisteredAlert
    it "fails each order queued, committed or failed a calendar month after its payment or its code's reservation, once, reporting it, its name released, the money paid for a paid order added to its balance and a code order's code released" testMonthEnds
    it "counts the month and the 3-hour alert from the latest selection of a name, not from the payment" testMonthFromSelection
    it "ends the month on the last day of a shorter month" testMonthClipped
    it "ends no month of an order whose register is sent and not settled, and ends it once the register reverted" testMonthRegisterInFlight
    it "fails an order waiting for its owner once its page has not read it for 3 days, logging and reporting it, holding its name" testUnreadReported
    it "fails an order waiting for its owner once its page has not read it for 3 days, a read of the order or its invoice stored at most hourly" testUnreadFails
  describe "confirmation depth" $
    it "judges a register landed by ownerOf only once the owner holds the name at the confirmation depth" testLandedAtDepth
  describe "retry" $ do
    it "refuses /retry of any order but a lapsed unsigned one with bad_request, a failed or signed one included, changing nothing, and answers an unknown order 404" testRetryNotFailed
    it "requeues a lapsed committed order, whose view says it lapsed, answering an old version at once after it and keeping its code" testRetryLapsed
    it "runs in the name's turn, answering registration_changed for an order whose name was selected meanwhile" testRetryInNameTurn
  describe "close" $ do
    it "closes a failed paid order taken, not live, its price credited and refunded, its reveal cleared, then refuses /retry, a selection of a name and a second refund, and accepts a new order for the name" testCloseFailed
    it "closes a lapsed unsigned committed order, naming its lapse as its end" testCloseLapsed
    it "registers a failed order whose owner holds the name, and one whose register row mined while the owner no longer holds it, refunding nothing" testCloseLanded
    it "refuses an unknown reference, an order not failed, a failed code order and a failed chain read, changing nothing" testCloseRefused
    it "refuses an order changed during its chain check as changed, writing nothing" testCloseLostRace
    it "refunds the whole balance, its earlier credits included" testCloseWhole
    it "rolls back the taken write and the credit when the refund is refused" testCloseRollback
    it "refuses to close an order with a recorded reversal of its payment, changing nothing" testCloseReversed

-- fixtures

data W = W
  { wCtx :: Ctx,
    wChain :: ChainClient,
    wSender :: Sender,
    wWorker :: Worker
  }

-- | The worker over the orders' store and fake node, with the registration poll, Try again and the selection of a name as namesRoutes registers them.
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
        ["api", "registration", rid, "name"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = selectNameHandler cOrders (weProviders web) (weWaiters web) (RegistrationId rid)}
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

expirySeconds :: SystemSeconds
expirySeconds = RoundedSystemTime $ fromIntegral expiry

at :: Word64 -> UTCTime
at = posixSecondsToUTCTime . fromIntegral

day :: NominalDiffTime
day = 86400

-- | Committed a minute before the order.
openPhase :: NameRegistration -> NameRegistration
openPhase r = r {nrPhase = OCommitted openCommit}

openCommit :: Commit
openCommit = Commit {cmTx = Just $ TxRef (TxHash $ hashFixture 8) 100, cmRevealAfter = RoundedSystemTime $ fromIntegral $ orderSeconds - minAge, cmRevealBefore = RoundedSystemTime $ fromIntegral $ orderSeconds - minAge + maxAge}

commitAt :: Word64 -> Maybe TxRef -> Commit
commitAt t tx = Commit {cmTx = tx, cmRevealAfter = RoundedSystemTime $ fromIntegral $ t + minAge, cmRevealBefore = RoundedSystemTime $ fromIntegral $ t + maxAge}

revealBeforeOf :: NameRegistration -> Maybe Word64
revealBeforeOf = fmap (fromIntegral . roundedSeconds . cmRevealBefore) . phaseCommit . nrPhase

-- | The reveal the page sends, its intent signed with the owner's key.
signedReveal :: Ctx -> Secp256k1PrivateKey -> NameReveal -> NameRegistration -> IO NameReveal
signedReveal Ctx {cRandom, cOrders, cNode} key rv r@NameRegistration {nrCommitment, nrOwner} = do
  let RegisterIntent {riRegistration} = intentOf (oeConfig cOrders) nrOwner (nrDuration r) rv
  sig <- either (error . show) id <$> signRecoverable cRandom key (registerIntentDigest (sponsorDomain fakeChainId fakeSponsor) riRegistration (rvNonce rv) (rvDeadline rv))
  answerCall cNode fakeNamePolicy (selector "acceptedTerms(bytes32,address)" <> unHex32 nrCommitment <> addressWord nrOwner) $ Returns $ unHex32 $ termsHash $ rvAcceptance rv
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
blockTime W {wChain} n = either (error . show) (fromIntegral . roundedSeconds . blockTimestamp) <$> blockAt wChain n

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
testCommitWaitRegister = withWorker $ \w@W {wCtx = ctx} -> do
  (text, code) <- issueCode ctx 8 2 id
  r <- registrationById ctx =<< registered ctx text aliceOrder
  commitWaitRegister w r
  isJust . pcSpentAt <$> codeOf ctx code `shouldReturn` True

testPaidRegister :: IO ()
testPaidRegister = withWorker $ \w@W {wCtx = ctx@Ctx {cClient}} -> do
  (invId, _) <- placed ctx aliceOrder
  settleNow ctx invId settled `shouldReturn` ISPaid
  r <- invoiceRegistration ctx invId
  commitWaitRegister w r
  (status, view) <- answerOf <$> webGet cClient (registrationPath $ nrId r)
  (status, phaseOf =<< view, (`fieldOf` "label") =<< view) `shouldBe` (200, Just "registered", Just "alicebakery")

testCodesOnlyRegister :: IO ()
testCodesOnlyRegister = withWorkerAs codesOnly $ \w@W {wCtx = ctx@Ctx {cClient}} -> do
  (text, code) <- issueCode ctx 8 2 id
  (status, coverage) <- post ctx "/api/code/check" (J.encode $ J.object ["code" J..= text])
  (status, (`fieldOf` "state") =<< coverage) `shouldBe` (200, Just "unused")
  placeOrder ctx aliceOrder `shouldReturn` refusedWith 403 "codes_only"
  r <- registrationById ctx =<< registered ctx text aliceOrder
  commitWaitRegister w r
  isJust . pcSpentAt <$> codeOf ctx code `shouldReturn` True
  (status', view) <- answerOf <$> webGet cClient (registrationPath $ nrId r)
  (status', phaseOf =<< view) `shouldBe` (200, Just "registered")
  stubCalls (cStub ctx) `shouldReturn` []

-- | A queued order with the reveal its order stored, committed, its acceptance relayed, and registered, with no reveal request.
commitWaitRegister :: W -> NameRegistration -> IO ()
commitWaitRegister w@W {wCtx = ctx} r = do
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
  -- committed, the acceptance relayed at once
  void $ published w r $ steps w r >>= awaiting
  committed <- stored ctx r
  let commitHash = TxHash $ Hex32 $ keccak256 commitRaw
      onChain = commitAt t $ Just $ TxRef commitHash commitBlock
  (nrPhase committed, nrReveal committed) `shouldBe` (OCommitted onChain, nrReveal r)
  [_, acceptRaw] <- sent w
  (txTo <$> decodeSignedTx acceptRaw) `shouldBe` Right fakeNamePolicy
  void $ mineNewest w True
  answerAccepted w r $ Returns fakeGeneralTerms
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
  (nrPhase x, nrHistory x, orderLive x) `shouldBe` (ORegistered expirySeconds now, TxHistory {thCommitTx = Just commitHash, thRegisterTx = Just registerTx}, False)
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
  revertedAt <- nowOf ctx
  stored ctx r `shouldReturn` r {nrLastAttempt = Just NameAttempt {naTx = RTCommit, naStage = ASDryRun, naReason = "UnexpiredCommitmentExists", naAt = revertedAt}}
  dryRunsPass w
  steps w r >>= waitingUntil >>= (`shouldBe` at (t + maxAge + 1))
  x <- stored ctx r
  nrPhase x `shouldBe` OCommitted (commitAt t Nothing)
  -- a commit sent, mined with status 0 after another commit of it
  r2 <- paidIn ctx (byB dynamisOrder) {oSecret = secretOf 2} id
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
  attemptsOf ctx r `shouldReturn` (0, 1, Just (RTCommit, ASSent, "mined with status 0"))

testCommitsCapped :: IO ()
testCommitsCapped = withWorker $ \w@W {wCtx = ctx@Ctx {cStore}} -> do
  r <- paidIn ctx aliceOrder id
  withTransaction cStore $ \db -> replicateM_ 4 $ recordAttempt db (nrId r) True NameAttempt {naTx = RTCommit, naStage = ASSent, naReason = "mined with status 0", naAt = orderTime}
  answerCommitment w r 0
  void $ steps w r >>= awaiting
  void $ mineNewest w False
  steps w r >>= done
  now <- nowOf ctx
  nrPhase <$> stored ctx r `shouldReturn` OFailed now FRRegistryRefused
  attemptsOf ctx r `shouldReturn` (0, 5, Just (RTCommit, ASSent, "mined with status 0"))
  length <$> sent w `shouldReturn` 1

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
  revertedAt <- nowOf ctx
  stored ctx r `shouldReturn` r {nrLastAttempt = Just NameAttempt {naTx = RTCommit, naStage = ASDryRun, naReason = "UnexpiredCommitmentExists", naAt = revertedAt}}
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
  r2 <- paidIn ctx (byB dynamisOrder) {oSecret = secretOf 2} $ \o -> o {nrPhase = OQueued, nrHistory = oldCommit, nrReveal = Just reveal}
  answerAccepted w r2 $ Returns $ unHex32 $ termsHash $ rvAcceptance reveal
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
testMinedCommitsUncounted = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder id
  sentCommits ctx r (TSMinedAt 120) [1 .. 5]
  answerCommitment w r 0
  void $ steps w r >>= awaiting
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PCommitting
  r2 <- paidIn ctx (byB dynamisOrder) {oSecret = secretOf 2} id
  sentCommits ctx r2 (TSRevertedAt 120) [11 .. 15]
  answerCommitment w r2 0
  void $ steps w r2 >>= awaiting
  namePhase . nrPhase <$> stored ctx r2 `shouldReturn` PCommitting
  length <$> sent w `shouldReturn` 2

testLostRace :: IO ()
testLostRace = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}, wWorker} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  answerAvailable w False
  dryRunsRevert w "NameNotAvailable(string)" $ abiTuple [Dynamic $ abiBytes "alicebakery"]
  (stepping, gate) <- pausedAt cNode "eth_call" $ stepRegistration wWorker (nrId r)
  changed <- rewrite ctx r $ \o -> o {nrPhase = OCommitted openCommit {cmRevealAfter = RoundedSystemTime $ roundedSeconds (cmRevealAfter openCommit) + 1}}
  releaseGate gate
  timeout prompt (wait stepping) >>= maybe (fail "the step did not finish") (pure . shape) >>= (`shouldBe` "Again")
  stored ctx r `shouldReturn` changed
  -- held at the commitment's read, a fresh worker's first chain read
  r2 <- paidIn ctx (byB dynamisOrder) {oSecret = secretOf 2} id
  answerCommitment w r2 $ orderSeconds - 60
  W {wWorker = fresh} <- started ctx
  (stepping', gate') <- pausedAt cNode "eth_call" $ stepRegistration fresh (nrId r2)
  changed' <- rewrite ctx r2 id
  releaseGate gate'
  timeout prompt (wait stepping') >>= maybe (fail "the step did not finish") (pure . shape) >>= (`shouldBe` "Again")
  stored ctx r2 `shouldReturn` changed'

testInNameTurn :: IO ()
testInNameTurn = withWorker $ \w@W {wCtx = ctx@Ctx {cOrders}} -> do
  r <- paidIn ctx aliceOrder committedPhase
  advance ctx $ 3 * day + 1
  holding <- newEmptyMVar
  release <- newEmptyMVar
  withAsync (withLabelTurn (oeTurns cOrders) (nrLabelHash r) $ putMVar holding () >> takeMVar release) $ \_ -> do
    takeMVar holding
    withAsync (steps w r) $ \stepping -> do
      -- the unread failure's write waits for the turn the holder has
      timeout prompt (atomically $ labelTurnCount (oeTurns cOrders) (nrLabelHash r) >>= check . (== 2)) >>= maybe (fail "the worker did not queue on the turn") pure
      stored ctx r `shouldReturn` r
      putMVar release ()
      timeout prompt (wait stepping) >>= maybe (fail "the step did not finish") done
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PFailed

-- | The amount and credited of the order's balance.
balanceFigures :: Ctx -> RegistrationId -> IO (Maybe (USDCents, USDCents))
balanceFigures Ctx {cStore} rid = fmap (\Balance {blAmount, blCredited} -> (blAmount, blCredited)) <$> withTransaction cStore (`getBalance` rid)

-- | The register's dry run reverts on a name someone else holds, so the registration ends taken.
endsTaken :: W -> NameRegistration -> IO ()
endsTaken w@W {wCtx = Ctx {cNode}} r = do
  answerAvailable w False
  setFault cNode "eth_estimateGas" $ RpcError 3 "execution reverted: ERC721: invalid token ID" Nothing
  published w r (steps w r) >>= done

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

testTakeReported :: IO ()
testTakeReported = withWorker $ \w@W {wCtx = ctx@Ctx {cOrders}, wSender} -> do
  reports <- newTVarIO []
  worker <- newWorker cOrders {oeReport = \t -> atomically $ modifyTVar' reports (t :)} wSender
  (bobInv, _) <- placed ctx (byB aliceOrder) {oSecret = secretOf 2}
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  settleNow ctx bobInv settled `shouldReturn` ISPaid
  atSettlement <- invoiceRegistration ctx bobInv
  nrPhase atSettlement `shouldBe` OTaken
  stepRegistration worker (nrId atSettlement) >>= done
  readTVarIO reports `shouldReturn` []
  logs <- capturingStderr $ endsTaken w {wWorker = worker} r
  namePhase . nrPhase <$> stored ctx r `shouldReturn` PTaken
  let RegistrationId rid = nrId r
      lost = "names worker: registration " <> rid <> " was taken: its name is no longer available on chain"
  readTVarIO reports `shouldReturn` [lost]
  T.count lost logs `shouldBe` 1
  advance ctx $ 3 * 3600 + 1
  alertUnregisteredPaid worker
  readTVarIO reports `shouldReturn` [lost]

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
  dryRunsRevert w "InvalidSignature()" ""
  replicateM_ 4 $ steps w r >>= stalled >>= (`shouldBe` SFRefused RTRegister (Just $ RRNamed RNInvalidSignature))
  revertedAt <- nowOf ctx
  stored ctx r `shouldReturn` r {nrLastAttempt = Just NameAttempt {naTx = RTRegister, naStage = ASDryRun, naReason = "InvalidSignature", naAt = revertedAt}}
  attemptsOf ctx r `shouldReturn` (4, 0, Just (RTRegister, ASDryRun, "InvalidSignature"))
  steps w r >>= done
  now <- nowOf ctx
  x <- stored ctx r
  (nrPhase x, nrHistory x, orderLive x) `shouldBe` (OFailed now FRRegistryRefused, nrHistory r, True)
  pcReservedBy <$> codeOf ctx code `shouldReturn` Just (nrId r)
  -- a revert without data while the name is available is retried past the lapse, stored once, and fails nothing
  answerCall cNode fakeController (selector "available(string)" <> abiTuple [Dynamic $ abiBytes "dynamis"]) $ Returns $ uint256 (1 :: Int)
  answerUnowned ctx "dynamis"
  setFault cNode "eth_estimateGas" $ RpcError (-32000) "execution reverted" Nothing
  replicateM_ 3 $ steps w p >>= stalled
  firstAt <- nowOf ctx
  advance ctx $ fromIntegral (roundedSeconds (cmRevealBefore openCommit) + 1 - fromIntegral orderSeconds)
  void $ steps w p >>= stalled
  namePhase . nrPhase <$> stored ctx p `shouldReturn` PCommitted
  (\(dryRuns, sends, attempt) -> (dryRuns, sends, (\(_, stage, _) -> stage) <$> attempt)) <$> attemptsOf ctx p `shouldReturn` (0, 0, Just ASDryRun)
  fmap naAt . nrLastAttempt <$> stored ctx p `shouldReturn` Just firstAt
  sent w `shouldReturn` []

attemptsOf :: Ctx -> NameRegistration -> IO (Int, Int, Maybe (RegistrationTx, AttemptStage, Text))
attemptsOf Ctx {cStore} NameRegistration {nrId = rid@(RegistrationId ridText)} = withTransaction cStore $ \db -> do
  [(dryRuns, sends)] <- DB.query db "SELECT failed_dry_runs, failed_sends FROM sx_badge_service_name_registrations WHERE registration_id = ?" [ridText]
  (dryRuns,sends,) . (>>= fmap (\NameAttempt {naTx, naStage, naReason} -> (naTx, naStage, naReason)) . nrLastAttempt) <$> getRegistration db rid

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
  r <- paidIn ctx aliceOrder (\o -> o {nrPhase = OCommitted openCommit {cmRevealBefore = RoundedSystemTime $ fromIntegral lapse}}) >>= revealed ctx keyA "alicebakery"
  rv <- maybe (fail "no reveal") pure $ nrReveal r
  let registerCall = registerWithCreditCalldata $ intentOf (oeConfig cOrders) (nrOwner r) (nrDuration r) rv
  answerAvailable w True
  answerCommitment w r (lapse - maxAge)
  revertDryRun cNode registerCall $ Just $ selector "InsufficientAllowance(uint256,uint256)"
  void $ steps w r >>= stalled
  stalledAt <- nowOf ctx
  advance ctx 11
  void $ steps w r >>= stalled
  stored ctx r `shouldReturn` r {nrLastAttempt = Just NameAttempt {naTx = RTRegister, naStage = ASDryRun, naReason = "InsufficientAllowance", naAt = stalledAt}}
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
testSendsCapped = withWorker $ \w@W {wCtx = ctx@Ctx {cStore}} -> do
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  answerAvailable w True
  answerUnowned ctx "alicebakery"
  withTransaction cStore $ \db -> replicateM_ 4 $ recordAttempt db (nrId r) True NameAttempt {naTx = RTRegister, naStage = ASSent, naReason = "mined with status 0", naAt = orderTime}
  h <- sentRegister ctx r 5 $ TSRevertedAt 120
  registering <- rewrite ctx r $ \o -> o {nrPhase = ORegistering openCommit h}
  steps w registering >>= done
  now <- nowOf ctx
  x <- stored ctx r
  nrPhase x `shouldBe` OFailed now FRRegistryRefused
  attemptsOf ctx r `shouldReturn` (0, 5, Just (RTRegister, ASSent, "mined with status 0"))
  sent w `shouldReturn` []
  fst <$> retryOf ctx x `shouldReturn` 400
  answerFree ctx "alicebakers"
  to <- selectionOf ctx "alicebakers" 10
  select ctx keyA x to `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` x

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
  rewrite ctx r (\o -> o {nrReveal = Just rv {rvAcceptance = TermsAcceptance {termsHash = Hex32 terms, nonce = 0, signature = sig}}})

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
  r <- paidIn ctx aliceOrder (\o -> o {nrPhase = OCommitted openCommit {cmRevealAfter = RoundedSystemTime $ fromIntegral $ orderSeconds + 60}}) >>= acceptedReveal ctx
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

testRelaysCapped :: IO ()
testRelaysCapped = withWorker $ \w@W {wCtx = ctx@Ctx {cStore}} -> do
  r <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  withTransaction cStore $ \db -> replicateM_ 4 $ recordAttempt db (nrId r) True NameAttempt {naTx = RTAccept, naStage = ASSent, naReason = "mined with status 0", naAt = addUTCTime (-60) orderTime}
  void $ steps w r >>= awaiting
  void $ mineNewest w False
  steps w r >>= done
  now <- nowOf ctx
  nrPhase <$> stored ctx r `shouldReturn` OFailed now FRRegistryRefused
  attemptsOf ctx r `shouldReturn` (0, 5, Just (RTAccept, ASSent, "mined with status 0"))
  length <$> sent w `shouldReturn` 1

testRelayOfEarlierCommitment :: IO ()
testRelayOfEarlierCommitment = withWorker $ \w@W {wCtx = ctx@Ctx {cStore}} -> do
  r <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  withTransaction cStore $ \db -> forM_ [(RTAccept, 40, TSRevertedAt 90), (RTCommit, 41, TSMinedAt 100)] $ \(tx, n, settlement) -> do
    let h = TxHash $ hashFixture n
    void $ insertTransaction db (TxRegistration tx $ nrId r) NewTransaction {ntxRaw = "raw", ntxHash = h, ntxNonce = fromIntegral n} orderTime
    recordTransactionOutcome db h TSPending settlement `shouldReturn` True
  void $ steps w r >>= awaiting
  attemptsOf ctx r `shouldReturn` (0, 0, Nothing)
  length <$> sent w `shouldReturn` 1

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
  published w r (steps w r >>= waitingUntil) `shouldReturn` at (orderSeconds - minAge + maxAge + 1)
  x <- stored ctx r
  (nrPhase x, nrReveal x) `shouldBe` (nrPhase r, Nothing)
  sent w `shouldReturn` []

testAcceptanceLapsed :: IO ()
testAcceptanceLapsed = withWorker $ \w@W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder (\o -> o {nrPhase = OCommitted openCommit {cmRevealBefore = RoundedSystemTime $ fromIntegral $ orderSeconds - minAge}}) >>= acceptedReveal ctx
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

testCommitAgain :: IO ()
testCommitAgain = withWorker $ \w@W {wCtx = ctx@Ctx {cOrders}, wSender} -> do
  reports <- newTVarIO []
  worker <- newWorker cOrders {oeReport = \t -> atomically $ modifyTVar' reports (t :)} wSender
  r <- paidIn ctx aliceOrder openPhase >>= revealed ctx keyA "alicebakery"
  answerAvailable w True
  dryRunsRevert w "CommitmentNotFound(bytes32)" $ unHex32 (nrCommitment r)
  void $ stepRegistration worker (nrId r)
  x <- stored ctx r
  (nrPhase x, nrReveal x) `shouldBe` (OQueued, nrReveal r)
  readTVarIO reports `shouldReturn` []
  lapsed <- rewrite ctx x $ \o -> o {nrPhase = OCommitted openCommit}
  advance ctx $ fromIntegral (roundedSeconds (cmRevealBefore openCommit) + 1 - fromIntegral orderSeconds)
  dryRunsRevert w "CommitmentTooOld(bytes32,uint256,uint256)" $ unHex32 (nrCommitment r) <> uint256 (orderSeconds - minAge) <> uint256 orderSeconds
  void $ stepRegistration worker (nrId lapsed)
  y <- stored ctx r
  (nrPhase y, nrReveal y) `shouldBe` (OQueued, nrReveal r)
  readTVarIO reports `shouldReturn` ["names worker: registration " <> (\(RegistrationId t) -> t) (nrId r) <> " is committed again: its commitment lapsed"]
  sent w `shouldReturn` []

testRevertClasses :: IO ()
testRevertClasses = do
  let permanent = [RNDurationTooLong, RNDurationTooShort, RNInvalidSignature, RNLabelTooLong, RNNameTooShort, RNReentrantCall, RNResolverRequiredWhenDataSupplied, RNReverseRecordNotSupported]
  [n | n <- actedReverts, revertClass (RRNamed n) == FCPermanent] `shouldBe` permanent
  map revertClass [RRNamed (RNOther "NotSponsor"), RRNamed (RNOther "NotController"), RRNamed (RNOther "NotProxy"), RRNamed RNInsufficientAllowance, RRWithoutData, RRUnnamed "ERC721: invalid token ID"]
    `shouldBe` replicate 6 FCTemporary

testFailureNotice :: IO ()
testFailureNotice = do
  let timedOut = SFChain $ ChainUnavailable "eth_call: timed out"
      resetRead = SFChain $ ChainUnavailable "eth_call: request failed"
      thrown = SFException "SQLite3 returned ErrorIO"
      labelTooLong = SFRefused RTRegister $ Just $ RRNamed RNLabelTooLong
      everyRetry = [0, 15 .. 660]
      atTenMinutes = replicate 40 False <> [True] <> replicate 4 False
  notices (zip everyRetry $ cycle [timedOut, resetRead]) `shouldBe` (replicate 45 True, atTenMinutes)
  notices (zip everyRetry $ cycle [SFAllowanceUsedUp, thrown]) `shouldBe` (replicate 45 True, atTenMinutes)
  notices (map (,timedOut) everyRetry) `shouldBe` (True : replicate 44 False, atTenMinutes)
  notices (map (,labelTooLong) everyRetry) `shouldBe` (True : replicate 44 False, replicate 45 False)
  notices (zip everyRetry $ cycle [timedOut, SFAllowanceUsedUp]) `shouldBe` (replicate 45 True, atTenMinutes)
  failureNotice (sinceStart 600) (Just (timedOut, t0, False)) SFAllowanceUsedUp `shouldBe` ((SFAllowanceUsedUp, t0, True), True, True)
  failureNotice (sinceStart 900) (Just (thrown, t0, True)) SFAllowanceUsedUp `shouldBe` ((SFAllowanceUsedUp, t0, True), True, False)
  where
    t0 = posixSecondsToUTCTime 1795000000
    sinceStart s = addUTCTime s t0
    notices failures = unzip $ go Nothing failures
      where
        go _ [] = []
        go streak ((s, reason) : rest) =
          let (streak', logged, reported) = failureNotice (sinceStart s) streak reason
           in (logged, reported) : go (Just streak') rest

testStepException :: IO ()
testStepException = withWorker $ \W {wCtx = ctx@Ctx {cStore, cOrders}, wSender} -> do
  reports <- newTVarIO []
  worker <- newWorker cOrders {oeReport = \t -> atomically $ modifyTVar' reports (t :)} wSender
  NameRegistration {nrId = RegistrationId rid} <- paidIn ctx aliceOrder id
  withTransaction cStore $ \db -> DB.execute db "UPDATE sx_badge_service_name_registrations SET commit_block = 1 WHERE registration_id = ?" [rid]
  logged <- capturingStderr $ withAsync (runWorkers worker) $ \_ -> do
    threadDelay 6000000
    readTVarIO reports `shouldReturn` []
    advance ctx 600
    timeout 16000000 (atomically $ readTVar reports >>= check . not . null) >>= maybe (fail "the step failing for 10 minutes was not reported") pure
  T.count ("names worker: registration " <> rid <> " waits: its step failed: ") logged `shouldBe` 1
  logged `shouldNotSatisfy` T.isInfixOf "the next scan starts it again"
  readTVarIO reports >>= (`shouldSatisfy` \case
    [report] -> ("names worker: registration " <> rid <> " waits: its step failed: ") `T.isPrefixOf` report && ", failing for 10 minutes" `T.isSuffixOf` report
    _ -> False)

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
  advance ctx $ fromIntegral (roundedSeconds (rvDeadline reveal) - fromIntegral orderSeconds) + 1
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
  answerFree ctx "alicebakers"
  failedOrder <- stored ctx r
  forM_ (zip ["alicebakers", "alicebakery"] [20, 21]) $ \(label, n) -> do
    to <- selectionOf ctx label n
    select ctx keyA failedOrder to `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` failedOrder

testRelayUnderOtherConfig :: IO ()
testRelayUnderOtherConfig = withWorker $ \W {wCtx = ctx@Ctx {cOrders}, wSender} -> do
  r <- paidIn ctx aliceOrder openPhase >>= acceptedReveal ctx
  let NamesConfig {nChain} = oeConfig cOrders
  worker <- newWorker cOrders {oeConfig = (oeConfig cOrders) {nChain = nChain {cChainId = cChainId nChain + 1}}} wSender
  void $ capturingStderr $ void $ stepRegistration worker (nrId r)
  now <- nowOf ctx
  nrPhase <$> stored ctx r `shouldReturn` OFailed now FRSignatureRefused
  sentTransactions (cNode ctx) `shouldReturn` []

testFailedOnlySettled :: IO ()
testFailedOnlySettled = withWorker $ \w@W {wCtx = ctx@Ctx {cNode}, wSender} -> do
  r <- paidIn ctx aliceOrder openPhase >>= \o -> signedReveal ctx keyA reveal {rvDeadline = RoundedSystemTime 9007199254740991} o >>= \rv -> rewrite ctx o (\x -> x {nrReveal = Just rv})
  r2 <- paidIn ctx (byB dynamisOrder) {oSecret = secretOf 2} (\o -> o {nrPhase = OCommitted openCommit {cmRevealBefore = RoundedSystemTime $ fromIntegral $ orderSeconds + 3 * 86400}}) >>= revealed ctx keyB "dynamis"
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
  (nrPhase x2, thRegisterTx $ nrHistory x2) `shouldBe` (ORegistered expirySeconds now, Nothing)

testLapseFailed :: IO ()
testLapseFailed = withWorker $ \w@W {wCtx = ctx} -> do
  (r, code) <- codeIn ctx 8 aliceOrder committedPhase
  steps w r >>= waitingUntil >>= (`shouldBe` at (orderSeconds + maxAge + 1))
  advance ctx $ day + 1
  now <- nowOf ctx
  steps w r >>= waitingUntil >>= (`shouldBe` addUTCTime 60 now)
  stored ctx r `shouldReturn` r
  pcReservedBy <$> codeOf ctx code `shouldReturn` Just (nrId r)
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
  Right p <- submitTransaction wSender (TxRegistration RTRegister $ nrId r) fakeSponsor (registerWithCreditCalldata $ intentOf (oeConfig cOrders) (nrOwner r) (nrDuration r) rv)
  w1 <- started ctx
  steps w1 r >>= awaiting >>= (`shouldBe` ptHash p) . ptHash
  x <- stored ctx r
  nrPhase x `shouldBe` ORegistering openCommit (ptHash p)
  steps w1 r >>= awaiting >>= (`shouldBe` ptHash p) . ptHash
  length <$> sent w `shouldReturn` 1

testRunWorkers :: IO ()
testRunWorkers = withWorker $ \w@W {wCtx = ctx@Ctx {cNode, cOrders}, wWorker} -> do
  (unpaid, _) <- placed ctx (byB aliceOrder) {oSecret = secretOf 2}
  r <- paidIn ctx aliceOrder id
  answerCommitment w r 0
  withAsync (runWorkers wWorker) $ \_ -> do
    timeout prompt (awaitSent cNode 1) >>= maybe (fail "no commit was sent") (const $ pure ())
    changed <- awaitChange (oePhases cOrders) (nrId r) (nrVersion <$> stored ctx r) (nrVersion r) prompt
    changed `shouldBe` True
    namePhase . nrPhase <$> stored ctx r `shouldReturn` PCommitting
  nrPhase <$> invoiceRegistration ctx unpaid `shouldReturn` OUnpaid unpaid
  length <$> sent w `shouldReturn` 1

testScanFails :: IO ()
testScanFails = withWorker $ \w@W {wCtx = ctx@Ctx {cStore, cNode}, wWorker} -> do
  r <- paidIn ctx aliceOrder id
  answerCommitment w r 0
  renameTable cStore "sx_badge_service_name_registrations" "name_registrations_away"
  logged <- capturingStderr $ withAsync (runWorkers wWorker) $ \_ -> do
    threadDelay 1000000
    renameTable cStore "name_registrations_away" "sx_badge_service_name_registrations"
    timeout (5000000 + prompt) (awaitSent cNode 1) >>= maybe (fail "no commit was sent after the next scan") (const $ pure ())
  logged `shouldSatisfy` T.isInfixOf "names worker: the scan failed; the next pass will run: "

testUnregisteredAlertsFail :: IO ()
testUnregisteredAlertsFail = withWorker $ \W {wCtx = ctx@Ctx {cStore, cOrders}, wSender} -> do
  reports <- newTVarIO []
  worker <- newWorker cOrders {oeReport = \t -> atomically $ modifyTVar' reports (t :)} wSender
  _ <- paidIn ctx aliceOrder id
  advance ctx $ 3 * 3600 + 1
  renameTable cStore "sx_badge_service_name_registrations" "name_registrations_away"
  logged <- capturingStderr $ withAsync (runUnregisteredAlerts worker) $ \lane -> do
    threadDelay 500000
    Async.poll lane >>= (`shouldSatisfy` isNothing)
  renameTable cStore "name_registrations_away" "sx_badge_service_name_registrations"
  logged `shouldSatisfy` T.isInfixOf "names: the check of unregistered paid orders failed; the next pass will run: "
  readTVarIO reports `shouldReturn` []
  alertUnregisteredPaid worker
  length <$> readTVarIO reports `shouldReturn` 1

testMonthPassFails :: IO ()
testMonthPassFails = withWorker $ \W {wCtx = ctx@Ctx {cStore, cOrders}, wWorker} -> do
  serviceStart <- oeNow cOrders
  passAt <- newTVarIO serviceStart
  renameTable cStore "sx_badge_service_name_registrations" "name_registrations_away"
  logged <- capturingStderr $ withAsync (runMonthEnds passAt wWorker) $ \_ -> threadDelay 500000
  advance ctx 1201
  withAsync (runMonthEnds passAt wWorker) $ \_ -> threadDelay 500000
  renameTable cStore "name_registrations_away" "sx_badge_service_name_registrations"
  logged `shouldSatisfy` T.isInfixOf "names month pass failed; the next pass will run: "
  readTVarIO passAt `shouldReturn` serviceStart
  now <- oeNow cOrders
  diffUTCTime now <$> readTVarIO passAt >>= (`shouldSatisfy` (> 1200))
  withAsync (runMonthEnds passAt wWorker) $ \_ ->
    timeout prompt (atomically $ readTVar passAt >>= \t -> if t == serviceStart then retry else pure t) `shouldReturn` Just now

testUnregisteredAlert :: IO ()
testUnregisteredAlert = withWorker $ \W {wCtx = ctx@Ctx {cOrders}, wSender} -> do
  reports <- newTVarIO []
  worker <- newWorker cOrders {oeReport = \t -> atomically $ modifyTVar' reports (t :)} wSender
  paid <- paidIn ctx aliceOrder id
  (code, _) <- codeIn ctx 7 dynamisOrder id
  answerFree ctx "alicebakers"
  void $ paidIn ctx aliceOrder {oLabel = "alicebakers", oSecret = secretOf 5} $ \o -> o {nrPhase = ORegistered expirySeconds orderTime}
  answerFree ctx "carolbakery"
  failedOrder <- paidIn ctx aliceOrder {oLabel = "carolbakery", oSecret = secretOf 6} failedPhase
  advance ctx $ 3 * 3600
  alertUnregisteredPaid worker
  readTVarIO reports `shouldReturn` []
  advance ctx 1
  alertUnregisteredPaid worker
  alertUnregisteredPaid worker
  let named NameRegistration {nrId = RegistrationId rid} = any (T.isInfixOf rid)
  sent' <- readTVarIO reports
  (length sent', named paid sent', named code sent', named failedOrder sent') `shouldBe` (3, True, True, True)

testMonthEnds :: IO ()
testMonthEnds = withWorker $ \W {wCtx = ctx@Ctx {cStore, cOrders}, wSender} -> do
  reports <- newTVarIO []
  wWorker <- newWorker cOrders {oeReport = \t -> atomically $ modifyTVar' reports (t :)} wSender
  queued <- paidIn ctx aliceOrder id
  failed <- paidIn ctx dynamisOrder failedPhase
  answerFree ctx "alicebakers"
  committing <- paidIn ctx aliceOrder {oLabel = "alicebakers", oSecret = secretOf 5} $ \o -> o {nrPhase = OCommitting $ TxHash $ hashFixture 8}
  answerFree ctx "carolbakery"
  (code, prepaid) <- codeIn ctx 8 (byB aliceOrder) {oLabel = "carolbakery", oSecret = secretOf 6} failedPhase
  let ended = [queued, failed, code]
      holders = withTransaction cStore $ \db -> mapM (labelHolder db . nrLabelHash) ended
  advance ctx $ diffUTCTime (addMonths 1 orderTime) orderTime - 1
  endMonths wWorker
  mapM (stored ctx) (committing : ended) `shouldReturn` committing : ended
  holders `shouldReturn` map (Just . nrId) ended
  advance ctx 1
  now <- nowOf ctx
  endMonths wWorker
  endMonths wWorker
  map nrPhase <$> mapM (stored ctx) ended `shouldReturn` replicate 3 (OFailed now FRMonthEnded)
  stored ctx committing `shouldReturn` committing
  mapM (balanceFigures ctx . nrId) ended `shouldReturn` [Just (nrPrice queued, nrPrice queued), Just (nrPrice failed, nrPrice failed), Nothing]
  pcReservedBy <$> codeOf ctx prepaid `shouldReturn` Nothing
  holders `shouldReturn` [Nothing, Nothing, Nothing]
  map orderLive <$> mapM (stored ctx) ended `shouldReturn` [False, False, False]
  closeOf ctx failed `shouldReturn` CLRefused CFMonthEnded
  sent' <- readTVarIO reports
  map (\NameRegistration {nrId = RegistrationId rid} -> length $ filter (T.isInfixOf $ "order " <> rid <> " was not registered within a month") sent') ended `shouldBe` [1, 1, 1]
  length sent' `shouldBe` 3

testMonthFromSelection :: IO ()
testMonthFromSelection = withWorker $ \W {wCtx = ctx@Ctx {cStore, cOrders}, wSender} -> do
  reports <- newTVarIO []
  wWorker <- newWorker cOrders {oeReport = \t -> atomically $ modifyTVar' reports (t :)} wSender
  r <- paidIn ctx aliceOrder $ \o -> o {nrPhase = OTaken}
  withTransaction cStore $ \db -> creditBalance db (nrId r) (nrPrice r) orderTime
  answerFree ctx "alicebakers"
  advance ctx $ 20 * day
  selectedAt <- nowOf ctx
  to <- selectionOf ctx "alicebakers" 10
  fst <$> select ctx keyA r to `shouldReturn` 200
  alertUnregisteredPaid wWorker
  readTVarIO reports `shouldReturn` []
  advance ctx $ 3 * 3600 + 1
  alertUnregisteredPaid wWorker
  let RegistrationId rid = nrId r
  map (T.isInfixOf $ "order " <> rid <> " had its name selected at ") <$> readTVarIO reports `shouldReturn` [True]
  advance ctx $ diffUTCTime (addMonths 1 orderTime) orderTime - 20 * day
  endMonths wWorker
  nrPhase <$> stored ctx r `shouldReturn` OQueued
  advance ctx $ diffUTCTime (addMonths 1 selectedAt) (addMonths 1 orderTime)
  now <- nowOf ctx
  endMonths wWorker
  nrPhase <$> stored ctx r `shouldReturn` OFailed now FRMonthEnded

testUnreadFails :: IO ()
testUnreadFails = withWorker $ \w@W {wCtx = ctx@Ctx {cClient}} -> do
  r <- paidIn ctx aliceOrder committedPhase
  let readAt = fmap nrReadAt . stored ctx
      InvoiceId invId = fromJust $ paymentInvoice $ nrPayment r
  advance ctx $ 2 * day
  void $ webGet cClient $ registrationPath $ nrId r
  readAt r `shouldReturn` addUTCTime (2 * day) orderTime
  advance ctx 1800
  void $ webGet cClient $ registrationPath $ nrId r
  readAt r `shouldReturn` addUTCTime (2 * day) orderTime
  advance ctx 1800
  void $ webGet cClient $ "/api/invoice/" <> T.unpack invId
  readAt r `shouldReturn` addUTCTime (2 * day + 3600) orderTime
  advance ctx $ 3 * day
  void $ steps w r
  nrPhase <$> stored ctx r `shouldReturn` nrPhase r
  advance ctx 1
  void $ steps w r
  now <- nowOf ctx
  nrPhase <$> stored ctx r `shouldReturn` OFailed now FRUnread

testUnreadReported :: IO ()
testUnreadReported = withWorker $ \W {wCtx = ctx@Ctx {cStore, cOrders}, wSender} -> do
  reports <- newTVarIO []
  worker <- newWorker cOrders {oeReport = \t -> atomically $ modifyTVar' reports (t :)} wSender
  r <- paidIn ctx aliceOrder committedPhase
  advance ctx $ 3 * day
  void $ stepRegistration worker (nrId r)
  stored ctx r `shouldReturn` r
  advance ctx 1
  logs <- capturingStderr $ void $ stepRegistration worker (nrId r)
  now <- nowOf ctx
  nrPhase <$> stored ctx r `shouldReturn` OFailed now FRUnread
  withTransaction cStore (\db -> labelHolder db (nrLabelHash r)) `shouldReturn` Just (nrId r)
  let RegistrationId rid = nrId r
      failed = T.isInfixOf ("registration " <> rid <> " failed: it waited for its owner")
  logs `shouldSatisfy` failed
  map failed <$> readTVarIO reports `shouldReturn` [True]

testMonthClipped :: IO ()
testMonthClipped = withWorker $ \W {wCtx = ctx@Ctx {cStore, cClock}, wWorker} -> do
  r <- paidIn ctx aliceOrder id
  let paidAt = posixSecondsToUTCTime 1801396800
      lastDay = posixSecondsToUTCTime 1803816000
  withTransaction cStore $ \db -> DB.execute db "UPDATE sx_badge_service_name_registrations SET selected_at = ? WHERE registration_id = ?" (paidAt, (\(RegistrationId t) -> t) $ nrId r)
  writeIORef cClock $ addUTCTime (-1) lastDay
  endMonths wWorker
  stored ctx r `shouldReturn` r {nrSelectedAt = Just paidAt}
  writeIORef cClock lastDay
  endMonths wWorker
  nrPhase <$> stored ctx r `shouldReturn` OFailed lastDay FRMonthEnded

testMonthRegisterInFlight :: IO ()
testMonthRegisterInFlight = withWorker $ \W {wCtx = ctx@Ctx {cStore}, wWorker} -> do
  r <- paidIn ctx aliceOrder openPhase
  let h = TxHash $ hashFixture 9
  withTransaction cStore $ \db -> void $ insertTransaction db (TxRegistration RTRegister $ nrId r) NewTransaction {ntxRaw = "raw", ntxHash = h, ntxNonce = 9} orderTime
  advance ctx $ diffUTCTime (addMonths 1 orderTime) orderTime
  endMonths wWorker
  stored ctx r `shouldReturn` r
  withTransaction cStore $ \db -> recordTransactionOutcome db h TSPending (TSRevertedAt 120) `shouldReturn` True
  now <- nowOf ctx
  endMonths wWorker
  nrPhase <$> stored ctx r `shouldReturn` OFailed now FRMonthEnded

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
  (nrPhase x, nrHistory x) `shouldBe` (ORegistered expirySeconds now, noHistory)

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
  registeredOrder <- paidIn ctx dynamisOrder $ \o -> o {nrPhase = ORegistered expirySeconds orderTime}
  taken <- paidIn ctx (byB dynamisOrder) {oSecret = secretOf 4} $ \o -> o {nrPhase = OTaken}
  live <- paidIn ctx (byB dynamisOrder) {oSecret = secretOf 2} $ \o -> o {nrPhase = OCommitted openCommit {cmRevealBefore = RoundedSystemTime $ fromIntegral $ orderSeconds + 2 * maxAge}}
  answerFree ctx "alicebakers"
  failed <- paidIn ctx aliceOrder {oLabel = "alicebakers", oSecret = secretOf 5} failedPhase
  answerFree ctx "carolbakery"
  signed <- paidIn ctx (byB aliceOrder) {oLabel = "carolbakery", oSecret = secretOf 6} $ \o -> (committedPhase o) {nrReveal = Just reveal}
  advance ctx $ day + 1
  forM_ [queued, live, registeredOrder, taken, failed, signed] $ \r -> do
    (nrPhase r,) <$> retryOf ctx r `shouldReturn` (nrPhase r, refusedWith 400 "bad_request")
    stored ctx r `shouldReturn` r
  retryOf ctx queued {nrId = RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g"} `shouldReturn` refusedWith 404 "not_found"

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
  renamed <- paidIn ctx (byB dynamisOrder) {oSecret = secretOf 2} committedPhase
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
  -- a name selected for it while the retry waited for the old name's turn
  (status', v') <- whileHeld renamed (retryOf ctx renamed) (void $ rewrite ctx renamed $ \o -> o {nrLabelHash = labelHash "alicebakers", nrLength = lengthOf 11})
  (status', (`fieldOf` "type") =<< v') `shouldBe` (409, Just "registration_changed")
  nrPhase <$> stored ctx renamed `shouldReturn` nrPhase renamed

closeOf :: Ctx -> NameRegistration -> IO Closed
closeOf Ctx {cOrders = OrdersEnv {oeStore, oeContracts, oeTurns, oeNow}} NameRegistration {nrId = RegistrationId rid} = closeRegistration oeStore oeContracts oeTurns oeNow rid

figuresOf :: Ctx -> NameRegistration -> IO (Maybe (USDCents, USDCents, USDCents))
figuresOf Ctx {cStore} NameRegistration {nrId = RegistrationId rid} = figures cStore rid

refundedWhole :: Ctx -> NameRegistration -> USDCents -> IO RefundRecord
refundedWhole ctx r price = (\ref -> RefundRecord {rdRegistration = nrId r, rdCents = price, rdLink = Just BalanceLink {lkRegistration = nrId r, lkOwner = nrOwner r, lkPayment = nrPayment r, lkProviderRef = Just ref, lkPaid = price, lkLateCredits = []}}) <$> paidRef ctx r

paidRef :: Ctx -> NameRegistration -> IO Text
paidRef Ctx {cStore} r = case paymentInvoice (nrPayment r) of
  Just invId -> maybe (error "no invoice row") irProviderRef <$> getInvoice cStore invId
  Nothing -> error $ "not paid by invoice: " <> show (nrPayment r)

testCloseFailed :: IO ()
testCloseFailed = withWorker $ \W {wCtx = ctx@Ctx {cStore, cOrders}} -> do
  r <- paidIn ctx aliceOrder $ \o -> (failedPhase o) {nrReveal = Just reveal}
  let again = aliceOrder {oSecret = secretOf 9, oSealed = "sealed-alicebakery-9"}
      price = oPrice aliceOrder
      RegistrationId rid = nrId r
  placeOrder ctx again `shouldReturn` refusedWith 409 "order_exists"
  refunded <- refundedWhole ctx r price
  closeOf ctx r `shouldReturn` CLRefunded orderTime refunded
  x <- stored ctx r
  (nrPhase x, nrReveal x, nrVersion x) `shouldBe` (OTaken, Nothing, nrVersion r + 1)
  figuresOf ctx r `shouldReturn` Just (0, price, price)
  turnsCount (oeTurns cOrders) `shouldReturn` 0
  recordRefund (oeTurns cOrders) cStore rid price orderTime `shouldReturn` Left (RefundAmount 0)
  retryOf ctx x `shouldReturn` refusedWith 400 "bad_request"
  answerFree ctx "alicebakers"
  to <- selectionOf ctx "alicebakers" 10
  select ctx keyA x to `shouldReturn` refusedWith 400 "bad_request"
  closeOf ctx x `shouldReturn` CLRefused CFNotFailedPaid
  stored ctx r `shouldReturn` x
  figuresOf ctx r `shouldReturn` Just (0, price, price)
  fst <$> placeOrder ctx again `shouldReturn` 200

testCloseLapsed :: IO ()
testCloseLapsed = withWorker $ \W {wCtx = ctx} -> do
  r <- paidIn ctx aliceOrder committedPhase
  advance ctx (2 * day)
  refunded <- refundedWhole ctx r (oPrice aliceOrder)
  closeOf ctx r `shouldReturn` CLRefunded (addUTCTime day orderTime) refunded
  x <- stored ctx r
  (nrPhase x, nrHistory x) `shouldBe` (OTaken, nrHistory r)

testCloseLanded :: IO ()
testCloseLanded = withWorker $ \W {wCtx = ctx} -> do
  owned <- paidIn ctx aliceOrder failedPhase
  answerOwner ctx "alicebakery" (nrOwner owned)
  answerExpiry ctx "alicebakery" expiry
  closeOf ctx owned `shouldReturn` CLRegistered (nrId owned)
  now <- nowOf ctx
  x <- stored ctx owned
  (nrPhase x, nrHistory x, orderLive x) `shouldBe` (ORegistered expirySeconds now, noHistory, False)
  figuresOf ctx owned `shouldReturn` Nothing
  minedOrder <- paidIn ctx dynamisOrder failedPhase
  answerUnowned ctx "dynamis"
  answerExpiry ctx "dynamis" expiry
  minedHash <- sentRegister ctx minedOrder 1 $ TSMinedAt 120
  closeOf ctx minedOrder `shouldReturn` CLRegistered (nrId minedOrder)
  y <- stored ctx minedOrder
  (nrPhase y, nrHistory y) `shouldBe` (ORegistered expirySeconds now, noHistory {thRegisterTx = Just minedHash})
  figuresOf ctx minedOrder `shouldReturn` Nothing

testCloseRefused :: IO ()
testCloseRefused = withWorker $ \W {wCtx = ctx@Ctx {cNode, cOrders = OrdersEnv {oeStore, oeContracts, oeTurns, oeNow}}} -> do
  closeRegistration oeStore oeContracts oeTurns oeNow "t2Jk8RmQ4vXw9NcP3yLb6g" `shouldReturn` CLRefused CFNoOrder
  answerFree ctx "carolbakery"
  (failedCode, _) <- codeIn ctx 8 (byB aliceOrder) {oLabel = "carolbakery", oSecret = secretOf 2} failedPhase
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

testCloseReversed :: IO ()
testCloseReversed = withWorker $ \W {wCtx = ctx@Ctx {cStore}} -> do
  r <- paidIn ctx aliceOrder failedPhase
  withTransaction cStore (\db -> recordReversal db "evt_1" (fromJust $ paymentInvoice $ nrPayment r) (RVDispute "dp_1" $ CurrencyAmount 10000)) `shouldReturn` Just 10000
  closeOf ctx r `shouldReturn` CLRefused CFReversed
  stored ctx r `shouldReturn` r
  figuresOf ctx r `shouldReturn` Nothing

testCloseWhole :: IO ()
testCloseWhole = withWorker $ \W {wCtx = ctx@Ctx {cStore}} -> do
  r <- paidIn ctx dynamisOrder failedPhase
  answerUnowned ctx "dynamis"
  let whole = 4000 + oPrice dynamisOrder
  withTransaction cStore $ \db -> creditBalance db (nrId r) 4000 orderTime
  closeOf ctx r >>= \case
    CLRefunded _ RefundRecord {rdCents} -> rdCents `shouldBe` whole
    other -> expectationFailure $ "not refunded: " <> show other
  nrPhase <$> stored ctx r `shouldReturn` OTaken
  figuresOf ctx r `shouldReturn` Just (0, whole, whole)
