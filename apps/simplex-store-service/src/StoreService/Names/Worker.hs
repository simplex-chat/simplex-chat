{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Each registration from queued to registered, taken or failed: its commit, the wait for the page's reveal, its acceptance relay, and its register;
-- and /retry, which queues one whose commitment lapsed unsigned again. A step starts from the stored registration, the sender's transactions in flight
-- and the chain, so a restart resumes any phase.
module StoreService.Names.Worker
  ( Worker,
    Next (..),
    StepFailure (..),
    newWorker,
    stepRegistration,
    runWorkers,
    alertUnregisteredPaid,
    runUnregisteredAlerts,
    endMonths,
    runMonthEnds,
    Retried (..),
    RetryRefusal (..),
    retryRegistration,
    checkFailedOrder,
    Closed (..),
    CloseRefusal (..),
    closeRegistration,
    maxSendsPerKind,
    sendsUsedUp,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, poll)
import Control.Concurrent.STM
import Control.Exception (finally, mask_)
import qualified Control.Exception as E
import Control.Logger.Simple (logError, logNote, logWarn)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (textEncode)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Chat.Badges.Ledger (addMonths)
import Simplex.Messaging.TMap (TMap)
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Util (catchOwn', tryOwnLogged, tshow, ($>>=))
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Balances (RefundRecord (..))
import StoreService.Names.Chain (Block (..), BlockTag (..), ChainError (..), ChainSeconds, RevertName (..), RevertReason (..), blockAt, chainSeconds, chainTime)
import StoreService.Names.Codes (releaseCode)
import StoreService.Names.Contracts (CommitmentAges (..), Contracts (..), acceptedTerms, commitmentAges, commitmentTime, communityName, confirmedTag, controllerAvailable, forgetNamePolicy, namePolicy)
import StoreService.Names.Orders (Landing (..), OrderRefusal, OrdersEnv (..), WriteRefusal (..), checkLanded, confirmLanded, versionWritten)
import StoreService.Names.Phases (awaitChange, publish)
import StoreService.Names.Registration (RegistrationId (..), TermsAcceptance (..), acceptCalldata, commitCalldata, policyDomain, registerWithCreditCalldata, sponsorDomain, termsKind, verifyAcceptance, verifyRegisterIntent)
import StoreService.Names.Reveal (intentOf)
import StoreService.Names.Sender (PendingTx, SendError (..), Sender, awaitOutcome, inFlightTransaction, ptHash, submitTransaction)
import StoreService.Names.Store
import StoreService.Names.Turns (Turns, withBalanceTurn, withLabelTurn, withRegistrationTurn)
import StoreService.Names.View (lapseMoment, lapsedAt, registrationVersion)

data Worker = Worker
  { wkOrders :: OrdersEnv,
    wkSender :: Sender,
    -- | the thread following each registration, kept by the scan
    wkThreads :: TMap RegistrationId (Async ())
  }

-- | What a step leaves to the next one.
data Next
  = -- | the registration is in none of the worker's phases
    Done
  | -- | the step wrote, or the registration changed since it was read
    Again
  | -- | nothing to do before this time, unless the registration moves on from this version
    WaitUntil Int64 UTCTime
  | -- | a chain read or a send failed, and the phase is kept
    Retry Int64 StepFailure
  | -- | as Retry, for a cause only the operator removes, logged as an error
    Stalled Int64 StepFailure
  | -- | a transaction of the registration is in flight
    Await PendingTx

data StepFailure
  = SFChain ChainError
  | SFSend SendError
  | SFWrite OrderRefusal
  | SFUnread
  | SFAllowanceUsedUp
  | SFTakenAboveDepth
  | SFGoneButAvailable
  | SFRegisterRefused (Maybe RevertReason)
  deriving (Eq, Show)

-- | How often the stored registrations are scanned for one without a thread, such as an order just paid or requeued.
scanInterval :: Int
scanInterval = 5000000

retryInterval :: NominalDiffTime
retryInterval = 15

-- | The registration is read again after this wait for its transaction's outcome.
outcomeWait :: Int
outcomeWait = 60000000

-- | A long wait is cut into waits of at most this, so a clock adjustment delays a moment by no more.
maxWait :: NominalDiffTime
maxWait = 60

-- | Block times are whole seconds.
chainSecond :: NominalDiffTime
chainSecond = 1

newWorker :: OrdersEnv -> Sender -> IO Worker
newWorker wkOrders wkSender = Worker wkOrders wkSender <$> TM.emptyIO

-- | A thread for each registration in the worker's phases, started by a scan every scanInterval; they stop with it.
runWorkers :: Worker -> IO ()
runWorkers w@Worker {wkThreads} =
  forever (tryOwnLogged "names worker: the scan failed; the next pass will run: " (startWorkers w) >> threadDelay scanInterval)
    `finally` (readTVarIO wkThreads >>= mapM_ cancel)

startWorkers :: Worker -> IO ()
startWorkers w@Worker {wkOrders = OrdersEnv {oeStore}, wkThreads} = do
  running <- readTVarIO wkThreads >>= M.traverseMaybeWithKey (\_ a -> maybe (Just a) (const Nothing) <$> poll a)
  atomically $ writeTVar wkThreads running
  active <- withTransaction oeStore activeRegistrations
  forM_ (filter (`M.notMember` running) active) $ \rid ->
    mask_ $ asyncWithUnmask (\unmask -> unmask $ work w rid) >>= \a -> atomically $ TM.insert rid a wkThreads

work :: Worker -> RegistrationId -> IO ()
work w rid@(RegistrationId ridText) = follow w rid Nothing `catchOwn'` \e -> logError ("names worker: registration " <> ridText <> " failed; the next scan starts it again: " <> tshow e)

-- | Steps until the registration leaves the worker's phases, logging a lasting failure once.
follow :: Worker -> RegistrationId -> Maybe StepFailure -> IO ()
follow w@Worker {wkOrders = OrdersEnv {oeStore, oePhases, oeNow}} rid@(RegistrationId ridText) lastFailure =
  stepRegistration w rid >>= \case
    Done -> pure ()
    Again -> follow w rid Nothing
    WaitUntil v t -> waitFor v t >> follow w rid Nothing
    Retry v reason -> waitAgain logWarn v reason
    Stalled v reason -> waitAgain logError v reason
    Await p -> awaitOutcome outcomeWait p >> follow w rid Nothing
  where
    waitAgain logAt v reason = do
      when (Just reason /= lastFailure) $ logAt $ "names worker: registration " <> ridText <> " waits: " <> stepFailureText reason
      now <- oeNow
      waitFor v $ addUTCTime retryInterval now
      follow w rid $ Just reason
    waitFor v t = do
      now <- oeNow
      let micros = max 0 $ ceiling $ min maxWait (diffUTCTime t now) * 1000000
      void $ awaitChange oePhases rid (registrationVersion oeStore rid) v micros
    stepFailureText = \case
      SFChain e -> tshow e
      SFSend e -> tshow e
      SFWrite refusal -> "its write was refused: " <> tshow refusal
      SFUnread -> "the register check's chain read failed"
      SFAllowanceUsedUp -> "the registrar's allowance on the sponsor is used up; the guardian raises it with setRegistrarAllowance"
      SFTakenAboveDepth -> "the name reads taken only above the confirmation depth, where a reorg can undo it"
      SFGoneButAvailable -> "the dry run reverted the name as gone, but it reads available"
      SFRegisterRefused reason -> "the register reverts while the name is available, " <> maybe "without a reason" tshow reason

stepRegistration :: Worker -> RegistrationId -> IO Next
stepRegistration w@Worker {wkOrders = OrdersEnv {oeStore, oeNow}} rid = do
  now <- oeNow
  withTransaction oeStore (`getRegistration` rid) >>= \case
    Nothing -> pure Done
    Just r -> case nrPhase r of
      OQueued -> commit w now r
      OCommitting _ -> commit w now r
      OCommitted c -> committed w now r c
      ORegistering c _ -> registering w now r c
      _ -> pure Done

-- | A commit of the registration in flight is waited for; otherwise commitments(commitment) decides.
commit :: Worker -> UTCTime -> NameRegistration -> IO Next
commit w@Worker {wkOrders = OrdersEnv {oeContracts}, wkSender} now r@NameRegistration {nrId, nrCommitment} =
  inFlightTransaction wkSender (TxRegistration RTCommit nrId) >>= \case
    Just p -> commitSent w now r p
    Nothing ->
      runExceptT ((,) <$> ExceptT (commitmentAges oeContracts) <*> ExceptT (commitmentTime oeContracts nrCommitment)) >>= \case
        Left e -> pure $ failure r $ SFChain e
        Right (_, Nothing) -> sendCommit w now r
        Right (ages, Just t) -> fromCommitment w now r ages t

-- | At the lapse second a commitment can neither register nor be committed again, so the worker waits a second past it,
-- then commits again with the stale fields cleared.
fromCommitment :: Worker -> UTCTime -> NameRegistration -> CommitmentAges -> ChainSeconds -> IO Next
fromCommitment w@Worker {wkOrders = OrdersEnv {oeConfig}} now r CommitmentAges {caMin, caMax} t = case (,) <$> after caMin <*> after caMax of
  Nothing -> pure $ failure r $ SFChain $ ChainUnavailable "commitments: the reveal window passes 2^64 seconds"
  Just window@(_, lapse)
    | reusable oeConfig now lapse -> toCommitted w now r window t
    | lapse >= chainSeconds now -> pure $ waitUntil r $ addUTCTime chainSecond $ chainTime lapse
    | lapsedFields r /= r -> save w now $ lapsedFields r
    | otherwise -> sendCommit w now r
  where
    after age = if age <= maxBound - t then Just (t + age) else Nothing

-- | A live commitment, or a stored intent, is reused while revealMargin is left before its end, time for the page's reveal and the register to mine.
reusable :: NamesConfig -> UTCTime -> ChainSeconds -> Bool
reusable NamesConfig {nRevealMarginSeconds} now end = end >= chainSeconds now + fromIntegral nRevealMarginSeconds

-- | The old commitment's transactions; the reveal stays for the next commitment.
lapsedFields :: NameRegistration -> NameRegistration
lapsedFields r = case nrPhase r of
  OQueued -> r {nrHistory = noHistory}
  OCommitting _ -> r {nrPhase = OQueued, nrHistory = noHistory}
  _ -> r

-- | The reveal times come from the commitment's time. When that is not the block of our commit mined with status 1,
-- someone committed it again after a lapse, so our commit is cleared.
toCommitted :: Worker -> UTCTime -> NameRegistration -> (ChainSeconds, ChainSeconds) -> ChainSeconds -> IO Next
toCommitted w now r (revealAfter, revealBefore) t =
  ourCommit w r t >>= \case
    Left e -> pure $ failure r $ SFChain e
    Right ours -> save w now r {nrPhase = OCommitted Commit {cmTx = ours, cmRevealAfter = revealAfter, cmRevealBefore = revealBefore}}

-- | Among the signatures of the commit last sent, the one mined with status 1 at the commitment's time.
ourCommit :: Worker -> NameRegistration -> ChainSeconds -> IO (Either ChainError (Maybe TxRef))
ourCommit Worker {wkOrders = OrdersEnv {oeStore, oeContracts}} NameRegistration {nrId} t = do
  txs <- withTransaction oeStore (`registrationTransactions` nrId)
  let commits = [tx | tx@NameTransaction {ntFor = TxRegistration RTCommit _} <- txs]
      lastSent = [ntNonce | NameTransaction {ntNonce} <- take 1 $ reverse commits]
  case [TxRef ntHash block | NameTransaction {ntHash, ntNonce, ntState = TxSettled (TSMinedAt block)} <- commits, ntNonce `elem` lastSent] of
    ref@TxRef {txBlock} : _ -> fmap (\Block {blockTimestamp} -> if blockTimestamp == t then Just ref else Nothing) <$> blockAt (ctChain oeContracts) txBlock
    [] -> pure $ Right Nothing

sendCommit :: Worker -> UTCTime -> NameRegistration -> IO Next
sendCommit w@Worker {wkOrders = OrdersEnv {oeConfig}, wkSender} now r@NameRegistration {nrId, nrCommitment} =
  withinSends w now r RTCommit $ submitTransaction wkSender (TxRegistration RTCommit nrId) (cController $ nChain oeConfig) (commitCalldata nrCommitment) >>= \case
    Right p -> commitSent w now r p
    Left SEInFlight -> pure Again
    -- UnexpiredCommitmentExists among them: a commit mined since the read, or a latest block still at the lapse second
    Left e -> pure $ failure r $ SFSend e

-- | Recorded with the newest signature's hash, so a fee-bumped replacement is the one the view shows.
commitSent :: Worker -> UTCTime -> NameRegistration -> PendingTx -> IO Next
commitSent w now r@NameRegistration {nrPhase} p
  | nrPhase == OCommitting (ptHash p) = pure $ Await p
  | otherwise = save w now r {nrPhase = OCommitting $ ptHash p}

-- | Waits for the page's reveal, past the lapse for /retry unless the links were refused, has its acceptance held by the name
-- policy, waits for revealAfter, then registers.
committed :: Worker -> UTCTime -> NameRegistration -> Commit -> IO Next
committed w@Worker {wkSender} now r@NameRegistration {nrId = regId@(RegistrationId rid), nrReveal, nrReadAt} c@Commit {cmRevealAfter} =
  inFlightTransaction wkSender (TxRegistration RTRegister regId) >>= \case
    Just p -> registerSent w now r c p
    Nothing -> case nrReveal of
      Nothing
        | now > unreadAt -> do
            logNote $ "names worker: registration " <> rid <> " failed: it waited for its owner, and its page has not read it since " <> tshow nrReadAt
            save w now r {nrPhase = OFailed now FRUnread}
        | otherwise -> case lapseMoment r of
            Just lapse | now <= lapse -> pure $ waitUntil r $ min unreadAt $ addUTCTime chainSecond lapse
            _ -> pure $ waitUntil r $ min unreadAt $ addUTCTime maxWait now
      Just rv@NameReveal {rvAcceptance, rvDeadline} ->
        -- past the commitment's lapse no acceptance is relayed, as the policy would commit again; the register's dry run then decides
        (if unlapsed now c then relayAcceptance w now r c rvDeadline rvAcceptance else pure Nothing) >>= \case
          Just next -> pure next
          Nothing
            | now < chainTime cmRevealAfter -> pure $ waitUntil r $ chainTime cmRevealAfter
            | otherwise -> sendRegister w now r c rv
  where
    unreadAt = addUTCTime (unreadDays * 86400) nrReadAt

unreadDays :: NominalDiffTime
unreadDays = 3

unlapsed :: UTCTime -> Commit -> Bool
unlapsed now Commit {cmRevealBefore} = now <= chainTime cmRevealBefore

-- | The relay of the reveal's acceptance to the name policy, sent without waiting for revealAfter; Nothing once the policy
-- stores that acceptance. One signed for another policy, before a setNamePolicy, or for the other kind of terms, clears the
-- reveal for the page to sign again; the relay's dry run's revert is judged as a register's is, unless the policy holds the
-- acceptance, which another sent first.
relayAcceptance :: Worker -> UTCTime -> NameRegistration -> Commit -> ChainSeconds -> TermsAcceptance -> IO (Maybe Next)
relayAcceptance w@Worker {wkOrders = OrdersEnv {oeContracts, oeConfig = NamesConfig {nChain = ChainConfig {cChainId}}}, wkSender} now r@NameRegistration {nrId = regId@(RegistrationId rid), nrOwner, nrCommitment, nrLabelHash} c deadline a@TermsAcceptance {taTermsHash} =
  inFlightTransaction wkSender (TxRegistration RTAccept regId) >>= \case
    Just p -> pure $ Just $ Await p
    Nothing ->
      runExceptT ((,,) <$> ExceptT (namePolicy oeContracts) <*> ExceptT (communityName oeContracts nrLabelHash) <*> ExceptT held) >>= \case
        Left e -> pure $ Just $ failure r $ SFChain e
        Right (policy, community, accepted)
          | accepted == taTermsHash -> pure Nothing
          | otherwise -> Just <$> withinSends w now r RTAccept (relay policy (termsKind community))
  where
    held = acceptedTerms oeContracts nrCommitment nrOwner
    relay policy kind =
      verifyAcceptance (policyDomain cChainId policy) kind nrOwner nrCommitment deadline a >>= \case
        Left refusal -> do
          logWarn $ "names worker: registration " <> rid <> ": its stored acceptance is refused, " <> T.pack refusal
          save w now r {nrReveal = Nothing}
        Right () ->
          submitTransaction wkSender (TxRegistration RTAccept regId) policy (acceptCalldata kind nrCommitment nrOwner deadline a) >>= \case
            Right p -> pure $ Await p
            Left SEInFlight -> pure Again
            Left (SEChain (ChainReverted reason)) ->
              held >>= \case
                Right accepted | accepted == taTermsHash -> pure Again
                _ -> judge w now r c $ Just reason
            Left e -> pure $ failure r $ SFSend e

-- | The stored intent is checked before its dry run, whose revert decides at once; no label turn is held across the send.
sendRegister :: Worker -> UTCTime -> NameRegistration -> Commit -> NameReveal -> IO Next
sendRegister w@Worker {wkOrders = OrdersEnv {oeConfig = cfg@NamesConfig {nChain = ChainConfig {cChainId, cSponsor}}, oeReport}, wkSender} now r@NameRegistration {nrId = regId@(RegistrationId rid)} c rv@NameReveal {rvDeadline}
  | chainSeconds now > rvDeadline = do
      logWarn $ "names worker: registration " <> rid <> ": its stored signature's deadline passed, so its page signs again"
      save w now r {nrReveal = Nothing}
  | otherwise =
      verifyRegisterIntent (sponsorDomain cChainId cSponsor) (chainSeconds now) intent >>= \case
        Left refusal -> do
          next <- save w now r {nrPhase = OFailed now FRSignatureRefused}
          let failed = "names worker: registration " <> rid <> " failed: its stored signature is refused under the service's current configuration, " <> T.pack refusal
          logError failed
          oeReport failed
          pure next
        Right () ->
          withinSends w now r RTRegister $ submitTransaction wkSender (TxRegistration RTRegister regId) cSponsor (registerWithCreditCalldata intent) >>= \case
            Right p -> registerSent w now r c p
            Left SEInFlight -> pure Again
            Left (SEChain (ChainReverted reason)) -> judge w now r c $ Just reason
            Left e -> pure $ failure r $ SFSend e
  where
    intent = intentOf cfg r rv

registerSent :: Worker -> UTCTime -> NameRegistration -> Commit -> PendingTx -> IO Next
registerSent w now r@NameRegistration {nrPhase} c p
  | nrPhase == ORegistering c (ptHash p) = pure $ Await p
  | otherwise = save w now r {nrPhase = ORegistering c $ ptHash p}

-- | With none of its register transactions in flight, none can be mined any more: each mined, or its nonce taken.
registering :: Worker -> UTCTime -> NameRegistration -> Commit -> IO Next
registering w@Worker {wkSender} now r@NameRegistration {nrId} c =
  inFlightTransaction wkSender (TxRegistration RTRegister nrId) >>= \case
    Just p -> registerSent w now r c p
    Nothing -> judge w now r c Nothing

data Availability = AvailableAtLatest | TakenAtDepth | TakenOnlyAboveDepth

-- | A register that did not go through, by its dry run's revert or once every transaction settled: registered when one mined
-- with status 1 or the owner holds the name; committed again on InvalidNonce or a refused terms acceptance, for the page to sign again, the name
-- policy read again on TermsNotAccepted; committing again with its reveal on CommitmentTooOld, which the sponsor reverts only after the
-- intent's and the allowance's checks; kept waiting while the sponsor allowance is used up; taken on any revert while the name is not
-- available at the confirmation depth. Otherwise, unless the dry run named the name as gone, a mined register is sent again, and
-- a dry run is retried while the commitment is live and fails the order once it lapsed; committed for the page without a reveal.
judge :: Worker -> UTCTime -> NameRegistration -> Commit -> Maybe RevertReason -> IO Next
judge w@Worker {wkOrders = oe@OrdersEnv {oeContracts}} now r@NameRegistration {nrReveal} c reason =
  inTurn w r $
    confirmLanded oe r >>= \case
      LRegistered v -> pure (Just v, Again)
      LChanged -> pure (Nothing, Again)
      LUnread -> pure (Nothing, failure r SFUnread)
      LNotLanded -> case reason of
        Just (RRNamed RNInvalidNonce) -> clearReveal
        Just (RRNamed RNTermsMismatch) -> clearReveal
        Just (RRNamed RNTermsNotAccepted) -> forgetNamePolicy oeContracts >> clearReveal
        Just (RRNamed RNCommitmentTooNew) -> pure (Nothing, waitUntil r $ addUTCTime chainSecond now)
        Just (RRNamed RNCommitmentTooOld)
          | unlapsed now c -> pure (Nothing, waitUntil r $ addUTCTime chainSecond now)
          | otherwise -> store w now r {nrPhase = OQueued, nrHistory = noHistory}
        Just (RRNamed RNInsufficientAllowance) -> pure (Nothing, Stalled (nrVersion r) SFAllowanceUsedUp)
        Just (RRNamed RNNameNotAvailable) -> notRegistered True
        Just (RRNamed RNNameReserved) -> notRegistered True
        _ -> notRegistered False
  where
    clearReveal = store w now r {nrReveal = Nothing}
    notRegistered gone = case nrReveal of
      Just NameReveal {rvLabel} ->
        availability rvLabel >>= \case
          Left e -> pure (Nothing, failure r $ SFChain e)
          Right TakenAtDepth -> taken w now r
          Right TakenOnlyAboveDepth -> pure (Nothing, failure r SFTakenAboveDepth)
          -- the dry run runs on the pending block, where a competitor's register that may never mine can hold the name
          Right AvailableAtLatest
            | gone -> pure (Nothing, failure r SFGoneButAvailable)
            | otherwise -> refused
      Nothing -> store w now r {nrPhase = OCommitted c}
    refused = case nrPhase r of
      ORegistering {} -> store w now r {nrPhase = OCommitted c}
      _
        | unlapsed now c -> pure (Nothing, Stalled (nrVersion r) $ SFRegisterRefused reason)
        | otherwise -> store w now r {nrPhase = OFailed now FRRegistryRefused}
    -- at the latest block, and at the confirmation depth while the latest reads it taken
    availability label = runExceptT $ do
      latest <- ExceptT $ controllerAvailable oeContracts Latest label
      if latest
        then pure AvailableAtLatest
        else do
          tag <- ExceptT $ confirmedTag oeContracts
          atDepth <- ExceptT $ controllerAvailable oeContracts tag label
          pure $ if atDepth then TakenOnlyAboveDepth else TakenAtDepth

-- | A paid order keeps its whole price as a balance of its own, credited in the taken write's transaction, registration before balance,
-- in the balance's turn after the name's; a code order's code is released.
taken :: Worker -> UTCTime -> NameRegistration -> IO (Maybe Int64, Next)
taken Worker {wkOrders} now r = written r <$> endOrder wkOrders now r {nrPhase = OTaken}

endOrder :: OrdersEnv -> UTCTime -> NameRegistration -> IO (Either WriteRefusal Int64)
endOrder OrdersEnv {oeStore, oeTurns} now r@NameRegistration {nrId, nrPayment} = case nrPayment of
  OPInvoice {} -> credited
  OPDifference {} -> credited
  OPBalance _ -> credited
  OPCode codeId -> fmap fst <$> releaseCode oeTurns oeStore codeId nrId (\db -> versionWritten <$> updateRegistration db r now)
  where
    credited = withBalanceTurn oeTurns nrId (withTransaction oeStore $ creditedWrite now r)

creditedTaken :: UTCTime -> NameRegistration -> DB.Connection -> IO (Either WriteRefusal Int64)
creditedTaken now r = creditedWrite now r {nrPhase = OTaken}

creditedWrite :: UTCTime -> NameRegistration -> DB.Connection -> IO (Either WriteRefusal Int64)
creditedWrite now r@NameRegistration {nrId, nrPrice} db = versionWritten <$> updateRegistration db r now >>= traverse (\v -> v <$ creditBalance db nrId nrPrice now)

-- | A compare-and-set on the version read, in the caller's turn of the name.
store :: Worker -> UTCTime -> NameRegistration -> IO (Maybe Int64, Next)
store Worker {wkOrders = OrdersEnv {oeStore}} now r = written r <$> withTransaction oeStore (\db -> versionWritten <$> updateRegistration db r now)

save :: Worker -> UTCTime -> NameRegistration -> IO Next
save w now r = inTurn w r $ store w now r

-- | In the name's turn, as the requests that change an order; the version written is published after the turn.
inTurn :: Worker -> NameRegistration -> IO (Maybe Int64, Next) -> IO Next
inTurn Worker {wkOrders = OrdersEnv {oeTurns, oePhases}} NameRegistration {nrId, nrLabelHash} action = do
  (v, next) <- withLabelTurn oeTurns nrLabelHash action
  mapM_ (atomically . publish oePhases nrId) v
  pure next

written :: NameRegistration -> Either WriteRefusal Int64 -> (Maybe Int64, Next)
written r = \case
  Right v -> (Just v, Again)
  Left WRChanged -> (Nothing, Again)
  Left (WRRefused refusal) -> (Nothing, failure r $ SFWrite refusal)

failure :: NameRegistration -> StepFailure -> Next
failure r = Retry (nrVersion r)

waitUntil :: NameRegistration -> UTCTime -> Next
waitUntil r = WaitUntil (nrVersion r)

-- | What /retry did: the version it wrote, the order changed since it was read, or a refusal.
data Retried = RTWritten Int64 | RTChanged | RTRefused RetryRefusal

data RetryRefusal = RFNotLapsed | RFLinksRefused | RFUnread | RFSendsUsedUp
  deriving (Eq, Show)

-- | /retry, in the caller's turn of the order's name: an order whose commitment lapsed unsigned is queued to be committed again,
-- keeping its payment and its code.
retryRegistration :: OrdersEnv -> NameRegistration -> IO Retried
retryRegistration OrdersEnv {oeStore, oeNow} r@NameRegistration {nrId = RegistrationId rid, nrLinksRefused} = do
  now <- oeNow
  if not (lapsedAt now r)
    then pure $ RTRefused RFNotLapsed
    else case nrLinksRefused of
      -- its commitment fixes the refused links, so only another name can register
      Just _ -> pure $ RTRefused RFLinksRefused
      Nothing ->
        withTransaction oeStore (\db -> versionWritten <$> updateRegistration db r {nrPhase = OQueued, nrHistory = noHistory} now) >>= \case
          Right v -> pure $ RTWritten v
          Left WRChanged -> pure RTChanged
          Left (WRRefused refusal) -> E.throwIO $ BrokenInvariant $ "registration " <> rid <> " is live while committed, yet its requeue was refused: " <> tshow refusal

checkFailedOrder :: OrdersEnv -> NameRegistration -> IO (Either Retried ())
checkFailedOrder oe r =
  confirmLanded oe r >>= \case
    LRegistered v -> pure $ Left $ RTWritten v
    LChanged -> pure $ Left RTChanged
    LUnread -> pure $ Left $ RTRefused RFUnread
    LNotLanded -> (\usedUp -> if usedUp then Left (RTRefused RFSendsUsedUp) else Right ()) <$> sendsUsedUp oe r

-- | An owner who changes their terms acceptance on chain makes every relay and register revert at the service's gas, so each
-- kind of transaction is sent at most this many times for one order, its moves included.
maxSendsPerKind :: Int
maxSendsPerKind = 5

-- | The send while the order has sent fewer than maxSendsPerKind transactions of its kind, else the order fails.
withinSends :: Worker -> UTCTime -> NameRegistration -> RegistrationTx -> IO Next -> IO Next
withinSends w@Worker {wkOrders} now r@NameRegistration {nrId = RegistrationId rid} step send = do
  txs <- orderTransactions wkOrders r
  let minedCommits = [ntNonce | NameTransaction {ntFor = TxRegistration RTCommit _, ntNonce, ntState = TxSettled TSMinedAt {}} <- txs]
      sent = sendCount step [tx | tx@NameTransaction {ntNonce} <- txs, ntNonce `notElem` minedCommits]
  if sent < maxSendsPerKind
    then send
    else do
      logWarn $ "names worker: registration " <> rid <> " failed: it sent " <> tshow sent <> " transactions of one kind, the most an order may"
      save w now r {nrPhase = OFailed now FRSendsUsedUp}

sendsUsedUp :: OrdersEnv -> NameRegistration -> IO Bool
sendsUsedUp oe r = orderTransactions oe r <&> \txs -> any (\step -> sendCount step txs >= maxSendsPerKind) [minBound .. maxBound]

-- | One per nonce, as a fee replacement is the same send.
sendCount :: RegistrationTx -> [NameTransaction] -> Int
sendCount step txs = length $ nubOrd [ntNonce | NameTransaction {ntFor = TxRegistration s _, ntNonce} <- txs, s == step]

orderTransactions :: OrdersEnv -> NameRegistration -> IO [NameTransaction]
orderTransactions OrdersEnv {oeStore} NameRegistration {nrId} = withTransaction oeStore (`registrationTransactions` nrId)

data Closed = CLRegistered RegistrationId | CLRefunded UTCTime RefundRecord | CLRefused CloseRefusal
  deriving (Eq, Show)

data CloseRefusal = CFNoOrder | CFNotFailedPaid | CFMonthEnded | CFUnread | CFChanged | CFHeld BalanceHold | CFBalanceChanged
  deriving (Eq, Show)

newtype CloseRefused = CloseRefused CloseRefusal
  deriving (Show)

instance E.Exception CloseRefused

closeRegistration :: DBStore -> Contracts -> Turns -> IO UTCTime -> Text -> IO Closed
closeRegistration st ct turns clock ref = do
  now <- clock
  withTransaction st (\db -> balanceByReference db ref now $>>= getRegistration db . fst) >>= \case
    Nothing -> refused CFNoOrder
    Just r ->
      withRegistrationTurn st turns r (closeFailed now) <&> \case
        Just (Right closed) -> closed
        _ -> CLRefused CFChanged
  where
    refused = pure . CLRefused
    closeFailed now r@NameRegistration {nrPayment, nrPhase} = case (nrPayment, nrPhase, lapseMoment r) of
      (OPCode _, _, _) -> refused CFNotFailedPaid
      (_, OFailed _ FRMonthEnded, _) -> refused CFMonthEnded
      (_, OFailed failedAt _, _) -> close now r failedAt
      (_, _, Just lapse) | now > lapse -> close now r lapse
      _ -> refused CFNotFailedPaid
    close now r@NameRegistration {nrId} endedAt =
      checkLanded st ct turns clock r >>= \case
        LRegistered _ -> pure $ CLRegistered nrId
        LChanged -> refused CFChanged
        LUnread -> refused CFUnread
        LNotLanded -> either CLRefused (CLRefunded endedAt) <$> refundTaken st turns now r {nrReveal = Nothing}

refundTaken :: DBStore -> Turns -> UTCTime -> NameRegistration -> IO (Either CloseRefusal RefundRecord)
refundTaken st turns now r@NameRegistration {nrId} =
  withBalanceTurn turns nrId $ either (\(CloseRefused refusal) -> Left refusal) id <$> E.try (withTransaction st refund)
  where
    refund db =
      getBalance db nrId now >>= \case
        Just Balance {blHold = Just hold} -> pure $ Left $ CFHeld hold
        _ ->
          creditedTaken now r db >>= \case
            Left WRChanged -> pure $ Left CFChanged
            Left (WRRefused refusal) -> E.throwIO $ BrokenInvariant $ "the taken write of order " <> ridText <> " was refused as " <> tshow refusal <> ", while a write that closes an order is refused only as changed"
            Right _ -> Right <$> refundAll db
    refundAll db = do
      cents <- maybe 0 blAmount <$> getBalance db nrId now
      refundBalance db nrId cents now >>= \case
        True -> RefundRecord nrId cents <$> balanceChain db nrId
        False -> E.throwIO $ CloseRefused CFBalanceChanged
    RegistrationId ridText = nrId

unregisteredAlertAge :: NominalDiffTime
unregisteredAlertAge = 3 * 3600

unregisteredCheckInterval :: Int
unregisteredCheckInterval = 300 * 1000000

alertUnregisteredPaid :: Worker -> IO ()
alertUnregisteredPaid Worker {wkOrders = OrdersEnv {oeStore, oeNow, oeReport}} = do
  now <- oeNow
  due <- withTransaction oeStore (`unregisteredPaidBefore` addUTCTime (negate unregisteredAlertAge) now)
  forM_ due $ \(rid@(RegistrationId ridText), phase, paidAt) -> do
    claimed <- withTransaction oeStore $ \db -> claimUnregisteredAlert db rid now
    when claimed $ do
      let alert = "names: order " <> ridText <> " was paid at " <> tshow paidAt <> " and is " <> textEncode phase <> ", not registered, 3 hours later"
      logError alert
      oeReport alert

runUnregisteredAlerts :: Worker -> IO ()
runUnregisteredAlerts w = forever $ tryOwnLogged "names: the check of unregistered paid orders failed; the next pass will run: " (alertUnregisteredPaid w) >> threadDelay unregisteredCheckInterval

shortestMonth :: NominalDiffTime
shortestMonth = 28 * 86400

monthCheckInterval :: Int
monthCheckInterval = 300 * 1000000

endMonths :: Worker -> IO ()
endMonths Worker {wkOrders = oe@OrdersEnv {oeStore, oeTurns, oePhases, oeNow}} = do
  now <- oeNow
  due <- withTransaction oeStore (`monthCandidates` addUTCTime (negate shortestMonth) now)
  forM_ [rid | (rid, paidAt) <- due, addMonths 1 paidAt <= now] $ \rid@(RegistrationId ridText) ->
    ( withTransaction oeStore (`getRegistration` rid)
        >>= traverse (\r -> withRegistrationTurn oeStore oeTurns r (monthEnded now))
        >>= \case
          Just (Just (Right (Right (v, kept)))) -> do
            atomically $ publish oePhases rid v
            logNote $ "names: order " <> ridText <> " was not registered within a month of its payment, so it failed, " <> kept
          _ -> pure ()
    )
      `catchOwn'` \e -> logError ("names month pass: registration " <> ridText <> " failed; the next pass tries it again: " <> tshow e)
  where
    monthEnded now r@NameRegistration {nrPayment}
      | orderLive r && namePhase (nrPhase r) `elem` [PQueued, PCommitted, PFailed] = fmap (,kept) <$> endOrder oe now r {nrPhase = OFailed now FRMonthEnded}
      | otherwise = pure $ Left WRChanged
      where
        kept = case nrPayment of
          OPCode _ -> "its code released"
          _ -> "the money paid for it added to its balance"

runMonthEnds :: Worker -> IO ()
runMonthEnds w = forever $ tryOwnLogged "names month pass failed; the next pass will run: " (endMonths w) >> threadDelay monthCheckInterval
