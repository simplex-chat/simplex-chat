{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Each registration from queued to registered, ended, taken or failed: its commit, the wait for the page's reveal, its acceptance relay, and its register;
-- /retry, which queues one whose commitment lapsed unsigned again; and the hourly sweep, which sets a registered one ended once its registration ended
-- in its dispute period. A step starts from the stored registration, the sender's transactions in flight and the chain, so a restart resumes any phase.
module StoreService.Names.Worker
  ( Worker,
    Next (..),
    StepFailure (..),
    newWorker,
    stepRegistration,
    runWorkers,
    alertUnregisteredSelected,
    runUnregisteredAlerts,
    endMonths,
    runMonthEnds,
    sweepEndedRegistrations,
    runEndedSweep,
    Retried (..),
    RetryRefusal (..),
    retryRegistration,
    Closed (..),
    CloseRefusal (..),
    closeRegistration,
    FailureClass (..),
    revertClass,
    failureNotice,
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
import Data.Bifunctor (first)
import Data.Either (lefts)
import Data.Functor ((<&>))
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Encoding.String (textEncode)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Chat.Badges.Ledger (addMonths)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), SystemSeconds, roundedToUTCTime)
import Simplex.Messaging.TMap (TMap)
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Util (catchOwn', tshow, ($>>=))
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Balances (RefundRecord (..))
import StoreService.Names.Chain (Block (..), BlockTag (..), ChainError (..), TxHash, blockAt)
import StoreService.Names.Codes (releaseCode)
import StoreService.Names.Contracts (CommitmentAges (..), Contracts (..), acceptedTerms, commitmentAges, commitmentTime, communityName, confirmedTag, controllerAvailable, disputeUntil, forgetNamePolicy, namePolicy)
import StoreService.Names.Orders (Landing (..), OrderRefusal, OrdersEnv (..), WriteRefusal (..), checkLanded, confirmLanded, confirmedBlock, versionWritten)
import StoreService.Names.Phases (RegistrationId (..), awaitChange, publish)
import StoreService.Names.Registration (TermsAcceptance (..), acceptCalldata, commitCalldata, disputeSeconds, policyDomain, registerWithCreditCalldata, sponsorDomain, termsKind, verifyAcceptance, verifyRegisterIntent)
import StoreService.Names.Reveal (intentOf)
import StoreService.Names.Revert (RevertName (..), RevertReason (..))
import StoreService.Names.Sender (PendingTx, SendError (..), Sender, awaitOutcome, inFlightTransaction, ptHash, submitTransaction, submitTransactionIf)
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
  | SFRefused RegistrationTx (Maybe RevertReason)
  | SFException Text
  deriving (Eq, Show)

data FailureClass = FCTemporary | FCPermanent
  deriving (Eq, Show)

revertClass :: RevertReason -> FailureClass
revertClass = \case
  RRNamed n
    | n `elem` [RNInvalidSignature, RNDurationTooLong, RNDurationTooShort, RNResolverRequiredWhenDataSupplied, RNReverseRecordNotSupported, RNNameTooShort, RNLabelTooLong, RNReentrantCall] -> FCPermanent
  _ -> FCTemporary

stepClass :: StepFailure -> FailureClass
stepClass = \case
  SFChain e -> chainClass e
  SFSend (SEChain e) -> chainClass e
  SFSend _ -> FCTemporary
  SFWrite _ -> FCTemporary
  SFUnread -> FCTemporary
  SFAllowanceUsedUp -> FCTemporary
  SFTakenAboveDepth -> FCTemporary
  SFGoneButAvailable -> FCTemporary
  SFRefused _ reason -> maybe FCTemporary revertClass reason
  SFException _ -> FCTemporary
  where
    chainClass = \case
      ChainReverted reason -> revertClass reason
      _ -> FCTemporary

reportAfter :: NominalDiffTime
reportAfter = 600

type FailureStreak = (StepFailure, UTCTime, Bool)

failureNotice :: UTCTime -> Maybe FailureStreak -> StepFailure -> (FailureStreak, Bool, Bool)
failureNotice now streak reason = ((reason, since, reported || report), logged, report)
  where
    (since, reported) = maybe (now, False) (\(_, at, done) -> (at, done)) streak
    logged = maybe True (\(lastReason, _, _) -> lastReason /= reason) streak
    report = not reported && stepClass reason /= FCPermanent && diffUTCTime now since >= reportAfter

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
  forever ((startWorkers w `catchOwn'` \e -> logError ("names worker: the scan failed; the next pass will run: " <> tshow e)) >> threadDelay scanInterval)
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

-- | Steps until the registration leaves the worker's phases, logging a lasting failure once and reporting it as its class says.
follow :: Worker -> RegistrationId -> Maybe FailureStreak -> IO ()
follow w@Worker {wkOrders = OrdersEnv {oeStore, oePhases, oeNow, oeReport}} rid@(RegistrationId ridText) streak =
  ((Right <$> stepRegistration w rid) `catchOwn'` (pure . Left)) >>= \case
    Left e -> waitAgain logError (SFException $ tshow e) $ \_ -> threadDelay $ ceiling $ retryInterval * 1000000
    Right next -> case next of
      Done -> pure ()
      Again -> follow w rid Nothing
      WaitUntil v t -> waitFor v t >> follow w rid Nothing
      Retry v reason -> waitAgain logWarn reason $ waitFor v
      Stalled v reason -> waitAgain logError reason $ waitFor v
      Await p -> awaitOutcome outcomeWait p >> follow w rid Nothing
  where
    waitAgain :: (Text -> IO ()) -> StepFailure -> (UTCTime -> IO ()) -> IO ()
    waitAgain logAt reason wait = do
      now <- oeNow
      let (streak', logged, reported) = failureNotice now streak reason
          waits = "names worker: registration " <> ridText <> " waits: " <> stepFailureText reason
      when logged $ logAt waits
      when reported $ oeReport $ waits <> ", failing for 10 minutes"
      wait $ addUTCTime retryInterval now
      follow w rid $ Just streak'
    waitFor v t = do
      now <- oeNow
      let micros = max 0 $ ceiling $ min maxWait (diffUTCTime t now) * 1000000
      void $ awaitChange oePhases rid (registrationVersion oeStore rid) v micros
    stepFailureText = \case
      SFChain e -> tshow e
      SFSend e -> tshow e
      SFWrite refusal -> "its write was refused: " <> tshow refusal
      SFUnread -> "its register could not be checked at the confirmed block"
      SFAllowanceUsedUp -> "the registrar's allowance on the sponsor is used up; the guardian raises it with setRegistrarAllowance"
      SFTakenAboveDepth -> "the name reads taken only above the confirmation depth, where a reorg can undo it"
      SFGoneButAvailable -> "the dry run reverted the name as gone, but it reads available"
      SFRefused tx reason -> "the " <> textEncode tx <> " reverts while the name is available, " <> maybe "without a reason" reasonText reason
      SFException e -> "its step failed: " <> e

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
        Right (_, Nothing) -> case nrPhase r of
          OCommitting h ->
            commitReverted w r h >>= \case
              True -> inTurn w r $ failedAttempt w now r RTCommit ASSent Nothing $ store w now r {nrPhase = OQueued}
              False -> sendCommit w now r
          _ -> sendCommit w now r
        Right (ages, Just t) -> fromCommitment w now r ages t

-- | The commit last sent, by its signature's nonce, mined with status 0.
commitReverted :: Worker -> NameRegistration -> TxHash -> IO Bool
commitReverted Worker {wkOrders = OrdersEnv {oeStore}} NameRegistration {nrId} h = do
  txs <- withTransaction oeStore (`registrationTransactions` nrId)
  let nonces = [ntNonce | NameTransaction {ntHash, ntNonce} <- txs, ntHash == h]
  pure $ or [True | NameTransaction {ntNonce, ntState = TxSettled TSRevertedAt {}} <- txs, ntNonce `elem` nonces]

-- | The newest relay of the current commitment, sent after its newest commit, by its signature's nonce, mined with status 0 and
-- with no failed attempt stored since it was sent.
relayReverted :: Worker -> NameRegistration -> IO Bool
relayReverted Worker {wkOrders = OrdersEnv {oeStore}} NameRegistration {nrId, nrLastAttempt} = withTransaction oeStore $ \db -> do
  relays <- filter (sentFor RTAccept) . reverse . takeWhile (not . sentFor RTCommit) . reverse <$> registrationTransactions db nrId
  let newest = [ntNonce | NameTransaction {ntNonce} <- take 1 $ reverse relays]
      group = [t | t@NameTransaction {ntNonce} <- relays, ntNonce `elem` newest]
      reverted = or [True | NameTransaction {ntState = TxSettled TSRevertedAt {}} <- group]
      stored sentAt = maybe False (\NameAttempt {naAt} -> naAt >= sentAt) nrLastAttempt
  pure $ case [ntSentAt | NameTransaction {ntSentAt} <- group] of
    [] -> False
    sent -> reverted && not (stored $ maximum sent)
  where
    sentFor tx NameTransaction {ntFor} = ntFor == TxRegistration tx nrId

-- | At the lapse second a commitment can neither register nor be committed again, so the worker waits a second past it,
-- then commits again with the stale fields cleared.
fromCommitment :: Worker -> UTCTime -> NameRegistration -> CommitmentAges -> SystemSeconds -> IO Next
fromCommitment w@Worker {wkOrders = OrdersEnv {oeConfig}} now r CommitmentAges {caMin, caMax} t = case (,) <$> after caMin <*> after caMax of
  Nothing -> pure $ failure r $ SFChain $ ChainUnavailable "commitments: the reveal window passes 2^63 seconds"
  Just window@(_, lapse)
    | reusable oeConfig now lapse -> toCommitted w now r window t
    | roundedToUTCTime lapse >= now -> pure $ waitUntil r $ addUTCTime chainSecond $ roundedToUTCTime lapse
    | lapsedFields r /= r -> save w now $ lapsedFields r
    | otherwise -> sendCommit w now r
  where
    after age
      | toInteger age <= toInteger (maxBound :: Int64) - toInteger (roundedSeconds t) = Just $ RoundedSystemTime $ roundedSeconds t + fromIntegral age
      | otherwise = Nothing

-- | A live commitment, or a stored intent, is reused while revealMargin is left before its end, time for the page's reveal and the register to mine.
reusable :: NamesConfig -> UTCTime -> SystemSeconds -> Bool
reusable NamesConfig {nRevealMarginSeconds} now end = roundedToUTCTime end >= addUTCTime (fromIntegral nRevealMarginSeconds) now

-- | The old commitment's transactions; the reveal stays for the next commitment.
lapsedFields :: NameRegistration -> NameRegistration
lapsedFields r = case nrPhase r of
  OQueued -> r {nrHistory = noHistory}
  OCommitting _ -> r {nrPhase = OQueued, nrHistory = noHistory}
  _ -> r

-- | The reveal times come from the commitment's time. When that is not the block of our commit mined with status 1,
-- someone committed it again after a lapse, so our commit is cleared.
toCommitted :: Worker -> UTCTime -> NameRegistration -> (SystemSeconds, SystemSeconds) -> SystemSeconds -> IO Next
toCommitted w now r (revealAfter, revealBefore) t =
  ourCommit w r t >>= \case
    Left e -> pure $ failure r $ SFChain e
    Right ours -> save w now r {nrPhase = OCommitted Commit {cmTx = ours, cmRevealAfter = revealAfter, cmRevealBefore = revealBefore}}

-- | Among the signatures of the commit last sent, the one mined with status 1 at the commitment's time.
ourCommit :: Worker -> NameRegistration -> SystemSeconds -> IO (Either ChainError (Maybe TxRef))
ourCommit Worker {wkOrders = OrdersEnv {oeStore, oeContracts}} NameRegistration {nrId} t = do
  txs <- withTransaction oeStore (`registrationTransactions` nrId)
  let commits = [tx | tx@NameTransaction {ntFor = TxRegistration RTCommit _} <- txs]
      lastSent = [ntNonce | NameTransaction {ntNonce} <- take 1 $ reverse commits]
  case [TxRef ntHash block | NameTransaction {ntHash, ntNonce, ntState = TxSettled (TSMinedAt block)} <- commits, ntNonce `elem` lastSent] of
    ref@TxRef {txBlock} : _ -> fmap (\Block {blockTimestamp} -> if blockTimestamp == t then Just ref else Nothing) <$> blockAt (ctChain oeContracts) txBlock
    [] -> pure $ Right Nothing

sendCommit :: Worker -> UTCTime -> NameRegistration -> IO Next
sendCommit w@Worker {wkOrders = OrdersEnv {oeConfig}, wkSender} now r@NameRegistration {nrId, nrCommitment} =
  submitTransaction wkSender (TxRegistration RTCommit nrId) (cController $ nChain oeConfig) (commitCalldata nrCommitment) >>= \case
    Right p -> commitSent w now r p
    Left SEInFlight -> pure Again
    -- UnexpiredCommitmentExists among them: a commit mined since the read, or a latest block still at the lapse second
    Left e@(SEChain (ChainReverted reason)) -> inTurn w r $ failedAttempt w now r RTCommit ASDryRun (Just reason) $ pure (Nothing, failure r $ SFSend e)
    Left e -> pure $ failure r $ SFSend e

-- | Recorded with the newest signature's hash, so a fee-bumped replacement is the one the view shows.
commitSent :: Worker -> UTCTime -> NameRegistration -> PendingTx -> IO Next
commitSent w now r@NameRegistration {nrPhase} p
  | nrPhase == OCommitting (ptHash p) = pure $ Await p
  | otherwise = save w now r {nrPhase = OCommitting $ ptHash p}

-- | Waits for the page's reveal, past the lapse for /retry, has its acceptance held by the name
-- policy, waits for revealAfter, then registers.
committed :: Worker -> UTCTime -> NameRegistration -> Commit -> IO Next
committed w@Worker {wkSender} now r@NameRegistration {nrId = regId, nrReveal, nrReadAt} c@Commit {cmRevealAfter} =
  inFlightTransaction wkSender (TxRegistration RTRegister regId) >>= \case
    Just p -> registerSent w now r c p
    Nothing -> case nrReveal of
      Nothing
        | now > unreadAt -> inTurn w r $ failOrder w now r FRUnread $ "it waited for its owner, and its page has not read it since " <> tshow nrReadAt
        | otherwise -> case lapseMoment r of
            Just lapse | now <= lapse -> pure $ waitUntil r $ min unreadAt $ addUTCTime chainSecond lapse
            _ -> pure $ waitUntil r $ min unreadAt $ addUTCTime maxWait now
      Just rv ->
        -- past the commitment's lapse no acceptance is relayed, as the policy would commit again; the register's dry run then decides
        (if unlapsed now c then relayAcceptance w now r c rv else pure Nothing) >>= \case
          Just next -> pure next
          Nothing
            | now < roundedToUTCTime cmRevealAfter -> pure $ waitUntil r $ roundedToUTCTime cmRevealAfter
            | otherwise -> sendRegister w now r c rv
  where
    unreadAt = addUTCTime (unreadDays * 86400) nrReadAt

unreadDays :: NominalDiffTime
unreadDays = 3

unlapsed :: UTCTime -> Commit -> Bool
unlapsed now Commit {cmRevealBefore} = now <= roundedToUTCTime cmRevealBefore

-- | The relay of the reveal's acceptance to the name policy, sent without waiting for revealAfter; Nothing once the policy
-- stores that acceptance. One signed for another policy, before a setNamePolicy, or for the other kind of terms, clears the
-- reveal for the page to sign again; the relay's dry run's revert is judged as a register's is, unless the policy holds the
-- acceptance, which another sent first.
relayAcceptance :: Worker -> UTCTime -> NameRegistration -> Commit -> NameReveal -> IO (Maybe Next)
relayAcceptance w@Worker {wkOrders = OrdersEnv {oeContracts, oeConfig = NamesConfig {nChain = ChainConfig {cChainId}}}, wkSender} now r@NameRegistration {nrId = regId@(RegistrationId rid), nrOwner, nrCommitment, nrLabelHash} c rv@NameReveal {rvDeadline = deadline, rvAcceptance = a@TermsAcceptance {termsHash}} =
  inFlightTransaction wkSender (TxRegistration RTAccept regId) >>= \case
    Just p -> pure $ Just $ Await p
    Nothing ->
      runExceptT ((,,) <$> ExceptT (namePolicy oeContracts) <*> ExceptT (communityName oeContracts nrLabelHash) <*> ExceptT held) >>= \case
        Left e -> pure $ Just $ failure r $ SFChain e
        Right (policy, community, accepted)
          | accepted == termsHash -> pure Nothing
          | otherwise -> Just <$> relay policy (termsKind community)
  where
    held = acceptedTerms oeContracts nrCommitment nrOwner
    relay policy kind =
      relayReverted w r >>= \case
        True -> inTurn w r $ failedAttempt w now r RTAccept ASSent Nothing $ pure (Nothing, Again)
        False
          | now > roundedToUTCTime deadline -> verifyRelay policy kind
          | otherwise -> intentRefused w now r rv >>= maybe (verifyRelay policy kind) pure
    verifyRelay policy kind =
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
                Right accepted | accepted == termsHash -> pure Again
                _ -> judge w now r c RTAccept $ Just reason
            Left e -> pure $ failure r $ SFSend e

-- | The stored intent is checked before its dry run, whose revert decides at once; no label turn is held across the send.
sendRegister :: Worker -> UTCTime -> NameRegistration -> Commit -> NameReveal -> IO Next
sendRegister w@Worker {wkOrders = OrdersEnv {oeConfig = cfg@NamesConfig {nChain = ChainConfig {cSponsor}}}, wkSender} now r@NameRegistration {nrId = regId@(RegistrationId rid)} c rv@NameReveal {rvDeadline}
  | now > roundedToUTCTime rvDeadline = do
      logWarn $ "names worker: registration " <> rid <> ": its stored signature's deadline passed, so its page signs again"
      save w now r {nrReveal = Nothing}
  | otherwise =
      intentRefused w now r rv >>= \case
        Just next -> pure next
        Nothing ->
          submitTransactionIf wkSender (TxRegistration RTRegister regId) (`lockRegistration` r) cSponsor (registerWithCreditCalldata $ intentOf cfg (nrOwner r) (nrDuration r) (nrDisputeDays r) rv) >>= \case
            Right p -> registerSent w now r c p
            Left SEInFlight -> pure Again
            Left SEChanged -> pure Again
            Left (SEChain (ChainReverted reason)) -> judge w now r c RTRegister $ Just reason
            Left e -> pure $ failure r $ SFSend e

intentRefused :: Worker -> UTCTime -> NameRegistration -> NameReveal -> IO (Maybe Next)
intentRefused w@Worker {wkOrders = OrdersEnv {oeConfig = cfg@NamesConfig {nChain = ChainConfig {cChainId, cSponsor}}}} now r@NameRegistration {nrId = RegistrationId rid} rv =
  verifyRegisterIntent (sponsorDomain cChainId cSponsor) now (intentOf cfg (nrOwner r) (nrDuration r) (nrDisputeDays r) rv) >>= \case
    Left refusal -> do
      logWarn $ "names worker: registration " <> rid <> ": its stored signature is refused under the service's current configuration, " <> T.pack refusal <> ", so its page signs again"
      Just <$> save w now r {nrReveal = Nothing}
    Right () -> pure Nothing

registerSent :: Worker -> UTCTime -> NameRegistration -> Commit -> PendingTx -> IO Next
registerSent w now r@NameRegistration {nrPhase} c p
  | nrPhase == ORegistering c (ptHash p) = pure $ Await p
  | otherwise = save w now r {nrPhase = ORegistering c $ ptHash p}

-- | With none of its register transactions in flight, none can be mined any more: each mined, or its nonce taken.
registering :: Worker -> UTCTime -> NameRegistration -> Commit -> IO Next
registering w@Worker {wkSender} now r@NameRegistration {nrId} c =
  inFlightTransaction wkSender (TxRegistration RTRegister nrId) >>= \case
    Just p -> registerSent w now r c p
    Nothing -> judge w now r c RTRegister Nothing

data Availability = AvailableAtLatest | TakenAtDepth | TakenOnlyAboveDepth

-- | A register or relay that did not go through, by its dry run's revert or once every transaction settled: registered when one mined
-- with status 1 or the owner holds the name; its reveal cleared on InvalidNonce, SignatureExpired or a refused terms acceptance, for the
-- page to sign again, the name policy read again on TermsNotAccepted; committing again with its reveal on CommitmentTooOld, which the
-- sponsor reverts only after the intent's and the allowance's checks, and on CommitmentNotFound; kept waiting while the sponsor allowance
-- is used up; taken on any revert while the name is not available at the confirmation depth. Otherwise, unless the dry run named the
-- name as gone, the attempt is stored and retried, and a permanent reason or a mined failure at its stage's cap fails the order;
-- committed for the page without a reveal.
judge :: Worker -> UTCTime -> NameRegistration -> Commit -> RegistrationTx -> Maybe RevertReason -> IO Next
judge w@Worker {wkOrders = oe@OrdersEnv {oeContracts, oeReport}} now r@NameRegistration {nrId = RegistrationId rid, nrReveal} c tx reason =
  inTurn w r $
    confirmLanded oe r >>= \case
      LRegistered v -> pure (Just v, Again)
      LEnded v -> pure (Just v, Again)
      LChanged -> pure (Nothing, Again)
      LUnread -> pure (Nothing, failure r SFUnread)
      LNotLanded -> case reason of
        Just (RRNamed RNInvalidNonce) -> clearReveal
        Just (RRNamed RNTermsMismatch) -> clearReveal
        Just (RRNamed RNTermsNotAccepted) -> forgetNamePolicy oeContracts >> clearReveal
        Just (RRNamed RNSignatureExpired) -> clearReveal
        Just (RRNamed RNCommitmentTooNew) -> pure (Nothing, waitUntil r $ addUTCTime chainSecond now)
        Just (RRNamed RNCommitmentTooOld)
          | unlapsed now c -> pure (Nothing, waitUntil r $ addUTCTime chainSecond now)
          | otherwise -> commitAgain True "its commitment lapsed"
        Just (RRNamed RNCommitmentNotFound) -> commitAgain False "its commitment is not on chain"
        Just (RRNamed RNInsufficientAllowance) -> failedAttempt w now r tx ASDryRun reason $ pure (Nothing, Stalled (nrVersion r) SFAllowanceUsedUp)
        Just (RRNamed RNNameNotAvailable) -> notRegistered True
        Just (RRNamed RNNameReserved) -> notRegistered True
        _ -> notRegistered False
  where
    clearReveal = store w now r {nrReveal = Nothing}
    commitAgain reported why = do
      let again = "names worker: registration " <> rid <> " is committed again: " <> why
      logWarn again
      when reported $ oeReport again
      store w now r {nrPhase = OQueued, nrHistory = noHistory}
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
      ORegistering {} -> failedAttempt w now r tx ASSent reason $ store w now r {nrPhase = OCommitted c}
      _ -> failedAttempt w now r tx ASDryRun reason $ pure (Nothing, Stalled (nrVersion r) $ SFRefused tx reason)
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
-- in the balance's turn after the name's; a code order keeps its code reserved.
taken :: Worker -> UTCTime -> NameRegistration -> IO (Maybe Int64, Next)
taken Worker {wkOrders = oe@OrdersEnv {oeReport}} now r@NameRegistration {nrId = RegistrationId rid} = do
  result@(v, _) <- written r <$> endOrder oe now r {nrPhase = OTaken now} (\_ -> pure True)
  forM_ v $ \_ -> do
    let lost = "names worker: registration " <> rid <> " was taken: its name is no longer available on chain"
    logError lost
    oeReport lost
  pure result

endOrder :: OrdersEnv -> UTCTime -> NameRegistration -> (DB.Connection -> IO Bool) -> IO (Either WriteRefusal Int64)
endOrder OrdersEnv {oeStore, oeTurns} now r@NameRegistration {nrId, nrPayment, nrPhase} mayEnd = case nrPayment of
  OPInvoice {} -> withBalanceTurn oeTurns nrId (withTransaction oeStore $ ending $ creditedWrite now r)
  OPCode _ | OTaken _ <- nrPhase -> withTransaction oeStore $ ending updated
  OPCode codeId -> fmap fst <$> releaseCode oeTurns oeStore codeId nrId (ending updated)
  where
    updated db = versionWritten <$> updateRegistration db r now
    ending write db =
      mayEnd db >>= \case
        True -> write db
        False -> pure $ Left WRChanged

creditedTaken :: UTCTime -> NameRegistration -> DB.Connection -> IO (Either WriteRefusal Int64)
creditedTaken now r = creditedWrite now r {nrPhase = OTaken now}

creditedWrite :: UTCTime -> NameRegistration -> DB.Connection -> IO (Either WriteRefusal Int64)
creditedWrite now r@NameRegistration {nrId, nrPrice} db = versionWritten <$> updateRegistration db r now >>= traverse (\v -> v <$ creditBalance db nrId nrPrice now)

failOrder :: Worker -> UTCTime -> NameRegistration -> FailReason -> Text -> IO (Maybe Int64, Next)
failOrder w@Worker {wkOrders = OrdersEnv {oeReport}} now r@NameRegistration {nrId = RegistrationId rid} reason why = do
  result@(v, _) <- store w now r {nrPhase = OFailed now reason}
  forM_ v $ \_ -> do
    let failed = "names worker: registration " <> rid <> " failed: " <> why
    logError failed
    oeReport failed
  pure result

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

data RetryRefusal = RFNotLapsed
  deriving (Eq, Show)

-- | /retry, in the caller's turn of the order's name: an order whose commitment lapsed unsigned is queued to be committed again,
-- keeping its payment and its code.
retryRegistration :: OrdersEnv -> NameRegistration -> IO Retried
retryRegistration OrdersEnv {oeStore, oeNow} r@NameRegistration {nrId = RegistrationId rid} = do
  now <- oeNow
  if not (lapsedAt now r)
    then pure $ RTRefused RFNotLapsed
    else
      withTransaction oeStore (\db -> versionWritten <$> updateRegistration db r {nrPhase = OQueued, nrHistory = noHistory} now) >>= \case
        Right v -> pure $ RTWritten v
        Left WRChanged -> pure RTChanged
        Left (WRRefused refusal) -> E.throwIO $ BrokenInvariant $ "registration " <> rid <> " is live while committed, yet its requeue was refused: " <> tshow refusal

failedAttempt :: Worker -> UTCTime -> NameRegistration -> RegistrationTx -> AttemptStage -> Maybe RevertReason -> IO (Maybe Int64, Next) -> IO (Maybe Int64, Next)
failedAttempt w@Worker {wkOrders = OrdersEnv {oeStore, oeConfig = NamesConfig {nMaxDryRunFailures, nMaxSentFailures}}} now r@NameRegistration {nrId, nrLastAttempt} tx stage reason underCap
  | counted = do
      failures <- withTransaction oeStore $ \db -> recordAttempt db nrId True attempt
      if failures >= cap
        then failOrder w now r FRRegistryRefused $ "its " <> textEncode tx <> " failed " <> tshow failures <> " times at its " <> stageText <> ", last: " <> why
        else underCap
  | otherwise = do
      when newReason $ void $ withTransaction oeStore $ \db -> recordAttempt db nrId False attempt
      underCap
  where
    counted = stage == ASSent || maybe False ((== FCPermanent) . revertClass) reason
    newReason = maybe True (\NameAttempt {naTx, naStage, naReason} -> (naTx, naStage, naReason) /= (tx, stage, why)) nrLastAttempt
    why = maybe "mined with status 0" reasonText reason
    attempt = NameAttempt {naTx = tx, naStage = stage, naReason = why, naAt = now}
    (cap, stageText) = case stage of
      ASDryRun -> (nMaxDryRunFailures, "dry run")
      ASSent -> (nMaxSentFailures, "sending")

reasonText :: RevertReason -> Text
reasonText = \case
  RRNamed n -> textEncode n
  RRUnnamed t -> t
  RRWithoutData -> "without data"

data Closed = CLRegistered RegistrationId | CLEnded RegistrationId | CLRefunded UTCTime RefundRecord | CLRefused CloseRefusal
  deriving (Eq, Show)

data CloseRefusal = CFNoOrder | CFNotFailedPaid | CFMonthEnded | CFUnread | CFChanged | CFBalanceChanged | CFReversed
  deriving (Eq, Show)

newtype CloseRefused = CloseRefused CloseRefusal
  deriving (Show)

instance E.Exception CloseRefused

closeRegistration :: DBStore -> Contracts -> Turns -> IO UTCTime -> Text -> IO Closed
closeRegistration st ct turns clock ref = do
  now <- clock
  withTransaction st (\db -> balanceByReference db ref $>>= getRegistration db . fst) >>= \case
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
        LEnded _ -> pure $ CLEnded nrId
        LChanged -> refused CFChanged
        LUnread -> refused CFUnread
        LNotLanded -> either CLRefused (CLRefunded endedAt) <$> refundTaken st turns now r {nrReveal = Nothing}

refundTaken :: DBStore -> Turns -> UTCTime -> NameRegistration -> IO (Either CloseRefusal RefundRecord)
refundTaken st turns now r@NameRegistration {nrId} =
  withBalanceTurn turns nrId $ either (\(CloseRefused refusal) -> Left refusal) id <$> E.try (withTransaction st refund)
  where
    refund db =
      reversalRecorded db nrId >>= \case
        True -> pure $ Left CFReversed
        False ->
          creditedTaken now r db >>= \case
            Left WRChanged -> pure $ Left CFChanged
            Left (WRRefused refusal) -> E.throwIO $ BrokenInvariant $ "the taken write of order " <> ridText <> " was refused as " <> tshow refusal <> ", while a write that closes an order is refused only as changed"
            Right _ -> Right <$> refundAll db
    refundAll db = do
      cents <- maybe 0 blAmount <$> getBalance db nrId
      refundBalance db nrId cents now >>= \case
        True -> RefundRecord nrId cents <$> balanceLink db nrId
        False -> E.throwIO $ CloseRefused CFBalanceChanged
    RegistrationId ridText = nrId

unregisteredAlertAge :: NominalDiffTime
unregisteredAlertAge = 3 * 3600

unregisteredCheckInterval :: Int
unregisteredCheckInterval = 300 * 1000000

alertUnregisteredSelected :: Worker -> IO ()
alertUnregisteredSelected Worker {wkOrders = OrdersEnv {oeStore, oeNow, oeReport}} = do
  now <- oeNow
  due <- withTransaction oeStore (`unregisteredSelectedBefore` addUTCTime (negate unregisteredAlertAge) now)
  forM_ due $ \(rid@(RegistrationId ridText), phase, selectedAt) -> do
    claimed <- withTransaction oeStore $ \db -> claimUnregisteredAlert db rid now
    when claimed $ do
      let alert = "names: order " <> ridText <> " had its name selected at " <> tshow selectedAt <> " and is " <> textEncode phase <> ", not registered, 3 hours later"
      logError alert
      oeReport alert

runUnregisteredAlerts :: Worker -> IO ()
runUnregisteredAlerts w = forever $ (alertUnregisteredSelected w `catchOwn'` \e -> logError ("names: the check of orders whose name is not registered 3 hours after its selection failed; the next pass will run: " <> tshow e)) >> threadDelay unregisteredCheckInterval

shortestMonth :: NominalDiffTime
shortestMonth = 28 * 86400

monthCheckInterval :: Int
monthCheckInterval = 300 * 1000000

endMonths :: Worker -> IO ()
endMonths Worker {wkOrders = oe@OrdersEnv {oeStore, oeTurns, oePhases, oeNow, oeReport}} = do
  now <- oeNow
  due <- withTransaction oeStore (`monthCandidates` addUTCTime (negate shortestMonth) now)
  forM_ [rid | (rid, selectedAt) <- due, addMonths 1 selectedAt <= now] $ \rid@(RegistrationId ridText) ->
    ( withTransaction oeStore (`getRegistration` rid)
        >>= traverse (\r -> withRegistrationTurn oeStore oeTurns r (monthEnded now))
        >>= \case
          Just (Just (Right (Right (v, kept)))) -> do
            atomically $ publish oePhases rid v
            let failed = "names: order " <> ridText <> " was not registered within a month of its name's selection, so it failed, " <> kept
            logError failed
            oeReport failed
          _ -> pure ()
    )
      `catchOwn'` \e -> logError ("names month pass: registration " <> ridText <> " failed; the next pass tries it again: " <> tshow e)
  where
    monthEnded now r@NameRegistration {nrId, nrPayment, nrSelectedAt}
      | orderLive r && namePhase (nrPhase r) `elem` [PQueued, PCommitted, PFailed] && maybe False ((<= now) . addMonths 1) nrSelectedAt = fmap (,kept) <$> endOrder oe now r {nrPhase = OFailed now FRMonthEnded} registerUnsent
      | otherwise = pure $ Left WRChanged
      where
        registerUnsent db =
          lockRegistration db r >>= \case
            True -> not <$> registerTxPending db nrId
            False -> pure False
        kept = case nrPayment of
          OPCode _ -> "its code released"
          _ -> "the money paid for it added to its balance"

runMonthEnds :: TVar UTCTime -> Worker -> IO ()
runMonthEnds passAt w@Worker {wkOrders = OrdersEnv {oeNow}} =
  forever $ (monthPass `catchOwn'` \e -> logError ("names month pass failed; the next pass will run: " <> tshow e)) >> threadDelay monthCheckInterval
  where
    monthPass = do
      started <- oeNow
      endMonths w
      atomically $ writeTVar passAt started

disputeReadMargin :: NominalDiffTime
disputeReadMargin = 7 * 86400

endedSweepInterval :: Int
endedSweepInterval = 3600 * 1000000

sweepEndedRegistrations :: Worker -> IO Bool
sweepEndedRegistrations w@Worker {wkOrders = OrdersEnv {oeStore, oeContracts, oeNow}} = do
  now <- oeNow
  disputable <- withTransaction oeStore (`disputableRegistrations` secondsAt (addUTCTime (negate disputeReadMargin) now))
  if null disputable
    then pure True
    else
      confirmedBlock oeContracts >>= \case
        Left e -> False <$ logWarn ("names ended sweep: a chain read failed, the pass is stopped: " <> tshow e)
        Right confirmed -> True <$ (mapM (\rid -> first (rid,) <$> endIfUnregistered now confirmed rid) disputable >>= mapM_ unread . take 1 . lefts)
  where
    secondsAt = RoundedSystemTime . floor . utcTimeToPOSIXSeconds
    unread (RegistrationId ridText, e) = logWarn $ "names ended sweep: registration " <> ridText <> ": a chain read failed, read again at the next pass: " <> tshow e
    endIfUnregistered now Block {blockNumber, blockTimestamp} rid@(RegistrationId ridText) =
      ( withTransaction oeStore (`getRegistration` rid) >>= \case
          Just r@NameRegistration {nrPhase = ORegistered expiry (Just disputeEnd) _, nrLabelHash, nrDisputeDays}
            | roundedSeconds disputeEnd < roundedSeconds blockTimestamp + fromIntegral (disputeSeconds nrDisputeDays) ->
                disputeUntil oeContracts (AtBlock blockNumber) nrLabelHash >>= traverse (\current -> when (current /= Just disputeEnd) $ void $ inTurn w r $ do
                  result@(v, _) <- store w now r {nrPhase = OEnded expiry disputeEnd}
                  forM_ v $ \_ -> logNote $ "names ended sweep: registration " <> ridText <> " set ended: disputeUntil(id) read at block " <> tshow blockNumber <> " is " <> maybe "0" (tshow . roundedSeconds) current <> ", not " <> tshow (roundedSeconds disputeEnd)
                  pure result)
          _ -> pure $ Right ()
      )
        `catchOwn'` \e -> Right () <$ logError ("names ended sweep: registration " <> ridText <> " failed; the next pass tries it again: " <> tshow e)

runEndedSweep :: TVar UTCTime -> Worker -> IO ()
runEndedSweep passAt w@Worker {wkOrders = OrdersEnv {oeNow}} =
  forever $ (endedPass `catchOwn'` \e -> logError ("names ended sweep failed; the next pass will run: " <> tshow e)) >> threadDelay endedSweepInterval
  where
    endedPass = do
      started <- oeNow
      completed <- sweepEndedRegistrations w
      when completed $ atomically $ writeTVar passAt started
