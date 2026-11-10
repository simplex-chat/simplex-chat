{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Each registration from queued to registered, taken or failed: its commit, the wait for the page's reveal, its acceptance relay, and its register;
-- and Try again, which queues a failed one again. A step starts from the stored registration, the sender's transactions in flight
-- and the chain, so a restart resumes any phase.
module StoreService.Names.Worker
  ( Worker,
    Next (..),
    newWorker,
    stepRegistration,
    runWorkers,
    sweepFailedCodes,
    runCodeSweep,
    pauseUncovered,
    runProbe,
    Retried (..),
    RetryRefusal (..),
    retryRegistration,
    maxSendsPerKind,
    sendsUsedUp,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, poll)
import Control.Concurrent.STM
import Control.Exception (finally, mask_)
import Control.Logger.Simple (logError, logNote, logWarn)
import Control.Monad (forM, forM_, forever, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.ByteString (ByteString)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Util (tshow, ($>>=))
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Names.Abi (hex)
import StoreService.Names.Chain (Block (..), BlockTag (..), ChainClient, ChainError (..), TxHash, blockAt)
import StoreService.Names.Codes (CodeRefusal (..), codeRefusal, releaseCode, reserveCode)
import StoreService.Names.Contracts (CommitmentAges (..), acceptedTerms, chainPriceCents, commitmentAges, commitmentTime, communityName, concurrent, confirmedTag, controllerAvailable, forgetNamePolicy, namePolicy, registerCost, registrarAllowance, sponsorNonce, together)
import StoreService.Names.Env (Pause (..), PauseReason (..))
import StoreService.Names.Orders (Landing (..), OrdersEnv (..), WriteRefusal (..), confirmLanded, unixSeconds, versionWritten)
import StoreService.Names.Phases (RegistrationId (..), awaitChange, publish)
import StoreService.Names.Registration (TermsAcceptance (..), acceptCalldata, commitCalldata, policyDomain, registerWithCreditCalldata, sponsorDomain, termsKind, verifyAcceptance, verifyRegisterIntent)
import StoreService.Names.Reveal (intentOf)
import StoreService.Names.Revert (RevertName (..))
import StoreService.Names.Sender (PendingTx, SendError (..), Sender, awaitOutcome, feesWithinCap, inFlightTransaction, keyUnderfunded, ptHash, replacementOverCap, submitTransaction)
import StoreService.Names.Store
import StoreService.Names.Turns (withBalanceTurn, withLabelTurn)
import StoreService.Names.View (lapseMoment, notSignedInTime, registrationAt, registrationVersion)
import StoreService.Safely (safely, safelyWith)

data Worker = Worker
  { wkOrders :: OrdersEnv,
    wkChain :: ChainClient,
    wkSender :: Sender,
    -- | the thread following each registration, kept by the scan
    wkThreads :: TVar (Map RegistrationId (Async ()))
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
    Retry Int64 Text
  | -- | as Retry, for a cause only the operator removes, logged as an error
    Stalled Int64 Text
  | -- | a transaction of the registration is in flight
    Await PendingTx
  | Paused

-- | How often the stored registrations are scanned for one without a thread, such as an order just paid or requeued.
scanInterval :: Int
scanInterval = 5000000

retryInterval :: NominalDiffTime
retryInterval = 15

retryMicros :: Int
retryMicros = ceiling $ retryInterval * 1000000

-- | The registration is read again after this wait for its transaction's outcome.
outcomeWait :: Int
outcomeWait = 60000000

-- | A long wait is cut into waits of at most this, so a clock adjustment delays a moment by no more.
maxWait :: NominalDiffTime
maxWait = 60

-- | Block times are whole seconds.
chainSecond :: NominalDiffTime
chainSecond = 1

-- | A failed code order keeps its code this long for Try again.
failedCodeHold :: NominalDiffTime
failedCodeHold = 30 * 86400

sweepInterval :: Int
sweepInterval = 86400 * 1000000

notRegistered :: Text
notRegistered = "The name could not be registered"

newWorker :: OrdersEnv -> ChainClient -> Sender -> IO Worker
newWorker wkOrders wkChain wkSender = Worker wkOrders wkChain wkSender <$> newTVarIO M.empty

-- | A thread for each registration in the worker's phases, started by a scan every scanInterval; they stop with it.
runWorkers :: Worker -> IO ()
runWorkers w@Worker {wkThreads} =
  forever (safely "names worker: the scan" (startWorkers w) >> threadDelay scanInterval)
    `finally` (readTVarIO wkThreads >>= mapM_ cancel)

startWorkers :: Worker -> IO ()
startWorkers w@Worker {wkOrders = OrdersEnv {oeStore}, wkThreads} = do
  running <- readTVarIO wkThreads >>= M.traverseMaybeWithKey (\_ a -> maybe (Just a) (const Nothing) <$> poll a)
  atomically $ writeTVar wkThreads running
  active <- withTransaction oeStore activeRegistrations
  forM_ (filter (`M.notMember` running) active) $ \rid ->
    mask_ $ asyncWithUnmask (\unmask -> unmask $ work w rid) >>= atomically . modifyTVar' wkThreads . M.insert rid

work :: Worker -> RegistrationId -> IO ()
work w rid@(RegistrationId ridText) = void $ safely ("names worker: registration " <> ridText) $ follow w rid Nothing

-- | Steps until the registration leaves the worker's phases, logging a lasting failure once.
follow :: Worker -> RegistrationId -> Maybe Text -> IO ()
follow w@Worker {wkOrders = OrdersEnv {oeStore, oePhases, oePause, oeNow}} rid@(RegistrationId ridText) lastFailure =
  stepRegistration w rid >>= \case
    Done -> pure ()
    Again -> follow w rid Nothing
    WaitUntil v t -> waitFor v t >> follow w rid Nothing
    Retry v reason -> waitAgain logWarn v reason
    Stalled v reason -> waitAgain logError v reason
    Await p -> awaitOutcome outcomeWait p >> follow w rid Nothing
    Paused -> do
      retried <- registerDelay retryMicros
      atomically $ (readTVar oePause >>= check . isNothing) `orElse` (readTVar retried >>= check)
      follow w rid Nothing
  where
    waitAgain logAt v reason = do
      when (Just reason /= lastFailure) $ logAt $ "names worker: registration " <> ridText <> " waits: " <> reason
      now <- oeNow
      waitFor v $ addUTCTime retryInterval now
      follow w rid $ Just reason
    waitFor v t = do
      now <- oeNow
      let micros = max 0 $ ceiling $ min maxWait (diffUTCTime t now) * 1000000
      void $ awaitChange oePhases rid (registrationVersion oeStore rid) v micros

stepRegistration :: Worker -> RegistrationId -> IO Next
stepRegistration w@Worker {wkOrders = OrdersEnv {oeStore, oeNow}} rid = do
  now <- oeNow
  withTransaction oeStore (`getRegistration` rid) >>= \case
    Nothing -> pure Done
    Just r -> case nrPhase r of
      PQueued -> queued w now r
      PCommitting -> commit w now r
      PCommitted -> committed w now r
      PRegistering -> registering w now r
      _ -> pure Done

queued :: Worker -> UTCTime -> NameRegistration -> IO Next
queued w@Worker {wkOrders = OrdersEnv {oeStore}} now r@NameRegistration {nrId, nrCommitment} =
  withTransaction oeStore activeOrders >>= covered w . throughOrder nrId >>= \case
    Right True -> commit w now r
    Right False -> Paused <$ pause w now PRAllowance (Just nrCommitment)
    Left e -> pure $ failure r e

-- | A commit of the registration in flight is waited for; otherwise commitments(commitment) decides.
commit :: Worker -> UTCTime -> NameRegistration -> IO Next
commit w@Worker {wkOrders = OrdersEnv {oeContracts}, wkSender} now r@NameRegistration {nrId, nrCommitment} =
  inFlightTransaction wkSender (TxCommit nrId) >>= \case
    Just p -> commitSent w now r p
    Nothing ->
      runExceptT ((,) <$> ExceptT (commitmentAges oeContracts) <*> ExceptT (commitmentTime oeContracts nrCommitment)) >>= \case
        Left e -> pure $ failure r e
        Right (_, Nothing) -> sendCommit w now r
        Right (ages, Just t) -> fromCommitment w now r ages t

-- | At the lapse second a commitment can neither register nor be committed again, so the worker waits a second past it,
-- then commits again with the stale fields cleared.
fromCommitment :: Worker -> UTCTime -> NameRegistration -> CommitmentAges -> Word64 -> IO Next
fromCommitment w@Worker {wkOrders = OrdersEnv {oeConfig}} now r ages t
  | reusable oeConfig now lapse = toCommitted w now r ages t
  | lapse >= unixSeconds now = pure $ waitUntil r $ addUTCTime chainSecond $ secondsTime lapse
  | lapsedFields r /= r = save w now $ lapsedFields r
  | otherwise = sendCommit w now r
  where
    lapse = commitmentLapse ages t

-- | The revealBefore of a commitment committed at this time.
commitmentLapse :: CommitmentAges -> Word64 -> Integer
commitmentLapse CommitmentAges {caMax} t = toInteger t + caMax

-- | A live commitment is reused while revealMargin is left before its lapse, time for the page's reveal and the register to mine.
reusable :: NamesConfig -> UTCTime -> Integer -> Bool
reusable NamesConfig {nRevealMarginSeconds} now lapse = lapse - unixSeconds now >= toInteger nRevealMarginSeconds

-- | The intent's deadline is the lapsed revealBefore, so it goes with the old transactions and reveal times; the page signs again.
lapsedFields :: NameRegistration -> NameRegistration
lapsedFields r =
  r
    { nrReveal = Nothing,
      nrError = Nothing,
      nrCommitTx = Nothing,
      nrCommitBlock = Nothing,
      nrRegisterTx = Nothing,
      nrRegisterBlock = Nothing,
      nrRevealAfter = Nothing,
      nrRevealBefore = Nothing
    }

-- | The reveal times come from the commitment's time. When that is not the block of our commit mined with status 1,
-- someone committed it again after a lapse, so our commit and the reveal signed for its window are cleared.
toCommitted :: Worker -> UTCTime -> NameRegistration -> CommitmentAges -> Word64 -> IO Next
toCommitted w now r CommitmentAges {caMin, caMax} t =
  ourCommit w r t >>= \case
    Left e -> pure $ failure r e
    Right ours -> save w now (minedBy ours) {nrPhase = PCommitted, nrRevealAfter = Just $ after caMin, nrRevealBefore = Just $ after caMax}
  where
    after age = fromInteger $ toInteger t + age
    minedBy = \case
      Just (h, block) -> r {nrCommitTx = Just h, nrCommitBlock = Just block}
      Nothing -> r {nrReveal = Nothing, nrCommitTx = Nothing, nrCommitBlock = Nothing}

-- | Among the signatures of the commit last sent, the one mined with status 1 at the commitment's time.
ourCommit :: Worker -> NameRegistration -> Word64 -> IO (Either ChainError (Maybe (TxHash, Word64)))
ourCommit Worker {wkOrders = OrdersEnv {oeStore}, wkChain} NameRegistration {nrId, nrCommitTx} t = do
  txs <- withTransaction oeStore (`registrationTransactions` nrId)
  let commits = [tx | tx@NameTransaction {ntFor = TxCommit _} <- txs]
      lastSent = [ntNonce | NameTransaction {ntHash, ntNonce} <- commits, Just ntHash == nrCommitTx]
  case [(ntHash, block) | NameTransaction {ntHash, ntNonce, ntStatus = TSMined, ntMinedBlock = Just block} <- commits, ntNonce `elem` lastSent] of
    (h, block) : _ -> fmap (\Block {blockTimestamp} -> if blockTimestamp == t then Just (h, block) else Nothing) <$> blockAt wkChain block
    [] -> pure $ Right Nothing

sendCommit :: Worker -> UTCTime -> NameRegistration -> IO Next
sendCommit w@Worker {wkOrders = OrdersEnv {oeConfig}, wkSender} now r@NameRegistration {nrId, nrCommitment} =
  withinSends w now r isCommit $ submitTransaction wkSender (TxCommit nrId) (cController $ nChain oeConfig) (commitCalldata nrCommitment) >>= \case
    Right p -> commitSent w now r p
    Left SEInFlight -> pure Again
    Left e@SEFeeAboveCap {} -> pauseAtFeeCap w now r e
    -- UnexpiredCommitmentExists among them: a commit mined since the read, or a latest block still at the lapse second
    Left e -> pure $ failure r e

-- | Recorded with the newest signature's hash, so a fee-bumped replacement is the one the view shows.
commitSent :: Worker -> UTCTime -> NameRegistration -> PendingTx -> IO Next
commitSent w now r@NameRegistration {nrPhase, nrCommitTx} p
  | nrPhase == PCommitting && nrCommitTx == Just (ptHash p) = pure $ Await p
  | otherwise = save w now r {nrPhase = PCommitting, nrCommitTx = Just $ ptHash p, nrCommitBlock = Nothing}

-- | Waits for the page's reveal until the lapse, has its acceptance held by the name policy, waits for revealAfter, then registers.
committed :: Worker -> UTCTime -> NameRegistration -> IO Next
committed w@Worker {wkSender} now r@NameRegistration {nrId, nrReveal, nrRevealAfter, nrRevealBefore} =
  inFlightTransaction wkSender (TxRegister nrId) >>= \case
    Just p -> registerSent w now r p
    Nothing -> case nrReveal of
      Nothing -> case lapseMoment r of
        Just lapse | now <= lapse -> pure $ waitUntil r $ addUTCTime chainSecond lapse
        _ -> inTurn w r $ failed w now r notSignedInTime "its buyer did not sign it in time"
      -- past the lapse nothing is sent: the policy would commit again, and the sponsor checks its allowance before the commitment
      Just _ | lapsed -> judge w now r Nothing
      Just rv@NameReveal {rvAcceptance, rvDeadline} ->
        (pure rvAcceptance $>>= relayAcceptance w now r rvDeadline) >>= \case
          Just next -> pure next
          Nothing -> case secondsTime . toInteger <$> nrRevealAfter of
            Just revealAfter | now < revealAfter -> pure $ waitUntil r revealAfter
            _ -> sendRegister w now r rv
  where
    lapsed = any ((now >) . secondsTime . toInteger) nrRevealBefore

-- | The relay of the reveal's acceptance to the name policy, sent without waiting for revealAfter; Nothing once the policy
-- stores that acceptance. One signed for another policy, before a setNamePolicy, or for the other kind of terms, clears the
-- reveal for the page to sign again; the relay's dry run's revert is judged as a register's is, unless the policy holds the
-- acceptance, which another sent first.
relayAcceptance :: Worker -> UTCTime -> NameRegistration -> Word64 -> TermsAcceptance -> IO (Maybe Next)
relayAcceptance w@Worker {wkOrders = OrdersEnv {oeContracts, oeConfig = NamesConfig {nChain = ChainConfig {cChainId}}}, wkSender} now r@NameRegistration {nrId = regId@(RegistrationId rid), nrOwner, nrCommitment, nrLabelHash} deadline a@TermsAcceptance {taTermsHash} =
  inFlightTransaction wkSender (TxAccept regId) >>= \case
    Just p -> pure $ Just $ Await p
    Nothing ->
      runExceptT ((,,) <$> ExceptT (namePolicy oeContracts) <*> ExceptT (communityName oeContracts nrLabelHash) <*> ExceptT held) >>= \case
        Left e -> pure $ Just $ failure r e
        Right (policy, listed, accepted)
          | accepted == taTermsHash -> pure Nothing
          | otherwise -> Just <$> withinSends w now r isAccept (relay policy (termsKind listed))
  where
    held = acceptedTerms oeContracts nrCommitment nrOwner
    relay policy kind =
      verifyAcceptance (policyDomain cChainId policy) kind nrOwner nrCommitment deadline a >>= \case
        Left refusal -> do
          logWarn $ "names worker: registration " <> rid <> ": its stored acceptance is refused, " <> T.pack refusal
          save w now r {nrReveal = Nothing}
        Right () ->
          submitTransaction wkSender (TxAccept regId) policy (acceptCalldata kind nrCommitment nrOwner deadline a) >>= \case
            Right p -> pure $ Await p
            Left SEInFlight -> pure Again
            Left (SEChain (ChainReverted reason)) ->
              held >>= \case
                Right accepted | accepted == taTermsHash -> pure Again
                _ -> judge w now r reason
            Left e@SEFeeAboveCap {} -> pauseAtFeeCap w now r e
            Left e -> pure $ failure r e

-- | The stored intent is checked before its dry run, whose revert decides at once; no label turn is held across the send.
sendRegister :: Worker -> UTCTime -> NameRegistration -> NameReveal -> IO Next
sendRegister w@Worker {wkOrders = OrdersEnv {oeConfig = cfg@NamesConfig {nChain = ChainConfig {cChainId, cSponsor}}}, wkSender} now r@NameRegistration {nrId = regId@(RegistrationId rid)} rv =
  verifyRegisterIntent (sponsorDomain cChainId cSponsor) (fromInteger $ unixSeconds now) intent >>= \case
    -- past its deadline the commitment has lapsed too; a wrong signer follows a change of the chain settings since the reveal
    Left refusal -> do
      logWarn $ "names worker: registration " <> rid <> ": its stored intent is refused, " <> T.pack refusal
      judge w now r Nothing
    Right () ->
      withinSends w now r isRegister $ submitTransaction wkSender (TxRegister regId) cSponsor (registerWithCreditCalldata intent) >>= \case
        Right p -> registerSent w now r p
        Left SEInFlight -> pure Again
        Left (SEChain (ChainReverted reason)) -> judge w now r reason
        Left e@SEFeeAboveCap {} -> pauseAtFeeCap w now r e
        Left e -> pure $ failure r e
  where
    intent = intentOf cfg r rv

pauseAtFeeCap :: Worker -> UTCTime -> NameRegistration -> SendError -> IO Next
pauseAtFeeCap w now r e = failure r e <$ pause w now PRFeeCap Nothing

registerSent :: Worker -> UTCTime -> NameRegistration -> PendingTx -> IO Next
registerSent w now r@NameRegistration {nrPhase, nrRegisterTx} p
  | nrPhase == PRegistering && nrRegisterTx == Just (ptHash p) = pure $ Await p
  | otherwise = save w now r {nrPhase = PRegistering, nrRegisterTx = Just $ ptHash p, nrRegisterBlock = Nothing}

-- | With none of its register transactions in flight, none can be mined any more: each mined, or its nonce taken.
registering :: Worker -> UTCTime -> NameRegistration -> IO Next
registering w@Worker {wkSender} now r@NameRegistration {nrId} =
  inFlightTransaction wkSender (TxRegister nrId) >>= \case
    Just p -> registerSent w now r p
    Nothing -> judge w now r Nothing

-- | A register that did not go through, by its dry run's revert or once every transaction settled: registered when one mined
-- with status 1 or the owner holds the name; committed again on InvalidNonce or a refused terms acceptance, for the page to sign again, the name
-- policy read again on TermsNotAccepted; kept waiting, registration paused, while the sponsor allowance is used up; taken on any revert while the name is not
-- available at the confirmation depth; committed again after a register mined with status 0, for the next dry run to find the cause;
-- failed otherwise, unless the dry run named the name as gone.
judge :: Worker -> UTCTime -> NameRegistration -> Maybe RevertName -> IO Next
judge w@Worker {wkOrders = oe@OrdersEnv {oeContracts}} now r@NameRegistration {nrPhase, nrCommitment, nrReveal} reason =
  inTurn w r $
    confirmLanded oe r >>= \case
      LRegistered v -> pure (Just v, Again)
      LChanged -> pure (Nothing, Again)
      LUnread -> pure (Nothing, Retry (nrVersion r) "the register check's chain read failed")
      LNotLanded -> case reason of
        Just e | e `elem` [RNInvalidNonce, RNTermsNotAccepted, RNTermsMismatch] -> do
          when (e == RNTermsNotAccepted) $ forgetNamePolicy oeContracts
          store w now r {nrReveal = Nothing}
        Just RNCommitmentTooNew -> pure (Nothing, waitUntil r $ addUTCTime chainSecond now)
        Just RNInsufficientAllowance -> do
          pause w now PRAllowance (Just nrCommitment)
          pure (Nothing, Stalled (nrVersion r) "the registrar's allowance on the sponsor is used up; the guardian raises it with setRegistrarAllowance")
        _ -> case nrReveal of
          Just NameReveal {rvLabel} ->
            availability rvLabel >>= \case
              Left e -> pure (Nothing, failure r e)
              Right (False, False) -> taken w now r
              Right (False, True) -> pure (Nothing, Retry (nrVersion r) "the name reads taken only above the confirmation depth, where a reorg can undo it")
              -- the dry run runs on the pending block, where a competitor's register that may never mine can hold the name
              Right (True, _)
                | reason `elem` map Just [RNNameNotAvailable, RNNameReserved] -> pure (Nothing, Retry (nrVersion r) "the dry run reverted the name as gone, but it reads available")
                | isNothing reason && nrPhase == PRegistering -> store w now r {nrPhase = PCommitted, nrRegisterTx = Nothing, nrRegisterBlock = Nothing}
                | otherwise -> registerFailed
          Nothing -> registerFailed
  where
    registerFailed = failed w now r notRegistered "its register did not go through"
    -- at the latest block, and at the confirmation depth while the latest reads it taken
    availability label = runExceptT $ do
      latest <- ExceptT $ controllerAvailable oeContracts Latest label
      if latest then pure (True, True) else (False,) <$> (ExceptT (confirmedTag oeContracts) >>= \tag -> ExceptT (controllerAvailable oeContracts tag label))

-- | A paid order keeps its whole price as a balance of its own, credited in the taken write's transaction, registration before balance,
-- in the balance's turn after the name's; a code order's code is released.
taken :: Worker -> UTCTime -> NameRegistration -> IO (Maybe Int64, Next)
taken Worker {wkOrders = OrdersEnv {oeStore, oeTurns}} now r@NameRegistration {nrId, nrPayment, nrPrice} = case nrPayment of
  RPInvoice {} -> credited
  RPBalance -> credited
  RPCode codeId -> written r . fmap fst <$> releaseCode oeTurns oeStore codeId nrId write
  where
    write db = versionWritten <$> updateRegistration db r {nrPhase = PTaken, nrError = Nothing} now
    credited = written r <$> withBalanceTurn oeTurns nrId (withTransaction oeStore $ \db -> write db >>= traverse (\v -> v <$ creditBalance db nrId nrPrice now))

-- | A code order keeps its code for Try again.
failed :: Worker -> UTCTime -> NameRegistration -> Text -> Text -> IO (Maybe Int64, Next)
failed w now r@NameRegistration {nrCommitment} shown cause = do
  result@(v, _) <- store w now r {nrPhase = PFailed, nrFailedAt = Just now, nrError = Just shown}
  when (isJust v) $ logError $ "names worker: the order with commitment " <> hex nrCommitment <> " failed: " <> cause
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
  Left (WRRefused refusal) -> (Nothing, Retry (nrVersion r) $ "its write was refused: " <> tshow refusal)

failure :: Show e => NameRegistration -> e -> Next
failure r e = Retry (nrVersion r) $ tshow e

waitUntil :: NameRegistration -> UTCTime -> Next
waitUntil r = WaitUntil (nrVersion r)

secondsTime :: Integer -> UTCTime
secondsTime = posixSecondsToUTCTime . fromInteger

-- | What Try again did: the version it wrote, the order changed since it was read, or a refusal.
data Retried = RTWritten Int64 | RTChanged | RTRefused RetryRefusal

data RetryRefusal = RFNotFailed | RFLinksRefused | RFCode CodeRefusal | RFUnread | RFSendsUsedUp
  deriving (Eq, Show)

-- | Try again, in the caller's turn of the order's name: registered when its register went through; else a code order's code
-- must still pay for it; else queued again at no charge, a released code reserved again in the code's turn.
retryRegistration :: OrdersEnv -> NameRegistration -> IO Retried
retryRegistration oe@OrdersEnv {oeNow} current = do
  now <- oeNow
  case registrationAt now current of
    -- its commitment fixes the refused links, so only another name can register
    NameRegistration {nrPhase = PFailed, nrLinksRefusedAt = Just _} -> pure $ RTRefused RFLinksRefused
    r@NameRegistration {nrPhase = PFailed} ->
      confirmLanded oe r >>= \case
        LRegistered v -> pure $ RTWritten v
        LChanged -> pure RTChanged
        LUnread -> pure $ RTRefused RFUnread
        LNotLanded ->
          sendsUsedUp oe r >>= \case
            True -> pure $ RTRefused RFSendsUsedUp
            False -> retryCode oe now r >>= either (pure . RTRefused . RFCode) (requeue oe now r)
    _ -> pure $ RTRefused RFNotFailed

-- | An owner who changes their terms acceptance on chain makes every relay and register revert at the service's gas, so each
-- kind of transaction is sent at most this many times for one order, its moves included.
maxSendsPerKind :: Int
maxSendsPerKind = 5

-- | The send while the order has sent fewer than maxSendsPerKind transactions of its kind, else the order fails.
withinSends :: Worker -> UTCTime -> NameRegistration -> (TxFor -> Bool) -> IO Next -> IO Next
withinSends w@Worker {wkOrders} now r ofKind send = do
  sent <- sendsOf wkOrders r ofKind
  if sent < maxSendsPerKind
    then send
    else inTurn w r $ failed w now r notRegistered $ "it sent " <> tshow sent <> " transactions of one kind, the most an order may"

sendsUsedUp :: OrdersEnv -> NameRegistration -> IO Bool
sendsUsedUp oe r = any (>= maxSendsPerKind) <$> mapM (sendsOf oe r) [isCommit, isAccept, isRegister]

-- | One per nonce, as a fee replacement is the same send.
sendsOf :: OrdersEnv -> NameRegistration -> (TxFor -> Bool) -> IO Int
sendsOf OrdersEnv {oeStore} NameRegistration {nrId} ofKind =
  length . nubOrd . map ntNonce . filter (ofKind . ntFor) <$> withTransaction oeStore (`registrationTransactions` nrId)

isCommit, isAccept, isRegister :: TxFor -> Bool
isCommit = \case TxCommit _ -> True; _ -> False
isAccept = \case TxAccept _ -> True; _ -> False
isRegister = \case TxRegister _ -> True; _ -> False

-- | The code a code order still holds, though revoked or expired since, or a released one it can reserve again.
retryCode :: OrdersEnv -> UTCTime -> NameRegistration -> IO (Either CodeRefusal (Maybe PrepaidCode))
retryCode OrdersEnv {oeStore} now NameRegistration {nrId, nrLength, nrPayment} = case nrPayment of
  RPInvoice {} -> pure $ Right Nothing
  RPBalance -> pure $ Right Nothing
  RPCode codeId ->
    withTransaction oeStore (`getPrepaidCodeById` codeId) <&> \case
      Just code -> maybe (Right $ Just code) Left $ codeRefusal now nrId nrLength code
      Nothing -> Left CRInvalid

-- | The register's transactions, which confirmLanded has read, are cleared with the failure; the reveal unless its intent still serves.
requeue :: OrdersEnv -> UTCTime -> NameRegistration -> Maybe PrepaidCode -> IO Retried
requeue oe@OrdersEnv {oeStore, oeTurns} now r@NameRegistration {nrId, nrLength} code =
  servingReveal oe now r >>= \case
    Left e -> RTRefused RFUnread <$ logWarn ("names: a retry's chain read failed: " <> tshow e)
    Right reveal -> do
      let requeued = r {nrPhase = PQueued, nrReveal = reveal, nrFailedAt = Nothing, nrError = Nothing, nrRegisterTx = Nothing, nrRegisterBlock = Nothing}
          write db = versionWritten <$> updateRegistration db requeued now
      outcome <- case code of
        Nothing -> Right <$> withTransaction oeStore write
        Just PrepaidCode {pcCodeHash} -> reserveCode oeTurns oeStore now pcCodeHash nrId nrLength write
      pure $ case outcome of
        Left refusal -> RTRefused $ RFCode refusal
        Right (Right v) -> RTWritten v
        -- a failed order is already live, so only a changed version refuses the write
        Right (Left _) -> RTChanged

-- | The stored reveal while its intent can still register: accepted by the worker's check before sending, the sponsor nonce unmoved,
-- and the commitment live with revealMargin left in a window the intent's deadline covers. A new commit or another's re-commit
-- opens a window the old deadline cannot serve.
servingReveal :: OrdersEnv -> UTCTime -> NameRegistration -> IO (Either ChainError (Maybe NameReveal))
servingReveal OrdersEnv {oeConfig = cfg@NamesConfig {nChain = ChainConfig {cChainId, cSponsor}}, oeContracts} now r@NameRegistration {nrOwner, nrCommitment, nrReveal} = case nrReveal of
  Nothing -> pure $ Right Nothing
  Just rv@NameReveal {rvNonce, rvDeadline} ->
    verifyRegisterIntent (sponsorDomain cChainId cSponsor) (fromInteger $ unixSeconds now) (intentOf cfg r rv) >>= \case
      Left _ -> pure $ Right Nothing
      Right () -> runExceptT $ do
        nonce <- ExceptT $ sponsorNonce oeContracts nrOwner
        if nonce /= rvNonce
          then pure Nothing
          else do
            ages <- ExceptT $ commitmentAges oeContracts
            committedAt <- ExceptT $ commitmentTime oeContracts nrCommitment
            pure $ case commitmentLapse ages <$> committedAt of
              Just lapse | reusable cfg now lapse && lapse <= toInteger rvDeadline -> Just rv
              _ -> Nothing

-- | Each code order failed for failedCodeHold gives its code back, in its name's and its code's turns, unless it was requeued meanwhile.
sweepFailedCodes :: Worker -> IO ()
sweepFailedCodes Worker {wkOrders = OrdersEnv {oeStore, oeTurns, oeNow}} = do
  cutoff <- addUTCTime (negate failedCodeHold) <$> oeNow
  orders <- withTransaction oeStore (`failedCodeOrdersBefore` cutoff)
  released <- forM orders $ \(rid@(RegistrationId ridText), codeId) ->
    safelyWith ("names code sweep: registration " <> ridText) False $
      withTransaction oeStore (`getRegistration` rid) >>= \case
        Just NameRegistration {nrLabelHash} -> withLabelTurn oeTurns nrLabelHash $ either (const False) snd <$> releaseCode oeTurns oeStore codeId rid (stillFailed cutoff rid)
        Nothing -> pure False
  let count = length $ filter id released
  when (count > 0) $ logNote $ "names code sweep: released the codes of " <> tshow count <> " failed order(s)"
  where
    stillFailed cutoff rid db =
      getRegistration db rid <&> \case
        Just NameRegistration {nrPhase = PFailed, nrFailedAt = Just failedAt} | failedAt < cutoff -> Right ()
        _ -> Left ()

runCodeSweep :: Worker -> IO ()
runCodeSweep w = forever $ safely "names code sweep" (sweepFailedCodes w) >> threadDelay sweepInterval

pauseUncovered :: Worker -> IO ()
pauseUncovered w@Worker {wkOrders = OrdersEnv {oeStore, oeNow}} = do
  orders <- withTransaction oeStore activeOrders
  forM_ (lastMaybe orders) $ \ActiveOrder {aoCommitment} ->
    covered w orders >>= \case
      Right True -> pure ()
      _ -> oeNow >>= \now -> pause w now PRAllowance (Just aoCommitment)
  where
    lastMaybe = listToMaybe . reverse

runProbe :: Worker -> IO ()
runProbe w@Worker {wkOrders = OrdersEnv {oePause, oeNow}, wkSender} = forever $ do
  paused <- atomically $ readTVar oePause >>= maybe (Left <$> senderCause) (pure . Right)
  case paused of
    Left reason -> oeNow >>= \now -> pause w now reason Nothing
    Right p@Pause {paReason} -> do
      holding <- safelyWith "names worker: the probe" [paReason] $ pauseCauses w
      case holding of
        [] -> atomically $ writeTVar oePause Nothing
        cause : _ -> do
          when (paReason `notElem` holding) $ atomically $ writeTVar oePause $ Just p {paReason = cause, paCommitment = Nothing}
          threadDelay retryMicros
  where
    senderCause = senderCauses wkSender >>= maybe retry pure . listToMaybe

pauseCauses :: Worker -> IO [PauseReason]
pauseCauses w@Worker {wkOrders = OrdersEnv {oeStore}, wkChain, wkSender} = do
  allowance <- withTransaction oeStore activeOrders >>= covered w
  fees <- feesWithinCap wkChain
  stuck <- atomically $ senderCauses wkSender
  pure $ [PRAllowance | allowance /= Right True] <> [PRFeeCap | fees /= Right True] <> stuck

senderCauses :: Sender -> STM [PauseReason]
senderCauses s = do
  overCap <- replacementOverCap s
  unfunded <- keyUnderfunded s
  pure $ [PRFeeCap | overCap] <> [PRUnfunded | unfunded]

pause :: Worker -> UTCTime -> PauseReason -> Maybe ByteString -> IO ()
pause Worker {wkOrders = OrdersEnv {oePause}} now reason commitment =
  atomically $ modifyTVar' oePause $ Just . fromMaybe Pause {paSince = now, paReason = reason, paCommitment = commitment}

covered :: Worker -> [ActiveOrder] -> IO (Either ChainError Bool)
covered Worker {wkOrders = OrdersEnv {oeContracts}} orders
  | null orders = pure $ Right True
  | otherwise = runExceptT $ do
      allowance <- ExceptT $ registrarAllowance oeContracts
      prices <- ExceptT $ together $ traverse (\len -> (len,) <$> concurrent (chainPriceCents oeContracts len 1)) $ nubOrd $ map aoLength orders
      pure $ allowance >= sum [registerCost cents aoDuration | (len, cents) <- prices, ActiveOrder {aoLength, aoDuration} <- orders, aoLength == len]

inFlight :: [ActiveOrder] -> [ActiveOrder]
inFlight = filter ((/= PQueued) . aoPhase)

throughOrder :: RegistrationId -> [ActiveOrder] -> [ActiveOrder]
throughOrder rid orders = inFlight orders <> before <> take 1 rest
  where
    (before, rest) = break ((== rid) . aoId) $ queuedOrders orders
