{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
    maxUnexpiredWaits,
    Retried (..),
    RetryRefusal (..),
    retryRegistration,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, asyncWithUnmask, cancel, poll)
import Control.Concurrent.STM
import Control.Exception (finally, mask_)
import Control.Monad (forM, forM_, forever, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Util (tshow, ($>>=))
import StoreService.Config (ChainConfig (..), NamesConfig (..))
import StoreService.Log (logError, logInfo, logWarn, safely, safelyWith)
import StoreService.Names.Chain (Block (..), ChainClient, ChainError (..), TxHash, blockAt)
import StoreService.Names.Codes (CodeRefusal (..), codeRefusal, releaseCode, reserveCode)
import StoreService.Names.Contracts (CommitmentAges (..), acceptedTerms, bonaFideName, commitmentAges, commitmentTime, controllerAvailable, namePolicy, sponsorNonce)
import StoreService.Names.Orders (Landing (..), OrdersEnv (..), WriteRefusal (..), confirmLanded, versionWritten)
import StoreService.Names.Phases (RegistrationId (..), awaitChange, publish)
import StoreService.Names.Registration (TermsAcceptance (..), acceptCalldata, commitCalldata, policyDomain, registerWithCreditCalldata, sponsorDomain, verifyAcceptance, verifyRegisterIntent)
import StoreService.Names.Reveal (intentOf)
import StoreService.Names.Revert (RevertName (..))
import StoreService.Names.Sender (PendingTx, SendError (..), Sender, awaitOutcome, inFlightTransaction, ptHash, submitTransaction)
import StoreService.Names.Store
import StoreService.Names.Turns (withBalanceTurn, withLabelTurn)
import StoreService.Names.View (lapseMoment, notSignedInTime, registrationAt, registrationVersion)

data Worker = Worker
  { wkOrders :: OrdersEnv,
    wkChain :: ChainClient,
    wkSender :: Sender,
    -- | the thread following each registration, kept by the scan
    wkThreads :: TVar (Map RegistrationId (Async ())),
    -- | the one-second waits in a row after a commit's dry run reverted UnexpiredCommitmentExists
    wkUnexpiredWaits :: TVar (Map RegistrationId Int)
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
  | -- | a transaction of the registration is in flight
    Await PendingTx

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

-- | One-second waits in a row after a commit's dry run reverts UnexpiredCommitmentExists, before the usual retry interval:
-- enough for the node's block to pass the lapse second, not to spin on a node that keeps disagreeing with its own read.
maxUnexpiredWaits :: Int
maxUnexpiredWaits = 3

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
newWorker wkOrders wkChain wkSender = Worker wkOrders wkChain wkSender <$> newTVarIO M.empty <*> newTVarIO M.empty

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
  forM_ [nrId | NameRegistration {nrId} <- active, M.notMember nrId running] $ \rid ->
    mask_ $ asyncWithUnmask (\unmask -> unmask $ work w rid) >>= atomically . modifyTVar' wkThreads . M.insert rid

work :: Worker -> RegistrationId -> IO ()
work w rid@(RegistrationId ridText) = void $ safely ("names worker: registration " <> ridText) $ follow w rid Nothing

-- | Steps until the registration leaves the worker's phases, logging a lasting failure once.
follow :: Worker -> RegistrationId -> Maybe Text -> IO ()
follow w@Worker {wkOrders = OrdersEnv {oeStore, oePhases, oeNow}} rid@(RegistrationId ridText) lastFailure =
  stepRegistration w rid >>= \case
    Done -> pure ()
    Again -> follow w rid Nothing
    WaitUntil v t -> waitFor v t >> follow w rid Nothing
    Retry v reason -> do
      when (Just reason /= lastFailure) $ logWarn $ "names worker: registration " <> ridText <> " waits: " <> reason
      now <- oeNow
      waitFor v $ addUTCTime retryInterval now
      follow w rid $ Just reason
    Await p -> awaitOutcome outcomeWait p >> follow w rid Nothing
  where
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
      PQueued -> commit w now r
      PCommitting -> commit w now r
      PCommitted -> committed w now r
      PRegistering -> registering w now r
      _ -> pure Done

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
toCommitted w now r@NameRegistration {nrId} CommitmentAges {caMin, caMax} t =
  unexpiredDone w nrId >> ourCommit w r t >>= \case
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
  submitTransaction wkSender (TxCommit nrId) (cController $ nChain oeConfig) (commitCalldata nrCommitment) >>= \case
    -- a commit mined since the read, or a latest block still at the lapse second
    Left (SEChain (ChainReverted (Just RNUnexpiredCommitmentExists))) -> unexpired w now r
    sendResult -> do
      unexpiredDone w nrId
      case sendResult of
        Right p -> commitSent w now r p
        Left SEInFlight -> pure Again
        Left e -> pure $ failure r e

unexpired :: Worker -> UTCTime -> NameRegistration -> IO Next
unexpired Worker {wkUnexpiredWaits} now r@NameRegistration {nrId = rid@(RegistrationId ridText)} = do
  waits <- atomically $ stateTVar wkUnexpiredWaits $ \m -> let n = M.findWithDefault 0 rid m + 1 in (n, M.insert rid n m)
  if waits <= maxUnexpiredWaits
    then do
      logWarn $ "names worker: registration " <> ridText <> ": the commit's dry run reverted UnexpiredCommitmentExists, wait " <> tshow waits <> " of a second"
      pure $ waitUntil r $ addUTCTime chainSecond now
    else pure $ Retry (nrVersion r) "the commit's dry run keeps reverting UnexpiredCommitmentExists"

unexpiredDone :: Worker -> RegistrationId -> IO ()
unexpiredDone Worker {wkUnexpiredWaits} = atomically . modifyTVar' wkUnexpiredWaits . M.delete

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
        _ -> save w now r {nrPhase = PFailed, nrFailedAt = Just now, nrError = Just notSignedInTime}
      Just rv@NameReveal {rvAcceptance, rvDeadline} ->
        (pure (if unlapsed then rvAcceptance else Nothing) $>>= relayAcceptance w now r rvDeadline) >>= \case
          Just next -> pure next
          Nothing -> case secondsTime . toInteger <$> nrRevealAfter of
            Just revealAfter | now < revealAfter -> pure $ waitUntil r revealAfter
            _ -> sendRegister w now r rv
  where
    -- past the commitment's lapse no acceptance is relayed, as the policy would commit again; the register's dry run then
    -- fails the order as it does a plain name's
    unlapsed = all ((now <=) . secondsTime . toInteger) nrRevealBefore

-- | The relay of the reveal's acceptance to the name policy, sent without waiting for revealAfter; Nothing once the policy
-- stores that acceptance. One signed for another policy, before a setNamePolicy, or for the other kind of terms, clears the
-- reveal for the page to sign again; the relay's dry run's revert is judged as a register's is, unless the policy holds the
-- acceptance, which another sent first.
relayAcceptance :: Worker -> UTCTime -> NameRegistration -> Word64 -> TermsAcceptance -> IO (Maybe Next)
relayAcceptance w@Worker {wkOrders = OrdersEnv {oeContracts, oeConfig = NamesConfig {nChain = ChainConfig {cChainId}}}, wkSender} now r@NameRegistration {nrId = regId@(RegistrationId rid), nrOwner, nrCommitment, nrLabelHash} deadline a@TermsAcceptance {taTermsHash} =
  inFlightTransaction wkSender (TxAccept regId) >>= \case
    Just p -> pure $ Just $ Await p
    Nothing ->
      runExceptT ((,,) <$> ExceptT (namePolicy oeContracts) <*> ExceptT (bonaFideName oeContracts nrLabelHash) <*> ExceptT held) >>= \case
        Left e -> pure $ Just $ failure r e
        Right (policy, listed, accepted)
          | accepted == taTermsHash -> pure Nothing
          | otherwise -> Just <$> relay policy listed
  where
    held = acceptedTerms oeContracts nrCommitment nrOwner
    relay policy listed =
      verifyAcceptance (policyDomain cChainId policy) listed nrOwner nrCommitment deadline a >>= \case
        Left refusal -> do
          logWarn $ "names worker: registration " <> rid <> ": its stored acceptance is refused, " <> T.pack refusal
          save w now r {nrReveal = Nothing}
        Right () ->
          submitTransaction wkSender (TxAccept regId) policy (acceptCalldata listed nrCommitment nrOwner deadline a) >>= \case
            Right p -> pure $ Await p
            Left SEInFlight -> pure Again
            Left (SEChain (ChainReverted reason)) ->
              held >>= \case
                Right accepted | accepted == taTermsHash -> pure Again
                _ -> judge w now r reason
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
      submitTransaction wkSender (TxRegister regId) cSponsor (registerWithCreditCalldata intent) >>= \case
        Right p -> registerSent w now r p
        Left SEInFlight -> pure Again
        Left (SEChain (ChainReverted reason)) -> judge w now r reason
        Left e -> pure $ failure r e
  where
    intent = intentOf cfg r rv

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
-- with status 1 or the owner holds the name; committed again on InvalidNonce or a refused terms acceptance, for the page to sign again; taken on any revert
-- while the name is not available at the latest block; failed otherwise, unless the dry run named the name as gone.
judge :: Worker -> UTCTime -> NameRegistration -> Maybe RevertName -> IO Next
judge w@Worker {wkOrders = oe@OrdersEnv {oeContracts}} now r@NameRegistration {nrReveal} reason =
  inTurn w r $
    confirmLanded oe r >>= \case
      LRegistered v -> pure (Just v, Again)
      LChanged -> pure (Nothing, Again)
      LUnread -> pure (Nothing, Retry (nrVersion r) "the register check's chain read failed")
      LNotLanded -> case reason of
        Just e | e `elem` [RNInvalidNonce, RNTermsNotAccepted, RNTermsMismatch] -> store w now r {nrReveal = Nothing}
        Just RNCommitmentTooNew -> pure (Nothing, waitUntil r $ addUTCTime chainSecond now)
        _ -> case nrReveal of
          Just NameReveal {rvLabel} ->
            controllerAvailable oeContracts rvLabel >>= \case
              Left e -> pure (Nothing, failure r e)
              Right False -> taken w now r
              -- the dry run runs on the pending block, where a competitor's register that may never mine can hold the name
              Right True
                | reason `elem` map Just [RNNameNotAvailable, RNNameReserved] -> pure (Nothing, Retry (nrVersion r) "the dry run reverted the name as gone, but it reads available")
                | otherwise -> failed w now r reason
          Nothing -> failed w now r reason

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
failed :: Worker -> UTCTime -> NameRegistration -> Maybe RevertName -> IO (Maybe Int64, Next)
failed w now r@NameRegistration {nrId = RegistrationId rid} reason = do
  when (reason == Just RNInsufficientAllowance) $
    logError $ "names worker: registration " <> rid <> " failed: the registrar's allowance on the sponsor is used up; the guardian raises it with setRegistrarAllowance"
  store w now r {nrPhase = PFailed, nrFailedAt = Just now, nrError = Just notRegistered}

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

unixSeconds :: UTCTime -> Integer
unixSeconds = floor . utcTimeToPOSIXSeconds

secondsTime :: Integer -> UTCTime
secondsTime = posixSecondsToUTCTime . fromInteger

-- | What Try again did: the version it wrote, the order changed since it was read, or a refusal.
data Retried = RTWritten Int64 | RTChanged | RTRefused RetryRefusal

data RetryRefusal = RFNotFailed | RFLinksRefused | RFCode CodeRefusal | RFUnread
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
        LNotLanded -> retryCode oe now r >>= either (pure . RTRefused . RFCode) (requeue oe now r)
    _ -> pure $ RTRefused RFNotFailed

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
  when (count > 0) $ logInfo $ "names code sweep: released the codes of " <> tshow count <> " failed order(s)"
  where
    stillFailed cutoff rid db =
      getRegistration db rid <&> \case
        Just NameRegistration {nrPhase = PFailed, nrFailedAt = Just failedAt} | failedAt < cutoff -> Right ()
        _ -> Left ()

runCodeSweep :: Worker -> IO ()
runCodeSweep w = forever $ safely "names code sweep" (sweepFailedCodes w) >> threadDelay sweepInterval
