{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Record edits signed by a name's owner, each request relayed as one transaction through the registrar key's sender and followed to its outcome.
module StoreService.Names.Records
  ( Records (rsPendingAfter, rsEditsPerYear, rsClaimed),
    EditRequest (..),
    EditAnswer (..),
    EditsLeft (..),
    newRecords,
    nameEditsLeft,
    relayEdit,
    runRecordEdits,
    recordsRound,
    deepBlock,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket, onException)
import Control.Logger.Simple (logError, logWarn)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.TH as JQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto (sha256Hash)
import Simplex.Messaging.Encoding (Encoding (..))
import Simplex.Messaging.Eth.Address (Address, recoverAddress)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain)
import Simplex.Messaging.Parsers (defaultJSON)
import Simplex.Messaging.SimplexName (LabelHash, SimplexTLD)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), SystemSeconds)
import Simplex.Messaging.Util (catchOwn', tshow)
import StoreService.Names.Chain
import StoreService.Names.Contracts (Contracts (ctChain), NameStatus (..), RegisteredName (..), nameStatus, resolverNonce, textRecord)
import StoreService.Names.Registration (Node, labelNode, multicallCalldata, recordKey, resolverDomain, setTextDigest, setTextWithSigCalldata)
import StoreService.Names.Revert (RevertName (..), revertName)
import StoreService.Names.Sender (SendError (..), Sender, submitTransaction)
import StoreService.Names.Store
import StoreService.Names.Turns (Turns, withLabelTurn)

data Records = Records
  { rsStore :: DBStore,
    rsTurns :: Turns,
    rsContracts :: Contracts,
    rsSender :: Sender,
    rsTLD :: SimplexTLD,
    rsEditsPerYear :: Int,
    rsNow :: IO UTCTime,
    -- | microseconds a request waits for its edit's outcome
    rsPendingAfter :: Int,
    -- | the edits a request or the lane is sending or judging, so no edit is sent twice
    rsClaimed :: TVar (Set RecordEditId),
    -- | the pending edits whose last chain read failed
    rsStalled :: TVar (Set RecordEditId),
    -- | grows on every settlement and stall, waking the requests that wait for an edit
    rsChanges :: TVar Int
  }

-- | The changes of one Save, in nonce order, relayed together or not at all.
data EditRequest = EditRequest
  { erLabelHash :: LabelHash,
    erChanges :: NonEmpty SignedChange
  }
  deriving (Eq, Show)

data EditAnswer
  = EASettled EditOutcome
  | -- | not settled within the wait, or the chain stopped answering after the send; the edit goes on
    EAPending
  | -- | the signers, the deadline, the name's state, owner or resolver, or another edit holding the nonce
    EARefused Text
  | EAStaleNonce
  | -- | the chain failed before anything was sent, and nothing is stored
    EAUnavailable
  | -- | the per-name cap, with the seconds until another edit fits
    EACapped Int
  | -- | the name's yearly limit is used up until this time
    EALimited UTCTime
  deriving (Eq, Show)

-- | The edits a name has left in its window, and when the next one returns; Nothing while none counts.
data EditsLeft = EditsLeft {left :: Int, nextAt :: Maybe UTCTime}
  deriving (Eq, Show)

$(JQ.deriveToJSON defaultJSON ''EditsLeft)

-- | What the chain shows of an edit none of whose transactions mined.
data Judgment
  = -- | one of its own hashes, or Nothing when the nonce passed with the record holding its value
    Applied (Maybe TxHash)
  | -- | the nonce passed with the record holding another value
    NonceTaken
  | NonceFree

-- | Below a proxy's idle timeout; the edit goes on after it.
editPendingAfter :: Int
editPendingAfter = 90000000

-- | Relayed edits per name in 'editCapWindow'.
editCap :: Int
editCap = 10

editCapWindow :: NominalDiffTime
editCapWindow = 3600

-- | The window of the yearly limit, in 365-day years as the names' terms count them.
editLimitWindow :: NominalDiffTime
editLimitWindow = 365 * 86400

recordsInterval :: Int
recordsInterval = 4000000

-- | Seconds an edit's deadline must leave past the latest block, or it could revert after paying gas.
-- Above two of the sender's fee replacements (Sender.replaceAfter) and 20 blocks of 12 seconds.
editDeadlineMargin :: Word64
editDeadlineMargin = 600

newRecords :: DBStore -> Turns -> Contracts -> Sender -> SimplexTLD -> Int -> IO UTCTime -> IO Records
newRecords rsStore rsTurns rsContracts rsSender rsTLD rsEditsPerYear rsNow = do
  rsClaimed <- newTVarIO S.empty
  rsStalled <- newTVarIO S.empty
  rsChanges <- newTVarIO 0
  pure Records {rsStore, rsTurns, rsContracts, rsSender, rsTLD, rsEditsPerYear, rsNow, rsPendingAfter = editPendingAfter, rsClaimed, rsStalled, rsChanges}

rsChain :: Records -> ChainClient
rsChain = ctChain . rsContracts

-- | Every relayed edit counts, a reverted one too, since its gas is paid. One refused or never sent does not.
nameEditsLeft :: DB.Connection -> Int -> LabelHash -> UTCTime -> IO EditsLeft
nameEditsLeft db perYear lh now = editsLeft perYear <$> relayedEditTimes db lh (addUTCTime (negate editLimitWindow) now)

editsLeft :: Int -> [UTCTime] -> EditsLeft
editsLeft perYear times =
  -- the oldest edit returns one, unless a lowered limit is overrun and later ones must go too
  EditsLeft {left = max 0 $ perYear - counted, nextAt = addUTCTime editLimitWindow <$> listToMaybe (drop (counted - perYear) times)}
  where
    counted = length times

-- | When the name's yearly limit is used up, the time the next edit fits.
usedUpUntil :: EditsLeft -> Maybe UTCTime
usedUpUntil EditsLeft {left, nextAt} = if left == 0 then nextAt else Nothing

-- | An identical repeat of a sent edit is answered from it, and the yearly limit is checked, before any chain read.
-- The wait for an outcome is bounded from the request's arrival, so the checks and the send count toward it.
relayEdit :: Records -> EditRequest -> IO EditAnswer
relayEdit rs@Records {rsPendingAfter, rsEditsPerYear, rsNow} req@EditRequest {erLabelHash, erChanges} = do
  timer <- registerDelay rsPendingAfter
  sequence <$> mapM (\(digest, SignedChange {scSignature}) -> recoverAddress scSignature digest) signed >>= \case
    Right (signer :| others) | all (== signer) others ->
      withStore rs (`getRecordEditByBody` bodyHash) >>= \case
        Just RecordEdit {reId} -> awaitEdit rs timer reId
        Nothing -> do
          now <- rsNow
          withStore rs (\db -> usedUpUntil <$> nameEditsLeft db rsEditsPerYear erLabelHash now)
            >>= maybe (newEdit rs timer req signer bodyHash) (pure . EALimited)
    _ -> pure $ EARefused "the changes are not signed by one key"
  where
    signed = L.map (\change -> (changeDigest rs erLabelHash change, change)) erChanges
    -- one change hashes as edits stored before requests took two, so their repeats still match
    bodyHash = Hex32 $ sha256Hash $ B.concat [digest <> smpEncode scSignature | (digest, SignedChange {scSignature}) <- L.toList signed]

newEdit :: Records -> TVar Bool -> EditRequest -> Address -> BodyHash -> IO EditAnswer
newEdit rs timer req signer bodyHash =
  chainAccepts rs req signer >>= \case
    Left e -> EAUnavailable <$ logWarn ("names records: an edit's chain checks failed: " <> tshow e)
    Right (Just refusal) -> pure refusal
    Right Nothing -> bracket (insertEdit rs req signer bodyHash) (mapM_ (release rs) . either (const Nothing) id) inserted >>= either pure (awaitEdit rs timer)
  where
    inserted = \case
      Left answer -> pure $ Left answer
      Right (Just editId) -> maybe (Right editId) Left <$> sendNew rs editId
      -- a pending or mined edit holds the nonce: the identical twin is waited for, any other body refused
      Right Nothing -> maybe (Left $ EARefused "another edit holds the nonce") (Right . reId) <$> withStore rs (`getRecordEditByBody` bodyHash)

-- | Nothing for the earliest deadline more than 'editDeadlineMargin' past the latest block's time, the signer the registry's owner on the default resolver, the first nonce the owner's next.
chainAccepts :: Records -> EditRequest -> Address -> IO (Either ChainError (Maybe EditAnswer))
chainAccepts rs@Records {rsContracts} EditRequest {erLabelHash, erChanges} signer = runExceptT $ do
  Block {blockTimestamp} <- ExceptT $ latestBlock $ rsChain rs
  if roundedSeconds (earliestDeadline erChanges) <= roundedSeconds blockTimestamp + fromIntegral editDeadlineMargin
    then refused "the deadline is too close to the latest block"
    else
      ExceptT (nameStatus rsContracts erLabelHash) >>= \case
        NSRegistered RegisteredName {rnOwner, rnResolver}
          | rnOwner /= signer -> refused "the signer does not own the name"
          | rnResolver /= resolver rs -> refused "the name is on another resolver"
          | otherwise -> (\nonce -> if nonce == firstNonce erChanges then Nothing else Just EAStaleNonce) <$> ExceptT (resolverNonce rsContracts Latest signer)
        _ -> refused "the name is not registered"
  where
    refused = pure . Just . EARefused

-- | Left when the name's yearly limit or hourly cap is used up, checked again under the name's turn against a concurrent edit.
-- Right Nothing when a pending or mined edit holds the first nonce.
insertEdit :: Records -> EditRequest -> Address -> BodyHash -> IO (Either EditAnswer (Maybe RecordEditId))
insertEdit rs@Records {rsStore, rsTurns, rsEditsPerYear, rsNow, rsClaimed} EditRequest {erLabelHash, erChanges} signer bodyHash =
  withLabelTurn rsTurns erLabelHash $ do
    now <- rsNow
    claimed <- newIORef Nothing
    let claim editId = editId <$ (atomically (modifyTVar' rsClaimed $ S.insert editId) >> writeIORef claimed (Just editId))
    (`onException` (readIORef claimed >>= mapM_ (release rs))) $
      withTransaction rsStore $ \db -> do
        times <- relayedEditTimes db erLabelHash (addUTCTime (negate editLimitWindow) now)
        let hour = dropWhile (<= addUTCTime (negate editCapWindow) now) times
        case usedUpUntil (editsLeft rsEditsPerYear times) of
          Just nextAt -> pure $ Left $ EALimited nextAt
          Nothing
            | Just oldest <- listToMaybe $ drop (editCap - 1) $ reverse hour -> pure $ Left $ EACapped $ max 1 $ ceiling $ diffUTCTime (addUTCTime editCapWindow oldest) now
            | otherwise -> Right <$> (insertRecordEdit db edit now >>= mapM claim)
  where
    edit = NewRecordEdit {nreLabelHash = erLabelHash, nreOwner = signer, nreChanges = erChanges, nreBodyHash = bodyHash}

-- | An edit that was not sent is answered and deleted while still claimed, so the lane cannot send it after.
sendNew :: Records -> RecordEditId -> IO (Maybe EditAnswer)
sendNew rs editId = withStore rs (`getRecordEdit` editId) >>= maybe (pure Nothing) send
  where
    send edit =
      sendEdit rs edit >>= \case
        Right () -> pure Nothing
        Left e -> do
          answer <- case e of
            -- anyone can relay a signed edit, so a revert may mean another relayer applied it
            SEChain (ChainReverted name) ->
              fmap judgmentOutcome <$> judge rs edit >>= \case
                Left e' -> EAUnavailable <$ logWarn ("names records: edit " <> tshow editId <> " reverted in its dry run and was not judged: " <> tshow e')
                Right outcome -> pure $ EASettled $ fromMaybe (EOReverted $ revertName name) outcome
            _ -> EAUnavailable <$ logWarn ("names records: edit " <> tshow editId <> " not sent: " <> tshow e)
          Just answer <$ forget rs editId

-- | Left when nothing was sent: the dry run reverted or the chain failed.
sendEdit :: Records -> RecordEdit -> IO (Either SendError ())
sendEdit rs@Records {rsSender} edit@RecordEdit {reId} =
  submitTransaction rsSender (TxEdit reId) (resolver rs) (editCalldata rs edit) >>= \case
    Right _ -> Right () <$ unstall rs reId
    Left SEInFlight -> pure $ Right ()
    Left e -> pure $ Left e

-- | Only sent edits are stored, so one never sent leaves no row and its repeat is checked anew.
forget :: Records -> RecordEditId -> IO ()
forget rs editId = withStore rs (`deleteUnsentEdit` editId) >> settled rs editId

-- | Waits until the edit settles, the chain stops answering for it, or the timer fires.
awaitEdit :: Records -> TVar Bool -> RecordEditId -> IO EditAnswer
awaitEdit rs@Records {rsChanges, rsStalled} timer editId = loop
  where
    loop = do
      -- read before the row, so a change committed after the read still wakes this request
      seen <- readTVarIO rsChanges
      withStore rs (`getRecordEdit` editId) >>= \case
        Nothing -> pure EAUnavailable
        Just RecordEdit {reState = ESSettled outcome} -> pure $ EASettled outcome
        Just RecordEdit {reState = ESPending} -> do
          stalled <- S.member editId <$> readTVarIO rsStalled
          if stalled
            then pure EAPending
            else do
              changed <- atomically $ (True <$ (readTVar rsChanges >>= check . (/= seen))) `orElse` (False <$ (readTVar timer >>= check))
              if changed then loop else pure EAPending

runRecordEdits :: Records -> IO ()
runRecordEdits rs = forever $ recordsRound rs >> threadDelay recordsInterval

-- | One pass over the pending edits that no request is sending. A pass or an edit that fails is logged, and the lane goes on.
recordsRound :: Records -> IO ()
recordsRound rs =
  ( do
      editIds <- withStore rs pendingEditIds
      unless (null editIds) $
        runExceptT (ExceptT (latestBlock $ rsChain rs) >>= \tip -> (tip,) <$> ExceptT (deepBlock (rsChain rs) tip)) >>= \case
          Left e -> mapM_ (\editId -> stall rs editId e) editIds
          -- each edit on its own, so a row that fails stops no other edit
          Right (tip, deep) -> forM_ editIds $ \editId ->
            withClaim rs editId () (withStore rs (`getRecordEdit` editId) >>= mapM_ (follow rs tip deep))
              `catchOwn'` \e -> logError ("names records: edit " <> tshow editId <> " failed; the next pass follows it: " <> tshow e)
  )
    `catchOwn'` \e -> logError ("names records: the round failed; the next pass will run: " <> tshow e)

-- | The newest block at the configured confirmation depth, as the sender counts it.
deepBlock :: ChainClient -> Block -> IO (Either ChainError Block)
deepBlock c tip@Block {blockNumber}
  | deep == blockNumber = pure $ Right tip
  | otherwise = blockAt c deep
  where
    deep = confirmedNumber c blockNumber

-- | One step for a pending edit, from its transactions' statuses as the sender settled them.
follow :: Records -> Block -> Block -> RecordEdit -> IO ()
follow rs Block {blockTimestamp} Block {blockTimestamp = deepTime} edit@RecordEdit {reId, reChanges} = do
  txs <- withStore rs (`editTransactions` reId)
  case txs of
    [] -> if ahead then sendUnsent else forget rs reId
    _
      | mined : _ <- [ntHash | NameTransaction {ntHash, ntState = TxSettled TSMinedAt {}} <- txs] -> settle rs reId $ EOApplied $ Just mined
      | any ((== TxPending) . ntState) txs -> if deepTime > reDeadline then expire else unstall rs reId
      -- a mined revert carries no data: past the deadline it is SignatureExpired, else unnamed
      | block : _ <- [b | NameTransaction {ntState = TxSettled (TSRevertedAt b)} <- txs] ->
          blockAt (rsChain rs) block >>= \case
            Left e -> stall rs reId e
            Right Block {blockTimestamp = minedTime} -> judged rs edit $ settle rs reId $ EOReverted $ if minedTime > reDeadline then signatureExpired else Nothing
      -- every transaction dropped: its registrar nonce went to another one
      | otherwise -> judged rs edit $ if ahead then resend else expire
  where
    reDeadline = earliestDeadline reChanges
    ahead = reDeadline > blockTimestamp
    expire = judged rs edit $ settle rs reId $ EOReverted signatureExpired
    sendUnsent =
      sendEdit rs edit >>= \case
        Left (SEChain (ChainReverted _)) -> forget rs reId
        r -> either (stall rs reId) pure r
    -- the edit was sent before, so its dry run's revert is its outcome
    resend =
      sendEdit rs edit >>= \case
        Left (SEChain (ChainReverted name)) -> settle rs reId $ EOReverted $ revertName name
        r -> either (stall rs reId) pure r

-- | Settles the edit as the chain shows it, or runs the action when its nonce is still free.
judged :: Records -> RecordEdit -> IO () -> IO ()
judged rs edit@RecordEdit {reId} whenFree =
  fmap judgmentOutcome <$> judge rs edit >>= \case
    Left e -> stall rs reId e
    Right outcome -> maybe whenFree (settle rs reId) outcome

-- | Its own transactions first: a status-1 receipt answers its hash. Without one, the owner's nonce and the records.
-- A nonce past the first change but not the last means another body took one of its nonces.
judge :: Records -> RecordEdit -> IO (Either ChainError Judgment)
judge rs@Records {rsContracts} RecordEdit {reId, reLabelHash, reOwner, reChanges} = runExceptT $ do
  txs <- liftIO $ withStore rs (`editTransactions` reId)
  receipts <- forM txs $ \NameTransaction {ntHash} -> (ntHash,) <$> ExceptT (transactionReceipt (rsChain rs) ntHash)
  case [h | (h, Just Receipt {receiptSuccess = True}) <- receipts] of
    h : _ -> pure $ Applied $ Just h
    [] -> do
      nonce <- ExceptT $ resolverNonce rsContracts Latest reOwner
      if
        | nonce <= firstNonce reChanges -> pure NonceFree
        | nonce <= lastNonce reChanges -> pure NonceTaken
        | otherwise -> do
            holds <- and <$> mapM (holdsChange . scChange) reChanges
            pure $ if holds then Applied Nothing else NonceTaken
  where
    holdsChange (RCSetText key value) = (== value) <$> record (recordKey key)
    record = ExceptT . textRecord rsContracts reLabelHash

judgmentOutcome :: Judgment -> Maybe EditOutcome
judgmentOutcome = \case
  Applied tx -> Just $ EOApplied tx
  NonceTaken -> Just $ EOReverted $ Just RNInvalidNonce
  NonceFree -> Nothing

settle :: Records -> RecordEditId -> EditOutcome -> IO ()
settle rs editId outcome = do
  void $ withStore rs $ \db -> settleRecordEdit db editId outcome
  settled rs editId

-- | Wakes the requests waiting for the edit, which read it again.
settled :: Records -> RecordEditId -> IO ()
settled Records {rsStalled, rsChanges} editId = atomically $ modifyTVar' rsStalled (S.delete editId) >> modifyTVar' rsChanges (+ 1)

-- | Logged once until the edit's next successful step.
stall :: Show e => Records -> RecordEditId -> e -> IO ()
stall Records {rsStalled, rsChanges} editId e = do
  new <- atomically $ do
    stalled <- readTVar rsStalled
    let new = not $ S.member editId stalled
    when new $ writeTVar rsStalled (S.insert editId stalled) >> modifyTVar' rsChanges (+ 1)
    pure new
  when new $ logWarn $ "names records: edit " <> tshow editId <> " waits for the chain: " <> tshow e

unstall :: Records -> RecordEditId -> IO ()
unstall Records {rsStalled} editId = atomically $ modifyTVar' rsStalled $ S.delete editId

-- | Runs the action only when nobody else holds the edit, else answers the fallback.
withClaim :: Records -> RecordEditId -> a -> IO a -> IO a
withClaim rs@Records {rsClaimed} editId fallback action =
  bracket claim (\claimed -> when claimed $ release rs editId) $ \claimed -> if claimed then action else pure fallback
  where
    claim = atomically $ do
      held <- readTVar rsClaimed
      if S.member editId held then pure False else True <$ writeTVar rsClaimed (S.insert editId held)

release :: Records -> RecordEditId -> IO ()
release Records {rsClaimed} editId = atomically $ modifyTVar' rsClaimed $ S.delete editId

changeDigest :: Records -> LabelHash -> SignedChange -> ByteString
changeDigest rs lh SignedChange {scChange = RCSetText key value, scNonce, scDeadline} =
  setTextDigest (domain rs) (nodeOf rs lh) key value scNonce scDeadline

-- | One change is sent as its own call, which keeps the revert's name; two go in one multicall.
editCalldata :: Records -> RecordEdit -> ByteString
editCalldata rs RecordEdit {reLabelHash, reChanges} = case reChanges of
  change :| [] -> changeCalldata change
  _ -> multicallCalldata $ map changeCalldata $ L.toList reChanges
  where
    node = nodeOf rs reLabelHash
    changeCalldata SignedChange {scChange = RCSetText key value, scNonce, scDeadline, scSignature} =
      setTextWithSigCalldata node key value scNonce scDeadline scSignature

firstNonce, lastNonce :: NonEmpty SignedChange -> Word64
firstNonce = scNonce . L.head
lastNonce = scNonce . L.last

-- | Past it the edit cannot land whole, so it is SignatureExpired.
earliestDeadline :: NonEmpty SignedChange -> SystemSeconds
earliestDeadline = minimum . L.map scDeadline

nodeOf :: Records -> LabelHash -> Node
nodeOf Records {rsTLD} = labelNode rsTLD

domain :: Records -> Eip712Domain
domain rs = resolverDomain (cChainId $ ccConfig $ rsChain rs) (resolver rs)

resolver :: Records -> Address
resolver = cResolver . ccConfig . rsChain

signatureExpired :: Maybe RevertName
signatureExpired = Just RNSignatureExpired

withStore :: Records -> (DB.Connection -> IO a) -> IO a
withStore Records {rsStore} = withTransaction rsStore
