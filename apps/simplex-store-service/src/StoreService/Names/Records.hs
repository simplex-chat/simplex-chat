{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Record edits signed by a name's owner, relayed through the registrar key's sender and followed to their outcome.
module StoreService.Names.Records
  ( Records (rsPendingAfter),
    EditRequest (..),
    EditAnswer (..),
    newRecords,
    relayEdit,
    runRecordEdits,
    recordsRound,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto (sha256Hash)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature)
import Simplex.Messaging.Eth.Address (Address, recoverAddress)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain, serializeSignature)
import Simplex.Messaging.SimplexName (LabelHash, SimplexTLD)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..))
import StoreService.Log (logWarn, safely)
import StoreService.Names.Chain
import StoreService.Names.Contracts (Contracts, NameStatus (..), RegisteredName (..), nameStatus, resolverNonce, textRecord)
import StoreService.Names.Registration (Node, channelRecordKey, clearRecordsDigest, clearRecordsWithSigCalldata, contactRecordKey, labelNode, resolverDomain, setTextDigest, setTextWithSigCalldata)
import StoreService.Names.Revert (RevertName (..), revertText)
import StoreService.Names.Sender (SendError (..), Sender, submitTransaction)
import StoreService.Names.Store
import StoreService.Names.Turns (Turns, withLabelTurn)

data Records = Records
  { rsStore :: DBStore,
    rsTurns :: Turns,
    rsContracts :: Contracts,
    rsChain :: ChainClient,
    rsSender :: Sender,
    rsTLD :: SimplexTLD,
    rsNow :: IO UTCTime,
    -- | microseconds a request waits for its edit's outcome
    rsPendingAfter :: Int,
    -- | the edits a request or the lane is sending or judging, so no edit is sent twice
    rsClaimed :: TVar (Set Int64),
    -- | the pending edits whose last chain read failed
    rsStalled :: TVar (Set Int64),
    -- | grows on every settlement and stall, waking the requests that wait for an edit
    rsChanges :: TVar Int
  }

data EditRequest = EditRequest
  { erLabelHash :: LabelHash,
    erChange :: RecordChange,
    erNonce :: Word64,
    erDeadline :: Word64,
    erSignature :: RecoverableSignature
  }
  deriving (Eq, Show)

data EditAnswer
  = -- | mined, Nothing when found applied with the applying transaction unknown
    EAApplied (Maybe TxHash)
  | -- | the revert's name when known; the nonce is free again
    EAReverted (Maybe Text)
  | -- | not settled within the wait, or the chain stopped answering after the send; the edit goes on
    EAPending
  | -- | the signer, the deadline, the owner or the nonce
    EARefused
  | -- | the chain failed before anything was sent, and nothing is stored
    EAUnavailable
  | -- | the per-name cap, with the seconds until another edit fits
    EACapped Int
  deriving (Eq, Show)

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

recordsInterval :: Int
recordsInterval = 4000000

newRecords :: DBStore -> Turns -> Contracts -> ChainClient -> Sender -> SimplexTLD -> IO UTCTime -> IO Records
newRecords rsStore rsTurns rsContracts rsChain rsSender rsTLD rsNow = do
  rsClaimed <- newTVarIO S.empty
  rsStalled <- newTVarIO S.empty
  rsChanges <- newTVarIO 0
  pure Records {rsStore, rsTurns, rsContracts, rsChain, rsSender, rsTLD, rsNow, rsPendingAfter = editPendingAfter, rsClaimed, rsStalled, rsChanges}

-- | An identical repeat of a sent edit is answered from it, before any chain read.
-- The wait for an outcome is bounded from the request's arrival, so the checks and the send count toward it.
relayEdit :: Records -> EditRequest -> IO EditAnswer
relayEdit rs@Records {rsPendingAfter} req@EditRequest {erSignature} = do
  timer <- registerDelay rsPendingAfter
  recoverAddress erSignature digest >>= \case
    Left _ -> pure EARefused
    Right signer ->
      withStore rs (`getRecordEditByBody` bodyHash) >>= \case
        Just RecordEdit {reId} -> awaitEdit rs timer reId
        Nothing -> newEdit rs timer req signer bodyHash
  where
    digest = editDigest rs req
    bodyHash = sha256Hash $ digest <> serializeSignature erSignature

newEdit :: Records -> TVar Bool -> EditRequest -> Address -> ByteString -> IO EditAnswer
newEdit rs timer req signer bodyHash =
  chainAccepts rs req signer >>= \case
    Left _ -> pure EAUnavailable
    Right False -> pure EARefused
    Right True ->
      insertEdit rs req signer bodyHash >>= \case
        Left wait -> pure $ EACapped wait
        Right (Just editId) -> sendNew rs timer editId
        -- a pending or mined edit holds the nonce: the identical twin is waited for, any other body refused
        Right Nothing -> withStore rs (`getRecordEditByBody` bodyHash) >>= maybe (pure EARefused) (awaitEdit rs timer . reId)

-- | The deadline after the latest block's time, the signer the registry's owner on the default resolver, the nonce the owner's next.
chainAccepts :: Records -> EditRequest -> Address -> IO (Either ChainError Bool)
chainAccepts Records {rsChain, rsContracts} EditRequest {erLabelHash, erNonce, erDeadline} signer = runExceptT $ do
  Block {blockTimestamp} <- ExceptT $ latestBlock rsChain
  if erDeadline <= blockTimestamp
    then pure False
    else
      ExceptT (nameStatus rsContracts erLabelHash) >>= \case
        NSRegistered RegisteredName {rnOwner, rnResolver}
          | rnOwner == signer && rnResolver == cResolver (ccConfig rsChain) -> (== erNonce) <$> ExceptT (resolverNonce rsContracts signer)
        _ -> pure False

-- | Left with the seconds to wait when the name's cap is used up; Right Nothing when a pending or mined edit holds the nonce.
insertEdit :: Records -> EditRequest -> Address -> ByteString -> IO (Either Int (Maybe Int64))
insertEdit Records {rsStore, rsTurns, rsNow} EditRequest {erLabelHash, erChange, erNonce, erDeadline, erSignature} signer bodyHash =
  withLabelTurn rsTurns erLabelHash $ do
    now <- rsNow
    withTransaction rsStore $ \db -> do
      times <- relayedEditTimes db erLabelHash (addUTCTime (negate editCapWindow) now)
      let relayed = length times
      if relayed >= editCap
        then pure $ Left $ max 1 $ ceiling $ diffUTCTime (addUTCTime editCapWindow $ times !! (relayed - editCap)) now
        else Right <$> insertRecordEdit db edit now
  where
    edit = NewRecordEdit {nreLabelHash = erLabelHash, nreOwner = signer, nreNonce = erNonce, nreChange = erChange, nreDeadline = erDeadline, nreSignature = erSignature, nreBodyHash = bodyHash}

-- | An edit that was not sent is answered and deleted while still claimed, so the lane cannot send it after.
sendNew :: Records -> TVar Bool -> Int64 -> IO EditAnswer
sendNew rs timer editId =
  withClaim rs editId Nothing (withStore rs (`getRecordEdit` editId) >>= maybe (pure Nothing) send) >>= maybe (awaitEdit rs timer editId) pure
  where
    send edit =
      sendEdit rs edit >>= \case
        Right () -> pure Nothing
        Left e -> do
          answer <- case e of
            SEChain (ChainReverted name) -> either (const EAUnavailable) (dryRunAnswer name) <$> judge rs edit
            _ -> EAUnavailable <$ logWarn ("names records: edit " <> tshow editId <> " not sent: " <> tshow e)
          Just answer <$ forget rs editId
    -- anyone can relay a signed edit, so a revert may mean another relayer applied it
    dryRunAnswer name = \case
      Applied tx -> EAApplied tx
      NonceTaken -> EAReverted $ revertText RNInvalidNonce
      NonceFree -> EAReverted $ revertText =<< name

-- | Left when nothing was sent: the dry run reverted or the chain failed.
sendEdit :: Records -> RecordEdit -> IO (Either SendError ())
sendEdit rs@Records {rsSender} edit@RecordEdit {reId} =
  submitTransaction rsSender (TxEdit reId) (resolver rs) (editCalldata rs edit) >>= \case
    Right _ -> Right () <$ unstall rs reId
    Left SEInFlight -> pure $ Right ()
    Left e -> pure $ Left e

-- | Only sent edits are stored, so one never sent leaves no row and its repeat is checked anew.
forget :: Records -> Int64 -> IO ()
forget rs editId = withStore rs (`deleteUnsentEdit` editId) >> settled rs editId

-- | Waits until the edit settles, the chain stops answering for it, or the timer fires.
awaitEdit :: Records -> TVar Bool -> Int64 -> IO EditAnswer
awaitEdit rs@Records {rsChanges, rsStalled} timer editId = loop
  where
    loop = do
      -- read before the row, so a change committed after the read still wakes this request
      seen <- readTVarIO rsChanges
      withStore rs (`getRecordEdit` editId) >>= \case
        Nothing -> pure EAUnavailable
        Just RecordEdit {reState = ESMined tx} -> pure $ EAApplied tx
        Just RecordEdit {reState = ESReverted revert} -> pure $ EAReverted revert
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
recordsRound rs@Records {rsChain} = void $ safely "names records: the round" $ do
  editIds <- withStore rs pendingEditIds
  unless (null editIds) $
    runExceptT (ExceptT (latestBlock rsChain) >>= \tip -> (tip,) <$> ExceptT (deepBlock rsChain tip)) >>= \case
      Left e -> mapM_ (\editId -> stall rs editId e) editIds
      -- each edit on its own, so a row that fails stops no other edit
      Right (tip, deep) -> forM_ editIds $ \editId ->
        safely ("names records: edit " <> tshow editId) $ withClaim rs editId () $ withStore rs (`getRecordEdit` editId) >>= mapM_ (follow rs tip deep)

-- | The newest block at the configured confirmation depth, as the sender counts it.
deepBlock :: ChainClient -> Block -> IO (Either ChainError Block)
deepBlock c@ChainClient {ccConfig = ChainConfig {cConfirmations}} tip@Block {blockNumber}
  | depth == 0 = pure $ Right tip
  | otherwise = blockAt c $ if blockNumber >= depth then blockNumber - depth else 0
  where
    depth = fromIntegral (max 1 cConfirmations) - 1

-- | One step for a pending edit, from its transactions' statuses as the sender settled them.
follow :: Records -> Block -> Block -> RecordEdit -> IO ()
follow rs@Records {rsChain} Block {blockTimestamp} Block {blockTimestamp = deepTime} edit@RecordEdit {reId, reDeadline} = do
  txs <- withStore rs (`editTransactions` reId)
  case txs of
    [] -> if ahead then sendUnsent else forget rs reId
    _
      | mined : _ <- [ntHash | NameTransaction {ntHash, ntStatus = TSMined} <- txs] -> settle rs reId $ ESMined $ Just mined
      | any ((== TSPending) . ntStatus) txs -> if deepTime > reDeadline then expire else unstall rs reId
      -- a mined revert carries no data: past the deadline it is SignatureExpired, else unnamed
      | block : _ <- [b | NameTransaction {ntStatus = TSReverted, ntMinedBlock = Just b} <- txs] ->
          blockAt rsChain block >>= \case
            Left e -> stall rs reId e
            Right Block {blockTimestamp = minedTime} -> judged rs edit $ settle rs reId $ ESReverted $ if minedTime > reDeadline then signatureExpired else Nothing
      -- every transaction dropped: its registrar nonce went to another one
      | otherwise -> judged rs edit $ if ahead then resend else expire
  where
    ahead = reDeadline > blockTimestamp
    expire = judged rs edit $ settle rs reId $ ESReverted signatureExpired
    sendUnsent =
      sendEdit rs edit >>= \case
        Left (SEChain (ChainReverted _)) -> forget rs reId
        r -> either (stall rs reId) pure r
    -- the edit was sent before, so its dry run's revert is its outcome
    resend =
      sendEdit rs edit >>= \case
        Left (SEChain (ChainReverted name)) -> settle rs reId $ ESReverted $ revertText =<< name
        r -> either (stall rs reId) pure r

-- | Settles the edit as the chain shows it, or runs the action when its nonce is still free.
judged :: Records -> RecordEdit -> IO () -> IO ()
judged rs edit@RecordEdit {reId} whenFree =
  judge rs edit >>= \case
    Left e -> stall rs reId e
    Right (Applied tx) -> settle rs reId $ ESMined tx
    Right NonceTaken -> settle rs reId $ ESReverted $ revertText RNInvalidNonce
    Right NonceFree -> whenFree

-- | Its own transactions first: a status-1 receipt answers its hash. Without one, the owner's nonce and the record.
judge :: Records -> RecordEdit -> IO (Either ChainError Judgment)
judge rs@Records {rsChain, rsContracts} RecordEdit {reId, reLabelHash, reOwner, reNonce, reChange} = runExceptT $ do
  txs <- liftIO $ withStore rs (`editTransactions` reId)
  receipts <- forM txs $ \NameTransaction {ntHash} -> (ntHash,) <$> ExceptT (transactionReceipt rsChain ntHash)
  case [h | (h, Just Receipt {receiptSuccess = True}) <- receipts] of
    h : _ -> pure $ Applied $ Just h
    [] -> do
      nonce <- ExceptT $ resolverNonce rsContracts reOwner
      if nonce <= reNonce
        then pure NonceFree
        else do
          holds <- case reChange of
            RCSetText key value -> (== value) <$> record (encodeUtf8 key)
            RCClear -> (&&) <$> (unset <$> record contactRecordKey) <*> (unset <$> record channelRecordKey)
          pure $ if holds then Applied Nothing else NonceTaken
  where
    record = ExceptT . textRecord rsContracts reLabelHash
    unset = (== "")

settle :: Records -> Int64 -> EditState -> IO ()
settle rs editId state = do
  void $ withStore rs $ \db -> settleRecordEdit db editId state
  settled rs editId

-- | Wakes the requests waiting for the edit, which read it again.
settled :: Records -> Int64 -> IO ()
settled Records {rsStalled, rsChanges} editId = atomically $ modifyTVar' rsStalled (S.delete editId) >> modifyTVar' rsChanges (+ 1)

-- | Logged once until the edit's next successful step.
stall :: Show e => Records -> Int64 -> e -> IO ()
stall Records {rsStalled, rsChanges} editId e = do
  new <- atomically $ do
    stalled <- readTVar rsStalled
    let new = not $ S.member editId stalled
    when new $ writeTVar rsStalled (S.insert editId stalled) >> modifyTVar' rsChanges (+ 1)
    pure new
  when new $ logWarn $ "names records: edit " <> tshow editId <> " waits for the chain: " <> tshow e

unstall :: Records -> Int64 -> IO ()
unstall Records {rsStalled} editId = atomically $ modifyTVar' rsStalled $ S.delete editId

-- | Runs the action only when nobody else holds the edit, else answers the fallback.
withClaim :: Records -> Int64 -> a -> IO a -> IO a
withClaim Records {rsClaimed} editId fallback action =
  bracket claim (\claimed -> when claimed release) $ \claimed -> if claimed then action else pure fallback
  where
    claim = atomically $ do
      held <- readTVar rsClaimed
      if S.member editId held then pure False else True <$ writeTVar rsClaimed (S.insert editId held)
    release = atomically $ modifyTVar' rsClaimed $ S.delete editId

editDigest :: Records -> EditRequest -> ByteString
editDigest rs EditRequest {erLabelHash, erChange, erNonce, erDeadline} = case erChange of
  RCSetText key value -> setTextDigest (domain rs) node key value erNonce erDeadline
  RCClear -> clearRecordsDigest (domain rs) node erNonce erDeadline
  where
    node = nodeOf rs erLabelHash

editCalldata :: Records -> RecordEdit -> ByteString
editCalldata rs RecordEdit {reLabelHash, reChange, reNonce, reDeadline, reSignature} = case reChange of
  RCSetText key value -> setTextWithSigCalldata node key value reNonce reDeadline reSignature
  RCClear -> clearRecordsWithSigCalldata node reNonce reDeadline reSignature
  where
    node = nodeOf rs reLabelHash

nodeOf :: Records -> LabelHash -> Node
nodeOf Records {rsTLD} = labelNode rsTLD

domain :: Records -> Eip712Domain
domain rs@Records {rsChain} = resolverDomain (cChainId $ ccConfig rsChain) (resolver rs)

resolver :: Records -> Address
resolver = cResolver . ccConfig . rsChain

signatureExpired :: Maybe Text
signatureExpired = revertText RNSignatureExpired

withStore :: Records -> (DB.Connection -> IO a) -> IO a
withStore Records {rsStore} = withTransaction rsStore
