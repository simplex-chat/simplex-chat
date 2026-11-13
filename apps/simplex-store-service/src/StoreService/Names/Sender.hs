{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Every transaction of the registrar key, sent once and in nonce order.
module StoreService.Names.Sender
  ( Sender,
    SendError (..),
    TxOutcome (..),
    PendingTx,
    ptHash,
    ptNonce,
    newSender,
    checkPendingSignatures,
    submitTransaction,
    inFlightTransaction,
    awaitOutcome,
    followTransactions,
    runSender,
    senderRound,
    replaceAfter,
    minTip,
    maxTxFeeWei,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (throwIO)
import Control.Logger.Simple (logError, logNote, logWarn)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (ChaChaDRG)
import Data.ByteString (ByteString)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime)
import Data.Word (Word64)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Chain
import StoreService.Names.Store (NameTransaction (..), NewTransaction (..), TxFor, TxStatus (..), insertTransaction, pendingTransactions, recordTransactionOutcome)
import StoreService.Names.View (addressText)
import StoreService.Safely (safely)
import StoreService.Store.Invoices (StoreDecodeError (..))

data Sender = Sender
  { snChain :: ChainClient,
    snRandom :: TVar ChaChaDRG,
    snStore :: DBStore,
    snNow :: IO UTCTime,
    -- | MVar wakes its waiters in FIFO order, so new transactions take their nonces in the order they came.
    snQueue :: MVar (),
    -- | Above every nonce this sender saved.
    snNextNonce :: TVar Word64,
    snInFlight :: TVar (Map Word64 InFlight)
  }

-- | The signatures for one registrar nonce: the first, then each fee-bumped replacement.
data InFlight = InFlight
  { ifFor :: TxFor,
    ifRequest :: TxRequest,
    ifRaw :: ByteString,
    -- | newest first
    ifHashes :: NonEmpty TxHash,
    ifReplaceAt :: UTCTime,
    -- | the block at which the nonce was first seen used with no receipt for any hash
    ifUsedSince :: Maybe Word64,
    -- | the last warning logged for this nonce, so a lasting failure is logged once
    ifLastWarning :: Maybe Text,
    ifOutcome :: TMVar TxOutcome
  }

-- | Nothing was signed or saved.
data SendError
  = -- | the dry run reverted, its gas is above the cap, or the chain did not answer
    SEChain ChainError
  | SESigning String
  | -- | this registration's or edit's transaction of this kind is still in flight
    SEInFlight
  | -- | the wei per gas the latest block needs is above the configured max fee per gas
    SEFeeAboveCap Integer
  deriving (Eq, Show)

-- | Settled at the configured confirmation depth.
data TxOutcome
  = TOConfirmed TxHash Receipt
  | -- | The registrar nonce is used and none of its hashes has a receipt: a pruned receipt or another transaction.
    -- The sender signs nothing more for it; its owner judges it from chain state.
    TONonceUsed
  deriving (Eq, Show)

data PendingTx = PendingTx
  { -- | the newest signature's hash
    ptHash :: TxHash,
    ptNonce :: Word64,
    ptOutcome :: TMVar TxOutcome
  }

-- | 3 blocks: a transaction still not mined by then is replaced if the market's fees have passed its own. A register's calldata
-- shows its label, so it must land well within the commitment's minimum age of about a minute.
replaceAfter :: NominalDiffTime
replaceAfter = 36

followInterval :: Int
followInterval = 4000000

-- | The max fee covers six full blocks in a row, each raising the base fee by 12.5%.
baseFeeHeadroom :: Integer
baseFeeHeadroom = 2

-- | 0.05 gwei, so quiet blocks whose median tip is near zero do not leave a transaction under every other.
minTip :: Word64
minTip = 50000000

-- | Nodes accept a replacement only with both fees at least 10% higher.
bumped :: Integer -> Integer
bumped fee = fee + fee `div` 8 + 1

-- | Resumes every saved transaction without a receipt. Owners resuming after a restart find theirs with 'inFlightTransaction'.
newSender :: TVar ChaChaDRG -> ChainClient -> DBStore -> IO UTCTime -> IO Sender
newSender snRandom snChain snStore snNow = do
  saved <- withTransaction snStore pendingTransactions
  inFlight <- M.fromList <$> mapM resumed (L.groupWith ntNonce saved)
  snQueue <- newMVar ()
  snNextNonce <- newTVarIO $ maybe 0 ((+ 1) . fst) $ M.lookupMax inFlight
  snInFlight <- newTVarIO inFlight
  pure Sender {snChain, snRandom, snStore, snNow, snQueue, snNextNonce, snInFlight}
  where
    resumed txs = do
      let NameTransaction {ntFor, ntNonce, ntRaw, ntSentAt} = L.last txs
      request <- either (throwIO . StoreDecodeError . ("name_transactions.raw: " <>) . T.pack) pure $ decodeSignedTx ntRaw
      outcome <- newEmptyTMVarIO
      pure
        ( ntNonce,
          InFlight
            { ifFor = ntFor,
              ifRequest = request,
              ifRaw = ntRaw,
              ifHashes = L.reverse $ L.map ntHash txs,
              ifReplaceAt = addUTCTime replaceAfter ntSentAt,
              ifUsedSince = Nothing,
              ifLastWarning = Nothing,
              ifOutcome = outcome
            }
        )

checkPendingSignatures :: TVar ChaChaDRG -> ChainConfig -> DBStore -> IO (Either String ())
checkPendingSignatures g ChainConfig {cChainId, cServiceKey} st = do
  registrar <- addressFromPrivateKey g cServiceKey
  saved <- withTransaction st pendingTransactions
  runExceptT $ forM_ saved $ \NameTransaction {ntNonce, ntRaw} -> do
    (chainId, signer) <- withExceptT ("name_transactions.raw: " <>) $ ExceptT $ transactionSigner ntRaw
    when (chainId /= cChainId) $ throwError $ refusal ntNonce ("for chain " <> show chainId) ("for chain.chain_id " <> show cChainId)
    when (signer /= registrar) $ throwError $ refusal ntNonce ("by " <> shownAddress signer) ("by " <> shownAddress registrar <> " of chain.service_key")
  where
    shownAddress = T.unpack . addressText
    refusal nonce signedFor configured =
      "a pending transaction of nonce " <> show nonce <> " was signed " <> signedFor <> ", not " <> configured
        <> "; start with the key and chain that signed it until no transaction is pending"

-- | Dry run, the next nonce, the market's fees, the signature, saved, then sent. A failed send is retried by 'followTransactions'.
submitTransaction :: Sender -> TxFor -> Address -> ByteString -> IO (Either SendError PendingTx)
submitTransaction s@Sender {snChain = c@ChainClient {ccRegistrar}, snRandom, snStore, snNow, snQueue, snNextNonce, snInFlight} for to calldata =
  withMVar snQueue $ \_ -> runExceptT $ do
    busy <- any ((== for) . ifFor) <$> liftIO (readTVarIO snInFlight)
    when busy $ throwError SEInFlight
    gas <- chain $ estimateGas c to calldata
    used <- chain $ transactionCount c ccRegistrar Latest
    nonce <- max used <$> liftIO (readTVarIO snNextNonce)
    market@(tip, baseFee) <- chain $ marketFees c
    let needed = baseFee + tip
        gasLimit = gasWithHeadroom c gas
        (cap, bounded) = feeCap c gasLimit
    when (needed > cap) $ do
      logWarn $ "names sender: the latest block needs " <> tshow needed <> " wei per gas, above the fee cap " <> tshow cap <> "; the transaction waits"
      throwError $ SEFeeAboveCap needed
    when (bounded && headroomFee market > cap) $
      logNote $ "names sender: nonce " <> tshow nonce <> " at max fee " <> tshow cap <> ", capped so that its gas limit " <> tshow gasLimit <> " costs at most " <> maxTxFeeText
    -- both fit Word64: the tip is at most the needed fee, and the max fee at most the cap
    let request = TxRequest {trTo = to, trData = calldata, trNonce = nonce, trGasLimit = gasLimit, trMaxFeePerGas = fromInteger $ min cap $ headroomFee market, trMaxPriorityFeePerGas = fromInteger tip}
    SignedTx {stRaw, stHash} <- withExceptT SESigning $ ExceptT $ signTransaction snRandom c request
    liftIO $ do
      now <- snNow
      saveTransaction snStore for stRaw stHash nonce now
      outcome <- newEmptyTMVarIO
      atomically $ do
        writeTVar snNextNonce $ nonce + 1
        modifyTVar' snInFlight $
          M.insert nonce InFlight {ifFor = for, ifRequest = request, ifRaw = stRaw, ifHashes = stHash :| [], ifReplaceAt = addUTCTime replaceAfter now, ifUsedSince = Nothing, ifLastWarning = Nothing, ifOutcome = outcome}
      broadcast s nonce stRaw
      pure PendingTx {ptHash = stHash, ptNonce = nonce, ptOutcome = outcome}
  where
    chain = withExceptT SEChain . ExceptT

inFlightTransaction :: Sender -> TxFor -> IO (Maybe PendingTx)
inFlightTransaction Sender {snInFlight} for =
  fmap pending . find ((== for) . ifFor . snd) . M.toList <$> readTVarIO snInFlight
  where
    pending (nonce, InFlight {ifHashes, ifOutcome}) = PendingTx {ptHash = L.head ifHashes, ptNonce = nonce, ptOutcome = ifOutcome}

-- | Nothing when the transaction is not settled within the microseconds.
awaitOutcome :: Int -> PendingTx -> IO (Maybe TxOutcome)
awaitOutcome micros PendingTx {ptOutcome} = do
  timer <- registerDelay micros
  atomically $ (Just <$> readTMVar ptOutcome) `orElse` (Nothing <$ (readTVar timer >>= check))

runSender :: Sender -> IO ()
runSender s = forever $ senderRound s >> threadDelay followInterval

-- | A round that fails is logged, so the lane goes on to the next one.
senderRound :: Sender -> IO ()
senderRound s = void $ safely "names sender: the round" (followTransactions s)

-- | One round over the transactions in flight, lowest nonce first. Only one thread may run it.
followTransactions :: Sender -> IO ()
followTransactions s@Sender {snChain = c@ChainClient {ccRegistrar}, snInFlight} = do
  inFlight <- readTVarIO snInFlight
  unless (M.null inFlight) $
    runExceptT ((,) <$> ExceptT (latestBlock c) <*> ExceptT (transactionCount c ccRegistrar Latest)) >>= \case
      Left e -> logWarn $ "names sender: " <> tshow e
      Right (Block {blockNumber}, used) -> forM_ (M.toAscList inFlight) $ uncurry $ follow s blockNumber used

follow :: Sender -> Word64 -> Word64 -> Word64 -> InFlight -> IO ()
follow s@Sender {snChain = c@ChainClient {ccConfig = ChainConfig {cConfirmations}}} latest used nonce tx@InFlight {ifHashes, ifUsedSince} =
  runExceptT (forM (L.toList ifHashes) $ \h -> (h,) <$> ExceptT (transactionReceipt c h)) >>= \case
    Left e -> warnNonce s nonce $ tshow e
    Right receipts -> case [(h, r) | (h, Just r) <- receipts] of
      (h, r@Receipt {receiptBlock}) : _
        | deep receiptBlock -> settle s nonce tx $ TOConfirmed h r
        | otherwise -> update s nonce $ \t -> t {ifUsedSince = Nothing}
      []
        | nonce < used -> do
            let since = fromMaybe latest ifUsedSince
            if deep since then settle s nonce tx TONonceUsed else update s nonce $ \t -> t {ifUsedSince = Just since}
        | otherwise -> do
            update s nonce $ \t -> t {ifUsedSince = Nothing}
            resend s nonce tx
  where
    deep block = latest + 1 >= block + fromIntegral cConfirmations

-- | A replacement when one is due and the market asks for it; otherwise the newest signature again if the node does not know it.
resend :: Sender -> Word64 -> InFlight -> IO ()
resend s@Sender {snChain = c, snNow} nonce tx@InFlight {ifRaw, ifHashes = newest :| _, ifReplaceAt, ifLastWarning} = do
  now <- snNow
  replaced <- if now >= ifReplaceAt then replace s nonce tx now else pure False
  unless replaced $
    transactionKnown c newest >>= \case
      Right False -> do
        when (isNothing ifLastWarning) $ logNote $ "names sender: rebroadcasting nonce " <> tshow nonce
        broadcast s nonce ifRaw
      Right True -> pure ()
      Left e -> warnNonce s nonce $ tshow e

replace :: Sender -> Word64 -> InFlight -> UTCTime -> IO Bool
replace s@Sender {snChain = c, snRandom, snStore} nonce InFlight {ifFor, ifRequest = old@TxRequest {trGasLimit, trMaxFeePerGas, trMaxPriorityFeePerGas}} now =
  marketFees c >>= \case
    Left e -> False <$ warnNonce s nonce (tshow e)
    Right market@(tip, _)
      | tip <= oldTip && headroomFee market <= oldMax -> False <$ update s nonce (\t -> t {ifReplaceAt = later})
      | bumped oldMax > cap -> do
          update s nonce $ \t -> t {ifReplaceAt = later}
          False <$ warnNonce s nonce ("not replaced: a replacement would pay over the fee cap " <> tshow cap)
      | otherwise -> do
          -- the bumped fees are at most the cap, so the replacement still outbids the old signature
          let wanted = max (headroomFee market) (bumped oldMax)
              maxFee' = min cap wanted
              tip' = min maxFee' $ max tip (bumped oldTip)
              request = old {trMaxPriorityFeePerGas = fromInteger tip', trMaxFeePerGas = fromInteger maxFee'}
          signTransaction snRandom c request >>= \case
            Left e -> False <$ logError ("names sender: replacement for nonce " <> tshow nonce <> " not signed: " <> T.pack e)
            Right SignedTx {stRaw, stHash} -> do
              saveTransaction snStore ifFor stRaw stHash nonce now
              update s nonce $ \t -> t {ifRequest = request, ifRaw = stRaw, ifHashes = stHash <| ifHashes t, ifReplaceAt = later}
              logNote $
                "names sender: replacing nonce " <> tshow nonce <> " at max fee " <> tshow maxFee'
                  <> (if bounded && wanted > cap then ", capped so that its gas limit " <> tshow trGasLimit <> " costs at most " <> maxTxFeeText else "")
              True <$ broadcast s nonce stRaw
  where
    later = addUTCTime replaceAfter now
    oldTip = toInteger trMaxPriorityFeePerGas
    oldMax = toInteger trMaxFeePerGas
    (cap, bounded) = feeCap c trGasLimit

-- | The confirmed hash gets its receipt's status; every other hash of the nonce can no longer be mined.
settle :: Sender -> Word64 -> InFlight -> TxOutcome -> IO ()
settle Sender {snStore, snInFlight} nonce InFlight {ifHashes, ifOutcome} outcome = do
  withTransaction snStore $ \db -> forM_ ifHashes $ \h -> recordTransactionOutcome db h TSPending (status h) (block h)
  when (outcome == TONonceUsed) $
    logWarn $ "names sender: nonce " <> tshow nonce <> " used with no receipt for its transactions; its owner judges it from chain state"
  atomically $ do
    modifyTVar' snInFlight $ M.delete nonce
    putTMVar ifOutcome outcome
  where
    status h = case outcome of
      TOConfirmed mined Receipt {receiptSuccess} | mined == h -> if receiptSuccess then TSMined else TSReverted
      _ -> TSDropped
    block h = case outcome of
      TOConfirmed mined Receipt {receiptBlock} | mined == h -> Just receiptBlock
      _ -> Nothing

update :: Sender -> Word64 -> (InFlight -> InFlight) -> IO ()
update Sender {snInFlight} nonce f = atomically $ modifyTVar' snInFlight $ M.adjust f nonce

-- | The recent blocks' median tip, at least minTip, and the latest base fee, as Integer so that no fee arithmetic wraps.
marketFees :: ChainClient -> IO (Either ChainError (Integer, Integer))
marketFees c = runExceptT $ do
  tip <- max minTip <$> ExceptT (recentTip c)
  Block {blockBaseFee} <- ExceptT $ latestBlock c
  pure (toInteger tip, toInteger blockBaseFee)

headroomFee :: (Integer, Integer) -> Integer
headroomFee (tip, baseFee) = baseFeeHeadroom * baseFee + tip

-- | A fifth above the estimate, within gas_limit_cap: state can change between the dry run and the block, and a transaction
-- that runs out of gas still pays for all of it.
gasWithHeadroom :: ChainClient -> Word64 -> Word64
gasWithHeadroom ChainClient {ccConfig = ChainConfig {cGasLimitCap}} gas = min cGasLimitCap (gas + gas `div` 5)

feeCap :: ChainClient -> Word64 -> (Integer, Bool)
feeCap ChainClient {ccConfig = ChainConfig {cMaxFeePerGas}} gasLimit = (min configured bound, bound < configured)
  where
    configured = toInteger cMaxFeePerGas
    bound = maxTxFeeWei `div` toInteger (max 1 gasLimit)

maxTxFeeWei :: Integer
maxTxFeeWei = 10_000_000_000_000_000

maxTxFeeText :: Text
maxTxFeeText = "0.01 ETH"

saveTransaction :: DBStore -> TxFor -> ByteString -> TxHash -> Word64 -> UTCTime -> IO ()
saveTransaction st for raw hash nonce now =
  withTransaction st $ \db -> void $ insertTransaction db for NewTransaction {ntxRaw = raw, ntxHash = hash, ntxNonce = nonce} now

broadcast :: Sender -> Word64 -> ByteString -> IO ()
broadcast s@Sender {snChain} nonce raw =
  sendRawTransaction snChain raw >>= \case
    Left e -> warnNonce s nonce $ "not sent: " <> tshow e
    Right _ -> update s nonce $ \t -> t {ifLastWarning = Nothing}

-- | Logged when it differs from the nonce's last warning. An underfunded key is an error, since only the operator can fund it.
warnNonce :: Sender -> Word64 -> Text -> IO ()
warnNonce Sender {snInFlight} nonce warning = do
  changed <- atomically $ do
    inFlight <- readTVar snInFlight
    case M.lookup nonce inFlight of
      Just tx@InFlight {ifLastWarning}
        | ifLastWarning == Just warning -> pure False
        | otherwise -> True <$ writeTVar snInFlight (M.insert nonce tx {ifLastWarning = Just warning} inFlight)
      Nothing -> pure True
  when changed $ (if underfundedMessage `T.isInfixOf` T.toLower warning then logError else logWarn) message
  where
    message = "names sender: nonce " <> tshow nonce <> ": " <> warning

-- | How geth, reth and erigon refuse a transaction whose key cannot pay gas × fee + value, in lower case; anvil capitalizes it.
underfundedMessage :: Text
underfundedMessage = "insufficient funds"
