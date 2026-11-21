{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesRecordsTests (namesRecordsTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (alice, gracePeriod, minPriceCentsPerYear, otherResolver, registry)
import Bots.StoreService.NamesStoreTests (t0)
import Bots.StoreService.WebTests (bounded, capturingStderr, refusedWith, renameTable, storeFixture, withServiceStore)
import qualified Bots.StoreService.WebTests as WT
import Bots.StoreService.WebUIMock (EphemeralWallet (..), newEphemeralWallet, walletFromCode)
import Control.Concurrent.Async (Async, async, cancel, concurrently, wait)
import Control.Concurrent.STM
import Control.Monad (forM, forM_, when)
import Crypto.Random (ChaChaDRG)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import qualified Data.ByteString.Lazy as LB
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import Network.HTTP.Client (RequestBody (..), defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseHeaders)
import Network.HTTP.Types (hContentType, statusCode)
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature (..), signRecoverable)
import Simplex.Messaging.Eth.Address (Address, recoverAddress)
import Simplex.Messaging.Encoding (Encoding (..))
import Simplex.Messaging.Eth.EIP712 (Eip712Domain)
import Simplex.Messaging.Eth.Transaction (Eip1559Tx (..))
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD (..))
import Simplex.Messaging.SystemTime (RoundedSystemTime (..), SystemSeconds)
import StoreService.Names.Chain
import StoreService.Names.Contracts (newContracts)
import StoreService.Names.Records
import StoreService.Names.Registration (LinkKind (..), Node, labelNode, linkMaxBytes, multicallCalldata, nodeBytes, parseSignature, resolverDomain, setTextDigest, setTextWithSigCalldata)
import StoreService.Names.Sender (Sender, followTransactions, newSender)
import StoreService.Names.Store (BodyHash, EditOutcome (..), EditState (..), NewRecordEdit (..), RecordChange (..), RecordEdit (..), SignedChange (..), getRecordEditByBody, insertRecordEdit)
import StoreService.Names.Turns (newTurns)
import StoreService.Web.Http (maxBodyBytes)
import StoreService.Web.Names.Records (EditInvalid (..), editRequest, editResponse, recordsHandler)
import System.Timeout (timeout)
import Test.Hspec
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (Only (..))
#else
import Database.SQLite.Simple (Only (..))
#endif

namesRecordsTests :: Spec
namesRecordsTests = describe "StoreService names: records" $ do
  -- the signatures and calldata marked viem are viem 2.57.5's, by the wallet of "abandon ... about" (account 0),
  -- with the SimplexResolver ABI of ens-contracts PR #36 at 23892cc
  g <- runIO C.newRandom
  describe "requests" $ do
    it "signs and encodes SetText edits as viem does" $ testViemVectors g
    it "encodes two edits as one multicall as viem does" $ testViemMulticall g
    it "reads a request's formats, then its link, then its signature" testRequestOrder
    it "takes one or two edits of distinct keys with consecutive nonces, and a clear only alone" testRequestEdits
    it "bounds a link at 1024 bytes and takes only a link of the key's kind" testLinkBound
  describe "relay" $ do
    it "relays an edit with viem's calldata and answers its hash once mined" $ testRelayMined g
    it "refuses an edit whose deadline is within ten minutes of the latest block, before any row or gas" $ testDeadlineMargin g
    it "refuses an edit of a name on another resolver, or past its expiry and grace, before any row or gas" $ testRefusedNameState g
    it "leaves the owner's cap untouched by ten edits a non-owner signs" $ testNonOwnerCap g
    it "checks a refused edit's repeat anew" $ testRefusedRepeat g
    it "refuses the eleventh relayed edit of a name within an hour, with the real wait" $ testCap g
    it "answers provider_unavailable and stores nothing when the chain fails before the send" $ testUnavailableBeforeSend g
    it "checks a dry-run-reverted edit's repeat anew" $ testDryRunRepeat g
    it "answers provider_unavailable and stores nothing when the re-read after a dry run's revert fails" $ testDryRunReReadFails g
    it "lets a concurrent twin lose the unique insert before any signing, and answers both" $ testConcurrentTwin g
    it "answers edit_pending at once when the chain stops answering after the send" $ testChainStopsAfterSend g
    it "logs a round whose read of the pending edits fails, and the next round follows them" $ testRoundFails g
    it "bounds the wait for edit_pending from the request's arrival, counting the checks and the send" $ testPendingFromArrival g
  describe "two edits" $ do
    it "relays two edits as one multicall transaction with one row, and answers its hash once mined" $ testTwoMined g
    it "refuses two edits when the second is signed by another key, or its nonce is not the owner's next, before any row or gas" $ testTwoRefused g
    it "answers an exact resend of two edits from the stored request, and checks either edit alone anew" $ testTwoResend g
    it "stores nothing when the dry run of two edits reverts, answering it with no name" $ testTwoDryRunReverted g
    it "settles two edits SignatureExpired once the earliest deadline is passed at depth" $ testTwoExpired g
    it "answers two edits applied with no hash only when the nonce passed both and both records hold their values" $ testTwoAppliedByAnother g
    it "answers two edits record_reverted InvalidNonce when another body took only the first nonce" $ testTwoFirstNonceTaken g
  describe "outcomes" $ do
    it "answers a revert of an edit another relayer applied, InvalidNonce or SignatureExpired past the deadline, as mined with no hash, its repeat too, and record_reverted InvalidNonce when another body used the nonce" $ testAppliedByAnother g
    it "answers a dry run's InvalidNonce revert of an edit another relayer applied as applied with no hash, storing nothing" $ testDryRunAppliedByAnother g
    it "frees a reverted edit's nonce for a new body, and answers the reverted edit's repeat while the new one is pending" $ testRevertFreesNonce g
    it "keeps an edit pending when the re-read after a mined revert fails, and settles it in a later round" $ testFailedReRead g
  describe "restarts" $ do
    it "sends a saved edit that was never sent, and follows a sent one to its receipt" $ testResume g
    it "forgets a saved edit that was never sent once its dry run reverts or its deadline passes" $ testUnsentForgotten g
    it "goes on to the next edit when one row fails to decode" $ testBadRowSkipped g
    it "answers an identical repeat after a restart from the stored edit, with no chain read" $ testRepeatAfterRestart g
    it "finds an edit that expired across a restart applied, and makes it mined" $ testRestartExpiredApplied g
    it "frees an edit past its deadline while its transaction is still rebroadcast" $ testExpiryFreesNonce g
  describe "yearly limit" $ do
    it "refuses a name's fourth edit within 365 days with when the first leaves, before any chain read, row or gas" $ testYearLimit g
    it "takes an edit again once the oldest counted one is 365 days old" $ testYearWindow g
    it "answers an accepted edit's resend as before once the limit is used up, counting it once" $ testYearResend g
    it "counts a pending edit and one reverted on chain, and no edit refused or never sent" $ testYearFailedRelays g
  describe "http" $ do
    it "answers the contract's statuses and bodies, and bad_request to a body over the cap" $ testHttp g
    it "answers 504 edit_pending, 503, 429 with the real wait in Retry-After, 409 edit_limit with nextAt and no Retry-After, 400 to a refusal and 409 stale_nonce" testEditResponses
    it "relays a body of two edits with links of the largest size" $ testHttpTwoLongLinks g

data Setup = Setup
  { node :: FakeEthNode,
    store :: DBStore,
    clock :: TVar UTCTime,
    chain :: ChainClient,
    random :: TVar ChaChaDRG,
    sender :: Sender,
    records :: Records,
    owner :: EphemeralWallet
  }

-- | A registered alicebakery on the default resolver, held by the viem wallet, its nonce 0 and no records; two confirmations.
withSetup :: TVar ChaChaDRG -> (Setup -> IO a) -> IO a
withSetup random action = withFakeEthNode $ \node -> withServiceStore $ \store -> do
  clock <- newTVarIO t0
  chain <- newChainClient random (fakeChainConfig node) {cConfirmations = 2}
  owner <- walletFromCode random viemCode
  (sender, records) <- start store clock chain random
  let s = Setup {node, store, clock, chain, random, sender, records, owner}
  answerName s
  action s

start :: DBStore -> TVar UTCTime -> ChainClient -> TVar ChaChaDRG -> IO (Sender, Records)
start store clock chain random = do
  sender <- newSender random chain store (readTVarIO clock)
  contracts <- newContracts store TLDSimplex minPriceCentsPerYear chain
  turns <- newTurns
  rs <- newRecords store turns contracts sender TLDSimplex editsPerYear (readTVarIO clock)
  pure (sender, rs {rsPendingAfter = longWait})

-- | A new sender and lane over the same store and node, as after the service restarts.
restart :: Setup -> IO Setup
restart s@Setup {store, clock, chain, random} = do
  (sender, records) <- start store clock chain random
  pure s {sender, records}

answerName :: Setup -> IO ()
answerName s@Setup {node, owner = EphemeralWallet {ewAddress}} = do
  answerCall node fakeBaseRegistrar (selector "ens()") $ Returns $ addressWord registry
  answerCall node fakeBaseRegistrar (selector "GRACE_PERIOD()") $ Returns $ uint256 gracePeriod
  answerCall node fakeBaseRegistrar (selector "nameExpires(uint256)" <> unLabel alice) $ Returns $ uint256 nameExpiry
  answerCall node registry (selector "owner(bytes32)" <> nodeBytes aliceNode) $ Returns $ addressWord ewAddress
  answerCall node registry (selector "resolver(bytes32)" <> nodeBytes aliceNode) $ Returns $ addressWord fakeResolver
  setNonce s 0
  setRecord s contactKey ""
  setRecord s channelKey ""

setNonce :: Setup -> Word64 -> IO ()
setNonce s@Setup {node} n = answerCall node fakeResolver (noncesCall s) $ Returns $ uint256 n

noncesCall :: Setup -> ByteString
noncesCall Setup {owner = EphemeralWallet {ewAddress}} = selector "nonces(address)" <> addressWord ewAddress

setRecord :: Setup -> Text -> Text -> IO ()
setRecord Setup {node} key value =
  answerCall node fakeResolver (selector "text(bytes32,string)" <> abiTuple [Static $ nodeBytes aliceNode, Dynamic $ abiBytes $ encodeUtf8 key]) $
    Returns $ abiTuple [Dynamic $ abiBytes $ encodeUtf8 value]

signChange :: Setup -> EphemeralWallet -> RecordChange -> Word64 -> Word64 -> IO SignedChange
signChange Setup {random} EphemeralWallet {ewKey} scChange scNonce dl = do
  let scDeadline = RoundedSystemTime $ fromIntegral dl
  scSignature <- signRecoverable random ewKey (digestOf scChange scNonce scDeadline) >>= either fail pure
  pure SignedChange {scChange, scNonce, scDeadline, scSignature}

-- | A request of one edit.
signEdit :: Setup -> EphemeralWallet -> RecordChange -> Word64 -> Word64 -> IO EditRequest
signEdit s w change nonce dl = editOf . (:| []) <$> signChange s w change nonce dl

editOf :: NonEmpty SignedChange -> EditRequest
editOf erChanges = EditRequest {erLabelHash = alice, erChanges}

-- | The owner's contact and channel edits, at this nonce and the next, in one request.
ownerTwo :: Setup -> Word64 -> Text -> Text -> IO EditRequest
ownerTwo s@Setup {owner} nonce contactValue channelValue = do
  first <- signChange s owner (contact contactValue) nonce deadline
  second <- signChange s owner (channel channelValue) (nonce + 1) deadline
  pure $ editOf $ first :| [second]

contact :: Text -> RecordChange
contact = RCSetText LKContact

channel :: Text -> RecordChange
channel = RCSetText LKChannel

-- | The owner's channel edit with nonce 0 and the default deadline.
ownerEdit :: Setup -> Text -> IO EditRequest
ownerEdit s@Setup {owner} value = signEdit s owner (channel value) 0 deadline

relay :: Setup -> EditRequest -> IO (Async EditAnswer)
relay Setup {records} = async . relayEdit records

-- | Mines the n-th transaction sent with this status, one block deeper, then runs the sender's round and the lane's.
mineSent :: Setup -> Int -> Bool -> IO TxHash
mineSent s@Setup {node, sender, records} n ok = do
  raws <- sentBy s n
  let h = C.keccak256 $ raws !! (n - 1)
  _ <- mine node [(h, ok)]
  _ <- mine node []
  followTransactions sender
  recordsRound records
  pure $ TxHash $ Hex32 h

-- | The transactions sent, once the request that sent the n-th has let go of its edit, since until then a round skips the edit.
sentBy :: Setup -> Int -> IO [ByteString]
sentBy Setup {node, records} n = bounded "the sent transactions" $ do
  raws <- awaitSent node n
  atomically $ readTVar (rsClaimed records) >>= check . S.null
  pure raws

answerOf :: Async EditAnswer -> IO EditAnswer
answerOf = bounded "the edit's answer" . wait

stateOf :: Setup -> EditRequest -> IO (Maybe EditState)
stateOf Setup {store} req = withTransaction store $ \db -> fmap reState <$> getRecordEditByBody db (bodyHashOf req)

editRows :: Setup -> IO Int
editRows Setup {store} = withTransaction store $ \db -> do
  [Only n] <- DB.query_ db "SELECT count(*) FROM sx_badge_service_name_record_edits"
  pure n

-- | As the relay keys a sent request: each signed digest and its signature, in order.
bodyHashOf :: EditRequest -> BodyHash
bodyHashOf EditRequest {erChanges} =
  Hex32 $ C.sha256Hash $ B.concat [digestOf scChange scNonce scDeadline <> smpEncode scSignature | SignedChange {scChange, scNonce, scDeadline, scSignature} <- L.toList erChanges]

-- | Saves an edit as a request does before it joins the sender, as if the service stopped right after.
saveUnsent :: Setup -> EditRequest -> IO ()
saveUnsent Setup {store, clock, owner = EphemeralWallet {ewAddress}} req@EditRequest {erChanges} = do
  now <- readTVarIO clock
  Just _ <- withTransaction store $ \db ->
    insertRecordEdit db NewRecordEdit {nreLabelHash = alice, nreOwner = ewAddress, nreChanges = erChanges, nreBodyHash = bodyHashOf req} now
  pure ()

passTime :: Setup -> NominalDiffTime -> IO ()
passTime Setup {clock} t = atomically $ modifyTVar' clock (addUTCTime t)

-- requests

testViemVectors :: TVar ChaChaDRG -> IO ()
testViemVectors g = withSetup g $ \s@Setup {owner = EphemeralWallet {ewAddress}} -> do
  ewAddress `shouldBe` viemAddress
  SignedChange {scSignature = setSig} <- signChange s (owner s) (channel viemChannelLink) 0 deadline
  setSig `shouldBe` viemSetSignature
  recoverAddress setSig (digestOf (channel viemChannelLink) 0 deadlineSeconds) `shouldReturn` Right viemAddress
  setTextWithSigCalldata aliceNode LKChannel viemChannelLink 0 deadlineSeconds setSig `shouldBe` viemSetCall

testViemMulticall :: TVar ChaChaDRG -> IO ()
testViemMulticall g = withSetup g $ \s -> do
  EditRequest {erChanges = first :| rest} <- ownerTwo s 0 viemContactLink viemChannelLink
  [second] <- pure rest
  (scSignature first, scSignature second) `shouldBe` (viemContactSignature, viemChannelSignature)
  multicallCalldata
    [ setTextWithSigCalldata aliceNode LKContact viemContactLink 0 deadlineSeconds (scSignature first),
      setTextWithSigCalldata aliceNode LKChannel viemChannelLink 1 deadlineSeconds (scSignature second)
    ]
    `shouldBe` viemMulticall

testRequestOrder :: IO ()
testRequestOrder = do
  let valid = editFields [("key", J.String channelKind), ("value", J.String viemChannelLink)]
      request = editRequest . requestOf . (: [])
  changesOf (request valid) `shouldBe` Right [channel viemChannelLink]
  request (editFields [("clear", J.Bool True)]) `shouldBe` Left EIBadRequest
  changesOf (request $ editFields [("key", J.String contactKind), ("value", "")]) `shouldBe` Right [contact ""]
  -- formats first: a bad label hash or key is bad_request even with a bad link
  editRequest (KM.insert "labelHash" (J.String $ "0x" <> T.toUpper (T.drop 2 aliceHashText)) $ requestOf [withLink "https://x/a"]) `shouldBe` Left EIBadRequest
  request (editFields [("key", "other"), ("value", J.String viemChannelLink)]) `shouldBe` Left EIBadRequest
  request (editFields [("key", J.String channelKey), ("value", J.String viemChannelLink)]) `shouldBe` Left EIBadRequest
  request (KM.insert "nonce" (J.Number (-1)) valid) `shouldBe` Left EIBadRequest
  request (KM.insert "deadline" (J.String "1767229200") valid) `shouldBe` Left EIBadRequest
  request (KM.delete "value" valid) `shouldBe` Left EIBadRequest
  -- the old body of one edit, with no list
  editRequest (KM.insert "labelHash" (J.String aliceHashText) valid) `shouldBe` Left EIBadRequest
  -- the link before the signature
  request (KM.insert "signature" "0x00" $ withLink "https://x/a") `shouldBe` Left EIInvalidLink
  request (KM.insert "signature" "0x00" valid) `shouldBe` Left EIBadRequest
  where
    withLink link = editFields [("key", J.String channelKind), ("value", J.String link)]

testRequestEdits :: IO ()
testRequestEdits = do
  let edit key value nonce = editFields [("key", J.String key), ("value", J.String value), ("nonce", J.toJSON (nonce :: Int))]
      clearAt nonce = editFields [("clear", J.Bool True), ("nonce", J.toJSON (nonce :: Int))]
      request = editRequest . requestOf
      contactAt = edit contactKind "https://smp8.simplex.im/a#Y8m3"
      channelAt = edit channelKind viemChannelLink
  changesOf (request [contactAt 4, channelAt 5]) `shouldBe` Right [contact "https://smp8.simplex.im/a#Y8m3", channel viemChannelLink]
  fmap (map scNonce . L.toList . erChanges) (request [channelAt 4, contactAt 5]) `shouldBe` Right [4, 5]
  forM_
    [ ("no edit", []),
      ("three edits", [contactAt 4, channelAt 5, edit channelKind "" 6]),
      ("one key twice", [channelAt 4, edit channelKind "" 5]),
      ("a clear", [clearAt 4]),
      ("a clear after a set", [contactAt 4, clearAt 5]),
      ("a nonce skipped", [contactAt 4, channelAt 6]),
      ("nonces reversed", [contactAt 5, channelAt 4]),
      ("one nonce twice", [contactAt 4, channelAt 4])
    ]
    $ \(what, edits) -> (what :: String, request edits) `shouldBe` (what, Left EIBadRequest)
  -- every edit's format before any link, and every link before any signature
  request [edit contactKind "https://x/a" 4, edit "other" viemChannelLink 5] `shouldBe` Left EIBadRequest
  request [KM.insert "signature" "0x00" $ contactAt 4, edit channelKind "https://x/c" 5] `shouldBe` Left EIInvalidLink

changesOf :: Either EditInvalid EditRequest -> Either EditInvalid [RecordChange]
changesOf = fmap (map scChange . L.toList . erChanges)

testLinkBound :: IO ()
testLinkBound = do
  let prefix = "https://smp8.simplex.im/c#"
      linkOf n = prefix <> T.replicate (n - T.length prefix) "A"
      edit key value = editRequest $ requestOf [editFields [("key", J.String key), ("value", J.String value)]]
  fmap (L.head . L.map scChange . erChanges) (edit channelKind $ linkOf linkMaxBytes) `shouldBe` Right (channel $ linkOf linkMaxBytes)
  edit channelKind (linkOf $ linkMaxBytes + 1) `shouldBe` Left EIInvalidLink
  edit contactKind viemChannelLink `shouldBe` Left EIInvalidLink
  edit channelKind "https://smp8.simplex.im/c#Y8m3;https://smp9.simplex.im/c#Z1" `shouldBe` Left EIInvalidLink

-- relay

testRelayMined :: TVar ChaChaDRG -> IO ()
testRelayMined g = withSetup g $ \s -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  [raw] <- sentBy s 1
  fmap (\Eip1559Tx {txTo, txData} -> (txTo, txData)) (decodeSignedTx raw) `shouldBe` Right (fakeResolver, viemSetCall)
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)
  stateOf s req `shouldReturn` Just (ESSettled $ EOApplied $ Just h)

testDeadlineMargin :: TVar ChaChaDRG -> IO ()
testDeadlineMargin g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  setBlock node 2 $ deadline - 600
  relayEdit records req `shouldReturn` EARefused "the deadline is too close to the latest block"
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []
  setBlock node 2 $ deadline - 601
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)

testRefusedNameState :: TVar ChaChaDRG -> IO ()
testRefusedNameState g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  answerCall node registry (selector "resolver(bytes32)" <> nodeBytes aliceNode) $ Returns $ addressWord otherResolver
  relayEdit records req `shouldReturn` EARefused "the name is on another resolver"
  answerCall node registry (selector "resolver(bytes32)" <> nodeBytes aliceNode) $ Returns $ addressWord fakeResolver
  -- the registry still names the owner, but the name lapsed: expiry and grace are before the latest block
  answerCall node fakeBaseRegistrar (selector "nameExpires(uint256)" <> unLabel alice) $ Returns $ uint256 (genesis - gracePeriod - 1)
  answerCall node fakeNamePolicy (selector "reservedNames(bytes32)" <> unLabel alice) $ Returns $ uint256 (0 :: Int)
  relayEdit records req `shouldReturn` EARefused "the name is not registered"
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []

testNonOwnerCap :: TVar ChaChaDRG -> IO ()
testNonOwnerCap g = withSetup g $ \s@Setup {records} -> do
  other <- newEphemeralWallet g
  forM_ [0 .. 9] $ \n -> (relayEdit records =<< signEdit s other (channel viemChannelLink) n deadline) `shouldReturn` EARefused "the signer does not own the name"
  editRows s `shouldReturn` 0
  r <- relay s =<< ownerEdit s viemChannelLink
  _ <- sentBy s 1
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)

testRefusedRepeat :: TVar ChaChaDRG -> IO ()
testRefusedRepeat g = withSetup g $ \s@Setup {owner} -> do
  req <- signEdit s owner (channel viemChannelLink) 1 deadline
  relayEdit (records s) req `shouldReturn` EAStaleNonce
  stateOf s req `shouldReturn` Nothing
  setNonce s 1
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)

testCap :: TVar ChaChaDRG -> IO ()
testCap g = withSetup g $ \s0@Setup {owner} -> do
  let s = roomy s0
      quick = s {records = (records s) {rsPendingAfter = shortWait}}
  forM_ [0 .. 9] $ \n -> do
    setNonce s n
    (relayEdit (records quick) =<< signEdit s owner (channel viemChannelLink) n deadline) `shouldReturn` EAPending
  setNonce s 10
  eleventh <- signEdit s owner (channel viemChannelLink) 10 deadline
  passTime s 1000
  relayEdit (records s) eleventh `shouldReturn` EACapped 2600
  editRows s `shouldReturn` 10
  passTime s 2601
  relayEdit (records quick) eleventh `shouldReturn` EAPending
  editRows s `shouldReturn` 11

testUnavailableBeforeSend :: TVar ChaChaDRG -> IO ()
testUnavailableBeforeSend g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  setFault node "eth_call" HttpFailure
  relayEdit records req `shouldReturn` EAUnavailable
  clearFault node "eth_call"
  setFault node "eth_feeHistory" HttpFailure
  relayEdit records req `shouldReturn` EAUnavailable
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []
  clearFault node "eth_feeHistory"
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)

testDryRunRepeat :: TVar ChaChaDRG -> IO ()
testDryRunRepeat g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  revertDryRun node viemSetCall $ Just $ selector "InvalidSignature()"
  relayEdit records req `shouldReturn` EASettled (EOReverted $ Just RNInvalidSignature)
  stateOf s req `shouldReturn` Nothing
  revertDryRun node viemSetCall Nothing
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)

testDryRunReReadFails :: TVar ChaChaDRG -> IO ()
testDryRunReReadFails g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  -- the nonce check reads it, the re-read after the revert finds no answer
  answerCallOnce node fakeResolver (noncesCall s) $ Returns $ uint256 (0 :: Int)
  forgetCall node fakeResolver (noncesCall s)
  revertDryRun node viemSetCall $ Just $ selector "InvalidSignature()"
  relayEdit records req `shouldReturn` EAUnavailable
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []

testConcurrentTwin :: TVar ChaChaDRG -> IO ()
testConcurrentTwin g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  -- both pass the repeat lookup before either inserts
  setFault node "eth_getBlockByNumber" $ Slow 300000
  answers <- async $ concurrently (relayEdit records req) (relayEdit records req)
  _ <- sentBy s 1
  clearFault node "eth_getBlockByNumber"
  h <- mineSent s 1 True
  bounded "the edits' answers" (wait answers) `shouldReturn` (EASettled (EOApplied $ Just h), EASettled (EOApplied $ Just h))
  length <$> sentTransactions node `shouldReturn` 1
  editRows s `shouldReturn` 1

testPendingFromArrival :: TVar ChaChaDRG -> IO ()
testPendingFromArrival g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  -- three block reads before the send: the deadline check, the name status and the market's fees
  setFault node "eth_getBlockByNumber" $ Slow slowBlock
  arrived <- getCurrentTime
  answer <- timeout (4 * waitFromArrival) $ relayEdit records {rsPendingAfter = waitFromArrival} req
  elapsed <- (`diffUTCTime` arrived) <$> getCurrentTime
  answer `shouldBe` Just EAPending
  length <$> sentTransactions node `shouldReturn` 1
  -- the checks and the send take about 4.5 s, so a wait started after them would end near 7 s
  elapsed `shouldSatisfy` (< 5.6)
  where
    slowBlock = 1500000
    waitFromArrival = 2500000

testRoundFails :: TVar ChaChaDRG -> IO ()
testRoundFails g = withSetup g $ \s@Setup {store, records} -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  _ <- sentBy s 1
  renameTable store "sx_badge_service_name_record_edits" "name_record_edits_away"
  logged <- capturingStderr $ recordsRound records
  renameTable store "name_record_edits_away" "sx_badge_service_name_record_edits"
  logged `shouldSatisfy` T.isInfixOf "names records: the round failed; the next pass will run: "
  h <- mineSent s 1 True
  relayEdit records req `shouldReturn` EASettled (EOApplied $ Just h)
  cancel r

testChainStopsAfterSend :: TVar ChaChaDRG -> IO ()
testChainStopsAfterSend g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  _ <- sentBy s 1
  setFault node "eth_getBlockByNumber" HttpFailure
  recordsRound records
  answerOf r `shouldReturn` EAPending
  relayEdit records req `shouldReturn` EAPending
  clearFault node "eth_getBlockByNumber"
  h <- mineSent s 1 True
  relayEdit records req `shouldReturn` EASettled (EOApplied $ Just h)

-- two edits

testTwoMined :: TVar ChaChaDRG -> IO ()
testTwoMined g = withSetup g $ \s@Setup {node} -> do
  req <- ownerTwo s 0 viemContactLink viemChannelLink
  r <- relay s req
  [raw] <- sentBy s 1
  fmap (\Eip1559Tx {txTo, txData} -> (txTo, txData)) (decodeSignedTx raw) `shouldBe` Right (fakeResolver, viemMulticall)
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)
  stateOf s req `shouldReturn` Just (ESSettled $ EOApplied $ Just h)
  editRows s `shouldReturn` 1
  length <$> sentTransactions node `shouldReturn` 1

testTwoRefused :: TVar ChaChaDRG -> IO ()
testTwoRefused g = withSetup g $ \s@Setup {node, records, owner} -> do
  other <- newEphemeralWallet g
  first <- signChange s owner (contact viemContactLink) 0 deadline
  stranger <- signChange s other (channel viemChannelLink) 1 deadline
  relayEdit records (editOf $ first :| [stranger]) `shouldReturn` EARefused "the changes are not signed by one key"
  passed <- signChange s owner (channel viemChannelLink) 1 (genesis - 1)
  relayEdit records (editOf $ first :| [passed]) `shouldReturn` EARefused "the deadline is too close to the latest block"
  setNonce s 1
  (relayEdit records =<< ownerTwo s 0 viemContactLink viemChannelLink) `shouldReturn` EAStaleNonce
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []

testTwoResend :: TVar ChaChaDRG -> IO ()
testTwoResend g = withSetup g $ \s@Setup {node, records} -> do
  req@EditRequest {erChanges = first :| rest} <- ownerTwo s 0 viemContactLink viemChannelLink
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)
  relayEdit records req `shouldReturn` EASettled (EOApplied $ Just h)
  editsLeftNow s `shouldReturn` EditsLeft (editsPerYear - 1) (Just $ addUTCTime year t0)
  -- either edit alone is another body, checked anew against the nonce both moved
  setNonce s 2
  relayEdit records (editOf $ first :| []) `shouldReturn` EAStaleNonce
  relayEdit records (editOf $ L.fromList rest) `shouldReturn` EAStaleNonce
  editRows s `shouldReturn` 1
  length <$> sentTransactions node `shouldReturn` 1

testTwoDryRunReverted :: TVar ChaChaDRG -> IO ()
testTwoDryRunReverted g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerTwo s 0 viemContactLink viemChannelLink
  revertDryRun node viemMulticall $ Just ""
  relayEdit records req `shouldReturn` EASettled (EOReverted Nothing)
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []
  revertDryRun node viemMulticall Nothing
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)

testTwoExpired :: TVar ChaChaDRG -> IO ()
testTwoExpired g = withSetup g $ \s@Setup {node, records, owner} -> do
  first <- signChange s owner (contact viemContactLink) 0 (deadline + 600)
  second <- signChange s owner (channel viemChannelLink) 1 deadline
  let req = editOf $ first :| [second]
  relayEdit records {rsPendingAfter = shortWait} req `shouldReturn` EAPending
  _ <- sentBy s 1
  -- the second edit's deadline, the earlier one, is passed at the latest block but not yet two confirmations deep
  setBlock node 300 deadline
  setBlock node 301 $ deadline + 1
  recordsRound records
  stateOf s req `shouldReturn` Just ESPending
  setBlock node 302 $ deadline + 2
  recordsRound records
  stateOf s req `shouldReturn` Just (ESSettled $ EOReverted $ Just RNSignatureExpired)
  editsLeftNow s `shouldReturn` EditsLeft (editsPerYear - 1) (Just $ addUTCTime year t0)

testTwoAppliedByAnother :: TVar ChaChaDRG -> IO ()
testTwoAppliedByAnother g =
  forM_ [(viemChannelLink, EASettled (EOApplied Nothing)), ("https://smp9.simplex.im/c#other", EASettled (EOReverted $ Just RNInvalidNonce))] $ \(held, answer) -> withSetup g $ \s -> do
    req <- ownerTwo s 0 viemContactLink viemChannelLink
    r <- relay s req
    _ <- sentBy s 1
    -- another relayer's transaction used both nonces first
    setNonce s 2
    setRecord s contactKey viemContactLink
    setRecord s channelKey held
    _ <- mineSent s 1 False
    answerOf r `shouldReturn` answer

testTwoFirstNonceTaken :: TVar ChaChaDRG -> IO ()
testTwoFirstNonceTaken g = withSetup g $ \s -> do
  req <- ownerTwo s 0 viemContactLink viemChannelLink
  r <- relay s req
  _ <- sentBy s 1
  -- the records happen to hold both values, but only the first nonce moved, so the request cannot land
  setNonce s 1
  setRecord s contactKey viemContactLink
  setRecord s channelKey viemChannelLink
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EASettled (EOReverted $ Just RNInvalidNonce)
  stateOf s req `shouldReturn` Just (ESSettled $ EOReverted $ Just RNInvalidNonce)

-- | The name's yearly edits left now, by the relay's clock.
editsLeftNow :: Setup -> IO EditsLeft
editsLeftNow Setup {store, clock} = readTVarIO clock >>= \now -> withTransaction store $ \db -> nameEditsLeft db editsPerYear alice now

-- outcomes

testAppliedByAnother :: TVar ChaChaDRG -> IO ()
testAppliedByAnother g =
  forM_
    [ (False, viemChannelLink, EASettled (EOApplied Nothing), ESSettled (EOApplied Nothing), Just (EASettled (EOApplied Nothing))),
      (True, viemChannelLink, EASettled (EOApplied Nothing), ESSettled (EOApplied Nothing), Nothing),
      (False, "https://smp9.simplex.im/c#other", EASettled (EOReverted $ Just RNInvalidNonce), ESSettled (EOReverted $ Just RNInvalidNonce), Nothing)
    ]
    $ \(pastDeadline, value, answer, state, repeated) -> withSetup g $ \s@Setup {node, records} -> do
      req <- ownerEdit s viemChannelLink
      r <- relay s req
      _ <- sentBy s 1
      -- mined in the block after the deadline, it reverted SignatureExpired
      when pastDeadline $ setBlock node 300 deadline
      -- another relayer's transaction mined first
      setNonce s 1
      setRecord s channelKey value
      _ <- mineSent s 1 False
      answerOf r `shouldReturn` answer
      stateOf s req `shouldReturn` Just state
      forM_ repeated $ \a -> relayEdit records req `shouldReturn` a

testDryRunAppliedByAnother :: TVar ChaChaDRG -> IO ()
testDryRunAppliedByAnother g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  -- the nonce check reads 0; another relayer applies the edit before the dry run
  answerCallOnce node fakeResolver (noncesCall s) $ Returns $ uint256 (0 :: Int)
  setNonce s 1
  setRecord s channelKey viemChannelLink
  revertDryRun node viemSetCall $ Just $ selector "InvalidNonce()"
  relayEdit records req `shouldReturn` EASettled (EOApplied Nothing)
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []

testRevertFreesNonce :: TVar ChaChaDRG -> IO ()
testRevertFreesNonce g = withSetup g $ \s@Setup {records} -> do
  first <- ownerEdit s viemChannelLink
  r <- relay s first
  -- reverted before the deadline with the nonce unused, so the revert has no name
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EASettled (EOReverted Nothing)
  second <- ownerEdit s "https://smp9.simplex.im/c#second"
  r' <- relay s second
  _ <- sentBy s 2
  relayEdit records first `shouldReturn` EASettled (EOReverted Nothing)
  stateOf s second `shouldReturn` Just ESPending
  h <- mineSent s 2 True
  answerOf r' `shouldReturn` EASettled (EOApplied $ Just h)

testFailedReRead :: TVar ChaChaDRG -> IO ()
testFailedReRead g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  _ <- sentBy s 1
  setFault node "eth_call" HttpFailure
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAPending
  stateOf s req `shouldReturn` Just ESPending
  relayEdit records req `shouldReturn` EAPending
  clearFault node "eth_call"
  recordsRound records
  stateOf s req `shouldReturn` Just (ESSettled (EOReverted Nothing))
  relayEdit records req `shouldReturn` EASettled (EOReverted Nothing)

testResume :: TVar ChaChaDRG -> IO ()
testResume g = withSetup g $ \s@Setup {node, owner} -> do
  unsent <- ownerEdit s viemChannelLink
  saveUnsent s unsent
  s' <- restart s
  recordsRound (records s')
  h <- mineSent s' 1 True
  stateOf s unsent `shouldReturn` Just (ESSettled $ EOApplied $ Just h)
  -- a sent edit whose transaction the node forgot while the service was down
  setNonce s 1
  sent <- signEdit s owner (channel "https://smp9.simplex.im/c#next") 1 deadline
  _ <- relayEdit (records s') {rsPendingAfter = shortWait} sent
  [_, raw] <- sentBy s' 2
  dropTransaction node $ C.keccak256 raw
  s'' <- restart s'
  followTransactions (sender s'')
  (!! 2) <$> sentTransactions node `shouldReturn` raw
  h' <- mineSent s'' 3 True
  stateOf s sent `shouldReturn` Just (ESSettled $ EOApplied $ Just h')
  TxHash (Hex32 $ C.keccak256 raw) `shouldBe` h'

testBadRowSkipped :: TVar ChaChaDRG -> IO ()
testBadRowSkipped g = withSetup g $ \s@Setup {store, clock} -> do
  now <- readTVarIO clock
  -- a text record of a key no edit can have
  withTransaction store $ \db -> do
    DB.execute
      db
      "INSERT INTO sx_badge_service_name_record_edits (label_hash, owner, nonce, body_hash, state, created_at) VALUES (?,?,5,?,'pending',?)"
      (alice, viemAddress, Binary $ B.replicate 32 0, now)
    DB.execute
      db
      "INSERT INTO sx_badge_service_name_record_changes (name_record_edit_id, nonce, record_key, record_value, deadline, signature) SELECT name_record_edit_id, 5, 'simplex.other', '', ?, ? FROM sx_badge_service_name_record_edits WHERE nonce = 5"
      (deadline, viemSetSignature)
  req <- ownerEdit s viemChannelLink
  saveUnsent s req
  s' <- restart s
  recordsRound (records s')
  h <- mineSent s' 1 True
  stateOf s req `shouldReturn` Just (ESSettled $ EOApplied $ Just h)

testUnsentForgotten :: TVar ChaChaDRG -> IO ()
testUnsentForgotten g = withSetup g $ \s@Setup {node, owner} -> do
  reverting <- ownerEdit s viemChannelLink
  saveUnsent s reverting
  revertDryRun node viemSetCall $ Just $ selector "InvalidSignature()"
  s' <- restart s
  recordsRound (records s')
  stateOf s reverting `shouldReturn` Nothing
  late <- signEdit s owner (channel viemChannelLink) 0 (deadline + 1)
  saveUnsent s late
  setBlock node 2 $ deadline + 1
  recordsRound (records s')
  stateOf s late `shouldReturn` Nothing
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []

testRepeatAfterRestart :: TVar ChaChaDRG -> IO ()
testRepeatAfterRestart g = withSetup g $ \s@Setup {node} -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)
  s' <- restart s
  setFault node "eth_call" HttpFailure
  setFault node "eth_getBlockByNumber" HttpFailure
  relayEdit (records s') req `shouldReturn` EASettled (EOApplied $ Just h)
  length <$> sentTransactions node `shouldReturn` 1

testRestartExpiredApplied :: TVar ChaChaDRG -> IO ()
testRestartExpiredApplied g = withSetup g $ \s@Setup {node} -> do
  req <- ownerEdit s viemChannelLink
  relayEdit (records s) {rsPendingAfter = shortWait} req `shouldReturn` EAPending
  _ <- sentBy s 1
  -- while the service is down the deadline passes, and another relayer applies the edit
  setBlock node 300 $ deadline + 1
  setBlock node 301 $ deadline + 2
  setNonce s 1
  setRecord s channelKey viemChannelLink
  s' <- restart s
  recordsRound (records s')
  stateOf s req `shouldReturn` Just (ESSettled (EOApplied Nothing))
  relayEdit (records s') req `shouldReturn` EASettled (EOApplied Nothing)

testExpiryFreesNonce :: TVar ChaChaDRG -> IO ()
testExpiryFreesNonce g = withSetup g $ \s@Setup {node, records, sender} -> do
  req <- ownerEdit s viemChannelLink
  relayEdit records {rsPendingAfter = shortWait} req `shouldReturn` EAPending
  [raw] <- sentBy s 1
  -- the latest block is past the deadline, but not yet the block two confirmations deep
  setBlock node 300 deadline
  setBlock node 301 $ deadline + 1
  recordsRound records
  stateOf s req `shouldReturn` Just ESPending
  setBlock node 302 $ deadline + 2
  recordsRound records
  stateOf s req `shouldReturn` Just (ESSettled $ EOReverted $ Just RNSignatureExpired)
  relayEdit records req `shouldReturn` EASettled (EOReverted $ Just RNSignatureExpired)
  dropTransaction node $ C.keccak256 raw
  followTransactions sender
  sentTransactions node `shouldReturn` [raw, raw]

-- yearly limit

testYearLimit :: TVar ChaChaDRG -> IO ()
testYearLimit g = withSetup g $ \s@Setup {node, records, owner} -> do
  _ <- appliedEdits s 3
  setNonce s 3
  fourth <- signEdit s owner (channel viemChannelLink) 3 deadline
  -- any chain read would answer provider_unavailable
  setFault node "eth_call" HttpFailure
  setFault node "eth_getBlockByNumber" HttpFailure
  relayEdit records fourth `shouldReturn` EALimited (addUTCTime year t0)
  editRows s `shouldReturn` 3
  length <$> sentTransactions node `shouldReturn` 3

testYearWindow :: TVar ChaChaDRG -> IO ()
testYearWindow g = withSetup g $ \s@Setup {records, owner} -> do
  -- at t0, a day later and two days later
  _ <- appliedEdits s 3
  setNonce s 3
  fourth <- signEdit s owner (channel viemChannelLink) 3 deadline
  passTime s $ year - 2 * day - 1
  relayEdit records fourth `shouldReturn` EALimited (addUTCTime year t0)
  passTime s 1
  r <- relay s fourth
  h <- mineSent s 4 True
  answerOf r `shouldReturn` EASettled (EOApplied $ Just h)
  setNonce s 4
  fifth <- signEdit s owner (channel "https://smp9.simplex.im/c#fifth") 4 deadline
  relayEdit records fifth `shouldReturn` EALimited (addUTCTime (year + day) t0)
  editRows s `shouldReturn` 4

testYearResend :: TVar ChaChaDRG -> IO ()
testYearResend g = withSetup g $ \s@Setup {node, records} -> do
  sent <- appliedEdits s 3
  forM_ sent $ \(req, h) -> relayEdit records req `shouldReturn` EASettled (EOApplied $ Just h)
  editRows s `shouldReturn` 3
  length <$> sentTransactions node `shouldReturn` 3

testYearFailedRelays :: TVar ChaChaDRG -> IO ()
testYearFailedRelays g = withSetup g $ \s@Setup {node, records, owner} -> do
  -- reverted on chain, its nonce unused, and its gas paid
  r <- relay s =<< ownerEdit s viemChannelLink
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EASettled (EOReverted Nothing)
  editsLeftNow s `shouldReturn` EditsLeft (editsPerYear - 1) (Just $ addUTCTime year t0)
  -- refused: another signer, then a passed deadline
  other <- newEphemeralWallet g
  (relayEdit records =<< signEdit s other (channel viemChannelLink) 0 deadline) `shouldReturn` EARefused "the signer does not own the name"
  (relayEdit records =<< signEdit s owner (channel viemChannelLink) 0 (genesis - 1)) `shouldReturn` EARefused "the deadline is too close to the latest block"
  -- never sent: the dry run reverted, then the fee read failed
  setFault node "eth_estimateGas" $ revertFault $ selector "InvalidSignature()"
  (relayEdit records =<< signEdit s owner (channel "https://smp9.simplex.im/c#dry") 0 deadline) `shouldReturn` EASettled (EOReverted $ Just RNInvalidSignature)
  clearFault node "eth_estimateGas"
  setFault node "eth_feeHistory" HttpFailure
  (relayEdit records =<< signEdit s owner (channel "https://smp9.simplex.im/c#fee") 0 deadline) `shouldReturn` EAUnavailable
  clearFault node "eth_feeHistory"
  -- with the reverted one, one mined and one pending use the limit up
  _ <- appliedEditsFrom s 0 1 1
  setNonce s 1
  unsettled <- signEdit s owner (channel "https://smp9.simplex.im/c#pending") 1 deadline
  relayEdit records {rsPendingAfter = shortWait} unsettled `shouldReturn` EAPending
  setNonce s 2
  fourth <- signEdit s owner (channel "https://smp9.simplex.im/c#fourth") 2 deadline
  relayEdit records fourth `shouldReturn` EALimited (addUTCTime year t0)
  -- the owner spent the pending edit's nonce on another body, so it reverts InvalidNonce, and still counts
  _ <- mineSent s 3 False
  stateOf s unsettled `shouldReturn` Just (ESSettled $ EOReverted $ Just RNInvalidNonce)
  relayEdit records fourth `shouldReturn` EALimited (addUTCTime year t0)
  editRows s `shouldReturn` 3

-- | The owner's channel edits from nonce 0, each relayed, mined and answered, a day apart from t0; their requests and hashes.
appliedEdits :: Setup -> Int -> IO [(EditRequest, TxHash)]
appliedEdits s = appliedEditsFrom s 0 0

-- | As 'appliedEdits', from a nonce, after this many transactions were sent.
appliedEditsFrom :: Setup -> Word64 -> Int -> Int -> IO [(EditRequest, TxHash)]
appliedEditsFrom s@Setup {owner} from sentBefore n =
  forM [0 .. n - 1] $ \i -> do
    let nonce = from + fromIntegral i
    when (i > 0) $ passTime s day
    setNonce s nonce
    req <- signEdit s owner (channel $ "https://smp9.simplex.im/c#e" <> T.pack (show nonce)) nonce deadline
    r <- relay s req
    h <- mineSent s (sentBefore + i + 1) True
    answerOf r `shouldReturn` EASettled (EOApplied $ Just h)
    pure (req, h)

-- | Room in the yearly limit for the tests of the hourly cap.
roomy :: Setup -> Setup
roomy s = s {records = (records s) {rsEditsPerYear = 100}}

day, year :: NominalDiffTime
day = 86400
year = 365 * day

yearSeconds :: Int
yearSeconds = 365 * 86400

-- http

testHttp :: TVar ChaChaDRG -> IO ()
testHttp g = withSetup g $ \s@Setup {records} -> withRecordsApp records $ \post -> do
  let editBody value = J.encode $ requestOf [editFields [("key", J.String channelKind), ("value", J.String value)]]
  post "[]" `shouldReturn` (refusedWith 400 "bad_request", Nothing)
  post (editBody "https://x/a") `shouldReturn` ((400, Just $ KM.fromList [("type", "invalid_link"), ("kind", J.Null)]), Nothing)
  let overCap = KM.insert "pad" (J.String $ T.replicate maxBodyBytes "a") $ requestOf [editFields [("key", J.String channelKind), ("value", J.String viemChannelLink)]]
  post (J.encode overCap) `shouldReturn` (refusedWith 400 "bad_request", Nothing)
  editRows s `shouldReturn` 0
  answer <- async $ post $ editBody viemChannelLink
  TxHash (Hex32 h) <- mineSent s 1 True
  bounded "the edit's answer" (wait answer) `shouldReturn` ((200, Just $ KM.fromList ["tx" .= hex h]), Nothing)
  setNonce s 1
  next <- async . post . J.encode . editJson =<< signEdit s (owner s) (channel "https://smp9.simplex.im/c#next") 1 deadline
  _ <- mineSent s 2 False
  bounded "the edit's answer" (wait next) `shouldReturn` (refusedWith 409 "record_reverted", Nothing)

testEditResponses :: IO ()
testEditResponses = do
  let tx = TxHash $ Hex32 $ B.replicate 32 0xab
      answers =
        [ ("records-applied", EASettled $ EOApplied $ Just tx),
          ("records-reverted", EASettled $ EOReverted Nothing),
          ("records-pending", EAPending),
          ("records-unavailable", EAUnavailable),
          ("records-rate-limited", EACapped 3600),
          ("records-edit-limit", EALimited (addUTCTime (fromIntegral yearSeconds) t0)),
          ("records-bad-request", EARefused "the name is not registered"),
          ("records-stale-nonce", EAStaleNonce)
        ]
  parts <- mapM (responseParts . editResponse . snd) answers
  forM_ (zip answers parts) $ \((name, _), ((status, body), retryAfter)) -> storeFixture name status (J.Object <$> body) retryAfter
  parts
    `shouldBe` [ ((200, Just $ KM.fromList ["tx" .= tx]), Nothing),
                 (refusedWith 409 "record_reverted", Nothing),
                 (refusedWith 504 "edit_pending", Nothing),
                 (refusedWith 503 "provider_unavailable", Nothing),
                 ((429, Just $ KM.fromList ["type" .= ("rate_limited" :: Text), "retryAfter" .= (3600 :: Int)]), Just "3600"),
                 ((409, Just $ KM.fromList ["type" .= ("edit_limit" :: Text), "nextAt" .= addUTCTime (fromIntegral yearSeconds) t0]), Nothing),
                 (refusedWith 400 "bad_request", Nothing),
                 (refusedWith 409 "stale_nonce", Nothing)
               ]

-- | A response's status, JSON body and Retry-After, as withRecordsApp reads them over HTTP.
responseParts :: W.Response -> IO ((Int, Maybe J.Object), Maybe ByteString)
responseParts r = do
  let (status, headers, withBody) = W.responseToStream r
  chunks <- newTVarIO mempty
  withBody $ \stream -> stream (\b -> atomically $ modifyTVar' chunks (<> b)) (pure ())
  bytes <- toLazyByteString <$> readTVarIO chunks
  pure ((statusCode status, J.decode bytes), lookup "Retry-After" headers)

testHttpTwoLongLinks :: TVar ChaChaDRG -> IO ()
testHttpTwoLongLinks g = withSetup g $ \s@Setup {records} -> withRecordsApp records $ \post -> do
  let longLink prefix = prefix <> T.replicate (linkMaxBytes - T.length prefix) "A"
  body <- J.encode . editJson <$> ownerTwo s 0 (longLink "https://smp8.simplex.im/a#") (longLink "https://smp8.simplex.im/c#")
  answer <- async $ post body
  TxHash (Hex32 h) <- mineSent s 1 True
  bounded "the edit's answer" (wait answer) `shouldReturn` ((200, Just $ KM.fromList ["tx" .= hex h]), Nothing)

-- | Posts a body to the records handler, answering the status, the JSON body and Retry-After.
withRecordsApp :: Records -> ((LB.ByteString -> IO ((Int, Maybe J.Object), Maybe ByteString)) -> IO a) -> IO a
withRecordsApp rs action = Warp.testWithApplication (pure $ recordsHandler rs) $ \port -> do
  manager <- newManager defaultManagerSettings
  action $ \body -> do
    req <- parseRequest $ "http://127.0.0.1:" <> show port <> "/api/name/records"
    resp <- httpLbs req {method = "POST", requestHeaders = [(hContentType, "application/json")], requestBody = RequestBodyLBS body} manager
    pure (WT.answerOf resp, lookup "Retry-After" $ responseHeaders resp)

-- fixtures

editJson :: EditRequest -> J.Object
editJson EditRequest {erChanges} = requestOf $ map changeJson $ L.toList erChanges

changeJson :: SignedChange -> J.Object
changeJson SignedChange {scChange = RCSetText key value, scNonce, scDeadline, scSignature} =
  KM.fromList
    [ ("nonce", J.toJSON scNonce),
      ("deadline", J.toJSON scDeadline),
      ("signature", J.String $ hex $ smpEncode scSignature),
      ("key", J.toJSON key),
      ("value", J.String value)
    ]

-- | The body of a request with these edits for alicebakery.
requestOf :: [J.Object] -> J.Object
requestOf edits = KM.fromList [("labelHash", J.String aliceHashText), ("edits", J.toJSON edits)]

-- | The service's default.
editsPerYear :: Int
editsPerYear = 3

longWait, shortWait :: Int
longWait = 4000000
shortWait = 200000

-- | The fields of viem's SetText edit but its key and value, with these added or replaced.
editFields :: [(J.Key, J.Value)] -> J.Object
editFields extra = KM.union (KM.fromList extra) $ KM.delete "key" $ KM.delete "value" $ changeJson viemChange

viemChange :: SignedChange
viemChange = SignedChange {scChange = channel viemChannelLink, scNonce = 0, scDeadline = deadlineSeconds, scSignature = viemSetSignature}

digestOf :: RecordChange -> Word64 -> SystemSeconds -> ByteString
digestOf (RCSetText key value) nonce dl = setTextDigest resolverDomain' aliceNode key value nonce dl

resolverDomain' :: Eip712Domain
resolverDomain' = resolverDomain fakeChainId fakeResolver

aliceNode :: Node
aliceNode = labelNode TLDSimplex alice

aliceHashText :: Text
aliceHashText = hex $ unLabel alice

unLabel :: LabelHash -> ByteString
unLabel (LabelHash h) = h

contactKey, channelKey :: Text
contactKey = "simplex.contact"
channelKey = "simplex.channel"

contactKind, channelKind :: Text
contactKind = "contact"
channelKind = "channel"

-- | An hour after the fake node's first block.
deadline :: Word64
deadline = 1767229200

deadlineSeconds :: SystemSeconds
deadlineSeconds = RoundedSystemTime $ fromIntegral deadline

nameExpiry :: Word64
nameExpiry = 1861833600

-- | The fake node's first block time.
genesis :: Word64
genesis = deadline - 3600

-- | The name code of BIP-39 entropy zero, the wallet of "abandon ... about".
viemCode :: Text
viemCode = "SN-00000-00000-00000-00000-00000-0VME2"

viemAddress :: Address
viemAddress = address "0x9858effd232b4033e47d90003d41ec34ecaeda94"

viemChannelLink :: Text
viemChannelLink = "https://smp8.simplex.im/c#Y8m3"

-- | viem: SetText(alicebakery.simplex, simplex.channel, viemChannelLink, 0, deadline)
viemSetSignature :: RecoverableSignature
viemSetSignature = either (error . show) id $ parseSignature "0xfce4c6865591609c07ef3d94f376187d8a20cd9d94b4affcb60b55e999038fbd4e45c42b5fafe66a8d4f756e99398602eacef777b4dce4afd0a767cddadf3ca91c"

viemContactLink :: Text
viemContactLink = "https://smp8.simplex.im/a#Y8m3"

-- | viem: SetText(alicebakery.simplex, simplex.contact, viemContactLink, 0, deadline)
viemContactSignature :: RecoverableSignature
viemContactSignature = either (error . show) id $ parseSignature "0x2154ef3fbd7244fe34a2c9b2850d44eda1181d1e234900e8419518cab458848f08b5033fd3cd37e7be2346d81266684b4ced6275e84d22829fd0aaa836fae8261c"

-- | viem: SetText(alicebakery.simplex, simplex.channel, viemChannelLink, 1, deadline)
viemChannelSignature :: RecoverableSignature
viemChannelSignature = either (error . show) id $ parseSignature "0xaecc4fa43c1387e50594ceb43365fe9028a546c63f2ebc3d9d266f992d0b493808009060f7b6831a97b81453ccfb71413e608d48d8eddc7be3c086887b0b46481b"

-- | viem: multicall([setTextWithSig of the contact edit, setTextWithSig of the channel edit])
viemMulticall :: ByteString
viemMulticall = unhex "ac9650d8000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000020000000000000000000000000000000000000000000000000000000000000040000000000000000000000000000000000000000000000000000000000000024000000000000000000000000000000000000000000000000000000000000001c471a218a70f4104919a21c22f566d9c7947c7c7d8addbf546c973851a7e449276ea54ec1a00000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006955c7100000000000000000000000000000000000000000000000000000000000000140000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e636f6e746163740000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e68747470733a2f2f736d70382e73696d706c65782e696d2f612359386d33000000000000000000000000000000000000000000000000000000000000000000412154ef3fbd7244fe34a2c9b2850d44eda1181d1e234900e8419518cab458848f08b5033fd3cd37e7be2346d81266684b4ced6275e84d22829fd0aaa836fae8261c000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001c471a218a70f4104919a21c22f566d9c7947c7c7d8addbf546c973851a7e449276ea54ec1a00000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000006955c7100000000000000000000000000000000000000000000000000000000000000140000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e6368616e6e656c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e68747470733a2f2f736d70382e73696d706c65782e696d2f632359386d3300000000000000000000000000000000000000000000000000000000000000000041aecc4fa43c1387e50594ceb43365fe9028a546c63f2ebc3d9d266f992d0b493808009060f7b6831a97b81453ccfb71413e608d48d8eddc7be3c086887b0b46481b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

viemSetCall :: ByteString
viemSetCall = unhex "71a218a70f4104919a21c22f566d9c7947c7c7d8addbf546c973851a7e449276ea54ec1a00000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006955c7100000000000000000000000000000000000000000000000000000000000000140000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e6368616e6e656c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e68747470733a2f2f736d70382e73696d706c65782e696d2f632359386d3300000000000000000000000000000000000000000000000000000000000000000041fce4c6865591609c07ef3d94f376187d8a20cd9d94b4affcb60b55e999038fbd4e45c42b5fafe66a8d4f756e99398602eacef777b4dce4afd0a767cddadf3ca91c00000000000000000000000000000000000000000000000000000000000000"
