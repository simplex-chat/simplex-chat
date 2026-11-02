{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.NamesRecordsTests (namesRecordsTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesStoreTests (t0)
import Bots.StoreService.WebTests (withServiceStore)
import Bots.StoreService.WebUIMock (EphemeralWallet (..), newEphemeralWallet, walletFromCode)
import Control.Concurrent.Async (Async, async, concurrently, wait)
import Control.Concurrent.STM
import Control.Monad (forM, forM_, when)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import qualified Crypto.PubKey.ECC.Types as ECC
import Crypto.Random (ChaChaDRG)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.ByteArray.Encoding (Base (Base16), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.HTTP.Client (RequestBody (..), defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseBody, responseHeaders, responseStatus)
import Network.HTTP.Types (hContentType, statusCode)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import Simplex.Messaging.Agent.Store.DB (Binary (..))
import qualified Simplex.Messaging.Agent.Store.DB as DB
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature (..), signRecoverable)
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address, recoverAddress, unAddress)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain, serializeSignature)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD (..), labelHash)
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Abi
import StoreService.Names.Chain
import StoreService.Names.Contracts (newContracts)
import StoreService.Names.Records
import StoreService.Names.Registration (Node, clearRecordsDigest, clearRecordsWithSigCalldata, labelNode, multicallCalldata, nodeBytes, resolverDomain, setTextDigest, setTextWithSigCalldata)
import StoreService.Names.Sender (Sender, followTransactions, newSender)
import StoreService.Names.Signatures (parseSignature)
import StoreService.Names.Store (EditState (..), NewRecordEdit (..), RecordChange (..), RecordEdit (..), SignedChange (..), getRecordEditByBody, insertRecordEdit)
import StoreService.Names.Turns (newTurns)
import StoreService.Web.Names.Records (EditInvalid (..), editRequest, recordsHandler)
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
    it "signs and encodes SetText and ClearRecords edits as viem does" $ testViemVectors g
    it "encodes two edits as one multicall as viem does" $ testViemMulticall g
    it "reads a request's formats, then its link, then its signature" testRequestOrder
    it "takes one or two edits of distinct keys with consecutive nonces, and a clear only alone" testRequestEdits
    it "bounds a link at 4096 bytes and takes only a link of the key's kind" testLinkBound
    it "refuses a high s, though recovery accepts it" testHighS
  describe "relay" $ do
    it "relays an edit with viem's calldata and answers its hash once mined" $ testRelayMined g
    it "refuses a wrong signer or a passed deadline before any row or gas" $ testRefusedBeforeRow g
    it "refuses an edit of a name on another resolver, or past its expiry and grace, before any row or gas" $ testRefusedNameState g
    it "leaves the owner's cap untouched by ten edits a non-owner signs" $ testNonOwnerCap g
    it "checks a refused edit's repeat anew" $ testRefusedRepeat g
    it "refuses the eleventh relayed edit of a name within an hour, with the real wait" $ testCap g
    it "answers provider_unavailable and stores nothing when the chain fails before the send" $ testUnavailableBeforeSend g
    it "stores no edit whose dry run reverted, so eleven of them leave the cap to sent edits" $ testDryRunNotCounted g
    it "checks a dry-run-reverted edit's repeat anew" $ testDryRunRepeat g
    it "answers provider_unavailable and stores nothing when the re-read after a dry run's revert fails" $ testDryRunReReadFails g
    it "lets a concurrent twin lose the unique insert before any signing, and answers both" $ testConcurrentTwin g
    it "answers edit_pending after the wait, and its repeat with the hash once mined" $ testPendingThenMined g
    it "answers edit_pending at once when the chain stops answering after the send" $ testChainStopsAfterSend g
    it "bounds the wait for edit_pending from the request's arrival, counting the checks and the send" $ testPendingFromArrival g
  describe "two edits" $ do
    it "relays two edits as one multicall transaction with one row, and answers its hash once mined" $ testTwoMined g
    it "refuses two edits when the second is signed by another key, or its nonce is not the owner's next, before any row or gas" $ testTwoRefused g
    it "answers an exact resend of two edits from the stored request, and checks either edit alone anew" $ testTwoResend g
    it "counts two edits as one edit of the yearly limit, taking the last one left" $ testTwoYearLimit g
    it "counts two edits as one request of the hourly cap" $ testTwoCap g
    it "answers a mined revert of two edits record_reverted with no name, giving its edit back" $ testTwoReverted g
    it "stores nothing when the dry run of two edits reverts, answering it with no name" $ testTwoDryRunReverted g
    it "settles two edits SignatureExpired once the earliest deadline is passed at depth" $ testTwoExpired g
    it "answers two edits applied with no hash only when the nonce passed both and both records hold their values" $ testTwoAppliedByAnother g
    it "answers two edits record_reverted InvalidNonce when another body took only the first nonce" $ testTwoFirstNonceTaken g
  describe "outcomes" $ do
    it "answers an InvalidNonce revert of an edit another relayer applied as mined with no hash, and its repeat too" $ testAppliedByAnother g
    it "answers a dry run's InvalidNonce revert of an edit another relayer applied as applied with no hash, storing nothing" $ testDryRunAppliedByAnother g
    it "answers a SignatureExpired revert of an edit another relayer applied as mined with no hash" $ testExpiredAppliedByAnother g
    it "answers record_reverted InvalidNonce when another body used the nonce with another value" $ testNonceTakenByAnother g
    it "relays a clear edit with viem's calldata, applied by another relayer when both records read empty" $ testClearApplied g
    it "answers a clear edit record_reverted InvalidNonce when either record holds a value after the nonce moved" $ testClearNonceTaken g
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
    it "counts a pending edit, and no edit refused, never sent, or reverted on chain" $ testYearFailedRelays g
  describe "http" $ do
    it "answers the contract's statuses and bodies" $ testHttp g
    it "answers 503, 504, and 429 with the real wait in Retry-After" $ testHttpUnsettled g
    it "answers 429 edit_limit with nextAt in unix seconds and no Retry-After" $ testHttpEditLimit g
    it "relays a body of two edits with links of 4096 bytes, above the 8192-byte cap of other endpoints" $ testHttpTwoLongLinks g

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
  contracts <- newContracts TLDSimplex chain
  turns <- newTurns
  rs <- newRecords store turns contracts chain sender TLDSimplex editsPerYear (readTVarIO clock)
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
signChange Setup {random} EphemeralWallet {ewKey} scChange scNonce scDeadline = do
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
contact = RCSetText contactKey

channel :: Text -> RecordChange
channel = RCSetText channelKey

-- | The owner's channel edit with nonce 0 and the default deadline.
ownerEdit :: Setup -> Text -> IO EditRequest
ownerEdit s@Setup {owner} value = signEdit s owner (channel value) 0 deadline

relay :: Setup -> EditRequest -> IO (Async EditAnswer)
relay Setup {records} = async . relayEdit records

-- | Mines the n-th transaction sent with this status, one block deeper, then runs the sender's round and the lane's.
mineSent :: Setup -> Int -> Bool -> IO TxHash
mineSent Setup {node, sender, records} n ok = do
  raws <- within $ awaitSent node n
  let h = C.keccak256 $ raws !! (n - 1)
  _ <- mine node [(h, ok)]
  _ <- mine node []
  followTransactions sender
  recordsRound records
  pure $ TxHash h

answerOf :: Async EditAnswer -> IO EditAnswer
answerOf = within . wait

within :: IO a -> IO a
within action = timeout 5000000 action >>= maybe (fail "timed out") pure

stateOf :: Setup -> EditRequest -> IO (Maybe EditState)
stateOf Setup {store} req = withTransaction store $ \db -> fmap reState <$> getRecordEditByBody db (bodyHashOf req)

editRows :: Setup -> IO Int
editRows Setup {store} = withTransaction store $ \db -> do
  [Only n] <- DB.query_ db "SELECT count(*) FROM sx_badge_service_name_record_edits"
  pure n

-- | As the relay keys a sent request: each signed digest and its signature, in order.
bodyHashOf :: EditRequest -> ByteString
bodyHashOf EditRequest {erChanges} =
  C.sha256Hash $ B.concat [digestOf scChange scNonce scDeadline <> serializeSignature scSignature | SignedChange {scChange, scNonce, scDeadline, scSignature} <- L.toList erChanges]

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
  recoverAddress setSig (digestOf (channel viemChannelLink) 0 deadline) `shouldReturn` Right viemAddress
  setTextWithSigCalldata aliceNode channelKey viemChannelLink 0 deadline setSig `shouldBe` viemSetCall
  SignedChange {scSignature = clearSig} <- signChange s (owner s) RCClear 1 deadline
  clearSig `shouldBe` viemClearSignature
  clearRecordsWithSigCalldata aliceNode 1 deadline clearSig `shouldBe` viemClearCall

testViemMulticall :: TVar ChaChaDRG -> IO ()
testViemMulticall g = withSetup g $ \s -> do
  EditRequest {erChanges = first :| rest} <- ownerTwo s 0 viemContactLink viemChannelLink
  [second] <- pure rest
  (scSignature first, scSignature second) `shouldBe` (viemContactSignature, viemChannelSignature)
  multicallCalldata
    [ setTextWithSigCalldata aliceNode contactKey viemContactLink 0 deadline (scSignature first),
      setTextWithSigCalldata aliceNode channelKey viemChannelLink 1 deadline (scSignature second)
    ]
    `shouldBe` viemMulticall

testRequestOrder :: IO ()
testRequestOrder = do
  let valid = editFields [("key", J.String channelKey), ("value", J.String viemChannelLink)]
      request = editRequest . requestOf . (: [])
  changesOf (request valid) `shouldBe` Right [channel viemChannelLink]
  changesOf (request $ editFields [("clear", J.Bool True)]) `shouldBe` Right [RCClear]
  changesOf (request $ editFields [("key", J.String contactKey), ("value", "")]) `shouldBe` Right [RCSetText contactKey ""]
  -- formats first: a bad label hash or key is bad_request even with a bad link
  editRequest (KM.insert "labelHash" (J.String $ "0x" <> T.toUpper (T.drop 2 aliceHashText)) $ requestOf [withLink "https://x/a"]) `shouldBe` Left EIBadRequest
  request (editFields [("key", "simplex.other"), ("value", J.String viemChannelLink)]) `shouldBe` Left EIBadRequest
  request (KM.insert "nonce" (J.Number (-1)) valid) `shouldBe` Left EIBadRequest
  request (KM.insert "deadline" (J.String "1767229200") valid) `shouldBe` Left EIBadRequest
  request (KM.delete "value" valid) `shouldBe` Left EIBadRequest
  -- the old body of one edit, with no list
  editRequest (KM.insert "labelHash" (J.String aliceHashText) valid) `shouldBe` Left EIBadRequest
  -- the link before the signature
  request (KM.insert "signature" "0x00" $ withLink "https://x/a") `shouldBe` Left EIInvalidLink
  request (KM.insert "signature" "0x00" valid) `shouldBe` Left EIBadRequest
  where
    withLink link = editFields [("key", J.String channelKey), ("value", J.String link)]

testRequestEdits :: IO ()
testRequestEdits = do
  let edit key value nonce = editFields [("key", J.String key), ("value", J.String value), ("nonce", J.toJSON (nonce :: Int))]
      clearAt nonce = editFields [("clear", J.Bool True), ("nonce", J.toJSON (nonce :: Int))]
      request = editRequest . requestOf
      contactAt = edit contactKey "https://smp8.simplex.im/a#Y8m3"
      channelAt = edit channelKey viemChannelLink
  changesOf (request [contactAt 4, channelAt 5]) `shouldBe` Right [contact "https://smp8.simplex.im/a#Y8m3", channel viemChannelLink]
  fmap (map scNonce . L.toList . erChanges) (request [channelAt 4, contactAt 5]) `shouldBe` Right [4, 5]
  forM_
    [ ("no edit", []),
      ("three edits", [contactAt 4, channelAt 5, edit channelKey "" 6]),
      ("one key twice", [channelAt 4, edit channelKey "" 5]),
      ("a clear after a set", [contactAt 4, clearAt 5]),
      ("a clear before a set", [clearAt 4, contactAt 5]),
      ("a nonce skipped", [contactAt 4, channelAt 6]),
      ("nonces reversed", [contactAt 5, channelAt 4]),
      ("one nonce twice", [contactAt 4, channelAt 4])
    ]
    $ \(what, edits) -> (what :: String, request edits) `shouldBe` (what, Left EIBadRequest)
  -- every edit's format before any link, and every link before any signature
  request [edit contactKey "https://x/a" 4, edit "simplex.other" viemChannelLink 5] `shouldBe` Left EIBadRequest
  request [KM.insert "signature" "0x00" $ contactAt 4, edit channelKey "https://x/c" 5] `shouldBe` Left EIInvalidLink

changesOf :: Either EditInvalid EditRequest -> Either EditInvalid [RecordChange]
changesOf = fmap (map scChange . L.toList . erChanges)

testLinkBound :: IO ()
testLinkBound = do
  let prefix = "https://smp8.simplex.im/c#"
      linkOf n = prefix <> T.replicate (n - T.length prefix) "a"
      edit key value = editRequest $ requestOf [editFields [("key", J.String key), ("value", J.String value)]]
  fmap (L.head . L.map scChange . erChanges) (edit channelKey $ linkOf 4096) `shouldBe` Right (channel $ linkOf 4096)
  edit channelKey (linkOf 4097) `shouldBe` Left EIInvalidLink
  edit contactKey viemChannelLink `shouldBe` Left EIInvalidLink
  edit channelKey "https://smp8.simplex.im/c#Y8m3;https://smp9.simplex.im/c#Z1" `shouldBe` Left EIInvalidLink

testHighS :: IO ()
testHighS = do
  let RecoverableSignature {rsR, rsS, rsRecId} = viemSetSignature
      highS = i2ospOf_ 32 $ curveOrder - os2ip rsS
      flipped = RecoverableSignature {rsR, rsS = highS, rsRecId = 1 - rsRecId}
  recoverAddress flipped (digestOf (channel viemChannelLink) 0 deadline) `shouldReturn` Right viemAddress
  editRequest (requestOf [KM.insert "signature" (J.String $ hex $ serializeSignature flipped) $ editFields [("key", J.String channelKey), ("value", J.String viemChannelLink)]])
    `shouldBe` Left EIBadRequest
  where
    curveOrder = ECC.ecc_n $ ECC.common_curve $ ECC.getCurveByName ECC.SEC_p256k1

-- relay

testRelayMined :: TVar ChaChaDRG -> IO ()
testRelayMined g = withSetup g $ \s@Setup {node} -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  [raw] <- within $ awaitSent node 1
  fmap (\TxRequest {trTo, trData} -> (trTo, trData)) (decodeSignedTx raw) `shouldBe` Right (fakeResolver, viemSetCall)
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EAApplied (Just h)
  stateOf s req `shouldReturn` Just (ESMined $ Just h)

testRefusedBeforeRow :: TVar ChaChaDRG -> IO ()
testRefusedBeforeRow g = withSetup g $ \s@Setup {node, records} -> do
  other <- newEphemeralWallet g
  (relayEdit records =<< signEdit s other (channel viemChannelLink) 0 deadline) `shouldReturn` EARefused
  setBlock node 2 deadline
  (relayEdit records =<< ownerEdit s viemChannelLink) `shouldReturn` EARefused
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []

testRefusedNameState :: TVar ChaChaDRG -> IO ()
testRefusedNameState g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  answerCall node registry (selector "resolver(bytes32)" <> nodeBytes aliceNode) $ Returns $ addressWord otherResolver
  relayEdit records req `shouldReturn` EARefused
  answerCall node registry (selector "resolver(bytes32)" <> nodeBytes aliceNode) $ Returns $ addressWord fakeResolver
  -- the registry still names the owner, but the name lapsed: expiry and grace are before the latest block
  answerCall node fakeBaseRegistrar (selector "nameExpires(uint256)" <> unLabel alice) $ Returns $ uint256 (genesis - gracePeriod - 1)
  answerCall node fakeController (selector "reservedNames(bytes32)" <> unLabel alice) $ Returns $ uint256 (0 :: Int)
  relayEdit records req `shouldReturn` EARefused
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []

testNonOwnerCap :: TVar ChaChaDRG -> IO ()
testNonOwnerCap g = withSetup g $ \s@Setup {node, records} -> do
  other <- newEphemeralWallet g
  forM_ [0 .. 9] $ \n -> (relayEdit records =<< signEdit s other (channel viemChannelLink) n deadline) `shouldReturn` EARefused
  editRows s `shouldReturn` 0
  r <- relay s =<< ownerEdit s viemChannelLink
  _ <- within $ awaitSent node 1
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EAApplied (Just h)

testRefusedRepeat :: TVar ChaChaDRG -> IO ()
testRefusedRepeat g = withSetup g $ \s@Setup {owner} -> do
  req <- signEdit s owner (channel viemChannelLink) 1 deadline
  relayEdit (records s) req `shouldReturn` EARefused
  stateOf s req `shouldReturn` Nothing
  setNonce s 1
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EAApplied (Just h)

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
  setFault node "eth_maxPriorityFeePerGas" HttpFailure
  relayEdit records req `shouldReturn` EAUnavailable
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []
  clearFault node "eth_maxPriorityFeePerGas"
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EAApplied (Just h)

testDryRunNotCounted :: TVar ChaChaDRG -> IO ()
testDryRunNotCounted g = withSetup g $ \s@Setup {node, records, owner} -> do
  setFault node "eth_estimateGas" $ revertFault $ selector "InvalidSignature()"
  forM_ [1 .. 11 :: Int] $ \n ->
    (relayEdit records =<< signEdit s owner (channel $ "https://smp8.simplex.im/c#n" <> T.pack (show n)) 0 deadline)
      `shouldReturn` EAReverted (Just "InvalidSignature")
  editRows s `shouldReturn` 0
  clearFault node "eth_estimateGas"
  r <- relay s =<< ownerEdit s viemChannelLink
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EAApplied (Just h)

testDryRunRepeat :: TVar ChaChaDRG -> IO ()
testDryRunRepeat g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  revertDryRun node viemSetCall $ Just $ selector "InvalidSignature()"
  relayEdit records req `shouldReturn` EAReverted (Just "InvalidSignature")
  stateOf s req `shouldReturn` Nothing
  revertDryRun node viemSetCall Nothing
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EAApplied (Just h)

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
  _ <- within $ awaitSent node 1
  clearFault node "eth_getBlockByNumber"
  h <- mineSent s 1 True
  within (wait answers) `shouldReturn` (EAApplied (Just h), EAApplied (Just h))
  length <$> sentTransactions node `shouldReturn` 1
  editRows s `shouldReturn` 1

testPendingThenMined :: TVar ChaChaDRG -> IO ()
testPendingThenMined g = withSetup g $ \s@Setup {records} -> do
  req <- ownerEdit s viemChannelLink
  relayEdit records {rsPendingAfter = shortWait} req `shouldReturn` EAPending
  h <- mineSent s 1 True
  relayEdit records req `shouldReturn` EAApplied (Just h)

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

testChainStopsAfterSend :: TVar ChaChaDRG -> IO ()
testChainStopsAfterSend g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  _ <- within $ awaitSent node 1
  setFault node "eth_getBlockByNumber" HttpFailure
  recordsRound records
  answerOf r `shouldReturn` EAPending
  relayEdit records req `shouldReturn` EAPending
  clearFault node "eth_getBlockByNumber"
  h <- mineSent s 1 True
  relayEdit records req `shouldReturn` EAApplied (Just h)

-- two edits

testTwoMined :: TVar ChaChaDRG -> IO ()
testTwoMined g = withSetup g $ \s@Setup {node} -> do
  req <- ownerTwo s 0 viemContactLink viemChannelLink
  r <- relay s req
  [raw] <- within $ awaitSent node 1
  fmap (\TxRequest {trTo, trData} -> (trTo, trData)) (decodeSignedTx raw) `shouldBe` Right (fakeResolver, viemMulticall)
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EAApplied (Just h)
  stateOf s req `shouldReturn` Just (ESMined $ Just h)
  editRows s `shouldReturn` 1
  length <$> sentTransactions node `shouldReturn` 1

testTwoRefused :: TVar ChaChaDRG -> IO ()
testTwoRefused g = withSetup g $ \s@Setup {node, records, owner} -> do
  other <- newEphemeralWallet g
  first <- signChange s owner (contact viemContactLink) 0 deadline
  stranger <- signChange s other (channel viemChannelLink) 1 deadline
  relayEdit records (editOf $ first :| [stranger]) `shouldReturn` EARefused
  passed <- signChange s owner (channel viemChannelLink) 1 (genesis - 1)
  relayEdit records (editOf $ first :| [passed]) `shouldReturn` EARefused
  setNonce s 1
  (relayEdit records =<< ownerTwo s 0 viemContactLink viemChannelLink) `shouldReturn` EARefused
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []

testTwoResend :: TVar ChaChaDRG -> IO ()
testTwoResend g = withSetup g $ \s@Setup {node, records} -> do
  req@EditRequest {erChanges = first :| rest} <- ownerTwo s 0 viemContactLink viemChannelLink
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EAApplied (Just h)
  relayEdit records req `shouldReturn` EAApplied (Just h)
  editsLeftNow s `shouldReturn` EditsLeft (editsPerYear - 1) (Just $ addUTCTime year t0)
  -- either edit alone is another body, checked anew against the nonce both moved
  setNonce s 2
  relayEdit records (editOf $ first :| []) `shouldReturn` EARefused
  relayEdit records (editOf $ L.fromList rest) `shouldReturn` EARefused
  editRows s `shouldReturn` 1
  length <$> sentTransactions node `shouldReturn` 1

testTwoYearLimit :: TVar ChaChaDRG -> IO ()
testTwoYearLimit g = withSetup g $ \s@Setup {records} -> do
  _ <- appliedEdits s 2
  setNonce s 2
  editsLeftNow s `shouldReturn` EditsLeft 1 (Just $ addUTCTime year t0)
  r <- relay s =<< ownerTwo s 2 viemContactLink viemChannelLink
  h <- mineSent s 3 True
  answerOf r `shouldReturn` EAApplied (Just h)
  setNonce s 4
  editsLeftNow s `shouldReturn` EditsLeft 0 (Just $ addUTCTime year t0)
  (relayEdit records =<< ownerTwo s 4 "https://smp8.simplex.im/a#next" "https://smp9.simplex.im/c#next") `shouldReturn` EALimited (addUTCTime year t0)
  editRows s `shouldReturn` 3

testTwoCap :: TVar ChaChaDRG -> IO ()
testTwoCap g = withSetup g $ \s0 -> do
  let s = roomy s0
      quick = (records s) {rsPendingAfter = shortWait}
      twoAt n = ownerTwo s n viemContactLink ("https://smp9.simplex.im/c#n" <> T.pack (show n))
  forM_ [0, 2 .. 18] $ \n -> do
    setNonce s n
    (relayEdit quick =<< twoAt n) `shouldReturn` EAPending
  setNonce s 20
  (relayEdit (records s) =<< twoAt 20) `shouldReturn` EACapped 3600
  editRows s `shouldReturn` 10

testTwoReverted :: TVar ChaChaDRG -> IO ()
testTwoReverted g = withSetup g $ \s@Setup {records} -> do
  req <- ownerTwo s 0 viemContactLink viemChannelLink
  r <- relay s req
  -- reverted before the deadlines with the nonce unused: multicall drops the inner error
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAReverted Nothing
  stateOf s req `shouldReturn` Just (ESReverted Nothing)
  editsLeftNow s `shouldReturn` EditsLeft editsPerYear Nothing
  relayEdit records req `shouldReturn` EAReverted Nothing

testTwoDryRunReverted :: TVar ChaChaDRG -> IO ()
testTwoDryRunReverted g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerTwo s 0 viemContactLink viemChannelLink
  revertDryRun node viemMulticall $ Just ""
  relayEdit records req `shouldReturn` EAReverted Nothing
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []
  revertDryRun node viemMulticall Nothing
  r <- relay s req
  h <- mineSent s 1 True
  answerOf r `shouldReturn` EAApplied (Just h)

testTwoExpired :: TVar ChaChaDRG -> IO ()
testTwoExpired g = withSetup g $ \s@Setup {node, records, owner} -> do
  first <- signChange s owner (contact viemContactLink) 0 (deadline + 600)
  second <- signChange s owner (channel viemChannelLink) 1 deadline
  let req = editOf $ first :| [second]
  relayEdit records {rsPendingAfter = shortWait} req `shouldReturn` EAPending
  _ <- within $ awaitSent node 1
  -- the second edit's deadline, the earlier one, is passed at the latest block but not yet two confirmations deep
  setBlock node 300 deadline
  setBlock node 301 $ deadline + 1
  recordsRound records
  stateOf s req `shouldReturn` Just ESPending
  setBlock node 302 $ deadline + 2
  recordsRound records
  stateOf s req `shouldReturn` Just (ESReverted $ Just "SignatureExpired")
  editsLeftNow s `shouldReturn` EditsLeft editsPerYear Nothing

testTwoAppliedByAnother :: TVar ChaChaDRG -> IO ()
testTwoAppliedByAnother g =
  forM_ [(viemChannelLink, EAApplied Nothing), ("https://smp9.simplex.im/c#other", EAReverted $ Just "InvalidNonce")] $ \(held, answer) -> withSetup g $ \s@Setup {node} -> do
    req <- ownerTwo s 0 viemContactLink viemChannelLink
    r <- relay s req
    _ <- within $ awaitSent node 1
    -- another relayer's transaction used both nonces first
    setNonce s 2
    setRecord s contactKey viemContactLink
    setRecord s channelKey held
    _ <- mineSent s 1 False
    answerOf r `shouldReturn` answer

testTwoFirstNonceTaken :: TVar ChaChaDRG -> IO ()
testTwoFirstNonceTaken g = withSetup g $ \s@Setup {node} -> do
  req <- ownerTwo s 0 viemContactLink viemChannelLink
  r <- relay s req
  _ <- within $ awaitSent node 1
  -- the records happen to hold both values, but only the first nonce moved, so the request cannot land
  setNonce s 1
  setRecord s contactKey viemContactLink
  setRecord s channelKey viemChannelLink
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAReverted (Just "InvalidNonce")
  stateOf s req `shouldReturn` Just (ESReverted $ Just "InvalidNonce")

-- | The name's yearly edits left now, by the relay's clock.
editsLeftNow :: Setup -> IO EditsLeft
editsLeftNow Setup {store, clock} = readTVarIO clock >>= \now -> withTransaction store $ \db -> nameEditsLeft db editsPerYear alice now

-- outcomes

testAppliedByAnother :: TVar ChaChaDRG -> IO ()
testAppliedByAnother g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  _ <- within $ awaitSent node 1
  -- another relayer's transaction with the same signed edit mined first
  setNonce s 1
  setRecord s channelKey viemChannelLink
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAApplied Nothing
  stateOf s req `shouldReturn` Just (ESMined Nothing)
  relayEdit records req `shouldReturn` EAApplied Nothing

testDryRunAppliedByAnother :: TVar ChaChaDRG -> IO ()
testDryRunAppliedByAnother g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  -- the nonce check reads 0; another relayer applies the edit before the dry run
  answerCallOnce node fakeResolver (noncesCall s) $ Returns $ uint256 (0 :: Int)
  setNonce s 1
  setRecord s channelKey viemChannelLink
  revertDryRun node viemSetCall $ Just $ selector "InvalidNonce()"
  relayEdit records req `shouldReturn` EAApplied Nothing
  editRows s `shouldReturn` 0
  sentTransactions node `shouldReturn` []

testExpiredAppliedByAnother :: TVar ChaChaDRG -> IO ()
testExpiredAppliedByAnother g = withSetup g $ \s@Setup {node} -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  _ <- within $ awaitSent node 1
  setBlock node 300 deadline
  setNonce s 1
  setRecord s channelKey viemChannelLink
  -- mined in the block after the deadline, so it reverted SignatureExpired
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAApplied Nothing
  stateOf s req `shouldReturn` Just (ESMined Nothing)

testNonceTakenByAnother :: TVar ChaChaDRG -> IO ()
testNonceTakenByAnother g = withSetup g $ \s@Setup {node} -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  _ <- within $ awaitSent node 1
  setNonce s 1
  setRecord s channelKey "https://smp9.simplex.im/c#other"
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAReverted (Just "InvalidNonce")
  stateOf s req `shouldReturn` Just (ESReverted $ Just "InvalidNonce")

testClearApplied :: TVar ChaChaDRG -> IO ()
testClearApplied g = withSetup g $ \s -> do
  setRecord s contactKey "https://smp8.simplex.im/a#Y8m3"
  setRecord s channelKey viemChannelLink
  r <- relayClear s
  setRecord s contactKey ""
  setRecord s channelKey ""
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAApplied Nothing

testClearNonceTaken :: TVar ChaChaDRG -> IO ()
testClearNonceTaken g = forM_ [contactKey, channelKey] $ \held -> withSetup g $ \s -> do
  r <- relayClear s
  setRecord s held "https://smp9.simplex.im/c#held"
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAReverted (Just "InvalidNonce")

-- | viem's clear edit, with nonce 1, sent; then the owner's nonce moves past it.
relayClear :: Setup -> IO (Async EditAnswer)
relayClear s@Setup {node, owner} = do
  setNonce s 1
  req <- signEdit s owner RCClear 1 deadline
  r <- relay s req
  [raw] <- within $ awaitSent node 1
  fmap trData (decodeSignedTx raw) `shouldBe` Right viemClearCall
  setNonce s 2
  pure r

testRevertFreesNonce :: TVar ChaChaDRG -> IO ()
testRevertFreesNonce g = withSetup g $ \s@Setup {node, records} -> do
  first <- ownerEdit s viemChannelLink
  r <- relay s first
  -- reverted before the deadline with the nonce unused, so the revert has no name
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAReverted Nothing
  second <- ownerEdit s "https://smp9.simplex.im/c#second"
  r' <- relay s second
  _ <- within $ awaitSent node 2
  relayEdit records first `shouldReturn` EAReverted Nothing
  stateOf s second `shouldReturn` Just ESPending
  h <- mineSent s 2 True
  answerOf r' `shouldReturn` EAApplied (Just h)

testFailedReRead :: TVar ChaChaDRG -> IO ()
testFailedReRead g = withSetup g $ \s@Setup {node, records} -> do
  req <- ownerEdit s viemChannelLink
  r <- relay s req
  _ <- within $ awaitSent node 1
  setFault node "eth_call" HttpFailure
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAPending
  stateOf s req `shouldReturn` Just ESPending
  relayEdit records req `shouldReturn` EAPending
  clearFault node "eth_call"
  recordsRound records
  stateOf s req `shouldReturn` Just (ESReverted Nothing)
  relayEdit records req `shouldReturn` EAReverted Nothing

testResume :: TVar ChaChaDRG -> IO ()
testResume g = withSetup g $ \s@Setup {node, owner} -> do
  unsent <- ownerEdit s viemChannelLink
  saveUnsent s unsent
  s' <- restart s
  recordsRound (records s')
  h <- mineSent s' 1 True
  stateOf s unsent `shouldReturn` Just (ESMined $ Just h)
  -- a sent edit whose transaction the node forgot while the service was down
  setNonce s 1
  sent <- signEdit s owner (channel "https://smp9.simplex.im/c#next") 1 deadline
  _ <- relayEdit (records s') {rsPendingAfter = shortWait} sent
  [_, raw] <- within $ awaitSent node 2
  dropTransaction node $ C.keccak256 raw
  s'' <- restart s'
  followTransactions (sender s'')
  (!! 2) <$> sentTransactions node `shouldReturn` raw
  h' <- mineSent s'' 3 True
  stateOf s sent `shouldReturn` Just (ESMined $ Just h')
  TxHash (C.keccak256 raw) `shouldBe` h'

testBadRowSkipped :: TVar ChaChaDRG -> IO ()
testBadRowSkipped g = withSetup g $ \s@Setup {store, clock} -> do
  now <- readTVarIO clock
  -- a text record with a key and no value, which no edit can be
  withTransaction store $ \db -> do
    DB.execute
      db
      "INSERT INTO sx_badge_service_name_record_edits (label_hash, owner, nonce, body_hash, state, created_at) VALUES (?,?,5,?,'pending',?)"
      (Binary $ unLabel alice, Binary $ unAddress viemAddress, Binary $ B.replicate 32 0, now)
    DB.execute
      db
      "INSERT INTO sx_badge_service_name_record_changes (name_record_edit_id, nonce, record_key, record_value, deadline, signature) SELECT name_record_edit_id, 5, 'simplex.channel', NULL, ?, ? FROM sx_badge_service_name_record_edits WHERE nonce = 5"
      (deadline, Binary $ serializeSignature viemSetSignature)
  req <- ownerEdit s viemChannelLink
  saveUnsent s req
  s' <- restart s
  recordsRound (records s')
  h <- mineSent s' 1 True
  stateOf s req `shouldReturn` Just (ESMined $ Just h)

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
  answerOf r `shouldReturn` EAApplied (Just h)
  s' <- restart s
  setFault node "eth_call" HttpFailure
  setFault node "eth_getBlockByNumber" HttpFailure
  relayEdit (records s') req `shouldReturn` EAApplied (Just h)
  length <$> sentTransactions node `shouldReturn` 1

testRestartExpiredApplied :: TVar ChaChaDRG -> IO ()
testRestartExpiredApplied g = withSetup g $ \s@Setup {node} -> do
  req <- ownerEdit s viemChannelLink
  relayEdit (records s) {rsPendingAfter = shortWait} req `shouldReturn` EAPending
  _ <- within $ awaitSent node 1
  -- while the service is down the deadline passes, and another relayer applies the edit
  setBlock node 300 $ deadline + 1
  setBlock node 301 $ deadline + 2
  setNonce s 1
  setRecord s channelKey viemChannelLink
  s' <- restart s
  recordsRound (records s')
  stateOf s req `shouldReturn` Just (ESMined Nothing)
  relayEdit (records s') req `shouldReturn` EAApplied Nothing

testExpiryFreesNonce :: TVar ChaChaDRG -> IO ()
testExpiryFreesNonce g = withSetup g $ \s@Setup {node, records, sender} -> do
  req <- ownerEdit s viemChannelLink
  relayEdit records {rsPendingAfter = shortWait} req `shouldReturn` EAPending
  [raw] <- within $ awaitSent node 1
  -- the latest block is past the deadline, but not yet the block two confirmations deep
  setBlock node 300 deadline
  setBlock node 301 $ deadline + 1
  recordsRound records
  stateOf s req `shouldReturn` Just ESPending
  setBlock node 302 $ deadline + 2
  recordsRound records
  stateOf s req `shouldReturn` Just (ESReverted $ Just "SignatureExpired")
  relayEdit records req `shouldReturn` EAReverted (Just "SignatureExpired")
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
  answerOf r `shouldReturn` EAApplied (Just h)
  setNonce s 4
  fifth <- signEdit s owner (channel "https://smp9.simplex.im/c#fifth") 4 deadline
  relayEdit records fifth `shouldReturn` EALimited (addUTCTime (year + day) t0)
  editRows s `shouldReturn` 4

testYearResend :: TVar ChaChaDRG -> IO ()
testYearResend g = withSetup g $ \s@Setup {node, records} -> do
  sent <- appliedEdits s 3
  forM_ sent $ \(req, h) -> relayEdit records req `shouldReturn` EAApplied (Just h)
  editRows s `shouldReturn` 3
  length <$> sentTransactions node `shouldReturn` 3

testYearFailedRelays :: TVar ChaChaDRG -> IO ()
testYearFailedRelays g = withSetup g $ \s@Setup {node, records, owner} -> do
  -- reverted on chain, its nonce unused
  r <- relay s =<< ownerEdit s viemChannelLink
  _ <- mineSent s 1 False
  answerOf r `shouldReturn` EAReverted Nothing
  -- refused: another signer, then a passed deadline
  other <- newEphemeralWallet g
  (relayEdit records =<< signEdit s other (channel viemChannelLink) 0 deadline) `shouldReturn` EARefused
  (relayEdit records =<< signEdit s owner (channel viemChannelLink) 0 (genesis - 1)) `shouldReturn` EARefused
  -- never sent: the dry run reverted, then the fee read failed
  setFault node "eth_estimateGas" $ revertFault $ selector "InvalidSignature()"
  (relayEdit records =<< signEdit s owner (channel "https://smp9.simplex.im/c#dry") 0 deadline) `shouldReturn` EAReverted (Just "InvalidSignature")
  clearFault node "eth_estimateGas"
  setFault node "eth_maxPriorityFeePerGas" HttpFailure
  (relayEdit records =<< signEdit s owner (channel "https://smp9.simplex.im/c#fee") 0 deadline) `shouldReturn` EAUnavailable
  clearFault node "eth_maxPriorityFeePerGas"
  -- two mined and one pending use the limit up
  _ <- appliedEditsFrom s 0 1 2
  setNonce s 2
  unsettled <- signEdit s owner (channel "https://smp9.simplex.im/c#pending") 2 deadline
  relayEdit records {rsPendingAfter = shortWait} unsettled `shouldReturn` EAPending
  setNonce s 3
  fourth <- signEdit s owner (channel "https://smp9.simplex.im/c#fourth") 3 deadline
  relayEdit records fourth `shouldReturn` EALimited (addUTCTime year t0)
  -- the pending edit reverts, which gives its edit back
  _ <- mineSent s 4 False
  stateOf s unsettled >>= (`shouldSatisfy` \case Just (ESReverted _) -> True; _ -> False)
  r' <- relay s fourth
  h <- mineSent s 5 True
  answerOf r' `shouldReturn` EAApplied (Just h)

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
    answerOf r `shouldReturn` EAApplied (Just h)
    pure (req, h)

-- | Room in the yearly limit for the tests of the hourly cap.
roomy :: Setup -> Setup
roomy s = s {records = (records s) {rsEditsPerYear = 100}}

day, year :: NominalDiffTime
day = 86400
year = 365 * day

yearSeconds, t0Seconds :: Int
yearSeconds = 365 * 86400
t0Seconds = floor $ utcTimeToPOSIXSeconds t0

-- http

testHttp :: TVar ChaChaDRG -> IO ()
testHttp g = withSetup g $ \s@Setup {records} -> withRecordsApp records $ \post -> do
  let editBody value = J.encode $ requestOf [editFields [("key", J.String channelKey), ("value", J.String value)]]
  post "[]" `shouldReturn` (400, Just $ errorJson "bad_request", Nothing)
  post (editBody "https://x/a") `shouldReturn` (400, Just $ errorJson "invalid_link", Nothing)
  answer <- async $ post $ editBody viemChannelLink
  TxHash h <- mineSent s 1 True
  within (wait answer) `shouldReturn` (200, Just $ J.object ["tx" .= hex h], Nothing)
  setNonce s 1
  next <- async . post . J.encode . editJson =<< signEdit s (owner s) (channel "https://smp9.simplex.im/c#next") 1 deadline
  _ <- mineSent s 2 False
  within (wait next) `shouldReturn` (409, Just $ J.object ["error" .= ("record_reverted" :: Text), "revert" .= J.Null], Nothing)

testHttpUnsettled :: TVar ChaChaDRG -> IO ()
testHttpUnsettled g = withSetup g $ \s@Setup {node, owner} -> do
  let quick = (records $ roomy s) {rsPendingAfter = shortWait}
      edit n = signEdit s owner (channel viemChannelLink) n deadline
  withRecordsApp quick $ \post -> do
    setFault node "eth_call" HttpFailure
    (post . J.encode . editJson =<< edit 0) `shouldReturn` (503, Just $ errorJson "provider_unavailable", Nothing)
    clearFault node "eth_call"
    (post . J.encode . editJson =<< edit 0) `shouldReturn` (504, Just $ errorJson "edit_pending", Nothing)
    forM_ [1 .. 9] $ \n -> do
      setNonce s n
      (relayEdit quick =<< edit n) `shouldReturn` EAPending
    setNonce s 10
    (post . J.encode . editJson =<< edit 10) `shouldReturn` (429, Just $ errorJson "rate_limited", Just "3600")

testHttpEditLimit :: TVar ChaChaDRG -> IO ()
testHttpEditLimit g = withSetup g $ \s@Setup {records, owner} -> do
  _ <- appliedEdits s 3
  setNonce s 3
  withRecordsApp records $ \post ->
    (post . J.encode . editJson =<< signEdit s owner (channel viemChannelLink) 3 deadline)
      `shouldReturn` (429, Just $ J.object ["error" .= ("edit_limit" :: Text), "nextAt" .= (t0Seconds + yearSeconds)], Nothing)

testHttpTwoLongLinks :: TVar ChaChaDRG -> IO ()
testHttpTwoLongLinks g = withSetup g $ \s@Setup {records} -> withRecordsApp records $ \post -> do
  let longLink prefix = prefix <> T.replicate (4096 - T.length prefix) "a"
  body <- J.encode . editJson <$> ownerTwo s 0 (longLink "https://smp8.simplex.im/a#") (longLink "https://smp8.simplex.im/c#")
  LB.length body `shouldSatisfy` (> 8192)
  answer <- async $ post body
  TxHash h <- mineSent s 1 True
  within (wait answer) `shouldReturn` (200, Just $ J.object ["tx" .= hex h], Nothing)

-- | Posts a body to the records handler, answering the status, the JSON body and Retry-After.
withRecordsApp :: Records -> ((LB.ByteString -> IO (Int, Maybe J.Value, Maybe ByteString)) -> IO a) -> IO a
withRecordsApp rs action = Warp.testWithApplication (pure $ recordsHandler rs) $ \port -> do
  manager <- newManager defaultManagerSettings
  action $ \body -> do
    req <- parseRequest $ "http://127.0.0.1:" <> show port <> "/api/name/records"
    resp <- httpLbs req {method = "POST", requestHeaders = [(hContentType, "application/json")], requestBody = RequestBodyLBS body} manager
    pure (statusCode $ responseStatus resp, J.decode $ responseBody resp, lookup "Retry-After" $ responseHeaders resp)

errorJson :: Text -> J.Value
errorJson tag = J.object ["error" .= tag]

-- fixtures

editJson :: EditRequest -> J.Object
editJson EditRequest {erChanges} = requestOf $ map changeJson $ L.toList erChanges

changeJson :: SignedChange -> J.Object
changeJson SignedChange {scChange, scNonce, scDeadline, scSignature} =
  KM.fromList $
    [ ("nonce", J.toJSON scNonce),
      ("deadline", J.toJSON scDeadline),
      ("signature", J.String $ hex $ serializeSignature scSignature)
    ]
      <> case scChange of
        RCSetText key value -> [("key", J.String key), ("value", J.String value)]
        RCClear -> [("clear", J.Bool True)]

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
viemChange = SignedChange {scChange = channel viemChannelLink, scNonce = 0, scDeadline = deadline, scSignature = viemSetSignature}

digestOf :: RecordChange -> Word64 -> Word64 -> ByteString
digestOf change nonce dl = case change of
  RCSetText key value -> setTextDigest resolverDomain' aliceNode key value nonce dl
  RCClear -> clearRecordsDigest resolverDomain' aliceNode nonce dl

resolverDomain' :: Eip712Domain
resolverDomain' = resolverDomain fakeChainId fakeResolver

alice :: LabelHash
alice = labelHash "alicebakery"

aliceNode :: Node
aliceNode = labelNode TLDSimplex alice

aliceHashText :: Text
aliceHashText = hex $ unLabel alice

unLabel :: LabelHash -> ByteString
unLabel (LabelHash h) = h

contactKey, channelKey :: Text
contactKey = "simplex.contact"
channelKey = "simplex.channel"

-- | An hour after the fake node's first block.
deadline :: Word64
deadline = 1767229200

nameExpiry, gracePeriod :: Word64
nameExpiry = 1861833600
gracePeriod = 7776000

registry, otherResolver :: Address
registry = address "0x5fbdb2315678afecb367f032d93f642f64180aa3"
otherResolver = address "0x4826533b4897376654bb4d4ad88b7fafd0c98528"

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

-- | viem: ClearRecords(alicebakery.simplex, 1, deadline)
viemClearSignature :: RecoverableSignature
viemClearSignature = either (error . show) id $ parseSignature "0x0565f7e5a591c200fcc358c1d9e9293721d0b99f0285b589211203e311e8e6167ee822d664048870bdd8552263d25bbf45e7f4985fd3b8c3a19cfced44ef9d561c"

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

viemSetCall, viemClearCall :: ByteString
viemSetCall = unhex "71a218a70f4104919a21c22f566d9c7947c7c7d8addbf546c973851a7e449276ea54ec1a00000000000000000000000000000000000000000000000000000000000000c000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000006955c7100000000000000000000000000000000000000000000000000000000000000140000000000000000000000000000000000000000000000000000000000000000f73696d706c65782e6368616e6e656c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e68747470733a2f2f736d70382e73696d706c65782e696d2f632359386d3300000000000000000000000000000000000000000000000000000000000000000041fce4c6865591609c07ef3d94f376187d8a20cd9d94b4affcb60b55e999038fbd4e45c42b5fafe66a8d4f756e99398602eacef777b4dce4afd0a767cddadf3ca91c00000000000000000000000000000000000000000000000000000000000000"
viemClearCall = unhex "6463e6140f4104919a21c22f566d9c7947c7c7d8addbf546c973851a7e449276ea54ec1a0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000006955c710000000000000000000000000000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000410565f7e5a591c200fcc358c1d9e9293721d0b99f0285b589211203e311e8e6167ee822d664048870bdd8552263d25bbf45e7f4985fd3b8c3a19cfced44ef9d561c00000000000000000000000000000000000000000000000000000000000000"

unhex :: ByteString -> ByteString
unhex = either error id . convertFromBase Base16

address :: ByteString -> Address
address = either error id . strDecode
