{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesOwnerTests
  ( namesOwnerTests,
    answerExpiry,
    answerFree,
    answerOwner,
    answerUnowned,
    byB,
    codeIn,
    committedPhase,
    dynamisOrder,
    failedPhase,
    findBody,
    labelBytes,
    linksRefusedPhase,
    move,
    moveBody,
    moveTo,
    ownerOfSelector,
    paidIn,
    postFind,
    postMove,
    rewrite,
    sentRegister,
    stored,
  )
where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (registrarOf, rentPriceAnswer, rentPriceCall, threeYearsOf11)
import Bots.StoreService.NamesOrdersTests
import Bots.StoreService.NamesStoreTests (btc, hashFixture, insertInvoiceRow, order, ownerA, paidOrder, reveal, t0)
import Bots.StoreService.NamesPhasesTests (held, registrationPath, seenVersion, woken)
import Bots.StoreService.WebTests (StubCall (..), answerFixture, bounded, capturingStderr, fieldOf, stubCalls, jsonObject, refusedWith, phaseOf, phaseValue, storeFixture, webGet, withApp)
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (forM, forM_, void)
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import Data.IORef (readIORef)
import Data.Foldable (toList)
import Data.List (find, sort, sortOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (addUTCTime)
import Data.Word (Word64, Word8)
import Simplex.Chat.PaymentService.Types (InvoiceId (..), InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Agent.Store.DB as DB
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)
import Simplex.Messaging.Encoding (Encoding (..))
import Simplex.Messaging.Names.Record (USDCents)
import Simplex.Messaging.SimplexName (LabelHash (..), labelHash)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..))
import Simplex.Messaging.Util (tshow)
import StoreService.Config (NamesConfig (..))
import StoreService.Names.Chain (Hex32 (..), TxHash (..), addressWord, hex, uint256)
import StoreService.Names.Orders (OrdersEnv (..), movable, signedRequestSeconds)
import StoreService.Names.Registration (LinkKind (..), OrderFields (..), RegistrationId (..), findMessage, parseAnyCaseAddress, personalDigest, retargetMessage)
import StoreService.Names.Store hiding (findLimit)
import StoreService.Names.Turns (labelTurnCount, withLabelTurn)
import StoreService.Names.View (addressText)
import StoreService.Store (RevokeResult (..))
import StoreService.Web.Http (createLimit, findLimit, holdMicros, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Owner (findHandler, retargetHandler)
import StoreService.Web.Names.Registration (registrationHandler)
import StoreService.Store.Invoices (InvoiceRow (..), getInvoice)
import StoreService.Web.Server (WebEnv (..), webApp)
import Test.Hspec

namesOwnerTests :: Spec
namesOwnerTests = describe "StoreService names: owner" $ do
  describe "find" $ do
    it "lists the owner's orders newest first, a moved, an unpaid and a lapsed one included, at most 50, and none for an owner of none" testFind
    it "refuses another signer's signature, a stale timestamp and a malformed body" testFindRefused
  describe "another name" $ do
    it "refuses a move from registered, queued, taken, and committed without a refused reveal" testMoveRefusedPhases
    it "moves a failed order, a lapsed one, and a committed one whose links were refused, to the same name with other links too, and to no name another paid order holds" testMoveAccepted
    it "cancels another buyer's open invoice for the name once a moved order holds it" testMoveHoldsName
    it "answers a body equal to the current fields with the view unchanged, counting no move, whatever its timestamp" testMoveRepeat
    it "moves an order by a body differing only in terms, storing them" testMoveTermsOnly
    it "answers terms_changed for an old version, other terms or the other kind, listed or not, a body differing only in terms included, and moves under the community terms, storing them" testMoveTermsChanged
    it "checks a code order moved to a community name for 2 years, and moved on to another name for its code's years again" testMoveCommunityYears
    it "prices a paid order moved to a community name for 2 years, whatever years it paid for" testMoveCommunityPaidYears
    it "checks the terms after the timestamp, the register check, the existing orders and the duration, before the chain, answering terms_unavailable while they are not set" testMoveTermsCheckOrder
    it "answers the fourth move move_limit with the current view, changing nothing" testMoveLimit
    it "refuses a move meeting another live order of the wallet's or the commitment with a bare order_exists, before the price" testMoveMeetsOrder
    it "runs the order checks on the new fields: invalid_name, the duration, name_taken, name_check_unavailable, after the register check, answered provider_unavailable when it fails" testMoveOrderChecks
    it "registers a failed order whose register went through, by its mined register or by ownerOf, answering registration_changed and spending a code" testMoveLanded
    it "spends a released code still unused when its failed order turns out registered, and alerts once when it was spent elsewhere meanwhile" testMoveLandedReleasedCode
    it "answers a dearer name price_too_high with its price, one priced under the floor name_check_unavailable crediting nothing, and moves to a cheaper one at its price, crediting the difference to its balance" testMovePrice
    it "keeps fromBalance through a move to a cheaper name" testMoveKeepsFromBalance
    it "checks a code order's code again: code_not_covering for a shorter name, a held code kept though revoked and expired since" testMoveCodeOrder
    it "reserves a released code again while unused, else answers code_used, code_reserved, code_invalid or code_expired" testMoveReleasedCode
    it "refuses sealed over 11095 characters, a body over 16384 bytes and malformed fields, accepting the longest" testMoveSizes
    it "refuses another signer's signature and a stale timestamp, and answers an unknown order 404" testMoveRefusedSigner
    it "answers registration_changed with the current view when the order changed during the checks, writing nothing" testMoveLostRace
    it "takes both turns in hash order when A moves to B while another order moves B to A, without deadlock, each refused as the other's failed order holds its name" testMovesCross
  describe "polls" $
    it "wakes a held registration poll by another name" testPollsWoken

-- fixtures

-- | The owner routes and the registration poll as namesRoutes registers them, with the code orders, since a NamesEnv needs a running chat controller.
withOwner :: (Ctx -> IO a) -> IO a
withOwner = withOwnerAs id

withOwnerAs :: (NamesConfig -> NamesConfig) -> (Ctx -> IO a) -> IO a
withOwnerAs change action = withOrders $ \ctx -> do
  let ctx' = configured change ctx
  forM_ ["alicebakery", "dynamis"] $ answerUnowned ctx'
  serveOwner ctx' action

-- | The owner routes over the context's settings.
serveOwner :: Ctx -> (Ctx -> IO a) -> IO a
serveOwner ctx@Ctx {cOrders = orders, cWeb = web} action = do
  let routes path = ownerRoutes orders web path <|> codeRoutes orders (weProviders web) (weWaiters web) path
      app req respond = fromMaybe (webApp web req respond) (namesApp (weHttp web) routes req respond)
  withApp app $ \client -> action ctx {cClient = client}

ownerRoutes :: OrdersEnv -> WebEnv -> [Text] -> Maybe Route
ownerRoutes orders WebEnv {weHttp, weProviders, weWaiters} = \case
  ["api", "registration", "find"] -> Just Route {rMethod = "POST", rLimit = findLimit, rHandler = findHandler orders}
  ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler orders weHttp holdMicros (RegistrationId rid)}
  ["api", "registration", rid, "name"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = retargetHandler orders weProviders weWaiters (RegistrationId rid)}
  _ -> Nothing

signWith :: Ctx -> Secp256k1PrivateKey -> ByteString -> IO Text
signWith Ctx {cRandom} key msg = hex . smpEncode . either (error . show) id <$> signRecoverable cRandom key (personalDigest msg)

clockSeconds :: Ctx -> IO Word64
clockSeconds Ctx {cClock} = seconds <$> readIORef cClock

signedWindow :: Word64
signedWindow = fromInteger signedRequestSeconds

byB :: Order -> Order
byB o = o {oOwner = ownerBText, oKey = keyB}

dynamisOrder :: Order
dynamisOrder = aliceOrder {oLabel = "dynamis", oSecret = secretOf 3, oSealed = "sealed-dynamis-3", oPrice = 20000}

-- viem's selectors of nameExpires(uint256), reservedNames(bytes32) and ownerOf(uint256), as in the calls of the contracts tests
nameExpiresSelector, reservedNamesSelector, ownerOfSelector :: ByteString
nameExpiresSelector = unhex "d6e4fa86"
reservedNamesSelector = unhex "389c0748"
ownerOfSelector = unhex "6352211e"

labelBytes :: Text -> ByteString
labelBytes label = lh
  where
    LabelHash lh = labelHash label

-- | ownerOf of a name nobody holds: the registrar's bare require, a revert without data.
answerUnowned :: Ctx -> Text -> IO ()
answerUnowned Ctx {cNode} label = answerCall cNode (registrarOf cNode) (ownerOfSelector <> labelBytes label) $ Reverts ""

answerOwner :: Ctx -> Text -> Address -> IO ()
answerOwner Ctx {cNode} label owner = answerCall cNode (registrarOf cNode) (ownerOfSelector <> labelBytes label) $ Returns $ addressWord owner

answerExpiry :: Ctx -> Text -> Word64 -> IO ()
answerExpiry Ctx {cNode} label expiry = answerCall cNode (registrarOf cNode) (nameExpiresSelector <> labelBytes label) $ Returns $ uint256 expiry

-- | A register transaction of the order as the sender saves it, then moved to this outcome.
sentRegister :: Ctx -> NameRegistration -> Word8 -> TxSettlement -> IO TxHash
sentRegister Ctx {cStore} NameRegistration {nrId} n settlement = withTransaction cStore $ \db -> do
  let h = TxHash $ hashFixture n
  void $ insertTransaction db (TxRegistration RTRegister nrId) NewTransaction {ntxRaw = "raw", ntxHash = h, ntxNonce = fromIntegral n} orderTime
  recordTransactionOutcome db h TSPending settlement `shouldReturn` True
  pure h

-- | The chain answers the name as available and held by nobody.
answerFree :: Ctx -> Text -> IO ()
answerFree ctx label = answerReserved ctx label 0 >> answerUnowned ctx label

answerReserved :: Ctx -> Text -> Int -> IO ()
answerReserved Ctx {cNode} label reason = do
  answerCall cNode (registrarOf cNode) (nameExpiresSelector <> lh) $ Returns $ uint256 (0 :: Int)
  answerCall cNode fakeNamePolicy (reservedNamesSelector <> lh) $ Returns $ uint256 reason
  where
    LabelHash lh = labelHash label

-- | A paid order, settled, then written as the worker would write it, without the signature its order stored, as after a rare path cleared it.
paidIn :: Ctx -> Order -> (NameRegistration -> NameRegistration) -> IO NameRegistration
paidIn ctx o change = do
  (invId, _) <- placed ctx o
  settleNow ctx invId settled `shouldReturn` ISPaid
  r <- invoiceRegistration ctx invId
  rewrite ctx r (change . unsigned)

-- | A code order of a code of this length, then written as paidIn writes it.
codeIn :: Ctx -> Int -> Order -> (NameRegistration -> NameRegistration) -> IO (NameRegistration, PrepaidCode)
codeIn ctx len o change = do
  (text, code) <- issueCode ctx len 2 id
  r <- registrationById ctx =<< registered ctx text o
  (,code) <$> rewrite ctx r (change . unsigned)

unsigned :: NameRegistration -> NameRegistration
unsigned r = r {nrReveal = Nothing}

-- | A write outside the turns, as the worker's, at the stored version.
rewrite :: Ctx -> NameRegistration -> (NameRegistration -> NameRegistration) -> IO NameRegistration
rewrite Ctx {cStore, cClock} NameRegistration {nrId} change = do
  t <- readIORef cClock
  withTransaction cStore $ \db -> do
    current <- fromJust <$> getRegistration db nrId
    updateRegistration db (change current) t >>= \case
      Written _ -> fromJust <$> getRegistration db nrId
      refused -> error $ "not written: " <> show refused

-- | The amount and credited of the order's balance.
balanceOf :: Ctx -> NameRegistration -> IO (Maybe (USDCents, USDCents))
balanceOf Ctx {cStore} NameRegistration {nrId} = fmap (\Balance {blAmount, blCredited} -> (blAmount, blCredited)) <$> withTransaction cStore (\db -> getBalance db nrId orderTime)

stored :: Ctx -> NameRegistration -> IO NameRegistration
stored ctx = registrationById ctx . nrId

failedPhase, registeredPhase, committedPhase, linksRefusedPhase :: NameRegistration -> NameRegistration
failedPhase r = r {nrPhase = OFailed orderTime FRRegistryRefused}
registeredPhase r = r {nrPhase = ORegistered (RoundedSystemTime $ fromIntegral $ seconds orderTime + 3 * 31536000) orderTime}
committedPhase r = r {nrPhase = OCommitted committedCommit}
linksRefusedPhase r = r {nrPhase = OCommitted committedCommit, nrLinksRefused = Just (orderTime, LKContact)}

committedCommit :: Commit
committedCommit = Commit {cmTx = Just $ TxRef (TxHash $ hashFixture 8) 100, cmRevealAfter = RoundedSystemTime $ fromIntegral $ seconds orderTime + 60, cmRevealBefore = RoundedSystemTime $ fromIntegral $ seconds orderTime + 86400}

-- | Another name's order, with the page's duration for 2 years at the service's clock.
moveTo :: Ctx -> Text -> Int -> IO Order
moveTo Ctx {cClock} label n = do
  t <- readIORef cClock
  pure aliceOrder {oLabel = label, oSecret = secretOf n, oSealed = "sealed-" <> label <> "-" <> tshow n, oDuration = yearsDuration 2 t}

moveBody :: Ctx -> Secp256k1PrivateKey -> RegistrationId -> Order -> IO J.Object
moveBody ctx key rid o = clockSeconds ctx >>= moveBodyAt ctx key rid o

moveBodyAt :: Ctx -> Secp256k1PrivateKey -> RegistrationId -> Order -> Word64 -> IO J.Object
moveBodyAt ctx@Ctx {cRandom} key rid o timestamp = do
  owner <- addressFromPrivateKey cRandom key
  let signed = o {oKey = key, oOwner = addressText owner, oTimestamp = timestamp}
      fields = fieldsOf signed
  sig <- signWith ctx key $ retargetMessage rid fields timestamp
  withReveal cRandom signed $ KM.fromList [("fields", fieldsJSON fields), ("timestamp", J.toJSON timestamp), ("signature", J.String sig)]

postMove :: Ctx -> RegistrationId -> J.Object -> IO (Int, Maybe J.Object)
postMove ctx rid = post ctx (registrationPath rid <> "/name") . J.encode

move :: Ctx -> Secp256k1PrivateKey -> NameRegistration -> Order -> IO (Int, Maybe J.Object)
move ctx key NameRegistration {nrId} o = moveBody ctx key nrId o >>= postMove ctx nrId

-- | The view of an accepted move, failing the test otherwise.
moved :: Ctx -> Secp256k1PrivateKey -> NameRegistration -> Order -> IO J.Object
moved ctx key r o =
  move ctx key r o >>= \case
    (200, Just v) -> pure v
    other -> error $ "move refused: " <> show other

getView :: Ctx -> NameRegistration -> IO J.Object
getView ctx NameRegistration {nrId} = webGet (cClient ctx) (registrationPath nrId) >>= jsonObject

findBody :: Ctx -> Secp256k1PrivateKey -> Text -> Word64 -> IO J.Object
findBody ctx key owner timestamp = do
  sig <- signWith ctx key $ findMessage (either (error . show) id $ parseAnyCaseAddress owner) timestamp
  pure $ KM.fromList [("owner", J.String owner), ("timestamp", J.toJSON timestamp), ("signature", J.String sig)]

postFind :: Ctx -> J.Object -> IO (Int, Maybe J.Object)
postFind ctx = post ctx "/api/registration/find" . J.encode

fieldsIn :: J.Object -> [(Text, J.Value)] -> Expectation
fieldsIn v = mapM_ (\(k, value) -> (k, fieldOf v k) `shouldBe` (k, Just value))

refusedWithView :: Text -> J.Object -> (Int, Maybe J.Object)
refusedWithView tag view = (409, Just $ KM.fromList [("type", J.String tag), ("registration", J.Object view)])

labelHashText :: Text -> J.Value
labelHashText label = J.String $ hex lh
  where
    LabelHash lh = labelHash label

codeRow :: Ctx -> PrepaidCode -> IO PrepaidCode
codeRow Ctx {cStore} PrepaidCode {pcId} = withTransaction cStore (`getPrepaidCodeById` pcId)

-- tests

testFind :: IO ()
testFind = withOwner $ \ctx@Ctx {cStore} -> do
  forM_ [1 .. 50 :: Int] $ \i -> do
    (_, code) <- issueCode ctx 8 2 id
    let o = order ("alicebakery" <> tshow i) (fromIntegral i)
        new = NewRegistration {nrgId = oldOrder i, nrgOwner = ownerA, nrgOrder = o, nrgReveal = reveal, nrgYears = 2, nrgOrderHash = orderHash ownerA o (OTCode $ pcCodeHash code)}
    withTransaction cStore (\db -> createCodeRegistration db new code (addUTCTime (fromIntegral i) t0)) `shouldReturn` Right ()
  answerFree ctx "alicebakers"
  answerUnowned ctx "alicebakery50"
  movedOrder <- registrationById ctx (oldOrder 50) >>= \r -> rewrite ctx r failedPhase
  to <- moveTo ctx "alicebakers" 10
  void $ moved ctx keyA movedOrder to
  (InvoiceId invId, _) <- placed ctx aliceOrder
  paid <- invoiceRegistration ctx (InvoiceId invId)
  advance ctx 1
  lapsed <- paidIn ctx dynamisOrder $ \r -> r {nrPhase = OCommitted committedCommit {cmRevealBefore = RoundedSystemTime $ fromIntegral $ seconds orderTime}}
  findBody ctx keyA (T.toLower ownerAText) (seconds orderTime) >>= postFind ctx >>= \case
    (200, Just v) | Just (J.Array found) <- KM.lookup "registrations" v -> do
      let entries = map objectEntry $ toList found
      map (`fieldOf` "registrationId") entries `shouldBe` map (Just . idText) (nrId lapsed : nrId paid : [oldOrder i | i <- [50, 49 .. 3]])
      case entries of
        lapsedEntry : unpaidEntry : moveEntry : _ -> do
          (fieldOf lapsedEntry "phase", namePhase $ nrPhase lapsed) `shouldBe` (Just "committed", PCommitted)
          sort (KM.keys unpaidEntry) `shouldBe` sort ["registrationId", "invoiceId", "phase", "createdAt", "labelHash", "sealed", "balanceSource", "heldBy"]
          unpaidEntry
            `shouldBe` KM.fromList
              [ ("registrationId", idText $ nrId paid),
                ("invoiceId", J.String invId),
                ("phase", "unpaid"),
                ("createdAt", J.toJSON orderTime),
                ("labelHash", labelHashText "alicebakery"),
                ("sealed", "sealed-alicebakery-1"),
                ("balanceSource", J.Null),
                ("heldBy", J.Null)
              ]
          moveEntry
            `shouldBe` KM.fromList
              [ ("registrationId", "old50"),
                ("invoiceId", J.Null),
                ("phase", "queued"),
                ("createdAt", J.toJSON $ addUTCTime 50 t0),
                ("labelHash", labelHashText "alicebakers"),
                ("sealed", "sealed-alicebakers-10"),
                ("balanceSource", J.Null),
                ("heldBy", J.Null)
              ]
        _ -> expectationFailure "fewer than three entries"
    other -> expectationFailure $ "find refused: " <> show other
  none <- findBody ctx keyB ownerBText (seconds orderTime) >>= postFind ctx
  storeFixture "find-none" (fst none) (J.Object <$> snd none) Nothing
  none `shouldBe` (200, Just $ KM.fromList [("registrations", J.Array mempty)])
  forM_ [1, 2] $ \i -> do
    (_, code) <- issueCode ctx 8 2 id
    let o = order ("bobbakery" <> tshow i) (fromIntegral $ 100 + i)
        new = NewRegistration {nrgId = listedOrder i, nrgOwner = ownerB, nrgOrder = o, nrgReveal = reveal, nrgYears = 2, nrgOrderHash = orderHash ownerB o (OTCode $ pcCodeHash code)}
    withTransaction cStore (\db -> createCodeRegistration db new code (addUTCTime (fromIntegral i) t0)) `shouldReturn` Right ()
  void $ registrationById ctx (listedOrder 1) >>= \r -> rewrite ctx r failedPhase
  listed <- findBody ctx keyB ownerBText (seconds orderTime) >>= postFind ctx
  storeFixture "find-orders" (fst listed) (J.Object <$> snd listed) Nothing
  case listed of
    (200, Just v) | Just (J.Array found) <- KM.lookup "registrations" v ->
      map ((\e -> (fieldOf e "registrationId", fieldOf e "phase")) . objectEntry) (toList found) `shouldBe` [(Just "listed2", Just "queued"), (Just "listed1", Just "failed")]
    other -> expectationFailure $ "find refused: " <> show other
  where
    ownerB = address "0x39ad11775d14FC8c9118Bd94f374DF3c7ad5655d"
    listedOrder i = RegistrationId $ "listed" <> tshow (i :: Int)
    oldOrder i = RegistrationId $ "old" <> tshow (i :: Int)
    idText (RegistrationId t) = J.String t
    objectEntry = \case
      J.Object o -> o
      other -> error $ "not an entry: " <> show other

testFindRefused :: IO ()
testFindRefused = withOwner $ \ctx -> do
  void $ placed ctx aliceOrder
  let ts = seconds orderTime
  (findBody ctx keyB ownerAText ts >>= postFind ctx) `shouldReturn` refusedWith 400 "bad_request"
  (findBody ctx keyA ownerAText (ts - signedWindow - 1) >>= postFind ctx) `shouldReturn` refusedWith 400 "bad_request"
  (findBody ctx keyA ownerAText (ts + signedWindow + 1) >>= postFind ctx) `shouldReturn` refusedWith 400 "bad_request"
  body <- findBody ctx keyA ownerAText ts
  postFind ctx (KM.insert "owner" "0x014f9c09" body) `shouldReturn` refusedWith 400 "bad_request"
  postFind ctx (KM.delete "signature" body) `shouldReturn` refusedWith 400 "bad_request"
  post ctx "/api/registration/find" "[]" `shouldReturn` refusedWith 400 "bad_request"
  findBody ctx keyA ownerAText (ts - signedWindow) >>= postFind ctx >>= (`shouldBe` 200) . fst

testMoveRefusedPhases :: IO ()
testMoveRefusedPhases = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  to <- moveTo ctx "alicebakers" 10
  r <- paidIn ctx aliceOrder registeredPhase
  move ctx keyA r to `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r
  let phases = [OUnpaid (InvoiceId "inv"), OClosed ICExpired, OQueued, OCommitting (TxHash $ hashFixture 8), OCommitted committedCommit, ORegistering committedCommit (TxHash $ hashFixture 9), nrPhase r, OTaken, nrPhase (failedPhase r)]
      movableWith at refused = [namePhase p | p <- phases, movable at r {nrPhase = p, nrLinksRefused = refused}]
  movableWith orderTime Nothing `shouldBe` [PFailed]
  movableWith orderTime (Just (orderTime, LKContact)) `shouldBe` [PCommitted, PFailed]
  movableWith (addUTCTime (86400 + 1) orderTime) Nothing `shouldBe` [PCommitted, PFailed]

testMoveHoldsName :: IO ()
testMoveHoldsName = withOwner $ \ctx@Ctx {cStore, cStub} -> do
  answerFree ctx "alicebakers"
  (aliceInv, _) <- placed ctx aliceOrder {oLabel = "alicebakers", oSecret = secretOf 9}
  aliceRef <- maybe (error "no invoice row") irProviderRef <$> getInvoice cStore aliceInv
  bobFailed <- paidIn ctx (byB dynamisOrder) failedPhase
  to <- moveTo ctx "alicebakers" 10
  void $ moved ctx keyB bobFailed to
  calls <- stubCalls cStub
  [r | StubCancel r <- calls] `shouldBe` [aliceRef]
  nrPhase <$> invoiceRegistration ctx aliceInv `shouldReturn` OClosed ICCancelled

testMoveAccepted :: IO ()
testMoveAccepted = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  failedOrder <- paidIn ctx aliceOrder failedPhase
  to <- moveTo ctx "alicebakers" 10
  v <- moved ctx keyA failedOrder to
  fieldsIn v
    [ ("phase", phaseValue "queued" []),
      ("labelHash", labelHashText "alicebakers"),
      ("commitment", J.toJSON $ ofCommitment $ fieldsOf to),
      ("sealed", J.String $ oSealed to),
      ("duration", J.toJSON $ oDuration to),
      ("price", J.Number 10000),
      ("moves", J.Number 1),
      ("version", J.toJSON $ nrVersion failedOrder + 1),
      ("linksRefused", J.Null),
      ("label", J.String "alicebakers"),
      ("commitTx", J.Null)
    ]
  written <- stored ctx failedOrder
  (nrPhase written, nrMoves written, nrOrderHash written) `shouldBe` (OQueued, 1, nrOrderHash failedOrder)
  -- the same name with other links, after a reveal refused for its links
  refusedOrder <- paidIn ctx (byB aliceOrder) {oSecret = secretOf 2} linksRefusedPhase
  sameName <- moveTo ctx "alicebakery" 11
  v2 <- moved ctx keyB refusedOrder sameName
  fieldsIn v2 [("phase", phaseValue "queued" []), ("labelHash", labelHashText "alicebakery"), ("linksRefused", J.Null), ("commitTx", J.Null), ("moves", J.Number 1)]
  nrPhase <$> stored ctx refusedOrder `shouldReturn` OQueued
  -- a committed order with no reveal past revealBefore reads failed
  lapsed <- paidIn ctx dynamisOrder $ \r -> r {nrPhase = OCommitted committedCommit {cmRevealBefore = RoundedSystemTime $ fromIntegral $ seconds orderTime - 1}}
  heldName <- moveTo ctx "alicebakery" 12
  move ctx keyA lapsed heldName `shouldReturn` refusedWith 409 "name_taken"
  sameLapsed <- moveTo ctx "dynamis" 12
  v3 <- moved ctx keyA lapsed sameLapsed
  fieldsIn v3 [("phase", phaseValue "queued" []), ("price", J.Number 20000)]

testMoveRepeat :: IO ()
testMoveRepeat = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  -- the current fields from failed: nothing moves
  (status, v) <- move ctx keyA r aliceOrder
  (status, phaseOf =<< v, (`fieldOf` "moves") =<< v) `shouldBe` (200, Just "failed", Just (J.Number 0))
  stored ctx r `shouldReturn` r
  to <- moveTo ctx "alicebakers" 10
  body <- moveBody ctx keyA (nrId r) to
  first' <- postMove ctx (nrId r) body
  fst first' `shouldBe` 200
  movedOnce <- stored ctx r
  advance ctx 3600
  postMove ctx (nrId r) body `shouldReturn` first'
  resigned <- moveBody ctx keyA (nrId r) to
  postMove ctx (nrId r) resigned `shouldReturn` first'
  setGeneralTerms (cNode ctx) fakeNewGeneralTermsText
  postMove ctx (nrId r) body `shouldReturn` first'
  stored ctx r `shouldReturn` movedOnce
  (nrMoves &&& nrAgreedTerms) movedOnce `shouldBe` (1, agreed plainTerms)

testMoveTermsOnly :: IO ()
testMoveTermsOnly = withOwner $ \ctx -> do
  r <- paidIn ctx aliceOrder $ \p -> (failedPhase p) {nrAgreedTerms = agreed olderTerms}
  (status, v) <- move ctx keyA r aliceOrder
  (status, phaseOf =<< v, (`fieldOf` "moves") =<< v) `shouldBe` (200, Just "queued", Just (J.Number 1))
  (nrAgreedTerms &&& nrMoves) <$> stored ctx r `shouldReturn` (agreed plainTerms, 1)

testMoveTermsChanged :: IO ()
testMoveTermsChanged = withOwnerAs (listName "alicebakers") $ \ctx -> do
  listOnChain ctx "alicebakers"
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  move ctx keyA r aliceOrder {oSecret = secretOf 11, oTerms = communityTerms} `shouldReturn` termsChanged fakeGeneralTermsText Nothing
  move ctx keyA r aliceOrder {oTerms = olderTerms} `shouldReturn` termsChanged fakeGeneralTermsText Nothing
  to <- moveTo ctx "alicebakers" 10
  move ctx keyA r to {oTerms = plainTerms} `shouldReturn` termsChanged fakeCommunityTermsText (Just bakers)
  stored ctx r `shouldReturn` r
  v <- moved ctx keyA r to {oTerms = communityTerms}
  fieldsIn v [("terms", J.toJSON $ agreed communityTerms), ("community", communityEntry $ Just bakers)]
  nrAgreedTerms <$> stored ctx r `shouldReturn` agreed communityTerms

testMoveCommunityYears :: IO ()
testMoveCommunityYears = withOwnerAs (listName "alicebakers") $ \ctx -> do
  listOnChain ctx "alicebakers"
  answerFree ctx "alicebakers"
  (code, _) <- issueCode ctx 8 5 id
  r <- registrationById ctx =<< registered ctx code aliceOrder {oDuration = yearsDuration 5 orderTime}
  failed <- rewrite ctx r failedPhase
  to <- (\o -> o {oTerms = communityTerms}) <$> moveTo ctx "alicebakers" 10
  move ctx keyA failed to {oDuration = yearsDuration 5 orderTime} `shouldReturn` refusedWith 400 "bad_request"
  void $ moved ctx keyA failed to
  answerFree ctx "alicecakes"
  again <- rewrite ctx r failedPhase
  toPlain <- moveTo ctx "alicecakes" 11
  void $ moved ctx keyA again toPlain {oDuration = yearsDuration 5 orderTime}
  (nrYears &&& nrDuration) <$> stored ctx r `shouldReturn` (5, yearsDuration 5 orderTime)

testMoveCommunityPaidYears :: IO ()
testMoveCommunityPaidYears = withOwnerAs (threeYears . listName "alicebakers") $ \ctx -> do
  listOnChain ctx "alicebakers"
  answerFree ctx "alicebakers"
  answerCall (cNode ctx) fakeController threeYearsOf11 $ Returns $ rentPriceAnswer 15000
  r <- paidIn ctx aliceOrder {oDuration = yearsDuration 3 orderTime, oPrice = 15000} failedPhase
  to <- (\o -> o {oTerms = communityTerms}) <$> moveTo ctx "alicebakers" 10
  void $ moved ctx keyA r to
  (nrYears &&& nrPrice) <$> stored ctx r `shouldReturn` (3, 10000)
  balanceOf ctx r `shouldReturn` Just (5000, 5000)

testMoveTermsCheckOrder :: IO ()
testMoveTermsCheckOrder = withOwner $ \ctx@Ctx {cNode} -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  to <- (\o -> o {oTerms = olderTerms}) <$> moveTo ctx "alicebakers" 10
  ts <- clockSeconds ctx
  (moveBodyAt ctx keyA (nrId r) to (ts - signedWindow - 1) >>= postMove ctx (nrId r)) `shouldReturn` refusedWith 400 "bad_request"
  setFault cNode "eth_call" HttpFailure
  move ctx keyA r to `shouldReturn` refusedWith 503 "provider_unavailable"
  clearFault cNode "eth_call"
  void $ placed ctx dynamisOrder
  toDynamis <- moveTo ctx "dynamis" 10
  move ctx keyA r toDynamis {oTerms = olderTerms} `shouldReturn` refusedWith 409 "order_exists"
  move ctx keyA r to {oDuration = oDuration to + 3601} `shouldReturn` refusedWith 400 "bad_request"
  setGeneralTerms cNode ""
  move ctx keyA r to `shouldReturn` (503, Just $ KM.fromList [("type", "terms_unavailable"), ("wait", J.Null)])
  setGeneralTerms cNode fakeGeneralTermsText
  answerReserved ctx "alicebakers" 1
  move ctx keyA r to `shouldReturn` termsChanged fakeGeneralTermsText Nothing
  stored ctx r `shouldReturn` r
  answerExpiry ctx "alicebakery" (seconds orderTime + 2 * 31536000)
  answerOwner ctx "alicebakery" ownerA
  answer <- move ctx keyA r to
  view <- getView ctx r
  answer `shouldBe` refusedWithView "registration_changed" view
  answerFixture "move-registration-changed" answer
  phaseOf view `shouldBe` Just "registered"

testMoveLimit :: IO ()
testMoveLimit = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  forM_ (zip [1 :: Int ..] ["alicebakers", "alicebakery", "alicebakers"]) $ \(n, label) -> do
    to <- moveTo ctx label (10 + n)
    v <- moved ctx keyA r to
    fieldOf v "moves" `shouldBe` Just (J.toJSON n)
    rewrite ctx r failedPhase
  limited <- stored ctx r
  to <- moveTo ctx "alicebakery" 20
  view <- getView ctx limited
  limitAnswer <- move ctx keyA limited to {oTerms = olderTerms}
  limitAnswer `shouldBe` refusedWithView "move_limit" view
  answerFixture "move-limit" limitAnswer
  (fieldOf view "moves", phaseOf view) `shouldBe` (Just (J.Number 3), Just "failed")
  stored ctx r `shouldReturn` limited

testMoveMeetsOrder :: IO ()
testMoveMeetsOrder = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  void $ placed ctx dynamisOrder
  toDynamis <- moveTo ctx "dynamis" 10
  move ctx keyA r toDynamis `shouldReturn` refusedWith 409 "order_exists"
  let bobs = (byB aliceOrder) {oLabel = "alicebakers", oSecret = secretOf 4}
  void $ placed ctx bobs
  toForeign <- (\o -> o {oCommitment = Just $ ofCommitment $ fieldsOf bobs}) <$> moveTo ctx "alicebakers" 4
  move ctx keyA r toForeign `shouldReturn` refusedWith 409 "order_exists"
  stored ctx r `shouldReturn` r

testMoveOrderChecks :: IO ()
testMoveOrderChecks = withOwner $ \ctx@Ctx {cNode} -> do
  r <- paidIn ctx aliceOrder failedPhase
  to <- moveTo ctx "alicebakers" 10
  let withLength len = moveBody ctx keyA (nrId r) to >>= postMove ctx (nrId r) . inObject "fields" (KM.insert "length" (J.Number len))
  withLength 5 `shouldReturn` refusedWith 400 "invalid_name"
  withLength 64 `shouldReturn` refusedWith 400 "invalid_name"
  answerFree ctx "alicebakers"
  move ctx keyA r to {oDuration = oDuration to + 3601} `shouldReturn` refusedWith 400 "bad_request"
  answerReserved ctx "alicebakers" 1
  move ctx keyA r to `shouldReturn` refusedWith 409 "name_taken"
  answerCall cNode fakeNamePolicy (reservedNamesSelector <> labelBytes "alicebakers") $ Reverts ""
  move ctx keyA r to `shouldReturn` refusedWith 503 "name_check_unavailable"
  setFault cNode "eth_call" HttpFailure
  move ctx keyA r to `shouldReturn` refusedWith 503 "provider_unavailable"
  stored ctx r `shouldReturn` r

testMoveLanded :: IO ()
testMoveLanded = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  minedOrder <- paidIn ctx aliceOrder failedPhase
  (ownedOrder, code) <- codeIn ctx 7 dynamisOrder failedPhase
  answerFree ctx "carolbakery"
  notLanded <- paidIn ctx (byB aliceOrder) {oLabel = "carolbakery", oSecret = secretOf 2} failedPhase
  let expiry = seconds orderTime + 2 * 31536000
  forM_ ["alicebakery", "dynamis", "carolbakery"] $ \label -> answerExpiry ctx label expiry
  TxHash (Hex32 minedHash) <- sentRegister ctx minedOrder 1 $ TSMinedAt 120
  answerOwner ctx "dynamis" (nrOwner ownedOrder)
  void $ sentRegister ctx notLanded 2 $ TSRevertedAt 120
  answerOwner ctx "alicebakery" (nrOwner minedOrder)
  answerOwner ctx "carolbakery" (nrOwner minedOrder)
  forM_ [(minedOrder, Just minedHash), (ownedOrder, Nothing)] $ \(r, hash) -> do
    poll <- held ctx (0, 1) $ registrationPath (nrId r) <> seenVersion (nrVersion r)
    to <- moveTo ctx "alicebakers" 10
    answer <- move ctx keyA r to
    view <- getView ctx r
    answer `shouldBe` refusedWithView "registration_changed" view
    woken "the registration poll" poll `shouldReturn` (200, view)
    fieldsIn view [("phase", phaseValue "registered" ["expiresAt" J..= expiry]), ("moves", J.Number 0), ("registerTx", maybe J.Null (J.String . hex) hash)]
    written <- stored ctx r
    (namePhase (nrPhase written), nrVersion written, nrLabelHash written) `shouldBe` (PRegistered, nrVersion r + 1, nrLabelHash r)
  pcSpentAt <$> codeRow ctx code `shouldReturn` Just orderTime
  to <- moveTo ctx "alicebakers" 11
  v <- moved ctx keyB notLanded to
  fieldsIn v [("phase", phaseValue "queued" []), ("moves", J.Number 1)]

testMoveLandedReleasedCode :: IO ()
testMoveLandedReleasedCode = withOwner $ \ctx@Ctx {cStore} -> do
  answerFree ctx "alicebakers"
  (unused, unusedCode) <- codeIn ctx 7 dynamisOrder failedPhase
  (spentElsewhere, spentCode) <- codeIn ctx 8 aliceOrder failedPhase
  answerFree ctx "carolbakery"
  spender <- paidIn ctx (byB aliceOrder) {oLabel = "carolbakery", oSecret = secretOf 20} id
  let expiry = seconds orderTime + 2 * 31536000
  forM_ [("dynamis", unused), ("alicebakery", spentElsewhere)] $ \(label, r) -> do
    answerExpiry ctx label expiry
    answerOwner ctx label (nrOwner r)
  forM_ [(unused, unusedCode), (spentElsewhere, spentCode)] $ \(r, code) ->
    withTransaction cStore (\db -> releasePrepaidCode db (pcId code) (nrId r)) `shouldReturn` True
  withTransaction cStore $ \db -> do
    reservePrepaidCode db (pcId spentCode) (nrId spender) orderTime `shouldReturn` True
    spendLandedPrepaidCode db (pcId spentCode) (nrId spender) orderTime `shouldReturn` True
  spenderBefore <- stored ctx spender
  spentBefore <- codeRow ctx spentCode
  to <- moveTo ctx "alicebakers" 10
  fst <$> move ctx keyA unused to `shouldReturn` 409
  namePhase . nrPhase <$> stored ctx unused `shouldReturn` PRegistered
  (pcReservedBy &&& pcSpentAt) <$> codeRow ctx unusedCode `shouldReturn` (Just (nrId unused), Just orderTime)
  to' <- moveTo ctx "alicebakers" 11
  logs <- capturingStderr $ fst <$> move ctx keyA spentElsewhere to' `shouldReturn` 409
  namePhase . nrPhase <$> stored ctx spentElsewhere `shouldReturn` PRegistered
  length (filter (\l -> "[ERROR " `T.isInfixOf` l && hex (unHex32 $ pcCodeHash spentCode) `T.isInfixOf` l && idText (nrId spentElsewhere) `T.isInfixOf` l) $ T.lines logs) `shouldBe` 1
  codeRow ctx spentCode `shouldReturn` spentBefore
  stored ctx spender `shouldReturn` spenderBefore
  where
    idText (RegistrationId t) = t

testMovePrice :: IO ()
testMovePrice = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  cheap <- paidIn ctx aliceOrder failedPhase
  toDynamis <- moveTo ctx "dynamis" 10
  priceAnswer <- move ctx keyA cheap toDynamis
  priceAnswer `shouldBe` (409, Just $ KM.fromList [("type", "price_too_high"), ("price", J.Number 20000)])
  answerFixture "move-price-too-high" priceAnswer
  stored ctx cheap `shouldReturn` cheap
  balanceOf ctx cheap `shouldReturn` Nothing
  dear <- paidIn ctx (byB dynamisOrder) failedPhase
  toCheaper <- moveTo ctx "alicebakers" 11
  answerCallOnce (cNode ctx) fakeController (rentPriceCall 11) $ Returns $ rentPriceAnswer 9999
  move ctx keyB dear toCheaper `shouldReturn` refusedWith 503 "name_check_unavailable"
  stored ctx dear `shouldReturn` dear
  balanceOf ctx dear `shouldReturn` Nothing
  v <- moved ctx keyB dear toCheaper
  (fieldOf v "price", phaseOf v, fieldOf v "payment") `shouldBe` (Just (J.Number 10000), Just "queued", Just (J.object ["type" J..= ("invoice" :: Text), "method" J..= ("btc" :: Text)]))
  fieldOf v "balance" `shouldBe` Just (J.object ["amount" J..= (10000 :: Int), "hold" J..= J.Null, "refunded" J..= (0 :: Int)])
  nrPrice <$> stored ctx dear `shouldReturn` 10000
  balanceOf ctx dear `shouldReturn` Just (10000, 10000)

-- | dynamis for 20000, 5000 of it from another order's balance, moved to alicebakers for 10000.
testMoveKeepsFromBalance :: IO ()
testMoveKeepsFromBalance = withOwner $ \ctx@Ctx {cStore} -> do
  answerFree ctx "alicebakers"
  paying <- paidIn ctx (byB aliceOrder) failedPhase
  withTransaction cStore $ \db -> creditBalance db (nrId paying) 5000 orderTime
  let fields = fieldsOf aliceOrder {oLabel = "dynamis", oSecret = secretOf 3, oSealed = "sealed-dynamis-3"}
      spend = Spend {spSource = nrId paying, spPrice = 20000, spBalance = 5000, spMethod = Just btc}
      new = NewRegistration {nrgId = RegistrationId "diff", nrgOwner = ownerA, nrgOrder = fields, nrgReveal = reveal, nrgYears = 2, nrgOrderHash = orderHash ownerA fields (OTBalance spend)}
  withTransaction cStore $ \db -> do
    insertInvoiceRow db "inv-diff" (addUTCTime 3600 orderTime) "open"
    createPaidRegistration db new (Just BalanceUse {buSource = nrId paying, buUsed = 5000}) (paidOrder "inv-diff") {poPrice = 20000} orderTime `shouldReturn` Right ()
  settleNow ctx (InvoiceId "inv-diff") settled `shouldReturn` ISPaid
  r <- registrationById ctx (RegistrationId "diff") >>= \d -> rewrite ctx d failedPhase
  to <- moveTo ctx "alicebakers" 11
  v <- moved ctx keyA r to
  (fieldOf v "price", fieldOf v "payment") `shouldBe` (Just (J.Number 10000), Just (J.object ["type" J..= ("difference" :: Text), "method" J..= ("btc" :: Text), "fromBalance" J..= (5000 :: Int)]))
  balanceOf ctx r `shouldReturn` Just (10000, 10000)
  balanceOf ctx paying `shouldReturn` Just (0, 5000)

testMoveCodeOrder :: IO ()
testMoveCodeOrder = withOwner $ \ctx@Ctx {cStore} -> do
  answerFree ctx "alicebakers"
  (r, code@PrepaidCode {pcCodeHash}) <- codeIn ctx 8 aliceOrder failedPhase
  toShorter <- moveTo ctx "dynamis" 10
  move ctx keyA r toShorter `shouldReturn` refusedWith 409 "code_not_covering"
  stored ctx r `shouldReturn` r
  fst <$> withTransaction cStore (\db -> revokePrepaidCode db pcCodeHash orderTime) `shouldReturn` Revoked (pcId code)
  withTransaction cStore $ \db -> DB.execute db "UPDATE sx_badge_service_name_prepaid_codes SET expires_at = ? WHERE code_hash = ?" (orderTime, pcCodeHash)
  to <- moveTo ctx "alicebakers" 11
  v <- moved ctx keyA r to
  (phaseOf v, fieldOf v "price") `shouldBe` (Just "queued", Just (J.Number 0))
  (pcReservedBy &&& pcSpentAt) <$> codeRow ctx code `shouldReturn` (Just (nrId r), Nothing)

testMoveReleasedCode :: IO ()
testMoveReleasedCode = withOwner $ \ctx@Ctx {cStore} -> do
  answerFree ctx "alicebakers"
  let refusals :: [(Text, PrepaidCode -> DB.Connection -> IO (), (Int, Maybe J.Object))]
      refusals =
        [("expired", \PrepaidCode {pcCodeHash} db -> DB.execute db "UPDATE sx_badge_service_name_prepaid_codes SET expires_at = ? WHERE code_hash = ?" (orderTime, pcCodeHash), refusedWith 409 "code_expired")]
  forM_ (zip [1 :: Int ..] refusals) $ \(n, (what, change, refusal)) -> do
    let label = "codeorder" <> tshow n
    answerFree ctx label
    (r, code) <- codeIn ctx 8 aliceOrder {oLabel = label, oSecret = secretOf (30 + n)} failedPhase
    withTransaction cStore (\db -> releasePrepaidCode db (pcId code) (nrId r)) `shouldReturn` True
    withTransaction cStore $ change code
    to <- moveTo ctx "alicebakers" (40 + n)
    (what,) <$> move ctx keyA r to `shouldReturn` (what, refusal)
    stored ctx r `shouldReturn` r
  (r, code) <- codeIn ctx 8 aliceOrder failedPhase
  withTransaction cStore (\db -> releasePrepaidCode db (pcId code) (nrId r)) `shouldReturn` True
  to <- moveTo ctx "alicebakers" 50
  void $ moved ctx keyA r to
  pcReservedBy <$> codeRow ctx code `shouldReturn` Just (nrId r)

testMoveSizes :: IO ()
testMoveSizes = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  to <- moveTo ctx "alicebakers" 10
  let longest = to {oSealed = T.replicate 11095 "s"}
  body <- moveBody ctx keyA (nrId r) longest
  postMove ctx (nrId r) (KM.insert "owner" (J.String $ T.replicate 5300 "0") body) `shouldReturn` refusedWith 400 "bad_request"
  post ctx (registrationPath (nrId r) <> "/name") "[]" `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r
  -- an owner in the body is ignored
  (status, v) <- postMove ctx (nrId r) (KM.insert "owner" (J.String ownerBText) body)
  (status, (`fieldOf` "sealed") =<< v, (`fieldOf` "owner") =<< v) `shouldBe` (200, Just (J.String $ oSealed longest), Just (J.String ownerAText))

testMoveRefusedSigner :: IO ()
testMoveRefusedSigner = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  to <- moveTo ctx "alicebakers" 10
  move ctx keyB r to `shouldReturn` refusedWith 400 "bad_request"
  stored ctx r `shouldReturn` r
  let unknown = RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g"
  (moveBody ctx keyA unknown to >>= postMove ctx unknown) `shouldReturn` refusedWith 404 "not_found"

testMoveLostRace :: IO ()
testMoveLostRace = withOwner $ \ctx@Ctx {cNode} -> do
  answerFree ctx "alicebakers"
  -- the cheaper move from dynamis takes the other branch of the price check; the first round writes no move
  forM_ [aliceOrder, dynamisOrder] $ \o -> do
    r <- paidIn ctx o failedPhase
    to <- moveTo ctx "alicebakers" 10
    -- held at the register check's ownerOf read, inside the turns
    (moving, gate) <- pausedAt cNode "eth_call" $ move ctx keyA r to
    changed <- rewrite ctx r id
    releaseGate gate
    answer <- bounded "the move" $ wait moving
    view <- getView ctx changed
    answer `shouldBe` refusedWithView "registration_changed" view
    fieldOf view "version" `shouldBe` Just (J.toJSON $ nrVersion r + 1)
    stored ctx r `shouldReturn` changed
    balanceOf ctx r `shouldReturn` Nothing

-- | Each round holds the higher hash's turn while both orders start, the one moving from the lower hash first in odd
-- rounds and last in even ones, so that without the hash order some round has each order holding one turn and waiting
-- for the other's.
testMovesCross :: IO ()
testMovesCross = withOwnerAs (\cfg -> cfg {nMaxOrderMoves = 100}) $ \ctx -> do
  answerFree ctx "alicebakers"
  let (low, high) = case sortOn hashOf ["alicebakery", "alicebakers"] of
        [l, h] -> (l, h)
        _ -> error "two names"
  a <- paidIn ctx aliceOrder {oLabel = low, oSecret = secretOf 60} failedPhase
  b <- paidIn ctx (byB aliceOrder) {oLabel = high, oSecret = secretOf 61} failedPhase
  forM_ [1 .. 6 :: Int] $ \n -> do
    rs <- mapM (stored ctx) [a, b]
    let onName label = fromJust $ find ((== labelHash label) . nrLabelHash) rs
        keyOf r = if nrOwner r == nrOwner a then keyA else keyB
        bodyTo r label k = moveTo ctx label (100 + 2 * n + k) >>= moveBody ctx (keyOf r) (nrId r)
        (fromLow, fromHigh) = (onName low, onName high)
    lowMove <- (fromLow,) <$> bodyTo fromLow high 0
    highMove <- (fromHigh,) <$> bodyTo fromHigh low 1
    holding <- newEmptyMVar
    release <- newEmptyMVar
    holder <- async $ withLabelTurn (oeTurns $ cOrders ctx) (labelHash high) $ putMVar holding () >> takeMVar release
    takeMVar holding
    let started = if odd n then [lowMove, highMove] else [highMove, lowMove]
        queuedOnLow k = bounded "the move's turn" $ atomically $ labelTurnCount (oeTurns $ cOrders ctx) (labelHash low) >>= check . (== k)
    moves <- forM (zip [1 ..] started) $ \(k, (r, body)) -> async (postMove ctx (nrId r) body) <* queuedOnLow k
    putMVar release ()
    wait holder
    outcomes <- bounded ("round " <> show n) $ mapM wait moves
    outcomes `shouldBe` replicate 2 (refusedWith 409 "name_taken")
    mapM (fmap nrLabelHash . stored ctx) [a, b] `shouldReturn` map labelHash [low, high]
  where
    hashOf label = (\(LabelHash h) -> h) (labelHash label)

testPollsWoken :: IO ()
testPollsWoken = withOwner $ \ctx -> do
  answerFree ctx "alicebakers"
  r <- paidIn ctx aliceOrder failedPhase
  poll <- held ctx (0, 1) $ registrationPath (nrId r) <> seenVersion (nrVersion r)
  to <- moveTo ctx "alicebakers" 10
  void $ moved ctx keyA r to
  (status, v) <- woken "the registration poll" poll
  (status, fieldOf v "version", phaseOf v) `shouldBe` (200, Just (J.toJSON $ nrVersion r + 1), Just "queued")
