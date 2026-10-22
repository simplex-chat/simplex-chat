{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesRevealTests (namesRevealTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (noncesCall)
import Bots.StoreService.NamesOrdersTests (Ctx (..), Order (..), aliceOrder, invoiceRegistration, orderTime, placed, post, registrationOf, settleNow, settled, withOrders)
import Bots.StoreService.NamesPhasesTests (change, held, prompt, registrationPath, seenVersion, woken)
import Bots.StoreService.NamesStoreTests (ownerA)
import Bots.StoreService.WebTests (bounded, jsonObject, statusOf, webGet, webRequestBody)
import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM
import Control.Monad (forM_, replicateM, when)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Data.Aeson as J
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.IORef (writeIORef)
import Data.Int (Int64)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word64)
import Network.HTTP.Client (Manager, RequestBody (..), defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody, requestHeaders, responseBody, responseStatus)
import Network.HTTP.Types (hContentType)
import Network.Wai (Application, responseLBS, strictRequestBody)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.PaymentService.Types (InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature (..), Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.Eth.EIP712 (serializeSignature)
import Simplex.Messaging.SimplexName (LabelHash (..), SimplexTLD (..), labelHash)
import Simplex.Messaging.Util (tshow)
import StoreService.Config (ChainConfig (..))
import StoreService.Names.Abi (hex, uint256)
import StoreService.Names.Chain (newChainClient)
import StoreService.Names.Contracts (newContracts)
import StoreService.Names.Links (linkMaxBytes)
import StoreService.Names.Orders (OrdersEnv (..))
import StoreService.Names.Phases (RegistrationId (..), publish)
import StoreService.Names.Registration (Registration (..), makeCommitment, registerIntentDigest, sponsorDomain)
import StoreService.Names.Signatures (OrderFields (..))
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), NameReveal (..), VersionWrite (..), getRegistration, retargetRegistration)
import StoreService.Names.Turns (labelTurnCount, withLabelTurns)
import StoreService.Web.Http (holdMicros, maxBodyBytes, nameBodyBytes, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Registration (registrationHandler, revealHandler)
import StoreService.Web.Server (WebEnv (..), webApp)
import System.Timeout (timeout)
import Test.Hspec

namesRevealTests :: Spec
namesRevealTests = describe "StoreService names: reveal" $ do
  describe "accepted" $ do
    it "accepts a reveal with links, setting the label at a new version and waking a held poll" testWithLinks
    it "accepts a reveal without links" testWithoutLinks
    it "accepts the reveal the page signs, as viem signs it" testViemReveal
    it "accepts a reveal with two records at the link bound, over the 8192-byte cap" testRecordsAtBound
    it "answers an identical repeat in a later phase, and refuses a different one" testRepeats
    it "answers after the name's turn, so a slow reader of one answer holds no other reveal" testAnswersAfterTurn
  describe "refused" $ do
    it "refuses a high-s intent, a wrong signer, a stale nonce, and a deadline past or before revealBefore, writing nothing" testIntentRefusals
    it "refuses a label not of the order, another secret or other links, writing nothing" testCommitmentRefusals
    it "refuses a commitment of another duration than the order's" testWrongDuration
    it "refuses a second link for one key with invalid_link, recording nothing" testSecondLink
    it "refuses an unknown id, a registration not committed or lapsed, and malformed bodies" testStateAndFormat
    it "answers provider_unavailable when the nonce read fails, writing nothing" testNonceUnavailable
  describe "links refusal" $ do
    it "records invalid_link for a committed link one byte over the bound, waking a held poll, once" testLinksRefused
    it "records no refusal for links that do not rebuild the commitment, or for another signer" testNoRefusalRecorded
  describe "races" $ do
    it "answers registration_changed with the current view when the registration changed during the checks" testLostRace
    it "runs one at a time with /name for the name, exactly one of them taking effect" testRacesName
    it "takes one read token per request, whatever its outcome" testReadToken

-- fixtures

-- | The fake node behind a proxy that, once armed, holds the next eth_call until released.
data Gate = Gate {gArmed :: TVar Bool, gArrived :: TMVar (), gRelease :: TMVar ()}

data Fx = Fx {fCtx :: Ctx, fGate :: Gate, fApp :: Application}

-- | The registration routes as namesRoutes registers them, the reveal's included.
withReveals :: (Fx -> IO a) -> IO a
withReveals action = withOrders $ \ctx@Ctx {cNode, cOrders, cWeb, cRandom} -> do
  gate <- Gate <$> newTVarIO False <*> newEmptyTMVarIO <*> newEmptyTMVarIO
  mgr <- newManager defaultManagerSettings
  answerCall cNode fakeSponsor noncesCall $ Returns $ uint256 (0 :: Int)
  Warp.testWithApplication (pure $ gatedNode gate mgr cNode) $ \nodePort -> do
    ct <- newContracts TLDSimplex =<< newChainClient cRandom (fakeChainConfig cNode) {cRpcUrl = "http://127.0.0.1:" <> show nodePort}
    let orders = cOrders {oeContracts = ct}
        web = cWeb {weOrders = Just orders}
        routes = \case
          ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler orders holdMicros rid}
          ["api", "registration", rid, "reveal"] -> Just Route {rMethod = "POST", rLimit = readLimit, rHandler = revealHandler orders rid}
          _ -> Nothing
        app req respond = fromMaybe (webApp web req respond) (namesApp (weHttp web) routes req respond)
    Warp.testWithApplication (pure app) $ \port ->
      action Fx {fCtx = ctx {cOrders = orders, cWeb = web, cClient = (mgr, "http://127.0.0.1:" <> show port)}, fGate = gate, fApp = app}

gatedNode :: Gate -> Manager -> FakeEthNode -> Application
gatedNode Gate {gArmed, gArrived, gRelease} mgr FakeEthNode {fnUrl} req respond = do
  body <- strictRequestBody req
  paused <- atomically $ readTVar gArmed >>= \armed -> armed <$ when armed (writeTVar gArmed False >> putTMVar gArrived ())
  when paused $ atomically $ takeTMVar gRelease
  forward <- parseRequest fnUrl
  r <- httpLbs forward {method = "POST", requestBody = RequestBodyLBS body, requestHeaders = [(hContentType, "application/json")]} mgr
  respond $ responseLBS (responseStatus r) [(hContentType, "application/json")] (responseBody r)

-- | The request, held at its first chain read: a reveal's nonce read, its last check before it writes.
pausedAtChainRead :: Gate -> IO a -> IO (Async a)
pausedAtChainRead Gate {gArmed, gArrived} request = do
  atomically $ writeTVar gArmed True
  inFlight <- async request
  timeout prompt (atomically $ takeTMVar gArrived) >>= maybe (cancel inFlight >> fail "the request did not reach a chain read") pure
  pure inFlight

releaseRead :: Gate -> IO ()
releaseRead Gate {gRelease} = atomically $ putTMVar gRelease ()

orderSeconds :: Word64
orderSeconds = floor $ utcTimeToPOSIXSeconds orderTime

-- | The commit's window: opened a minute before the order's time, closing a day after it.
revealAfter, revealBefore :: Word64
revealAfter = orderSeconds - 60
revealBefore = orderSeconds + 86400

keyA :: Secp256k1PrivateKey
keyA = oKey aliceOrder

contactLink, channelLink :: Text
contactLink = "https://smp8.simplex.im/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rS"
channelLink = "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4Lm?v=1"

-- | A short link of the kind's path padded to this many bytes.
padded :: Text -> Int -> Text
padded kind n = base <> T.replicate (n - T.length base) "a"
  where
    base = "https://smp6.simplex.im/" <> kind <> "#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw?x="

alice :: Registration
alice =
  Registration
    { rLabel = "alicebakery",
      rTLD = TLDSimplex,
      rOwner = ownerA,
      rDuration = oDuration aliceOrder,
      rSecret = B.replicate 32 0x5a,
      rResolver = fakeResolver,
      rContactLink = Just contactLink,
      rChannelLink = Just channelLink
    }

-- | The same owner's order for another name, at its 7-letter price.
dynamis :: Registration
dynamis = alice {rLabel = "dynamis"}

-- | At the launch prices for 2 years that the orders fixture answers: 7 letters 20000 cents, 11 letters 10000.
orderOf :: Registration -> Order
orderOf r@Registration {rLabel} = aliceOrder {oLabel = rLabel, oCommitment = makeCommitment r, oPrice = if T.length rLabel == 7 then 20000 else 10000}

-- | Ordered, paid, and committed by the worker with its reveal window open.
committed :: Ctx -> Order -> IO RegistrationId
committed ctx o = do
  NameRegistration {nrId} <- paidOrder ctx o
  _ <- change ctx nrId $ \r -> r {nrPhase = PCommitted, nrRevealAfter = Just revealAfter, nrRevealBefore = Just revealBefore}
  pure nrId

paidOrder :: Ctx -> Order -> IO NameRegistration
paidOrder ctx o = do
  (inv, _) <- placed ctx o
  settleNow ctx inv settled `shouldReturn` Right ISPaid
  invoiceRegistration ctx inv

signIntent :: Ctx -> Secp256k1PrivateKey -> Registration -> Word64 -> Word64 -> IO RecoverableSignature
signIntent Ctx {cRandom} key r nonce deadline =
  either error id <$> signRecoverable cRandom key (registerIntentDigest (sponsorDomain fakeChainId fakeSponsor) r nonce deadline)

bodyOf :: RecoverableSignature -> Registration -> Word64 -> Word64 -> J.Object
bodyOf sig Registration {rLabel, rSecret, rContactLink, rChannelLink} nonce deadline =
  KM.fromList
    [ ("label", J.String rLabel),
      ("secret", J.String $ hex rSecret),
      ("records", J.object ["contact" .= maybeToList rContactLink, "channel" .= maybeToList rChannelLink]),
      ("nonce", J.toJSON nonce),
      ("deadline", J.toJSON deadline),
      ("signature", J.String $ hex $ serializeSignature sig)
    ]

-- | As the page sends it: the owner's intent for the chain's nonce 0, until the commitment's revealBefore.
ownerBody :: Ctx -> Registration -> IO J.Object
ownerBody ctx r = (\sig -> bodyOf sig r 0 revealBefore) <$> signIntent ctx keyA r 0 revealBefore

withRecords :: [Text] -> [Text] -> J.Object -> J.Object
withRecords contact channel = KM.insert "records" $ J.object ["contact" .= contact, "channel" .= channel]

ownerReveal :: Ctx -> RegistrationId -> Registration -> IO (Int, Maybe J.Object)
ownerReveal ctx rid r = reveal ctx rid =<< ownerBody ctx r

revealPath :: RegistrationId -> String
revealPath rid = registrationPath rid <> "/reveal"

reveal :: Ctx -> RegistrationId -> J.Object -> IO (Int, Maybe J.Object)
reveal ctx rid = post ctx (revealPath rid) . J.encode

accepted :: (Int, Maybe J.Object)
accepted = (200, Just KM.empty)

refused :: Int -> Text -> (Int, Maybe J.Object)
refused status tag = (status, Just $ KM.singleton "error" (J.String tag))

badRequest :: (Int, Maybe J.Object)
badRequest = refused 400 "bad_request"

stored :: Ctx -> RegistrationId -> IO NameRegistration
stored Ctx {cStore} rid = withTransaction cStore $ \db -> fromJust <$> getRegistration db rid

-- | The fields a refused reveal must leave as they were.
unwritten :: NameRegistration -> (Int64, Maybe NameReveal, Bool)
unwritten NameRegistration {nrVersion, nrReveal, nrLinksRefusedAt} = (nrVersion, nrReveal, isJust nrLinksRefusedAt)

field :: Text -> J.Object -> Maybe J.Value
field k = KM.lookup (K.fromText k)

-- | The same signature with s mirrored to n - s, which recovers the same signer.
highS :: RecoverableSignature -> RecoverableSignature
highS sig@RecoverableSignature {rsS, rsRecId} = sig {rsS = i2ospOf_ 32 (n - os2ip rsS), rsRecId = 1 - rsRecId}
  where
    n = ECC.ecc_n $ ECC.common_curve $ ECC.getCurveByName ECC.SEC_p256k1

-- | /name as section 8 runs it from committed: only after a reveal refused for its links, under both names' turns, a compare-and-set.
moveName :: Ctx -> IO () -> RegistrationId -> OrderFields -> IO (Either Text Int64)
moveName ctx@Ctx {cStore, cOrders = OrdersEnv {oeTurns, oePhases}} inTurns rid new@OrderFields {ofLabelHash} = do
  NameRegistration {nrLabelHash} <- stored ctx rid
  withLabelTurns oeTurns nrLabelHash ofLabelHash $ do
    inTurns
    r@NameRegistration {nrPhase, nrLinksRefusedAt, nrPrice} <- stored ctx rid
    if nrPhase == PCommitted && isNothing nrLinksRefusedAt
      then pure $ Left "bad_request"
      else
        withTransaction cStore (\db -> retargetRegistration db r new nrPrice orderTime) >>= \case
          Written v -> Right v <$ atomically (publish oePhases rid v)
          other -> pure $ Left $ tshow other

-- | Waits until this many requests hold or wait for the label's turn.
queuedOn :: Ctx -> LabelHash -> Int -> IO ()
queuedOn Ctx {cOrders = OrdersEnv {oeTurns}} lh n =
  timeout prompt (atomically $ labelTurnCount oeTurns lh >>= check . (== n)) >>= maybe (fail $ show n <> " requests did not queue on the turn") pure

fieldsOf :: Registration -> OrderFields
fieldsOf r@Registration {rLabel, rDuration} =
  OrderFields {ofLabelHash = labelHash rLabel, ofLength = T.length rLabel, ofDuration = rDuration, ofCommitment = makeCommitment r, ofSealed = "sealed-" <> rLabel}

-- tests

testWithLinks :: IO ()
testWithLinks = bounded "with links" $ withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice)
  NameRegistration {nrVersion = v} <- stored ctx rid
  poll <- held ctx (0, 1) $ registrationPath rid <> seenVersion v
  sig <- signIntent ctx keyA alice 0 revealBefore
  reveal ctx rid (bodyOf sig alice 0 revealBefore) `shouldReturn` accepted
  NameRegistration {nrVersion, nrPhase, nrReveal, nrLinksRefusedAt} <- stored ctx rid
  (nrVersion, nrPhase, nrLinksRefusedAt) `shouldBe` (v + 1, PCommitted, Nothing)
  nrReveal
    `shouldBe` Just NameReveal {rvLabel = "alicebakery", rvSecret = rSecret alice, rvContactLink = Just contactLink, rvChannelLink = Just channelLink, rvNonce = 0, rvDeadline = revealBefore, rvSignature = sig}
  (status, view) <- woken "registration poll" poll
  (status, field "version" view, field "label" view) `shouldBe` (200, Just (J.toJSON $ v + 1), Just "alicebakery")

testWithoutLinks :: IO ()
testWithoutLinks = withReveals $ \Fx {fCtx = ctx} -> do
  let bare = alice {rContactLink = Nothing, rChannelLink = Nothing}
  rid <- committed ctx (orderOf bare)
  ownerReveal ctx rid bare `shouldReturn` accepted
  fmap (\NameReveal {rvLabel, rvContactLink, rvChannelLink} -> (rvLabel, rvContactLink, rvChannelLink)) . nrReveal <$> stored ctx rid
    `shouldReturn` Just ("alicebakery", Nothing, Nothing)

-- | The webapp's dynamis order (NamesSignatureTests), revealed with viem 2.57.5's signTypedData by the webapp's wallet, account 0.
testViemReveal :: IO ()
testViemReveal = withReveals $ \Fx {fCtx = ctx} -> do
  let page = dynamis {rSecret = B.replicate 32 0x11, rContactLink = Just "https://smp8.simplex.im/a#contact", rChannelLink = Nothing}
      viemSignature = "0xed92eb1e37a191cb270651b57fd132eb652357a90aa602e8c29ed1dfabe0a14738f0bfe38fc2d9687ad5e0820cad2f5af386f00087cb91b4eb6577c2ec2869641c"
      body =
        KM.fromList
          [ ("label", "dynamis"),
            ("secret", J.String $ "0x" <> T.replicate 32 "11"),
            ("records", J.object ["contact" .= ["https://smp8.simplex.im/a#contact" :: Text], "channel" .= ([] :: [Text])]),
            ("nonce", J.Number 0),
            ("deadline", J.Number 1792660500),
            ("signature", J.String viemSignature)
          ]
  hex (makeCommitment page) `shouldBe` "0x9cf04ddef2eedfb02e6e7ba58f879c0636c584b6acc67ffe9f97a06bfb016ac1"
  (\sig -> bodyOf sig page 0 revealBefore) <$> signIntent ctx keyA page 0 revealBefore `shouldReturn` body
  rid <- committed ctx (orderOf page)
  reveal ctx rid body `shouldReturn` accepted

testRecordsAtBound :: IO ()
testRecordsAtBound = withReveals $ \Fx {fCtx = ctx} -> do
  let longest = alice {rContactLink = Just $ padded "a" linkMaxBytes, rChannelLink = Just $ padded "c" linkMaxBytes}
  rid <- committed ctx (orderOf longest)
  body <- J.encode <$> ownerBody ctx longest
  LB.length body `shouldSatisfy` (\n -> n > fromIntegral maxBodyBytes && n <= fromIntegral nameBodyBytes)
  post ctx (revealPath rid) body `shouldReturn` accepted

testRepeats :: IO ()
testRepeats = withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice)
  body <- ownerBody ctx alice
  reveal ctx rid body `shouldReturn` accepted
  v <- change ctx rid $ \r -> r {nrPhase = PRegistering}
  reveal ctx rid body `shouldReturn` accepted
  resigned <- (\sig -> bodyOf sig alice 0 revealBefore) <$> signIntent ctx keyA alice 1 revealBefore
  reveal ctx rid resigned `shouldReturn` badRequest
  reveal ctx rid (KM.insert "label" "alicebakerz" body) `shouldReturn` badRequest
  nrVersion <$> stored ctx rid `shouldReturn` v

-- | A response held before it is written, as for a client that reads it slowly, must not hold the name's turn.
testAnswersAfterTurn :: IO ()
testAnswersAfterTurn = bounded "answers after the turn" $ withReveals $ \Fx {fCtx = ctx, fApp} -> do
  rid <- committed ctx (orderOf alice)
  body <- ownerBody ctx alice
  writing <- newEmptyMVar
  release <- newEmptyMVar
  let slow req respond = fApp req $ \r -> putMVar writing () >> takeMVar release >> respond r
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure slow) $ \port -> do
    first' <- async $ statusOf <$> webRequestBody (mgr, "http://127.0.0.1:" <> show port) "POST" (revealPath rid) [(hContentType, "application/json")] (J.encode body)
    takeMVar writing
    timeout 2000000 (reveal ctx rid body) `shouldReturn` Just accepted
    putMVar release ()
    wait first' `shouldReturn` 200

testIntentRefusals :: IO ()
testIntentRefusals = withReveals $ \Fx {fCtx = ctx@Ctx {cNode}} -> do
  rid <- committed ctx (orderOf alice)
  unchanged <- unwritten <$> stored ctx rid
  sig <- signIntent ctx keyA alice 0 revealBefore
  other <- signIntent ctx fakeRegistrarKey alice 0 revealBefore
  early <- signIntent ctx keyA alice 0 (revealBefore - 1)
  past <- signIntent ctx keyA alice 0 (orderSeconds - 1)
  forM_
    [ ("high s", bodyOf (highS sig) alice 0 revealBefore),
      ("another signer", bodyOf other alice 0 revealBefore),
      ("a deadline a second before revealBefore", bodyOf early alice 0 (revealBefore - 1)),
      ("a past deadline", bodyOf past alice 0 (orderSeconds - 1))
    ]
    $ \(what, body) -> (what,) <$> reveal ctx rid body `shouldReturn` (what :: String, badRequest)
  answerCall cNode fakeSponsor noncesCall $ Returns $ uint256 (1 :: Int)
  reveal ctx rid (bodyOf sig alice 0 revealBefore) `shouldReturn` badRequest
  unwritten <$> stored ctx rid `shouldReturn` unchanged
  answerCall cNode fakeSponsor noncesCall $ Returns $ uint256 (0 :: Int)
  reveal ctx rid (bodyOf sig alice 0 revealBefore) `shouldReturn` accepted

testCommitmentRefusals :: IO ()
testCommitmentRefusals = withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice)
  unchanged <- unwritten <$> stored ctx rid
  forM_
    [ ("a label of another hash", alice {rLabel = "alicebakerz"}),
      ("a label in upper case", alice {rLabel = "Alicebakery"}),
      ("another secret", alice {rSecret = B.replicate 32 0x5b}),
      ("another contact link", alice {rContactLink = Just "https://smp8.simplex.im/a#other"}),
      ("no channel link", alice {rChannelLink = Nothing})
    ]
    $ \(what, r) -> (what,) <$> ownerReveal ctx rid r `shouldReturn` (what :: String, badRequest)
  unwritten <$> stored ctx rid `shouldReturn` unchanged
  ownerReveal ctx rid alice `shouldReturn` accepted

-- | The commitment is of another duration than the order states; the intent, of the order's, passes on its own.
testWrongDuration :: IO ()
testWrongDuration = withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice {rDuration = rDuration alice + 1})
  nrDuration <$> stored ctx rid `shouldReturn` rDuration alice
  ownerReveal ctx rid alice `shouldReturn` badRequest
  nrReveal <$> stored ctx rid `shouldReturn` Nothing

testSecondLink :: IO ()
testSecondLink = withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice)
  unchanged <- unwritten <$> stored ctx rid
  body <- ownerBody ctx alice
  reveal ctx rid (withRecords [contactLink, "https://smp8.simplex.im/a#other"] [channelLink] body) `shouldReturn` refused 400 "invalid_link"
  reveal ctx rid (withRecords [contactLink] [channelLink, channelLink] body) `shouldReturn` refused 400 "invalid_link"
  unwritten <$> stored ctx rid `shouldReturn` unchanged

testStateAndFormat :: IO ()
testStateAndFormat = withReveals $ \Fx {fCtx = ctx@Ctx {cClock}} -> do
  body <- ownerBody ctx alice
  reveal ctx (RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g") body `shouldReturn` refused 404 "not_found"
  NameRegistration {nrId = rid} <- paidOrder ctx (orderOf alice)
  reveal ctx rid body `shouldReturn` badRequest
  _ <- change ctx rid $ \r -> r {nrPhase = PCommitted, nrRevealAfter = Just revealAfter, nrRevealBefore = Just revealBefore}
  unchanged <- unwritten <$> stored ctx rid
  let text k = case KM.lookup k body of
        Just (J.String t) -> t
        _ -> error "not text"
      upper t = "0x" <> T.toUpper (T.drop 2 t)
  forM_
    [ ("a signature in upper case", KM.insert "signature" (J.String $ upper $ text "signature") body),
      ("no signature", KM.delete "signature" body),
      ("a label that is not text", KM.insert "label" (J.Number 1) body),
      ("a secret in upper case", KM.insert "secret" (J.String $ upper $ text "secret") body),
      ("a short secret", KM.insert "secret" (J.String $ T.dropEnd 2 $ text "secret") body),
      ("a nonce as text", KM.insert "nonce" (J.String "0") body),
      ("a fractional deadline", KM.insert "deadline" (J.Number 1792660500.5) body),
      ("no records", KM.delete "records" body),
      ("records without a channel list", KM.insert "records" (J.object ["contact" .= [contactLink]]) body),
      ("a link that is not text", KM.insert "records" (J.object ["contact" .= [J.Number 1], "channel" .= [channelLink]]) body)
    ]
    $ \(what, b) -> (what,) <$> reveal ctx rid b `shouldReturn` (what :: String, badRequest)
  forM_ [("not an object", "[]"), ("not JSON", "label=alicebakery"), ("over the cap", J.encode $ KM.insert "pad" (J.String $ T.replicate nameBodyBytes "a") body)] $
    \(what, b) -> (what,) <$> post ctx (revealPath rid) b `shouldReturn` (what :: String, badRequest)
  unwritten <$> stored ctx rid `shouldReturn` unchanged
  -- signed past revealBefore, so only the lapse refuses it
  let deadline = revealBefore + 3600
  late <- (\sig -> bodyOf sig alice 0 deadline) <$> signIntent ctx keyA alice 0 deadline
  writeIORef cClock $ posixSecondsToUTCTime $ fromIntegral revealBefore + 1
  reveal ctx rid late `shouldReturn` badRequest
  unwritten <$> stored ctx rid `shouldReturn` unchanged
  writeIORef cClock orderTime
  reveal ctx rid late `shouldReturn` accepted

testNonceUnavailable :: IO ()
testNonceUnavailable = withReveals $ \Fx {fCtx = ctx@Ctx {cNode}} -> do
  rid <- committed ctx (orderOf alice)
  unchanged <- unwritten <$> stored ctx rid
  body <- ownerBody ctx alice
  setFault cNode "eth_call" HttpFailure
  reveal ctx rid body `shouldReturn` refused 503 "provider_unavailable"
  clearFault cNode "eth_call"
  unwritten <$> stored ctx rid `shouldReturn` unchanged
  reveal ctx rid body `shouldReturn` accepted

testLinksRefused :: IO ()
testLinksRefused = bounded "links refused" $ withReveals $ \Fx {fCtx = ctx@Ctx {cClient}} -> do
  let overBound = alice {rContactLink = Just $ padded "a" (linkMaxBytes + 1)}
  rid <- committed ctx (orderOf overBound)
  NameRegistration {nrVersion = v} <- stored ctx rid
  poll <- held ctx (0, 1) $ registrationPath rid <> seenVersion v
  body <- ownerBody ctx overBound
  reveal ctx rid body `shouldReturn` refused 400 "invalid_link"
  (status, view) <- woken "registration poll" poll
  (status, field "version" view, field "linksRefused" view, field "label" view) `shouldBe` (200, Just (J.toJSON $ v + 1), Just (J.Bool True), Just J.Null)
  NameRegistration {nrVersion, nrReveal, nrLinksRefusedAt} <- stored ctx rid
  (nrVersion, nrReveal, nrLinksRefusedAt) `shouldBe` (v + 1, Nothing, Just orderTime)
  -- another browser, holding only the order link
  r <- webGet cClient (registrationPath rid)
  other <- jsonObject r
  (statusOf r, field "linksRefused" other) `shouldBe` (200, Just (J.Bool True))
  reveal ctx rid body `shouldReturn` refused 400 "invalid_link"
  unwritten <$> stored ctx rid `shouldReturn` (v + 1, Nothing, True)

testNoRefusalRecorded :: IO ()
testNoRefusalRecorded = withReveals $ \Fx {fCtx = ctx} -> do
  committedGood <- committed ctx (orderOf alice)
  goodBefore <- unwritten <$> stored ctx committedGood
  ownerReveal ctx committedGood alice {rContactLink = Just "hello"} `shouldReturn` badRequest
  unwritten <$> stored ctx committedGood `shouldReturn` goodBefore
  let overBound = dynamis {rContactLink = Just $ padded "a" (linkMaxBytes + 1)}
  committedBad <- committed ctx (orderOf overBound)
  badBefore <- unwritten <$> stored ctx committedBad
  other <- signIntent ctx fakeRegistrarKey overBound 0 revealBefore
  reveal ctx committedBad (bodyOf other overBound 0 revealBefore) `shouldReturn` badRequest
  unwritten <$> stored ctx committedBad `shouldReturn` badBefore
  ownerReveal ctx committedBad overBound `shouldReturn` refused 400 "invalid_link"
  isJust . nrLinksRefusedAt <$> stored ctx committedBad `shouldReturn` True

-- | A writer that takes no turn, as the worker re-committing, moves the version while the reveal reads the nonce.
testLostRace :: IO ()
testLostRace = bounded "lost race" $ withReveals $ \Fx {fCtx = ctx, fGate} -> do
  let overBound = dynamis {rContactLink = Just $ padded "a" (linkMaxBytes + 1)}
  forM_ [(alice, "accepted"), (overBound, "links refused")] $ \(r, what :: String) -> do
    rid <- committed ctx (orderOf r)
    body <- ownerBody ctx r
    inFlight <- pausedAtChainRead fGate (reveal ctx rid body)
    v <- change ctx rid $ \x -> x {nrRevealAfter = (+ 60) <$> nrRevealAfter x, nrRevealBefore = (+ 60) <$> nrRevealBefore x}
    releaseRead fGate
    (status, answer) <- wait inFlight
    let view = registrationOf $ fromJust answer
    (what, status, field "error" =<< answer, field "version" view, field "label" view, field "linksRefused" view, field "revealBefore" view)
      `shouldBe` (what, 409, Just "registration_changed", Just (J.toJSON v), Just J.Null, Just (J.Bool False), Just (J.toJSON $ revealBefore + 60))
    (what,) . unwritten <$> stored ctx rid `shouldReturn` (what, (v, Nothing, False))

testRacesName :: IO ()
testRacesName = bounded "races /name" $ withReveals $ \Fx {fCtx = ctx, fGate} -> do
  let moved = dynamis {rContactLink = Just contactLink}
  -- the reveal holds the name's turn while it reads the nonce, so /name waits and then finds it revealed
  rid <- committed ctx (orderOf alice)
  inFlight <- pausedAtChainRead fGate (ownerReveal ctx rid alice)
  move <- async $ moveName ctx (pure ()) rid (fieldsOf moved)
  queuedOn ctx (labelHash "alicebakery") 2
  releaseRead fGate
  wait inFlight `shouldReturn` accepted
  wait move `shouldReturn` Left "bad_request"
  NameRegistration {nrLabelHash, nrReveal} <- stored ctx rid
  (nrLabelHash, rvLabel <$> nrReveal) `shouldBe` (labelHash "alicebakery", Just "alicebakery")
  -- /name holds the turns of a reveal refused for its links, so the reveal waits and then finds the order moved
  let overBound = dynamis {rContactLink = Just $ padded "a" (linkMaxBytes + 1)}
  refusedRid <- committed ctx (orderOf overBound)
  body <- ownerBody ctx overBound
  reveal ctx refusedRid body `shouldReturn` refused 400 "invalid_link"
  entered <- newEmptyMVar
  release <- newEmptyMVar
  move' <- async $ moveName ctx (putMVar entered () >> takeMVar release) refusedRid (fieldsOf moved {rLabel = "dynamit"})
  takeMVar entered
  late <- async $ reveal ctx refusedRid body
  queuedOn ctx (labelHash "dynamis") 2
  putMVar release ()
  Right v <- wait move'
  (status, answer) <- wait late
  let view = registrationOf $ fromJust answer
      LabelHash dynamit = labelHash "dynamit"
  (status, field "error" =<< answer, field "version" view, field "phase" view, field "labelHash" view)
    `shouldBe` (409, Just "registration_changed", Just (J.toJSON v), Just "queued", Just (J.String $ hex dynamit))

testReadToken :: IO ()
testReadToken = withReveals $ \Fx {fCtx = ctx@Ctx {cClient}} -> do
  rid <- committed ctx (orderOf alice)
  body <- J.encode <$> ownerBody ctx alice
  let client = [(hContentType, "application/json"), ("X-Forwarded-For", "203.0.113.9")]
      from verb path b = statusOf <$> webRequestBody cClient verb path client b
      round' =
        sequence
          [ from "POST" (revealPath rid) body,
            from "POST" (revealPath rid) "{}",
            from "POST" (revealPath $ RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g") body,
            from "GET" (registrationPath rid) ""
          ]
  concat <$> replicateM 15 round' `shouldReturn` concat (replicate 15 [200, 400, 404, 200])
  from "POST" (revealPath rid) body `shouldReturn` 429
  from "GET" (registrationPath rid) "" `shouldReturn` 429
