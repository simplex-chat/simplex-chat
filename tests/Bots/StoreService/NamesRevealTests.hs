{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesRevealTests (namesRevealTests) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests (noncesCall)
import Bots.StoreService.NamesOrdersTests (Ctx (..), Order (..), aliceOrder, blockedRequests, communityTerms, configured, invoiceRegistration, keyA, movedSponsor, orderSeconds, orderTime, otherSponsor, placed, post, recordingHandlers, registrationOf, settleNow, settled, withOrders)
import Bots.StoreService.NamesOwnerTests (answerFree, postSelection, selectionBody, selectionOf)
import Bots.StoreService.NamesPhasesTests (change, held, registrationPath, seenVersion, woken)
import Bots.StoreService.NamesStoreTests (hashFixture, ownerA)
import Bots.StoreService.WebTests (bounded, fieldOf, refusedWith, phaseFieldOf, statusOf, webRequestBody)
import Control.Concurrent.Async (Async, async, wait)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Data.Aeson as J
import Data.Aeson ((.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JT
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.IORef (writeIORef)
import Data.Int (Int64)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (hContentType)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Chat.PaymentService.Types (InvoiceStatus (..))
import Simplex.Messaging.Agent.Store.Common (withTransaction)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.Secp256k1 (RecoverableSignature (..), Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.Encoding (Encoding (..))
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.SimplexName (SimplexTLD (..), labelHash)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..))
import StoreService.Config (NamesConfig (nDowntime))
import StoreService.Names.Abi (hex, uint256)
import StoreService.Names.Chain (Hex32 (..), TxHash (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Registration (DisputeDays, Registration (..), TermsAcceptance (..), acceptanceDigest, linkMaxBytes, makeCommitment, maxDisputeDays, minDisputeDays, policyDomain, registerIntentDigest, sponsorDomain, termsKind)
import StoreService.Names.Reveal (RevealRequest (..))
import StoreService.Names.Orders (OrdersEnv (oeTurns))
import StoreService.Names.Store (Commit (..), FailReason (..), NameRegistration (..), NameReveal (..), OrderPhase (..), TxHistory (..), getRegistration, noHistory)
import StoreService.Names.Turns (withLabelTurn)
import StoreService.Web.Http (createLimit, holdMicros, maxBodyBytes, readLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Owner (selectNameHandler)
import StoreService.Web.Names.Registration (registrationHandler, revealHandler)
import StoreService.Web.Server (WebEnv (..), webApp)
import System.Timeout (timeout)
import Test.Hspec

namesRevealTests :: Spec
namesRevealTests = describe "StoreService names: reveal" $ do
  describe "accepted" $ do
    it "accepts a reveal with links, setting the label at a new version and waking a held poll" testWithLinks
    it "accepts a reveal without links" testWithoutLinks
    it "accepts a reveal in the downtime" testRevealDowntime
    it "accepts the reveal the page signs, as viem signs it" testViemReveal
    it "accepts a reveal with two records at the link bound, within the body cap" testRecordsAtBound
    it "answers an identical repeat in a later phase, refuses a different one by the owner, and answers one the owner did not sign as an unknown order" testRepeats
    it "answers after the name's turn, so a slow reader of one answer holds no other reveal" testAnswersAfterTurn
    it "continues an order failed with its page unread: queued again with the reveal stored and its transactions cleared, and refuses one failed otherwise" testUnreadContinues
  describe "refused" $ do
    it "refuses a high-s intent, answers a wrong signer as an unknown order, refuses a stale nonce, a deadline before revealBefore or over a week ahead, writing nothing, and accepts one a week ahead" testIntentRefusals
    it "answers an intent signed over other dispute days than the order's as an unknown order, writing nothing" testOtherDisputeDays
    it "answers an intent signed over the sponsor the service used before as an unknown order, writing nothing, and accepts one over its current sponsor" testOtherSponsor
    it "refuses a label not of the order, another secret or other links, writing nothing" testCommitmentRefusals
    it "refuses a commitment of another duration than the order's" testWrongDuration
    it "refuses a list of links for a key as bad_request, recording nothing" testSecondLink
    it "answers an unknown id 404, a malformed body 400 for a known and an unknown id alike, and refuses a registration not committed or lapsed" testStateAndFormat
    it "answers provider_unavailable when the nonce read fails, writing nothing" testNonceUnavailable
  describe "terms acceptance" $ do
    it "stores the owner's acceptance of the policy's current community terms, at the intent's deadline, with a listed name's reveal" testAcceptanceStored
    it "refuses a reveal without an acceptance, or by another signer, for other terms, of the other kind or at another nonce, a listed name's and a plain one's" testAcceptanceRefusals
    it "refuses a listed name's reveal whose acceptance is not an object, storing nothing" testAcceptanceMalformed
    it "reads an acceptance only with a 32-byte terms hash, a number nonce and a signature" testAcceptanceFields
    it "answers 503 for a listed name's reveal while its listing or the owner's policy nonce cannot be read, or before the policy has community terms, storing nothing" testAcceptanceUnavailable
  describe "races" $ do
    it "answers registration_changed with the current view when the registration changed during the checks" testLostRace
    it "answers a reveal by another signer as an unknown order without waiting for the order's turn" testOtherSignerOutsideTurn
    it "runs one at a time with /name for the name, exactly one of them taking effect" testRacesName

-- fixtures

data Fx = Fx {fCtx :: Ctx, fApp :: Application}

-- | The registration routes as namesRoutes registers them, the reveal's and the selection's included.
withReveals :: (Fx -> IO a) -> IO a
withReveals action = withOrders $ \ctx@Ctx {cNode} -> do
  answerCall cNode fakeSponsor noncesCall $ Returns $ uint256 (0 :: Int)
  serveReveals ctx action

serveReveals :: Ctx -> (Fx -> IO a) -> IO a
serveReveals ctx@Ctx {cOrders, cWeb, cHandlers} action = do
  let web = cWeb {weOrders = Just cOrders}
      routes = \case
        ["api", "registration", rid] -> Just Route {rMethod = "GET", rLimit = readLimit, rHandler = registrationHandler cOrders (weHttp web) holdMicros (RegistrationId rid)}
        ["api", "registration", rid, "reveal"] -> Just Route {rMethod = "POST", rLimit = readLimit, rHandler = revealHandler cOrders (RegistrationId rid)}
        ["api", "registration", rid, "name"] -> Just Route {rMethod = "POST", rLimit = createLimit, rHandler = selectNameHandler cOrders (weProviders web) (weWaiters web) (RegistrationId rid)}
        _ -> Nothing
      app req respond = fromMaybe (webApp web req respond) (namesApp (weHttp web) routes req respond)
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure $ recordingHandlers cHandlers app) $ \port ->
    action Fx {fCtx = ctx {cWeb = web, cClient = (mgr, "http://127.0.0.1:" <> show port)}, fApp = app}

-- | The request, held at its first chain read: a reveal's nonce read, or /name's name check, in its turns.
pausedAtChainRead :: Ctx -> IO a -> IO (Async a, Gate)
pausedAtChainRead Ctx {cNode} = pausedAt cNode "eth_call"

-- | The commit's window: opened a minute before the order's time, closing a day after it.
revealAfter, revealBefore :: Word64
revealAfter = orderSeconds - 60
revealBefore = orderSeconds + 86400

openWindow :: Commit
openWindow = Commit {cmTx = Nothing, cmRevealAfter = RoundedSystemTime $ fromIntegral revealAfter, cmRevealBefore = RoundedSystemTime $ fromIntegral revealBefore}

contactLink, channelLink :: Text
contactLink = "https://smp8.simplex.im/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rSAAAAAAAAA"
channelLink = "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA?h=smp9.simplex.im"

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
      rSecret = Hex32 $ B.replicate 32 0x5a,
      rResolver = fakeResolver,
      rContactLink = Just contactLink,
      rChannelLink = Just channelLink
    }

-- | The same owner's order for another name, at its 7-letter price.
dynamis :: Registration
dynamis = alice {rLabel = "dynamis"}

-- | At the launch prices for 2 years that the orders fixture answers: 7 letters 20000 cents, 11 letters 10000.
orderOf :: Registration -> Order
orderOf Registration {rLabel, rSecret, rContactLink, rChannelLink} = aliceOrder {oLabel = rLabel, oSecret = rSecret, oContact = rContactLink, oChannel = rChannelLink, oPrice = if T.length rLabel == 7 then 20000 else 10000}

-- | Ordered, paid, and committed by the worker with its reveal window open.
committed :: Ctx -> Order -> IO RegistrationId
committed ctx o = do
  NameRegistration {nrId} <- paidOrder ctx o
  _ <- change ctx nrId $ \r -> r {nrPhase = OCommitted openWindow}
  pure nrId

committedUnchecked :: Ctx -> Registration -> IO RegistrationId
committedUnchecked ctx r = do
  rid <- committed ctx $ orderOf r {rContactLink = Nothing, rChannelLink = Nothing}
  rid <$ change ctx rid (\x -> x {nrCommitment = makeCommitment r})

paidOrder :: Ctx -> Order -> IO NameRegistration
paidOrder ctx o = do
  (inv, _) <- placed ctx o
  settleNow ctx inv settled `shouldReturn` ISPaid
  NameRegistration {nrId} <- invoiceRegistration ctx inv
  _ <- change ctx nrId $ \r -> r {nrReveal = Nothing}
  stored ctx nrId

signIntent :: Ctx -> Secp256k1PrivateKey -> Registration -> Word64 -> Word64 -> IO RecoverableSignature
signIntent ctx key r = signIntentOver ctx key r minDisputeDays

signIntentOver :: Ctx -> Secp256k1PrivateKey -> Registration -> DisputeDays -> Word64 -> Word64 -> IO RecoverableSignature
signIntentOver = signIntentFor fakeSponsor

signIntentFor :: Address -> Ctx -> Secp256k1PrivateKey -> Registration -> DisputeDays -> Word64 -> Word64 -> IO RecoverableSignature
signIntentFor sponsor Ctx {cRandom} key r days nonce deadline =
  either error id <$> signRecoverable cRandom key (registerIntentDigest (sponsorDomain fakeChainId sponsor) r days nonce $ RoundedSystemTime $ fromIntegral deadline)

bodyOf :: RecoverableSignature -> Registration -> Word64 -> Word64 -> J.Object
bodyOf sig Registration {rLabel, rSecret, rContactLink, rChannelLink} nonce deadline =
  KM.fromList
    [ ("label", J.String rLabel),
      ("secret", J.String $ hex $ unHex32 rSecret),
      ("records", J.object ["contact" .= rContactLink, "channel" .= rChannelLink]),
      ("nonce", J.toJSON nonce),
      ("deadline", J.toJSON deadline),
      ("signature", J.String $ hex $ smpEncode sig)
    ]

-- | As the page sends it: the owner's intent for the chain's nonce 0, until the commitment's revealBefore, and the owner's
-- acceptance of the general terms.
ownerBody :: Ctx -> Registration -> IO J.Object
ownerBody ctx r = do
  sig <- signIntent ctx keyA r 0 revealBefore
  acceptance <- acceptanceOf ctx keyA r False fakeGeneralTerms 0 revealBefore
  pure $ withAcceptance acceptance $ bodyOf sig r 0 revealBefore

ownerReveal :: Ctx -> RegistrationId -> Registration -> IO (Int, Maybe J.Object)
ownerReveal ctx rid r = reveal ctx rid =<< ownerBody ctx r

revealPath :: RegistrationId -> String
revealPath rid = registrationPath rid <> "/reveal"

reveal :: Ctx -> RegistrationId -> J.Object -> IO (Int, Maybe J.Object)
reveal ctx rid = post ctx (revealPath rid) . J.encode

accepted :: (Int, Maybe J.Object)
accepted = (200, Just KM.empty)

badRequest :: (Int, Maybe J.Object)
badRequest = refusedWith 400 "bad_request"

notFound :: (Int, Maybe J.Object)
notFound = refusedWith 404 "not_found"

stored :: Ctx -> RegistrationId -> IO NameRegistration
stored Ctx {cStore} rid = withTransaction cStore $ \db -> fromJust <$> getRegistration db rid

-- | Alice's name listed by the policy, and her policy nonce at 2.
withListedReveals :: (Fx -> IO a) -> IO a
withListedReveals action = withReveals $ \fx@Fx {fCtx = Ctx {cNode}} -> do
  listCommunityName cNode $ labelHash "alicebakery"
  answerCall cNode fakeNamePolicy noncesCall $ Returns $ uint256 (2 :: Int)
  action fx

-- | Alice's order under the community terms, as the page orders a listed name.
listedOrder :: Order
listedOrder = (orderOf alice) {oTerms = communityTerms, oAcceptanceNonce = 2}

signAcceptance :: Ctx -> Secp256k1PrivateKey -> Bool -> B.ByteString -> Word64 -> IO TermsAcceptance
signAcceptance ctx key community terms nonce = acceptanceOf ctx key alice community terms nonce revealBefore

-- | The acceptance signed with the given deadline, which must be the intent's.
acceptanceOf :: Ctx -> Secp256k1PrivateKey -> Registration -> Bool -> B.ByteString -> Word64 -> Word64 -> IO TermsAcceptance
acceptanceOf Ctx {cRandom} key r community terms nonce deadline = do
  sig <- either error id <$> signRecoverable cRandom key (acceptanceDigest (policyDomain fakeChainId fakeNamePolicy) (termsKind community) (makeCommitment r) (Hex32 terms) nonce $ RoundedSystemTime $ fromIntegral deadline)
  pure TermsAcceptance {termsHash = Hex32 terms, nonce, signature = sig}

withAcceptance :: TermsAcceptance -> J.Object -> J.Object
withAcceptance TermsAcceptance {termsHash, nonce, signature} =
  KM.insert "acceptance" $ J.object ["termsHash" .= hex (unHex32 termsHash), "nonce" .= nonce, "signature" .= hex (smpEncode signature)]

testAcceptanceStored :: IO ()
testAcceptanceStored = withListedReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx listedOrder
  -- past revealBefore, as checkIntent allows: the acceptance is checked with the intent's deadline
  let deadline = revealBefore + 3600
  sig <- signIntent ctx keyA alice 0 deadline
  acceptance <- acceptanceOf ctx keyA alice True fakeCommunityTerms 2 deadline
  reveal ctx rid (withAcceptance acceptance $ bodyOf sig alice 0 deadline) `shouldReturn` accepted
  fmap rvAcceptance . nrReveal <$> stored ctx rid `shouldReturn` Just acceptance

testAcceptanceRefusals :: IO ()
testAcceptanceRefusals = do
  withListedReveals $ \Fx {fCtx = ctx} -> do
    rid <- committed ctx listedOrder
    sig <- signIntent ctx keyA alice 0 revealBefore
    let body = bodyOf sig alice 0 revealBefore
        refused acceptance = reveal ctx rid (maybe id withAcceptance acceptance body) `shouldReturn` badRequest
    refused Nothing
    refused . Just =<< signAcceptance ctx fakeRegistrarKey True fakeCommunityTerms 2
    refused . Just =<< signAcceptance ctx keyA True (C.keccak256 "SimpleX community terms v0") 2
    refused . Just =<< signAcceptance ctx keyA True fakeCommunityTerms 1
    nrReveal <$> stored ctx rid `shouldReturn` Nothing
  withReveals $ \Fx {fCtx = ctx} -> do
    rid <- committed ctx (orderOf alice)
    sig <- signIntent ctx keyA alice 0 revealBefore
    let body = bodyOf sig alice 0 revealBefore
        refused acceptance = reveal ctx rid (maybe id withAcceptance acceptance body) `shouldReturn` badRequest
    refused Nothing
    refused . Just =<< signAcceptance ctx keyA True fakeGeneralTerms 0
    refused . Just =<< signAcceptance ctx keyA False fakeCommunityTerms 0
    refused . Just =<< signAcceptance ctx keyA False fakeGeneralTerms 1
    nrReveal <$> stored ctx rid `shouldReturn` Nothing

testAcceptanceMalformed :: IO ()
testAcceptanceMalformed = withListedReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx listedOrder
  sig <- signIntent ctx keyA alice 0 revealBefore
  reveal ctx rid (KM.insert "acceptance" (J.String "acceptance") $ bodyOf sig alice 0 revealBefore) `shouldReturn` badRequest
  nrReveal <$> stored ctx rid `shouldReturn` Nothing

testAcceptanceFields :: IO ()
testAcceptanceFields = do
  g <- C.newRandom
  sig <- either error id <$> signRecoverable g keyA (C.keccak256 "acceptance")
  let fields = KM.fromList ["termsHash" .= hex fakeCommunityTerms, "nonce" .= (2 :: Int), "signature" .= hex (smpEncode sig)]
      replaced k v = J.Object $ KM.insert k v fields
      acceptanceRead a = acceptance <$> JT.parseMaybe J.parseJSON (J.Object $ KM.insert "acceptance" a $ bodyOf sig alice 0 revealBefore)
  acceptanceRead (J.Object fields) `shouldBe` Just TermsAcceptance {termsHash = Hex32 fakeCommunityTerms, nonce = 2, signature = sig}
  forM_
    [ J.String "acceptance",
      replaced "termsHash" $ J.String $ "0x" <> T.toUpper (T.drop 2 $ hex fakeCommunityTerms),
      replaced "termsHash" "0x1234",
      replaced "nonce" "2",
      replaced "signature" "0x00"
    ]
    $ \a -> acceptanceRead a `shouldBe` Nothing

testAcceptanceUnavailable :: IO ()
testAcceptanceUnavailable = withListedReveals $ \Fx {fCtx = ctx@Ctx {cNode}} -> do
  rid <- committed ctx listedOrder
  sig <- signIntent ctx keyA alice 0 revealBefore
  acceptance <- signAcceptance ctx keyA True fakeCommunityTerms 2
  let body = withAcceptance acceptance $ bodyOf sig alice 0 revealBefore
      unavailable = do
        reveal ctx rid body `shouldReturn` refusedWith 503 "provider_unavailable"
        nrReveal <$> stored ctx rid `shouldReturn` Nothing
  answerCall cNode fakeNamePolicy noncesCall $ Reverts ""
  unavailable
  answerCall cNode fakeNamePolicy noncesCall $ Returns $ uint256 (2 :: Int)
  answerListing cNode (labelHash "alicebakery") $ Reverts ""
  unavailable
  listCommunityName cNode $ labelHash "alicebakery"
  -- before the policy has community terms there are none to accept
  setCommunityTerms cNode ""
  unavailable
  setCommunityTerms cNode fakeCommunityTermsText
  reveal ctx rid body `shouldReturn` accepted

-- | The fields a refused reveal must leave as they were.
unwritten :: NameRegistration -> (Int64, Maybe NameReveal)
unwritten NameRegistration {nrVersion, nrReveal} = (nrVersion, nrReveal)

-- | The same signature with s mirrored to n - s, which recovers the same signer.
highS :: RecoverableSignature -> RecoverableSignature
highS sig@RecoverableSignature {rsS, rsRecId} = sig {rsS = i2ospOf_ 32 (n - os2ip rsS), rsRecId = 1 - rsRecId}
  where
    n = ECC.ecc_n $ ECC.common_curve $ ECC.getCurveByName ECC.SEC_p256k1

-- tests

testWithLinks :: IO ()
testWithLinks = bounded "with links" $ withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice)
  NameRegistration {nrVersion = v} <- stored ctx rid
  poll <- held ctx (0, 1) $ registrationPath rid <> seenVersion v
  sig <- signIntent ctx keyA alice 0 revealBefore
  acceptance <- acceptanceOf ctx keyA alice False fakeGeneralTerms 0 revealBefore
  reveal ctx rid (withAcceptance acceptance $ bodyOf sig alice 0 revealBefore) `shouldReturn` accepted
  NameRegistration {nrVersion, nrPhase, nrReveal} <- stored ctx rid
  (nrVersion, nrPhase) `shouldBe` (v + 1, OCommitted openWindow)
  nrReveal
    `shouldBe` Just NameReveal {rvLabel = "alicebakery", rvSecret = rSecret alice, rvContactLink = Just contactLink, rvChannelLink = Just channelLink, rvNonce = 0, rvDeadline = RoundedSystemTime $ fromIntegral revealBefore, rvSignature = sig, rvAcceptance = acceptance}
  (status, view) <- woken "registration poll" poll
  (status, fieldOf view "version", fieldOf view "label") `shouldBe` (200, Just (J.toJSON $ v + 1), Just "alicebakery")

testUnreadContinues :: IO ()
testUnreadContinues = withReveals $ \Fx {fCtx = ctx} -> do
  NameRegistration {nrId = rid} <- paidOrder ctx (orderOf alice)
  let failedAt = orderTime
      commitTx = TxHistory {thCommitTx = Just $ TxHash $ hashFixture 7, thRegisterTx = Nothing}
  _ <- change ctx rid $ \r -> r {nrPhase = OFailed failedAt FRRegistryRefused, nrHistory = commitTx}
  ownerReveal ctx rid alice `shouldReturn` badRequest
  _ <- change ctx rid $ \r -> r {nrPhase = OFailed failedAt FRUnread}
  ownerReveal ctx rid alice `shouldReturn` accepted
  (\NameRegistration {nrPhase, nrHistory, nrReveal} -> (nrPhase, nrHistory, rvLabel <$> nrReveal)) <$> stored ctx rid
    `shouldReturn` (OQueued, noHistory, Just "alicebakery")

testWithoutLinks :: IO ()
testWithoutLinks = withReveals $ \Fx {fCtx = ctx} -> do
  let bare = alice {rContactLink = Nothing, rChannelLink = Nothing}
  rid <- committed ctx (orderOf bare)
  ownerReveal ctx rid bare `shouldReturn` accepted
  fmap (\NameReveal {rvLabel, rvContactLink, rvChannelLink} -> (rvLabel, rvContactLink, rvChannelLink)) . nrReveal <$> stored ctx rid
    `shouldReturn` Just ("alicebakery", Nothing, Nothing)

testRevealDowntime :: IO ()
testRevealDowntime = withReveals $ \Fx {fCtx = open} -> do
  let bare = alice {rContactLink = Nothing, rChannelLink = Nothing}
  rid <- committed open (orderOf bare)
  serveReveals (configured (\c -> c {nDowntime = True}) open) $ \Fx {fCtx = ctx} ->
    ownerReveal ctx rid bare `shouldReturn` accepted
  fmap rvLabel . nrReveal <$> stored open rid `shouldReturn` Just "alicebakery"

-- | The webapp's dynamis order (NamesSignatureTests), revealed with viem 2.57.5's signTypedData by the webapp's wallet, account 0.
testViemReveal :: IO ()
testViemReveal = withReveals $ \Fx {fCtx = ctx} -> do
  let page = dynamis {rSecret = Hex32 $ B.replicate 32 0x11, rContactLink = Just "https://smp8.simplex.im/a#contact", rChannelLink = Nothing}
      viemSignature = "0xe4e98c38e568bddc234db14f7166fbc1a8c70a7b05e41316d7bec221ac56ae55770c94685678d89d9371c391c09a965e2dede2e3e10f1ce31e46ff5fac614e471b"
      viemAcceptance = "0x7c7dc4d7631f5325eddf2af595b524b6f7aaad7854aac7aaf748d6f116b56ad961c97acd616fcb60e0f43545ea2fa5f7bb7f96235f476c6abcc68295f93e36071c"
      body =
        KM.fromList
          [ ("label", "dynamis"),
            ("secret", J.String $ "0x" <> T.replicate 32 "11"),
            ("records", J.object ["contact" .= ("https://smp8.simplex.im/a#contact" :: Text), "channel" .= J.Null]),
            ("nonce", J.Number 0),
            ("deadline", J.Number 1792660500),
            ("signature", J.String viemSignature),
            ("acceptance", J.object ["termsHash" .= hex fakeGeneralTerms, "nonce" .= (0 :: Int), "signature" .= (viemAcceptance :: Text)])
          ]
  hex (unHex32 $ makeCommitment page) `shouldBe` "0x9cf04ddef2eedfb02e6e7ba58f879c0636c584b6acc67ffe9f97a06bfb016ac1"
  ownerBody ctx page `shouldReturn` body
  rid <- committedUnchecked ctx page
  reveal ctx rid body `shouldReturn` accepted

testRecordsAtBound :: IO ()
testRecordsAtBound = withReveals $ \Fx {fCtx = ctx} -> do
  let longest = alice {rContactLink = Just $ padded "a" linkMaxBytes, rChannelLink = Just $ padded "c" linkMaxBytes}
  rid <- committed ctx (orderOf longest)
  body <- J.encode <$> ownerBody ctx longest
  LB.length body `shouldSatisfy` (<= fromIntegral maxBodyBytes)
  post ctx (revealPath rid) body `shouldReturn` accepted

testRepeats :: IO ()
testRepeats = withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice)
  body <- ownerBody ctx alice
  reveal ctx rid body `shouldReturn` accepted
  v <- change ctx rid $ \r -> r {nrPhase = ORegistering openWindow $ TxHash $ hashFixture 9}
  reveal ctx rid body `shouldReturn` accepted
  resigned <- (\sig -> KM.insert "acceptance" (fromJust $ KM.lookup "acceptance" body) $ bodyOf sig alice 1 revealBefore) <$> signIntent ctx keyA alice 1 revealBefore
  reveal ctx rid resigned `shouldReturn` badRequest
  reveal ctx rid (KM.insert "label" "alicebakerz" body) `shouldReturn` notFound
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
  let weekAhead = orderSeconds + 7 * 86400
      pastWeek = weekAhead + 601
  late <- signIntent ctx keyA alice 0 pastWeek
  acceptance <- acceptanceOf ctx keyA alice False fakeGeneralTerms 0 revealBefore
  lateAcceptance <- acceptanceOf ctx keyA alice False fakeGeneralTerms 0 pastWeek
  forM_
    [ ("high s", withAcceptance acceptance $ bodyOf (highS sig) alice 0 revealBefore, badRequest),
      ("another signer", withAcceptance acceptance $ bodyOf other alice 0 revealBefore, notFound),
      ("a deadline a second before revealBefore", withAcceptance acceptance $ bodyOf early alice 0 (revealBefore - 1), badRequest),
      ("a deadline a week and the signed request slack ahead, and a second", withAcceptance lateAcceptance $ bodyOf late alice 0 pastWeek, badRequest)
    ]
    $ \(what, body, refusal) -> (what,) <$> reveal ctx rid body `shouldReturn` (what :: String, refusal)
  answerCall cNode fakeSponsor noncesCall $ Returns $ uint256 (1 :: Int)
  reveal ctx rid (withAcceptance acceptance $ bodyOf sig alice 0 revealBefore) `shouldReturn` badRequest
  unwritten <$> stored ctx rid `shouldReturn` unchanged
  answerCall cNode fakeSponsor noncesCall $ Returns $ uint256 (0 :: Int)
  week <- signIntent ctx keyA alice 0 weekAhead
  weekAcceptance <- acceptanceOf ctx keyA alice False fakeGeneralTerms 0 weekAhead
  reveal ctx rid (withAcceptance weekAcceptance $ bodyOf week alice 0 weekAhead) `shouldReturn` accepted

testOtherDisputeDays :: IO ()
testOtherDisputeDays = withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice)
  unchanged <- unwritten <$> stored ctx rid
  nrDisputeDays <$> stored ctx rid `shouldReturn` minDisputeDays
  acceptance <- acceptanceOf ctx keyA alice False fakeGeneralTerms 0 revealBefore
  sig <- signIntentOver ctx keyA alice maxDisputeDays 0 revealBefore
  reveal ctx rid (withAcceptance acceptance $ bodyOf sig alice 0 revealBefore) `shouldReturn` notFound
  unwritten <$> stored ctx rid `shouldReturn` unchanged
  ownerReveal ctx rid alice `shouldReturn` accepted

testOtherSponsor :: IO ()
testOtherSponsor = withReveals $ \Fx {fCtx = open} -> do
  rid <- committed open (orderOf alice)
  unchanged <- unwritten <$> stored open rid
  serveReveals (configured movedSponsor open) $ \Fx {fCtx = ctx} -> do
    ownerReveal ctx rid alice `shouldReturn` notFound
    unwritten <$> stored ctx rid `shouldReturn` unchanged
    acceptance <- acceptanceOf ctx keyA alice False fakeGeneralTerms 0 revealBefore
    sig <- signIntentFor otherSponsor ctx keyA alice minDisputeDays 0 revealBefore
    reveal ctx rid (withAcceptance acceptance $ bodyOf sig alice 0 revealBefore) `shouldReturn` accepted
    fmap rvLabel . nrReveal <$> stored ctx rid `shouldReturn` Just "alicebakery"

testCommitmentRefusals :: IO ()
testCommitmentRefusals = withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice)
  unchanged <- unwritten <$> stored ctx rid
  forM_
    [ ("a label of another hash", alice {rLabel = "alicebakerz"}),
      ("a label in upper case", alice {rLabel = "Alicebakery"}),
      ("another secret", alice {rSecret = Hex32 $ B.replicate 32 0x5b}),
      ("another contact link", alice {rContactLink = Just "https://smp8.simplex.im/a#other"}),
      ("no channel link", alice {rChannelLink = Nothing})
    ]
    $ \(what, r) -> (what,) <$> ownerReveal ctx rid r `shouldReturn` (what :: String, badRequest)
  unwritten <$> stored ctx rid `shouldReturn` unchanged
  ownerReveal ctx rid alice `shouldReturn` accepted

-- | The commitment is of another duration than the order states; the intent, of the order's, passes on its own.
testWrongDuration :: IO ()
testWrongDuration = withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committedUnchecked ctx alice {rDuration = rDuration alice + 1}
  nrDuration <$> stored ctx rid `shouldReturn` rDuration alice
  ownerReveal ctx rid alice `shouldReturn` badRequest
  nrReveal <$> stored ctx rid `shouldReturn` Nothing

testSecondLink :: IO ()
testSecondLink = withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice)
  unchanged <- unwritten <$> stored ctx rid
  body <- ownerBody ctx alice
  let records contact channel = KM.insert "records" (J.object ["contact" .= contact, "channel" .= channel]) body
  reveal ctx rid (records [contactLink, "https://smp8.simplex.im/a#other"] [channelLink]) `shouldReturn` badRequest
  reveal ctx rid (records [contactLink] [channelLink]) `shouldReturn` badRequest
  unwritten <$> stored ctx rid `shouldReturn` unchanged

testStateAndFormat :: IO ()
testStateAndFormat = withReveals $ \Fx {fCtx = ctx@Ctx {cClock}} -> do
  body <- ownerBody ctx alice
  let unknown = RegistrationId "t2Jk8RmQ4vXw9NcP3yLb6g"
  reveal ctx unknown body `shouldReturn` notFound
  NameRegistration {nrId = rid} <- paidOrder ctx (orderOf alice)
  reveal ctx rid body `shouldReturn` badRequest
  _ <- change ctx rid $ \r -> r {nrPhase = OCommitted openWindow}
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
      ("records not an object", KM.insert "records" (J.toJSON [contactLink]) body),
      ("a link that is not text", KM.insert "records" (J.object ["contact" .= J.Number 1, "channel" .= channelLink]) body)
    ]
    $ \(what, b) -> forM_ [rid, unknown] $ \r -> (what, r == rid,) <$> reveal ctx r b `shouldReturn` (what :: String, r == rid, badRequest)
  forM_ [("not an object", "[]"), ("not JSON", "label=alicebakery"), ("over the cap", J.encode $ KM.insert "pad" (J.String $ T.replicate maxBodyBytes "a") body)] $
    \(what, b) -> forM_ [rid, unknown] $ \r -> (what, r == rid,) <$> post ctx (revealPath r) b `shouldReturn` (what :: String, r == rid, badRequest)
  unwritten <$> stored ctx rid `shouldReturn` unchanged
  -- signed past revealBefore, so only the lapse refuses it
  let deadline = revealBefore + 3600
  acceptance <- acceptanceOf ctx keyA alice False fakeGeneralTerms 0 deadline
  late <- (\sig -> withAcceptance acceptance $ bodyOf sig alice 0 deadline) <$> signIntent ctx keyA alice 0 deadline
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
  reveal ctx rid body `shouldReturn` refusedWith 503 "provider_unavailable"
  clearFault cNode "eth_call"
  unwritten <$> stored ctx rid `shouldReturn` unchanged
  reveal ctx rid body `shouldReturn` accepted

-- | A writer that takes no turn, as the worker re-committing, moves the version while the reveal reads the nonce.
testLostRace :: IO ()
testLostRace = bounded "lost race" $ withReveals $ \Fx {fCtx = ctx} -> do
  rid <- committed ctx (orderOf alice)
  body <- ownerBody ctx alice
  (inFlight, gate) <- pausedAtChainRead ctx (reveal ctx rid body)
  v <- change ctx rid $ \x -> x {nrPhase = OCommitted openWindow {cmRevealAfter = RoundedSystemTime $ fromIntegral $ revealAfter + 60, cmRevealBefore = RoundedSystemTime $ fromIntegral $ revealBefore + 60}}
  releaseGate gate
  (status, answer) <- wait inFlight
  let view = registrationOf $ fromJust answer
  (status, (`fieldOf` "type") =<< answer, fieldOf view "version", fieldOf view "label", phaseFieldOf view "revealBefore")
    `shouldBe` (409, Just "registration_changed", Just (J.toJSON v), Just J.Null, Just (J.toJSON $ revealBefore + 60))
  unwritten <$> stored ctx rid `shouldReturn` (v, Nothing)

testOtherSignerOutsideTurn :: IO ()
testOtherSignerOutsideTurn = bounded "another signer outside the turn" $ withReveals $ \Fx {fCtx = ctx@Ctx {cOrders}} -> do
  rid <- committed ctx (orderOf alice)
  NameRegistration {nrLabelHash} <- stored ctx rid
  sig <- signIntent ctx fakeRegistrarKey alice 0 revealBefore
  acceptance <- acceptanceOf ctx keyA alice False fakeGeneralTerms 0 revealBefore
  withLabelTurn (oeTurns cOrders) nrLabelHash (timeout 2000000 $ reveal ctx rid $ withAcceptance acceptance $ bodyOf sig alice 0 revealBefore)
    `shouldReturn` Just notFound

testRacesName :: IO ()
testRacesName = bounded "races /name" $ withReveals $ \Fx {fCtx = ctx} -> do
  -- the reveal holds the name's turn while it reads the nonce, so /name waits and then finds it revealed
  rid <- committed ctx (orderOf alice)
  answerFree ctx "alicebakers"
  toAlicebakers <- selectionOf ctx "alicebakers" 1
  (inFlight, gate) <- pausedAtChainRead ctx (ownerReveal ctx rid alice)
  selection <- async $ postSelection ctx rid =<< selectionBody ctx keyA rid toAlicebakers
  blockedRequests ctx 1
  releaseGate gate
  wait inFlight `shouldReturn` accepted
  wait selection `shouldReturn` badRequest
  NameRegistration {nrLabelHash, nrReveal} <- stored ctx rid
  (nrLabelHash, rvLabel <$> nrReveal) `shouldBe` (labelHash "alicebakery", Just "alicebakery")
