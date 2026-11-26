{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesSearchTests (namesSearchTests, aliceHex) where

import Bots.StoreService.FakeEthNode
import Bots.StoreService.NamesContractsTests
  ( alice,
    answerRegistered,
    answerRegistrar,
    channelTextCall,
    contactTextCall,
    gracePeriod,
    nameExpiresCall,
    noncesCall,
    now,
    otherResolver,
    registrarOf,
    rentPriceAnswer,
    rentPriceCall,
    reservedNamesCall,
    resolverOf,
    withContracts,
    year,
  )
import Bots.StoreService.NamesOrdersTests (communityEntry, ownerAText, termsEntry, testNamesConfig, threeYears)
import Bots.StoreService.NamesProfilesTests (bakeryLink, channelData, contactData, jpeg, linkDataProfile, newsLink, parsed)
import Bots.StoreService.NamesStoreTests (atNonce, commitFixture, hashFixture, issue, newEdit, order, ownerA, placeCode, setChannel, t0, usable)
import Bots.StoreService.WebTests (answerOf, refusedWith, responseFixture, withServiceStore, withApp)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM_)
import Crypto.Random (ChaChaDRG)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as JT
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime, getCurrentTime)
import Network.HTTP.Client (Manager, Request, RequestBody (..), httpLbs, method, parseRequest, requestBody, responseHeaders)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (status404, status429)
import Simplex.Chat.Types (ChatPeerType (..), ImageData (..), Profile (..))
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.SystemTime (RoundedSystemTime (..))
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Util (safeDecodeUtf8)
import StoreService.Config (ListenerConfig (..), NamesConfig (..))
import StoreService.Names.Abi (AbiValue (..), abiBytes, abiTuple, hex, selector, uint256)
import StoreService.Names.Chain (TxHash (..))
import StoreService.Names.Contracts (Contracts (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Registration (LinkKind (..), linkMaxBytes)
import StoreService.Names.Profiles (HostResolver, ProfileFetcher, ProfileLimits (..), defaultProfileLimits, monotonicSeconds, newProfiles)
import StoreService.Names.Store (EditOutcome (..), FailReason (..), NameRegistrationOrder (..), OrderPhase (..), VersionWrite (..), getRegistration, insertRecordEdit, settleRecordEdit, updateRegistration)
import StoreService.Web.Http (HttpEnv (..), maxSearches, newHttpEnv, notFound, previewLimit, searchLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Search (previewHandler, recordLinks, searchHandler, signHandler, termsHandler)
import Test.Hspec

namesSearchTests :: Spec
namesSearchTests = describe "StoreService names: search" $ do
  g <- runIO C.newRandom
  describe "search" $ do
    it "answers an available name with the chain's price for the paid years, the term start and the order's contracts" $ testAvailable g
    it "answers a name the chain has available as taken while an order here holds it, its register in flight or failed included, without a price, and as available once its month ended" $ testTaken g
    it "answers a registered name with its entities, raw records, owner, resolver, nonce, expiry and grace period" $ testRegistered g
    it "holds a search slot through the chain reads only, answering rate_limited while every slot is taken" $ testSearchSlot g
    it "answers a listed name's community entry when available, reserved, being registered here and registered, a plain answer for an ini line the policy does not list, the community terms on chain as they change, and 503 while its listing is unread" $ testCommunity g
    it "answers an available name's terms null while the terms of its kind cannot be read: not set, of a link the service does not take, or of a text not fetched now" $ testTermsUnavailable g
    it "answers a name the policy lists without an ini line as a community name without its text" $ testCommunityWithoutText g
    it "answers a listed name's price for its community's 2 years, whatever years the configuration gives" $ testCommunityYears g
    it "answers a name with a word the service refuses at the order as available, as for any name" $ testUnavailableAtOrderOnly g
    it "takes the first 5 short links of a record's kind, splitting on ';', and none from other values" testRecordLinks
    it "answers a registered name's record edits left and when the next one returns, counting the edits of 365 days, reverted too" $ testRecordEdits g
    it "answers provider_unavailable to any failed chain read, records and price included, and to a price under the floor, and reads no terms link for a registered name" $ testFailedReads g
    it "fails a search whose reads together pass one deadline, though each read meets it" $ testOneDeadline g
    it "fails a search whose terms text arrives past the deadline, and answers the text that fetch kept to the next search" $ testSlowTerms g
    it "answers bad_request to a hash not in 0x and 64 lowercase hex, before the length" $ testBadHash g
    it "answers invalid_name to a length not a decimal integer in 6-63" $ testBadLength g
  describe "signing read" $
    it "answers the owner's sponsor nonce and the acceptance the policy asks of the name's kind, bad_request to a malformed hash or an owner not in its checksum case, and name_check_unavailable while either read fails" $ testSignRead g
  describe "link preview" $ do
    it "answers each kind of chat as an entity without its links" $ testPreviewKinds g
    it "answers invalid_link to anything but one short link within the link bound" $ testPreviewInvalid g
    it "answers bad_request to a body that is not a JSON object or is over its cap" $ testPreviewBadBody g
  describe "terms" $ do
    it "answers either current terms' link and text by its hash, not_found to another hash, an unset or untaken link, terms_unavailable with Retry-After to a text not fetched now, provider_unavailable when unread and bad_request to a malformed hash" $ testTerms g

-- harness

data Search = Search {node :: FakeEthNode, store :: DBStore, client :: (Manager, String), fetched :: TVar [Text], searches :: TVar Int, searchesAtFetch :: TVar [Int]}

-- | The routes as namesRoutes registers them, since a NamesEnv needs a running chat controller.
withSearch :: TVar ChaChaDRG -> (NamesConfig -> NamesConfig) -> (Contracts -> Contracts) -> (Search -> IO a) -> IO a
withSearch g adjustConfig adjust action = withServiceStore $ \store -> withContracts g $ \node ct -> do
  fetched <- newTVarIO []
  searchesAtFetch <- newTVarIO []
  http@HttpEnv {heSearches} <- newHttpEnv ListenerConfig {lHost = "127.0.0.1", lBadgesPort = 0, lBadgesSite = Nothing, lTrustForwardedFor = False, lNamesSite = Nothing, lMetricsPort = Nothing}
  let fetch kind sl = atomically (readTVar heSearches >>= modifyTVar' searchesAtFetch . (:)) >> fakeFetch fetched kind sl
  ps <- newProfiles defaultProfileLimits {fetchTimeout = fetchMicros} monotonicSeconds fetch stubResolver
  let cfg = adjustConfig $ testNamesConfig node
      routes = \case
        ["api", "link", "preview"] -> Just Route {rMethod = "POST", rLimit = previewLimit, rHandler = previewHandler ps}
        ["api", "terms", hash] -> Just Route {rMethod = "GET", rLimit = searchLimit, rHandler = termsHandler (adjust ct) hash}
        ["api", "name", hash] -> Just Route {rMethod = "GET", rLimit = searchLimit, rHandler = searchHandler cfg http store (adjust ct) ps (pure t0) hash}
        ["api", "name", hash, "sign"] -> Just Route {rMethod = "GET", rLimit = searchLimit, rHandler = signHandler (adjust ct) hash}
        _ -> Nothing
      app req respond = fromMaybe (respond notFound) (namesApp http routes req respond)
  withApp app $ \client -> action Search {node, store, client, fetched, searches = heSearches, searchesAtFetch}

aliceCommunity :: Text
aliceCommunity = "bakers and bakeries in Lisbon"

listAlice :: NamesConfig -> NamesConfig
listAlice cfg = cfg {nCommunity = M.singleton alice aliceCommunity}

-- | The answer as it reads for a name the policy lists, with its text from the ini and, when available, the policy's community terms.
listed :: Answer -> Answer
listed = listedUnder fakeCommunityTermsText

-- | As 'listed', under the community terms of this text.
listedUnder :: ByteString -> Answer -> Answer
listedUnder text = withTerms (Just $ termsEntry text) . withField "community" (communityEntry $ Just aliceCommunity)

withTerms :: Maybe J.Value -> Answer -> Answer
withTerms terms (status, answer) = (status, (\o -> maybe o (\f -> KM.insert "found" (withFoundTerms f) o) (KM.lookup "found" o)) <$> answer)
  where
    withFoundTerms (J.Object f) | KM.lookup "type" f == Just (str "available") = J.Object $ maybe (KM.delete "terms" f) (\t -> KM.insert "terms" t f) terms
    withFoundTerms f = f

withField :: J.Key -> J.Value -> Answer -> Answer
withField k v (status, answer) = (status, KM.insert k v <$> answer)

fetchMicros :: Int
fetchMicros = 500000

-- | Answers by the tag at the start of the link's key; a link tagged Gone is not read.
fakeFetch :: TVar [Text] -> ProfileFetcher
fakeFetch fetched _ sl = do
  let l = decodeUtf8 $ strEncode sl
  atomically $ modifyTVar' fetched (l :)
  pure $ Right $ case T.drop 1 $ snd $ T.breakOn "#" l of
    key
      | "Y8m3" `T.isPrefixOf` key -> Just $ contactData False bakery
      | "Rk27" `T.isPrefixOf` key -> Just $ channelData Nothing (Just 1204)
      | "Bot" `T.isPrefixOf` key -> Just $ contactData False helper {peerType = Just CPTBot}
      | "Biz" `T.isPrefixOf` key -> Just $ contactData True helper
      | otherwise -> Nothing
  where
    bakery = linkDataProfile "Alice's Bakery" "Sourdough and pastries, Lisbon" (Just $ ImageData jpeg)
    helper = linkDataProfile "helper" "" Nothing

-- | private.test resolves to a private address, any other name to a public one.
stubResolver :: HostResolver
stubResolver = \case
  "private.test" -> pure [THIPv4 (10, 0, 0, 1)]
  _ -> pure [THIPv4 (1, 1, 1, 1)]

-- | A short link with a 32-byte key starting with the tag, which the chat core parses.
taggedLink :: Text -> Char -> Text -> Text
taggedLink host kind tag = "https://" <> host <> "/" <> T.singleton kind <> "#" <> tag <> T.replicate (keyChars - T.length tag) "A"
  where
    keyChars = 43

fetchedLinks :: Search -> IO [Text]
fetchedLinks Search {fetched} = readTVarIO fetched

type Answer = (Int, Maybe J.Object)

send :: Search -> String -> (Request -> Request) -> IO (HTTP.Response LB.ByteString)
send Search {client = (mgr, base)} path modify = do
  req <- parseRequest (base <> path)
  httpLbs (modify req) mgr

searchName :: Search -> Text -> String -> IO Answer
searchName s hash query = answerOf <$> send s ("/api/name/" <> T.unpack hash <> query) id

searchAlice :: Search -> Int -> IO Answer
searchAlice s len = searchName s aliceHex ("?length=" <> show len)

fixtureOf :: String -> IO (HTTP.Response LB.ByteString) -> IO Answer
fixtureOf name request = request >>= \r -> answerOf r <$ responseFixture name r

searchFixture :: String -> Search -> Int -> IO Answer
searchFixture name s len = fixtureOf name $ send s ("/api/name/" <> T.unpack aliceHex <> "?length=" <> show len) id

postPreview :: Search -> LB.ByteString -> IO (HTTP.Response LB.ByteString)
postPreview s body = send s "/api/link/preview" $ \req -> req {method = "POST", requestBody = RequestBodyLBS body}

previewOf :: Search -> Text -> IO Answer
previewOf s l = answerOf <$> postPreview s (J.encode $ J.object ["link" .= l])

ok :: [(J.Key, J.Value)] -> Answer
ok = (200,) . Just . KM.fromList

-- chain

-- viem: keccak256 of "alicebakery"
aliceHex :: Text
aliceHex = "0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6"

-- viem: getAddress of the fake node's owner, default resolver and another resolver
ownerText, resolverText, otherResolverText :: Text
ownerText = "0x014f9c09CcEeDEF1B713110d04D3a07d5B5D9b73"
resolverText = "0x2279B7A0a67DB372996a5FaB50D91eAA73d2eBe6"
otherResolverText = "0x4826533B4897376654Bb4d4AD88B7faFD0C98528"

nameState :: FakeEthNode -> Integer -> Integer -> IO ()
nameState node expires reason = do
  answerRegistrar node
  setBlock node 100 now
  answerCall node (registrarOf node) nameExpiresCall $ Returns $ uint256 expires
  answerCall node fakeNamePolicy reservedNamesCall $ Returns $ uint256 reason

available, reserved :: FakeEthNode -> IO ()
available node = nameState node 0 0
reserved node = nameState node 0 1

registeredWith :: FakeEthNode -> Text -> Text -> IO ()
registeredWith node contact channel = do
  nameState node (toInteger $ now + year) 0
  answerRegistered node (resolverOf node)
  answerCall node (resolverOf node) contactTextCall $ Returns $ textAnswer contact
  answerCall node (resolverOf node) channelTextCall $ Returns $ textAnswer channel
  answerCall node (resolverOf node) noncesCall $ Returns $ uint256 (0 :: Int)

textAnswer :: Text -> ByteString
textAnswer t = abiTuple [Dynamic $ abiBytes $ encodeUtf8 t]

str :: Text -> J.Value
str = J.String

found :: [(J.Key, J.Value)] -> Answer
found fields = ok ["found" .= J.object fields]

availableAnswer :: Int -> Answer
availableAnswer price = found ["type" .= str "available", "price" .= price, "years" .= (2 :: Int), "terms" .= termsEntry fakeGeneralTermsText]

registeredAnswer :: [J.Value] -> J.Value -> Text -> Answer
registeredAnswer entities records resolver =
  found
    [ "type" .= str "registered",
      "owner" .= ownerText,
      "resolver" .= resolver,
      "expiresAt" .= (now + year),
      "gracePeriod" .= gracePeriod,
      "entities" .= entities,
      "records" .= records
    ]

editsLeft :: Int -> Maybe UTCTime -> J.Value
editsLeft left nextAt = J.object $ ["left" .= left] <> maybe [] (\t -> ["nextAt" .= t]) nextAt

reservedAnswer, takenAnswer :: Answer
reservedAnswer = found ["type" .= str "reserved"]
takenAnswer = found ["type" .= str "taken"]

ownRecords :: Text -> Text -> J.Value
ownRecords contact channel = J.object ["type" .= str "own", "contact" .= contact, "channel" .= channel, "nonce" .= (0 :: Int), "edits" .= editsLeft 3 Nothing]

otherRecords :: J.Value
otherRecords = J.object ["type" .= str "other"]

-- | A preview's fields, and with the link and its deep link a search answer's entity.
chat :: Text -> [(J.Key, J.Value)] -> [(J.Key, J.Value)]
chat kind profile = ["kind" .= kind, "profile" .= J.object ["type" .= str "ok", "linkProfile" .= J.object profile]]

contactChat :: Text -> Text -> Maybe Text -> Text -> [(J.Key, J.Value)]
contactChat displayName fullName image chatKind =
  chat "contact" $ ["kind" .= chatKind, "displayName" .= displayName, "fullName" .= fullName] <> maybe [] (\i -> ["image" .= i]) image

bakeryChat, newsChat :: [(J.Key, J.Value)]
bakeryChat = contactChat "Alice's Bakery" "Sourdough and pastries, Lisbon" (Just jpeg) "contact"
newsChat = chat "channel" ["kind" .= str "channel", "displayName" .= str "Bakery news", "fullName" .= str "Daily bakes and opening hours", "subscribers" .= (1204 :: Int)]

entity :: [(J.Key, J.Value)] -> Text -> Text -> J.Value
entity fields l deepLink = J.object ["preview" .= J.object fields, "link" .= l, "deepLink" .= deepLink]

fullLink :: Text
fullLink = "https://simplex.chat/contact#/?v=2&smp=smp%3A%2F%2Fabc"

timed :: IO a -> IO (a, NominalDiffTime)
timed action = do
  start <- getCurrentTime
  a <- action
  (a,) . (`diffUTCTime` start) <$> getCurrentTime

setPhase :: DBStore -> OrderPhase -> IO ()
setPhase st phase = withTransaction st $ \db -> do
  r <- fromJust <$> getRegistration db (RegistrationId "r1")
  updateRegistration db r {nrPhase = phase} t0 >>= (`shouldSatisfy` written)
  where
    written = \case
      Written _ -> True
      _ -> False

-- search

testAvailable :: TVar ChaChaDRG -> IO ()
testAvailable g = withSearch g id id $ \s@Search {node} -> do
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  answerCall node fakeController (rentPriceCall 6) $ Returns $ rentPriceAnswer 40000
  searchFixture "search-available" s 12 `shouldReturn` availableAnswer 10000
  searchAlice s 6 `shouldReturn` availableAnswer 40000

testUnavailableAtOrderOnly :: TVar ChaChaDRG -> IO ()
testUnavailableAtOrderOnly g = withSearch g (\c -> c {nUnavailable = ["bakery"]}) id $ \s@Search {node} -> do
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  searchAlice s 12 `shouldReturn` availableAnswer 10000

testTaken :: TVar ChaChaDRG -> IO ()
testTaken g = withSearch g id id $ \s@Search {node, store} -> do
  available node
  code <- issue store 1 usable
  placeCode store "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
  searchAlice s 12 `shouldReturn` takenAnswer
  setPhase store $ ORegistering commitFixture $ TxHash $ hashFixture 9
  -- no price is answered, so reading one would fail the search
  searchFixture "search-taken" s 12 `shouldReturn` takenAnswer
  setPhase store $ OFailed t0 FRRegistryRefused
  searchAlice s 12 `shouldReturn` takenAnswer
  setPhase store $ OFailed t0 FRMonthEnded
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  searchAlice s 12 `shouldReturn` availableAnswer 10000
  setPhase store $ ORegistered (RoundedSystemTime 1861833600) Nothing t0
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  searchAlice s 12 `shouldReturn` availableAnswer 10000

testRegistered :: TVar ChaChaDRG -> IO ()
testRegistered g = withSearch g id id $ \s@Search {node} -> do
  registeredWith node bakeryLink newsLink
  searchFixture "search-registered" s 12
    `shouldReturn` registeredAnswer
      [ entity bakeryChat bakeryLink "simplex:/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rSAAAAAAAAA?h=smp8.simplex.im",
        entity newsChat newsLink "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA?h=smp9.simplex.im"
      ]
      (ownRecords bakeryLink newsLink)
      resolverText

testSearchSlot :: TVar ChaChaDRG -> IO ()
testSearchSlot g = withSearch g id id $ \s@Search {node, searches, searchesAtFetch} -> do
  registeredWith node bakeryLink newsLink
  fst <$> searchAlice s 12 `shouldReturn` 200
  readTVarIO searchesAtFetch `shouldReturn` [0, 0]
  atomically $ writeTVar searches maxSearches
  searchFixture "search-rate-limited" s 12 `shouldReturn` withField "retryAfter" (J.Number 1) (refusedWith 429 "rate_limited")

testCommunity :: TVar ChaChaDRG -> IO ()
testCommunity g = withSearch g listAlice id $ \s@Search {node, store} -> do
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  -- an ini line for a name the policy does not list: a plain name
  searchAlice s 12 `shouldReturn` availableAnswer 10000
  listCommunityName node alice
  searchFixture "search-available-community" s 12 `shouldReturn` listed (availableAnswer 10000)
  reserved node
  searchFixture "search-reserved-community" s 12 `shouldReturn` listed reservedAnswer
  available node
  code <- issue store 1 usable
  placeCode store "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
  setPhase store $ ORegistering commitFixture $ TxHash $ hashFixture 9
  searchAlice s 12 `shouldReturn` listed takenAnswer
  registeredWith node fullLink ""
  let registered = registeredAnswer [] (ownRecords fullLink "") resolverText
  searchAlice s 12 `shouldReturn` listed registered
  -- new terms on chain: the available answer carries them; a listing that cannot be read: no answer
  available node
  setCommunityTerms node fakeNewCommunityTermsText
  setPhase store $ ORegistered (RoundedSystemTime 1861833600) Nothing t0
  searchAlice s 12 `shouldReturn` listedUnder fakeNewCommunityTermsText (availableAnswer 10000)
  answerListing node alice $ Reverts ""
  searchAlice s 12 `shouldReturn` refusedWith 503 "provider_unavailable"

testTermsUnavailable :: TVar ChaChaDRG -> IO ()
testTermsUnavailable g = withSearch g listAlice id $ \s@Search {node} -> do
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  setGeneralTerms node ""
  searchFixture "search-available-without-terms" s 12 `shouldReturn` withTerms Nothing (availableAnswer 10000)
  listCommunityName node alice
  searchAlice s 12 `shouldReturn` listed (availableAnswer 10000)
  setCommunityTerms node ""
  searchAlice s 12 `shouldReturn` withTerms Nothing (listed $ availableAnswer 10000)
  answerListing node alice $ Returns $ uint256 (0 :: Int)
  answerCall node fakeNamePolicy (selector "generalTermsLink()") $ Returns $ textAnswer "https://example.org/terms.md"
  nextBlock node
  searchAlice s 12 `shouldReturn` withTerms Nothing (availableAnswer 10000)
  setGeneralTerms node fakeNewGeneralTermsText
  answerText node (fakeTermsPath fakeNewGeneralTermsText) $ TextAnswer status429 [("Retry-After", "2")] ""
  searchAlice s 12 `shouldReturn` withTerms Nothing (availableAnswer 10000)

testTerms :: TVar ChaChaDRG -> IO ()
testTerms g = withSearch g id id $ \s@Search {node} -> do
  let terms h = send s ("/api/terms/" <> T.unpack h) id
      termsAnswer text = ok ["link" .= fakeTermsLink text, "text" .= decodeUtf8 text]
      otherLink = "https://example.org/terms.md"
  fixtureOf "terms" (terms $ hex fakeGeneralTerms) `shouldReturn` termsAnswer fakeGeneralTermsText
  answerOf <$> terms (hex fakeCommunityTerms) `shouldReturn` termsAnswer fakeCommunityTermsText
  fixtureOf "terms-not-found" (terms $ hex fakeNewCommunityTerms) `shouldReturn` refusedWith 404 "not_found"
  setCommunityTerms node fakeNewCommunityTermsText
  answerOf <$> terms (hex fakeNewCommunityTerms) `shouldReturn` termsAnswer fakeNewCommunityTermsText
  answerOf <$> terms (hex fakeCommunityTerms) `shouldReturn` refusedWith 404 "not_found"
  setGeneralTerms node ""
  answerOf <$> terms (hex $ C.keccak256 "") `shouldReturn` refusedWith 404 "not_found"
  answerOf <$> terms (hex fakeGeneralTerms) `shouldReturn` refusedWith 404 "not_found"
  answerCall node fakeNamePolicy (selector "generalTermsLink()") $ Returns $ textAnswer otherLink
  nextBlock node
  answerOf <$> terms (hex $ C.keccak256 $ encodeUtf8 otherLink) `shouldReturn` refusedWith 404 "not_found"
  setGeneralTerms node fakeNewGeneralTermsText
  answerText node (fakeTermsPath fakeNewGeneralTermsText) $ TextAnswer status429 [("Retry-After", "60")] ""
  r <- terms (hex fakeNewGeneralTerms)
  responseFixture "terms-unavailable" r
  (answerOf r, lookup "Retry-After" (responseHeaders r)) `shouldBe` (withField "wait" (J.Number 60) (refusedWith 503 "terms_unavailable"), Just "60")
  answerText node (fakeTermsPath fakeNewGeneralTermsText) $ TextAnswer status404 [] ""
  answerOf <$> terms (hex fakeNewCommunityTerms) `shouldReturn` termsAnswer fakeNewCommunityTermsText
  answerCall node fakeNamePolicy (selector "generalTermsLink()") $ Reverts ""
  nextBlock node
  answerOf <$> terms (hex fakeNewCommunityTerms) `shouldReturn` refusedWith 503 "provider_unavailable"
  forM_ [T.toUpper (hex fakeGeneralTerms), T.drop 2 (hex fakeGeneralTerms), "0x12"] $ \h ->
    answerOf <$> terms h `shouldReturn` refusedWith 400 "bad_request"

testCommunityWithoutText :: TVar ChaChaDRG -> IO ()
testCommunityWithoutText g = withSearch g id id $ \s@Search {node} -> do
  listCommunityName node alice
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  answer <- searchAlice s 12
  (snd answer >>= JT.parseMaybe (J..: "community")) `shouldBe` Just (communityEntry Nothing)

testCommunityYears :: TVar ChaChaDRG -> IO ()
testCommunityYears g = withSearch g threeYears id $ \s@Search {node} -> do
  listCommunityName node alice
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  answer <- searchAlice s 12
  (snd answer >>= JT.parseMaybe (\o -> (,,) <$> (o J..: "found" >>= (J..: "years")) <*> (o J..: "found" >>= (J..: "price")) <*> (o J..: "community" >>= (J..: "years")))) `shouldBe` Just (2 :: Int, 10000 :: Int, 2 :: Int)

testRecordLinks :: IO ()
testRecordLinks = do
  let botLink = taggedLink "smp8.simplex.im" 'a' "Bot"
      bots = map (\n -> taggedLink "smp8.simplex.im" 'a' ("Bot" <> T.pack (show n))) [1 .. 7 :: Int]
  recordLinks LKContact (bakeryLink <> ";" <> botLink <> " ; " <> newsLink <> ";" <> fullLink <> ";;") `shouldBe` parsedAs LKContact [bakeryLink, botLink]
  recordLinks LKChannel (" " <> newsLink <> ";" <> bakeryLink <> ";hello") `shouldBe` parsedAs LKChannel [newsLink]
  recordLinks LKContact fullLink `shouldBe` []
  -- a value of another kind first, so the cap counts only the record's own links
  recordLinks LKContact (T.intercalate ";" $ newsLink : bots) `shouldBe` parsedAs LKContact (take 5 bots)
  where
    parsedAs kind = map ((kind,) . parsed)

testRecordEdits :: TVar ChaChaDRG -> IO ()
testRecordEdits g = forM_ [(4, 1, 10), (2, 0, 5)] $ \(perYear, left, returnsFrom) -> withSearch g (\cfg -> cfg {nRecordEditsPerYear = perYear}) id $ \s@Search {node, store} -> do
  registeredWith node fullLink ""
  let daysAgo :: Int -> UTCTime
      daysAgo d = addUTCTime (fromIntegral $ negate d * 86400) t0
      inserted n d = withTransaction store $ \db -> insertRecordEdit db (atNonce (fromIntegral n) $ newEdit ownerA n setChannel) (daysAgo d)
  Just _ <- inserted 1 366
  Just minedId <- inserted 2 10
  Just revertedId <- inserted 3 5
  Just _ <- inserted 4 1
  withTransaction store $ \db -> do
    settleRecordEdit db minedId (EOApplied Nothing) `shouldReturn` True
    settleRecordEdit db revertedId (EOReverted Nothing) `shouldReturn` True
  answer <- searchFixture ("search-registered-edits-" <> show left) s 12
  (snd answer >>= JT.parseMaybe (\o -> o J..: "found" >>= (J..: "records") >>= (J..: "edits")))
    `shouldBe` Just (editsLeft left $ Just $ addUTCTime (365 * 86400) $ daysAgo returnsFrom)

testFailedReads :: TVar ChaChaDRG -> IO ()
testFailedReads g = withSearch g id id $ \s@Search {node} -> do
  let unavailable = refusedWith 503 "provider_unavailable"
  available node
  searchFixture "search-provider-unavailable" s 12 `shouldReturn` unavailable
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 9999
  searchAlice s 12 `shouldReturn` unavailable
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  searchAlice s 12 `shouldReturn` availableAnswer 10000
  setFault node "eth_getBlockByNumber" HttpFailure
  searchAlice s 12 `shouldReturn` unavailable
  clearFault node "eth_getBlockByNumber"
  answerCall node fakeNamePolicy reservedNamesCall $ Reverts ""
  searchAlice s 12 `shouldReturn` unavailable
  registeredWith node bakeryLink newsLink
  answerCall node (resolverOf node) channelTextCall $ Reverts ""
  searchAlice s 12 `shouldReturn` unavailable
  answerCall node (resolverOf node) channelTextCall $ Returns $ textAnswer ""
  answerCall node (resolverOf node) noncesCall $ Reverts ""
  searchAlice s 12 `shouldReturn` unavailable
  answerCall node (resolverOf node) noncesCall $ Returns $ uint256 (0 :: Int)
  fst <$> searchAlice s 12 `shouldReturn` 200
  answerCall node fakeNamePolicy (selector "generalTermsLink()") $ Reverts ""
  nextBlock node
  fst <$> searchAlice s 12 `shouldReturn` 200

testOneDeadline :: TVar ChaChaDRG -> IO ()
testOneDeadline g = withSearch g id (\ct -> ct {ctDeadline = deadline}) $ \s@Search {node} -> do
  registeredWith node bakeryLink newsLink
  -- reads the registrar's constants, which a running service has read once
  fst <$> searchAlice s 12 `shouldReturn` 200
  forM_ ["eth_call", "eth_getBlockByNumber"] $ \m -> setFault node m $ Slow $ deadline * 2 `div` 7
  (answer, elapsed) <- timed $ searchAlice s 12
  answer `shouldBe` refusedWith 503 "provider_unavailable"
  elapsed `shouldSatisfy` (< 3)
  answerRegistered node otherResolver
  searchFixture "search-registered-other" s 12 `shouldReturn` registeredAnswer [] otherRecords otherResolverText
  where
    deadline = 2000000

testSlowTerms :: TVar ChaChaDRG -> IO ()
testSlowTerms g = withSearch g id (\ct -> ct {ctDeadline = deadline}) $ \s@Search {node} -> do
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  setFault node "GET" $ Slow $ deadline * 3 `div` 2
  (answer, elapsed) <- timed $ searchAlice s 12
  answer `shouldBe` refusedWith 503 "provider_unavailable"
  elapsed `shouldSatisfy` (< 2)
  clearFault node "GET"
  threadDelay deadline
  searchAlice s 12 `shouldReturn` availableAnswer 10000
  fetchedTexts node `shouldReturn` [fakeTermsPath fakeGeneralTermsText]
  where
    deadline = 1000000

testBadHash :: TVar ChaChaDRG -> IO ()
testBadHash g = withSearch g id id $ \s -> do
  let hexDigits = T.drop 2 aliceHex
  forM_ [T.toUpper aliceHex, "0x" <> T.toUpper hexDigits, hexDigits, "0x" <> T.drop 1 hexDigits, aliceHex <> "0", "0x" <> T.replicate 64 "g", "0X" <> hexDigits] $ \h ->
    (h, searchName s h "?length=12") `shouldReturnPair` refusedWith 400 "bad_request"
  fixtureOf "search-bad-request" (send s ("/api/name/0x" <> T.unpack (T.toUpper hexDigits) <> "?length=5") id) `shouldReturn` refusedWith 400 "bad_request"
  where
    shouldReturnPair (h, act) expected = act >>= \a -> (h, a) `shouldBe` (h, expected)

testSignRead :: TVar ChaChaDRG -> IO ()
testSignRead g = withSearch g id id $ \s@Search {node} -> do
  answerCall node fakeSponsor noncesCall $ Returns $ uint256 (4 :: Int)
  answerCall node fakeNamePolicy noncesCall $ Returns $ uint256 (2 :: Int)
  let signPath hash owner = "/api/name/" <> T.unpack hash <> "/sign?owner=" <> T.unpack owner
      asked community terms = ok [("type", "read"), ("intentNonce", J.Number 4), ("acceptance", J.object ["community" .= community, "policy" .= safeDecodeUtf8 (strEncode fakeNamePolicy), "termsHash" .= hex terms, "nonce" .= (2 :: Int)])]
  fixtureOf "sign-read" (send s (signPath aliceHex ownerAText) id) `shouldReturn` asked False fakeGeneralTerms
  listCommunityName node alice
  answerOf <$> send s (signPath aliceHex ownerAText) id `shouldReturn` asked True fakeCommunityTerms
  answerOf <$> send s (signPath aliceHex (T.toLower ownerAText)) id `shouldReturn` refusedWith 400 "bad_request"
  answerOf <$> send s (signPath (T.toUpper aliceHex) ownerAText) id `shouldReturn` refusedWith 400 "bad_request"
  answerOf <$> send s ("/api/name/" <> T.unpack aliceHex <> "/sign") id `shouldReturn` refusedWith 400 "bad_request"
  setFault node "eth_call" HttpFailure
  fixtureOf "sign-unavailable" (send s (signPath aliceHex ownerAText) id) `shouldReturn` refusedWith 503 "name_check_unavailable"

testBadLength :: TVar ChaChaDRG -> IO ()
testBadLength g = withSearch g id id $ \s@Search {node} -> do
  reserved node
  searchFixture "search-invalid-name" s 5 `shouldReturn` refusedWith 400 "invalid_name"
  forM_ ["?length=64", "", "?length=", "?length", "?length=abc", "?length=6.0", "?length=%2B7", "?length=-6", "?length=%207", "?length=0x6", "?length=18446744073709551622"] $ \q ->
    searchName s aliceHex q >>= \a -> (q, a) `shouldBe` (q, refusedWith 400 "invalid_name")
  searchFixture "search-reserved" s 6 `shouldReturn` reservedAnswer
  forM_ ["?length=63", "?length=007"] $ \q ->
    searchName s aliceHex q >>= \a -> (q, a) `shouldBe` (q, reservedAnswer)

-- link preview

testPreviewKinds :: TVar ChaChaDRG -> IO ()
testPreviewKinds g = withSearch g id id $ \s -> do
  let previewFixture name l = fixtureOf name $ postPreview s (J.encode $ J.object ["link" .= l])
  previewFixture "preview-contact" bakeryLink `shouldReturn` ok bakeryChat
  previewFixture "preview-channel" newsLink `shouldReturn` ok newsChat
  previewOf s "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA?h=smp9.simplex.im" `shouldReturn` ok newsChat
  previewFixture "preview-bot" (taggedLink "smp8.simplex.im" 'a' "Bot") `shouldReturn` ok (contactChat "helper" "" Nothing "bot")
  previewFixture "preview-business" (taggedLink "smp8.simplex.im" 'a' "Biz") `shouldReturn` ok (contactChat "helper" "" Nothing "business")
  previewFixture "preview-unavailable" (taggedLink "smp8.simplex.im" 'a' "Gone") `shouldReturn` ok ["kind" .= str "contact", "profile" .= J.object ["type" .= str "unavailable"]]

testPreviewInvalid :: TVar ChaChaDRG -> IO ()
testPreviewInvalid g = withSearch g id id $ \s -> do
  let base = bakeryLink <> "?x="
      padded n = base <> T.replicate (n - T.length base) "a"
      invalid = (400, Just $ KM.fromList [("type", "invalid_link"), ("kind", J.Null)])
  fixtureOf "preview-invalid-link" (postPreview s $ J.encode $ J.object ["link" .= fullLink]) `shouldReturn` invalid
  forM_ ["hello", "", bakeryLink <> ";" <> bakeryLink, " " <> bakeryLink, taggedLink "smp8.simplex.im" 'g' "Y8m3", padded (linkMaxBytes + 1)] $ \l ->
    previewOf s l >>= \a -> (l, a) `shouldBe` (l, invalid)
  answerOf <$> postPreview s "{}" `shouldReturn` invalid
  answerOf <$> postPreview s "{\"link\": 5}" `shouldReturn` invalid
  fetchedLinks s `shouldReturn` []
  fst <$> previewOf s (padded linkMaxBytes) `shouldReturn` 200

testPreviewBadBody :: TVar ChaChaDRG -> IO ()
testPreviewBadBody g = withSearch g id id $ \s -> do
  forM_ ["[]", "\"link\"", "not json", "", J.encode (J.object ["link" .= T.replicate 9000 "a"])] $ \body ->
    answerOf <$> postPreview s body >>= \a -> (body, a) `shouldBe` (body, refusedWith 400 "bad_request")
