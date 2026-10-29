{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesSearchTests (namesSearchTests, aliceHex) where

import Bots.StoreService.ConfigTests (termsUrl, termsVersion)
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
import Bots.StoreService.NamesOrdersTests (communityEntry, communityEntryOf)
import Bots.StoreService.NamesProfilesTests (bakeryLink, jpeg, newsLink)
import Bots.StoreService.NamesStoreTests (issue, order, ownerA, placeCode, t0, usable)
import Bots.StoreService.WebTests (withServiceStore)
import Control.Concurrent.STM
import Control.Monad (forM_, replicateM_)
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
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), diffUTCTime, getCurrentTime)
import Network.HTTP.Client (Manager, Request, RequestBody (..), defaultManagerSettings, httpLbs, method, newManager, parseRequest, requestBody, responseBody, responseHeaders, responseStatus)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.SimplexName (SimplexTLD (..))
import Simplex.Messaging.Transport.Client (TransportHost (..))
import StoreService.Config (ListenerConfig (..), NamesConfig (..), TermsConfig (..))
import StoreService.Names.Abi (AbiValue (..), abiBytes, abiTuple, bytes32FromHex, hex, selector, uint256)
import StoreService.Names.Contracts (Contracts (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Profiles (ChatKind (..), HostResolver, LinkProfile (..), ProfileFetcher, ProfileLimits (..), defaultProfileLimits, monotonicSeconds, newProfiles)
import StoreService.Names.Store (NamePhase (..), NameRegistration (..), VersionWrite (..), getRegistration, updateRegistration)
import StoreService.Web.Http (newHttpEnv, notFound, previewLimit, searchLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Search (communityTermsHandler, previewHandler, searchHandler)
import Test.Hspec

namesSearchTests :: Spec
namesSearchTests = describe "StoreService names: search" $ do
  g <- runIO C.newRandom
  describe "search" $ do
    it "answers an available name with the chain's price for the paid years, the term start and the order's contracts" $ testAvailable g
    it "answers a reserved name" $ testReserved g
    it "answers a name the chain has available as registered while its register here is sent, without a price" $ testRegistering g
    it "answers a registered name with its entities, raw records, owner, resolver, nonce, expiry and grace period" $ testRegistered g
    it "answers a listed name's community entry when available, reserved, being registered here and registered, a plain answer for an ini line the policy does not list, the terms on chain as they change, and 503 while its listing is unread" $ testCommunity g
    it "answers a name the policy lists without an ini line as a community name without its text" $ testCommunityWithoutText g
    it "answers a listed name's price for its community's 2 years, whatever years the configuration gives" $ testCommunityYears g
    it "makes one entity per short link of its record's kind, splitting on ';', and none from other values" $ testRecordLinks g
    it "makes entities of the first 5 short links of a record only, fetching no other" $ testRecordLinksCap g
    it "reads no records from another resolver" $ testOtherResolver g
    it "answers a link whose profile was not read unread, and a private host's unavailable without fetching it" $ testEntityProfiles g
    it "answers provider_unavailable to any failed chain read, records and price included" $ testFailedReads g
    it "fails a search whose reads together pass one deadline, though each read meets it" $ testOneDeadline g
    it "answers bad_request to a hash not in 0x and 64 lowercase hex, before the length" $ testBadHash g
    it "answers invalid_name to a length not a decimal integer in 6-63" $ testBadLength g
    it "takes a search token for every request, whatever its outcome, and answers another method 405" $ testSearchLimit g
  describe "link preview" $ do
    it "answers each kind of chat as an entity without its links" $ testPreviewKinds g
    it "answers invalid_link to anything but one short link within 4096 bytes" $ testPreviewInvalid g
    it "answers bad_request to a body that is not a JSON object or is over its cap" $ testPreviewBadBody g
    it "accepts a link to a private host with its profile unavailable, never fetching it" $ testPreviewPrivateHost g
    it "answers a link whose profile was not read unread" $ testPreviewUnread g
    it "takes a preview token for every request, whatever its outcome, and answers another method 405" $ testPreviewLimit g
  describe "community terms" $ do
    it "answers the policy's terms text by its hash, not_found to another hash or before the terms are set, provider_unavailable when unread and bad_request to a malformed hash" $ testCommunityTermsText g

-- harness

data Search = Search {node :: FakeEthNode, store :: DBStore, client :: (Manager, String), fetched :: TVar [Text]}

-- | The routes as namesRoutes registers them, since a NamesEnv needs a running chat controller.
withSearch :: TVar ChaChaDRG -> (NamesConfig -> NamesConfig) -> (Contracts -> Contracts) -> (Search -> IO a) -> IO a
withSearch g adjustConfig adjust action = withServiceStore $ \store -> withContracts g $ \node ct -> do
  fetched <- newTVarIO []
  ps <- newProfiles defaultProfileLimits {fetchTimeout = fetchMicros} monotonicSeconds (fakeFetch fetched) stubResolver
  http <- newHttpEnv ListenerConfig {lHost = "127.0.0.1", lBadgesPort = 0, lBadgesStaticDir = "", lServeWebapp = False, lBadgesExportDir = Nothing, lTrustForwardedFor = False, lNamesStaticDir = Nothing, lNamesExportDir = Nothing, lNamesPort = Nothing}
  let cfg = adjustConfig $ namesConfig node
      routes = \case
        ["api", "link", "preview"] -> Just Route {rMethod = "POST", rLimit = previewLimit, rHandler = previewHandler ps}
        ["api", "community-terms", hash] -> Just Route {rMethod = "GET", rLimit = searchLimit, rHandler = communityTermsHandler (adjust ct) hash}
        ["api", "name", hash] -> Just Route {rMethod = "GET", rLimit = searchLimit, rHandler = searchHandler cfg store (adjust ct) ps hash}
        _ -> Nothing
      app req respond = fromMaybe (respond notFound) (namesApp http routes req respond)
  mgr <- newManager defaultManagerSettings
  Warp.testWithApplication (pure app) $ \port -> action Search {node, store, client = (mgr, "http://127.0.0.1:" <> show port), fetched}

namesConfig :: FakeEthNode -> NamesConfig
namesConfig node =
  NamesConfig
    { nTLD = TLDSimplex,
      nYears = 2,
      nTermStart = UTCTime (fromGregorian 2027 1 1) 0,
      nTermSlackSeconds = 3600,
      nRevealMarginSeconds = 900,
      nMaxOrderMoves = 3,
      nTerms = TermsConfig {tVersion = termsVersion, tUrl = termsUrl},
      nCommunity = M.empty,
      nChain = fakeChainConfig node
    }

aliceCommunity :: Text
aliceCommunity = "bakers and bakeries in Lisbon"

listAlice :: NamesConfig -> NamesConfig
listAlice cfg = cfg {nCommunity = M.singleton (fromJust $ bytes32FromHex aliceHex) aliceCommunity}

-- | The answer as it reads for a name the policy lists, with its text from the ini and the policy's terms.
listed :: Answer -> Answer
listed = listedUnder fakeCommunityTerms

-- | As 'listed', under these community terms.
listedUnder :: ByteString -> Answer -> Answer
listedUnder h (status, answer) = (status, withEntry <$> answer)
  where
    withEntry :: J.Value -> J.Value
    withEntry = \case
      J.Object o -> J.Object $ KM.insert "community" (communityEntryOf h aliceCommunity) o
      v -> v

fetchMicros :: Int
fetchMicros = 500000

-- | Answers by the tag at the start of the link's key; a link tagged Gone is not read.
fakeFetch :: TVar [Text] -> ProfileFetcher
fakeFetch fetched sl = do
  let l = decodeUtf8 $ strEncode sl
  atomically $ modifyTVar' fetched (l :)
  pure $ case T.drop 1 $ snd $ T.breakOn "#" l of
    key
      | "Y8m3" `T.isPrefixOf` key -> Just bakery
      | "Rk27" `T.isPrefixOf` key -> Just news
      | "Bot" `T.isPrefixOf` key -> Just helper {kind = CKBot}
      | "Biz" `T.isPrefixOf` key -> Just helper {kind = CKBusiness}
      | otherwise -> Nothing
  where
    bakery = LinkProfile {kind = CKContact, displayName = "Alice's Bakery", fullName = "Sourdough and pastries, Lisbon", image = Just jpeg, subscribers = Nothing}
    news = LinkProfile {kind = CKChannel, displayName = "Bakery news", fullName = "Daily bakes and opening hours", image = Nothing, subscribers = Just 1204}
    helper = LinkProfile {kind = CKContact, displayName = "helper", fullName = "", image = Nothing, subscribers = Nothing}

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

type Answer = (Int, Maybe J.Value)

answerOf :: HTTP.Response LB.ByteString -> Answer
answerOf r = (statusCode (responseStatus r), J.decode (responseBody r))

send :: Search -> String -> (Request -> Request) -> IO (HTTP.Response LB.ByteString)
send Search {client = (mgr, base)} path modify = do
  req <- parseRequest (base <> path)
  httpLbs (modify req) mgr

searchName :: Search -> Text -> String -> IO Answer
searchName s hash query = answerOf <$> send s ("/api/name/" <> T.unpack hash <> query) id

searchAlice :: Search -> Int -> IO Answer
searchAlice s len = searchName s aliceHex ("?length=" <> show len)

postPreview :: Search -> LB.ByteString -> IO (HTTP.Response LB.ByteString)
postPreview s body = send s "/api/link/preview" $ \req -> req {method = "POST", requestBody = RequestBodyLBS body}

previewOf :: Search -> Text -> IO Answer
previewOf s l = answerOf <$> postPreview s (J.encode $ J.object ["link" .= l])

ok :: [(J.Key, J.Value)] -> Answer
ok = (200,) . Just . J.object

refused :: Int -> Text -> Answer
refused status tag = (status, Just $ J.object ["error" .= tag])

-- chain

-- viem: keccak256 of "alicebakery"
aliceHex :: Text
aliceHex = "0xbcc46b7948ff6be55bc3ff4db5bc4be16638f29e818a89244dae11caaed7c9c6"

-- viem: getAddress of the fake node's owner, default resolver, sponsor and another resolver
ownerText, resolverText, sponsorText, otherResolverText :: Text
ownerText = "0x014f9c09CcEeDEF1B713110d04D3a07d5B5D9b73"
resolverText = "0x2279B7A0a67DB372996a5FaB50D91eAA73d2eBe6"
sponsorText = "0x9A676e781A523b5d0C0e43731313A708CB607508"
otherResolverText = "0x4826533B4897376654Bb4d4AD88B7faFD0C98528"

termStart :: Int
termStart = 1798761600

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

availableAnswer :: Int -> Answer
availableAnswer price =
  ok
    [ "status" .= str "available",
      "price" .= price,
      "currency" .= str "usd",
      "years" .= (2 :: Int),
      "termStart" .= termStart,
      "resolver" .= resolverText,
      "sponsor" .= sponsorText,
      "chainId" .= fakeChainId,
      "tld" .= str "simplex",
      "community" .= J.Null,
      "terms" .= J.object ["version" .= termsVersion, "url" .= termsUrl]
    ]

registeredAnswer :: [J.Value] -> J.Value -> Text -> J.Value -> Answer
registeredAnswer entities records resolver nonce =
  ok
    [ "status" .= str "registered",
      "entities" .= entities,
      "records" .= records,
      "owner" .= ownerText,
      "resolver" .= resolver,
      "recordsNonce" .= nonce,
      "expiresAt" .= (now + year),
      "gracePeriod" .= gracePeriod,
      "chainId" .= fakeChainId,
      "tld" .= str "simplex",
      "community" .= J.Null
    ]

reservedAnswer, registeringAnswer :: Answer
reservedAnswer = ok ["status" .= str "reserved", "community" .= J.Null]
registeringAnswer = ok ["status" .= str "registered", "entities" .= ([] :: [J.Value]), "tld" .= str "simplex", "community" .= J.Null]

recordsOf :: Text -> Text -> J.Value
recordsOf contact channel = J.object ["contact" .= contact, "channel" .= channel]

-- | A preview's fields, and with the link and its deep link a search answer's entity.
chat :: Text -> Text -> Text -> Text -> Maybe Text -> Maybe Int -> Text -> [(J.Key, J.Value)]
chat t kind displayName fullName image subscribers profile =
  ["type" .= t, "kind" .= kind, "displayName" .= displayName, "fullName" .= fullName, "image" .= image, "subscribers" .= subscribers, "profile" .= profile]

unreadChat :: Text -> Text -> Text -> [(J.Key, J.Value)]
unreadChat t kind = chat t kind "" "" Nothing Nothing

bakeryChat, newsChat :: [(J.Key, J.Value)]
bakeryChat = chat "address" "contact" "Alice's Bakery" "Sourdough and pastries, Lisbon" (Just jpeg) Nothing "ok"
newsChat = chat "channel" "channel" "Bakery news" "Daily bakes and opening hours" Nothing (Just 1204) "ok"

entity :: [(J.Key, J.Value)] -> Text -> Text -> J.Value
entity fields l deepLink = J.object $ fields <> ["link" .= l, "deepLink" .= deepLink]

-- | Each entity's type, kind, profile and link.
entitySummaries :: Answer -> [(Text, Text, Text, Text)]
entitySummaries = \case
  (200, Just v) -> fromMaybe [] $ JT.parseMaybe summaries v
  _ -> []
  where
    summaries = J.withObject "answer" $ \o -> o J..: "entities" >>= mapM (J.withObject "entity" $ \e -> (,,,) <$> e J..: "type" <*> e J..: "kind" <*> e J..: "profile" <*> e J..: "link")

fullLink :: Text
fullLink = "https://simplex.chat/contact#/?v=2&smp=smp%3A%2F%2Fabc"

timed :: IO a -> IO (a, NominalDiffTime)
timed action = do
  start <- getCurrentTime
  a <- action
  (a,) . (`diffUTCTime` start) <$> getCurrentTime

setPhase :: DBStore -> NamePhase -> IO ()
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
  searchAlice s 12 `shouldReturn` availableAnswer 10000
  searchAlice s 6 `shouldReturn` availableAnswer 40000

testReserved :: TVar ChaChaDRG -> IO ()
testReserved g = withSearch g id id $ \s@Search {node} -> do
  reserved node
  searchAlice s 12 `shouldReturn` reservedAnswer

testRegistering :: TVar ChaChaDRG -> IO ()
testRegistering g = withSearch g id id $ \s@Search {node, store} -> do
  available node
  code <- issue store 1 usable
  placeCode store "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
  setPhase store PRegistering
  -- no price is answered, so reading one would fail the search
  searchAlice s 12 `shouldReturn` registeringAnswer
  setPhase store PRegistered
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  searchAlice s 12 `shouldReturn` availableAnswer 10000

testRegistered :: TVar ChaChaDRG -> IO ()
testRegistered g = withSearch g id id $ \s@Search {node} -> do
  registeredWith node bakeryLink newsLink
  searchAlice s 12
    `shouldReturn` registeredAnswer
      [ entity bakeryChat bakeryLink "simplex:/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rSAAAAAAAAA?h=smp8.simplex.im",
        entity newsChat newsLink "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA?h=smp9.simplex.im"
      ]
      (recordsOf bakeryLink newsLink)
      resolverText
      (J.toJSON (0 :: Int))

testCommunity :: TVar ChaChaDRG -> IO ()
testCommunity g = withSearch g listAlice id $ \s@Search {node, store} -> do
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  -- an ini line for a name the policy does not list: a plain name
  searchAlice s 12 `shouldReturn` availableAnswer 10000
  listCommunityName node alice
  searchAlice s 12 `shouldReturn` listed (availableAnswer 10000)
  reserved node
  searchAlice s 12 `shouldReturn` listed reservedAnswer
  available node
  code <- issue store 1 usable
  placeCode store "r1" ownerA (order "alicebakery" 1) code `shouldReturn` Right ()
  setPhase store PRegistering
  searchAlice s 12 `shouldReturn` listed registeringAnswer
  registeredWith node fullLink ""
  let registered = registeredAnswer [] (recordsOf fullLink "") resolverText (J.toJSON (0 :: Int))
  searchAlice s 12 `shouldReturn` listed registered
  -- new terms on chain: every answer carries them; a listing that cannot be read: no answer
  setCommunityTerms node fakeNewCommunityTermsText
  searchAlice s 12 `shouldReturn` listedUnder fakeNewCommunityTerms registered
  reserved node
  searchAlice s 12 `shouldReturn` listedUnder fakeNewCommunityTerms reservedAnswer
  available node
  searchAlice s 12 `shouldReturn` listedUnder fakeNewCommunityTerms registeringAnswer
  setPhase store PRegistered
  searchAlice s 12 `shouldReturn` listedUnder fakeNewCommunityTerms (availableAnswer 10000)
  answerListing node alice $ Reverts ""
  searchAlice s 12 `shouldReturn` refused 503 "provider_unavailable"

testCommunityTermsText :: TVar ChaChaDRG -> IO ()
testCommunityTermsText g = withSearch g id id $ \s@Search {node} -> do
  let terms h = send s ("/api/community-terms/" <> T.unpack h) id
      text r = (statusCode (responseStatus r), lookup hContentType (responseHeaders r), responseBody r)
  text <$> terms (hex fakeCommunityTerms) `shouldReturn` (200, Just "text/plain; charset=utf-8", LB.fromStrict fakeCommunityTermsText)
  answerOf <$> terms (hex fakeNewCommunityTerms) `shouldReturn` refused 404 "not_found"
  setCommunityTerms node fakeNewCommunityTermsText
  text <$> terms (hex fakeNewCommunityTerms) `shouldReturn` (200, Just "text/plain; charset=utf-8", LB.fromStrict fakeNewCommunityTermsText)
  answerOf <$> terms (hex fakeCommunityTerms) `shouldReturn` refused 404 "not_found"
  setCommunityTerms node ""
  answerOf <$> terms (hex $ C.keccak256 "") `shouldReturn` refused 404 "not_found"
  answerCall node fakeNamePolicy (selector "termsText()") $ Reverts ""
  answerOf <$> terms (hex fakeCommunityTerms) `shouldReturn` refused 503 "provider_unavailable"
  forM_ [T.toUpper (hex fakeCommunityTerms), T.drop 2 (hex fakeCommunityTerms), "0x12"] $ \h ->
    answerOf <$> terms h `shouldReturn` refused 400 "bad_request"

testCommunityWithoutText :: TVar ChaChaDRG -> IO ()
testCommunityWithoutText g = withSearch g id id $ \s@Search {node} -> do
  listCommunityName node alice
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  answer <- searchAlice s 12
  (snd answer >>= JT.parseMaybe (J.withObject "answer" (J..: "community"))) `shouldBe` Just (communityEntry J.Null)

testCommunityYears :: TVar ChaChaDRG -> IO ()
testCommunityYears g = withSearch g (\c -> c {nYears = 3}) id $ \s@Search {node} -> do
  listCommunityName node alice
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  answer <- searchAlice s 12
  (snd answer >>= JT.parseMaybe (J.withObject "answer" $ \o -> (,,) <$> o J..: "years" <*> o J..: "price" <*> (o J..: "community" >>= (J..: "years")))) `shouldBe` Just (3 :: Int, 10000 :: Int, 2 :: Int)

testRecordLinks :: TVar ChaChaDRG -> IO ()
testRecordLinks g = withSearch g id id $ \s@Search {node} -> do
  let botLink = taggedLink "smp8.simplex.im" 'a' "Bot"
      contact = bakeryLink <> ";" <> botLink <> " ; " <> newsLink <> ";" <> fullLink <> ";;"
      channel = " " <> newsLink <> ";" <> bakeryLink <> ";hello"
  registeredWith node contact channel
  answer <- searchAlice s 12
  entitySummaries answer
    `shouldBe` [("address", "contact", "ok", bakeryLink), ("address", "bot", "ok", botLink), ("channel", "channel", "ok", newsLink)]
  (snd answer >>= JT.parseMaybe (J.withObject "answer" (J..: "records"))) `shouldBe` Just (recordsOf contact channel)
  registeredWith node fullLink ""
  searchAlice s 12 `shouldReturn` registeredAnswer [] (recordsOf fullLink "") resolverText (J.toJSON (0 :: Int))

testRecordLinksCap :: TVar ChaChaDRG -> IO ()
testRecordLinksCap g = withSearch g id id $ \s@Search {node} -> do
  let contacts = map (\n -> taggedLink "smp8.simplex.im" 'a' ("Bot" <> T.pack (show n))) [1 .. 7 :: Int]
  -- a value of another kind first, so the cap counts only the record's own links
  registeredWith node (T.intercalate ";" $ newsLink : contacts) newsLink
  answer <- searchAlice s 12
  entitySummaries answer
    `shouldBe` map ("address", "bot", "ok",) (take 5 contacts) <> [("channel", "channel", "ok", newsLink)]
  length . filter (T.isInfixOf "#Bot") <$> fetchedLinks s `shouldReturn` 5

testOtherResolver :: TVar ChaChaDRG -> IO ()
testOtherResolver g = withSearch g id id $ \s@Search {node} -> do
  nameState node (toInteger $ now + year) 0
  answerRegistered node otherResolver
  searchAlice s 12 `shouldReturn` registeredAnswer [] J.Null otherResolverText J.Null

testEntityProfiles :: TVar ChaChaDRG -> IO ()
testEntityProfiles g = withSearch g id id $ \s@Search {node} -> do
  let gone = taggedLink "smp8.simplex.im" 'a' "Gone"
      privateName = taggedLink "private.test" 'a' "Y8m3"
      privateLiteral = taggedLink "10.0.0.1" 'c' "Rk27"
  registeredWith node (gone <> ";" <> privateName) privateLiteral
  searchAlice s 12
    `shouldReturn` registeredAnswer
      [ entity (unreadChat "address" "contact" "unread") gone (deepLinkAt "smp8.simplex.im" 'a' "Gone"),
        entity (unreadChat "address" "contact" "unavailable") privateName (deepLinkAt "private.test" 'a' "Y8m3"),
        entity (unreadChat "channel" "channel" "unavailable") privateLiteral (deepLinkAt "10.0.0.1" 'c' "Rk27")
      ]
      (recordsOf (gone <> ";" <> privateName) privateLiteral)
      resolverText
      (J.toJSON (0 :: Int))
  map (T.isInfixOf "#Gone") <$> fetchedLinks s `shouldReturn` [True]
  where
    deepLinkAt host kind tag = "simplex:/" <> T.singleton kind <> "#" <> snd (T.breakOnEnd "#" $ taggedLink host kind tag) <> "?h=" <> host

testFailedReads :: TVar ChaChaDRG -> IO ()
testFailedReads g = withSearch g id id $ \s@Search {node} -> do
  let unavailable = refused 503 "provider_unavailable"
  available node
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

-- | Each slow read takes 40% of the deadline: the status's two concurrent rounds meet it, the records' third round passes it.
testOneDeadline :: TVar ChaChaDRG -> IO ()
testOneDeadline g = withSearch g id (\ct -> ct {ctDeadline = deadline}) $ \s@Search {node} -> do
  registeredWith node bakeryLink newsLink
  -- reads the registrar's constants, which a running service has read once
  fst <$> searchAlice s 12 `shouldReturn` 200
  forM_ ["eth_call", "eth_getBlockByNumber"] $ \m -> setFault node m $ Slow $ deadline * 2 `div` 5
  (answer, elapsed) <- timed $ searchAlice s 12
  answer `shouldBe` refused 503 "provider_unavailable"
  elapsed `shouldSatisfy` (< 3)
  answerRegistered node otherResolver
  searchAlice s 12 `shouldReturn` registeredAnswer [] J.Null otherResolverText J.Null
  where
    deadline = 2000000

testBadHash :: TVar ChaChaDRG -> IO ()
testBadHash g = withSearch g id id $ \s -> do
  let hexDigits = T.drop 2 aliceHex
  forM_ [T.toUpper aliceHex, "0x" <> T.toUpper hexDigits, hexDigits, "0x" <> T.drop 1 hexDigits, aliceHex <> "0", "0x" <> T.replicate 64 "g", "0X" <> hexDigits] $ \h ->
    (h, searchName s h "?length=12") `shouldReturnPair` refused 400 "bad_request"
  searchName s ("0x" <> T.toUpper hexDigits) "?length=5" `shouldReturn` refused 400 "bad_request"
  where
    shouldReturnPair (h, act) expected = act >>= \a -> (h, a) `shouldBe` (h, expected)

testBadLength :: TVar ChaChaDRG -> IO ()
testBadLength g = withSearch g id id $ \s@Search {node} -> do
  reserved node
  forM_ ["?length=5", "?length=64", "", "?length=", "?length", "?length=abc", "?length=6.0", "?length=%2B7", "?length=-6", "?length=%207", "?length=0x6", "?length=18446744073709551622"] $ \q ->
    searchName s aliceHex q >>= \a -> (q, a) `shouldBe` (q, refused 400 "invalid_name")
  forM_ ["?length=6", "?length=63", "?length=007"] $ \q ->
    searchName s aliceHex q >>= \a -> (q, a) `shouldBe` (q, reservedAnswer)

testSearchLimit :: TVar ChaChaDRG -> IO ()
testSearchLimit g = withSearch g id id $ \s -> do
  replicateM_ 30 $ searchName s "0x00" "?length=12" `shouldReturn` refused 400 "bad_request"
  r <- send s ("/api/name/" <> T.unpack aliceHex <> "?length=12") id
  answerOf r `shouldBe` refused 429 "rate_limited"
  lookup "Retry-After" (responseHeaders r) `shouldSatisfy` maybe False (> "0")
  r' <- send s ("/api/name/" <> T.unpack aliceHex <> "?length=12") $ \req -> req {method = "POST"}
  answerOf r' `shouldBe` refused 405 "method_not_allowed"
  lookup "Allow" (responseHeaders r') `shouldBe` Just "GET"

-- link preview

testPreviewKinds :: TVar ChaChaDRG -> IO ()
testPreviewKinds g = withSearch g id id $ \s -> do
  previewOf s bakeryLink `shouldReturn` ok bakeryChat
  previewOf s newsLink `shouldReturn` ok newsChat
  previewOf s "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA?h=smp9.simplex.im" `shouldReturn` ok newsChat
  previewOf s (taggedLink "smp8.simplex.im" 'a' "Bot") `shouldReturn` ok (chat "address" "bot" "helper" "" Nothing Nothing "ok")
  previewOf s (taggedLink "smp8.simplex.im" 'a' "Biz") `shouldReturn` ok (chat "address" "business" "helper" "" Nothing Nothing "ok")

testPreviewInvalid :: TVar ChaChaDRG -> IO ()
testPreviewInvalid g = withSearch g id id $ \s -> do
  let base = bakeryLink <> "?x="
      padded n = base <> T.replicate (n - T.length base) "a"
      invalid = refused 400 "invalid_link"
  forM_ ["hello", "", fullLink, bakeryLink <> ";" <> bakeryLink, " " <> bakeryLink, taggedLink "smp8.simplex.im" 'g' "Y8m3", padded 4097] $ \l ->
    previewOf s l >>= \a -> (l, a) `shouldBe` (l, invalid)
  answerOf <$> postPreview s "{}" `shouldReturn` invalid
  answerOf <$> postPreview s "{\"link\": 5}" `shouldReturn` invalid
  fetchedLinks s `shouldReturn` []
  fst <$> previewOf s (padded 4096) `shouldReturn` 200

testPreviewBadBody :: TVar ChaChaDRG -> IO ()
testPreviewBadBody g = withSearch g id id $ \s -> do
  forM_ ["[]", "\"link\"", "not json", "", J.encode (J.object ["link" .= T.replicate 9000 "a"])] $ \body ->
    answerOf <$> postPreview s body >>= \a -> (body, a) `shouldBe` (body, refused 400 "bad_request")

testPreviewPrivateHost :: TVar ChaChaDRG -> IO ()
testPreviewPrivateHost g = withSearch g id id $ \s -> do
  previewOf s (taggedLink "10.0.0.1" 'a' "Y8m3") `shouldReturn` ok (unreadChat "address" "contact" "unavailable")
  previewOf s (taggedLink "private.test" 'c' "Rk27") `shouldReturn` ok (unreadChat "channel" "channel" "unavailable")
  fetchedLinks s `shouldReturn` []

testPreviewUnread :: TVar ChaChaDRG -> IO ()
testPreviewUnread g = withSearch g id id $ \s -> do
  previewOf s (taggedLink "smp8.simplex.im" 'c' "Gone") `shouldReturn` ok (unreadChat "channel" "channel" "unread")
  length <$> fetchedLinks s `shouldReturn` 1

testPreviewLimit :: TVar ChaChaDRG -> IO ()
testPreviewLimit g = withSearch g id id $ \s -> do
  replicateM_ 20 $ previewOf s "hello" `shouldReturn` refused 400 "invalid_link"
  r <- postPreview s (J.encode $ J.object ["link" .= bakeryLink])
  answerOf r `shouldBe` refused 429 "rate_limited"
  lookup "Retry-After" (responseHeaders r) `shouldSatisfy` maybe False (> "0")
  r' <- send s "/api/link/preview" id
  answerOf r' `shouldBe` refused 405 "method_not_allowed"
  lookup "Allow" (responseHeaders r') `shouldBe` Just "POST"
