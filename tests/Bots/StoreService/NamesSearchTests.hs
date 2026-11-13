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
import Bots.StoreService.NamesOrdersTests (communityEntry, termsEntry, testNamesConfig, threeYears)
import Bots.StoreService.NamesProfilesTests (bakeryLink, jpeg, newsLink)
import Bots.StoreService.NamesStoreTests (atNonce, issue, newEdit, order, ownerA, placeCode, setChannel, t0, usable)
import Bots.StoreService.WebTests (withServiceStore, withApp)
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
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Client (Manager, Request, RequestBody (..), httpLbs, method, parseRequest, requestBody, responseBody, responseHeaders, responseStatus)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (status404, status429)
import Network.HTTP.Types.Status (statusCode)
import Simplex.Messaging.Agent.Store.Common (DBStore, withTransaction)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Transport.Client (TransportHost (..))
import StoreService.Config (ListenerConfig (..), NamesConfig (..))
import StoreService.Names.Abi (AbiValue (..), abiBytes, abiTuple, bytes32FromHex, hex, selector, uint256)
import StoreService.Names.Contracts (Contracts (..))
import StoreService.Names.Phases (RegistrationId (..))
import StoreService.Names.Links (LinkKind (..), linkMaxBytes)
import StoreService.Names.Profiles (ChatKind (..), HostResolver, LinkProfile (..), ProfileFetcher, ProfileLimits (..), defaultProfileLimits, monotonicSeconds, newProfiles)
import StoreService.Names.Store (EditState (..), NamePhase (..), NameRegistration (..), VersionWrite (..), getRegistration, insertRecordEdit, settleRecordEdit, updateRegistration)
import StoreService.Web.Http (HttpEnv (..), maxSearches, newHttpEnv, notFound, previewLimit, searchLimit)
import StoreService.Web.Names (Route (..), namesApp)
import StoreService.Web.Names.Search (previewHandler, recordLinks, searchHandler, termsHandler)
import Test.Hspec

namesSearchTests :: Spec
namesSearchTests = describe "StoreService names: search" $ do
  g <- runIO C.newRandom
  describe "search" $ do
    it "answers an available name with the chain's price for the paid years, the term start and the order's contracts" $ testAvailable g
    it "answers a name the chain has available as registered while its register here is sent, without a price" $ testRegistering g
    it "answers a registered name with its entities, raw records, owner, resolver, nonce, expiry and grace period" $ testRegistered g
    it "holds a search slot through the chain reads only, answering rate_limited while every slot is taken" $ testSearchSlot g
    it "answers a listed name's community entry when available, reserved, being registered here and registered, a plain answer for an ini line the policy does not list, the community terms on chain as they change, and 503 while its listing is unread" $ testCommunity g
    it "answers an available name's terms null while the terms of its kind cannot be read: not set, of a link the service does not take, or of a text not fetched now" $ testTermsUnavailable g
    it "answers a name the policy lists without an ini line as a community name without its text" $ testCommunityWithoutText g
    it "answers a listed name's price for its community's 2 years, whatever years the configuration gives" $ testCommunityYears g
    it "takes the first 5 short links of a record's kind, splitting on ';', and none from other values" testRecordLinks
    it "answers a registered name's record edits left and when the next one returns, counting the edits of 365 days, reverted too" $ testRecordEdits g
    it "answers provider_unavailable to any failed chain read, records and price included, and to a price under the floor, and reads no terms link for a registered name" $ testFailedReads g
    it "fails a search whose reads together pass one deadline, though each read meets it" $ testOneDeadline g
    it "fails a search whose terms text arrives past the deadline, and answers the text that fetch kept to the next search" $ testSlowTerms g
    it "answers bad_request to a hash not in 0x and 64 lowercase hex, before the length" $ testBadHash g
    it "answers invalid_name to a length not a decimal integer in 6-63" $ testBadLength g
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
  http@HttpEnv {heSearches} <- newHttpEnv ListenerConfig {lHost = "127.0.0.1", lBadgesPort = 0, lBadgesStaticDir = "", lServeWebapp = False, lBadgesExportDir = Nothing, lTrustForwardedFor = False, lNamesStaticDir = Nothing, lNamesExportDir = Nothing, lNamesPort = Nothing, lMetricsPort = Nothing}
  let fetch sl = atomically (readTVar heSearches >>= modifyTVar' searchesAtFetch . (:)) >> fakeFetch fetched sl
  ps <- newProfiles defaultProfileLimits {fetchTimeout = fetchMicros} monotonicSeconds fetch stubResolver
  let cfg = adjustConfig $ testNamesConfig node
      routes = \case
        ["api", "link", "preview"] -> Just Route {rMethod = "POST", rLimit = previewLimit, rHandler = previewHandler ps}
        ["api", "terms", hash] -> Just Route {rMethod = "GET", rLimit = searchLimit, rHandler = termsHandler (adjust ct) hash}
        ["api", "name", hash] -> Just Route {rMethod = "GET", rLimit = searchLimit, rHandler = searchHandler cfg http store (adjust ct) ps (pure t0) hash}
        _ -> Nothing
      app req respond = fromMaybe (respond notFound) (namesApp http routes req respond)
  withApp app $ \client -> action Search {node, store, client, fetched, searches = heSearches, searchesAtFetch}

aliceCommunity :: Text
aliceCommunity = "bakers and bakeries in Lisbon"

listAlice :: NamesConfig -> NamesConfig
listAlice cfg = cfg {nCommunity = M.singleton (fromJust $ bytes32FromHex aliceHex) aliceCommunity}

-- | The answer as it reads for a name the policy lists, with its text from the ini and, when available, the policy's community terms.
listed :: Answer -> Answer
listed = listedUnder fakeCommunityTermsText

-- | As 'listed', under the community terms of this text.
listedUnder :: ByteString -> Answer -> Answer
listedUnder text = withTerms (termsEntry text) . withField "community" (communityEntry aliceCommunity)

withTerms :: J.Value -> Answer -> Answer
withTerms terms answer@(_, Just (J.Object o))
  | KM.member "terms" o = withField "terms" terms answer
withTerms _ answer = answer

withField :: J.Key -> J.Value -> Answer -> Answer
withField k v (status, answer) = (status, inserted <$> answer)
  where
    inserted = \case
      J.Object o -> J.Object $ KM.insert k v o
      other -> other

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
      "terms" .= termsEntry fakeGeneralTermsText
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
      "recordEdits" .= if records == J.Null then J.Null else editsLeft 3 J.Null,
      "expiresAt" .= (now + year),
      "gracePeriod" .= gracePeriod,
      "chainId" .= fakeChainId,
      "tld" .= str "simplex",
      "community" .= J.Null
    ]

editsLeft :: Int -> J.Value -> J.Value
editsLeft left nextAt = J.object ["left" .= left, "nextAt" .= nextAt]

reservedAnswer, registeringAnswer :: Answer
reservedAnswer = ok ["status" .= str "reserved", "community" .= J.Null]
registeringAnswer = ok ["status" .= str "registered", "entities" .= ([] :: [J.Value]), "tld" .= str "simplex", "community" .= J.Null]

recordsOf :: Text -> Text -> J.Value
recordsOf contact channel = J.object ["contact" .= contact, "channel" .= channel]

-- | A preview's fields, and with the link and its deep link a search answer's entity.
chat :: Text -> Text -> Text -> Text -> Maybe Text -> Maybe Int -> Text -> [(J.Key, J.Value)]
chat t kind displayName fullName image subscribers profile =
  ["type" .= t, "kind" .= kind, "displayName" .= displayName, "fullName" .= fullName, "image" .= image, "subscribers" .= subscribers, "profile" .= profile]

bakeryChat, newsChat :: [(J.Key, J.Value)]
bakeryChat = chat "address" "contact" "Alice's Bakery" "Sourdough and pastries, Lisbon" (Just jpeg) Nothing "ok"
newsChat = chat "channel" "channel" "Bakery news" "Daily bakes and opening hours" Nothing (Just 1204) "ok"

entity :: [(J.Key, J.Value)] -> Text -> Text -> J.Value
entity fields l deepLink = J.object $ fields <> ["link" .= l, "deepLink" .= deepLink]

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

testSearchSlot :: TVar ChaChaDRG -> IO ()
testSearchSlot g = withSearch g id id $ \s@Search {node, searches, searchesAtFetch} -> do
  registeredWith node bakeryLink newsLink
  fst <$> searchAlice s 12 `shouldReturn` 200
  readTVarIO searchesAtFetch `shouldReturn` [0, 0]
  atomically $ writeTVar searches maxSearches
  searchAlice s 12 `shouldReturn` refused 429 "rate_limited"

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
  -- new terms on chain: the available answer carries them; a listing that cannot be read: no answer
  available node
  setCommunityTerms node fakeNewCommunityTermsText
  setPhase store PRegistered
  searchAlice s 12 `shouldReturn` listedUnder fakeNewCommunityTermsText (availableAnswer 10000)
  answerListing node alice $ Reverts ""
  searchAlice s 12 `shouldReturn` refused 503 "provider_unavailable"

testTermsUnavailable :: TVar ChaChaDRG -> IO ()
testTermsUnavailable g = withSearch g listAlice id $ \s@Search {node} -> do
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  setGeneralTerms node ""
  searchAlice s 12 `shouldReturn` withTerms J.Null (availableAnswer 10000)
  listCommunityName node alice
  searchAlice s 12 `shouldReturn` listed (availableAnswer 10000)
  setCommunityTerms node ""
  searchAlice s 12 `shouldReturn` withTerms J.Null (listed $ availableAnswer 10000)
  answerListing node alice $ Returns $ uint256 (0 :: Int)
  answerCall node fakeNamePolicy (selector "generalTermsLink()") $ Returns $ textAnswer "https://example.org/terms.md"
  nextBlock node
  searchAlice s 12 `shouldReturn` withTerms J.Null (availableAnswer 10000)
  setGeneralTerms node fakeNewGeneralTermsText
  answerText node (fakeTermsPath fakeNewGeneralTermsText) $ TextAnswer status429 [("Retry-After", "2")] ""
  searchAlice s 12 `shouldReturn` withTerms J.Null (availableAnswer 10000)

testTerms :: TVar ChaChaDRG -> IO ()
testTerms g = withSearch g id id $ \s@Search {node} -> do
  let terms h = send s ("/api/terms/" <> T.unpack h) id
      termsAnswer text = ok ["link" .= fakeTermsLink text, "text" .= decodeUtf8 text]
      otherLink = "https://example.org/terms.md"
  answerOf <$> terms (hex fakeGeneralTerms) `shouldReturn` termsAnswer fakeGeneralTermsText
  answerOf <$> terms (hex fakeCommunityTerms) `shouldReturn` termsAnswer fakeCommunityTermsText
  answerOf <$> terms (hex fakeNewCommunityTerms) `shouldReturn` refused 404 "not_found"
  setCommunityTerms node fakeNewCommunityTermsText
  answerOf <$> terms (hex fakeNewCommunityTerms) `shouldReturn` termsAnswer fakeNewCommunityTermsText
  answerOf <$> terms (hex fakeCommunityTerms) `shouldReturn` refused 404 "not_found"
  setGeneralTerms node ""
  answerOf <$> terms (hex $ C.keccak256 "") `shouldReturn` refused 404 "not_found"
  answerOf <$> terms (hex fakeGeneralTerms) `shouldReturn` refused 404 "not_found"
  answerCall node fakeNamePolicy (selector "generalTermsLink()") $ Returns $ textAnswer otherLink
  nextBlock node
  answerOf <$> terms (hex $ C.keccak256 $ encodeUtf8 otherLink) `shouldReturn` refused 404 "not_found"
  setGeneralTerms node fakeNewGeneralTermsText
  answerText node (fakeTermsPath fakeNewGeneralTermsText) $ TextAnswer status429 [("Retry-After", "60")] ""
  r <- terms (hex fakeNewGeneralTerms)
  (answerOf r, lookup "Retry-After" (responseHeaders r)) `shouldBe` (refused 503 "terms_unavailable", Just "60")
  answerText node (fakeTermsPath fakeNewGeneralTermsText) $ TextAnswer status404 [] ""
  answerOf <$> terms (hex fakeNewCommunityTerms) `shouldReturn` termsAnswer fakeNewCommunityTermsText
  answerCall node fakeNamePolicy (selector "generalTermsLink()") $ Reverts ""
  nextBlock node
  answerOf <$> terms (hex fakeNewCommunityTerms) `shouldReturn` refused 503 "provider_unavailable"
  forM_ [T.toUpper (hex fakeGeneralTerms), T.drop 2 (hex fakeGeneralTerms), "0x12"] $ \h ->
    answerOf <$> terms h `shouldReturn` refused 400 "bad_request"

testCommunityWithoutText :: TVar ChaChaDRG -> IO ()
testCommunityWithoutText g = withSearch g id id $ \s@Search {node} -> do
  listCommunityName node alice
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  answer <- searchAlice s 12
  (snd answer >>= JT.parseMaybe (J.withObject "answer" (J..: "community"))) `shouldBe` Just (communityEntry J.Null)

testCommunityYears :: TVar ChaChaDRG -> IO ()
testCommunityYears g = withSearch g threeYears id $ \s@Search {node} -> do
  listCommunityName node alice
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  answer <- searchAlice s 12
  (snd answer >>= JT.parseMaybe (J.withObject "answer" $ \o -> (,,) <$> o J..: "years" <*> o J..: "price" <*> (o J..: "community" >>= (J..: "years")))) `shouldBe` Just (3 :: Int, 10000 :: Int, 2 :: Int)

testRecordLinks :: IO ()
testRecordLinks = do
  let botLink = taggedLink "smp8.simplex.im" 'a' "Bot"
      bots = map (\n -> taggedLink "smp8.simplex.im" 'a' ("Bot" <> T.pack (show n))) [1 .. 7 :: Int]
  recordLinks LKContact (bakeryLink <> ";" <> botLink <> " ; " <> newsLink <> ";" <> fullLink <> ";;") `shouldBe` [(LKContact, bakeryLink), (LKContact, botLink)]
  recordLinks LKChannel (" " <> newsLink <> ";" <> bakeryLink <> ";hello") `shouldBe` [(LKChannel, newsLink)]
  recordLinks LKContact fullLink `shouldBe` []
  -- a value of another kind first, so the cap counts only the record's own links
  recordLinks LKContact (T.intercalate ";" $ newsLink : bots) `shouldBe` map (LKContact,) (take 5 bots)

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
    settleRecordEdit db minedId (ESMined Nothing) `shouldReturn` True
    settleRecordEdit db revertedId (ESReverted Nothing) `shouldReturn` True
  answer <- searchAlice s 12
  (snd answer >>= JT.parseMaybe (J.withObject "answer" (J..: "recordEdits")))
    `shouldBe` Just (editsLeft left $ J.toJSON (floor (utcTimeToPOSIXSeconds $ daysAgo returnsFrom) + 365 * 86400 :: Int))

testFailedReads :: TVar ChaChaDRG -> IO ()
testFailedReads g = withSearch g id id $ \s@Search {node} -> do
  let unavailable = refused 503 "provider_unavailable"
  available node
  searchAlice s 12 `shouldReturn` unavailable
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

testSlowTerms :: TVar ChaChaDRG -> IO ()
testSlowTerms g = withSearch g id (\ct -> ct {ctDeadline = deadline}) $ \s@Search {node} -> do
  available node
  answerCall node fakeController (rentPriceCall 12) $ Returns $ rentPriceAnswer 10000
  setFault node "GET" $ Slow $ deadline * 3 `div` 2
  (answer, elapsed) <- timed $ searchAlice s 12
  answer `shouldBe` refused 503 "provider_unavailable"
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
  forM_ ["hello", "", fullLink, bakeryLink <> ";" <> bakeryLink, " " <> bakeryLink, taggedLink "smp8.simplex.im" 'g' "Y8m3", padded (linkMaxBytes + 1)] $ \l ->
    previewOf s l >>= \a -> (l, a) `shouldBe` (l, invalid)
  answerOf <$> postPreview s "{}" `shouldReturn` invalid
  answerOf <$> postPreview s "{\"link\": 5}" `shouldReturn` invalid
  fetchedLinks s `shouldReturn` []
  fst <$> previewOf s (padded linkMaxBytes) `shouldReturn` 200

testPreviewBadBody :: TVar ChaChaDRG -> IO ()
testPreviewBadBody g = withSearch g id id $ \s -> do
  forM_ ["[]", "\"link\"", "not json", "", J.encode (J.object ["link" .= T.replicate 9000 "a"])] $ \body ->
    answerOf <$> postPreview s body >>= \a -> (body, a) `shouldBe` (body, refused 400 "bad_request")
