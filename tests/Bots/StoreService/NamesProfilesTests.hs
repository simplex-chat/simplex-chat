{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesProfilesTests (namesProfilesTests, namesProfilesChatTests, bakeryLink, newsLink, jpeg, contactData, channelData, linkDataProfile, parsed) where

import ChatClient (TestCC (..), testChat2)
import ChatTests.DBUtils (TestParams)
import ChatTests.Groups (prepareChannel1Relay)
import ChatTests.Utils (aliceProfile, bobProfile, getContactLinks, withRelay, (##>))
import qualified ChatTests.Utils as CU
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, concurrently, replicateConcurrently, wait)
import Control.Concurrent.STM
import Control.Exception (uninterruptibleMask_)
import Control.Monad (forM_, replicateM_)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.Int (Int64)
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Word (Word32, Word64)
import Simplex.Chat.Controller (ChatController (..), ChatError (..), ChatErrorType (..))
import Simplex.Chat.Protocol (ContactShortLinkData (..), GroupShortLinkData (..), PublicGroupData (..))
import Simplex.Chat.Types (AgentConnId (..), B64UrlByteString (..), ChatPeerType (..), GroupProfile (..), GroupType (..), ImageData (..), Profile (..), PublicGroupProfile (..))
import Simplex.Messaging.Agent.Protocol (AgentErrorType (BROKER, SMP), ConnShortLink (..), ConnectionMode (..), simplexShortLink)
import Simplex.Messaging.Encoding.String (strDecode, strEncode)
import Simplex.Messaging.Protocol (BrokerErrorType (TIMEOUT), ErrorType (AUTH), SMPServer)
import Simplex.Messaging.Transport.Client (TransportHost (..))
import StoreService.Names.Profiles hiding (preview)
import StoreService.Names.Registration (LinkKind (..))
import Test.Hspec

namesProfilesTests :: Spec
namesProfilesTests = describe "StoreService names: profiles" $ do
  describe "link data" $
    it "answers the link data read, unavailable for data that does not decode or a link its server refuses, and unread for a failure that may pass" testReadOutcome
  describe "JSON" $ do
    it "encodes a contact entity with its profile, peer type and business flag" testEntityContactJSON
    it "encodes a channel entity with its group type, subscribers and a simplex:/ deep link" testEntityChannelJSON
    it "encodes previews without a profile as their kind and read state alone" testPreviewEmptyJSON
    it "drops an image that is not a JPEG or PNG data URI, as the chat core encodes one" testImageJSON
    it "gives a deep link that parses back in the core with the link's server" testDeepLinkParses
  describe "bounds and cache" $ do
    it "answers a link again from the cache, in either spelling, and fetches it again once the cache expires" testCacheExpiry
    it "does not cache a failed fetch" testFailureNotCached
    it "evicts the entry that expires soonest when the cache is full" testCacheBound
    it "gives up a fetch that takes longer than the timeout, freeing its slot" testFetchTimeout
    it "fills the preview bound while search still fetches, and a preview with no slot in time is unread" testPreviewBound
    it "fills the search bound while previews still fetch" testSearchBound
    it "lets a fetch wait for a slot that frees within the timeout" testSlotFreesInTime
  describe "host rule" $ do
    it "passes public addresses and names resolving to them, and refuses private, loopback, CGNAT, link-local, unique-local, unspecified, multicast, reserved, broadcast, benchmarking and site-local ones, also IPv4-mapped, IPv4-compatible and NAT64" testHostsChecked
    it "allows .onion hosts without resolving them, and names that do not resolve here" testOnionUnresolvable
    it "refuses a link with one public and one private host, and a name with one private address" testMixedHosts
    it "answers a refused link unavailable without fetching it" testRefusedNotFetched
  describe "host lookups" $ do
    it "bounds the lookups in flight across searches and previews, and a lookup with no slot in time is unavailable" testLookupBound
    it "leaves search its lookups while previews hold all of theirs" testPreviewLookupShare
    it "remembers a refused host until its time passes, also when its lookup answers after the caller gave up" testRefusedHostCache
    it "shares a host's running lookup among overlapping searches, holding one slot for it, and answers unread at once past its timeout" testSharedHostLookup

namesProfilesChatTests :: SpecWith TestParams
namesProfilesChatTests =
  describe "StoreService names: profiles" $
    CU.it "reads the link data of an address and a channel through the chat core, for another user and for their owner" testChatProfiles

-- Link data

contactData :: Bool -> Profile -> LinkData
contactData business profile = LDContact ContactShortLinkData {profile, message = Nothing, business, localBadge = Nothing, chatLayout = Nothing}

channelData :: Maybe PublicGroupProfile -> Maybe Int64 -> LinkData
channelData publicGroup count =
  LDChannel
    GroupShortLinkData
      { groupProfile = GroupProfile {displayName = "Bakery news", fullName = "Daily bakes and opening hours", shortDescr = Nothing, description = Nothing, image = Nothing, publicGroup, groupPreferences = Nothing, memberAdmission = Nothing, chatLayout = Nothing},
        publicGroupData = PublicGroupData <$> count
      }

alice :: Profile
alice = bakeryWith (Just (ImageData jpeg))

bakeryWith :: Maybe ImageData -> Profile
bakeryWith = linkDataProfile "Alice's Bakery" "Sourdough and pastries, Lisbon"

linkDataProfile :: Text -> Text -> Maybe ImageData -> Profile
linkDataProfile displayName fullName image =
  Profile {displayName, fullName, shortDescr = Nothing, description = Nothing, image, contactLink = Nothing, preferences = Nothing, peerType = Nothing, badge = Nothing, contactDomain = Nothing}

jpeg :: Text
jpeg = "data:image/jpg;base64,/9j/4AAQSkZJRg=="

testReadOutcome :: IO ()
testReadOutcome =
  forM_
    [ ("decoded", Right $ Just bakery, ProfileOk $ linkProfileOf bakery),
      ("not decoded", Right Nothing, ProfileUnavailable),
      ("refused by its server", Left $ agentFailure $ SMP "smp8.simplex.im" AUTH, ProfileUnavailable),
      ("its server timed out", Left $ agentFailure $ BROKER "smp8.simplex.im" TIMEOUT, ProfileUnread),
      ("this service failed", Left $ ChatError $ CEException "failed", ProfileUnread)
    ]
    $ \(what, fetched, answer) -> do
      ps <- quietProfiles (const $ pure fetched) publicResolver
      (\(Preview _ r) -> (what :: String, r)) <$> linkPreview ps LKContact (parsed bakeryLink) `shouldReturn` (what, answer)
  where
    bakery = contactData False alice

agentFailure :: AgentErrorType -> ChatError
agentFailure e = ChatErrorAgent e (AgentConnId "") Nothing

-- JSON

bakeryLink, newsLink :: Text
bakeryLink = "https://smp8.simplex.im/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rSAAAAAAAAA"
newsLink = "https://smp9.simplex.im/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA"

shouldEncodeAs :: J.ToJSON a => a -> LB.ByteString -> Expectation
shouldEncodeAs v expected = Just (J.toJSON v) `shouldBe` J.decode expected

parsed :: Text -> ConnShortLink 'CMContact
parsed = either error id . strDecode . encodeUtf8

linksOf :: Entity -> Maybe (Text, Text)
linksOf = JT.parseMaybe (J.withObject "entity" $ \o -> (,) <$> o J..: "link" <*> o J..: "deepLink") . J.toJSON

testEntityContactJSON :: IO ()
testEntityContactJSON = do
  ps <- quietProfiles (const $ pure $ Right $ Just $ contactData False alice) publicResolver
  e <- linkEntity ps LKContact (parsed bakeryLink)
  e
    `shouldEncodeAs` "{\"preview\": {\"kind\": \"contact\", \"profile\": {\"type\": \"ok\", \"linkProfile\": {\"kind\": \"contact\",\
                     \ \"displayName\": \"Alice's Bakery\", \"fullName\": \"Sourdough and pastries, Lisbon\", \"image\": \"data:image/jpg;base64,/9j/4AAQSkZJRg==\"}}},\
                     \ \"link\": \"https://smp8.simplex.im/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rSAAAAAAAAA\", \"deepLink\": \"simplex:/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rSAAAAAAAAA?h=smp8.simplex.im\"}"
  Preview LKContact (ProfileOk $ linkProfileOf $ contactData False alice {peerType = Just CPTBot})
    `shouldEncodeAs` "{\"kind\": \"contact\", \"profile\": {\"type\": \"ok\", \"linkProfile\": {\"kind\": \"bot\",\
                     \ \"displayName\": \"Alice's Bakery\", \"fullName\": \"Sourdough and pastries, Lisbon\", \"image\": \"data:image/jpg;base64,/9j/4AAQSkZJRg==\"}}}"
  (\(Preview _ r) -> r) (Preview LKContact (ProfileOk $ linkProfileOf $ contactData True alice {peerType = Just CPTBot}))
    `shouldBe` ProfileOk LinkProfile {kind = CKBusiness, displayName = "Alice's Bakery", fullName = "Sourdough and pastries, Lisbon", image = Just jpeg, subscribers = Nothing}

testEntityChannelJSON :: IO ()
testEntityChannelJSON = do
  let news = channelData (Just channel) (Just 1204)
  ps <- quietProfiles (const $ pure $ Right $ Just news) publicResolver
  e <- linkEntity ps LKChannel (parsed newsLink)
  Just (J.toJSON e)
    `shouldBe` Just
      ( J.object
          [ "preview" J..= okPreview "channel" (linkProfileOf news),
            "link" J..= ("https://smp9.simplex.im/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA" :: Text),
            "deepLink" J..= ("simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA?h=smp9.simplex.im" :: Text)
          ]
      )
  linkProfileOf news `shouldBe` LinkProfile {kind = CKChannel, displayName = "Bakery news", fullName = "Daily bakes and opening hours", image = Nothing, subscribers = Just 1204}
  Preview LKChannel (ProfileOk $ linkProfileOf $ channelData Nothing Nothing)
    `shouldEncodeAs` "{\"kind\": \"channel\", \"profile\": {\"type\": \"ok\", \"linkProfile\": {\"kind\": \"channel\",\
                     \ \"displayName\": \"Bakery news\", \"fullName\": \"Daily bakes and opening hours\"}}}"
  let simplexLink = "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA?h=smp9.simplex.im"
  linksOf <$> linkEntity ps LKChannel (parsed simplexLink) `shouldReturn` Just (simplexLink, simplexLink)
  where
    channel = PublicGroupProfile {groupType = GTChannel, groupLink = parsed newsLink, publicGroupId = B64UrlByteString "news", publicGroupAccess = Nothing}

okPreview :: Text -> LinkProfile -> J.Value
okPreview kind p = J.object ["kind" J..= kind, "profile" J..= J.object ["type" J..= ("ok" :: Text), "linkProfile" J..= p]]

testDeepLinkParses :: IO ()
testDeepLinkParses = do
  ps <- quietProfiles answerAll publicResolver
  mapM_ (deepLinkKeepsServer ps) [bakeryLink, keyedLink, twoHostLink]
  where
    keyedLink = "https://smp.example.com/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rSAAAAAAAAA?p=5224&c=LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI"
    twoHostLink = "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA?h=a.example.com%2Cb.example.com"
    deepLinkKeepsServer ps l = do
      deepLink <- fmap snd . linksOf <$> linkEntity ps LKContact (parsed l)
      let original = parseLink l
          reparsed = maybe (Left "no deep link") parseLink deepLink
      (fmap ("simplex:/" `T.isPrefixOf`) deepLink, serverOf <$> reparsed) `shouldBe` (Just True, serverOf <$> original)
      reparsed `shouldBe` (simplexShortLink <$> original)
    parseLink :: Text -> Either String (ConnShortLink 'CMContact)
    parseLink = strDecode . encodeUtf8
    serverOf :: ConnShortLink 'CMContact -> SMPServer
    serverOf (CSLContact _ _ srv _) = srv

testPreviewEmptyJSON :: IO ()
testPreviewEmptyJSON = do
  Preview LKChannel ProfileUnread `shouldEncodeAs` "{\"kind\": \"channel\", \"profile\": {\"type\": \"unread\"}}"
  Preview LKContact ProfileUnavailable `shouldEncodeAs` "{\"kind\": \"contact\", \"profile\": {\"type\": \"unavailable\"}}"

testImageJSON :: IO ()
testImageJSON = do
  let imageOf :: Profile -> IO (Maybe (Maybe Text))
      imageOf p = do
        ps <- quietProfiles (const $ pure $ Right $ Just $ contactData False p) publicResolver
        JT.parseMaybe imageField . J.toJSON <$> linkPreview ps LKContact (parsed bakeryLink)
      imageField = J.withObject "preview" $ \o -> o J..: "profile" >>= (J..: "linkProfile") >>= (J..:? "image")
  imageOf (bakeryWith (Just (ImageData "javascript:alert(1)"))) `shouldReturn` Just Nothing
  imageOf (bakeryWith (Just (ImageData "https://example.com/a.png"))) `shouldReturn` Just Nothing
  imageOf alice `shouldReturn` Just (Just jpeg)

-- Bounds and cache

-- | Long enough that a fetch which should have been answered from a slot or the cache would be seen waiting on it.
testTimeout :: Int
testTimeout = 500000

testLimits :: ProfileLimits
testLimits = defaultProfileLimits {fetchTimeout = testTimeout}

data Fake = Fake {profiles :: Profiles, clockAt :: TVar Word64, fetched :: TVar [Text]}

-- | A fetcher that records each link it is asked for, then answers with @answer@.
newFake :: ProfileLimits -> (Text -> IO (Either ChatError (Maybe LinkData))) -> HostResolver -> IO Fake
newFake limits answer resolver = do
  clockAt <- newTVarIO 0
  fetched <- newTVarIO []
  let fetcher _ sl = do
        let l = linkText sl
        atomically $ modifyTVar' fetched (l :)
        answer l
  profiles <- newProfiles limits (readTVarIO clockAt) fetcher resolver
  pure Fake {profiles, clockAt, fetched}

quietProfiles :: (Text -> IO (Either ChatError (Maybe LinkData))) -> HostResolver -> IO Profiles
quietProfiles answer resolver = (\Fake {profiles} -> profiles) <$> newFake testLimits answer resolver

linkText :: ConnShortLink 'CMContact -> Text
linkText = decodeUtf8 . strEncode

-- | A contact address link with a distinct canonical key, so it round-trips through the chat core's parser.
addressLink :: Int -> Text
addressLink = addressLinkAt "smp8.simplex.im"

addressLinkAt :: Text -> Int -> Text
addressLinkAt host n = "https://" <> host <> "/a#" <> decodeUtf8 (B64URL.encodeUnpadded (B.pack (take 32 (show n <> repeat '.'))))

profileFor :: Text -> LinkData
profileFor l = contactData False $ linkDataProfile (T.takeEnd 8 l) "" Nothing

answerAll :: Text -> IO (Either ChatError (Maybe LinkData))
answerAll = pure . Right . Just . profileFor

fetchCount :: Fake -> IO Int
fetchCount Fake {fetched} = length <$> readTVarIO fetched

preview :: Fake -> Text -> IO ProfileRead
preview Fake {profiles} l = (\(Preview _ r) -> r) <$> linkPreview profiles LKContact (parsed l)

search :: Fake -> Text -> IO ProfileRead
search Fake {profiles} l = (\(Entity (Preview _ r) _ _) -> r) <$> linkEntity profiles LKContact (parsed l)

testCacheExpiry :: IO ()
testCacheExpiry = do
  fake@Fake {clockAt} <- newFake testLimits answerAll publicResolver
  let l = addressLink 1
  preview fake l `shouldReturn` ProfileOk (linkProfileOf $ profileFor l)
  search fake l `shouldReturn` ProfileOk (linkProfileOf $ profileFor l)
  fetchCount fake `shouldReturn` 1
  preview fake (linkText $ simplexShortLink $ parsed l) `shouldReturn` ProfileOk (linkProfileOf $ profileFor l)
  fetchCount fake `shouldReturn` 1
  atomically $ writeTVar clockAt 599
  preview fake l `shouldReturn` ProfileOk (linkProfileOf $ profileFor l)
  fetchCount fake `shouldReturn` 1
  atomically $ writeTVar clockAt 600
  preview fake l `shouldReturn` ProfileOk (linkProfileOf $ profileFor l)
  fetchCount fake `shouldReturn` 2

testFailureNotCached :: IO ()
testFailureNotCached = do
  failing <- newTVarIO True
  fake <- newFake testLimits (\l -> readTVarIO failing >>= \f -> pure $ if f then Left (agentFailure $ BROKER "smp8.simplex.im" TIMEOUT) else Right (Just $ profileFor l)) publicResolver
  let l = addressLink 1
  preview fake l `shouldReturn` ProfileUnread
  atomically $ writeTVar failing False
  preview fake l `shouldReturn` ProfileOk (linkProfileOf $ profileFor l)
  fetchCount fake `shouldReturn` 2

testCacheBound :: IO ()
testCacheBound = do
  fake@Fake {clockAt} <- newFake testLimits {maxCachedProfiles = 2} answerAll publicResolver
  let a = addressLink 1
      b = addressLink 2
      c = addressLink 3
  mapM_ (\(t, l) -> atomically (writeTVar clockAt t) >> preview fake l) [(0, a), (1, b), (2, c)]
  fetchCount fake `shouldReturn` 3
  preview fake b `shouldReturn` ProfileOk (linkProfileOf $ profileFor b)
  preview fake c `shouldReturn` ProfileOk (linkProfileOf $ profileFor c)
  fetchCount fake `shouldReturn` 3
  preview fake a `shouldReturn` ProfileOk (linkProfileOf $ profileFor a)
  fetchCount fake `shouldReturn` 4

testFetchTimeout :: IO ()
testFetchTimeout = do
  gate <- newEmptyTMVarIO
  fake <- newFake testLimits (\l -> if l == addressLink 0 then atomically (readTMVar gate) >> answerAll l else answerAll l) publicResolver
  start <- getCurrentTime
  replicateConcurrently 2 (preview fake (addressLink 0)) `shouldReturn` [ProfileUnread, ProfileUnread]
  elapsed <- (`diffUTCTime` start) <$> getCurrentTime
  elapsed `shouldSatisfy` (< 2 * micros testTimeout)
  -- Both preview slots were taken by the timed-out fetches; they must be free again.
  replicateConcurrently 2 (preview fake (addressLink 1)) `shouldReturn` replicate 2 (ProfileOk (linkProfileOf $ profileFor (addressLink 1)))
  where
    micros n = fromIntegral n / 1000000

-- | A fetcher for which links numbered below 100 report that they started, then hold their slot until the gate opens, past their timeout.
gatedFake :: IO (Fake, TQueue (), TMVar ())
gatedFake = do
  started <- newTQueueIO
  gate <- newEmptyTMVarIO
  let answer l
        | l `elem` map addressLink [0 .. 99] = atomically (writeTQueue started ()) >> uninterruptibleMask_ (atomically $ readTMVar gate) >> answerAll l
        | otherwise = answerAll l
  fake <- newFake testLimits answer publicResolver
  pure (fake, started, gate)

testPreviewBound :: IO ()
testPreviewBound = do
  (raced, fetchedThen) <- boundFilled preview 2
  raced `shouldBe` (ProfileOk (linkProfileOf $ profileFor (addressLink 100)), ProfileUnread)
  fetchedThen `shouldNotContain` [addressLink 101]

testSearchBound :: IO ()
testSearchBound = do
  (raced, fetchedThen) <- boundFilled search 4
  raced `shouldBe` (ProfileUnread, ProfileOk (linkProfileOf $ profileFor (addressLink 101)))
  fetchedThen `shouldNotContain` [addressLink 100]

-- | Holds a full pool of one kind of read, then races a search and a preview against it, and lets the held reads go.
boundFilled :: (Fake -> Text -> IO ProfileRead) -> Int -> IO ((ProfileRead, ProfileRead), [Text])
boundFilled hold n = do
  (fake, started, gate) <- gatedFake
  held <- mapM (async . hold fake . addressLink) [0 .. n - 1]
  replicateM_ n (atomically $ readTQueue started)
  raced <- concurrently (search fake (addressLink 100)) (preview fake (addressLink 101))
  fetchedThen <- readTVarIO (fetched fake)
  atomically $ putTMVar gate ()
  mapM_ wait held
  pure (raced, fetchedThen)

testSlotFreesInTime :: IO ()
testSlotFreesInTime = do
  (fake, started, gate) <- gatedFake
  held <- mapM (async . preview fake . addressLink) [0, 1]
  replicateM_ 2 (atomically $ readTQueue started)
  waiting <- async $ preview fake (addressLink 100)
  threadDelay (testTimeout `div` 5)
  atomically $ putTMVar gate ()
  wait waiting `shouldReturn` ProfileOk (linkProfileOf $ profileFor (addressLink 100))
  mapM wait held `shouldReturn` map (ProfileOk . linkProfileOf . profileFor . addressLink) [0, 1]

-- Host rule

-- | Resolves the names of these tests; any other name does not resolve.
stubResolver :: TVar [String] -> HostResolver
stubResolver asked name = do
  atomically $ modifyTVar' asked (name :)
  pure $ Map.findWithDefault [] name addresses
  where
    addresses =
      Map.fromList
        [ ("smp8.simplex.im", ["1.1.1.1", "2606:4700::1111"]),
          ("loopback.test", ["127.0.0.1"]),
          ("private.test", ["10.0.0.1"]),
          ("cgnat.test", ["100.64.0.1"]),
          ("link-local.test", ["fe80::1"]),
          ("unique-local.test", ["fd00::1"]),
          ("mixed.test", ["1.1.1.1", "192.168.1.1"])
        ]

publicResolver :: HostResolver
publicResolver _ = pure ["1.1.1.1"]

hostCheck :: HostResolver -> L.NonEmpty TransportHost -> IO HostCheck
hostCheck resolver hosts = do
  ps <- quietProfiles answerAll resolver
  deadline <- deadlineIn testTimeout
  checkHosts ps deadline hosts

fetchable :: [TransportHost] -> IO Bool
fetchable hosts = newTVarIO [] >>= \asked -> (== HostPublic) <$> hostCheck (stubResolver asked) (L.fromList hosts)

testHostsChecked :: IO ()
testHostsChecked = do
  fetchable ["smp8.simplex.im", "8.8.8.8"] `shouldReturn` True
  forM_ (map (,True) public <> map (,False) refused) $ \(h, ok) -> (h,) <$> fetchable [h] `shouldReturn` (h, ok)
  where
    public = ["1.1.1.1", "2606:4700::1111", "smp8.simplex.im", mapped (1, 1, 1, 1), "198.20.0.1", "198.17.255.255", compatible (1, 1, 1, 1), nat64 (1, 1, 1, 1)]
    refused =
      ["127.0.0.1", "10.0.0.1", "172.16.5.4", "192.168.1.1", "100.64.0.1", "169.254.1.1", "::1", "fe80::1", "fd00::1"]
        <> ["loopback.test", "private.test", "cgnat.test", "link-local.test", "unique-local.test"]
        <> ["0.0.0.0", "::", "224.0.0.1", "ff02::1", mapped (127, 0, 0, 1), mapped (10, 0, 0, 1)]
        <> ["240.0.0.1", "255.255.255.255", "198.18.0.1", "198.19.255.254", "fec0::1"]
        <> map compatible [(127, 0, 0, 1), (10, 0, 0, 1)]
        <> map nat64 [(127, 0, 0, 1), (192, 168, 1, 1), (100, 64, 0, 1)]
    compatible = THIPv6 . (0, 0, 0,) . ipv4
    nat64 = THIPv6 . (0x0064ff9b, 0, 0,) . ipv4

testOnionUnresolvable :: IO ()
testOnionUnresolvable = do
  asked <- newTVarIO []
  hostCheck (stubResolver asked) ("abcdefghij234567.onion" :| []) `shouldReturn` HostPublic
  hostCheck (stubResolver asked) (THDomainName "abcdefghij234567.ONION." :| []) `shouldReturn` HostPublic
  hostCheck (stubResolver asked) ("abcdefghij234567.onion" :| ["127.0.0.1"]) `shouldReturn` HostRefused
  readTVarIO asked `shouldReturn` []
  hostCheck (stubResolver asked) ("unknown.test" :| []) `shouldReturn` HostPublic
  readTVarIO asked `shouldReturn` ["unknown.test"]
  hostCheck (const $ ioError $ userError "no such name") ("unknown.test" :| []) `shouldReturn` HostPublic

-- | An IPv4-mapped IPv6 address, @::ffff:a.b.c.d@.
mapped :: (Word32, Word32, Word32, Word32) -> TransportHost
mapped = THIPv6 . (0, 0, 0xffff,) . ipv4

ipv4 :: (Word32, Word32, Word32, Word32) -> Word32
ipv4 (a, b, c, d) = a * 0x1000000 + b * 0x10000 + c * 0x100 + d

testMixedHosts :: IO ()
testMixedHosts = do
  fetchable ["smp8.simplex.im", "10.0.0.1"] `shouldReturn` False
  fetchable ["1.1.1.1", "loopback.test"] `shouldReturn` False
  fetchable ["mixed.test"] `shouldReturn` False

testRefusedNotFetched :: IO ()
testRefusedNotFetched = do
  asked <- newTVarIO []
  fake@Fake {profiles} <- newFake testLimits answerAll (stubResolver asked)
  let l = addressLink 1
  (\(Preview _ r) -> r) <$> linkPreview profiles LKContact (parsed $ l <> "?h=private.test") `shouldReturn` ProfileUnavailable
  (\(Entity (Preview _ r) _ _) -> r) <$> linkEntity profiles LKContact (parsed $ addressLinkAt "127.0.0.1" 1) `shouldReturn` ProfileUnavailable
  fetchCount fake `shouldReturn` 0
  (\(Preview _ r) -> r) <$> linkPreview profiles LKContact (parsed l) `shouldReturn` ProfileOk (linkProfileOf $ profileFor l)
  fetchCount fake `shouldReturn` 1

-- Host lookups

-- | A resolver for which names starting with "held" report that their lookup started, then block past any timeout until the gate opens,
-- as getAddrInfo does; held names then resolve to a private address, and every other name to a public one.
heldResolver :: TVar [String] -> TQueue () -> TMVar () -> HostResolver
heldResolver asked started gate name = do
  atomically $ modifyTVar' asked (name :)
  if "held" `isPrefixOf` name
    then ["10.0.0.1"] <$ (atomically (writeTQueue started ()) >> uninterruptibleMask_ (atomically $ readTMVar gate))
    else pure ["1.1.1.1"]

testLookupBound :: IO ()
testLookupBound = do
  asked <- newTVarIO []
  started <- newTQueueIO
  gate <- newEmptyTMVarIO
  fake <- newFake testLimits {hostLookupSlots = 2} answerAll (heldResolver asked started gate)
  let at host = addressLinkAt host 1
  held <- mapM async [preview fake (at "held-1.test"), search fake (at "held-2.test")]
  replicateM_ 2 (atomically $ readTQueue started)
  mapM wait held `shouldReturn` [ProfileUnread, ProfileUnread]
  preview fake (at "free.test") `shouldReturn` ProfileUnavailable
  readTVarIO asked >>= (`shouldNotContain` ["free.test"])
  atomically $ putTMVar gate ()
  preview fake (at "free.test") `shouldReturn` ProfileOk (linkProfileOf $ profileFor (at "free.test"))
  fetchCount fake `shouldReturn` 1

testPreviewLookupShare :: IO ()
testPreviewLookupShare = do
  asked <- newTVarIO []
  started <- newTQueueIO
  gate <- newEmptyTMVarIO
  fake <- newFake testLimits {hostLookupSlots = 3, previewLookupSlots = 1} answerAll (heldResolver asked started gate)
  let at host = addressLinkAt host 1
  preview fake (at "held-1.test") `shouldReturn` ProfileUnread
  atomically $ readTQueue started
  preview fake (at "free.test") `shouldReturn` ProfileUnavailable
  search fake (at "free.test") `shouldReturn` ProfileOk (linkProfileOf $ profileFor (at "free.test"))
  atomically $ putTMVar gate ()

testRefusedHostCache :: IO ()
testRefusedHostCache = do
  asked <- newTVarIO []
  started <- newTQueueIO
  gate <- newEmptyTMVarIO
  let resolver name = if "held" `isPrefixOf` name then heldResolver asked started gate name else stubResolver asked name
  fake@Fake {clockAt} <- newFake testLimits {hostLookupSlots = 1} answerAll resolver
  let lookups host = length . filter (== host) <$> readTVarIO asked
  mapM_ (\n -> preview fake (addressLinkAt "private.test" n) `shouldReturn` ProfileUnavailable) [1, 2]
  lookups "private.test" `shouldReturn` 1
  atomically $ writeTVar clockAt 59
  preview fake (addressLinkAt "private.test" 3) `shouldReturn` ProfileUnavailable
  lookups "private.test" `shouldReturn` 1
  atomically $ writeTVar clockAt 60
  preview fake (addressLinkAt "private.test" 4) `shouldReturn` ProfileUnavailable
  lookups "private.test" `shouldReturn` 2
  preview fake (addressLinkAt "held.test" 1) `shouldReturn` ProfileUnread
  atomically $ putTMVar gate ()
  -- The one lookup slot frees in the same transaction that remembers the late refusal.
  preview fake (addressLink 1) `shouldReturn` ProfileOk (linkProfileOf $ profileFor (addressLink 1))
  preview fake (addressLinkAt "held.test" 2) `shouldReturn` ProfileUnavailable
  lookups "held.test" `shouldReturn` 1
  fetchCount fake `shouldReturn` 1

testSharedHostLookup :: IO ()
testSharedHostLookup = do
  asked <- newTVarIO []
  gate <- newEmptyTMVarIO
  let resolver name
        | name == "slow.test" = do
            atomically $ modifyTVar' asked (name :)
            ["1.1.1.1"] <$ uninterruptibleMask_ (atomically $ readTMVar gate)
        | otherwise = stubResolver asked name
  fake <- newFake testLimits {hostLookupSlots = 1} answerAll resolver
  let lookups = length . filter (== "slow.test") <$> readTVarIO asked
      slow = addressLinkAt "slow.test"
  replicateConcurrently 4 (search fake (slow 1)) `shouldReturn` replicate 4 ProfileUnread
  joined <- getCurrentTime
  search fake (slow 2) `shouldReturn` ProfileUnread
  (`diffUTCTime` joined) <$> getCurrentTime >>= (`shouldSatisfy` (< fromIntegral testTimeout / 1000000))
  lookups `shouldReturn` 1
  search fake (addressLink 1) `shouldReturn` ProfileUnavailable
  atomically $ putTMVar gate ()
  search fake (addressLink 2) `shouldReturn` ProfileOk (linkProfileOf $ profileFor (addressLink 2))
  search fake (slow 3) `shouldReturn` ProfileOk (linkProfileOf $ profileFor (slow 3))

-- Through the chat core

testChatProfiles :: TestParams -> IO ()
testChatProfiles ps = testChat2 aliceProfile bobProfile test ps
  where
    test alice' bob = withRelay ps $ \relay -> do
      bob ##> "/ad"
      (address, _) <- getContactLinks bob True
      (channel, _) <- prepareChannel1Relay "news" bob relay
      let fetch TestCC {chatController = cc@ChatController {currentUser}} kind l = do
            Just user <- readTVarIO currentUser
            sl <- either fail pure $ strDecode $ B.pack l
            chatProfile cc user kind sl >>= either (fail . show) pure
          Profile {image = bobImage} = bobProfile
          bobData (Just (LDContact ContactShortLinkData {profile = Profile {displayName, image}, business})) = (displayName, image, business) == ("bob", bobImage, False)
          bobData _ = False
          newsData (Just (LDChannel GroupShortLinkData {groupProfile = GroupProfile {displayName}, publicGroupData})) = (displayName, publicGroupData) == ("news", Just $ PublicGroupData 1)
          newsData _ = False
      fetch alice' LKContact address >>= (`shouldSatisfy` bobData)
      fetch alice' LKChannel channel >>= (`shouldSatisfy` newsData)
      fetch bob LKContact address >>= (`shouldSatisfy` bobData)
      fetch bob LKChannel channel >>= (`shouldSatisfy` newsData)
