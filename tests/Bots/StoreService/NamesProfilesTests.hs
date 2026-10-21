{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Bots.StoreService.NamesProfilesTests (namesProfilesTests, namesProfilesChatTests) where

import ChatClient (TestCC (..), testChat2)
import ChatTests.DBUtils (TestParams)
import ChatTests.Groups (prepareChannel1Relay)
import ChatTests.Utils (aliceProfile, bobProfile, getContactLinks, withRelay, (##>))
import qualified ChatTests.Utils as CU
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, concurrently, replicateConcurrently, wait)
import Control.Concurrent.STM
import Control.Exception (uninterruptibleMask_)
import Control.Monad (replicateM_)
import qualified Data.Aeson as J
import Data.Int (Int64)
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Word (Word32, Word64)
import Simplex.Chat.Controller (ChatErrorType (..), ChatError (..), ConnectionPlan (..), ContactAddressPlan (..), GroupLinkPlan (..))
import Simplex.Chat.Protocol (ContactShortLinkData (..), GroupShortLinkData (..), PublicGroupData (..))
import Simplex.Chat.Types (ChatPeerType (..), GroupProfile (..), ImageData (..), Profile (..))
import Simplex.Messaging.Agent.Protocol (ConnShortLink, ConnectionMode (..))
import Simplex.Messaging.Encoding.String (strDecode, strEncode)
import Simplex.Messaging.Transport.Client (TransportHost (..))
import StoreService.Names.Profiles
import Test.Hspec hiding (it)
import qualified Test.Hspec as H

namesProfilesTests :: Spec
namesProfilesTests = describe "StoreService names: profiles" $ do
  describe "plan to profile" $ do
    H.it "reads a contact address's link data as a contact" testPlanContact
    H.it "reads a business address by its flag or by its profile's peer type" testPlanBusiness
    H.it "reads a bot address by its profile's peer type" testPlanBot
    H.it "reads a channel with its subscriber count, also when it has no relays or needs an update" testPlanChannel
    H.it "reads the user's own address from the user's profile" testPlanOwnLink
    H.it "reads nothing from an error, missing link data or a connection being made" testPlanNothing
    H.it "drops an image that is not a data:image URI" testPlanImage
  describe "JSON" $ do
    H.it "encodes a contact entity as the contract's example" testEntityContactJSON
    H.it "encodes a channel entity with subscribers and a simplex:/ deep link" testEntityChannelJSON
    H.it "encodes previews without a profile as empty, kind from the link" testPreviewEmptyJSON
  describe "bounds and cache" $ do
    H.it "answers a link again from the cache, and fetches it again once the cache expires" testCacheExpiry
    H.it "does not cache a failed fetch" testFailureNotCached
    H.it "evicts the entry that expires soonest when the cache is full" testCacheBound
    H.it "gives up a fetch that takes longer than the timeout, freeing its slot" testFetchTimeout
    H.it "fills the preview bound while search still fetches, and a preview with no slot in time is unread" testPreviewBound
    H.it "fills the search bound while previews still fetch" testSearchBound
    H.it "lets a fetch wait for a slot that frees within the timeout" testSlotFreesInTime
    H.it "answers a link the chat core cannot parse unavailable without fetching" testUnparseableLink
  describe "host rule" $ do
    H.it "passes public IP literals and a name resolving to public addresses" testPublicHosts
    H.it "refuses private, loopback, CGNAT, link-local and unique-local literals" testPrivateLiterals
    H.it "refuses names resolving to those addresses" testPrivateNames
    H.it "refuses unspecified, multicast and IPv4-mapped private addresses" testOtherRefused
    H.it "refuses .onion hosts without resolving them, and unresolvable names" testOnionUnresolvable
    H.it "refuses a link with one public and one private host, and a name with one private address" testMixedHosts
    H.it "answers a refused link unavailable without fetching it" testRefusedNotFetched

namesProfilesChatTests :: SpecWith TestParams
namesProfilesChatTests =
  describe "StoreService names: profiles" $
    CU.it "reads an address and a channel through the chat core, and the owner's own from its store" testChatProfiles

-- Plans

ownProfile :: Profile
ownProfile = linkDataProfile "store" "SimpleX store" Nothing

contactData :: Bool -> Profile -> ConnectionPlan
contactData business profile =
  CPContactAddress CAPOk {contactSLinkData_ = Just ContactShortLinkData {profile, message = Nothing, business, localBadge = Nothing}, ownerVerification = Nothing} Nothing

channelData :: Maybe Int64 -> GroupShortLinkData
channelData count =
  GroupShortLinkData
    { groupProfile = GroupProfile {displayName = "news", fullName = "Bakery news", shortDescr = Nothing, description = Nothing, image = Nothing, publicGroup = Nothing, groupPreferences = Nothing, memberAdmission = Nothing},
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
jpeg = "data:image/jpeg;base64,/9j/4AAQSkZJRg=="

testPlanContact :: IO ()
testPlanContact =
  planProfile ownProfile (contactData False alice)
    `shouldBe` Just LinkProfile {kind = CKContact, displayName = "Alice's Bakery", fullName = "Sourdough and pastries, Lisbon", image = Just jpeg, subscribers = Nothing}

testPlanBusiness :: IO ()
testPlanBusiness = do
  kindOf (contactData True alice) `shouldBe` Just CKBusiness
  kindOf (contactData False alice {peerType = Just CPTBusiness}) `shouldBe` Just CKBusiness
  kindOf (contactData False alice {peerType = Just CPTHuman}) `shouldBe` Just CKContact

testPlanBot :: IO ()
testPlanBot = do
  kindOf (contactData False alice {peerType = Just CPTBot}) `shouldBe` Just CKBot
  kindOf (contactData True alice {peerType = Just CPTBot}) `shouldBe` Just CKBusiness

testPlanChannel :: IO ()
testPlanChannel = do
  let news count = Just LinkProfile {kind = CKChannel, displayName = "news", fullName = "Bakery news", image = Nothing, subscribers = count}
  planProfile ownProfile (CPGroupLink GLPOk {groupSLinkInfo_ = Nothing, groupSLinkData_ = Just (channelData (Just 1204)), ownerVerification = Nothing} Nothing) `shouldBe` news (Just 1204)
  planProfile ownProfile (CPGroupLink GLPOk {groupSLinkInfo_ = Nothing, groupSLinkData_ = Just (channelData Nothing), ownerVerification = Nothing} Nothing) `shouldBe` news Nothing
  planProfile ownProfile (CPGroupLink (GLPNoRelays (Just (channelData (Just 3)))) Nothing) `shouldBe` news (Just 3)
  planProfile ownProfile (CPGroupLink (GLPUpdateRequired (Just (channelData (Just 4)))) Nothing) `shouldBe` news (Just 4)

testPlanOwnLink :: IO ()
testPlanOwnLink =
  planProfile ownProfile (CPContactAddress CAPOwnLink Nothing)
    `shouldBe` Just LinkProfile {kind = CKContact, displayName = "store", fullName = "SimpleX store", image = Nothing, subscribers = Nothing}

testPlanNothing :: IO ()
testPlanNothing = do
  planProfile ownProfile (CPError (ChatError CEInvalidConnReq)) `shouldBe` Nothing
  planProfile ownProfile (CPContactAddress CAPOk {contactSLinkData_ = Nothing, ownerVerification = Nothing} Nothing) `shouldBe` Nothing
  planProfile ownProfile (CPGroupLink GLPOk {groupSLinkInfo_ = Nothing, groupSLinkData_ = Nothing, ownerVerification = Nothing} Nothing) `shouldBe` Nothing
  planProfile ownProfile (CPContactAddress CAPConnectingConfirmReconnect Nothing) `shouldBe` Nothing
  planProfile ownProfile (CPGroupLink GLPConnectingConfirmReconnect Nothing) `shouldBe` Nothing
  planProfile ownProfile (CPGroupLink (GLPConnectingProhibit Nothing) Nothing) `shouldBe` Nothing

testPlanImage :: IO ()
testPlanImage = do
  let imageOf p = planProfile ownProfile (contactData False p) >>= \LinkProfile {image} -> image
  imageOf (bakeryWith (Just (ImageData "javascript:alert(1)"))) `shouldBe` Nothing
  imageOf (bakeryWith (Just (ImageData "https://example.com/a.png"))) `shouldBe` Nothing
  imageOf alice `shouldBe` Just jpeg

kindOf :: ConnectionPlan -> Maybe ChatKind
kindOf plan = (\LinkProfile {kind} -> kind) <$> planProfile ownProfile plan

-- JSON

bakeryLink, newsLink :: Text
bakeryLink = "https://smp8.simplex.im/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rSAAAAAAAAA"
newsLink = "https://smp9.simplex.im/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA"

shouldEncodeAs :: J.ToJSON a => a -> LB.ByteString -> Expectation
shouldEncodeAs v expected = Just (J.toJSON v) `shouldBe` J.decode expected

testEntityContactJSON :: IO ()
testEntityContactJSON = do
  ps <- quietProfiles (pure . Just . const bakery) publicResolver
  e <- linkEntity ps ETAddress [publicHost] bakeryLink
  e
    `shouldEncodeAs` "{\"type\": \"address\", \"kind\": \"contact\", \"displayName\": \"Alice's Bakery\", \"fullName\": \"Sourdough and pastries, Lisbon\",\
                     \ \"image\": \"data:image/jpeg;base64,/9j/4AAQSkZJRg==\", \"subscribers\": null, \"profile\": \"ok\",\
                     \ \"link\": \"https://smp8.simplex.im/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rSAAAAAAAAA\", \"deepLink\": \"simplex:/a#Y8m3qT0vLw2Ab7cD9eF1gH3jK5mN7pQ9rSAAAAAAAAA\"}"
  where
    bakery = LinkProfile {kind = CKContact, displayName = "Alice's Bakery", fullName = "Sourdough and pastries, Lisbon", image = Just jpeg, subscribers = Nothing}

testEntityChannelJSON :: IO ()
testEntityChannelJSON = do
  ps <- quietProfiles (pure . Just . const news) publicResolver
  e <- linkEntity ps ETChannel [publicHost] newsLink
  e
    `shouldEncodeAs` "{\"type\": \"channel\", \"kind\": \"channel\", \"displayName\": \"Bakery news\", \"fullName\": \"Daily bakes and opening hours\",\
                     \ \"image\": null, \"subscribers\": 1204, \"profile\": \"ok\",\
                     \ \"link\": \"https://smp9.simplex.im/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA\", \"deepLink\": \"simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA\"}"
  let simplexLink = "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4LmAAAAAAAAA?h=smp9.simplex.im"
  Entity {link, deepLink} <- linkEntity ps ETChannel [publicHost] simplexLink
  (link, deepLink) `shouldBe` (simplexLink, simplexLink)
  where
    news = LinkProfile {kind = CKChannel, displayName = "Bakery news", fullName = "Daily bakes and opening hours", image = Nothing, subscribers = Just 1204}

testPreviewEmptyJSON :: IO ()
testPreviewEmptyJSON = do
  Preview ETChannel ProfileUnread
    `shouldEncodeAs` "{\"type\": \"channel\", \"kind\": \"channel\", \"displayName\": \"\", \"fullName\": \"\", \"image\": null, \"subscribers\": null, \"profile\": \"unread\"}"
  Preview ETAddress ProfileUnavailable
    `shouldEncodeAs` "{\"type\": \"address\", \"kind\": \"contact\", \"displayName\": \"\", \"fullName\": \"\", \"image\": null, \"subscribers\": null, \"profile\": \"unavailable\"}"
  Preview ETAddress (ProfileOk LinkProfile {kind = CKBot, displayName = "helper", fullName = "", image = Nothing, subscribers = Nothing})
    `shouldEncodeAs` "{\"type\": \"address\", \"kind\": \"bot\", \"displayName\": \"helper\", \"fullName\": \"\", \"image\": null, \"subscribers\": null, \"profile\": \"ok\"}"

-- Bounds and cache

-- | Long enough that a fetch which should have been answered from a slot or the cache would be seen waiting on it.
testTimeout :: Int
testTimeout = 500000

testLimits :: ProfileLimits
testLimits = defaultProfileLimits {fetchTimeout = testTimeout}

data Fake = Fake {profiles :: Profiles, clockAt :: TVar Word64, fetched :: TVar [Text]}

-- | A fetcher that records each link it is asked for, then answers with @answer@.
newFake :: ProfileLimits -> (Text -> IO (Maybe LinkProfile)) -> HostResolver -> IO Fake
newFake limits answer resolver = do
  clockAt <- newTVarIO 0
  fetched <- newTVarIO []
  let fetcher sl = do
        let l = linkText sl
        atomically $ modifyTVar' fetched (l :)
        answer l
  profiles <- newProfiles limits (readTVarIO clockAt) fetcher resolver
  pure Fake {profiles, clockAt, fetched}

quietProfiles :: (Text -> IO (Maybe LinkProfile)) -> HostResolver -> IO Profiles
quietProfiles answer resolver = (\Fake {profiles} -> profiles) <$> newFake testLimits answer resolver

linkText :: ConnShortLink 'CMContact -> Text
linkText = decodeUtf8 . strEncode

-- | A contact address link with a distinct canonical key, so it round-trips through the chat core's parser.
addressLink :: Int -> Text
addressLink n = "https://smp8.simplex.im/a#" <> decodeUtf8 (B64URL.encodeUnpadded (B.pack (take 32 (show n <> repeat '.'))))

profileFor :: Text -> LinkProfile
profileFor l = LinkProfile {kind = CKContact, displayName = T.takeEnd 8 l, fullName = "", image = Nothing, subscribers = Nothing}

answerAll :: Text -> IO (Maybe LinkProfile)
answerAll = pure . Just . profileFor

fetchCount :: Fake -> IO Int
fetchCount Fake {fetched} = length <$> readTVarIO fetched

preview :: Fake -> Text -> IO ProfileRead
preview Fake {profiles} l = (\(Preview _ r) -> r) <$> linkPreview profiles ETAddress [publicHost] l

search :: Fake -> Text -> IO ProfileRead
search Fake {profiles} l = (\Entity {entityPreview = Preview _ r} -> r) <$> linkEntity profiles ETAddress [publicHost] l

testCacheExpiry :: IO ()
testCacheExpiry = do
  fake@Fake {clockAt} <- newFake testLimits answerAll publicResolver
  let l = addressLink 1
  preview fake l `shouldReturn` ProfileOk (profileFor l)
  search fake l `shouldReturn` ProfileOk (profileFor l)
  fetchCount fake `shouldReturn` 1
  atomically $ writeTVar clockAt 599
  preview fake l `shouldReturn` ProfileOk (profileFor l)
  fetchCount fake `shouldReturn` 1
  atomically $ writeTVar clockAt 600
  preview fake l `shouldReturn` ProfileOk (profileFor l)
  fetchCount fake `shouldReturn` 2

testFailureNotCached :: IO ()
testFailureNotCached = do
  failing <- newTVarIO True
  fake <- newFake testLimits (\l -> readTVarIO failing >>= \f -> pure $ if f then Nothing else Just (profileFor l)) publicResolver
  let l = addressLink 1
  preview fake l `shouldReturn` ProfileUnread
  atomically $ writeTVar failing False
  preview fake l `shouldReturn` ProfileOk (profileFor l)
  fetchCount fake `shouldReturn` 2

testCacheBound :: IO ()
testCacheBound = do
  fake@Fake {clockAt} <- newFake testLimits {maxCachedProfiles = 2} answerAll publicResolver
  let a = addressLink 1
      b = addressLink 2
      c = addressLink 3
  mapM_ (\(t, l) -> atomically (writeTVar clockAt t) >> preview fake l) [(0, a), (1, b), (2, c)]
  fetchCount fake `shouldReturn` 3
  preview fake b `shouldReturn` ProfileOk (profileFor b)
  preview fake c `shouldReturn` ProfileOk (profileFor c)
  fetchCount fake `shouldReturn` 3
  preview fake a `shouldReturn` ProfileOk (profileFor a)
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
  replicateConcurrently 2 (preview fake (addressLink 1)) `shouldReturn` replicate 2 (ProfileOk (profileFor (addressLink 1)))
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
  (fake, started, gate) <- gatedFake
  held <- mapM (async . preview fake . addressLink) [0, 1]
  replicateM_ 2 (atomically $ readTQueue started)
  (searched, waited) <- concurrently (search fake (addressLink 100)) (preview fake (addressLink 101))
  searched `shouldBe` ProfileOk (profileFor (addressLink 100))
  waited `shouldBe` ProfileUnread
  readTVarIO (fetched fake) >>= (`shouldNotContain` [addressLink 101])
  atomically $ putTMVar gate ()
  mapM wait held >>= (`shouldSatisfy` all (`elem` [ProfileUnread, ProfileOk (profileFor (addressLink 0)), ProfileOk (profileFor (addressLink 1))]))

testSearchBound :: IO ()
testSearchBound = do
  (fake, started, gate) <- gatedFake
  held <- mapM (async . search fake . addressLink) [0 .. 3]
  replicateM_ 4 (atomically $ readTQueue started)
  (waited, previewed) <- concurrently (search fake (addressLink 100)) (preview fake (addressLink 101))
  waited `shouldBe` ProfileUnread
  previewed `shouldBe` ProfileOk (profileFor (addressLink 101))
  readTVarIO (fetched fake) >>= (`shouldNotContain` [addressLink 100])
  atomically $ putTMVar gate ()
  mapM_ wait held

testSlotFreesInTime :: IO ()
testSlotFreesInTime = do
  (fake, started, gate) <- gatedFake
  held <- mapM (async . preview fake . addressLink) [0, 1]
  replicateM_ 2 (atomically $ readTQueue started)
  waiting <- async $ preview fake (addressLink 100)
  threadDelay (testTimeout `div` 5)
  atomically $ putTMVar gate ()
  wait waiting `shouldReturn` ProfileOk (profileFor (addressLink 100))
  mapM wait held `shouldReturn` map (ProfileOk . profileFor . addressLink) [0, 1]

testUnparseableLink :: IO ()
testUnparseableLink = do
  fake <- newFake testLimits answerAll publicResolver
  mapM_ (\l -> preview fake l `shouldReturn` ProfileUnavailable) ["https://smp8.simplex.im/a#x", "simplex:/c#Rk27xN4pDe9Tz1Wb3Yc5Vd7Xf9Hg2Jk4Lm"]
  fetchCount fake `shouldReturn` 0

-- Host rule

publicHost :: TransportHost
publicHost = "smp8.simplex.im"

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

fetchable :: [TransportHost] -> IO Bool
fetchable hosts = newTVarIO [] >>= \asked -> hostsFetchable (stubResolver asked) hosts

testPublicHosts :: IO ()
testPublicHosts = do
  fetchable ["1.1.1.1"] `shouldReturn` True
  fetchable ["2606:4700::1111"] `shouldReturn` True
  fetchable ["smp8.simplex.im"] `shouldReturn` True
  fetchable ["smp8.simplex.im", "8.8.8.8"] `shouldReturn` True

testPrivateLiterals :: IO ()
testPrivateLiterals =
  mapM_ (\h -> (h,) <$> fetchable [h] `shouldReturn` (h, False)) ["127.0.0.1", "10.0.0.1", "172.16.5.4", "192.168.1.1", "100.64.0.1", "169.254.1.1", "::1", "fe80::1", "fd00::1"]

testPrivateNames :: IO ()
testPrivateNames =
  mapM_ (\h -> (h,) <$> fetchable [h] `shouldReturn` (h, False)) ["loopback.test", "private.test", "cgnat.test", "link-local.test", "unique-local.test"]

testOtherRefused :: IO ()
testOtherRefused = do
  mapM_ (\h -> (h,) <$> fetchable [h] `shouldReturn` (h, False)) ["0.0.0.0", "::", "224.0.0.1", "ff02::1", mapped (127, 0, 0, 1), mapped (10, 0, 0, 1)]
  fetchable [mapped (1, 1, 1, 1)] `shouldReturn` True
  fetchable [] `shouldReturn` False

testOnionUnresolvable :: IO ()
testOnionUnresolvable = do
  asked <- newTVarIO []
  hostsFetchable (stubResolver asked) ["abcdefghij234567.onion"] `shouldReturn` False
  hostsFetchable (stubResolver asked) [THDomainName "abcdefghij234567.ONION."] `shouldReturn` False
  readTVarIO asked `shouldReturn` []
  hostsFetchable (stubResolver asked) ["unknown.test"] `shouldReturn` False
  readTVarIO asked `shouldReturn` ["unknown.test"]

-- | An IPv4-mapped IPv6 address, @::ffff:a.b.c.d@.
mapped :: (Word32, Word32, Word32, Word32) -> TransportHost
mapped (a, b, c, d) = THIPv6 (0, 0, 0xffff, a * 0x1000000 + b * 0x10000 + c * 0x100 + d)

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
  (\(Preview _ r) -> r) <$> linkPreview profiles ETAddress ["smp8.simplex.im", "private.test"] l `shouldReturn` ProfileUnavailable
  (\Entity {entityPreview = Preview _ r} -> r) <$> linkEntity profiles ETAddress ["127.0.0.1"] l `shouldReturn` ProfileUnavailable
  fetchCount fake `shouldReturn` 0
  (\(Preview _ r) -> r) <$> linkPreview profiles ETAddress ["smp8.simplex.im"] l `shouldReturn` ProfileOk (profileFor l)
  fetchCount fake `shouldReturn` 1

-- Through the chat core

testChatProfiles :: TestParams -> IO ()
testChatProfiles ps = testChat2 aliceProfile bobProfile test ps
  where
    test alice' bob = withRelay ps $ \relay -> do
      bob ##> "/ad"
      (address, _) <- getContactLinks bob True
      (channel, _) <- prepareChannel1Relay "news" bob relay
      let fetch TestCC {chatController} l = either error (chatProfile chatController 1) (strDecode (B.pack l))
          Profile {image = bobImage} = bobProfile
          bobLinkProfile = Just LinkProfile {kind = CKContact, displayName = "bob", fullName = "", image = (\(ImageData i) -> i) <$> bobImage, subscribers = Nothing}
          news = Just LinkProfile {kind = CKChannel, displayName = "news", fullName = "", image = Nothing, subscribers = Just 1}
      fetch alice' address `shouldReturn` bobLinkProfile
      fetch alice' channel `shouldReturn` news
      fetch bob address `shouldReturn` bobLinkProfile
      fetch bob channel `shouldReturn` news
