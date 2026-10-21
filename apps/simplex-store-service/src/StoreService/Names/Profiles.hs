{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The host rule is best effort: the chat core resolves a host again when it connects, so the deployment's egress rule is the real guard.
module StoreService.Names.Profiles
  ( Profiles,
    ProfileLimits (..),
    ProfileFetcher,
    HostResolver,
    ChatKind (..),
    LinkProfile (..),
    ProfileRead (..),
    Preview (..),
    Entity (..),
    HostCheck (..),
    Deadline,
    defaultProfileLimits,
    newProfiles,
    monotonicSeconds,
    deadlineIn,
    chatProfile,
    systemResolver,
    planProfile,
    checkHosts,
    linkEntity,
    linkPreview,
  )
where

import Control.Concurrent (forkIOWithUnmask)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, mask_, try)
import Control.Monad (join, unless)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Bool (bool)
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (dropWhileEnd, isSuffixOf)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (AddrInfo (..), HostName, SockAddr (..), SocketType (..), defaultHints, getAddrInfo, hostAddressToTuple)
import Simplex.Chat.Controller (ChatResponse (..), ChatCommand (APIConnectPlan), ChatController, ConnectionPlan (..), ContactAddressPlan (..), GroupLinkPlan (..), PlanResolveMode (..))
import Simplex.Chat.Core (sendChatCmd)
import Simplex.Chat.Protocol (ContactShortLinkData (..), GroupShortLinkData (..), PublicGroupData (..))
import Simplex.Chat.Types (AConnectTarget (..), ChatPeerType (..), Contact (..), ContactNameOrLink (..), ConnectTarget (..), GroupInfo (..), GroupProfile (..), GroupSummary (..), ImageData (..), Profile (..), User (..), fromLocalProfile)
import Simplex.Messaging.Agent.Protocol (ConnShortLink, ConnectionMode (..), SConnectionMode (..), UserId, simplexShortLink)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Transport.Client (TransportHost (..))
import Simplex.Messaging.Util (safeDecodeUtf8)
import StoreService.Names.Links (LinkKind (..), parseShortLink, shortLinkHosts)
import System.Timeout (timeout)

data ProfileLimits = ProfileLimits
  { searchFetchSlots :: Int,
    previewFetchSlots :: Int,
    -- | Host name lookups in flight across the service, each holding a thread until the resolver answers.
    hostLookupSlots :: Int,
    -- | Microseconds for one fetch, the wait for its slot and its host check included.
    fetchTimeout :: Int,
    cacheSeconds :: Word64,
    maxCachedProfiles :: Int,
    refusedHostSeconds :: Word64,
    maxRefusedHosts :: Int
  }

defaultProfileLimits :: ProfileLimits
defaultProfileLimits =
  ProfileLimits
    { searchFetchSlots = 4,
      previewFetchSlots = 2,
      hostLookupSlots = 8,
      fetchTimeout = 3000000,
      cacheSeconds = 600,
      maxCachedProfiles = 2048,
      refusedHostSeconds = 60,
      maxRefusedHosts = 2048
    }

-- | Nothing when the link data could not be read.
type ProfileFetcher = ConnShortLink 'CMContact -> IO (Maybe LinkProfile)

-- | The addresses a name resolves to; none or an exception when it does not resolve.
type HostResolver = HostName -> IO [TransportHost]

data Profiles = Profiles
  { limits :: ProfileLimits,
    clock :: IO Word64,
    fetchLink :: ProfileFetcher,
    resolveHost :: HostResolver,
    searchSlots :: TVar Int,
    previewSlots :: TVar Int,
    lookupSlots :: TVar Int,
    cache :: TVar (Map Text (Word64, LinkProfile)),
    refusedHosts :: TVar (Map HostName (Word64, ()))
  }

newProfiles :: ProfileLimits -> IO Word64 -> ProfileFetcher -> HostResolver -> IO Profiles
newProfiles limits@ProfileLimits {searchFetchSlots, previewFetchSlots, hostLookupSlots} clock fetchLink resolveHost = do
  searchSlots <- newTVarIO searchFetchSlots
  previewSlots <- newTVarIO previewFetchSlots
  lookupSlots <- newTVarIO hostLookupSlots
  cache <- newTVarIO Map.empty
  refusedHosts <- newTVarIO Map.empty
  pure Profiles {limits, clock, fetchLink, resolveHost, searchSlots, previewSlots, lookupSlots, cache, refusedHosts}

monotonicSeconds :: IO Word64
monotonicSeconds = (`div` 1000000000) <$> getMonotonicTimeNSec

-- | A time on the monotonic clock, in nanoseconds.
newtype Deadline = Deadline Word64

nanosPerMicro :: Word64
nanosPerMicro = 1000

deadlineIn :: Int -> IO Deadline
deadlineIn micros = Deadline . (+ fromIntegral micros * nanosPerMicro) <$> getMonotonicTimeNSec

remainingMicros :: Deadline -> IO Int
remainingMicros (Deadline d) = (\now -> if now >= d then 0 else fromIntegral $ (d - now) `div` nanosPerMicro) <$> getMonotonicTimeNSec

-- | Nothing once the deadline passes without the transaction completing.
waitUntil :: Deadline -> STM a -> IO (Maybe a)
waitUntil deadline act = do
  expired <- registerDelay =<< remainingMicros deadline
  atomically $ (Just <$> act) `orElse` (Nothing <$ (readTVar expired >>= check))

takeSlot :: TVar Int -> STM ()
takeSlot slots = readTVar slots >>= \n -> check (n > 0) >> writeTVar slots (n - 1)

releaseSlot :: TVar Int -> STM ()
releaseSlot slots = modifyTVar' slots (+ 1)

-- | Nothing when no slot frees before the deadline, or the action does not end by it.
withSlot :: Deadline -> TVar Int -> IO a -> IO (Maybe a)
withSlot deadline slots act =
  bracket (waitUntil deadline $ takeSlot slots) (mapM_ $ \() -> atomically $ releaseSlot slots) $ \case
    Nothing -> pure Nothing
    Just () -> remainingMicros deadline >>= (`timeout` act)

freshEntry :: Ord k => Word64 -> k -> Map k (Word64, v) -> Maybe v
freshEntry now k m = case Map.lookup k m of
  Just (expiry, v) | expiry > now -> Just v
  _ -> Nothing

-- | A full map first drops its expired entries, then the entry that expires soonest.
insertBounded :: Ord k => Int -> Word64 -> k -> (Word64, v) -> Map k (Word64, v) -> Map k (Word64, v)
insertBounded maxSize now k entry m = Map.insert k entry room
  where
    room
      | Map.size m < maxSize || Map.member k m = m
      | otherwise =
          let live = Map.filter ((> now) . fst) m
           in if Map.size live < maxSize then live else maybe live (`Map.delete` live) (soonestExpiry live)
    soonestExpiry = fmap fst . Map.foldrWithKey (\k' (e, _) acc -> if maybe True ((e <) . snd) acc then Just (k', e) else acc) Nothing

data ChatKind = CKContact | CKBusiness | CKBot | CKChannel
  deriving (Eq, Show)

data LinkProfile = LinkProfile
  { kind :: ChatKind,
    displayName :: Text,
    fullName :: Text,
    image :: Maybe Text,
    subscribers :: Maybe Int64
  }
  deriving (Eq, Show)

data ProfileRead = ProfileOk LinkProfile | ProfileUnread | ProfileUnavailable
  deriving (Eq, Show)

data Preview = Preview {previewType :: LinkKind, previewProfile :: ProfileRead}
  deriving (Eq, Show)

data Entity = Entity {entityPreview :: Preview, link :: Text, deepLink :: Text}
  deriving (Eq, Show)

instance J.ToJSON Preview where
  toJSON = J.object . previewPairs

instance J.ToJSON Entity where
  toJSON Entity {entityPreview, link, deepLink} = J.object $ previewPairs entityPreview <> ["link" .= link, "deepLink" .= deepLink]

previewPairs :: Preview -> [JT.Pair]
previewPairs (Preview t r) =
  [ "type" .= linkKindText t,
    "kind" .= chatKindText kind,
    "displayName" .= displayName,
    "fullName" .= fullName,
    "image" .= image,
    "subscribers" .= subscribers,
    "profile" .= readText
  ]
  where
    (LinkProfile {kind, displayName, fullName, image, subscribers}, readText) = case r of
      ProfileOk p -> (p, "ok" :: Text)
      ProfileUnread -> (emptyProfile, "unread")
      ProfileUnavailable -> (emptyProfile, "unavailable")
    emptyProfile = LinkProfile {kind = chatKind t, displayName = "", fullName = "", image = Nothing, subscribers = Nothing}
    chatKind = \case
      LKContact -> CKContact
      LKChannel -> CKChannel

-- | The record the link came from, as the contract names it: @simplex.contact@ holds an address.
linkKindText :: LinkKind -> Text
linkKindText = \case
  LKContact -> "address"
  LKChannel -> "channel"

chatKindText :: ChatKind -> Text
chatKindText = \case
  CKContact -> "contact"
  CKBusiness -> "business"
  CKBot -> "bot"
  CKChannel -> "channel"

-- | A search answer's entity, within the search bound. Every server host of the link must pass the host rule.
-- The deep link is the core's @simplex:/@ form, which carries the server; a link the core cannot parse keeps its own text.
linkEntity :: Profiles -> LinkKind -> Text -> IO Entity
linkEntity ps@Profiles {searchSlots} t link = do
  r <- boundedProfile ps searchSlots link sl_
  pure Entity {entityPreview = Preview t r, link, deepLink = maybe link (safeDecodeUtf8 . strEncode . simplexShortLink) sl_}
  where
    sl_ = parseShortLink link

-- | A link preview, within the preview bound.
linkPreview :: Profiles -> LinkKind -> Text -> IO Preview
linkPreview ps@Profiles {previewSlots} t link = Preview t <$> boundedProfile ps previewSlots link (parseShortLink link)

-- | A link the chat core cannot parse names no server it could fetch from, so it is not fetched.
boundedProfile :: Profiles -> TVar Int -> Text -> Maybe (ConnShortLink 'CMContact) -> IO ProfileRead
boundedProfile ps@Profiles {limits = ProfileLimits {fetchTimeout, cacheSeconds, maxCachedProfiles}, clock, fetchLink, cache} slots link sl_ =
  cached >>= \case
    Just p -> pure $ ProfileOk p
    Nothing -> case sl_ of
      Nothing -> pure ProfileUnavailable
      Just sl -> do
        deadline <- deadlineIn fetchTimeout
        checkHosts ps deadline (shortLinkHosts sl) >>= \case
          HostRefused -> pure ProfileUnavailable
          HostUnanswered -> pure ProfileUnread
          HostPublic -> maybe ProfileUnread ProfileOk . join <$> withSlot deadline slots (fetchLink sl >>= mapM remember)
  where
    cached = clock >>= \now -> freshEntry now link <$> readTVarIO cache
    remember p = do
      now <- clock
      p <$ atomically (modifyTVar' cache $ insertBounded maxCachedProfiles now link (now + cacheSeconds, p))

-- | Runs the connect plan, which reads the link data without connecting. A known chat answers with its stored profile.
chatProfile :: ChatController -> UserId -> ProfileFetcher
chatProfile cc userId sl =
  sendChatCmd cc (APIConnectPlan userId (Just $ ACTarget SCMContact $ CTShortContact $ CTLink sl) PRMUnknown Nothing) >>= \case
    Right CRConnectionPlan {user = User {profile}, connectionPlan} -> pure $ planProfile (fromLocalProfile profile) connectionPlan
    _ -> pure Nothing

-- | The first argument is the profile of the user who ran the plan, which their own address shows.
planProfile :: Profile -> ConnectionPlan -> Maybe LinkProfile
planProfile own = \case
  CPContactAddress plan _ -> case plan of
    CAPOk {contactSLinkData_} -> (\ContactShortLinkData {profile, business} -> contactProfile business profile) <$> contactSLinkData_
    CAPOwnLink -> Just $ contactProfile False own
    CAPConnectingConfirmReconnect -> Nothing
    CAPConnectingProhibit {contact} -> Just $ knownContact contact
    CAPKnown {contact} -> Just $ knownContact contact
    CAPContactViaAddress {contact} -> Just $ knownContact contact
  CPGroupLink plan _ -> case plan of
    GLPOk {groupSLinkData_} -> channelProfile <$> groupSLinkData_
    GLPNoRelays {groupSLinkData_} -> channelProfile <$> groupSLinkData_
    GLPUpdateRequired {groupSLinkData_} -> channelProfile <$> groupSLinkData_
    GLPOwnLink {groupInfo} -> Just $ knownGroup groupInfo
    GLPConnectingConfirmReconnect -> Nothing
    GLPConnectingProhibit {groupInfo_} -> knownGroup <$> groupInfo_
    GLPKnown {groupInfo} -> Just $ knownGroup groupInfo
  CPInvitationLink _ -> Nothing
  CPError _ -> Nothing
  where
    knownContact Contact {profile} = contactProfile False (fromLocalProfile profile)
    channelProfile GroupShortLinkData {groupProfile, publicGroupData} =
      groupProfileOf CKChannel groupProfile ((\PublicGroupData {publicMemberCount} -> publicMemberCount) <$> publicGroupData)
    knownGroup GroupInfo {groupProfile, businessChat, groupSummary = GroupSummary {publicMemberCount}} = case businessChat of
      Just _ -> groupProfileOf CKBusiness groupProfile Nothing
      Nothing -> groupProfileOf CKChannel groupProfile publicMemberCount

contactProfile :: Bool -> Profile -> LinkProfile
contactProfile business Profile {displayName, fullName, image, peerType} =
  LinkProfile {kind, displayName, fullName, image = image >>= imageURI, subscribers = Nothing}
  where
    kind
      | business || peerType == Just CPTBusiness = CKBusiness
      | peerType == Just CPTBot = CKBot
      | otherwise = CKContact

groupProfileOf :: ChatKind -> GroupProfile -> Maybe Int64 -> LinkProfile
groupProfileOf kind GroupProfile {displayName, fullName, image} subscribers =
  LinkProfile {kind, displayName, fullName, image = image >>= imageURI, subscribers}

-- | A profile image the page can show as it is.
imageURI :: ImageData -> Maybe Text
imageURI (ImageData uri)
  | "data:image/" `T.isPrefixOf` uri = Just uri
  | otherwise = Nothing

data HostCheck = HostPublic | HostRefused | HostUnanswered
  deriving (Eq, Show)

-- | Refused when any host is an onion address, does not resolve, is or resolves to an address outside the public internet,
-- or finds no lookup slot before the deadline; unanswered when a lookup does not answer by the deadline.
checkHosts :: Profiles -> Deadline -> NonEmpty TransportHost -> IO HostCheck
checkHosts ps deadline = checkAll . L.toList
  where
    checkAll = \case
      [] -> pure HostPublic
      host : hosts ->
        checkHost host >>= \case
          HostPublic -> checkAll hosts
          result -> pure result
    checkHost = \case
      THDomainName name
        | onionName name -> pure HostRefused
        | otherwise -> lookupHost ps deadline name
      host -> pure $ if publicAddress host then HostPublic else HostRefused
    onionName name = ".onion" `isSuffixOf` dropWhileEnd (== '.') (map toLower name)

-- | getAddrInfo is a foreign call that a timeout cannot interrupt, so each lookup runs in its own thread,
-- which holds its lookup slot until the resolver answers, also after the caller has given up.
lookupHost :: Profiles -> Deadline -> HostName -> IO HostCheck
lookupHost Profiles {limits = ProfileLimits {refusedHostSeconds, maxRefusedHosts}, clock, resolveHost, lookupSlots, refusedHosts} deadline name = do
  now <- clock
  cachedRefusal <- isJust . freshEntry now name <$> readTVarIO refusedHosts
  if cachedRefusal
    then pure HostRefused
    else do
      answer <- newEmptyTMVarIO
      started <- mask_ $ waitUntil deadline (takeSlot lookupSlots) >>= mapM (\() -> forkIOWithUnmask $ \unmask -> try (unmask $ resolveHost name) >>= finish answer)
      case started of
        Nothing -> pure HostRefused
        Just _ -> maybe HostUnanswered (bool HostRefused HostPublic) <$> waitUntil deadline (readTMVar answer)
  where
    finish answer resolved = do
      let public = either (\(_ :: SomeException) -> False) allPublic resolved
      now <- clock
      atomically $ do
        unless public $ modifyTVar' refusedHosts $ insertBounded maxRefusedHosts now name (now + refusedHostSeconds, ())
        putTMVar answer public
        releaseSlot lookupSlots
    allPublic addresses = not (null addresses) && all publicAddress addresses

publicAddress :: TransportHost -> Bool
publicAddress = \case
  THIPv4 a -> publicIPv4 (ipv4Word a)
  THIPv6 a -> publicIPv6 a
  _ -> False

-- | Unspecified ("this network"), private, CGNAT, loopback, link-local, benchmarking, multicast and reserved with broadcast.
refusedIPv4 :: [(Word32, Int)]
refusedIPv4 =
  [ (ipv4Word (0, 0, 0, 0), 8),
    (ipv4Word (10, 0, 0, 0), 8),
    (ipv4Word (100, 64, 0, 0), 10),
    (ipv4Word (127, 0, 0, 0), 8),
    (ipv4Word (169, 254, 0, 0), 16),
    (ipv4Word (172, 16, 0, 0), 12),
    (ipv4Word (192, 168, 0, 0), 16),
    (ipv4Word (198, 18, 0, 0), 15),
    (ipv4Word (224, 0, 0, 0), 4),
    (ipv4Word (240, 0, 0, 0), 4)
  ]

-- | Unique-local, link-local, site-local and multicast, as prefixes of the address's first 32 bits.
refusedIPv6 :: [(Word32, Int)]
refusedIPv6 = [(0xfc000000, 7), (0xfe800000, 10), (0xfec00000, 10), (0xff000000, 8)]

-- | The third 32-bit word of an IPv4-mapped IPv6 address, @::ffff:a.b.c.d@.
ipv4MappedWord :: Word32
ipv4MappedWord = 0xffff

-- | The first 32-bit word of the NAT64 prefix @64:ff9b::/96@.
nat64Word :: Word32
nat64Word = 0x0064ff9b

publicIPv4 :: Word32 -> Bool
publicIPv4 a = not $ any (inPrefix a) refusedIPv4

-- | An address that carries an IPv4 address in its last 32 bits is judged by it: IPv4-mapped, IPv4-compatible (with @::@ and @::1@) and NAT64.
publicIPv6 :: (Word32, Word32, Word32, Word32) -> Bool
publicIPv6 (w0, w1, w2, w3)
  | w0 == 0 && w1 == 0 && (w2 == ipv4MappedWord || w2 == 0) = publicIPv4 w3
  | w0 == nat64Word && w1 == 0 && w2 == 0 = publicIPv4 w3
  | otherwise = not $ any (inPrefix w0) refusedIPv6

inPrefix :: Word32 -> (Word32, Int) -> Bool
inPrefix a (net, len) = a `shiftR` (32 - len) == net `shiftR` (32 - len)

ipv4Word :: (Word8, Word8, Word8, Word8) -> Word32
ipv4Word (a, b, c, d) = fromIntegral a `shiftL` 24 .|. fromIntegral b `shiftL` 16 .|. fromIntegral c `shiftL` 8 .|. fromIntegral d

systemResolver :: HostResolver
systemResolver name = mapMaybe (sockHost . addrAddress) <$> getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just name) Nothing
  where
    sockHost = \case
      SockAddrInet _ a -> Just $ THIPv4 $ hostAddressToTuple a
      SockAddrInet6 _ _ a _ -> Just $ THIPv6 a
      SockAddrUnix _ -> Nothing
