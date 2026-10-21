{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
    defaultProfileLimits,
    newProfiles,
    monotonicSeconds,
    chatProfile,
    systemResolver,
    planProfile,
    hostsFetchable,
    linkEntity,
    linkPreview,
  )
where

import Control.Concurrent (forkFinally)
import Control.Concurrent.STM
import Control.Exception (bracket_)
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as JT
import Data.Bits (shiftL, shiftR, (.|.))
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (dropWhileEnd, isSuffixOf)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
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
import Simplex.Messaging.Util (anyM, safeDecodeUtf8)
import StoreService.Names.Links (LinkKind (..), parseShortLink, shortLinkHosts)
import System.Timeout (timeout)

data ProfileLimits = ProfileLimits
  { searchFetchSlots :: Int,
    previewFetchSlots :: Int,
    -- | Microseconds for one fetch, the wait for its slot and its host check included.
    fetchTimeout :: Int,
    cacheSeconds :: Word64,
    maxCachedProfiles :: Int
  }

defaultProfileLimits :: ProfileLimits
defaultProfileLimits =
  ProfileLimits
    { searchFetchSlots = 4,
      previewFetchSlots = 2,
      fetchTimeout = 3000000,
      cacheSeconds = 600,
      maxCachedProfiles = 2048
    }

-- | Nothing when the link data could not be read.
type ProfileFetcher = ConnShortLink 'CMContact -> IO (Maybe LinkProfile)

-- | The addresses a name resolves to, none when it does not resolve.
type HostResolver = HostName -> IO [TransportHost]

data Profiles = Profiles
  { limits :: ProfileLimits,
    clock :: IO Word64,
    fetchLink :: ProfileFetcher,
    resolveHost :: HostResolver,
    searchSlots :: TVar Int,
    previewSlots :: TVar Int,
    cache :: TVar (Map Text (Word64, LinkProfile))
  }

newProfiles :: ProfileLimits -> IO Word64 -> ProfileFetcher -> HostResolver -> IO Profiles
newProfiles limits@ProfileLimits {searchFetchSlots, previewFetchSlots} clock fetchLink resolveHost = do
  searchSlots <- newTVarIO searchFetchSlots
  previewSlots <- newTVarIO previewFetchSlots
  cache <- newTVarIO Map.empty
  pure Profiles {limits, clock, fetchLink, resolveHost, searchSlots, previewSlots, cache}

monotonicSeconds :: IO Word64
monotonicSeconds = (`div` 1000000000) <$> getMonotonicTimeNSec

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
boundedProfile Profiles {limits = ProfileLimits {fetchTimeout, cacheSeconds, maxCachedProfiles}, clock, fetchLink, resolveHost, cache} slots link sl_ =
  cached >>= \case
    Just p -> pure $ ProfileOk p
    Nothing -> case sl_ of
      Nothing -> pure ProfileUnavailable
      Just sl -> fromMaybe ProfileUnread <$> timeout fetchTimeout (checkAndFetch sl)
  where
    cached = do
      now <- clock
      (\m -> case Map.lookup link m of Just (expiry, p) | expiry > now -> Just p; _ -> Nothing) <$> readTVarIO cache
    checkAndFetch sl =
      hostsFetchable resolveHost (shortLinkHosts sl) >>= \case
        False -> pure ProfileUnavailable
        True -> withSlot (fetchLink sl) >>= maybe (pure ProfileUnread) (\p -> ProfileOk p <$ remember p)
    withSlot =
      bracket_
        (atomically $ readTVar slots >>= \n -> check (n > 0) >> writeTVar slots (n - 1))
        (atomically $ modifyTVar' slots (+ 1))
    remember p = do
      now <- clock
      atomically $ modifyTVar' cache $ Map.insert link (now + cacheSeconds, p) . makeRoom now
    makeRoom now m
      | Map.size m < maxCachedProfiles || Map.member link m = m
      | otherwise =
          let live = Map.filter ((> now) . fst) m
           in if Map.size live < maxCachedProfiles then live else maybe live (`Map.delete` live) (soonestExpiry live)
    soonestExpiry = fmap fst . Map.foldrWithKey (\k (e, _) acc -> if maybe True ((e <) . snd) acc then Just (k, e) else acc) Nothing

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

-- | False when any host is an onion address, does not resolve, or is or resolves to an address outside the public internet.
hostsFetchable :: HostResolver -> NonEmpty TransportHost -> IO Bool
hostsFetchable resolve hosts = not <$> anyM (map refused $ L.toList hosts)
  where
    refused = \case
      THDomainName name
        | onionName name -> pure True
        | otherwise -> (\addresses -> null addresses || not (all publicAddress addresses)) <$> resolve name
      host -> pure $ not $ publicAddress host
    onionName name = ".onion" `isSuffixOf` dropWhileEnd (== '.') (map toLower name)

publicAddress :: TransportHost -> Bool
publicAddress = \case
  THIPv4 a -> publicIPv4 (ipv4Word a)
  THIPv6 a -> publicIPv6 a
  _ -> False

-- | Unspecified ("this network"), private, CGNAT, loopback, link-local and multicast.
refusedIPv4 :: [(Word32, Int)]
refusedIPv4 =
  [ (ipv4Word (0, 0, 0, 0), 8),
    (ipv4Word (10, 0, 0, 0), 8),
    (ipv4Word (100, 64, 0, 0), 10),
    (ipv4Word (127, 0, 0, 0), 8),
    (ipv4Word (169, 254, 0, 0), 16),
    (ipv4Word (172, 16, 0, 0), 12),
    (ipv4Word (192, 168, 0, 0), 16),
    (ipv4Word (224, 0, 0, 0), 4)
  ]

-- | Unique-local, link-local and multicast, as prefixes of the address's first 32 bits.
refusedIPv6 :: [(Word32, Int)]
refusedIPv6 = [(0xfc000000, 7), (0xfe800000, 10), (0xff000000, 8)]

-- | The third 32-bit word of an IPv4-mapped IPv6 address, @::ffff:a.b.c.d@.
ipv4MappedWord :: Word32
ipv4MappedWord = 0xffff

publicIPv4 :: Word32 -> Bool
publicIPv4 a = not $ any (inPrefix a) refusedIPv4

publicIPv6 :: (Word32, Word32, Word32, Word32) -> Bool
publicIPv6 (w0, w1, w2, w3)
  | w0 == 0 && w1 == 0 && w2 == ipv4MappedWord = publicIPv4 w3
  | w0 == 0 && w1 == 0 && w2 == 0 && w3 <= 1 = False
  | otherwise = not $ any (inPrefix w0) refusedIPv6

inPrefix :: Word32 -> (Word32, Int) -> Bool
inPrefix a (net, len) = a `shiftR` (32 - len) == net `shiftR` (32 - len)

ipv4Word :: (Word8, Word8, Word8, Word8) -> Word32
ipv4Word (a, b, c, d) = fromIntegral a `shiftL` 24 .|. fromIntegral b `shiftL` 16 .|. fromIntegral c `shiftL` 8 .|. fromIntegral d

-- | getAddrInfo is a foreign call that a timeout cannot interrupt, so it runs in its own thread, which a timed-out caller leaves to finish.
systemResolver :: HostResolver
systemResolver name = do
  result <- newEmptyTMVarIO
  _ <- forkFinally (getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just name) Nothing) $
    atomically . putTMVar result . either (const []) (mapMaybe (sockHost . addrAddress))
  atomically $ readTMVar result
  where
    sockHost = \case
      SockAddrInet _ a -> Just $ THIPv4 $ hostAddressToTuple a
      SockAddrInet6 _ _ a _ -> Just $ THIPv6 a
      SockAddrUnix _ -> Nothing
