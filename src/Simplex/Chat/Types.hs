{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Simplex.Chat.Types where

import Crypto.Number.Serialize (os2ip)
import Data.Aeson (FromJSON (..), ToJSON (..), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as B
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Util
import Simplex.FileTransfer.Description (FileDigest)
import Simplex.Messaging.Agent.Protocol (ACommandTag (..), ACorrId, AParty (..), APartyCmdTag (..), ConnId, ConnectionMode (..), ConnectionRequestUri, InvitationId, SAEntity (..), UserId)
import Simplex.Messaging.Crypto.File (CryptoFileArgs (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, fromTextField_, sumTypeJSON, taggedObjectJSON)
import Simplex.Messaging.Protocol (ProtoServerWithAuth, ProtocolTypeI)
import Simplex.Messaging.Util ((<$?>))
import Simplex.Messaging.Version

class IsContact a where
  contactId' :: a -> ContactId
  profile' :: a -> LocalProfile
  localDisplayName' :: a -> ContactName
  preferences' :: a -> Maybe Preferences

instance IsContact User where
  contactId' u = u.userContactId
  {-# INLINE contactId' #-}
  profile' u = u.profile
  {-# INLINE profile' #-}
  localDisplayName' u = u.localDisplayName
  {-# INLINE localDisplayName' #-}
  preferences' User {profile = LocalProfile {preferences}} = preferences
  {-# INLINE preferences' #-}

instance IsContact Contact where
  contactId' c = c.contactId
  {-# INLINE contactId' #-}
  profile' c = c.profile
  {-# INLINE profile' #-}
  localDisplayName' c = c.localDisplayName
  {-# INLINE localDisplayName' #-}
  preferences' Contact {profile = LocalProfile {preferences}} = preferences
  {-# INLINE preferences' #-}

newtype AgentUserId = AgentUserId UserId
  deriving (Eq, Show)

instance StrEncoding AgentUserId where
  strEncode (AgentUserId uId) = strEncode uId
  strDecode s = AgentUserId <$> strDecode s
  strP = AgentUserId <$> strP

instance FromJSON AgentUserId where
  parseJSON = strParseJSON "AgentUserId"

instance ToJSON AgentUserId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromField AgentUserId where fromField f = AgentUserId <$> fromField f

instance ToField AgentUserId where toField (AgentUserId uId) = toField uId

aUserId :: User -> UserId
aUserId User {agentUserId = AgentUserId uId} = uId

data User = User
  { userId :: UserId,
    agentUserId :: AgentUserId,
    userContactId :: ContactId,
    localDisplayName :: ContactName,
    profile :: LocalProfile,
    fullPreferences :: FullPreferences,
    activeUser :: Bool,
    viewPwdHash :: Maybe UserPwdHash,
    showNtfs :: Bool,
    sendRcptsContacts :: Bool,
    sendRcptsSmallGroups :: Bool
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON User where
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}

data NewUser = NewUser
  { profile :: Maybe Profile,
    sameServers :: Bool,
    pastTimestamp :: Bool
  }
  deriving (Show, Generic, FromJSON)

newtype B64UrlByteString = B64UrlByteString ByteString
  deriving (Eq, Show)

instance FromField B64UrlByteString where fromField f = B64UrlByteString <$> fromField f

instance ToField B64UrlByteString where toField (B64UrlByteString m) = toField m

instance StrEncoding B64UrlByteString where
  strEncode (B64UrlByteString m) = strEncode m
  strP = B64UrlByteString <$> strP

instance FromJSON B64UrlByteString where
  parseJSON = strParseJSON "B64UrlByteString"

instance ToJSON B64UrlByteString where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data UserPwdHash = UserPwdHash {hash :: B64UrlByteString, salt :: B64UrlByteString}
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON UserPwdHash where toEncoding = J.genericToEncoding J.defaultOptions

data UserInfo = UserInfo
  { user :: User,
    unreadCount :: Int
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON UserInfo where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

type ContactId = Int64

type ProfileId = Int64

data Contact = Contact
  { contactId :: ContactId,
    localDisplayName :: ContactName,
    profile :: LocalProfile,
    activeConn :: Connection,
    viaGroup :: Maybe Int64,
    contactUsed :: Bool,
    chatSettings :: ChatSettings,
    userPreferences :: Preferences,
    mergedPreferences :: ContactUserPreferences,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    chatTs :: Maybe UTCTime,
    contactGroupMemberId :: Maybe GroupMemberId,
    contactGrpInvSent :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON Contact where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

contactConn :: Contact -> Connection
contactConn Contact{activeConn} = activeConn

contactConnId :: Contact -> ConnId
contactConnId = aConnId . contactConn

type IncognitoEnabled = Bool

contactConnIncognito :: Contact -> IncognitoEnabled
contactConnIncognito = connIncognito . contactConn

contactDirect :: Contact -> Bool
contactDirect Contact {activeConn = Connection {connLevel, viaGroupLink}} = connLevel == 0 && not viaGroupLink

directOrUsed :: Contact -> Bool
directOrUsed ct@Contact {contactUsed} =
  contactDirect ct || contactUsed

anyDirectOrUsed :: Contact -> Bool
anyDirectOrUsed Contact {contactUsed, activeConn = Connection {connLevel}} = connLevel == 0 || contactUsed

contactSecurityCode :: Contact -> Maybe SecurityCode
contactSecurityCode Contact {activeConn} = connectionCode activeConn

data ContactRef = ContactRef
  { contactId :: ContactId,
    connId :: Int64,
    agentConnId :: AgentConnId,
    localDisplayName :: ContactName
  }
  deriving (Eq, Show, Generic)

instance ToJSON ContactRef where toEncoding = J.genericToEncoding J.defaultOptions

data ContactOrGroupMember = CGMContact Contact | CGMGroupMember GroupInfo GroupMember
  deriving (Show)

contactOrGroupMemberIds :: ContactOrGroupMember -> (Maybe ContactId, Maybe GroupMemberId)
contactOrGroupMemberIds = \case
  CGMContact Contact {contactId} -> (Just contactId, Nothing)
  CGMGroupMember _ GroupMember {groupMemberId} -> (Nothing, Just groupMemberId)

contactOrGroupMemberIncognito :: ContactOrGroupMember -> IncognitoEnabled
contactOrGroupMemberIncognito = \case
  CGMContact ct -> contactConnIncognito ct
  CGMGroupMember _ m -> memberIncognito m

data UserContact = UserContact
  { userContactLinkId :: Int64,
    connReqContact :: ConnReqContact,
    groupId :: Maybe GroupId
  }
  deriving (Eq, Show, Generic)

userContactGroupId :: UserContact -> Maybe GroupId
userContactGroupId UserContact {groupId} = groupId

instance ToJSON UserContact where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

data UserContactRequest = UserContactRequest
  { contactRequestId :: Int64,
    agentInvitationId :: AgentInvId,
    userContactLinkId :: Int64,
    agentContactConnId :: AgentConnId, -- connection id of user contact
    cReqChatVRange :: JVersionRange,
    localDisplayName :: ContactName,
    profileId :: Int64,
    profile :: Profile,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    xContactId :: Maybe XContactId
  }
  deriving (Eq, Show, Generic)

instance ToJSON UserContactRequest where
  toEncoding = J.genericToEncoding J.defaultOptions

newtype XContactId = XContactId ByteString
  deriving (Eq, Show)

instance FromField XContactId where fromField f = XContactId <$> fromField f

instance ToField XContactId where toField (XContactId m) = toField m

instance StrEncoding XContactId where
  strEncode (XContactId m) = strEncode m
  strDecode s = XContactId <$> strDecode s
  strP = XContactId <$> strP

instance FromJSON XContactId where
  parseJSON = strParseJSON "XContactId"

instance ToJSON XContactId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

newtype ConnReqUriHash = ConnReqUriHash {unConnReqUriHash :: ByteString}
  deriving (Eq, Show)

instance FromField ConnReqUriHash where fromField f = ConnReqUriHash <$> fromField f

instance ToField ConnReqUriHash where toField (ConnReqUriHash m) = toField m

instance StrEncoding ConnReqUriHash where
  strEncode (ConnReqUriHash m) = strEncode m
  strDecode s = ConnReqUriHash <$> strDecode s
  strP = ConnReqUriHash <$> strP

instance FromJSON ConnReqUriHash where
  parseJSON = strParseJSON "ConnReqUriHash"

instance ToJSON ConnReqUriHash where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data ContactOrRequest = CORContact Contact | CORRequest UserContactRequest

type UserName = Text

type ContactName = Text

type GroupName = Text

optionalFullName :: ContactName -> Text -> Text
optionalFullName displayName fullName
  | T.null fullName || displayName == fullName = ""
  | otherwise = " (" <> fullName <> ")"

data Group = Group {groupInfo :: GroupInfo, members :: [GroupMember]}
  deriving (Eq, Show, Generic)

instance ToJSON Group where toEncoding = J.genericToEncoding J.defaultOptions

type GroupId = Int64

data GroupInfo = GroupInfo
  { groupId :: GroupId,
    localDisplayName :: GroupName,
    groupProfile :: GroupProfile,
    fullGroupPreferences :: FullGroupPreferences,
    membership :: GroupMember,
    hostConnCustomUserProfileId :: Maybe ProfileId,
    chatSettings :: ChatSettings,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    chatTs :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON GroupInfo where toEncoding = J.genericToEncoding J.defaultOptions

groupName' :: GroupInfo -> GroupName
groupName' GroupInfo {localDisplayName = g} = g

data GroupSummary = GroupSummary
  { currentMembers :: Int
  }
  deriving (Show, Generic)

instance ToJSON GroupSummary where toEncoding = J.genericToEncoding J.defaultOptions

data ContactOrGroup = CGContact Contact | CGGroup Group

contactAndGroupIds :: ContactOrGroup -> (Maybe ContactId, Maybe GroupId)
contactAndGroupIds = \case
  CGContact Contact {contactId} -> (Just contactId, Nothing)
  CGGroup (Group GroupInfo {groupId} _) -> (Nothing, Just groupId)

-- TODO when more settings are added we should create another type to allow partial setting updates (with all Maybe properties)
data ChatSettings = ChatSettings
  { enableNtfs :: Bool,
    sendRcpts :: Maybe Bool,
    favorite :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON ChatSettings where toEncoding = J.genericToEncoding J.defaultOptions

defaultChatSettings :: ChatSettings
defaultChatSettings =
  ChatSettings
    { enableNtfs = True,
      sendRcpts = Nothing,
      favorite = False
    }

pattern DisableNtfs :: ChatSettings
pattern DisableNtfs <- ChatSettings {enableNtfs = False}

featureAllowed :: SChatFeature f -> (PrefEnabled -> Bool) -> Contact -> Bool
featureAllowed feature forWhom Contact {mergedPreferences} =
  let ContactUserPreference {enabled} = getContactUserPreference feature mergedPreferences
   in forWhom enabled

groupFeatureAllowed :: GroupFeatureI f => SGroupFeature f -> GroupInfo -> Bool
groupFeatureAllowed feature gInfo = groupFeatureAllowed' feature $ fullGroupPreferences gInfo

mergeUserChatPrefs :: User -> Contact -> FullPreferences
mergeUserChatPrefs user ct = mergeUserChatPrefs' user (contactConnIncognito ct) (userPreferences ct)

mergeUserChatPrefs' :: User -> Bool -> Preferences -> FullPreferences
mergeUserChatPrefs' user connectedIncognito userPreferences =
  let userPrefs = if connectedIncognito then Nothing else preferences' user
   in mergePreferences (Just userPreferences) userPrefs

updateMergedPreferences :: User -> Contact -> Contact
updateMergedPreferences user ct =
  let mergedPreferences = contactUserPreferences user (userPreferences ct) (preferences' ct) (contactConnIncognito ct)
   in ct {mergedPreferences}

contactUserPreferences :: User -> Preferences -> Maybe Preferences -> Bool -> ContactUserPreferences
contactUserPreferences user userPreferences contactPreferences connectedIncognito =
  ContactUserPreferences
    { timedMessages = pref SCFTimedMessages,
      fullDelete = pref SCFFullDelete,
      reactions = pref SCFReactions,
      voice = pref SCFVoice,
      calls = pref SCFCalls
    }
  where
    pref :: FeatureI f => SChatFeature f -> ContactUserPreference (FeaturePreference f)
    pref f =
      ContactUserPreference
        { enabled = prefEnabled (asymmetric f) userPref ctPref,
          -- incognito contact cannot have default user preference used
          userPreference = if connectedIncognito then CUPContact ctUserPref else maybe (CUPUser userPref) CUPContact ctUserPref_,
          contactPreference = ctPref
        }
      where
        asymmetric SCFTimedMessages = False
        asymmetric _ = True
        ctUserPref = getPreference f userPreferences
        ctUserPref_ = chatPrefSel f userPreferences
        userPref = getPreference f ctUserPrefs
        ctPref = getPreference f ctPrefs
    ctUserPrefs = mergeUserChatPrefs' user connectedIncognito userPreferences
    ctPrefs = mergePreferences contactPreferences Nothing

data Profile = Profile
  { displayName :: ContactName,
    fullName :: Text,
    image :: Maybe ImageData,
    contactLink :: Maybe ConnReqContact,
    preferences :: Maybe Preferences
    -- fields that should not be read into this data type to prevent sending them as part of profile to contacts:
    -- - contact_profile_id
    -- - incognito
    -- - local_alias
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON Profile where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

-- check if profiles match ignoring preferences
profilesMatch :: LocalProfile -> LocalProfile -> Bool
profilesMatch
  LocalProfile {displayName = n1, fullName = fn1, image = i1}
  LocalProfile {displayName = n2, fullName = fn2, image = i2} =
    n1 == n2 && fn1 == fn2 && i1 == i2

data IncognitoProfile = NewIncognito Profile | ExistingIncognito LocalProfile

type LocalAlias = Text

data LocalProfile = LocalProfile
  { profileId :: ProfileId,
    displayName :: ContactName,
    fullName :: Text,
    image :: Maybe ImageData,
    contactLink :: Maybe ConnReqContact,
    preferences :: Maybe Preferences,
    localAlias :: LocalAlias
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON LocalProfile where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

localProfileId :: LocalProfile -> ProfileId
localProfileId LocalProfile{profileId} = profileId

toLocalProfile :: ProfileId -> Profile -> LocalAlias -> LocalProfile
toLocalProfile profileId Profile {displayName, fullName, image, contactLink, preferences} localAlias =
  LocalProfile {profileId, displayName, fullName, image, contactLink, preferences, localAlias}

fromLocalProfile :: LocalProfile -> Profile
fromLocalProfile LocalProfile {displayName, fullName, image, contactLink, preferences} =
  Profile {displayName, fullName, image, contactLink, preferences}

data GroupProfile = GroupProfile
  { displayName :: GroupName,
    fullName :: Text,
    description :: Maybe Text,
    image :: Maybe ImageData,
    groupPreferences :: Maybe GroupPreferences
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupProfile where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

newtype ImageData = ImageData Text
  deriving (Eq, Show)

instance FromJSON ImageData where
  parseJSON = fmap ImageData . J.parseJSON

instance ToJSON ImageData where
  toJSON (ImageData t) = J.toJSON t
  toEncoding (ImageData t) = J.toEncoding t

instance ToField ImageData where toField (ImageData t) = toField t

instance FromField ImageData where fromField = fmap ImageData . fromField

data CReqClientData = CRDataGroup {groupLinkId :: GroupLinkId}
  deriving (Generic)

instance ToJSON CReqClientData where
  toJSON = J.genericToJSON . taggedObjectJSON $ dropPrefix "CRData"
  toEncoding = J.genericToEncoding . taggedObjectJSON $ dropPrefix "CRData"

instance FromJSON CReqClientData where
  parseJSON = J.genericParseJSON . taggedObjectJSON $ dropPrefix "CRData"

newtype GroupLinkId = GroupLinkId {unGroupLinkId :: ByteString} -- used to identify invitation via group link
  deriving (Eq, Show)

instance FromField GroupLinkId where fromField f = GroupLinkId <$> fromField f

instance ToField GroupLinkId where toField (GroupLinkId g) = toField g

instance StrEncoding GroupLinkId where
  strEncode (GroupLinkId g) = strEncode g
  strDecode s = GroupLinkId <$> strDecode s
  strP = GroupLinkId <$> strP

instance FromJSON GroupLinkId where
  parseJSON = strParseJSON "GroupLinkId"

instance ToJSON GroupLinkId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data GroupInvitation = GroupInvitation
  { fromMember :: MemberIdRole,
    invitedMember :: MemberIdRole,
    connRequest :: ConnReqInvitation,
    groupProfile :: GroupProfile,
    groupLinkId :: Maybe GroupLinkId
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupInvitation where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data MemberIdRole = MemberIdRole
  { memberId :: MemberId,
    memberRole :: GroupMemberRole
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON MemberIdRole where toEncoding = J.genericToEncoding J.defaultOptions

data IntroInvitation = IntroInvitation
  { groupConnReq :: ConnReqInvitation,
    directConnReq :: Maybe ConnReqInvitation
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON IntroInvitation where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data MemberInfo = MemberInfo
  { memberId :: MemberId,
    memberRole :: GroupMemberRole,
    v :: Maybe ChatVersionRange,
    profile :: Profile
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON MemberInfo where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

memberInfo :: GroupMember -> MemberInfo
memberInfo GroupMember {memberId, memberRole, memberProfile, activeConn} =
  MemberInfo memberId memberRole memberChatVRange (fromLocalProfile memberProfile)
  where
    memberChatVRange = ChatVersionRange . fromJVersionRange . peerChatVRange <$> activeConn

data ReceivedGroupInvitation = ReceivedGroupInvitation
  { fromMember :: GroupMember,
    connRequest :: ConnReqInvitation,
    groupInfo :: GroupInfo
  }
  deriving (Eq, Show)

type GroupMemberId = Int64

-- memberProfile's profileId is COALESCE(member_profile_id, contact_profile_id), member_profile_id is non null
-- if incognito profile was saved for member (used for hosts and invitees in incognito groups)
data GroupMember = GroupMember
  { groupMemberId :: GroupMemberId,
    groupId :: GroupId,
    memberId :: MemberId,
    memberRole :: GroupMemberRole,
    memberCategory :: GroupMemberCategory,
    memberStatus :: GroupMemberStatus,
    invitedBy :: InvitedBy,
    localDisplayName :: ContactName,
    -- for membership, memberProfile can be either user's profile or incognito profile, based on memberIncognito test.
    -- for other members it's whatever profile the local user can see (there is no info about whether it's main or incognito profile for remote users).
    memberProfile :: LocalProfile,
    -- this is the ID of the associated contact (it will be used to send direct messages to the member)
    memberContactId :: Maybe ContactId,
    -- for membership it would always point to user's contact
    -- it is used to test for incognito status by comparing with ID in memberProfile
    memberContactProfileId :: ProfileId,
    activeConn :: Maybe Connection
  }
  deriving (Eq, Show, Generic)

instance ToJSON GroupMember where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data GroupMemberRef = GroupMemberRef {groupMemberId :: Int64, profile :: Profile}
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupMemberRef where toEncoding = J.genericToEncoding J.defaultOptions

groupMemberRef :: GroupMember -> GroupMemberRef
groupMemberRef GroupMember {groupMemberId, memberProfile = p} =
  GroupMemberRef {groupMemberId, profile = fromLocalProfile p}

memberConn :: GroupMember -> Maybe Connection
memberConn GroupMember{activeConn} = activeConn

memberConnId :: GroupMember -> Maybe ConnId
memberConnId GroupMember {activeConn} = aConnId <$> activeConn

groupMemberId' :: GroupMember -> GroupMemberId
groupMemberId' GroupMember {groupMemberId} = groupMemberId

memberIncognito :: GroupMember -> IncognitoEnabled
memberIncognito GroupMember {memberProfile, memberContactProfileId} = localProfileId memberProfile /= memberContactProfileId

incognitoMembership :: GroupInfo -> IncognitoEnabled
incognitoMembership GroupInfo {membership} = memberIncognito membership

-- returns profile when membership is incognito, otherwise Nothing
incognitoMembershipProfile :: GroupInfo -> Maybe LocalProfile
incognitoMembershipProfile GroupInfo {membership = m@GroupMember {memberProfile}}
  | memberIncognito m = Just memberProfile
  | otherwise = Nothing

memberSecurityCode :: GroupMember -> Maybe SecurityCode
memberSecurityCode GroupMember {activeConn} = connectionCode =<< activeConn

data NewGroupMember = NewGroupMember
  { memInfo :: MemberInfo,
    memCategory :: GroupMemberCategory,
    memStatus :: GroupMemberStatus,
    memInvitedBy :: InvitedBy,
    localDisplayName :: ContactName,
    memProfileId :: Int64,
    memContactId :: Maybe Int64
  }

newtype MemberId = MemberId {unMemberId :: ByteString}
  deriving (Eq, Show)

instance FromField MemberId where fromField f = MemberId <$> fromField f

instance ToField MemberId where toField (MemberId m) = toField m

instance StrEncoding MemberId where
  strEncode (MemberId m) = strEncode m
  strDecode s = MemberId <$> strDecode s
  strP = MemberId <$> strP

instance FromJSON MemberId where
  parseJSON = strParseJSON "MemberId"

instance ToJSON MemberId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data InvitedBy = IBContact {byContactId :: Int64} | IBUser | IBUnknown
  deriving (Eq, Show, Generic)

instance ToJSON InvitedBy where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "IB"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "IB"

toInvitedBy :: Int64 -> Maybe Int64 -> InvitedBy
toInvitedBy userCtId (Just ctId)
  | userCtId == ctId = IBUser
  | otherwise = IBContact ctId
toInvitedBy _ Nothing = IBUnknown

fromInvitedBy :: Int64 -> InvitedBy -> Maybe Int64
fromInvitedBy userCtId = \case
  IBUnknown -> Nothing
  IBContact ctId -> Just ctId
  IBUser -> Just userCtId

data GroupMemberRole
  = GRObserver -- connects to all group members and receives all messages, can't send messages
  | GRAuthor -- reserved, unused
  | GRMember -- + can send messages to all group members
  | GRAdmin -- + add/remove members, change member role (excl. Owners)
  | GROwner -- + delete and change group information, add/remove/change roles for Owners
  deriving (Eq, Show, Ord)

instance FromField GroupMemberRole where fromField = fromBlobField_ strDecode

instance ToField GroupMemberRole where toField = toField . strEncode

instance StrEncoding GroupMemberRole where
  strEncode = \case
    GROwner -> "owner"
    GRAdmin -> "admin"
    GRMember -> "member"
    GRAuthor -> "author"
    GRObserver -> "observer"
  strDecode = \case
    "owner" -> Right GROwner
    "admin" -> Right GRAdmin
    "member" -> Right GRMember
    "author" -> Right GRAuthor
    "observer" -> Right GRObserver
    r -> Left $ "bad GroupMemberRole " <> B.unpack r
  strP = strDecode <$?> A.takeByteString

instance FromJSON GroupMemberRole where
  parseJSON = strParseJSON "GroupMemberRole"

instance ToJSON GroupMemberRole where
  toJSON = strToJSON
  toEncoding = strToJEncoding

newtype Probe = Probe {unProbe :: ByteString}
  deriving (Eq, Show)

instance StrEncoding Probe where
  strEncode (Probe p) = strEncode p
  strDecode s = Probe <$> strDecode s
  strP = Probe <$> strP

instance FromJSON Probe where
  parseJSON = strParseJSON "Probe"

instance ToJSON Probe where
  toJSON = strToJSON
  toEncoding = strToJEncoding

newtype ProbeHash = ProbeHash {unProbeHash :: ByteString}
  deriving (Eq, Show)

instance StrEncoding ProbeHash where
  strEncode (ProbeHash p) = strEncode p
  strDecode s = ProbeHash <$> strDecode s
  strP = ProbeHash <$> strP

instance FromJSON ProbeHash where
  parseJSON = strParseJSON "ProbeHash"

instance ToJSON ProbeHash where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data GroupMemberCategory
  = GCUserMember
  | GCInviteeMember -- member invited by the user
  | GCHostMember -- member who invited the user
  | GCPreMember -- member who joined before the user and was introduced to the user (user receives x.grp.mem.intro about such members)
  | GCPostMember -- member who joined after the user to whom the user was introduced (user receives x.grp.mem.new announcing these members and then x.grp.mem.fwd with invitation from these members)
  deriving (Eq, Show)

instance FromField GroupMemberCategory where fromField = fromTextField_ textDecode

instance ToField GroupMemberCategory where toField = toField . textEncode

instance ToJSON GroupMemberCategory where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance TextEncoding GroupMemberCategory where
  textDecode = \case
    "user" -> Just GCUserMember
    "invitee" -> Just GCInviteeMember
    "host" -> Just GCHostMember
    "pre" -> Just GCPreMember
    "post" -> Just GCPostMember
    _ -> Nothing
  textEncode = \case
    GCUserMember -> "user"
    GCInviteeMember -> "invitee"
    GCHostMember -> "host"
    GCPreMember -> "pre"
    GCPostMember -> "post"

data GroupMemberStatus
  = GSMemRemoved -- member who was removed from the group
  | GSMemLeft -- member who left the group
  | GSMemGroupDeleted -- user member of the deleted group
  | GSMemInvited -- member is sent to or received invitation to join the group
  | GSMemIntroduced -- user received x.grp.mem.intro for this member (only with GCPreMember)
  | GSMemIntroInvited -- member is sent to or received from intro invitation
  | GSMemAccepted -- member accepted invitation (only User and Invitee)
  | GSMemAnnounced -- host announced (x.grp.mem.new) a member (Invitee and PostMember) to the group - at this point this member can send messages and invite other members (if they have sufficient permissions)
  | GSMemConnected -- member created the group connection with the inviting member
  | GSMemComplete -- host confirmed (x.grp.mem.all) that a member (User, Invitee and PostMember) created group connections with all previous members
  | GSMemCreator -- user member that created the group (only GCUserMember)
  deriving (Eq, Show, Ord)

instance FromField GroupMemberStatus where fromField = fromTextField_ textDecode

instance ToField GroupMemberStatus where toField = toField . textEncode

instance ToJSON GroupMemberStatus where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

memberActive :: GroupMember -> Bool
memberActive m = case memberStatus m of
  GSMemRemoved -> False
  GSMemLeft -> False
  GSMemGroupDeleted -> False
  GSMemInvited -> False
  GSMemIntroduced -> False
  GSMemIntroInvited -> False
  GSMemAccepted -> False
  GSMemAnnounced -> False
  GSMemConnected -> True
  GSMemComplete -> True
  GSMemCreator -> True

memberCurrent :: GroupMember -> Bool
memberCurrent = memberCurrent' . memberStatus

-- update getGroupSummary if this is changed
memberCurrent' :: GroupMemberStatus -> Bool
memberCurrent' = \case
  GSMemRemoved -> False
  GSMemLeft -> False
  GSMemGroupDeleted -> False
  GSMemInvited -> False
  GSMemIntroduced -> True
  GSMemIntroInvited -> True
  GSMemAccepted -> True
  GSMemAnnounced -> True
  GSMemConnected -> True
  GSMemComplete -> True
  GSMemCreator -> True

memberRemoved :: GroupMember -> Bool
memberRemoved m = case memberStatus m of
  GSMemRemoved -> True
  GSMemLeft -> True
  GSMemGroupDeleted -> True
  GSMemInvited -> False
  GSMemIntroduced -> False
  GSMemIntroInvited -> False
  GSMemAccepted -> False
  GSMemAnnounced -> False
  GSMemConnected -> False
  GSMemComplete -> False
  GSMemCreator -> False

instance TextEncoding GroupMemberStatus where
  textDecode = \case
    "removed" -> Just GSMemRemoved
    "left" -> Just GSMemLeft
    "deleted" -> Just GSMemGroupDeleted
    "invited" -> Just GSMemInvited
    "introduced" -> Just GSMemIntroduced
    "intro-inv" -> Just GSMemIntroInvited
    "accepted" -> Just GSMemAccepted
    "announced" -> Just GSMemAnnounced
    "connected" -> Just GSMemConnected
    "complete" -> Just GSMemComplete
    "creator" -> Just GSMemCreator
    _ -> Nothing
  textEncode = \case
    GSMemRemoved -> "removed"
    GSMemLeft -> "left"
    GSMemGroupDeleted -> "deleted"
    GSMemInvited -> "invited"
    GSMemIntroduced -> "introduced"
    GSMemIntroInvited -> "intro-inv"
    GSMemAccepted -> "accepted"
    GSMemAnnounced -> "announced"
    GSMemConnected -> "connected"
    GSMemComplete -> "complete"
    GSMemCreator -> "creator"

data SndFileTransfer = SndFileTransfer
  { fileId :: FileTransferId,
    fileName :: String,
    filePath :: String,
    fileSize :: Integer,
    chunkSize :: Integer,
    recipientDisplayName :: ContactName,
    connId :: Int64,
    agentConnId :: AgentConnId,
    groupMemberId :: Maybe Int64,
    fileStatus :: FileStatus,
    fileDescrId :: Maybe Int64,
    fileInline :: Maybe InlineFileMode
  }
  deriving (Eq, Show, Generic)

instance ToJSON SndFileTransfer where toEncoding = J.genericToEncoding J.defaultOptions

sndFileTransferConnId :: SndFileTransfer -> ConnId
sndFileTransferConnId SndFileTransfer {agentConnId = AgentConnId acId} = acId

type FileTransferId = Int64

data FileInvitation = FileInvitation
  { fileName :: String,
    fileSize :: Integer,
    fileDigest :: Maybe FileDigest,
    fileConnReq :: Maybe ConnReqInvitation,
    fileInline :: Maybe InlineFileMode,
    fileDescr :: Maybe FileDescr
  }
  deriving (Eq, Show, Generic)

instance ToJSON FileInvitation where
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}

instance FromJSON FileInvitation where
  parseJSON = J.genericParseJSON J.defaultOptions {J.omitNothingFields = True}

data FileDescr = FileDescr {fileDescrText :: Text, fileDescrPartNo :: Int, fileDescrComplete :: Bool}
  deriving (Eq, Show, Generic)

instance ToJSON FileDescr where
  toEncoding = J.genericToEncoding J.defaultOptions
  toJSON = J.genericToJSON J.defaultOptions

instance FromJSON FileDescr where
  parseJSON = J.genericParseJSON J.defaultOptions

xftpFileInvitation :: FilePath -> Integer -> FileDescr -> FileInvitation
xftpFileInvitation fileName fileSize fileDescr =
  FileInvitation
    { fileName,
      fileSize,
      fileDigest = Nothing,
      fileConnReq = Nothing,
      fileInline = Nothing,
      fileDescr = Just fileDescr
    }

data InlineFileMode
  = IFMOffer -- file will be sent inline once accepted
  | IFMSent -- file is sent inline without acceptance
  deriving (Eq, Show, Generic)

instance TextEncoding InlineFileMode where
  textEncode = \case
    IFMOffer -> "offer"
    IFMSent -> "sent"
  textDecode = \case
    "offer" -> Just IFMOffer
    "sent" -> Just IFMSent
    _ -> Nothing

instance FromField InlineFileMode where fromField = fromTextField_ textDecode

instance ToField InlineFileMode where toField = toField . textEncode

instance FromJSON InlineFileMode where
  parseJSON = J.withText "InlineFileMode" $ maybe (fail "bad InlineFileMode") pure . textDecode

instance ToJSON InlineFileMode where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

data RcvFileTransfer = RcvFileTransfer
  { fileId :: FileTransferId,
    xftpRcvFile :: Maybe XFTPRcvFile,
    fileInvitation :: FileInvitation,
    fileStatus :: RcvFileStatus,
    rcvFileInline :: Maybe InlineFileMode,
    senderDisplayName :: ContactName,
    chunkSize :: Integer,
    cancelled :: Bool,
    grpMemberId :: Maybe Int64,
    -- XFTP files are encrypted as they are received, they are never stored unecrypted
    -- SMP files are encrypted after all chunks are received
    cryptoArgs :: Maybe CryptoFileArgs
  }
  deriving (Eq, Show, Generic)

instance ToJSON RcvFileTransfer where toEncoding = J.genericToEncoding J.defaultOptions

data XFTPRcvFile = XFTPRcvFile
  { rcvFileDescription :: RcvFileDescr,
    agentRcvFileId :: Maybe AgentRcvFileId,
    agentRcvFileDeleted :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON XFTPRcvFile where toEncoding = J.genericToEncoding J.defaultOptions

data RcvFileDescr = RcvFileDescr
  { fileDescrId :: Int64,
    fileDescrText :: Text,
    fileDescrPartNo :: Int,
    fileDescrComplete :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON RcvFileDescr where toEncoding = J.genericToEncoding J.defaultOptions

data RcvFileStatus
  = RFSNew
  | RFSAccepted RcvFileInfo
  | RFSConnected RcvFileInfo
  | RFSComplete RcvFileInfo
  | RFSCancelled (Maybe RcvFileInfo)
  deriving (Eq, Show, Generic)

instance ToJSON RcvFileStatus where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "RFS"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "RFS"

rcvFileComplete :: RcvFileStatus -> Bool
rcvFileComplete = \case
  RFSComplete _ -> True
  _ -> False

rcvFileCompleteOrCancelled :: RcvFileTransfer -> Bool
rcvFileCompleteOrCancelled RcvFileTransfer {fileStatus, cancelled} = rcvFileComplete fileStatus || cancelled

data RcvFileInfo = RcvFileInfo
  { filePath :: FilePath,
    connId :: Maybe Int64,
    agentConnId :: Maybe AgentConnId
  }
  deriving (Eq, Show, Generic)

instance ToJSON RcvFileInfo where toEncoding = J.genericToEncoding J.defaultOptions

liveRcvFileTransferInfo :: RcvFileTransfer -> Maybe RcvFileInfo
liveRcvFileTransferInfo RcvFileTransfer {fileStatus} = case fileStatus of
  RFSAccepted fi -> Just fi
  RFSConnected fi -> Just fi
  _ -> Nothing

liveRcvFileTransferConnId :: RcvFileTransfer -> Maybe ConnId
liveRcvFileTransferConnId ft = acId =<< liveRcvFileTransferInfo ft
  where
    acId RcvFileInfo {agentConnId = Just (AgentConnId cId)} = Just cId
    acId _ = Nothing

liveRcvFileTransferPath :: RcvFileTransfer -> Maybe FilePath
liveRcvFileTransferPath ft = fp <$> liveRcvFileTransferInfo ft
  where
    fp RcvFileInfo {filePath} = filePath

newtype AgentConnId = AgentConnId ConnId
  deriving (Eq, Show)

instance StrEncoding AgentConnId where
  strEncode (AgentConnId connId) = strEncode connId
  strDecode s = AgentConnId <$> strDecode s
  strP = AgentConnId <$> strP

instance ToJSON AgentConnId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromField AgentConnId where fromField f = AgentConnId <$> fromField f

instance ToField AgentConnId where toField (AgentConnId m) = toField m

newtype AgentSndFileId = AgentSndFileId ConnId
  deriving (Eq, Show)

instance StrEncoding AgentSndFileId where
  strEncode (AgentSndFileId connId) = strEncode connId
  strDecode s = AgentSndFileId <$> strDecode s
  strP = AgentSndFileId <$> strP

instance ToJSON AgentSndFileId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromField AgentSndFileId where fromField f = AgentSndFileId <$> fromField f

instance ToField AgentSndFileId where toField (AgentSndFileId m) = toField m

newtype AgentRcvFileId = AgentRcvFileId ConnId
  deriving (Eq, Show)

instance StrEncoding AgentRcvFileId where
  strEncode (AgentRcvFileId connId) = strEncode connId
  strDecode s = AgentRcvFileId <$> strDecode s
  strP = AgentRcvFileId <$> strP

instance ToJSON AgentRcvFileId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromField AgentRcvFileId where fromField f = AgentRcvFileId <$> fromField f

instance ToField AgentRcvFileId where toField (AgentRcvFileId m) = toField m

newtype AgentInvId = AgentInvId InvitationId
  deriving (Eq, Show)

instance StrEncoding AgentInvId where
  strEncode (AgentInvId connId) = strEncode connId
  strDecode s = AgentInvId <$> strDecode s
  strP = AgentInvId <$> strP

instance ToJSON AgentInvId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromField AgentInvId where fromField f = AgentInvId <$> fromField f

instance ToField AgentInvId where toField (AgentInvId m) = toField m

data FileTransfer
  = FTSnd
      { fileTransferMeta :: FileTransferMeta,
        sndFileTransfers :: [SndFileTransfer]
      }
  | FTRcv {rcvFileTransfer :: RcvFileTransfer}
  deriving (Show, Generic)

instance ToJSON FileTransfer where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "FT"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "FT"

data FileTransferMeta = FileTransferMeta
  { fileId :: FileTransferId,
    xftpSndFile :: Maybe XFTPSndFile,
    fileName :: String,
    filePath :: String,
    fileSize :: Integer,
    fileInline :: Maybe InlineFileMode,
    chunkSize :: Integer,
    cancelled :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON FileTransferMeta where toEncoding = J.genericToEncoding J.defaultOptions

data XFTPSndFile = XFTPSndFile
  { agentSndFileId :: AgentSndFileId,
    privateSndFileDescr :: Maybe Text,
    agentSndFileDeleted :: Bool,
    cryptoArgs :: Maybe CryptoFileArgs
  }
  deriving (Eq, Show, Generic)

instance ToJSON XFTPSndFile where toEncoding = J.genericToEncoding J.defaultOptions

fileTransferCancelled :: FileTransfer -> Bool
fileTransferCancelled (FTSnd FileTransferMeta {cancelled} _) = cancelled
fileTransferCancelled (FTRcv RcvFileTransfer {cancelled}) = cancelled

-- For XFTP file transfers FSConnected means "uploaded to XFTP relays"
data FileStatus = FSNew | FSAccepted | FSConnected | FSComplete | FSCancelled deriving (Eq, Ord, Show)

instance FromField FileStatus where fromField = fromTextField_ textDecode

instance ToField FileStatus where toField = toField . textEncode

instance ToJSON FileStatus where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance TextEncoding FileStatus where
  textDecode = \case
    "new" -> Just FSNew
    "accepted" -> Just FSAccepted
    "connected" -> Just FSConnected
    "complete" -> Just FSComplete
    "cancelled" -> Just FSCancelled
    _ -> Nothing
  textEncode = \case
    FSNew -> "new"
    FSAccepted -> "accepted"
    FSConnected -> "connected"
    FSComplete -> "complete"
    FSCancelled -> "cancelled"

data RcvChunkStatus = RcvChunkOk | RcvChunkFinal | RcvChunkDuplicate | RcvChunkError
  deriving (Eq, Show)

type ConnReqInvitation = ConnectionRequestUri 'CMInvitation

type ConnReqContact = ConnectionRequestUri 'CMContact

data Connection = Connection
  { connId :: Int64,
    agentConnId :: AgentConnId,
    peerChatVRange :: JVersionRange,
    connLevel :: Int,
    viaContact :: Maybe Int64, -- group member contact ID, if not direct connection
    viaUserContactLink :: Maybe Int64, -- user contact link ID, if connected via "user address"
    viaGroupLink :: Bool, -- whether contact connected via group link
    groupLinkId :: Maybe GroupLinkId,
    customUserProfileId :: Maybe Int64,
    connType :: ConnType,
    connStatus :: ConnStatus,
    localAlias :: Text,
    entityId :: Maybe Int64, -- contact, group member, file ID or user contact ID
    connectionCode :: Maybe SecurityCode,
    authErrCounter :: Int,
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

authErrDisableCount :: Int
authErrDisableCount = 10

connDisabled :: Connection -> Bool
connDisabled Connection {authErrCounter} = authErrCounter >= authErrDisableCount

data SecurityCode = SecurityCode {securityCode :: Text, verifiedAt :: UTCTime}
  deriving (Eq, Show, Generic)

instance ToJSON SecurityCode where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

verificationCode :: ByteString -> Text
verificationCode = T.pack . unwords . chunks 5 . show . os2ip
  where
    chunks _ [] = []
    chunks n xs = let (h, t) = splitAt n xs in h : chunks n t

sameVerificationCode :: Text -> Text -> Bool
sameVerificationCode c1 c2 = noSpaces c1 == noSpaces c2
  where
    noSpaces = T.filter (/= ' ')

aConnId :: Connection -> ConnId
aConnId Connection {agentConnId = AgentConnId cId} = cId

connIncognito :: Connection -> Bool
connIncognito Connection {customUserProfileId} = isJust customUserProfileId

instance ToJSON Connection where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data PendingContactConnection = PendingContactConnection
  { pccConnId :: Int64,
    pccAgentConnId :: AgentConnId,
    pccConnStatus :: ConnStatus,
    viaContactUri :: Bool,
    viaUserContactLink :: Maybe Int64,
    groupLinkId :: Maybe GroupLinkId,
    customUserProfileId :: Maybe Int64,
    connReqInv :: Maybe ConnReqInvitation,
    localAlias :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

aConnId' :: PendingContactConnection -> ConnId
aConnId' PendingContactConnection {pccAgentConnId = AgentConnId cId} = cId

instance ToJSON PendingContactConnection where toEncoding = J.genericToEncoding J.defaultOptions

data ConnStatus
  = -- | connection is created by initiating party with agent NEW command (createConnection)
    ConnNew
  | -- | connection is joined by joining party with agent JOIN command (joinConnection)
    ConnJoined
  | -- | initiating party received CONF notification (to be renamed to REQ)
    ConnRequested
  | -- | initiating party accepted connection with agent LET command (to be renamed to ACPT) (allowConnection)
    ConnAccepted
  | -- | connection can be sent messages to (after joining party received INFO notification)
    ConnSndReady
  | -- | connection is ready for both parties to send and receive messages
    ConnReady
  | -- | connection deleted
    ConnDeleted
  deriving (Eq, Show, Read)

instance FromField ConnStatus where fromField = fromTextField_ textDecode

instance ToField ConnStatus where toField = toField . textEncode

instance ToJSON ConnStatus where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance TextEncoding ConnStatus where
  textDecode = \case
    "new" -> Just ConnNew
    "joined" -> Just ConnJoined
    "requested" -> Just ConnRequested
    "accepted" -> Just ConnAccepted
    "snd-ready" -> Just ConnSndReady
    "ready" -> Just ConnReady
    "deleted" -> Just ConnDeleted
    _ -> Nothing
  textEncode = \case
    ConnNew -> "new"
    ConnJoined -> "joined"
    ConnRequested -> "requested"
    ConnAccepted -> "accepted"
    ConnSndReady -> "snd-ready"
    ConnReady -> "ready"
    ConnDeleted -> "deleted"

data ConnType = ConnContact | ConnMember | ConnSndFile | ConnRcvFile | ConnUserContact
  deriving (Eq, Show)

instance FromField ConnType where fromField = fromTextField_ textDecode

instance ToField ConnType where toField = toField . textEncode

instance ToJSON ConnType where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance TextEncoding ConnType where
  textDecode = \case
    "contact" -> Just ConnContact
    "member" -> Just ConnMember
    "snd_file" -> Just ConnSndFile
    "rcv_file" -> Just ConnRcvFile
    "user_contact" -> Just ConnUserContact
    _ -> Nothing
  textEncode = \case
    ConnContact -> "contact"
    ConnMember -> "member"
    ConnSndFile -> "snd_file"
    ConnRcvFile -> "rcv_file"
    ConnUserContact -> "user_contact"

data NewConnection = NewConnection
  { agentConnId :: ByteString,
    connLevel :: Int,
    viaConn :: Maybe Int64
  }

data GroupMemberIntro = GroupMemberIntro
  { introId :: Int64,
    reMember :: GroupMember,
    toMember :: GroupMember,
    introStatus :: GroupMemberIntroStatus,
    introInvitation :: Maybe IntroInvitation
  }
  deriving (Show)

data GroupMemberIntroStatus
  = GMIntroPending
  | GMIntroSent
  | GMIntroInvReceived
  | GMIntroInvForwarded
  | GMIntroReConnected
  | GMIntroToConnected
  | GMIntroConnected
  deriving (Show)

instance FromField GroupMemberIntroStatus where fromField = fromTextField_ introStatusT

instance ToField GroupMemberIntroStatus where toField = toField . serializeIntroStatus

introStatusT :: Text -> Maybe GroupMemberIntroStatus
introStatusT = \case
  "new" -> Just GMIntroPending
  "sent" -> Just GMIntroSent
  "rcv" -> Just GMIntroInvReceived
  "fwd" -> Just GMIntroInvForwarded
  "re-con" -> Just GMIntroReConnected
  "to-con" -> Just GMIntroToConnected
  "con" -> Just GMIntroConnected
  _ -> Nothing

serializeIntroStatus :: GroupMemberIntroStatus -> Text
serializeIntroStatus = \case
  GMIntroPending -> "new"
  GMIntroSent -> "sent"
  GMIntroInvReceived -> "rcv"
  GMIntroInvForwarded -> "fwd"
  GMIntroReConnected -> "re-con"
  GMIntroToConnected -> "to-con"
  GMIntroConnected -> "con"

data Notification = Notification {title :: Text, text :: Text}

textParseJSON :: TextEncoding a => String -> J.Value -> JT.Parser a
textParseJSON name = J.withText name $ maybe (fail $ "bad " <> name) pure . textDecode

type CommandId = Int64

aCorrId :: CommandId -> ACorrId
aCorrId = pack . show

commandId :: ACorrId -> String
commandId = unpack

data CommandStatus
  = CSCreated
  | CSCompleted -- unused - was replaced with deleteCommand
  | CSError -- internal command error, e.g. not matching connection id or unexpected response, not related to agent message ERR
  deriving (Show, Generic)

instance FromField CommandStatus where fromField = fromTextField_ textDecode

instance ToField CommandStatus where toField = toField . textEncode

instance TextEncoding CommandStatus where
  textDecode = \case
    "created" -> Just CSCreated
    "completed" -> Just CSCompleted
    "error" -> Just CSError
    _ -> Nothing
  textEncode = \case
    CSCreated -> "created"
    CSCompleted -> "completed"
    CSError -> "error"

data CommandFunction
  = CFCreateConnGrpMemInv
  | CFCreateConnGrpInv
  | CFCreateConnFileInvDirect
  | CFCreateConnFileInvGroup
  | CFJoinConn
  | CFAllowConn
  | CFAcceptContact
  | CFAckMessage
  | CFDeleteConn -- not used
  deriving (Eq, Show, Generic)

instance FromField CommandFunction where fromField = fromTextField_ textDecode

instance ToField CommandFunction where toField = toField . textEncode

instance TextEncoding CommandFunction where
  textDecode = \case
    "create_conn" -> Just CFCreateConnGrpMemInv
    "create_conn_grp_inv" -> Just CFCreateConnGrpInv
    "create_conn_file_inv_direct" -> Just CFCreateConnFileInvDirect
    "create_conn_file_inv_group" -> Just CFCreateConnFileInvGroup
    "join_conn" -> Just CFJoinConn
    "allow_conn" -> Just CFAllowConn
    "accept_contact" -> Just CFAcceptContact
    "ack_message" -> Just CFAckMessage
    "delete_conn" -> Just CFDeleteConn
    _ -> Nothing
  textEncode = \case
    CFCreateConnGrpMemInv -> "create_conn"
    CFCreateConnGrpInv -> "create_conn_grp_inv"
    CFCreateConnFileInvDirect -> "create_conn_file_inv_direct"
    CFCreateConnFileInvGroup -> "create_conn_file_inv_group"
    CFJoinConn -> "join_conn"
    CFAllowConn -> "allow_conn"
    CFAcceptContact -> "accept_contact"
    CFAckMessage -> "ack_message"
    CFDeleteConn -> "delete_conn"

commandExpectedResponse :: CommandFunction -> APartyCmdTag 'Agent
commandExpectedResponse = \case
  CFCreateConnGrpMemInv -> t INV_
  CFCreateConnGrpInv -> t INV_
  CFCreateConnFileInvDirect -> t INV_
  CFCreateConnFileInvGroup -> t INV_
  CFJoinConn -> t OK_
  CFAllowConn -> t OK_
  CFAcceptContact -> t OK_
  CFAckMessage -> t OK_
  CFDeleteConn -> t OK_
  where
    t = APCT SAEConn

data CommandData = CommandData
  { cmdId :: CommandId,
    cmdConnId :: Maybe Int64,
    cmdFunction :: CommandFunction,
    cmdStatus :: CommandStatus
  }
  deriving (Show)

-- ad-hoc type for data required for XGrpMemIntro continuation
data XGrpMemIntroCont = XGrpMemIntroCont
  { groupId :: GroupId,
    groupMemberId :: GroupMemberId,
    memberId :: MemberId,
    groupConnReq :: ConnReqInvitation
  }
  deriving (Show)

data ServerCfg p = ServerCfg
  { server :: ProtoServerWithAuth p,
    preset :: Bool,
    tested :: Maybe Bool,
    enabled :: Bool
  }
  deriving (Show, Generic)

instance ProtocolTypeI p => ToJSON (ServerCfg p) where
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}

instance ProtocolTypeI p => FromJSON (ServerCfg p) where
  parseJSON = J.genericParseJSON J.defaultOptions {J.omitNothingFields = True}

newtype ChatVersionRange = ChatVersionRange {fromChatVRange :: VersionRange} deriving (Eq, Show)

chatInitialVRange :: VersionRange
chatInitialVRange = versionToRange 1

instance FromJSON ChatVersionRange where
  parseJSON v = ChatVersionRange <$> strParseJSON "ChatVersionRange" v

instance ToJSON ChatVersionRange where
  toJSON (ChatVersionRange vr) = strToJSON vr
  toEncoding (ChatVersionRange vr) = strToJEncoding vr

newtype JVersionRange = JVersionRange {fromJVersionRange :: VersionRange} deriving (Eq, Show)

instance ToJSON JVersionRange where
  toJSON (JVersionRange (VersionRange minV maxV)) = J.object ["minVersion" .= minV, "maxVersion" .= maxV]
  toEncoding (JVersionRange (VersionRange minV maxV)) = J.pairs $ "minVersion" .= minV <> "maxVersion" .= maxV
