{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Simplex.Chat.Types where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as JT
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Typeable
import Database.SQLite.Simple (ResultError (..), SQLData (..))
import Database.SQLite.Simple.FromField (FieldParser, FromField (..), returnError)
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Generics (Generic)
import Simplex.Messaging.Agent.Protocol (ACommandTag (..), ACorrId, AParty (..), ConnId, ConnectionMode (..), ConnectionRequestUri, InvitationId)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, fromTextField_, sumTypeJSON, taggedObjectJSON)
import Simplex.Messaging.Util (safeDecodeUtf8, (<$?>))

class IsContact a where
  contactId' :: a -> ContactId
  profile' :: a -> LocalProfile
  localDisplayName' :: a -> ContactName

instance IsContact User where
  contactId' = userContactId
  profile' = profile
  localDisplayName' = localDisplayName

instance IsContact Contact where
  contactId' = contactId
  profile' = profile
  localDisplayName' = localDisplayName

data User = User
  { userId :: UserId,
    userContactId :: ContactId,
    localDisplayName :: ContactName,
    profile :: LocalProfile,
    activeUser :: Bool
  }
  deriving (Show, Generic, FromJSON)

instance ToJSON User where toEncoding = J.genericToEncoding J.defaultOptions

type UserId = ContactId

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
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON Contact where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

contactConn :: Contact -> Connection
contactConn = activeConn

contactConnId :: Contact -> ConnId
contactConnId Contact {activeConn} = aConnId activeConn

contactConnIncognito :: Contact -> Bool
contactConnIncognito = isJust . customUserProfileId'

customUserProfileId' :: Contact -> Maybe Int64
customUserProfileId' Contact {activeConn} = customUserProfileId (activeConn :: Connection)

data ContactRef = ContactRef
  { contactId :: ContactId,
    localDisplayName :: ContactName
  }
  deriving (Eq, Show, Generic)

instance ToJSON ContactRef where toEncoding = J.genericToEncoding J.defaultOptions

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
    membership :: GroupMember,
    hostConnCustomUserProfileId :: Maybe ProfileId,
    chatSettings :: ChatSettings,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON GroupInfo where toEncoding = J.genericToEncoding J.defaultOptions

groupName' :: GroupInfo -> GroupName
groupName' GroupInfo {localDisplayName = g} = g

-- TODO when more settings are added we should create another type to allow partial setting updates (with all Maybe properties)
data ChatSettings = ChatSettings
  { enableNtfs :: Bool
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON ChatSettings where toEncoding = J.genericToEncoding J.defaultOptions

defaultChatSettings :: ChatSettings
defaultChatSettings = ChatSettings {enableNtfs = True}

pattern DisableNtfs :: ChatSettings
pattern DisableNtfs = ChatSettings {enableNtfs = False}

data ChatFeature
  = CFFullDelete
  | -- | CFReceipts
    CFVoice

allChatFeatures :: [ChatFeature]
allChatFeatures =
  [ CFFullDelete,
    -- CFReceipts,
    CFVoice
  ]

chatPrefSel :: ChatFeature -> Preferences -> Maybe Preference
chatPrefSel = \case
  CFFullDelete -> fullDelete
  -- CFReceipts -> receipts
  CFVoice -> voice

chatPrefName :: ChatFeature -> Text
chatPrefName = \case
  CFFullDelete -> "full message deletion"
  -- CFReceipts -> "delivery receipts"
  CFVoice -> "voice messages"

class HasPreferences p where
  preferences' :: p -> Maybe Preferences

instance HasPreferences User where
  preferences' User {profile = LocalProfile {preferences}} = preferences
  {-# INLINE preferences' #-}

instance HasPreferences Contact where
  preferences' Contact {profile = LocalProfile {preferences}} = preferences
  {-# INLINE preferences' #-}

class PreferenceI p where
  getPreference :: ChatFeature -> p -> Preference

instance PreferenceI Preferences where
  getPreference pt prefs = fromMaybe (getPreference pt defaultChatPrefs) (chatPrefSel pt prefs)

instance PreferenceI (Maybe Preferences) where
  getPreference pt prefs = fromMaybe (getPreference pt defaultChatPrefs) (chatPrefSel pt =<< prefs)

instance PreferenceI FullPreferences where
  getPreference = \case
    CFFullDelete -> fullDelete
    -- CFReceipts -> receipts
    CFVoice -> voice
  {-# INLINE getPreference #-}

-- collection of optional chat preferences for the user and the contact
data Preferences = Preferences
  { fullDelete :: Maybe Preference,
    -- receipts :: Maybe Preference,
    voice :: Maybe Preference
  }
  deriving (Eq, Show, Generic, FromJSON)

data GroupPreferences = GroupPreferences
  { fullDelete :: Maybe GroupPreference,
    -- receipts :: Maybe GroupPreference,
    voice :: Maybe GroupPreference
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupPreferences where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

instance ToField GroupPreferences where
  toField = toField . encodeJSON

instance FromField GroupPreferences where
  fromField = fromTextField_ decodeJSON

-- full collection of chat preferences defined in the app - it is used to ensure we include all preferences and to simplify processing
-- if some of the preferences are not defined in Preferences, defaults from defaultChatPrefs are used here.
data FullPreferences = FullPreferences
  { fullDelete :: Preference,
    -- receipts :: Preference,
    voice :: Preference
  }
  deriving (Eq)

-- merged preferences of user for a given contact - they differentiate between specific preferences for the contact and global user preferences
data ContactUserPreferences = ContactUserPreferences
  { fullDelete :: ContactUserPreference,
    -- receipts :: ContactUserPreference,
    voice :: ContactUserPreference
  }
  deriving (Show, Generic)

data ContactUserPreference = ContactUserPreference
  { enabled :: PrefEnabled,
    userPreference :: ContactUserPref,
    contactPreference :: Preference
  }
  deriving (Show, Generic)

data ContactUserPref = CUPContact {preference :: Preference} | CUPUser {preference :: Preference}
  deriving (Show, Generic)

instance ToJSON ContactUserPreferences where toEncoding = J.genericToEncoding J.defaultOptions

instance ToJSON ContactUserPreference where toEncoding = J.genericToEncoding J.defaultOptions

instance ToJSON ContactUserPref where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "CUP"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "CUP"

toChatPrefs :: FullPreferences -> Preferences
toChatPrefs FullPreferences {fullDelete, voice} =
  Preferences
    { fullDelete = Just fullDelete,
      -- receipts = Just receipts,
      voice = Just voice
    }

defaultChatPrefs :: FullPreferences
defaultChatPrefs =
  FullPreferences
    { fullDelete = Preference {allow = FANo},
      -- receipts = Preference {allow = FANo},
      voice = Preference {allow = FAYes}
    }

emptyChatPrefs :: Preferences
emptyChatPrefs = Preferences Nothing Nothing

instance ToJSON Preferences where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

instance ToField Preferences where
  toField = toField . encodeJSON

instance FromField Preferences where
  fromField = fromTextField_ decodeJSON

data Preference = Preference
  {allow :: FeatureAllowed}
  deriving (Eq, Show, Generic, FromJSON)

data GroupPreference = GroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON Preference where toEncoding = J.genericToEncoding J.defaultOptions

instance ToJSON GroupPreference where toEncoding = J.genericToEncoding J.defaultOptions

data FeatureAllowed
  = FAAlways -- allow unconditionally
  | FAYes -- allow, if peer allows it
  | FANo -- do not allow
  deriving (Eq, Show, Generic)

data GroupFeatureEnabled = FEOn | FEOff
  deriving (Eq, Show, Generic)

instance FromField FeatureAllowed where fromField = fromBlobField_ strDecode

instance ToField FeatureAllowed where toField = toField . strEncode

instance StrEncoding FeatureAllowed where
  strEncode = \case
    FAAlways -> "always"
    FAYes -> "yes"
    FANo -> "no"
  strDecode = \case
    "always" -> Right FAAlways
    "yes" -> Right FAYes
    "no" -> Right FANo
    r -> Left $ "bad FeatureAllowed " <> B.unpack r
  strP = strDecode <$?> A.takeByteString

instance FromJSON FeatureAllowed where
  parseJSON = strParseJSON "FeatureAllowed"

instance ToJSON FeatureAllowed where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromField GroupFeatureEnabled where fromField = fromBlobField_ strDecode

instance ToField GroupFeatureEnabled where toField = toField . strEncode

instance StrEncoding GroupFeatureEnabled where
  strEncode = \case
    FEOn -> "on"
    FEOff -> "off"
  strDecode = \case
    "on" -> Right FEOn
    "off" -> Right FEOff
    r -> Left $ "bad GroupFeatureEnabled " <> B.unpack r
  strP = strDecode <$?> A.takeByteString

instance FromJSON GroupFeatureEnabled where
  parseJSON = strParseJSON "GroupFeatureEnabled"

instance ToJSON GroupFeatureEnabled where
  toJSON = strToJSON
  toEncoding = strToJEncoding

mergePreferences :: Maybe Preferences -> Maybe Preferences -> FullPreferences
mergePreferences contactPrefs userPreferences =
  FullPreferences
    { fullDelete = pref CFFullDelete,
      -- receipts = pref CFReceipts,
      voice = pref CFVoice
    }
  where
    pref pt =
      let sel = chatPrefSel pt
       in fromMaybe (getPreference pt defaultChatPrefs) $ (contactPrefs >>= sel) <|> (userPreferences >>= sel)

mergeUserChatPrefs :: User -> Contact -> FullPreferences
mergeUserChatPrefs user ct =
  let userPrefs = if contactConnIncognito ct then Nothing else preferences' user
   in mergePreferences (Just $ userPreferences ct) userPrefs

data PrefEnabled = PrefEnabled {forUser :: Bool, forContact :: Bool}
  deriving (Show, Generic)

instance ToJSON PrefEnabled where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

prefEnabled :: Preference -> Preference -> PrefEnabled
prefEnabled Preference {allow = user} Preference {allow = contact} = case (user, contact) of
  (FAAlways, FANo) -> PrefEnabled {forUser = False, forContact = True}
  (FANo, FAAlways) -> PrefEnabled {forUser = True, forContact = False}
  (_, FANo) -> PrefEnabled False False
  (FANo, _) -> PrefEnabled False False
  _ -> PrefEnabled True True

contactUserPreferences :: User -> Contact -> ContactUserPreferences
contactUserPreferences user ct =
  ContactUserPreferences
    { fullDelete = pref CFFullDelete,
      -- receipts = pref CFReceipts,
      voice = pref CFVoice
    }
  where
    pref pt =
      ContactUserPreference
        { enabled = prefEnabled userPref ctPref,
          -- incognito contact cannot have default user preference used
          userPreference = if contactConnIncognito ct then CUPContact ctUserPref else maybe (CUPUser userPref) CUPContact ctUserPref_,
          contactPreference = ctPref
        }
      where
        ctUserPref = getPreference pt $ userPreferences ct
        ctUserPref_ = chatPrefSel pt $ userPreferences ct
        userPref = getPreference pt ctUserPrefs
        ctPref = getPreference pt ctPrefs
    ctUserPrefs = mergeUserChatPrefs user ct
    ctPrefs = mergePreferences (preferences' ct) Nothing

getContactUserPrefefence :: ChatFeature -> ContactUserPreferences -> ContactUserPreference
getContactUserPrefefence = \case
  CFFullDelete -> fullDelete
  -- CFReceipts -> receipts
  CFVoice -> voice

data Profile = Profile
  { displayName :: ContactName,
    fullName :: Text,
    image :: Maybe ImageData,
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
profilesMatch :: Profile -> Profile -> Bool
profilesMatch
  Profile {displayName = n1, fullName = fn1, image = i1}
  Profile {displayName = n2, fullName = fn2, image = i2} =
    n1 == n2 && fn1 == fn2 && i1 == i2

data IncognitoProfile = NewIncognito Profile | ExistingIncognito LocalProfile

type LocalAlias = Text

data LocalProfile = LocalProfile
  { profileId :: ProfileId,
    displayName :: ContactName,
    fullName :: Text,
    image :: Maybe ImageData,
    preferences :: Maybe Preferences,
    localAlias :: LocalAlias
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON LocalProfile where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

localProfileId :: LocalProfile -> ProfileId
localProfileId = profileId

toLocalProfile :: ProfileId -> Profile -> LocalAlias -> LocalProfile
toLocalProfile profileId Profile {displayName, fullName, image, preferences} localAlias =
  LocalProfile {profileId, displayName, fullName, image, preferences, localAlias}

fromLocalProfile :: LocalProfile -> Profile
fromLocalProfile LocalProfile {displayName, fullName, image, preferences} =
  Profile {displayName, fullName, image, preferences}

data GroupProfile = GroupProfile
  { displayName :: GroupName,
    fullName :: Text,
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
    directConnReq :: ConnReqInvitation
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON IntroInvitation where toEncoding = J.genericToEncoding J.defaultOptions

data MemberInfo = MemberInfo
  { memberId :: MemberId,
    memberRole :: GroupMemberRole,
    profile :: Profile
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON MemberInfo where toEncoding = J.genericToEncoding J.defaultOptions

memberInfo :: GroupMember -> MemberInfo
memberInfo GroupMember {memberId, memberRole, memberProfile} =
  MemberInfo memberId memberRole (fromLocalProfile memberProfile)

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
    memberProfile :: LocalProfile,
    memberContactId :: Maybe ContactId,
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
memberConn = activeConn

memberConnId :: GroupMember -> Maybe ConnId
memberConnId GroupMember {activeConn} = aConnId <$> activeConn

groupMemberId' :: GroupMember -> GroupMemberId
groupMemberId' GroupMember {groupMemberId} = groupMemberId

memberIncognito :: GroupMember -> Bool
memberIncognito GroupMember {memberProfile, memberContactProfileId} = localProfileId memberProfile /= memberContactProfileId

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
  = GRAuthor -- can send messages to all group members
  | GRMember -- + add new members with role Member and below
  | GRAdmin -- + change member roles (excl. Owners), add Admins, remove members (excl. Owners)
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
  strDecode = \case
    "owner" -> Right GROwner
    "admin" -> Right GRAdmin
    "member" -> Right GRMember
    "author" -> Right GRAuthor
    r -> Left $ "bad GroupMemberRole " <> B.unpack r
  strP = strDecode <$?> A.takeByteString

instance FromJSON GroupMemberRole where
  parseJSON = strParseJSON "GroupMemberRole"

instance ToJSON GroupMemberRole where
  toJSON = strToJSON
  toEncoding = strToJEncoding

fromBlobField_ :: Typeable k => (ByteString -> Either String k) -> FieldParser k
fromBlobField_ p = \case
  f@(Field (SQLBlob b) _) ->
    case p b of
      Right k -> Ok k
      Left e -> returnError ConversionFailed f ("could not parse field: " ++ e)
  f -> returnError ConversionFailed f "expecting SQLBlob column type"

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
memberCurrent m = case memberStatus m of
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
    fileStatus :: FileStatus,
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
    fileConnReq :: Maybe ConnReqInvitation,
    fileInline :: Maybe InlineFileMode
  }
  deriving (Eq, Show, Generic)

instance ToJSON FileInvitation where
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}

instance FromJSON FileInvitation where
  parseJSON = J.genericParseJSON J.defaultOptions {J.omitNothingFields = True}

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
    fileInvitation :: FileInvitation,
    fileStatus :: RcvFileStatus,
    rcvFileInline :: Maybe InlineFileMode,
    senderDisplayName :: ContactName,
    chunkSize :: Integer,
    cancelled :: Bool,
    grpMemberId :: Maybe Int64
  }
  deriving (Eq, Show, Generic)

instance ToJSON RcvFileTransfer where toEncoding = J.genericToEncoding J.defaultOptions

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

data RcvFileInfo = RcvFileInfo
  { filePath :: FilePath,
    connId :: Int64,
    agentConnId :: AgentConnId
  }
  deriving (Eq, Show, Generic)

instance ToJSON RcvFileInfo where toEncoding = J.genericToEncoding J.defaultOptions

liveRcvFileTransferConnId :: RcvFileTransfer -> Maybe ConnId
liveRcvFileTransferConnId RcvFileTransfer {fileStatus} = case fileStatus of
  RFSAccepted fi -> acId fi
  RFSConnected fi -> acId fi
  _ -> Nothing
  where
    acId RcvFileInfo {agentConnId = AgentConnId cId} = Just cId

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
    fileName :: String,
    filePath :: String,
    fileSize :: Integer,
    fileInline :: Maybe InlineFileMode,
    chunkSize :: Integer,
    cancelled :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON FileTransferMeta where toEncoding = J.genericToEncoding J.defaultOptions

fileTransferCancelled :: FileTransfer -> Bool
fileTransferCancelled (FTSnd FileTransferMeta {cancelled} _) = cancelled
fileTransferCancelled (FTRcv RcvFileTransfer {cancelled}) = cancelled

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
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

aConnId :: Connection -> ConnId
aConnId Connection {agentConnId = AgentConnId cId} = cId

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

type JSONString = String

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
  | CFJoinConn
  | CFAllowConn
  | CFAcceptContact
  | CFAckMessage
  | CFDeleteConn
  deriving (Eq, Show, Generic)

instance FromField CommandFunction where fromField = fromTextField_ textDecode

instance ToField CommandFunction where toField = toField . textEncode

instance TextEncoding CommandFunction where
  textDecode = \case
    "create_conn" -> Just CFCreateConnGrpMemInv
    "create_conn_grp_inv" -> Just CFCreateConnGrpInv
    "join_conn" -> Just CFJoinConn
    "allow_conn" -> Just CFAllowConn
    "accept_contact" -> Just CFAcceptContact
    "ack_message" -> Just CFAckMessage
    "delete_conn" -> Just CFDeleteConn
    _ -> Nothing
  textEncode = \case
    CFCreateConnGrpMemInv -> "create_conn"
    CFCreateConnGrpInv -> "create_conn_grp_inv"
    CFJoinConn -> "join_conn"
    CFAllowConn -> "allow_conn"
    CFAcceptContact -> "accept_contact"
    CFAckMessage -> "ack_message"
    CFDeleteConn -> "delete_conn"

commandExpectedResponse :: CommandFunction -> ACommandTag 'Agent
commandExpectedResponse = \case
  CFCreateConnGrpMemInv -> INV_
  CFCreateConnGrpInv -> INV_
  CFJoinConn -> OK_
  CFAllowConn -> OK_
  CFAcceptContact -> OK_
  CFAckMessage -> OK_
  CFDeleteConn -> OK_

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

encodeJSON :: ToJSON a => a -> Text
encodeJSON = safeDecodeUtf8 . LB.toStrict . J.encode

decodeJSON :: FromJSON a => Text -> Maybe a
decodeJSON = J.decode . LB.fromStrict . encodeUtf8
