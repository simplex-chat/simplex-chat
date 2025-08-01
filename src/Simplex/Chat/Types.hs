{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Simplex.Chat.Types where

import Control.Applicative ((<|>))
import Crypto.Number.Serialize (os2ip)
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as LB
import Data.Functor (($>))
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Simplex.Chat.Types.Preferences
import Simplex.Chat.Types.Shared
import Simplex.Chat.Types.UITheme
import Simplex.Chat.Types.Util
import Simplex.FileTransfer.Description (FileDigest)
import Simplex.FileTransfer.Types (RcvFileId, SndFileId)
import Simplex.Messaging.Agent.Protocol (ACorrId, ACreatedConnLink, AEventTag (..), AEvtTag (..), ConnId, ConnShortLink, ConnectionLink, ConnectionMode (..), ConnectionRequestUri, CreatedConnLink, InvitationId, SAEntity (..), UserId)
import Simplex.Messaging.Agent.Store.DB (Binary (..), blobFieldDecoder, fromTextField_)
import Simplex.Messaging.Crypto.File (CryptoFileArgs (..))
import Simplex.Messaging.Crypto.Ratchet (PQEncryption (..), PQSupport, pattern PQEncOff)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, sumTypeJSON)
import Simplex.Messaging.Util (decodeJSON, encodeJSON, safeDecodeUtf8, (<$?>))
import Simplex.Messaging.Version
import Simplex.Messaging.Version.Internal
#if defined(dbPostgres)
import Database.PostgreSQL.Simple (ResultError (..))
import Database.PostgreSQL.Simple.FromField (FromField(..), FieldParser, returnError)
import Database.PostgreSQL.Simple.ToField (ToField (..))
#else
import Database.SQLite.Simple (ResultError (..))
import Database.SQLite.Simple.FromField (FromField (..), FieldParser, returnError)
import Database.SQLite.Simple.ToField (ToField (..))
#endif

class IsContact a where
  contactId' :: a -> ContactId
  profile' :: a -> LocalProfile
  localDisplayName' :: a -> ContactName
  preferences' :: a -> Maybe Preferences

instance IsContact User where
  contactId' User {userContactId} = userContactId
  {-# INLINE contactId' #-}
  profile' User {profile} = profile
  {-# INLINE profile' #-}
  localDisplayName' User {localDisplayName} = localDisplayName
  {-# INLINE localDisplayName' #-}
  preferences' User {profile = LocalProfile {preferences}} = preferences
  {-# INLINE preferences' #-}

instance IsContact Contact where
  contactId' Contact {contactId} = contactId
  {-# INLINE contactId' #-}
  profile' Contact {profile} = profile
  {-# INLINE profile' #-}
  localDisplayName' Contact {localDisplayName} = localDisplayName
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

deriving newtype instance FromField AgentUserId

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
    activeOrder :: Int64,
    viewPwdHash :: Maybe UserPwdHash,
    showNtfs :: Bool,
    sendRcptsContacts :: Bool,
    sendRcptsSmallGroups :: Bool,
    userMemberProfileUpdatedAt :: Maybe UTCTime,
    uiThemes :: Maybe UIThemeEntityOverrides
  }
  deriving (Show)

data NewUser = NewUser
  { profile :: Maybe Profile,
    pastTimestamp :: Bool
  }
  deriving (Show)

newtype B64UrlByteString = B64UrlByteString ByteString
  deriving (Eq, Show)
  deriving newtype (FromField)

instance ToField B64UrlByteString where toField (B64UrlByteString m) = toField $ Binary m

instance StrEncoding B64UrlByteString where
  strEncode (B64UrlByteString m) = strEncode m
  strP = B64UrlByteString <$> strP

instance FromJSON B64UrlByteString where
  parseJSON = strParseJSON "B64UrlByteString"

instance ToJSON B64UrlByteString where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data UserPwdHash = UserPwdHash {hash :: B64UrlByteString, salt :: B64UrlByteString}
  deriving (Eq, Show)

data UserInfo = UserInfo
  { user :: User,
    unreadCount :: Int
  }
  deriving (Show)

type ContactId = Int64

type ProfileId = Int64

type ChatTagId = Int64

data Contact = Contact
  { contactId :: ContactId,
    localDisplayName :: ContactName,
    profile :: LocalProfile,
    activeConn :: Maybe Connection,
    viaGroup :: Maybe Int64,
    contactUsed :: Bool,
    contactStatus :: ContactStatus,
    chatSettings :: ChatSettings,
    userPreferences :: Preferences,
    mergedPreferences :: ContactUserPreferences,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    chatTs :: Maybe UTCTime,
    preparedContact :: Maybe PreparedContact,
    contactRequestId :: Maybe Int64,
    contactGroupMemberId :: Maybe GroupMemberId,
    contactGrpInvSent :: Bool,
    chatTags :: [ChatTagId],
    chatItemTTL :: Maybe Int64,
    uiThemes :: Maybe UIThemeEntityOverrides,
    chatDeleted :: Bool,
    customData :: Maybe CustomData
  }
  deriving (Eq, Show)

contactRequestId' :: Contact -> Maybe Int64
contactRequestId' Contact {contactRequestId} = contactRequestId

data PreparedContact = PreparedContact
  { connLinkToConnect :: ACreatedConnLink,
    uiConnLinkType :: ConnectionMode,
    welcomeSharedMsgId :: Maybe SharedMsgId,
    requestSharedMsgId :: Maybe SharedMsgId
  }
  deriving (Eq, Show)

newtype SharedMsgId = SharedMsgId ByteString
  deriving (Eq, Show)
  deriving newtype (FromField)

instance ToField SharedMsgId where toField (SharedMsgId m) = toField $ Binary m

instance StrEncoding SharedMsgId where
  strEncode (SharedMsgId m) = strEncode m
  strDecode s = SharedMsgId <$> strDecode s
  strP = SharedMsgId <$> strP

instance FromJSON SharedMsgId where
  parseJSON = strParseJSON "SharedMsgId"

instance ToJSON SharedMsgId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

newtype CustomData = CustomData J.Object
  deriving (Eq, Show)

instance ToJSON CustomData where
  toJSON (CustomData v) = toJSON v
  toEncoding (CustomData v) = toEncoding v

instance FromJSON CustomData where
  parseJSON = J.withObject "CustomData" (pure . CustomData)

instance ToField CustomData where toField (CustomData v) = toField . Binary . LB.toStrict $ J.encode v

instance FromField CustomData where fromField = blobFieldDecoder J.eitherDecodeStrict

contactConn :: Contact -> Maybe Connection
contactConn Contact {activeConn} = activeConn

contactConnId :: Contact -> Maybe ConnId
contactConnId c = aConnId <$> contactConn c

type IncognitoEnabled = Bool

contactConnIncognito :: Contact -> IncognitoEnabled
contactConnIncognito = maybe False connIncognito . contactConn

contactDirect :: Contact -> Bool
contactDirect Contact {activeConn} = maybe True connDirect activeConn

connDirect :: Connection -> Bool
connDirect Connection {connLevel, viaGroupLink} = connLevel == 0 && not viaGroupLink

directOrUsed :: Contact -> Bool
directOrUsed ct@Contact {contactUsed} =
  contactDirect ct || contactUsed

anyDirectOrUsed :: Contact -> Bool
anyDirectOrUsed Contact {contactUsed, activeConn} = ((\Connection {connLevel} -> connLevel) <$> activeConn) == Just 0 || contactUsed

contactReady :: Contact -> Bool
contactReady Contact {activeConn} = maybe False connReady activeConn

contactActive :: Contact -> Bool
contactActive Contact {contactStatus} = contactStatus == CSActive

contactDeleted :: Contact -> Bool
contactDeleted Contact {contactStatus} = contactStatus == CSDeleted || contactStatus == CSDeletedByUser

contactSecurityCode :: Contact -> Maybe SecurityCode
contactSecurityCode Contact {activeConn} = connectionCode =<< activeConn

contactPQEnabled :: Contact -> PQEncryption
contactPQEnabled Contact {activeConn} = maybe PQEncOff connPQEnabled activeConn

data ContactStatus
  = CSActive
  | CSDeleted
  | CSDeletedByUser
  deriving (Eq, Show, Ord)

instance FromField ContactStatus where fromField = fromTextField_ textDecode

instance ToField ContactStatus where toField = toField . textEncode

instance FromJSON ContactStatus where
  parseJSON = textParseJSON "ContactStatus"

instance ToJSON ContactStatus where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance TextEncoding ContactStatus where
  textDecode = \case
    "active" -> Just CSActive
    "deleted" -> Just CSDeleted
    "deletedByUser" -> Just CSDeletedByUser
    _ -> Nothing
  textEncode = \case
    CSActive -> "active"
    CSDeleted -> "deleted"
    CSDeletedByUser -> "deletedByUser"

data ContactRef = ContactRef
  { contactId :: ContactId,
    connId :: Int64,
    agentConnId :: AgentConnId,
    localDisplayName :: ContactName
  }
  deriving (Eq, Show)

data ContactOrMember = COMContact Contact | COMGroupMember GroupMember
  deriving (Show)

contactOrMemberIds :: ContactOrMember -> (Maybe ContactId, Maybe GroupMemberId)
contactOrMemberIds = \case
  COMContact Contact {contactId} -> (Just contactId, Nothing)
  COMGroupMember GroupMember {groupMemberId} -> (Nothing, Just groupMemberId)

contactOrMemberIncognito :: ContactOrMember -> IncognitoEnabled
contactOrMemberIncognito = \case
  COMContact ct -> contactConnIncognito ct
  COMGroupMember m -> memberIncognito m

data UserContact = UserContact
  { userContactLinkId :: Int64,
    connReqContact :: ConnReqContact,
    groupId :: Maybe GroupId
  }
  deriving (Eq, Show)

userContactGroupId :: UserContact -> Maybe GroupId
userContactGroupId UserContact {groupId} = groupId

data UserContactRequest = UserContactRequest
  { contactRequestId :: Int64,
    agentInvitationId :: AgentInvId,
    contactId_ :: Maybe ContactId,
    businessGroupId_ :: Maybe GroupId,
    userContactLinkId_ :: Maybe Int64,
    cReqChatVRange :: VersionRangeChat,
    localDisplayName :: ContactName,
    profileId :: Int64,
    profile :: Profile,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    xContactId :: Maybe XContactId,
    pqSupport :: PQSupport,
    welcomeSharedMsgId :: Maybe SharedMsgId,
    requestSharedMsgId :: Maybe SharedMsgId
  }
  deriving (Eq, Show)

newtype XContactId = XContactId ByteString
  deriving (Eq, Show)
  deriving newtype (FromField)

instance ToField XContactId where toField (XContactId m) = toField $ Binary m

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
  deriving newtype (FromField)

instance ToField ConnReqUriHash where toField (ConnReqUriHash m) = toField $ Binary m

instance StrEncoding ConnReqUriHash where
  strEncode (ConnReqUriHash m) = strEncode m
  strDecode s = ConnReqUriHash <$> strDecode s
  strP = ConnReqUriHash <$> strP

instance FromJSON ConnReqUriHash where
  parseJSON = strParseJSON "ConnReqUriHash"

instance ToJSON ConnReqUriHash where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data RequestEntity
  = REContact Contact
  | REBusinessChat GroupInfo GroupMember

type RepeatRequest = Bool

data RequestStage
  = RSAcceptedRequest
      { acceptedRequest :: Maybe UserContactRequest, -- Request is optional to support deleted legacy requests
        requestEntity :: RequestEntity
      }
  | RSCurrentRequest
      { previousRequest :: Maybe UserContactRequest,
        currentRequest :: UserContactRequest,
        requestEntity_ :: Maybe RequestEntity -- Entity is optional to support legacy requests without entity
      }

type UserName = Text

type ContactName = Text

type MemberName = Text

type GroupName = Text

optionalFullName :: ContactName -> Text -> Maybe Text -> Text
optionalFullName displayName fullName shortDescr
  | T.null fullName || displayName == fullName = maybe "" (\sd -> " (" <> sd <> ")") shortDescr
  | otherwise = " (" <> fullName <> ")"

data ShortGroup = ShortGroup
  { shortInfo :: ShortGroupInfo,
    members :: [ShortGroupMember]
  }

data ShortGroupInfo = ShortGroupInfo
  { groupId :: GroupId,
    groupName :: GroupName,
    membershipStatus :: GroupMemberStatus
  }
  deriving (Eq, Show)

data ShortGroupMember = ShortGroupMember
  { groupMemberId :: GroupMemberId,
    groupId :: GroupId,
    memberName :: ContactName,
    connId :: AgentConnId
  }
  deriving (Show)

data Group = Group {groupInfo :: GroupInfo, members :: [GroupMember]}
  deriving (Eq, Show)

type GroupId = Int64

data GroupInfo = GroupInfo
  { groupId :: GroupId,
    localDisplayName :: GroupName,
    groupProfile :: GroupProfile,
    localAlias :: Text,
    businessChat :: Maybe BusinessChatInfo,
    fullGroupPreferences :: FullGroupPreferences,
    membership :: GroupMember,
    chatSettings :: ChatSettings,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    chatTs :: Maybe UTCTime,
    userMemberProfileSentAt :: Maybe UTCTime,
    preparedGroup :: Maybe PreparedGroup,
    chatTags :: [ChatTagId],
    chatItemTTL :: Maybe Int64,
    uiThemes :: Maybe UIThemeEntityOverrides,
    customData :: Maybe CustomData,
    membersRequireAttention :: Int
  }
  deriving (Eq, Show)

data BusinessChatType
  = BCBusiness -- used on the customer side
  | BCCustomer -- used on the business side
  deriving (Eq, Show)

instance TextEncoding BusinessChatType where
  textEncode = \case
    BCBusiness -> "business"
    BCCustomer -> "customer"
  textDecode = \case
    "business" -> Just BCBusiness
    "customer" -> Just BCCustomer
    _ -> Nothing

instance FromField BusinessChatType where fromField = fromTextField_ textDecode

instance ToField BusinessChatType where toField = toField . textEncode

data PreparedGroup = PreparedGroup
  { connLinkToConnect :: CreatedLinkContact,
    connLinkPreparedConnection :: Bool,
    connLinkStartedConnection :: Bool,
    welcomeSharedMsgId :: Maybe SharedMsgId, -- it is stored only for business chats, and only if welcome message is specified
    requestSharedMsgId :: Maybe SharedMsgId
  }
  deriving (Eq, Show)

groupName' :: GroupInfo -> GroupName
groupName' GroupInfo {localDisplayName = g} = g

data GroupSummary = GroupSummary
  { currentMembers :: Int
  }
  deriving (Show)

data GroupInfoSummary = GIS {groupInfo :: GroupInfo, groupSummary :: GroupSummary}
  deriving (Show)

data ContactOrGroup = CGContact Contact | CGGroup GroupInfo [GroupMember]

data PreparedChatEntity = PCEContact Contact | PCEGroup {groupInfo :: GroupInfo, hostMember :: GroupMember}

contactAndGroupIds :: ContactOrGroup -> (Maybe ContactId, Maybe GroupId)
contactAndGroupIds = \case
  CGContact Contact {contactId} -> (Just contactId, Nothing)
  CGGroup GroupInfo {groupId} _ -> (Nothing, Just groupId)

-- TODO when more settings are added we should create another type to allow partial setting updates (with all Maybe properties)
data ChatSettings = ChatSettings
  { enableNtfs :: MsgFilter,
    sendRcpts :: Maybe Bool,
    favorite :: Bool
  }
  deriving (Eq, Show)

defaultChatSettings :: ChatSettings
defaultChatSettings =
  ChatSettings
    { enableNtfs = MFAll,
      sendRcpts = Nothing,
      favorite = False
    }

chatHasNtfs :: ChatSettings -> Bool
chatHasNtfs ChatSettings {enableNtfs} = enableNtfs /= MFNone

data MsgFilter = MFNone | MFAll | MFMentions
  deriving (Eq, Show)

msgFilterInt :: MsgFilter -> Int
msgFilterInt = \case
  MFNone -> 0
  MFAll -> 1
  MFMentions -> 2

msgFilterIntP :: Int64 -> Maybe MsgFilter
msgFilterIntP = \case
  0 -> Just MFNone
  1 -> Just MFAll
  2 -> Just MFMentions
  _ -> Just MFAll

fromIntField_ :: Typeable a => (Int64 -> Maybe a) -> FieldParser a
#if defined(dbPostgres)
fromIntField_ fromInt f val = fromField f val >>= parseInt
#else
fromIntField_ fromInt f = fromField f >>= parseInt
#endif
  where
    parseInt i = case fromInt i of
      Just x -> pure x
      _ -> returnError ConversionFailed f $ "invalid integer: " <> show i

featureAllowed :: SChatFeature f -> (PrefEnabled -> Bool) -> Contact -> Bool
featureAllowed feature forWhom Contact {mergedPreferences} =
  let ContactUserPreference {enabled} = getContactUserPreference feature mergedPreferences
   in forWhom enabled

groupFeatureAllowed :: GroupFeatureNoRoleI f => SGroupFeature f -> GroupInfo -> Bool
groupFeatureAllowed feature gInfo = groupFeatureAllowed' feature $ fullGroupPreferences gInfo

groupFeatureMemberAllowed :: GroupFeatureRoleI f => SGroupFeature f -> GroupMember -> GroupInfo -> Bool
groupFeatureMemberAllowed feature GroupMember {memberRole} =
  groupFeatureMemberAllowed' feature memberRole . fullGroupPreferences

mergeUserChatPrefs :: User -> Contact -> FullPreferences
mergeUserChatPrefs user ct = mergeUserChatPrefs' user (contactConnIncognito ct) (userPreferences ct)

mergeUserChatPrefs' :: User -> Bool -> Preferences -> FullPreferences
mergeUserChatPrefs' user connectedIncognito userPreferences =
  let userPrefs = if connectedIncognito then Nothing else preferences' user
   in mergePreferences (Just userPreferences) userPrefs False

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
        ctPref = getPreference f contactPreferences
    ctUserPrefs = mergeUserChatPrefs' user connectedIncognito userPreferences

data Profile = Profile
  { displayName :: ContactName,
    fullName :: Text,
    shortDescr :: Maybe Text, -- short description limited to 160 characters
    image :: Maybe ImageData,
    contactLink :: Maybe ConnLinkContact,
    preferences :: Maybe Preferences
    -- fields that should not be read into this data type to prevent sending them as part of profile to contacts:
    -- - contact_profile_id
    -- - incognito
    -- - local_alias
  }
  deriving (Eq, Show)

profileFromName :: ContactName -> Profile
profileFromName displayName =
  Profile {displayName, fullName = "", shortDescr = Nothing, image = Nothing, contactLink = Nothing, preferences = Nothing}

-- check if profiles match ignoring preferences
profilesMatch :: LocalProfile -> LocalProfile -> Bool
profilesMatch
  LocalProfile {displayName = n1, fullName = fn1, image = i1}
  LocalProfile {displayName = n2, fullName = fn2, image = i2} =
    n1 == n2 && fn1 == fn2 && i1 == i2

redactedMemberProfile :: Profile -> Profile
redactedMemberProfile Profile {displayName, fullName, shortDescr, image} =
  Profile {displayName, fullName, shortDescr, image, contactLink = Nothing, preferences = Nothing}

data IncognitoProfile = NewIncognito Profile | ExistingIncognito LocalProfile

fromIncognitoProfile :: IncognitoProfile -> Profile
fromIncognitoProfile = \case
  NewIncognito p -> p
  ExistingIncognito lp -> fromLocalProfile lp

userProfileInGroup :: User -> Maybe Profile -> Profile
userProfileInGroup User {profile = p} incognitoProfile =
  let p' = fromMaybe (fromLocalProfile p) incognitoProfile
   in redactedMemberProfile p'

userProfileDirect :: User -> Maybe Profile -> Maybe Contact -> Bool -> Profile
userProfileDirect user@User {profile = p} incognitoProfile ct canFallbackToUserTTL =
  let p' = fromMaybe (fromLocalProfile p) incognitoProfile
      fullPrefs = mergePreferences (userPreferences <$> ct) userPrefs canFallbackToUserTTL
   in (p' :: Profile) {preferences = Just $ toChatPrefs fullPrefs}
  where
    userPrefs
      | isNothing incognitoProfile = preferences' user
      | otherwise = -- supplement user level TTL to incognito (default) preferences so that it can serve as fallback
          let FullPreferences {timedMessages = TimedMessagesPreference {allow}} = defaultChatPrefs
              userLevelTTL = preferences' user >>= chatPrefSel SCFTimedMessages >>= (\TimedMessagesPreference {ttl} -> ttl)
           in Just $ toChatPrefs (defaultChatPrefs :: FullPreferences) {timedMessages = TimedMessagesPreference {allow, ttl = userLevelTTL}}

type LocalAlias = Text

data LocalProfile = LocalProfile
  { profileId :: ProfileId,
    displayName :: ContactName,
    fullName :: Text,
    shortDescr :: Maybe Text,
    image :: Maybe ImageData,
    contactLink :: Maybe ConnLinkContact,
    preferences :: Maybe Preferences,
    localAlias :: LocalAlias
  }
  deriving (Eq, Show)

localProfileId :: LocalProfile -> ProfileId
localProfileId LocalProfile {profileId} = profileId

toLocalProfile :: ProfileId -> Profile -> LocalAlias -> LocalProfile
toLocalProfile profileId Profile {displayName, fullName, shortDescr, image, contactLink, preferences} localAlias =
  LocalProfile {profileId, displayName, fullName, shortDescr, image, contactLink, preferences, localAlias}

fromLocalProfile :: LocalProfile -> Profile
fromLocalProfile LocalProfile {displayName, fullName, shortDescr, image, contactLink, preferences} =
  Profile {displayName, fullName, shortDescr, image, contactLink, preferences}

data GroupProfile = GroupProfile
  { displayName :: GroupName,
    fullName :: Text,
    shortDescr :: Maybe Text, -- short description limited to 160 characters
    description :: Maybe Text, -- this has been repurposed as welcome message
    image :: Maybe ImageData,
    groupPreferences :: Maybe GroupPreferences,
    memberAdmission :: Maybe GroupMemberAdmission
  }
  deriving (Eq, Show)

data GroupMemberAdmission = GroupMemberAdmission
  { -- names :: Maybe MemberCriteria,
    -- captcha :: Maybe MemberCriteria,
    review :: Maybe MemberCriteria
  }
  deriving (Eq, Show)

data MemberCriteria = MCAll
  deriving (Eq, Show)

emptyGroupMemberAdmission :: GroupMemberAdmission
emptyGroupMemberAdmission = GroupMemberAdmission Nothing

newtype ImageData = ImageData Text
  deriving (Eq, Show)

instance FromJSON ImageData where
  parseJSON = fmap ImageData . J.parseJSON

instance ToJSON ImageData where
  toJSON (ImageData t) = J.toJSON t
  toEncoding (ImageData t) = J.toEncoding t

instance ToField ImageData where toField (ImageData t) = toField t

deriving newtype instance FromField ImageData

data CReqClientData = CRDataGroup {groupLinkId :: GroupLinkId}

newtype GroupLinkId = GroupLinkId {unGroupLinkId :: ByteString} -- used to identify invitation via group link
  deriving (Eq, Show)
  deriving newtype (FromField)

instance ToField GroupLinkId where toField (GroupLinkId g) = toField $ Binary g

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
    business :: Maybe BusinessChatInfo,
    groupLinkId :: Maybe GroupLinkId,
    groupSize :: Maybe Int
  }
  deriving (Eq, Show)

data GroupLinkInvitation = GroupLinkInvitation
  { fromMember :: MemberIdRole,
    fromMemberName :: ContactName,
    invitedMember :: MemberIdRole,
    groupProfile :: GroupProfile,
    accepted :: Maybe GroupAcceptance,
    business :: Maybe BusinessChatInfo,
    groupSize :: Maybe Int
  }
  deriving (Eq, Show)

data GroupLinkRejection = GroupLinkRejection
  { fromMember :: MemberIdRole,
    invitedMember :: MemberIdRole,
    groupProfile :: GroupProfile,
    rejectionReason :: GroupRejectionReason
  }
  deriving (Eq, Show)

data GroupRejectionReason
  = GRRLongName
  | GRRBlockedName
  | GRRUnknown {text :: Text}
  deriving (Eq, Show)

instance FromField GroupRejectionReason where fromField = blobFieldDecoder strDecode

instance ToField GroupRejectionReason where toField = toField . strEncode

instance StrEncoding GroupRejectionReason where
  strEncode = \case
    GRRLongName -> "long_name"
    GRRBlockedName -> "blocked_name"
    GRRUnknown text -> encodeUtf8 text
  strP =
    "long_name" $> GRRLongName
    <|> "blocked_name" $> GRRBlockedName
    <|> GRRUnknown . safeDecodeUtf8 <$> A.takeByteString

instance FromJSON GroupRejectionReason where
  parseJSON = strParseJSON "GroupRejectionReason"

instance ToJSON GroupRejectionReason where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data MemberIdRole = MemberIdRole
  { memberId :: MemberId,
    memberRole :: GroupMemberRole
  }
  deriving (Eq, Show)

data IntroInvitation = IntroInvitation
  { groupConnReq :: ConnReqInvitation,
    directConnReq :: Maybe ConnReqInvitation
  }
  deriving (Eq, Show)

data MemberInfo = MemberInfo
  { memberId :: MemberId,
    memberRole :: GroupMemberRole,
    v :: Maybe ChatVersionRange,
    profile :: Profile
  }
  deriving (Eq, Show)

data BusinessChatInfo = BusinessChatInfo
  { chatType :: BusinessChatType,
    businessId :: MemberId,
    customerId :: MemberId
  }
  deriving (Eq, Show)

memberInfo :: GroupMember -> MemberInfo
memberInfo GroupMember {memberId, memberRole, memberProfile, activeConn} =
  MemberInfo
    { memberId,
      memberRole,
      v = ChatVersionRange . peerChatVRange <$> activeConn,
      profile = redactedMemberProfile $ fromLocalProfile memberProfile
    }

data MemberRestrictionStatus
  = MRSBlocked
  | MRSUnrestricted
  | MRSUnknown Text
  deriving (Eq, Show)

instance FromField MemberRestrictionStatus where fromField = blobFieldDecoder strDecode

instance ToField MemberRestrictionStatus where toField = toField . strEncode

instance StrEncoding MemberRestrictionStatus where
  strEncode = \case
    MRSBlocked -> "blocked"
    MRSUnrestricted -> "unrestricted"
    MRSUnknown tag -> encodeUtf8 tag
  strDecode s = Right $ case s of
    "blocked" -> MRSBlocked
    "unrestricted" -> MRSUnrestricted
    tag -> MRSUnknown $ safeDecodeUtf8 tag
  strP = strDecode <$?> A.takeByteString

instance FromJSON MemberRestrictionStatus where
  parseJSON = strParseJSON "MemberRestrictionStatus"

instance ToJSON MemberRestrictionStatus where
  toJSON = strToJSON
  toEncoding = strToJEncoding

mrsBlocked :: MemberRestrictionStatus -> Bool
mrsBlocked = \case
  MRSBlocked -> True
  _ -> False

data MemberRestrictions = MemberRestrictions
  { restriction :: MemberRestrictionStatus
  }
  deriving (Eq, Show)

memberRestrictions :: GroupMember -> Maybe MemberRestrictions
memberRestrictions m
  | blockedByAdmin m = Just MemberRestrictions {restriction = MRSBlocked}
  | otherwise = Nothing

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
    memberSettings :: GroupMemberSettings,
    blockedByAdmin :: Bool,
    invitedBy :: InvitedBy,
    invitedByGroupMemberId :: Maybe GroupMemberId,
    localDisplayName :: ContactName,
    -- for membership, memberProfile can be either user's profile or incognito profile, based on memberIncognito test.
    -- for other members it's whatever profile the local user can see (there is no info about whether it's main or incognito profile for remote users).
    memberProfile :: LocalProfile,
    -- this is the ID of the associated contact (it will be used to send direct messages to the member)
    memberContactId :: Maybe ContactId,
    -- for membership it would always point to user's contact
    -- it is used to test for incognito status by comparing with ID in memberProfile
    memberContactProfileId :: ProfileId,
    activeConn :: Maybe Connection,
    -- member chat protocol version range; if member has active connection, its version range is preferred;
    -- for membership current supportedChatVRange is set, it's not updated on protocol version increase in database,
    -- but it's correctly set on read (see toGroupInfo)
    memberChatVRange :: VersionRangeChat,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    supportChat :: Maybe GroupSupportChat
  }
  deriving (Eq, Show)

data GroupSupportChat = GroupSupportChat
  { chatTs :: UTCTime,
    unread :: Int64,
    memberAttention :: Int64,
    mentions :: Int64,
    lastMsgFromMemberTs :: Maybe UTCTime
  }
  deriving (Eq, Show)

gmRequiresAttention :: GroupMember -> Bool
gmRequiresAttention m@GroupMember {supportChat} =
  memberPending m || maybe False supportChatAttention supportChat
  where
    supportChatAttention GroupSupportChat {memberAttention, mentions} =
      memberAttention > 0 || mentions > 0

data GroupMemberRef = GroupMemberRef {groupMemberId :: Int64, profile :: Profile}
  deriving (Eq, Show)

groupMemberRef :: GroupMember -> GroupMemberRef
groupMemberRef GroupMember {groupMemberId, memberProfile = p} =
  GroupMemberRef {groupMemberId, profile = fromLocalProfile p}

memberConn :: GroupMember -> Maybe Connection
memberConn GroupMember {activeConn} = activeConn

memberConnId :: GroupMember -> Maybe ConnId
memberConnId GroupMember {activeConn} = aConnId <$> activeConn

sameMemberId :: MemberId -> GroupMember -> Bool
sameMemberId memId GroupMember {memberId} = memId == memberId

memberChatVRange' :: GroupMember -> VersionRangeChat
memberChatVRange' GroupMember {activeConn, memberChatVRange} = case activeConn of
  Just Connection {peerChatVRange} -> peerChatVRange
  Nothing -> memberChatVRange

supportsVersion :: GroupMember -> VersionChat -> Bool
supportsVersion m v = maxVersion (memberChatVRange' m) >= v

groupMemberId' :: GroupMember -> GroupMemberId
groupMemberId' GroupMember {groupMemberId} = groupMemberId

memberId' :: GroupMember -> MemberId
memberId' GroupMember {memberId} = memberId

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
    memRestriction :: Maybe MemberRestrictionStatus,
    memInvitedBy :: InvitedBy,
    memInvitedByGroupMemberId :: Maybe GroupMemberId,
    localDisplayName :: ContactName,
    memProfileId :: Int64,
    memContactId :: Maybe Int64
  }

newtype MemberId = MemberId {unMemberId :: ByteString}
  deriving (Eq, Ord, Show)
  deriving newtype (FromField)

instance ToField MemberId where toField (MemberId m) = toField $ Binary m

instance StrEncoding MemberId where
  strEncode (MemberId m) = strEncode m
  strDecode s = MemberId <$> strDecode s
  strP = MemberId <$> strP

instance FromJSON MemberId where
  parseJSON = strParseJSON "MemberId"

instance ToJSON MemberId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

nameFromMemberId :: MemberId -> ContactName
nameFromMemberId = T.take 7 . safeDecodeUtf8 . B64.encode . unMemberId

data InvitedBy = IBContact {byContactId :: Int64} | IBUser | IBUnknown
  deriving (Eq, Show)

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

data GroupMemberSettings = GroupMemberSettings
  { showMessages :: Bool
  }
  deriving (Eq, Show)

defaultMemberSettings :: GroupMemberSettings
defaultMemberSettings = GroupMemberSettings {showMessages = True}

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

instance FromJSON GroupMemberCategory where
  parseJSON = textParseJSON "GroupMemberCategory"

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
  = GSMemRejected -- joining member who was rejected by the host, or host that rejected the join
  | GSMemRemoved -- member who was removed from the group
  | GSMemLeft -- member who left the group
  | GSMemGroupDeleted -- user member of the deleted group
  | GSMemUnknown -- unknown member, whose message was forwarded by an admin (likely member wasn't introduced due to not being a current member, but message was included in history)
  | GSMemInvited -- member is sent to or received invitation to join the group
  | GSMemPendingApproval -- member is connected to host but pending host approval before connecting to other members ("knocking")
  | GSMemPendingReview -- member is introduced to admins but pending admin review before connecting to other members ("knocking")
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

instance FromJSON GroupMemberStatus where
  parseJSON = textParseJSON "GroupMemberStatus"

instance ToJSON GroupMemberStatus where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

acceptanceToStatus :: Maybe GroupMemberAdmission -> GroupAcceptance -> GroupMemberStatus
acceptanceToStatus memberAdmission groupAcceptance
  | groupAcceptance == GAPendingApproval = GSMemPendingApproval
  | groupAcceptance == GAPendingReview = GSMemPendingReview
  | (memberAdmission >>= review) == Just MCAll = GSMemPendingReview
  | otherwise = GSMemAccepted

memberActive :: GroupMember -> Bool
memberActive m = case memberStatus m of
  GSMemRejected -> False
  GSMemRemoved -> False
  GSMemLeft -> False
  GSMemGroupDeleted -> False
  GSMemUnknown -> False
  GSMemInvited -> False
  GSMemPendingApproval -> True
  GSMemPendingReview -> True
  GSMemIntroduced -> False
  GSMemIntroInvited -> False
  GSMemAccepted -> False
  GSMemAnnounced -> False
  GSMemConnected -> True
  GSMemComplete -> True
  GSMemCreator -> True

memberCurrent :: GroupMember -> Bool
memberCurrent = memberCurrent' . memberStatus

memberPending :: GroupMember -> Bool
memberPending m = case memberStatus m of
  GSMemPendingApproval -> True
  GSMemPendingReview -> True
  _ -> False

memberCurrentOrPending :: GroupMember -> Bool
memberCurrentOrPending m = memberCurrent m || memberPending m

-- update getGroupSummary if this is changed
memberCurrent' :: GroupMemberStatus -> Bool
memberCurrent' = \case
  GSMemRejected -> False
  GSMemRemoved -> False
  GSMemLeft -> False
  GSMemGroupDeleted -> False
  GSMemUnknown -> False
  GSMemInvited -> False
  GSMemPendingApproval -> False
  GSMemPendingReview -> False
  GSMemIntroduced -> True
  GSMemIntroInvited -> True
  GSMemAccepted -> True
  GSMemAnnounced -> True
  GSMemConnected -> True
  GSMemComplete -> True
  GSMemCreator -> True

memberRemoved :: GroupMember -> Bool
memberRemoved m = case memberStatus m of
  GSMemRejected -> True
  GSMemRemoved -> True
  GSMemLeft -> True
  GSMemGroupDeleted -> True
  GSMemUnknown -> False
  GSMemInvited -> False
  GSMemPendingApproval -> False
  GSMemPendingReview -> False
  GSMemIntroduced -> False
  GSMemIntroInvited -> False
  GSMemAccepted -> False
  GSMemAnnounced -> False
  GSMemConnected -> False
  GSMemComplete -> False
  GSMemCreator -> False

instance TextEncoding GroupMemberStatus where
  textDecode = \case
    "rejected" -> Just GSMemRejected
    "removed" -> Just GSMemRemoved
    "left" -> Just GSMemLeft
    "deleted" -> Just GSMemGroupDeleted
    "unknown" -> Just GSMemUnknown
    "invited" -> Just GSMemInvited
    "pending_approval" -> Just GSMemPendingApproval
    "pending_review" -> Just GSMemPendingReview
    "introduced" -> Just GSMemIntroduced
    "intro-inv" -> Just GSMemIntroInvited
    "accepted" -> Just GSMemAccepted
    "announced" -> Just GSMemAnnounced
    "connected" -> Just GSMemConnected
    "complete" -> Just GSMemComplete
    "creator" -> Just GSMemCreator
    _ -> Nothing
  textEncode = \case
    GSMemRejected -> "rejected"
    GSMemRemoved -> "removed"
    GSMemLeft -> "left"
    GSMemGroupDeleted -> "deleted"
    GSMemUnknown -> "unknown"
    GSMemInvited -> "invited"
    GSMemPendingApproval -> "pending_approval"
    GSMemPendingReview -> "pending_review"
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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data FileDescr = FileDescr {fileDescrText :: Text, fileDescrPartNo :: Int, fileDescrComplete :: Bool}
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  parseJSON = textParseJSON "InlineFileMode"

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
  deriving (Eq, Show)

data XFTPRcvFile = XFTPRcvFile
  { rcvFileDescription :: RcvFileDescr,
    agentRcvFileId :: Maybe AgentRcvFileId,
    agentRcvFileDeleted :: Bool,
    userApprovedRelays :: Bool
  }
  deriving (Eq, Show)

type RcvFileDescrText = Text

data RcvFileDescr = RcvFileDescr
  { fileDescrId :: Int64,
    fileDescrText :: RcvFileDescrText,
    fileDescrPartNo :: Int,
    fileDescrComplete :: Bool
  }
  deriving (Eq, Show)

data RcvFileStatus
  = RFSNew
  | RFSAccepted {fileInfo :: RcvFileInfo}
  | RFSConnected {fileInfo :: RcvFileInfo}
  | RFSComplete {fileInfo :: RcvFileInfo}
  | RFSCancelled {fileInfo_ :: Maybe RcvFileInfo}
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Ord, Show)
  deriving newtype (FromField)

instance ToField AgentConnId where toField (AgentConnId m) = toField $ Binary m

instance StrEncoding AgentConnId where
  strEncode (AgentConnId connId) = strEncode connId
  strDecode s = AgentConnId <$> strDecode s
  strP = AgentConnId <$> strP

instance FromJSON AgentConnId where
  parseJSON = strParseJSON "AgentConnId"

instance ToJSON AgentConnId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

newtype AgentSndFileId = AgentSndFileId SndFileId
  deriving (Eq, Show)
  deriving newtype (FromField)

instance ToField AgentSndFileId where toField (AgentSndFileId m) = toField $ Binary m

instance StrEncoding AgentSndFileId where
  strEncode (AgentSndFileId connId) = strEncode connId
  strDecode s = AgentSndFileId <$> strDecode s
  strP = AgentSndFileId <$> strP

instance FromJSON AgentSndFileId where
  parseJSON = strParseJSON "AgentSndFileId"

instance ToJSON AgentSndFileId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

newtype AgentRcvFileId = AgentRcvFileId RcvFileId
  deriving (Eq, Show)
  deriving newtype (FromField)

instance ToField AgentRcvFileId where toField (AgentRcvFileId m) = toField $ Binary m

instance StrEncoding AgentRcvFileId where
  strEncode (AgentRcvFileId connId) = strEncode connId
  strDecode s = AgentRcvFileId <$> strDecode s
  strP = AgentRcvFileId <$> strP

instance FromJSON AgentRcvFileId where
  parseJSON = strParseJSON "AgentRcvFileId"

instance ToJSON AgentRcvFileId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

newtype AgentInvId = AgentInvId InvitationId
  deriving (Eq, Show)

instance StrEncoding AgentInvId where
  strEncode (AgentInvId connId) = strEncode connId
  strDecode s = AgentInvId <$> strDecode s
  strP = AgentInvId <$> strP

instance FromJSON AgentInvId where
  parseJSON = strParseJSON "AgentInvId"

instance ToJSON AgentInvId where
  toJSON = strToJSON
  toEncoding = strToJEncoding

deriving newtype instance FromField AgentInvId

instance ToField AgentInvId where toField (AgentInvId m) = toField m

data FileTransfer
  = FTSnd
      { fileTransferMeta :: FileTransferMeta,
        sndFileTransfers :: [SndFileTransfer]
      }
  | FTRcv {rcvFileTransfer :: RcvFileTransfer}
  deriving (Show)

data FileTransferMeta = FileTransferMeta
  { fileId :: FileTransferId,
    xftpSndFile :: Maybe XFTPSndFile,
    xftpRedirectFor :: Maybe FileTransferId,
    fileName :: String,
    filePath :: String,
    fileSize :: Integer,
    fileInline :: Maybe InlineFileMode,
    chunkSize :: Integer,
    cancelled :: Bool
  }
  deriving (Eq, Show)

data LocalFileMeta = LocalFileMeta
  { fileId :: FileTransferId,
    fileName :: String,
    filePath :: String,
    fileSize :: Integer,
    fileCryptoArgs :: Maybe CryptoFileArgs
  }
  deriving (Eq, Show)

data XFTPSndFile = XFTPSndFile
  { agentSndFileId :: AgentSndFileId,
    privateSndFileDescr :: Maybe Text,
    agentSndFileDeleted :: Bool,
    cryptoArgs :: Maybe CryptoFileArgs
  }
  deriving (Eq, Show)

fileTransferCancelled :: FileTransfer -> Bool
fileTransferCancelled (FTSnd FileTransferMeta {cancelled} _) = cancelled
fileTransferCancelled (FTRcv RcvFileTransfer {cancelled}) = cancelled

-- For XFTP file transfers FSConnected means "uploaded to XFTP relays"
data FileStatus = FSNew | FSAccepted | FSConnected | FSComplete | FSCancelled deriving (Eq, Ord, Show)

instance FromField FileStatus where fromField = fromTextField_ textDecode

instance ToField FileStatus where toField = toField . textEncode

instance FromJSON FileStatus where
  parseJSON = textParseJSON "FileStatus"

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

type CreatedLinkInvitation = CreatedConnLink 'CMInvitation

type CreatedLinkContact = CreatedConnLink 'CMContact

type ConnLinkContact = ConnectionLink 'CMContact

type ShortLinkInvitation = ConnShortLink 'CMInvitation

type ShortLinkContact = ConnShortLink 'CMContact

data Connection = Connection
  { connId :: Int64,
    agentConnId :: AgentConnId,
    connChatVersion :: VersionChat,
    peerChatVRange :: VersionRangeChat,
    connLevel :: Int,
    viaContact :: Maybe Int64, -- group member contact ID, if not direct connection
    viaUserContactLink :: Maybe Int64, -- user contact link ID, if connected via "user address"
    viaGroupLink :: Bool, -- whether contact connected via group link
    groupLinkId :: Maybe GroupLinkId,
    xContactId :: Maybe XContactId,
    customUserProfileId :: Maybe Int64,
    connType :: ConnType,
    connStatus :: ConnStatus,
    contactConnInitiated :: Bool,
    localAlias :: Text,
    entityId :: Maybe Int64, -- contact, group member, file ID or user contact ID
    connectionCode :: Maybe SecurityCode,
    pqSupport :: PQSupport,
    pqEncryption :: PQEncryption,
    pqSndEnabled :: Maybe PQEncryption,
    pqRcvEnabled :: Maybe PQEncryption,
    authErrCounter :: Int,
    quotaErrCounter :: Int, -- if exceeds limit messages to group members are created as pending; sending to contacts is unaffected by this
    createdAt :: UTCTime
  }
  deriving (Eq, Show)

connReady :: Connection -> Bool
connReady Connection {connStatus} = connStatus == ConnReady || connStatus == ConnSndReady

authErrDisableCount :: Int
authErrDisableCount = 10

connDisabled :: Connection -> Bool
connDisabled Connection {authErrCounter} = authErrCounter >= authErrDisableCount

quotaErrInactiveCount :: Int
quotaErrInactiveCount = 5

quotaErrSetOnMERR :: Int
quotaErrSetOnMERR = 999

connInactive :: Connection -> Bool
connInactive Connection {quotaErrCounter} = quotaErrCounter >= quotaErrInactiveCount

data SecurityCode = SecurityCode {securityCode :: Text, verifiedAt :: UTCTime}
  deriving (Eq, Show)

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

connPQEnabled :: Connection -> PQEncryption
connPQEnabled Connection {pqSndEnabled = Just (PQEncryption s), pqRcvEnabled = Just (PQEncryption r)} = PQEncryption $ s && r
connPQEnabled _ = PQEncOff

data PendingContactConnection = PendingContactConnection
  { pccConnId :: Int64,
    pccAgentConnId :: AgentConnId,
    pccConnStatus :: ConnStatus,
    viaContactUri :: Bool, -- whether connection was created via contact request to a contact link
    viaUserContactLink :: Maybe Int64,
    groupLinkId :: Maybe GroupLinkId,
    customUserProfileId :: Maybe Int64,
    connLinkInv :: Maybe CreatedLinkInvitation,
    localAlias :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show)

mkPendingContactConnection :: Connection -> Maybe CreatedLinkInvitation -> PendingContactConnection
mkPendingContactConnection Connection {connId, agentConnId, connStatus, xContactId, viaUserContactLink, groupLinkId, customUserProfileId, localAlias, createdAt} connLinkInv =
  PendingContactConnection
  { pccConnId = connId,
    pccAgentConnId = agentConnId,
    pccConnStatus = connStatus,
    viaContactUri = isJust xContactId,
    viaUserContactLink,
    groupLinkId,
    customUserProfileId,
    connLinkInv,
    localAlias,
    createdAt,
    updatedAt = createdAt
  }

aConnId' :: PendingContactConnection -> ConnId
aConnId' PendingContactConnection {pccAgentConnId = AgentConnId cId} = cId

data ConnStatus
  = -- | connection is created by initiating party with agent NEW command (createConnection)
    ConnNew
  | -- | connection is prepared, to avoid changing keys on invitation links when retrying.
    ConnPrepared
  | -- | connection is joined by joining party with agent JOIN command (joinConnection)
    ConnJoined
  | -- | initiating party received CONF notification (to be renamed to REQ)
    ConnRequested
  | -- | initiating party accepted connection with agent LET command (to be renamed to ACPT) (allowConnection)
    ConnAccepted
  | -- | connection can be sent messages to (after joining party received INFO notification, or after securing snd queue on join)
    ConnSndReady
  | -- | connection is ready for both parties to send and receive messages
    ConnReady
  | -- | connection deleted
    ConnDeleted
  deriving (Eq, Show, Read)

instance FromField ConnStatus where fromField = fromTextField_ textDecode

instance ToField ConnStatus where toField = toField . textEncode

instance FromJSON ConnStatus where
  parseJSON = textParseJSON "ConnStatus"

instance ToJSON ConnStatus where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance TextEncoding ConnStatus where
  textDecode = \case
    "new" -> Just ConnNew
    "prepared" -> Just ConnPrepared
    "joined" -> Just ConnJoined
    "requested" -> Just ConnRequested
    "accepted" -> Just ConnAccepted
    "snd-ready" -> Just ConnSndReady
    "ready" -> Just ConnReady
    "deleted" -> Just ConnDeleted
    _ -> Nothing
  textEncode = \case
    ConnNew -> "new"
    ConnPrepared -> "prepared"
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

instance FromJSON ConnType where
  parseJSON = textParseJSON "ConnType"

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
  deriving (Eq, Show)

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

data NetworkStatus
  = NSUnknown
  | NSConnected
  | NSDisconnected
  | NSError {connectionError :: String}
  deriving (Eq, Ord, Show)

netStatusStr :: NetworkStatus -> String
netStatusStr = \case
  NSUnknown -> "unknown"
  NSConnected -> "connected"
  NSDisconnected -> "disconnected"
  NSError e -> "error: " <> e

data ConnNetworkStatus = ConnNetworkStatus
  { agentConnId :: AgentConnId,
    networkStatus :: NetworkStatus
  }
  deriving (Show)

type CommandId = Int64

aCorrId :: CommandId -> ACorrId
aCorrId = pack . show

commandId :: ACorrId -> String
commandId = unpack

data CommandStatus
  = CSCreated
  | CSCompleted -- unused - was replaced with deleteCommand
  | CSError -- internal command error, e.g. not matching connection id or unexpected response, not related to agent message ERR
  deriving (Show)

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
  | CFAckMessage -- not used
  | CFDeleteConn -- not used
  deriving (Eq, Show)

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

commandExpectedResponse :: CommandFunction -> AEvtTag
commandExpectedResponse = \case
  CFCreateConnGrpMemInv -> t INV_
  CFCreateConnGrpInv -> t INV_
  CFCreateConnFileInvDirect -> t INV_
  CFCreateConnFileInvGroup -> t INV_
  CFJoinConn -> t JOINED_
  CFAllowConn -> t OK_
  CFAcceptContact -> t JOINED_
  CFAckMessage -> t OK_
  CFDeleteConn -> t OK_
  where
    t = AEvtTag SAEConn

data CommandData = CommandData
  { cmdId :: CommandId,
    cmdConnId :: Maybe Int64,
    cmdFunction :: CommandFunction,
    cmdStatus :: CommandStatus
  }
  deriving (Show)

data ChatTag = ChatTag
  { chatTagId :: Int64,
    chatTagText :: Text,
    chatTagEmoji :: Maybe Text
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

-- | Entity for local chats
data NoteFolder = NoteFolder
  { noteFolderId :: NoteFolderId,
    userId :: UserId,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    chatTs :: UTCTime,
    favorite :: Bool,
    unread :: Bool
  }
  deriving (Eq, Show)

type NoteFolderId = Int64

data ChatVersion

instance VersionScope ChatVersion

type VersionChat = Version ChatVersion

type VersionRangeChat = VersionRange ChatVersion

pattern VersionChat :: Word16 -> VersionChat
pattern VersionChat v = Version v

-- this newtype exists to have a concise JSON encoding of version ranges in chat protocol messages in the form of "1-2" or just "1"
newtype ChatVersionRange = ChatVersionRange {fromChatVRange :: VersionRangeChat} deriving (Eq, Show)

-- TODO v6.0 review
peerConnChatVersion :: VersionRangeChat -> VersionRangeChat -> VersionChat
peerConnChatVersion _local@(VersionRange lmin lmax) _peer@(VersionRange rmin rmax)
  | lmin <= rmax && rmin <= lmax = min lmax rmax -- compatible
  | rmin > lmax = rmin
  | otherwise = rmax

initialChatVersion :: VersionChat
initialChatVersion = VersionChat 1

chatInitialVRange :: VersionRangeChat
chatInitialVRange = versionToRange initialChatVersion

instance FromJSON ChatVersionRange where
  parseJSON v = ChatVersionRange <$> strParseJSON "ChatVersionRange" v

instance ToJSON ChatVersionRange where
  toJSON (ChatVersionRange vr) = strToJSON vr
  toEncoding (ChatVersionRange vr) = strToJEncoding vr

-- This type is needed for backward compatibility of new remote controller with old remote host.
-- See CONTRIBUTING.md
newtype BoolDef = BoolDef Bool
  deriving newtype (Show, ToJSON)

instance FromJSON BoolDef where
  parseJSON v = BoolDef <$> parseJSON v
  omittedField = Just (BoolDef False)

$(JQ.deriveJSON defaultJSON ''UserContact)

$(JQ.deriveJSON defaultJSON ''Profile)

$(JQ.deriveJSON defaultJSON ''LocalProfile)

$(JQ.deriveJSON defaultJSON ''UserContactRequest)

$(JQ.deriveJSON (enumJSON $ dropPrefix "MC") {J.tagSingleConstructors = True} ''MemberCriteria)

$(JQ.deriveJSON defaultJSON ''GroupMemberAdmission)

instance ToField GroupMemberAdmission where
  toField = toField . encodeJSON

instance FromField GroupMemberAdmission where
  fromField = fromTextField_ decodeJSON

$(JQ.deriveJSON defaultJSON ''GroupProfile)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "IB") ''InvitedBy)

$(JQ.deriveJSON defaultJSON ''GroupMemberSettings)

$(JQ.deriveJSON defaultJSON ''SecurityCode)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "NS") ''NetworkStatus)

$(JQ.deriveJSON defaultJSON ''ConnNetworkStatus)

$(JQ.deriveJSON defaultJSON ''Connection)

$(JQ.deriveJSON defaultJSON ''PendingContactConnection)

$(JQ.deriveJSON defaultJSON ''GroupSupportChat)

$(JQ.deriveJSON defaultJSON ''GroupMember)

$(JQ.deriveJSON (enumJSON $ dropPrefix "MF") ''MsgFilter)

$(JQ.deriveJSON defaultJSON ''ChatSettings)

$(JQ.deriveJSON (enumJSON $ dropPrefix "BC") ''BusinessChatType)

$(JQ.deriveJSON defaultJSON ''BusinessChatInfo)

$(JQ.deriveJSON defaultJSON ''PreparedGroup)

$(JQ.deriveJSON defaultJSON ''GroupInfo)

$(JQ.deriveJSON defaultJSON ''Group)

$(JQ.deriveJSON defaultJSON ''GroupSummary)

$(JQ.deriveJSON defaultJSON ''GroupInfoSummary)

instance FromField MsgFilter where fromField = fromIntField_ msgFilterIntP

instance ToField MsgFilter where toField = toField . msgFilterInt

$(JQ.deriveJSON defaultJSON ''CReqClientData)

$(JQ.deriveJSON defaultJSON ''MemberIdRole)

$(JQ.deriveJSON defaultJSON ''MemberInfo)

$(JQ.deriveJSON defaultJSON ''GroupInvitation)

$(JQ.deriveJSON defaultJSON ''GroupLinkInvitation)

$(JQ.deriveJSON defaultJSON ''GroupLinkRejection)

$(JQ.deriveJSON defaultJSON ''IntroInvitation)

$(JQ.deriveJSON defaultJSON ''MemberRestrictions)

$(JQ.deriveJSON defaultJSON ''GroupMemberRef)

$(JQ.deriveJSON defaultJSON ''FileDescr)

$(JQ.deriveJSON defaultJSON ''FileInvitation)

$(JQ.deriveJSON defaultJSON ''SndFileTransfer)

$(JQ.deriveJSON defaultJSON ''RcvFileDescr)

$(JQ.deriveJSON defaultJSON ''XFTPRcvFile)

$(JQ.deriveJSON defaultJSON ''RcvFileInfo)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "RFS") ''RcvFileStatus)

$(JQ.deriveJSON defaultJSON ''RcvFileTransfer)

$(JQ.deriveJSON defaultJSON ''XFTPSndFile)

$(JQ.deriveJSON defaultJSON ''FileTransferMeta)

$(JQ.deriveJSON defaultJSON ''PreparedContact)

$(JQ.deriveJSON defaultJSON ''LocalFileMeta)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "FT") ''FileTransfer)

$(JQ.deriveJSON defaultJSON ''UserPwdHash)

$(JQ.deriveJSON defaultJSON ''User)

$(JQ.deriveJSON defaultJSON ''NewUser)

$(JQ.deriveJSON defaultJSON ''UserInfo)

$(JQ.deriveJSON defaultJSON ''Contact)

$(JQ.deriveJSON defaultJSON ''ContactRef)

$(JQ.deriveJSON defaultJSON ''NoteFolder)

$(JQ.deriveJSON defaultJSON ''ChatTag)

$(JQ.deriveJSON defaultJSON ''ShortGroupInfo)

$(JQ.deriveJSON defaultJSON ''ShortGroupMember)
