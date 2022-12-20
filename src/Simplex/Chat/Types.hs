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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Simplex.Chat.Types where

import Control.Applicative ((<|>))
import Crypto.Number.Serialize (os2ip)
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
import GHC.Records.Compat
import Simplex.Messaging.Agent.Protocol (ACommandTag (..), ACorrId, AParty (..), ConnId, ConnectionMode (..), ConnectionRequestUri, InvitationId)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (dropPrefix, enumJSON, fromTextField_, sumTypeJSON, taggedObjectJSON)
import Simplex.Messaging.Protocol (SMPServerWithAuth)
import Simplex.Messaging.Util (safeDecodeUtf8, (<$?>))

class IsContact a where
  contactId' :: a -> ContactId
  profile' :: a -> LocalProfile
  localDisplayName' :: a -> ContactName
  preferences' :: a -> Maybe Preferences

instance IsContact User where
  contactId' = userContactId
  {-# INLINE contactId' #-}
  profile' = profile
  {-# INLINE profile' #-}
  localDisplayName' = localDisplayName
  {-# INLINE localDisplayName' #-}
  preferences' User {profile = LocalProfile {preferences}} = preferences
  {-# INLINE preferences' #-}

instance IsContact Contact where
  contactId' = contactId
  {-# INLINE contactId' #-}
  profile' = profile
  {-# INLINE profile' #-}
  localDisplayName' = localDisplayName
  {-# INLINE localDisplayName' #-}
  preferences' Contact {profile = LocalProfile {preferences}} = preferences
  {-# INLINE preferences' #-}

data User = User
  { userId :: UserId,
    userContactId :: ContactId,
    localDisplayName :: ContactName,
    profile :: LocalProfile,
    fullPreferences :: FullPreferences,
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
    mergedPreferences :: ContactUserPreferences,
    confirmPrefPending :: Bool,
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
contactConnId = aConnId . contactConn

contactConnIncognito :: Contact -> Bool
contactConnIncognito = connIncognito . contactConn

directOrUsed :: Contact -> Bool
directOrUsed Contact {contactUsed, activeConn = Connection {connLevel, viaGroupLink}} =
  (connLevel == 0 && not viaGroupLink) || contactUsed

anyDirectOrUsed :: Contact -> Bool
anyDirectOrUsed Contact {contactUsed, activeConn = Connection {connLevel}} = connLevel == 0 || contactUsed

contactSecurityCode :: Contact -> Maybe SecurityCode
contactSecurityCode Contact {activeConn} = connectionCode activeConn

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
    fullGroupPreferences :: FullGroupPreferences,
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
  = CFTimedMessages
  | CFFullDelete
  | -- | CFReceipts
    CFVoice
  deriving (Show, Generic)

data SChatFeature (f :: ChatFeature) where
  SCFTimedMessages :: SChatFeature 'CFTimedMessages
  SCFFullDelete :: SChatFeature 'CFFullDelete
  SCFVoice :: SChatFeature 'CFVoice

deriving instance Show (SChatFeature f)

data AChatFeature = forall f. FeatureI f => ACF (SChatFeature f)

deriving instance Show AChatFeature

chatFeatureToText :: ChatFeature -> Text
chatFeatureToText = \case
  CFTimedMessages -> "Disappearing messages"
  CFFullDelete -> "Full deletion"
  CFVoice -> "Voice messages"

featureAllowed :: SChatFeature f -> (PrefEnabled -> Bool) -> Contact -> Bool
featureAllowed feature forWhom Contact {mergedPreferences} =
  let ContactUserPreference {enabled} = getContactUserPreference feature mergedPreferences
   in forWhom enabled

instance ToJSON ChatFeature where
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "CF"
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "CF"

instance FromJSON ChatFeature where
  parseJSON = J.genericParseJSON . enumJSON $ dropPrefix "CF"

allChatFeatures :: [AChatFeature]
allChatFeatures =
  [ ACF SCFTimedMessages,
    ACF SCFFullDelete,
    -- CFReceipts,
    ACF SCFVoice
  ]

chatPrefSel :: SChatFeature f -> Preferences -> Maybe (FeaturePreference f)
chatPrefSel = \case
  SCFTimedMessages -> timedMessages
  SCFFullDelete -> fullDelete
  -- CFReceipts -> receipts
  SCFVoice -> voice

chatFeature :: SChatFeature f -> ChatFeature
chatFeature = \case
  SCFTimedMessages -> CFTimedMessages
  SCFFullDelete -> CFFullDelete
  SCFVoice -> CFVoice

class PreferenceI p where
  getPreference :: SChatFeature f -> p -> FeaturePreference f

instance PreferenceI Preferences where
  getPreference f prefs = fromMaybe (getPreference f defaultChatPrefs) (chatPrefSel f prefs)

instance PreferenceI (Maybe Preferences) where
  getPreference f prefs = fromMaybe (getPreference f defaultChatPrefs) (chatPrefSel f =<< prefs)

instance PreferenceI FullPreferences where
  getPreference = \case
    SCFTimedMessages -> timedMessages
    SCFFullDelete -> fullDelete
    -- CFReceipts -> receipts
    SCFVoice -> voice
  {-# INLINE getPreference #-}

setPreference :: forall f. FeatureI f => SChatFeature f -> Maybe FeatureAllowed -> Maybe Preferences -> Preferences
setPreference f allow_ prefs_ = setPreference_ f pref prefs
  where
    pref = setAllow <$> allow_
    setAllow :: FeatureAllowed -> FeaturePreference f
    setAllow = setField @"allow" (getPreference f prefs)
    prefs = toChatPrefs $ mergePreferences Nothing prefs_

setPreference' :: SChatFeature f -> Maybe (FeaturePreference f) -> Maybe Preferences -> Preferences
setPreference' f pref_ prefs_ = setPreference_ f pref_ prefs
  where
    prefs = toChatPrefs $ mergePreferences Nothing prefs_

setPreference_ :: SChatFeature f -> Maybe (FeaturePreference f) -> Preferences -> Preferences
setPreference_ f pref_ prefs =
  case f of
    SCFTimedMessages -> prefs {timedMessages = pref_}
    SCFFullDelete -> prefs {fullDelete = pref_}
    SCFVoice -> prefs {voice = pref_}

-- collection of optional chat preferences for the user and the contact
data Preferences = Preferences
  { timedMessages :: Maybe TimedMessagesPreference,
    fullDelete :: Maybe FullDeletePreference,
    -- receipts :: Maybe SimplePreference,
    voice :: Maybe VoicePreference
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON Preferences where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

instance ToField Preferences where
  toField = toField . encodeJSON

instance FromField Preferences where
  fromField = fromTextField_ decodeJSON

data GroupFeature
  = GFTimedMessages
  | GFDirectMessages
  | GFFullDelete
  | -- | GFReceipts
    GFVoice
  deriving (Show, Generic)

data SGroupFeature (f :: GroupFeature) where
  SGFTimedMessages :: SGroupFeature 'GFTimedMessages
  SGFDirectMessages :: SGroupFeature 'GFDirectMessages
  SGFFullDelete :: SGroupFeature 'GFFullDelete
  -- SGFReceipts
  SGFVoice :: SGroupFeature 'GFVoice

deriving instance Show (SGroupFeature f)

data AGroupFeature = forall f. GroupFeatureI f => AGF (SGroupFeature f)

deriving instance Show AGroupFeature

groupFeatureToText :: GroupFeature -> Text
groupFeatureToText = \case
  GFTimedMessages -> "Disappearing messages"
  GFDirectMessages -> "Direct messages"
  GFFullDelete -> "Full deletion"
  GFVoice -> "Voice messages"

groupFeatureAllowed :: GroupFeatureI f => SGroupFeature f -> GroupInfo -> Bool
groupFeatureAllowed feature gInfo = groupFeatureAllowed' feature $ fullGroupPreferences gInfo

groupFeatureAllowed' :: GroupFeatureI f => SGroupFeature f -> FullGroupPreferences -> Bool
groupFeatureAllowed' feature prefs =
  getField @"enable" (getGroupPreference feature prefs) == FEOn

instance ToJSON GroupFeature where
  toEncoding = J.genericToEncoding . enumJSON $ dropPrefix "GF"
  toJSON = J.genericToJSON . enumJSON $ dropPrefix "GF"

instance FromJSON GroupFeature where
  parseJSON = J.genericParseJSON . enumJSON $ dropPrefix "GF"

allGroupFeatures :: [AGroupFeature]
allGroupFeatures =
  [ AGF SGFTimedMessages,
    AGF SGFDirectMessages,
    AGF SGFFullDelete,
    -- GFReceipts,
    AGF SGFVoice
  ]

groupPrefSel :: SGroupFeature f -> GroupPreferences -> Maybe (GroupFeaturePreference f)
groupPrefSel = \case
  SGFTimedMessages -> timedMessages
  SGFDirectMessages -> directMessages
  SGFFullDelete -> fullDelete
  -- GFReceipts -> receipts
  SGFVoice -> voice

toGroupFeature :: SGroupFeature f -> GroupFeature
toGroupFeature = \case
  SGFTimedMessages -> GFTimedMessages
  SGFDirectMessages -> GFDirectMessages
  SGFFullDelete -> GFFullDelete
  SGFVoice -> GFVoice

class GroupPreferenceI p where
  getGroupPreference :: SGroupFeature f -> p -> GroupFeaturePreference f

instance GroupPreferenceI GroupPreferences where
  getGroupPreference pt prefs = fromMaybe (getGroupPreference pt defaultGroupPrefs) (groupPrefSel pt prefs)

instance GroupPreferenceI (Maybe GroupPreferences) where
  getGroupPreference pt prefs = fromMaybe (getGroupPreference pt defaultGroupPrefs) (groupPrefSel pt =<< prefs)

instance GroupPreferenceI FullGroupPreferences where
  getGroupPreference = \case
    SGFTimedMessages -> timedMessages
    SGFDirectMessages -> directMessages
    SGFFullDelete -> fullDelete
    -- GFReceipts -> receipts
    SGFVoice -> voice
  {-# INLINE getGroupPreference #-}

-- collection of optional group preferences
data GroupPreferences = GroupPreferences
  { timedMessages :: Maybe TimedMessagesGroupPreference,
    directMessages :: Maybe DirectMessagesGroupPreference,
    fullDelete :: Maybe FullDeleteGroupPreference,
    -- receipts :: Maybe GroupPreference,
    voice :: Maybe VoiceGroupPreference
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupPreferences where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

instance ToField GroupPreferences where
  toField = toField . encodeJSON

instance FromField GroupPreferences where
  fromField = fromTextField_ decodeJSON

setGroupPreference :: forall f. GroupFeatureI f => SGroupFeature f -> GroupFeatureEnabled -> Maybe GroupPreferences -> GroupPreferences
setGroupPreference f enable prefs_ = setGroupPreference_ f pref prefs
  where
    prefs = mergeGroupPreferences prefs_
    pref :: GroupFeaturePreference f
    pref = setField @"enable" (getGroupPreference f prefs) enable

setGroupPreference' :: SGroupFeature f -> GroupFeaturePreference f -> Maybe GroupPreferences -> GroupPreferences
setGroupPreference' f pref prefs_ = setGroupPreference_ f pref prefs
  where
    prefs = mergeGroupPreferences prefs_

setGroupPreference_ :: SGroupFeature f -> GroupFeaturePreference f -> FullGroupPreferences -> GroupPreferences
setGroupPreference_ f pref prefs =
  toGroupPreferences $ case f of
    SGFTimedMessages -> prefs {timedMessages = pref}
    SGFDirectMessages -> prefs {directMessages = pref}
    SGFVoice -> prefs {voice = pref}
    SGFFullDelete -> prefs {fullDelete = pref}

setGroupTimedMessagesPreference :: TimedMessagesGroupPreference -> Maybe GroupPreferences -> GroupPreferences
setGroupTimedMessagesPreference pref prefs_ =
  toGroupPreferences $ prefs {timedMessages = pref}
  where
    prefs = mergeGroupPreferences prefs_

-- full collection of chat preferences defined in the app - it is used to ensure we include all preferences and to simplify processing
-- if some of the preferences are not defined in Preferences, defaults from defaultChatPrefs are used here.
data FullPreferences = FullPreferences
  { timedMessages :: TimedMessagesPreference,
    fullDelete :: FullDeletePreference,
    -- receipts :: SimplePreference,
    voice :: VoicePreference
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON FullPreferences where toEncoding = J.genericToEncoding J.defaultOptions

-- full collection of group preferences defined in the app - it is used to ensure we include all preferences and to simplify processing
-- if some of the preferences are not defined in GroupPreferences, defaults from defaultGroupPrefs are used here.
data FullGroupPreferences = FullGroupPreferences
  { timedMessages :: TimedMessagesGroupPreference,
    directMessages :: DirectMessagesGroupPreference,
    fullDelete :: FullDeleteGroupPreference,
    -- receipts :: GroupPreference,
    voice :: VoiceGroupPreference
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON FullGroupPreferences where toEncoding = J.genericToEncoding J.defaultOptions

-- merged preferences of user for a given contact - they differentiate between specific preferences for the contact and global user preferences
data ContactUserPreferences = ContactUserPreferences
  { timedMessages :: ContactUserPreference TimedMessagesPreference,
    fullDelete :: ContactUserPreference FullDeletePreference,
    -- receipts :: ContactUserPreference,
    voice :: ContactUserPreference VoicePreference
  }
  deriving (Eq, Show, Generic)

data ContactUserPreference p = ContactUserPreference
  { enabled :: PrefEnabled,
    userPreference :: ContactUserPref p,
    contactPreference :: p
  }
  deriving (Eq, Show, Generic)

data ContactUserPref p = CUPContact {preference :: p} | CUPUser {preference :: p}
  deriving (Eq, Show, Generic)

instance ToJSON ContactUserPreferences where toEncoding = J.genericToEncoding J.defaultOptions

instance ToJSON p => ToJSON (ContactUserPreference p) where toEncoding = J.genericToEncoding J.defaultOptions

instance ToJSON p => ToJSON (ContactUserPref p) where
  toJSON = J.genericToJSON . sumTypeJSON $ dropPrefix "CUP"
  toEncoding = J.genericToEncoding . sumTypeJSON $ dropPrefix "CUP"

toChatPrefs :: FullPreferences -> Preferences
toChatPrefs FullPreferences {fullDelete, voice, timedMessages} =
  Preferences
    { timedMessages = Just timedMessages,
      fullDelete = Just fullDelete,
      -- receipts = Just receipts,
      voice = Just voice
    }

defaultChatPrefs :: FullPreferences
defaultChatPrefs =
  FullPreferences
    { timedMessages = TimedMessagesPreference {allow = FANo, ttl = Nothing},
      fullDelete = FullDeletePreference {allow = FANo},
      -- receipts = SimplePreference {allow = FANo},
      voice = VoicePreference {allow = FAYes}
    }

emptyChatPrefs :: Preferences
emptyChatPrefs = Preferences Nothing Nothing Nothing

defaultGroupPrefs :: FullGroupPreferences
defaultGroupPrefs =
  FullGroupPreferences
    { timedMessages = TimedMessagesGroupPreference {enable = FEOff, ttl = 86400},
      directMessages = DirectMessagesGroupPreference {enable = FEOff},
      fullDelete = FullDeleteGroupPreference {enable = FEOff},
      -- receipts = GroupPreference {enable = FEOff},
      voice = VoiceGroupPreference {enable = FEOn}
    }

emptyGroupPrefs :: GroupPreferences
emptyGroupPrefs = GroupPreferences Nothing Nothing Nothing Nothing

data TimedMessagesPreference = TimedMessagesPreference
  { allow :: FeatureAllowed,
    ttl :: Maybe Int
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON TimedMessagesPreference where
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}

data FullDeletePreference = FullDeletePreference {allow :: FeatureAllowed}
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON FullDeletePreference where toEncoding = J.genericToEncoding J.defaultOptions

data VoicePreference = VoicePreference {allow :: FeatureAllowed}
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON VoicePreference where toEncoding = J.genericToEncoding J.defaultOptions

class (Eq (FeaturePreference f), HasField "allow" (FeaturePreference f) FeatureAllowed) => FeatureI f where
  type FeaturePreference (f :: ChatFeature) = p | p -> f
  prefParam :: FeaturePreference f -> Maybe Int

instance HasField "allow" TimedMessagesPreference FeatureAllowed where
  hasField p = (\allow -> p {allow}, allow (p :: TimedMessagesPreference))

instance HasField "allow" FullDeletePreference FeatureAllowed where
  hasField p = (\allow -> p {allow}, allow (p :: FullDeletePreference))

instance HasField "allow" VoicePreference FeatureAllowed where
  hasField p = (\allow -> p {allow}, allow (p :: VoicePreference))

instance FeatureI 'CFTimedMessages where
  type FeaturePreference 'CFTimedMessages = TimedMessagesPreference
  prefParam TimedMessagesPreference {ttl} = ttl

instance FeatureI 'CFFullDelete where
  type FeaturePreference 'CFFullDelete = FullDeletePreference
  prefParam _ = Nothing

instance FeatureI 'CFVoice where
  type FeaturePreference 'CFVoice = VoicePreference
  prefParam _ = Nothing

data GroupPreference = GroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show, Generic, FromJSON)

data TimedMessagesGroupPreference = TimedMessagesGroupPreference
  { enable :: GroupFeatureEnabled,
    ttl :: Int
  }
  deriving (Eq, Show, Generic, FromJSON)

data DirectMessagesGroupPreference = DirectMessagesGroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show, Generic, FromJSON)

data FullDeleteGroupPreference = FullDeleteGroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show, Generic, FromJSON)

data VoiceGroupPreference = VoiceGroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON GroupPreference where toEncoding = J.genericToEncoding J.defaultOptions

instance ToJSON TimedMessagesGroupPreference where toEncoding = J.genericToEncoding J.defaultOptions

instance ToJSON DirectMessagesGroupPreference where toEncoding = J.genericToEncoding J.defaultOptions

instance ToJSON FullDeleteGroupPreference where toEncoding = J.genericToEncoding J.defaultOptions

instance ToJSON VoiceGroupPreference where toEncoding = J.genericToEncoding J.defaultOptions

class (Eq (GroupFeaturePreference f), HasField "enable" (GroupFeaturePreference f) GroupFeatureEnabled) => GroupFeatureI f where
  type GroupFeaturePreference (f :: GroupFeature) = p | p -> f
  groupPrefParam :: GroupFeaturePreference f -> Maybe Int

instance HasField "enable" GroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, enable (p :: GroupPreference))

instance HasField "enable" TimedMessagesGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, enable (p :: TimedMessagesGroupPreference))

instance HasField "enable" DirectMessagesGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, enable (p :: DirectMessagesGroupPreference))

instance HasField "enable" FullDeleteGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, enable (p :: FullDeleteGroupPreference))

instance HasField "enable" VoiceGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, enable (p :: VoiceGroupPreference))

instance GroupFeatureI 'GFTimedMessages where
  type GroupFeaturePreference 'GFTimedMessages = TimedMessagesGroupPreference
  groupPrefParam TimedMessagesGroupPreference {ttl} = Just ttl

instance GroupFeatureI 'GFDirectMessages where
  type GroupFeaturePreference 'GFDirectMessages = DirectMessagesGroupPreference
  groupPrefParam _ = Nothing

instance GroupFeatureI 'GFFullDelete where
  type GroupFeaturePreference 'GFFullDelete = FullDeleteGroupPreference
  groupPrefParam _ = Nothing

instance GroupFeatureI 'GFVoice where
  type GroupFeaturePreference 'GFVoice = VoiceGroupPreference
  groupPrefParam _ = Nothing

groupPrefToText :: HasField "enable" p GroupFeatureEnabled => p -> Maybe Int -> Text
groupPrefToText p = groupPrefToText_ $ getField @"enable" p

groupPrefToText' :: GroupFeatureI f => GroupFeaturePreference f -> Text
groupPrefToText' p = groupPrefToText_ (getField @"enable" p) (groupPrefParam p)

groupPrefToText_ :: GroupFeatureEnabled -> Maybe Int -> Text
groupPrefToText_ enabled param = do
  let enabledText = safeDecodeUtf8 . strEncode $ enabled
      paramText = if enabled == FEOn then maybe "" (\n -> ", after " <> timedTTLText n) param else ""
   in enabledText <> paramText

timedTTLText :: Int -> Text
timedTTLText 0 = "0 sec"
timedTTLText ttl = do
  let (m', s) = ttl `quotRem` 60
      (h', m) = m' `quotRem` 60
      (d', h) = h' `quotRem` 24
      (mm, d) = d' `quotRem` 30
  T.pack . unwords $
    [mms mm | mm /= 0] <> [ds d | d /= 0] <> [hs h | h /= 0] <> [ms m | m /= 0] <> [ss s | s /= 0]
  where
    ss s = show s <> " sec"
    ms m = show m <> " min"
    hs 1 = "1 hour"
    hs h = show h <> " hours"
    ds 1 = "1 day"
    ds 7 = "1 week"
    ds 14 = "2 weeks"
    ds d = show d <> " days"
    mms 1 = "1 month"
    mms mm = show mm <> " months"

toGroupPreference :: GroupFeatureI f => GroupFeaturePreference f -> GroupPreference
toGroupPreference p = GroupPreference {enable = getField @"enable" p}

data FeatureAllowed
  = FAAlways -- allow unconditionally
  | FAYes -- allow, if peer allows it
  | FANo -- do not allow
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

data GroupFeatureEnabled = FEOn | FEOff
  deriving (Eq, Show, Generic)

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

groupFeatureState :: GroupFeatureI f => GroupFeaturePreference f -> (GroupFeatureEnabled, Maybe Int)
groupFeatureState p =
  let enable = getField @"enable" p
      param = if enable == FEOn then groupPrefParam p else Nothing
   in (enable, param)

mergePreferences :: Maybe Preferences -> Maybe Preferences -> FullPreferences
mergePreferences contactPrefs userPreferences =
  FullPreferences
    { timedMessages = pref SCFTimedMessages,
      fullDelete = pref SCFFullDelete,
      -- receipts = pref CFReceipts,
      voice = pref SCFVoice
    }
  where
    pref :: SChatFeature f -> FeaturePreference f
    pref f =
      let sel = chatPrefSel f
       in fromMaybe (getPreference f defaultChatPrefs) $ (contactPrefs >>= sel) <|> (userPreferences >>= sel)

mergeUserChatPrefs :: User -> Contact -> FullPreferences
mergeUserChatPrefs user ct = mergeUserChatPrefs' user (contactConnIncognito ct) (userPreferences ct)

mergeUserChatPrefs' :: User -> Bool -> Preferences -> FullPreferences
mergeUserChatPrefs' user connectedIncognito userPreferences =
  let userPrefs = if connectedIncognito then Nothing else preferences' user
   in mergePreferences (Just userPreferences) userPrefs

mergeGroupPreferences :: Maybe GroupPreferences -> FullGroupPreferences
mergeGroupPreferences groupPreferences =
  FullGroupPreferences
    { timedMessages = pref SGFTimedMessages,
      directMessages = pref SGFDirectMessages,
      fullDelete = pref SGFFullDelete,
      -- receipts = pref GFReceipts,
      voice = pref SGFVoice
    }
  where
    pref :: SGroupFeature f -> GroupFeaturePreference f
    pref pt = fromMaybe (getGroupPreference pt defaultGroupPrefs) (groupPreferences >>= groupPrefSel pt)

toGroupPreferences :: FullGroupPreferences -> GroupPreferences
toGroupPreferences groupPreferences =
  GroupPreferences
    { timedMessages = pref SGFTimedMessages,
      directMessages = pref SGFDirectMessages,
      fullDelete = pref SGFFullDelete,
      -- receipts = pref GFReceipts,
      voice = pref SGFVoice
    }
  where
    pref :: SGroupFeature f -> Maybe (GroupFeaturePreference f)
    pref f = Just $ getGroupPreference f groupPreferences

data PrefEnabled = PrefEnabled {forUser :: Bool, forContact :: Bool}
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON PrefEnabled where
  toJSON = J.genericToJSON J.defaultOptions
  toEncoding = J.genericToEncoding J.defaultOptions

prefEnabled :: FeatureI f => Bool -> FeaturePreference f -> FeaturePreference f -> PrefEnabled
prefEnabled asymmetric user contact = case (getField @"allow" user, getField @"allow" contact) of
  (FAAlways, FANo) -> PrefEnabled {forUser = False, forContact = asymmetric}
  (FANo, FAAlways) -> PrefEnabled {forUser = asymmetric, forContact = False}
  (_, FANo) -> PrefEnabled False False
  (FANo, _) -> PrefEnabled False False
  _ -> PrefEnabled True True

prefToText :: PrefEnabled -> Maybe Int -> Text
prefToText enabled param =
  let paramText = if enabled == PrefEnabled True True then prefParamText param else ""
   in prefEnabledToText enabled <> paramText

prefParamText :: Maybe Int -> Text
prefParamText = maybe "" (\n -> ", after " <> timedTTLText n)

prefEnabledToText :: PrefEnabled -> Text
prefEnabledToText = \case
  PrefEnabled True True -> "enabled"
  PrefEnabled False False -> "off"
  PrefEnabled {forUser = True, forContact = False} -> "enabled for you"
  PrefEnabled {forUser = False, forContact = True} -> "enabled for contact"

prefToText' :: FeatureI f => FeaturePreference f -> Text
prefToText' p =
  let allowed = getField @"allow" p
      allowedText = case getField @"allow" p of
        FAAlways -> "always"
        FAYes -> "yes"
        FANo -> "no"
      paramText = if allowed == FAAlways || allowed == FAYes then prefParamText (prefParam p) else ""
   in allowedText <> paramText

featureState :: FeatureI f => ContactUserPreference (FeaturePreference f) -> (PrefEnabled, Maybe Int)
featureState ContactUserPreference {enabled, userPreference} =
  let param = if forUser enabled then prefParam $ preference userPreference else Nothing
   in (enabled, param)

updateMergedPreferences :: User -> Contact -> Contact
updateMergedPreferences user ct =
  let mergedPreferences = contactUserPreferences user (userPreferences ct) (preferences' ct) (contactConnIncognito ct)
   in ct {mergedPreferences}

contactUserPreferences :: User -> Preferences -> Maybe Preferences -> Bool -> ContactUserPreferences
contactUserPreferences user userPreferences contactPreferences connectedIncognito =
  ContactUserPreferences
    { timedMessages = pref SCFTimedMessages,
      fullDelete = pref SCFFullDelete,
      -- receipts = pref CFReceipts,
      voice = pref SCFVoice
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

getContactUserPreference :: SChatFeature f -> ContactUserPreferences -> ContactUserPreference (FeaturePreference f)
getContactUserPreference = \case
  SCFTimedMessages -> timedMessages
  SCFFullDelete -> fullDelete
  -- CFReceipts -> receipts
  SCFVoice -> voice

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
    connectionCode :: Maybe SecurityCode,
    createdAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

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
  | CFCreateConnFileInvDirect
  | CFCreateConnFileInvGroup
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

commandExpectedResponse :: CommandFunction -> ACommandTag 'Agent
commandExpectedResponse = \case
  CFCreateConnGrpMemInv -> INV_
  CFCreateConnGrpInv -> INV_
  CFCreateConnFileInvDirect -> INV_
  CFCreateConnFileInvGroup -> INV_
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

data ServerCfg = ServerCfg
  { server :: SMPServerWithAuth,
    preset :: Bool,
    tested :: Maybe Bool,
    enabled :: Bool
  }
  deriving (Show, Generic)

instance ToJSON ServerCfg where
  toEncoding = J.genericToEncoding J.defaultOptions {J.omitNothingFields = True}
  toJSON = J.genericToJSON J.defaultOptions {J.omitNothingFields = True}

instance FromJSON ServerCfg where
  parseJSON = J.genericParseJSON J.defaultOptions {J.omitNothingFields = True}
