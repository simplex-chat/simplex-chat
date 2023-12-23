{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Simplex.Chat.Types.Preferences where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.TH as J
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import GHC.Records.Compat
import Simplex.Chat.Types.Util
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fromTextField_, sumTypeJSON)
import Simplex.Messaging.Util (safeDecodeUtf8, (<$?>))

data ChatFeature
  = CFTimedMessages
  | CFFullDelete
  | CFReactions
  | CFVoice
  | CFCalls
  deriving (Show)

data SChatFeature (f :: ChatFeature) where
  SCFTimedMessages :: SChatFeature 'CFTimedMessages
  SCFFullDelete :: SChatFeature 'CFFullDelete
  SCFReactions :: SChatFeature 'CFReactions
  SCFVoice :: SChatFeature 'CFVoice
  SCFCalls :: SChatFeature 'CFCalls

deriving instance Show (SChatFeature f)

data AChatFeature = forall f. FeatureI f => ACF (SChatFeature f)

deriving instance Show AChatFeature

chatFeatureNameText :: ChatFeature -> Text
chatFeatureNameText = \case
  CFTimedMessages -> "Disappearing messages"
  CFFullDelete -> "Full deletion"
  CFReactions -> "Message reactions"
  CFVoice -> "Voice messages"
  CFCalls -> "Audio/video calls"

chatFeatureNameText' :: SChatFeature f -> Text
chatFeatureNameText' = chatFeatureNameText . chatFeature

allChatFeatures :: [AChatFeature]
allChatFeatures =
  [ ACF SCFTimedMessages,
    ACF SCFFullDelete,
    ACF SCFReactions,
    ACF SCFVoice,
    ACF SCFCalls
  ]

chatPrefSel :: SChatFeature f -> Preferences -> Maybe (FeaturePreference f)
chatPrefSel f ps = case f of
  SCFTimedMessages -> ps.timedMessages
  SCFFullDelete -> ps.fullDelete
  SCFReactions -> ps.reactions
  SCFVoice -> ps.voice
  SCFCalls -> ps.calls

chatFeature :: SChatFeature f -> ChatFeature
chatFeature = \case
  SCFTimedMessages -> CFTimedMessages
  SCFFullDelete -> CFFullDelete
  SCFReactions -> CFReactions
  SCFVoice -> CFVoice
  SCFCalls -> CFCalls

class PreferenceI p where
  getPreference :: SChatFeature f -> p -> FeaturePreference f

instance PreferenceI Preferences where
  getPreference f prefs = fromMaybe (getPreference f defaultChatPrefs) (chatPrefSel f prefs)

instance PreferenceI (Maybe Preferences) where
  getPreference f prefs = fromMaybe (getPreference f defaultChatPrefs) (chatPrefSel f =<< prefs)

instance PreferenceI FullPreferences where
  getPreference f ps = case f of
    SCFTimedMessages -> ps.timedMessages
    SCFFullDelete -> ps.fullDelete
    SCFReactions -> ps.reactions
    SCFVoice -> ps.voice
    SCFCalls -> ps.calls
  {-# INLINE getPreference #-}

setPreference :: forall f. FeatureI f => SChatFeature f -> Maybe FeatureAllowed -> Maybe Preferences -> Preferences
setPreference f allow_ prefs_ = setPreference_ f pref $ fromMaybe emptyChatPrefs prefs_
  where
    pref = setAllow <$> allow_
    setAllow :: FeatureAllowed -> FeaturePreference f
    setAllow = setField @"allow" (getPreference f prefs)
    prefs = mergePreferences Nothing prefs_

setPreference' :: SChatFeature f -> Maybe (FeaturePreference f) -> Maybe Preferences -> Preferences
setPreference' f pref_ prefs_ = setPreference_ f pref_ $ fromMaybe emptyChatPrefs prefs_

setPreference_ :: SChatFeature f -> Maybe (FeaturePreference f) -> Preferences -> Preferences
setPreference_ f pref_ prefs =
  case f of
    SCFTimedMessages -> prefs {timedMessages = pref_}
    SCFFullDelete -> prefs {fullDelete = pref_}
    SCFReactions -> prefs {reactions = pref_}
    SCFVoice -> prefs {voice = pref_}
    SCFCalls -> prefs {calls = pref_}

-- collection of optional chat preferences for the user and the contact
data Preferences = Preferences
  { timedMessages :: Maybe TimedMessagesPreference,
    fullDelete :: Maybe FullDeletePreference,
    reactions :: Maybe ReactionsPreference,
    voice :: Maybe VoicePreference,
    calls :: Maybe CallsPreference
  }
  deriving (Eq, Show)

data GroupFeature
  = GFTimedMessages
  | GFDirectMessages
  | GFFullDelete
  | GFReactions
  | GFVoice
  | GFFiles
  | GFHistory
  deriving (Show)

data SGroupFeature (f :: GroupFeature) where
  SGFTimedMessages :: SGroupFeature 'GFTimedMessages
  SGFDirectMessages :: SGroupFeature 'GFDirectMessages
  SGFFullDelete :: SGroupFeature 'GFFullDelete
  SGFReactions :: SGroupFeature 'GFReactions
  SGFVoice :: SGroupFeature 'GFVoice
  SGFFiles :: SGroupFeature 'GFFiles
  SGFHistory :: SGroupFeature 'GFHistory

deriving instance Show (SGroupFeature f)

data AGroupFeature = forall f. GroupFeatureI f => AGF (SGroupFeature f)

deriving instance Show AGroupFeature

groupFeatureNameText :: GroupFeature -> Text
groupFeatureNameText = \case
  GFTimedMessages -> "Disappearing messages"
  GFDirectMessages -> "Direct messages"
  GFFullDelete -> "Full deletion"
  GFReactions -> "Message reactions"
  GFVoice -> "Voice messages"
  GFFiles -> "Files and media"
  GFHistory -> "Recent history"

groupFeatureNameText' :: SGroupFeature f -> Text
groupFeatureNameText' = groupFeatureNameText . toGroupFeature

groupFeatureAllowed' :: GroupFeatureI f => SGroupFeature f -> FullGroupPreferences -> Bool
groupFeatureAllowed' feature prefs =
  getField @"enable" (getGroupPreference feature prefs) == FEOn

allGroupFeatureItems :: [AGroupFeature]
allGroupFeatureItems =
  [ AGF SGFTimedMessages,
    AGF SGFDirectMessages,
    AGF SGFFullDelete,
    AGF SGFReactions,
    AGF SGFVoice,
    AGF SGFFiles
  ]

allGroupFeatures :: [AGroupFeature]
allGroupFeatures = allGroupFeatureItems <> [AGF SGFHistory]

groupPrefSel :: SGroupFeature f -> GroupPreferences -> Maybe (GroupFeaturePreference f)
groupPrefSel f ps = case f of
  SGFTimedMessages -> ps.timedMessages
  SGFDirectMessages -> ps.directMessages
  SGFFullDelete -> ps.fullDelete
  SGFReactions -> ps.reactions
  SGFVoice -> ps.voice
  SGFFiles -> ps.files
  SGFHistory -> ps.history

toGroupFeature :: SGroupFeature f -> GroupFeature
toGroupFeature = \case
  SGFTimedMessages -> GFTimedMessages
  SGFDirectMessages -> GFDirectMessages
  SGFFullDelete -> GFFullDelete
  SGFReactions -> GFReactions
  SGFVoice -> GFVoice
  SGFFiles -> GFFiles
  SGFHistory -> GFHistory

class GroupPreferenceI p where
  getGroupPreference :: SGroupFeature f -> p -> GroupFeaturePreference f

instance GroupPreferenceI GroupPreferences where
  getGroupPreference pt prefs = fromMaybe (getGroupPreference pt defaultGroupPrefs) (groupPrefSel pt prefs)

instance GroupPreferenceI (Maybe GroupPreferences) where
  getGroupPreference pt prefs = fromMaybe (getGroupPreference pt defaultGroupPrefs) (groupPrefSel pt =<< prefs)

instance GroupPreferenceI FullGroupPreferences where
  getGroupPreference f ps = case f of
    SGFTimedMessages -> ps.timedMessages
    SGFDirectMessages -> ps.directMessages
    SGFFullDelete -> ps.fullDelete
    SGFReactions -> ps.reactions
    SGFVoice -> ps.voice
    SGFFiles -> ps.files
    SGFHistory -> ps.history
  {-# INLINE getGroupPreference #-}

-- collection of optional group preferences
data GroupPreferences = GroupPreferences
  { timedMessages :: Maybe TimedMessagesGroupPreference,
    directMessages :: Maybe DirectMessagesGroupPreference,
    fullDelete :: Maybe FullDeleteGroupPreference,
    reactions :: Maybe ReactionsGroupPreference,
    voice :: Maybe VoiceGroupPreference,
    files :: Maybe FilesGroupPreference,
    history :: Maybe HistoryGroupPreference
  }
  deriving (Eq, Show)

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
    SGFFullDelete -> prefs {fullDelete = pref}
    SGFReactions -> prefs {reactions = pref}
    SGFVoice -> prefs {voice = pref}
    SGFFiles -> prefs {files = pref}
    SGFHistory -> prefs {history = pref}

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
    reactions :: ReactionsPreference,
    voice :: VoicePreference,
    calls :: CallsPreference
  }
  deriving (Eq, Show)

-- full collection of group preferences defined in the app - it is used to ensure we include all preferences and to simplify processing
-- if some of the preferences are not defined in GroupPreferences, defaults from defaultGroupPrefs are used here.
data FullGroupPreferences = FullGroupPreferences
  { timedMessages :: TimedMessagesGroupPreference,
    directMessages :: DirectMessagesGroupPreference,
    fullDelete :: FullDeleteGroupPreference,
    reactions :: ReactionsGroupPreference,
    voice :: VoiceGroupPreference,
    files :: FilesGroupPreference,
    history :: HistoryGroupPreference
  }
  deriving (Eq, Show)

-- merged preferences of user for a given contact - they differentiate between specific preferences for the contact and global user preferences
data ContactUserPreferences = ContactUserPreferences
  { timedMessages :: ContactUserPreference TimedMessagesPreference,
    fullDelete :: ContactUserPreference FullDeletePreference,
    reactions :: ContactUserPreference ReactionsPreference,
    voice :: ContactUserPreference VoicePreference,
    calls :: ContactUserPreference CallsPreference
  }
  deriving (Eq, Show)

data ContactUserPreference p = ContactUserPreference
  { enabled :: PrefEnabled,
    userPreference :: ContactUserPref p,
    contactPreference :: p
  }
  deriving (Eq, Show)

data ContactUserPref p = CUPContact {preference :: p} | CUPUser {preference :: p}
  deriving (Eq, Show)

toChatPrefs :: FullPreferences -> Preferences
toChatPrefs FullPreferences {timedMessages, fullDelete, reactions, voice, calls} =
  Preferences
    { timedMessages = Just timedMessages,
      fullDelete = Just fullDelete,
      reactions = Just reactions,
      voice = Just voice,
      calls = Just calls
    }

defaultChatPrefs :: FullPreferences
defaultChatPrefs =
  FullPreferences
    { timedMessages = TimedMessagesPreference {allow = FAYes, ttl = Nothing},
      fullDelete = FullDeletePreference {allow = FANo},
      reactions = ReactionsPreference {allow = FAYes},
      voice = VoicePreference {allow = FAYes},
      calls = CallsPreference {allow = FAYes}
    }

emptyChatPrefs :: Preferences
emptyChatPrefs = Preferences Nothing Nothing Nothing Nothing Nothing

defaultGroupPrefs :: FullGroupPreferences
defaultGroupPrefs =
  FullGroupPreferences
    { timedMessages = TimedMessagesGroupPreference {enable = FEOff, ttl = Just 86400},
      directMessages = DirectMessagesGroupPreference {enable = FEOff},
      fullDelete = FullDeleteGroupPreference {enable = FEOff},
      reactions = ReactionsGroupPreference {enable = FEOn},
      voice = VoiceGroupPreference {enable = FEOn},
      files = FilesGroupPreference {enable = FEOn},
      history = HistoryGroupPreference {enable = FEOff}
    }

emptyGroupPrefs :: GroupPreferences
emptyGroupPrefs = GroupPreferences Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data TimedMessagesPreference = TimedMessagesPreference
  { allow :: FeatureAllowed,
    ttl :: Maybe Int
  }
  deriving (Eq, Show)

data FullDeletePreference = FullDeletePreference {allow :: FeatureAllowed}
  deriving (Eq, Show)

data ReactionsPreference = ReactionsPreference {allow :: FeatureAllowed}
  deriving (Eq, Show)

data VoicePreference = VoicePreference {allow :: FeatureAllowed}
  deriving (Eq, Show)

data CallsPreference = CallsPreference {allow :: FeatureAllowed}
  deriving (Eq, Show)

class (Eq (FeaturePreference f), HasField "allow" (FeaturePreference f) FeatureAllowed) => FeatureI f where
  type FeaturePreference (f :: ChatFeature) = p | p -> f
  sFeature :: SChatFeature f
  prefParam :: FeaturePreference f -> Maybe Int

instance HasField "allow" TimedMessagesPreference FeatureAllowed where
  hasField p = (\allow -> p {allow}, p.allow)

instance HasField "allow" FullDeletePreference FeatureAllowed where
  hasField p = (\allow -> p {allow}, p.allow)

instance HasField "allow" ReactionsPreference FeatureAllowed where
  hasField p = (\allow -> p {allow}, p.allow)

instance HasField "allow" VoicePreference FeatureAllowed where
  hasField p = (\allow -> p {allow}, p.allow)

instance HasField "allow" CallsPreference FeatureAllowed where
  hasField p = (\allow -> p {allow}, p.allow)

instance FeatureI 'CFTimedMessages where
  type FeaturePreference 'CFTimedMessages = TimedMessagesPreference
  sFeature = SCFTimedMessages
  prefParam TimedMessagesPreference {ttl} = ttl

instance FeatureI 'CFFullDelete where
  type FeaturePreference 'CFFullDelete = FullDeletePreference
  sFeature = SCFFullDelete
  prefParam _ = Nothing

instance FeatureI 'CFReactions where
  type FeaturePreference 'CFReactions = ReactionsPreference
  sFeature = SCFReactions
  prefParam _ = Nothing

instance FeatureI 'CFVoice where
  type FeaturePreference 'CFVoice = VoicePreference
  sFeature = SCFVoice
  prefParam _ = Nothing

instance FeatureI 'CFCalls where
  type FeaturePreference 'CFCalls = CallsPreference
  sFeature = SCFCalls
  prefParam _ = Nothing

data GroupPreference = GroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show)

data TimedMessagesGroupPreference = TimedMessagesGroupPreference
  { enable :: GroupFeatureEnabled,
    ttl :: Maybe Int
  }
  deriving (Eq, Show)

data DirectMessagesGroupPreference = DirectMessagesGroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show)

data FullDeleteGroupPreference = FullDeleteGroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show)

data ReactionsGroupPreference = ReactionsGroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show)

data VoiceGroupPreference = VoiceGroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show)

data FilesGroupPreference = FilesGroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show)

data HistoryGroupPreference = HistoryGroupPreference
  {enable :: GroupFeatureEnabled}
  deriving (Eq, Show)

class (Eq (GroupFeaturePreference f), HasField "enable" (GroupFeaturePreference f) GroupFeatureEnabled) => GroupFeatureI f where
  type GroupFeaturePreference (f :: GroupFeature) = p | p -> f
  sGroupFeature :: SGroupFeature f
  groupPrefParam :: GroupFeaturePreference f -> Maybe Int

instance HasField "enable" GroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, p.enable)

instance HasField "enable" TimedMessagesGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, p.enable)

instance HasField "enable" DirectMessagesGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, p.enable)

instance HasField "enable" ReactionsGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, p.enable)

instance HasField "enable" FullDeleteGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, p.enable)

instance HasField "enable" VoiceGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, p.enable)

instance HasField "enable" FilesGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, p.enable)

instance HasField "enable" HistoryGroupPreference GroupFeatureEnabled where
  hasField p = (\enable -> p {enable}, p.enable)

instance GroupFeatureI 'GFTimedMessages where
  type GroupFeaturePreference 'GFTimedMessages = TimedMessagesGroupPreference
  sGroupFeature = SGFTimedMessages
  groupPrefParam TimedMessagesGroupPreference {ttl} = ttl

instance GroupFeatureI 'GFDirectMessages where
  type GroupFeaturePreference 'GFDirectMessages = DirectMessagesGroupPreference
  sGroupFeature = SGFDirectMessages
  groupPrefParam _ = Nothing

instance GroupFeatureI 'GFFullDelete where
  type GroupFeaturePreference 'GFFullDelete = FullDeleteGroupPreference
  sGroupFeature = SGFFullDelete
  groupPrefParam _ = Nothing

instance GroupFeatureI 'GFReactions where
  type GroupFeaturePreference 'GFReactions = ReactionsGroupPreference
  sGroupFeature = SGFReactions
  groupPrefParam _ = Nothing

instance GroupFeatureI 'GFVoice where
  type GroupFeaturePreference 'GFVoice = VoiceGroupPreference
  sGroupFeature = SGFVoice
  groupPrefParam _ = Nothing

instance GroupFeatureI 'GFFiles where
  type GroupFeaturePreference 'GFFiles = FilesGroupPreference
  sGroupFeature = SGFFiles
  groupPrefParam _ = Nothing

instance GroupFeatureI 'GFHistory where
  type GroupFeaturePreference 'GFHistory = HistoryGroupPreference
  sGroupFeature = SGFHistory
  groupPrefParam _ = Nothing

groupPrefStateText :: HasField "enable" p GroupFeatureEnabled => GroupFeature -> p -> Maybe Int -> Text
groupPrefStateText feature pref param =
  let enabled = getField @"enable" pref
      paramText = if enabled == FEOn then groupParamText_ feature param else ""
   in groupFeatureNameText feature <> ": " <> safeDecodeUtf8 (strEncode enabled) <> paramText

groupParamText_ :: GroupFeature -> Maybe Int -> Text
groupParamText_ feature param = case feature of
  GFTimedMessages -> maybe "" (\p -> " (" <> timedTTLText p <> ")") param
  _ -> ""

groupPreferenceText :: forall f. GroupFeatureI f => GroupFeaturePreference f -> Text
groupPreferenceText pref =
  let feature = toGroupFeature $ sGroupFeature @f
   in groupPrefStateText feature pref $ groupPrefParam pref

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
      reactions = pref SCFReactions,
      voice = pref SCFVoice,
      calls = pref SCFCalls
    }
  where
    pref :: SChatFeature f -> FeaturePreference f
    pref f =
      let sel = chatPrefSel f
       in fromMaybe (getPreference f defaultChatPrefs) $ (contactPrefs >>= sel) <|> (userPreferences >>= sel)

mergeGroupPreferences :: Maybe GroupPreferences -> FullGroupPreferences
mergeGroupPreferences groupPreferences =
  FullGroupPreferences
    { timedMessages = pref SGFTimedMessages,
      directMessages = pref SGFDirectMessages,
      fullDelete = pref SGFFullDelete,
      reactions = pref SGFReactions,
      voice = pref SGFVoice,
      files = pref SGFFiles,
      history = pref SGFHistory
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
      reactions = pref SGFReactions,
      voice = pref SGFVoice,
      files = pref SGFFiles,
      history = pref SGFHistory
    }
  where
    pref :: SGroupFeature f -> Maybe (GroupFeaturePreference f)
    pref f = Just $ getGroupPreference f groupPreferences

data PrefEnabled = PrefEnabled {forUser :: Bool, forContact :: Bool}
  deriving (Eq, Show)

prefEnabled :: FeatureI f => Bool -> FeaturePreference f -> FeaturePreference f -> PrefEnabled
prefEnabled asymmetric user contact = case (getField @"allow" user, getField @"allow" contact) of
  (FAAlways, FANo) -> PrefEnabled {forUser = False, forContact = asymmetric}
  (FANo, FAAlways) -> PrefEnabled {forUser = asymmetric, forContact = False}
  (_, FANo) -> PrefEnabled False False
  (FANo, _) -> PrefEnabled False False
  _ -> PrefEnabled True True

prefStateText :: ChatFeature -> FeatureAllowed -> Maybe Int -> Text
prefStateText feature allowed param = case allowed of
  FANo -> "cancelled " <> chatFeatureNameText feature
  _ -> "offered " <> chatFeatureNameText feature <> paramText_ feature param

featureStateText :: ChatFeature -> PrefEnabled -> Maybe Int -> Text
featureStateText feature enabled param =
  chatFeatureNameText feature <> ": " <> prefEnabledToText feature enabled param <> case enabled of
    PrefEnabled {forUser = True} -> paramText_ feature param
    _ -> ""

paramText_ :: ChatFeature -> Maybe Int -> Text
paramText_ feature param = case feature of
  CFTimedMessages -> maybe "" (\p -> " (" <> timedTTLText p <> ")") param
  _ -> ""

prefEnabledToText :: ChatFeature -> PrefEnabled -> Maybe Int -> Text
prefEnabledToText f enabled param = case enabled of
  PrefEnabled True True -> enabledStr
  PrefEnabled False False -> "off"
  PrefEnabled {forUser = True, forContact = False} -> enabledStr <> " for you"
  PrefEnabled {forUser = False, forContact = True} -> enabledStr <> " for contact"
  where
    enabledStr = case f of
      CFTimedMessages -> if isJust param then "enabled" else "allowed"
      _ -> "enabled"

preferenceText :: forall f. FeatureI f => FeaturePreference f -> Text
preferenceText p =
  let feature = chatFeature $ sFeature @f
      allowed = getField @"allow" p
      paramText = if allowed == FAAlways || allowed == FAYes then paramText_ feature (prefParam p) else ""
   in safeDecodeUtf8 (strEncode allowed) <> paramText

featureState :: FeatureI f => ContactUserPreference (FeaturePreference f) -> (PrefEnabled, Maybe Int)
featureState ContactUserPreference {enabled, userPreference} =
  let param = if forUser enabled then prefParam $ preference userPreference else Nothing
   in (enabled, param)

preferenceState :: FeatureI f => FeaturePreference f -> (FeatureAllowed, Maybe Int)
preferenceState pref =
  let allow = getField @"allow" pref
      param = if allow == FAAlways || allow == FAYes then prefParam pref else Nothing
   in (allow, param)

getContactUserPreference :: SChatFeature f -> ContactUserPreferences -> ContactUserPreference (FeaturePreference f)
getContactUserPreference f ps = case f of
  SCFTimedMessages -> ps.timedMessages
  SCFFullDelete -> ps.fullDelete
  SCFReactions -> ps.reactions
  SCFVoice -> ps.voice
  SCFCalls -> ps.calls

$(J.deriveJSON (enumJSON $ dropPrefix "CF") ''ChatFeature)

$(J.deriveJSON (enumJSON $ dropPrefix "GF") ''GroupFeature)

$(J.deriveJSON defaultJSON ''TimedMessagesPreference)

$(J.deriveJSON defaultJSON ''FullDeletePreference)

$(J.deriveJSON defaultJSON ''ReactionsPreference)

$(J.deriveJSON defaultJSON ''VoicePreference)

$(J.deriveJSON defaultJSON ''CallsPreference)

$(J.deriveJSON defaultJSON ''Preferences)

instance ToField Preferences where
  toField = toField . encodeJSON

instance FromField Preferences where
  fromField = fromTextField_ decodeJSON

$(J.deriveJSON defaultJSON ''GroupPreference)

$(J.deriveJSON defaultJSON ''TimedMessagesGroupPreference)

$(J.deriveJSON defaultJSON ''DirectMessagesGroupPreference)

$(J.deriveJSON defaultJSON ''ReactionsGroupPreference)

$(J.deriveJSON defaultJSON ''FullDeleteGroupPreference)

$(J.deriveJSON defaultJSON ''VoiceGroupPreference)

$(J.deriveJSON defaultJSON ''FilesGroupPreference)

$(J.deriveJSON defaultJSON ''HistoryGroupPreference)

$(J.deriveJSON defaultJSON ''GroupPreferences)

instance ToField GroupPreferences where
  toField = toField . encodeJSON

instance FromField GroupPreferences where
  fromField = fromTextField_ decodeJSON

$(J.deriveJSON defaultJSON ''FullPreferences)

$(J.deriveJSON defaultJSON ''FullGroupPreferences)

$(J.deriveJSON defaultJSON ''PrefEnabled)

instance FromJSON p => FromJSON (ContactUserPref p) where
  parseJSON = $(J.mkParseJSON (sumTypeJSON $ dropPrefix "CUP") ''ContactUserPref)

instance ToJSON p => ToJSON (ContactUserPref p) where
  toJSON = $(J.mkToJSON (sumTypeJSON $ dropPrefix "CUP") ''ContactUserPref)
  toEncoding = $(J.mkToEncoding (sumTypeJSON $ dropPrefix "CUP") ''ContactUserPref)

instance FromJSON p => FromJSON (ContactUserPreference p) where
  parseJSON = $(J.mkParseJSON defaultJSON ''ContactUserPreference)

instance ToJSON p => ToJSON (ContactUserPreference p) where
  toJSON = $(J.mkToJSON defaultJSON ''ContactUserPreference)
  toEncoding = $(J.mkToEncoding defaultJSON ''ContactUserPreference)

$(J.deriveJSON defaultJSON ''ContactUserPreferences)
