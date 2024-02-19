{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.AppSettings where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), (.:?))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Simplex.Messaging.Client (NetworkConfig, defaultNetworkConfig)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON)
import Simplex.Messaging.Util (catchAll_)

data AppPlatform = APIOS | APAndroid | APDesktop
  deriving (Show)

data AppSettings = AppSettings
  { appPlatform :: Maybe AppPlatform,
    networkConfig :: Maybe NetworkConfig,
    privacyEncryptLocalFiles :: Maybe Bool,
    privacyAcceptImages :: Maybe Bool,
    privacyLinkPreviews :: Maybe Bool,
    privacyShowChatPreviews :: Maybe Bool,
    privacySaveLastDraft :: Maybe Bool,
    privacyProtectScreen :: Maybe Bool,
    webrtcPolicyRelay :: Maybe Bool,
    webrtcICEServers :: Maybe [Text],
    callKitCallsInRecents :: Maybe Bool, -- iOS only
    confirmRemoteSessions :: Maybe Bool,
    connectRemoteViaMulticast :: Maybe Bool,
    connectRemoteViaMulticastAuto :: Maybe Bool,
    developerTools :: Maybe Bool
  }
  deriving (Show)

defaultAppSettings :: AppSettings
defaultAppSettings =
  AppSettings
    { appPlatform = Nothing,
      networkConfig = Just defaultNetworkConfig,
      privacyEncryptLocalFiles = Just True,
      privacyAcceptImages = Just True,
      privacyLinkPreviews = Just True,
      privacyShowChatPreviews = Just True,
      privacySaveLastDraft = Just True,
      privacyProtectScreen = Just False,
      webrtcPolicyRelay = Just True,
      webrtcICEServers = Just [],
      callKitCallsInRecents = Nothing,
      confirmRemoteSessions = Just False,
      connectRemoteViaMulticast = Just True,
      connectRemoteViaMulticastAuto = Just True,
      developerTools = Just False
    }

defaultParseAppSettings :: AppSettings
defaultParseAppSettings =
  AppSettings
    { appPlatform = Nothing,
      networkConfig = Nothing,
      privacyEncryptLocalFiles = Nothing,
      privacyAcceptImages = Nothing,
      privacyLinkPreviews = Nothing,
      privacyShowChatPreviews = Nothing,
      privacySaveLastDraft = Nothing,
      privacyProtectScreen = Nothing,
      webrtcPolicyRelay = Nothing,
      webrtcICEServers = Nothing,
      callKitCallsInRecents = Nothing,
      confirmRemoteSessions = Nothing,
      connectRemoteViaMulticast = Nothing,
      connectRemoteViaMulticastAuto = Nothing,
      developerTools = Nothing
    }

combineAppSettings :: AppSettings -> AppSettings -> AppSettings
combineAppSettings platformDefaults storedSettings =
  AppSettings
    { appPlatform = p appPlatform, 
      networkConfig = p networkConfig,
      privacyEncryptLocalFiles = p privacyEncryptLocalFiles,
      privacyAcceptImages = p privacyAcceptImages,
      privacyLinkPreviews = p privacyLinkPreviews,
      privacyShowChatPreviews = p privacyShowChatPreviews,
      privacySaveLastDraft = p privacySaveLastDraft,
      privacyProtectScreen = p privacyProtectScreen,
      webrtcPolicyRelay = p webrtcPolicyRelay,
      webrtcICEServers = p webrtcICEServers,
      callKitCallsInRecents = p callKitCallsInRecents,
      confirmRemoteSessions = p confirmRemoteSessions,
      connectRemoteViaMulticast = p connectRemoteViaMulticast,
      connectRemoteViaMulticastAuto = p connectRemoteViaMulticastAuto,
      developerTools = p developerTools
    }
  where
    p :: (AppSettings -> Maybe a) -> Maybe a
    p sel = sel storedSettings <|> sel platformDefaults <|> sel defaultAppSettings

$(JQ.deriveJSON (enumJSON $ dropPrefix "AP") ''AppPlatform)

$(JQ.deriveToJSON defaultJSON ''AppSettings)

instance FromJSON AppSettings where
  parseJSON (J.Object v) = do
    appPlatform <- p "appPlatform"
    networkConfig <- p "networkConfig"
    privacyEncryptLocalFiles <- p "privacyEncryptLocalFiles"
    privacyAcceptImages <- p "privacyAcceptImages"
    privacyLinkPreviews <- p "privacyLinkPreviews"
    privacyShowChatPreviews <- p "privacyShowChatPreviews"
    privacySaveLastDraft <- p "privacySaveLastDraft"
    privacyProtectScreen <- p "privacyProtectScreen"
    webrtcPolicyRelay <- p "webrtcPolicyRelay"
    webrtcICEServers <- p "webrtcICEServers"
    callKitCallsInRecents <- p "callKitCallsInRecents"
    confirmRemoteSessions <- p "confirmRemoteSessions"
    connectRemoteViaMulticast <- p "connectRemoteViaMulticast"
    connectRemoteViaMulticastAuto <- p "connectRemoteViaMulticastAuto"
    developerTools <- p "developerTools"
    pure
      AppSettings
        { appPlatform,
          networkConfig,
          privacyEncryptLocalFiles,
          privacyAcceptImages,
          privacyLinkPreviews,
          privacyShowChatPreviews,
          privacySaveLastDraft,
          privacyProtectScreen,
          webrtcPolicyRelay,
          webrtcICEServers,
          callKitCallsInRecents,
          confirmRemoteSessions,
          connectRemoteViaMulticast,
          connectRemoteViaMulticastAuto,
          developerTools
        }
    where
      p key = v .:? key <|> pure Nothing
  parseJSON _ = pure defaultParseAppSettings

readAppSettings :: FilePath -> Maybe AppSettings -> IO AppSettings
readAppSettings f platformDefaults =
  combineAppSettings (fromMaybe defaultAppSettings platformDefaults) . fromMaybe defaultParseAppSettings
    <$> (J.decodeFileStrict f `catchAll_` pure Nothing)
