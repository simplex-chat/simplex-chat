{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.AppSettings where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), (.:?))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Simplex.Chat.Types.UITheme
import Simplex.Messaging.Client (NetworkConfig, defaultNetworkConfig)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON)
import Simplex.Messaging.Util (catchAll_)

data AppPlatform = APIOS | APAndroid | APDesktop deriving (Show)

data NotificationMode = NMOff | NMPeriodic | NMInstant deriving (Show)

data NotificationPreviewMode = NPMHidden | NPMContact | NPMMessage deriving (Show)

data LockScreenCalls = LSCDisable | LSCShow | LSCAccept deriving (Show)

data AppSettings = AppSettings
  { appPlatform :: Maybe AppPlatform,
    networkConfig :: Maybe NetworkConfig,
    networkProxy :: Maybe NetworkProxy,
    privacyEncryptLocalFiles :: Maybe Bool,
    privacyAskToApproveRelays :: Maybe Bool,
    privacyAcceptImages :: Maybe Bool,
    privacyLinkPreviews :: Maybe Bool,
    privacyShowChatPreviews :: Maybe Bool,
    privacySaveLastDraft :: Maybe Bool,
    privacyProtectScreen :: Maybe Bool,
    privacyMediaBlurRadius :: Maybe Int,
    notificationMode :: Maybe NotificationMode,
    notificationPreviewMode :: Maybe NotificationPreviewMode,
    webrtcPolicyRelay :: Maybe Bool,
    webrtcICEServers :: Maybe [Text],
    confirmRemoteSessions :: Maybe Bool,
    connectRemoteViaMulticast :: Maybe Bool,
    connectRemoteViaMulticastAuto :: Maybe Bool,
    developerTools :: Maybe Bool,
    confirmDBUpgrades :: Maybe Bool,
    androidCallOnLockScreen :: Maybe LockScreenCalls,
    iosCallKitEnabled :: Maybe Bool,
    iosCallKitCallsInRecents :: Maybe Bool,
    uiProfileImageCornerRadius :: Maybe Double,
    uiChatItemRoundness :: Maybe Double,
    uiChatItemTail :: Maybe Bool,
    uiColorScheme :: Maybe UIColorScheme,
    uiDarkColorScheme :: Maybe DarkColorScheme,
    uiCurrentThemeIds :: Maybe (Map ThemeColorScheme Text),
    uiThemes :: Maybe [UITheme],
    oneHandUI :: Maybe Bool,
    chatBottomBar :: Maybe Bool
  }
  deriving (Show)

data NetworkProxy = NetworkProxy
  { host :: Text,
    port :: Int,
    auth :: NetworkProxyAuth,
    username :: Text,
    password :: Text
  }
  deriving (Show)

data NetworkProxyAuth = NPAUsername | NPAIsolate
  deriving (Show)

defaultAppSettings :: AppSettings
defaultAppSettings =
  AppSettings
    { appPlatform = Nothing,
      networkConfig = Just defaultNetworkConfig,
      networkProxy = Nothing,
      privacyEncryptLocalFiles = Just True,
      privacyAskToApproveRelays = Just True,
      privacyAcceptImages = Just True,
      privacyLinkPreviews = Just True,
      privacyShowChatPreviews = Just True,
      privacySaveLastDraft = Just True,
      privacyProtectScreen = Just False,
      privacyMediaBlurRadius = Just 0,
      notificationMode = Just NMInstant,
      notificationPreviewMode = Just NPMMessage,
      webrtcPolicyRelay = Just True,
      webrtcICEServers = Just [],
      confirmRemoteSessions = Just False,
      connectRemoteViaMulticast = Just True,
      connectRemoteViaMulticastAuto = Just True,
      developerTools = Just False,
      confirmDBUpgrades = Just False,
      androidCallOnLockScreen = Just LSCShow,
      iosCallKitEnabled = Just True,
      iosCallKitCallsInRecents = Just False,
      uiProfileImageCornerRadius = Just 22.5,
      uiChatItemRoundness = Just 0.75,
      uiChatItemTail = Just True,
      uiColorScheme = Just UCSSystem,
      uiDarkColorScheme = Just DCSSimplex,
      uiCurrentThemeIds = Nothing,
      uiThemes = Nothing,
      oneHandUI = Just True,
      chatBottomBar = Just True
    }

defaultParseAppSettings :: AppSettings
defaultParseAppSettings =
  AppSettings
    { appPlatform = Nothing,
      networkConfig = Nothing,
      networkProxy = Nothing,
      privacyEncryptLocalFiles = Nothing,
      privacyAskToApproveRelays = Nothing,
      privacyAcceptImages = Nothing,
      privacyLinkPreviews = Nothing,
      privacyShowChatPreviews = Nothing,
      privacySaveLastDraft = Nothing,
      privacyProtectScreen = Nothing,
      privacyMediaBlurRadius = Nothing,
      notificationMode = Nothing,
      notificationPreviewMode = Nothing,
      webrtcPolicyRelay = Nothing,
      webrtcICEServers = Nothing,
      confirmRemoteSessions = Nothing,
      connectRemoteViaMulticast = Nothing,
      connectRemoteViaMulticastAuto = Nothing,
      developerTools = Nothing,
      confirmDBUpgrades = Nothing,
      androidCallOnLockScreen = Nothing,
      iosCallKitEnabled = Nothing,
      iosCallKitCallsInRecents = Nothing,
      uiProfileImageCornerRadius = Nothing,
      uiChatItemRoundness = Nothing,
      uiChatItemTail = Nothing,
      uiColorScheme = Nothing,
      uiDarkColorScheme = Nothing,
      uiCurrentThemeIds = Nothing,
      uiThemes = Nothing,
      oneHandUI = Nothing,
      chatBottomBar = Nothing
    }

combineAppSettings :: AppSettings -> AppSettings -> AppSettings
combineAppSettings platformDefaults storedSettings =
  AppSettings
    { appPlatform = p appPlatform,
      networkConfig = p networkConfig,
      networkProxy = p networkProxy,
      privacyEncryptLocalFiles = p privacyEncryptLocalFiles,
      privacyAskToApproveRelays = p privacyAskToApproveRelays,
      privacyAcceptImages = p privacyAcceptImages,
      privacyLinkPreviews = p privacyLinkPreviews,
      privacyShowChatPreviews = p privacyShowChatPreviews,
      privacySaveLastDraft = p privacySaveLastDraft,
      privacyProtectScreen = p privacyProtectScreen,
      privacyMediaBlurRadius = p privacyMediaBlurRadius,
      notificationMode = p notificationMode,
      notificationPreviewMode = p notificationPreviewMode,
      webrtcPolicyRelay = p webrtcPolicyRelay,
      webrtcICEServers = p webrtcICEServers,
      confirmRemoteSessions = p confirmRemoteSessions,
      connectRemoteViaMulticast = p connectRemoteViaMulticast,
      connectRemoteViaMulticastAuto = p connectRemoteViaMulticastAuto,
      developerTools = p developerTools,
      confirmDBUpgrades = p confirmDBUpgrades,
      iosCallKitEnabled = p iosCallKitEnabled,
      iosCallKitCallsInRecents = p iosCallKitCallsInRecents,
      androidCallOnLockScreen = p androidCallOnLockScreen,
      uiProfileImageCornerRadius = p uiProfileImageCornerRadius,
      uiChatItemRoundness = p uiChatItemRoundness,
      uiChatItemTail = p uiChatItemTail,
      uiColorScheme = p uiColorScheme,
      uiDarkColorScheme = p uiDarkColorScheme,
      uiCurrentThemeIds = p uiCurrentThemeIds,
      uiThemes = p uiThemes,
      oneHandUI = p oneHandUI,
      chatBottomBar = p chatBottomBar
    }
  where
    p :: (AppSettings -> Maybe a) -> Maybe a
    p sel = sel storedSettings <|> sel platformDefaults <|> sel defaultAppSettings

$(JQ.deriveJSON (enumJSON $ dropPrefix "AP") ''AppPlatform)

$(JQ.deriveJSON (enumJSON $ dropPrefix "NM") ''NotificationMode)

$(JQ.deriveJSON (enumJSON $ dropPrefix "NPM") ''NotificationPreviewMode)

$(JQ.deriveJSON (enumJSON $ dropPrefix "LSC") ''LockScreenCalls)

$(JQ.deriveJSON (enumJSON $ dropPrefix "NPA") ''NetworkProxyAuth)

$(JQ.deriveJSON defaultJSON ''NetworkProxy)

$(JQ.deriveToJSON defaultJSON ''AppSettings)

instance FromJSON AppSettings where
  parseJSON (J.Object v) = do
    appPlatform <- p "appPlatform"
    networkConfig <- p "networkConfig"
    networkProxy <- p "networkProxy"
    privacyEncryptLocalFiles <- p "privacyEncryptLocalFiles"
    privacyAskToApproveRelays <- p "privacyAskToApproveRelays"
    privacyAcceptImages <- p "privacyAcceptImages"
    privacyLinkPreviews <- p "privacyLinkPreviews"
    privacyShowChatPreviews <- p "privacyShowChatPreviews"
    privacySaveLastDraft <- p "privacySaveLastDraft"
    privacyProtectScreen <- p "privacyProtectScreen"
    privacyMediaBlurRadius <- p "privacyMediaBlurRadius"
    notificationMode <- p "notificationMode"
    notificationPreviewMode <- p "notificationPreviewMode"
    webrtcPolicyRelay <- p "webrtcPolicyRelay"
    webrtcICEServers <- p "webrtcICEServers"
    confirmRemoteSessions <- p "confirmRemoteSessions"
    connectRemoteViaMulticast <- p "connectRemoteViaMulticast"
    connectRemoteViaMulticastAuto <- p "connectRemoteViaMulticastAuto"
    developerTools <- p "developerTools"
    confirmDBUpgrades <- p "confirmDBUpgrades"
    iosCallKitEnabled <- p "iosCallKitEnabled"
    iosCallKitCallsInRecents <- p "iosCallKitCallsInRecents"
    androidCallOnLockScreen <- p "androidCallOnLockScreen"
    uiProfileImageCornerRadius <- p "uiProfileImageCornerRadius"
    uiChatItemRoundness <- p "uiChatItemRoundness"
    uiChatItemTail <- p "uiChatItemTail"
    uiColorScheme <- p "uiColorScheme"
    uiDarkColorScheme <- p "uiDarkColorScheme"
    uiCurrentThemeIds <- p "uiCurrentThemeIds"
    uiThemes <- p "uiThemes"
    oneHandUI <- p "oneHandUI"
    chatBottomBar <- p "chatBottomBar"
    pure
      AppSettings
        { appPlatform,
          networkConfig,
          networkProxy,
          privacyEncryptLocalFiles,
          privacyAskToApproveRelays,
          privacyAcceptImages,
          privacyLinkPreviews,
          privacyShowChatPreviews,
          privacySaveLastDraft,
          privacyProtectScreen,
          privacyMediaBlurRadius,
          notificationMode,
          notificationPreviewMode,
          webrtcPolicyRelay,
          webrtcICEServers,
          confirmRemoteSessions,
          connectRemoteViaMulticast,
          connectRemoteViaMulticastAuto,
          developerTools,
          confirmDBUpgrades,
          iosCallKitEnabled,
          iosCallKitCallsInRecents,
          androidCallOnLockScreen,
          uiProfileImageCornerRadius,
          uiChatItemRoundness,
          uiChatItemTail,
          uiColorScheme,
          uiDarkColorScheme,
          uiCurrentThemeIds,
          uiThemes,
          oneHandUI,
          chatBottomBar
        }
    where
      p key = v .:? key <|> pure Nothing
  parseJSON _ = pure defaultParseAppSettings

readAppSettings :: FilePath -> Maybe AppSettings -> IO AppSettings
readAppSettings f platformDefaults =
  combineAppSettings (fromMaybe defaultAppSettings platformDefaults) . fromMaybe defaultParseAppSettings
    <$> (J.decodeFileStrict f `catchAll_` pure Nothing)
