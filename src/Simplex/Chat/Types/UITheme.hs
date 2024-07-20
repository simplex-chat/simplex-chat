{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Types.UITheme where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.TH as JQ
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Simplex.Chat.Types.Util
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fromTextField_)
import Simplex.Messaging.Util (decodeJSON, encodeJSON)

data UITheme = UITheme
  { themeId :: Text,
    base :: ThemeColorScheme,
    wallpaper :: Maybe ChatWallpaper,
    colors :: UIColors
  }
  deriving (Eq, Show)

data UIColorMode = UCMLight | UCMDark
  deriving (Eq, Show)

data UIThemeEntityOverrides = UIThemeEntityOverrides
  { light :: Maybe UIThemeEntityOverride,
    dark :: Maybe UIThemeEntityOverride
  }
  deriving (Eq, Show)

data UIThemeEntityOverride = UIThemeEntityOverride
  { mode :: UIColorMode,
    wallpaper :: Maybe ChatWallpaper,
    colors :: UIColors
  }
  deriving (Eq, Show)

data DarkColorScheme = DCSDark | DCSBlack | DCSSimplex
  deriving (Eq, Ord, Show)

data ThemeColorScheme = TCSLight | TCSDark DarkColorScheme
  deriving (Eq, Ord, Show)

data UIColorScheme = UCSSystem | UCSFixed ThemeColorScheme
  deriving (Eq, Ord, Show)

instance TextEncoding DarkColorScheme where
  textEncode = \case
    DCSDark -> "DARK"
    DCSBlack -> "BLACK"
    DCSSimplex -> "SIMPLEX"
  textDecode s =
    Just $ case s of
      "DARK" -> DCSDark
      "BLACK" -> DCSBlack
      "SIMPLEX" -> DCSSimplex
      _ -> DCSDark

instance TextEncoding ThemeColorScheme where
  textEncode = \case
    TCSLight -> "LIGHT"
    TCSDark s -> textEncode s
  textDecode = \case
    "LIGHT" -> Just TCSLight
    s -> TCSDark <$> textDecode s

instance TextEncoding UIColorScheme where
  textEncode = \case
    UCSSystem -> "SYSTEM"
    UCSFixed s -> textEncode s
  textDecode = \case
    "SYSTEM" -> Just UCSSystem
    s -> UCSFixed <$> textDecode s

instance FromJSON DarkColorScheme where
  parseJSON = textParseJSON "DarkColorScheme"

instance ToJSON DarkColorScheme where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance FromJSON ThemeColorScheme where
  parseJSON = textParseJSON "ThemeColorScheme"

instance ToJSON ThemeColorScheme where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance FromJSON UIColorScheme where
  parseJSON = textParseJSON "UIColorScheme"

instance ToJSON UIColorScheme where
  toJSON = J.String . textEncode
  toEncoding = JE.text . textEncode

instance J.FromJSONKey ThemeColorScheme where
  fromJSONKey = J.FromJSONKeyText $ fromMaybe (TCSDark DCSDark) . textDecode

instance J.ToJSONKey ThemeColorScheme where
  toJSONKey = J.ToJSONKeyText (JK.fromText . textEncode) (JE.text . textEncode)

data ChatWallpaper = ChatWallpaper
  { preset :: Maybe Text,
    imageFile :: Maybe FilePath,
    background :: Maybe UIColor,
    tint :: Maybe UIColor,
    scaleType :: Maybe ChatWallpaperScale,
    scale :: Maybe Double
  }
  deriving (Eq, Show)

data ChatWallpaperScale = CWSFill | CWSFit | CWSRepeat
  deriving (Eq, Show)

data UIColors = UIColors
  { accent :: Maybe UIColor,
    accentVariant :: Maybe UIColor,
    secondary :: Maybe UIColor,
    secondaryVariant :: Maybe UIColor,
    background :: Maybe UIColor,
    menus :: Maybe UIColor,
    title :: Maybe UIColor,
    accentVariant2 :: Maybe UIColor,
    sentMessage :: Maybe UIColor,
    sentReply :: Maybe UIColor,
    receivedMessage :: Maybe UIColor,
    receivedReply :: Maybe UIColor
  }
  deriving (Eq, Show)

defaultUIColors :: UIColors
defaultUIColors = UIColors Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

newtype UIColor = UIColor String
  deriving (Eq, Show)

instance FromJSON UIColor where
  parseJSON v = toColor =<< J.parseJSON v
    where
      toColor s@('#' : cs)
        | length cs == 8 && all hexDigit cs = pure $ UIColor s
      toColor _ = fail "bad UIColor"
      hexDigit c = (c >= '0' && c <= '9') || (let c' = toLower c in c' >= 'a' && c' <= 'f')

instance ToJSON UIColor where
  toJSON (UIColor t) = J.toJSON t
  toEncoding (UIColor t) = J.toEncoding t

$(JQ.deriveJSON (enumJSON $ dropPrefix "UCM") ''UIColorMode)

$(JQ.deriveJSON (enumJSON $ dropPrefix "CWS") ''ChatWallpaperScale)

$(JQ.deriveJSON defaultJSON ''ChatWallpaper)

$(JQ.deriveJSON defaultJSON ''UIColors)

$(JQ.deriveJSON defaultJSON ''UIThemeEntityOverride)

$(JQ.deriveJSON defaultJSON ''UIThemeEntityOverrides)

$(JQ.deriveJSON defaultJSON ''UITheme)

instance ToField UIThemeEntityOverrides where
  toField = toField . encodeJSON

instance FromField UIThemeEntityOverrides where
  fromField = fromTextField_ $ Just . fromMaybe (UIThemeEntityOverrides Nothing Nothing) . decodeJSON
