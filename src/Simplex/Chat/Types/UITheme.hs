{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Types.UITheme where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Simplex.Chat.Types.Util
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fromTextField_)

data UITheme = UITheme
  { themeId :: Text,
    base :: Text,
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

data UIColorScheme
  = UCSSystem
  | UCSLight
  | UCSDark
  | UCSBlack
  | UCSSimplex
  deriving (Show)

data DarkColorScheme = DCSDark | DCSBlack | DCSSimplex
  deriving (Show)

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
    sentMessage :: Maybe UIColor,
    receivedMessage :: Maybe UIColor
  }
  deriving (Eq, Show)

defaultUIColors :: UIColors
defaultUIColors = UIColors Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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

$(JQ.deriveJSON (enumJSON $ dropPrefix "DCS") ''DarkColorScheme)

$(JQ.deriveJSON (enumJSON $ dropPrefix "UCM") ''UIColorMode)

$(JQ.deriveJSON (enumJSON $ dropPrefix "UCS") ''UIColorScheme)

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
