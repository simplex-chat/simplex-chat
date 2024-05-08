{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Types.UITheme where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Simplex.Chat.Types.Util
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, fromTextField_)
import Simplex.Messaging.Util ((<$?>))

data UIThemes = UIThemes
  { light :: Maybe UITheme,
    dark :: Maybe UITheme,
    simplex :: Maybe UITheme
  }
  deriving (Eq, Show)

data UITheme = UITheme
  { base :: ThemeColorScheme,
    wallpaper :: Maybe ChatWallpaper,
    colors :: UIColors
  }
  deriving (Eq, Show)

data UIColorMode = UCMLight | UCMDark
  deriving (Eq, Show)

data UIThemeOverrides = UIThemeOverrides
  { light :: Maybe UIThemeOverride,
    dark :: Maybe UIThemeOverride
  }
  deriving (Eq, Show)

data UIThemeOverride = UIThemeOverride
  { mode :: UIColorMode,
    wallpaper :: Maybe ChatWallpaper,
    colors :: UIColors
  }
  deriving (Eq, Show)

data ThemeColorScheme = TCSLight | TCSDark | TCSSimplex
  deriving (Eq, Show)

data UIColorScheme
  = UCSSystem
  | UCSLight
  | UCSDark
  | UCSSimplex
  deriving (Show)

data DarkColorScheme = DCSDark | DCSSimplex
  deriving (Show)

instance StrEncoding ThemeColorScheme where
  strEncode = \case
    TCSLight -> "LIGHT"
    TCSDark -> "DARK"
    TCSSimplex -> "SIMPLEX"
  strDecode = \case
    "LIGHT" -> Right TCSLight
    "DARK" -> Right TCSDark
    "SIMPLEX" -> Right TCSSimplex
    _ -> Left "bad ColorScheme"
  strP = strDecode <$?> A.takeTill (== ' ')

instance FromJSON ThemeColorScheme where
  parseJSON = strParseJSON "ThemeColorScheme"

instance ToJSON ThemeColorScheme where
  toJSON = strToJSON
  toEncoding = strToJEncoding

data ChatWallpaper = ChatWallpaper
  { preset :: Maybe ChatWallpaperPreset,
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

data ChatWallpaperPreset
  = CWPKids
  | CWPCats
  | CWPPets
  | CWPFlowers
  | CWPHearts
  | CWPSocial
  | CWPTravel
  | CWPInternet
  | CWPSpace
  | CWPSchool
  deriving (Eq, Show)

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

$(JQ.deriveJSON (enumJSON $ dropPrefix "CWP") ''ChatWallpaperPreset)

$(JQ.deriveJSON defaultJSON ''ChatWallpaper)

$(JQ.deriveJSON defaultJSON ''UIColors)

$(JQ.deriveJSON defaultJSON ''UIThemeOverride)

$(JQ.deriveJSON defaultJSON ''UIThemeOverrides)

$(JQ.deriveJSON defaultJSON ''UITheme)

$(JQ.deriveJSON defaultJSON ''UIThemes)

instance ToField UIThemeOverrides where
  toField = toField . encodeJSON

instance FromField UIThemeOverrides where
  fromField = fromTextField_ $ Just . fromMaybe (UIThemeOverrides Nothing Nothing) . decodeJSON
