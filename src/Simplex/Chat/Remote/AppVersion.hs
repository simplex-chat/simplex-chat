{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Remote.AppVersion
  ( AppVersionRange (minVersion, maxVersion),
    pattern AppVersionRange,
    AppVersion (..),
    pattern AppCompatible,
    mkAppVersionRange,
    compatibleAppVersion,
    isAppCompatible,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.TH as JQ
import qualified Data.Text as T
import Data.Version (parseVersion, showVersion)
import qualified Data.Version as V
import Simplex.Messaging.Parsers (defaultJSON)
import Text.ParserCombinators.ReadP (readP_to_S)

newtype AppVersion = AppVersion {appVersion :: V.Version}
  deriving (Eq, Ord, Show)

instance ToJSON AppVersion where
  toJSON (AppVersion v) = J.String . T.pack $ showVersion v
  toEncoding (AppVersion v) = JE.text . T.pack $ showVersion v

instance FromJSON AppVersion where
  parseJSON = J.withText "AppVersion" $ parse . T.unpack
    where
      parse s = case filter (null . snd) $ readP_to_S parseVersion s of
        (v, _) : _ -> pure $ AppVersion v
        _ -> fail $ "bad AppVersion: " <> s

data AppVersionRange = AppVRange
  { minVersion :: AppVersion,
    maxVersion :: AppVersion
  }
  deriving (Show)

pattern AppVersionRange :: AppVersion -> AppVersion -> AppVersionRange
pattern AppVersionRange v1 v2 <- AppVRange v1 v2

{-# COMPLETE AppVersionRange #-}

mkAppVersionRange :: AppVersion -> AppVersion -> AppVersionRange
mkAppVersionRange v1 v2
  | v1 <= v2 = AppVRange v1 v2
  | otherwise = error "invalid version range"

newtype AppCompatible a = AppCompatible_ a

pattern AppCompatible :: a -> AppCompatible a
pattern AppCompatible a <- AppCompatible_ a

{-# COMPLETE AppCompatible #-}

isAppCompatible :: AppVersion -> AppVersionRange -> Bool
isAppCompatible v (AppVRange v1 v2) = v1 <= v && v <= v2

isCompatibleAppRange :: AppVersionRange -> AppVersionRange -> Bool
isCompatibleAppRange (AppVRange min1 max1) (AppVRange min2 max2) = min1 <= max2 && min2 <= max1

compatibleAppVersion :: AppVersionRange -> AppVersionRange -> Maybe (AppCompatible AppVersion)
compatibleAppVersion vr1 vr2 =
  min (maxVersion vr1) (maxVersion vr2) `mkCompatibleIf` isCompatibleAppRange vr1 vr2

mkCompatibleIf :: AppVersion -> Bool -> Maybe (AppCompatible AppVersion)
v `mkCompatibleIf` cond = if cond then Just $ AppCompatible_ v else Nothing

$(JQ.deriveJSON defaultJSON ''AppVersionRange)
