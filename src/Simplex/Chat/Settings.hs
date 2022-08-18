{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.Settings where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Type.Equality
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (fromTextField_)
import Simplex.Messaging.Util (eitherToMaybe, (<$?>))

type ChatSettings = Map SettingName ASettingValue

data SettingName = SNNotifications deriving (Eq, Ord, Show)

instance ToJSONKey SettingName where
  toJSONKey = toJSONKeyText $ decodeLatin1 . strEncode

instance FromJSONKey SettingName where
  fromJSONKey = FromJSONKeyValue parseJSON

instance StrEncoding SettingName where
  strEncode = \case
    SNNotifications -> "notifications"
  strDecode = \case
    "notifications" -> Right SNNotifications
    _ -> Left "bad SettingName"

instance ToField SettingName where toField = toField . decodeLatin1 . strEncode

instance FromField SettingName where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

instance ToJSON SettingName where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromJSON SettingName where
  parseJSON = strParseJSON "ASettingValue"

data SettingType = TBool | TInt deriving (Show)

data SSettingType (t :: SettingType) where
  STBool :: SSettingType 'TBool
  STInt :: SSettingType 'TInt

deriving instance Show (SSettingType t)

instance TestEquality SSettingType where
  testEquality STBool STBool = Just Refl
  testEquality STInt STInt = Just Refl
  testEquality _ _ = Nothing

class SettingTypeI (t :: SettingType) where
  settingType :: SSettingType t

instance SettingTypeI 'TBool where settingType = STBool

instance SettingTypeI 'TInt where settingType = STInt

checkSettingType :: forall x t t'. (SettingTypeI t, SettingTypeI t') => x t' -> Either String (x t)
checkSettingType x = case testEquality (settingType @t) (settingType @t') of
  Just Refl -> Right x
  Nothing -> Left "bad SettingType"

data ASettingType = forall t. SettingTypeI t => ASettingType (SSettingType t)

instance SettingTypeI t => TextEncoding (SSettingType t) where
  textEncode = \case
    STBool -> "bool"
    STInt -> "int"
  textDecode s = textDecode s >>= \(ASettingType t) -> eitherToMaybe (checkSettingType t)

instance TextEncoding ASettingType where
  textEncode (ASettingType t) = textEncode t
  textDecode = \case
    "bool" -> Just $ ASettingType STBool
    "int" -> Just $ ASettingType STInt
    _ -> Nothing

instance SettingTypeI t => ToField (SSettingType t) where toField = toField . textEncode

instance FromField ASettingType where fromField = fromTextField_ textDecode

data SettingValue (t :: SettingType) where
  SVBool :: Bool -> SettingValue 'TBool
  SVInt :: Int -> SettingValue 'TInt

data ASettingValue = forall t. SettingTypeI t => ASV (SSettingType t) (SettingValue t)

deriving instance Show (SettingValue t)

deriving instance Show ASettingValue

instance SettingTypeI t => ToField (SettingValue t) where toField = toField . decodeLatin1 . strEncode

instance FromField ASettingValue where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

instance SettingTypeI t => StrEncoding (SettingValue t) where
  strEncode = \case
    SVBool s -> if s then "true" else "false"
    SVInt s -> B.pack $ show s
  strP = (\(ASV _ s) -> checkSettingType s) <$?> strP

instance StrEncoding ASettingValue where
  strEncode (ASV _ s) = strEncode s
  strP = A.peekChar >>= settingP
    where
      settingP Nothing = fail "empty SettingValue"
      settingP (Just c)
        | c >= '0' && c <= '9' =
          ASV STInt <$> (SVInt <$> (A.decimal <* (A.endOfInput <|> fail "SettingValue: bad number")))
        | c == 't' = ("true" $> ASV STBool (SVBool True)) <|> fail "SettingValue: bad bool"
        | c == 'f' = ("false" $> ASV STBool (SVBool False)) <|> fail "SettingValue: bad bool"
        | otherwise = fail "bad SettingValue"

instance ToJSON ASettingValue where
  toJSON = strToJSON
  toEncoding = strToJEncoding

instance FromJSON ASettingValue where
  parseJSON = strParseJSON "ASettingValue"
