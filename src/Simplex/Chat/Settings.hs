{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.Settings where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import Data.Functor (($>))
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Type.Equality
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Simplex.Chat.Util (eitherToMaybe)
import Simplex.Messaging.Encoding.String
import Simplex.Messaging.Parsers (fromTextField_)
import Simplex.Messaging.Util ((<$?>))

data SettingType = TString | TBool | TInt

data SSettingType (t :: SettingType) where
  STString :: SSettingType 'TString
  STBool :: SSettingType 'TBool
  STInt :: SSettingType 'TInt

instance TestEquality SSettingType where
  testEquality STString STString = Just Refl
  testEquality STBool STBool = Just Refl
  testEquality STInt STInt = Just Refl
  testEquality _ _ = Nothing

class SettingTypeI (t :: SettingType) where
  settingType :: SSettingType t

instance SettingTypeI 'TString where settingType = STString

instance SettingTypeI 'TBool where settingType = STBool

instance SettingTypeI 'TInt where settingType = STInt

checkSettingType :: forall x t t'. (SettingTypeI t, SettingTypeI t') => x t' -> Either String (x t)
checkSettingType x = case testEquality (settingType @t) (settingType @t') of
  Just Refl -> Right x
  Nothing -> Left "bad SettingType"

data ASettingType = forall t. SettingTypeI t => ASettingType (SSettingType t)

instance SettingTypeI t => TextEncoding (SSettingType t) where
  textEncode = \case
    STString -> "string"
    STBool -> "bool"
    STInt -> "int"
  textDecode s = textDecode s >>= \(ASettingType t) -> eitherToMaybe (checkSettingType t)

instance TextEncoding ASettingType where
  textEncode (ASettingType t) = textEncode t
  textDecode = \case
    "string" -> Just $ ASettingType STString
    "bool" -> Just $ ASettingType STBool
    "int" -> Just $ ASettingType STInt
    _ -> Nothing

instance SettingTypeI t => ToField (SSettingType t) where toField = toField . textEncode

instance FromField ASettingType where fromField = fromTextField_ textDecode

data SettingValue (t :: SettingType) where
  SVString :: String -> SettingValue 'TString
  SVBool :: Bool -> SettingValue 'TBool
  SVInt :: Int -> SettingValue 'TInt

data ASettingValue = forall t. SettingTypeI t => ASV (SSettingType t) (SettingValue t)

instance SettingTypeI t => ToField (SettingValue t) where toField = toField . decodeLatin1 . strEncode

instance FromField ASettingValue where fromField = fromTextField_ $ eitherToMaybe . strDecode . encodeUtf8

instance SettingTypeI t => StrEncoding (SettingValue t) where
  strEncode = \case
    SVString s -> "\"" <> B.pack s <> "\""
    SVBool s -> if s then "true" else "false"
    SVInt s -> B.pack $ show s
  strP = (\(ASV _ s) -> checkSettingType s) <$?> strP

instance StrEncoding ASettingValue where
  strEncode (ASV _ s) = strEncode s
  strP = A.peekChar >>= settingP
    where
      settingP Nothing = fail "empty SettingValue"
      settingP (Just c)
        | c == '"' =
          A.takeByteString
            >>= \s ->
              if B.length s >= 2 && B.last s == '"'
                then pure . ASV STString $ (SVString . B.unpack . B.init $ B.tail s)
                else fail "SettingValue: bad string"
        | c >= '0' && c <= '9' =
          ASV STInt <$> (SVInt <$> (A.decimal <* (A.endOfInput <|> fail "SettingValue: bad number")))
        | c == 't' = ("true" $> ASV STBool (SVBool True)) <|> fail "SettingValue: bad bool"
        | c == 'f' = ("false" $> ASV STBool (SVBool False)) <|> fail "SettingValue: bad bool"
        | otherwise = fail "bad SettingValue"
