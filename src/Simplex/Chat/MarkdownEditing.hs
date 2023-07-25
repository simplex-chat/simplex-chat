{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}


module Simplex.Chat.MarkdownEditing where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics ( Generic )
import Simplex.Messaging.Parsers ( sumTypeJSON ) 
import Data.Diff.Myers
import Simplex.Chat.Markdown ( FormattedText(..), Format )


data EditingOperation = EOAdd | EODelete | EOSubstitute
  deriving (Show, Eq)


data EditedChar = EditedChar 
  { format :: Maybe Format
  , char :: Char
  , operation :: Maybe EditingOperation
  }
  deriving (Show, Eq)


-- TODO unused?
data EditedText =  EditedText {format :: Maybe Format, text :: Text, added :: Maybe Bool}
  deriving (Eq, Show, Generic)

instance ToJSON EditedText where
  toEncoding :: EditedText -> J.Encoding
  toEncoding = J.genericToEncoding $ sumTypeJSON id


formattedEditedText :: [FormattedText] -> [FormattedText] -> [EditedChar]
formattedEditedText s s' = myersDiff (toEditedChars s) (toEditedChars s')


toEditedChars :: [FormattedText] -> [EditedChar]
toEditedChars = concatMap toChars
  where
    toChars FormattedText {format, text} =
      map (\char -> EditedChar {format, char, operation = Nothing}) $ T.unpack text


-- TODO delete?
-- fromEditedChars :: [EditedChar] -> [EditedText]
-- fromEditedChars = reverse . foldl' addChar []
--   where
--     addChar :: [EditedText] -> EditedChar -> [EditedText]
--     addChar [] c = [toText c]
--     addChar ts@(t : rest) c
--       | sameFormat t c = appendChar t c : rest
--       | otherwise = toText c : ts

--     toText :: EditedChar -> EditedText
--     toText EditedChar {format, char, added} = EditedText {format, text = T.singleton char, added}
    
--     sameFormat :: EditedText -> EditedChar -> Bool
--     sameFormat EditedText {format, added} EditedChar {format = format', added = added'} = format == format' && added == added'
    
--     appendChar :: EditedText -> EditedChar -> EditedText
--     appendChar t@EditedText {text} EditedChar {char} = t {text = text <> T.singleton char}


myersDiff :: [EditedChar] -> [EditedChar] -> [EditedChar]
myersDiff s1 s2 = undefined