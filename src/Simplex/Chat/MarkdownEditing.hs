{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}


module Simplex.Chat.MarkdownEditing where

import           Data.Aeson (ToJSON)
import qualified Data.Aeson as J
import qualified Data.Foldable as F
import qualified Data.Sequence as S
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics ( Generic )
import           Simplex.Messaging.Parsers ( sumTypeJSON ) 
import qualified Data.Diff.Myers as DM
import           Simplex.Chat.Markdown ( FormattedText(..), Format )

import qualified Debug.Trace as DBG

data EditingOperation = Add | Delete | Substitute
  deriving (Show, Eq)


data EditedChar = EditedChar 
  { format :: Maybe Format
  , char :: Char
  , operation :: Maybe EditingOperation
  }
  deriving (Show, Eq)


data EditedText =  EditedText 
  { format :: Maybe Format
  , text :: Text
  , added :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON EditedText where
  toEncoding :: EditedText -> J.Encoding
  toEncoding = J.genericToEncoding $ sumTypeJSON id


formattedEditedText :: [FormattedText] -> [FormattedText] -> [EditedChar]
formattedEditedText s s' = diff (toEditedChars s) (toEditedChars s')


toEditedChars :: [FormattedText] -> [EditedChar]
toEditedChars = concatMap toChars
  where
    toChars FormattedText {format, text} =
      map (\char -> EditedChar {format, char, operation = Nothing}) $ T.unpack text


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


toText :: [EditedChar] -> T.Text
toText = T.pack . fmap char  


fromEdits :: S.Seq EditedChar -> S.Seq EditedChar -> S.Seq DM.Edit -> S.Seq EditedChar
fromEdits left right edits =   
  let
    delIndices :: S.Seq Int
    delIndices = F.foldl' f S.Empty edits
      where
        f :: S.Seq Int -> DM.Edit -> S.Seq Int
        f acc e = case e of
          DM.EditInsert {} -> acc
          DM.EditDelete m n -> acc S.>< S.fromList [m .. n]

    markDels :: S.Seq EditedChar
    markDels = S.mapWithIndex f left
      where
        f :: Int -> EditedChar -> EditedChar
        f i c = if i `elem` delIndices then c {operation = Just Delete} else c

    addAdds :: S.Seq EditedChar -> S.Seq EditedChar
    addAdds base = F.foldr f base edits -- start from end and work backwards, hence foldr
      where
        f :: DM.Edit -> S.Seq EditedChar -> S.Seq EditedChar
        f e acc = case e of
          DM.EditDelete {} -> acc
          DM.EditInsert i m n -> DBG.trace ("DM.EditInsert i m n: " <> show (i, m, n, rightChars, adds)) $ S.take i acc S.>< adds S.>< S.drop i acc
            where 
              rightChars = S.take (n - m + 1) $ S.drop m right
              adds = fmap (\c -> c {operation = Just Add}) rightChars
  in
    addAdds markDels


diff :: [EditedChar] -> [EditedChar] -> [EditedChar]
diff left right = F.toList $ fromEdits (S.fromList left) (S.fromList right) edits
  where edits = DM.diffTexts (toText left) (toText right)