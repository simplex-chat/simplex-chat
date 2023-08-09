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
import           Data.Sequence ( Seq(..), (><) )
import qualified Data.Sequence as S
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics ( Generic )
import           Simplex.Messaging.Parsers ( sumTypeJSON ) 
import qualified Data.Diff.Myers as D
import           Simplex.Chat.Markdown ( FormattedText(..), Format )

import qualified Debug.Trace as DBG

data EditingOperation 
  = AddChar 
  | DeleteChar 
  | ChangeFormatOnly 
  -- todo ? | SubstitueChar
  deriving (Show, Eq)


data EditedChar = EditedChar 
  { format :: Maybe Format
  , char :: Char
  , operation :: Maybe EditingOperation
  }
  deriving (Show, Eq)

newtype Diffs = Diffs [EditedChar]
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


fromEdits :: Seq EditedChar -> Seq EditedChar -> Seq D.Edit -> Seq EditedChar
fromEdits left right edits =   
  let
    delIndices :: Seq Int
    delIndices = F.foldl' f S.Empty edits
      where
        f :: Seq Int -> D.Edit -> Seq Int
        f acc e = case e of
          D.EditInsert {} -> acc
          D.EditDelete m n -> acc >< S.fromList [m .. n]

    markDels :: Seq EditedChar
    markDels = S.mapWithIndex f left
      where
        f :: Int -> EditedChar -> EditedChar
        f i c = if i `elem` delIndices then c {operation = Just DeleteChar} else c

    addAdds :: Seq EditedChar -> Seq EditedChar
    addAdds base = F.foldr f base edits -- start from end and work backwards, hence foldr
      where
        f :: D.Edit -> Seq EditedChar -> Seq EditedChar
        f e acc = case e of
          D.EditDelete {} -> acc
          D.EditInsert i m n -> DBG.trace ("D.EditInsert i m n: " <> show (i, m, n, rightChars, adds)) $ S.take i acc >< adds >< S.drop i acc
            where 
              rightChars = S.take (n - m + 1) $ S.drop m right
              adds = fmap (\c -> c {operation = Just AddChar}) rightChars
  in
    addAdds markDels


-- todo unused
diff :: [EditedChar] -> [EditedChar] -> [EditedChar]
diff left right = F.toList $ fromEdits (S.fromList left) (S.fromList right) edits
  where edits = D.diffTexts (toText left) (toText right)


findDiffs :: [EditedChar] -> [EditedChar] -> Diffs
findDiffs left right = Diffs diffs
    where 
      textDiffs = D.diffTexts (toText left) (toText right)  
      formatDiffs = 
      diffs = 