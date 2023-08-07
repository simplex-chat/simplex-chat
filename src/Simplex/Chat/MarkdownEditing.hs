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



data EditingOperation = Add | Delete | Substitute
  deriving (Show, Eq)


data EditedChar = EditedChar 
  { format :: Maybe Format
  , char :: Char
  , operation :: Maybe EditingOperation
  }
  deriving (Show, Eq)


-- TODO unused?
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


-- -- todo move to util
-- takeStartingAt :: Int -> Int -> S.Seq a -> S.Seq a

diff :: [EditedChar] -> [EditedChar] -> [EditedChar]
diff left right = 
  -- No Substitute for now; however a subsequent scan for adjacent Add/Delete can swap both for one sub
  let
    toText :: [EditedChar] -> T.Text
    toText = T.pack . fmap char 

    fromEdits :: S.Seq EditedChar -> S.Seq EditedChar -> S.Seq DM.Edit -> S.Seq EditedChar
    -- fromEdits ls rs es = snd $ F.foldl' f (0, S.empty) es
    --   where 
    --     f :: (Int, S.Seq EditedChar) -> DM.Edit -> (Int, S.Seq EditedChar)
    --     f (parsedUntilI, res) e = case e of
    --       DM.EditDelete leftFrom leftTo -> 
    --         (parsedUntilI', res <> (S.take leftFrom ls)) -- ++ ___ 
    --           where parsedUntilI' = parsedUntilI + leftTo
    fromEdits ls rs es = snd $ F.foldl' f (ls, S.empty) es
      where 
        f :: (S.Seq EditedChar, S.Seq EditedChar) -> DM.Edit -> (S.Seq EditedChar, S.Seq EditedChar)
        f (leftFeed, res) e = case e of
          DM.EditDelete leftFrom leftTo -> 
            (leftFeed', res <> S.take leftFrom leftFeed <> drops)
              where 
                leftFeed' = S.drop leftFrom leftFeed
                drops = undefined

          DM.EditInsert leftPos rightFrom rightTo -> 
            undefined



    -- fromEdit :: S.Seq EditedChar -> S.Seq EditedChar -> DM.Edit -> S.Seq EditedChar
    -- fromEdit ls rs e = case e of
    --   DM.EditDelete leftFrom leftTo -> (S.take leftFrom ls) ++ ___ ++ (S.drop (leftTo + 1) ls)
    --   DM.EditInsert leftPos rightFrom rightTo -> _

    leftT = toText left
    rightT = toText right
    leftS = S.fromList left
    rightS = S.fromList right

    edits = DM.diffTexts leftT rightT
  in
    F.toList $ fromEdits leftS rightS edits