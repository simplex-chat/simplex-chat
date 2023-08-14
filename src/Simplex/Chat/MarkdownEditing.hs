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
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable as F
import           Data.Sequence ( Seq(..), (><) )
import qualified Data.Sequence as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import           Data.Word ( Word64 )
import           GHC.Generics ( Generic )
-- import           Network.ByteOrder (word64) -- also remove from cabal
import           Simplex.Messaging.Parsers ( sumTypeJSON ) 
import qualified Data.Diff.Myers as D
import           Simplex.Chat.Markdown ( FormattedText(..), Format )
-- import           Sound.Osc.Coding.Byte
import qualified Debug.Trace as DBG

-- todo unused
-- data EditingOperation 
--   = AddChar 
--   | DeleteChar 
--   | ChangeFormatOnly 
--   | SubstitueChar
--   deriving (Show, Eq)


-- todo unused
-- data EditedChar = EditedChar 
--   { format :: Maybe Format
--   , char :: Char
--   , operation :: Maybe EditingOperation
--   }
--     deriving (Show, Eq)

-- data DiffStatus 
--     = Unchanged
--     | Inserted 
--     | Deleted 
--     | ChangedFormatOnly {newFormat :: Maybe Format}
--     -- | Replaced {original :: FormattedChar} -- same as Delete+Insert
--     deriving (Show, Eq)


data DiffStatus 
    = UnchangedTextually DiffUnchangedTextuallyStatus
    | Inserted 
    | Deleted 
    deriving (Show, Eq)

data DiffUnchangedTextuallyStatus
    = TotallyUnchanged
    | ChangedFormat {newFormat :: Maybe Format}
    deriving (Show, Eq)

data DiffedChar = DiffedChar FormattedChar DiffStatus
    deriving (Show, Eq)

data FormattedChar = FormattedChar 
    { format :: Maybe Format
    , char :: Char
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


newtype DeleteIndicies = DeleteIndicies {deleteIndicies :: Seq Int} deriving (Show, Eq)
newtype InsertIndicies = InsertIndicies {insertIndicies :: Seq Int} deriving (Show, Eq)


-- formattedEditedText :: [FormattedText] -> [FormattedText] -> [EditedChar]
-- formattedEditedText s s' = diff (toEditedChars s) (toEditedChars s')


-- toEditedChars :: [FormattedText] -> [EditedChar]
-- toEditedChars = concatMap toChars
--   where
--     toChars FormattedText {format, text} =
--       map (\char -> EditedChar {format, char, operation = Nothing}) $ T.unpack text


toFormattedChars :: [FormattedText] -> [FormattedChar]
toFormattedChars = concatMap toChars
    where
    toChars FormattedText {format, text} =
        map (\char -> FormattedChar {format, char}) $ T.unpack text


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


-- fromEdits :: Seq EditedChar -> Seq EditedChar -> Seq D.Edit -> Seq EditedChar
-- fromEdits left right edits =   
--   let
--     delIndices :: Seq Int
--     delIndices = F.foldl' f S.Empty edits
--       where
--         f :: Seq Int -> D.Edit -> Seq Int
--         f acc e = case e of
--           D.EditInsert {} -> acc
--           D.EditDelete m n -> acc >< S.fromList [m .. n]

--     markDeletes :: Seq EditedChar
--     markDeletes = S.mapWithIndex f left
--       where
--         f :: Int -> EditedChar -> EditedChar
--         f i c = if i `elem` delIndices then c {operation = Just DeleteChar} else c

--     addInserts :: Seq EditedChar -> Seq EditedChar
--     addInserts base = F.foldr f base edits -- start from end and work backwards, hence foldr
--       where
--         f :: D.Edit -> Seq EditedChar -> Seq EditedChar
--         f e acc = case e of
--           D.EditDelete {} -> acc
--           D.EditInsert i m n -> DBG.trace ("D.EditInsert i m n: " <> show (i, m, n, rightChars, adds)) $ S.take i acc >< adds >< S.drop i acc
--             where 
--               rightChars = S.take (n - m + 1) $ S.drop m right
--               adds = fmap (\c -> c {operation = Just AddChar}) rightChars
--   in
--     addInserts markDeletes


-- todo unused
-- diff :: [EditedChar] -> [EditedChar] -> [EditedChar]
-- diff left right = F.toList $ fromEdits (S.fromList left) (S.fromList right) edits
--   where edits = D.diffTexts (toText left) (toText right)


nonNegativeInts :: Seq Int
nonNegativeInts = S.fromList [0 ..]


findDiffs :: Seq FormattedChar -> Seq FormattedChar -> Seq DiffedChar
findDiffs left right = 
    let
        toText :: Seq FormattedChar -> T.Text
        toText = T.pack . F.toList . fmap char  

        edits :: Seq D.Edit
        edits = D.diffTexts (toText left) (toText right)  

        indices :: (DeleteIndicies, InsertIndicies)
        indices = F.foldl' f (DeleteIndicies S.empty, InsertIndicies S.empty) edits
            where
            f :: (DeleteIndicies, InsertIndicies) -> D.Edit -> (DeleteIndicies, InsertIndicies)
            f (x@(DeleteIndicies ds), y@(InsertIndicies is)) e = case e of
                D.EditDelete   m n -> (x', y)  where x' = DeleteIndicies $ ds >< S.fromList [m .. n]  
                D.EditInsert _ m n -> (x , y') where y' = InsertIndicies $ is >< S.fromList [m .. n] 

        -- -- todo unused
        -- withDeleteDiffs :: Seq DiffedChar
        -- withDeleteDiffs = S.mapWithIndex f pristine
        --     where
        --     pristine :: Seq DiffedChar
        --     pristine = fmap (`DiffedChar` Unchanged) left

        --     ns = deleteIndicies $ fst indices

        --     f :: Int -> DiffedChar -> DiffedChar
        --     f i x@(DiffedChar c _) = if i `elem` ns then DiffedChar c Deleted else x
        




        -- withMarkedFormatChanges :: Seq DiffedChar
        -- withMarkedFormatChanges = undefined
        --     where
        --     withDeleteDiffsZ = S.zip nonNegativeInts withDeleteDiffs
        --     unchangedTextually = S.filter (\(_, x -> not $ )) withDeleteDiffsZ
            
        unchangedTextually :: Seq (Int, FormattedChar, FormattedChar) -- indexed in original
        unchangedTextually = f <$> S.zip leftWithoutDeletes rightWithoutInserts
            where
            leftWithoutDeletes :: Seq (Int, FormattedChar) 
            leftWithoutDeletes = S.filter (\(i, _) -> i `notElem` ns) leftZ -- indexed in original
                where
                ns = deleteIndicies $ fst indices
                leftZ = S.zip nonNegativeInts left

            rightWithoutInserts :: Seq (Int, FormattedChar)
            rightWithoutInserts = S.filter (\(i, _) -> i `notElem` ns) rightZ -- indexed in original
                where
                ns = insertIndicies $ snd indices
                rightZ = S.zip nonNegativeInts right

            f :: ((Int, FormattedChar), (Int, FormattedChar)) -> (Int, FormattedChar, FormattedChar)
            f ((i,c), (_,d)) = (i,c,d)

        analysisOfUnchangedTextually :: Seq (Int, DiffUnchangedTextuallyStatus)
        analysisOfUnchangedTextually = f <$> unchangedTextually
            where
            f :: (Int, FormattedChar, FormattedChar) -> (Int, DiffUnchangedTextuallyStatus)
            f (i, FormattedChar fL _, FormattedChar fR _) = (i, result)
                where 
                result :: DiffUnchangedTextuallyStatus
                result = 
                    if fL == fR then TotallyUnchanged
                    else ChangedFormat {newFormat = fR}
       


    in
        undefined