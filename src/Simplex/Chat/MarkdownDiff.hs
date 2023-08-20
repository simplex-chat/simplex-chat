{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}


module Simplex.Chat.MarkdownDiff
    ( DiffChar(..)
    , DiffPlainChar(..)
    , DiffStatus(..)
    , DiffPlainStatus(..)
    , DiffFormatStatus(..)
    , FormatChar(..)
    , LeftSide(..)
    , RightSide(..)
    , diff
    , plainDiff
    )
    where


import qualified Data.Foldable as F
import           Data.Function ((&))
import qualified Data.Map.Strict as M
import           Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Diff.Myers as D
import           Simplex.Chat.Markdown (Format)


data DiffStatus 
    = UnchangedChar DiffFormatStatus
    | Inserted 
    | Deleted 
    deriving (Show, Eq)


data DiffPlainStatus
    = UnchangedP
    | InsertedP
    | DeletedP
    deriving (Show, Eq)


data DiffFormatStatus
    = UnchangedFormat
    | ChangedToFormat (Maybe Format)
    deriving (Show, Eq)


data DiffChar = DiffChar FormatChar DiffStatus
    deriving (Show, Eq)


data DiffPlainChar = DiffPlainChar Char DiffPlainStatus
    deriving (Show, Eq)


data FormatChar = FormatChar 
    { char :: Char
    , format :: Maybe Format
    }
    deriving (Show, Eq)


newtype LeftSide  a = LeftSide  a deriving (Show, Eq)
newtype RightSide a = RightSide a deriving (Show, Eq)


newtype DeleteIndicies = DeleteIndicies (Seq Int) deriving (Show, Eq)
newtype InsertIndicies = InsertIndicies (Seq Int) deriving (Show, Eq)


plainDiff :: LeftSide T.Text -> RightSide T.Text -> Seq DiffPlainChar
plainDiff (LeftSide left) (RightSide right) = toPlain <$> formattedDiff
    where
    formattedDiff = diff (LeftSide $ toFormatted left) (RightSide $ toFormatted right)

    toPlain :: DiffChar -> DiffPlainChar
    toPlain (DiffChar (FormatChar c _) diffStatus) = DiffPlainChar c diffStatusPlain
        where 
        diffStatusPlain = case diffStatus of
            UnchangedChar _ -> UnchangedP
            Inserted -> InsertedP
            Deleted -> DeletedP

    toFormatted :: T.Text -> Seq FormatChar
    toFormatted = fmap (`FormatChar` Nothing) . S.fromList . T.unpack             


diff :: LeftSide (Seq FormatChar) -> RightSide (Seq FormatChar) -> Seq DiffChar
diff (LeftSide left) (RightSide right) = addInserts markDeletesAndUnchangedChars
    where
    edits = D.diffTexts (toText left) (toText right)  
    (DeleteIndicies deleteIndicies, InsertIndicies insertIndicies) = indices 
    
    toText :: Seq FormatChar -> T.Text
    toText = T.pack . F.toList . fmap char 

    indices :: (DeleteIndicies, InsertIndicies)
    indices = F.foldl' f (DeleteIndicies S.empty, InsertIndicies S.empty) edits
        where
        f :: (DeleteIndicies, InsertIndicies) -> D.Edit -> (DeleteIndicies, InsertIndicies)
        f (x@(DeleteIndicies ds), y@(InsertIndicies is)) e = case e of
            D.EditDelete   m n -> (x', y)  where x' = DeleteIndicies $ ds >< S.fromList [m .. n]  
            D.EditInsert _ m n -> (x , y') where y' = InsertIndicies $ is >< S.fromList [m .. n] 

    unchangedChars :: M.Map Int DiffFormatStatus -- indexed in left
    unchangedChars = F.foldl' f mempty unchangedCharPairs
        where
        unchangedCharPairs :: Seq (Int, FormatChar, FormatChar) 
        unchangedCharPairs = g <$> S.zip leftWithoutDeletes rightWithoutInserts

        leftWithoutDeletes :: Seq (Int, FormatChar) 
        leftWithoutDeletes = 
            left
            & S.zip (S.fromList [0 .. S.length left - 1])
            & S.filter (\(i, _) -> i `notElem` deleteIndicies)

        rightWithoutInserts :: Seq (Int, FormatChar) 
        rightWithoutInserts = 
            right
            & S.zip (S.fromList [0 .. S.length right - 1])
            & S.filter (\(i, _) -> i `notElem` insertIndicies)

        f :: M.Map Int DiffFormatStatus -> (Int, FormatChar, FormatChar) -> M.Map Int DiffFormatStatus
        f acc (i, FormatChar _ fL, FormatChar _ fR) = M.insert i x acc
            where x = if fL == fR then UnchangedFormat else ChangedToFormat fR

        g :: ((Int, FormatChar), (Int, FormatChar)) -> (Int, FormatChar, FormatChar)
        g ((i,c), (_,d)) = (i,c,d)       

    markDeletesAndUnchangedChars :: Seq DiffChar
    markDeletesAndUnchangedChars = S.mapWithIndex f left
        where
        f :: Int -> FormatChar -> DiffChar
        f i x = DiffChar x $
            if i `elem` deleteIndicies then Deleted 
            else UnchangedChar $ unchangedChars M.! i -- should never error             

    addInserts :: Seq DiffChar -> Seq DiffChar
    addInserts base = F.foldr f base edits -- start from end and work backwards, hence foldr
        where
        f :: D.Edit -> Seq DiffChar -> Seq DiffChar
        f e acc = case e of
            D.EditDelete _ _ -> acc
            D.EditInsert i m n -> S.take i' acc >< inserts >< S.drop i' acc 
         -- D.EditInsert i m n -> S.take i  acc >< inserts >< S.drop i  acc              
            -- if ok to have inserts before deletes, use i not i'
            -- Using i of course is faster, but perhaps i' approach can be optimised
              
                where 
                i' = slidePastDeleteBlock i

                slidePastDeleteBlock :: Int -> Int
                slidePastDeleteBlock x = case S.lookup x acc of
                    Nothing -> x
                    Just (DiffChar _ diffStatus) -> 
                        if diffStatus == Deleted then slidePastDeleteBlock (x + 1) 
                        else x

                rightFormatChars = S.take (n - m + 1) $ S.drop m right
                inserts = fmap (`DiffChar` Inserted) rightFormatChars