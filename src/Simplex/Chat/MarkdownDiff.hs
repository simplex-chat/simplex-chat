{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Simplex.Chat.MarkdownDiff
  ( DiffChar (..),
    DiffPlainChar (..),
    DiffFormatStatus (..),
    FormatChar (..),
    diff,
    plainDiff,
  )
where

import qualified Data.Diff.Myers as D
import qualified Data.Foldable as F
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as S
import qualified Data.Text as T
import Simplex.Chat.Markdown (EditAction (..), Format)

data DiffFormatStatus
  = UnchangedFormat
  | ChangedToFormat (Maybe Format)
  deriving (Show, Eq)

data DiffChar = DiffChar FormatChar (Maybe EditAction)
  deriving (Show, Eq)

data DiffPlainChar = DiffPlainChar Char (Maybe EditAction)
  deriving (Show, Eq)

data FormatChar = FormatChar
  { char :: Char,
    format :: Maybe Format
  }
  deriving (Show, Eq)

newtype DeleteIndices = DeleteIndices (Seq Int) deriving (Show, Eq)

newtype InsertIndices = InsertIndices (Seq Int) deriving (Show, Eq)

plainDiff :: T.Text -> T.Text -> Seq DiffPlainChar
plainDiff left right = toPlain <$> formattedDiff
  where
    formattedDiff = diff (toFormatted left) (toFormatted right)
    toPlain :: DiffChar -> DiffPlainChar
    toPlain (DiffChar (FormatChar c _) editAction) = DiffPlainChar c editActionPlain
      where
        editActionPlain = case editAction of
          Just EAInsert -> Just EAInsert
          Just EADelete -> Just EADelete
          Just EAChangeFormat -> Nothing
          Nothing -> Nothing

    toFormatted :: T.Text -> Seq FormatChar
    toFormatted = fmap (`FormatChar` Nothing) . S.fromList . T.unpack

diff :: Seq FormatChar -> Seq FormatChar -> Seq DiffChar
diff left right = addInserts markDeletesAndUnchangedChars
  where
    edits = D.diffTexts (toText left) (toText right)
    (DeleteIndices deleteIndicies, InsertIndices insertIndicies) = indices

    toText :: Seq FormatChar -> T.Text
    toText = T.pack . F.toList . fmap char

    indices :: (DeleteIndices, InsertIndices)
    indices = F.foldl' f (DeleteIndices S.empty, InsertIndices S.empty) edits
      where
        f :: (DeleteIndices, InsertIndices) -> D.Edit -> (DeleteIndices, InsertIndices)
        f (x@(DeleteIndices ds), y@(InsertIndices is)) e = case e of
          D.EditDelete m n -> (x', y) where x' = DeleteIndices $ ds >< S.fromList [m .. n]
          D.EditInsert _ m n -> (x, y') where y' = InsertIndices $ is >< S.fromList [m .. n]

    unchangedChars :: Map Int DiffFormatStatus -- indexed in left
    unchangedChars = F.foldl' f mempty unchangedCharPairs
      where
        unchangedCharPairs :: Seq (Int, FormatChar, FormatChar)
        unchangedCharPairs = g <$> S.zip leftWithoutDeletes rightWithoutInserts

        leftWithoutDeletes :: Seq (Int, FormatChar)
        leftWithoutDeletes =
          S.filter (\(i, _) -> i `notElem` deleteIndicies) $
            S.zip (S.fromList [0 .. S.length left - 1]) left

        rightWithoutInserts :: Seq (Int, FormatChar)
        rightWithoutInserts =
          S.filter (\(i, _) -> i `notElem` insertIndicies) $
            S.zip (S.fromList [0 .. S.length right - 1]) right

        f :: Map Int DiffFormatStatus -> (Int, FormatChar, FormatChar) -> Map Int DiffFormatStatus
        f acc (i, FormatChar _ fL, FormatChar _ fR) = M.insert i x acc
          where
            x = if fL == fR then UnchangedFormat else ChangedToFormat fR

        g :: ((Int, FormatChar), (Int, FormatChar)) -> (Int, FormatChar, FormatChar)
        g ((i, c), (_, d)) = (i, c, d)

    markDeletesAndUnchangedChars :: Seq DiffChar
    markDeletesAndUnchangedChars = S.mapWithIndex f left
      where
        f :: Int -> FormatChar -> DiffChar
        f i x@(FormatChar c _)
          | i `elem` deleteIndicies = DiffChar x (Just EADelete)
          | otherwise = case unchangedChars M.! i of -- should never error
              UnchangedFormat -> DiffChar x Nothing
              ChangedToFormat f' -> DiffChar (FormatChar c f') (Just EAChangeFormat)
    addInserts :: Seq DiffChar -> Seq DiffChar
    addInserts base = F.foldr f base edits -- start from end and work backwards, hence foldr
      where
        f :: D.Edit -> Seq DiffChar -> Seq DiffChar
        f e acc = case e of
          D.EditDelete _ _ -> acc
          D.EditInsert i m n -> S.take i' acc >< inserts >< S.drop i' acc
            where
              -- D.EditInsert i m n -> S.take i  acc >< inserts >< S.drop i  acc
              -- if ok to have inserts before deletes, use i not i'
              -- Using i of course is faster, but perhaps i' approach can be optimised

              i' = slidePastDeleteBlock i

              slidePastDeleteBlock :: Int -> Int
              slidePastDeleteBlock x = case S.lookup x acc of
                Nothing -> x
                Just (DiffChar _ diffStatus) ->
                  if diffStatus == Just EADelete
                    then slidePastDeleteBlock (x + 1)
                    else x

              rightFormatChars = S.take (n - m + 1) $ S.drop m right
              inserts = fmap (`DiffChar` Just EAInsert) rightFormatChars
