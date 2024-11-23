{-# LANGUAGE OverloadedStrings #-}

module Simplex.Chat.Operators.Conditions where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

stripFrontMatter :: Text -> Text
stripFrontMatter =
  T.unlines
    -- . dropWhile ("# " `T.isPrefixOf`) -- strip title
    . dropWhile (T.all isSpace)
    . dropWhile fm
    . (\ls -> let ls' = dropWhile (not . fm) ls in if null ls' then ls else ls')
    . dropWhile fm
    . T.lines
  where
    fm = ("---" `T.isPrefixOf`)
