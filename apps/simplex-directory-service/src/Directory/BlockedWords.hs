module Directory.BlockedWords where

import Data.Char (isMark, isPunctuation, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Normalize as TN

containsBlockedWords :: Map Char [Char] -> [String] -> Text -> Bool
containsBlockedWords spelling blockedWords s =
  let normalizedWords = concatMap words $ normalizeText spelling s
      -- Fully normalize the entire string (no spaces or punctuation)
      fullNorm = normalizeText spelling $ T.filter (not . isSpace) s
      -- Check if any individual word is a swear word
      wordCheck = any (`elem` blockedWords) normalizedWords
      -- Check if the full string, when normalized, matches a swear word exactly
      fullCheck = any (\bw -> T.length s <= length bw * 2 && any (bw ==) fullNorm) blockedWords
      -- Check if the string is a single word (no spaces)
      isSingleWord = not $ T.any isSpace s
   in wordCheck || (fullCheck && not isSingleWord)

normalizeText :: Map Char [Char] -> Text -> [String]
normalizeText spelling =
  filter (not . null)
    . map (filter (\c -> not (isPunctuation c) && not (isMark c)))
    . allSubstitutions spelling
    . removeTriples
    . T.unpack
    . T.toLower
    . TN.normalize TN.NFKD

-- replaces triple and larger occurences with doubles
removeTriples :: String -> String
removeTriples xs = go xs '\0' False
  where
    go [] _ _ = []
    go (c : cs) prev samePrev
      | prev /= c = c : go cs c False
      | samePrev = go cs c True
      | otherwise = c : go cs c True

-- Generate all possible strings by substituting each character
allSubstitutions :: Map Char [Char] -> String -> [String]
allSubstitutions spelling = sequence . map substs
  where
    substs c = fromMaybe [c] $ M.lookup c spelling

wordVariants :: [(String, [String])] -> String -> [String]
wordVariants [] s = [s]
wordVariants (sub : subs) s = concatMap (wordVariants subs) (replace sub)
  where
    replace (pat, tos) = go s
      where
        go [] = [""]
        go s'@(c : rest)
          | pat `isPrefixOf` s' =
              let s'' = drop (length pat) s'
                  restVariants = go s''
              in map (pat <>) restVariants
                   <> concatMap (\to -> map (to <>) restVariants) tos
          | otherwise = map (c :) (go rest)
