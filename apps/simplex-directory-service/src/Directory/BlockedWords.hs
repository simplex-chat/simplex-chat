{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Directory.BlockedWords where

import Data.Char (isMark, isPunctuation, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Normalize as TN

data BlockedWordsConfig = BlockedWordsConfig
  { blockedWords :: Set Text,
    blockedFragments :: Set Text,
    extensionRules :: [(String, [String])],
    spelling :: Map Char [Char]
  }

hasBlockedFragments :: BlockedWordsConfig -> Text -> Bool
hasBlockedFragments BlockedWordsConfig {spelling, blockedFragments} s =
  any (\w -> any (`T.isInfixOf` w) blockedFragments) ws
  where
    ws = S.fromList $ filter (not . T.null) $ normalizeText spelling s

hasBlockedWords :: BlockedWordsConfig -> Text -> Bool
hasBlockedWords BlockedWordsConfig {spelling, blockedWords} s =
  not $ ws1 `S.disjoint` blockedWords && (length ws <= 1 || ws2 `S.disjoint` blockedWords)
  where
    ws = T.words s
    ws1 = normalizeWords ws
    ws2 = normalizeWords $ T.splitOn "  " s
    normalizeWords = S.fromList . filter (not . T.null) . concatMap (normalizeText spelling)

normalizeText :: Map Char [Char] -> Text -> [Text]
normalizeText spelling' =
  map (T.pack . filter (\c -> not $ isSpace c || isPunctuation c || isMark c))
    . allSubstitutions spelling'
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
allSubstitutions spelling' = sequence . map substs
  where
    substs c = fromMaybe [c] $ M.lookup c spelling'

wordVariants :: [(String, [String])] -> String -> [Text]
wordVariants [] s = [T.pack s]
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
