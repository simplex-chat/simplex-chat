module Directory.BlockedWords where

import Data.Char (isMark, isPunctuation, isSpace, toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Normalize as TN
-- import qualified Data.Text.ICU as ICU

containsBlockedWords :: Text -> [String] -> Bool
containsBlockedWords s blockedWords =
    let normalizedWords = concatMap words $ normalizeText s
        -- Fully normalize the entire string (no spaces or punctuation)
        fullNorm = normalizeText $ T.filter (not . isSpace) s
        -- Check if any individual word is a swear word
        wordCheck = any (`elem` blockedWords) normalizedWords
        -- Check if the full string, when normalized, matches a swear word exactly
        fullCheck = any (\sw -> any (\fn -> fn == sw && T.length s <= length sw * 2) fullNorm) blockedWords
        -- Check if the string is a single word (no spaces)
        isSingleWord = not $ T.any isSpace s
     in wordCheck || (fullCheck && not isSingleWord)

normalizeText :: Text -> [String]
normalizeText =
  filter (not . null)
    . map (filter (\c -> not (isPunctuation c) && not (isMark c)))
    . allSubstitutions
    . removeTriples
    . T.unpack
    . T.toLower
    . TN.normalize TN.NFKD
  --   . ICU.transliterate transliterator
  -- where
  --   transliterator = ICU.trans "Any-Latin; Latin-ASCII"

textSubstitutions :: M.Map Char [Char]
textSubstitutions = M.fromList []

-- replaces triple and larger occurences with doubles
removeTriples :: String -> String
removeTriples xs = go xs '\0' False
  where
    go [] _ _ = []
    go (c : cs) prev samePrev
      | prev /= c = c : go cs c False
      | samePrev = go cs c True
      | otherwise = c : go cs c True

substOptions :: Char -> [Char]
substOptions c = fromMaybe [c] $ M.lookup c textSubstitutions

-- Generate all possible strings by substituting each character
allSubstitutions :: String -> [String]
allSubstitutions s = sequence $ map substOptions s
