{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Badge redemption codes, shared by the client, the badge service and the mint.
--
-- A code is @SXB-@ and 20 Crockford base32 characters in four groups of five:
-- 19 payload characters and a final check character.
--
-- Reading folds the characters the alphabet omits so that a code copied by hand still
-- verifies: it is case-insensitive and maps @I@ and @L@ to @1@ and @O@ to @0@.
--
-- The check character is Luhn mod N with N = 32 over the payload values, which keeps it
-- inside the same 32-character alphabet. It detects every single-character substitution
-- and every transposition of adjacent characters except '0' next to 'Z' - the values 0 and
-- N-1, which is Luhn's one blind spot at any base.
--
-- 'BadgeCode' is only constructed by 'parseBadgeCode' and 'randomBadgeCode', so a code
-- whose check character fails cannot be hashed, looked up or sent.
module Simplex.Chat.Badges.Code
  ( BadgeCode,
    parseBadgeCode,
    randomBadgeCode,
    badgeCodeText,
    badgeCodeHash,
    formatBadgeCode,
  )
where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isAlphaNum, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Simplex.Messaging.Crypto as C

-- | A code that has passed its check character, in canonical form: 'codePrefix' followed by
-- 20 upper-case alphabet characters, without separators.
newtype BadgeCode = BadgeCode Text
  deriving (Eq, Show)

-- Crockford base32: the digits and the upper-case letters except I, L, O and U.
alphabet :: String
alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

base :: Int
base = 32

codeLength :: Int
codeLength = 20

groupLength :: Int
groupLength = 5

codePrefix :: Text
codePrefix = "SXB"

-- | The Crockford value of a character, folding the omitted characters onto the digits they
-- are mistaken for.
charValue :: Char -> Maybe Int
charValue c = case toUpper c of
  'I' -> Just 1
  'L' -> Just 1
  'O' -> Just 0
  u -> lookup u $ zip alphabet [0 ..]

valueChar :: Int -> Char
valueChar v = alphabet !! v

-- | Luhn mod N (N = 32): the value that makes the whole code sum to zero modulo the base.
checkValue :: [Int] -> Int
checkValue payload = (base - total `mod` base) `mod` base
  where
    -- doubling every second value from the right, as the check character sits to the right of the payload
    total = fst $ foldr step (0, 2) payload
    step v (sum', factor) =
      let addend = factor * v
       in (sum' + addend `div` base + addend `mod` base, if factor == 2 then 1 else 2)

-- | Read a code as typed: any case, separators optional, ambiguous characters folded.
-- 'Nothing' for anything not well-formed, a failed check character included.
parseBadgeCode :: Text -> Maybe BadgeCode
parseBadgeCode t = do
  body <- T.stripPrefix codePrefix $ T.toUpper $ T.filter isAlphaNum t
  vs <- mapM charValue $ T.unpack body
  let (payload, checkChar) = splitAt (codeLength - 1) vs
  if T.length body == codeLength && checkChar == [checkValue payload]
    -- rebuilt from the values, not from body: that is what folds I/L/O into the canonical form
    then Just $ BadgeCode $ codePrefix <> T.pack (map valueChar vs)
    else Nothing

-- | A new code from the CSPRNG. 256 is a multiple of the base, so a byte reduces without bias.
randomBadgeCode :: TVar ChaChaDRG -> IO BadgeCode
randomBadgeCode drg = do
  bs <- atomically $ C.randomBytes (codeLength - 1) drg
  let payload = map ((`mod` base) . fromEnum) $ B.unpack bs
      vs = payload <> [checkValue payload]
  pure $ BadgeCode $ codePrefix <> T.pack (map valueChar vs)

-- | The canonical form: the only representation of a code that is hashed or sent.
badgeCodeText :: BadgeCode -> Text
badgeCodeText (BadgeCode t) = t

-- | The only thing about a code stored service-side: SHA-256 over the ASCII bytes of the
-- canonical form, prefix included.
badgeCodeHash :: BadgeCode -> ByteString
badgeCodeHash = C.sha256Hash . encodeUtf8 . badgeCodeText

-- | The code as it is shown and printed, in four groups of five.
formatBadgeCode :: BadgeCode -> Text
formatBadgeCode (BadgeCode t) = T.intercalate "-" $ codePrefix : groups (T.drop (T.length codePrefix) t)
  where
    groups s
      | T.null s = []
      | otherwise = let (g, rest) = T.splitAt groupLength s in g : groups rest
