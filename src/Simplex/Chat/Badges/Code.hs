{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Badge redemption codes, shared by the client, the store service and the checkout site.
--
-- A code is "SB-" and 20 Crockford base32 characters in four groups of five:
-- 19 payload characters and a final check character.
--
-- A prepaid name code is "SN", the shortest name length it pays for in one or two ASCII digits
-- (6 to 63, no leading zero), then 20 characters in four groups of five: 19 payload
-- characters and a check character over the length digits and the payload.
--
-- Reading folds the characters the alphabet omits so that a code copied by hand still
-- verifies: it is case-insensitive and maps 'I' and 'L' to '1' and 'O' to '0', except in a
-- prepaid code's length.
--
-- The check character of badge and prepaid codes is Luhn mod N with N = 32 over the values
-- before it, which keeps it inside the same 32-character alphabet. It detects every
-- single-character substitution and every transposition of adjacent characters except '0'
-- next to 'Z' - the values 0 and N-1, which is Luhn's one blind spot at any base.
--
-- 'BadgeCode' and 'NamePrepaidCode' are only constructed by their parse and random
-- functions, so a code whose check character fails cannot be hashed, looked up or sent.
module Simplex.Chat.Badges.Code
  ( BadgeCode,
    parseBadgeCode,
    randomBadgeCode,
    badgeCodeText,
    badgeCodeHash,
    formatBadgeCode,
    NameLength,
    nameLength,
    nameLengthInt,
    parseNameCode,
    formatNameCode,
    NamePrepaidCode,
    randomNamePrepaidCode,
    namePrepaidCodeText,
    namePrepaidCodeHash,
    namePrepaidCodeLength,
    minNameLength,
    maxNameLength,
    minNameYears,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.STM
import Control.Monad (guard, unless)
import Crypto.Random (ChaChaDRG)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (digitToInt, isAlphaNum, toUpper)
import Data.List (elemIndex, foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Util (eitherToMaybe, tshow)

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
codePrefix = "SB"

-- | The Crockford value of a character, folding the omitted characters onto the digits they
-- are mistaken for.
charValue :: Char -> Maybe Int
charValue c = case toUpper c of
  'I' -> Just 1
  'L' -> Just 1
  'O' -> Just 0
  u -> elemIndex u alphabet

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

randomBadgeCode :: TVar ChaChaDRG -> IO BadgeCode
randomBadgeCode drg = do
  payload <- randomPayload drg
  let vs = payload <> [checkValue payload]
  pure $ BadgeCode $ codePrefix <> T.pack (map valueChar vs)

-- | The payload values of a new badge or prepaid code from the CSPRNG. 256 is a multiple of
-- the base, so a byte reduces without bias.
randomPayload :: TVar ChaChaDRG -> IO [Int]
randomPayload drg = map ((`mod` base) . fromEnum) . B.unpack <$> atomically (C.randomBytes (codeLength - 1) drg)

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

nameCodePrefix :: Text
nameCodePrefix = "SN"

minNameLength :: Int
minNameLength = 6

maxNameLength :: Int
maxNameLength = 63

-- | The controller refuses a registration under 730 days.
minNameYears :: Int
minNameYears = 2

newtype NameLength = NameLength Int
  deriving (Eq, Ord, Show)

nameLength :: Int -> Maybe NameLength
nameLength n = NameLength n <$ guard (n >= minNameLength && n <= maxNameLength)

nameLengthInt :: NameLength -> Int
nameLengthInt (NameLength n) = n

-- | Read a prepaid code as typed, as 'parseBadgeCode' reads a badge code.
parseNameCode :: Text -> Maybe NamePrepaidCode
parseNameCode = eitherToMaybe . strDecode . encodeUtf8 . T.toUpper . T.filter isAlphaNum

-- | The code as it is shown and printed.
formatNameCode :: NamePrepaidCode -> Text
formatNameCode (NamePrepaidCode len body) = T.intercalate "-" $ (nameCodePrefix <> tshow (nameLengthInt len)) : T.chunksOf groupLength body

-- | A prepaid code that has passed its check: the shortest name length it pays for and the
-- 20 canonical characters after it.
data NamePrepaidCode = NamePrepaidCode NameLength Text
  deriving (Eq, Show)

-- | Encoded in the canonical form; read from it or from the shown form, the 20 characters in any case.
instance StrEncoding NamePrepaidCode where
  strEncode = encodeUtf8 . namePrepaidCodeText
  strP = A.string (encodeUtf8 nameCodePrefix) *> (codeAfter 2 <|> codeAfter 1)
    where
      -- a leading zero adds nothing to the check, so it would give the same code a second text and hash
      codeAfter digits = do
        lenDigits <- (:) <$> A.satisfy (\c -> c >= '1' && c <= '9') <*> A.count (digits - 1) A.digit
        len <- maybe (fail "a name length outside 6 to 63") pure $ nameLength $ foldl' (\n c -> n * 10 + digitToInt c) 0 lenDigits
        vs <- concat <$> A.count (codeLength `div` groupLength) (A.option '-' (A.char '-') *> A.count groupLength codeValue)
        let (payload, checkChar) = splitAt (codeLength - 1) vs
        unless (checkChar == [checkValue $ lengthValues len <> payload]) $ fail "a wrong check character"
        pure $ NamePrepaidCode len $ T.pack $ map valueChar vs
      codeValue = A.anyChar >>= maybe (fail "not a code character") pure . charValue

randomNamePrepaidCode :: TVar ChaChaDRG -> NameLength -> IO NamePrepaidCode
randomNamePrepaidCode drg len = do
  payload <- randomPayload drg
  let vs = payload <> [checkValue $ lengthValues len <> payload]
  pure $ NamePrepaidCode len $ T.pack $ map valueChar vs

lengthValues :: NameLength -> [Int]
lengthValues = map digitToInt . show . nameLengthInt

-- | The canonical form, "SN", the length and the 20 characters: the only representation hashed.
namePrepaidCodeText :: NamePrepaidCode -> Text
namePrepaidCodeText (NamePrepaidCode len body) = nameCodePrefix <> tshow (nameLengthInt len) <> body

-- | SHA-256 over the ASCII bytes of the canonical form, as 'badgeCodeHash'.
namePrepaidCodeHash :: NamePrepaidCode -> ByteString
namePrepaidCodeHash = C.sha256Hash . encodeUtf8 . namePrepaidCodeText

namePrepaidCodeLength :: NamePrepaidCode -> NameLength
namePrepaidCodeLength (NamePrepaidCode len _) = len
