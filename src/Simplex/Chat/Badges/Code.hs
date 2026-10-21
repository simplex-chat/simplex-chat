{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Badge redemption codes, shared by the client, the store service and the checkout site.
--
-- A code is "SB-" and 20 Crockford base32 characters in four groups of five:
-- 19 payload characters and a final check character.
--
-- A name code is one of two kinds, told apart by the length of the text after "SN":
-- - an owner code, "SN-" and 30 characters in six groups of five: the 128-bit wallet
--   entropy and the first 22 bits of its SHA-256;
-- - a prepaid code, "SN", the shortest name length it pays for in one or two ASCII digits
--   (6 to 63, no leading zero), then 20 characters in four groups of five: 19 payload
--   characters and a check character over the length digits and the payload.
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
    NameCode (..),
    parseNameCode,
    formatNameCode,
    NameOwnerCode,
    randomNameOwnerCode,
    parseNameOwnerCode,
    formatNameOwnerCode,
    nameOwnerCodeEntropy,
    NamePrepaidCode,
    randomNamePrepaidCode,
    namePrepaidCodeText,
    namePrepaidCodeHash,
    namePrepaidCodeLength,
  )
where

import Control.Concurrent.STM
import Control.Monad (guard)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Crypto.Random (ChaChaDRG)
import Data.Bits (shiftR)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (digitToInt, isAlphaNum, toUpper)
import Data.List (elemIndex, foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BIP39 (EntropyStrength (..), WalletEntropy, mkEntropy, randomEntropy, unEntropy)
import Simplex.Messaging.Util (eitherToMaybe, tshow)
import Text.Read (readMaybe)

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

data NameCode = NCOwner NameOwnerCode | NCPrepaid NamePrepaidCode
  deriving (Eq, Show)

nameCodePrefix :: Text
nameCodePrefix = "SN"

-- | Read either kind of name code as typed, as 'parseBadgeCode' reads a badge code.
parseNameCode :: Text -> Maybe NameCode
parseNameCode t = do
  body <- T.stripPrefix nameCodePrefix $ T.toUpper $ T.filter isAlphaNum t
  case T.length body of
    n
      | n == nameOwnerCodeLength -> NCOwner <$> parseOwnerBody body
      | n - codeLength `elem` [1 .. length (show maxPrepaidNameLength)] -> NCPrepaid <$> parsePrepaidBody body
      | otherwise -> Nothing

-- | The code as it is shown and printed.
formatNameCode :: NameCode -> Text
formatNameCode = \case
  NCOwner c -> formatNameOwnerCode c
  NCPrepaid (NamePrepaidCode len body) -> T.intercalate "-" $ (nameCodePrefix <> tshow len) : T.chunksOf groupLength body

newtype NameOwnerCode = NameOwnerCode WalletEntropy
  deriving (Eq, Show)

nameOwnerCodeLength :: Int
nameOwnerCodeLength = 30

nameOwnerCodeChecksumBits :: Int
nameOwnerCodeChecksumBits = 22

randomNameOwnerCode :: TVar ChaChaDRG -> IO NameOwnerCode
randomNameOwnerCode drg = NameOwnerCode <$> atomically (randomEntropy ES128 drg)

parseNameOwnerCode :: Text -> Maybe NameOwnerCode
parseNameOwnerCode t =
  parseNameCode t >>= \case
    NCOwner c -> Just c
    NCPrepaid _ -> Nothing

parseOwnerBody :: Text -> Maybe NameOwnerCode
parseOwnerBody body = do
  vs <- mapM charValue $ T.unpack body
  let (payload, checksum) = foldl' (\acc v -> acc * toInteger base + toInteger v) 0 vs `divMod` (2 ^ nameOwnerCodeChecksumBits)
  guard $ checksum == nameOwnerCodeChecksum payload
  NameOwnerCode <$> eitherToMaybe (mkEntropy $ i2ospOf_ 16 payload)

formatNameOwnerCode :: NameOwnerCode -> Text
formatNameOwnerCode (NameOwnerCode e) = T.intercalate "-" $ nameCodePrefix : T.chunksOf groupLength (T.pack $ map valueChar values)
  where
    payload = os2ip $ unEntropy e
    n = payload * 2 ^ nameOwnerCodeChecksumBits + nameOwnerCodeChecksum payload
    values = reverse $ take nameOwnerCodeLength $ map (fromInteger . (`mod` toInteger base)) $ iterate (`div` toInteger base) n

nameOwnerCodeEntropy :: NameOwnerCode -> WalletEntropy
nameOwnerCodeEntropy (NameOwnerCode e) = e

nameOwnerCodeChecksum :: Integer -> Integer
nameOwnerCodeChecksum = (`shiftR` (256 - nameOwnerCodeChecksumBits)) . os2ip . C.sha256Hash . i2ospOf_ 16

-- | A prepaid code that has passed its check: the shortest name length it pays for and the
-- 20 canonical characters after it.
data NamePrepaidCode = NamePrepaidCode Int Text
  deriving (Eq, Show)

minPrepaidNameLength :: Int
minPrepaidNameLength = 6

maxPrepaidNameLength :: Int
maxPrepaidNameLength = 63

-- | The length must be 6 to 63, or the code will not parse.
randomNamePrepaidCode :: TVar ChaChaDRG -> Int -> IO NamePrepaidCode
randomNamePrepaidCode drg len = do
  payload <- randomPayload drg
  let vs = payload <> [checkValue $ lengthValues len <> payload]
  pure $ NamePrepaidCode len $ T.pack $ map valueChar vs

parsePrepaidBody :: Text -> Maybe NamePrepaidCode
parsePrepaidBody body = do
  let (lenText, codeText) = T.splitAt (T.length body - codeLength) body
  len <- readMaybe $ T.unpack lenText
  -- a leading zero adds nothing to the check, so it would give the same code a second text and hash
  guard $ tshow len == lenText && len >= minPrepaidNameLength && len <= maxPrepaidNameLength
  vs <- mapM charValue $ T.unpack codeText
  let (payload, checkChar) = splitAt (codeLength - 1) vs
  guard $ checkChar == [checkValue $ lengthValues len <> payload]
  Just $ NamePrepaidCode len $ T.pack $ map valueChar vs

lengthValues :: Int -> [Int]
lengthValues = map digitToInt . show

-- | The canonical form, "SN", the length and the 20 characters: the only representation hashed.
namePrepaidCodeText :: NamePrepaidCode -> Text
namePrepaidCodeText (NamePrepaidCode len body) = nameCodePrefix <> tshow len <> body

-- | SHA-256 over the ASCII bytes of the canonical form, as 'badgeCodeHash'.
namePrepaidCodeHash :: NamePrepaidCode -> ByteString
namePrepaidCodeHash = C.sha256Hash . encodeUtf8 . namePrepaidCodeText

namePrepaidCodeLength :: NamePrepaidCode -> Int
namePrepaidCodeLength (NamePrepaidCode len _) = len
