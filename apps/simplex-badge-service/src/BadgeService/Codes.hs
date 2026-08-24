{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure, database-free primitives for redemption codes (docs/protocol/badges-web.md): the
-- Crockford base32 encoding with its check character, deriving an order's code from the
-- badge service's long-lived secret, generating a batch code, normalizing whatever a user
-- typed back to canonical form, hashing that form for lookup, and classifying the row
-- 'BadgeService.Store.getCodeByHash' returns into a 'RedeemOutcome'. The lookup and every
-- write live in B7; this module supplies only what does not touch the database.
--
-- A code is a bearer secret to a paid good: its 95 bits of entropy are the load-bearing
-- defence, everything here only shapes honest traffic. Two properties matter more than the
-- rest: a code failing the check character is rejected in 'classifyRedemption' before
-- 'lookupCode' is ever forced, so guessing costs an attacker no database round trip on 31 of
-- every 32 attempts; and 'RedeemInvalid'/'RedeemRevoked' both exist so support tooling can
-- tell an unknown code from a revoked one, but every caller outside this module maps both to
-- the same wire error, so a guesser cannot learn that a code once existed.
module BadgeService.Codes
  ( RedeemOutcome (..),
    deriveOrderCode,
    generateBatchCode,
    normalizeCode,
    codeHash,
    verifyChecksum,
    classifyRedemption,
    loadCodeSecret,
    decodeCodeSecret,
  )
where

-- 'BadgeService.Store' also defines 'BadgeService.Store.NewBadgeCode' and
-- 'BadgeService.Store.NewIssuance' with overlapping field names ('badgeType', 'months',
-- 'expiresAt', ...): the fields below are only ever used inside a 'BadgeCode { .. }' pattern
-- (NamedFieldPuns), where the named constructor makes the choice unambiguous; none is used as
-- a bare selector function, which is the one form DuplicateRecordFields cannot disambiguate.
import BadgeService.Store (BadgeCode (BadgeCode, badgeType, expiresAt, months, redeemedPurchaseId, revokedAt))
import Control.Concurrent.STM (TVar, atomically)
import qualified Crypto.MAC.HMAC as HMAC
import Crypto.Hash (SHA256)
import Crypto.Random (ChaChaDRG)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8
import Data.Bits (testBit)
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.List (elemIndex, foldl')
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Simplex.Chat.Badges (BadgeType)
import qualified Simplex.Messaging.Crypto as C
import System.Exit (exitFailure)

-- | The outcome of redeeming a presented code, resolved against the row
-- 'BadgeService.Store.getCodeByHash' returns and the purchase key making the request.
-- 'RedeemInvalid' and 'RedeemRevoked' are kept distinct here for support tooling
-- (@codes status@, H2) but both map to the same @code_invalid@ wire error later — a revoked
-- code must read exactly like one that never existed. 'RedeemUsedByOther' maps to
-- @code_used@, 'RedeemExpired' to @code_expired@. 'RedeemAlreadyRedeemedBySameKey' is not an
-- error: it carries the purchase id whose cached credential B7 replays. 'RedeemOk' carries
-- the badge type and the number of months to credit.
data RedeemOutcome
  = RedeemOk BadgeType Int
  | RedeemInvalid
  | RedeemRevoked
  | RedeemUsedByOther
  | RedeemAlreadyRedeemedBySameKey Int64
  | RedeemExpired
  deriving (Eq, Show)

-- Crockford base32 -----------------------------------------------------------------------

-- | Crockford's base32 alphabet: 32 symbols, excluding 'I', 'L', 'O' and 'U' (easily confused
-- with '1', '1', '0' and 'V'). Index in this list is the 5-bit value.
crockfordAlphabet :: String
crockfordAlphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

alphabetSize :: Int
alphabetSize = length crockfordAlphabet

charValue :: Char -> Maybe Word8
charValue c = fromIntegral <$> elemIndex c crockfordAlphabet

valueChar :: Word8 -> Char
valueChar v = crockfordAlphabet !! fromIntegral v

-- | 19 data characters, carrying exactly 95 bits.
dataChars :: Int
dataChars = 19

bitsPerChar :: Int
bitsPerChar = 5

dataBits :: Int
dataBits = dataChars * bitsPerChar

-- | The unweighted sum of the 19 data values, mod 32, encoded in the same alphabet. This is
-- deliberately not Crockford's own mod-37 check symbol, which needs five symbols outside the
-- alphabet: an unweighted sum mod 32 detects every single-character substitution (changing one
-- value changes the sum by a nonzero amount less than 32, so it never wraps back to the same
-- residue) but not transpositions. That is accepted, not a bug to fix.
checksumOf :: [Word8] -> Word8
checksumOf values = fromIntegral (sum (map fromIntegral values :: [Int]) `mod` alphabetSize)

-- | Big-endian bits of a byte string, most significant bit first.
bitsOf :: ByteString -> [Bool]
bitsOf = concatMap byteBits . BS.unpack
  where
    byteBits w = [testBit w i | i <- [7, 6 .. 0]]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

bitsToValue :: [Bool] -> Word8
bitsToValue = foldl' (\acc b -> acc * 2 + if b then 1 else 0) 0

-- | The first 95 bits of a byte string (at least 12 bytes) as 19 data characters plus the
-- check character: 20 characters total, undashed and unprefixed.
encodeDataBytes :: ByteString -> Text
encodeDataBytes source =
  let dataValues = map bitsToValue (chunksOf bitsPerChar (take dataBits (bitsOf source)))
      checkValue = checksumOf dataValues
   in T.pack (map valueChar (dataValues ++ [checkValue]))

-- | Display prefix: 'S', 'X' and 'B' are themselves valid data characters, which is exactly
-- why 'normalizeCode' only strips it when doing so leaves a full 20-character code behind.
codePrefix :: Text
codePrefix = "SXB"

groupSize :: Int
groupSize = 5

textChunksOf :: Int -> Text -> [Text]
textChunksOf n t
  | T.null t = []
  | otherwise = let (a, b) = T.splitAt n t in a : textChunksOf n b

-- | @SXB-XXXXX-XXXXX-XXXXX-XXXXX@ from a 20-character undashed code.
formatCode :: Text -> Text
formatCode raw = codePrefix <> "-" <> T.intercalate "-" (textChunksOf groupSize raw)

-- Order and batch codes -----------------------------------------------------------------------

hmacSha256 :: ByteString -> ByteString -> ByteString
hmacSha256 key msg = BA.convert (HMAC.hmacGetDigest (HMAC.hmac key msg :: HMAC.HMAC SHA256))

-- | @code = encode (HMAC_SHA256 codeSecret orderId)@, truncated to the 95 bits the format
-- carries plus its check character. Fully recomputable from 'codeSecret' and @orderId@ alone,
-- so a browser reload is answerable with no plaintext code ever at rest, and so rotating
-- 'codeSecret' breaks recomputation for every order minted under the old one (H5).
deriveOrderCode :: ByteString -> Text -> Text
deriveOrderCode secret orderId = formatCode (encodeDataBytes (hmacSha256 secret (encodeUtf8 orderId)))

-- | Enough entropy for the 95 data bits the format carries, with a byte to spare.
randomCodeBytes :: Int
randomCodeBytes = 12

-- | A fresh code from 'C.randomBytes', same encoding as an order code. Printed once by B8 and
-- never recoverable — nothing here persists the plaintext.
generateBatchCode :: TVar ChaChaDRG -> IO Text
generateBatchCode drg = formatCode . encodeDataBytes <$> atomically (C.randomBytes randomCodeBytes drg)

-- Normalization, hashing and checksum -----------------------------------------------------------------------

-- | Upper-cases, strips '-' and whitespace, strips a leading @SXB@ only when doing so leaves
-- exactly 20 characters — 'S', 'X' and 'B' are themselves valid data characters, so an
-- unconditional strip would corrupt a bare 20-character code whose first three happen to spell
-- @SXB@ — and finally folds 'I'/'L' to \'1\' and 'O' to \'0\'. A code is accepted with or
-- without the prefix.
normalizeCode :: Text -> Text
normalizeCode raw =
  T.map foldAmbiguous unprefixed
  where
    stripped = T.filter (\c -> c /= '-' && not (isSpace c)) (T.toUpper raw)
    unprefixed
      | codePrefix `T.isPrefixOf` stripped && T.length stripped == 3 + dataChars + 1 = T.drop 3 stripped
      | otherwise = stripped
    foldAmbiguous 'I' = '1'
    foldAmbiguous 'L' = '1'
    foldAmbiguous 'O' = '0'
    foldAmbiguous c = c

-- | SHA-256 over a normalized code (the output of 'normalizeCode'), used both as the storage
-- key and the lookup key, so every presentation of the same code resolves to the same row.
codeHash :: Text -> ByteString
codeHash = C.sha256Hash . encodeUtf8

-- | True when a normalized 20-character code's check character matches the unweighted sum of
-- its 19 data values, mod 32. False for anything the wrong length or containing a character
-- outside the alphabet.
verifyChecksum :: Text -> Bool
verifyChecksum code = case traverse charValue (T.unpack code) of
  Just values | length values == dataChars + 1 -> case splitAt dataChars values of
    (dataValues, [checkValue]) -> checkValue == checksumOf dataValues
    _ -> False
  _ -> False

-- Classification -----------------------------------------------------------------------

-- | Normalizes and checksum-verifies a presented code, and only then calls @lookupCode@ — a
-- checksum failure returns 'RedeemInvalid' without @lookupCode@ ever being forced, so 31 of
-- every 32 random guesses cost no database round trip. @lookupCode@ is
-- 'BadgeService.Store.getCodeByHash' partially applied to its connection; the 'Monad' is left
-- abstract so a test can pass a stub that fails if it is ever called.
classifyRedemption ::
  Monad m =>
  UTCTime ->
  C.PublicKeyEd25519 ->
  (ByteString -> m (Maybe (BadgeCode, Maybe C.PublicKeyEd25519))) ->
  Text ->
  m RedeemOutcome
classifyRedemption now purchaseKey lookupCode presentedCode
  | verifyChecksum normalized = classifyRow now purchaseKey <$> lookupCode (codeHash normalized)
  | otherwise = pure RedeemInvalid
  where
    normalized = normalizeCode presentedCode

-- | The row-level classification: 'Nothing' (an unknown hash, or the checksum-failure case
-- above) and a revoked row both become 'RedeemInvalid'\/'RedeemRevoked' respectively — kept
-- distinct here, collapsed to the same wire error by every caller outside this module.
classifyRow :: UTCTime -> C.PublicKeyEd25519 -> Maybe (BadgeCode, Maybe C.PublicKeyEd25519) -> RedeemOutcome
classifyRow _ _ Nothing = RedeemInvalid
classifyRow now purchaseKey (Just (BadgeCode {badgeType, months, expiresAt, redeemedPurchaseId, revokedAt}, redeemerKey))
  | isJust revokedAt = RedeemRevoked
  | Just pid <- redeemedPurchaseId =
      if redeemerKey == Just purchaseKey then RedeemAlreadyRedeemedBySameKey pid else RedeemUsedByOther
  | expiresAt < now = RedeemExpired
  | otherwise = RedeemOk badgeType (fromIntegral months)

-- Secret loading -----------------------------------------------------------------------

minCodeSecretBytes :: Int
minCodeSecretBytes = 32

-- | Standard base64 on one line, trailing whitespace stripped, decoding to at least 32 bytes.
-- Pure so it is testable without a file.
decodeCodeSecret :: ByteString -> Either String ByteString
decodeCodeSecret raw = case B64.decode (B8.dropWhileEnd isSpace raw) of
  Left err -> Left ("invalid base64: " <> err)
  Right decoded
    | BS.length decoded < minCodeSecretBytes ->
        Left ("must decode to at least " <> show minCodeSecretBytes <> " bytes, got " <> show (BS.length decoded))
    | otherwise -> Right decoded

-- | Reads and decodes '[codes] secret_file' (A6), exiting the process on a bad secret: this is
-- the long-lived HMAC key behind every order-derived code, so a misconfigured deploy must fail
-- at startup, not at the first redemption.
loadCodeSecret :: FilePath -> IO ByteString
loadCodeSecret path = do
  raw <- B8.readFile path
  case decodeCodeSecret raw of
    Right secret -> pure secret
    Left err -> putStrLn ("codes secret_file " <> path <> ": " <> err) >> exitFailure
