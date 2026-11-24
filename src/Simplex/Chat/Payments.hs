{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.Payments
  ( PaymentRequest (..),
    PaymentPayee (..),
    PaymentNetwork (..),
    LightningPayee (..),
    AddressChecksum (..),
    hasOtherAddress,
  )
where

import Control.Applicative (optional, (<|>))
import Control.Monad (guard, join, unless, when)
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.Bits (bit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteString as BW
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (chr, digitToInt, isAlphaNum, isAscii, isDigit, isHexDigit, isLower, isUpper, toLower)
import Data.Either (rights)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64, Word8)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, parseAll, sumTypeJSON)
import Simplex.Messaging.Util (eitherToMaybe, listToEither, safeDecodeUtf8, tshow, (<$?>))
import qualified URI.ByteString as U

data PaymentRequest = PaymentRequest
  { payee :: PaymentPayee,
    alternatives :: [LightningPayee],
    amount :: Maybe Text,
    token :: Maybe Text,
    label :: Maybe Text,
    message :: Maybe Text,
    walletUri :: Text
  }
  deriving (Eq, Show)

data PaymentPayee
  = PPAddress {network :: PaymentNetwork, address :: Text, checksum :: AddressChecksum}
  | PPLightning {lightning :: LightningPayee}
  deriving (Eq, Show)

data PaymentNetwork
  = PNBitcoin
  | PNEthereum {chainId :: Maybe Int64}
  | PNMonero
  | PNZcash
  | PNSolana
  | PNLitecoin
  | PNDogecoin
  | PNBitcoinCash
  deriving (Eq, Show)

data LightningPayee
  = LPInvoice {invoice :: Text, description :: Maybe Text, expiresAt :: UTCTime}
  | LPOffer {offer :: Text}
  | LPLnurl {lnurl :: Text, host :: Text}
  deriving (Eq, Show)

data AddressChecksum = ACVerified | ACUnchecked
  deriving (Eq, Show)

instance StrEncoding PaymentRequest where
  strEncode PaymentRequest {walletUri} = encodeUtf8 walletUri
  strDecode s = case B.break (== ':') s of
    (scheme, rest) | not (B.null rest) -> paymentUri (B.map toLower scheme) (B.drop 1 rest)
    _ -> listToEither "not a payment address" $ filter hasChecksum $ addressRequests s
  strP = strDecode <$?> A.takeTill (\c -> c == ' ' || c == '\n')

hasOtherAddress :: PaymentRequest -> Text -> Bool
hasOtherAddress PaymentRequest {payee, alternatives, token} t = any (`notElem` requestAddresses) textAddresses
  where
    requestAddresses = payeeAddress payee : map lightningAddress alternatives <> maybeToList token
    textAddresses = [payeeAddress p | w <- T.split (not . isAlphaNum) t, not (T.null w), PaymentRequest {payee = p} <- addressRequests $ encodeUtf8 w]

addressRequests :: ByteString -> [PaymentRequest]
addressRequests s
  | B.all (\c -> isAscii c && isAlphaNum c) s = rights $ map (`paymentUri` s) ["bitcoin", "lightning", "ethereum", "monero", "zcash", "litecoin", "dogecoin", "bitcoincash", "solana"]
  | otherwise = []

hasChecksum :: PaymentRequest -> Bool
hasChecksum PaymentRequest {payee} = case payee of
  PPAddress {checksum} -> checksum == ACVerified
  PPLightning {lightning} -> case lightning of
    LPInvoice {} -> True
    LPOffer {} -> False
    LPLnurl {} -> True

payeeAddress :: PaymentPayee -> Text
payeeAddress = \case
  PPAddress {address} -> address
  PPLightning {lightning} -> lightningAddress lightning

lightningAddress :: LightningPayee -> Text
lightningAddress = \case
  LPInvoice {invoice} -> invoice
  LPOffer {offer} -> offer
  LPLnurl {lnurl} -> lnurl

type QueryParam = (ByteString, ByteString)

paymentUri :: ByteString -> ByteString -> Either String PaymentRequest
paymentUri scheme body = do
  unless (B.all uriChar body) $ Left "invalid URI character"
  params <- queryParams $ B.drop 1 query
  when (any (B.isPrefixOf "req-" . paramKey) params) $ Left "unknown required parameter"
  (keys, request) <- schemeRequest scheme path params
  when (any (\k -> k `notElem` ["lightning", "lno"] && length (paramValues k params) > 1) keys) $ Left "repeated parameter"
  pure $ request $ safeDecodeUtf8 $ scheme <> ":" <> path <> walletQuery keys params
  where
    (path, query) = B.break (== '?') $ fromMaybe body $ B.stripPrefix "//" body

queryParams :: ByteString -> Either String [QueryParam]
queryParams q
  | B.null q = Right []
  | otherwise = mapM queryParam $ B.split '&' q
  where
    queryParam p = case B.break (== '=') p of
      (k, v) | not (B.null k) -> Right (k, v)
      _ -> Left "empty parameter"

paramKey :: QueryParam -> ByteString
paramKey = B.map toLower . fst

paramValues :: ByteString -> [QueryParam] -> [ByteString]
paramValues k ps = [B.drop 1 v | p@(_, v) <- ps, paramKey p == k]

param :: ByteString -> [QueryParam] -> Maybe ByteString
param k = listToMaybe . paramValues k

walletQuery :: [ByteString] -> [QueryParam] -> ByteString
walletQuery keys = \case
  [] -> ""
  ps -> "?" <> B.intercalate "&" (map normalized ps)
  where
    normalized p@(k, v) = (if paramKey p `elem` keys then paramKey p else k) <> v

schemeRequest :: ByteString -> ByteString -> [QueryParam] -> Either String ([ByteString], Text -> PaymentRequest)
schemeRequest scheme path ps = case scheme of
  "bitcoin" -> bitcoinRequest
  "litecoin" -> onChainRequest PNLitecoin $ litecoinAddress path
  "dogecoin" -> onChainRequest PNDogecoin $ dogecoinAddress path
  "bitcoincash" -> onChainRequest PNBitcoinCash $ cashAddress path
  "zcash" -> zcashRequest
  "monero" -> moneroRequest
  "solana" -> solanaRequest
  "ethereum" -> ethereumRequest
  "lightning" -> lightningRequest
  _ -> Left "not a payment URI"
  where
    bip21Keys = ["amount", "label", "message"]
    bip21Params = (,,) <$> traverse (decimalAmount 8) (param "amount" ps) <*> paramText "label" <*> paramText "message"
    paramText k = join <$> traverse percentText (param k ps)
    onChainRequest network address_ = do
      a <- required "invalid address" address_
      (amount, label, message) <- bip21Params
      pure (bip21Keys, PaymentRequest (PPAddress network a ACVerified) [] amount Nothing label message)
    bitcoinRequest = do
      invoices <- mapM (required "invalid lightning payment" . lightningPayee) $ paramValues "lightning" ps
      offers <- mapM (required "invalid offer" . fmap (,Nothing) . offerPayee) $ paramValues "lno" ps
      let instructions = invoices <> offers
      (payee, alternatives, payeeAmount) <-
        if B.null path
          then case instructions of
            (p, a) : more -> Right (PPLightning p, map fst more, a)
            [] -> Left "no payee"
          else (\a -> (PPAddress PNBitcoin a ACVerified, map fst instructions, Nothing)) <$> required "invalid address" (bitcoinAddress path)
      (amount, label, message) <- bip21Params
      pure (bip21Keys <> ["lightning", "lno"], PaymentRequest payee alternatives (amount <|> payeeAmount) Nothing label message)
    zcashRequest = do
      when (any (B.elem '.' . fst) ps) $ Left "several payments"
      addressText <- case (B.null path, param "address" ps) of
        (True, Just a) -> Right a
        (False, Nothing) -> Right path
        _ -> Left "one address required"
      (a, memoAllowed) <- required "invalid address" $ (,False) <$> zcashTransparent addressText <|> (,True) <$> zcashShielded addressText
      memo <- traverse memoText $ param "memo" ps
      when (isJust memo && not memoAllowed) $ Left "memo with a transparent address"
      (amount, label, message) <- bip21Params
      pure (bip21Keys <> ["address", "memo"], PaymentRequest (PPAddress PNZcash a ACVerified) [] amount Nothing label (message <|> join memo))
    moneroRequest = do
      when (B.elem ';' path || any (B.elem ';' . snd) ps) $ Left "several recipients"
      a <- required "invalid address" $ moneroAddress path
      amount <- traverse (decimalAmount 12) $ param "tx_amount" ps
      label <- paramText "recipient_name"
      message <- paramText "tx_description"
      pure (["tx_amount", "recipient_name", "tx_description"], PaymentRequest (PPAddress PNMonero a ACVerified) [] amount Nothing label message)
    solanaRequest = do
      a <- required "invalid address" $ solanaAddress path
      token <- traverse (required "invalid token" . solanaAddress) $ param "spl-token" ps
      amount <- traverse (decimalAmount $ if isJust token then maxBound else 9) $ param "amount" ps
      label <- paramText "label"
      message <- paramText "message"
      memo <- paramText "memo"
      pure (["amount", "spl-token", "label", "message", "memo"], PaymentRequest (PPAddress PNSolana a ACUnchecked) [] amount token label (message <|> memo))
    ethereumRequest = do
      (target, chainId, function) <- parseAll ethereumPathP path
      (targetAddress, targetChecksum) <- required "invalid address" $ evmAddress target
      let network = PNEthereum chainId
      case function of
        Nothing -> do
          amount <- traverse (fmap (decimalText 18) . evmInteger) $ param "value" ps
          pure (["value"], PaymentRequest (PPAddress network targetAddress targetChecksum) [] amount Nothing Nothing Nothing)
        Just "transfer" -> do
          unless (isNothing $ param "value" ps) $ Left "value in a token transfer"
          (a, addressChecksum) <- required "invalid address" $ evmAddress =<< param "address" ps
          amount <- traverse (fmap tshow . evmInteger) $ param "uint256" ps
          pure (["address", "uint256"], PaymentRequest (PPAddress network a addressChecksum) [] amount (Just targetAddress) Nothing Nothing)
        Just _ -> Left "unsupported function"
    lightningRequest = do
      (payee, amount) <- required "invalid lightning payment" $ lightningPayee path <|> (,Nothing) <$> offerPayee path
      pure ([], PaymentRequest (PPLightning payee) [] amount Nothing Nothing Nothing)

ethereumPathP :: A.Parser (ByteString, Maybe Int64, Maybe ByteString)
ethereumPathP = do
  target <- optional (A.string "pay-") *> A.takeWhile1 (\c -> c /= '@' && c /= '/')
  chainId <- optional $ A.char '@' *> chainIdP
  function <- optional $ A.char '/' *> A.takeByteString
  pure (target, chainId, function)
  where
    chainIdP =
      A.takeWhile1 isDigit >>= \digits -> case B.readInteger digits of
        Just (n, "") | n > 0 && n <= toInteger (maxBound :: Int64) -> pure $ fromInteger n
        _ -> fail "invalid chain ID"

bitcoinAddress :: ByteString -> Maybe Text
bitcoinAddress s = segwitAddress "bc" s <|> base58Address ["\0", "\5"] s

litecoinAddress :: ByteString -> Maybe Text
litecoinAddress s = segwitAddress "ltc" s <|> base58Address ["\48", "\50", "\5"] s

dogecoinAddress :: ByteString -> Maybe Text
dogecoinAddress = base58Address ["\30", "\22"]

zcashTransparent :: ByteString -> Maybe Text
zcashTransparent = base58Address ["\x1c\xb8", "\x1c\xbd"]

zcashShielded :: ByteString -> Maybe Text
zcashShielded s = bech32Address "zs" Bech32 (== 43) s <|> bech32Address "u" Bech32m (> 0) s

cashAddress :: ByteString -> Maybe Text
cashAddress s = do
  guard $ B.length s == 42 && singleCase s
  values <- mapM bech32Value $ B.unpack $ B.map toLower s
  guard $ bchPolymod 35 cashAddrGenerators (map (.&. 31) (BW.unpack "bitcoincash") <> [0] <> values) == 1
  payload <- fromBase32 $ take 34 values
  guard $ B.length payload == 21 && B.take 1 payload `elem` ["\0", "\8"]
  pure $ lowerText s

moneroAddress :: ByteString -> Maybe Text
moneroAddress s = do
  networks <- lookup (B.length s) [(95, ["\18", "\42"]), (106, ["\19"])]
  bytes <- B.concat <$> mapM moneroBlock (blocks s)
  let (payload, check) = B.splitAt (B.length bytes - 4) bytes
  guard $ check == B.take 4 (C.keccak256 payload) && B.take 1 payload `elem` networks
  pure $ safeDecodeUtf8 s
  where
    blocks b = if B.null b then [] else B.take 11 b : blocks (B.drop 11 b)
    moneroBlock block = do
      size <- lookup (B.length block) [(2, 1), (3, 2), (5, 3), (6, 4), (7, 5), (9, 6), (10, 7), (11, 8)]
      n <- base58Integer block
      guard $ n < 256 ^ size
      pure $ integerBytes size n

solanaAddress :: ByteString -> Maybe Text
solanaAddress s = do
  guard $ B.length s >= 32 && B.length s <= 44
  bytes <- base58Decode s
  guard $ B.length bytes == 32
  pure $ safeDecodeUtf8 s

evmAddress :: ByteString -> Maybe (Text, AddressChecksum)
evmAddress s = do
  hex <- B.stripPrefix "0x" s
  a <- eitherToMaybe (strDecode s :: Either String Address)
  pure (safeDecodeUtf8 $ strEncode a, if B.any isUpper hex && B.any isLower hex then ACVerified else ACUnchecked)

lightningPayee :: ByteString -> Maybe (LightningPayee, Maybe Text)
lightningPayee s = invoicePayee s <|> (,Nothing) <$> lnurlPayee s

invoicePayee :: ByteString -> Maybe (LightningPayee, Maybe Text)
invoicePayee s = do
  hrp <- B.stripSuffix "1" $ fst $ B.breakEnd (== '1') $ B.map toLower s
  amount <- invoiceAmount =<< B.stripPrefix "lnbc" hrp
  (Bech32, values) <- bech32Decode hrp s
  guard $ length values >= 7 + 104
  let (timestamp, rest) = splitAt 7 values
  fields <- taggedFields $ take (length rest - 104) rest
  description <- traverse utf8Field $ lookup 13 fields
  expiry <- maybe (Just 3600) expirySeconds $ lookup 6 fields
  pure (LPInvoice {invoice = lowerText s, description = nonEmptyText =<< description, expiresAt = posixSecondsToUTCTime $ fromInteger $ base32Integer timestamp + expiry}, amount)
  where
    utf8Field v = eitherToMaybe . decodeUtf8' =<< fromBase32 v
    expirySeconds v = base32Integer v <$ guard (length v <= 13)

invoiceAmount :: ByteString -> Maybe (Maybe Text)
invoiceAmount hrpAmount
  | B.null hrpAmount = Just Nothing
  | otherwise = do
      let (digits, multiplier) = B.span isDigit hrpAmount
      guard $ not $ B.null digits
      places <- case multiplier of
        "" -> Just 0
        "m" -> Just 3
        "u" -> Just 6
        "n" -> Just 9
        "p" | "0" `B.isSuffixOf` digits -> Just 12
        _ -> Nothing
      Just $ Just $ decimalText places $ digitsInteger digits

taggedFields :: [Word8] -> Maybe [(Word8, [Word8])]
taggedFields = \case
  [] -> Just []
  t : l1 : l2 : rest ->
    let len = fromIntegral l1 * 32 + fromIntegral l2
        (field, rest') = splitAt len rest
     in if length field == len then ((t, field) :) <$> taggedFields rest' else Nothing
  _ -> Nothing

lnurlPayee :: ByteString -> Maybe LightningPayee
lnurlPayee s = do
  (Bech32, values) <- bech32Decode "lnurl" s
  U.URI {uriScheme = U.Scheme scheme, uriAuthority = Just U.Authority {authorityHost = U.Host h}} <- eitherToMaybe . U.parseURI U.laxURIParserOptions =<< fromBase32 values
  guard $ B.map toLower scheme == "https" || (B.map toLower scheme == "http" && ".onion" `B.isSuffixOf` B.map toLower h)
  pure LPLnurl {lnurl = lowerText s, host = safeDecodeUtf8 h}

offerPayee :: ByteString -> Maybe LightningPayee
offerPayee s = do
  offerData <- B.stripPrefix "lno1" $ B.map toLower s
  guard $ singleCase s && not (B.null offerData) && B.all (`B.elem` bech32Chars) offerData
  pure LPOffer {offer = lowerText s}

data Bech32Encoding = Bech32 | Bech32m
  deriving (Eq)

segwitAddress :: ByteString -> ByteString -> Maybe Text
segwitAddress hrp s = do
  guard $ B.length s <= 90
  (encoding, version : program5) <- bech32Decode hrp s
  program <- fromBase32 program5
  let len = B.length program
  guard $ version <= 16 && len >= 2 && len <= 40 && if version == 0 then encoding == Bech32 && (len == 20 || len == 32) else encoding == Bech32m
  pure $ lowerText s

bech32Address :: ByteString -> Bech32Encoding -> (Int -> Bool) -> ByteString -> Maybe Text
bech32Address hrp encoding validLength s = do
  (encoding', values) <- bech32Decode hrp s
  bytes <- fromBase32 values
  guard $ encoding' == encoding && validLength (B.length bytes)
  pure $ lowerText s

bech32Decode :: ByteString -> ByteString -> Maybe (Bech32Encoding, [Word8])
bech32Decode hrp s = do
  dataPart <- B.stripPrefix (hrp <> "1") $ B.map toLower s
  guard $ singleCase s && B.length dataPart >= 6
  values <- mapM bech32Value $ B.unpack dataPart
  encoding <- case bchPolymod 25 bech32Generators (map (`shiftR` 5) (BW.unpack hrp) <> [0] <> map (.&. 31) (BW.unpack hrp) <> values) of
    1 -> Just Bech32
    0x2bc830a3 -> Just Bech32m
    _ -> Nothing
  pure (encoding, take (length values - 6) values)

bech32Value :: Char -> Maybe Word8
bech32Value c = fromIntegral <$> B.elemIndex c bech32Chars

bech32Chars :: ByteString
bech32Chars = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

bech32Generators :: [Word64]
bech32Generators = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]

cashAddrGenerators :: [Word64]
cashAddrGenerators = [0x98f2bc8e61, 0x79b76d99e2, 0xf33e5fb3c4, 0xae2eabe2a8, 0x1e4f43e470]

bchPolymod :: Int -> [Word64] -> [Word8] -> Word64
bchPolymod top generators = foldl' step 1
  where
    step c v = foldl' (\c' (i, g) -> if testBit c (top + i) then c' `xor` g else c') (((c .&. (bit top - 1)) `shiftL` 5) `xor` fromIntegral v) (zip [0 ..] generators)

fromBase32 :: [Word8] -> Maybe ByteString
fromBase32 = go 0 0 []
  where
    go :: Word64 -> Int -> [Word8] -> [Word8] -> Maybe ByteString
    go acc bits out = \case
      v : vs
        | bits >= 3 -> go (acc' .&. (bit bits' - 1)) bits' (fromIntegral (acc' `shiftR` bits') : out) vs
        | otherwise -> go acc' (bits + 5) out vs
        where
          acc' = (acc `shiftL` 5) .|. fromIntegral v
          bits' = bits - 3
      [] -> if bits < 5 && acc == 0 then Just $ BW.pack $ reverse out else Nothing

base32Integer :: [Word8] -> Integer
base32Integer = foldl' (\n v -> n * 32 + toInteger v) 0

base58Address :: [ByteString] -> ByteString -> Maybe Text
base58Address versions s = do
  guard $ B.length s >= 25 && B.length s <= 36
  bytes <- base58Decode s
  let (payload, check) = B.splitAt (B.length bytes - 4) bytes
  guard $ check == B.take 4 (C.sha256Hash $ C.sha256Hash payload) && any (\v -> B.length payload == B.length v + 20 && v `B.isPrefixOf` payload) versions
  pure $ safeDecodeUtf8 s

base58Decode :: ByteString -> Maybe ByteString
base58Decode s = (\n -> B.replicate (B.length $ B.takeWhile (== '1') s) '\0' <> integerBytes (length $ takeWhile (> 0) $ iterate (`shiftR` 8) n) n) <$> base58Integer s

base58Integer :: ByteString -> Maybe Integer
base58Integer = B.foldl' (\n c -> (\n' d -> n' * 58 + toInteger d) <$> n <*> B.elemIndex c base58Chars) (Just 0)

base58Chars :: ByteString
base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

integerBytes :: Int -> Integer -> ByteString
integerBytes size n = BW.pack [fromInteger $ n `shiftR` (8 * i) .&. 255 | i <- [size - 1, size - 2 .. 0]]

evmInteger :: ByteString -> Either String Integer
evmInteger = parseAll $ do
  whole <- A.takeWhile isDigit
  fraction <- A.option "" $ A.char '.' *> A.takeWhile1 isDigit
  power <- A.option "" $ A.satisfy (\c -> c == 'e' || c == 'E') *> A.takeWhile1 isDigit
  let digits = whole <> fraction
      scale = fromInteger (digitsInteger power) - B.length fraction
      (q, r) = if scale >= 0 then (digitsInteger digits * 10 ^ scale, 0) else digitsInteger digits `quotRem` (10 ^ negate scale)
  guard $ not (B.null digits) && B.length digits <= 78 && B.length power <= 2 && r == 0 && q < 2 ^ (256 :: Int)
  pure q

decimalAmount :: Int -> ByteString -> Either String Text
decimalAmount places s = case B.split '.' s of
  [whole] | decimalDigits whole -> Right $ safeDecodeUtf8 s
  [whole, fraction] | decimalDigits whole && decimalDigits fraction && B.length fraction <= places -> Right $ safeDecodeUtf8 s
  _ -> Left "invalid amount"
  where
    decimalDigits d = not (B.null d) && B.all isDigit d

decimalText :: Int -> Integer -> Text
decimalText places n = case T.dropWhileEnd (== '0') $ T.justifyRight places '0' $ tshow fraction of
  "" -> tshow whole
  digits -> tshow whole <> "." <> digits
  where
    (whole, fraction) = n `quotRem` (10 ^ places)

digitsInteger :: ByteString -> Integer
digitsInteger = B.foldl' (\n c -> n * 10 + toInteger (digitToInt c)) 0

memoText :: ByteString -> Either String (Maybe Text)
memoText v = do
  memo <- B64U.decodeUnpadded v
  when (B.length memo > 512) $ Left "memo is too long"
  pure $ nonEmptyText =<< eitherToMaybe (decodeUtf8' memo)

percentText :: ByteString -> Either String (Maybe Text)
percentText v = nonEmptyText <$> (first show . decodeUtf8' =<< percentDecode v)

percentDecode :: ByteString -> Either String ByteString
percentDecode = fmap B.concat . chunks
  where
    chunks s = case B.break (== '%') s of
      (before, "") -> Right [before]
      (before, rest) -> case B.unpack $ B.take 3 rest of
        [_, h, l] | isHexDigit h && isHexDigit l -> ([before, B.singleton $ chr $ digitToInt h * 16 + digitToInt l] <>) <$> chunks (B.drop 3 rest)
        _ -> Left "invalid percent-encoding"

uriChar :: Char -> Bool
uriChar c = isAscii c && (isAlphaNum c || c `B.elem` "-._~:/?@!$&'()*+,;=%")

singleCase :: ByteString -> Bool
singleCase s = not (B.any isUpper s && B.any isLower s)

lowerText :: ByteString -> Text
lowerText = safeDecodeUtf8 . B.map toLower

nonEmptyText :: Text -> Maybe Text
nonEmptyText t = if T.null t then Nothing else Just t

required :: String -> Maybe a -> Either String a
required e = maybe (Left e) Right

$(JQ.deriveJSON (enumJSON $ dropPrefix "AC") ''AddressChecksum)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "PN") ''PaymentNetwork)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "LP") ''LightningPayee)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "PP") ''PaymentPayee)

$(JQ.deriveJSON defaultJSON ''PaymentRequest)
