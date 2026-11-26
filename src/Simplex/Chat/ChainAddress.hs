{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.ChainAddress
  ( ChainAddress (..),
    ChainCurrency (..),
    UnlistedCurrency (..),
    bareAddressForm,
    hyperlinkAddress,
  )
where

import Control.Applicative (optional, (<|>))
import Control.Monad (guard, join, mfilter, unless, when)
import qualified Data.Aeson.TH as JQ
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import Data.Bits (bit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteString as BW
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (chr, digitToInt, isAlphaNum, isAscii, isDigit, isHexDigit, isLower, isUpper, toLower)
import Data.Int (Int64)
import Data.List (find, foldl')
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word64, Word8)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, parseAll, sumTypeJSON)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8, tshow, (<$?>))
import qualified URI.ByteString as U

data ChainAddress = ChainAddress
  { currency :: ChainCurrency,
    address :: Text,
    label :: Maybe Text,
    walletUri :: Text
  }
  deriving (Eq, Show)

data ChainCurrency
  = CYBitcoin {amount :: Maybe Text}
  | CYLightning {amount :: Maybe Text}
  | CYTicker {ticker :: Text, chainName :: Maybe Text, amount :: Maybe Text}
  | CYUnlisted {unlisted :: UnlistedCurrency}
  deriving (Eq, Show)

data UnlistedCurrency = ULChain | ULToken | ULChainAndToken
  deriving (Eq, Show)

instance StrEncoding ChainAddress where
  strEncode ChainAddress {walletUri} = encodeUtf8 walletUri
  strDecode s = maybe (bareAddress s) (fmap fst) $ uriAddress s
  strP = strDecode <$?> A.takeTill (\c -> c == ' ' || c == '\n')

hyperlinkAddress :: Text -> Text -> Maybe (Either String (Maybe Text, ChainAddress))
hyperlinkAddress t l = (>>= shownText) <$> uriAddress (encodeUtf8 l)
  where
    shownText (a, addresses)
      | t == l = Right (Nothing, a)
      | hasOtherAddress addresses t = Left "other address in hyperlink text"
      | otherwise = Right (mfilter (not . hasAddress addresses) (Just t), a)

hasOtherAddress :: [Text] -> Text -> Bool
hasOtherAddress addresses t = any otherAddress $ T.split (\c -> not (isAscii c && isAlphaNum c)) t
  where
    otherAddress w = addressLike (encodeUtf8 w) && T.toLower w `notElem` map T.toLower addresses

hasAddress :: [Text] -> Text -> Bool
hasAddress addresses t = any ((`T.isInfixOf` T.toLower t) . T.toLower) addresses

data BareForm = BareForm
  { selects :: ByteString -> Bool,
    scheme :: ByteString,
    validate :: ByteString -> Maybe (ChainCurrency, Text)
  }

bareForms :: [BareForm]
bareForms =
  [ coinForm (oneCase ["bc1"] $ between 14 90) "bitcoin" bitcoin $ segwitAddress "bc",
    coinForm (anyCase ["1", "3"] $ between 25 36) "bitcoin" bitcoin $ base58Address ["\0", "\5"],
    BareForm (oneCase ["lnbc"] (>= 37)) "lightning" $ fmap (\(a, amount) -> (CYLightning amount, a)) . invoicePayee,
    coinForm (oneCase ["lnurl1"] $ const True) "lightning" (CYLightning Nothing) lnurlPayee,
    coinForm (anyCase ["4", "8"] (`elem` [95, 106])) "monero" (coin "XMR") moneroAddress,
    coinForm (anyCase ["t1", "t3"] $ between 25 36) "zcash" (coin "ZEC") transparentAddress,
    coinForm (oneCase ["zs1"] (== 78)) "zcash" (coin "ZEC") $ saplingAddress "zs",
    coinForm (oneCase ["u1"] $ const True) "zcash" (coin "ZEC") unifiedAddress,
    coinForm (oneCase ["ltc1"] $ between 14 90) "litecoin" (coin "LTC") $ segwitAddress "ltc",
    coinForm (anyCase ["L", "M"] $ between 25 36) "litecoin" (coin "LTC") $ base58Address ["\48", "\50"],
    coinForm (anyCase ["D", "9", "A"] $ between 25 36) "dogecoin" (coin "DOGE") dogecoinAddress,
    coinForm (oneCase ["q", "p"] (== 42)) "bitcoincash" (coin "BCH") cashAddress,
    coinForm mixedCaseHex "ethereum" (CYUnlisted ULChain) evmAddress,
    coinForm (oneCase ["za1"] (== 78)) "flux" (coin "FLUX") $ saplingAddress "za"
  ]
  where
    coinForm selects' scheme' currency address_ = BareForm selects' scheme' $ fmap (currency,) . address_
    bitcoin = CYBitcoin Nothing
    coin t = CYTicker t Nothing Nothing
    mixedCaseHex s = B.length s == 42 && "0x" `B.isPrefixOf` s && B.all isHexDigit (B.drop 2 s) && not (singleCase $ B.drop 2 s)

bareForm :: ByteString -> Maybe BareForm
bareForm s = if alnumWord s then find (`selects` s) bareForms else Nothing

bareAddressForm :: ByteString -> Bool
bareAddressForm = isJust . bareForm

bareAddress :: ByteString -> Either String ChainAddress
bareAddress s = do
  BareForm {scheme, validate} <- required "not an address" $ bareForm s
  (currency, address) <- required "invalid address" $ validate s
  pure ChainAddress {currency, address, label = Nothing, walletUri = safeDecodeUtf8 $ scheme <> ":" <> s}

addressLike :: ByteString -> Bool
addressLike s = isJust (bareForm s) || alnumWord s && any ($ s) uncheckedForms
  where
    uncheckedForms =
      [ \w -> between 32 44 (B.length w) && B.all (`B.elem` base58Chars) w,
        \w -> B.length w == 42 && "0x" `B.isPrefixOf` w && B.all isHexDigit (B.drop 2 w),
        oneCase ["lno1"] $ const True
      ]

alnumWord :: ByteString -> Bool
alnumWord s = B.length s >= 14 && B.all (\c -> isAscii c && isAlphaNum c) s

oneCase :: [ByteString] -> (Int -> Bool) -> ByteString -> Bool
oneCase prefixes validLength s = singleCase s && validLength (B.length s) && any (`B.isPrefixOf` B.map toLower s) prefixes

anyCase :: [ByteString] -> (Int -> Bool) -> ByteString -> Bool
anyCase prefixes validLength s = validLength (B.length s) && any (`B.isPrefixOf` s) prefixes

between :: Int -> Int -> Int -> Bool
between from to n = from <= n && n <= to

data UriAddress = UriAddress
  { currency :: ChainCurrency,
    address :: Text,
    otherAddresses :: [Text],
    label :: Maybe Text,
    message :: Maybe Text
  }

type QueryParam = (ByteString, ByteString)

type SchemeParser = ByteString -> [QueryParam] -> Either String ([ByteString], UriAddress)

uriAddress :: ByteString -> Maybe (Either String (ChainAddress, [Text]))
uriAddress s = do
  guard $ not $ B.null rest
  parser <- lookup scheme' schemeParsers
  pure $ chainUri scheme' parser $ B.drop 1 rest
  where
    (scheme', rest) = first (B.map toLower) $ B.break (== ':') s

chainUri :: ByteString -> SchemeParser -> ByteString -> Either String (ChainAddress, [Text])
chainUri scheme' parser body = do
  unless (B.all uriChar body) $ Left "invalid URI character"
  params <- queryParams $ B.drop 1 query
  when (any (B.isPrefixOf "req-" . paramKey) params) $ Left "unknown required parameter"
  (keys, UriAddress {currency, address, otherAddresses, label, message}) <- parser path params
  when (any (\k -> k `notElem` ["lightning", "lno"] && length (paramValues k params) > 1) keys) $ Left "repeated parameter"
  let addresses = address : otherAddresses
  when (any (hasOtherAddress addresses) $ catMaybes [label, message]) $ Left "other address in label or message"
  pure (ChainAddress {currency, address, label = mfilter (not . hasAddress addresses) label, walletUri = safeDecodeUtf8 $ scheme' <> ":" <> path <> walletQuery keys params}, addresses)
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

paramText :: ByteString -> [QueryParam] -> Either String (Maybe Text)
paramText k ps = join <$> traverse percentText (param k ps)

walletQuery :: [ByteString] -> [QueryParam] -> ByteString
walletQuery keys = \case
  [] -> ""
  ps -> "?" <> B.intercalate "&" (map normalized ps)
  where
    normalized p@(k, v) = (if paramKey p `elem` keys then paramKey p else k) <> v

bip21Keys :: [ByteString]
bip21Keys = ["amount", "label", "message"]

bip21Params :: [QueryParam] -> Either String (Maybe Text, Maybe Text, Maybe Text)
bip21Params ps = (,,) <$> traverse (decimalAmount 8) (param "amount" ps) <*> paramText "label" ps <*> paramText "message" ps

schemeParsers :: [(ByteString, SchemeParser)]
schemeParsers =
  [ ("bitcoin", bitcoinParser),
    ("lightning", lightningParser),
    ("ethereum", ethereumParser),
    ("monero", moneroParser),
    ("zcash", zip321Parser "ZEC" transparentAddress $ \s -> saplingAddress "zs" s <|> unifiedAddress s),
    ("litecoin", coinParser "LTC" litecoinAddress),
    ("dogecoin", coinParser "DOGE" dogecoinAddress),
    ("bitcoincash", coinParser "BCH" cashAddress),
    ("solana", solanaParser),
    ("flux", fluxParser),
    ("zel", fluxParser)
  ]

coinParser :: Text -> (ByteString -> Maybe Text) -> SchemeParser
coinParser ticker address_ path ps = do
  a <- required "invalid address" $ address_ path
  (amount, label, message) <- bip21Params ps
  pure (bip21Keys, UriAddress (CYTicker ticker Nothing amount) a [] label message)

bitcoinParser :: SchemeParser
bitcoinParser path ps = do
  invoices <- mapM (required "invalid lightning payment" . lightningPayee) $ paramValues "lightning" ps
  offers <- mapM (required "invalid offer" . fmap (,Nothing) . offerPayee) $ paramValues "lno" ps
  let instructions = invoices <> offers
  (amount, label, message) <- bip21Params ps
  (currency, a, others) <-
    if B.null path
      then case instructions of
        (p, invoiceAmount') : more -> Right (CYLightning (amount <|> invoiceAmount'), p, map fst more)
        [] -> Left "no payee"
      else (\a -> (CYBitcoin amount, a, map fst instructions)) <$> required "invalid address" (bitcoinAddress path)
  pure (bip21Keys <> ["lightning", "lno"], UriAddress currency a others label message)

lightningParser :: SchemeParser
lightningParser path _ = do
  (a, amount) <- required "invalid lightning payment" $ lightningPayee path <|> (,Nothing) <$> offerPayee path
  pure ([], UriAddress (CYLightning amount) a [] Nothing Nothing)

moneroParser :: SchemeParser
moneroParser path ps = do
  when (B.elem ';' path || any (B.elem ';' . snd) ps) $ Left "several recipients"
  a <- required "invalid address" $ moneroAddress path
  amount <- traverse (decimalAmount 12) $ param "tx_amount" ps
  label <- paramText "recipient_name" ps
  message <- paramText "tx_description" ps
  pure (["tx_amount", "recipient_name", "tx_description"], UriAddress (CYTicker "XMR" Nothing amount) a [] label message)

zip321Parser :: Text -> (ByteString -> Maybe Text) -> (ByteString -> Maybe Text) -> SchemeParser
zip321Parser ticker transparent shielded path ps = do
  when (any (B.elem '.' . fst) ps) $ Left "several payments"
  addressText <- case (B.null path, param "address" ps) of
    (True, Just a) -> Right a
    (False, Nothing) -> Right path
    _ -> Left "one address required"
  (a, memoAllowed) <- required "invalid address" $ (,False) <$> transparent addressText <|> (,True) <$> shielded addressText
  memo <- traverse memoText $ param "memo" ps
  when (isJust memo && not memoAllowed) $ Left "memo with a transparent address"
  (amount, label, message) <- bip21Params ps
  pure (bip21Keys <> ["address", "memo"], UriAddress (CYTicker ticker Nothing amount) a [] label (message <|> join memo))

fluxParser :: SchemeParser
fluxParser path ps = do
  unless (maybe True (== "pay") $ lowerParam "action") $ Left "action other than pay"
  unless (isNothing $ param "callback" ps) $ Left "callback"
  unless (maybe True (`elem` ["flux", "zelcash", "zel"]) $ lowerParam "coin") $ Left "coin other than Flux"
  first (<> ["action", "coin"]) <$> zip321Parser "FLUX" transparentAddress (saplingAddress "za") path ps
  where
    lowerParam k = B.map toLower <$> param k ps

solanaParser :: SchemeParser
solanaParser path ps = do
  a <- required "invalid address" $ solanaAddress path
  mint <- traverse (required "invalid token" . solanaAddress) $ param "spl-token" ps
  currency <- case mint of
    Nothing -> CYTicker "SOL" Nothing <$> amountWith 9
    Just m -> case lookup m solanaTokens of
      Just ListedToken {symbol, decimals} -> CYTicker symbol (Just "Solana") <$> amountWith decimals
      Nothing -> CYUnlisted ULToken <$ amountWith maxBound
  label <- paramText "label" ps
  message <- paramText "message" ps
  memo <- paramText "memo" ps
  pure (["amount", "spl-token", "label", "message", "memo"], UriAddress currency a (maybeToList mint) label (message <|> memo))
  where
    amountWith places = traverse (decimalAmount places) $ param "amount" ps

ethereumParser :: SchemeParser
ethereumParser path ps = do
  (target, chainId, function) <- parseAll ethereumPathP path
  targetAddress <- required "invalid address" $ evmAddress target
  let chain = (`lookup` evmChains) =<< chainId
  case function of
    Nothing -> do
      value <- traverse evmInteger $ param "value" ps
      let currency = maybe (CYUnlisted ULChain) (\ListedChain {shownName, coin} -> CYTicker coin shownName $ decimalText 18 <$> value) chain
      pure (["value"], UriAddress currency targetAddress [] Nothing Nothing)
    Just "transfer" -> do
      unless (isNothing $ param "value" ps) $ Left "value in a token transfer"
      payee <- required "invalid address" $ evmAddress =<< param "address" ps
      units <- traverse evmInteger $ param "uint256" ps
      let currency = case chain of
            Just ListedChain {shownName, tokens} -> maybe (CYUnlisted ULToken) (\ListedToken {symbol, decimals} -> CYTicker symbol shownName $ decimalText decimals <$> units) $ lookup targetAddress tokens
            Nothing -> CYUnlisted ULChainAndToken
      pure (["address", "uint256"], UriAddress currency payee [targetAddress] Nothing Nothing)
    Just _ -> Left "unsupported function"

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

data ListedChain = ListedChain {shownName :: Maybe Text, coin :: Text, tokens :: [(Text, ListedToken)]}

data ListedToken = ListedToken {symbol :: Text, decimals :: Int}

evmChains :: [(Int64, ListedChain)]
evmChains =
  [ (1, ListedChain Nothing "ETH" [usdt "0xdAC17F958D2ee523a2206206994597C13D831ec7" 6, usdc "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48" 6]),
    (10, ListedChain (Just "OP Mainnet") "ETH" [usdt "0x94b008aA00579c1307B0EF2c499aD98a8ce58e58" 6, usdc "0x0b2C639c533813f4Aa9D7837CAf62653d097Ff85" 6]),
    (56, ListedChain (Just "BNB Smart Chain") "BNB" [usdt "0x55d398326f99059fF775485246999027B3197955" 18, usdc "0x8AC76a51cc950d9822D68b83fE1Ad97B32Cd580d" 18]),
    (137, ListedChain (Just "Polygon") "POL" [usdt "0xc2132D05D31c914a87C6611C10748AEb04B58e8F" 6, usdc "0x3c499c542cEF5E3811e1192ce70d8cC03d5c3359" 6]),
    (8453, ListedChain (Just "Base") "ETH" [usdt "0xfde4C96c8593536E31F229EA8f37b2ADa2699bb2" 6, usdc "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913" 6]),
    (42161, ListedChain (Just "Arbitrum One") "ETH" [usdt "0xFd086bC7CD5C481DCC9C85ebE478A1C0b69FCbb9" 6, usdc "0xaf88d065e77c8cC2239327C5EDb3A432268e5831" 6])
  ]
  where
    usdt contract = (contract,) . ListedToken "USDT"
    usdc contract = (contract,) . ListedToken "USDC"

solanaTokens :: [(Text, ListedToken)]
solanaTokens =
  [ ("Es9vMFrzaCERmJfrF4H2FYD4KCoNkY11McCe8BenwNYB", ListedToken "USDT" 6),
    ("EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v", ListedToken "USDC" 6)
  ]

bitcoinAddress :: ByteString -> Maybe Text
bitcoinAddress s = segwitAddress "bc" s <|> base58Address ["\0", "\5"] s

litecoinAddress :: ByteString -> Maybe Text
litecoinAddress s = segwitAddress "ltc" s <|> base58Address ["\48", "\50", "\5"] s

dogecoinAddress :: ByteString -> Maybe Text
dogecoinAddress = base58Address ["\30", "\22"]

transparentAddress :: ByteString -> Maybe Text
transparentAddress = base58Address ["\x1c\xb8", "\x1c\xbd"]

saplingAddress :: ByteString -> ByteString -> Maybe Text
saplingAddress hrp = bech32Address hrp Bech32 (== 43)

unifiedAddress :: ByteString -> Maybe Text
unifiedAddress = bech32Address "u" Bech32m (> 0)

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

evmAddress :: ByteString -> Maybe Text
evmAddress s = do
  guard $ "0x" `B.isPrefixOf` s
  a <- eitherToMaybe (strDecode s :: Either String Address)
  pure $ safeDecodeUtf8 $ strEncode a

lightningPayee :: ByteString -> Maybe (Text, Maybe Text)
lightningPayee s = invoicePayee s <|> (,Nothing) <$> lnurlPayee s

invoicePayee :: ByteString -> Maybe (Text, Maybe Text)
invoicePayee s = do
  hrp <- B.stripSuffix "1" $ fst $ B.breakEnd (== '1') $ B.map toLower s
  amount <- invoiceAmount =<< B.stripPrefix "lnbc" hrp
  (Bech32, values) <- bech32Decode hrp s
  guard $ length values >= 7 + 104
  pure (lowerText s, amount)

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

lnurlPayee :: ByteString -> Maybe Text
lnurlPayee s = do
  (Bech32, values) <- bech32Decode "lnurl" s
  U.URI {uriScheme = U.Scheme scheme', uriAuthority = Just U.Authority {authorityHost = U.Host h}} <- eitherToMaybe . U.parseURI U.laxURIParserOptions =<< fromBase32 values
  guard $ B.map toLower scheme' == "https" || (B.map toLower scheme' == "http" && ".onion" `B.isSuffixOf` B.map toLower h)
  pure $ lowerText s

offerPayee :: ByteString -> Maybe Text
offerPayee s = do
  offerData <- B.stripPrefix "lno1" $ B.map toLower s
  guard $ singleCase s && not (B.null offerData) && B.all (`B.elem` bech32Chars) offerData
  pure $ lowerText s

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

$(JQ.deriveJSON (enumJSON $ dropPrefix "UL") ''UnlistedCurrency)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CY") ''ChainCurrency)

$(JQ.deriveJSON defaultJSON ''ChainAddress)
