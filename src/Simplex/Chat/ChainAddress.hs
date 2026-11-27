{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.ChainAddress
  ( ChainAddress (..),
    ChainCurrency (..),
    UnknownCurrency (..),
    bareAddressForm,
    hyperlinkAddress,
  )
where

import Control.Applicative (optional, (<|>))
import Control.Monad (foldM, guard, join, mfilter, void, (<=<))
import qualified Data.Aeson.TH as JQ
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Attoparsec.Combinator (lookAhead)
import Data.Bifunctor (first)
import Data.Bits (bit, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteString as BW
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (chr, digitToInt, isAlphaNum, isAscii, isDigit, isHexDigit, isLower, isUpper, toLower, toUpper)
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Ix (inRange)
import Data.List (elemIndex, find, foldl', unfoldr)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word64, Word8)
import Simplex.Messaging.Agent.QueryString (QSPEscaping (..), QueryStringParams (..), queryParamStr)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Encoding.String (StrEncoding (..))
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, enumJSON, parseAll, sumTypeJSON)
import Simplex.Messaging.Util (eitherToMaybe, safeDecodeUtf8, tshow, (<$$>), (<$?>))
import qualified URI.ByteString as U

data ChainAddress = ChainAddress
  { currency :: ChainCurrency,
    address :: Text,
    walletUri :: Text
  }
  deriving (Eq, Show)

data ChainCurrency
  = CYBitcoin {amount :: Maybe Text}
  | CYLightning {amount :: Maybe Text}
  | CYTicker {ticker :: Text, chainName :: Maybe Text, amount :: Maybe Text}
  | CYUnknown {unknown :: UnknownCurrency}
  deriving (Eq, Show)

data UnknownCurrency = UCChain | UCToken | UCChainAndToken
  deriving (Eq, Show)

instance StrEncoding ChainAddress where
  strEncode ChainAddress {walletUri} = encodeUtf8 walletUri
  strP = fst <$> chainUriP <|> bareAddressP

hyperlinkAddress :: Text -> Text -> Either String (Maybe Text, ChainAddress)
hyperlinkAddress t l = shownText =<< parseAll chainUriP (encodeUtf8 l)
  where
    shownText (a, addresses)
      | t == l = Right (Nothing, a)
      | hasOtherAddress addresses t = Left "other address in hyperlink text"
      | otherwise = Right (mfilter (not . hasAddress addresses) (Just t), a)

bareAddressForm :: ByteString -> Bool
bareAddressForm = isRight . parseAll bareSchemeP

bareAddressP :: Parser ChainAddress
bareAddressP = walletAddress <$?> A.match bareSchemeP
  where
    walletAddress (word, scheme) = fst <$> parseAll chainUriP (scheme <> ":" <> word)

bareSchemeP :: Parser ByteString
bareSchemeP =
  lookAhead (mfilter ((>= 14) . B.length) $ A.takeWhile1 isAsciiAlphaNum)
    *> A.choice
      [ "bitcoin" <$ segwitP "bc",
        "bitcoin" <$ bitcoinBase58P,
        "lightning" <$ invoiceP,
        "lightning" <$ lnurlP,
        "monero" <$ moneroP,
        "zcash" <$ transparentP,
        "zcash" <$ saplingP "zs",
        "zcash" <$ unifiedP,
        "litecoin" <$ segwitP "ltc",
        "litecoin" <$ base58CheckP ["L", "M"] ["\48", "\50"],
        "dogecoin" <$ dogecoinP,
        "bitcoincash" <$ cashAddrP,
        "ethereum" <$ mfilter (not . singleCase . B.drop 2) evmWordP,
        "flux" <$ saplingP "za"
      ]

chainUriP :: Parser (ChainAddress, NonEmpty Text)
chainUriP =
  lookAhead (A.takeWhile1 (A.inClass "a-zA-Z") *> A.char ':')
    *> A.choice
      [ uriP "bitcoin" (bip21Keys <> ["lightning", "lno"]) $ bitcoinLink <$> optional (addressP [segwitP "bc", bitcoinBase58P]),
        uriP "lightning" [] $ lightningLink <$> addressP [invoiceP, (,Nothing) <$$> lnurlP, (,Nothing) <$$> offerP],
        uriP "ethereum" ["value", "address", "uint256"] ethereumP,
        uriP "monero" ["tx_amount", "recipient_name", "tx_description"] $ moneroLink <$> addressP [moneroP],
        uriP "zcash" zip321Keys $ zip321Link "ZEC" zcashAddressP <$> optional zcashAddressP,
        uriP "litecoin" bip21Keys $ coinLink "LTC" <$> addressP [segwitP "ltc", base58CheckP ["L", "M", "3"] ["\48", "\50", "\5"]],
        uriP "dogecoin" bip21Keys $ coinLink "DOGE" <$> addressP [dogecoinP],
        uriP "bitcoincash" bip21Keys $ coinLink "BCH" <$> addressP [cashAddrP],
        uriP "solana" ["amount", "spl-token", "label", "message", "memo"] $ solanaLink <$> addressP [solanaKeyP],
        uriP "flux" fluxKeys $ fluxLink <$> optional fluxAddressP,
        uriP "zel" fluxKeys $ zelLink <$> optional fluxAddressP
      ]
  where
    bip21Keys = ["amount", "label", "message"]
    zip321Keys = bip21Keys <> ["address", "memo"]
    fluxKeys = zip321Keys <> ["action", "coin", "callback"]

uriP :: ByteString -> [ByteString] -> Parser (QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)) -> Parser (ChainAddress, NonEmpty Text)
uriP scheme keys pathP = do
  (path, link) <- A.stringCI scheme *> A.char ':' *> optional (A.string "//") *> A.match pathP
  query <- queryP keys
  first ($ normalizedUri path query) <$> link query
  where
    normalizedUri path query = safeDecodeUtf8 $ scheme <> ":" <> path <> queryText query
    queryText query@(QSP _ q)
      | null q = ""
      | otherwise = "?" <> strEncode query

queryP :: [ByteString] -> Parser QueryStringParams
queryP keys = QSP QNoEscaping <$> A.option [] (A.char '?' *> paramP `A.sepBy` A.char '&')
  where
    paramP = (,) <$> keyP <*> A.option "" (A.char '=' *> A.takeWhile valueChar)
    keyP = knownKey <$> mfilter (not . ("req-" `B.isPrefixOf`) . B.map toLower) (A.takeWhile1 (\c -> isAscii c && (isAlphaNum c || c `B.elem` "-._~")))
    knownKey k = fromMaybe k $ find (== B.map toLower k) keys
    valueChar c = isAscii c && (isAlphaNum c || c `B.elem` "-._~:/?@!$'()*+,;=%")

param :: Parser a -> ByteString -> QueryStringParams -> Parser (Maybe a)
param p key query = params p key query >>= oneValue
  where
    oneValue [] = pure Nothing
    oneValue [a] = pure (Just a)
    oneValue _ = fail "repeated parameter"

params :: Parser a -> ByteString -> QueryStringParams -> Parser [a]
params p key = traverse (either fail pure . parseAll p) . paramValues key

paramValues :: ByteString -> QueryStringParams -> [ByteString]
paramValues key (QSP _ q) = [v | (k, v) <- q, k == key]

textParam :: ByteString -> QueryStringParams -> Parser (Maybe Text)
textParam key query = join <$> param uriTextP key query

chainLink :: ChainCurrency -> NonEmpty Text -> [Maybe Text] -> Parser (Text -> ChainAddress, NonEmpty Text)
chainLink currency addresses@(address :| _) texts
  | any (hasOtherAddress addresses) (catMaybes texts) = fail "other address in label or message"
  | otherwise = pure (ChainAddress currency address, addresses)

bitcoinLink :: Maybe Text -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
bitcoinLink address query = do
  instructions <- (<>) <$> params (addressP [invoiceP, (,Nothing) <$$> lnurlP]) "lightning" query <*> params ((,Nothing) <$> addressP [offerP]) "lno" query
  case (address, instructions) of
    (Just a, _) -> bip21Link CYBitcoin (a :| map fst instructions) [] query
    (Nothing, (a, invoiceAmount) : more) -> bip21Link (CYLightning . (<|> invoiceAmount)) (a :| map fst more) [] query
    (Nothing, []) -> fail "no address"

coinLink :: Text -> Text -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
coinLink ticker address = bip21Link (CYTicker ticker Nothing) (address :| []) []

bip21Link :: (Maybe Text -> ChainCurrency) -> NonEmpty Text -> [Maybe Text] -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
bip21Link currency addresses texts query = do
  amount <- param (amountP 8) "amount" query
  label <- textParam "label" query
  message <- textParam "message" query
  chainLink (currency amount) addresses (label : message : texts)

lightningLink :: (Text, Maybe Text) -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
lightningLink (address, amount) _ = chainLink (CYLightning amount) (address :| []) []

ethereumP :: Parser (QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text))
ethereumP = do
  target <- optional (A.string "pay-") *> addressP [evmP]
  chain <- (>>= (`lookup` evmChains)) <$> optional (A.char '@' *> chainIdP)
  transferLink target chain <$ A.string "/transfer" <|> pure (valueLink target chain)
  where
    chainIdP = mfilter (inRange (1, toInteger (maxBound :: Int64))) A.decimal

valueLink :: Text -> Maybe ChainInfo -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
valueLink address chain query = do
  value <- param evmIntegerP "value" query
  let currency = maybe (CYUnknown UCChain) (\ChainInfo {chainName, coin} -> CYTicker coin chainName $ decimalText 18 <$> value) chain
  chainLink currency (address :| []) []

transferLink :: Text -> Maybe ChainInfo -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
transferLink token chain query = do
  guard $ isNothing $ queryParamStr "value" query
  address <- maybe (fail "no address") pure =<< param (addressP [evmP]) "address" query
  units <- param evmIntegerP "uint256" query
  let currency = case chain of
        Just ChainInfo {chainName, tokens} -> maybe (CYUnknown UCToken) (\ChainToken {ticker, decimals} -> CYTicker ticker chainName $ decimalText decimals <$> units) $ lookup token tokens
        Nothing -> CYUnknown UCChainAndToken
  chainLink currency (address :| [token]) []

moneroLink :: Text -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
moneroLink address query@(QSP _ q) = do
  guard $ all (B.notElem ';' . snd) q
  amount <- param (amountP 12) "tx_amount" query
  label <- textParam "recipient_name" query
  message <- textParam "tx_description" query
  chainLink (CYTicker "XMR" Nothing amount) (address :| []) [label, message]

zip321Link :: Text -> Parser (Text, Bool) -> Maybe (Text, Bool) -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
zip321Link ticker coinAddressP path query@(QSP _ q) = do
  guard $ not $ any (B.elem '.' . fst) q
  (address, shielded) <- oneAddress =<< param coinAddressP "address" query
  memo <- param memoP "memo" query
  guard $ shielded || isNothing memo
  bip21Link (CYTicker ticker Nothing) (address :| []) [join memo] query
  where
    oneAddress addressParam = case (path, addressParam) of
      (Just a, Nothing) -> pure a
      (Nothing, Just a) -> pure a
      _ -> fail "one address required"

zcashAddressP :: Parser (Text, Bool)
zcashAddressP = (,False) <$> addressP [transparentP] <|> (,True) <$> addressP [saplingP "zs", unifiedP]

fluxAddressP :: Parser (Text, Bool)
fluxAddressP = (,False) <$> addressP [transparentP] <|> (,True) <$> addressP [saplingP "za"]

fluxLink :: Maybe (Text, Bool) -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
fluxLink path query = do
  _ <- param (A.stringCI "pay") "action" query
  guard $ isNothing $ queryParamStr "callback" query
  _ <- param (A.choice $ map A.stringCI ["flux", "zelcash", "zel"]) "coin" query
  zip321Link "FLUX" fluxAddressP path query

zelLink :: Maybe (Text, Bool) -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
zelLink path query = do
  guard $ isJust path || isJust (queryParamStr "coin" query)
  fluxLink path query

solanaLink :: Text -> QueryStringParams -> Parser (Text -> ChainAddress, NonEmpty Text)
solanaLink address query = do
  mint <- param (addressP [solanaKeyP]) "spl-token" query
  currency <- case mint of
    Nothing -> CYTicker "SOL" Nothing <$> amountWith 9
    Just m -> maybe (CYUnknown UCToken <$ amountWith maxBound) (\ChainToken {ticker, decimals} -> CYTicker ticker (Just "Solana") <$> amountWith decimals) $ lookup m solanaTokens
  label <- textParam "label" query
  message <- textParam "message" query
  memo <- textParam "memo" query
  chainLink currency (address :| maybeToList mint) [label, message, memo]
  where
    amountWith places = param (amountP places) "amount" query

addressP :: [Parser (Maybe a)] -> Parser a
addressP forms = maybe (fail "invalid address") pure =<< A.choice forms

segwitP :: ByteString -> Parser (Maybe Text)
segwitP hrp = segwitAddress hrp <$> oneCaseP [hrp <> "1"] (inRange (14, 90))

bitcoinBase58P :: Parser (Maybe Text)
bitcoinBase58P = base58CheckP ["1", "3"] ["\0", "\5"]

transparentP :: Parser (Maybe Text)
transparentP = base58CheckP ["t1", "t3"] ["\x1c\xb8", "\x1c\xbd"]

dogecoinP :: Parser (Maybe Text)
dogecoinP = base58CheckP ["D", "9", "A"] ["\30", "\22"]

base58CheckP :: [ByteString] -> [ByteString] -> Parser (Maybe Text)
base58CheckP prefixes versions = base58CheckAddress versions <$> wordP prefixes (inRange (25, 36))

invoiceP :: Parser (Maybe (Text, Maybe Text))
invoiceP = invoiceAddress <$> oneCaseP ["lnbc"] (>= 37)

lnurlP :: Parser (Maybe Text)
lnurlP = lnurlAddress <$> oneCaseP ["lnurl1"] (>= 14)

offerP :: Parser (Maybe Text)
offerP = offerAddress <$> oneCaseP ["lno1"] (>= 14)

moneroP :: Parser (Maybe Text)
moneroP = moneroAddress ["\18", "\42"] <$> wordP ["4", "8"] (== 95) <|> moneroAddress ["\19"] <$> wordP ["4"] (== 106)

saplingP :: ByteString -> Parser (Maybe Text)
saplingP hrp = bech32Address hrp bech32Const (== 43) <$> oneCaseP [hrp <> "1"] (== 78)

unifiedP :: Parser (Maybe Text)
unifiedP = bech32Address "u" bech32mConst (> 0) <$> oneCaseP ["u1"] (>= 14)

cashAddrP :: Parser (Maybe Text)
cashAddrP = cashAddress <$> oneCaseP ["q", "p"] (== 42)

evmP :: Parser (Maybe Text)
evmP = evmAddress <$> evmWordP

solanaKeyP :: Parser (Maybe Text)
solanaKeyP = solanaAddress <$> mfilter (\w -> inRange (32, 44) (B.length w) && B.all (`B.elem` base58Chars) w) (A.takeWhile1 isAsciiAlphaNum)

wordP :: [ByteString] -> (Int -> Bool) -> Parser ByteString
wordP prefixes validLength = mfilter (validLength . B.length) $ fst <$> A.match (A.choice (map A.string prefixes) *> A.takeWhile isAsciiAlphaNum)

oneCaseP :: [ByteString] -> (Int -> Bool) -> Parser ByteString
oneCaseP prefixes = mfilter singleCase . wordP (prefixes <> map (B.map toUpper) prefixes)

evmWordP :: Parser ByteString
evmWordP = mfilter (B.all isHexDigit . B.drop 2) $ wordP ["0x"] (== 42)

addressLike :: ByteString -> Bool
addressLike = isRight . parseAll (void bareSchemeP <|> void solanaKeyP <|> void evmWordP <|> void offerP)

hasOtherAddress :: NonEmpty Text -> Text -> Bool
hasOtherAddress addresses = any otherAddress . T.split (not . isAsciiAlphaNum)
  where
    otherAddress w = addressLike (encodeUtf8 w) && T.toLower w `notElem` fmap T.toLower addresses

hasAddress :: NonEmpty Text -> Text -> Bool
hasAddress addresses t = any ((`T.isInfixOf` T.toLower t) . T.toLower) addresses

data ChainInfo = ChainInfo {chainName :: Maybe Text, coin :: Text, tokens :: [(Text, ChainToken)]}

data ChainToken = ChainToken {ticker :: Text, decimals :: Int}

evmChains :: [(Integer, ChainInfo)]
evmChains =
  [ (1, ChainInfo Nothing "ETH" [usdt "0xdAC17F958D2ee523a2206206994597C13D831ec7" 6, usdc "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48" 6]),
    (10, ChainInfo (Just "OP Mainnet") "ETH" [usdt "0x94b008aA00579c1307B0EF2c499aD98a8ce58e58" 6, usdc "0x0b2C639c533813f4Aa9D7837CAf62653d097Ff85" 6]),
    (56, ChainInfo (Just "BNB Smart Chain") "BNB" [usdt "0x55d398326f99059fF775485246999027B3197955" 18, usdc "0x8AC76a51cc950d9822D68b83fE1Ad97B32Cd580d" 18]),
    (137, ChainInfo (Just "Polygon") "POL" [usdt "0xc2132D05D31c914a87C6611C10748AEb04B58e8F" 6, usdc "0x3c499c542cEF5E3811e1192ce70d8cC03d5c3359" 6]),
    (8453, ChainInfo (Just "Base") "ETH" [usdt "0xfde4C96c8593536E31F229EA8f37b2ADa2699bb2" 6, usdc "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913" 6]),
    (42161, ChainInfo (Just "Arbitrum One") "ETH" [usdt "0xFd086bC7CD5C481DCC9C85ebE478A1C0b69FCbb9" 6, usdc "0xaf88d065e77c8cC2239327C5EDb3A432268e5831" 6])
  ]
  where
    usdt contract = (contract,) . ChainToken "USDT"
    usdc contract = (contract,) . ChainToken "USDC"

solanaTokens :: [(Text, ChainToken)]
solanaTokens =
  [ ("Es9vMFrzaCERmJfrF4H2FYD4KCoNkY11McCe8BenwNYB", ChainToken "USDT" 6),
    ("EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v", ChainToken "USDC" 6)
  ]

segwitAddress :: ByteString -> ByteString -> Maybe Text
segwitAddress hrp s = do
  (hrp', residue, version : program5) <- bech32 s
  program <- fromBase32 program5
  let size = B.length program
  lowerText s
    <$ guard
      ( hrp' == hrp
          && version <= 16
          && inRange (2, 40) size
          && residue == (if version == 0 then bech32Const else bech32mConst)
          && (version /= 0 || size == 20 || size == 32)
      )

bech32Address :: ByteString -> Word64 -> (Int -> Bool) -> ByteString -> Maybe Text
bech32Address hrp constant validLength s = do
  (hrp', residue, values) <- bech32 s
  bytes <- fromBase32 values
  lowerText s <$ guard (hrp' == hrp && residue == constant && validLength (B.length bytes))

invoiceAddress :: ByteString -> Maybe (Text, Maybe Text)
invoiceAddress s = do
  (hrp, residue, values) <- bech32 s
  invoiceAmount <- eitherToMaybe $ parseAll invoiceAmountP hrp
  (lowerText s, invoiceAmount) <$ guard (residue == bech32Const && length values >= 7 + 104)

invoiceAmountP :: Parser (Maybe Text)
invoiceAmountP = A.string "lnbc" *> optional (A.takeWhile1 isDigit >>= btcAmountP)
  where
    btcAmountP digits = (`decimalText` digitsInteger digits) <$> A.option 0 (3 <$ A.char 'm' <|> 6 <$ A.char 'u' <|> 9 <$ A.char 'n' <|> 12 <$ (guard ("0" `B.isSuffixOf` digits) *> A.char 'p'))

lnurlAddress :: ByteString -> Maybe Text
lnurlAddress s = do
  (hrp, residue, values) <- bech32 s
  guard $ hrp == "lnurl" && residue == bech32Const
  U.URI {uriScheme = U.Scheme scheme, uriAuthority = Just U.Authority {authorityHost = U.Host host}} <- eitherToMaybe . U.parseURI U.laxURIParserOptions =<< fromBase32 values
  lowerText s <$ guard (B.map toLower scheme == "https" || B.map toLower scheme == "http" && ".onion" `B.isSuffixOf` B.map toLower host)

offerAddress :: ByteString -> Maybe Text
offerAddress s = lowerText s <$ guard (B.all (`B.elem` bech32Chars) $ B.drop 4 $ B.map toLower s)

cashAddress :: ByteString -> Maybe Text
cashAddress s = do
  values <- traverse bech32Value $ B.unpack $ B.map toLower s
  payload <- fromBase32 $ take 34 values
  lowerText s
    <$ guard
      ( polymod 35 cashAddrGenerators (map (.&. 31) (BW.unpack "bitcoincash") <> [0] <> values) == 1
          && B.length payload == 21
          && B.take 1 payload `elem` ["\0", "\8"]
      )

bech32 :: ByteString -> Maybe (ByteString, Word64, [Word8])
bech32 s = do
  hrp <- B.stripSuffix "1" hrpAndSeparator
  values <- traverse bech32Value $ B.unpack dataPart
  guard $ length values >= 6
  pure (hrp, polymod 25 bech32Generators (expandedHrp hrp <> values), take (length values - 6) values)
  where
    (hrpAndSeparator, dataPart) = B.breakEnd (== '1') $ B.map toLower s
    expandedHrp hrp = map (`shiftR` 5) (BW.unpack hrp) <> [0] <> map (.&. 31) (BW.unpack hrp)

bech32Value :: Char -> Maybe Word8
bech32Value c = fromIntegral <$> B.elemIndex c bech32Chars

bech32Chars :: ByteString
bech32Chars = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

bech32Const :: Word64
bech32Const = 1

bech32mConst :: Word64
bech32mConst = 0x2bc830a3

bech32Generators :: [Word64]
bech32Generators = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]

cashAddrGenerators :: [Word64]
cashAddrGenerators = [0x98f2bc8e61, 0x79b76d99e2, 0xf33e5fb3c4, 0xae2eabe2a8, 0x1e4f43e470]

polymod :: Int -> [Word64] -> [Word8] -> Word64
polymod top generators = foldl' step 1
  where
    step c v = foldl' xor ((c .&. (bit top - 1)) `shiftL` 5 `xor` fromIntegral v) [g | (i, g) <- zip [top ..] generators, testBit c i]

fromBase32 :: [Word8] -> Maybe ByteString
fromBase32 values = BW.pack (reverse bytes) <$ guard (pending < 5 && rest == 0)
  where
    (rest, pending, bytes) = foldl' regroup (0 :: Word64, 0 :: Int, []) values
    regroup (acc, n, out) v
      | n >= 3 = (acc' .&. (bit (n - 3) - 1), n - 3, fromIntegral (acc' `shiftR` (n - 3)) : out)
      | otherwise = (acc', n + 5, out)
      where
        acc' = acc `shiftL` 5 .|. fromIntegral v

base58CheckAddress :: [ByteString] -> ByteString -> Maybe Text
base58CheckAddress versions s = do
  bytes <- base58Decode s
  let (payload, check) = B.splitAt (B.length bytes - 4) bytes
  safeDecodeUtf8 s <$ guard (check == B.take 4 (C.sha256Hash $ C.sha256Hash payload) && any (\v -> B.length payload == B.length v + 20 && v `B.isPrefixOf` payload) versions)

moneroAddress :: [ByteString] -> ByteString -> Maybe Text
moneroAddress networks s = do
  bytes <- mconcat <$> traverse moneroBlock (unfoldr (\b -> B.splitAt 11 b <$ guard (not $ B.null b)) s)
  let (payload, check) = B.splitAt (B.length bytes - 4) bytes
  safeDecodeUtf8 s <$ guard (check == B.take 4 (C.keccak256 payload) && B.take 1 payload `elem` networks)
  where
    moneroBlock block = do
      size <- elemIndex (B.length block) [0, 2, 3, 5, 6, 7, 9, 10, 11]
      n <- base58Integer block
      integerBytes size n <$ guard (n < 256 ^ size)

solanaAddress :: ByteString -> Maybe Text
solanaAddress s = safeDecodeUtf8 s <$ guard ((B.length <$> base58Decode s) == Just 32)

evmAddress :: ByteString -> Maybe Text
evmAddress s = safeDecodeUtf8 . strEncode <$> eitherToMaybe (strDecode s :: Either String Address)

base58Decode :: ByteString -> Maybe ByteString
base58Decode s = (\n -> B.replicate (B.length $ B.takeWhile (== '1') s) '\0' <> integerBytes (byteLength n) n) <$> base58Integer s
  where
    byteLength = length . takeWhile (> 0) . iterate (`shiftR` 8)

base58Integer :: ByteString -> Maybe Integer
base58Integer = foldM (\n c -> (n * 58 +) . toInteger <$> B.elemIndex c base58Chars) 0 . B.unpack

base58Chars :: ByteString
base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

integerBytes :: Int -> Integer -> ByteString
integerBytes size n = BW.pack [fromInteger $ n `shiftR` (8 * i) .&. 255 | i <- [size - 1, size - 2 .. 0]]

amountP :: Int -> Parser Text
amountP places = safeDecodeUtf8 . fst <$> A.match (A.takeWhile1 isDigit *> optional (A.char '.' *> mfilter ((<= places) . B.length) (A.takeWhile1 isDigit)))

evmIntegerP :: Parser Integer
evmIntegerP = do
  (digits, places) <- mfilter (inRange (1, 78) . B.length . fst) $ (\whole fraction -> (whole <> fraction, B.length fraction)) <$> A.takeWhile isDigit <*> A.option "" (A.char '.' *> A.takeWhile1 isDigit)
  power <- A.option 0 $ A.satisfy (\c -> c == 'e' || c == 'E') *> (fromInteger . digitsInteger <$> mfilter ((<= 2) . B.length) (A.takeWhile1 isDigit))
  let (n, remainder) = (digitsInteger digits * 10 ^ max 0 (power - places)) `quotRem` (10 ^ max 0 (places - power))
  n <$ guard (remainder == 0 && n < 2 ^ (256 :: Int))

uriTextP :: Parser (Maybe Text)
uriTextP = (fmap nonEmptyText . first show . decodeUtf8' . B.concat) <$?> A.many' (A.takeWhile1 (/= '%') <|> B.singleton <$> (A.char '%' *> hexByteP))
  where
    hexByteP = (\high low -> chr $ digitToInt high * 16 + digitToInt low) <$> A.satisfy isHexDigit <*> A.satisfy isHexDigit

memoP :: Parser (Maybe Text)
memoP = (nonEmptyText <=< eitherToMaybe . decodeUtf8') <$> mfilter ((<= 512) . B.length) (B64U.decodeUnpadded <$?> A.takeByteString)

decimalText :: Int -> Integer -> Text
decimalText places n = case T.dropWhileEnd (== '0') $ T.justifyRight places '0' $ tshow fraction of
  "" -> tshow whole
  digits -> tshow whole <> "." <> digits
  where
    (whole, fraction) = n `quotRem` (10 ^ places)

digitsInteger :: ByteString -> Integer
digitsInteger = B.foldl' (\n c -> n * 10 + toInteger (digitToInt c)) 0

nonEmptyText :: Text -> Maybe Text
nonEmptyText = mfilter (not . T.null) . Just

singleCase :: ByteString -> Bool
singleCase s = not (B.any isUpper s && B.any isLower s)

lowerText :: ByteString -> Text
lowerText = safeDecodeUtf8 . B.map toLower

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

$(JQ.deriveJSON (enumJSON $ dropPrefix "UC") ''UnknownCurrency)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "CY") ''ChainCurrency)

$(JQ.deriveJSON defaultJSON ''ChainAddress)
