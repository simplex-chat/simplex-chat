{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Providers.AddressProof
  ( AddressProof (..),
    ViewProof (..),
    ExtendedPublicKey,
    xpChainCode,
    BitcoinKey,
    MoneroKey,
    extendedPublicKey,
    childKey,
    compressedKey,
    p2wpkhAddress,
    bitcoinKey,
    bitcoinPayee,
    receiveKeyText,
    moneroKey,
    moneroPayee,
    primaryAddress,
    subaddressPrefix,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Crypto.Error (eitherCryptoError, throwCryptoError)
import qualified Crypto.ECC.Edwards25519 as Ed
import Crypto.Hash (RIPEMD160 (..), SHA512, hashWith)
import Crypto.MAC.HMAC (HMAC, hmac)
import Crypto.Number.ModArithmetic (expFast)
import Crypto.Number.Serialize (i2osp, i2ospOf_, os2ip)
import qualified Crypto.PubKey.ECC.Prim as ECC
import qualified Crypto.PubKey.ECC.Types as ECC
import Data.Aeson ((.=))
import qualified Data.Aeson as J
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bits (shiftL, shiftR, testBit, xor, (.&.), (.|.))
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import Data.List (elemIndex, foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word8)
import Simplex.Messaging.Agent.Store.DB (FromField (..), ToField (..), fromTextField_)
import Simplex.Messaging.Crypto (keccak256, sha256Hash)
import Simplex.Messaging.Encoding.String (TextEncoding (..))
import Simplex.Messaging.Util (eitherToMaybe)
import StoreService.Names.Abi (abiWordSize, hex, hexDigits, lowerHexP, parseText)

data AddressProof
  = APBitcoin
      {keyTweak :: ByteString}
  | APMonero
      { spendTweak :: ByteString,
        viewProof :: ViewProof
      }
  deriving (Eq, Show)

data ViewProof = ViewProof
  { challenge :: ByteString,
    response :: ByteString
  }
  deriving (Eq, Show)

instance J.ToJSON AddressProof where
  toJSON = \case
    APBitcoin {keyTweak} -> J.object ["type" .= ("btc" :: Text), "keyTweak" .= hexDigits keyTweak]
    APMonero {spendTweak, viewProof = ViewProof {challenge, response}} ->
      J.object
        [ "type" .= ("xmr" :: Text),
          "spendTweak" .= hexDigits spendTweak,
          "viewProof" .= J.object ["challenge" .= hexDigits challenge, "response" .= hexDigits response]
        ]

instance TextEncoding AddressProof where
  textEncode =
    T.unwords . \case
      APBitcoin {keyTweak} -> ["btc", hexDigits keyTweak]
      APMonero {spendTweak, viewProof = ViewProof {challenge, response}} -> ["xmr", hexDigits spendTweak, hexDigits challenge, hexDigits response]
  textDecode = eitherToMaybe . parseText proofP
    where
      proofP = (A.string "btc " *> (APBitcoin <$> scalarP)) <|> (A.string "xmr " *> (APMonero <$> scalarP <*> (ViewProof <$> (A.char ' ' *> scalarP) <*> (A.char ' ' *> scalarP))))
      scalarP = lowerHexP abiWordSize

instance FromField AddressProof where fromField = fromTextField_ textDecode

instance ToField AddressProof where toField = toField . textEncode

data ExtendedPublicKey = ExtendedPublicKey {xpKey :: ECC.Point, xpChainCode :: ByteString}
  deriving (Eq, Show)

data BitcoinKey = BitcoinKey {bkHrp :: Text, bkReceive :: ExtendedPublicKey}
  deriving (Eq)

data MoneroKey = MoneroKey
  { mkAddress :: Text,
    mkSubaddressPrefix :: Word8,
    mkSpendKey :: Ed.Point,
    mkViewPublic :: ByteString,
    mkViewKey :: Ed.Scalar
  }
  deriving (Eq)

secp256k1 :: ECC.Curve
secp256k1 = ECC.getCurveByName ECC.SEC_p256k1

curveOrder :: Integer
curveOrder = ECC.ecc_n $ ECC.common_curve secp256k1

fieldPrime :: Integer
fieldPrime = 2 ^ (256 :: Int) - 2 ^ (32 :: Int) - 977

serP :: ECC.Point -> ByteString
serP = \case
  ECC.Point x y -> B.cons (if odd y then 3 else 2) (i2ospOf_ 32 x)
  ECC.PointO -> error "serP: an extended key's point is never the point at infinity"

parseP :: ByteString -> Maybe ECC.Point
parseP b = case B.uncons b of
  Just (prefix, xBytes)
    | prefix `elem` [2, 3] && B.length xBytes == 32 && x < fieldPrime && (y * y) `mod` fieldPrime == ySquared ->
        Just $ ECC.Point x $ if odd y == (prefix == 3) then y else fieldPrime - y
    where
      x = os2ip xBytes
      ySquared = (x * x * x + 7) `mod` fieldPrime
      y = expFast ySquared ((fieldPrime + 1) `div` 4) fieldPrime
  _ -> Nothing

extendedPublicKey :: ByteString -> Text -> Either String ExtendedPublicKey
extendedPublicKey version t = do
  raw <- maybe (Left "not base58") Right $ base58Decode t
  let (payload, checksum) = B.splitAt 78 raw
  unless (B.length raw == 82 && B.take 4 (sha256Hash $ sha256Hash payload) == checksum) $ Left "not an extended key with its checksum"
  unless (B.take 4 payload == version) $ Left "another network's or another kind's extended key"
  key <- maybe (Left "its key is not a point") Right $ parseP $ B.drop 45 payload
  pure ExtendedPublicKey {xpKey = key, xpChainCode = B.take 32 $ B.drop 13 payload}

childKey :: ExtendedPublicKey -> Word32 -> Maybe (ByteString, ExtendedPublicKey)
childKey ExtendedPublicKey {xpKey, xpChainCode} i
  | testBit i 31 || t >= curveOrder || ECC.isPointAtInfinity key = Nothing
  | otherwise = Just (il, ExtendedPublicKey {xpKey = key, xpChainCode = ir})
  where
    (il, ir) = B.splitAt 32 $ BA.convert (hmac xpChainCode (serP xpKey <> i2ospOf_ 4 (toInteger i)) :: HMAC SHA512)
    t = os2ip il
    key = ECC.pointAdd secp256k1 (ECC.pointBaseMul secp256k1 t) xpKey

p2wpkhAddress :: Text -> ByteString -> Text
p2wpkhAddress hrp = segwitV0 hrp . BA.convert . hashWith RIPEMD160 . sha256Hash

bitcoinKey :: Text -> Text -> Either String BitcoinKey
bitcoinKey xpub hrp = do
  version <- maybe (Left "btc_hrp must be bc, tb or bcrt") Right $ lookup hrp [("bc", "\x04\x88\xB2\x1E"), ("tb", "\x04\x35\x87\xCF"), ("bcrt", "\x04\x35\x87\xCF")]
  account <- either (Left . ("btc_xpub must be the derivation scheme of a single-signature native SegWit wallet, an xpub for bc or a tpub for tb and bcrt with nothing after it: " <>)) Right $ extendedPublicKey version xpub
  (_, bkReceive) <- maybe (Left "btc_xpub derives no receiving branch") Right $ childKey account 0
  pure BitcoinKey {bkHrp = hrp, bkReceive}

bitcoinPayee :: BitcoinKey -> Text -> Either String (Text, AddressProof)
bitcoinPayee BitcoinKey {bkHrp, bkReceive} keyPath = do
  i <- either (const $ Left $ "the key path " <> T.unpack keyPath <> " is not 0/i") Right $ parseText (A.string "0/" *> A.decimal) keyPath
  unless (i < 2 ^ (31 :: Int)) $ Left $ "the key path " <> T.unpack keyPath <> " is not of a receiving address"
  (t, ExtendedPublicKey {xpKey}) <- maybe (Left $ "the key path " <> T.unpack keyPath <> " derives no key") Right $ childKey bkReceive (fromInteger i)
  pure (p2wpkhAddress bkHrp (serP xpKey), APBitcoin {keyTweak = t})

compressedKey :: ExtendedPublicKey -> ByteString
compressedKey = serP . xpKey

receiveKeyText :: BitcoinKey -> Text
receiveKeyText = hex . compressedKey . bkReceive

segwitV0 :: Text -> ByteString -> Text
segwitV0 hrp program = hrp <> "1" <> T.pack (map (BC.index bech32Charset . fromIntegral) (dat <> checksum))
  where
    dat = 0 : toBase32 (B.unpack program)
    polymod = bech32Polymod (hrpExpand <> dat <> replicate 6 0) `xor` 1
    checksum = [fromIntegral ((polymod `shiftR` (5 * (5 - n))) .&. 31) | n <- [0 .. 5 :: Int]]
    hrpExpand = map ((`shiftR` 5) . fromIntegral . ord) (T.unpack hrp) <> [0] <> map ((.&. 31) . fromIntegral . ord) (T.unpack hrp)

bech32Charset :: ByteString
bech32Charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

bech32Polymod :: [Word8] -> Word32
bech32Polymod = foldl' step 1
  where
    step chk v = foldl' xor (((chk .&. 0x1ffffff) `shiftL` 5) `xor` fromIntegral v) [g | (n, g) <- zip [0 ..] generators, testBit (chk `shiftR` 25) n]
    generators = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3]

toBase32 :: [Word8] -> [Word8]
toBase32 = go 0 0
  where
    go :: Word32 -> Int -> [Word8] -> [Word8]
    go acc bits = \case
      b : rest -> emit (((acc `shiftL` 8) .|. fromIntegral b) .&. 0xfff) (bits + 8) rest
      [] -> [fromIntegral ((acc `shiftL` (5 - bits)) .&. 31) | bits > 0]
    emit acc bits rest
      | bits >= 5 = fromIntegral ((acc `shiftR` (bits - 5)) .&. 31) : emit acc (bits - 5) rest
      | otherwise = go acc bits rest

base58Alphabet :: ByteString
base58Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

base58Digits :: Text -> Maybe [Integer]
base58Digits = mapM (fmap toInteger . (`BC.elemIndex` base58Alphabet)) . T.unpack

base58Decode :: Text -> Maybe ByteString
base58Decode t = do
  digits <- base58Digits t
  let n = foldl' (\acc d -> acc * 58 + d) 0 digits
  pure $ B.replicate (T.length $ T.takeWhile (== '1') t) 0 <> (if n == 0 then B.empty else i2osp n)

moneroKey :: Text -> Text -> Either String MoneroKey
moneroKey address viewKey = do
  raw <- maybe (Left "xmr_address is not a Monero address") Right $ moneroBase58Decode address
  (prefix, keys) <- case B.uncons raw of
    Just (network, rest) | B.length rest == 68 && B.take 4 (keccak256 $ B.take 65 raw) == B.drop 64 rest -> Right (network, B.take 64 rest)
    _ -> Left "xmr_address is not a Monero address with its checksum"
  mkSubaddressPrefix <- maybe (Left "xmr_address must be a wallet's primary address on mainnet, testnet or stagenet") Right $ lookup prefix [(18, 42), (53, 63), (24, 36)]
  let (spend, mkViewPublic) = B.splitAt 32 keys
  mkSpendKey <- either (const $ Left "xmr_address has no spend key") Right $ eitherCryptoError $ Ed.pointDecode spend
  key <- either (const $ Left "xmr_view_key must be 64 lowercase hex digits") Right $ parseText (lowerHexP abiWordSize) viewKey
  let mkViewKey = scalarOf key
  unless (Ed.scalarEncode mkViewKey == key) $ Left "xmr_view_key is not a reduced scalar"
  unless (Ed.pointEncode (Ed.toPoint mkViewKey) == mkViewPublic) $ Left "xmr_view_key is not the view key of xmr_address"
  pure MoneroKey {mkAddress = address, mkSubaddressPrefix, mkSpendKey, mkViewPublic, mkViewKey}

moneroPayee :: MoneroKey -> Word32 -> Word32 -> (Text, AddressProof)
moneroPayee MoneroKey {mkSubaddressPrefix, mkSpendKey, mkViewPublic, mkViewKey} i j =
  (moneroAddress mkSubaddressPrefix dBytes cBytes, APMonero {spendTweak = Ed.scalarEncode m, viewProof = ViewProof {challenge = Ed.scalarEncode c, response = scalarBytes s}})
  where
    viewKey :: ByteString
    viewKey = Ed.scalarEncode mkViewKey
    m = hashScalar $ "SubAddr\0" <> viewKey <> le32 i <> le32 j
    d = Ed.pointAdd mkSpendKey (Ed.toPoint m)
    dBytes, cBytes :: ByteString
    dBytes = Ed.pointEncode d
    cBytes = Ed.pointEncode $ Ed.pointMul mkViewKey d
    k = hashScalar $ "simplex.domains view proof nonce" <> viewKey <> dBytes
    c = hashScalar $ "simplex.domains view proof" <> mkViewPublic <> dBytes <> cBytes <> Ed.pointEncode (Ed.toPoint k) <> Ed.pointEncode (Ed.pointMul k d)
    s = (scalarInteger k - scalarInteger c * scalarInteger mkViewKey) `mod` ed25519Order
    le32 = B.reverse . i2ospOf_ 4 . toInteger

primaryAddress :: MoneroKey -> Text
primaryAddress = mkAddress

subaddressPrefix :: MoneroKey -> Word8
subaddressPrefix = mkSubaddressPrefix

ed25519Order :: Integer
ed25519Order = 2 ^ (252 :: Int) + 27742317777372353535851937790883648493

hashScalar :: ByteString -> Ed.Scalar
hashScalar = scalarOf . keccak256

scalarOf :: ByteString -> Ed.Scalar
scalarOf = throwCryptoError . Ed.scalarDecodeLong

scalarInteger :: Ed.Scalar -> Integer
scalarInteger s = os2ip $ B.reverse (Ed.scalarEncode s :: ByteString)

scalarBytes :: Integer -> ByteString
scalarBytes = B.reverse . i2ospOf_ 32

moneroAddress :: Word8 -> ByteString -> ByteString -> Text
moneroAddress prefix spend view = moneroBase58Encode $ body <> B.take 4 (keccak256 body)
  where
    body = B.cons prefix (spend <> view)

moneroBlockSizes :: [Int]
moneroBlockSizes = [0, 2, 3, 5, 6, 7, 9, 10, 11]

moneroBase58Encode :: ByteString -> Text
moneroBase58Encode b
  | B.null b = ""
  | otherwise = encodedBlock (B.take 8 b) <> moneroBase58Encode (B.drop 8 b)
  where
    encodedBlock block = T.pack [BC.index base58Alphabet (fromInteger d) | d <- reverse $ take (moneroBlockSizes !! B.length block) $ map (`mod` 58) $ iterate (`div` 58) $ os2ip block]

moneroBase58Decode :: Text -> Maybe ByteString
moneroBase58Decode t
  | T.null t = Just B.empty
  | otherwise = do
      let (chunk, rest) = T.splitAt 11 t
      size <- T.length chunk `elemIndex` moneroBlockSizes
      n <- foldl' (\acc d -> acc * 58 + d) 0 <$> base58Digits chunk
      if n < 256 ^ size then (i2ospOf_ size n <>) <$> moneroBase58Decode rest else Nothing
