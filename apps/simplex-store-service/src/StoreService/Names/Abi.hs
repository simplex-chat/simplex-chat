{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module StoreService.Names.Abi
  ( AbiValue (..),
    AbiError (..),
    abiTuple,
    abiBytes,
    abiArray,
    uint256,
    addressWord,
    addressFromBytes,
    selector,
    hex,
    decodeUint256,
    decodeWord64,
    decodeAddress,
    decodeBool,
    decodeBytes32,
    decodeBytes,
    decodeString,
    decodePrices,
  )
where

import Control.Monad (unless)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Data.Bifunctor (first)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (mapAccumL)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, decodeUtf8')
import Data.Word (Word64)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Encoding.String (strDecode)
import Simplex.Messaging.Eth.Address (Address, unAddress)

data AbiValue = Static ByteString | Dynamic ByteString

data AbiError
  = AEShort
  | AEOffset
  | AENotAddress
  | AENotBool
  | AENotUtf8
  | AERange
  deriving (Eq, Show)

abiWordSize :: Int
abiWordSize = 32

addressPadding :: Int
addressPadding = 12

abiTuple :: [AbiValue] -> ByteString
abiTuple values = B.concat heads <> B.concat [tl | Dynamic tl <- values]
  where
    heads = snd $ mapAccumL encodeHead (abiWordSize * length values) values
    encodeHead offset = \case
      Static w -> (offset, w)
      Dynamic tl -> (offset + B.length tl, uint256 offset)

abiBytes :: ByteString -> ByteString
abiBytes bs = uint256 (B.length bs) <> bs <> B.replicate (negate (B.length bs) `mod` abiWordSize) 0

abiArray :: [ByteString] -> ByteString
abiArray items = uint256 (length items) <> abiTuple (map Dynamic items)

uint256 :: Integral a => a -> ByteString
uint256 = i2ospOf_ abiWordSize . toInteger

addressWord :: Address -> ByteString
addressWord a = B.replicate addressPadding 0 <> unAddress a

selector :: ByteString -> ByteString
selector = B.take 4 . keccak256

hex :: ByteString -> Text
hex bs = "0x" <> decodeLatin1 (convertToBase Base16 bs)

decodeUint256 :: ByteString -> Either AbiError Integer
decodeUint256 = uintAt 0

decodeWord64 :: ByteString -> Either AbiError Word64
decodeWord64 ret = do
  n <- decodeUint256 ret
  if n <= toInteger (maxBound :: Word64) then Right (fromInteger n) else Left AERange

decodeAddress :: ByteString -> Either AbiError Address
decodeAddress ret = do
  (padding, a) <- B.splitAt addressPadding <$> wordAt 0 ret
  unless (B.all (== 0) padding) $ Left AENotAddress
  maybe (Left AENotAddress) Right $ addressFromBytes a

-- | Address has no exported constructor; its parser takes lowercase hex without the EIP-55 check.
addressFromBytes :: ByteString -> Maybe Address
addressFromBytes = either (const Nothing) Just . strDecode . convertToBase Base16

decodeBool :: ByteString -> Either AbiError Bool
decodeBool ret =
  uintAt 0 ret >>= \case
    0 -> Right False
    1 -> Right True
    _ -> Left AENotBool

decodeBytes32 :: ByteString -> Either AbiError ByteString
decodeBytes32 = wordAt 0

decodeBytes :: ByteString -> Either AbiError ByteString
decodeBytes ret = offsetAt 0 ret >>= bytesAt ret

decodeString :: ByteString -> Either AbiError Text
decodeString ret = decodeBytes ret >>= first (const AENotUtf8) . decodeUtf8'

-- | Decodes @(uint256, (uint256, uint256)[])@, the oracle's base price and its prices by label length.
decodePrices :: ByteString -> Either AbiError (Integer, [(Integer, Integer)])
decodePrices ret = do
  base <- uintAt 0 ret
  items <- offsetAt abiWordSize ret
  n <- offsetAt items ret
  let pairAt i = let o = items + abiWordSize * (1 + 2 * i) in (,) <$> uintAt o ret <*> uintAt (o + abiWordSize) ret
  (base,) <$> mapM pairAt [0 .. n - 1]

wordAt :: Int -> ByteString -> Either AbiError ByteString
wordAt o ret
  | o + abiWordSize <= B.length ret = Right $ B.take abiWordSize $ B.drop o ret
  | otherwise = Left AEShort

uintAt :: Int -> ByteString -> Either AbiError Integer
uintAt o ret = os2ip <$> wordAt o ret

-- an offset or a length, never more than the data's size, so it fits an Int
offsetAt :: Int -> ByteString -> Either AbiError Int
offsetAt o ret = do
  n <- uintAt o ret
  if n <= toInteger (B.length ret) then Right (fromInteger n) else Left AEOffset

bytesAt :: ByteString -> Int -> Either AbiError ByteString
bytesAt ret o = do
  n <- offsetAt o ret
  let start = o + abiWordSize
  if start + n <= B.length ret then Right $ B.take n $ B.drop start ret else Left AEShort
