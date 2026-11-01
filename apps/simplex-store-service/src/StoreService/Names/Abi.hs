{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Abi
  ( AbiValue (..),
    abiTuple,
    abiBytes,
    abiArray,
    uint256,
    addressWord,
    selector,
    hex,
  )
where

import Crypto.Number.Serialize (i2ospOf_)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (mapAccumL)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Simplex.Messaging.Crypto (keccak256)
import Simplex.Messaging.Eth.Address (Address, unAddress)

data AbiValue = Static ByteString | Dynamic ByteString

abiTuple :: [AbiValue] -> ByteString
abiTuple values = B.concat heads <> B.concat [tl | Dynamic tl <- values]
  where
    heads = snd $ mapAccumL encodeHead (32 * length values) values
    encodeHead offset = \case
      Static w -> (offset, w)
      Dynamic tl -> (offset + B.length tl, uint256 offset)

abiBytes :: ByteString -> ByteString
abiBytes bs = uint256 (B.length bs) <> bs <> B.replicate (negate (B.length bs) `mod` 32) 0

abiArray :: [ByteString] -> ByteString
abiArray items = uint256 (length items) <> abiTuple (map Dynamic items)

uint256 :: Integral a => a -> ByteString
uint256 = i2ospOf_ 32 . toInteger

addressWord :: Address -> ByteString
addressWord a = B.replicate 12 0 <> unAddress a

selector :: ByteString -> ByteString
selector = B.take 4 . keccak256

hex :: ByteString -> Text
hex bs = "0x" <> decodeLatin1 (convertToBase Base16 bs)
