{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Abi
  ( hex,
  )
where

import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)

hex :: ByteString -> Text
hex bs = "0x" <> decodeLatin1 (convertToBase Base16 bs)
