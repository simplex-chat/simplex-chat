{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Providers.AddressProof
  ( AddressProof (..),
    ViewProof (..),
    ExtendedKey (..),
    BitcoinKey,
    MoneroKey,
    extendedKey,
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

import qualified Crypto.ECC.Edwards25519 as Ed
import qualified Crypto.PubKey.ECC.Types as ECC
import qualified Data.Aeson as J
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Simplex.Messaging.Agent.Store.DB (FromField (..), ToField (..), fromTextField_)
import Simplex.Messaging.Encoding.String (TextEncoding (..))

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
  toJSON _ = J.Null

instance TextEncoding AddressProof where
  textEncode _ = ""
  textDecode _ = Nothing

instance FromField AddressProof where fromField = fromTextField_ textDecode

instance ToField AddressProof where toField = toField . textEncode

data ExtendedKey = ExtendedKey {xkKey :: ECC.Point, xkChainCode :: ByteString}
  deriving (Eq, Show)

data BitcoinKey = BitcoinKey {bkHrp :: Text, bkReceive :: ExtendedKey}
  deriving (Eq)

data MoneroKey = MoneroKey
  { mkAddress :: Text,
    mkSubaddressPrefix :: Word8,
    mkSpendKey :: Ed.Point,
    mkViewPublic :: ByteString,
    mkViewKey :: Ed.Scalar
  }
  deriving (Eq)

extendedKey :: ByteString -> Text -> Either String ExtendedKey
extendedKey _ _ = Left "extended keys are not read yet"

childKey :: ExtendedKey -> Word32 -> Maybe (ByteString, ExtendedKey)
childKey _ _ = Nothing

compressedKey :: ExtendedKey -> ByteString
compressedKey _ = B.empty

p2wpkhAddress :: Text -> ByteString -> Text
p2wpkhAddress _ _ = ""

bitcoinKey :: Text -> Text -> Either String BitcoinKey
bitcoinKey _ _ = Left "Bitcoin keys are not read yet"

bitcoinPayee :: BitcoinKey -> Text -> Either String (Text, AddressProof)
bitcoinPayee _ _ = Left "Bitcoin addresses are not derived yet"

receiveKeyText :: BitcoinKey -> Text
receiveKeyText BitcoinKey {bkHrp} = bkHrp

moneroKey :: Text -> Text -> Either String MoneroKey
moneroKey _ _ = Left "Monero keys are not read yet"

moneroPayee :: MoneroKey -> Word32 -> Word32 -> (Text, AddressProof)
moneroPayee MoneroKey {mkAddress} _ _ = (mkAddress, APBitcoin B.empty)

primaryAddress :: MoneroKey -> Text
primaryAddress = mkAddress

subaddressPrefix :: MoneroKey -> Word8
subaddressPrefix = mkSubaddressPrefix
