{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Wallet
  ( AccountKey,
    WalletAddress (..),
    WalletInfo (..),
    nextAccount,
    newEntropy,
    masterMnemonic,
    deriveAccount,
    accountSecret,
  )
where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson.TH as JQ
import Data.Bifunctor (first)
import qualified Data.ByteArray.Encoding as BAE
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Word (Word32)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import Simplex.Messaging.Crypto.BIP44 (AccountIndex, CoinType (..), bip44Path, mkAccountIndex)
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (Address, deriveAddress)
import Simplex.Messaging.Parsers (defaultJSON)

type AccountKey = S.Secp256k1PrivateKey

data WalletAddress = WalletAddress
  { accountIndex :: AccountIndex,
    keyPath :: Text,
    address :: Address
  }
  deriving (Show)

data WalletInfo = WalletInfo
  { accountIndexes :: [AccountIndex],
    nextAccountIndex :: Maybe Word32
  }
  deriving (Show)

nextAccount :: Maybe Word32 -> Either String AccountIndex
nextAccount = maybe (Left "the next account is unknown after an import") (first (const "every account index is used") . mkAccountIndex)

newEntropy :: TVar ChaChaDRG -> IO B39.WalletEntropy
newEntropy = atomically . B39.randomEntropy B39.ES256

masterMnemonic :: B39.WalletEntropy -> Text
masterMnemonic = decodeLatin1 . B39.entropyPhrase

deriveAccount :: TVar ChaChaDRG -> B39.WalletEntropy -> AccountIndex -> IO (Either String (AccountKey, WalletAddress))
deriveAccount g entropy n = case B32.masterKey (B39.entropySeed entropy "") of
  Left e -> pure $ Left e
  Right master -> fmap account <$> deriveAddress g master path
  where
    path = bip44Path Ethereum n
    account (xk, a) = (B32.xkKey xk, WalletAddress {accountIndex = n, keyPath = decodeLatin1 $ B32.renderPath path, address = a})

accountSecret :: AccountKey -> Text
accountSecret k = "0x" <> decodeLatin1 (BAE.convertToBase BAE.Base16 $ S.unPrivateKey k)

$(JQ.deriveJSON defaultJSON ''WalletAddress)

$(JQ.deriveJSON defaultJSON ''WalletInfo)
