{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Wallet
  ( AccountKey,
    WalletAddress (..),
    WalletInfo (..),
    WalletError (..),
    newEntropy,
    entropyFromMnemonic,
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
import Simplex.Messaging.Crypto.BIP44 (AccountIndex, CoinType (..), bip44Path)
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (Address, deriveAddress)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, sumTypeJSON)

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

data WalletError
  = WENoMaster
  | WEMasterExists
  | WEBadMnemonic
  | WEHiddenProfile
  | WEAccountBound
  | WEAccountNotHeld
  | WECounterUnknown
  | WEAccountsExhausted
  deriving (Eq, Show)

masterStrength :: B39.EntropyStrength
masterStrength = B39.ES256

newEntropy :: TVar ChaChaDRG -> IO B39.WalletEntropy
newEntropy = atomically . B39.randomEntropy masterStrength

entropyFromMnemonic :: Text -> Either WalletError B39.WalletEntropy
entropyFromMnemonic = first (const WEBadMnemonic) . B39.parsePhrase

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

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "WE") ''WalletError)
