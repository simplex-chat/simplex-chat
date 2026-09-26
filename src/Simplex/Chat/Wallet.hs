{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Wallet
  ( AccountKey,
    WalletAddress (..),
    WalletInfo (..),
    WalletError (..),
    newWalletMaster,
    entropyFromMnemonic,
    importWalletMaster,
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
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)
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

newWalletMaster :: TVar ChaChaDRG -> IO B32.WalletMaster
newWalletMaster g = B32.mkWalletMaster <$> atomically (B39.randomEntropy masterStrength g)

entropyFromMnemonic :: Text -> Either WalletError B39.WalletEntropy
entropyFromMnemonic = first (const WEBadMnemonic) . B39.parsePhrase

importWalletMaster :: Text -> Either WalletError B32.WalletMaster
importWalletMaster phrase = B32.mkWalletMaster <$> entropyFromMnemonic phrase

masterMnemonic :: B32.WalletMaster -> Text
masterMnemonic = decodeLatin1 . B39.entropyPhrase . B32.masterEntropy

deriveAccount :: TVar ChaChaDRG -> B32.WalletMaster -> AccountIndex -> IO (AccountKey, WalletAddress)
deriveAccount g master n = do
  k <- B32.xkKey <$> B32.derivePath g (B32.walletMasterKey master) path
  a <- addressFromPrivateKey g k
  pure (k, WalletAddress {accountIndex = n, keyPath = decodeLatin1 $ B32.renderPath path, address = a})
  where
    path = bip44Path Ethereum n

accountSecret :: AccountKey -> Text
accountSecret k = "0x" <> decodeLatin1 (BAE.convertToBase BAE.Base16 $ S.unPrivateKey k)

$(JQ.deriveJSON defaultJSON ''WalletAddress)

$(JQ.deriveJSON defaultJSON ''WalletInfo)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "WE") ''WalletError)
