{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Wallet
  ( AccountIndex,
    AccountKey,
    WalletAddress (..),
    WalletError (..),
    newSeedEntropy,
    entropyFromMnemonic,
    newWalletMaster,
    seedMnemonic,
    deriveAccount,
    accountSecret,
  )
where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson.TH as JQ
import Data.Bifunctor (first)
import qualified Data.ByteArray.Encoding as BAE
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import Simplex.Messaging.Crypto.BIP44 (AccountIndex, CoinType (..), bip44Path)
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (addressFromPrivateKey)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, sumTypeJSON)
import Simplex.Messaging.Util (liftError')

type AccountKey = S.Secp256k1PrivateKey

data WalletAddress = WalletAddress
  { accountIndex :: AccountIndex,
    keyPath :: Text,
    address :: Text
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
  | WEDerivation {derivationError :: String}
  deriving (Eq, Show)

masterStrength :: B39.EntropyStrength
masterStrength = B39.ES256

newSeedEntropy :: TVar ChaChaDRG -> STM B39.WalletEntropy
newSeedEntropy = B39.randomEntropy masterStrength

entropyFromMnemonic :: Text -> Either WalletError B39.WalletEntropy
entropyFromMnemonic phrase = case B39.parsePhrase phrase of
  Right ent | B39.entropyWordCount ent == 24 -> Right ent
  _ -> Left WEBadMnemonic

newWalletMaster :: B39.WalletEntropy -> Either WalletError B32.WalletMaster
newWalletMaster ent = first WEDerivation $ B32.mkWalletMaster ent ""

seedMnemonic :: B32.WalletMaster -> Text
seedMnemonic = decodeLatin1 . B39.entropyPhrase . B32.masterEntropy

deriveAccount :: TVar ChaChaDRG -> B32.WalletMaster -> AccountIndex -> IO (Either WalletError (AccountKey, WalletAddress))
deriveAccount g master n = runExceptT $ do
  k <- B32.xkKey <$> liftError' WEDerivation (B32.derivePath g (B32.walletMasterKey master) path)
  a <- liftIO $ addressFromPrivateKey g k
  pure (k, WalletAddress {accountIndex = n, keyPath = decodeLatin1 $ B32.renderPath path, address = decodeLatin1 $ strEncode a})
  where
    path = bip44Path Ethereum n

accountSecret :: AccountKey -> Text
accountSecret k = "0x" <> decodeLatin1 (BAE.convertToBase BAE.Base16 $ S.unPrivateKey k)

$(JQ.deriveJSON defaultJSON ''WalletAddress)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "WE") ''WalletError)
