{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Wallet
  ( WalletAddress (..),
    WalletInfo (..),
    newEntropy,
    masterMnemonic,
    deriveAccount,
  )
where

import Control.Concurrent.STM
import Control.Monad.Except
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson.TH as JQ
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Word (Word32)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import Simplex.Messaging.Crypto.BIP44 (AccountIndex, CoinType (..), bip44Path)
import Simplex.Messaging.Eth.Address (Address, deriveAddress)
import Simplex.Messaging.Parsers (defaultJSON)

data WalletAddress = WalletAddress
  { accountIndex :: AccountIndex,
    keyPath :: Text,
    address :: Address
  }
  deriving (Show)

data WalletInfo = WalletInfo
  { accountIndexes :: [AccountIndex],
    nextAccountIndex :: Word32
  }
  deriving (Show)

newEntropy :: TVar ChaChaDRG -> IO B39.WalletEntropy
newEntropy = atomically . B39.randomEntropy B39.ES256

masterMnemonic :: B39.WalletEntropy -> Text
masterMnemonic = decodeLatin1 . B39.entropyPhrase

deriveAccount :: TVar ChaChaDRG -> B39.WalletEntropy -> AccountIndex -> IO (Either String WalletAddress)
deriveAccount g entropy n = runExceptT $ do
  master <- liftEither $ B32.masterKey $ B39.entropySeed entropy ""
  (_, address) <- ExceptT $ deriveAddress g master path
  pure WalletAddress {accountIndex = n, keyPath = decodeLatin1 $ B32.renderPath path, address}
  where
    path = bip44Path Ethereum n

$(JQ.deriveJSON defaultJSON ''WalletAddress)

$(JQ.deriveJSON defaultJSON ''WalletInfo)
