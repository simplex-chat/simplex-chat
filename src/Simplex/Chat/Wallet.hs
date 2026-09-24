{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.Wallet
  ( AccountIndex,
    AccountKey,
    WalletAddress (..),
    WalletError (..),
    newSeedEntropy,
    entropyFromMnemonic,
    seedMnemonic,
    deriveAccount,
    accountSecret,
    checkAccountIndex,
  )
where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson.TH as JQ
import Data.Bifunctor (bimap)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Word (Word32)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Eth.Address (addressFromPrivateKey, ethereumPath)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, sumTypeJSON)
import Simplex.Messaging.Util (liftEitherWith, liftError')

type AccountIndex = Word32

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
  | WEIndexTooLarge
  | WEDerivation {derivationError :: String}
  deriving (Eq, Show)

checkAccountIndex :: AccountIndex -> Either WalletError ()
checkAccountIndex n = () <$ accountPath n

accountPath :: AccountIndex -> Either WalletError [Word32]
accountPath n = maybe (Left WEIndexTooLarge) Right $ ethereumPath n 0

masterStrength :: B39.MnemonicStrength
masterStrength = B39.MS256

newSeedEntropy :: TVar ChaChaDRG -> STM BA.ScrubbedBytes
newSeedEntropy g = B39.mnemonicToEntropy <$> B39.randomMnemonic masterStrength g

entropyFromMnemonic :: Text -> Either WalletError BA.ScrubbedBytes
entropyFromMnemonic phrase = case B39.parseMnemonic phrase of
  Right m | length (B39.mnemonicWords m) == B39.strengthWordCount masterStrength ->
    Right $ B39.mnemonicToEntropy m
  _ -> Left WEBadMnemonic

seedMnemonic :: BA.ScrubbedBytes -> Either WalletError Text
seedMnemonic = bimap WEDerivation (decodeLatin1 . B39.mnemonicPhrase) . B39.entropyToMnemonic

deriveAccount :: BA.ScrubbedBytes -> AccountIndex -> IO (Either WalletError (AccountKey, WalletAddress))
deriveAccount entropy n = runExceptT $ do
  path <- liftEither $ accountPath n
  m <- liftEitherWith WEDerivation $ B39.entropyToMnemonic entropy
  master <- liftError' WEDerivation $ B32.masterKey (B39.mnemonicToSeed m "")
  k <- B32.xkKey <$> liftError' WEDerivation (B32.derivePath master path)
  a <- liftIO $ addressFromPrivateKey k
  pure (k, WalletAddress {accountIndex = n, keyPath = decodeLatin1 $ B32.renderPath path, address = decodeLatin1 $ strEncode a})

accountSecret :: AccountKey -> Text
accountSecret k = "0x" <> decodeLatin1 (BAE.convertToBase BAE.Base16 $ S.unPrivateKey k)

$(JQ.deriveJSON defaultJSON ''WalletAddress)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "WE") ''WalletError)
