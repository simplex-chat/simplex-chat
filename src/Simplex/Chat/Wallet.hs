{-# LANGUAGE OverloadedStrings #-}

-- | BIP-39 seeds and the keys derived from them.
--
-- Two layers: one account path per profile, and one key per name under it. A
-- per-profile key would hand over every name that profile owns.
module Simplex.Chat.Wallet
  ( SeedId (..),
    WalletSeed (..),
    AccountIndex,
    NameIndex,
    AccountRef (..),
    WalletAccount,
    newSeed,
    importRecoveryKey,
    recoveryKeyPhrase,
    deriveNameKey,
    renderNameKeyPath,
    accountAddress,
  )
where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Word (Word32)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)

newtype SeedId = SeedId Int64
  deriving (Eq, Ord, Show)

-- | BIP-44 account index, one per chat profile.
type AccountIndex = Word32

-- | BIP-44 address index, one per name.
type NameIndex = Word32

data WalletSeed = WalletSeed
  { wsId :: SeedId,
    wsEntropy :: ByteString
  }
  deriving (Eq)

instance Show WalletSeed where
  show s = "WalletSeed " <> show (wsId s) <> " <redacted>"

data AccountRef = AccountRef
  { arSeedId :: SeedId,
    arIndex :: AccountIndex
  }
  deriving (Eq, Show)

data WalletAccount = WalletAccount
  { waRef :: AccountRef,
    waKey :: S.PrivateKey
  }
  deriving (Eq)

instance Show WalletAccount where
  show a = "WalletAccount " <> show (waRef a) <> " <redacted>"

-- | No 25th-word passphrase: it would be a second secret to back up.
newSeed :: B39.MnemonicStrength -> TVar ChaChaDRG -> STM ByteString
newSeed strength g = B39.mnemonicToEntropy <$> B39.randomMnemonic strength g

importRecoveryKey :: ByteString -> Either String ByteString
importRecoveryKey phrase = B39.mnemonicToEntropy <$> B39.parseMnemonic phrase

recoveryKeyPhrase :: WalletSeed -> Either String ByteString
recoveryKeyPhrase s = B39.mnemonicPhrase <$> B39.entropyToMnemonic (wsEntropy s)

-- | Standard BIP-44, so the phrase reaches the same addresses in other wallets.
nameKeyPath :: AccountIndex -> NameIndex -> [Word32]
nameKeyPath acc nm = [B32.hardened 44, B32.hardened 60, B32.hardened acc, 0, nm]

renderNameKeyPath :: AccountIndex -> NameIndex -> Text
renderNameKeyPath acc nm = decodeLatin1 . B32.renderPath $ nameKeyPath acc nm

deriveNameKey :: WalletSeed -> AccountIndex -> NameIndex -> Either String WalletAccount
deriveNameKey s acc nm = do
  m <- B39.entropyToMnemonic (wsEntropy s)
  master <- B32.masterKey (B39.mnemonicToSeed m "")
  xk <- B32.derivePath master (nameKeyPath acc nm)
  pure WalletAccount {waRef = AccountRef {arSeedId = wsId s, arIndex = acc}, waKey = B32.xkKey xk}

accountAddress :: WalletAccount -> Address
accountAddress = addressFromPrivateKey . waKey
