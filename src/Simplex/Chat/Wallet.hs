{-# LANGUAGE OverloadedStrings #-}

-- | BIP-39 seeds and the keys derived from them.
--
-- One key per name. A name's secret is a leaf, so exporting it hands over that
-- name only.
module Simplex.Chat.Wallet
  ( SeedId,
    WalletSeed (..),
    NameIndex,
    newSeed,
    importRecoveryKey,
    recoveryKeyPhrase,
    seedMaster,
    deriveNameKey,
    renderNameKeyPath,
    nameKeySecret,
  )
where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Word (Word32)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (ethereumPath)

type SeedId = Int64

-- | BIP-44 address index, one per name. Names sit in account 0, from index 1:
-- account 0 index 0 is left for the profile accounts to start beside.
type NameIndex = Word32

data WalletSeed = WalletSeed
  { wsId :: SeedId,
    wsEntropy :: ByteString
  }
  deriving (Eq)

instance Show WalletSeed where
  show s = "WalletSeed " <> show (wsId s) <> " <redacted>"

-- | No 25th-word passphrase: it would be a second secret to back up.
newSeed :: B39.MnemonicStrength -> TVar ChaChaDRG -> STM ByteString
newSeed strength g = B39.mnemonicToEntropy <$> B39.randomMnemonic strength g

importRecoveryKey :: ByteString -> Either String ByteString
importRecoveryKey phrase = B39.mnemonicToEntropy <$> B39.parseMnemonic phrase

recoveryKeyPhrase :: WalletSeed -> Either String ByteString
recoveryKeyPhrase s = B39.mnemonicPhrase <$> B39.entropyToMnemonic (wsEntropy s)

-- | Deriving this runs PBKDF2, so it is done once per command.
seedMaster :: WalletSeed -> Either String B32.ExtendedKey
seedMaster s = do
  m <- B39.entropyToMnemonic (wsEntropy s)
  B32.masterKey (B39.mnemonicToSeed m "")

renderNameKeyPath :: NameIndex -> Text
renderNameKeyPath nm = decodeLatin1 . B32.renderPath $ ethereumPath 0 nm

deriveNameKey :: B32.ExtendedKey -> NameIndex -> Either String S.PrivateKey
deriveNameKey master nm = B32.xkKey <$> B32.derivePath master (ethereumPath 0 nm)

-- | As wallets take it when a key is imported on its own.
nameKeySecret :: S.PrivateKey -> ByteString
nameKeySecret k = "0x" <> BAE.convertToBase BAE.Base16 (S.unPrivateKey k)
