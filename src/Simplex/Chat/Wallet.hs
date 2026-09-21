{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The device wallet: one BIP-39 seed, and the accounts derived from it.
--
-- An account is a hardened BIP-44 account, @m\/44'\/60'\/n'\/0\/0@, which is how
-- Ledger Live lays out an Ethereum wallet. The account level is hardened, so an
-- exported account key hands over that account and reaches no other.
--
-- Nothing here knows about chat profiles. Which profile an account belongs to is
-- a mapping in "Simplex.Chat.Store.Wallets".
module Simplex.Chat.Wallet
  ( AccountIndex,
    AccountKey,
    WalletAddress (..),
    WalletError (..),
    newSeedEntropy,
    entropyFromMnemonic,
    seedMnemonic,
    seedMaster,
    renderAccountPath,
    deriveAccountKey,
    accountSecret,
    checkAccountIndex,
  )
where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import qualified Data.Aeson.TH as JQ
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1)
import Data.Word (Word32)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (ethereumPath)
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, sumTypeJSON)

-- | BIP-44 account index. One account owns one thing on chain, one name to
-- begin with.
type AccountIndex = Word32

-- | The key at an account index. It owns whatever that account owns.
type AccountKey = S.Secp256k1PrivateKey

-- | One derived address, with the index it came from, so a caller that left the
-- index out knows what it got.
data WalletAddress = WalletAddress
  { accountIndex :: AccountIndex,
    keyPath :: Text,
    address :: Text
  }
  deriving (Show)

data WalletError
  = WENoMaster -- the device has no master entropy
  | WEMasterExists -- create, when it already has one
  | WEBadMnemonic -- wrong word count, wrong word, or bad checksum
  | WEHiddenProfile -- bind, on a profile the app hides
  | WEAccountBound -- bind or export account, on an account another profile holds
  | WECounterUnknown -- no counter to read yet, after an import
  | WEIndexTooLarge -- at or above 2^31
  | WEDerivation {derivationError :: String} -- BIP-32 or BIP-39 said no
  deriving (Eq, Show)

-- | BIP-32 hardens an index by adding 2^31, so an index at or above it is
-- already a hardened component and derives the same key as the index it wraps
-- onto. Refusing it is what keeps one account index to one key.
checkAccountIndex :: AccountIndex -> Either WalletError ()
checkAccountIndex n = if n >= B32.hardenedOffset then Left WEIndexTooLarge else Right ()

-- | 24 words. No 25th-word passphrase: it would be a second secret to back up,
-- and losing it would look exactly like losing the phrase.
masterStrength :: B39.MnemonicStrength
masterStrength = B39.MS256

newSeedEntropy :: TVar ChaChaDRG -> STM BA.ScrubbedBytes
newSeedEntropy g = BA.convert . B39.mnemonicToEntropy <$> B39.randomMnemonic masterStrength g

entropyFromMnemonic :: ByteString -> Either WalletError BA.ScrubbedBytes
entropyFromMnemonic phrase = case B39.parseMnemonic phrase of
  Right m | length (B39.mnemonicWords m) == B39.strengthWordCount masterStrength ->
    Right . BA.convert $ B39.mnemonicToEntropy m
  _ -> Left WEBadMnemonic

seedMnemonic :: BA.ScrubbedBytes -> Either WalletError Text
seedMnemonic entropy =
  bipError . fmap (decodeLatin1 . B39.mnemonicPhrase) . B39.entropyToMnemonic $ entropyBytes entropy

-- | Deriving this runs PBKDF2, so it is done once per command.
seedMaster :: BA.ScrubbedBytes -> Either WalletError B32.ExtendedKey
seedMaster entropy = do
  m <- bipError . B39.entropyToMnemonic $ entropyBytes entropy
  bipError . B32.masterKey $ B39.mnemonicToSeed m ""

accountPath :: AccountIndex -> [Word32]
accountPath n = ethereumPath n 0

renderAccountPath :: AccountIndex -> Text
renderAccountPath = decodeLatin1 . B32.renderPath . accountPath

deriveAccountKey :: B32.ExtendedKey -> AccountIndex -> Either WalletError AccountKey
deriveAccountKey master n = B32.xkKey <$> bipError (B32.derivePath master $ accountPath n)

-- | As wallets take it when a key is imported on its own.
accountSecret :: AccountKey -> Text
accountSecret k = "0x" <> decodeLatin1 (BAE.convertToBase BAE.Base16 $ S.unPrivateKey k)

-- | The copy BIP-39 takes is a plain 'ByteString' and is not wiped.
entropyBytes :: BA.ScrubbedBytes -> ByteString
entropyBytes = BA.convert

-- | The BIP-32 and BIP-39 functions report failure as a string. For entropy
-- this module produced only 'B32.masterKey' and 'B32.derivePath' can fail at
-- all, with a negligible probability, and nothing here retries, so the whole
-- family shares one constructor.
bipError :: Either String a -> Either WalletError a
bipError = either (Left . WEDerivation) Right

$(JQ.deriveJSON defaultJSON ''WalletAddress)

$(JQ.deriveJSON (sumTypeJSON $ dropPrefix "WE") ''WalletError)
