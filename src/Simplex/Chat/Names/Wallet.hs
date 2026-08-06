{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The name wallet: one BIP-39 seed per chat database, one Ethereum key per
-- chat profile.
--
-- Keys derive at @m\/44'\/60'\/i'\/0\/0@ where @i@ is the profile's account
-- index, so names bought under different chat profiles are owned by different
-- addresses and are not linked to each other on-chain — while still recovering
-- from a single phrase.
--
-- This module is pure: seed persistence, the account-index allocation and the
-- chat commands are separate concerns and are not implemented here yet.
--
-- The user never holds ETH and this is not a wallet in the product sense: the
-- key exists only to own names and to sign EIP-712 intents that SimpleX relays
-- and pays for. Nothing here constructs or broadcasts a transaction.
module Simplex.Chat.Names.Wallet
  ( NameWallet,
    ProfileKey (..),
    newNameWallet,
    nameWalletEntropy,
    importRecoveryKey,
    recoveryKeyPhrase,
    profileKey,
    profileAddress,
    signTypedData,
    EthSignature (..),
    ethSignatureBytes,
  )
where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word32, Word8)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey, ethereumPath)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain, Value, hashTypedData)

-- | The root secret, held as BIP-39 entropy. Stored in the chat database so it
-- rides the existing archive export and Migrate-to-another-device flows.
--
-- 'Show' is redacting: this is the single secret behind every name the user
-- owns.
newtype NameWallet = NameWallet {nameWalletEntropy :: ByteString}
  deriving (Eq)

instance Show NameWallet where
  show _ = "NameWallet <redacted>"

-- | A profile's derived key and its BIP-44 account index.
data ProfileKey = ProfileKey
  { pkAccount :: Word32,
    pkKey :: S.PrivateKey
  }
  deriving (Eq)

instance Show ProfileKey where
  show pk = "ProfileKey " <> show (pkAccount pk) <> " <redacted>"

-- | An Ethereum signature: @r || s || v@, 65 bytes, with @v = recId + 27@.
data EthSignature = EthSignature
  { esR :: ByteString,
    esS :: ByteString,
    esV :: Word8
  }
  deriving (Eq, Show)

ethSignatureBytes :: EthSignature -> ByteString
ethSignatureBytes s = esR s <> esS s <> B.singleton (esV s)

-- | Create a wallet with fresh entropy. Lazy by design: called only when the
-- user buys their first name.
newNameWallet :: B39.MnemonicStrength -> TVar ChaChaDRG -> STM NameWallet
newNameWallet strength g = NameWallet . B39.mnemonicToEntropy <$> B39.randomMnemonic strength g

-- | Import from a recovery phrase, validating the wordlist and BIP-39 checksum.
importRecoveryKey :: ByteString -> Either String NameWallet
importRecoveryKey phrase = NameWallet . B39.mnemonicToEntropy <$> B39.parseMnemonic phrase

-- | The phrase to show the user under "name recovery key".
recoveryKeyPhrase :: NameWallet -> Either String ByteString
recoveryKeyPhrase (NameWallet ent) = B39.mnemonicPhrase <$> B39.entropyToMnemonic ent

-- | Derive the key for a chat profile's account index.
--
-- BIP-39 seed derivation uses an empty passphrase: the 25th-word passphrase
-- would be a second secret to back up, and losing it would be indistinguishable
-- from losing the phrase.
profileKey :: NameWallet -> Word32 -> Either String ProfileKey
profileKey (NameWallet ent) account = do
  m <- B39.entropyToMnemonic ent
  master <- B32.masterKey (B39.mnemonicToSeed m "")
  xk <- B32.derivePath master (ethereumPath account)
  pure ProfileKey {pkAccount = account, pkKey = B32.xkKey xk}

profileAddress :: ProfileKey -> Address
profileAddress = addressFromPrivateKey . pkKey

-- | Sign an EIP-712 intent. The type string must match the contract's exactly,
-- in EIP-712 canonical form.
signTypedData :: ProfileKey -> Eip712Domain -> ByteString -> [Value] -> Either String EthSignature
signTypedData pk domain typeString members = do
  digest <- hashTypedData domain typeString members
  sig <- S.signRecoverable (pkKey pk) digest
  let compact = S.rsCompact sig
  pure
    EthSignature
      { esR = B.take 32 compact,
        esS = B.drop 32 compact,
        esV = fromIntegral (S.rsRecId sig) + 27
      }
