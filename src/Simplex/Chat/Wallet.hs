{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The wallet: BIP-39 seeds, and the per-chat-profile accounts derived from
-- them.
--
-- Three distinct things, three names, used consistently in code, DB and UI:
--
--   * __seed__ — BIP-39 entropy. Generic and profile-scoped, /not/ name-specific:
--     if a general wallet feature is added later it uses this same seed.
--   * __account__ — a profile's derived key at @m\/44'\/60'\/i'\/0\/0@, where @i@
--     is the profile's account index. Its address is what owns names. One
--     account can own many names.
--   * __wallet__ — this module: creation, derivation, storage and signing.
--
-- Names are a /consumer/ of the wallet, which is why this sits at
-- "Simplex.Chat.Wallet" rather than under "Simplex.Chat.Names".
--
-- The schema allows several seeds; a profile binds to exactly one plus its own
-- account index. Only the single-seed case is reachable from the UI. Modelling
-- the extra dimension now means importing a second recovery key later is a UI
-- change rather than a migration of live key material.
--
-- This module is pure. Persistence lives in "Simplex.Chat.Store.Wallets".
module Simplex.Chat.Wallet
  ( SeedId (..),
    WalletSeed (..),
    AccountIndex,
    AccountRef (..),
    WalletAccount (..),
    EthSignature (..),
    newSeed,
    importRecoveryKey,
    recoveryKeyPhrase,
    deriveAccount,
    accountAddress,
    signDigest,
    ethSignatureBytes,
    recoverSigner,
  )
where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.Word (Word32, Word8)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey, addressFromPublicKey, ethereumPath)

newtype SeedId = SeedId Int64
  deriving (Eq, Ord, Show)

-- | BIP-44 account index within a seed. One per chat profile.
type AccountIndex = Word32

-- | A seed, held as BIP-39 entropy. Stored in the chat database so it rides the
-- existing archive export and Migrate-to-another-device flows.
--
-- 'Show' is redacting: this is the root secret behind every name it owns.
data WalletSeed = WalletSeed
  { wsId :: SeedId,
    wsEntropy :: ByteString,
    wsBackedUp :: Bool
  }
  deriving (Eq)

instance Show WalletSeed where
  show s = "WalletSeed " <> show (wsId s) <> " <redacted, backedUp=" <> show (wsBackedUp s) <> ">"

-- | What a chat profile stores: which seed, and which account index within it.
data AccountRef = AccountRef
  { arSeedId :: SeedId,
    arIndex :: AccountIndex
  }
  deriving (Eq, Show)

-- | A derived account: the reference plus the key it resolves to.
data WalletAccount = WalletAccount
  { waRef :: AccountRef,
    waKey :: S.PrivateKey
  }
  deriving (Eq)

instance Show WalletAccount where
  show a = "WalletAccount " <> show (waRef a) <> " <redacted>"

-- | An Ethereum signature: @r || s || v@, 65 bytes, with @v = recId + 27@.
data EthSignature = EthSignature
  { esR :: ByteString,
    esS :: ByteString,
    esV :: Word8
  }
  deriving (Eq, Show)

ethSignatureBytes :: EthSignature -> ByteString
ethSignatureBytes s = esR s <> esS s <> B.singleton (esV s)

-- | Fresh seed entropy. Lazy by design: called only when the user buys their
-- first name. The id is assigned on insert.
newSeed :: B39.MnemonicStrength -> TVar ChaChaDRG -> STM ByteString
newSeed strength g = B39.mnemonicToEntropy <$> B39.randomMnemonic strength g

-- | Import from a recovery phrase, validating the wordlist and BIP-39 checksum.
importRecoveryKey :: ByteString -> Either String ByteString
importRecoveryKey phrase = B39.mnemonicToEntropy <$> B39.parseMnemonic phrase

-- | The phrase to show the user under "recovery key".
recoveryKeyPhrase :: WalletSeed -> Either String ByteString
recoveryKeyPhrase s = B39.mnemonicPhrase <$> B39.entropyToMnemonic (wsEntropy s)

-- | Derive a profile's account.
--
-- BIP-39 seed derivation uses an empty passphrase: a 25th-word passphrase would
-- be a second secret to back up, and losing it would be indistinguishable from
-- losing the phrase.
deriveAccount :: WalletSeed -> AccountIndex -> Either String WalletAccount
deriveAccount s ix = do
  m <- B39.entropyToMnemonic (wsEntropy s)
  master <- B32.masterKey (B39.mnemonicToSeed m "")
  xk <- B32.derivePath master (ethereumPath ix)
  pure WalletAccount {waRef = AccountRef {arSeedId = wsId s, arIndex = ix}, waKey = B32.xkKey xk}

accountAddress :: WalletAccount -> Address
accountAddress = addressFromPrivateKey . waKey

-- | Sign a 32-byte digest (an EIP-712 @hashTypedData@ result).
signDigest :: WalletAccount -> ByteString -> Either String EthSignature
signDigest a digest = do
  sig <- S.signRecoverable (waKey a) digest
  let compact = S.rsCompact sig
  pure
    EthSignature
      { esR = B.take 32 compact,
        esS = B.drop 32 compact,
        esV = fromIntegral (S.rsRecId sig) + 27
      }

-- | Recover the address that produced a signature over a digest. This is what a
-- verifier does — the relayer, and the contracts on-chain.
recoverSigner :: EthSignature -> ByteString -> Either String Address
recoverSigner s digest
  | esV s < 27 || esV s > 30 = Left "signature: v out of range"
  | otherwise = do
      let sig = S.RecoverableSignature {S.rsCompact = esR s <> esS s, S.rsRecId = fromIntegral (esV s) - 27}
      addressFromPublicKey <$> S.recoverPublicKey sig digest
