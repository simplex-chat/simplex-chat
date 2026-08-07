{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The wallet: BIP-39 seeds, and the per-chat-profile accounts derived from
-- them.
--
-- Three distinct things, three names, used consistently in code, DB and UI:
--
--   * __seed__ — BIP-39 entropy. Generic and profile-scoped, /not/ name-specific:
--     if a general wallet feature is added later it uses this same seed.
--   * __account__ — a profile's slot in a seed, index @i@. It holds a key per
--     chain, not one key: a main address that owns the names the profile buys,
--     and a stealth spend\/view pair whose public halves are published as a
--     meta-address. One account can own many names.
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
    Chain (..),
    chainText,
    parseChain,
    StealthKeys (..),
    newSeed,
    importRecoveryKey,
    recoveryKeyPhrase,
    deriveAccount,
    accountAddress,
    deriveStealthKeys,
    accountMetaAddress,
    mainPath,
    stealthSpendPath,
    stealthViewPath,
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
import Data.Text (Text)
import Data.Word (Word32, Word8)
import Simplex.Messaging.Crypto.BIP32 (hardened)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey, addressFromPublicKey, ethereumPath)
import qualified Simplex.Messaging.Eth.Stealth as St

newtype SeedId = SeedId Int64
  deriving (Eq, Ord, Show)

-- | BIP-44 account index within a seed. One per chat profile.
type AccountIndex = Word32

-- | The chains an account can hold keys on.
--
-- Present in full from the first migration although only 'ChainEth' is
-- implemented, so that adding Bitcoin or Monero later changes no type signature
-- and no stored row — see the layout note above 'mainPath'.
data Chain = ChainEth | ChainBtc | ChainXmr
  deriving (Eq, Ord, Show)

-- | Wire and database form. Stable: these strings are stored in
-- @wallet_one_time_addresses.chain@.
chainText :: Chain -> Text
chainText = \case
  ChainEth -> "eth"
  ChainBtc -> "btc"
  ChainXmr -> "xmr"

parseChain :: Text -> Maybe Chain
parseChain = \case
  "eth" -> Just ChainEth
  "btc" -> Just ChainBtc
  "xmr" -> Just ChainXmr
  _ -> Nothing

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

-- | The BIP-32 master key for a seed.
--
-- BIP-39 seed derivation uses an empty passphrase: a 25th-word passphrase would
-- be a second secret to back up, and losing it would be indistinguishable from
-- losing the phrase.
seedMaster :: WalletSeed -> Either String B32.ExtendedKey
seedMaster s = do
  m <- B39.entropyToMnemonic (wsEntropy s)
  B32.masterKey (B39.mnemonicToSeed m "")

-- Derivation layout.
--
-- These paths are the one part of the wallet that can never change: altering
-- them after a user holds a name means moving assets, and there is no safe
-- migration. They live here rather than in the crypto library because the
-- layout is a product decision; the library holds only standard paths.
--
-- @
-- seed (BIP-39)
-- └── account i                              one per chat profile
--     ├── ETH   m\/44'\/60'\/i'\/0\/0           main — names bought by this profile
--     │         m\/5564'\/60'\/i'\/0'\/0        stealth spend
--     │         m\/5564'\/60'\/i'\/1'\/0        stealth view
--     ├── BTC   m\/352'\/0'\/i'\/0'\/0          silent-payment spend   (BIP-352)
--     │         m\/352'\/0'\/i'\/1'\/0          silent-payment scan
--     └── XMR   m\/44'\/128'\/i'\/0'            then SHA3 + sc_reduce32, native subaddresses
-- @
--
-- The @0'@ = spend, @1'@ = view convention is BIP-352's, reused across chains so
-- there is one layout to remember. Purpose 5564 is ours: ERC-5564 has no
-- registered BIP-43 purpose, so this number is defined here once and never
-- changed.

-- | Main account path: the ordinary address, which owns names this profile buys.
mainPath :: Chain -> AccountIndex -> Either String [Word32]
mainPath c ix = case c of
  ChainEth -> Right $ ethereumPath ix
  ChainBtc -> Left "wallet: bitcoin is not implemented"
  ChainXmr -> Left "wallet: monero is not implemented"

-- | Stealth spending key path. The recipient's private half of the meta-address.
stealthSpendPath :: Chain -> AccountIndex -> Either String [Word32]
stealthSpendPath c ix = case c of
  ChainEth -> Right [hardened 5564, hardened 60, hardened ix, hardened 0, 0]
  ChainBtc -> Right [hardened 352, hardened 0, hardened ix, hardened 0, 0]
  ChainXmr -> Left "wallet: monero derives stealth keys natively, not on this path"

-- | Stealth viewing key path. Finds one-time addresses; cannot spend from them.
stealthViewPath :: Chain -> AccountIndex -> Either String [Word32]
stealthViewPath c ix = case c of
  ChainEth -> Right [hardened 5564, hardened 60, hardened ix, hardened 1, 0]
  ChainBtc -> Right [hardened 352, hardened 0, hardened ix, hardened 1, 0]
  ChainXmr -> Left "wallet: monero derives stealth keys natively, not on this path"

-- | Derive a profile's main account.
deriveAccount :: WalletSeed -> AccountIndex -> Either String WalletAccount
deriveAccount s ix = do
  master <- seedMaster s
  path <- mainPath ChainEth ix
  xk <- B32.derivePath master path
  pure WalletAccount {waRef = AccountRef {arSeedId = wsId s, arIndex = ix}, waKey = B32.xkKey xk}

accountAddress :: WalletAccount -> Address
accountAddress = addressFromPrivateKey . waKey

-- | The stealth pair for an account: spending and viewing keys.
--
-- Both derive from the same seed at fixed paths, so publishing a meta-address
-- adds nothing to back up — the recovery phrase already covers it.
--
-- 'Show' is redacting: the spending key controls every name sent to this profile.
data StealthKeys = StealthKeys
  { skSpend :: S.PrivateKey,
    skView :: S.PrivateKey
  }
  deriving (Eq)

instance Show StealthKeys where
  show _ = "StealthKeys <redacted>"

deriveStealthKeys :: WalletSeed -> Chain -> AccountIndex -> Either String StealthKeys
deriveStealthKeys s c ix = do
  master <- seedMaster s
  spendPath <- stealthSpendPath c ix
  viewPath <- stealthViewPath c ix
  spend <- B32.derivePath master spendPath
  view <- B32.derivePath master viewPath
  pure StealthKeys {skSpend = B32.xkKey spend, skView = B32.xkKey view}

-- | The account's published meta-address.
--
-- The encoding is ERC-5564's and lives in "Simplex.Messaging.Eth.Stealth"; what
-- belongs here is only which keys go into it. Not an address and never
-- on-chain, which is what makes it safe to put in a SimpleX profile: holding it
-- lets someone send to this profile and nothing else, since deriving a one-time
-- address needs either the sender's ephemeral secret or the private viewing key.
accountMetaAddress :: StealthKeys -> St.StealthMetaAddress
accountMetaAddress ks = St.metaAddress (skSpend ks) (skView ks)

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
