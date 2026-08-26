{-# LANGUAGE OverloadedStrings #-}

-- | The wallet: BIP-39 seeds, and the keys derived from them.
--
--   * __seed__ — BIP-39 entropy. Generic, /not/ name-specific.
--   * __account__ — a profile's slot in a seed, BIP-44 account index @i@.
--   * __name key__ — @m\/44'\/60'\/i'\/0\/k@: one key per name, at BIP-44
--     address index @k@ under the profile that bought it. This is what the
--     registry records as the name's owner.
--   * __wallet__ — this module: creation and derivation.
--
-- One key per name, not one per profile. A per-profile key would mean exporting
-- it hands over every name that profile owns, and would put every name's signed
-- record edits behind one shared nonce counter on the resolver. Both are avoided
-- by giving each name its own address index. @k = 0@ is the profile's first
-- name.
--
-- Names are a /consumer/ of the wallet, which is why this sits here rather than
-- under "Simplex.Chat.Names".
--
-- The schema allows several seeds; a profile binds to exactly one plus its own
-- account index. Only the single-seed case is reachable today.
--
-- This module is pure. Persistence lives in "Simplex.Chat.Store.Wallets".
--
-- This is the read-only subset needed to register a name: no signing, no
-- stealth keys, no recovery-phrase import or export. Those are additive —
-- the types here match the full wallet, so adding them changes no signature.
module Simplex.Chat.Wallet
  ( SeedId (..),
    WalletSeed (..),
    AccountIndex,
    NameIndex,
    AccountRef (..),
    WalletAccount (..),
    newSeed,
    deriveNameKey,
    deriveAtPath,
    nameKeyPath,
    renderNameKeyPath,
    accountAddress,
  )
where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.Word (Word32)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)

newtype SeedId = SeedId Int64
  deriving (Eq, Ord, Show)

-- | BIP-44 account index within a seed. One per chat profile.
type AccountIndex = Word32

-- | BIP-44 address index within a profile account. One per name.
type NameIndex = Word32

-- | A seed, held as BIP-39 entropy. Stored in the chat database so it rides the
-- existing archive export and Migrate-to-another-device flows.
--
-- 'Show' is redacting: this is the root secret behind every name it owns.
data WalletSeed = WalletSeed
  { wsId :: SeedId,
    wsEntropy :: ByteString
  }
  deriving (Eq)

instance Show WalletSeed where
  show s = "WalletSeed " <> show (wsId s) <> " <redacted>"

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

-- | Fresh seed entropy. The caller stores it; this module never persists.
-- A 25th-word passphrase is deliberately not used — it would be a second secret
-- to back up.
newSeed :: B39.MnemonicStrength -> TVar ChaChaDRG -> STM ByteString
newSeed strength g = B39.mnemonicToEntropy <$> B39.randomMnemonic strength g

-- | @m\/44'\/60'\/i'\/0\/k@ — the standard BIP-44 layout, with the profile at
-- the account level and the name at the address level. Nothing here is a custom
-- path, so profile @i@'s names are the account list an ordinary Ethereum wallet
-- would show for that account.
nameKeyPath :: AccountIndex -> NameIndex -> [Word32]
nameKeyPath acc nm = [B32.hardened 44, B32.hardened 60, B32.hardened acc, 0, nm]

-- | The path a name's key was derived at, for display. Users need it only to
-- import a single name into a third-party wallet.
renderNameKeyPath :: AccountIndex -> NameIndex -> Text
renderNameKeyPath acc nm = decodeLatin1 . B32.renderPath $ nameKeyPath acc nm

-- | Derive the key that owns one name.
deriveNameKey :: WalletSeed -> AccountIndex -> NameIndex -> Either String WalletAccount
deriveNameKey s acc nm = do
  m <- B39.entropyToMnemonic (wsEntropy s)
  master <- B32.masterKey (B39.mnemonicToSeed m "")
  xk <- B32.derivePath master (nameKeyPath acc nm)
  pure WalletAccount {waRef = AccountRef {arSeedId = wsId s, arIndex = acc}, waKey = B32.xkKey xk}

-- | Derive at a path given literally, e.g. @"m\/44'\/60'\/0'\/0\/1"@ or @"m"@
-- for the master key with no derivation.
--
-- Names record the path they were derived at rather than an index, because a
-- name found on an imported seed may sit on a layout that is not ours — a name
-- bought in a dapp is typically at the master key. Re-deriving from the stored
-- path keeps those usable without special-casing them.
deriveAtPath :: WalletSeed -> AccountIndex -> Text -> Either String WalletAccount
deriveAtPath s acc path = do
  m <- B39.entropyToMnemonic (wsEntropy s)
  master <- B32.masterKey (B39.mnemonicToSeed m "")
  ixs <- B32.parsePath (encodeUtf8 path)
  xk <- B32.derivePath master ixs
  pure WalletAccount {waRef = AccountRef {arSeedId = wsId s, arIndex = acc}, waKey = B32.xkKey xk}

-- | The Ethereum address that owns the name this key was derived for.
accountAddress :: WalletAccount -> Address
accountAddress = addressFromPrivateKey . waKey

-- Stealth addresses, when they arrive, hang off the same profile account but
-- are not at a derivation path at all. A profile publishes one meta-address —
-- a spend key and a viewing key, both hardened under purpose 5564' — and a
-- sender derives a fresh destination from it as @spend + H(r·view)·G@. The
-- recipient's key for that destination is @spend + H(view·R)@, recomputed from
-- the sender's ephemeral public key @R@ rather than from an index. So the
-- wallet must be able to hold a key that is "spend key plus a scalar", which is
-- why one meta-address per profile is enough for any number of received names.
