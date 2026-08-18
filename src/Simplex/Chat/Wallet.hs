{-# LANGUAGE OverloadedStrings #-}

-- | The wallet: BIP-39 seeds, and the per-chat-profile accounts derived from
-- them.
--
--   * __seed__ — BIP-39 entropy. Generic and profile-scoped, /not/ name-specific.
--   * __account__ — a profile's slot in a seed, index @i@, holding the main
--     address that owns the names the profile registers.
--   * __wallet__ — this module: creation and derivation.
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
    AccountRef (..),
    WalletAccount (..),
    newSeed,
    deriveAccount,
    accountAddress,
  )
where

import Control.Concurrent.STM
import Crypto.Random (ChaChaDRG)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Word (Word32)
import qualified Simplex.Messaging.Crypto.BIP32 as B32
import qualified Simplex.Messaging.Crypto.BIP39 as B39
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey, ethereumPath)

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

-- | Fresh seed entropy. The caller stores it; this module never persists.
-- A 25th-word passphrase is deliberately not used — it would be a second secret
-- to back up.
newSeed :: B39.MnemonicStrength -> TVar ChaChaDRG -> STM ByteString
newSeed strength g = B39.mnemonicToEntropy <$> B39.randomMnemonic strength g

-- | Derive the account at @m\/44'\/60'\/i'\/0\/0@.
deriveAccount :: WalletSeed -> AccountIndex -> Either String WalletAccount
deriveAccount s ix = do
  m <- B39.entropyToMnemonic (wsEntropy s)
  master <- B32.masterKey (B39.mnemonicToSeed m "")
  xk <- B32.derivePath master (ethereumPath ix)
  pure WalletAccount {waRef = AccountRef {arSeedId = wsId s, arIndex = ix}, waKey = B32.xkKey xk}

-- | The Ethereum address that owns names registered by this account.
accountAddress :: WalletAccount -> Address
accountAddress = addressFromPrivateKey . waKey
