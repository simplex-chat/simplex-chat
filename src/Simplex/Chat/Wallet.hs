{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    NameTree,
    nameTree,
    deriveNameKey,
    renderNameKeyPath,
    nameKeySecret,
  )
where

import Control.Concurrent.STM
import Crypto.Error (CryptoFailable (..))
import qualified Crypto.Hash as H
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.MAC.HMAC as HMAC
import Crypto.Random (ChaChaDRG)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
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

-- | Where name keys hang. Deriving one runs a slow hash, so it is done once per
-- command rather than once per key.
data NameTree = NameTree {ntMaster :: B32.ExtendedKey, ntSecret :: Bool}

bip39Seed :: WalletSeed -> Either String ByteString
bip39Seed s = (`B39.mnemonicToSeed` "") <$> B39.entropyToMnemonic (wsEntropy s)

-- | Without a secret, the seed's own tree. With one, a subtree that a scan of
-- the seed does not reach, as its master comes from the secret.
nameTree :: WalletSeed -> Maybe Text -> Either String NameTree
nameTree s = \case
  Nothing -> (\m -> NameTree m False) <$> (B32.masterKey =<< bip39Seed s)
  Just secret -> do
    seed <- bip39Seed s
    kdf <- case Argon2.hash argonOptions (encodeUtf8 secret) seed 32 of
      CryptoPassed (k :: ByteString) -> Right k
      CryptoFailed e -> Left $ "wallet secret: " <> show e
    let i = BA.convert (HMAC.hmac subtreeLabel kdf :: HMAC.HMAC H.SHA512) :: ByteString
    key <- S.mkPrivateKey (B.take 32 i)
    pure NameTree {ntMaster = B32.ExtendedKey {B32.xkKey = key, B32.xkChainCode = B.drop 32 i}, ntSecret = True}
  where
    subtreeLabel = "simplex wallet subtree" :: ByteString
    -- pinned, not defaultOptions: a library default moving would move every key
    argonOptions =
      Argon2.Options
        { Argon2.iterations = 3,
          Argon2.memory = 65536,
          Argon2.parallelism = 1,
          Argon2.variant = Argon2.Argon2id,
          Argon2.version = Argon2.Version13
        }

renderNameKeyPath :: NameTree -> NameIndex -> Text
renderNameKeyPath t nm
  | ntSecret t = "secret/" <> T.pack (show nm)
  | otherwise = decodeLatin1 . B32.renderPath $ ethereumPath 0 nm

deriveNameKey :: NameTree -> NameIndex -> Either String S.PrivateKey
deriveNameKey t nm
  | ntSecret t = B32.xkKey <$> B32.deriveChild (ntMaster t) nm
  | otherwise = B32.xkKey <$> B32.derivePath (ntMaster t) (ethereumPath 0 nm)

-- | As wallets take it when a key is imported on its own.
nameKeySecret :: S.PrivateKey -> ByteString
nameKeySecret k = "0x" <> BAE.convertToBase BAE.Base16 (S.unPrivateKey k)
