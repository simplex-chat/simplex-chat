{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stealth addressing bound to wallet accounts.
--
-- "Simplex.Messaging.Eth.Stealth" holds the ERC-5564 scheme itself. This module
-- is the part that knows about profiles: it generates the sender's ephemeral
-- key, scans a batch of announcements on behalf of an account, and turns a
-- match into a key that can sign.
--
-- Discovery in normal use is a chat message from the sender, not a scan: the
-- sender can only derive a destination if they hold the recipient's
-- meta-address, which reaches them through the profile over an established
-- connection. Scanning exists so that a recovery phrase alone is sufficient —
-- restore on a clean device and there is no message to read, so the ephemeral
-- key must also be recoverable from the chain.
module Simplex.Chat.Wallet.Stealth
  ( Announcement (..),
    OneTimeAccount (..),
    giftDestination,
    scanAnnouncements,
    oneTimeAccount,
    exportOneTimeKey,
    metaAddressHex,
    parseMetaAddressHex,
    bytesHex,
    parseHexBytes,
  )
where

import Control.Concurrent.STM
import Control.Monad ((<=<))
import Crypto.Random (ChaChaDRG)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Maybe (mapMaybe)
import Simplex.Chat.Wallet (StealthKeys (..))
import qualified Simplex.Messaging.Crypto as C
import qualified Simplex.Messaging.Crypto.Secp256k1 as S
import Simplex.Messaging.Eth.Address (Address, addressFromPrivateKey)
import qualified Simplex.Messaging.Eth.Stealth as St

-- | One entry of what the registrar announced: the sender's ephemeral public
-- key and the view tag that lets it be discarded cheaply.
data Announcement = Announcement
  { anEphemeralPubKey :: ByteString,
    anViewTag :: St.ViewTag
  }
  deriving (Eq, Show)

-- | A destination this account controls. Not an 'Simplex.Chat.Wallet.AccountRef':
-- it has no account index, and is reached only through the seed plus the
-- ephemeral key that produced it.
--
-- 'Show' is redacting.
data OneTimeAccount = OneTimeAccount
  { otaAddress :: Address,
    otaEphemeralPubKey :: ByteString,
    otaKey :: S.PrivateKey
  }
  deriving (Eq)

instance Show OneTimeAccount where
  show a = "OneTimeAccount " <> show (otaAddress a) <> " <redacted>"

-- | Sender side: a fresh destination for a recipient's published meta-address.
--
-- The ephemeral key is generated here and immediately discarded — only its
-- public half is kept, in the returned destination. Reusing one across
-- recipients would let them link the destinations, so there is no way to supply
-- it.
giftDestination :: TVar ChaChaDRG -> St.StealthMetaAddress -> IO (Either String St.StealthDestination)
giftDestination g ma = go (10 :: Int)
  where
    go 0 = pure $ Left "stealth: could not generate an ephemeral key"
    go n = do
      bs <- atomically $ C.randomBytes S.privateKeySize g
      case S.mkPrivateKey bs of
        -- A uniform 32 bytes is out of range with probability about 2^-128;
        -- retrying is simpler to reason about than reducing mod n.
        Left _ -> go (n - 1)
        Right eph -> pure $ St.stealthDestination eph ma

-- | Recipient side: which of these announcements are ours.
--
-- The view tag is checked first, inside 'St.stealthMatch', so a non-match costs
-- one point multiplication and one hash rather than a full derivation.
-- Announcements that fail to parse are skipped rather than failing the batch: a
-- scan runs over whatever the chain holds, including entries written by other
-- software.
scanAnnouncements :: StealthKeys -> [Announcement] -> [(Announcement, Address)]
scanAnnouncements ks = mapMaybe match
  where
    spend = S.publicKey (skSpend ks)
    match an = case St.stealthMatch (skView ks) spend (anEphemeralPubKey an) (anViewTag an) of
      Right (Just addr) -> Just (an, addr)
      _ -> Nothing

-- | The account for a destination: its address and the key that signs for it.
oneTimeAccount :: StealthKeys -> ByteString -> Either String OneTimeAccount
oneTimeAccount ks ephemeralPubKey = do
  key <- St.stealthPrivateKey (skSpend ks) (skView ks) ephemeralPubKey
  pure
    OneTimeAccount
      { otaAddress = addressFromPrivateKey key,
        otaEphemeralPubKey = ephemeralPubKey,
        otaKey = key
      }

-- | The meta-address as hex, which is how it travels in a profile field and
-- how a user pastes it into @\/names gift@.
metaAddressHex :: St.StealthMetaAddress -> ByteString
metaAddressHex = toHex . St.metaAddressBytes

parseMetaAddressHex :: ByteString -> Either String St.StealthMetaAddress
parseMetaAddressHex = St.parseMetaAddress <=< fromHex

-- | Hex for values that travel as text: the ephemeral key in a transfer
-- message, and the exported one-time key.
bytesHex :: ByteString -> ByteString
bytesHex = toHex

toHex :: ByteString -> ByteString
toHex = B.concatMap $ \w -> B.pack [hexDigit (w `div` 16), hexDigit (w `mod` 16)]
  where
    hexDigit n
      | n < 10 = 0x30 + n
      | otherwise = 0x57 + n

-- | Decode hex that arrived as text, e.g. an ephemeral key in a message.
parseHexBytes :: ByteString -> Either String ByteString
parseHexBytes = fromHex

fromHex :: ByteString -> Either String ByteString
fromHex bs
  | odd (B.length bs) = Left "expected an even number of hex digits"
  | otherwise = B.pack <$> mapM pair (chunk $ B.unpack bs)
  where
    chunk (a : b : rest) = (a, b) : chunk rest
    chunk _ = []
    pair (a, b) = (\h l -> h * 16 + l) <$> digit a <*> digit b
    digit w
      | w >= 0x30 && w <= 0x39 = Right (w - 0x30)
      | w >= 0x61 && w <= 0x66 = Right (w - 0x57)
      | w >= 0x41 && w <= 0x46 = Right (w - 0x37)
      | otherwise = Left "not a hex digit"

-- | The raw private key for one received name, as hex.
--
-- This is the escape hatch that keeps a received name non-custodial: an
-- ordinary secp256k1 key any wallet will import. It discloses that one address
-- and nothing else — not the seed, not the other names, not the meta-address —
-- which is why it is safe to offer per name.
exportOneTimeKey :: OneTimeAccount -> ByteString
exportOneTimeKey = toHex . S.unPrivateKey . otaKey
