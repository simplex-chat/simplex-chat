{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.WebUIMock
  ( EphemeralWallet (..),
    newEphemeralWallet,
    redemptionCode,
    walletFromCode,
    signRegisterIntent,
    NameOwnerCode,
    randomNameOwnerCode,
    parseNameOwnerCode,
    formatNameOwnerCode,
    nameOwnerCodeEntropy,
  )
where

import StoreService.Names.Registration (RegisterIntent (..), Registration, registerIntentDigest)
import Control.Concurrent.STM (TVar, atomically)
import Control.Monad (guard)
import Crypto.Number.Serialize (i2ospOf_, os2ip)
import Crypto.Random (ChaChaDRG)
import Data.Bits (shiftR)
import Data.Char (isAlphaNum, toUpper)
import Data.List (elemIndex, foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.BIP32 (masterKey, xkKey)
import Simplex.Messaging.Crypto.BIP39 (EntropyStrength (..), WalletEntropy, entropySeed, mkEntropy, randomEntropy, unEntropy)
import Simplex.Messaging.Util (eitherToMaybe)
import Simplex.Messaging.Crypto.BIP44 (CoinType (..), bip44Path, mkAccountIndex)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.Eth.Address (Address, deriveAddress)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain)

data EphemeralWallet = EphemeralWallet
  { ewCode :: NameOwnerCode,
    ewKey :: Secp256k1PrivateKey,
    ewAddress :: Address
  }

newEphemeralWallet :: TVar ChaChaDRG -> IO EphemeralWallet
newEphemeralWallet g = randomNameOwnerCode g >>= walletFromNameOwnerCode g

redemptionCode :: EphemeralWallet -> Text
redemptionCode = formatNameOwnerCode . ewCode

walletFromCode :: TVar ChaChaDRG -> Text -> IO EphemeralWallet
walletFromCode g = maybe (fail "invalid name code") (walletFromNameOwnerCode g) . parseNameOwnerCode

signRegisterIntent :: TVar ChaChaDRG -> Eip712Domain -> EphemeralWallet -> Registration -> Word64 -> IO RegisterIntent
signRegisterIntent g domain EphemeralWallet {ewKey} r deadline =
  RegisterIntent r nonce deadline <$> (signRecoverable g ewKey (registerIntentDigest domain r nonce deadline) >>= either fail pure)
  where
    nonce = 0

newtype NameOwnerCode = NameOwnerCode WalletEntropy
  deriving (Eq, Show)

ownerCodeLength :: Int
ownerCodeLength = 30

ownerChecksumBits :: Int
ownerChecksumBits = 22

alphabet :: String
alphabet = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

randomNameOwnerCode :: TVar ChaChaDRG -> IO NameOwnerCode
randomNameOwnerCode drg = NameOwnerCode <$> atomically (randomEntropy ES128 drg)

parseNameOwnerCode :: Text -> Maybe NameOwnerCode
parseNameOwnerCode t = do
  body <- T.stripPrefix "SN" $ T.toUpper $ T.filter isAlphaNum t
  guard $ T.length body == ownerCodeLength
  vs <- mapM charValue $ T.unpack body
  let (payload, checksum) = foldl' (\acc v -> acc * 32 + toInteger v) 0 vs `divMod` (2 ^ ownerChecksumBits)
  guard $ checksum == ownerChecksum payload
  NameOwnerCode <$> eitherToMaybe (mkEntropy $ i2ospOf_ 16 payload)
  where
    charValue c = case toUpper c of
      'I' -> Just 1
      'L' -> Just 1
      'O' -> Just 0
      u -> elemIndex u alphabet

formatNameOwnerCode :: NameOwnerCode -> Text
formatNameOwnerCode (NameOwnerCode e) = T.intercalate "-" $ "SN" : T.chunksOf 5 (T.pack $ map (alphabet !!) values)
  where
    payload = os2ip $ unEntropy e
    n = payload * 2 ^ ownerChecksumBits + ownerChecksum payload
    values = reverse $ take ownerCodeLength $ map (fromInteger . (`mod` 32)) $ iterate (`div` 32) n

nameOwnerCodeEntropy :: NameOwnerCode -> WalletEntropy
nameOwnerCodeEntropy (NameOwnerCode e) = e

ownerChecksum :: Integer -> Integer
ownerChecksum = (`shiftR` (256 - ownerChecksumBits)) . os2ip . C.sha256Hash . i2ospOf_ 16

walletFromNameOwnerCode :: TVar ChaChaDRG -> NameOwnerCode -> IO EphemeralWallet
walletFromNameOwnerCode g ewCode = do
  master <- either fail pure $ masterKey $ entropySeed (nameOwnerCodeEntropy ewCode) ""
  account <- either fail pure $ mkAccountIndex 0
  (xk, ewAddress) <- deriveAddress g master (bip44Path Ethereum account) >>= either fail pure
  pure EphemeralWallet {ewCode, ewKey = xkKey xk, ewAddress}
