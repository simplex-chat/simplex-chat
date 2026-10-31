{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.StoreService.WebUIMock
  ( EphemeralWallet (..),
    newEphemeralWallet,
    redemptionCode,
    walletFromCode,
    signRegisterIntent,
  )
where

import StoreService.Names.Registration (RegisterIntent (..), Registration, registerIntentDigest)
import Control.Concurrent.STM (TVar)
import Crypto.Random (ChaChaDRG)
import Data.Text (Text)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (NameOwnerCode, formatNameOwnerCode, nameOwnerCodeEntropy, parseNameOwnerCode, randomNameOwnerCode)
import Simplex.Messaging.Crypto.BIP32 (masterKey, xkKey)
import Simplex.Messaging.Crypto.BIP39 (entropySeed)
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

walletFromNameOwnerCode :: TVar ChaChaDRG -> NameOwnerCode -> IO EphemeralWallet
walletFromNameOwnerCode g ewCode = do
  master <- either fail pure $ masterKey $ entropySeed (nameOwnerCodeEntropy ewCode) ""
  account <- either fail pure $ mkAccountIndex 0
  (xk, ewAddress) <- deriveAddress g master (bip44Path Ethereum account) >>= either fail pure
  pure EphemeralWallet {ewCode, ewKey = xkKey xk, ewAddress}
