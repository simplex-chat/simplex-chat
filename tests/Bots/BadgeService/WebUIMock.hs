{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bots.BadgeService.WebUIMock
  ( EphemeralWallet (..),
    newEphemeralWallet,
    redemptionCode,
    walletFromCode,
    signRegisterIntent,
  )
where

import BadgeService.Names.Registration (RegisterIntent (..), Registration, registerIntentDigest)
import Control.Concurrent.STM (TVar)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Crypto.Random (ChaChaDRG)
import Data.Text (Text)
import Data.Word (Word64)
import Simplex.Chat.Badges.Code (NameCode, formatNameCode, nameCodeEntropy, parseNameCode, randomNameCode)
import Simplex.Messaging.Crypto.BIP32 (masterKey, xkKey)
import Simplex.Messaging.Crypto.BIP39 (entropySeed)
import Simplex.Messaging.Crypto.BIP44 (CoinType (..), bip44Path, mkAccountIndex)
import Simplex.Messaging.Crypto.Secp256k1 (Secp256k1PrivateKey, signRecoverable)
import Simplex.Messaging.Eth.Address (Address, deriveAddress)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain)
import Simplex.Messaging.Util ((<$$>))

data EphemeralWallet = EphemeralWallet
  { ewCode :: NameCode,
    ewKey :: Secp256k1PrivateKey,
    ewAddress :: Address
  }

newEphemeralWallet :: TVar ChaChaDRG -> IO EphemeralWallet
newEphemeralWallet g = randomNameCode g >>= walletFromNameCode g >>= either fail pure

redemptionCode :: EphemeralWallet -> Text
redemptionCode = formatNameCode . ewCode

walletFromCode :: TVar ChaChaDRG -> Text -> IO (Either String EphemeralWallet)
walletFromCode g = maybe (pure $ Left "invalid name code") (walletFromNameCode g) . parseNameCode

signRegisterIntent :: TVar ChaChaDRG -> Eip712Domain -> EphemeralWallet -> Registration -> Word64 -> IO (Either String RegisterIntent)
signRegisterIntent g domain EphemeralWallet {ewKey} r deadline =
  RegisterIntent r deadline <$$> signRecoverable g ewKey (registerIntentDigest domain r deadline)

walletFromNameCode :: TVar ChaChaDRG -> NameCode -> IO (Either String EphemeralWallet)
walletFromNameCode g ewCode = runExceptT $ do
  master <- liftEither $ masterKey $ entropySeed (nameCodeEntropy ewCode) ""
  account <- liftEither $ mkAccountIndex 0
  (xk, ewAddress) <- ExceptT $ deriveAddress g master (bip44Path Ethereum account)
  pure EphemeralWallet {ewCode, ewKey = xkKey xk, ewAddress}
