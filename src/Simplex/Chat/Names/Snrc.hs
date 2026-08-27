{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The SNRC contract surface as the client sees it: name hashing, and the
-- EIP-712 intents a user signs for the relayer to submit.
--
-- The type strings here must match the deployed contracts byte for byte, in
-- EIP-712 canonical form (no spaces after commas). They are the client half of
-- @setTextWithSig@ on @SimplexResolver@.
module Simplex.Chat.Names.Snrc
  ( SnrcDeployment (..),
    RecordKey (..),
    Intent (..),
    SignedIntent (..),
    recordKeyText,
    parseRecordKey,
    labelHash,
    nameHash,
    tokenId,
    intent712,
    intentDigest,
    signSnrcIntent,
    setTextTypeString,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Simplex.Chat.Wallet (Eip712Intent (..), EthSignature, WalletAccount, signIntent)
import Simplex.Messaging.Eth.EIP712 (hashTypedData)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Eth.EIP712 (Eip712Domain (..), Value (..))
import Simplex.Messaging.Eth.Keccak (keccak256)

-- | Where a TLD is deployed. The verifying contract differs per intent kind, so
-- both are carried.
data SnrcDeployment = SnrcDeployment
  { sdTld :: ByteString,
    sdChainId :: Integer,
    sdRegistrar :: Address,
    sdResolver :: Address
  }
  deriving (Eq, Show)

-- | The two SimpleX link records. Independent: setting one must not disturb the
-- other, which is why the CLI takes the record as a subcommand.
data RecordKey = RKContact | RKChannel
  deriving (Eq, Show)

recordKeyText :: RecordKey -> Text
recordKeyText = \case
  RKContact -> "simplex.contact"
  RKChannel -> "simplex.channel"

parseRecordKey :: Text -> Either String RecordKey
parseRecordKey = \case
  "contact" -> Right RKContact
  "channel" -> Right RKChannel
  k -> Left $ "unknown record " <> show k <> ", expected contact or channel"

data Intent = SetTextRecord
  { sxName :: Text,
    sxKey :: RecordKey,
    sxValue :: Text,
    sxNonce :: Integer,
    sxDeadline :: Integer
  }
  deriving (Eq, Show)

data SignedIntent = SignedIntent
  { siIntent :: Intent,
    siSignature :: EthSignature
  }

setTextTypeString :: ByteString
setTextTypeString = "SetText(bytes32 node,string key,string value,uint256 nonce,uint256 deadline)"

labelHash :: ByteString -> ByteString
labelHash = keccak256

-- | ENS namehash of a fully-qualified name, e.g. @alice.simplex@.
nameHash :: ByteString -> ByteString
nameHash name
  | B.null name = B.replicate 32 0
  | otherwise = foldr step (B.replicate 32 0) (BC.split '.' name)
  where
    step lbl node = keccak256 (node <> labelHash lbl)

-- | @BaseRegistrar@ token id: @uint256(keccak256(label))@.
tokenId :: ByteString -> Integer
tokenId = B.foldl' (\acc w -> acc * 256 + fromIntegral w) 0 . labelHash

-- | The typed-data an intent signs. Deliberately the only bridge to the wallet:
-- 'signIntent' takes this, never a bare digest.
intent712 :: SnrcDeployment -> Intent -> Eip712Intent
intent712 d SetTextRecord {sxName, sxKey, sxValue, sxNonce, sxDeadline} =
  Eip712Intent
    { eiDomain =
        Eip712Domain
          { edName = "SimplexResolver",
            edVersion = "1",
            edChainId = sdChainId d,
            edVerifyingContract = sdResolver d
          },
      eiTypeString = setTextTypeString,
      eiValues =
        [ VFixedBytes (nameHash (encodeUtf8 sxName)),
          VString (encodeUtf8 (recordKeyText sxKey)),
          VString (encodeUtf8 sxValue),
          VUint sxNonce,
          VUint sxDeadline
        ]
    }

signSnrcIntent :: WalletAccount -> SnrcDeployment -> Intent -> Either String SignedIntent
signSnrcIntent acc d i = SignedIntent i <$> signIntent acc (intent712 d i)


-- | The 32 bytes an intent signs. What the relayer recomputes to recover the
-- signer, and what the contract hashes on chain.
intentDigest :: SnrcDeployment -> Intent -> Either String ByteString
intentDigest d i =
  let Eip712Intent {eiDomain, eiTypeString, eiValues} = intent712 d i
   in hashTypedData eiDomain eiTypeString eiValues
