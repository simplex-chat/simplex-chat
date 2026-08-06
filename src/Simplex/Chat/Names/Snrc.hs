{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The SNRC contract surface as the client sees it: name hashing, and the
-- EIP-712 intents the user signs for the relayer to submit.
--
-- The type strings here must match the contracts byte for byte, in EIP-712
-- canonical form (no spaces after commas). They are the client half of
-- @transferWithSig@ on @BaseRegistrarImplementation@ and @setTextWithSig@ on
-- @SimplexResolver@.
module Simplex.Chat.Names.Snrc
  ( SnrcDeployment (..),
    Intent (..),
    SignedIntent (..),
    labelHash,
    nameHash,
    tokenId,
    intentDigest,
    transferTypeString,
    setTextTypeString,
    contactRecordKey,
    channelRecordKey,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Simplex.Chat.Wallet (EthSignature)
import Simplex.Messaging.Eth.Address (Address)
import Simplex.Messaging.Eth.EIP712
import Simplex.Messaging.Eth.Keccak (keccak256)

-- | Where a TLD is deployed. One of these per TLD; the verifying contract
-- differs per intent kind, so both are carried.
data SnrcDeployment = SnrcDeployment
  { sdTld :: ByteString, -- ^ e.g. @"simplex"@, without the dot
    sdChainId :: Integer,
    sdRegistrar :: Address, -- ^ verifying contract for transfers
    sdResolver :: Address -- ^ verifying contract for record writes
  }
  deriving (Eq, Show)

data Intent
  = -- | Gift a name to another address.
    TransferName {tiFrom :: Address, tiTo :: Address, tiLabel :: ByteString, tiNonce :: Integer, tiDeadline :: Integer}
  | -- | Repoint a name at a different link.
    SetTextRecord {sxName :: ByteString, sxKey :: ByteString, sxValue :: ByteString, sxNonce :: Integer, sxDeadline :: Integer}
  deriving (Eq, Show)

data SignedIntent = SignedIntent
  { siIntent :: Intent,
    siSignature :: EthSignature
  }
  deriving (Eq, Show)

-- | ENS text record keys carrying SimpleX links.
contactRecordKey, channelRecordKey :: ByteString
contactRecordKey = "simplex.contact"
channelRecordKey = "simplex.channel"

transferTypeString :: ByteString
transferTypeString = "TransferName(address from,address to,uint256 tokenId,uint256 nonce,uint256 deadline)"

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

-- | The 32-byte digest the user signs.
intentDigest :: SnrcDeployment -> Intent -> Either String ByteString
intentDigest d = \case
  TransferName {tiFrom, tiTo, tiLabel, tiNonce, tiDeadline} ->
    hashTypedData
      (domain "SimplexNames" (sdRegistrar d))
      transferTypeString
      [VAddress tiFrom, VAddress tiTo, VUint (tokenId tiLabel), VUint tiNonce, VUint tiDeadline]
  SetTextRecord {sxName, sxKey, sxValue, sxNonce, sxDeadline} ->
    hashTypedData
      (domain "SimplexResolver" (sdResolver d))
      setTextTypeString
      [VFixedBytes (nameHash sxName), VString sxKey, VString sxValue, VUint sxNonce, VUint sxDeadline]
  where
    domain n c = Eip712Domain {edName = n, edVersion = "1", edChainId = sdChainId d, edVerifyingContract = c}
