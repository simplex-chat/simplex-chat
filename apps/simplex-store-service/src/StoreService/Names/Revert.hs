{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Revert (RevertName (..), decodeRevert) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import StoreService.Names.Abi (selector)

-- | The custom errors of SimplexController, SimplexSponsor, BaseRegistrarImplementation and SimplexResolver.
-- The sponsor passes the controller's and the base registrar's reverts through, so any call can revert with any of them.
data RevertName
  = RNAlreadyFrozen
  | RNCommitmentNotFound
  | RNCommitmentTooNew
  | RNCommitmentTooOld
  | RNDurationTooShort
  | RNFrozen
  | RNInsufficientAllowance
  | RNInsufficientValue
  | RNInvalidEVMAddress
  | RNInvalidNonce
  | RNInvalidSignature
  | RNLabelTooLong
  | RNMaxCommitmentAgeTooHigh
  | RNMaxCommitmentAgeTooLow
  | RNMinCharLengthCanOnlyDecrease
  | RNMinCharLengthZero
  | RNMinCommitmentAgeZero
  | RNNameNotAvailable
  | RNNameNotReserved
  | RNNameReserved
  | RNNameTooShort
  | RNNftGateCanOnlyBeDisabled
  | RNNftRequired
  | RNNodeNotOwnedByRegistrar
  | RNNotBeneficiary
  | RNNotNameOwner
  | RNNotOffered
  | RNNotOwnerOrBeneficiary
  | RNNotSponsor
  | RNNotSubnameRegistrar
  | RNOffsetOutOfBoundsError
  | RNOwnerMustBeCaller
  | RNPublicSalesClosed
  | RNReasonRequired
  | RNRecipientMustBeCaller
  | RNReentrantCall
  | RNResolverRequiredWhenDataSupplied
  | RNReverseRecordNotSupported
  | RNSignatureExpired
  | RNTransferFailed
  | RNTransferToSelf
  | RNUnexpiredCommitmentExists
  | RNZeroAddress
  deriving (Eq, Show, Enum, Bounded)

revertSignature :: RevertName -> ByteString
revertSignature = \case
  RNAlreadyFrozen -> "AlreadyFrozen()"
  RNCommitmentNotFound -> "CommitmentNotFound(bytes32)"
  RNCommitmentTooNew -> "CommitmentTooNew(bytes32,uint256,uint256)"
  RNCommitmentTooOld -> "CommitmentTooOld(bytes32,uint256,uint256)"
  RNDurationTooShort -> "DurationTooShort(uint256)"
  RNFrozen -> "Frozen()"
  RNInsufficientAllowance -> "InsufficientAllowance(uint256,uint256)"
  RNInsufficientValue -> "InsufficientValue()"
  RNInvalidEVMAddress -> "InvalidEVMAddress(bytes)"
  RNInvalidNonce -> "InvalidNonce()"
  RNInvalidSignature -> "InvalidSignature()"
  RNLabelTooLong -> "LabelTooLong(uint256,uint256)"
  RNMaxCommitmentAgeTooHigh -> "MaxCommitmentAgeTooHigh()"
  RNMaxCommitmentAgeTooLow -> "MaxCommitmentAgeTooLow()"
  RNMinCharLengthCanOnlyDecrease -> "MinCharLengthCanOnlyDecrease()"
  RNMinCharLengthZero -> "MinCharLengthZero()"
  RNMinCommitmentAgeZero -> "MinCommitmentAgeZero()"
  RNNameNotAvailable -> "NameNotAvailable(string)"
  RNNameNotReserved -> "NameNotReserved(string)"
  RNNameReserved -> "NameReserved(string)"
  RNNameTooShort -> "NameTooShort(string,uint8)"
  RNNftGateCanOnlyBeDisabled -> "NftGateCanOnlyBeDisabled()"
  RNNftRequired -> "NftRequired()"
  RNNodeNotOwnedByRegistrar -> "NodeNotOwnedByRegistrar()"
  RNNotBeneficiary -> "NotBeneficiary()"
  RNNotNameOwner -> "NotNameOwner()"
  RNNotOffered -> "NotOffered()"
  RNNotOwnerOrBeneficiary -> "NotOwnerOrBeneficiary()"
  RNNotSponsor -> "NotSponsor()"
  RNNotSubnameRegistrar -> "NotSubnameRegistrar()"
  RNOffsetOutOfBoundsError -> "OffsetOutOfBoundsError(uint256,uint256)"
  RNOwnerMustBeCaller -> "OwnerMustBeCaller()"
  RNPublicSalesClosed -> "PublicSalesClosed()"
  RNReasonRequired -> "ReasonRequired()"
  RNRecipientMustBeCaller -> "RecipientMustBeCaller()"
  RNReentrantCall -> "ReentrantCall()"
  RNResolverRequiredWhenDataSupplied -> "ResolverRequiredWhenDataSupplied()"
  RNReverseRecordNotSupported -> "ReverseRecordNotSupported()"
  RNSignatureExpired -> "SignatureExpired()"
  RNTransferFailed -> "TransferFailed()"
  RNTransferToSelf -> "TransferToSelf()"
  RNUnexpiredCommitmentExists -> "UnexpiredCommitmentExists(bytes32)"
  RNZeroAddress -> "ZeroAddress()"

revertSelectors :: Map ByteString RevertName
revertSelectors = M.fromList [(selector $ revertSignature n, n) | n <- [minBound .. maxBound]]

-- | The error a revert's data names by its selector, if it is one of the contracts' errors.
decodeRevert :: ByteString -> Maybe RevertName
decodeRevert revertData = M.lookup (B.take selectorSize revertData) revertSelectors
  where
    selectorSize = 4
