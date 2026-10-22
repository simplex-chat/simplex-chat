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
  | -- | a revert with empty or no data, as a bare @require@ or @revert()@ gives
    RNWithoutData
  deriving (Eq, Show, Enum, Bounded)

-- | Nothing for a revert without data, which has no selector.
revertSignature :: RevertName -> Maybe ByteString
revertSignature = \case
  RNAlreadyFrozen -> Just "AlreadyFrozen()"
  RNCommitmentNotFound -> Just "CommitmentNotFound(bytes32)"
  RNCommitmentTooNew -> Just "CommitmentTooNew(bytes32,uint256,uint256)"
  RNCommitmentTooOld -> Just "CommitmentTooOld(bytes32,uint256,uint256)"
  RNDurationTooShort -> Just "DurationTooShort(uint256)"
  RNFrozen -> Just "Frozen()"
  RNInsufficientAllowance -> Just "InsufficientAllowance(uint256,uint256)"
  RNInsufficientValue -> Just "InsufficientValue()"
  RNInvalidEVMAddress -> Just "InvalidEVMAddress(bytes)"
  RNInvalidNonce -> Just "InvalidNonce()"
  RNInvalidSignature -> Just "InvalidSignature()"
  RNLabelTooLong -> Just "LabelTooLong(uint256,uint256)"
  RNMaxCommitmentAgeTooHigh -> Just "MaxCommitmentAgeTooHigh()"
  RNMaxCommitmentAgeTooLow -> Just "MaxCommitmentAgeTooLow()"
  RNMinCharLengthCanOnlyDecrease -> Just "MinCharLengthCanOnlyDecrease()"
  RNMinCharLengthZero -> Just "MinCharLengthZero()"
  RNMinCommitmentAgeZero -> Just "MinCommitmentAgeZero()"
  RNNameNotAvailable -> Just "NameNotAvailable(string)"
  RNNameNotReserved -> Just "NameNotReserved(string)"
  RNNameReserved -> Just "NameReserved(string)"
  RNNameTooShort -> Just "NameTooShort(string,uint8)"
  RNNftGateCanOnlyBeDisabled -> Just "NftGateCanOnlyBeDisabled()"
  RNNftRequired -> Just "NftRequired()"
  RNNodeNotOwnedByRegistrar -> Just "NodeNotOwnedByRegistrar()"
  RNNotBeneficiary -> Just "NotBeneficiary()"
  RNNotNameOwner -> Just "NotNameOwner()"
  RNNotOffered -> Just "NotOffered()"
  RNNotOwnerOrBeneficiary -> Just "NotOwnerOrBeneficiary()"
  RNNotSponsor -> Just "NotSponsor()"
  RNNotSubnameRegistrar -> Just "NotSubnameRegistrar()"
  RNOffsetOutOfBoundsError -> Just "OffsetOutOfBoundsError(uint256,uint256)"
  RNOwnerMustBeCaller -> Just "OwnerMustBeCaller()"
  RNPublicSalesClosed -> Just "PublicSalesClosed()"
  RNReasonRequired -> Just "ReasonRequired()"
  RNRecipientMustBeCaller -> Just "RecipientMustBeCaller()"
  RNReentrantCall -> Just "ReentrantCall()"
  RNResolverRequiredWhenDataSupplied -> Just "ResolverRequiredWhenDataSupplied()"
  RNReverseRecordNotSupported -> Just "ReverseRecordNotSupported()"
  RNSignatureExpired -> Just "SignatureExpired()"
  RNTransferFailed -> Just "TransferFailed()"
  RNTransferToSelf -> Just "TransferToSelf()"
  RNUnexpiredCommitmentExists -> Just "UnexpiredCommitmentExists(bytes32)"
  RNZeroAddress -> Just "ZeroAddress()"
  RNWithoutData -> Nothing

revertSelectors :: Map ByteString RevertName
revertSelectors = M.fromList [(selector signature, n) | n <- [minBound .. maxBound], Just signature <- [revertSignature n]]

-- | The error a revert's data names by its selector, if it is one of the contracts' errors.
decodeRevert :: ByteString -> Maybe RevertName
decodeRevert revertData
  | B.null revertData = Just RNWithoutData
  | otherwise = M.lookup (B.take selectorSize revertData) revertSelectors
  where
    selectorSize = 4
