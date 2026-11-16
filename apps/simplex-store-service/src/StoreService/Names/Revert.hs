{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StoreService.Names.Revert (RevertName (..), RevertReason (..), maxNodeMessageLength, decodeRevert, revertText, revertReasonText) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1)
import StoreService.Names.Abi (decodeString, hex, selector)

-- | The custom errors of SimplexController, SimplexSponsor, BaseRegistrarImplementation and SimplexResolver, and the reservation and terms errors of SimplexNamePolicy, that the service tells apart.
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
  | RNTermsMismatch
  | RNTermsNotAccepted
  | RNTransferFailed
  | RNTransferToSelf
  | RNUnexpiredCommitmentExists
  | RNZeroAddress
  deriving (Eq, Show, Enum, Bounded)

data RevertReason = RRWithoutData | RRNamed RevertName | RRUnnamed Text
  deriving (Eq, Show)

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
  RNTermsMismatch -> "TermsMismatch(bytes32,bytes32)"
  RNTermsNotAccepted -> "TermsNotAccepted(string)"
  RNTransferFailed -> "TransferFailed()"
  RNTransferToSelf -> "TransferToSelf()"
  RNUnexpiredCommitmentExists -> "UnexpiredCommitmentExists(bytes32)"
  RNZeroAddress -> "ZeroAddress()"

-- | The error's name as the contracts declare it.
revertText :: RevertName -> Text
revertText = decodeLatin1 . BC.takeWhile (/= '(') . revertSignature

-- | As the revert column and the answer's revert field show it, none for a revert without data.
revertReasonText :: RevertReason -> Maybe Text
revertReasonText = \case
  RRWithoutData -> Nothing
  RRNamed n -> Just $ revertText n
  RRUnnamed reason -> Just reason

revertSelectors :: Map ByteString RevertName
revertSelectors = M.fromList [(selector $ revertSignature n, n) | n <- [minBound .. maxBound]]

-- | A node's error message is cut to this many characters.
maxNodeMessageLength :: Int
maxNodeMessageLength = 200

-- | A revert's reason by its data: one of the contracts' errors by its selector, Solidity's Error(string) by its message,
-- any other by its selector in hex.
decodeRevert :: ByteString -> RevertReason
decodeRevert revertData
  | B.null revertData = RRWithoutData
  | Just n <- M.lookup revertSelector revertSelectors = RRNamed n
  | revertSelector == selector "Error(string)", Right message <- decodeString arguments = RRUnnamed $ T.take maxNodeMessageLength message
  | otherwise = RRUnnamed $ hex revertSelector
  where
    (revertSelector, arguments) = B.splitAt selectorSize revertData
    selectorSize = 4
